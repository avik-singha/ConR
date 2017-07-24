#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(jsonlite)
library(tm)
library(SnowballC)
library(lsa)
library(ggplot2)
library(plotly)

##Get the engine ready
data<-fread("./ConR/Data_file/Data_conr.csv")
sentnc_const<-list(Doc1=data[1,Content],Doc2=data[2,Content],Doc3=data[3,Content])


corps<-VectorSource(sentnc_const)
txt.corpus <- VCorpus(corps,readerControl=list(reader=readPlain,language="english"))

for(i in 1:length(txt.corpus)){
  txt.corpus[[i]] <- removeWords(txt.corpus[[i]], c(stopwords("english")))
  txt.corpus[[i]] <- removePunctuation(txt.corpus[[i]])
  txt.corpus[[i]]$content<-tolower(txt.corpus[[i]]$content)
  txt.corpus[[i]]<-stemDocument(txt.corpus[[i]])
  
}

# Define server logic required to compare
shinyServer(function(input, output) {
   query_const<-eventReactive(input$enter,{
    return(input$search)
   })
  
  
   cmp<-function(){
   q_c<-VectorSource((query_const()))
   query.corpus<-VCorpus(q_c,readerControl = list(reader=readPlain,language="english"))
   query.corpus[[1]]$content
   query.corpus[[1]] <- removeWords(query.corpus[[1]], c(stopwords("english")))
   query.corpus[[1]] <- removePunctuation(query.corpus[[1]])
   query.corpus[[1]]$content<-tolower(query.corpus[[1]]$content)
   query.corpus[[1]]<-stemDocument(query.corpus[[1]])
   
   txt.corpus<-append(query.corpus,txt.corpus)
   
   for(i in 1:length(txt.corpus)){
     txt.corpus[[i]] <- removeWords(txt.corpus[[i]], c(stopwords("english"))) 
   }
   
   
   
   term_frequencies<-sapply(1:length(txt.corpus),function(i){
     termFreq(txt.corpus[[i]],control = list(wordLengths=c(1,Inf)))/length(words(txt.corpus[[i]])) })
   
   sq<-1:length(txt.corpus)
   idf<-sapply(1:length(txt.corpus),function(i){
     c<-0
     
     for(j in which((sq %in% i)==F)){(c<-c+ifelse(sort(unique(words(txt.corpus[[i]]))) %in% 
                                                    unique(words(txt.corpus[[j]]))==TRUE,1,0))}
     
     1+log(length(txt.corpus)/(1+c))
     
     
     
   })
   
   tfidf<-sapply(1:length(txt.corpus),function(i){term_frequencies[[i]]*idf[[i]]})
   
   
   similarity<-sapply(2:length(txt.corpus),function(i){
     
     if(length(tfidf[[1]][(names(tfidf[[1]])%in%names(tfidf[[i]]))])!=1){
       norm1<-sqrt(sum(tfidf[[1]][(names(tfidf[[1]])%in%names(tfidf[[i]]))] ^2) ) #normalize the 1st vector 
       normm<-sqrt(sum(tfidf[[i]][(names(tfidf[[i]])%in%names(tfidf[[1]]))]^2))  #normalize rest of vectors
       sum((tfidf[[1]][(names(tfidf[[1]])%in%names(tfidf[[i]]))]/norm1)*(tfidf[[i]][(names(tfidf[[i]])%in%names(tfidf[[1]]))]/normm))
     }
     else
       sum((tfidf[[1]][(names(tfidf[[1]])%in%names(tfidf[[i]]))])*(tfidf[[i]][(names(tfidf[[i]])%in%names(tfidf[[1]]))]))
     
   })
   return(similarity)
   }
   
   
   
  
   out<-function(){
     
     x<-cbind(Document=1:nrow(data),Score=cmp()*100,data)
     return(x)
       
     }
   
   output$res<-renderTable({
     if(length(which(out()[,Score]>50))==0)
       data.table(`Result:`="No Relevant Documents")
     
       else{
     out()[out()[,Score]>50,c("Document","Score","Content")]
       }
     
   })
   output$res2<-renderTable({
     if(length(which(out()[,Score]<=50))==0)
       data.table(`Result:`="No Irrelevant Documents")
     
     else{
     out()[out()[,Score]<=50,c("Document","Score","Content")]
     }
     
   })
   
   
   
   rate_out<-function(){
     query_tag<-"general"
     data_Relevant<-cbind(out()[out()[,Score]>50],Tag=c("overview","plot")) 
     return(score(data_Relevant,query_tag))
     }
       
   

   
   tag_check<-function(x,y){
     temp<-vector(mode="numeric",length = length(y))
     for(i in 1:length(y))
     { 
       if(x=="general")
         temp[i]<-1
       else if(x==y[i])
         temp[i]<-1
       else
         temp[i]<-0
       
       
     }
     return(temp)
   }
   score<-function(data_Relevant,query_tag){
     d1<-data_Relevant[,Likes]-data_Relevant[,Dislikes]
     d1<-(abs(d1))/(max(d1)-min(d1))
     d1<-d1/sum(d1)
     
     
     d2<-as.numeric(Sys.Date()-as.Date(data_Relevant[,DoP],format="%d-%m-%Y %H:%M"))
     d2<-1/d2
     
     weights<-1/2^seq(1,4,1)
     
     d3<-(data_Relevant[,SoP])/(max(data_Relevant[,SoP])-min(data_Relevant[,SoP]))
     d3<-d3/sum(d3)
     
     d4<-tag_check(query_tag,data_Relevant[,Tag])
     
     wtd_score<-weights[1]*d4+weights[2]*d3+weights[2]*d1+weights[4]*d2
     return(wtd_score)
   }
   
   output$res3<-renderPlot({
     if(length(which(out()[,Score]>50))==0)
      {plot(5,5,"n",xlab = "Document",ylab = "Content_Quality",main = c("No Relevant Documents"))}
     
     else{
     y<-data.table(out()[out()[,Score]>50,c("Document")],Content_Quality=rate_out()*100)
     ggplot(y,aes(Document,Content_Quality,color=(Document)))+geom_point()
     }
   })
   
})
