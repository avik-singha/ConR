#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(fluidPage(theme="bootstrap.css",
  
  # Application title
  titlePanel("ConR Application Beta V1.0"),
  
  
  fluidRow(
    sidebarPanel(
       textInput("search","Enter the query to search:",placeholder = "Search Query"),
       actionButton("enter","Enter"),helpText(em("Note: Since this is a Beta Version, certain features are in its limited capacity. Full version of the product would be released soon.")),
       width = 3
       ),
    
    
    column(5,
      tabsetPanel(
        tabPanel("Relevant Search Results",tableOutput("res")),
        tabPanel("Irrelevant Search Results",tableOutput("res2")),
        tabPanel("Content Quality",plotOutput("res3"))
      ) 
    ),
    column(4,h4("Team:",br(),a(h3(em("Juggernaut.Co")), href="https://github.com/avik-singha/ConR", target="blank"),br(),h4("Creators:",br(),a(h3(em("Kanishka Mukherjee")),href="https://www.linkedin.com/in/kanishka-mukherjee-dsc/", target="blank") ,a(h3(em("Avik Singha")),href="https://www.linkedin.com/in/aviksingha/", target="blank"))))
  )
))
