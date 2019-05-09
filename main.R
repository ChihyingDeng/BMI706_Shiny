library(shiny)
library(shinydashboard)

q_list<- read.csv('data/question_list.csv') 

ui <- dashboardPage(
     dashboardHeader(title = "OSMI dataset"),
     dashboardSidebar(
          sidebarMenu(
               menuItem("Question", tabName = "Question"),
               menuItem("Living_country", tabName = "Living_country"),
               menuItem("Work", tabName = "Work"),
               menuItem("Disease", tabName = "Disease")
          )
     ),
     dashboardBody(
          tabItems(
               tabItem(tabName = "Question",
                       h2(source("1_question.R"))),
               tabItem(tabName = "Living_country",
                       h2(source("2_country.R"))),
               tabItem(tabName = "Work",
                       h2(source("3_work.R"))),
               tabItem(tabName = "Disease",
                       h2(source("4_disease.R")))
          )
     )
)

server <- function(input,output,server){}

shinyApp(ui, server)
