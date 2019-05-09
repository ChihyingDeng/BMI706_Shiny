library(shiny)
library(shinydashboard)

q_list<- read.csv('data/question_list.csv') 

ui <- dashboardPage(
     dashboardHeader(title = "OSMI dataset"),
     dashboardSidebar(
          sidebarMenu(
               menuItem("Home", tabName = "Home", 
                        icon = icon("home")),
               menuItem("Question", tabName = "Question", 
                        icon = icon("check")),
               menuItem("Living_country", tabName = "Living_country",
                        icon = icon("check")),
               menuItem("Work", tabName = "Work",
                        icon = icon("check")),

               menuItem("Source code", icon = icon("file-code-o"), 
                        href = "https://github.com/ChihyingDeng/BMI706_Shiny")
          )
     ),
     dashboardBody(
          tabItems(
               tabItem(tabName = "Home",
                       h2("Open Source Mental Illness (OSMI)"),
                       p("2016 survey conducted on employees in tech community regarding mental health disorders")),
               tabItem(tabName = "Question",
                       h2(source("1_question.R"))),
               tabItem(tabName = "Living_country",
                       h2(source("2_country.R"))),
               tabItem(tabName = "Work",
                       h2(source("3_work.R")))

          )
     )
)

server <- function(input,output,server){}

shinyApp(ui, server)
