library(reshape)
library(reshape2)
library(plotly)
library(shiny)
library(crosstalk)
library(tidyr)
library(viridis) 
library(wesanderson)


question <- read.csv('data/question.csv') 
q_list <- read.csv('data/question_list.csv') 
data <- read.csv('data/data_code.csv')

names(data) <- c("ResponseID", "selfemployed", "companysize", "techcompany", "ITrole", 
               "employers", "familyHx", "pastHx", "current", "currentDz", 
               "currentDz.maybe", "pastHxpro","pastHxDz","seekTx", "age", "gender", 
               "age.group", "country.live","state.live", "country.work", "state.work", 
               "work.position", "remote","live_country_code", "work_country_code",
               "live_state_code", "work_state_code")

ui <- fluidPage(
     titlePanel("Mental Illness in Tech Compancy"),
     sidebarPanel(
          selectInput("Gender", 
                      label = "Gender:",
                      choices = c('All', as.character(unique(data$gender))),
                      multiple = FALSE),
          selectInput("FamilyHx", 
                      label = "Family history of mental illness:",
                      choices = c('All', as.character(unique(data$familyHx))),
                      multiple = FALSE),
          selectInput("PastHx", 
                      label = "Past history of mental illness:",
                      choices = c('All', as.character(unique(data$pastHx))),
                      multiple = FALSE),
          selectInput("PastHxDz", 
                      label = "What kinds of past history:",
                      choices = c('All', as.character(unique(data$pastHxDz))),
                      multiple = TRUE, selected = "All"),
          selectInput("Current", 
                      label = "Current mental illness:",
                      choices = c('All', as.character(unique(data$current))),
                      multiple = FALSE),
          selectInput("CurrentDz", 
                      label = "What kinds of current condition:",
                      choices = c('All', as.character(unique(data$currentDz))),
                      multiple = TRUE, selected = "All"),
          selectInput("Selfemployed", 
                      label = "Self-employed:",
                      choices = c('All', as.character(unique(data$selfemployed))),
                      multiple = FALSE),
          selectInput("Company", 
                      label = "Company size:",
                      choices = c('All', as.character(unique(data$companysize))),
                      multiple = FALSE),
          selectInput("Work", 
                      label = "Work position:",
                      choices = c('All', as.character(unique(data$work.position))),
                      multiple = TRUE, selected = "All"),
          sliderInput("Age", 
                      label = "Age:",
                      min = 1, max = 99, value = c(1, 99)),
          checkboxGroupInput("QA", 
                             label = "Question Answer:",
                             choices = c("Yes","No","Maybe","Often","Rare","N/A","Other"),
                             selected = c("Yes","No","Maybe","Often","Rare","N/A","Other"))
     ),
     
     mainPanel(
          plotlyOutput("map"),
          plotlyOutput("stat"),    
          plotlyOutput("plot"), 
          verbatimTextOutput("question"),
          verbatimTextOutput("click")
     )
);


server <- function(input, output, session) {
     
     lst <- reactive({
          lst <- data
          if (input$Gender!="All"){
               lst <- lst %>% filter(gender==input$Gender)}
          if (input$FamilyHx!="All"){
               lst <- lst %>% filter(familyHx==input$FamilyHx)}
          if (input$PastHx!="All"){
               lst <- lst %>% filter(pastHx==input$PastHx)}
          if (input$PastHxDz!="All"){
               lst <- lst %>% filter(pastHxDz==input$PastHxDz)}
          if (input$Current!="All"){
               lst <- lst %>% filter(current==input$Current)}
          if (input$CurrentDz!="All"){
               lst <- lst %>% filter(currentDz==input$CurrentDz)}
          if (input$Selfemployed!="All"){
               lst <- lst %>% filter(selfemployed==input$Selfemployed)}
          if (input$Company!="All"){
               lst <- lst %>% filter(companysize==input$Company)}
          if (input$Work!="All"){
               lst <- lst %>% filter(work.position==input$Work)}

          lst <- lst %>% filter(age>=input$Age[1] & age<=input$Age[2]) 
     })
     
     output$map <- renderPlotly({
          lst <- lst()
          lst <- as.data.frame(table(lst$live_state_code))
          names(lst) <- c('country','count') 
          lst <- lst %>% filter(country!='')
          sd <- SharedData$new(lst)
          l <- list(color = toRGB("grey"), width = 0.5)
          g <- list(
               showframe = FALSE,
               showcoastlines = TRUE,
               projection = list(type = 'albers usa') 
          )
          
          p <- plot_geo(sd, locationmode = 'USA-states') %>%
               add_trace(
                    z = ~count, color = ~count, colors = viridis(200),
                    text = ~country, locations = ~country, marker = list(line = l)
               ) %>%
               colorbar(title = 'people') %>%
               layout(title = 'Live states of the participants', geo = g) 
          p
     })
     
     output$stat <- renderPlotly({
          lst <- lst()
          age <- as.data.frame(table(lst$age.group)) 
          age$cumsum <- cumsum(age$Freq)-120
          gender <- as.data.frame(table(lst$gender)) 
          gender$cumsum <- cumsum(gender$Freq)-120
          position <- as.data.frame(table(lst$work.position))
          position$cumsum <- cumsum(position$Freq)-120
          
          p_1 <- plot_ly(age) %>% 
               add_bars(x=~Freq, y=~Var1, color=~Var1, colors = 'Set2') %>%
               layout(yaxis = list(title="Age"))
          p_2 <- plot_ly(gender) %>% 
               add_bars(x=~Freq, y=~Var1, color=~Var1, colors = 'Set3') %>%
               layout(yaxis = list(title="Gender"))
          s <- subplot(p_1, p_2, nrows = 2, shareX = TRUE,
                        titleX = FALSE, titleY = TRUE) %>%
               layout(title = 'Demographics of the participants', barmode='group', 
                      showlegend = FALSE) 
          s
     })   
     
     output$plot <- renderPlotly({
          lst <- lst()
          sub <- question %>% filter(ResponseID %in% unique(lst$ResponseID)) %>%  
               filter(Q!='')
          sub$f <- 1
          sub <- sub %>% cast(Q~A, sum, value="f") %>% 
               melt(id = c("Q")) %>% filter(A %in% input$QA) 
          levels(sub$Q) <- paste("Q", 1:42, sep = "")
          plot_ly(sub) %>% 
               add_bars(x=~Q, y=~value, color=~A, colors = "Pastel1") %>%
               layout(title ="Response to the 42 questions", barmode = 'stack',
                      xaxis = list(title="Question"),
                      yaxis = list(title="Response"))
     })  
     
     output$question <- renderPrint({
          print(q_list$Question)
     })
     
     output$click <- renderPrint({
          d <- event_data("plotly_relayout")
          if (!is.null(d)){
               invisible(updateSliderInput(session, "range", value = c(d[[1]], d[[2]])))
          }
     })
}

shinyApp(ui, server)

