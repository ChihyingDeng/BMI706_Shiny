library(plotly)
library(shiny)
library(tidyverse)



osmi <- read.delim("data/OSMI.txt", stringsAsFactors = F)
if_so <- read.delim("data/if_so.txt", stringsAsFactors = F)
if_so <- if_so %>% filter(If.so..what.conditions.were.you.diagnosed.with != "")

work_position <- read.delim("data/work_position.txt", stringsAsFactors = F)

osmi <- merge(osmi, if_so)
osmi <- merge(osmi, work_position)

osmi$Age.Group <- factor(osmi$Age.Group, 
                         levels = c("Under 20", "20-25", "26-30", "31-35", "36-40", 
                                    "41-45", "46-50", "51-55", "56-60", "61-65", "Over 65"))

country.list <- c("All", unique(osmi$What.country.do.you.live.in))
disease.list <- c("All", unique(osmi$If.so..what.conditions.were.you.diagnosed.with))
position.list <- c("All", unique(osmi$Which.of.the.following.best.describes.your.work.position))
employees.list <- c("All", unique(osmi$How.many.employees.does.your.company.or.organization.have))
family.list <- c(unique(osmi$Do.you.have.a.family.history.of.mental.illness))
remotely.list <- c(unique(osmi$Do.you.work.remotely))
gender.list <- c(unique(osmi$What.is.your.gender))



ui3 <- fluidPage(
  titlePanel("Work"),
  sidebarPanel(
    selectInput("country", "Select country:", multiple = TRUE,
                choices = country.list, selected = c("United States of America")),
    selectInput("disease", "Select disease:", multiple = TRUE,
                choices = disease.list, selected = c("All")),
    selectInput("family", "Do you have a family history of mental illness:", multiple = TRUE,
                choices = family.list, selected = family.list),
    selectInput("gender", "Select gender:", multiple = TRUE,
                choices = gender.list, selected = gender.list),
    selectInput("remotely", "Do you work remotely:", multiple = TRUE,
                choices = remotely.list, selected = remotely.list),
    selectInput("employees", "How many employees does your company have:", multiple = TRUE,
                choices = employees.list, selected = c("All")),
    selectInput("position", "What is your work position:", multiple = TRUE,
                choices = position.list, selected = c("All")),

    sliderInput("age", "Age range:",
                min = 15, max = 99, value = c(15,99))
    
  ),
  
  mainPanel(
    plotlyOutput("plot1"),
    plotlyOutput("plot2"),
    plotlyOutput("plot3"),
    plotlyOutput("plot4"),
    plotlyOutput("plot5")
  )
)


server3 <- function(input, output, session) {
  
  data <- reactive({
    d3 <- osmi

    if(!("All" %in% input$country)){
      d3 <- d3 %>% filter(What.country.do.you.live.in %in% input$country)
    }
    if(!("All" %in% input$disease)){
      d3 <- d3 %>% filter(If.so..what.conditions.were.you.diagnosed.with %in% input$disease)
    }
    if(!("All" %in% input$position)){
      d3 <- d3 %>% filter(Which.of.the.following.best.describes.your.work.position %in% input$position)
    }
    if(!("All" %in% input$employees)){
      d3 <- d3 %>% filter(How.many.employees.does.your.company.or.organization.have %in% input$employees)
    }
    print(dim(d3))

    d3 <- d3 %>% filter(Do.you.have.a.family.history.of.mental.illness %in% input$family,
                          What.is.your.gender %in% input$gender,
                          Do.you.work.remotely %in% input$remotely,
                          What.is.your.age > input$age[1]-1,
                          What.is.your.age < input$age[2]+1
                          )
    print(dim(d3))

    d3

  })


  output$plot1 <- renderPlotly({
    
    d3 <- data()
    d3.c <- d3 %>% group_by(What.country.do.you.live.in, If.so..what.conditions.were.you.diagnosed.with) %>% summarise(ppl.count=length(unique(ResponseID)))
    p <- ggplot(d3.c, aes(x=If.so..what.conditions.were.you.diagnosed.with, y=ppl.count, fill=What.country.do.you.live.in)) + 
      geom_bar(stat = "identity", position = "stack") + coord_flip() + theme_bw() + theme(legend.position = "none") +
      xlab("Disease") + ylab("Number of ppl")
    ggplotly(p)

  })


  output$plot2 <- renderPlotly({
    
    d3 <- data()
    p <- ggplot(d3, aes(x=Do.you.have.a.family.history.of.mental.illness, fill=Do.you.have.a.family.history.of.mental.illness)) + 
      geom_bar() + theme_bw() + theme(legend.position = "none") + facet_wrap(~What.is.your.gender) +
      xlab("Do you have a family history of mental illness") + ylab("Number of ppl")
    ggplotly(p)

  })


  output$plot3 <- renderPlotly({
    
    d3 <- data()
    p <- ggplot(d3, aes(x=Age.Group, fill=If.so..what.conditions.were.you.diagnosed.with)) + 
      geom_bar() + theme_bw() + theme(legend.position = "none") + facet_wrap(~What.is.your.gender, ncol=1) +
      xlab("Age group") + ylab("Number of ppl")
    ggplotly(p)

  })


  output$plot4 <- renderPlotly({
    
    d3 <- data()
    d3.c <- d3 %>% group_by(Do.you.work.remotely, How.many.employees.does.your.company.or.organization.have) %>% summarise(ppl.count=length(unique(ResponseID)))
    d3.c <- d3.c[d3.c$How.many.employees.does.your.company.or.organization.have != "", ]
    d3.c$How.many.employees.does.your.company.or.organization.have <- factor(d3.c$How.many.employees.does.your.company.or.organization.have, 
                                                                              levels = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"))
    d3.c <- as.data.frame(d3.c)
    p <- ggplot(d3.c, aes(x=How.many.employees.does.your.company.or.organization.have, y=ppl.count, group=Do.you.work.remotely, color=Do.you.work.remotely)) + 
      geom_point() + geom_line() + theme_bw() +
      xlab("How many employees does your company have") + ylab("Number of ppl")
    ggplotly(p)

  })


  output$plot5 <- renderPlotly({
    
    d3 <- data()
    d3.c <- d3 %>% group_by(Which.of.the.following.best.describes.your.work.position) %>% summarise(ppl.count=length(unique(ResponseID))) 
    d3.c <- as.data.frame(d3.c)

    p <- ggplot(d3.c, aes(x=Which.of.the.following.best.describes.your.work.position, y=ppl.count, color=Which.of.the.following.best.describes.your.work.position)) + 
      geom_segment(aes(xend=Which.of.the.following.best.describes.your.work.position), yend=0, size=0.25) + geom_point(size=1) + theme_bw() +
      xlab("Work position") + ylab("Number of ppl") + theme(legend.position = "none", axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = -90))
    ggplotly(p)

  })


}

shinyApp(ui3, server3)

