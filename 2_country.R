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



ui1 <- fluidPage(
  titlePanel("Living country"),
  sidebarPanel(
    selectInput("country", "Select country:", multiple = TRUE,
                choices = country.list, selected = c("United States of America")
    ),
    sliderInput("age", "Age range:",
                min = 15, max = 99, value = c(15,99))
  ),
  
  mainPanel(
    plotlyOutput("plot1"),
    plotlyOutput("plot2")
  )
)


server1 <- function(input, output, session) {
  

  output$plot1 <- renderPlotly({
    
    d1 <- osmi %>% filter(What.is.your.age > input$age[1]-1, What.is.your.age < input$age[2]+1)
    
    if("All" %in% input$country){      

      p1 <- ggplot(d1, aes(x = Age.Group, group = What.country.do.you.live.in, color=What.country.do.you.live.in)) + 
        geom_point(stat = "count") + geom_line(stat = "count") + theme_bw() + theme(legend.position = "none") +
        ggtitle("All") + xlab("Age group") + ylab("Number of ppl")    

    }else{
      
      d1 <- d1 %>% filter(What.country.do.you.live.in %in% input$country)      
      p1 <- ggplot(d1, aes(x = Age.Group, group = What.country.do.you.live.in, color=What.country.do.you.live.in)) + 
        geom_point(stat = "count") + geom_line(stat = "count") + theme_bw() + theme(legend.position = "none") +
        ggtitle(paste0(input$country, collapse = " / ")) + xlab("Age group") + ylab("Number of ppl")
      
    }

    ggplotly(p1, dynamicTicks = TRUE)

  })


  output$plot2 <- renderPlotly({
    
    d1 <- osmi %>% filter(What.is.your.age > input$age[1]-1, What.is.your.age < input$age[2]+1)
    
    if("All" %in% input$country){
      
      p2 <- ggplot(d1, aes(What.country.do.you.live.in, fill=What.country.do.you.live.in)) + geom_bar(stat="count") + 
        coord_flip() + theme_bw() + theme(legend.position = "none") + xlab("Number of ppl") + ylab("Country")
      
    }else{
      
      d1 <- d1 %>% filter(What.country.do.you.live.in %in% input$country)

      p2 <- ggplot(d1, aes(What.country.do.you.live.in, fill=What.country.do.you.live.in)) + geom_bar(stat="count") + 
        coord_flip() + theme_bw() + theme(legend.position = "none") + xlab("Country") + ylab("Number of ppl")
      
    }

        ggplotly(p2, dynamicTicks = TRUE)
  })
  

}

shinyApp(ui1, server1)


