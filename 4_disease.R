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


df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")

ui2 <- fluidPage(
  titlePanel("Disease"),
  sidebarPanel(
    selectInput("disease", "Select disease:", multiple = TRUE,
                choices = disease.list, selected = c("All"))
  ),
  
  mainPanel(
    plotlyOutput("plot1"),
    plotlyOutput("plot2")
  )
)


server2 <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    
    d2 <- osmi %>% filter(What.country.do.you.live.in == "United States of America")
    d2$code <- df$code[match(d2$What.US.state.or.territory.do.you.live.in, df$state)]
    
    if(!("All" %in% input$disease)){
      d2 <- d2 %>% filter(If.so..what.conditions.were.you.diagnosed.with %in% input$disease)
    }

    d2.c <- d2 %>% group_by(What.US.state.or.territory.do.you.live.in, If.so..what.conditions.were.you.diagnosed.with) %>% summarise(ppl.count=length(unique(ResponseID)))
    d2.c$code <- df$code[match(d2.c$What.US.state.or.territory.do.you.live.in, df$state)]

    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('grey90')
    )


    p <- plot_geo(d2.c, locationmode = 'USA-states') %>%
    add_trace(
      z = ~ppl.count, text = ~What.US.state.or.territory.do.you.live.in, locations = ~code,
      color = ~ppl.count, colors = 'Blues'
    ) %>%
    colorbar(title = "Number of ppl") %>%
    layout(
      title = 'Number of ppl',
      geo = g
    )

    p

  })


  output$plot2 <- renderPlotly({
    
    d2 <- osmi %>% filter(What.country.do.you.live.in == "United States of America")
    
    if(!("All" %in% input$disease)){
      d2 <- d2 %>% filter(If.so..what.conditions.were.you.diagnosed.with %in% input$disease)
    }

    p <- ggplot(d2, aes(x=If.so..what.conditions.were.you.diagnosed.with, y=What.is.your.age, fill=If.so..what.conditions.were.you.diagnosed.with)) + 
      geom_boxplot() + theme_bw() + theme(legend.position = "none") +
      xlab("Disease") + ylab("Age") + 
      scale_x_discrete(labels = abbreviate) +
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=8))
    ggplotly(p)

  })


}

shinyApp(ui2, server2)

