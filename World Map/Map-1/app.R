library(shiny)
library(ggplot2)
library(tidyverse)
library(plyr)


monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}
posts2 <- readRDS(file = "posts2.rds")
grouped <- posts2
group <- grouped %>% group_by(month=format(grouped$date, "%Y-%m"))

splitted <- split(posts2, format(posts2$date, "%Y-%m"))

wrld <- tbl_df(map_data("world"))
wrld <- filter(wrld, region != "Antarctica")

ui <- fluidPage(
  title = "NewCiv Post Frequency",
  tabPanel("Map", suppressWarnings(plotOutput("map", click="map_hover")), uiOutput("dynamic")),
  hr(),
  sliderInput("slider", "Time", 
              min = as.Date("2010-08-01"),
              max =as.Date("2018-12-01"),
              value=as.Date("2010-08-01"),
              timeFormat="%Y-%m"),
  textOutput("SliderText")
)
server <- shinyServer(function(input, output){

  sliderMonth <- reactiveValues()
  
  observe({
    full.date <- as.POSIXct(input$slider, tz="GMT")
    sliderMonth$Month <- as.character(monthStart(full.date))
  })
  
  output$SliderText <- renderText({sliderMonth$Month})
  shiny <- reactive({
    format(as.Date(input$slider), "%Y-%m")
  })

  dat <- reactive({
    count <- count(data.frame(splitted[[shiny()]]))
    
    counted <- ddply(count,"city_name",summarize,long=mean(longitude),lat=mean(latitude),freq=sum(freq))
    counted <- counted[order(counted$freq),]
    counted[nrow(counted),1] <- "Bots"
    counted[nrow(counted),2:3] <- c(0,-55)
    counted
  })
  
  output$dynamic <- renderUI({
    req(input$map_hover) 
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    counted <- dat()
    hover <- input$map_hover 
    y <- nearPoints(counted, input$map_hover, threshold = 30, addDist = TRUE)
    req(nrow(y) != 0)
    print(y[order(y$freq, decreasing = TRUE),c(1, 4, 5)], row.names = FALSE)
  })
  
  output$map <- renderPlot({
    suppressWarnings(
      ggplot() +
        geom_map(
          map = wrld, data = wrld, aes(long, lat, map_id=region),
          color = "grey", fill ="white", size=0.1
        ) +
        geom_point(
          data = dat(), aes(long, lat, size = freq), 
          shape=21, fill = "red", color = "white", stroke=0.01
        ) +
        scale_size(name = "# IPs", label=scales::comma, range = c(2,25), limits = c(1,5000)) +
        ggalt::coord_proj("+proj=wintri") +
        ggthemes::theme_map() +
        theme(legend.justification = "center") +
        theme(legend.position = "bottom") +
        labs(title=paste("NewCiv Post frequency ", shiny())) +
        annotate("text", x = 0, y = -60, label = "Bots")
    )
  }, height = 900, width = 1850)

})

shinyApp(ui = ui, server = server)