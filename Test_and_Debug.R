setwd("C:\\Users\\lpeix1\\OneDrive - Monsanto\\shiny")
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(fmsb)
library(reshape2)

data<-read.table("HH_CORN_NEW.csv", h=T, sep=",")
View(data)

ui <- pageWithSidebar(
  headerPanel('H2H comparison'),
  sidebarPanel(
    shinythemes::themeSelector(),
    
    selectInput('Season', 'Season', data.frame(unique(data$Season))),
    selectInput('Protocol', 'Region', unique(data.frame(data$Protocol))),
    selectInput('Index_environment', 'EI', unique(data.frame(data$Index_environment))),
    selectInput('Head', 'Head', unique(data.frame(data$Head))),
    selectInput('Other', 'Other', unique(data.frame(data$Other)))
  ),
  mainPanel(
    plotOutput('plot')
  )
)

server <- function(input, output) {
  
  
  #OK FUNCIONANDO
  selectedData <- reactive({
    subset(data, as.character(data$Season)==as.character(input$Season)&
             as.character(data$Protocol)==as.character(input$Protocol)&
             as.character(data$Index_environment)==as.character(input$Index_environment)&
             as.character(data$Head)==as.character(input$Head)&
             as.character(data$Other)==as.character(input$Other))
  })
  
  
  # OK FUNCIONANDO
  Ntrait<- reactive ({
    (unique(data.frame(selectedData()$Trait)))
  })
  
  #Assim?
  data8 <- reactive({
    head<-dcast(selectedData(),Season+Protocol+Country+macro+
                  Index_environment+Head~Trait,
                value.var=c("Head_Mean"))
    other<-dcast(selectedData(),Season+Protocol+Country+macro+
                   Index_environment+Other~Trait,
                 value.var=c("Other_Mean"))
    colnames(other)<-colnames(head)
    traits<-data.frame(rbind(head[,7:length(head)],other[,7:length(head)]))
    colMax <- function(data) sapply(data, max, na.rm = TRUE)
    colMin <- function(data) sapply(data, min, na.rm = TRUE)
    max<-colMax(traits)
    min<-colMin(traits)
    data8<-data.frame(rbind(max,min,traits))
    rownames(data8)<-c("max", "min", as.character(selectedData()$Head[1]),as.character(selectedData()$Other[1]))
    data8
  })
  
  output$plot <- renderPlot({
    if (ncol(data8())>2)
    {
      colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
      colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
      radarchart( data8()  , axistype=0 , 
                  #custom polygon
                  pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
                  #custom the grid
                  cglcol="grey", cglty=3, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                  #custom labels
                  vlcex=1.2, calcex=1.2, title=paste(input$Season, input$Protocol, input$Index_environment, sep="\n"), paxislabels=3
      )
      legend(x=0.7, y=1, legend = rownames(data8()[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
    }
   })
}

shinyApp(ui, server)
