#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

r<-iris


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("IRIS"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "flores", label = strong("Selecciona la especie"),
                      choices = unique(iris$Species),
                      #multiple = TRUE,
                      selected = "setosa"),
            #sliderInput("bins",
            #            "Number of bins:",
            #            min = 1,
            #            max = 50,
            #            value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
           tabPanel("GRAFICOS",plotOutput("distPlot")),
           tabPanel("TABLA",tableOutput("distPlot2"),
                    tabPanel("TABLA",tableOutput("distPlot3"))),
           tabPanel("SUMMARY",verbatimTextOutput("distPlot4"),
                    tabPanel("SUMMARY",verbatimTextOutput("distPlot5")))
                      )
                    )
                 )
               )


##renglon,columna

#?unique

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#  ##Subset
#  choice <- reactive({
#    r%>%
#      filter(
#        Species == input$"Species")
#  })
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R

      
      choice  <- input$flores
      if(choice=="setosa"){
        x    <- filter(iris,Species=="setosa")
        y    <- x[,4]
      }else if(choice=="versicolor"){
        x    <- filter(iris,Species=="versicolor")
        y    <- x[,4]
      }else if(choice=="virginica"){
        x    <- filter(iris,Species=="virginica")
        y    <- x[,4]}
        
      #x    <- iris[, 4]

        # draw the histogram with the specified number of bins
        hist(y,  col = 'darkgray', border = 'white',
             xlab = 'Especies',
             main = 'Especies iris')
    })
    
    output$distPlot2 <- renderTable({
      tabla<-head(iris)
      tabla
    })
    output$distPlot3 <- renderTable({
      tabla2<-tail(iris)
      tabla2
    })
    output$distPlot4 <- renderPrint({
      choice  <- input$flores
      if(choice=="setosa"){
        x    <- filter(iris,Species=="setosa")
      }else if(choice=="versicolor"){
        x    <- filter(iris,Species=="versicolor")
      }else if(choice=="virginica"){
        x    <- filter(iris,Species=="virginica")}
    summary(x)
      })
    output$distPlot5 <- renderPrint({
      summary(iris)
    })
    
}

#faithful
#iris
#?filter
#?reactive
#?req

# Run the application 
shinyApp(ui = ui, server = server)
