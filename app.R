#


library(shiny)
tit <- read.csv("Titanic.csv")
tit <- na.omit(tit)

ui <- fluidPage(titlePanel("Titanic Passengers"),
    fluidRow(column(4,
             selectInput("type", "Condition:",
             choices = c('Survived' = "Survived",
                          "PClass" = "Pclass",
                         "Sex" = "Sex")),
             selectInput('ucolor1', 'Fill color:',
                         choices = c(`Green` = "palegreen4",
                                     `Yellow` = "palegoldenrod",
                                     `DarkPink` = "deeppink4",
                                     `Pink` = "rosybrown3"))),
      column(4,
             selectInput("cl", "Parameter:",
             choices = c('Fare' = "Fare",
                         'Age' = "Age")),
             selectInput('ucolor2', 'Fill color:',
                         choices = c(`Green` = "palegreen4",
                                     `Yellow` = "palegoldenrod",
                                     `Darkpink` = "deeppink4",
                                     `Pink` = "rosybrown3"))),
      mainPanel(fluidRow(splitLayout(
        cellWidths = c("50%", "50%"),
        plotOutput("distPlot1"),
        plotOutput("distPlot2"))))))
      


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
      if (input$type == "Survived"){
        x <- tit$Survived
      }
      if (input$type == "Sex"){
        x <- tit$Sex
      }
      if (input$type == "Pclass"){
        x <- tit$Pclass
      }
        barplot(table(x), col = input$ucolor1,
                main = c("Barplot of ", input$type))})
    output$distPlot2 <- renderPlot({
      if (input$cl == "Fare"){
        x <- tit$Fare
      }
      else {
        x <- tit$Age
      }
        #bins <- seq(0, length(x))
        hist(x, breaks=10, col = input$ucolor2,
             main = c("Histogram of passengers ", input$cl),
             xlab = input$cl, ylab = "counts")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
