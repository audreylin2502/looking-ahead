library(shiny)

ui <- fluidPage(options=list(height=40),
  sliderInput("commutetime", "If commute time is", min = 1, max = 150, value = 30),
  sliderInput("fuelefficiency", "and fuel efficiency is", min = 1, max = 50, value = 5),
  "then, the total amount of 42-gallon barrels of oil is", textOutput("product1"),
  sliderInput("energycost", "If energy cost is", min = 1, max = 500, value = 10),
  sliderInput("electricityrate", "and electricity rate per kWh is", min = 0.005, max = 5, value = 0.001),
  "then, the total amount of 42-gallon barrels of oil is", textOutput("product2"),
)

server <- function(input, output, session) {
  output$product1 <- renderText({ 
    product1 <- input$commutetime * input$fuelefficiency * 4.090625 * 0.00238038562
    product1
  })
 output$product2 <- renderText({ 
    product2 <- input$energycost * input$electricityrate * 0.741 * 0.00238038562
    product2
  })
}

shinyApp(ui, server)


