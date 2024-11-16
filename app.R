library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Diamond Price Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot_type", 
                   "Analysis Type:",
                   choices = c("GAM Smoothing" = "gam",
                               "Linear Regression" = "linear")),
            sliderInput("point_alpha",
                  "Point Transparency:",
                  min = 0.1, max = 1,
                  value = 0.3,step = 0.1),
            sliderInput("line_width",
                  "Line Width:",
                  min = 0.25, max = 2,
                  value = 1,step =0.25)
      ),
    mainPanel(
      plotOutput("diamondPlot")
    )
  )
)

#Define Server
server <- function(input, output) {
  output$diamondPlot <- renderPlot({
    dplot <- ggplot(diamonds, aes(x = carat, y = price)) +
      geom_point(alpha = input$point_alpha, 
                 shape = 21, 
                 size = 0.3)
    if(input$plot_type == "gam") {
      dplot <- dplot +
        # Add colored GAM lines by cut
        geom_smooth(aes(color = cut), 
                    method = "gam",
                    se = FALSE, 
                    linewidth = input$line_width,
                    alpha = 0.4) +

        geom_smooth(method = "lm",
                    linewidth = input$line_width,
                    alpha = 0.8)
    } else {
      dplot <- dplot +

        geom_smooth(aes(color = cut),
                    method = "lm",
                    se = FALSE,
                    linewidth = input$line_width,
                    alpha = 0.4) +

        geom_smooth(method = "lm",
                    linewidth = input$line_width * 1.2,
                    color = "blue",
                    alpha = 0.8)
    }
    
    dplot + theme_minimal() +
      labs(x = "Carat Weight",y = "Price (USD)",color = "Diamond Cut") +
      theme(legend.position = "bottom")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
