# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Data Analytics in Action"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select industry
      selectInput("industry", "Choose an Industry:", 
                  choices = c("Retail", "Healthcare", "Finance", "Sports")),
      
      # Dropdown to select the type of analytics
      selectInput("analytics_type", "Choose Analytics Type:", 
                  choices = c("Descriptive", "Predictive", "Prescriptive")),
      
      br(),
      p("This app demonstrates the power of data analytics across industries. Choose an industry and the type of analytics to explore how data transforms decision-making!")
    ),
    
    mainPanel(
      h3(textOutput("selectedIndustry")),
      h4(textOutput("selectedAnalytics")),
      plotOutput("industryPlot"),
      br(),
      textOutput("explanation")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Display selected industry
  output$selectedIndustry <- renderText({
    paste("Industry:", input$industry)
  })
  
  # Display selected analytics type
  output$selectedAnalytics <- renderText({
    paste("Analytics Type:", input$analytics_type)
  })
  
  # Plot based on industry
  output$industryPlot <- renderPlot({
    data <- mtcars %>%
      mutate(gear = factor(gear),
             carb = factor(carb))
    
    if (input$industry == "Retail") {
      ggplot(data, aes(x = gear, fill = carb)) + 
        geom_bar() + 
        ggtitle("Retail Sales Example: Car Gear and Carb Distribution") +
        theme_minimal()
      
    } else if (input$industry == "Healthcare") {
      ggplot(data, aes(x = wt, y = mpg)) + 
        geom_point() + 
        ggtitle("Healthcare Trends Example: Weight vs. MPG") +
        theme_minimal()
      
    } else if (input$industry == "Finance") {
      ggplot(data, aes(x = wt, y = hp)) + 
        geom_line() + 
        ggtitle("Finance Example: Weight vs. Horsepower") +
        theme_minimal()
      
    } else if (input$industry == "Sports") {
      ggplot(data, aes(x = factor(cyl), fill = factor(am))) + 
        geom_bar() + 
        ggtitle("Sports Example: Cylinder Count by Transmission Type") +
        theme_minimal()
    }
  })
  
  # Dynamic explanation based on industry and analytics type
  output$explanation <- renderText({
    if (input$industry == "Retail") {
      if (input$analytics_type == "Descriptive") {
        "In retail, descriptive analytics helps track what happened by analyzing historical sales data."
      } else if (input$analytics_type == "Predictive") {
        "Retail uses predictive analytics to forecast future sales and optimize inventory."
      } else if (input$analytics_type == "Prescriptive") {
        "Prescriptive analytics in retail suggests the best actions to improve customer experience and sales."
      }
    } else if (input$industry == "Healthcare") {
      if (input$analytics_type == "Descriptive") {
        "Descriptive analytics in healthcare tracks patient data and trends over time."
      } else if (input$analytics_type == "Predictive") {
        "Predictive analytics in healthcare helps foresee health trends and potential outbreaks."
      } else if (input$analytics_type == "Prescriptive") {
        "Prescriptive analytics helps healthcare providers recommend personalized treatments."
      }
    } else if (input$industry == "Finance") {
      if (input$analytics_type == "Descriptive") {
        "Descriptive analytics in finance helps review past transactions and market trends."
      } else if (input$analytics_type == "Predictive") {
        "Finance uses predictive analytics to forecast market changes and investment opportunities."
      } else if (input$analytics_type == "Prescriptive") {
        "Prescriptive analytics in finance provides strategies for investment and risk management."
      }
    } else if (input$industry == "Sports") {
      if (input$analytics_type == "Descriptive") {
        "Descriptive analytics in sports tracks player statistics and game performance."
      } else if (input$analytics_type == "Predictive") {
        "Predictive analytics helps sports teams foresee player performance and game outcomes."
      } else if (input$analytics_type == "Prescriptive") {
        "Prescriptive analytics recommends strategies for player drafts and game tactics."
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
