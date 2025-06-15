
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Define the UI
ui <- fluidPage(
  titlePanel("Titanic Dataset Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for uploading a CSV file
      fileInput("file1", "Choose CSV File", accept = c(".csv")),
      
      # Input for selecting passenger class
      selectInput("class", "Select Passenger Class", choices = c(1, 2, 3)),
      
      # Slider input for age range
      sliderInput("ageRange", "Age Range", 
                  min = 0, max = 80, value = c(0, 80))
    ),
    
    mainPanel(
      # Output for the first plot
      plotOutput("survivalPlot"),
      
      # Output for the second plot
      plotOutput("ageDistPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to read the uploaded CSV file
  dataset <- reactive({
    req(input$file1)  # Ensure file is uploaded
    read_csv(input$file1$datapath)  # Read the uploaded file
  })
  
  # Filter data based on input
  filtered_data <- reactive({
    req(dataset())  # Ensure dataset is loaded
    dataset() %>%
      filter(Pclass == input$class, Age >= input$ageRange[1], Age <= input$ageRange[2])
  })
  
  # Create the survival rate plot
  output$survivalPlot <- renderPlot({
    req(filtered_data())  # Ensure data is filtered
    ggplot(filtered_data(), aes(x = Sex, fill = factor(Survived))) +
      geom_bar(position = "dodge") +
      labs(title = "Survival Rate by Gender",
           x = "Gender", y = "Count", fill = "Survived") +
      theme_minimal()
  })
  
  # Create the age distribution plot
  output$ageDistPlot <- renderPlot({
    req(filtered_data())  # Ensure data is filtered
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
      labs(title = "Age Distribution of Passengers",
           x = "Age", y = "Frequency") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)




