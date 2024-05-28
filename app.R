library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Interactive Scatter Plot with CSV Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fileInput", "Choose a CSV File:",
                  choices = list.files(pattern = "*.csv", full.names = TRUE)),
      textInput("filename", "Enter Filename for Download:", "DownloadedData"),  # Default filename provided
      hr(),
      h5("Selected Points"),
      p("Hold Shift for aggregate, hold alt for disintegrate."),
      downloadButton("downloadData", "Download Selected Data"),
      width = 3
    ),
    mainPanel(
      plotlyOutput("scatterPlot", height = "1000px"),
      verbatimTextOutput("selectedPoints"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  # Render scatter plot
  
  df <- reactive({
    req(input$fileInput)  # This makes sure that the code waits for the input to be available
    read.csv(input$fileInput)
  })
  
  output$scatterPlot <- renderPlotly({
    p <- plot_ly(data = df(), x = ~x, y = ~y, type = 'scattergl', mode = 'markers',
                 marker = list(
                   size = 1, # note, you want to use larger dots on shiny server
                   color = '#1f77b4',  # Default color
                   opacity = 1,  # Ensure points are fully visible
                   line = list(
                     color = '#1f77b4',  # Default border color
                     width = 0  # Default border width
                   )
                 ),
                 selected = list(
                   marker = list(
                     opacity = 1,
                     color = 'red',  # Same color for selected points to prevent fading
                     line = list(
                       color = 'red',  # Same border color for selected points
                       width = 0  # Same border width for selected points
                     )
                   )
                 ),
                 unselected = list(
                   marker = list(
                     opacity = 1,  # If you still want to show unselected points but fainter, adjust opacity here
                     color = '#1f77b4'  # Same color for unselected points
                   )
                 )
    ) %>%
      layout(dragmode = "select")
    return(p)
  })
  
  # Store selected points
  selected_points <- reactiveValues(points = NULL)
  
  # Update selected points
  # observe({
  #   event_data <- event_data("plotly_selected")
  #   if (!is.null(event_data)) {
  #     selected_points$points <- event_data[, c("x", "y")]  # Extract only x and y
  #   }
  # })
  ##################
  observe({
    event_data <- event_data("plotly_selected")
    if (!is.null(event_data)) {
      # Correcting the indexing to match R's one-based indexing
      ids <- df()$X[event_data$pointNumber + 1]  # Make sure the IDs align with the selected points
      selected_points$points <- data.frame(ID = ids, event_data[c("x", "y")])
    }
  })
  
  
  
  # Show selected points
  output$selectedPoints <- renderPrint({
    if (!is.null(selected_points$points)) {
      total <- nrow(selected_points$points)
      dataToShow <- head(selected_points$points, 10)
      print(dataToShow)
      cat("\nTotal selected: ", total, " points\n")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")  # Use the filename from the text input
    },
    content = function(file) {
      write.csv(selected_points$points, file, row.names = FALSE)
    }
  )
  
  
  
}


shinyApp(ui, server)