#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(REDCapR)
library(labelled)

# REDCap API connection settings

redcap_url <- "https://redcap.hmri.org.au/api/"
redcap_token <- "0E9C1022CDE906CFE307D17707A6DAE0"

# Research Development Opportunites Register Record Editor

ui <- fluidPage(
  titlePanel("REDCap Record Editor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("record_id", "Select Record:", choices = NULL),
      uiOutput("edit_fields"),
      actionButton("save", "Save Changes")
    ),
    mainPanel(
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Fetch data from REDCap with labels
  fetch_data <- reactive({
    result <- redcap_read_oneshot(
      redcap_uri = redcap_url,
      token = redcap_token,
      raw_or_label = "label"
    )
    
    if (result$success) {
      data <- result$data
      return(data)
    } else {
      showNotification("Error fetching data from REDCap.", type = "error")
      return(NULL)
    }
  })
  
  observe({
    data <- fetch_data()
    if (!is.null(data)) {
      updateSelectInput(session, "record_id", choices = data[["record_id"]])
    }
  })
  
  output$data_table <- renderDT({
    data <- fetch_data()
    if (!is.null(data)) {
      datatable(data, selection = "single", editable = TRUE, rownames = FALSE)
    }
  })
  
  output$edit_fields <- renderUI({
    req(input$record_id)
    data <- fetch_data()
    if (!is.null(data)) {
      record <- data[data$record_id == input$record_id, ]
      lapply(names(record), function(col) {
        # Check if column is categorical (factor or labeled)
        if (is.factor(record[[col]]) || inherits(record[[col]], "labelled")) {
          # Get the levels of the factor
          choices <- levels(record[[col]])
          # Create a dropdown with labels but store numeric codes
          selectInput(col, label = col, 
                      choices = setNames(1:length(choices), choices), 
                      selected = as.character(record[[col]]))
        } else {
          # Use textInput for non-categorical columns
          textInput(col, label = col, value = record[[col]])
        }
      })
    }
  })
  
  observeEvent(input$save, {
    data <- fetch_data()
    if (!is.null(data)) {
      new_data <- data
      for (col in names(new_data)) {
        if (is.factor(new_data[[col]]) || inherits(new_data[[col]], "labelled")) {
          # Map the selected label back to the numeric code
          new_data[new_data$record_id == input$record_id, col] <- as.integer(input[[col]])
        } else {
          new_data[new_data$record_id == input$record_id, col] <- input[[col]]
        }
      }
      
      # Update the record in REDCap
      redcap_write(
        new_data,
        redcap_uri = redcap_url,
        token = redcap_token
      )
      showNotification("Record updated successfully!", type = "message")
    }
  })
}

shinyApp(ui, server)