#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

install.packages("auth0")

library(shiny)
library(DT)
library(REDCapR)
library(labelled)
library(auth0)


# REDCap API connection settings

redcap_url <-  Sys.getenv("REDCAP_URL")
redcap_token <- Sys.getenv("REDCAP_TOKEN")

# Research Development Opportunities Register Record Editor

ui <- auth0_ui(
  fluidPage(
    titlePanel("Research Development Opportunities Register Record Editor"),
    sidebarLayout(
      sidebarPanel(
        h4(textOutput("welcome_msg")),
        selectInput("record_id", "Select Record:", choices = NULL),
        uiOutput("edit_fields"),
        actionButton("save", "Save Changes")
      ),
      mainPanel(
        fluidRow(
          column(6, downloadButton("export_csv", "Export to CSV", class = "btn-sm")),
          column(6, downloadButton("export_excel", "Export to Excel", class = "btn-sm")),
          style = "margin-bottom: 15px;"
        ),
        DTOutput("data_table")
      ) 
    )
  )
)

server <- function(input, output, session) {
  
  # Access user info from Auth0
  user_info <- reactive({
    session$userData$auth0_info
  })
  
  # Display welcome message
  output$welcome_msg <- renderText({
    paste("Welcome,", user_info()$name)
  })
  
  # Fetch both raw and labeled data from REDCap
  fetch_data <- reactive({
    # Get labeled data for display
    labeled_data <- redcap_read_oneshot(
      redcap_uri = redcap_url,
      token = redcap_token,
      raw_or_label = "label"
    )
    
    # Get raw data for editing
    raw_data <- redcap_read_oneshot(
      redcap_uri = redcap_url,
      token = redcap_token,
      raw_or_label = "raw"
    )
    
    if (labeled_data$success && raw_data$success) {
      # Get metadata to understand field types
      meta_data <- redcap_metadata_read(
        redcap_uri = redcap_url,
        token = redcap_token
      )$data
      
      return(list(
        labeled = labeled_data$data,
        raw = raw_data$data,
        meta = meta_data
      ))
    } else {
      showNotification("Error fetching data from REDCap.", type = "error")
      return(NULL)
    }
  })
  
  # Update record selector
  observe({
    data <- fetch_data()
    if (!is.null(data)) {
      updateSelectInput(session, "record_id", choices = data$raw[["record_id"]])
    }
  })
  
  # Display labeled data in table
  output$data_table <- renderDT({
    data <- fetch_data()
    if (!is.null(data)) {
      datatable(data$labeled, selection = "single", rownames = FALSE)
    }
  })
  
  # Create editable fields
  output$edit_fields <- renderUI({
    req(input$record_id)
    data <- fetch_data()
    if (!is.null(data)) {
      record_raw <- data$raw[data$raw$record_id == input$record_id, ]
      record_labeled <- data$labeled[data$labeled$record_id == input$record_id, ]
      meta <- data$meta
      
      fields <- lapply(names(record_raw), function(col) {
        # Skip record_id field
        if (col == "record_id") return(NULL)
        
        # Get field metadata
        field_meta <- meta[meta$field_name == col, ]
        
        # Handle different field types
        if (nrow(field_meta) > 0) {
          # For radio, dropdown, or checkbox fields
          if (field_meta$field_type %in% c("radio", "dropdown", "checkbox")) {
            # Parse choices
            choices <- parse_choices(field_meta$select_choices_or_calculations)
            
            if (field_meta$field_type == "checkbox") {
              # Handle checkbox fields differently (multiple options)
              checkbox_options <- lapply(choices, function(choice) {
                checkboxInput(
                  inputId = paste0(col, "___", choice$code),
                  label = choice$label,
                  value = record_raw[[paste0(col, "___", choice$code)]] == "1"
                )
              })
              return(checkbox_options)
            } else {
              # For radio/dropdown - create select input with labels but store codes
              selected_value <- as.character(record_raw[[col]])
              return(
                selectInput(
                  inputId = col,
                  label = col,
                  choices = setNames(
                    sapply(choices, function(x) x$code),
                    sapply(choices, function(x) x$label)
                  ),
                  selected = selected_value
                )
              )
            }
          } else {
            # For text fields
            return(
              textInput(
                inputId = col,
                label = col,
                value = record_raw[[col]]
              )
            )
          }
        } else {
          # Fallback for fields not in metadata
          return(
            textInput(
              inputId = col,
              label = col,
              value = record_raw[[col]]
            )
          )
        }
      })
      
      # Remove NULL elements and flatten list for checkbox fields
      fields <- Filter(Negate(is.null), fields)
      if (length(fields) > 0) {
        do.call(tagList, fields)
      }
    }
  })
  
  # Helper function to parse REDCap choices string
  parse_choices <- function(choices_str) {
    if (is.null(choices_str) || choices_str == "") return(list())
    
    choices <- strsplit(choices_str, "\\s*\\|\\s*")[[1]]
    lapply(choices, function(choice) {
      parts <- strsplit(choice, "\\s*,\\s*")[[1]]
      if (length(parts) >= 2) {
        list(code = parts[1], label = paste(parts[-1], collapse = ","))
      } else {
        list(code = parts[1], label = parts[1])
      }
    })
  }
  
  # Download handlers should be at the top level, not inside observeEvent
  output$export_csv <- downloadHandler(
    filename = function() {
      paste("redcap-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data <- fetch_data()
      if (!is.null(data)) {
        write.csv(data$labeled, file, row.names = FALSE)
      }
    }
  )
  
  output$export_excel <- downloadHandler(
    filename = function() {
      paste("redcap-data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      data <- fetch_data()
      if (!is.null(data)) {
        writexl::write_xlsx(data$labeled, file)
      }
    }
  )
  
  # Audit logging with user email
  observeEvent(input$save, {
    writeLines(
      sprintf("%s: User %s edited record %s", 
              Sys.time(), 
              user_info()$email, 
              input$record_id),
      "audit.log"
    )
  })
  
  # Save changes
  observeEvent(input$save, {
    data <- fetch_data()
    if (!is.null(data)) {
      # Create a new data frame with just the record being edited
      updated_record <- data$raw[data$raw$record_id == input$record_id, ]
      
      # Get metadata to understand field types
      meta <- data$meta
      
      # Update each field
      for (col in names(updated_record)) {
        # Skip record_id
        if (col == "record_id") next
        
        # Get field metadata
        field_meta <- meta[meta$field_name == col, ]
        
        if (nrow(field_meta) > 0) {
          if (field_meta$field_type == "checkbox") {
            # Handle checkbox fields - they have multiple ___ columns
            choices <- parse_choices(field_meta$select_choices_or_calculations)
            for (choice in choices) {
              checkbox_id <- paste0(col, "___", choice$code)
              if (!is.null(input[[checkbox_id]])) {
                updated_record[[checkbox_id]] <- ifelse(input[[checkbox_id]], "1", "0")
              }
            }
          } else {
            # Update regular fields
            if (!is.null(input[[col]])) {
              updated_record[[col]] <- input[[col]]
            }
          }
        } else {
          # Update fields not in metadata
          if (!is.null(input[[col]])) {
            updated_record[[col]] <- input[[col]]
          }
        }
      }
      
      # Update the record in REDCap
      result <- redcap_write(
        ds_to_write = updated_record,
        redcap_uri = redcap_url,
        token = redcap_token
      )
      
      if (result$success) {
        showNotification("Record updated successfully!", type = "message")
      } else {
        showNotification("Failed to update record.", type = "error")
      }
      
    }
  })
}

#shinyApp(ui, server)
shinyAppAuth0(ui, server)
#options(shiny.port = 8080)
