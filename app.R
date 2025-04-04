
install.packages("auth0", repos = "https://cloud.r-project.org" )
library(shiny)
library(auth0)
library(REDCapR)
library(DT)
library(writexl)


#options(shiny.port = 8080)
#options(auth0_redirect_uri = "http://localhost:8080")

ui <- fluidPage(
  titlePanel("Research Opportunities Editor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("record_id", "Select Record:", choices = NULL),
      uiOutput("field_editor"),
      actionButton("save", "Save Changes", class = "btn-primary")
    ),
    mainPanel(
      fluidRow(
        column(3, downloadButton("export_csv", "Save as CSV", class = "btn-success")),
        column(3, downloadButton("export_excel", "Save as Excel", class = "btn-info"))
      ),
      br(),
      DTOutput("record_table")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store both raw and labeled data
  data_store <- reactiveValues(
    labeled = NULL,
    raw = NULL,
    meta = NULL
  )
  
  # Fetch all required data
  observe({
    req(session$userData$auth0_credentials)
    
    # Get labeled data for display
    labeled <- redcap_read_oneshot(
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN"),
      raw_or_label = "label"
    )$data
    
    # Get raw data for editing
    raw <- redcap_read_oneshot(
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN"),
      raw_or_label = "raw"
    )$data
    
    # Get metadata
    meta <- redcap_metadata_read(
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN")
    )$data
    
    data_store$labeled <- labeled
    data_store$raw <- raw
    data_store$meta <- meta
  })
  
  # Update record selector
  observe({
    req(data_store$raw)
    updateSelectInput(session, "record_id", choices = data_store$raw$record_id)
  })
  
  # Render field editor
  output$field_editor <- renderUI({
    req(input$record_id, data_store$raw, data_store$labeled, data_store$meta)
    
    raw_record <- data_store$raw[data_store$raw$record_id == input$record_id, ]
    labeled_record <- data_store$labeled[data_store$labeled$record_id == input$record_id, ]
    fields <- setdiff(names(raw_record), "record_id")
    
    lapply(fields, function(field) {
      meta <- data_store$meta[data_store$meta$field_name == field, ]
      
      # Handle checkbox fields (special case)
      if (nrow(meta) > 0 && meta$field_type == "checkbox") {
        choices <- parse_choices(meta$select_choices_or_calculations)
        checkbox_options <- lapply(choices, function(choice) {
          checkbox_name <- paste0(field, "___", choice$value)
          checkboxInput(
            inputId = checkbox_name,
            label = choice$label,
            value = raw_record[[checkbox_name]] == "1"
          )
        })
        return(checkbox_options)
      }
      
      # Handle dropdown/radio fields
      if (nrow(meta) > 0 && meta$field_type %in% c("dropdown", "radio")) {
        choices <- parse_choices(meta$select_choices_or_calculations)
        if (length(choices) > 0) {
          return(selectInput(
            inputId = field,
            label = field,
            choices = setNames(sapply(choices, `[[`, "value"), 
                               sapply(choices, `[[`, "label")),
            selected = raw_record[[field]]
          ))
        }
      }
      
      # Default text input
      textInput(field, label = field, value = raw_record[[field]])
    })
  })
  
  # Save handler - writes raw data back to REDCap
  observeEvent(input$save, {
    req(input$record_id, data_store$raw)
    
    # Create updated record from inputs
    updated_record <- data_store$raw[data_store$raw$record_id == input$record_id, ]
    meta <- data_store$meta
    
    # Update each field from inputs
    for (field in names(updated_record)) {
      if (field == "record_id") next
      
      field_meta <- meta[meta$field_name == sub("___.*", "", field), ]
      
      # Handle checkbox fields
      if (nrow(field_meta) > 0 && field_meta$field_type == "checkbox") {
        checkbox_name <- field
        updated_record[[checkbox_name]] <- ifelse(isTRUE(input[[checkbox_name]]), "1", "0")
        next
      }
      
      # Handle regular fields
      if (field %in% names(input)) {
        updated_record[[field]] <- input[[field]]
      }
    }
    
    # Write to REDCap
    result <- redcap_write(
      ds_to_write = updated_record,
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN")
    )
    
    if (result$success) {
      showNotification("Record saved successfully!", type = "message")
      # Refresh data
      data_store$raw <- redcap_read_oneshot(
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN"),
        raw_or_label = "raw"
      )$data
    } else {
      showNotification("Failed to save record", type = "error")
    }
  })
  
  # Helper function to parse REDCap choices
  parse_choices <- function(choices_str) {
    if (is.null(choices_str) || is.na(choices_str) || choices_str == "") {
      return(list())
    }
    
    choices <- strsplit(choices_str, "\\s*\\|\\s*")[[1]]
    lapply(choices, function(choice) {
      parts <- strsplit(trimws(choice), "\\s*,\\s*")[[1]]
      if (length(parts) >= 2) {
        list(value = parts[1], label = paste(parts[-1], collapse = ", "))
      } else {
        list(value = parts[1], label = parts[1])
      }
    })
  }
  
  # Display labeled data table
  output$record_table <- renderDT({
    req(data_store$labeled)
    datatable(data_store$labeled, rownames = FALSE)
  })
  
  # Export handlers
  output$export_csv <- downloadHandler(
    filename = function() paste("opportunities-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_store$labeled, file, row.names = FALSE)
  )
  
  output$export_excel <- downloadHandler(
    filename = function() paste("opportunities-", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) writexl::write_xlsx(data_store$labeled, file)
  )
}

shinyAppAuth0(ui, server)