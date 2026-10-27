# ============================================================================
# SFCN / EMLassemblyline Metadata Workflow Dashboard
# Shiny GUI for configuring the EMLassemblyline/EMLeditor/NPSdataverse workflow
#
# Save this as app.R and run with: shiny::runApp()
# Requires: shiny, jsonlite, lubridate, readr
# ============================================================================

library(shiny)
library(jsonlite)
library(lubridate)
library(readr)
library(shinyjs)
library(htmltools)
library(cli)

# ---- Pipeline dependencies ----
# These match the library stack from the original flat workflow script.
# They're loaded here so that scripts sourced from ./source_code/ during
# "Run Pipeline" have the same functions available (e.g. dplyr's %>%) that
# they would if the whole workflow were run as one flat script.
library(svDialogs)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(EMLassemblyline)
# library(pak)  # only needed once, for installing NPSdataverse — not required at runtime
# pak::pkg_install("nationalparkservice/NPSdataverse")

CONFIG_PATH <- "workflow_config.json"

# ---- Hardcoded paths (no GUI for these — edit directly if your folder
# layout differs from the defaults below) ----
working_folder     <- "./working_folder"
metadata_template  <- "./MetadataTemplate.xlsx"
filled_form_folder <- "./filled_form"

# ---- Helper: default config (mirrors your original hardcoded script) ------
default_config <- list(
  task                   = "metadata",
  geo_data               = FALSE,
  tax_data               = TRUE,
  
  metadata_id            = "SFCN_Forest_Monitoring",
  package_title          = "SFCN Forest Monitoring Protocol Data",
  data_type              = "complete",
  park_units             = c("BICY", "EVER", "BISC", "DRTO", "BUIS", "SARI", "VIIS"),
  producing_units        = c("SFCN"),
  cui_code               = "PUBLIC",
  int_rights             = "public",
  language               = "English",
  ready_to_publish       = FALSE,
  upload_data_package    = FALSE,
  datastore_doi          = "",
  doi                    = "",
  project                = "",
  
  data_taxa_tables       = "2020-2025_SFCN_Forest_Diversity_v1.csv",
  data_taxa_fields       = "harmonized_name",
  
  data_coordinates_table = "BiscayneNP_CreelInterviewData.csv",
  data_latitude          = "x_coord",
  data_longitude         = "y_coord",
  data_sitename          = "marker_point",
  
  data_dates_table       = "2020-2025_SFCN_Forest_Events_v1.csv",
  data_startdate_field   = "eval_date",
  data_enddate_field     = "event_end_date"
)

ALL_CUI_CODES  <- c("PUBLIC", "FED ONLY", "FEDCON", "DL ONLY", "NOCON")
ALL_INT_RIGHTS <- c("public", "restricted", "CC0")

# Load existing config if present, else use defaults
load_config <- function() {
  if (file.exists(CONFIG_PATH)) {
    cfg <- tryCatch(jsonlite::fromJSON(CONFIG_PATH, simplifyVector = TRUE),
                    error = function(e) NULL)
    if (!is.null(cfg)) {
      merged <- modifyList(default_config, cfg)
      return(merged)
    }
  }
  default_config
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  title = "SFCN Metadata Workflow Config",
  useShinyjs(),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('scroll-log', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) { el.scrollTop = el.scrollHeight; }
    });
  ")),
  tags$head(tags$style(HTML("
    body { background-color: #f7f8fa; font-size: 16px; }
    .well { background-color: #ffffff; border: 1px solid #e2e4e8; }
    h3 { margin-top: 0; }
    label, .control-label { font-size: 15px; }
    .form-control, select.form-control { font-size: 15px; }
    p, .help-block, .shiny-input-container small { font-size: 14.5px; }
    .section-title {
      font-weight: 600; font-size: 17px; color: #2c3e50;
      border-bottom: 2px solid #4a90d9; padding-bottom: 4px; margin-bottom: 14px;
    }
    .derived-box {
      background: #eef3fb; border-radius: 6px; padding: 10px 14px;
      font-family: monospace; font-size: 14px;
    }
    .code-preview {
      background: #1e1e1e; color: #d4d4d4; padding: 12px; border-radius: 6px;
      font-family: monospace; font-size: 13.5px; white-space: pre-wrap;
      max-height: 480px; overflow-y: auto;
    }
    .page-nav { margin: 18px 0; }
    .btn { font-size: 15px; }
  "))),
  
  titlePanel("Metadata Dashboard"),
  
  # ---------------- PAGE 1 ----------------
  conditionalPanel(
    condition = "output.current_page == 1",
    p("Step 1 of 2 — choose your workflow."),
    fluidRow(
      column(6,
           wellPanel(
  div(class = "section-title", "Workflow Toggle"),
  selectInput("task", "task", choices = c("workbook", "fix", "metadata", "reboot")),
  
  tags$ul(style = "font-size:14.5px; color:#444; padding-left:18px;",
    tags$li(tags$strong("workbook"), " – Start here. Builds the Excel metadata form from your CSVs."),
    tags$li(tags$strong("fix"), " – Add, rename, reclass, or delete attributes after the workbook exists."),
    tags$li(tags$strong("metadata"), " – Final step. Turns the filled-in Excel form into the EML file."),
    tags$li(tags$strong("reboot"), " – Rebuilds the Excel workbook from existing txt files.")
  ),
  
  conditionalPanel(
    condition = "input.task == 'workbook'",
    tags$hr(),
    checkboxInput("geo_data", "geo_data", value = FALSE),
    helpText("Include geographic coverage (coordinates/site names)."),
    checkboxInput("tax_data", "tax_data", value = TRUE),
    helpText("Include taxonomic coverage (species names column).")
  )
)
      )
    ),
    div(class = "page-nav",
        actionButton("to_page2", "Next →", class = "btn-primary", width = "200px")
    )
  ),
  
  # ---------------- PAGE 2 ----------------
  conditionalPanel(
    condition = "output.current_page == 2",
    p("Step 2 of 2 — fields shown below depend on the task you picked (currently: ",
      tags$strong(textOutput("current_task_label", inline = TRUE)), ")."),
    uiOutput("scan_status_box"),
    
    fluidRow(
      column(12,
             wellPanel(
               div(class = "section-title", "File Name & Title"),
               textInput("metadata_id", "metadata_id", value = ""),
               helpText("Becomes <metadata_id>_metadata.xml"),
               textInput("package_title", "package_title (5+ words)", value = ""),
               uiOutput("title_word_warning")
             )
      )
    ),
    
    # ---- task == "metadata": Data Info ----
    conditionalPanel(
      condition = "input.task == 'metadata'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Data Info"),
                 selectInput("data_type", "data_type", choices = c("ongoing", "complete")),
                 textInput("park_units", "park_units (comma-separated)", value = ""),
                 textInput("producing_units", "producing_units (comma-separated)", value = ""),
                 selectInput("cui_code", "cui_code", choices = ALL_CUI_CODES),
                 selectInput("int_rights", "int_rights", choices = ALL_INT_RIGHTS),
                 textInput("language", "language", value = "English")
               )
        )
      ),
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Publishing"),
                 checkboxInput("ready_to_publish", "Ready to Publish", value = FALSE),
                 p(style = "color:#555; font-size:15px;",
                   "When unchecked, the pipeline will build and write the EML file but will ",
                   "NOT set a DataStore DOI/project or upload the data package — those steps are ",
                   "skipped entirely. Check this only once the metadata is finalized and approved."),
                 conditionalPanel(
                   condition = "input.ready_to_publish == true",
                   textInput("datastore_doi", "datastore_doi", value = ""),
                   textInput("doi", "doi", value = ""),
                   textInput("project", "project (DataStore Reference ID)", value = ""),
                   tags$hr(),
                   checkboxInput("upload_data_package", "Also upload the data package to DataStore", value = FALSE),
                   helpText("Unchecked: the DOI/project are set, but nothing is uploaded. ",
                            "Checked: the data package files are uploaded as well.")
                 )
                 
               )
        )
      ),
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Run Checks"),
                 p(style = "color:#555; font-size:15px;",
                   "Optional, independent of Run Pipeline. Requires that 10_EML_creation.R has ",
                   "already been run at least once so metadata_id_metadata.xml exists."),
                 fluidRow(
                   column(6, actionButton("run_eml_validate_btn", "🔎 Run EML Validate", width = "100%")),
                   column(6, actionButton("run_dpchecker_btn", "🔎 Run DPchecker Congruence Checks", width = "100%"))
                 ),
                 br(),
                 tags$pre(id = "checks_log_box",
                          style = "background:#fdfdfd; color:#1a1a1a; border:1px solid #d5d8dc; padding:12px; border-radius:6px; font-size:13px; font-family: Consolas, 'Courier New', monospace; line-height:1.45; max-height:420px; overflow-y:auto; white-space:pre-wrap; word-break:break-word;",
                          "")
               )
        )
      )
    ),
    
    # ---- task == "workbook": Taxonomy / Geospatial / Date Info ----
    conditionalPanel(
      condition = "input.task == 'workbook'",
      fluidRow(
        column(4,
               div(id = "taxonomy_section",
                   wellPanel(
                     div(class = "section-title", "Taxonomy Info"),
                     selectInput("data_taxa_tables", "data_taxa_tables (file)", choices = character(0)),
                     selectInput("data_taxa_fields", "data_taxa_fields (column)", choices = character(0))
                   )
               )
        ),
        column(4,
               div(id = "geospatial_section",
                   wellPanel(
                     div(class = "section-title", "Geospatial Info"),
                     selectInput("data_coordinates_table", "data_coordinates_table (file)", choices = character(0)),
                     selectInput("data_latitude", "data_latitude (column)", choices = character(0)),
                     selectInput("data_longitude", "data_longitude (column)", choices = character(0)),
                     selectInput("data_sitename", "data_sitename (column)", choices = character(0))
                   )
               )
        ),
        column(4,
               wellPanel(
                 div(class = "section-title", "Date Info"),
                 selectInput("data_dates_table", "data_dates_table (file)", choices = character(0)),
                 selectInput("data_startdate_field", "data_startdate_field (column)", choices = character(0)),
                 selectInput("data_enddate_field", "data_enddate_field (column)", choices = character(0)),
                 actionButton("calc_dates", "Calculate start/end dates", icon = icon("calendar"), width = "100%"),
                 uiOutput("date_range_output")
               )
        )
      )
    ),
    
    # ---- task == "fix": Add / Rename / Reclass / Delete attribute ----
    conditionalPanel(
      condition = "input.task == 'fix'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Attribute Fix"),
                 p(style = "color:#555; font-size:15px;",
                   "Syncs from the filled Excel workbook (excel → txt), lets you queue up ",
                   "add/rename/reclass/delete edits below, then rebuilds the workbook (txt → excel) ",
                   "when you click Apply Fixes."),
                 actionButton("sync_from_excel_btn", "🔄 Sync from Excel (run excel → txt)", width = "320px"),
                 uiOutput("fix_sync_status")
               )
        )
      ),
      fluidRow(
        column(6,
               wellPanel(
                 div(class = "section-title", "➕ Add Attribute"),
                 selectInput("add_table", "Table", choices = character(0)),
                 textInput("add_name", "attributeName", value = ""),
                 textInput("add_definition", "attributeDefinition", value = ""),
                 selectInput("add_class", "class", choices = c("numeric", "categorical", "character", "Date")),
                 textInput("add_unit", "unit (numeric only)", value = ""),
                 textInput("add_datetime_format", "dateTimeFormatString (Date only)", value = ""),
                 textInput("add_missing_code", "missingValueCode", value = ""),
                 textInput("add_missing_explanation", "missingValueCodeExplanation", value = ""),
                 actionButton("queue_add", "Queue: Add Attribute", width = "100%")
               )
        ),
        column(6,
               wellPanel(
                 div(class = "section-title", "✏️ Rename Attribute"),
                 selectInput("rename_table", "Table", choices = character(0)),
                 selectInput("rename_old_name", "Existing attribute", choices = character(0)),
                 textInput("rename_new_name", "New name", value = ""),
                 actionButton("queue_rename", "Queue: Rename Attribute", width = "100%")
               )
        )
      ),
      fluidRow(
        column(6,
               wellPanel(
                 div(class = "section-title", "🔁 Reclass Attribute"),
                 selectInput("reclass_table", "Table", choices = character(0)),
                 selectInput("reclass_name", "Existing attribute", choices = character(0)),
                 selectInput("reclass_new_class", "New class", choices = c("numeric", "categorical", "character", "Date")),
                 p(style = "color:#777; font-size:14px;", "Changing to 'categorical' auto-generates catvars codes from the CSV's unique values (definitions left blank to fill in). Changing away from 'categorical' removes its catvars rows."),
                 actionButton("queue_reclass", "Queue: Reclass Attribute", width = "100%")
               )
        ),
        column(6,
               wellPanel(
                 div(class = "section-title", "🗑 Delete Attribute"),
                 selectInput("delete_table", "Table", choices = character(0)),
                 selectInput("delete_name", "Existing attribute", choices = character(0)),
                 p(style = "color:#777; font-size:14px;", "Also removes any catvars rows for this attribute, if it was categorical."),
                 actionButton("queue_delete", "Queue: Delete Attribute", width = "100%")
               )
        )
      ),
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Pending Edits Queue"),
                 uiOutput("fix_queue_box"),
                 br(),
                 actionButton("apply_fixes_btn", "✅ Apply Fixes (run txt edits + rebuild Excel)", class = "btn-primary", width = "420px"),
                 br(), br(),
                 tags$pre(id = "fix_log_box",
                          style = "background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; font-size:14px; max-height:260px; overflow-y:auto; white-space:pre-wrap;",
                          "")
               )
        )
      )
    ),
    
    div(class = "page-nav",
        actionButton("to_page1", "← Back", width = "160px"),
        conditionalPanel(
          condition = "input.task == 'workbook'",
          actionButton("rescan_btn", "🔄 Re-scan folder", width = "200px")
        )
    ),
    
    # ---- task == "reboot": nothing extra needed beyond File Name & Title ----
    conditionalPanel(
      condition = "input.task == 'reboot'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Reboot"),
                 p(style = "color:#555; font-size:15px;",
                   "Rebuilds the Excel workbook directly from the existing txt files in working_folder. ",
                   "No other setup needed — just click Run below.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.task != 'fix'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Run Pipeline"),
                 conditionalPanel(
                   condition = "input.task == 'workbook'",
                   p(style = "color:#555; font-size:15px;",
                     "Runs the numbered scripts in ./source_code based on your current task/toggles. ",
                     "Some steps may open interactive dialogs/tables outside the browser (e.g. in RStudio) — ",
                     "watch the R console and resolve those before continuing.")
                 ),
                 actionButton("run_pipeline_btn", "▶ Run", class = "btn-primary", width = "260px"),
                 br(), br(),
                 tags$div(id = "pipeline_steps_box", class = "derived-box", "No pipeline run yet."),
                 br(),
                 tags$pre(id = "pipeline_log_box",
                          style = "background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; font-size:14px; max-height:300px; overflow-y:auto; white-space:pre-wrap;",
                          ""),
                 br(),
                 tags$div(id = "pipeline_done_banner", style = "display:none;")
               )
        )
      )
    ),
    
    fluidRow(
      column(12,
             wellPanel(
               div(class = "section-title", "Save / Load / Export"),
               fluidRow(
                 column(3, actionButton("save_btn", "💾 Save answers to JSON", width = "100%", class = "btn-primary")),
                 column(3, actionButton("load_btn", "📂 Reload saved JSON", width = "100%")),
                 column(3, actionButton("reset_btn", "↺ Reset to defaults", width = "100%")),
                 column(3, downloadButton("download_json", "⬇ Download JSON", style = "width:100%"))
               ),
               br(),
               verbatimTextOutput("status_msg")
             )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  cfg <- reactiveVal(load_config())
  status <- reactiveVal("")
  page <- reactiveVal(1)
  scan_status <- reactiveVal("")
  
  output$current_page <- reactive({ page() })
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
  
  output$current_task_label <- renderText(input$task)
  
  # ---- Initialize UI from loaded config (page 1 + page 2 shared fields) ----
  observe({
    c0 <- isolate(cfg())
    updateSelectInput(session, "task", selected = c0$task)
    updateCheckboxInput(session, "geo_data", value = isTRUE(c0$geo_data))
    updateCheckboxInput(session, "tax_data", value = isTRUE(c0$tax_data))
    updateTextInput(session, "metadata_id", value = c0$metadata_id)
    updateTextInput(session, "package_title", value = c0$package_title)
    updateSelectInput(session, "data_type", selected = c0$data_type)
    updateTextInput(session, "park_units", value = paste(c0$park_units, collapse = ", "))
    updateTextInput(session, "producing_units", value = paste(c0$producing_units, collapse = ", "))
    updateSelectInput(session, "cui_code", selected = c0$cui_code)
    updateSelectInput(session, "int_rights", selected = c0$int_rights)
    updateTextInput(session, "language", value = c0$language)
    updateCheckboxInput(session, "ready_to_publish", value = isTRUE(c0$ready_to_publish))
    updateCheckboxInput(session, "upload_data_package", value = isTRUE(c0$upload_data_package))
    updateTextInput(session, "datastore_doi", value = c0$datastore_doi)
    updateTextInput(session, "doi", value = c0$doi)
    updateTextInput(session, "project", value = c0$project)
  })
  
  # ---- Page navigation ----
  observeEvent(input$to_page2, {
    page(2)
    if (input$task == "workbook") {
      do_scan(preserve_selection = TRUE)
    }
  })
  observeEvent(input$to_page1, { page(1) })
  observeEvent(input$rescan_btn, { do_scan(preserve_selection = TRUE) })
  
  # ---- Hide Taxonomy & Geospatial sections when their corresponding toggle
  # (tax_data / geo_data) is unchecked. The underlying inputs stay in the
  # DOM (just hidden via CSS), so any values already entered are preserved
  # and reappear if the section is shown again later.
  observe({
    if (isTRUE(input$tax_data)) {
      shinyjs::show(id = "taxonomy_section")
    } else {
      shinyjs::hide(id = "taxonomy_section")
    }
  })
  
  observe({
    if (isTRUE(input$geo_data)) {
      shinyjs::show(id = "geospatial_section")
    } else {
      shinyjs::hide(id = "geospatial_section")
    }
  })
  
  output$taxonomy_disabled_note <- renderUI({
    if (!isTRUE(input$tax_data)) {
      tags$p(style = "color:#9aa3ad; font-size:12px; font-style:italic;",
             "tax_data is unchecked on page 1 — these fields won't be used, but your answers are kept.")
    }
  })
  
  output$geospatial_disabled_note <- renderUI({
    if (!isTRUE(input$geo_data)) {
      tags$p(style = "color:#9aa3ad; font-size:12px; font-style:italic;",
             "geo_data is unchecked on page 1 — these fields won't be used, but your answers are kept.")
    }
  })
  
  # ---- Scan working_folder for CSVs, populate the three file dropdowns ----
  do_scan <- function(preserve_selection = TRUE) {
    wf <- working_folder
    c0 <- isolate(cfg())
    
    if (is.null(wf) || wf == "" || !dir.exists(wf)) {
      scan_status(sprintf("⚠️ Folder not found: '%s'. Check the hardcoded working_folder path in app.R.", wf))
      updateSelectInput(session, "data_taxa_tables", choices = character(0))
      updateSelectInput(session, "data_coordinates_table", choices = character(0))
      updateSelectInput(session, "data_dates_table", choices = character(0))
      return(invisible())
    }
    
    files <- list.files(wf, pattern = "\\.csv$", full.names = FALSE)
    if (length(files) == 0) {
      scan_status(sprintf("⚠️ No CSV files found in '%s'.", wf))
      updateSelectInput(session, "data_taxa_tables", choices = character(0))
      updateSelectInput(session, "data_coordinates_table", choices = character(0))
      updateSelectInput(session, "data_dates_table", choices = character(0))
      return(invisible())
    }
    
    pick <- function(saved, current) {
      if (preserve_selection) {
        if (!is.null(saved) && saved %in% files) return(saved)
        if (!is.null(current) && current %in% files) return(current)
      }
      files[1]
    }
    
    updateSelectInput(session, "data_taxa_tables", choices = files,
                      selected = pick(c0$data_taxa_tables, isolate(input$data_taxa_tables)))
    updateSelectInput(session, "data_coordinates_table", choices = files,
                      selected = pick(c0$data_coordinates_table, isolate(input$data_coordinates_table)))
    updateSelectInput(session, "data_dates_table", choices = files,
                      selected = pick(c0$data_dates_table, isolate(input$data_dates_table)))
    
    scan_status(sprintf("✅ Found %d CSV file(s) in '%s'.", length(files), wf))
  }
  
  # ---- Helper: read just the header row of a CSV, robustly ----
  # Uses readr::read_csv with n_max = 0 so it parses zero data rows but still
  # gets correct quoting, embedded-comma handling, and BOM handling —
  # matching how the real pipeline will read the file later.
  get_csv_headers <- function(filename) {
    if (is.null(filename) || filename == "") return(character(0))
    f <- file.path(working_folder, filename)
    if (!file.exists(f)) return(character(0))
    headers <- tryCatch({
      df <- suppressWarnings(suppressMessages(
        readr::read_csv(f, n_max = 0, show_col_types = FALSE, progress = FALSE)
      ))
      trimws(names(df))
    }, error = function(e) character(0))
    headers
  }
  
  # ---- Populate column dropdowns whenever the source file changes ----
  observeEvent(input$data_taxa_tables, {
    c0 <- isolate(cfg())
    cols <- get_csv_headers(input$data_taxa_tables)
    sel <- if (!is.null(c0$data_taxa_fields) && c0$data_taxa_fields %in% cols) c0$data_taxa_fields else character(0)
    updateSelectInput(session, "data_taxa_fields", choices = cols, selected = sel)
  }, ignoreInit = FALSE)
  
  observeEvent(input$data_coordinates_table, {
    c0 <- isolate(cfg())
    cols <- get_csv_headers(input$data_coordinates_table)
    pick <- function(saved) if (!is.null(saved) && saved %in% cols) saved else character(0)
    updateSelectInput(session, "data_latitude", choices = cols, selected = pick(c0$data_latitude))
    updateSelectInput(session, "data_longitude", choices = cols, selected = pick(c0$data_longitude))
    updateSelectInput(session, "data_sitename", choices = cols, selected = pick(c0$data_sitename))
  }, ignoreInit = FALSE)
  
  observeEvent(input$data_dates_table, {
    c0 <- isolate(cfg())
    cols <- get_csv_headers(input$data_dates_table)
    pick <- function(saved) if (!is.null(saved) && saved %in% cols) saved else character(0)
    updateSelectInput(session, "data_startdate_field", choices = cols, selected = pick(c0$data_startdate_field))
    updateSelectInput(session, "data_enddate_field", choices = cols, selected = pick(c0$data_enddate_field))
  }, ignoreInit = FALSE)
  
  output$scan_status_box <- renderUI({
    msg <- scan_status()
    if (msg == "") return(NULL)
    is_warning <- grepl("^⚠️", msg)
    tags$div(
      style = sprintf("padding:10px 14px; border-radius:6px; margin-bottom:14px; font-size:15px; %s",
                      if (is_warning) "background:#fdecea; color:#c0392b;" else "background:#eafaf1; color:#1e8449;"),
      msg
    )
  })
  
  # ---- Word count warning for title ----
  output$title_word_warning <- renderUI({
    n <- length(strsplit(trimws(input$package_title), "\\s+")[[1]])
    if (input$package_title == "") return(NULL)
    if (n < 5) {
      tags$p(style = "color:#c0392b; font-size:14px;", sprintf("⚠️ Only %d word(s) — needs 5 or more.", n))
    } else {
      tags$p(style = "color:#27ae60; font-size:14px;", sprintf("✅ %d words.", n))
    }
  })
  
  # ---- Calculate start/end dates from selected file ----
  observeEvent(input$calc_dates, {
    wf <- working_folder
    f <- file.path(wf, input$data_dates_table)
    if (!file.exists(f)) {
      output$date_range_output <- renderUI(tags$p(style = "color:#c0392b; font-size:14px;", paste("⚠️ File not found:", f)))
      return(invisible())
    }
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !(input$data_startdate_field %in% names(df)) || !(input$data_enddate_field %in% names(df))) {
      output$date_range_output <- renderUI(tags$p(style = "color:#c0392b; font-size:14px;", "⚠️ Column(s) not found in file."))
      return(invisible())
    }
    start_parsed <- suppressWarnings(as.Date(min(lubridate::parse_date_time(
      df[[input$data_startdate_field]], orders = c("ymd", "mdy", "dmy")), na.rm = TRUE)))
    end_parsed <- suppressWarnings(as.Date(max(lubridate::parse_date_time(
      df[[input$data_enddate_field]], orders = c("ymd", "mdy", "dmy")), na.rm = TRUE)))
    output$date_range_output <- renderUI(
      tags$div(class = "derived-box", style = "margin-top:8px;",
               sprintf("startdate: %s\nenddate:   %s", start_parsed, end_parsed))
    )
  })
  
  # ==========================================================================
  # ---- "fix" task: Add / Rename / Reclass / Delete attribute workflow ----
  # ==========================================================================
  ATTR_COLS <- c("attributeName", "attributeDefinition", "class", "unit",
                 "dateTimeFormatString", "missingValueCode", "missingValueCodeExplanation")
  
  fix_tables <- reactiveVal(character(0))   # table base names, e.g. "2020-2025_SFCN_Forest_Diversity_v1"
  fix_queue  <- reactiveVal(list())         # list of pending edit operations
  
  attr_file_for   <- function(tbl) file.path(working_folder, paste0("attributes_", tbl, ".txt"))
  catvars_file_for <- function(tbl) file.path(working_folder, paste0("catvars_", tbl, ".txt"))
  
  read_attr_file <- function(tbl) {
    f <- attr_file_for(tbl)
    if (!file.exists(f)) return(NULL)
    df <- tryCatch(read.delim(f, header = TRUE, stringsAsFactors = FALSE, colClasses = "character"),
                   error = function(e) NULL)
    df
  }
  
  write_attr_file <- function(tbl, df) {
    write.table(df, attr_file_for(tbl), sep = "\t", row.names = FALSE, quote = FALSE, na = "")
  }
  
  read_catvars_file <- function(tbl) {
    f <- catvars_file_for(tbl)
    if (!file.exists(f)) return(data.frame(attributeName = character(0), code = character(0), definition = character(0), stringsAsFactors = FALSE))
    df <- tryCatch(read.delim(f, header = FALSE, stringsAsFactors = FALSE, colClasses = "character"),
                   error = function(e) NULL)
    if (is.null(df)) return(data.frame(attributeName = character(0), code = character(0), definition = character(0), stringsAsFactors = FALSE))
    colnames(df) <- c("attributeName", "code", "definition")[seq_len(ncol(df))]
    df
  }
  
  write_catvars_file <- function(tbl, df) {
    write.table(df, catvars_file_for(tbl), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
  }
  
  # ---- Refresh the list of available tables (from attributes_*.txt present in working_folder) ----
  refresh_fix_tables <- function() {
    files <- list.files(working_folder, pattern = "^attributes_.*\\.txt$", full.names = FALSE)
    tbls <- sub("^attributes_", "", sub("\\.txt$", "", files))
    fix_tables(tbls)
    updateSelectInput(session, "add_table", choices = tbls)
    updateSelectInput(session, "rename_table", choices = tbls)
    updateSelectInput(session, "reclass_table", choices = tbls)
    updateSelectInput(session, "delete_table", choices = tbls)
  }
  
  # ---- Update attribute-name dropdowns whenever their table selection changes ----
  observeEvent(input$rename_table, {
    df <- read_attr_file(input$rename_table)
    updateSelectInput(session, "rename_old_name", choices = if (!is.null(df)) df$attributeName else character(0))
  }, ignoreInit = FALSE)
  
  observeEvent(input$reclass_table, {
    df <- read_attr_file(input$reclass_table)
    updateSelectInput(session, "reclass_name", choices = if (!is.null(df)) df$attributeName else character(0))
  }, ignoreInit = FALSE)
  
  observeEvent(input$delete_table, {
    df <- read_attr_file(input$delete_table)
    updateSelectInput(session, "delete_name", choices = if (!is.null(df)) df$attributeName else character(0))
  }, ignoreInit = FALSE)
  
  # ---- "Sync from Excel" button: runs 09_excel_to_txt.R, then refreshes tables ----
  output$fix_sync_status <- renderUI(NULL)
  observeEvent(input$sync_from_excel_btn, {
    result <- tryCatch({
      source(file.path("source_code", "09_excel_to_txt.R"), local = .GlobalEnv)
      TRUE
    }, error = function(e) {
      output$fix_sync_status <<- renderUI(
        tags$p(style = "color:#c0392b; font-size:15px;", paste("❌ Sync failed:", conditionMessage(e)))
      )
      FALSE
    })
    if (isTRUE(result)) {
      refresh_fix_tables()
      output$fix_sync_status <- renderUI(
        tags$p(style = "color:#1e8449; font-size:15px;", "✅ Synced from Excel. Tables refreshed below.")
      )
    }
  })
  
  # Also try an initial (silent) table refresh in case txt files already exist
  observe({
    if (input$task == "fix" && page() == 2) {
      isolate(refresh_fix_tables())
    }
  })
  
  # ---- Queue buttons: each appends a pending operation to fix_queue ----
  add_to_queue <- function(op) {
    q <- fix_queue()
    q[[length(q) + 1]] <- op
    fix_queue(q)
  }
  
  observeEvent(input$queue_add, {
    req(input$add_table, input$add_name)
    add_to_queue(list(
      type = "add", table = input$add_table, name = input$add_name,
      definition = input$add_definition, class = input$add_class,
      unit = input$add_unit, datetime_format = input$add_datetime_format,
      missing_code = input$add_missing_code, missing_explanation = input$add_missing_explanation
    ))
  })
  
  observeEvent(input$queue_rename, {
    req(input$rename_table, input$rename_old_name, input$rename_new_name)
    add_to_queue(list(
      type = "rename", table = input$rename_table,
      old_name = input$rename_old_name, new_name = input$rename_new_name
    ))
  })
  
  observeEvent(input$queue_reclass, {
    req(input$reclass_table, input$reclass_name, input$reclass_new_class)
    add_to_queue(list(
      type = "reclass", table = input$reclass_table,
      name = input$reclass_name, new_class = input$reclass_new_class
    ))
  })
  
  observeEvent(input$queue_delete, {
    req(input$delete_table, input$delete_name)
    add_to_queue(list(type = "delete", table = input$delete_table, name = input$delete_name))
  })
  
  # ---- Render the pending queue with per-item remove buttons ----
  output$fix_queue_box <- renderUI({
    q <- fix_queue()
    if (length(q) == 0) return(tags$p(style = "color:#888; font-size:15px;", "No pending edits queued yet."))
    rows <- lapply(seq_along(q), function(i) {
      op <- q[[i]]
      label <- switch(op$type,
                      add     = sprintf("➕ Add '%s' (%s) to %s", op$name, op$class, op$table),
                      rename  = sprintf("✏️ Rename '%s' → '%s' in %s", op$old_name, op$new_name, op$table),
                      reclass = sprintf("🔁 Reclass '%s' → %s in %s", op$name, op$new_class, op$table),
                      delete  = sprintf("🗑 Delete '%s' from %s", op$name, op$table)
      )
      fluidRow(
        column(10, tags$p(style = "margin-bottom:4px; font-size:15px;", label)),
        column(2, actionButton(paste0("remove_queue_", i), "Remove", class = "btn-sm"))
      )
    })
    do.call(tagList, rows)
  })
  
  observe({
    q <- fix_queue()
    lapply(seq_along(q), function(i) {
      local({
        idx <- i
        observeEvent(input[[paste0("remove_queue_", idx)]], {
          cur <- fix_queue()
          cur[[idx]] <- NULL
          fix_queue(cur)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
  })
  
  # ---- Apply Fixes: process the whole queue, then rebuild Excel via script 08 ----
  observeEvent(input$apply_fixes_btn, {
    q <- fix_queue()
    log_lines_fix <- character(0)
    appendf <- function(msg) {
      log_lines_fix <<- c(log_lines_fix, msg)
      shinyjs::html(id = "fix_log_box", html = paste(log_lines_fix, collapse = "\n"))
    }
    
    if (length(q) == 0) {
      appendf("⚠️ No pending edits to apply.")
      return(invisible())
    }
    
    appendf(paste("▶ Applying", length(q), "edit(s) at", format(Sys.time(), "%H:%M:%S")))
    
    csv_for_table <- function(tbl) {
      candidate <- file.path(working_folder, paste0(tbl, ".csv"))
      if (file.exists(candidate)) candidate else NA
    }
    
    success <- TRUE
    for (op in q) {
      tbl <- op$table
      attr_df <- read_attr_file(tbl)
      if (is.null(attr_df)) {
        appendf(sprintf("❌ Could not read attributes file for table '%s' — skipping.", tbl))
        success <- FALSE
        next
      }
      
      if (op$type == "add") {
        new_row <- setNames(as.list(rep("", length(ATTR_COLS))), ATTR_COLS)
        new_row$attributeName <- op$name
        new_row$attributeDefinition <- op$definition
        new_row$class <- op$class
        new_row$unit <- if (op$class == "numeric") op$unit else ""
        new_row$dateTimeFormatString <- if (op$class == "Date") op$datetime_format else ""
        new_row$missingValueCode <- op$missing_code
        new_row$missingValueCodeExplanation <- op$missing_explanation
        attr_df <- rbind(attr_df, as.data.frame(new_row, stringsAsFactors = FALSE))
        write_attr_file(tbl, attr_df)
        appendf(sprintf("✅ Added '%s' to %s", op$name, tbl))
        
        if (op$class == "categorical") {
          csv_path <- csv_for_table(tbl)
          if (!is.na(csv_path) && op$name %in% names(suppressMessages(suppressWarnings(readr::read_csv(csv_path, n_max = 0, show_col_types = FALSE))))) {
            vals <- tryCatch(unique(na.omit(readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)[[op$name]])),
                             error = function(e) character(0))
            if (length(vals) > 0) {
              cv <- read_catvars_file(tbl)
              new_cv <- data.frame(attributeName = op$name, code = as.character(vals), definition = "", stringsAsFactors = FALSE)
              cv <- rbind(cv, new_cv)
              write_catvars_file(tbl, cv)
              appendf(sprintf("  ↳ Auto-generated %d catvars code(s) for '%s' (definitions left blank)", length(vals), op$name))
            }
          }
        }
        
      } else if (op$type == "rename") {
        idx <- which(attr_df$attributeName == op$old_name)
        if (length(idx) == 0) {
          appendf(sprintf("❌ '%s' not found in %s — skipping rename.", op$old_name, tbl))
          success <- FALSE
          next
        }
        attr_df$attributeName[idx] <- op$new_name
        write_attr_file(tbl, attr_df)
        cv <- read_catvars_file(tbl)
        if (nrow(cv) > 0 && any(cv$attributeName == op$old_name)) {
          cv$attributeName[cv$attributeName == op$old_name] <- op$new_name
          write_catvars_file(tbl, cv)
        }
        appendf(sprintf("✅ Renamed '%s' → '%s' in %s", op$old_name, op$new_name, tbl))
        
      } else if (op$type == "reclass") {
        idx <- which(attr_df$attributeName == op$name)
        if (length(idx) == 0) {
          appendf(sprintf("❌ '%s' not found in %s — skipping reclass.", op$name, tbl))
          success <- FALSE
          next
        }
        old_class <- attr_df$class[idx]
        attr_df$class[idx] <- op$new_class
        write_attr_file(tbl, attr_df)
        appendf(sprintf("✅ Reclassed '%s' from %s → %s in %s", op$name, old_class, op$new_class, tbl))
        
        cv <- read_catvars_file(tbl)
        if (op$new_class == "categorical" && old_class != "categorical") {
          csv_path <- csv_for_table(tbl)
          if (!is.na(csv_path)) {
            vals <- tryCatch(unique(na.omit(readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)[[op$name]])),
                             error = function(e) character(0))
            if (length(vals) > 0) {
              new_cv <- data.frame(attributeName = op$name, code = as.character(vals), definition = "", stringsAsFactors = FALSE)
              cv <- rbind(cv, new_cv)
              write_catvars_file(tbl, cv)
              appendf(sprintf("  ↳ Auto-generated %d catvars code(s) for '%s' (definitions left blank)", length(vals), op$name))
            } else {
              appendf(sprintf("  ⚠️ Could not read unique values for '%s' from CSV — no catvars generated.", op$name))
            }
          } else {
            appendf(sprintf("  ⚠️ Could not find CSV for table '%s' — no catvars generated.", tbl))
          }
        } else if (op$new_class != "categorical" && old_class == "categorical") {
          if (nrow(cv) > 0 && any(cv$attributeName == op$name)) {
            cv <- cv[cv$attributeName != op$name, ]
            write_catvars_file(tbl, cv)
            appendf(sprintf("  ↳ Removed catvars rows for '%s' (no longer categorical)", op$name))
          }
        }
        
      } else if (op$type == "delete") {
        idx <- which(attr_df$attributeName == op$name)
        if (length(idx) == 0) {
          appendf(sprintf("❌ '%s' not found in %s — skipping delete.", op$name, tbl))
          success <- FALSE
          next
        }
        attr_df <- attr_df[-idx, ]
        write_attr_file(tbl, attr_df)
        cv <- read_catvars_file(tbl)
        if (nrow(cv) > 0 && any(cv$attributeName == op$name)) {
          cv <- cv[cv$attributeName != op$name, ]
          write_catvars_file(tbl, cv)
        }
        appendf(sprintf("✅ Deleted '%s' from %s", op$name, tbl))
      }
    }
    
    # ---- Rebuild the Excel workbook from the now-edited txt files ----
    appendf("⏳ Rebuilding Excel workbook (txt → excel)...")
    rebuild_ok <- tryCatch({
      source(file.path("source_code", "08_txt_to_excel.R"), local = .GlobalEnv)
      TRUE
    }, error = function(e) {
      appendf(paste("❌ Error rebuilding Excel workbook:", conditionMessage(e)))
      FALSE
    })
    
    if (isTRUE(rebuild_ok)) {
      appendf("✅ Excel workbook rebuilt successfully.")
      fix_queue(list())
      refresh_fix_tables()
    }
    
    appendf(paste("■ Apply Fixes finished at", format(Sys.time(), "%H:%M:%S")))
  })
  
  # ---- Build config list from current inputs ----
  build_config <- reactive({
    list(
      working_folder         = working_folder,
      metadata_template      = metadata_template,
      filled_form_folder     = filled_form_folder,
      task                   = input$task,
      geo_data               = input$geo_data,
      tax_data               = input$tax_data,
      metadata_id            = input$metadata_id,
      package_title          = input$package_title,
      data_type              = input$data_type,
      park_units             = trimws(strsplit(input$park_units, ",")[[1]]),
      producing_units        = trimws(strsplit(input$producing_units, ",")[[1]]),
      cui_code               = input$cui_code,
      int_rights             = input$int_rights,
      language               = input$language,
      ready_to_publish       = input$ready_to_publish,
      upload_data_package    = input$upload_data_package,
      datastore_doi          = input$datastore_doi,
      doi                    = input$doi,
      project                = input$project,
      data_taxa_tables       = input$data_taxa_tables,
      data_taxa_fields       = input$data_taxa_fields,
      data_coordinates_table = input$data_coordinates_table,
      data_latitude          = input$data_latitude,
      data_longitude         = input$data_longitude,
      data_sitename          = input$data_sitename,
      data_dates_table       = input$data_dates_table,
      data_startdate_field   = input$data_startdate_field,
      data_enddate_field     = input$data_enddate_field
    )
  })
  
  # ---- Save ----
  observeEvent(input$save_btn, {
    jsonlite::write_json(build_config(), CONFIG_PATH, auto_unbox = TRUE, pretty = TRUE)
    cfg(build_config())
    status(paste("✅ Saved to", normalizePath(CONFIG_PATH, mustWork = FALSE), "at", format(Sys.time(), "%H:%M:%S")))
  })
  
  # ---- Reload ----
  observeEvent(input$load_btn, {
    loaded <- load_config()
    cfg(loaded)
    status(paste("📂 Reloaded from", CONFIG_PATH, "at", format(Sys.time(), "%H:%M:%S")))
  })
  
  # ---- Reset ----
  observeEvent(input$reset_btn, {
    cfg(default_config)
    status("↺ Reset to defaults (not yet saved).")
  })
  
  output$status_msg <- renderText(status())
  
  # ---- Downloads ----
  output$download_json <- downloadHandler(
    filename = function() "workflow_config.json",
    content = function(file) {
      jsonlite::write_json(build_config(), file, auto_unbox = TRUE, pretty = TRUE)
    }
  )
  
  # ==========================================================================
  # ---- Run Pipeline: sources 01-10 source_code scripts based on config ----
  # ==========================================================================
  # Progress is written directly to the DOM via shinyjs::html() rather than
  # through reactiveVal + renderUI/renderText. Reactive outputs only repaint
  # once Shiny's event loop flushes, which happens after the whole
  # observeEvent finishes — since source() runs synchronously in a loop here,
  # that would mean no visible updates until the entire pipeline is done.
  # Direct DOM writes happen immediately, so the log/step list update live.
  step_status <- new.env()
  
  render_steps_html <- function() {
    ids <- ls(step_status)
    if (length(ids) == 0) return("No pipeline run yet.")
    icon_for <- function(s) switch(s, running = "⏳", done = "✅", skipped = "⏭️", error = "❌", "•")
    lines <- vapply(ids, function(id) sprintf("%s %s", icon_for(get(id, envir = step_status)), id), character(1))
    paste(lines, collapse = "<br>")
  }
  
  log_lines <- character(0)
  append_log <- function(msg) {
    log_lines <<- c(log_lines, msg)
    shinyjs::html(id = "pipeline_log_box", html = paste(log_lines, collapse = "\n"))
    session$sendCustomMessage("scroll-log", list(id = "pipeline_log_box"))
  }
  
  set_step <- function(step_id, status) {
    assign(step_id, status, envir = step_status)
    shinyjs::html(id = "pipeline_steps_box", html = render_steps_html())
  }
  
  observeEvent(input$run_pipeline_btn, {
    rm(list = ls(step_status), envir = step_status)
    log_lines <<- character(0)
    shinyjs::html(id = "pipeline_steps_box", html = "No pipeline run yet.")
    shinyjs::html(id = "pipeline_log_box", html = "")
    shinyjs::html(id = "pipeline_done_banner", html = "")
    shinyjs::hide(id = "pipeline_done_banner")
    
    c0 <- build_config()
    
    # ---- 1. Push config values into the global environment, exactly as the
    # original hardcoded script did, so source()'d scripts can see them. ----
    assign("working_folder", working_folder, envir = .GlobalEnv)
    assign("metadata_template", metadata_template, envir = .GlobalEnv)
    assign("metadata_file", list.files(filled_form_folder, pattern = "\\.xlsx$", full.names = TRUE)[1], envir = .GlobalEnv)
    assign("data_files", list.files(working_folder, pattern = "*.csv"), envir = .GlobalEnv)
    assign("data_names", character(length(list.files(working_folder, pattern = "*.csv"))), envir = .GlobalEnv)
    assign("data_descriptions", character(length(list.files(working_folder, pattern = "*.csv"))), envir = .GlobalEnv)
    
    assign("task", c0$task, envir = .GlobalEnv)
    assign("geo_data", c0$geo_data, envir = .GlobalEnv)
    assign("tax_data", c0$tax_data, envir = .GlobalEnv)
    
    run_attribute_gen       <- c0$task == "workbook"
    run_attribute_check     <- c0$task == "workbook"
    run_specific_attr_check <- c0$task == "fix"
    run_delete_catvars      <- c0$task == "fix"
    run_catvar_gen          <- c0$task %in% c("workbook", "fix")
    run_geospatial_gen      <- c0$task == "workbook" && isTRUE(c0$geo_data)
    run_taxonomy_gen        <- c0$task == "workbook" && isTRUE(c0$tax_data)
    run_txt_to_excel        <- c0$task %in% c("workbook", "fix", "reboot")
    run_excel_to_txt        <- c0$task == "metadata"
    run_eml_creation        <- c0$task == "metadata"
    
    assign("run_attribute_gen", run_attribute_gen, envir = .GlobalEnv)
    assign("run_attribute_check", run_attribute_check, envir = .GlobalEnv)
    assign("run_specific_attr_check", run_specific_attr_check, envir = .GlobalEnv)
    assign("run_delete_catvars", run_delete_catvars, envir = .GlobalEnv)
    assign("run_catvar_gen", run_catvar_gen, envir = .GlobalEnv)
    assign("run_geospatial_gen", run_geospatial_gen, envir = .GlobalEnv)
    assign("run_taxonomy_gen", run_taxonomy_gen, envir = .GlobalEnv)
    assign("run_txt_to_excel", run_txt_to_excel, envir = .GlobalEnv)
    assign("run_excel_to_txt", run_excel_to_txt, envir = .GlobalEnv)
    assign("run_eml_creation", run_eml_creation, envir = .GlobalEnv)
    
    assign("metadata_id", c0$metadata_id, envir = .GlobalEnv)
    assign("package_title", c0$package_title, envir = .GlobalEnv)
    assign("data_type", c0$data_type, envir = .GlobalEnv)
    assign("park_units", c0$park_units, envir = .GlobalEnv)
    assign("producing_units", c0$producing_units, envir = .GlobalEnv)
    assign("cui_code", c0$cui_code, envir = .GlobalEnv)
    assign("int_rights", c0$int_rights, envir = .GlobalEnv)
    assign("language", c0$language, envir = .GlobalEnv)
    assign("ready_to_publish", isTRUE(c0$ready_to_publish), envir = .GlobalEnv)
    assign("upload_data_package", isTRUE(c0$ready_to_publish) && isTRUE(c0$upload_data_package), envir = .GlobalEnv)
    assign("datastore_doi", c0$datastore_doi, envir = .GlobalEnv)
    assign("doi", c0$doi, envir = .GlobalEnv)
    assign("project", c0$project, envir = .GlobalEnv)
    
    assign("data_taxa_tables", c0$data_taxa_tables, envir = .GlobalEnv)
    assign("data_taxa_fields", c0$data_taxa_fields, envir = .GlobalEnv)
    
    assign("data_coordinates_table", c0$data_coordinates_table, envir = .GlobalEnv)
    assign("data_latitude", c0$data_latitude, envir = .GlobalEnv)
    assign("data_longitude", c0$data_longitude, envir = .GlobalEnv)
    assign("data_sitename", c0$data_sitename, envir = .GlobalEnv)
    
    assign("data_dates_table", c0$data_dates_table, envir = .GlobalEnv)
    assign("data_startdate_field", c0$data_startdate_field, envir = .GlobalEnv)
    assign("data_enddate_field", c0$data_enddate_field, envir = .GlobalEnv)
    
    assign("data_urls", rep("temporary URL", length(list.files(working_folder, pattern = "*.csv"))), envir = .GlobalEnv)
    
    append_log(paste("▶ Pipeline started at", format(Sys.time(), "%H:%M:%S")))
    append_log(paste("  task =", c0$task, "| geo_data =", c0$geo_data, "| tax_data =", c0$tax_data))
    
    # ---- 2. Compute dates_df / startdate / enddate, same as original script ----
    tryCatch({
      dates_df <- read.csv(file.path(working_folder, c0$data_dates_table))
      assign("dates_df", dates_df, envir = .GlobalEnv)
      startdate <- as.Date(min(lubridate::parse_date_time(
        dates_df[[c0$data_startdate_field]], orders = c("ymd", "mdy", "dmy")), na.rm = TRUE))
      enddate <- as.Date(max(lubridate::parse_date_time(
        dates_df[[c0$data_enddate_field]], orders = c("ymd", "mdy", "dmy")), na.rm = TRUE))
      assign("startdate", startdate, envir = .GlobalEnv)
      assign("enddate", enddate, envir = .GlobalEnv)
      append_log(sprintf("  startdate = %s | enddate = %s", startdate, enddate))
    }, error = function(e) {
      append_log(paste("❌ Error computing dates_df/startdate/enddate:", conditionMessage(e)))
    })
    
    # ---- 3. Source each numbered script in order, respecting run_* flags ----
    steps <- list(
      list(id = "01_attribute_generation.R",        run = run_attribute_gen),
      list(id = "02_attribute_check.R",              run = run_attribute_check),
      list(id = "03_specific_attribute_check.R",     run = run_specific_attr_check),
      list(id = "04_delete_catvars.R",                run = run_delete_catvars),
      list(id = "05_catvar_generation.R",             run = run_catvar_gen),
      list(id = "06_geospatial_generation.R",         run = run_geospatial_gen),
      list(id = "07_taxonomy_generation.R",           run = run_taxonomy_gen),
      list(id = "08_txt_to_excel.R",                  run = run_txt_to_excel),
      list(id = "09_excel_to_txt.R",                  run = run_excel_to_txt),
      list(id = "10_EML_creation.R",                  run = run_eml_creation)
    )
    
    pipeline_failed <- FALSE
    
    for (s in steps) {
      if (!isTRUE(s$run)) {
        set_step(s$id, "skipped")
        next
      }
      set_step(s$id, "running")
      append_log(paste("⏳ Running", s$id, "..."))
      
      script_path <- file.path("source_code", s$id)
      if (!file.exists(script_path)) {
        set_step(s$id, "error")
        append_log(paste("❌", s$id, "not found at", script_path, "— stopping pipeline."))
        pipeline_failed <- TRUE
        break
      }
      
      result <- tryCatch({
        source(script_path, local = .GlobalEnv)
        TRUE
      }, error = function(e) {
        append_log(paste("❌ Error in", s$id, ":", conditionMessage(e)))
        FALSE
      })
      
      if (isTRUE(result)) {
        set_step(s$id, "done")
        append_log(paste("✅ Finished", s$id))
      } else {
        set_step(s$id, "error")
        append_log("Pipeline stopped due to error above.")
        pipeline_failed <- TRUE
        break
      }
    }
    
    append_log(paste("■ Pipeline run ended at", format(Sys.time(), "%H:%M:%S")))
    
    if (pipeline_failed) {
      shinyjs::html(id = "pipeline_done_banner",
                    html = "❌ <strong>Pipeline stopped due to an error.</strong> Check the log above, fix the issue, and click Run Pipeline again.")
      shinyjs::runjs("document.getElementById('pipeline_done_banner').style.cssText = 'display:block; background:#fdecea; color:#c0392b; padding:14px 18px; border-radius:6px; font-size:16px; margin-top:4px;';")
    } else {
      shinyjs::html(id = "pipeline_done_banner",
                    html = "✅ <strong>Pipeline finished running.</strong> You can close this window now.")
      shinyjs::runjs("document.getElementById('pipeline_done_banner').style.cssText = 'display:block; background:#eafaf1; color:#1e8449; padding:14px 18px; border-radius:6px; font-size:16px; margin-top:4px;';")
    }
  })
  
  # ==========================================================================
  # ---- Run Checks: EML validate / DPchecker, independent of Run Pipeline ----
  # ==========================================================================
  checks_log <- character(0)
  append_checks_log <- function(msg, raw = FALSE) {
    checks_log <<- c(checks_log, msg)
    html_text <- if (raw) style_check_lines(paste(checks_log, collapse = "\n")) else htmltools::htmlEscape(paste(checks_log, collapse = "\n"))
    shinyjs::html(id = "checks_log_box", html = html_text)
  }
  
  # ---- Clean up captured console text for display: strip any ANSI escape
  # codes (color, cursor movement, hyperlinks, etc. from cli) using cli's own
  # ansi_strip(), fix encoding, then HTML-escape. No color styling — just
  # make sure the text itself reads correctly. ----
  style_check_lines <- function(text) {
    Encoding(text) <- "UTF-8"
    text <- tryCatch(cli::ansi_strip(text), error = function(e) text)
    # Fallback cleanup in case any stray escape fragments remain
    text <- gsub("\u001b\\[[0-9;?]*[a-zA-Z]", "", text, perl = TRUE)
    text <- gsub("\u001b\\][^\u0007\u001b]*(\u0007|\u001b\\\\)", "", text, perl = TRUE)
    text <- gsub("[\u0080-\u009F]", "", text, perl = TRUE)
    htmltools::htmlEscape(text)
  }
  
  get_xml_path <- function() {
    c0 <- isolate(build_config())
    paste0(c0$metadata_id, "_metadata.xml")
  }
  
  observeEvent(input$run_eml_validate_btn, {
    xml_path <- get_xml_path()
    append_checks_log(paste("▶ Running EML::eml_validate() at", format(Sys.time(), "%H:%M:%S")))
    if (!file.exists(xml_path)) {
      append_checks_log(paste("❌ File not found:", xml_path, "— run the pipeline (task = metadata) first."))
      return(invisible())
    }
    result <- tryCatch({
      my_metadata <- EML::read_eml(xml_path, from = "xml")
      validation <- EML::eml_validate(my_metadata)
      append_checks_log(paste("Result:", if (isTRUE(validation)) "✅ Valid EML." else "❌ Invalid EML — see details below."))
      if (!isTRUE(validation)) {
        append_checks_log(paste(capture.output(print(attr(validation, "errors"))), collapse = "\n"))
      }
      TRUE
    }, error = function(e) {
      append_checks_log(paste("❌ Error during validation:", conditionMessage(e)))
      FALSE
    })
    append_checks_log(paste("■ EML validate finished at", format(Sys.time(), "%H:%M:%S")))
  })
  
  observeEvent(input$run_dpchecker_btn, {
    xml_path <- get_xml_path()
    append_checks_log(paste("▶ Running DPchecker::run_congruence_checks() at", format(Sys.time(), "%H:%M:%S")))
    if (!file.exists(xml_path)) {
      append_checks_log(paste("❌ File not found:", xml_path, "— run the pipeline (task = metadata) first."))
      return(invisible())
    }
    xml_working <- file.path(working_folder, basename(xml_path))
    append_checks_log("  Running metadata-only congruence check and full congruence check...")
    append_checks_log("  📋 DPchecker's output doesn't display reliably inside this log box — check your R console (RStudio) for the full, correctly formatted results.")
    result <- tryCatch({
      file.copy(xml_path, xml_working, overwrite = TRUE)
      DPchecker::run_congruence_checks(directory = working_folder, check_metadata_only = TRUE)
      DPchecker::run_congruence_checks(directory = working_folder)
      TRUE
    }, error = function(e) {
      append_checks_log(paste("❌ Error during DPchecker run:", conditionMessage(e)))
      FALSE
    }, finally = {
      if (file.exists(xml_working)) file.remove(xml_working)
    })
    if (isTRUE(result)) {
      append_checks_log("  ✅ DPchecker run completed — see R console above for results.")
    }
    append_checks_log(paste("■ DPchecker run finished at", format(Sys.time(), "%H:%M:%S")))
  })
}

shinyApp(ui, server)