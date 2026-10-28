# ============================================================================
# SFCN / EMLassemblyline Metadata Workflow Dashboard
# Shiny GUI for configuring the EMLassemblyline/EMLeditor/NPSdataverse workflow
#
# Save this as app.R and run with: shiny::runApp()
# Requires: shiny, jsonlite, lubridate, readr
# ============================================================================

# ---- Auto-install any missing required packages before loading them -------
required_cran_packages <- c(
  "shiny", "jsonlite", "lubridate", "readr", "shinyjs", "htmltools", "cli",
  "svDialogs", "readxl", "dplyr", "tidyr", "openxlsx", "EMLassemblyline"
)
missing_cran <- required_cran_packages[!sapply(required_cran_packages, requireNamespace, quietly = TRUE)]
if (length(missing_cran) > 0) {
  message("Installing missing CRAN packages: ", paste(missing_cran, collapse = ", "))
  install.packages(missing_cran)
}

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

if (!requireNamespace("EMLeditor", quietly = TRUE) ||
    !requireNamespace("EML", quietly = TRUE) ||
    !requireNamespace("DPchecker", quietly = TRUE)) {
  message("Installing NPSdataverse (includes EMLeditor, EML, DPchecker) from GitHub...")
  pak::pkg_install("nationalparkservice/NPSdataverse")
}

library(shiny)
library(jsonlite)
library(lubridate)
library(readr)
library(shinyjs)
library(htmltools)
library(cli)

library(svDialogs)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(EMLassemblyline)

CONFIG_PATH <- "workflow_config.json"

working_folder     <- "./working_folder"
metadata_template  <- "./MetadataTemplate.xlsx"
filled_form_folder <- "./filled_form"

default_config <- list(
  task                   = "workbook",
  geo_data               = FALSE,
  tax_data               = FALSE,
  
  metadata_id            = "",
  package_title          = "",
  data_type              = "complete",
  park_units             = character(0),
  producing_units        = character(0),
  cui_code               = "PUBLIC",
  int_rights             = "public",
  language               = "English",
  ready_to_publish       = FALSE,
  upload_data_package    = FALSE,
  doi_mode               = "new",
  ds_ref_code            = "",
  doi                    = "",
  project                = "",
  
  data_taxa_tables       = "",
  data_taxa_fields       = "",
  
  data_coordinates_table = "",
  data_latitude          = "",
  data_longitude         = "",
  data_sitename          = "",
  
  data_dates_table       = "",
  data_startdate_field   = "",
  data_enddate_field     = ""
)

ALL_CUI_CODES  <- c("PUBLIC", "FED ONLY", "FEDCON", "DL ONLY", "NOCON")
ALL_INT_RIGHTS <- c("public", "restricted", "CC0")

TASK_LABELS <- c(
  workbook = "1. Build Workbook",
  fix      = "2. Edit Workbook",
  metadata = "3. Create Metadata",
  reboot   = "Rebuild Workbook (Advanced)"
)

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
    Shiny.addCustomMessageHandler('flash-queued', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) {
        var original = el.innerHTML;
        el.classList.add('btn-queued');
        setTimeout(function() {
          el.classList.remove('btn-queued');
        }, msg.duration || 1500);
      }
    });
  ")),
  tags$script(HTML("
    $(document).on('click', '.reorder-btn', function() {
      var id = $(this).attr('id');
      if (!id) return;
      Shiny.setInputValue(id, Math.random(), {priority: 'event'});
    });
  ")),
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
    
    * { box-sizing: border-box; }
    
    body {
      background: linear-gradient(135deg, #f8f6f0 0%, #f6f3ea 50%, #f5f7f1 100%);
      background-attachment: fixed;
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
      font-size: 18px;
      color: #2b2620;
    }
    
    .container-fluid { max-width: 1300px; }
    
    .title-panel-custom {
      font-weight: 800;
      font-size: 2.4rem;
      color: #5c4a2e;
      letter-spacing: -0.02em;
      margin-bottom: 4px;
    }
    
    .well {
      background: #fffdf9;
      border: 1px solid #e3dcc6;
      border-radius: 16px;
      box-shadow: 0 1px 2px rgba(74,58,30,0.04), 0 8px 24px -8px rgba(74,58,30,0.10);
      padding: 24px 26px;
      transition: box-shadow 0.25s ease, transform 0.15s ease;
    }
    .well:hover {
      box-shadow: 0 1px 3px rgba(74,58,30,0.06), 0 14px 32px -10px rgba(74,58,30,0.16);
    }
    
    label, .control-label { font-size: 16.5px; font-weight: 600; color: #4a4335; letter-spacing: 0.01em; }
    
    .form-control, select.form-control {
      font-size: 17px;
      border-radius: 6px;
      border: 1.5px solid #c9b385;
      padding: 10px 13px;
      transition: border-color 0.15s ease, box-shadow 0.15s ease;
      background-color: #fffdf7;
    }
    .form-control:focus, select.form-control:focus {
      border-color: #4a7c9e;
      box-shadow: 0 0 0 4px rgba(74,124,158,0.18);
      background-color: #ffffff;
    }
    
    p, .help-block, .shiny-input-container small { font-size: 16px; color: #5c5540; }
    
    .section-title {
      font-weight: 700;
      font-size: 18.5px;
      color: #2b2620;
      position: relative;
      padding-bottom: 10px;
      padding-left: 2px;
      margin-bottom: 18px;
      letter-spacing: -0.01em;
    }
    .section-title::after {
      content: '';
      position: absolute;
      left: 2px; bottom: 0;
      width: 42px; height: 4px;
      border-radius: 4px;
      background: linear-gradient(90deg, #4d6a4a, #8a6c3f);
    }
    
    .derived-box {
      background: linear-gradient(135deg, #eef2ea, #f3ead9);
      border: 1px solid #ddd3b8;
      border-radius: 12px;
      padding: 14px 16px;
      font-family: 'SF Mono', Consolas, monospace;
      font-size: 15px;
      color: #3d3824;
    }
    
    .code-preview {
      background: #22201a; color: #e8e2d4; padding: 14px; border-radius: 12px;
      font-family: 'SF Mono', Consolas, monospace; font-size: 15px; white-space: pre-wrap;
      max-height: 480px; overflow-y: auto;
    }
    
    .page-nav { margin: 22px 0; display: flex; align-items: center; gap: 16px; flex-wrap: wrap; }
    
    .btn {
      font-size: 16px;
      font-weight: 700;
      border-radius: 10px;
      padding: 11px 19px;
      border: 1.5px solid transparent;
      transition: transform 0.12s ease, box-shadow 0.12s ease, filter 0.12s ease;
      letter-spacing: 0.01em;
      color: #ffffff;
      box-shadow: 0 2px 6px rgba(43,38,32,0.12);
    }
    .btn:hover { transform: translateY(-1px); filter: brightness(1.05); }
    .btn:active { transform: translateY(0); }
    
    .btn.btn-primary,
    .btn.btn-primary:visited,
    .btn.btn-primary span,
    .btn.btn-primary * {
      background: linear-gradient(135deg, #5c7a52, #3f5c38) !important;
      background-color: #4a6b42 !important;
      color: #ffffff !important;
      border-color: #3f5c38 !important;
      box-shadow: 0 4px 14px -4px rgba(63,92,56,0.55);
    }
    .btn.btn-primary:hover, .btn.btn-primary:focus,
    .btn.btn-primary:hover *, .btn.btn-primary:focus * {
      background: linear-gradient(135deg, #6a8a5f, #4a6b42) !important;
      color: #ffffff !important;
      box-shadow: 0 6px 18px -4px rgba(63,92,56,0.65);
    }
    
    #run_pipeline_btn span, #run_pipeline_btn *,
    #publish_btn span, #publish_btn * {
      background: transparent !important;
      box-shadow: none !important;
    }
    
    .btn.btn-default,
    .btn.btn-default:visited {
      background-color: #efe7d4 !important;
      color: #4a4327 !important;
      border-color: #d9cca3 !important;
    }
    .btn.btn-default:hover, .btn.btn-default:focus {
      background-color: #e6dcc3 !important;
      color: #38331f !important;
      border-color: #c9b98a !important;
    }
    
    /* Plain action buttons (no btn-primary/btn-default class, e.g. the Back
       buttons) get their own slightly different neutral tone so they read as
       secondary without competing with the sage-green primary buttons. */
    .btn:not(.btn-primary):not(.btn-default) {
      background-color: #e4e8dd !important;
      color: #3d4a37 !important;
      border-color: #c7d0bb !important;
    }
    .btn:not(.btn-primary):not(.btn-default):hover,
    .btn:not(.btn-primary):not(.btn-default):focus {
      background-color: #d7ddc9 !important;
      color: #2e3a29 !important;
      border-color: #b4c0a4 !important;
    }
    
    #run_pipeline_btn.btn-primary, #publish_btn.btn-primary {
      background: linear-gradient(135deg, #5c7a52, #3f5c38) !important;
      color: #ffffff !important;
      box-shadow: 0 4px 14px -4px rgba(63,92,56,0.55) !important;
    }
    
    hr { border-top: 1.5px solid #ebe4d3; }
    
    pre {
      border-radius: 12px !important;
    }
    
    .checkbox label, .shiny-input-container .checkbox { font-weight: 500; }
    
    .step-indicator {
      display: flex; gap: 8px; align-items: center; margin-bottom: 18px; flex-wrap: wrap;
    }
    .step-pill {
      padding: 6px 14px; border-radius: 999px; font-size: 14.5px; font-weight: 700;
      background: #efe7d4; color: #8a7f5c; border: 1.5px solid #e3dcc9;
    }
    .step-pill.active {
      background: linear-gradient(135deg, #5c7a52, #3f5c38); color: #fff; border-color: #3f5c38;
    }
    .step-pill.done {
      background: #eef2ea; color: #4d6a4a; border-color: #cfe0ca;
    }
    
    .btn:disabled, .btn.disabled {
      opacity: 0.45 !important;
      cursor: not-allowed !important;
      filter: grayscale(0.3);
      transform: none !important;
      box-shadow: none !important;
    }
    .btn:disabled:hover, .btn.disabled:hover {
      transform: none !important;
      filter: grayscale(0.3) !important;
    }
    
    .btn-queued {
      background: linear-gradient(135deg, #6a9c52, #4d7a3a) !important;
      color: #ffffff !important;
      box-shadow: 0 0 0 4px rgba(106,156,82,0.3) !important;
      transition: background 0.2s ease, box-shadow 0.2s ease;
    }
    .btn-queued::after {
      content: ' ✓';
    }
    
    ::-webkit-scrollbar { width: 10px; height: 10px; }
    ::-webkit-scrollbar-track { background: transparent; }
    ::-webkit-scrollbar-thumb { background: #d9cfb0; border-radius: 8px; }
    ::-webkit-scrollbar-thumb:hover { background: #c4b894; }
  "))),
  
  div(style = "padding: 8px 2px 18px 2px;",
      div(class = "title-panel-custom", "Metadata Dashboard")
  ),
  
  # ---------------- PAGE 1: Task picker ----------------
  conditionalPanel(
    condition = "output.current_page == 1",
    p("Step 1 — choose your workflow."),
    fluidRow(
      column(6,
             wellPanel(
               div(class = "section-title", "Workflow Toggle"),
               selectInput("task", "task",
                           choices = c("1. Build Workbook" = "workbook",
                                       "2. Edit Workbook" = "fix",
                                       "3. Create Metadata" = "metadata",
                                       "Rebuild Workbook (Advanced)" = "reboot")),
               
               tags$ul(style = "font-size:16px; color:#444; padding-left:18px;",
                       tags$li(tags$strong("1. Build Workbook"), " – Start here. Builds the Excel metadata form from your CSVs."),
                       tags$li(tags$strong("2. Edit Workbook"), " – Add, rename, reclass, or delete attributes after the workbook exists."),
                       tags$li(tags$strong("3. Create Metadata"), " – Final step. Turns the filled-in Excel form into the EML file, runs checks, and publishes."),
                       tags$li(tags$strong("Rebuild Workbook (Advanced)"), " – Rebuilds the Excel workbook from existing txt files.")
               ),
               
               conditionalPanel(
                 condition = "input.task == 'workbook' || input.task == 'metadata'",
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
    ),
    
    div(style = "text-align:center; padding: 18px 0 6px 0; color:#a39a7f; font-size:14px;",
        "Dashboard made by Katie Herrmann"
    )
  ),
  
  # ---------------- PAGE 2: Fields + Run (build workbook / fix / reboot / create metadata) ----------------
  conditionalPanel(
    condition = "output.current_page == 2",
    
    conditionalPanel(
      condition = "input.task == 'metadata'",
      div(class = "step-indicator",
          tags$span(class = "step-pill active", "① Create Metadata"),
          tags$span(class = "step-pill", "② Run Checks"),
          tags$span(class = "step-pill", "③ Publish")
      )
    ),
    
    p("Step 2 — fields shown below depend on the task you picked (currently: ",
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
    
    # ---- task == "metadata": Data Info + Taxonomy/Geo + Date Info ----
    conditionalPanel(
      condition = "input.task == 'metadata'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Data Info"),
                 selectInput("data_type", "data_type", choices = c("ongoing", "complete")),
                 helpText("Is this data collection finished, or will more data be added later?"),
                 textInput("park_units", "park_units (comma-separated)", value = ""),
                 helpText("Four-letter park codes where this data applies, e.g. BICY, EVER."),
                 textInput("producing_units", "producing_units (comma-separated)", value = ""),
                 helpText("The NPS network or unit(s) responsible for producing this data, e.g. SFCN."),
                 selectInput("cui_code", "cui_code", choices = ALL_CUI_CODES),
                 helpText("Controlled Unclassified Information category. Use PUBLIC unless your data has access restrictions."),
                 selectInput("int_rights", "int_rights", choices = ALL_INT_RIGHTS),
                 helpText("Who is allowed to use this data once published. Use public unless told otherwise."),
                 textInput("language", "language", value = "English")
               )
        )
      ),
      fluidRow(
        column(6,
               div(id = "taxonomy_section_md",
                   wellPanel(
                     div(class = "section-title", "Taxonomy Info"),
                     selectInput("data_taxa_tables", "data_taxa_tables (file)", choices = character(0)),
                     selectInput("data_taxa_fields", "data_taxa_fields (column)", choices = character(0))
                   )
               )
        ),
        column(6,
               div(id = "geospatial_section_md",
                   wellPanel(
                     div(class = "section-title", "Geospatial Info"),
                     selectInput("data_coordinates_table", "data_coordinates_table (file)", choices = character(0)),
                     selectInput("data_latitude", "data_latitude (column)", choices = character(0)),
                     selectInput("data_longitude", "data_longitude (column)", choices = character(0)),
                     selectInput("data_sitename", "data_sitename (column)", choices = character(0))
                   )
               )
        )
      ),
      fluidRow(
        column(12,
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
    
    # ---- task == "workbook": Taxonomy / Geospatial Info ----
    conditionalPanel(
      condition = "input.task == 'workbook'",
      fluidRow(
        column(6,
               div(id = "taxonomy_section",
                   wellPanel(
                     div(class = "section-title", "Taxonomy Info"),
                     selectInput("data_taxa_tables", "data_taxa_tables (file)", choices = character(0)),
                     selectInput("data_taxa_fields", "data_taxa_fields (column)", choices = character(0))
                   )
               )
        ),
        column(6,
               div(id = "geospatial_section",
                   wellPanel(
                     div(class = "section-title", "Geospatial Info"),
                     selectInput("data_coordinates_table", "data_coordinates_table (file)", choices = character(0)),
                     selectInput("data_latitude", "data_latitude (column)", choices = character(0)),
                     selectInput("data_longitude", "data_longitude (column)", choices = character(0)),
                     selectInput("data_sitename", "data_sitename (column)", choices = character(0))
                   )
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
                 p(style = "color:#555; font-size:17px;",
                   "Syncs from the filled Excel workbook (excel → txt), lets you queue up ",
                   "add/rename/reclass/delete edits below, then rebuilds the workbook (txt → excel) ",
                   "when you click Apply Fixes."),
                 actionButton("sync_from_excel_btn", "🔄 Sync from Excel", width = "320px"),
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
                 p(style = "color:#777; font-size:16px;", "Changing to 'categorical' auto-generates catvars codes from the CSV's unique values (definitions left blank to fill in). Changing away from 'categorical' removes its catvars rows."),
                 actionButton("queue_reclass", "Queue: Reclass Attribute", width = "100%")
               )
        ),
        column(6,
               wellPanel(
                 div(class = "section-title", "🗑 Delete Attribute"),
                 selectInput("delete_table", "Table", choices = character(0)),
                 selectInput("delete_name", "Existing attribute", choices = character(0)),
                 p(style = "color:#777; font-size:16px;", "Also removes any catvars rows for this attribute, if it was categorical."),
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
                 actionButton("apply_fixes_btn", "✅ Apply Fixes", class = "btn-primary", width = "420px"),
                 br(), br(),
                 tags$pre(id = "fix_log_box",
                          style = "background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; font-size:14px; max-height:260px; overflow-y:auto; white-space:pre-wrap;",
                          "")
               )
        )
      ),
      
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "🔀 Reorder Attributes"),
                 p(style = "color:#777; font-size:16px;",
                   "Matches an attribute table's row order to its CSV's column order. ",
                   "Applies immediately to the attributes file on disk."),
                 selectInput("reorder_table", "Table", choices = character(0)),
                 fluidRow(
                   column(6, actionButton("reorder_match_csv", "📐 Match CSV column order", width = "100%", class = "btn-primary")),
                   column(6, actionButton("reorder_apply_fixes_btn", "✅ Apply Fixes (if anything's queued)", width = "100%"))
                 ),
                 uiOutput("reorder_apply_fixes_hint"),
                 br(),
                 uiOutput("reorder_list_box"),
                 br(),
                 tags$pre(id = "reorder_fix_log_box",
                          style = "background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; font-size:14px; max-height:220px; overflow-y:auto; white-space:pre-wrap; display:none;",
                          "")
               )
        )
      )
    ),
    
    # ---- task == "reboot" ----
    conditionalPanel(
      condition = "input.task == 'reboot'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Rebuild Workbook"),
                 p(style = "color:#555; font-size:17px;",
                   "Rebuilds the Excel workbook directly from the existing txt files in working_folder. ",
                   "No other setup needed — just click Run below.")
               )
        )
      )
    ),
    
    div(class = "page-nav",
        actionButton("to_page1", "← Back", width = "160px"),
        conditionalPanel(
          condition = "input.task == 'workbook' || input.task == 'metadata'",
          actionButton("rescan_btn", "🔄 Re-scan folder", width = "200px")
        )
    ),
    
    # ---- Run section: shown for workbook / metadata / reboot (not fix, which has its own Apply Fixes) ----
    conditionalPanel(
      condition = "input.task != 'fix'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Run"),
                 conditionalPanel(
                   condition = "input.task == 'workbook'",
                   p(style = "color:#555; font-size:17px;",
                     "Builds your Excel workbook based on your current settings above. ",
                     "Some steps may open interactive dialogs/tables outside the browser (e.g. in RStudio) — ",
                     "watch the R console and resolve those before continuing.")
                 ),
                 conditionalPanel(
                   condition = "input.task == 'metadata'",
                   p(style = "color:#555; font-size:17px;",
                     "Create the metadata package (EML file) from your filled-in Excel workbook. ",
                     "Once this finishes, continue to Run Checks on the next page.")
                 ),
                 conditionalPanel(
                   condition = "input.task == 'reboot'",
                   p(style = "color:#555; font-size:17px;",
                     "Rebuilds the Excel workbook from existing txt files.")
                 ),
                 actionButton("run_pipeline_btn", "▶ Run", class = "btn-primary", width = "260px"),
                 br(), br(),
                 tags$div(id = "pipeline_steps_box", class = "derived-box", "No run yet."),
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
    
    # ---- Next-step nav for metadata only ----
    conditionalPanel(
      condition = "input.task == 'metadata'",
      div(class = "page-nav",
          actionButton("to_page3", "Next: Run Checks →", class = "btn-primary", width = "260px")
      )
    ),
    
    # ---- Save/Load/Export: shown here for non-metadata tasks only (metadata's copy is on page 4) ----
    conditionalPanel(
      condition = "input.task != 'metadata'",
      fluidRow(
        column(12,
               wellPanel(
                 div(class = "section-title", "Save / Load / Export"),
                 p(style = "color:#555; font-size:17px; font-style:italic;",
                   "Tired of filling in fields over and over again? Save your settings here!"),
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
    ),
    
    div(style = "text-align:center; padding: 18px 0 6px 0; color:#a39a7f; font-size:14px;",
        "Dashboard made by Katie Herrmann"
    )
  ),
  
  # ---------------- PAGE 3 (metadata only): Run Checks ----------------
  conditionalPanel(
    condition = "output.current_page == 3",
    
    div(class = "step-indicator",
        tags$span(class = "step-pill done", "① Create Metadata"),
        tags$span(class = "step-pill active", "② Run Checks"),
        tags$span(class = "step-pill", "③ Publish")
    ),
    
    p("Step 3 — run checks on your newly created metadata before moving on to publishing."),
    
    fluidRow(
      column(12,
             wellPanel(
               div(class = "section-title", "Run Checks"),
               p(style = "color:#555; font-size:17px;",
                 "Requires that the metadata package was already created on the previous page ",
                 "(metadata_id_metadata.xml must exist)."),
               p(style = "color:#777; font-size:16px;",
                 "If something looks off here, check the Excel workbook's attributes tab first — ",
                 "most issues come from incomplete attribute or missing-value info."),
               fluidRow(
                 column(6, actionButton("run_eml_validate_btn", "🔎 Run EML Validate", width = "100%")),
                 column(6, actionButton("run_dpchecker_btn", "🔎 Run DPchecker Congruence Checks", width = "100%"))
               ),
               br(),
               tags$pre(id = "checks_log_box",
                        style = "background:#fdfdfd; color:#1a1a1a; border:1px solid #d5d8dc; padding:12px; border-radius:6px; font-size:15px; font-family: Consolas, 'Courier New', monospace; line-height:1.45; max-height:420px; overflow-y:auto; white-space:pre-wrap; word-break:break-word;",
                        "")
             )
      )
    ),
    
    div(class = "page-nav",
        actionButton("to_page2_from3", "← Back", width = "160px"),
        uiOutput("to_page4_btn_ui")
    ),
    
    div(style = "text-align:center; padding: 18px 0 6px 0; color:#a39a7f; font-size:14px;",
        "Dashboard made by Katie Herrmann"
    )
  ),
  
  # ---------------- PAGE 4 (metadata only): Publishing ----------------
  conditionalPanel(
    condition = "output.current_page == 4",
    
    div(class = "step-indicator",
        tags$span(class = "step-pill done", "① Create Metadata"),
        tags$span(class = "step-pill done", "② Run Checks"),
        tags$span(class = "step-pill active", "③ Publish")
    ),
    
    p("Step 4 — final review before publishing."),
    
    fluidRow(
      column(12,
             wellPanel(
               style = "border: 2px solid #e8a33d;",
               div(class = "section-title", style = "color:#b5651d; border-bottom-color:#e8a33d;", "⚠️ Publishing"),
               div(style = "background:#fff6e5; border:1px solid #f0c674; border-radius:6px; padding:14px 16px; margin-bottom:14px;",
                   tags$p(style = "color:#8a5a00; font-size:17.5px; font-weight:600; margin-bottom:6px;",
                          "⚠️ This is the final, publishing step."),
                   tags$p(style = "color:#7a5200; font-size:16.5px; margin-bottom:0;",
                          "Checking \"Ready to Publish\" sets a permanent DataStore DOI/project on this metadata, and — ",
                          "if you also check \"upload the data package\" — uploads your data files to DataStore. ",
                          "Once published, this is not something you can casually undo. Only do this once the metadata ",
                          "is fully finalized, checked, and reviewed/approved.")
               ),
               checkboxInput("ready_to_publish", "Ready to Publish", value = FALSE),
               conditionalPanel(
                 condition = "input.ready_to_publish == true",
                 div(style = "background:#fdecea; border:1px solid #f1a9a0; border-radius:6px; padding:12px 16px; margin:10px 0 14px 0;",
                     checkboxInput("confirm_publish", 
                                   tags$span(style="color:#a12a1a; font-weight:600;",
                                             "I understand this will finalize and publish this data package, and that this cannot be easily undone."),
                                   value = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.confirm_publish == true",
                   radioButtons("doi_mode", "DOI",
                                choices = c("Create a new DOI (auto-generates a DataStore draft reference)" = "new",
                                            "I already have a DataStore reference code" = "existing"),
                                selected = "new"),
                   conditionalPanel(
                     condition = "input.doi_mode == 'existing'",
                     textInput("ds_ref_code", "DataStore reference code (7 digits)", value = ""),
                     helpText("Enter ONLY the 7-digit reference code from the draft reference you already created on DataStore \u2014 not the full URL or DOI.")
                   ),
                   textInput("project", "project (DataStore Reference ID)", value = ""),
                   tags$hr(),
                   div(style = "background:#fdf6e3; border:2px solid #e8a33d; border-radius:8px; padding:14px 16px; margin: 6px 0 4px 0;",
                       checkboxInput("upload_data_package", 
                                     tags$span(style = "font-weight:800; font-size:17px; color:#5c4a2e;", 
                                               "📦 Also upload the data package to DataStore"), 
                                     value = FALSE),
                       helpText(style = "margin-bottom:0;",
                                "Unchecked: the DOI/project are set, but nothing is uploaded. ",
                                "Checked: the data package files are uploaded as well."),
                       tags$p(style = "color:#a15c00; font-size:14.5px; font-weight:600; margin: 10px 0 0 0;",
                              "⚠️ Known DataStore API limitation: you can only upload files this way ",
                              "ONCE per reference. If you need to replace or fix files after this, you ",
                              "must do it manually through the DataStore web interface — re-running ",
                              "Publish will not let you re-upload.")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.confirm_publish == false",
                   tags$p(style = "color:#a12a1a; font-size:16px; font-style:italic;",
                          "Check the confirmation box above to reveal publishing fields.")
                 )
               )
             )
      )
    ),
    
    fluidRow(
      column(12,
             wellPanel(
               div(class = "section-title", "Publish"),
               p(style = "color:#555; font-size:17px;",
                 "Applies your DataStore DOI/project (and uploads the data package, if checked) to the ",
                 "metadata package already created. This does not regenerate the EML file."),
               actionButton("publish_btn", "▶ Run", class = "btn-primary", width = "260px"),
               uiOutput("publish_run_warning"),
               br(), br(),
               tags$pre(id = "publish_log_box",
                        style = "background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; font-size:14px; max-height:300px; overflow-y:auto; white-space:pre-wrap;",
                        ""),
               br(),
               uiOutput("resulting_doi_box"),
               tags$div(id = "publish_done_banner", style = "display:none;")
             )
      )
    ),
    
    fluidRow(
      column(12,
             wellPanel(
               div(class = "section-title", "Save / Load / Export"),
               p(style = "color:#555; font-size:17px; font-style:italic;",
                 "Tired of filling in fields over and over again? Save your settings here!"),
               fluidRow(
                 column(3, actionButton("save_btn_p4", "💾 Save answers to JSON", width = "100%", class = "btn-primary")),
                 column(3, actionButton("load_btn_p4", "📂 Reload saved JSON", width = "100%")),
                 column(3, actionButton("reset_btn_p4", "↺ Reset to defaults", width = "100%")),
                 column(3, downloadButton("download_json_p4", "⬇ Download JSON", style = "width:100%"))
               ),
               br(),
               verbatimTextOutput("status_msg_p4")
             )
      )
    ),
    
    div(class = "page-nav",
        actionButton("to_page3_from4", "← Back", width = "160px")
    ),
    
    div(style = "text-align:center; padding: 18px 0 6px 0; color:#a39a7f; font-size:14px;",
        "Dashboard made by Katie Herrmann"
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
  checks_run_at_least_once <- reactiveVal(FALSE)
  
  output$current_page <- reactive({ page() })
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
  
  output$current_task_label <- renderText(TASK_LABELS[[input$task]])
  
  # Publish button starts disabled every session, until both publish
  # checkboxes are checked (see the observe() below).
  shinyjs::disable("publish_btn")
  
  # ---- Flip the Publish button to a red/rust "armed" state once the user
  # has checked Ready to Publish AND the explicit confirmation checkbox. ----
  observe({
    armed <- isTRUE(input$ready_to_publish) && isTRUE(input$confirm_publish)
    if (armed) {
      shinyjs::enable("publish_btn")
      shinyjs::runjs("
        setTimeout(function() {
          var btn = document.getElementById('publish_btn');
          if (btn) {
            btn.style.setProperty('background', 'linear-gradient(135deg, #b5533d, #8c3f2d)', 'important');
            btn.style.setProperty('box-shadow', '0 4px 14px -4px rgba(140,63,45,0.6)', 'important');
            btn.style.setProperty('color', '#ffffff', 'important');
            btn.style.setProperty('border-color', '#8c3f2d', 'important');
            var children = btn.querySelectorAll('*');
            children.forEach(function(el) {
              el.style.setProperty('background', 'transparent', 'important');
              el.style.setProperty('box-shadow', 'none', 'important');
              el.style.setProperty('color', '#ffffff', 'important');
            });
          }
        }, 50);
      ")
    } else {
      shinyjs::disable("publish_btn")
      shinyjs::runjs("
        setTimeout(function() {
          var btn = document.getElementById('publish_btn');
          if (btn) {
            btn.style.removeProperty('background');
            btn.style.removeProperty('box-shadow');
            btn.style.removeProperty('color');
            btn.style.removeProperty('border-color');
          }
        }, 50);
      ")
    }
  })
  
  output$publish_run_warning <- renderUI({
    armed <- isTRUE(input$ready_to_publish) && isTRUE(input$confirm_publish)
    if (armed) {
      tags$p(style = "color:#c0392b; font-size:16px; font-weight:600; margin-top:8px;",
             "🚀 Clicking Run will finalize and publish this data package.")
    }
  })
  
  output$resulting_doi_box <- renderUI({
    saved_cfg <- cfg()
    if (!is.null(saved_cfg$doi) && nzchar(saved_cfg$doi)) {
      tags$div(class = "derived-box", style = "margin-bottom:10px;",
               sprintf("Current DOI on record: %s", saved_cfg$doi))
    } else {
      NULL
    }
  })
  
  # ---- Push a config list into all the UI inputs ----
  apply_config_to_inputs <- function(c0, keep_task = TRUE) {
    if (!keep_task) {
      updateSelectInput(session, "task", selected = c0$task)
    }
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
    updateRadioButtons(session, "doi_mode", selected = if (!is.null(c0$doi_mode)) c0$doi_mode else "new")
    updateTextInput(session, "ds_ref_code", value = c0$ds_ref_code)
    updateTextInput(session, "project", value = c0$project)
  }
  
  observe({
    apply_config_to_inputs(isolate(cfg()), keep_task = FALSE)
  })
  
  # ---- Page navigation ----
  observeEvent(input$to_page2, {
    page(2)
    if (input$task %in% c("workbook", "metadata")) {
      do_scan(preserve_selection = TRUE)
    }
  })
  observeEvent(input$to_page1, { page(1) })
  observeEvent(input$rescan_btn, { do_scan(preserve_selection = TRUE) })
  
  observeEvent(input$to_page3, { page(3) })
  observeEvent(input$to_page2_from3, { page(2) })
  observeEvent(input$to_page4, { page(4) })
  observeEvent(input$to_page3_from4, { page(3) })
  
  output$to_page4_btn_ui <- renderUI({
    if (isTRUE(checks_run_at_least_once())) {
      actionButton("to_page4", "Next: Publishing →", class = "btn-primary", width = "260px")
    } else {
      tags$div(
        style = "margin-top: 6px;",
        tags$p(style = "color:#8a5a00; font-size:16px; font-style:italic;",
               "Run at least one check above (EML Validate or DPchecker) to continue.")
      )
    }
  })
  
  # ---- Hide Taxonomy & Geospatial sections when their corresponding toggle
  # (tax_data / geo_data) is unchecked. ----
  observe({
    if (isTRUE(input$tax_data)) {
      shinyjs::show(id = "taxonomy_section")
      shinyjs::show(id = "taxonomy_section_md")
    } else {
      shinyjs::hide(id = "taxonomy_section")
      shinyjs::hide(id = "taxonomy_section_md")
    }
  })
  
  observe({
    if (isTRUE(input$geo_data)) {
      shinyjs::show(id = "geospatial_section")
      shinyjs::show(id = "geospatial_section_md")
    } else {
      shinyjs::hide(id = "geospatial_section")
      shinyjs::hide(id = "geospatial_section_md")
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
      style = sprintf("padding:12px 16px; border-radius:12px; margin-bottom:16px; font-size:16.5px; font-weight:600; %s",
                      if (is_warning) "background:#fef2f2; color:#b91c1c; border:1px solid #fecaca;" else "background:#f0fdf4; color:#15803d; border:1px solid #bbf7d0;"),
      msg
    )
  })
  
  output$title_word_warning <- renderUI({
    n <- length(strsplit(trimws(input$package_title), "\\s+")[[1]])
    if (input$package_title == "") return(NULL)
    if (n < 5) {
      tags$p(style = "color:#b91c1c; font-size:15.5px; font-weight:600;", sprintf("⚠️ Only %d word(s) — needs 5 or more.", n))
    } else {
      tags$p(style = "color:#15803d; font-size:15.5px; font-weight:600;", sprintf("✅ %d words.", n))
    }
  })
  
  observeEvent(input$calc_dates, {
    wf <- working_folder
    f <- file.path(wf, input$data_dates_table)
    if (!file.exists(f)) {
      output$date_range_output <- renderUI(tags$p(style = "color:#c0392b; font-size:16px;", paste("⚠️ File not found:", f)))
      return(invisible())
    }
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !(input$data_startdate_field %in% names(df)) || !(input$data_enddate_field %in% names(df))) {
      output$date_range_output <- renderUI(tags$p(style = "color:#c0392b; font-size:16px;", "⚠️ Column(s) not found in file."))
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
  
  fix_queue  <- reactiveVal(list())
  reorder_warning <- reactiveVal(NULL)        # character vector of unmatched attribute names, or NULL
  reorder_missing_attrs <- reactiveVal(NULL)  # character vector of CSV columns with no attribute row, or NULL
  
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
  
  refresh_fix_tables <- function() {
    files <- list.files(working_folder, pattern = "^attributes_.*\\.txt$", full.names = FALSE)
    tbls <- sub("^attributes_", "", sub("\\.txt$", "", files))
    updateSelectInput(session, "add_table", choices = tbls)
    updateSelectInput(session, "rename_table", choices = tbls)
    updateSelectInput(session, "reclass_table", choices = tbls)
    updateSelectInput(session, "delete_table", choices = tbls)
    updateSelectInput(session, "reorder_table", choices = tbls)
  }
  
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
  
  # ==========================================================================
  # ---- Reorder Attributes: applies immediately, no queue needed ----
  # ==========================================================================
  csv_path_for_table <- function(tbl) {
    candidate <- file.path(working_folder, paste0(tbl, ".csv"))
    if (file.exists(candidate)) candidate else NA
  }
  
  get_csv_header_order <- function(tbl) {
    p <- csv_path_for_table(tbl)
    if (is.na(p)) return(character(0))
    tryCatch(
      trimws(names(suppressMessages(suppressWarnings(
        readr::read_csv(p, n_max = 0, show_col_types = FALSE, progress = FALSE)
      )))),
      error = function(e) character(0)
    )
  }
  
  render_reorder_list <- function(tbl) {
    df <- read_attr_file(tbl)
    if (is.null(df) || nrow(df) == 0) {
      return(tags$p(style = "color:#888; font-size:16px;", "No attributes found for this table."))
    }
    n <- nrow(df)
    rows <- lapply(seq_len(n), function(i) {
      is_first <- i == 1
      is_last  <- i == n
      tags$div(
        class = "reorder-row",
        style = sprintf(
          "display:flex; align-items:center; gap:12px; padding:9px 12px; border-bottom:1px solid #eee2cc; %s",
          if (i %% 2 == 0) "background:#fbf8f0;" else "background:transparent;"
        ),
        tags$span(
          style = "font-family:'SF Mono',Consolas,monospace; font-size:13px; font-weight:700; color:#a39a7f; width:22px; text-align:right; flex-shrink:0;",
          i
        ),
        tags$span(
          style = "flex:1; font-size:16px; color:#2b2620; font-weight:500; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
          df$attributeName[i]
        ),
        tags$div(
          style = "display:flex; gap:2px; flex-shrink:0;",
          tags$button(
            id = paste0("reorder_up_", i), class = "reorder-btn", type = "button",
            disabled = if (is_first) "disabled" else NULL,
            title = "Move up",
            HTML("&#9650;")
          ),
          tags$button(
            id = paste0("reorder_down_", i), class = "reorder-btn", type = "button",
            disabled = if (is_last) "disabled" else NULL,
            title = "Move down",
            HTML("&#9660;")
          )
        )
      )
    })
    
    tagList(
      tags$style(HTML("
        .reorder-btn {
          width: 26px; height: 26px;
          border-radius: 6px;
          border: 1px solid #e3dcc9;
          background: #fffefb;
          color: #6a6350;
          font-size: 10px;
          line-height: 1;
          cursor: pointer;
          display: flex; align-items: center; justify-content: center;
          transition: background 0.12s ease, border-color 0.12s ease, color 0.12s ease;
          padding: 0;
        }
        .reorder-btn:hover:not(:disabled) {
          background: #5c7a52;
          border-color: #3f5c38;
          color: #ffffff;
        }
        .reorder-btn:disabled {
          opacity: 0.3;
          cursor: not-allowed;
        }
        .reorder-row:hover {
          background: #f3ead9 !important;
        }
      ")),
      tags$div(
        style = "border:1px solid #eee2cc; border-radius:10px; overflow:hidden;",
        do.call(tagList, rows)
      )
    )
  }
  
  output$reorder_list_box <- renderUI({
    w <- reorder_warning()
    m <- reorder_missing_attrs()
    
    missing_ui <- if (!is.null(m) && length(m) > 0) {
      tags$div(
        style = "background:#fdecea; border:1px solid #f1a9a0; border-radius:8px; padding:10px 14px; margin-bottom:10px; font-size:15.5px; color:#a12a1a;",
        tags$strong("🚫 CSV columns with no attribute defined at all: "),
        paste(m, collapse = ", "),
        tags$br(),
        tags$span(style = "font-size:14.5px; font-weight:400;",
                  "These won't appear in this list until you add an attribute row for them (see Add Attribute).")
      )
    } else {
      NULL
    }
    
    warning_ui <- if (!is.null(w) && length(w) > 0) {
      tags$div(
        style = "background:#fff6e5; border:1px solid #f0c674; border-radius:8px; padding:10px 14px; margin-bottom:12px; font-size:15.5px; color:#8a5a00;",
        tags$strong("⚠️ Not found in CSV header (left at the end): "),
        paste(w, collapse = ", ")
      )
    } else {
      NULL
    }
    tagList(missing_ui, warning_ui, render_reorder_list(input$reorder_table))
  })
  
  observeEvent(input$reorder_table, {
    reorder_warning(NULL)
    reorder_missing_attrs(NULL)
  })
  
  move_attr_row <- function(tbl, from_idx, to_idx) {
    df <- read_attr_file(tbl)
    if (is.null(df) || from_idx < 1 || to_idx < 1 || from_idx > nrow(df) || to_idx > nrow(df)) return(invisible())
    ord <- seq_len(nrow(df))
    ord <- ord[ord != from_idx]
    ord <- append(ord, from_idx, after = to_idx - 1)
    df <- df[ord, , drop = FALSE]
    write_attr_file(tbl, df)
  }
  
  observe({
    tbl <- input$reorder_table
    req(tbl)
    df <- read_attr_file(tbl)
    n <- if (is.null(df)) 0 else nrow(df)
    if (n == 0) return(invisible())
    lapply(seq_len(n), function(i) {
      local({
        idx <- i
        observeEvent(input[[paste0("reorder_up_", idx)]], {
          move_attr_row(input$reorder_table, idx, idx - 1)
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("reorder_down_", idx)]], {
          move_attr_row(input$reorder_table, idx, idx + 1)
        }, ignoreInit = TRUE)
      })
    })
  })
  
  observeEvent(input$reorder_match_csv, {
    tbl <- input$reorder_table
    req(tbl)
    df <- read_attr_file(tbl)
    if (is.null(df) || nrow(df) == 0) {
      reorder_warning(NULL)
      reorder_missing_attrs(NULL)
      return(invisible())
    }
    csv_order <- get_csv_header_order(tbl)
    if (length(csv_order) == 0) {
      reorder_warning("⚠️ Could not read CSV header for this table — no changes made.")
      reorder_missing_attrs(NULL)
      return(invisible())
    }
    
    # CSV columns that have no corresponding attribute row at all — these are
    # invisible to the reorder itself (nothing to move), so they're tracked
    # and surfaced separately rather than silently dropped.
    missing_attrs <- setdiff(csv_order, df$attributeName)
    reorder_missing_attrs(if (length(missing_attrs) > 0) missing_attrs else NULL)
    
    matched_idx   <- match(csv_order, df$attributeName)
    matched_idx   <- matched_idx[!is.na(matched_idx)]
    unmatched_idx <- setdiff(seq_len(nrow(df)), matched_idx)
    
    new_order <- c(matched_idx, unmatched_idx)
    df <- df[new_order, , drop = FALSE]
    write_attr_file(tbl, df)
    
    unmatched_names <- if (length(unmatched_idx) > 0) df$attributeName[(length(matched_idx) + 1):nrow(df)] else character(0)
    reorder_warning(if (length(unmatched_names) > 0) unmatched_names else NULL)
  })
  
  output$fix_sync_status <- renderUI(NULL)
  observeEvent(input$sync_from_excel_btn, {
    result <- tryCatch({
      source(file.path("source_code", "09_excel_to_txt.R"), local = .GlobalEnv)
      TRUE
    }, error = function(e) {
      output$fix_sync_status <<- renderUI(
        tags$p(style = "color:#c0392b; font-size:17px;", paste("❌ Sync failed:", conditionMessage(e)))
      )
      FALSE
    })
    if (isTRUE(result)) {
      refresh_fix_tables()
      output$fix_sync_status <- renderUI(
        tags$p(style = "color:#1e8449; font-size:17px;", "✅ Synced from Excel. Tables refreshed below.")
      )
    }
  })
  
  observe({
    if (input$task == "fix" && page() == 2) {
      isolate(refresh_fix_tables())
    }
  })
  
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
    session$sendCustomMessage("flash-queued", list(id = "queue_add", duration = 1500))
  })
  
  observeEvent(input$queue_rename, {
    req(input$rename_table, input$rename_old_name, input$rename_new_name)
    add_to_queue(list(
      type = "rename", table = input$rename_table,
      old_name = input$rename_old_name, new_name = input$rename_new_name
    ))
    session$sendCustomMessage("flash-queued", list(id = "queue_rename", duration = 1500))
  })
  
  observeEvent(input$queue_reclass, {
    req(input$reclass_table, input$reclass_name, input$reclass_new_class)
    add_to_queue(list(
      type = "reclass", table = input$reclass_table,
      name = input$reclass_name, new_class = input$reclass_new_class
    ))
    session$sendCustomMessage("flash-queued", list(id = "queue_reclass", duration = 1500))
  })
  
  observeEvent(input$queue_delete, {
    req(input$delete_table, input$delete_name)
    add_to_queue(list(type = "delete", table = input$delete_table, name = input$delete_name))
    session$sendCustomMessage("flash-queued", list(id = "queue_delete", duration = 1500))
  })
  
  output$fix_queue_box <- renderUI({
    q <- fix_queue()
    if (length(q) == 0) return(tags$p(style = "color:#888; font-size:17px;", "No pending edits queued yet."))
    rows <- lapply(seq_along(q), function(i) {
      op <- q[[i]]
      label <- switch(op$type,
                      add     = sprintf("➕ Add '%s' (%s) to %s", op$name, op$class, op$table),
                      rename  = sprintf("✏️ Rename '%s' → '%s' in %s", op$old_name, op$new_name, op$table),
                      reclass = sprintf("🔁 Reclass '%s' → %s in %s", op$name, op$new_class, op$table),
                      delete  = sprintf("🗑 Delete '%s' from %s", op$name, op$table)
      )
      fluidRow(
        column(10, tags$p(style = "margin-bottom:4px; font-size:17px;", label)),
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
  
  # Shared apply-fixes logic, callable from either button. `log_box_id` lets
  # each button write its progress to its own <pre> element so the person
  # sees feedback wherever they clicked from.
  run_apply_fixes <- function(log_box_id) {
    q <- fix_queue()
    log_lines_fix <- character(0)
    appendf <- function(msg) {
      log_lines_fix <<- c(log_lines_fix, msg)
      shinyjs::html(id = log_box_id, html = paste(log_lines_fix, collapse = "\n"))
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
  }
  
  observeEvent(input$apply_fixes_btn, {
    run_apply_fixes("fix_log_box")
  })
  
  observeEvent(input$reorder_apply_fixes_btn, {
    shinyjs::show(id = "reorder_fix_log_box")
    run_apply_fixes("reorder_fix_log_box")
  })
  
  output$reorder_apply_fixes_hint <- renderUI({
    n <- length(fix_queue())
    if (n == 0) {
      tags$p(style = "color:#888; font-size:14.5px; margin-top:6px;", "No pending edits queued.")
    } else {
      tags$p(style = "color:#8a5a00; font-size:14.5px; margin-top:6px;",
             sprintf("%d edit(s) queued — must click to apply before reordering picks them up.", n))
    }
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
      confirm_publish        = input$confirm_publish,
      upload_data_package    = input$upload_data_package,
      doi_mode               = input$doi_mode,
      ds_ref_code            = input$ds_ref_code,
      doi                    = isolate(cfg())$doi,  # populated after publish; not a direct input
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
  
  # ---- Save / Load / Reset / Download (page 2, non-metadata tasks) ----
  observeEvent(input$save_btn, {
    jsonlite::write_json(build_config(), CONFIG_PATH, auto_unbox = TRUE, pretty = TRUE)
    cfg(build_config())
    status(paste("✅ Saved to", normalizePath(CONFIG_PATH, mustWork = FALSE), "at", format(Sys.time(), "%H:%M:%S")))
  })
  
  observeEvent(input$load_btn, {
    loaded <- load_config()
    cfg(loaded)
    apply_config_to_inputs(loaded, keep_task = FALSE)
    status(paste("📂 Reloaded from", CONFIG_PATH, "at", format(Sys.time(), "%H:%M:%S")))
  })
  
  observeEvent(input$reset_btn, {
    cfg(default_config)
    apply_config_to_inputs(default_config, keep_task = TRUE)
    if (page() == 2 && input$task %in% c("workbook", "metadata")) {
      do_scan(preserve_selection = TRUE)
    }
    status("↺ Reset to defaults (not yet saved). Your current step was left as-is.")
  })
  
  output$status_msg <- renderText(status())
  
  output$download_json <- downloadHandler(
    filename = function() "workflow_config.json",
    content = function(file) {
      jsonlite::write_json(build_config(), file, auto_unbox = TRUE, pretty = TRUE)
    }
  )
  
  # ---- Save / Load / Reset / Download (page 4, metadata's Publishing page) ----
  observeEvent(input$save_btn_p4, {
    jsonlite::write_json(build_config(), CONFIG_PATH, auto_unbox = TRUE, pretty = TRUE)
    cfg(build_config())
    status(paste("✅ Saved to", normalizePath(CONFIG_PATH, mustWork = FALSE), "at", format(Sys.time(), "%H:%M:%S")))
  })
  
  observeEvent(input$load_btn_p4, {
    loaded <- load_config()
    cfg(loaded)
    apply_config_to_inputs(loaded, keep_task = FALSE)
    status(paste("📂 Reloaded from", CONFIG_PATH, "at", format(Sys.time(), "%H:%M:%S")))
  })
  
  observeEvent(input$reset_btn_p4, {
    cfg(default_config)
    apply_config_to_inputs(default_config, keep_task = TRUE)
    status("↺ Reset to defaults (not yet saved). Your current step was left as-is.")
  })
  
  output$status_msg_p4 <- renderText(status())
  
  output$download_json_p4 <- downloadHandler(
    filename = function() "workflow_config.json",
    content = function(file) {
      jsonlite::write_json(build_config(), file, auto_unbox = TRUE, pretty = TRUE)
    }
  )
  
  # ==========================================================================
  # ---- Run: sources 01-10 source_code scripts based on config (page 2) ----
  # ==========================================================================
  step_status <- new.env()
  
  render_steps_html <- function() {
    ids <- ls(step_status)
    if (length(ids) == 0) return("No run yet.")
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
    shinyjs::html(id = "pipeline_steps_box", html = "No run yet.")
    shinyjs::html(id = "pipeline_log_box", html = "")
    shinyjs::html(id = "pipeline_done_banner", html = "")
    shinyjs::hide(id = "pipeline_done_banner")
    
    c0 <- build_config()
    
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
    run_geospatial_gen      <- c0$task %in% c("workbook", "metadata") && isTRUE(c0$geo_data)
    run_taxonomy_gen        <- c0$task %in% c("workbook", "metadata") && isTRUE(c0$tax_data)
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
    # Publishing now happens on its own page/button (publish_btn) — the main
    # Run here never sets a DOI/project or uploads, regardless of checkbox state.
    assign("ready_to_publish", FALSE, envir = .GlobalEnv)
    assign("upload_data_package", FALSE, envir = .GlobalEnv)
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
    
    append_log(paste("▶ Run started at", format(Sys.time(), "%H:%M:%S")))
    append_log(paste("  task =", c0$task, "| geo_data =", c0$geo_data, "| tax_data =", c0$tax_data))
    
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
        append_log(paste("❌", s$id, "not found at", script_path, "— stopping."))
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
        append_log("Run stopped due to error above.")
        pipeline_failed <- TRUE
        break
      }
    }
    
    append_log(paste("■ Run ended at", format(Sys.time(), "%H:%M:%S")))
    
    if (pipeline_failed) {
      shinyjs::html(id = "pipeline_done_banner",
                    html = "❌ <strong>Run stopped due to an error.</strong> Check the log above, fix the issue, and click Run again.")
      shinyjs::runjs("document.getElementById('pipeline_done_banner').style.cssText = 'display:block; background:#fef2f2; color:#b91c1c; border:1px solid #fecaca; padding:16px 20px; border-radius:12px; font-size:17.5px; margin-top:6px;';")
    } else {
      shinyjs::html(id = "pipeline_done_banner",
                    html = "✅ <strong>Run finished.</strong> Continue to the next step below.")
      shinyjs::runjs("document.getElementById('pipeline_done_banner').style.cssText = 'display:block; background:#f0fdf4; color:#15803d; border:1px solid #bbf7d0; padding:16px 20px; border-radius:12px; font-size:17.5px; margin-top:6px;';")
    }
  })
  
  # ==========================================================================
  # ---- Run Checks: EML validate / DPchecker (page 3) ----
  # ==========================================================================
  checks_log <- character(0)
  append_checks_log <- function(msg, raw = FALSE) {
    checks_log <<- c(checks_log, msg)
    html_text <- if (raw) style_check_lines(paste(checks_log, collapse = "\n")) else htmltools::htmlEscape(paste(checks_log, collapse = "\n"))
    shinyjs::html(id = "checks_log_box", html = html_text)
  }
  
  style_check_lines <- function(text) {
    Encoding(text) <- "UTF-8"
    text <- tryCatch(cli::ansi_strip(text), error = function(e) text)
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
      append_checks_log(paste("❌ File not found:", xml_path, "— go back and run Create Metadata first."))
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
    checks_run_at_least_once(TRUE)
  })
  
  observeEvent(input$run_dpchecker_btn, {
    xml_path <- get_xml_path()
    append_checks_log(paste("▶ Running DPchecker::run_congruence_checks() at", format(Sys.time(), "%H:%M:%S")))
    if (!file.exists(xml_path)) {
      append_checks_log(paste("❌ File not found:", xml_path, "— go back and run Create Metadata first."))
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
    checks_run_at_least_once(TRUE)
  })
  
  # ==========================================================================
  # ---- Publish: applies DOI/project/upload to the EXISTING EML file ----
  # ---- (page 4), without regenerating it via make_eml(). ----
  # ==========================================================================
  publish_log_lines <- character(0)
  append_publish_log <- function(msg) {
    publish_log_lines <<- c(publish_log_lines, msg)
    shinyjs::html(id = "publish_log_box", html = paste(publish_log_lines, collapse = "\n"))
  }
  
  # ---- Check both the on-disk XML and the saved JSON config for an existing
  # DOI, since the two can drift out of sync (hand-edited XML, stale JSON from
  # an earlier run, etc). Returns the existing DOI string if found in either
  # place, or NULL if neither has one. ----
  find_existing_doi <- function(xml_path) {
    doi_from_xml <- NULL
    if (file.exists(xml_path)) {
      doi_from_xml <- tryCatch({
        my_metadata <- EML::read_eml(xml_path, from = "xml")
        alt_id <- my_metadata$dataset$alternateIdentifier
        if (!is.null(alt_id) && length(alt_id) > 0) {
          val <- if (is.list(alt_id)) alt_id[[1]] else alt_id[1]
          if (!is.null(val) && nzchar(val)) val else NULL
        } else {
          NULL
        }
      }, error = function(e) NULL)
    }
    
    doi_from_json <- NULL
    saved_cfg <- load_config()
    if (!is.null(saved_cfg$doi) && nzchar(saved_cfg$doi)) {
      doi_from_json <- saved_cfg$doi
    }
    
    list(xml = doi_from_xml, json = doi_from_json)
  }
  
  # ---- Does the actual publish work once the user has confirmed through any
  # necessary modal. Separated out so it can be called either immediately (no
  # existing DOI found) or after modal confirmation. ----
  do_publish <- function() {
    publish_log_lines <<- character(0)
    shinyjs::html(id = "publish_log_box", html = "")
    shinyjs::html(id = "publish_done_banner", html = "")
    shinyjs::hide(id = "publish_done_banner")
    
    c0 <- build_config()
    xml_path <- paste0(c0$metadata_id, "_metadata.xml")
    
    append_publish_log(paste("▶ Publish started at", format(Sys.time(), "%H:%M:%S")))
    
    if (!file.exists(xml_path)) {
      append_publish_log(paste("❌ File not found:", xml_path, "— go back and run Create Metadata first."))
      shinyjs::html(id = "publish_done_banner",
                    html = "❌ <strong>Publish stopped — metadata file not found.</strong>")
      shinyjs::runjs("document.getElementById('publish_done_banner').style.cssText = 'display:block; background:#fef2f2; color:#b91c1c; border:1px solid #fecaca; padding:16px 20px; border-radius:12px; font-size:17.5px; margin-top:6px;';")
      return(invisible())
    }
    
    result <- tryCatch({
      append_publish_log(paste("⏳ Reading existing metadata from", xml_path, "..."))
      my_metadata <- EML::read_eml(xml_path, from = "xml")
      
      if (identical(input$doi_mode, "existing")) {
        ds_ref <- trimws(input$ds_ref_code)
        if (!nzchar(ds_ref)) {
          stop("No DataStore reference code entered. Go back and enter the 7-digit code, or switch to 'Create a new DOI'.")
        }
        append_publish_log(paste("⏳ Setting DOI from existing DataStore reference code", ds_ref, "..."))
        my_metadata <- EMLeditor::set_doi(my_metadata, ds_ref, force = TRUE)
      } else {
        append_publish_log("⏳ Creating a new DOI (initiating a draft reference on DataStore)...")
        my_metadata <- EMLeditor::set_datastore_doi(my_metadata, force = TRUE)
      }
      
      append_publish_log(paste("⏳ Setting project =", c0$project))
      my_metadata <- EMLeditor::set_project(my_metadata, c0$project)
      
      append_publish_log(paste("⏳ Writing updated metadata back to", xml_path, "..."))
      EML::write_eml(my_metadata, xml_path)
      
      xml_text <- readLines(xml_path, warn = FALSE)
      xml_text <- gsub("<alternateIdentifier>doi: ", "<alternateIdentifier>", xml_text)
      writeLines(xml_text, xml_path)
      
      # Record the resulting DOI back into the saved config, so future
      # "existing DOI" checks (find_existing_doi) see it even if the XML
      # alone isn't checked (e.g. before Create Metadata is re-run).
      new_alt_id <- tryCatch({
        reread <- EML::read_eml(xml_path, from = "xml")
        alt_id <- reread$dataset$alternateIdentifier
        if (!is.null(alt_id) && length(alt_id) > 0) {
          val <- if (is.list(alt_id)) alt_id[[1]] else alt_id[1]
          if (!is.null(val) && nzchar(val)) val else NULL
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      if (!is.null(new_alt_id)) {
        updated_cfg <- isolate(cfg())
        updated_cfg$doi <- new_alt_id
        cfg(updated_cfg)
        jsonlite::write_json(updated_cfg, CONFIG_PATH, auto_unbox = TRUE, pretty = TRUE)
        append_publish_log(paste("  Recorded DOI in saved config:", new_alt_id))
      }
      
      if (isTRUE(c0$upload_data_package)) {
        # upload_data_package() requires the *_metadata.xml file to live
        # alongside the data CSVs in `directory` — copy it there first,
        # the same way DPchecker's congruence checks need it temporarily.
        xml_in_working <- file.path(working_folder, basename(xml_path))
        file.copy(xml_path, xml_in_working, overwrite = TRUE)
        
        append_publish_log(paste("⏳ Uploading data package to DataStore from", working_folder, "..."))
        upload_result <- tryCatch({
          EMLeditor::upload_data_package(directory = working_folder, force = TRUE)
          TRUE
        }, error = function(e) {
          append_publish_log(paste("❌ Error during upload:", conditionMessage(e)))
          FALSE
        }, finally = {
          if (file.exists(xml_in_working)) file.remove(xml_in_working)
        })
        
        if (isTRUE(upload_result)) {
          append_publish_log("✅ Data package uploaded. Check DataStore's 'Files and Links' tab to confirm.")
        }
      } else {
        append_publish_log("  (Upload not checked — DOI/project set, nothing uploaded.)")
      }
      
      TRUE
    }, error = function(e) {
      append_publish_log(paste("❌ Error during publish:", conditionMessage(e)))
      FALSE
    })
    
    append_publish_log(paste("■ Publish ended at", format(Sys.time(), "%H:%M:%S")))
    
    if (isTRUE(result)) {
      shinyjs::html(id = "publish_done_banner",
                    html = "✅ <strong>Publish finished.</strong> You can close this window now.")
      shinyjs::runjs("document.getElementById('publish_done_banner').style.cssText = 'display:block; background:#f0fdf4; color:#15803d; border:1px solid #bbf7d0; padding:16px 20px; border-radius:12px; font-size:17.5px; margin-top:6px;';")
    } else {
      shinyjs::html(id = "publish_done_banner",
                    html = "❌ <strong>Publish stopped due to an error.</strong> Check the log above.")
      shinyjs::runjs("document.getElementById('publish_done_banner').style.cssText = 'display:block; background:#fef2f2; color:#b91c1c; border:1px solid #fecaca; padding:16px 20px; border-radius:12px; font-size:17.5px; margin-top:6px;';")
    }
  }
  
  observeEvent(input$publish_btn, {
    if (!isTRUE(input$ready_to_publish) || !isTRUE(input$confirm_publish)) {
      publish_log_lines <<- character(0)
      shinyjs::html(id = "publish_log_box", html = "")
      append_publish_log("⚠️ Check \"Ready to Publish\" and the confirmation checkbox above before running Publish.")
      return(invisible())
    }
    
    c0 <- isolate(build_config())
    xml_path <- paste0(c0$metadata_id, "_metadata.xml")
    existing <- find_existing_doi(xml_path)
    
    if (!is.null(existing$xml) || !is.null(existing$json)) {
      sources <- c()
      if (!is.null(existing$xml))  sources <- c(sources, sprintf("metadata file (%s): %s", basename(xml_path), existing$xml))
      if (!is.null(existing$json)) sources <- c(sources, sprintf("saved config (%s): %s", CONFIG_PATH, existing$json))
      
      showModal(modalDialog(
        title = "⚠️ Existing DOI found",
        tags$div(
          tags$p("This package already appears to have a DOI:"),
          tags$ul(lapply(sources, tags$li)),
          tags$p(style = "font-weight:600; color:#a12a1a;",
                 "Continuing may overwrite this DOI with a new one. This cannot be easily undone.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_overwrite_doi", "Overwrite and Continue", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    } else {
      do_publish()
    }
  })
  
  observeEvent(input$confirm_overwrite_doi, {
    removeModal()
    do_publish()
  })
}

shinyApp(ui, server)