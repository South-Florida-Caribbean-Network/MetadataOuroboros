# EMLassemblyline/EMLeditor/NPSdataverse workflow adapted by Katie Herrmann (2026)

library(pak)
library(lubridate)
library(svDialogs)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(EMLassemblyline)
library(readr)
#pak::pkg_install("nationalparkservice/NPSdataverse")

working_folder <- "./working_folder"
metadata_template <- "./MetadataTemplate.xlsx"
metadata_file <- list.files("./filled_form", pattern = "\\.xlsx$", full.names = TRUE)[1]
data_files <- list.files("./working_folder", pattern = "*.csv")
data_names <- character(length(data_files))
data_descriptions <- character(length(data_files))


#------------------- Toggle different workflows --------------------------------

task     <- "reboot"   # "workbook" | "fix" | "metadata" | "reboot"
geo_data <- FALSE         # only used when task == "workbook"
tax_data <- TRUE         # only used when task == "workbook"

run_attribute_gen       <- task == "workbook"
run_attribute_check     <- task == "workbook"
run_specific_attr_check <- task == "fix"
run_delete_catvars      <- task == "fix"
run_catvar_gen          <- task %in% c("workbook", "fix")
run_geospatial_gen      <- task == "workbook" && geo_data
run_taxonomy_gen        <- task == "workbook" && tax_data
run_txt_to_excel        <- task %in% c("workbook", "fix", "reboot")
run_excel_to_txt        <- task == "metadata"
run_eml_creation        <- task == "metadata"


#---------------------- Fill these out!-----------------------------------------

# ---- File Name ----
# This + "_metadata.xml" will be your eventual file name.
metadata_id             <- "Example_Conch"

# ---- Package Title ----
# Needs to be 5 words or more:
package_title           <- "Metadata testing the ouroboros code"

# ---- Data Info ----
# Can be "ongoing" or "complete"
data_type               <- "complete"
# The park units the data was collected at.
park_units              <- c("BISC","BUIS","DRTO","SARI","VIIS")
# The unit that produced the data.
producing_units         <- c("SFCN")
# Can be "PUBLIC", "FED ONLY", "FEDCON", "DL ONLY", or "NOCON"
cui_code                <- c("PUBLIC")
# Choose from "restricted", "public" or "CC0" (zero)
int_rights              <- c("public")
language                <- c("English")

# ---- Taxonomy Info ----
# The file(s) where scientific names are located.
data_taxa_tables        <- "taxonomy.csv"
# The column where your scientific names are within the data files.
data_taxa_fields        <- "species"

# ---- Geospatial Info ----
# The table with the geographic coordinates.
data_coordinates_table  <- "plots_table.csv"
# The columns with the latitude, longitude, and name of that point.
data_latitude           <- "X_Coord"
data_longitude          <- "Y_Coord"
data_sitename           <- "Plot"

# ---- Date Info ----
# The table and columns with dates. Start and end may be different columns.
data_dates_table        <- "2021_2024_SFCN_QueenConch_ConchData_v1.csv"
data_startdate_field    <- "Date"
data_enddate_field      <- "Date"


#--------------------- You don't need to edit this next bit --------------------
data_urls <- c(rep("temporary URL", length(data_files)))


# Automatically grab the oldest and newest dates from those columns.
dates_df <- read.csv(file.path(working_folder, data_dates_table))
startdate <- as.Date(min(parse_date_time(dates_df[[data_startdate_field]], 
                                         orders = c("ymd", "mdy", "dmy")), 
                         na.rm = TRUE))
enddate   <- as.Date(max(parse_date_time(dates_df[[data_enddate_field]], 
                                         orders = c("ymd", "mdy", "dmy")), 
                         na.rm = TRUE))


#---------------------------- Run this -----------------------------------------

# 01: Creates attribute txt files.
if (run_attribute_gen) source(file.path("source_code", "01_attribute_generation.R"))

# 02: Checks attribute txt files to make sure classes are correct. A table will
#     pop up for editing. Valid classes: "numeric", "categorical", "character", "Date"
if (run_attribute_check) source(file.path("source_code", "02_attribute_check.R"))

# 03: Call up a specific table to recheck its attribute classes.
if (run_specific_attr_check) source(file.path("source_code", "03_specific_attribute_check.R"))

# 04: Deletes catvar data so you can regenerate it cleanly.
if (run_delete_catvars) source(file.path("source_code", "04_delete_catvars.R"))

# 05: Creates categorical variable txt files.
if (run_catvar_gen) source(file.path("source_code", "05_catvar_generation.R"))

# 06: Creates geospatial txt files.
if (run_geospatial_gen) source(file.path("source_code", "06_geospatial_generation.R"))

# 07: Creates taxonomy txt files. This sometimes takes a while to run.
#     See script for helpful tips if your taxonomy isn't resolving.
if (run_taxonomy_gen) source(file.path("source_code", "07_taxonomy_generation.R"))

# 08: Uses txt files to populate the Excel document.
if (run_txt_to_excel) source(file.path("source_code", "08_txt_to_excel.R"))

# 09: Excel to txt files.
if (run_excel_to_txt) source(file.path("source_code", "09_excel_to_txt.R"))

# 10: Final EML creation. If you are publishing on IRMA, please uncomment 
#     lines 29, 33, and 35 as needed. Typically set to commented so they do not 
#     run before the metadata has been approved for publishing.
if (run_eml_creation) source(file.path("source_code", "10_EML_creation.R"))
