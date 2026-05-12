
#------------------------------ TXT TO EXCEL -----------------------------------

#### Metadata Template #####
new_metadata_file <- file.path(dirname(working_folder), "blank_form", paste0(metadata_id, ".xlsx"))
success <- file.copy(metadata_template, new_metadata_file, overwrite = TRUE)
if (!success) stop("Copy failed. Check path and permissions.")
wb <- loadWorkbook(new_metadata_file)

#-------------- Attributes txt to Excel ----------------------------------------

# Grab attribute files from EML folder
txt_files <- list.files(path = working_folder, pattern = "^attribute.*\\.txt$", full.names = TRUE)

# Define the sheet name
sheet_name <- "attributes"

# Starting row (saves space for title and explanations)
start_row <- 3

# Iterate through each attribute file
for (txt_file in txt_files) {
  
  # Read the file
  data <- read_delim(txt_file, delim = "\t")
  
  # Write the file name in column A, same row as data, so you know which data comes from which csv
  writeData(wb, sheet = sheet_name, x = basename(txt_file), startCol = 1, startRow = start_row)
  
  # Write the table starting at the specified sheet and cell
  writeData(wb, sheet = sheet_name, x = data, startCol = col2int("B"), startRow = start_row)
  
  # Update the start_row for the next file
  start_row <- start_row + nrow(data) + 1
  
  # Save state
  saveWorkbook(wb, new_metadata_file, overwrite = TRUE)
}

# For human readability and simplicity, enter a blank row before every new table
# Done here and not earlier to prevent longer tables from overwriting blanks in shorter tables

# Read the sheet
data <- read.xlsx(new_metadata_file, sheet = sheet_name)

# Insert blank rows before each non-empty value in column A
insert_blank_rows <- function(df) {
  new_df <- data.frame()
  for (i in 1:nrow(df)) {
    if (!is.na(df[i, 1]) && df[i, 1] != "") {
      new_df <- rbind(new_df, rep(NA, ncol(df)))
    }
    new_df <- rbind(new_df, df[i, ])
  }
  colnames(new_df) <- colnames(df)
  return(new_df)
}

# Keep first 3 rows normal, don't need an empty row before the first table
header <- data[1:3, ]
body   <- data[-(1:3), ]

# Apply blank-row insertion only to the body
new_body <- insert_blank_rows(body)

# Combine header + modified body
final_data <- rbind(header, new_body)

# Write back into the same sheet
writeData(wb, sheet = sheet_name, x = final_data, withFilter = FALSE)

# Save workbook
saveWorkbook(wb, new_metadata_file, overwrite = TRUE)

#-------------------------- CatVars txt to Excel -------------------------------
txt_files <- list.files(path = working_folder, pattern = "^catvars.*\\.txt$", full.names = TRUE)

# Define the sheet name
sheet_name <- "catagorical_value_definitions"

# starting row (saves space for title and explanations)
start_row <- 2

# Iterate through each catvar file
for (txt_file in txt_files) {
  # Read the file
  data <- read_delim(txt_file, delim = "\t")
  
  # Write the file name in column A, same row as data, so you know which data comes from which csv
  writeData(wb, sheet = sheet_name, x = basename(txt_file), startCol = 1, startRow = start_row)
  # Write the table starting at the specified sheet and cell
  writeData(wb, sheet = sheet_name, x = data, startCol = col2int("B"), startRow = start_row)
  
  # Need to specify to keep A-C columns locked. Don't know why it works automatically for attributes and not for catvars or tableinfo.
  rows_written <- start_row:(start_row + nrow(data))
  unlocked_style <- createStyle(locked = FALSE)
  addStyle(
    wb,
    sheet = sheet_name,
    style  = unlocked_style,
    rows   = rows_written,
    cols   = 4,          
    gridExpand = TRUE,
    stack = TRUE         
  )
  
  # Update the start_row for the next entry. For some reason this doesn't have the same overwriting issue as attributes, so we don't need to rewrite things
  start_row <- start_row + nrow(data) + 2
  
  # Save state after each write to ensure it's reflected in the last_row calculation
  saveWorkbook(wb, new_metadata_file, overwrite = TRUE)
}
#---------------------- Table Names and Descriptions ---------------------------

# Need to keep certain columns from accidental editing.
rows_written <- 4:(4 + length(data_files) - 1)
unlocked_style <- createStyle(locked = FALSE)
addStyle(
  wb,
  sheet = "table_info",
  style  = unlocked_style,
  rows   = rows_written,
  cols   = c(2, 3),          
  gridExpand = TRUE,
  stack = TRUE         
)
writeData(
  wb,
  sheet = "table_info",
  x = basename(data_files),
  startCol = 1,
  startRow = 4,
  colNames = FALSE,
  rowNames = FALSE
)

# Final save the workbook after all files have been processed
saveWorkbook(wb, new_metadata_file, overwrite = TRUE)

##### end #####