
##### EXCEL TO TXT ##### #####


#### cat vars ####

filled_form_folder <- file.path(working_folder)
excel_files <- list.files(path = file.path(working_folder, "..", "filled_form"),
                          pattern = "\\.xlsx$", full.names = TRUE)
if (length(excel_files) == 0) stop("No Excel file found in filled_form folder.")

metadata_file <- excel_files[1] 

sheet_name <- "catagorical_value_definitions"
rowstart <- 2
metadata <- read_excel(
  metadata_file,
  sheet = sheet_name,
  skip = rowstart - 1,
  col_names = FALSE
)

colnames(metadata) <- c("txt_file_name", "attributeName", "code", "definition")

metadata <- metadata %>%
  fill(txt_file_name, .direction = "down") %>%
  filter(!is.na(txt_file_name)) %>%
  filter(!(is.na(attributeName) & is.na(code) & is.na(definition)))

# Function to process and write data for each txt file
process_and_write_txt <- function(file_name, data) {
  
  # Select relevant columns
  data_to_write <- data %>%
    select(attributeName, code, definition)
  
  # Output file path
  output_file <- file.path(filled_form_folder, file_name)
  
  # Write or append WITHOUT headers
  write.table(
    data_to_write,
    file = output_file,
    sep = "\t",
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    append = file.exists(output_file)
  )
  
  cat("Data written to", output_file, "\n")
}

# Remove old output files (important!)
unique_files <- unique(metadata$txt_file_name)

for (f in unique_files) {
  out <- file.path(filled_form_folder, f)
  if (file.exists(out)) {
    file.remove(out)
  }
}

# Process and write data for each txt file
for (file_name in unique_files) {
  file_data <- metadata %>%
    filter(txt_file_name == file_name)
  
  process_and_write_txt(file_name, file_data)
}

# Read data names and descriptions back from the filled Excel sheet
init_sheet <- read.xlsx(metadata_file, sheet = "table_info")

data_names        <- init_sheet[3:nrow(init_sheet), 2]  
data_descriptions <- init_sheet[3:nrow(init_sheet), 3]  

# Drop any empty rows (in case template has more rows than datasets)
data_names        <- data_names[!is.na(data_names)]
data_descriptions <- data_descriptions[!is.na(data_descriptions)]


#### abstract ####

sheet_name <- "abstract"

val <- read_excel(
  metadata_file,
  sheet = sheet_name,
  range = "B2",
  col_names = FALSE
)

abstract_value <- if (ncol(val) == 0 || nrow(val) == 0) {
  NA
} else {
  v <- val[[1]]
  if (is.na(v) || v == "") NA else v
}

if (!is.na(abstract_value)) {
  abstract_value <- as.character(abstract_value)
  
  # Trying to remove any txt weirdness
  abstract_value <- gsub("\r\n", " ", abstract_value)   
  abstract_value <- gsub("\r",   " ", abstract_value)   
  abstract_value <- gsub("\n",   " ", abstract_value)   
  
  # Collapse multiple spaces
  abstract_value <- gsub("\\s+", " ", abstract_value)
  
  # Trim leading/trailing whitespace
  abstract_value <- trimws(abstract_value)
  
  # Strip any remaining non-printing control characters
  abstract_value <- gsub("[[:cntrl:]]", "", abstract_value)
  
  # writeLines() kept introducing things into the txt file so we are just going 
  # to  use a raw connection instead
  con <- file(file.path(filled_form_folder, "abstract.txt"), open = "wt")
  cat(abstract_value, file = con, append = FALSE)
  close(con)
}

#### additional info ####

sheet_name <- "additional info"

# Read B2
val <- read_excel(
  metadata_file,
  sheet = sheet_name,
  range = "B2",
  col_names = FALSE
)

additional_info_value <- if (ncol(val) == 0 || nrow(val) == 0) {
  NA
} else {
  v <- val[[1]]
  if (is.na(v) || v == "") NA else v
}

writeLines(as.character(additional_info_value), file.path(filled_form_folder, "additional_info.txt"))

#### methods ####

sheet_name <- "methods"

# Read B2
methods_value <- read_excel(metadata_file, sheet = sheet_name, range = "B2", col_names = FALSE)[[1]]

writeLines(as.character(methods_value), file.path(filled_form_folder, "methods.txt"))

#### keywords ####

sheet_name <- "keywords"

# Read columns B and C starting at row 4
keywords_data <- read_excel(metadata_file, sheet = sheet_name, range = "B4:C104", col_names = FALSE)

# Drop rows where both columns are empty
keywords_data <- keywords_data[rowSums(is.na(keywords_data)) != ncol(keywords_data), ]

# Convert to character to avoid factors/NA issues
keywords_data[] <- lapply(keywords_data, function(x) as.character(na.omit(x)))

# Write out as two columns, tab-separated
write.table(
  keywords_data,
  file = file.path(filled_form_folder, "keywords.txt"),
  sep = "\t",          # keeps two columns separated by tabs
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

#### custom units ####

sheet_name <- "custom_units"

# Read columns B and C starting at row 4
custom_units_data <- read_excel(metadata_file, sheet = sheet_name, range = "A8:E104", col_names = FALSE)

# Drop rows where both columns are empty
custom_units_data <- custom_units_data[rowSums(is.na(custom_units_data)) != ncol(custom_units_data), ]

# Convert to character to avoid factors/NA issues
custom_units_data[] <- lapply(custom_units_data, function(x) as.character(na.omit(x)))

# Write out as two columns, tab-separated
write.table(
  custom_units_data,
  file = file.path(filled_form_folder, "custom_units.txt"),
  sep = "\t",          # keeps two columns separated by tabs
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

#### personnel ####

sheet_name <- "personnel"

# Read A-J
personnel_data <- read_excel(metadata_file, sheet = sheet_name, range = "A13:J104", col_names = FALSE)

# Drop rows where all columns are empty
personnel_data <- personnel_data[rowSums(is.na(personnel_data)) != ncol(personnel_data), ]

# Convert everything to character
personnel_data <- data.frame(lapply(personnel_data, as.character), stringsAsFactors = FALSE)

# Write out as tab-separated text
write.table(
  personnel_data,
  file = file.path(filled_form_folder, "personnel.txt"),
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

#### attributes ####

# attributes uses the file names in col A to determine which rows end up in 
# which txt file. That way you don't need 10 different attribute tabs, one for
# each csv. 
sheet_name <- "attributes"

# Read A–H (A=txt_file_name, B–H=7 fields). Adjust start row if needed.
metadata <- read_excel(
  metadata_file,
  sheet = sheet_name,
  range = "A3:H999",
  col_names = FALSE
)

# Assign exact column names (1 + 7)
colnames(metadata) <- c(
  "txt_file_name",
  "attributeName",
  "attributeDefinition",
  "class",
  "unit",
  "dateTimeFormatString",
  "missingValueCode",
  "missingValueCodeExplanation"
)

# Carry forward txt_file_name
metadata <- metadata %>%
  fill(txt_file_name, .direction = "down")

# Ensure character type for consistent output (do NOT drop NAs)
metadata <- metadata %>%
  mutate(across(everything(), ~ as.character(.)))

# Keep rows where txt_file_name exists AND at least one attribute field is non-empty.
# This preserves literal "(blank)" and "NA" text, only dropping true NA/"" empties.
nonempty_attr <- function(x) !is.na(x) & trimws(x) != ""
metadata <- metadata %>%
  filter(!is.na(txt_file_name) & trimws(txt_file_name) != "") %>%
  filter(if_any(
    c(attributeName, attributeDefinition, class, unit,
      dateTimeFormatString, missingValueCode, missingValueCodeExplanation),
    nonempty_attr
  ))

# Function to process and write data for each txt file
process_and_write_txt <- function(file_name, data) {
  data_to_write <- data %>%
    select(attributeName, attributeDefinition, class, unit,
           dateTimeFormatString, missingValueCode, missingValueCodeExplanation)
  
  # Replace NA with empty strings
  data_to_write[is.na(data_to_write)] <- ""
  
  output_file <- file.path(filled_form_folder, file_name)
  
  # Always overwrite the file, no headers
  write.table(
    data_to_write,
    file = output_file,
    sep = "\t",
    col.names = FALSE,   
    row.names = FALSE,
    quote = FALSE
  )
  
  cat("File rewritten:", output_file, "\n")
  
}

# Process by file name
unique_files <- unique(metadata$txt_file_name)

for (file_name in unique_files) {
  file_data <- metadata %>% filter(txt_file_name == file_name)
  process_and_write_txt(file_name, file_data)
}

##### end #####

