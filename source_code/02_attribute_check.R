# Looking at/editing attribute tables
attr_files <- list.files(
  path = working_folder,
  pattern = "^attributes_.*\\.txt$",
  full.names = TRUE
)
if (length(attr_files) == 0) stop("No attribute files found in the folder.")

for (f in attr_files) {
  message("Editing: ", basename(f))
  attributes_data <- read.delim(f, header = TRUE, stringsAsFactors = FALSE)
  valid_classes <- c("numeric", "categorical", "character", "Date")
  
  attributes_data <- edit(attributes_data)
  
  bad_rows <- which(!attributes_data$class %in% valid_classes)
  if (length(bad_rows) > 0) {
    stop(
      "Invalid class value(s) found:\n",
      "  Row(s):   ", paste(bad_rows, collapse = ", "), "\n",
      "  Values: ", paste(shQuote(attributes_data$class[bad_rows]), collapse = ", "), "\n",
      "  Valid classes: ", paste(shQuote(valid_classes), collapse = ", ")
    )
  }
  attributes_data <- attributes_data %>%
    mutate(
      unit = case_when(class == "numeric" ~ "!Add units here!", .default = ""),
      dateTimeFormatString = case_when(class == "Date" ~ "!Add datetime specifier here!", .default = "")
    )
  write.table(attributes_data, f, sep = "\t", row.names = FALSE, quote = FALSE)
}
