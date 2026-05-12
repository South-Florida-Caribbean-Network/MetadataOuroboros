# If you need to look at just one attribute table
attr_files <- list.files(
  path = working_folder,
  pattern = "^attributes_.*\\.txt$",
  full.names = TRUE
)
if (length(attr_files) == 0) stop("No attribute files found in the folder.")

file_to_open <- dlgList(attr_files, title = "Select attribute file to view")$res

if (!is.null(file_to_open) && file.exists(file_to_open)) {
  attributes_data <- read.delim(file_to_open, header = TRUE, stringsAsFactors = FALSE)
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
  write.table(attributes_data, file_to_open, sep = "\t", row.names = FALSE, quote = FALSE)
} else {
  message("No file selected or file does not exist.")
}
