txt_files <- list.files(path = working_folder, pattern = "^catvars.*\\.txt$", full.names = TRUE)
file.remove(txt_files)