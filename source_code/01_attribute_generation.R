#### EMLAssemblyline Part 1: The txt creation ####
EMLassemblyline::template_core_metadata(path = working_folder,
                                        license = "CC0") # that '0' is a zero!

EMLassemblyline::template_table_attributes(path = working_folder,
                                           data.table = data_files,
                                           write.file = TRUE)
