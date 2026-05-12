
library(EMLassemblyline)

my_metadata <- EMLassemblyline::make_eml(path = working_folder,
                                         dataset.title = package_title,
                                         data.table = data_files,
                                         data.table.name = data_names,
                                         data.table.description = data_descriptions,
                                         data.table.url = data_urls,
                                         temporal.coverage = c(startdate, enddate),
                                         maintenance.description = data_type,
                                         package.id = metadata_id,
                                         return.obj = TRUE,
                                         write.file = FALSE)

my_metadata <- EMLeditor::set_cui_code(my_metadata, cui_code)

my_metadata <- EMLeditor::set_int_rights(my_metadata, int_rights)

my_metadata <- EMLeditor::set_content_units(my_metadata,
                                            park_units)

my_metadata <- EMLeditor::set_producing_units(my_metadata, producing_units)

my_metadata <- EMLeditor::set_language(my_metadata,
                                       language)

# For demo purposes, this is turned off:
#my_metadata <- EMLeditor::set_datastore_doi(my_metadata)

# where "1234567" is the DataStore Reference id for the Project
# that the data package should be linked to.
#my_metadata <- EMLeditor::set_project(my_metadata, 1234567)

#EMLeditor::upload_data_package()

EML::eml_validate(my_metadata)

EMLassemblyline::issues()

# I wanted the xml file to pop up in the main folder, but for DPchecker it needs 
# to be in the folder with the txt files. So temporarily copying it there just to 
# run the congruence_checks tool, then deleting it.

# Write to main folder (permanent home).
xml_main <- paste0(metadata_id, "_metadata.xml")
EML::write_eml(my_metadata, xml_main)

# Copy to working_folder just for DPchecker.
xml_working <- file.path(working_folder, paste0(metadata_id, "_metadata.xml"))
file.copy(xml_main, xml_working, overwrite = TRUE)

# Run checks.
DPchecker::run_congruence_checks(directory = working_folder, check_metadata_only = TRUE)
DPchecker::run_congruence_checks(directory = working_folder)

# Clean up copy from working_folder.
file.remove(xml_working)
