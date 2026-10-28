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
my_metadata <- EMLeditor::set_content_units(my_metadata, park_units)
my_metadata <- EMLeditor::set_producing_units(my_metadata, producing_units)
my_metadata <- EMLeditor::set_language(my_metadata, language)

# ---------------------------------------------------------------------------
# Optional: set a DOI directly from the console.
#
# The dashboard's Publish step (page 4 / publish_btn) handles DOI/project/
# upload logic on its own and does NOT rely on these variables — it always
# runs with force = TRUE and uses its own in-app overwrite-confirmation
# modal instead of a console prompt. These flags only matter if you're
# running this script by hand (e.g. straight in the R console or via
# source()), not through the dashboard.
#
# To use this from the console, set ONE of the following before sourcing:
#
#   set_doi_now <- "existing"   # use an existing DataStore reference code
#   ds_ref_code  <- "1234567"   #   ...the 7-digit code, required if "existing"
#
#   set_doi_now <- "new"        # auto-generate a new DOI / draft reference
#
# Leave set_doi_now unset (or NULL/"none") to skip DOI-setting entirely,
# which is the default/dashboard behavior.
#
# Both set_doi() and set_datastore_doi() default to force = FALSE here, so
# if a DOI already exists you'll get the normal interactive console prompt
# asking whether you want to overwrite it.
# ---------------------------------------------------------------------------
if (!exists("set_doi_now")) set_doi_now <- "none"

if (identical(set_doi_now, "existing")) {
  if (!exists("ds_ref_code") || !nzchar(ds_ref_code)) {
    stop("set_doi_now is 'existing' but ds_ref_code is not set. ",
         "Set ds_ref_code <- \"1234567\" (your 7-digit DataStore reference code) before sourcing.")
  }
  my_metadata <- EMLeditor::set_doi(my_metadata, ds_ref_code)
  
} else if (identical(set_doi_now, "new")) {
  my_metadata <- EMLeditor::set_datastore_doi(my_metadata)
  
} else if (!identical(set_doi_now, "none")) {
  stop("set_doi_now must be \"existing\", \"new\", or \"none\" (got: ", set_doi_now, ")")
}

EML::eml_validate(my_metadata)
EMLassemblyline::issues()

# I wanted the xml file to pop up in the main folder, but for DPchecker it
# needs to be in the folder with the txt files. So temporarily copying it
# there just to run the congruence_checks tool, then deleting it.

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

# Normalize any "doi: " prefix EMLassemblyline/EMLeditor may have left in
# alternateIdentifier (harmless no-op if there's no DOI yet).
xml_text <- readLines(xml_main, warn = FALSE)
xml_text <- gsub("<alternateIdentifier>doi: ", "<alternateIdentifier>", xml_text)
writeLines(xml_text, xml_main)

if (!identical(set_doi_now, "none")) {
  doi_check <- grep("alternateIdentifier", readLines(xml_main, warn = FALSE), value = TRUE)
  if (length(doi_check) > 0) {
    message("DOI recorded in metadata: ", doi_check)
  }
}