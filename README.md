# MetadataOuroboros

MetadataOuroboros builds on the NPSdataverse/EMLassemblyline/EMLeditor workflow to 
simplify the metadata creation process! It moves all required inputs in 
one place so you can fill everything out in one fell swoop. It also generates a 
formatted, color-coded Excel workbook with data entry restrictions, making it 
easier for data managers to hand off to staff for metadata collection. Once the 
workbook is returned, the code extracts the responses, populates the 
EMLAssemblyLine txt files, and produces a finished XML file.

If you have any questions, please reach out to Katherine "Katie" Herrmann at KatherineHerrmann@nps.gov. 

For more information on NPSdataverse/EMLassemblyline/EMLeditor, please check out https://doi-nps.github.io/EMLeditor/

---

## Toggles

Open `00_initialize.R` and set your workflow toggle before running:

| Toggle | When to use |
|--------|-------------|
| `workbook` | Initial setup: generates the blank Excel workbook and EMLassemblyline txt files |
| `fix` | Corrects misclassified categorical variables, updates the attribute table, and regenerates the workbook. Only run this step on newly generated workbooks, before any fields have been filled out! |
| `metadata` | Reads the filled workbook from `filled_form/` and produces the final XML file |

You can also enable the following flags if your dataset includes:
- `geo_data` — geospatial data
- `tax_data` — taxonomic data

---

## How to Use

1.  Place your CSV files in `working_folder/`.
2.  Open `00_initialize.R`.
3.  Set your toggle and any data flags (see Toggles above).
4.  Fill out all fields in the **"Fill these out!"** section.
5.  Set the toggle to `workbook` and run the full script.
6.  Review the attribute tables that appear, then confirm that all attributes 
    are correctly classified. If not, edit them in the table. You only need to 
    edit `class`, other fields will be adjusted automatically.
7.  Open the generated workbook in `blank_form/` and verify:
    - The **Attributes** tab looks correct.
    - The **Categorical Values** tabs look correct.
    - If anything needs correcting, switch the toggle to `fix`, rerun the 
      script, and make the necessary changes. Repeat until the workbook looks 
      right.
    - *Warning*: Do not run `fix` on filled out workbooks! It overwrites pages!
8.  Distribute the workbook to whoever will be filling out the metadata.
9.  When the workbook is returned, review it for completeness. Data management 
    may need to fill in certain fields, such as no-data values for attributes or 
    personnel information.
10. Place the completed workbook in `filled_form/`.
11. Switch the toggle to `metadata` and run the full script.
12. If any schema or template validation errors appear, run the validation 
    checks and resolve them before finalizing.
13. If you are publishing on IRMA, uncomment lines 29, 33, and 35 as needed. 
    These lines are typically left commented out to prevent accidental publishing 
    before the metadata have been approved.

---

## Workbook

The workbook is generated from the `MetadataTemplate.xlsx`. It uses conditional 
formatting, protected sheets, color coding, and data validation. If you want to 
make any changes you just need to edit the template, and those changes will 
carry over to all forms generated.

`table_info`, `attributes`, and `categorical_value_definitions` all have locked 
columns so users cannot accidentally overwrite pre-populated fields.

Either before the workbook is handed off or when it is handed back, data 
management typically will need to fill out part of `attributes` 
(specifically the MissingValueCodes column) and `personnel`.

---

## Special Thanks

The Excel workbook is inspired by metadata workbooks created by Terry Arundel at the USGS.
Special thanks to Caitlin Andrews, Wendy Thorsdatter, Melina Kompella, and the data scientists at the NPS Inventory & Monitoring Division.
