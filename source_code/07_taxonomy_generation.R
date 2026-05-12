# Tricks for EML Taxonomy :

# 1. You cannot have Genus sp., Genus spp., and Genus in the same csv without it 
#    flipping its little computer table. 
#    Larus sp., Buteo, and Anhinga spp. all together are ok. 
#    Larus sp., Larus spp., Buteo, Buteo sp. will just make the computer blow a 
#    raspberry at you.
# 
# 2. You can have higher level scientific names (so Class, Order, Etc.) and it 
#    will grab the code for you, but only if they are properly capitalized. 
#    Mammalia will work, mammalia will not. 
# 
# 3. Genus cf. species and Genus species in the same csv is ok.
#
# 4. Genus Species or genus species will not work (capitalization matters).
# 
# 5. taxa.authority = c(3,9, 11),  #3 = ITIS, 9 = World Register of Marine 
#    Species, 11 = Global Biodiversity Information Facility Backbone Taxonomy. 
#    The order you put them in is the order they run in.

EMLassemblyline::template_taxonomic_coverage(path = working_folder,
                                             data.path = working_folder,
                                             taxa.table = data_taxa_tables,
                                             taxa.col = data_taxa_fields,
                                             taxa.authority = c(3,11),
                                             taxa.name.type = 'scientific',
                                             write.file = TRUE)