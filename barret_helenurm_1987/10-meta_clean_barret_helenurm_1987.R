# # Set libraries
# library(reshape2)
# library(tidyr)
# library(jsonlite)
# library(httr)
# library(data.table)
# library(rcrossref)
# library(taxize)
# library(stringr)
# 
# library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- 46.553731
lon  <- -66.071245
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name   = "Number of pollinator",
              table_owner = "interactions",
              description = "Number of insects captured while pollination",
              unit        = "Individual captured")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

refs <- list(doi       = "10.1139/b87-278",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "http://labs.eeb.utoronto.ca/barrett/pdf/schb_57.pdf",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/plant_pollinator/excel/barret&helenurm_1987.xls",
             author    = "barret",
             year      = "1987",
             bibtex    = "@article{Barrett_1987, doi = {10.1139/b87-278}, url = {https://doi.org/10.1139%2Fb87-278}, year = 1987, month = {oct}, publisher = {Canadian Science Publishing}, volume = {65}, number = {10}, pages = {2036--2046}, author = {Spencer C. H. Barrett and Kaius Helenurm}, title = {The reproductive biology of boreal forest herbs. I. Breeding systems and pollination}, journal = {Canadian Journal of Botany}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1979-01-01",
               value = 0)


datasets <- list(name        = "barret_helenurm_1987",
                 date        = "1979-01-01",
                 description = "Understory perennial plants interaction with pollinator, 5 km east of Doaktown, Northumberland County, central New Brunswick, Canada",
                 public      = TRUE)


traits <- list(date = "1979-01-01")


networks <- list(name             = "barret_helenurm_1987",
                 date             = "1979-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Understory perennial plants interaction with pollinator, 5 km east of Doaktown, Northumberland County, central New Brunswick, Canada",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1979-01-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation/capture",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Open file
# barret_helenurm_1987 <- read.csv2(file = "importation_mangal/barret&helenurm_1987/raw/barret&helenurm_1987.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
# # Cleaning for melt()
# ## Merge two first COLUMNS Genus species
# barret_helenurm_1987[is.na(barret_helenurm_1987)] <- "sp."
# barret_helenurm_1987 <- unite(barret_helenurm_1987, sp1, c(V1, V2), sep = " ", remove = TRUE)
# 
# ### Si on choisis de retirer les sp, bonne fonction pour unir les 2 colonnes avec
# ### un " " sans inclure les NA: str_interp(string, env = parent.frame())
# 
# ## Get ROW one with Genus_species
# x  <- paste(barret_helenurm_1987[1, ], sep =" ", barret_helenurm_1987[2, ])
# x[1] <- "species"
# colnames(barret_helenurm_1987) <- x
# rm(x)
# 
# ## Delete unused row
# barret_helenurm_1987 <- barret_helenurm_1987[-c(1, 2, 3), -2]
# 
# # Melt df
# barret_helenurm_1987 <- melt(barret_helenurm_1987, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(barret_helenurm_1987) <- c("sp_taxon_1", "sp_taxon_2", "value")
# barret_helenurm_1987 <- subset(barret_helenurm_1987, barret_helenurm_1987$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(barret_helenurm_1987$sp_taxon_2)), as.vector(unique(barret_helenurm_1987$sp_taxon_1)))
# 
# 
# ### Check for spelling mistakes... ###
# 
# 
# ## Remove sp
# 
# taxo_back <- vector()
# 
# for (i in 1:length(taxon)) {
#   
#   if(((str_detect(taxon[i], "[:digit:]") == TRUE || str_detect(taxon[i], "[:punct:]") == TRUE) &
#        str_detect(taxon[i], "sp") == TRUE) ||
#        str_detect(taxon[i], "n\\.i\\.") == TRUE ||
#        str_detect(taxon[i], "sp$") == TRUE){
#     
#     taxo_back[i] <- word(taxon[i], start = 1)
#     
#   } else {
#     taxo_back[i] <- taxon[i]
#   }
# }
# 
# taxo_back <- unique(taxo_back)
# 
# 
# ## Select only taxa not yet in db
# 
# server <- "http://localhost:3000"
# 
# taxo_back_df <- data.frame()
# 
# for (i in 1:length(taxo_back)) {
#   
#   path <- modify_url(server, path = paste0("/api/v0/","taxo_backs/?name=", str_replace(taxo_back[i], " ", "%20")))
#   
#   if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json")))) == 0) {
#     
#     taxo_back_df[nrow(taxo_back_df)+1, 1] <- taxo_back[i]
#   }
# }
# 
# rm(taxo_back)
# names(taxo_back_df) <- c("name")
# 
# ## Get code by species
# taxo_back_df[, "bold"] <- NA
# taxo_back_df[, "eol"]  <- NA
# taxo_back_df[, "tsn"]  <- NA
# taxo_back_df[, "ncbi"] <- NA
# 
# ### Encore probleme d"identification avec les api... ###
# 
# for (i in 1:nrow(taxo_back_df)) {
#   try (expr = (taxo_back_df[i, 2] <- get_boldid(taxo_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
#   try (expr = (taxo_back_df[i, 3] <- get_eolid(taxo_back_df[i, 1], row = 5, verbose = FALSE, key = 110258)[1]), silent = TRUE)
#   try (expr = (taxo_back_df[i, 4] <- get_tsn(taxo_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
#   try (expr = (taxo_back_df[i, 5] <- get_uid(taxo_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
# }
# 
# # Create taxons_df
# 
# taxons_df <- data.frame(taxon, NA)
# names(taxons_df) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){   
#     
#     taxons_df[i, 2] <- word(taxons_df[i, 1], start = 1)
#     
#   } else {
#     taxons_df[i, 2] <- as.character(taxons_df[i, 1])
#   }
# }
# 
# #------------------------------
# # Set traits table
# #------------------------------
# 
# # traits_df <- read.csv2(file = "importation_mangal/barret&helenurm_1987/data/barret_helenurm_1987_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = "importation_mangal/barret&helenurm_1987/data/barret_helenurm_1987_taxo_back.csv", row.names = FALSE)
# write.csv2(x = taxons_df, file = "importation_mangal/barret&helenurm_1987/data/barret_helenurm_1987_taxons.csv", row.names = FALSE)
# write.csv2(x = barret_helenurm_1987, file = "importation_mangal/barret&helenurm_1987/data/barret_helenurm_1987_inter.csv", row.names = FALSE)
# # write.csv2(x = traits_df, file = "importation_mangal/barret&helenurm_1987/data/barret_helenurm_1987_traits.csv", row.names = FALSE)

taxo_back_df <- read.csv2("importation_mangal/barret_helenurm_1987/data/barret_helenurm_1987_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("importation_mangal/barret_helenurm_1987/data/barret_helenurm_1987_taxons.csv", header = TRUE)
barret_helenurm_1987 <- read.csv2("importation_mangal/barret_helenurm_1987/data/barret_helenurm_1987_inter.csv", header = TRUE)
# traits_df <- read.csv2("importation_mangal/barret&helenurm_1987/data/barret_helenurm_1987_traits.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attributes(attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
POST_refs()
POST_users()
# POST_environments(enviro, attr_##)
POST_datasets()
POST_networks(networks, enviro)
POST_taxo_back()
POST_taxons(taxons_df)
# POST_traits(traits_df)
POST_interactions(barret_helenurm_1987, enviro = enviro, attr = attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, barret_helenurm_1987)
