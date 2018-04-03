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

lat  <- 68.35
lon  <- 18.5
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name   = "Presence/Absence",
              table_owner = "interactions",
              description = "Presence or absence of a recorded interaction",
              unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

refs <- list(doi       = "10.2307/3683041",
             jstor     = "3683041",
             pmid      = "null",
             paper_url = "https://www.jstor.org/stable/3683041?seq=1#page_scan_tab_contents",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/plant_pollinator/excel/elberling&olesen_1999.xls",
             author    = "elberling",
             year      = "1999",
             bibtex    = "@article{10.2307/3683041, ISSN = {09067590, 16000587}, URL = {http://www.jstor.org/stable/3683041}, author = {Heidi Elberling and Jens M. Olesen}, journal = {Ecography}, number = {3}, pages = {314-323}, publisher = {[Nordic Society Oikos, Wiley]}, title = {The Structure of a High Latitude Plant-Flower Visitor System: The Dominance of Flies}, volume = {22}, year = {1999}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1994-08-23",
               value = 0)


datasets <- list(name        = "elberling_olesen_1999",
                 date        = "1994-08-23",
                 description = "Flower-visiting insect at Mt. Latnjatjarro, northern Sweden",
                 public      = TRUE)


traits <- list(date = "1994-08-23")


networks <- list(name             = "elberling_olesen_1999",
                 date             = "1994-08-23",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Flower-visiting insect at Mt. Latnjatjarro, northern Sweden",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1994-08-23",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation/transect",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


# #------------------------------
#   # Cleaning matrix
# #------------------------------
# # Open file
# elberling_olesen_1999 <- read.csv2(file = "importation_mangal/elberling_olesen_1999/raw/elberling_olesen_1999.csv", header = FALSE, sep = ";")
#
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# x  <- unname(unlist(elberling_olesen_1999[1, ]))
# x[1] <- 1
# colnames(elberling_olesen_1999) <- unlist(x)
# rm(x)
# 
# ## Delete unused row
# elberling_olesen_1999 <- elberling_olesen_1999[-1, ]
# 
# # Melt df
# elberling_olesen_1999 <- melt(elberling_olesen_1999, id.vars = c(1), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(elberling_olesen_1999) <- c("sp_taxon_1", "sp_taxon_2", "value")
# elberling_olesen_1999 <- subset(elberling_olesen_1999, elberling_olesen_1999$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(elberling_olesen_1999$sp_taxon_2)), as.vector(unique(elberling_olesen_1999$sp_taxon_1)))
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
# # traits_df <- read.csv2(file = "importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = "importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_taxo_back.csv", row.names = FALSE)
# write.csv2(x = taxons_df, file = "importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_taxons.csv", row.names = FALSE)
# write.csv2(x = elberling_olesen_1999, file = "importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_inter.csv", row.names = FALSE)
# # write.csv2(x = traits_df, file = "importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_traits.csv", row.names = FALSE)

taxo_back_df <- read.csv2("importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_taxons.csv", header = TRUE)
elberling_olesen_1999 <- read.csv2("importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_inter.csv", header = TRUE)
# traits_df <- read.csv2("importation_mangal/elberling_olesen_1999/data/elberling_olesen_1999_traits.csv", header = TRUE)

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
POST_networks(networks, enviro = enviro)
POST_taxo_back()
POST_taxons(taxons_df)
# POST_traits(traits_df)
POST_interactions(elberling_olesen_1999, enviro = enviro, attr = attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, elberling_olesen_1999)
