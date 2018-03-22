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

lat  <- -25.445615
lon  <- -48.916297
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

refs <- list(doi       = "10.1590/s0100-84042005000200003",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "http://www.scielo.br/pdf/rbb/v28n2/a03v28n2.pdf",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/plant_pollinator/excel/kaehler_et_al_2005.xlsx",
             author    = "kaehler",
             year      = "2005",
             bibtex    = "@article{Kaehler_2005, doi = {10.1590/s0100-84042005000200003}, url = {https://doi.org/10.1590%2Fs0100-84042005000200003}, year = 2005, month = {jun}, publisher = {FapUNIFESP and SciELO}, volume = {28}, number = {2}, pages = {219--228}, author = {Miriam Kaehler and Isabela G. Varassin and Renato Goldenberg}, title = {Polinizacao em uma comunidade de bromelias em floresta atlantica alto-montana no estado do Parana, Brasil}, journal = {Revista Brasileira de Botanica}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "2002-04-01",
               value = 0)


datasets <- list(name        = "kaehler_et_al_2005",
                 date        = "2002-04-01",
                 description = "Pollination of a bromeliad community in the high montane Atlantic rain forest in Paran? state, Brazil",
                 public      = TRUE)


traits <- list(date = "2002-04-01")


networks <- list(name             = "kaehler_et_al_2005",
                 date             = "2002-04-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Pollination of a bromeliad community in the high montane Atlantic rain forest in Paran? state, Brazil",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2002-04-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Set WD
# setwd("importation_mangal/kaehler_et_al_2005")
# 
# # Open file
# kaehler_et_al_2005 <- read.csv2(file = "raw/kaehler_et_al_2005.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# kaehler_et_al_2005[2, 1] <- "species"
# colnames(kaehler_et_al_2005) <- kaehler_et_al_2005[2, ]
# 
# ## Delete unused row
# kaehler_et_al_2005 <- kaehler_et_al_2005[-c(1, 2), ]
# 
# # Melt df
# kaehler_et_al_2005 <- melt(kaehler_et_al_2005, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(kaehler_et_al_2005) <- c("sp_taxon_1", "sp_taxon_2", "value")
# kaehler_et_al_2005 <- subset(kaehler_et_al_2005, kaehler_et_al_2005$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(kaehler_et_al_2005$sp_taxon_2)), as.vector(unique(kaehler_et_al_2005$sp_taxon_1)))
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
# # traits_df <- read.csv2(file = "data/kaehler_et_al_2005_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/kaehler_et_al_2005_taxo_back.csv"), row.names = FALSE)
# write.csv2(x = taxons_df, file = paste0(getwd(), "/data/kaehler_et_al_2005_taxons.csv"), row.names = FALSE)
# write.csv2(x = kaehler_et_al_2005, file = paste0(getwd(), "/data/kaehler_et_al_2005_inter.csv"), row.names = FALSE)
# # write.csv2(x = traits_df, file = paste0(getwd(), "/data/kaehler_et_al_2005_traits.csv"), row.names = FALSE)

setwd("importation_mangal/kaehler_et_al_2005")
taxo_back_df <- read.csv2("data/kaehler_et_al_2005_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("data/kaehler_et_al_2005_taxons.csv", header = TRUE)
kaehler_et_al_2005 <- read.csv2("data/kaehler_et_al_2005_inter.csv", header = TRUE)
# traits_df <- read.csv2("data/kaehler_et_al_2005_traits.csv", header = TRUE)

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
POST_interactions(kaehler_et_al_2005, enviro = enviro, attr = attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, kaehler_et_al_2005)
