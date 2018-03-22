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

lat  <- 74.5
lon  <- -20.5
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name   = "Presence/Absence",
              table_owner = "interactions",
              description = "Presence or absence of a recorded interaction",
              unit        = "null")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

refs <- list(doi       = "null",
             jstor     = "null",
             pmid      = "null",
             url       = "null",
             paper_url = "null",
             data_url  = "http://www.web-of-life.es/map.php?type=5",
             author    = "elberling",
             year      = "NA",
             bibtex    = "null")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1111-11-11",
               value = 0)


datasets <- list(name        = "Elberling_Olesen",
                 date        = "1111-11-11",
                 description = "null",
                 public      = TRUE)


traits <- list(date = "1111-11-11")


networks <- list(name             = "Elberling_Olesen",
                 date             = "1111-11-11",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "null",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1111-11-11",
              direction     = "unknown",
              type          = "unknown",
              method        = "null",
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
# setwd("importation_mangal/Elberling_Olesen")
# 
# # Open file
# Elberling_Olesen <- read.csv2(file = "raw/Elberling_Olesen.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# x  <- Elberling_Olesen[1, ]
# x[1] <- "species"
# colnames(Elberling_Olesen) <- x
# rm(x)
# 
# ## Delete unused row
# Elberling_Olesen <- Elberling_Olesen[-1, ]
# 
# # Melt df
# Elberling_Olesen <- melt(Elberling_Olesen, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(Elberling_Olesen) <- c("sp_taxon_1", "sp_taxon_2", "value")
# Elberling_Olesen <- subset(Elberling_Olesen, Elberling_Olesen$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(Elberling_Olesen$sp_taxon_2)), as.vector(unique(Elberling_Olesen$sp_taxon_1)))
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
# # traits_df <- read.csv2(file = "data/Elberling_Olesen_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/Elberling_Olesen_taxo_back.csv"), row.names = FALSE)
# write.csv2(x = taxons_df, file = paste0(getwd(), "/data/Elberling_Olesen_taxons.csv"), row.names = FALSE)
# write.csv2(x = Elberling_Olesen, file = paste0(getwd(), "/data/Elberling_Olesen_inter.csv"), row.names = FALSE)
# # write.csv2(x = traits_df, file = paste0(getwd(), "/data/Elberling_Olesen_traits.csv"), row.names = FALSE)

setwd("importation_mangal/Elberling_Olesen")
taxo_back_df <- read.csv2("data/Elberling_Olesen_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("data/Elberling_Olesen_taxons.csv", header = TRUE)
Elberling_Olesen <- read.csv2("data/Elberling_Olesen_inter.csv", header = TRUE)
# traits_df <- read.csv2("data/Elberling_Olesen_traits.csv", header = TRUE)

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
POST_interactions(Elberling_Olesen, enviro = enviro, attr = attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, Elberling_Olesen)
