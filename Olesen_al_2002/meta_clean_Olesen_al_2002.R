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

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Number of visit by a pollinator",
                   table_owner = "interactions",
                   description = "Number of individual of a species observed/caught on a flower",
                   unit        = "Number of individual")


# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")


refs <- list(doi       = "10.1046/j.1472-4642.2002.00148.x",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://onlinelibrary.wiley.com/doi/abs/10.1046/j.1472-4642.2002.00148.x",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/html/olesen_et_al_2002.html",
             author    = "olesen",
             year      = "2002",
             bibtex    = "@article{Olesen_2002, doi = {10.1046/j.1472-4642.2002.00148.x}, url = {https://doi.org/10.1046%2Fj.1472-4642.2002.00148.x}, year = 2002, month = {may}, publisher = {Wiley-Blackwell}, volume = {8}, number = {3}, pages = {181--192}, author = {Jens M. Olesen and Louise I. Eskildsen and Shadila Venkatasamy}, title = {Invasion of pollination networks on oceanic islands: importance of invader complexes and endemic super generalists}, journal = {Diversity and Distributions}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


datasets <- list(name        = "Olesen_al_2002",
                 date        = "2000-01-01",
                 description = "Pollination networks for two oceanic islands, the Azorean Flores and the Mauritian Ile aux Aigrettes",
                 public      = TRUE)


traits <- list(date = "1111-11-11")


# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Set WD
# setwd("importation_mangal/olesen_al_2002")
# 
#   # Open file
#   olesen_aigrettes <- read.csv2(file = "raw/olesen_aigrettes.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
#   # Cleaning for melt()
#   ## Merge two first COLUMNS Genus species
#   olesen_aigrettes <- olesen_aigrettes[-1, -1]
#   olesen_aigrettes[1:2, 1:2] <- "sp."
#   olesen_aigrettes <- unite(olesen_aigrettes, sp1, c(V2, V3), sep = " ", remove = TRUE)
# 
#   ## Get ROW one with Genus_species
#   x  <- paste(olesen_aigrettes[1, ], sep =" ", olesen_aigrettes[2, ])
#   x[1] <- "species"
#   colnames(olesen_aigrettes) <- x
#   rm(x)
# 
#   ## Delete unused row
#   olesen_aigrettes <- olesen_aigrettes[-c(1:3), -2]
# 
#   # Melt df
#   olesen_aigrettes <- melt(olesen_aigrettes, id.vars = c("species"), na.rm = TRUE)
# 
#   # Remove interaction value = 0 (no interaction)
#   names(olesen_aigrettes) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   olesen_aigrettes <- subset(olesen_aigrettes, olesen_aigrettes$value != 0)
# 
# # olesen_flores
# 
#   olesen_flores <- read.csv2(file = "raw/olesen_flores.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
#   olesen_flores <- olesen_flores[-1, -1]
#   olesen_flores[1:2, 1:2] <- "sp."
#   olesen_flores <- unite(olesen_flores, sp1, c(V2, V3), sep = " ", remove = TRUE)
#   x  <- paste(olesen_flores[1, ], sep =" ", olesen_flores[2, ])
#   x[1] <- "species"
#   colnames(olesen_flores) <- x
#   rm(x)
#   olesen_flores <- olesen_flores[-c(1:3), -2]
#   olesen_flores <- melt(olesen_flores, id.vars = c("species"), na.rm = TRUE)
#   names(olesen_flores) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   olesen_flores <- subset(olesen_flores, olesen_flores$value != 0)
#   
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- unique(c(as.vector(unique(olesen_aigrettes$sp_taxon_2)), as.vector(unique(olesen_aigrettes$sp_taxon_1)),
#                   as.vector(unique(olesen_flores$sp_taxon_2)), as.vector(unique(olesen_flores$sp_taxon_1))))
# 
# 
# ### Check for spelling mistakes... ###
# 
# 
# ### Remove sp
# 
# taxo_back <- vector()
# 
# for (i in 1:length(taxon)) {
# 
#   if(((str_detect(taxon[i], "[:digit:]") == TRUE || str_detect(taxon[i], "[:punct:]") == TRUE) &
#        str_detect(taxon[i], "sp") == TRUE) ||
#        str_detect(taxon[i], "n\\.i\\.") ||
#        str_detect(taxon[i], "sp$")){
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

# # Writing taxo_back_df
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/olesen_taxo_back.csv"), row.names = FALSE)

setwd("importation_mangal/olesen")
taxo_back_df <- read.csv2("data/olesen_taxo_back.csv", header = TRUE)

#------------------------------
  # POST commun table
#------------------------------
POST_attributes(attr_inter)

# POST_attributes(attr1)

POST_refs()

POST_users()

POST_datasets()

POST_taxo_back()

# POST_traits(traits_df)

# #------------------------------
# # olesen 1
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(olesen_aigrettes$sp_taxon_2)), as.vector(unique(olesen_aigrettes$sp_taxon_1)))
# 
# taxons_df1 <- data.frame(taxon, NA)
# names(taxons_df1) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df1)) {
# 
#   if(((str_detect(taxons_df1[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df1[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df1[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df1[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df1[i, 1], "sp$") == TRUE){
# 
#     taxons_df1[i, 2] <- word(taxons_df1[i, 1], start = 1)
# 
#   } else {
#     taxons_df1[i, 2] <- as.character(taxons_df1[i, 1])
#   }
# }

# Set metadata

lat  <- -20.420476
lon  <- 57.732639
srid <- 4326

networks <- list(name               = "olesen_aigrettes",
                   date             = "1999-06-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Pollination networks of the Mauritian Ile aux Aigrettes",
                   public           = TRUE,
                   all_interactions = FALSE)

enviro1 <- list(name  = "attribute name",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1111-11-11",
                value = 0)

inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1999-06-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "field observations",
              description   = "null",
              public        = FALSE,
              lat           = lat,
              lon           = lon,
              srid          = srid)

setwd("importation_mangal/olesen")
taxon_df1 <- read.csv2("data/olesen_aigrettes_taxons.csv", header = TRUE)
olesen_aigrettes <- read.csv2("data/olesen_aigrettes_inter.csv", header = TRUE)

# POST table
# POST_environments(enviro1, attr1)
POST_networks(networks, enviro = enviro1)
POST_taxons(taxons_df1)
POST_interactions(olesen_aigrettes, enviro = enviro1, attr_inter)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df1, file = paste0(getwd(), "/data/olesen_aigrettes_taxons.csv"), row.names = FALSE)
# write.csv2(x = olesen_aigrettes, file = paste0(getwd(), "/data/olesen_aigrettes_inter.csv"), row.names = FALSE)



# #------------------------------
# # olesen 2
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(olesen_flores$sp_taxon_2)), as.vector(unique(olesen_flores$sp_taxon_1)))
# 
# taxons_df2 <- data.frame(taxon, NA)
# names(taxons_df2) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df2)) {
# 
#   if(((str_detect(taxons_df2[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df2[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df2[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df2[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df2[i, 1], "sp$") == TRUE){
# 
#     taxons_df2[i, 2] <- word(taxons_df2[i, 1], start = 1)
# 
#   } else {
#     taxons_df2[i, 2] <- as.character(taxons_df2[i, 1])
#   }
# }

# Set metadata

lat  <- 39.444798
lon  <- -31.193594
srid <- 4326

networks <- list(name             = "olesen_flores",
                 date             = "2000-07-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Pollination networks of the Azorean Flores island",
                 public           = TRUE,
                 all_interactions = FALSE)

enviro2 <- list(name  = "attribute name",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1111-11-11",
                value = 0)

inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2000-07-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "field observations",
              description   = "null",
              public        = FALSE,
              lat           = lat,
              lon           = lon,
              srid          = srid)

setwd("importation_mangal/olesen")
taxon_df2 <- read.csv2("data/olesen_flores_taxons.csv", header = TRUE)
olesen_flores <- read.csv2("data/olesen_flores_inter.csv", header = TRUE)

# POST table
# POST_environments(enviro2, attr1)
POST_networks(networks, enviro = enviro2)
POST_taxons(taxons_df2)
POST_interactions(olesen_flores, enviro = enviro2, attr_inter)

# Writing taxon and interaction table
# write.csv2(x = taxons_df2, file = paste0(getwd(), "/data/olesen_flores_taxons.csv"), row.names = FALSE)
# write.csv2(x = olesen_flores, file = paste0(getwd(), "/data/olesen_flores_inter.csv"), row.names = FALSE)

rm(taxon, lat, lon, srid, attr_inter, refs, users, enviro1, enviro2, datasets, traits, networks, inter, taxons_df1, taxons_df2, taxo_back_df, olesen_aigrettes, olesen_flores)
