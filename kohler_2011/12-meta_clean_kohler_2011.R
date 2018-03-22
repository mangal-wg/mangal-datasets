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

lat  <- 0
lon  <- 0
srid <- 4326
  
# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "frequency of interaction",
                   table_owner = "interactions",
                   description = "frequency of interactions between pairs of species",
                   unit        = "NA")


attr1 <- list(name        = "mean altitude",
              table_owner = "interactions",
              description = "mean altitude of the plot studied",
              unit        = "meters")


refs <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "null",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/html/kohler2011.html",
             author    = "kohler",
             year      = "2011",
             bibtex    = "NA")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


datasets <- list(name        = "kohler_2011",
                 date        = "2011-01-01",
                 description = "Hummingbirds-flowers interactions in an altitudinal gradient in the Brazilian Atlantic Rainforest",
                 public      = TRUE)


traits <- list(date = "2011-01-01")


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2011-01-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "unknown",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



#------------------------------
  # Cleaning matrix
#------------------------------

# # Set WD
# setwd("importation_mangal/kohler_2011")
# 
#   # Open file
#   kohler_2011_350_600_m <- read.csv2(file = paste0("raw/kohler_2011_350_600_m.csv"), header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
#   # Cleaning for melt()
#   ## Get ROW one with Genus_species
#   x  <- kohler_2011_350_600_m[1, ]
#   x[1] <- "species"
#   colnames(kohler_2011_350_600_m) <- x
#   rm(x)
# 
#   ## Delete unused row
#   kohler_2011_350_600_m <- kohler_2011_350_600_m[-1, ]
# 
#   # Melt df
#   kohler_2011_350_600_m <- melt(kohler_2011_350_600_m, id.vars = c("species"), na.rm = TRUE)
# 
#   # Retirer les 0 et ajouter dans la table network edge_list = FALSE
#   names(kohler_2011_350_600_m) <- c("sp_taxon_1", "sp_taxon_2", "value")
# 
#   # Remove interaction value = 0 (no interaction)
#   names(kohler_2011_350_600_m) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   kohler_2011_350_600_m <- subset(kohler_2011_350_600_m, kohler_2011_350_600_m$value != 0)
# 
# # kohler_2011_600_850_m
# 
#   kohler_2011_600_850_m <- read.csv2(file = "raw/kohler_2011_600_850_m.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
#   x  <- kohler_2011_600_850_m[1, ]
#   x[1] <- "species"
#   colnames(kohler_2011_600_850_m) <- x
#   rm(x)
#   kohler_2011_600_850_m <- kohler_2011_600_850_m[-1, ]
#   kohler_2011_600_850_m <- melt(kohler_2011_600_850_m, id.vars = c("species"), na.rm = TRUE)
#   names(kohler_2011_600_850_m) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   kohler_2011_600_850_m <- subset(kohler_2011_600_850_m, kohler_2011_600_850_m$value != 0)
# 
# # kohler_2011_850_1100_m
# 
#   kohler_2011_850_1100_m <- read.csv2(file = "raw/kohler_2011_850_1100_m.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
#   x  <- kohler_2011_850_1100_m[1, ]
#   x[1] <- "species"
#   colnames(kohler_2011_850_1100_m) <- x
#   rm(x)
#   kohler_2011_850_1100_m <- kohler_2011_850_1100_m[-1, ]
#   kohler_2011_850_1100_m <- melt(kohler_2011_850_1100_m, id.vars = c("species"), na.rm = TRUE)
#   names(kohler_2011_850_1100_m) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   kohler_2011_850_1100_m <- subset(kohler_2011_850_1100_m, kohler_2011_850_1100_m$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- unique(c(as.vector(unique(kohler_2011_350_600_m$sp_taxon_2)), as.vector(unique(kohler_2011_350_600_m$sp_taxon_1)), 
#                   as.vector(unique(kohler_2011_600_850_m$sp_taxon_2)), as.vector(unique(kohler_2011_600_850_m$sp_taxon_1)),
#                   as.vector(unique(kohler_2011_850_1100_m$sp_taxon_2)), as.vector(unique(kohler_2011_850_1100_m$sp_taxon_1))))
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
# # Writing taxo_back_df
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/arroyo_taxo_back.csv"), row.names = FALSE)

setwd("importation_mangal/arroyo")
taxo_back_df <- read.csv2("data/arroyo_taxo_back.csv", header = TRUE)

#------------------------------
  # POST commun table
#------------------------------
POST_attributes(attr_inter)

POST_attributes(attr1)

POST_refs()

POST_users()

POST_datasets()

POST_taxo_back()

# POST_traits(traits_df)

# # ------------------------------
# # Arroyo 1
# # ------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(kohler_2011_350_600_m$sp_taxon_2)), as.vector(unique(kohler_2011_350_600_m$sp_taxon_1)))
# 
# taxons_df1 <- data.frame(taxon, NA)
# names(taxons_df1) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df1)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){
#     
#     taxons_df1[i, 2] <- word(taxons_df1[i, 1], start = 1)
#     
#   } else {
#     taxons_df1[i, 2] <- as.character(taxons_df1[i, 1])
#   }
# }

# Set metadata
networks <- list(name               = "kohler_2011_350_600_m",
                   date             = "2011-01-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Hummingbirds-flowers interactions in an altitudinal gradient in the Brazilian Atlantic Rainforest, 350 to 600 m",
                   public           = TRUE,
                   all_interactions = FALSE)

enviro1 <- list(name  = "mean altitude",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "2011-01-01",
                value = 475)

setwd("importation_mangal/arroyo")
taxon_df1 <- read.csv2("data/kohler_2011_350_600_m_taxons.csv", header = TRUE)
kohler_2011_350_600_m <- read.csv2("data/kohler_2011_350_600_m_inter.csv", header = TRUE)

# POST table
POST_environments(enviro1, attr1)
POST_networks(networks, enviro = enviro1)
POST_taxons(taxons_df1)
POST_interactions(kohler_2011_350_600_m, enviro1, attr = attr_inter)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df1, file = paste0(getwd(), "/data/kohler_2011_350_600_m_taxons.csv"), row.names = FALSE)
# write.csv2(x = kohler_2011_350_600_m, file = paste0(getwd(), "/data/kohler_2011_350_600_m_inter.csv"), row.names = FALSE)



# #------------------------------
# # Arroyo 2
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(kohler_2011_600_850_m$sp_taxon_2)), as.vector(unique(kohler_2011_600_850_m$sp_taxon_1)))
# 
# taxons_df2 <- data.frame(taxon, NA)
# names(taxons_df2) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df2)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){  
#     
#     taxons_df2[i, 2] <- word(taxons_df2[i, 1], start = 1)
#     
#   } else {
#     taxons_df2[i, 2] <- as.character(taxons_df2[i, 1])
#   }
# }

# Set metadata
networks <- list(name             = "kohler_2011_600_850_m",
                 date             = "2011-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Hummingbirds-flowers interactions in an altitudinal gradient in the Brazilian Atlantic Rainforest, 600 to 850 m",
                 public           = TRUE,
                 all_interactions = FALSE)

enviro2 <- list(name  = "mean altitude",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "2011-01-01",
                value = 725)

setwd("importation_mangal/arroyo")
taxon_df2 <- read.csv2("data/kohler_2011_600_850_m_taxons.csv", header = TRUE)
kohler_2011_600_850_m <- read.csv2("data/kohler_2011_600_850_m_inter.csv", header = TRUE)

# POST table
POST_environments(enviro2, attr1)
POST_networks(networks, enviro2)
POST_taxons(taxons_df2)
POST_interactions(kohler_2011_600_850_m, enviro2, attr = attr_inter)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df2, file = paste0(getwd(), "/data/kohler_2011_600_850_m_taxons.csv"), row.names = FALSE)
# write.csv2(x = kohler_2011_600_850_m, file = paste0(getwd(), "/data/kohler_2011_600_850_m_inter.csv"), row.names = FALSE)


# #------------------------------
# # Arroyo 3
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(kohler_2011_850_1100_m$sp_taxon_2)), as.vector(unique(kohler_2011_850_1100_m$sp_taxon_1)))
# 
# taxons_df3 <- data.frame(taxon, NA)
# names(taxons_df3) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df3)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){  
#     
#     taxons_df3[i, 2] <- word(taxons_df3[i, 1], start = 1)
#     
#   } else {
#     taxons_df3[i, 2] <- as.character(taxons_df3[i, 1])
#   }
# }

# Set metadata
networks <- list(name             = "kohler_2011_850_1100_m",
                 date             = "2011-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Hummingbirds-flowers interactions in an altitudinal gradient in the Brazilian Atlantic Rainforest, 850 to 1100 m",
                 public           = TRUE,
                 all_interactions = FALSE)

enviro3 <- list(name  = "mean altitude",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "2011-01-01",
                value = 975)

setwd("importation_mangal/arroyo")
taxon_df3 <- read.csv2("data/kohler_2011_850_1100_m_taxons.csv", header = TRUE)
kohler_2011_850_1100_m <- read.csv2("data/kohler_2011_850_1100_m_inter.csv", header = TRUE)

# POST table
POST_environments(enviro3, attr1)
POST_networks(networks, enviro3)
POST_taxons(taxons_df3)
POST_interactions(kohler_2011_850_1100_m, enviro3, attr = attr_inter)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df3, file = paste0(getwd(), "/data/kohler_2011_850_1100_m_taxons.csv"), row.names = FALSE)
# write.csv2(x = kohler_2011_850_1100_m, file = paste0(getwd(), "/data/kohler_2011_850_1100_m_inter.csv"), row.names = FALSE)

rm(taxon, lat, lon, srid, attr_inter, attr1, refs, users, enviro1, enviro2, enviro3, datasets, traits, networks, inter, taxons_df1, taxons_df2, taxons_df3, taxo_back_df, kohler_2011_350_600_m, kohler_2011_600_850_m, kohler_2011_850_1100_m)
