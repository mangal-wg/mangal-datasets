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

lat  <- 35.5833
lon  <- 138.3833
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Number of visit by a pollinator",
                   table_owner = "interactions",
                   description = "Number of individual of a species observed/caught on a flower",
                   unit        = "Number of individual")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

refs <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/156107/1/cbl02802_119.pdf",
             data_url  = "http://www.web-of-life.es/map.php?type=5",
             author    = "kato",
             year      = "1993",
             bibtex    = "null")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "attribute name",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1111-11-11",
               value = 0)


datasets <- list(name        = "kato_1993",
                 date        = "1991-09-01",
                 description = "Flower and anthophilous insect interactions in the primary cool-temperate subalpine forests and meadows at Mt. Kushigata, Yamanashi Prefecture, Japan",
                 public      = TRUE)


traits <- list(date = "1111-11-11")


networks <- list(name             = "kato_1993",
                 date             = "1991-09-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description = "Flower and anthophilous insect interactions in the primary cool-temperate subalpine forests and meadows at Mt. Kushigata, Yamanashi Prefecture, Japan",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1991-09-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observations/captures",
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
# setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/kato_1993")
# 
# # Open file
# kato_1993 <- read.csv2(file = "raw/kato_1993.csv", header = FALSE, sep = ",")
# 
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# x  <- unname(unlist(kato_1993[1, ]))
# x[1] <- 1
# colnames(kato_1993) <- unlist(x)
# rm(x)
# 
# ## Delete unused row
# kato_1993 <- kato_1993[-1, ]
# 
# # Melt df
# kato_1993 <- melt(kato_1993, id.vars = c(1), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(kato_1993) <- c("sp_taxon_1", "sp_taxon_2", "value")
# kato_1993 <- subset(kato_1993, kato_1993$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(kato_1993$sp_taxon_2)), as.vector(unique(kato_1993$sp_taxon_1)))
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
#       str_detect(taxons_df[i, 1], "sp$") == TRUE){
# 
#     taxons_df[i, 2] <- word(taxons_df[i, 1], start = 1)
# 
#   } else {
#     taxons_df[i, 2] <- as.character(taxons_df[i, 1])
#   }
# }

#------------------------------
# Set traits table
#------------------------------

# traits_df <- read.csv2(file = "data/kato_1993_traits.csv", header = TRUE)

# traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# names(traits_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxon and interaction table
#------------------------------

# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/kato_1993_taxo_back.csv"), row.names = FALSE)
# write.csv2(x = taxons_df, file = paste0(getwd(), "/data/kato_1993_taxons.csv"), row.names = FALSE)
# write.csv2(x = kato_1993, file = paste0(getwd(), "/data/kato_1993_inter.csv"), row.names = FALSE)
# write.csv2(x = traits_df, file = paste0(getwd(), "/data/kato_1993_traits.csv"), row.names = FALSE)

setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/kato_1993")
taxo_back_df <- read.csv2("data/kato_1993_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("data/kato_1993_taxons.csv", header = TRUE)
kato_1993 <- read.csv2("data/kato_1993_inter.csv", header = TRUE)
# traits_df <- read.csv2("data/kato_1993_traits.csv", header = TRUE)

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
POST_interactions(kato_1993, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, kato_1993)
