# # Set libraries
# library(reshape2)
# library(tidyr)
# library(jsonlite)
# library(httr)
# library(data.table)
# library(rcrossref)
# library(taxize)
# library(stringr)
# library(httr)
# 
# library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- 47.000000
lon  <- 72.232323
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name   = "Pollinisator recorded on a flower",
              table_owner = "interactions",
              description = "Presence or absence of interaction",
              unit        = "NA")

# Exemple to test traits table
# attr1 <- list(name       = "pistil",
#              table_owner = "traits",
#              description = "pistil length",
#              unit        = "cm")
# 
# attr2 <- list(name        = "etamine",
#               table_owner = "traits",
#               description = "etamine length",
#               unit        = "cm")
# 
# attr3 <- list(name        = "petale",
#               table_owner = "traits",
#               description = "petale length",
#               unit        = "cm")
# 
# attr4 <- list(name        = "sepale",
#               table_owner = "traits",
#               description = "setale length",
#               unit        = "cm")

refs <- list(doi       = "0.2307/3565022",
             jstor     = "NA",
             pmid      = "NA",
             author    = "hocking",
             year      = "1968",
             paper_url = "http://www.jstor.org/stable/3565022",
             data_url  = "http://www.web-of-life.es/map.php?type=5",
             bibtex    = "NA")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1963-06-01",
               value = 0)


datasets <- list(name        = "Howking_1968",
                 date        = "1963-06-01",
                 description = "Insect activity recorded on flower at Lake Hazen, Ellesmere Island, N.W.T., Canada",
                 public      = TRUE)


traits <- list(date = "1111-11-11")


networks <- list(name             = "Howking_1968",
                 date             = "1968-06-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Insect activity recorded on flower at Lake Hazen, Ellesmere Island, N.W.T., Canada",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "individual",
              taxon_2_level = "individual",
              date          = "1968-06-01",
              direction     = "directed",
              type          = "unknown",
              method        = "Field observations",
              description   = "Visit of an insect to a flower",
              public        = FALSE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Set WD
# setwd("importation_mangal/Test_Hocking_1968")
# 
# # Open file
# hocking_1968 <- read.csv2(file = "raw/hocking_1968.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
# # Cleaning for melt()
# ## Merge two first COLUMNS Genus species
# hocking_1968[is.na(hocking_1968)] <- "sp."
# hocking_1968 <- unite(hocking_1968, sp1, c(V1, V2), sep = " ", remove = TRUE)
# 
# ## Get ROW one with Genus_species
# x  <- paste(hocking_1968[1, ], sep =" ", hocking_1968[2, ])
# x[1] <- "species"
# colnames(hocking_1968) <- x
# rm(x)
# 
# ## Delete unused row
# hocking_1968 <- hocking_1968[-c(1, 2), ]
# 
# # Melt df
# hocking_1968 <- melt(hocking_1968, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(hocking_1968) <- c("sp_taxon_1", "sp_taxon_2", "value")
# hocking_1968 <- subset(hocking_1968, hocking_1968$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon tables
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(hocking_1968$sp_taxon_2)), as.vector(unique(hocking_1968$sp_taxon_1)))
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
# # 
# # traits_df <- read.csv2(file = "data/hocking_1968_traits.csv", header = TRUE)
# # 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/hocking_1968_taxo_back.csv"), row.names = FALSE)
# write.csv2(x = hocking_1968, file = paste0(getwd(), "/data/hocking_1968_inter.csv"), row.names = FALSE)
# write.csv2(x = taxons_df, file = paste0(getwd(), "/data/hocking_1968_taxons.csv"), row.names = FALSE)
# # write.csv2(x = traits_df, file = paste0(getwd(), "/data/hocking_1968_traits.csv"), row.names = FALSE)

setwd("importation_mangal/Test_Hocking_1968")
taxo_back_df <- read.csv2("data/hocking_1968_taxo_back.csv", header = TRUE)
hocking_1968 <- read.csv2("data/hocking_1968_inter.csv", header = TRUE)
taxons_df <- read.csv2("data/hocking_1968_taxons.csv", header = TRUE)
# traits_df <- read.csv2("data/hocking_1968_traits.csv", header = TRUE)

#------------------------------
  # Throwing injection functions
#------------------------------
POST_attributes(attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
# POST_attributes(attr3)
# POST_attributes(attr4)
POST_refs()
POST_users()
# POST_environments(enviro, attr_inter)
POST_datasets()
POST_networks(networks, enviro = enviro)
POST_taxo_back()
POST_taxons(taxons_df)
# POST_traits(traits_df)
POST_interactions(hocking_1968, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, hocking_1968)
