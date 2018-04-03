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

lat  <- -8.505
lon  <- -37.201389
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "number of bee visits to flowers",
                   table_owner = "interactions",
                   description = "number of bee visits to flowers",
                   unit        = "visits")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1111/j.1365-2656.2009.01567.x",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2009.01567.x/pdf",
             data_url  = "http://www.web-of-life.es/map.php?type=5",
             author    = "bezerra",
             year      = "2009",
             bibtex    = "@article{Bezerra_2009, doi = {10.1111/j.1365-2656.2009.01567.x}, url = {https://doi.org/10.1111%2Fj.1365-2656.2009.01567.x}, year = 2009, month = {jul}, publisher = {Wiley-Blackwell}, volume = {78}, number = {5}, pages = {1096--1101},author = {Elisangela L.S. Bezerra and Isabel C. Machado and Marco A. R. Mello}, title = {Pollination networks of oil-flowers: a tiny world within the smallest of all worlds}, journal = {Journal of Animal Ecology}}")


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


datasets <- list(name        = "bezerra_2009",
                 date        = "2006-12-01",
                 description = "oil-flowers (Malpighiaceae) and their bee visitors from a Brazilian steppe, Parque Nacional do Catimbau, in the municipality of Buique (PE), northeastern Brazil",
                 public      = TRUE)


traits <- list(date = "2006-12-01")


networks <- list(name             = "bezerra_2009",
                 date             = "2006-12-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "oil-flowers (Malpighiaceae) and their bee visitors from a Brazilian steppe, Parque Nacional do Catimbau, in the municipality of Buique (PE), northeastern Brazil",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2006-12-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "field observation",
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
# bezerra_2009 <- read.csv2(file = "importation_mangal/bezerra_2009/raw/bezerra_2009.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# x  <- bezerra_2009[1, ]
# x[1] <- "species"
# colnames(bezerra_2009) <- x
# rm(x)
# 
# ## Delete unused row
# bezerra_2009 <- bezerra_2009[-1, ]
# 
# # Melt df
# bezerra_2009 <- melt(bezerra_2009, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(bezerra_2009) <- c("sp_taxon_1", "sp_taxon_2", "value")
# bezerra_2009 <- subset(bezerra_2009, bezerra_2009$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(bezerra_2009$sp_taxon_2)), as.vector(unique(bezerra_2009$sp_taxon_1)))
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
# # traits_df <- read.csv2(file = "importation_mangal/bezerra_2009/data/bezerra_2009_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = "importation_mangal/bezerra_2009/data/bezerra_2009_taxo_back.csv", row.names = FALSE)
# write.csv2(x = taxons_df, file = "importation_mangal/bezerra_2009/data/bezerra_2009_taxons.csv", row.names = FALSE)
# write.csv2(x = bezerra_2009, file = "importation_mangal/bezerra_2009/data/bezerra_2009_inter.csv", row.names = FALSE)
# # write.csv2(x = traits_df, file = "importation_mangal/bezerra_2009/data/bezerra_2009_traits.csv", row.names = FALSE)

taxo_back_df <- read.csv2("importation_mangal/bezerra_2009/data/bezerra_2009_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("importation_mangal/bezerra_2009/data/bezerra_2009_taxons.csv", header = TRUE)
bezerra_2009 <- read.csv2("importation_mangal/bezerra_2009/data/bezerra_2009_inter.csv", header = TRUE)
# traits_df <- read.csv2("importation_mangal/bezerra_2009/data/bezerra_2009_traits.csv", header = TRUE)

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
POST_interactions(bezerra_2009, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, bezerra_2009)
