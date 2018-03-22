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

lat  <- 75
lon  <- -114.966667
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name  = "Number of pollinator",
             table_owner = "interactions",
             description = "Number of insects captured while pollination",
             unit        = "individual captured")


refs <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://www.biodiversitylibrary.org/pdf4/075601100090098.pdf",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/plant_pollinator/excel/mosquin_1967.xls",
             author    = "Mosquin",
             year      = "1967",
             bibtex    = "@article{article, author = {Mosquin, Theodore and Martin, J. E. H.}, year = {1967}, pages = {201-205}, title = {Observations on the pollination biology of plants on Melville Island, N.W.T., Canada}, journal = {Canadian Field Naturalist}, volume = {81}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1965-06-31",
               value = 0)


datasets <- list(name        = "Mosquin_Martin_1967",
                 date        = "1965-07-31",
                 description = "Occurence of flower-visiting insect on plant species, two miles north of Bailey Point, Melville Island, N.W.T., Canada",
                 public      = TRUE)


traits <- list(date = "1965-06-31")


networks <- list(name             = "Mosquin_Martin_1967",
                 date             = "1965-07-31",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Occurence of flower-visiting insect on plant species, two miles north of Bailey Point, Melville Island, N.W.T., Canada",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1965-07-31",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation/capture",
              description   = "Number of pollinator capture while pollination",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Set WD
# setwd("importation_mangal/Mosquin_Martin_1967")
# 
# # Open file
# Mosquin_Martin_1967 <- read.csv2(file = "raw/Mosquin_Martin_1967.csv", header = FALSE, sep = ",")
# 
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# x  <- unname(unlist(Mosquin_Martin_1967[1, ]))
# x[1] <- 1
# colnames(Mosquin_Martin_1967) <- unlist(x)
# rm(x)
# 
# ## Delete unused row
# Mosquin_Martin_1967 <- Mosquin_Martin_1967[-1, ]
# 
# # Melt df
# Mosquin_Martin_1967 <- melt(Mosquin_Martin_1967, id.vars = c(1), na.rm = TRUE)
# 
# ## Remove interaction value = 0 (no interaction)
# names(Mosquin_Martin_1967) <- c("sp_taxon_1", "sp_taxon_2", "value")
# Mosquin_Martin_1967 <- subset(Mosquin_Martin_1967, Mosquin_Martin_1967$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(Mosquin_Martin_1967$sp_taxon_2)), as.vector(unique(Mosquin_Martin_1967$sp_taxon_1)))
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
# # traits_df <- read.csv2(file = "data/Mosquin_Martin_1967_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/Mosquin_Martin_1967_taxo_back.csv"), row.names = FALSE)
# write.csv2(x = taxons_df, file = paste0(getwd(), "/data/Mosquin_Martin_1967_taxons.csv"), row.names = FALSE)
# write.csv2(x = Mosquin_Martin_1967, file = paste0(getwd(), "/data/Mosquin_Martin_1967_inter.csv"), row.names = FALSE)
# # write.csv2(x = traits_df, file = paste0(getwd(), "/data/Mosquin_Martin_1967_traits.csv"), row.names = FALSE)

setwd("importation_mangal/Mosquin_Martin_1967")
taxo_back_df <- read.csv2("data/Mosquin_Martin_1967_taxo_back.csv", header = TRUE)
Mosquin_Martin_1967 <- read.csv2("data/Mosquin_Martin_1967_inter.csv", header = TRUE)
taxons_df <- read.csv2("data/Mosquin_Martin_1967_taxons.csv", header = TRUE)
# traits_df <- read.csv2("data/Mosquin_Martin_1967_traits.csv", header = TRUE)

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
POST_interactions(Mosquin_Martin_1967, enviro = enviro, attr = attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, Mosquin_Martin_1967)
