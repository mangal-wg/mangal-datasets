# # Set libraries
library(reshape2)
library(tidyr)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)
# 
# library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- -24.2653
lon  <- -48.4069
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name  = "presence/absence",
             table_owner = "interaction",
             description = "Presence or absence of a recorded interaction",
             unit        = "NA")


ref <-  list(doi       = "10.5962/bhl.title.11538",
             jstor     = "NA",
             pmid      = "NA",
             author    = "roberson",
             year      = "1929",
             paper_url = "https://www.biodiversitylibrary.org/bibliography/11538#/summary",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/html/robertson_1929.html",
             bibtex    = "@book{bhl43820, title = {Flowers and insects; lists of visitors of four hundred and fifty-three flowers,  }, copyright = {Public domain.  Published 1923-1963 with notice but no evidence of copyright renewal found in Stanford Copyright Renewal Database.  Contact dcc@library.uiuc.edu for information.}, url = {https://www.biodiversitylibrary.org/item/43820}, publisher = {Carlinville, Ill.,n.p.}, author = {Robertson, Charles,}, year = {}, pages = {234}, keywords = {Bees|Fertilization of plants|Flowers|Illinois|Insects|Macoupin County|}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "NAME",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1899-07-01",
               value = 0)


dataset <- list(name        = "Roberson_1929",
                 date        = "1899-07-01",
                 description = "Insects observed pollinating flowers, ten miles of Carlinville, Illinois, USA",
                 public      = TRUE)


trait <- list(date = "1899-07-01")


network <- list(name             = "Roberson_1929",
                 date             = "1899-07-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Insects observed pollinating flowers, ten miles of Carlinville, Illinois, USA",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "1899-07-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observations",
              description   = "Visit of an insect to a flower",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Open file
# data <- read.table(file = "importation_mangal/roberson_1929/raw/roberson_1929_data.txt", header = FALSE, sep = " ")
# 
# # Set colum's and row's name -> other .txt file
# ## Add row names
# data[, ncol(data)+1] <- as.vector((read.table(file = "raw/row_sp.txt"))[,1])
# 
# ## Add col names
# col_sp <- read.table(file = "raw/col_sp.txt", header = FALSE, sep = " ")
# col_sp <- c(as.vector(col_sp[,1]), "species")
# colnames(data) <- col_sp
# 
# ## Order df
# roberson_1929 <- data[, c(457, 1:456)]
# 
# # Drop
# rm(list = c('data', 'col_sp'))
# 
# # Melt df
# roberson_1929 <- melt(roberson_1929, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(roberson_1929) <- c("sp_taxon_1", "sp_taxon_2", "value")
# roberson_1929 <- subset(roberson_1929, roberson_1929$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(roberson_1929$sp_taxon_2)), as.vector(unique(roberson_1929$sp_taxon_1)))
# 
# 
# ### Check for spelling mistakes... ###
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
#   try (expr = (taxo_back_df[i, 4]<- get_tsn(taxo_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
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
# # traits_df <- read.csv2(file = "importation_mangal/roberson_1929/data/hocking_1968_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxons_df, file = "importation_mangal/roberson_1929/data/roberson_1929_taxons.csv", row.names = FALSE)
# write.csv2(x = roberson_1929, file = "importation_mangal/roberson_1929/data/roberson_1929_inter.csv", row.names = FALSE)
# write.csv2(x = taxo_back_df, file = "importation_mangal/roberson_1929/data/roberson_1929_taxo_back.csv", row.names = FALSE)
# # write.csv2(x = traits_df, file = "importation_mangal/roberson_1929/data/roberson_1929_traits.csv", row.names = FALSE)

taxo_back_df <- read.csv2("importation_mangal/roberson_1929/data/roberson_1929_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("importation_mangal/roberson_1929/data/roberson_1929_taxons.csv", header = TRUE)
roberson_1929 <- read.csv2("importation_mangal/roberson_1929/data/roberson_1929_inter.csv", header = TRUE)
# traits_df <- read.csv2("importation_mangal/roberson_1929/data/roberson_1929_traits.csv", header = TRUE)

#------------------------------
  # Throwing injection functions
#------------------------------
POST_attribute(attr = attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
POST_ref(ref = ref)
POST_users(users = users)
# POST_environment(enviro, attr_##)
POST_dataset()
POST_network(network, enviro = enviro)
POST_taxonomy()
POST_node(taxons_df)
# POST_traits(trait_df)
POST_interaction(roberson_1929, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, roberson_1929)