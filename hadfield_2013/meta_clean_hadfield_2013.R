# Set libraries
library(reshape2)
library(tidyr)
library(jsonlite)
library(httr)
library(data.table)
library(taxize)
library(stringr)

library(mangal)

#------------------------------
  # Metadata
#------------------------------

srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "abundance of fleas on mammals",
                   table_owner = "interactions",
                   description = "abundance of flea species on mammal species",
                   unit        = "abundance")


ref <- list(doi        = "10.1086/674445",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "http://www.jstor.org/stable/10.1086/674445",
            data_url  = "https://datadryad.org/resource/doi:10.5061/dryad.jf3tj",
            author    = "hadfield",
            year      = "2014",
            bibtex    = "@article{doi:10.1086/674445,author = {Jarrod D. Hadfield and Boris R. Krasnov and Robert Poulin and Shinichi Nakagawa},title = {A Tale of Two Phylogenies: Comparative Analyses of Ecological Interactions.},journal = {The American Naturalist},volume = {183},number = {2},pages = {174-187},year = {2014},doi = {10.1086/674445},note ={PMID: 24464193},URL = {https://doi.org/10.1086/674445},eprint = { https://doi.org/10.1086/674445}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "attribute name",
               date  = "1111-11-11",
               value = 0)


dataset <- list(name        = "hadfield_2014",
                date        = "2014-02-01",
                description = "Flea distribution and abundance on small mammals (Soricomorpha and Rodentia) in 51 different regions of the Palearctic",
                public      = TRUE)


trait <- list(date = "1111-11-11")


# #------------------------------
#   # Cleaning matrix
# #------------------------------
# hadfield_2013 <- list()
# 
# for (i in 1:51) {
# 
#   # Open file
#   hadfield_2013[[i]] <- read.csv2(file = paste0("mangal-datasets/hadfield_2013/raw/A_HP_0", i, ".csv"), header = FALSE, sep = ",")
# 
#   # Cleaning for melt()
#   ## Get ROW one with Genus_species
#   x  <- as.character(unname(unlist(hadfield_2013[[i]][1, ])))
#   x[1] <- 1
#   colnames(hadfield_2013[[i]]) <- x
#   rm(x)
# 
#   ## Delete unused row
#   hadfield_2013[[i]] <- hadfield_2013[[i]][-1, -2]
# 
#   # Melt df
#   hadfield_2013[[i]] <- melt(hadfield_2013[[i]], id.vars = c(1), na.rm = TRUE)
# 
#   # Remove interaction value = 0 (no interaction)
#   names(hadfield_2013[[i]]) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   hadfield_2013[[i]] <- subset(hadfield_2013[[i]], hadfield_2013[[i]]$value != 0)
# 
#   print(head(hadfield_2013[[i]]))
# }
# 
# meta <- read.csv2(file = "mangal-datasets/hadfield_2013/raw/metadata.csv", header = TRUE, sep = ",")
# saveRDS(hadfield_2013, file = "mangal-datasets/hadfield_2013/data/hadfield_2013.Rdata")
# 
# 
# #------------------------------
# # Set taxa_back table
# #------------------------------
# 
# taxa_back <- vector()
# 
# for (j in 1:51) {
# 
#   ## Get Unique taxa of data
#   taxa <- c(as.character(hadfield_2013[[j]][, "sp_taxon_1"]), 
#             as.character(hadfield_2013[[j]][, "sp_taxon_2"]))
# 
#   ### Remove sp
# 
#   for (i in 1:length(taxa)) {
# 
#     if(((str_detect(taxa[i], "[:digit:]") == TRUE || str_detect(taxa[i], "[:punct:]") == TRUE) == TRUE &
#          str_detect(taxa[i], "sp") == TRUE) ||
#          str_detect(taxa[i], "indet\\.") == TRUE ||
#          str_detect(taxa[i], "sp$") == TRUE ||
#          str_detect(taxa[i], "\\?") == TRUE ||
#          str_detect(taxa[i], "nr\\.") == TRUE){
#     
#       taxa[i] <- word(taxa[i], start = 1)
# 
#     } else {
#       taxa[i] <- taxa[i]
#     }
#   }
#   taxa_back <- c(taxa_back, taxa)
# }
# 
# taxa_back <- unique(taxa_back)
# 
# 
# ## Select only taxa not yet in db
# 
# server <- "http://poisotlab.biol.umontreal.ca"
# 
# taxa_back_df <- data.frame()
# 
# for (i in 1:length(taxa_back)) {
# 
#   path <- modify_url(server, path = paste0("/api/v2/","taxa_back/?name=", str_replace(taxa_back[i], " ", "%20")))
# 
#   if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json", 
#                                                           "Authorization" = paste("bearer", readRDS("mangal-datasets/.httr-oauth")))))) == 0) {
# 
#     taxa_back_df[nrow(taxa_back_df)+1, 1] <- taxa_back[i]
#   }
# }
# 
# rm(taxa_back)
# names(taxa_back_df) <- c("name")
# 
# ## Get code by species
# taxa_back_df[, "bold"] <- NA
# taxa_back_df[, "eol"]  <- NA
# taxa_back_df[, "tsn"]  <- NA
# taxa_back_df[, "ncbi"] <- NA
# 
# ### Encore probleme d"identification avec les api... ###
# 
# for (i in 1:nrow(taxa_back_df)) {
#   try (expr = (taxa_back_df[i, 2] <- get_boldid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
#   try (expr = (taxa_back_df[i, 3] <- get_eolid(taxa_back_df[i, 1], row = 5, verbose = FALSE, key = 110258)[1]), silent = TRUE)
#   try (expr = (taxa_back_df[i, 4] <- get_tsn(taxa_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
#   try (expr = (taxa_back_df[i, 5] <- get_uid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
# }
# 
# # Writing taxa_back_df
# write.csv2(x = taxa_back_df, file = "mangal-datasets/hadfield_2013/data/hadfield_2013_taxa_back.csv", row.names = FALSE)
# 
# 
# #------------------------------
# # Set taxa table
# #------------------------------
# taxa_df <- list()
# 
# for (j in 1:51) {
#   
#   ## Get Unique taxa of data
#   taxa <- c(as.character(hadfield_2013[[j]][, "sp_taxon_1"]), 
#                  as.character(hadfield_2013[[j]][, "sp_taxon_2"]))
# 
# 
#   taxa <- unique(taxa)
# 
#   taxa_df[[j]] <- as.data.frame(taxa)
#   taxa_df[[j]]["name_clear"] <- NA
#   names(taxa_df[[j]]) <- c("original_name", "name_clear")
# 
#   for (i in 1:nrow(taxa_df[[j]])) {
#   
#     if(((str_detect(taxa_df[[j]][i, 1], "[:digit:]") == TRUE || str_detect(taxa_df[[j]][i, 1], "[:punct:]") == TRUE) &
#          str_detect(taxa_df[[j]][i, 1], "sp") == TRUE) ||
#          str_detect(taxa_df[[j]][i, 1], "indet\\.") == TRUE ||
#          str_detect(taxa_df[[j]][i, 1], "\\.") == TRUE ||
#          str_detect(taxa_df[[j]][i, 1], "sp$") == TRUE){
#     
#       taxa_df[[j]][i, 2] <- word(taxa_df[[j]][i, 1], start = 1)
#     
#     } else {
#       taxa_df[[j]][i, 2] <- as.character(taxa_df[[j]][i, 1])
#     }
#   }
# }
# 
# # Writing taxa_df
# saveRDS(taxa_df, file = "mangal-datasets/hadfield_2013/data/hadfield_2013_taxa_df.Rdata")
# 

#------------------------------
# Open dataframes
#------------------------------

hadfield_2013 <- readRDS("mangal-datasets/hadfield_2013/data/hadfield_2013.Rdata")
taxa_back_df <- read.csv2("mangal-datasets/hadfield_2013/data/hadfield_2013_taxa_back.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
taxa_df <- readRDS("mangal-datasets/hadfield_2013/data/hadfield_2013_taxa_df.Rdata")


#------------------------------
  # POST commun table
#------------------------------
POST_attribute(attr_inter)

POST_ref(ref = ref)

POST_user(users = users)

POST_dataset(dataset = dataset, users = users, ref = ref)

POST_taxa_back(taxa_back = taxa_back_df)


#------------------------------
 # Injection loop : Network by network
#------------------------------
for (i in 1:51) {
  
  
  network <- list(name             = paste("hadfield_2014", meta[i, 1]),
                  date             = "2014-02-01",
                  lat              = as.numeric(meta[i, 2]),
                  lon              = as.numeric(meta[i, 3]),
                  srid             = srid,
                  description      = paste("Flea distribution and abundance on small mammals (Soricomorpha and Rodentia) at", meta[i, 1]),
                  public           = TRUE,
                  all_interactions = FALSE)

  inter <- list(taxon_1_level = "taxon",
                taxon_2_level = "taxon",
                date          = "2014-02-01",
                direction     = "directed",
                type          = "parasitism",
                method        = "field observations",
                description   = "null",
                public        = TRUE,
                lat           = as.numeric(meta[i, 2]),
                lon           = as.numeric(meta[i, 3]),
                srid          = srid)
  
  POST_network(network_lst = network, enviro = enviro, dataset = dataset, users = users)
  
  POST_taxon(taxa_df = taxa_df[[i]])
  
  POST_interaction(inter_df = hadfield_2013[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users)
}

rm(list = ls())
