# Set libraries
library(reshape2)
library(tidyr)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)

library("readxl")

library(mangal)

#------------------------------
  # Metadata
#------------------------------

srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "presence/absence",
                   table_owner = "interactions",
                   description = "Presence or absence of an interaction",
                   unit        = "NA")


ref <- list(doi       = "10.1007/s10641-010-9606-0",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://link.springer.com/article/10.1007%2Fs10641-010-9606-0",
            data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/anemone_fish/ricciardi-et-al-2010.xls",
            author    = "ricciardi",
            year      = "2010",
            bibtex    = "@article{Ricciardi_2010,doi = {10.1007/s10641-010-9606-0},url = {https://doi.org/10.1007%2Fs10641-010-9606-0},year = 2010,month = {feb},publisher = {Springer Nature},volume = {87},number = {4},pages = {333--347},author = {Francesco Ricciardi and Massimo Boyer and Jeff Ollerton},title = {Assemblage and interaction structure of the anemonefish-anemone mutualism across the Manado region of Sulawesi, Indonesia},journal = {Environmental Biology of Fishes}}")


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


dataset <- list(name         = "ricciardi_2010",
                 date        = "2006-07-01",
                 description = "structure of local anemonefish-anemone networks across the Manado region of Sulawesi, Indonesia",
                 public      = TRUE)


trait <- list(date = "1111-11-11")


# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
# # Open file
# path = "mangal-datasets/ricciardi_2010/raw/ricciardi-et-al-2010.xls"
# ricciardi_2010 <- list()
# 
# ricciardi_2010[[1]] <- as.data.frame(read_excel(path, range = "B14:G20"))
# ricciardi_2010[[2]] <- as.data.frame(read_excel(path, range = "B22:F24"))
# ricciardi_2010[[3]] <- as.data.frame(read_excel(path, range = "B26:D29"))
# ricciardi_2010[[4]] <- as.data.frame(read_excel(path, range = "B31:G35"))
# ricciardi_2010[[5]] <- as.data.frame(read_excel(path, range = "B37:F40"))
# ricciardi_2010[[6]] <- as.data.frame(read_excel(path, range = "B42:F46"))
# ricciardi_2010[[7]] <- as.data.frame(read_excel(path, range = "B48:E50"))
# ricciardi_2010[[8]] <- as.data.frame(read_excel(path, range = "B52:E57"))
# ricciardi_2010[[9]] <- as.data.frame(read_excel(path, range = "B59:F63"))
# ricciardi_2010[[10]] <- as.data.frame(read_excel(path, range = "B65:E69"))
# ricciardi_2010[[11]] <- as.data.frame(read_excel(path, range = "B71:F75"))
# ricciardi_2010[[12]] <- as.data.frame(read_excel(path, range = "B77:F81"))
# ricciardi_2010[[13]] <- as.data.frame(read_excel(path, range = "B83:F87"))
# ricciardi_2010[[14]] <- as.data.frame(read_excel(path, range = "B89:G94"))
# ricciardi_2010[[15]] <- as.data.frame(read_excel(path, range = "B96:G100"))
# ricciardi_2010[[16]] <- as.data.frame(read_excel(path, range = "B102:E106"))
# 
# for (i in 1:16) {
# 
#   # Melt df
#   ricciardi_2010[[i]] <- melt(ricciardi_2010[[i]], id.vars = c("X__1"), na.rm = TRUE)
# 
#   # Remove interaction value = 0 (no interaction)
#   names(ricciardi_2010[[i]]) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   ricciardi_2010[[i]] <- subset(ricciardi_2010[[i]], ricciardi_2010[[i]]$value != 0)
# 
#   print(head(ricciardi_2010[[i]]))
# }
# 
# sites <- unclass(na.omit(read_excel(path, range = "A13:A102")))
# sites <- sites$'Sites:'
# lat <- c(1.585, 1.787, 1.616, 1.606, 1.767, 1.616, 1.608, 1.464, 1.594, 1.608, 1.455, 1.639, 1.63, 1.651, 1.578, 1.612)
# lon <- c(124.819, 124.778, 124.704, 124.752, 124.793, 124.756, 124.864, 124.793, 124.773, 124.735, 124.744, 124.743, 124.768, 124.698, 124.792, 124.786)
# sites <- data.frame(sites, lat, lon)
# 
# saveRDS(ricciardi_2010, file = "mangal-datasets/ricciardi_2010/data/ricciardi_2010.Rdata")
# saveRDS(sites, file = "mangal-datasets/ricciardi_2010/data/ricciardi_2010_sites.Rdata")
# 
# 
# #------------------------------
# # Set taxa_back table
# #------------------------------
# 
# taxa_back <- vector()
# 
# for (j in 1:16) {
# 
#   ## Get Unique taxa of data
#   taxa <- c(as.character(ricciardi_2010[[j]][, "sp_taxon_1"]),
#             as.character(ricciardi_2010[[j]][, "sp_taxon_2"]))
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
# write.csv2(x = taxa_back_df, file = "mangal-datasets/ricciardi_2010/data/ricciardi_2010_taxa_back.csv", row.names = FALSE)
# 
# 
# #------------------------------
# # Set taxa table
# #------------------------------
# taxa_df <- list()
# 
# for (j in 1:16) {
# 
#   ## Get Unique taxa of data
#   taxa <- c(as.character(ricciardi_2010[[j]][, "sp_taxon_1"]),
#                  as.character(ricciardi_2010[[j]][, "sp_taxon_2"]))
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
# saveRDS(taxa_df, file = "mangal-datasets/ricciardi_2010/data/ricciardi_2010_taxa_df.Rdata")
# 
# 
# #------------------------------
# # Set traits table
# #------------------------------
# 
# # trait_df <- read.csv2(file = "mangal-datasets/ricciardi_2010/data/ricciardi_2010_trait.csv", header = TRUE)
# 
# # trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

ricciardi_2010 <- readRDS("mangal-datasets/ricciardi_2010/data/ricciardi_2010.Rdata")
taxa_back_df <- read.csv2("mangal-datasets/ricciardi_2010/data/ricciardi_2010_taxa_back.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
taxa_df <- readRDS("mangal-datasets/ricciardi_2010/data/ricciardi_2010_taxa_df.Rdata")
sites <- readRDS("mangal-datasets/ricciardi_2010/data/ricciardi_2010_sites.Rdata")


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
for (i in 1:16) {
  
  
  network <- list(name             = paste("hadfield_2014", sites[i, 1]),
                  date             = "2006-07-01",
                  lat              = as.numeric(sites[i, 2]),
                  lon              = as.numeric(sites[i, 3]),
                  srid             = srid,
                  description      = paste("structure of local anemonefish-anemone networks across the Manado region of Sulawesi, Indonesia, ", sites[i, 1]),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  inter <- list(taxon_1_level = "taxon",
                taxon_2_level = "taxon",
                date          = "2006-07-01",
                direction     = "directed",
                type          = "mutualism",
                method        = "scuba dive observations",
                description   = "null",
                public        = TRUE,
                lat           = as.numeric(sites[i, 2]),
                lon           = as.numeric(sites[i, 3]),
                srid          = srid)
  
  POST_network(network_lst = network, enviro = enviro, dataset = dataset, users = users)
  
  POST_taxon(taxa_df = taxa_df[[i]])
  
  POST_interaction(inter_df = ricciardi_2010[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users)
}

rm(list = ls())