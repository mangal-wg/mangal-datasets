# Set libraries
library(reshape2)
library(tidyr)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)

library(mangal)

#------------------------------
  # Metadata
#------------------------------

srid <- 4326
xy <- read.csv2("mangal-datasets/ponisio_2017/raw/coordinates.csv", header = TRUE, stringsAsFactors = FALSE)
lat <- xy$lat
lon <- xy$lon
rm(xy)

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "mean interaction frequency",
                   table_owner = "interactions",
                   description = "mean interaction frequency over the sampling season",
                   unit        = "NA")


ref <- list(doi       = "10.1111/ele.12821",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12821",
            data_url  = "https://github.com/lponisio/hedgerow_assembly/tree/master/data",
            author    = "ponisio",
            year      = "2017",
            bibtex    = "@article{Ponisio_2017,doi = {10.1111/ele.12821},url = {https://doi.org/10.1111%2Fele.12821},year = 2017,month = {sep},publisher = {Wiley-Blackwell},volume = {20},number = {10},pages = {1261--1272},author = {Lauren C. Ponisio and Marilia P. Gaiarsa and Claire Kremen},editor = {Dominique Gravel},title = {Opportunistic attachment assembles plant-pollinator networks},journal = {Ecology Letters}}")

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


dataset <- list(name         = "ponisio_2017",
                 date        = "2017-01-01",
                 description = "assembly of plant-pollinator communities at native plant restoration sites in an agricultural landscape, California",
                 public      = TRUE)


trait <- list(date = "2017-01-01")


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
load(file = "mangal-datasets/ponisio_2017/raw/networks/all_networks_years.Rdata")
load(file = "mangal-datasets/ponisio_2017/raw/networks/expanded_networks.Rdata") # Creer les network avec les interactions!
ponisio_2017 <- nets
nets <- data.frame(names = names(nets))


for (i in 1:length(ponisio_2017)) {

  # Melt df
  ponisio_2017[[i]] <- melt(ponisio_2017[[i]])

  # Remove interaction value = 0 (no interaction)
  names(ponisio_2017[[i]]) <- c("sp_taxon_1", "sp_taxon_2", "value")
  ponisio_2017[[i]] <- subset(ponisio_2017[[i]], ponisio_2017[[i]]$value != 0)

  ponisio_2017[[i]]$sp_taxon_1 <- as.character(ponisio_2017[[i]]$sp_taxon_1)
  ponisio_2017[[i]]$sp_taxon_2 <- as.character(ponisio_2017[[i]]$sp_taxon_2)
  
  # Remove name in parenteses
  for (j in 1:nrow(ponisio_2017[[i]])) {
    ponisio_2017[[i]][[j, 1]] <- str_remove(ponisio_2017[[i]][[j, 1]], " \\(.*\\)")
    ponisio_2017[[i]][[j, 2]] <- str_remove(ponisio_2017[[i]][[j, 2]], " \\(.*\\)")
  }
}

saveRDS(ponisio_2017, file = "mangal-datasets/ponisio_2017/data/ponisio_2017.Rdata")


#------------------------------
# Set taxa_back table
#------------------------------

taxa_back <- vector()

for (j in 1:length(ponisio_2017)) {

  ## Get Unique taxa of data
  taxa <- c(ponisio_2017[[j]][, "sp_taxon_1"],
            ponisio_2017[[j]][, "sp_taxon_2"])

  ### Remove sp

  for (i in 1:length(taxa)) {

    if(((str_detect(taxa[i], "[:digit:]") == TRUE || str_detect(taxa[i], "[:punct:]") == TRUE) == TRUE &
         str_detect(taxa[i], "sp") == TRUE) ||
         str_detect(taxa[i], "indet\\.") == TRUE ||
         str_detect(taxa[i], "sp$") == TRUE ||
         str_detect(taxa[i], "\\?") == TRUE ||
         str_detect(taxa[i], "nr\\.") == TRUE){

      taxa[i] <- word(taxa[i], start = 1)

    } else {
      taxa[i] <- taxa[i]
    }
  }
  taxa_back <- c(taxa_back, taxa)
}

taxa_back <- unique(taxa_back)


## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- data.frame()

for (i in 1:length(taxa_back)) {

  path <- modify_url(server, path = paste0("/api/v2/","taxa_back/?name=", str_replace(taxa_back[i], " ", "%20")))

  if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json",
                                                          "Authorization" = paste("bearer", readRDS("mangal-datasets/.httr-oauth")))))) == 0) {

    taxa_back_df[nrow(taxa_back_df)+1, 1] <- taxa_back[i]
  }
}

rm(taxa_back)
names(taxa_back_df) <- c("name")

## Get code by species
taxa_back_df[, "bold"] <- NA
taxa_back_df[, "eol"]  <- NA
taxa_back_df[, "tsn"]  <- NA
taxa_back_df[, "ncbi"] <- NA

### Encore probleme d"identification avec les api... ###

for (i in 1:nrow(taxa_back_df)) {
  try (expr = (taxa_back_df[i, 2] <- get_boldid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 3] <- get_eolid(taxa_back_df[i, 1], row = 5, verbose = FALSE, key = 110258)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 4] <- get_tsn(taxa_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 5] <- get_uid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
}

# Writing taxa_back_df
write.csv2(x = taxa_back_df, file = "mangal-datasets/ponisio_2017/data/ponisio_2017_taxa_back.csv", row.names = FALSE)


#------------------------------
# Set taxa table
#------------------------------
taxa_df <- list()

for (j in 1:length(ponisio_2017)) {

  ## Get Unique taxa of data
  taxa <- c(as.character(ponisio_2017[[j]][, "sp_taxon_1"]),
                 as.character(ponisio_2017[[j]][, "sp_taxon_2"]))


  taxa <- unique(taxa)

  taxa_df[[j]] <- as.data.frame(taxa)
  taxa_df[[j]]["name_clear"] <- NA
  names(taxa_df[[j]]) <- c("original_name", "name_clear")

  for (i in 1:nrow(taxa_df[[j]])) {

    if(((str_detect(taxa_df[[j]][i, 1], "[:digit:]") == TRUE || str_detect(taxa_df[[j]][i, 1], "[:punct:]") == TRUE) &
         str_detect(taxa_df[[j]][i, 1], "sp") == TRUE) ||
         str_detect(taxa_df[[j]][i, 1], "indet\\.") == TRUE ||
         str_detect(taxa_df[[j]][i, 1], "\\.") == TRUE ||
         str_detect(taxa_df[[j]][i, 1], "sp$") == TRUE){

      taxa_df[[j]][i, 2] <- word(taxa_df[[j]][i, 1], start = 1)

    } else {
      taxa_df[[j]][i, 2] <- as.character(taxa_df[[j]][i, 1])
    }
  }
}

# Writing taxa_df
saveRDS(taxa_df, file = "mangal-datasets/ponisio_2017/data/ponisio_2017_taxa_df.Rdata")


#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/ponisio_2017/data/ponisio_2017_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Set network table
#------------------------------
nets["date"] <- paste0(str_sub(nets[,1], -4, -1), "-01-01")
write.csv2(nets, "mangal-datasets/ponisio_2017/data/ponisio_2017_nets.csv")


#------------------------------
# Writing taxa and interaction table
#------------------------------

ponisio_2017 <- readRDS("mangal-datasets/ponisio_2017/data/ponisio_2017.Rdata")
taxa_back_df <- read.csv2("mangal-datasets/ponisio_2017/data/ponisio_2017_taxa_back.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
taxa_df <- readRDS("mangal-datasets/ponisio_2017/data/ponisio_2017_taxa_df.Rdata")
nets <- read.csv2("mangal-datasets/ponisio_2017/data/ponisio_2017_nets.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)


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
for (i in 1:length(ponisio_2017)) {
  
  
  network <- list(name             = paste("ponisio_2017_", as.character(nets[i, 1])),
                  date             = nets[i, 2],
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste("assembly of plant-pollinator communities at native plant restoration sites in an agricultural landscape, California, ", nets[i, 1]),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  inter <- list(taxon_1_level = "taxon",
                taxon_2_level = "taxon",
                date          = nets[i, 2],
                direction     = "directed",
                type          = "mutualism",
                method        = "field capture",
                description   = "null",
                public        = TRUE,
                lat           = lat,
                lon           = lon,
                srid          = srid)
  
  POST_network(network_lst = network, enviro = enviro, dataset = dataset, users = users)
  
  POST_taxon(taxa_df = taxa_df[[i]])
  
  POST_interaction(inter_df = ponisio_2017[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users)
}

rm(list = ls())