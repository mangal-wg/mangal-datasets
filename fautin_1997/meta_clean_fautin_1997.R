# Set libraries
library(reshape2)
library(tidyr)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)

devtools::install_github("tidyverse/readxl")
library("readxl")

library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- 0
lon  <- 0
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence or absence of an interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi       = "10.2307/1447266",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://www.jstor.org/stable/1447266?origin=crossref&seq=1#page_scan_tab_contents",
            data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/anemone_fish/anemonefish.xls",
            author    = "fautin",
            year      = "1993",
            bibtex    = "@article{Mariscal_1993,doi = {10.2307/1447266},url = {https://doi.org/10.2307%2F1447266},year = 1993,month = {aug},publisher = {{JSTOR}},volume = {1993},number = {3},pages = {899},author = {Richard N. Mariscal and Daphne G. Fautin and Gerald R. Allen},title = {Field Guide to Anemonefishes and Their Host Sea Anemones},journal = {Copeia}}")


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


dataset <- list(name         = "fautin_1993",
                 date        = "1993-01-01",
                 description = "Anemonfishes-anemons intractions in the tropical Indo-Pacific ocean",
                 public      = TRUE)


trait <- list(date = "1111-11-11")


network <- list(name             = "fautin_1993",
                 date             = "1993-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Anemonfishes-anemons intractions in the tropical Indo-Pacific ocean",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1993-01-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation + literature",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
path = "mangal-datasets/fautin_1997/raw/fautin_1997.xls"
fautin_1997 <- read_excel(path, range = "B14:M40")

# Cleaning for melt()
## Fill NAs
fautin_1997[is.na(fautin_1997)] <- word(as.character(fautin_1997[12, 1]), start = 1)
fautin_1997[12, 1] <- word(fautin_1997[12, 1], start = 1)

## Merge two first COLUMNS Genus species
fautin_1997 <- unite(fautin_1997, "anemonefish", c("X__1", "Host anemones:"), sep = " ", remove = TRUE)

# Melt df
fautin_1997 <- melt(fautin_1997, id.vars = c("anemonefish"), na.rm = TRUE)

# Remove interaction value = 0 (no interaction)
names(fautin_1997) <- c("sp_taxon_1", "sp_taxon_2", "value")
fautin_1997 <- subset(fautin_1997, fautin_1997$value != 0)

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
taxa_back <- c(as.vector(unique(fautin_1997$sp_taxon_2)), as.vector(unique(fautin_1997$sp_taxon_1)))

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

# Create taxa_df

taxa_df <- data.frame(taxa, NA)
names(taxa_df) <- c("original_name", "name_clear")

for (i in 1:nrow(taxa_df)) {

  if(((str_detect(taxa_df[i, 1], "[:digit:]") == TRUE || str_detect(taxa_df[i, 1], "[:punct:]") == TRUE) &
      str_detect(taxa_df[i, 1], "sp") == TRUE) ||
      str_detect(taxa_df[i, 1], "n\\.i\\.") == TRUE ||
      str_detect(taxa_df[i, 1], "sp$") == TRUE){

    taxa_df[i, 2] <- word(taxa_df[i, 1], start = 1)

  } else {
    taxa_df[i, 2] <- as.character(taxa_df[i, 1])
  }
}

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/fautin_1997/data/fautin_1997_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets/fautin_1997/data/fautin_1997_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxa_df, file = "mangal-datasets/fautin_1997/data/fautin_1997_taxa.csv", row.names = FALSE)
write.csv2(x = fautin_1997, file = "mangal-datasets/fautin_1997/data/fautin_1997_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/fautin_1997/data/fautin_1997_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/fautin_1997/data/fautin_1997_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/fautin_1997/data/fautin_1997_taxa.csv", header = TRUE)
# fautin_1997 <- read.csv2("mangal-datasets/fautin_1997/data/fautin_1997_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/fautin_1997/data/fautin_1997_trait.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attribute(attr = attr_inter)
# POST_attribute(attr1)
# POST_attribute(attr2)
POST_ref(ref)
POST_user(users)
# POST_environment(enviro, attr_##)
POST_dataset(dataset, users, ref)
POST_network(network_lst = network, enviro = enviro, dataset, users)
POST_taxa_back(taxa_back = taxa_back_df)
POST_taxon(taxa_df)
# POST_traits(trait_df, network)
POST_interaction(inter_df = fautin_1997, inter = inter, enviro = enviro, attr = attr_inter, users)

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, fautin_1997)
