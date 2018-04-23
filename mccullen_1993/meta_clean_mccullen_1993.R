# Set libraries
library(reshape2)
library(tidyr)
#library(jsonlite)
#library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)

library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- -0.5
lon  <- -90.5
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Pollinisator recorded on a flower",
                   table_owner = "interactions",
                   description = "Presence or absence of interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi        = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "null",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/html/mccullen_1993.html",
             author    = "mccullen",
             year      = "1993",
             bibtex    = "NA")


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


dataset <- list(name        = "mccullen_1993",
                 date        = "1993-01-01",
                 description = "compilation of records on plant-flower visitor interactions in the Galápagos archipelago found in the literature. Pinta Island",
                 public      = TRUE)


trait <- list(date = "1111-11-11")


network <- list(name              = "mccullen_1993",
                 date             = "1993-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "compilation of records on plant-flower visitor interactions in the Galápagos archipelago found in the literature. Pinta Island",
                 public           = FALSE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1993-01-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Search in the literature",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
mccullen_1993 <- read.csv2(file = "mangal-datasets/mccullen_1993/raw/mccullen_1993.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "", sep = ",")

# Cleaning for melt()
## Merge two first COLUMNS Genus species
mccullen_1993[is.na(mccullen_1993)] <- "sp."
mccullen_1993 <- unite(mccullen_1993, sp1, c(V1, V2), sep = " ", remove = TRUE)

## Get ROW one with Genus_species
x  <- paste(mccullen_1993[1, ], sep =" ", mccullen_1993[2, ])
x[1] <- "species"
colnames(mccullen_1993) <- x
rm(x)

## Delete unused row
mccullen_1993 <- mccullen_1993[-c(1, 2, 3), -2]

## Change taxonomy -> Use familly when there is only a description of a unknown genus
mccullen_1993[c(10, 18, 19, 23, 24, 32, 33, 34, 47), 1] <- c("grillidae", "hemerobiidae", "sphingidae", "miridae", "cicadellidae", "gelechioidea", "pyralidae", "tortricidae", "acrididae")
mccullen_1993 <- mccullen_1993[-c(5, 8, 11), ]

# Melt df
mccullen_1993 <- melt(mccullen_1993, id.vars = c("species"), na.rm = TRUE)

# Remove interaction value = 0 (no interaction)
names(mccullen_1993) <- c("sp_taxon_1", "sp_taxon_2", "value")
mccullen_1993 <- subset(mccullen_1993, mccullen_1993$value != 0)

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
taxa <- c(as.vector(unique(mccullen_1993$sp_taxon_2)), as.vector(unique(mccullen_1993$sp_taxon_1)))


### Check for spelling mistakes... ###


## Remove sp

taxa_back <- vector()

for (i in 1:length(taxa)) {

  if(((str_detect(taxa[i], "[:digit:]") == TRUE || str_detect(taxa[i], "[:punct:]") == TRUE) &
       str_detect(taxa[i], "sp") == TRUE) ||
       str_detect(taxa[i], "n\\.i\\.") == TRUE ||
       str_detect(taxa[i], "sp$") == TRUE){

    taxa_back[i] <- word(taxa[i], start = 1)

  } else {
    taxa_back[i] <- taxa[i]
  }
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

# trait_df <- read.csv2(file = "mangal-datasets/mccullen_1993/data/mccullen_1993_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets/mccullen_1993/data/mccullen_1993_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxa_df, file = "mangal-datasets/mccullen_1993/data/mccullen_1993_taxa.csv", row.names = FALSE)
write.csv2(x = mccullen_1993, file = "mangal-datasets/mccullen_1993/data/mccullen_1993_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/mccullen_1993/data/mccullen_1993_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/mccullen_1993/data/mccullen_1993_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/mccullen_1993/data/mccullen_1993_taxa.csv", header = TRUE)
# mccullen_1993 <- read.csv2("mangal-datasets/mccullen_1993/data/mccullen_1993_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/mccullen_1993/data/mccullen_1993_trait.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attribute(attr_inter)
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
POST_interaction(inter_df = mccullen_1993, inter = inter, enviro = enviro, attr = attr_inter, users)

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, mccullen_1993)
