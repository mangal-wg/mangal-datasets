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

lat  <- 36.075391
lon  <- -79.001843
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Number of pollinator",
                   table_owner = "interactions",
                   description = "Number of insect captured while pollination",
                   unit        = "individual captured")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi        = "10.2307/2937269",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.2307%2F2937269",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/html/motten_1982.html",
             author    = "motten",
             year      = "1982",
             bibtex    = "@article{Motten_1986,doi = {10.2307/2937269},url = {https://doi.org/10.2307%2F2937269},year = 1986,month = {feb},publisher = {Wiley-Blackwell},volume = {56},number = {1},pages = {21--42},author = {Alexander F. Motten},title = {Pollination Ecology of the Spring Wildflower Community of a Temperate Deciduous Forest},journal = {Ecological Monographs}}")


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


dataset <- list(name         = "motten_1982",
                 date        = "1982-01-01",
                 description = "spring wildflower community of mesic deciduous forests in piedmont North Carolina",
                 public      = TRUE)


trait <- list(date = "1111-11-11")


network <- list(name              = "motten_1982",
                 date             = "1982-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "spring wildflower community of mesic deciduous forests in piedmont North Carolina",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1982-01-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "null",
              description   = "capture",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
motten_1982 <- read.csv2(file = "mangal-datasets/motten_1982/raw/motten_1982.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "", sep = ",")

# Cleaning for melt()
## Merge two first COLUMNS Genus species
motten_1982[is.na(motten_1982)] <- "sp."
motten_1982[, 2] <- word(motten_1982[, 2], -1)
motten_1982 <- unite(motten_1982, sp1, c(V1, V2), sep = " ", remove = TRUE)

### Si on choisis de retirer les sp, bonne fonction pour unir les 2 colonnes avec
### un " " sans inclure les NA: str_interp(string, env = parent.frame())

## Get ROW one with Genus_species
x  <- paste(motten_1982[1, ], sep =" ", motten_1982[2, ])
x[1] <- "species"
colnames(motten_1982) <- x
rm(x)

## Delete unused row
motten_1982 <- motten_1982[-c(1, 2, 3), -2]

# Melt df
motten_1982 <- melt(motten_1982, id.vars = c("species"), na.rm = TRUE)

# Remove interaction value = 0 (no interaction)
names(motten_1982) <- c("sp_taxon_1", "sp_taxon_2", "value")
motten_1982 <- subset(motten_1982, motten_1982$value != 0)

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
taxa <- c(as.vector(unique(motten_1982$sp_taxon_2)), as.vector(unique(motten_1982$sp_taxon_1)))


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

# trait_df <- read.csv2(file = "mangal-datasets/motten_1982/data/motten_1982_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets/motten_1982/data/motten_1982_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxa_df, file = "mangal-datasets/motten_1982/data/motten_1982_taxa.csv", row.names = FALSE)
write.csv2(x = motten_1982, file = "mangal-datasets/motten_1982/data/motten_1982_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/motten_1982/data/motten_1982_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/motten_1982/data/motten_1982_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/motten_1982/data/motten_1982_taxa.csv", header = TRUE)
# motten_1982 <- read.csv2("mangal-datasets/motten_1982/data/motten_1982_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/motten_1982/data/motten_1982_trait.csv", header = TRUE)

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
POST_interaction(inter_df = motten_1982, inter = inter, enviro = enviro, attr = attr_inter, users)

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, motten_1982)
