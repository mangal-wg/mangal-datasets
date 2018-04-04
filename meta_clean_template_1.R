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

lat  <- 0
lon  <- 0
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "meening of the interaction value",
                   table_owner = "interactions",
                   description = "DESCRIPTION",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "null",
             data_url  = "URL of the attached data",
             author    = "firt author name",
             year      = "NA",
             bibtex    = "bibtext long format")


user <- list(name         = "NAME",
              email        = "null",
              orcid        = "null",
              organization = "null",
              type         = "administrator")


enviro <- list(name  = "attribute name",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1111-11-11",
               value = 0)


dataset <- list(name        = "NAME",
                 date        = "1111-11-11",
                 description = "Description of the dataset collected",
                 public      = FALSE)


trait <- list(date = "1111-11-11")


network <- list(name             = "NAME",
                 date             = "1111-11-11",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Description of the network collected",
                 public           = FALSE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "[taxon, population, individual]",
              taxon_2_level = "[taxon, population, individual]",
              date          = "1111-11-11",
              direction     = "unknown",
              type          = "unknown",
              method        = "null",
              description   = "null",
              public        = FALSE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
FW_name <- read.csv2(file = "importation_mangal/FW_name/raw/FW_name.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")

# Cleaning for melt()
## Merge two first COLUMNS Genus species
FW_name[is.na(FW_name)] <- "sp."
FW_name <- unite(FW_name, sp1, c(V1, V2), sep = " ", remove = TRUE)

### Si on choisis de retirer les sp, bonne fonction pour unir les 2 colonnes avec
### un " " sans inclure les NA: str_interp(string, env = parent.frame())

## Get ROW one with Genus_species
x  <- paste(FW_name[1, ], sep =" ", FW_name[2, ])
x[1] <- "species"
colnames(FW_name) <- x
rm(x)

## Delete unused row
FW_name <- FW_name[-c(1, 2), ]

# Melt df
FW_name <- melt(FW_name, id.vars = c("species"), na.rm = TRUE)

# Remove interaction value = 0 (no interaction)
names(FW_name) <- c("sp_taxon_1", "sp_taxon_2", "value")
FW_name <- subset(FW_name, FW_name$value != 0)

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
taxa <- c(as.vector(unique(FW_name$sp_taxon_2)), as.vector(unique(FW_name$sp_taxon_1)))


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

  path <- modify_url(server, path = paste0("/api/v2/","taxa_backs/?name=", str_replace(taxa_back[i], " ", "%20")))

  if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json", 
                                                          "Authorization" = paste("bearer", readRDS("importation_mangal/.httr-oauth")))))) == 0) {

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

# trait_df <- read.csv2(file = "importation_mangal/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "importation_mangal/FW_name/data/FW_name_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxa_df, file = "importation_mangal/FW_name/data/FW_name_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "importation_mangal/FW_name/data/FW_name_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "importation_mangal/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("importation_mangal/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("importation_mangal/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("importation_mangal/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("importation_mangal/FW_name/data/FW_name_trait.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attribute(attr_inter)
# POST_attribute(attr1)
# POST_attribute(attr2)
POST_ref()
POST_user()
# POST_environment(enviro, attr_##)
POST_dataset()
POST_network(network, enviro = enviro)
POST_taxa_back()
POST_taxon(taxa_df)
# POST_traits(trait_df)
POST_interaction(FW_name, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, ref, user, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)
