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

lat  <- 0
lon  <- 0
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "meening of the interaction value",
                   table_owner = "interactions",
                   description = "DESCRIPTION",
                   unit        = "NA")


# attr1 <- list(name        = "name",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "name",
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


user <- list(name         = "name",
              email        = "null",
              orcid        = "null",
              organization = "null",
              type         = "administrator")


dataset <- list(name        = "name",
                 date        = "1111-11-11",
                 description = "Description of the dataset collected",
                 public      = FALSE)


trait <- list(date = "1111-11-11")


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
  FW_name_I <- read.csv2(file = "importation_mangal/FW_name/raw/FW_name_I.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")

  # Cleaning for melt()
  ## Merge two first COLUMNS Genus species
  FW_name_I[1:2, 1:2] <- "sp."
  FW_name_I <- unite(FW_name_I, sp1, c(V1, V2), sep = " ", remove = TRUE)

  ## Get ROW one with Genus_species
  x  <- paste(FW_name_I[1, ], sep =" ", FW_name_I[2, ])
  x[1] <- "species"
  colnames(FW_name_I) <- x
  rm(x)

  ## Delete unused row
  FW_name_I <- FW_name_I[-c(1:3), -2]

  # Melt df
  FW_name_I <- melt(FW_name_I, id.vars = c("species"), na.rm = TRUE)

  # Retirer les 0 et ajouter dans la table network edge_list = FALSE

  names(FW_name_I) <- c("sp_taxon_1", "sp_taxon_2", "value")

  # Remove interaction value = 0 (no interaction)
  names(FW_name_I) <- c("sp_taxon_1", "sp_taxon_2", "value")
  FW_name_I <- subset(FW_name_I, FW_name_I$value != 0)

# FW_name_II

  FW_name_II <- read.csv2(file = "importation_mangal/FW_name/raw/FW_name_II.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
  FW_name_II[1:2, 1:2] <- "sp."
  FW_name_II <- unite(FW_name_II, sp1, c(V1, V2), sep = " ", remove = TRUE)
  x  <- paste(FW_name_II[1, ], sep =" ", FW_name_II[2, ])
  x[1] <- "species"
  colnames(FW_name_II) <- x
  rm(x)
  FW_name_II <- FW_name_II[-c(1:3), -2]
  FW_name_II <- melt(FW_name_II, id.vars = c("species"), na.rm = TRUE)
  names(FW_name_II) <- c("sp_taxon_1", "sp_taxon_2", "value")
  FW_name_II <- subset(FW_name_II, FW_name_II$value != 0)

# FW_name_III

  FW_name_III <- read.csv2(file = "importation_mangal/FW_name/raw/FW_name_III.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
  FW_name_III[1:2, 1:2] <- "sp."
  FW_name_III <- unite(FW_name_III, sp1, c(V1, V2), sep = " ", remove = TRUE)
  x  <- paste(FW_name_III[1, ], sep =" ", FW_name_III[2, ])
  x[1] <- "species"
  colnames(FW_name_III) <- x
  rm(x)
  FW_name_III <- FW_name_III[-c(1:3), -2]
  FW_name_III <- melt(FW_name_III, id.vars = c("species"), na.rm = TRUE)
  names(FW_name_III) <- c("sp_taxon_1", "sp_taxon_2", "value")
  FW_name_III <- subset(FW_name_III, FW_name_III$value != 0)

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
taxa <- unique(c(as.vector(unique(FW_name_I$sp_taxon_2)), as.vector(unique(FW_name_I$sp_taxon_1)),
                  as.vector(unique(FW_name_II$sp_taxon_2)), as.vector(unique(FW_name_II$sp_taxon_1)),
                  as.vector(unique(FW_name_III$sp_taxon_2)), as.vector(unique(FW_name_III$sp_taxon_1))))


### Check for spelling mistakes... ###


### Remove sp

taxo_back <- vector()

for (i in 1:length(taxa)) {

  if(((str_detect(taxa[i], "[:digit:]") == TRUE || str_detect(taxa[i], "[:punct:]") == TRUE) &
       str_detect(taxa[i], "sp") == TRUE) ||
       str_detect(taxa[i], "n\\.i\\.") ||
       str_detect(taxa[i], "sp$")){

    taxo_back[i] <- word(taxa[i], start = 1)

  } else {
    taxo_back[i] <- taxa[i]
  }
}

taxo_back <- unique(taxo_back)


## Select only taxa not yet in db

server <- "http://localhost:3000"

taxo_back_df <- data.frame()

for (i in 1:length(taxo_back)) {

  path <- modify_url(server, path = paste0("/api/v0/","taxo_backs/?name=", str_replace(taxo_back[i], " ", "%20")))

  if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json")))) == 0) {

    taxo_back_df[nrow(taxo_back_df)+1, 1] <- taxo_back[i]
  }
}

rm(taxo_back)
names(taxo_back_df) <- c("name")

## Get code by species
taxo_back_df[, "bold"] <- NA
taxo_back_df[, "eol"]  <- NA
taxo_back_df[, "tsn"]  <- NA
taxo_back_df[, "ncbi"] <- NA

### Encore probleme d"identification avec les api... ###

for (i in 1:nrow(taxo_back_df)) {
  try (expr = (taxo_back_df[i, 2] <- get_boldid(taxo_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
  try (expr = (taxo_back_df[i, 3] <- get_eolid(taxo_back_df[i, 1], row = 5, verbose = FALSE, key = 110258)[1]), silent = TRUE)
  try (expr = (taxo_back_df[i, 4] <- get_tsn(taxo_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
  try (expr = (taxo_back_df[i, 5] <- get_uid(taxo_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
}

# Writing taxo_back_df
write.csv2(x = taxo_back_df, file = "importation_mangal/FW_name/data/FW_name_taxo_back.csv", row.names = FALSE)

# taxo_back_df <- read.csv2("importation_mangal/FW_name/data/FW_name_taxo_back.csv", header = TRUE)

#------------------------------
  # POST commun table
#------------------------------
POST_attributes(attr_inter)

POST_attributes(attr1)

POST_refs()

POST_users()

POST_datasets()

POST_taxo_back()

# POST_traits(traits_df)

#------------------------------
# FW_name 1
#------------------------------

# Create taxa_df
taxa <- c(as.vector(unique(FW_name_I$sp_taxon_2)), as.vector(unique(FW_name_I$sp_taxon_1)))

taxa_df1 <- data.frame(taxa, NA)
names(taxa_df1) <- c("original_name", "name_clear")

for (i in 1:nrow(taxa_df1)) {

  if(((str_detect(taxa_df1[i, 1], "[:digit:]") == TRUE || str_detect(taxa_df1[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxa_df1[i, 1], "sp") == TRUE) ||
       str_detect(taxa_df1[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxa_df1[i, 1], "sp$") == TRUE){

    taxa_df1[i, 2] <- word(taxa_df1[i, 1], start = 1)

  } else {
    taxa_df1[i, 2] <- as.character(taxa_df1[i, 1])
  }
}

# Set metadata
networks <- list(name               = "FW_name_I",
                   date             = "1111-11-11",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Description of the network collected",
                   public           = TRUE,
                   all_interactions = FALSE)

enviro1 <- list(name  = "attribute name",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1111-11-11",
                value = 0)

# taxa_df1 <- read.csv2("importation_mangal/FW_name/data/FW_name_I_taxa.csv", header = TRUE)
# FW_name_I <- read.csv2("importation_mangal/FW_name/data/FW_name_I_inter.csv", header = TRUE)

# POST table
POST_environments(enviro1, attr1)
POST_networks(networks, enviro = enviro1)
POST_taxons(taxa_df1)
POST_interactions(FW_name_I, enviro = enviro1, attr_inter)

# Writing taxa and interaction table
write.csv2(x = taxa_df1, file = "importation_mangal/FW_name/data/FW_name_I_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name_I, file = "importation_mangal/FW_name/data/FW_name_I_inter.csv", row.names = FALSE)



#------------------------------
# FW_name 2
#------------------------------

# Create taxa_df
taxa <- c(as.vector(unique(FW_name_II$sp_taxon_2)), as.vector(unique(FW_name_II$sp_taxon_1)))

taxa_df2 <- data.frame(taxa, NA)
names(taxa_df2) <- c("original_name", "name_clear")

for (i in 1:nrow(taxa_df2)) {

  if(((str_detect(taxa_df2[i, 1], "[:digit:]") == TRUE || str_detect(taxa_df2[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxa_df2[i, 1], "sp") == TRUE) ||
       str_detect(taxa_df2[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxa_df2[i, 1], "sp$") == TRUE){

    taxa_df2[i, 2] <- word(taxa_df2[i, 1], start = 1)

  } else {
    taxa_df2[i, 2] <- as.character(taxa_df2[i, 1])
  }
}

# Set metadata
networks <- list(name             = "FW_name_II",
                 date             = "1111-11-11",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Description of the network collected",
                 public           = TRUE,
                 all_interactions = FALSE)

enviro2 <- list(name  = "attribute name",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1111-11-11",
                value = 0)

# taxa_df2 <- read.csv2("importation_mangal/FW_name/data/FW_name_II_taxa.csv", header = TRUE)
# FW_name_II <- read.csv2("importation_mangal/FW_name/data/FW_name_II_inter.csv", header = TRUE)

# POST table
POST_environments(enviro2, attr1)
POST_networks(networks, enviro = enviro2)
POST_taxons(taxa_df2)
POST_interactions(FW_name_II, enviro = enviro2, attr_inter)

# Writing taxa and interaction table
write.csv2(x = taxa_df2, file = "importation_mangal/FW_name/data/FW_name_II_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name_II, file = "importation_mangal/FW_name/data/FW_name_II_inter.csv", row.names = FALSE)



#------------------------------
# FW_name 3
#------------------------------

# Create taxa_df
taxa <- c(as.vector(unique(FW_name_III$sp_taxon_2)), as.vector(unique(FW_name_III$sp_taxon_1)))

taxa_df3 <- data.frame(taxa, NA)
names(taxa_df3) <- c("original_name", "name_clear")

for (i in 1:nrow(taxa_df3)) {

  if(((str_detect(taxa_df3[i, 1], "[:digit:]") == TRUE || str_detect(taxa_df3[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxa_df3[i, 1], "sp") == TRUE) ||
       str_detect(taxa_df3[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxa_df3[i, 1], "sp$") == TRUE){

    taxa_df3[i, 2] <- word(taxa_df3[i, 1], start = 1)

  } else {
    taxa_df3[i, 2] <- as.character(taxa_df3[i, 1])
  }
}

# Set metadata
networks <- list(name             = "FW_name_III",
                 date             = "1111-11-11",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Description of the network collected",
                 public           = TRUE,
                 all_interactions = FALSE)

enviro3 <- list(name  = "attribute name",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1111-11-11",
                value = 0)

# taxa_df3 <- read.csv2("importation_mangal/FW_name/data/FW_name_III_taxa.csv", header = TRUE)
# FW_name_III <- read.csv2("importation_mangal/FW_name/data/FW_name_III_inter.csv", header = TRUE)

# POST table
POST_environments(enviro3, attr1)
POST_networks(networks, enviro = enviro3)
POST_taxons(taxa_df3)
POST_interactions(FW_name_III, enviro = enviro3, attr_inter)

# Writing taxa and interaction table
write.csv2(x = taxa_df3, file = "importation_mangal/FW_name/data/FW_name_III_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name_III, file = "importation_mangal/FW_name/data/FW_name_III_inter.csv", row.names = FALSE)

rm(taxa, lat, lon, srid, attr_inter, attr1, refs, users, enviro1, enviro2, enviro3, datasets, traits, networks, inter, taxa_df1, taxa_df2, taxa_df3, taxo_back_df, FW_name_I, FW_name_II, FW_name_III)
