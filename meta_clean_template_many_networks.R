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


# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")


refs <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "null",
             data_url  = "URL of the attached data",
             author    = "firt author name",
             year      = "NA",
             bibtex    = "bibtext long format")


users <- list(name         = "NAME",
              email        = "null",
              orcid        = "null",
              organization = "null",
              type         = "administrator")


datasets <- list(name        = "NAME",
                 date        = "1111-11-11",
                 description = "Description of the dataset collected",
                 public      = FALSE)


traits <- list(date = "1111-11-11")


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

# Set WD
setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/FW_NAME")

  # Open file
  FW_NAME_I <- read.csv2(file = paste0("raw/FW_NAME_I.csv"), header = FALSE, stringsAsFactors = FALSE, na.strings = "")

  # Cleaning for melt()
  ## Merge two first COLUMNS Genus species
  FW_NAME_I[1:2, 1:2] <- "sp."
  FW_NAME_I <- unite(FW_NAME_I, sp1, c(V1, V2), sep = " ", remove = TRUE)

  ## Get ROW one with Genus_species
  x  <- paste(FW_NAME_I[1, ], sep =" ", FW_NAME_I[2, ])
  x[1] <- "species"
  colnames(FW_NAME_I) <- x
  rm(x)

  ## Delete unused row
  FW_NAME_I <- FW_NAME_I[-c(1:3), -2]

  # Melt df
  FW_NAME_I <- melt(FW_NAME_I, id.vars = c("species"), na.rm = TRUE)

  # Retirer les 0 et ajouter dans la table network edge_list = FALSE

  names(FW_NAME_I) <- c("sp_taxon_1", "sp_taxon_2", "value")

  # Remove interaction value = 0 (no interaction)
  names(FW_NAME_I) <- c("sp_taxon_1", "sp_taxon_2", "value")
  FW_NAME_I <- subset(FW_NAME_I, FW_NAME_I$value != 0)

# FW_NAME_II

  FW_NAME_II <- read.csv2(file = "raw/FW_NAME_II.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
  FW_NAME_II[1:2, 1:2] <- "sp."
  FW_NAME_II <- unite(FW_NAME_II, sp1, c(V1, V2), sep = " ", remove = TRUE)
  x  <- paste(FW_NAME_II[1, ], sep =" ", FW_NAME_II[2, ])
  x[1] <- "species"
  colnames(FW_NAME_II) <- x
  rm(x)
  FW_NAME_II <- FW_NAME_II[-c(1:3), -2]
  FW_NAME_II <- melt(FW_NAME_II, id.vars = c("species"), na.rm = TRUE)
  names(FW_NAME_II) <- c("sp_taxon_1", "sp_taxon_2", "value")
  FW_NAME_II <- subset(FW_NAME_II, FW_NAME_II$value != 0)

# FW_NAME_III

  FW_NAME_III <- read.csv2(file = "raw/FW_NAME_III.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
  FW_NAME_III[1:2, 1:2] <- "sp."
  FW_NAME_III <- unite(FW_NAME_III, sp1, c(V1, V2), sep = " ", remove = TRUE)
  x  <- paste(FW_NAME_III[1, ], sep =" ", FW_NAME_III[2, ])
  x[1] <- "species"
  colnames(FW_NAME_III) <- x
  rm(x)
  FW_NAME_III <- FW_NAME_III[-c(1:3), -2]
  FW_NAME_III <- melt(FW_NAME_III, id.vars = c("species"), na.rm = TRUE)
  names(FW_NAME_III) <- c("sp_taxon_1", "sp_taxon_2", "value")
  FW_NAME_III <- subset(FW_NAME_III, FW_NAME_III$value != 0)

#------------------------------
# Set taxo_back and taxon table
#------------------------------
# Create taxo_back_df

## Get Unique taxon of data
taxon <- unique(c(as.vector(unique(FW_NAME_I$sp_taxon_2)), as.vector(unique(FW_NAME_I$sp_taxon_1)),
                  as.vector(unique(FW_NAME_II$sp_taxon_2)), as.vector(unique(FW_NAME_II$sp_taxon_1)),
                  as.vector(unique(FW_NAME_III$sp_taxon_2)), as.vector(unique(FW_NAME_III$sp_taxon_1))))


### Check for spelling mistakes... ###


### Remove sp

taxo_back <- vector()

for (i in 1:length(taxon)) {

  if(((str_detect(taxon[i], "[:digit:]") == TRUE || str_detect(taxon[i], "[:punct:]") == TRUE) &
       str_detect(taxon[i], "sp") == TRUE) ||
       str_detect(taxon[i], "n\\.i\\.") ||
       str_detect(taxon[i], "sp$")){

    taxo_back[i] <- word(taxon[i], start = 1)

  } else {
    taxo_back[i] <- taxon[i]
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
write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/FW_NAME_taxo_back.csv"), row.names = FALSE)

# setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/FW_NAME")
# taxo_back_df <- read.csv2("data/FW_NAME_taxo_back.csv", header = TRUE)

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
# FW_NAME 1
#------------------------------

# Create taxons_df
taxon <- c(as.vector(unique(FW_NAME_I$sp_taxon_2)), as.vector(unique(FW_NAME_I$sp_taxon_1)))

taxons_df1 <- data.frame(taxon, NA)
names(taxons_df1) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df1)) {

  if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxons_df[i, 1], "sp") == TRUE) ||
       str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxons_df[i, 1], "sp$") == TRUE){

    taxons_df1[i, 2] <- word(taxons_df1[i, 1], start = 1)

  } else {
    taxons_df1[i, 2] <- as.character(taxons_df1[i, 1])
  }
}

# Set metadata
networks <- list(name               = "FW_NAME_I",
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

# setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/FW_NAME")
# taxon_df1 <- read.csv2("data/FW_NAME_I_taxons.csv", header = TRUE)
# FW_NAME_I <- read.csv2("data/FW_NAME_I_inter.csv", header = TRUE)

# POST table
POST_environments(enviro1, attr1)
POST_networks(networks, enviro = enviro1)
POST_taxons(taxons_df1)
POST_interactions(FW_NAME_I, enviro = enviro1, attr_inter)

# Writing taxon and interaction table
write.csv2(x = taxons_df1, file = paste0(getwd(), "/data/FW_NAME_I_taxons.csv"), row.names = FALSE)
write.csv2(x = FW_NAME_I, file = paste0(getwd(), "/data/FW_NAME_I_inter.csv"), row.names = FALSE)



#------------------------------
# FW_NAME 2
#------------------------------

# Create taxons_df
taxon <- c(as.vector(unique(FW_NAME_II$sp_taxon_2)), as.vector(unique(FW_NAME_II$sp_taxon_1)))

taxons_df2 <- data.frame(taxon, NA)
names(taxons_df2) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df2)) {

  if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxons_df[i, 1], "sp") == TRUE) ||
       str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxons_df[i, 1], "sp$") == TRUE){

    taxons_df2[i, 2] <- word(taxons_df2[i, 1], start = 1)

  } else {
    taxons_df2[i, 2] <- as.character(taxons_df2[i, 1])
  }
}

# Set metadata
networks <- list(name             = "FW_NAME_II",
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

# setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/FW_NAME")
# taxon_df2 <- read.csv2("data/FW_NAME_II_taxons.csv", header = TRUE)
# FW_NAME_II <- read.csv2("data/FW_NAME_II_inter.csv", header = TRUE)

# POST table
POST_environments(enviro2, attr1)
POST_networks(networks, enviro = enviro2)
POST_taxons(taxons_df2)
POST_interactions(FW_NAME_II, enviro = enviro2, attr_inter)

# Writing taxon and interaction table
write.csv2(x = taxons_df2, file = paste0(getwd(), "/data/FW_NAME_II_taxons.csv"), row.names = FALSE)
write.csv2(x = FW_NAME_II, file = paste0(getwd(), "/data/FW_NAME_II_inter.csv"), row.names = FALSE)



#------------------------------
# FW_NAME 3
#------------------------------

# Create taxons_df
taxon <- c(as.vector(unique(FW_NAME_III$sp_taxon_2)), as.vector(unique(FW_NAME_III$sp_taxon_1)))

taxons_df3 <- data.frame(taxon, NA)
names(taxons_df3) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df3)) {

  if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxons_df[i, 1], "sp") == TRUE) ||
       str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxons_df[i, 1], "sp$") == TRUE){

    taxons_df3[i, 2] <- word(taxons_df3[i, 1], start = 1)

  } else {
    taxons_df3[i, 2] <- as.character(taxons_df3[i, 1])
  }
}

# Set metadata
networks <- list(name             = "FW_NAME_III",
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

# setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal/FW_NAME")
# taxon_df3 <- read.csv2("data/FW_NAME_III_taxons.csv", header = TRUE)
# FW_NAME_III <- read.csv2("data/FW_NAME_III_inter.csv", header = TRUE)

# POST table
POST_environments(enviro3, attr1)
POST_networks(networks, enviro = enviro3)
POST_taxons(taxons_df3)
POST_interactions(FW_NAME_III, enviro = enviro3, attr_inter)

# Writing taxon and interaction table
write.csv2(x = taxons_df3, file = paste0(getwd(), "/data/FW_NAME_III_taxons.csv"), row.names = FALSE)
write.csv2(x = FW_NAME_III, file = paste0(getwd(), "/data/FW_NAME_III_inter.csv"), row.names = FALSE)

rm(taxon, lat, lon, srid, attr_inter, attr1, refs, users, enviro1, enviro2, enviro3, datasets, traits, networks, inter, taxons_df1, taxons_df2, taxons_df3, taxo_back_df, FW_NAME_I, FW_NAME_II, FW_NAME_III)
