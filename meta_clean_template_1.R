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


enviro <- list(name  = "attribute name",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "1111-11-11",
               value = 0)


datasets <- list(name        = "NAME",
                 date        = "1111-11-11",
                 description = "Description of the dataset collected",
                 public      = FALSE)


traits <- list(date = "1111-11-11")


networks <- list(name             = "NAME",
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
# Set taxo_back and taxon table
#------------------------------
# Create taxo_back_df

## Get Unique taxon of data
taxon <- c(as.vector(unique(FW_name$sp_taxon_2)), as.vector(unique(FW_name$sp_taxon_1)))


### Check for spelling mistakes... ###


## Remove sp

taxo_back <- vector()

for (i in 1:length(taxon)) {

  if(((str_detect(taxon[i], "[:digit:]") == TRUE || str_detect(taxon[i], "[:punct:]") == TRUE) &
       str_detect(taxon[i], "sp") == TRUE) ||
       str_detect(taxon[i], "n\\.i\\.") == TRUE ||
       str_detect(taxon[i], "sp$") == TRUE){

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

# Create taxons_df

taxons_df <- data.frame(taxon, NA)
names(taxons_df) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df)) {

  if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
      str_detect(taxons_df[i, 1], "sp") == TRUE) ||
      str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
      str_detect(taxons_df[i, 1], "sp$") == TRUE){

    taxons_df[i, 2] <- word(taxons_df[i, 1], start = 1)

  } else {
    taxons_df[i, 2] <- as.character(taxons_df[i, 1])
  }
}

#------------------------------
# Set traits table
#------------------------------

# traits_df <- read.csv2(file = "importation_mangal/FW_name/data/FW_name_traits.csv", header = TRUE)

# traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# names(traits_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxon and interaction table
#------------------------------

write.csv2(x = taxo_back_df, file = "importation_mangal/FW_name/data/FW_name_taxo_back.csv", row.names = FALSE)
write.csv2(x = taxons_df, file = "importation_mangal/FW_name/data/FW_name_taxons.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "importation_mangal/FW_name/data/FW_name_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "importation_mangal/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxo_back_df <- read.csv2("importation_mangal/FW_name/data/FW_name_taxo_back.csv", header = TRUE)
# taxons_df <- read.csv2("importation_mangal/FW_name/data/FW_name_taxons.csv", header = TRUE)
# FW_name <- read.csv2("importation_mangal/FW_name/data/FW_name_inter.csv", header = TRUE)
# traits_df <- read.csv2("importation_mangal/FW_name/data/FW_name_traits.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attributes(attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
POST_refs()
POST_users()
# POST_environments(enviro, attr_##)
POST_datasets()
POST_networks(networks, enviro = enviro)
POST_taxo_back()
POST_taxons(taxons_df)
# POST_traits(traits_df)
POST_interactions(FW_name, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, FW_name)
