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


refs <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "null",
             data_url  = "URL of the attached data",
             author    = "firt author name",
             year      = "NA",
             bibtex    = "bibtext long format")


users <- list(name         = "name",
              email        = "null",
              orcid        = "null",
              organization = "null",
              type         = "administrator")


datasets <- list(name        = "name",
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
library(magrittr)
library(tidyr)

unlink("importation_mangal/Kolpelke_2017/reshapeSalix/csv", recursive = TRUE)
unlink("importation_mangal/Kolpelke_2017/reshapeSalix/rdata", recursive = TRUE)
  
source("importation_mangal/Kolpelke_2017/reshapeSalix/lib/format4R.r")
get_formatData("importation_mangal/Kolpelke_2017/reshapeSalix/data/Salix_webs.csv")

# Open files
df_site    <- readRDS("importation_mangal/Kolpelke_2017/rdata/df_site.rds")
df_salix   <- readRDS("importation_mangal/Kolpelke_2017/rdata/df_salix.rds")
df_galler  <- readRDS("importation_mangal/Kolpelke_2017/rdata/df_galler.rds")
df_parasit <- readRDS("importation_mangal/Kolpelke_2017/rdata/df_parasit.rds")
df_inter   <- readRDS("importation_mangal/Kolpelke_2017/rdata/df_interact.rds")


# Merging site and interaction
site_inter <- merge(df_inter, df_salix, by = "RSAL")
site_inter <- merge(site_inter, df_galler, by = "RGALLER")
site_inter <- merge(site_inter, df_parasit, by = "RPAR")
site_inter <- merge(site_inter, df_site, by = "REARING_NUMBER")

# Isolate interactions  
inter_galler <- site_inter[, c(1, 26, 7, 11, 5 )]
names(inter_galler) <- c("REARING_NUMBER", "date", "taxon_sp_1", "taxon_sp_2", "value")
inter_galler["attr"] <- "Number of galls"
inter_galler["salix"] <- "Salix "
inter_galler <- unite(inter_galler, taxon_sp_1, c(salix, taxon_sp_1), sep = "") 

inter_para <- site_inter[, c(1, 26, 11, 24, 6)]
names(inter_para) <- c("REARING_NUMBER", "date", "taxon_sp_1", "taxon_sp_2", "value")
inter_para[, "taxon_sp_2"] <- word(inter_para[, "taxon_sp_2"], start = 1, end = 2)
inter_para["attr"] <- "Number of parasited galls"

Kolpelke_2017_inter <- rbind(inter_galler, inter_para)
rm(inter_galler, inter_para)


#------------------------------
# Set taxo_back and taxon table
#------------------------------
# Create taxo_back_df

## Get Unique taxon of data
taxon <- c(unique(Kolpelke_2017_inter[, "taxon_sp_1"]), 
           unique(Kolpelke_2017_inter[, "taxon_sp_2"]))

### Remove sp

taxo_back <- vector()

for (i in 1:length(taxon)) {

  if(((str_detect(taxon[i], "[:digit:]") == TRUE || str_detect(taxon[i], "[:punct:]") == TRUE) &
       str_detect(taxon[i], "sp") == TRUE) ||
       str_detect(taxon[i], "indet.") == TRUE ||
       str_detect(taxon[i], "sp$") == TRUE) {

    taxo_back[i] <- word(taxon[i], start = 1)

  } else {
    taxo_back[i] <- taxon[i]
  }
}

taxo_back <- unique(taxo_back)


## Select only taxa not yet in db

server <- 

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
write.csv2(x = taxo_back_df, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxo_back.csv", row.names = FALSE)

# taxo_back_df <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxo_back.csv", header = TRUE)


# Cleaning




# for(i in 1:length(#unique des sites)){
#
#}




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
# Kolpelke_2017 1
#------------------------------

# Create taxons_df
taxon <- c(as.vector(unique(Kolpelke_2017_I$sp_taxon_2)), as.vector(unique(Kolpelke_2017_I$sp_taxon_1)))

taxons_df1 <- data.frame(taxon, NA)
names(taxons_df1) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df1)) {

  if(((str_detect(taxons_df1[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df1[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxons_df1[i, 1], "sp") == TRUE) ||
       str_detect(taxons_df1[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxons_df1[i, 1], "sp$") == TRUE){

    taxons_df1[i, 2] <- word(taxons_df1[i, 1], start = 1)

  } else {
    taxons_df1[i, 2] <- as.character(taxons_df1[i, 1])
  }
}

# Set metadata
networks <- list(name               = "Kolpelke_2017_I",
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

# taxon_df1 <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_taxons.csv", header = TRUE)
# Kolpelke_2017_I <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_inter.csv", header = TRUE)

# POST table
POST_environments(enviro1, attr1)
POST_networks(networks, enviro = enviro1)
POST_taxons(taxons_df1)
POST_interactions(Kolpelke_2017_I, enviro = enviro1, attr_inter)

# Writing taxon and interaction table
write.csv2(x = taxons_df1, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_taxons.csv", row.names = FALSE)
write.csv2(x = Kolpelke_2017_I, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_inter.csv", row.names = FALSE)



#------------------------------
# Kolpelke_2017 2
#------------------------------

# Create taxons_df
taxon <- c(as.vector(unique(Kolpelke_2017_II$sp_taxon_2)), as.vector(unique(Kolpelke_2017_II$sp_taxon_1)))

taxons_df2 <- data.frame(taxon, NA)
names(taxons_df2) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df2)) {

  if(((str_detect(taxons_df2[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df2[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxons_df2[i, 1], "sp") == TRUE) ||
       str_detect(taxons_df2[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxons_df2[i, 1], "sp$") == TRUE){

    taxons_df2[i, 2] <- word(taxons_df2[i, 1], start = 1)

  } else {
    taxons_df2[i, 2] <- as.character(taxons_df2[i, 1])
  }
}

# Set metadata
networks <- list(name             = "Kolpelke_2017_II",
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

# taxon_df2 <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_taxons.csv", header = TRUE)
# Kolpelke_2017_II <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_inter.csv", header = TRUE)

# POST table
POST_environments(enviro2, attr1)
POST_networks(networks, enviro = enviro2)
POST_taxons(taxons_df2)
POST_interactions(Kolpelke_2017_II, enviro = enviro2, attr_inter)

# Writing taxon and interaction table
write.csv2(x = taxons_df2, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_taxons.csv", row.names = FALSE)
write.csv2(x = Kolpelke_2017_II, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_inter.csv", row.names = FALSE)



#------------------------------
# Kolpelke_2017 3
#------------------------------

# Create taxons_df
taxon <- c(as.vector(unique(Kolpelke_2017_III$sp_taxon_2)), as.vector(unique(Kolpelke_2017_III$sp_taxon_1)))

taxons_df3 <- data.frame(taxon, NA)
names(taxons_df3) <- c("original_name", "name_clear")

for (i in 1:nrow(taxons_df3)) {

  if(((str_detect(taxons_df3[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df3[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxons_df3[i, 1], "sp") == TRUE) ||
       str_detect(taxons_df3[i, 1], "n\\.i\\.") == TRUE ||
       str_detect(taxons_df3[i, 1], "sp$") == TRUE){

    taxons_df3[i, 2] <- word(taxons_df3[i, 1], start = 1)

  } else {
    taxons_df3[i, 2] <- as.character(taxons_df3[i, 1])
  }
}

# Set metadata
networks <- list(name             = "Kolpelke_2017_III",
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

# taxon_df3 <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_taxons.csv", header = TRUE)
# Kolpelke_2017_III <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_inter.csv", header = TRUE)

# POST table
POST_environments(enviro3, attr1)
POST_networks(networks, enviro = enviro3)
POST_taxons(taxons_df3)
POST_interactions(Kolpelke_2017_III, enviro = enviro3, attr_inter)

# Writing taxon and interaction table
write.csv2(x = taxons_df3, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_taxons.csv", row.names = FALSE)
write.csv2(x = Kolpelke_2017_III, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_inter.csv", row.names = FALSE)

rm(taxon, lat, lon, srid, attr_inter, attr1, refs, users, enviro1, enviro2, enviro3, datasets, traits, networks, inter, taxons_df1, taxons_df2, taxons_df3, taxo_back_df, Kolpelke_2017_I, Kolpelke_2017_II, Kolpelke_2017_III)
