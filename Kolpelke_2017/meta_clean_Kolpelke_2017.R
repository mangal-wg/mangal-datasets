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

# Must fill all CAP fields; null fields optional

attr_gall <- list(name        = "Number of galls",
                   table_owner = "interactions",
                   description = "DESCRIPTION",
                   unit        = "NA")

attr_paras <- list(name        = "Number of parasited galls",
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
library(magrittr)

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
rm(inter_galler, inter_para, site_inter)

# Add localistaiton
coord <- subset(df_site, select = c("REARING_NUMBER", "NDECDEG", "EDECDEG"))
names(coord) <- c("REARING_NUMBER", "lat", "lon")
Kolpelke_2017_inter <- merge(Kolpelke_2017_inter, coord, by = "REARING_NUMBER")
Kolpelke_2017_inter <- na.omit(Kolpelke_2017_inter)
rm(coord)

# Add type of interaction
Kolpelke_2017_inter[, "type"] <- "parasitism"
inquiline <- as.vector(t(word(subset(df_parasit, df_parasit$`P/I` == "I" ,select = "FULL_NAME")[, "FULL_NAME"], start = 1, end = 2)))

V <- which(Kolpelke_2017_inter[, "taxon_sp_2"] %in% inquiline)
Kolpelke_2017_inter[V, "type"] <- "commensalism"

# write interaction table
write.csv2(x = Kolpelke_2017_inter, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_inter.csv", row.names = FALSE)

#------------------------------
# Set taxa_back table
#------------------------------

## Get Unique taxa of data
taxa <- c(unique(Kolpelke_2017_inter[, "taxon_sp_1"]), 
           unique(Kolpelke_2017_inter[, "taxon_sp_2"]))
taxa <- taxa[!is.na(taxa)]

### Remove sp

taxa_back <- vector()

for (i in 1:length(taxa)) {

  if(((str_detect(taxa[i], "[:digit:]") == TRUE || str_detect(taxa[i], "[:punct:]") == TRUE) == TRUE &
       str_detect(taxa[i], "sp") == TRUE) ||
       str_detect(taxa[i], "indet\\.") == TRUE ||
       str_detect(taxa[i], "sp$") == TRUE ||
       str_detect(taxa[i], "\\?") == TRUE ||
       str_detect(taxa[i], "nr\\.") == TRUE){
    
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

# Writing taxa_back_df
write.csv2(x = taxa_back_df, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxa_back.csv", row.names = FALSE)

#------------------------------
# Set taxa table
#------------------------------

taxon_sp_1 <- subset(Kolpelke_2017_inter, select = c("REARING_NUMBER", "taxon_sp_1"))
taxon_sp_2 <- subset(Kolpelke_2017_inter, select = c("REARING_NUMBER", "taxon_sp_2"))
colnames(taxon_sp_1) = colnames(taxon_sp_2)

taxa_df <- rbind(taxon_sp_1, taxon_sp_2)
colnames(taxa_df) <- c("REARING_NUMBER", "original_name")
taxa_df <- unique(taxa_df)
taxa_df <- na.omit(taxa_df)
taxa_df["name_clear"] <- NA



for (i in 1:nrow(taxa_df)) {
  
  if(((str_detect(taxa_df[i, 2], "[:digit:]") == TRUE || str_detect(taxa_df[i, 1], "[:punct:]") == TRUE) &
       str_detect(taxa_df[i, 2], "sp") == TRUE) ||
       str_detect(taxa_df[i, 2], "indet\\.") == TRUE ||
       str_detect(taxa_df[i, 2], "\\.") == TRUE ||
       str_detect(taxa_df[i, 2], "sp$") == TRUE){
    
    taxa_df[i, 3] <- word(taxa_df[i, 2], start = 1)
    
  } else {
    taxa_df[i, 3] <- as.character(taxa_df[i, 2])
  }
}

rm(taxon_sp_1, taxon_sp_2)

# Writing taxa_df
write.csv2(x = taxa_df, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxa_df.csv", row.names = FALSE)

#------------------------------
# Set environment table
#------------------------------

enviro_df <- df_site[, c(1,3,7,8,9)]
enviro_df["name"] <- "elevation"
names(enviro_df) <- c("REARING_NUMBER", "date", "la", "lon", "value", "name")
  
# writing enviro_df
write.csv2(x = enviro_df, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_enviro_df.csv", row.names = FALSE)

#------------------------------
# Set traits table
#------------------------------

trait_df <- df_inter[, c(1, 3, 4)]
trait_df <- merge(trait_df, df_galler[, c(1, 7)], by = "RGALLER")
df_parasit <- rbind(df_parasit, rep("none", 10)) # Il faut ajouter la line "none" dans df_parasit parce que certaine interaction n<ont pas de parasitoide, donc on a besoin de ca pouf faire le merge
trait_df <- merge(trait_df, df_parasit[, c(1, 8, 9, 10)], by = "RPAR") # Quand on merge on perd des donnees

# Next : extraire des tableaux des galleurs et des parasitoides. Il va falloir faire deux tableaux car galleur 1 trait et parasitoide 3 traits. 

# Kolpelke_2017_inter <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_inter.csv", header = TRUE, sep = ";")
# taxa_back_df <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxa_back.csv", header = TRUE, sep = ";")
# taxa_df <- read.csv2(""importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxa_df.csv", header = TRUE, sep = ";")
# enviro_df <- read.csv2(""importation_mangal/Kolpelke_2017/data/Kolpelke_2017_enviro_df.csv", header = TRUE, sep = ";")




#------------------------------
  # POST commun table
#------------------------------
POST_attribute(attr_inter)

POST_attribute(attr1)

POST_ref()

POST_user()

POST_dataset()

POST_taxa_back()

# POST_traits(traits_df)

#------------------------------
# Kolpelke_2017 1
#------------------------------

# Create taxa_df
taxa <- c(as.vector(unique(Kolpelke_2017_I$sp_taxon_2)), as.vector(unique(Kolpelke_2017_I$sp_taxon_1)))

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
network <- list(name               = "Kolpelke_2017_I",
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

# taxa_df1 <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_taxa.csv", header = TRUE)
# Kolpelke_2017_I <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_inter.csv", header = TRUE)

# POST table
POST_environment(enviro1, attr1)
POST_network(networks, enviro = enviro1)
POST_taxon(taxa_df1)
POST_interaction(Kolpelke_2017_I, enviro = enviro1, attr_inter)

# Writing taxa and interaction table
write.csv2(x = taxa_df1, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_taxa.csv", row.names = FALSE)
write.csv2(x = Kolpelke_2017_I, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_I_inter.csv", row.names = FALSE)



#------------------------------
# Kolpelke_2017 2
#------------------------------

# Create taxa_df
taxa <- c(as.vector(unique(Kolpelke_2017_II$sp_taxon_2)), as.vector(unique(Kolpelke_2017_II$sp_taxon_1)))

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
network <- list(name             = "Kolpelke_2017_II",
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

# taxa_df2 <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_taxa.csv", header = TRUE)
# Kolpelke_2017_II <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_inter.csv", header = TRUE)

# POST table
POST_environment(enviro2, attr1)
POST_network(networks, enviro = enviro2)
POST_taxon(taxa_df2)
POST_interaction(Kolpelke_2017_II, enviro = enviro2, attr_inter)

# Writing taxa and interaction table
write.csv2(x = taxa_df2, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_taxa.csv", row.names = FALSE)
write.csv2(x = Kolpelke_2017_II, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_II_inter.csv", row.names = FALSE)



#------------------------------
# Kolpelke_2017 3
#------------------------------

# Create taxa_df
taxa <- c(as.vector(unique(Kolpelke_2017_III$sp_taxon_2)), as.vector(unique(Kolpelke_2017_III$sp_taxon_1)))

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
network <- list(name             = "Kolpelke_2017_III",
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

# taxa_df3 <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_taxa.csv", header = TRUE)
# Kolpelke_2017_III <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_inter.csv", header = TRUE)

# POST table
POST_environment(enviro3, attr1)
POST_network(networks, enviro = enviro3)
POST_taxon(taxa_df3)
POST_interaction(Kolpelke_2017_III, enviro = enviro3, attr_inter)

# Writing taxa and interaction table
write.csv2(x = taxa_df3, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_taxa.csv", row.names = FALSE)
write.csv2(x = Kolpelke_2017_III, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_III_inter.csv", row.names = FALSE)

rm(taxa, lat, lon, srid, attr_inter, attr1, refs, users, enviro1, enviro2, enviro3, datasets, traits, networks, inter, taxa_df1, taxa_df2, taxa_df3, taxa_back_df, Kolpelke_2017_I, Kolpelke_2017_II, Kolpelke_2017_III)
