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

attr_gall <- list(name         = "Number of galls",
                   table_owner = "interactions",
                   description = "Number of galls",
                   unit        = "NA")


attr_paras <- list(name       = "Number of parasited galls",
                  table_owner = "interactions",
                  description = "Number of parasited galls",
                  unit        = "NA")


attr_altitude <- list(name        = "Altitude of the site",
                      table_owner = "environments",
                      description = "Altitude of the site",
                      unit        = "meters") 


attr_Endo_Ecto <- list(name       = "Endo/Ecto",
                      table_owner = "traits",
                      description = "Type of parasitism (Endoparasitisim or Ectoparasitism",
                      unit        = "NA")


attr_Koino_Idio <- list(name      = "Koino/Idio",
                      table_owner = "traits",
                      description = "Is the parasite Koinobiont or Idiobiont",
                      unit        = "NA")


attr_stage <- list(name        = "stage of the host",
                   table_owner = "traits",
                   description = "Stage of the host at time of parasitism",
                   unit        = "NA")


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
rm(inquiline, V)

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
# Set trait table
#------------------------------

# isolate galler trait
trait_galler <- merge(df_inter[, c(1, 3)], df_galler[, c(1, 4, 7)], by = "RGALLER")
trait_galler <- trait_galler[, -1]
trait_galler["name"] <- "gall type"
names(trait_galler) <- c("REARING_NUMBER", "taxon", "value", "name")

# isolate parasite trait
trait_parasit <- merge(df_inter[, c(1, 4)], df_parasit[, c(1, 8, 9, 10, 11)], by = "RPAR")
trait_parasit$FULL_NAME <- word(trait_parasit$FULL_NAME, 1, 2)
trait_parasit <- trait_parasit[, -1]

  # Replace code by original names
  trait_parasit$`ENDO/ECTO`[trait_parasit$`ENDO/ECTO`=="Endo"] <- "Endoparasitism"
  trait_parasit$`ENDO/ECTO`[trait_parasit$`ENDO/ECTO`=="Ecto"] <- "Ectoparasitism"
  trait_parasit$`KOINO/IDIO`[trait_parasit$`KOINO/IDIO`=="Koino"] <- "Koinobiont"
  trait_parasit$`KOINO/IDIO`[trait_parasit$`KOINO/IDIO`=="Idio"] <- "Idiobiont"
  trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`[trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`=="1INSTAR"] <- "1st instar larvae"
  trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`[trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`=="LINSTAR"] <- "later instar larvae"
  trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`[trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`=="COCOON"] <- "cocoons"
  trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`[trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`=="EGG"] <- "eggs"
  trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`[trait_parasit$`1INSTAR/LINSTAR/COCOON/EGG`=="1INSTAR/COCOON"] <- "1st instar larvae or eggs"

names(trait_parasit) <- c("REARING_NUMBER", "Endo/Ecto", "Koino/Idio", "stage of the host", "taxon")
trait_parasit <- melt(trait_parasit, id.vars = c("REARING_NUMBER", "taxon"), na.rm = TRUE)
trait_parasit[which(trait_parasit$value==""), 4] <- NA
trait_parasit <- na.omit(trait_parasit)
names(trait_parasit) <- c("REARING_NUMBER", "taxon", "name", "value")

# write traits df
write.csv2(x = trait_galler, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_trait_galler.csv", row.names = FALSE)
write.csv2(x = trait_parasit, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_trait_parasit.csv", row.names = FALSE)

#------------------------------
# Set network table
#------------------------------

network_df <- df_site

# Select unique network, give a number
unique_network <- network_df[, c(3, 4, 5, 6)]
unique_network <- unique(unique_network)
unique_network["number"] <- c(1:nrow(unique_network))

# Merge, row number as unique network key
network_df <- merge(network_df, unique_network, by = c("LEG", "COUNTRY", "REGION", "SITE"))

# Replicate date and number
network_df <- cbind(network_df, network_df[, c("number", "LEG")])

# Create name and description column
network_df["X"] <- "Kolpelke"
network_df <- unite(network_df, name, c("X", number), sep = " ")
network_df <- unite(network_df, description, c("LEG", "COUNTRY", "REGION", "SITE"), sep = ", ")

# Create enviro / Remove unused row and change names
enviro_df <- network_df[, c(4, 5, 6, 8, 9)]

network_df <- network_df[, -c(3, 6)]
names(network_df) <- c("description", "REARING_NUMBER", "lat", "lon", "name", "number", "date")

# writing network_df
write.csv2(x = network_df, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_network_df.csv", row.names = FALSE)

#------------------------------
# Set environment table
#------------------------------

enviro_df["name"] <- "elevation"
names(enviro_df) <- c("lat", "lon", "value", "number", "date", "name")
enviro_df <- unique(enviro_df)

# writing enviro_df
write.csv2(x = enviro_df, file = "importation_mangal/Kolpelke_2017/data/Kolpelke_2017_enviro_df.csv", row.names = FALSE)


Kolpelke_2017_inter <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_inter.csv", header = TRUE, sep = ";")
taxa_back_df <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxa_back.csv", header = TRUE, sep = ";")
taxa_df <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_taxa_df.csv", header = TRUE, sep = ";")
enviro_df <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_enviro_df.csv", header = TRUE, sep = ";")
trait_galler <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_trait_galler.csv", header = TRUE, sep = ";")
trait_parasit <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_trait_parasit.csv", header = TRUE, sep = ";")
network_df <- read.csv2("importation_mangal/Kolpelke_2017/data/Kolpelke_2017_network_df.csv", header = TRUE, sep = ";")


#------------------------------
  # POST commun table
#------------------------------
POST_attribute(attr_gall)
POST_attribute(attr_paras)
POST_attribute(attr_altitude)
POST_attribute(attr_Endo_Ecto)
POST_attribute(attr_Koino_Idio)
POST_attribute(attr_stage)

POST_ref()

POST_user()

POST_dataset()

POST_taxa_back()

# GET_fkey for the attribute id in the interaction table
Kolpelke_2017_inter["attr_id"] <- NA
for (i in 1:nrow(Kolpelke_2017_inter)) {
  Kolpelke_2017_inter[i, "attr_id"] <- GET_fkey("attribute", c("name", "unit"), c(Kolpelke_2017_inter[i, "attr"], NA))
}

#------------------------------
 # Injection loop : Network by network
#------------------------------

# First loop
for (i in 1:max(network_df$number)){
  
  # Subset table to select data from i dataset
  network_temp <- subset(network_df, network_df$number == 1)
      rearing_number <- as.vector(network_temp$REARING_NUMBER)
  network_temp <- network_temp[1, ]
      
  enviro_temp  <- subset(enviro_df, enviro_df$number == 1)
  
  # Subset table to match rearing_number of the i dataset
  inter_temp     <- subset(Kolpelke_2017_inter, Kolpelke_2017_inter$REARING_NUMBER %in% rearing_number)
 
  taxa_temp     <- subset(taxa_df, taxa_df$REARING_NUMBER %in% rearing_number)
  taxa_temp     <- taxon_temp[!duplicated(taxon_temp[,"original_name"]),]
   
  t_galler_temp  <- subset(subset(trait_galler, trait_galler$REARING_NUMBER %in% rearing_number), duplicated(taxon))
 
  t_parasit_temp <- subset(trait_parasit, trait_parasit$REARING_NUMBER %in% rearing_number)
  t_parasit_temp <- t_parasit_temp[!duplicated(t_parasit_temp[,c("taxon", "name")]),]

  # Set metadata
  
  network <- list(date             = as.character(network_temp[1, 7]),
                  lat              = network_temp[1, 3],
                  lon              = network_temp[1, 4],
                  srid             = srid,
                  description      = as.character(network_temp[1, 1]),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  enviro <- list(name  = enviro_temp[1, 6],
                 lat   = enviro_temp[1, 1],
                 lon   = enviro_temp[1, 2],
                 srid  = srid,
                 date  = as.character(enviro_temp[1, 5]),
                 value = enviro_temp[1, 3])
  

  inter <- list(taxon_1_level = "population",
                taxon_2_level = "population",
                date          = as.character(inter_temp[1, 2]),
                direction     = "directed",
                method        = "Field observation",
                description   = "null",
                public        = TRUE,
                lat           = inter_temp[1, 7],
                lon           = inter_temp[1, 8],
                srid          = srid)

  # Inject table 
  
  POST_environment(enviro, attr_altitude)
  POST_network(network, enviro = enviro)
  POST_taxon(taxa_temp)
  POST_trait(t_galler_temp)
  POST_trait(t_parasit_temp)
  POST_interaction(inter_temp, enviro = enviro_temp)
  
}

rm(list = ls())
