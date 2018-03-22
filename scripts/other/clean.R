# Set libraries
library(RPostgreSQL)
library(jsonlite)
library(httr)
library(taxize)

source('./api.R')

# Cleaning attributes and post it (To have access to attr_id for interactions)
## Change NA to "unspecified"
attributes[which(is.na(attributes["name"])), "name"] <- "unspecified"

## Fill "name", "description" and "unit"
#attributes[, "table_owner"] <- "Vincent"
#attributes[1, "description"] <- "Number of galls on their host"
#attributes[2, "description"] <- "Individuals who interact with another individuals"
#attributes[3, "description"] <- "Visits of a pollinator to a flower"
#attributes[4, "description"] <- "Unspecified"
#attributes[, "unit"] <- "Occurrence"
## Description no appropriate with name. Trying with loop

for (i in 1:nrow(attributes)) {
  if (attributes[i,"name"] == "galls"){
    attributes[i,"description"] <- "Number of galls on their host"}
  if (attributes[i,"name"] == "visits") {
    attributes[i,"description"] <- "Visits of a pollinator to a flower"}
  if (attributes[i,"name"] == "individuals") {
    attributes[i,"description"] <- "Individuals who interact with another individuals"}
  if (attributes[i,"name"] == "unspecified") {
    attributes[i,"description"] <- "Unspecified"}
  }


## "attributes" as a list
attr_list <- list()
for (i in 1:nrow(attributes)) {
  attr_list[[i]] <- as.list(attributes[i,])
}


## post attributes
post("attributes", attr_list)


# Cleaning refs
refs <- replace(refs, refs == "", NA)


## "refs" as a list
refs_list <- list()
for (i in 1:nrow(refs)) {
  refs_list[[i]] <- as.list(refs[i,])
}


# Cleaning users
## Add "type", "orcid", "organization"
users[, "orcid"] <- NA
users[, "organization"] <- NA
users[, "type"] <- "user"

## Paste "first_name" and "last_name" in "name"
users <- tidyr::unite_(users, "name", c("first_name", "last_name"), sep = " ", remove   = TRUE)

## "users" as a list
users_list <- list()
for (i in 1:nrow(users)) {
  users_list[[i]] <- as.list(users[i,])
}


# Cleaning taxa
## "taxons" as a list
taxons_list <- list()
for (i in 1:nrow(taxons)) {
  taxons_list[[i]] <- as.list(taxons[i,])
}

## Retrieving IDs with taxize # DONE ONCE
# id_tsn <- get_ids(taxons$name, db = c("itis"))
# id_ncbi <- get_ids(taxons$name, db = c("ncbi"))
# id_eol <- get_ids(taxons$name, db = c("eol"))
# id_gbif <- get_ids(taxons$name, db = c("gbif"))

## Update taxon table
# id_tsn <- as.integer(readRDS('./taxonomy/itis_taxons.rda'))
# id_ncbi <-  as.integer(readRDS('./taxonomy/ncbi_taxons.rda'))
# taxons$tsn <- id_tsn
# taxons$ncbi <- id_ncbi


# Cleaning datasets
datasets <- replace(datasets, datasets == "", NA)

## "datasets" as a list
datasets_list <- list()
for (i in 1:nrow(datasets)) {
  datasets_list[[i]] <- as.list(datasets[i,])
}


# Cleaning networks
## Create coordinates dataframe
coordinates <- data.frame(cbind(longitude = networks[, "longitude"], latitude =  networks[, "latitude"]))

## Create networks as a list without coordinates
networks_list <- list()
for (i in 1:nrow(networks)) {
  networks_list[[i]] <- as.list(networks[i,-c(which(colnames(networks) == "latitude"), which(colnames(networks) == "longitude"))])
}

## Create geojson (as.vector(t()) to have the real coordinates)
for (i in 1:nrow(coordinates)) {
  networks_list[[i]]$localisation <- list(type = "point", coordinates = c(as.vector(t(coordinates[i, "longitude"])), as.vector(t(coordinates[i, "latitude"]))))
}


# Cleaning interactions
## problem with unique data
## subset(interactions, taxon_1 == 823 & taxon_2 == 676)
interactions <- unique(interactions)
interactions <- interactions[!duplicated(interactions[, c(2:(ncol(interactions)-4))]),]


## Retreive "attributes" for attr_id
con_dev <- dbConnect(PostgreSQL(),user="postgres", dbname="mangal_dev")
attr <- dbGetQuery(con_dev, "SELECT id, name FROM attributes")
interactions[, "attr_id"] <- integer()

## Change NA to "unspecified"
interactions[which(is.na(interactions["units_f"])), "units_f"] <- "unspecified"

## Create coordinates dataframe
coordinates <- data.frame(cbind(longitude = interactions[, "longitude"], latitude =  interactions[, "latitude"]))

## Create interactions as a list without "latitude" and "longitude"
interactions_list <- list()
for (i in 1:nrow(interactions)) {
  ### Change "female" for "F" and "male" for "M"
  if (interactions[i, "taxon_2_sex"] == "female") {
    interactions[i, "taxon_2_sex"] <- "F"
  }
  if (interactions[i, "taxon_2_sex"] == "male") {
    interactions[i, "taxon_2_sex"] <- "M"
  }
  
  ### "interactions" as a list
  interactions_list[[i]] <- as.list(interactions[i,-c(which(colnames(interactions) == "latitude"), which(colnames(interactions) == "longitude"))])
  
  ### get attr_id from "attributes" table
  interactions_list[[i]]$attr_id <- attr[which(attr[,2] == interactions_list[[i]]$units_f), 1]
  
  ### Create geojson (as.vector(t()) to have the real coordinates)
  interactions_list[[i]]$localisation <- list(type = "point", coordinates = c(as.vector(t(coordinates[i, "longitude"])), as.vector(t(coordinates[i, "latitude"]))))
}


############### Compare salix data with salix on v1 #################


# net_id <- subset(networks, id == 452, select = "name")
# net_brute <- subset(df_site, SITE == net_id[1,1], select = "REARING_NUMBER")
# inter_brute <- df_interact[df_interact$REARING_NUMBER %in% net_brute[,1],]
# salix_galler <- df_salix_galler[df_salix_galler$REARING_NUMBER %in% net_brute[,1],]
# site_brute <- df_site[df_site$REARING_NUMBER %in% unique(inter_brute$REARING_NUMBER), ]
# v1 <- subset(x, network_id == 452, select = c(-7, -8, -9, -10, -12, -13, -14, -15, -16, -17, -19, -20))
# v1
# inter_brute