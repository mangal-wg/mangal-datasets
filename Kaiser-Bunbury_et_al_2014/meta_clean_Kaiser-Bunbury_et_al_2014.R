# Set libraries
library(reshape2)
library(tidyr)
#library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)

library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- -4.666667
lon  <- 55.43333
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Number of visit by a pollinator taxon",
                   table_owner = "interactions",
                   description = "The mean visitation frequency per hour  of a pollinator taxon on a plant taxon",
                   unit        = "Mean number of a pollinator taxon visiting a plant taxon per hour")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1890/14-0024.1 ",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/14-0024.1",
             data_url  = "http://www.web-of-life.es/2.0/map.php",
             author    = "KAISER-BUNBURY",
             year      = "2014",
             bibtex    = "@article{Kaiser_Bunbury_2014, doi = {10.1890/14-0024.1}, url = {https://doi.org/10.1890%2F14-0024.1}, year = 2014, month = {dec}, publisher = {Wiley}, volume = {95}, number = {12}, pages = {3314--3324}, author = {Christopher N. Kaiser-Bunbury and Diego P. V{\'{a}}zquez and Martina Stang and Jaboury Ghazoul}, title = {Determinants of the microstructure of plant{\textendash}pollinator networks}, journal = {Ecology}}")


users <- list(name         = "Benjamin Mercier",
              email        = "Benjamin.b.mercier@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "User")


# enviro <- list(name  = "attribute name",
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)


datasets <- list(name        = "Kaiser-Bunbury_et_al_2014",
                 date        = "2007-09-01",
                 description = "Plant-pollinator networks on Mahé in the Seychelles, Indian Ocean",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


inter <- list(date          = "2007-09-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
# Cleaning matrix
#------------------------------
library(tidyr) #Cleaning data
library(tibble) # New format of df
library(dplyr) # Manipulate df
library(readr) # Read data
library(forcats) # Factor manipulation
library(purrr) # Fonctionnal programming
library(tm)

# Open file
filenames <- list.files("mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2014/raw", pattern="*.csv", full.names=TRUE)
FW_name <- lapply(filenames, read.csv)
names(FW_name) <- c('M_PL_061_01','M_PL_061_02','M_PL_061_03','M_PL_061_04','M_PL_061_05','M_PL_061_06','M_PL_061_07','M_PL_061_08','M_PL_061_09','M_PL_061_10','M_PL_061_11',
                    'M_PL_061_12','M_PL_061_13','M_PL_061_14','M_PL_061_15','M_PL_061_16','M_PL_061_17','M_PL_061_18','M_PL_061_19','M_PL_061_20','M_PL_061_21',
                    'M_PL_061_22','M_PL_061_23','M_PL_061_24','M_PL_061_25','M_PL_061_26','M_PL_061_27','M_PL_061_28','M_PL_061_29','M_PL_061_30','M_PL_061_31',
                    'M_PL_061_32','M_PL_061_33','M_PL_061_34','M_PL_061_35','M_PL_061_36','M_PL_061_37','M_PL_061_38','M_PL_061_39','M_PL_061_40','M_PL_061_41',
                    'M_PL_061_42','M_PL_061_43','M_PL_061_44','M_PL_061_45','M_PL_061_46','M_PL_061_47','M_PL_061_048')
# Melt df
for(i in 1:length(FW_name)){
  df <- data.frame()
  df <- FW_name[[i]]
  df <- melt(df, id.vars = c(1), na.rm = TRUE)
  names(df) <- c("sp_taxon_1", "sp_taxon_2", "value")
  without_str <- str_remove_all(df$sp_taxon_2, '\\.M_PL_[:digit:]+')
  without_str <- gsub('\\.', ' ', without_str)
  df[,2] <- without_str
  #df_temp <- data.frame(network = rep(names(FW_name)[i], times = nrow(df))) #merci clement pour cette ligne
  #df <- cbind(df_temp, df)
  df <- subset(df, df$value != 0)
  FW_name[[i]] <- df
}
#inter_final <- do.call(rbind, FW_name)
FW_name <- map(FW_name, ~.x[,c(2,1,3)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
#taxa <- c(as.vector(unique(df$sp_taxon_2)), as.vector(unique(df$sp_taxon_1)))
taxa <- FW_name
for(i in 1:length(taxa)){
  df <- data.frame()
  df <- taxa[[i]]
  df <- df[,-3]
  df$sp_taxon_2 <- as.character(df$sp_taxon_2)
  df <- stack(df)
  df <- df[,-2]
  df <- unique(df)
  taxa[[i]] <- df
}

for(i in 1:length(taxa)){
  df <- data.frame()
  df <- as.data.frame(taxa[[i]], stringsAsFactors = F)
  colnames(df) <- "value"
  taxa_resolve <- gnr_resolve(df$value, canonical = F, best_match_only = T)
  temp <- unlist(attributes(taxa_resolve)$not_known)
  if(length(temp) != 0){
    sp_not_known <- data.frame(user_supplied_name=unlist(attributes(taxa_resolve)$not_known) , submitted_name=unlist(attributes(taxa_resolve)$not_known), matched_name=unlist(attributes(taxa_resolve)$not_known), NA, NA)
    names(sp_not_known) <- names(taxa_resolve)
    sp_not_known$submitted_name <-  paste0(str_to_upper(str_extract(sp_not_known$user_supplied_name, ".{1}")), str_remove(sp_not_known$user_supplied_name, ".{1}"))
    sp_not_known$matched_name <- sp_not_known$submitted_name
    taxa_resolve <- rbind(taxa_resolve, sp_not_known)
  }
  df <- left_join(df, taxa_resolve, by = c('value' = 'user_supplied_name'))
  df <- df[,-c(2,4,5)]
  names(df) <- c('original_name', 'name_clear')
  taxa[[i]] <- df
}

### Check for spelling mistakes... ###
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Unidentified Fusellovirus", "Unidentified", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Soulamea terminalioides Baker", "Soulamea terminalioides", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Excoecaria benthamiana Hemsl.", "Excoecaria benthamiana", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Physiphora azurea (Hendel, 1912)", "Physiphora azurea", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Simosyrphus aegyptius (Wiedemann 1830)", "Simosyrphus aegyptius", name_clear)))

## Remove sp
taxa_back <- do.call(rbind, taxa) 
taxa_back <- taxa_back$name_clear
taxa_back <- unique(taxa_back)


## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- data.frame()

for (i in 1:length(taxa_back)) {
  
  path <- modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", str_replace(taxa_back[i], " ", "%20")))
  if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json")))) == 0) {
    
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
taxa_back_df[4,4] <- "766652"
taxa_back_df[58,4] <- "25148"
taxa_back_df[61,4] <- "1025733"

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2014/data/Kaiser-Bunbury_et_al_2014_taxa_back.csv", row.names = FALSE)
saveRDS(taxa, file = "mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2014/data/Kaiser-Bunbury_et_al_2014_taxa.csv")
saveRDS(FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2014/data/Kaiser-Bunbury_et_al_2014_inter.csv")
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

#------------------------------
# POST commun table
#------------------------------
POST_attribute(attr = attr_inter)

POST_ref(ref = refs)

POST_users(users = users)

POST_dataset(dataset = datasets, users = users, ref = refs)

POST_taxonomy(taxo = taxa_back_df)

#------------------------------
# Injection loop : Network by network
#------------------------------
for (i in 1:48) {
  
  
  networks <- list(name             = paste0("Kaiser-Bunbury_et_al_2014_",i),
                   date             = "2007-09-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Plant-pollinator network on Mahé in the Seychelles, Indian Ocean",
                   public           = TRUE,
                   all_interactions = FALSE)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxa[[i]], network = networks)
  
  POST_interaction(inter_df = FW_name[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
