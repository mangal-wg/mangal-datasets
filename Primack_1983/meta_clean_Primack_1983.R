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


srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Pollinator recorded on a flower",
                   table_owner = "interactions",
                   description = "Presence or absence of interaction",
                   unit        = "NA")

refs <- list(doi       = "10.1080/0028825X.1983.10428561",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://www.tandfonline.com/doi/pdf/10.1080/0028825X.1983.10428561",
             data_url  = "http://www.web-of-life.es/map.php?type=7",
             author    = "Richard B. Primack",
             year      = "1983",
             bibtex    = "@article{Primack_1983, doi = {10.1080/0028825x.1983.10428561}, url = {https://doi.org/10.1080%2F0028825x.1983.10428561}, year = 1983, month = {jul}, publisher = {Informa {UK} Limited}, volume = {21}, number = {3}, pages = {317--333}, author = {Richard B. Primack}, title = {Insect pollination in the New Zealand mountain flora}, journal = {New Zealand Journal of Botany}}")


users <- list(name         = "Benjamin Mercier",
              email        = "Benjamin.b.mercier@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "user")


# enviro <- list(name  = "attribute name",
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)


datasets <- list(name        = "Primack_1983",
                 date        = "1976-12-01",
                 description = "Mountain flora-pollinator interactions in 3 different sites, New-Zealand",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


#------------------------------
  # Cleaning matrix
#------------------------------
filenames <- list.files("mangal-datasets-ben/Ones_done_before_Patrick/Primack_1983/raw", pattern="*.csv", full.names=TRUE)
FW_name <- lapply(filenames, read.csv)
#M_PL_027=Arthurs Pass, M_PL_028=Cass, M_PL_029=Craigieburn
names(FW_name) <- c('Arthurs Pass','Cass','Craigieburn')

# Melt df
for(i in 1:length(FW_name)){
  df <- data.frame()
  df <- FW_name[[i]]
  df <- melt(df, id.vars = c(1), na.rm = TRUE)
  names(df) <- c("sp_taxon_1", "sp_taxon_2", "value")
  without_str2 <- str_remove_all(df$sp_taxon_2, '\\.M_PL_[:digit:]+')
  without_str1 <- str_remove_all(df$sp_taxon_1, '\\M_PL_[:digit:]+')
  without_str2 <- gsub('\\.', ' ', without_str2)
  df[,1] <- without_str1
  df[,2] <- without_str2
  #df_temp <- data.frame(network = rep(names(FW_name)[i], times = nrow(df))) #merci clement pour cette ligne
  #df <- cbind(df_temp, df)
  df <- subset(df, df$value != 0)
  FW_name[[i]] <- df
}
FW_name <- map(FW_name, ~.x[,c(2,1,3)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))



#------------------------------
# Set taxo_back and taxon table
#------------------------------
# Create taxo_back_df

## Get Unique taxon of data
taxa <- FW_name
for(i in 1:length(taxa)){
  df <- data.frame()
  df <- taxa[[i]]
  df <- df[,-3]
  df <- stack(df)
  df <- df[,-2]
  df <- unique(df)
  taxa[[i]] <- df
}
for(i in 1:length(taxa)){
  df <- data.frame()
  df <- as.data.frame(taxa[[i]], stringsAsFactors = F)
  colnames(df) <- "value"
  taxa_resolve <- gnr_resolve(df$value, canonical = T, best_match_only = T)
  temp <- unlist(attributes(taxa_resolve)$not_known)
  if(length(temp) != 0){
    sp_not_known <- data.frame(user_supplied_name=unlist(attributes(taxa_resolve)$not_known) , submitted_name=unlist(attributes(taxa_resolve)$not_known), matched_name2=unlist(attributes(taxa_resolve)$not_known), NA, NA)
    names(sp_not_known) <- names(taxa_resolve)
    sp_not_known$submitted_name <-  paste0(str_to_upper(str_extract(sp_not_known$user_supplied_name, ".{1}")), str_remove(sp_not_known$user_supplied_name, ".{1}"))
    sp_not_known$matched_name2 <- sp_not_known$submitted_name
    taxa_resolve <- rbind(taxa_resolve, sp_not_known)
    
  }
  df <- left_join(df, taxa_resolve, by = c('value' = 'user_supplied_name'))
  df <- df[,-c(2,3,4)]
  names(df) <- c('original_name', 'name_clear')
  taxa[[i]] <- df
}
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Argirophenga sp1", "Argirophenga", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Proscission sp1", "Proscissio", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Musc  sp1", "Musca", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Appis mellifera", "Apis mellifera", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Sawpogon proximus", "Saropogon proximus", name_clear)))
taxa <- map(taxa, ~mutate(.x, name_clear = ifelse(name_clear == "Brachyome sinclairii", "Brachycome sinclairii", name_clear)))


## Remove sp
taxa_back <- do.call(rbind, taxa) 
taxa_back <- taxa_back$name_clear
taxa_back <- unique(taxa_back)


## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- data.frame()


for (i in 1:length(taxa_back)) {
  
  path <- modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", str_replace_all(taxa_back[i], " ", "%20")))
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
taxa_back_df[33,4] <- "141076"
taxa_back_df[131,4] <- "941555"
taxa_back_df[159,4] <- "520258"
taxa_back_df[168,4] <- "501966"
taxa_back_df[176,4] <- "33585"

#------------------------------
# Set traits table
#------------------------------

# traits_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_traits.csv", header = TRUE)

# traits_df <- melt(traits_df, id.vars = c("taxa"), na.rm = TRUE)
# names(traits_df) <- c("taxa", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Primack_1983/data/Primack_1983_taxa_back.csv", row.names = FALSE)
saveRDS(taxa, file = "mangal-datasets-ben/Ones_done_before_Patrick/Primack_1983/data/Primack_1983_taxa.csv")
saveRDS(FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Primack_1983/data/Primack_1983_inter.csv")
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# traits_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_traits.csv", header = TRUE)

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
#------------------------------<
meta <- data.frame(c('Arthurs pass', 'Cass', 'Craigieburn'), c(-42.95, -43.0282, -43.0995), c(171.5667, 171.7847, 171.7202), stringsAsFactors = F)
colnames(meta) <- c('name', 'lat', 'lon')
for (i in 1:3) {
  
  
  networks <- list(name             = paste0("Primack_1983_",i),
                  date             = "1976-12-01",
                  lat              = meta[i, 2],
                  lon              = meta[i, 3],
                  srid             = srid,
                  description      = paste0("Mountain flora-pollinator interactions at ", meta[i, 1],", New-Zealand"),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  inter <- list(date          = "1976-12-01",
                direction     = "directed",
                type          = "mutualism",
                method        = "Observation and capturing individuals",
                description   = 'Touched the anthers and stigmas',
                public        = TRUE,
                lat           = meta[i, 2],
                lon           = meta[i, 3],
                srid          = srid)
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxa[[i]], network = networks)
  
  POST_interaction(inter_df = FW_name[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
