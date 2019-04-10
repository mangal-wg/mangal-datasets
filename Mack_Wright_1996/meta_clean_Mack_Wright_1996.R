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

lat  <- -6.7233
lon  <- 145.0933
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Bird-fruit interaction",
                   table_owner = "interactions",
                   description = "Presence-absence of interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1071/mu9960089",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://www.tandfonline.com/doi/abs/10.1071/MU9960089",
             data_url  = "http://www.web-of-life.es/map.php",
             author    = "Mack and Wright",
             year      = "1996",
             bibtex    = "@article{Mack_1996, doi = {10.1071/mu9960089}, url = {https://doi.org/10.1071%2Fmu9960089}, year = 1996, month = {jun}, publisher = {Informa {UK} Limited}, volume = {96}, number = {2} pages = {89--101}, author = {Andrew L. Mack and Debra D. Wright}, title = {Notes on Occurrence and Feeding of Birds at Crater Mountain Biological Research Station, Papua New Guinea}, journal = {Emu - Austral Ornithology}}")


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


datasets <- list(name        = "Mack_Wright_1996",
                 date        = "1989-01-01",
                 description = "Bird-fruit interaction at Crater Mountain Biological Research Station, Papua New Guinea",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Mack_Wright_1996",
                 date             = "1989-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Bird-fruit interaction at Crater Mountain Biological Research Station, Papua New Guinea",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "1989-01-01",
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
# Open file
FW_name <- read.csv2(file = "mangal-datasets-ben/Ones_done_before_Patrick/Mack_Wright_1996/raw/M_SD_018.csv", header = TRUE, sep = ",")

# Melt df
FW_name <- melt(FW_name, id.vars = c(1), na.rm = TRUE)
names(FW_name) <- c("sp_taxon_1", "sp_taxon_2", "value")

#remove uncessary string
without_str2 <- str_remove_all(FW_name$sp_taxon_2, '\\.M_SD_[:digit:]+')
without_str2 <- gsub('\\.', ' ', without_str2)
without_str1 <- str_remove_all(FW_name$sp_taxon_1, '\\M_SD_[:digit:]+')
FW_name[,2] <- without_str2
FW_name[,1] <- without_str1

# Remove interaction value = 0 (no interaction)
FW_name <- subset(FW_name, FW_name$value != 0)
FW_name$sp_taxon_1 <- str_replace(FW_name$sp_taxon_1, "\\.", "")

FW_name <- FW_name[,c(2,1,3)]
FW_name <- `colnames<-`(FW_name, c("sp_taxon_1", "sp_taxon_2", "value"))

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data

taxa <- c(as.vector(unique(FW_name$sp_taxon_2)), as.vector(unique(FW_name$sp_taxon_1)))

### Check for spelling mistakes... ###
taxa_resolve <- gnr_resolve(taxa, canonical = T, best_match_only = T)
temp <- unlist(attributes(taxa_resolve)$not_known)

if(length(temp) != 0){
  sp_not_known <- data.frame(user_supplied_name=unlist(attributes(taxa_resolve)$not_known) , submitted_name=unlist(attributes(taxa_resolve)$not_known), matched_name=unlist(attributes(taxa_resolve)$not_known), NA, NA)
  names(sp_not_known) <- names(taxa_resolve)
  sp_not_known$submitted_name <-  paste0(str_to_upper(str_extract(sp_not_known$user_supplied_name, ".{1}")), str_remove(sp_not_known$user_supplied_name, ".{1}"))
  sp_not_known$matched_name2 <- sp_not_known$submitted_name
  taxa_resolve <- rbind(taxa_resolve, sp_not_known)
}


for (i in 1:length(taxa_resolve$matched_name2)) {
  
  if(((str_detect(taxa_resolve[i,5], "[:digit:]") == TRUE || str_detect(taxa_resolve[i,5], "[:punct:]") == TRUE) &
      str_detect(taxa_resolve[i,5], "sp") == TRUE) ||
     str_detect(taxa_resolve[i,5], "n\\.i\\.") ||
     str_detect(taxa_resolve[i,5], "sp$")){
    
    taxa_resolve[i,5] <- word(taxa_resolve[i,5], start = 1)
    
  } else {
    taxa_resolve[i,5] <- taxa_resolve[i,5]
  }
}

# Create taxa_df
taxons_df <- data.frame(taxa_resolve$user_supplied_name, taxa_resolve$matched_name2, stringsAsFactors = F)
names(taxons_df) <- c("original_name", "name_clear")
taxons_df$name_clear <- str_remove_all(taxons_df$name_clear, '\\s\\(.*\\)$')



### Creating taxa_back
taxa_back <- taxons_df$name_clear
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
taxa_back_df[11,4] <- "21507"
taxa_back_df[12,4] <- "21505"
taxa_back_df[16,4] <- "564964"
taxa_back_df[24,4] <- "559439"
taxa_back_df[26,4] <- "177390"
taxa_back_df[30,4] <- "177339"
taxa_back_df[34,4] <- "559552"
taxa_back_df[38,4] <- "177401"
taxa_back_df[40,4] <- "561065"
taxa_back_df[41,4] <- "554716"
taxa_back_df[45,4] <- "177334"
taxa_back_df[46,4] <- "676916"

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Mack_Wright_1996/data/Mack_Wright_1996_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxons_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Mack_Wright_1996/data/Mack_Wright_1996_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Mack_Wright_1996/data/Mack_Wright_1996_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attribute(attr = attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
POST_ref(ref = refs)
POST_users(users = users)
# POST_environment(enviro, attr_##)
POST_dataset(dataset = datasets, users = users, ref = refs)
POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
POST_taxonomy(taxo = taxa_back_df)
POST_node(node_df = taxons_df, network = networks)
# POST_traits(trait_df)
POST_interaction(inter_df = FW_name, attr = attr_inter, inter = inter, users = users, network = networks, enviro = enviro)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrice.interaction, FW_name, taxa_back_df, taxa_resolve, i, path, server, taxa, temp, without_., without_str)
