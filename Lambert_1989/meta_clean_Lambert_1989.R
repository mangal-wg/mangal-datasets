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

lat  <- 3.7167
lon  <- 102.2833
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Bird-fruit interaction",
                   table_owner = "interactions",
                   description = "Presence-absence of interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

refs <- list(doi       = "10.1017/s0266467400003850",
             jstor     = "https://www.jstor.org/stable/2559409?seq=1#metadata_info_tab_contents",
             pmid      = "NA",
             paper_url = "https://www.jstor.org/stable/pdf/2559409.pdf?refreqid=excelsior%3A72357767be7dcb8f484b6520a19a615f",
             data_url  = "http://www.web-of-life.es/map.php?type=7",
             author    = "Lambert",
             year      = "1989",
             bibtex    = "@article{Lambert_1989, doi = {10.1017/s0266467400003850}, url = {https://doi.org/10.1017%2Fs0266467400003850}, year = 1989, month = {nov}, publisher = {Cambridge University Press ({CUP})}, volume = {5}, number = {04}, pages = {401--412}, author = {Frank Lambert}, title = {Fig-eating by birds in a Malaysian lowland rain forest}, journal = {Journal of Tropical Ecology}}")


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


datasets <- list(name        = "Lambert_1989",
                 date        = "1984-03-01",
                 description = "Bird-fruit interaction in a lowland rain forest, Malaysia",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Lambert_1989",
                 date             = "1984-03-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Bird-fruit interaction in a lowland rain forest, Malaysia",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "1984-03-01",
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
FW_name <- read.csv2(file = "mangal-datasets-ben/Ones_done_before_Patrick/Lambert_1989/raw/M_SD_016.csv", header = TRUE, sep = ",")

# Melt df
FW_name <- melt(FW_name, id.vars = c(1), na.rm = TRUE)
names(FW_name) <- c("sp_taxon_1", "sp_taxon_2", "value")

#remove uncessary string
without_. <- gsub('.', ' ', FW_name$sp_taxon_2, fixed=T)
FW_name[,2] <- without_.

# Remove interaction value = 0 (no interaction)
FW_name <- subset(FW_name, FW_name$value != 0)

FW_name <- FW_name[,c(2,1,3)]
FW_name <-   `colnames<-`(FW_name, c("sp_taxon_1", "sp_taxon_2", "value"))
#------------------------------
# Set taxo_back and taxon table
#------------------------------
## Get Unique taxon of data
taxa <- c(as.vector(unique(FW_name$sp_taxon_2)), as.vector(unique(FW_name$sp_taxon_1)))


### Check for spelling mistakes... ###
taxa_resolve <- gnr_resolve(taxa, canonical = T, best_match_only = T)
taxa_resolve[58,5] <- "Rhyticeros corrugatus"
temp <- unlist(attributes(taxa_resolve)$not_known)

if(length(temp) != 0){
  sp_not_known <- data.frame(user_supplied_name=unlist(attributes(taxa_resolve)$not_known) , submitted_name=unlist(attributes(taxa_resolve)$not_known), matched_name=unlist(attributes(taxa_resolve)$not_known), NA, NA)
  names(sp_not_known) <- names(taxa_resolve)
  sp_not_known$submitted_name <-  paste0(str_to_upper(str_extract(sp_not_known$user_supplied_name, ".{1}")), str_remove(sp_not_known$user_supplied_name, ".{1}"))
  sp_not_known$matched_name <- sp_not_known$submitted_name
  taxa_resolve <- rbind(taxa_resolve, sp_not_known)
}
# Create taxa_df
taxons_df <- data.frame(taxa, taxa_resolve$matched_name2, stringsAsFactors = F)
names(taxons_df) <- c("original_name", "name_clear")
taxons_df$name_clear <- str_remove_all(taxons_df$name_clear, '\\s\\(.*\\)$')
taxons_df[77,2] <-  'Harpactes diardii'

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
taxa_back_df[27,4] <- "554176"
taxa_back_df[28,4] <- "558996"
taxa_back_df[29,4] <- "559230"
taxa_back_df[32,4] <- "554178"
taxa_back_df[35,4] <- "554186"
taxa_back_df[36,4] <- "562605"
taxa_back_df[37,4] <- "559226"
taxa_back_df[38,4] <- "562457"
taxa_back_df[40,4] <- "554425"
taxa_back_df[42,4] <- "559725"
taxa_back_df[43,4] <- "562607"
taxa_back_df[44,4] <- "177290"
taxa_back_df[47,4] <- "562625"
taxa_back_df[53,4] <- "558497"
taxa_back_df[70,4] <- "562263"
taxa_back_df[73,4] <- "554421"
taxa_back_df[77,4] <- "554510"
taxa_back_df[79,4] <- "562611"
taxa_back_df[82,4] <- "562628"

#------------------------------
# Set traits table
#------------------------------

# traits_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_traits.csv", header = TRUE)

# traits_df <- melt(traits_df, id.vars = c("taxa"), na.rm = TRUE)
# names(traits_df) <- c("taxa", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Lambert_1989/data/Lambert_1989_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxons_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Lambert_1989/data/Lambert_1989_taxons.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Lambert_1989/data/Lambert_1989_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# traits_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_traits.csv", header = TRUE)

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
