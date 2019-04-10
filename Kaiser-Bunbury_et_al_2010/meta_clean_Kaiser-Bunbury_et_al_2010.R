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

lat  <- -20.42636934535798
lon  <- 57.45094299316406
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "number of visit by a pollinator",
                   table_owner = "interactions",
                   description = "Number of individual of a species observed/caught on a flower",
                   unit        = "number of individual")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1111/j.1461-0248.2009.01437.x",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://onlinelibrary.wiley.com/doi/full/10.1111/j.1461-0248.2009.01437.x",
            data_url  = "http://www.web-of-life.es/2.0/map.php",
            author    = "KAISER-BUNBURY",
            year      = "2010",
            bibtex    = "@article{Kaiser_Bunbury_2010, doi = {10.1111/j.1461-0248.2009.01437.x}, url = {https://doi.org/10.1111%2Fj.1461-0248.2009.01437.x}, year = 2010, month = {apr}, publisher = {Wiley}, volume = {13}, number = {4}, pages = {442--452}, author = {Christopher N. Kaiser-Bunbury and Stefanie Muff and Jane Memmott and Christine B. Müller and Amedeo Caflisch}, title = {The robustness of pollination networks to the loss of species and interactions: a quantitative approach incorporating pollinator behaviour}, journal = {Ecology Letters}}")


users <- list(name         = "Benjamin Mercier",
              email        = "Benjamin.b.mercier@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "User")


# enviro <- list(name  = "attribute name",   ### site with exotic and without### 
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)


datasets <- list(name       = "Kaiser-Bunbury_et_al_2010",
                date        = "2003-09-01",
                description = "Plant-pollinator system at two sites in Black River Gorges National Park, Mauritius",
                public      = TRUE)


#trait <- list(date = "1111-11-11")




inter <- list(date          = "2003-09-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation",
              description   = "Pollinators touching the sexual parts of the flowers",
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
filenames <- list.files("mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2010/raw", pattern="*.csv", full.names=TRUE)
FW_name <- lapply(filenames, read.csv)
names(FW_name) <- c('M_PL_060_01','M_PL_060_02','M_PL_060_03','M_PL_060_04','M_PL_060_05','M_PL_060_06','M_PL_060_07','M_PL_060_08','M_PL_060_09','M_PL_060_10','M_PL_060_11',
                    'M_PL_060_12','M_PL_060_13','M_PL_060_14','M_PL_060_15','M_PL_060_16','M_PL_060_17','M_PL_060_18','M_PL_060_19','M_PL_060_20','M_PL_060_21',
                    'M_PL_060_22','M_PL_060_23','M_PL_060_24')
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
  #df_temp <- data.frame(network = rep(names(FW_name)[i], times = nrow(df))) 
  #df <- cbind(df_temp, df)
  df <- subset(df, df$value != 0)
  FW_name[[i]] <- df
}
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
  df <- stack(df)
  df <- df[,-2]
  df <- unique(df)
  taxa[[i]] <- df
}
#taxa <- map(FW_name, ~unique(c(as.vector(.x$sp_taxon_2), as.vector(.x$sp_taxon_1))))
#taxa <- map(taxa, ~`colnames<-`(.x, "value"))

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
taxa[[1]]$name_clear <- str_replace_all(taxa[[1]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Ceratitis rosa Karsch, 1887'='Ceratitis rosa', 'Homoneura quadrivitta (Walker, 1849)'='Homoneura quadrivitta', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata'))
taxa[[1]][20,2] <- 'Homoneura quadrivitta'
taxa[[2]]$name_clear <- str_replace_all(taxa[[2]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Dasytinae Laporte, 1840'= 'Dasytinae Laporte', 'Orthellia Robineau-Desvoidy, 1863'='Orthellia albigena', 'Pelecophora interrupta Ali.'='Pelecophora interrupta'))
taxa[[3]]$name_clear <- str_replace_all(taxa[[3]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Syritta nigrifemorata Macquart, 1842'= 'Syritta nigrifemorata', 'Acalyptrate sp1'='Acalyptrate', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Parnara naso Fabricius, 1798'='Parnara naso'))
taxa[[3]][42,2] <- 'Mauritiobrium undulatum'
taxa[[4]]$name_clear <- str_replace_all(taxa[[4]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Pandanus wiehei Bosser & J.Guého'= 'Pandanus wiehei', 'Parnara naso Fabricius, 1798'='Parnara naso', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Orthellia Robineau-Desvoidy, 1863'='Orthellia albigena', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata'))
taxa[[4]][39,2] <- 'Mauritiobrium undulatum'
taxa[[5]]$name_clear <- str_replace_all(taxa[[5]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Pandanus wiehei Bosser & J.Guého'= 'Pandanus wiehei', 'Parnara naso Fabricius, 1798'='Parnara naso', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Orthellia Robineau-Desvoidy, 1863'='Orthellia albigena', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Calcidoidae sp1'='Calcidoidae', 'Eumolpinae Hope, 1840'='Eumolpinae'))
taxa[[5]][30,2] <- 'Mauritiobrium undulatum'
taxa[[6]]$name_clear <- str_replace_all(taxa[[6]]$name_clear, c('Badula platyphylla (A. DC.) M.J.E. Coode' = 'Badula platyphylla', 'Parnara naso Fabricius, 1798'= 'Parnara naso', 'Scopariinae Guenée, 1854'='Scopariinae', 'Foudia madagascariensis (Linnaeus, 1766)'='Foudia madagascariensis'))
taxa[[6]][62,2] <- 'Badula platyphylla'
taxa[[6]][35,2] <- 'Foudia madagascariensis'
taxa[[7]]$name_clear <- str_replace_all(taxa[[7]]$name_clear, c('Labourdonnaisia calophylloides Bojer' = 'Labourdonnaisia calophylloides', 'Pandanus barkleyi Balf.f.'= 'Pandanus barkleyi', 'Badula platyphylla (A. DC.) M.J.E. Coode'='Badula platyphylla', 'Pandanus wiehei Bosser & J.Guého'='Pandanus wiehei', 'Ceratitis rosa Karsch, 1887'='Ceratitis rosa', 'Brachymyrmex sp1'='Brachymyrmex', 'Parnara naso Fabricius, 1798'='Parnara naso', 'Orthellia Robineau-Desvoidy, 1863'='Orthellia albigena'))
taxa[[7]][64,2] <- 'Badula platyphylla'
taxa[[8]]$name_clear <- str_replace_all(taxa[[8]]$name_clear, c('Labourdonnaisia calophylloides Bojer' = 'Labourdonnaisia calophylloides', 'Pandanus barkleyi Balf.f.'= 'Pandanus barkleyi', 'Tabernaemontana persicariifolia Jacq.'='Tabernaemontana persicariifolia','Brachymyrmex sp1'='Brachymyrmex'))
taxa[[9]]$name_clear <- str_replace_all(taxa[[9]]$name_clear, c('Labourdonnaisia calophylloides Bojer' = 'Labourdonnaisia calophylloides', 'Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Tabernaemontana persicariifolia Jacq.'='Tabernaemontana persicariifolia','Brachymyrmex sp1'='Brachymyrmex', 'Parnara naso Fabricius, 1798'='Parnara naso', 'Papilio manlius Fabricius, 1798'='Papilio manliu', 'Pelecophora interrupta Ali.'='Pelecophora interrupta'))
taxa[[10]]$name_clear <- str_replace_all(taxa[[10]]$name_clear, c('Labourdonnaisia calophylloides Bojer' = 'Labourdonnaisia calophylloides', 'Syzygium petrinense J. Bosser & J. Guého'= 'Syzygium petrinense', 'Tabernaemontana persicariifolia Jacq.'='Tabernaemontana persicariifolia','Brachymyrmex sp1'='Brachymyrmex', 'Parnara naso Fabricius, 1798'='Parnara naso', 'Scopariinae Guenée, 1854'='Scopariinae', 'Strictopterinae sp1'='Strictopterinae'))
taxa[[11]]$name_clear <- str_replace_all(taxa[[11]]$name_clear, c('Labourdonnaisia calophylloides Bojer' = 'Labourdonnaisia calophylloides', 'Syzygium petrinense J. Bosser & J. Guého'= 'Syzygium petrinense', 'Brachymyrmex sp1'='Brachymyrmex', 'Parnara naso Fabricius, 1798'='Parnara naso'))
taxa[[12]]$name_clear <- str_replace_all(taxa[[12]]$name_clear, c('Syzygium petrinense J. Bosser & J. Guého' = 'Syzygium petrinense', 'Paleorhiza sp1'= 'Paleorhiza', 'Scolia carnifex Coquerel 1855'='Scolia carnifex'))
taxa[[13]]$name_clear <- str_replace_all(taxa[[13]]$name_clear, c('Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Ceratitis rosa Karsch, 1887'='Ceratitis rosa'))
taxa[[14]]$name_clear <- str_replace_all(taxa[[14]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Parnara naso Fabricius, 1798'='Parnara naso', 'Scopariinae Guenée, 1854'='Scopariinae'))
taxa[[14]][13,2] <- 'Mauritiobrium undulatum'
taxa[[15]]$name_clear <- str_replace_all(taxa[[15]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Palexorista Townsend, 1921'='Palexorista', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Parnara naso Fabricius, 1798'='Parnara naso'))
taxa[[15]][28,2] <- 'Mauritiobrium undulatum'
taxa[[16]]$name_clear <- str_replace_all(taxa[[16]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Eumolpinae Hope, 1840'= 'Eumolpinae', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Parnara naso Fabricius, 1798'='Parnara naso'))
taxa[[16]][34,2] <- 'Mauritiobrium undulatum'
taxa[[17]]$name_clear <- str_replace_all(taxa[[17]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Parnara naso Fabricius, 1798'= 'Parnara naso', 'Catopsilia florella (Fabricius, 1775)'='Catopsilia florella', 'Cryptocephalinae Gyllenhal, 1813'='Cryptocephalinae', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata'))
taxa[[17]][23,2] <- 'Catopsilia florella'
taxa[[17]][31,2] <- 'Mauritiobrium undulatum'
taxa[[18]]$name_clear <- str_replace_all(taxa[[18]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Eumolpinae Hope, 1840'= 'Eumolpinae', 'Badula platyphylla (A. DC.) M.J.E. Coode'='Badula platyphylla', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata', 'Parnara naso Fabricius, 1798'='Parnara naso'))
taxa[[18]][39,2] <- 'Badula platyphylla'
taxa[[19]]$name_clear <- str_replace_all(taxa[[19]]$name_clear, c('Labourdonnaisia calophylloides Bojer' = 'Labourdonnaisia calophylloides', 'Badula platyphylla (A. DC.) M.J.E. Coode'= 'Badula platyphylla', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Scolia carnifex Coquerel 1855'='Scolia carnifex'))
taxa[[19]][28,2] <- 'Badula platyphylla'
taxa[[20]]$name_clear <- str_replace_all(taxa[[20]]$name_clear, c('Tabernaemontana persicariifolia Jacq.' = 'Tabernaemontana persicariifolia', 'Labourdonnaisia calophylloides Bojer'= 'Labourdonnaisia calophylloides', 'Brachymyrmex sp1'='Brachymyrmex', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata', 'Orthellia Robineau-Desvoidy, 1863'='Orthellia albigena'))
taxa[[21]]$name_clear <- str_replace_all(taxa[[21]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Mauritiobrium undulatum (Pic, 1935)'= 'Mauritiobrium undulatum', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata'))
taxa[[21]][14,2] <- 'Mauritiobrium undulatum'
taxa[[22]]$name_clear <- str_replace_all(taxa[[22]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Croton grangerioides Bojer ex Baill.'= 'Croton grangerioides', 'Labourdonnaisia calophylloides Bojer'='Labourdonnaisia calophylloides', 'Pelecophora interrupta Ali.'='Pelecophora interrupta', 'Syritta nigrifemorata Macquart, 1842'='Syritta nigrifemorata', 'Mauritiobrium undulatum (Pic, 1935)'='Mauritiobrium undulatum'))
taxa[[22]][20,2] <- 'Mauritiobrium undulatum'
taxa[[23]]$name_clear <- str_replace_all(taxa[[23]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Syzygium petrinense J. Bosser & J. Guého'='Syzygium petrinense', 'Croton grangerioides Bojer ex Baill.'='Croton grangerioides', 'Labourdonnaisia calophylloides Bojer'='Labourdonnaisia calophylloides', 'Parnara naso Fabricius, 1798'='Parnara naso'))
taxa[[24]]$name_clear <- str_replace_all(taxa[[24]]$name_clear, c('Brachymyrmex sp1' = 'Brachymyrmex', 'Orthellia Robineau-Desvoidy, 1863'= 'Orthellia albigena', 'Syzygium petrinense J. Bosser & J. Guého'='Syzygium petrinense', 'Croton grangerioides Bojer ex Baill.'='Croton grangerioides', 'Tabernaemontana persicariifolia Jacq.'='Tabernaemontana persicariifolia', 'Rubus alceifolius Poir.'='Rubus alceifolius'))


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
taxa_back_df[17,4] <- "121634"
taxa_back_df[50,4] <- "150296"
taxa_back_df[55,4] <- "28675"
taxa_back_df[81,4] <- "810307"
taxa_back_df[178,4] <- "651302"

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2010/data/Kaiser-Bunbury_et_al_2010_taxa_back.csv", row.names = FALSE)
saveRDS(taxa, file = "mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2010/data/Kaiser-Bunbury_et_al_2010_taxa.csv")
saveRDS(FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Kaiser-Bunbury_et_al_2010/data/Kaiser-Bunbury_et_al_2010_inter.csv")
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
for (i in 1:24) {
  
  
  networks <- list(name             = paste0("Kaiser-Bunbury_et_al_2010_",i),
                   date             = "2003-09-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Plant-pollinator system at two sites in Black River Gorges National Park, Mauritius",
                   public           = TRUE,
                   all_interactions = FALSE)


POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)

POST_node(node_df = taxa[[i]], network = networks)

POST_interaction(inter_df = FW_name[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
