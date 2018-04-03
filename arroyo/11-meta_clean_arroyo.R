# # Set libraries
# library(reshape2)
# library(tidyr)
# library(jsonlite)
# library(httr)
# library(data.table)
# library(rcrossref)
# library(taxize)
# library(stringr)
# 
# library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- -33.283334 
lon  <- -70.266668
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence or absence of a recorded interaction",
                   unit        = "NA")


attr1 <- list(name        = "mean altitude",
              table_owner = "interactions",
              description = "mean altitude of plot studied",
              unit        = "meters")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")


refs <- list(doi       = "10.2307/2442833",
             jstor     = "2442833",
             pmid      = "NA",
             paper_url = "http://www.jstor.org/stable/2442833",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/html/arroyo_1982.html",
             author    = "arroyo",
             year      = "1982",
             bibtex    = "@article{10.2307/2442833, ISSN = {00029122, 15372197}, URL = {http://www.jstor.org/stable/2442833}, author = {Mary T. Kalin Arroyo and Richard Primack and Juan Armesto}, journal = {American Journal of Botany}, number = {1}, pages = {82--97}, publisher = {Botanical Society of America}, title = {Community Studies in Pollination Ecology in the High Temperate Andes of Central Chile. I. Pollination Mechanisms and Altitudinal Variation}, volume = {69}, year = {1982}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


datasets <- list(name        = "arroyo_1982",
                 date        = "1981-03-01",
                 description = "Plant-pollinator interaction at three altudinal levels (subandean scrub, cushion-plant, subnival feldfield) in the Andrean zone on the Cordon del Cepo in central Chile",
                 public      = TRUE)


traits <- list(date = "1981-03-01")


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1981-03-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observations",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



# #------------------------------
#   # Cleaning matrix
# #------------------------------
# 
#   # Open file
#   arroyo_I <- read.csv2(file = "importation_mangal/arroyo/raw/arroyo_I.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
#   # Cleaning for melt()
#   ## Merge two first COLUMNS Genus species
#   arroyo_I[1:2, 1:2] <- "sp."
#   arroyo_I <- unite(arroyo_I, sp1, c(V1, V2), sep = " ", remove = TRUE)
# 
#   ## Get ROW one with Genus_species
#   x  <- paste(arroyo_I[1, ], sep =" ", arroyo_I[2, ])
#   x[1] <- "species"
#   colnames(arroyo_I) <- x
#   rm(x)
# 
#   ## Delete unused row
#   arroyo_I <- arroyo_I[-c(1:3), -2]
# 
#   # Melt df
#   arroyo_I <- melt(arroyo_I, id.vars = c("species"), na.rm = TRUE)
# 
#   # Retirer les 0 et ajouter dans la table network edge_list = FALSE
# 
#   names(arroyo_I) <- c("sp_taxon_1", "sp_taxon_2", "value")
# 
#   # Remove interaction value = 0 (no interaction)
#   names(arroyo_I) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   arroyo_I <- subset(arroyo_I, arroyo_I$value != 0)
# 
# # arroyo_II
# 
#   arroyo_II <- read.csv2(file = "importation_mangal/arroyo/raw/arroyo_II.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
#   arroyo_II[1:2, 1:2] <- "sp."
#   arroyo_II <- unite(arroyo_II, sp1, c(V1, V2), sep = " ", remove = TRUE)
#   x  <- paste(arroyo_II[1, ], sep =" ", arroyo_II[2, ])
#   x[1] <- "species"
#   colnames(arroyo_II) <- x
#   rm(x)
#   arroyo_II <- arroyo_II[-c(1:3), -2]
#   arroyo_II <- melt(arroyo_II, id.vars = c("species"), na.rm = TRUE)
#   names(arroyo_II) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   arroyo_II <- subset(arroyo_II, arroyo_II$value != 0)
# 
# # arroyo_III
# 
#   arroyo_III <- read.csv2(file = "importation_mangal/arroyo/raw/arroyo_III.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
#   arroyo_III[1:2, 1:2] <- "sp."
#   arroyo_III <- unite(arroyo_III, sp1, c(V1, V2), sep = " ", remove = TRUE)
#   x  <- paste(arroyo_III[1, ], sep =" ", arroyo_III[2, ])
#   x[1] <- "species"
#   colnames(arroyo_III) <- x
#   rm(x)
#   arroyo_III <- arroyo_III[-c(1:3), -2]
#   arroyo_III <- melt(arroyo_III, id.vars = c("species"), na.rm = TRUE)
#   names(arroyo_III) <- c("sp_taxon_1", "sp_taxon_2", "value")
#   arroyo_III <- subset(arroyo_III, arroyo_III$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- unique(c(as.vector(unique(arroyo_I$sp_taxon_2)), as.vector(unique(arroyo_I$sp_taxon_1)), 
#                   as.vector(unique(arroyo_II$sp_taxon_2)), as.vector(unique(arroyo_II$sp_taxon_1)),
#                   as.vector(unique(arroyo_III$sp_taxon_2)), as.vector(unique(arroyo_III$sp_taxon_1))))
# 
# 
# ### Check for spelling mistakes... ###
# 
# 
# ### Remove sp
# 
# taxo_back <- vector()
# 
# for (i in 1:length(taxon)) {
# 
#   if(((str_detect(taxon[i], "[:digit:]") == TRUE || str_detect(taxon[i], "[:punct:]") == TRUE) &
#        str_detect(taxon[i], "sp") == TRUE) ||
#        str_detect(taxon[i], "n\\.i\\.") == TRUE ||
#        str_detect(taxon[i], "sp$") == TRUE){
#   
#   } else {
#     taxo_back[i] <- taxon[i]
#   }
# }
# 
# taxo_back <- unique(taxo_back)
# 
# 
# ## Select only taxa not yet in db
# 
# server <- "http://localhost:3000"
# 
# taxo_back_df <- data.frame()
# 
# for (i in 1:length(taxo_back)) {
#   
#   path <- modify_url(server, path = paste0("/api/v0/","taxo_backs/?name=", str_replace(taxo_back[i], " ", "%20")))
#   
#   if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json")))) == 0) {
#     
#     taxo_back_df[nrow(taxo_back_df)+1, 1] <- taxo_back[i]
#   }
# }
# 
# rm(taxo_back)
# names(taxo_back_df) <- c("name")
# 
# ## Get code by species
# taxo_back_df[, "bold"] <- NA
# taxo_back_df[, "eol"]  <- NA
# taxo_back_df[, "tsn"]  <- NA
# taxo_back_df[, "ncbi"] <- NA
# 
# ### Encore probleme d"identification avec les api... ###
# 
# for (i in 1:nrow(taxo_back_df)) {
#   try (expr = (taxo_back_df[i, 2] <- get_boldid(taxo_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
#   try (expr = (taxo_back_df[i, 3] <- get_eolid(taxo_back_df[i, 1], row = 5, verbose = FALSE, key = 110258)[1]), silent = TRUE)
#   try (expr = (taxo_back_df[i, 4] <- get_tsn(taxo_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
#   try (expr = (taxo_back_df[i, 5] <- get_uid(taxo_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
# }
# 
# # Writing taxo_back_df
# write.csv2(x = taxo_back_df, file = "importation_mangal/arroyo/data/arroyo_taxo_back.csv", row.names = FALSE)

taxo_back_df <- read.csv2("importation_mangal/arroyo/data/arroyo_taxo_back.csv", header = TRUE)

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

# #------------------------------
# # arroyo 1
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(arroyo_I$sp_taxon_2)), as.vector(unique(arroyo_I$sp_taxon_1)))
# 
# taxons_df1 <- data.frame(taxon, NA)
# names(taxons_df1) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df1)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){   
#     
#     taxons_df1[i, 2] <- word(taxons_df1[i, 1], start = 1)
#     
#   } else {
#     taxons_df1[i, 2] <- as.character(taxons_df1[i, 1])
#   }
# }

# Set metadata
networks <- list(name               = "arroyo_1982_subandean",
                   date             = "1981-03-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Plant-polinator interaction in subandean scrub in the Andrean zone on the Cordon del Cepo in central Chile",
                   public           = TRUE,
                   all_interactions = FALSE)

enviro1 <- list(name  = "mean altitude",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1981-03-01",
                value = 2400)

taxons_df1 <- read.csv2("importation_mangal/arroyo/data/arroyo_I_taxons.csv", header = TRUE)
arroyo_I  <- read.csv2("importation_mangal/arroyo/data/arroyo_I_inter.csv", header = TRUE)

# POST table
POST_environments(enviro1, attr1)
POST_networks(networks, enviro = enviro1)
POST_taxons(taxons_df1)
POST_interactions(arroyo_I, enviro = enviro1, attr = attr_inter)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df1, file = "importation_mangal/arroyo/data/arroyo_I_taxons.csv", row.names = FALSE)
# write.csv2(x = arroyo_I, file = "importation_mangal/arroyo/data/arroyo_I_inter.csv", row.names = FALSE)

# #------------------------------
# # arroyo 2
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(arroyo_II$sp_taxon_2)), as.vector(unique(arroyo_II$sp_taxon_1)))
# 
# taxons_df2 <- data.frame(taxon, NA)
# names(taxons_df2) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df2)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){
#     
#     taxons_df2[i, 2] <- word(taxons_df2[i, 1], start = 1)
#     
#   } else {
#     taxons_df2[i, 2] <- as.character(taxons_df2[i, 1])
#   }
# }

# Set metadata
networks <- list(name              = "arroyo_1982_cushion-plant",
                   date             = "1981-03-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Plant-polinator interaction of cushion-plant in the Andrean zone on the Cordon del Cepo in central Chile",
                   public           = TRUE,
                   all_interactions = FALSE)

enviro2 <- list(name  = "mean altitude",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1981-03-01",
                value = 2900)

taxons_df2 <- read.csv2("importation_mangal/arroyo/data/arroyo_II_taxons.csv", header = TRUE)
arroyo_II <- read.csv2("importation_mangal/arroyo/data/arroyo_II_inter.csv", header = TRUE)

# POST table
POST_environments(enviro2, attr1)
POST_networks(networks, enviro = enviro2)
POST_taxons(taxons_df2)
POST_interactions(arroyo_II, enviro = enviro2)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df2, file = "importation_mangal/arroyo/data/arroyo_II_taxons.csv", row.names = FALSE)
# write.csv2(x = arroyo_II, file = "importation_mangal/arroyo/data/arroyo_II_inter.csv", row.names = FALSE)

# #------------------------------
# # arroyo 3
# #------------------------------
# 
# # Create taxons_df
# taxon <- c(as.vector(unique(arroyo_III$sp_taxon_2)), as.vector(unique(arroyo_III$sp_taxon_1)))
# 
# taxons_df3 <- data.frame(taxon, NA)
# names(taxons_df3) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df3)) {
#   
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){  
#     
#     taxons_df3[i, 2] <- word(taxons_df3[i, 1], start = 1)
#     
#   } else {
#     taxons_df3[i, 2] <- as.character(taxons_df3[i, 1])
#   }
# }

# Set metadata
networks <- list(name               = "arroyo_1982_subnival_feldfiel",
                   date             = "1981-03-01",
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = "Plant-polinator interaction in subnival feldfield in the Andrean zone on the Cordon del Cepo in central Chile",
                   public           = TRUE,
                   all_interactions = FALSE)

enviro3 <- list(name  = "mean altitude",
                lat   = lat,
                lon   = lon,
                srid  = srid,
                date  = "1981-03-01",
                value = 3400)

taxons_df3 <- read.csv2("importation_mangal/arroyo/data/arroyo_III_taxons.csv", header = TRUE)
arroyo_III <- read.csv2("importation_mangal/arroyo/data/arroyo_III_inter.csv", header = TRUE)

# POST table
POST_environments(enviro3, attr1)
POST_networks(networks, enviro = enviro3)
POST_taxons(taxons_df3)
POST_interactions(arroyo_III, enviro = enviro3, attr = attr_inter)

# # Writing taxon and interaction table
# write.csv2(x = taxons_df3, file = paste0(getwd(), "/data/arroyo_III_taxons.csv"), row.names = FALSE)
# write.csv2(x = arroyo_III, file = paste0(getwd(), "/data/arroyo_III_inter.csv"), row.names = FALSE)

rm(lat, lon, srid, attr_inter, attr1, refs, users, enviro1, enviro2, enviro3, datasets, traits, networks, inter, taxons_df1, taxons_df2, taxons_df3, taxo_back_df, arroyo_I, arroyo_II, arroyo_III)
