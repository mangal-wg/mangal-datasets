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

lat  <- -19.965850
lon  <- -40.531745
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "frequency of interaction",
                   table_owner = "interactions",
                   description = "frequency of interaction between pairs of species",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1016/j.actao.2012.06.001",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.1016%2Fj.actao.2012.06.001",
             data_url  = "https://www.nceas.ucsb.edu/interactionweb/data/plant_pollinator/excel/varassin_sazima_2012.xlsx",
             author    = "varassin",
             year      = "2012",
             bibtex    = "@article{Varassin_2012,	doi = {10.1016/j.actao.2012.06.001},	url = {https://doi.org/10.1016%2Fj.actao.2012.06.001},	year = 2012,	month = {aug},	publisher = {Elsevier {BV}}, volume = {43},	pages = {104--112},	author = {Isabela Galarda Varassin and Marlies Sazima},	title = {Spatial heterogeneity and the distribution of bromeliad pollinators in the Atlantic Forest},	journal = {Acta Oecologica}}")


users <- list(name         = "Gabriel Bergeron",
              email        = "gabriel.bergeron3@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "administrator")


enviro <- list(name  = "attribute name",
               lat   = lat,
               lon   = lon,
               srid  = srid,
               date  = "2001-06-01",
               value = 0)


datasets <- list(name        = "varassin_sazima_2012",
                 date        = "2001-06-01",
                 description = "Bromeliad-pollinator interaction in the Estacao Biologica de Santa Lucia in southeastern Brazil",
                 public      = FALSE)


traits <- list(date = "2001-06-01")


networks <- list(name             = "varassin_sazima_2012",
                 date             = "2001-06-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Bromeliad-pollinator interaction in the Estacao Biologica de Santa Lucia in southeastern Brazil",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2001-06-01",
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
# # Set WD
# setwd("importation_mangal/varassin_sazima_2012")
# 
# # Open file
# varassin_sazima_2012 <- read.csv2(file = "raw/varassin_sazima_2012.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = "")
# 
# # Cleaning for melt()
# ## Get ROW one with Genus_species
# x  <- varassin_sazima_2012[2, ]
# x[1] <- "species"
# colnames(varassin_sazima_2012) <- x
# rm(x)
# 
# ## Delete unused row
# varassin_sazima_2012 <- varassin_sazima_2012[-c(1, 2), ]
# 
# # Melt df
# varassin_sazima_2012 <- melt(varassin_sazima_2012, id.vars = c("species"), na.rm = TRUE)
# 
# # Remove interaction value = 0 (no interaction)
# names(varassin_sazima_2012) <- c("sp_taxon_1", "sp_taxon_2", "value")
# varassin_sazima_2012 <- subset(varassin_sazima_2012, varassin_sazima_2012$value != 0)
# 
# #------------------------------
# # Set taxo_back and taxon table
# #------------------------------
# # Create taxo_back_df
# 
# ## Get Unique taxon of data
# taxon <- c(as.vector(unique(varassin_sazima_2012$sp_taxon_2)), as.vector(unique(varassin_sazima_2012$sp_taxon_1)))
# 
# 
# ### Check for spelling mistakes... ###
# 
# 
# ## Remove sp
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
#     taxo_back[i] <- word(taxon[i], start = 1)
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
# # Create taxons_df
# 
# taxons_df <- data.frame(taxon, NA)
# names(taxons_df) <- c("original_name", "name_clear")
# 
# for (i in 1:nrow(taxons_df)) {
# 
#   if(((str_detect(taxons_df[i, 1], "[:digit:]") == TRUE || str_detect(taxons_df[i, 1], "[:punct:]") == TRUE) &
#        str_detect(taxons_df[i, 1], "sp") == TRUE) ||
#        str_detect(taxons_df[i, 1], "n\\.i\\.") == TRUE ||
#        str_detect(taxons_df[i, 1], "sp$") == TRUE){
#     
#     taxons_df[i, 2] <- word(taxons_df[i, 1], start = 1)
# 
#   } else {
#     taxons_df[i, 2] <- as.character(taxons_df[i, 1])
#   }
# }
# 
# #------------------------------
# # Set traits table
# #------------------------------
# 
# # traits_df <- read.csv2(file = "data/varassin_sazima_2012_traits.csv", header = TRUE)
# 
# # traits_df <- melt(traits_df, id.vars = c("taxon"), na.rm = TRUE)
# # names(traits_df) <- c("taxon", "name", "value")
# 
# #------------------------------
# # Writing taxon and interaction table
# #------------------------------
# 
# write.csv2(x = taxo_back_df, file = paste0(getwd(), "/data/varassin_sazima_2012_taxo_back.csv"), row.names = FALSE)
# write.csv2(x = taxons_df, file = paste0(getwd(), "/data/varassin_sazima_2012_taxons.csv"), row.names = FALSE)
# write.csv2(x = varassin_sazima_2012, file = paste0(getwd(), "/data/varassin_sazima_2012_inter.csv"), row.names = FALSE)
# # write.csv2(x = traits_df, file = paste0(getwd(), "/data/varassin_sazima_2012_traits.csv"), row.names = FALSE)

setwd("importation_mangal/varassin_sazima_2012")
taxo_back_df <- read.csv2("data/varassin_sazima_2012_taxo_back.csv", header = TRUE)
taxons_df <- read.csv2("data/varassin_sazima_2012_taxons.csv", header = TRUE)
varassin_sazima_2012 <- read.csv2("data/varassin_sazima_2012_inter.csv", header = TRUE)
# traits_df <- read.csv2("data/varassin_sazima_2012_traits.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attributes(attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
POST_refs()
POST_users()
# POST_environments(enviro, attr_##)
POST_datasets()
POST_networks(networks, enviro = enviro)
POST_taxo_back()
POST_taxons(taxons_df)
# POST_traits(traits_df)
POST_interactions(varassin_sazima_2012, enviro = enviro, attr_inter)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, varassin_sazima_2012)
