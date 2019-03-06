# Set libraries
library(tidyr)
library(dplyr)
library(readr)
library(forcats)
library(purrr)
library(tibble)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)
library(fs)
library(measurements)
library(mangal)

#------------------------------
  # Metadata
#------------------------------

srid <- 4326
folder_name <- "havens_1992"

lake_localisation <- read.csv2(paste0(folder_name, "/Raw/Havens_localisation.csv"),
                               header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Latitude = conv_unit(.$Latitude, "deg_min_sec", "dec_deg"),
         Longitude = conv_unit(.$Longitude, "deg_min_sec", "dec_deg"))

lake_properties <- read_table2(paste0(folder_name, "/Raw/Havens_data.txt"),
                               col_type = cols(.default = col_double(), Lake = col_character(),
                                               Latitude = col_character(), Longitude = col_character())) %>%
  select(1:2, Elevation:DOC) %>%
  arrange(Lake_nb) %>%
  right_join(lake_localisation, by = "Lake") %>%
  select(-1) %>%
  split(.$Lake) %>%
  map(~select(.x, -1))

name_lake <- names(lake_properties)
  
# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence of absence of a recorded interaction",
                   unit        = "NA")

attr_altitude <- list(name        = "Altitude of the lake",
                      table_owner = "environments",
                      description = "Altitude of the lake",
                      unit        = "meters")

attr_area <- list(name        = "Area of the lake",
              table_owner = "environments",
              description = "Area of the lake",
              unit        = "hectares")

attr_max_depth <- list(name        = "Maximum depth of the lake",
              table_owner = "environments",
              description = "Maximum depth of the lake",
              unit        = "meters")

attr_lake_watershed_area <- list(name        = "Ratio lake / watershed area",
              table_owner = "TABLE_OWNER",
              description = "Ratio lake / watershed area",
              unit        = "NA")

attr_volume <- list(name        = "Volume of the lake",
              table_owner = "environments",
              description = "Volume of the lake",
              unit        = "cubic meters")

attr_prws <- list(name        = "Precipitation on the watershed",
              table_owner = "environments",
              description = "Precipitation on the watershed",
              unit        = "10^4 cubic meters per year")

attr_runoff <- list(name        = "Watershed runoff to the lake",
              table_owner = "environments",
              description = "Watershed runoff to the lake",
              unit        = "10^4 cubic meters per year")

attr_Etws <- list(name        = "Evapotranspiration from the watershed",
              table_owner = "environments",
              description = "Evapotranspiration from the watershed",
              unit        = "10^4 cubic meters per year")

attr_Prlake <- list(name        = "Precipitation onto the lake surface",
              table_owner = "environments",
              description = "Precipitation onto the lake surface",
              unit        = "10^4 cubic meters per year")

attr_Evapl <- list(name        = "Evaporation from the lake surface",
              table_owner = "environments",
              description = "Evaporation from the lake surface",
              unit        = "10^4 cubic meters per year")

attr_Outflow <- list(name        = "Discharge from the outlet",
              table_owner = "environments",
              description = "Discharge from the outlet",
              unit        = "10^4 cubic meters per year")

attr_Th <- list(name        = "Hyraulic retention time",
              table_owner = "environments",
              description = "Hyraulic retention time",
              unit        = "years")

attr_pH <- list(name        = "pH of the lake",
              table_owner = "environments",
              description = "pH of the lake",
              unit        = "NA")

attr_ANC <- list(name        = "Acid neutralizing capacity of the lake",
              table_owner = "environments",
              description = "Acid neutralizing capacity of the lake",
              unit        = "micro equivalent per litre")

attr_Conduct <- list(name        = "Conductance of the lake",
              table_owner = "environments",
              description = "Conductance of the lake",
              unit        = "micro siemens per meter")

attr_SO <- list(name        = "Sulfate concentration of the lake",
              table_owner = "environments",
              description = "Sulfate concentration of the lake",
              unit        = "micro equivalent per litre")

attr_NO <- list(name        = "Nitrate concentration of the lake",
              table_owner = "environments",
              description = "Nitrate concentration of the lake",
              unit        = "micro equivalent per litre")

attr_Ca <- list(name        = "Calcium concentration of the lake",
              table_owner = "environments",
              description = "Calcium concentration of the lake",
              unit        = "micro equivalent per litre")

attr_Al <- list(name        = "Monomeric aluminium concentration of the lake",
              table_owner = "environments",
              description = "Monomeric aluminium concentration of the lake",
              unit        = "micro equivalent per litre")

attr_DOC <- list(name        = "Dissolved organic carbon concentration of the lake",
              table_owner = "environments",
              description = "Dissolved organic carbon concentration of the lake",
              unit        = "milligram per litre")

ref <- list(doi       = "10.1126/science.257.5073.1107",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.1126%2Fscience.257.5073.1107",
             data_url  = "URL of the attached data",
             author    = "Karl Havens",
             year      = "1992",
             bibtex    = "@article{Havens_1992, doi = {10.1126/science.257.5073.1107}, url = {https://doi.org/10.1126%2Fscience.257.5073.1107}, year = 1992, month = {aug}, publisher = {American Association for the Advancement of Science ({AAAS})}, volume = {257}, number = {5073}, pages = {1107--1109}, author = {K. Havens}, title = {Scale and Structure in Natural Food Webs}, journal = {Science}}")

users <- list(name         = "Clément VIOLET",
              email        = "clement.violet@etudiant.univ-brest.fr",
              orcid        = "0000-0001-6217-5891",
              organization = "Université de Bretagne Occidentale",
              type         = "administrator")

dataset <- list(name        = "havens_1992",
                 date        = "1984-06-01",
                 description = "Pelagic communities of small lakes and ponds of the Adirondack",
                 public      = TRUE)

network <- list(`Alford Lake` = list(name             = "Alford_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$AlfordLake$Latitude[1],
                                     lon              = lake_properties$AlfordLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Alford Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                `Balsom Lake` = list(name             = "Balsom_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BalsomLake$Latitude[1],
                                     lon              = lake_properties$BalsomLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Balsom Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
           `Burntbridge Lake` = list(name             = "Burntbridge_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BurntbridgeLake$Latitude[1],
                                     lon              = lake_properties$BurntbridgeLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Burntbridge Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                `Beaver Lake` = list(name             = "Beaver_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BeaverLake$Latitude[1],
                                     lon              = lake_properties$BeaverLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Beaver Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `BigHope Lake` = list(name             = "Big_Hope_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BigHopeLake$Latitude[1],
                                     lon              = lake_properties$BigHopeLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the BigHope Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                 `BrandyLake` = list(name             = "Brandy_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BrandyLake$Latitude[1],
                                     lon              = lake_properties$BrandyLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Brandy Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
            `BridgeBrookLake` = list(name             = "Bridge_Brook_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BridgeBrookLake$Latitude[1],
                                     lon              = lake_properties$BridgeBrookLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Bridge Brook Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
           `Brook Trout Lake` = list(name             = "Brook_Trout_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BrookTroutLake$Latitude[1],
                                     lon              = lake_properties$BrookTroutLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Brook Trout Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Buck Pond` = list(name             = "Buck_Pond_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$BuckPond$Latitude[1],
                                     lon              = lake_properties$BuckPond$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Buck Pond.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `Cascade Lake` = list(name             = "Cascade_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$CascadeLake$Latitude[1],
                                     lon              = lake_properties$CascadeLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Cascade Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Chub Lake` = list(name             = "Chub_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$ChubLake$Latitude[1],
                                     lon              = lake_properties$ChubLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Chub Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Chub Pond` = list(name             = "Chub_Pond_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$ChubPond$Latitude[1],
                                     lon              = lake_properties$ChubPond$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Chub Pond.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `Connery Lake` = list(name             = "Connery_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$ConneryLake$Latitude[1],
                                     lon              = lake_properties$ConneryLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Connery Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Constable Lake` = list(name             = "Constable_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$ConstableLake$Latitude[1],
                                     lon              = lake_properties$ConstableLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Constable Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Deep Lake` = list(name             = "Deep_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$DeepLake$Latitude[1],
                                     lon              = lake_properties$DeepLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Deep Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `Emerald Lake` = list(name             = "Emerald_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$EmeraldLake$Latitude[1],
                                     lon              = lake_properties$EmeraldLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Emerald Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `FallsLake` = list(name             = "Falls_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$FallsLake$Latitude[1],
                                     lon              = lake_properties$FallsLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Falls Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                   `FawnLake` = list(name             = "Fawn_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$FawnLake$Latitude[1],
                                     lon              = lake_properties$FawnLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Fawn Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
            `Federation Lake` = list(name             = "Federation_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$FederationLake$Latitude[1],
                                     lon              = lake_properties$FederationLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Federation Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                 `Goose Lake` = list(name             = "Goose_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$GooseLake$Latitude[1],
                                     lon              = lake_properties$GooseLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Goose Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                 `Grass Lake` = list(name             = "Grass_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$GrassLake$Latitude[1],
                                     lon              = lake_properties$GrassLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Grass Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Gull Lake` = list(name             = "Gull_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$GullLake$Latitude[1],
                                     lon              = lake_properties$GullLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Gull Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
            `Gull Lake North` = list(name             = "Gull_Lake_North_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$GullLakeNorth$Latitude[1],
                                     lon              = lake_properties$GullLakeNorth$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Gull Lake North.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Helldiver Pond` = list(name             = "Helldiver_Pond_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$HelldiverPond$Latitude[1],
                                     lon              = lake_properties$HelldiverPond$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Helldiver Pond.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `High Pond` = list(name             = "High_Pond_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$HighPond$Latitude[1],
                                     lon              = lake_properties$HighPond$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the High Pond.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Hoel Lake` = list(name             = "Hoel_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$HoelLake$Latitude[1],
                                     lon              = lake_properties$HoelLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Hoel Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Horseshoe Lake` = list(name             = "Horseshoe_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$HorseshoeLake$Latitude[1],
                                     lon              = lake_properties$HorseshoeLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Horseshoe Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                `Indian Lake` = list(name             = "Indian_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$IndianLake$Latitude[1],
                                     lon              = lake_properties$IndianLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Indian Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Long Lake` = list(name             = "Long_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$LongLake$Latitude[1],
                                     lon              = lake_properties$LongLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Long Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Loon Lake` = list(name             = "Loon_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$LoonLake$Latitude[1],
                                     lon              = lake_properties$LoonLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Loon Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Lost Lake` = list(name             = "Lost_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$LostLake$Latitude[1],
                                     lon              = lake_properties$LostLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Lost Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Lost Lake East` = list(name             = "Lost_Lake_East_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$LostLakeEast$Latitude[1],
                                     lon              = lake_properties$LostLakeEast$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Lost Lake East.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
        `Little Rainbow Lake` = list(name             = "Little_Rainbow_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$LittleRainbowLake$Latitude[1],
                                     lon              = lake_properties$LittleRainbowLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Little Rainbow Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
          `Lower Sister Lake` = list(name             = "Lower_Sister_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$LowerSisterLake$Latitude[1],
                                     lon              = lake_properties$LowerSisterLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Lower Sister Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                `Oswego Lake` = list(name             = "Oswego_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$OswegoLake$Latitude[1],
                                     lon              = lake_properties$OswegoLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the OswegoLake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                   `Owl Lake` = list(name             = "Owl_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$OwlLake$Latitude[1],
                                     lon              = lake_properties$OwlLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Owl Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                   `Rat Lake` = list(name             = "Rat_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$RatLake$Latitude[1],
                                     lon              = lake_properties$RatLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Rat Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Razorback Lake` = list(name             = "Razorback_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$RazorbackLake$Latitude[1],
                                     lon              = lake_properties$RazorbackLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Razorback Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                   `RockLake` = list(name             = "Rock_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$RockLake$Latitude[1],
                                     lon              = lake_properties$RockLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Rock Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `Russian Lake` = list(name             = "Russian_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$RussianLake$Latitude[1],
                                     lon              = lake_properties$RussianLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Russian Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `Safford Lake` = list(name             = "Safford_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$SaffordLake$Latitude[1],
                                     lon              = lake_properties$SaffordLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Safford Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Sand Lake` = list(name             = "Sand_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$SandLake$Latitude[1],
                                     lon              = lake_properties$SandLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Sand Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                 `South Lake` = list(name             = "South_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$SouthLake$Latitude[1],
                                     lon              = lake_properties$SouthLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the South Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                 `Squaw Lake` = list(name             = "Squaw_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$SquawLake$Latitude[1],
                                     lon              = lake_properties$SquawLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Squaw Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `StinkLake` = list(name             = "Stink_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$StinkLake$Latitude[1],
                                     lon              = lake_properties$StinkLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Stink Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Twin Lake East` = list(name             = "Twin_Lake_East_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$TwinLakeEast$Latitude[1],
                                     lon              = lake_properties$TwinLakeEast$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Twin Lake East.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
             `Twin Lake West` = list(name             = "Twin_Lake_West_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$TwinLakeWest$Latitude[1],
                                     lon              = lake_properties$TwinLakeWest$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Twin Lake West.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
           `Twelfth Tee Lake` = list(name             = "Twelfth_Tee_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$TwelfthTeeLake$Latitude[1],
                                     lon              = lake_properties$TwelfthTeeLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Twelfth Tee Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
               `Whipple Lake` = list(name             = "Whipple_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$WhippleLake$Latitude[1],
                                     lon              = lake_properties$WhippleLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Whipple Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE),
                  `Wolf Lake` = list(name             = "Wolf_Lake_havens_1992",
                                     date             = "1984-06-01",
                                     lat              = lake_properties$WolfLake$Latitude[1],
                                     lon              = lake_properties$WolfLake$Longitude[1],
                                     srid             = srid,
                                     description      = "Food web of pelagic communities of small lakes and ponds of the Wolf Lake.",
                                     public           = TRUE,
                                     all_interactions = TRUE))


inter <- list(`Alford Lake` = list(name             = "Alford_Lake_havens_1992",
                                   date             = "1984-06-01",
                                   lat              = lake_properties$AlfordLake$Latitude[1],
                                   lon              = lake_properties$AlfordLake$Longitude[1],
                                   srid             = srid,
                                   description      = "Food web of pelagic communities of small lakes and ponds of the Alford Lake.",
                                   public           = TRUE,
                                   method           = "biblio",
                                   directed         = "directed"),
              `Balsom Lake` = list(name             = "Balsom_Lake_havens_1992",
                                   date             = "1984-06-01",
                                   lat              = lake_properties$BalsomLake$Latitude[1],
                                   lon              = lake_properties$BalsomLake$Longitude[1],
                                   srid             = srid,
                                   description      = "Food web of pelagic communities of small lakes and ponds of the Balsom Lake.",
                                   public           = TRUE,
                                   method           = "biblio",
                                   directed         = "directed"),
              `Burntbridge Lake` = list(name             = "Burntbridge_Lake_havens_1992",
                                        date             = "1984-06-01",
                                        lat              = lake_properties$BurntbridgeLake$Latitude[1],
                                        lon              = lake_properties$BurntbridgeLake$Longitude[1],
                                        srid             = srid,
                                        description      = "Food web of pelagic communities of small lakes and ponds of the Burntbridge Lake.",
                                        public           = TRUE,
                                        method           = "biblio",
                                        directed         = "directed"),
              `Beaver Lake` = list(name             = "Beaver_Lake_havens_1992",
                                   date             = "1984-06-01",
                                   lat              = lake_properties$BeaverLake$Latitude[1],
                                   lon              = lake_properties$BeaverLake$Longitude[1],
                                   srid             = srid,
                                   description      = "Food web of pelagic communities of small lakes and ponds of the Beaver Lake.",
                                   public           = TRUE,
                                   method           = "biblio",
                                   directed         = "directed"),
              `BigHope Lake` = list(name             = "Big_Hope_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$BigHopeLake$Latitude[1],
                                    lon              = lake_properties$BigHopeLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the BigHope Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `BrandyLake` = list(name             = "Brandy_Lake_havens_1992",
                                  date             = "1984-06-01",
                                  lat              = lake_properties$BrandyLake$Latitude[1],
                                  lon              = lake_properties$BrandyLake$Longitude[1],
                                  srid             = srid,
                                  description      = "Food web of pelagic communities of small lakes and ponds of the Brandy Lake.",
                                  public           = TRUE,
                                  method           = "biblio",
                                  directed         = "directed"),
              `BridgeBrookLake` = list(name             = "Bridge_Brook_Lake_havens_1992",
                                       date             = "1984-06-01",
                                       lat              = lake_properties$BridgeBrookLake$Latitude[1],
                                       lon              = lake_properties$BridgeBrookLake$Longitude[1],
                                       srid             = srid,
                                       description      = "Food web of pelagic communities of small lakes and ponds of the Bridge Brook Lake.",
                                       public           = TRUE,
                                       method           = "biblio",
                                       directed         = "directed"),
              `Brook Trout Lake` = list(name             = "Brook_Trout_Lake_havens_1992",
                                        date             = "1984-06-01",
                                        lat              = lake_properties$BrookTroutLake$Latitude[1],
                                        lon              = lake_properties$BrookTroutLake$Longitude[1],
                                        srid             = srid,
                                        description      = "Food web of pelagic communities of small lakes and ponds of the Brook Trout Lake.",
                                        public           = TRUE,
                                        method           = "biblio",
                                        directed         = "directed"),
              `Buck Pond` = list(name             = "Buck_Pond_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$BuckPond$Latitude[1],
                                 lon              = lake_properties$BuckPond$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Buck Pond.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Cascade Lake` = list(name             = "Cascade_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$CascadeLake$Latitude[1],
                                    lon              = lake_properties$CascadeLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the Cascade Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `Chub Lake` = list(name             = "Chub_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$ChubLake$Latitude[1],
                                 lon              = lake_properties$ChubLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Chub Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Chub Pond` = list(name             = "Chub_Pond_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$ChubPond$Latitude[1],
                                 lon              = lake_properties$ChubPond$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Chub Pond.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Connery Lake` = list(name             = "Connery_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$ConneryLake$Latitude[1],
                                    lon              = lake_properties$ConneryLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the Connery Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `Constable Lake` = list(name             = "Constable_Lake_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$ConstableLake$Latitude[1],
                                      lon              = lake_properties$ConstableLake$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Constable Lake.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `Deep Lake` = list(name             = "Deep_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$DeepLake$Latitude[1],
                                 lon              = lake_properties$DeepLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Deep Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Emerald Lake` = list(name             = "Emerald_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$EmeraldLake$Latitude[1],
                                    lon              = lake_properties$EmeraldLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the Emerald Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `FallsLake` = list(name             = "Falls_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$FallsLake$Latitude[1],
                                 lon              = lake_properties$FallsLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Falls Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `FawnLake` = list(name             = "Fawn_Lake_havens_1992",
                                date             = "1984-06-01",
                                lat              = lake_properties$FawnLake$Latitude[1],
                                lon              = lake_properties$FawnLake$Longitude[1],
                                srid             = srid,
                                description      = "Food web of pelagic communities of small lakes and ponds of the Fawn Lake.",
                                public           = TRUE,
                                method           = "biblio",
                                directed         = "directed"),
              `Federation Lake` = list(name             = "Federation_Lake_havens_1992",
                                       date             = "1984-06-01",
                                       lat              = lake_properties$FederationLake$Latitude[1],
                                       lon              = lake_properties$FederationLake$Longitude[1],
                                       srid             = srid,
                                       description      = "Food web of pelagic communities of small lakes and ponds of the Federation Lake.",
                                       public           = TRUE,
                                       method           = "biblio",
                                       directed         = "directed"),
              `Goose Lake` = list(name             = "Goose_Lake_havens_1992",
                                  date             = "1984-06-01",
                                  lat              = lake_properties$GooseLake$Latitude[1],
                                  lon              = lake_properties$GooseLake$Longitude[1],
                                  srid             = srid,
                                  description      = "Food web of pelagic communities of small lakes and ponds of the Goose Lake.",
                                  public           = TRUE,
                                  method           = "biblio",
                                  directed         = "directed"),
              `Grass Lake` = list(name             = "Grass_Lake_havens_1992",
                                  date             = "1984-06-01",
                                  lat              = lake_properties$GrassLake$Latitude[1],
                                  lon              = lake_properties$GrassLake$Longitude[1],
                                  srid             = srid,
                                  description      = "Food web of pelagic communities of small lakes and ponds of the Grass Lake.",
                                  public           = TRUE,
                                  method           = "biblio",
                                  directed         = "directed"),
              `Gull Lake` = list(name             = "Gull_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$GullLake$Latitude[1],
                                 lon              = lake_properties$GullLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Gull Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Gull Lake North` = list(name             = "Gull_Lake_North_havens_1992",
                                       date             = "1984-06-01",
                                       lat              = lake_properties$GullLakeNorth$Latitude[1],
                                       lon              = lake_properties$GullLakeNorth$Longitude[1],
                                       srid             = srid,
                                       description      = "Food web of pelagic communities of small lakes and ponds of the Gull Lake North.",
                                       public           = TRUE,
                                       method           = "biblio",
                                       directed         = "directed"),
              `Helldiver Pond` = list(name             = "Helldiver_Pond_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$HelldiverPond$Latitude[1],
                                      lon              = lake_properties$HelldiverPond$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Helldiver Pond.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `High Pond` = list(name             = "High_Pond_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$HighPond$Latitude[1],
                                 lon              = lake_properties$HighPond$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the High Pond.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Hoel Lake` = list(name             = "Hoel_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$HoelLake$Latitude[1],
                                 lon              = lake_properties$HoelLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Hoel Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Horseshoe Lake` = list(name             = "Horseshoe_Lake_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$HorseshoeLake$Latitude[1],
                                      lon              = lake_properties$HorseshoeLake$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Horseshoe Lake.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `Indian Lake` = list(name             = "Indian_Lake_havens_1992",
                                   date             = "1984-06-01",
                                   lat              = lake_properties$IndianLake$Latitude[1],
                                   lon              = lake_properties$IndianLake$Longitude[1],
                                   srid             = srid,
                                   description      = "Food web of pelagic communities of small lakes and ponds of the Indian Lake.",
                                   public           = TRUE,
                                   method           = "biblio",
                                   directed         = "directed"),
              `Long Lake` = list(name             = "Long_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$LongLake$Latitude[1],
                                 lon              = lake_properties$LongLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Long Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Loon Lake` = list(name             = "Loon_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$LoonLake$Latitude[1],
                                 lon              = lake_properties$LoonLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Loon Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Lost Lake` = list(name             = "Lost_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$LostLake$Latitude[1],
                                 lon              = lake_properties$LostLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Lost Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Lost Lake East` = list(name             = "Lost_Lake_East_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$LostLakeEast$Latitude[1],
                                      lon              = lake_properties$LostLakeEast$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Lost Lake East.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `Little Rainbow Lake` = list(name             = "Little_Rainbow_Lake_havens_1992",
                                           date             = "1984-06-01",
                                           lat              = lake_properties$LittleRainbowLake$Latitude[1],
                                           lon              = lake_properties$LittleRainbowLake$Longitude[1],
                                           srid             = srid,
                                           description      = "Food web of pelagic communities of small lakes and ponds of the Little Rainbow Lake.",
                                           public           = TRUE,
                                           method           = "biblio",
                                           directed         = "directed"),
              `Lower Sister Lake` = list(name             = "Lower_Sister_Lake_havens_1992",
                                         date             = "1984-06-01",
                                         lat              = lake_properties$LowerSisterLake$Latitude[1],
                                         lon              = lake_properties$LowerSisterLake$Longitude[1],
                                         srid             = srid,
                                         description      = "Food web of pelagic communities of small lakes and ponds of the Lower Sister Lake.",
                                         public           = TRUE,
                                         method           = "biblio",
                                         directed         = "directed"),
              `Oswego Lake` = list(name             = "Oswego_Lake_havens_1992",
                                   date             = "1984-06-01",
                                   lat              = lake_properties$OswegoLake$Latitude[1],
                                   lon              = lake_properties$OswegoLake$Longitude[1],
                                   srid             = srid,
                                   description      = "Food web of pelagic communities of small lakes and ponds of the OswegoLake.",
                                   public           = TRUE,
                                   method           = "biblio",
                                   directed         = "directed"),
              `Owl Lake` = list(name             = "Owl_Lake_havens_1992",
                                date             = "1984-06-01",
                                lat              = lake_properties$OwlLake$Latitude[1],
                                lon              = lake_properties$OwlLake$Longitude[1],
                                srid             = srid,
                                description      = "Food web of pelagic communities of small lakes and ponds of the Owl Lake.",
                                public           = TRUE,
                                method           = "biblio",
                                directed         = "directed"),
              `Rat Lake` = list(name             = "Rat_Lake_havens_1992",
                                date             = "1984-06-01",
                                lat              = lake_properties$RatLake$Latitude[1],
                                lon              = lake_properties$RatLake$Longitude[1],
                                srid             = srid,
                                description      = "Food web of pelagic communities of small lakes and ponds of the Rat Lake.",
                                public           = TRUE,
                                method           = "biblio",
                                directed         = "directed"),
              `Razorback Lake` = list(name             = "Razorback_Lake_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$RazorbackLake$Latitude[1],
                                      lon              = lake_properties$RazorbackLake$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Razorback Lake.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `RockLake` = list(name             = "Rock_Lake_havens_1992",
                                date             = "1984-06-01",
                                lat              = lake_properties$RockLake$Latitude[1],
                                lon              = lake_properties$RockLake$Longitude[1],
                                srid             = srid,
                                description      = "Food web of pelagic communities of small lakes and ponds of the Rock Lake.",
                                public           = TRUE,
                                method           = "biblio",
                                directed         = "directed"),
              `Russian Lake` = list(name             = "Russian_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$RussianLake$Latitude[1],
                                    lon              = lake_properties$RussianLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the Russian Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `Safford Lake` = list(name             = "Safford_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$SaffordLake$Latitude[1],
                                    lon              = lake_properties$SaffordLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the Safford Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `Sand Lake` = list(name             = "Sand_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$SandLake$Latitude[1],
                                 lon              = lake_properties$SandLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Sand Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `South Lake` = list(name             = "South_Lake_havens_1992",
                                  date             = "1984-06-01",
                                  lat              = lake_properties$SouthLake$Latitude[1],
                                  lon              = lake_properties$SouthLake$Longitude[1],
                                  srid             = srid,
                                  description      = "Food web of pelagic communities of small lakes and ponds of the South Lake.",
                                  public           = TRUE,
                                  method           = "biblio",
                                  directed         = "directed"),
              `Squaw Lake` = list(name             = "Squaw_Lake_havens_1992",
                                  date             = "1984-06-01",
                                  lat              = lake_properties$SquawLake$Latitude[1],
                                  lon              = lake_properties$SquawLake$Longitude[1],
                                  srid             = srid,
                                  description      = "Food web of pelagic communities of small lakes and ponds of the Squaw Lake.",
                                  public           = TRUE,
                                  method           = "biblio",
                                  directed         = "directed"),
              `StinkLake` = list(name             = "Stink_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$StinkLake$Latitude[1],
                                 lon              = lake_properties$StinkLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Stink Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"),
              `Twin Lake East` = list(name             = "Twin_Lake_East_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$TwinLakeEast$Latitude[1],
                                      lon              = lake_properties$TwinLakeEast$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Twin Lake East.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `Twin Lake West` = list(name             = "Twin_Lake_West_havens_1992",
                                      date             = "1984-06-01",
                                      lat              = lake_properties$TwinLakeWest$Latitude[1],
                                      lon              = lake_properties$TwinLakeWest$Longitude[1],
                                      srid             = srid,
                                      description      = "Food web of pelagic communities of small lakes and ponds of the Twin Lake West.",
                                      public           = TRUE,
                                      method           = "biblio",
                                      directed         = "directed"),
              `Twelfth Tee Lake` = list(name             = "Twelfth_Tee_Lake_havens_1992",
                                        date             = "1984-06-01",
                                        lat              = lake_properties$TwelfthTeeLake$Latitude[1],
                                        lon              = lake_properties$TwelfthTeeLake$Longitude[1],
                                        srid             = srid,
                                        description      = "Food web of pelagic communities of small lakes and ponds of the Twelfth Tee Lake.",
                                        public           = TRUE,
                                        method           = "biblio",
                                        directed         = "directed"),
              `Whipple Lake` = list(name             = "Whipple_Lake_havens_1992",
                                    date             = "1984-06-01",
                                    lat              = lake_properties$WhippleLake$Latitude[1],
                                    lon              = lake_properties$WhippleLake$Longitude[1],
                                    srid             = srid,
                                    description      = "Food web of pelagic communities of small lakes and ponds of the Whipple Lake.",
                                    public           = TRUE,
                                    method           = "biblio",
                                    directed         = "directed"),
              `Wolf Lake` = list(name             = "Wolf_Lake_havens_1992",
                                 date             = "1984-06-01",
                                 lat              = lake_properties$WolfLake$Latitude[1],
                                 lon              = lake_properties$WolfLake$Longitude[1],
                                 srid             = srid,
                                 description      = "Food web of pelagic communities of small lakes and ponds of the Wolf Lake.",
                                 public           = TRUE,
                                 method           = "biblio",
                                 directed         = "directed"))

enviro <- lake_properties %>%
  map(~select(.x, -c(1:2))) %>%
  map(~gather(.x, key = "name", value = "value"))


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file

metaweb <- read.table(paste0(folder_name, "/Raw/metaweb_Havens.txt"))

presence <- read.table(paste0(folder_name, "/Raw/presence_Havens.txt"))

sp_name_txt <- read_table2(paste0(folder_name, "/Raw/Havens_sp_list.txt"),
                           col_types = cols("i", "c", "i", "i", "i", "i", "i","c")) %>%
  select(-(3:8) )%>% # 3:7 are useless columns, 8th column is empty
  filter_all(all_vars(!is.na(.))) # Remove last two lines : full of NA
sp_name_txt[178,2] <- "Cyclotellasp"
sp_name_txt[207,2] <- "Chroococcussp"
sp_name_txt[122,2] <- "Staurastrumsp"
sp_name_txt[201,2] <- "Synurasp"

sp_name_PRN <- read.table(paste0(folder_name, "/Raw/Food_Web/TAXCODES.PRN"),
                          sep = "\t", skip = 7, colClasses = c("character", NULL), nrow = -1) %>%
  {str_remove(unlist(.),"\\s\\([^()]*\\)\\s*.*")} %>% # Remove last part with "(...)...."
  str_squish() %>% # Remove whitespaces begining and ending of the sp name
  enframe(name = NULL, value = "X1") # Must be df to start cleanning it
sp_name_PRN[178,1] <- "178 Cyclotella sp."
sp_name_PRN[207,1] <- "207 Chroococcus sp."
sp_name_PRN[122,1] <- "122 Staurastrum sp."
sp_name_PRN[201,1] <- "201 Synura sp."

sp_name_PRN_special <- filter(sp_name_PRN, X1 %in% c("221 periphyton", # These are special name containg only one word
                                              "119 nanoflagellates", # Or more than three (NB : double is considered has a number)
                                              "5 Salvelinus fontinalis x S. namaycush")) %>%
  unlist() # Coerce to vector format to combine with the other names

sp_name <- sp_name_PRN %>%
  filter(!X1 %in% c("221 periphyton", "119 nanoflagellates", #Remove these specials names
                    "5 Salvelinus fontinalis x S. namaycush")) %>%
  unlist() %>% # Coerce to vector to concatenante with sp_name_PRN_special
  word(end = 3) %>% #
  c(sp_name_PRN_special) %>%
  enframe(NULL, value = "spaced_name") %>% # Coerce to df again
  mutate(Nb = as.double(str_extract_all(unlist(.),"[:digit:]{1,3}"))) %>% # Make a column of number at the begining
  mutate(spaced_name = str_remove(unlist(.$spaced_name),"^[:digit:]{1,3}\\s")) %>% # Remove the numbers of the strings
  arrange(Nb) %>% # Reorder it
  full_join(sp_name_txt, 'Nb') %>% # Join it by id.
  select(2,1) %>% # Keeping only the id and the spaced_name col
  deframe() %>%
  as.factor() %>%
  fct_inorder()

data_file <- paste0(folder_name, "/raw/Food_Web") %>% #Getting all the file into one list
  dir_ls() %>%
  as.character() %>%
  discard(str_detect(., "TAXCODES.PRN")) %>% # This file containt only info about taxa, remove it
  map(~read_table(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>% #Read all file
  map(~unite(.x, sp_id, "X1":"X3", sep = "")) %>% #Merged the col 1:3 because sp id is split on three column
  set_names(paste0("FW_", name_lake)) #Each ellement of the list has the name of the file


lines <- list(
  line_1 = map(data_file,~slice(.x, 1)), #id number species is set on three rows.
  line_2 = map(data_file,~slice(.x, 2)),
  line_3 = map(data_file,~slice(.x, 3))
  )

merge_sp_id <- pmap(lines,~str_c(..1,..2,..3)) %>% #Concatenate the two part id to make one full sp id
  map2(modify(lines$line_1, ~names(.x)), ~`names<-`(.x, .y)) # modify() go deeper inside list levels


data_file <- data_file %>%
  map(~slice(.x, c(4:nrow(.x)))) %>%
  map2(modify(merge_sp_id,~.x), ~rbind(.y, .x)) %>%
  map(~mutate(.x, sp_id = str_replace_all(.x$sp_id, "NANANANANANANANANA", NA_character_)))

file_col_name <- data_file %>% # Saving column sp name of all file
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~as.integer(.x)) %>% # Coerce to integer linking it to sp_name
  map(~{.x <- levels(sp_name)[.x]}) # Apply the correct level to the corect sp number.

file_row_name <- data_file %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
  map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~as.integer(.x)) %>% # Coerce to integer linking it to sp_name
  map(~{.x <- levels(sp_name)[.x]}) # Apply the correct level to the corect sp number.


FW_name <- data_file %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>%
  map2(modify(file_row_name, ~.x), ~mutate(.x, sp_id = .y)) %>%
  map(~column_to_rownames(.x, "sp_id")) %>% # Store sp name as row name
  map2(modify(file_col_name, ~.x), ~{`names<-`(.x, .y)}) %>%
  map(~rownames_to_column(.x, "sp_taxon_2")) %>%
  map(~gather(.x, "sp_taxon_1", "value", -sp_taxon_2)) %>% #Convert large df to long format
  map(~select(.x, sp_taxon_1, sp_taxon_2, value)) %>%
  map(~filter(.x, value != 0)) %>%# Remove 0 interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(Cryptomonas\\sovata)|(C\\.\\serosa)|(Peridinium\\slimbatum)|
                                           (P\\.\\swisconsinense)|(P\\.\\sinconspicuum)|(P\\.\\scinctum)|(Ceratium\\scarolinianum)|
                                           (Gymnodinium\\spalustre)|(Euglena\\ssp\\.)|(E\\.\\sacus)|(nanoflagellates)|(Zygnema\\ssp\\.)|
                                           (Staurastrum\\smegacanthum)|(s\\.\\ssp\\.)|(Halotheca\\ssp\\.)|(Desmidium\\ssp\\.)|(Coelastrum\\ssp\\.)|
                                           (C\\.\\scambricum)|(Xanthidium\\ssp\\.)|(X\\.\\sarmatum)|(Kirchneriella\\slunaris)|(Oocyctis\\ssp\\.)|
                                           (Sphaerocystis\\sschroeteri)|(Schroederia\\ssetigera)|(Elakatrothrix\\sgelatinosa)|(Arthrodesmus\\sincus)|
                                           (A\\.\\ssubulatus)|(A\\.\\soctocornis)|(Tetraedrom\\sminimum)|(T\\.\\scaudatum)|(T\\.\\ssp\\.)|
                                           (Botryococcus\\sbraunii)|(Crucigenia\\squadrata)|(C\\.\\srectangularis)|(C\\.\\stetrapaedia)|
                                           (C\\.\\scrucifera)|(Spondylosium\\splanum)|(Ankistrodesmus\\sfalcatus)|(Quadrigula\\sclosterioides)|
                                           (Pediastrum\\stetras)|(Scenedesmus\\ssp\\.)|(S\\.\\squadricauda)|(S\\.\\sdimorphus)|(S\\.\\slongus)|
                                           (S\\.\\sserratus)|(S\\.\\sarcuatus)|(Cosmarium\\ssp\\.)|(Euastrum\\ssp\\.)|(Closterium\\ssp\\.)|
                                           (Micrasterias\\ssp\\.)|(Ankistrodesmus\\ssp\\.)|(A\\.\\sspiralis)|(Quadrigula\\ssp\\.)|(Q\\.\\slacustris)|
                                           (Dimorphocuccus\\slunatus)|(Nephrocytium\\ssp\\.)|(Dictyosphaerium\\spulchellum\\s\\sCarteria\\ssp\\.)|
                                           (Chlamydomonas\\ssp\\.)|(Rhizosolenia\\seriensis)|(Pleurosigma\\ssp\\.)|(Cocconeia\\ssp\\.)|
                                           (Navicula\\ssp\\.)|(Tabellaria\\sfenestrata)|(T\\.\\sflocculosa)|(Asterionella\\sformosa)|
                                           (Melosira\\ssp\\.)|(Cyclotella\\sglomerata)|(C\\.\\smichiganiana)|(C\\.\\ssp\\.)|(Fragillaria\\scrotonensis)|
                                           (F\\.\\sintermedia)|(F\\.\\ssp\\.)|(Nitzschia\\ssp\\.)|(Gyrosigma\\ssp\\.)|(Gomphonema\\ssp\\.)|
                                           (Synedra\\snana)|(S\\.\\sulna)|(Diatoma\\ssp\\.)|(Mallamonas\\shamata)|(M\\.\\sacaroides)|
                                           (M\\.\\shindonii)|(M\\.\\sakrokomos)|(M\\.\\ssp\\.)|(Dinobryon\\sbavaricum)|(D\\.\\sdivergens)|
                                           (D\\.\\ssertularia)|(D\\.\\ssp\\.)|(Diceras\\ssp\\.)|(Chrysocapsa\\splanktonica)|(Chrysosphaerella\\slongispina)|
                                           (\\sSynura\\suvella)|(S\\.\\ssp\\.)|(Uroglenopsis\\samericana)|(Keriphyron\\ssp\\.)|(Merismopedia\\stenuissima)|
                                           (M\\.\\spunctata)|(Chroococcus\\slimneticus)|(C\\.\\ssp\\.)|(Aphanothece\\ssp\\.)|(Anabaena\\ssp\\.)|(A\\.\\sflos-aquae)|
                                           (Coelosphaerium\\ssp\\.)|(C\\.\\spallidum)|(Aphanocapsa\\ssp\\.)|(Lyngbya\\ssp\\.)|(Rhabdoderma\\slineare)|
                                           (R\\.\\sgorksii)|(R\\.\\ssp\\.)|(Anacystis\\sincerta)|(Microsystis\\ssp\\.)|(benthic\\sdetritus)|(periphyton)|
                                           (Staurastrum\\ssp\\.)|(Synura\\ssp\\.)|(Chroococcus\\ssp\\.)|(Cyclotella\\ssp\\.)"), "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Dd]ebris.*)|(.*[Dd]etri*)"), "commensalism", .x$type)))

############################################################################################
# Use this code to see which species are phytos and deduct which interactions are herbivory
############################################################################################
# phyto_name_txt <- read_table2(paste0("mangal-datasets/", folder_name, "/Raw/Havens_sp_list.txt"),
#                               col_types = cols("i", "c", "i", "i", "i", "i", "i","c")) %>%
#   filter(Phyto == 1) %>%
#   select(-(3:8))
# 
# phyto_sp <- sp_name_PRN %>%
#   filter(!X1 %in% c("221 periphyton", "119 nanoflagellates", #Remove these specials names
#                     "5 Salvelinus fontinalis x S. namaycush")) %>%
#   unlist() %>% # Coerce to vector to concatenante with sp_name_PRN_special
#   word(end = 3) %>% #
#   c(sp_name_PRN_special) %>%
#   enframe(NULL, value = "spaced_name") %>% # Coerce to df again
#   mutate(Nb = as.double(str_extract_all(unlist(.),"[:digit:]{1,3}"))) %>% # Make a column of number at the begining
#   mutate(spaced_name = str_remove(unlist(.$spaced_name),"^[:digit:]{1,3}\\s")) %>% # Remove the numbers of the strings
#   arrange(Nb) %>% # Reorder it
#   full_join(phyto_name_txt, 'Nb') %>%
#   filter(!is.na(Name)) %>%
#   select(-(2:3)) %>%
#   unlist() %>%
#   unname()

rm(file_col_name, file_row_name, data_file)


#------------------------------
# Set taxo_back and taxa table
#------------------------------

taxa <- FW_name %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
  map(~gather(.x, id, sp)) %>%
  modify(~deframe(.x)) %>%
  map(~unique(.x)) %>%
  map(~str_remove_all(.x,"\\s\\(.*\\)$")) %>%
  set_names(paste0("Taxa_", name_lake)) %>%
  map(~enframe(.x, name = "id", value = "original_name"))

# taxa <- FW_name %>%
#   map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
#   map(~gather(.x, id, sp)) %>%
#   modify(~deframe(.x)) %>%
#   map(~unique(.x)) %>%
#   map(~str_remove_all(.x,"\\s\\(.*\\)$")) %>%
#   set_names(paste0("Taxa_", name_lake)) %>%
#   map(~enframe(.x, name = "id", value = "original_name")) %>%
#   map_dfr(~.x) %>%
#   unique() %>%
#   as.data.frame()

sp_name <- sp_name %>%
  as.character() %>%
  enframe(name = "id", value = "sp")

sp_name_corrected <- sp_name
sp_name_corrected$sp[42] <- "Copepod nauplii" # Wrong orthograph
sp_name_corrected$sp[178] <- "Cyclotella sp." # Duplicated abreviation C. sp.
sp_name_corrected$sp[207] <- "Chroococcus sp."
sp_name_corrected$sp[122] <- "Staurastrum sp." # Duplicated abreviation S. sp.
sp_name_corrected$sp[201] <- "Synura sp."
sp_name_corrected$sp[219] <- "Microcystis sp."

# taxa_corrected <- taxa %>%
#   map(~mutate(.x, sp = str_remove_all(.x$sp, "\\s\\(.*\\)$"))) %>% # Correct vernacular name at the lowest level possible (species)
#   map(~mutate(.x, sp = str_replace_all(.x$sp, c("dinoflagellates" = "Dinophyceae", "Rangia" = "Rangia cuneata"))))

sp_name_corrected$sp <- str_replace_all(sp_name_corrected$sp,
                                  c("A. affinis" = "Alona affinis", "A. flos-aquae" = "Anabaena flos-aquae",
                                    "A. octocornis" = "Arthrodesmus octocornis", "A. rectangula" = "Alona rectangula",
                                    "A. quadrangularis" = "Alona quadrangularis", "A. subulatus" = "Arthrodesmus subulatus",
                                    "C. cambricum" = "Coelastrum cambricum", "C. crucifera" = "Crucigenia crucifera",
                                    "C. erosa" = "Cryptomonas erosa", "C. michiganiana" = "Cyclotella michiganiana",
                                    "C. pallidum" = "Coelosphaerium pallidum",  "C. rectangularis"= "Crucigenia rectangularis",
                                    "C. reticulata" = "Ceriodaphnia reticulata", "C. scutifer" = "Cyclops scutifer",
                                    "C. vernalis" = "Cyclops vernalis", "D. ambigua" = "Daphnia ambigua",
                                    "D. catawba" = "Daphnia catawba", "D. divergens" = "Dinobryon divergens",
                                    "D. sertularia" = "Dinobryon sertularia", "D. dubia" = "Daphnia dubia", "D. leptomus" = "Diaptomus leptomus",
                                    "D. longiremis" = "Daphnia longiremis", "D. parvula" = "Daphnia parvula",  "D. pulex" = "Daphnia pulex",
                                    "D. retrocurva" = "Daphnia retrocurva", "D. sicilus" = "Diaptomus sicilus", "D. sp." = "Dinobryon sp.",
                                    "F. sp." = "Fragilaria sp.", "K. crassa" = "Keratella crassa", "K. longispina" = "Kelicottia longispina",
                                    "K. taurocephala" = "Keratella taurocephala", "K. testudo" = "Keratella testudo", "L. mira" = "Lecane mira",
                                    "M. acaroides" = "Mallamonas acaroides", "M. akrokomos" = "Mallomonas akrokomos",
                                    "M. hindonii" = "Mallomonas hindonii", "M. punctata" = "Merismopedia punctata",
                                    "M. sp." = "Mallamonas sp.", "P. cinctum" = "Peridinium cinctum", "P. hudsoni" = "Ploesoma hudsoni",
                                    "P. inconspicuum" = "Peridinium inconspicuum", "P. major" = "Polyarthra major",
                                    "P. remata" = "Polyarthra remata", "P. vulgaris" = "Polyarthra vulgaris", "R. gorskii" = "Rhabdoderma gorskii",
                                    "S. longus" = "Scenedesmus longus", "S. quadricauda" = "Scenedesmus quadricauda",
                                    "S. ulna" = "Synedra ulna", "T. caudatum" = "Tetraedrom caudatum",
                                    "T. flocculosa" = "Tabellaria flocculosa", "T. multicrinnis" = "Trichocerca multicrinnis",
                                    "T. pusilla" = "Trichocerca pusilla", "T. similis" = "Trichocerca similis", "T. sp." = "Tetraedrom sp.",
                                    "X. armatum" = "Xanthidium armatum"))

sp_name_checked <- sp_name_corrected$sp %>%
  gnr_resolve(canonical = FALSE, best_match_only = T) # Resolve taxonomique name


sp_name_not_known <- attributes(sp_name_checked)$not_known %>% # Names not recognized by Global Names Recognition and Discovery.
  enframe(name = NULL, value = "user_supplied_name") %>% # Create a df simillar to sp_name_cleaned to bind it whith sp_name_cleaned
  mutate(., submitted_name = paste0(str_to_upper(str_extract(.$user_supplied_name, ".{1}")), str_remove(.$user_supplied_name, ".{1}"))) %>%
  mutate(matched_name = submitted_name,
         data_source_title = NA_character_,
         score = NA_real_)

taxa_df_global <- sp_name_checked %>% # Taxa resolved
  bind_rows(sp_name_not_known) %>% # Taxa not resolved
  select(user_supplied_name, matched_name) %>% # Select only two column of interest
  left_join(sp_name_corrected, by = c("user_supplied_name" = "sp")) %>% # Join the table without taxa resolved
  select(-1) %>% # Remove useless column
  left_join(sp_name, by = "id") %>% # Join with the orignal sp table in the same order
  select(sp, matched_name) %>%
  `names<-`(c("original_name", "name_clear"))

taxa_df_global$name_clear <- taxa_df_global$name_clear %>%
  str_remove_all("\\ssp\\.*$") %>%
  str_replace_all(fixed(". "), "_") %>%
  str_remove_all(fixed("."))

taxa_df_global$name_clear[3] <- "Salmo trutta" # Manually editing some wrong name added by Global Names Recognition and Discovery.
taxa_df_global$name_clear[5] <- "Salvelinus fontinalis x S. namaycush"
taxa_df_global$name_clear[11] <- "Amploblites rupestris"
taxa_df_global$name_clear[19] <- "Pimphales promelas"
taxa_df_global$name_clear[26] <- "Pimphales notatus"
taxa_df_global$name_clear[28] <- "Fish eggs"
taxa_df_global$name_clear[29] <- "Fish fry"
taxa_df_global$name_clear[35] <- "Tropocyclops parsinus"
taxa_df_global$name_clear[42] <- "Copepod nauplii"
taxa_df_global$name_clear[44] <- "Diaptomus leptopus"
taxa_df_global$name_clear[53] <- "Bosmina longirsotris"
taxa_df_global$name_clear[70] <- "Pleuroxus hamulatus"
taxa_df_global$name_clear[89] <- "C. unicornis"
taxa_df_global$name_clear[90] <- "C. hippocrepis"
taxa_df_global$name_clear[96] <- "Manfredium eudactylotum"
taxa_df_global$name_clear[99] <- "Eucentrum"
taxa_df_global$name_clear[108] <- "Euglena"
taxa_df_global$name_clear[121] <- "Elakatothrix gelatinosa"
taxa_df_global$name_clear[126] <- "Tetraedron caudatum"
taxa_df_global$name_clear[131] <- "C. tetrapaedia"
taxa_df_global$name_clear[146] <- "Dimorphococcus lunatus"
taxa_df_global$name_clear[161] <- "Fragilaria crotonensis"
taxa_df_global$name_clear[169] <- "Mallomonas hamata"
taxa_df_global$name_clear[170] <- "Mallomonas acaroides"

taxa_df_global$name_clear[220] <- "R_sp1"

taxa_df <- taxa %>%
  map(~inner_join(.x, taxa_df_global, by = "original_name")) %>%
  map(~select(.x, -id))


## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- taxa_df %>%
  map(~unlist(.x$name_clear)) %>%
  map(~unname(.x)) %>%
  flatten_chr() %>%
  unique() %>%
  .[-209] %>% # Remove R_sp1
  map_chr(~{modify_url(server, path = paste0("/api/v2/","taxonomy?name=", str_replace_all(.x, " ", "%20")))}) %>%
  map_chr(~str_replace_all(.x, ".*,%20.*", "_")) %>%
  map_chr(~str_replace_all(.x, ".*%20-%20.*", "-")) %>%
  map_chr(~str_replace_all(.x, "\\.%20", "__")) %>%
  keep(~length(content((GET(url = .x, config = add_headers("Content-type" = "application/json","Authorization" = paste("bearer", readRDS(".httr-oauth"))))))) == 0) %>%
  map_chr(~str_remove_all(.x, fixed("http://poisotlab.biol.umontreal.ca/api/v2/taxonomy?name="))) %>%
  map_chr(~str_replace_all(.x, fixed("%20"), " ")) %>%
  map_chr(~str_replace_all(.x, fixed("__"), ". ")) %>%
  map_chr(~str_replace_all(.x, fixed("_"), ". "))

taxa_back_df <- taxa_back_df %>%
  enframe(name = NULL, value = "name") %>%
  mutate(bold = as.double(unlist({map(.$name, ~get_boldid(.x, row = 5, verbose = FALSE)[1])})),
         eol = as.double(unlist({map(.$name, ~get_eolid(.x, row = 5, verbose = FALSE, key = 110258)[1])})),
         tsn = as.double(unlist({map(.$name, ~get_tsn(.x, row = 5, verbose = FALSE)[1])})),
         ncbi = as.double(unlist({map(.$name, ~get_uid(.x, row = 5, verbose = FALSE, key = "679d0a26947d9b6432371b268ec0c7b39b08")[1])}))) # Add API KEy for NCBI

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

# write.csv2(x         = taxa_back_df,
#            file      = paste0(getwd(), "/", folder_name, "/data/", folder_name, "_taxonomy.csv"),
#            row.names = FALSE)

if(is.null(names(taxa_df)) == TRUE){ # Control flow statement if there is multiple dataset in this paper.
  
  taxa_df %>%
    walk(~write.csv2(.x, 
                     file      = paste0(getwd(), "/", folder_name, "/data/", folder_name, "_node.csv"), 
                     row.names = FALSE))
  
  
}else{
  
  taxa_df %>%
    names() %>%
    walk(~write.csv2(x         = taxa_df[[.]],
                     file      = paste0(getwd(), "/", folder_name, "/data/", folder_name, "_", ., "_node.csv"),
                     row.names = FALSE))
  
}

if(is.null(names(FW_name)) == TRUE){
  
  FW_name %>%
    walk(~write.csv2(.x, 
                     file      = paste0(folder_name,"/data/",folder_name, "_inter.csv"), 
                     row.names = FALSE))
  
}else{
  
  FW_name %>%
    names() %>%
    walk(~write.csv2(FW_name[[.]], 
                     file      = paste0(folder_name,"/data/",folder_name, "_", ., "_inter.csv"),
                     row.names = FALSE))
}


# trait_df %>%
#   names() %>%
#   walk(~write.csv2(x = trait_df[[.]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_", ., "_trait.csv"), row.names = FALSE))
# 
# taxa_back_df <-  paste0("mangal-datasets/", folder_name, "/data/", folder_name, "_taxa_back.csv") %>%
#   read_csv2(col_types = cols("c", "d", "d", "d", "d"))
# 
# taxa_df <-  paste0("mangal-datasets/", folder_name, "/data/") %>%
#   dir_ls() %>%
#   as.character() %>%
#   str_subset(fixed("taxa.csv")) %>%
#   map(~read_csv2(.x, col_types = cols("c", "c")))
# 
# FW_name <-  paste0("mangal-datasets/", folder_name, "/data/") %>%
#   dir_ls() %>%
#   as.character() %>%
#   str_subset(fixed("inter.csv")) %>%
#   map(~read_csv2(.x, col_types = cols("c", "c", "i")))
# 
# trait_df <-  paste0("mangal-datasets/", folder_name, "/data/") %>%
#   dir_ls() %>%
#   as.character() %>%
#   str_subset(fixed("trait.csv")) %>%
#   map(~read_csv2(.x, col_types = cols("c", "c", "d")))


#------------------------------
# Throwing injection functions
#------------------------------

## Metadata
POST_attribute(attr_inter)
# POST_attribute(attr1)
# POST_attribute(attr2)
POST_ref(ref)
POST_users(users)
# POST_environment(enviro, attr_##)
POST_dataset(dataset, users, ref)

## Network
map(network,~POST_network(network_lst = .x, dataset = dataset, users = users, enviro = NULL))

## Taxonomy
POST_taxonomy(taxa_back_df)

## Node
map2(taxa_df, network, ~POST_node(.x, .y))

## Interaction
# map(FW_name, ~POST_interaction(.x, inter = inter, enviro = NULL, attr = attr_inter, users, network = network))
# l <- list(FW_name, inter, network)
# pmap(list(FW_name, inter, network), ~POST_interaction(inter_df = ..1, inter = ..2, enviro = NULL, attr = attr_inter, users = users, network = ..3))
for(i in 1:length(FW_name)){
  
  POST_interaction(inter_df = FW_name[[i]], inter = inter[[i]], attr = attr_inter, users = users, network = network[[i]])
  cat(paste0(round(i/length(FW_name)*100, 2), " % terminé\n"))
}

# Updating information
## updating localisaion of the networks

request_url <- httr::modify_url(url = "http://poisotlab.biol.umontreal.ca", path = paste0("/api/v2/","network"), query="q=%havens_1992")

responses <- httr::GET(url = request_url, config = httr::add_headers(Authorization = paste("Bearer", readRDS(".httr-oauth"))))

havens_net_def <- fromJSON(content(responses, "text"))

resp <- list()
for(r in 1:nrow(havens_net_def)){
  uri <- httr::modify_url(url = "http://poisotlab.biol.umontreal.ca", path = paste0("/api/v2/network/", havens_net_def$id[r]))

  resp[[r]] <- PUT(uri,
                   body   = list(localisation = list(type = "Point",
                                                     coordinates = c(lake_localisation$Longitude[r], lake_localisation$Latitude[r]))),
                   config = httr::add_headers(Authorization = paste("Bearer", readRDS(".httr-oauth"))),
                   encode = "json")
}

## updating network description
network_ids <- c("1247", "1226") # Id of the network to update
uri <- character(length = 2)
description <- c("Food web of pelagic communities of small lakes and ponds of the Lost Lake East.",
                 "Food web of pelagic communities of small lakes and ponds of the Chub Lake.")
resp <- list()

for(id in 1:length(network_ids)){
  uri[id] <- httr::modify_url(url = "http://poisotlab.biol.umontreal.ca", path = paste0("/api/v2/","network/", network_ids[id]))
}

for (modif in 1:length(uri)){  
  resp[[modif]] <- httr::PUT(url = uri[modif], 
                    body = list(description = description[modif]),
                    config = httr::add_headers(Authorization = paste("Bearer", readRDS(".httr-oauth"))))
}


rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)

