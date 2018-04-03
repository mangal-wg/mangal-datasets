# Export data into MANGAL

# Download mangal package
GITHUB_PAT <- "e381e60e716f4c5330859f6cbd3f4b22d07359bc"
devtools::install_github("gabrielbouleau/mangal.package", oauth = GITHUB_PAT)

# Set library
library(mangal)

# Set WD
setwd("to folder root")

# Source data and export
source("importation_mangal/roberson_1929/1-meta_clean_roberson_1929.R")
source("importation_mangal/Test_Hocking_1968/2-meta_clean_hocking_1968.R")
source("importation_mangal/Witt_1998/3-meta_clean_Witt_1998.R")
source("importation_mangal/Silva_De_Marco_Hasui_Gomes_2002/4-meta_clean_Silva_De_Marco_Hasui_Gomes_2002.R")
source("importation_mangal/Mosquin_Martin_1967/5-meta_clean_Mosquin_Martin_1967.R")
source("importation_mangal/Lundgren_Olesen_2005/6-meta_clean_Lundgren_Olesen_2005.R")
source("importation_mangal/kaehler_et_al_2005/7-meta_clean_kaehler_et_al_2005.R")
source("importation_mangal/elberling_olesen_1999/8-meta_clean_elberling_olesen_1999.R")
source("importation_mangal/Elberling_Olesen/9-meta_clean_Elberling_Olesen.R")
source("importation_mangal/barret_helenurm_1987/10-meta_clean_barret_helenurm_1987.R")
source("importation_mangal/arroyo/11-meta_clean_arroyo.R")
source("importation_mangal/kohler_2011/12-meta_clean_kohler_2011.R")
source("importation_mangal/varassin_sazima_2012/13-meta_clean_varassin_sazima_2012.R")
source("importation_mangal/bezerra_2009/14-meta_clean_bezerra_2009.R")
source("importation_mangal/frost_1980/15-meta_clean_frost_1980.R")
source("importation_mangal/Olesen_al_2002/meta_clean_Olesen_al_2002.R")
source("importation_mangal/percival_1974/meta_clean_percival_1974.R")






