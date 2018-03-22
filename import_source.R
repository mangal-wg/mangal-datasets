# Export data into MANGAL

# Download mangal package
devtools::install_github("gabrielbouleau/mangal.package")

# Set library
library(mangal)

# Set WD
setwd("importation_mangal")

# Source data and export
source("roberson_1929/1-meta_clean_roberson_1929.R")
source("Test_Hocking_1968/2-meta_clean_hocking_1968.R")
source("Witt_1998/3-meta_clean_Witt_1998.R")
source("Silva_De_Marco_Hasui_Gomes_2002/4-meta_clean_Silva_De_Marco_Hasui_Gomes_2002.R")
source("Mosquin_Martin_1967/5-meta_clean_Mosquin_Martin_1967.R")
source("Lundgren_Olesen_2005/6-meta_clean_Lundgren_Olesen_2005.R")
source("kaehler_et_al_2005/7-meta_clean_kaehler_et_al_2005.R")
source("elberling_olesen_1999/8-meta_clean_elberling_olesen_1999.R")
source("Elberling_Olesen/9-meta_clean_Elberling_Olesen")
source("barret_helenurm_1987/10-meta_clean_barret_helenurm_1987.R")
source("arroyo/11-meta_clean_arroyo.R")
source("kohler_2011/12-meta_clean_kohler_2011.R")
source("varassin_sazima_2012/13-meta_clean_varassin_sazima_2012.R")
source("bezerra_2009/14-meta_clean_bezerra_2009.R")
source("frost_1980/15-meta_clean_template_frost_1980.R")
