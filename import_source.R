# Export data into MANGAL

# Download mangal package
GITHUB_PAT <- "personnal access token"
devtools::install_github("SteveViss/mangal.package", oauth = GITHUB_PAT)

# Set library
library(mangal)

# Set WD
setwd("to folder root")

# Source data and export
# IMPORTANT CHANGES OCCURRED FOR THE SCRIPTS AND FUNCTIONS // THE SCRIPTS BELOW WHERE NOT CHANGES
source("mangal-datasets/roberson_1929/1-meta_clean_roberson_1929.R")
source("mangal-datasets/Test_Hocking_1968/2-meta_clean_hocking_1968.R")
source("mangal-datasets/Witt_1998/3-meta_clean_Witt_1998.R")
source("mangal-datasets/Silva_De_Marco_Hasui_Gomes_2002/4-meta_clean_Silva_De_Marco_Hasui_Gomes_2002.R")
source("mangal-datasets/Mosquin_Martin_1967/5-meta_clean_Mosquin_Martin_1967.R")
source("mangal-datasets/Lundgren_Olesen_2005/6-meta_clean_Lundgren_Olesen_2005.R")
source("mangal-datasets/kaehler_et_al_2005/7-meta_clean_kaehler_et_al_2005.R")
source("mangal-datasets/elberling_olesen_1999/8-meta_clean_elberling_olesen_1999.R")
source("mangal-datasets/Elberling_Olesen/9-meta_clean_Elberling_Olesen.R")
source("mangal-datasets/barret_helenurm_1987/10-meta_clean_barret_helenurm_1987.R")
source("mangal-datasets/arroyo/11-meta_clean_arroyo.R")
source("mangal-datasets/kohler_2011/12-meta_clean_kohler_2011.R")
source("mangal-datasets/varassin_sazima_2012/13-meta_clean_varassin_sazima_2012.R")
source("mangal-datasets/bezerra_2009/14-meta_clean_bezerra_2009.R")
source("mangal-datasets/frost_1980/15-meta_clean_frost_1980.R")
source("mangal-datasets/Olesen_al_2002/meta_clean_Olesen_al_2002.R")
source("mangal-datasets/percival_1974/meta_clean_percival_1974.R")
# SCRIPTS BELOW WERE WRITTEN WITH THE NEW FORMAT
source("mangal-datasets/fautin_1997/meta_clean_fautin_1997.R")
source("mangal-datasets/mccullen_1993/meta_clean_mccullen_1993.R")
source("mangal-datasets/kato_1993/meta_clean_kato_1993.R")
source("mangal-datasets/wheelwringth_1984/meta_clean_wheelwringth_1984.R")
source("mangal-datasets/motten_1982/meta_clean_motten_1982.R")
source("mangal-datasets/hadfield_2013/meta_clean_hadfield_2013.R")
source("mangal-datasets/ricciardi_2010/meta_clean_ricciardi_2010.R")
source("mangal-datasets/olesen/meta_clean_olesen.R")





