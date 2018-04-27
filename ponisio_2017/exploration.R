samples <- read.csv2("samples.csv", sep = ",", header = TRUE) # Sites + years

traits <- read.csv2("traits.csv", sep = ",", header = TRUE) # all traits?
# load(file = "traitsbee.Rdata") # Isolate bee traits

load(file = "networks/all_networks_years.Rdata") #
load(file = "networks/expanded_networks.Rdata") # Creer les network avec les interactions!


# load(file = "species/allSamples.Rdata") # matrice de plante et de pollinisateur?

# plants <- read.csv2("speciesChange/plants.csv", sep = ",", header = TRUE) # Colonist vs extinction???
# plants_change <- read.csv2("speciesChange/plants_change.csv", sep = ",", header = TRUE)
# plants_char <- read.csv2("speciesChange/plants_char.csv", sep = ",", header = TRUE)
# pollinators <- read.csv2("speciesChange/pollinators.csv", sep = ",", header = TRUE)
