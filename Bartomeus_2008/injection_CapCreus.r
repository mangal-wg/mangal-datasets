# Templet filled for CapdeCreus data by I. Bartomeus using:

# Ben: Do I understand that you don't want to inject the data from the last 6 network?: MED3CA, MED4CA, MIQ1OP, MIQ2OP, SEL1OP, SEL2OP., or it's a "mistake" in the ntw_data where you loop over only the 6 first network?
# Nacho: A mistake, I want all 12 in :D

# R Guidelines / Template for Mangal data injection
# More information on Mangal and API Endpoints at https://mangal.io and https://mangal.io/doc/api/
# This guideline is an aggregation of different script parts coming from different people at the Mangal project.
# Author of this guideline : Benjamin Mercier
# Date : August 2020

# All caps field must be filled, null fields are optional.
# Use one formatting/injection script per dataset. A dataset can contain multiple networks/sites.

###################################################################################################
# Set the metadata
# Fill the information in the lists below to the best of your knowledge

#load data needed later on: 
data <- read.csv("data/CapCreusNtw.csv", stringsAsFactors = FALSE)
#head(data)


# Reference
reference <- list(doi = "10.1007/s00442-007-0946-1", # desirable
                  jstor = "null",
                  pmid = "null", # Might not have one? 
                  paper_url = "https://doi.org/10.1007/s00442-007-0946-1", # desirable
                  data_url = "null", 
                  first_author = "Ignasi Bartomeus",
                  year = "2008",
                  bibtex = "@article{bartomeus_contrasting_2008,
	                          title = {Contrasting effects of invasive plants in plant-pollinator networks},
	                          volume = {155},
	                          issn = {1432-1939},
	                          url = {https://doi.org/10.1007/s00442-007-0946-1},
	                          doi = {10.1007/s00442-007-0946-1},
	                          abstract = {The structural organization of mutualism networks, typified by interspecific positive interactions, is important to maintain community diversity. However, there is little information available about the effect of introduced species on the structure of such networks. We compared uninvaded and invaded ecological communities, to examine how two species of invasive plants with large and showy flowers (Carpobrotusaffine acinaciformis and Opuntiastricta) affect the structure of Mediterranean plant-pollinator networks. To attribute differences in pollination to the direct presence of the invasive species, areas were surveyed that contained similar native plant species cover, diversity and floral composition, with or without the invaders. Both invasive plant species received significantly more pollinator visits than any native species and invaders interacted strongly with pollinators. Overall, the pollinator community richness was similar in invaded and uninvaded plots, and only a few generalist pollinators visited invasive species exclusively. Invasive plants acted as pollination super generalists. The two species studied were visited by 43\\% and 31\\% of the total insect taxa in the community, respectively, suggesting they play a central role in the plant-pollinator networks. Carpobrotus and Opuntia had contrasting effects on pollinator visitation rates to native plants: Carpobrotus facilitated the visit of pollinators to native species, whereas Opuntia competed for pollinators with native species, increasing the nestedness of the plant-pollinator network. These results indicate that the introduction of a new species to a community can have important consequences for the structure of the plant-pollinator network.},
	                          number = {4},
	                          journal = {Oecologia},
	                          author = {Bartomeus, Ignasi and Vilà, Montserrat and Santamaría, Luís},
	                          month = apr,
	                          year = {2008},
	                          pages = {761--770}
                            }")
                  
# User
# Not usefull right now for people outside the Mangal project since they can't upload data yet.
user <- list(name = "Ignasi Bartomeus",
             email = "nacho.bartomeus@gmail.com", # Email where you can be reached
             orcid = "0000-0001-7893-4389", # ORCID_ID
             organization = "Estación Biológica de Doñana", # Ex: "Université de Sherbrooke"
             type = "user") # %in% c("administrator", "user")

# Dataset
dataset <- list(name        = "Bartomeus_2005",
                date        = "2005-00-00",
                description = "Plant-pollinator networks collected in Mediterranean scrublands. There are six pairs of netwoks, and each pair has one invaded by exotic species site and a control site separated ~ 200 m", #Ex: "Food web structure of rocky intertidal communities in New England and Washington"
                public      = TRUE) #Is this available publicly

# Network
# If only one network (or if lat/lon and description doesn't vary between networks) in the dataset proceed to fill once the network list below
# If there is multiple networks, AND if the description/latitude/longitude varies between networks, what I used to do was to have .txt or .csv file (or can create a data_frame in R directly) with 3 columns.
# The columns were "network_description", "latitude", "longitude", and I for-looped over it during the injection since we can only inject one table at a time.
ntw_data <- data.frame(name = c("BAT1CA", "BAT2CA", "FRA1OP", "FRA2OP", 
                                "MED1CA", "MED2CA", "MED3CA", "MED4CA", 
                                "MIQ1OP", "MIQ2OP", "SEL1OP", "SEL2OP"),
                       network_description = paste("plant-pollintor network sampled in Cap de Creus region, Spain", 
                                                   c("", "; this site is invaded by Carpobrotus affine acinaciformis", 
                                                         "", "; this site is invaded by Opuntia stricta",
                                                         "", "; this site is invaded by Carpobrotus affine acinaciformis",
                                                         "", "; this site is invaded by Carpobrotus affine acinaciformis",
                                                         "", "; this site is invaded by Opuntia stricta",
                                                         "", "; this site is invaded by Opuntia stricta"), sep = ""),
                       latitude = c(42.352,
                                    42.354,
                                    42.417,
                                    42.417,
                                    42.324,
                                    42.323,
                                    42.320,
                                    42.319,
                                    42.398,
                                    42.398,
                                    42.301,
                                    42.300),
                       longitude = c(3.177,
                                     3.175,
                                     3.158,
                                     3.160,
                                     3.293,
                                     3.297,
                                     3.305,
                                     3.303,
                                     3.147,
                                     3.150,
                                     3.229,
                                     3.231))

head(ntw_data)

# Attribute
# Used to describe an attribute that can be linked to Interaction table, Trait table and Environment table.
attribute <- list( name        = "Frequency", # Interaction: "Presence/Absence", "Frequency" etc., Trait: "body length", "mass" etc., Environment: "precipitation" etc.
                   table_owner = "interaction", # %in% c("interaction","trait","environment") 
                   description = "Frequency of interaction", # Interaction: "Presence or absence of interaction", Trait: "Body length of the organism", Environment: "Quantity of precipitation".
                   unit        = "Number of visits recorded per 6 minutes") # necessary if  it's a Trait or Environment attribute, facultative if Interaction attribute (ex: no units for Presence/Absence)

####################################################################################################

# Interaction
# If there is multiple network, separate the interaction data_frame by networks/sites into a list. 
# The interaction "matrices" have to be long format (NO ADJACENCY MATRICES):
# First column is named "sp_taxon_1" (making the interaction), second column is named "sp_taxon_2" (receiving the interaction) and third column named "value" is the interaction value.
# Interaction value is either 1/0 if it's Presence/Absence, or a whole/real positive number if it's a frequency or relative diet.

# All the informations in the list below can vary between each interaction in the network. If they don't vary, just fill once the list below.
# If they vary, they can be "manually" added as a column to the LONG FORMAT interaction matrix.
# For example, in the same network, we could have predators eating their prey (type = "predation") and herbivores grazing on plants (type = "herbivory")
# This way, we "cbind" a column named "type", and on each line where we have predation, the value of the column "type" will be "predation". (Same logic for the herbivory)
# If an argument varies in a network, remove it from the list below. (Because in the injection process, each argument is cbinded to the long format interaction matrix)

#NOTE from NACHO: I am bit lost here. I can't find where the interactions are stored. 
#would it work something like
# Ben: Yes, with slight changes!
interaction_data <- data.frame(network= data$site, #if needed inject per network looping through the 12 sites
                               sp_taxon_1 = data$gen_sp,
                               sp_taxon_2 = data$plant,
                               value = data$freq,
                               date = paste0(data$year, "-", data$month, "-", data$day)) # Ben: added the date here and removed it from the list below since it varies among the different interaction.
  
interaction_data$date <- as.Date(interaction_data$date, format = "%Y-%m-%d") # Ben: Reformated the date to have the 0 for the months and days.
interaction_data <- split(interaction_data, f = interaction_data$network, drop = TRUE) # Ben : The information in the dataframe is exactly what we need. What I used to do though was separate each dataframe per network into a list, so I could embedded the injection of the network data and the interaction data in the same "for loop", as the "i" will get the related data from network and interaction.
interaction_data <- lapply(interaction_data, function(x) {x[,-1]}) # Ben: Remove the first column as we don't need it in the database. It's going to be related by a Foreign Key, generated when injected.

####################################################################################################

# Node
# (Might be a weird table to make if the taxonomy is already well resolved, but we need it because sometimes in the original interaction matrices there are names like : Canis sp. so we want to link Canis sp. to only the genus "Canis" in the Taxonomy table.)
# Same as the interaction data, separate each nodes (taxon) by networks/sites into a list.
# We need a data_frame with two columns:
# First column (original_name): Names of all the taxons that were originally used in the interaction matrix, might be common name
# Second column (clear_name): Names that were taxonomically resolved i.e.: no "sp.", no whitespaces etc.
# To get the name resolved (if they aren't already) I used to use taxize::gnr_resolve()
# Ben: 
library(taxize)
# Creating the node dataframe for each network. First column is the original_name found in the network, and second column (name_clear) are the names resolved i.e.: without "sp" etc.
nodes <- interaction_data
nodes <- lapply(nodes, function(x) unique(c(x$sp_taxon_1, x$sp_taxon_2))) #NACHO: This is not working for me¿? Returns numbers, not taxon names
#nodes <- lapply(nodes, function(x) unique(x[,c(1,2)])) #This works
nodes_temp <- lapply(nodes, function(x) gsub("[A-Z]{1}[[:digit:]]{1,}$", "", x)) # Removing the capital letters/numbers at the end, and the "sp".
nodes_temp <- lapply(nodes_temp, function(x) gsub(" sp", "", x))
#This last two lapply do not work (give indexes, not names) but I can't figure why¿?
nodes <- purrr::map2(nodes, nodes_temp, ~cbind(as.data.frame(.x), as.data.frame(.y))) # Cbinding the two back together into a dataframe
#head(nodes)

# Getting the name resolved
resolved_nodes <- lapply(nodes, function(x) {as.data.frame(taxize::gnr_resolve(x[,".y"], canonical = TRUE, highestscore = TRUE, best_match_only = TRUE))})
NA_nodes <- lapply(resolved_nodes, function(x) attributes(x)$not_known) # Getting the taxons that aren't recognized. Will check them manually on ITIS/GBIF/Internet..

# Manually fixing the taxons in the second column that were not found with taxize:gnr_resolve and are stored in NA_nodes.
# Coul you check the taxonomy modifications I made, and point modify it if it's not correct?
nodes <- lapply(nodes, function(x) {x[,".y"] <- stringr::str_replace_all(x[,".y"], c("Psylotrix viridicoerulea" = "Psilotrix viridicoerulea","Chryptocephalus" = "Cryptocephalus", "Criptocephalus" = "Cryptocephalus", "Sphaerophoeria" = "Sphaerophoria", "Equium sabulicola" = "Echium sabulicola", "Dorichnium pentaphylum" = "Dorycnium pentaphyllum", "Myrabilis quadripunctata" = "Mylabris quadripunctata", "Sirphidae" = "Syrphidae", "Vanesa atlantica" = "Vanessa atalanta", "Lassiopogon" = "Lasiopogon")); return(x)})
#Nacho: yes, all modifications are correct, THANKS!

# Regetting the name resolved
resolved_nodes <- lapply(nodes, function(x) {as.data.frame(taxize::gnr_resolve(x[,".y"], canonical = TRUE, highestscore = TRUE, best_match_only = TRUE))})

# Matching the name back into nodes
nodes <- purrr::map2(nodes, resolved_nodes, ~dplyr::left_join(.x, .y, by = c(".y" = "user_supplied_name"))) # Matching back the resolved name to the original_name
nodes <- lapply(nodes, function(x) x[,c(".x", "matched_name2")]) # Selecting only the column we need
nodes <- lapply(nodes, function(x) `colnames<-`(x, c("original_name", "name_clear"))) # Changing column names

# *** Have to be careful because taxize::gnr_resolve(), sometimes give back a resolved name that isn't as precise as the name we supplied ***
# Example: in the second network, "Carpobrotus affinis acinaciformis" got downgraded to "Carpobrotus"
# I found to still use taxize::gnr_resolve() and give a visual inspection after is still the fastest way to resolve that taxonomy to the best possible resolution (because most network don't have a lot of species, so a visual inspection is usually quick).
# If you have an idea that could be faster please don't hesitate to share it with me!

# So here I rechanged to their original_name the nodes that were downgraded to a lesser resolved taxonomy
nodes[[2]][which(nodes[[2]]$name_clear == "Carpobrotus"),"name_clear"] <- "Carpobrotus affinis acinaciformis"
nodes[[6]][which(nodes[[6]]$name_clear == "Carpobrotus"),"name_clear"] <- "Carpobrotus affinis acinaciformis"
nodes[[8]][which(nodes[[8]]$name_clear == "Carpobrotus"),"name_clear"] <- "Carpobrotus affinis acinaciformis"

####################################################################################################

# Taxonomy
# Check which taxon are already in the Mangal Taxonomy Endpoint
# We will only add the ones that aren't already in Mangal

taxa_back <- unique(do.call(rbind, nodes)$name_clear)

server <- "https://mangal.io"
taxa_back_df <- data.frame()

for (i in 1:length(taxa_back)) {
  
  path <- httr::modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", gsub(" ", "%20", taxa_back[i])))
  if (length(httr::content(httr::GET(url = path, config = httr::add_headers("Content-type" = "application/json")))) == 0) {
    
    taxa_back_df[nrow(taxa_back_df)+1, 1] <- taxa_back[i]
  }
}

# Create column to store the different taxonomy database IDs
taxa_back_df[, "bold"] <- NA
taxa_back_df[, "eol"]  <- NA
taxa_back_df[, "tsn"]  <- NA
taxa_back_df[, "ncbi"] <- NA
taxa_back_df[, "gbif"] <- NA
taxa_back_df[, "col"] <- NA
taxa_back_df[, "rank"] <- NA

# Loop to get the IDs for each taxon
for (i in 1:nrow(taxa_back_df)) {
  try (expr = (taxa_back_df[i, "bold"] <- get_boldid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, "eol"] <- get_eolid(taxa_back_df[i, 1], row = 5, verbose = FALSE)), silent = TRUE) # Might need a key?
  try (expr = (taxa_back_df[i, "tsn"] <- get_tsn(taxa_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, "ncbi"] <- get_uid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
}

for(i in 1:nrow(taxa_back_df)){
  try(expr = taxa_back_df[i, "gbif"] <- as.gbifid(get_gbifid(taxa_back_df[i, "name"])))

}
########################################################################################################

# Environment
#NOTE: I have no environment variables, so I ignore this part.
# Coming soon
# Example:
# The attribute related to the environmental data
# enviro_attribute <- list(name        = "NAME OF THE ENVIRONMENT ATTRIBUTE", # Ex: "annual rainfall"
#                          table_owner = "environment",
#                          description = "BRIEF DESCRIPTION", # Ex: "The annual rainfall of the study area"
#                          unit        = "UNITS") # Ex: "mm"
# The actual environmental data
# enviro_rain <- list(name  = "NAME OF THE ENVIRONMENTAL DATA", # Ex: "annual rainfall"
#                lat   = LAT, # Latitude
#                lon   = LON, # Longitude
#                srid  = SRID, # Spatial reference system
#                date  = "YEAR-MONTH-DAY", # Date of the recorded environmental data
#                value = VALUE) # Value 

#########################################################################################################

# Trait
#NOTE: I have no trait variables, so I ignore this part.
# Coming soon
# Example:
# The attribute related to the trait data
# trait_attribute <- list(name        = "NAME OF THE TRAIT ATTRIBUTE", # Ex: "Body size"
#                         table_owner = "trait",
#                         description = "Average body length", # Ex: "Average body length"
#                         unit        = "UNITS") # Ex: "mm"
# The actual trait data
# Create or import a data_frame with three columns:
# First column is the taxons names ("taxon")
# Second column is the trait name ("name")
# Third column is the trait value ("value")

##########################################################################################################

# Injection
POST_attribute(attr = attribute)

POST_ref(ref = reference)

POST_users(users = user)

POST_dataset(dataset = dataset, users = user, ref = reference)

POST_taxonomy(taxo = taxa_back_df)


for(i in 1:12){ ##WARNING: Now it overscripts network each time, so injection has to bee added into the loop. Ben: That is exactly what I used to do!
  network <- list(name = paste0("Bartomeus_2008_", ntw_data$name[i]), # Just added the year after the name.
                  date = "2005-00-00",
                  lat = ntw_data$latitude[i], # Latitude
                  lon = ntw_data$longitude[i], # Longitude
                  srid = 3857, # Spatial reference system
                  description = ntw_data$network_description[i], # Might bring more precision than the dataset description ex: "Food web structure of an exposed rocky shore community, Pemaquid point, New England"
                  public = TRUE, # Are the data publicly available
                  all_interactions = FALSE) # Is the network recording ALL presence AND absence of interactions

# Ben: I removed the date field, because it was a field that varied between interaction, so I added it to the interaction dataframe.
  interaction <- list(direction     = "UNDIRECTED", # Direction of the interaction
                      type          = "MUTUALISM",
                      method        = "OBSERVATION", # The general method with which the interaction was recorded
                      description   = "null", # Not necessary
                      public        = TRUE, # Are the data publicly available
                      lat           = ntw_data[i,"lat"], # Latitude #QUESTION: Can we extract this from the network table? Ben: Yes that is exactly what I used to do!!!
                      lon           = ntw_data[i, "lon"], # Longitude
                      srid          = 3857) # Spatial reference system

  POST_network(network_lst = network, dataset = dataset, users = user)
  
  POST_node(node_df = nodes[[i]], network = network)
  
  POST_interaction(inter_df = interaction_data[[i]], inter = interaction, attr = attribute, users = user, network = network)
}


