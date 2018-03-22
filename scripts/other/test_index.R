source("scripts/clean/test_clean.R")

# *** Regler avec fct GET_id ***
# Interactions -> associate species to their id 
#interactions[,"taxon_1"] <- NA
#interactions[,"taxon_2"] <- NA

# for (i in 1:length(taxons.df)) {
  
 # for (j in 1:length(interactions)) {
    
   # if(taxons.df[1,i] == interactions[1,j]){
    # interactions[7,j] <- taxons.df[2,i]}
    
   # if(taxons.df[1,i] == interactions[2,j]){
    # interactions[8,j] <- taxons.df[2,i]}
 # }
# }



# Interactions as list
interactions.list <- setNames(split(interactions, seq(nrow(interactions))), rownames(interactions))

# List as JSON
for (i in 1:length(interactions.list)) {
  interactions.list[[i]] <- toJSON(interactions.list[[i]])
}

# Table users
# Create
users <- data.frame(id = 1, name = "Hocking", email = "exemple@exemple.com")

# As list
users.list <- setNames(split(users, seq(nrow(users))), rownames(users))

# List to JSON
for (i in 1:length(users.list)) {
  users.list[[i]] <- toJSON(users.list[[i]])
}

# Table datasets
# Create
datasets <- data.frame(id = 1, name = "Hocking's network", public = TRUE, ref_id = 1)

# As list
datasets.list <- setNames(split(datasets, seq(nrow(datasets))), rownames(datasets))

#List to JSON
for (i in 1:length(datasets.list)) {
  datasets.list[[i]] <- toJSON(datasets.list[[i]])
}

# Table networks
# Create
networks <- data.frame(id = 1, name = "Pollinisator_test", public = TRUE, dataset_id = 1, user_id = 1)

# As list
networks.list <- setNames(split(networks, seq(nrow(networks))), rownames(networks))

# List to JSON
for (i in 1:length(networks.list)) {
  networks.list[[i]] <- toJSON(networks.list[[i]])
}

# Bind list
final.list <- list(users = users.list, datasets = datasets.list, networks = networks.list, interactions = interactions.list) 

