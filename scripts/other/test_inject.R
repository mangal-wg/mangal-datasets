# Set writing directory
setwd("C:/Users/Dell_Gabriel/Desktop/StageGravel/importation_mangal")

# Set libraries
library(RPostgreSQL)
library(httr)

source("scripts/index/test_index.R")

### Il faut ouvrir la connection avec le terminal pour ouvrir nodemon:
### cd C:/Users/Dell_Gabriel/Desktop/StageGravel/mangal-backend-master >nodemon
### Moyen de le faire avec R?

# Connect to db
con <- dbConnect(PostgreSQL(),
                 host = "localhost",
                 port = 5432,
                 user = "postgres",
                 dbname = "mangal_dev")

  ### Enlever les braquettes! ###

# Que la ligne JSON
user.post <- final.list[[1]][[1]]

# Enlever les braquettes
user.post <- substr(user.post, 2, (nchar(user.post)-1))

#
POST("http://localhost:3000/api/v0/users", body = user.post, config = add_headers("Content-type" = "application/json"))
