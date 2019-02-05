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

# lat  <- 0
# lon  <- 0
srid <- 4326
folder_name <- "havens_1992"

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "DESCRIPTION",
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

# enviro <- list(name  = "attribute name",
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)

users <- list(name         = "Clément VIOLET",
              email        = "clement.violet@etudiant.univ-brest.fr",
              orcid        = "0000-0001-6217-5891",
              organization = "Université de Bretagne Occidentale",
              type         = "administrator")

dataset <- list(name        = "havens_1992",
                 date        = "1984-06-01",
                 description = "Pelagic communities of small lakes and ponds of the Adirondack",
                 public      = TRUE)

network <- list(name             = "havens_1992",
                 date             = "1984-06-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Food web Pelagic communities of small lakes and ponds of the Adirondack",
                 public           = TRUE,
                 all_interactions = TRUE)

inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1984-06-01",
              direction     = "directed",
              # type          = "Predation",
              method        = "null",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file

metaweb <- read.table(paste0("mangal-datasets/", folder_name, "/Raw/metaweb_Havens.txt"))

presence <- read.table(paste0("mangal-datasets/", folder_name, "/Raw/presence_Havens.txt"))

lake_properties <- read_table2(paste0("mangal-datasets/", folder_name, "/Raw/Havens_data.txt"),
                               col_type = cols(.default = col_double(), Lake = col_character(),
                                    Latitude = col_character(), Longitude = col_character())) %>%
  select(1:2, Latitude:DOC) %>%
  mutate(Latitude = str_replace_all(.$Latitude, "\\.", " "),
         Longitude = str_replace_all(.$Longitude, "\\.", " ")) %>%
  mutate(Latitude = paste(.$Latitude, "00"),
         Longitude = paste(.$Longitude, "00")) %>%
  mutate(Latitude = round(as.numeric(conv_unit(.$Latitude, from = "deg_min_sec", to = "dec_deg")),2),
         Longitude = round(as.numeric(conv_unit(.$Longitude, from = "deg_min_sec", to = "dec_deg")),2)) %>%
  arrange(Lake_nb) %>%
  select(-1)
name_lake <- lake_properties$Lake
lake_properties$Lake <- str_c("Env_", lake_properties$Lake)
lake_properties <- lake_properties %>%
  split(lake_properties$Lake) %>% # Split into a list of df
  map(~select(.x, -1)) # Remove column with lake name

sp_name_txt <- read_table2(paste0("mangal-datasets/", folder_name, "/Raw/Havens_sp_list.txt"),
                           col_types = cols("i", "c", "i", "i", "i", "i", "i","c")) %>%
  select(-(3:8) )%>% # 3:7 are useless columns, 8th column is empty
  filter_all(all_vars(!is.na(.))) # Remove last two lines : full of NA

sp_name_PRN <- read.table(paste0("mangal-datasets/", folder_name, "/Raw/Food_Web/TAXCODES.PRN"),
                          sep = "\t", skip = 7, colClasses = c("character", NULL), nrow = -1) %>%
  {str_remove(unlist(.),"\\s\\([^()]*\\)\\s*.*")} %>% # Remove last part with "(...)...."
  str_squish() %>% # Remove whitespaces begining and ending of the sp name
  enframe(name = NULL, value = "X1") # Must be df to start cleanning it

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

# sp_name <- sp_name %>%
#   select(-1) %>% # Remove unused variable
#   unlist() %>% # Coerce to vector
#   unname() %>% # Remove elements names
#   as.factor() %>%
#   fct_inorder() # Reorder factor


# name_file <- paste0("mangal-datasets/", folder_name, "/raw/Food_Web") %>% # Get the name of all file to name the list after each file
#   dir_ls() %>%
#   as.character() %>% # Convert the fs class object to character
#   discard(str_detect(., "TAXCODES.PRN")) %>% # This file containt only info about taxa, remove it
#   str_remove_all(path(paste0("mangal-datasets/", folder_name, "/raw/Food_Web/"))) %>% # Cleaning name each file
#   str_remove_all(".PRN") %>%
#   str_remove_all("[:punct:]")

data_file <- paste0("mangal-datasets/", folder_name, "/raw/Food_Web") %>% #Getting all the file into one list
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

# data_file <- data_file[1]

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
  map(~rownames_to_column(.x, "sp_taxon_1")) %>%
  map(~gather(.x, "sp_taxon_2", "value", -sp_taxon_1)) %>% #Convert large df to long format
  map(~filter(.x, value != 0)) # Remove 0 interaction


FW_name <- data_file %>% # Construct the Food Web matric to inject
  map(~filter(.x, !is.na(select(.x,1)))) %>%
  map2(modify(file_row_name, ~.x), ~mutate(.x, sp_id = .y)) %>%
  map(~column_to_rownames(.x, "sp_id")) %>% # Store sp name as row name
  map2(modify(file_col_name, ~.x), ~{`names<-`(.x, .y)}) %>%
  map(~rownames_to_column(.x, "sp_taxon_1")) %>%
  map(~gather(.x, "sp_taxon_2", "value", -sp_taxon_1)) %>% #Convert large df to long format
  map(~filter(.x, value != 0)) %>%# Remove 0 interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(Cryptomonas\\sovata)|(C\\.\\serosa)|(Peridinium\\slimbatum)|
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
                                           (R\\.\\sgorksii)|(R\\.\\ssp\\.)|(Anacystis\\sincerta)|(Microsystis\\ssp\\.)|(benthic\\sdetritus)|(periphyton)"), "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Dd]ebris.*)|(.*[Dd]etri*)"), "commensalism", .x$type)))

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
  map(~enframe(.x, name = NULL, value = "original_name"))

test_name <- FW_name %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
  map(~gather(.x, id, sp)) %>%
  modify(~deframe(.x)) %>%
  map(~unique(.x)) %>%
  map(~str_remove_all(.x,"\\s\\(.*\\)$")) %>%
  set_names(paste0("Taxa_", name_lake)) %>%
  map(~enframe(.x, name = NULL, value = "original_name")) %>%
  map_dfr(~.x) %>%
  unique() %>%
  as.data.frame()

C\\. vernalis|periphyton|Peridinium limbatum|P. wisconsinense|P. inconspicuum

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
  str_remove_all("\\ssp.*$") %>%
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
taxa_df_global$name_clear[89] <- "C"
taxa_df_global$name_clear[90] <- "C"
taxa_df_global$name_clear[96] <- "Manfredium eudactylotum"
taxa_df_global$name_clear[99] <- "Eucentrum"
taxa_df_global$name_clear[108] <- "Euglena"
taxa_df_global$name_clear[121] <- "Elakatothrix gelatinosa"
taxa_df_global$name_clear[126] <- "Tetraedron caudatum"
taxa_df_global$name_clear[131] <- "C"
taxa_df_global$name_clear[146] <- "Dimorphococcus lunatus"
taxa_df_global$name_clear[161] <- "Fragilaria crotonensis"
taxa_df_global$name_clear[169] <- "Mallomonas hamata"
taxa_df_global$name_clear[170] <- "Mallomonas acaroides"

taxa_df <- taxa %>%
  modify(~inner_join(.x, taxa_df_global, by = "original_name"))

# taxa_back_sp <- taxa %>%
#   map(~word(.x[str_detect(.x,"([:digit:])|(sp\\.$)")]))
#
# taxa_back <- taxa %>%
#   map(~str_subset(.x, "^(?!.*sp\\.)")) %>%
#   map2(taxa_back_sp,~c(.x, .y))
#
#
# ## Get taxa with "sp"
# taxa_back_sp <- taxa %>%
#   enframe(name = NULL, value = "taxa") %>%
#   map_df(~word(.x[str_detect(.x,"([:digit:])|(sp.$)")])) %>%
#   na.omit()
#
# ## Get taxa with "()"
# taxa_back_parenthesis <- taxa %>%
#   enframe(name = NULL, value = "taxa") %>% #Create a tibble from a vector
#   filter(!str_detect(taxa, fixed("Salvelinus fontinalis x Salvelinus namaycush (splake)")) & str_detect(taxa, "\\(|\\)")) %>%
#   map_dfr(~word(.x, end = 2))
#
# ## Special case with this one
# taxa_back_parenthesis <- taxa %>%
#   enframe(name = NULL, value = "taxa") %>%
#   filter(str_detect(taxa, fixed("Salvelinus fontinalis x Salvelinus namaycush (splake)"))) %>%
#   str_replace(fixed("Salvelinus fontinalis x Salvelinus namaycush (splake)"), "Salvelinus fontinalis x Salvelinus namaycush") %>%
#   rbind(taxa_back_parenthesis,.)
#
# ## Getting all back together
# taxa_back <- taxa %>%
#   enframe(name = NULL, value = "taxa") %>%
#   filter(!str_detect(taxa,"([:digit:])|(sp.$)") & !str_detect(taxa, "\\(|\\)")) %>%
#   rbind(., taxa_back_parenthesis) %>%
#   unlist() %>%
#   unname()
#
# rm(taxa_back_parenthesis, taxa_back_sp)


## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- taxa_df %>%
  map(~unlist(.x$name_clear)) %>%
  map(~unname(.x)) %>%
  flatten_chr() %>%
  unique() %>%
  map_chr(~{modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", str_replace_all(.x, " ", "%20")))}) %>%
  map_chr(~str_replace_all(.x, ".*,%20.*", "_")) %>%
  map_chr(~str_replace_all(.x, ".*%20-%20.*", "-")) %>%
  map_chr(~str_replace_all(.x, "\\.%20", "__")) %>%
  keep(~length(content((GET(url = .x, config = add_headers("Content-type" = "application/json","Authorization" = paste("bearer", readRDS("mangal-datasets/.httr-oauth"))))))) == 0) %>%
  map_chr(~str_remove_all(.x, fixed("http://poisotlab.biol.umontreal.ca/api/v2/taxonomy/?name="))) %>%
  map_chr(~str_replace_all(.x, fixed("%20"), " ")) %>%
  map_chr(~str_replace_all(.x, fixed("__"), ". ")) %>%
  map_chr(~str_replace_all(.x, fixed("_"), ", "))

taxa_back_df <- taxa_back_df %>%
  enframe(name = NULL, value = "name") %>%
  mutate(bold = as.double(unlist({map(.$name,~get_boldid(.x, row = 5, verbose = FALSE)[1])})),
         eol = NA_real_, #Add NA in eol column : See taxize issue : #718 EOL: maybe completely remove the data source from taxize
         tsn = as.double(unlist({map(.$name,~get_tsn(.x, row = 5, verbose = FALSE)[1])})),
         ncbi = as.double(unlist({map(.$name,~get_uid(.x, row = 5, verbose = FALSE, key = "679d0a26947d9b6432371b268ec0c7b39b08")[1])}))) # Add API KEy for NCBI

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets/FW_name/data/FW_name_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxa_df, file = "mangal-datasets/FW_name/data/FW_name_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "mangal-datasets/FW_name/data/FW_name_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
# Common table
##Attributes
POST_attribute(attr_inter)
POST_attribute(attr_altitude)
POST_attribute(attr_area)
POST_attribute(attr_max_depth)
POST_attribute(attr_lake_watershed_area)
POST_attribute(attr_volume)
POST_attribute(attr_prws)
POST_attribute(attr_runoff)
POST_attribute(attr_Etws)
POST_attribute(attr_Prlake)
POST_attribute(attr_Evapl)
POST_attribute(attr_Outflow)
POST_attribute(attr_Th)
POST_attribute(attr_pH)
POST_attribute(attr_ANC)
POST_attribute(attr_Conduct)
POST_attribute(attr_SO)
POST_attribute(attr_NO)
POST_attribute(attr_Ca)
POST_attribute(attr_Al)
POST_attribute(attr_DOC)

## Others
POST_ref(ref)
POST_user(users)
POST_dataset(dataset, users, ref)
POST_taxa_back(taxa_back)

POST_network(network_lst = , enviro = enviro, dataset, users)
POST_taxon(taxa_df)

# Unique table

POST_environment(enviro, attr_)
POST_interaction(inter_df = FW_name, inter = inter, enviro = enviro, attr = attr_inter, users)
POST_network(network_lst = , enviro = enviro, dataset, users)
POST_taxon(taxa_df)


map(lake_properties, ~POST_environment(enviro = list(name  = "Nom_du_lac",
                                                     lat   = "Lattitude du lac",
                                                     lon   = "Longitude du lac",
                                                     srid  = srid,
                                                     date  = "1111-11-11",
                                                     value = "Valeur du paramètre environnemental")),
                                       attr = ))

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)
