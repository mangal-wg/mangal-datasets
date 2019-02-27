sep_pooled <- function(x, sep){
  
  require(tidyr)
  require(stringr)
  require(dplyr)
  require(tibble)
  
  # Arguments :
  # x An interaction matrix
  # sep Separator delimiting collapsed value. Must be a regular expression
  
  # Return :
  # A list returning :
  #   First element is the interaction matrix dataframe with no pooled elements.
  #   Second element is the name of the species that were pooled (ready to inject into gnr_resolve)
  
  # Exemple 
  # df <- data.frame(sp_taxon_1 = c("sp1", "sp2", "sp3", "sp4 and sp6"),
  #                 sp_taxon_2 = c("sp7 and sp8", "sp9", "sp11 and sp12 and sp13", "sp10"),
  #                 value = rep(1, times = 4),
  #                 stringsAsFactors = FALSE)
  # 
  # sep_pooled(df, sep = "\\sand\\s")
    
  #
  if(!is.data.frame(x)){
    stop("x must be a dataframe !")
  }
  if(!is.character(sep)){
    stop("sep must be a character vector !")
  }
  
  depooled_temp <- mutate(x, sp_taxon_1_temp = str_split(x$sp_taxon_1, sep), # "Depooled" the pooled rows. Each sp_taxon rows of the dataframe is now a list.
                             sp_taxon_2_temp = str_split(x$sp_taxon_2, sep))
  depooled_temp <- unnest(depooled_temp, sp_taxon_1_temp, .preserve = c(sp_taxon_1, sp_taxon_2, sp_taxon_2_temp, value, type)) # Coerce the sp_taxon_1 lists to observations (duplicate the paired observation from the others columns)
  depooled_temp <- unnest(depooled_temp, sp_taxon_2_temp, .preserve = c(sp_taxon_1, sp_taxon_2, sp_taxon_1_temp, value, type)) # Coerce the sp_taxon_2 lists to observations (duplicate the paired observation from the others columns)
  
  name_to_taxize <- unique(unlist(str_split(str_subset(unlist(select(filter(depooled_temp, str_detect(depooled_temp$sp_taxon_1, sep) | str_detect(depooled_temp$sp_taxon_2, sep)), sp_taxon_1, sp_taxon_2)), sep), sep))) # Get the name of the depooled species
  
  depooled <- select(depooled_temp, sp_taxon_1 = sp_taxon_1_temp, sp_taxon_2 = sp_taxon_2_temp, value, type) # Formating the depooled dataframe
  return(list(depooled = depooled, name_to_taxize))
}