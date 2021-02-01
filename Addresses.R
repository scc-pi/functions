# HEADER ------------------------------------------------------------------
# Functions to help with processing address data

library(tidyverse); library(magrittr); library(PostcodesioR)

# Add postcode variables to a dataframe via postcodes.io  
postcode_details_add <- function(df, pcd_var, .admin_district = TRUE, .lat_long = FALSE){
  
  # Check the function arguments make sense
  stopifnot(.admin_district | .lat_long) # at least one has to be true
  
  # We'll rename the postcode column back before it's returned 
  df <- df %>% 
    rename(postcode = {{pcd_var}})
  
  # Prepare an empty tibble to hold the cumulative results
  postcode_details <- tibble(postcode = character())
  if(.admin_district){
    postcode_details %<>%
      mutate(admin_district = character())
  }
  if(.lat_long){
    postcode_details %<>%
      mutate(longitude = numeric(),
             latitude = numeric())
  }
  
  # Subset of empty postcodes we add back at the end
  empty_postcodes <- df %>% 
    filter(is.na(postcode))
  if(.admin_district){
    empty_postcodes %<>% 
      mutate(admin_district = as.character(NA)) 
  }
  if(.lat_long){
    empty_postcodes %<>% 
      mutate(longitude = as.numeric(NA),
             latitude = as.numeric(NA))
  } 
  
  # Don't try and process empty postcodes  
  df %<>%
    drop_na(postcode)
  
  # postcode.io puts 100 row limit on bulk_postcode_lookup
  batch_size_max <- 100
  
  # Numbers involved in breaking postcode lookup into batches
  total_rows <- nrow(df)
  whole_batches <- total_rows %/% batch_size_max
  remainder_batch_size <- total_rows %% batch_size_max
  n_batches <- ifelse(remainder_batch_size == 0, whole_batches, whole_batches + 1)
  
  # Batch-by-batch postcode lookup
  for (i in 1:n_batches) {
    
    # Determine the size of the batch
    batch_size <- ifelse(i != n_batches, batch_size_max, remainder_batch_size)
    
    # Small probability (1 in 100) but need to check ...
    batch_size <- ifelse(batch_size == 0, batch_size_max, batch_size)
    
    # Determine the start and end records of the batch
    batch_start <- ifelse(i == 1, 1, ((i-1) * batch_size_max) + 1)
    batch_end <- ifelse(i == n_batches, 
                        (ifelse(i == 1, i, (i-1)) * batch_size_max) + remainder_batch_size,
                        i * batch_size_max)
    
    # Get a batch of petition records
    batch <- df %>% 
      slice(batch_start:batch_end)
    
    # Request to postcode.io
    bulk_lookup_rslt <- bulk_postcode_lookup(list(postcodes = batch$postcode))
    
    # Extract and format result from postcode.io 
    batch_rslt_tbl <- tibble(postcode = map_chr(bulk_lookup_rslt, c(1, 1), .default = ""))
    if(.admin_district){
      batch_rslt_tbl %<>%
        mutate(admin_district = map_chr(bulk_lookup_rslt, c(2, 17), .default = "not found"))
    }
    if(.lat_long){
      batch_rslt_tbl %<>%
        mutate(longitude = map_dbl(bulk_lookup_rslt, c(2, 7), .default = NA),
               latitude = map_dbl(bulk_lookup_rslt, c(2, 8), .default = NA))
    }
    
    # Cumulative results from batches
    postcode_details <- bind_rows(postcode_details, batch_rslt_tbl)
  }
  
  # Remove duplicates (so subsequent join works)
  postcode_details %<>%
    distinct(postcode, .keep_all = TRUE)
  
  # Format the dataframe we're returning
  df %<>%
    left_join(postcode_details, by = c("postcode")) %>% # Add postcode details to data
    bind_rows(empty_postcodes) %>% # Add null postcode records
    rename({{pcd_var}} := postcode) # Rename postcode column back to original name
}

# Extract the UPRNs from the feature created 
# by our llpg_world_geocode() Python function
extract_uprns <- function(path, feature){
  
  uprns <- st_read(dsn = path, layer = feature) %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(addr_type = str_extract(locator_family_id, ".+?(?=:)")) %>% 
    mutate(addr_type = str_replace_all(addr_type, "'", "")) %>% 
    mutate(uprn = str_extract(locator_family_id, "[^:]+$")) %>% 
    mutate(uprn = ifelse(str_length(uprn) == 11,
                         str_c("0", uprn),
                         uprn)) %>% 
    mutate(uprn  = ifelse(addr_type != "ADDRESS", NA, uprn)) %>% 
    select(-user_uprn, -starts_with("locator_")) %>% # Remove columns with prefix of "locator_" 
    rename_at(.vars = vars(starts_with("user_")),
              .funs = ~sub("^user_", "", .)) %>% # Remove prefix "user_" from column names
    arrange(desc(status), desc(addr_type), score)
}

# Add a multi-line address variable to a 
# partial single-line address variable
add_addr_field <- function(addr, field){
  if(is.na(field)){
    addr <- addr
  } else {
    ifelse(is.na(addr), 
           addr <- field, 
           addr <- str_c(addr, ", ", field))
  }
}

# Pass a list of multi-line address variables and get a 
# single-line (comma & space separated) address variable
combine_addr <- function(...){
  addr_fields <- list(...)
  addr <- NA
  for (i in seq_along(addr_fields)) {
    addr <- add_addr_field(addr, addr_fields[[i]])
  }
  return(addr)
}