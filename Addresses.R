# HEADER ------------------------------------------------------------------
# Functions to help with processing address data

library(tidyverse); library(magrittr); library(PostcodesioR); library(janitor)

# Add postcode variables to a data frame via PostcodesioR ----------------  
# TODO: Fix pcd_name other than default
# TODO: Handle incorrect spelling of vars
add_pcd_vars <- function(df, pcd_name = "postcode",
                         .admin_district = TRUE, .lat_long = FALSE, 
                         other_vars = character(0)){

  ## Build a vector of variables of interest ----
  
  # Create an empty character vector  
  pcd_vars <- character(0)

  # From the arguments passed in the function call
  if(.admin_district) pcd_vars %<>% append("admin_district")   
  if(.lat_long) pcd_vars %<>% append(c("longitude", "latitude"))
  if(length(other_vars)>0) pcd_vars %<>% append(other_vars)   
  
  # Remove any duplicates
  pcd_vars %<>% unique()
  
  ## Build a postcode lookup template and a list of columns in the template ----
  
  # Empty postcode lookup data frame template
  pcd_template <- postcode_lookup("S1 2HH") %>%
    filter(postcode == "ZZ ZZZ")
  
  # Data frame of postcode lookup column names, types & indices
  pcd_cols <- tibble(name = colnames(pcd_template), 
                     type = lapply(pcd_template, class)) %>% #TODO: lose or use & extract from list
    rowid_to_column(var = "rowid") %>% 
    relocate(rowid, .after = last_col()) 
  
  # The position of "codes" helps with nested "*_code" values
  min_codes_var <- pcd_cols %>% 
    filter(str_ends(name, "_code")) %>% 
    filter(rowid == min(rowid))
  
  codes_position <- min_codes_var$rowid
  
  # Add index used to extract var from results
  pcd_cols %<>% 
    mutate(index = map(rowid, 
                       ~ ifelse(.x < codes_position,
                                list(c(2, .x)),
                                list(c(2, 
                                       codes_position, 
                                       .x + 1 - codes_position))))) %>% 
    select(-rowid)
  
  # Filter list of postcode columns to vars of interest
  pcd_cols %<>% filter(name %in% pcd_vars)
  
  # Filter template columns to vars of interest
  pcd_details <- select(pcd_template, all_of(pcd_vars))
  
  # Need to distinguish between postcode passed & matched e.g. s1 2hh & S1 2HH
  pcd_match <- "postcode" %in% colnames(pcd_details) # flag is used more than once 
  if(pcd_match) pcd_details %<>% mutate(pcd_match = character(0)) 
  
  ## Do some checks -----
  
  # Check we have some postcode variables in the function arguments
  stopifnot(length(pcd_vars) > 0) 
  
  # Check we have some VALID postcode variables in the function arguments
  stopifnot(nrow(filter(pcd_cols, name %in% pcd_vars)) > 0) 
  
  # Warn if there's an unused postcode variable from the function arguments
  if(length(pcd_vars)!=length(pcd_cols)) {
    warnings(str_c("One or more of the variables requested is not available. ",
                   "Check spelling and docs.ropensci.org/PostcodesioR/"))
  }
  
  ## Handle empty & duplicate postcodes -----
  
  # Snapshot to refer and join to later 
  df_orig <- df
  
  # RpostcodesioR expects postcodes to be labelled "postcode" 
  df %<>% rename(postcode = {{pcd_name}}) # we rename it back in the returned df
  
  # # Subset of empty postcodes we add back at the end
  # empty_pcds <- df %>% 
  #   filter(is.na(postcode)) %>% 
  #   bind_rows(pcd_details) # with empty variables of interest
  
  # Don't process empty or duplicate postcodes  
  df %<>%
    drop_na(postcode) %>% 
    distinct(postcode, .keep_all = TRUE)
  
  ## Batch-by-batch postcode lookup ----
  
  # PostcodesioR puts 100 row limit on bulk_postcode_lookup
  #  - so we're going to have to process the request in batches
  batch_size_max <- 100
  
  ### Batch loop prep ----
  
  # Numbers involved in breaking postcode lookup into batches
  total_rows <- nrow(df)
  whole_batches <- total_rows %/% batch_size_max
  remainder_batch_size <- total_rows %% batch_size_max
  n_batches <- ifelse(remainder_batch_size == 0, whole_batches, whole_batches + 1)
  
  ### Batch loop ----
  for (i in 1:n_batches) {
  #i <- 1
  
    #### Slice a batch ----
    
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
    batch <- slice(df, batch_start:batch_end)
    
    #### Bulk lookup request to postcodes.io ----
    bulk_lookup_rslt <- bulk_postcode_lookup(list(postcodes = batch$postcode))
    
    #### Extract the vars from the postcodes.io results in to a data frame ----
    df_batch_rslt <- map2_dfc(pcd_cols$name, pcd_cols$index,
                              ~  tibble(!!.x := map(bulk_lookup_rslt,
                                                    unlist(.y),
                                                    .default = NA))) %>%
      unnest(cols = all_of(pcd_vars))
    
    # Need to distinguish between postcode passed & matched e.g. s1 2hh & S1 2HH
    if(pcd_match) df_batch_rslt %<>% rename(pcd_match = postcode) 
    
    # Add postcode passed i.e. our primary key
    df_batch_rslt %<>%  
      add_column(postcode = map_chr(bulk_lookup_rslt, c(1, 1), .default = ""),
                 .before = 1)
    
    # Cumulative results from batches
    pcd_details <- bind_rows(pcd_details, df_batch_rslt)
  }
  
  # Stitch the subsets and variables together
  df_new  <- df_orig %>%
    left_join(pcd_details, by = c("postcode")) #%>% # add postcode variables of interest to the original data
    #bind_rows(empty_pcds) %>% # add records in the request with null postcodes
}

# Add LLPG variables to a data frame via Portal locator ---------------- 

set_python <- function(){
  
  # Reticulate provides the interface for Python packages
  library(reticulate)
  
  # Define the Python environment that comes with ArcGIS Pro
  use_condaenv("arcgispro-py3-clone", required=TRUE)
  
  # TODO: RStudio prj &/or global setting?
  #       Sys.getenv("python_env")?
  #       Pass name of conda env as a function parameter?
  #       Best practice?
  #       Does arcgis package need local ArcGIS desktop install?
  #       Anticipate arcgis not being installed (& conda?)
}

get_gis <- function(){
  
  # TODO: set_python() if not set
  
  # Import the GIS library from ArcGIS Python API 
  arcgis_gis <- import("arcgis.gis")
  
  # Try and retrieve Portal credentials from .environ file
  portal_id = Sys.getenv("portal_id")
  portal_pwd = Sys.getenv("portal_pwd")
  
  # Prompt for Portal credentials if necessary
  if (portal_id == "") portal_id <- rstudioapi::askForPassword("Portal user ID")
  if (portal_pwd == "") portal_pwd <- rstudioapi::askForPassword("Portal password")
  
  # Login to Portal and get a GIS object
  gis <- arcgis_gis$GIS("https://sheffieldcitycouncil.cloud.esriuk.com/portal/home/", 
                        portal_id, portal_pwd)
}

batch_geocode <- function(addr_batch, addr_name, geocoding, portal_geocoder){
  
  # browser()
  
  # Add row ID to the batch for a join later
  addr_batch <- mutate(addr_batch, row_id = row_number())
  
  # List of addresses to geocode
  addresses_to_geocode <- addr_batch %>% 
    select(all_of(addr_name)) %>% 
    deframe()
  
  # Geocode
  results <- geocoding$batch_geocode(addresses_to_geocode, 
                                     geocoder = portal_geocoder)
  
  # Put the hierarchical list of geocode results into a data frame
  df_results <- tibble(result = results) %>% 
    unnest_wider(result) %>% 
    unnest_wider(location) %>% 
    unnest_wider(attributes) %>% 
    clean_names() %>% 
    # select(result_id, match_addr, score, status, addr_type, 
    #        uprn, blpu_class, ward_code, parish_code, usrn) %>% 
    mutate(result_id = result_id + 1) # row_id starts at 1 not 0
  
  # Join results to the original batch data
  left_join(addr_batch, df_results, by=c("row_id" = "result_id")) %>% 
    select(-batch_id, -row_id) #remove ID vars used for processing in batches
}

geocode <- function(addr, addr_name = "address", portal_gis){
  
  # Add suffix
  addr <- rename_with(addr, ~str_c("ORIG_", .))
  addr_name <- str_c("ORIG_", addr_name)
  
  # browser()
  
  # Import the geocoding library from ArcGIS Python API 
  geocoding <- import("arcgis.geocoding")
  
  # List the different Portal geocoders
  geocoders <- geocoding$get_geocoders(portal_gis)
  #print(geocoders)
  
  # Get the geocoder we want to use
  geocoder_llpg_world <- geocoders[[2]]
  print(str_c("geocoder_llpg_world: ", geocoder_llpg_world))
  
  # Determine the batch size we should use
  batch_size <- geocoder_llpg_world$properties$locatorProperties$SuggestedBatchSize
  print(str_c("BatchSize: ", batch_size))
  
  # Split data frame to process in batches
  batches <- group_split(addr, batch_id = ceiling(row_number()/batch_size))
  
  # Process batches and combine
  df_batches <- lapply(batches, batch_geocode, 
                       addr_name, geocoding, geocoder_llpg_world) %>% 
    bind_rows() %>% 
    mutate(uprn = ifelse(str_length(uprn) == 11, #ensure UPRN is 12 characters
                         str_c("0", uprn),
                         uprn)) %>%
    select(starts_with("ORIG_"), one_of(c("uprn", "x","y"))) %>% 
    rename_with(~str_remove(.,"^ORIG_"))
}

# Add a multi-line address variable to a partial single-line address variable ----
add_addr_field <- function(addr, field){
  if(is.na(field)){
    addr <- addr
  } else {
    ifelse(is.na(addr), 
           addr <- field, 
           addr <- str_c(addr, ", ", field))
  }
}

# Pass a list of multi-line address variables and get a ... ----
# single-line (comma & space separated) address variable
combine_addr <- function(...){
  addr_fields <- list(...)
  addr <- NA
  for (i in seq_along(addr_fields)) {
    addr <- add_addr_field(addr, addr_fields[[i]])
  }
  return(addr)
}

# DEPRECATED Add postcode variables to a data frame via RpostcodesioR ----  
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

#  DEPRECATED Extract the UPRNs from the feature ... ---- 
#  ... created by our llpg_world_geocode() Python function
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