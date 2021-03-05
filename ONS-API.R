## ---------------------------
##
## Script name: ons-datasets-beta.R
##
## Purpose of script: Useful functions for exploring and returning ONS datasets
##                    from their beta API
##
## Author: Laurie Platt
##
## Date Created: 2018-12-13
##
##
## ---------------------------
##
## Notes:
##  TODO(Laurie Platt): Update notes
##  1. ONS_Datasets_Beta.xlxs created in working directory
##  2. ONS API reference @ https://developer.beta.ons.gov.uk/
##
## ---------------------------

# A suite of useful functions
library(tidyverse) 

# Not core tidyverse
library(httr)
library(jsonlite)

ons_df <- function(path, query_list){
  # Description:  Makes a request for data to the ONS.
  #
  # Args:
  #   path:       The first part of a request is a URL to the API endpoint
  #               e.g. https://api.beta.ons.gov.uk/v1/datasets/cpih01/editions/time-series/versions/5/observations
  #   query_list: The second part of the request is a query.
  #               To be provided as a list of valid dimensions
  #               and valid dimension values.
  #               e.g. list(time="*", aggregate="cpih1dim1A0", 
  #                                   geography="K02000001")
  #               This argument is optional i.e. the function
  #               can be called with just the first argument
  #          
  # Returns:
  #   Tibble with ONS data if successful
  #
  # TODO(Laurie Platt): Is this generic enough for all/other API's i.e. change
  #                     function name to getAPITibble?
  # TODO(Laurie Platt): Move to an ONS package? Github?
  # TODO(Laurie Platt): Error Handling 
  
  # Handle optional second argument
  if(missing(query_list)) {
    request <- GET(url = path)
  } else {
    request <- GET(url = path, query = query_list)
  }
  
  # Check the status of the last HTTP response 
  http_status_code <- request$status_code
  
  # A non-200 code means the request has failed
  if (http_status_code != 200) {

    # Provide some feedback
    print("ONS dataset not returned!", quote = FALSE)

  } else {

    # Parse the content returned from the server as text
    response <- content(request, as = "text", encoding = "UTF-8")

    # Parse the JSON content and convert it to a data frame
    df <- fromJSON(response, flatten = TRUE) %>%
      data.frame()

    # Coerce the dataframe to a tibble
    tib <- as_tibble(df)
  }
  
  return (tib)
}

list_of_ons_datasets <- function(){
  # Description:  Get a list of the datasets available via the ONS beta API
  #
  # Returns:
  #   Tibble listing ONS beta datasets data if successful
  #
  
  # path <- "https://api.beta.ons.gov.uk/v1/datasets"
  
  df <- ons_df("https://api.beta.ons.gov.uk/v1/datasets") %>%
    select(items.description, items.links.latest_version.href)
}

GetDimensions <- function(latest_version_href){
  # Description:  Get the dimensions of an ONS dataset.
  #
  # Args:
  #   latest_version_href:  Needs to be a value in the "items.links.latest_version.href"
  #                         column that's returned from getListOfONSBetaDatasets().
  # Returns:
  #   Tibble describing the dimensions of the ONS dataset if successful
  #

  dimensions_path <- paste(latest_version_href, "/dimensions", sep="")
  
  tib <- GetONSBetaTibble(dimensions_path)
  
  # Only need some of the columns
  tib <- select(tib, items.name, items.links.options.href)
  
  return (tib)
}

GetOptions <- function(options_href){
  # Description:  Get the options of an ONS dataset dimension.
  #
  # Args:
  #   options_href:  Needs to be a value in the "items.links.options.href"
  #                         column that's returned from getDimensions().
  # Returns:
  #   Tibble describing the options of the ONS dataset dimension if successful
  #

  tib <- GetONSBetaTibble(options_href)
  
  # Only need some of the columns
  tib <- select(tib, items.dimension, items.label, items.option)
  
  return (tib)
}

GetDimensionsAndOptions <- function(latest_version_href){
  # Description:  Get the dimensions and options for an ONS dataset
  #
  # Args:
  #   dimension_href: Needs to be a value in the "items.links.latest_version.href"
  #                   column that's returned from getListOfONSBetaDatasets().
  # Returns:
  #   Tibble describing the dimensions and options of an ONS dataset if successful
  #
  
  # Get the dimensions of the ONS dataset
  dimensions_tib <- GetDimensions(latest_version_href)

  # We're going to keep a list of tibbles - 
  # one tibble of options for each dimension
  tib_list = list()
  
  # Loop through each row in the dimension tibble
  for (i in 1:nrow(dimensions_tib)) {
  # TODO(Laurie Platt): Instead of for loop use sapply() or pipe?
    
    # Get the URL for the options API call
    options_href <- toString(dimensions_tib[i, 2])
    # TODO(Laurie Platt): change column number to name
    
    # Get a tibble of options for ONE items.links.options.href
    options_tib <- GetOptions(options_href)
    
    # Add the tibble to our list of tibbles
    tib_list[[i]] <- options_tib
  }

  # Combine all our tibbles into a single tibble
  single_tib = bind_rows(tib_list)

  return (single_tib)
}

