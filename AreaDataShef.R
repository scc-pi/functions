# HEADER ------------------------------------------------------------------
# Functions that get data from APIs for different types of 
# areas in Sheffield  e.g. for LSOAs from ONS


# MID-YEAR POPULATION ESTIMATES (ONS) -------------------------------------
population_estimates <- function(){
  path <- "http://api.beta.ons.gov.uk/v1/datasets/mid-year-pop-est/editions/time-series/versions/2/dimensions"
  dimensions_tib = GetONSBetaTibble(path)
  View(dimensions_tib)
  
}



#
# # Test ONS API call
# 



