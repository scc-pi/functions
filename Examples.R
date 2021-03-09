# HEADER ------------------------------------------------------------------
# Example functions used in notes

library(tidyverse)

#library(esri2sf) #maxRecordCount bug for ONS Open Geography
# Location of local (relative or absolute) or GitHub ssc-pi esri2sf branch
esri2sf_branch <- "https://raw.githubusercontent.com/scc-pi/esri2sf/master/R/"
# If amending the function, comment out variable assignment above and uncomment below
#esri2sf_branch <- "../esri2sf/R/"
source(str_c(esri2sf_branch, "esri2sf.R"))
source(str_c(esri2sf_branch, "zzz.R"))

# Return a data frame of Sheffield LSOA codes and names
# from ONS Open Geography Portal
shef_lsoa_codes <- function(){

  esri2df(url = str_c(ons_geog_base_url, "LSOA11_UTLA20_EW_LU/FeatureServer/0"),
          where = "UTLA20NM='Sheffield'") %>%
    select(LSOA11CD, LSOA11NM)
}