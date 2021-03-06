# HEADER ------------------------------------------------------------------
# Functions that get data and features from APIs for different types of
# areas in Sheffield e.g. Sheffield ward boundaries from SCC Open Data


# NOTES -------------------------------------------------------------------
# ONS & SCC ESRI based API hubs:
#  - ONS Open Geography Portal
#  - SCC Open Data
#  - SCC AGOL
#  - SCC Portal
#
# Resources to help use the APIs:
#  - ArcGIS REST API https://developers.arcgis.com/rest/services-reference/query-feature-service-layer-.htm
#  - SQL 92 is used by ESRI based APIs 
#  - HTML encoding for building URL queries https://www.w3schools.com/tags/ref_urlencode.ASP

library(tidyverse)

#library(esri2sf) #maxRecordCount bug for ONS Open Geography
# Location of local (relative or absolute) or GitHub ssc-pi esri2sf branch
esri2sf_branch <- "https://raw.githubusercontent.com/scc-pi/esri2sf/master/R/"
# If amending the function, comment out variable assignment above and uncomment below
# esri2sf_branch <- "../esri2sf/R/"
source(str_c(esri2sf_branch, "esri2sf.R"))
source(str_c(esri2sf_branch, "zzz.R"))

# Functions to help access SCC Portal features
functions_dir <- "https://raw.githubusercontent.com/scc-pi/functions/main/"
# If amending the function, comment out variable assignment above and uncomment below
#functions_dir <- ""
#source(str_c(functions_dir, "Portal.R"))

# Base URL for ONS Open Geography Portal
ons_geog_base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"

# Return a data frame of Sheffield LSOA codes and names
# from ONS Open Geography Portal
shef_lsoa_codes <- function(){

  esri2df(url = str_c(ons_geog_base_url, "LSOA11_UTLA20_EW_LU/FeatureServer/0"),
          where = "UTLA20NM='Sheffield'") %>%
    select(LSOA11CD, LSOA11NM)
}

# Return a data frame of Sheffield Ward codes and names
# from ONS Open Geography Portal
shef_ward_codes <- function(){
  
  esri2df(url = str_c(ons_geog_base_url, "WD20_LAD20_UK_LU_v2/FeatureServer/0"),
          where = "LAD20NM='Sheffield'")  %>%
    select(WD20CD, WD20NM)
}

# Return Sheffield LSOA boundaries as simple features
# from ONS Open Geography Portal
shef_lsoa_features <- function(detail = "onsGeneralised"){
  
  boundary_detail <- switch(detail,
                            onsFullResolution = "Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_V2",
                            onsGeneralised = "Lower_Layer_Super_Output_Areas_DEC_2011_EW_BGC_V3",
                            onsSuperGeneralised = "Lower_Layer_Super_Output_Areas_DEC_2011_EW_BSC_V3")
  
  esri2sf(url = str_c(ons_geog_base_url, boundary_detail, "/FeatureServer/0"),
          where = "LSOA11NM LIKE'Sheffield%'",
          geomType = "esriGeometryPolygon")
}

# Return Sheffield MSOA boundaries as simple features
# from ONS Open Geography Portal
shef_msoa_features <- function(detail = "onsGeneralised"){

  boundary_detail <- switch(detail,
                            onsFullResolution = "Middle_Layer_Super_Output_Areas_December_2011_EW_BFC_V2",
                            onsGeneralised = "Middle_Layer_Super_Output_Areas_DEC_2011_EW_BGC_V3",
                            onsSuperGeneralised = "Middle_Layer_Super_Output_Areas_DEC_2011_EW_BSC_V3")
  
  esri2sf(url = str_c(ons_geog_base_url, boundary_detail, "/FeatureServer/0"),
          where = "MSOA11NM LIKE'Sheffield%'",
          geomType = "esriGeometryPolygon")
}

# Return Sheffield MSOA boundaries as simple features
# from ONS Open Geography Portal
shef_ward_features <- function(detail = "onsGeneralised"){

  boundary_detail <- switch(detail,
                            onsFullResolution = "Wards_December_2020_UK_BFC_V2",
                            onsGeneralised = "Wards_December_2020_UK_BGC_V2",
                            onsSuperGeneralised = "Wards_December_2020_UK_BSC_V2")
  
  df_ward_codes <- shef_ward_codes() %>% 
    mutate(WD20CD = str_c("'", WD20CD, "'"))
  
  s_ward_codes <- str_c(df_ward_codes$WD20CD, collapse = ",")
  
  clause = str_c("WD20CD IN (", s_ward_codes, ")")
  
  esri2sf(url = str_c(ons_geog_base_url, boundary_detail, "/FeatureServer/0"),
          where = clause,
          geomType = "esriGeometryPolygon")  
}

# Return Sheffield IMD lookup as a data frame
# from ONS Open Geography Portal
shef_imd_lookup <- function(){
  
  esri2df(url = str_c(ons_geog_base_url, 
                      "Index_of_Multiple_Deprivation_December_2019_Lookup_in_England/",
                      "FeatureServer/0"),
          where = "LAD19NM = 'Sheffield'")  
}

# Return Sheffield IMD as LSOA simple features
# from ONS Open Geography Portal
shef_imd_features <- function(){
  
  imd <- shef_imd_lookup() %>% 
    select(LSOA11CD, IMD19)
  
  lsoa <- shef_lsoa_features() %>% 
    select(-starts_with("BNG"), -LONG, -LAT)
  
  number_of_lsoas <- 31388
  
  imd_features <- lsoa %>% 
    left_join(imd, by = "LSOA11CD") %>% 
    rename(imd_rank = IMD19) %>% 
    mutate(imd_decile = ntile(1:number_of_lsoas, 10))
}

# # Return 100 Sheffield neighbourhood boundaries as simple features
# # from SCC Portal
# shef_neighbourhood_features <- function(portal_token = ""){
#   
#   if(length(portal_token) == 0) {
#     portal_token <- generatePortalToken(Sys.getenv("portal_id"),
#                                         Sys.getenv("portal_pwd"))
#   }
#   
#   esri2sf(url = "https://sheffieldcitycouncil.cloud.esriuk.com/portal/sharing/servers/1f70fe34009e4967a0b6290807f46df1/rest/services/AGOL/Boundaries/MapServer/13",
#           token = portal_token,
#           geomType = "esriGeometryPolygon")
# }

# shef_care_home_features <- function(portal_token = ""){
#   
#   if(length(portal_token) == 0) {
#     portal_token <- generatePortalToken(Sys.getenv("portal_id"),
#                                         Sys.getenv("portal_pwd"))
#   }
#   
#   care_homes_url <- str_c(portal_base_url, 
#                           "d1d56858e23048baaa2ffe5f8fa9b577/rest/services/",
#                           "Portal/Health_Internal/MapServer/5")
#   
#   # browser()
#   
#   portal2sf(url = care_homes_url,
#             token = portal_token,
#             geomType = "esriGeometryPoint")
# }
# 
# portal_token <- generatePortalToken(Sys.getenv("portal_id"),
#                                     Sys.getenv("portal_pwd"))
# 
# sf_care_homes <- shef_care_home_features(portal_token)
# 
# sf_neighbourhoods <- shef_neighbourhood_features()
