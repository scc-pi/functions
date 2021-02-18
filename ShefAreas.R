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
#  - ArcGIS REst API https://developers.arcgis.com/rest/services-reference/query-feature-service-layer-.htm
#  - SQL 92 is used by ESRI based APIs 
#  - HTML encoding for building URL queries https://www.w3schools.com/tags/ref_urlencode.ASP

library(tidyverse); library(janitor)

#library(esri2sf)
# Location of local (relative or absolute) or GitHub ssc-pi esri2sf branch
esri2sf_branch <- "https://raw.githubusercontent.com/scc-pi/esri2sf/master/R/"
# If amending the function, comment out code above and uncomment below
# esri2sf_branch <- "../../BI/Repo/esri2sf/R/"
source(str_c(esri2sf_branch, "esri2sf.R"))
source(str_c(esri2sf_branch, "zzz.R"))

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
  
  # https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD20_LAD20_UK_LU_v2/FeatureServer/0/query?outFields=*&where=1%3D1
  
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