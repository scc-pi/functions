library(httr); library(tidyverse)
#library(dplyr); library(jsonlite); library(sf)

portal_base_url <- "https://sheffieldcitycouncil.cloud.esriuk.com/portal/sharing/"
portal_token_url <- str_c(portal_base_url, "rest/generateToken")

# Generate auth token from SCC Portal GIS server
#  - adapted from esri2sf function for AGOL
generatePortalToken <- function(uid = "", pwd = "", expiration = 5000) {

  if (uid == "") uid <- rstudioapi::askForPassword("Portal user ID")
  if (pwd == "") pwd <- rstudioapi::askForPassword("Portal password")
  
  query <- list(
    username = uid,
    password = pwd,
    expiration = expiration,
    client = "requestip",
    f = "json"
  )
  
  r <- POST(portal_token_url, body = query, encode = "form")
  
  content(r, "parsed")$token
}

portal2sf <- function(url, outFields = c("*"), where = "1=1", 
                      bbox = NULL, token = "", 
                      geomType = NULL, crs = 4326, ...) {
  
  browser()
  
  pst <- POST(url,
              query = list(f = "json", token = token),
              encode = "form",
              config = config(ssl_verifypeer = FALSE))
  
  #ct <- content(pst, as = "text")
  ct <- content(pst)
  
  layerInfo <- jsonlite::fromJSON(ct)
  
  print(layerInfo$type)
  if (is.null(geomType)) {
    if (is.null(layerInfo$geometryType))
      stop("geomType is NULL and layer geometry type ('esriGeometryPolygon' or 'esriGeometryPoint' or 'esriGeometryPolyline') could not be inferred from server.")
    
    geomType <- layerInfo$geometryType
  }
  
  print(geomType)
  
  if (!is.null(layerInfo$extent$spatialReference$latestWkid))
    print(paste0("Coordinate Reference System: ", layerInfo$extent$spatialReference$latestWkid))
  
  if (class(bbox) == "bbox") {
    if ((sf::st_crs(bbox)$input != layerInfo$extent$spatialReference$latestWkid) && !is.null(layerInfo$extent$spatialReference$latestWkid)) {
      bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), layerInfo$extent$spatialReference$latestWkid))
    }
  } else if (!is.null(bbox)) {
    stop("The provided bbox must be a class bbox object.")
  }
  
  bbox <- paste0(unlist(as.list(bbox), use.names=FALSE), collapse = ",")
  
  queryUrl <- paste(url, "query", sep = "/")
  
  esriFeatures <- getEsriFeatures(queryUrl, outFields, where, bbox, token, ...)
  esri2sfGeom(esriFeatures, geomType, crs)
}
