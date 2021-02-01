# HEADER ------------------------------------------------------------------
# Geocode a table of addresses

# ArcGIS Python integration modules
import arcpy
from arcgis.gis import GIS

# Set environment variable
arcpy.env.outputCoordinateSystem = arcpy.SpatialReference(27700) #BNG
arcpy.env.overwriteOutput = True
arcpy.env.qualifiedFieldNames = False

def llpg_world_geocode(addr_tbl, geocode_result, portal_id, portal_pwd):

  # Set local variables
  portal_url = "https://sheffieldcitycouncil.cloud.esriuk.com/portal/home/"
  address_locator = "https://sheffieldcitycouncil.cloud.esriuk.com/portal/sharing/servers/" \
                    "474642d4069941b989bef8bf69029522/rest/services/" \
                    "LLPGCascadeWorld/CASCADE/GeocodeServer/LLPG and World Cascade"
  address_fields = "'Single Line Input' address_to_geocode VISIBLE NONE"

  # Login to our Portal
  gis =  GIS(portal_url, portal_id, portal_pwd)
  print("Logged in as " + str(gis.properties.user.username))

  # Geocode
  arcpy.GeocodeAddresses_geocoding(addr_tbl, address_locator, address_fields, geocode_result,'STATIC')
