# postgis_functions
R functions using postgis for spatial analysis

The postgis_functions.r script provides a number of functions that could be useful for spatial data analysis using PostGIS through R. Requires RODBC and rgdal libraries installed in R. A database with postgis extension enabled and an odbc connection. Also in Linux it would be a good idea to create a .pgpass file https://www.postgresql.org/docs/9.4/static/libpq-pgpass.html

1. The function get_shp\_names lists the shapefile names in a directory that match a pattern. It can be used as input for the input_or_append function. Two arguments:

  - The directory path.

  - A regular expression pattern.

2. The function import_or\_append can be used to import a single shapefile or append multiple shapefiles in a table in Postgres. I created it initially to make it easier to upload Ordnance Survey data that are split into different grids, which makes it time consuming to be appended manually in a single table. Four arguments:
  
  - An odbc connection created with the odbcConnect function from the RODBC library.
  
  - The directory of the shapefiles.
  
  - The name of the new table in Postgres (if the table exists it will be dropped).
  
  - A vector of names of shapefiles (the get_shp\_names function can be useful if multiple shapefiles have to be appended).
