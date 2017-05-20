# postgis_functions
R functions using postgis for spatial analysis

The postgis_functions.r script provides a number of functions that could be useful for spatial data analysis using PostGIS through R. Requires RODBC and rgdal libraries installed in R. A database with postgis extension enabled and an odbc connection. Also in Linux it would be a good idea to create a .pgpass file https://www.postgresql.org/docs/9.4/static/libpq-pgpass.html

1. The function get_shp\_names lists the shapefile names in a directory that match a pattern. It can be used as input for the input_or_append function. Two arguments:

  - The directory path (string).

  - A regular expression pattern (string).

2. The function import_or\_append can be used to import a single shapefile or append multiple shapefiles in a table in Postgres. I created it initially to make it easier to upload Ordnance Survey data that are split into different grids, which makes it time consuming to be appended manually in a single table. Five arguments:
  
  - DBMS connection created with the dbConnect function from the DBI library.
  
  - The directory or directories of the shapefiles (string).
  
  - The names of shapefiles (the get_shp\_names function can be useful if multiple shapefiles have to be appended) (vector of strings).
  
  - The projection (integer, e.g. 27700 for British National Grid.
  
  - The name of the new table in Postgres (if the table exists it will be dropped) (string).
  
3. The function get_field\_names is used internally to obtain the schema of the tables.

4. The function road_poly\_intersect can be used to intersect a road network with polygons (geographical boundaries). Input is a line or multiline geometry and polygon or multipolygon geometry, outputs a line geometry with the fields from both input tables as well as two fields with the area and perimeter of the polygons. Four arguments:

  - DBMS connection created with the dbConnect function from the DBI library.
  
  - The name of the road network table (string).
  
  - The name of the geographical boundaries table (string).
  
  - The name of the output table (string).
  
5. The function poly_poly\_intersect can be used to intersect two tables with polygon or multipolygon geometries. Returns a table with multipolygon geometry representing the intersection of the two geometries and fields either from the left or both tables. Five arguments:

  - DBMS connection created with the dbConnect function from the DBI library.
  
  - The name of the first table (string).
  
  - The name of the second table (string).
  
  - The name of the output table (string).
  
  - Optionally join the fields of the second table with the first (boolean).
  
6. The function poly_poly\_union can be used to union two tables with polygon or multipolygon geometries. Returns a table with multipolygon geometry representing the union of the two geometries and fields either from the left or both tables. Five arguments:

  - DBMS connection created with the dbConnect function from the DBI library.
  
  - The name of the first table (string).
  
  - The name of the second table (string).
  
  - The name of the output table (string).
  
  - Optionally join the fields of the second table with the first (boolean).
  

