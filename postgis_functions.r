library(RPostgreSQL)

#######################################################################################################
#                                    Get shapefile names by pattern                                   #
#######################################################################################################
get_shp_names <- function(working_dir, pattern){
  pattern_rm <- "\\(|\\)|:|;|,|&"
  shp_names <- list.files(working_dir)
  shp_names <- shp_names[grep(gsub(pattern_rm, "", pattern), gsub(pattern_rm, "", shp_names))]
  shp_names <- shp_names[which(substr(shp_names, nchar(shp_names) - 2, nchar(shp_names)) == "shp")]
}

#######################################################################################################
#                                     Import shapefiles in POSTGIS                                    #
#######################################################################################################

import_or_append <- function(con, working_dirs, shp_names, srid, table_name){
  
  if (length(working_dirs) > 1 & length(working_dirs) != length(shp_names)){
    stop("if more that one working_dirs is provided, the number of working_dirs should be equal to the number of shp_names")
  }
  
  if (length(working_dirs) == 1 & length(shp_names) > 1){
    working_dirs <- rep(working_dirs, length(shp_names))
  }
  
  dbExecute(con, paste0("DROP TABLE IF EXISTS ", table_name))
  
  con_info <- dbGetInfo(con)
  username <- con_info$user
  db_name <- con_info$dbname
  
  if (length(shp_names) > 0){
    s2p <- paste('shp2pgsql -c -s ', srid, ' -W "latin1" "', working_dirs[1], '/', shp_names[1], '" ', table_name, ' | psql -U ', username, ' -d ', db_name, ' -w', sep="")
    system(s2p)
    
    if (length(shp_names) > 1){
      for (i in 2:length(shp_names)){
        s2p <- paste('shp2pgsql -a -s ', srid, ' -W "latin1" "', working_dirs[i], '/', shp_names[i], '" ', table_name, ' | psql -U ', username, ' -d ', db_name, ' -w', sep="")
        system(s2p)
      }
    }
    
    dbExecute(con, paste("CREATE INDEX ON", table_name, "USING GIST(geom)"))
    
    ################################# Correct invalid geometries #####################################
    
    query <- paste("SELECT gid, ST_IsValidReason(geom) FROM", table_name, "WHERE ST_IsValid(geom)=false;")
    results <- dbGetQuery(con, query)
    
    if (nrow(results) > 0){
      tryCatch(
        {
          query <- paste("UPDATE", table_name, "SET geom = ST_MakeValid(geom) WHERE ST_IsValid(geom)=false;")
          corrected <- dbExecute(con, query)
          if (corrected == 1){
            print(paste(nrow(results), " invalid geometries were found and corrected"))
          } else if (corrected == -1){
            cat(nrow(results), "invalid geometries were found but it was not possible to be corrected.\n",
                "Perhaps use ST_Buffer(geom, 0) to correct geometry, e.g. (assuming multipolygons as geometry and srid = 27700)\n",
                "CREATE TABLE new_table(gid serial PRIMARY KEY, field1, geom geometry(MultiPolygon, 27700));\n",
                "INSERT INTO new_table(field1, geom) SELECT field1, ST_Multi(ST_Buffer(geom, 0)) AS geom FROM old_table;\n",
                "CREATE INDEX ON new_table USING gist(geom);\n",
                "Then check geometry by:\n SELECT gid, ST_IsValidReason(geom) FROM new_table WHERE ST_IsValid(geom)=false;")}
          },
        error = function(err){
          print(err)
        }
      )
    }
    print(paste("The shapefile ", shp_names, "was found based on the shapefile names provided"))
    ###################################################################################################
  } else {
    print("No shapefiles were found")
  }
}


#######################################################################################################
#                                       Correct Geometry                                              #
#######################################################################################################

# correct_geom <- function(con, table_name, new_table_name){
#   
#   query <- paste("SELECT column_name, data_type, character_maximum_length FROM information_schema.columns WHERE table_name = '",
#                  table_name, "';", sep = "")
#   cols <- dbGetQuery(con, query)
#   
# }

#######################################################################################################
#                                    Get field names from table                                       #
#######################################################################################################
get_field_names <- function(con, table_name, id_field, geom_field, get_type = T, table_prefix = "", table_suffix=""){
  
  query <- paste("SELECT column_name, data_type, character_maximum_length FROM information_schema.columns WHERE table_name = '",
                 table_name, "';", sep = "")
  cols <- dbGetQuery(con, query)
  cols <- cols[! cols$column_name  %in% c(id_field, geom_field), ]
  
  string <- ""
  for (i in 1:nrow(cols)){
    if (i != nrow(cols)){
      end <- ","
    } else {
      end <- ""
    }
    if (get_type){
      if (cols[i,"data_type"] == "character varying"){
        string <- paste(string, " ", cols[i, 1], table_suffix, " ", cols[i, 2], "(", cols[i, 3], ")", end, sep="")
      } else {
        string <- paste(string, " ", cols[i, 1], table_suffix, " ", cols[i, 2], end, sep = "")
      }
    } else {
      string <- paste(string, " ", table_prefix, cols[i, 1], table_suffix, end, sep = "")
    }
  }
  return(string)
}
#######################################################################################################

attach_suffix <- function(field_names, suffix){
  field_names <- strsplit(sub("^\\s+", "", unlist(strsplit(field_names, ","))), " ")
  return(do.call(paste, c(lapply(field_names, function(x) paste(c(paste(x[1], suffix, sep = "_"), x[2:length(x)]), collapse = " ")), sep= ", ")))
}

#######################################################################################################
#                            Join attributes from polygons with road network                          #
#######################################################################################################
road_polys_join <- function(con, roads_table, polygons_table, table_out, road_id = "gid", road_geom="geom", polygon_id = "", polygon_geom="geom"){
  
  ########################################### Create new table ########################################
  road_fields <- get_field_names(con = con, table_name = roads_table, id_field = road_id, geom_field = road_geom)
  polygons_fields <- get_field_names(con = con, table_name = polygons_table, id_field = polygon_id, geom_field = polygon_geom)
  
  road_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", roads_table, "', 'geom');",sep="")))
  polygons_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", polygons_table, "', 'geom');",sep="")))
  
  if (polygons_srid != road_srid){
    stop(paste('Error! Roads network projection (srid=', road_srid, ') differs from polygons projection (srid=',polygons_srid,")",sep=''))
  }
  
  road_fields <- attach_suffix(road_fields, "road")
  polygons_fields <- attach_suffix(polygons_fields, "poly")
  
  query <- paste("CREATE TABLE ", table_out, "(gid serial PRIMARY KEY,", road_fields, ",", polygons_fields, ");", sep = "")
  dbExecute(con, query)
  
  query <- paste("SELECT AddGeometryColumn('public','", table_out, "','geom',", road_srid, ",'LINESTRING',2);", sep="")
  dbExecute(con, query)
  
  ############################################ Intersect ##############################################
  query <- paste0("INSERT INTO ", table_out, "(",
                 get_field_names(con = con, table_name = table_out, id_field = "gid", geom_field = "", get_type = F),
                 ") SELECT DISTINCT ",
                 get_field_names(con = con, table_name = roads_table, id_field = road_id, geom_field = road_geom, get_type = F, table_prefix="r."), ", ",
                 get_field_names(con = con, table_name = polygons_table, id_field = polygon_id, geom_field = polygon_geom, get_type = F, table_prefix="p."),
                 ", (ST_Dump(r.",road_geom, ")).geom FROM ", roads_table, " AS r LEFT JOIN ", polygons_table,
                 " AS p ON ST_Intersects(r.geom, p.geom);")
  dbExecute(con, query)
  dbExecute(con, paste("CREATE INDEX ON", table_out, "USING gist(geom)"))
  
}
########################################################################################################


#######################################################################################################
#                              Intersect Road network with Polygons                                   #
#######################################################################################################
road_polys_intersect <- function(con, roads_table, polygons_table, table_out){
  
  ########################################### Create new table ########################################
  road_fields <- get_field_names(roads_table,con=con)
  polygons_fields <- get_field_names(polygons_table,con=con)
  
  road_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", roads_table, "', 'geom');",sep="")))
  polygons_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", polygons_table, "', 'geom');",sep="")))
  
  if (polygons_srid != road_srid){
    stop(paste('Error! Roads network projection (srid=', road_srid, ') differs from polygons projection (srid=',polygons_srid,")",sep=''))
  }
  
  query <- paste("CREATE TABLE ", table_out, "(gid serial PRIMARY KEY,", road_fields, ",", polygons_fields, ", area real, perimeter real);", sep = "")
  dbExecute(con, query)
  
  query <- paste("SELECT AddGeometryColumn('public','", table_out, "','geom',", road_srid, ",'LINESTRING',2);", sep="")
  dbExecute(con, query)
  
  ############################################ Intersect ##############################################
  query <- paste("INSERT INTO ", table_out, "(", get_field_names(con=con, roads_table, get_type = F),",",
                 get_field_names(polygons_table, get_type = F,con=con), ", area, perimeter, geom) SELECT",
                 get_field_names(roads_table, get_type = F, table_prefix="r.",con=con), ",",
                 get_field_names(polygons_table, get_type = F, table_prefix="p.",con=con),
                 ", ST_AREA(p.geom) AS area, ST_Perimeter(p.geom) AS perimeter",
                 ", CASE WHEN ST_GeometryType(ST_Intersection(r.geom, p.geom)) NOT IN",
                 " ('ST_GeometryCollection') THEN",
                 " (ST_Dump(ST_Intersection(r.geom, p.geom))).geom ELSE",
                 " (ST_Dump(ST_CollectionExtract(ST_Intersection(r.geom, p.geom),2))).geom",
                 " END AS geom FROM ", roads_table, " AS r, ", polygons_table,
                 " AS p WHERE ST_Intersects(r.geom, p.geom);", sep="")
  dbExecute(con, query)
  dbExecute(con, paste("CREATE INDEX ON", table_out, "USING gist(geom)"))
  
}
########################################################################################################


########################################################################################################
# Clip poly1 by poly2, return common area of poly1 and poly2, optionally add fields of poly2 in clipped poly1
########################################################################################################
poly_poly_intersect <- function(con, poly1, poly2, poly_out, add_poly2_fields = T){
  
  query <- paste("DROP TABLE IF EXISTS ", poly_out, ";", sep = "")
  dbExecute(con, query)
  
  ########################################### Create new table ########################################
  
  poly1_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", poly1, "', 'geom');",sep="")))
  poly2_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", poly2, "', 'geom');",sep="")))
  
  if (poly1_srid != poly2_srid){
    stop(paste('Error! poly1 projection (srid=', poly1_srid, ') is not the same as poly2 projection (srid=',poly2_srid,")",sep=''))
  }
  
  if (add_poly2_fields){
    poly1_fields <- get_field_names(poly1, table_suffix = 1, con=con)
    poly2_fields <- get_field_names(poly2, table_suffix = 2, con=con)
    query <- paste("CREATE TABLE ", poly_out, "(gid serial PRIMARY KEY,", poly1_fields, ",", poly2_fields, ");", sep = "")
    dbExecute(con, query)
  } else {
    poly1_fields <- get_field_names(poly1, con=con)
    # poly2_fields <- get_field_names(poly2, con=con)
    query <- paste("CREATE TABLE ", poly_out, "(gid serial PRIMARY KEY,", poly1_fields, ");", sep = "")
    dbExecute(con, query)
  }
  
  query <- paste("SELECT AddGeometryColumn('public','", poly_out, "','geom',", poly1_srid, ",'MULTIPOLYGON',2);", sep="")
  dbExecute(con, query)
  dbExecute(con, paste("CREATE INDEX ", poly_out, "_geom_idx ON ", poly_out, " USING GIST (geom);", sep = ""))
  
  ############################################ Intersect ##############################################
  query <- paste("INSERT INTO ", poly_out, "(", ifelse(add_poly2_fields, paste(get_field_names(poly1, get_type = F, table_suffix = 1, con=con),",",
                                                                               get_field_names(poly2, get_type = F, table_suffix = 2, con=con), sep = ""), get_field_names(poly1, get_type = F,con=con)),
                 ", geom) SELECT", ifelse(add_poly2_fields, paste(
                   get_field_names(poly1, get_type = F, table_prefix="p1.",con=con), ",",
                   get_field_names(poly2, get_type = F, table_prefix="p2.",con=con), sep = ""), 
                   get_field_names(poly1, get_type = F, table_prefix="p1.",con=con)),
                 ", ST_MULTI(ST_Buffer(ST_Intersection(p1.geom, p2.geom), 0.0)) AS geom FROM ",
                 poly1, " AS p1 INNER JOIN ", poly2, " AS p2 ",
                 "ON ST_Intersects(p1.geom, p2.geom) ",
                 "WHERE NOT ST_IsEmpty(ST_Buffer(ST_Intersection(p1.geom, p2.geom), 0.0));", sep="")
  dbExecute(con, query)
  
}

#######################################################################################################
#                             Union of polygons                                                       #
#######################################################################################################
poly_poly_union <- function(con, poly1, poly2, poly_out, add_poly2_fields = T){
  
  query <- paste("DROP TABLE IF EXISTS ", poly_out, ";", sep = "")
  dbExecute(con, query)
  
  ########################################### Create new table ########################################
  
  poly1_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", poly1, "', 'geom');",sep="")))
  poly2_srid <- as.integer(dbGetQuery(con, paste("select FIND_SRID('public', '", poly2, "', 'geom');",sep="")))
  
  if (poly1_srid != poly2_srid){
    stop(paste('Error! poly1 projection (srid=', poly1_srid, ') is not the same as poly2 projection (srid=',poly2_srid,")",sep=''))
  }
  
  if (add_poly2_fields){
    poly1_fields <- get_field_names(poly1, table_suffix = 1, con=con)
    poly2_fields <- get_field_names(poly2, table_suffix = 2, con=con)
    query <- paste("CREATE TABLE ", poly_out, "(gid serial PRIMARY KEY,", poly1_fields, ",", poly2_fields, ");", sep = "")
    dbExecute(con, query)
  } else {
    poly1_fields <- get_field_names(poly1, con=con)
    query <- paste("CREATE TABLE ", poly_out, "(gid serial PRIMARY KEY,", poly1_fields, ");", sep = "")
    dbExecute(con, query)
  }
  
  query <- paste("SELECT AddGeometryColumn('public','", poly_out, "','geom',", poly1_srid, ",'MULTIPOLYGON',2);", sep="")
  dbExecute(con, query)
  
  ############################################ Intersect ##############################################
  query <- paste("INSERT INTO ", poly_out, "(", ifelse(add_poly2_fields, paste(get_field_names(poly1, get_type = F, table_suffix = 1, con=con),",",
                                                                               get_field_names(poly2, get_type = F, table_suffix = 2, con=con), sep = ""), get_field_names(poly1, get_type = F,con=con)),
                 ", geom) SELECT", ifelse(add_poly2_fields, paste(
                   get_field_names(poly1, get_type = F, table_prefix="p1.",con=con), ",",
                   get_field_names(poly2, get_type = F, table_prefix="p2.",con=con), sep = ""), 
                   get_field_names(poly1, get_type = F, table_prefix="p1.",con=con)),
                 ", ST_MULTI(ST_Union(p1.geom, p2.geom)) AS geom FROM ",
                 poly1, " AS p1 INNER JOIN ", poly2, " AS p2 ",
                 "ON ST_Intersects(p1.geom, p2.geom) ",
                 "WHERE NOT ST_IsEmpty(ST_Buffer(ST_Intersection(p1.geom, p2.geom), 0.0));", sep="")
  dbExecute(con, query)
  
}
########################################################################################################
#                            Polygons road connectivity                                                #
########################################################################################################
polys_road_connectivity <- function(db_name, username, password, road_poly_table, poly_id_field, out_table){
  
  con <- odbcConnect(db_name, uid = username, pwd = password)
  
  ############################### Create topology ######################################################
  query <- paste("ALTER TABLE ", road_poly_table, " ADD COLUMN source integer;\n",
                 " ALTER TABLE ", road_poly_table, " ADD COLUMN target integer;\n",
                 " SELECT pgr_createTopology('", road_poly_table, "', 0.00001, 'geom', 'gid');", sep="")
  dbExecute(con, query)
  
  query <- paste("CREATE TABLE ", out_table, " AS ",
                 "WITH ",
                 "t1 AS",
                 " (SELECT * FROM",
                 " (SELECT DISTINCT s.", poly_id_field, " AS ", poly_id_field,"_s, ", "t.",poly_id_field, " AS ", poly_id_field,"_t",
                 " FROM ", road_poly_table, " AS s, ", road_poly_table, " AS t",
                 " WHERE s.source = t.target",
                 " OR (s.source = t.source AND t.target != s.target)",
                 " OR (s.target = t.target AND s.source != t.source)) AS q1",
                 " WHERE q1.", poly_id_field, "_s != q1.", poly_id_field, "_t),",
                 " t2 AS",
                 " (SELECT ", poly_id_field, "_t AS oa1, ARRAY_AGG(DISTINCT ", poly_id_field, "_s) AS ar1 FROM t1",
                 " GROUP BY ", poly_id_field, "_t),",
                 " t3 AS",
                 " (SELECT ", poly_id_field, "_s AS oa2, ARRAY_AGG(DISTINCT ", poly_id_field, "_t) AS ar2 FROM t1",
                 " GROUP BY ", poly_id_field, "_s)",
                 " SELECT ", road_poly_table, ".", poly_id_field, " AS ", poly_id_field,
                 ", ", road_poly_table, ".area AS area",
                 ", ", road_poly_table, ".perimeter AS perimeter",
                 ", ARRAY_LENGTH(ARRAY(SELECT DISTINCT UNNEST(ARRAY_CAT(t2.ar1, t3.ar2))), 1) AS polys_nr FROM ", road_poly_table,
                 " LEFT OUTER JOIN t2 ON ", road_poly_table, ".", poly_id_field, " = t2.oa1",
                 " LEFT OUTER JOIN t3 ON ", road_poly_table, ".", poly_id_field, " = t3.oa2",
                 " GROUP BY ", road_poly_table, ".", poly_id_field, ", ", road_poly_table, ".area, ", road_poly_table, ".perimeter, t2.ar1, t3.ar2;",sep="")
  dbExecute(con, query)
  
}

#######################################################################################################################
#                                      Sum of road length per OA                                                      #
#                              Select only A road, Major road, Highway                                                #
#######################################################################################################################
road_length <- function(db_name, username, password, road_poly_table, road_class_field, polygons_field, table_out){
  
  con <- odbcConnect(db_name, uid = username, pwd = password)
  
  query <- paste("CREATE TABLE ", table_out, " AS ",
                 "SELECT DISTINCT q1.", polygons_field, " FROM ", road_poly_table, " AS q1",
                 " LEFT OUTER JOIN",
                 " (SELECT q2.", polygons_field, ", SUM(ST_Length(q2.geom)) FROM (",
                 "SELECT ", polygons_field, ", ", road_class_field, ", geom FROM ", road_poly_table,
                 " WHERE ", road_class_field, " IN ('A Road', 'Motorway', 'Primary Road',",
                 " 'Motorway, Collapsed Dual Carriageway', 'Primary Road, Collapsed Dual Carriageway','A Road, Collapsed Dual Carriageway')) AS q2",
                 " GROUP BY q2.", polygons_field, ") AS q3",
                 " ON q1.", polygons_field, " = q3.", polygons_field, sep = "")
  
  dbExecute(con, query)
  
}
