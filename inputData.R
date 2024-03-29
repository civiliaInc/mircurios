


########################################################
## Load the geometric data
load.geom <- function(input, output,session){
  if( file_ext(input$geom_obj$name) == "kmz" ) geom_obj <<- readOGR(input$geom_obj)
  if( file_ext(input$geom_obj$name) == "zip" ){
    geom_obj <- input$geom_obj$datapath
    ## Unzip 
    link<-"/tmp/"
    unzip(geom_obj,junkpaths=TRUE,exdir=link)
    list_files <- unzip(geom_obj,list=TRUE) %>%
      dplyr::filter(grepl(pattern = ".shp", x = Name))
    ## Inputs to read shp
    i.dir <- path.expand(link)
    i.layer <- basename(file_path_sans_ext(list_files$Name))
    ## Read the shp
    rm(geom_obj)
    geom_obj <- readOGR(dsn = i.dir, layer = i.layer)
    unlink(paste(link,"/*",sep=""))
  } 
  ## Transforme le systeme de coordonnées
  #if( !grepl(pattern = "WGS84", x=crs(x=geom_obj,asText=TRUE)) ){
  geom_obj <- spTransform(geom_obj, CRS("+init=epsg:4326"))
  #  }
  
  ## Garde les noms      
  list_names <- colnames( geom_obj@data )
  for( i in seq_along(list_names)){
    if( !grepl(x=geom_obj@data[list_names[i]], pattern = "[A:Za:z]")) list_names[i] <- NA
  }                     
  list_names <- list_names[which(!is.na(list_names))]
  geom_obj@data <- geom_obj@data %>% dplyr::select(all_of(list_names))
  
  ## Cree un nom unique court
  geom_obj@data$nom_total <- ""
  for( i in 1:nrow(geom_obj@data)){
    i.nom_total <- paste(unique(as.character((geom_obj@data[i,]))),collapse=" ")
    geom_obj@data$nom_total[i] <- i.nom_total   
  }
  
  .GlobalEnv$geom_obj <- geom_obj
  
}

########################################################
## Load the GTFS
load.gtfs.static <- function(input, output,session){
  
  ## Should the GTFS loading be faster
  if( input$calcKPI ) calcKPI <- TRUE else calcKPI <- FALSE
  
  ## Progress bar to show each step
  withProgress(message = 'En cours', {
    
    ## Unzip file
    setProgress(value=0.1, detail = "Ouverture du GTFS")
    gtfs <- input$gtfs_zip$datapath
    rdn <- paste(round(runif(1,0,1000)) + round(runif(1,0,1000)), "_", as.integer(Sys.time()),"/",sep="")
    link <- paste("/tmp/gtfs_",rdn,sep="")
    unlink(link,recursive = TRUE)
    dir.create(link)
    unzip(gtfs,junkpaths=TRUE,exdir=link)
    
    ## Import gtfs static
    stop_times_df <- fread(paste0(link,"stop_times.txt"))
    stops_df <- fread(paste0(link,"stops.txt"))
    trips_df <- fread(paste0(link,"trips.txt"))
    shapes_lk <- paste0(link,"shapes.txt")
    shapes_df <- ifelse( file.exists(shapes_lk), fread(shapes_lk), NA)
    if(length(shapes_df) < 2 ) shapes_df <- NA
    routes_df <- fread(paste0(link,"routes.txt"))
    agency_df <- fread(paste0(link,"agency.txt"))
    
    ## Empty 
    unlink(link,recursive = TRUE)
    
    ## Convert time to POSIXct format
    stop_times_df$arrival_time   <- as_datetime(stop_times_df$arrival_time,format="%H:%M:%S")
    stop_times_df$departure_time <- as_datetime(stop_times_df$departure_time,format="%H:%M:%S")
    
    ## Sequence
    stop_times_df <- stop_times_df %>%
      group_by(trip_id) %>%
      dplyr::mutate(stop_sequence = row_number()) %>%
      ungroup()
    
    ## Numeric coord
    stops_df <- stops_df %>%
      dplyr::mutate(stop_lon = as.numeric(stop_lon),
             stop_lat = as.numeric(stop_lat)) %>%
      dplyr::filter( !is.na(stop_lon) & !is.na(stop_lat) )
    
    ## For each trip, compute the straight distance between each stop
    ## Use the driving time (arrival - departure) to estimate the speed in kmh
    
    stop_times_df <- stop_times_df %>%
      left_join(stops_df, by = c("stop_id")) %>% 
      left_join(trips_df, by = c("trip_id"))
    
    if( !calcKPI ){
      stop_times_df$dist2prev <- 1
      stop_times_df$time2prev <- 1
      stop_times_df$speed2prev.kmh <- 1
      stop_times_df$cumDist <- 1
      stops_speed <- stop_times_df
    }else{
      setProgress(value=0.4, detail = "Calcul de la distance")
      
    ## Compute distances
    distances <- stop_times_df %>%
      group_by(trip_id) %>%
      dplyr::mutate(orig_lat = lag(stop_lat), # Add the (n-1) stop coord
             orig_lon = lag(stop_lon)) %>%
      dplyr::filter(stop_sequence > 1) %>%
      as.data.frame() %>%
      dplyr::select(stop_lat,stop_lon,orig_lat,orig_lon) %>%
      distinct() %>%
      dplyr::filter(!is.na(orig_lat) & !is.na(orig_lon)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dist2prev = distHaversine(p1 = c(orig_lon, orig_lat), p2 = c(stop_lon, stop_lat))) %>%
      ungroup() %>%
      as.data.frame() 
    
    setProgress(value=0.5, detail = "Calcul des temps")
    
    duration <- stop_times_df %>%
      group_by(trip_id) %>%
      dplyr::mutate(orig_arrival_time = dplyr::lag(arrival_time)) %>%
      dplyr::filter(stop_sequence > 1) %>%
      as.data.frame() %>%
      dplyr::select(arrival_time,orig_arrival_time) %>%
      distinct() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time2prev =  ifelse( !calcKPI, 1, as.numeric(difftime(arrival_time, orig_arrival_time, units="secs")))) %>%
      ungroup() %>%
      as.data.frame() 
    
        setProgress(value=0.6, detail = "Calcul de la vitesse")
          
        stops_speed_seq1 <- dplyr::filter(stop_times_df, stop_sequence == 1) %>%
          mutate(
            speed2prev.kmh = NA,
            cumDist = 0,
            dist2prev = NA,
            time2prev = NA,
            orig_lat = NA,
            orig_lon = NA,
            orig_arrival_time = NA)
        
        stops_speed <- stop_times_df %>%
          group_by(trip_id) %>%
          dplyr::mutate(orig_lat = dplyr::lag(stop_lat, order_by = trip_id), # Add the (n-1) stop coord
                        orig_lon = lag(stop_lon),
                        orig_arrival_time = lag(arrival_time)) %>%
          dplyr::filter(stop_sequence > 1) %>%
          left_join(distances, by=c("orig_lat", "orig_lon", "stop_lon", "stop_lat")) %>%
          left_join(duration, by=c("arrival_time", "orig_arrival_time")) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(speed2prev.kmh = ifelse(time2prev > 0, 3.6 * dist2prev / time2prev, NA)) %>%
          dplyr::mutate(time2prev = ifelse(stop_sequence == 1, 0, time2prev),
                        dist2prev = ifelse(stop_sequence == 1, 0, dist2prev),
                        speed2prev.kmh = ifelse(stop_sequence == 1, 0, speed2prev.kmh)) %>%
          ungroup() %>%
          as.data.frame() %>%
          group_by(trip_id) %>%
          dplyr::mutate(cumDist = cumsum(dist2prev)) %>%
          ungroup() %>%
          dplyr::filter(!is.na(speed2prev.kmh))
    
        stops_speed <- rbind(stops_speed_seq1, stops_speed )
    }
    
    ## For each stop, compute the average speed when reaching it
    setProgress(value=0.8, detail = "Calcul des moyennes")
    
    stops.avg <- stops_speed %>%
      group_by(stop_id) %>%
      dplyr::summarise(stop.avgspeed.kmh = mean(speed2prev.kmh, na.rm = TRUE)) %>%
      ungroup() 
    
    stops_speed <- left_join(stops_speed, stops.avg,by=c("stop_id"))
    
    avg.stops_speed <- stops_speed %>% 
      as.data.frame() %>% 
      distinct(stop_id,stop_name,stop.avgspeed.kmh,stop_lat,stop_lon)
    
    ## For each route, compute the average speed over all stops
    trips.avg <- stops_speed %>%
      group_by(trip_id) %>%
      dplyr::summarise(tot_dist = sum(dist2prev, na.rm=TRUE),
                tot_time = sum(time2prev, na.rm=TRUE)) %>%
      dplyr::mutate(comm_speed = 3.6 * tot_dist / tot_time) %>%
      ungroup()
    avg.routes_speed <- aggregate(list(route.avgspeed.kmh=stops_speed$speed2prev.kmh),list(route_id=stops_speed$route_id),FUN=mean,na.rm=TRUE)
    
    ## For each route, find a significant trip
    list_longest_trips <- stops_speed %>%
      group_by(route_id,trip_id) %>%
      dplyr::summarise(max_seq = dplyr::n()) %>%
      ungroup() %>%
      group_by(route_id) %>%
      dplyr::filter(max_seq == max(max_seq)) %>%
      dplyr::filter(row_number() == 1)
    
    stops_longest_trips <- stops_speed %>% 
      dplyr::filter(trip_id %in% list_longest_trips$trip_id) %>%
      dplyr::select(route_id, trip_id, stop_id, stop_sequence, dist2prev, cumDist, speed2prev.kmh, time2prev)
    
    ## Get the mean, 1/3 and 2/3 percentiles for the route speeds
    q.routes_speed <- (avg.routes_speed %>% na.omit() %>%
                         dplyr::filter(route.avgspeed.kmh < mean(route.avgspeed.kmh)) %>%
                         dplyr::summarise('1/3'=quantile(route.avgspeed.kmh,probs=1/3), 
                                   '2/3'=quantile(route.avgspeed.kmh,probs=2/3)))
    q1 <- as.numeric(q.routes_speed$`1/3`)
    q2 <- as.numeric(q.routes_speed$`2/3`)
    
    ## Set the color of the routes depending on their speed
    avg.routes_speed$col <- ifelse( avg.routes_speed$route.avgspeed.kmh > q2, "green",
                                    ifelse(avg.routes_speed$route.avgspeed.kmh<=q2 & avg.routes_speed$route.avgspeed.kmh>q1,"yellow",
                                           ifelse(avg.routes_speed$route.avgspeed.kmh<=q1,"red","black")))
    
    ## Set the color of the stops depending on their speed
    avg.stops_speed$col <- ifelse( avg.stops_speed$stop.avgspeed.kmh > q2, "green",
                                   ifelse(avg.stops_speed$stop.avgspeed.kmh<=q2 & avg.stops_speed$stop.avgspeed.kmh>q1,"yellow",
                                          ifelse(avg.stops_speed$stop.avgspeed.kmh<=q1,"red","black")))
    
    ## Set ID 
    avg.routes_speed <- avg.routes_speed %>%  arrange(route.avgspeed.kmh) %>% dplyr::filter(!is.na(route.avgspeed.kmh))%>% as.data.frame()
    avg.routes_speed$ID <- seq.int(nrow(avg.routes_speed))
    avg.routes_speed <- avg.routes_speed
    .GlobalEnv$avg.routes_speed <- avg.routes_speed
    updateSelectInput(session,"lignes",choices = c("toutes les lignes",avg.routes_speed$route_id))
    
    
    ## Basic city map
    coord <- data.frame(lon=mean(stops_df$stop_lon,na.rm=T),lat=mean(stops_df$stop_lat,na.rm=T))
    .GlobalEnv$map.city <- leaflet() %>%
      addTiles() %>%
      setView(coord$lon, coord$lat, zoom = 11) 
  })
  
  ## Assign KPI to gtfs
  withProgress(message = 'En cours', value = 0, {
    sh_df <- vector(mode = "list", length = length(routes_df$route_id))
    col_rte <- vector(mode = "list", length = length(routes_df$route_id))
    stops_rte <- vector(mode = "list", length = length(routes_df$route_id))
    for( i in seq_along(routes_df$route_id) ){
      
      setProgress( value=round(i/length(routes_df$route_id),1), detail = "Récupération des lignes...")
      
      i.route_id <- routes_df$route_id[i]
      i.trips <- trips_df %>% dplyr::filter(route_id==i.route_id)
      
      if( !is.na(shapes_df) ) i.shapes <- shapes_df %>% dplyr::filter(shape_id %in% i.trips$shape_id) else i.shapes <- NULL
      
      i.stop_times <- stop_times_df %>% dplyr::filter(trip_id %in% i.trips$trip_id)
      i.stops_KPI <- dplyr::filter(stops_longest_trips, route_id == i.route_id ) 
      
      ## Get the route color
      if( "route_color" %in% routes_df ){
        col_rte[[i]] <- paste0("#",routes_df %>% dplyr::filter(route_id==i.route_id) %>% distinct(route_color) %>% as.character())
      }
      else{
        col_rte[[i]] <- "red"
      }
      
      ## Get the route stops
      stops_rte[[i]] <- stops_df %>% 
        dplyr::filter(stop_id %in% i.stops_KPI$stop_id) %>% 
        left_join(i.stops_KPI, by="stop_id") %>% arrange(stop_sequence) 
      
      ## Get all shapes for a given route
      if( !is.na(shapes_df) ) sh_df[[i]] <- shapes_df %>% dplyr::filter(shape_id %in% i.trips$shape_id) else sh_df[[i]] <- NA
    }
    
    .GlobalEnv$reseau <- list(routes_df = routes_df,
                              sh_df = sh_df,
                              col_rte = col_rte,
                              stops_rte = stops_rte,
                              stops = stops_df,
                              stops_speed = stops_speed)
    
  })
  
  ## job done, create the plots
  #source("vitesses.R")
  #plot.vitesses(output)
  source("maps.R")
  plot.map(output,reseau)
  # source("variations.R")
  #  plot.variations(output)
  
}
