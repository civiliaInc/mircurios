
load.gtfs.static <- function(input, output,session){
  
  ## Progress bar to show each step
  withProgress(message = 'En cours', value = 0, {


      ## Unzip file
      incProgress(0.1, detail = "Décompression du GTFS...")

      gtfs <- input$gtfs_zip$datapath
      link<-"/tmp/"
      unzip(gtfs,junkpaths=TRUE,exdir=link)
 #     if (is.null(inFile))
 #         return(NULL)

#          read.csv(inFile$datapath

                   

      
    ## Import gtfs static
    incProgress(0.1, detail = "Ouverture du GTFS...")

    stop_times_df <- fread(paste0(link,"stop_times.txt"))
    stops_df <- fread(paste0(link,"stops.txt"))
    trips_df <- fread(paste0(link,"trips.txt"))
    shapes_df <- fread(paste0(link,"shapes.txt"))
    routes_df <- fread(paste0(link,"routes.txt"))
    agency_df <- fread(paste0(link,"agency.txt"))
    
    ## Convert time to POSIXct format
    stop_times_df$arrival_time <- as.POSIXct(stop_times_df$arrival_time,format="%H:%M:%S")
    stop_times_df$departure_time <- as.POSIXct(stop_times_df$departure_time,format="%H:%M:%S")
    
    ## Columns useless in coming data
    drops <- c("service_id", "stop_headsign", "pickup_type", "drop_off_type", 
               "shape_dist_traveled", "stop_code", "zone_id", "stop_url", 
               "location_type", "parent_station", 
               "stop_desc","trip_headsign","trip_short_name","direction_id","block_id")
    
    ## For each trip, compute the straight distance between each stop
    ## Use the driving time (arrival - departure) to estimate the speed in kmh
    incProgress(0.2, detail = "Calcul des temps entre chaque arrêt...")
    stops_speed <<- (stop_times_df %>%
                       left_join(stops_df, by = c("stop_id")) %>% # join stops positions
                       group_by(trip_id,stop_sequence) %>% 
                       left_join(trips_df, by=c("trip_id")) %>% # add info on trip
                       as.data.frame() %>% # necessary for time2prev and orig coord
                       mutate(orig_lat = ifelse( stop_sequence == 1, NA, lag(stop_lat)), # Add the (n-1) stop coord
                              orig_lon = ifelse( stop_sequence == 1, NA, lag(stop_lon))) %>%
                       mutate(time2prev = ifelse( stop_sequence == 1, NA, as.numeric(arrival_time-lag(departure_time)))) %>% # Time (n-1) to (n)
                       select(-one_of(drops))
    ) 
    
    incProgress(0.4, detail = "Calcul des distances entre les arrêts...")
    dist <- function(lat1, lon1, lat2, lon2){ r <- acos(sin(lat1) * sin(lat2) + cos(lat1)*cos(lat2) * cos(abs(lon2-lon1))) * 6378137 }
    deg2rad <- function(deg) {(deg * pi) / (180)}
    stops_speed <<- stops_speed %>% mutate(dist2prev = dist(deg2rad(stop_lat),deg2rad(stop_lon),deg2rad(orig_lat),deg2rad(orig_lon)))
    
    incProgress(0.6, detail = "Calcul des vitesses entre les arrêts...")
    stops_speed <<- (stops_speed %>% mutate(speed2prev.kmh = ifelse(time2prev>0, 3.6 * dist2prev / time2prev, NA)))
    
    ## For each stop, compute the average speed when reaching it
    incProgress(0.8, detail = "Calcul des vitesses commerciales par arrêt")
    stops.avg <- aggregate(list(stop.avgspeed.kmh=stops_speed$speed2prev.kmh),list(stop_id=stops_speed$stop_id),FUN=mean)
    stops_speed <<- stops_speed %>% inner_join(stops.avg,by=c("stop_id"))
    avg.stops_speed <<- (stops_speed %>% as.data.frame() %>% distinct(stop_id,stop_name,stop.avgspeed.kmh,stop_lat,stop_lon))
    
    ## For each route, compute the average speed over all stops
    incProgress(0.9, detail = "Calcul des vitesses commerciales par ligne...")
    avg.routes_speed <<- aggregate(list(route.avgspeed.kmh=stops_speed$speed2prev.kmh),list(route_id=stops_speed$route_id),FUN=mean,na.rm=TRUE)
    
    ## Get the mean, 1/3 and 2/3 percentiles for the route speeds
    q.routes_speed <- (avg.routes_speed %>% na.omit() %>%
                         filter(route.avgspeed.kmh < mean(route.avgspeed.kmh)) %>%
                         summarise('1/3'=quantile(route.avgspeed.kmh,probs=1/3), 
                                   '2/3'=quantile(route.avgspeed.kmh,probs=2/3)))
    q1 <- as.numeric(q.routes_speed$`1/3`)
    q2 <- as.numeric(q.routes_speed$`2/3`)
    
    ## Set the color of the routes depending on their speed
    avg.routes_speed$col <<- ifelse( avg.routes_speed$route.avgspeed.kmh > q2, "green",
                                     ifelse(avg.routes_speed$route.avgspeed.kmh<=q2 & avg.routes_speed$route.avgspeed.kmh>q1,"yellow",
                                            ifelse(avg.routes_speed$route.avgspeed.kmh<=q1,"red","black")))
    
    ## Set the color of the stops depending on their speed
    avg.stops_speed$col <<- ifelse( avg.stops_speed$stop.avgspeed.kmh > q2, "green",
                                    ifelse(avg.stops_speed$stop.avgspeed.kmh<=q2 & avg.stops_speed$stop.avgspeed.kmh>q1,"yellow",
                                           ifelse(avg.stops_speed$stop.avgspeed.kmh<=q1,"red","black")))
    
    ## Set ID 
    avg.routes_speed <<- avg.routes_speed %>%  arrange(route.avgspeed.kmh) %>% filter(!is.na(route.avgspeed.kmh))%>% as.data.frame()
    avg.routes_speed$ID <<- seq.int(nrow(avg.routes_speed))
    updateSelectInput(session,"lignes",choices = avg.routes_speed$route_id)
    
    ## Basic city map
    incProgress(0.95, detail = "Récupération des coordonnées de la ville...")
    coord <- data.frame(lon=mean(stops_df$stop_lon),lat=mean(stops_df$stop_lat))
    map.city <<- leaflet() %>%
        addTiles() %>%
#              addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=68c4cd328d3b484091812a76fae093fd') %>%
      setView(coord$lon, coord$lat, zoom = 11) 
    
    ## Extract shapes for each route
    sh.df <<- vector(mode = "list", length = length(routes_df$route_id))
    col.rte <<- vector(mode = "list", length = length(routes_df$route_id))
    stops.rte <<- vector(mode = "list", length = length(routes_df$route_id))
    for( i in seq_along(routes_df$route_id) ){
      i.route_id <- routes_df$route_id[i]
      i.trips <- trips_df %>% filter(route_id==i.route_id)
      i.shapes <- shapes_df %>% filter(shape_id %in% i.trips$shape_id)
      i.stop_times <- stop_times_df %>% filter(trip_id %in% i.trips$trip_id)
      ## Get the route stops
      .GlobalEnv$stops.rte[[i]] <- stops_df %>% filter(stop_id %in% i.stop_times$stop_id)
      ## Get the route color
      .GlobalEnv$col.rte[[i]] <- paste0("#",routes_df %>% filter(route_id==i.route_id) %>% distinct(route_color) %>% as.character())
      ## Get all shapes for a given route
      .GlobalEnv$sh.df[[i]] <- shapes_df %>% filter(shape_id %in% i.trips$shape_id)
    }
    
    ## Set global
    .GlobalEnv$routes_df <- routes_df
    
    ## job done, create the plots
    source("vitesses.R")
    plot.vitesses(output)
    source("maps.R")
    plot.map(output)
    source("variations.R")
    plot.variations(output)
  })
}
