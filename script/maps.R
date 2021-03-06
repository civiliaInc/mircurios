################################
## Build the map
plot.map <- function(output){
  ## Plot the bus map with the speed as color for each stop
  stops.green <- (avg.stops_speed %>% filter(col=="green"))
  stops.yellow <- (avg.stops_speed %>% filter(col=="yellow"))
  stops.red <- (avg.stops_speed %>% filter(col=="red"))
  
  map1 <- addCircles(map.city,stops.green$stop_lon,
                     stops.green$stop_lat,
                     stroke=TRUE,
                     color=stops.green$col,
                     label=paste0(stops.green$stop_name,", ID: ",stops.green$stop_id,
                                  ", v=", round(stops.green$stop.avgspeed.kmh,digits =0)," km/h"),
                     radius=20,
                     fillOpacity=0.2, group = "Green stops") %>%
    addCircles(stops.yellow$stop_lon,
               stops.yellow$stop_lat,
               stroke=TRUE,
               color=stops.yellow$col,
               label=paste0(stops.yellow$stop_name,", ID: ",stops.yellow$stop_id, 
                            ", v=", round(stops.yellow$stop.avgspeed.kmh,digits =0)," km/h"),
               radius=20,
               fillOpacity=0.2, group = "Yellow stops") %>%
    addCircles(stops.red$stop_lon,
               stops.red$stop_lat,
               stroke=TRUE,
               color=stops.red$col,
               label=paste0(stops.red$stop_name,", ID: ",stops.red$stop_id, 
                            ", v=", round(stops.red$stop.avgspeed.kmh,digits =0)," km/h"),
               radius=20,
               fillOpacity=0.2, group = "Red stops")
  
  ## Plot the lines with their color
  for( i in seq_along(routes_df$route_id) ){
    i.route_id <- routes_df$route_id[i]
    i.shapes <- sh.df[[i]]
    i.stops <- stops.rte[[i]]
    i.group <- paste0("Line ", i.route_id)
    ## Plot each shape separetely
    for( i.shape_id in unique(i.shapes$shape_id)){
      j.shapes <- i.shapes %>% filter(shape_id == i.shape_id)
      map1 <- addPolylines(map1,
                           data=j.shapes, 
                           ~shape_pt_lon,
                           ~shape_pt_lat,
                           col="green", 
#                           col=col.rte[[i]], 
                           fill = F,
                           opacity=0.8,
                           label=paste0("Line ", i.route_id),
                           highlightOptions = highlightOptions(weight = 7, 
                                                               opacity = 1,
                                                               bringToFront = T, 
                                                               color="darkgrey"),
                           group = i.group
      )
    }
    ## Add the stops
    map1 <- addCircles(map1,
                       data = i.stops,
                       ~stop_lon,
               ~stop_lat,
               stroke=TRUE,
               color="red",
               label=~paste(stop_name,"id:", as.character(stop_id)),
               radius=20,
               fillOpacity=1, 
               group = i.group)
  }
    
    map1 <- map1 %>%  addWebGLHeatmap(data = stops.red,
                                      lng=~stop_lon, 
                                      lat=~stop_lat,
                                      opacity=0.6,
                                      size = 900,
                                      units="m",
                                      group="Heat points")
    
    map1 <- map1 %>%  addWebGLHeatmap(data = stops.green,
                                      lng=~stop_lon, 
                                      lat=~stop_lat,
                                      opacity=0.6,
                                      size = 900,
                                      units="m",
                                      group="Cool points", 
                                      gradientTexture="deep-sea")
    
    ## Hide all groups
    map1 <- map1 %>% hideGroup(c("Red stops","Green stops","Yellow stops",paste0("Line ",avg.routes_speed$route_id),"Heat points","Cool points"))
    
    ## Layers control
    #map1 <- addLayersControl( map1,
    #                          overlayGroups = c("Green stops","Yellow stops","Red stops",paste0("Line ",1:9),"Heat points","Cool points"),
    #                          options = layersControlOptions(collapsed = FALSE) ) %>%
    
    ## Shiny object for the map
    output$busmap <- renderLeaflet(map1)
  }
  
