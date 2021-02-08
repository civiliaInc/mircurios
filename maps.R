################################
## Build the map
plot.map <- function(output,reseau){
  ## Plot the bus map with the speed as color for each stop
  
  map1 <- map.city
  ## Plot the lines with their color
  withProgress(message = 'En cours', {
      
  # for( i in seq_along(reseau$routes_df$route_id) ){
  #   
  #   print(i)
  #   incProgress( round(i / length(reseau$routes_df$route_id),1) , detail = "Création des cartes")
  #   i.route_id <- reseau$routes_df$route_id[i]
  #   i.shapes <- reseau$sh_df[[i]]
  #   i.stops <- reseau$stops_rte[[i]]
  #   i.group <- paste0("Line ", i.route_id)
  #   
  #   if( nrow(i.shapes) != 0 ){
  #   ## Plot each shape separetely
  #   for( i.shape_id in unique(i.shapes$shape_id)){
  #     j.shapes <- i.shapes %>% filter(shape_id == i.shape_id)
  #      map1 <- addPolylines(map1,
  #                          data=j.shapes,
  #                          ~shape_pt_lon,
  #                          ~shape_pt_lat,
  #                          col="green",
  #                          fill = F,
  #                          opacity=0.8,
  #                          label=paste0("Line ", i.route_id),
  #                          highlightOptions = highlightOptions(weight = 7,
  #                                                              opacity = 1,
  #                                                              bringToFront = T,
  #                                                              color="darkgrey"),
  #                          group = i.group
  #     )
  #   }
  #   }else{
  #     print(2)
  #     start.time <- Sys.time()
  #     ## Add segments between each stop
  #     for( j in 2:nrow(i.stops) ){
  #       j.stops <- filter(i.stops, stop_sequence == j | stop_sequence == j - 1 )
  # 
  #       map1 <- addPolylines(map1,
  #                          lng = c(j.stops$stop_lon[1], j.stops$stop_lon[2]),
  #                          lat = c(j.stops$stop_lat[1], j.stops$stop_lat[2]),
  #                          stroke=TRUE,
  #                          color="green",
  #                          label = paste("#", j-1, " ", round(j.stops$dist2prev[2]), " m ", "( ", round(j.stops$cumDist[2]), " m )", sep=""),
  #                          fillOpacity=1,
  #                          group = i.group)
  # 
  #     }
  #     
  #     end.time <- Sys.time()
  #     time.taken <- end.time - start.time
  #     print(time.taken)
  #     
  #     
  #     
  #     
  #     
  #     ###########
  #     
  #     
  #     
  #   }
  #   
  #   
  #   
  #   ## Add the stops
  #   map1 <- addCircles(map1,
  #                      data = i.stops,
  #                      ~stop_lon,
  #              ~stop_lat,
  #              stroke=TRUE,
  #              color="red",
  #              label=~paste(stop_name,"id:", as.character(stop_id)),
  #              radius=20,
  #              fillOpacity=1, 
  #              group = i.group)
  #   
  #   print(3)
  #   
  #   
  # }
  
  
  
  ## Add all lines with an offset
  # all_shapes <- vector(mode = "list", length = length(routes_df$route_id))
  # for( i in seq_along(routes_df$route_id) ){
  #   i.route_id <- routes_df$route_id[i]
  #   i.shapes <- sh.df[[i]] %>% select(shape_pt_lon, shape_pt_lat)
  #  saveRDS(i.shapes,file=paste("test",i,".RDS",sep=""))
  #    coordinates(i.shapes) <- c("shape_pt_lon","shape_pt_lat")
  #   Lines(list(Line(i.shapes)),ID=i.route_id)
  #   print(class(i.shapes))
  #   all_shapes[[i]] <- i.shapes
  # }
  # print(str(all_shapes))
  # all_shapes <- SpatialLines(all_shapes)
  # 
  # S1 <- readRDS("test1.RDS")
  # S1 <- Line(S1)
  # S1 <- Lines(list(S1), ID = "1")
  # S2 <- readRDS("test2.RDS")
  # S2 <- Line(S2)
  # S2 <- Lines(list(S2), ID = "2")
  # 
  # 
  # l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
  # Sl1 <- Line(l1)
  # 
  # Sl <- SpatialLines(list(S1, S2))
  # Sl <- SpatialLinesDataFrame(Sl, data = data.frame(route_id = routes_df$route_id))
  # 
  # map.city %>%
  #   addPolylines(data=Sl,
  #                options=list(offset=20)
  #   )
  # 
  # # map1 <- addPolylines(map1,
  # #                      data=j.shapes,
  # #                      ~shape_pt_lon,
  # #                      ~shape_pt_lat,
  # #                      col="green",
  # #                      fill = F,
  # #                      opacity=0.8,
  # #                      label=paste0("Line ", i.route_id),
  # #                      highlightOptions = highlightOptions(weight = 7,
  # #                                                          opacity = 1,
  # #                                                          bringToFront = T,
  # #                                                          color="darkgrey"),
  # #                      group = i.group
  # # )
  # ##########
  
    # map1 <- map1 %>%  addWebGLHeatmap(data = stops.red,
    #                                   lng=~stop_lon, 
    #                                   lat=~stop_lat,
    #                                   opacity=0.6,
    #                                   size = 900,
    #                                   units="m",
    #                                   group="Heat points")
    # 
    # map1 <- map1 %>%  addWebGLHeatmap(data = stops.green,
    #                                   lng=~stop_lon, 
    #                                   lat=~stop_lat,
    #                                   opacity=0.6,
    #                                   size = 900,
    #                                   units="m",
    #                                   group="Cool points", 
    #                                   gradientTexture="deep-sea")
    # 
    ## If a geom object has been provided
    if( exists("geom_obj") ){
      
      
      print("affiche la geom")
      print(geom_obj)
      map1 <- map1 %>% addPolygons(data=geom_obj, 
                                       label=~nom_total,
                                       opacity = 0.5,
                                       fillOpacity = 0.,
                                       highlight = highlightOptions(
                                         weight = 3,
                                         fillOpacity = 0.,
                                         opacity = 1,
                                         color = "red",
                                         bringToFront = TRUE,
                                         sendToBack = TRUE),
                                       group="geom")
      
      
    }
    
    ## Hide all groups
    map1 <- map1 %>% 
      hideGroup(c("Red stops","Green stops","Yellow stops",
                  paste0("Line ",avg.routes_speed$route_id),"Heat points","Cool points"))
    
    ## Shiny object for the map
    output$busmap <- renderLeaflet(map1)
    
  })
  }
  
