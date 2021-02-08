shinyServer( function(input, output,session ) {
  
  ## Set up the daashboard
  output$page <- renderUI({ page.dashboard() })
  
  ## Load the data
  observeEvent(input$geom_obj, load.geom(input,output,session) )
  observeEvent(input$gtfs_zip, load.gtfs.static(input, output,session) )
  
  ## Maps changes according to user input
  observe({
    proxy <- leafletProxy("busmap", session)

    if( !is.null(input$geom) ){
    if( input$geom == TRUE ){
      proxy %>% showGroup("geom")
    } else {
      proxy %>% hideGroup("geom")
    }
    }
    
    # zones.grp <- c("Cool points","Heat points")
    # for( i in 1:length(zones.grp)){
    #   if( i %in% input$zones ) proxy %>% showGroup(zones.grp[i]) else proxy %>% hideGroup(zones.grp[i])
    # }
    # points.grp <- c("Green stops","Yellow stops","Red stops")
    # for( i in 1:length(points.grp)){
    #   if( i %in% input$points ) proxy %>% showGroup(points.grp[i]) else proxy %>% hideGroup(points.grp[i])
    # }
    
    if( exists("avg.routes_speed")) routes.grp <- avg.routes_speed$route_id else routes.grp <- c()
    ## Display single routes
    for( rte in routes.grp){
      #if( rte %in% input$lignes ) proxy %>% showGroup(paste0("Line ",rte)) else proxy %>%hideGroup(paste0("Line ",rte))
      
      if( rte %in% input$lignes ){
        
        proxy %>% clearShapes() 
        i <- which(reseau$routes_df$route_id == rte)
        i.route_id <- reseau$routes_df$route_id[i]
        i.shapes <- reseau$sh_df[[i]]
        i.stops <- reseau$stops_rte[[i]]

        # if( nrow(i.shapes) != 0 ){
        #   ## Plot each shape separetely
        #   for( i.shape_id in unique(i.shapes$shape_id)){
        #     j.shapes <- i.shapes %>% filter(shape_id == i.shape_id)
        #     proxy <- addPolylines(proxy,
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
        #                                                              color="darkgrey")
        #     )
        #   }
        # }else{
          
        ## Add segments between each stop
           for( j in 2:nrow(i.stops) ){
             j.stops <- filter(i.stops, stop_sequence == j | stop_sequence == j - 1 )

             proxy %>%
               addPolylines(lng = c(j.stops$stop_lon[1], j.stops$stop_lon[2]),
                            lat = c(j.stops$stop_lat[1], j.stops$stop_lat[2]),
                            stroke=TRUE,
                            color="green",
                            label = paste("#", j-1, " ", round(j.stops$dist2prev[2]), " m ", "( ", round(j.stops$cumDist[2]), " m )", sep=""),
                            fillOpacity=1) %>%
                 addCircles(lng = j.stops$stop_lon[1],
                            lat = j.stops$stop_lat[1],
                            stroke=TRUE,
                            color="red",
                            label=paste(j.stops$stop_name[1],"id:", as.character(j.stops$stop_id[1])),
                            radius=20,
                            fillOpacity=1)
             
           }
          
       # }
        
        
      
        
        
      }
      
      
      ####### XXXXXX
    }
    ## Display all routes
    # if( !is.null(input$lignes)){
    #   if( input$lignes == "toutes les lignes"){
    #     for( rte in routes.grp){
    #       proxy %>% showGroup(paste0("Line ",rte))
    #     }
    #   }
    # }
    
  })
  
  
  
  
  ## -------------------
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  
  # If zoom button was clicked this time, and store the value, and rezoom
  #    rezoom <- "first"
  #    if (!identical(lastZoomButtonValue, input$zoomButton)) {
  #      lastZoomButtonValue <<- input$zoomButton
  #      rezoom <- "always"
  #    }
  #   map <- map %>% mapOptions(zoomToLimits = rezoom)
  
  ## -------------------
  
  
  
  #  output$vitesses <- renderPlotly({
  #    x <- list(
  #      title = "Routes"
  #    )
  #    y <- list(
  #      title = "Vitesses commerciales (Km/h)",
  #      range = c(0,60)
  #    )
  #    plot_ly(routes_df,
  #            x=~routes_chosen,
  #            y=~routes_speed,
  #            type="scatter",
  #            mode="markers",
  #            color=~col,
  #            hoverinfo = "text",
  #            name = "routes_chosen",
  #            text = paste("Route ", routes_chosen, " : ", round(routes_speed,digits=1), "Km/h"),
  #            marker = list(size = 15, opacity=0.65,
  #                          color = ~col)
  #            ) %>%
  #      layout(xaxis = x, yaxis = y) %>%
  #      layout(showlegend = FALSE)
  #      })
  
} )

