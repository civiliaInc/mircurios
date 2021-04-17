shinyServer( function(input, output,session ) {
  
  ## Set up the dashboard
  output$page <- renderUI({ page.dashboard() })
  
  ## Load the data
  #observeEvent(input$geom_obj, load.geom(input,output,session) )
  observeEvent(input$gtfs_zip, load.gtfs.static(input, output,session) )
  
  
  ## When a polygon is drawn, show the stops included
  observeEvent(input$busmap_draw_new_feature, {
    ## Extract the drawn element
    fig <- input$busmap_draw_new_feature$geometry
    fig <- unlist(fig$coordinates)
    
    ## If the observed object is a polyline
    if( length(fig) >= 8 ){
      ## Extract fig coordinates
      lng <- fig[seq(from=1, to=length(fig), by=2)]
      lat <- fig[seq(from=2, to=length(fig), by=2)]
      fig_polygon <- data.frame(lng, lat)
      ## Make a polygon
      fig_polygon <- Polygon(fig_polygon)
      fig_polygon <- Polygons(list(fig_polygon),1)
      fig_polygon <- SpatialPolygons(list(fig_polygon))
      proj4string(fig_polygon) <- CRS.wgs84
      ## Extract stops within the polygon
      poly.stops <- reseau$stops
      coordinates(poly.stops) <- ~ stop_lon + stop_lat
      proj4string(poly.stops) <- CRS.wgs84
      poly.stops$inFig <- over(x=poly.stops, y=fig_polygon) 
      poly.stops <- poly.stops %>% subset(!is.na(inFig))
      .GlobalEnv$poly.stops <- as.data.frame(poly.stops)
      ## Identify corresponding routes
      poly.routes <- reseau$stops_speed %>%
        filter(stop_id %in% poly.stops$stop_id) %>%
        dplyr::select(route_id) %>%
        distinct()
      .GlobalEnv$poly.routes <- poly.routes %>% as.data.frame()
      
      ## before doing anything ask for the extraction to process
      if( is.null(input$extraction) ){
        showModal(modalDialog(title=NULL,
                              h3("Choisissez d'abord le type d'extraction : lignes ou arrêts."),
                              footer = tagList(
                                modalButton("Retour")
                              )))
      }else{
      ## Add the routes on the map
      if( nrow(poly.routes) > 0 ){
        ## Display text info
        showModal(modalDialog(title=NULL,
                              h3(paste("Lignes passant par la zone : ", paste(poly.routes$route_id, collapse = ", "), sep="\n")),
                              footer = tagList(
                                modalButton("Voir la carte"),
                                actionButton("saveZone", "Sauver le corridor")
                              )))
        ## Display the routes
        if( input$extraction == 1){
          for( rte in poly.routes$route_id){
            plot_route(rte,session)
          }
        }
        ## Display the stops data
        if( input$extraction == 2){
       
          plot_stops(session)
          
          poly.routes_data <- reseau$stops_speed %>%
            filter(route_id %in% poly.routes$route_id ) %>%
            filter(stop_id %in% poly.stops$stop_id)
          
          poly.graph <- plot_ly(data = poly.routes_data,
                             x = ~time2prev,
                             y = ~speed2prev.kmh,
                             text = ~paste("Ligne ", route_id, "<br>",
                                           "arrêt ", stop_id, "<br>",
                                           "sequence ", stop_sequence, "<br>",
                                           "direction ", direction_id, "<br>",
                                           "distance ", round(dist2prev), " m <br>",
                                           "temps ", round(time2prev), " sec <br>",
                                           "vitesse ", round(speed2prev.kmh), " km/h <br>",
                                           sep=""),
                             hoverinfo = "text",
                             type = "scatter",
                             mode = "markers", 
                             width=1000) %>%
            layout(xaxis = list(title = "Durée inter-arrêt précédent (sec.)"),
                   yaxis = list(title = "Vitesse inter-arrêt précédent (km/h)"))
          
          output$graph <- renderPlotly({poly.graph})
          }
        }
      }
      
      ## Make the polygon global
      .GlobalEnv$fig_polygon <- fig_polygon
    
    ## If the observed object is a polyline
    if( length(fig) == 2 ){
      
      .GlobalEnv$fig_point <- fig
      showModal(modalDialog(title="Corridor orienté",
                            footer = tagList(
                              modalButton("Annuler"),
                              downloadButton("confirmSaveZone", "Sauver")
                            )))
      
    }
    }
  })
  
  ## Save the zone
  observeEvent(input$saveZone, {
    showModal(modalDialog(
      textInput("zoneId", h3("Nom du corridor"),placeholder = ''),
      selectInput("zoneDir1", label = h3("Direction 1"), 
                  choices = list("Nord", "Sud", "Est", "Ouest"), 
                  selected = "Nord"),
      numericInput("zoneGain_dir1", label = h3("Gain en temps (sec)"), value=0),
      selectInput("zoneDir2", label = h3("Direction 2"), 
                  choices = list("Nord", "Sud", "Est", "Ouest"), 
                  selected = "Nord"),
      numericInput("zoneGain_dir2", label = h3("Gain en temps (sec)"), value=0),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("orientateCorridor", "Suivant")
      )))
    
  })
  
  ## Orientate the corridor
  observeEvent(input$orientateCorridor, {
  showModal(modalDialog(
    title = "Placer un marqueur à l'entrée de la direction 1",
    "Il servira à orienter le corridor.",
    easyClose = TRUE,
    footer = tagList(modalButton("Ok"))
  ))
  })
  
  ## Save the zone
  output$confirmSaveZone <- downloadHandler(
    filename = function() {
      ## Save the polygon
      replace_words <- c(" "="", "â"="a","é"="e","è"="e", "ô"="o", "'"="", "’"="","[.]"="", "/"="-", "ç"="c")
      id = str_replace_all(input$zoneId, replace_words) 
      out <- paste("corridor_",id,".zip",sep="")
      return(out)
    },
    content = function(file) {
      
      out <- gather_results_corridor()
      zip(zipfile = file, files = out)
    }
  )
  
  gather_results_corridor <- function(out){
    replace_words <- c(" "="", "â"="a","é"="e","è"="e", "ô"="o", "'"="", "’"="","[.]"="", "/"="-", "ç"="c")
    id = str_replace_all(input$zoneId, replace_words) 
    zoneDir1 <- input$zoneDir1
    zoneGain_dir1 <- input$zoneGain_dir1
    zoneDir2 <- input$zoneDir2
    zoneGain_dir2 <- input$zoneGain_dir2
    entrance_dir1_lon <- fig_point[1]
    entrance_dir1_lat <- fig_point[2]
    
    data <- data.frame(id, zoneDir1, zoneGain_dir1, zoneDir2, zoneGain_dir2, lng1 = entrance_dir1_lon, lat1 = entrance_dir1_lat)
    fig_polygon <- SpatialPolygonsDataFrame(fig_polygon, data, match.ID = F)
    out <- paste("out/","corridor_",id,sep="")
    
    unlink(out,recursive = TRUE)
    dir.create(out)
    fwrite(x = poly.routes, file = paste(out,"/routes.csv",sep=""))
    fwrite(x = poly.stops, file = paste(out,"/stops.csv",sep=""))
    writeOGR(obj=fig_polygon, dsn=out, layer="corridor", driver="ESRI Shapefile") # this is in geographical projection
    return(out)
  }
  
  ## Maps changes according to user input
  
  ## Clean up the map
  observeEvent(input$cleanAll,{
    proxy <- leafletProxy("busmap", session)
    proxy %>% clearShapes() 
  })
  
  ## Choose lines
  observeEvent(input$lignes,{
    proxy <- leafletProxy("busmap", session)
    
    # if( !is.null(input$geom) ){
    #   if( input$geom == TRUE ){
    #     proxy %>% showGroup("geom")
    #   } else {
    #     proxy %>% hideGroup("geom")
    #   }
    # }
    
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
      if( rte %in% input$lignes ){
        plot_route(rte, session)
      }
      
      ## Graph of speed for one route
      if( length(input$lignes) == 1 ){
        
        i.routes_data <- reseau$stops_speed %>%
          filter(route_id == input$lignes )
        
       i.graph <- plot_ly(data = i.routes_data,
                              x = ~stop_sequence,
                              y = ~speed2prev.kmh,
                              text = ~paste("Ligne ", route_id, "<br>",
                                            "arrêt ", stop_id, "<br>",
                                            "sequence ", stop_sequence, "<br>",
                                            "direction ", direction_id, "<br>",
                                            "distance ", round(dist2prev), " m <br>",
                                            "temps ", round(time2prev), " sec <br>",
                                            "vitesse ", round(speed2prev.kmh), " km/h <br>",
                                            sep=""),
                              hoverinfo = "text",
                              type = "bar",
                              width=1000) %>%
          layout(yaxis = list(title = "Vitesse inter-arrêt précédent (km/h)"),
                 xaxis = list(title = "Séquence d'arrêt"))
        
        output$graph <- renderPlotly({i.graph})
        
      }
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

