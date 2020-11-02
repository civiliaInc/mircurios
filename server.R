shinyServer( function(input, output,session ) {
  
  ## Update the user status (logged or not)
  user <- reactiveValues(logged = FALSE)
  
  ## Look at the connection button
  # observeEvent(input$connection, {
  #   
  #   ## If not logged, check login and password
  #   if (user$logged == FALSE) {
  #     if (!is.null(input$login)) {
  #       if (input$login > 0) {
  #         
  #         ## Retrieve login and password
  #         login <- isolate(input$login)
  #         pwd   <- isolate(input$pwd)
  #         
  #         ## Test login and pwd
  #         if( test.login.pwd(login,pwd) == TRUE ) user$logged <- TRUE
  #       }
  #     }
  #   }
  # })
  
  ## Is the user logged in or not?
#  observe({
    
    ## Unlogged user: welcome page
#    if ( user$logged == FALSE) output$page <- renderUI({ page.login() })
    
    ## Logged user: dashboard page
#    if ( user$logged == TRUE){
      output$page <- renderUI({ page.dashboard() })
#    }
#  })
  
  ## Load the static gtfs data
  observeEvent(input$gtfs_zip, load.gtfs.static(input, output,session) )
  #observeEvent(input$button.loadData, load.gtfs.static(output,session) )
  
  ## Maps changes according to user input
  observe({
    proxy <- leafletProxy("busmap", session)
    zones.grp <- c("Cool points","Heat points")
    for( i in 1:length(zones.grp)){
      if( i %in% input$zones ) proxy %>% showGroup(zones.grp[i]) else proxy %>% hideGroup(zones.grp[i])
    }
    
    points.grp <- c("Green stops","Yellow stops","Red stops")
    for( i in 1:length(points.grp)){
     if( i %in% input$points ) proxy %>% showGroup(points.grp[i]) else proxy %>% hideGroup(points.grp[i])
    }
    
    if( exists("avg.routes_speed")) routes.grp <- avg.routes_speed$route_id else routes.grp <- c()
    ## Display single routes
    for( rte in routes.grp){
      if( rte %in% input$lignes ) proxy %>% showGroup(paste0("Line ",rte)) else proxy %>%hideGroup(paste0("Line ",rte))
    }
    ## Display all routes
    if( !is.null(input$lignes)){
    if( input$lignes == "toutes les lignes"){
      for( rte in routes.grp){
    proxy %>% showGroup(paste0("Line ",rte))
      }
    }
    }
    
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

