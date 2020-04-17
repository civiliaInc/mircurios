########################################################################################
## Average speeds for routes -> to be replaced by average over trips more than trips
plot.vitesses <- function(output){
output$vitesses <- renderPlotly({
  x.tick <- list(
    size = 8
  )
  x <- list(
    title = "Routes",
    autotick = FALSE,
    tickfont = x.tick,
    tickmode = "array",
    ticks = "outside",
    tickvals = avg.routes_speed$ID,
    ticktext = avg.routes_speed$route_id,
    range=c(min(avg.routes_speed$ID)-1,max(avg.routes_speed$ID)+1)
  )
  y <- list(
    title = "Vitesses commerciales moyennes (Km/h)"
  )
  mean.speed <- mean(avg.routes_speed$route.avgspeed.kmh)
  
  
  plot_ly(avg.routes_speed,
          x=~ID,
          y=~route.avgspeed.kmh,
          type="bar",
          text = ~paste("Route ", route_id, " : ", round(route.avgspeed.kmh,digits=1), "Km/h"),
          hoverinfo = "text",
          name = "routes_chosen",
          marker = list(size = 15, opacity=0.65, color=~col)
  ) %>%
    layout(xaxis = x, yaxis = y) %>%
    layout(showlegend = FALSE)%>%
    add_segments(x=-10,xend=100,y=mean.speed,yen=mean.speed,mode="lines")
})
}

  

