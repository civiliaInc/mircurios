plot.variations <- function(output){
  
## Split the data on stops according to the day time
## Set the colors to the color for the mean value of the total speed
## 6am-9am
prime <- stops_speed %>% 
  dplyr::filter(hour(arrival_time) >= 6 & hour(arrival_time) < 9) %>% 
  group_by(route_id) %>% 
  mutate(route.avgspeed.kmh=mean(speed2prev.kmh,na.rm=TRUE)) %>%
  distinct(route.avgspeed.kmh) %>%
  select(route_id,route.avgspeed.kmh)
## 9am-3pm
sexte <- stops_speed %>% 
  dplyr::filter(hour(arrival_time) >= 8 & hour(arrival_time) < 15) %>% 
  group_by(route_id) %>% 
  mutate(route.avgspeed.kmh=mean(speed2prev.kmh,na.rm=TRUE)) %>%
  distinct(route.avgspeed.kmh) %>%
  select(route_id,route.avgspeed.kmh)
## 3pm-6pm
none <- stops_speed %>% 
  dplyr::filter(hour(arrival_time) >= 15 & hour(arrival_time) < 18) %>% 
  group_by(route_id) %>% 
  mutate(route.avgspeed.kmh=mean(speed2prev.kmh,na.rm=TRUE)) %>%
  distinct(route.avgspeed.kmh) %>%
  select(route_id,route.avgspeed.kmh)
## 6pm-13pm
vepres <- stops_speed %>% 
  dplyr::filter(hour(arrival_time) >= 18 & hour(arrival_time) < 25) %>% 
  group_by(route_id) %>% 
  mutate(route.avgspeed.kmh=mean(speed2prev.kmh,na.rm=TRUE)) %>%
  distinct(route.avgspeed.kmh) %>%
  select(route_id,route.avgspeed.kmh)

## Merge all mean speed in a single dataset for all routes
route.var <- prime %>% 
    full_join(sexte,by="route_id") %>% 
    full_join(none,by="route_id") %>%
    full_join(vepres,by="route_id") %>%
  select(1,"prime"=2,"sexte"=3,"none"=4,"vepres"=5) %>%
  left_join(avg.routes_speed,by="route_id")

output$variations <- renderPlotly({
  
## Make a plotly out of these mean speeds
x <- list(
  title = "Périodes de la journée",
  autotick = FALSE,
  tickmode = "array",
  ticks = "outside",
  tickvals = 1:4,
  ticktext = c("6h-9h","9h-15h","15h-18h","18-25h"),
  range=c(0.5,4.5)
)
y <- list(
  dtick=5,
  range=c(0,max(route.var[,2:5])),
  title = "Vitesses commerciales moyennes (Km/h)"
)

## Plot the first route to create the canvas
yval <- as.data.frame(t(route.var[1,2:5]))
yval$period <- 1:4
colnames(yval)[1] <- "speed"
p <- plot_ly(data=yval, x=~period, y=~speed, type="scatter",mode="markers+lines") %>% layout(xaxis = x, yaxis = y)

## Loop over remaining routes
for( i in 1:nrow(route.var)){
  yval <- as.data.frame(t(route.var[i,2:5]))
  yval$period <- 1:4
  colnames(yval)[1] <- "speed"
  col.trc <- as.character(route.var[i,7])
  num.trc <- as.character(route.var[i,1])
p <- add_trace(p, data=yval, x=~period, y=~speed, type="scatter", mode="markers+lines",
                text = paste0("Route ",num.trc),
               hoverinfo = text,
                marker=list(color=col.trc),
                line=list(color=col.trc), evaluate=TRUE)
}
p %>%
  layout(showlegend = FALSE, title = 'Évolution des vitesses',
         shapes = list(type = "rect",
                       fillcolor = "lightblue", line = list(color = "lightblue",opacity=0.3), opacity = 0.3,
                       x0 = 1, x1 = 4,
                       y0 = 10, y1 = 30)) ## Add a rectangle for the mean value

})

}




