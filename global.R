
pkg_vec <- c("shiny","shinydashboard","leaflet","leaflet.extras","plotly","ggplot2","sp",
             "geosphere","ggmap","plyr", "dplyr", "lubridate", "data.table", "rgdal", "tools", "raster")

for( pkg in pkg_vec ){
  if( !(pkg %in% installed.packages()) ){
    install.packages(pkg, character.only = TRUE)
  }
  suppressWarnings(suppressMessages(library(pkg,character.only = TRUE)))
}


CRS.wgs84 <- CRS("+init=epsg:4326") 

options(shiny.maxRequestSize=30*1024^2) 

source("utilities.R")
source("dashboard.R")
source("inputData.R")
source("contact.R")




