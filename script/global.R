
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(ggplot2)
library(sp)
library(geosphere)
library(ggmap)
library(plyr)
library(dplyr)
library(lubridate)
library(data.table)

options(shiny.maxRequestSize=30*1024^2) 

source("utilities.R")
source("dashboard.R")
source("inputData.R")
source("contact.R")

