
##########################################
## Dashboard structure
page.dashboard <- function()
{
  dashboardPage(
    ## General color
    skin = "green",
    ## Dashboard header
    dashboardHeader(title = "MIRCURIOS"),
    ## Dashboard sidebar
    dashboardSidebar(db.sidebar()),
    ## Dashboard body
    dashboardBody(
      ## Tabs on the side
      tabItems(
        tabItem(tabName = "carte",
                db.body.carte.test()),
        tabItem(tabName = "vitesses",
                db.body.vitesses()),
        tabItem(tabName = "variations",
                db.body.variations()),
        tabItem(tabName = "contact",
                db.body.contact())
      )
    )
  )
}

##########################################
## Dashboard sidebar
db.sidebar <- function(){
  # The dynamically-generated user panel
  sidebarMenu(
    #fileInput("geom_obj", buttonLabel="Charger une géométrie", label=NULL,
    #          multiple = FALSE,
    #          accept = c(".zip","kmz")),
    fileInput("gtfs_zip", buttonLabel="Charger un GTFS", label=NULL,
              multiple = FALSE,
              accept = c(".zip")),
    actionButton("cleanAll", "Nettoyage"),
    menuItem("Carte", tabName = "carte", icon = icon("globe"))
    #menuItem("Vitesses", tabName = "vitesses", icon = icon("bus")),
    #menuItem("Variations", tabName = "variations", icon = icon("line-chart")),
    #menuItem("Contact", tabName = "contact", icon = icon("envelope"))
  )
}

##########################################
## Speeds
db.body.vitesses <- function(){
  fluidRow(
    ## Speeds
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               plotlyOutput("vitesses", height = 500, width="100%")
           )
    )
  )
}
##########################################
## Contact
db.body.contact <- function(){
  page.contact()
}

##########################################
## Dashboard body : maps
db.body.carte.test <- function(){
  
  div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      ## City map with all infos
      leafletOutput("busmap", height = 1000, width='auto'),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    selectInput("lignes", h3("Lignes de bus"), c() ),
                    checkboxInput("geom", label = h4("Géométrie"), value=FALSE)
                    #checkboxInput("allStops", label = h4("Tous les arrêts"), value=FALSE)
                    #checkboxGroupInput("zones", label = h3("Zones de vitesse"), 
                    #                   choices = list("Rapides" = 1, "Lentes" = 2))
               
      )
      
  )
  
  
  
}


##########################################
## Dashboard body : maps
db.body.carte <- function(){
  fluidRow(
    ## Map
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("busmap", height = 500)
           )
           # ,
           # box(width = NULL,
           #     uiOutput("numVehiclesTable")
           # )
    )
    ## Selection boxes
    #column(width = 3,
    #       box(width = NULL, status = "warning",
    #           uiOutput("routeSelect"),
    #           checkboxGroupInput("routeNum", "Show",
    #                              choices = c(
    #                                "1" = 1,
    #                                "2" = 2,
    #                                "3" = 3,
    #                                "4" = 4,
    #                                "All stops" = 5,
    #                                "All routes" = 6
    #                              ),
    #                              selected = c(1, 2, 3, 4)
    #           ),
    #           p(
    #             class = "text-muted",
    #             paste("Note: a route number can have several different trips, each",
    #                   "with a different path. Only the most commonly-used path will",
    #                   "be displayed on the map."
    #             )
    #           ),
    #           actionButton("zoomButton", "Zoom to fit buses")
    #       ),
    #       box(width = NULL, status = "warning",
    #           selectInput("interval", "Refresh interval",
    #                       choices = c(
    #                         "30 seconds" = 30,
    #                         "1 minute" = 60,
    #                         "2 minutes" = 120,
    #                         "5 minutes" = 300,
    ##                         "10 minutes" = 600
    #                       ),
    #                       selected = "60"
    #           ),
    #           uiOutput("timeSinceLastUpdate"),
    #           actionButton("refresh", "Refresh now"),
    #           p(class = "text-muted",
    #             br(),
    #             "Source data updates every 30 seconds."
    #           )
    #       )
    #)
  )
}

#######################################
## Variation of speeds over periods
db.body.variations <- function(){
  fluidRow(
    ## Speeds
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               plotlyOutput("variations", height = 500, width="100%")
           )
    )
  )
}
#######################################
db.body.performances <- function(){}
