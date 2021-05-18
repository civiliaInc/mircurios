########################################################
## Depending on the login, display an html page
#shinyUI(htmlOutput("page"))

shinyUI(

  dashboardPage(
    ## General color
    skin = "black",
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
        tabItem(tabName = "graphes",
                db.body.graphes())
      )
    )
  )

)