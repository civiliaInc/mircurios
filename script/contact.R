######################################
## Contact page
page.contact <- function(){
    fluidRow(
  HTML("<br><br>"),
  fluidRow(column(width=4,offset=0,selectInput("mail.to", label = h4("Qui contacter?"), 
                                               choices = list("Support technique Mircurios" = 1, "Gestionnaire STS" = 2, "Service de maintenance" = 3, "Civilia" = 4), 
                                               selected = 1))),
  fluidRow(column(width=5,offset=0,  passwordInput("mail.subject", "Sujet :", value="") ) ),
  fluidRow(column(width=12,offset=0,  textAreaInput("mail.msg", "Message :", value="Votre message...") ) ),
  fluidRow(column(width=12,offset=0, actionButton("mail.send", "Envoyer" ) ) ),
  HTML("<br><br>")
  )
}
