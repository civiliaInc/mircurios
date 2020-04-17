################################################
## Welcome page with login
page.login <- function()
{
  fluidPage(
    tabPanel("Login",
             br(),
             tags$form(
               div(style="margin-left:320px;margin-top:20px;",
                   img(src="CIVILIA_logo_couleur_mac.png", width=270, height=100)),
               div(style="margin-left:300px;margin-top:50px;",
                   textInput("login", "Nom d'utilisateur"),
                   passwordInput("pwd",label = "Mot de passe"),
                   actionButton("connection","Connexion")
               )
             )
    )
  )
}

#########################################################
## Check login and password
test.login.pwd <- function(login,pwd){
login.pwd <- FALSE
## Is that a known login? 
test.login <- login %in% db$login

if( test.login == FALSE ) showModal(modalDialog(title = "Connection refusée", "Ce login est inconnu", easyClose = TRUE))
else{
  ## Is that a known pwd
  test.pwd <- pwd %in% db$pwd
  if( test.pwd == FALSE ) showModal(modalDialog(title = "Connection refusée", "Ce mot de passe est inconnu", easyClose = TRUE))
  else{
    login.pwd <- TRUE
    ## Retrieve the DB
    get.db(login)
  }
}

return(login.pwd)
}

#########################################################
## Logins and passwords

db.login <- c("sts")
db.pwd <- c("sts")
db <<- list("login"=db.login,"pwd"=db.pwd)

## Given a login, retrieve the DB info
get.db <- function( login ){
  db.entry <- which( db$login == login )
  this.db <<- list("login"=db$login[db.entry],"pwd"=db$pwd[db.entry])
}

