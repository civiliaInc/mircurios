
######################################
## Wrapper for multiline messages
textareaInput <- function(inputId, label, value="", placeholder="", rows=5){
  tagList(
    div(strong(label), style="margin-top: 5px;"),
    tags$style(type="text/css", "textarea {width:200%; margin-top: 5px;}"),
    tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
}