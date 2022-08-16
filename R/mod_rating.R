#' rating UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rating_ui <- function(id){
  ns <- NS(id)
  tagList( 
  bs4Dash::box( title = "Info rating banci",icon = icon("info"),
               status = "lightblue",width = 12,collapsible = T,collapsed = F,maximizable = T),
  
  bs4Dash::box( title = "Baza istorica a ratingurilor",icon = icon("database"),
                footer = "De aici baza de date a ratingurilor poate fi editata manual. 
                Este recomandabil sa folosesti casuta de mai jos, Update existing ratings, intrucat are implementate mai multe reguli
                de validare. Aici nu voi calcula scorul sau clasa de risc finala, ci 
                acestea vor trebui sa fie introduse manual. Nu validez rating-ul selectat in functie de agentie",
               status = "lightblue",width = 12,collapsible = T,collapsed = TRUE,maximizable = T,
               mod_rating_database_ui("rating_database_ui_1") ),
  
  bs4Dash::box( title = "Update existing ratings",icon = icon("edit"),
               status = "lightblue",width = 12,collapsible = T,collapsed = F,maximizable = T,
               mod_rating_update_ui("rating_update_ui_1") )
  )
}
    
#' rating Server Functions
#'
#' @noRd 
mod_rating_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
  })
}
    
## To be copied in the UI
# mod_rating_ui("rating_ui_1")
    
## To be copied in the server
# mod_rating_server("rating_ui_1")
