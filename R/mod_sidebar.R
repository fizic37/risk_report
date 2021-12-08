#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  
  
  bs4Dash::sidebarMenuOutput(outputId = ns("sidebar")) 
  
}
    
#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id, vals) {
  
  moduleServer(id, function(input, output, session) {
    
     risk_user <- bs4Dash::sidebarMenu(
      id = session$ns("tabs"),
      bs4Dash::menuItem(
        tabName = "home",
        text = "Home",
        icon = icon("home"),
        selected = F ),
      
     
      #bs4Dash::menuItem(tabName = "prudentialitate",  text = "Prudentialitate",
        #icon = icon("check-square"),selected = T,startExpanded = T,
        
        bs4Dash::menuItem(
          text = "Raport prudentialitate",
          tabName = "prudentialitate",
          selected = TRUE,
          icon = icon("product-hunt")  )   ,
      hr(),
      
     bs4Dash::menuItem(
        tabName = "banci",
        text = "Banci - Clasa de Risc",
        icon = icon("university"),
        selected = FALSE,startExpanded = T,
          
        bs4Dash::menuSubItem(
           text = "Lista banci ",
           tabName = "lista_banci",
           selected = F,
            icon = icon("list")  )
        #,bs4Dash::menuSubItem(text = "Genereaza rating nou ",tabName = "new_rating", selected = F,icon = icon("folder-plus")  )
        ),
      hr(),
      bs4Dash::menuItem(text = "Admin",icon = icon("toolbox"),tabName = "admin",selected = FALSE)
      
     )
        
     
  
  output$sidebar <- bs4Dash::renderMenu(risk_user)
  
  
  observeEvent(input$tabs,{ 
    # I use this in order to have a selection of all inputs in sidebar. This way, I don`t have to call modules
    # every time a sidebar is selected, I only call modules ones.`
    vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)  })
    
  } )  
   
  
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_ui_1")
