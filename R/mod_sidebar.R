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
    
     admin_user <- bs4Dash::sidebarMenu(
      id = session$ns("tabs"),
      bs4Dash::menuItem(
        tabName = "home",
        text = "Home",
        icon = icon("home"),
        selected = F ),
      
      hr(),
    
      bs4Dash::menuItem(
          text = "Raport prudentialitate",
          tabName = "prudentialitate",
          selected = TRUE,
          icon = icon("product-hunt")  )   ,
      hr(),
      
      bs4Dash::menuItem( text = "BNR raportari",startExpanded = TRUE,
              tabName = "bnr", selected = FALSE,  icon = icon("bold"),
          
          bs4Dash::menuSubItem(text = "Prima Casa",tabName = "prima_casa",
                               icon = icon("house-user"),selected = FALSE ) ),
      
     hr(),
      
     bs4Dash::menuItem( tabName = "banci",
        text = "Banci - Clasa de Risc",   icon = icon("star"),
        selected = FALSE,startExpanded = F),
       
      hr(),
     
      bs4Dash::menuItem(text = "Administrator",icon = icon("tools"),tabName = "admin",selected = FALSE)
      
     )
     
     risk_user <- bs4Dash::sidebarMenu(
       id = session$ns("tabs"),
       bs4Dash::menuItem(
         tabName = "home",
         text = "Home",
         icon = icon("home"),
         selected = F ),
       
       hr(),
       
       bs4Dash::menuItem(
         text = "Raport prudentialitate",
         tabName = "prudentialitate",
         selected = TRUE,
         icon = icon("product-hunt")  )   ,
       hr(),
       
       bs4Dash::menuItem( text = "BNR raportari",startExpanded = TRUE,
                          tabName = "bnr", selected = FALSE,  icon = icon("bold"),
                          
                          bs4Dash::menuSubItem(text = "Prima Casa",tabName = "prima_casa",
                                               icon = icon("house-user"),selected = FALSE ) ),
       
       hr(),
       
       bs4Dash::menuItem( tabName = "banci",
                          text = "Banci - Clasa de Risc",   icon = icon("star"),
                          selected = FALSE,startExpanded = F)
       
       )
     
     guest_user <- bs4Dash::sidebarMenu(
       id = session$ns("tabs"),
       
       bs4Dash::menuItem(
         tabName = "home",
         text = "Home",
         icon = icon("home"),
         selected = F ),
       
       hr(),
       
       bs4Dash::menuItem(
         text = "Raport prudentialitate",
         tabName = "prudentialitate",
         selected = TRUE,
         icon = icon("product-hunt")  )   ,
       hr(),
       
       
       bs4Dash::menuItem( tabName = "banci",
                          text = "Banci - Clasa de Risc",   icon = icon("star"),
                          selected = FALSE,startExpanded = F)
       
     )
        
  no_user <- bs4Dash::sidebarMenu()
  
  
  output$sidebar <- bs4Dash::renderMenu({
     if( is.null(vals$user_type) ) {no_user} else if ( vals$user_type == 'admin') {admin_user} else { risk_user}
   
  })
  
 
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
