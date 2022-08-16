#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  sidebar_selected <- c()
  box_selected <- c()
  raport_selected_tab <- c() #"Cap I - Garantii"
  final_report_check <- c()
    
  view_baza_solduri <- readRDS("R/reactivedata/solduri/view_baza_sold.rds")
  
  
  
  vals <- reactiveValues(sidebar_selected = sidebar_selected, view_baza_solduri = view_baza_solduri,
                         raport_selected_tab = raport_selected_tab, 
                         box_selected=box_selected,
                         final_report_check = final_report_check,box_upload_solduri_block = FALSE)
  
  vals_balanta <- reactiveValues()
  
  observeEvent(vals$report_date,{
  
    vals$previous_month <- vals$report_date %>% lubridate::floor_date(unit = "months")-1
  
    vals$previous_year <- lubridate::`%m-%`(vals$report_date, lubridate::years(1))
  })
  
  
  
  mod_sidebar_server(id = "sidebar_ui_1", vals = vals)
  
  observeEvent(vals$sidebar_selected,{
    
    if (sum("prudentialitate" == vals$sidebar_selected)==1) {
      mod_prudentialitate_server("prudentialitate_ui_1", vals)
    
      vals$sidebar_selected <- c(vals$sidebar_selected,"prudentialitate") }
    
    if (sum("prima_casa" == vals$sidebar_selected)==1) {
      callModule(mod_raportare_bnr_server, "raportare_bnr_ui_1", vals)
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"prima_casa") }
    
    
    if (sum("banci" == vals$sidebar_selected)==1) {
      vals$baza_date_rating <- readRDS( "R/reactivedata/banci/baza_date_rating.rds" )
      
      mod_rating_update_server("rating_update_ui_1", vals)
      
      mod_rating_database_server("rating_database_ui_1", vals)
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"banci") }
    
    if (sum("admin" == vals$sidebar_selected)==1) {
      mod_admin_server("admin_ui_1", vals)
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"admin") }
    
    } )
  
  #observeEvent(vals$raport_selected_tab,{
  observe({
    
    if ( sum("provizioane_plati" == vals$final_report_check)==1 || sum("provizioane_plati" == vals$raport_selected_tab)==1 ) {
      mod_provizioane_server("provizioane_ui_1", vals) 
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'provizioane_plati')
      vals$final_report_check <- c(vals$final_report_check, "provizioane_plati")
    }
    
    if ( sum("grupuri" == vals$final_report_check)==1 || sum("grupuri" == vals$raport_selected_tab)==1 ) {
      mod_top_expuneri_grupuri_server("top_expuneri_grupuri_ui_1", vals)
      mod_grupuri_server("grupuri_ui_1", vals) 
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'grupuri')
      vals$final_report_check <- c(vals$final_report_check, "grupuri")
    }
    
    
    if ( sum("plafoane" == vals$final_report_check) == 1 || sum("plafoane" == vals$raport_selected_tab)==1 ) { 
      mod_plafoane_server("plafoane_ui_1", vals)
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'plafoane')
      vals$final_report_check <- c(vals$final_report_check, "plafoane")
    }
    
    
    if ( sum("plasamente" == vals$raport_selected_tab)==1  || sum("plasamente" == vals$final_report_check) == 1)    { 
      
      mod_plasamente_server("plasamente_ui_1", vals, vals_balanta)
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'plasamente')
      vals$final_report_check <- c(vals$final_report_check, "plasamente")
    }
    
    
    if ( sum("plati" == vals$final_report_check) == 1 || sum("plati" == vals$raport_selected_tab)==1 ) { 
      mod_plati_server("plati_ui_1", vals)
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'plati') 
      vals$final_report_check <- c(vals$final_report_check, "plati")
    }
    
    if ( sum("anexe" == vals$final_report_check) == 1 || sum("anexe" == vals$raport_selected_tab)==1 ) { 
      mod_anexe_server("anexe_ui_1", vals)
      
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'anexe') 
      vals$final_report_check <- c(vals$final_report_check, "anexe")
    }
    
    
    if ( sum("final_report" == vals$raport_selected_tab)==1 ) {  
      mod_final_report_server("final_report_ui_1", vals) 
      vals$raport_selected_tab <- c(vals$raport_selected_tab, 'final_report')      } 
    
    
  })
  
  observeEvent(vals$box_selected,{ 
    
    if ( sum("box_tabel2" == vals$box_selected) == 1 ) { 
     
       mod_valute_server("valute_ui_1", vals)
      vals$box_selected <- c(vals$box_selected, "box_tabel2") # make sure server function is only called once
      }
    
    
    if ( sum("box_database_solduri" == vals$box_selected) == 1 )  { 
     
       mod_garantii_database_server("garantii_database_ui_1", vals)
      vals$box_selected <- c(vals$box_selected, "box_database_solduri")
      } 
    
    
    if ( sum("box_upload_solduri" == vals$box_selected) == 1 ) { 
      
      mod_garantii_upload_server("garantii_upload_ui_1", vals)  
      vals$box_selected <- c(vals$box_selected, "box_upload_solduri") } 
    
    
    if ( sum("box_manual_solduri" == vals$box_selected) == 1 )  { 
      
      mod_garantii_manual_server("garantii_manual_ui_1", vals) 
      vals$box_selected <- c(vals$box_selected, "box_manual_solduri")
      } 
    
    # I do not know why below 2 boxes need to be selected twice
    if ( sum("box_database_plasamente" == vals$box_selected) == 2 ) { 
      
      mod_balanta_database_server( "balanta_database_ui_1", vals, vals_balanta )
      vals$box_selected <- c(vals$box_selected, "box_database_plasamente")
      } 
    
    if ( sum("box_upload_plasamente" == vals$box_selected) == 2 ) { 
      
      mod_plasamente_upload_server("plasamente_upload_ui_1", vals, vals_balanta) 
      vals$box_selected <- c(vals$box_selected, "box_upload_plasamente")
      }
      
  })
  
  
}
