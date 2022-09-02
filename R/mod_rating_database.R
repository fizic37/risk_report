#' rating_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rating_database_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    
    shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),
    
    column(width = 12, h6("Baza de date a ratingurilor. Editeaza-o cu atentie!", style = "color:#ff007b"), br()),
    
    column(width = 8,shinyWidgets::actionBttn(ns("save_baza_date"),label = "De aici salvezi 
                                modificarile bazei de date din excel-ul de mai jos",
                    icon = icon("save"),style = "stretch",color = "primary")),
    
    column(width = 4, shinyWidgets::downloadBttn(ns("down_baza_date"),label = "Download baza de date",
                                                 style = "stretch",color = "primary")),
    column(width = 12, br()),
    
  
  column(width = 12,rhandsontable::rHandsontableOutput(ns("baza_date_rating")) )
  )
      
}
    
#' rating_database Server Functions
#'
#' @noRd 
mod_rating_database_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    
    
    tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    mapare_rating <- readRDS("R/reactivedata/banci/mapare_rating.rds")
    
    vals_rating_database <- reactiveValues()
    
    
    output$baza_date_rating <- rhandsontable::renderRHandsontable( { req( vals$baza_date_rating )
      
    rhandsontable::rhandsontable( data = vals$baza_date_rating %>% dplyr::select(-CodFinantator),
                      readOnly = FALSE,width = 1370, height = 600, 
                      rowHeaderWidth = 100,search = TRUE,rowHeaders = NULL ) %>% 
      
      rhandsontable::hot_cols(manualColumnResize = TRUE, colWidths = c(150,rep(112,18)),
                              columnSorting = TRUE,fixedColumnsLeft = 2) %>%
      
      rhandsontable::hot_col(col = "DenumireFinantator",type = "dropdown",
                             source = unique(tabela_nume_banci$DenumireFinantator)) %>%
      
      rhandsontable::hot_col( col = "Are_rating_extern",  type = "dropdown",
                              source = c("DA","NU"),strict = TRUE,  allowInvalid = FALSE ) %>%
      rhandsontable::hot_col(col = c("Active", "Limita_Banca", "Resurse_financiare_totale"),
                             type = "numeric",format = "000,000") %>%
      
      rhandsontable::hot_col(col = "Clasa_Risc",type = "dropdown", source = c("A","B","C","D","E"),
                             strict = TRUE,  allowInvalid = FALSE) %>%
      
      rhandsontable::hot_col( col = c("Opinia_Auditorului","Actionariat"),  type = "dropdown",
                              source = c(8, 4, 0, NA_integer_),strict = TRUE,  allowInvalid = FALSE ) %>%
      
      rhandsontable::hot_col( col = c("Agentie"),  type = "dropdown",
                              source = unique(mapare_rating$Agentie)#unique(NA_character_,mapare_rating$Agentie)
                              ,strict = TRUE,  allowInvalid = FALSE ) %>%
      
      rhandsontable::hot_col( col = c("Rating_Extern"), type = "dropdown",  
                          source = unique(mapare_rating$Ratings)#unique(NA_character_,mapare_rating$Ratings)
                              ,strict = TRUE,  allowInvalid = FALSE )
    } )
    
    output$down_baza_date <- downloadHandler( filename = function() {"baza_date_rating.csv"},content = function(file) {
      readr::write_csv(x = vals$baza_date_rating, file = file ) } )
    
    observeEvent(input$save_baza_date,{ req(input$baza_date_rating)
      
      vals_rating_database$baza_date_rating <- rhandsontable::hot_to_r(input$baza_date_rating) %>%
        dplyr::left_join(y = tabela_nume_banci %>% dplyr::select(CodFinantator,DenumireFinantator), by = "DenumireFinantator")
      
     
      if ( janitor::compare_df_cols_same(vals_rating_database$baza_date_rating, vals$baza_date_rating)) {
        saveRDS(object = vals_rating_database$baza_date_rating, file = "R/reactivedata/banci/baza_date_rating.rds")
        
        vals$baza_date_rating <- vals_rating_database$baza_date_rating
          
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
            .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
      } else { 
        shinyFeedback::showToast(title = "ERROR",message = "Nu am putut salva", type = "error",
                .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
        }
      
    })
    
    
  
    })
}
    
## To be copied in the UI
# mod_rating_database_ui("rating_database_ui_1")
    
## To be copied in the server
# mod_rating_database_server("rating_database_ui_1")
