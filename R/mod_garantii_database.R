#' garantii_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

# This modules handles submenu soldul de garantii under sidebar Prudentialitate - Database Tab
mod_garantii_database_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(   shinyjs::useShinyjs(),
    column( width = 3, selectInput( inputId = ns("date_baza_solduri"),
        label = "Selecteaza data raportului",  choices = c() )  ),
    column(width = 3),
    column( width = 6,  br(),    downloadLink(outputId = ns("down_solduri"),
                   label = "Download tabelul de mai jos in format detaliat")),
    
    DT::dataTableOutput(ns("baza_date_solduri"))
  )
  
}
    
#' garantii_database Server Functions
#'
#' @noRd 
mod_garantii_database_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #Below observer selects tab Database when sidebar soldul de garantii is selected and 
    #updates selecion dates of the table according to what exists in file (vals$view_baza_solduri - first read inside app.server)
    # It also renders the table as it depends to the observed event - vals$view_baza_solduri
    
    observeEvent(vals$view_baza_solduri,{
      updateSelectInput(inputId = 'date_baza_solduri',  session = session,
      choices = vals$view_baza_solduri$data_raport %>% unique() %>% sort(decreasing = TRUE) )
      
      output$baza_date_solduri <- DT::renderDataTable(
        DT::datatable(data = vals$view_baza_solduri %>%
                        dplyr::filter(data_raport == input$date_baza_solduri)  %>%
                        dplyr::select(-data_raport) %>% dplyr::arrange(Tip_surse, desc(Sold_garantii)),
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                              paste0("Baza de date a soldurilor la data de ", input$date_baza_solduri)),
                      options = list(dom =  "Bt", pageLength = nrow(vals$view_baza_solduri), buttons = c("copy","csv","excel")),
                      rownames = F,extensions = "Buttons") %>%
                        DT::formatRound(columns = 3:6, digits = 0)
      )
      output$down_solduri <- downloadHandler(filename = function() { paste0("solduri_",input$date_baza_solduri,".csv") },
                content = function(file) { readr::write_csv(x = readRDS("R/reactivedata/solduri/baza_banci.rds") %>%
                    dplyr::filter(data_raport == input$date_baza_solduri), file = file) }  )
      
      })
    
    
    
     
    
    
    
    } )
  
}
 
    
## To be copied in the UI
# mod_garantii_database_ui("garantii_database_ui_1")
    
## To be copied in the server
# mod_garantii_database_server("garantii_database_ui_1")
