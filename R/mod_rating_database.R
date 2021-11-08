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
  fluidRow( column(width = 4,
    dateInput(inputId = ns("data_limite"),label = "Data la care voi afisa limitele in vigoare",
              min = as.Date("2020-08-28"),value = Sys.Date(),language = "ro",autoclose = TRUE)),
    column(width = 8, br(), downloadLink(outputId = ns("down_limite"),label = "Downloadeaza limitele valabile la data selectata",
                                   class = "pull-right")),
    column(width = 12,DT::dataTableOutput(outputId = ns("baza_date_limite")))
  )
}
    
#' rating_database Server Functions
#'
#' @noRd 
mod_rating_database_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(vals$report_date, {
    updateDateInput(session = session,inputId = "data_limite",value = vals$report_date)  })
    
    baza_date_limite <- readRDS("R/reactivedata/banci/sinteza_limite.rds")
    
    baza_date_limite_filtrata <- eventReactive(eventExpr = input$data_limite,{
      baza_date_limite %>% dplyr::filter(DataInitiala <= input$data_limite, DataExpirare >= input$data_limite) %>%
        dplyr::arrange(desc(LimitaTrezorerie),DenumireFinantator)
    })
    
    output$baza_date_limite <- DT::renderDataTable( DT::datatable(data = baza_date_limite_filtrata(),
        rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                      paste0("Limitele de trezorerie valabile la data de ", input$data_limite)) ) %>%
          DT::formatRound(columns = 4,digits = 0))
    
    output$down_limite <- downloadHandler(filename = function() { paste0("limite_trezorerie_", input$data_limite,".csv") },
                content = function(file) { readr::write_csv(x = baza_date_limite_filtrata(),file = file) } )
   
 
  })
}
    
## To be copied in the UI
# mod_rating_database_ui("rating_database_ui_1")
    
## To be copied in the server
# mod_rating_database_server("rating_database_ui_1")
