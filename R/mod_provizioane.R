#' provizioane UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_provizioane_ui <- function(id){
  ns <- NS(id)
  fluidPage(br(),
    shinybusy::add_busy_spinner(  color = "#ff007b",    position = "bottom-right",    timeout = 200 ),
    
    DT::dataTableOutput(ns("provizioane_plati")))
}
    
#' provizioane Server Functions
#'
#' @noRd 
mod_provizioane_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(vals, {
      vals$provizioane_plati <-   readRDS("R/reactivedata/plati/external_volume/baza_provizioane_plati.rds") %>%
        dplyr::filter(data_raport %in% c(vals$report_date, vals$previous_month, vals$previous_year)) %>%
        dplyr::group_by(data_raport) %>% dplyr::summarise(
          PlatiTotale = sum(PlatiEfective),
          RecuperariTotale =    sum(TotalRecuperat),
          CreantaNeta = sum(Plata_neta),
          GarantiiAccesorii =   sum(ValoareAdmisaFNG),
          Provizion = sum(ProvizionNou)     ) %>%
        dplyr::mutate(Acoperire_Provizioane = Provizion / CreantaNeta) %>%
        dplyr::arrange(desc(data_raport))
        })
    
   
   
    output$provizioane_plati <- DT::renderDataTable(DT::datatable(data = vals$provizioane_plati,
        rownames = FALSE, options = list(dom = "Bt", buttons = c("copy","csv", "excel")), extensions = "Buttons",
        caption = "Sinteza provizioane specifice") %>%
      DT::formatRound(columns = 2:6, digits = 0) %>% DT::formatPercentage(columns = 7, digits = 1)
    )
 
  })
}
    
## To be copied in the UI
# mod_provizioane_ui("provizioane_ui_1")
    
## To be copied in the server
# mod_provizioane_server("provizioane_ui_1")
