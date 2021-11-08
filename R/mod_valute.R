#' valute UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' Handles Sold garantii - tabel 2 box
mod_valute_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("tabel2")), 
    hr(),
    DT::dataTableOutput(ns("tabel2_extins"))
  )
}
    
#' valute Server Functions
#'
#' @noRd 
mod_valute_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    baza_banci <- readRDS("R/reactivedata/solduri/baza_banci.rds") %>% dplyr::filter(Tip_surse == "Surse_proprii")
    
    
    observeEvent(vals$report_date,{
    
      
    vals$tabel2 <- tryCatch(expr = {
      baza_banci %>% dplyr::filter(Tip_surse == "Surse_proprii") %>% 
      dplyr::filter(data_raport== vals$report_date) %>%
      dplyr::group_by(Valuta) %>% dplyr::summarise(Sold_garantii = sum(`Soldul garantiei [in LEI]`)) %>%
      dplyr::mutate(Variatie_luna_anterioara = purrr::map2_dbl(.x = Valuta,.y = Sold_garantii,.f = ~ (.y/
        sum(baza_banci$`Soldul garantiei [in LEI]`[baza_banci$Valuta==.x & baza_banci$data_raport==vals$previous_month]))-1),
                    Variatie_anul_anterior = purrr::map2_dbl(.x = Valuta,.y = Sold_garantii,.f = ~ (.y/
            sum(baza_banci$`Soldul garantiei [in LEI]`[baza_banci$Valuta==.x & baza_banci$data_raport==vals$previous_year]))-1)) %>%
      dplyr::arrange(desc(Sold_garantii)) %>% dplyr::bind_rows(data.frame(Valuta="Total",
                    Sold_garantii = sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$Tip_surse=="Surse_proprii" & 
                      vals$view_baza_solduri$data_raport==vals$report_date] ),
          Variatie_luna_anterioara =  sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$Tip_surse=="Surse_proprii" & 
             vals$view_baza_solduri$data_raport==vals$report_date])/sum(vals$view_baza_solduri$Sold_garantii[
               vals$view_baza_solduri$Tip_surse=="Surse_proprii" & vals$view_baza_solduri$data_raport==vals$previous_month])-1,
          Variatie_anul_anterior = sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$Tip_surse=="Surse_proprii" & 
             vals$view_baza_solduri$data_raport==vals$report_date])/sum(vals$view_baza_solduri$Sold_garantii[
               vals$view_baza_solduri$Tip_surse=="Surse_proprii" & vals$view_baza_solduri$data_raport==vals$previous_year])-1))
    }, error = function(e) { data.frame(Valuta = conditionMessage(e), Sold_curent=NA_integer_, Sold_anterior=NA_real_) } )
    
    
    output$tabel2 <- DT::renderDataTable( { 
      DT::datatable(data = vals$tabel2,rownames = FALSE, options = list(dom = "Bt", buttons = c("copy","excel")), 
          extensions = "Buttons", colnames = c("Valuta",paste0("Sold ",vals$report_date),paste0("Variatie fata de ",vals$previous_month),
                  paste0("Variatie fata de ", vals$previous_year)),
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                    "Tabelul 2 – Evoluţia pe valute a soldului garanţiilor emise pe seama capitalurilor proprii:")) %>% 
        DT::formatRound(columns = 2,digits = 0) %>% DT::formatPercentage(columns = 3:ncol(vals$tabel2), digits = 1)
                 })
   
    
    tabel2_extins <- reactive( {  req(vals$report_date, vals$previous_month, vals$previous_year)
        baza_banci %>% dplyr::filter(Tip_surse == "Surse_proprii", data_raport %in% c(vals$report_date, vals$previous_month, 
              vals$previous_year) ) %>% dplyr::group_by(data_raport, Valuta) %>%  
          dplyr::summarise(Sold_garantii = sum(`Soldul garantiei [in LEI]`)) %>% dplyr::arrange(desc(data_raport), desc(Sold_garantii)) %>% 
          tidyr::pivot_wider( names_from = data_raport,       values_from = Sold_garantii,  names_prefix = "Sold_")  })
    
    output$tabel2_extins <- DT::renderDataTable(DT::datatable(data = tabel2_extins() %>% janitor::adorn_totals(where = "row"),
        rownames = FALSE, options = list(dom="Bt", buttons = c("copy","excel")),extensions = "Buttons",
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                     "Solduri pe valute surse proprii")) %>% DT::formatRound(columns = 2:ncol(tabel2_extins())))
    
    })
  })
}
    
## To be copied in the UI
# mod_valute_ui("valute_ui_1")
    
## To be copied in the server
# mod_valute_server("valute_ui_1")
