#' top_expuneri_grupuri UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_top_expuneri_grupuri_ui <- function(id){
  # Handles Grupuri Debitori tabpanel - boxes Database grupuri & Top 10 expuneri garantate
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(color = "#ff007b", position = "bottom-right", timeout = 200),
    
  bs4Dash::box( title = "Database grupuri", collapsible = TRUE, collapsed = FALSE, width = 12, maximizable = T,
                status = "primary",icon = icon("database"),
                DT::dataTableOutput( ns("grupuri_database") )  ),
  
  bs4Dash::box( title = "Top 10 expuneri garantare",collapsible = TRUE,collapsed = FALSE,width = 12,maximizable = T,
                status = "primary", icon = icon("chart-line"),
      fluidPage(
          shinyWidgets::airMonthpickerInput(ns("date_expuneri"),label = "Data expunerilor",
                                            value = Sys.Date(), width = "300px"),
 
            fluidRow(column(width = 8,   DT::dataTableOutput(ns("top_expuneri"))),
                      column(width = 4, DT::dataTableOutput(ns("top_expuneri_proprii")))
  ))
  )
  )
}
    
#' top_expuneri_grupuri Server Functions
#'
#' @noRd 
mod_top_expuneri_grupuri_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals_expuneri <- reactiveValues()
    
    baza_grupuri <- readRDS("R/reactivedata/grupuri/baza_grupuri.rds")
    
    # Process Database grupuri. I use observeevent in order to pass vals$grupuri to mod_final_report
    observeEvent(vals,{
    vals$grupuri <- baza_grupuri %>% dplyr::group_by(data_grupuri) %>%
      dplyr::summarise(Nr_grupuri = dplyr::n_distinct(GrupId),
                       Expunere_garantare_totala = sum(Expunere_garantare, na.rm = TRUE)  ) %>%
      dplyr::left_join(
        y = baza_grupuri %>% dplyr::filter(tipologie_conventie == "surse_proprii") %>% dplyr::group_by(data_grupuri) %>%
          dplyr::summarise(Expunere_garantare_surse_proprii = sum(Expunere_garantare, na.rm = TRUE)  ),
        by = "data_grupuri") %>%
      
      dplyr::left_join(
        y = baza_grupuri %>% dplyr::filter(Expunere_garantare > 0) %>% dplyr::group_by(data_grupuri) %>%
          dplyr::summarise(Nr_grupuri_expunere_garantare =  dplyr::n_distinct(GrupId)), by = "data_grupuri") %>%
      dplyr::arrange(desc(data_grupuri))
    
    })
    
    updateSelectInput("date_expuneri",session = session,choices = sort(unique(baza_grupuri$data_grupuri), 
                                                                       decreasing = TRUE))
    
    
    
    
    output$grupuri_database <- DT::renderDataTable(
      DT::datatable(  data =  vals$grupuri,   rownames = FALSE, extensions = "Buttons",
                      options = list(dom = "Bt", pageLength = 5),
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                        "Baza date Grupuri:")) %>% DT::formatRound(columns = 2:5, digits = 0)   )
    
    ##########Process Top 10 expuneri garantate   #############
    
    # Update data expuneri input to vals$report_date only once; I want to let the user the possibility
    # to choose old expuneri date no matter vals$report_date. I had to add 1 to updated value, a strange behaviour of the function
    observeEvent( vals$report_date,{
      shinyWidgets::updateAirDateInput(session = session,inputId = "date_expuneri",
                                       value = vals$report_date )
      vals_expuneri$data_expuneri <- vals$report_date
    }, once = TRUE)
    
    observeEvent( vals_expuneri$data_expuneri,{ 
      
      solduri_provizioane <- readRDS("R/external_volumes/portofoliu/portof_database.rds") %>% 
        dplyr::filter(anul_de_raportare == 
                        lubridate::`%m-%`(vals$report_date, months(lubridate::month(vals$report_date)%%3))) %>% 
        dplyr::group_by(`Cod Partener`) %>% dplyr::summarise(Provizion = sum(provizion_contabil, na.rm = T))
      
      baza_solduri <- readRDS("R/reactivedata/solduri/baza_banci.rds") %>% 
        dplyr::filter(data_raport == vals_expuneri$data_expuneri) %>%
        
        dplyr::left_join(baza_grupuri %>% dplyr::filter(data_grupuri == vals_expuneri$data_expuneri ) %>% 
                           dplyr::group_by(Code,Grup,NrGrup) %>% dplyr::summarise(nr=dplyr::n()) %>% dplyr::select(-nr),
                         by = c("Cod Partener" = "Code")) 
      
      baza_solduri <- baza_solduri %>% dplyr::mutate(Grup = ifelse(is.na(Grup), Beneficiar,Grup) )  %>%
        dplyr::left_join(y = solduri_provizioane, by = "Cod Partener") 
      
      baza_solduri <- baza_solduri %>% dplyr::mutate(dplyr::across(Provizion, ~ifelse(is.na(.x),0,.x)))
      
      vals$expuneri_surse_proprii <- baza_solduri %>% dplyr::filter(Tip_surse == "Surse_proprii")
      
      
      vals$top_expuneri_surse_proprii <- baza_solduri %>% dplyr::filter(Tip_surse == "Surse_proprii") %>% 
        dplyr::group_by(Grup) %>% 
        dplyr::summarise(Expunere=sum(`Soldul garantiei [in LEI]`) - sum(Provizion),
                         Pondere = sum(`Soldul garantiei [in LEI]`)/sum(baza_solduri$`Soldul garantiei [in LEI]`[
                           baza_solduri$Tip_surse == "Surse_proprii"])) %>% 
        dplyr::arrange(desc(Expunere)) %>%   as.data.frame() %>% 
        dplyr::slice(1:10) %>% janitor::adorn_totals(where = "row")
      
      vals$top_expuneri_fonduri_proprii <- baza_solduri %>% dplyr::group_by(Grup, Tip_surse) %>% 
        dplyr::summarise(Expunere=sum(`Soldul garantiei [in LEI]`) - sum(Provizion),
                         Pondere = sum(`Soldul garantiei [in LEI]`)/sum(baza_solduri$`Soldul garantiei [in LEI]`)) %>% 
        dplyr::arrange(desc(Expunere)) %>%   as.data.frame() %>% 
        dplyr::slice(1:10) %>% janitor::adorn_totals(where = "row")
      
      output$top_expuneri <- DT::renderDataTable(
        DT::datatable(data = vals$top_expuneri_fonduri_proprii,
                      options = list(dom = "Bt", pageLength = 11),rownames = TRUE,extensions = "Buttons",
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
            paste0("Top 10 Expuneri Brute - surse proprii si administrare la data de ", vals_expuneri$data_expuneri))) %>%
          DT::formatRound(columns = 3,digits = 0) %>% DT::formatPercentage(columns = 4,digits = 2)
      )
      
      
      output$top_expuneri_proprii <- DT::renderDataTable(
        DT::datatable(data = vals$top_expuneri_surse_proprii,
                      options = list(dom = "Bt", pageLength=11),rownames = TRUE,extensions = "Buttons",
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
            paste0("Top 10 expuneri Surse Proprii la data de ", vals_expuneri$data_expuneri))) %>%
          DT::formatRound(columns = 2,digits = 0) %>% DT::formatPercentage(columns = 3,digits = 2)
      )
      
      
    })
 
  })
}
    
## To be copied in the UI
# mod_top_expuneri_grupuri_ui("top_expuneri_grupuri_ui_1")
    
## To be copied in the server
# mod_top_expuneri_grupuri_server("top_expuneri_grupuri_ui_1")
