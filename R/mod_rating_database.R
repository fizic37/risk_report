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
    
    baza_date_limite <- readRDS("R/reactivedata/banci/sinteza_limite.rds")
    
    tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    vals_rating <- reactiveValues( baza_date_limite = baza_date_limite )
    
    observeEvent(vals$report_date, {
    
      updateDateInput(session = session,inputId = "data_limite",value = vals$report_date)  },once = TRUE)
    
   
    
    observeEvent(vals_rating$baza_date_limite,{
    
      vals_rating$baza_date_limite_filtrata <- vals_rating$baza_date_limite %>% 
      dplyr::filter(DataInitiala <= input$data_limite, DataExpirare >= input$data_limite) %>%
        dplyr::arrange(desc(LimitaTrezorerie),DenumireFinantator)  
      
      output$baza_date_limite <-
        DT::renderDataTable(
          DT::datatable(
            data =  vals_rating$baza_date_limite_filtrata,
            selection = list(
              mode = "single",
              selected = NULL,
              target = "row"
            ),
            extensions = "Buttons",
            rownames = FALSE,
            options = list(
              dom = "Bftip",
              paging = FALSE,
              scrollY = "300px",
              buttons = c("copy", "excel")
            ),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left;',
              paste0(
                "Limitele de trezorerie valabile la data de ",
                input$data_limite,
                ". Click pe banca pentru a edita"
              )
            )
          ) %>%
            DT::formatRound(columns = 4, digits = 0)
        )
      
      output$down_limite <- downloadHandler(filename = function() { paste0("limite_trezorerie_", input$data_limite,".csv") },
              content = function(file) { readr::write_csv(x = vals_rating$baza_date_limite_filtrata,file = file) } )
      
      })
    
    
    observeEvent(input$baza_date_limite_rows_selected,{
     
      
      vals_rating$banca_selectata <-  vals_rating$baza_date_limite_filtrata  %>%   
                                dplyr::slice( input$baza_date_limite_rows_selected )
      
      showModal(session = session,
                modalDialog( size = "l",
                  title = paste( "Editeaza banca ", vals_rating$banca_selectata$DenumireFinantator ),
                  footer = list( actionButton( inputId =  ns("save_banca"),
                      label = "Save",  icon = icon("save") ),
                    modalButton(label = "Close", icon = icon("times"))  ),
                  fluidRow(
                    column( width = 6,
                      shinyWidgets::pickerInput(
                        ns("select_cod_finantator"),
                        label = "Cod Finantator", options = list('actions-box' = TRUE),
                        choices = tabela_nume_banci$CodFinantator,
                        selected = vals_rating$banca_selectata$CodFinantator
                      ),
                      shinyWidgets::pickerInput(ns("select_clasa_risc"),label = "Clasa de Risc",
                                                choices = vals_rating$baza_date_limite$ClasaRisc %>% unique(),
                                                selected = vals_rating$banca_selectata$ClasaRisc),
                      shinyWidgets::airDatepickerInput(ns("data_initiala"),label = "Data Initiala",
                                        value = vals_rating$banca_selectata$DataInitiala,language = "ro")
                    ),
                    column( width = 6,
                      shinyWidgets::pickerInput(
                        ns("select_denumire_finantator"),
                        label = "Denumire Finantator", options = list('actions-box' = TRUE),
                        choices = tabela_nume_banci$DenumireFinantator,
                        selected = vals_rating$banca_selectata$DenumireFinantator
                      ),
                      shinyWidgets::autonumericInput(ns("limita_trezorerie"),align = "right",decimalPlaces = 0,
                                    label = "Limita trezorerie",value = vals_rating$banca_selectata$LimitaTrezorerie),
                      shinyWidgets::airDatepickerInput(ns("data_expirare"),label = "Data Expirare",
                                                       value = vals_rating$banca_selectata$DataExpirare,language = "ro")
                    )
                  )
                ))
      
      
    })
    
    observeEvent( input$select_denumire_finantator,{
      shinyWidgets::updatePickerInput(session = session,inputId = "select_cod_finantator",
          selected = tabela_nume_banci %>% dplyr::filter(DenumireFinantator == input$select_denumire_finantator) %>% 
            dplyr::pull(CodFinantator)) })
    
    observeEvent( input$select_cod_finantator,{
      shinyWidgets::updatePickerInput(session = session,inputId = "select_denumire_finantator",
        selected = tabela_nume_banci %>% dplyr::filter(CodFinantator == input$select_cod_finantator) %>% 
                                        dplyr::pull(DenumireFinantator)) })
    
    observeEvent(input$save_banca,{
      shinyWidgets::ask_confirmation(ns("confirm_save"),title = "CONFIRM?",
              text = "Esti sigur ca vrei sa salvezi modificarile efectuate?",
              btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
    })
    
    observeEvent(input$confirm_save, { req(input$confirm_save == TRUE)
      removeModal(session = session)
      
      vals_rating$new_bank <- data.frame(CodFinantator = input$select_cod_finantator, 
            DenumireFinantator = input$select_denumire_finantator, ClasaRisc = input$select_clasa_risc,
            LimitaTrezorerie = as.numeric( input$limita_trezorerie ), DataInitiala = input$data_initiala,
            DataExpirare = input$data_expirare, PlafonProcentual = vals_rating$banca_selectata$PlafonProcentual)
      
      if (janitor::compare_df_cols_same(vals_rating$new_bank, vals_rating$baza_date_limite)) {
        
         vals_rating$baza_date_limite <-  dplyr::bind_rows( vals_rating$new_bank,
           vals_rating$baza_date_limite %>% dplyr::filter(CodFinantator != vals_rating$banca_selectata$CodFinantator) )
         
         saveRDS(object =  vals_rating$baza_date_limite, file = "R/reactivedata/banci/sinteza_limite.rds")
         
         shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
              .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
         
      }
      
      else { shinyFeedback::showToast( type = "error",message = "Could not save to database. Contact the administrator!",
                                    title = "ERROR",keepVisible = TRUE ) }
      
     
      
    })
 
  })
}
    
## To be copied in the UI
# mod_rating_database_ui("rating_database_ui_1")
    
## To be copied in the server
# mod_rating_database_server("rating_database_ui_1")
