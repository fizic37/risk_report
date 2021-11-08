#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_ui <- function(id){
  ns <- NS(id)
  bs4Dash::tabsetPanel(id = ns("admin"),  selected = T,  
  shiny::tabPanel(title = "Banci",value = "banci", icon = icon("university"), 
                  shinyFeedback::useShinyFeedback(),
      tagList( br(),
            bs4Dash::box(title = "Gestiune fuziuni banci",status = "primary",collapsible = T,collapsed = T,maximizable = T,
                         footer = "Click pe o banca din tabel si editeaza data si noul finantator cu care a fuzionat",width = 12,
                  tagList(  DT::dataTableOutput(ns("lista_banci")))
  )
  )
  )
  )
}
    
#' admin Server Functions
#'
#' @noRd 
mod_admin_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    vals_admin <- reactiveValues(tabela_nume_banci = tabela_nume_banci)
    
    observeEvent(input$admin,{ vals$admin_selected_tab <- input$admin  } )
    
   
    output$lista_banci <- DT::renderDataTable( DT::datatable(data = vals_admin$tabela_nume_banci,
      selection = list(mode = "single",selected = NULL, target = "row"),rownames = FALSE,
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                    "Lista bancilor")) )
  
    observeEvent(input$lista_banci_rows_selected,{
      
      vals_admin$banca_selectata <- vals_admin$tabela_nume_banci  %>%   dplyr::slice(input$lista_banci_rows_selected)
      
      showModal(session = session,modalDialog(title = paste("Editeaza banca ",vals_admin$banca_selectata$DenumireFinantator),
                                              size = "l",   
          footer = list(actionButton(inputId =  ns("save_banca"),label = "Save", icon = icon("save")),
                                                            modalButton(label = "Close",icon = icon("times")) ),
          fluidRow(column(width = 6, shinyWidgets::airDatepickerInput(inputId = session$ns("data_fuziune"),
                                  value =  Sys.Date(), language = "ro",autoClose = TRUE,
                                   label = "Selecteaza data de fuziune a bancii") ),
                  column(width = 6, shinyWidgets::pickerInput(inputId = session$ns("banca_absorbanta"),
                                  choices = unique(vals_admin$tabela_nume_banci$DenumireFinantator), options = list(`live-search` = TRUE),
                                  label = "Selecteaza noul finantator absorbant") )
          )
      ) )
      
      
    })
    
    observeEvent(input$save_banca, {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_banca"),title = 'CONFIRM',
                                     btn_colors = c("#ff007b","#00ff84"), type = "success",
                                     text = "Esti sigur ca vrei sa salvezi modificarile efectuate?") 
    })
    
    observeEvent(input$confirm_banca,{req(input$confirm_banca == TRUE ) 
      vals_admin$df_new <- data.frame(FinantatorID = vals_admin$banca_selectata$FinantatorID,
                                      CodFinantator = vals_admin$banca_selectata$CodFinantator,
                                      DenumireFinantator = input$banca_absorbanta,
                                      DataInitiala = input$data_fuziune,
                                      DataExpirare = as.Date("2100-01-01")   )
      
      vals_admin$tabela_nume_banci$DataExpirare[vals_admin$tabela_nume_banci$FinantatorID==vals_admin$banca_selectata$FinantatorID] <- input$data_fuziune
      
      vals_admin$tabela_nume_banci <- dplyr::bind_rows(vals_admin$tabela_nume_banci, vals_admin$df_new)
      
      saveRDS(object = vals_admin$tabela_nume_banci,file = "R/reactivedata/banci/tabela_nume_banci.rds")
      
      removeModal(session = session)
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Successfully saved",
                               .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
      
      })
    
    
    
  })
}
    
## To be copied in the UI
# mod_admin_ui("admin_ui_1")
    
## To be copied in the server
# mod_admin_server("admin_ui_1")
