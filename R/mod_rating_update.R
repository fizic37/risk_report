#' rating_update UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rating_update_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    
    shinyjs::useShinyjs(),
    
    fluidRow(
      column(width = 4,
             dateInput(inputId = ns("data_limite"),label = "Data la care voi afisa limitele in vigoare",
                       min = as.Date("2020-08-28"),value = Sys.Date(),language = "ro",autoclose = TRUE),
             hr() ),
      
      column(width = 12,DT::dataTableOutput(outputId = ns("baza_date_limite")), br()),
      
      
      # actionButton(inputId = ns("new_bank"),label = "Introdu o banca noua",icon = icon("plus-square"))
      shinyWidgets::actionBttn(inputId = ns("new_bank"),style = "stretch",color = "primary",
                               label = "Introdu o banca noua",icon = icon("plus-square"))
    )
  )
}
    
#' rating_update Server Functions
#'
#' @noRd 
mod_rating_update_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
   #baza_date_rating <- readRDS( "R/reactivedata/banci/baza_date_rating.rds" )
    
    tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    mapare_rating <- readRDS("R/reactivedata/banci/mapare_rating.rds") %>% dplyr::mutate(id = paste0(Agentie,Ratings))
    
    vals_rating <- reactiveValues()
    
    #Observer to update only once data limite to vals$report_date
    observeEvent(vals$report_date, {
      
      updateDateInput(session = session,inputId = "data_limite",value = vals$report_date)  },once = TRUE)
    
    baza_date_limite_filtrata <- reactive( { req(vals$baza_date_rating)
      
      vals$baza_date_rating %>% 
        dplyr::filter(DataInitiala <= input$data_limite, DataExpirare >= input$data_limite) %>% 
        dplyr::arrange(Clasa_Risc, desc(Limita_Banca))
    })
    
    
    output$baza_date_limite <- DT::renderDataTable( { req( baza_date_limite_filtrata() )
      DT::datatable( data =  baza_date_limite_filtrata() %>% dplyr::select( DenumireFinantator,
                  Are_rating_extern, Clasa_Risc, Limita_Banca, DataInitiala, DataExpirare) %>%
                       dplyr::mutate(dplyr::across(.cols = where(is.character),  ~as.factor(.x))),
                     
                     options = list( scrollY = "300px", paging = FALSE, dom = "Bt",buttons = c("copy", "excel", "csv") ),
                     
                     extensions = "Buttons", filter = "top",rownames = FALSE, 
                     selection = list( mode = "single", selected = NULL, target = "row"),
                     caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', paste0( 
                       "Limitele de trezorerie valabile la data de ", input$data_limite, ". Click pe banca pentru a edita") )) %>%
        DT::formatRound(columns = 4, digits = 0)   })
    
    
    observeEvent( input$baza_date_limite_rows_selected, {
      req( !is.null(input$baza_date_limite_rows_selected) )
      
      vals_rating$banca_selectata <-  baza_date_limite_filtrata()  %>%  dplyr::slice(input$baza_date_limite_rows_selected)
      
      
      
      showModal(session = session, modalDialog(size = "l",
            title = paste0( "Editeaza banca ", vals_rating$banca_selectata$DenumireFinantator),
            footer = list( div(style = "padding-right: 100px; color: #ff007b; padding-top: 10px;",
            h6("Daca modifici Data Initiala, voi salva o noua limita de trezorerie") ),
            actionButton(ns("save_banca_rating"), label = "Save", icon = icon("save") ),
            modalButton(label = "Close", icon = icon("times"))     ),
                                               
            fluidRow( column(width = 6,   shinyWidgets::pickerInput( ns("select_denumire_finantator_rating"),
                      label = "Denumire Finantator", options = list(`live-search` = TRUE),
                      choices = tabela_nume_banci$DenumireFinantator %>% unique(),
                      selected = vals_rating$banca_selectata$DenumireFinantator),
                      shinyWidgets::autonumericInput( ns("active_banca"),
                        align = "right",decimalPlaces = 0, label = "Active banca, mii lei",
                                  value = vals_rating$banca_selectata$Active ), 
                    
                      shinyWidgets::autonumericInput( inputId = ns("resurse_financiare"),
                            label = "Resursele financiare", align = "right",
                                decimalPlaces = 0, value =  vals_rating$banca_selectata$Resurse_financiare_totale),
                      textOutput(ns("warning_resurse_financiare")),
                                                                
                      shinyWidgets::pickerInput( inputId = ns("pondere_banca"),
                         choices = c(0.2, 0.15, 0.1, 0.05),
                               label = 'Pondere clasa de risc' ),
                      
                      shinyjs::disable(id = 'pondere_banca', asis = FALSE),
                                                                
                      shinyWidgets::autonumericInput( ns("limita_trezorerie_rating"),
                        align = "right", decimalPlaces = 0, label = "Limita trezorerie",
                        value = vals_rating$banca_selectata$Limita_Banca),
                      textOutput(ns("warning_limita_trezorerie")),
                      tags$head(tags$style("#rating_update_ui_1-warning_limita_trezorerie{color: #ff00fb;")),
                                                                
                      shinyWidgets::pickerInput( ns("select_clasa_risc_rating"),
                       label = "Clasa de Risc", choices = c("A", "B", "C", "D", "E"),
                        selected = vals_rating$banca_selectata$Clasa_Risc ),
                      verbatimTextOutput(ns("warning_select_clasa_risc_rating"))
                      ) ,
                                               
      column(width = 6, shinyWidgets::airDatepickerInput( ns("data_initiala_rating"),
                       minDate = vals_rating$banca_selectata$DataInitiala,
                       label = "Data Initiala",language = "ro", autoClose = TRUE,
                         value = vals_rating$banca_selectata$DataInitiala ), 
        
                           shinyWidgets::airDatepickerInput( ns("data_expirare_rating"),
                                  label = "Data Expirare", language = "ro", autoClose = TRUE,
                                         value = vals_rating$banca_selectata$DataExpirare ),
                                              
                     shinyWidgets::pickerInput( ns("are_rating"),
                         selected = vals_rating$banca_selectata$Are_rating_extern,
                            label = "Are rating extern", choices = c("DA", "NU") ),
                                                      
                                                     
                                                     uiOutput(ns("show_ratings")),
                                                      uiOutput(ns("show_scor"))
                                               ),
                                               
                                               column(width = 12, rhandsontable::rHandsontableOutput(outputId = ns("tabel_scoring")))
                                               )
      ))
      
      #shinyjs::disable('select_clasa_risc_rating')
      
      vals_rating$clasa_finala <- input$select_clasa_risc_rating   
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE  )
    
    
    output$warning_select_clasa_risc_rating <- renderText({ 
      req(input$select_clasa_risc_rating,vals_rating$banca_selectata)
      
      shinyFeedback::feedbackWarning(inputId = "select_clasa_risc_rating",color = "#ff00fb",
      text = paste0("Clasa de risc salvata este - ", vals_rating$banca_selectata$Clasa_Risc,
                    " si este diferita de cea afisata/calculata"),
      show = input$select_clasa_risc_rating != vals_rating$banca_selectata$Clasa_Risc)
      
    })
    
    output$warning_limita_trezorerie <- renderText({  # shinyFeedback does not updates autonumericinput
      req(input$limita_trezorerie_rating,vals_rating$banca_selectata)
      
      if (input$limita_trezorerie_rating != vals_rating$banca_selectata$Limita_Banca) {
        paste0("Limita de trezorerie salvata este - ", round(vals_rating$banca_selectata$Limita_Banca/1000000,1),
               " mil lei si este diferita de cea afisata/calculata")
      }
      
      
    })
    
    output$warning_resurse_financiare <- renderText({
      req(input$resurse_financiare,vals_rating$banca_selectata)
      
      if (input$resurse_financiare != vals_rating$banca_selectata$Resurse_financiare_totale) {
        paste0("Resursele financiare salvate sunt - ", vals_rating$banca_selectata$Resurse_financiare_totale,
               " si sunt diferite de cele afisate/calculate")
      }
    })
    
    # I show ratings UI (agentie and select rating) only when are_rating==DA
    output$show_ratings <- renderUI( { req(input$are_rating == "DA", vals_rating$banca_selectata)
      
      fluidRow(
        selectInput(inputId = session$ns("select_agentie"),label = "Agentia de rating",
                    choices = unique(mapare_rating$Agentie),
                    selected = vals_rating$banca_selectata$Agentie %>% unique() ),
        
        selectInput(inputId = session$ns("select_rating"),label = "Rating-ul",
                    choices = unique(mapare_rating$Ratings) )
        
      )
      
    })
    
   
    # Below observer updates select_rating in functie de agentia de rating (agentiile au ratinguri diferite)
    observeEvent(input$select_agentie,{
      updateSelectInput(session = session,inputId = 'select_rating',
                        choices = mapare_rating %>% dplyr::filter(Agentie == input$select_agentie) %>% dplyr::pull(Ratings),
                        selected = vals_rating$banca_selectata$Rating_Extern )
    })
    
    # I show tabel pentru calcul scor final only when are_rating == NU
    output$tabel_scoring <-   rhandsontable::renderRHandsontable( {  req(input$are_rating == "NU")
      
      rhandsontable::rhandsontable(
        data = data.frame(  Solvabilitate = vals_rating$banca_selectata$Solvabilitate,
                            Rata_Credite_Neperf =  vals_rating$banca_selectata$Rata_Credite_Neperf,
                            Grad_Acoperie_Neperf = vals_rating$banca_selectata$Grad_Acoperie_Neperf,
                            ROE = vals_rating$banca_selectata$ROE,
                            Cost_Venit = vals_rating$banca_selectata$Cost_Venit,
                            Credite_Depozite = vals_rating$banca_selectata$Credite_Depozite,
                            Opinia_Auditorului = vals_rating$banca_selectata$Opinia_Auditorului,
                            Actionariat = vals_rating$banca_selectata$Actionariat,
                            row.names = vals_rating$banca_selectata$DenumireFinantator) %>% 
          dplyr::mutate(dplyr::across(.cols = dplyr::everything(),  ~ifelse(is.na(.x),0,.x))),
        readOnly = FALSE, rowHeaderWidth = 100 ) %>% 
        rhandsontable::hot_col(hot = ., col = 1:6, format = "0.00%") %>%
        rhandsontable::hot_col( col = 7:8,  type = "dropdown",
                                source = c(8, 4, 0),strict = TRUE,  allowInvalid = FALSE ) })
    
    # I show scor final only when are_rating == NU
    output$show_scor <- renderUI( { req(input$are_rating == "NU")
      numericInput(inputId = ns("scor_final"),label = "Scor final",value = 0,min = 0,max = 60)
    })
    
    # I calculate scor final, I update scor final UI and select clasa risc ratin UI
    observeEvent( input$tabel_scoring,{
      vals_rating$tabel_scoring <- rhandsontable::hot_to_r(input$tabel_scoring)
      
      vals_rating$scoring_final <-  ifelse( vals_rating$tabel_scoring$Solvabilitate > 0.15,    8,
                                            ifelse(vals_rating$tabel_scoring$Solvabilitate>=0.12,4,0)) + 
        ifelse(vals_rating$tabel_scoring$Rata_Credite_Neperf<0.03,8,
               ifelse(vals_rating$tabel_scoring$Rata_Credite_Neperf<=0.08,4,0)) + 
        ifelse(vals_rating$tabel_scoring$Grad_Acoperie_Neperf>0.55,8,
               ifelse(vals_rating$tabel_scoring$Grad_Acoperie_Neperf>=0.4,4,0)) + 
        ifelse(vals_rating$tabel_scoring$ROE>0.1,8,
               ifelse(vals_rating$tabel_scoring$ROE>=0.06,4,0)) + 
        ifelse(vals_rating$tabel_scoring$Cost_Venit<0.5,8,
               ifelse(vals_rating$tabel_scoring$Cost_Venit<=0.6,4,0)) + 
        ifelse(vals_rating$tabel_scoring$Credite_Depozite<1,4,
               ifelse(vals_rating$tabel_scoring$Credite_Depozite<=1.5,2,0)) + 
        vals_rating$tabel_scoring$Opinia_Auditorului+vals_rating$tabel_scoring$Actionariat
      
      vals_rating$clasa_finala <-  ifelse( vals_rating$scoring_final >= 48,   "A",
                                           ifelse( vals_rating$scoring_final >= 40,  "B",
                                                   ifelse(  vals_rating$scoring_final >= 32, "C",
                                                            ifelse(vals_rating$scoring_final >= 20, "D", "E")  )  )  )
      
      updateNumericInput(session = session,inputId = "scor_final",value = vals_rating$scoring_final)
      
      shinyjs::disable("scor_final")
      
      shinyWidgets::updatePickerInput(session = session,inputId = 'select_clasa_risc_rating',
                                      selected = vals_rating$clasa_finala)
      
    })
    
    
    
    # I update select clasa risc UI whenever select clasa risc rating changes 
    observeEvent(input$select_rating,{
      shinyWidgets::updatePickerInput(session = session,inputId = 'select_clasa_risc_rating', selected = 
          mapare_rating %>% dplyr::filter(Agentie == input$select_agentie, Ratings == input$select_rating) %>% 
                                        dplyr::pull(Clasa_Risc) %>% unique())
      
      vals_rating$clasa_finala  <- input$select_clasa_risc_rating
    })
    
    
    
    # I update pondere banca UI everytime input$select_clasa_risc changes. This input changes also through updateselectinput
    # which is activated when select_rating and when  inout$tabel_scoring.
    observeEvent( input$select_clasa_risc_rating,{
      
      shinyWidgets::updatePickerInput(session = session,inputId = "pondere_banca",
                selected = mapare_rating %>% dplyr::filter(Clasa_Risc == input$select_clasa_risc_rating) %>%
                        dplyr::slice(1) %>% dplyr::pull(Pondere_Resurse))
      
      
    })
    
  
    
    limita_trezorerie <-  eventReactive( c(input$pondere_banca, input$resurse_financiare),{
      new_limit <- min( 0.04*input$active_banca*1000, ifelse( input$select_clasa_risc_rating == "E",100000,
              input$resurse_financiare * as.numeric(input$pondere_banca)),na.rm = TRUE )
      return( round(new_limit/1000000,1) * 1000000 )
      
      } )
    
    # I update limita trezorerie UI whenever I calculate it or update it inside vals_rating$limita_trezorerie
    observeEvent( limita_trezorerie(),{
      shinyWidgets::updateAutonumericInput(session = session,inputId = 'limita_trezorerie_rating',
                                           value = round( limita_trezorerie()/1000000,1)*1000000)
    })
    
    observeEvent(input$save_banca_rating,{
      shinyWidgets::ask_confirmation(ns("confirm_save"),
                                     title = ifelse(input$data_initiala_rating == vals_rating$banca_selectata$DataInitiala,
                                                    "Confirmi modificarile?", "Confirmi noua limita de trezorerie?"),
                                     text = ifelse(input$data_initiala_rating == vals_rating$banca_selectata$DataInitiala,
                                                   "Esti sigur ca vrei sa salvezi modificarile efectuate?",
                                                   "Esti sigur ca vrei sa salvezi noua limita de trezorerie?"),
                                     btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
    })
    
    # Here I only save baza date limite vechi and calculate baza date rating
    observeEvent(input$confirm_save, {  req(input$confirm_save == TRUE, limita_trezorerie() )
      removeModal(session = session)
      
      if ( input$are_rating == "NU" ) {
        
        tryCatch(expr = {
        vals_rating$df_new_scoring <- data.frame(row.names = NULL,stringsAsFactors = FALSE,check.names = FALSE,
          CodFinantator = vals_rating$banca_selectata$CodFinantator,
          DenumireFinantator = input$select_denumire_finantator_rating,
          DataInitiala = input$data_initiala_rating,
          DataExpirare = input$data_expirare_rating,
          Are_rating_extern = input$are_rating,
          Active = as.numeric(input$active_banca),
          Punctaj_Final = vals_rating$scoring_final,
          Clasa_Risc = input$select_clasa_risc_rating,
          Limita_Banca = limita_trezorerie() ) %>% cbind(vals_rating$tabel_scoring)
        }, error = function(e) {
          shiny:::reactiveStop(shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                 text = "Nu ai completat toate campurile. Refresh the app and start again") ) 
          
          }  ) 
        
      }  else if ( input$are_rating == "DA" ) {
        
        tryCatch(expr = {
        vals_rating$df_new_scoring <- data.frame(row.names = NULL,stringsAsFactors = FALSE,check.names = FALSE,
          CodFinantator = vals_rating$banca_selectata$CodFinantator,
          DenumireFinantator = input$select_denumire_finantator_rating,
          Active = as.numeric(input$active_banca),
          DataInitiala = input$data_initiala_rating,
          DataExpirare = input$data_expirare_rating,
          Are_rating_extern = input$are_rating,
          Agentie = input$select_agentie,
          Rating_Extern = input$select_rating,
          Clasa_Risc = input$select_clasa_risc_rating,
          Limita_Banca = limita_trezorerie() ) }, error = function(e) {
            shiny:::reactiveStop( shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                        text = "Nu ai completat toate campurile. Refresh the app and start again") )
            }  ) 
      }
      
      vals_rating$df_old <- vals$baza_date_rating %>% dplyr::mutate(id = paste0(CodFinantator,DataInitiala))
      vals_rating$df_new <- vals_rating$df_new_scoring %>% dplyr::mutate(id = paste0(CodFinantator,DataInitiala))
      vals_rating$element_id <- vals_rating$df_new$id
      vals_rating$column_id = "id"
      vals_rating$finalise_process_compare_df = FALSE
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = vals_rating, red="#ff007b",green="#00ff84")
      
      
    })
    
    observeEvent(vals_rating$finalise_process_compare_df,{ req(vals_rating$finalise_process_compare_df == TRUE )
      
      if ( input$data_initiala_rating > vals_rating$banca_selectata$DataInitiala) {
        # I replace Data Expirare of modified bank with the new DataInitiala minus 1
        vals_rating$baza_date_rating <-  vals_rating$df_new_prel %>% dplyr::mutate(
          DataExpirare = ifelse(  DenumireFinantator == input$select_denumire_finantator_rating &
                                    DataInitiala == vals_rating$banca_selectata$DataInitiala, input$data_initiala_rating - 1,
                                  DataExpirare)) %>% dplyr::mutate(dplyr::across(DataExpirare,  ~ as.Date.numeric(
                                    x = .x, origin = as.Date("1970-01-1") ))) %>% magrittr::set_rownames(value = NULL)
      } else { vals_rating$baza_date_rating <-  vals_rating$df_new_prel %>% magrittr::set_rownames(value = NULL)  }
      
      
      
      saveRDS(object = vals_rating$baza_date_rating, file = "R/reactivedata/banci/baza_date_rating.rds" )
      
      #vals$baza_date_rating <- vals_rating$baza_date_rating
      
      vals_rating$finalise_process_compare_df <- FALSE
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
            .options = list("timeOut"=2000, 'positionClass'="toast-top-full-width", "progressBar" = TRUE))
      
      shinyjs::refresh()
      
      
      
      
      
    
      
    })
    
 
  })
}
    
## To be copied in the UI
# mod_rating_update_ui("rating_update_ui_1")
    
## To be copied in the server
# mod_rating_update_server("rating_update_ui_1")
