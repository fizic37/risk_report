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
    
   tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    mapare_rating <- readRDS("R/reactivedata/banci/mapare_rating.rds") %>% dplyr::mutate(id = paste0(Agentie,Ratings))
    
    vals_rating <- reactiveValues()
    
   
    
    #Observer to update only once data limite to vals$report_date
    observeEvent(vals$report_date, {
      
      updateDateInput(session = session,inputId = "data_limite",value = vals$report_date)  },once = TRUE)
    
    baza_date_limite_filtrata <- reactive( { req(vals$baza_date_rating)
      
      # I want to show the maximum available date for a bank limit
      vals$baza_date_rating %>% dplyr::left_join( by = "DenumireFinantator", y = vals$baza_date_rating %>% 
        dplyr::filter(DataInitiala <= input$data_limite) %>% 
          dplyr::group_by(DenumireFinantator) %>% dplyr::summarise(max_date = max(DataInitiala) ) ) %>%
        dplyr::filter(DataInitiala == max_date) %>% dplyr::select(-max_date) %>% 
        dplyr::arrange(Clasa_Risc, desc(Limita_Banca))
    })
    
    
    output$baza_date_limite <- DT::renderDataTable( { req( baza_date_limite_filtrata() )
      
      DT::datatable( data =  baza_date_limite_filtrata() %>% dplyr::select( DenumireFinantator,
                  Are_rating_extern, Clasa_Risc, Limita_Banca, DataInitiala) %>%
                    #dplyr::mutate(dplyr::across(.cols = DataInitiala,~as.character(.x))) %>%
                    dplyr::mutate(dplyr::across(.cols = where(is.character),  ~as.factor(.x))),
                     
                     options = list( scrollY = "300px", paging = FALSE, dom = "Bt",buttons = c("copy", "excel", "csv"),
                                     columnDefs = list(list(width = '50px', targets = 1:3),
                                                       list(width = '100px', targets = c(0,4)) ) ),
                     
                     extensions = "Buttons", filter = "top",rownames = FALSE, 
                     selection = list( mode = "single", selected = NULL, target = "row"),
                     caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', paste0( 
                       "Limitele de trezorerie valabile la data de ", input$data_limite, ". Click pe banca pentru a edita") )) %>%
        DT::formatRound(columns = 4, digits = 0)   })
    
    
    observeEvent( input$baza_date_limite_rows_selected, {
      req( !is.null(input$baza_date_limite_rows_selected) )
      
      vals_rating$banca_selectata <-  baza_date_limite_filtrata()  %>%  dplyr::slice(input$baza_date_limite_rows_selected)
      
      vals_rating$tabel_scor_financiar <- data.frame(  Solvabilitate = vals_rating$banca_selectata$Solvabilitate,
                                                       Rata_Credite_Neperf =  vals_rating$banca_selectata$Rata_Credite_Neperf,
                                                       Grad_Acoperie_Neperf = vals_rating$banca_selectata$Grad_Acoperie_Neperf,
                                                       ROE = vals_rating$banca_selectata$ROE,
                                                       Cost_Venit = vals_rating$banca_selectata$Cost_Venit,
                                                       Credite_Depozite = vals_rating$banca_selectata$Credite_Depozite,
                                                       Opinia_Auditorului = vals_rating$banca_selectata$Opinia_Auditorului,
                                                       Actionariat = vals_rating$banca_selectata$Actionariat,
                                                       row.names = vals_rating$banca_selectata$DenumireFinantator) %>% 
        dplyr::mutate(dplyr::across(.cols = dplyr::everything(),  ~ifelse(is.na(.x),0,.x)))
      
      vals_rating$scoring_final <-  ifelse( vals_rating$tabel_scor_financiar$Solvabilitate > 0.15,    8,
                                            ifelse(vals_rating$tabel_scor_financiar$Solvabilitate>=0.12,4,0)) + 
        ifelse(vals_rating$tabel_scor_financiar$Rata_Credite_Neperf<0.03,8,
               ifelse(vals_rating$tabel_scor_financiar$Rata_Credite_Neperf<=0.08,4,0)) + 
        ifelse(vals_rating$tabel_scor_financiar$Grad_Acoperie_Neperf>0.55,8,
               ifelse(vals_rating$tabel_scor_financiar$Grad_Acoperie_Neperf>=0.4,4,0)) + 
        ifelse(vals_rating$tabel_scor_financiar$ROE>0.1,8,
               ifelse(vals_rating$tabel_scor_financiar$ROE>=0.06,4,0)) + 
        ifelse(vals_rating$tabel_scor_financiar$Cost_Venit<0.5,8,
               ifelse(vals_rating$tabel_scor_financiar$Cost_Venit<=0.6,4,0)) + 
        ifelse(vals_rating$tabel_scor_financiar$Credite_Depozite<1,4,
               ifelse(vals_rating$tabel_scor_financiar$Credite_Depozite<=1.5,2,0)) + 
        vals_rating$tabel_scor_financiar$Opinia_Auditorului+vals_rating$tabel_scor_financiar$Actionariat
      
      vals_rating$pondere_clasa_risc <- ifelse( vals_rating$banca_selectata$Clasa_Risc == "A",
                                                '0.2',
                                                ifelse(
                                                  vals_rating$banca_selectata$Clasa_Risc == "B",
                                                  '0.15',
                                                  ifelse(
                                                    vals_rating$banca_selectata$Clasa_Risc == "C",
                                                    '0.1',
                                                    ifelse(vals_rating$banca_selectata$Clasa_Risc == "D", '0.05',
                                                           '0'))) )
     
      showModal(session = session, modalDialog(size = "l",
            title = paste0( "Editeaza banca ", vals_rating$banca_selectata$DenumireFinantator),
            footer = list( div(style = "padding-right: 100px; color: #ff007b; padding-top: 10px;",
            h6("Daca modifici Data Initiala si dai click save, voi salva o noua limita de trezorerie.
               Daca dai click update voi updata vechile informatii.") ),
            
            shinyjs::useShinyjs(),
            
            actionButton(ns("update_banca_rating"),label = "Update", icon = icon("edit")),
            actionButton(ns("save_banca_rating"), label = "Save", icon = icon("save") ),
            actionButton(ns("close_modal"),label = "Close",icon = icon("times"))
            ),
                                               
            fluidRow( column(width = 6,  shinyWidgets::pickerInput( ns("select_denumire_finantator_rating"),
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
                      tags$head(tags$style("#rating_update_ui_1-warning_resurse_financiare{color: #ff00fb;")),
                                                                
                      shinyWidgets::pickerInput( inputId = ns("pondere_banca"),
                         choices = c(0.2, 0.15, 0.1, 0.05),selected = vals_rating$pondere_clasa_risc,
                               label = 'Pondere clasa de risc' ),
                      
                      #shinyjs::disable(id = 'pondere_banca', asis = FALSE),
                                                                
                      
                      shinyWidgets::pickerInput( ns("select_clasa_risc_rating"),
                       label = "Clasa de Risc", choices = c("A", "B", "C", "D", "E"),
                        selected = vals_rating$banca_selectata$Clasa_Risc ),
                      verbatimTextOutput(ns("warning_select_clasa_risc_rating"))
                      ) ,
                                               
       column(width = 6, 
      shinyWidgets::airDatepickerInput( ns("data_initiala_rating"),
      minDate = as.Date("2021-12-13"),position = 'left top',
      label = "Data Initiala",language = "ro", autoClose = TRUE,value = vals_rating$banca_selectata$DataInitiala ),
        # dateInput(inputId = ns("data_initiala_rating"),label = "Data de incepere a limitei", autoclose = TRUE,
        #   min = as.Date("2021-12-13"),
        #   language = "ro", value=Sys.Date() ), #value = vals_rating$banca_selectata$DataInitiala),
                           
                     shinyWidgets::pickerInput( ns("are_rating"),
                         selected = vals_rating$banca_selectata$Are_rating_extern,
                            label = "Are rating extern", choices = c("DA", "NU") ),
             shinyWidgets::autonumericInput( ns("limita_trezorerie_rating"),
                                             align = "right", decimalPlaces = 0, label = "Limita trezorerie",
                                             value = vals_rating$banca_selectata$Limita_Banca),
             textOutput(ns("warning_limita_trezorerie")),
             tags$head(tags$style("#rating_update_ui_1-warning_limita_trezorerie{color: #ff00fb;")),                          
      
                                                
                        
      uiOutput(ns("show_ratings")),
                        uiOutput(ns("show_scor"))
                                               ),
                                               
                column(width = 12, rhandsontable::rHandsontableOutput(outputId = ns("tabel_scoring"))),
      
                                                     )
      ))
      
      #shinyjs::disable('select_clasa_risc_rating')
      
      vals_rating$clasa_finala <- input$select_clasa_risc_rating   
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE  )
    
    
    
    observeEvent(input$data_initiala_rating,{
      if ( input$data_initiala_rating != vals_rating$banca_selectata$DataInitiala ) {
        shinyjs::disable('update_banca_rating') 
        shinyjs::enable('save_banca_rating') }
      else if ( input$data_initiala_rating == vals_rating$banca_selectata$DataInitiala ) { 
        shinyjs::disable('save_banca_rating')
        shinyjs::enable('update_banca_rating') }
        
    })
   
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
        paste0("Resursele financiare salvate sunt - ", formatC(digits = 0,format = "d",big.mark = ",",
          vals_rating$banca_selectata$Resurse_financiare_totale),
               " lei si sunt diferite de cele afisate/calculate")
      }
    })
    
    # I show ratings UI (agentie and select rating) only when are_rating==DA
    output$show_ratings <- renderUI( { req(input$are_rating == "DA", vals_rating$banca_selectata)
      
      fluidRow(
        selectInput(inputId = session$ns("select_agentie"),label = "Agentia de rating",
                    choices = unique(mapare_rating$Agentie),
                    selected = vals_rating$banca_selectata$Agentie),
        
        selectInput(inputId = session$ns("select_rating"),label = "Rating-ul",
                    choices = unique(mapare_rating$Ratings), 
                    selected = vals_rating$banca_selectata$Rating_Extern )
        
      )
      
    })
    
   
    # Below observer updates select_rating in functie de agentia de rating (agentiile au ratinguri diferite)
    observeEvent( c(input$select_agentie,vals_rating$banca_selectata$Rating_Extern), { req(input$select_agentie)
      updateSelectInput(session = session,inputId = 'select_rating',
      choices = mapare_rating %>% dplyr::filter(Agentie == input$select_agentie) %>% dplyr::pull(Ratings),
      selected = vals_rating$banca_selectata$Rating_Extern )
    })
    
    # I show tabel pentru calcul scor final only when are_rating == NU
   
   
     output$tabel_scoring <-   rhandsontable::renderRHandsontable( {  
     req(input$are_rating == "NU", vals_rating$tabel_scor_financiar)
      
      rhandsontable::rhandsontable(
        data = vals_rating$tabel_scor_financiar,
        readOnly = FALSE, rowHeaderWidth = 100 ) %>% 
        rhandsontable::hot_cols(manualColumnResize = TRUE) %>%
        rhandsontable::hot_col(hot = ., col = 1:6, format = "0.00%") %>%
        rhandsontable::hot_col( col = 7:8,  type = "dropdown",
            source = c(8, 4, 0),strict = TRUE,  allowInvalid = FALSE ) })
    
    # I show scor final only when are_rating == NU
    output$show_scor <- renderUI( { req(input$are_rating == "NU")
      numericInput(inputId = ns("scor_final"),label = "Scor final",
                   value = vals_rating$scoring_final, min = 0, max = 60)
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
    observeEvent( input$select_rating,{ req(input$select_agentie)
      shinyWidgets::updatePickerInput(session = session,inputId = 'select_clasa_risc_rating', selected = 
          mapare_rating %>% dplyr::filter(Agentie == input$select_agentie, Ratings == input$select_rating) %>% 
                                        dplyr::pull(Clasa_Risc) %>% unique())
      
      vals_rating$clasa_finala  <- input$select_clasa_risc_rating
    })
    
    
    
    # I update pondere banca UI everytime input$select_clasa_risc changes. This input changes also through updateselectinput
    # which is activated when select_rating and when  inout$tabel_scoring.
    observeEvent( input$select_clasa_risc_rating,{
      vals_rating$pondere_clasa_risc <- ifelse( input$select_clasa_risc_rating == "A",
                                                '0.2',
                                                ifelse(
                                                  input$select_clasa_risc_rating == "B",
                                                  '0.15',
                                                  ifelse(
                                                    input$select_clasa_risc_rating == "C",
                                                    '0.1',
                                                    ifelse(input$select_clasa_risc_rating == "D", '0.05',
                                                           '0'))) )
       shinyWidgets::updatePickerInput(session = session,inputId = "pondere_banca",
         selected = vals_rating$pondere_clasa_risc )
          
       #mapare_rating %>% dplyr::filter(Clasa_Risc == input$select_clasa_risc_rating) %>%
              #                   dplyr::slice(1) %>% dplyr::pull(Pondere_Resurse)
      
     })
    
  
    
    limita_trezorerie <-  eventReactive( c(input$pondere_banca, input$resurse_financiare, 
                    input$select_clasa_risc_rating, input$active_banca),
        {
      new_limit <- min( 0.04*input$active_banca*1000, ifelse( input$select_clasa_risc_rating == "E",100000,
              input$resurse_financiare * as.numeric(input$pondere_banca)),na.rm = TRUE )
      return( round(new_limit/1000000,1) * 1000000 )
      
      } )
    
   
    
    
    # I update limita trezorerie UI whenever I calculate it or update it inside vals_rating$limita_trezorerie
    observeEvent( limita_trezorerie(),{
      shinyWidgets::updateAutonumericInput(session = session,inputId = 'limita_trezorerie_rating',
                                           value = round( limita_trezorerie()/1000000,1)*1000000)
    })
    
    observeEvent(input$save_banca_rating,{ vals_rating$save_banca_rating <- input$save_banca_rating })
    
    
     observeEvent( vals_rating$save_banca_rating,{
      shinyWidgets::ask_confirmation(ns("confirm_save"),
              title = ifelse(input$data_initiala_rating == vals_rating$banca_selectata$DataInitiala,
                      "Confirmi modificarile?", "Confirmi noua limita de trezorerie?"),
              text = ifelse(input$data_initiala_rating == vals_rating$banca_selectata$DataInitiala,
                     "Esti sigur ca vrei sa salvezi modificarile efectuate?",
                     "Esti sigur ca vrei sa salvezi noua limita de trezorerie?"),
                 btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
      vals_rating$save_banca_rating <- NULL
          })
    
    observeEvent( input$confirm_save,{
      vals_rating$confirm_save <- input$confirm_save
    } )
    
    observeEvent(input$update_banca_rating,{
      shinyWidgets::ask_confirmation(ns("confirm_update"), title = 'CONFIRM',
              text = "Esti sigur ca vrei sa salvezi modificarile efectuate?",
    btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
      
         })
    
    observeEvent( input$confirm_update,{
      vals_rating$confirm_update <- input$confirm_update
    })
    
    # Here I only save baza date limite vechi and calculate baza date rating
    observeEvent( c(vals_rating$confirm_save,vals_rating$confirm_update), {  
      req( any(vals_rating$confirm_save == TRUE, vals_rating$confirm_update == TRUE), limita_trezorerie() )
    
      removeModal(session = session)
      
      if ( input$are_rating == "NU" ) {
        
        tryCatch(expr = {
        vals_rating$df_new_scoring <- data.frame(row.names = NULL,stringsAsFactors = FALSE,check.names = FALSE,
          CodFinantator = vals_rating$banca_selectata$CodFinantator,
          DenumireFinantator = input$select_denumire_finantator_rating,
          DataInitiala = input$data_initiala_rating,
          Are_rating_extern = input$are_rating,
          Active = as.numeric(input$active_banca),
          Punctaj_Final = vals_rating$scoring_final,
          Clasa_Risc = input$select_clasa_risc_rating,
          Resurse_financiare_totale = as.numeric(input$resurse_financiare),
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
          Are_rating_extern = input$are_rating,
          Agentie = input$select_agentie,
          Rating_Extern = input$select_rating,
          Clasa_Risc = input$select_clasa_risc_rating,
          Resurse_financiare_totale = as.numeric(input$resurse_financiare),
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
      
      vals_rating$confirm_save <- NULL
      vals_rating$confirm_update <- NULL
      
    })
    
    
    observeEvent( vals_rating$finalise_process_compare_df,{ req(vals_rating$finalise_process_compare_df == TRUE )
     
      #if ( input$data_initiala_rating > vals_rating$banca_selectata$DataInitiala) {
        
        vals_rating$baza_date_rating <-  vals_rating$df_new_prel %>% dplyr::select(-id) %>%
          magrittr::set_rownames(value = NULL)
      #} else { vals_rating$baza_date_rating <-  vals_rating$df_new_prel %>% dplyr::select(-id) %>% magrittr::set_rownames(value = NULL)  }
      
     
      
      saveRDS(object = vals_rating$baza_date_rating, file = "R/reactivedata/banci/baza_date_rating.rds" )
      
      vals$baza_date_rating <- vals_rating$baza_date_rating
      
      vals_rating$finalise_process_compare_df <- NULL
      
      vals_rating$element_id <- NULL
      
      vals_rating$banca_selectata <- NULL
      vals_rating$clasa_finala <- NULL
      vals_rating$tabel_scoring <- NULL
      vals_rating$tabel_scor_financiar <- NULL
      vals_rating$scoring_final <- NULL
      
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
          .options = list("timeOut"=2000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      
    })
    
    observeEvent(input$close_modal,{
      removeModal(session)
      shinyjs::reset("are_rating")
      vals_rating$update_banca_rating <- NULL
      vals_rating$tabel_scoring <- NULL
      vals_rating$tabel_scor_financiar <- NULL
      vals_rating$scoring_final <- NULL
    })
    
 
  })
}
    
## To be copied in the UI
# mod_rating_update_ui("rating_update_ui_1")
    
## To be copied in the server
# mod_rating_update_server("rating_update_ui_1")
