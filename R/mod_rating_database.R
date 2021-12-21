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
    
#' rating_database Server Functions
#'
#' @noRd 
mod_rating_database_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    baza_date_limite_vechi <- readRDS("R/reactivedata/banci/sinteza_limite.rds")
    
    baza_date_rating <- readRDS("R/reactivedata/banci/baza_date_rating.rds")
    
    tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    mapare_rating <- readRDS("R/reactivedata/banci/mapare_rating.rds")
    
    vals_rating <- reactiveValues( baza_date_limite_vechi = baza_date_limite_vechi,
                                   baza_date_rating = baza_date_rating )
    
    observeEvent(vals$report_date, {
    
      updateDateInput(session = session,inputId = "data_limite",value = vals$report_date)  },once = TRUE)
    
    # Tabelul din baza de date se modifica de fiecare data cand se modificat input$date sau se actualizeaza
    # vals_ating_baza_limite_vechi sau vals_rating_baza_date_rating
    to_listen_baza_date <- reactive({
      list(input$data_limite, vals_rating$baza_date_limite_vechi, vals_rating$baza_date_rating)
    })  
    
     observeEvent( to_listen_baza_date(),{ 
      
      if ( input$data_limite < as.Date("2021-12-13") ){
        
        vals_rating$baza_date_limite_filtrata <- vals_rating$baza_date_limite_vechi %>% 
            dplyr::filter(DataInitiala <= input$data_limite, DataExpirare >= input$data_limite) %>%
              dplyr::arrange(desc(LimitaTrezorerie),DenumireFinantator)
        
        output$baza_date_limite <- DT::renderDataTable( { req( vals_rating$baza_date_limite_filtrata )
          if ( input$data_limite < as.Date("2021-12-13") ) {
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
        
       
      }
      
      else if ( input$data_limite >= as.Date("2021-12-13") ) {
        vals_rating$baza_date_limite_filtrata <- vals_rating$baza_date_rating %>% 
          dplyr::filter(DataInitiala <= input$data_limite, DataExpirare >= input$data_limite) %>% 
            dplyr::arrange(Clasa_Risc, desc(Limita_Banca))
        }
     
    })
    
   
     } else if ( input$data_limite >= as.Date("2021-12-13") ) {
       
       vals_rating$baza_date_limite_filtrata <- vals_rating$baza_date_rating %>% 
         dplyr::filter(DataInitiala <= input$data_limite, DataExpirare >= input$data_limite) %>%
         dplyr::arrange(desc(Limita_Banca),DenumireFinantator)
       
       output$baza_date_limite <-  DT::renderDataTable( {req(vals_rating$baza_date_limite_filtrata)
         DT::datatable(
         data =  vals_rating$baza_date_limite_filtrata %>%
           dplyr::select(
             CodFinantator,
             Are_rating_extern,
             Clasa_Risc,
             Limita_Banca,
             DataInitiala,
             DataExpirare
           ) %>%
           dplyr::mutate(dplyr::across(.cols = where(is.character),  ~
                                         as.factor(.x))),
         rownames = FALSE,
         options = list(
           scrollY = "300px",
           paging = FALSE,
           dom = "Bt",
           buttons = c("copy", "excel", "csv")
         ),
         extensions = "Buttons",
         filter = "top",
         selection = list(
           mode = "single",
           selected = NULL,
           target = "row"
         )
       ) %>%
         DT::formatRound(columns = 4, digits = 0)
       })
        
     }
     
   })
    
    
     
     to_listen_update <- reactive({ list( input$baza_date_limite_rows_selected,  input$new_bank )})
     
     # Main observer
    observeEvent( to_listen_update(), {  req( any( !is.null(to_listen_update()[[1]]),
                                             to_listen_update()[[2]] >0 ) )
               
     vals_rating$banca_selectata <-  vals_rating$baza_date_limite_filtrata  %>%   
          dplyr::slice( ifelse(is.null(input$baza_date_limite_rows_selected),1,
                               input$baza_date_limite_rows_selected))
      
      
      showModal(session = session, modalDialog(size = "l",
              title = paste0("Editeaza banca ", vals_rating$banca_selectata$CodFinantator),
              
              footer = list( actionButton( inputId =  ns("save_banca_rating"),
                                           label = "Save",  icon = icon("save") ),
                             modalButton(label = "Close", icon = icon("times"))  ),
              fluidRow(
                column( width = 6,
                        shinyWidgets::pickerInput(
                          ns("select_cod_finantator_rating"),
                          label = "Cod Finantator", options = list(`live-search` = TRUE),
                          choices = tabela_nume_banci$CodFinantator %>% unique() ,
                          selected = vals_rating$banca_selectata$CodFinantator ),
                        
                        
                        shinyWidgets::airDatepickerInput(ns("data_initiala_rating"),label = "Data Initiala",
                                value = vals_rating$banca_selectata$DataInitiala,language = "ro"),
                        
                        shinyWidgets::autonumericInput(inputId = ns("resurse_financiare"),label = "Resursele financiare",
                                                       align = "right",decimalPlaces = 0,value = 1191129218),
                        
                        shinyWidgets::pickerInput(inputId = ns("pondere_banca"),choices = c(0.2,0.15,0.1,0.05,100000),
                                                  label = 'Pondere clasa de risc'),
                        
                        shinyjs::disable(id = 'pondere_banca',asis = FALSE),
                        
                        shinyWidgets::autonumericInput(ns("limita_trezorerie_rating"),align = "right",decimalPlaces = 0,
                                label = "Limita trezorerie",value = vals_rating$banca_selectata$Limita_Banca)
                        ) ,
                
                column( width = 6,
                        shinyWidgets::pickerInput(
                          ns("select_denumire_finantator_rating"),
                          label = "Denumire Finantator", options = list(`live-search` = TRUE),
                          choices = tabela_nume_banci$DenumireFinantator %>% unique(),
                          selected = vals_rating$banca_selectata$DenumireFinantator),
                        
                        shinyWidgets::airDatepickerInput(ns("data_expirare_rating"),label = "Data Expirare",
                                      value = vals_rating$banca_selectata$DataExpirare,language = "ro"),
                        
                        shinyWidgets::autonumericInput(ns("active_banca"),align = "right",decimalPlaces = 0,
                                label = "Active banca, mii lei",value = vals_rating$banca_selectata$Active),
                        
                        shinyWidgets::pickerInput(ns("select_clasa_risc_rating"),label = "Clasa de Risc",
                                                  choices = c("A","B","C","D","E"),
                                                  selected = vals_rating$banca_selectata$Clasa_Risc),
                        
                        
                        if ( input$data_limite >=  as.Date("2021-12-13") | input$new_bank > 0 ) {
                          shinyWidgets::pickerInput(inputId = ns("are_rating"),
                                                    selected = vals_rating$banca_selectata$Are_rating_extern,
                                                    label = "Are rating extern",choices = c("DA","NU"))
                        },
                        
                        uiOutput(ns("show_ratings")),
                        uiOutput(ns("show_scor"))
                ),
              
              column(width = 12, rhandsontable::rHandsontableOutput(outputId = ns("tabel_scoring")))
              )  )  )
              
      
    }, ignoreInit = TRUE,ignoreNULL = TRUE,)
    
    # I show ratings UI (agentie and select rating) only wher are_rating==DA
    output$show_ratings <- renderUI( { req(input$are_rating == "DA")
      fluidRow(
        selectInput(inputId = session$ns("select_agentie"),label = "Agentia de rating",
                                  choices = unique(mapare_rating$Agentie),
                    selected = vals_rating$banca_selectata$Agentie %>% unique() ),
        
        selectInput(inputId = session$ns("select_rating"),label = "Rating-ul",
                                  choices = unique(mapare_rating$Ratings),
                    selected = vals_rating$banca_selectata$Rating_Extern %>% unique() ) 
        )       })
    
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
            row.names = input$select_cod_finantator_rating) %>% dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                                                      ~ifelse(is.na(.x),0,.x))),
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
      
      vals_rating$clasa_finala <-   ifelse(   vals_rating$scoring_final >= 48,    "A",
          ifelse(   vals_rating$scoring_final >= 40,    "B",
            ifelse(  vals_rating$scoring_final >= 32,   "C",
              ifelse(vals_rating$scoring_final >= 20, "D", "E")      )   )  )
      
      updateNumericInput(session = session,inputId = "scor_final",value = vals_rating$scoring_final)
      
      shinyWidgets::updatePickerInput(session = session,inputId = 'select_clasa_risc_rating',
                                      selected = vals_rating$clasa_finala)
      
    })
     
    # I update available ratings UI whenever agentie changes 
    observeEvent(input$select_agentie, {
      updateSelectInput(session = session,inputId = 'select_rating',choices = mapare_rating %>% 
                          dplyr::filter(Agentie == input$select_agentie) %>% dplyr::pull(Ratings) %>% unique())
    })
    
    # I update select clasa risc UI whenever select clasa risc rating changes 
    observeEvent(input$select_rating,{
      shinyWidgets::updatePickerInput(session = session,inputId = 'select_clasa_risc_rating', selected = 
                  mapare_rating %>% dplyr::filter(Agentie == input$select_agentie, Ratings == input$select_rating) %>% 
                                        dplyr::pull(Clasa_Risc) %>% unique())
    })
    
    # I update selected CodFnantator UI whenever I select another denumireFinantator
    observeEvent( input$select_denumire_finantator_rating,{
      shinyWidgets::updatePickerInput(session = session,inputId = "select_cod_finantator_rating",
          choices = c(tabela_nume_banci %>% dplyr::filter(DenumireFinantator == input$select_denumire_finantator_rating) %>% 
            dplyr::pull(CodFinantator), "showAll") )
      
     })
    
    observeEvent( input$select_cod_finantator_rating,{ req( input$select_cod_finantator_rating == "showAll")
      shinyWidgets::updatePickerInput(session = session,inputId = "select_cod_finantator_rating",
                                      choices = unique(tabela_nume_banci$CodFinantator)) } )
    
    # I update pondere banca UI everytime input$select_clasa_risc changes. This input changes also through updateselectinput
    # which is activated when select_rating and when  inout$tabel_scoring.
    observeEvent( input$select_clasa_risc_rating,{
      
      shinyWidgets::updatePickerInput(session = session,inputId = "pondere_banca",
        selected = mapare_rating %>% dplyr::filter(Clasa_Risc == input$select_clasa_risc_rating) %>%
          dplyr::slice(1) %>% dplyr::pull(Pondere_Resurse))
       
     
      })
    
    # I define to_listen() a multiple event in order to calculate vals_rating$limita_trezorerie
    to_listen <- reactive({
      list(input$select_cod_finantator_rating, input$pondere_banca, input$resurse_financiare)
    })
    
    # Calculate  vals_rating$limita_trezorerie
    observeEvent(to_listen(),{
      vals_rating$limita_trezorerie <-  min( 0.04*input$active_banca*1000, 
                  ifelse( input$select_clasa_risc_rating == "E",100000,
                     input$resurse_financiare * as.numeric(input$pondere_banca)),na.rm = TRUE )
    })
    
    # I update limita trezorerie UI whenever I calculate it or update it inside vals_rating$limita_trezorerie
    observeEvent( vals_rating$limita_trezorerie,{
      shinyWidgets::updateAutonumericInput(session = session,inputId = 'limita_trezorerie_rating',
                                           value = round(vals_rating$limita_trezorerie/1000000,1)*1000000)
    })
    
    observeEvent(input$save_banca_rating,{
      shinyWidgets::ask_confirmation(ns("confirm_save"),title = "CONFIRM?",
              text = "Esti sigur ca vrei sa salvezi modificarile efectuate?",
              btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
    })
    
    # Here I only save baza date limite vechi and calculate baza date rating
    observeEvent(input$confirm_save, {  req(input$confirm_save == TRUE)
      removeModal(session = session)
      
      if ( input$data_limite < as.Date("2021-12-13") ) {
      
        vals_rating$new_bank <- data.frame(CodFinantator = input$select_cod_finantator_rating, 
            DenumireFinantator = input$select_denumire_finantator_rating, ClasaRisc = input$select_clasa_risc,
            LimitaTrezorerie = as.numeric( input$limita_trezorerie ), DataInitiala = input$data_initiala,
            DataExpirare = input$data_expirare, PlafonProcentual = vals_rating$banca_selectata$PlafonProcentual)
      
      if (janitor::compare_df_cols_same(vals_rating$new_bank, vals_rating$baza_date_limite_vechi)) {
        
         vals_rating$baza_date_limite_vechi <-  dplyr::bind_rows( vals_rating$new_bank,
              vals_rating$baza_date_limite_vechi %>%
                  dplyr::filter(CodFinantator != vals_rating$banca_selectata$CodFinantator) )
         
         saveRDS(object =  vals_rating$baza_date_limite_vechi, file = "R/reactivedata/banci/sinteza_limite.rds")
         
         shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
              .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
         
      }
      
      else { shinyFeedback::showToast( type = "error",message = "Could not save to database. Contact the administrator!",
                                    title = "ERROR",keepVisible = TRUE ) }
      
      }
      
      else {
        if ( input$are_rating == "NU" ) {
         
          vals_rating$df_new_scoring <- data.frame(
            CodFinantator = input$select_cod_finantator_rating,
            DenumireFinantator = input$select_denumire_finantator_rating,
            DataInitiala = input$data_initiala_rating,
              DataExpirare = input$data_expirare_rating,
            Are_rating_extern = input$are_rating,
            Active = input$active_banca,
            Punctaj_Final = vals_rating$scoring_final,
            Clasa_Risc = vals_rating$clasa_finala,
            Limita_Banca = vals_rating$limita_trezorerie ) %>% cbind(vals_rating$tabel_scoring)
          
          
        }  else if ( input$are_rating == "DA" ) {
          vals_rating$df_new_scoring <- data.frame(
            CodFinantator = input$select_cod_finantator_rating,
            DenumireFinantator = input$select_denumire_finantator_rating,
            DataInitiala = input$data_initiala_rating,
            DataExpirare = input$data_expirare_rating,
            Are_rating_extern = input$are_rating,
            Agentie = input$select_agentie,
            Rating_Extern = input$select_rating,
            Clasa_Risc = input$clasa_finala,
            Limita_Banca = vals_rating$limita_trezorerie ) %>% cbind(vals_rating$tabel_scoring)
        }
        
      }
      
     
      
     })
    
    # Observer to save modifications for baza date rating. vals_rating$df_new_scoring is reactive only
    # when saving is finalised.
    
    observeEvent( vals_rating$df_new_scoring, {
      
      if (janitor::compare_df_cols_same( vals_rating$df_new_scoring, vals_rating$baza_date_rating, 
                          bind_method = "bind_rows" )) {
        
       vals_rating$baza_date_rating <-  dplyr::bind_rows( vals_rating$df_new_scoring,
              vals_rating$baza_date_rating %>% dplyr::filter(CodFinantator != vals_rating$banca_selectata$CodFinantator) )
        
        saveRDS(object =  vals_rating$baza_date_rating, file = "R/reactivedata/banci/baza_date_rating.rds")
        
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
          .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
        
      }  else { shinyFeedback::showToast( type = "error",message = "Could not save to database. Contact the administrator!",
                                       title = "ERROR",keepVisible = TRUE ) }
      
    })
 
  })
}
    
## To be copied in the UI
# mod_rating_database_ui("rating_database_ui_1")
    
## To be copied in the server
# mod_rating_database_server("rating_database_ui_1")
