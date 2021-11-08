#' rating UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rating_ui <- function(id){
  ns <- NS(id)
  bs4Dash::tabsetPanel(id = ns("rating"),selected = TRUE,
        shiny::tabPanel(title = "New rating", icon = icon("folder-open"), br(), 
  fluidPage(
    
    fluidRow(
      shinyFeedback::useShinyFeedback(),
      column(width = 4, selectInput(inputId = ns("select_generare_rating"),label = "Load existent rating - by date",
                              choices = c())),
      column(width = 4, dateInput(inputId = ns("data_rating"),label = "Data de intrare in vigoare a rating-ului",
                            value = as.Date("2021-10-01"))),
      column(width = 4, shinyWidgets::autonumericInput(inputId = ns("resurse_financiare"),
                        label = "Resursele financiare ale Fondului",value = 1188888160,align = "right",decimalPlaces = 0,
                        digitGroupSeparator = ",")),
      column(width = 2, selectInput(inputId = ns("select_bank"),label = "Selecteaza banca",
                            choices = c())),
      column(width = 2, shinyWidgets::autonumericInput(inputId = ns("active_banca"),
              label = "Active banca, mii lei", value=0,align = "right",decimalPlaces = 0)),
      column(width = 2, shinyWidgets::prettyRadioButtons(inputId = ns("are_rating"),label = "Are rating extern?",
                                inline = TRUE,choices = c("NU","DA"),status = "primary")),
      column(width = 6, uiOutput(ns("show_ratings"))),
      
      column(width = 12, uiOutput(ns("show_scoring"))),
      
      hr(),
      
      column(width = 12, rhandsontable::rHandsontableOutput(outputId = ns("tabel_scoring"))),
      
      column(width = 12, uiOutput(ns("show_scor_intern")))
    )
  )     ),
  shiny::tabPanel(title = "Rating results", icon = icon("align-center"), 
      tagList(bs4Dash::box(title = "Banci cu Rating extern",icon = icon("star"),width = 12,
                               status = "primary",collapsible = TRUE,collapsed = TRUE,
                DT::dataTableOutput(ns("external_ratings"))),
              bs4Dash::box(title = "Banci fara rating extern",icon = icon("calculator"),width = 12,
                           status = "primary",collapsible = TRUE,collapsed = TRUE,
                DT::dataTableOutput(ns("internal_scoring")))
      )
  )
  )
}
    
#' rating Server Functions
#'
#' @noRd 
mod_rating_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    lista_banci <- readRDS("R/reactivedata/banci/lista_banci.rds") %>% dplyr::pull(BANCA_CHARISMA)
    mapare_rating <- readRDS("R/reactivedata/banci/mapare_rating.rds")
    baza_date_rating <- readRDS("R/reactivedata/banci/baza_date_rating.rds")
    vals_rating <- reactiveValues(baza_date_rating = baza_date_rating)
   
     updateSelectInput(session = session,inputId = "select_bank",choices = lista_banci)
     
     output$external_ratings <- DT::renderDataTable( {req(vals_rating$baza_date_rating)
       DT::datatable(data = vals_rating$baza_date_rating %>% dplyr::filter(Are_rating_extern == "DA") %>%
            dplyr::select(data_rating, Banca,Agentie,Rating_Extern,Clasa_Risc, Limita_Banca,Active),
      rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                 'Banci cu rating extern:'),options = list(dom = "Btp", buttons=c("copy","csv","excel")),
      extensions = "Buttons") %>%     DT::formatRound(columns = 6:7,digits = 0) } )
     
     output$internal_scoring <- DT::renderDataTable( {req(vals_rating$baza_date_rating)
       DT::datatable(data = vals_rating$baza_date_rating %>% dplyr::filter(Are_rating_extern == "NU") %>%
          dplyr::select(data_rating, Banca,Punctaj_Final,Clasa_Risc, Limita_Banca,Active,
            Solvabilitate, Rata_Credite_Neperf, Grad_Acoperie_Neperf,   ROE, Cost_Venit,
            Credite_Depozite, Opinia_Auditorului, Actionariat),
        rownames = FALSE, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                  'Banci fara rating extern:'),options = list(dom = "Btp", buttons = c("copy","csv","excel"),
                                                              pageLength=10),
        extensions = "Buttons") %>%   DT::formatRound(columns = 5:6,digits = 0) %>% 
         DT::formatPercentage(columns = 7:12,digits = 2) } )
     
     
     # It does not work
     observeEvent(input$active_banca,{
       if(input$active_banca <= 0) {
       shinyFeedback::showFeedbackWarning(inputId = ns("active_banca"),session = session,
          icon = icon("exclamation-triangle"),text = "Active>0",color = "#ff007b") }
       else { shinyFeedback::hideFeedback(inputId = ns("active_banca") )}
     })
     
    observeEvent(input$are_rating, { req(input$are_rating=="DA") 
      
      output$show_ratings <- renderUI( {req(input$are_rating=="DA")  
       fluidRow(column(width = 3,
        selectInput(inputId = session$ns("select_agentie"),label = "Agentia de rating",
                  choices = unique(mapare_rating$Agentie))),
        column(width = 3,
        selectInput(inputId = session$ns("select_rating"),label = "Rating-ul",
                    choices = c()))
      ) } )
      
      observeEvent(input$select_agentie, {
        updateSelectInput(session = session,inputId = 'select_rating',choices = mapare_rating %>% 
                            dplyr::filter(Agentie == input$select_agentie) %>% dplyr::pull(Ratings) %>% unique())
       })
      
      observeEvent(input$select_rating,{
        output$show_scoring <- renderUI( { req(input$are_rating=="DA")
          fluidRow(column(width = 3, selectInput(inputId = session$ns("clasa_risc"),
              label = "Clasa de risc rezultata este:",choices = mapare_rating %>% 
                dplyr::filter(Agentie == input$select_agentie, Ratings == input$select_rating) %>% 
                dplyr::pull(Clasa_Risc) %>% unique())),
              column(width = 3, selectInput(inputId = session$ns("pondere_resurse"),
                label = "Pondere din resursele financiare:",choices = mapare_rating %>% 
                dplyr::filter(Agentie == input$select_agentie, Ratings == input$select_rating) %>% 
                dplyr::pull(Pondere_Resurse) %>% unique())),
              column(width = 3, shinyWidgets::autonumericInput(inputId = session$ns("limita_banca"),
                "Limita de expunere pe banca:",decimalPlaces = 0,value =min(round(0.04*input$active_banca*1000/100000,0)*100000,
                                  round(mapare_rating %>% 
                  dplyr::filter(Agentie == input$select_agentie, Ratings == input$select_rating) %>% 
                  dplyr::pull(Pondere_Resurse) %>% unique() * input$resurse_financiare/100000,0)*100000))),
              column(width = 3, br(), shinyWidgets::actionBttn(inputId = session$ns("save_rating"),
                  label = paste0("Save ",input$select_bank),icon = icon("save"),style = "stretch",color = "primary") )
          )       })
      })
      
    })
        
        
   output$tabel_scoring <- rhandsontable::renderRHandsontable( {req(input$are_rating=="NU")
      rhandsontable::rhandsontable(data = data.frame(Solvabilitate=0,Rata_Credite_Neperf=0.1,Grad_Acoperie_Neperf=0,
         ROE=0,Cost_Venit=1,Credite_Depozite=2,Opinia_Auditorului=8,Actionariat=0, 
         row.names = input$select_bank),readOnly = FALSE,rowHeaderWidth = 100) %>%
         rhandsontable::hot_col(hot = .,col = 1:6,format = "0.00%") %>%
          rhandsontable::hot_col(col = 7:8,type = "dropdown",source = c(8,4,0),strict = TRUE,allowInvalid = FALSE)
    })
   
   output$show_scor_intern <- renderUI( { req(input$are_rating=="NU") 
     tagList(br(),
     fluidRow(column(width = 3, selectInput(session$ns("punctaj_final"),label = "Punctaj final", choices = c(8))),
              column(width = 3, selectInput(session$ns("clasa_finala"),label = "Clasa de Risc", choices = c("E"))),
              column(width = 3, shinyWidgets::autonumericInput(inputId = session$ns("limita_finala"),
                          label = "Limita trezorerie",value = 100000,decimalPlaces = 0)),
              column(width = 3, br(),shinyWidgets::actionBttn(inputId = session$ns("save_scoring"),
                  label = paste0("Save ",input$select_bank),icon = icon("save"),style = "stretch",color = "primary"))
                     
              ) )
     })
   
   observeEvent(input$tabel_scoring,{
     vals_rating$tabel_scoring <- rhandsontable::hot_to_r(input$tabel_scoring)
     
     vals_rating$scoring_final <-  ifelse( vals_rating$tabel_scoring$Solvabilitate > 0.15,
         8,    ifelse(vals_rating$tabel_scoring$Solvabilitate>=0.12,4,0)) + 
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
     
     vals_rating$clasa_finala <- ifelse(vals_rating$scoring_final >= 48,"A",ifelse(vals_rating$scoring_final >= 40,"B",
                      ifelse(vals_rating$scoring_final >= 32,"C",ifelse(vals_rating$scoring_final >= 20,"D","E"))))
     
     vals_rating$limita_calculata <- min(0.04*input$active_banca*1000,
     (mapare_rating$Pondere_Resurse[match(vals_rating$clasa_finala,table = mapare_rating$Clasa_Risc)] %>% 
        unique())*input$resurse_financiare)
     
     # Handles clasa E. Clasa E is not available on the above mapare_rating, so it is identified with is.na
     vals_rating$limita_finala <- ifelse(is.na(vals_rating$limita_calculata), 100000,
                                        round(vals_rating$limita_calculata/100000,0)*100000)
     
      updateSelectInput(session = session,inputId = 'punctaj_final',choices = vals_rating$scoring_final)
     shinyjs::disable(id = 'punctaj_final', asis = FALSE)
     
     updateSelectInput(session = session,inputId = 'clasa_finala',choices =  vals_rating$clasa_finala)
     shinyjs::disable(id = 'clasa_finala', asis = FALSE)
     
     shinyWidgets::updateAutonumericInput(session = session,inputId = 'limita_finala',value = vals_rating$limita_finala)
     
     
     })
    
    observeEvent(input$save_rating, {
      if(input$active_banca>0) {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save"),title = "Confirm?",type = "info",
        text = "Esti sigur ca vrei sa salvezi datele completate?",
          btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84")) }
      else { shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",btn_colors = "#ff007b",
                      text = "Nu ai completat activele bancii!")}
    })
    
    observeEvent(input$save_scoring, {
      if(input$active_banca>0) {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_scoring"),title = "Confirm?",type = "info",
                                     text = "Esti sigur ca vrei sa salvezi datele completate?",
                                     btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84")) }
     
       else { shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",btn_colors = "#ff007b",
                                          text = "Nu ai completat activele bancii!")}
    })
    
    observeEvent(input$confirm_scoring,{ req(input$confirm_scoring == TRUE)
      vals_rating$df_new_scoring <- data.frame(data_rating = input$data_rating,
                Banca = input$select_bank, Are_rating_extern = input$are_rating,Active = input$active_banca,
                Punctaj_Final = vals_rating$scoring_final,Clasa_Risc = vals_rating$clasa_finala,
                Limita_Banca=vals_rating$limita_finala) %>% cbind(vals_rating$tabel_scoring)
      
      vals_rating$baza_date_rating <- dplyr::bind_rows(vals_rating$baza_date_rating %>% 
                                                         # I first check to see if the bank already exists for data_rating==input$data_rating
                                                         dplyr::mutate(already_exists = ifelse(data_rating==input$data_rating & Banca==input$select_bank,1,0)) %>% 
                                                         dplyr::filter(already_exists==0) %>% dplyr::select(-already_exists),
                                                       vals_rating$df_new_scoring)
      
      saveRDS(object = vals_rating$baza_date_rating, file = "R/reactivedata/banci/baza_date_rating.rds")
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                               .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      
      shinyjs::disable(id = "save_scoring",asis = FALSE)
      
      shinyWidgets::updateAutonumericInput(session = session, value=0,inputId = "active_banca")
    })
    
    observeEvent(input$confirm_save,{ req(input$confirm_save == TRUE)
    
    vals_rating$df_new <- data.frame(data_rating = input$data_rating,
              Banca = input$select_bank, Are_rating_extern = input$are_rating,Agentie=input$select_agentie,
              Rating_Extern = input$select_rating, Clasa_Risc=input$clasa_risc, Limita_Banca=input$limita_banca,
              Active = input$active_banca)
     
     vals_rating$baza_date_rating <- dplyr::bind_rows(vals_rating$baza_date_rating %>% 
       # I first check to see if the bank already exists for data_rating==input$data_rating
       dplyr::mutate(already_exists = ifelse(data_rating==input$data_rating & Banca==input$select_bank,1,0)) %>% 
       dplyr::filter(already_exists==0) %>% dplyr::select(-already_exists),
       vals_rating$df_new)
     
       saveRDS(object = vals_rating$baza_date_rating, file = "R/reactivedata/banci/baza_date_rating.rds")
    
    shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
        .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    
    shinyjs::disable(id = "save_rating",asis = FALSE)
      })
    
   
 
  })
}
    
## To be copied in the UI
# mod_rating_ui("rating_ui_1")
    
## To be copied in the server
# mod_rating_server("rating_ui_1")
