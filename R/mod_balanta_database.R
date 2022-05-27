#' balanta_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_balanta_database_ui <- function(id){
  # Handles Database plasamente box from Plasamente tabpanel menu
  ns <- NS(id)
  
  fluidPage( br(),
             shinyFeedback::useShinyFeedback(),
  fluidRow(
    column( width = 3,
            shinyWidgets::airDatepickerInput(inputId = ns("date_baza_balanta"), 
                    value = as.Date("1999-01-01"),language = "ro",autoClose = TRUE,
                      label = "Selecteaza data raportului"), br() ),
                
    
    column(width = 3,   selectInput(ns("tip_sursa"),label = "Selecteaza tipul sursei",
                                    choices =  c()) ),
    
    column(width = 3,   selectInput(ns("tip_plasament"),label = "Selecteaza tipul plasamentului",
                                    choices = c()) ),
    
    column(width = 3,   shinyWidgets::pickerInput(ns("contrapartida"),label = "Selecteaza Contrapartida",
                                                  choices =  c(),options = list('live-search' = TRUE))  )
    
  ),
    
   DT::dataTableOutput(ns("baza_date_balanta")),
  
  br(),
  
  shinyWidgets::actionBttn(inputId = ns("sterge_observatia"),label = "Sterge data selectata",
                        icon = icon("trash-alt"),style = "stretch",color = "danger")
  )
  
}
    
#' balanta_database Server Functions
#'
#' @noRd 
mod_balanta_database_server <- function(id, vals, vals_balanta){
  # It is called in app_server.R with vals argument (to be used for vals$report_date for initialization of date_baza_balanta)
  #   and vals_balanta which can update also inside mod_plasamente.R
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tabela_banci <- readRDS("R/reactivedata/banci/tabela_nume_banci.rds")
    
   
    shinyWidgets::updateAirDateInput( session = session,inputId = "date_baza_balanta",
                                           value = max( vals$report_date ) )
    
    observeEvent(input$date_baza_balanta, { req( input$date_baza_balanta != "1999-01-01" )
      
      vals_balanta$tabel_nume_banci <- tabela_banci %>% dplyr::filter(DataInitiala <= input$date_baza_balanta & 
                                            DataExpirare >= input$date_baza_balanta) %>% dplyr::select(2:3)
      
      vals_balanta$filtered_database <- vals_balanta$balanta_database %>%
        dplyr::left_join( vals_balanta$tabel_nume_banci, by = c("Banca" = "CodFinantator")) %>% 
        dplyr::mutate(DenumireFinantator = ifelse(is.na(DenumireFinantator),Banca,DenumireFinantator)) %>%
        dplyr::filter(data_balanta==input$date_baza_balanta) 
      
      
      })
    
    # Once I produce filtered database I updated the rest of my UI selection. This way, selectInput
    # will only display available data for the selected date
    observeEvent( vals_balanta$filtered_database, { 
    
    shinyWidgets::updatePickerInput(session = session, "contrapartida",
                      choices = c("all",unique(  vals_balanta$filtered_database$DenumireFinantator ) ) )
    
    updateSelectInput(session = session,inputId = "tip_sursa", 
                      choices = c("all",unique( vals_balanta$filtered_database$tip_sursa )))
    
    updateSelectInput(session = session,inputId = "tip_plasament",
                      choices = c("all",unique( vals_balanta$filtered_database$tip_plasament )))
  })
  
 
    # Final table needs filtered database and updated selectInput
      tabel_final <- reactive ( { req( vals_balanta$filtered_database, !is.null(input$contrapartida),
                                  input$tip_plasament !="", input$tip_sursa != "" )
     vals_balanta$filtered_database %>% 
      dplyr::filter(if (input$tip_sursa=="all") TRUE else tip_sursa==input$tip_sursa) %>%
      dplyr::filter(if (input$tip_plasament=="all") TRUE else tip_plasament==input$tip_plasament) %>%
      dplyr::filter(if (input$contrapartida=="all") TRUE else DenumireFinantator==input$contrapartida) %>%
      dplyr::select(-tip_sursa, -Banca) %>%
      dplyr::group_by(DenumireFinantator, tip_plasament) %>%
      tidyr::pivot_wider(names_from = tip_plasament,
                         values_from =  `Solduri finale|Debit`,values_fill = 0 ) %>% 
      dplyr::select(-`Simbol cont`,- `Denumire cont`,-data_balanta) %>%
      dplyr::summarise_all(.funs = ~sum(.)) %>%
      dplyr::mutate(Expunere_totala = rowSums(dplyr::select_if(.,is.numeric)))
    
  })
     
       
      output$baza_date_balanta <- DT::renderDataTable({ req( tabel_final() )
        DT::datatable(data =  tabel_final() %>%
                        janitor::adorn_totals(where = "row",fill = "-") %>%
                        dplyr::arrange(desc(Expunere_totala)), rownames = FALSE,
                      options = list(dom = "Bt", buttons = c("copy","csv","excel"), paging=FALSE,scrollY = "300px"), 
                      extensions = "Buttons", caption = htmltools::tags$caption(
                        style = 'caption-side: top; text-align: left;',"Expunerea pe contrapartide") ) %>%
          DT::formatRound(columns = 2:ncol( tabel_final() ),digits = 0) 
          # It does not work. I am guessing it does not receive a datatable after it was formatted Round
          #DT::formatStyle(table = .,columns = 0,target = "row",fontWeight = DT::styleEqual(1, "bold")
           })
      
      observeEvent( input$sterge_observatia,{
        shinyWidgets::ask_confirmation(inputId = ns("confirm_delete"),title = "CONFIRM",
              text = paste0("Esti sigur ca vrei sa stergi toate plasamentele din data de ", input$date_baza_balanta),
              btn_labels = c("NU, renunta","OK, sterge"),btn_colors = c("#ff007b","#00ff84"), type = "warning")
        
      })
      
      
      observeEvent( input$confirm_delete, {req(input$confirm_delete == TRUE )
        vals_balanta$balanta_database <-  vals_balanta$balanta_database %>% 
              dplyr::filter( data_balanta != input$date_baza_balanta)
        
        saveRDS(object =  vals_balanta$balanta_database,file = "R/reactivedata/balanta/balanta_database.rds")
        
        shinyFeedback::showToast(type = "success",title = "SUCCES",keepVisible = TRUE,
                    message = "Successfully deleted")
        
        
        })
      
    })
    
    
 
  
}
    
## To be copied in the UI
# mod_balanta_database_ui("balanta_database_ui_1")
    
## To be copied in the server
# mod_balanta_database_server("balanta_database_ui_1")
