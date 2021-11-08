#' garantii_manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_garantii_manual_ui <- function(id){
  ns <- NS(id)
  bs4Dash::box(title = "Completeaza manual garantiile de stat", width = 12, id = ns("box_manual_solduri"),
               icon = icon("keyboard"), collapsible = T,collapsed = T, maximizable = T,
               footer = "Atentie, nu sunt disponibile pentru a fi selectate decat datele soldurilor uploadate anterior.",
  fluidPage(br(),
  fluidRow(
    hr(),
    column(width = 12,h4("Se completeaza manual datele Prima Casa, OUG37 si Investeste in tine.")),
    br(), br(), hr(),
    column(width = 4, selectInput(inputId = ns("select_program"),
                                         label = "Selecteaza programul",
                                         choices = c("Prima Casa","Investeste in tine", "OUG37"))),
           column(width = 4, selectInput(inputId = ns("data_solduri_manuale"),
                                         label = "Selecteaza data soldurilor",choices = c()) ),
           column(width = 4, br(), 
                  actionButton(inputId = ns("save_solduri_manuale"), icon = icon("save"),
                                label = "Salveaza datele introduse mai jos")),
           column(width = 3, shinyWidgets::autonumericInput(inputId = ns("sold_garantii_input"),
                                  label = "Soldul garantiilor, lei", value = 0,align = "right",
                                  digitGroupSeparator = ",",decimalPlaces = 2,minimumValue = ,
                                  decimalCharacter = "."  )),
           column(width = 3, shinyWidgets::autonumericInput(ns("nr_contracte_input"),
                                  digitGroupSeparator = ",",decimalPlaces = 0, value = 0,align = "right",
                                  label = "Numarul de contracte")),
           column(width = 3, shinyWidgets::autonumericInput(ns("nr_beneficiari_input"),
                                  label = "Numarul de beneficiari", decimalPlaces = 0, value = 0,align = "right")),
           column(width = 3, shinyWidgets::autonumericInput(inputId = ns("sold_credit_input"),
                                  label = "Soldul creditelor, lei", value = 0,align = "right",
                                  digitGroupSeparator = ",",decimalPlaces = 2,minimumValue = 0,decimalCharacter = "."))
  )  )  
  )
}
    
#' garantii_manual Server Functions
#'
#' @noRd 
mod_garantii_manual_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(vals$view_baza_solduri,{
    
    updateSelectInput(inputId = 'data_solduri_manuale', session = session,
                      choices = vals$view_baza_solduri$data_raport %>% unique() )
    
    shinyjs::disable(id = "save_solduri_manuale", asis = FALSE)
    })
    
    observe({req(input$sold_garantii_input >0, input$nr_contracte_input>0,
                 input$nr_beneficiari_input >0,input$sold_credit_input >0)
      shinyjs::enable(id = "save_solduri_manuale", asis = FALSE)
    })
    
    observeEvent(input$save_solduri_manuale,{
      df_manual <- reactive({
        data.frame(Tip_surse = "Nume_cont_stat", "Tip fonduri" = input$select_program,check.names = FALSE,
                   Nr_contracte = input$nr_contracte_input, Nr_beneficiari = input$nr_beneficiari_input,
                   Sold_garantii = input$sold_garantii_input, 
                   data_raport = as.Date.character(input$data_solduri_manuale),
                   Sold_credite_garantate = input$sold_credit_input)   })
      
      if (janitor::compare_df_cols_same(df_manual(),vals$view_baza_solduri)) {
        
        vals$view_baza_solduri <- dplyr::bind_rows(df_manual(),vals$view_baza_solduri)
        
        saveRDS(object = vals$view_baza_solduri, file = "R/reactivedata/view_baza_sold.rds")
        
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
        
        shinyWidgets::updateAutonumericInput(session = session,inputId = "sold_garantii_input",value = 0)
        shinyWidgets::updateAutonumericInput(session = session,inputId = "nr_contracte_input", value=0)
        shinyWidgets::updateAutonumericInput(session = session,inputId = "nr_beneficiari_input", value=0)
        shinyWidgets::updateAutonumericInput(session = session,inputId = "sold_credit_input", value=0)
        
        
      }
      
      else {shinyFeedback::showToast(type = "error",title = "ERROR",message = "Failed to save", keepVisible = F) }
      
    })
    
    
    
    
 
  })
}
    
## To be copied in the UI
# mod_garantii_manual_ui("garantii_manual_ui_1")
    
## To be copied in the server
# mod_garantii_manual_server("garantii_manual_ui_1")
