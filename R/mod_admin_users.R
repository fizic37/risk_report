#' admin_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_users_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyFeedback::useShinyFeedback(),
    br(),
    column(width = 6,DT::DTOutput(ns("current_users"))),
    
    column(width = 6, actionButton(ns("new_user"),label = "New user", icon = icon("plus-square")))
  )
}
    
#' admin_users Server Functions
#'
#' @noRd 
mod_admin_users_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    credentials <- readRDS("R/credentials/credentials.rds")
    
    vals_credentials <- reactiveValues( credentials = credentials)
    
    output$current_users <- DT::renderDataTable( DT::datatable(data = vals_credentials$credentials %>%
    dplyr::select(-passod), rownames = FALSE,
      options = list( paging = FALSE, dom = "t", pageLength = 5), 
      selection = list( mode = "single", selected = NULL, target = "row"),
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
      "Current users. Click pe user pentru a modifica") ) )
    
    observeEvent(input$current_users_rows_selected,{
      
      vals_credentials$selected_user <- vals_credentials$credentials %>% dplyr::slice(input$current_users_rows_selected)
      
      showModal( modalDialog(title = paste0("Update user ", vals_credentials$selected_user$username_id), size="l",
                            footer = list(
                              h6("De aici poti doar sa generezi o parola noua sau sa modifici permisiunile de acces
                                 in aplicatie pentru userul selectat. Atentie, parola generata poate fi vazuta
                                 o singura si doar in aceasta fereastra."),
                              actionButton(ns("update_user"),label = "Update",icon = icon("edit")),
                                          actionButton(ns("close_modal"),"Close",icon = icon("times"))),
                            fluidRow(column(width = 6, selectInput(ns("user_type"),label = "User type",
                                                      choices = c("admin", "risk_user","guest"), 
                                                      selected = vals_credentials$selected_user$permission)),
                                     column(width = 6, br(),actionLink(ns("new_password"),
                                                            label = "Generate new password",icon = icon("key")),
                                     textOutput(ns("password_generated")))
                            )
                            )
      )
    })
    
   
    observeEvent(input$update_user,{
      shinyWidgets::ask_confirmation(ns("confirm_update"), title = 'CONFIRM',
            text = "Esti sigur ca vrei sa salvezi modificarile efectuate?",
      btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
      vals_credentials$confirm_update <- input$confirm_update
    })
    
    observeEvent(  vals_credentials$confirm_update, { req(   vals_credentials$confirm_update == TRUE )
      vals_credentials$updated_user <- data.frame(username_id =  vals_credentials$selected_user$username_id,
                passod   = sapply(vals_credentials$new_password,sodium::password_store),
                permission  = input$user_type, stringsAsFactors = F,row.names = NULL)
      
      vals_credentials$credentials <- dplyr::bind_rows(vals_credentials$updated_user, vals_credentials$credentials %>% 
                                  dplyr::filter(username_id != vals_credentials$selected_user$username_id))
      saveRDS(object = vals_credentials$credentials, file = "R/credentials/credentials.rds" )
      vals_credentials$confirm_update <- NULL
      removeModal(session)
        
        })
    
    
     output$password_generated <- renderText({ req(input$new_password)
      vals_credentials$new_password <- stringi::stri_rand_strings(n=1, length=12, pattern="[A-Za-z0-9]")
      vals_credentials$new_password
      })
    
    observeEvent(input$close_modal,{
      removeModal(session)
      vals_credentials$selected_user <- NULL
      
    })
    
    observeEvent(input$new_user,{
      
      showModal( modalDialog(title = "New User", size = "l",
                             footer = list(
                               h6("De aici poti  sa generezi un user nou. Atentie, parola generata poate fi vazuta
                                 o singura si doar in aceasta fereastra. Butonul save se va activa doar dupa ce
                                  vei genera parola"),
                               shinyjs::useShinyjs(),
                               actionButton(ns("save_new_user"),label = "Save",icon = icon("save")),
                               modalButton(icon = icon("times"),label = "Close") ),
        fluidRow(column(width = 6, 
                        textInput(ns("new_user_id"),label = "User ID"),
                        selectInput(ns("new_user_type"),label = "User type",
                    choices = c("admin", "risk_user","guest"), selected = "guest") ),
                                                                    
                column(width = 6, br(),actionLink(ns("new_user_password"),
                            label = "Generate new password",icon = icon("key")),
                            textOutput(ns("new_password_generated")))
                             )
      ) ) 
      
      shinyjs::disable("save_new_user")
      
    })
    
    output$new_password_generated <- renderText({ req(input$new_user_password)
      vals_credentials$new_user_password <- stringi::stri_rand_strings(n=1, length=12, pattern="[A-Za-z0-9]")
      shinyjs::enable("save_new_user")
      vals_credentials$new_user_password 
    })
    
    observeEvent(input$save_new_user,{
      shinyWidgets::ask_confirmation(ns("confirm_new_user"), title = 'CONFIRM',
            text = "Esti sigur ca vrei sa generezi un user nou?",
            btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
     })
    
    observeEvent(input$confirm_new_user,{
      vals_credentials$confirm_new_user <- input$confirm_new_user
    })
   
    
    observeEvent(vals_credentials$confirm_new_user, { req( vals_credentials$confirm_new_user == TRUE )
      req(vals_credentials$confirm_new_user == TRUE, vals_credentials$new_user_password) 
     
      
      vals_credentials$new_user <- data.frame(username_id =  input$new_user_id,
            passod   = sapply(vals_credentials$new_user_password,sodium::password_store),
            permission  = input$new_user_type, stringsAsFactors = F,row.names = NULL)
     
       if ( input$new_user_id %in% credentials$username_id ) {
        shinyFeedback::showToast(type = "error",title = "STOP", keepVisible = TRUE,
                message = "Acest User ID exista deja in baza de date" )
         
         removeModal(session)
                        
      } else if ( input$new_user_id == "" ) {
        shinyFeedback::showToast(type = "error",title = "STOP", keepVisible = TRUE,
                    message = "STOP, nu ai completat User Id")
        removeModal(session)
      }
      
      else {
      vals_credentials$credentials <- dplyr::bind_rows( vals_credentials$new_user, vals_credentials$credentials)
      saveRDS(object = vals_credentials$credentials,file = "R/credentials/credentials.rds" )
      vals_credentials$confirm_new_user <- NULL
      removeModal(session)
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
        .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      }
      
      })
    
    
 
  })
}
    
## To be copied in the UI
# mod_admin_users_ui("admin_users_ui_1")
    
## To be copied in the server
# mod_admin_users_server("admin_users_ui_1")
