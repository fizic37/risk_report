#' plasamente UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plasamente_ui <- function(id){
  ns <- NS(id)
  
  # This module handles UI calling for plasamente tab and processes Tabel 9 and Tabel 10.
  # Attention: I need boxes here in order to call specific modules when boxes are maximized or not collapsed
  tagList(
    shinybusy::add_busy_spinner(  color = "#ff007b",    position = "bottom-right",    timeout = 200    ),
    
    bs4Dash::box(title = "Database plasamente",collapsible = T,collapsed = T,maximizable = T,width = 12,
                 icon = icon("database"),gradient = T,background = "white",id = ns("box_database_plasamente"),
                 mod_balanta_database_ui("balanta_database_ui_1") ),
    
    bs4Dash::box(title = "Upload plasamente", collapsible = T,collapsed = T,maximizable = T,width = 12,
                 icon = icon("file-import"),id = ns("box_upload_plasamente"),
                 footer = "Se uploadeaza balanta de verificare care contine clasele de conturi 271 si 272. 
               Balanta se obtine din Charisma-Contabilitate-Rapoarte-Balanta de verificare-Balanta de verificare-
                 Balanta de verificare la inceput de an - Include NC de ajustare - Totaluri pe ultimul nivel afisat.",
            
                     mod_plasamente_upload_ui("plasamente_upload_ui_1")),
  
    bs4Dash::box(title = "Tabelul 9 - Evolutia resurselor financiare",icon=icon("table"),status = "primary",
    width = 12,collapsible = T,collapsed = FALSE,  DT::dataTableOutput(ns("tabel9")) ),
  
  
    bs4Dash::box(title = "Tabelul 10 - Evolutia distributiei surselor financiare proprii ale FNGCIMM", status = "primary",
  icon = icon("table"),collapsible = T, collapsed = FALSE, width = 12, DT::dataTableOutput(ns("tabel10")))
  
  )
  
  

}
    
#' plasamente Server Functions
#'
#' @noRd 
mod_plasamente_server <- function(id, vals, vals_balanta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
   
    observeEvent(input$box_database_plasamente, { req(any(input$box_database_plasamente$collapsed==FALSE, 
                                                         input$box_database_plasamente$maximized==TRUE))
      
      vals$box_selected <- c(vals$box_selected,"box_database_plasamente")
      
   })
    
    observeEvent(input$box_upload_plasamente, {req(any(input$box_upload_plasamente$collapsed==FALSE, 
                                                       input$box_upload_plasamente$maximized==TRUE))
     
      # bs4Dash::updateBox( id = "box_upload_plasamente",action = "update",session = session,
                      #    options = list(collapsible=FALSE,maximizable = FALSE ) )
      
      vals$box_selected <- c(vals$box_selected,"box_upload_plasamente")
      
    })
    
   
    observe( { vals_balanta$balanta_database <- readRDS("R/reactivedata/balanta/balanta_database.rds") } )
      
    to_listen <- reactive({ list(vals$report_date, vals_balanta$balanta_database) })
    
    observeEvent( to_listen(), { req( vals$previous_month, vals$previous_year ) 
      
      vals$tabel9 <-  tryCatch(expr = { vals_balanta$balanta_database %>% 
          dplyr::filter(data_balanta %in% c(vals$report_date, vals$previous_month, vals$previous_year)) %>%
      dplyr::group_by(data_balanta,tip_sursa) %>% dplyr::summarise(Expunere = sum(`Solduri finale|Debit`)) %>%
      dplyr::arrange(desc(tip_sursa)) %>%
      tidyr::pivot_wider(names_from = tip_sursa,values_from = Expunere) %>% 
      dplyr::arrange(desc(data_balanta)) %>% janitor::adorn_totals(where = "col") %>%
      dplyr::mutate(Pondere_Surse_Proprii = Surse_Proprii/Total) %>% as.data.frame()
    },error = function(e) { data.frame(data_balanta = conditionMessage(e),
                            Surse_Proprii=NA_integer_,Surse_Administrare=NA_integer_,Total=NA_integer_,
                            Pondere_Surse_Proprii = NA_real_) 
      }
    )
    
    
    output$tabel9 <- DT::renderDataTable( { req(vals$tabel9)
      DT::datatable(data = vals$tabel9, rownames = FALSE, options = list(dom="Bt", buttons = c("copy","csv","excel")),
          extensions = "Buttons", caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
            paste0("Sinteza disponbilitati la data de: ", vals$report_date))) %>%
        DT::formatRound(columns = 2:4,digits = 0) %>% DT::formatPercentage(columns = 5,digits = 0) }
    ) 
    
   
    vals$tabel10 <-  tryCatch(expr = {
      vals_balanta$balanta_database %>% dplyr::filter(
      data_balanta %in% c( vals$report_date, vals$previous_month, vals$previous_year),
      tip_sursa == "Surse_Proprii") %>% dplyr::group_by(data_balanta, tip_plasament) %>% 
      dplyr::summarise(Expunere = sum(`Solduri finale|Debit`)) %>% dplyr::arrange(desc(data_balanta), desc(Expunere)) %>%
      dplyr::mutate(Ponderi = prop.table(Expunere)) %>%
      tidyr::pivot_wider(names_from = data_balanta,names_sep = "_",values_from = c(Expunere, Ponderi)) %>%
        dplyr::select(-dplyr::matches(paste0("Expunere_",vals$previous_month, '|', "Expunere_",vals$previous_year))) %>%
      janitor::adorn_totals(where = "row")
    }, error = function(e) { data.frame(tip_plasament = conditionMessage(e),Expunere=NA_integer_,
                          Ponderi=NA_real_) } )
    
     
      output$tabel10 <- DT::renderDataTable({ req(vals$tabel10)
        DT::datatable(  data = vals$tabel10,rownames = FALSE,
          options = list(dom = "Bt", buttons = c("copy", "csv", "excel")), extensions = "Buttons",
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
            'Evoluţia distribuţiei surselor financiare proprii ale FNGCIMM ') ) %>%
          DT::formatRound(
            columns = if (sum(stringr::str_detect( string = names(vals$tabel10), pattern = "Expunere")) == 0)
              0 else stringr::str_which(string = names(vals$tabel10), pattern = "Expunere"),
            digits = 0) %>% DT::formatPercentage(
            columns = if (sum(stringr::str_detect(
              string = names(vals$tabel10), pattern = "Ponderi")) == 0)
              0  else stringr::str_which(string = names(vals$tabel10), pattern = "Ponderi"),
            digits = 1)
      })
      
    })
    
  })
}
    
## To be copied in the UI
# mod_plasamente_ui("plasamente_ui_1")
    
## To be copied in the server
# mod_plasamente_server("plasamente_ui_1")
