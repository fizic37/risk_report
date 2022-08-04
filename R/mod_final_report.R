#' final_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_final_report_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    br(),
    shinybusy::add_busy_spinner(color = "#ff007b", position = "bottom-right", timeout = 200),
    shinyjs::useShinyjs(),
  fluidRow(column(width = 8,
  h5("Aici se genereaza fisierul word continand raportul de prudentialitate.
     Atentie, pentru a se putea genera, va trebui sa vizualizezi tabelele de mai jos sau sa bifezi Checked pentru a le calcula.")),
  column(width = 4,
  shinyWidgets::actionBttn(inputId = ns("action_report"),label = "Genereaza raportul de prudentialitate",
                           icon = icon("file-word"),style = "stretch",color = "primary")
  )),
  
 hr(),
 
  lapply(X = c("3si4","baza_date_grupuri","provizioane_plati","9si10",11, "anexe"), function(s) {
   
     shinyWidgets::awesomeRadio(
      inputId = ns(paste0("tabel",s)),
      label = paste0("Tabel ",s),status = "danger",
      choices = c("Checked","Please check"),
      selected = "Please check",
      inline = TRUE,checkbox = FALSE)  } )
  
  )
      
   
}
    
#' final_report Server Functions
#'
#' @noRd 
mod_final_report_server <- function(id,vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    shinyjs::disable(id = "action_report")
    
#lapply(X = c("3si4", "baza_date_grupuri", "provizioane_plati",9,10,11, 'anexe'), function(s) { # shinyjs::disable(id = paste0("tabel",s)) })
    
    observeEvent( input$tabel3si4,{ req(input$tabel3si4 == "Checked")
      vals$final_report_check <- "plafoane"
          })
    
    observeEvent( input$tabelbaza_date_grupuri,{ req(input$tabelbaza_date_grupuri == "Checked")
      vals$final_report_check <- "grupuri"
      
    })
   
    observeEvent( input$tabelprovizioane_plati,{ req(input$tabelprovizioane_plati == "Checked")
      vals$final_report_check <- "provizioane_plati" 
    })
    
    observeEvent( input$tabel9si10,{ req(input$tabel9si10 == "Checked")
      vals$final_report_check <- "plasamente"
      })
    
    observeEvent( input$tabel11,{ req(input$tabel11 == "Checked")
      vals$final_report_check <- "plati"
    })
    
    observeEvent( input$tabelanexe,{ req(input$tabelanexe == "Checked")
      vals$final_report_check <- "anexe"
    })
    
   # If vals$tables are not checked above, they can be loaded automatically with below observer.
   observe({
    
    if (!is.null(vals$tabel4)) (shinyWidgets::updateAwesomeRadio(session = session,inputId = "tabel3si4",
                                                                 selected = "Checked",status = "successs"))
    
    if (!is.null(vals$grupuri)) (shinyWidgets::updateAwesomeRadio(session = session,inputId = "tabelbaza_date_grupuri",
                                                                 selected = "Checked",status = "successs"))
    
    if (!is.null(vals$provizioane_plati)) (shinyWidgets::updateAwesomeRadio(session = session,inputId = "tabelprovizioane_plati",
                                                                  selected = "Checked",status = "successs"))
    
    if (!is.null(vals$tabel10)) (shinyWidgets::updateAwesomeRadio(session = session,inputId = "tabel9si10",
                                                                 selected = "Checked",status = "successs"))
    
    if (!is.null(vals$tabel11)) (shinyWidgets::updateAwesomeRadio(session = session,inputId = "tabel11",
                                                                 selected = "Checked",status = "successs"))
      
    if (all(!is.null(vals$anexa_A),!is.null(vals$anexaC_final))) (shinyWidgets::updateAwesomeRadio(session = session,
                                      inputId = "tabelanexe", selected = "Checked",status = "successs"))  
    
    if ( all( !is.null(vals$tabel4), !is.null(vals$grupuri), !is.null(vals$provizioane_plati),!is.null(vals$tabel9),
            !is.null(vals$tabel10),!is.null(vals$tabel11) , !is.null(vals$anexa_A),
             !is.null(vals$anexaC_final) ) ) shinyjs::enable(id = "action_report")
    
     })
   
   #observeEvent(vals,{browser()})
      
    observeEvent(input$action_report,{
      
      showModal(modalDialog(title = "ATENTIE!",size = "l",
              h3(paste0("Esti sigur ca vrei generezi raportul de prudentialitate la data de ",vals$report_date, " ?") ),
              footer = tagList(
                h6("Procesarea poate dura cateva secunde"),
                shinyWidgets::downloadBttn(outputId = session$ns("generate_report"),label = "Download",
                              style = "stretch", color = "success", size = "md"),
                shinyWidgets::actionBttn(inputId = session$ns("cancel_download"),label = "Cancel",style = "stretch",
                                  icon = icon("window-close"),color = "danger",size = "md")
                              )))
      
      observeEvent(input$cancel_download,{ removeModal(session = session) } )
      
    
    output$generate_report <-   downloadHandler( filename = function() { "prudentialitate.docx" },
            content = function(file) {
              removeModal(session = session)
              
              temporary_directory <- tempdir()
              tempReport <-   file.path(temporary_directory, "prudentialitate.Rmd")
              templateReport <-    file.path(temporary_directory, "template_prudentialitate.docx")
              
              file.copy(from = "prudentialitate.Rmd",
                        to =  tempReport,
                        overwrite = TRUE)
              file.copy(from = "R/reactivedata/template_prudentialitate.docx",
                        to =  templateReport,
                        overwrite = TRUE)
             
              fonduri_proprii = readRDS("R/reactivedata/solduri/baza_plafoane.rds") %>% 
                dplyr::filter(data_raport == vals$report_date) %>% dplyr::pull(Fonduri_proprii) %>% max(.,1)
              
              tabel7 <- readRDS("R/reactivedata/tabel7.rds") %>% 
                dplyr::mutate(`% din fonduri proprii*` = `Expunere neta, lei`*100/fonduri_proprii)
              
              params = list(luna_curenta = vals$report_date, tabel1 = vals$tabel1, tabel2 = vals$tabel2, 
                            tabel3 = vals$tabel3, tabel4 = vals$tabel4, grupuri_expunere = 
                              vals$grupuri %>% dplyr::filter(data_grupuri==vals$report_date) %>% 
                              dplyr::pull(Nr_grupuri_expunere_garantare),
                            grupuri_constituite = vals$grupuri %>% dplyr::filter(data_grupuri==vals$report_date) %>% 
                              dplyr::pull(Nr_grupuri),
                            expunere_grupuri = vals$grupuri %>% dplyr::filter(data_grupuri==vals$report_date) %>% 
                              dplyr::pull(Expunere_garantare_totala),
                            tabel5 =  vals$top_expuneri_fonduri_proprii, tabel6 = vals$top_expuneri_surse_proprii,
                            tabel7 = tabel7, tabel8 = vals$provizioane_plati, tabel9 = vals$tabel9, tabel10=vals$tabel10,
                            tabel11 = vals$tabel11, anexaA = vals$anexa_A,anexaB = vals$anexaC_final)
              
              rmarkdown::render(input = tempReport,
                output_file = file,
                params = params,
                envir = new.env(parent = globalenv())
              )
            }
          )
    
    updateActionButton(session = session,inputId = "action_report")
    })
       
  })
}
    
## To be copied in the UI
# mod_final_report_ui("final_report_ui_1")
    
## To be copied in the server
# mod_final_report_server("final_report_ui_1")
