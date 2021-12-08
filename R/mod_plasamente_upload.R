#' plasamente_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plasamente_upload_ui <- function(id){
  ns <- NS(id)
  
  
  fluidPage( shinyjs::useShinyjs(),
             fluidRow( column(width = 4,
                              shinyFeedback::useShinyFeedback(),
                              fileInput(
                                inputId = ns("balanta_upload"),
                                accept = c(".xls", ".xlsx"),
                                placeholder = "No file uploaded",
                                buttonLabel = "Excel only",
                                label = "Upload balanta de verificare") ),
                       column(width = 4, uiOutput(ns("show_balanta_date"))),
                       column(width = 4, br(),uiOutput(ns("show_down") )),
                       column(width = 12, DT::dataTableOutput(ns("verificare_conturi")), br() ), 
                       column(width = 3, uiOutput(ns("show_save"))),
                       br(), hr(),
                       uiOutput(ns("missing_banks"))
             ))
  
}
    
#' plasamente_upload Server Functions
#'
#' @noRd 
mod_plasamente_upload_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    coresp_banci_curente <- readRDS("R/reactivedata/balanta/coresp_banci_curente.rds")
    
    coresp_banci_depozite <- readRDS("R/reactivedata/balanta/coresp_banci_depozite.rds")
    
    vals_balanta_upload <- reactiveValues( nume_obligatorii = c("Simbol cont","Denumire cont",
                                             "Solduri finale|Debit", "Solduri finale|Credit"), check_banks=TRUE )
    
    updateTabsetPanel(session = session, inputId = 'tab_upload',selected = "Database")
    
    observeEvent(input$balanta_upload,{
      
      shiny::validate(shiny::need(tools::file_ext(input$balanta_upload$datapath) %in% c("xlsx","xls"),
                                  message = paste0("XLSX only! You uploaded a ",tools::file_ext(input$balanta_upload$datapath)," file")))
      
      vals_balanta_upload$file_input = input$balanta_upload$datapath
      
      mod_read_excel_server("read_excel_ui_1",excel_reactive = vals_balanta_upload, red = "#dd4b39")  })
    
    observeEvent(vals_balanta_upload$all_names,{ req(vals_balanta_upload$all_names == TRUE)
      
      output$show_balanta_date <- renderUI( dateInput(inputId = session$ns("balanta_date"),
                                                      label = "Selecteaza data balantei uploadate",value = vals$report_date) )
      
      
      # I deduct sold credit form sold debit and keep only the new processed sold debit
      vals_balanta_upload$file_read_prel <- vals_balanta_upload$file_read_prel %>% 
        dplyr::mutate("Solduri finale|Debit" = `Solduri finale|Debit` - `Solduri finale|Credit`) %>%
        dplyr::select(-"Solduri finale|Credit")
      
      conturi_curente_banci <-  reactive({
        conturi_curente <- vals_balanta_upload$file_read_prel %>%
          dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                          pattern = rebus::`%R%`('^271', rebus::zero_or_more(rebus::DIGIT) )))
        
        
        conturi_curente <- conturi_curente %>% dplyr::left_join(coresp_banci_curente %>% dplyr::select(1, 3),
                                                                by = "Simbol cont")
        
        return( conturi_curente %>%      dplyr::slice(  which(
          purrr::map( conturi_curente$`Simbol cont`,
                      ~ sum(  stringr::str_detect(  string = conturi_curente$`Simbol cont`,
                                                    pattern = rebus::`%R%`(.x, rebus::one_or_more(rebus::DIGIT))  ) ) == 0) %>% unlist() == TRUE)) %>%
            dplyr::filter(`Solduri finale|Debit` != 0) %>%
            dplyr::mutate(tip_plasament = ifelse( stringr::str_detect(string = `Denumire cont`, pattern = 
                                                                        "gestionari|cautiune|mobiliara|CAUTIUNE|Cautiune"), "Gestionari_Cautiuni_Garantii", "Conturi_Curente" ),
                          tip_sursa = ifelse(stringr::str_detect(string = `Denumire cont`, 
                                                                 pattern = "MADR|administrare|SAPARD"), "Surse_Administrare", "Surse_Proprii")  )
        )   })
      
      conturi_depozite_banci <- reactive({
        conturi_depozite <- vals_balanta_upload$file_read_prel %>%
          dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                          pattern = rebus::`%R%`('^272', rebus::zero_or_more(rebus::DIGIT)     )))
        
        conturi_depozite <- conturi_depozite %>% dplyr::left_join(coresp_banci_depozite %>% dplyr::select(1, 3),
                                                                  by = "Simbol cont")
        
        return( conturi_depozite %>%    dplyr::slice( which(
          purrr::map( conturi_depozite$`Simbol cont`,
                      ~ sum(  stringr::str_detect(string = conturi_depozite$`Simbol cont`,
                                                  pattern = rebus::`%R%`(.x, rebus::one_or_more(rebus::DIGIT))) ) == 0 ) %>% unlist() == TRUE )) %>%
            dplyr::filter(`Solduri finale|Debit` != 0) %>%
            dplyr::mutate(tip_plasament = ifelse( stringr::str_detect(string = `Denumire cont`,
                                                                      pattern = "mobiliara"),"Gestionari_Cautiuni_Garantii", "Depozite"),
                          tip_sursa = ifelse(stringr::str_detect(string = `Denumire cont`, 
                                                                 pattern = "MADR|administrare|SAPARD"), "Surse_Administrare", "Surse_Proprii")  )
        )  })
      
      titluri <- reactive({
        extract_titluri <- vals_balanta_upload$file_read_prel %>%
          dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                          pattern = rebus::`%R%`('^304', rebus::zero_or_more(rebus::DIGIT))))
        
        return(extract_titluri %>% dplyr::slice(which(
          purrr::map(extract_titluri$`Simbol cont`,
                     ~ sum(stringr::str_detect(string = extract_titluri$`Simbol cont`,
                                               pattern = rebus::`%R%`(.x, rebus::one_or_more(rebus::DIGIT))  )  ) == 0  ) %>% unlist() == TRUE)) %>%
            dplyr::filter(`Solduri finale|Debit` != 0) %>%  dplyr::mutate(
              tip_plasament = ifelse(stringr::str_detect(string = `Denumire cont`, 
                                                         pattern = "SMALL FINANCE"),"obligatiuni_corporative", "titluri_stat"),
              tip_sursa = ifelse(stringr::str_detect(string = `Denumire cont`, 
                                                     pattern = "MADR|administrare|SAPARD"),"Surse_Administrare","Surse_Proprii"),
              Banca = ifelse(stringr::str_detect(string = `Denumire cont`, pattern = "SMALL FINANCE"),
                             "SMALL FINANCE", "ROMANIA" ) ) 
        )  })
      
      trezorerie_detaliat <- reactive({ req(conturi_curente_banci(), conturi_depozite_banci(), titluri() )
        dplyr::bind_rows(conturi_curente_banci(), conturi_depozite_banci(), titluri() )    })
      
      vals_balanta_upload$df_new <- trezorerie_detaliat()
      
      # I do not use it for the moment
      conturi_banci_sinteza <- reactive({ req( trezorerie_detaliat() )
        trezorerie_detaliat()  %>% tidyr::pivot_wider(names_from = c(tip_plasament, tip_sursa),
                                                      names_sep = "_",values_from = "Solduri finale|Debit",values_fill = 0) %>% 
          dplyr::select(-c(1:2)) %>% dplyr::group_by(Banca) %>% dplyr::summarise_all(.funs = ~sum(.)) %>%
          dplyr::mutate(Expunere_banca =rowSums(x = dplyr::select_if(.tbl = .,.predicate = is.numeric)))
      })
      
      output$conturi_banci <- DT::renderDataTable({ 
        DT::datatable(width = "350px",data =   conturi_banci_sinteza() %>%
                        janitor::adorn_totals(where = "row",fill = "-") %>%
                        dplyr::arrange(desc(Expunere_banca)) %>%
                        dplyr::mutate(row_names = 0:nrow(conturi_banci_sinteza())) %>%
                        tibble::column_to_rownames('row_names'), 
                      rownames = TRUE, options = list(dom = "tp", pageLength=6,autoWidth = TRUE, scrollX=TRUE,
                                                      columnDefs = list(list(width = '50%', targets = list(2:3)))),
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                        'Trezoreria FNGCIMM') ) %>%
          DT::formatRound(columns = 2:9,digits = 0)
      })
      
      output$show_down <- renderUI( { req(conturi_curente_banci(), conturi_depozite_banci())
        downloadLink(session$ns("down_conturi"),label = "Downloadeaza balanta prelucrata", class = "pull-right")  })
      
      #output$down_conturi <- downloadHandler(filename = function() {"banci_conturi.csv"},content = function(file) {
      # readr::write_csv(x = conturi_banci () %>%  dplyr::select(-1) %>%
      #                  dplyr::group_by(Banca) %>% dplyr::summarise_all(.funs = ~sum(.)) %>%
      #                 janitor::adorn_totals(where = "row",fill = "-"),file=file)})
      
      output$down_conturi <- downloadHandler(filename = function() { paste0("balanta_",input$balanta_date,".csv") },
                                             content = function(file) { readr::write_csv(x = vals_balanta_upload$df_new, file=file) }    )
      
      output$verificare_conturi <-      DT::renderDataTable( { req(trezorerie_detaliat)
        DT::datatable(options = list(dom = "t"), data = 
                        data.frame(row.names = c("Titluri","Conturi Curente", "Depozite, din care:", "Depozite mobiliare"),stringsAsFactors = T,
                                   Balanta = c(
                                     vals_balanta_upload$file_read_prel %>%
                                       dplyr::filter(`Simbol cont` == "304") %>% dplyr::pull(`Solduri finale|Debit`),
                                     vals_balanta_upload$file_read_prel %>%
                                       dplyr::filter(`Simbol cont` == "271") %>% dplyr::pull(`Solduri finale|Debit`),
                                     vals_balanta_upload$file_read_prel %>%
                                       dplyr::filter(`Simbol cont` == "272") %>% dplyr::pull(`Solduri finale|Debit`),
                                     vals_balanta_upload$file_read_prel %>% dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                                            rebus::`%R%`('^272',rebus::zero_or_more(rebus::DIGIT)))) %>%
                                       dplyr::slice( stringr::str_which(string = `Denumire cont`,
                                                                        pattern = "gestionari|cautiune|mobiliara|CAUTIUNE|Cautiune") ) %>%
                                       dplyr::pull(`Solduri finale|Debit`) %>% sum(.)),
                                   
                                   Prelucrat = c( trezorerie_detaliat() %>% dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                                            rebus::`%R%`('^304',rebus::zero_or_more(rebus::DIGIT)))) %>% 
                                                    dplyr::pull(`Solduri finale|Debit`) %>% sum(.),
                                                  
                                                  trezorerie_detaliat() %>% dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                                            rebus::`%R%`('^271',rebus::zero_or_more(rebus::DIGIT)))) %>% 
                                                    dplyr::pull(`Solduri finale|Debit`) %>% sum(.),
                                                  trezorerie_detaliat() %>% dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                                            rebus::`%R%`('^272',rebus::zero_or_more(rebus::DIGIT)))) %>% 
                                                    dplyr::pull(`Solduri finale|Debit`) %>% sum(.),
                                                  trezorerie_detaliat() %>% dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                                            rebus::`%R%`('^272',rebus::zero_or_more(rebus::DIGIT)))) %>% 
                                                    dplyr::slice(stringr::str_which(string = `Denumire cont`,
                                                                                    pattern = "gestionari|cautiune|mobiliara|CAUTIUNE|Cautiune")) %>%
                                                    dplyr::pull(`Solduri finale|Debit`) %>% sum(.)
                                   )  ) %>% dplyr::mutate(Check = ifelse(abs(Prelucrat - Balanta) >1, "Check","OK"))  ) %>% 
          DT::formatRound(columns = 1:2,digits = 0) %>%
          DT::formatStyle(table = .,
                          columns = 3,valueColumns = 3, backgroundColor = DT::styleInterval("Check",c("gray","yellow")))})
    })
    
    # I need a separate observer, because input$date is accessible very late due to the fact that is renderUI  
    
    observeEvent(input$balanta_date,{
      vals_balanta_upload$df_new <- vals_balanta_upload$df_new %>% dplyr::mutate(data_balanta = input$balanta_date)
    })  
    
    #Below observer shows missing banks UI for the case when there are new accounting accounts not assigned
    # to banks within coresp_banci_curente or coresp_banci_depozite
    observeEvent(vals_balanta_upload$df_new,{
      
      output$show_save <- renderUI({ req(vals_balanta_upload$check_banks == FALSE)
        actionLink(inputId = session$ns("save_balanta"),label = "Salveaza balanta prelucrata",icon = icon("save")) })
      
      if(!any(is.na(vals_balanta_upload$df_new$Banca))) {
        vals_balanta_upload$check_banks <- FALSE   }
      
      else {
        output$missing_banks <- renderUI({
          fluidRow(
            column(width = 12,
                   h3("STOP, nu am identificat toate bancile.Completeaza in coloana 3:"), br() ),
            column(width = 12,rhandsontable::rHandsontableOutput(session$ns("fill_banks")), br() ),
            
            column(width = 6,
                   actionLink(inputId = session$ns("save_missing_banks"),label = "Salveaza bancile modificate", icon=icon("save")))
          )
        })
        output$fill_banks <- rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(data=vals_balanta_upload$df_new %>% dplyr::filter(is.na(Banca)) %>% 
                                         dplyr::select(1:2,4), rowHeaders = NULL,readOnly = F,colHeaders = 
                                         c("Simbol Cont", "Denumire Cont", "Selecteaza Finantatorul"),
                                       width = 600, height = 250) %>% 
            rhandsontable::hot_col(col = 3,type = "dropdown",
                                   source = unique(c(coresp_banci_depozite$Banca,coresp_banci_curente$Banca))) %>%
            rhandsontable::hot_col(col = 1:2,readOnly = TRUE) })
        
        shinyjs::disable(id = "save_missing_banks",asis = FALSE)
      }
    })
    
    # Below observer handles filling inside rhandontable
    observeEvent(input$fill_banks,{
      vals_balanta_upload$fill_banks <- rhandsontable::hot_to_r(input$fill_banks)
      
      if (!any(is.null( vals_balanta_upload$fill_blanks$Banca))) {
        vals_balanta_upload$check_banks <- FALSE }
      
      
    })
    
    
    
    observeEvent(input$save_missing_banks,{
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_fill_banks"),
                                     title = "CONFIRM?", type = "info",
                                     text = paste0("Esti sigur ca vrei sa salvezi bancile completate ?"),
                                     btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"))
    })
    
    observeEvent(input$confirm_fill_banks,{ req(input$confirm_fill_banks == TRUE)
      
      if (any(is.na(vals_balanta_upload$fill_banks$Banca)) ) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Am in continuare banci lipsa",
                                     type="error",btn_colors = "#ff007b") }
      else {
        
        coresp_banci_curente <- dplyr::bind_rows(coresp_banci_curente,vals_balanta_upload$fill_banks %>% 
                                                   dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                   pattern = rebus::`%R%`('^271', rebus::zero_or_more(rebus::DIGIT)))))
        coresp_banci_depozite <- dplyr::bind_rows(coresp_banci_depozite, vals_balanta_upload$fill_banks %>% 
                                                    dplyr::slice(stringr::str_which(string = `Simbol cont`,
                                                                                    pattern = rebus::`%R%`('^272', rebus::zero_or_more(rebus::DIGIT)))))
        
        saveRDS(object = coresp_banci_curente, file = "R/reactivedata/balanta/coresp_banci_curente.rds")
        saveRDS(object = coresp_banci_depozite, file = "R/reactivedata/balanta/coresp_banci_depozite.rds")
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                                 .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
        
        vals_balanta_upload$check_banks <- FALSE
      }
    })
    
    observeEvent(input$save_balanta,{
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save"),title = "CONFIRM?",
                                     text = paste0("Esti sigur ca vrei sa salvezi balanta la data de ", input$balanta_date, " ?"),
                                     btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
    })
    
    observeEvent(input$confirm_save,{ req(input$confirm_save == TRUE)
      balanta_database <- readRDS("R/reactivedata/balanta/balanta_database.rds")
      
      vals_balanta_upload$df_old <- balanta_database
      # I don`t need df$new, is already defined above
      #vals_balanta_upload$df_new <- trezorerie_detaliat()
      vals_balanta_upload$element_id <- as.Date(input$balanta_date)
      vals_balanta_upload$column_id = "data_balanta"
      vals_balanta_upload$finalise_process_compare_df = FALSE
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = vals_balanta_upload, red="#ff007b",green="#00ff84")
    })
    
    observeEvent(vals_balanta_upload$finalise_process_compare_df,{ req(vals_balanta_upload$finalise_process_compare_df == TRUE )
      saveRDS(object = vals_balanta_upload$df_new_prel,file = "R/reactivedata/balanta/balanta_database.rds")
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                               .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      vals_balanta_upload$finalise_process_compare_df <- FALSE
    })
    
  })
}
    
## To be copied in the UI
# mod_plasamente_upload_ui("plasamente_upload_ui_1")
    
## To be copied in the server
# mod_plasamente_upload_server("plasamente_upload_ui_1")
