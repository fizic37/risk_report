#' garantii_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_garantii_upload_ui <- function(id){
  ns <- NS(id)
  fluidPage(br(),
  fluidRow(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(color = "#ff007b", position = "bottom-right", timeout = 200),
    
    shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),
    
    column(width = 3 , fileInput(inputId = ns("upload_solduri"),buttonLabel = "Excel only",
                                placeholder = "No file uploaded",
                                accept = c(".xlsx",".xls"),
                                label = "Upload fisierul de solduri")),
    
    column(width = 3, textOutput(outputId = ns("mesaj_eroare_upload")) ),
    
    column(width = 3,uiOutput(ns("show_date_upload_sold")) ),
    column(width = 3,br(), uiOutput(ns("show_save"))),
    
    br(), br(), hr(),
    DT::dataTableOutput(ns("sumar_upload"))
    
  ) )
}
    
#' garantii_upload Server Functions
#'
#' @noRd 
mod_garantii_upload_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals_portof_upload <- reactiveValues( nume_obligatorii = c( "ID Document",
          'Beneficiar',  'Cod Partener',  'Banca',  'Nr contract',    'Valuta',
          'Soldul garantiei [in LEI]',    "Data contract",'Soldul creditului [in LEI]',   
          'Procentul de garantare',   'Tip Fond [centralizat]',    'Tip fonduri','Produs[centralizat]'),
        column_types_date = c("Data contract") )
    
    
    observeEvent(input$upload_solduri,{
      shiny::validate(shiny::need(tools::file_ext(input$upload_solduri$datapath) %in% c("xlsx","xls"),
                                  message = paste0("XLSX only! You uploaded a ",tools::file_ext(input$upload_solduri$datapath)," file")))
      
      vals_portof_upload$file_input = input$upload_solduri$datapath
      
      mod_read_excel_server("read_excel_ui_1",excel_reactive = vals_portof_upload, red = "#ff007b")
      
      output$mesaj_eroare_upload <- renderText({ req(vals_portof_upload$all_names == FALSE)
        paste0("STOP, nu am gasit coloanele: ",
               paste(vals_portof_upload$missing_names,collapse = "; ") %>% stringr::str_c())
        })
      #shinyjs::disable("upload_solduri")
    })  
  
    
    # Below observer activates after excel module is called and the excel contains all the mandatory column names
    observeEvent(vals_portof_upload$all_names,{ req(vals_portof_upload$all_names == TRUE)
      
      portofoliu_second_read <<- reactive({  vals_portof_upload$file_read_prel %>%  
          dplyr::mutate_at(.vars = c( 'Soldul garantiei [in LEI]', 'Procentul de garantare','ID Document' ),
                           .funs = as.numeric) %>% dplyr::mutate_at(.vars = 'Cod Partener', .funs = as.character) %>%
          
          dplyr::mutate(Tip_surse = ifelse(`Tip Fond [centralizat]`=="3. Garantii in nume si cont stat","Nume_cont_stat",
                                           ifelse(`Tip Fond [centralizat]`== "2. Garantii din surse MADR","Surse_administrare","Surse_proprii"))) %>%
          # I process an error within excel file
          dplyr::mutate(Tip_surse = ifelse(is.na(`Tip Fond [centralizat]`) & `Tip fonduri` == "INVEST",
                                           "Nume_cont_stat",Tip_surse))
      })
      
      
      sunt_nume_lipsa <- reactive ({ vals_portof_upload$nume_obligatorii %in% names(portofoliu_second_read()) %>% 
          all() })
      
      nume_lipsa <- reactive ({ setdiff(vals_portof_upload$nume_obligatorii, names(portofoliu_second_read()))  })
      
      date_upload_file <- reactive({ shiny::validate(shiny::need(
        sunt_nume_lipsa(), message = paste0("Lipseste coloana: ", 
                                            nume_lipsa(), collapse = ";") %>% stringr::str_c()))
        tryCatch({
          max(as.Date(portofoliu_second_read()$`Data contract`), na.rm = TRUE) %>% 
            lubridate::round_date(unit = "bimonth") -1}, error = function(e) Sys.Date() )
      })
      
      sumar_upload <- reactive({ req(portofoliu_second_read())
        portofoliu_second_read() %>% dplyr::mutate("Tip fonduri" = 
                       ifelse(`Tip fonduri`=="01.Fd. proprii",`Produs[centralizat]`,`Tip fonduri`)) %>% 
          dplyr::group_by(Tip_surse, Program = `Tip fonduri`) %>%
          dplyr::summarise(Nr_contracte = dplyr::n(), Nr_beneficiari = dplyr::n_distinct(`Cod Partener`),
                           Sold_garantii=sum(`Soldul garantiei [in LEI]`), 
                           Sold_credite_garantate = sum(`Soldul creditului [in LEI]`) ) %>%
          dplyr::arrange(Tip_surse, desc(Sold_garantii))        
                                    })
      
      output$sumar_upload <- DT::renderDataTable({ req(sumar_upload())
        DT::datatable(data = sumar_upload(), rownames = FALSE,
                      options = list(dom = "Bt", buttons = c('copy', 'csv', 'excel'), pageLength= nrow(sumar_upload())),
                      extensions = "Buttons",
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                paste0("Sumar date uploadate la ", input$data_upload_sold))) %>%
                        DT::formatRound(columns = 3:6,digits = 0) })
      
      
      
      output$show_save <- renderUI({ req(portofoliu_second_read())
        actionLink(inputId = session$ns("save_upload"),width = "320px",
                     label = "Click aici pentru a salva datele uploadate",icon = icon("save")) })
      
      output$show_date_upload_sold <- renderUI({ req(date_upload_file())
        
        #dateInput(inputId = session$ns("data_upload_sold"),value = date_upload_file(),
                      #label = "Selecteaza data portofoliului uploadat",autoclose = TRUE)
        shinyWidgets::airDatepickerInput(inputId = session$ns("data_upload_sold"),value = date_upload_file(),language = "ro",
            label = "Selecteaza data portofoliului uploadat",view = "months",autoClose = T)
        })
      
    })
    
    observeEvent(input$save_upload,{
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save"),title = "Confirm",
              text = "Esti sigur ca vrei sa salvezi datele uploadate? Garantiile de stat nu vor fi salvate detaliat.",
                   btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
      
    })
    
    observeEvent(input$confirm_save,{req(input$confirm_save == TRUE)
      
     baza_banci <- readRDS("R/reactivedata/solduri/baza_banci.rds")
      
     vals_portof_upload$df_new <- portofoliu_second_read() %>% dplyr::filter(Tip_surse != "Nume_cont_stat") %>%
                                  dplyr::select(-`Tip Fond [centralizat]`, -`Data contract`) %>% 
                                  dplyr::mutate(data_raport = input$data_upload_sold)
     
     vals_portof_upload$df_old <- baza_banci
     vals_portof_upload$element_id <- input$data_upload_sold
     vals_portof_upload$column_id = "data_raport"
     vals_portof_upload$finalise_process_compare_df = FALSE
    
     callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = vals_portof_upload, red="#ff007b",green="#00ff84")  
      
    })
    
    observeEvent(vals_portof_upload$finalise_process_compare_df,{ req(vals_portof_upload$finalise_process_compare_df == TRUE )
      
      vals$view_baza_solduri <- portofoliu_second_read() %>% dplyr::mutate("Tip fonduri" = 
                ifelse(`Tip fonduri`=="01.Fd. proprii",`Produs[centralizat]`,`Tip fonduri`)) %>% 
        dplyr::group_by(Tip_surse, `Tip fonduri`) %>%
        dplyr::summarise(    Nr_contracte = dplyr::n(),
                             Nr_beneficiari = dplyr::n_distinct(`Cod Partener`),
                             Sold_garantii = sum(`Soldul garantiei [in LEI]`),
                             Sold_credite_garantate = sum(`Soldul creditului [in LEI]`)) %>%
        dplyr::ungroup() %>% dplyr::mutate(data_raport = input$data_upload_sold) %>%
        dplyr::bind_rows(vals$view_baza_solduri %>% dplyr::filter(data_raport != input$data_upload_sold))
      
      saveRDS(object = vals$view_baza_solduri,file = "R/reactivedata/view_baza_sold.rds")
      
      saveRDS(object = vals_portof_upload$df_new_prel, file = "R/reactivedata/baza_banci.rds")
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                               .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
      
      updateSelectInput(inputId = 'date_baza_solduri', session = session,
                        choices = vals$view_baza_solduri$data_raport %>% unique())
      
      #shinyjs::disable(id = "save_upload", asis = TRUE)
      removeUI(selector = "#garantii_upload_ui_1-save_upload",session = session)
      vals_portof_upload$finalise_process_compare_df <- FALSE
        })
 
  })
}
    
## To be copied in the UI
# mod_garantii_upload_ui("garantii_upload_ui_1")
    
## To be copied in the server
# mod_garantii_upload_server("garantii_upload_ui_1")
