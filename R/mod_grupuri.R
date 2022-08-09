#' grupuri UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_grupuri_ui <- function(id){
  # Handles Grupuri debitori tabpanel UI generation and also UI of upload grupuri box
  ns <- NS(id)
  tagList(
    
    bs4Dash::box(title = "Upload grupuri",collapsible = TRUE,collapsed = TRUE,width = 12,maximizable = T,
                 status = "primary", icon = icon("file-upload"), 
                 footer = "Se uploadeaza fisierul Grupuri IMM Expuneri/plati din Rapoarte adapative",
                 fluidPage( 
                   shinybusy::add_busy_spinner(  color = "#ff007b",    position = "bottom-right",    timeout = 200    ),
                   fluidRow(
                     column( width = 2, fileInput(  inputId = ns("grupuri_upload"),
                                                    label = "Upload grupuri", width = "300px", accept = c(".xlsx", ".xls"),
                                                    buttonLabel = "Excel only", placeholder = "No file uploaded")    ),
                     column(width = 4, uiOutput(ns("show_toggle_missing"))),
                     
                     column(width = 3, uiOutput(ns("show_down_grupuri_partiale")) ),
                     
                     column(width = 3, uiOutput(ns("show_grup_date"))),
                   
                     column(width = 12, DT::dataTableOutput(ns('grupuri_detaliate')) ),
                     
                     column(width = 12, uiOutput(ns("show_down_grupuri")), br() ),
                    
                     column(width = 12, DT::dataTableOutput(ns( "grupuri_selectate")))
                    
                   
                                    )  )  ),
  
    mod_top_expuneri_grupuri_ui("top_expuneri_grupuri_ui_1")
  )
   
}
    
#' grupuri Server Functions
#'
#' @noRd 
mod_grupuri_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    nume_obligatorii_grupuri <- c("GrupId","NrGrup","Grup","Beneficiar","CUI","Nrcontract")
    
    grupuri_reactive <- reactiveValues(nume_obligatorii=nume_obligatorii_grupuri, need_more_data='FALSE')
    
    observeEvent(input$grupuri_upload, {
      
      
      grupuri_reactive$file_input <-  input$grupuri_upload$datapath
      
      mod_read_excel_server("read_excel_ui_1",excel_reactive = grupuri_reactive)
      
      
      output$show_grup_date <- renderUI({req(grupuri_reactive$all_names)
        if (grupuri_reactive$all_names==FALSE) {
          h5(paste("Lipseste coloana:",grupuri_reactive$missing_names,collapse = " ; "))}
        
        else { shinyWidgets::airDatepickerInput(inputId = ns("data_grupuri"),label = "Data de constituire a grupurilor:",
                        value = vals$report_date, width = "300px") }
        
      })
      
    })
    
      observe({  req(grupuri_reactive$all_names == TRUE)
        grupuri_reactive$fisier_prelucrat <-  dplyr::mutate(grupuri_reactive$file_read_prel,  
              data_constituire_grup = lubridate::make_date( year = paste0( "20",
                  stringr::str_sub( grupuri_reactive$file_read_prel$NrGrup, start = -2, end = -1) ) %>% as.numeric(),
              month = stringr::str_sub( grupuri_reactive$file_read_prel$NrGrup,
                start = -4, end = -3) %>% as.numeric(),
              day = stringr::str_sub( grupuri_reactive$file_read_prel$NrGrup,
                start = -6, end = -5) %>% as.numeric()) )
      })
      
      # Main observer where grupuri is processed and output
      observeEvent(input$data_grupuri,{
        
        grupuri_reactive$bi_grupuri <- readRDS('R/reactivedata/grupuri/bi_grupuri.rds')
        
        finantari_grupuri <- readRDS("R/reactivedata/grupuri/finantari_grupuri.rds") 
        
        baza_solduri <- readRDS("R/reactivedata/solduri/baza_banci.rds")
        
        grupuri_reactive$filtered_data <- dplyr::filter(.data = grupuri_reactive$fisier_prelucrat,
          grupuri_reactive$fisier_prelucrat$data_constituire_grup <= input$data_grupuri) %>% 
          dplyr::select(nume_obligatorii_grupuri,data_constituire_grup) %>%
          dplyr::left_join( grupuri_reactive$bi_grupuri, by = c("Nrcontract" = "Numar contract")) 
        
        
        # I process tipologie conventie, for every contract that contains OUG, AGRO, PROD, GCU sau GCI it will be garantie_stat
        grupuri_reactive$filtered_data <- grupuri_reactive$filtered_data %>%
          dplyr::mutate(tipologie_conventie = ifelse( is.na(tipologie_conventie) & 
              stringr::str_detect(string = grupuri_reactive$filtered_data$Nrcontract, pattern = "OUG|AGRO|PROD|GCU|GCI"),
                    "garantii_stat", tipologie_conventie))
        
        
        # I process tipologie_conventie - for my fixed list of finantari
        grupuri_reactive$filtered_data <- grupuri_reactive$filtered_data %>% dplyr::mutate(tipologie_conventie = 
              ifelse(is.na(tipologie_conventie) & Nrcontract %in% finantari_grupuri$Nrcontract,
                      "finantare", tipologie_conventie) ) %>%
          dplyr::mutate(unic_tipologie = ifelse(tipologie_conventie %in% c("surse_proprii", "surse_administrare", "finantare"),
              "surse_proprii sau administrare", ifelse(is.na(tipologie_conventie),"fara_contract","garantii_stat")))
        
        
        if ( length( grupuri_reactive$filtered_data$Nrcontract[is.na(
          grupuri_reactive$filtered_data$tipologie_conventie) & grupuri_reactive$filtered_data$Nrcontract!="-"]) >0 ) {
          grupuri_reactive$need_more_data <- 'TRUE'
        }
        
        output$show_down_grupuri_partiale <- renderUI({ 
          req( grupuri_reactive$filtered_data, grupuri_reactive$need_more_data == 'TRUE' )
          
          div(style = "padding-top: 35px;", shinyWidgets::downloadBttn(outputId = ns("down_grupuri_partiale"),
            label = "Download grupuri partiale", style = "simple",color = "success",size = "sm") )  })
        
        output$down_grupuri_partiale <- downloadHandler(filename = function() {
              paste0("grupuri_partiale_",input$data_grupuri,".csv") },
              content = function(file) { readr::write_csv(x = grupuri_reactive$grupuri_selectate, file = file ) } )
        
        
        
        output$show_toggle_missing <- renderUI({ 
          switch( grupuri_reactive$need_more_data,
                 'TRUE' = tagList( br(),
                div(style="margin-left:20px; padding-top: 10px;",
                shinyWidgets::actionBttn(ns("show_missing"),icon =icon("hand"), 
                                         color="danger",style = "simple", size="sm",
                                   label = "STOP, click here to update BI") ) ),
                'FALSE' = div( style="margin-left:120px; padding-top: 28px;",
                                   shinyWidgets::actionBttn(ns("save_grupuri"),label = "Salveaza grupurile uploadate",
                              color = "success",style = "stretch",size = "md",icon = icon("save")) ) 
                        
                )
          
        })
        
        output$show_down_grupuri <- renderUI( { req( grupuri_reactive$need_more_data == 'FALSE')
          shinyWidgets::downloadBttn(ns("down_grupuri_selectate"),
                label = "Download grupurile detaliate de mai sus",style = "stretch",color = "primary",size = "sm")
        })
        
        # I get unique combination of unic_tipologie_conventie for every distinct group in order to filter out later
        grupuri_reactive$grupuri_prelucrate <- grupuri_reactive$filtered_data %>% 
          dplyr::arrange(GrupId,unic_tipologie) %>%
          dplyr::group_by(GrupId,.drop = TRUE) %>% 
          dplyr::summarise(unic_tipologie = paste(unique(unic_tipologie),collapse = " si ")) %>% 
          dplyr::ungroup()
        
        
        # I obtain the final list of grupuri after I only keep for groups that contain surse proprii sau administarre
        grupuri_reactive$grupuri_selectate <- grupuri_reactive$filtered_data %>% 
          dplyr::filter(GrupId %in% (grupuri_reactive$grupuri_prelucrate %>% 
          dplyr::slice(stringr::str_which(string = grupuri_reactive$grupuri_prelucrate$unic_tipologie,
                              pattern = "surse_proprii sau administrare")) %>% dplyr::pull(GrupId)))
        
        baza_solduri_filtrata <- reactive({
          baza_solduri %>% dplyr::filter(data_raport == input$data_grupuri)  })
        
        
        grupuri_reactive$grupuri_selectate <- grupuri_reactive$grupuri_selectate %>% 
          dplyr::left_join(y = baza_solduri_filtrata() %>% 
                             dplyr::select("Nr contract", Expunere_garantare = "Soldul garantiei [in LEI]"),
                           by = c("Nrcontract" = "Nr contract"))
        
        output$grupuri_selectate <- DT::renderDataTable( {req(grupuri_reactive$filtered_data)
          switch(EXPR = grupuri_reactive$need_more_data,
                 'TRUE' =  DT::datatable(data =  grupuri_reactive$filtered_data %>% dplyr::filter(is.na(tipologie_conventie) & 
                              Nrcontract!="-") %>% dplyr::mutate(dplyr::across(.cols = 3:4, ~as.factor(.x))), 
                              options = list(pageLength = 4, dom = "Ftp", scrollY=TRUE),filter = "top",
                              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: #ff5964;',
                               "Nu se constituie grupurile de mai jos din cauza lipsei conventiei. Click pe 
                                butonul Stop de mai sus pentru a actualiza BI-ul."), rownames = F),
                 'FALSE' = DT::datatable(data = grupuri_reactive$grupuri_selectate %>% 
                              dplyr::summarise(Nr_grupuri = dplyr::n_distinct(GrupId), 
                                Expunere_garantare = sum(Expunere_garantare, na.rm = TRUE)) %>%
                                 dplyr::mutate(Expunere_garantare_surse_proprii = 
                                  sum(grupuri_reactive$grupuri_selectate$Expunere_garantare[
                                  grupuri_reactive$grupuri_selectate$tipologie_conventie=="surse_proprii"],na.rm = T)),
                                 rownames = FALSE, options =  list(dom = "Bt"),extensions = 'Buttons',
                                 caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                   paste0('Sinteza grupuri constituite la data de  ', input$data_grupuri))) %>%
                   DT::formatRound(columns = 1:3,digits = 0)
                
          )
        })
        
        
        output$grupuri_detaliate <- DT::renderDataTable({ req( grupuri_reactive$need_more_data == 'FALSE' )
          
            DT::datatable(
              data = grupuri_reactive$grupuri_selectate %>% 
                dplyr::mutate_at(.vars = "tipologie_conventie", as.factor) %>%
                dplyr::select(nume_obligatorii_grupuri,
                              data_constituire_grup, tipologie_conventie),  rownames = FALSE,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: #28b78d;',
                        paste0( "Lista detaliata a grupurilor valabila la data de ",
                                as.character(input$data_grupuri)) ),
              filter = list(position = "top", clear = TRUE, plain = TRUE),
              options = list(dom = "tp", autoWidth = TRUE,   scrollY = TRUE,
                             scrollX = TRUE,   pageLength = 5,   searchHighlight = TRUE,
                             fixedColumns = list(leftColumns = 4),
                             columnDefs = list(list(width = '100px', targets = 4),
                                               list(width = '175px', targets = 3) ) )) } )
          
        
        output$down_grupuri_selectate <- downloadHandler(
          filename = function() { paste0("grupuri_constituite_",input$data_grupuri,".csv") },
          content = function(file) {readr::write_csv(x = grupuri_reactive$grupuri_selectate,path = file) } )
       
        
      
      })
      
      # Observer to show modal when data is missing
      observeEvent( input$show_missing,{
        showModal( modalDialog(title = "Actualizeaza BI-ul", size = "l", footer = modalButton("Close",
                                      icon = icon("square-xmark")),
                   fluidRow( column(width = 6,
                                    fileInput(inputId = ns("input_bi_grupuri"),label = "Upload BI",accept = ".xlsx",
                                    buttonLabel = "Excel only",placeholder = "No file uploaded"),
                                    textOutput(ns("mesaj_bi_grupuri")) ),
                             column(width = 6, br(), shinyWidgets::downloadBttn(ns("link_bi_grupuri"),size = "sm",
                                style = "stretch",color = "primary",label = "Click aici pentru a downloada modelul de BI"))
                   )
        )
        )
        
        output$link_bi_grupuri <- downloadHandler(filename = function() {"BI_grupuri.xlsx"}, content = function(file) {
          file.copy(from = "R/reactivedata/grupuri/bi_grupuri.xlsx",to = file)       })
      })
      
     # Observer to process input of BI grupuri 
      observeEvent(input$input_bi_grupuri,{
        tipologie_conventie <-  reactive( {
          readxl::read_excel(input$input_bi_grupuri$datapath, sheet = "tipologie_conventie",
                             skip =4 ) %>% dplyr::select("Tip conventie",tipologie_conventie) })
        
        bi_grupuri <- reactive({
          bi_grupuri <- readxl::read_excel(input$input_bi_grupuri$datapath, sheet = "baza_contracte",
                                           skip =4)
          
          bi_grupuri$`Data contract` <- as.Date.character(x = bi_grupuri$`Data contract`, 
                                                          format = "%Y-%m-%d 00:00:00.000")
          
          bi_grupuri <- bi_grupuri %>% dplyr::left_join(tipologie_conventie())
        })
        
       
        if ( nrow(bi_grupuri()) < nrow(grupuri_reactive$bi_grupuri) )  {
          output$mesaj_bi_grupuri <- renderText("STOP, am in baza de date mai multe inregistrari decat mi-ai furnizat tu")
        }
        else {
          grupuri_reactive$bi_grupuri <- bi_grupuri()
          saveRDS(object = grupuri_reactive$bi_grupuri,file = 'R/reactivedata/grupuri/bi_grupuri.rds')
          file.copy(from = input$input_bi_grupuri$datapath,to = "R/reactivedata/grupuri/bi_grupuri.xlsx",overwrite = TRUE)
          output$mesaj_bi_grupuri <- renderText({ 
            "Baza de date a fost actualizata cu succes. Inchide fereastra si mai uploadeaza inca o data fisierul de grupuri" })
        }
      })
      
      # Save grupuri
      observeEvent(input$save_grupuri, {
        shinyWidgets::ask_confirmation( inputId = session$ns("confirm_save"),
              title = "CONFIRM", text = paste0("Esti sigur ca vrei sa salvezigrupurile la data de ",
             input$data_grupuri), type = "info",  btn_labels = c("NU, renunta", "OK, salveaza"),
                                        btn_colors = c("#ff007b", "#00ff84") )  })
      
      observeEvent(input$confirm_save,{ req(input$confirm_save == TRUE)
        baza_grupuri <- readRDS("R/reactivedata/grupuri/baza_grupuri.rds")
        
        grupuri_reactive$df_new <- grupuri_reactive$grupuri_selectate %>% 
          dplyr::mutate(data_grupuri = input$data_grupuri)
        grupuri_reactive$df_old <- baza_grupuri
        grupuri_reactive$element_id <- input$data_grupuri
        grupuri_reactive$column_id = "data_grupuri"
        grupuri_reactive$finalise_process_compare_df = FALSE
        
        
        callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = grupuri_reactive, 
                   red = "#ff007b", green = "#00ff84")
        
        observe({ req(grupuri_reactive$finalise_process_compare_df == TRUE )
          
          saveRDS(object = grupuri_reactive$df_new_prel,file = "R/reactivedata/grupuri/baza_grupuri.rds")
          
          shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
            .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
          
        })
        
      })
      
      
  })
  
}
    
## To be copied in the UI
# mod_grupuri_ui("grupuri_ui_1")
    
## To be copied in the server
# mod_grupuri_server("grupuri_ui_1")
