#' plati UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plati_ui <- function(id){
  # Handles UI and server of Plati si cerrei plata tabpanel
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(  color = "#ff007b",    position = "bottom-right",    timeout = 200),
    shinyFeedback::useShinyFeedback(),
    
    bs4Dash::box(title = "Tabelul 11 - rata platilor de garantii",collapsible = T,collapsed = F,
                 maximizable = T, width = 12,icon = icon("border-all"),
                 DT::dataTableOutput(ns("tabel11"))),
    fluidRow(
    bs4Dash::box(title = "Baza de date a platilor de garantii", collapsible = T,
                 collapsed = T, maximizable = T, width = 6, icon = icon("database"),
                 fluidRow(column(width = 12,                              
                                 DT::dataTableOutput(ns("plati_database")), br()),
                          column(width = 6, shinyWidgets::materialSwitch(inputId = ns("change_view"),label = "Change view",
                                                                         value = FALSE,status = "primary") ),
                          column(width = 6, shinyWidgets::materialSwitch(inputId = ns("show_snapshots"),
                                label = "Show BI data snapshots", value=FALSE,status = "primary") ),
                          column(width = 12,DT::dataTableOutput(ns("snapshots")))
                 )),
    
    bs4Dash::box(title = "Baza de date a cererilor de plata",collapsible = T,
                 collapsed = T, maximizable = T, width = 6, icon = icon("database"),
                 fluidRow(column(width = 12,                              
                                 DT::dataTableOutput(ns("cereri_plata_database")), br()),
                          column(width = 3, shinyWidgets::materialSwitch(inputId = ns("change_view_cereri_plata"),
                                  label = "Change view",   value = FALSE,status = "primary") ),
                          column(width = 5, downloadLink(outputId = ns("down_cereri_plata"),
                                        label = "Download fisierul de cereri de plata")),
                          column(width = 4, textOutput(outputId = ns("show_max_cereri_plata")))
                 ))
    ),                  
    
    
    fluidRow(                    
    bs4Dash::box(title='Upload BI file to update plati database', collapsible = T,
                 collapsed = T, maximizable = T, width = 6,icon=icon("file-excel"),
                 footer = "Se downloadeaza BI din link-ul atasat, se actualizeaza data snapshot pentru ambele tabele, se salveaza local si
      apoi se uploadeaza fisierul salvat.",
                 fluidRow(
                   column(width = 6, fileInput(ns("bi_upload"),"Upload BI file",accept = ".xlsx",buttonLabel = "Excel only",
                                               placeholder = "Nothing uploaded") ),
                   
                   column(width = 6, br(), textOutput(outputId = ns("messages"))),
                   
                   hr(),
                   
                   column(width = 12, br(), #div(style="display:inline-block;margin-left: 40%;padding-bottom: 10px;", 
                              downloadLink(outputId = ns("bi_link"),label = "Click aici pentru a downloada modelul de BI")),
                   
                   column(width = 8, DT::dataTableOutput(ns("sinteza_upload"))),
                   column(width = 4),
                   column(width = 6,uiOutput(outputId = ns("show_save")))
                 )
    ),
    
    bs4Dash::box(title='Upload file for Cereri de plata', collapsible = T,
                 collapsed = T, maximizable = T, width = 6,icon=icon("file"),
                 footer = "Se downloadeaza fisierul din link-ul atasat, se actualizeaza coloanele DocumentId, 
                 Data_cerere_plata si cerere_plata, se salveaza local ca excel sau csv si se uploadeaza folosind butonul de mai sus",
                 
                 fluidRow(
                 column(width = 5, fileInput(ns("cereri_plata_upload"),"Upload cereri plata file",
                                             accept = ".xlsx",buttonLabel = "CSV or Excel",
                                             placeholder = "Nothing uploaded") ),
                 
                 column(width = 7, br(), verbatimTextOutput(outputId = ns("cereri_plata_messages"))),
                 
                 hr(),
                 
                 column(width = 6, br(),
                              downloadLink(outputId = ns("cereri_plata_link_excel"),
                                  label = "Download modelul Excel de fisier cereri plata")),
                 
                 column(width = 6, br(),
                        downloadLink(outputId = ns("cereri_plata_link_csv"),
                                     label = "Download modelul CSV de fisier cereri plata")),
                 
                 column(width = 8, DT::dataTableOutput(ns("sinteza_upload_cereri_plata"))),
                 column(width = 4),
                 column(width = 6,uiOutput(outputId = ns("show_save_cereri_plata")))
    ) )
    )
  )
}
    
#' plati Server Functions
#'
#' @noRd 
mod_plati_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    bi_snapshots <- readRDS("R/reactivedata/plati/bi_snapshots.rds")
    
    
    plati_database <- readRDS("R/reactivedata/plati/plati_database.rds")
    
    cereri_plata_database <- readRDS("R/reactivedata/plati/external_volume_cereri_plata/cereri_plata.rds")
    
    
    vals_plati <- reactiveValues(snapshots = bi_snapshots, cereri_plata_database = cereri_plata_database)
    
    output$cereri_plata_link_csv <- downloadHandler( filename = function() {"cereri_plata.csv"},content = function(file) {
      readr::write_csv(x = cereri_plata_database,file = file) } )
    
    output$bi_link <- downloadHandler( filename = function() {"BI_plati.xlsx"},content = function(file) {
      file.copy(from = "R/reactivedata/plati/BI_plati.xlsx",to = file) } )
    
    output$cereri_plata_link_excel <- downloadHandler( filename = function() {"cereri_plata.xlsx"},content = function(file) {
      file.copy(from = "R/reactivedata/plati/cereri_plata.xlsx",to = file) } )
    
    output$show_max_cereri_plata <- renderText({ req(vals_plati$cereri_plata_database)
      paste0("Data maxima a cererii de plata este: ",max(vals_plati$cereri_plata_database$Data_cerere_plata))
      })
    
    output$down_cereri_plata <- downloadHandler( filename = function() {"cereri_plata.csv"},content = function(file) {
      readr::write_csv(x = cereri_plata_database,file = file)   } )
                                                
    
    observeEvent(input$cereri_plata_upload,{
      
      tryCatch(expr = {
        
        vals_plati$cereri_plata_read <- switch( EXPR = tools::file_ext(input$cereri_plata_upload$datapath),
          "csv" = readr::read_csv(input$cereri_plata_upload$datapath) %>% 
            dplyr::select(DocumentId,`Cod Partener`,Data_cerere_plata,Cerere_Plata) %>%
            dplyr::mutate(dplyr::across(.cols = 1:2, ~as.character(.x))) %>%
            dplyr::mutate(dplyr::across(Data_cerere_plata, ~janitor::convert_to_date(x = .x))),
        
        "xlsx" = readxl::read_excel(input$cereri_plata_upload$datapath,sheet = 1) %>% 
            dplyr::select(DocumentId,`Cod Partener`,Data_cerere_plata,Cerere_Plata) %>%
            dplyr::mutate(dplyr::across(.cols = 1:2, ~as.character(.x))) %>%
            dplyr::mutate(dplyr::across(Data_cerere_plata, ~janitor::convert_to_date(x = .x)))
        )
         
        
     
      if ( janitor::compare_df_cols_same( vals_plati$cereri_plata_read, cereri_plata_database) &
        max(vals_plati$cereri_plata_read$Data_cerere_plata, na.rm=T) >= 
        max(cereri_plata_database$Data_cerere_plata,na.rm=T) ) {
        
       saveRDS(object = vals_plati$cereri_plata_read, file = "R/reactivedata/plati/external_volume_cereri_plata/cereri_plata.rds")
        
        if ( tools::file_ext(input$cereri_plata_upload$datapath ) == "xlsx" ) { 
          file.copy(from = input$cereri_plata_upload$datapath,to = "R/reactivedata/plati/cereri_plata.xlsx", overwrite = T) }
        
        else if ( tools::file_ext(input$cereri_plata_upload$datapath ) == "csv" ) {
          file.copy(from = input$cereri_plata_upload$datapath,to = "R/reactivedata/plati/cereri_plata.csv", overwrite = T)      }
        
        vals_plati$cereri_plata_database <- vals_plati$cereri_plata_read
        
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
              .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      }
      
        else if ( any(is.na( vals_plati$cereri_plata_read$DocumentId), is.na(vals_plati$cereri_plata_read$`Cod Partener`),
                is.na(vals_plati$cereri_plata_read$Data_cerere_plata), is.na(vals_plati$cereri_plata_read$Cerere_Plata)) ) {
          
          output$cereri_plata_messages <- renderPrint("Nu pot accepta date lipsa ale DocumentId, Cod Partener, 
                                                    Data_cerere_plata sau Cerere_Plata")
        }
        
        else {  output$cereri_plata_messages <- renderPrint(  c(janitor::compare_df_cols( vals_plati$cereri_plata_read,
          cereri_plata_database,return = "mismatch"), "am cereri de plata mai noi in memorie") ) }
      },
      error = function(e) { shinyFeedback::showToast(title = "STOP",message = paste0("ERROR, show it to your administrator: ",e),
                                                      type = "error",keepVisible = T)
        
        
        
        }
      )
      
    })
    
    # assemble of cereri plata database different views
    observeEvent(input$change_view_cereri_plata,{ 
       
      if (input$change_view_cereri_plata == TRUE) {
        vals_plati$cereri_plata_prelucrate <- vals_plati$cereri_plata_database %>% 
          dplyr::group_by(Anul=lubridate::year(Data_cerere_plata)) %>%
          dplyr::summarise(Cereri_Plata = sum(Cerere_Plata)) %>% dplyr::arrange(desc(Anul))
        vals_plati$round_col_cereri_plata <- 2
        vals_plati$caption_cereri_plata <- "Cereri de plata anuale"
        vals_plati$page_length_cereri_plata <- 5 }
      else {
        vals_plati$cereri_plata_prelucrate <- cereri_plata_database %>%
          dplyr::group_by(Anul=lubridate::year(Data_cerere_plata),
                          Luna=lubridate::month(Data_cerere_plata,label = T)) %>%  
          dplyr::summarise(Cereri_Plata = sum(Cerere_Plata)) %>% dplyr::arrange(desc(Anul))
        vals_plati$round_col_cereri_plata <- 3
        vals_plati$caption_cereri_plata <- paste0("Cereri plata lunare anul ", lubridate::year(vals$report_date))
        vals_plati$page_length_cereri_plata <- max(5,vals_plati$cereri_plata_prelucrate %>% 
                                                     dplyr::filter(Anul == lubridate::year(Sys.Date())) %>% nrow())
      }
    })
    
    output$cereri_plata_database <- DT::renderDataTable({ req(vals_plati$cereri_plata_prelucrate)
      DT::datatable( data = vals_plati$cereri_plata_prelucrate,rownames = F,
                     caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left;',
                                vals_plati$caption_cereri_plata ),extensions = "Buttons",
                    options = list( paging=FALSE, dom = "Bt", buttons=c("copy","excel"), scrollY = "200px") ) %>% 
        DT::formatRound(columns = vals_plati$round_col_cereri_plata,digits = 0)
    })
    
    # Assemble of tabel11
    observeEvent(vals, { req(vals$report_date, vals$view_baza_solduri, vals$previous_month,vals_plati$cereri_plata_prelucrate)
      
     previous_year_end <- lubridate::make_date(year = lubridate::year(vals$report_date)-1,month = 12,day = 31)
     
     sold_inceput_an <- vals$view_baza_solduri %>% dplyr::filter(data_raport==previous_year_end, Tip_surse == 'Surse_proprii') %>%
       dplyr::pull(Sold_garantii) %>% sum()
     
     vals_plati$tabel11_plati <- plati_database %>% dplyr::filter(lubridate::year(data_plata)==lubridate::year(vals$report_date)) %>%  
       dplyr::group_by(Anul=lubridate::year(data_plata),Luna=lubridate::month(data_plata,label = T)) %>% 
       dplyr::summarise(Plati = sum(Plata))  %>% dplyr::mutate(Plati_cumulate=cumsum(Plati)) %>%
       dplyr::arrange(desc(Luna)) %>% dplyr::filter(Luna %in% lubridate::month(c(vals$report_date, vals$previous_month),label = T)) %>%
        dplyr::select(-Plati) %>% dplyr::arrange(desc(Luna)) %>% 
        dplyr::mutate(Rata_anualizata_plati = Plati_cumulate/sold_inceput_an*12/as.numeric(Luna))
     
     vals_plati$tabel11_cereri_plata <- cereri_plata_database %>% dplyr::filter(lubridate::year(Data_cerere_plata)==lubridate::year(vals$report_date)) %>%  
       dplyr::group_by(Anul=lubridate::year(Data_cerere_plata),Luna=lubridate::month(Data_cerere_plata,label = T)) %>% 
       dplyr::summarise(Cereri_Plata = sum(Cerere_Plata))  %>% dplyr::mutate(Cereri_plata_cumulate=cumsum(Cereri_Plata)) %>% 
       dplyr::arrange(desc(Luna)) %>% dplyr::filter(Luna %in% lubridate::month(c(vals$report_date, vals$previous_month),label = T)) %>% 
       dplyr::select(-Cereri_Plata) %>% dplyr::arrange(desc(Luna)) %>% 
        dplyr::mutate(Rata_anualizata_cereri_plata = Cereri_plata_cumulate/sold_inceput_an*12/as.numeric(Luna))
     
     vals$tabel11 <-  vals_plati$tabel11_plati %>% dplyr::left_join(y = vals_plati$tabel11_cereri_plata,by = c("Anul","Luna"))
     
     output$tabel11 <- DT::renderDataTable( {req(vals$tabel11) 
       DT::datatable(data = vals$tabel11, rownames = FALSE,
          caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left;',
                    paste0("Tabelul 11 - Plati cumulate ",lubridate::year(vals$report_date) ) ),
                  options = list(dom="Bt",buttons=c("copy","excel")), extensions = "Buttons") %>% 
                  DT::formatRound(columns = c(3,5),digits = 0) %>%
                    DT::formatPercentage(columns = c(4,6),digits = 1)  })
   })
    
    
    # assemble of plati database different views
    observeEvent(input$change_view,{ 
      if (input$change_view == TRUE) {
        vals_plati$plati_prelucrate <- plati_database %>% dplyr::group_by(Anul = lubridate::year(data_plata)) %>% 
          dplyr::summarise("Plati Efectuate"=sum(Plata)) %>% dplyr::arrange(desc(Anul))
        vals_plati$round_col <- 2
        vals_plati$caption <- "Baza de date plati anuale:"
        vals_plati$page_length <- 5 }
      else {
        vals_plati$plati_prelucrate <- plati_database %>% dplyr::group_by(Anul=lubridate::year(data_plata),
            Luna=lubridate::month(data_plata,label = T)) %>%  
          dplyr::summarise(Plati = sum(Plata)) %>% dplyr::arrange(desc(Anul))
        vals_plati$round_col <- 3
        vals_plati$caption <- "Baza de date plati lunare:"
        vals_plati$page_length <- max(5,vals_plati$plati_prelucrate %>% dplyr::filter(Anul == lubridate::year(Sys.Date())) %>%
                                        nrow())
      }
        })
    
    output$plati_database <- DT::renderDataTable( { req(vals_plati$plati_prelucrate)
      DT::datatable(rownames = FALSE,  extensions = "Buttons",data = vals_plati$plati_prelucrate,
       options = list(dom = "Bt", paging = FALSE, buttons = c("copy","excel"), scrollY = "200px"),
       caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',vals_plati$caption)) %>%
        DT::formatRound(columns = vals_plati$round_col,digits = 0) } )
    
    
    output$snapshots <- DT::renderDataTable( { req(input$show_snapshots == TRUE)
      DT::datatable(rownames = FALSE, data = vals_plati$snapshots,
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',  'Current BI snapshots stored:')
        , options = list(dom = "t")) } )
    
  
    observeEvent(input$bi_upload,{
      vals_plati$snapshot_bi_plata1 <- readxl::read_excel( input$bi_upload$datapath,range = "bi!A1:B2") %>%
        dplyr::mutate(All = as.Date(All)) %>% dplyr::pull(All)
      
      vals_plati$snapshot_bi_plata2 <- readxl::read_excel(input$bi_upload$datapath,range = "bi!I1:J2") %>%
        dplyr::mutate(All = as.Date(All)) %>% dplyr::pull(All)
      
      
      if (any (vals_plati$snapshot_bi_plata1 <= bi_snapshots$snapshot_bi_plata1, 
               vals_plati$snapshot_bi_plata2 <= bi_snapshots$snapshot_bi_plata2)) {
        output$messages <- renderText("STOP, 
              nu voi prelucra si salva fisiere BI cu data snapshot mai mica sau egala decat ce am stocat")
      }
      else {
        coresp_luni_data_plata <- readRDS("R/reactivedata/plati/coresp_luni.rds")
      
        bi_upload <- reactive({
          bi_plata1 <- readxl::read_excel(input$bi_upload$datapath,range = "bi!A6:E5000") %>% dplyr::filter(!is.na(Year)) %>%
            dplyr::left_join(coresp_luni_data_plata, by = "Month")
          
          bi_plata1 <- bi_plata1 %>% dplyr::mutate(data_plata = lubridate::make_date(year = as.numeric(Year),
                month = Digit, day=1)) %>% dplyr::mutate(`Cod Finantator Generic` = 
                              ifelse(`Cod Finantator Generic` == "8479295",'INTESA',`Cod Finantator Generic`)) %>%
            dplyr::select(Banca=`Cod Finantator Generic`,Plata=Total, data_plata)
          
          
          bi_plata2 <- readxl::read_excel(input$bi_upload$datapath,range = "bi!I6:M5000") %>% dplyr::filter(!is.na(Year)) %>%
            dplyr::left_join(coresp_luni_data_plata, by = "Month")
          
          bi_plata2 <- bi_plata2 %>% dplyr::mutate(data_plata = lubridate::make_date(year = as.numeric(Year),
              month = Digit, day=1)) %>%    dplyr::select(Banca=`Cod Finantator Generic`,Plata=Total, data_plata)
          
          file.copy(from = input$bi_upload$datapath,to = "R/reactivedata/plati/BI_plati.xlsx",overwrite = TRUE)
          
          return(dplyr::bind_rows(bi_plata1, bi_plata2))        })
        
        
        output$show_save <- renderUI( { req( bi_upload() )
          actionLink(session$ns("save_bi"),"Salveaza BI uploadat", icon("save"))  })
        
        output$sinteza_upload <- DT::renderDataTable( DT::datatable(rownames = FALSE,
              data = bi_upload()  %>% dplyr::group_by(Anul=lubridate::year(data_plata)) %>% 
                dplyr::summarise(Plati = sum(Plata)) %>% dplyr::arrange(desc(Anul)),
              options = list(dom = "Btp", buttons = c("copy", "csv", "excel"),pageLength = 5),
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', 
                  'Sinteza plati uploadate:')) %>%  DT::formatRound(columns = 2, digits = 0)          )
        
        vals_plati$snapshots <- data.frame( snapshot_bi_plata1 = vals_plati$snapshot_bi_plata1, 
                                          snapshot_bi_plata2 = vals_plati$snapshot_bi_plata2 )
        
        vals_plati$plati_database <- bi_upload()
        
        vals_plati$plati_prelucrate <- bi_upload() %>% dplyr::group_by(Anul=lubridate::year(data_plata)) %>% 
          dplyr::summarise(Plati = sum(Plata)) %>% dplyr::arrange(desc(Anul))
      }    
      
    })
    
    observeEvent(input$save_bi,{
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_save"),
                                     title = "CONFIRM?", type = "info",
                                     text = paste0("Esti sigur ca vrei sa salvezi bi-ul uploadat ?"),
                                     btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"))
    })
    
    observeEvent(input$confirm_save,{ req(input$confirm_save == TRUE)
      saveRDS( vals_plati$snapshots, "R/reactivedata/plati/bi_snapshots.rds")
      saveRDS( vals_plati$plati_database, "R/reactivedata/plati/plati_database.rds")
      
      file.copy(from = input$bi_upload$datapath,to = "R/reactivedata/plati/BI_plati.xlsx",overwrite = TRUE)
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
            .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      })
    
  })
}
    
## To be copied in the UI
# mod_plati_ui("plati_ui_1")
    
## To be copied in the server
# mod_plati_server("plati_ui_1")
