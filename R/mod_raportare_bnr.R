#' raportare_bnr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_raportare_bnr_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
  bs4Dash::tabBox(width = 12,  
                  tabPanel(title = "Upload files",icon = icon("upload"),
    fluidRow(
    bs4Dash::box(title = "Upload BI acordari",width = 12,status = "primary",collapsible=T, collapsed=F,
                 maximizable = TRUE, icon = icon("file-excel"),
        footer = "Se downloadeaza modelul de BI folosind link-ul, se actualizeaza corespunzator snapshot-ul si se filtreaza
                             luna de raportare. Fisierul astfel salvat se uploadeaza folosind butonul dedicat.",
    fluidRow(      
        column(width = 6,fileInput(inputId = ns("bi_pc_input"),accept = c(".xlsx",".xls"),width = "300px",
                  label = "Upload BI acordari",buttonLabel = "Excel only",placeholder = "no file uploaded")),
        column(width = 6, br(),downloadLink(outputId = ns("link_bi_pc_acordari"), 
          label = "Downloadeaza modelul de BI acordari Prima Casa"))
    ),
    DT::dataTableOutput(ns("bi_prelucrat"))),
    
    
    bs4Dash::box(title = "Upload Sold Prima Casa",width = 12,status = "primary",collapsible=T, collapsed=F,
                 maximizable = TRUE, icon = icon("file-excel"),
          footer = "Se uploadeaza pivot-ul soldului de garantii Prima Casa folosind link-ul furnizat.
          Pivotul se construieste pornind de la baza de date a soldului PC, 
          Cod Finantator pe rows, TipGarantie pe rows, soldul creditului, a garantiei si numarul de contracte in 
          values, display pe classic, repeat all item labels.",    
    fluidRow(
      column(width = 6, fileInput(inputId = ns("sold_pc_pivot_input"),accept = c(".xlsx",".xls"),width = "300px",
        label = "Upload pivot sold PC",buttonLabel = "Excel only",placeholder = "no file uploaded")),
      
      column( width = 6,  br(),  br(), shinyWidgets::prettyToggle(
                inputId = ns("show_pivot"),
                label_off = "Vezi modelul de pivot",
                label_on = "Ascunde modelul de pivot",
                icon_off = icon("table"),
                icon_on = icon("eye-slash"),
                value = FALSE,
                status_off = "primary",
                status_on = "warning",
                shape = "curve",
                outline =  TRUE,
                fill = TRUE,
                plain = TRUE,
                animation = "rotate") ),
      
      
    
      column(width = 12,  DT::dataTableOutput(outputId = ns("sold_pc_prelucrat")), br() ), 
    
   
    column(width = 6, br(), uiOutput(outputId = ns("show_save_pc_sold"))),
    
    column(width = 6, uiOutput(ns("show_save_date"))),
    
    column(width = 12,
    
    br(),
    
    DT::dataTableOutput(outputId = ns("model_sold_pc") ) )
    )
    )
    )
    ),
    
    tabPanel(title = "Corespondenta Banci",icon = icon("check-circle"),
             DT::dataTableOutput(ns("coresp_banci_bi")), 
             hr(),
             DT::dataTableOutput(ns("coresp_banci_sold")))
  )
  )
  
}
    
#' raportare_bnr Server Function
#'
#' @noRd 
mod_raportare_bnr_server <- function(input, output, session, vals){
  
  ns <- session$ns
  
  coresp_banci <- readRDS("R/reactivedata/pc/coresp_banci_sold_pc.rds")
  coresp_banci_bi <- readRDS("R/reactivedata/pc/coresp_banci_bi_pc.rds")
  
  nume_obligatorii_sold <- c("Cod Finantator", "TipGarantie", "Sum of Sold Credit (Lei)", "Sum of Sold Garantie (Lei)",
                             "Count of Cod Partener")
  
  sold_reactiv <- reactiveValues(nume_obligatorii=nume_obligatorii_sold, ok_save = FALSE)
  
  output$coresp_banci_bi <- DT::renderDataTable(  DT::datatable(data = coresp_banci_bi,rownames = FALSE,
        extensions = "Buttons", caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left;',"Corespondenta banci raportare BI garantii acordate PC:" ),
      options = list(dom = "Bft",buttons = c("copy","excel"),paging = FALSE, scrollY = "300px") ) )
  
  output$coresp_banci_sold <- DT::renderDataTable( DT::datatable( data = coresp_banci,rownames = FALSE,
        extensions = "Buttons", caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left;',"Corespondenta banci raportare sold PC catre BNR:") ,
        options = list(dom = "Bft",buttons = c("copy","excel"),paging = FALSE, scrollY = "300px") ) )
  
  output$link_bi_pc_acordari <- downloadHandler(filename = "bi_acordari_pc.xlsx",content = function(file) {
    file.copy(from = "R/reactivedata/pc/bi_acordari.xlsx",to = file)   })
  
 
  
  output$model_sold_pc <-  DT::renderDataTable( { req(input$show_pivot == TRUE)
      DT::datatable( data = data.frame( Rows = c("Cod Finantator", "Tip Garantie",""),
      Columns = c("", "",""), Values = c(  "Sum of Sold Credit (Lei)",    "Sum of Sold Garantie (Lei)",
        "Count of Cod Partener" ) ), caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left;',
        "Pivotul Prima Casa se construieste folosind modelul de mai jos:"),
      rownames = FALSE, options = list(dom = "t"))  } )
  
  
  sketch = htmltools::withTags(table(class = 'display',
    thead(tr(
        th(rowspan = 2, 'Finantator'),
        th(colspan = 3, 'Contract de garantare'),
        th(colspan = 3, 'Promisiune de garantare')),
      tr(
        lapply(c('Sold credit (LEI)','Sold garantie (LEI)','Numar contracte',
                 'Sold credit (LEI)','Sold garantie (LEI)','Numar promisiuni'), th) ) ) ))
  
  sketch_bi = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Finantator'),
        th(colspan = 3, 'Contract de garantare'),
        th(colspan = 3, 'Promisiune de garantare')
      ),
      tr(lapply(c('Numar Solicitari','Finantare RON','Garantie RON',
                 'Numar Solicitari','Finantare RON','Garantie RON'), th)))))
  
  observeEvent( input$pc_date, {  
    sold_reactiv$pc_date <- lubridate::`%m+%`(input$pc_date,months(1) ) -1  }) 
 
  observeEvent(input$save_pc, {
    shinyWidgets::ask_confirmation(ns("confirm_save"),
        text = "Esti sigur ca vrei sa salvezi datele uploadate? Garantiile nu vor fi salvate detaliat.",
        btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info")
  } )
  
  observeEvent(input$confirm_save,{ req(input$confirm_save == TRUE)
    
    sold_reactiv$new_pc_sold <- data.frame( Tip_surse = "Nume_cont_stat", "Tip fonduri" = "Prima Casa",check.names = FALSE,
          Nr_contracte = sum(sold_reactiv$fisier_prelucrat$`Count of Cod Partener_CG`) + 
                sum(sold_reactiv$fisier_prelucrat$`Count of Cod Partener_PG`),
          Sold_garantii = sum(sold_reactiv$fisier_prelucrat$`Sum of Sold Garantie (Lei)_CG`) + 
                      sum(sold_reactiv$fisier_prelucrat$`Sum of Sold Garantie (Lei)_PG`), 
          data_raport = sold_reactiv$pc_date,
          Sold_credite_garantate = sum(sold_reactiv$fisier_prelucrat$`Sum of Sold Credit (Lei)_CG`) + 
            sum(sold_reactiv$fisier_prelucrat$`Sum of Sold Credit (Lei)_PG`)) %>% 
      dplyr::mutate(Nr_beneficiari = Nr_contracte) %>%
          dplyr::mutate( dplyr::across(.cols = dplyr::contains("Sold"), ~as.numeric(.x))) %>% 
              dplyr::mutate( dplyr::across(.cols = dplyr::contains("Nr_"), ~as.integer(.x)))
    
   sold_reactiv$ok_save <- janitor::compare_df_cols_same( sold_reactiv$new_pc_sold ,vals$view_baza_solduri)
   
   if ( sold_reactiv$ok_save ) {
     
     vals$view_baza_solduri <- dplyr::bind_rows( sold_reactiv$new_pc_sold, vals$view_baza_solduri %>% 
          dplyr::mutate(temp_column = ifelse(`Tip fonduri` == "Prima Casa" & 
               data_raport == input$pc_date,1,0)) %>% dplyr::filter(temp_column==0) %>% 
                                                dplyr::select(-temp_column))
    
     saveRDS(object = vals$view_baza_solduri, file = "R/reactivedata/solduri/view_baza_sold.rds")
     
     shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
          .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
     
     sold_reactiv$ok_save <- FALSE
   }
   else { shinyFeedback::showToast(type = "error",title = "ERROR",message = "Failed to save", keepVisible = F) }
  } )
  
  observeEvent( input$sold_pc_pivot_input,{
    
    sold_reactiv$file_input <-  input$sold_pc_pivot_input$datapath
   
    
    mod_read_excel_server("read_excel_ui_1",excel_reactive=sold_reactiv)
    
    output$show_save_pc_sold <- renderUI( { req(sold_reactiv$fisier_prelucrat )
      shinyWidgets::actionBttn(inputId = session$ns("save_pc"),color = "primary",
                               label = "Save Prima Casa above",icon = icon("save"),style = "stretch") })
    
    output$show_save_date <- renderUI( { req(sold_reactiv$fisier_prelucrat )
      shinyWidgets::airMonthpickerInput(inputId = ns("pc_date"),label = "Select Prima Casa save date",width = "300px",
          value = vals$report_date, language = "ro", autoClose = TRUE, position = "top right") })
     
     
    observe({ req( sold_reactiv$all_names==TRUE )
      sold_reactiv$fisier_prelucrat <- sold_reactiv$file_read_prel %>% 
        dplyr::filter(`Cod Finantator` != "Grand Total") %>% 
        dplyr::left_join(coresp_banci, by = c("Cod Finantator" = "Banca_Generat_Charisma")) %>% 
        dplyr::select(-`Cod Finantator`) %>%
        tidyr::pivot_wider(names_from = TipGarantie, values_from = c(`Sum of Sold Credit (Lei)`,
            `Sum of Sold Garantie (Lei)`,`Count of Cod Partener`),values_fn = sum,values_fill=0) %>% 
        dplyr::select(-dplyr::contains("NA")) %>% 
        dplyr::group_by(Banca_Raport_BNR) %>% dplyr::summarise_all(.funs = ~sum(.)) %>% 
        dplyr::rename_at(.vars = 1,.funs = ~c("Finantator")) %>%
        dplyr::select(Finantator,dplyr::contains("CG"),dplyr::contains("PG")) %>%
        dplyr::arrange(desc(`Sum of Sold Credit (Lei)_CG`)) 
        #janitor::adorn_totals(where = "row",na.rm = TRUE,name = "Total") 
     
       output$sold_pc_prelucrat <- DT::renderDataTable( { req(sold_reactiv$fisier_prelucrat)
        DT::datatable(data = sold_reactiv$fisier_prelucrat,rownames = FALSE,
            options = list( dom = "Bft", paging = FALSE, scrollY = "300px",
                  buttons=c("copy","excel") ), container = sketch,extensions = "Buttons",
            caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left;',
                              "Sold Prima Casa:") ) %>% DT::formatRound(columns = 2:7,digits = 0) 
        } )
      
      })
    
  } )
    
  observeEvent(input$bi_pc_input,{
    
    nume_obligatorii_bi <- c("Nume Banca", "TipDocument", "Numar Solicitari", "Finantare RON","Garantie RON")
    
    bi_reactiv <- reactiveValues(nume_obligatorii=nume_obligatorii_bi)
    
    bi_reactiv$file_input <-  input$bi_pc_input$datapath
    
    mod_read_excel_server("read_excel_ui_1", excel_reactive = bi_reactiv)
   
    observe({ req(bi_reactiv$all_names==TRUE)
      bi_reactiv$fisier_prelucrat <- bi_reactiv$file_read_prel %>% dplyr::filter(`Nume Banca` != "Grand Total") %>% 
        dplyr::left_join(coresp_banci_bi, by = c("Nume Banca" = "Banca_BI")) %>% 
        dplyr::select(-`Nume Banca`) %>%
        dplyr::mutate(TipDocument = ifelse(stringr::str_detect(TipDocument,pattern = "Promisiune"),
                                           "PG","CG")) %>%
        tidyr::pivot_wider(names_from = TipDocument, values_from = c(`Numar Solicitari`,`Finantare RON`,
              `Garantie RON`),values_fn = sum,values_fill=0) %>% 
        dplyr::select(-dplyr::contains("_NA")) %>% 
        dplyr::group_by(Banca_Raport_BNR) %>% dplyr::summarise_all(.funs = ~sum(.)) %>% 
        dplyr::rename_at(.vars = 1,.funs = ~c("Finantator")) %>%
        dplyr::select(Finantator,dplyr::contains("CG"),dplyr::contains("PG")) %>% 
        dplyr::arrange(desc(`Garantie RON_CG`)) %>%
        janitor::adorn_totals(where = "row",na.rm = TRUE,name = "Total") 
      
      output$bi_prelucrat <- DT::renderDataTable({req(bi_reactiv$fisier_prelucrat)
        DT::datatable( data = bi_reactiv$fisier_prelucrat, rownames = FALSE,
                      options = list(dom = "Bt",paging = FALSE, scrollY = "300px",
              buttons=c("copy","excel") ), container = sketch_bi,extensions = "Buttons",
                      caption =  htmltools::tags$caption(  style = 'caption-side: top; text-align: left;',
                          "Garantii acordate Prima Casa") )  %>%
                    DT::formatRound(columns = 2:7,digits = 0)  })
      
      file.copy( from = input$bi_pc_input$datapath, to = "R/reactivedata/pc/bi_acordari.xlsx",overwrite = TRUE)  
      })
    
  } )
    
  
                                

 
}
    
## To be copied in the UI
# mod_raportare_bnr_ui("raportare_bnr_ui_1")
    
## To be copied in the server
# callModule(mod_raportare_bnr_server, "raportare_bnr_ui_1")
 
