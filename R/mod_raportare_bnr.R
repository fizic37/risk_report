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
  
  bs4Dash::tabsetPanel( id = ns("panel_bnr"),
    selected = "upload", type = "pills",  side = "right",
    tabPanel( title = "Upload files",
      icon = icon("upload"),  value = "upload",
      
      tagList(    br(),
        
        bs4Dash::box(
          title = "Upload BI acordari",
          width = 12,   status = "info",
          footer = "Se downloadeaza modelul de BI folosind link-ul, se actualizeaza corespunzator snapshot-ul si se filtreaza
                             luna de raportare. Fisierul astfel salvat se uploadeaza folosind butonul dedicat.",
          fluidRow(
            column(
              width = 6,
              fileInput(
                inputId = ns("bi_pc_input"),
                accept = c(".xlsx", ".xls"),
                width = "300px",
                label = "Upload BI acordari",
                buttonLabel = "Excel only",
                placeholder = "no file uploaded"
              )
            ),
            column(
              width = 6,
              br(),
              downloadLink(
                outputId = ns("link_bi_pc_acordari"),
                label = "Downloadeaza modelul de BI acordari Prima Casa"
              )
            )
          ),
          DT::dataTableOutput(ns("bi_prelucrat"))
        ),
        
        bs4Dash::box(
          title = "Upload Sold Prima Casa",
          width = 12,
          status = "info",
          footer = "Se uploadeaza pivot-ul soldului de garantii Prima Casa folosind link-ul furnizat.
          Pivotul se construieste pornind de la baza de date a soldului PC,
          Cod Finantator pe rows, TipGarantie pe rows, soldul creditului, a garantiei si numarul de contracte in
          values, display pe classic, repeat all item labels.",
          
          fluidRow(
            br(),
            column(
              width = 6,
              fileInput(
                inputId = ns("sold_pc_pivot_input"),
                accept = c(".xlsx", ".xls"),
                width = "300px",
                label = "Upload pivot sold PC",
                buttonLabel = "Excel only",
                placeholder = "no file uploaded"
              )
            ),
            column(
              width = 6,
              br(),
              downloadLink(ns("link_pivot_sold"),
                           label = "Click aici pentru a downloada modelul de pivot")
            )
          ),
          
          DT::dataTableOutput(outputId = ns("sold_pc_prelucrat"))
        )
      )
    ),
    
    tabPanel(
      title = "Corespondenta Banci",
      icon = icon("check-circle"), br(),
      DT::dataTableOutput(ns("coresp_banci_bi")),
      hr(),
      DT::dataTableOutput(ns("coresp_banci_sold"))
    )
  )
  
}
    
#' raportare_bnr Server Function
#'
#' @noRd 
mod_raportare_bnr_server <- function(input, output, session){
  
  ns <- session$ns
 
  coresp_banci_bi <- readRDS("R/reactivedata/pc/coresp_banci_bi_pc.rds")
  
  coresp_banci_sold <- readRDS("R/reactivedata/pc/coresp_banci_sold_pc.rds")
  
  
  output$coresp_banci_bi <- DT::renderDataTable({DT::datatable(data = coresp_banci_bi,rownames = FALSE,
        extensions = "Buttons", caption = "Corespondenta banci raportare BI garantii acordate PC:",
      options = list(dom = "Bftp",buttons = c("excel","csv"), pageLength=7)) })
  
  output$coresp_banci_sold <- DT::renderDataTable({DT::datatable(data = coresp_banci_sold,rownames = FALSE,
        extensions = "Buttons", caption = "Corespondenta banci raportare sold PC catre BNR:",
                options = list(dom = "Bftp", buttons = c("excel", "csv"), pageLength = 7)) })
  
  output$link_bi_pc_acordari <- downloadHandler(filename = "bi_acordari_pc.xlsx",content = function(file) {
    file.copy(from = "R/reactivedata/pc/bi_acordari.xlsx",to = file)   })
  
  output$link_pivot_sold <- downloadHandler(filename = "sold_pc.xlsx",content = function(file) {
    file.copy(from = "R/reactivedata/pc/pivot_sold.xlsx",to = file)   })
  
  
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
  
 
  
  observeEvent(input$sold_pc_pivot_input,{
    
    nume_obligatorii_sold <- c("Cod Finantator", "TipGarantie", "Sum of Sold Credit (Lei)", "Sum of Sold Garantie (Lei)",
                               "Count of Cod Partener")
    
    sold_reactiv <- reactiveValues( nume_obligatorii = nume_obligatorii_sold )
    
    sold_reactiv$file_input <-  input$sold_pc_pivot_input$datapath
    

   mod_read_excel_server("read_excel_ui_1", excel_reactive = sold_reactiv)

    observe({req(sold_reactiv$all_names==TRUE)
      sold_reactiv$fisier_prelucrat <- sold_reactiv$file_read_prel %>% 
        dplyr::filter(`Cod Finantator` != "Grand Total") %>% 
        dplyr::left_join(coresp_banci_sold, by = c("Cod Finantator" = "Banca_Generat_Charisma")) %>% 
        dplyr::select(-`Cod Finantator`) %>%
        tidyr::pivot_wider(names_from = TipGarantie, values_from = c(`Sum of Sold Credit (Lei)`,
                                                                     `Sum of Sold Garantie (Lei)`,`Count of Cod Partener`),values_fn = sum,values_fill=0) %>% 
        dplyr::select(-dplyr::contains("NA")) %>% 
        dplyr::group_by(Banca_Raport_BNR) %>% dplyr::summarise_all(.funs = ~sum(.)) %>% 
        dplyr::rename_at(.vars = 1,.funs = ~c("Finantator")) %>%
        dplyr::select(Finantator,dplyr::contains("CG"),dplyr::contains("PG")) %>%
        dplyr::arrange(desc(`Sum of Sold Credit (Lei)_CG`)) %>%
        janitor::adorn_totals(where = "row",na.rm = TRUE,name = "Total") 
      
     # browser()
      output$sold_pc_prelucrat <- DT::renderDataTable( { req( sold_reactiv$fisier_prelucrat )
        dt_generate_function(df = sold_reactiv$fisier_prelucrat, pageLength = (nrow(sold_reactiv$fisier_prelucrat)+1),
              container = sketch,caption = "Sold Prima Casa", show_buttons = TRUE, round_col=2:7) })
      
    })
    
  
    
   })
  
  observeEvent(input$bi_pc_input,{
    
    nume_obligatorii_bi <- c("Nume Banca", "TipDocument", "Numar Solicitari", "Finantare RON","Garantie RON")
    
    bi_reactiv <- reactiveValues( nume_obligatorii=nume_obligatorii_bi )
    
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
        DT::datatable(data = bi_reactiv$fisier_prelucrat,rownames = FALSE,
                      options = list(dom = "Bt",pageLength=(nrow(bi_reactiv$fisier_prelucrat)+1),
              buttons=c("excel","csv")), container = sketch_bi,extensions = "Buttons",
                      caption = "Garantii acordate Prima Casa") %>% DT::formatRound(columns = 2:7,digits = 0)  })
      
      file.copy(from = bi_reactiv$file_input, to = "R/reactivedata/pc/bi_acordari.xlsx",overwrite = TRUE)
      
      })
    
  })
    
  
                                

 
}
    
## To be copied in the UI
# mod_raportare_bnr_ui("raportare_bnr_ui_1")
    
## To be copied in the server
# callModule(mod_raportare_bnr_server, "raportare_bnr_ui_1")
 
