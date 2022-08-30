#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_ui <- function(id){
  ns <- NS(id)
  bs4Dash::tabsetPanel(id = ns("admin"),  selected = T,
  
  shiny::tabPanel(title = "Uploads - diverse",value = "uploads",icon = icon("file-upload"),
                  
                  bs4Dash::box( title="Upload modelul de raport final",
                                status = "info",width = 12, collapsible = T, collapsed = T,
                                maximizable = TRUE, icon = icon("file-arrow-up"),
                                footer = "Atentie, nu voi prelucra upload-ul tau. 
                  Doar il voi salva si voi incerca sa scriu in el info updatate.",
                                
                      fluidRow(column(width = 3, fileInput(inputId = ns("upload_doc"),
                                     label = "Uploadeaza modelul de raport final",accept = c(".docx"),
                                   placeholder = "nothing uploaded",buttonLabel = "docx only")),
                                         
                               column(width = 8, div(style = "padding-top: 24px; padding-left: 190px;",
                                     shinyWidgets::downloadBttn(outputId = ns("down_doc"),
                                    label = "Downloadeaza modelul existent de raport",
                                      style = "stretch",color = "primary")))) )
  ),
                       
  shiny::tabPanel(title = "Banci",value = "banci", icon = icon("university"), 
                  shinyFeedback::useShinyFeedback(),
     fluidPage(
            br(),
            fluidRow(
            bs4Dash::box( title = "Lista banci fuziuni",status = "primary",collapsible = T,collapsed = T,maximizable = T,
                         footer = "Click pe o banca din tabel si editeaza data si noul finantator cu care a fuzionat",
                         width = 8, icon = icon("object-ungroup"),
                         DT::dataTableOutput(ns("lista_banci")) ),
            
            bs4Dash::box(title = "Update banci din BI",status = "primary",collapsible = T,collapsed = T,maximizable = T,
                         width = 4, icon = icon("file-excel"),
                         footer = "Se downloadeaza modelul de BI folosind link-ul Model BI, se modifica data snapshot, se 
                         salveaza local fisierul si se uploadeaza. Ulterior voi verifica daca sunt banci noi si te voi intreba 
                         daca doresti sa actualizez",
                  fluidRow(
                    column(width = 8, fileInput(inputId = ns("upload_bi_banci"),accept = ".xlsx",label = "BI Banci",
                                                placeholder = "Nothing uploaded",buttonLabel = "Excel")),
                    column(width = 4, br(),br(), downloadLink(ns("link_bi_banci"),label = "Model BI")),
                    
                    column(width = 12, DT::dataTableOutput(ns("diferente_bi_banci")), 
                           uiOutput(outputId = ns("show_save_diferente_bi")))
                    
                  )  ) ),
            
            bs4Dash::box( title = "Conturi contabile - corespondenta banci",status = "primary",collapsible = T,collapsed = F,
                          maximizable = T,  width = 12, icon = icon("hand-point-right"),
                          fluidRow(column(width = 6,
                          DT::dataTableOutput(ns("conturi_curente"))),
                          column(width = 6, DT::dataTableOutput(ns("conturi_depozite"))) ) )
            
  )
  )
  
)
}
    
#' admin Server Functions
#'
#' @noRd 
mod_admin_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tabela_nume_banci <- readRDS(file = "R/reactivedata/banci/tabela_nume_banci.rds")
    
    conturi_curente <- readRDS(file = "R/reactivedata/balanta/coresp_banci_curente.rds")
    
    conturi_depozite <- readRDS(file = "R/reactivedata/balanta/coresp_banci_depozite.rds") %>%
        dplyr::filter(Banca != "")
    
    vals_admin <- reactiveValues(tabela_nume_banci = tabela_nume_banci, conturi_curente = conturi_curente,
                                 conturi_depozite = conturi_depozite )
    
    updateTabsetPanel(session = session, inputId = 'admin',selected = "banci")
    
   
    output$conturi_curente <- DT::renderDataTable(
     DT::datatable(data= vals_admin$conturi_curente %>% dplyr::mutate(dplyr::across(.cols = dplyr::everything(), ~as.factor(.x))),
                   rownames = FALSE, extensions = "Buttons", 
                   caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                        "Lista conturilor curente"), 
                   options = list(paging = FALSE, scrollY = "300px",dom = "Btfip", buttons= c("copy","excel"))))
    
    observeEvent( input$upload_doc,{ shiny::validate( shiny::need(expr = 
      tools::file_ext(input$upload_doc$datapath) == "docx", message = "Docx only" ) )
      file.copy(from = input$upload_doc$datapath, to = "template_prudentialitate.docx",overwrite = TRUE)
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Am salvat cu succes modelul de raport final",
      .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE) )
    })
    
    output$down_doc <- downloadHandler( filename = function() {"template_prudentialitate.docx"},
          content = function(file) {file.copy(from = "template_prudentialitate.docx", to = file, overwrite = TRUE ) } )
    
    output$conturi_depozite <- DT::renderDataTable( DT::datatable(data = vals_admin$conturi_depozite, rownames = FALSE,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                "Lista conturilor aferente depozitelor bancare"),  
            options = list(paging = FALSE, scrollY = "300px",dom = "Btfip", buttons= c("copy","excel"))))
    
    observeEvent(input$admin,{ vals$admin_selected_tab <- input$admin  } )
    
   
    output$lista_banci <- DT::renderDataTable( DT::datatable(data = vals_admin$tabela_nume_banci,
      selection = list(mode = "single",selected = NULL, target = "row"),rownames = FALSE, extensions = "Buttons",
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                    "Lista bancilor"), options = list(paging = FALSE, scrollY = "300px",dom = "Btfip",
                                            buttons = c("copy","excel"))) )
   
    output$link_bi_banci <- downloadHandler(filename = function() { "BI_banci.xlsx" }, content = function(file) {
      file.copy( from = "R/reactivedata/banci/BI_banci.xlsx" , to = file ) } )
    
    
    observeEvent(input$upload_bi_banci,{
      
      vals_admin$bi_banci <- readxl::read_excel(input$upload_bi_banci$datapath,skip = 4) %>% 
        dplyr::filter(`Unique ID` != "Nespecificat") %>% dplyr::select(FinantatorID = `Unique ID`,
            CodFinantator = `Cod Finantator Generic`, DenumireFinantator = `Denumire Finantator Generic`)
      
      
      vals_admin$bi_diferente <- vals_admin$bi_banci[which(is.na(match(vals_admin$bi_banci$FinantatorID,
                                      table = vals_admin$tabela_nume_banci$FinantatorID))),]
      
    
      if ( nrow(vals_admin$bi_diferente) == 0 ) {
        shinyFeedback::showToast(type = "success",title = "SUCCES! No need for further action",
        message = "Baza de date a bancilor este sincronizata cu BI-ul furnizat",keepVisible = TRUE, session = session)
      } else if ( nrow(vals_admin$bi_diferente) > 0) {
        
        output$diferente_bi_banci <- DT::renderDataTable(DT::datatable(data = vals_admin$bi_diferente,
              rownames = FALSE, options = list(dom="t"), caption = "STOP, in BI-ul furnizat am identificat bancile noi de mai jos. 
              Vrei sa le salvez in baza de date?"))
        
        output$show_save_diferente_bi <- renderUI( actionLink(inputId = ns("save_diferente_bi"),
                              label = "Salveaza diferentele de mai sus in baza de date",icon = icon('save')))
      }
      
    })
    
    observeEvent(input$lista_banci_rows_selected,{
      
      vals_admin$banca_selectata <- vals_admin$tabela_nume_banci  %>%   dplyr::slice(input$lista_banci_rows_selected)
      
      showModal(session = session,modalDialog(title = paste("Editeaza banca ",vals_admin$banca_selectata$DenumireFinantator),
                                              size = "l",   
          footer = list(actionButton(inputId =  ns("save_banca"),label = "Save", icon = icon("save")),
                                                            modalButton(label = "Close",icon = icon("times")) ),
          fluidRow(column(width = 6, shinyWidgets::airDatepickerInput(inputId = session$ns("data_fuziune"),
                                  value =  Sys.Date(), language = "ro",autoClose = TRUE,
                                   label = "Selecteaza data de fuziune a bancii") ),
                  column(width = 6, shinyWidgets::pickerInput(inputId = session$ns("banca_absorbanta"),
                                  choices = unique(vals_admin$tabela_nume_banci$DenumireFinantator), options = list(`live-search` = TRUE),
                                  label = "Selecteaza noul finantator absorbant") )
          )
      ) )
      
      
    })
    
    observeEvent(input$save_banca, {
      shinyWidgets::ask_confirmation(inputId = session$ns("confirm_banca"),title = 'CONFIRM',
                                     btn_colors = c("#ff007b","#00ff84"), type = "success",
                                     text = "Esti sigur ca vrei sa salvezi modificarile efectuate?") 
    })
    
    observeEvent(input$confirm_banca,{req(input$confirm_banca == TRUE ) 
      vals_admin$df_new <- data.frame(FinantatorID = vals_admin$banca_selectata$FinantatorID,
                                      CodFinantator = vals_admin$banca_selectata$CodFinantator,
                                      DenumireFinantator = input$banca_absorbanta,
                                      DataInitiala = input$data_fuziune,
                                      DataExpirare = as.Date("2100-01-01")   )
      
      vals_admin$tabela_nume_banci$DataExpirare[vals_admin$tabela_nume_banci$FinantatorID==vals_admin$banca_selectata$FinantatorID] <- input$data_fuziune
      
      vals_admin$tabela_nume_banci <- dplyr::bind_rows(vals_admin$tabela_nume_banci, vals_admin$df_new)
      
      saveRDS(object = vals_admin$tabela_nume_banci,file = "R/reactivedata/banci/tabela_nume_banci.rds")
      
      removeModal(session = session)
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Successfully saved",
                               .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
      
      })
    
    observeEvent(input$save_diferente_bi, {
      vals_admin$bi_diferente_final <- vals_admin$bi_diferente %>% dplyr::mutate(DataInitiala = as.Date("1999-01-01"),
                                                                      DataExpirare = as.Date("2100-01-01"))
      
      if (janitor::compare_df_cols_same(vals_admin$bi_diferente_final, vals_admin$tabela_nume_banci)) {
        
        vals_admin$tabela_nume_banci <- dplyr::bind_rows(vals_admin$bi_diferente_final, vals_admin$tabela_nume_banci)
        
        saveRDS(object = vals_admin$tabela_nume_banci,file = "R/reactivedata/banci/tabela_nume_banci.rds")
        
        file.copy(from = input$upload_bi_banci$datapath,to = "R/reactivedata/banci/BI_banci.xlsx",overwrite = TRUE)
        
        shinyFeedback::showToast(type = "success",message = "Saved to banci database",title = "Success",keepVisible = T)
      } else {shinyFeedback::showToast(type = "error",message = "NU am salvat, spune-i administratorului aplicatiei",
                        title = "Success",keepVisible = T) }
    })
      
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_admin_ui("admin_ui_1")
    
## To be copied in the server
# mod_admin_server("admin_ui_1")
