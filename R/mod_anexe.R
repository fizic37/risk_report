#' anexe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_anexe_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(  color = "#ff007b",    position = "bottom-right",    timeout = 200 ),
    
    bs4Dash::box(title = "Top 10 expuneri surse proprii", icon = icon("chart-bar"),width = 12,
                 status = "primary",collapsible = TRUE,collapsed = T,
                 downloadButton(ns("down_top_expuneri"),"Download top 10 expuneri"),
                 DT::dataTableOutput(ns("top_expuneri"))),
    
    bs4Dash::box(title = "Anexa A - Banci, solduri, clase de risc", icon = icon("university"),width = 12,
                 status = "primary",collapsible = TRUE,collapsed = FALSE,
                 fluidRow(column(width = 4,
                 shinyWidgets::autonumericInput(ns("cap_proprii"),value = 0,align = "right",width = "300px",
                                         label = "Input fonduri proprii minus subordonate",modifyValueOnWheel = FALSE,
                                         digitGroupSeparator = ",",decimalPlaces = 0) ),
                 
                 column(width = 4,br(), div(style="display:inline-block;margin-left: 40%;padding-top: 27px;",
                                  downloadLink(ns("down_anexaA"),"Download Anexa A")) ),
                 
                 column(width = 4, br(),div(style="display:inline-block;margin-left: 40%;padding-top: 27px;",
                                actionLink(ns("show_clase"),"View clasele de Risc",icon=icon("table"))) ),
                    
                 column(width = 12,  DT::dataTableOutput(ns("anexa_A"))),
                  column(width = 12, shinyWidgets::prettyToggle(inputId = ns("show_rata_platilor"),
                              status_on = "danger", icon_on = icon("wye-slash"), icon_off = icon("table"),
                              value = FALSE,status_off = "primary",label_on = "Hide rata platilor",
                                label_off = "Click to show rata platilor pe banci")),
                          column(width = 12, DT::dataTableOutput(ns("rata_platilor")))
                 )
           ),
    
    bs4Dash::box(title = "Anexa B - Depozite bancare, clase risc", icon = icon("money-bill-alt"), width = 12,
                 status = "primary", collapsible = T,collapsed = F,
                  DT::dataTableOutput(ns("anexaB")) )
    
      )
}
    
#' anexe Server Functions
#'
#' @noRd 
mod_anexe_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    vals_anexe <- reactiveValues()
    
    tabela_nume_banci <- readRDS("R/reactivedata/banci/tabela_nume_banci.rds") %>% 
      dplyr::filter(DataInitiala <= vals$report_date & DataExpirare >= vals$report_date) %>% dplyr::select(2:3)
    
    observeEvent(input$show_clase,{ req(vals_anexe$clase_risc)
      
      showModal(modalDialog(title = "Clase de Risc Finantatori - neajustate pentru rata platilor", size = "l", 
                            DT::dataTableOutput(session$ns("clase_risc"))  ))
      
      output$clase_risc <- DT::renderDataTable( DT::datatable(data = vals_anexe$clase_risc %>% dplyr::select(1:3) %>%
                dplyr::mutate(dplyr::across(dplyr::everything(),~as.factor(.x))), extensions = "Buttons",rownames = F,
          options = list(dom = "Bftip", paging = FALSE, scrollY="400px", buttons = c("copy","excel","csv")) ) )
    })
    
    observeEvent(vals$report_date, {
      #Calculate Rata Platilor pe Banci
      
      vals_anexe$begining_year <- lubridate::make_date(lubridate::year(vals$previous_year),12,31)
      
      vals_anexe$cap_proprii <- readRDS("R/reactivedata/solduri/baza_plafoane.rds") %>% 
        dplyr::filter(data_raport == vals$report_date) %>% dplyr::slice(1) %>%
        dplyr::mutate(new_col = Fonduri_proprii-Impr_subordon) %>% dplyr::pull(new_col)
      
      shinyWidgets::updateAutonumericInput(session = session,inputId = 'cap_proprii',
                                           value = vals_anexe$cap_proprii)
      
      #if (length(vals_anexe$cap_proprii == 0)) ( shinyjs::disable(id = "cap_proprii") )
      
     
      
      baza_plati <- readRDS("R/reactivedata/plati/bi_plati.rds") %>%
      dplyr::filter(data_plata <= vals$report_date & data_plata >  vals_anexe$begining_year) %>%
        dplyr::left_join(tabela_nume_banci, by = c("Banca" = "CodFinantator")) %>%
        dplyr::group_by(Banca = DenumireFinantator) %>% dplyr::summarise(Plati = sum(Plata))
      
      # Else clause is legacy code. Since July 31st 2022 I only use baza_date_rating.rds
      if ( vals$report_date >= as.Date("2022-07-31") ) {
        vals_anexe$clase_risc_prelucrate <- readRDS("R/reactivedata/banci/baza_date_rating.rds") %>%
          dplyr::select(CodFinantator, DenumireFinantator,ClasaRisc=Clasa_Risc, 
                        LimitaTrezorerie=Limita_Banca, DataInitiala) 
        
        vals_anexe$unique_dates <- unique(vals_anexe$clase_risc_prelucrate$DataInitiala) 
        
        vals_anexe$rating_date <- max(vals_anexe$unique_dates[which(vals_anexe$unique_dates <= vals$report_date)])
        
        vals_anexe$clase_risc <-   vals_anexe$clase_risc_prelucrate %>% 
          dplyr::filter( DataInitiala == vals_anexe$rating_date ) %>%
          dplyr::mutate(PlafonProcentual = ifelse(ClasaRisc=="A",0.3,ifelse(ClasaRisc=="B",0.23,
                    ifelse(ClasaRisc=="C",0.18,ifelse(ClasaRisc=="D",0.13,NA_real_)))))
        
        vals_anexe$caption_anexa_b <- paste0("Anexa B la data de ", vals$report_date, 
                " Data la care sunt extrase limitele de expunere este ", vals_anexe$rating_date)
              }  else { 
                vals_anexe$clase_risc <- readRDS("R/reactivedata/banci/sinteza_limite.rds") %>%
        dplyr::filter(DataInitiala <= vals$report_date &   DataExpirare >= vals$report_date  ) }
      
    
   
    vals_anexe$solduri_begining_year <- readRDS("R/reactivedata/solduri/baza_banci.rds") %>% 
      dplyr::filter(data_raport==vals_anexe$begining_year, Tip_surse == "Surse_proprii") %>% 
      dplyr::left_join(tabela_nume_banci, by = c("Banca" = "CodFinantator")) %>%
      dplyr::group_by(Banca = DenumireFinantator) %>%
      dplyr::summarise(Sold_Garantii=sum(`Soldul garantiei [in LEI]`))
    
    vals_anexe$rata_anualizata_plati <- sum(baza_plati$Plati)/sum(vals_anexe$solduri_begining_year$Sold_Garantii) * 12/
      lubridate::month(vals$report_date)
    
   
    vals_anexe$rata_platilor <- vals_anexe$solduri_begining_year %>% dplyr::group_by(Banca) %>%
      dplyr::summarise(Sold_Garantii = sum(Sold_Garantii)) %>%
      dplyr::left_join(baza_plati, by = "Banca") %>% dplyr::mutate(Plati = ifelse(is.na(Plati), 0, Plati)) %>%
      janitor::adorn_totals(where = "row",  fill = "-",   na.rm = T) %>%
      dplyr::mutate(Rata_Plati = Plati / Sold_Garantii * 12 / lubridate::month(vals$report_date)) %>%
      dplyr::mutate(rank_order = ifelse(Banca == "Total", 0, 1)) %>%
      dplyr::arrange(rank_order, desc(Rata_Plati)) %>% dplyr::select(-rank_order) %>%
      # I do not collect now Plafon Procentual & Plafon Trezorerie as it will be needed after adjusting Clasa_Risc
      dplyr::left_join(y = vals_anexe$clase_risc %>% dplyr::select(2:3), by = c("Banca" = "DenumireFinantator") ) %>%
      dplyr::mutate(ajustare_clasa = ifelse(
        Rata_Plati <=   vals_anexe$rata_anualizata_plati + 0.04,0, ifelse(
          Rata_Plati <=   vals_anexe$rata_anualizata_plati + 0.08,
          1, ifelse(Rata_Plati <=   vals_anexe$rata_anualizata_plati + 0.1, 2, 3) )   )) %>%
      dplyr::mutate(Clasa_Risc_Ajustata = LETTERS[match(ClasaRisc, LETTERS) + ajustare_clasa]) %>% dplyr::left_join(
        vals_anexe$clase_risc %>% dplyr::filter(ClasaRisc != "-") %>% dplyr::group_by(ClasaRisc) %>% dplyr::summarise(
          PlafonProcentual = max(PlafonProcentual, na.rm = T),
          LimitaTrezorerie = max(LimitaTrezorerie, na.rm = T) ) ,  by = c("Clasa_Risc_Ajustata" = "ClasaRisc")   )
    
    
    vals_anexe$banci_ajustate <- vals_anexe$rata_platilor %>% dplyr::filter(ajustare_clasa > 0) %>%
      dplyr::select(Banca, Clasa_Risc_Ajustata,PlafonProcentual, LimitaTrezorerie)
    
    output$rata_platilor <- DT::renderDataTable( { req(input$show_rata_platilor == TRUE)
      DT::datatable(data = vals_anexe$rata_platilor, rownames = FALSE, extensions = "Buttons",
          options = list(dom = "Bftip", paging = FALSE, scrollY="300px", buttons = c("copy","excel","csv")), 
          caption =  htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                        "Rata anualizata a Platilor pe banci:") )  %>%
            DT::formatRound(columns = c(2:3,9), digits = 0) %>% DT::formatPercentage(columns = 4,digits = 1) })
    
    ############### ANEXA B
    vals_anexe$balanta_proprii <- readRDS("R/reactivedata/balanta/balanta_database.rds") %>%
      dplyr::filter( tip_sursa == "Surse_Proprii", data_balanta == vals$report_date)
    
    vals_anexe$resurse_proprii <- sum(vals_anexe$balanta_proprii$`Solduri finale|Debit`)
    
   
    
    vals_anexe$anexac <-    dplyr::left_join(   x = vals_anexe$balanta_proprii %>%
          dplyr::filter(tip_plasament %in% c( "Conturi_Curente", "Gestionari_Cautiuni_Garantii","Depozite")  ) %>%  
            dplyr::mutate(Banca = ifelse(Banca == "TREZORERIE", "UNICREDIT", Banca)) %>% 
            dplyr::mutate(Banca = as.character(Banca)) %>% 
            dplyr::left_join(tabela_nume_banci, by = c("Banca" = "CodFinantator")) %>%
            dplyr::group_by(Banca = DenumireFinantator, tip_plasament) %>%
            dplyr::summarise(Expunere = sum(`Solduri finale|Debit`)) %>% as.data.frame(),
          y = vals_anexe$clase_risc  %>% dplyr::select(DenumireFinantator,  ClasaRisc,  LimitaTrezorerie ),
          by = c("Banca" = "DenumireFinantator")  )    
    
    
    vals_anexe$anexaC_tabel <-  tryCatch(expr = { 
      vals_anexe$anexac %>% tidyr::pivot_wider(names_from = tip_plasament,
              values_from = Expunere, values_fill = 0) %>% dplyr::arrange(ClasaRisc, desc(Depozite)) %>%
      dplyr::mutate(Grad_Utilizare_Plafon = (Conturi_Curente+Depozite+Gestionari_Cautiuni_Garantii)/LimitaTrezorerie) %>%
      dplyr::mutate(rank_order = match(ClasaRisc, LETTERS))
    },  error= function(e) (data.frame(Banca="No data for selected date",ClasaRisc="",LimitaTrezorerie=NA_integer_,
             Conturi_Curente=NA_integer_,Depozite=NA_integer_,Gestionari_Cautiuni_Garantii=NA_integer_,
             Grad_Utilizare_Plafon=NA_integer_, rank_order=NA_integer_)) ) 
    
   
    
    vals$anexaC_final <-  vals_anexe$anexaC_tabel %>%  dplyr::bind_rows(
      vals_anexe$anexaC_tabel %>% dplyr::group_by(ClasaRisc) %>% dplyr::select(-Banca)  %>%
          dplyr::summarise_all(.funs = ~ sum(.)) %>% janitor::adorn_totals(where = "row") %>% dplyr::mutate(
              LimitaTrezorerie = ifelse( ClasaRisc == "A",  vals_anexe$resurse_proprii,
               ifelse( ClasaRisc == "B", 0.75 * vals_anexe$resurse_proprii,
                 ifelse( ClasaRisc == "C", vals_anexe$resurse_proprii * 0.5,
                   ifelse( ClasaRisc == "D", 0.3 * vals_anexe$resurse_proprii,
                     ifelse(ClasaRisc == "E", 0.15 * vals_anexe$resurse_proprii, NA_real_)  ) ) ) ) ) %>%
          dplyr::mutate(rank_order = match(ClasaRisc, LETTERS)) %>%
          dplyr::mutate(ClasaRisc = ifelse(ClasaRisc == "Total", "Grand Total", paste0("Subtotal ", ClasaRisc))) %>%
          dplyr::mutate(Banca = "-") %>%
          dplyr::mutate( Grad_Utilizare_Plafon = (Conturi_Curente+Depozite+Gestionari_Cautiuni_Garantii)/LimitaTrezorerie) ) %>%
            dplyr::arrange(rank_order) %>% dplyr::select(-rank_order)
    
   
    
   output$anexaB <- DT::renderDataTable({ req(vals$anexaC_final)
    
      DT::datatable(data = vals$anexaC_final, rownames = F, extensions = "Buttons",
                    options = list(dom = "Bfti", paging = FALSE, scrollY = "300px", buttons = c("copy","excel","csv")), 
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                  vals_anexe$caption_anexa_b )) %>%
        DT::formatRound(columns = 3:6,digits = 0) %>% DT::formatPercentage(columns = 7,digits = 1)  })
   
  })
    
    # Calculate Anexa A
    observeEvent(input$cap_proprii,{ req(vals_anexe$banci_ajustate)
      
      vals_anexe$solduri_curente <- readRDS("R/reactivedata/solduri/baza_banci.rds") %>% 
        dplyr::filter(data_raport==vals$report_date,  Tip_surse == "Surse_proprii") %>% 
        
        dplyr::left_join(tabela_nume_banci, by = c("Banca" = "CodFinantator")) %>%  dplyr::group_by(Banca=DenumireFinantator) %>% 
        
        dplyr::summarise(Sold_Garantii=sum(`Soldul garantiei [in LEI]`)) %>%  
        dplyr::left_join(y = vals_anexe$clase_risc %>% dplyr::select(2,3,6), by = c("Banca" = "DenumireFinantator") ) %>%
        dplyr::mutate(ClasaRisc = ifelse(Banca %in% vals_anexe$banci_ajustate$Banca, 
              vals_anexe$banci_ajustate$Clasa_Risc_Ajustata[match(Banca,vals_anexe$banci_ajustate$Banca)],ClasaRisc),
                      PlafonProcentual = ifelse(Banca %in% vals_anexe$banci_ajustate$Banca, 
                   vals_anexe$banci_ajustate$PlafonProcentual[match(Banca,vals_anexe$banci_ajustate$Banca)],PlafonProcentual)) %>%
        dplyr::mutate(Plafon_Finantator = PlafonProcentual * input$cap_proprii * 7) %>%
        dplyr::mutate(Utilizare_Plafon_Finantator = Sold_Garantii/Plafon_Finantator) %>%
        dplyr::mutate(Utilizare_Plafon_Garantare = Sold_Garantii/(input$cap_proprii * 7)) %>%
        dplyr::mutate(dplyr::across(.cols = ClasaRisc,~as.character(.x))) %>%
        dplyr::mutate(dplyr::across(.cols = PlafonProcentual,~as.numeric(.x)))
      
      
      vals$anexa_A <- vals_anexe$solduri_curente %>% dplyr::arrange(ClasaRisc,desc(Sold_Garantii)) %>% 
        dplyr::mutate(rank_order = match(x = ClasaRisc, table = LETTERS) ) %>%
        dplyr::bind_rows( vals_anexe$solduri_curente %>% dplyr::group_by(ClasaRisc=as.character(ClasaRisc)) %>% 
            dplyr::summarise(Sold_Garantii=sum(Sold_Garantii), Utilizare_Plafon_Garantare = sum(Utilizare_Plafon_Garantare)) %>% 
              as.data.frame() %>% janitor::adorn_totals(where = "row",name = "Grand Total") %>%
                            dplyr::mutate(Banca = "-",Plafon_Finantator = NA_real_,Utilizare_Plafon_Finantator=NA_real_) %>%
                            dplyr::mutate(rank_order = match(x = ClasaRisc, table = LETTERS) ) %>% 
                            dplyr::mutate(ClasaRisc = ifelse(ClasaRisc == "Grand Total",ClasaRisc,
                                                              paste0("Subtotal ", ClasaRisc))) ) %>%
        dplyr::arrange(rank_order, ClasaRisc) %>%
        dplyr::select(ClasaRisc, Banca, Sold_Garantii, Plafon_Finantator,Utilizare_Plafon_Finantator,Utilizare_Plafon_Garantare)
      
      output$anexa_A <- DT::renderDataTable( DT::datatable(rownames = FALSE,
                data = vals$anexa_A, options = list(dom = "fti", paging=FALSE, scrollY = "300px"),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                  paste0("Anexa A la ", vals$report_date, " :")))  %>% 
                  DT::formatRound(columns = 3:4,digits = 0) %>%
                                DT::formatPercentage(columns = 5:6, digits = 1))
      
      output$down_anexaA <- downloadHandler(filename = function() { paste0("anexaA_", vals$report_date,".csv") },
                    content = function(file) { readr::write_csv(x = vals$anexa_A, file = file ) } )
      
    })
    
    
    observeEvent(vals$top_expuneri_surse_proprii, {
      
      vals$tabel_top_expuneri <- dplyr::filter(.data = vals$expuneri_surse_proprii, 
                                               Grup %in% vals$top_expuneri_surse_proprii$Grup) %>% 
        dplyr::group_by(Grup, Beneficiar) %>%
        dplyr::summarise(Expunere_Contabila = sum(`Soldul garantiei [in LEI]`),
                         Provizion_Contabil = sum(Provizion)) %>%
        dplyr::mutate(Expunere_Bruta = Expunere_Contabila - Provizion_Contabil,
                      rank_order = match(Grup, vals$top_expuneri_surse_proprii$Grup)) %>%
        dplyr::arrange(rank_order) %>% dplyr::select(-rank_order) %>% janitor::adorn_totals(where = "row",fill = "-")
      
      output$top_expuneri <- DT::renderDataTable( DT::datatable(data = vals$tabel_top_expuneri,
                                                                rownames = FALSE,  options = list(dom="tp", pageLength = 6), 
                                                                caption = #htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                                  "Top 10 expuneri surse proprii - detaliat" ) %>% 
                                                    DT::formatRound(columns = 3:5)  )
      
      output$down_top_expuneri <- downloadHandler(filename = function() { paste0("top_expuneri ",vals$report_date,".csv")},
                            content = function(file) { readr::write_csv(x =  vals$tabel_top_expuneri, file = file) } )
      
    })
    
  
    
    
   
    
  })
}
    
## To be copied in the UI
# mod_anexe_ui("anexe_ui_1")
    
## To be copied in the server
# mod_anexe_server("anexe_ui_1")
