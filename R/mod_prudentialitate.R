#' prudentialitate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prudentialitate_ui <- function(id){
  ns <- NS(id)
  
  # Handles UI calling for all Prudentialitate sidebar and also processes Tabel 1 box
  
  bs4Dash::tabsetPanel( id = ns("raport"),  selected = T,
    shinyjs::useShinyjs(),
    
    shiny::tabPanel(title = "Solduri Garantii",value = "garantii",icon = icon("arrow-alt-circle-right"),
      tagList(  br(),
              
      bs4Dash::box(title = "Data raport - Foarte important!",status = "primary",width = 12,
                   footer = "Doar de aici se va selecta luna raportului de prudentialitate",
                   collapsible = T,collapsed = F,icon = icon("calendar"), maximizable = T,
                   column(width = 3,
                   shinyWidgets::airMonthpickerInput(inputId = ns("data_prudentialitate"), autoClose=TRUE,
                          label = "Data raportului de prudentialitate",value =  ifelse( lubridate::day(Sys.Date()) <= 10,
                          lubridate::`%m-%`(Sys.Date(), months(2)) %>% lubridate::floor_date(unit = "month"),
                          lubridate::`%m-%`(Sys.Date(), months(1)) %>% lubridate::floor_date(unit = "month")  ) %>% 
                            lubridate::as_date() ) )),
      
      bs4Dash::box(title = "Baza de date a soldurilor de garantii", width = 12, id = ns("box_database_solduri"),
                   icon = icon("database"), collapsible = T,collapsed = T, maximizable = T,
                   mod_garantii_database_ui("garantii_database_ui_1")),
      
      bs4Dash::box(title = "Upload fisierul de solduri", width = 12, id = ns("box_upload_solduri"),
                   icon = icon("file-upload"), collapsible = T,collapsed = T, maximizable = T,
                   mod_garantii_upload_ui("garantii_upload_ui_1")),
      
      bs4Dash::box(title = "Completeaza manual garantiile de stat", width = 12, id = ns("box_manual_solduri"),
                   icon = icon("keyboard"), collapsible = T,collapsed = T, maximizable = T,
                   footer = "Atentie, nu sunt disponibile pentru a fi selectate decat datele soldurilor uploadate anterior.",
      mod_garantii_manual_ui("garantii_manual_ui_1") ),  
      
      bs4Dash::box(title = "Tabel 1 - Evolutia soldurilor de garantii",status = "primary",
                   icon = icon("chart-bar"),collapsible = T,collapsed = T, width = 12,maximizable = T,
                   DT::dataTableOutput(ns("tabel1")) ),
      
      bs4Dash::box(title = "Tabelul 2 - Evolutia pe valute a portofoliului",width = 12,id = ns("box_tabel2"),
                   icon = icon("yen-sign"),collapsible = T,collapsed = F,maximizable = T,status = "primary",
                   mod_valute_ui("valute_ui_1")) 
      ) ),
  
    
    shiny::tabPanel(title = "Plafoane de Garantare",icon = icon("product-hunt"), value = "plafoane",br(),
                    mod_plafoane_ui("plafoane_ui_1")),
    
    shiny::tabPanel(title = "Grupuri debitori",icon = icon("users"), value = "grupuri",br(),
                    mod_grupuri_ui("grupuri_ui_1")),
    
    shiny::tabPanel(title = "Provizioane plati", icon = icon("dollar-sign"),value = "provizioane_plati",
                    mod_provizioane_ui("provizioane_ui_1")),
    
    shiny::tabPanel(title = "Plasamente",icon = icon("euro-sign"), value="plasamente", br(),
                    mod_plasamente_ui("plasamente_ui_1")),
    
    shiny::tabPanel(title = "Plati si cereri plata",icon = icon("paypal"), value="plati", br(),
                    mod_plati_ui("plati_ui_1")),
    
    shiny::tabPanel(title = "Anexe",icon = icon("adn"), value="anexe", br(),
                    mod_anexe_ui("anexe_ui_1")),
    
    shiny::tabPanel(title = "Fisierul Word",value = "final_report",icon = icon("file-word"),
                    mod_final_report_ui("final_report_ui_1") )
  ) 
}
    
#' prudentialitate Server Functions
#'
#' @noRd 
mod_prudentialitate_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    view_baza_solduri <- readRDS("R/reactivedata/solduri/view_baza_sold.rds")
    vals_prudent <- reactiveValues(view_baza_solduri = view_baza_solduri)
    
    # Below observer selects tab garantii when sidebar selecting raport de prudentialitate and
    # Updates data_prudentialitate to vals$report_date calculated within app_server
    
    updateTabsetPanel(session = session, inputId = 'raport',selected = "garantii")
    
    # Below observer captures selected tabs within raport de prudentialitate. 
    # Variable vals$selected_tab is mainly used within app_server to handle module calling
    
    
    observeEvent(input$raport,{ vals$raport_selected_tab <- c(vals$raport_selected_tab,input$raport) })
    
   
    # Below observers capture selected box from sold garantii. The info is processed within app_server where the corresponding module
    #  server is called.The module server is called once the box is maximized or opened (with plus sign). 
    
    observeEvent(input$box_tabel2, {req(any(input$box_tabel2$collapsed==FALSE, input$box_tabel2$maximized==TRUE))
      vals$box_selected <-"box_tabel2"
      })
    
    observeEvent(input$box_database_solduri, {req(any(input$box_database_solduri$collapsed==FALSE,
                                                      input$box_database_solduri$maximized==TRUE))
      vals$box_selected <- "box_database_solduri"
    })
    
    observeEvent(input$box_upload_solduri, {req( any(input$box_upload_solduri$collapsed==FALSE,
                                                     input$box_upload_solduri$maximized==TRUE) )
      vals$box_selected <- "box_upload_solduri"
    })
    
    observeEvent(input$box_manual_solduri, {req( any(input$box_manual_solduri$collapsed==FALSE, 
                                                     input$box_manual_solduri$maximized == TRUE) )
      vals$box_selected <- "box_manual_solduri"
    })
   
    
   
      
      # Below I assign input$data_prudentialitate to vals$report_date
      observeEvent(input$data_prudentialitate, { 
        
        # The day of input$data_prudentialitate will always be 1
        vals$report_date <- lubridate::`%m+%`(input$data_prudentialitate,months(1) ) -1
      })
      
      # Below I calculate and expose tabel1
      observe( {req(vals$report_date,vals$previous_month, vals$previous_year)
        
        # I calculate totals for Tip fonduri of tabel1
        vals_prudent$tabel1 <- vals$view_baza_solduri %>% dplyr::filter(data_raport == vals$report_date) %>%
          dplyr::group_by(Tip_surse, `Tip fonduri`) %>% dplyr::summarise(
            Solduri_luna_raportare = sum(Sold_garantii),
            Nr_contracte_luna_raportare = sum(Nr_contracte)) %>% 
            dplyr::arrange(desc(Tip_surse), desc(Solduri_luna_raportare)) %>%
          dplyr::mutate(  rank = dplyr::cur_group_id(),
            Variatie_sold_luna_anterioara = purrr::map2_dbl(
              .x = `Tip fonduri`,       .y = Solduri_luna_raportare,
              .f = ~ (.y /sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$`Tip fonduri` == .x &
                             vals$view_baza_solduri$data_raport == vals$previous_month])) - 1 ),
            Variatie_sold_an_anterior = purrr::map2_dbl(
              .x = `Tip fonduri`,
              .y = Solduri_luna_raportare,
              .f = ~ (.y /
                        sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$`Tip fonduri` == .x &
                                                                   vals$view_baza_solduri$data_raport == vals$previous_year])) -
                1
            ),
            Variatie_contracte_luna_anterioara = purrr::map2_dbl(
              .x = `Tip fonduri`,
              .y = Nr_contracte_luna_raportare,
              .f = ~
                (.y / sum(vals$view_baza_solduri$Nr_contracte[vals$view_baza_solduri$`Tip fonduri` == .x &
                                                                vals$view_baza_solduri$data_raport == vals$previous_month])) -
                1
            ),
            Variatie_contracte_an_anterior = purrr::map2_dbl(
              .x = `Tip fonduri`,
              .y = Nr_contracte_luna_raportare,
              .f = ~
                (.y / sum(vals$view_baza_solduri$Nr_contracte[vals$view_baza_solduri$`Tip fonduri` == .x &
                                                                vals$view_baza_solduri$data_raport == vals$previous_year])) -
                1
            )
          ) %>% dplyr::mutate("Tip fonduri" =
                           stringr::str_remove_all(string = `Tip fonduri`, pattern = '[:digit:][:digit:]\\.') %>%
                           stringr::str_trim(string = ., side = "left") %>%
                           stringr::str_replace_all(string = .,  pattern = "INVEST",replacement = "IMM INVEST" ) %>%
                           stringr::str_replace_all(string = .,  pattern = "AGRO",  replacement = "IMM AGRO"))
        
        # I calculate totals for Tip_surse of tabel1
        vals_prudent$tabel1_totals <- vals$view_baza_solduri %>% dplyr::filter(data_raport == vals$report_date) %>%
          dplyr::group_by(Tip_surse) %>% dplyr::mutate(rank = dplyr::cur_group_id()) %>%
          dplyr::summarise(
            Solduri_luna_raportare = sum(Sold_garantii), Nr_contracte_luna_raportare = sum(Nr_contracte)) %>% 
          dplyr::arrange(desc(Tip_surse)) %>%  dplyr::mutate(
            Variatie_sold_luna_anterioara = purrr::map2_dbl( .x = Tip_surse, .y = Solduri_luna_raportare,
              .f = ~ (.y / sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$Tip_surse == .x &
                vals$view_baza_solduri$data_raport == vals$previous_month])) - 1),
            Variatie_sold_an_anterior = purrr::map2_dbl( .x = Tip_surse, .y = Solduri_luna_raportare,
              .f = ~ (.y /sum(vals$view_baza_solduri$Sold_garantii[vals$view_baza_solduri$Tip_surse == .x &
                          vals$view_baza_solduri$data_raport == vals$previous_year])) -1),
            Variatie_contracte_luna_anterioara = purrr::map2_dbl(.x = Tip_surse,.y = Nr_contracte_luna_raportare,
                .f = ~(.y / sum(vals$view_baza_solduri$Nr_contracte[vals$view_baza_solduri$Tip_surse == .x &
                      vals$view_baza_solduri$data_raport == vals$previous_month])) -1),
            Variatie_contracte_an_anterior = purrr::map2_dbl( .x = Tip_surse,.y = Nr_contracte_luna_raportare,
                .f = ~(.y / sum(vals$view_baza_solduri$Nr_contracte[vals$view_baza_solduri$Tip_surse == .x &
                      vals$view_baza_solduri$data_raport == vals$previous_year])) -1),
            `Tip fonduri` = paste0(Tip_surse, " din care: ")) %>% dplyr::left_join(
              y = vals_prudent$tabel1 %>% dplyr::group_by(Tip_surse) %>% 
                            dplyr::summarise(rank = max(rank)), by = "Tip_surse")
          
        
        # Final assembly of tabel1
        vals$tabel1 <- dplyr::bind_rows(vals_prudent$tabel1_totals,vals_prudent$tabel1 ) %>% 
          dplyr::arrange(desc(rank), desc(Solduri_luna_raportare)) %>% dplyr::select(-Tip_surse, -rank) %>% 
          dplyr::relocate(`Tip fonduri`, .before = Solduri_luna_raportare)
       
        
        output$tabel1 <- DT::renderDataTable(  { req(vals$tabel1) 
          DT::datatable(data = vals$tabel1,options = list(dom = "Bftip", paging=FALSE, scrollY = "300px",
                                      buttons = c("copy","excel", "csv")),
              rownames = F, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                          "Tabelul 1 – Evolutia soldurilor de garantii:"),extensions = "Buttons",
              colnames = c("Tip fonduri",paste0("Solduri ", vals$report_date),paste0("Nr contracte ",vals$report_date),
                           paste0("Variatia sold fata de ", vals$previous_month),paste0("Variatie sold fata de ",vals$previous_year),
                           paste0("Variatie nr contracte fata de ",vals$previous_month),
                           paste0('Variatie nr contracte fata de ', vals$previous_year))) %>%
            DT::formatRound(columns = 2:3,digits = 0) %>% DT::formatPercentage(columns = 4:7,digits = 2)
        })
        
       
       
      })
   
    
  })
    
  
}
    
## To be copied in the UI
# mod_prudentialitate_ui("prudentialitate_ui_1")
    
## To be copied in the server
# mod_prudentialitate_server("prudentialitate_ui_1")
