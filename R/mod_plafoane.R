#' plafoane UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plafoane_ui <- function(id,vals){
  ns <- NS(id)
  tagList(
  bs4Dash::box(title = "Update Plafoane de Garantare Surse proprii si administrare",
               icon = icon("edit"),width = 12,maximizable = T,
               status = "primary",collapsible = TRUE,collapsed = TRUE,
  fluidPage( shinyFeedback::useShinyFeedback(),
        fluidRow(
        column(width = 6,
        fluidRow(
        shinyWidgets::airMonthpickerInput(inputId = ns("plafoane_date") ,
        label = "Data plafoanelor",value = Sys.Date()) ),
        shinyWidgets::autonumericInput(inputId = ns("cap_proprii"),label = "Capitaluri proprii",value = 0,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        shinyWidgets::autonumericInput(inputId = ns("impr_subordon"),label = "Imprumuturi subordonate",value = 0,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        shinyWidgets::autonumericInput(inputId = ns("fonduri_proprii"),label = "Fonduri proprii",value = 0,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        shinyWidgets::autonumericInput(inputId = ns("oug_79"),label = "OUG 79",value = 467839276,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        shinyWidgets::autonumericInput(inputId = ns("oug_43"),label = "OUG 43",value = 10000000,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        shinyWidgets::autonumericInput(inputId = ns("lg_329"),label = "Legea 329",value = 1700000,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        shinyWidgets::autonumericInput(inputId = ns("lg_218"),label = "Legea 218",value = 585607,align = "right",
                                       digitGroupSeparator = ",",decimalPlaces = 0,minimumValue = 0,
                                       decimalCharacter = "."  ),
        
        shinyWidgets::actionBttn(inputId = ns("save_plafoane"),icon = icon("save"),color = "primary",
                                 label = "Salveaza datele de mai sus") ),
        column(width = 6, DT::dataTableOutput(ns("baza_surse_proprii")), hr(),
               DT::dataTableOutput(ns("baza_surse_administrare")))
      ) ) ),
  bs4Dash::box(title = "Tabelurile 3 si 4 ale Raportului de Prudentialitate",
               icon = icon("table"),width = 12, maximizable = TRUE,
               status = "primary",collapsible = TRUE,collapsed = FALSE,
               DT::dataTableOutput(ns("utilizare_plafoane")), hr(),
               DT::dataTableOutput(ns("sinteza_plafoane"))
                                  )
  )
}
    
#' plafoane Server Functions
#'
#' @noRd 
mod_plafoane_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    baza_plafoane <- readRDS("R/reactivedata/solduri/baza_plafoane.rds")
    
    vals_plafoane <- reactiveValues(baza_plafoane = baza_plafoane)
    
    # Outputs baza de date a fondurilor proprii (to thw right of the page)
    output$baza_surse_proprii <- DT::renderDataTable( { DT::datatable(data = vals_plafoane$baza_plafoane %>% 
                        dplyr::select(1:4),     rownames = F,extensions = "Buttons",
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
              "Baza de date a Fondurilor Proprii:"),
          options = list(dom = "Bt", buttons = c("copy","csv","excel"))) %>% DT::formatRound(columns = 1:3,digits = 0) })
    # Outputs baza de date a plafoanelor din surse administrare (to the right of the page)
    output$baza_surse_administrare <-  DT::renderDataTable( { DT::datatable(
          data = vals_plafoane$baza_plafoane %>% dplyr::select(4:8),
          rownames = F, extensions = "Buttons",
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
            "Baza de date a Surselor in Administrare:"),
          options = list(dom = "Bt", buttons = c("copy", "csv", "excel")) ) %>% DT::formatRound(columns = 2:5, digits = 0)
    })
    
    # Main processing observer. It assembles main tables depending on report date, previous month, previous year
    observeEvent(vals_plafoane$baza_plafoane, {
      
      # First I extract plafoanele lunii curente
      vals_plafoane$plafoane_data_curenta <- vals_plafoane$baza_plafoane %>% 
        dplyr::filter(data_raport == vals$report_date) %>% dplyr::select(-data_raport)
      # Apoi extrag plafoanele lunii anterioare
      vals_plafoane$plafoane_luna_anterioara <- vals_plafoane$baza_plafoane %>% 
        dplyr::filter(data_raport == vals$previous_month) %>% dplyr::select(-data_raport)
      # Si apoi plafoanele anului anterior
      vals_plafoane$plafoane_an_anterior <- vals_plafoane$baza_plafoane %>% 
        dplyr::filter(data_raport ==  vals$previous_year) %>% dplyr::select(-data_raport)
      # Mai jos calculez doar coloana de utilizare a plafoanelor la luna curenta
      vals_plafoane$utilizare_plafoane <- vals$view_baza_solduri %>% 
        dplyr::filter(Tip_surse != "Nume_cont_stat", data_raport == vals$report_date) %>% dplyr::select(1:2,5) %>%
        dplyr::arrange(desc(Tip_surse), desc(Sold_garantii)) %>%
        dplyr::mutate(Plafon_Garantare = ifelse(Tip_surse == "Surse_proprii",
                                                (vals_plafoane$plafoane_data_curenta$Fonduri_proprii -vals_plafoane$plafoane_data_curenta$Impr_subordon)*7,
                                                ifelse(`Tip fonduri` == "03. APDRP (OG79)",vals_plafoane$plafoane_data_curenta$OG_79*5,
                                                       ifelse(`Tip fonduri` == "07. OUG43",vals_plafoane$plafoane_data_curenta$OUG_43*5,
                                                              ifelse(`Tip fonduri` == "04. LG 329", vals_plafoane$plafoane_data_curenta$LG_329*5,
                                                                     ifelse(`Tip fonduri` == "02. Lg218", vals_plafoane$plafoane_data_curenta$LG_218,NA_real_))))) ) %>%
        dplyr::mutate(Utilizare_Plafon = Sold_garantii/Plafon_Garantare) %>%
        dplyr::rename_at(.vars = 'Utilizare_Plafon', ~paste0("Utilizare_Plafon_",vals$report_date))
      # Apoi calculez doar coloana de utilizare a plafoanelor la luna anterioara
      vals_plafoane$utilizare_plafoane_luna_anterioara <- vals$view_baza_solduri %>% 
        dplyr::filter(Tip_surse != "Nume_cont_stat", data_raport == vals$previous_month) %>% dplyr::select(1:2,5) %>%
        dplyr::arrange(desc(Tip_surse), desc(Sold_garantii)) %>%
        dplyr::mutate(Plafon_Garantare = ifelse(Tip_surse == "Surse_proprii",
                                                (vals_plafoane$plafoane_luna_anterioara$Fonduri_proprii - vals_plafoane$plafoane_luna_anterioara$Impr_subordon)*7,
                                                ifelse(`Tip fonduri` == "03. APDRP (OG79)",vals_plafoane$plafoane_luna_anterioara$OG_79*5,
                                                       ifelse(`Tip fonduri` == "07. OUG43",vals_plafoane$plafoane_luna_anterioara$OUG_43*5,
                                                              ifelse(`Tip fonduri` == "04. LG 329", vals_plafoane$plafoane_luna_anterioara$LG_329*5,
                                                                     ifelse(`Tip fonduri` == "02. Lg218", vals_plafoane$plafoane_luna_anterioara$LG_218,NA_real_))))) ) %>%
        dplyr::mutate(Utilizare_Plafon = Sold_garantii/Plafon_Garantare) %>%
        dplyr::rename_at(.vars = 'Utilizare_Plafon', ~paste0("Utilizare_Plafon_",vals$previous_month))
      
      # Utilizarea plafoanelor de garantare in anul anterior
      vals_plafoane$utilizare_plafoane_an_anterior <- vals$view_baza_solduri %>% 
        dplyr::filter(Tip_surse != "Nume_cont_stat", data_raport ==  vals$previous_year) %>% dplyr::select(1:2,5) %>%
        dplyr::arrange(desc(Tip_surse), desc(Sold_garantii)) %>%
        dplyr::mutate(Plafon_Garantare = ifelse(Tip_surse == "Surse_proprii",
                                                (vals_plafoane$plafoane_an_anterior$Fonduri_proprii - vals_plafoane$plafoane_an_anterior$Impr_subordon)*7,
                                                ifelse(`Tip fonduri` == "03. APDRP (OG79)",vals_plafoane$plafoane_an_anterior$OG_79*5,
                                                       ifelse(`Tip fonduri` == "07. OUG43",vals_plafoane$plafoane_an_anterior$OUG_43*5,
                                                              ifelse(`Tip fonduri` == "04. LG 329", vals_plafoane$plafoane_an_anterior$LG_329*5,
                                                                     ifelse(`Tip fonduri` == "02. Lg218", vals_plafoane$plafoane_an_anterior$LG_218,NA_real_))))) ) %>%
        dplyr::mutate(Utilizare_Plafon = Sold_garantii/Plafon_Garantare) %>%
        dplyr::rename_at(.vars = 'Utilizare_Plafon', ~paste0("Utilizare_Plafon_", vals$previous_year))
      
      
      # Asamblez tabelul 3 si utilizarea plafoanelor folosind left join dupa tip surse
      
      vals$tabel3 <- vals_plafoane$utilizare_plafoane %>%
        dplyr::select(2, 4, 5) %>% dplyr::left_join(vals_plafoane$utilizare_plafoane_luna_anterioara %>%
                                                      dplyr::select(2, 5),by = "Tip fonduri") %>% dplyr::left_join(vals_plafoane$utilizare_plafoane_an_anterior %>%
                                                                                                                     dplyr::select(2, 5),by = "Tip fonduri") %>% dplyr::mutate("Tip fonduri" =
                                                                                                                                                                                 stringr::str_remove_all(string = `Tip fonduri`, pattern = '[:digit:][:digit:]\\.') %>%
                                                                                                                                                                                 stringr::str_trim(string = ., side = "left"))
      
      
      output$utilizare_plafoane <- DT::renderDataTable(DT::datatable(data = vals$tabel3,rownames = FALSE,
                                                                     caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                                                                       "Tabelul 3 - Utilizarea plafoanelor de garantare"),options = list(dom = "Bt", buttons=c("copy","csv","excel")),
                                                                     extensions = "Buttons") %>% DT::formatRound(columns = 2,digits = 0) %>% 
                                                         DT::formatPercentage(columns = 3:5,digits = 1))
      
      # Asamblez tabelul 4
      vals$tabel4 <- vals$view_baza_solduri %>% dplyr::filter(Tip_surse != "Nume_cont_stat", 
                data_raport == vals$report_date) %>% dplyr::group_by(Tip_surse) %>%
        dplyr::summarise(Sold_garantii=sum(Sold_garantii)) %>% dplyr::arrange(Tip_surse) %>%
        janitor::adorn_totals(where = "row",name = "Total Surse proprii si administrare") %>%
        cbind(data.frame(Sursa_de_finantare = c("Imprumuturi subordonate", "Capitaluri proprii","Fonduri proprii"),
                         "Valoare_sursa_de_finantare"= c(vals_plafoane$plafoane_data_curenta$Impr_subordon,
                    vals_plafoane$plafoane_data_curenta$Cap_proprii,vals_plafoane$plafoane_data_curenta$Fonduri_proprii)))
      
      
      output$sinteza_plafoane <-   DT::renderDataTable( DT::datatable(  data = vals$tabel4,
            rownames = FALSE,   caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
              paste0( "Tabelul 4 - Sinteza solduri de garanţii si surse de finantare – ",
                vals$report_date  )  ), extensions = "Buttons",
            options = list(dom = "Bt", buttons = c("copy", "csv", "excel"))) %>% 
              DT::formatRound(columns = c(2, 4), digits = 0)    )
      
    } )
    
    # Observer to update data plafoane to vals$report_date and disable save plafoane
    observeEvent(vals$report_date,{
    shinyWidgets::updateAirDateInput(session = session,inputId = "plafoane_date", value=vals$report_date) 
      shinyjs::disable(id = "save_plafoane", asis = FALSE)
      # The options to disable plafoane date might not be a bad idea, but it will not allow to update old plafoane
      #shinyjs::disable(id = "plafoane_date", asis = FALSE)
      #vals$report_date <- input$plafoane_date #%>% lubridate::round_date(unit = "bimonth")-1
      })
    
    # Observer for activating save button when all inputs are greater than zero
    observe({req(input$cap_proprii > 0, input$impr_subordon > 0,input$fonduri_proprii > 0, input$oug_79 > 0,
              input$oug_43 > 0,input$lg_329 > 0,input$lg_218 > 0)
      shinyjs::enable(id = "save_plafoane", asis = FALSE)
    })
    
    # Standard observer when calling compare_df module
    observeEvent(input$save_plafoane,{
      
     vals_plafoane$df_new <- data.frame(Cap_proprii = as.numeric(input$cap_proprii),
            Impr_subordon = as.numeric(input$impr_subordon), Fonduri_proprii = as.numeric(input$fonduri_proprii),
            data_raport = input$plafoane_date,
            OG_79 = as.numeric(input$oug_79),OUG_43 = as.numeric(input$oug_43),
            LG_329 = as.numeric(input$lg_329),LG_218 = as.numeric(input$lg_218))
      
      vals_plafoane$df_old <- baza_plafoane
      vals_plafoane$element_id <- input$plafoane_date #%>% lubridate::round_date(unit = "bimonth")-1
      vals_plafoane$column_id = "data_raport"
      vals_plafoane$finalise_process_compare_df = FALSE
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = vals_plafoane, red="#ff007b",green="#00ff84") 
     
       
     })
    
    # Standard observer when calling compare_df module
    observeEvent(vals_plafoane$finalise_process_compare_df,{ req(vals_plafoane$finalise_process_compare_df == TRUE )
    
      vals_plafoane$baza_plafoane <- vals_plafoane$df_new_prel
      saveRDS(object = vals_plafoane$df_new_prel,file = "R/reactivedata/baza_plafoane.rds")
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
            .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      shinyjs::disable(id = "save_plafoane", asis = FALSE)
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_plafoane_ui("plafoane_ui_1")
    
## To be copied in the server
# mod_plafoane_server("plafoane_ui_1")
