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
        column(width = 4,
        uiOutput(ns("show_plafoane_date")),
       
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
                                 label = "Salveaza datele de mai sus",style = "stretch") ),
        
        column( width = 8, DT::dataTableOutput(ns("baza_surse_proprii")),
                tags$script(src = "inst/app/www/plafoane_buttons.js"),
                tags$script(paste0("plafoane_module_js('", ns(''), "')")),
                hr(),
               DT::dataTableOutput(ns("baza_surse_administrare")),
                )
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
    
    observeEvent(vals_plafoane$baza_plafoane,{
    
    vals_plafoane$actions <- purrr::map_chr(vals_plafoane$baza_plafoane$data_raport, function(id_) {
      paste0(
        '<button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ',
        id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>'
      )  })
    
    })
    
    observeEvent( input$data_raport_to_delete,{
      shinyWidgets::ask_confirmation(inputId = ns("confirm_delete"),title = "CONFIRM",
          text = paste0("Esti sigur ca vrei sa stergi inregistrarile din data de ",input$data_raport_to_delete," ?",
                      " Vor fi sterse si sursele in administrare."),
          btn_labels = c("NU, renunta","OK, sterge"),btn_colors = c("#ff007b","#00ff84"), type = "info") } )
    
    observeEvent(input$confirm_delete, {req(input$confirm_delete == TRUE) 
      vals_plafoane$baza_plafoane <- vals_plafoane$baza_plafoane %>% 
        dplyr::filter(data_raport != input$data_raport_to_delete)
      
      saveRDS(object = vals_plafoane$baza_plafoane, "R/reactivedata/solduri/baza_plafoane.rds" )
      
      })
    
    # Outputs baza de date a fondurilor proprii (to the right of the page)
    output$baza_surse_proprii <- DT::renderDataTable({
        DT::datatable(
        data =  cbind(tibble::tibble(" " = vals_plafoane$actions),vals_plafoane$baza_plafoane  %>% 
                        dplyr::select(1:4) ),     rownames = F,extensions = "Buttons", escape = FALSE,
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                          "Baza de date a Fondurilor Proprii:"),
        options = list(dom = "Btp", buttons = c("copy","csv","excel"), pageLength = 5)) %>% DT::formatRound(columns = 2:4,digits = 0) })
   
    
    # Outputs baza de date a plafoanelor din surse administrare (to the right of the page)
    output$baza_surse_administrare <-  DT::renderDataTable( { DT::datatable(
          data = vals_plafoane$baza_plafoane %>% dplyr::select(4:8),
          rownames = F, extensions = "Buttons",
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
            "Baza de date a Surselor in Administrare:"),
          options = list(dom = "Btp", buttons = c("copy", "csv", "excel"),pageLength=5) ) %>% DT::formatRound(columns = 2:5, digits = 0)
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
        dplyr::filter(Tip_surse != "Nume_cont_stat", data_raport == vals$report_date) %>% dplyr::select(1:2, 5) %>%
        dplyr::arrange(desc(Tip_surse), desc(Sold_garantii)) %>%
        dplyr::mutate(Plafon_Garantare = ifelse(
          Tip_surse == "Surse_proprii",
          ( vals_plafoane$plafoane_data_curenta$Fonduri_proprii - vals_plafoane$plafoane_data_curenta$Impr_subordon ) * 7,
          ifelse(
            `Tip fonduri` == "03. APDRP (OG79)",
            vals_plafoane$plafoane_data_curenta$OG_79 * 5,
            ifelse(
              `Tip fonduri` == "07. OUG43",
              vals_plafoane$plafoane_data_curenta$OUG_43 * 5,
              ifelse(
                `Tip fonduri` == "04. LG 329",
                vals_plafoane$plafoane_data_curenta$LG_329 * 5,
                ifelse(
                  `Tip fonduri` == "02. Lg218",
                  vals_plafoane$plafoane_data_curenta$LG_218,
                  NA_real_
                )
              )
            )
          )
        )) %>%
        dplyr::mutate(Tip_surse = ifelse(Tip_surse == "Surse_proprii",Tip_surse,`Tip fonduri`)) %>% 
          dplyr::group_by(Tip_surse) %>% dplyr::summarise(Plafon_Garantare = mean(Plafon_Garantare),
            Sold_garantii=sum(Sold_garantii)) %>% dplyr::mutate(Utilizare_Plafon = Sold_garantii/Plafon_Garantare) %>%
              dplyr::rename_at(.vars = 'Utilizare_Plafon', ~ paste0("Utilizare_Plafon_", vals$report_date))
      
      # Apoi calculez doar coloana de utilizare a plafoanelor la luna anterioara
      vals_plafoane$utilizare_plafoane_luna_anterioara <-
        vals$view_baza_solduri %>%
        dplyr::filter(Tip_surse != "Nume_cont_stat",
                      data_raport == vals$previous_month) %>% dplyr::select(1:2, 5) %>%
        dplyr::arrange(desc(Tip_surse), desc(Sold_garantii)) %>%
        dplyr::mutate(Plafon_Garantare = ifelse(
          Tip_surse == "Surse_proprii",
          (
            vals_plafoane$plafoane_luna_anterioara$Fonduri_proprii - vals_plafoane$plafoane_luna_anterioara$Impr_subordon
          ) * 7,
          ifelse(
            `Tip fonduri` == "03. APDRP (OG79)",
            vals_plafoane$plafoane_luna_anterioara$OG_79 * 5,
            ifelse(
              `Tip fonduri` == "07. OUG43",
              vals_plafoane$plafoane_luna_anterioara$OUG_43 * 5,
              ifelse(
                `Tip fonduri` == "04. LG 329",
                vals_plafoane$plafoane_luna_anterioara$LG_329 * 5,
                ifelse(
                  `Tip fonduri` == "02. Lg218",
                  vals_plafoane$plafoane_luna_anterioara$LG_218,
                  NA_real_
                )
              )
            )
          )
        )) %>%
        dplyr::mutate(Tip_surse = ifelse(Tip_surse == "Surse_proprii",Tip_surse,`Tip fonduri`)) %>% 
        dplyr::group_by(Tip_surse) %>% dplyr::summarise(Plafon_Garantare = mean(Plafon_Garantare),
            Sold_garantii=sum(Sold_garantii)) %>% dplyr::mutate(Utilizare_Plafon = Sold_garantii/Plafon_Garantare) %>%
        dplyr::rename_at(.vars = 'Utilizare_Plafon', ~ paste0("Utilizare_Plafon_", vals$previous_month))
      
      # Utilizarea plafoanelor de garantare in anul anterior
      
      vals_plafoane$utilizare_plafoane_an_anterior <-
        vals$view_baza_solduri %>%
        dplyr::filter(Tip_surse != "Nume_cont_stat",
                      data_raport ==  vals$previous_year) %>% dplyr::select(1:2, 5) %>%
        dplyr::arrange(desc(Tip_surse), desc(Sold_garantii)) %>%
        dplyr::mutate(Plafon_Garantare = ifelse(
          Tip_surse == "Surse_proprii",
          (
            vals_plafoane$plafoane_an_anterior$Fonduri_proprii - vals_plafoane$plafoane_an_anterior$Impr_subordon
          ) * 7,
          ifelse(
            `Tip fonduri` == "03. APDRP (OG79)",
            vals_plafoane$plafoane_an_anterior$OG_79 * 5,
            ifelse(
              `Tip fonduri` == "07. OUG43",
              vals_plafoane$plafoane_an_anterior$OUG_43 * 5,
              ifelse(
                `Tip fonduri` == "04. LG 329",
                vals_plafoane$plafoane_an_anterior$LG_329 * 5,
                ifelse(
                  `Tip fonduri` == "02. Lg218",
                  vals_plafoane$plafoane_an_anterior$LG_218,
                  NA_real_
                )
              )
            )
          )
        )) %>%
        dplyr::mutate(Tip_surse = ifelse(Tip_surse == "Surse_proprii",Tip_surse,`Tip fonduri`)) %>% 
        dplyr::group_by(Tip_surse) %>% dplyr::summarise(Plafon_Garantare = mean(Plafon_Garantare),
          Sold_garantii=sum(Sold_garantii)) %>% dplyr::mutate(Utilizare_Plafon = Sold_garantii/Plafon_Garantare) %>% 
        dplyr::rename_at(.vars = 'Utilizare_Plafon', ~ paste0("Utilizare_Plafon_", vals$previous_year))
      
      
      # Asamblez tabelul 3 si utilizarea plafoanelor folosind left join dupa tip surse
      
     vals$tabel3 <- tryCatch( expr = {vals_plafoane$utilizare_plafoane %>% dplyr::left_join(
          y = vals_plafoane$utilizare_plafoane_luna_anterioara %>% dplyr::select(-2, -3),
          by = "Tip_surse" ) %>% dplyr::left_join(
          y = vals_plafoane$utilizare_plafoane_an_anterior %>% dplyr::select(-2, -3),
          by = "Tip_surse" ) %>% dplyr::arrange(desc(Plafon_Garantare)) %>% 
        dplyr::mutate(Tip_surse = stringr::str_remove_all(string = Tip_surse, pattern = '[:digit:][:digit:]\\.') %>%
          stringr::str_trim(string = ., side = "left"))}, error = function(e) {
            data.frame(Tip_surse="Upload more data",Plafon_Garantare=0,Sold_garantii = 0,Utilizare_Plafon_curent= 0,
                       Utilizare_Plafon_luna_anterioara=0,Utilizare_plafon_an_anterior=0)
          } )
      
      output$utilizare_plafoane <-  DT::renderDataTable(  DT::datatable(  data = vals$tabel3,
            rownames = FALSE,
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                              "Tabelul 3 - Utilizarea plafoanelor de garantare"),
            options = list(dom = "Bt", buttons = c("copy", "csv", "excel")),
            extensions = "Buttons"
          ) %>% DT::formatRound(columns = 2:3, digits = 0) %>%
            DT::formatPercentage(columns = 4:6, digits = 1)
        )
      
      # Asamblez tabelul 4
      vals$tabel4 <- vals$view_baza_solduri %>% dplyr::filter(Tip_surse != "Nume_cont_stat", 
                data_raport == vals$report_date) %>% dplyr::group_by(Tip_surse) %>%
        dplyr::summarise(Sold_garantii=sum(Sold_garantii)) %>% dplyr::arrange(Tip_surse) %>%
        janitor::adorn_totals(where = "row",name = "Total Surse proprii si administrare") %>%
        cbind( tryCatch(expr = { 
          data.frame(Sursa_de_finantare = c("Imprumuturi subordonate", "Capitaluri proprii","Fonduri proprii"),
                         "Valoare_sursa_de_finantare"= c(vals_plafoane$plafoane_data_curenta$Impr_subordon,
                    vals_plafoane$plafoane_data_curenta$Cap_proprii,vals_plafoane$plafoane_data_curenta$Fonduri_proprii)) },
          error = function(e) { data.frame(Sursa_de_finantare = c("Imprumuturi subordonate", "Capitaluri proprii","Fonduri proprii"),
                                           "Valoare_sursa_de_finantare" = rep("No data",3) ) }
        ) )
      
    output$sinteza_plafoane <-   DT::renderDataTable( DT::datatable(  data = vals$tabel4,
            rownames = FALSE,   caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
              paste0( "Tabelul 4 - Sinteza solduri de garanţii si surse de finantare – ",
                vals$report_date  )  ), extensions = "Buttons",
            options = list(dom = "Bt", buttons = c("copy", "csv", "excel"))) %>% 
              DT::formatRound(columns = c(2, 4), digits = 0)    )
      
    } )
    
    # Observer to update data plafoane to vals$report_date 
    output$show_plafoane_date  <- renderUI({ req(vals$report_date) 
      shinyWidgets::airMonthpickerInput(inputId = ns("plafoane_date") ,autoClose=TRUE,
      label = "Data plafoanelor", value = lubridate::`%m-%`(vals$report_date,months(1) ) + 1) })
    
    # Observer for generating vals_plafoane$plafoane_date which is used later.
    observeEvent(input$plafoane_date,{
      vals_plafoane$plafoane_date <- lubridate::`%m+%`(input$plafoane_date,months(1) ) -1
      #shinyjs::disable(id = "save_plafoane", asis = FALSE)     
      })
    
    # Observer for activating save button when all inputs are greater than zero
    #observe({req(input$cap_proprii > 0, input$impr_subordon > 0,input$fonduri_proprii > 0, input$oug_79 > 0,
      #        input$oug_43 > 0,input$lg_329 > 0,input$lg_218 > 0)
     # shinyjs::enable(id = "save_plafoane", asis = FALSE) })
    
    # Standard observer when calling compare_df module
    observeEvent(input$save_plafoane,{
      
     vals_plafoane$df_new <- data.frame(Cap_proprii = as.numeric(input$cap_proprii),
            Impr_subordon = as.numeric(input$impr_subordon), Fonduri_proprii = as.numeric(input$fonduri_proprii),
            data_raport =  vals_plafoane$plafoane_date,
            OG_79 = as.numeric(input$oug_79),OUG_43 = as.numeric(input$oug_43),
            LG_329 = as.numeric(input$lg_329),LG_218 = as.numeric(input$lg_218))
      
      vals_plafoane$df_old <- baza_plafoane
      vals_plafoane$element_id <-  vals_plafoane$plafoane_date
      vals_plafoane$column_id = "data_raport"
      vals_plafoane$finalise_process_compare_df = FALSE
      
     
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = vals_plafoane, red="#ff007b",green="#00ff84") 
     
       
     })
    
    # Standard observer when calling compare_df module
    observeEvent(vals_plafoane$finalise_process_compare_df,{ req(vals_plafoane$finalise_process_compare_df == TRUE )
    
      vals_plafoane$baza_plafoane <- vals_plafoane$df_new_prel
      saveRDS(object = vals_plafoane$df_new_prel,file = "R/reactivedata/solduri/baza_plafoane.rds")
      
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
