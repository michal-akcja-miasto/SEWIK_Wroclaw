function(session, input, output) {
  
  ## binding inputs ----
  
  output$mapka <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
                                L.control.zoom({ position: 'topright' }).addTo(this)
                            }") %>% 
      addScaleBar(position = "bottomleft", 
                  options = scaleBarOptions(imperial = FALSE)) %>% 
      addTiles() %>% 
      addPolygons(data = osiedla,
                  group = "Granice osiedli",
                  color = "#222",
                  weight = 2,
                  fill = FALSE) %>% 
      addLayersControl(overlayGroups = c("Granice osiedli"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observeEvent(
    input$rok, {
      daty_range <- si_wroclaw %>% 
        filter(between(rok, input$rok[1], input$rok[2])) %>% 
        pull(data) %>% 
        range()
      
      updateDateRangeInput(
        inputId = "daty",
        min = daty_range[1],
        max = daty_range[2],
        start = daty_range[1],
        end = daty_range[2]
      )
    }
  )
  
  observe({
    if(input$czy_edu_dist){
      showElement("edu_dist")
    } else {
      hideElement("edu_dist")
    }
  })
  
  ## filter data ----
  
  filter_by_input <- function(data, var){
    data %>% 
      {if(!is.null(input[[var]])){
        filter(., .data[[var]] %in% input[[var]])
      } else {.}
      }
  }
  
  si_app <- reactive({
    si_wroclaw %>% 
      filter(between(rok, input$rok[1], input$rok[2])) %>%
      {if(input$ulica == ""){
        .
      } else {
        if(input$pozwalaj_na_literowki) {
          filter(., 
                 agrepl(input$ulica, ulica, ignore.case = TRUE, max.distance = 2)
                 | agrepl(input$ulica, ulica_x, ignore.case = TRUE, max.distance = 2))
        } else {
          filter(., grepl(input$ulica, ulica, ignore.case = TRUE)
                 | grepl(input$ulica, ulica_x, ignore.case = TRUE))
        }
      }} %>% 
      {if(input$ulica2 == ""){
        .
      } else {
        if(input$pozwalaj_na_literowki) {
          filter(.,
                 (agrepl(input$ulica, ulica, ignore.case = TRUE, max.distance = 2)
                  & agrepl(input$ulica2, ulica_x, ignore.case = TRUE, max.distance = 2))
                 | (agrepl(input$ulica, ulica_x, ignore.case = TRUE, max.distance = 2)
                    & agrepl(input$ulica2, ulica, ignore.case = TRUE, max.distance = 2)))
        } else {
          filter(., 
                 (grepl(input$ulica, ulica, ignore.case = TRUE)
                  & grepl(input$ulica2, ulica_x, ignore.case = TRUE))
                 | (grepl(input$ulica, ulica_x, ignore.case = TRUE)
                    & grepl(input$ulica2, ulica, ignore.case = TRUE)))
        }
      }} %>% 
      filter_by_input("osiedle") %>% 
      {if(input$czy_edu_dist){
        filter(., edu_dist*1000 <= input$edu_dist)
      } else {
        .
      }} %>% 
      filter(data >= coalesce(input$daty[1],min(data))
             & data <= coalesce(input$daty[2],max(data))) %>% 
      filter(godzina >= strftime(input$czas_od, "%R")
             & godzina <= "if"(strftime(input$czas_do, "%R") == "00:00", "23:59", strftime(input$czas_do, "%R"))) %>% 
      filter_by_input("oswietlenie") %>% 
      filter_by_input("miejsce") %>% 
      filter_by_input("rodzaj_drogi") %>% 
      filter_by_input("sygnalizacja") %>% 
      filter_by_input("skrzyzowanie") %>% 
      filter_by_input("zabudowany") %>% 
      {if(is.null(input$predkosc) | (input$predkosc[1] == 0 & input$predkosc[2] == 140)){
        .
      } else {
        filter(., between(predkosc_max, input$predkosc[1], input$predkosc[2]))
      }} %>% 
      {if(is.null(input$skutki)){
        .
      } else if(!input$rev_skutki){
        filter(., (("Śmierć na miejscu" %in% input$skutki) & (skutek_smierc_na_miejscu>0))
               | (("Śmierć 30 dni" %in% input$skutki) & (skutek_smierc_30dni>0))
               | (("Lekko ranny" %in% input$skutki) & (skutek_ranny_lekko>0))
               | (("Ciężko ranny" %in% input$skutki) & (skutek_ranny_ciezko>0))
        )
      } else {
        filter(., !((("Śmierć na miejscu" %in% input$skutki) & (skutek_smierc_na_miejscu>0))
                    | (("Śmierć 30 dni" %in% input$skutki) & (skutek_smierc_30dni>0))
                    | (("Lekko ranny" %in% input$skutki) & (skutek_ranny_lekko>0))
                    | (("Ciężko ranny" %in% input$skutki) & (skutek_ranny_ciezko>0)))
        )
      }} %>% 
      {if(is.null(input$skutki_pr)){
        .
      } else if(!input$rev_skutki_pr){
        filter(., (("Pieszy - Śmierć na miejscu" %in% input$skutki_pr) & (pieszy_skutek_smierc_na_miejscu>0))
               | (("Pieszy - Śmierć 30 dni" %in% input$skutki_pr) & (pieszy_skutek_smierc_30dni>0))
               | (("Pieszy - Lekko ranny" %in% input$skutki_pr) & (pieszy_skutek_ranny_lekko>0))
               | (("Pieszy - Ciężko ranny" %in% input$skutki_pr) & (pieszy_skutek_ranny_ciezko>0))
               | (("Rowerzysta - Śmierć na miejscu" %in% input$skutki_pr) & (rower_skutek_smierc_na_miejscu>0))
               | (("Rowerzysta - Śmierć 30 dni" %in% input$skutki_pr) & (rower_skutek_smierc_30dni>0))
               | (("Rowerzysta - Lekko ranny" %in% input$skutki_pr) & (rower_skutek_ranny_lekko>0))
               | (("Rowerzysta - Ciężko ranny" %in% input$skutki_pr) & (rower_skutek_ranny_ciezko>0))
        )
      } else {
        filter(., !((("Pieszy - Śmierć na miejscu" %in% input$skutki_pr) & (pieszy_skutek_smierc_na_miejscu>0))
                    | (("Pieszy - Śmierć 30 dni" %in% input$skutki_pr) & (pieszy_skutek_smierc_30dni>0))
                    | (("Pieszy - Lekko ranny" %in% input$skutki_pr) & (pieszy_skutek_ranny_lekko>0))
                    | (("Pieszy - Ciężko ranny" %in% input$skutki_pr) & (pieszy_skutek_ranny_ciezko>0))
                    | (("Rowerzysta - Śmierć na miejscu" %in% input$skutki_pr) & (rower_skutek_smierc_na_miejscu>0))
                    | (("Rowerzysta - Śmierć 30 dni" %in% input$skutki_pr) & (rower_skutek_smierc_30dni>0))
                    | (("Rowerzysta - Lekko ranny" %in% input$skutki_pr) & (rower_skutek_ranny_lekko>0))
                    | (("Rowerzysta - Ciężko ranny" %in% input$skutki_pr) & (rower_skutek_ranny_ciezko>0)))
        )
      }} %>% 
      filter_by_input("rodzaj_zdarzenia") %>% 
      filter_by_input("wina") %>% 
      filter_by_input("wina_kierujacy") %>% 
      filter_by_input("wina_pieszy") %>% 
      filter_by_input("wina_inne") %>% 
      {if(is.null(input$uczestnicy)){
        .
      } else if(input$any_all == "or"){
        filter(., (("Pieszy" %in% input$uczestnicy) & (uczestnik_pieszy>0))
               | (("Rower" %in% input$uczestnicy) & (uczestnik_rower>0))
               | (("Skuter" %in% input$uczestnicy) & (uczestnik_skuter>0))
               | (("Motocykl" %in% input$uczestnicy) & (uczestnik_motocykl>0))
               | (("Quad" %in% input$uczestnicy) & (uczestnik_quad>0))
               | (("Samochód osobowy" %in% input$uczestnicy) & (uczestnik_sam_osobowy>0))
               | (("Samochód ciężarowy" %in% input$uczestnicy) & (uczestnik_sam_ciezarowy>0))
               | (("Autobus" %in% input$uczestnicy) & (uczestnik_autobus>0))
               | (("Tramwaj, trolejbus" %in% input$uczestnicy) & (uczestnik_tram>0))
               | (("Traktor" %in% input$uczestnicy) & (uczestnik_traktor>0))
               | (("Pojazd wolnobieżny" %in% input$uczestnicy) & (uczestnik_poj_wolnobiezny>0))
               | (("Pociąg" %in% input$uczestnicy) & (uczestnik_pociag>0))
               | (("Pojazd uprzywilejowany" %in% input$uczestnicy) & (uczestnik_poj_uprzywilejowany>0))
               | (("Przewożący mat. niebezpieczne" %in% input$uczestnicy) & (uczestnik_poj_przew_mat_niebezp>0))
               | (("Nieznany" %in% input$uczestnicy) & (uczestnik_nieznany>0))
        )
      } else {
        filter(., (ifelse(rep("Pieszy" %in% input$uczestnicy, nrow(.)), uczestnik_pieszy>0, TRUE))
               & (ifelse(rep("Rower" %in% input$uczestnicy, nrow(.)), uczestnik_rower>0, TRUE))
               & (ifelse(rep("Skuter" %in% input$uczestnicy, nrow(.)), uczestnik_skuter>0, TRUE))
               & (ifelse(rep("Motocykl" %in% input$uczestnicy, nrow(.)), uczestnik_motocykl>0, TRUE))
               & (ifelse(rep("Quad" %in% input$uczestnicy, nrow(.)), uczestnik_quad>0, TRUE))
               & (ifelse(rep("Samochód osobowy" %in% input$uczestnicy, nrow(.)), uczestnik_sam_osobowy>0, TRUE))
               & (ifelse(rep("Samochód ciężarowy" %in% input$uczestnicy, nrow(.)), uczestnik_sam_ciezarowy>0, TRUE))
               & (ifelse(rep("Autobus" %in% input$uczestnicy, nrow(.)), uczestnik_autobus>0, TRUE))
               & (ifelse(rep("Tramwaj, trolejbus" %in% input$uczestnicy, nrow(.)), uczestnik_tram>0, TRUE))
               & (ifelse(rep("Traktor" %in% input$uczestnicy, nrow(.)), uczestnik_traktor>0, TRUE))
               & (ifelse(rep("Pojazd wolnobieżny" %in% input$uczestnicy, nrow(.)), uczestnik_poj_wolnobiezny>0, TRUE))
               & (ifelse(rep("Pociąg" %in% input$uczestnicy, nrow(.)), uczestnik_pociag>0, TRUE))
               & (ifelse(rep("Pojazd uprzywilejowany" %in% input$uczestnicy, nrow(.)), uczestnik_poj_uprzywilejowany>0, TRUE))
               & (ifelse(rep("Przewożący mat. niebezpieczne" %in% input$uczestnicy, nrow(.)), uczestnik_poj_przew_mat_niebezp>0, TRUE))
               & (ifelse(rep("Nieznany" %in% input$uczestnicy, nrow(.)), uczestnik_nieznany>0, TRUE))
        )
      }}
  })
  
  szkoly_app <- reactive({
    list(
      data = if(input$czy_edu_dist) szkoly else head(szkoly,0),
      radius = input$edu_dist
    )
  })
  
  output$rekordy <- renderText(
    sprintf("Wybrano %s zdarzeń", format(nrow(si_app()), big.mark = " "))
  )
  
  react_vals <- reactiveValues(trigger = 0)
  react_vals$leaflet_data <- isolate(si_app())
  react_vals$szkoly <- isolate(szkoly_app())
  
  observeEvent(
    input$przelicz,
    {
      if(nrow(si_app())>20.e3){
        showModal(
          modalDialog(
            title = "Uwaga!",
            "Wybrano ponad 20 tysięcy zdarzeń - może to spowodować spowolnienie 
            działania aplikacji lub przekroczyć dostępny na serwerze limit pamięci!",
            easyClose = FALSE,
            footer = list(
              actionButton("warn_ok", "Rozumiem"),
              actionButton("warn_cancel", "Nie ryzykuję")
            )
          )) 
      } else if(nrow(si_app()) == 0){
        showModal(
          modalDialog(
            title = "Uwaga!",
            "Nie wybrano żadnych zdarzeń - zmień filtry.",
            easyClose = TRUE,
            footer = modalButton("OK")
          )) 
      } else {
        react_vals$leaflet_data <- isolate(si_app())
        react_vals$szkoly <- isolate(szkoly_app())
        react_vals$trigger <- react_vals$trigger + 1
      }
    })
  
  observeEvent(
    input$warn_ok, {
      react_vals$leaflet_data <- isolate(si_app())
      react_vals$szkoly <- isolate(szkoly_app())
      removeModal()
      react_vals$trigger <- react_vals$trigger + 1
    }
  )
  
  observeEvent(
    input$warn_cancel, {
      ##reset all inputs to defaults
      # shinyjs::reset("rok")
      # shinyjs::reset("kontrolki")
      removeModal()
    }
  )
  
  ## reset inputs ----
  
  observeEvent(
    input$reset_all, {
      shinyjs::reset("rok")
      shinyjs::reset("kontrolki")
    }
  )
  
  
  ## update map of accidents ----
  
  observe({
    react_vals$trigger
    
    leafletProxy("mapka") %>% 
      clearGroup("zdarzenia") %>%
      clearGroup("szkoly") %>%
      addCircleMarkers(data = react_vals$szkoly$data,
                       lng = ~lng, lat = ~lat,
                       radius = 5,
                       stroke = FALSE,
                       fillColor = "grey",
                       fillOpacity = 0.4,
                       group = "szkoly") %>% 
      addCircles(data = react_vals$szkoly$data,
                 lng = ~lng, lat = ~lat,
                 radius = react_vals$szkoly$radius,
                 stroke = FALSE,
                 fillColor = "grey",
                 fillOpacity = 0.3,
                 group = "szkoly") %>%
      addCircleMarkers(data = react_vals$leaflet_data,
                       lng = ~lon, lat = ~lat,
                       label = ~lapply(as.list(popup), shiny::HTML),
                       popup = ~popup,
                       color = ~color,
                       clusterOptions = TRUE,
                       group = "zdarzenia") %>% 
      {if(nrow(react_vals$szkoly$data) > 0) {
        addLegend(.,
                  position = "bottomleft",
                  colors = c(kolory, "grey"),
                  labels = c(names(kolory), "Szkoły"),
                  layerId = "legenda")
      } else {
        addLegend(.,
                  position = "bottomleft",
                  colors = kolory,
                  labels = names(kolory),
                  layerId = "legenda")
      }}
  })
  
  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX#
  ## Wykresy / raporty ---------------------
  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX#
  
  #Reactive UI
  output$opcje_raportu <- renderUI({
    if (input$typ_raportu == "") {
      #placeholder
    } else {
      
      choice_list <- list()
      choice_list[["Czas zdarzeń"]] <-
        c("Zmienność roczna" = "rok",
          "Zmienność miesięczna" = "miesiac",
          "Dzień tygodnia" = "dow",
          "Godzina" = "godz",
          "Warunki atmosferyczne" = "war_atmosf",
          "Oświetlenie" = "oswietlenie")
      choice_list[["Lokalizacja zdarzeń"]] <-
        c("Ulica",
          "Skrzyżowanie",
          "Osiedle" = "osiedle")
      choice_list[["Rodzaj zdarzeń"]] <-
        c("Rodzaj zdarzenia" = "rodzaj_zdarzenia")
      choice_list[["Miejsce zdarzeń"]] <-
        c("Charakterystyka miejsca" = "miejsce",
          "Geometra drogi" = "geometria",
          "Sygnalizacja świetlna" = "sygnalizacja",
          "Prędkość dopuszczalna" = "predkosc_max",
          "Rodzaj skrzyżowania" = "skrzyzowanie",
          "Teren zabudowany" = "zabudowany")
      choice_list[["Przyczyny zdarzeń"]] <-
        c("Wina" = "wina",
          "Przyczyny - kierowcy" = "wina_kierujacy",
          "Przyczyny - piesi" = "wina_pieszy",
          "Inne przyczyny" = "wina_inne")
      
      
      prettyRadioButtons("jaki_raport", "Statystyka:", 
                         choice_list[[input$typ_raportu]])
    }
    
  })
  #dane do raportów ----
  si_rap <- reactive({
    validate(need(input$jaki_raport, "Wybierz typ raportu"))
    
    if(input$use_bbox){
      validate(need(input$mapka_bounds, 
                    "Przejdź do zakładki z mapą, aby aplikacja wczytała granice wyświetlanego widoku"))
    }
    
    
    react_vals$trigger
    
    si_tmp <- react_vals$leaflet_data %>%
      mutate(
        rok = lubridate::year(data),
        miesiac = lubridate::month(data),
        dow = lubridate::wday(data, label = TRUE, abbr = FALSE, week_start = 1), #, locale = "Polish_Poland.1250"
        godz = substr(godzina, 1, 2),
        skrzyz = ifelse(is.na(ulica_x), NA, paste(ulica,"/",ulica_x))
      ) %>% 
      {if(input$use_bbox){
        filter(., 
               between(lat, input$mapka_bounds$south, input$mapka_bounds$north),
               between(lon, input$mapka_bounds$west, input$mapka_bounds$east))
      } else . }
    
    si_tmp <- if(is.null(input$jaki_raport)){
      data.frame("Wybierz_typ_raportu" = character())
    } else if(input$jaki_raport == "Ulica"){
      rbind(
        si_tmp %>% filter(!is.na(ulica)) %>% select(ulica, starts_with("skutek")),
        si_tmp %>% filter(!is.na(ulica_x)) %>% select(ulica = ulica_x, starts_with("skutek"))
      ) %>%
        group_by(ulica) %>% 
        summarise(zdarzenia = n(),
                  zmarli = sum(skutek_smierc_na_miejscu) + sum(skutek_smierc_30dni),
                  ciężko_ranni = sum(skutek_ranny_ciezko),
                  lekko_ranni = sum(skutek_ranny_lekko)
                  # zmarli_zdarzenia = sum((skutek_smierc_na_miejscu | skutek_smierc_30dni)>0),
                  # ciężko_ranni_zdarzenia = sum(skutek_ranny_ciezko>0),
                  # lekko_ranni_zdarzenia = sum(skutek_ranny_lekko>0)
        ) %>% 
        arrange(desc(zdarzenia))
    } else if(input$jaki_raport == "Skrzyżowanie"){
      si_tmp %>% 
        select(starts_with("ulica"), starts_with("skutek")) %>% 
        filter(!is.na(ulica) & !is.na(ulica_x)) %>% 
        mutate(ulica = str_trim(ulica),
               ulica_x = str_trim(ulica_x)) %>% 
        filter(ulica != ulica_x) %>% 
        group_by(ulica, ulica_x) %>% 
        summarise(zdarzenia = n(),
                  zmarli = sum(skutek_smierc_na_miejscu) + sum(skutek_smierc_30dni),
                  ciężko_ranni = sum(skutek_ranny_ciezko),
                  lekko_ranni = sum(skutek_ranny_lekko)
                  # zmarli_zdarzenia = sum((skutek_smierc_na_miejscu | skutek_smierc_30dni)>0),
                  # ciężko_ranni_zdarzenia = sum(skutek_ranny_ciezko>0),
                  # lekko_ranni_zdarzenia = sum(skutek_ranny_lekko>0)
        ) %>% 
        ungroup %>% 
        left_join(.,., 
                  by = c("ulica" = "ulica_x", "ulica_x" = "ulica"),
                  suffix = c(".x",".y")
        ) %>% 
        mutate(winning = ifelse(zdarzenia.x > zdarzenia.y, "x", "y")) %>% 
        anti_join(.,
                  filter(., winning == "y"),
                  by = c("ulica" = "ulica", "ulica_x" = "ulica_x")) %>% 
        mutate(skrzyz = paste(ulica,"/",ulica_x),
               zdarzenia = zdarzenia.x + if_na(zdarzenia.y),
               zmarli = zmarli.x + if_na(zmarli.y),
               ciężko_ranni = ciężko_ranni.x + if_na(ciężko_ranni.y),
               lekko_ranni = lekko_ranni.x + if_na(lekko_ranni.y)
               # zmarli_zdarzenia = zmarli_zdarzenia.x + if_na(zmarli_zdarzenia.y),
               # ciężko_ranni_zdarzenia = ciężko_ranni_zdarzenia.x + if_na(ciężko_ranni_zdarzenia.y),
               # lekko_ranni_zdarzenia = lekko_ranni_zdarzenia.x + if_na(lekko_ranni_zdarzenia.y)
        ) %>% 
        select(-ends_with(".x")) %>% 
        select(-ends_with(".y")) %>% 
        select(-starts_with("ulica")) %>% 
        select(-winning) %>% 
        rename(skrzyzowanie = skrzyz) %>% 
        arrange(desc(zdarzenia))
      
    } else {
      si_tmp %>% 
        mutate_if(is.character, ~coalesce(.,"b/d")) %>% 
        group_by(.data[[input$jaki_raport]]) %>% 
        summarise(zdarzenia = n(),
                  zmarli = sum(skutek_smierc_na_miejscu) + sum(skutek_smierc_30dni),
                  ciężko_ranni = sum(skutek_ranny_ciezko),
                  lekko_ranni = sum(skutek_ranny_lekko)
                  # zmarli_zdarzenia = sum((skutek_smierc_na_miejscu | skutek_smierc_30dni)>0),
                  # ciężko_ranni_zdarzenia = sum(skutek_ranny_ciezko>0),
                  # lekko_ranni_zdarzenia = sum(skutek_ranny_lekko>0)
        ) %>% 
        arrange(desc(zdarzenia))
    }
    if(req(input$typ_raportu) == "Czas zdarzeń") {
      si_tmp <- si_tmp %>% arrange(.data[[req(input$jaki_raport)]])
    }
    si_tmp
  })
  
  output$wizualizacja_output <- renderUI({
    if(input$wizualizacja == "Tabela"){
      withSpinner(DT::dataTableOutput("raport_table"))
    } else {
      fluidRow(
        prettyRadioButtons("co_na_wykresie", NULL,
                           c("Zdarzenia", "Ofiary"), inline = TRUE),
        withSpinner(plotlyOutput("raport_plot"))
      )
    }
  })
  
  
  si_plot <- reactive({
    si_tmp <- si_rap()
    
    group_var <- names(si_tmp)[1]
    
    if(input$jaki_raport %in% c("Ulica","Skrzyżowanie") | nrow(si_tmp)>30) {
      si_tmp <- si_tmp %>% head(25)
    }
    
    si_tmp <- if (input$co_na_wykresie == "Zdarzenia") {
      si_tmp %>% 
        select(all_of(c(group_var, "zdarzenia"))) %>%
        rename("gv" = group_var) %>% 
        filter(gv != "b/d") %>% 
        mutate(gv = factor(gv, levels = gv)) %>% 
        plot_ly(x = ~gv, y=~zdarzenia, type = "bar") %>% 
        layout(xaxis = list(title = group_var))
    } else {
      si_tmp %>% 
        rename("gv" = group_var) %>% 
        filter(gv != "b/d") %>% 
        mutate(gv = factor(gv, levels = gv)) %>% 
        plot_ly(x = ~gv, y=~zmarli, type = "bar", name = "zmarli") %>% 
        add_trace(y=~ciężko_ranni, name = "ciężko ranni") %>% 
        add_trace(y=~lekko_ranni, name = "lekko ranni") %>% 
        layout(barmode = "stack",
               xaxis = list(title = group_var),
               yaxis = list(title = "ofiary"))
    }
    
  })
  
  output$raport_table <- DT::renderDataTable(DT::datatable(
    si_rap()
    , options = list(autoWidth = TRUE, pageLength = 25, dom = 'irtlp')
  ))
  output$raport_plot <- renderPlotly({
    si_plot()
  })
  
}
