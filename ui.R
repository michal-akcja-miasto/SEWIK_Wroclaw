navbarPage(
  "Wypadki we Wrocławiu", id="nav", collapsible=TRUE,
  tabPanel(
    "Mapa interaktywna",
    useShinyjs(),
    
    div(class="outer",
        
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("script.js")
        ),
        
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("mapka", width="100%", height="100%"),
        
        # Shiny versions prior to 0.11 should use class = "modal" instead.
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = FALSE, top = 61, left = 20, right = "auto", bottom = "auto",
          width = 330, height = "auto",
          
          h3("Wypadki na ulicach Wrocławia"),
          
          verbatimTextOutput("rekordy", placeholder = TRUE),
          
          actionButton("przelicz", "Zastosuj filtry i pokaż na mapie"),
          
          hr(),
          
          dropdown(
            fluidPage(
              id = "kontrolki",
              style='max-height: min(calc(var(--app-height) - 355px), 60vh); overflow-y: auto;',
              fluidRow(
                id = "row1",
                column(4,
                       h3(list(icon("search-location"), "Lokalizacja")),
                       textInput("ulica", 
                                 label = "Ulica", 
                                 value = NULL),
                       textInput("ulica2", 
                                 label = "Ulica 2 (skrzyżowanie)", 
                                 value = NULL),
                       #pozwalaj_na_literowki
                       prettySwitch("pozwalaj_na_literowki", 
                                    'Pozwalaj na literówki w polach "ulica"', 
                                    value = FALSE,
                                    fill = TRUE,
                                    status = "primary"),
                       selectizeInput("osiedle",
                                      label = "Osiedle",
                                      choices = sort(unique(si_wroclaw$osiedle)),
                                      selected = NULL,
                                      multiple = TRUE),
                       hr(),
                       
                       prettySwitch("czy_edu_dist", 
                                    'Zdarzenia w poblizu szkół', 
                                    value = FALSE,
                                    fill = TRUE,
                                    status = "primary"),
                       hidden(sliderInput("edu_dist", 
                                          label = "Odległość od szkoły [m]", 
                                          min = 100, 
                                          max = 500, 
                                          value = 300, 
                                          step = 100))
                ),
                
                column(4,
                       h3(list(icon("clock"), "Czas")),
                       sliderInput("rok",
                                   "Wybierz lata danych",
                                   min = min(si_wroclaw$rok),
                                   max = max(si_wroclaw$rok),
                                   value = c(max(si_wroclaw$rok)-1, max(si_wroclaw$rok)),
                                   step = 1,
                                   sep = ""),
                       dateRangeInput("daty", 
                                      label = "Data",
                                      start = min(si_wroclaw$data),
                                      end = max(si_wroclaw$data),
                                      min = min(si_wroclaw$data),
                                      max = max(si_wroclaw$data),
                                      startview = "year",
                                      language = "pl",
                                      separator = " - "),
                       shinyTime::timeInput("czas_od",
                                            label = "Godzina od:",
                                            value = NULL,
                                            seconds = FALSE),
                       shinyTime::timeInput("czas_do",
                                            label = "Godzina do:",
                                            value = strptime("23:59","%R"),
                                            seconds = FALSE),
                       selectizeInput("oswietlenie",
                                      label = "Warunki oświetleniowe",
                                      choices = sort(unique(si_wroclaw$oswietlenie)),
                                      selected = NULL,
                                      multiple = TRUE)
                ),
                column(4,
                       h3(list(icon("map-marker-alt"), "Miejsce")),
                       selectizeInput("miejsce",
                                      label = "Miejsce zdarzenia",
                                      choices = sort(unique(si_wroclaw$miejsce)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("rodzaj_drogi",
                                      label = "Rodzaj drogi",
                                      choices = sort(unique(si_wroclaw$rodzaj_drogi)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("sygnalizacja",
                                      label = "Sygnalizacja świetlna",
                                      choices = sort(unique(si_wroclaw$sygnalizacja)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("skrzyzowanie",
                                      label = "Rodzaj skrzyżowania",
                                      choices = sort(unique(si_wroclaw$skrzyzowanie)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("zabudowany",
                                      label = "Obszar zabudowany",
                                      choices = sort(unique(si_wroclaw$zabudowany)),
                                      selected = NULL,
                                      multiple = TRUE),
                       sliderInput("predkosc", 
                                   label = "Prędkość maksymalna", 
                                   min = 0, 
                                   max = 140, 
                                   value = c(0,140), 
                                   step = 10),
                       
                )
                
              ),
              fluidRow(
                id = "row2",
                column(4,
                       h3(list(icon("ambulance"), "Skutki")),
                       selectizeInput("skutki",
                                      label = "Obrażenia ofiar",
                                      choices = sort(c("Śmierć na miejscu",
                                                       "Śmierć 30 dni",
                                                       "Lekko ranny",
                                                       "Ciężko ranny")),
                                      selected = NULL,
                                      multiple = TRUE),
                       prettySwitch("rev_skutki", 
                                    "Odwróć działanie powyższego filtra", 
                                    value = FALSE,
                                    fill = TRUE,
                                    status = "primary"),
                       selectizeInput("skutki_pr",
                                      label = "Obrażenia pieszych i rowerzystów",
                                      choices = sort(c("Pieszy - Śmierć na miejscu",
                                                       "Pieszy - Śmierć 30 dni",
                                                       "Pieszy - Lekko ranny",
                                                       "Pieszy - Ciężko ranny",
                                                       "Rowerzysta - Śmierć na miejscu",
                                                       "Rowerzysta - Śmierć 30 dni",
                                                       "Rowerzysta - Lekko ranny",
                                                       "Rowerzysta - Ciężko ranny"
                                      )),
                                      selected = NULL,
                                      multiple = TRUE),
                       prettySwitch("rev_skutki_pr", 
                                    "Odwróć działanie powyższego filtra", 
                                    value = FALSE,
                                    fill = TRUE,
                                    status = "primary")
                ),
                column(4,
                       h3(list(icon("car-crash"), "Przyczyny")),
                       selectizeInput("rodzaj_zdarzenia",
                                      label = "Rodzaj zdarzenia",
                                      choices = sort(unique(si_wroclaw$rodzaj_zdarzenia)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("wina",
                                      label = "Winny",
                                      choices = sort(unique(si_wroclaw$wina)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("wina_kierujacy",
                                      label = "Przyczyny kierujących",
                                      choices = sort(unique(si_wroclaw$wina_kierujacy)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("wina_pieszy",
                                      label = "Przyczyny pieszych",
                                      choices = sort(unique(si_wroclaw$wina_pieszy)),
                                      selected = NULL,
                                      multiple = TRUE),
                       selectizeInput("wina_inne",
                                      label = "Inne przyczyny",
                                      choices = sort(unique(si_wroclaw$wina_inne)),
                                      selected = NULL,
                                      multiple = TRUE)
                ),
                column(4,
                       h3(list(icon("users"), "Uczestnicy")),
                       prettyRadioButtons("any_all",
                                          NULL,
                                          choices = c(
                                            "Przynajmniej jeden z zaznaczonych (OR)" = "or",
                                            "Wszyscy zaznaczeni (AND)" = "and"
                                          )),
                       checkboxGroupInput("uczestnicy",
                                          label = NULL,
                                          choices = c("Pieszy",
                                                      "Rower",
                                                      "Skuter",
                                                      "Motocykl",
                                                      "Quad",
                                                      "Samochód osobowy",
                                                      "Samochód ciężarowy",
                                                      "Autobus",
                                                      "Tramwaj, trolejbus",
                                                      "Traktor",
                                                      "Pojazd wolnobieżny",
                                                      "Pociąg",
                                                      "Pojazd uprzywilejowany",
                                                      "Przewożący mat. niebezpieczne",
                                                      "Nieznany")
                       )
                )),
              
              actionButton("reset_all",
                           "Resetuj filtry",
                           style = "background-color:lightgray",
                           icon = icon("redo"),
                           width = "100%")
              
            ),
            style = "unite",
            status = "primary",
            icon = icon("sliders-h"),
            width = "calc(100vw - 57px)",
            label = "Wyświetl filtry"
          )
          
        ),
        
        absolutePanel(
          id = "cite", class = "panel panel-default hidden-xs", fixed = TRUE,
          draggable = FALSE, top = "auto", left = "auto", right = 20, bottom = 20,
          width = 330, height = "auto",
          
          p("Dane z SEWIK dla Wrocławia"),
          p(list("kontakt: ", a("michal.kucharczyk@akcjamiasto.org", 
                                href = "mailto:michal.kucharczyk@akcjamiasto.org")))
          
        )
    )
  ),
  
  tabPanel(
    "Wykresy/raporty",
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        p(icon("info-circle"), HTML("Filtry danych wybierz w zakładce z mapą.<br> 
          Tylko wyświetlone na mapie filtry mają zastosowanie.")),
        br(),
        HTML("Pokaż tylko zdarzenia w aktualnie wyświetlanym widoku mapy (przybliż lub przesuń widok w zakładce z mapą)"),
        prettySwitch("use_bbox", 
                     "Włącz", 
                     value = FALSE,
                     fill = TRUE,
                     status = "primary"),
        hr(),
        div(h4("Jaki raport?"), style = "padding-bottom: 5px;"),
        prettyRadioButtons("wizualizacja",
                           label = NULL,
                           choices = c("Wykres", "Tabela")),
        selectInput("typ_raportu","Typ raportu",
                    c("Wybierz typ" = "",
                      "Czas zdarzeń",
                      "Lokalizacja zdarzeń",
                      "Rodzaj zdarzeń",
                      "Miejsce zdarzeń",
                      "Przyczyny zdarzeń"
                    )),
        uiOutput("opcje_raportu")),
      mainPanel = mainPanel(
        uiOutput("wizualizacja_output")
      )
    )
  )
)
