library(dplyr)
library(shiny)
library(lubridate)
library(leaflet)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tidyr)
library(plotly)
library(stringr)
library(shinycssloaders)
options(spinner.color = "grey")
options(spinner.type = 8)

si_wroclaw <- readRDS("dane/si_wroclaw_os.rds")
szkoly <- readRDS("dane/szkoly.rds") # https://gis.um.wroc.pl/imap/?gpmap=eduk
osiedla <- readRDS("dane/osiedla.rds") # http://geoportal.wroclaw.pl/zasoby/?zasob=granice_osiedla

kolory <- c(
  `Ofiary śmiertelne` = "black",
  `Ciężko ranni` = "firebrick", 
  `Lekko ranni` = "darkorange",
  `Kolizje` = "#03F"
)

szkoly <- szkoly %>% 
  distinct(lng, lat)

si_wroclaw <- si_wroclaw %>% 
  mutate(uczestnik_rower2 = ifelse(uczestnik_rower>0, ifelse(uczestnik_rower>1, "rowery", "rower"), NA),
         uczestnik_skuter2 = ifelse(uczestnik_skuter>0, ifelse(uczestnik_skuter>1, "skutery", "skuter"), NA),
         uczestnik_motocykl2 = ifelse(uczestnik_motocykl>0,ifelse(uczestnik_motocykl>1,"motocykle","motocykl"), NA),
         uczestnik_quad2 = ifelse(uczestnik_quad>0,ifelse(uczestnik_quad>1,"quady","quad"), NA),
         uczestnik_sam_osobowy2 = ifelse(uczestnik_sam_osobowy>0,ifelse(uczestnik_sam_osobowy>1,"sam.osobowe","sam.osobowy"), NA),
         uczestnik_sam_ciezarowy2 = ifelse(uczestnik_sam_ciezarowy>0,ifelse(uczestnik_sam_ciezarowy>1,"sam.ciężarowe","sam.ciężarowy"), NA),
         uczestnik_autobus2 = ifelse(uczestnik_autobus>0,ifelse(uczestnik_autobus>1,"autobusy","autobus"), NA),
         uczestnik_tram2 = ifelse(uczestnik_tram>0,ifelse(uczestnik_tram>1,"tramwaje","tramwaj"), NA),
         uczestnik_traktor2 = ifelse(uczestnik_traktor>0,ifelse(uczestnik_traktor>1,"traktory","traktor"), NA),
         uczestnik_poj_wolnobiezny2 = ifelse(uczestnik_poj_wolnobiezny>0,ifelse(uczestnik_poj_wolnobiezny>1,"poj.wolnobieżne","poj.wolnobieżny"), NA),
         uczestnik_pociag2 = ifelse(uczestnik_pociag>0,ifelse(uczestnik_pociag>1,"pociągi","pociąg"), NA),
         uczestnik_poj_uprzywilejowany2 = ifelse(uczestnik_poj_uprzywilejowany>0,ifelse(uczestnik_poj_uprzywilejowany>1,"poj.uprzywilejowane","poj.uprzywilejowany"), NA),
         uczestnik_poj_przew_mat_niebezp2 = ifelse(uczestnik_poj_przew_mat_niebezp >0,ifelse(uczestnik_poj_przew_mat_niebezp >1,"poj.przew.mat.niebezp.","poj.przew.mat.niebezp."), NA),
         uczestnik_nieznany2 = ifelse(uczestnik_nieznany>0,ifelse(uczestnik_nieznany>1,"nieznane","nieznany"), NA),
         uczestnik_pieszy2 = ifelse(uczestnik_pieszy>0,ifelse(uczestnik_pieszy>1,"piesi","pieszy"), NA)
  ) %>% 
  unite(uczestnicy, 
        uczestnik_rower2:uczestnik_pieszy2,
        sep = ", ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(popup = paste0(
    sprintf("ID = %s (kliknij po szczegóły na sewik.pl)<br>
             ulica = %s<br>
             osiedle = %s<br>
             data = %s<br>
             godzina = %s<br>
             oświetlenie = %s<br>
             rodzaj_zdarzenia = %s",
            paste0("<a href=http://sewik.pl/accident/",ID,' target="_blank">',ID,"</a>"),
            coalesce(ulica, "b/d"),
            coalesce(osiedle, "b/d"),
            data,
            godzina,
            oswietlenie,
            rodzaj_zdarzenia),
    sprintf("<br>Uczestnicy: %s", uczestnicy),
    ifelse(skutek_smierc_na_miejscu>0, "<br>• Śmierć na miejscu", ""),
    ifelse(skutek_smierc_30dni>0, "<br>• Śmierć 30 dni", ""),
    ifelse(skutek_ranny_ciezko>0, "<br>• Ciężko ranni", ""),
    ifelse(skutek_ranny_lekko>0, "<br>• Lekko ranni", ""),
    ifelse(pieszy_skutek_smierc_na_miejscu>0, "<br>• PIESI: Śmierć na miejscu", ""),
    ifelse(pieszy_skutek_smierc_30dni>0, "<br>• PIESI: Śmierć 30 dni", ""),
    ifelse(pieszy_skutek_ranny_lekko>0, "<br>• PIESI: Lekko ranni", ""),
    ifelse(pieszy_skutek_ranny_ciezko>0, "<br>• PIESI: Ciężko ranni", ""),
    ifelse(rower_skutek_smierc_na_miejscu>0, "<br>• ROWERZYŚCI: Śmierć na miejscu", ""),
    ifelse(rower_skutek_smierc_30dni>0, "<br>• ROWERZYŚCI: Śmierć 30 dni", ""),
    ifelse(rower_skutek_ranny_lekko>0, "<br>• ROWERZYŚCI: Lekko ranni", ""),
    ifelse(rower_skutek_ranny_ciezko>0, "<br>• ROWERZYŚCI: Ciężko ranni", "")
  )) %>% 
  mutate(color = case_when(
    skutek_smierc_na_miejscu>0 ~ setNames(kolory["Ofiary śmiertelne"], NULL),
    skutek_smierc_30dni>0 ~ setNames(kolory["Ofiary śmiertelne"], NULL),
    skutek_ranny_ciezko>0 ~ setNames(kolory["Ciężko ranni"], NULL),
    skutek_ranny_lekko>0 ~ setNames(kolory["Lekko ranni"], NULL),
    TRUE ~ setNames(kolory["Kolizje"], NULL)
  )) %>% 
  mutate(rok = year(data))

if_na <- function(x, sub = 0){ifelse(is.na(x),sub,x)}
