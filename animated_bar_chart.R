library(dplyr)
library(readr)
library(zoo)
library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(tidyverse)

options(scipen=999) #per non usare la notazione scientifica

url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

df_covid <- read_csv(file = url)
dati<-(df_covid[c("data", "denominazione_regione", "totale_casi")])

#Organizzo dati e assegno un ranking

dati_fine <- dati %>%
  mutate(data = as.Date(data)) %>%
  group_by(data) %>%
  arrange(data, desc(totale_casi)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <=21)

#Correggo nomi regioni per renderli più leggibili

dati_fine$denominazione_regione[dati_fine$denominazione_regione == "Emilia-Romagna"] <-"E. Romagna"
dati_fine$denominazione_regione[dati_fine$denominazione_regione == "Friuli Venezia Giulia"] <-"F. V. Giulia"
dati_fine$denominazione_regione[dati_fine$denominazione_regione == "P.A. Bolzano"] <-"Bolzano"
dati_fine$denominazione_regione[dati_fine$denominazione_regione == "P.A. Trento"] <-"Trento"
dati_fine$denominazione_regione[dati_fine$denominazione_regione == "Valle d'Aosta"] <-"V. d'Aosta"

# Animazione

anim <- dati_fine %>%
  ggplot() +
  geom_col(aes(rank, totale_casi, fill = denominazione_regione)) +
  geom_text(aes(rank, totale_casi, label = totale_casi), hjust=-0.1,  col = "black", size = 5) +
  geom_text(aes(rank, y=0 , label = denominazione_regione), hjust=1.1,   col = "black", size = 5) + 
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  geom_hline(yintercept = 800000,color = "grey", size=1)+
  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=25, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(data, transition_length = 2, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Totale casi COVID-19',  
       subtitle  =  "{closest_state}",
       caption  = "Grafico di Alessio Garau | Dati: https://github.com/pcm-dpc/COVID-19") 

#GIF

animate(anim, 1200, fps = 25,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif")) 

#MP4

animate(anim, duration = 1200, fps = 25, renderer = av_renderer())
anim_save("output.mp4", anim)
