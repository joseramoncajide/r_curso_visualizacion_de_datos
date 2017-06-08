##########################################################################
# Jose Cajide - @jrcajide - https://www.linkedin.com/in/jrcajide/
# Curso de introducción a R: Webscraping
##########################################################################

rm(list=ls()) 
cat("\014")

# instalar librerías ------------------------------------------------------
# 
list.of.packages <- c("tidyverse", "rvest", "stringr", "treemap", "viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(rvest)
library(stringr)
library(treemap)
library(viridis)


#----------------------------------------------------------------------------
# rvest
#----------------------------------------------------------------------------


url_madrid <- "http://resultados.elpais.com/elecciones/2011/municipales/12/28/79.html"
html_madrid <- read_html(url_madrid)

madrid <- html_madrid %>% html_node("#tablaVotosPartidos") %>% html_table()
names(madrid) <- c("partidos", "concejales", "votos", "porcentaje")

treemap(madrid, 
        index=c("partidos"), 
        vSize="votos", 
        type="index",
        border.lwds=.3,
        border.col="#FFFFFF",
        palette=viridis(15))