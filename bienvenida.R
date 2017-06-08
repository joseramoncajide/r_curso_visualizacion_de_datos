##########################################################################
# Jose Cajide - @jrcajide - https://www.linkedin.com/in/jrcajide/
# Motivación
##########################################################################

list.of.packages <- c("tidyverse", "ggmap", "hrbrthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("hrbrmstr/darksky")

library(ggmap)
library(hrbrthemes)
library(darksky)

Sys.setenv("DARKSKY_API_KEY" = "c39232b69c6d8f1f0564c59ef2bb8996")

isdefe <- geocode("Isdefe", source = "google")

now <- get_current_forecast(isdefe[2], isdefe[1])
temp <- round( (now$currently$temperature -  32) / 1.8, 0)

ggmap(get_map(isdefe, zoom = 15, maptype = "watercolor")) +
  geom_point(data=isdefe, size = 7, shape = 13, color = "red") +
  labs(title="Visualización de datos con R", subtitle= "Isdefe, 9 de Junio", caption = paste(temp, "º en MADRID")) +
  xlab("") +
  ylab("") +
  theme_ipsum_rc(grid='Y', axis=F, ticks = F) +
  theme( legend.position = "bottom", axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(), axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())
ggsave('img/intro.png')
