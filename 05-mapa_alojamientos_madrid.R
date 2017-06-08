##########################################################################
# Jose Cajide - @jrcajide - https://www.linkedin.com/in/jrcajide/
# Mapas
##########################################################################

# instalar librerías ------------------------------------------------------
list.of.packages <- c("xml2", "leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(xml2)
library(leaflet)

#----------------------------------------------------------------------------
# Obtención de datos desde XML
#----------------------------------------------------------------------------


alojamientos_madrid <- read_xml("http://datos.madrid.es/egob/catalogo/300032-10037102-turismo-alojamientos.xml")

# alojamientos_madrid %>%  xml_name()
# alojamientos_madrid %>% xml_children()
# alojamientos_madrid %>% xml_text()

nombres <- alojamientos_madrid %>% xml_find_all(".//name") %>% xml_text()
emails <- alojamientos_madrid %>% xml_find_all(".//email") %>% xml_text()
lat <- alojamientos_madrid %>% xml_find_all(".//latitude") %>% xml_text()
lon <- alojamientos_madrid %>% xml_find_all(".//longitude") %>% xml_text()

alojamientos_madrid <- as_data_frame(cbind(nombres, emails, lat, lon))

alojamientos_madrid$lat <- as.numeric(alojamientos_madrid$lat)
alojamientos_madrid$lon <- as.numeric(alojamientos_madrid$lon)

leaflet(alojamientos_madrid) %>%
  setView(lng = -3.7037902, lat = 40.4167754, zoom = 12) %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat)
