##########################################################################
# Jose Cajide - @jrcajide - https://www.linkedin.com/in/jrcajide/
# Manejo de datos
##########################################################################

# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


# instalar librerías ------------------------------------------------------
# 
list.of.packages <- c("tidyverse", "maps", "geosphere", "circlize", "ggrepel", "viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Importar datos
flights <- read_csv('data/flights.csv')

# seleccionar variables (columnas)
flights %>% select(dep_time, arr_time)

# Nota: `starts_with`, `ends_with`, y `matches` (para RegEx) buscan columnas por nombre
flights %>% select(year:day, contains("delay"), matches("time$"))

# filtrar observaciones (filas)
flights %>%  filter(dep_delay > 60)

# Todo junto
flights %>%
  select(carrier, dep_delay) %>%
  filter(dep_delay > 60)

# Ordenar el resultado
flights %>%
  select(carrier, dep_delay) %>%
  filter(dep_delay > 60) %>% 
  arrange(desc(dep_delay))

# Crear una nueva varible (columna)
flights %>% select(distance, air_time) %>%
  mutate(speed = distance / air_time*60)

# Agrupar
flights %>%
  group_by(dest) %>%
  summarise(averge_delay = mean(arr_delay, na.rm=TRUE))

# media de retraso
flights %>%
  group_by(carrier) %>%
  summarise_each(funs(mean(., na.rm = TRUE)), dep_delay, arr_delay) %>% 
  arrange((dep_delay))

# retrasos máximos y mínimos de salida y llegada por cada compañia
flights %>%
  group_by(carrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("delay"))

# Número de vuelos por cada día del mes y aerolínea ordenados descendentemente
flights %>%
  group_by(day, carrier) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>% 
  ggplot(aes(x = factor(day), y = total, fill=carrier)) + 
  geom_bar(stat = "identity") + 
  theme_light()

# número total de vuelos y número de aviones distintos que han volado a cada destino
flights %>%
  group_by(dest) %>%
  summarise(total = n(), aviones = n_distinct(tailnum))

# Aeropuertos con mayores retrasos en la salida
flights %>% 
  select(origin, dep_delay) %>% 
  group_by(origin) %>% 
  summarise(retraso = mean(dep_delay, na.rm=T)) %>% 
  arrange(desc(retraso))

# Calcular, para cada aerolínea, que dos días del mes han tenido mayores retrasos en la salida
flights %>%
  group_by(carrier) %>%
  select(day, dep_delay) %>%
  top_n(2)  %>%
  arrange(carrier, desc(dep_delay))

# Número de vuelos por mes y variación respecto al dia anterior 
flights %>%
  group_by(day) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count)) %>% 
  ggplot(aes(x=day, y=change)) + geom_line(color="red") + theme_light()



# múltiples fuentes -------------------------------------------------------

airlines <- read_csv('data/airlines.csv')
airports <- read_csv('data/airports.csv')
planes <- read_csv('data/planes.csv')

# Relaciones: https://github.com/joseramoncajide/curso_introduccion_R/blob/master/img/relational-nycflights.png 
# flights => planes por tailnum
# flights => airlines por carrier
# flights => airports por origin y dest
# flights => weather por origin, year, month, day y hour


flights %>%
  select(carrier, flight, tailnum) %>% 
  left_join(airlines, by = "carrier")

# Por aerolínea, número de fabricantes distintos de aviones 

flights %>% 
  select(carrier, flight, tailnum) %>% 
  left_join(planes, by = "tailnum", "seats") %>% 
  group_by(carrier) %>% 
  summarise(fabricantes = n_distinct(manufacturer)) %>% 
  ggplot(aes(x=carrier, y=fabricantes)) + 
  geom_bar(stat = 'identity') + 
  theme_light()

# ¿Y los nombres del fabricante?
flights %>% 
  select(carrier, flight, tailnum) %>% 
  left_join(planes, by = "tailnum", "seats") %>% 
  left_join(airlines, by = "carrier") %>% 
  group_by(name) %>% 
  summarise(fabricantes = n_distinct(manufacturer)) %>% 
  arrange(desc(fabricantes)) %>% 
  ggplot(aes(x=name, y=fabricantes)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  theme_light()

# Rerordenación de los ejes por un valor
# reorder(name, fabricantes)

flights %>% 
  select(carrier, flight, tailnum) %>% 
  left_join(planes, by = "tailnum", "seats") %>% 
  left_join(airlines, by = "carrier") %>% 
  group_by(name) %>% 
  summarise(fabricantes = n_distinct(manufacturer)) %>% 
  arrange(desc(fabricantes)) %>% 
  ggplot(aes(reorder(x = name, fabricantes), y=fabricantes)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  theme_minimal() +
  theme_light()


# Compañías con los aviones más antiguos
flights %>% 
  select(carrier, flight, tailnum) %>% 
  left_join(planes, by = "tailnum", "seats") %>% 
  left_join(airlines, by = "carrier") %>% 
  select(name, year) %>% 
  mutate(antiguedad = 2017 - year) %>% 
  group_by(name) %>% 
  summarise(antiguedad_media = median(antiguedad, na.rm=T)) %>% 
  arrange(desc(antiguedad_media)) %>% 
  ggplot(aes(reorder(x = name, antiguedad_media), y=antiguedad_media)) + geom_point(stat='identity', size=3) + 
  coord_flip() +
  theme_minimal()


# por partes --------------------------------------------------------------

location <- airports %>% 
  select(dest = faa, name, lat, lon)
location

delays <- flights %>%
  group_by(dest) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  arrange(desc(arr_delay)) %>%
  inner_join(location)

library('ggrepel')
library('viridis')

ggplot(delays, aes(lon, lat)) + 
  borders("state",fill = "grey90", colour = "white") + 
  geom_point(aes(colour = arr_delay), size = 3, alpha = 0.9) + 
  scale_colour_gradientn(colours = rainbow(10)) +
  # scale_color_viridis(discrete=F) +
  # scale_fill_continuous() +
  geom_label_repel(data = subset(delays, arr_delay > 19), aes(lon, lat, label = name), size = 5,
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.3, "lines")) +
  coord_quickmap() + theme_minimal()


# Extra: Visualización: conexiones
# 
library(geosphere)
library(maps)

usairports <- filter(airports, lat < 48.5)
usairports <- filter(usairports, lon > -130)
usairports <- filter(usairports, faa!="JFK") #todos menos JFK
jfk <- filter(airports, faa=="JFK") #sólo JFK

maps::map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0), main = "Conexiones con JFK")
points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")




for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}

title(main = list("Conexiones JFK", cex = 1.5,col = "chocolate1", font = 2))



# Extra: Visualización: rutas
library(circlize)
dev.off()
circos.par(start.degree = 90)

flights %>% 
  group_by(origin, dest) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  top_n(50) %>% 
  chordDiagram(directional = 1, direction.type = c("diffHeight"), link.arr.length = 0.2)

circos.clear()
