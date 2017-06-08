##########################################################################
# Jose Cajide - @jrcajide
# Visualización con ggplot2 + googleVis
##########################################################################


# https://www.youtube.com/watch?v=jbkSRLYSojo

# instalar librerías ------------------------------------------------------

list.of.packages <- c("tidyverse", "gapminder", "googleVis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(gapminder)
library(googleVis)


ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) 

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) + geom_smooth()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_point(aes(color=continent)) + geom_smooth(lwd=1, se=FALSE, method="lm", col="black")

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent))  + geom_point() + scale_x_log10()  + geom_smooth(se=F, lwd=1)


gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3) + 
  ylab("Esperanza de vida") + 
  xlab("PIB per cápita") + 
  labs(title="Esperanza de vida y PIB per cápita", subtitle= "Fuente: Gapminder", caption = "Visualización: R + ggplot2") + 
  theme(legend.position="bottom")  + 
  theme_minimal() 


# googleVis ---------------------------------------------------------------

gg <- gvisMotionChart(gapminder, idvar='country', timevar='year', colorvar ='continent', xvar = 'gdpPercap', yvar = 'lifeExp', sizevar='pop')
plot(gg) # Necesita Flash
