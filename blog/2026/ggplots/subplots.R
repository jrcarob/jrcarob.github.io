# https://analisisydecision.es/incluir-subplot-en-mapa-con-ggplot/

library(mapSpain)
library(sf)
library(tidyverse)
library(lubridate)
library(ggplotify)

df <- read.csv("https://momo.isciii.es/public/momo/data", encoding = "UTF-8")

df2 <- df %>% dplyr::filter(ambito =='ccaa' & nombre_sexo=='todos' & cod_gedad=='all') %>% 
  mutate(fecha_defuncion=as.Date(fecha_defuncion, '%Y-%m-%d')) %>%
  filter(year(fecha_defuncion)*100 + month(fecha_defuncion)>=202012) %>% 
  filter(fecha_defuncion <= today() - 5) %>% 
  mutate(exceso = round(defunciones_observadas/defunciones_esperadas-1,4)*100,
         iso2.ccaa.code = paste0("ES-",cod_ambito,sep=""))

#Mapa estático
CCAA.sf <- esp_get_ccaa()

#Elijo los centroides de las CCAA para pintar el gráfico
centroides <- st_coordinates(st_centroid(CCAA.sf$geometry))
centroides <- data.frame(centroides)
iso2.ccaa.code <- CCAA.sf$iso2.ccaa.code
centroides <- cbind.data.frame(iso2.ccaa.code, centroides)

df2 <- left_join(df2, centroides)

# Este es un ejemplo del gráfico que quiero realizar
dt <- filter(df2,iso2.ccaa.code==paste0("ES-AN"))

AN <- ggplot(data=dt, aes(x=fecha_defuncion, y=exceso, group=1)) + 
  geom_line(color="red") + ylim(-10, 100) + theme_classic()+ 
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) + geom_smooth()
AN

AN <- as.grob(AN)

subgrafico <- annotation_custom(AN, xmin = unique(dt$X) - 1.2 , xmax = unique(dt$X) + 
                                  1.2, ymin = unique(dt$Y) - 1.2, ymax = unique(dt$Y) + 1.2)

ggplot() + geom_sf(data=CCAA.sf, color="white")  +
  geom_sf(data = esp_get_can_box(), colour = "grey50") + subgrafico +
  theme_light()

# Pinto el mapa en modo bucle
mapa <- ggplot() + geom_sf(data=CCAA.sf, color="white")  +
  geom_sf(data = esp_get_can_box(), colour = "grey50") + theme_classic()

for (i in iso2.ccaa.code) {
  dt <- filter(df2,iso2.ccaa.code == i )
  
  p <- ggplot(data=dt, aes(x=fecha_defuncion, y=exceso, group=1)) + 
    geom_line(color="red") + ylim(-10, 100) + theme_classic()+ 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background  = element_rect(fill = "transparent")) + geom_smooth()
  
  
  p <- as.grob(p)
  
  subgrafico <- annotation_custom(p, xmin = unique(dt$X) - 0.8 , xmax = unique(dt$X) + 
                                    0.8, ymin = unique(dt$Y) - 0.5, ymax = unique(dt$Y) + 0.5)
  
  mapa <- mapa + subgrafico + subgrafico}

mapa <- mapa +theme_minimal()

mapa
