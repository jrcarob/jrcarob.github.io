# https://dominicroye.github.io/es/2019/visualizar-el-crecimiento-urbano/

# instalamos los paquetes necesarios
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("feedeR")) install.packages("feedeR")
if(!require("fs")) install.packages("fs")
if(!require("lubridate")) install.packages("lubridate")
if(!require("fs")) install.packages("fs")
if(!require("tmap")) install.packages("tmap")
if(!require("classInt")) install.packages("classInt")
if(!require("showtext")) install.packages("showtext")
if(!require("sysfonts")) install.packages("sysfonts")
if(!require("rvest")) install.packages("rvest")

# cargamos los paquetes
library(feedeR)
library(sf) 
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(tmap)
library(rvest)

url <- "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.bu.atom.xml"

# importamos los RSS con enlaces de provincias
prov_enlaces <- feed.extract(url)
str(prov_enlaces) #estructura es lista

# extraemos la tabla con los enlaces
prov_enlaces_tab <- as_tibble(prov_enlaces$items) %>% 
  mutate(title = repair_encoding(title))


# filtramos la provincia y obtenemos la url RSS
cor_atom <- filter(prov_enlaces_tab, str_detect(title, "Córdoba")) %>% pull(link)

# importamos la RSS
cor_enlaces <- feed.extract(cor_atom)

# obtenemos la tabla con los enlaces de descarga
cor_enlaces_tab <- cor_enlaces$items
cor_enlaces_tab <- mutate(cor_enlaces_tab, title = repair_encoding(title),
                          link = repair_encoding(link)) 

# filtramos la tabla con el nombre de la ciudad
cor_link <- filter(cor_enlaces_tab, str_detect(title, "CORDOBA")) %>% pull(link)
cor_link

# creamos un archivo temporal 
temp <- tempfile()

# descargamos los datos
download.file(URLencode(cor_link), temp)

# descomprimimos a una carpeta llamda buildings
unzip(temp, exdir = "buildings")

# obtenemos la ruta con el archivo
file_cor <- dir_ls("buildings", regexp = "building.gml")

# importamos los datos
buildings_cor <- st_read(file_cor)

# 
buildings_cor <- mutate(buildings_cor, 
                        beginning = str_replace(beginning, "^-", "0000") %>% 
                          ymd_hms() %>% as_date())

#descarga de familia tipográfica
sysfonts::font_add_google("Montserrat", "Montserrat")

#usar showtext para familias tipográficas
showtext::showtext_auto() 
#limitamos al periodo posterior a 1750
filter(buildings_cor, beginning >= "1750-01-01") %>%
  ggplot(aes(beginning)) + 
  geom_density(fill = "#2166ac", alpha = 0.7) +
  scale_x_date(date_breaks = "20 year", 
               date_labels = "%Y") +
  theme_minimal() +
  theme(title = element_text(family = "Montserrat"),
        axis.text = element_text(family = "Montserrat")) +
  labs(y = "",x = "", title = "Evolución del desarrollo urbano en Córdoba")

# obtenemos las coordinadas de Córdoba
ciudad_point <- tmaptools::geocode_OSM("Cordoba", 
                                       as.sf = TRUE)

# proyectamos los datos
ciudad_point <- st_transform(ciudad_point, 25830)

# creamos un buffer de 2 kms
point_bf <- st_buffer(ciudad_point, 2000)

# obtenemos la intersección entre el buffer y la edificación
buildings_cor20 <- st_intersection(buildings_cor, point_bf)

#encontrar 15 clases
br <- classIntervals(year(buildings_cor20$beginning), 15, "quantile")

#crear etiquetas
lab <- names(print(br, under = "<", over = ">", cutlabels = FALSE))

#categorizar el año
buildings_cor20 <- mutate(buildings_cor20, 
                          yr_cl = cut(year(beginning), br$brks, labels = lab, include.lowest = TRUE))

#colores
col_spec <- RColorBrewer::brewer.pal(11, "Spectral")

#función de una gama de colores
col_spec_fun <- colorRampPalette(col_spec)


#crear los mapas
tm_shape(buildings_cor20) +
  tm_polygons("yr_cl", 
              border.col = "transparent",
              palette = col_spec_fun(15),
              textNA = "Sin datos",
              title = "CÓRDOBA") +
  tm_layout(bg.color = "black",
            outer.bg.color = "black",
            legend.outside = TRUE,
            legend.text.color = "white",
            legend.text.fontfamily = "Montserrat", 
            panel.label.fontfamily = "Montserrat",
            panel.label.color = "white",
            panel.label.bg.color = "black",
            panel.label.size = 5,
            panel.label.fontface = "bold",
            title = "CÓRDOBA")

#mapa tmap de Córdoba
m <-   tm_shape(buildings_cor20) +
  tm_polygons("yr_cl", 
              border.col = "transparent",
              palette = col_spec_fun(15),
              textNA = "Without data",
              title = "CÓRDOBA")


#mapa dinámico
tmap_leaflet(m)
