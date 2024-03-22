### pa hacer weas 
### por si se nos olvida escogimos 
### sueño	y nutricion

# librerias ----

library(rio)
library(dplyr)
library(ggplot2)
library(patchwork)

datos <- import("ENS-reducida.xlsx")

str(datos)
View(datos)

datitos <- datos %>% 
  select(c(1:6),c(27:38),c(66,76))

head(datitos)
