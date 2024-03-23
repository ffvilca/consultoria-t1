library(rio)
library(dplyr)
library(ggplot2)
library(patchwork)

datos <- import("ENS-reducida.xlsx")

str(datos)
View(datos)

datitos <- datos %>% 
  select(c(1:6),c(27:38),c(66:76)) %>% 
  na.omit()

head(datitos)
str(datitos)
View(datitos)

datitos2 <- datitos %>% 
  select(2,4,6,11,12,c(17:20),c(23:25),28,29)

corrplot::corrplot(cor(datitos2), method = "ellipse")

datitos2 <- datitos2 %>% 
  mutate(horas_suegno_sem = ifelse(ts5 <= 5, "Menos o 5 horas",
                                   ifelse(5 < ts5 & ts5 <= 10, "Entre 5 a 10 horas",
                                          "Más de 10 horas")))


