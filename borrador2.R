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

table(datitos$trastorno_suegno) 

datitos2 <- datitos %>%
  mutate(est_nut = factor(est_nut, 
                          levels = c(0, 1, 3, 4, 5), 
                          labels = c("Normal", "Enflaquecido", "Sobrepeso", "Obeso", "Obeso mórbido"))) + 
  mutate(trastorno_suegno_1 = ifelse())

ggplot(datitos2, aes(x = est_nut, fill=trastorno_suegno)) +
  geom_bar(aes(fill = trastorno_suegno), position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) + 
  labs(title = "Estado Nutricional con Trastorno de Sueño",
       x = "Estado Nutricional",
       y = "Cantidad")
