### pa hacer weas 
### por si se nos olvida escogimos 
### sueño	y nutricion

# librerias y datos ----

library(rio)
library(dplyr)
library(ggplot2)
library(patchwork)

datos <- import("ENS-reducida.xlsx")

str(datos)
View(datos)

View(datitos)
datitos <- datos %>% 
  select(c(1:6),c(27:38),c(66:76))

head(datitos)

datitos = na.omit(datitos)

datitos = as.data.frame(datitos)


datos_nut <- datitos2 %>%
  filter(est_nut %in% c("Enflaquecido", "Normal", "Sobrepeso", "Obeso", "Obeso mórbido"))

# Crear el gráfico de torta
ggplot(datos_nut, aes(x = "", fill = est_nut)) +
  geom_bar(width = 1, position = "fill") + 
  coord_polar(theta = "y") + 
  labs(title = "Distribución de Estados Nutricionales")

#histograma horas de sueño

ggplot() +
  geom_histogram(data = datitos, aes(x = ts5, fill = "Horas de sueño en la semana"), alpha = 0.7, color = "black", bins = 10) +
  geom_histogram(data = datitos, aes(x = ts6, fill = "Horas de sueño el fin de semana"), alpha = 0.7, color = "black", bins = 10) +
  labs(title = "Horas de sueño en semana y fin de semana",
       x = "Valores",
       y = "Frecuencia") +
  scale_fill_manual(name = "Variables",
                    values = c("Horas de sueño en la semana" = "maroon", "Horas de sueño el fin de semana" = "#FF6A6A")) +
  theme_minimal()

#estado nutricional con sueño

datitos2 <- datitos %>%
  mutate(est_nut = factor(est_nut, 
                          levels = c(1, 0, 3, 4, 5), 
                          labels = c("Enflaquecido","Normal", "Sobrepeso", "Obeso", "Obeso mórbido")),
         trastorno_suegno = factor(trastorno_suegno, levels = c(0, 1)))

ggplot(datitos2, aes(x = est_nut, fill=trastorno_suegno)) +
  geom_bar(aes(fill = trastorno_suegno), position = "dodge") +
  scale_fill_manual(values = c("pink", "#FF69B4"), labels = c("Sin trastorno", "Con trastorno")) + 
  labs(title = "Estado Nutricional con Trastorno de Sueño",
       x = " ",
       y = " ",
       fill=" ")+
  theme_minimal()

#torta apnea, trastorno sueño 
ggplot(datitos2)



