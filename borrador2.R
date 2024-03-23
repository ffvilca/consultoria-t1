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
                                          "Más de 10 horas"))) %>% 
  mutate(horas_suegno_fds = ifelse(ts6 <= 5, "Menos o 5 horas",
                                   ifelse(5 < ts6 & ts6 <= 10, "Entre 5 a 10 horas",
                                          "Más de 10 horas")))

tab1 <- datitos2 %>%
  group_by(est_nut,horas_suegno_sem) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100,2)) 

graf1 <- tab1 %>% 
  ggplot(aes(x = est_nut, y = Cantidad, fill = horas_suegno_sem,
             text = str_glue("Categoria: {est_nut}\n{horas_suegno_sem}\nCantidad: {format(Cantidad, big.mark = '.', decimal.mark = ',')}\nPorcentaje: {format(Porcentaje, big.mark = '.', decimal.mark = ',')}%"))) +
  geom_col(aes( fill = horas_suegno_sem) , position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Estado nutricional",
       y = "",
       fill = "Horas de sueño semanal",
       title = "") +
  scale_fill_manual(values = c("#F72C25","#124FB3","#822E81"),
                    labels = c("Menos o 5 horas",
                               "Entre 5 a 10 horas",
                               "Más de 10 horas"))
graf1

plotly::ggplotly(graf1, tooltip = "text")

tab2 <- datitos2 %>%
  group_by(est_nut,horas_suegno_fds) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100,2)) 


graf2 <- tab2 %>% 
  ggplot(aes(x = est_nut, y = Cantidad, fill = horas_suegno_fds,
             text = str_glue("Categoria: {est_nut}\n{horas_suegno_fds}\nCantidad: {format(Cantidad, big.mark = '.', decimal.mark = ',')}\nPorcentaje: {format(Porcentaje, big.mark = '.', decimal.mark = ',')}%"))) +
  geom_col(aes( fill = horas_suegno_fds) , position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Estado nutricional",
       y = "",
       fill = "Horas de sueño fin de semana",
       title = "") +
  scale_fill_manual(values = c("#F72C25","#124FB3","#822E81"),
                    labels = c("Menos o 5 horas",
                               "Entre 5 a 10 horas",
                               "Más de 10 horas"))
graf2

plotly::ggplotly(graf2, tooltip = "text")
