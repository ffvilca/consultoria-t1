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

#### resumen de datos generales

table(datitos2$Nivel_Educ)
table(datitos2$Sexo)
table(datitos$Edad_Codificada)
table(datitos$AREA)

# 1=15-24  2=25-44 3=45-64 4=65+


datitos %>% 
  mutate(Edad_Codificada_2 = ifelse(Edad_Codificada == 1,"15 a 24 años",
                                    ifelse(Edad_Codificada == 2, "25 a 44 años",
                                           ifelse(Edad_Codificada == 3, "45 a 64 años","65 años o más")))) %>% 
  mutate(Edad_Codificada_2 = factor(Edad_Codificada_2,
                                    labels = c("15 a 24 años",
                                               "25 a 44 años",
                                               "45 a 64 años",
                                               "65 años o más")) ) %>% 
  group_by(Edad_Codificada_2) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100,2)) %>% 
  kable(digits = 2, align = "c",booktabs = TRUE,
        col.names = c("Edad","Cantidad", "Porcentaje"),
        full.with =FALSE,
        format.args=list(decimal.mark=',', big.mark = '.')) %>%
  kable_minimal() %>%   
  row_spec(0, bold = T, color = "white", background = "#ff0054")


# 1=Bajo (<8 años) 2=Medio (8-12 años) 3=Alto (>12 años)

datitos %>% 
  mutate(Nivel_Educ_2 = ifelse(Nivel_Educ == 1,"Bajo",
                               ifelse(Nivel_Educ == 2, "Medio","Alto"))) %>% 
  mutate(Nivel_Educ_2 = factor(Nivel_Educ_2,
                               labels = c("Bajo",
                                          "Medio",
                                          "Alto")) ) %>% 
  group_by(Nivel_Educ_2) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100,2)) %>% 
  kable(digits = 2, align = "c",booktabs = TRUE,
        col.names = c("Nivel Educacional","Cantidad", "Porcentaje"),
        full.with =FALSE,
        format.args=list(decimal.mark=',', big.mark = '.')) %>%
  kable_minimal() %>%   
  row_spec(0, bold = T, color = "white", background = "#ff0054")

# Norte, Centro, C-Sur, Sur y RM


datitos %>% 
  mutate(AREA_2 = factor(AREA,
                         labels = c("Norte",
                                    "Centro",
                                    "RM",
                                    "C-Sur",
                                    "Sur")) ) %>% 
  group_by(AREA_2) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100,2)) %>% 
  kable(digits = 2, align = "c",booktabs = TRUE,
        col.names = c("Área","Cantidad", "Porcentaje"),
        full.with =FALSE,
        format.args=list(decimal.mark=',', big.mark = '.')) %>%
  kable_minimal() %>%   
  row_spec(0, bold = T, color = "white", background = "#ff0054")

# 1=Masculino  2=Femenino

datitos %>% 
  mutate(Sexo_2 = ifelse(Sexo == 1,"Masculino","Femenino")) %>% 
  group_by(Sexo_2) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100,2)) %>% 
  kable(digits = 2, align = "c",booktabs = TRUE,
        col.names = c("Sexo","Cantidad", "Porcentaje"),
        full.with =FALSE,
        format.args=list(decimal.mark=',', big.mark = '.')) %>%
  kable_minimal() %>%   
  row_spec(0, bold = T, color = "white", background = "#ff0054")
