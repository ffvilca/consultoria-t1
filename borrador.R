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

datitos2 <- datitos %>% 
  select(2,4,6,11,12,c(17:20),c(23:25),28,29)
datos_nut <- datitos2 %>%
  filter(est_nut %in% c("Enflaquecido", "Normal", "Sobrepeso", "Obeso", "Obeso mórbido"))

# Crear el gráfico de torta
ggplot(datos_nut, aes(x = " ", fill = est_nut)) +
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

### la wea del chi-cuadrado



library(rio)
library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(ggrepel)
library(kableExtra)

datos <- import("ENS-reducida.xlsx")

datitos <- datos %>% 
  mutate(Edad_Codificada_2 = ifelse(Edad_Codificada == 1,"15 a 24 años",
                                    ifelse(Edad_Codificada == 2, "25 a 44 años",
                                           ifelse(Edad_Codificada == 3, "45 a 64 años","65 años o más")))) %>% 
  mutate(Edad_Codificada_2 = factor(Edad_Codificada_2,
                                    labels = c("15 a 24 años",
                                               "25 a 44 años",
                                               "45 a 64 años",
                                               "65 años o más")) ) %>% 
  mutate(AREA_2 = factor(AREA,
                         labels = c("Norte",
                                    "Centro",
                                    "RM",
                                    "C-Sur",
                                    "Sur")) ) %>% 
  select(c(1:6),c(27:38),c(66:76),143,144)

datitos2 <-  datitos %>%
  mutate(Sexo_2 = ifelse(Sexo == 1,"Masculino","Femenino")) %>% 
  mutate(Nivel_Educ_2 = ifelse(Nivel_Educ == 1,"Bajo",
                               ifelse(Nivel_Educ == 2, "Medio","Alto"))) %>% 
  mutate(Nivel_Educ_2 = factor(Nivel_Educ_2,
                               labels = c("Bajo",
                                          "Medio",
                                          "Alto")) ) %>% 
  mutate(horas_suegno_sem = ifelse(ts5 <= 5, "Menos o 5 horas",
                                   ifelse(5 < ts5 & ts5 <= 10,
                                          "Entre 5 a 10 horas",
                                          "Más de 10 horas"))) %>% 
  mutate(horas_suegno_fds = ifelse(ts6 <= 5, "Menos o 5 horas",
                                   ifelse(5<ts6 & ts6 <= 10,"Entre 5 a 10 horas",
                                          "Más de 10 horas"))) %>% 
  mutate(est_nut = factor(est_nut, 
                          levels = c(1, 0, 3, 4, 5), 
                          labels = c("Enflaquecido","Normal", "Sobrepeso",
                                     "Obeso", "Obeso mórbido")),
         trastorno_suegno = factor(trastorno_suegno, levels = c(0, 1)),
         sospecha_apnea = factor(sospecha_apnea, levels = c(0, 1)),
         horas_suegno_fds = factor(horas_suegno_fds,
                                   levels = c("Menos o 5 horas",
                                              "Entre 5 a 10 horas",
                                              "Más de 10 horas")),
         horas_suegno_sem = factor(horas_suegno_sem,
                                   levels = c("Menos o 5 horas",
                                              "Entre 5 a 10 horas",
                                              "Más de 10 horas"))) %>%
  
  select(2,4,6,11,12,c(17:20),c(23:25),28,29,c(32:35)) %>% 
  na.omit()


View(datitos2)

cor.test(datitos2$imc, as.numeric(datitos2$trastorno_suegno))
cor.test(datitos2$imc, as.numeric(datitos2$trastorno_suegno), method="spearman")

cor.test(datitos2$Edad, as.numeric(datitos2$trastorno_suegno)) # ESTEEEE
cor.test(datitos$Edad_Codificada, datitos$trastorno_suegno)  # ESTEEEEE

a = table(datitos$Edad_Codificada, datitos$trastorno_suegno)

chisq.test(a)

horas / estado 
cor.test(datitos$ts6, datitos$est_nut)
cor.test(datitos$Peso, datitos2$horas_suegno_sem)

# Crear la tabla de contingencia
tabla_contingencia <- matrix(c(29, 401, 592, 0, 35, 42, 701, 871, 0, 67), nrow = 5, byrow = F)
rownames(tabla_contingencia) <- c("Enflaquecido", "Normal", "Sobrepeso", "Obeso", "Obeso mórbido")
colnames(tabla_contingencia) <- c("0", "1")

# Realizar el test de chi-cuadrado
resultado_chi_cuadrado <- chisq.test(tabla_contingencia)

# Mostrar los resultados
print(resultado_chi_cuadrado)


prop.table(table(datitos2$est_nut,datitos2$trastorno_suegno),margin = 1)

## Relaciones entre Estado Nutricional y Trastorno del sueño:
#| label: fig-f5
#| fig-cap: "Estado Nutricional de los individuos de la muestra"

ggplot(datitos2, aes(x = est_nut, fill=trastorno_suegno)) +
  geom_bar(aes(fill = trastorno_suegno), position = "dodge") +
  scale_fill_manual(values = c("pink", "#FF69B4"), labels = c("Sin trastorno", "Con trastorno")) + 
  labs(title = "Estado Nutricional con Trastorno de Sueño",
       x = " ",
       y = " ",
       fill=" ")+
  theme_minimal()

tablita <- table(datitos2$est_nut,datitos2$trastorno_suegno)


chisq.test(tablita)


# \onecolumn
# \begin{center}
# \vfill
# \LARGE Nutrición, Sueño y Salud: Un Análisis Estadístico \\[10pt]
# \large EYP3007 - Consultoría Estadística \\[10pt]
# Constanza Segovia González \\ Francisca Vilca Sánchez
# \vfill
# \end{center}
# \twocolumn
# 
# \newpage



