#Objetivos

#1. Verificar si existe una diferencia significativa en la proporción de estudiantes 
#que utilizan ChatGPT según su género.

#2. Constatar si la proporción de estudiantes que consideran que ChatGPT facilita la educación híbrida 
#es mayor que aquellos que creen que facilita la educación tradicional.

#3. Identificar las principales áreas de aplicación de ChatGPT, desde la asistencia académica 
#hasta la resolución de problemas complejos

#4. Predecir el nivel de motivación para estudiar de los estudiantes en función de su percepción 
#sobre si ChatGPT mejora su rendimiento académico.

#5. Evaluar si la proporción de estudiantes que consideran que ChatGPT mejora su empleabilidad 
#es significativa respecto al total de encuestados.

install.packages("DescTools")
library(DescTools)

install.packages("tidyverse")
library(tidyverse)

install.packages("UsingR")
library(UsingR)

install.packages("gridExtra")
library(gridExtra)

install.packages("glmnet")
library(glmnet)

library(ggplot2)

library(dplyr)

library(moments)

library(MASS)

#Carga de base de datos
bd_purosChat <- read.csv("C:\\Users\\howar\\Downloads\\P_Estad\\datos_puros.csv", 
                         header = TRUE, sep = ";", check.names = FALSE)
names(bd_purosChat)

#-----------ESTADÍSTICA DESCRIPTIVA------------------
bd_nuevo <- bd_purosChat %>% drop_na(Q3, Q15, Q35e)

nrow(bd_nuevo) #Cuenta el # de filas en el df
cant_estudiantes <- length(bd_nuevo$Q3)
cant_estudiantes

#---Edades General---
media_edad <- mean(bd_nuevo$Q3, na.rm = TRUE)
media_edad
# Varianza y desviación estándar
var_edad <- var(bd_nuevo$Q3, na.rm = TRUE)
var_edad
sd_edad <- sd(bd_nuevo$Q3, na.rm = TRUE)
sd_edad
# Rango
rango_edad <- range(bd_nuevo$Q3, na.rm = TRUE)
rango_edad
# Kurtosis
kurtosis_edad <- kurtosis(bd_nuevo$Q3, na.rm = TRUE)
kurtosis_edad
# Sesgo
sesgo_edad <- skewness(bd_nuevo$Q3, na.rm = TRUE)
sesgo_edad

#----Frecuencia de uso de ChatGPT---
# Tabla de frecuencias
frecuencia_uso <- table(bd_nuevo$Q15)
frecuencia_uso

bd_Dos <- bd_purosChat %>%
  drop_na(Q2, Q13) %>%  
  mutate(
    Q2 = recode(Q2, "1" = "Masculino", "2" = "Femenino", "3" = "Otro", "4" = "Prefiero no decirlo"), 
    Q13 = recode(Q13, "1" = "Sí", "2" = "No")
  )

# Tabla de frecuencias
motivacion <- table(bd_nuevo$Q35e)
motivacion

#---Histograma---
#Histograma EDAD
ggplot(bd_nuevo, aes(x = Q3)) +
  geom_histogram(binwidth = 5, fill = "cyan", color = "black", alpha = 0.7) +
  labs(
    title = "Distribución de Edades de los Estudiantes",
    subtitle = "Histograma de 1389 edades de estudiantes ecuatorianos",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT",
    x = "Edad",
    y = "Frecuencia"
  ) +
  theme_light()

#---Diagrama de Barras---
#Diagrama de Barras Frecuencia
grafico_Uso <- ggplot(bd_nuevo, aes(x = Q15)) +
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  labs(
    title = "Frecuencia de Uso de ChatGPT",
    subtitle = "Distribución de la frecuencia de uso entre estudiantes",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT",
    x = "Frecuencia de Uso",
    y = "Cantidad de Estudiantes"
  ) +
  theme_light()

t_significados_Uso <- data.frame(
  Valor = c(1, 2, 3, 4, 5),
  Significado = c("Rara vez", "Ocasionalmente", "Moderadamente", "Considerablemente", "Extensivamente")
)

t_grob_Uso <- tableGrob(t_significados_Uso, rows = NULL, theme = ttheme_minimal(base_size = 10))

grid.arrange(grafico_Uso, t_grob_Uso, ncol = 1, heights = c(3, 1))

#Diagrama de barras Motivacion
grafico_Mot <- ggplot(bd_nuevo, aes(x = Q35e)) +
  geom_bar(fill = "green", color = "black", alpha = 0.7) +
  labs(
    title = "Nivel de Motivación para Estudiar",
    subtitle = "Distribución del nivel de motivación entre estudiantes",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT",
    x = "Nivel de Motivación",
    y = "Cantidad de Estudiantes"
  ) +
  theme_light()

t_significados <- data.frame(
  Valor = c(1, 2, 3, 4, 5),
  Significado = c("Muy en desacuerdo", "En desacuerdo", "Neutral", "De acuerdo", "Muy de acuerdo")
)

t_grob_Mot <- tableGrob(t_significados, rows = NULL, theme = ttheme_minimal(base_size = 10))

grid.arrange(grafico_Mot, t_grob_Mot, ncol = 1, heights = c(3, 1))

ggplot(bd_Dos, aes(x = Q2, fill = Q13)) +
  geom_bar(alpha = 0.8, position = "stack") +
  theme_light() +
  labs(
    title = "Distribución de Uso de ChatGPT por Género",
    subtitle = "Comportamiento de los estudiantes respecto al uso de ChatGPT",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT",
    x = "Género",
    y = "Cantidad de Estudiantes",
    fill = "¿Usa ChatGPT?"
  ) +
  scale_fill_manual(values = c("Sí" = "blue", "No" = "darkred")) +  # Colores personalizados
  theme_light()



#---------------Filtrado de datos--------------------------
bd_online_trad <- bd_purosChat %>%
  drop_na(Q19h, Q19j) %>%  
  mutate(
    Q19h = recode(Q19h, "1" = "Muy en desacuerdo", "2" = "En desacuerdo", "3" = "Neutral", "4" = "De acuerdo", "5" = "Muy de acuerdo"),
    Q19j = recode(Q19j, "1" = "Muy en desacuerdo", "2" = "En desacuerdo", "3" = "Neutral", "4" = "De acuerdo", "5" = "Muy de acuerdo")
  )


bd_comparacion <- bd_online_trad %>%
  select(Q19h, Q19j) %>%
  pivot_longer(cols = everything(), names_to = "Pregunta", values_to = "Respuesta") %>%
  mutate(Pregunta = recode(Pregunta, "Q19h" = "Educación Tradicional", "Q19j" = "Educación Híbrida"))

ggplot(bd_comparacion, aes(x = Respuesta, fill = Pregunta)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  theme_light() +
  labs(
    title = "Percepción de los Estudiantes sobre ChatGPT en la Educación",
    subtitle = "Comparación entre educación tradicional y educación híbrida",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT",
    x = "Nivel de Acuerdo",
    y = "Cantidad de Estudiantes",
    fill = "Facilita"
  ) +
  scale_fill_manual(values = c("Educación Tradicional" = "blue", "Educación Híbrida" = "orange")) +
  theme_light()

#Prueba normalidad edad
qqnorm(bd_purosChat$Q3, ylab = "Edad registrada por 1398 estudiantes " , main = "Gráfico Q-Q Normal de Edad", 
       col = "lightblue")
qqline(bd_purosChat$Q3, col = "darkgreen")

#Prueba de Kolmogorov - Smirnov
#H0: la variable edad es normal
#Ha: No es cierto H0

media_edad
sd_edad
ks.test(x = bd_purosChat$Q3, y = "pnorm", media_edad, sd_edad)
#P es 2.2e-16 no es normal

#----------Objetivo 1----------------
#H0: No existe relación entre el género y el uso de ChatGPT P0=P1
#H1: las proporciones de uso de ChatGPT son diferentes entre hombres y mujeres P0!=P1
#Q2: Genero
#Q13: Uso de ChatGPT (SÍ/NO)
bd_OB1 <- bd_purosChat %>%
  select(Q2, Q13) %>%  
  drop_na(Q2, Q13) %>%
  mutate(
    Q2 = recode(Q2, "1" = "Hombre", "2" = "Mujer"),  
    Q13 = recode(Q13, "1" = "Sí", "2" = "No")  
  )

bd_OB1

tabla_contingencia_OB1 <- table(bd_OB1)
tabla_contingencia_OB1
prubea_OB1 <- chisq.test(tabla_contingencia_OB1)
prubea_OB1
prop.test(tabla_contingencia_OB1)
#P 0.00134 rechaza H0


#----------Objetivo 2----------------
#Q19h: Percepción de si ChatGPT facilita la educación tradicional.
#Q19j: Percepción de si ChatGPT facilita la educación híbrida.

#H0: las proporciones de estudiantes que están de acuerdo o en desacuerdo con que ChatGPT facilita 
#la educación tradicional y la educación híbrida son independientes.

#H1: Existe una asociación entre la percepción de que ChatGPT facilita la educación tradicional 
#y la percepción de que facilita la educación híbrida.

bd_percep_filt <- bd_purosChat %>%
  select(Q19h, Q19j) %>%
  drop_na(Q19h, Q19j)

bd_percep_filt <- bd_percep_filt %>%
  mutate(
    Q19h = case_when(
      Q19h == 1 ~ "Muy en desacuerdo",
      Q19h == 2 ~ "En desacuerdo",
      Q19h == 3 ~ "Neutral",
      Q19h == 4 ~ "De acuerdo",
      Q19h == 5 ~ "Muy de acuerdo"
    ),
    Q19j = case_when(
      Q19j == 1 ~ "Muy en desacuerdo",
      Q19j == 2 ~ "En desacuerdo",
      Q19j == 3 ~ "Neutral",
      Q19j == 4 ~ "De acuerdo",
      Q19j == 5 ~ "Muy de acuerdo"
    )
  )

bd_percep_filt <- bd_percep_filt %>%
  mutate(
    Q19h = case_when(
      Q19h %in% c("Muy en desacuerdo", "En desacuerdo") ~ "En desacuerdo",
      Q19h %in% c("De acuerdo", "Muy de acuerdo") ~ "De acuerdo",
      TRUE ~ NA_character_  
    ),
    Q19j = case_when(
      Q19j %in% c("Muy en desacuerdo", "En desacuerdo") ~ "En desacuerdo",
      Q19j %in% c("De acuerdo", "Muy de acuerdo") ~ "De acuerdo",
      TRUE ~ NA_character_  
    )
  ) %>%
  drop_na(Q19h, Q19j)  

bd_percep_filt <- bd_percep_filt %>%
  rename(
    "Facilita Educación Tradicional" = Q19h,
    "Facilita Educación Online" = Q19j
  )
bd_percep_filt
head(bd_percep_filt)

#Tabla de contingencia 
tabla_contingencia <- table(bd_percep_filt)
tabla_contingencia
prueba_chi <- chisq.test(tabla_contingencia)
prueba_chi
#p-value = 2.2e-16 por lo tanto se rechaza H0

#-----------Objetivo 3---------------
bd_aplicaciones <- bd_purosChat %>%
  select(Q18a:Q18l) %>%
  rename(
    "Escritura académica" = Q18a,
    "Escritura profesional" = Q18b,
    "Escritura creativa" = Q18c,
    "Corrección de textos" = Q18d,
    "Lluvia de ideas" = Q18e,
    "Traducción" = Q18f,
    "Resumen de textos" = Q18g,
    "Ayuda con cálculos" = Q18h,
    "Asistencia en el estudio" = Q18i,
    "Asistencia personal" = Q18j,
    "Asistencia en investigación" = Q18k,
    "Asistencia en programación" = Q18l
  ) %>%
  drop_na()

bd_aplicaciones <- bd_aplicaciones %>%
  mutate(across(everything(), ~ recode(.x,
                                       `1` = "Nunca",
                                       `2` = "Rara vez",
                                       `3` = "A veces",
                                       `4` = "Frecuentemente",
                                       `5` = "Siempre"
  )))
bd_aplicaciones

frecuencias_aplicaciones <- bd_aplicaciones %>%
  pivot_longer(cols = everything(), names_to = "Area", values_to = "Frecuencia") %>%
  group_by(Area, Frecuencia) %>%
  summarise(Total = n(), .groups = 'drop')

# Ordenar los niveles de "Frecuencia" para que aparezcan en el orden correcto en el gráfico
frecuencias_aplicaciones$Frecuencia <- factor(
  frecuencias_aplicaciones$Frecuencia,
  levels = c("Nunca", "Rara vez", "A veces", "Frecuentemente", "Siempre")
)

# Crear el gráfico de barras apiladas
ggplot(frecuencias_aplicaciones, aes(x = reorder(Area, -Total), y = Total, fill = Frecuencia)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  labs(
    title = "Frecuencia de Uso de ChatGPT en Diferentes Áreas de Aplicación",
    subtitle = "Distribución de respuestas según la frecuencia de uso",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT",
    x = "Área de Aplicación",
    y = "Frecuencia de Uso",
    fill = "Frecuencia"
  ) +
  scale_fill_manual(values = c("Nunca" = "red", "Rara vez" = "orange", "A veces" = "yellow", "Frecuentemente" = "lightgreen", "Siempre" = "darkgreen")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 12, hjust = 0.5),  
    legend.position = "bottom"  
  )

#-----------Objetivo 4---------------
bd_motivacion <- bd_purosChat %>%
  select(Q27b, Q35e) %>%
  drop_na()

head(bd_motivacion)

#Correlación Spaerman
correlacion <- cor(bd_motivacion$Q27b, bd_motivacion$Q35e, method = "spearman")
correlacion

modelo_ordinal <- polr(as.factor(Q35e) ~ Q27b, data = bd_motivacion, Hess = TRUE)
summary(modelo_ordinal)

# Calcular el valor p para la variable Q27b
coeficientes <- coef(summary(modelo_ordinal))
p_valor <- pnorm(abs(coeficientes["Q27b", "t value"]), lower.tail = FALSE) * 2
p_valor

#-----------Objetivo 5----------------
#H0: La proporción de estudiantes que consideran que ChatGPT mejora su empleabilidad es 
#igual a una proporción de referencia
#H1: La proporción de estudiantes que consideran que ChatGPT mejora su empleabilidad es 
#diferente de la proporción de referencia.
bd_empleabilidad <- bd_purosChat %>%
  select(Q27j) %>%  # Seleccionar la columna de percepción de mejora de empleabilidad
  drop_na(Q27j) %>%  # Eliminar filas con valores faltantes
  mutate(
    Q27j = recode(Q27j, "1" = "Muy en desacuerdo", "2" = "En desacuerdo", "3" = "Neutral", "4" = "De acuerdo", "5" = "Muy de acuerdo")  # Traducir valores
  )

n_estudiantes_positivos <- sum(bd_empleabilidad$Q27j %in% c("De acuerdo", "Muy de acuerdo"))

n_total <- nrow(bd_empleabilidad)

proporcion_positivos <- n_estudiantes_positivos / n_total
proporcion_positivos

prueba_proporcion <- prop.test(n_estudiantes_positivos, n_total, p = 0.5, alternative = "two.sided")
prueba_proporcion

names(bd_purosChat)


#----------------Diagrama de cajas--------------------
bd_cajas <- bd_purosChat %>%
  select(Q3,Q13) %>%
  filter(!is.na(Q13)) %>% 
  filter(!is.na(Q3)) %>% 
  mutate(Usa_ChatGPT = recode(Q13, "1" = "Sí", "2" = "No")) 
bd_cajas

ggplot(bd_cajas, aes(x = Usa_ChatGPT, y = Q3)) +
  geom_boxplot(fill = c("skyblue", "orange"), color = "black") +
  labs(
    title = "Diagrama de Cajas: Edad según Uso de ChatGPT",
    x = "Uso de ChatGPT",
    y = "Edad",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT"
  ) +
  theme_light()


#EMPLEABILIDAD
bd_cajas_emp <- bd_purosChat %>%
  select(Q3, Q27j) %>%  
  filter(!is.na(Q3)) %>% 
  filter(!is.na(Q27j)) %>% 
  mutate(Empleabilidad = factor(Q27j, levels = 1:5, 
                                labels = c("Muy en desacuerdo", "En desacuerdo", 
                                           "Neutral", "De acuerdo", "Muy de acuerdo")))

ggplot(bd_cajas_emp, aes(x = Empleabilidad, y = Q3)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Diagrama de Cajas: Edad según Percepción de Mejora en Empleabilidad",
    x = "Percepción de Mejora en Empleabilidad",
    y = "Edad",
    caption = "Fuente: Percepción de los estudiantes de ChatGPT"
  ) +
  theme_light()

edad_breaks <- seq(min(bd_nuevo$Q3, na.rm = TRUE), 
                   max(bd_nuevo$Q3, na.rm = TRUE) + 5, 
                   by = 5)

# Crear tabla de frecuencia para edad
tabla_edad <- bd_nuevo %>%
  mutate(Intervalo = cut(Q3, breaks = edad_breaks, right = FALSE)) %>%
  group_by(Intervalo) %>%
  summarise(
    Frecuencia_Absoluta = n(),
    Frecuencia_Relativa = n() / nrow(bd_nuevo)
  ) %>%
  mutate(
    Marca_Clase = (as.numeric(Intervalo) - 0.5) * 5,
    Frecuencia_Acumulada = cumsum(Frecuencia_Absoluta),
    Frecuencia_Relativa_Acumulada = cumsum(Frecuencia_Relativa)
  ) %>%
  select(Intervalo, Marca_Clase, Frecuencia_Absoluta, Frecuencia_Acumulada, 
         Frecuencia_Relativa, Frecuencia_Relativa_Acumulada)
tabla_edad
