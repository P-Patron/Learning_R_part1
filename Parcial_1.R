# Ejercicio 1 ----

set.seed(2025)
vector_aleatorio = rgamma(n=70, shape = 2, scale = 3)

# calculo del promedio para el vector aleatorio
mean_vector = mean(vector_aleatorio)

# calculo de la mediana para el vector aleatorio
median_vector = median(vector_aleatorio)

# calculo de la desviación estandar para el vector aleatoria
ds_vector = sd(vector_aleatorio)

print(mean_vector)
print(median_vector)
print(ds_vector)


valores_mediana = vector_aleatorio[vector_aleatorio > median_vector]
print(valores_mediana)

valores_rango4_10 = vector_aleatorio[vector_aleatorio >= 4 & vector_aleatorio <= 10]
print(valores_rang4_10)

rango_length = length(valores_rango4_10)
print(rango_length)

valores_impares <- vector_aleatorio[seq(1, length(vector_aleatorio), by = 2)]
print(valores_impares)

# Ejercicio 2 ----

find("lung")
find("Prestige")

# Lung esta en el paquete survival 
library(dplyr)

# En lung se encuentran datos relacionadas con pacientes que 
# sobrevivieron a cancer de pulmon.

# Prestige es en el paquete carData 
library(carData)

# En Prestige hay información sobre distintas carreras educativas
# en las cuales esta relacionado el año promedio de educación
# la cantidad de mujeres por carrera
# los ingresos promedios en dolores 

library(dplyr)
library(carData)
data("Prestige")

data_prestige <- Prestige %>%
  drop_na(prestige)
view(data_prestige)

result <- data_prestige %>%
  drop_na(type) %>%
  group_by(type) %>%
  summarise(
    mean_prestige = mean(prestige),
    median_prestige = median(prestige),
    sd_prestige = sd(prestige)
  )

print(result)

#Ejercicio 3 ----
  
content <- "reproducible research enhances reliability"

chart <- unlist(str_split(content,""))

print(chart)

la_h <- which(chart == "h") [1]
print(la_h)

count_r <- sum(chart[1:(la_h- 1)] == "r") 
print(count_r)

# Ejercicio 4 ----

set.seed(123)

result_dado = c(1,2,3,4,5,6) 
num.resultado_4 = 0
num.lanzamientos = 0
historial <- c()

while ( num.resultado_4 < 8) {
  res <- sample(x=result_dado, size = 1)
  num.lanzamientos = num.lanzamientos + 1
  historial[num.lanzamientos] = res
  if (res == 4){
    num.resultado_4 = num.resultado_4 + 1
  }
}

print(historial)

sum(historial == 4)

length_historial = length(historial)
print(length_historial)

# Ejercicio 5 ----

r_i_c <- function () {
  n = 90
  media_ric = 0 
  scale = 1
  
  data_ric <- rlogis(n, location = media_ric, scale = scale)
  
  cuartil_1 <- media_ric + scale * log(0.25 / (1 - 0.25))  # Cuantil 25%
  cuartil_3 <- media_ric + scale * log(0.75 / (1 - 0.75))  # Cuantil 75%
  
  rango_cuartil <- cuartil_3 - cuartil_1
  
  print(paste("El rango intercuartílico teórico es:", rango_cuartil))
  
}

r_i_c()

# Ejercicio 6 ----


data_semillas <- read.table("/Users/cpinilla/Paola/Master_Uninorte/Estadistica computacional/germinacion.txt", header = TRUE, sep = "")

head(data_semillas, 3)

germinacion <- data_semillas %>%
  mutate(orobanche = recode(orobanche, "a75" = "Hiedra", "a73" = "Trebol"))


filtro_observacion <- germinacion %>%
  filter(conteo != 10) %>%
  arrange(desc(conteo))
head(filtro_observacion, 5)

print(filtro_observacion)  


orobanche = c("a70", "a71", "a72", "Hiedra", "a74", "Trebol", "a76")
fungicida = c("Ecoticid-K1", "Ecoticid-K2", "Ecoticid-K3", "Ecoticid-K0",
              "Ecoticid-K4", "Ecoticid-K9", "Ecoticid-K8")

tratamiento = data.frame(orobanche, fungicida)

control <- germinacion %>%
  left_join(tratamiento, by = "orobanche")

conteo_semillas <- control %>%
  group_by(fungicida) %>%
  summarise(total_germinadas = sum(conteo))


