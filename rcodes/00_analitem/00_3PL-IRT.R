############################################# 
### Análisis de ítems de la muestraE 2025 ###
###                                       ###
### Creación de una escala de             ###
### equipamiento para la estratificación  ###
### del marco de muestreo                 ###
###                                       ###
### ECLAC's Official Mission              ### 
### Author: Andrés Gutiérrez              ###
### Date: 2025                            ###
############################################# 

rm(list = ls())
options(psipen = -100, digits = 3)

library(WrightMap)
library(eRm)
library(ltm)
library(ggplot2)
library(foreign)
library(dplyr)
library(gridExtra)
library(CTT)
library(magrittr)
library(haven)
library(purrr)
select <- dplyr::select


set_data <- read_sav("Data/MiestraE_analitem/BD_viviendas_muestraE.sav")

## Estructura de los datos 
str(set_data)
labelled::generate_dictionary(set_data)

summary(set_data)

map(set_data, table)


################################################################################

dat_1 <- set_data %>% select(bici:fonofijo)

################################################################################
### Teoría clásica ###
################################################################################

names(dat_1)

#Reliability
cronbach.alpha(dat_1)  

# Items: 18
# Sample units: 32768
# alpha: 0.801

# proporciones de bienestar
# proporciones de aciertos
prop <- colMeans(dat_1)
sort(prop, decreasing = T)
# varianza
var <- prop * (1 - prop)
sort(var, decreasing = T)

# proporciones estandarizadas
bienestar <- rowMeans(dat_1)
head(bienestar)
mean(bienestar)
sd(bienestar)

bienestar.est <- scale(bienestar)
hist(bienestar.est)

dificultad.est <- scale(prop)
hist(dificultad.est)

score <- 5 + 2 * bienestar.est
mean(score); sd(score)
min(score); max(score)

hist(score)
boxplot(score)


################################################################################
### 3PL de Birnbaum ###
################################################################################

#####################
# Análisis de ítems #
#####################

res_3pl_1 <- tpm(dat_1)
res_3pl_1
plot(res_3pl_1)

## Item Characteristic Curves

for (i in 1:ncol(dat_1)) {
  plot(res_3pl_1, items = i, legend = TRUE)
  abline(h = 0.5, lty = 3, col = 2)
  abline(v = 0, lty = 2, col = 3)
}

## Item Information Curves
plot(res_3pl_1, items = c(1:6), type = "IIC", legend = T)
plot(res_3pl_1, items = c(7:12), type = "IIC", legend = T)
plot(res_3pl_1, items = c(13:18), type = "IIC", legend = T)


## Test Information Function
plot(res_3pl_1, type = "IIC", items = 0, legend = T)
plot(res_3pl_1, type = "IIC", legend = T)

##########################
# Genración de la escala #
##########################
# compu      
# micro      
# calefon 
# celular


anaitem <- as.data.frame(coef(res_3pl_1))
rownames(anaitem)
anaitem[order(anaitem$Dffclt),  ]
## Information at 3.5SD
anaitem$info <- NULL
anaitem$names <- NULL
set.seed(1234)
for (i in 1:ncol(dat_1)) {
  anaitem$names[i] <- rownames(anaitem)[i] 
  anaitem$info[i] <- round(100 * unlist(information(res_3pl_1, c(-3.5,3.5), items = i))$PropRange)
}

arrange(anaitem, Dscrmn, info)

## Eliminando las variables que menos aportan informacion

dat_1 %<>% select(-bote       ,
                  -carreta    ,
                  -moto,
                  -radio      
)

### Ejecutando el modelo 3PL con los item recortados

res_3pl_1 <- tpm(dat_1)

## coefiecientes del modelo 3PL 

anaitem <- as.data.frame(coef(res_3pl_1))
rownames(anaitem)
anaitem[order(anaitem$Dffclt),  ]

plot(res_3pl_1)

plot(res_3pl_1, items = c(4, 9, 10, 14), type = "IIC", legend = T)
plot(res_3pl_1, items = c(4, 9, 10, 14), legend = T)

## Standardizing the scores and creating the index
pres <- factor.scores(res_3pl_1, dat_1)

theta.est <- pres$score.dat$z1
hist(theta.est, breaks = 30)
plot(theta.est)

beta.est <- pres$coef[, 2]
hist(beta.est)
plot(beta.est)

c.est <- pres$coef[, 1]
hist(c.est)
plot(c.est)

a.est <- pres$coef[, 3]
hist(a.est)
plot(a.est)

wrightMap(theta.est, sort(beta.est))

## Test Information Function
plot(res_3pl_1, type = "IIC", items = 0, legend = T)
plot(res_3pl_1, type = "IIC", legend = T)

## Análisis de ítems
anaitem <- as.data.frame(coef(res_3pl_1))
rownames(anaitem)
anaitem[order(anaitem$Dscrmn),  ]


