## Eduard Martinez
## Update: 11-03-2024

## limpiar entonro
rm(list=ls())

## instalar/llamar pacman
require(pacman)

## usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(tidyverse, ## manipular/limpiar conjuntos de datos.
       rio, ## para leer/escribir archivos desde diferentes formatos. 
       skimr, ## skim: describir un conjunto de datos
       janitor) ##  tabyl: frecuencias relativas

## **[1.] Aplicación: GEIH*

# Importe 
cg <- import("input/Enero - Cabecera - Caracteristicas generales (Personas).csv") %>% clean_names()

ocu <- import("input/Enero - Cabecera - Ocupados.csv") %>% clean_names()

# verifique las llaves con el siguiente vector c("directorio","secuencia_p","orden")

# colapse los datos de la GEIH
geih <- left_join(x = cg, y = ocu, by = c("directorio","secuencia_p","orden"))

## **[2.] Descriptivas de un conjunto de datos**

### **2.1 Generales**

# Utilice summary para una descripción general 

# select + summarize_all 


### **2.2 Agrupadas**

# ingreso laboral promedio por sexo

# ingreso laboral promedio por sexo y tipo de contrato

# ingreso laboral promedio/mediano y años promedio en fondo de pension por sexo

# ingreso laboral promedio/mediano y años promedio en fondo de pension por sexo y tipo contrato

# guardar resultados en objeto


### **2.3 Pivotear**

# pivot_longer

# pivot_wider



