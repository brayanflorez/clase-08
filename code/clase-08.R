## Brayan Florez
## Update: 1 -03-2024

## limpiar entonro
rm(list=ls())

## instalar/llamar pacman
require(pacman)

## usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(tidyverse, ## manipular/limpiar conjuntos de datos.
       rio, ## para leer/escribir archivos desde diferentes formatos. 
       skimr, ## skim: describir un conjunto de datos
       janitor) ##  tabyl: frecuencias relativas

## **[1.] Aplicación: GEIH

# Importe 
cg <- import("input/Enero - Cabecera - Caracteristicas generales (Personas).csv") %>% clean_names()

ocu <- import("input/Enero - Cabecera - Ocupados.csv") %>% clean_names()

##clean_names limpia los nombres de al información, ej: todos los ponen en minuscula

# verifique las llaves con el siguiente vector c("directorio","secuencia_p","orden")

# colapse los datos de la GEIH
geih <- left_join(x = cg, y = ocu, by = c("directorio","secuencia_p","orden"))

#Left_joinMantiene todos los datos de X y todos los de Y qeu tambieén están en X

## **[2.] Descriptivas de un conjunto de datos**
tabyl(geih$p6020)


### **2.1 Generales**

# Utilice summary para una descripción general 

#summarize_all
#summarize_if
#summarize

summary(geih$p6040)
select(geih, p6020,p6040,p6500)%>%summary()
select(geih, p6020,p6040,p6500)%>%summarize_all(mean)

#summarize con Z me resume toda mi base de datos


# select + summarize_all 


### **2.2 Agrupadas**

geih <- mutate(geih, mujer=ifelse(p6020==2,1,0))

# ingreso laboral promedio por sexo

geih%>% group_by(mujer)%>%summarise(ing_mean=mean(p6500,na.rm=T))

# ingreso laboral promedio por sexo y tipo de contrato

geih%>% group_by(mujer,p6450)%>%summarise(ing_mean=mean(p6500,na.rm=T))

# ingreso laboral promedio/mediano y años promedio en fondo de pension por sexo

geih%>% 
  group_by(mujer,p6450)%>%
  summarise(ing_mean=mean(p6500,na.rm=T)
            ,med_edad=median(p6040,na.rm=T))

#Diferencia de salarios por departamento

ing <- geih%>% 
  group_by(mujer,dpto.x)%>%
  summarise(ing_mean=mean(p6500,na.rm=T))

ing
export(ing,"output/ingreso_mujer.xlsx")


## pivot wider

ing_p <-pivot_wider(data=ing, 
                     id_cols=dpto.x ,
                     names_from=mujer ,
                     values_from=ing_mean)

##rename

ing_p <- rename(.data=ing_p, "mean_mujer"="1", "mean_hombre"="0")

ing_p <- mutate(ing_p, mean_dif=mean_mujer-mean_hombre)

hist(ing_p$mean_dif)




#agregarle a eso la mediana de lo hombre, mujeres y al diferencia entre esos. En total tienen que quedar 7 columnas. 












# ingreso laboral promedio/mediano y años promedio en fondo de pension por sexo y tipo contrato

# guardar resultados en objeto


### **2.3 Pivotear**

# pivot_longer

# pivot_wider



