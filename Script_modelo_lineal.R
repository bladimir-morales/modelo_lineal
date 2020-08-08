#Librerias
library(dplyr)
library(readxl)
library(ggplot2)
rm(list=ls())

#Base de datos de la Y=esperanzade vida Banco Mundial 2018
esp_vida=read_xls("C:/Users/Bladimir/Documents/Proyectos/modelos_estadisticos/BD/esperanzavida.xls") %>% 
  select(pais,cod_pais,esp_vida="2018") %>% 
  dplyr::filter(!is.na(esp_vida))
#Base de datos de la X=tasa de analfabatismo Banco Mundial 2018
tasa_analfab=read_xls("C:/Users/Bladimir/Documents/Proyectos/modelos_estadisticos/BD/analfabetismo.xls") %>% 
  select(pais,cod_pais,tasa_analfab="2018") %>% 
  dplyr::filter(!is.na(tasa_analfab))

bd=inner_join(tasa_analfab,esp_vida) 

#Análisis de los datos
## Análisis de correlación de Pearson
cor.test(bd$esp_vida,bd$tasa_analfab)
## Gráfico
ggplot(bd,aes(x=tasa_analfab,y=esp_vida))+
  geom_point()+
  labs(title="Dispersograma",
       caption = "Datos Banco Mundial 2018",
       y="Esperanza de vida",
       x="Tasa de analfabetismo")

#Modelo Lineal
mod_lin=lm(esp_vida ~ tasa_analfab,data=bd)
summary(mod_lin)

## Análisis de Varianza
anova(mod_lin)

## IC para los parámetros estimados
confint(mod_lin,level=0.95)
## Intervalo de confianza para el valor medio de las predicciones de Y (95%)
x0=data.frame(tasa_analfab=85)
predict(mod_lin,newdata = x0,interval = "confidence")
## Intervalo de confianza para un valor individual de las Y (95%)
predict(mod_lin,newdata = x0,interval = "prediction")

#Gráfico del modelo
pred_media=predict(mod_lin,interval="confidence")
pred_individual=predict(mod_lin,interval = "prediction")
bd_pred=cbind(bd,pred_individual)

ggplot(bd_pred,aes(tasa_analfab,esp_vida))+
  geom_point()+
  geom_line(aes(y=lwr),color="red",linetype="dashed")+
  geom_line(aes(y=upr),color="red",linetype="dashed")+
  geom_smooth(method=lm,se=T)+
  labs(title="Modelo de Regresión Lineal",
       caption = "Datos Banco Mundial 2018",
       y="Esperanza de vida",
       x="Tasa de analfabetismo")

#Análisis de residuos
bd_pred$residuos=residuals(mod_lin)
ggplot(bd_pred,aes(tasa_analfab,esp_vida))+
  geom_smooth(method = lm,se=F)+
  geom_segment(aes(xend=tasa_analfab,yend=fit),alpha=.4)+
  labs(title="Residuos respecto al modelo lineal",
       y="Esperanza de vida",
       x="Tasa de analfabetismo")
#Normalidad de residuos estandarizados
bd_pred$res_estand=rstandard(mod_lin)

ggplot(bd_pred)+
  stat_qq(aes(sample=res_estand))+
  geom_abline(color="blue")+
  labs(title="Normal Q-Q Plot",
       y="Residuos Estandarizados",
       x="Teóricos")
##Histograma de residuos
ggplot(bd_pred,aes(res_estand))+
  geom_histogram()+
  labs(title="Histograma de Residuos Estandarizados",
       x="Residuos estandarizados")

##Boxplot de residuos
ggplot(bd_pred,aes(y=res_estand))+
  geom_boxplot()+
  labs(title="Boxplot de Residuos Estandarizados",
       y="Residuos estandarizados")

#Varianza Constante
## Predichos y Residuos 
ggplot(bd_pred,aes(fit,res_estand))+
  geom_point()+
  geom_hline(yintercept = 0,color="blue")+
  labs(title="Plot Predichos vs Residuos Estandarizados",
       y="Residuos estandarizados",
       x="Predichos")

## Predichos y Residuos Studentizados
bd_pred$res_student=rstudent(mod_lin)
ggplot(bd_pred,aes(x=fit,res_student))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="Plot Predichos vs Residuos Studentizados",
       y="Predichos",
       x="Residuos Studentizados")

# Valores atípicos
## variable independiente y residuos
ggplot(bd_pred,aes(tasa_analfab,res_estand))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="Plot Tasa de Analfabetismo vs Residuos",
       y="Residuos estandarizados",
       x="Tasa analfabetismo")

# Gráfico de autocorrelación de residuos
ggplot(bd_pred,aes(x=c(1:length(bd$pais)),y=residuos))+
  geom_point()+
  geom_line(color="blue")+
  geom_hline(yintercept = 0)+
  labs(title="Autocorrelación de Residuos",
       y="Residuos",
       x="Números ordenados")
  

  





