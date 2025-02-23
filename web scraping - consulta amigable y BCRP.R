##############################################################################
########################## WEB SCRAPING ######################################
##################### Consulta amigable MEF ##################################
##############################################################################
##############################################################################

linkedin: https://www.linkedin.com/in/mirko-smith/

library(rvest)
library(dplyr)

##############################
# DATOS GASTO 4 TRIMESTRE 2023
##############################

#cargar la información html, notese que en el link, 2023 es el año de los datos y 12 el mes, pudiendo usar 1, 3, 6, 9, haciendo referencia a los trimestre 1,2,3 y 4
url1<-"https://apps5.mineco.gob.pe/transparencia/Reportes/RptDEPxFUA.aspx?y=2023&t=12"
pw1<-read_html(url1)

#seleccionar la información (nodo) de la pagina y trasnformar a formato df
df1<-pw1 %>% html_nodes("#Pnl0 td") %>% html_text() %>% .[-2] %>% 
  matrix(.,ncol=27,byrow = T) %>% as.data.frame() # para ubicar el nodo puede usar el codigo fuente de html o la extensión SelectorGadget en Chrome

  #-ajustamos los nombres de columas y filas de la información
names(df1)<-c("DEPARTAMENTO",substring(df1[1,2:26],5),"TOTAL")
df1[,1]<-substring(df1[,1],5)

  #-seleccionamos los datos y trasnformamos en clase numérico
df1<-df1 %>% .[-1,] %>% mutate(across(!matches("DEPARTAMENTO"), 
                ~ as.numeric(gsub(",", "", .)))) %>% 
  mutate(AÑO="2023/Q4")

##############################
# DATOS GASTO 4 TRIMESTRE 2024
##############################

#cargar la información html
url2<-"https://apps5.mineco.gob.pe/transparencia/Reportes/RptDEPxFUA.aspx?y=2024&t=12"
pw2<-read_html(url2)

#seleccionar la información de la pagina y trasnformar a formato df
df2<-pw2 %>% html_nodes("#Pnl0 td") %>% html_text() %>% .[-2] %>% 
  matrix(.,ncol=27,byrow = T) %>% as.data.frame()

  #-ajustamos los nombres de columas y filas de la información
names(df2)<-c("DEPARTAMENTO",substring(df2[1,2:26],5),"TOTAL")
df2[,1]<-substring(df2[,1],5)

#-seleccionamos los datos y trasnformamos en clase numérico
df2<-df2 %>% .[-1,] %>% mutate(across(!matches("DEPARTAMENTO"), 
                                    ~ as.numeric(gsub(",", "", .)))) %>% 
  mutate(AÑO="2024/Q4")

###################################
# UNION DE LOS DATOS 2023 Y 2024
##################################

df<-rbind(df1,df2)

################################################################################
##############################  ANÁLISIS DE DATOS ##############################
################################################################################


#Calculamos la varaición porcentual del gasto por departamento y o guardamos en df_final
df_final<-df %>% select(DEPARTAMENTO,AÑO,TOTAL)%>% arrange(DEPARTAMENTO, AÑO) %>%  
  group_by(DEPARTAMENTO) %>% 
  mutate(across(
  where(is.numeric),
  ~ (. - lag(.)) / lag(.) * 100,
  .names = "VAR_{col}"
)) %>% select(DEPARTAMENTO,VAR_TOTAL) %>% na.omit()

################################################################################
############################## PRESENTACIÓN VISUAL #############################
################################################################################

library(geodata)
library(ggplot2)


# Con el paquete geodata descargamos el mapa de los departamentos del Perú
mp<-geodata::gadm(country = "PER", level=1,path=tempdir()) %>% sf::st_as_sf()
mp$NAME_1<-chartr("ÁÉÍÓÚ", "AEIOU", toupper(mp$NAME_1)) # transformamos a mayusculas los nombres

# unimos nuestro data frame que contiene el mapa a nuestra información de gasto, luego creamos un grupo para colorear un mapa de color

mp_df<-mp %>% inner_join(df_final,
                  by=join_by("NAME_1"=="DEPARTAMENTO"))

mp_df<-mp_df %>%
  mutate("grupo"=cut(mp_df$VAR_TOTAL,5))
# usando ggplot usamos geom_sf para graficar el mapa y las demas capas para añadir los sombrear los departamentos, colocalr el nombre del departamento y el dato de variaciaón %

mp_df %>%
  ggplot()+
  geom_sf(aes(fill=`grupo`))+
  geom_sf_text(aes(label = NAME_1),
               size = 1.8, nudge_y = 0.25,col="black")+
  geom_sf_label(aes(label=round(`VAR_TOTAL`,2)),
                size=1.8,nudge_y = -0.25,fill="gray69",col="black")+
  scale_fill_brewer(palette = "RdYlBu")+
  labs(x = NULL,
       y = NULL,
       title = expression("Perú: Crecimiento del gasto público por departamento Q4-2024"^1),
       subtitle = "(Variación porcentual 12 meses)",
       caption = expression("Nota. Elaborado a partir del Ministerio de Economía y Finanzas\nla variación se calcula respecto al Q4-2023"^1),
       fill="Intervalos %")+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title=element_text(size=10, hjust=0,vjust = +1.5),
        plot.subtitle=element_text(size=8, hjust=0, face = "italic"),
        plot.caption = element_text(hjust = 0, size =6,),
        legend.key.height = unit (0.3, 'cm'),
        legend.key.width = unit (0.3, 'cm'),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        strip.text = element_text(size = 9, vjust = -0.1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
  
#ggdark::invert_theme_elements(ggthemes::theme_fivethirtyeight())
# para utilizar un tema oscuro


##############################################################################
########################## WEB SCRAPING ######################################
#######################  series de BCRP #####################################
##############################################################################
##############################################################################
# usando la pagina del BCRP, descargamos la serie reservas internacionales netas con serie PN00026MM
library(dplyr)
library(rvest)
#cargar la información html, notese que se si se desea descargar otra serie 
#se debe modificar la serie "PN00026MM", frecuencia e intervalo de datos
url1<-"https://estadisticas.bcrp.gob.pe/estadisticas/series/mensuales/resultados/PN00026MM/html/2010-1/2024-12/"
pw1<-read_html(url1)

#seleccionar la tabla del html y trasnformar a formato df
# para ubicar el nodo puede usar el codigo fuente de html o la extensión SelectorGadget en Chrome
datos<-pw1 %>% html_nodes("table.series") %>% html_table() %>% .[[1]]

print(datos)


names(datos)[2]<-"RIN" # renombramos la columna por su acronimo

#transformamos a formato time series
ts_20<-ts(datos[,2],start =c(2010,1), #desde el año 2000, primer mes
              frequency = 12)

movav<-ma(ts_20, order = 20, centre = T) #calculamos media movil de orden 20

# graficamos la serie sin antes diferenciarla en 12 meses
serie_RIN<-forecast::autoplot(cbind(diff(ts_20,12),diff(movav,12)),size=1,alpha=0.5) +
  labs(y= NULL,
       title = "Reservas internacionales netas",
       subtitle = "(millones de s/)",
       caption="Nota. Elaborado a partir del Banco central de Resrva del Perú",
       colour="")+
  scale_colour_manual(labels = c("Real", "Media movil (20)"), 
                      values = c("steelblue2", "red"))+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title=element_text(size=10, hjust=0,vjust = +1.5),
        plot.subtitle=element_text(size=9, hjust=0, face = "italic"),
        plot.caption = element_text(hjust = 0, size =8,),
        legend.key.height = unit (0.3, 'cm'),
        legend.key.width = unit (0.3, 'cm'),
        legend.text = element_text(size=7),
        legend.title = element_text(size=7),
        strip.text = element_text(size = 9, vjust = -0.1),
        axis.text.y = element_blank())


ts_20 %>% diff(12) %>% auto.arima()->fore_ts # estimamos un SARIMA


forecast_RIN<-fore_ts %>% 
  forecast::forecast(h=12) %>% 
  sw_sweep() %>% 
  ggplot( aes(x = index, y = RIN, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95, fill="IC 95%"),
              color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80,fill="IC 80%"),
              color = NA, size = 0, alpha = 0.3) +
  geom_line(size = 1) +
  labs(title = "Reservas internacionales netas - Predicción usando SARIMA", 
       x = NULL, y = NULL ,colour="", fill="")+
  scale_colour_manual(labels = c("Real", "Forecast"), 
                      values = c("steelblue2", "red"))+
  scale_fill_manual(values=c("#D5DBFF","#596DD5"))+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title=element_text(size=10, hjust=0,vjust = +1.5),
        plot.subtitle=element_text(size=9, hjust=0, face = "italic"),
        plot.caption = element_text(hjust = 0, size =8,),
        legend.key.height = unit (0.3, 'cm'),
        legend.key.width = unit (0.3, 'cm'),
        legend.text = element_text(size=7),
        legend.title = element_text(size=7),
        strip.text = element_text(size = 9, vjust = -0.1),
        axis.text.y = element_blank())

library(patchwork)

serie_RIN/forecast_RIN

