#Remuneraciones
#Datos de los Censos Económicos 2019


##Borrar datos del entornot
rm(list=ls())

##Crear folders de almacenamiento
dir.create("censoseco2019")
dir.create("censoseco2019/catálogo")

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, kableExtra, extrafont)


##Url general de los Censos económicos 2019

url<-"https://www.inegi.org.mx/contenidos/programas/ce/2019/Datosabiertos/ce2019_"

#Url del catálogo para crear la lista
urlcat<-"https://www.inegi.org.mx/contenidos/app/ageeml/catun_entidad.zip"



#Descarga de catálogo de entidades federativas====

#Se crea tempfile para no almacenar los zips
temp<-tempfile()
##Descargar y extraer catálogo
download.file(urlcat,
              destfile = temp)

unzip(temp,
      exdir = "censoseco2019/catálogo")
unlink(temp)


#Leer archivo de catálogo y arreglar

cat<-read.csv("censoseco2019/catálogo/AGEEML_20211271319772.csv",
              encoding ="latin1" )%>%
  #Remover la última fila
  slice(1:(n()-1))%>%
  #Renombrar y seleccionar la variable de interés
  rename(entidad=1,
         nom_ent=2,
         abrev=3)%>%
  select(entidad,nom_ent,abrev)%>%
  #Quitar punto al final, minúsculas y quitar espacios intermedios
  mutate(abrev=str_remove(abrev,"[.]"))%>%
  mutate(abrev=tolower(abrev))%>%
  mutate(abrev=str_replace_all(abrev," ",""))


#Crear listado de abreviaturas
lista<-cat$abrev

#Descarga de archivos de Censos Económicos 2019====
##Descargar y extrar los datos para las 32 entidades federativas
for (i in seq_along(lista)) {  
  ##Descargar
  temp<-tempfile()
  download.file(paste0(url,lista[i],"_csv.zip"),
                destfile = temp)
  
  ##Extraer
  unzip(temp,
        exdir = "censoseco2019")
  unlink(temp)
  
  
}


#Remover archivos de bitácora de cambios

for (i in seq_along(lista)) {
  if (file.exists(paste0("censoseco2019/conjunto_de_datos/bitacora_de_cambios_ce2019_",
                         lista[i],
                         ".csv"))) {
    file.remove(paste0("censoseco2019/conjunto_de_datos/bitacora_de_cambios_ce2019_",
                       lista[i],
                       ".csv"))
  }
  
}


#Lectura y limpieza de datos====
archivosest<-list.files(path = "censoseco2019/conjunto_de_datos", 
                        pattern = "ce2019_")



datosce<-purrr::map(archivosest,
                    ~ read_csv(glue::glue("censoseco2019/conjunto_de_datos/{.x}"), 
                               na = "*") %>% 
                      #Nombres de las variables en minúsculas
                      janitor::clean_names()%>%
                      #Seleccionar variables a usar
                      select(entidad,codigo,
                             a171a)%>%
                      #Renombrar personal ocupado
                      rename(po=a171a)%>%
                      #Transformar código de edificación residencial,
                      #manufacturas y transportes aéreos a dos dígitos
                      mutate(codigo=ifelse(codigo==2361,99,
                                           ifelse(codigo=="31-33",31,
                                                  ifelse(codigo=="48-49",48,
                                                         codigo))))%>%
                      #Filtrar por sector
                      filter(nchar(as.character(codigo)) == 2)%>%
                      #Obtener totales de cada actividad
                      group_by(entidad,codigo)%>%
                      #Se filtra la primera observación
                      slice(c(1))%>%
                      ungroup()%>%
                      ##Se transforma la lista a dataframe
                      as.data.frame.data.frame())


##Datos agrupados
po<-data.table::rbindlist(datosce, fill = TRUE, idcol = F)%>%
  #Transformar la clave de la entidad a numérico y pegarle los nombres
  mutate(entidad=as.numeric(entidad))%>%
  left_join(cat)


#Códigos de actividad
codact<-read.csv("censoseco2019/catalogos/tc_codigo_actividad.csv", 
                 encoding = "UTF-8", check.names = T,header = T)%>%
  ##Extraer códigos de index
  rownames_to_column(.,"codigo")%>%
  #Renombrar
  rename(nombre=2,
         desc=3)%>%
  #Seleccionar variables de interés
  select(codigo, nombre,desc)%>%
  ##Transformar 
  #edificación residencial a sector en catálogo
  mutate(desc=ifelse(nombre=="Edificación residencial" & desc=="Rama",
                     "Sector",desc),
         codigo=ifelse(nombre=="Edificación residencial" & desc=="Sector","99",
                       #Manufacturas y transportes a dos dígitos
                       ifelse(codigo=="31-33","31",
                              ifelse(codigo=="48-49","48",codigo))))%>%
  #>Filtrar sectores
  filter(desc=="Sector")


##Llamar los datos de Producción bruta
po<-po%>%
  #Pegar categorías del catálogo de actividades económicas
  left_join(codact)%>%
  mutate(po = replace_na(po, 0))




##Gráficas para cada una de las entidades federativas====

#Generar nombres para filtro y números para guardar los archivos
entidades<-unique(cat$nom_ent)
num_ent<-unique(cat$entidad)


for (i in seq_along(entidades)) {
  
  po%>%
    filter(nom_ent==entidades[i])%>%
    
    #Variables de interés  
    select(nombre,po)%>%
    #Ordenar de mayor a menor
    arrange(po)%>%
    mutate(nombre=factor(nombre, levels = nombre))%>%
    ##Gráfica de barras
    
    ggplot(.,aes(nombre,po))+
    geom_bar(stat="identity", fill="#e6550d", width=.8) +
    geom_text(aes(label=format(round(po,2), big.mark = ",")),
              vjust=0.5,hjust=0, size=6.5,fontface="bold")+
    coord_flip() +
    xlab("") +
    theme_minimal()+
    labs(
      title = paste0(entidades[i],". Remuneración media por persona ocupada remunerada por sector"),
      subtitle = "(pesos)",
      y = "Pesos",
      x="",
      caption = "Nota: Remuneración media por persona ocupada remunerada: 
Pagos y aportaciones, en dinero y especie, antes de cualquier deducción, que recibió en promedio cada persona remunerada durante 2018. 
Resulta de dividir el monto de las remuneraciones pagadas al personal remunerado que depende de la razón social, 
entre el total de personal ocupado remunerado.
Fuente: Elaborado por CANADEVI Nacional. Gerencia de Fondos de Vivienda. Coordinación de Indicadores de Vivienda
con información de INEGI. Censos Económicos, 2019."
    )+
    theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
          plot.caption = element_text(hjust = 0,size=18),
          legend.position = "none",
          axis.text.x = element_blank(),
          text=element_text("Century Gothic",
                            size=20))
    
  
  ggsave(file=paste0("censoseco2019/rem",num_ent[i],".png"),
         height=15, width=46, units='in', dpi=300)
  }