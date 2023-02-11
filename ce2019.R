#Cuadros valor bruto de la producción estatales
#Datos de los Censos Económicos 2019


##Borrar datos del entorno
rm(list=ls())

##Crear folders de almacenamiento
dir.create("censoseco2019")
dir.create("censoseco2019/catálogo")

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, kableExtra)


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

cat<-read.csv("censoseco2019/catálogo/AGEEML_20231161410585.csv",
              encoding ="latin1" )%>%
    
  #Renombrar y seleccionar la variable de interés
  rename(entidad=2,
         nom_ent=3,
    abrev=4)%>%
  select(entidad,nom_ent,abrev)%>%
  #Quitar punto al final, minúsculas y quitar espacios intermedios
  mutate(abrev=str_remove(abrev,"[.]"))%>%
  mutate(abrev=tolower(abrev))%>%
  mutate(abrev=str_replace_all(abrev," ",""))
  

#Crear listado de abreviaturas
lista<-unique(cat$abrev)

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
                    a111a)%>%
             #Renombrar valor agregado
             rename(pbt=a111a)%>%
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
pbt<-data.table::rbindlist(datosce, fill = TRUE, idcol = F)%>%
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
  pbt<-pbt%>%
     #Pegar categorías del catálogo de actividades económicas
    left_join(codact)%>%
    mutate(pbt = replace_na(pbt, 0))%>%
    group_by(nom_ent)%>%
    ##Sumar el valor de la producción sin considerar edificación residencial
    mutate(total=sum(pbt[codigo!="99"]),
           #Calcular porcentaje del total
    pct=pbt/total*100)%>%
    ungroup()
  
  
  
  
  ##Tablas para cada una de las entidades federativas====
  
  #Generar nombres para filtro y números para guardar los archivos
  entidades<-unique(cat$nom_ent)
  num_ent<-unique(cat$entidad)
  
  
  for (i in seq_along(entidades)) {
  
    pbt%>%
  filter(nom_ent==entidades[i])%>%
    #Ordenar de mayor a menor
    arrange(desc(pct))%>%
    #Variables de interés  
    select(nombre,pct)%>%  
    #Formato de números
    mutate(pct=format(round(pct,1)))%>%
    ##Crear tabla
    kable(caption=paste(text_spec(entidades[i],bold=T, 
                                   color="black",font_size = 30 ),
                         text_spec(". ",
                                   bold=T, color="black",font_size = 30),
                         text_spec("Distribución de las actividades económicas",
                                   bold=T, color="black",font_size = 30),
                         sep="\n"),
          format="html",
          align = "c",
          col.names = c("Sector",
                        "Porcentaje de la producción bruta"))%>%
    kable_styling(full_width = F, font_size = 20,
                  html_font = "Century Gothic")%>%
    row_spec(0, bold = F, color = "black", background = "#feb24c")%>%
    footnote(general = "Elaborado por CANADEVI Nacional. Gerencia de Fondos de Vivienda. 
Coordinación de Indicadores de Vivienda con datos de INEGI. Censos Económicos 2019.\n
La suma de los porcentajes es mayor a 100 ya que en el presente cuadro se incluye a la edificación residencial\n
La producción bruta total es el valor de todos los bienes y servicios producidos o comercializados por la unidad económica como resultado del ejercicio de sus actividades, comprendiendo el valor de los productos elaborados; el margen bruto de comercialización; las obras ejecutadas; los ingresos por la prestación de servicios, así como el alquiler de maquinaria y equipo, y otros bienes muebles e inmuebles; el valor de los activos fijos producidos para uso propio, entre otros. Incluye la variación de existencias de productos en proceso. Los bienes y servicios se valoran a precios productor.",
             general_title = "
Fuente: ",
             threeparttable=T)%>%
    as_image(file=paste0("censoseco2019/vbp",num_ent[i],".png"))
  }
