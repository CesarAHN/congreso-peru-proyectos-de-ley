#---------------------------------------------------------------------------------
#                 CONGRESO DEL PERÚ - PROYECTOS DE LEY.
#---------------------------------------------------------------------------------

library(rvest)
library(dplyr)
library(openxlsx)
library(ggplot2)
#devtools::install_github("CesarAHN/datametria")
library(datametria)
library(tidyr)
library(stringr)
library(ggrepel)
#devtools::install_github("clauswilke/ggtextures")
library(ggtextures)
library(magick)
library(raster)
library(ggspatial)
library(sf)

# Lista de congresistas de la república. 

pw <- read_html("https://www.congreso.gob.pe/congresistasregion/")

congresistas<-pw %>% html_nodes("#objContents > div.modHTM > table") %>% html_table() %>% as.data.frame()

congresistas<-congresistas[,2:4]
congresistas<-na.omit(congresistas)

for (i in 1:length(congresistas$Var.2)) {
  if(congresistas$Var.2[i]==""){
    congresistas$Var.2[i]<-congresistas$Var.2[i-1]
  } 
}

congresistas<-congresistas %>% filter(Var.2!=APELLIDOS.Y.NOMBRES)

congresistas[,1:3]<-apply(congresistas[,1:3],2,limpiecito)

names(congresistas)<-c("REGION","Autores", "Partido")

congresistas$Autores<-ifelse(grepl("BUSTAMANTE DONAYRE",congresistas$Autores),
                             "BUSTAMANTE DONAYRE CARLOS ERNESTO",congresistas$Autores)
congresistas$Autores<-ifelse(grepl("DIAZ MONAGO",congresistas$Autores),
                             "DIAZ MONAGO FREDDY ROLAND",congresistas$Autores)
congresistas$Autores<-ifelse(grepl("YOREL KIRA",congresistas$Autores),
                             "ALCARRAZ AGÜERO YOREL KIRA",congresistas$Autores)

congresistas$Partido<-ifelse(grepl("AGRUPADO",congresistas$Partido),"NO AGRUPADO",
                             congresistas$Partido)
congresistas$Partido<-ifelse(grepl("AVANZA PAIS",congresistas$Partido),
                             "AVANZA PAIS", congresistas$Partido)

#-------------------------------------------------------------------
proy<-read.xlsx("ReporteProyectosLey.xlsx")

names(proy)<-proy[1,]

proy<-proy[-1,]

proy[,1:6]<-apply(proy[,1:6], 2, limpiecito)

proy<-proy %>% mutate(Autores = str_split(Autores, ";")) %>%
  unnest()

proy$Autores<-limpiecito(proy$Autores)
proy$Autores<-gsub(",","", proy$Autores)

# proy$Autores<-ifelse(is.na(proy$Autores), proy$Proponente, proy$Autores)

# Solo congresistas. 
proy<-proy[proy$Proponente=="CONGRESO",]

sum(is.na(proy$Autores))

proy[is.na(proy$Autores),] # 2 proyectos que no tienen autores.

#-------------------------------------------------------------------
# Juntando bases. 
df<-left_join(proy, congresistas, by="Autores")
sum(is.na(df$Partido))

unique(df[is.na(df$Partido),]$Autores)

df$Partido<-ifelse(grepl("HERRERA MAMANI FERNANDO",df$Autores),"PERU LIBRE", df$Partido)
df$REGION<-ifelse(grepl("HERRERA MAMANI FERNANDO",df$Autores),"TACNA", df$REGION)

tt<-tibble(Partido=c("ACCION POPULAR","FUERZA POPULAR","ALIANZA PARA EL PROGRESO","AVANZA PAIS",
                     "JUNTOS POR EL PERU","NO AGRUPADO","PERU LIBRE","PODEMOS PERU",
                     "RENOVACION POPULAR","SOMOS PERU"),
           image=list(image_read("https://upload.wikimedia.org/wikipedia/commons/e/ed/Acci%C3%B3n_Popular.png"),
                      image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/d/de/Fuerza_popular.svg/1200px-Fuerza_popular.svg.png"),
                      image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Alianza_para_el_Progreso_Peru.svg/160px-Alianza_para_el_Progreso_Peru.svg.png"),
                      image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Avanza_Pa%C3%ADs_2021.jpg/800px-Avanza_Pa%C3%ADs_2021.jpg"),
                      image_read("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTnCsB796Mu2XiOi8is51rafuf56gN41QQhFU4FDPaxNnAb8pjmXMKcELKjSi8v8G91JPE&usqp=CAU"),
                      image_read("https://r.datametria.com/wp-content/uploads/2021/12/agrupado.png"),
                      image_read("https://seeklogo.com/images/P/peru-libre-lapiz-logo-6FB42CF126-seeklogo.com.png"),
                      image_read("https://upload.wikimedia.org/wikipedia/commons/0/07/Logo_Podemos_Per%C3%BA.png"),
                      image_read("https://yt3.ggpht.com/ytc/AKedOLRUqtO4GNJob_QEoz7C4_5ZkZb8Eiu2HRfPjjrz=s900-c-k-c0x00ffffff-no-rj"),
                      image_read("https://pbs.twimg.com/profile_images/1348412879100325889/-PIqfVGa_400x400.jpg")))

df<-left_join(df,tt)

df$mes<-gsub("(\\d{4})-(\\d{2})-(\\d{2})","\\2",df$`Fecha de Presentación`)
df$dia<-gsub("(\\d{4})-(\\d{2})-(\\d{2})","\\3",df$`Fecha de Presentación`)

mm<-c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE",
      "NOVIEMBRE","DICIEMBRE")
meses<-data.frame(mes=c(paste0("0",1:9),10:12),mes_v=ordered(mm,levels=mm))
df<-left_join(df, meses)

#-------------------------------------------------------------------
# Números de proyectos de ley por meses.
df %>% distinct(`Proyecto de Ley`,mes_v) %>% group_by(mes_v) %>% count() %>% 
  ggplot(aes(x=mes_v,y=n, fill=mes_v))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Meses",y="Número de proyectos",title = "PROYECTOS DE LEY PRESENTADOS POR MES.",
       fill="", caption = "Solo proyectos presentados por el congreso.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(label=n),show.legend=F, bg="white", size=5)+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))

#-----
# Participación de partidos por proyectos.
df %>% distinct(`Proyecto de Ley`,Partido, image) %>% group_by(Partido,image) %>% 
  count() %>% filter(!is.na(Partido)) %>% 
  ggplot(aes(x=reorder(Partido,n),y=n, fill=Partido, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Partidos",y="Número de proyectos",title = "PRESENTACIÓN DE PROYECTOS DE LEY\nBANCADAS INVOLUCRADAS",
       fill="", caption = "Solo proyectos presentados por el congreso.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Partido,n),y=n+10,label=n),show.legend=F, bg="white", size=5)+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))

#-----
# Participación relativa de partidos en proyectos.
df %>% distinct(`Proyecto de Ley`,Partido,image) %>% group_by(Partido,image) %>% 
  count() %>% filter(!is.na(Partido)) %>% 
  left_join(congresistas %>% group_by(Partido) %>% summarise(tot=length(Partido))) %>% 
  mutate(p=round(n/tot,2)) %>% 
  ggplot(aes(x=reorder(Partido,p),y=p, fill=Partido, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Partidos",y="Número de proyectos",title = "PARTICIPACIÓN RELATIVA DE BANCADAS EN LA PRESENTACIÓN\nDE PROYECTOS***.",
       fill="", caption = "Solo proyectos presentados por el congreso.\n***Se entiende como el coeficiente entre proyectos presentados y el número de congresistas por bancada.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Partido,p),y=p+2,label=p),show.legend=F, bg="white", size=5)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7,hjust = 0),
        plot.title = element_text(face = "bold"))

#-----
# Congresistas con más proyectos presentados. 
df %>% group_by(Partido,Autores,image) %>% count() %>% arrange(-n) %>% head(n=10) %>% 
  ggplot(aes(x=reorder(Autores,n),y=n, fill=Autores, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Congresistas",y="Número de proyectos",title = "TOP - 10 CONGRESISTAS\nINVOLUCRADOS EN LA PRESENTACIÓN DE PROYECTOS DE LEY.",
       fill="", caption = "Solo proyectos presentados por el congreso.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Autores,n),y=n+10,label=n),show.legend=F, bg="white", size=5)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))

#-----
# Proyectos por la región del congresista.
Peru<-getData('GADM', country='Peru', level=1) %>% st_as_sf() # Mapa de Perú. 

Peru$NAME_1<-limpiecito(Peru$NAME_1)
Peru$NAME_1<-ifelse(Peru$NAME_1=="LIMA","LIMA PROVINCIAS",Peru$NAME_1)
Peru$NAME_1<-ifelse(grepl("PROVINCE",Peru$NAME_1),"LIMA",Peru$NAME_1)

Peru<-left_join(Peru, df %>% distinct(`Proyecto de Ley`,REGION) %>% group_by(REGION) %>% 
                  count() %>% filter(!is.na(REGION) & !grepl("EXTERIOR",REGION)),
                by=c("NAME_1"="REGION"))
Peru$etiq<-paste0(Peru$NAME_1,"\n",Peru$n)

Peru_d<-st_centroid(Peru)                                
Peru_d<-cbind(Peru, st_coordinates(st_centroid(Peru$geometry)))

Sur_America <- st_read("C:/OTROS/shapefile/sudamerica/Sudamérica.shp") # Sudamérica 
SurAmerica_utm <- st_transform(Sur_America,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

ggplot()+
  geom_sf(data= SurAmerica_utm, fill="White")+
  geom_sf(data= Peru, color="black", aes(fill=n))+
  scale_fill_distiller(palette = "YlGnBu", na.value = 'white',trans = "reverse")+
  annotate(geom = "text", x = -80, y = -10, label = "Océano \nPacífico", fontface = "italic", color = "Blue", size = 3)+
  annotate(geom = "text", x = -78, y = -2, label = "Ecuador", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -72, y = -1, label = "Colombia", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -70, y = -7, label = "Brasil", fontface = "italic", color = "Black", size = 3)+
  coord_sf(xlim = c(-81.3307,-68.65311), ylim = c(-18.3518,-0.03747),expand = FALSE)+
  ggrepel::geom_label_repel(data = Peru_d, aes(x=X, y=Y, label = etiq), size = 3,
                            color="black", fontface = "bold", alpha=.8)+
  theme_minimal() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "lightblue1",colour= "black", size = 1))+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.9, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  guides(fill = guide_legend(barheight = .01, barwidth = .7, raster = FALSE,
                             title = "Número de\nproyectos"))+
  labs(caption = "No se considera los proyectos de los congresistas representantes del extranjero.\nFUENTE:CONGRESO DE LA REPÚBLICA",
       title = "NÚMERO DE PROYECTOS DE LEY EN LOS QUE PARTICIPARON\nLOS CONGRESISTAS POR REGIÓN")+
  theme(legend.position = c(0.15,0.25),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(face = "bold"),
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))

#-----
# Participación relativa en la presentación de proyectos por región.
Peru<-getData('GADM', country='Peru', level=1) %>% st_as_sf() # Mapa de Perú. 

Peru$NAME_1<-limpiecito(Peru$NAME_1)
Peru$NAME_1<-ifelse(Peru$NAME_1=="LIMA","LIMA PROVINCIAS",Peru$NAME_1)
Peru$NAME_1<-ifelse(grepl("PROVINCE",Peru$NAME_1),"LIMA",Peru$NAME_1)

Peru<-left_join(Peru, df %>% distinct(`Proyecto de Ley`,REGION) %>% group_by(REGION) %>% 
                  count() %>% filter(!is.na(REGION) & !grepl("EXTERIOR",REGION)) %>% 
                  left_join(congresistas %>% group_by(REGION) %>% summarise(tot=length(REGION))) %>% 
                  mutate(p=round(n/tot,2)),by=c("NAME_1"="REGION"))
Peru$etiq<-paste0(Peru$NAME_1,"\n",Peru$p)

Peru_d<-st_centroid(Peru)                                
Peru_d<-cbind(Peru, st_coordinates(st_centroid(Peru$geometry)))

Sur_America <- st_read("C:/OTROS/shapefile/sudamerica/Sudamérica.shp") # Sudamérica 
SurAmerica_utm <- st_transform(Sur_America,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

ggplot()+
  geom_sf(data= SurAmerica_utm, fill="White")+
  geom_sf(data= Peru, color="black", aes(fill=p))+
  scale_fill_distiller(palette = "YlGnBu", na.value = 'white',trans = "reverse")+
  annotate(geom = "text", x = -80, y = -10, label = "Océano \nPacífico", fontface = "italic", color = "Blue", size = 3)+
  annotate(geom = "text", x = -78, y = -2, label = "Ecuador", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -72, y = -1, label = "Colombia", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -70, y = -7, label = "Brasil", fontface = "italic", color = "Black", size = 3)+
  coord_sf(xlim = c(-81.3307,-68.65311), ylim = c(-18.3518,-0.03747),expand = FALSE)+
  ggrepel::geom_label_repel(data = Peru_d, aes(x=X, y=Y, label = etiq), size = 3,
                            color="black", fontface = "bold", alpha=.8)+
  theme_minimal() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "lightblue1",colour= "black", size = 1))+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.9, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  guides(fill = guide_legend(barheight = .01, barwidth = .7, raster = FALSE,
                             title = "Cociente:"))+
  labs(caption = "No se considera los proyectos de los congresistas representantes del extranjero.\n***Se entiende como el coeficiente entre proyectos presentados y el número de congresistas por región.\nFUENTE:CONGRESO DE LA REPÚBLICA.",
       title = "PARTICIPACIÓN RELATIVA EN PROYECTOS DE LEY POR REGIÓN***")+
  theme(legend.position = c(0.15,0.25),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(face = "bold"),
        plot.caption = element_text(face = "bold", size = 7, hjust = 0),
        plot.title = element_text(face = "bold"))

#------
# Proyectos de ley publicados o en autografa por bancada.
df %>% distinct(`Proyecto de Ley`,Estado, Partido,image) %>% group_by(Partido,image) %>% count(Estado) %>% 
  filter(grepl("AUTOGRAFA|PERUANO",Estado)) %>% filter(!is.na(Partido)) %>% mutate(sum=sum(n)) %>% 
  dplyr::select(-Estado,-n) %>% distinct() %>% 
  ggplot(aes(x=reorder(Partido,sum),y=sum, fill=Partido, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Congresistas",y="Número de proyectos",title = "PROYECTOS DE LEY PUBLICADOS EN EL PERUANO O EN AUTÓGRAFA\nPOR BANCADA.",
       fill="", caption = "Solo proyectos presentados por el congreso.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Partido,sum),y=sum+.5,label=sum),show.legend=F, bg="white", size=5)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))

#--------
# Participación en proyectos de ley que han sido publicados o en autografa.
df %>% distinct(`Proyecto de Ley`,Estado, Partido,image) %>% group_by(Partido,image) %>% count(Estado) %>% 
  filter(grepl("AUTOGRAFA|PERUANO",Estado)) %>% filter(!is.na(Partido)) %>% mutate(sum=sum(n)) %>% 
  dplyr::select(-Estado,-n) %>% distinct() %>% 
  left_join(congresistas %>% group_by(Partido) %>% summarise(tot=length(Partido))) %>% 
  mutate(p=round(sum/tot,2)) %>% 
  ggplot(aes(x=reorder(Partido,p),y=p, fill=Partido, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Congresistas",y="Número de proyectos",title = "PARTICIPACIÓN EN PROYECTOS DE LEY PUBLICADOS EN EL PERUANO O EN AUTÓGRAFA\nPOR BANCADA.",
       fill="", caption = "Solo proyectos presentados por el congreso.\n***Se entiende como el coeficiente entre proyectos presentados y el número de congresistas por bancada.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Partido,p),y=p+.1,label=p),show.legend=F, bg="white", size=5)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7, hjust = 0),
        plot.title = element_text(face = "bold"))

#-----------------
# Participación de congresistas en leyes publicadas en el peruano o en autografa.
df %>% distinct(`Proyecto de Ley`,Estado, Autores,image) %>% group_by(Autores,image) %>% count(Estado) %>% 
  filter(grepl("AUTOGRAFA|PERUANO",Estado)) %>% filter(!is.na(Autores)) %>% mutate(sum=sum(n)) %>% 
  dplyr::select(-Estado,-n) %>% distinct() %>% arrange(-sum) %>% head(n=8) %>% 
  ggplot(aes(x=reorder(Autores,sum),y=sum, fill=Autores, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Congresistas",y="Número de proyectos",title = "TOP 8 - CONGRESISTAS.\nCON MÁS PROYECTOS DE LEY PUBLICADOS EN EL PERUANO\nO EN AUTÓGRAFA.",
       fill="", caption = "Solo proyectos presentados por el congreso.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Autores,sum),y=sum+.5,label=sum),show.legend=F, bg="white", size=5)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))
