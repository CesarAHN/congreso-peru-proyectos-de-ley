
# BIENVENIDOS!!!

# ¿CÓMO ESTÁN TRABAJANDO LOS CONGRESISTAS DEL PERÚ - PROYECTOS DE LEY?

Se crea este repositorio con el fin de poder conocer cómo están
trabajando los congresistas de la república del Perú, específicamente en
la presentación de proyectos de ley y en la concreción final de
convertirse en ley.

Se muestra información del número de proyectos de ley y leyes por
bancadas, regiones y por congresistas.

Esta información se extrajo desde el portal Voto informado. —&gt;
<https://www.congreso.gob.pe/>

## ¿CÓMO ESTÁ TRABAJANDO EL CONGRESO?

### PROYECTOS DE LEY.

#### Proyectos de ley presentados por meses.

``` r
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
```

![](README-unnamed-chunk-3-1.png)<!-- -->

#### Bancadas involucradas en la presentación de proyectos de ley.

``` r
df %>% distinct(`Proyecto de Ley`,Partido, image) %>% group_by(Partido,image) %>% 
  count() %>% filter(!is.na(Partido)) %>% 
  ggplot(aes(x=reorder(Partido,n),y=n, fill=Partido, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Partidos",y="Número de proyectos",title = "PRESENTACIÓN DE PROYECTOS DE LEY\nBANCADAS INVOLUCRADAS.",
       fill="", caption = "Solo proyectos presentados por el congreso.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Partido,n),y=n+10,label=n),show.legend=F, bg="white", size=5)+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7),
        plot.title = element_text(face = "bold"))
```

![](README-unnamed-chunk-4-1.png)<!-- -->

#### Participación relativa de bancadas en la presentación de proyectos de ley.

``` r
df %>% distinct(`Proyecto de Ley`,Partido,image) %>% group_by(Partido,image) %>% 
  count() %>% filter(!is.na(Partido)) %>% 
  left_join(congresistas %>% group_by(Partido) %>% summarise(tot=length(Partido))) %>% 
  mutate(p=round(n/tot,2)) %>% 
  ggplot(aes(x=reorder(Partido,p),y=p, fill=Partido, image=image))+
  geom_isotype_col(img_height = grid::unit(1, "null"), img_width = NULL,
                   ncol = 1, nrow = 1, hjust = 1, vjust = 0.5)+
  scale_fill_brewer(palette = "Spectral")+
  labs(x="Partidos",y="Número de proyectos",title = "PARTICIPACIÓN RELATIVA DE BANCADAS EN LA PRESENTACIÓN\nDE PROYECTOS DE LEY***.",
       fill="", caption = "Solo proyectos presentados por el congreso.\n***Se entiende como el coeficiente entre proyectos presentados y el número de congresistas por bancada.\nFUENTE: CONGRESO DE LA REPÚBLICA.")+
  geom_label(aes(x=reorder(Partido,p),y=p+2,label=p),show.legend=F, bg="white", size=5)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        plot.caption = element_text(face = "bold", size = 7,hjust = 0),
        plot.title = element_text(face = "bold"))
# Joining, by = "Partido"
```

![](README-unnamed-chunk-5-1.png)<!-- -->

#### Congresistas con más proyectos de ley presentados.

``` r
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
```

![](README-unnamed-chunk-6-1.png)<!-- -->

#### Proyectos de ley presentados por regiones.

``` r
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
# Reading layer `SudamÃ©rica' from data source 
#   `C:\OTROS\shapefile\sudamerica\SudamÃ©rica.shp' using driver `ESRI Shapefile'
# Simple feature collection with 15 features and 1 field
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -109.4461 ymin: -58.49861 xmax: -26.24139 ymax: 12.59028
# Geodetic CRS:  WGS 84
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
```

![](README-unnamed-chunk-7-1.png)<!-- -->

#### Participación relativa en la presentación de proyectos de ley por regiones.

``` r
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
# Reading layer `SudamÃ©rica' from data source 
#   `C:\OTROS\shapefile\sudamerica\SudamÃ©rica.shp' using driver `ESRI Shapefile'
# Simple feature collection with 15 features and 1 field
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -109.4461 ymin: -58.49861 xmax: -26.24139 ymax: 12.59028
# Geodetic CRS:  WGS 84
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
```

![](README-unnamed-chunk-8-1.png)<!-- -->

#### Proyectos de ley publicados o en autografa por bancada.

``` r
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
```

![](README-unnamed-chunk-9-1.png)<!-- -->

#### Participación en proyectos de ley que han sido publicados o en autografa.

``` r
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
# Joining, by = "Partido"
```

![](README-unnamed-chunk-10-1.png)<!-- -->

# Participación de congresistas en leyes publicadas en el peruano o en autografa.

``` r
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
```

![](README-unnamed-chunk-11-1.png)<!-- -->

## Actualizaciones.

La primera versión se publicó el día **11/12/2021**, pero en los
siguientes días se estará actualizando la información hasta el 15 de
diciembre del 2021, día que entrará en receso el congreso.

Para la extracción, limpieza y gráficos del repositorio se usa el
software R en su totalidad. Si tiene alguna sugerencia o comentario
puede enviarnos un correo a: **<pe.cesar.huamani.n@uni.pe>** o
**<cesar.huamani@datametria.com>**

Muchas gracias.
