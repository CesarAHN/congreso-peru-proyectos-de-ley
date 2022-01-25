
# BIENVENIDOS!!!

# ¿CÓMO ESTÁN TRABAJANDO LOS CONGRESISTAS DEL PERÚ - PROYECTOS DE LEY?

Actualizado a **2022-01-24**.

Se crea este repositorio con el fin de poder conocer cómo están
trabajando los congresistas de la república del Perú, específicamente en
la presentación de proyectos de ley y en la concreción final de
convertirse en ley.

Se muestra información del número de proyectos de ley y leyes por
bancadas, regiones y por congresistas.

Esta información se extrajo desde el portal del congreso de la república
—&gt; <https://www.congreso.gob.pe/>

Los códigos lo puede encontrar en el repositorio con el nombre de
`script_congreso_pl.R` y el data frame que compila toda la información
es `df`.

**IMPORTANTE:**

Los proyectos de ley son presentados por un autor principal, pero a este
se pueden unir más congresistas que apoyan el proyecto a los cuales se
les llama “coautores”. En los gráficos siguientes cuando se menciona la
palabra “involucrados” se hace referencia a los congresistas que son
autores o coautores en un determinado proyecto de ley, cuando se indica
la palabra “autores principales” se hace referencia al congresista que
presentó el proyecto de ley lo que implica que él generó el documento
que contiene la exposición de motivos.

Se aclara esta situación, ya que los congresistas que son autores
principales son los que en realidad juntamente con sus asesores crean el
proyecto de ley, por otra parte, los congresistas que son coautores solo
apoyan en la presentación.

## ¿CÓMO ESTÁ TRABAJANDO EL CONGRESO?

### PROYECTOS DE LEY.

#### Bancadas involucradas en la presentación de proyectos de ley.

![](README-unnamed-chunk-3-1.png)<!-- -->

#### Participación relativa de bancadas en la presentación de proyectos de ley.

![](README-unnamed-chunk-4-1.png)<!-- -->

#### Congresistas con más proyectos de ley presentados.

![](README-unnamed-chunk-5-1.png)<!-- -->

#### Congresistas como autores principales en la presentación de proyectos de ley.

![](README-unnamed-chunk-6-1.png)<!-- -->

#### Congresistas con menos proyectos de ley presentados.

Todos los congresistas al menos han estado involucrados en la
presentación de un proyecto de ley.

![](README-unnamed-chunk-8-1.png)<!-- -->

#### Congresistas con menos proyectos de ley presentados como autores principales.

Solo el congresista:

    #                   Autores          Partido
    # 1 BERMEJO ROJAS GUILLERMO PERU DEMOCRATICO

No ha presentado proyectos de ley como autor principal.

------------------------------------------------------------------------

![](README-unnamed-chunk-10-1.png)<!-- -->

#### Proyectos de ley presentados por regiones.

    # Reading layer `SudamÃ©rica' from data source 
    #   `C:\OTROS\shapefile\sudamerica\SudamÃ©rica.shp' using driver `ESRI Shapefile'
    # Simple feature collection with 15 features and 1 field
    # Geometry type: MULTIPOLYGON
    # Dimension:     XY
    # Bounding box:  xmin: -109.4461 ymin: -58.49861 xmax: -26.24139 ymax: 12.59028
    # Geodetic CRS:  WGS 84

![](README-unnamed-chunk-11-1.png)<!-- -->

#### Participación relativa en la presentación de proyectos de ley por regiones.

    # Reading layer `SudamÃ©rica' from data source 
    #   `C:\OTROS\shapefile\sudamerica\SudamÃ©rica.shp' using driver `ESRI Shapefile'
    # Simple feature collection with 15 features and 1 field
    # Geometry type: MULTIPOLYGON
    # Dimension:     XY
    # Bounding box:  xmin: -109.4461 ymin: -58.49861 xmax: -26.24139 ymax: 12.59028
    # Geodetic CRS:  WGS 84

![](README-unnamed-chunk-12-1.png)<!-- -->

#### Proyectos de ley publicados o en autografa por bancada.

![](README-unnamed-chunk-13-1.png)<!-- -->

#### Participación en proyectos de ley que han sido publicados o en autografa.

![](README-unnamed-chunk-14-1.png)<!-- -->

#### Congresistas involucrados en leyes publicadas en el peruano o en autografa.

![](README-unnamed-chunk-15-1.png)<!-- -->

#### Congresistas como autores principales en leyes publicadas en el peruano o en autógrafa.

![](README-unnamed-chunk-16-1.png)<!-- -->

### Palabras más usadas en los títulos de los proyectos de ley.

![](README-unnamed-chunk-18-1.png)<!-- -->

### Palabras que se relacionan más en los proyectos de ley.

![](README-unnamed-chunk-19-1.png)<!-- -->

### Palabras más usadas en la presentación de proyectos de ley del top 4 congresistas con más proyectos de ley presentados.

![](README-unnamed-chunk-20-1.png)<!-- -->

### Palabras que más se relacionan en la presentación de proyectos de ley del top 4 congresistas con más proyectos de ley presentados.

![](README-unnamed-chunk-21-1.png)<!-- -->

## Actualizaciones.

La primera versión se publicó el día **11/12/2021**, pero en los
siguientes días se ha venido actualizando la información, en un
principio se pensó hasta el 15 de diciembre, pero el congreso amplió su
legislatura por segunda vez y estará sesionando hasta el 02 de febrero
de 2022, día que entrará en receso el congreso.

Para la extracción, limpieza y gráficos del repositorio se usa el
software R en su totalidad. Si tiene alguna sugerencia o comentario
puede enviarnos un correo a: **<pe.cesar.huamani.n@uni.pe>** o
**<cesar.huamani@datametria.com>**
