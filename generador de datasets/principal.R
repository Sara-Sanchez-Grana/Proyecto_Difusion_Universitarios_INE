
# asignar working directorio: variable del sistema creada en iniciar.bat
wd_sysvar <- Sys.getenv("directorio")

# lo señalamos en la ventanta WINDOWS de ejecución
print(paste0("Variable de SISTEMA directorio: ",wd_sysvar))

# existen metodos alternativos a la variable de sistema en versiones anteriores
# de este script


# Inicializar Funciones, librerias y directorios ------------------------------

source("libreria_funciones.R")
source("compara_entrada_vieja_nueva.R")
rm(packs, install_pack)

# Inicializar el workbook -----------------------------------------------------

# asignar el nombre del archivo del libro excel .xlsm
macro <- "ficha.xlsm"

# generar la ruta del archivo excel y la visualizmos en ventana WINDOWS
fn_sysvar <- paste(wd_sysvar, macro, sep = "", collapse = "")
fn_sysvar <- gsub("/", "\\\\", fn_sysvar)
print(paste0("ruta por variable del SISTEMA: ",fn_sysvar))

# ejecutar excel, que sea visible y no muestre las alertas
app <- COMCreate("Excel.Application",existing = TRUE)
app[["Visible"]] <- TRUE
app[["DisplayAlerts"]] <- FALSE

# Abrir el archivo .xlsm
wb <- app$Workbooks()$Open(fn_sysvar)
rm(macro, fn_sysvar)

# Crear las sheet necesarias
sheets <- hojas(wb)
nuevas <- c("metadata", "avisos", "peticiones", "registro")
nuevas <- nuevas[!nuevas %in% sheets]
for (i in nuevas){
  nueva_hoja <- wb$Sheets()$Add()[["Name"]]
  try(wb$Sheets(nueva_hoja)[["Name"]] <- paste(i), silent = TRUE)
}
rm(nuevas, i)

# el estado de la memoria de entrada es "muy oculto"
# el valor de enumeracion de xlVeryHidden es 2
sh <- wb$Sheets("registro")
sh[["Visible"]] <- 2

# cargar los estilos de celda personalizados del workbook
# estos estilos se crearon y guardaron en la hoja excel
id_header <- estilos(wb = wb, estilo = "header")$num
id_cell <- estilos(wb, "cell")$num
estilo_header <- wb[["Styles"]]$Item(id_header)
estilo_cell <- wb[["Styles"]]$Item(id_cell)

# Lectura y creacion de dataframes --------------------------------------------

# metadata se genera entera en cada ejecucion ??? DE MOMENTO...
entrada <- metadata <- leer_df(wb = wb, sheet = "entrada", ncols = 7)

# registro es la copia del anterior metadata para poder encontrar las novedades
# si no existe lo inicializamos
aux <- sh$Cells(1,1)[["Value"]]
registro <- if (!is.null(aux)){
  leer_df(wb, "registro", ncols = 7)
} else {
  data.frame()
}

# avisos lee los avisos anteriores y añade los nuevos
aux <- wb$Sheets("avisos")$Cells(1,1)[["Value"]]
if ("avisos" %in% sheets && !is.null(aux)){
  avisos_previo <- leer_df(wb, "avisos", 5)
  sin_avisos <- FALSE
} else sin_avisos <- TRUE
avisos_df<- data.frame(Fecha = NA,
                       Hora = NA,
                       Aviso = NA,
                       Set = NA,
                       Variable = NA)
if (sin_avisos) avisos_previo <- avisos_df

# peticiones
long <- rep(NA, nrow(metadata))
peticiones <- data.frame(variable = long,
                         variable_aux = long,
                         set = long,
                         url_serie = long,
                         url_tabla = long)
rm(sheets, sin_avisos, long)

# Manipulación del contenido del excel ----------------------------------------

# Eliminar espacios al principio y al final de cada celda (TRIM)
metadata <- as.data.frame(apply(metadata,
                                2,
                                function(x) gsub("^\\s+|\\s+$", "", x)
)
)
# crear columna formato
metadata$formato <- NA

if (nrow(registro) > 0) {
  i <- repetidas(metadata)
  metadata <- ifelse(is.na(i$repetidas), metadata, metadata[-i$repetidas, ])
  fallo <- i$nombre_repetido
} else {
  fallo <- NA
}

repes <- "El nombre de esta variable está repetido"
no_repes <- "No hay nombres de variables repetidos"
ok <- length(fallo) == 1 && is.na(fallo)
av_repes <- avisos(fallo = fallo,
                   ok = length(fallo),
                   mnsj_fallo = repes,
                   mnsj_ok = no_repes)
# Generar avisos por nombres de variable repetidos
# despues de haber incluido los combos y modificar la funcion
# para que solo compare los pares variable-valor de los cruces



# condiciones para determinar formatos y posibles anomalías
t3 <- with(metadata, (grepl("^\\d+$", tabla) |
                        grepl("?t=", tabla)) &
             grepl("^[A-Za-z]+\\d+$", t3_serie)
)
pc <- with(metadata, grepl("path=", tabla) |
             grepl("\\.px", tabla) &
             grepl("-{1}", t3_serie)
)
anomalia <- with(metadata, grepl("^-", tabla) | # posiblemente para quitar
                   grepl("^$", tabla) |
                   grepl("^$", t3_serie) |
                   is.na(tabla) |
                   is.na(t3_serie) |
                   grepl("^[A-Za-z]+\\d+$", tabla) |
                   grepl("[A-Za-z]$", t3_serie)
)

# Detecta si una fila es de dos categorias simultaneamente
check_1 <- t3 & pc
check_2 <- t3 & anomalia
check_3 <- pc & anomalia
check_4 <- check_1 | check_2 | check_3

# Detecta si todas las filas se han clasificado en alguna categoría
check_5 <- t3 | pc | anomalia

# Resumen de duplicidad o ausencia
check_6 <- sum(check_5) == nrow(metadata)
check_7 <- sum(check_4) == 0
ok <- check_6 && check_7

fallo <- which(check_4 | !check_5)

# crear una tabla de avisos
formato_mal <- "Se han producido errores en la generación de la columna formato."
formato_bien <- "Se han asignado correctamente los formatos."
av_formato <- avisos(fallo,
                     ok,
                     mnsj_fallo = formato_mal,
                     mnsj_ok = formato_bien )

avisos_df <- rbind(avisos_previo, av_repes, av_formato)
# eliminar filas de avisos que sean todo NA
row_NA <- unlist(apply(is.na.data.frame(avisos_df), 1, all))
avisos_df <- avisos_df[!row_NA, ]

# Aplicar funciones de <formato> y formulas de conversión de tablas
metadata <- within(metadata, {
  formato[t3] <- "Tempus 3"
  formato[pc] <- "PC-Axis"
  formato[anomalia] <- "ANOMALIA"
  tabla[t3] <- tempus_3(tabla)[t3]
  tabla[pc] <- pc_axis(tabla)[pc]
})

#liberar memoria
rm(check_1, check_2, check_3, check_4, check_5, check_6, check_7,
   ok, tmp, fallo, nuevo_aviso, avisos_previo, row_NA,
   t3, pc, anomalia, tempus_3, pc_axis,
   av_repes, av_formato)

# peticiones
vars <- variables_INE() #BORRAR???

geogr <- with(metadata, und_anlsis %in% c("ccaa", "prov"))

# INCORPORAR columna
peticiones[, 1:3] <- metadata[, c(1, 5, 2)]

# genera una lista de metadatos estilo json
cruz <- try(mapply(metadatos_geogr,
                   metadata$variable,
                   metadata$t3_serie,
                   metadata$tabla,
                   metadata$und_anlsis))
cruz <- apply(cruz, 2, function(x) {
  a <-list(x$Variable,
           x$Tabla,
           x$`Cod. IOE`,
           x$`Op. Estad.`,
           x$Periodicidad,
           x$`Fecha Publ.`,
           x$`Periodo ref.`,
           x$Cruces)
  names(a) <- rownames(cruz)
  a
})


# generar metadatos
var_split <- split(x = metadata$variable, metadata$set)

metas <- lapply(var_split, function(x) cruz[x])
metas <- lapply(metas, function(x) jsonlite::toJSON(x,))
mapply(function(a, b) write_json(x = a, path =  b),
       metas,
       paste0(names(metas), ".json")
)

# el parametro "nult=1" debera sustituirse en cada variable según corresponda
# habrá que rellenar ese dato a mano en el excel con columnas adicionales

# el vector de indices <geogr> habra que sustituirlo cuando incorporemos
# el resto de patrones, como sexo
peticiones$url_tabla[geogr] <- vapply(metadata$tabla,
                                      function (x) generador_url(funcion = "DATOS_TABLA",
                                                                 input = x,
                                                                 parametros = c("tip=AM","nult=1")
                                      ),
                                      character(1)
)[geogr]



# Aqui terminaria este script. hay que anyadir toda la parte de RDCOM
# para gestionar los excel y la grabacion de archivos de metadatos
# en formato JSON o como corresponda

# toda la parte de generacion de datos se pasara a otro script que ira
# asociado a otro boton

petic_split <- split(peticiones, metadata$tabla)

# hacer una seleccion en cruz de las variables de la tabla only: cruz_split???
aux <- function(x) {
  i <- vapply(cruz, function(y) y$Tabla == x, logical(1))
  cruz[i]
}
cruz_split <- lapply(unique(metadata$tabla), aux)
names(cruz_split) <- unlist(lapply(cruz_split, function(x) unlist(lapply(x, function(y) y$Tabla))[1]))
cruz_split <- cruz_split[names(petic_split)]

aux <- function(a, b, c) {
  d <- content(GET(a$url_tabla[1]))
  # indices en <datos> de los datos de cada tabla + ids de la variable
  # patron correspondiente a cada dato
  i <- indices_datos_tabla(id = b, datos = d, metadatos = c)
  # subset del conjunto <datos>: solo los datos de la tabla correspondiente
  d <- lapply(i, function(x) data = d[x$indices])
  # de cada dato extraemos solo la sublist Data y la convertimos en un df
  d <- lapply(d,
              function(x) lapply(x,
                                 function(y) data.frame(y$Data[[1]])
              )
  )
  # agrupamos para cada tabla sus datos en un df, cada linea es un dato
  d <- lapply(d, function(x) do.call(rbind, x))
  # ponemos el nombre de la variable, el identificador del patron y el set en el df de datos
  d <-
    mapply(function(x, y, u, v) {
      x$variable <- y
      x$id <- u$id
      x$set <- v
      x
    },
    d,
    names(d),
    i,
    a$set,
    SIMPLIFY = FALSE)
  return(d)
}
full_datos <-
  mapply(FUN = aux,
         petic_split,
         names(petic_split),
         cruz_split,
         SIMPLIFY = FALSE)
rm(aux)
# dentro de cada tabla fusionamos todos los dfs en uno solo
full_datos <-
  lapply(full_datos,
         function(x) {
           s <- do.call(rbind, x)
           rownames(s) <- NULL
           s
         }
  )
# fusionamos los dfs de datos de todas las tablas
full_datos <- do.call(rbind, full_datos)
rownames(full_datos) <- NULL


# generar datasets
d <- split(full_datos, full_datos$set)
rm(full_datos)
d <- lapply(d, function(x) split(x, x$variable))
d <- lapply(d, function(x) lapply(x, function (y) y[, c("Valor", "id")]))

# con reduce se pierden los nombres de las variables en las columnas "VAlor"
nombres <- lapply(d, names)
aux <- function(x, y) merge(x, y, by = "id", all = TRUE)
d <- lapply(d, function(x) Reduce(f = aux, x = x))
d <- mapply(function(x,y) {names(x)[-1] <- y; x}, d, nombres, SIMPLIFY = FALSE)

# ordenar las variables tal y como aparecen en las fichas de datos de los datasets
orden <- split(metadata[, 1], metadata[, 2])
d <- mapply(function(x, y) x[c("id", y)], d, orden, SIMPLIFY = FALSE)

# generar excels de datasets
mapply(function(a, b) writexl::write_xlsx(x = a, path =  b),
       d,
       paste0(names(d), ".xlsx"))







# escribimos las tablas

escribir_df(wb = wb, sheet = "metadata", objeto = metadata)
escribir_df(wb, "avisos", avisos_df)
escribir_df(wb, "peticiones", peticiones)

# damos formato por rangos

sh_m <- wb$Sheets("metadata")
sh_a <- wb$Sheets("avisos")
sh_p <- wb$Sheets("peticiones")

#cabeceras
rng_1 <- sh_m$Range(sh_m$Cells(1, 1),
                    sh_m$Cells(1, dim(metadata)[2]))
rng_2 <- sh_a$Range(sh_a$Cells(1, 1),
                    sh_a$Cells(1, dim(avisos_df)[2]))
rng_3 <- sh_p$Range(sh_p$Cells(1, 1),
                    sh_p$Cells(1, dim(peticiones)[2]))
rng_1[["Style"]] <- rng_2[["Style"]] <- rng_3[["Style"]] <- estilo_header

# cuerpo de tablas
rng_1 <- sh_m$Range(sh_m$Cells(2, 1),
                    sh_m$Cells(dim(metadata)[1]+1, dim(metadata)[2]))
rng_2 <- sh_a$Range(sh_a$Cells(2, 1),
                    sh_a$Cells(dim(avisos_df)[1]+1, dim(avisos_df)[2]))
rng_3 <- sh_p$Range(sh_p$Cells(2, 1),
                    sh_p$Cells(dim(peticiones)[1]+1, dim(peticiones)[2]))
rng_1[["Style"]] <- rng_2[["Style"]] <- rng_3[["Style"]] <- estilo_cell

#memoria
rm(rng_1, rng_2, rng_3)

# Anchos de columna (numero de la ultima fila)
tope_f <- sh_m[["Rows"]][["Count"]]
tope_c <- sh_m[["Columns"]][["Count"]]

# metadata
rng <- sh_m$Range(sh_m$Cells(1, 1),
                  sh_m$Cells(tope_f, dim(metadata)[2]))
rng[["ColumnWidth"]] <- 10

rng <- sh_m$Range(sh_m$Cells(1, 2),
                  sh_m$Cells(tope_f, 2))
rng[["ColumnWidth"]] <- 15

rng <- sh_m$Range(sh_m$Cells(1, 1),
                  sh_m$Cells(dim(metadata)[1] + 1, tope_c))
rng[["RowHeight"]] <- 15

# avisos
rng <- sh_a$Range(sh_a$Cells(1, 1),
                  sh_a$Cells(tope_f, dim(avisos_df)[2]))
rng[["ColumnWidth"]] <- 10

rng <- sh_a$Range(sh_a$Cells(1, 3),
                  sh_a$Cells(tope_f, 3))
rng[["ColumnWidth"]] <- 35

rng <- sh_a$Range(sh_a$Cells(1, 4),
                  sh_a$Cells(tope_f, 4))
rng[["ColumnWidth"]] <- 25

rng <- sh_a$Range(sh_a$Cells(1, 1),
                  sh_a$Cells(dim(avisos_df)[1] + 1, tope_c))
rng[["RowHeight"]] <- 15

# peticiiones
rng <- sh_p$Range(sh_p$Cells(1, 1),
                  sh_p$Cells(tope_f, dim(peticiones)[2]))
rng[["ColumnWidth"]] <- 10

rng <- sh_p$Range(sh_p$Cells(1, 1),
                  sh_p$Cells(dim(peticiones)[1] + 1, tope_c))
rng[["RowHeight"]] <- 15


# Inmovilizar paneles (metadata, entrada y peticiones)

for (i in c("metadata", "entrada", "peticiones")){
  wb$Sheets(i)$Activate()
  wb$Sheets(i)$Cells(2, 2)$Select()
  try(app[["ActiveWindow"]][["FreezePanes"]] <- FALSE, silent = TRUE)
  try(app[["ActiveWindow"]][["FreezePanes"]] <- TRUE, silent = TRUE)
}

# Cierre
wb$Save()
wb$Close(TRUE)
app$Quit()

#restaurar el sistema --------------------------------------------------------

# es necesario?
# setwd(viejo_wd)

#liberamos memoria (es necesario?)
rm(wd,
   wb,
   tmp, fallo, nuevo_aviso,
   metadata, avisos_df,
   rng,
   estilo_cell, estilo_header,
   sh_p, sh_a, sh_m)

quit(save = "no")