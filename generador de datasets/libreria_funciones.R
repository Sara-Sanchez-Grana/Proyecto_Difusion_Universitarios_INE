
# Install and load the necessary packages-------------------
install_pack <- function(pkg){
  aux <- function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, aux)
}
# Resolve conflicts between packages------------------------
masked_functions <- function(...){
  aux <- conflicted::conflict_scout(c("dplyr", "plyr", "MASS", ...))
  for (i in 1:length(aux)){
    conflicted::conflict_prefer(names(aux)[i], "dplyr")
  }
}


if (!"RDCOMClient" %in% installed.packages()){
  install.packages("http://www.omegahat.net/RDCOMClient/RDCOMClient_0.93-0.zip", repos = NULL)
}
packs <- c("tidyverse", "httr", "jsonlite")
install_pack(packs)
library(RDCOMClient)

# generador_url() ---------------------------------------------------
# Funcion para generar url requests
generador_url <- function(idioma = "ES",
                          funcion = NULL,
                          input = NULL,
                          parametros = NULL){
  funciones <<- c("OPERACIONES_DISPONIBLES", "OPERACION", "VARIABLES",
                  "VARIABLES_OPERACION", "VALORES_VARIABLE",
                  "VALORES_VARIABLEOPERACION", "TABLAS_OPERACION",
                  "GRUPOS_TABLA", "VALORES_GRUPOSTABLA",
                  "SERIE", "SERIES_OPERACION", "VALORES_SERIE",
                  "DATOS_METADATAOPERACION", "DATOS_TABLA")

  base <- "https://servicios.ine.es/wstempus/js/"

  idioma <- if (idioma %in% c("EN", "ES")){
    paste0(idioma, "/")
  } else {
    stop('El argumento idioma introducido es distinto de "ES" o "EN"')
  }

  funcion <- if(is.null(funcion)){
    stop("El argumento funcion es obligatorio")
  } else if (funcion %in% funciones){
    paste0(funcion, "/")
  } else {
    stop("El argumento funcion introducido no corresponde con ninguna funcion aceptada")
  }

  # input puede ser un vector de inputs. Un único elemento también es un vector
  input <- if(!is.null(input)){
    capture.output(cat(input, sep = "/"))
  } else {NULL}

  parametros <- if (!is.null(parametros)) {
    parametros[1] <- paste0("?", parametros[1])
    capture.output(cat(parametros, sep = "&"))
  } else {NULL}

  url <- paste0(base, idioma, funcion, input, parametros)
  url
}
# replace_null()  ---------------------------------------------
# funcion para cambiar elementos NULL por string vacia en listas anidadas

replace_null <- function(x, replacement = "") {
  lapply(x, function(x) {
    if (is.list(x)){
      replace_null(x)
    } else {
      if (is.null(x)) replacement else(x)
    }
  }
  )
}

# replace_empty() ----------------------------------------------
replace_empty <- function(x, replacement = "") {
  rapply(x,
         function(x) if(length(x) == 0) replacement else x,
         classes = "logical",
         how = "replace")

}

replace_NA <- function(x, replacement = NULL) {
  rapply(x,
         function(x) if(is.na(x)) replacement else x,
         classes = "logical",
         how = "replace")

}

# fechas()--------------------------------------------------------------

# Las fechas en formato date-time expresan el número de segundos que han transcurrido
# desde el 01-01-1970. El INE proporciona este dato en milisegundos, por lo que será
# necesario transformarla

fechas <- function(fecha_ine, formato ="%d-%m-%Y"){
  # de milisegundos a segundos
  temp <- fecha_ine / 1000
  # de número a formato date-time POSIX in Central European Time (zona horaria)
  fecha_hora <- as_datetime(temp, tz = "CET")
  fecha <- format(fecha_hora, formato)
  salida <- list(completa = fecha_hora, fecha = fecha)
  return(salida)
}

# tempus_3() y pc_axis() ---------------------------------------
# Funciones para extraer tablas de las urls (path y t)
tempus_3 <- function(string){
  if (!length(string)) stop("string input is empty")
  aux <- function(x){
    if (!grepl("\\D", x)) x
    else if (grepl("\\?t=", x)){
      tmp <- strsplit(x, split = "t=", fixed = TRUE)[[1]][2]
      tmp <- strsplit(tmp, split = "&", fixed = TRUE)[[1]][1]
      gsub("&file=", "", tmp)
    }
    else NA_character_
  }
  unlist(lapply(string, aux))
}

pc_axis <- function(string){
  if (!length(string)) stop("string input is empty")
  aux <- function(x){
    if (grepl("path=", x) & grepl("\\.px", x)) {
      tmp <- strsplit(x, split = "path=/", fixed = TRUE)[[1]][2]
      tmp <- strsplit(tmp, split = "&L=0", fixed = TRUE)[[1]][1]
      gsub("&file=", "", tmp)
    }
    else if(!grepl("path=", x) & grepl("\\.px", x)) x
    else NA_character_
  }
  unlist(lapply(string, aux))
}

# variables_INE() -------------------------------------------------------------

variables_INE <- function(){
  url <- generador_url(funcion = "VARIABLES")
  json <- content(GET(url), encoding = "UTF-8")
  id <- vapply(json, function(x) x$Id, FUN.VALUE = numeric(1))
  nom <- vapply(json, function(x) x$Nombre, FUN.VALUE = character(1))
  cod <- vapply(json, function(x) x$Codigo, FUN.VALUE = character(1))
  vars <- data.frame(Id = id, Nombre = nom, Codigo = cod)
  vars[vars == ""] <- NA
  return(vars)
}

# metadatos_geogr() ------------------------------------------------------
# Genera la url request y los metadatos de una variables de nuestros datasets

metadatos_geogr <- function(var,
                            id = NULL,
                            tabla,
                            und_geo){
  if (und_geo == "ccaa") {
    cod <- 70
    nombres <- ccaa_nombres
    ids <- ccaa_id
  } else if (und_geo == "prov") {
    cod <- 115
    nombres <- prov_nombres
    ids <- prov_id
  }

  parametros <- c("det=2", "tip=AM")
  url <- generador_url(funcion = "SERIE",
                       input = id,
                       parametros = parametros)
  json <- content(GET(url), encoding = "UTF-8")
  json <- replace_null(json, NA)

  # Extraer los metadatos de interes de cada serie id
  ioe <- paste0("IOE", json$Operacion$Cod_IOE)
  ioe_nombre <- json$Operacion$Nombre
  per <- json$Periodicidad[["Nombre"]]
  publ_dt <- json$Publicacion$PubFechaAct$Fecha
  ref_dt <- json$Publicacion$PubFechaAct$Anyo
  # Extraer los metadatos de los combos (variables de cruce)
  var_id <- lapply(json$MetaData, function(x) x$Variable$Id)
  val_id <- lapply(json$MetaData, function(x) x$Id)
  var_nombre <- lapply(json$MetaData, function(x) x$Variable$Nombre)
  val_nombre <- lapply(json$MetaData, function(x) x$Nombre)
  combos <- mapply(list, list(var_nombre = var_nombre,
                              var_id = var_id,
                              val_nombre = val_nombre,
                              val_id = val_id)
  )

  # Variable geogr�fica de cruce: Incluir todos los valores
  geogr <- which(unlist(combos$var_id) == cod)
  combos$val_nombre[[geogr]] <- nombres
  combos$val_id[[geogr]] <- ids

  # Convierte la lista de nombres e ids en lista de variables cruzadas
  combo_l <-
    mapply(
      function(x, y, v, w) list(Variable = x,
                                Variable_id = y,
                                Valor = v,
                                Valor_id = w),
      combos$var_nombre,
      combos$var_id,
      combos$val_nombre,
      combos$val_id)
  combo_l <- as.list(as.data.frame(combo_l))

  metadatos_lista <- list(Variable = var,
                          Tabla = tabla,
                          `Cod. IOE` = ioe,
                          `Op. Estad.` = ioe_nombre,
                          Periodicidad = per,
                          `Fecha Publ.` = publ_dt,
                          `Periodo ref.` = ref_dt,
                          Cruces = combo_l)

  return(metadatos_lista)
  # # Convierte los cruces en data.frame para poder ser pasados a
  # # encuentra_dato()
  # tmp_1 <- data.frame(metadatos_lista[-8], check.names = FALSE)
  #
  # # Todos los df de combos deben tener las mismas dimensiones
  # tmp_2 <- data.frame(metadatos_lista$Cruces)
  # tmp_2 <-cbind(tmp_2, matrix(NA, ncol = 24-dim(tmp_2)[2]))
  # names(tmp_2) <- c("Variable_1", "Var_1", "Valor_1", "Val_1",
  #                   "Variable_2", "Var_2", "Valor_2", "Val_2",
  #                   "Variable_3", "Var_3", "Valor_3", "Val_3",
  #                   "Variable_4", "Var_4", "Valor_4", "Val_4",
  #                   "Variable_5", "Var_5", "Valor_5", "Val_5",
  #                   "Variable_6", "Var_6", "Valor_6", "Val_6")
  # metadatos_df <- cbind(tmp_1, tmp_2)
  #
  # salida <- list(lista = metadatos_lista, df = metadatos_df)
  # return(salida)
}



# tabla_datos() ---------------------------------------------------------------
# Convierte los datos descargados de una tabla en un df.
# Cada linea del df es un dato con sus cruces y demas metadatos

# SE PUEDE HACER SUBSET EN LA SALIDA DE tabla_datos() con el vector de
# salida de encuentra_dato()
tabla_datos <- function (url){
  datos <- content(GET(url), encoding = "UTF-8")
  #Funciones auxiliares para extraer los id de variable y valor
  aux_varid <- function(x){
    lapply(x, function(y) y$Variable$Id)
  }
  aux_valid <- function(x){
    lapply(x, function(y) y$Id)
  }

  #crear un dataframe por cada dato a partir de sus metadatos
  tmp <- lapply(datos, function(x) x$MetaData)
  n_vars <- length(tmp[[1]])
  nombres <- paste0(rep(c("var_", "val_"), each = n_vars),
                    seq_len(n_vars)
  )
  df <- lapply(tmp,
               function(x) data.frame(var = aux_varid(x),
                                      val = aux_valid(x)
               )
  )
  # todos los df deben tener los mismo nombres para poder hacer rbind
  df  <- lapply(df, function(x) {
    names(x) <- nombres
    x
  }
  )
  df <- data.frame(t(do.call(rbind, args = lista_df)))

  # Anyadir el dato y el resto de metadatos
  df$COD <- vapply(datos, function(x) x$COD, character(1))
  df$Anyo <- vapply(datos, function(x) x$Data[[1]]$Anyo, numeric(1))
  df$T3_Periodo <- vapply(datos, function(x) x$Data[[1]]$T3_Periodo, character(1))
  df$T3_TipoDato <- vapply(datos, function(x) x$Data[[1]]$T3_TipoDato, character(1))
  df$Fecha <- vapply(datos, function(x) x$Data[[1]]$Fecha, character(1))
  df$Valor <- vapply(datos, function(x) x$Data[[1]]$Valor, numeric(1))
  retunr(df)
}

# encuentra_dato() --------------------------------------------

# encuentra el indice (F, F,.., T, ...F) de un unico cruce
encuentra_dato <- function(datos,
                           var_1, val_1,
                           var_2, val_2,
                           var_3, val_3,
                           var_4, val_4,
                           var_5, val_5,
                           var_6, val_6,
                           var_7, val_7,
                           var_8, val_8,
                           var_9, val_9,
                           var_10, val_10) {
  tmp <- lapply(datos, function(x) x$MetaData)
  a <- list(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10,
            val_1, val_2, val_3, val_4, val_5, val_6, val_7, val_8, val_9, val_10)
  a <- lapply(a, function(x) if(is.na(x)) NULL else x)

  aux <- function(x,
                  var_1, val_1,
                  var_2 = NULL, val_2 = NULL,
                  var_3 = NULL, val_3 = NULL,
                  var_4 = NULL, val_4 = NULL,
                  var_5 = NULL, val_5 = NULL,
                  var_6 = NULL, val_6 = NULL,
                  var_7 = NULL, val_7 = NULL,
                  var_8 = NULL, val_8 = NULL,
                  var_9 = NULL, val_9 = NULL,
                  var_10 = NULL, val_10 = NULL) {
    lapply(x, function(y) {
      c(y$Id[y$Variable$Id == var_1] == val_1,
        y$Id[y$Variable$Id == var_2] == val_2,
        y$Id[y$Variable$Id == var_3] == val_3,
        y$Id[y$Variable$Id == var_4] == val_4,
        y$Id[y$Variable$Id == var_5] == val_5,
        y$Id[y$Variable$Id == var_6] == val_6,
        y$Id[y$Variable$Id == var_7] == val_7,
        y$Id[y$Variable$Id == var_8] == val_8,
        y$Id[y$Variable$Id == var_9] == val_9,
        y$Id[y$Variable$Id == var_10] == val_10
      )
    })
  }
  s <- lapply(tmp, aux,
              var_1 = a[[1]], var_2 = a[[2]],
              var_3 = a[[3]], var_4 = a[[4]],
              var_5 = a[[5]], var_6 = a[[6]],
              var_7 = a[[7]], var_8 = a[[8]],
              var_9 = a[[9]], var_10 = a[[10]],
              val_1 = a[[11]], val_2 = a[[12]],
              val_3 = a[[13]], val_4 = a[[14]],
              val_5 = a[[15]], val_6 = a[[16]],
              val_7 = a[[17]], val_8 = a[[18]],
              val_9 = a[[19]], val_10 = a[[20]]
  )
  s <- replace_empty(x = s, replacement = FALSE)
  s <- lapply(s, function(x) all(unlist(x)))
  return(unlist(s))
}


# indices_datos_tabla() -------------------------------------------------------
# necesitamos argumentos de la funcion:
# - id de tabla (a la que pertenece datos): id
# - un json de datos: datos
# - un json de metadatos de variables: metadatos

# utiliza internamente la funcion encuentra dato

indices_datos_tabla <- function(id, datos, metadatos){

  # lista con metadatos de los datos descargados
  # tmp <- lapply(datos, function(x) x$MetaData)

  # localizar variables pertenecientes a tabla
  i <- vapply(metadatos,
              function(x) x$Tabla == id,
              logical(1))

  # Genera lista de variables de la tabla con sus cruces
  x <- lapply(metadatos[i], function(x) x$Cruces)

  # elimina elementos Variable y valor
  r <- c("Variable", "Valor")
  x <-lapply(x,
             function(x) lapply(x,
                                function(y) y[names(y) %in% r == FALSE]
             )
  )

  # por cada variable de metadatos genera un df en el que cada fila
  # es un cruce que debe ser buscado en df
  x <- lapply(x, function(x) data.frame(x))

  a <-
    lapply(x, function(y) {
      apply(y, 1, function(z) {
        # # asignar cada fila de x a var_1, val_1, var_2, val_2,...
        asignar(z)
        i <- encuentra_dato(datos,
                            var_1 = var_1, var_2 = var_2,
                            var_3 = var_3, var_4 = var_4,
                            var_5 = var_5, var_6 = var_6,
                            var_7 = var_7, var_8 = var_8,
                            var_9 = var_9, var_10 = var_10,
                            val_1 = val_1, val_2 = val_2,
                            val_3 = val_3, val_4 = val_4,
                            val_5 = val_5, val_6 = val_6,
                            val_7 = val_7, val_8 = val_8,
                            val_9 = val_9, val_10 = val_10)
        rm(var_1, var_2, var_3, var_4, var_5, var_6,var_7, var_8, var_9, var_10,
           val_1, val_2, val_3, val_4, val_5, val_6,val_7, val_8, val_9, val_10)
        return(i)
      }
      )
    }
    )


  # encuentra la columna en x que contiene los valores que cambian (aragon, madrid,...)
  i <- lapply(x,
              function(y) apply(y,
                                2,
                                function(z) sum(z %in% z[duplicated(z)])
              )
  )
  i <- lapply(i, function(x) which(x == min(x)))

  # valores de la variable que sirve de patron (ccaa, prov, ...)
  e <- mapply(function(a, b) a[b], x, i)
  names(e) <- names(i)

  # indices de los datos descargados que coinciden con metadatos
  w <- lapply(a, function(x) apply(x, 2, which))

  # combina indices de datos tabla y valores de (ccaa, prov,...)
  ind <- function(a, b){z <- list(indices = b, id = a); return(z)}
  s <- mapply(ind,e, w, SIMPLIFY = FALSE)

  # la salida es una lista los nombres de los elementos son las
  # variables. Cada elemento contiene los indices y los valores(id)
  return(s)
}

# cruces()---------------------------------------------------------------------
# Genera los ids de las variables que se cruzan en una tabla (combo)
cruces <- function(tabla){
  vars <- variables_INE()
  funcion_1 = "GRUPOS_TABLA"
  funcion_2 <- "VALORES_GRUPOSTABLA"
  input_1 <- as.numeric(tabla)
  url_1 <- generador_url(funcion = funcion_1, input = input_1)
  json_1 <- content(GET(url_1), encoding = "UTF-8")
  # obtenemos los nombres para las variables de cruce
  ids <- vapply(json_1, function(x) x$Nombre, character(1))
  # para variable obtenemos el id
  # a veces algun id no se encuentra en la tabla de vars
  id <- vapply(ids, function(x) vars$Id[vars$Nombre == x], numeric(1))
  return(id)
}

# values_vars() ------------------------------------------------------
# generar los valores que toma una variable <id>
values_vars <- function(id){
  url <- generador_url(funcion = "VALORES_VARIABLE",input = id)
  json <- RJSONIO::fromJSON(url, encoding = "UTF-8")
  #pasar a base
  values <- map_dfr(json, `[`, c("Fk_Variable", "Id", "Nombre", "Codigo")) %>%
    na_if("NULL") %>%
    na_if("") %>%
    arrange(Codigo)
  return(values)
}

# Comunidades y provincias incluye CEUTA Y MELILLA
ccaa_id <- unname(unlist(values_vars(70)[2:20, "Id"]))
ccaa_nombres <- unname(unlist(values_vars(70)[2:20, "Nombre"]))
prov_id <- unname(unlist(values_vars(115)[2:52, "Id"]))
prov_nombres <- unname(unlist(values_vars(115)[2:52, "Nombre"]))
# rango_df()-------------------------------------------------------------------
# Determina rango de celdas RDCOMClient
# Los rangos empiezan en A1. Se podr�a modificar

rango_df <- function(wb, sheet, ncols){
  sheet <- wb$Sheets(sheet)
  last <- numeric(length = ncols)
  for (i in seq_len(ncols)){
    last[i] <- sheet$Cells(1, i)$End(-4121)[["Row"]]
  }
  last <- last[last < sheet[["Rows"]][["Count"]]]
  blank <- logical(length = length(last))
  for (i in seq_along(last)){
    checks <- logical(length = ncols)
    for (j in seq_len(ncols)){
      checks[j] <- is.null(sheet$Cells(last[i]+1, j)[["Value"]])
    }
    blank[i] <- all(checks)
  }
  filas <- min(last[blank])
  rng <- sheet$Range(sheet$Cells(1,1), sheet$Cells(filas, ncols))
  salida <- list(filas = filas, rng = rng)
  return(salida)
}

# leer_df()--------------------------------------------------------------------
# Leer valores de un rango de celdas excel RDCOMClient
# el rango empieza en A1

leer_df <- function(wb, sheet, ncols){
  rng <- rango_df(wb, sheet, ncols)$rng
  valores <- rng[["Value"]]
  valores <- replace_null(valores, NA)
  lst <- lapply(valores, unlist)
  matrx <- do.call(cbind, lst)
  df <- data.frame(matrx)
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  return(df)
}

# escribir_df()----------------------------------------------------------------
# Escribir un df en una hoja excel empezando en A1

escribir_df <- function(wb, sheet, objeto){
  cabecera <- data.frame(t(names(objeto)))
  names(cabecera) <- names(objeto)
  datos <- rbind(cabecera, objeto)
  sh <- wb$Sheets(sheet)
  nc <- ncol(datos)
  nr <- nrow(datos)
  rng <- sh$Range(sh$Cells(1, 1), sh$Cells(nr, nc))
  rng[["Value"]] <- asCOMArray(datos)
}

# hojas()----------------------------------------------------------------------
# obtener el nombre de las celdas de un libro

hojas <- function(wb){
  numero_sheets <- wb$Sheets()[["Count"]]
  sheets <- character(length = numero_sheets)
  for (i in seq_len(numero_sheets)){
    sheets[i] <- wb$Sheets(i)[["Name"]]
  }
  return(sheets)
}

# estilos() -------------------------------------------------------------------
# Obtener los estilos de un workbook y el numero de un estilo concreto
estilos <- function(wb, estilo = NULL){
  tabla <- data.frame(id=1:60,
                      estilo=sapply(1:60,
                                    function(x) try(wb[["Styles"]]$Item(x)[["Name"]], silent = TRUE)
                      )
  )
  if (!is.null(estilo)){
    num <- tabla$id[tabla$estilo == estilo]
  } else num <- NA
  salida <- list(tabla = tabla, num = num)
  return(salida)
}


# avisos() --------------------------------------------------------------------

# fallo es un vector de indices del df correspondiente. Se�alan fila con fallo
# ok es la condicion que se�ala la NO existencia de fallo. TRUE or FALSE
# msj_fallo es mensaje de fallo
avisos <- function(fallo, ok, mnsj_fallo, mnsj_ok){
  tmp <- Sys.time()
  warning <- data.frame(matrix(NA, ncol = 5))
  warning <- warning[rep(1, times=length(fallo)), ]
  names(warning) <- c("Fecha", "Hora", "Aviso", "Set", "Variable")

  if (!ok) {
    for (i in fallo){
      nuevo_aviso = c(format(tmp, "%d-%m-%Y"),
                      format(tmp, "%X"),
                      mnsj_fallo,
                      metadata$set[i],
                      metadata$variable_aux[i])
      warning[which(fallo == i), ] <- nuevo_aviso
    }
  } else if(ok){
    nuevo_aviso = c(format(tmp, "%d-%m-%Y"),
                    format(tmp, "%r"),
                    mnsj_ok,
                    NA,
                    NA)
    warning[1, ] <- nuevo_aviso
  }
  return(warning)
}


# asignar() -------------------------------------------------------------------
# asignar cada fila de x a var_1, val_1, var_2, val_2,...

asignar <- function(x){
  var_1 <<- x[1]
  val_1 <<- x[2]
  var_2 <<- x[3]
  val_2 <<- x[4]
  var_3 <<- x[5]
  val_3 <<- x[6]
  var_4 <<- x[7]
  val_4 <<- x[8]
  var_5 <<- x[9]
  val_5 <<- x[10]
  var_6 <<- x[11]
  val_6 <<- x[12]
  var_7 <<- x[13]
  val_7 <<- x[14]
  var_8 <<- x[15]
  val_8 <<-x[16]
  var_9 <<- x[17]
  val_9 <<-x[18]
  var_10 <<- x[19]
  val_10 <<-x[20]
}
