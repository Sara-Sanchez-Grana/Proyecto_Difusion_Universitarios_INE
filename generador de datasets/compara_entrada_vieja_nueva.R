# datos TEST ------------------------------------------------------------------

set.seed(1234)
registro <- data.frame(variable = seq.int(10),
                 dos = sample(1:100, 10),
                 tres = sample(month.name, 10, replace = T),
                 cuatro = rnorm(10, 5, 2))

# df con una nueva linea
nl <- c(variable = 11,
                  dos = sample(1:100, 1),
                  tres = sample(month.name, 1, replace = T),
                  cuatro = rnorm(1, 5, 2))
df_newline <- rbind(registro, nl)

# df con una linea menos
df_removed_line <- registro[-6, ]

# df con una linea sustituida
df_sustituida <- registro
df_sustituida[3, ] <- nl

# df con una variable original modificada
df_var_modif <- registro
df_var_modif[4, "tres"] <- "lunes"

# df con fila repetida y otra fila con el nombre de la variable repetido
linea_repe_1 <- registro[4, ]
df_repeticiones <- rbind(registro[1:4, ], linea_repe_1, registro[5:10, ])
df_repeticiones[7, 1] <- registro[3, 1]

# sustitucion() ---------------------------------------------------------------

# detecta si se ha sustituido una variable vieja por una nueva --> OK
sustitucion <- function(df_nuevo){
  which(!df_nuevo$variable %in% registro$variable)
}

# eliminada() -----------------------------------------------------------------

# detecta si se ha eliminado una variable --> OK
eliminada <- function(df_nuevo){
  which(!registro$variable %in% df_nuevo$variable)
}

# adicional() -----------------------------------------------------------------

# detecta si se ha anyadido una variable --> OK
adicional <- function(df_nuevo){
  which(!df_nuevo$variable %in% registro$variable)
}

# repetidas() -----------------------------------------------------------------

# devuelve filas repetidas y variables diferentes con el mismo nombre 
repetidas <- function(df_nuevo){
  repe <- df_nuevo$variable[duplicated(df_nuevo$variable)]
  n <- 
    unlist(
      lapply(
        repe, 
        function(x) which(df_nuevo$variable == x)
        )
      )
if (length(n) >= 2){
  combi <- t(combn(n, 2))
  z <- 
    apply(
      combi, 
      1, 
      function(x){
        all(
          df_nuevo[x[1], ] == df_nuevo[x[2], ]
        )
      } 
    )
  i_b <- unique(combi[z, 2])
  #indices de variables diferentes pero con el nombre repetido
  i_c <- setdiff(n, unique(combi[z, 1:2]))
} else {
 i_b <- i_c <- NA 
}
  salida <- list(repetidas = i_b, nombre_repetido = i_c)
  return(salida)
  }
        
  
   
# modificadas() ---------------------------------------------------------------

# el input sera cada identificador del df_nuevo (df_nuevo$variable)
# esta funcion detecta una modificacion de un valor de una variable ya existente

modificadas <- function(input_variable){
  # encuentra fila original
  i_nuevo <- which(df_nuevo$variable == input_variable)
  # encuentra fila homologa en registro
  i_viejo <- which(registro$variable == input_variable)
  cambio <- if(length(i_viejo)){
    any(registro[i_viejo, ] != df_nuevo[i_nuevo, ])
  }
  ifelse(cambio, i_nuevo, NULL)
  return(index_nuevo)
}

# detecta las variables que permanecen
preexistente <- function(df_nuevo){
  which(df_nuevo$variable %in% registro$variable)
}

# detectar las variables preexistentes que han sufrido
# alguna modificacion en sus metadata

modificadas <- function(df_nuevo) {
  vapply(df_nuevo$variable[preexistente(df_nuevo)], modificacion, numeric(1))
}

# eliminar las modificadas
# metadata <- metadata[metadata$variable != entrada$variable[modificadas], ]
# habra que regenerar las modificadas (FUNCION GENERICA)
# XXXXX <- lapply(metadata[modificadas, "X"], metadatos)

