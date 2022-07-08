# Funcion para generar cortes economicos estilo deciles
# df (data.frame) base de datos de referencia
# factor (string) nombre de la variable que es factor de expansion
# variable.ref (string) nombre de la variable de referencia (la que se ordena en forma ascendente)
# cortes (integer) numero de cortes
# nombre (string) nombre de la nueva variable con los cortes
# prefijo (string) prefijo para la variable de corte
corte.economico <- function(df, pesos, variable.ref, cortes, nombre, prefijo = ""){
  if ( !(pesos %in% names(df)) ) stop(paste0("La variable ", pesos, " no esta en df"))
  if ( !(variable.ref %in% names(df)) ) stop(paste0("La variable ", variable.ref, " no esta en df"))
  # Estandarizamos nombres
  names(df)[match(pesos, names(df))] <- "factor"
  names(df)[match(variable.ref, names(df))] <- "var"
  # Cuantos elementos hay en la poblacion objetivo?
  num.elem <- sum(df$factor)
  num.elem.corte <- floor(num.elem / cortes)
  vec.cortes <- seq(from = num.elem.corte, to = num.elem, by = num.elem.corte)
  if( length(vec.cortes) == cortes) { vec.cortes <- vec.cortes[-cortes]}
  # Variables de referencia
  df2 <- data.frame(df) %>% 
    dplyr::arrange(., var) %>%
    dplyr::mutate(., 
                  sumacum = cumsum(factor),
                  sumacum.lag = lag(sumacum, default = 0)
    ) 
  # En que valor los cortes estan entre cumsum.lag y cumsum?
  df2[, nombre] <- NA
  indices <- NULL
  suma.acum.exacta <- NULL
  for(i in seq(vec.cortes)) {
    indices <- c(indices, which(df2$sumacum.lag < vec.cortes[i] & df2$sumacum > vec.cortes[i]))
    
    # print(indices)
    df2[df2$sumacum <= vec.cortes[i] & is.na(df2[, nombre]), nombre] <- i  
    ## Cabe la posiblidad, que suma acumulada sea exacta y no falte hacer un corte en los renglones. Para eso, guardamos el indice para quitarlo despues del vec.cortes
    if (length(which(df2$sumacum.lag < vec.cortes[i] & df2$sumacum > vec.cortes[i])) == 0) { suma.acum.exacta <- c(suma.acum.exacta, i) }
  }
  df2[, nombre][is.na(df2[, nombre])] <- cortes
  # O necesitas un corte o no necesitas un corte
  # print("vec.cortes")
  # print(vec.cortes)
  
  assertthat::assert_that(length(indices) + length(suma.acum.exacta) == cortes - 1)
  
  # POr ultimo, quito los que acumularon exacto del vector de cortes *no hacen falta!*
  if(!is.null(suma.acum.exacta)) {vec.cortes <- vec.cortes[-suma.acum.exacta]}
  # Creamos variable auxiliar con cortes
  df2$cortes <- rep(0, nrow(df2))
  df2$cortes[indices] <- vec.cortes
  # Nos concentramos en los que debemos abrir
  a <- dplyr::filter(df2, cortes > 0)
  agreg <- NULL
  for (i in seq(nrow(a))) {
    vi <- a[i, "cortes"] - a[i, "sumacum.lag"] # pasar al decil anterior
    vs <- a[i, "factor"] - vi # lo que se queda ese decil
    nuevo.renglon <- a[i, ]
    nuevo.renglon[, nombre] <- a[i, nombre] - 1 # le paso al decil ANTERIOR
    agreg <- rbind(agreg
                   , cbind(nuevo.renglon, fac2 = vi)
                   , cbind(a[i, ], fac2 = vs)
    )
  }
  agreg$factor <- agreg$fac2
  agreg <- dplyr::select(data.frame(agreg), -fac2)
  
  # Pego viejos renglones menos cortes con cortes duplicados
  res <- rbind(
    dplyr::filter(df2, cortes == 0),
    agreg) %>% 
    data.frame(.) %>%
    dplyr::select(., -sumacum, -sumacum.lag, -cortes)
  # Valido que la suma de factores del nuevo df sea igual al original
  assertthat::assert_that(sum(res$factor) == num.elem)
  # Devolvemos nombres
  names(res)[match("factor", names(res))] <- pesos
  names(res)[match("var", names(res))] <- variable.ref
  res[, nombre] <- stringr::str_pad(res[, nombre], width = nchar(cortes), side = "left", pad = "0")
  if( prefijo != "" ) res[, nombre] <- paste0(prefijo, res[, nombre])
  return(res)
}

# por grupo, eg. anio o estado
corte.economico.gpo <- function(datos, grupo, pesos, variable.ref, cortes, nombre, prefijo = "") {
  ## NA's
  nas <- datos[is.na(datos[, variable.ref]), ]
  datos <- datos[!is.na(datos[, variable.ref]), ]
  ## Deciles
  names(datos)[match(grupo, names(datos))] <- "grupo"
  nueva <- NULL
  for( g in unique(datos$grupo) ) {
    nueva <- rbind(nueva, 
                   corte.economico(df = dplyr::filter(datos, grupo == g), 
                                   variable.ref = variable.ref, 
                                   pesos = pesos, 
                                   cortes = cortes, 
                                   nombre = nombre, 
                                   prefijo = prefijo))
  }
  names(nueva)[match("grupo", names(nueva))] <- grupo
  
  ## Pegamos los NA
  nueva <- dplyr::bind_rows(nueva, nas)
  return(nueva)
}



# library(dplyr)
# df.b <- readRDS("../output/concentrado_all_years.rds") %>% dplyr::filter(., anio == "2012") %>%
#   dplyr::select(., proyecto, folioviv, foliohog, factor, ing_cor)
# 
# df <- dplyr::sample_n(df.b, 100)
# 
# df.d <- corte.economico(df = df, variable.ref = "ing_cor", pesos = "factor", cortes = 10, nombre = "deciles", prefijo = "d")