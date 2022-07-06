

## Esta función crea un objeto "matrix" especial que puede almacenar en caché su inverso.


makeCacheMatrix <- function( matrix  = matrix()) {
  
  # Almacena el valor inverso
  inversa <- NULL
  # Establece la matriz original
  set <- function(y) {
    matrix <<- y
    inversa <<- NULL
  }
  # Para obtener la matriz original
  get <- function() matrix
  # Para establecer el valor inverso
  set_inversa <- function(inv) inversa <<- inv
  # Para obtener el valor inverso
  get_inversa <- function() inversa
  
  # Se obtiene un lista de las 4 funciones que
  # creamos, esta lista es la "matriz especial "
  list(set = set, get = get, set_inversa = set_inversa, get_inversa = get_inversa)
}


# # Esta función calcula la inversa de la "matriz" especial devuelta por
# # makeCacheMatrix arriba. Si ya se ha calculado la inversa (y
# # la matriz no ha cambiado), entonces el cachesolve debería recuperar el
# # inversa desde el caché.


cacheSolve  <-  function(special_matrix , ... ) {
  inversa <- special_matrix$get_inversa()
  if(!is.null(inversa)) {
    message("Obtener datos almacenados en caché")
    return(inversa)
  }
  data <- special_matrix$get()
  inversa <- solve(data, ...)
  special_matrix$set_inversa(inversa)
  inversa
}






