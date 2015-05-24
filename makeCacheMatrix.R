
Week 3. Assigment 2.

#--------------------------------------------------------
# Capturando la matrix 
#--------------------------------------------------------
## Creates a special "matrix" object that can cache its inverse
# Crea una matrix especial que puede almacenar en cache su inversa

makeCacheMatrix <- function(x = matrix()) {
        # i will store the inverse
        # esta variable almacena la inversa
             inv <- NULL
             
        ## set should be used to alter the matrix
        # it invalidates the cache
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get simply returns the raw matrix
        # Captura la marix
        
        get <- function() {x}
        
        # setinv sets the inv variable
        # should be used only by cacheSolve
        # esta funcion sera retornada en la funcion cacheSolve
        
        setinv <- function(i) {inv <<- i}             
        getinv <- function() inv
        
        # return the special matrix
        # retorna la matrix especial
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#--------------------------------------------------------
## Calculate the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cachesolve will retrieve the inverse from the cache.
# Obtenienddo la inversa de la matrix
#--------------------------------------------------------

cacheSolve <- function(x, ...) {
        # get the cached inverse
        inv <- x$getinv()
        if(!is.null(inv)) {
                # if the inverse if actually cached, just return it
                # si el valor de la inversa es calculado, lo retorna
                message("Obteniendo la data en memoria")
                return(inv)
        }
        # otherwise, calculate the inverse and cache it
        # condicion del else que ejecuta la operacion  inversa al proceso solicitado anteriormente
        mymatrix <- x$get()
        inv <- ginv(mymatrix, ...)
        x$setinv(inv)
        return(inv)
}

