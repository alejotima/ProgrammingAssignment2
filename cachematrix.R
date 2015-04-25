## This function creates a  matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initializing the inverse property
    inv = NULL
    
    ## Method to get the matrix 
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Method to get the matrix
    get = function() {
        x
    }
    
    ## Method to set the inverse of the matrix
    setInverse = function(inverse) {
        inv <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getInverse = function() {
        inv
    }
    
    ## Returns the list of methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function.
cacheSolve <- function(x, ...) {
    ## getting a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
    if(!is.null(inv)) {
        message("Obteniendo datos de cache")
        return(inv)
    }
    
    ## getting the matrix from our object
    data <- x$get()
    
    ## calculating the inverse by using matrix multiplication
    m <- solve(data) %*% data
    
    ## storing the inverse to the object to future usage
    x$setInverse(m)
    
    ## returning a matrix that is the inverse of 'x
    m
}
 