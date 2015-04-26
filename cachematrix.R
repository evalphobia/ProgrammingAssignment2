#' Compute caching inversed matrix.

#' makeCacheMatrix makes a matrix with caches.
#' @param Matrix x 
#' @return List
#'         - function set Set value to [x]
#'         - function get Get value of [x]
#'         - function setInverse Set value to [iv](inversed matrix of [x])
#'         - function getInverse Get value of [iv]
makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y){
        x <<- y
        iv <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inverse){
        iv <<- inverse
    }
    getInverse <- function(){
        iv
    }
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#' cacheSolve returns inversed matrix of input matrix
#' if cache hits, get matrix from the cache, or solve it.
#' @param Matrix x result of makeCacheMatrix(x) 
#' @return Matrix inversed matrix of [x]
cacheSolve <- function(x, ...) {
    iv <- x$getInverse()
    if(!is.null(iv)) {
        message("getting cached data")
        iv
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setInverse(iv)
    iv
}
