
## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    get <- function() m

    ## to set the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }

    ## to get the inverse of the matrix
    getinverse <- function() i

    ## Return a list of the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachesolve <-to retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    i <- x$getinverse()

    if( !is.null(i) ) {
            message("getting cached data")
            return(i)
    }

    data <- x$get()

    i <- solve(data) %*% data

    x$setinverse(i)

    i
}
