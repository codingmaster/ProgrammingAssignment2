## These functions calculate inverse of invertible matrix and put it to cache.
# If inverse was found in cache the cache version is returned
# Otherwise the inverse matrix is calculated and put to cache and returned
##

## This function takes an invertible matrix as argument
#  For this matrix it creates 4 functions:
#  set - sets the matrix to cache
#  get - gets the matrix from cache
#  setInverse - sets the calculated inverse of the matrix to cache
#  getInverse - gets the calculated inverse from the cache
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function calculates the inverse of the invertible matrix with the solve function
#  It takes cached matrix as argument
#  If inverse of the matrix is in cache the cache is returned
#  Otherwise the inverse is calculated, set to cache and returned
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}