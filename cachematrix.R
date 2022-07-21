## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Function: makeCacheMatrix - A special "matrix" object is created by this function that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         g <- NULL
    set <- function(y) {
        x <<- y
        g <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) g <<- inverse
    getinverse <- function() g
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)


}


## Write a short comment describing this function

## Function: cacheSolve - The inverse of the special "matrix" returned by makeCacheMatrix above is computed by this function
## The cacheSolve should retrieve the inverse from the cache, if the inverse has already been calculated (and the matrix has not changed).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        g <- x$getinverse()
    if(!is.null(g)) {
        message("getting cached data")
        return(g)
    }
    matrix <- x$get()
    g <- solve(matrix, ...)
    x$setinverse(g)
    g

}
