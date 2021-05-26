#################### Assignment 2: Caching the Inverse of a Matrix ##################################
## function create "matrix" object that will cache its inverse.
rm(list=ls(all=TRUE));
rm(list=ls(all=TRUE)); 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## This function computes the inverse of the "matrix" makeCacheMatrix. If the inverse has been previously calculated and the matrix has not been changed, the inverse will be retrieved from the cache.
 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
	}
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

###Example 1:#### 
##we can create a "matrix" and create cache and retrieve the inverse.

my_matrix <- makeCacheMatrix(matrix(3:6, 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

###Example 2: #####

my_matrix <- makeCacheMatrix(matrix(c(23,4,5,26,47,9,19,43,52), 3, 3))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

