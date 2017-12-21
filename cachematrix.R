Create a Matrix with a cached inverse, the matrix and its inverse
can both be set and set directly or solved with its companion
function, i.e. cacheSolve(x, ...)
 x - matrix to be cached
list - with name set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,get = get,setInverse = setInverse,
    getInverse = getInverse)
}
Resolved the inverse of a matrix assuming it is inversible, the
function made used of the built-in inverse matrix cache from its
argument, i.e. x of type list
 x - list of cached matrix and its inverse
 matrix - the inverse matrix of x


cacheSolve <- function(x, ...) {

    
    
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

