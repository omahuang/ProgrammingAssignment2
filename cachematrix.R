## makeCacheMatrix() first initialize 'x' as a empty matrix, and 'inv' as NULL
## set() function assign a new matrix value to 'x' and reset the 'inv' as NULL
## get() function return the value of 'x'
## setInverse() function store the value of inverse in 'inv'
## getInverse() return the value of 'inv'

## This function creates a list that cache a matrix's inverse value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function takes the result of makeCacheMatrix() as input.
## It first checks if the value of 'inv' already exist, 
## if it does, then return the value of 'inv'
## if not, then inverse the matrix, store it in 'inv' and return the value. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv        
}
