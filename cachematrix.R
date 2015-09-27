## The first function, makeCacheMatrix creates a special "matrix", that cache its inverse by
## set the value of the matrix
## get the value of the matrix 
## set the value of the matrix inverse
## get the value of the matrix inverse

## The resulting output will be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list( set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will output the inverse matrix of the original matrix stored in cache
## If the inverse matrix is not in cache it will calculate the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if (!is.null(inv){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        
        inv
}
}
