## cacheMatrix.R
## These functions allow a matrix to be stored in a special object
## together with its inverse. Costly recomputation is avoided by returning
## the cached value for subsequent requests for the inverse.
##
## Example:
##      a <- matrix(c(4,2,7,6),2,2)
##      b <- makeCacheMatrix(a)
##
##      cacheSolve(b)
##      [,1] [,2]
##      [1,]  0.6 -0.7
##      [2,] -0.2  0.4
##
##      cacheSolve(b)
##      getting cached data
##      [,1] [,2]
##      [1,]  0.6 -0.7
##      [2,] -0.2  0.4


## makeCacheMatrix - This function create a matrix "object" that can 
##              cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the cached inverse value
        inv <- NULL
    
        ## set() - store the new matrix value    
        set <- function(y) {
                x <<- y
                
                ## The stored inverse is invalidated because of the 
                ## new matrix value. Reset it to NULL.
                inv <<- NULL
        }
    
        ## get() - return the stored matrix value
        get <- function() x        

        ## setinverse() - store the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## getinverse() - return the stored inverse of the matrix
        getinverse <- function() inv
        
        ## 
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve(x) - This function computes the inverse of the special "matrix" x
##              returned by makeCacheMatrix. If the inverse has already been
##              calculated and then the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## Try to get the stored inverse value
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                ## Return the stored value found
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise, get the matrix data and calculate the inverse.
        ## Store the calculated value in the matrix object for caching.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## Return the calculated value
        inv
}
