## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This is function below creates a cache of an inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    #makes m empty first
        set <- function(y) {
                x <<- y       # y is the matrix being set into the value.
                m <<- NULL    # m is the empty value
        }
        
        get <- function() x  #call the value of X
        
        setinverse <- function(inversematrix) m <<- inversematrix   # puts a value into m that
        
        getinverse <- function() m  #gets the value of m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()     #will get the most current value of x
                m <- solve(data, ...)      #the solve here is for inverse of a matrix
                x$setinverse(m)
                m

}
