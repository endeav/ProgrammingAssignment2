## The aim for the following two functions is to provide a less time expensive
## way to calculate the inverse of a 2x2 square matrix by caching functions.

## "makeCacheMatrix" function creates a new environment every time it is called
## containing the defined functions set, get, getinverse, setinverse
## as well as variable inv and matrix x (the parameter of the function).
## From OOP perspective this can be seen as a class with its members.

makeCacheMatrix <- function(x = matrix()) {
            # Initiate local variables
            inv <- NULL

            ## Define the getters and setters
            set <- function(y){
                  x <<- y
                  inv <<- NULL ## Set/Reset inv to null in case the matrix changes
            }
            get <- function() x

            ## In this context the "superassignment" operator is used due
            ## to writting functions in functions(Closure);
            ## if "inv" is not defined in this enclosure then a search will
            ## be performed in the parent environment until reaching the global
            ## environment; in case no such variable is found a new one will be created
            setinverse <- function(solve) inv <<- solve
            getinverse <- function() inv

            ## Functions cannot be subset therefore a list must be returned
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## "cacheSolve" is the function that returns the inverse of a "special" matrix
## using the methods defined in "makeCacheMatrix".
## It takes as parameters a makeCacheMatrix "object".
## When it's called for the first time it calculates the inverse and
## then stores it in cache.
## The cache is the corresponding environment created when makeCacheMatrix was
## called.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Check if the inverse has already been calculated in cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        ## Otherwise calculate it
        data <- x$get() ## Get the matrix
        inv <- solve(data) ## Calculate the inverse
        x$setinverse(inv) ## Save/set its value

        inv ## Return the local variable that contains the inverse or null
}
