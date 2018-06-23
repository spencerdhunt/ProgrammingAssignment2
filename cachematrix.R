## Put comments here that give an overall description of what your
## functions do

## Spencer Hunt :)

#This will allow you to define a matrix variable that lets you 
#solve for the inverse and cache the values for easier/faster access later.

## Write a short comment describing this function
## The returns a list of functions that defines a matrix and
## an inverse value as null, the <<- operators let you access this information
## in the parent function so you can call on existing values when solving

makeCacheMatrix <- function(x = matrix()) {
    ## returns the 'matrix' object
    m <- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <- function() x #return the matrix x
    setinverse <- function(inv) m <<- inv #set the inverse but have it accessible in the parent func
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
    ) #return the list with functions that provie the matrix and the corresponding fxns
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...){
    #returns the inverse function
    m <- x$getinverse() #try pulling an existing inverse value of the matrix
    if(!is.null(m)){ # if it exists return the cached data
        message("getting cached data")
        return(m)
    }
    #otherwise solve for the matrix inverse and return it
    # but also 'cache' the newly solved inverse value
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
