## Functions that :
## creates a matrix with a cacheable inverse (assumes that the input matrix is invertible)
## inverts the matrix and stores the result in the object itself so that the next attempt to 
## invert the matrix will actually read the result from the cache, provided the original matrix
## doesn't change


## Creates a custom matrix object starting from a matrix that have the ability to cache
## data. Provides the following functions :
## get() : get the original matrix
## set() : set a new "original" matrix; this operation clears the cache. 
## getCached() : returns the cached object
## setCached() : set the cached object 
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    get <- function() x
    set <- function(mm){
        x <<-mm
        cache <<- null
    }
    setCached <- function(inverse) cache <<-inverse
    getCached <- function() cache
    
    list(set=set, get=get, setCached=setCached, getCached=getCached)
}


## Solves the inverse of a matrix and stores the result in a cached object in case a new call to
## solve the inverse of the same matrix is performed.
## Assumes that the input x is the result of makeCacheMatrix(y) where y is an invertible matrix


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse<-x$getCached()
    if(!is.null(inverse)){
        print('cached inverse')
        return(inverse)
    }
    my_matrix<-x$get()
    inverse<-solve(my_matrix)
    x$setCached(inverse)
    inverse
}
