## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    get <- function() x
    set <- function(mm){
        x <<-mm
        cache <<- null
    }
    setInv <- function(inverse) cache <<-inverse
    getInv <- function() cache
    
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse<-x$getInv()
    if(!is.null(inverse)){
        print('cached inverse')
        return(inverse)
    }
    my_matrix<-x$get()
    inverse<-solve(my_matrix)
    x$setInv(inverse)
    inverse
}
