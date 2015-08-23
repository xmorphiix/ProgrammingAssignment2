## The idea ist to create a function that can cache the inverse of a special Matrix objeckt 
## that can later be computed by a second function.
## There are two functions necessary to do so. 
## First the function makeCacheMatrix and second the function cacheSolve

## The function makeCacheMatrix() takes an object attributed with a matrix (e.g. x<-matrix(1:4,2,2))
## which then is transformed to its inverse and cached within the function makeCacheMatrix()

makeCacheMatrix<- function(x = matrix()) {
        i<- NULL
        set <- function(y) {
                x<<-y
                i<<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        
}

## The function cacheSolve() takes an object attributed with the content of the function makeCacheMatric()
## (e.g. y <- makeCahceMatrix(x)), checks if it contains an inverted Matrix,
## if yes, it prints out the inverted matrix.

cacheSolve <- function(x, ...){
        i<- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setInverse(i)
        i
}
