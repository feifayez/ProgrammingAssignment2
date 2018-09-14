##  The cachematrix.R contains two functions, makeCacheMatrix() and cacheSolve(). The makeCacheMatrix() function creates an R object that 
## stores the matrix and its inverse. The cacheSolve() function requires an argument that is returned by makeCacheMatrix() in order to 
## retrieve the cached value that is stored in makeCacheMatrix() object's environment. If the cached value dosen't exisit, cacheSolve() 
## will calcualte the inverse of the matrix and store it in the marketCacheMatrix() object's environment.

## The makeCacheMatrix() function returns a list of functions [set(), get(), setinverse(), and getinverse()] and two data objects (x and i)
## to the parent environment. The makeCacheMatrix() function dosen't conduct any calculation. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve() function takes an R object returned by makeCacheMatrix() function. It first check to see if the inverse of the matrix
## created with makeCacheMatrix() already exisits. If yes, it returns the cached value. If not, it calculate the inverse and store it in 
## the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message ("getting cached value")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

## The reason that the cacheSolve() function can access the value of x and i through the set(), get(), setinverse(), and getinverse()
## functions in the parent envrionment of makeCacheMatrix() are because: (1) lexical scoping, (2)  functions that return objects 
## of type list() also allow access to any other objects defined in the environment of the original function, (3) cacheSolve() 
## takes argument that is of type makeCacheMatrix().

## Acknowledgement: Without the explaination by Len Greski in his article Demystifying makeVector(), I would have not been
## able to understand, let alone complete, this assginment. I would recommend everyone to read it throughly. 
## datasciencectacontent/markdown/rprog-breakingDownMakeVector.md
