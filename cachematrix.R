##Solution for Coursera R assignment 2
##Create cache matrix inverse function  


## Function  to create a cache matrix 
## sample usage  makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(amatrix) {
        x <<- amatrix
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function(){ 
        m                   
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse )
  
}


## Get the the cached matrix or create it if it does not exist
## sample usage a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) 
## cacheSolve(a)

cacheSolve <- function(x, ...) {
    
    cachem <- x$getInverse()
  ##Check if already cached    
    if(!is.null(cachem)) {
        message("getting cached data")
        return(cachem)
    }
    data <- x$get()
    cachem <- solve(data, ...)
    x$setInverse(cachem)
    cachem 
    
  ## Return a matrix that is the inverse of 'x'
}

## Used for testing
testmatrixcacheinverter <- function(){
    am = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
    print(am$get()) 
    print(cacheSolve(am))
    print(cacheSolve(am))##check to make sure using cache matrix
    am$getInverse()
}