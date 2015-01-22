## the makeCacheMAtrix function returns four objects: 
## set to define the value of the matrix
## get to retrive the matrix, setinverse to establish the matrix inverse
## and getinverse to retrieve the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## cacheSolve calculates de inverse of the matrix.
## if the inverse already exists (getinverse is not null)
## then it returns the cached matrix return(m)
## if its null, it retrieves the matrix (data) and calculates
## the inverse through the solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}
