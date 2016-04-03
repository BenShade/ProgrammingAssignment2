## These funstions enable the creation of a matrix and its inverse which can later be returned
## without having to recalculate it each time.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ReverseMatrix <- NULL
  set <- function(y){
    x <<- y
    ReverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) ReverseMatrix <<- solve
  getinverse <- function() ReverseMatrix
  list( set = set
        ,get = get
        ,setinverse = setinverse
        ,getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ReverseMatrix <- x$getinverse()
  if (!is.null(ReverseMatrix)) {
    message("Getting cached inverse matrix")
    return(ReverseMatrix)
  }
  message("Calculating inverse of matrix")
  SourceMatrix <- x$get()
  ReverseMatrix <-solve(SourceMatrix, ...)
  x$setinverse(ReverseMatrix)
  ReverseMatrix
}
