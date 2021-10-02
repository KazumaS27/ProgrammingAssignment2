## Through this function, a variable would be created
## as x in the input in comparison with the fact that the matrix is the solution
## and if not, x is equivalent to NULL 

makeCacheMatrix <- function(x = matrix()) {
  in <- NULL
  set <- function(y) {
    x <<- b
    in <<- NULL
  }

  get <- function() {x}
  setInverseMatrix <- function(inverse) {in <<- inverse}
  getInverseMatrix <- function () {in}
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

## Through this, you will be assessed in erecting the inversed matrix as
## caches which would retrieve
## and try again later in order to prompt


cacheSolve <- function(x, ...) {
  in <- x$getInverseMatrix()
  if(!is.null(in)) {
    message("retrieving cached data")
    return(in)
  }

  ans <- x$get()
  in <- solve(ans,...)
  x$setInv(in)
  in
}