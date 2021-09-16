## This function will make the variable
## as x in the input whereas the matrix is the solution
## and if not, x is equals to NULL 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }

  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}

## This will assess you in creating the inversed matrix as
## caches that it will retrieve
## and will try again later to prompt


cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("retrieving matrix")
    return(inv)
  }

  ans<-x$get()
  inv<-solve(ans,...)
  x$setInv(inv)
  inv

}