##This R code calculates the Inverse of a Square Matrix if its not already calculated.
##If it is calculated,it simply returns the inverse matrix from the cache thus saving processing time
##These functionalities are achieved by below two functions.

## Function1: makeCacheMatrix - takes argument'x' which is a Square Matrix
## This function creates the matrix and saves its Inverse by caching concept(<<-)
## It returns a special matrix which is simply a list of functions to get/set matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { x <<- y
                       inv <<- NULL}
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get = get, setInv = setInv, getInv=getInv)
}


## Function2: CacheSolve - takes argument 'x' which is a special matrix created using above function
## This function first checks if Inverse for a matrix is already created
## If yes, it returns the Inverse matrix from Cache
## If not, it calculates the inverse and sets the cache variable and returns the same

cacheSolve <- function(x, ...) {
       
  inv <- x$getInv()
  if(!is.null(inv))
    { print("getting matrix from cache")
      return(inv) }             
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  return(inv)   ## Returns a matrix that is the inverse of 'x'
}
