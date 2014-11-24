## This program computes the inverse of a Matrix, and catch the result in cache, so as retreibe from cache whenever you need
## it. Saving the time for computing matrix inverses everytime.

## makeCacheMatrix()
## Input: Matrix "X"
## Output: list of functions set, get, setinv & getinv
## get - Returns the data in the matrix x
## set - Sets matrix x with the data passed as parameter
## getinv - gets the inverse of the matrix x
## setinv - sets the inverse of the matrix x through the function parameter/arguement

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
      }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve()
## Input: Matrix "X", whose inverse we need to compute
## Output: X matrix inverse
## This program calls  the already computed mean through getinv()
## If it is NULL, then it gets the matrix using get(), then computes the inverse and sets the inverse through setinv,
## else, it returns the cached value


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
                message("getting cached data")
                 return(inv)
         }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
