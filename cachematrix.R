## If applied together, the two functions below return the inverse
## of the matrix that the first function takes as an argument (
## assumed that the matrix is invertible).

## The first function calculates the inverse of the martix that it
## takes as an argument and stores it in the cache.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setim <- function(solve) im <<- solve
  getim <- function() im
  list(set = set, get = get, setim = setim, getim = getim)
}


## The second function retrieves the inverse of the matrix stored
## in the cache (if there is one) and prints it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getim()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setim(im)
  im      
}
