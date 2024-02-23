## 2 functions which together return the inverse of a matrix, either by calcutating it or,
## by returning that cached matrix without calculation if the mean of that matrix is 
## already in the cache

# 1st function :create the cache for the inverse matrix 
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

# 2nd function: retrieves a matrix and return its inverse either from the cache 
# or by calculating it if not in the cache
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i       ## Returns a matrix that is the inverse of 'i'
}

