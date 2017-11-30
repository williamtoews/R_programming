# makeVector is a function that makes a special vector which is a list containing...
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeVector <- function(x = numeric()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


# cachemean is a function that checks if the mean of x is already calculated,
# if it is then it gets the mean from the cache
# if not, then it calculates the mean and caches it

cachemean <- function(x, ...){
  m <- x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


### OK, that works, now on to the assignment.


# makeCacheMatrix
# this function is analogous to makeVector...

makeCacheMatrix <- function(mat = matrix()){
  inv <- NULL
  set <- function(y){
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
  

# cacheSolve
# this function is analogous to cachemean, but it checks for a solved inverse to the matix

cacheSolve <- function(mat, ...){
  inv <- mat$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}


# this only works if mat has an inverse.

# spend some time trying to understand this ok?