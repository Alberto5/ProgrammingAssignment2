## It works together with cachesolve, this function creates the "method" which is later executed
## and it's result is latter cached again in this function (the inverse matrix)
## m can be called from both functions because of the superassignment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   ## creates m as an object with Null
  set <- function(y) {  ## creates a function "set" that superassigs y to x and m to Null 
    x <<- y
    m <<- NULL
  }
  get <- function() x   ## defines get as a function that returns x
  setinverse <- function(inverse) m <<- inverse  ## defines a function that superassigs the result of the inverse function to m (from the setinverse in the cacheSolve function)
  getinverse <- function() m   ## function that retrives m and assings it to getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## lists all elements names, so they can be called from cacheSolve function  
}


## this function first checks if the inverse was calculated before (cached in x),
## if it is, it returns a message and "m", which is the inverse matrix
## if x x$getinverse is null, then the function gets the data and calculates the inverse matrix,
## the inverse matrix is later stored on x, and the function returns the inverse (m)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()             # assigning the inverse from x to "m"
  if(!is.null(m)) {               #checking if "m" is null, if its not, returning a message and "m
    message("getting cached data")
    return(m)
  }
  data <- x$get()     # uses the get from x (from makecachematrix)
  m <- solve(data, ...) #inverts the matrix and assigns it to "m"
  x$setinverse(m)       # assigns the m matrix to "x" as a property
  m                   # Returns "m"
}
