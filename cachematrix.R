# The following two functions form the inverse of a square matrix using the 
# solve function, and cache the result. 

############################   Example   #######################################
# First form a matrix                                                          #
# > mat <- matrix(c(1, 2, 0, 1), nrow = 2, ncol = 2)                           #
# > mat                                                                        #
#      [,1] [,2]                                                               #
# [1,]    1    0                                                               #
# [2,]    2    1                                                               #
#                                                                              #
# Use the makeCacheMatrix()                                                    #
# > cachemat <- makeCacheMatrix(mat)                                           #
#                                                                              #
# The first time you use the cacheSolve() function it forms the inverse        #
# > cacheSolve(cachemat)                                                       #
#      [,1] [,2]                                                               #
# [1,]    1    0                                                               #
# [2,]   -2    1                                                               #
#                                                                              #
# The next time it is used it returns the cached inverse                       #
# > cacheSolve(cachemat)                                                       #
# getting cached data                                                          #
#      [,1] [,2]                                                               #
# [1,]    1    0                                                               #
# [2,]   -2    1                                                               #
#                                                                              #
################################################################################

# The makeCacheMatrix function creates a special "matrix" object that can cache
# its inverse. It forms a list containing a function to 
# 1.set the special matrix
# 2.get the special matrix
# 3.set the inverse
# 4.get the inverse
# It does not check that the matrix is square or has an inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# The following function returns the inverse of the matrix created with the 
# above function. It first checks to see if the inverse has already been formed.
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse, using the solve() function, and caches 
# it via the setinv function.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
	message("getting cached inverse")
	return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$setinv(m)
	m
}
