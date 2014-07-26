# Together, these two functions solve for the inverse of a matrix and cache the 
#     inverse so that if there are any additional requests to solve that same 
#     matrix, costly recomputation is avoided.    


# makeCacheMatrix() is a function which stores two matrices and outputs a list 
#     containing functions which let other functions outside makeCacheMatrix's 
#     environment assign and access these matrices.  

# Matrix 'x' is the function's input, an invertible matrix.  
# 'cache' is NULL, but the following function, cacheSolve(), assigns it 
#     to be the inverse of 'x'.  
# 'setMatrix(y)' lets other functions outside makeCacheMatrix()'s environment
#     clear 'cache' and set 'y' to be the new 'x'.
# 'getMatrix()' accesses matrix 'x', the matrix to be inverted.
# 'setCache(someMatrix)' lets functions outside makeCacheMatrix()'s 
#     environment assign 'someMatrix' to be the new 'cache'.  
#     'getCache()' accesses 'cache'.  

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      setMatrix <- function(y) {
            x <<- y
            cache <<- NULL
      }
      getMatrix <- function() x
      setCache <- function(someMatrix) cache <<- someMatrix
      getCache <- function() cache

      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setCache = setCache,
           getCache = getCache)
}


#cacheSolve() is a function which accesses a matrix in makeCacheMatrix()'s 
#     environment ('cache'), determines whether a solution has already been 
#     stored in 'cache', and either returns that solution or accesses the 
#     other matrix stored in makeCacheMatrix's environment ( called 'x' in 
#     makeCacheMatrix() and 'data' in cacheSolve() ), computes its inverse, 
#     then caches and returns that inverse.  

cacheSolve <- function(x, ...) {
      cache <- x$getCache()
      if (!is.null(cache)) {
            message("Getting cached solution.")
            return(cache)
      }
      data <- x$getMatrix()
      cache <- solve(data, ...)
      x$setCache(cache)
      cache
}