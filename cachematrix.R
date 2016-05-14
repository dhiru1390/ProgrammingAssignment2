# The Programming Assignment 2 objective was to write two functions where, the first function
# creates a special sqaure matrix and cache the inverse value of the matrix and the second function
# solves for the inverse of the matrix by checking in the first place, whether the cache has a stored value, if not
# it calculates the inverse of the matirx and returns the same to the function.

# MakeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# --> SetMatrix      set the value of a matrix
# --> GetMatrix      get the value of a matrix
# --> CacheInverse   get the cahced value (inverse of the matrix)
# --> GetInverse     get the cahced value (inverse of the matrix)

MakeCacheMatrix <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        SetMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # returns the stored matrix
        GetMatrix <- function() {
                x
        }

        # cache the given argument 
        CacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        GetInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, CacheInverse = CacheInverse, GetInverse = GetInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# MakeCacheMatrix
CacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$GetInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$GetMatrix()
        inverse <- solve(data)
        y$CacheInverse(inverse)
        
        # return the inverse
        inverse
}
