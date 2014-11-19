##---------------- Programming Assingment 2 ------------------------------------------------------------------
# Example: Caching the Inverse of a Matrix
# In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.
 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##------------------------ Testing ------------------------------------------------------------------ 
amatrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)
a <- makeCacheMatrix(amatrix)   # create a matrix and assign it to a
a$get()                  # prints out matrix with values 1,2,3,4
a$getinverse()           # NULL result, getinverse() calls global variable m which is initialized to NULL
cacheSolve(a)            # calculates the inverse of the matrix
a$getinverse()           # result is inverse of a
cacheSolve(a)            # calls cachSolve again, this time the inverse has been calculated already and gets call from cache,"getting cached data"

amatrix <- matrix(c(10,20,30,40), nrow=2, ncol=2)
a$set(amatrix)           # set the values of matrix to amatrix
a$get()                  # prints out matrix with new values 10,20,30,40
a$getinverse()           # NULL because the set function called earlier initialized m <<- NULL
cacheSolve(a)            # m is NULL, therefore the inverse is calculated and get cached
cacheSolve(a)            # m !null so the inverse is retrieved from the cached data

a$get()                  # get a matrix with values 10 20 30 40
a$setinverse(0)          # do NOT call setinverse() directly despite it being accessible for the reason you will see next
a$getinverse()           # [1] 0        obviously non-sense since...
a$get()                  # get a matrix with values 10 20 30 40
cacheSolve(a)            # [1] 0 , the call to setinverse() effectively corrupted the functioning of the code

amatrix <- matrix(c(5, 25, 15, 65), nrow=2, ncol=2)
a <- makeCacheMatrix(amatrix)  # create new matrix with values 5, 25, 15, 65
a$get()                  # get a matrix with values 5, 25, 15, 65
cacheSolve(a)            # calculate inverse
cacheSolve(a)            # getting cached data
