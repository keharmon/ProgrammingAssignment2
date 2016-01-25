## The first function stores a list of functions
## The second function calculates the inverse of a matrix but sees if it is stored
## before proceeding to calculate the inverse and store it.


## Makes "special" object storing a list of 4 functions (e.g., set, get) to set and get matrix value
## and set and get inverse value
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## If i (for inverse) exists in memory, the function tells you it is getting the value and returns it
## If i is null, data is assigned the matrix created in first function, inverse is taken and stored
cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##trying it out
m.matrix <- makeCacheMatrix(matrix(5:8, 2, 2)) #creating my matrix

m.matrix$get() #returns my matrix

m.matrix$getinverse() #doesn't exist yet

cacheSolve(m.matrix) #returns inverse for my matrix

m.matrix$getinverse() #this is stored now so returns inverse for my matrix




