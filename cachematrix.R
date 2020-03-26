#Overall, I have two functions. The first defines and creates a list of set and get functions that 
##allows me to set matricies and inverses and grab them as I need. The other function actually 
##calculates the inverse of the matricies but it first searches to see if this inverse was previously 
##calculated using the functions defined in the first function.


##makeCacheMatrix() builds a set of set and get functions and returns these functions within a list 
##to the parent environment

makeCacheMatrix <- function(x = matrix()) { ##to make the matrix straight into this function, need 
  ##(()) before putting in data, rows, and columns
  i <- NULL ##i is set to NULL creating it as an object within the makeCacheMatrix() environment that 
  ##is to be used later in the function
  set <- function(y) { ##need to call this argument y (instead of x) to keep the code easy to understand
    x <<- y ##<<- assigns the value to the right of the operator to an object in the parent environment 
    ##named by the object left of the operator
    i <<- NULL ##this line clears any value of i from a previous execution of cacheSolve()
  }
  get <- function() x ##getting the value i from the makeCacheMatrix parent environment
  setinverse <- function(inverse) i <<- inverse ##need to access the inverse after we're done with 
  ##this function so i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##create a list of all the functions with names
}


##cacheSolve() calculates the inverse of the matrix created above; however, it first checks to see 
##if the inverse has already been calculated, if it has already been calculated, it gets the inverse 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setinverse() function

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##attempts to retrieve an inverse from the object passed in the argument
  if(!is.null(i)) { ##if value isn't equal to NULL here we would have a valid cached inverse returned 
    ##to parent environment
    message("getting cached data") 
    return(i)
  }
  data <- x$get() ##if there is no cached inverse for the matrix, it will calculate the inverse and 
  ##setinverse() on the input object
  i <- solve(data, ...) ##note that solve only works on invertible matrices (ie. squares)
  x$setinverse(i)
  i ##it will then return the inverse matrix to the parent environment and print the inverse object
}
