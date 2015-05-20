## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix creates three functions, "get", "setinverse", "getinverse" 
## which are returned in a list
##
## cacheSolve operates on the functions created in makeCacheMatrix and checks 
## whether there is already a value stored for the inverse of a given matrix



## Write a short comment describing this function
##
## makeCacheMatrix creates three separate functions and initiates a variable i to
## store the inverse of a matrix. The variables the functions are assigned to
## are returned as a list, so they can be easily store and accessed by the
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # "get" function returns the value that was passed into the "makeCacheMatrix" 
  # function
  get <- function() x
  
  # "setinverse" function takes the argument inverse and assigns this value to
  # the variable i. This will overwrite the value "NULL" which has been assigned
  # to i in the first call of the "makeCacheMatrix" function
  setinverse <- function(inverse) i <<- inverse
  
  # "getinverse" function returns the current value of i
  getinverse <- function() i
  
  # returns a list of the functions created above, so they can be stored and
  # accessed easily
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
##
## chacheSolve takes the list returned by makeCacheMatrix as input and then computes
## the inverse of the matrix originally put into makeCacheMatrix or retrieves the 
## corresponding inverse if it has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # use the "getinverse" function defined in makeCacheMatrix. This assigns the
  # value of the variable i in the function "makeCacheMatrix" to the variable i
  # in the "cacheSolve" function
  i <- x$getinverse()
  
  
  # the if loop checks whether there is already another value than NULL stored in
  # the variable i. In that case the value stored in i will be returned and 
  # the execution of the rest of the function will be stopped
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # this assigns the value of original argument put into the function "makeCacheMatrix"
  # to the variable data by using the "get" function defined above
  data <- x$get()
  
  # this line computes the inverse of the matrix stored in the variable data and 
  # stores this inverse in the variable i
  i <- solve(data, ...)
  
  # this line uses the function "setinverse" to change the value associated with
  # the variable i in the above function "makeCacheMatrix". The variable i in 
  # "makeCacheMatrix" now contains the inverse which has just been computed
  # in the previous line
  x$setinverse(i)
  
  i
}

