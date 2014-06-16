
## We define a data type to cache one matrix and its inverse, aimming to avoid 
## to calculate repeatedly the inverse. This definition of data type is similar
## to class definition in OOP.
##
## To use it we have to declare the tipe (instantiate the class) using function
## 'makeCacheMatrix()' and then apply function 'cacheSolve()' on the declared
## type.

## 'makeCacheMatrix()' constructs a data type as list of functions containing:
## (a) a function to set the matrix
## (b) a function to get the matrix
## (c) a function to set the inverted matrix (it have to be calculated before
##     with 'cacheSolve()' function)
## (d) a function to get the inverted matrix 

makeCacheMatrix <- function(m = matrix()) {
    solution <- NULL
    set <- function(matrix) {
        m <<- matrix
        solution <<- NULL
    }
    get <- function() m
    setsolution <- function(solve) solution <<- solve
    getsolution <- function() solution
    list(set = set, get = get, 
         setsolution = setsolution,
         getsolution = getsolution)
}

## 'cacheSolve()' calculates the inverse of a matrix previously stored in a 
## data type 'makeCacheMatrix()' and cache it to be disponible using the
## data type.
## It (i) tries to get the inverse from an object of data type 'cacheSolve()',
## then (ii) it tests wether or not the inverse has been calculated before and
## is accesible by the object 'makeCacheMatrix()'. If the inverse is accesible 
## it (iii.1) returns the inverse matrix and finish function, in other case i (iii.2) 
## gets de matrix, calculate its inverse, cache it using object's function
## 'setsolution()' and finish function returning the inverse.
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
    solution <- m$getsolution()
    if(!is.null(solution)) {
        message("getting cached data")
        return(solution)
    }
    data <- m$get()
    solution <- solve(data, ...)
    m$setsolution(solution)
    return(solution)
}

## EXAMPLES

## Ex. #1
## A random matrix of 5x5
n_rows = 5
n_cols = 5
n_elem = n_rows*n_cols
elems = sample(1:n_elem, size = n_elem, replace = TRUE)
m <- matrix(elems, nrow = n_rows, ncol= n_cols)

## Creating an object of type 'makeCacheMatrix()'
mcm <- makeCacheMatrix(m)
mcm$get()         # There is a matrix into the object
mcm$getsolution() # It is not calculated inverse yet
cacheSolve(mcm)   # Calculates inverse and caches it
mcm$getsolution() # There is the inverse already cache in the object
mcm$get() %*% mcm$getsolution() # They are inverse each other
sum(mcm$get() %*% mcm$getsolution())

## Ex. #2
## A random matrix of 500x500
n_rows = 500
n_cols = 500
n_elem = n_rows*n_cols
elems = sample(1:n_elem, size = n_elem, replace = TRUE)
m <- matrix(elems, nrow = n_rows, ncol= n_cols)

## Creating an object of type 'makeCacheMatrix()'
mcm <- makeCacheMatrix(m)
mcm$get()         # There is a matrix into the object
mcm$getsolution() # It is not calculated inverse yet
tmp <- cacheSolve(mcm)   # Calculates inverse and caches it
tmp <- mcm$getsolution() # There is the inverse already cache in the object
tmp <- mcm$get() %*% mcm$getsolution() # They are inverse each other
sum(mcm$get() %*% mcm$getsolution())

# ## Ex. #3
# ## A random matrix of 5000x5000
# n_rows = 5000
# n_cols = 5000
# n_elem = n_rows*n_cols
# elems = sample(1:n_elem, size = n_elem, replace = TRUE)
# m <- matrix(elems, nrow = n_rows, ncol= n_cols)
# 
# ## Creating an object of type 'makeCacheMatrix()'
# mcm <- makeCacheMatrix(m)
# mcm$get()         # There is a matrix into the object
# mcm$getsolution() # It is not calculated inverse yet
# tmp <- cacheSolve(mcm)   # Calculates inverse and caches it
# tmp <- mcm$getsolution() # There is the inverse already cache in the object
# tmp <- mcm$get() %*% mcm$getsolution() # They are inverse each other
# sum(mcm$get() %*% mcm$getsolution())
