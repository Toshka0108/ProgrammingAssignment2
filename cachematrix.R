## makeCacheMatrix function creates an object or smth similar, which contains 
## a matrix, its mean and 4 functions (set matrix and get it, set solved_matrix
## and get it). 
## cacheSolve function takes makeCacheMatrix object and looks for solved_matrix.
## If solved_matrix is not found (it's NULL), then the FUN computes it and set.
## If solved_matrix is found, the FUN get it.

## I'm not a programmer, so words 'variable', 'objects' and some other are used 
## intuitively.
## This FUN creates a list of fucntion and smth similar to two variables
## (Matrix x and its Inverse inversed_mat). Set function change Matrix to the 
## argument and clear Inverse. Get just get Matrix =). set_inverse change
## Inverse to the argument. get_inverse get Inverse.

makeCacheMatrix <- function(x = matrix()) {
    inversed_mat <- NULL
    set <- function(y) {
        x <<- y
        inversed_mat <<- NULL
    }
    get <- function() x
    set_inverse <- function(solved) inversed_mat  <<- solved
    get_inverse <- function() inversed_mat
    list(set = set,get = get,set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## First of all, this FUN checks if makeCacheMatrix object contains Inverse. 
## If it does, it just get it, otherwise it counts it and set the value to 
## Inverse variable.

cacheSolve <- function(x, ...) {
    inversed_mat <- x$get_inverse()
    if(!is.null(inversed_mat)) {
        message("getting cached data")
        return(inversed_mat)
    }
    data <- x$get()
    inversed_mat <- solve(data, ...)
    x$set_inverse(inversed_mat)
    inversed_mat
}

## This code tests the functions.

test_mat <- makeCacheMatrix(matrix(2:5,2,2))
cacheSolve(test_mat)
cacheSolve(test_mat)
test_mat <- makeCacheMatrix(matrix(c(1,0,3,4),2,2))
cacheSolve(test_mat)
cacheSolve(test_mat)
test_mat$get() %*% cacheSolve(test_mat)
