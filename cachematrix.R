## The makeCacheMatrix and cacheSolve functions together create a matrix 
## object to calculate and cache an inverse matrix and then be able
## to return the value of that inverse matrix, once caclulated or from
## cache


## makeCacheMatrix creates a matrix object and returns a list that contains
## a function to set and get the value of the matrix, and set and get the
## the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	# m is the inverse matrix and is reset to NULL whenever makeCacheMAtrix
	# is called
	m <- NULL

    set <- function(y) {  #  takes an input matrix
            x <<- y       #  saves the input matrix as x
            m <<- NULL    #  resets the mean to NULL, basically what happens
                          #  when a new object is generated
    }

    get <- function() x  # this funct returns the value of original matrix

    set_inverse <- function(inverse) {  # called by cacheSolve during 1st 
    								  # cacheSolve execution
    	m <<- inverse  #  store the value of inverse using superassignment
    }

    get_inverse <- function() m  #  this will return the cached value to the
    							 #  cacheSolve function on subsequent accesses

    # makeCacheMatrix returns a list, which cacheSolve is then able to
    # access
    list(set = set, get = get,
    	set_inverse = set_inverse,
    	get_inverse = get_inverse)

}


## cacheSolve calculates the inverse of the matrix from makeCacheMatrix
## it firsts checks if the mean is been caclulated and cached
## if not it calculates the inverse and sets the value of inverse in the cache
## via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$get_inverse()  # access object x and get the value of matrix inverse

    if(!is.null(m)) { # if inverse was cached the followin is executed
    	message("getting cached data")  # this message is printed
    	return(m)  #  returns the inverse and return() terminates cacheSolve()
    }

    data <- x$get()  # if m is NULL cacheSolve executes this line

    m <- solve(data, ...)  # if m is NULL cacheSolve, calculate inverse matrix

    x$set_inverse(m)  # store the inverse in x

    m  #  return the inverse to the code that called this function

}
