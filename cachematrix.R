#A matrix whose inverse is to be calculated is supplied as argument to makeCacheMatrix function, which returns a list. If the returned list is assigned to a variable, say x, then x can be an argument to cacheSolve function, which returns the inverse of the matrix. The inverse is calculated in cacheSolve only on the first invocation and stored in x. As long as x is unchanged, the subsequent invocations of cacheSolve (with argument x) will retrieve the stored inverse (cached) from x, rather than calculating all over again. If x changes, the inverse is calculated again and the new inverse is stored in x. Thereafter, subsequent invocations of cacheSolve with argument x will retrieve the cached value until x unchanged.

#makeCacheMatrix takes a matrix as argument and returns a list with functions to store a new matrix (set), retrieve the matrix(get), store the inverse of the matrix (setInv), and retrieve the inverse of the matrix(getInv). When a new matrix is set, the stored inverse (if any) is reset to null. 


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get,
		  setInv = setInv,
		  getInv = getInv)
}

#cacheSolve takes as argument the list, say x, returned by makeCacheMatrix function, and returns the inverse of the matrix that was supplied as argument to makeCacheMatrix function. In the first invocation of cacheSolve, the inverse of the matrix is calculated and the inverse is stored (set) in x. On subsequent invocations of cacheSolve, the inverse stored in x is returned, without calculating the inverse again. If x changes, then the inverse is calculated again and the inverse is stored in x, which is retrieved on subsequent invocations of cacheSolve.  

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}
