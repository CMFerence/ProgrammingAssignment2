## It starts with creating a 3x3 matrix with 9 numbers selected from 1 to 27.
## It calls this matrix "ogmat" and solves the inverse which it calls "invmat"
## It sets up a function called makeCacheMatrix which caches the inverse of "ogmat"
## It creates a function called cacheSolve, which either calculates the inverse
## or retrieves it from the cache.
## Finally, it runs makeCacheMatrix once to set up the cache and cacheSolve twice to calculate the inverse
## and retrieve it from the cache, then prints "ogmat" and "invmat" to confirm the cached
## inversed matrix, and multiples "ogmat" and "invmat" to get the identity matrix.

x <- sample(1:27, 9)
ogmat <- matrix(x, nrow=3, ncol=3)
invmat <<- solve(ogmat)

## The makeCacheMatrix function creates an object that contains four functions [set(), get(), 
## setinverse(), & getinverse()] and two objects [x & i] which are assigned values in
## the parent environment.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(invert) i <<- invert
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## The cacheSolve function retrieves the inverse of the returned object from  makeCacheMatrix(),
## or calculates it if it is not found cached.


cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)) {
		print("Inverse found in cache...")
		return(i)
	}
	print("Inverse solved...")
	cSmat <- x$get()
	i <- solve(cSmat, ...)
	x$setinverse(i)
	i
}

mCM <- makeCacheMatrix(ogmat)
cacheSolve(mCM)
cacheSolve(mCM)
ogmat
invmat
ogmat %*% invmat

##The End
