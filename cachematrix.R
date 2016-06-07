## makeCacheMatrix creates an object representing a list, which contains following functions: 
## get the matrix(1), set the matrix(2), set the inverse(3) and get the matrix invesre(4)
## cacheSolve 1) checks whether an object made by makeCacheMatrix contains an inverse of the original matrix 
## if not: calculates an invers matrix and writes it into the object

makeCacheMatrix <- function(x = matrix()) {
    
    s<-NULL
    # definition for a set the matrix function
    set_mat<-function(y){
        # assigning values to variables x and s, placed in an enviroment of "makeCacheMatrix"
        x<<-y
        s<<-NULL
    }
    # definition for a get the matrix function
    get_mat<-function() x
    # definition for a set the invers matrix function
    set_inv_mat<-function(solve) s<<- solve
    # definition for a get the invers matrix function
    get_inv_mat<-function() s
    # definition for a return of the makeCacheMatrix - list of 4 functions
    list(set_mat=set_mat, 
         get_mat=get_mat,
         set_inv_mat=set_inv_mat,
         get_inv_mat=get_inv_mat)
}

cacheSolve <- function(x=matrix(), ...) {
    
    s<-x$get_inv_mat() # derives an inverse matrix from object of makeCacheMatrix()
    # checks whether this inverse is null, if not: returns this value
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    # calculates an inverse matrix from object of makeCacheMatrix() and records it to this objects 
    matrix<-x$get_mat()
    s<-solve(matrix, ...)
    x$set_inv_mat(s)
    s
}