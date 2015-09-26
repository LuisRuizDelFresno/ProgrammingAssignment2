## Put comments here that give an overall description of what your
## functions do: 
# makeCacheMatrix defines 4 functions to set and get a matrix 
# and its inverse in cache. 
# cacheSolve uses 3 of these functions to return the inverse matrix, 
# preferently retriving it from cache, but it doesn't exist solving it. 

# makeCacheMatrix <- function(x = matrix()) returns a list of 4 functions: 
# It is a function with an argument: x, that by default is an empty matrix
# #1:
    # It creates an empty object: "solution_mtrx"
# #2: 
    # It create a couple of functions setter/getter to set/get the original matrixs 
    # : "set_mtrx" and "get_mtrx"
    # and another for setting/getting the calculated matrix 
    # (in our case the inverse matrix):
    # : "set_solution_mtrx" and "get_solution_mtrx"
    # In every couple, the SETTERS do look for cached values: 
        # "y" stands for the original matix; and "solution" for the calculated matrix
        # We force to look for cached values using the operator "<<-". 
    # The "<<-" operator does the same as "<-", but produce 
        # a search for the definition of the variable being assigned 
        # through parent environments, before than in the global enviroment. 
        # If such a variable is found (and its binding is not locked) then 
        # its value is redefined, otherwise assignment takes place in 
        # the global environment. 
    # GETTERS just look for the value of mtrx or inv_mtrx

# #3: At the end, we return a list with 4 elements, 
    # each is one of these 4 functions set... or get..., named with the same spelling

makeCacheMatrix <- function(mtrx = matrix()) {
    
    #1 
    solution_mtrx <- NULL
    
    #2
    set_mtrx <- function(y) {
        mtrx <<- y
        solution_mtrx <<- NULL
    }
    get_mtrx <- function() mtrx
    
    set_solution_mtrx <- function(solution) solution_mtrx <<- solution
    get_solution_mtrx <- function() solution_mtrx
    
    #3
    list(set_mtrx = set_mtrx, get_mtrx = get_mtrx,
         set_solution_mtrx = set_solution_mtrx,
         get_solution_mtrx = get_solution_mtrx)

}

## Write a short comment describing this function
# cacheSolve(x, ...)   
# By definition this function uses the list produced in the above function 
# (makeCacheMatrix) as argmunet "x". 
# #4
    # It gets the function for getting the inverse matrix from the list "x"
# #5 
    # if the inverse matirx already exist it print a message, 
    # returns th inverse matrix and quit
# #6
    # if it doesn't exist, get the original matrix (calling the getter function from
    # the list), solve it, obtaining the inverse matrix
    # call the setter function to save it in cache
    # and return it

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'mtrx'
    # 4
    solution_matrix <- x$get_solution_mtrx()

    #5 
    if(!is.null(solution_matrix)) {
        message("getting cached data")
        return(solution_matrix)
    }
    
    #6
    data            <- x$get_mtrx()
    solution_matrix <- solve(data, ...)
    x$set_solution_mtrx(solution_matrix)
    
    solution_matrix
}
