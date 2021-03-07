## Esse script recebe uma matriz inversível, gera a matriz inverida e armazena
## esse resultado em cache, para que sempre que precisarmos recuperar novamente
## essa matriz invertida, não precisemos executar novamente os cálculos de inversão
## pois já teremos esses resultados em cache. Importante ressaltar que essa fase 
## de recalcular a matrix só será evitada caso a matriz original não sofra alteração


## Minha primeira função (makeCacheMatrix()) recebe uma matrix inversível e
## gera uma matriz invertida a partir da matrix original. Essa função também
## gera 4 listas que poderão ser utilizadas pela minha segunda função (cacheSolve()),
makeCacheMatrix <- function(x = matrix()) {
    mat_i <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        mat_i <<- NULL
    }
    
    setsolve <- function(solve) mat_i <<- solve
    getsolve <- function() mat_i
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Essa segunda função, recebe o objeto criado através da função makeCacheMatrix(),
## contendo suas listas e verifica se a matriz original não sofreu alteração, 
## nesse caso, ela não executará novamente a inversão e apenas retornará a matriz
## invertida já armazenada em cache, caso contrário, ou seja, caso sua matriz
## original tenha sofrido alguma alteração, a função cacheSolve() vai executar
## novamente a inversão dessa nova matriz, visto que esta nova não possuia sua 
## iversão armazenada em cache.
cacheSolve <- function(x, ...) {
    mat_i <- x$getsolve()
    if(!is.null(mat_i)) {
        message("getting cached data")
        return(mat_i)
    }
    data <- x$get()
    mat_i <- solve(data, ...)
    x$setsolve(mat_i)
    mat_i
}