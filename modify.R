RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


knapsack_dynamic <- function(x,W){
  stopifnot(colnames(x) == c("w","v"))
  stopifnot((ncol(x) == 2))
  stopifnot(all(x > 0))
  
  weight <- x$w
  value <- x$v
  nrow <- nrow(x) + 1
  ncol <- W + 1
  array <- array(0,dim = c(nrow,ncol))
  
  for(i in 2:nrow){
    for(j in 1:ncol){
      if(weight[i - 1] > (j - 1)){
        array[i,j] <- array[i - 1, j]
      }else{
        array[i,j] <- max(array[i - 1,j],array[i - 1,j - weight[i - 1]]+value[i - 1])
      }
    }
  }
  
  # the following code is used to find the best combination for biggest value
  
  i <- nrow
  j <- ncol
  elements <- c()
  
  while(i > 1){
    if(array[i,j] > array[i - 1,j]){
      elements <- append(elements,i - 1)
      j <- j - weight[i - 1]
      i <- i - 1
      
    }else{
      i <- i - 1
      
    }
  }
  the_biggest_value <- array[nrow,ncol]
  return(list(value = the_biggest_value,elements = elements))
  
  
  
  
}
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
