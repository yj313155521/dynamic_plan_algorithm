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
  nrow_1 <- nrow(x) + 1
  ncol_1 <- W + 1
  dp_array <- array(0,dim = c(nrow_1,ncol_1))
  
  for(i in 2:nrow_1){
    for(j in 1:ncol_1){
      if(weight[i - 1] > (j - 1)){
        dp_array[i,j] <- dp_array[i - 1, j]
      }else{
        dp_array[i,j] <- max(dp_array[i - 1,j],dp_array[i - 1,j - weight[i - 1]]+value[i - 1])
      }
    }
  }
  
# the following code is used to find the best combination for biggest value
  
  i <- nrow_1
  j <- ncol_1
  elements_1 <- c()
  
  while(i > 1){
    if(dp_array[i,j] > dp_array[i - 1,j]){
      elements_1 <- append(elements_1,i - 1)
      j <- j - weight[i - 1]
      i <- i - 1
      
    }else{
      i <- i - 1
      
    }
  }
  the_biggest_value <- dp_array[nrow_1,ncol_1]
  return(list(value = the_biggest_value,elements = elements_1))
  
  
  
  
}
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)




