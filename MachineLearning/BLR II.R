 
vect <- sample(1:20,5)
sorted <- quickSort(vect)
quickSort <- function(vect) {
 
  if(length(vect) <= 1){
    return(vect)    
  }
  
  element <- vect[1]
  rest <- vect[-1]
  v1 <- rest[rest<element]
  v2 <- rest[rest>element]
  
  v1 <- quickSort(v1)
  v2 <- quickSort(v2)
  return(c(v1, element, v2))
}



bv.list <- list(colsSab=c(0.92,0.87,0.90,0.86),
                                  colsE=c(0.97,0.92,1.04,0.96,0.96),
                               colsSO=c(0.91,0.91,0.94,0.85))
