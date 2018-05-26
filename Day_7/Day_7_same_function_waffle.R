#                         the function is not finished yet
#                         the function is not finished yet
#                         the function is not finished yet
waf <- function(val, n_row = NULL){
  require(RColorBrewer)
  if(!is.numeric(val)) stop("'val' must be numeric vector", call. = FALSE)
  if(is.null(n_row)){
    if(sum(val) < 300){
      n_row <- ceiling(sum(val)/(2/3)/35)#what is it?
    } else {
      n_row <- floor(sum(val)/(2/3)/60)#what is it?
    }
  }
  n_col <- ceiling(sum(val)/n_row)
  x <- 0:(n_col + 1)
  y <- 0:(n_row + 1)
  print(x)#look output
  print(y)#look output
  co <- vector('character', 0L)
  col_vec <- brewer.pal(8,name = 'Set2')
  for(i in seq_along(val)){
    y <- rep(col_vec[i], val[i])
    co <- c(co, y)
  }
  col_matrix <- matrix(c(co, rep('white', n_row - (sum(val) %% n_row))), ncol = n_row)
  print(col_matrix)#look output
  print(dim(col_matrix))#look output
  #plot
  plot(1:(dim(col_matrix)[2] + 1), 1:(dim(col_matrix)[2] + 1), type = 'n')
  for(j in 0:(dim(col_matrix)[2] - 1)){
    rect(xleft = j + 1, ybottom = 1, xright = j + 1.95, ytop = 1.95, border = 'white')
    for(i in 1:(dim(col_matrix)[1])){
      rect(xleft = 1 + j, ybottom = i, xright = 1 + 0.95 + j, ytop = i + 0.95, 
           col = col_matrix[i, j + 1],  border = 'white')
    }
  }
}

#test
waf(c(40, 5), n_row = 8)