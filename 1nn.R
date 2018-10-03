colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1 ) 

euclideanDistance <- function(u, v)
{     
	sqrt(sum((u - v)^2)) 
}

xl<-(iris[,3:5])
OY<-c(seq(from=0.0, to=3.0, by=0.1))
OX<-c(seq(from=0.0, to=7.0, by=0.1))

sortObjectsByDist <- function(xl, point, metricFunction = euclideanDistance)имиьтт

{
     l <- dim(xl)[1]    
	 n <- dim(xl)[2] - 1 
	 distances <- matrix(NA, l, 2) 
	      for (i in 1:l)  
		  {         
			distances[i, ] <- c(i, metricFunction(xl[i, 1:n], point))
		  }  
	 orderedXl <- xl[order(distances[, 2]), ]
}	

NN1 <- function(xl, point) {	  
	 orderedXl <- sortObjectsByDist(xl, point)     
	 n <- dim(orderedXl)[2] - 1 
	 class <- orderedXl[1, n + 1] 
	 return (class)
}

for(i in OX){
	for(j in OY){
	point<-c(i,j)
	class <- NN1(xl, point) 
	points(point[1], point[2], pch = 21, col = colors[class], asp = 1) }
}
