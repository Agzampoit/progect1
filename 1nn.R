colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1 ) ##рисуем выборку по признакам

euclideanDistance <- function(u, v) ##функция расстояний, эвклидово расстояние
{     
	sqrt(sum((u - v)^2)) 
}

xl<-(iris[,3:5]) ## выборка
OY<-c(seq(from=0.0, to=3.0, by=0.1))## расстояние расположения точек по OY, от 0 до 3 с шагом 0,1
OX<-c(seq(from=0.0, to=7.0, by=0.1))## расстояние расположения точек по OX, от 0 до 7 с шагом 0,1

sortObjectsByDist <- function(xl, point, metricFunction = euclideanDistance)## сортировка объектов согласно растояния до объекта point

{
     l <- dim(xl)[1]    
	 n <- dim(xl)[2] - 1 
	 distances <- matrix(NA, l, 2)## создаем матрицу расстояний 
	      for (i in 1:l)  
		  {         
			distances[i, ] <- c(i, metricFunction(xl[i, 1:n], point))## расстояние от каждой точки до точки point, классифицируемой
		  }  
	 orderedXl <- xl[order(distances[, 2]), ]## сортировка выборки
}	
## применяем метод 1NN
NN1 <- function(xl, point) {	  
	 orderedXl <- sortObjectsByDist(xl, point)## сортировка выборки согласно классифицируемого объекта    
	 n <- dim(orderedXl)[2] - 1 
	 class <- orderedXl[1, n + 1] ## получение класса соседа
	 return (class)## возвращаем класс
}

for(i in OX){
	for(j in OY){
	point<-c(i,j)
	class <- NN1(xl, point) 
	points(point[1], point[2], pch = 21, col = colors[class], asp = 1) }## классификация заданного объекта
}
