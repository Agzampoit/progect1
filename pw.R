euclideanDistance <- function(u, v)
{
  return (sqrt(sum((u - v)^2)))
}

core1 = function(z){
  return ((0.5 * (abs(z) <= 1) )) #функция прямоугольного ядра
}
core2 = function(z){
  return ((1 - abs(z)) * (abs(z) <= 1)) #функция для треугольного ядра
}
core3 = function(z){
  return ((15 / 16) * (1 - z ^ 2) ^ 2 * (abs(z) <= 1)) #функция для квартического ядра
}
core4 = function(z){
  return ((3/4*(1-z^2)*(abs(z)<=1))) #функция для ядра Епанечникова
}
core5 = function(z){
  (((2*pi)^(-1/2)) * exp(-1/2*z^2)) #функция для Гауссовского ядра
}

PW <- function(xl,point, h)
{
    weight <- matrix(NA, l, 2) #матрица расстояний и весов
    for (p in 1:l) {
      weight[p, 1] <- euclideanDistance(xl[p, 1:n], point) # расстояния от классифицируемого объекта u до каждого i-го соседа
      z <- weight[p, 1] / h # аргумент функции ядра
      cores <- c(core1(z), core2(z), core3(z), core4(z), core5(z)) #функции ядер
      
      weight[p, 2] <- cores[2] # подсчёт веса для треугольного ядра
    }
    
    classes <- data.frame(weight[ , 1], weight[ , 2], xl[ , 3]) # таблица данных названий расстояний, ядер и классов 
    colnames(classes) <- c("Distances", "Weights", "Species")
    
    sumSetosa <- sum(classes[classes$Species == "setosa", 2])
    sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
    sumVirginica <- sum(classes[classes$Species == "virginica", 2])
    answer <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                     nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica')))
    if(answer[1,1]==0&&answer[1,2]==0&&answer[1,3]==0){
      class='white'
    }
    else{
      class = colors[which.max(answer)]
    }
    return(class)
} 

map <- function(xl,h)
{
OY<-seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.1)
OX<-seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.1)

for(i in OY) {
  for(j in OX) {
    point <- c(i, j)
    class <- PW(xl,point,h)
    points(point[1],point[2], pch=21, col= colors[class], asp=1)
  }
}
}

