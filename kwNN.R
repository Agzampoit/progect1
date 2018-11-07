colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1,
     main = "Задача классификации kwNN", xlab = "длина листа", ylab = "ширина листа" )

euclideanDistance <- function(u, v) {
  sqrt(sum((u - v)^2))
}

k <- 6
q <- 0.5 # для начала

xl <- iris[, 3:5]
l <- dim(xl)[1] 
n <- dim(xl)[2] - 1

OY<-seq(from=0.0, to=3.0, by=0.1)## расстояние расположения точек по OY, от 0 до 3 с шагом 0,1
OX<-seq(from=0.0, to=7.0, by=0.1)## расстояние расположения точек по OX, от 0 до 7 с шагом 0,1

for(i in OX) {
  for(j in OY) {
    z <- c(i, j)
    distances <- matrix(NA, l, 2)# расстояния от классифицируемого объекта u до каждого i-го соседа 
    for(p in 1:l) {
      distances[p, ] <- c(p, euclideanDistance(xl[p, 1:n], z))
    }
    orderedxl <- xl[order(distances[ , 2]), ]# сортировка расстояний
    weights <- c(NA)# подсчёт весов для каждого i-го объекта
    for(t in 1:l) {
       weights[t] <- q^t #весовая функция
    }
    orderedxl_weighted <- cbind(orderedxl, weights)# создаем матрицу, которая с помощью cbind объединяет массивы расстояний и весов 
    classes <- orderedxl_weighted[1:k, (n + 1):(n + 2)] # получаем классы с именами первых k ближайших соседей и их веса
    sumSetosa <- sum(classes[classes$Species == "setosa", 2])
    sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
    sumVirginica <- sum(classes[classes$Species == "virginica", 2])
    answer <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                  nrow = 1, ncol = 3, byrow = TRUE, list(c(1), c('setosa', 'versicolor', 'virginica')))#матрица имен классов и их сумм весов, которая заполняется по строкам
    points(z[1], z[2],  pch = 21, col = colors[which.max(answer)], asp=1) #закрашиваем точки в тот цвет класса, чей вес максимальный
  }
}

