# Евклидово расстояние
euclideanDistance <- function(u, v)
{
sqrt(sum((u - v)^2))
}

## Сортируем объекты согласно расстояния до объекта z
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) ## задаем функцию расстояния
{
l <- dim(xl)[1]
n <- dim(xl)[2] - 1

distances <- matrix(NA, l, 2)## задаем матрицу расстояния
for (i in 1:l)
{
distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z)) ## считаем расстояние от классифицируемой точки до остальных точек выборки
}

orderedXl <- xl[order(distances[, 2]), ] ##сортируем
return (orderedXl);
}

##Применяем метод kNN
kNN <- function(xl_1, z, k)
xl <- iris_new[, 3:5]
{

orderedXl <- sortObjectsByDist(xl, z) ## Сортируем выборку согласно классифицируемого объекта
n <- dim(orderedXl)[2] - 1

classes <- orderedXl[1:k, n + 1] ## Получаем классы первых k соседей
counts <- table(classes) ## Составляем таблицу встречаемости каждого класса
class <- names(which.max(counts)) ## Находим класс, который доминирует среди первых соседей
return (class) ## возвращаем класс
}

plot(NULL, NULL, type = "l", xlim = c(0, 150), ylim = c(0, 1), xlab = 'k', ylab = 'LOO') #график зависимости
Ox <- seq(from = 1, to = 150, by = 5) # ось OX с длинной от 1 до 150 с шагом 5
Oy <- c() # LOO по оси OY

#устанавливаем оптимальные значения loo и k
LOO_opt <- 1
k_opt <- 1

for(k in Ox) {
  R <- 0 #заводим переменную с начальным значением 0, в которую будут накапливаться ошибки
  for(i in 1:l) {
    iris_new <- iris[-i, ] # наша выборка без i-го элемента
    z <- iris[i, 3:4]
    if(knn(iris_new, z, k) != iris[i, 5]) { #если алгоритм ошибся
      R <- R + 1 #тогда ошибка увеличивается
    } 
  }
#после того как мы перебрали все элементы, считаем loo
  LOO <- R/l #где R накопитель ошибки, а l-количество элементов выборки
  Oy <- c(Oy, LOO)
  
#Если найденное loo оказалось меньше изначального оптимального значения, то сделаем это новое значение оптимальным и соотвественно k тоже оптимально
  if(LOO < LOO_opt) {
    LOO_opt <- LOO
    k_opt <- k
  }
}

print(Ox)
print(Oy)
print(k_opt)

lines(Ox, Oy, pch = 8, bg = "black", col = "blue")
points(k_opt, LOO_opt, pch = 22, bg = "black", col = "black")