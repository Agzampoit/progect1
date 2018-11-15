
Задачи классификации
 =======================
 Метрические алгоритмы классификации 
------------------------------------------------------------------------

**Метрические методы обучения** - методы, основанные на анализе сходства объектов. Для формализации понятия сходства вводится функция расстояния между объектами ![](http://latex.codecogs.com/gif.latex?p(x_1,x_2)). Чем меньше расстояние между объектами, тем больше объекты похожи друг на друга. Метрические классификаторы опираются на гипотезу компактности, которая предполагает, что *схожим объектам чаще соответствуют схожие ответы*.

**Опр 1.1.** Метрические алгоритмы классификации с обучающей выборкой ![](https://latex.codecogs.com/gif.latex?%5Cinline%20X%5E%7Bl%7D) относят классифицируемый объект *u* к тому классу *y*, для которого суммарный вес ближайших обучающих объектов максимален.  

Ниже представлена таблица, в которой сравниваются разные алгоритмы классификации:  


First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell


Алгоритм k ближайших соседей (kNN)
-------------------------------------
Данный алгоритм классификации относит классифицируемый объект *z* к тому классу *y*, к которому относится большинство из *k* его ближайших соседей.    
Имеется некоторая выборка ![](https://latex.codecogs.com/gif.latex?%5Cinline%20X%5E%7Bl%7D), состоящая из объектов *x(i), i = 1, ..., l* (например, выборка ирисов Фишера), и класифицируемый объект, который обозначим *z*. Чтобы посчитать расстояние от классифицируемого объекта до остальных точек, нужно использовать функцию расстояния(Евклидово расстояние).  
Далее отсортируем объекты согласно посчитаного расстояния до объекта *z*:  
```diff
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) ## задаем функцию расстояния
 {
    distances <- matrix(NA, l, 2)## задаем матрицу расстояния
     for (i in 1:l)
       {
        distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z)) ## считаем расстояние от классифицируемой точки до остальных точек выборки
       }

 orderedXl <- xl[order(distances[, 2]), ] ##сортируем согласно посчитаного расстояния
 return (orderedXl);
 }
```
Применяем сам метод kNN, то есть создаем функцию, которая сортирует выборку согласно нашего классифицируемого объекта *z* и относит его к классу ближайших соседей:
```diff
kNN <- function(xl, z, k)
{
 orderedXl <- sortObjectsByDist(xl, z) ## Сортируем выборку согласно классифицируемого объекта
 n <- dim(orderedXl)[2] - 1

 classes <- orderedXl[1:k, n + 1] ## Получаем классы первых k соседей
 counts <- table(classes) ## Составляем таблицу встречаемости каждого класса
 class <- names(which.max(counts)) ## Находим класс, который доминирует среди первых соседей
 return (class) ## возвращаем класс
 }

```
 В конце задаем точку *z*, которую нужно классфицировать(ее координаты, выборку и тд).  

Ниже представлен итог работы алгоритма при *k=6*.  
![knn](https://user-images.githubusercontent.com/43229815/48431971-23601980-e784-11e8-9b3a-f1736d24173b.png)   
[kNN](https://github.com/sefayehalilova/progect1/blob/master/knn.R)

**Скользящий контроль(leave-one-out) для knn**  
   
LeaveOneOut (или LOO) - простая перекрестная проверка, которая необходима, чтобы оценить при каких значениях k алгоритм knn оптимален, и на сколько он ошибается.  
Нужно проверить, как часто будет ошибаться алгоритм, если по одному выбирать элементы из обучающей выборки.  
*Алгоритм состоит в следующем*: извлечь элемент, обучить оставшиеся элементы, классифицировать извлеченный, затем вернуть его обратно. Так нужно поступить со всеми элементами выборки.  
Если классифицируемый объект xi не исключать из обучающей выборки, то ближайшим соседом xi всегда будет сам xi, и минимальное (нулевое) значение функционала LOO(k) будет достигаться при k = 1.  
Реализация самого LOO в коде выглядит так:  
```diff

for(k in Ox) {
  R <- 0 
  for(i in 1:l) {
    iris_new <- iris[-i, ] 
    z <- iris[i, 3:4]
    if(knn(iris_new, z, k) != iris[i, 5]) { 
      R <- R + 1 
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

```

*График зависимости LOO от k*:  
![loo](https://user-images.githubusercontent.com/43229815/48148795-adfbd100-e2cb-11e8-9c79-b74f736a31bd.png)  

k оптимальное (k_opt) достигается при минимальном LOO (оптимальное k равно 6).  

Алгоритм 1NN
-----------------------------------
Алгоритм ближайшего соседа(1NN) является самым простым алгоритмом клссификации.  
Данный алгоритм классификации относит классифицируемый объект u к тому классу y, к которому относится его ближайший сосед.
Для начала нужно задать метрическую функцию, затем нужно отсортировать выборку по расстояниям от точек до классфицируемой точки, после чего классифицируемый элемент относим к классу, к которому принадлежит ближайший элемент(первый в отсортированной выборке).

Имеется некоторая выборка Xl, состоящая из объектов x(i), i = 1, ..., l (снова выборка ирисов Фишера). Как и в задаче kNN, задаем функцию расстояния, считаем расстояния от классифицируемой точки до остальных точек выборки, сортируем эти расстояния.  


Далее применяем метод 1NN и классифицируем данный объект, то есть найти ближайший объект выборки и вернуть класс, к которому принадлежит наш объект:
```diff
# применяем метод 1NN
NN1 <- function(xl, point) {	  
	  orderedXl <- sortObjectsByDist(xl, point) # сортировка выборки согласно классифицируемого объекта    
	  n <- dim(orderedXl)[2] - 1 
	  class <- orderedXl[1, n + 1] # получение класса соседа
	  return (class) # возвращаем класс
}

for(i in OX){
	for(j in OY){
	  point<-c(i,j)
	  class <- NN1(xl, point) 
	  points(point[1], point[2], pch = 21, col = colors[class], asp = 1) } # классификация заданного объекта
}
```
Ниже представлен итог работы данного алгоритма:
![1nn](https://user-images.githubusercontent.com/43229815/48433796-1691f480-e789-11e8-8c17-54f60a593be9.png)   

[1NN](https://github.com/sefayehalilova/progect1/blob/master/1nn.R)  

Алгоритм k взвешенных ближайших соседей (kwNN)
----------------------------------------------------------------------  
  Имеется некоторая выборка Xl, состоящая из объектов x(i), i = 1, ..., l ( выборка ирисов Фишера). Данный алгоритм классификации относит объект u к тому классу y, у которого максимальна сумма весов w_i его ближайших k соседей x(u_i), то есть объект относится к тому классу, который набирает больший суммарный вес среди k ближайших соседей.  
  В алгоритме kwNN используется весовая функция, которая оценивает степень важности при классификации заданного элемента к какому-либо классу, что и отличает его от алгоритма kNN.  
  kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улучшая качество классификации.  
  Весовая функция kwNN выглядит следующим образом:  
  ```diff
for(i in 1:l) {
       weights[i] <- q^i
    }
```    
Сама функция выглядит так:  
  ```diff
distances <- matrix(NA, l, 2)# расстояния от классифицируемого объекта u до каждого i-го соседа 
    for(p in 1:l) {
      distances[p, ] <- c(p, euclideanDistance(xl[p, 1:n], z))
    }
    orderedxl <- xl[order(distances[ , 2]), ]# сортировка расстояний
    
    weights <- c(NA)# подсчёт весов для каждого i-го объекта
    for(i in 1:l) {
       weights[i] <- q^i 
    }
    orderedxl_weighted <- cbind(orderedxl, weights)
    classes <- orderedxl_weighted[1:k, (n + 1):(n + 2)] # названия первых k ближайших соседей и их веса
    sumSetosa <- sum(classes[classes$Species == "setosa", 2])
    sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
    sumVirginica <- sum(classes[classes$Species == "virginica", 2])
    answer <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                  nrow = 1, ncol = 3, byrow = TRUE, list(c(1), c('setosa', 'versicolor', 'virginica')))#матрица имен классов и их сумм весов, которая заполняется по строкам
points(z[1], z[2], pch = 21, col = colors[which.max(answer)], asp=1) #закрашиваем точки в тот цвет класса, чей вес максимальный
```    
Результат работы алгоритма:  
![kwnn](https://user-images.githubusercontent.com/43229815/48442083-3253c580-e79e-11e8-879e-847b926014df.png)  
*Ниже представлен график зависимости LOO от q*:  
![wknnloo](https://user-images.githubusercontent.com/43229815/48499976-cd09de00-e84a-11e8-918d-baa683dad670.png)  
Оптимальное q достигается при минимальном LOO(оптимальное q равно 0.5).  
Результат алгоритма kwNN лучше, чем результат алгоритма kNN, так как алгоритм kwNN учитывает порядок соседей классифицируемого объекта.





  
  
 

