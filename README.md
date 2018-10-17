
Задачи классификации
 =======================
 Метрические алгоритмы классификации 
------------------------------------------------------------------------

**Метрические методы обучения** - методы, основанные на анализе сходства объектов. Для формализации понятия сходства вводится функция расстояния между объектами ![](https://latex.codecogs.com/gif.latex?%5Cinline%20p%28u%2C%20x_%7Bu%7D%29). Чем меньше расстояние между объектами, тем больше объекты похожи друг на друга. Метрические классификаторы опираются на гипотезу компактности, которая предполагает, что *схожим объектам чаще соответствуют схожие ответы*.

**Опр 1.1.** Метрические алгоритмы классификации с обучающей выборкой ![](https://latex.codecogs.com/gif.latex?%5Cinline%20X%5E%7Bl%7D) относят объект *u* к тому классу *y*, для которого суммарный вес ближайших обучающих объектов максимален:  
![](https://latex.codecogs.com/gif.latex?a%28u%2C%20X%5E%7Bl%7D%29%3D%20%5Carg%20%5Cmax%20%5CGamma%20_%7By%7D%28u%2C%20X%5E%7Bl%7D%29);  
![](https://latex.codecogs.com/gif.latex?%5CGamma%20_%7By%7D%28u%2C%20X%5E%7Bl%7D%29%3D%20%5Csum_%7Bi%3D1%7D%5E%7Bl%7D%5By_%7Bu%7D%5E%7B%28i%29%7D%3D%20y%5D%5Comega%20%28i%2Cu%29),  
где *w(u,i)*-весовая функция, которая оценивает степень важности *i*-го соседа для классификации объекта *u*. Функция ![](https://latex.codecogs.com/gif.latex?%5Cinline%20%5CGamma%20_%7By%7D%28u%2C%20X%5E%7Bl%7D%29) называется оценкой близости объекта *u* к классу *y*. Выбирая различную весовую функцию *w(i, u)* можно получать различные метрические классификаторы.


 Алгоритм k ближайших соседей (kNN)
-------------------------------------
Имеется некоторая выборка ![](https://latex.codecogs.com/gif.latex?%5Cinline%20X%5E%7Bl%7D), состоящая из объектов *x(i), i = 1, ..., l* (допустим, выборка ирисов Фишера, в нашем случае).   
Данный алгоритм классификации относит классифицируемый объект *u* к тому классу *y*, к которому относится большинство из *k* его ближайших соседей.
Алгоритм относит объект *u* к тому классу, который наберёт большее число голосов:  
![](https://latex.codecogs.com/gif.latex?%5Cinline%20a%28u%2C%20X%5E%7Bl%7D%2C%20k%29%3D%20%5Carg%20%5Cmax%20%5Csum_%7Bi%3D1%7D%5E%7Bk%7D%5By_%7Bu%7D%5E%7B%28i%29%7D%3D%20y%5D),  

***Реализация алгоритма: kNN***

(вставить скрины)

### Преимущества:

1. Простота реализации и возможность введения различных модификаций весовой функции *w(i,u)*.
2. При k, подобранном около оптимального, алгоритм "неплохо" классифицирует.
3. "Прецедентная" логика работы алгоритма.

### Недостатки:
1. Приходится хранить всю обучающую выборку целиком.
2. Максимальная сумма объектов в counts может достигаться в нескольких классах одновременно.
3. "Скудный" набор параметров.
4. Точки, расстояние между которыми одинаково, не все будут учитываться.

Алгоритм 1NN
-----------------------------------
Алгоритм ближайшего соседа(1NN) является самым простым алгоритмом клссификации.  
Данный алгоритм классификации относит классифицируемый объект u к тому классу y, к которому относится его ближайший сосед.
Единственное достоинство этого алгоритма - простота реализации.

### Недостатков больше:
-----------
1. Неустойчивость к погрешностям. 
2. Отсутсвие параметров, которые можно было бы настраивать по выборке.
Алгоритм полностью зависит от того, насколько удачно выбрана метрика p(расстояния).
3. В результате - низкое качество классификации.

***Описание кода***:
-------------------
Имеется некоторая выборка Xl, состоящая из объектов x(i), i = 1, ..., l (снова выборка ирисов Фишера). 
Необходимо задать функцию расстояния. В качестве функции расстояния использовать обычное евклидово расстояние.  
Далее необходимо отсортировать объекты согласно этого расстояния до классифицируемого объекта.
Реализация весовой функции производится таким образом:

```diff
 distances <- matrix(NA, l, 2) #расстояния от классифицируемого объекта u до каждого i-го соседа  
	      for (i in 1:l)  
		  {         
			distances[i, ] <- c(i, metricFunction(xl[i, 1:n], point)) # сортировка расстояний
		  }  
	 orderedXl <- xl[order(distances[, 2]), ] # сортировка выборки 
```
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
Реализация алгоритма:
--------------------------------
![](https://raw.githubusercontent.com/sefayehalilova/progect1/branch/path/to/1nn.jpg)
