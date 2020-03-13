#라이브러리 호출
library(rpart)

#데이터셋
iris_data <- iris

#seed 생성
set.seed(123)

#샘플 키값 저장
idx <-sample(1:nrow(iris_data),0.7*nrow(iris_data))
idx

#7:3비율로 분리
iris_train <- iris_data[idx,]
iris_predict <- iris_data[-idx,]

#데이터확인
head(iris)
#  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa
#2          4.9         3.0          1.4         0.2  setosa
#3          4.7         3.2          1.3         0.2  setosa
#4          4.6         3.1          1.5         0.2  setosa
#5          5.0         3.6          1.4         0.2  setosa
#6          5.4         3.9          1.7         0.4  setosa


#모델생성
#Species 를 종속변수, ~ 나머지 모든 변수를 독립변수로
#data=iris 데이터를 사용
Treemodel <-  rpart(Species~. , data= iris_train)

#모델확인
Treemodel

#n= 105 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#1) root 105 67 setosa (0.3619048 0.3142857 0.3238095)  
#  2) Petal.Length< 2.6 38  0 setosa (1.0000000 0.0000000 0.0000000) *
#  3) Petal.Length>=2.6 67 33 virginica (0.0000000 0.4925373 0.5074627)  
#    6) Petal.Length< 4.75 29  0 versicolor (0.0000000 1.0000000 0.0000000) *
#    7) Petal.Length>=4.75 38  4 virginica (0.0000000 0.1052632 0.8947368) *


#결과를 도표로 생성
plot(Treemodel, margin=.1)
text(Treemodel, cex=1)

#predict  데이터로 예측하기
predict(Treemodel, newdata=iris_predict, type = "class")

         2          4         10         13         19         21 
    setosa     setosa     setosa     setosa     setosa     setosa 
        22         31         32         41         42         49 
    setosa     setosa     setosa     setosa     setosa     setosa 
        51         52         58         59         62         63 
versicolor versicolor versicolor versicolor versicolor versicolor 
        67         72         76         78         81         84 
versicolor versicolor versicolor  virginica versicolor  virginica 
        88         91         96         98         99        102 
versicolor versicolor versicolor versicolor versicolor  virginica 
       103        105        107        109        110        117 
 virginica  virginica versicolor  virginica  virginica  virginica 
       120        125        129        133        136        140 
 virginica  virginica  virginica  virginica  virginica  virginica 
       141        144        149 
 virginica  virginica  virginica 
Levels: setosa versicolor virginica

#성능평가
library(gmodels)
predict_result <- predict(Treemodel, newdata=iris_predict, type = "class")
CrossTable(x = iris_predict$Species, y=predict_result, prop.chisq = F)

                     | predict_result 
iris_predict$Species |     setosa | versicolor |  virginica |  Row Total | 
---------------------|------------|------------|------------|------------|
              setosa |         12 |          0 |          0 |         12 | 
                     |      1.000 |      0.000 |      0.000 |      0.267 | 
                     |      1.000 |      0.000 |      0.000 |            | 
                     |      0.267 |      0.000 |      0.000 |            | 
---------------------|------------|------------|------------|------------|
          versicolor |          0 |         15 |          2 |         17 | 
                     |      0.000 |      0.882 |      0.118 |      0.378 | 
                     |      0.000 |      0.938 |      0.118 |            | 
                     |      0.000 |      0.333 |      0.044 |            | 
---------------------|------------|------------|------------|------------|
           virginica |          0 |          1 |         15 |         16 | 
                     |      0.000 |      0.062 |      0.938 |      0.356 | 
                     |      0.000 |      0.062 |      0.882 |            | 
                     |      0.000 |      0.022 |      0.333 |            | 
---------------------|------------|------------|------------|------------|
        Column Total |         12 |         16 |         17 |         45 | 
                     |      0.267 |      0.356 |      0.378 |            | 
---------------------|------------|------------|------------|------------|


