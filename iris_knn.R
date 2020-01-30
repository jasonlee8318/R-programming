#iris data 확인
head(iris)

#결과리턴
#  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa
#2          4.9         3.0          1.4         0.2  setosa
#3          4.7         3.2          1.3         0.2  setosa
#4          4.6         3.1          1.5         0.2  setosa
#5          5.0         3.6          1.4         0.2  setosa
#6          5.4         3.9          1.7         0.4  setosa

#iris data row확인
nrow(iris)

#결과리턴
#[1] 150

#정규화함수생성
min_max_normal <- function(x){
     num <- x  - min(x)
     m_n <- max(x) - min(x)
     print(num/m_n)
     #return(num/m_n)
 }

#정규화된 데이터 셋 구성
normal_iris <- as.data.frame(lapply(iris[1:4], min_max_normal)) 

#데이터 셋 확인
head(normal_iris)

#  Sepal.Length Sepal.Width Petal.Length Petal.Width
#1   0.22222222   0.6250000   0.06779661  0.04166667
#2   0.16666667   0.4166667   0.06779661  0.04166667
#3   0.11111111   0.5000000   0.05084746  0.04166667
#4   0.08333333   0.4583333   0.08474576  0.04166667
#5   0.19444444   0.6666667   0.06779661  0.04166667
#6   0.30555556   0.7916667   0.11864407  0.12500000


#species 열과 결합
final_normal_iris <- data.frame(normal_iris, Species= iris$Species)

#test data 셋 구성 (1-120행)
normal_iris_train <- final_normal_iris[1:120,]

#predict data 셋 구성 (121-150행)
normal_iris_predict <- final_normal_iris[121:150,]


#KNN 라이브러리호출
library(class)

#KNN 모델생성(k=3으로 설정)
#파라메타: train에는 학습 데이터 셋을 입력, test에는 예측 데이터 셋을 입력, cl 에는 학습데이터의 예측값 (y)값을 입력함.
model <- knn(train=normal_iris_train[,-5], test=normal_iris_predict[,-5],cl=normal_iris_train$Species, k=3)

#모델확인
summary(model)


#모델평가
library(gmodels)
CrossTable(x = normal_iris_predict$Species, y=model, prop.chisq = F)

#                            | model 
#normal_iris_predict$Species | versicolor |  virginica |  Row Total | 
#----------------------------|------------|------------|------------|
#                  virginica |          4 |         26 |         30 | 
#                            |      0.133 |      0.867 |            | 
#----------------------------|------------|------------|------------|
#               Column Total |          4 |         26 |         30 | 
#----------------------------|------------|------------|------------|