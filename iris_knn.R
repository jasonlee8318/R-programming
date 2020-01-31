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


#학습, 검정 데이터로 분리
#seed 생성
set.seed(123)

#샘플 키값 저장
idx <-sample(1:nrow(final_normal_iris),0.7*nrow(final_normal_iris))
idx

#  [1]  44 118  61 130 138   7  77 128  79  65 134  64  94 142  14 122  33   6 150 126 116  90  82 127  83  89  68  74  36
# [30]  18 147 108 143 146   3  55  87  25 135  26  16  46  45  40  17  15 113  48  28 114   5 132 137  12  54  20  97  71
# [59] 131  35  60   9  34  24  93  39  69 124  66 112 148  50  56   1  37 106  29 119 111   8 121  47 123  53 145  92 139
# [88]  57 115  11  86  85  95  38  70  80  43 100 104  27  30  75 101  73  23

#7:3비율로 분리
normal_iris_train <- final_normal_iris[idx,]
normal_iris_predict <- final_normal_iris[-idx,]

#분리되 데이터 row 확인
nrow(normal_iris_train)
#[1] 105

nrow(normal_iris_predict)
#[1] 45

#KNN 라이브러리호출
library(class)

#KNN 모델생성(k=3으로 설정)
#파라메타: train에는 학습 데이터 셋을 입력, test에는 예측 데이터 셋을 입력, cl 에는 학습데이터의 예측값 (y)값을 입력함.
model <- knn(train=normal_iris_train[,-5], test=normal_iris_predict[,-5],cl=normal_iris_train$Species, k=3)

#모델확인
summary(model)

#    setosa versicolor  virginica 
#        12         18         15

#모델평가
library(gmodels)
CrossTable(x = normal_iris_predict$Species, y=model, prop.chisq = F)

#                            | model 
#normal_iris_predict$Species |     setosa | versicolor |  virginica |  Row Total | 
#----------------------------|------------|------------|------------|------------|
#                     setosa |         12 |          0 |          0 |         12 | 
#                            |      1.000 |      0.000 |      0.000 |      0.267 | 
#                            |      1.000 |      0.000 |      0.000 |            | 
#                            |      0.267 |      0.000 |      0.000 |            | 
#----------------------------|------------|------------|------------|------------|
#                 versicolor |          0 |         16 |          1 |         17 | 
#                            |      0.000 |      0.941 |      0.059 |      0.378 | 
#                            |      0.000 |      0.889 |      0.067 |            | 
#                            |      0.000 |      0.356 |      0.022 |            | 
#----------------------------|------------|------------|------------|------------|
#                  virginica |          0 |          2 |         14 |         16 | 
#                            |      0.000 |      0.125 |      0.875 |      0.356 | 
#                            |      0.000 |      0.111 |      0.933 |            | 
#                            |      0.000 |      0.044 |      0.311 |            | 
#----------------------------|------------|------------|------------|------------|
#               Column Total |         12 |         18 |         15 |         45 | 
#                            |      0.267 |      0.400 |      0.333 |            | 
#----------------------------|------------|------------|------------|------------|