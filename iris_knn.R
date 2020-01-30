#iris data Ȯ��
head(iris)

#�������
#  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa
#2          4.9         3.0          1.4         0.2  setosa
#3          4.7         3.2          1.3         0.2  setosa
#4          4.6         3.1          1.5         0.2  setosa
#5          5.0         3.6          1.4         0.2  setosa
#6          5.4         3.9          1.7         0.4  setosa

#iris data rowȮ��
nrow(iris)

#�������
#[1] 150

#����ȭ�Լ�����
min_max_normal <- function(x){
     num <- x  - min(x)
     m_n <- max(x) - min(x)
     print(num/m_n)
     #return(num/m_n)
 }

#����ȭ�� ������ �� ����
normal_iris <- as.data.frame(lapply(iris[1:4], min_max_normal)) 

#������ �� Ȯ��
head(normal_iris)

#  Sepal.Length Sepal.Width Petal.Length Petal.Width
#1   0.22222222   0.6250000   0.06779661  0.04166667
#2   0.16666667   0.4166667   0.06779661  0.04166667
#3   0.11111111   0.5000000   0.05084746  0.04166667
#4   0.08333333   0.4583333   0.08474576  0.04166667
#5   0.19444444   0.6666667   0.06779661  0.04166667
#6   0.30555556   0.7916667   0.11864407  0.12500000


#species ���� ����
final_normal_iris <- data.frame(normal_iris, Species= iris$Species)

#test data �� ���� (1-120��)
normal_iris_train <- final_normal_iris[1:120,]

#predict data �� ���� (121-150��)
normal_iris_predict <- final_normal_iris[121:150,]


#KNN ���̺귯��ȣ��
library(class)

#KNN �𵨻���(k=3���� ����)
#�Ķ��Ÿ: train���� �н� ������ ���� �Է�, test���� ���� ������ ���� �Է�, cl ���� �н��������� ������ (y)���� �Է���.
model <- knn(train=normal_iris_train[,-5], test=normal_iris_predict[,-5],cl=normal_iris_train$Species, k=3)

#��Ȯ��
summary(model)


#����
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