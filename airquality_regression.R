head(airquality)
#  Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2
#3    12     149 12.6   74     5   3
#4    18     313 11.5   62     5   4
#5    NA      NA 14.3   56     5   5
#6    28      NA 14.9   66     5   6

#데이터셋카피
airquality2 <- airquality

#결측치있는 레코드 제거
airquality2 <- na.omit(airquality2)

#최종 레코드수 확인
nrow(airquality)
#[1] 153

nrow(airquality2)
#[1] 111

#모델생성
model <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality2)

#모델확인
summary(model)

#Call:
#lm(formula = Ozone ~ Solar.R + Wind + Temp, data = airquality2)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-40.485 -14.219  -3.551  10.097  95.619 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -64.34208   23.05472  -2.791  0.00623 ** 
#Solar.R       0.05982    0.02319   2.580  0.01124 *  
#Wind         -3.33359    0.65441  -5.094 1.52e-06 ***
#Temp          1.65209    0.25353   6.516 2.42e-09 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 21.18 on 107 degrees of freedom
#Multiple R-squared:  0.6059,	Adjusted R-squared:  0.5948 
#F-statistic: 54.83 on 3 and 107 DF,  p-value: < 2.2e-16