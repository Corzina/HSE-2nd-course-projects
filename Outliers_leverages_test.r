#Honework 15

install.packages("haven")
install.packages("ggplot2")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")
install.packages("dplyr")
install.packages("broom")
install.packages("olsrr")
install.packages("memisc")

library(haven)
library(ggplot2)
library(sandwich)
library(lmtest)
library(car)
library(dplyr)
library(broom)
library(olsrr)
library(memisc)

#1

data <- read_dta("hw15.dta")

data <- dplyr::select(data, ch_schools_pc, afreq, nozemstvo, distance_moscow, goodsoil, lnurban, lnpopn, province_capital)
data <- na.omit(data)
head(data)

model1 <-lm(ch_schools_pc ~ afreq + nozemstvo + distance_moscow + goodsoil + lnurban + lnpopn + province_capital, data=data)

anova(model1)
summary(model1)
vcov(model1)

# 2

# 2.1

ggplot(data, aes(afreq, ch_schools_pc)) + geom_point()
ggplot(data, aes(afreq, model1$residuals^2)) + geom_point()

ggplot(data, aes(nozemstvo, ch_schools_pc)) + geom_point()
ggplot(data, aes(nozemstvo, model1$residuals^2)) + geom_point()

ggplot(data, aes(distance_moscow, ch_schools_pc)) + geom_point()
ggplot(data, aes(distance_moscow, model1$residuals^2)) + geom_point()

ggplot(data, aes(goodsoil, ch_schools_pc)) + geom_point()
ggplot(data, aes(lnurban, ch_schools_pc)) + geom_point()
ggplot(data, aes(lnpopn, ch_schools_pc)) + geom_point()
ggplot(data, aes(province_capital, ch_schools_pc)) + geom_point()

# 2.2

bptest(model1)

# 2.3

gqtest(model1, order.by = ~afreq , data = data, fraction = 0.25)
gqtest(model1, order.by = ~nozemstvo , data = data, fraction = 0.25)
gqtest(model1, order.by = ~distance_moscow, data = data, fraction = 0.25)
gqtest(model1, order.by = ~goodsoil, data = data, fraction = 0.25)
gqtest(model1, order.by = ~lnurban, data = data, fraction = 0.25)
gqtest(model1, order.by = ~lnpopn, data = data, fraction = 0.25)
gqtest(model1, order.by = ~province_capital, data = data, fraction = 0.25)

# 2.4

vcovHC(model1)
coeftest(model1, vcov=vcovHC(model1))

# 3

#3.1

car::outlierTest(model1)
car::influencePlot(model1)
car::influenceIndexPlot(model1)

#3.2

ols_plot_cooksd_bar(model1)

#наиболее влиятельные наблюдения 43, 163 и 160

influence_data <- augment(model1) %>% mutate(index = 1:n())

# создадим датасет с отраженными оценками влиятельности наблюдений по мере кука

top_cook <- influence_data %>% top_n(3, .cooksd)
top_cook$index

# вынесем три самых влиятельных наблюдения в отдельное значение и оценим модель без них

model1_obrez <- update(model1, subset = c(-top_cook$index))
mtable(model1, model1_obrez)









# ниже черновики по вебинару с визуализацией

install.packages('stargazer')
library(stargazer)
library(broom)
library(knitr)

stargazer(model1)

stargazer(tidy(mtable(model1, model1_obrez)))

kable(tidy(bptest(model1)), format = 'latex')
