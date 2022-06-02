library(olsrr)

# dane "aircraft" z pakietu robustbase ------------------------------------


data(aircraft, package="robustbase")
aircraft


# stworzenie modelu -------------------------------------------------------


model_1 <- lm(data = aircraft, Y~X1+X2+X3+X4)
summary(model_1)

model_2 <- lm(data = aircraft, Y~X1+X3+X4)
summary(model_2)

model_3 <- lm(data = aircraft, Y~X3+X4)
summary(model_3)


### wybieramy model 1 na poziomie istotonosci 0,1. 
### R2 = 0,088 - model bardzo dobrez dopasowany do danych
### Skorygowany R2 - 0,86 - lepsze niø w modelu 3 (0,81)

plot(model_1$residuals,
     main = 'Wykres reszt modelu 1',
     ylab = 'Reszty',
     col = 'cadetblue2', lwd=15)
shapiro.test(model_1$residuals)

# test White'a ------------------------------------------------------------


library(skedastic)

white_lm(model_1)
### p-value wieksze niz 0,05 wiec brak podstaw do odrzucenia H0, czyli wystepuje homoskedastycznosc



# Obliczenie MAE i MSE----------------------------------------------------------

sum_reszt <- sum(abs(model_1$residuals))
n <- 23

MAE <- sum_reszt/n
MAE

kwa_reszt <- sum(model_1$residuals^2)

MSE <- kwa_reszt/n
sqrt(MSE)

# weryfikacja istnienia problemu wspÛ≥liniowoúci --------------------------

# wyniki ols_regress {olsrr}
ols_regress(data = aircraft, Y~X1+X2+X3+X4)


# tolerancja i VIF (Variance Inflation Factor)
ols_vif_tol(model_1)
### dla zadnej zmiennej tolerancja nie jest mniejsza niz 0,1 a wiec nie wystepuje problem z wspolliniowoscia
### dla wszystkich zmiennych wartosci VIF sa ponizej 10



# ocena wspolliniowosci razem
ols_coll_diag(model_1)
### dla wyrzu wolnego, X1, X2 Condition Index ponizej 10, czyli ok
### dla X3, X4 Condition Index <10;30>, wiec nalezy sie przyjrzec


#  Identyfikacja obserwacji odstajπcych, o wysokiej düwigni, wp≥ywowych.------------------------------------------------------------

ols_plot_resid_stand(model_1)

ols_plot_resid_lev(model_1)

plot(cooks.distance(model_1))

plot(model_1, which=5)

ols_plot_cooksd_bar(model_1)









