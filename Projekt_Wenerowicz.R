# Statystyki opisowe ------------------------------------------------------

summary(dane_projekt[2:8])
sapply(dane_projekt[2:8],var)

# Przygotowanie danych do stworzenia modelu ekonometrycznego --------------

# histogramy  

dane <- data.frame(dane_projekt[2:8],
                   row.names = dane_projekt$Wojewodztwo)
library(glue)
Dane_names <- names(dane)
for (i in 1:7) {
  Dane_names[i]
  main_hist <- glue("Histogram zmiennej: '{Dane_names[i]}'")
  hist(dane[,i], 
       main = main_hist,
       xlab = Dane_names[i],
       ylab = "Czestotliwosc wystepowania",
       col = "salmon", 
       border = "black", 
       cex = 0.5)
}

for (i in 1:7) {
  dane[,i]
  b <-shapiro.test(dane[,i])
  print(paste(colnames(dane[i]), " est:", round(b$statistic,3), " p=value:", round(b$p.value,4)))
}
# Sprawdzenie danych ------------------------------------------------------

# wykresy rozrzutu 

library(corrplot)

korelacja <- cor(dane)
for (i in 2:7) {
  dane[,i]
  a <-cor.test(dane$widzowie, dane[,i])
  print(paste(colnames(dane[i]), " est:", round(a$estimate, 3), " p=value:", round(a$p.value, 3)))
}

options(scipen=999)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = 'na.or.complete')
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = if(r>0.5)  2 else 1.5)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.35) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "salmon3", ...)
}
pairs(dane, panel = panel.smooth,
      cex = 1.2, pch = 1, bg = "blue", horOdd=TRUE,
      diag.panel = panel.hist, cex.labels = 1.5, font.labels = 1, lower.panel = panel.cor)

##print(cor.test(dane$widzowie, colnames(dane[,i])))

# Stworzenie pierwszego modelu z 6 zmiennymi objasniajacymi ---------------

model <- lm(data=dane, widzowie~ludnosc+miejsca+teatrinst+przedkon+bezrob+dochod)
summary(model)

# Stworzenie pierwszego modelu z 3 zmiennymi objasniajacymi ---------------

model1 <- lm(data = dane, widzowie~miejsca+przedkon+bezrob)
summary(model1)

#Wartosci teoretyczne
model1$fitted.values

#Reszty
model1$residuals

#Wykresy diagnostyczne
plot(model1$residuals, model1$fitted.values, xlab= "Reszty",
     ylab = "Wartoscci teoretyczne" ,pch=16, main = "Wykres diagnostyczny")

hist(model1$residuals, xlab = "Reszty modelu", ylab = "Czestosc wystepowania",
     col = "salmon", main = "Histogram reszt modelu", border = "black")


# Test Shapiro-Wilka ------------------------------------------------------

shapiro.test(model1$residuals) #h0=normalny, pvalue wysokie - brak podstaw do odrzucenia h0

