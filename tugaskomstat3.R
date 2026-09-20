data("airquality")
str(airquality)

dens <- density(airquality$Wind)
hist(airquality$Wind, probability = TRUE, col = "#ADD8E6", border = "white",
     ylim = c(0,max(dens$y)),
     xlab = "kecepatan angin (mph)", main = "Histogram dan Density Wind")
lines(dens, col = "red", lwd = 2)

boxplot(airquality$Wind, horizontal = TRUE, col = "lightgreen",
        main = "Boxplot Wind", xlab = "Kecepatan angin (mph)")
stem(airquality$Wind)

plot(Wind ~ Temp, data = airquality, pch = 19, col = "steelblue",
     xlab = "Suhu (F)", ylab = "Kecepatan angin (mph)",
     main = "scatter plot suhu dengan kecepatan angin")
rug(airquality$Temp); rug(airquality$Wind, side = 2)
abline(lm(Wind ~ Temp, data = airquality), col = "red", lwd = 2, lty = 2)

