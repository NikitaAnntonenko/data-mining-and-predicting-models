a <- c(1477, 1625, 1035, 1307, 1321, 1630, 1911, 1898, 1920, 1620, 1267, 1145)

hist(a, col = "yellow",
     xlab = "Середньодобовий видобуток за 2015 рік, тон", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(a)[2])))
lines(density(a), col = "red", lwd = 2)

b <- c(1650, 1015, 820, 862, 878, 828, 584, 507, 1087, 1252, 1292, 1355)

hist(b, col = "yellow",
     xlab = "Середньодобовий видобуток за 2016 рік, тон", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(b)[2])))
lines(density(b), col = "red", lwd = 2)

hist(c(a,b), col = "yellow",
     xlab = "Середньодобовий видобуток за 2015-2016 рік, тон", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(c(a,b))[2])))
lines(density(c(a,b)), col = "red", lwd = 2)

rm(list = c("a","b"))