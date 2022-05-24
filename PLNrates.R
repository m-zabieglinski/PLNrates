getNBPRates <- function(year){
  #pobieramy tutaj pliki CSV ze strony NBP (bo finalnie chcemy wykres od 2013, a CSV prosciej niz xls)
  file0 <- paste("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_", year,".csv", sep = "")
  d0 <- read.csv(file = file0, sep = ';')
  
  #obrobka do odp. formatu
  d0 <- d0[, c('data', 'X1USD', 'X1EUR')]
  d0 <- d0[-1,]
  d0 <- d0[1:(dim(d0)[1]-3),]
  rownames(d0) <- 1:nrow(d0)
  for (row in 1:nrow(d0))
  {
    d0[row,2] <- gsub(",", ".", d0[row,2])
    d0[row,3] <- gsub(",", ".", d0[row,3])
  }
  colnames(d0) <- c("date", "usd", "eur")
  d0$eur <- as.numeric(d0$eur)
  d0$usd <- as.numeric(d0$usd)
  d0$date <- as.Date(d0$date, format = "%Y%m%d")
  return(d0)
}

#lista ramek do scalenia
nbp_list <- c()
for (x in 2013:2020)
{
  d1 <- getNBPRates(x)
  nbp_list <- append(nbp_list, list(d1))
}

#scalanie dplyrem
library(dplyr)
ratesTotal <- bind_rows(nbp_list)

#wektor linii pionowych na rysunku
v_al = c(
  as.numeric(as.Date("2014-01-01", "%Y-%m-%d")),
  as.numeric(as.Date("2016-01-01", "%Y-%m-%d")),
  as.numeric(as.Date("2018-01-01", "%Y-%m-%d")),
  as.numeric(as.Date("2020-01-01", "%Y-%m-%d"))
  )

#malowanie - zakladam, ze euro sa zawsze drozsze od dolarow, zeby nie szukac minimow w dwoch kolumnach
par(mar = c(4,5,1,1), las = 1)
plot(ratesTotal$date, ratesTotal$usd, type = "l", ylab = "Currency rate in PLN", xlab = "Date",
     ylim = c(min(ratesTotal[,2]), max(ratesTotal[,3])),
     col = "red", lwd = 1.7,
     panel.first =
       c(
         abline(h = c(3, 3.5, 4, 4.5), lty = "solid", col = "lightgrey"),
         abline(v = v_al, lty = "solid", col = "lightgrey")
       )
)
lines(ratesTotal$date, ratesTotal$eur, col = "blue", lwd = 1.7)
legend("bottomright", legend = c("EUR", "USD"), lwd = 2, col = c("blue", "red"))