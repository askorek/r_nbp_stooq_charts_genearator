library(psych)        
library(e1071)  
library(vioplot)

# ----- W R NIE MA GOTOWEJ FUNKCJI DLA MODY -------------------------
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---------------- WPROWADZANIE DAT I WYBÓR TICKERA -----------------
cat("Prosze podac zakres czasowy notowan w formacie YYYY-MM-DD")
while(TRUE) {
  date_begin = as.Date(readline("podaj date poczatkowa YYYY-MM-DD: "))
  
  if (date_begin < as.Date('2001-01-01')){
    cat('Data poczatkowa jest nieprawidlowa, program obsluguje notowania od roku 2001')
  }
  else {
    break
  }
}

# -----------------------------------------------------------------------
while(TRUE) {
  date_end = as.Date(readline("podaj date koncowa: "))
  
  if (date_end > Sys.Date()){
    print('Data koncowa jest nieprawidlowa, program nie obsluguje przyszlych dat')
  }
  else {
    break
  }
}

# --------------------------------------------------------------------------------
while(TRUE) {
  nbp_or_gpw = tolower(readline("aby uzyskac notowania walut wpisz 'W', aby uzyskac notowania gieldy wpisz 'G'"))
  if (nbp_or_gpw != 'w' && nbp_or_gpw != 'g'){
    print(nbp_or_gpw)
    print('Wpisano nieprawidłową wartosc, sprobuj jeszcze raz')
  }
  else {
    break
  }
}

# ----------------------------------------------------------------------------------------
ticker = toupper(readline("Podaj skrot waluty/notowania, ktorego statystyki chcesz uzyskac: "))

#ticker = 'wig'
#date_begin = as.Date("2010-11-11")
#date_end = as.Date("2012-12-11")
#nbp_or_gpw = 'g'

# ------------------------------------------------------------------------------------------

all_data = c() #tutaj trafia wszystkie dane ze strony
all_time = c() #tutaj dodatkowo zbieramy czas notowan

# ---------------- POBIERANIE DANYCH NBP ---------------------
download_file_folder = getwd() #ustawienie folderu do pobierania plików z danymi

if (nbp_or_gpw == 'w') {
  dest_file=paste(download_file_folder,'/',"data.json", sep='')

    days_difference = as.numeric(difftime(date_end, date_begin, units="days")) #nie da sie sciagnac wiecej niz 1 rok na raz, tutaj sprawdzam ile dni

  if (days_difference < 366){ #jeśli mniej niż rok to po prostu sciagnij za jednym razem
    url = paste("http://api.nbp.pl/api/exchangerates/rates/a/", ticker, "/", date_begin, "/", date_end , "/?format=json", sep='')
    download.file(url, dest_file,quiet = FALSE, mode = "wb")
    json_data = fromJSON("data.json") #wczytaj sciagniete dane
    all_data = json_data$rates[3]$mid
    all_time = json_data$rates[2]$effectiveDate
  }
  else {
    current_begin = date_begin
    current_end = date_begin +366 #pobieranie max 1 rok na raz
    while(current_end < date_end) {
      url = paste("http://api.nbp.pl/api/exchangerates/rates/a/", ticker, "/", current_begin, "/", current_end , "/?format=json", sep ='')
      download.file(url, dest_file,quiet = FALSE, mode = "wb")
      current_begin = current_begin + 367
      current_end = current_end + 367
      json_data = fromJSON("data.json") #wczytaj sciagniete dane
      all_data = c(all_data, json_data$rates[3]$mid)
      all_time = c(all_time, json_data$rates[2]$effectiveDate)
    }
    
    url = paste("http://api.nbp.pl/api/exchangerates/rates/a/", ticker, "/", current_begin, "/", date_end , "/?format=json", sep ='')
    download.file(url, dest_file,quiet = FALSE, mode = "wb")
    json_data = fromJSON("data.json") #wczytaj sciagniete dane
    all_data = c(all_data, json_data$rates[3]$mid)
    all_time = c(all_time, json_data$rates[2]$effectiveDate)
    
  }
}

# ---------------- POBIERANIE DANYCH STOOQ -------------------
if (nbp_or_gpw == 'g') {
  dest_file=paste(download_file_folder,'/',"data.csv", sep='')
  url = paste("https://stooq.pl/q/d/l/?s=", ticker, "&d1=", gsub('-', '', date_begin), "&d2=", gsub('-', '', date_end), "&i=d", sep='')
  download.file(url, dest_file,quiet = FALSE, mode = "wb")
  data = read.csv("data.csv", header = TRUE)
  all_data = data$Zamkniecie
  all_time = data$Data
}


# --------------- WYLICZENIE WSKAZNIKOW STATYSTYCZNYCH -------
print(paste("Wielkosc proby:", length(all_data)))
print(paste("Min:", min(all_data)))
print(paste("Max:", max(all_data)))
print(paste("Srednia arytmetyczna:", mean(all_data)))
print(paste("Srednia geometryczna:", geometric.mean(all_data)))
print(paste("Srednia harmoniczna:", harmonic.mean(all_data)))
print(paste("Kwantyl rzedu 1/4:", quantile(all_data, (0.25))))
print(paste("Kwantyl rzedu 3/4:", quantile(all_data, (0.75))))
print(paste("Mediana:", median(all_data)))
print("Przedzial zmiennosci proby:")
print(range(all_data))
print(paste("Wariancja:", var(all_data)))
print(paste("Odchylenie standardowe:", sd(all_data)))
print(paste("Dominanta:", Mode(all_data)))
print(paste("Rozstep miedzykwartylowy:", IQR(all_data)))
print(paste("Kurtoza:", kurtosis(all_data)))
print(paste("Skosnosc: ", skewness(all_data)))

# ------------- WYKRESY ----------
hist(all_data, main=paste('Histogram notowan', ticker), xlab = "wartosc", ylab = "czestotliwość", col="red")
boxplot(all_data, main=paste('Wykres pudelkowy notowan', ticker), ylab = "wartosc")
vioplot(all_data)
plot(as.Date(all_time), all_data, type='l', main ="zmiany wartości aktywu w czasie", ylab="czas", xlab="wartosc")
stripchart(all_data, main = "wykres typu stripchart", ylab = "wartość", method = "jitter", col="orange", pch=1)