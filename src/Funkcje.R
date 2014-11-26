#Tnie muzyke na kwałki
#Music - obiekt klasy Wave
#from - od której sekundy ciąć
#to - do której sekundy ciąć
#normlize - czy normalizować od -1 do 1. Wartość domyślna: FALSE
CutMusic <- function(Music, from, to, normalize = FALSE){
  
  if(normalize == TRUE && max(Music@left)>1){
    Music@left <- Music@left/2^(Music@bit-1)  
  }
  
  sound <- Music@left
  samp <- length(sound)
  rate <- Music@samp.rate
  time <- samp/rate 
  if(from<0) return("CutMusic Error: from < 0")
  if(to>time) return("CutMusic Error: to > length(Music)") 
  if(from>=to) return("CutMusic Error: from >= to")
  
  leftIndex <- from*rate
  rightIndex <- to*rate 
  sound <- sound[seq(leftIndex, rightIndex, 1)]
  
  Music@left <- sound
  
  return(Music)
}


#Wylicza tempo (naiwne podejście)
#Mono - obiekt klasy Wave, znormalizowany, mono
Tempo <- function (Mono) {
  
  #Zmiana częstotliwości próbkowania, żeby liczyło się szybciej
  Mono = downsample(Mono, 2000)
  #Wartość bezwzględna
  Mono@left = abs(Mono@left)
  #6x filtr dolnoprzepustowy, żeby wygładzić sygnał
  for (i in seq(1:6)){
    x = LPfilter(Mono@left, 10, 1/Mono@samp.rate)
    Mono@left = x
  }
  #Szukaj maksimów
  m = localMaxima(x)
  #Wyrzuć pierwszą wartość
  m = m[m!=1]
  #średnia i wartość bezwzględna z wartości maksimów
  meanVal = mean(x[m])
  standDev = sd(x[m])
  #Jeżeli dane maksimum jest
  #większe od mean-0.5*std, lub
  #mniesze od mean+2*std, to zostaw - reszte wywal
  m = m[x[m]>meanVal-0.5*standDev & x[m]<meanVal+2*standDev]
  #Interwały pomiędzy maksimami
  interval = rep(0,length(m)-1)
  for(i in seq(1:length(m)-1)){
    interval[i] <- m[i+1]-m[i]
  }
  #Wartość średnia, odchylenie standardowe i mediana interwałów przeliczona na sekundy
  intMeanVal = mean(interval, na.rm = TRUE)/2000
  intStdVal = sd(interval, na.rm = TRUE)/2000
  intMedVal = median(interval, na.rm = TRUE)/2000
  
#   plot(x, type="l")
#   for (i in seq(1:length(m))){
#       abline(v=m[i], col="red")
#   }
#   abline(h=meanVal+2*standDev, col="green")
#   abline(h=meanVal-0.5*standDev, col="green")
#   abline(h=meanVal, col="blue")
  
  #Funckja zwraca mediane, średnią i odchylenie standardowe interwałów
  result = c(intMedVal, intMeanVal, intStdVal)
  
  return(result)
}

#Wylicza tempo dla kwałków muzyki
#Music - obiekt klasy Wave, mono, znormalizowany
#duration - czas trwania kawałków, domyślnie 10s
MusicTempo <- function (Music, duration=10) {
  samp = length(Music@left)
  rate = Music@samp.rate
  time = samp/rate
  k=1
  mean = rep(0,floor(time/duration)-1)
  std = rep(0,floor(time/duration)-1)
  median = rep(0,floor(time/duration)-1)
  
  for(i in seq(0, time-2*duration, duration)){
    M = CutMusic(Music,from=i, to=i+duration, normalize=FALSE)
    w = Tempo(M)
    median[k] = w[1]
    mean[k] = w[2]
    std[k] = w[3]
    k=k+1
  }
  
  result = data.frame(median,mean,std)
  
  return(result)
}

#Filtr dolnoprzepustowy I rzędu
#x - wektor liczb
#fgr - częstotliwość graniczna w Hz
#dt - częstotliwość próbkowania. Z reguły 1/44100. dt = music@samp.rate
LPfilter <- function(x, fgr, dt){
  N = length(x)
  T = 1/(2*pi*fgr)
  y = rep(0,N)
  y[1]=x[1]
  for (i in 2:N) {
    y[i]=T/(T+dt)*y[i-1]+dt/(T+dt)*x[i]
  }
  return(y)
}

#from: http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
