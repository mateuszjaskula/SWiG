#Tnie muzyke na kwa³ki
#Music - obiekt klasy Wave
#from - od której sekundy ci¹æ
#to - do której sekundy ci¹æ
#normlize - czy normalizowaæ od -1 do 1. Wartoœæ domyœlna: FALSE
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


#Wylicza tempo
#Mono - obiekt klasy Wave, znormalizowany, mono
Tempo <- function (Mono) {
  
  #Zmiana czêstotliwoœci próbkowania, ¿eby liczy³o siê szybciej
  Mono = downsample(Mono, 2000)
  #Wartoœæ bezwzglêdna
  Mono@left = abs(Mono@left)
  #6x filtr dolnoprzepustowy, ¿eby wyg³adziæ sygna³
  for (i in seq(1:6)){
    x = LPfilter(Mono@left, 10, 1/Mono@samp.rate)
    Mono@left = x
  }
  #Szukaj maksimów
  m = localMaxima(x)
  #Wyrzuæ pierwsz¹ wartoœæ
  m = m[m!=1]
  #œrednia i wartoœæ bezwzglêdna z wartoœci maksimów
  meanVal = mean(x[m])
  standDev = sd(x[m])
  #Je¿eli dane maksimum jest
  #wiêksze od mean-0.5*std, lub
  #mniesze od mean+2*std, to zostaw - reszte wywal
  m = m[x[m]>meanVal-0.5*standDev & x[m]<meanVal+2*standDev]
  #Interwa³y pomiêdzy maksimami
  interval = rep(0,length(m)-1)
  for(i in seq(1:length(m)-1)){
    interval[i] <- m[i+1]-m[i]
  }
  #Wartoœæ œrednia, odchylenie standardowe i mediana interwa³ów przeliczona na sekundy
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
  
  #Funckja zwraca mediane, œredni¹ i odchylenie standardowe interwa³ów
  result = c(intMedVal, intMeanVal, intStdVal)
  
  return(result)
}

#Wylicza tempo dla kwa³ków muzyki
#Music - obiekt klasy Wave, mono, znormalizowany
#duration - czas trwania kawa³ków, domyœlnie 10s
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

#Filtr dolnoprzepustowy I rzêdu
#x - wektor liczb
#fgr - czêstotliwoœæ graniczna w Hz
#dt - czêstotliwoœæ próbkowania. Z regu³y 1/44100. dt = music@samp.rate
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

PassHz <- function (Hz, Rp=0.1, Rs=10, plot=FALSE) {
  Fs <- 44100
  chord2 <- cheb1ord(c((Hz-10)/(Fs/2), (Hz+10)/(Fs/2)), c((Hz-20)/(Fs/2), (Hz+20)/(Fs/2)), Rp, Rs)
  filter <- cheby1(chord2)
  
  if(plot==TRUE){
    fr = freqz(filter, Fs=Fs)
    plot(fr$f, 20*log10(abs(fr$h)),type="l",xlim=c(0,10000), ylim=c(-100,10))
  }
  
  return(filter)
}

BandPass <- function (from, to, Rp=0.1, Rs=1.3, plot=FALSE) {
  Fs <- 44100
  Wp = c((from-10)/(Fs/2), (to+10)/(Fs/2))
  Ws = c((from-20)/(Fs/2), (to+20)/(Fs/2))
  chord2 <- cheb1ord(Wp, Ws, Rp, Rs)
  filter <- cheby1(chord2)
  if(plot==TRUE){
    fr = freqz(filter, Fs=Fs)
    plot(fr$f, 20*log10(abs(fr$h)),type="l",xlim=c(0,10000), ylim=c(-100,10))
  }
  
  return(filter)
}

GetMeanAndStd <- function (Mono, from, to) {
  wav = CutMusic(Mono, from=from, to=to, normalize=TRUE)
  
  Fs <- wav@samp.rate
  step <- trunc(20*Fs/1000)            # one spectral slice every 20 ms
  window <- trunc(20*Fs/1000)          # 20 ms data window
  fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
  spg <- specgram(wav@left, fftn, Fs, hanning(fftn), window-step)
  S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
  S <- S/max(S)         # normalize magnitude so that max is 0 dB.
  S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
  S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
  
  v = rep(0, length(spg$f))
  v = rowMeans(20*log10(Mod(spg$S)))
  s = rowSds(20*log10(Mod(spg$S)))
  
  d = data.frame(v,s)
  return(d)
  
}
