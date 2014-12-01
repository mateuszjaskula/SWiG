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
  if(from < 0) return("CutMusic Error: from < 0")
  if(to>time) return("CutMusic Error: to > length(Music)") 
  if(from>=to) return("CutMusic Error: from >= to")
  
  leftIndex <- from*rate
  rightIndex <- to*rate 
  #Je¿eli from==0, to zaczyna od pierwszego indeksu (w R pierwszy indeks to [1] a nie [0])
  if(leftIndex==0){
    leftIndex=1
  }
  sound <- sound[seq(leftIndex, rightIndex, 1)]
  
  Music@left <- sound
  
  return(Music)
}


#Wylicza tempo (naiwne podejœcie. Nied³ugo spróbuje z lepszym filtrem)
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
  
    plot(x, type="l")
    for (i in seq(1:length(m))){
        abline(v=m[i], col="red")
    }
    abline(h=meanVal+2*standDev, col="green")
    abline(h=meanVal-0.5*standDev, col="green")
    abline(h=meanVal, col="blue")
  
  #Funckja zwraca mediane, œredni¹ i odchylenie standardowe interwa³ów
  result = c(intMedVal, intMeanVal, intStdVal)
  
  return(result)
}

#Wylicza tempo dla kawa³ków muzyki. Zwraca data.frame z median¹, œredni¹ i odchyleniami standardowymi dla ka¿dego kawa³ka
#Bazuje na funkcji Tempo (patrz: powy¿ej)
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

#Filtr pasmowo przepustowy dla podanej czêstotliwoœci Hz.
#¯eby zobaczyæ, czy dzia³a, to nale¿y rysowaæ wykres (plot=TRUE)
#W razie potrzeby mo¿na pokrêciæ parametrami Rp i Rs
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

#Filtr pasmowo przepustowy dla czêstotliwoœci od 'from' do 'to' w Hz
#¯eby zobaczyæ, czy dzia³a, to nale¿y rysowaæ wykres (plot=TRUE)
#W razie potrzeby mo¿na pokrêciæ parametrami Rp i Rs
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

#Liczy œredni¹ amplitutdê [dB] dla ca³ego pasma czêstotliwoœci oraz jej odchylenie standardowe
#¯eby nie robiæ tego dla ca³ej piosenki, to mo¿na okreœliæ od której (from) do której (to) sekundy.
#Mono - obiekt klasy wave, mono
GetMeanAndStd <- function (Mono, from, to) {
  wav = CutMusic(Mono, from=from, to=to, normalize=TRUE)
  
  Fs <- wav@samp.rate
  step <- trunc(20*Fs/1000)            # one spectral slice every 20 ms
  window <- trunc(20*Fs/1000)          # 20 ms data window
  fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
  spg <- specgram(wav@left, fftn, Fs, hanning(fftn), window-step)
  
  avr = rep(0, length(spg$f))
  avr = rowMeans(20*log10(Mod(spg$S)))
  std = rowSds(20*log10(Mod(spg$S)))
  f = spg$f
  #Zwraca data.frame z wektorem œrednich, odchyleñ i czêstotliwoœci
  d = data.frame(avr,std,f)
  return(d)
  
}

#root mean square
#Do porównywania dwóch wektorów
RMS <- function (x, y) {
  return (sqrt(sum((x-y)^2)/length(x)))
}

#Zwraca œrodekowe 's' sekund utworu
Middle <- function(Mono,s){
  rate = Mono@samp.rate
  samp = length(Mono@left)
  
  sound = Mono@left[seq(samp/2-s/2*rate, samp/2+s/2*rate,1)]
  
  result = Mono
  result@left = sound
  return(result)
}

#Zwraca œrodekowe 'p'%  utworu
MiddlePerc <- function(Mono,p){
  rate = Mono@samp.rate
  samp = length(Mono@left)
  
  if(p>100) p = 100
  if(p<0) p = 0
  sound = Mono@left[seq(samp/2-p*samp/200,samp/2+p*samp/200,1)]
  
  result = Mono
  result@left = sound
  return(result)
}

#Jak w nazwie funkcji
Hz2Mel <- function(Hz){
  Mel = 1125*log(1+Hz/700)
  return(Mel)
}

#Jak w nazwie funkcji
Mel2Hz <- function(Mel){
  Hz = 700*(exp(Mel/1125)-1)
  return(Hz)
}

#mel filterbank
#http://practicalcryptography.com/miscellaneous/machine-learning/guide-mel-frequency-cepstral-coefficients-mfccs/
TriangularWindowFilterbank <- function(f, M, n){
  
  H = f[length(f)]
  t = seq(1,M+2,1)
  a = (Hz2Mel(H)-Hz2Mel(0))/(M+1)
  b = Hz2Mel(0)-a
  y = a*t+b
  freqs = Mel2Hz(y)
  
  y=rep(0,length(f))
  
  for (i in seq(1:length(f))){
    
    #if(f[i]<freqs[n]) y[i] = 0
    #if(f[i]>freqs[n+2]) y[i] = 0
    
    if(f[i]>=freqs[n] & f[i]<freqs[n+1]) 
      y[i]=(f[i]-freqs[n])/(freqs[n+1]-freqs[n])
    
    if(f[i]>=freqs[n+1] & f[i]<=freqs[n+2]) 
      y[i]=(f[i]-freqs[n+2])/(freqs[n+1]-freqs[n+2])    
    
  }
  return(y)
}

#Tak jak w:
#http://practicalcryptography.com/miscellaneous/machine-learning/guide-mel-frequency-cepstral-coefficients-mfccs/
#Funkcja liczy wspó³czynniki MFFC dla podanego obiektu wave
#wlen - d³ugoœæ ramki w ms
#ovlen - overlaping [ms]
#M - iloœæ filtrów w "mel filterbank"
#Funkcja zwraca macierz v. W wierszach s¹ kolejne wspó³czynniki MFCC dla ka¿dej z ramek
#Potrzebne paczki:
# - dtt
# - signal 
# Lepiej u¿yæ funkcji MFCC, która jest poni¿ej. Ta nie jest zoptymalizowana i d³ugo siê liczy
myMFCC <- function(wave, wlen=20, ovlap=20, M=26){
  
  #Tworzenie periodogramu
  Fs <- wave@samp.rate
  step <- trunc(ovlap*Fs/1000)         # one spectral slice every 20 ms
  window <- trunc(wlen*Fs/1000)        # 20 ms data window
  fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
  spg <- specgram(wave@left, fftn, Fs, hanning(fftn), window-step)
  #ilosc ramek: length(spg$t)
  #czas trwania ramki: fftn/Fs
  #i-ta ramka: spg$S[,i]
  #czas rozpoczêcia i-tej ramki: spg$t[i]
  
  #Liczenie wspó³czynników MFCC dla ka¿dej ramki
  c = matrix(0,length(spg$t),M)
  v = matrix(0,length(spg$t),13)
  for(i in seq(1,length(spg$t))){
    P = 1/fftn*(Mod(spg$S[,i]))^2 #Moc w ramce
    
    for (j in seq(1,M)){
      Pc = P*TriangularWindowFilterbank(spg$f, M, j)
      c[i,j] = log10(sum(Pc))
    }
    #Liczenie DCT
    c[i,] = dct(c[i,])
    #Zwraca wspó³czynniki od 1 do 13. Reszte ignoruje
    v[i,] = c[i,][seq(1,13,1)]
  }
  
  return(v)
}

#Wylicza MFCC korzystaj¹c z funkcji melfcc z bibliteki tuneR
#Dodatkowo wylicza delte, czyli "differential coefficients" 
#(http://www.cs.cmu.edu/~robust/Papers/KumarKimSternICA11.pdf - równanie (1))
#Music - obiekt klasy wave
#wintime - szerokoœæ okna w sekundach
#hoptime - co jaki okres czasu maj¹ zaczynaæ siê nowe ramki (odpowiada za overlaping)
#numcep - ile wspó³czynników MFCC ma zwróciæ
#Funkcja zwraca data.frame w postaci:
# nr_wspó³czynnika     œrednia    odchylenie    œrednia delty    odchylenie delty
#Wymagane paczki:
# - tuneR
# - matrixStats
MFCC <- function(Music, wintime = 0.02, hoptime = 0.01, numcep=13){
  
  #Licz MFCC
  m <- melfcc(Music, wintime = wintime, hoptime = hoptime, numcep=numcep, usecmp=FALSE,
              modelorder=NULL, spec_out=FALSE, frames_in_rows=FALSE,
              sumpower = TRUE, preemph=0, nbands=26, bwidth=1, dcttype="t2",
              lifterexp = 0.6)
  #Œrednia dla ka¿dego ze wspó³czynników ze wszystkich ramek
  meanVal = rowMeans(m)
  #ochylenie dla ka¿dego ze wspó³czynników ze wszystkich ramek
  stdVal = rowSds(m)
  #Mediana raczej nie bêdzie potrzebna
  #medVal = rowMedians(m)
  
  #Licz delty
  #http://www.cs.cmu.edu/~robust/Papers/KumarKimSternICA11.pdf - równanie (1)
  #dla m = 2
  delta = matrix(0, length(m[1,])-4, length(m[,1]))
  for(i in seq(3, length(m[1,])-2, 1)){
    delta[i-2,] = m[,i+2]-m[,i-2]  
  }
  
  #Œrednia i ochylenie dla delt 
  Delta_meanVal = colMeans(delta)
  Delta_stdVal = colSds(delta)
  #Delta_medVal = colMedians(delta)
  
  d = data.frame(meanVal, stdVal, Delta_meanVal, Delta_stdVal)
  return(d)
}