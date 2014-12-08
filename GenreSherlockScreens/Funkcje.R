library(tuneR)
library(signal)
library(matrixStats)

#G³ówna funkcja ca³ego programu
#path - œcie¿ka do pliku
#np: C:/Users/Krystian/Downloads/Metallica - Seek And Destroy  .mp3
#Je¿eli plik jest w workspace, to wystarczy podaæ sam¹ nazwê z rozszerzeniem (wav lub mp3)
#UWAGA z regu³y chyba jest '\', a R potrzebuje '/'
#a,b,c,d,e,f,g - wspó³czynniki wagowe poszczególnych metod
#Funkcja zwraca data.frame z polami M,R,J,C i H (metal, rock, jazz,...). Wartoœæ danego pola, to procentowa przynale¿noœæ do gatunku.
ClassifyMusicGenre <- function(path, a=0.2, b=0.6, c=0.05, d=0.05, e=0.05, f=0.05, g=0){
  
  if( substr(path, nchar(path)-2, nchar(path))=="wav" ){
    music = readWave(path)
  }
  else if(substr(path, nchar(path)-2, nchar(path))=="mp3"){
    music = readMP3(path)
  }
  else{
    return(data.frame(M=0,
                      R=0,
                      J=0,
                      C=0,
                      H=0,
                      E=0))
  }
  
  music = mono(music, which="left")
  music = Middle(music,120)
  music@left = music@left/2^(music@bit-1)
  
  
  wMAS = GetMeanAndStd(music)
  wMFCC = MFCC(music)
  
  #wektor gatunków
  genres = c("Metal","Rock","Jazz","Classic","Rap_And_HipHop")
  #Wektor z miejscem na wyniki
  result = rep(0,length(genres))
  #Pêtla po ka¿dnym gatunku
  for(j in seq(1,length(genres))){
    #Wczytaj baze
    wz = CSV_read(genres[j])
    #Wylicz
    result[j] = a*RMS(wz$avr, wMAS$avr)+
      b*RMS(wz$std, wMAS$std)+
      c*RMS(wz$meanVal, wMFCC$meanVal)+
      d*RMS(wz$stdVal, wMFCC$stdVal)+
      e*RMS(wz$Delta_meanVal, wMFCC$Delta_meanVal)+
      f*RMS(wz$Delta_stdVal, wMFCC$Delta_stdVal)#+
    #g*abs(KLD(wz, wMFCC))
    
    #Przeliczanie na procenty. Wynik=0 -> 100%, wynik>=A ->0%
    A = 20
    if(result[j]>A){
      result[j] = 0
    }
    else{
      #result[j] = (-1/A*result[j]+1)*100  #Podzia³ liniowy
      result[j] = (-1/log(A+1)*log(result[j]+1)+1)*100 #Podzia³ logarytmiczny
    }
    #Powy¿sze przeliczanie na procenty, to taka moja propozycja, je¿eli ktoœ ma lepszy pomys³, to mo¿na to zmieniæ (jak wszystko z reszt¹ :D)
  }  
  
  
  return(data.frame(M=result[1],
                    R=result[2],
                    J=result[3],
                    C=result[4],
                    H=result[5]))
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

#Liczy œredni¹ amplitutdê [dB] dla ca³ego pasma czêstotliwoœci oraz jej odchylenie standardowe
#¯eby nie robiæ tego dla ca³ej piosenki, to mo¿na okreœliæ od której (from) do której (to) sekundy.
#Mono - obiekt klasy wave, mono
GetMeanAndStd <- function (wav, from=0, to=length(Mono@left)/Mono@samp.rate) {
  #wav = CutMusic(Mono, from=from, to=to, normalize=TRUE)
  
  Fs <- wav@samp.rate
  step <- trunc(10*Fs/1000)            # one spectral slice every 10 ms
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


CSV_read <- function (genre){
  
  MAS = paste("Baza/MAS_",genre,".csv",sep="")
  MFCC = paste("Baza/MFCC_",genre,".csv",sep="")
  MFCC_covMatrix = paste("Baza/MFCC_covMatrix_",genre,".csv",sep="")
  #ustaw wlasciwe nazwygatunek 
  getMeanAndStd_data <- read.csv(file=MAS,head=TRUE,sep=",")
  MFCC_list_data <- read.csv(file=MFCC,head=TRUE,sep=",")
  MFCC_matrix_data <- read.csv(file=MFCC_covMatrix,head=TRUE,sep=",")
  
  avr <- getMeanAndStd_data$avr
  std <- getMeanAndStd_data$std
  f   <- getMeanAndStd_data$f
  
  meanVal       <- MFCC_list_data$meanVal
  stdVal        <- MFCC_list_data$stdVal
  Delta_meanVal <- MFCC_list_data$Delta_meanVal
  Delta_stdVal  <- MFCC_list_data$Delta_stdVal
  
  Sigma1  <- matrix(MFCC_matrix_data$Sigma.1)
  Sigma2  <- matrix(MFCC_matrix_data$Sigma.2)
  Sigma3  <- matrix(MFCC_matrix_data$Sigma.3)
  Sigma4  <- matrix(MFCC_matrix_data$Sigma.4)
  Sigma5  <- matrix(MFCC_matrix_data$Sigma.5)
  Sigma6  <- matrix(MFCC_matrix_data$Sigma.6)
  Sigma7  <- matrix(MFCC_matrix_data$Sigma.7)
  Sigma8  <- matrix(MFCC_matrix_data$Sigma.8)
  Sigma9  <- matrix(MFCC_matrix_data$Sigma.9)
  Sigma10 <- matrix(MFCC_matrix_data$Sigma.10)
  Sigma11 <- matrix(MFCC_matrix_data$Sigma.11)
  Sigma12 <- matrix(MFCC_matrix_data$Sigma.12)
  Sigma13 <- matrix(MFCC_matrix_data$Sigma.13)
  
  Sigma <- cbind(Sigma1,
                 Sigma2,
                 Sigma3,
                 Sigma4,
                 Sigma5,
                 Sigma6,
                 Sigma7,
                 Sigma8,
                 Sigma9,
                 Sigma10,
                 Sigma11,
                 Sigma12,
                 Sigma13)
  
  DeltaSigma1  <- matrix(MFCC_matrix_data$Delta_Sigma.1)
  DeltaSigma2  <- matrix(MFCC_matrix_data$Delta_Sigma.2)
  DeltaSigma3  <- matrix(MFCC_matrix_data$Delta_Sigma.3)
  DeltaSigma4  <- matrix(MFCC_matrix_data$Delta_Sigma.4)
  DeltaSigma5  <- matrix(MFCC_matrix_data$Delta_Sigma.5)
  DeltaSigma6  <- matrix(MFCC_matrix_data$Delta_Sigma.6)
  DeltaSigma7  <- matrix(MFCC_matrix_data$Delta_Sigma.7)
  DeltaSigma8  <- matrix(MFCC_matrix_data$Delta_Sigma.8)
  DeltaSigma9  <- matrix(MFCC_matrix_data$Delta_Sigma.9)
  DeltaSigma10 <- matrix(MFCC_matrix_data$Delta_Sigma.10)
  DeltaSigma11 <- matrix(MFCC_matrix_data$Delta_Sigma.11)
  DeltaSigma12 <- matrix(MFCC_matrix_data$Delta_Sigma.12)
  DeltaSigma13 <- matrix(MFCC_matrix_data$Delta_Sigma.13)
  
  Delta_Sigma <- cbind(DeltaSigma1,
                       DeltaSigma2,
                       DeltaSigma3,
                       DeltaSigma4,
                       DeltaSigma5,
                       DeltaSigma6,
                       DeltaSigma7,
                       DeltaSigma8,
                       DeltaSigma9,
                       DeltaSigma10,
                       DeltaSigma11,
                       DeltaSigma12,
                       DeltaSigma13)
  
  #w nazwa utworu
  w = list(avr=avr, 
           std=std, 
           f=f, 
           meanVal=meanVal, 
           stdVal=stdVal, 
           Delta_meanVal=Delta_meanVal, 
           Delta_stdVal=Delta_stdVal, 
           Sigma=Sigma, 
           Delta_Sigma=Delta_Sigma)
  return (w)
}

#Wylicza MFCC korzystaj¹c z funkcji melfcc z bibliteki tuneR
#Dodatkowo wylicza delte, czyli "differential coefficients" 
#(http://www.cs.cmu.edu/~robust/Papers/KumarKimSternICA11.pdf - równanie (1))
#Music - obiekt klasy wave
#wintime - szerokoœæ okna w sekundach
#hoptime - co jaki okres czasu maj¹ zaczynaæ siê nowe ramki (odpowiada za overlaping)
#numcep - ile wspó³czynników MFCC ma zwróciæ
#Funkcja zwraca liste:
# meanVal - wartoœæ œrednia wspó³czynników
# stdVal - odchylenie standardowe wspó³czynników
# Delta_meanVal - wartoœæ œrednia delty
# Delta_stdVal - wartoœæ oczekiwana delty
# Sigma - estymator macierzy kowariancji dla n-wymiarowego rozk³adu normalnego
#Wymagane paczki:
# - tuneR
# - matrixStats
MFCC <- function(Music, wintime = 0.02, hoptime = 0.01, numcep=20){
  
  #Licz MFCC
  m <- melfcc(Music, wintime = wintime, hoptime = hoptime, numcep=numcep, usecmp=FALSE,
              modelorder=NULL, spec_out=FALSE, frames_in_rows=FALSE,
              sumpower = TRUE, preemph=0.96, nbands=40, bwidth=1, dcttype="t2",
              lifterexp = 0.6)
  #Œrednia dla ka¿dego ze wspó³czynników ze wszystkich ramek
  meanVal = rowMeans(m)
  #ochylenie dla ka¿dego ze wspó³czynników ze wszystkich ramek
  stdVal = rowSds(m)
  #Mediana raczej nie bêdzie potrzebna
  #medVal = rowMedians(m)
  
  #Obliczanie macierzy kowariancji 
  #(estymator o najwiêkszej wiarygodnoœci dla n-wymiarowego rozk³adu normalnego)
  #http://pl.wikipedia.org/wiki/Wielowymiarowy_rozk%C5%82ad_normalny
  Sigma = matrix(0,numcep,numcep)
  for(i in seq(1, length(m[1,]))){
    Sigma = Sigma + (m[,i]-meanVal)%*%t(m[,i]-meanVal)
  }
  Sigma = 1/length(m[1,])*Sigma
  
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
  
  #Macierz kowariancji delt
  Delta_Sigma = matrix(0,numcep,numcep)
  for(i in seq(1, length(delta[,1]))){
    Delta_Sigma = Delta_Sigma + (delta[i,]-Delta_meanVal)%*%t(delta[i,]-Delta_meanVal)
  }
  Delta_Sigma = 1/length(delta[,1])*Delta_Sigma
  
  d = list(meanVal=meanVal, 
           stdVal=stdVal, 
           Delta_meanVal=Delta_meanVal, 
           Delta_stdVal=Delta_stdVal, 
           Sigma=Sigma, 
           Delta_Sigma = Delta_Sigma)
  return(d)
}

#Œlad macierzy
tr <- function(A){
  return(ifelse(isSquare(A), sum(diag(A)) ,NA))
}

#Czy macierz jest kwadratowa
isSquare <- function(A){
  return(is.matrix(A) && (nrow(A)==ncol(A)))
}

#Kullback-Leible Divergence
#S³u¿y do porównywania dwóch rozk³adów normlanych Gaussa. N-wymiarowcyh
KullbackLeibleDivergence <- function(mi0, mi1, Sigma0, Sigma1){
  
  D = log(det(Sigma0)/det(Sigma1))
  +tr(ginv(Sigma0)%*%Sigma1)
  +t(mi1-mi0)%*%ginv(Sigma0)%*%(mi1-mi0)
  -nrow(Sigma0)
  #   D = -log(det(Sigma0)/det(Sigma1))
  #   +tr(ginv(Sigma1)%*%Sigma0)
  #   +t(mi1-mi0)%*%ginv(Sigma1)%*%(mi1-mi0)
  #   -nrow(Sigma0)
  #   D = 1/2*D
  return(D)
}

# d0 i d1 to wyniki funkcji MFCC dla ró¿nych utworów
#KLD nie jest symetryczne, dlatego trzeba zrobic tak:
KLD <- function(d0,d1){
  
  mi0 = d0$meanVal
  mi1 = d1$meanVal
  Sigma0 = d0$Sigma
  Sigma1 = d1$Sigma
  
  D = KullbackLeibleDivergence(mi0, mi1, Sigma0, Sigma1)
  +KullbackLeibleDivergence(mi1, mi0, Sigma1, Sigma0)
  
  return(D)
  #return(abs(floor(100*D)/100))
}

#Tworzy baze danych 
#fol - folder z muzyk¹ danego gatunku (w workspace)
#dir - wektor tytu³ów z rozszerzeniem wav
#Przyk³adowo fol="Jazz"
#dir = c("j1.wav",  "j2.wav",  "j3.wav",  "j4.wav",  "j5.wav",
#        "j6.wav",  "j7.wav",  "j8.wav",  "j9.wav",  "j10.wav",
#        "j11.wav", "j12.wav", "j13.wav", "j14.wav", "j15.wav",
#        "j16.wav", "j17.wav", "j18.wav", "j19.wav", "j20.wav")
#Lepiej ¿eby poszczególne gatunki by³y w osobnych folderach o nazwach Metal, Rock, Jazz, Classic, Rap_And_HipHop
DataBase <- function (fol, dir, N=length(dir)) {
  
  result1 = vector("list",N)
  result2 = vector("list",N)
  
  for(i in seq(1,N)){
    music = readWave(paste(fol,"/",dir[i],sep=""))
    music = mono(music,which="left")
    music@left = music@left/2^(music@bit-1)
    
    result1[[i]] = GetMeanAndStd(music)
    result2[[i]] = MFCC(music)
  }
  
  avr = rep(0, length(result1[[1]]$avr))
  std = rep(0, length(result1[[1]]$std))
  avravr=0
  avrstd=0
  avrmeanVal = 0
  avrstdVal = 0
  avrDeltameanVal = 0
  avrDeltastdVal = 0
  avrSigma = matrix(0,N,N)
  avrDeltaSigma = matrix(0,N,N)
  for(i in seq(1,N)){
    a1 = result1[[i]]$avr
    a2 = result1[[i]]$std
    avravr = avravr+1/N*a1
    avrstd = avrstd+1/N*a2
    
    a3 = result2[[i]]$meanVal
    a4 = result2[[i]]$stdVal
    a5 = result2[[i]]$Delta_meanVal
    a6 = result2[[i]]$Delta_stdVal
    avrmeanVal = avrmeanVal+1/N*a3
    avrstdVal = avrstdVal+1/N*a4
    avrDeltameanVal = avrDeltameanVal+1/N*a5
    avrDeltastdVal = avrDeltastdVal+1/N*a6
    
    A7 = result2[[i]]$Sigma
    avrSigma = avrSigma + 1/N*A7
    
    A8 = result2[[i]]$Sigma
    avrDeltaSigma = avrDeltaSigma + 1/N*A8
  }
  f = result1[[1]]$f
  getMeanAndStd_data = list(avr=avravr,std=avrstd,f=f)
  write.csv(getMeanAndStd_data, file=paste("Baza/MAS_",fol,".csv",sep=""))
  
  MFCC_data = list(meanVal = avrmeanVal,
                   stdVal = avrstdVal,
                   Delta_meanVal = avrDeltameanVal,
                   Delta_stdVal = avrDeltastdVal)
  write.csv(MFCC_data, file=paste("Baza/MFCC_",fol,".csv",sep=""))
  
  write.csv(list(Sigma=avrSigma, Delta_Sigma=avrDeltaSigma),
            file=paste("Baza/MFCC_covMatrix_",fol,".csv",sep=""))
}

Metaldir = c("m1.wav",  "m2.wav",  "m3.wav",  "m4.wav",  "m5.wav",
             "m6.wav",  "m7.wav",  "m8.wav",  "m9.wav",  "m10.wav",
             "m11.wav", "m12.wav", "m13.wav", "m14.wav", "m15.wav",  
             "m16.wav")

Rockdir =  c("r1.wav",  "r2.wav",  "r3.wav",  "r4.wav",  "r5.wav",
             "r6.wav",  "r7.wav",  "r8.wav",  "r9.wav",  "r10.wav",
             "r11.wav", "r12.wav", "r13.wav", "r14.wav", "r15.wav",  
             "r16.wav", "r17.wav", "r18.wav", "r19.wav", "r20.wav")

Classicdir=c("c1.wav",  "c2.wav",  "c3.wav",  "c4.wav",  "c5.wav",
             "c6.wav",  "c7.wav",  "c8.wav",  "c9.wav",  "c10.wav",
             "c11.wav", "c12.wav", "c13.wav", "c14.wav", "c15.wav", 
             "c16.wav", "c17.wav", "c18.wav", "c19.wav", "c20.wav")

Jazzdir =  c("j1.wav",  "j2.wav",  "j3.wav",  "j4.wav",  "j5.wav",
             "j6.wav",  "j7.wav",  "j8.wav",  "j9.wav",  "j10.wav",
             "j11.wav", "j12.wav", "j13.wav", "j14.wav", "j15.wav",
             "j16.wav", "j17.wav", "j18.wav", "j19.wav", "j20.wav")

HipHopdir= c("h1.wav",  "h2.wav",  "h3.wav",  "h4.wav",  "h5.wav",
             "h6.wav",  "h7.wav",  "h8.wav",  "h9.wav",  "h10.wav",
             "h11.wav", "h12.wav", "h13.wav", "h14.wav", "h15.wav",
             "h16.wav", "h17.wav", "h18.wav", "h19.wav", "h20.wav")