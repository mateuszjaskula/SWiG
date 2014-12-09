#Potrzebne paczki:
# - signal
# - tuneR
# - matrixStats 

#Logarytmiczny rozk³ad pomiêdzy 50Hz a 20kHz w wektorze freqs 
#Wykorzystane przy wizualizacji "s³upków" dla danych czêstotliwoœci
t = seq(1,20,1)
a = (log10(20000)-log10(50))/19
b = log10(50)-a
y = a*t+b
freqs = 10^y

#Tak siê wczytuje muzyke: (wykonaæ tylko raz, a nie za ka¿dym uruchomieniem skryptu)
# Stereo = readMP3("tytul.mp3")  #lub readWave("tytul.wav")
# Mono = mono(Stereo, which="left")

#Ucinanie muzyki + normalizacja (zakres od -1 do 1)
Mono11 = CutMusic(Mono, from=10, to=140, normalize=TRUE)

#Jakby ktoœ chcia³ pos³uchaæ, to trzeba przywróciæ normalny zakres i wywo³aæ funkcj¹ play(Mono11)
#Mono11@left = Mono11@left * 2^(Mono11@bit-1)

#Tak mo¿na generowaæ w³asne sygna³y dŸwiêkowe:
#Mono11 = noise(kind="white")/2+sine(400)#(sine(400)+sine(100)+sine(500)+sine(800))/4

#Filtr Butterwortha. Wszystko o funkcjach 'buttord' i 'butter' w helpie
#mo¿na siê pobawic parametrami buttord ¿eby uzyskaæ dobr¹ charakterystykê
#freqz robi charakterystykê czêstotliwoœci¹w¹ filtru
# Fs <- 44100
# btord <- buttord(500/Fs, 600/Fs, 0.8, 7)
# filtr <- butter(btord)
# hf <- freqz(filtr, Fs = Fs)
# plot(hf$f, 20*log10(abs(hf$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))

#Filtr Czebyszewa. Wszystko w helpie
# Fs <- 44100
# chord2 <- cheb1ord(360/(Fs/2),350/(Fs/2), 0.7, 5)
# filtrHP <- cheby1(chord2)
# hf2 <- freqz(filtrHP, Fs = Fs)
# plot(hf2$f, 20*log10(abs(hf2$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))

#Filtr eliptyczny. Wszystko w helpie
# Fs <- 44100
# elord1 <- ellipord(0.03, 0.031, 0.5, 29)
# filtr <- ellip(elord1)
# hf1 <- freqz(filtr, Fs = Fs)
# plot(hf1$f, 20*log10(abs(hf1$h)),type="l",xlim=c(0,10000))

#Filtr FIR. Wszystko w helpie
# filtr = fir1(100, c(1/Fs*500, 1/Fs*600), "pass")
# h = freqz(filtr, Fs=Fs)
# plot(h$f, 20*log10(abs(h$h)),type="l",xlim=c(0,10000))

# Dolnoprzepustowy filtr Czebyszewa. Parametry dobrane tak ¿eby wyodrêbniæ stopê z perkusji
# chord2 <- cheb1ord(100/(Fs/2),110/(Fs/2), 0.03, 2)
# filtr <- cheby1(chord2)
# hf2 <- freqz(filtr, Fs = Fs)
# plot(hf2$f, 20*log10(abs(hf2$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))

#Jakieœ przyk³adowe filtry z wykorzystaniem stworzonej przeze mnie funkcji BandPass (patrz: podstawowe funkcje.R)
#filtr = BandPass(50,200,Rp=0.1,Rs=0.2,plot=TRUE)
#filtr = BandPass(500,2000,Rp=0.2,Rs=0.22,plot=TRUE)
#filtr = BandPass(40,500,Rp=0.14,Rs=0.25,plot=TRUE)
#filtr = BandPass(1000,2300,Rp=0.09,Rs=0.2,plot=TRUE)
#filtr = BandPass(40,500,Rp=0.14,Rs=0.25,plot=TRUE)
#filtr = BandPass(50,100,Rp=0.1,Rs=0.25,plot=TRUE)

#sound jest wektorem zawieraj¹cym wartoœci sygna³u z obiektu klasy wave (lewy kana³)
sound = Mono11@left

#Tak siê stosuje filtr
#sound = filter(filtr,sound)


#Mono11@left = as.numeric(sound)


wav = Mono11

#Jakby ktoœ chcia³ pos³uchaæ, to trzeba przywróciæ normalny zakres i wywo³aæ funkcj¹ play(Mus)
Mus = wav
Mus@left = Mus@left*2^(Mus@bit-1)
#Na wszelki wypadek (a i tak czasem nie pomaga :D)
Mus@left[Mus@left>2^15-1]=2^15-1
Mus@left[Mus@left< -2^15]=2^15

#Funkcja specgram tworzy spektogram sygna³u. Zwraca obiekt (tutaj spg) który zawiera:
#spg$S - Macierz, której wiersze to wartoœci transfomraty Fouriera (zespolone!) dla danego kawa³ka (czyli okienka o d³ugoœci 20ms).
#spg$f - wektor czêstotliwoœci odpowiadaj¹cych wartoœciom wierszy spg$S
#spg$t - wektor czasów dla poszczególnych kolumn spg$S. Wiêc i-ty element to czas w którym "zaczyna" siê i-te oienko
Fs <- wav@samp.rate
step <- trunc(20*Fs/1000)            # one spectral slice every 20 ms
window <- trunc(20*Fs/1000)          # 20 ms data window
fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
spg <- specgram(wav@left, fftn, Fs, hanning(fftn), window-step)

#Wyœwietlanie spektogramu. Fajne, bo kolorowe, ale dla nas bezu¿yteczne
#S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
#S <- S/max(S)         # normalize magnitude so that max is 0 dB.
#S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
#S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
#image(t(20*log10(S)), axes = TRUE)  #, col = gray(0:255 / 255))

#Wizualizacja
#Zrobi³em ¿ebym widzia³ czy dzia³a jak powinno. Mo¿e siê przyda, a mo¿e nie
#Na lewym wykresie wyœwietla widmo co dwudziestego okienka,
#a na prawym wartoœci œrednie okreœlone w przedzia³ach zdefiniowanych w wektorze freq (patrz: góra tego dokumentu)
#dodatkowo czerwonymi liniami zaznaczone s¹ odchylenia standardowe dla przedzia³ów. Nie wiem dlaczego dla pierwszych paru nie dzia³a
#Nie sugerujcie siê oznaczeniami osi poziomej prawego wykresu
db = rep(0,length(freqs))
std = rep(0,length(freqs))
for(i in seq(1,length(spg$t)/20-1)){
  par(mfrow = c(1, 2))
  plot(spg$f, 20*log10(Mod(spg$S[,20*i])),type="l", log="x")#,xlim=c(0,20000)
  #abline(v=10*50*i, col="green")
  #abline(v=10*length(spg$t), col="red")
  
  for(j in seq(1,length(freqs))){
    AA = 20*log10(Mod(spg$S[,20*i]))
    if(j-1==0){
      B=0
    }
    else{
      B=freqs[j-1]
    }
    db[j] = mean(AA[spg$f>B & spg$f<freqs[j]], na.rm = TRUE)
    std[j]= sd(AA[spg$f>B & spg$f<freqs[j]], na.rm = TRUE)
  }

  plot(seq(1,20,1),db)
  points(seq(1,20,1),db-std, col="red",type="l")
  points(seq(1,20,1),db+std, col="red",type="l")
  
  
  #plot(spg$f, (Mod(spg$S[,i])),type="h",xlim=c(0,10000))
  Sys.sleep(0.07)
}
par(mfrow = c(1,1))

