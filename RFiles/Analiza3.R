y=c(50,69,94,129,176,241,331,453,620,850,1200,1600,2200,3000,4100,5600,7700,11000,14000,20000)
ylog = log10(y)
t = seq(1,20,1)
# plot(t,ylog)
a = (ylog[20]-ylog[1])/19
b = ylog[1]-a
y2 = a*t+b
# lines(t,y2)
freqs = 10^y2
# plot(t,y)
# lines(t,f)


Mono11 = CutMusic(Mono, from=10, to=140, normalize=TRUE)
Mono11@left = Mono11@left * 2^(Mono11@bit-1)
A = Mono11
#Mono11 = noise(kind="white")/2+sine(400)#(sine(400)+sine(100)+sine(500)+sine(800))/4

# Fs <- 44100
# btord <- buttord(500/Fs, 600/Fs, 0.8, 7)
# filtr <- butter(btord)
# hf <- freqz(filtr, Fs = Fs)
# plot(hf$f, 20*log10(abs(hf$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))

# Fs <- 44100
# chord2 <- cheb1ord(360/(Fs/2),350/(Fs/2), 0.7, 5)
# filtrHP <- cheby1(chord2)
# hf2 <- freqz(filtrHP, Fs = Fs)
# plot(hf2$f, 20*log10(abs(hf2$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))
# 
# Fs <- 44100
# chord2 <- cheb1ord(440/(Fs/2),450/(Fs/2), 0.7, 3)
# filtrLP <- cheby1(chord2)
# hf2 <- freqz(filtrLP, Fs = Fs)
# lines(hf2$f, 20*log10(abs(hf2$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))


# Fs <- 44100
# elord1 <- ellipord(0.03, 0.031, 0.5, 29)
# filtr <- ellip(elord1)
# hf1 <- freqz(filtr, Fs = Fs)
# plot(hf1$f, 20*log10(abs(hf1$h)),type="l",xlim=c(0,10000))

# filtr = fir1(100, c(1/Fs*500, 1/Fs*600), "pass")
# h = freqz(filtr, Fs=Fs)
# plot(h$f, 20*log10(abs(h$h)),type="l",xlim=c(0,10000))

# Fs <- 44100
# F = 400;

# #Stopa
# chord2 <- cheb1ord(100/(Fs/2),110/(Fs/2), 0.03, 2)
# filtr <- cheby1(chord2)
# hf2 <- freqz(filtr, Fs = Fs)
# plot(hf2$f, 20*log10(abs(hf2$h)),type="l", xlim=c(0,1000), ylim=c(-100,100))

#filtr = BandPass(50,200,Rp=0.1,Rs=0.2,plot=TRUE)


#filtr = BandPass(500,2000,Rp=0.2,Rs=0.22,plot=TRUE)
#filtr = BandPass(40,500,Rp=0.14,Rs=0.25,plot=TRUE)
#filtr = BandPass(1000,2300,Rp=0.09,Rs=0.2,plot=TRUE)
#filtr = BandPass(40,500,Rp=0.14,Rs=0.25,plot=TRUE)

#filtr = BandPass(50,100,Rp=0.1,Rs=0.25,plot=TRUE)

sound = Mono11@left
#sound = filter(filtr,sound)
#sound = filter(filtr,sound)
Mono11@left = as.numeric(sound)
wav = Mono11


Mus = wav
Mus@left[Mus@left>2^15-1]=2^15-1
Mus@left[Mus@left< -2^15]=2^15

Fs <- wav@samp.rate
step <- trunc(20*Fs/1000)            # one spectral slice every 20 ms
window <- trunc(20*Fs/1000)          # 20 ms data window
fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
spg <- specgram(wav@left, fftn, Fs, hanning(fftn), window-step)
S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
S <- S/max(S)         # normalize magnitude so that max is 0 dB.
S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
#image(t(20*log10(S)), axes = TRUE)  #, col = gray(0:255 / 255))

db = rep(0,length(freqs))
std = rep(0,length(freqs))
#Wizualizacja
# for(i in seq(1,length(spg$t)/20-1)){
#   par(mfrow = c(1, 2))
#   plot(spg$f, 20*log10(Mod(spg$S[,20*i])),type="l", log="x")#,xlim=c(0,20000)
#   #abline(v=10*50*i, col="green")
#   #abline(v=10*length(spg$t), col="red")
#   
#   for(j in seq(1,length(freqs))){
#     AA = 20*log10(Mod(spg$S[,20*i]))
#     if(j-1==0){
#       B=0
#     }
#     else{
#       B=freqs[j-1]
#     }
#     db[j] = mean(AA[spg$f>B & spg$f<freqs[j]], na.rm = TRUE)
#     std[j]= sd(AA[spg$f>B & spg$f<freqs[j]], na.rm = TRUE)
#   }
# 
#   plot(seq(1,20,1),db)
#   points(seq(1,20,1),db-std, col="red",type="l")
#   points(seq(1,20,1),db+std, col="red",type="l")
#   
#   
#   #plot(spg$f, (Mod(spg$S[,i])),type="h",xlim=c(0,10000))
#   Sys.sleep(0.07)
# }
# par(mfrow = c(1,1))


# ## calculate the DWT of linear chirp
# linchirp <- make.signal("linchirp", n=1024)
# result <- wavDWT(linchirp, wavelet="s8", n.levels=5, keep.series=TRUE)
# ## plot the transform shifted for approximate zero
# ## phase alignment
# plot(wavShift(result))
# ## plot summary
# eda.plot(result)
# ## summarize the transform
# summary(result)

# v = rep(0,length(spg$S[,1]))
# for (i in seq(1,length(spg$t))){
#   v = v+20*log10(Mod(spg$S[,i]))/length(spg$t)
#   
# }

v = rep(0, length(spg$f))
v = rowMeans(20*log10(Mod(spg$S)))
s = rowSds(20*log10(Mod(spg$S)))
