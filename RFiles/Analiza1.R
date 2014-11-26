library("tuneR", lib.loc="~/R/win-library/3.1")
#Wczytaj plik mp3
#wStereoMP3 = readMP3("hehe.mp3")
#wStereoWAV = readWave("dzwiek1.wav")
#Konwertuj na mono
#wMonoWAV = mono(wStereoWAV, which="left")
#Normowanie. Wartoœci od -1 do 1
wMono@left = wMono@left/2^(wMono@bit-1)
plot(wMono)

#parametry œcie¿ki dŸwiêkowej
samp = length(wMono@left)
rate = wMono@samp.rate
time = samp/rate

# wMono = (sine(100)+sine(200)+sine(300)+sine(400)+sine(500))/5
# plot(wMono)
# 
# #parametry œcie¿ki dŸwiêkowej
# samp = length(wMono@left)
# rate = wMono@samp.rate
# time = samp/rate

timeArray <- (0:(samp-1)) / rate
sound = wMono@left
sound = LPfiltr(sound, 50, 1/rate)
soundH = HanningWindow(sound)
trans = fft(soundH, inverse="FALSE")
widmo = 2*Mod(trans)
timeArray = timeArray*samp;
plot(timeArray, widmo, type="h", xlim=c(0,500))
