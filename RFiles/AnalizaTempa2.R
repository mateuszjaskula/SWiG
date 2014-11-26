#library("tuneR", lib.loc="~/R/win-library/3.1")
#Stereo2 = readMP3("Kenny Loggins - Footloose  .mp3")
#Mono = mono(Stereo2, which="left")

Mono33 = CutMusic(Mono3, from=30, t=40, normalize=TRUE)
Mus = Mono33
Mus@left = Mus@left*2^(Mus@bit-1)

FT = fft(HanningWindow(Mono33@left))
x = seq(1,length(Mono33@left),1)
y = Mod(FT)
