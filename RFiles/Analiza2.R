# library("tuneR", lib.loc="~/R/win-library/3.1")
# Stereo = readMP3("Tanssiorkesteri Lossimies - Volga.mp3")
#Mono = mono(Stereo, which="left")
#Normowanie. Wartoœci od -1 do 1
#Mono@left = Mono@left/2^(Mono@bit-1)
# #parametry œcie¿ki dŸwiêkowej
# samp = length(Mono@left)
# rate = Mono@samp.rate
# time = samp/rate


#Music - obiekt klasy Wave
#from - od której sekundy ci¹æ
#to - do której sekundy ci¹æ
#normlize - czy normalizowaæ od -1 do 1. Wartoœæ domyœlna: FALSE
CutMusic <- function(Music, from, to, normalize = FALSE){
  
  sound = Music@left
  samp = length(sound)
  rate = Music@samp.rate
  time = samp/rate 
  if(from<0) return("CutMusic Error: from < 0")
  if(to>time) return("CutMusic Error: to > length(Music)") 
  if(from>=to) return("CutMusic Error: from >= to")
  
  leftIndex = from*rate
  rightIndex = to*rate 
  sound = sound[seq(leftIndex, rightIndex, 1)]
  
  Music@left = sound
  
  return(Music)
  
}
