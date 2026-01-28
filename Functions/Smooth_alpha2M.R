#Smoothing and alpha-2-M correction

#Late smoothing
#lateTimepoint <- length(Tdif)-input$smtail
#lateTime <- Tdif[lateTimepoint]
fun_latesmooth<-function(ps, plateF, smtail){
  #Tdif<-plateF[,1][-1]
  Tdif<-plateF[,1]
  lateTimepoint <- length(Tdif)-smtail
  lateTime <- Tdif[lateTimepoint]
  smoothfit<-supsmu(Tdif, ps)
  ifelse(Tdif>lateTime, smoothfit$y, ps)
}

#all smoothing
fun_allsmooth <- function(as, plateF){
  #Tdif<-plateF[,1][-1]
  Tdif<-plateF[,1]
  smoothfit <- supsmu(Tdif, as)
  smoothfit$y
}

#Alpha-2-M correction
fun_TGcorr<-function(M, plateF, smtail){
  endTimepoint <- length(M)
  Tdif<-plateF[,1]
  lateTimepoint <- length(Tdif)-smtail
  lateTime <- Tdif[lateTimepoint]
  endVal<-mean(M[lateTimepoint:endTimepoint])
  Fdummy<-rep(endVal, length(Tdif))
  Fnew<-ifelse(Tdif>lateTime, Fdummy, M)
  FTm<-endVal*(cumsum(Fnew[1:lateTimepoint])/max(cumsum(Fnew[1:lateTimepoint])))
  Fdummy[1:lateTimepoint]<-FTm
  M-Fdummy
}

fun_TaM<-function(C, plateF, smtail){
  Tdif<-plateF[,1]
  endTimepoint <- length(C)
  lateTimepoint <- length(C)-smtail
  lateTime <- Tdif[lateTimepoint]
  endVal<-mean(C[lateTimepoint:endTimepoint])
  Fdummy<-rep(endVal, length(Tdif))
  Fnew<-ifelse(Tdif>lateTime, Fdummy, C)
  FTm<-endVal*(cumsum(Fnew[1:lateTimepoint])/max(cumsum(Fnew[1:lateTimepoint])))
  Fdummy[1:lateTimepoint]<-FTm
  Fdummy
}