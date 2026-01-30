#Analysis of derivative curves
uppy<-function(curve, Timedf, pclag){
  
  minAbs=curve[1] #use this if not fixing minAbs
  pointmax<-which.max(curve)
  allMaxT<-Timedf[pointmax] #find the corresponding Time and F for this point
  allMaxA<-curve[pointmax]
  upTime<-Timedf[c(1:pointmax)] #vector of time to max
  upAbs<-curve[c(1:pointmax)]  #vector of absorbances to max
  pcChange<-(pclag*(max(curve)-minAbs))+minAbs
  startPoint<-which(abs(upAbs-pcChange)==min(abs(upAbs-pcChange)))[1]
  #This prevents crashing if there are blank wells
  thresh <- 0.005
  #ifelse(max(curve)-min(curve)<thresh,
  ifelse((max(curve)-min(curve)<thresh | min(upAbs)>=pcChange),
         startAbs<-upAbs[startPoint],
         startAbs<-round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x,3)
  )
  
  #StartTime is fitted if abs > threshold, otherwise is closest point
  #This prevents crashing if there are blank wells
  #ifelse(max(curve)-min(curve)<thresh,
  ifelse((max(curve)-min(curve)<thresh | min(upAbs)>=pcChange),
         startTime<-upTime[startPoint],
         startTime<-round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y,3)
  )
  
  changeA=allMaxA-minAbs
  
  upcurve<-c(minAbs, startTime, pcChange, startAbs, startPoint, pointmax,allMaxT, allMaxA)
  
}

downy<-function(curve, Timedf, pclag){
  #could try smoothing the whole curve to get pointmax
  pointmax<-which.max(curve)
  downTime<-Timedf[-c(1:pointmax)] #vector of time to max
  downAbsR<-curve[-c(1:pointmax)]  #vector of absorbances to max
  #maybe it's best to smooth the whole tail??
  downSmu<-supsmu(downTime, downAbsR)
  downAbs<-downSmu$y
  endAbs<-min(downAbs)#+input$offset
  #THIS IS NEW. SHOULD THE minAbs BE THE MIN ABS FOR THE DOWN CURVE?
  #DOES SOLVE THE PROBLEM OF POINTMIN BEING WRONG
  minAbs <- endAbs #try it this way incase the curve does not go back to zero
  ##which point closest to minimum
  pointmin<-which(abs(downAbs-endAbs)==min(abs(downAbs-endAbs)))[1]
  pcChange<-pclag*(max(curve)-minAbs)+minAbs
  
  #NEW decaypoint is where set% lysis occurs
  #if_else(curve[length(curve)]>=pcChange, decayPoint <- length(curve), decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1] )
  ifelse(downAbs[length(downAbs)]>=pcChange, decayPoint <- length(downAbs), decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1] )
  #New decayabs
  thresh <- 0.005
  #if_else(max(curve)-min(curve)<thresh,
  ifelse((max(curve)-min(curve)<thresh | min(downAbs)>=pcChange),
          decayAbs<-downAbs[decayPoint],
          decayAbs<-round(approx(downTime, downAbs, xout = pcChange, ties = mean)$x,3)
  )
  
  #StartTime is fitted if abs > threshold, otherwise is closest point
  #This prevents crashing if there are blank wells
  #New decaytime
  #ifelse(max(curve)-min(curve)<thresh,
  ifelse((max(curve)-min(curve)<thresh | min(downAbs)>=pcChange),
         decayTime<-downTime[decayPoint],
         decayTime<-round(approx(downAbs, downTime, xout = decayAbs, ties = mean)$y,3)
  )
  #Use smoothed curve for AUC-better for noisy time to tail
  smoothfit <- supsmu(Timedf, curve)
  curve_s <- smoothfit$y
  #This AUC is with %lag and decay 
  #Should do this on the smoothed curves????
  AUC<-sum(diff(Timedf[1:(pointmax+decayPoint)])*(head(curve[1:(pointmax+decayPoint)],-1)+tail(curve[1:(pointmax+decayPoint)],-1)))/2
  #AUC<-flux::auc(Timedf[1:(pointmax+decayPoint)], curve[1:(pointmax+decayPoint)], thresh = decayAbs)
  
  #some safety net for insufficient lysis
  #ifelse(downAbs[length(downAbs)] >=pcChange, lysPoint <- lastPoint, lysPoint <- decayPoint+pointmax)
  ifelse(downAbs[length(downAbs)] >=pcChange, decayTime <- NA, decayTime <- decayTime)
  
  # downcurve<-c(decayTime, decayTime.i,decayAbs, decayAbs.i, decayPoint, pointmax, AUC, pointmin)
  downcurve<-c(decayTime, decayTime,decayAbs, decayAbs, decayPoint, pointmax, AUC, pointmin)
  
}

