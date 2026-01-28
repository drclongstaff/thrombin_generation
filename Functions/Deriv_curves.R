#Make derivative curve and conversion of F to [Thrombin]

##More transformations

#Make the derivative curves
fun_diff<-function(d1, plateF){
  t1 <- plateF[[1]]
  res<-diff(d1)/diff(t1)
  res
}

#Converting Fluorescence to Thrombin
fun_FtoT<-function(Th, CalibT, calSlope){
  nowT<-CalibT*(Th/calSlope)
}