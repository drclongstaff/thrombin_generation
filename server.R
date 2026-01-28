library(shiny)                             # Load the Shiny library

#Code to increase size of downloaded files
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output) {      # Set up the Shiny Server
  
  ###CALIBRATOR DATA
  #Load calibrator data
  CalibF0<-reactive({
    inputFile <- input$data0
    if (is.null(inputFile)) 
    read.csv("./Data/CalibratorsA.csv")
    else(
      load_file(input$data0$name, input$data0$datapath, input$sheetc, input$skipc) %>% 
        clean_names() %>% 
        remove_empty( which=c("rows", "cols"), 
                      cutoff=1, quiet=TRUE) |> #remove empty cols and rows
        sapply( \(x) replace(x, x  %in% "", NA)) |> #replace empty cells with NA
        as.data.frame()
    )
    
  })
  
  #Generate the calibrator dataframe and add means
  select_cols <- reactive({
    req(input$select_cols)
    sel <- as.numeric(strsplit(input$select_cols, ",")[[1]])
    sel
  })
  #make list of wells to show
  col_names <- reactive({
    req(input$select_cols, data())
    colnames(data())[select_cols()]
  })
  
  CalibF <- reactive({
    myCals00 <- CalibF0() %>% select(1, select_cols()) 
    #myCals0 <- lapply(myCals00, function(n) Zerod_fun(n)) |> data.frame()
    myCals <- myCals00 %>% mutate(means=rowMeans(myCals00[-1]))
    myCals[1:input$limitD,]
  })
 
  #Get calibrator column names
  varCal<- reactive({
    calCols<-colnames(CalibF()[-1]) #remember to remove the first Time column
  })
  
  #Choose calibrator to analyse  
  output$whatCal<-renderUI({
    selectInput("calCols",
                label= "curve",
                choices = varCal())
  })
  
  #CalibDat is the selected calibration curve
  CalibDat<-reactive({
    #req(CalibF())
   CalibDat <- data.frame("Time"=CalibF()[[1]], "F"=CalibF()[[input$calCols]])
  })
  
  #Fit the polynomial for the calibrator raw data and get coefficients
  Poly4<-reactive({
    fun_coeffs(CalibDat()[, 1], CalibDat()[, 2])
    #fun_coeffs(CalibF()[, 1], CalibF()[, 2])
  })
  
  #Get values for slope and intercept of fastest or initial rates
  SlopeInt1 <- reactive({
    A <- Poly4()[1] 
    B <- Poly4()[2]
    C <- Poly4()[3] 
    D <- Poly4()[4] 
    E <- Poly4()[5] 
    f <- expression(A+B*x+C*x^2+D*x^3+E*x^4)
    df <- D(f,'x')
    #x <- ifelse(CalibDat()[,1]==0, CalibDat()[, 1], c(0,CalibDat()[, 1]))
    #x <- c(0,CalibDat()[, 1]) #this adds a zero to the calibdat time
    x <- CalibDat()[, 1] #changed from CalibDat()
    #x <- 0 #should we use the first point or zero
    newvo <- max(eval(df), na.rm = TRUE) #finds max slope if not at origin
    #x <- x[1]
    x <- 0
    newint <- eval(f) #finds value at origin
    #will return intercept and slope to generate the fastest (or initial) rate
    as.numeric(c(newint, newvo))
        })
  
 
  # yp is the perfect straight line of the initial slope
  yp <- reactive({
    ypc<-SlopeInt1()[1]+CalibDat()[,1]*SlopeInt1()[2] #corrected y
                })
 
  #Polynomial fitting get coefficients
  #Poly5 is from the plot of raw calibrator data and initial slope yp
  #Will be used to correct all the sample curves
  Poly5<-reactive({
    #fun_coeffs(CalibF()[, 2], yp())
    fun_coeffs(CalibDat()[, 2], yp())
  })
 
  #Polynomial correction function
  #Uses the eval function and the coefficients from Poly5
  #The argument y is supplied by the Purrr operations below
  fun_poly <- function(y){
    
    fun_eval(y, Poly5()[1] , Poly5()[2], Poly5()[3], Poly5()[4], Poly5()[5])
  }
  
  #Corrects the raw calibdat using the Poly5 coefficients
  #fun_poly can be used when y is supplied with the poly5 coefficients
  yideal_P <- reactive({
    fun_poly(CalibDat()[, 2])#replaced CalibDat()
    #fun_eval(CalibDat()[, 2], Poly5()[1] , Poly5()[2], Poly5()[3], Poly5()[4])
  })
  
  #To generate the thrombin curve for the calibrator
  #changed CalibDat to CalibF
 CalibThrombin <- reactive ({
   CalibThrombin<-CalibDat()[-1] %>%  
    map_df(~fun_poly(.x)) %>% 
    map_df(~fun_FtoT(.x, input$CalibT, input$calSlope)) %>% 
    add_column(CalibDat()[,1], .before = 1) %>% as.data.frame()
   diffres <- diff(CalibThrombin[,2])/diff(CalibThrombin[,1])
   Tdiff <- CalibThrombin[,1][-1]
   Calibd <- data.frame(cbind(Tdiff, diffres))
   Calibd[nrow(Calibd)+1,] <- c(NA, NA)
   Calibd
 }) 
  
   #Graph of fitted calibrator 
   #changed CalibDat to CalibF 
  output$myCalib<-renderPlot({
    #req(CalibDat())
    #if(is.null(input$calCols)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    minX <- as.double(min(CalibDat()[1], na.rm = TRUE))
    minY <- as.double(min(CalibDat()[2], na.rm = TRUE))
    maxX <- as.double(max(CalibDat()[1], na.rm = TRUE))
    maxY <- as.double(max(CalibDat()[2], na.rm = TRUE))
    #Generate the fitted curve for the raw data
    yfit <- fun_eval(CalibDat()[,1], Poly4()[1], Poly4()[2], Poly4()[3], Poly4()[4], Poly4()[5] )
    #Draw the graphs
    plot(CalibF()[,1], CalibF()[, input$calCols], #raw data 
         pch="+", xlim = c(minX, maxX), ylim=c(minY, maxY), xlab = "Time", ylab = "Fluorescence",
         main =  paste("Calibrator : maximum slope =", round(SlopeInt1()[2], 4)))
    points(CalibF()[,1], yp(), col = "red") #inital or fastest rate
    lines(CalibF()[,1], yfit, col = "magenta") #polynomial fit to raw data
    points(CalibF()[,1], yideal_P(), pch="x", col = "olivedrab") #the corrected raw data to match the initial/fastest rate
    legend("bottomright", legend=c("raw","fit", "initial rate", "corrected"), pch=c("+","_", "O", "X"), col = c("black","magenta", "red",  "olivedrab"))
  })
  
  #Calibrator-test of fitting-plots
  output$mytest <- renderPlot({
    #req(CalibDat())
    #if(is.null(CalibDat())){return(NULL)}
    par(mfrow = c(2,2))  # Split the plotting panel into a 2 x 2 grid
    #Fit the polynomial again to get the $fitted and $residuals and plot
    Yobs<-CalibDat()[, 2]
    TcF<-CalibDat()[, 1]
    Fitpoly4 <- fun_POLYN(TcF, Yobs)
    plot(Fitpoly4$fitted, Fitpoly4$residuals, xlab="Fitted", ylab="Residuals", bty="n")
    abline(h=0, lty=2, col="red") #add red line at zero to help view systematic errors
    #Plot stability of thrombin calibration
    plot(CalibThrombin()[,1], CalibThrombin()[,2], type="l", col="blue", xlab="Time", ylab="Thrombin nM", bty="n")
    abline(h=input$CalibT, lty=2, col="red")
    #Plot cumsum of residuals for interrogation of systematic deviations in $residuals
    plot(CalibThrombin()[,1], cumsum(Fitpoly4$residuals), xlab="Time", ylab="cusum residuals", bty="n")
    abline(h=0, lty=2, col="red")
    #Plot qqnorm to test for normal distribution of errors
    qqnorm(Fitpoly4$residuals, main="", bty="n")
    qqline(Fitpoly4$residuals, lty=2)
                    })
  
  
  ###TEST DATA
  ##Read raw fluorescence data
  RawF0 <- reactive({
    inputFile <- input$data1
    if (is.null(inputFile)) 
    read.csv("./Data/NewDat3.csv")#[c(1, 2:29)]
    else(
      load_file(input$data1$name, input$data1$datapath, input$sheetd, input$skipd) %>% 
        clean_names() %>%
        remove_empty( which=c("rows", "cols"), 
                              cutoff=1, quiet=TRUE) |> #remove empty cols and rows
        sapply( \(x) replace(x, x  %in% "", NA)) |> #replace empty cells with NA
        as.data.frame()
    )
    
  })
  select_samples <- reactive({
    req(input$select_samples)
    sel <- as.numeric(strsplit(input$select_samples, ",")[[1]])
    sel
  })
  #make list of wells to show
  samples_names <- reactive({
    req(input$select_samples, RawF0())
    colnames(data())[select_samples()]
  })
  
  #This is truncated or columns chosen 
  #and is responsive when the truncation is changed
  RawF <- reactive({
    RawF0A <- RawF0()[c(1, select_samples())]
    RawF1 <- RawF0A[1:(nrow(RawF0())-input$truncpoints),]
  })
  
  output$value <- renderUI({ print(14) })
 
   #This is for the settings table state calibrators 
  output$calset<- renderUI({
    inputFile <- input$data0$name
    if (is.null(inputFile)) 
      return(basename("./Data/CalibratorsA.csv"))
    else(basename(inputFile))
  }) 
  
  #This is for the settings table raw data
  output$fileset<- renderUI({
    inputFile <- input$data1$name
    if (is.null(inputFile)) 
      return(basename("./Data/NewDat3.csv"))
    else(basename(inputFile))
  })

  
  ##Corrections for raw data using polynomial function above
  RawFP <- reactive ({
    RawF <- RawF()
    plateFP<-RawF[-1] %>%  
      map_df(~fun_poly(.x)) %>% add_column(RawF[,1], .before = 1) %>% as.data.frame()
  })
 
  #These are the plots of raw data with or without polynomial correction
  output$myplotsF<-renderPlot({
    if(is.null(input$calCols)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    TimeF<-RawF()[,1]
    #DatwellsF<-RawF()[,-1]
    DatwellsF<-RawF() %>% select(-1)
    
    absWellsF<-length(DatwellsF[1,])
    samplesF<-names(DatwellsF)
    maxF<-max(DatwellsF)
    maxT<-max(TimeF)
    
    RowNumF<-input$numrows
    
    par(mfrow=c(RowNumF,(absWellsF/RowNumF)))
    par(mar=c(0.6,3.2,0.2,0.2)) # dimensions for figure
    
    switch(input$Transf,
           "none"=plateFT<-RawF(),
           "Polynomial"=plateFT<-RawFP())
    
    #for (f in 1: absWellsF ) {
    for(f in seq_len(absWellsF)){ 
      yi<- DatwellsF[,f] 
      
      plots<-plot(TimeF, yi, type = "l", col= "blue", lwd = 2, xlim= c(0, maxT), 
                  ylim=c(0, max(plateFT[-1])),    axes=FALSE)
      axis(1, seq(0, maxT, maxT), pos=0)
      axis(2, seq(0,maxF,maxF),las=2, pos=0,cex.axis=1.2, col.axis="red")
      lines(plateFT[,1], plateFT[,f+1],lwd=1, col = "red")
      text(maxT*.1, maxF*.8, samplesF[f])
    }

  })
  
  #Print a table of the polynomial coefficients (optional) 
  output$Fluor<-renderTable({
    Pco<-matrix(c(Poly5()[1], Poly5()[2], Poly5()[3] , Poly5()[4]), Poly5()[5], nrow = 1) #,  ,Poly5()[6]
    colnames(Pco)<-c("x0", "x1", "x2", "x3") #"x4")#, "x5"
    formatC(Pco)
  })
 
  #Generates the type of raw data
  #As raw fluorescence diff or converted to thrombin or with subtracted T-a2M
  #With or without smoothing
  readData <- reactive({
    
    #Use raw fluorescence or with polynomial correction
    switch(input$Transf,
           "none"=plateFC<-RawF(),
            "Polynomial"=plateFC<-RawFP()
           ) 
    #The first derivative curves
    plateFd<-plateFC[-1] %>%  #select the raw fluorescent curves to make derivatives (d)
      map_df(~fun_diff(.x, plateFC)) %>% add_column(plateFC[,1][-1], .before = 1) %>% as.data.frame()
    #The first derivative curves are converted to thrombin (T)
    plateFdT <- plateFd[-1] %>% 
      map_df(~fun_FtoT(.x, input$CalibT, input$calSlope)) %>% add_column(plateFd[[1]], .before = 1) %>% as.data.frame()
    #The Thrombin curves with late smoothing
    plateFdTs <- plateFdT[-1] %>% 
      map_df(~fun_latesmooth(.x, plateFdT,input$smtail )) %>% add_column(plateFdT[[1]], .before = 1) %>% as.data.frame()
    #The thrombin curves with all smoothing
    plateFdTa <- plateFdT[-1] %>% 
      map_df(~fun_allsmooth(.x, plateFdT)) %>% add_column(plateFdT[[1]], .before = 1) %>% as.data.frame()
    #The late smooth Thrombin curves with correction for alpha2M
    plateFdTsM <- plateFdTs[-1] %>% 
      map_df(~fun_TGcorr(.x, plateFdTs, input$smtail)) %>% add_column(plateFdTs[[1]], .before = 1) %>% as.data.frame()
    
    #Which curve to be displayed
    switch(input$a2Mcor, 
           "F"=plateN <- plateFd, #first derivative of fluorescence
           "Thrombin"=plateN <- plateFdT, #converted to thrombin
           "Smooth.all" = plateN <- plateFdTa,#allsmooth curve?
           "Smooth.tail" = plateN <- plateFdTs, #latesmooth curve?
           "-T-alpha-2M" = plateN <- plateFdTsM)
    #write_clip(plateN)
    plateN
    
  })
  
  #Generate the T-alpha-2-M-curve
  a2MData <- reactive({
    #plateM <- RawF() #start with fluorescence data
    
    switch(input$Transf,
           "none"=plateMC<-RawF(),
          # "H-transform"=plateMC<-RawFH(),
           "Polynomial"=plateMC<-RawFP()
          ) 
    #Convert raw fluorescence before or after polynomial correction to derivative->thrombin
    plateMdT <- plateMC[-1]  %>%  
      map_df(~fun_diff(.x, plateMC)) %>% map_df(~fun_FtoT(.x, input$CalibT, input$calSlope)) %>% add_column(plateMC[,1][-1], .before = 1) %>% as.data.frame()
    #Use the drivative->thrombin curve to make the alpha2M curve
    plateMdTM <- plateMdT[-1] %>% 
      map_df(~fun_TaM(.x, plateMdT, input$smtail)) %>% add_column(plateMdT[[1]], .before = 1) %>% as.data.frame()
  })
 
###DO THE ANALYSIS ON THE CURVES
  ##Various functions
  
  TabRes <- reactive({
    
  #functions uppy and downy are before the server code   
  
  pclag<- input$lagChange*0.01 #changes the number to %
  myDatcorrTaMT <-readData()
  Timedf<-myDatcorrTaMT[,1]
  
  TabResShort<-myDatcorrTaMT[-1] %>%  map_df(~ data.frame(Ao=uppy(.x, Timedf, pclag)[1], #2 minAbs= positions in table after column name
                                                          Lag=uppy(.x, Timedf, pclag)[2], #3 start time
                                                          AUC=downy(.x, Timedf, pclag)[7], #4 AUC
                                                          Peak=uppy(.x, Timedf, pclag)[8], #5 max F or thrombin
                                                          ttPeak=uppy(.x, Timedf, pclag)[7], #6 time to max
                                                          ttTail=downy(.x, Timedf, pclag)[2], #7 time to decay point (10%)
                                                          LagReading=uppy(.x, Timedf, pclag)[3], #8 F or Thrombin at lag
                                                          Startpoint=uppy(.x, Timedf, pclag)[5], #9 point at end of lag
                                                          Pointmax=uppy(.x, Timedf, pclag)[6], #10 point at max
                                                          DecayPoint=(uppy(.x, Timedf, pclag)[6])+(downy(.x, Timedf, pclag)[5]), #11 decay point 
                                                          MinPoint=downy(.x, Timedf, pclag)[8], #12 point to return to baseline
                                                          Base=(downy(.x, Timedf, pclag)[2]-uppy(.x, Timedf, pclag)[2]), #13 time between lag and return to lag reading
                                                          LagtoPeak =uppy(.x, Timedf, pclag)[7]-uppy(.x, Timedf, pclag)[2])) %>% #14 time between end of lag and peak
    add_column(Sample=names(myDatcorrTaMT[-1]), .before = 1)
  #clipr::write_clip(TabResShort) #REMOVE BEFORE UPLOAD
  TabResShort
  })
  
  output$resultsTable<-renderTable({
    
    
    TabRes()[,c(1:8, 13,14)]
    
  })
  
output$plotsTable<-renderTable ({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    RowNum<-input$numrows
    
    
    data<-switch(input$tabRes, 
                 "1"=matrix(TabRes()[,1], byrow=TRUE, nrow=RowNum),
                 "2"=matrix(TabRes()[,2], byrow=TRUE, nrow=RowNum),
                 "3"=matrix(TabRes()[,3], byrow=TRUE, nrow=RowNum),
                 "4"=matrix(TabRes()[,4], byrow=TRUE, nrow=RowNum),
                 "5"=matrix(TabRes()[,5], byrow=TRUE, nrow=RowNum), 
                 "6"=matrix(TabRes()[,6], byrow=TRUE, nrow=RowNum),
                 "7"=matrix(TabRes()[,7], byrow=TRUE, nrow=RowNum), 
                 "8"=matrix(TabRes()[,8], byrow=TRUE, nrow=RowNum),
               

    )
    
    colnames(data) =as.character(1:(length(data)/RowNum))
    data
    
  })

##Plot of single curve
#Also need to select well

var<- reactive({
  colnames(readData()[-1])
})



output$what<-renderUI({
  selectInput("colmnames",
              label= h5("Select a column of absorbance data"),
              choices = var())
}) 

output$nowwhat<-renderUI({
  selectInput("colmnames",
              label= h5("Copy a column of absorbance data"),
              choices = var())
}) 

##Plot the curves
output$myplotAll<-renderPlot({
  #if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
  plate0 <- readData()
  plateM<- a2MData()
  
  Time<-round(plate0[,1], 2)
  #Datwells<-plate0[,-1]
  Datwells<-plate0 %>% select(-1)
  # maxy <- max(Datwells)
  pointsnum<-length(Datwells[,1])-1
  absWells<-length(Datwells[1,])
  samples<-names(Datwells)
  
  initiation<-input$num
  RowNum<-input$numrows
  
  par(mfrow=c(RowNum,(absWells/RowNum)))
  par(mar=c(0.2,0.2,0.2,0.2)) # dimensions for figure
  
  mint<-min(Time)
  for(k in seq_len(absWells)){
  #for (k in 1: absWells ) {
    
    yi<- Datwells[,k] 
    #t<-Time
    
    plots<-plot(Time, yi, type = "l", col= "slategrey", lwd = 3, xaxt="n", yaxt="n",  ylim = c(min(Datwells), max(Datwells)))
    lines(plateM[,1], plateM[,k+1],lwd=1, col = "orange")
    #lines(platedT()[,1], platedT()[, k+1], lty=2, col = "green")#shows thrombin curve
    lines(Time[TabRes()[k,9]:TabRes()[k,10]], yi[TabRes()[k,9]:TabRes()[k,10]],col="green", lwd=3)
    lines(Time[TabRes()[k,10]:TabRes()[k,11]], yi[TabRes()[k,10]:TabRes()[k,11]],col="blue", lwd=3)
    lines(Time[TabRes()[k,11]:TabRes()[k,12]], yi[TabRes()[k,11]:TabRes()[k,12]],col="red", lwd=2)
    
    switch(input$tabRes,
           "2"= abline("h"=TabRes()[k,2], col = "black", lty=2),
           "5"= abline("h"=TabRes()[k,5], col = "black", lty=2),
           "3"= abline("v"=TabRes()[k,3], col = "black", lty=2),
           "6"= abline("v"=TabRes()[k,6], col = "black", lty=2),
           "7"= abline("v"=TabRes()[k,7], col = "black", lty=2),
           "8"= abline("h"=TabRes()[k,8], col = "magenta", lty=2),
           "10"= abline("v"=TabRes()[k,10], col = "blue", lty=1),
           "4"=polygon(Time[TabRes()[k,9]: TabRes()[k,11]], yi[TabRes()[k,9]: TabRes()[k,11]], col = "khaki")
           #"4"=polygon(Time[1: TabRes()[k,12]], yi[1: TabRes()[k,12]], col = "khaki")
           
    )          
  }
  
})

#plot for single curve
output$myplot<-renderPlot({
  if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
  plate0 <- readData()
  plateM<- a2MData()
  
  TaM<-plateM[,input$colmnames]
  
  Time<-round(plate0[,1], 2)
  #Datwells<-plate0[,-1]
  Datwells<-plate0 %>% select(-1)
  
  yi<-Datwells[,input$colmnames] # Rather than get and attach, this just reads in the column chosen earlier
  
  k<-which(TabRes()[, 1]==input$colmnames)
 
  mint<-min(Time)
  
 
    
    plots<-plot(Time, yi, type = "l", col= "slategrey", lwd = 3, ylim = c(min(Datwells), max(Datwells)*1.1), ylab = "Thrombin or Fluorescence")
    lines(plateM[,1], plateM[,k+1],lwd=1, col = "orange")
    #lines(platedT()[,1], platedT()[, k+1], lty=2, col = "green")
    lines(Time[TabRes()[k,9]:TabRes()[k,10]], yi[TabRes()[k,9]:TabRes()[k,10]],col="green", lwd=3)
    lines(Time[TabRes()[k,10]:TabRes()[k,11]], yi[TabRes()[k,10]:TabRes()[k,11]],col="blue", lwd=3)
    lines(Time[TabRes()[k,11]:TabRes()[k,12]], yi[TabRes()[k,11]:TabRes()[k,12]],col="red", lwd=2)
    
  
           "2"= abline("h"=TabRes()[k,2], col = "black", lty=2)
           "5"= abline("h"=TabRes()[k,5], col = "black", lty=2)
           "3"= abline("v"=TabRes()[k,3], col = "black", lty=2)
           "6"= abline("v"=TabRes()[k,6], col = "black", lty=2)
           "7"= abline("v"=TabRes()[k,7], col = "black", lty=2)
           "8"= abline("h"=TabRes()[k,8], col = "magenta", lty=2)
           #"10"= abline("v"=TabRes()[k,10], col = "blue", lty=1)
           # "4"=polygon(Time[TabRes()[k,9]: TabRes()[k,11]], yi[TabRes()[k,9]: TabRes()[k,11]], col = "grey80")
           #"4"=polygon(Time[1: TabRes()[k,12]], yi[1: TabRes()[k,12]], col = "grey90")
           
  
})

output$curveTable<-renderTable({
  if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
  TabNames<-colnames(TabRes())
  ColNam<-c("Parameter", "Result")
  
  All.Res<-TabRes() %>% filter(Sample == input$colmnames) %>% select(1:7)
  All.Res.Tab<-cbind(TabNames[1:7], t(All.Res))
  colnames(All.Res.Tab)<-ColNam
  
  All.Res.Tab
})


output$rawData<-renderTable({
  rawData <- switch(input$whatRaw,
                    "Raw fluorescence"= RawF(),
                    "Analysed" =  readData()
                    )
 #clipr::write_clip(rawData)#remove for online version
  rawData
})

output$calData <- renderTable({
  calData <- switch(input$whatCal,
                    "Raw Calibrator"= CalibF0(),
                    "Selected" =  CalibF()
  )
  calData 
  
  
})

setsTab<-reactive({
  
  setTable<-matrix(c( 
   # "Date", format(Sys.Date(), "%d %b %Y"), "Time", format(Sys.time(), "%X"),
    
    #"Read interval s", round(RawF()[2,1]-RawF()[1,1], 4),  "% Change for end of lag", input$lagChange,
    
    
    "", "____Calibrator_settings____", "", "",
    
    "sheet", input$sheetc, "Read interval s", round(RawF()[2,1]-RawF()[1,1], 4),
   
    "Selected cols", input$select_cols,  "Chosen calibrator", input$calCols,
    
    "Points for fit", input$limitD, "Concentration of calibrator", input$CalibT,
    
     "Calibrator rate", input$calSlope, "", "",
    
    "", "____Data_analysis____", "","",
    
    "sheet", input$sheetd, "Read interval s", round(CalibF()[2,1]-CalibF()[1,1], 4), 
   
    "selected cols", input$select_samples, "", "",
    
    "Truncate data", input$truncpoints, "Points to smooth tail", input$smtail,
    
    "Correction method", input$Transf, "T-alpha-2M correction", input$a2Mcor

  ),
  
  byrow=TRUE, nrow=10)
  
  colnames(setTable)<-c("Parameter", "Value", "Parameter", "Value")
  setTable
})



output$settings<-renderTable({
  setTable<-setsTab()
  
  setTable
})
output$text4 <- renderText({
  This_session[[1]]
})

output$text5 <- renderText({
  This_session[[2]]
})

output$session <- renderTable({
  
  # Extract other packages with versions
  other_packages <- session$otherPkgs
  if (!is.null(other_packages)) {
    other_pkg_info <- data.frame(
      Package = names(other_packages),
      Version = sapply(other_packages, function(x) x$Version),
      stringsAsFactors = FALSE
    )
  } else {
    other_pkg_info <- data.frame(Package = character(0), Version = character(0))
  }
})



})
