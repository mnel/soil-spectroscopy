## some functions required for the calibration report for the RaCA project.

## load and rename

loadRename <- function(.fname, .name,...){
## a function that loads a saved R object from a file
## and allows you to rename it 
## .fname = the file name
## .name the name of the object saved within that file  
  load(as.character(.fname))
  .df <- get(as.character(.name))
  return(.df)
}



saveObjectAs <- function(object,.fname, .name){
## a function that allows you to save an object to file, with a different name to 
## that within your workspace
## object = an 'R' object
## .fname = the file name you wish to save it as
## .name = the name within that file
  assign(.name,object)
  save(list = .name, file =.fname)
  }

save_as <- function(.object, .name, .file_path, .ext = '.RData'){
 ## a function that allows you to save an object to file, with a different name to 
## that within your workspace
## object = an 'R' object
## .fname = the file name you wish to save it as
## .name = the name within that file
  assign(.name, .object)
  save(list = .name, file = file.path(as.character(.file_path), paste(as.character(.name), as.character(.ext), sep = ''))) 
  
}


table_to_data.frame <- function(.x, .id = 'id'){
  ## create table
  table_x <- table(.x)
  ## make data.frame
  .df <- data.frame(.names = names(table_x), count = as.numeric(table_x))
  names(.df)[1] <- .id
  return(.df)
}

## table to data frame
tabDF <- function(x, ind = 'ind'){
    ret <- data.frame(nam = names(x), count = as.numeric(x))
    names(ret)[1] <- ind
    ret
  }

## A function that gives the length of a vector without the NAs
 lengthNA <- function(x){length(na.omit(x))}

## 
length_NA <- function(.vector){length(na.omit(.vector))}


## a function to find the number of profiles with data
 nUniqueProfiles <-  Vectorize(function(.data,.var, .profile = 'userPedonId'){
    length(unique(.data[!is.na(.data[,.var]),.profile]))
  }, vectorize.args ='.var')

n_unique_ids <- Vectorize(function(.data, .var, .id){
  length(unique(.data[!is.na(.data[,.var]), .id]))
}, vectorize.args = '.var')

##function to read in spectra from a file

readSpectra <- function(vnirNames, filePath, extension = '.txt'){
  ## a function to read in from a .csv file, the second column containing the values
  readFun <- function(x){read.csv(file.path(filePath, paste(x, extension, sep = '')), header = T)[,2]}
  ## apply to a list
   sList <-llply(vnirNames,readFun)
  spectra <- do.call('rbind',sList)
}




average_in_interval <- function(.spectra, .all, .in_interval){
  
}

## A function to convert spectra to rgb
spectraToRGB <- function(spectra){

  blue <- mean(spectra[350:2500%in%(450:520)])
  red <- mean(spectra[350:2500%in%(600:690)])
  green <- mean(spectra[350:2500%in%(520:600)])
  colour <- rgb(red,green,blue)
 data.frame(red,green,blue,colour)
}


propertiesLatex <- Vectorize(function(.name){
switch(as.character(.name) ,
  'clay' = 'Clay', 'sand' = 'Sand', 'totalC' = 'Total C', 
  'estOrgC' = 'Organic C', 'cecClayRatio' = 'CEC:Clay', 
  'pHWater' = 'pH (Water)', 'pHCaCl' = 'pH $ \\left( {\\rm CaCl_2 } \\right)$',
   'cecNH4' = 'CEC', 'feCD' = 'Iron Oxide', 'CaCO3' = '${\\rm CaCO_3}$'      ) 
})

getDatasetPLS <- function(.name){
  library(Hmisc)
  load(file.path('savedData','models', paste(.name,'PLSTrainingInfo.RData',sep ='')))
  trainingInfo <- get( paste(.name,'PLSTrainingInfo',sep=''))
  if(class(trainingInfo) == 'try-error'){stop(paste('No training info found for ', .name, '\n'))}
  training <- trainingInfo$training
  whichData <- trainingInfo$whichData
  
  
  trainingReflect <- data.frame(.var = soilData[training,.name], reflectance = I(as.matrix(reflect10SGD1[training,keep10])))
  names(trainingReflect)[1] <- .name
  trainingAbs <- data.frame(.var = soilData[training,.name], abs = I(as.matrix(abs10SGD1[training,keep10])))
  names(trainingAbs)[1] <- .name  
  validReflect <- data.frame(.var = soilData[-training,.name], reflectance = I(as.matrix(reflect10SGD1[-training,keep10])))
  names(validReflect)[1] <- .name
  validAbs <- data.frame(.var = soilData[-training,.name], abs = I(as.matrix(abs10SGD1[-training,keep10])))
  names(validAbs)[1] <- .name  
                                                                           
  llist(trainingReflect, trainingAbs, validReflect, validAbs)     
                                                                           
  
}


makeTrainingInfo <- function(.name, .allData, .savePath,.rate,...){
    ## organize training info 
    .data <- .allData[,.name]
    ## find the NAs and deal appropriately (remove from sampling pool)
      whichNa <- attr( na.omit(.data) , 'na.action' )
      all <-  1:length(.data)
      if(is.null(whichNa)) {
              whichData <- all} else{ 
              whichData <-  all[ -whichNa]}
     ## create the training and validation  datasets
    training <- sample(whichData, floor(.rate *length (whichData)) )
    validation <- whichData[-training]
    .dataTrain <- .data[training]
    .dataValidate <- .data[validation]
    ## stor the issues
    trainingInfo <- llist(training, validation, whichData, .data, .dataTrain, .dataValidate )
    ## rename and save
    trainingInfo.name <- paste(.name,'PLSTrainingInfo',sep ='')
    trainingInfo.fname <- file.path(.savePath, paste(trainingInfo.name, '.RData',sep=''))
    saveObjectAs(trainingInfo, .fname = trainingInfo.fname, .name = trainingInfo.name)
    data.frame(property = .name,trainingInfo.fname,trainingInfo.name, sampling = paste('.rate',.rate,sep='') )
    }

plsFit <- function(property, spectra.fname, spectra.name, trainingInfo.fname, trainingInfo.name, filter, .keep, 
                   sampling, .savePath,...){
   ## load training info  
   trainingInfo <- loadRename(trainingInfo.fname, trainingInfo.name )
    ## load reflectance data
    spectra <- loadRename(spectra.fname,spectra.name )
     ## organize data
    plsData <- data.frame(trainingInfo$.dataTrain, spectra = I(as.matrix(spectra[trainingInfo$training,.keep])))
    names(plsData)[1]  <- as.character(property)
    ## fit model
    plsModel <- plsr(as.formula(paste(as.character(property), '~', 'spectra', sep = ' ')), data = plsData)
    ## rename and save
    model.name <- paste(.name,'PLS',.type,sep ='')
    model.fname <- file.path(.savePath, paste(model.name, '.RData', sep = '' ))
  
   saveObjectAs(plsModel, .fname = model.fname, model.name = model.name)
   ## tidyup 
   rm(list = c('plsData','spectra','plsModel','trainingInfo'))
    gc()
      
   data.frame(property, model.fname,model.name, spectra = .type, sampling)
  }




plsrSummary <- function(.name){
  ## load model
  modelReflect <- loadRename(file.path('savedData','models', paste(.name,'PLSReflect.RData',sep ='')), paste(.name,'PLSReflect',sep =''))
  ## load training data
  trainingInfo <- loadRename(file.path('savedData','models', paste(.name,'PLSTrainingInfo.RData',sep ='')),paste(.name,'PLSTrainingInfo',sep ='') )
  ## load spectra
  spectraReflect <- loadRename('savedData/reflect10SGD1.RData', 'reflect10SGD1')
  validationReflect <- data.frame(trainingInfo$.dataValidate, spectra = I(as.matrix(spectraReflect[trainingInfo$validation,keep10])))
  names(validationReflect)[1] <- .name
  ## Calculate statistics for training data set
  reflectResultsTrain <- data.frame(components = 0:206, 
        rmsep = RMSEP(modelReflect)$val[,,1:207],
        r2 = pls::R2(modelReflect)$val[,,1:207],
        type = 'training')
  ## calculate validation statistics
  reflectResultsValid <- data.frame(components = 0:206,
        rmsep = RMSEP(modelReflect, newdata = validationReflect)$val[,,1:207], 
        r2 = pls::R2(modelReflect,newdata = validationReflect)$val[,,1:207],
        type = 'validation')
  ## load absorbance model  
  modelAbs <-loadRename(file.path('savedData','models', paste(.name,'PLSAbs.RData',sep ='')), paste(.name,'PLSAbs',sep ='') )
  ## load absorbance data
   spectraAbs <- loadRename('savedData/abs10SGD1.RData', 'abs10SGD1')
  validationAbs <- data.frame(trainingInfo$.dataValidate, spectra = I(as.matrix(spectraAbs[trainingInfo$validation,keep10])))
  names(validationAbs)[1] <- .name
  
  ## Calculate statistics for training data set
  absResultsTrain <- data.frame(components = 0:206, 
        rmsep = RMSEP(modelAbs)$val[,,1:207],
        r2 = pls::R2(modelAbs)$val[,,1:207],
        type = 'training')
  ## calculate validation statistics
  absResultsValid <- data.frame(components = 0:206, 
        rmsep = RMSEP(modelAbs, newdata = validationAbs)$val[,,1:207],
        r2 = pls::R2(modelAbs,newdata = validationAbs)$val[,,1:207],
        type = 'validation')
  ## organize results  
  returning <- data.frame(rbind( reflectResultsTrain, reflectResultsValid,
                                absResultsTrain, absResultsValid),
                          using = as.factor(rep(c('reflectance', 'absorbance'), each= (207*2)
                                                )
                                            )
                          )
    
  returning$id <- .name
  returning$latexId <- propertiesLatex(.name)
  returning
}


cubistFit <- function(.name, argList = list(x = 'reflect10SGD1', .fname = 'savedData/reflect10SGD1.RData', .type = 'Reflect')){
  trainingInfo <- loadRename(file.path('savedData','models', paste(.name,'PLSTrainingInfo.RData',sep ='')),paste(.name,'PLSTrainingInfo',sep ='') )
  .dataTrain <- trainingInfo$.dataTrain
    ## load reflectance data
    spectra <- loadRename(argList$.fname,argList$x )
     ## organize data
    cubistX <- data.frame(spectra[trainingInfo$training,keep10])
    names(cubistX) <-      paste('w',seq(400,2450,by=10),sep='')
    ## cubist model reflectance 
     cubistModel <- cubist(x = cubistX, y = .dataTrain)
    ## fit model
  ## rename and save
    assign(paste(.name,'Cubist', argList$.type,sep =''), cubistModel)
      ## save objects
    save(list = paste(.name,'Cubist', argList$.type,sep =''),
         file = file.path('savedData','models', paste(.name,'Cubist', argList$.type,'.RData',sep ='')))
}

cubistSummaryHelper <- function(.name,.type){
  ## load model
  cubistModel <- loadRename(file.path('savedData','models', paste(.name,'Cubist', .type,'.RData',sep ='')),paste(.name,'Cubist', .type,sep =''))
  ## load training info
  trainingInfo <- loadRename(file.path('savedData','models', paste(.name,'PLSTrainingInfo.RData',sep ='')),paste(.name,'PLSTrainingInfo',sep =''))
  ## load spectra
  if(.type == 'Reflect'){argList <- list( x = 'reflect10SGD1', .type ='Reflect', .fname = 'savedData/reflect10SGD1.RData')}
  if(.type == 'Abs'){argList <- list( x = 'abs10SGD1', .type ='Abs', .fname = 'savedData/abs10SGD1.RData')}
  spectra <- loadRename(argList$.fname,argList$x )
   ## organize data
   allData <- data.frame(spectra[,keep10])
  names(allData) <-      paste('w',seq(400,2450,by=10),sep='')
  ## make predictions
  predCubist <- predict(cubistModel, newdata = allData)
    ## create data frame with truth
  resultsDF <- data.frame(truth = trainingInfo$.data, prediction = predCubist, using = .type)
  ## get type
   resultsDF$type <- 'xx'
   resultsDF$type[is.na(trainingInfo$.data)] <- 'No Data'
   resultsDF$type[trainingInfo$training] <- 'training'
   resultsDF$type[trainingInfo$validation] <- 'validation'
  
  return(resultsDF)
}
  

cubistSummary <- function(.name){
  ## use the helper function
   reflectDF <- cubistSummaryHelper(.name,.type='Reflect')
   absDF <-  cubistSummaryHelper(.name,.type='Abs')
  assessDF <- rbind(reflectDF, absDF)
  ## calculate MSE
  mseValues <- tapply(X=apply(assessDF[,1:2],1,function(x){diff(x)^2}),INDEX = with(assessDF, interaction(type,using)),FUN = mean)
  data.frame(t(mseValues))
   
  }


library(KernSmooth)
cHullDeviation <- function(.spectra, wavelengths = 350:2500, rangeB = 350:2500){
  ## organize data
  inData <- wavelengths %in% rangeB
  .data <- sortedXyData(wavelengths[inData], .spectra[inData])
  ## calculate convex rull
  cHull <- chull(.spectra[inData])
  cHull <- cHull[which(cHull==1):length(cHull)]
  ## calculate linear approximation between hull points
  linearApprox <- approx(.data[cHull,], xout = rangeB, method = 'linear', ties = 'mean')
  
  deviation <-  ( linearApprox[[2]] -.spectra[inData] )/linearApprox[[2]]
  attr(deviation, 'hull') <-linearApprox[[2]]
  return(deviation)
}

cHullDeviationAbs <- function(.abs, wavelengths = 350:2500, rangeB = 350:2500){
  ## organize data
  inData <- wavelengths %in% rangeB
  .data <- sortedXyData(wavelengths[inData], .abs[inData])
  ## calculate convex rull
  cHull <- chull(.abs[inData])
  cHull <- cHull[1:(which(cHull==1)-1)]
  ## calculate linear approximation between hull points
  linearApprox <- approx(.data[cHull,], xout = rangeB, method = 'linear', ties = 'mean')
  
  deviation <-  (  .abs[inData]-linearApprox[[2]] )
  attr(deviation, 'hull') <-linearApprox[[2]]
  return(deviation)
}

 
drop0trailing.format <- function(x, ...) {
format(x,drop0trailing=T)
 }


readUSGS <- function(fName){
  .data <- read.table(file = fName, skip=17, na.strings = '-1.23e34', 
                      col.names = c('wavelength', 'reflectance','sd'))
  .data[,1] <- .data[,1] * 1000
  .data
}


readReference <- function(.fname, .detail){
  ## read in spectra
 usgsSpectra <- readUSGS(as.character(.fname))
 .mineral = names(.fname)
 ## smooth to 1-nm data
 splineSmooth <- spline(usgsSpectra[,1:2], xout = 350:2500)
 ## get the mineral band range
 rangeMineral <- .detail[[.mineral]]$wvRange
 ## calculate the convex hull deviations
 crSpectra <- cHullDeviation(splineSmooth$y, rangeB = rangeMineral)
 ## organize into a data frame
 crSpectraReference <- data.frame(wavelength = rangeMineral, value = crSpectra )
 ## rename 
 assign(paste('cr',.mineral,'Reference',sep=''),crSpectraReference)
 ## save
 save(list = c(paste('cr',.mineral,'Reference',sep='') ), file = paste('savedData/cr',.mineral,'Reference.RData',sep=''))
}



wideBandMin <-  function(.data, wideBand){which.min( 1 - .data[wideBand])}

maxID <- function(.spectra,.bandRange){data.frame(max = max(.spectra), which.max = .bandRange[which.max(.spectra)])}

relativeAbundance <- function(.spectra, .reference, .diagSpectra, .diagReference, bandWidth){
  referenceValue <- .reference[bandWidth == .diagReference]
  sampleValue <- .spectra[bandWidth == .diagSpectra]
  sampleValue / referenceValue
}


crMinerals <- function(.list, .spectraDetail= list(.fname = 'savedData/reflectAll.RData', .name ='reflectAll')){
      ## load reflectance
    spectra <- loadRename(.fname = .spectraDetail$.fname, .name = .spectraDetail$.name)
    ## calculate chull deviations
    .mineral = .list$.mineral
    crRange <- apply(spectra, 1, cHullDeviation, rangeB = .list$wvRange)
   
    ## calculate some quantiles
    crQ <-melt(data.frame( t( apply( crRange, 1, quantile,
                          prob = c(0.05,0.06, 0.5,0.84, 0.95))), 
                       wavelength = .list$wvRange), id = 'wavelength')
    ## rename the variable 
    levels(crQ$variable) <- c(0.05,0.06, 0.5,0.84, 0.95)
     ## rename
    assign(paste('crRange', .mineral, sep=''), crRange)
    assign(paste('cr', .mineral,'Q', sep=''), crQ)
      ## save data
      save(list = c(paste('crRange', .mineral, sep='')), file = paste('savedData/crRange', .list$.mineral,'.RData', sep=''))
      save(list = c(paste('cr', .mineral,'Q', sep='')), file = paste('savedData/cr', .list$.mineral,'Q.RData', sep=''))      
}


abundanceFunction <- function(.mineral){
 ## get reference spectra
 
  referenceCR <- loadRename(.fname = paste('savedData/cr',.mineral,'Reference.RData',sep=''),.name = paste('cr',.mineral,'Reference',sep=''))
## which are the wide band
  whichWide <- referenceCR$wavelength %in% wideBandList[[.mineral]]
  ## for reference spectra
 referenceID  <- which.min(1-referenceCR[whichWide,2])
  ## what band is that
 diagnosticBand  <- referenceCR$wavelength[whichWide][referenceID]
 ## return
 return(llist(whichWide, referenceID, diagnosticBand))
}
  
idMinima <- function(.List){
  
 ## load data  
 .mineral <- .list$.mineral
  crRange <- loadRename(.fname = paste('savedData/crRange',.mineral,'.RData',sep=''), .name = paste('crRange', .mineral, sep=''))
 ## and band range
 wideBand <- abundanceDiagnostics[[.mineral]]$whichWide
 mineralBands  <- apply( crRange, 2, wideBandMin, wideBand = wideBand)
 idBandMinima  <-  wideBandList[[.mineral]][mineralBands]
 ## cacluate derivative  
 idDeriv <- apply(apply(crRange,2,sgolayfilt,p=2,n=11,m=1)[wideBand,],2, function(x){ which.max(cumsum(x >0))})
 idBandDeriv <- wideBandList[[.mineral]][idDeriv]
 ## organize
 bandID <- data.frame(minima = idBandMinima,derivative = idBandDeriv)
 ## rename
 assign(paste('bandID',.mineral,sep =''), bandID)
 ## save
 save(list=c(paste('bandID',.mineral,sep ='')), file = file.path('savedData',paste('bandID',.mineral,'.RData',sep ='')))
}

feAbundance <- function(.list){
  .mineral <- .list$.mineral  
  crRange <- loadRename(paste('savedData/crRange',.mineral,'.RData',sep=''),paste('crRange',.mineral,sep=''))
  refVal <- max(get(paste('cr',.mineral,'Reference',sep=''))[,2])
  abundance <- apply(crRange,2,max) / refVal
  assign(paste('abundance',.mineral,sep=''), abundance)
  save(list = c(paste('abundance',.mineral,sep='')), file = paste('savedData/abundance',.mineral,'.RData',sep=''))
}

mineralAbundance <- function(.list){
 ## load data
  .mineral <- .list$.mineral
  crRange <- loadRename( paste('savedData/crRange',.mineral,'.RData',sep=''),paste('crRange',.mineral,sep=''))
  crRef <- loadRename( paste('savedData/cr',.mineral,'Reference.RData',sep=''),paste('cr',.mineral,'Reference',sep=''))
  diagRef <- abundanceDiagnostics[[.mineral]]$diagnosticBand
  diagSpec <-usedBands[.mineral]
  bandRange <- .list$wvRange
  ## calculate the relative Abundance
  relAbundance  <- apply(crRange, 2, relativeAbundance,
    .reference = crRef[,2], .diagSpectra = diagSpec, .diagReference= diagRef, bandWidth = bandRange)
  ## rename
  assign(paste('abundanceRelative',.mineral,sep =''), relAbundance)
  ## save
  save(list = c(paste('abundanceRelative',.mineral,sep ='')), file = paste('savedData/abundanceRelative',.mineral,'.RData',sep =''))
  }

calcNIODI <- function(minerals = c('Goethite','Hematite')){
  load(paste('savedData/crRange', minerals[1],'.RData', sep=''))
  load(paste('savedData/crRange', minerals[2],'.RData', sep=''))
  dHematite <- adply( crRangeHematite, 2,maxID, .bandRange = rangeHematite)
  dGoethite <- adply( crRangeGoethite, 2,maxID, .bandRange = rangeGoethite)
 ## calculate 
 NIODI <- n_I(dGoethite$max, dHematite$max)
 ## save
  save(NIODI, file = 'savedData/NIODI.RData')
return(NIODI)
  }

n_I <- function(A,B){
  ab <- cbind(A,B)
  ret <- (A-B)/ (A+B)
  ##look for NA
  both0 <- apply(ab,1, function(x){(x[1] == 0)&(x[2]==0)})
  ret[both0] <- rep(0, table(both0)[2])
  ret}

sampleBy <- function(.id, .var, .inData,.rate,id.name){
  ## where are the NAs?
  .naVar <- is.na(.inData[[.var]])
  ## unique ids (nas removed)
  uniqueID <- as.character( unique ((.inData[!.naVar,])[,.id]))
  allID <- as.character(unique(.inData[[.id]]))
  ## sampled IDs
  sampledIDs <- sample(uniqueID, size = floor(.rate * length(uniqueID)))
  ## return list with sampled IDs, validation IDs and no data IDs
  validationIDs <- uniqueID[!(uniqueID %in% sampledIDs )]                      
  noDataIDs <- allID[!(allID %in% uniqueID)]
  trainingRows <-  whichDataOut[.inData[,.id] %in% sampledIDs]
  validationRows <- whichDataOut[.inData[,.id] %in% validationIDs]
  ## remove NAs
  trainingRows <- trainingRows[!is.na(.inData[trainingRows,.var])]
  validationRows <- validationRows[!is.na(.inData[validationRows,.var])]
  noDataRows <- whichDataOut[-c(trainingRows,validationRows)]
  outPut <- llist(sampledIDs,validationIDs, noDataIDs,trainingRows,validationRows,noDataRows)                     
  saveObjectAs(object = outPut, .name = paste(.var,id.name,'TrainingInfo',sep=''),.fname = paste('savedData/',.var,id.name,'TrainingInfo.RData',sep='') )
  return(outPut)
  }

tileFunction <- function(.lev,max  = 2048 ,offset = 404){
  wb <- offset + seq(max / 2^(.lev+1),max,by = max/2^(.lev))
  geom_tile(data = data.frame(wb,scale=.lev), colour='black',fill=NA)
}

bandCentre <- function(.lev,max  = 2048 ,offset = 404){
  offset + seq(max / 2^(.lev+1),max,by = max/2^(.lev))
}


bandCentreID <-  function(.lev,max  = 2048 ,offset = 404){
  .level <- bandCentre(.lev,max=max,offset=offset)
  paste('w',.level,.lev,sep='.')
  
}

waveCoefs <- function(wdObject,.lev,max  = 2048 ,offset = 404){
  coefs <- accessD(wdObject,.lev)
  names(coefs) <- bandCentreID(.lev, max = max, offset = offset)
  coefs
}

mvOutlierRef <- function(.var,.sc = 5, .spectra, .subset = whichDataOut, .inData, .whichBands = keep10, .qcrit = 0.975){
  ## load spectra
  spectra <- loadRename(.fname = paste('savedData/',.spectra,'AllSGD1.RData',sep = ''), .name = paste(.spectra,'AllSGD1',sep = ''))
  ## organize data
  ## remove nas
  whichUse <- .subset[!is.na(.inData[.subset,.var])]
  plsData <- data.frame(.var = .inData[whichUse,.var], spectra = I(as.matrix(spectra[whichUse,.whichBands])))
  ## run model
  plsModel <- plsr(.var~spectra, data = plsData, ncomp = .sc)
  ## get scores
  plsScores <- scores(plsModel)
  ## use Mahalanobis distance on scores
  outlierTest <- sign1(plsScores, qcrit = .qcrit)
  ## which samples are outliers
  ## identify non-na 
  toUse <- whichUse[as.logical(outlierTest$wfinal01)]
  potentialOutlier <- whichUse[!as.logical(outlierTest$wfinal01)] 
  
  mvOut <- llist(toUse,potentialOutlier)
  ## save
  varname <- paste(.var,.spectra,'MVOutliers',sep='')
  saveObjectAs(mvOut, .fname = file.path('savedData',paste(varname,'.RData',sep='')), .name = varname)
  return(data.frame(nout = length(potentialOutlier), total = length(whichUse)))
  }


outlierMV <- function( .Data,.sc = 5, .subRows,.formula, .qcrit = 0.975, .dataCol){
  ## load spectra
  whichUse <- .subRows[!is.na(.Data[.subRows,.dataCol])]
  plsData <- .Data[whichUse,]
  ## run model
  plsModel <- plsr(.formula, data = plsData, ncomp = .sc)
  ## get scores
  plsScores <- scores(plsModel)
  ## use Mahalanobis distance on scores
  outlierTest <- sign1(plsScores, qcrit = .qcrit)
  ## which samples are outliers
  ## identify non-na 
  toUse <- whichUse[as.logical(outlierTest$wfinal01)]
  potentialOutlier <- whichUse[!as.logical(outlierTest$wfinal01)] 
  
  mvOut <- llist(toUse,potentialOutlier)
  mvOut
  }


orderedSample <- function(.y,.mvOut){
   
   sorted <- .mvOut$toUse[order(.y[.mvOut$toUse])]
   allRows <- seq(along=sorted)
   vRows <- seq(3,length(allRows),by=3)
   trainingRows <- sorted[allRows[-vRows]]
   validationRows <- sorted[vRows]
   outPut <- llist(trainingRows,validationRows)
   outPut
   }

makeTableDF <- function(.sampling){
  .data <- melt(subset(samplingResults, sampling ==.sampling), measure.var = 'MSE', id.var = c('variable','using','dataset'))
  rmse <- ddply(.data, ~dataset:using,function(x){sqrt(x$value)})
  .df <- data.frame(t(rmse[,2:ncol(rmse)]))
  names(.df) <- rmse[,'dataset:using']
  .df <- .df[,paste( c('training','validation'),rep(c('reflect','abs'),each = 2),sep=':')]
  row.names(.df) <-  as.character(sapply(whichProperties,propertiesLatex))
  names(.df) <- paste(rep(c('Refl','Abs'),each = 2), c('(training)','(validation)'))
## latexify
xTabRMSE <- xtable(.df, digits=3)
## and print using booktabs  
print(xTabRMSE,sanitize.text.function = function(x){x},floating=FALSE,hline.after=NULL,
        add.to.row=list(pos=list(-1,0, nrow(xTabRMSE)),command=c('\\toprule ','\\midrule ','\\bottomrule ')))
  }


orderedSampling <- function(.var,.spectra,.inData){
   varname <- paste(.var,.spectra,'MVOutliers',sep='')
   info <- loadRename(.fname = file.path('savedData',paste(varname,'.RData',sep='')), .name = varname)
   sorted <- info$toUse[order(.inData[info$toUse,.var])]
   allRows <- seq(along=sorted)
   vRows <- seq(3,length(allRows),by=3)
   trainingRows <- sorted[allRows[-vRows]]
   validationRows <- sorted[vRows]
    outPut <- llist(trainingRows,validationRows)
  saveObjectAs(object = outPut, .name = paste(.var,'MVOTrainingInfo',sep=''),.fname = paste('savedData/',.var,'MVOTrainingInfo.RData',sep='') )
}

outlierSample <- function(.var){
  load('savedData/mvOutliers.RData')
  inSamples <- whichDataOut[!is.na(soilDataOut[,.var])]
  outlier <- mvOutliers[[.var]]$outlier
  inSamples2 <- inSamples[!outlier]
  orderVar <- order(soilDataOut[inSamples2,.var])
  sortVar <-  sort(soilDataOut[inSamples2,.var])
  trainingRows <- inSamples2[orderVar][c(seq(1,length(orderVar),by=3), seq(2, length(orderVar),by=3))]
  validationRows <- inSamples2[orderVar][seq(3,length(orderVar),by=3)]
  outPut <- llist(trainingRows,validationRows)
  saveObjectAs(object = outPut, .name = paste(.var,'MVOTrainingInfo',sep=''),.fname = paste('savedData/',.var,'MVOTrainingInfo.RData',sep='') )
}

cubistSamplingFit <- function(.name,.type,.spectra){
  ## load sampling info
  trainingInfo <- loadRename(.fname = paste('savedData/',.name,.type,'TrainingInfo.RData',sep=''), .name = paste(.name,.type,'TrainingInfo',sep=''))
  trainingData <- soilData[trainingInfo$trainingRows,.name]
  spectra <- loadRename(paste('savedData/',.spectra,'10SGD1.RData',sep=''),paste(.spectra,'10SGD1',sep=''))
  cubistX <- as.data.frame(spectra[trainingInfo$trainingRows,keep10])
  names(cubistX) <- paste('w',seq(400,2450,by=10),sep='')
  cubistModel <- cubist(x = cubistX, y = trainingData)
  ## save the results
  .name <-  paste(.name,.spectra,.type,'Cubist',sep ='')
  saveObjectAs(cubistModel,.fname = file.path('savedData','models',.name), .name=.name)
}


## and summarize
cubistSummaryHelperSampling <- function(.name,.type, .spectra){
  mname <- paste(.name,.spectra,.type,'Cubist',sep ='')
  tname <- paste(.name,.type,'TrainingInfo',sep='')
  ## load model
  cubistModel <- loadRename(file.path('savedData','models', paste(mname,sep ='')), mname)
  ## load training info
  trainingInfo <-  loadRename(.fname = paste('savedData/',.name,.type,'TrainingInfo.RData',sep=''), .name = tname)
  ## load spectra
  argList <- list( x = paste(tolower(.spectra),'10SGD1',sep=''), .type = .spectra, .fname =paste('savedData/', tolower(.spectra),'10SGD1.RData',sep='') )
  spectra <- loadRename(argList$.fname,argList$x )
   ## organize data
   allData <- data.frame(spectra[,keep10])
  names(allData) <-      paste('w',seq(400,2450,by=10),sep='')
  ## make predictions -- training
  fittedCubist <- predict(cubistModel, newdata = allData[trainingInfo$trainingRows,])
  predictedCubist <- predict(cubistModel, newdata = allData[trainingInfo$validationRows,]) 
  fittedMAE <- mean(abs(soilData[trainingInfo$trainingRows,.name] - fittedCubist),na.rm = T)
  validMAE <- mean(abs(soilData[trainingInfo$validationRows,.name] - predictedCubist),na.rm = T)
  fittedMSE <- mean((soilData[trainingInfo$trainingRows,.name] - fittedCubist)^2,na.rm=T)
  validMSE <- mean((soilData[trainingInfo$validationRows,.name] - predictedCubist)^2, na.rm =T)
  
  data.frame(MAE = c(fittedMAE,validMAE), 
             MSE  = c(fittedMSE,validMSE),
             dataset = as.factor(c('training','validation')),
             sampling = .type, using = .spectra,variable = .name)
}


cubistSamplingSummary <- function(termList){
  .name <- termList$.name
  .type <- termList$.type
  .spectra <- termList$.spectra
  df <- cubistSummaryHelperSampling(.name,.type,.spectra)

}



power2 <- function(.char){paste(.char,'+I(',.char,'^2)')}
makeFormula <- function(.names,.y){
  as.formula(paste(.y, '~',paste(sapply(.names,power2),collapse='+')))
}

waveletModel <- function(.var,.coefs,.which,.number, .inData){
  ## load training Info
  info <- loadRename(.name = paste( .var, 'MVOTrainingInfo', sep = ''), .fname = paste('savedData/', .var, 'MVOTrainingInfo.RData', sep = ''))
  ## create data frames
  trainingData <- data.frame(.var = .inData[info$trainingRows,.var],.coefs[info$trainingRows,])
  validationData <- data.frame(.var = .inData[info$validationRows,.var],.coefs[info$validationRows,])
  ## make formula
  waveFormula <- makeFormula(.names = names(.coefs)[varOrder[1:.number]], .y = '.var')
  ## fit model
  waveModel <- lm(waveFormula,trainingData )
  ## calculate adjusted R squared
  adjR2 <- summary(waveModel)$adj.r.squared
  
  ## predict
  validationPredict <- predict(waveModel, newdata = validationData)
  ## and calculate RMSE
  rmse <- sqrt( mean( (validationData$.var - validationPredict )^2,na.rm=T) )
  
  data.frame(adjR2, rmse)
  
}


