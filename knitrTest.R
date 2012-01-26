
## @knitr setup
options(replace.assign=TRUE,width=90)
knit_hooks$set(fig=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})
setwd('c:/Research/Spectra/calibrationReport')
owd <- getwd()


## @knitr setup-libraries
## Begin by loading the required libraries into R.
library(signal) ; library(pls);  library(ggplot2); library(tikzDevice); library(treemap); library(gridExtra); library(munsell); library(Hmisc); library(Cubist); library(xtable); library(wavethresh); library(mvoutlier)
## load some required functions
##
source('racaFunctions.R')


## @knitr set-seed
## Set the random seed. This allows any `random' components to be repeated with the same results. 
set.seed(271011)
memory.limit(size=4095)


## @knitr load-data
## Load in the soil property data and perform a bit of house-keeping.
## some file locations and names
baseDir <- 'c:/Research/Spectra'
spectraDir <- 'asciiSpectra'
figuresDir <- file.path(baseDir,'Report','figures')
propertiesFile <- 'new_soil_properties.csv'
## read in the data
soilData <- read.csv(file.path(baseDir,propertiesFile), header=T)
## remove 'dB columns
soilData <- soilData[,-c(17:21)]
## rename to a standard convention
newNames <- c('labProjectName', 'userPedonId', 'labHorizonId', 'userHorizonId',
              'horizonDesignation', 'horizonTop', 'horizonBottom', 'vnirName',
              'totalC', 'CaCO3', 'estOrgC', 'clay', 'sand', 'cecClayRatio', 
              'taxonomy', 'labTexture', 'pHWater', 'pHCaCl', 'cecNH4', 'feCD')
names(soilData) <- newNames
## save the data
save(soilData,file='savedData/soilData.RData')


## @knitr some-summaries
## how many samples with spectra recorded
nSpectra <- nrow(soilData)
## how many samples recorded for each variable
nEach <-  tabDF( apply( soilData, 2, lengthNA))
## how many separate profiles  
nProfiles <- length( levels( soilData$userPedonId))
## and for each variable
nEach$nProfiles <- nUniqueProfiles(.data = soilData, .var = names(soilData))

## summarize by taxonomy
summaryTax <- tabDF( table( na.omit( ddply( soilData, 'userPedonId', 
  function(x){tax <- unique( x$taxonomy); data.frame( taxonomy = tax)}
  ))$taxonomy)[-1], ind = 'taxonomy')
## create a tree plot of the taxonomy  
treeTax <- tmPlot(summaryTax,'taxonomy',vSize = 'count',sortID = '-size',saveTm=T)[[1]][[1]] 

treeTax$count <- summaryTax$count
## how many separate profiles with taxonomy recorded
nProfileTaxonomy <- sum(summaryTax$count)
## organize some of this information
numbers <- llist(nSpectra, nProfiles, nProfileTaxonomy, nEach,treeTax)
## save this information
save(summaryTax, file = 'savedData/summaryTax.RData')
save(numbers, file = 'savedData/numbers.RData')



## @knitr raca-functions-cache
source('racaFunctions.R')


## @knitr treeplot-taxonomy
## create the ggplot object for the tree plot of the taxonomy information
nameCount <- ddply(numbers$treeTax,1,function(x){paste(x$ind,x$count,sep= '  \n ')})$V1
treeMapTaxonomy <- ggplot(numbers$treeTax, aes(xmin = x0,ymin = y0, ymax = y0 + h, xmax = x0 + w)) + 
  geom_rect(aes(fill = ind,col = ind )) + 
  geom_text(aes(x=x0+ w/2,y=y0+h/2,label=nameCount, size=count)) + 
  opts(axis.text.x = theme_blank(), axis.text.y = theme_blank(), 
    axis.ticks = theme_blank(), axis.title.y = theme_blank(), 
    axis.title.x = theme_blank(), panel.background = theme_blank(), 
    legend.position = 'none') +
  scale_fill_manual(values = rev(brewer.pal(n=12,name='Set3')))+ 
  scale_colour_manual(values = rev(brewer.pal(n=12,name='Set3')))+
  scale_size(to = c(2,7))+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


## @knitr treeMapPlot
treeMapTaxonomy


## @knitr create-histograms
clayHistogram <- ggplot(soilData,aes(x=clay)) + 
  geom_histogram(binwidth = 2,fill = 'black',colour='black') +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  xlab('Clay / \\%')

sandHistogram <- ggplot(soilData,aes(x=sand)) + 
  geom_histogram(binwidth = 2,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('Sand / \\%')

pHWaterHistogram <- ggplot(soilData,aes(x=pHWater)) + 
  geom_histogram(binwidth = 0.25,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('pH (${\\rm H_2O}$)')  
  
pHCaCl2Histogram <- ggplot(soilData,aes(x=pHCaCl)) + 
  geom_histogram(binwidth = 0.25,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('pH (${\\rm CaCl_2}$)')


cecHistogram <- ggplot(soilData,aes(x=cecNH4)) + 
  geom_histogram(binwidth = 2,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('CEC')

  
cecClayHistogram <- ggplot(soilData,aes(x=cecClayRatio)) + 
  geom_histogram(binwidth = 0.5,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('CEC / Clay')
  
feHistogram <- ggplot(soilData,aes(x=feCD)) + 
  geom_histogram(binwidth = 0.25,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('${\\rm Fe_2O_3}$ ')


caCO3Histogram <- ggplot(soilData,aes(x = CaCO3)) + 
  geom_histogram(binwidth = 2.5,fill = 'black',colour='black') +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0, 0)) +
  xlab('${\\rm CaCO_3}$ ')



## @knitr table-results
## format a table using booktabs in LaTeX

resultsTable <- nEach[c(9:14,17:20),2:3]
row.names(resultsTable) <- c('Total C','${\\rm CaCO_3}$', 'Organic C','Clay', 'Sand','CEC:Clay','pH (Water)', 'pH (${\\rm CaCl_2}$)', 'CEC','Iron Oxide')
names(resultsTable) <- c('Samples','Profiles')
xTab <- xtable(resultsTable, digits=0)
  print(xTab,sanitize.text.function = function(x){x},floating=FALSE,hline.after=NULL,
                  add.to.row=list(pos=list(-1,0, nrow(xTab)),command=c('\\toprule ','\\midrule ','\\bottomrule ')))


## @knitr histogramsPlot
theme_set(theme_bw())
grid.arrange(clayHistogram, sandHistogram, cecHistogram, cecClayHistogram, pHWaterHistogram, pHCaCl2Histogram, feHistogram, caCO3Histogram,ncol=2)


## @knitr readin-spectra
waveLengths <- 350:2500
## use 500 and 2451 as cut-offs  
  keepLengths <- (waveLengths >= 500) & (waveLengths <= 2451)
## a set that is dyadic
  keepDyadic <-  (waveLengths >= 404) & (waveLengths <= 2451)
## read in all reflectance spectra spectra
readInSpectra <- function(specList){
  fpath <- specList$fpath
  .type <- specList$.type
  ## read in spectra
  spectra <- readSpectra(soilData$vnirName, filePath = fpath) 
  ## rename and save
  saveObjectAs(object = spectra,.fname = paste('savedData/',.type,'All.RData',sep=''), .name = paste(.type,'All',sep='') )
  rm(list = ls())
  invisible(gc())
}
  
  
l_ply(list(list(fpath = 'c:/Research/Spectra/asciiSpectra', .type = 'reflect'),list(fpath = 'c:/Research/Spectra/asc1_R', .type = 'abs') ), readInSpectra)

## wave lengths 
  
## trim reflectance
trimSpectra <- function(trimList){
  ## house keeping
  .type = trimList$.type
  whichData <-trimList$whichData
  .name <- trimList$.name
  ## load spectra
  spectra <- loadRename(.fname = paste('savedData/',.type,'All.RData',sep=''), .name = paste(.type,'All',sep=''))
  ## trim
  spectraT <- spectra[,whichData]
  ## save 
  saveObjectAs(object = spectraT,.fname = paste('savedData/',.type,.name,'.RData',sep=''), .name = paste(.type,.name,sep='') )
  rm(list = ls())
  invisible(gc())
}

trimList <- expand.grid(.type = c('reflect','abs'),.name = c('Dyadic','Keep'))
trimList$whichData <- rep(c('keepLengths','keepDyadic'), each=2)
trimLists <- apply(trimList,1,function(x){xt <- as.list(x); xt$whichData <- get(xt$whichData);xt})
## trim the spectra
l_ply(trimLists,trimSpectra)
## garbage collection
rm(trimLists)
invisible(gc())


## @knitr savtivsky-golay
## fit s-g filter to relfectance
sgFilter <- function(){
  load(file='savedData/reflectAll.RData')
  load('savedData/absAll.RData')
  reflectAllSG   <- t(apply(reflectAll, 1, sgolayfilt, n = 11, p = 2, m = 0))
  reflectAllSGD1 <- t(apply(reflectAll, 1, sgolayfilt, n = 11, p = 2, m = 1))
  ## and to absorbance
  absAllSG   <- t(apply(absAll, 1, sgolayfilt, n = 11, p = 2, m = 0))
  absAllSGD1 <- t(apply(absAll, 1, sgolayfilt, n = 11, p = 2, m = 1))

  ## save
  save(reflectAllSG, file = 'savedData/reflectAllSG.RData')
  save(reflectAllSGD1, file = 'savedData/reflectAllSGD1.RData')
  save(absAllSG, file = 'savedData/absAllSG.RData')
  save(absAllSGD1, file = 'savedData/absAllSGD1.RData')
}

sgFilter()


## @knitr subset-wavelengths
## some indices for subsetting every tenth waveband
every10 <- seq(1, 2151, by = 10)
keep10  <- seq(350, 2500, by = 10) %in% seq(400, 2450, by = 10)  
subsetWavelengths <- function(){
  ## load
  load('savedData/reflectAll.RData')
  load('savedData/absAll.RData')
  reflect10 <- reflectAll[,every10]
  abs10 <- absAll[,every10]
  ## fit S-G fileter
  reflect10SG   <- t(apply(reflect10, 1, sgolayfilt, n = 3, p = 2, m = 0))
  reflect10SGD1 <- t(apply(reflect10, 1, sgolayfilt, n = 3, p = 2, m = 1))
  ## and to absorbance
  abs10SG   <- t(apply(abs10, 1, sgolayfilt, n = 3, p = 2, m = 0))
  abs10SGD1 <- t(apply(abs10, 1, sgolayfilt, n = 3, p = 2, m = 1))
  ## save 
  save(reflect10SG, file = 'savedData/reflect10SG.RData')
  save(reflect10SGD1, file = 'savedData/reflect10SGD1.RData')
  save(abs10SG, file = 'savedData/abs10SG.RData')
  save(abs10SGD1, file = 'savedData/abs10SGD1.RData')
  
  }

subsetWavelengths()



## @knitr soilColour
## calculate the RGB colours 

soilColourCalculate <- function(){
  ## load data
  load('savedData/reflectAll.RData')
  ## calculate the RGB colours
  rgbCols <- adply(reflectAll,1, .fun= 'spectraToRGB')
  ## convert to munsell colours
  rm(reflectAll)
  invisible(gc())
  mnslCols <-table( rgb2mnsl( R = rgbCols[,2], G = rgbCols[,3], B = rgbCols[,4] ) )
  munsellDF <- data.frame(ind = as.character(names( mnslCols )),count = as.numeric(mnslCols))
  ## save data
  
  save(rgbCols, file = 'savedData/rgbCols.RData')  
  save(mnslCols, file = 'savedData/mnslCols.RData')
  save(munsellDF, file = 'savedData/munsellDF.RData')
}

soilColourCalculate()
  
## load the data for the tree map plot of munsell colours
load( 'savedData/munsellDF.RData')
  ## create a tree map plot
treeMunsell <- tmPlot(munsellDF,'ind',vSize = 'count',sortID = '-size',saveTm=T)[[1]][[1]] 
## some tidying up
treeMunsell$count <- munsellDF$count
## convert to hex colour for plotting
treeMunsell$hexCol <- mnsl2hex(treeMunsell$ind)
## and create labels for plotting
treeMunsell$nameCountM <- ddply(treeMunsell,1,function(x){paste(x$ind,x$count,sep= '  \n ')})$V1
## create the treeMap
treeMapMunsell <- ggplot(treeMunsell, aes(xmin = x0,ymin = y0, ymax = y0 + h, xmax = x0 + w)) + 
  geom_rect(aes(fill = hexCol,colour=hexCol )) + 
  scale_fill_identity()+
  scale_colour_identity()+
  geom_text(aes(x=x0+ w/2,y=y0+h/2,label = nameCountM, size=count))+
  opts(axis.text.x = theme_blank(), axis.text.y = theme_blank(), axis.ticks = theme_blank(), 
    axis.title.y = theme_blank(), axis.title.x = theme_blank(), panel.background = theme_blank(), 
   legend.position = 'none') +
  scale_size(to = c(1,7))+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


## @knitr figure-munsellcolour
treeMapMunsell


## @knitr PLSR-models
## the properties in question  
whichProperties <-  names(soilData)[c(9:14,17:20)]
##
source('racaFunctions.R')
## organize training info
  l_ply(whichProperties,'makeTrainingInfo')#'spectraPLS'
## fit model to reflectance
  
  l_ply(whichProperties,'plsFit', argList = list( x = 'reflect10SGD1', .type ='Reflect', .fname = 'savedData/reflect10SGD1.RData'))
  ## to absorbance
  l_ply(whichProperties,'plsFit', argList = list( x = 'abs10SGD1', .type ='Abs', .fname = 'savedData/abs10SGD1.RData'))
 
  



## @knitr summarize-plsr
source('racaFunctions.R')
plsrTraining <- ldply(whichProperties, plsrSummary)
save(plsrTraining, file = 'savedData/plsrTraining.RData')
rm(plsrTraining)
invisible(gc())


## @knitr figure-rmsePlot
options(opts(warn = -1))
load('savedData/plsrTraining.RData')
rmsePlot <- ggplot(plsrTraining, aes(x = components, y = rmsep, group = using:type)) + 
    facet_wrap(~latexId,ncol=2,scales ='free_y') +
    geom_line(aes(colour= using:type)) +
    opts(strip.background = theme_blank()) +
    scale_colour_manual(values = c("red","blue",'green','orange')) +
    ylab('RMSE')

theme_set(theme_bw())
rmsePlot
## clean up
rm(list = c('plsrTraining','rmsePlot'))
invisible(gc())


## @knitr figure-r2Plot
options(opts(warn = -1))
load('savedData/plsrTraining.RData')
theme_set(theme_bw())
r2Plot <- ggplot(plsrTraining, aes(x = components, y = r2, group = using:type)) + 
    facet_wrap(~latexId,ncol=2,scales ='free_y') +
    geom_line(aes(colour= using:type)) +
    opts(strip.background = theme_blank())+
    scale_colour_manual(values = c("red","blue",'green','orange')) +
    ylab('$R^2$')+ylim(0,1)
r2Plot
   
## clean up
rm(list = c('plsrTraining','r2Plot'))
invisible(gc())


## @knitr cubist-models
## make sure the functions are here
source('racaFunctions.R')
## fit models using cubist -- Reflectance
l_ply(whichProperties,cubistFit, argList = list( x = 'reflect10SGD1', .type ='Reflect', .fname = 'savedData/reflect10SGD1.RData'))
## fit models using cubist -- Absorbance
l_ply(whichProperties,cubistFit, argList = list( x = 'abs10SGD1', .type ='Abs', .fname = 'savedData/abs10SGD1.RData'))



## @knitr cubist-summary
## calculate the MSE correctly this 
cubistMSE <- ldply(whichProperties, cubistSummary)
## convert to RMSE
## first 4 columns are what is required
rmseTable <- sqrt(cubistMSE[,1:4])
## pretty-up the table
## row names
##
row.names(rmseTable) <- c('Total C','${\\rm CaCO_3}$', 'Organic C','Clay', 'Sand','CEC:Clay','pH (Water)', 'pH (${\\rm CaCl_2}$)', 'CEC','Iron Oxide')
## column names
names(rmseTable) <- paste(rep(c('Refl','Abs'),each = 2), c('(training)','(validation)'))
## latexify
xTabRMSE <- xtable(rmseTable, digits=3)
## and print using booktabs  
print(xTabRMSE,sanitize.text.function = function(x){x},floating=FALSE,hline.after=NULL,
        add.to.row=list(pos=list(-1,0, nrow(xTabRMSE)),command=c('\\toprule ','\\midrule ','\\bottomrule ')))


## @knitr continuum-removal
## splits for each mineral range
## names of minerals
clayList <- llist('Kaolinite','Smectite','Illite')
invisible(gc())
## clays
rangeSmectite <- 1830:2130
inSmectite <- 350:2500 %in% rangeSmectite
rangeKaolinite <- 2131:2260
inKolinite <- 350:2500 %in% rangeKaolinite
rangeIllite <- 2261:2410
inIllite <- 350:2500 %in% rangeIllite

## calculate the continuum-removed reflectance for each mineral
l_ply(clayList,crMinerals)


## @knitr reference-spectra
## reference spectra
## according to clark et al / Viscarra Rossel
spectraBands <- list(Kaolinite = 2165, Smectite = 1912 , Illite = 2345 )
## filenames of reference spectra
referenceSpectraFiles <- list(Kaolinite = 'kaolinite_cm9.11962.asc', Smectite ='montmorillonite_sca2.14557.asc',Illite = 'illite_imt1.10982.asc' )
## read in spectra and save
l_ply(clayList, readReference)


## @knitr figure-minerals
## load the data
 for(.mineral in c('Smectite','Illite','Kaolinite')){
      load(file = paste('savedData/cr', .mineral,'Q.RData', sep='')) 
      load(file = paste('savedData/cr',.mineral,'Reference.RData',sep=''))
    }
## create the individual plots
smectitePlot <- ggplot(crSmectiteQ, aes( x = wavelength, y =1- value )) + 
  geom_line( aes( colour = variable ),size = 1, alpha= 1 ) +
  geom_line(data = crSmectiteReference, colour = 'red', size = 1) +
  geom_vline(xintercept = 1912) + 
  geom_vline(xintercept = (1912 + c(-15, 15)), linetype = 2) +
  xlab('') + 
  ylab('') + 
  scale_x_continuous(breaks = seq(1900, 2050, by=150)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  scale_colour_brewer('Soil spectra \n (Quantiles)', palette = 'Greys') +
  opts(legend.title = theme_text(  size = 12 * 0.8, hjust = 0, face = 'plain'))
##
kaolinitePlot <- ggplot(crKaoliniteQ, aes( x = wavelength, y =1- value )) + 
  geom_line( aes( colour = variable ),size = 1, alpha= 1 ) +
  geom_line(data = crKaoliniteReference, colour = 'red', size = 1) + 
  geom_vline(xintercept = c(2165)) +
  geom_vline(xintercept = (2165 + c(-15, 15)), linetype = 2) +
  xlab('') + 
  ylab('') + 
  scale_x_continuous(breaks = seq(2150, 2250, by=100)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  scale_colour_brewer('Soil spectra \n (Quantiles)', palette = 'Greys') +
  opts(legend.title = theme_text(  size = 12 * 0.8, hjust = 0, face = 'plain'))
##
illitePlot <- ggplot(crIlliteQ, aes( x = wavelength, y =1- value )) + 
  geom_line( aes( colour = variable ),size = 1, alpha= 1 ) +
  geom_line(data = crIlliteReference, colour = 'red', size = 1) +
  geom_vline(xintercept = c(2345)) +
  geom_vline(xintercept = (2345 + c(-15, 15)), linetype = 2) +
  xlab('') + 
  ylab('') + 
  scale_x_continuous(breaks = seq(2300, 2450, by=100))+
  scale_y_continuous( formatter = 'drop0trailing.format') +
  scale_colour_brewer('Soil spectra \n (Quantiles)', palette = 'Greys') +
  opts(legend.title = theme_text(  size = 12 * 0.8, hjust = 0, face = 'plain'))
## create the legend for the combined plot
mineralLegend <- ggplotGrob(kaolinitePlot + opts(keep ='legend_box')) 
## one needs to provide the legend with a well-defined width
legend <- gTree(children = gList(mineralLegend), cl = 'legendGrob')
widthDetails.legendGrob <- function(x) unit(3, "cm")
## arrange the various plots, legend and axis titles
grid.arrange(smectitePlot + opts(legend.position = 'none', 
                                 panel.grid.major = theme_blank(), 
                                 panel.grid.minor = theme_blank()),
            kaolinitePlot + opts(legend.position = 'none', 
                                 panel.grid.major = theme_blank(), 
                                 panel.grid.minor = theme_blank()),
            illitePlot  + opts(legend.position = 'none', 
                                 panel.grid.major = theme_blank(), 
                                 panel.grid.minor = theme_blank()),
            legend = legend, nrow = 1,
            left = 'Continuum-removed reflectance',
             sub = 'Wavelength / nm')
## add the a, b , c labels
grid.text(x = 0.02, y = 0.97, label = '(a)')
grid.text(x = 0.32, y = 0.97, label = '(b)')
grid.text(x = 0.62, y = 0.97, label = '(c)')
 for(.mineral in c('Smectite','Illite','Kaolinite')){
     rm(list = paste('savedData/cr', .mineral,'Q.RData', sep='')) 
     rm(list = paste('savedData/cr',.mineral,'Reference.RData',sep=''))
    }



## @knitr mineral-abundance
## identify the wavelength bands
source('racaFunctions.R')
wideBandList <- list(Illite = 2330:2360, Smectite = 1897:1927, Kaolinite = 2151:2180)
## absorbance in reference spectra
abundanceDiagnostics <- llply(clayList, .fun = abundanceFunction)
## find minima in band regions
l_ply(clayList, .fun = idMinima)



usedBands <- list(Kaolinite = 2165 , Illite = 2350 , Smectite = 1911)



## @knitr figure-diagnosticBands
## load data
for(.mineral in c('Kaolinite','Smectite','Illite')){
  load( file.path('savedData',paste('bandID',.mineral,'.RData',sep ='')))
  
  }

## create the individual plots
smectiteDiagBandPlot  <- ggplot(bandIDSmectite) + 
  geom_density( aes(x = minima)) +
  geom_density( aes(x = derivative) , col='grey') +
  xlab('') + 
  ylab('') +
  geom_vline(aes(xintercept = abundanceDiagnostics$Smectite$diagnosticBand), col='red') +
  geom_vline(aes(xintercept = 1912), col='blue') +
  scale_x_continuous(breaks = seq(1900,1925,by=10)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())
  
kaoliniteDiagBandPlot  <- ggplot(bandIDKaolinite) + 
  geom_density( aes(x = minima)) +
  geom_density( aes(x = derivative) , col='grey') +
  xlab('') + 
  ylab('') +
  geom_vline(aes(xintercept = abundanceDiagnostics$Kaolinite$diagnosticBand), col='red') +
  geom_vline(aes(xintercept = 2165), col='blue') +
  scale_x_continuous(breaks = seq(2150,2180,by=15)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())
##
illiteDiagBandPlot <- ggplot(bandIDIllite) + 
  geom_density( aes(x = minima), col='black') +
  geom_density( aes(x = derivative), col='grey') +
  xlab('') + 
  ylab('') +
  geom_vline(aes(xintercept = abundanceDiagnostics$Iillite$diagnosticBand), col='red') +
  geom_vline(aes(xintercept = 2345), col='blue') +
  scale_x_continuous(breaks = seq(2330,2360,by=15)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())

## arrange the various plots, legend and axis titles
grid.arrange(smectiteDiagBandPlot + opts(legend.position = 'none'),
            kaoliniteDiagBandPlot + opts(legend.position = 'none')+ ylim(c(0,0.3)),
            illiteDiagBandPlot  + opts(legend.position = 'none') ,
            left = 'Density',
             sub = 'Wavelength / nm',
             nrow = 1)
## add the a, b , c labels
grid.text(x = 0.02, y = 0.97, label = '(a)')
grid.text(x = 0.37, y = 0.97, label = '(b)')
grid.text(x = 0.69, y = 0.97, label = '(c)')


## @knitr mineral-abundance-calcuations
## caculate the relative abundances
## saved as paste('savedData/abundanceRelative',.mineral,'.RData',sep='')
source('racaFunctions.R')
l_ply(clayList, .fun = mineralAbundance)


## @knitr feOxide-calculations
## bandwidths
## fe Oxide
feList <- llist('Hematite','Goethite')
rangeHematite <- 860:930
inHematite <- 350:2500 %in% rangeHematite
rangeGoethite <- 910:970
inGoethite <- 350:2500 %in% rangeGoethite
## commented out code refers to Viscarra Rossel 2011 procedure, first calculating
## the local minima on the refletance
## hematiteIds <- apply(reflectAll[,350:2500 %in% hematiteBands], 1, which.min)
## crRangeHematite <- apply(reflectAll, 1, cHullDeviation, rangeB = hematiteBands)
## dHematite <- sapply(1:3171, function(x){crRangeHematite[hematiteIds[x],x]})
## goethiteIds <- apply(reflectAll[,350:2500 %in% goethiteBands], 1, which.min)
## crRangeGoethite <- apply(reflectAll, 1, cHullDeviation, rangeB = goethiteBands)
## dGoethite <- sapply(1:3171, function(x){crRangeGoethite[goethiteIds[x],x]})

## caculate the cHull deviations
l_ply(feList,crMinerals)
## calculate NIODI
NIODI <- calcNIODI()


## @knitr fe-oxide-reference
## read in hematite reference
referenceSpectraFiles$Hematite <- 'hematite_fe2602.9271.asc'
referenceSpectraFiles$Goethite <- 'goethite_ws220.8392.asc'
## read in spectra and save
l_ply(feList, readReference)
## calculate abundance diagnostics
wideBandList$Hematite <- rangeHematite
wideBandList$Goethite <- rangeGoethite

abundanceDiagnosticsFe <- llply(feList, .fun = abundanceFunction)



## @knitr figure-feOxide
## create the individual plots
 for(.mineral in c('Hematite','Goethite')){
      load(file = paste('savedData/cr', .mineral,'Q.RData', sep='')) 
      load(file = paste('savedData/cr',.mineral,'Reference.RData',sep=''))
    }

hematitePlot <- ggplot(crHematiteQ, aes( x = wavelength, y =1- value )) + 
  geom_line( aes( colour = variable ),size = 1, alpha= 1 ) +
  geom_line(data = crHematiteReference, colour = 'red', size = 1) +
  xlab('') + 
  ylab('') + 
  scale_x_continuous(breaks = seq(860,930, by=20)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  scale_colour_brewer('Soil spectra \n (Quantiles)', palette = 'Greys') +
  opts(legend.title = theme_text(  size = 12 * 0.8, hjust = 0, face = 'plain'),
       panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())
##
goethitePlot <- ggplot(crGoethiteQ, aes( x = wavelength, y =1- value )) + 
  geom_line( aes( colour = variable ),size = 1, alpha= 1 ) +
  geom_line(data = crGoethiteReference, colour = 'red', size = 1) +
  xlab('') + 
  ylab('') + 
  scale_x_continuous(breaks = seq(810,970, by=20)) +
  scale_y_continuous( formatter = 'drop0trailing.format') +
  scale_colour_brewer('Soil spectra \n (Quantiles)', palette = 'Greys') +
  opts(legend.title = theme_text(  size = 12 * 0.8, hjust = 0, face = 'plain'),
       panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())
## create the legend for the combined plot
feLegend <- ggplotGrob(goethitePlot + opts(keep ='legend_box')) 
## one needs to provide the legend with a well-defined width
legendFe <- gTree(children = gList(feLegend), cl = 'legendGrob')
widthDetails.legendGrob <- function(x) unit(3, "cm")
## arrange the various plots, legend and axis titles
grid.arrange(hematitePlot + opts(legend.position = 'none'),
            goethitePlot + opts(legend.position = 'none'),
            legend = legendFe, 
            nrow = 1,
            left = 'Continuum-removed reflectance',
            sub = 'Wavelength / nm')
## add the a, b , c labels
grid.text(x = 0.02, y = 0.97, label = '(a)')
grid.text(x = 0.48, y = 0.97, label = '(b)')


## @knitr fe-abundance
## calculate hematite abundance relative to peak near 880
source('racaFunctions.R')
l_ply(feList, feAbundance)


## @knitr spectral-outlier
## create outlier data frame
createDF <- function(){
  load('savedData/reflectAll.RData')
  load('savedData/absAll.RData')

outDF <- data.frame(wavelength = 350:2500, 
            outlierRef = reflectAll[3144,], subsoilRef = reflectAll[3145,],
            outlierAbs = absAll[3144,], subsoilAbs = absAll[3145,])
save(outDF, file = 'savedData/outDF.RData')  
outDF
  }
outDF <- createDF()

## create plot
outPlot <- ggplot(outDF, aes(x = wavelength)) + opts(panel.grid.major = theme_blank(),
                  panel.grid.minor = theme_blank(), legend.position = 'none') +
            xlab('Wavelength / nm')+
            scale_y_continuous( formatter = 'drop0trailing.format') 
## and arrange two plots on one page
grid.arrange(outPlot + geom_line(aes(y = outlierRef), col='red') + 
              geom_line(aes(y = subsoilRef)) +  ylab('Reflectance'),
            outPlot + geom_line(aes(y = outlierAbs), col='red') + 
              geom_line(aes(y = subsoilAbs)) +  ylab('$ - {\\rm log} \\left(R\\right)$'),
             ncol = 2)
## clean up
rm(list = c('outDF','outPlot'))
invisible(gc())



## @knitr outlier-removal
## remove the outliers
soilDataOut <- soilData[-c(1981,3144),]
## set up a vector which removes the outliers
whichDataOut <- (1:3171)[-c(1981,3144)]

## @knitr outlier-detection
## calculate the outliers using mahalanobis distance
## this now uses sign1 function in mvoutlier package ()
source('racaFunctions.R')
## reflectance outliers
reflectOutliers <- ldply(whichProperties, .fun = mvOutlierRef, .sc = 5 , .spectra = 'reflect', .subset = whichDataOut, .inData =soilData, .whichBands = keep10, .qcrit = 0.975)
## absorbance outliers
absOutliers <- ldply(whichProperties, .fun = mvOutlierRef, .sc = 5 , .spectra = 'abs', .subset = whichDataOut, .inData =soilData, .whichBands = keep10, .qcrit = 0.975)


## @knitr table-results
## format a table using booktabs in LaTeX

outlierTable <- cbind(reflectOutliers$nout,absOutliers)
row.names(outlierTable) <- c('Total C','${\\rm CaCO_3}$', 'Organic C','Clay', 'Sand','CEC:Clay','pH (Water)', 'pH (${\\rm CaCl_2}$)', 'CEC','Iron Oxide')
names(outlierTable) <- c('Reflect','Abs','Total')
outXTable <- xtable(outlierTable, digits=3)
  print(outXTable,sanitize.text.function = function(x){x},floating=FALSE,hline.after=NULL,
                  add.to.row=list(pos=list(-1,0, nrow(xTab)),command=c('\\toprule ','\\midrule ','\\bottomrule ')))


## @knitr sampling-methods
##
source('racaFunctions.R')
## sampling by profiles (with outliers removed)
l_ply(whichProperties, .fun = sampleBy, .id = 'userPedonId', .inData =soilDataOut, .rate = 0.7,id.name = 'Profiles')
## sampling by projects
l_ply(whichProperties, .fun = sampleBy, .id = 'labProjectName', .inData =soilDataOut, .rate = 0.7,id.name = 'Projects')

## identify outlier sample IDs for each variable / sample every third sorted row
l_ply(whichProperties, .fun = orderedSampling, .spectra = 'reflect', .inData = soilData)
l_ply(whichProperties, .fun = orderedSampling, .spectra = 'abs', .inData = soilData)


## @knitr cubist-refit
source('racaFunctions.R')
## re fit cubist models with different sampling methods
## by Profile

## by profiles
l_ply(whichProperties,.fun = 'cubistSamplingFit', .type = 'Profiles', .spectra = 'reflect')
## by projects
l_ply(whichProperties,.fun = 'cubistSamplingFit', .type = 'Projects', .spectra = 'reflect')
## using MV outliers and sorted sampling
l_ply(whichProperties,.fun = 'cubistSamplingFit', .type = 'MVO', .spectra = 'reflect')

## and for absorbance
l_ply(whichProperties,.fun = 'cubistSamplingFit', .type = 'Profiles', .spectra = 'abs')
## by projects
l_ply(whichProperties,.fun = 'cubistSamplingFit', .type = 'Projects', .spectra = 'abs')
## using MV outliers and sorted sampling
l_ply(whichProperties,.fun = 'cubistSamplingFit', .type = 'MVO', .spectra = 'abs')


## @knitr summarize-refit
source('racaFunctions.R')
samplingTerms <- expand.grid(.type = c('MVO','Profiles','Projects'),.name = whichProperties, .spectra = c('reflect','abs'))

samplingResults <- ldply(apply(samplingTerms,1,as.list),.fun ='cubistSamplingSummary')


## @knitr table-profile-summary
source('racaFunctions.R')
## calculate the MSE correctly this 
makeTableDF('Profiles')


## @knitr table-project-summary
source('racaFunctions.R')
makeTableDF('Projects')


## @knitr table-MVO-summary
source('racaFunctions.R')
makeTableDF('MVO')


## @knitr wavelets-cr
waveletFunction <- function(){
  load('savedData/reflectAllSG.RData')
## calculate the continuum deviations for 404:2451 nm removing the two outliers
crDyadic <- apply(reflectAllSG[whichDataOut,],1, cHullDeviation, rangeB = 404:2451)
## calculate the wavelet decomposition
wvPeriodic <- apply(crDyadic,2,wd, bc = 'periodic',filter.number = 2, family = 'DaubExPhase')
## extract the wavelet coefficients
wvCoefs <- ldply(wvPeriodic, function(x){unlist(llply(0:10,waveCoefs, wdObject=x))})
## calculate the variance
coefVars <- apply(wvCoefs, 2, var)
## and the order
varOrder <- order(coefVars, decreasing = T)

  save(crDyadic, file = 'savedData/crDyadic.RData')
  save(wvPeriodic, file = 'savedData/wvPeriodic.RData')
  save(wvCoefs, file = 'savedData/wvCoefs.RData')
  save(varOrder,file = 'savedData/varOrder.RData')
  
}
waveletFunction()


## @knitr wavelets-cr-abs
## create quadratic polynomial models





