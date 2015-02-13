pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  dir <- directory; pol <- pollutant; datasets <- id
  
  if(pol=="sulfate"){
    chemcol <- 2             #column indicator, column 2 for sulfate
  } else if(pol=="nitrate"){
    chemcol <- 3             #column indicator, column 3 for nitrate
  }
#  print(c(dir,pol,chemcol))

  chemtot <- vector()
  for (i in datasets){
    j <- formatC(i,width=3,flag="0")
    chem <- na.omit(read.csv(paste(dir,"/",j,".csv",sep=""))[,chemcol])
#    chem <- chem[,chemcol]
#    chem <- na.omit(chem)
    chemtot <- c(chemtot,chem)
  }
  chemmean <- mean(chemtot)
  chemmean
}