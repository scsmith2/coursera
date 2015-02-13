corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  dir <- directory
  t <- threshold
  
#  chemtot <- data.frame()
  cors <- vector(mode="numeric")
  k <- 0
  for (i in 1:332){
    j <- formatC(i,width=3,flag="0")
    chem <- read.csv(paste(dir,"/",j,".csv",sep=""))
    chem <- chem[complete.cases(chem),]
    if(length(chem[,1])<t){
      next
    } else {
    k <- k+1
#    chemtot <- rbind(chemtot,chem)
    cors[k] <- cor(chem[,2],chem[,3])
    }
  }
  cors <- na.omit(cors)
  cors
}