library(futile.logger)
source('Infrastructure/LibraryImports.R')
source('Infrastructure/GlobalConstants.R')
source('Infrastructure/LoggingFunctions.R')
source("./Level1/PEDSnet CDM/v2/Execute_Level1_PEDSnet_DQA.R", chdir=T)
source("./Level2/Execute_Level2_PEDSnet_DQA.R", chdir = T)

#Fixes rJava out of memory exception
options(java.parameters = "-Xmx1024m")

flog.appender(appender.file("dqa.log"), name='debug.io')

flog.info('Log started!')
runDQA<-function(level)
{
  if (level == 1) {
    
    runAndLog(
      FUN = executeLevel1DQA,
      success_log = 'Level 1 DQA succesfully finished!',
      error_log='Level 1 DQA failed to finish')
    
  } else if (level == 2) {

    runAndLog(
      FUN=executeLevel2DQA,
      success_log = 'Level 2 DQA succesfully finished!',
      error_log='Level 2 DQA failed to finish')
    
  } else {
    message('Level must be either 1 or 2')
  }
}
