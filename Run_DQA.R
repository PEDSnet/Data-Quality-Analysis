library(futile.logger)
source('Infrastructure/LibraryImports.R')
source('Infrastructure/ReportImports.R', chdir = T)
source('Infrastructure/GlobalConstants.R')
source('Infrastructure/LoggingFunctions.R')
source('Resources/site_info.R')
source("./Main/Level1/v2/Execute_Level1_PEDSnet_DQA.R", chdir=T)
source("./Main/Level2/Execute_Level2_PEDSnet_DQA.R", chdir = T)

Sys.setenv(TZ="GMT")
Sys.setenv(ORA_SDTZ="GMT") 

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

generateSingleReport <- function(level, report) {
    test_report = NULL
    if(level == 1) {
        test_report <- g_level1_reports[[report]]
    } else if (level == 2) {
     
      
        test_report <- g_level2_reports[[report]]
    }

    if (is.null(test_report)) {

        if(level != 1 && level != 2) {
            flog.error("Level must be 1 or 2")
        } else if (level == 1) {
            flog.error("%s is not a valid level 1 report", report)
            flog.info("Available reports are as follows: ")
            flog.info(ls(g_level1_reports))
        } else {
            flog.error("%s is not a valid level 2 report", report)
            flog.info("Available reports are as follows: ")
            flog.info(ls(g_level2_reports))
        }
    }
    else {
        runAndLog(
            FUN = test_report,
            success_log = paste(report, ' report succesfully generated.', sep=""),
            error_log = paste('Failed to generate ', report, ' report, see dqa.log for more details.', sep="")
        )
    }
}
