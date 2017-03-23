timeFunction<-function(FUN, ...) {
  total_time = system.time(expr=FUN(...))
}

runAndLog<-function(FUN, success_log, error_log, ...) {
  tryCatch(
    expr = {
      run_time<-timeFunction(FUN, ...)
      output_msg = paste(success_log, ". Finished in %s seconds.", sep='')
      flog.info(output_msg, run_time, name='debug.io')

      flog.info(success_log)
    },error = function(e) {
      flog.info(error_log)

      output_msg = paste(error_log, " : ", e, sep='')
      flog.info(output_msg, name='debug.io')
    }
    )
}
