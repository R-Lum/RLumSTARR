#'@title Run Automated TRUE-Light Extraction over multiple datasets
#'
#'@description Runs [extract_TRUELight] automatically in a parallel processing
#'mode using [parallel::mclapply]. In essence, the function calls [extract_TRUELight]
#'and extracts the RF curves from the results and constructs an [Luminescence::RLum.Analysis-class]
#'for further analyses.
#'
#'@param data [array] (**required**): object created by [create_RFCurveArray]
#'
#'@param ROI [numeric] (*optional*): ROIs to be analysed, if nothing is given
#'all ROIs are analysed, however, the first ROIS is discarded!
#'
#'@param stepping [numeric] (*with default*): the stepping parameter from and to
#'be passed to [extract_TRUELight]
#'
#'@param mc.cores [numeric] (*with default*): number of cores used for the processing,
#'passed to [parallel::mclapply]
#'
#'@param method_control [list] (*optional*): parameters to be passed to [extract_TRUELight]
#'
#'@param include_jags_output [logical] (*with default*): allows to include the output
#'from JAGS as info object in the [Luminescence::RLum.Analysis-class] output objects
#'for further diagnostics. Setting this option to `TRUE` is not recommended for large datasets,
#'since it will tremendously inflate the size of output and consume a lot of memory.
#'If in-depth diagnostics are required, the function [extract_TRUELight] is recommended
#'instead.
#'
#'@param verbose [logical] (*with default*): enable/disable verbose mode. The output of
#'the MCMC sampling using `rjags` is **always** silent.
#'
#'@return The output is a [list] containing [Luminescence::RLum.Analysis-class] objects
#'with two [Luminescence::RLum.Data.Curve-class] objects for `RF_nat` and `RF_reg` respectively.
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@examples
#'
#'## list files using package external data
#'files <- list.files(system.file("extdata", "", package="RLumSTARR"), full.names=TRUE)
#'## create curve array
#'dat <- create_RFCurveArray(files = files)
#'output <- run_TRUELightExtraction(
#' data = dat,
#' stepping = 15,
#' mc.cores = 1,
#' ROI = 5,
#' verbose = TRUE,
#' method_control = list(
#'  n.chain = 1,
#'  sample = 0,
#'  thin = 20))
#'
#'@md
#'@export
run_TRUELightExtraction <- function(
  data,
  ROI,
  stepping = 1,
  mc.cores = max(c(1, parallel::detectCores() - 2)),
  method_control = list(),
  include_jags_output = FALSE,
  verbose = TRUE
) {

  ##set ROI ... remove first
  if(is.null(ROI)) ROI <- 2:dim(data[[1]])[2]

  ## set ROI names
  ROI_names <- colnames(data[[1]])

  if(verbose) {
    cat("\n[run_TRULightExtraction()]\n\n")
    cli::cli_alert_success("Setting up functions")
    if(length(method_control) > 0){
      text <- method_control

      if(!is.null(text$model)) text$model <- "<user defined model>"

      cli::cli_alert_info("Applying user-defined modelling parameters:")
      cat("  -->", paste(paste0("  ",names(text), ": ",text)), "\n")

    }
    cli::cli_alert_info("Entering number-crunching ... ")
  }

  .fun <- function(x, stepping, method_control, include_jags_output) {
    records <- list()
    for(i in c("RF_nat", "RF_reg")){
      system(paste("echo '--[+] running extraction ",i," ROI: ", x, "'"))
      records[[i]] <- extract_TRUELight(
        data,
        element = i,
        ROI = x,
        stepping = stepping[1],
        verbose = FALSE,
        method_control = method_control)
    }

    system(paste0("echo '--\033[32m[\xE2\x9C\x94]\033[39m <combining RF_nat and RF_reg ROI: ",x, "> (", ROI_names[x],")'"))

    ## set info object
    info <- list(ROI = ROI_names[x])
    if(include_jags_output){
      info <- c(info, list(
         jags_output = list(
          RF_nat = records$RF_nat$jags_output,
          RF_reg = records$RF_reg$jags_output
        )))
    }

    ## create output object
    Luminescence::set_RLum(
      "RLum.Analysis",
      originator = "run_TRUELightExtraction",
      protocol = "Bayesian analysis",
      records = list(records$RF_nat$RF_curve, records$RF_reg$RF_curve),
      info = info
    )

  }

# Run extraction ----------------------------------------------------------
  if(verbose) cli::cat_rule("START")
  output <-
    parallel::mclapply(
      ROI,
      FUN = .fun,
      method_control = method_control,
      mc.cores = mc.cores,
      stepping = stepping[1],
      include_jags_output = include_jags_output
    )
  if(verbose) cli::cat_rule("DONE")


# Return ------------------------------------------------------------------
  ## assign ROI names
  names(output) <- ROI_names[ROI]

  return(output)
}
