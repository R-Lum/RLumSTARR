#'@title Melt output from RLumSTARR into simple data frames
#'
#'@description The function provides a convenient way to convert the objects
#'created by RLumSTARR into simple [data.frame]s that can be processed conveniently by other
#'functions for example the package `ggplot2`.
#'
#'@param x (**required**): input object of class `RLumSTARR_RFCurveArray` or
#'`RLumSTARR_TRUELight`
#'
#'@param ... further parameters, currently not used
#'
#'@return [data.frame]
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University
#'(United Kingdom)
#'
#'@example
#'
#'## list files using package external data
#'files <- list.files(system.file("extdata", "", package="RLumSTARR"), full.names=TRUE)
#'
#'## create curve array
#'dat <- create_RFCurveArray(files = files)
#'
#'## melt
#'df <- melt_RLumSTARR(dat)
#'
#'@md
#'@export
melt_RLumSTARR <- function(x, ...) {
  df <- NULL

  # RLumSTARR_RFCurveArray -------------------
  if(attr(x, "class") == "RLumSTARR_RFCurveArray") {
    df <- data.table::rbindlist(lapply(names(x), function(x){
        ## get dim names
        dim_names <- dimnames(x)
        x_len <- length(dim_names[[1]])
        y_len <- length(dim_names[[2]])
        z_len <- length(dim_names[[3]])

        ## extract columns
        data.frame(
          CHANNEL_ID = rep(rep(1:x_len, y_len), z_len),
          TIME = as.numeric(rep(rep(dim_names[[1]], y_len), z_len)),
          ROI_ID = rep(dim_names[[2]], each = x_len * z_len),
          ROI_AREA = as.numeric(rep(unlist(
            strsplit(dim_names[[3]], ", ", fixed = TRUE)), each = x_len)),
          VALUE = as.numeric(x)
        )
    }))
  }
  # RLumSTARR_TRUELight -------------------
  if(attr(x, "class") == "RLumSTARR_TRUELight") {
    ## extract parameters
    l <- get_MCMCParameters(x)

    ## get time values
    TIME <- as.numeric(rownames(l[[1]]))

    df <- data.table::rbindlist(lapply(names(l), function(x){
      data.frame(
        TIME = TIME,
        PARAMETER = x,
        CATEGORY = 1:ncol(l[[x]]),
        VALUE = as.numeric(l[[x]])
      )
    }))

  }

  return(df)
}
