#'@title Extracts a parameter from an MCMC list
#'
#'@description Short cut to extract a parameter from an MCMC list or an object of class
#'`RLumSTARR_TRUELight` created by the functions [extract_TRUELight] or [run_TRUELightExtraction].
#'In the latter case, the row names of the extracted matrices have the same dimension as the RF curve.
#'If more processing is wanted, `'coda'` package should be used.
#'
#'@param mcmc [coda::mcmc] or [coda::mcmc.list] (**required**): input object,
#'if created by [extract_TRUELight] the correct object is extracted automatically
#'
#'@param parameter [character] (*optional*): name of the parameter to be extracted. If `NULL` (the default)
#'all found parameters are extracted and the result is a list with matrices of those parameter. If `parameter`
#'is a vector, the function will try to extract the names parameters.
#'
#'@param prob [numeric] (*with default*): probability for the HPD calculation (cf. [coda::HPDinterval])
#'
#'@param unlist [logical] (*with default*): if `TRUE` the output is a [matrix] of the means
#'of the lower and upper intervals of the parameter. If the parameter was estimated based on
#'multiple chains, this chains are also subject to an average calculation. If `FALSE` the output is a
#'[list] as returned by [coda::HPDinterval]
#'
#'@return Returns a matrix with the parameter value or a named [list] with such matrices if `parameters` has
#'a length > 1.
#'
#'@section Function version: 0.1.1
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@seealso [coda::HPDinterval], [extract_TRUELight]
#'
#'@examples
#'
#'## load example files
#'files <- list.files(system.file("extdata", "", package="RLumSTARR"), full.names=TRUE)
#'
#'##prepare data and run model
#'dat <- create_RFCurveArray(files = files)
#'output <-
#'  extract_TRUELight(
#'    data = dat,
#'    ROI = c(4),
#'    stepping = 60,
#'    verbose = FALSE,
#'    method_control = list(
#'    n.chain = 1,
#'    n.iter = 50,
#'    thin = 20))
#'
#'##extract parameters
#'get_MCMCParameters(output)
#'
#'@md
#'@export
get_MCMCParameters <- function(
  mcmc,
  parameter = NULL,
  prob = 0.95,
  unlist = TRUE
) {

# Input check -------------------------------------------------------------
  ##add support objects from this package ... autmatic mcmc and row name extraction
  if(attr(mcmc, "class") == "RLumSTARR_TRUELight") {
    row_names <- as.character(mcmc[["RF_curve"]]@data[,1])
    mcmc <- mcmc[["jags_output"]]
    attr(mcmc, "row_names") <- row_names

  }

  ##check name parameter
  if(is.null(parameter))
    parameter <- unique(regmatches(coda::varnames(mcmc), regexpr(".[^\\[]+",coda::varnames(mcmc))))

# Parameter is a vector call -----------------------------------------------
  if(length(parameter)>1) {
    temp <- lapply(parameter, function(p){
      get_MCMCParameters(mcmc, parameter = p, prob, unlist)
    })
    names(temp) <- parameter
    return(temp)

  }

# Run extraction ----------------------------------------------------------
  ## set parameters for column
  ncol <- 1
  valid_names <- unlist(strsplit(coda::varnames(mcmc),"[",TRUE))
  valid_names <- unique(valid_names[seq(1, length(valid_names), 2)])

  ##check input
  if(!parameter[1] %in% valid_names)
    stop("[get_MCMCParameters()] Invalid parameter name, valid names are: ",
         paste(valid_names, collapse= ", "), call. = FALSE)

  ## extract parameter
  l <- mcmc[,grepl(coda::varnames(mcmc), pattern = paste0("^",parameter[1]))]

  ##if the list is more complex we have to be do more ... we only test
  ##the first because it should be ok for the rest
  if(grepl(coda::varnames(l[[1]])[1], pattern = ",", fixed = TRUE)){
    names <- coda::varnames(l)
    names <- unique(unlist(strsplit(names, ",", TRUE))[seq(2, length(names) * 2, 2)])
    ncol <- max(as.numeric(unlist(strsplit(names, "]", TRUE))))

  }

  ## calculate HPD interval and condense into matrix if needed
  l <- coda::HPDinterval(l, prob = prob[1])
  if(unlist) {
     l <- matrix(rowMeans(matrix(unlist(l), ncol = 2 * length(l))), ncol = ncol)
     colnames(l) <- paste0(parameter[1],"_",1:ncol)
  }

  ##set row names
  if(!is(l, "list") && !is.null(attr(mcmc, "row_names")))
    rownames(l) <- attr(mcmc, "row_names")

# Return ------------------------------------------------------------------
  return(l)
}
