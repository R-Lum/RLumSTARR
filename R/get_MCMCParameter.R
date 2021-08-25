#'@title Extracts a parameter from an MCMC list
#'
#'@description Short cut to extract a parameter from an MCMC list. If more processing
#'is wanted, the `'coda'` package can be used
#'
#'@param mcmc [coda::mcmc] or [coda::mcmc.list] (**required**): input object,
#'if created by [extract_TRUELight] the correct object is extracted automatically
#'
#'@param parameter [character] (**required**): name of the parameter to be extracted
#'
#'@param prob [numeric] (*with default*): probability for the HPD calculation (cf. [coda::HPDinterval])
#'
#'@param unlist [logical] (*with default*): if `TRUE` the output is a [matrix] of the means
#'of the lower and upper intervals of the parameter. If the parameter was estimated based on
#'multiple chains, this chains are also subject to an average calculation. If `FALSE` the output is a
#'[list] as returned by [coda::HPDinterval]
#'
#'@return Returns a matrix with the parameter value or a [list]
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@seealso [coda::HPDinterval]
#'
#'@examples
#'
#'##TODO
#'
#'@md
#'@export
get_MCMCParameter <- function(
  mcmc,
  parameter,
  prob = 0.95,
  unlist = TRUE
) {
  ##add support objects from this package
  if(attr(mcmc, "class") == "RLumSTARR.TRUELight")
    mcmc <- mcmc[["jags_output"]]

  ## set parameters for column
  valid_names <- unlist(strsplit(coda::varnames(mcmc),"[",TRUE))
  valid_names <- unique(valid_names[seq(1, length(valid_names), 2)])

  ##check input
  if(!parameter[1] %in% valid_names)
    stop("[get_MCMCParameter()] Invalid parameter name, valid names are: ",
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
     l <- matrix(rowMeans(matrix(unlist(l), ncol = 2 * length(l))), ncol = 1)
     colnames(l) <- parameter[1]
  }

# Return ------------------------------------------------------------------
  return(l)
}

