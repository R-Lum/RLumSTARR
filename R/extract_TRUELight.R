#'@title Extract True Light from the Camera Measurements using a Bayesian
#'Approach
#'
#'@description A Bayesian modelling approach to extract the true light using the
#'expanding region-of-interest (ROI) approach proposed by Cunningham and Clark-Balzan (2017).
#'The function will return the results for a **single** curve from a **single** ROI
#'
#'@details
#'
#'**Method control**
#'
#'Supported options to be passed via the parameter `method_control`, most
#'of them are used internally for the calls to [runjags::run.jags] and [rjags::coda.samples].
#'
#'\tabular{llll}{
#'**PARAMETER** \tab **TYPE** \tab **DEFAULT** \tab **DESCRIPTION**\cr
#'`n.chain` \tab [numeric] \tab `3` \tab the number of MCMC chains \cr
#'`thin` \tab [numeric] \tab `10` \tab thinning interval used for the monitoring\cr
#'`burnin` \tab [numeric] \tab `4000` \tab number of burn-in iterations, cf. [runjags::run.jags] \cr
#'`sample` \tab [numeric] \tab `10000` \tab number of total number of samples, `0` skips the MCMC sampling \cr
#'`adapt`  \tab [numeric] \tab  `1000` \tab number of adaptive iterations, cf. [runjags::run.jags] \cr
#'`summarise` \tab [logical] \tab `FALSE` \tab calculate output statistics \cr
#'`method` \tab [character] \tab `"rjags"` \tab way `runjags` will call JAGS, possible are `'rjags'`, `'simple'`, `'interruptible'`, `'parallel'`, `'rjparallel'`, `'background'`, `'bgparallel'` or `'snow'` cf. [runjags::run.jags] \cr
#' jags.refresh` \tab [numeric] \tab `0.1` \tab refresh rate of update of the iteration process, select larger values of complex and long runs \cr
#'`variable.names` \tab [character] \tab `"alpha"` \tab variable names to monitor, `alpha` is always monitored \cr
#'`model` \tab [character] \tab the default model \tab the bugs model
#'}
#'
#'*Note: The argument `model` allows to heavily modified the underlying model. To avoid crashes the paramters passed by `variable.names` will always be cross-checked against parameteres present in the model. Unknown parameters will be skipped!*
#'
#'@param data [array] (**required**): object created by [create_RFCurveArray]
#'
#'@param element [character] (*with default*): element from the input to be analysed,
#''supported are only `RF_nat` or `RF_reg`
#'
#'@param ROI [numeric] (*optional*): ROI to be analysed, if nothing is given
#'all ROIs are analysed, however, the first ROIS is discarded!
#'
#'@param stepping [numeric] (*with default*): stepping parameter that allows
#'you to model only every xth (the value in `stepping`). This option can be extremely
#'useful to play with data because it dramatically improves the modelling speed because
#'less data are considered.
#'
#'@param method_control [list] (*optional*): parameter to be passed to `rjags`.
#'Supported are `n.chain`, `n.iter`, `thin`, `variable.names`, `model`, see details for more.
#'
#'@param verbose [logical] (*with default*): enable/disable terminal feedback
#'
#'@return Returns a list of class `RLumSTARR_TRUELight` with an the following elements:
#'
#' `...$RF_curve`: [Luminescence::RLum.Data.Curve-class] object
#'(the RF curve with the true light)
#'
#'`...$rjags_output`: [rjags::coda.samples] output for further processing.
#'*Note: Regardless the observed variable, the parameter alpha
#'will always be used to create the curve*
#'
#'`...$model`: the model used to run the Bayesian process, use [writeLines] to have
#'nicely formatted terminal output
#'
#'@keywords datagen
#'
#'@section Function version: 0.1.1
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@examples
#'
#'## list files using package external data
#'files <- list.files(system.file("extdata", "", package="RLumSTARR"), full.names=TRUE)
#'
#'## create curve array
#'dat <- create_RFCurveArray(files = files)
#'output <- extract_TRUELight(
#'  data = dat,
#'  ROI = c(4),
#'  stepping = 10,
#'  verbose = FALSE,
#'  method_control = list(
#'    n.chain = 2,
#'    sample = 100,
#'    thin = 20))
#'
#'@references Cunningham, A.C., Clark-Balzan, L., 2017. Overcoming crosstalk in
#'luminescence images of mineral grains. Radiation Measurements 106, 498–505.
#'doi:10.1016/j.radmeas.2017.06.004
#'
#'@seealso [create_RFCurveArray], [get_MCMCParameters]
#'
#'@md
#'@export
extract_TRUELight <- function(
  data,
  element = c("RF_nat", "RF_reg"),
  ROI = 2,
  stepping = 1,
  method_control = list(),
  verbose = TRUE
) {

# Input check -------------------------------------------------------------
if(attr(data, "class") != "RLumSTARR_RFCurveArray")
  stop("[extract_TRUELight()] input must be of type RLumSTARR_RFCurveArray!", call. = FALSE)

if(!any(element[1] %in% names(data)))
  stop("[extract_TRUELight()] element invalid", call. = FALSE)

## select
data <- data[[element[1]]]

# Bayesian model core -----------------------------------------------------
model_default <- "model {
  for (i in 1:TIMES) {
     ##priors
     alpha[i] ~ dlnorm(0, 1) T(0, )

     a_alpha[i] ~ dnorm(0.2, 1/(0.2^2)) T(0.1, 1)
     b_alpha[i] ~ dnorm(1, 1/(25^2)) T(0, )

     for (j in 1:length(ROI_AREA)) {
       ##set liklihoods
       Y[i, j] ~ dnorm(mu[i,j], 1)

         ##parameters
         mu[i,j] <- phi[i, j] + omega[i,j] + epsilon[i,j]

         ##the internal light contribution
         phi.star[i,j] <- alpha[i] * delta_alpha[i, j]
         delta_alpha[i, j] <- 1 - a_alpha[i] * exp(-ROI_AREA[j] / b_alpha[i])

         ##external light contribution
         omega.star[i,j] ~ dlnorm(0,1)

         ## error component ... it looks like a normal distribution
         epsilon[i, j] ~ dnorm(600,1E-01)

     }
     ## apply ordering constraints
     ## (decending for phi, ascending for omega)
     phi[i, 1:length(ROI_AREA)] <- -sort(-phi.star[i, ])
     omega[i, 1:length(ROI_AREA)] <- sort(omega.star[i, ])

   }
}"

# Bayesian process --------------------------------------------------------
method_control <- modifyList(x = list(
    n.chain = 3,
    sample = 10000,
    thin = 10,
    burnin = 4000,
    adapt = 1000,
    summarise = FALSE,
    method = 'rjags',
    jags.refresh = 0.1,
    variable.names = NULL,
    model = model_default),
  val = method_control)

## take input
Y <- data[seq(1,nrow(data),stepping[1]),abs(ROI[1]),]
temp_area <- unlist(strsplit(dimnames(data)[[3]], ","))
roi_area <- as.numeric(temp_area[seq(2,length(temp_area),dim(data)[2])])
colnames(Y) <- roi_area

## make sure the variable names exist in the model and can be observed
## this also avoids a lot input errors fast testing of modified models
variable.names <- character()
for(v in unique(c("alpha", method_control$variable.names))){
  if(grepl(v, method_control$model[1], fixed = TRUE)) {
    variable.names <- c(variable.names, v)

  } else {
    warning(paste0("[extract_TRUELight()] variable ",v," unknown in the model; skipped!"))

  }
}

# Run JAGS ----------------------------------------------------------------
if(verbose) cli::cat_rule("[extract_TRUELight()]")

## run modelling
jags_output <- suppressWarnings(runjags::run.jags(
  model = method_control$model[1],
  monitor = variable.names,
  n.chain = method_control$n.chain[1],
  thin = method_control$thin[1],
  data = list(
    Y = Y,
    ROI_AREA = roi_area,
    TIMES = nrow(Y)),
  silent.jags = !verbose[1],
  jags.refresh = method_control$jags.refresh,
  summarise = method_control$summarise[1],
  method = method_control$method[1],
  burnin = method_control$burnin[1],
  sample = method_control$sample[1],
  adapt = method_control$adapt[1]))



# Output ------------------------------------------------------------------
## extract only alpha values, this way we can observe more variables
## without trashing the curve
if(length(jags_output$mcmc[[1]]) > 0) {
  alpha <- get_MCMCParameters(jags_output$mcmc, "alpha")

} else {
  alpha <- matrix(NA,ncol = 1, nrow = length(rownames(Y)))

}

## set new RLum object
curve <- Luminescence::set_RLum(
  class = "RLum.Data.Curve",
  recordType = "RF",
  curveType = "measured",
  data = cbind(as.numeric(rownames(Y)), alpha))

output <- list(RF_curve = curve, jags_output = jags_output, model = method_control$model[1])
attr(output, "class") <- "RLumSTARR_TRUELight"
attr(output$model, "class") <- "RLumSTARR_model"

return(output)
}
