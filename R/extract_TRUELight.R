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
#'of them are used internally for the calls to [rjags::jags.model] and [rjags::coda.samples].
#'
#'\tabular{lll}{
#'**PARAMETER** \tab **TYPE** \tab **DESCRIPTION**\cr
#'`n.chain` \tab [numeric] \tab the number of MCMC chains \cr
#'`n.iter` \tab [numeric] \tab number of iterations for the MC runs \cr
#'`thin` \tab [numeric] \tab thinning interval used for the monitoring \cr
#'`variable.names` \tab [character] \tab variable names to monitor, `alpha` is always monitored \cr
#'`model` \tab [character] \tab the bugs model
#'}
#'
#'@param data [array] (**required**): object created by [create_RFCurveArray]
#'
#'@param element [character] (*with default*): element from the input to be analysed,
#''supported are only `RF_nat` or `RF_reg`
#'
#'@param ROI [numeric] (*optional*): ROI to be analysed, if nothing is given
#'all ROIs are analysed, however, the first ROIS is discarded!
#'
#'@param method_control [list] (*optional*): parameter to be passed to `rjags`.
#'Supported are `n.chain`, `n.iter`, `thin`, `variable.names`, `model`, see details for more.
#'
#'@param verbose [logical] (*with default*): enable/disable terminal feedback
#'
#'@return Returns a list with an the following elements:
#'
#' `...$RF_curve`: [Luminescence::RLum.Data.Curve-class] object
#'(the RF curve with the true light)
#'
#'`...$rjags_model`: [rjags::coda.samples] output for further processing.
#'*Note: Regardless the observed variable, the parameter alpha
#'will always be used to create the curve*
#'
#'`...$model`: the model used to run the Bayesian process, use [writeLines] to have
#'nicely formatted terminal output
#'
#'@section Function version: 0.1.0
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
#'output <-
#'extract_TRUELight(
#'  data = dat,
#'  ROI = c(4),
#'  verbose = FALSE,
#'  method_control = list(
#'    n.chain = 1,
#'    n.iter = 50,
#'    thin = 20))
#'
#'@references Cunningham, A.C., Clark-Balzan, L., 2017. Overcoming crosstalk in
#'luminescence images of mineral grains. Radiation Measurements 106, 498–505.
#'doi:10.1016/j.radmeas.2017.06.004
#'
#'@seealso [create_RFCurveArray], [get_MCMCParameter]
#'
#'@md
#'@export
extract_TRUELight <- function(
  data,
  element = c("RF_nat", "RF_reg"),
  ROI = 2,
  method_control = list(),
  verbose = TRUE
) {

# Input check -------------------------------------------------------------
if(attr(data, "class") != "RLumSTARR.RFCurveArray")
  stop("[extract_TRUELight()] input must be of type RLumSTARR.RFCurveArray!", call. = FALSE)

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
    n.iter = 2000,
    thin = 20,
    variable.names = NULL,
    model = model_default),
  val = method_control)

## take input
Y <- data[,abs(ROI[1]),]

temp_area <- unlist(strsplit(dimnames(data)[[3]], ","))
roi_area <- as.numeric(temp_area[seq(2,length(temp_area),dim(data)[2])])
colnames(Y) <- roi_area

##set model (we do this here to close the model connect)
model <- textConnection(method_control$model[1])

##set jags model
jags <- rjags::jags.model(
    file = model,
    n.chain = method_control$n.chain[1],
    quiet = !verbose[1],
    data = list(
      Y = Y,
      ROI_AREA = roi_area,
      TIMES = nrow(data))
)

close(model)

## generate posterior samples
jags_output <-
  rjags::coda.samples(
    model = jags,
    variable.names = c("alpha", method_control$variable.names),
    n.iter = method_control$n.iter[1],
    thin = method_control$thin[1],
    progress.bar = if(verbose[1]) 'text' else 'none'
  )


## extract only alpha values, this way we can observe more variables
## without trashing the curve
alpha <- get_MCMCParameter(jags_output, "alpha")

## set new RLum object
curve <- Luminescence::set_RLum(
  class = "RLum.Data.Curve",
  curveType = "RF",
  data = cbind(as.numeric(rownames(data)), alpha))

output <- list(RF_curve = curve, jags_output = jags_output, model = method_control$model[1])
attr(output, "class") <- "RLumSTARR.TRUELight"

return(output)
}
