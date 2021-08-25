#'@title Extract True Light from the Camera Measurements using a Bayesian
#'Approach
#'
#'@description A Bayesian modelling approach to extract the true light using the
#'expanding region-of-interest (ROI) approach proposed by Cunningham and Clark-Balzan (2017).
#'The function will return the results for a single curve.
#'
#'@param data [array] (**required**): curve array created by [create_RFCurveArray]
#'
#'@param ROI [numeric] (*with default*): ROI to be analysed
#'
#'@param method_control [list] (*optional*): parameter to be passed to `rjags`.
#'Supported are `n.chain`, `n.iter`, `thin`, `variable.names`, `model`
#'
#'@return Returns a list with an [Luminescence::RLum.Data.Curve-class] object
#'(the RF curve with the true light) and the [rjags::coda.samples] output for
#'further processing. *Note: Regardless the observed variable, the parameter alpha
#'will be always be used to create the curve*
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@examples
#'
#'##TODO
#'
#'@references Cunningham, A.C., Clark-Balzan, L., 2017. Overcoming crosstalk in
#'luminescence images of mineral grains. Radiation Measurements 106, 498–505.
#'doi:10.1016/j.radmeas.2017.06.004
#'
#'@md
#'@export
extract_TRUELight <- function(
  data,
  ROI = 1,
  method_control = list()
) {

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
    n.chain = 1,
    n.iter = 200,
    thin = 20,
    variable.names = NULL,
    model = model_default),
  val = method_control)

##extract area values ... we skip the first ROI, since this is the biggest
##TODO: this might crash
Y <- data[,ROI[1],]
temp_area <- unlist(strsplit(dimnames(data)[[3]], ","))
roi_area <- as.numeric(temp_area[seq(2,length(temp_area),dim(data)[2])])
colnames(Y) <- roi_area

##set model (we do this here to close the model connect)
model <- textConnection(method_control$model)

##set jags model
jags <- rjags::jags.model(
    file = model,
    n.chain = method_control$n.chain,
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
    n.iter = method_control$n.iter,
    thin = method_control$thin
  )


## extract only alpha values, this way we can observe more variables
## without trashing the curve
alpha <- get_MCMCParameter(jags_output, "alpha")

## set new RLum object
curve <- Luminescence::set_RLum(
  class = "RLum.Data.Curve",
  curveType = "RF",
  data = cbind(as.numeric(rownames(data)), alpha))

return(list(curve = curve, jags_output = jags_output))
}

