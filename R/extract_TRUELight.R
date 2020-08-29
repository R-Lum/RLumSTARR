#'@title Extract True Light from the Camera measurements using a Bayesian
#'Approach
#'
#'@description Run the Bayesian model to extract the true light from a ROI
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
#'further processing.
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@examples
#'
#'##TODO
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
     alpha[i] ~ dnorm(0, 1) T(0, )
     beta[i] ~ dnorm(0, 1) T(0, )

     a_alpha[i] ~ dnorm(0.2, 1/(0.2^2)) T(0.1, 1)
     b_alpha[i] ~ dnorm(50, 1/(25^2)) T(0, )

     a_beta[i] ~ dnorm(0.2, 1/(0.2^2)) T(0.1, 1)
     b_beta[i] ~ dnorm(50, 1/(25^2)) T(0, )


     for (j in 1:length(ROI_AREA)) {
       ##set liklihoods
       # Y[i, j] ~ dnorm(phi[i, j] + omega[i, j], 1)
       Y[i, j] ~ dnorm(phi[i, j] + omega[i,j] + epsilon[i, j], 1)

         ##the internal light contribution
         phi.star[i,j] <- alpha[i] * delta_alpha[i, j]
         delta_alpha[i, j] <- 1 - a_alpha[i] * exp(-ROI_AREA[j] / b_alpha[i])

         ##external light contribution
         omega.star[i, j] <- beta[i] * delta_beta[i, j]
         delta_beta[i, j] <- 1 - a_beta[i] * exp(-ROI_AREA[j] / b_beta[i])

         ## error component ... it looks like a normal distribution
         epsilon[i, j] ~ dnorm(500,1)

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
    n.iter = 1000,
    thin = 100,
    variable.names = "alpha",
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
    variable.names = method_control$variable.names,
    n.iter = method_control$n.iter,
    thin = method_control$thin
  )

## calculate HPD
HPD <- coda::HPDinterval(jags_output)

## set new RLum curve
curve <- Luminescence::set_RLum(class = "RLum.Data.Curve",
                  curveType = "RF",
                  data = matrix(c(as.numeric(rownames(data)), rowMeans(
                    matrix(unlist(HPD), ncol = 2 * method_control$n.chain)
                  )), ncol = 2))

return(list(curve = curve, jags_output = jags_output))
}