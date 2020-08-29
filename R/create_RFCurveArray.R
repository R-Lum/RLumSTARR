#'@title Create Multidimensional Curve Arrays from File input
#'
#'@description Helper function to create a multidimensional curve array to
#'prepare the Bayesian process
#'
#'@param files [list] (**required**): list of `.rf` files to be imported
#'
#'@return Returns a list with two arrays for the `RF_nat` and the `RF_reg` curve
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
create_RFCurveArray <- function(
  files
){

## import files (we do not use the self-call option on purpose)
files <- lapply(files, Luminescence::read_RF2R)

ROI_table <- lapply(1:length(files), function(x) {

  ##get ROI information
  df <- Luminescence::plot_ROI(files[[x]], plot = FALSE)$ROI

  ##extract signal sums
  signal <- t(vapply(files[[x]], function(s) {
    RF_nat <- sum(s@records[[1]][-1,2]) ## minus the first, because it is an artefact
    RF_reg <- sum(s@records[[2]][-1,2]) ## minus the first, because it is an artefact
    c(RF_nat, RF_reg)
  }, numeric(2)))

  cbind(GROUP = x, df, sum_RF_nat = signal[,1], sum_RF_reg  = signal[,2])
})

## extract ROI information
ROI_AREA <- vapply(ROI_table, function(x) x[,c("area")], numeric(nrow(ROI_table[[1]])))

## create super array RF_nat
RF_nat_time <- files[[1]][[1]]@records[[1]][,1]
RF_nat <- array(data = NA, c(length(RF_nat_time), nrow(ROI_AREA), ncol(ROI_AREA)))

## create super array RF_reg
RF_reg_time <- files[[1]][[1]]@records[[2]][,1]
RF_reg <- array(data = NA, c(length(RF_reg_time), nrow(ROI_AREA), ncol(ROI_AREA)))

## fill arrays
for(i in 1:dim(RF_nat)[3]){
  for(j in 1:dim(RF_nat)[2]) {
    RF_nat[,j,i] <- files[[i]][[j]]@records[[1]]@data[,2]
    RF_reg[,j,i] <- files[[i]][[j]]@records[[2]]@data[,2]
  }

}

## set dimnames
dim_name <- vapply(1:ncol(ROI_AREA), function(x) paste(ROI_AREA[,x], collapse = ", "), character(1))
dimnames(RF_nat) <- list(RF_nat_time, rownames(ROI_AREA), dim_name)
dimnames(RF_reg) <- list(RF_reg_time, rownames(ROI_AREA), dim_name)

return(list(RF_nat, RF_reg))
}
