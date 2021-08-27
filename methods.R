#'methods_RLuSTARR
#'
#' @param x: input object
#'
#' @param y: input object
#'
#' @keywords internal
#' @md
#' @name methods_RLumSTARR
NULL

####################################################################################################
# methods for generic: plot()
# ##################################################################################################
#' @rdname methods_RLum
#' @method plot RLumSTARR.RFCurveArray
#' @export
plot.RLumSTARR.RFCurveArray <- function(x, y = NULL, ...) {
  for(z in 1:dim(x[[1]])[3]) {
    if(max(par()$mfrow) == 1) {
      on.exit(par(mfrow = c(1,1)))
      par(mfrow = c(1,2))
    }
    for(c in c("RF_nat", "RF_reg")) {
      dim_names <- dimnames(x[[c]])
      zlab <- as.numeric(strsplit(dim_names[[3]], split = ",", fixed = TRUE)[[z]])
      x[[c]][, , z] <- t(t(x[[c]][, , z])/matrixStats::colMaxs(x[[c]][, , z]))

      graphics::matplot(
        x = as.numeric(dim_names[[1]]),
        y = x[[c]][, , z],
        type = "l",
        lty = 1,
        lwd = 1,
        col = khroma::color("smooth rainbow")(),
        xlab = "Time [s]",
        ylab = "norm. IR-RF [a.u.]",
        main = paste0(c, " #", z)
      )
      mtext(side = 3, paste("ROI area(s): ", paste(unique(zlab), collapse = ",")), cex = 0.7)
    }
  }
}

plot(x = dat)
