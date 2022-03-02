test_that("create_RFCurveArray", {
  testthat::skip_on_cran()
  local_edition(3)

  ## set files for tests
  files <-
    list.files(system.file("extdata", "", package = "RLumSTARR"), full.names =
                 TRUE)

  ## import standard
  expect_s3_class(create_RFCurveArray(files), "RLumSTARR_RFCurveArray")

  ## import alternative
  a <- array(runif(300, 0,255), c(10,10,3))
  roi <- matrix(c(2.,4,2,5,6,7,1,1,1), ncol = 3)

  files <-  merge_RLum(lapply(list(roi, roi, roi), function(x) Luminescence::extract_ROI(a, x)))
  t <- expect_s3_class(create_RFCurveArray(t), "RLumSTARR_RFCurveArray")


})

