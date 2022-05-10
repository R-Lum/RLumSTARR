#'@title Run ImageJ SR-RF macro
#'
#'@description The script runs the SR-RF ImageJ macro in batch mode out of
#'R
#'
#'@param path [character] (**required**): path to files to be analysed
#'
#'@param RF_nat [character] (*default*): name of the RF_nat file
#'
#'@param RF_reg [character] (*default*): name of the RF_reg file
#'
#'@param bg_rm [character] (*with default*): background subtraction options.
#'Allowed are `none` (no background subtraction), `take_from_RF_reg` (takes the
#'last 100 channels from the RF_reg signal: dangerous) or `<your file name>` (this
#'does not work in batch mode)
#'
#'@param image_group_size [numeric] (*with default*): grouping value for running median
#'to remove outliers
#'
#'@param image_alignment [logical] (*with default*): enable/disable image alignment
#'
#'@param first_slices_rm [logical] (*with default*): remove first slice of each curve set
#'
#'@param noise_tolorance [numeric] (*with default*): noise tolerance parameter
#'
#'@param ROI_size [numeric] (*with default*): ROI size in pixel
#'
#'@param center_x [numeric] (*with default*): aliquot ROI centre x-coordinate
#'
#'@param center_y [numeric] (*with default*): aliquot ROI centre y-coordinate
#'
#'@param diameter [numeric] (*with default*): relative diameter aliquot ROI
#'
#'@param use_predefined_ROIs [logical] (*with default*): use pre-defined ROIs imported
#'from a file `ROIs.zip` found in the same folder as the files
#'
#'@param channel_time [numeric] (*with default*): channel time, this parameter was set the
#'moment the sequence was written
#'
#'@param save_workflow_images [logical] (*with default*): enable/disable writing of additional workflow images
#'
#'@param save_additional_results [logical] (*with default*): enable/disable writing of additional workflow images
#'
#'@param save_signal_decay_videos [logical] (*with default*): enable/disable writing of additional workflow videos
#'
#'@param offset_time [numeric] (*with default*): offset time for the time axis
#'
#'@param .ImageJ [numeric] (*with default*): Path to ImageJ (the macro is shipped with the package)
#'
#'@return This functions returns the path of the analysed data
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@examples
#'
#'##TODO
#'
#'@md
#'@export
run_ImageJ <- function(
  path,
  RF_nat = "default",
  RF_reg = "default",
  bg_rm = "take_from_RF_reg",
  image_group_size = 5,
  image_alignment = TRUE,
  first_slices_rm = FALSE,
  noise_tolorance = 10,
  ROI_size = 10,
  center_x = 0.5,
  center_y = 0.5,
  diameter = 0.9,
  use_predefined_ROIs = FALSE,
  channel_time = 5,
  save_workflow_images = FALSE,
  save_additional_results = FALSE,
  save_signal_decay_videos = FALSE,
  offset_time = 0,
  .ImageJ = "/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
) {

  ##construct call
  call_ImageJ <- paste(.ImageJ," -batch", system.file("ImageJ_macros", "SR-RF.ijm", package = "RLumSTARR"))
  call_macro <-
    paste(c(
        paste0(normalizePath(path),"/"),
        RF_nat[1],
        RF_reg[1],
        bg_rm[1],
        image_group_size[1],
        tolower(as.character(image_alignment))[1],
        tolower(as.character(first_slices_rm))[1],
        noise_tolorance[1],
        ROI_size[1],
        center_x[1],
        center_y[1],
        diameter[1],
        tolower(as.character(use_predefined_ROIs))[1],
        channel_time[1],
        tolower(as.character(save_workflow_images))[1],
        tolower(as.character(save_additional_results))[1],
        tolower(as.character(save_signal_decay_videos))[1],
        offset_time[1]),
      collapse = "+"
    )

  ##call
  output <- system(paste(c(call_ImageJ, call_macro), collapse = " "), intern = TRUE)
  writeLines(output)

  return(trimws(strsplit(x = output[grepl(output, pattern = "Save path")], split = ":")[[1]][2]))

}
