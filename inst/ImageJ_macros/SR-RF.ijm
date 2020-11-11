////////////////////////////////////////////////////////////////////////////////
// ImageJ macro for grain-wise radioluminescence curve extraction from images
// Authors: Dirk Mittelstrass (dirk.mittelstrass@luminescence.de),
//          Sebastian Kreutzer (sebastian.kreutzer@aber.ac.uk)
// Licence: GPL-3

macro "SR-RF" {
//
  macro_version = "0.1.0";
  macro_date = "2020-09-21";
//
//
////////////////////////////////////////////////////////////////////////////////
// Instant help ----------------------------------------------------------------
  help = "<html>"
     +"<h2>SR-RF ImageJ macro instant help</h2>"
     +"Macro authors: Dirk Mittelstrass and Sebastian Kreutzer<br>"
     +"Macro version: " + macro_version + " (" + macro_date + ")<br>"
     +"Licence: GPL-3<br>"
     +"<h4>Background image</h4>"
     +"This option allows to specify an image used for background subtraction"
     +"<ul>"
     +"<li> 'none': The default, no background is subtracted"
     +"<li> 'take_from_RF_reg': The last 100 slices of the RF_reg image stack are taken."
     +" Be careful with that option, it will bias your results if the background shows a curvature!"
     +"<li> all other options allow to specify a specify image stack of your choice"
     +"</ul>"
     +"<h4>Parameter 'group sizes'</h4>"
     +"This parameter groups the images. The number specifies the size of each group."
     +"Grouping helps to remove spikes and to smoothen the image."
     +"<ul>"
     +"<li> Group size = 1: Nothing will be done, no grouping is applied."
     +"<li> Group size = 2: Pixels with the lowest intensity from two images are taken to create a new image."
     +"<li> Group size >= 3: A median filter is applied."
     +"</ul>"
     +"Recommended are uneven group sizes, e.g., 3,5,7,..., <br>"
     +"<h4>Parameter 'Image alignment'</h4>"
     +"Enables or disables the geometry correction."
     +"<h4>Parameter 'Delete first slices'</h4>"
     +"Deletes the first slice of each image; this is useful in the case the camera has produced image artefacts."
     +"If set the channel time is corrected automatically by an offset, considering the deleted slices the first."
     +"<h4>Parameter 'Noise tolerance'</h4>"
     +"Noise tolerance level used for the grain identification. You have to play with the parameter to find a meaningful value for your data."
     +"<h4>Parameter 'ROI diameter'</h4>"
     +"The ROI diameter in pixels. The size depends on your grain-size fraction and the chosen binning."
     +"Rule of thumb for the recommended 2x2 binning: 50-80 µm: 3 px; 120–160 µm: 5 px; ca 250 µm: 8 px"
     +"<h4>Aliquot ROI parameters</h4>"
     +"The settings allow you do modify the shape of the ROIs. Just try it."
     +"<h4>Use pre-defined ROIs </h4>"
     +"This settings allows you to use pre-defined ROIs, which were stored in ROIs.zip in the same folder as the input data."
     +"Alternatively you are free to modify the ROIs during the data processing.<br>"
     +"<i>If pre-defined ROIs are used, all other ROI settings are ignored!</i>"
     +"<h4>Parameter 'Channel time'</h4>"
     +"The channel time chosen in your measurement sequence."
     +"<h4>Save output options</h4>"
     +"These options allow you to save the graphical output in a subfolder in the input path of your images."
     +"<h2>Batch mode</h2>"
     +"This macro can be also run in a non-interactive mode from the terminal. "
     +"In such as case the macro enters the batch mode, which does not allow further user interaction.<br>"
     +"The batch mode requires that the macro is called with all arguments in their correct order. Example: <br>"
     +"<code>./ImageJ-macosx  -batch /Applications/Fiji.app/macros/SR-RF.ijm /Test_RF/+default+default+take_from_RF_reg+9+1+10+8+0.5+0.5+0.9+5+0+0+0</code><br>"
     +"All arguments need to separated by '+'. For further details contact the macro authors"
     +"";
////////////////////////////////////////////////////////////////////////////////
//Startup messages and checks
batch_mode = is("Batch Mode");
arguments = getArgument();
//
print("[SR-RF macro version: " + macro_version + " (" + macro_date + ")]");
if(batch_mode == 1) { print(">> Entering batch mode...."); }
print(">> Called with:" + arguments);
//
//
// ########## HIDDEN PARAMETERS ###########
//
// Alternative bright spot remover
remove_outliers = false;
outlier_threshold = 300;
outlier_tracer_radius = 3;

// Image alignment: Translation interpolation mode
alignment_interpolation = "None"; //available: "None", "Bilinear", "Bicubic"

//Lock-up table for the image colouring
//display_LUT = "Blue Orange icb" // reasonable compromise between Gray and spectral view ... and looks nice
display_LUT = "Grays"; // ImageJ default
//display_LUT = "Spectrum"  // Red -> rainbow -> white; classic false colour view
//display_LUT = "16 Colors";  // Black -> inverted rainbow -> red; useful to see signals cross talk

//Further display properties
//the following parameters doesn't change data evaluation in any way
display_saturation = "0.4"; //higher = brighter
display_ROI_color = "yellow"; //change to "black" if display_LUT = "Spectrum"
display_ROI_font_size = 12; //smaller if you use binned images

//variable setting (this will change further below)
background_subtraction = true;

// video properties
video_framerate = 10;
video_compression = "JPEG"; //available: "JPEG", "PNG", "none"

// #######################################
// ToDo-List:
// - revalute grain finder algorithm
// - add Table functions
// - add Plot functions
// - translate scale bar into µm

//############### Preset arguments ###############
if(arguments == ""){
  preset_input_path = "";
  preset_RF_NAT = "";
  preset_RF_REG = "";
  preset_BG_subtraction = "none";
  preset_image_group_size = 5;
  preset_image_alignment = true;
  preset_first_slices_rm = false;
  preset_noise_tolorance = 20;
  preset_ROI_size = 7;
  preset_center_x = 0.5;
  preset_center_y = 0.5;
  preset_diameter = 0.9;
  preset_use_predefined_ROI_file = false;
  preset_channel_time = 5;
  preset_save_workflow_images = false;
  preset_save_additional_results = false;
  preset_save_signal_decay_videos = false;
  preset_offset_time = 0;
} else {
  //extract arguments if they come in with the macro call
  arguments = split(arguments, "+");

  //assign arguments
  preset_input_path = arguments[0];
  preset_RF_NAT = arguments[1];
  preset_RF_REG = arguments[2];
  preset_BG_subtraction = arguments[3];
  preset_image_group_size = arguments[4];
  preset_image_alignment = arguments[5];
  preset_first_slices_rm = arguments[6];
  preset_noise_tolorance = arguments[7];
  preset_ROI_size = arguments[8];
  preset_center_x = arguments[9];
  preset_center_y = arguments[10];
  preset_diameter = arguments[11];
  preset_use_predefined_ROI_file = arguments[12];
  preset_channel_time = arguments[13];
  preset_save_workflow_images = arguments[14];
  preset_save_additional_results = arguments[15];
  preset_save_signal_decay_videos = arguments[16];
  preset_offset_time = arguments[17];

}

//###############Load images of needed from path################################
//this is the case that we run the macro from a terminal
if(nImages < 2 && preset_input_path != "") {
  list = getFileList(preset_input_path);
  for (i = 0; i < list.length; i++){
    if(endsWith(list[i],".tif")){
      open(preset_input_path + list[i]);
    }
  }
}

//this is the case that the macro was opened the first time
if(batch_mode == false && nImages < 2){
  preset_input_path = File.openDialog("Select a path");
  dir = File.getParent(preset_input_path);
  list = getFileList(dir);
  for (i = 0; i < list.length; i++){
    if(endsWith(list[i],".tif")){
      open(list[i]);
    }
  }
}

//############### Create dialog box for the user settings ######################
//
//find available stacks, taken from:
//http://imagej.1557.x6.nabble.com/Selecting-image-windows-as-a-menu-in-ijm-td4888539.html
  StackTitles = newArray(nImages);
  nStacks=0;
  for (i = 1; i <= nImages; i++) {
    selectImage(i);
    if (nSlices > 1) {
      StackTitles[nStacks] = getTitle();
      nStacks++;
    }
  }
if (nStacks < 2) exit("Less than 2 image stacks available");

ImageTitles = newArray(nImages + 2);
ImageTitles[0] = "none";
ImageTitles[1] = "take_from_RF_reg";
for (i = 1; i <= nImages; i++) {
    selectImage(i);
    ImageTitles[(i+1)] = getTitle();
}

// File selection --------------------------------------------------------------
//###  display dialog box to set parameters ###
if(batch_mode != 1){
  Dialog.create("SR-RF macro to process IR-RF images (" + macro_version + ")");
  if(preset_RF_NAT == "") {
    Dialog.addChoice("RF_nat: Natural dose RF stack", Array.sort(StackTitles), StackTitles[0]);

  } else {
    Dialog.addChoice("RF_nat: Natural dose RF stack", Array.sort(StackTitles), preset_RF_NAT);

  }

  if(preset_RF_REG == "") {
    Dialog.addChoice("RF_reg: Bleached sample RF stack", Array.sort(StackTitles), StackTitles[1]);

  } else{
    Dialog.addChoice("RF_reg: Bleached sample RF stack", Array.sort(StackTitles), preset_RF_REG);

  }

  // Background subtraction ------------------------------------------------------
  Dialog.addMessage("--------- Background subtraction ---------");
  Dialog.addChoice("Signal background (optional)", ImageTitles, preset_BG_subtraction);

  // Parameterisation ------------------------------------------------------
  Dialog.addMessage("--------- Parametrise image manipulation ---------");
  Dialog.addNumber("Image group size", preset_image_group_size);
  Dialog.addCheckbox("Apply image alignment", preset_image_alignment);
  //Dialog.addCheckbox("Crop aliquot area", true);
  Dialog.addCheckbox("Delete first slices", false);

  // ROI selection parameters
  Dialog.addMessage("--------- Parametrise regions of interest ------------");
  Dialog.addMessage("Grain finder:");
  Dialog.addNumber("Noise tolerance (cts)", preset_noise_tolorance);
  Dialog.addNumber("ROI diameter (pixel)", preset_ROI_size);
  Dialog.addMessage("Aliquot ROI:");
  Dialog.addNumber("Center X (0-1)", preset_center_x);
  Dialog.addNumber("Center Y (0-1)", preset_center_y);
  Dialog.addNumber("Diameter (0-1)", preset_diameter);
  Dialog.addCheckbox("Use pre-defined ROIs", false);

  // ROI selection parameters
  Dialog.addMessage("--------- Result options ---------------------------------");
  Dialog.addNumber("Channel time (sec)", preset_channel_time);
  Dialog.addCheckbox("Save workflow images", preset_save_workflow_images);
  Dialog.addCheckbox("Save additional result pictures", preset_save_additional_results);
  Dialog.addCheckbox("Save signal decay videos", preset_save_signal_decay_videos);
  Dialog.addHelp(help);
  Dialog.show();

  //read DialogBox parameters and set secondary parameters
  nat_title = Dialog.getChoice();
  reg_title = Dialog.getChoice();

  if (nat_title == reg_title) exit("Image stacks for natural and bleached measurement are the same. Please reconsider stack selection.");

  bac_title = Dialog.getChoice();
  if (bac_title == "none") background_subtraction = false;

  //image processing parameters
  running_median = Dialog.getNumber();
  image_alignment = Dialog.getCheckbox();
  //crop_aliquot = Dialog.getCheckbox();
  first_slices_rm = Dialog.getCheckbox();

  // ROI selection parameters
  grain_finder_threshold = Dialog.getNumber();
  grain_d = Dialog.getNumber();
  aliquot_x = Dialog.getNumber();
  aliquot_y = Dialog.getNumber();
  aliquot_d = Dialog.getNumber();
  //You painted your own ROIs and don't want to use the automatic ROI selection algorithm?
  use_predefined_ROI_file = Dialog.getCheckbox();

  channel_time = Dialog.getNumber();
  temporary_data = Dialog.getCheckbox();
  result_pictures = Dialog.getCheckbox();
  decay_videos = Dialog.getCheckbox();

} else {

  //take values from the input ... in batch mode only
  preset_input_path = arguments[0];
  print(" >> Working directory: " + preset_input_path);

  //automated or manual window selection
  if(preset_RF_NAT == "default") {
    nat_title = StackTitles[0];
  } else {
    nat_title = preset_RF_NAT;

  }

  if(preset_RF_REG == "default") {
    reg_title = StackTitles[1];

  } else {
    reg_title = preset_RF_REG;

  }

  print(" >> RF_nat: " + nat_title);
  print(" >> RF_reg: " + reg_title);

  bac_title = preset_BG_subtraction;
  if (bac_title == "none") background_subtraction = false;

  running_median = parseFloat(preset_image_group_size);
  image_alignment = preset_image_alignment;
  first_slices_rm = preset_first_slices_rm;
  grain_finder_threshold = parseFloat(preset_noise_tolorance);
  grain_d = parseFloat(preset_ROI_size);
  aliquot_x = parseFloat(preset_center_x);
  aliquot_y = parseFloat(preset_center_y);
  aliquot_d = parseFloat(preset_diameter);
  use_predefined_ROI_file = preset_use_predefined_ROI_file;
  channel_time = parseFloat(preset_channel_time);
  temporary_data = preset_save_workflow_images;
  result_pictures = preset_save_additional_results;
  decay_videos = preset_save_signal_decay_videos;

}

// calculate offset
if(first_slices_rm==true || first_slices_rm=="true"){
  offset_time = running_median * channel_time;
} else {
  offset_time = preset_offset_time;
}

// Create subdirectory based on actual date and time as save path
save_name = nat_title + "_";
 getDateAndTime(year, month, dayOfWeek, dayOfMonth, hour, minute, second, msec);
 save_name = save_name + year + "-" ;
 if (month<10) {save_name = save_name+"0";}
 save_name = save_name + month + "-";
 if (dayOfMonth<10) {save_name = save_name+"0";}
 save_name = save_name + dayOfMonth + "_";
 if (hour<10) {save_name = save_name+"0";}
 save_name = save_name + hour;
 if (minute<10) {save_name = save_name+"0";}
 save_name = save_name + minute;
 if (second<10) {save_name = save_name+"0";}
 save_name = save_name + second;

 input_path = getDirectory("image");
 save_path = getDirectory("image") + save_name;
 File.makeDirectory(save_path);

// currently unused; to reduce memory usage further
function crop(stack_name) {
	selectWindow(stack_name);
	roiManager("Select", 0);
	run("Crop");
}

//########## MAIN CODE  ############
// delete frist slices
if(first_slices_rm==true || first_slices_rm == "true"){
  selectWindow(nat_title);
  setSlice(1);
  run("Delete Slice");

  selectWindow(reg_title);
  setSlice(1);
  run("Delete Slice");

}


// this function executes step 1 to 3. The function calls can be found below the function
function create_corrected_substack(raw_stack, new_name){

	selectWindow(raw_stack);

	// 1.	The image stack is reduced to a length multiple of the running median length
	new_length = floor(nSlices / running_median) * running_median;

	while (nSlices > new_length) {
		setSlice(nSlices);
		run("Delete Slice");
	}

	// 2.	The stacks are transformed into a median projection.
	if(running_median==1){
		run("Duplicate...", "duplicate");
	}
	if(running_median==2){
		run("Grouped Z Project...", "projection=[Min Intensity] group=2");
	}
	if(running_median > 2){
		run("Grouped Z Project...", "projection=Median group=" + running_median);
	}
	rename(new_name);
	//close("temp");
	selectWindow(new_name);

	// 2.1.	Optional: An outlier removal algorithm is applied
	if(remove_outliers) {
		for(i=1; i <= nSlices; i++){
			setSlice(i);
			run("Remove Outliers...", "radius=" + outlier_tracer_radius +" threshold=" + outlier_threshold +" which=Bright slice");
		};
	};

	//NOTE: put in here further pre-processing, for example running mean or software binning
};
// Apply running median of the RF stacks
create_corrected_substack(nat_title, "nat");
create_corrected_substack(reg_title, "reg");

// 4.	Optional: A background image, also processed by step 1 to 4, is subtracted from every other image
function background_correction (signal_stack, background_image){
	selectWindow(signal_stack);
	rename("temp");
	imageCalculator("Subtract create stack", "temp", background_image);
	rename(signal_stack);
	close("temp");
}

if(background_subtraction){
  if(bac_title == "take_from_RF_reg") {
    selectWindow(reg_title);
    run("Make Substack...", "  slices="+nSlices-99+"-"+nSlices);
    run("Z Project...", "projection=Median");
    background_image = getTitle();
  } else {
  	selectWindow(bac_title);
  	if(nSlices > 1) {
  		run("Z Project...", "projection=Median");
  		background_image = getTitle();
  	} else {
  		background_image = bac_title;
  	}
  }
  background_correction("nat", background_image);
  background_correction("reg", background_image);
}

// 5.	A median signal picture is created from the running median stacks
selectWindow("nat");
run("Z Project...", "projection=Median");
rename("MEDIAN_nat");

selectWindow("reg");
run("Z Project...", "projection=Median");
//run("Z Project...", "projection=[Average Intensity]");
rename("MEDIAN_reg");

// 5.	Optional: An image alignment algorithm minimises the deviation
// between the both mean signal pictures and gives the translation and rotation parameters back
// Its the same algorithm as in AgesGalore2GUI (Greilich et al. 2015)

translation_x = 0; translation_y = 0; rotation = 0;
getDimensions(width, height, channels, slices, frames);
if(image_alignment == true || image_alignment == "true"){

// 5.1. get rotation and translation parameters
// taken from ...
	run("TurboReg ",
		"-align "
		+ "-window MEDIAN_reg " // Source (window reference).
		+ "0 0 " + (width - 1) + " " + (height - 1) + " " // No cropping.
		+ "-window MEDIAN_nat " // Target (file reference).
		+ "0 0 " + (width - 1) + " " + (height - 1) + " " // No cropping.
		+ "-rigidBody " // This corresponds to rotation and translation.
		+ (width / 2) + " " + (height / 2) + " " // Source translation landmark.
		+ (width / 2) + " " + (height / 2) + " " // Target translation landmark.
		+ (width / 4) + " " + (height / 2) + " " // Source first rotation landmark.
		+ (width / 4) + " " + (height / 2) + " " // Target first rotation landmark.
		+ (3 * width / 4) + " " + (height / 2) + " " // Source second rotation landmark.
		+ (3 * width / 4) + " " + (height / 2) + " " // Target second rotation landmark.
		+ "-hideOutput");

	sourceX0 = getResult("sourceX", 0); // First line of the table.
	sourceY0 = getResult("sourceY", 0);
	targetX0 = getResult("targetX", 0);
	targetY0 = getResult("targetY", 0);
	sourceX1 = getResult("sourceX", 1); // Second line of the table.
	sourceY1 = getResult("sourceY", 1);
	targetX1 = getResult("targetX", 1);
	targetY1 = getResult("targetY", 1);
	sourceX2 = getResult("sourceX", 2); // Third line of the table.
	sourceY2 = getResult("sourceY", 2);
	targetX2 = getResult("targetX", 2);
	targetY2 = getResult("targetY", 2);

	// Calculate Translation
	dx = targetX0 - sourceX0;
	dy = targetY0 - sourceY0;
	//if (abs(dx) > dxmax) dxmax = abs(dx);
	//if (abs(dy) > dymax) dymax = abs(dy);

	// Calculate Rotation
	dxa = sourceX2 - sourceX1;
	dya = sourceY2 - sourceY1;
	sourceAngle = atan2(dya, dxa);
	dxa = targetX2 - targetX1;
	dya = targetY2 - targetY1;
	targetAngle = atan2(dya, dxa);
	rotation = targetAngle - sourceAngle; // Amount of rotation, in radian units.
	rotation = rotation * 180.0 / PI;	 // ... and degree

	translation_x = dx;
	translation_y = dy;

	print(">> Translation in x direction [pixel]: " + dx);
	print(">> Translation in y direction [pixel]: " + dy);
	print(">> Rotation [degree]: " + rotation);

	// now rotate and translate the bleached measurement data
	selectWindow("reg");
	run("Rotate... ", "angle=" + rotation + " grid=1 interpolation=" + alignment_interpolation + " stack");
	run("Translate...", "x=" + dx + " y=" + dy + " interpolation=" + alignment_interpolation + " stack");

	selectWindow("MEDIAN_reg");
	run("Rotate... ", "angle=" + rotation + " grid=1 interpolation=" + alignment_interpolation + " slice");
	run("Translate...", "x=" + dx + " y=" + dy + " interpolation=" + alignment_interpolation + " slice");
}

// 7.	The mean signal pictures from step 4 (eventually corrected by step 5 and 6) are combined to a “ROI source” picture
imageCalculator("Average create", "MEDIAN_nat","MEDIAN_reg");
rename("ROI_source");
//run("Subtract Background...", "rolling=20 disable");

// 9.	The ROI source picture is searched for local maximums to define them as grains
// They get marked, if their value is a predefined threshold higher than the surrounding pixels (noise tolerance)
// lets make the signals visible
function tune_contrast(){
  setMinAndMax(min, max);
  run(display_LUT);
    //ToDo:
  //zoom if binned
  //relocate	windows
}

roiManager("reset");

if(use_predefined_ROI_file == false || use_predefined_ROI_file == "false") {
  run("Find Maxima...", "noise=" + grain_finder_threshold + " output=[Point Selection] exclude");
  roiManager("Add");
  getSelectionCoordinates(xpoints, ypoints);
  roiManager("Delete");

  // 10.	Set ROI for the whole aliquot
  getDimensions(width, height, channels, slices, frames);
  x_up_left = round(width * (aliquot_x - aliquot_d / 2));
  y_up_left = round(height * (aliquot_y - aliquot_d / 2));
  oval_width = round(width * aliquot_d);
  oval_height = round(height * aliquot_d);
  makeOval(x_up_left, y_up_left, oval_width, oval_height);
  roiManager("Add");

  // 10.1. Tune the image contrast on the whole-aliquot-ROI of the ROI source image. Use the resulting display properties for ALL images
  run("Enhance Contrast", "saturated=" + display_saturation);
  run(display_LUT);
  getMinAndMax(min, max); // remember the displayed value range to set later images to the same range

  //roiManager("Deselect");
  selectWindow("nat"); tune_contrast();
  selectWindow("reg"); tune_contrast();
  selectWindow("MEDIAN_nat"); tune_contrast();
  selectWindow("MEDIAN_reg"); tune_contrast();
  selectWindow("ROI_source");

  // 10.	 Around every maximum, a circle with predefined radius is outlined. These circles represent the ROIs
  grain_r = floor(grain_d / 2);
  for (i=0; i < lengthOf(xpoints); i++) {
  	makeOval(xpoints[i] - grain_r, ypoints[i] - grain_r, grain_d, grain_d);
  	roiManager("Add");
  }

} else {
  print(">> Loading pre-defined ROIs ...");
  roiManager("Open", input_path + "/ROIs.zip");
  run(display_LUT);
  getMinAndMax(min, max); // remember the displayed value range to set later images to the same range

}

//the if-condition is needed due to a bug in ImageJ it does not recognise the
//macro environment in the batch mode and shows an image
if(batch_mode != 1) roiManager("Set Color", display_ROI_color);
roiManager("show All with labels");
//re-arrange windows
run("Tile");

// At this point stops the script and gives the user the chance to rearrange and
// modify the ROI collection by using the functions of the ROI manager
if(batch_mode != 1) waitForUser("You can now add/delete/manipulate ROIs if wanted.\nOK when done.");

// 12.	The ROI collection is saved as a ZIP file, containing TXT files with geometric data.
roiManager('save', save_path + "/ROIs.zip");

// 13.	The result CSV file is now built. First, some ROI properties
table_string = "<macro_version=" + macro_version + " imagej_version=" + IJ.getFullVersion + " ID=" + save_name
				+ " source_nat=" + nat_title + " source_reg=" + reg_title + " source_background=" + bac_title
				+ " image_width=" + width + " image_height=" + height
				+ " running_median=" + running_median + " background_subtraction=" + background_subtraction + " image_alignment=" + image_alignment
				+ " translation_x=" + translation_x + " translation_y=" + translation_y + " rotation=" + rotation
				+ " grain_threshold=" + grain_finder_threshold + " grain_d=" + grain_d
				+ ">\n";

table_string = table_string + "<Statistics>\n";
first_line = "ROI:";
second_line = "x:";
third_line = "y:";
fourth_line = "width:";
fifth_line = "height:";
sixth_line = "area:";

for (i=0; i < roiManager("count"); i++) {
	first_line = first_line + "\tROI " + (i + 1);
	roiManager("select", i);
	getSelectionBounds(x, y, width, height);
	getStatistics(area, mean);
	second_line = second_line + "\t" + x;
	third_line = third_line + "\t" + y;
	fourth_line = fourth_line + "\t" + width;
	fifth_line = fifth_line + "\t" + height;
	sixth_line = sixth_line + "\t" + area;
}
table_string = table_string + first_line + "\n" + second_line + "\n" + third_line + "\n" + fourth_line + "\n" + fifth_line + "\n" + sixth_line+ "\n";
table_string = table_string + "</Statistics>\n\n";

// 13.1. A text string is continuously built, containing the mean pixel value of each ROI of each corrected signal image (“nat” and “reg” stack)
function write_data(selected_window) {

	// Table header:
	data_line = "No.\ttime (sec)";
	for (i=0; i < roiManager("count"); i++) {
		if(i==0) {
			data_line = data_line + "\taliquot";
		} else {
			data_line = data_line + "\tgrain " + i;
		}
	}
	data_line = data_line + "\n";

	// Table:
	selectWindow(selected_window);
	for(i=1; i <= nSlices; i++){
    values_line = toString(i) + "\t" + (((i-0.5) * running_median * channel_time) + offset_time);
    setSlice(i);
		for (j=0; j < roiManager("count"); j++) {
			roiManager("select", j);
			getStatistics(area, mean);
			values_line = values_line + "\t" + mean;
		}
		data_line = data_line + values_line + "\n";
	}
	return data_line;
}

table_string = table_string + "<Natural>\n" + write_data("nat") + "</Natural>\n\n";
table_string = table_string + "<Bleached>\n" + write_data("reg") + "</Bleached>";

// 14.	This text string is saved as ASCII file in the results subdirectory with tabulator characters as delimiters
File.saveString(table_string, save_path + "/table.rf");

// 14.1. One overview picture shall be saved. A new function is needed
function create_picture(window, file_name, image_type, uniform_contrast) {

	selectWindow(window);
	run("Select None");
	roiManager("Show All");
	roiManager("Show None");
	run("Duplicate...", "title=" + file_name);

	if(uniform_contrast==true){
		tune_contrast();
	} else {
		run("Grays");
		roiManager("select", 0);
		run("Enhance Contrast", "saturated=" + display_saturation);
		run("Select None");
	}

	if(image_type=="scale"){
		run("Scale Bar...", "width=50 height=4 font=12 color=White background=None location=[Lower Right] bold label");
		run("Size...", "width=512 height=512 constrain average interpolation=None");
		run("Calibration Bar...", "location=[Upper Right] fill=White label=Black number=5 decimal=0 font=12 zoom=1 overlay");
		run("Flatten");
	}
	if(image_type=="ROIs"){
		roiManager("show All without labels");
		run("Flatten");
		roiManager("show None");
		run("Size...", "width=512 height=512 constrain average interpolation=None");
	}
	if(image_type=="ROIs_labeled"){
		roiManager("show All with labels");
		run("Flatten");
		roiManager("show None");
		run("Size...", "width=512 height=512 constrain average interpolation=None");
	}

	//run("Label...", "format=Text starting=0 interval=1 x=5 y=20 font=12 text=[" + text + ": ]");
	saveAs("PNG", save_path + "/" + file_name + "_" + image_type + ".png");
	close(file_name + "*");
	//selectWindow(file_name); close();
}

create_picture("ROI_source", "superposition", "scale", true);
create_picture("ROI_source", "superposition", "ROIs_labeled", true);

if(temporary_data == true || temporary_data == "true"){
	selectWindow("nat"); save(save_path + "/nat.tif");
	selectWindow("reg"); save(save_path + "/reg.tif");
	selectWindow("MEDIAN_nat"); save(save_path + "/MEDIAN_nat.tif");
	selectWindow("MEDIAN_reg"); save(save_path + "/MEDIAN_reg.tif");
	selectWindow("ROI_source"); save(save_path + "/ROI_source.tif");
	if(background_subtraction){
		selectWindow(background_image); save(save_path + "/background.tif");
	}
}

//  Several additional PNG pictures are created and saved for documentation purposes in the same subdirectory
if(result_pictures == true || result_pictures == "true"){

	// Median pictures
	create_picture("ROI_source", "superposition", "plain", true);
	create_picture("ROI_source", "superposition", "ROIs", true);
	create_picture("MEDIAN_nat", "natural_median", "plain", true);
	create_picture("MEDIAN_nat", "natural_median", "ROIs", true);
	create_picture("MEDIAN_nat", "natural_median", "scale", true);
	create_picture("MEDIAN_reg", "bleached_median", "plain", true);
	create_picture("MEDIAN_reg", "bleached_median", "ROIs", true);
	create_picture("MEDIAN_reg", "bleached_median", "scale", true);
	if(background_subtraction){
		create_picture(background_image, "background", "plain", false);
		create_picture(background_image, "background", "scale", false);
	}

	// First/Last pictures
	selectWindow("nat"); setSlice(1); run("Select None"); run("Duplicate...", "title=FIRST_nat");
	create_picture("FIRST_nat", "natural_first", "plain", true);
	create_picture("FIRST_nat", "natural_first", "scale", true);
	selectWindow("nat"); setSlice(nSlices); run("Select None"); run("Duplicate...", "title=LAST_nat");
	create_picture("LAST_nat", "natural_last", "plain", true);
	create_picture("LAST_nat", "natural_last", "scale", true);

	selectWindow("reg"); setSlice(1); run("Select None"); run("Duplicate...", "title=FIRST_reg");
	create_picture("FIRST_reg", "bleached_first", "plain", true);
	create_picture("FIRST_reg", "bleached_first", "scale", true);
	selectWindow("reg"); setSlice(nSlices); run("Select None"); run("Duplicate...", "title=LAST_reg");
	create_picture("LAST_reg", "bleached_last", "plain", true);
	create_picture("LAST_reg", "bleached_last", "scale", true);

	// Difference pictures :
	imageCalculator("Subtract create", "MEDIAN_nat","MEDIAN_reg");
	rename("DIFF_MEDIAN");
	create_picture("DIFF_MEDIAN", "medians_difference", "plain", false);
	create_picture("DIFF_MEDIAN", "medians_difference", "scale", false);
	selectWindow("DIFF_MEDIAN"); close();

	imageCalculator("Subtract create", "FIRST_nat","LAST_nat");
	rename("DIFF_nat");
	create_picture("DIFF_nat", "natural_decay", "plain", false);
	create_picture("DIFF_nat", "natural_decay", "scale", false);
	selectWindow("DIFF_nat"); close(); selectWindow("FIRST_nat"); close(); selectWindow("LAST_nat"); close();

	imageCalculator("Subtract create", "FIRST_reg","LAST_reg");
	rename("DIFF_reg");
	create_picture("DIFF_reg", "bleached_decay", "plain", false);
	create_picture("DIFF_reg", "bleached_decay", "scale", false);
	selectWindow("DIFF_reg"); close(); selectWindow("FIRST_reg"); close(); selectWindow("LAST_reg"); close();
}
if(decay_videos == true || decay_videos == "true"){
	// AVI videos of the nat and reg stack (as they appear after step 6) are created and saved in the results subdirectory
	function make_video(window, text){
		selectWindow(window);
		roiManager("Show All with labels");
		roiManager("Show None");
		tune_contrast();
		start_time = 0.5 * running_median * channel_time;
		interval_time = running_median * channel_time;
		//run("Scale Bar...", "width=50 height=4 font=12 color=White background=None location=[Lower Right] bold label");
		run("Size...", "width=512 height=512 constrain average interpolation=None");
		run("Calibration Bar...", "location=[Upper Right] fill=White label=Black number=5 decimal=0 font=12 zoom=1 overlay");
		run("Label...", "format=Text starting=0 interval=1 x=5 y=20 font=18 text=[" + text + ": ] range=1-"  + nSlices);
		run("Label...", "format=0 starting=" + start_time + " interval=" + interval_time + " x=90 y=20 font=18 text=sec range=1-" + nSlices);
		run("AVI... ", "compression=" + video_compression + " frame=" + video_framerate + " save=" + save_path + "/video_" + text + ".avi");
		//close();
	}
	make_video("nat", "natural");
	make_video("reg", "bleached");
}

//re-arrange windows
run("Tile");

if(batch_mode != 1) {
  Dialog.create("ROI evaluation finished");
  Dialog.addMessage("Data was saved successfully!\n \nclick OK to close all windows except the source file and the macro window \nto retry with different parameters or evaluate another measurement file  \n\nclick CANCEL to keep all windows open");
  Dialog.show();
}

//close windows
image_list = getList("image.titles");
window_list = getList("window.titles");
for (i=2; i<image_list.length; i++) close;
for (i=0; i<window_list.length; i++) close(window_list[i]) ;

//re-run macro but preserve settings ... but not in batch mode
//otherwise we have an endless loop
if(batch_mode != 1){
  first_slices_rm = false; //otherwise we create a delete loop
  runMacro("SR-RF", input_path + "+"
    + nat_title + "+"
    + reg_title + "+"
    + bac_title + "+"
    + running_median + "+"
    + image_alignment + "+"
    + first_slices_rm + "+"
    + grain_finder_threshold + "+"
    + grain_d + "+"
    + aliquot_x + "+"
    + aliquot_y + "+"
    + aliquot_d + "+"
    + use_predefined_ROI_file + "+"
    + channel_time + "+"
    + temporary_data + "+"
    + result_pictures + "+"
    + decay_videos + "+"
    + offset_time);
} else {
  print(">> Save path: " + save_path);

}
exit();
}//end macro
