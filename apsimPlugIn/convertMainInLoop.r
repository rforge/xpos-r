##
 # FILE convertMainInLoop.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # convert bruce's downscaled format into apsim's met file
 #########################################################################

##
 # REQUIRED SOURCES
 #########################################################################
source('convertFunctions.r');
source('rwfileOp.r');

################################################################################
## USER SETTINGS START HERE
## fill in what you need only. I know it sounds obvious, but ...
################################################################################
## 
 # DATA: where and what
 ###############################################################################
init_paths <- function()
{
path <- 	list(	"input"=			"Z:/Philadelphia/",
			"output"=			"C:/Documents and Settings/crespo/Desktop/ApsimInBergRiver/MetFiles/",
			"data"=list("tmin"=	"tmn/",
					"tmax"=	"tmx/",
					"ppt"=	"ppt/"
			)
		);
return(path);
}

## 
 # GC MODELS NAMES (MODEL 1 IS NCEP)
 ###############################################################################
init_gcmNames <- function()
{
gcmNames <- list(	"ncep"=	list(	"con"=	"ncep2.1",
						"futA"=	NULL,
						"futB"=	NULL),
			"cccm"=	list(	"con"=	"cccma_cgcm3_1",
						"futA"=	"cccma_cgcm3_1-fa",
						"futB"=	"cccma_cgcm3_1-fb"),
			"cnrm"=	list(	"con"=	"cnrm_cm3",
						"futA"=	"cnrm_cm3-fa",
						"futB"=	"cnrm_cm3-fb"),
			"csiro35"=	list(	"con"=	"csiro_mk3_5",
						"futA"=	"csiro_mk3_5-fa",
						"futB"=	"csiro_mk3_5-fb"),
			"gfdl"=	list(	"con"=	"gfdl_cm2_0",
						"futA"=	"gfdl_cm2_0-fa",
						"futB"=	"gfdl_cm2_0-fb"),
			"giss"=	list(	"con"=	"giss_model_e_r",
						"futA"=	"giss_model_e_r-fa",
						"futB"=	"giss_model_e_r-fb"),
			"ipsl"=	list(	"con"=	"ipsl_cm4",
						"futA"=	"ipsl_cm4-fa",
						"futB"=	"ipsl_cm4-fb"),
			"echo"=	list(	"con"=	"miub_echo_g",
						"futA"=	"miub_echo_g-fa",
						"futB"=	"miub_echo_g-fb"),
			"echam"=	list(	"con"=	"mpi_echam5",
						"futA"=	"mpi_echam5-fa",
						"futB"=	"mpi_echam5-fb"),
			"cgcm"=	list(	"con"=	"mri_cgcm2_3_2a",
						"futA"=	"mri_cgcm2_3_2a-fa",
						"futB"=	"mri_cgcm2_3_2a-fb")
	);
return(gcmNames);
}


 ###############################################################################
 ###############################################################################
 # MAIN START HERE
 ###############################################################################
 ###############################################################################

path <- init_paths();
gcms <- init_gcmNames();

# main loop
for (g in 1:length(gcms)){
	for (p in 1:3){
print(gcms[[g]][[p]]);
		path2data <- path;
		path2data$input <- paste(path$input,gcms[[g]][[p]],"/",sep="");
		path2data$output <- paste(path$output,gcms[[g]][[p]],"/",sep="");
		convert_OneStation4OnePeriod(	path2data,		# path to data
							"0021130_A.txt",	# station Name for temperature files
							"00211300.txt",	# station Name for precipitation files
							"philadelphia",	# my output file name (no extension)
							inland=TRUE		# inland {TRUE,FALSE}
				     			);
		if(g==1) break;
	}
}

