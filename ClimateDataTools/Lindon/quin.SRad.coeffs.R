##############################################################################################################
# Title      : quin.SRad.coeffs.R
# Purpose    : Creates output table of best fitting atmospheric transmissivity coefficients for each quinary 
#              catchment for calculating SRad
# Author     : Lyndon Estes
# Draws from : s_rad_function_25032011.R; csm.funcs.R; ldefuncs.R 
# Used by    : arc.SRad.coeffs.R
# Notes      : Uses solRadH function to predicted SRad values using Hargreaves approach using a range of 
#              atmospheric transmissivity coefficients, calculates RMSE between each predicted values and 
#              SRad from Quinary Catchment, and produces output table containing the Quinary ID number, its 
#              DSSAT specific identifier, lat, long, and elevation of Quinary centroid, the best fitting
#              atmospheric transmissivity coefficient (Krs), and the RMSE value. 
#              The portion that reads in the Quinary Catchment data is currently written to read in DSSAT
#              .wth files. It should be quite easily adapted to read in from other formats (e.g. original 
#              UKZN data or UCT format).
##############################################################################################################

setwd("/DSSAT45/WEATHER")

solRadH <- function(jd, lat, elev, tmn, tmx, Krs) {
# Calculates solar radiation function according to modified Hargreaves method, adapted from code
# developed by Olivier Crespo of University of Cape Town CSAG.
# Args: 
#   jd: Vector of Julian days of the year (not Julian dates), e.g. 365 for Dec. 31 in non-leap year
#   lat: Latitude, negative for Southern Hemisphere
#   elev: Elevation in m
#   tmn: Vector of minimum temperatures
#   tmx: Vector of maximum temperatures
#   Krs: Coefficient for atmospheric transmissivity
# Returns: 
#   Vector of solar radiation values for time series analyzed:
# References:
#   Ball, R.A., Purcell, L.C., and Carey, S.K. 2004. Evaluation of solar radiation prediction models in North 
#     America. Agronomy Journal, 96(2), 391.
#   Allen, R.G., Pereira, L.S., Raes, D., Smith, M. et al. 1998. Crop evapotranspiration-Guidelines for 
#     computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome.

  Gsc <- 0.0820  # solar constant = 0.0820 MJ.m^(-2).min^(-1)
  phi <- pi * lat / 180  # latitude [rad] (N.B. South lat shouls be negative)
  delta <- 0.409 * sin((2 * pi * jd / 365) - 1.39)  # solar decimation [rad]
  Dr <- 1 + 0.033 * cos(2 * pi * jd / 365)  # inverse relative distance Earth-Sun
  Ws <- acos(-tan(phi) * tan(delta))  # sunset hour angle [rad]

  # Extraterrestrial radiation for daily periods [MJ.m^(-2).day^(-1)]
  Ra <- (24 * 60 / pi) * Gsc * Dr * (Ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(Ws))
  
  # estimate of the atmospheric transmissivity
  Tt <- Krs * (1 + 2.7 * 10^(-5) * elev) * sqrt(tmx - tmn)
  
  # Solar radiation estimate
  srad <- Ra * Tt
  return(srad)
}

# Vector of atmospheric transmissivity coefficients to test. Goes beyond range recommended by 
cvec <- seq(0.13, 0.20, by = 0.001)  

tic <- Sys.time()
q.SR.coeffs <- as.data.frame(do.call("rbind", (lapply(dir(pattern = ".WTH"), function(x) {

  # Read main table, coordinates, elev, and quinary names (adapt this section if necessary to read from other
  # formats--currently reads in DSSAT WTH files)
  indat <- readtabcols(y = x, pat = "DATE|SRAD|TMAX|TMIN", drop.col = "", sk = 4, sep = "")
  xyz <- as.numeric(scan(x, what = "numeric", skip = 3, nlines = 1)[2:4])
  nms <- unlist(strsplit(scan(x, what = "character", nlines = 1)[4], "/"))

  sr.rmse <- sapply(cvec, function(x) {
    RMSE(indat$SRAD, 
         solRadH(jd = as.numeric(substr(indat[, 1], 3, 5)), lat = xyz[1], elev = xyz[3], tmn = indat$TMIN, 
                 tmx = indat$TMAX, Krs = x))
  })
  out.v <- c(nms, xyz, cvec[which(sr.rmse == min(sr.rmse))], round(min(sr.rmse), 3))
  names(out.v) <- c("QNUM", "QCODE", "lat", "long", "elev", "Krs", "RMSE")
  return(out.v)
}))))
toc <- Sys.time()
toc - tic  # 2.878484 hours to process all quinaries

# Note: 13/4/11 - rechecked results against values from perl calculated WTH files (CSM_driver_devel3.R) for
# quins AAA-C. The same. 

# Change classes of variables
q.SR.coeffs2 <- q.SR.coeffs
q.SR.coeffs2$lat <- round(as.numeric(as.character(q.SR.coeffs2$lat)), 2)
q.SR.coeffs2$long <-round(as.numeric(as.character(q.SR.coeffs2$long)), 2)
q.SR.coeffs2$elev <- as.integer(as.character(q.SR.coeffs2$elev))
q.SR.coeffs2$Krs <-round(as.numeric(as.character(q.SR.coeffs2$Krs)), 3)
q.SR.coeffs2$RMSE <-round(as.numeric(as.character(q.SR.coeffs2$RMSE)), 3)

#q.SR.coeffs2[q.SR.coeffs2$Krs == min(q.SR.coeffs2$Krs), ]  # Min Krs is 0.13, assoc. w/max RMSE 3.275
#q.SR.coeffs2[q.SR.coeffs2$Krs == max(q.SR.coeffs2$Krs), ]  # Max Krs is 0.164
#q.SR.coeffs2[q.SR.coeffs2$RMSE == min(q.SR.coeffs2$RMSE), ]  # Min RMSE is 1.097
#mean(q.SR.coeffs2$Krs)  # Mean Krs is 0.151
#mean(q.SR.coeffs2$RMSE)  # Mean RMSE is 1.81

# Save coefficients dataframe as Rdata object
save(q.SR.coeffs2, 
     file = "~/eMaphepha_ami/Post_doc/Data/Climdata_and_agreements/Quinary_wth/SRad.coeffs.RData")



