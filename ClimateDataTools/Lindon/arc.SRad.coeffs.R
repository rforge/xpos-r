##############################################################################################################
# Title      : arc.SRad.coeffs.R
# Purpose    : Creates output table of best fitting atmospheric transmissivity coefficients for each of 11 
#              ARC AWS stations whose data has been converted to DSSAT WTH format
# Author     : Lyndon Estes
# Draws from : s_rad_function_25032011.R; csm.funcs.R; ldefuncs.R; quin.SRad.coeffs.R 
# Used by    : 
# Notes      : Uses solRadH function to predicted SRad values using Hargreaves approach using a range of 
#              atmospheric transmissivity coefficients, calculates RMSE between each predicted values and 
#              SRad from Quinary Catchment, and produces output table containing the Quinary ID number, its 
#              DSSAT specific identifier, lat, long, and elevation of ARC station, the best fitting
#              atmospheric transmissivity coefficient (Krs), and the RMSE value. 
#              This is done as a follow up to compare results with those found by quin.SRad.coeffs.R, using
#              real SRad observations versus the modeled ones in the Quinary Catchment database.  A caution:
#              the quality of the data produced by the ARC stations may be poor in some cases.  
##############################################################################################################

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

RMSE <- function(obs, pred) {
# Calculates RMSE between two values
# Args: 
#   obs: Vector of observed values
#   pred: Vector of predicted values
# Returns:
# RMSE in units of the observations
  sqrt(mean((obs - pred)^2))
}

# Vector of atmospheric transmissivity coefficients to test. Goes beyond range recommended by 
cvec <- seq(0.10, 0.20, by = 0.001)  # Note I am using a wider range than the ones applied to the quinaries

tic <- Sys.time()
setwd("~/eMaphepha_ami/RSGIS/Post_doc_data/Climate/ARC_AWS/Michelle_Srad_11042011/ARC_WTH/")
a.SR.coeffs <- as.data.frame(do.call("rbind", (lapply(dir(pattern = ".WTH"), function(x) {

  # Read main table, coordinates, elev, and quinary names (adapt this section if necessary to read from other
  # formats--currently reads in DSSAT WTH files)
  cat("AWS = ", x, "\n")
  indat <- readtabcols(y = x, pat = "DATE|SRAD|TMAX|TMIN", drop.col = "", sk = 4, sep = "")
  indat[, 1] <- sprintf("%05i", indat[, 1])  # Adds zero back in front of years < 2010
  indat <- indat[!(indat$TMIN > indat$TMAX | indat$TMAX >= 60 | indat$TMAX <= 0), ]  # Remove some bad values 
  xyz <- as.numeric(scan(x, what = "numeric", skip = 3, nlines = 1)[2:4])
  nms <- unlist(strsplit(scan(x, what = "character", nlines = 1)[4], "/"))

  sr.rmse <- sapply(cvec, function(x) {
    RMSE(indat$SRAD, 
         solRadH(jd = as.numeric(substr(indat[, 1], 3, 5)), lat = xyz[1], elev = xyz[3], tmn = indat$TMIN, 
                 tmx = indat$TMAX, Krs = x))
  })
  out.v <- c(nms, xyz, cvec[which(sr.rmse == min(sr.rmse))], round(min(sr.rmse), 3))
  names(out.v) <- c("SNUM", "SCODE", "lat", "long", "elev", "Krs", "RMSE")
  return(out.v)
}))))
toc <- Sys.time()
toc - tic  # 3.9 seconds

# Change classes of variables
a.SR.coeffs2 <- a.SR.coeffs
a.SR.coeffs2$lat <- round(as.numeric(as.character(a.SR.coeffs2$lat)), 2)
a.SR.coeffs2$long <-round(as.numeric(as.character(a.SR.coeffs2$long)), 2)
a.SR.coeffs2$elev <- as.integer(as.character(a.SR.coeffs2$elev))
a.SR.coeffs2$Krs <-round(as.numeric(as.character(a.SR.coeffs2$Krs)), 3)
a.SR.coeffs2$RMSE <-round(as.numeric(as.character(a.SR.coeffs2$RMSE)), 3)

# Bring in quinary coefficients for comparison
load("~/eMaphepha_ami/Post_doc/Data/Climdata_and_agreements/Quinary_wth/SRad.coeffs.RData")
# Load small table showing quinaries in which ARC stations fall into (by Michelle)
setwd("~/eMaphepha_ami/RSGIS/Post_doc_data/Climate/ARC_AWS/Michelle_Srad_11042011")
arc.v.q <- read.csv("ARC_Stations_vs_Quins_11042011.csv")
a.SR.coeffs2$Station <- as.numeric(sub("ARC", "", a.SR.coeffs2$SNUM))
a.SR.coeff.q <- merge(a.SR.coeffs2, arc.v.q[, c(1, 2, 4)], by = "Station")
a.SR.coeff.q$QNUM <- sprintf("%s%04i", "quin", a.SR.coeff.q$Quin.Number)
qs <- q.SR.coeffs2[q.SR.coeffs2$QNUM %in% a.SR.coeff.q$QNUM, ]
colnames(qs)[3:7] <- paste("q", colnames(qs)[3:7], sep = "") 
a.SR.coeff.q2 <- merge(a.SR.coeff.q, qs, by = "QNUM")
a.SR.coeff.q2 <- a.SR.coeff.q2[, c(2, 4, 10, 5, 13, 6, 14, 7, 15, 8, 16, 9, 17)]

plot(a.SR.coeff.q2$Krs, a.SR.coeff.q2$lat)  # ARC lat versus Krs
plot(a.SR.coeff.q2$Krs, a.SR.coeff.q2$elev)  # ARC elev versus Krs
plot(q.SR.coeffs2$Krs, q.SR.coeffs2$elev)
plot(q.SR.coeffs2$Krs, q.SR.coeffs2$lat)
# Compare quinary Krs to ARC Krs
plot(a.SR.coeff.q2$qKrs, a.SR.coeff.q2$Krs)
krs.lm <- summary(lm(Krs ~ qKrs, a.SR.coeff.q2))
abline(krs.lm)  # General positive relationship between quinary Krs and Arc Krs. 
RMSE(a.SR.coeff.q2$Krs, a.SR.coeff.q2$qKrs)  # RMSE between Krs for ARC and corresponding Quin = 0.018 

# Save coefficients dataframe as Rdata object
save(list = ls(), 
     file = "~/eMaphepha_ami/Post_doc/Data/Climdata_and_agreements/Quinary_wth/a.SRad.coeffs.RData")


