

#####################################################################
#####   Compute ARIZONA Harness-based Metal Criterion   #############
#####################################################################

# Date: 2022-11-19
# Author: M. Reusswig
# Note: 

# Function to ingest string codes, check they are valid, and return a concatenated
# output of the codes if they are.
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
make_type <- function(parameter, type, use) {
  
  '%not_in%' <- Negate('%in%')
  
  # Validate input are string values
  if(!is.character(parameter) | !is.character(type) | !is.character(use)) stop("Parameter, type, and use inputs must be strings.")
  
  # Validate only one of each input was used
  if(length(parameter) != 1 | length(type) != 1 | length(use) != 1) stop("Parameter, type, and use inputs must each have a single element input.")
  
  # Normalize capitalization of input
  parameter <- toupper(parameter)
  type      <- toupper(type)
  use       <- toupper(use)
  
  # Check in input values are valid
  valid_parameters <- c("CADMIUM",
                        "CHROMIUM(III)",
                        "COPPER",
                        "LEAD",
                        "NICKEL",
                        "SILVER",
                        "ZINC")
  valid_types <- c("CHRONIC", "ACUTE")
  valid_uses <- c("AWC", "AWW", "AWEDW", "AWE")
  
  if(parameter %not_in% valid_parameters) stop(paste0("Invalid parameter input. Parameter name must by one of: ", paste0(valid_parameters, collapse =  ", ")))
  if(type %not_in% valid_types) stop(paste0("Invalid type input. Type name must by one of: ", paste0(valid_types, collapse =  ", ")))
  if(use %not_in% valid_uses) stop(paste0("Invalid use input. Use name must by one of: ", paste0(valid_uses, collapse =  ", ")))
  
  # Concatenate each input into a combined code
  out <- paste0(parameter, "_", type, "_", use)
  
  return(out)
  
  }

# Function to compute the metal's conversion factor.
# hardness = numeric type, hardness value in mg/L CaCO3
# type_code = string type value taken from make_type function
calc_cf <- function(hardness, type_code) {
  
  b = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid Conversion Factor
             
             "CADMIUM_ACUTE_AWC"   = 1.136672,
             "CADMIUM_ACUTE_AWW"   = 1.136672,
             "CADMIUM_ACUTE_AWEDW" = 1.136672,
             "CADMIUM_ACUTE_AWE"   = 1.136672,
             
             "CADMIUM_CHRONIC_AWC"   = 1.101672,
             "CADMIUM_CHRONIC_AWW"   = 1.101672,
             "CADMIUM_CHRONIC_AWEDW" = 1.101672,
             "CADMIUM_CHRONIC_AWE"   = 0,
             
             "CHROMIUM(III)_ACUTE_AWC"   = 0.316,
             "CHROMIUM(III)_ACUTE_AWW"   = 0.316,
             "CHROMIUM(III)_ACUTE_AWEDW" = 0.316,
             "CHROMIUM(III)_ACUTE_AWE"   = 0.316,
             
             "CHROMIUM(III)_CHRONIC_AWC"   = 0.860,
             "CHROMIUM(III)_CHRONIC_AWW"   = 0.860,
             "CHROMIUM(III)_CHRONIC_AWEDW" = 0.860,
             "CHROMIUM(III)_CHRONIC_AWE"   = 0,
             
             "COPPER_ACUTE_AWC"   = 0.960,
             "COPPER_ACUTE_AWW"   = 0.960,
             "COPPER_ACUTE_AWEDW" = 0.960,
             "COPPER_ACUTE_AWE"   = 0.960,
             
             "COPPER_CHRONIC_AWC"   = 0.960,
             "COPPER_CHRONIC_AWW"   = 0.960,
             "COPPER_CHRONIC_AWEDW" = 0.960,
             "COPPER_CHRONIC_AWE"   = 0,
             
             "LEAD_ACUTE_AWC"   = 1.46203,
             "LEAD_ACUTE_AWW"   = 1.46203,
             "LEAD_ACUTE_AWEDW" = 1.46203,
             "LEAD_ACUTE_AWE"   = 1.46203,
             
             "LEAD_CHRONIC_AWC"   = 1.46203,
             "LEAD_CHRONIC_AWW"   = 1.46203,
             "LEAD_CHRONIC_AWEDW" = 1.46203,
             "LEAD_CHRONIC_AWE"   = 0,
             
             "NICKEL_ACUTE_AWC"   = 0.998,
             "NICKEL_ACUTE_AWW"   = 0.998,
             "NICKEL_ACUTE_AWEDW" = 0.998,
             "NICKEL_ACUTE_AWE"   = 0.998,
             
             "NICKEL_CHRONIC_AWC"   = 0.997,
             "NICKEL_CHRONIC_AWW"   = 0.997,
             "NICKEL_CHRONIC_AWEDW" = 0.997,
             "NICKEL_CHRONIC_AWE"   = 0,
             
             "SILVER_ACUTE_AWC"   = 0.850,
             "SILVER_ACUTE_AWW"   = 0.850,
             "SILVER_ACUTE_AWEDW" = 0.850,
             "SILVER_ACUTE_AWE"   = 0.850,
             
             "SILVER_CHRONIC_AWC"   = 0,
             "SILVER_CHRONIC_AWW"   = 0,
             "SILVER_CHRONIC_AWEDW" = 0,
             "SILVER_CHRONIC_AWE"   = 0,
             
             "ZINC_ACUTE_AWC"   = 0.978,
             "ZINC_ACUTE_AWW"   = 0.978,
             "ZINC_ACUTE_AWEDW" = 0.978,
             "ZINC_ACUTE_AWE"   = 0.978,
             
             "ZINC_CHRONIC_AWC"   = 0.978,
             "ZINC_CHRONIC_AWW"   = 0.978,
             "ZINC_CHRONIC_AWEDW" = 0.978,
             "ZINC_CHRONIC_AWE"   = 0
             )
  
  m = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid Conversion Factor, or for which there is no slope (m) term.
             
             "CADMIUM_ACUTE_AWC"   = -0.041838,
             "CADMIUM_ACUTE_AWW"   = -0.041838,
             "CADMIUM_ACUTE_AWEDW" = -0.041838,
             "CADMIUM_ACUTE_AWE"   = -0.041838,
             
             "CADMIUM_CHRONIC_AWC"   = -0.041838,
             "CADMIUM_CHRONIC_AWW"   = -0.041838,
             "CADMIUM_CHRONIC_AWEDW" = -0.041838,
             "CADMIUM_CHRONIC_AWE"   = 0,
             
             "CHROMIUM(III)_ACUTE_AWC"   = 0,
             "CHROMIUM(III)_ACUTE_AWW"   = 0,
             "CHROMIUM(III)_ACUTE_AWEDW" = 0,
             "CHROMIUM(III)_ACUTE_AWE"   = 0,
             
             "CHROMIUM(III)_CHRONIC_AWC"   = 0,
             "CHROMIUM(III)_CHRONIC_AWW"   = 0,
             "CHROMIUM(III)_CHRONIC_AWEDW" = 0,
             "CHROMIUM(III)_CHRONIC_AWE"   = 0,
             
             "COPPER_ACUTE_AWC"   = 0,
             "COPPER_ACUTE_AWW"   = 0,
             "COPPER_ACUTE_AWEDW" = 0,
             "COPPER_ACUTE_AWE"   = 0,
             
             "COPPER_CHRONIC_AWC"   = 0,
             "COPPER_CHRONIC_AWW"   = 0,
             "COPPER_CHRONIC_AWEDW" = 0,
             "COPPER_CHRONIC_AWE"   = 0,
             
             "LEAD_ACUTE_AWC"   = -0.145712,
             "LEAD_ACUTE_AWW"   = -0.145712,
             "LEAD_ACUTE_AWEDW" = -0.145712,
             "LEAD_ACUTE_AWE"   = -0.145712,
             
             "LEAD_CHRONIC_AWC"   = -0.145712,
             "LEAD_CHRONIC_AWW"   = -0.145712,
             "LEAD_CHRONIC_AWEDW" = -0.145712,
             "LEAD_CHRONIC_AWE"   = 0,
             
             "NICKEL_ACUTE_AWC"   = 0,
             "NICKEL_ACUTE_AWW"   = 0,
             "NICKEL_ACUTE_AWEDW" = 0,
             "NICKEL_ACUTE_AWE"   = 0,
             
             "NICKEL_CHRONIC_AWC"   = 0,
             "NICKEL_CHRONIC_AWW"   = 0,
             "NICKEL_CHRONIC_AWEDW" = 0,
             "NICKEL_CHRONIC_AWE"   = 0,
             
             "SILVER_ACUTE_AWC"   = 0,
             "SILVER_ACUTE_AWW"   = 0,
             "SILVER_ACUTE_AWEDW" = 0,
             "SILVER_ACUTE_AWE"   = 0,
             
             "SILVER_CHRONIC_AWC"   = 0,
             "SILVER_CHRONIC_AWW"   = 0,
             "SILVER_CHRONIC_AWEDW" = 0,
             "SILVER_CHRONIC_AWE"   = 0,
             
             "ZINC_ACUTE_AWC"   = 0,
             "ZINC_ACUTE_AWW"   = 0,
             "ZINC_ACUTE_AWEDW" = 0,
             "ZINC_ACUTE_AWE"   = 0,
             
             "ZINC_CHRONIC_AWC"   = 0,
             "ZINC_CHRONIC_AWW"   = 0,
             "ZINC_CHRONIC_AWEDW" = 0,
             "ZINC_CHRONIC_AWE"   = 0
             )
  
  cf = b + log(hardness) * m
  
  return(cf)
}

# Function to compute the metal's total recoverable criterion in ug/L.
# hardness = numeric type, hardness value in mg/L CaCO3
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
calc_total_metal_criterion <- function(hardness, parameter, type, use) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, use = use)
  
  # Use type_code to get the slope and intercept of the total recoverable metal criterion.
  b = switch(type_code, 
             # -999 is used for type_code values for which there is not a 
             # valid dissolved criterion
             
             "CADMIUM_ACUTE_AWC"   = -3.924,
             "CADMIUM_ACUTE_AWW"   = -3.6867,
             "CADMIUM_ACUTE_AWEDW" = -3.6867,
             "CADMIUM_ACUTE_AWE"   = -0.9691,
             
             "CADMIUM_CHRONIC_AWC"   = -4.719,
             "CADMIUM_CHRONIC_AWW"   = -2.715,
             "CADMIUM_CHRONIC_AWEDW" = -2.715,
             "CADMIUM_CHRONIC_AWE"   = -999,
             
             "CHROMIUM(III)_ACUTE_AWC"   = 3.7256,
             "CHROMIUM(III)_ACUTE_AWW"   = 3.7256,
             "CHROMIUM(III)_ACUTE_AWEDW" = 3.7256,
             "CHROMIUM(III)_ACUTE_AWE"   = 4.936,
             
             "CHROMIUM(III)_CHRONIC_AWC"   = 0.6848,
             "CHROMIUM(III)_CHRONIC_AWW"   = 0.6848,
             "CHROMIUM(III)_CHRONIC_AWEDW" = 0.6848,
             "CHROMIUM(III)_CHRONIC_AWE"   = -999,
             
             "COPPER_ACUTE_AWC"   = -1.702,
             "COPPER_ACUTE_AWW"   = -1.702,
             "COPPER_ACUTE_AWEDW" = -1.702,
             "COPPER_ACUTE_AWE"   = -1.151,
             
             "COPPER_CHRONIC_AWC"   = -1.702,
             "COPPER_CHRONIC_AWW"   = -1.702,
             "COPPER_CHRONIC_AWEDW" = -1.702,
             "COPPER_CHRONIC_AWE"   = -999,
             
             "LEAD_ACUTE_AWC"   = -1.46,
             "LEAD_ACUTE_AWW"   = -1.46,
             "LEAD_ACUTE_AWEDW" = -1.46,
             "LEAD_ACUTE_AWE"   = -0.713,
             
             "LEAD_CHRONIC_AWC"   = -4.705,
             "LEAD_CHRONIC_AWW"   = -4.705,
             "LEAD_CHRONIC_AWEDW" = -4.705,
             "LEAD_CHRONIC_AWE"   = -999,
             
             "NICKEL_ACUTE_AWC"   = 2.255,
             "NICKEL_ACUTE_AWW"   = 2.255,
             "NICKEL_ACUTE_AWEDW" = 2.255,
             "NICKEL_ACUTE_AWE"   = 4.438,
             
             "NICKEL_CHRONIC_AWC"   = 0.058,
             "NICKEL_CHRONIC_AWW"   = 0.058,
             "NICKEL_CHRONIC_AWEDW" = 0.058,
             "NICKEL_CHRONIC_AWE"   = -999,
             
             "SILVER_ACUTE_AWC"   = -6.59,
             "SILVER_ACUTE_AWW"   = -6.59,
             "SILVER_ACUTE_AWEDW" = -6.59,
             "SILVER_ACUTE_AWE"   = -6.59,
             
             "SILVER_CHRONIC_AWC"   = -999,
             "SILVER_CHRONIC_AWW"   = -999,
             "SILVER_CHRONIC_AWEDW" = -999,
             "SILVER_CHRONIC_AWE"   = -999,
             
             "ZINC_ACUTE_AWC"   = 0.884,
             "ZINC_ACUTE_AWW"   = 0.884,
             "ZINC_ACUTE_AWEDW" = 0.884,
             "ZINC_ACUTE_AWE"   = 3.134,
             
             "ZINC_CHRONIC_AWC"   = 0.884,
             "ZINC_CHRONIC_AWW"   = 0.884,
             "ZINC_CHRONIC_AWEDW" = 0.884,
             "ZINC_CHRONIC_AWE"   = -999
  )
  
  m = switch(type_code, 
             # 0 is used for type_code values for which there is not a 
             # valid dissolved criterion.
             
             "CADMIUM_ACUTE_AWC"   = 1.0166,
             "CADMIUM_ACUTE_AWW"   = 1.128,
             "CADMIUM_ACUTE_AWEDW" = 1.128,
             "CADMIUM_ACUTE_AWE"   = 1.128,
             
             "CADMIUM_CHRONIC_AWC"   = 0.7409,
             "CADMIUM_CHRONIC_AWW"   = 0.7852,
             "CADMIUM_CHRONIC_AWEDW" = 0.7852,
             "CADMIUM_CHRONIC_AWE"   = 0,
             
             "CHROMIUM(III)_ACUTE_AWC"   = 0.819,
             "CHROMIUM(III)_ACUTE_AWW"   = 0.819,
             "CHROMIUM(III)_ACUTE_AWEDW" = 0.819,
             "CHROMIUM(III)_ACUTE_AWE"   = 0.819,
             
             "CHROMIUM(III)_CHRONIC_AWC"   = 0.819,
             "CHROMIUM(III)_CHRONIC_AWW"   = 0.819,
             "CHROMIUM(III)_CHRONIC_AWEDW" = 0.819,
             "CHROMIUM(III)_CHRONIC_AWE"   = 0,
             
             "COPPER_ACUTE_AWC"   = 0.9422,
             "COPPER_ACUTE_AWW"   = 0.9422,
             "COPPER_ACUTE_AWEDW" = 0.9422,
             "COPPER_ACUTE_AWE"   = 0.9422,
             
             "COPPER_CHRONIC_AWC"   = 0.8545,
             "COPPER_CHRONIC_AWW"   = 0.8545,
             "COPPER_CHRONIC_AWEDW" = 0.8545,
             "COPPER_CHRONIC_AWE"   = 0,
             
             "LEAD_ACUTE_AWC"   = 1.273,
             "LEAD_ACUTE_AWW"   = 1.273,
             "LEAD_ACUTE_AWEDW" = 1.273,
             "LEAD_ACUTE_AWE"   = 1.273,
             
             "LEAD_CHRONIC_AWC"   = 1.273,
             "LEAD_CHRONIC_AWW"   = 1.273,
             "LEAD_CHRONIC_AWEDW" = 1.273,
             "LEAD_CHRONIC_AWE"   = 0,
             
             "NICKEL_ACUTE_AWC"   = 0.846,
             "NICKEL_ACUTE_AWW"   = 0.846,
             "NICKEL_ACUTE_AWEDW" = 0.846,
             "NICKEL_ACUTE_AWE"   = 0.846,
             
             "NICKEL_CHRONIC_AWC"   = 0.846,
             "NICKEL_CHRONIC_AWW"   = 0.846,
             "NICKEL_CHRONIC_AWEDW" = 0.846,
             "NICKEL_CHRONIC_AWE"   = 0,
             
             "SILVER_ACUTE_AWC"   = 1.72,
             "SILVER_ACUTE_AWW"   = 1.72,
             "SILVER_ACUTE_AWEDW" = 1.72,
             "SILVER_ACUTE_AWE"   = 1.72,
             
             "SILVER_CHRONIC_AWC"   = 0,
             "SILVER_CHRONIC_AWW"   = 0,
             "SILVER_CHRONIC_AWEDW" = 0,
             "SILVER_CHRONIC_AWE"   = 0,
             
             "ZINC_ACUTE_AWC"   = 0.8473,
             "ZINC_ACUTE_AWW"   = 0.8473,
             "ZINC_ACUTE_AWEDW" = 0.8473,
             "ZINC_ACUTE_AWE"   = 0.8473,
             
             "ZINC_CHRONIC_AWC"   = 0.8473,
             "ZINC_CHRONIC_AWW"   = 0.8473,
             "ZINC_CHRONIC_AWEDW" = 0.8473,
             "ZINC_CHRONIC_AWE"   = 0
  )
  
  total_crit = exp(b + log(hardness) * m)
  
  total_crit <- ifelse(total_crit == 0, NA, total_crit)
  
  return(total_crit)
}


# Function to compute the metal's dissolved criterion.
# hardness = numeric type, hardness value in mg/L CaCO3
# parameter = string giving the name of the metal
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
calc_dissolved_metal_criterion <- function(hardness, parameter, type, use) {
  
  # Validate input and create a type code to feed cf and total metal criterion functions.
  type_code <- make_type(parameter = parameter, type = type, use = use)
  
  # Calculate the conversion factor
  cf <- calc_cf(hardness = hardness, type_code = type_code)
  
  # Calculate the total recoverable criterion in units of ug/L 
  total_crit <- calc_total_metal_criterion(harness = hardness, type_code = type_code)
  
  # Calculate the dissolved criterion in units of ug/L
  dissolved_crit <- total_crit * cf
  
  return(dissolved_crit)
  
}