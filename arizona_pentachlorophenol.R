

#######################################################################
#####   Compute ARIZONA A&W Pentachlorophenol Criterion   #############
#######################################################################

# Date: 2022-11-21
# Author: Matthew Reusswig
# Note: Based on ADEQ's 2016 Water Quality Standards.

# Function to compute acute and chronic pentachlorophenol standards.
# pH = numeric value for the sample pH in standard units.
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
calc_pentachlorophenol <- function(pH, type, use) {
  
  '%not_in%' <- Negate('%in%')
  
  # Validate input are string values
  if(!is.character(type) | !is.character(use)) stop("Criteria Type, and use inputs must be strings.")
  
  # Validate input are numeric type
  if(!is.numeric(pH)) stop("pH input must be a numeric type value.")
  
  # Validate only one of each input was used
  if(length(pH) != 1 | length(type) != 1 | length(use) != 1) stop("pH, type, and use inputs must each have a single element input each.")
  
  # Normalize capitalization of input
  type      <- toupper(type)
  use       <- toupper(use)
  
  # Check in input values are valid
  valid_types <- c("CHRONIC", "ACUTE")
  valid_uses <- c("AWC", "AWW", "AWEDW", "AWE")
  
  if(type %not_in% valid_types) stop(paste0("Invalid type input. Type name must by one of: ", paste0(valid_types, collapse =  ", ")))
  if(use %not_in% valid_uses) stop(paste0("Invalid use input. Use name must by one of: ", paste0(valid_uses, collapse =  ", ")))
  
  # make a type_code out of the criteria type and designated use inputs
  type_code <- paste0(type, "_", use)
  
  # Select criteria parameter based on type_code
  a <- switch(type_code,
              "ACUTE_AWC" = 4.83,
              "ACUTE_AWW" = 4.83,
              "ACUTE_AWEDW" = 4.83,
              
              "ACUTE_AWE" = 3.4306,
              
              "CHRONIC_AWC" = 5.29,
              "CHRONIC_AWW" = 5.29,
              "CHRONIC_AWEDW" = 5.29,
              
              "CHRONIC_AWE" = NA)
  
  # Compute standard
  out <- exp(1.005 * pH - a)
  
  return(out)
  
}
