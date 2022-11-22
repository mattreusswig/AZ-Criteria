

#############################################################
#####   Compute ARIZONA A&W Ammonia Criterion   #############
#############################################################

# Date: 2022-11-21
# Author: Matthew Reusswig
# Note: Based on ADEQ's 2016 Water Quality Standards.

# Function to compute acute and chronic ammonia standards.
# pH = numeric value for the sample pH in standard units.
# temperature = numeric value giving the sample temperature in degrees celcius. While the acute criteria are not a function of temperature, a numeric value must still be included in the input; however, it does not matter what value.
# type = string giving the type of criteria (acute or chronic)
# use = string giving the Arizona designated use (AWC, AWWW, AWEDW, AWE)
calc_ammonia <- function(pH, temperature, type, use) {
  
  
  '%not_in%' <- Negate('%in%')
  
  # Validate input are string values
  if(!is.character(type) | !is.character(use)) stop("Criteria Type, and use inputs must be strings.")
  
  # Validate input are numeric type
  if(!is.numeric(pH) | !is.numeric(temperature)) stop("pH and temperature inputs must be numeric type values.")
  
  # Validate only one of each input was used
  if(length(pH) != 1 |length(temperature) != 1 | length(type) != 1 | length(use) != 1) stop("pH, temperature, type, and use inputs must each have a single element input each.")
  
  # Normalize capitalization of input
  type      <- toupper(type)
  use       <- toupper(use)
  
  # Check in input values are valid
  valid_types <- c("CHRONIC", "ACUTE")
  valid_uses <- c("AWC", "AWW", "AWEDW", "AWE")
  
  if(type %not_in% valid_types) stop(paste0("Invalid type input. Type name must by one of: ", paste0(valid_types, collapse =  ", ")))
  if(use %not_in% valid_uses) stop(paste0("Invalid use input. Use name must by one of: ", paste0(valid_uses, collapse =  ", ")))
  
  out <- if(type == "CHRONIC" & (use %in% c("AWC", "AWW", "AWEDW"))) {
    
    # Compute A&W cold, warm, and EDW CCC
    ( (0.0577 / (1 + 10^(7.688 - pH))) + (2.487 / (1 + 10^(pH - 7.688))) ) * 
      min(2.85, 1.45 * 10^(0.028*(25-temperature)))
    
  } else if (type == "ACUTE" & use == "AWC") {
    
    # Compute A&W cold CMC
    ( (0.275 / (1 + 10^(7.204 - pH))) + (39.0 / (1 + 10^(pH - 7.204))) )  
    
  } else if (type == "ACUTE" & (use %in% c("AWW", "AWEDW"))) {
    
    # Compute A&W warm and EDW CMC
    CMC_w = ( (0.411 / (1 + 10^(7.204 - pH))) + (58.4 / (1 + 10^(pH - 7.204))) )
    
  } else if (use == "AWE") {
    
    # There are no ephemeral standards
    NA  
    
  } else { stop("Something went wrong if the if/else statement.")}
  
  return(out)
  
}
