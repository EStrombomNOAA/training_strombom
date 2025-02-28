# <function_name> <- function(arg1, arg2, ...) {
#   <code for the function to run>
#   return(<something>)
# }

interior_f <- c(186, 175, 182)
# convert to celsius
interior_c <- (interior_f - 32) * 5/9

exterior_f <- c(75, 78, 80)
exterior_c <- (exterior_f - 32) * 5/9

surface_f <- c(103, 109, 98)
surface_c <- (surface_f - 32) * 5/9


#define function to convert f to c
# the version below is more self-documenting
convert_f_to_c <- function(fahr){
  celsius <-(fahr - 32) * 5/9
  return(celsius)
}

convert_f_to_c(surface_f)


# define function to convert c to f

convert_c_to_f <- function(celsius){
  fahr <- celsius * 9/5 + 32
  return(fahr)
}

convert_c_to_f(surface_c)

# to check it worked
identical(surface_f, convert_c_to_f(surface_c))

# a more complete function
# caller must give temp. if caller doesn't give units, default it F
convert_temps <- function(temp, units = "F"){
  if (!units %in% c("C", "F")){
    stop("Units must be C or F")
  }
    
  if (units == "F"){
    fahr <- temp
    celsius <- convert_f_to_c(fahr)
  } else {
    celsius <- temp
    fahr <- convert_c_to_f(celsius)
  }
  kelvin <- celsius + 273.15
  
  out_df <- data.frame(fahr, celsius, kelvin)
  
  return(out_df)
}

convert_temps(surface_f, units = "F")

library(dplyr)

data.frame(f = surface_f) %>%
  mutate(c = convert_f_to_c(f))

add_hot_or_cold <- function(df, thresh = 70){
  if (!"fahr" %in% names(df)) {
    stop("Data frame must have fahr column!")
  }
  out_df <- df %>% 
    mutate(hotcold = ifelse(fahr > thresh, "hot", "cold"))
  return(out_df)
}

data.frame(fahr = surface_f) %>%
  mutate(c = convert_f_to_c(fahr)) %>%
  add_hot_or_cold(thresh = 105)

