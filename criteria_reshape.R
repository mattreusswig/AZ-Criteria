## Pivot the criteria table from a wide to long format ##
## 
library(tidyr)
library(stringr)
library(dplyr)

d <- readr::read_csv("DEQSTANDARDSAPPROVED.csv")

d_cols <- names(d)
d_cols <- d_cols[c(1:28)]

d <- d[, d_cols]

w <- pivot_longer(d, 
                  cols = DWS_MAX:PBC_MIN, 
                  names_to = "criteria", 
                  values_to = "VALUE", 
                  values_drop_na = TRUE, 
                  values_ptypes = numeric())

w <- separate(w, criteria, into = c("CRITERIA", NA, NA), sep = "_", remove = FALSE) |>   
  mutate(TYPE1 = str_extract(criteria, "ACUTE|CHRONIC"),
                   TYPE2 = str_extract(criteria, "MIN|MAX")) |>
  select(-criteria)

readr::write_csv(w, "Criteria_Reorganized_2022-11-19.csv")
