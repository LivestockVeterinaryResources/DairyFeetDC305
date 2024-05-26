# Set Up file for analysis

#Load Packages----

# rio tips comes from https://epirhandbook.com/suggested-packages-1.html

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  janitor,       # tables
  tidyverse,     # data mgmt and viz
  readr,         # import 
  zoo,           # dates
  flextable,      # pretty tables
  #broom,         # model output
  #scales,        # for tables %
  gtsummary,     # tables
  gt, 
  broom,         # tid models
  #rstatix,
  #meantables,
  RColorBrewer,
  survminer,     # survival
  Hmisc,        # for CI
  ggtext,       # dim graph
  ragg,         # dim graph
  knitr,         # tables
  epiR,          # Conf Inf
  survival,
  ggridges       # for ridgeplots
)

# load data----
lame4 <-import("datafiles/lame4.rds")
lameleg <- import("datafiles/lameleg.rds")
mall <- import("datafiles/mall.rds") 
lamecull <- import("datafiles/lamecull.rds")
startdat <- "2022-10-01"
enddat <- "2023-10-01"
startym <- 2022+9/12   #to get 12 months yearmon format year + month#-1/12
endym <- 2023+9/12
start_month_1y <- "Oct 2022"
end_month_1y <- "Sep 2023"
regfarm1 <- "b4"
regfarm2 <- "b1"
regfarm3 <- "b5"
regfarm4 <- "b2"
regfarm5 <- "b3"
regfarm6 <- "b8"
regfarm7 <- "b6"
regfarm8 <- "b7"

# set up filters for farms  
farms <- c(regfarm1, regfarm2, regfarm6, regfarm7, regfarm8)

lame4 <- lame4 |> 
  filter(farm %in% farms)

mall <- mall |> 
  filter(farm %in% farms)

lamecull <- lamecull |> 
  filter(farm %in% farms)

# set up repeated variables
les_variables <- c("lesion", "dd", "footrot", "wld",
                   "soleulcer", "solefract", "hem", "cork", "other", "axial",
                   "toeulcer", "thin", "inf", "noninf", "toe", "injury")
