# Import and Manage Farm Lameness data

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
  #broom,         # model output
  #scales,        # for tables %
  #  gtsummmary,
  #rstatix,
  #meantables,
  knitr,         # tables
  #epiR,          # Sens/Spec
  skimr          # quick summaries     
)

# setup variables used throughout----
farm1_name <- "Template" # enter Farm name for reports
filename1 <- "temp" # enter name used for farm in DC export

# start month 5 years ago 
startmon <- "Jan 2018"

# for variables defined below use last full month of data
endmon <- "Dec 2023" 
start_month_1y <- "Jan 2023"
end_month_1y <- "Dec 2023"
censorend <- c("12-31-23")

# trim date item name in dairycomp
ftdat_item <- "ftdat" 
breed_item <- "cbrd"
fresh_date_item <- "fdat"
birth_date_item <- "bdat"

# timing of mid lactation trims
# splits DIM of trimming in categories 
# for early enter the DIM that mid lactation trim start
# for mid enter the DIM that mid lacations trims are supposed to end
early <- 115
mid <- 131

# Import files----
# lame and dry files
import_lame <- function(filename, farm_name) {
  data <- import(paste0("sourcedata/",
                        filename, "_lame.csv"),
                 fill = TRUE) |>
    clean_names() |>
    select(-c(technician, v14)) |>
    mutate(farm = farm_name)
  return(data)
}

farm1_lame <- import_lame(filename1, farm1_name)

# import dry data ####
import_dry <- function(filename, farm_name) {
  read_csv(paste0("sourcedata/", filename, "_dry_cull.csv"), 
           show_col_types = FALSE) |>
    clean_names() |>
    # change the x16 as needed might be 17 if included FTREM
    select(-c(protocols, technician, x14)) |>
    mutate(farm = farm_name,
           # needed as some columns shifted due to commas in rem feild
           dim = as.numeric(dim))
}

farm1_dry <- import_dry(filename1, farm1_name)

# merge farms
lame <- farm1_lame
dry <- farm1_dry

# remove imported data
rm(farm1_lame, farm1_dry)

# import denominator data 

# create function to import until month column has = signs
import_until_condition <- function(file, stop_sequence) {
  lines <- readLines(file)
  rows_to_import <- numeric()
  
  for (i in 1:length(lines)) {
    if (grepl(stop_sequence, lines[i])) {
      rows_to_import <- 1:(i - 2)
      break
    }
  }
  
  imported_data <- read.csv(file, header = TRUE, 
                            nrows = length(rows_to_import))
  return(imported_data)
}

# function to repeat naming
import_and_transform <- function(file, farm_name, stop_sequence, column_name) {
  import_until_condition(file, stop_sequence) |> 
    clean_names() %>% 
    mutate(farm = farm_name,
           ftyearmon = my(dates),
           ftyearmon = as.yearmon(ftyearmon),
           !!sym(column_name) := milking) %>% 
    select(farm, ftyearmon, !!sym(column_name))
}

# importing data 
# farm 1
farm1_mall <- import_and_transform(paste0("sourcedata/", 
                                          filename1, "_all.csv"),
                                   farm1_name, "========", "allmilking")
farm1_m1 <- import_and_transform(paste0("sourcedata/",
                                        filename1, "_lact1.csv"),
                                 farm1_name, "========", "l1milking")
farm1_m2 <- import_and_transform(paste0("sourcedata/",
                                        filename1, "_lact2.csv"),
                                 farm1_name, "========", "l2milking")
farm1_m3 <- import_and_transform(paste0("sourcedata/", filename1,
                                        "_lact3.csv"), farm1_name,
                                 "========", "l3milking")
farm1_m4p <- import_and_transform(paste0("sourcedata/", filename1,
                                         "_lact4p.csv"), farm1_name,
                                  "========", "l4pmilking")

# combine denominator files

mall <- farm1_mall %>% 
  left_join(farm1_m1,  by = c("farm", "ftyearmon")) %>% 
  left_join(farm1_m2,  by = c("farm", "ftyearmon")) %>%
  left_join(farm1_m3,  by = c("farm", "ftyearmon")) |> 
  left_join(farm1_m4p,  by = c("farm", "ftyearmon"))

rm(farm1_m1, farm1_m2, farm1_m3, farm1_m4p)

export(mall, "datafiles/mall.rds")
rm(farm1_mall)

# Data Management:lame----
# change lame file names
lame <- lame %>% 
  rename(birth = birth_date_item,
         frsh = fresh_date_item,
         breed = breed_item) |> 
  mutate(cowid = id,
         ftdim = dim,
         # quiet stops warnings
         birth = mdy(birth, quiet = TRUE),
         frsh = mdy(frsh, quiet = TRUE),
         ftdat = mdy(date),
         ftmon = month(ftdat),
         ftyear = year(ftdat),
         ftyearmon = as.yearmon(ftdat)
         ) %>%
  # filter out adults and goes back 60 months
  filter(frsh != is.na(frsh)) %>% 
  filter(ftyearmon >= startmon & ftyearmon <= endmon) %>% 
  select(-c(id, dim, date))

#recode lesions info----
# create functions to avoid repeating code
str_contains <- function(string, pattern, ignore_case=TRUE){
  str_detect(string, regex(pattern, ignore_case=ignore_case))
  }

lame <- lame |> 
  mutate(event = str_trim(event), # removes stupid spaces from DC
         trimonly = (str_contains(event, "Footrim") & 
                       str_contains(remark, "TRM") |
                       str_contains(protocols, "Trim Only") |
                       str_contains(remark, "None") |
                       str_contains(remark, "NoFT") |
                       str_contains(remark, "OK") |
                       str_contains(protocols, "Jose") | # due to funny II in remark
                       str_contains(protocols, "Nacho") | # due to funny II in remark
                       str_contains(remark, "TRIM")|
                       str_contains(remark, "main")
         ) & !str_contains(remark, "inj"),
         injury = str_contains(remark, "inj") |
           str_contains(remark, "NoFT") |
           str_contains(protocols, "Upperleg")|
           str_contains(protocols,"inj"),
         dd = (str_contains(protocols, "Dig")| 
           str_contains(remark, "HW")|
           str_contains(protocols, "Dermatitis")|
           str_contains(protocols, "Hairy") | 
           str_contains(protocols, "Wart") | 
           str_contains(remark, "Wart") |
             str_contains(remark, "HAIRY W")|
           str_contains(remark, "Dig")
         )& !str_contains(remark, "TD") & 
           !str_contains(remark, "NEED") &
           !str_contains(remark, "DOWN") &  
           !str_contains(remark, "DEX"),
         footrot = str_contains(protocols, "Foot") & 
           str_contains(protocols, "rot") |
           str_contains(remark, "rot") |
           str_contains(protocols, "Footrot")|
           str_contains(remark, "XNLF") |
#           str_contains(remark, "pen") |
           str_contains(remark, "FOOTROT")|
           str_contains(remark, "FTROT"),
         wld = (str_contains(protocols, "White")  | 
           str_contains(remark, "WTLN") |
             str_contains(remark, "white") |
           str_contains(protocols, "WL L"))
         & !str_contains(protocols, "Whitelinein") & # removes axial wall cracks
           !str_contains(remark, "WART") &
           !str_contains(remark, "HAIRY") &
           !str_contains(remark, "ABS") &
          !str_contains(remark, "DOWN") &  
           !str_contains(remark, "OVER") &
           !str_contains(remark, "WRP") &
           !str_contains(remark, "WRAP") &
           !str_contains(remark, "HW"), 
         hem = str_contains(protocols, "Hem"),
         soleulcer = (str_contains(protocols, "Sole Ulcer")|
                        str_contains(protocols, "Abscess") |
                        str_contains(remark, "ULCR") |
                        str_contains(remark, "ULC") |
                        str_contains(remark, "ULCER") |
                        str_contains(remark, "ABS")|
                        str_contains(protocols, "laminitis")|
           str_contains(protocols, "Ulcer")
           ) & !str_contains(protocols, "Toe") & # removes toe ulcer cows
           !str_contains(remark, "DERMA") &
           !str_contains(remark, "Hairy") &
           !str_contains(remark, "HEAL") &
           !str_contains(remark, "Z") &
           !str_contains(remark, "WART") &
           !str_contains(remark, "MORTELAR") &
           !str_contains(remark, "DEX") &
           !str_contains(remark, "BAN") &
           !str_contains(remark, "MAIN"), # removes trim cows
         solefract = str_contains(protocols, "Fracture"),
         toeulcer = str_contains(protocols, "Toe Ulcer"),
         thin = str_contains(protocols, "Thin") |
           str_contains(remark, "Z"),
         cork = str_contains(protocols, "Cork"),
         axial = str_contains(protocols, "WhiteLineIn"),
         # also include cork in other
         other = str_contains(protocols, "Other") | 
           # for farms that enter Axial fissures 
           str_contains(protocols, "WhiteLineIn") |
           str_contains(remark, "othr") |
            str_contains(remark, "block") |
           str_contains(remark, "Beef") |
           str_contains(protocols,"Cork"),
         toe = str_contains(protocols, "Toe")|
           toeulcer | thin,
         inf = dd | footrot,
         noninf = soleulcer | wld | solefract | hem) |>
  mutate(across(where(is_logical), function(x) { +x }))
         
# delete footrim events when both a trim and lame entered on same day
lame <- lame %>% 
  group_by(farm, cowid, ftdat) %>% 
  slice_min(trimonly) %>% 
  ungroup() 

# sites----
# to get site of lesions (for farms with sites)
lame <- lame %>% 
   mutate(site = case_when(pen < 70 ~ "Site 1",
                    pen > 69 ~ "Site 2"),
          site = as_factor(site)
   )

# create leg variable (move to below trimonly creation so it only does lames)
lameleg <- lame %>% 
  # only for lame cows and not injury as those are a mess
  filter(trimonly == 0 & injury == 0) %>% 
  select(farm, cowid, ftdat, event,remark, dd, footrot, site,
         soleulcer, solefract, wld, hem,
         injury, other, cork, toe, thin, inf, noninf, toeulcer, axial ) %>% 
  mutate(leg = case_when(str_detect(remark, "RF") ~ "RF",
                         str_detect(remark, "Q2") ~ "RF",
                         str_detect(remark, "LF") ~ "LF",
                         str_detect(remark, "Q1") ~ "LF",
                         str_detect(remark, "LH") ~ "LH",
                         str_detect(remark, "LB") ~ "LH",
                         str_detect(remark, "LR") ~ "LH",
                         str_detect(remark, "Q4") ~ "LH",
                         str_detect(remark, "RH") ~ "RH",
                         str_detect(remark, "RB") ~ "RH",
                         str_detect(remark, "RR") ~ "RH",
                         str_detect(remark, "Q3") ~ "RH",
                         str_detect(remark, "RRLR") ~ "RE",
                         str_detect(remark, "BRBL") ~ "RE",
                         str_detect(remark, "LHRH") ~ "RE",
                         ),
         leg = as_factor(leg)
         )
## need to get cow with multiple lesion on 1 day into 1 row 
# FOR FUTURE LOOK to see if can use long data as I think for graph 
# commands might be easier and less code and easier to filter
lame2 <- lame %>% 
  group_by(farm, cowid, ftdat) %>% 
  summarise(across(.cols = c(dd, footrot, wld, soleulcer,injury, 
                             cork, other, hem,
                             solefract, toeulcer, thin,
                             inf, noninf, toe, axial), max)
  ) %>% 
  ungroup()

# rejoin above with 
lame3 <- lame %>%
  # pick only variables of interest 
  select(farm, cowid, birth, frsh, ftmon, ftyear, ftyearmon, lact, 
         ftdat, ftdim, site,
         breed, trimonly) %>% 
  distinct(farm, cowid, ftdat, .keep_all = TRUE) %>%  # get's rid of duplicate ftdats 
  left_join(lame2) %>% 
  # count lxtrimmed 
  group_by(cowid) %>% 
  # calculate times trimmed in data set
  mutate(totxtrimmed = dense_rank(ftdat)) %>% 
  arrange(totxtrimmed) %>%
  mutate(prevftdat = ymd(lag(ftdat, n=1)),
         # time dif for any trim type
         diffprevftdat = ftdat-prevftdat,
         time2nextftdat = lead(diffprevftdat, n=1),
         # determine if trim (1) or lame trim
         prevtrimtype = lag(trimonly, n=1),
         # calc for lame cows time from last trim if last was a trim
         timefromtrim = if_else(trimonly == 0 & prevtrimtype == 1,
                                ftdat-prevftdat, NA),
         time2nextlame = lead(timefromtrim, n=1))%>% 
  ungroup()

#saving interim files in case of crash----
export(dry, "datafiles/dry.rds")
export(lame, "datafiles/lame.rds")
export(lame2, "datafiles/lame2.rds")
export(lame3, "datafiles/lame3.rds")
export(lameleg, "datafiles/lameleg.rds")

# create variables for graphs
# count xlame by lactation and by life, no farm in group as cows go between farms
lame4 <- lame3 %>%
  filter(trimonly == 0) %>% 
  group_by(cowid) %>% 
  arrange(ftdat) %>% 
  mutate(lifexlame = as.numeric(dense_rank(ftdat))) %>%
  # calc time from last lame
  mutate(prevlamedat = lag(ftdat,n = 1),
         diffprevlam = ftdat - prevlamedat) %>%
  ungroup() %>% 
  group_by(cowid, lact) %>% 
  # count x lame in lact
  mutate(lactxlame = as.numeric(dense_rank(ftdat))) %>%  
  ungroup() %>% 
  #count x lesions
  group_by(cowid, dd) %>% 
  mutate(lifexdd = if_else(dd == 1, 
                           as.numeric(dense_rank(ftdat)), NA)) %>%  # needs to be NA not 0 so fill works below
  ungroup() %>% 
  group_by(cowid, soleulcer) %>% 
  mutate(lifexulc = if_else(soleulcer == 1, 
                            as.numeric(dense_rank(ftdat)), NA))%>%
  ungroup() %>% 
  group_by(cowid, wld) %>% 
  mutate(lifexwld = if_else(wld == 1,
                            as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>% 
  group_by(cowid, thin) %>% 
  mutate(lifexthin = if_else(thin == 1,
                            as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>% 
  group_by(cowid, toeulcer) %>% 
  mutate(lifextoe = if_else(toeulcer == 1,
                            as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>% 
  group_by(cowid, footrot) %>% 
  mutate(lifexfr = if_else(footrot == 1,
                           as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>% 
  group_by(cowid, solefract) %>% 
  mutate(lifexfract = if_else(solefract == 1,
                              as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>% 
  group_by(cowid, cork) %>% 
  mutate(lifexcork = if_else(cork == 1,
                             as.numeric(dense_rank(ftdat)), NA)) %>%
  ungroup() %>% 
  group_by(cowid, axial) %>% 
  mutate(lifexaxial = if_else(axial == 1,
                               as.numeric(dense_rank(ftdat)), NA)) |> 
  ungroup() %>% 
  group_by(cowid, hem) %>% 
  mutate(lifexhem = if_else(hem == 1,
                             as.numeric(dense_rank(ftdat)), NA)) %>%
  ungroup() %>% 
  group_by(cowid, other) %>% 
  mutate(lifexother = if_else(other == 1,
                             as.numeric(dense_rank(ftdat)), NA)) %>%
  ungroup() |> 
  group_by(cowid, inf) %>% 
  mutate(lifexinf = if_else(inf == 1,
                            as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>% 
  group_by(cowid, noninf) %>% 
  mutate(lifexnoninf = if_else(noninf == 1,
                            as.numeric(dense_rank(ftdat)), NA)) %>%
  ungroup() %>% 
  group_by(cowid, injury) %>% 
  mutate(lifexinj = if_else(injury == 1,
                               as.numeric(dense_rank(ftdat)), NA)) |> 
  ungroup() %>% 
  group_by(cowid, toe) %>% 
  mutate(lifextoe = if_else(toe == 1,
                            as.numeric(dense_rank(ftdat)), NA)) %>% 
  ungroup() %>%
  right_join(lame3)

lame4 <- lame4 %>%
  group_by(cowid, lact) %>% 
  arrange(ftdat) %>% 
  # fills first row with 0 if not lame
  mutate(lactxlame = if_else(is.na(lactxlame)& is.na(prevtrimtype),
                             0, lactxlame)) %>% 
  # fills rest of NA's
  fill(lactxlame, .direction = "down") %>% 
  # to fix if first trim in next lact = just a trim
  mutate(lactxlame = if_else(is.na(lactxlame),
                             0, lactxlame)) %>%
  ungroup() %>% 
  group_by(cowid) %>%
  arrange(cowid, ftdat) %>%
  # does above for all lifex's
  mutate(across(starts_with("lifex"), 
                function(x) {if_else(is.na(x) & is.na(prevtrimtype), 0, x)
  })) %>%
  fill(starts_with("lifex"), .direction = "down")

export(lame4, "datafiles/lame4.rds")
# cleans up unused files
rm(lame, lame2, lame3)

# DataMan:Cull----
cullonly <- dry %>% 
  filter(event == "SOLD" | event == "DIED") %>%
  rename(birth = birth_date_item,
         ftdat = ftdat_item,
         frsh = fresh_date_item,
         breed = breed_item) |> 
    mutate(cowid = id,
         culled = 1,
         culldim = dim,
         #quiet stops warnings
         birth = mdy(birth, quiet = TRUE),
         frsh = mdy(frsh, quiet = TRUE),
         culldat = mdy(date),
         cullftdat = mdy(ftdat, quiet = TRUE),
         cullmon = month(culldat),
         cullyear = year(culldat),
         cullyearmon = as.yearmon(culldat)
  ) %>%
  # filter out heifers and goes back 36 months
  filter(frsh != is.na(frsh)) %>% 
  filter(cullyearmon>= startmon) %>% 
  filter(cullyearmon<= endmon) %>% 
  select(-c(id, dim, date, ftdat))

# join with lame data
# creates censorend data use last date of cowfile

lamecull <- lame4 %>% 
  left_join(cullonly, by = c("farm", "cowid",  
                             "lact", "breed", "birth", "frsh")) %>% 
  mutate(diftrimcull = culldat-ftdat,
         culled = if_else(is.na(culled),0, culled),
         cenenddat = mdy(censorend),
         difftrimcen = cenenddat - ftdat,
         #creating censordat, based on timetonextftdat
         censordat = case_when(is.na(time2nextftdat) & culled == 1 
                               ~ diftrimcull,
                               is.na(time2nextftdat) & culled == 0 
                               ~ difftrimcen,
                               TRUE ~ time2nextftdat)) %>%
  # to only culled the last trim as culled 
  group_by(cowid, lact) %>% 
  mutate(culled = if_else(censordat != diftrimcull & culled == 1, 
                          0, culled)) %>%
  ungroup()  
  # used to check code
  #select(farm, cowid, lact, ftdat, time2nextftdat, culled, culldat, cenenddat, 
  #       censordat, diftrimcull, difftrimcen)


export(lamecull, 'datafiles/lamecull.rds')

# remove unneeded
rm(cullonly)

# DataMan:DRY----
dryonly <- dry %>% 
  filter(event == "DRY") %>% 
  rename(birth = birth_date_item,
         frsh = fresh_date_item,
         ftdat = ftdat_item,
         breed = breed_item) |> 
  mutate(cowid = id,
         drydim = dim,
         #quiet stops warnings
         # birth = mdy(birth, quiet = TRUE), for MN farms
         birth = mdy(birth, quiet = TRUE),
         # frsh = mdy(frsh, quiet = TRUE),
         frsh = mdy(frsh, quiet = TRUE),
         drydat = mdy(date),
         dryftdat = mdy(ftdat, quiet = TRUE),
         drymon = month(drydat),
         dryyear = year(drydat),
         dryyearmon = as.yearmon(drydat)
  ) %>%
  # filter out heifers and goes back 12 months
  filter(frsh != is.na(frsh)) %>% 
  filter(dryyearmon>= start_month_1y) %>%
  filter(dryyearmon<= end_month_1y) %>%
  select(-c(id, dim, date, contains("lam"), contains("rem"), ftdat)) |> 
  # to filter out cows with 2 drydats in 1 lactation
  group_by(cowid, birth, frsh) |> 
  arrange(drydat) |> 
  slice_max(drydat) |> 
  ungroup()


#merge with lame file
lamedry <- lame4 %>% 
  select(farm, cowid, birth, frsh, lact, breed, ftdat, trimonly, lifexlame,
         lactxlame, totxtrimmed, prevftdat) %>% 
  left_join(dryonly, by = c("farm", "cowid", "birth", "frsh", "lact", "breed"),
            relationship = "many-to-many") %>% 
  # classify based on days from dry date
  mutate(diftrimdry = drydat-ftdat,
         diftrimdry2 = abs(diftrimdry),
         drytrim = case_when(diftrimdry < -7 ~ 0,
                             diftrimdry >60 ~0,
                             is.na(diftrimdry) ~ 0,
                             TRUE ~ 1)) %>% 
  #keep only the drytrim info
  filter(drytrim == 1) |>
  # to sort out cows with trims before and after
  mutate(post_dry_trim = if_else(diftrimdry<0, 1, 0))|>
  group_by(cowid, lact, drydat) %>% 
  arrange(post_dry_trim)|>
  slice_min(post_dry_trim, n = 1, with_ties = FALSE)|>
  arrange(diftrimdry)|>
  slice_min(diftrimdry, n=1, with_ties = F) %>% 
  ungroup() %>% 
  select(c(farm, cowid, birth, frsh, lact, breed, 
           drydat, diftrimdry, ftdat, trimonly, drytrim, post_dry_trim))
# still some duplicate farm cowid, but those frsh 3x in 1 year

lamecull <- lamecull |> 
  left_join(lamedry, by = c("farm", "cowid", "birth", "frsh", "lact", "breed", 
                            "ftdat", "trimonly"),
            relationship = "many-to-many") |> 
  mutate(trim_type = case_when(drytrim == 1 ~ "Dry",
                               ftdim < early ~ "Early",
                               ftdim >= early & ftdim <= mid ~ "Mid",
                               ftdim > mid & drytrim == 0 ~ "Late",
                               ftdim > mid & is.na(drytrim) ~ "Late",
                               TRUE ~ "Late"))

#remove files no longer needed
rm(dryonly)
export(lamedry, "datafiles/lamedry.rds")
export(lamecull, "datafiles/lamecull.rds")


