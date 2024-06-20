

#Testing stuff ------------
lame4_original<-lame4 #for testing
cowid_list<-unique(lame4_original$cowid)[c(3:7)] #select a few cows for testing
lame4<-lame4_original%>%
  filter(cowid %in% cowid_list)





# new code for repeats--------------
fxn_get_lesion_dates<-function(lesion, life){
  
  if(dim(lame4)[1]>0)
  les1 <<- lame4 |>
    # filter out the specific lesion
    filter(lifexlame >0) %>%
    filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
    mutate(les_date_1 = ftdat) |>
    select(farm, cowid, frsh, les_date_1) |>
    left_join(lame4, by = c("farm", "cowid", "frsh"))
  
  if(dim(les1)[1]>0)
  les2<<- les1 |>
    filter(!!sym(life) > 0 ) |>
    filter(les_date_1 <= ftdat) %>%
    filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
    mutate(les_date_2 = ftdat) |>
    select(farm, cowid, frsh, les_date_1, les_date_2) |>
    left_join(lame4, by = c("farm", "cowid", "frsh"))
  
  if(dim(les2)[1]>0)
  les2to3 <<- lame4 |>
    # filter out the specific lesion
    filter(  lifexlame >0) %>%
    filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
    mutate(les_date_2 = ftdat) |>
    select(farm, cowid, frsh, les_date_2) |>
    left_join(lame4, by = c("farm", "cowid", "frsh"))
  
  if(dim(les2to3)[1]>0)
  les3 <<- les2to3 |>
    filter(!!sym(life) > 0 ) |>
    filter(les_date_2 <= ftdat) %>%
    filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
    mutate(les_date_3 = ftdat) |>
    select(farm, cowid, frsh, les_date_2, les_date_3) |>
    left_join(lame4, by = c("farm", "cowid", "frsh"))
  
  if(dim(les3)[1]>0)
  les3to4 <<- lame4 |>
    # filter out the specific lesion
    filter(  lifexlame >0) %>%
    filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
    mutate(les_date_3 = ftdat) |>
    select(farm, cowid, frsh, les_date_3) |>
    left_join(lame4, by = c("farm", "cowid", "frsh"))
  
  if(dim(les3to4)[1]>0)
  les4 <<- les3to4 |>
    filter(!!sym(life) > 0 ) |>
    filter(les_date_3 <= ftdat) %>%
    filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
    mutate(les_date_4 = ftdat) |>
    select(farm, cowid, frsh, les_date_3, les_date_4) |>
    left_join(lame4, by = c("farm", "cowid", "frsh"))
  
  # now filter out just specific cows with each case
  if(dim(les1)[1]>0)
  les1 <<- les1 |>
    filter(!!sym(lesion) == 1, !!sym(life) == 1)
  
  if(dim(les2)[1]>0)
  les2 <<- les2 |>
    filter(!!sym(lesion) == 1, !!sym(life) == 2)
  
  if((dim(les1)[1]>0)&(dim(les2)[1]>0))
  reples1to2 <<- bind_rows(les1, les2) |>
    select(farm, cowid, !!sym(life)) %>%
    group_by(farm, !!sym(life)) %>%
    count() %>%
    pivot_wider(names_from = !!sym(life), values_from = n,
                names_prefix = "case") %>%
    mutate(les_repeat = (round(case2/case1*100,1)),
           case = 1) %>%
    select(farm, case, les_repeat)
  
  if(dim(les2to3)[1]>0)
  les2to3 <<- les2to3 |>
    filter(!!sym(lesion) == 1, !!sym(life) == 2)
  
  if(dim(les3)[1]>0)
  les3 <<- les3 |>
    filter(!!sym(lesion) == 1, !!sym(life) == 3)
  
  if((dim(les3)[1]>0)&(dim(les2to3)[1]>0))
  reples2to3 <<- bind_rows(les3, les2to3) |>
    select(farm, cowid, !!sym(life)) %>%
    group_by(farm, !!sym(life)) %>%
    count() %>%
    pivot_wider(names_from = !!sym(life), values_from = n,
                names_prefix = "case") %>%
    mutate(les_repeat = (round(case3/case2*100,1)),
           case = 2) %>%
    select(farm, case, les_repeat)
  
  if(exists('les3to4'))
  les3to4 <<- les3to4 |>
    filter(!!sym(lesion) == 1, !!sym(life) == 3)
  
  if(exists('les4'))
  les4 <<- les4 |>
    filter(!!sym(lesion) == 1, !!sym(life) == 4)
  
  if(exists('les4')&exists('les3to4'))
  reples3to4 <<- bind_rows(les4, les3to4) |>
    select(farm, cowid, !!sym(life)) %>%
    group_by(farm, !!sym(life)) %>%
    count() %>%
    pivot_wider(names_from = !!sym(life), values_from = n,
                names_prefix = "case") %>%
    mutate(les_repeat = (round(case4/case3*100,1)),
           case = 3) %>%
    select(farm, case, les_repeat)
}




#************************---------------

#dd--------------------
fxn_get_lesion_dates(lesion = 'dd', life = 'lifexdd')


repdd <- bind_rows(reples1to2, reples2to3, reples3to4) |>
  rename(dd_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, 
   les1to2, les2to3, les3to4)
# ulcer -------------------------------------

fxn_get_lesion_dates(lesion = "soleulcer", life = "lifexulc")

repulc <- bind_rows(reples1to2, reples2to3, reples3to4) |>
  rename(ulc_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, 
   les1to2, les2to3, les3to4)

# wld ------------------------------------------

fxn_get_lesion_dates(lesion = "wld", life = "lifexwld")

repwld <- bind_rows(reples1to2, reples2to3, reples3to4) |>
  rename(wld_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, 
   les1to2, les2to3, les3to4)
# footrot ------------------------------------------

fxn_get_lesion_dates(lesion = "footrot", life = "lifexfr")

repfr <- bind_rows(reples1to2, reples2to3) |>
  rename(fr_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, 
   les1to2, les2to3, les3to4)
#***********************-----

# old code for repeats -----------------------------------------

# # dd ----------------------------------
#
# lesion <- "dd"
# life <- "lifexdd"
#
# les1 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
#   mutate(les_date_1 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2<- les1 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_1 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2to3 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3 <- les2to3 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_2 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3to4 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les4 <- les3to4 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_3 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
#   mutate(les_date_4 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3, les_date_4) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# # now filter out just specific cows with each case
# les1 <- les1 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 1)
#
# les2 <- les2 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# reples1to2 <- bind_rows(les1, les2) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case2/case1*100,1)),
#          case = 1) %>%
#   select(farm, case, les_repeat)
#
# les2to3 <- les2to3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# les3 <- les3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# reples2to3 <- bind_rows(les3, les2to3) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case3/case2*100,1)),
#          case = 2) %>%
#   select(farm, case, les_repeat)
#
# les3to4 <- les3to4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# les4 <- les4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 4)
#
# reples3to4 <- bind_rows(les4, les3to4) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case4/case3*100,1)),
#          case = 3) %>%
#   select(farm, case, les_repeat)
#
# repdd <- bind_rows(reples1to2, reples2to3, reples3to4) |>
#   rename(dd_repeat = les_repeat)
# rm(reples1to2, reples2to3, reples3to4,
#    les4, les3, les2, les1, les2to3)
#
# # ulcer -----------------------------------------
#
# lesion <- "soleulcer"
# life <- "lifexulc"
#
# les1 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
#   mutate(les_date_1 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2<- les1 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_1 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2to3 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3 <- les2to3 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_2 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3to4 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les4 <- les3to4 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_3 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
#   mutate(les_date_4 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3, les_date_4) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# # now filter out just specific cows with each case
# les1 <- les1 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 1)
#
# les2 <- les2 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# reples1to2 <- bind_rows(les1, les2) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case2/case1*100,1)),
#          case = 1) %>%
#   select(farm, case, les_repeat)
#
# les2to3 <- les2to3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# les3 <- les3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# reples2to3 <- bind_rows(les3, les2to3) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case3/case2*100,1)),
#          case = 2) %>%
#   select(farm, case, les_repeat)
#
# les3to4 <- les3to4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# les4 <- les4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 4)
#
# reples3to4 <- bind_rows(les4, les3to4) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case4/case3*100,1)),
#          case = 3) %>%
#   select(farm, case, les_repeat)
#
# repulc <- bind_rows(reples1to2, reples2to3, reples3to4) |>
#   rename(ulc_repeat = les_repeat)
#
# rm(reples1to2, reples2to3, reples3to4,
#    les4, les3, les2, les1, les2to3)
#
#
# # wld ----------------------------------
#
# lesion <- "wld"
# life <- "lifexwld"
#
# les1 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
#   mutate(les_date_1 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2<- les1 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_1 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2to3 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3 <- les2to3 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_2 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3to4 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les4 <- les3to4 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_3 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
#   mutate(les_date_4 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3, les_date_4) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# # now filter out just specific cows with each case
# les1 <- les1 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 1)
#
# les2 <- les2 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# reples1to2 <- bind_rows(les1, les2) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case2/case1*100,1)),
#          case = 1) %>%
#   select(farm, case, les_repeat)
#
# les2to3 <- les2to3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# les3 <- les3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# reples2to3 <- bind_rows(les3, les2to3) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case3/case2*100,1)),
#          case = 2) %>%
#   select(farm, case, les_repeat)
#
# les3to4 <- les3to4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# les4 <- les4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 4)
#
# reples3to4 <- bind_rows(les4, les3to4) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case4/case3*100,1)),
#          case = 3) %>%
#   select(farm, case, les_repeat)
#
# repwld <- bind_rows(reples1to2, reples2to3, reples3to4) |>
#   rename(wld_repeat = les_repeat)
#
# rm(reples1to2, reples2to3, reples3to4,
#    les4, les3, les2, les1, les2to3)
#
# # footrot ------------------------------------------
#
# lesion <- "footrot"
# life <- "lifexfr"
#
# les1 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
#   mutate(les_date_1 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2<- les1 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_1 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_1, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# les2to3 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
#   mutate(les_date_2 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3 <- les2to3 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_2 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_2, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les3to4 <- lame4 |>
#   # filter out the specific lesion
#   filter(  lifexlame >0) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
#   mutate(les_date_3 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
#
# les4 <- les3to4 |>
#   filter(!!sym(life) > 0 ) |>
#   filter(les_date_3 <= ftdat) %>%
#   filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
#   mutate(les_date_4 = ftdat) |>
#   select(farm, cowid, frsh, les_date_3, les_date_4) |>
#   left_join(lame4, by = c("farm", "cowid", "frsh"))
#
# # now filter out just specific cows with each case
# les1 <- les1 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 1)
#
# les2 <- les2 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# reples1to2 <- bind_rows(les1, les2) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case2/case1*100,1)),
#          case = 1) %>%
#   select(farm, case, les_repeat)
#
# les2to3 <- les2to3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 2)
#
# les3 <- les3 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# reples2to3 <- bind_rows(les3, les2to3) |>
#   select(farm, cowid, !!sym(life)) %>%
#   group_by(farm, !!sym(life)) %>%
#   count() %>%
#   pivot_wider(names_from = !!sym(life), values_from = n,
#               names_prefix = "case") %>%
#   mutate(les_repeat = (round(case3/case2*100,1)),
#          case = 2) %>%
#   select(farm, case, les_repeat)
#
# les3to4 <- les3to4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 3)
#
# les4 <- les4 |>
#   filter(!!sym(lesion) == 1, !!sym(life) == 4)
#
# repfr <- bind_rows(reples1to2, reples2to3) |>
#   rename(fr_repeat = les_repeat)
#
# rm(reples1to2, reples2to3,
#    les4, les3, les2, les1, les2to3)
#




#***********----------
#Final TABLE for repeats -----------


# reptable ---------------------------

reptable <- repdd |> 
  left_join(repwld) %>% 
  left_join(repulc) %>%
  left_join(repfr) |> 
  ungroup()
#write_rds(reptable, 'reptable_for_test.rds') #write out the table using old code



# formate table ----------------------------
reptable1 <- reptable %>% 
  arrange(case)%>%
  select(-c(farm)) |> 
  flextable() %>% 
  add_header_row(values = c("Repeating Case", "Repeat %"),
                 colwidths = c(1,4)) |> 
  merge_at(i=1:2, j=1, part = "header") |> 
  set_header_labels("",
                    dd_repeat = "DD",
                    wld_repeat = "WLD",
                    ulc_repeat = "Sole Ulcer",
                    fr_repeat = "Footrot" 
  ) %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  bold(i = 2, bold = TRUE, part = "header") |> 
  labelizor(part = "body", labels = c("1" = "1st to 2nd",
                                      "2" = "2nd to 3rd",
                                      "3" = "3rd to 4th")) |> 
  align(align = "center", part = "all") |> 
  autofit()


#test new code --------------------
reptable_for_test<-read_rds('reptable_for_test.rds')

waldo::compare(reptable_for_test, reptable)
