
# new code for repeats


lesion <- "dd"
life <- "lifexdd"

les1 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
  mutate(les_date_1 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2<- les1 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_1 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1, les_date_2) |>
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2to3 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3 <- les2to3 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_2 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3to4 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les4 <- les3to4 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_3 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
  mutate(les_date_4 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3, les_date_4) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

# now filter out just specific cows with each case
les1 <- les1 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 1)

les2 <- les2 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

reples1to2 <- bind_rows(les1, les2) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case2/case1*100,1)),
         case = 1) %>% 
  select(farm, case, les_repeat)

les2to3 <- les2to3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

les3 <- les3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

reples2to3 <- bind_rows(les3, les2to3) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case3/case2*100,1)),
         case = 2) %>% 
  select(farm, case, les_repeat)

les3to4 <- les3to4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

les4 <- les4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 4)

reples3to4 <- bind_rows(les4, les3to4) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case4/case3*100,1)),
         case = 3) %>% 
  select(farm, case, les_repeat)

repdd <- bind_rows(reples1to2, reples2to3, reples3to4) |> 
  rename(dd_repeat = les_repeat)
rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, les2to3)

# ulcer

lesion <- "soleulcer"
life <- "lifexulc"

les1 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
  mutate(les_date_1 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2<- les1 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_1 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1, les_date_2) |>
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2to3 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3 <- les2to3 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_2 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3to4 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les4 <- les3to4 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_3 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
  mutate(les_date_4 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3, les_date_4) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

# now filter out just specific cows with each case
les1 <- les1 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 1)

les2 <- les2 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

reples1to2 <- bind_rows(les1, les2) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case2/case1*100,1)),
         case = 1) %>% 
  select(farm, case, les_repeat)

les2to3 <- les2to3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

les3 <- les3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

reples2to3 <- bind_rows(les3, les2to3) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case3/case2*100,1)),
         case = 2) %>% 
  select(farm, case, les_repeat)

les3to4 <- les3to4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

les4 <- les4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 4)

reples3to4 <- bind_rows(les4, les3to4) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case4/case3*100,1)),
         case = 3) %>% 
  select(farm, case, les_repeat)

repulc <- bind_rows(reples1to2, reples2to3, reples3to4) |> 
  rename(ulc_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, les2to3)


# wld

lesion <- "wld"
life <- "lifexwld"

les1 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
  mutate(les_date_1 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2<- les1 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_1 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1, les_date_2) |>
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2to3 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3 <- les2to3 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_2 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3to4 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les4 <- les3to4 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_3 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
  mutate(les_date_4 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3, les_date_4) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

# now filter out just specific cows with each case
les1 <- les1 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 1)

les2 <- les2 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

reples1to2 <- bind_rows(les1, les2) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case2/case1*100,1)),
         case = 1) %>% 
  select(farm, case, les_repeat)

les2to3 <- les2to3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

les3 <- les3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

reples2to3 <- bind_rows(les3, les2to3) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case3/case2*100,1)),
         case = 2) %>% 
  select(farm, case, les_repeat)

les3to4 <- les3to4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

les4 <- les4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 4)

reples3to4 <- bind_rows(les4, les3to4) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case4/case3*100,1)),
         case = 3) %>% 
  select(farm, case, les_repeat)

repwld <- bind_rows(reples1to2, reples2to3, reples3to4) |> 
  rename(wld_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, les2to3)

# footrot

lesion <- "footrot"
life <- "lifexfr"

les1 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
  mutate(les_date_1 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2<- les1 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_1 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1, les_date_2) |>
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2to3 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3 <- les2to3 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_2 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3to4 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les4 <- les3to4 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_3 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
  mutate(les_date_4 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3, les_date_4) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

# now filter out just specific cows with each case
les1 <- les1 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 1)

les2 <- les2 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

reples1to2 <- bind_rows(les1, les2) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case2/case1*100,1)),
         case = 1) %>% 
  select(farm, case, les_repeat)

les2to3 <- les2to3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

les3 <- les3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

reples2to3 <- bind_rows(les3, les2to3) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case3/case2*100,1)),
         case = 2) %>% 
  select(farm, case, les_repeat)

les3to4 <- les3to4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

les4 <- les4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 4)

reples3to4 <- bind_rows(les4, les3to4) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case4/case3*100,1)),
         case = 3) %>% 
  select(farm, case, les_repeat)

repfr <- bind_rows(reples1to2, reples2to3, reples3to4) |> 
  rename(fr_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, les2to3)

lesion <- "toe"
life <- "lifextoe"

les1 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1 , !!sym(life) == 1) |>
  mutate(les_date_1 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2<- les1 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_1 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_1, les_date_2) |>
  left_join(lame4, by = c("farm", "cowid", "frsh"))

les2to3 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 2) |>
  mutate(les_date_2 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3 <- les2to3 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_2 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_2, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les3to4 <- lame4 |> 
  # filter out the specific lesion
  filter(ftdat > startdat & lifexlame >0) %>% 
  filter(!!sym(lesion) == 1, !!sym(life) == 3) |>
  mutate(les_date_3 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))


les4 <- les3to4 |> 
  filter(!!sym(life) > 0 ) |> 
  filter(les_date_3 <= ftdat) %>%
  filter(!!sym(lesion) == 1, !!sym(life) == 4) |>
  mutate(les_date_4 = ftdat) |> 
  select(farm, cowid, frsh, les_date_3, les_date_4) |> 
  left_join(lame4, by = c("farm", "cowid", "frsh"))

# now filter out just specific cows with each case
les1 <- les1 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 1)

les2 <- les2 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

reples1to2 <- bind_rows(les1, les2) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case2/case1*100,1)),
         case = 1) %>% 
  select(farm, case, les_repeat)

les2to3 <- les2to3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 2)

les3 <- les3 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

reples2to3 <- bind_rows(les3, les2to3) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case3/case2*100,1)),
         case = 2) %>% 
  select(farm, case, les_repeat)

les3to4 <- les3to4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 3)

les4 <- les4 |> 
  filter(!!sym(lesion) == 1, !!sym(life) == 4)

reples3to4 <- bind_rows(les4, les3to4) |> 
  select(farm, cowid, !!sym(life)) %>% 
  group_by(farm, !!sym(life)) %>% 
  count() %>% 
  pivot_wider(names_from = !!sym(life), values_from = n,
              names_prefix = "case") %>% 
  mutate(les_repeat = (round(case4/case3*100,1)),
         case = 3) %>% 
  select(farm, case, les_repeat)

reptoe <- bind_rows(reples1to2, reples2to3, reples3to4) |> 
  rename(toe_repeat = les_repeat)

rm(reples1to2, reples2to3, reples3to4,
   les4, les3, les2, les1, les2to3)

reptable <- repdd |> 
  left_join(repwld) %>% 
  left_join(repulc) %>%
  left_join(reptoe) |> 
  left_join(repfr) |> 
  ungroup()


# table
reptable1 <- reptable %>% 
  filter(case == 1) |> 
  arrange(dd_repeat)%>%
  select(-c(case)) |> 
  flextable() %>% 
  add_header_row(values = c("Farm", "Repeat %"),
                 colwidths = c(1,5)) |> 
  merge_at(i=1:2, j=1, part = "header") |> 
  set_header_labels("",
                    dd_repeat = "DD",
                    wld_repeat = "WLD",
                    ulc_repeat = "Sole Ulcer",
                    toe_repeat = "Toe/Thin",
                    fr_repeat = "Footrot") %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  bold(i = 2, bold = TRUE, part = "header") |> 
  align(align = "center", part = "all") 

# table
reptable2 <- reptable %>% 
  filter(case == 2) |> 
  arrange(dd_repeat)%>%
  select(-c(case)) |> 
  flextable() %>% 
  add_header_row(values = c("Farm", "Repeat %"),
                 colwidths = c(1,5)) |> 
  merge_at(i=1:2, j=1, part = "header") |> 
  set_header_labels("",
                    dd_repeat = "DD",
                    wld_repeat = "WLD",
                    ulc_repeat = "Sole Ulcer",
                    toe_repeat = "Toe/Thin",
                    fr_repeat = "Footrot") %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  bold(i = 2, bold = TRUE, part = "header") |> 
  align(align = "center", part = "all") 
