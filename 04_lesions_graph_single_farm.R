# set up data
# get average pop/year
mallavg <- mall %>% 
  group_by(farm ) %>% 
  summarise(across(c(contains("milking")),
                   mean,
                   na.rm = TRUE)) %>%
  mutate(across(c(contains("milking")),round ))

# count # of lesions/during the year
ygerror <- lame4 %>% 
  filter(lifexlame >0) %>% # only cows with lesions
  filter(ftdat < enddat) %>%
  mutate(lesion = recode(trimonly, '0'= 1, '1' = 0)) %>% 
  group_by(farm ) %>% 
  summarise(across(all_of(les_variables), sum, na.rm = TRUE)) %>% 
  left_join(mallavg, by = c("farm" )) %>% 
  # get totals/month
  select(-c(l1milking, l2milking, l3milking, l4pmilking))

# system summary
ygerrort <- ygerror %>%
  summarise(across(c(allmilking, all_of(les_variables)), sum, na.rm = TRUE)) %>% 
  mutate(farm = "System")

#puts the 2 files together
ygerrorall<-bind_rows(ygerror, ygerrort)  %>% 
  # make longer format for graphing and calculations
  pivot_longer(cols = c(les_variables),
               names_to = "lestype",
               values_to = "counts") %>% 
  mutate(cases = round(counts/allmilking*100,1)) # to replace binconinf not working

# merge
ygerrorall3 <- ygerrorall %>% 
  rename(all_counts = counts)
#  left_join(df, by =c("farm", "counts","allmilking", "lestype"))

rm(ygerror, ygerrorall, ygerrort)

# Graphn1:just 1st case ever for any lesion (ie no other history before)
yg1error <- lame4 %>% 
  filter(lifexlame == 1) %>% # only cows with lesions
  filter(ftdat < enddat) %>%
  mutate(lesion = recode(trimonly, '0'= 1, '1' = 0)) %>% 
  group_by(farm ) %>% 
  summarise(across(all_of(les_variables), sum, na.rm = TRUE)) %>% 
  left_join(mallavg, by = c("farm" )) %>% 
  # get totals/month
  select(-c(l1milking, l2milking, l3milking, l4pmilking))

# system summary
yg1errort <- yg1error %>%
  summarise(across(c(allmilking, all_of(les_variables)), sum, na.rm = TRUE)) %>% 
  mutate(farm = "System")


yg1errorall <- bind_rows(yg1error, yg1errort) %>% 
  #make longer format for graphing and calculations
  pivot_longer(cols = c(les_variables),
               names_to = "lestype",
               values_to = "counts") %>% 
  mutate(n1cases = round(counts/allmilking*100,1)) # to replace binconinf not working

# merge
yg1errorall3 <- yg1errorall %>% 
  #  left_join(df, by =c("farm", "counts","allmilking", "lestype"))%>% 
  rename(n1_counts = counts)

# join total with n1
toterrorall <- ygerrorall3 %>% 
  left_join(yg1errorall3, by =c("farm" , "allmilking", "lestype")) |> 
  mutate(farm = as_factor(farm)) |> 
  ungroup()

rm(yg1error, yg1errort, yg1errorall, yg1errorall3, ygerrorall3)


all_les_labels <- c(inf = "Infectious",
                    noninf = "Non-Infectious",
                    soleulcer = "Sole Ulcer",
                    wld = "White Line Disease",
                    dd = "Digital Dermatitis",
                    footrot = "Foot Rot",
                    thin = "Thin Sole",
                    cork = "Cork Screw",
                    injury = "Upper Leg",
                    solefract = "Sole Fracture",
                    toe = "Toe Lesion",
                    other = "Other Lesion",
                    hem = "Hemorrhage",
                    axial = "Axial Wall Crack",
                    lesion = "Any Lesion")

min <- toterrorall |> 
  filter(all_counts != 0) %>% 
  filter(farm != "System") |> 
  ungroup() |> 
  mutate(min = min(n1cases)) |> 
  slice_head() |>
  mutate((min = floor(min))) |> 
  select(min) 

min_limit <- min$min[1] 


max <- toterrorall |> 
  filter(all_counts != 0) %>% 
  filter(farm != "System") |> 
  ungroup() |> 
  mutate(max = max(cases)) |> 
  slice_head() |> 
  select(max) |> 
  mutate(max =ceiling(max)
  )
# extract max value
max_limit <- max$max[1]  

break_graph <- max_limit/10

# Lesion Cleveland graph----
lesgclev <- toterrorall %>%
  # filter out lesions with zeros
  filter(farm != "System") |> 
  filter(all_counts != 0) %>% 
  mutate(lestype = fct_reorder(lestype, cases)) %>% 
  group_by(farm) %>% 
  ggplot() +
  geom_segment(aes(x = lestype, xend = lestype, 
                   y = cases, yend = n1cases, 
                   color = "Repeats"), 
               linewidth = 2) +
  geom_point(aes(x = lestype, y = cases, color = "All"), size = 6) +
  geom_text(aes(lestype, cases, label = round(cases, digits = 0)), 
            nudge_x = +.01, 
            color = "white", size = 2.5,
            show.legend = FALSE) +
  geom_point(aes(x = lestype, y = n1cases, color = "1st"), size = 6) +
  geom_text(aes(lestype, n1cases, label = round(n1cases, digits = 0)), nudge_x = +.01,  
            color = "white", size = 2.5) +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("# of cases per 100 cows") + 
  scale_y_continuous(limits = c(0, max_limit),
                     breaks = seq(0, max_limit, by = 5),
                     expand = c(0.02, 0.02)) +
  scale_x_discrete(expand = c(0.05, 0.05),
                   labels = all_les_labels) +
  scale_color_manual(name = "# of Cases", 
                     values = c("Repeats" = "#FDE725FF", #viridis colours
                                "All" = "#440154FF", 
                                "1st" = "#7ad151ff"),
                     labels = c("1st", "Repeats", "All"),
                     breaks = c("1st", "Repeats", "All")
  )+
  guides(color = guide_legend(override.aes = list(shape = c(19, NA, 19),
                                                  linetype = c(1, 1, 1)
  ),
  label.position = "bottom",
  title.position = "top",
  title.hjust = 0.5)
  )+
  theme(legend.position = "top")


