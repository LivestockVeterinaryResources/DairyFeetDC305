## noninf DIM distribution
# labels for graph
lact.labs <- c("1" = "1st Lactation", 
               "2" = "2nd Lactation",
               "3" = "3+ Lactation")

dimles <- lame4 %>%
  # filter out the specific lesion
  filter(lifexlame > 0 & noninf == 1) %>% # only cows with lesions
  # mutate to create 1st, 2nd, 3rd lactgp
  mutate(lctgp = case_when(lact == 1 ~ "1",
                           lact == 2 ~ "2",
                           TRUE ~ "3"),
         lifexcat = case_when(lifexnoninf == 1 ~ "1st Case",
                              lifexnoninf == 2 ~ "2nd Case",
                              TRUE ~ "Chronic"), 
         dimcat = case_when(ftdim<31 ~ "30",
                            ftdim>30 & ftdim <61 ~ "60",
                            ftdim>60 & ftdim <91 ~ "90",
                            ftdim>90 & ftdim <121 ~ "120",
                            ftdim>120 & ftdim <151 ~ "150",
                            ftdim>150 & ftdim <181 ~ "180",
                            ftdim>180 & ftdim <211 ~ "210",
                            ftdim>210 & ftdim <241 ~ "240",
                            ftdim>240 & ftdim <271 ~ "270",
                            ftdim>270 & ftdim <301 ~ "300",
                            ftdim>300 & ftdim <331 ~ "330",
                            ftdim>330 & ftdim <361 ~ "360",
                            ftdim>360 & ftdim <391 ~ "390",
                            TRUE ~ "420")) %>%
  mutate(lctgp = as_factor(lctgp),
         dimcat = as.numeric(dimcat),
         lifexcat = as_factor(lifexcat)) %>% 
  select(farm, cowid, lctgp, ftdim, lifexinf, lifexcat, dimcat) 

# need to summarize counts across dim categories, lact and lifexdd
# add group by farm here if code changed
dimlescat <- dimles %>% 
  group_by(lifexcat) %>% 
  count(dimcat, lctgp, .drop = FALSE) %>%
  ungroup()
# calc total for each category 
# then change to %
dimlestot <- dimles %>% 
  group_by(lifexcat, lctgp) %>% 
  count() %>%
  ungroup() |> 
  rename(total = n) |> 
  right_join(dimlescat, by = c("lifexcat", "lctgp")) |> 
  mutate(prop = (n/total)*100)  |> 
  select(lctgp, dimcat, lifexcat, prop) |> 
  rename(n = prop) # to maintain code below

# to get totals  
dimlest <- dimles %>% 
  # add farm if changing data set
  group_by(lctgp, dimcat) %>%
  count() %>% 
  mutate(lifexcat = "Total") |> 
  ungroup()
# calc total for each category 
# then change to %
dimlessum <- dimles %>% 
  group_by(lctgp) %>% 
  count() %>%
  ungroup() |> 
  rename(total = n) |> 
  right_join(dimlest, by = c("lctgp")) |> 
  mutate(prop = (n/total)*100) |> 
  select(lctgp, dimcat, prop) |> 
  mutate(lifexcat = "Total") |> 
  rename(n = prop) |> # to maintain code below
  bind_rows(dimlestot) %>% 
  # need to pivot wide
  pivot_wider(names_from = lifexcat, values_from = n) %>% 
  clean_names() %>%
  # creates repeat % probably needs and overall one
  mutate(first = x1st_case,
         second = x2nd_case) %>% 
  select(-c(x1st_case, x2nd_case))

# clean space
rm(dimlest, dimlestot)

# set max limit
max <- dimlessum |> 
  mutate(max_total = ceiling(total),
         max_first = ceiling(first),
         max_second = ceiling(second),
         max = case_when(max_total > max_first & 
                           max_first < max_second ~ max_total,
                         max_first > max_total &
                           max_second < max_first ~ max_first,
                         TRUE ~ max_second)
  )|>
  arrange(desc(max)) |> 
  slice_head() |> 
  select(max) |> 
  mutate(max = ceiling(max/10)*10)
# extract max value
max_limit <- max$max[1]  

# needed for graph
offset <- 7.5

# system
dimg_farm1 <- dimlessum %>%
  mutate(lctgp = fct_relevel(lctgp,"3","2","1")) |>  
  ggplot(aes(dimcat, total)) +
  geom_linerange(aes(dimcat - offset, ymin = 0, ymax = total), 
                 color = "#440154ff", alpha = .8, size = 1.4) +
  geom_point(aes(dimcat - offset, total), 
             color = "#440154ff", size = 3) +
  geom_linerange(aes(dimcat, ymin = 0, ymax = first), 
                 color = "#FDE725FF", alpha = .8, size = 1.4) +
  geom_linerange(aes(dimcat + offset, ymin = 0, ymax = second), 
                 color = "#21908CFF", alpha = .8, size = 1.4) +
  geom_point(aes(dimcat , first), 
             color = "#FDE725FF", size = 3) +
  geom_point(aes(dimcat + offset, second), 
             color = "#21908CFF", size = 3) +
  facet_wrap(~lctgp, ncol = 1, strip.position = "top", 
             labeller = labeller(lctgp = lact.labs),
             scales = "free"
            ) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = "% of Cases in Category",
       title = "Days in Milk distribution in the past 12 months",
       caption = "Lifetime Case Type: <span style='color:#440154ff;'>All</span> • <span style='color:#FDE725FF;'>1st</span> • <span style='color:#21908CFF;'>2nd </span>")+
  scale_x_continuous(breaks = seq (30, 420, by = 30), limits = c(0,430))+
  scale_y_continuous(breaks = seq(0, max_limit, by = 5), 
                     limits = c(0, max_limit),
                     position = "left") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text.x = element_text(color = "grey45", margin = margin(t = 1, b = 1)),
    axis.text.y = element_text(color = "grey 45", margin = margin(l = 7)),
    plot.title = element_markdown(hjust = 0.5, color = "black"),
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "black", hjust = 0.5),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(.4, "lines"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey15", linewidth = .3),
  ) 
