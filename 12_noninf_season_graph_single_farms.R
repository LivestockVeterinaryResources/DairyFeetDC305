# File to create Seasonal Graphs

# change this for each graph
lesion <- "noninf"

# calculate min/max for graphs

min <- toterrorallm |> 
  ungroup() |> 
  filter(lestype == lesion) %>% 
  mutate(min = min(n1cases)) |> 
  slice_head() |>
  mutate((min = floor(min))) |> 
  select(min) 

min_limit <- min$min[1] 


max <- toterrorallm |> 
  ungroup() |> 
  filter(lestype == lesion) %>% 
  mutate(max = max(cases)) |> 
  slice_head() |> 
  select(max) |> 
  mutate(max =ceiling(max)
  )
# extract max value
max_limit <- max$max[1]  

break_graph <- max_limit/10

# Season Graph----
non_inf_season_lesgclev <- toterrorallm %>%
  filter(lestype == lesion) %>% 
  arrange(desc(ftyearmon)) %>% 
  mutate(ftyearmon = yearmon(ftyearmon),
         ftyearmon = as.factor(ftyearmon),
         ftyearmon = fct_rev(ftyearmon)) %>% 
  ggplot() +
  geom_segment(aes(x = ftyearmon, xend = ftyearmon, 
                   y = cases, yend = n1cases, 
                   color = "Repeats"), 
               linewidth = 2) +
  geom_point(aes(x = ftyearmon, y = cases, color = "All"), size = 8) +
  geom_text(aes(ftyearmon, cases, label = round(cases, digits = 1)), 
            nudge_x = +.01, 
            color = "white", size = 2.5,
            show.legend = FALSE) +
  geom_point(aes(x = ftyearmon, y = n1cases, color = "1st"), size = 8) +
  geom_text(aes(ftyearmon, n1cases, label = round(n1cases, digits = 1)), nudge_x = +.01,  
            color = "white", size = 2.6) +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("Monthly # of cases per 100 cows") + 
  scale_y_continuous(limits = c(0, max_limit),
                     expand = c(0.02, 0.02)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
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
  theme(legend.position = "top")  # Positions the legend at the top
