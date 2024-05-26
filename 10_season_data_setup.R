# File to create Seasonal Graphs

# Season Data----

# season milking by farm
mallavgm <- mall %>% 
  #to get 12 months yearmon format p year + month#-1/112
  filter(ftyearmon >= startym) %>% 
  filter(ftyearmon < endym)

# lesion totals by month by farm
ygerrormall <- lame4 %>%
  filter(ftdat >= startdat & lifexlame >0) %>% # only cows with lesions
  filter(ftdat < enddat) %>%
  mutate(lesion = recode(trimonly, '0'= 1, '1' = 0)) %>% 
  group_by(farm, ftyearmon) %>% 
  summarise(across(all_of(les_variables), ~sum(.x, na.rm = TRUE))) %>% 
  left_join(mallavgm, by = c("farm" , "ftyearmon")) %>% 
  # get totals/month
  select(-c(l1milking, l2milking, l3milking, l4pmilking)) %>% 
  pivot_longer(cols = all_of(les_variables),
               names_to = "lestype",
               values_to = "counts") %>% 
  mutate(cases = round(counts/allmilking*100,1)) 

#Graphn1:just 1st case ever for any lesion (ie no other history before)----
yg1errormall <- lame4 %>% 
  filter(ftdat > startdat & lifexlame == 1) %>% # only cows with lesions
  filter(ftdat < enddat) %>%
  mutate(lesion = recode(trimonly, '0'= 1, '1' = 0)) %>% 
  group_by(farm, ftyearmon) %>% 
  summarise(across(all_of(les_variables), ~ sum(.x, na.rm = TRUE))) %>% 
  left_join(mallavgm, by = c("farm" ,"ftyearmon")) %>% 
  # get totals/month
  select(-c(l1milking, l2milking, l3milking, l4pmilking)) %>% 
  pivot_longer(cols = all_of(les_variables),
               names_to = "lestype",
               values_to = "counts") %>% 
  mutate(n1cases = round(counts/allmilking*100,1))  

#join total with n1
toterrorallm <- ygerrormall %>% 
  left_join(yg1errormall, by =c("farm","ftyearmon", "allmilking", 
                                "lestype")) %>% 
  select(-c(counts.x, counts.y))