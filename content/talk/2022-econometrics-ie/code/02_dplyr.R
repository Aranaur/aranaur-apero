library(tidyverse)

starwars

# filter()

# slice()

# arrange()

starwars %>% 
  arrange(height)

starwars %>% 
  arrange(desc(height))

starwars %>% 
  arrange(-height)

starwars %>% 
  arrange(desc(height, mass))

# select()

starwars %>% 
  select(1, 3, 5)

starwars %>% 
  select(name, mass, skin_color)

starwars %>% 
  select(5:8)

starwars %>% 
  select(1, skin_color:sex)

starwars %>% 
  select(-name)

starwars %>% 
  select(!(name:mass))

starwars %>% 
  select(!c(name:mass, sex))

starwars %>% 
  select(ends_with("color"))

starwars %>% 
  select(starts_with("s"))

starwars %>% 
  select(contains("_"))

starwars %>% 
  select(contains("_") | starts_with("s"))

starwars %>% 
  select(where(is.numeric))

starwars %>% 
  select(where(is.numeric), everything())

# relocate()

starwars %>% 
  relocate(sex)

starwars %>% 
  relocate(name, .after = sex)

starwars %>% 
  relocate(name, .before = sex)

starwars %>% 
  relocate(where(is.numeric), .after = where(is.character))

# rename()

starwars %>% 
  rename(name_char = name,
         mass_kg = mass,
         height_mm = height)

starwars %>% 
  rename_with(toupper)

starwars %>% 
  rename_with(toupper, contains("_"))

starwars %>% 
  rename_with(~ gsub("_", ".", .x))


# mutate()

starwars %>% 
  mutate(mass_g = mass * 1000) %>% 
  relocate(mass_g)

starwars %>% 
  select(1:3) %>% 
  mutate(BMI = mass / (height/100)^2) %>% 
  arrange(-BMI)

starwars_bmi <- starwars %>% 
  select(1:3) %>% 
  transmute(name,
            BMI = mass / (height/100)^2)

starwars %>% 
  mutate(sex = as.factor(sex))

starwars %>% 
  mutate(name = NULL)

# group_by() + summarise()

starwars %>% 
  group_by(sex) %>% 
  summarise(mass_mean = mean(mass, na.rm = TRUE),
            mass_median = median(mass, na.rm = TRUE),
            mass_sd = sd(mass, na.rm = TRUE))

starwars %>% 
  group_by(sex, gender) %>% 
  summarise(mass_mean = mean(mass, na.rm = TRUE),
            mass_median = median(mass, na.rm = TRUE),
            mass_sd = sd(mass, na.rm = TRUE))


starwars %>% 
  group_by(sex) %>% 
  summarise(mass_mean = mean(mass, na.rm = TRUE),
            height_mean = mean(height, na.rm = TRUE))

starwars %>% 
  group_by(sex) %>% 
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = TRUE,
                   .names = "mean_{.col}"))

starwars %>% 
  group_by(sex) %>% 
  summarise(across(where(is.numeric),
                   list(avg = mean, stdev = sd, median = median),
                   na.rm = TRUE,
                   .names = "{.fn}_{.col}"))

starwars %>% 
  group_by(sex) %>% 
  summarise(n = n())

starwars %>% 
  count(sex, gender)

# case_when()

starwars %>% 
  mutate(mass_bin = case_when(
    mass > 100 ~ "large",
    mass > 60 & mass <= 100 ~ "medium",
    mass <= 60 ~ "small",
    TRUE ~ "0"
   )) %>% 
  select(name, mass, mass_bin)





























