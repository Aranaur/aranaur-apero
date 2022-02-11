library(tidyverse)
library(here)
library(vroom)
library(readxl)
library(palmerpenguins)

here()


# .csv
read_file <- read_delim(here("00_data/csv/bikes.csv"), delim = ";")

read_url <- read_csv("https://git.io/JztOr")

vroom_url <- vroom("https://git.io/JztOr")

vroom_file <- vroom(here("00_data/csv/bikes.csv"))

files <- fs::dir_ls("00_data/same_structure", glob = "*.csv")
vroom_all <- vroom(files)

# .xlsx

read_xlsx <- read_excel(here("00_data/xlsx/bikes.xlsx"))

# data.frame vs. tibble
iris
class(iris)

as_tibble(iris)
class(as_tibble(iris))

glimpse(iris)
glimpse(read_url)
glimpse(vroom_all)

# dplyr
penguins
glimpse(penguins)

# filter
filter(penguins, species == "Adelie")

filter(penguins, species == "Adelie" | species == "Gentoo")

filter(penguins, species %in% c("Adelie", "Gentoo"))

sort(sqrt(abs(sin(1:10))))

1:10 %>% 
  sin() %>% 
  abs() %>% 
  sqrt() %>% 
  sort()

penguins %>% 
  filter(species == "Adelie")


penguins %>% 
  filter(species == "Adelie" & sex == "male")

pen_adel_male_deam <- penguins %>% 
  filter(species == "Adelie",
         sex == "male",
         island == "Dream")

write_csv(pen_adel_male_deam, "pen_adel_male_deam.csv")


penguins %>% 
  filter(bill_length_mm  > 40,
         species == "Adelie")

penguins %>% 
  filter(species != "Adelie")

# slice

penguins %>% 
  slice(1:5)

penguins %>% 
  slice(1:5, 10)

penguins %>% 
  slice(-(1:3))

penguins %>% 
  slice_head(n = 20)

penguins %>% 
  slice_tail(n = 20)


penguins %>% 
  slice_max(bill_length_mm, n = 3)

penguins %>% 
  slice_max(body_mass_g, n = 3, with_ties = FALSE)

penguins %>% 
  slice_sample(n = 3)

penguins %>% 
  slice_sample(n = 50, replace = TRUE)

# arrange()

penguins %>% 
  arrange(bill_length_mm)

penguins %>% 
  arrange(desc(bill_length_mm))

penguins %>% 
  arrange(-bill_length_mm)

penguins %>% 
  arrange(bill_length_mm, bill_depth_mm)

# select()

penguins %>% 
  select(year, island, species)

penguins %>% 
  select(species:body_mass_g)

penguins %>% 
  select(3:5, 8)


penguins %>% 
  select(-sex)

penguins %>% 
  select(!c(sex, year, species:bill_length_mm))


penguins %>% 
  select(starts_with("bill"))

penguins %>% 
  select(ends_with("mm"))

penguins %>% 
  select(contains("length"))

penguins %>% 
  select(contains("length") | starts_with("bill"))

penguins %>% 
  select(where(is.integer))

penguins %>% 
  select(where(is.factor))

# relocate()

penguins %>% 
  relocate(year)

penguins %>% 
  relocate(body_mass_g, .after = island)

penguins %>% 
  relocate(body_mass_g, .before = year)

penguins %>% 
  relocate(where(is.numeric), .after = where(is.factor))

penguins_reloc_tbl <- penguins %>% 
  relocate(where(is.numeric), .before = where(is.factor))

# rename()

# df %>% 
#   rename(новое_имя = старое_имя)

penguins %>% 
  rename(palmer_island = island,
         mass = body_mass_g)

penguins %>% 
  rename_with(toupper)

penguins %>% 
  rename_with(toupper, ends_with("mm"))

penguins %>% 
  rename_with(~ gsub("_", ".", .x))
  
# mutate()

penguins %>% 
  mutate(body_mass_kg = body_mass_g / 1000, .after = body_mass_g)

penguins_mutate_tbl <- penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm,
         body_mass_kg = body_mass_g / 1000,
         flipper_length_m = flipper_length_mm / 1000)

penguins %>% 
  mutate(record_number = row_number()) %>% 
  relocate(record_number)

penguins %>% 
  transmute(record_number = row_number(), species)

penguins %>% 
  transmute(record_number = seq(1:n()), species)

penguins %>% 
  mutate(island = as.character(island))

penguins %>% 
  pull(island)

penguins %>% 
  mutate(island = fct_relevel(island, "Torgersen", "Biscoe", "Dream")) %>% 
  pull(island)

# group_by() %>% summarise()

penguins %>% 
  group_by(species) %>% 
  summarise(mass_mean = mean(body_mass_g, na.rm = TRUE),
            mass_median = median(body_mass_g, na.rm = TRUE),
            mass_sd = sd(body_mass_g, na.rm = TRUE))

penguins %>% 
  # отберём нужные столбцы
  select(species, sex, body_mass_g) %>% 
  
  drop_na() %>% 
  group_by(species, sex) %>% 
  summarise(mass_mean = mean(body_mass_g, na.rm = TRUE),
            mass_median = median(body_mass_g, na.rm = TRUE),
            mass_sd = sd(body_mass_g, na.rm = TRUE),
            mass_max = max(body_mass_g),
            mass_min = min(body_mass_g)) %>% 
  ungroup()

penguins %>% 
  group_by(species) %>% 
  summarise(bill_length_mean = mean(bill_length_mm, na.rm = TRUE),
            bill_depth_mean = mean(bill_depth_mm, na.rm = TRUE),
            flipper_length_mean = mean(flipper_length_mm, na.rm = TRUE))

penguins %>% 
  group_by(species) %>% 
  summarise(across(ends_with("mm"), mean, na.rm = TRUE))

penguins %>% 
  group_by(species) %>% 
  summarise(across(bill_length_mm:body_mass_g, mean, na.rm = TRUE))

penguins %>% 
  group_by(species) %>% 
  summarise(across(c(bill_length_mm, body_mass_g), mean, na.rm = TRUE))


penguins %>% 
  group_by(species) %>% 
  summarise(across(ends_with("mm"),
                   mean,
                   na.rm = TRUE,
                   .names = "mean_{.col}"))

penguins %>% 
  group_by(species) %>% 
  summarise(across(ends_with("mm"),
                   list(avg = mean, stdev = sd, md = median),
                   na.rm = TRUE,
                   .names = "{.fn}_{.col}"))

penguins %>% 
  group_by(species) %>% 
  summarise(n = n())

penguins %>% 
  count(species)

penguins %>% 
  count(species, year)

# case_when()

penguins %>% 
  mutate(size_bin = case_when(
    body_mass_g > 4500 ~ "large",
    body_mass_g > 3000 & body_mass_g <= 4500 ~ "medium",
    body_mass_g <= 3000 ~ "small"
  )) %>% 
  select(body_mass_g, size_bin)


penguins$body_mass_g %>% 
  quantile(probs = 0.25, na.rm = TRUE)

penguins %>% 
  select(species, year, flipper_length_mm) %>% 
  rename(srudy_year = year) %>% 
  filter(species == "Adelie") %>% 
  mutate(
    flipper_rank = case_when(
      flipper_length_mm < 200 ~ 1,
      flipper_length_mm >= 200 ~ 2,
      TRUE ~ 0
    )
  )

# pivot_longer() + pivot_wider()

penguins %>% 
  pivot_longer(cols = contains("_"),
               names_to = "var_name",
               values_to = "var_value")

penguins_longer <- penguins %>% 
  pivot_longer(cols = contains("_"),
               names_sep = "_",
               names_to = c("part", "measure", "unit"),
               values_to = "var_value")
  
penguins_longer %>% 
  pivot_wider(names_from = c("part", "measure", "unit"),
              values_from = "var_value",
              names_sep = "_",
              values_fn = mean)


world_bank <- vroom(here("00_data/world_bank/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3607744.csv"), skip = 4)

world_bank_longer_ukraine <- world_bank %>% 
  rename_with(~ gsub(" ", "_", .x)) %>% 
  rename_with(tolower) %>% 
  pivot_longer(cols = -c("country_name":"indicator_code"),
               names_to = "year",
               values_to = "GDP") %>% 
  select(-c(indicator_name, indicator_code)) %>% 
  filter(country_name == "Ukraine",
         year %in% c(1992:2005))
  

