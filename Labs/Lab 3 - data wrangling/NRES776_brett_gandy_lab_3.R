# load packages ----
library(here)
library(tidyverse)
library(palmerpenguins)

# read csv ----

clean_lab <- read_csv(file = here("data", "NRES776_cleaned_lab3.csv"))

dirty_lab <- read_csv(file = here("data", "NRES776_dirty_lab3.csv"))

clean_lab <- penguins
dirty_lab <- penguins_raw

# remove unneeded columns -----

slim_dirty <- dirty_lab %>% 
  select(Species, Island, `Culmen Length (mm)`, `Culmen Depth (mm)`,
           `Flipper Length (mm)`, `Body Mass (g)`, Sex, `Date Egg`) %>% 
 # rename columns ----
  
   rename(species = Species,
        island = Island,
        bill_length_mm = `Culmen Length (mm)`,
        bill_depth_mm = `Culmen Depth (mm)`,
        flipper_length_mm = `Flipper Length (mm)`,
        body_mass_g = `Body Mass (g)`,
        sex = Sex, 
        year = `Date Egg`) %>%  
  
  # rename data to match clean file ----
  
  mutate(species = case_when(
        species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
        species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
        species == "Chinstrap penguin (Pygoscelis antarctica)" ~"Chinstrap",
        )) %>% 
  mutate(sex = case_when(
    sex == "MALE" ~ "male",
    sex == "FEMALE" ~ "female",
    .default = NA
  )) %>% 
  
  # change year from date to integer---- 
  
  mutate(year = as.integer(substring(year, 1 ,4))) %>% 
  mutate(year = as.numeric(year)) %>% 
  relocate(year, .after = sex) %>%
  mutate(sex = as_factor(sex))

  
#final check -----


all.equal(target = clean_lab, current = slim_dirty)
         
         
         
         
         
         
         
        