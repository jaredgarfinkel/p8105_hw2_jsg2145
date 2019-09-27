mr_trash = readxl::read_excel("./data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", 
                              sheet = "Mr. Trash Wheel", 
                              range = readxl::cell_cols("A:N")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(dumpster)) %>% 
  mutate(
    sports_balls = 
      round(sports_balls, digits = 0) %>% 
      as.integer())
mr_trash %>% 
  ggplot(aes(x = date, y = polystyrene)) + 
  geom_col()
c(1,7:9)
x <- 1:4
names(x) <- letters[1:4]
x
c(x)
as.vector(x)
dim(x) <- c(2,2)
x
c(x)
as.vector(x)
ll <- list(A = 1, c = "C")
c(ll, d= list(1:3))
c(list(A = c(B =1)), recursive = TRUE)
str(mr_trash$dumpster)
library(tidyverse)
library(ggridges)
skim(mr_trash)

?skim
group_by(mr_trash, variable) %>% 
  ggplot(aes(x = year)) +
  geom_density_ridges()

?desc
str(mr_trash$variable)
tbl <- arsenal::tableby(year ~ chip_bags + cigarette_butts + glass_bottles + grocery_bags + homes_powered + plastic_bottles + polystyrene + volume_cubic_yards + weight_tons, data = mr_trash)
tbl %>% 
  ggplot(aes(x = cigarette butts)) +
  
tbl_chip <- mr_trash %>%
  ggplot(aes(x = year, y = chip_bags)) +
  geom_col()
tbl_cigarettes <- mr_trash %>% 
  ggplot(aes(x = year, y = cigarette_butts)) +
  geom_col()
tbl_glass <- mr_trash %>% 
  ggplot(aes(x = year, y = glass_bottles)) +
  geom_col()
tbl_bags <- mr_trash %>%
  ggplot(aes(x = year, y = grocery_bags)) +
  geom_col()
tbl_homes <- mr_trash %>%
  ggplot(aes(x = year, y = homes_powered)) +
  geom_col()
tbl_plastic <- mr_trash %>%
  ggplot(aes(x = year, y = plastic_bottles)) +
  geom_col()
tbl_poly <- mr_trash %>%
  ggplot(aes(x = year, y = polystyrene)) +
  geom_col()
tbl_vol <- mr_trash %>%
  ggplot(aes(x = year, y = volume_cubic_yards)) +
  geom_col()
tbl_weight <- mr_trash %>%
  ggplot(aes(x = year, y = weight_tons)) +
  geom_col()
tbl_balls <- mr_trash %>% 
  ggplot(aes(x = year, y = sports_balls)) +
  geom_col()

dump <- mr_trash %>% 
  group_by(year, dumpster) %>% 
  mutate(count = n()) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_col()
dump

tbl_chip
tbl_cigarettes
tbl_glass
tbl_bags
tbl_homes
tbl_plastic
tbl_poly
tbl_vol
tbl_weight
tbl_balls
precip1718 %>% 
  filter(year == 2017) %>% 
  mutate(months = factor(month, levels = month.name)) %>% 
  ggplot(aes(x = months, y = total)) +
  geom_col()
  
precip1718 %>% 
  filter(year == 2018) %>% 
  mutate(months = factor(month, levels = month.name)) %>% 
  ggplot(aes(x = months, y = total)) +
  geom_col()
