
# Install and load data

library(tidyverse)
library(datasauRus)

# Have a look at the data
datasaurus_dozen_wide
data("datasaurus_dozen_wide")
datasauRus::twelve_from_slant_wide

# Obtain info
?datasaurus_dozen_wide



#Create a section by Control+Shift+R
# Part 1 ------------------------------------------------------------------

summary(datasaurus_dozen_wide)
glimpse(datasaurus_dozen_wide)

# Command + shift + m: %>% --> pipe operator
datasaurus_dozen_wide %>% 
  ggplot() + 
  geom_point(mapping = aes(x=away_x, y=away_y))


# Tidying Data ------------------------------------------------------------
#column = variable and row = observation

df <- datasaurus_dozen_wide %>%
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -id,
               names_to = c("dataset","var_name"),
               values_to = "value",
               names_pattern = "^(.*)_(.*)$") %>% 
  pivot_wider(names_from = var_name,values_from = value) %>% 
  select(-id)

# Summary statistics ------------------------------------------------------

#names of the dataset
df %>% 
  distinct(dataset)

dataset_list <- c("dino","star","bullseye","away")

df_filter <- df %>% 
  filter(dataset == "dino" %in% dataset_list)

df %>% 
  filter(dataset=="dino") %>% 
  summarise(mean_x=mean(x),
            sd_x=sd(x))

df %>%  
  filter(dataset %in% dataset_list) %>% 
  group_by(dataset) %>% 
  summarise(across(c(x,y),list(mean=mean,sd=sd)))

df %>%  
  filter(dataset %in% dataset_list) %>% 
  group_by(dataset) %>% 
  summarise(across(c(x, y), list(mean = mean, sd = sd)), slope = coef(lm(y ~ x))[2])

# Plotting ----------------------------------------------------------------

df %>% 
  filter(dataset %in% dataset_list) %>% 
  ggplot()+
  geom_point(aes(x,y))+
  facet_wrap(vars(dataset))

df %>% 
  filter(dataset %in% dataset_list) %>% 
  ggplot(aes(x,y,color = dataset)) +
  geom_point() +
  geom_smooth(method = "lm",se=FALSE, color= "black") +
  facet_wrap(vars(dataset))


# Part 2: The titanic dataset ------------------------------------------------------------------

#use assignment side
titanic <- read_csv("/Users/ingridcanelles/Documents/GitHub/statcomp/datasets/titanic.csv")

# Gender vs survival
titanic = titanic %>% 
  mutate(across(c(Survived,Pclass,Sex),as.factor))

titanic %>% 
  ggplot()+
  geom_bar(aes(y=Sex,fill=Survived),position = "dodge2")+
  labs(title = "Survival Comparison: Male vs Female",
       x = "Count of Passengers",
       y = "Gender")

# Survival by gender and class

titanic %>% 
  ggplot() +
  geom_bar(aes(y = Sex, fill = Survived), position = "dodge2") +
  facet_wrap(~Pclass) + 
  # Apply a clean theme and descriptive titles
  theme_minimal() +
  labs(title = "Survival by Gender across Passenger Classes",
       x = "Count of Passengers",
       y = "Gender")

# Age distribution

titanic %>% 
  filter(!is.na(Age)) %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution of Survivors vs Non-Survivors",
       x = "Age (Years)",
       y = "Density")

# Fare and survival

titanic %>% 
  ggplot(aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Survival Outcome based on Ticket Fare",
       y = "Fare Price (Log Scale)",
       x = "Survived")

Interpretation: From the gender vs survival plot, women were much more likely to survive than men (most men did not survive).
Splitting by passenger class (Pclass) keeps the same pattern: 1st- and 2nd-class women show high survival, while 3rd-class women have lower survival than the upper classes.
For men, 3rd class concentrates the highest number of deaths; even in 1st and 2nd class, male survival is low compared to females.
The age distribution suggests a weaker effect: survivors skew slightly toward younger/adult ages, while older ages appear more among non-survivors.
The fare (ticket price) boxplot indicates that survivors generally paid higher fares, suggesting that higher socioeconomic status / class increased survival chances.









