#####################################################################################
# Set-up the environment

## Set-up the Directories
repoDir <- "C:/Users/Joanna/Dropbox/Repositories/ACS_Share-of-Earnings" # This should be your master project folder (Project GitRepository)
subDir1 <- "data" # This will be the name of the folder where data output goes
subDir2 <- "figures" # This will be the name of the folder where figures are saved
dataDir <- file.path(repoDir, subDir1)
figDir  <- file.path(repoDir, subDir2)

## This will create sub-directory data folder in the master project directory if doesn't exist
if (!dir.exists(dataDir)){
  dir.create(dataDir)
} else {
  print("data directory already exists!")
}

## This will create sub-directory figures folder in the master project directory if doesn't exist
if (!dir.exists(figDir)){
  dir.create(figDir)
} else {
  print("figure directory already exists!")
}

setwd(file.path(repoDir)) # Set the working-directory to the master project folder

## Create a data extract using CPS
# Create a variable within the IPUMS data extract system that contains the income of a respondent's spouse by using
# the Attach Characteristics option. To do so, you should first select your samples and variables,
# which must contain INCTOT. Before submitting your extract, you will be given the option to choose "Attach characteristics" on the 
# extract request screen. Check the box for "Spouse" on the INCTOT row. This will add a variable to your data extract request called
# INCTOT_SP. Now simply submit your extract. You should then add up inctot and inctot_sp for one spouse member.

# Samples:    Respondents - 1960, 1970, 1980, 1990, 2000, 2001-2017
# Variables:
# "year"      "datanum"    "hhwt"    "eldch"    "sex"     "age"    "marst"   "inctot"   
# "sex_sp"    "inctot_sp"


## Set up instructions for importing the data 
# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
# Updated ATUS Data

## Load libraries
library(ipumsr)
library(tidyverse, warn.conflicts = FALSE)
library(questionr)
library(ggplot2)

## Load ATUS Data into R
ddi <- read_ipums_ddi("usa_00013.xml") # This assumes your data extract was saved in the repoDir folder.
data <- read_ipums_micro(ddi)

## Make the variable names lowercase
data <- data %>% rename_all(tolower)

#####################################################################################
# Clean the data

## Change class from labelled
lapply(data, class) # Preview which variables are labelled

data <- data %>% # Did this in multiple steps for computer memory purposes.
  mutate( eldch      = as.integer(lbl_clean(eldch)),
          sex        = as_factor(lbl_clean(sex)),
          sex_sp     = as_factor(lbl_clean(sex_sp)))

data <- data %>%
  mutate( age        = as.integer(lbl_clean(age)),
          marst      = as_factor(lbl_clean(marst)))

data <- data %>%
  mutate( inctot     = as.numeric(lbl_clean(inctot)))

data <- data %>%
  mutate( inctot_sp  = as.numeric(lbl_clean(inctot_sp)))

earndat <- data # Create a new dataset in case anything goes wrong

#####################################################################################
# Measures & Sample

## Age of Eldest child in household
earndat <- earndat %>%
  mutate(
    kidu18 = case_when(
      eldch <  17 ~ 1L,
      eldch >= 18 ~ 0L))

earndat <- earndat %>% ## Keep only households with kid u 18 in HH
  filter(kidu18 == 1)

## Limit to 1 person in the household
 earndat <- earndat %>%
  filter(pernum == 1)

## Marital status
earndat <- earndat %>%
  mutate(
    marsolo = case_when(
      marst == "Married, spouse present" | marst == "Married, spouse absent" ~ "Married",
      marst == "Never married/single"    | marst == "Separated" |
      marst == "Divorced"                | marst == "Widowed"                ~ "Solo",
      TRUE                                                                   ~  NA_character_ 
    ))

## Breadwinner
# 9999998 Missing. 
# 9999999 = N.I.U. (Not in Universe).

earndat$inctot[earndat$inctot >= 9999998] <- NA
earndat$inctot_sp[earndat$inctot_sp >= 9999998] <- NA

earndat$inctot[earndat$inctot >= 9999999] <- 0
earndat$inctot_sp[earndat$inctot_sp >= 9999999] <- 0

### keep respondents with non-negative incomes & couples with positive total income
earndat <- earndat %>%
  mutate(
    nonneg = case_when(
      (inctot + inctot_sp) >=0 ~ 1,
      inctot               >=0 ~ 1,
      TRUE                     ~ 0))

earndat <- earndat %>%
  filter(nonneg == 1)

## Create breadwinning categories (50% threshold)
earndat <- earndat %>%
  mutate(
    bwcat = case_when(
      marsolo == "Solo"    & sex == "Female"                             ~ "SoloFBW",
      marsolo == "Married" & sex == "Female" & ((inctot/inctot_sp) > .5) ~ "MarFBW",
      marsolo == "Married" & sex == "Male"   & ((inctot/inctot_sp) < .5) ~ "MarFBW",
      TRUE                                                               ~ "NotFBW"
    ))

## Descriptives
freq <- data.frame(wtd.table(earndat$year, earndat$bwcat, weights = earndat$hhwt, digits = 2))

earnavg <- freq  %>%
  group_by(Var1, Var2) %>%
  summarise(n = sum(Freq)) %>%
  mutate(percentage = n / sum(n))

earnavg$Var1 <- as.character(earnavg$Var1)
earnavg$Var1 <- as.numeric(earnavg$Var1)

## Figure
fig <- earnavg %>%
  filter(Var2 != "NotFBW" & (Var1<= 2000 | Var1 == 2010 | Var1==2017)) %>%
  ggplot((aes(x = Var1,  y = percentage, fill = Var2))) +
  geom_area(color = "black") +
  geom_line(position="stack", linetype="dashed", size = 1.2,  color = c("white")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .45)) +
  scale_x_continuous(limits = c(1960, 2017), breaks = c(1960,1970,1980,1990,2000,2010,2017)) +
  scale_fill_manual(name="",
                    breaks=c("MarFBW", "SoloFBW"),
                    labels=c("Married-couple families",
                             "Mother-only families"),
                    values=c("#666767", "#CA5462")) +
  labs(title = "Mothers as primary or sole earners, 1960-2017",
          subtitle = "Percent of households with children under age 18 \nin which mothers are the primary or sole earner") +
  labs(caption = "Data source: 1960-2000 Decennial Census \n2010-2017 American Community Surveys") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        legend.justification = "top",
        legend.text   = element_text(size=16),
        plot.title    = element_text(size = 20, face = "bold"),
        axis.text     = element_text(size = 14))

fig

ggsave("figures/momearn.png", fig, width = 10, height = 6, dpi = 300)