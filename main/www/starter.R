# install tidyverse and remotes if not available
if (!require("tidyverse", character.only = TRUE))
  install.packages("tidyverse", dependencies = TRUE, 
                   repos = "http://cran.us.r-project.org")
# load data
soccer <- read.csv("https://raw.githubusercontent.com/mjmoon/sta490-soccer/main/data/soccer.csv")
library(tidyverse)

# create variable
soccer <- soccer %>%
  mutate(skintone = (rater1 + rater2) / 2)

################################################################
# Conduct the multiverse analysis using `soccer` data set here #
################################################################

