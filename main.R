library(tidyverse)
library(DataExplorer)

train <- read.csv("train.csv")
glimpse(train)
create_report(train)
