library(tidyverse)
library(DataExplorer)
library(lubridate)

train <- read.csv("train.csv")
glimpse(train)
summary(train)
#create_report(train)
#correct employee 1&2 variables
train$employee1 <- recode_factor(train$employee1, "-" = NA_character_)
levels(train$employee1)[levels(train$employee1)==''] <- NA

train$employee2 <- recode_factor(train$employee2, "-" = NA_character_)
levels(train$employee2)[levels(train$employee2)==''] <- NA

summary(train$employee1)
summary(train$employee2)

#correct instkind
levels(train$instkind)[levels(train$instkind)==''] <- NA
summary(train$instkind)

plot_missing(train)

# indices of rows (instances) containing NA values in them
row_indices <- 
  train %>% 
  map_df(is.na) %>%                              # turn into an is.na() matrix
  transmute(rowHasNAvalue = rowSums(.) > 0) %>%  # create variable indicating whether the row has an NA value
  as.matrix() %>%                                # convert into matrix (for the following 'which' function)
  which()  

train[row_indices,] %>% View('train_data_NA_values')

table(train$OC)

#working on openDate
train$openDate <- as.Date(as.character(train$openDate), format = "%Y%m%d")
summary(train$openDate)

train$openDateY <- year(train$openDate)
summary(train$openDateY)
ggplot(train, aes(x = OC, y = openDateY, fill = OC)) +
  geom_boxplot(varwidth = TRUE)

train$openDateM <- month(train$openDate)
summary(train$openDateM)
ggplot(train, aes(x = OC, y = openDateM, fill = OC)) +
  geom_boxplot(varwidth = TRUE)

train$openDateD <- day(train$openDate)
summary(train$openDateD)
ggplot(train, aes(x = OC, y = openDateD, fill = OC)) +
  geom_boxplot(varwidth = TRUE)

#bedCount
ggplot(train, aes(x = OC, y = bedCount, fill = OC)) +
  geom_boxplot(varwidth = TRUE) #we need to remove the outlier here to not affect the model

closedBedCount <- boxplot.stats(subset(train, OC != "open")$bedCount) 
closedBedCount

bedCountSum <- summary(subset(train, OC != "open")$bedCount)
train[which(train$OC != "open" & train$bedCount %in% closedBedCount$out),][["bedCount"]] <- bedCountSum[['Median']]
train[which(train$OC != "open" & is.na(train$bedCount)),][["bedCount"]] <- bedCountSum[['Median']]
summary(subset(train, OC != "open")$bedCount)

openedBedCount <- boxplot.stats(subset(train, OC == "open")$bedCount)  
openedBedCount

bedCountSum <- summary(subset(train, OC == "open")$bedCount)
train[which(train$OC == "open" & train$bedCount %in% openedBedCount$out),][["bedCount"]] <- bedCountSum[['Median']]
train[which(train$OC == "open" & is.na(train$bedCount)),][["bedCount"]] <- bedCountSum[['Median']]
summary(subset(train, OC == "open")$bedCount)

ggplot(train, aes(x = OC, y = bedCount, fill = OC)) +
  geom_boxplot(varwidth = TRUE)

#fields with 1 suffix
plot_correlation(train[-row_indices,8:31])
#log(revenue1)
train %>% 
  select(OC, revenue1) %>% 
  filter(!is.na(revenue1)) %>% 
  ggplot(aes(x = log(revenue1), fill = OC)) + 
  geom_histogram()
train$revenue1Log <- log(train$revenue1)
train %>% 
  select(OC, revenue1Log) %>% 
  filter(!is.na(revenue1Log)) %>% 
  ggplot(aes(x = OC, y = revenue1Log, fill = OC)) + 
  geom_boxplot()

#log(salescost1)
train %>% 
  select(OC, salescost1) %>% 
  filter(!is.na(salescost1)) %>% 
  filter(salescost1 > 0) %>% 
  ggplot(aes(x = log(salescost1), fill = OC)) + 
  geom_histogram(bins = 10)

train$salescost1log <- log(train$salescost1)

train %>% 
  select(OC, salescost1log) %>% 
  filter(!is.na(salescost1log)) %>% 
  filter(salescost1log != 0) %>% 
  #ggplot(aes(x = OC, y = log(salescost1), fill = OC)) + 
  ggplot(aes(x = OC, y = salescost1log, fill = OC)) + 
  geom_boxplot()

#log(sga1)
train %>% 
  select(OC, sga1) %>% 
  filter(!is.na(sga1)) %>% 
  ggplot(aes(x = log(sga1), fill = OC)) + 
  geom_histogram()
train$sga1log <- log(train$sga1)
train %>% 
  select(OC, sga1log) %>% 
  filter(!is.na(sga1log)) %>% 
  filter(sga1log != 0) %>% 
  ggplot(aes(x = OC, y = sga1log, fill = OC)) + 
  geom_boxplot()

#log(salary1)
train %>% 
  select(OC, salary1) %>% 
  filter(!is.na(salary1)) %>% 
  ggplot(aes(x = log(salary1), fill = OC)) + 
  geom_histogram()
train$salary1log <- log(train$salary1)
train %>% 
  select(OC, salary1log) %>% 
  filter(!is.na(salary1log)) %>% 
  filter(salary1log != 0) %>% 
  ggplot(aes(x = OC, y = salary1log, fill = OC)) + 
  geom_boxplot()

#log(noi1)
train %>% 
  select(OC, noi1) %>% 
  filter(!is.na(noi1)) %>% 
  ggplot(aes(x = log(noi1), fill = OC)) + 
  geom_histogram()
train$noi1log <- log(train$noi1)
train %>% 
  select(OC, noi1log) %>% 
  filter(!is.na(noi1log)) %>% 
  filter(noi1log != 0) %>% 
  ggplot(aes(x = OC, y = noi1log, fill = OC)) + 
  geom_boxplot()

#log(noe1)
train %>% 
  select(OC, noe1) %>% 
  filter(!is.na(noe1)) %>% 
  ggplot(aes(x = log(noe1), fill = OC)) + 
  geom_histogram()
train$noe1log <- log(train$noe1)
train %>% 
  select(OC, noe1log) %>% 
  filter(!is.na(noe1log)) %>% 
  filter(noe1log != 0) %>% 
  ggplot(aes(x = OC, y = noe1log, fill = OC)) + 
  geom_boxplot()

#log(interest1)
train %>% 
  select(OC, interest1) %>% 
  filter(!is.na(interest1)) %>% 
  ggplot(aes(x = log(interest1), fill = OC)) + 
  geom_histogram()
train$interest1log <- log(train$interest1)
train %>% 
  select(OC, interest1log) %>% 
  filter(!is.na(interest1log)) %>% 
  filter(interest1log != 0) %>% 
  ggplot(aes(x = OC, y = interest1log, fill = OC)) + 
  geom_boxplot()

#log(ctax1)
train %>% 
  select(OC, ctax1) %>% 
  filter(!is.na(ctax1)) %>% 
  ggplot(aes(x = log(ctax1), fill = OC)) + 
  geom_histogram(bins = 10)
train$ctax1log <- log(train$ctax1)
train %>% 
  select(OC, ctax1log) %>% 
  filter(!is.na(ctax1log)) %>% 
  filter(ctax1log != 0) %>% 
  ggplot(aes(x = OC, y = ctax1log, fill = OC)) + 
  geom_boxplot()


#log(profit1)
train %>% 
  select(OC, profit1) %>% 
  filter(!is.na(profit1)) %>% 
  ggplot(aes(x = log(profit1), fill = OC)) + 
  geom_histogram(bins = 10)
train$profit1log <- log(train$profit1)
train %>% 
  select(OC, profit1log) %>% 
  filter(!is.na(profit1log)) %>% 
  filter(profit1log != 0) %>% 
  ggplot(aes(x = OC, y = profit1log, fill = OC)) + 
  geom_boxplot()

#log(liquidAsset1)
train %>% 
  select(OC, liquidAsset1) %>% 
  filter(!is.na(liquidAsset1)) %>% 
  ggplot(aes(x = log(liquidAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$liquidAsset1log <- log(train$liquidAsset1)
train %>% 
  select(OC, liquidAsset1log) %>% 
  filter(!is.na(liquidAsset1log)) %>% 
  filter(liquidAsset1log != 0) %>% 
  ggplot(aes(x = OC, y = liquidAsset1log, fill = OC)) + 
  geom_boxplot()

#log(quickAsset1)
train %>% 
  select(OC, quickAsset1) %>% 
  filter(!is.na(quickAsset1)) %>% 
  ggplot(aes(x = log(quickAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$quickAsset1log <- log(train$quickAsset1)
train %>% 
  select(OC, quickAsset1log) %>% 
  filter(!is.na(quickAsset1log)) %>% 
  filter(quickAsset1log != 0) %>% 
  ggplot(aes(x = OC, y = quickAsset1log, fill = OC)) + 
  geom_boxplot()



#log(inventoryAsset1)
train %>% 
  select(OC, inventoryAsset1) %>% 
  filter(!is.na(inventoryAsset1)) %>% 
  ggplot(aes(x = log(inventoryAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$inventoryAsset1 <- log(train$inventoryAsset1)
train %>% 
  select(OC, inventoryAsset1) %>% 
  filter(!is.na(inventoryAsset1)) %>% 
  filter(inventoryAsset1 != 0) %>% 
  ggplot(aes(x = OC, y = inventoryAsset1, fill = OC)) + 
  geom_boxplot()

#log(nonCAsset1)
train %>% 
  select(OC, nonCAsset1) %>% 
  filter(!is.na(nonCAsset1)) %>% 
  ggplot(aes(x = log(nonCAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$nonCAsset1 <- log(train$nonCAsset1)
train %>% 
  select(OC, nonCAsset1) %>% 
  filter(!is.na(nonCAsset1)) %>% 
  filter(nonCAsset1 != 0) %>% 
  ggplot(aes(x = OC, y = nonCAsset1, fill = OC)) + 
  geom_boxplot()

#log(tanAsset1)
train %>% 
  select(OC, tanAsset1) %>% 
  filter(!is.na(tanAsset1)) %>% 
  ggplot(aes(x = log(tanAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$tanAsset1 <- log(train$tanAsset1)
train %>% 
  select(OC, tanAsset1) %>% 
  filter(!is.na(tanAsset1)) %>% 
  filter(tanAsset1 != 0) %>% 
  ggplot(aes(x = OC, y = tanAsset1, fill = OC)) + 
  geom_boxplot()

#log(OnonCAsset1)
train %>% 
  select(OC, OnonCAsset1) %>% 
  filter(!is.na(OnonCAsset1)) %>% 
  ggplot(aes(x = log(OnonCAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$OnonCAsset1 <- log(train$OnonCAsset1)
train %>% 
  select(OC, OnonCAsset1) %>% 
  filter(!is.na(OnonCAsset1)) %>% 
  filter(OnonCAsset1 != 0) %>% 
  ggplot(aes(x = OC, y = OnonCAsset1, fill = OC)) + 
  geom_boxplot()

#receivableL1
train$receivableL1 <- NULL


#log(debt1)
train %>% 
  select(OC, debt1) %>% 
  filter(!is.na(debt1)) %>% 
  ggplot(aes(x = log(debt1), fill = OC)) + 
  geom_histogram(bins = 10)
train$debt1 <- log(train$debt1)
train %>% 
  select(OC, debt1) %>% 
  filter(!is.na(debt1)) %>% 
  filter(debt1 != 0) %>% 
  ggplot(aes(x = OC, y = debt1, fill = OC)) + 
  geom_boxplot()

#log(liquidLiabilities1)
train %>% 
  select(OC, liquidLiabilities1) %>% 
  filter(!is.na(liquidLiabilities1)) %>% 
  ggplot(aes(x = log(liquidLiabilities1), fill = OC)) + 
  geom_histogram(bins = 10)
train$liquidLiabilities1 <- log(train$liquidLiabilities1)
train %>% 
  select(OC, liquidLiabilities1) %>% 
  filter(!is.na(liquidLiabilities1)) %>% 
  filter(liquidLiabilities1 != 0) %>% 
  ggplot(aes(x = OC, y = liquidLiabilities1, fill = OC)) + 
  geom_boxplot()

#log(shortLoan1)
train %>% 
  select(OC, shortLoan1) %>% 
  filter(!is.na(shortLoan1)) %>% 
  ggplot(aes(x = log(shortLoan1), fill = OC)) + 
  geom_histogram(bins = 10)
train$shortLoan1 <- log(train$shortLoan1)
train %>% 
  select(OC, shortLoan1) %>% 
  filter(!is.na(shortLoan1)) %>% 
  filter(shortLoan1 != 0) %>% 
  ggplot(aes(x = OC, y = shortLoan1, fill = OC)) + 
  geom_boxplot()

#log(NCLiabilities1)
train %>% 
  select(OC, NCLiabilities1) %>% 
  filter(!is.na(NCLiabilities1)) %>% 
  ggplot(aes(x = log(NCLiabilities1), fill = OC)) + 
  geom_histogram(bins = 10)
train$NCLiabilities1 <- log(train$NCLiabilities1)
train %>% 
  select(OC, NCLiabilities1) %>% 
  filter(!is.na(NCLiabilities1)) %>% 
  filter(NCLiabilities1 != 0) %>% 
  ggplot(aes(x = OC, y = NCLiabilities1, fill = OC)) + 
  geom_boxplot()

#log(longLoan1)
train %>% 
  select(OC, longLoan1) %>% 
  filter(!is.na(longLoan1)) %>% 
  ggplot(aes(x = log(longLoan1), fill = OC)) + 
  geom_histogram(bins = 10)
train$longLoan1 <- log(train$longLoan1)
train %>% 
  select(OC, longLoan1) %>% 
  filter(!is.na(longLoan1)) %>% 
  filter(longLoan1 != 0) %>% 
  ggplot(aes(x = OC, y = longLoan1, fill = OC)) + 
  geom_boxplot()

#log(netAsset1)
train %>% 
  select(OC, netAsset1) %>% 
  filter(!is.na(netAsset1)) %>% 
  ggplot(aes(x = log(netAsset1), fill = OC)) + 
  geom_histogram(bins = 10)
train$netAsset1 <- log(train$netAsset1)
train %>% 
  select(OC, netAsset1) %>% 
  filter(!is.na(netAsset1)) %>% 
  filter(netAsset1 != 0) %>% 
  ggplot(aes(x = OC, y = netAsset1, fill = OC)) + 
  geom_boxplot()

#log(surplus1)
train %>% 
  select(OC, surplus1) %>% 
  filter(!is.na(surplus1)) %>% 
  ggplot(aes(x = log(surplus1), fill = OC)) + 
  geom_histogram(bins = 10)
train$surplus1 <- log(train$surplus1)
train %>% 
  select(OC, surplus1) %>% 
  filter(!is.na(surplus1)) %>% 
  filter(surplus1 != 0) %>% 
  ggplot(aes(x = OC, y = surplus1, fill = OC)) + 
  geom_boxplot()