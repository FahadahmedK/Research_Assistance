rm(list = ls())
#setwd('D:/Work/Economic History/Wolf/Regional GDP')

library(xlsx)
library(haven)
library(foreign)
library(plyr)
library(dplyr)

df1 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 2, startRow = 6, endRow = 179)

colnames(df1) <- replace(colnames(df1), 1:15, c('Country', 'NUTS', 'Region',
                              '1900', '1910', '1925', '1938','1950', '1960','1970',
                              '1980', '1990', '2000', '2010', '2015'))



df2 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 3, startRow = 6, endRow = 179)

colnames(df2) <- replace(colnames(df2), 1:15, c('Country', 'NUTS', 'Region',
                                              '1900', '1910', '1925', '1938','1950', '1960','1970',
                                              '1980', '1990', '2000', '2010', '2015'))



df4 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 5, startRow = 6, endRow = 179)

colnames(df4) <- replace(colnames(df4), 1:15, c('Country', 'NUTS', 'Region',
                                              '1900', '1910', '1925', '1938','1950', '1960','1970',
                                              '1980', '1990', '2000', '2010', '2015'))

df5 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 6, startRow = 6, endRow = 179)

colnames(df5) <- replace(colnames(df5), 1:15, c('Country', 'NUTS', 'Region',
                                              '1900', '1910', '1925', '1938','1950', '1960','1970',
                                              '1980', '1990', '2000', '2010', '2015'))

df6 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 7, startRow = 6, endRow = 179)

colnames(df6) <- replace(colnames(df6), 1:15, c('Country', 'NUTS', 'Region',
                                              '1900', '1910', '1925', '1938','1950', '1960','1970',
                                              '1980', '1990', '2000', '2010', '2015'))

df7 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 8, startRow = 6, endRow = 179)

colnames(df7) <- replace(colnames(df7), 1:15, c('Country', 'NUTS', 'Region',
                                              '1900', '1910', '1925', '1938','1950', '1960','1970',
                                              '1980', '1990', '2000', '2010', '2015'))



df3 <- read.xlsx2('RosesWolf_RegionalGDP_v6.xlsx', sheetIndex = 4, startRow = 6, endRow = 179)


colnames(df3) <- replace(colnames(df3), 1:4, c('Country', 'NUTS', 'Region', '2010'))
                         


# GDP 1990

df_gdp90<- data.frame()

for(i in unique(df1[,'Country'])) {
  data <- df1[df1$Country ==i,]
  
  for(j in 4:length(colnames(df1))) {
    
    
    data_yearwise <-  data[, c(1,2,3,j)]

    colnames(data_yearwise) = c(colnames(data)[1:3], 'gdp_1990')
    
    df_gdp90 <- rbind(df_gdp90, data_yearwise)

  }
  
}

df_gdp90$Year = NA
for(i in unique(df1[,'Country'])) {
  data <- df_gdp90[df_gdp90$Country ==i,]
  
  no_regs <- length(unique(data$Region))
  
  df_gdp90[df_gdp90$Country == i,]$Year = rep(as.numeric(colnames(df1)[4:15]),c(rep(no_regs,12 ))) 
  
  
}  

# GDP 2011

df_gdp2011<- data.frame()

for(i in unique(df2[,'Country'])) {
  data <- df2[df2$Country ==i,]
  
  for(j in 4:length(colnames(df2))) {
    
    
    data_yearwise <-  data[, c(1,2,3,j)]
    
    colnames(data_yearwise) = c(colnames(data)[1:3], 'gdp_2011')
    
    df_gdp2011 <- rbind(df_gdp2011, data_yearwise)
    
  }
  
}

df_gdp2011$Year = NA
for(i in unique(df2[,'Country'])) {
  data <- df_gdp2011[df_gdp2011$Country ==i,]
  
  no_regs <- length(unique(data$Region))
  
  df_gdp2011[df_gdp2011$Country == i,]$Year = rep(as.numeric(colnames(df2)[4:15]),c(rep(no_regs,12 ))) 
  
  
}  

# POPULATION
df_pop<- data.frame()

for(i in unique(df4[,'Country'])) {
  data <- df4[df4$Country ==i,]
  
  for(j in 4:length(colnames(df4))) {
    
    
    data_yearwise <-  data[, c(1,2,3,j)]
    
    colnames(data_yearwise) = c(colnames(data)[1:3], 'population')
    
    df_pop <- rbind(df_pop, data_yearwise)
    
  }
  
}

df_pop$Year = NA
for(i in unique(df4[,'Country'])) {
  data <- df_pop[df_pop$Country ==i,]
  
  no_regs <- length(unique(data$Region))
  
  df_pop[df_pop$Country == i,]$Year = rep(as.numeric(colnames(df4)[4:15]),c(rep(no_regs,12 ))) 
  
  
}  

# EMP AGRI SHARE

df_agrishare <- data.frame()

for(i in unique(df5[,'Country'])) {
  data <- df5[df5$Country ==i,]
  
  for(j in 4:length(colnames(df5))) {
    
    
    data_yearwise <-  data[, c(1,2,3,j)]
    
    colnames(data_yearwise) = c(colnames(data)[1:3], 'emp_agrishare')
    
    df_agrishare <- rbind(df_agrishare, data_yearwise)
    
  }
  
}

df_agrishare$Year = NA
for(i in unique(df5[,'Country'])) {
  data <- df_agrishare[df_agrishare$Country ==i,]
  
  no_regs <- length(unique(data$Region))
  
  df_agrishare[df_agrishare$Country == i,]$Year = rep(as.numeric(colnames(df5)[4:15]),c(rep(no_regs,12 ))) 
  
  
}


#EMP IND SHARE

df_indshare <- data.frame()

for(i in unique(df6[,'Country'])) {
  data <- df6[df6$Country ==i,]
  
  for(j in 4:length(colnames(df6))) {
    
    
    data_yearwise <-  data[, c(1,2,3,j)]
    
    colnames(data_yearwise) = c(colnames(data)[1:3], 'emp_indshare')
    
    df_indshare <- rbind(df_indshare, data_yearwise)
    
  }
  
}

df_indshare$Year = NA
for(i in unique(df6[,'Country'])) {
  data <- df_indshare[df_indshare$Country ==i,]
  
  no_regs <- length(unique(data$Region))
  
  df_indshare[df_indshare$Country == i,]$Year = rep(as.numeric(colnames(df6)[4:15]),c(rep(no_regs,12 ))) 
  
  
}


#EM SERV SHARE

df_servshare <- data.frame()

for(i in unique(df7[,'Country'])) {
  data <- df7[df7$Country ==i,]
  
  for(j in 4:length(colnames(df7))) {
    
    
    data_yearwise <-  data[, c(1,2,3,j)]
    
    colnames(data_yearwise) = c(colnames(data)[1:3], 'emp_servshare')
    
    df_servshare <- rbind(df_servshare, data_yearwise)
    
  }
  
}

df_servshare$Year = NA
for(i in unique(df7[,'Country'])) {
  data <- df_servshare[df_servshare$Country ==i,]
  
  no_regs <- length(unique(data$Region))
  
  df_servshare[df_servshare$Country == i,]$Year = rep(as.numeric(colnames(df7)[4:15]),c(rep(no_regs,12 ))) 
  
  
}


final_regional <- Reduce(function(x,y) merge(x,y, all = TRUE), 
                        list(df_gdp90, df_gdp2011, df_pop, df_agrishare,
                             df_indshare, df_servshare))

final_regional$area = NA

for(i in unique(final_regional[,'Country'])) {
  
  data <- final_regional[final_regional$Country == i, ]
  
  for(j in unique(data$Region)) {
    data <- final_regional[final_regional$Country ==i & final_regional$Region ==j,]
    
    final_regional[final_regional$Country ==i & final_regional$Region ==j,]$area <-
      rep(df3[df3$Country ==i & df3$Region ==j,'2010'], nrow(data))
    
    
  }
    
}






final_regional = mutate(final_regional, id = 0)


for(i in 1:length(unique(final_regional[, 'NUTS']))) {
  final_regional[final_regional$NUTS == sort(unique(final_regional[, 'NUTS']))[i], ]$id = i 
}

final_regional <- final_regional %>% relocate(id) %>% 
  relocate(NUTS, .after = id) %>% relocate(Region, .after = NUTS) %>%
  arrange(id)

head(final_regional)

#write_dta(final_regional, 'RosesWolf_RegionalGDP.dta')

write.csv(final_regional, 'RosesWolf_Regional.csv')
