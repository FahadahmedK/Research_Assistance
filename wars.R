library(dplyr)
#library(Xlsx)

rm(list = ls())

#setting working directory containing data on wars (https://correlatesofwar.org/data-sets/MIDs)

#setwd('P:/wg/10 Research/Frequency of Wars/MID 5.0')

data_MID <- read.csv('MIDB 5.0.csv', header = TRUE)
data_MID <- data_MID[order(data_MID$styear), ]

#homogenizing the starting year for participants

for(i in 1:length(unique(data_MID$dispnum))) {
  data_frame <- data_MID[data_MID$dispnum == unique(data_MID$dispnum)[i],]
  mat <- as.matrix(table(data_frame$disp, data_frame$styear))
  data_frame$styear <- replace(data_frame$styear, 1:nrow(data_frame), as.numeric(colnames(mat)[which(mat[1,] == max(mat[1,]))]))
  data_MID[data_MID$dispnum == unique(data_MID$dispnum)[i],] <- data_frame
}


#number of disputes conditional on hostlev3&above
cond_hostlev3 <-  data_MID[data_MID$hostlev >= 3,]
cond_hostlev3 <- cond_hostlev3[order(cond_hostlev3$styear),]
numdisp_hostlev3 <- data.frame('styear' = unique(cond_hostlev3$styear), 'numdisp' = NA)
for(i in 1:nrow(numdisp_hostlev3)) {
  year = numdisp_hostlev3$styear[i]
  data <- cond_hostlev3[cond_hostlev3$styear == year,]
  numdisp_hostlev3[i, 'numdisp'] <- length(unique(data$dispnum))
}

write.xlsx(numdisp_hostlev3, file = 'results_numdisp.xlsx',
           sheetName = 'host3above', row.names = FALSE)


#number of disputes conditional on hostlev4&above

cond_hostlev4 <-  data_MID[data_MID$hostlev >= 4,]
cond_hostlev4 <- cond_hostlev4[order(cond_hostlev4$styear),]
numdisp_hostlev4 <- data.frame('styear' = unique(cond_hostlev4$styear), 'numdisp' = NA)
for(i in 1:nrow(numdisp_hostlev4)) {
  year = numdisp_hostlev4$styear[i]
  data <- cond_hostlev4[cond_hostlev4$styear == year,]
  numdisp_hostlev4[i, 'numdisp'] <- length(unique(data$dispnum))
}

write.xlsx(numdisp_hostlev4, file = 'results_numdisp.xlsx',
           sheetName = 'host4above', append = TRUE,  row.names = FALSE)


a<- data_MID[data_MID$hostlev >=4, ]
a <- a[a$styear == 2014,]
length(unique(a$dispnum))

data_MID[data_MID$dispnum == 4604,]


numdisp_hostlev4

#number of disputes conditional on fatality level 4&above

cond_fatlev4 <-  data_MID[data_MID$fatality >=4,]
cond_fatlev4 <- cond_fatlev4[order(cond_fatlev4$styear),]
numdisp_fatlev4 <- data.frame('styear' = unique(cond_fatlev4$styear), 'numdisp' = NA)
for(i in 1:nrow(numdisp_fatlev4)) {
  year = numdisp_fatlev4$styear[i]
  data <- cond_fatlev4[cond_fatlev4$styear == year,]
  numdisp_fatlev4[i, 'numdisp'] <- length(unique(data$dispnum))
}

write.xlsx(numdisp_fatlev4, file = 'results_numdisp.xlsx',
           sheetName = 'fat4above', append = TRUE,  row.names = FALSE)


#number of disputes conditional on fatality level 4 and hostlev3 and above

cond_fat4_host3 <-  data_MID[data_MID$fatality ==4 & data_MID$hostlev >=3,]
cond_fat4_host3 <- cond_fat4_host3[order(cond_fat4_host3$styear),]
numdisp_fat4_host3 <- data.frame('styear' = unique(cond_fat4_host3$styear), 'numdisp' = NA)
for(i in 1:nrow(numdisp_fat4_host3)) {
  year = numdisp_fat4_host3$styear[i]
  data <- cond_fat4_host3[cond_fat4_host3$styear == year,]
  numdisp_fat4_host3[i, 'numdisp'] <- length(unique(data$dispnum))
}


write.xlsx(numdisp_fat4_host3, file = 'results_numdisp.xlsx',
           sheetName = 'host3above_fat4', append = TRUE,  row.names = FALSE)



#number of disputes conditional on fatality level 4 and hostlev4 and above


cond_fat4_host4 <-  data_MID[data_MID$fatality >=4 & data_MID$hostlev >=4,]
cond_fat4_host4 <- cond_fat4_host4[order(cond_fat4_host4$styear),]
numdisp_fat4_host4 <- data.frame('styear' = unique(cond_fat4_host4$styear), 'numdisp' = NA)
for(i in 1:nrow(numdisp_fat4_host4)) {
  year = numdisp_fat4_host4$styear[i]
  data <- cond_fat4_host4[cond_fat4_host4$styear == year,]
  numdisp_fat4_host4[i, 'numdisp'] <- length(unique(data$dispnum))
}

write.xlsx(numdisp_fat4_host4, file = 'results_numdisp.xlsx',
           sheetName = 'host4above_fat4above', append = TRUE,  row.names = FALSE)


#matching hostlevel for countries involved in the same dispute

head(data)
tail(data)

data <- data_MID

for(i in 1:length(unique(data$dispnum))) {
  data_frame <- data[data$dispnum == unique(data$dispnum)[i],]
  data_frame$hostlev <- replace(data_frame$hostlev, 1:nrow(data_frame), max(data_frame$hostlev))
  data[data$dispnum == unique(data$dispnum)[i],] <- data_frame
}

#number of participants per dispute per year cond on hostlev3

cond_hostlev3 <- data[data$hostlev >=3,]
numpart_hostlev3 <- data.frame('year' = NA, 'dispnum' = unique(cond_hostlev3$dispnum), 'numpart' = NA)

for(i in 1:nrow(numpart_hostlev3)) {
  data_hostlev3 <- cond_hostlev3[cond_hostlev3$dispnum == numpart_hostlev3$dispnum[i],]
  numpart_hostlev3[i, 'numpart'] = nrow(data_hostlev3)
  numpart_hostlev3[i, 'year'] = min(data_hostlev3$styear)
}

write.xlsx(numpart_hostlev3, file = 'results_numpart.xlsx',
           sheetName = 'host3above', append = TRUE,  row.names = FALSE)


#number of participants per dispute per year cond on hostlev4 and above


cond_hostlev4 <- data[data$hostlev >=4,]
numpart_hostlev4 <- data.frame('year' = NA, 'dispnum' = unique(cond_hostlev3$dispnum), 'numpart' = NA)

for(i in 1:nrow(numpart_hostlev4)) {
  data_hostlev4 <- cond_hostlev4[cond_hostlev3$dispnum == numpart_hostlev4$dispnum[i],]
  numpart_hostlev4[i, 'numpart'] = nrow(data_hostlev4)
  numpart_hostlev4[i, 'year'] = min(data_hostlev4$styear)
}

write.xlsx(numpart_hostlev4, file = 'results_numpart.xlsx',
           sheetName = 'host4above', append = TRUE,  row.names = FALSE)



#assigning max fatality level

head(data_MID)
head(data)

for(i in 1:length(unique(data$dispnum))) {
  data_frame <- data[data$dispnum == unique(data$dispnum)[i],]
  data_frame$fatality <- replace(data_frame$fatality, 1:nrow(data_frame), max(data_frame$fatality))
  data[data$dispnum == unique(data$dispnum)[i],] <- data_frame
}

#number of participants conditional on fatality level 4 and above

cond_fatlev4 <- data[data$fatality >=4,]
numpart_fatlev4 <- data.frame('year' = NA, 'dispnum' = unique(cond_fatlev4$dispnum), 'numpart' = NA)

for(i in 1:nrow(numpart_fatlev4)) {
  data_fatlev4 <- cond_fatlev4[cond_fatlev4$dispnum == numpart_fatlev4$dispnum[i],]
  numpart_fatlev4[i, 'numpart'] = nrow(data_fatlev4)
  numpart_fatlev4[i, 'year'] = min(data_fatlev4$styear)
}

write.xlsx(numpart_fatlev4, file = 'results_numpart.xlsx',
           sheetName = 'fatlev4', append = TRUE,  row.names = FALSE)

#number of participants conditional on fatality level 4 and above and hostlev3 and above 

cond_fat4_host3 <- data[data$fatality ==4 & data$hostlev>=3,]
numpart_fat4_host3<- data.frame('year' = NA, 'dispnum' = unique(cond_fat4_host3$dispnum), 'numpart' = NA)

for(i in 1:nrow(numpart_fat4_host3)) {
  data_fat4_host3 <- cond_fat4_host3[cond_fat4_host3$dispnum == numpart_fat4_host3$dispnum[i],]
  numpart_fat4_host3[i, 'numpart'] = nrow(data_fat4_host3)
  numpart_fat4_host3[i, 'year'] = min(data_fat4_host3$styear)
}

write.xlsx(numpart_fat4_host3, file = 'results_numpart.xlsx',
           sheetName = 'fat4_host3above', append = TRUE,  row.names = FALSE)


#number of participants conditional on fatlev4 and above and hostlev3 and above 

cond_fat4above_host4 <- data[data$fatality >=4 & data$hostlev>=4,]
numpart_fat4above_host4<- data.frame('year' = NA, 'dispnum' = unique(cond_fat4above_host4$dispnum), 'numpart' = NA)

for(i in 1:nrow(numpart_fat4above_host4)) {
  data_fat4above_host4 <- cond_fat4above_host4[cond_fat4above_host4$dispnum == numpart_fat4above_host4$dispnum[i],]
  numpart_fat4above_host4[i, 'numpart'] = nrow(data_fat4above_host4)
  numpart_fat4above_host4[i, 'year'] = min(data_fat4above_host4$styear)
}

write.xlsx(numpart_fat4above_host4, file = 'results_numpart.xlsx',
           sheetName = 'fat4above_host3above', append = TRUE,  row.names = FALSE)
