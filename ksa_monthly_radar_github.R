install.packages("fmsb")
library("fmsb")

# Reading in the data from .csv file. This .csv contains approximately 700 rows, each of which contains information on a discrete individual executed in Saudi Arabia between 2014 and 2019
KSA <- read.csv ("KSA_individuals_csv.csv", header=TRUE, sep=",")
  
# Converting date of execution column to date type
KSA$Date.of.execution <- as.Date (KSA$Date.of.execution, "%m/%d/%Y")
    
# Creating execution month and year columns
KSA$Execution.month <- format(KSA$Date.of.execution, "%m")
KSA$Execution.year <- format(KSA$Date.of.execution, "%Y")
      
# Converting month and year columns to numeric so the for loop below doesn't exclude months 01-09
KSA$Execution.month <- as.numeric(KSA$Execution.month)
KSA$Execution.year <- as.numeric(KSA$Execution.year)
      
# Creating five empty lists of 12 NULL values, to be filled by the for loop
ksa_years_list_2014 <- vector("list", 12)
ksa_years_list_2015 <- vector("list", 12)
ksa_years_list_2016 <- vector("list", 12)
ksa_years_list_2017 <- vector("list", 12)
ksa_years_list_2018 <- vector("list", 12)
ksa_years_list_2019 <- vector("list", 12)
      
# for loop that fills each slot of the empty lists with the length of the execution month column (i.e. number of executions) for each month
for (i in 1:12) {ksa_years_list_2014[[i]] <- length((subset(KSA,
                                                            KSA$Execution.month == i & KSA$Execution.year == 2014))$Execution.month)
          
ksa_years_list_2015[[i]] <- length((subset(KSA,
                                           KSA$Execution.month == i & KSA$Execution.year == 2015))$Execution.month)
          
ksa_years_list_2016[[i]] <- length((subset(KSA,
                                           KSA$Execution.month == i & KSA$Execution.year == 2016))$Execution.month)
          
ksa_years_list_2017[[i]] <- length((subset(KSA,
                                           KSA$Execution.month == i & KSA$Execution.year == 2017))$Execution.month)
          
ksa_years_list_2018[[i]] <- length((subset(KSA,
                                           KSA$Execution.month == i & KSA$Execution.year == 2018))$Execution.month)
          
ksa_years_list_2019[[i]] <- length((subset(KSA,
                                           KSA$Execution.month == i & KSA$Execution.year == 2019))$Execution.month)
}
      
# Turning each list into a dataframe
ksa_2014_ex <- data.frame(ksa_years_list_2014)
ksa_2015_ex <- data.frame(ksa_years_list_2015)
ksa_2016_ex <- data.frame(ksa_years_list_2016)
ksa_2017_ex <- data.frame(ksa_years_list_2017)
ksa_2018_ex <- data.frame(ksa_years_list_2018)
ksa_2019_ex <- data.frame(ksa_years_list_2019)

# Renaming the dataframe columns
colnames (ksa_2014_ex) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (ksa_2015_ex) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (ksa_2016_ex) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (ksa_2017_ex) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (ksa_2018_ex) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (ksa_2019_ex) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      
# Combining all the dataframe rows into one dataframe
ksa_final <- rbind(ksa_2014_ex, ksa_2015_ex, ksa_2016_ex, ksa_2017_ex, ksa_2018_ex, ksa_2019_ex)
      
# Renaming the new dataframe rows
rownames (ksa_final) <- c(2014, 2015, 2016, 2017, 2018, 2019)
      
# To use radarchart(), the df has to have max and min values in each column. The below adds max and min rows of 0 and 60, each repeating 12 times
ksa_final <- rbind(ksa_final, (rep.int(60, 12)))
ksa_final <- rbind(ksa_final, (rep.int(0, 12)))
      
# Rearranging the rows because the pattern of rows for radarchart() to work has to be max, min, data:
ksa_final <- ksa_final[c(7,8,6,5,4,3,2,1),]
      
      
# Making the chart matrix
par(mar=c(1, 1, 1, 1), #decrease default margin
    oma=c(0,0,5,0),
    bg = "floralwhite",
    cex = 1,
    col="gray20")

layout(matrix(1:6, ncol=2))

radarchart(ksa_final[c(1,2,3),],
           title = 2019,
           axistype=1,
           caxislabels=c("","",30,"",60),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)

radarchart(ksa_final[c(1,2,5),],
           title = 2017,
           axistype=1,
           caxislabels=c("","",30,"",60),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)

radarchart(ksa_final[c(1,2,7),],
           title = 2015,
           axistype=1,
           caxislabels=c("","",30,"",60),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)

radarchart(ksa_final[c(1,2,4),],
           title = 2018,
           axistype=1,
           caxislabels=c("","",30,"",60),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)

radarchart(ksa_final[c(1,2,6),],
           title = 2016,
           axistype=1,
           caxislabels=c("","",30,"",60),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)

radarchart(ksa_final[c(1,2,8),],
           title = 2014,
           axistype=1,
           caxislabels=c("","",30,"",60),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)

mtext("Monthly Executions in Saudia Arabia: 2014-2019", side = 3, line = 2, outer = TRUE)
      