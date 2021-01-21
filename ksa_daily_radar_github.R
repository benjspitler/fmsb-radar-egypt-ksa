library(fmsb)

# Reading in the data from .csv file. This .csv contains approximately 700 rows, each of which contains information on a discrete individual executed in Saudi Arabia between 2014 and 2019
KSA <- read.csv ("KSA_individuals_csv.csv", header=TRUE, sep=",")
  
# Converting date of execution column to date type
KSA$Date.of.execution <- as.Date (KSA$Date.of.execution, "%m/%d/%Y")
    
# Creating Execution.day.of.year and Execution.year columns
KSA$Execution.day.of.year <- format(KSA$Date.of.execution, "%j")
KSA$Execution.year <- format(KSA$Date.of.execution, "%Y")
      
# Converting Execution.day.of.year and Execution.year to numeric:
KSA$Execution.day.of.year <- as.numeric(KSA$Execution.day.of.year)
KSA$Execution.year <- as.numeric(KSA$Execution.year)
      
# Creating empty lists of 365 NULL Values
ksa_daily_2014_list <- vector("list", 365)
ksa_daily_2015_list <- vector("list", 365)
ksa_daily_2016_list <- vector("list", 365)
ksa_daily_2017_list <- vector("list", 365)
ksa_daily_2018_list <- vector("list", 365)
ksa_daily_2019_list <- vector("list", 365)
      
# for loop that fills each slot of the empty lists with the length of the Execution.month column (i.e. number of executions) for each month
for (i in 1:365) {ksa_daily_2014_list[[i]] <- length((subset(KSA,
                                                             KSA$Execution.day.of.year == i & KSA$Execution.year == 2014))$Execution.day.of.year)
          
ksa_daily_2015_list[[i]] <- length((subset(KSA,
                                           KSA$Execution.day.of.year == i & KSA$Execution.year == 2015))$Execution.day.of.year)
          
ksa_daily_2016_list[[i]] <- length((subset(KSA,
                                           KSA$Execution.day.of.year == i & KSA$Execution.year == 2016))$Execution.day.of.year)
          
ksa_daily_2017_list[[i]] <- length((subset(KSA,
                                           KSA$Execution.day.of.year == i & KSA$Execution.year == 2017))$Execution.day.of.year)
          
ksa_daily_2018_list[[i]] <- length((subset(KSA,
                                           KSA$Execution.day.of.year == i & KSA$Execution.year == 2018))$Execution.day.of.year)
          
ksa_daily_2019_list[[i]] <- length((subset(KSA,
                                           KSA$Execution.day.of.year == i & KSA$Execution.year == 2019))$Execution.day.of.year)
}
      
# Turning each list into a dataframe
ksa_2014_daily_df <- data.frame(ksa_daily_2014_list)
ksa_2015_daily_df <- data.frame(ksa_daily_2015_list)
ksa_2016_daily_df <- data.frame(ksa_daily_2016_list)
ksa_2017_daily_df <- data.frame(ksa_daily_2017_list)
ksa_2018_daily_df <- data.frame(ksa_daily_2018_list)
ksa_2019_daily_df <- data.frame(ksa_daily_2019_list)
      
# Renaming the dataframe columns
colnames (ksa_2014_daily_df) <- c(1:365)
colnames (ksa_2015_daily_df) <- c (1:365)
colnames (ksa_2016_daily_df) <- c (1:365)
colnames (ksa_2017_daily_df) <- c (1:365)
colnames (ksa_2018_daily_df) <- c (1:365)
colnames (ksa_2019_daily_df) <- c (1:365)
      
# Combining all the dataframe rows into one dataframe
ksa_daily_final <- rbind(ksa_2014_daily_df, ksa_2015_daily_df, ksa_2016_daily_df, ksa_2017_daily_df, ksa_2018_daily_df, ksa_2019_daily_df)
      
# Renaming the new dataframe rows
rownames (ksa_daily_final) <- c(2014, 2015, 2016, 2017, 2018, 2019)
      
# To use radarchart(), the df has to have max and min values in each column. The below adds max and min rows of 0
# and 1, each repeating 365 times
ksa_daily_final <- rbind(ksa_daily_final, (rep.int(1, 365)))
ksa_daily_final <- rbind(ksa_daily_final, (rep.int(0, 365)))
      
# Rearranging the rows because the pattern of rows for radarchart() to work has to be max, min, data:
ksa_daily_final <- ksa_daily_final[c(7,8,6,5,4,3,2,1),]
      
# Making the chart matrix
par(mar=c(1, 1, 1, 1), #decrease default margin
    oma=c(0,0,5,0),
    bg = "floralwhite",
    cex = 1,
    col="gray20"
    ) 

layout(matrix(1:6, ncol=2))

radarchart(ksa_daily_final[c(1,2,3),],
           title = 2019,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(ksa_daily_final[c(1,2,5),],
           title = 2017,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(ksa_daily_final[c(1,2,7),],
           title = 2015,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(ksa_daily_final[c(1,2,4),],
           title = 2018,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(ksa_daily_final[c(1,2,6),],
           title = 2016,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(ksa_daily_final[c(1,2,8),],
           title = 2014,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

mtext("Daily Executions in Saudia Arabia: 2014-2019", side = 3, line = 2, outer = TRUE)
      