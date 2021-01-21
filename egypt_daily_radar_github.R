install.packages("fmsb")
library("fmsb")

# Reading in the Egypt data
EDPI <- read.csv ("egypt_individuals_csv.csv", header=TRUE, sep=",")
  
# Converting the column containing the date of each death sentence to date type
EDPI$Crim.1.Judgement.Date <- as.Date (EDPI$Crim.1.Judgement.Date, "%m/%d/%Y")
    
# Creating year of death sentence and day of year of death sentence columns
EDPI$Crim.1.Judgement.Day.of.Year <- format(EDPI$Crim.1.Judgement.Date, "%j")
EDPI$Crim.1.Judgement.Year <- format(EDPI$Crim.1.Judgement.Date, "%Y")
      
# Converting year and day of year columns to numeric
EDPI$Crim.1.Judgement.Year <- as.numeric(EDPI$Crim.1.Judgement.Year)
EDPI$Crim.1.Judgement.Day.of.Year <- as.numeric(EDPI$Crim.1.Judgement.Day.of.Year)
      
# Creating eight empty lists of 365 NULL Values
egypt_daily_2011_list <- vector("list", 365)
egypt_daily_2012_list <- vector("list", 365)
egypt_daily_2013_list <- vector("list", 365)
egypt_daily_2014_list <- vector("list", 365)
egypt_daily_2015_list <- vector("list", 365)
egypt_daily_2016_list <- vector("list", 365)
egypt_daily_2017_list <- vector("list", 365)
egypt_daily_2018_list <- vector("list", 365)
      
      
# for loop that fills each slot of the empty lists with the length of the day of year column (i.e. number of death sentences) for each month
for (i in 1:365) {egypt_daily_2011_list[[i]] <- length((subset(EDPI,
                                                               EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2011))$Crim.1.Judgement.Day.of.Year)

egypt_daily_2012_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2012))$Crim.1.Judgement.Day.of.Year)
          
egypt_daily_2013_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2013))$Crim.1.Judgement.Day.of.Year)
          
egypt_daily_2014_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2014))$Crim.1.Judgement.Day.of.Year)
          
egypt_daily_2015_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2015))$Crim.1.Judgement.Day.of.Year)
          
egypt_daily_2016_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2016))$Crim.1.Judgement.Day.of.Year)
          
egypt_daily_2017_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2017))$Crim.1.Judgement.Day.of.Year)
          
egypt_daily_2018_list[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Day.of.Year == i & EDPI$Crim.1.Judgement.Year == 2018))$Crim.1.Judgement.Day.of.Year)
        }
      
# Turning each list into a dataframe
egypt_2011_daily_df <- data.frame(egypt_daily_2011_list)
egypt_2012_daily_df <- data.frame(egypt_daily_2012_list)
egypt_2013_daily_df <- data.frame(egypt_daily_2013_list)
egypt_2014_daily_df <- data.frame(egypt_daily_2014_list)
egypt_2015_daily_df <- data.frame(egypt_daily_2015_list)
egypt_2016_daily_df <- data.frame(egypt_daily_2016_list)
egypt_2017_daily_df <- data.frame(egypt_daily_2017_list)
egypt_2018_daily_df <- data.frame(egypt_daily_2018_list)
      
# Renaming the dataframe columns
colnames (egypt_2011_daily_df) <- c(1:365)
colnames (egypt_2012_daily_df) <- c(1:365)
colnames (egypt_2013_daily_df) <- c(1:365)
colnames (egypt_2014_daily_df) <- c(1:365)
colnames (egypt_2015_daily_df) <- c(1:365)
colnames (egypt_2016_daily_df) <- c(1:365)
colnames (egypt_2017_daily_df) <- c(1:365)
colnames (egypt_2018_daily_df) <- c(1:365)
      
# Combining all the dataframe rows into one dataframe
egypt_daily_final <- rbind(egypt_2011_daily_df, egypt_2012_daily_df, egypt_2013_daily_df, egypt_2014_daily_df, egypt_2015_daily_df, egypt_2016_daily_df, egypt_2017_daily_df, egypt_2018_daily_df)
      
# Renaming the new dataframe rows
rownames (egypt_daily_final) <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
      
# To use radarchart(), the df has to have max and min values in each column. The below adds max and min rows of 0 and 1, each repeating 365 times
egypt_daily_final <- rbind(egypt_daily_final, (rep.int(1, 365)))
egypt_daily_final <- rbind(egypt_daily_final, (rep.int(0, 365)))
      
# Rearranging the rows because the pattern of rows for radarchart() to work has to be max, min, data:
egypt_daily_final <- egypt_daily_final[c(9,10,8,7,6,5,4,3,2,1),]
      
# Making the chart matrix
par(mar=c(1, 1, 1, 1), #decrease default margin
    oma=c(0,0,5,0),
    bg = "floralwhite",
    cex = 1,
    col="gray20") 
      
layout(matrix(1:8, ncol=2))
radarchart(egypt_daily_final[c(1,2,3),],
           title = 2018,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,4),],
           title = 2017,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,5),],
           title = 2016,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,6),],
           title = 2015,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,7),],
           title = 2014,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,8),],
           title = 2013,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,9),],
           title = 2012,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)

radarchart(egypt_daily_final[c(1,2,10),],
           title = 2011,
           axistype=1,
           caxislabels=c("","","","",""),
           calcex=1.1,
           axislabcol="darkgreen",
           cglcol="black",
           pcol=2)
mtext("Daily Preliminary Death Sentences in Egypt: 2014-2019", side = 3, line = 2, outer = TRUE)
      