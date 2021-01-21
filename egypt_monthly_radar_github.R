install.packages("fmsb")
library("fmsb")

# Reading in the data from .csv file. This .csv contains 3000+ rows, each of which contains information on a discrete individual sentenced to death by an Egyptian court between 2011 and 2018
EDPI <- read.csv ("egypt_individuals_csv.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
  
# Converting the column containing the date of each death sentence to date type
EDPI$Crim.1.Judgement.Date <- as.Date (EDPI$Crim.1.Judgement.Date, "%m/%d/%Y")
    
# Creating a new column by extracting the month of each sentence
EDPI$Crim.1.Judgement.Month <- format(EDPI$Crim.1.Judgement.Date, "%m")
      
# Converting month of sentence column to numeric so the for loop below doesn't exclude months 01-09
EDPI$Crim.1.Judgement.Month <- as.numeric(EDPI$Crim.1.Judgement.Month)
        
# Creating eight empty lists of 12 NULL values, to be filled by the for loop
egypt_years_list_2011 <- vector("list", 12)
egypt_years_list_2012 <- vector("list", 12)
egypt_years_list_2013 <- vector("list", 12)
egypt_years_list_2014 <- vector("list", 12)
egypt_years_list_2015 <- vector("list", 12)
egypt_years_list_2016 <- vector("list", 12)
egypt_years_list_2017 <- vector("list", 12)
egypt_years_list_2018 <- vector("list", 12)
          
# for loop that fills each slot of the empty lists with the length of the date column (i.e. number of death sentences) for each month
for (i in 1:12) {
  egypt_years_list_2011[[i]] <- length((subset(EDPI,
                                               EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2011))$Crim.1.Judgement.Year)
              
egypt_years_list_2012[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2012))$Crim.1.Judgement.Year)
              
egypt_years_list_2013[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2013))$Crim.1.Judgement.Year)
              
egypt_years_list_2014[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2014))$Crim.1.Judgement.Year)
              
egypt_years_list_2015[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2015))$Crim.1.Judgement.Year)
              
egypt_years_list_2016[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2016))$Crim.1.Judgement.Year)
              
egypt_years_list_2017[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2017))$Crim.1.Judgement.Year)
              
egypt_years_list_2018[[i]] <- length((subset(EDPI,
                                             EDPI$Crim.1.Judgement.Month == i & EDPI$Crim.1.Judgement.Year == 2018))$Crim.1.Judgement.Year)
            }
          
# Turning each list into a dataframe
egypt_2011_prelim <- data.frame(egypt_years_list_2011)
egypt_2012_prelim <- data.frame(egypt_years_list_2012)
egypt_2013_prelim <- data.frame(egypt_years_list_2013)
egypt_2014_prelim <- data.frame(egypt_years_list_2014)
egypt_2015_prelim <- data.frame(egypt_years_list_2015)
egypt_2016_prelim <- data.frame(egypt_years_list_2016)
egypt_2017_prelim <- data.frame(egypt_years_list_2017)
egypt_2018_prelim <- data.frame(egypt_years_list_2018)
          
# Renaming the dataframe columns
colnames (egypt_2011_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2012_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2013_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2014_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2015_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2016_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2017_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames (egypt_2018_prelim) <- c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
          
# Combining all the dataframe rows into one dataframe
egypt_final <- rbind(egypt_2011_prelim, egypt_2012_prelim, egypt_2013_prelim, egypt_2014_prelim, egypt_2015_prelim, egypt_2016_prelim, egypt_2017_prelim, egypt_2018_prelim)
          
# Renaming the new dataframe rows
rownames (egypt_final) <- c(2011,2012,2013, 2014, 2015, 2016, 2017, 2018)
          
# To use radarchart(), the df has to have max and min values in each column. The below adds max and min rows of 0 and 40, each repeating 12 times
egypt_final <- rbind(egypt_final, (rep.int(40, 12)))
egypt_final <- rbind(egypt_final, (rep.int(0, 12)))
          
# Rearranging the rows because the pattern of rows for radarchart() to work has to be max, min, data:
egypt_final <- egypt_final[c(9,10,8,7,6,5,4,3,2,1),]
          
# Making the chart matrix
par(xpd=TRUE, mar=c(1, 1, 1, 1), #xpd prevents labels from being cut off. mar decreases default margin.
    oma=c(0,0,5,0),
    bg = "floralwhite",
    cex = 1,
    col="gray20") 

layout(matrix(1:8, ncol=4))
          
radarchart(egypt_final[c(1,2,3),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2018", font = 2)
          
radarchart(egypt_final[c(1,2,7),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2014", font = 2)
          
radarchart(egypt_final[c(1,2,4),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2017", font = 2)
          
radarchart(egypt_final[c(1,2,8),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2013", font = 2)
          
radarchart(egypt_final[c(1,2,5),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2016", font = 2)
          
radarchart(egypt_final[c(1,2,9),],
                     axistype=1,
                     caxislabels=c("","",20,"",40),
                     calcex=1.1,
                     axislabcol="black",
                     cglcol="black",
                     pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2012", font = 2)
          
radarchart(egypt_final[c(1,2,6),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2015", font = 2)
          
radarchart(egypt_final[c(1,2,10),],
           axistype=1,
           caxislabels=c("","",20,"",40),
           calcex=1.1,
           axislabcol="black",
           cglcol="black",
           pcol=2)
mtext(side = 3, line = -4, at = 0, cex = 1.75, "2011", font = 2)
          
mtext("Monthly Preliminary Death Sentences in Egypt: 2011-2018", side = 3, line = 2, outer = TRUE)
          