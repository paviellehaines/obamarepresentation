###################SECTION 1: INSTALL LIBRARIES (NEED ONLY BE RUN ONCE, BEFORE ANYTHING ELSE; DOES NOT NEED TO BE RUN EACH TIME THE FILE IS REOPENED)###################

# INSTALL REQUIRED LIBRARIES ---------------------------------

#This code was written using R 3.3.1

#This step ensures that all the libraries required to run the anlayses are installed to your R program. You only need to run this section of the code once.

install.packages("foreign")
install.packages("gmodels")
install.packages("plyr")
install.packages("MASS")
install.packages("aod")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("Zelig")
install.packages("ZeligChoice")
install.packages("coefplot")
install.packages("reshape2")
install.packages("gplots")
install.packages("survey")
install.packages("xlsx")
install.packages("RColorBrewer")
install.packages("irr")
install.packages("clusterSEs")
install.packages("dplyr")
install.packages("tibble")
install.packages("tseries")





###################SECTION 2: CALL LIBRARIES AND DATA (MUST BE RUN EACH TIME THE FILE IS OPENED, BEFORE SECTIONS 3 AND 4)###################

# READ IN REQUIRED LIBRARIES AND DATA -------------------------------------

library(foreign) #Call libraries
library(gmodels)
library(plyr)
library(MASS)
library(aod)
library(Hmisc)
library(ggplot2)
library(Zelig)
library(ZeligChoice)
library(coefplot)
library(reshape2)
library(gplots)
library(survey)
library(xlsx)
library(RColorBrewer)
library(irr)
library(clusterSEs)
library(dplyr)
library(tibble)

#You must set the working directory to the source file location before reading in the data

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Read in intial budget and speech files
yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricData.csv", header = TRUE)

head(yearlyspending) #View first few rows of data
head(yearlyspeeches)





###################SECTION 3: ORGANIZE DATA (NEED ONLY BE RUN ONCE, BEFORE SECTION 4; DOES NOT NEED TO BE RUN EACH TIME THE FILE IS REOPENED)###################

# ORGANIZE DEMOGRAPHIC & POLITCAL DATA -----------------------------------------------

#Unemployment

unemploymentlevel <- read.csv("DOL Number of Unemployed R Format.csv", header = TRUE) #Read in required data for unemployment
unemploymentrate <- read.csv("DOL Unemployment Rate R Format.csv", header = TRUE)

#https://data.bls.gov/timeseries/LNS14000000 Websites for original data files
#https://data.bls.gov/timeseries/LNU03000000

head(unemploymentlevel) #View first few rows
head(unemploymentrate)

unemployment <- data.frame(year = unemploymentrate$Year, unemployed = unemploymentlevel$Annual, percentunemployed = unemploymentrate$Annual) #Combine and rename variables
head(unemployment)


#Poverty

poverty <- read.csv("Census Bureau Poverty Data 1964-2015 R Format.csv", header = TRUE) #Read in required data

#http://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html Website for original data file

poverty16 <- subset(poverty, year == 2015) #Take 2015 values for 2016
poverty16$year <- 2016
poverty <- rbind(poverty, poverty16)

poverty <- arrange(poverty, year) #Organized poverty data by year
head(poverty)


#Race

race <- read.csv("Census Bureau Race Data 1966-2015 R Format.csv", header = TRUE) #Read in racial demographic data

#http://www.census.gov/quickfacts/table/PST045216/00 Website for 1966-2015 data
#https://www.cdc.gov/nchs/data/statab/pop6097.pdf Website for 1964-1965 data

race16 <- subset(race, year == 2015) #Take 2015 values for 2016
race16$year <- 2016
race <- rbind(race, race16)

race <- rbind(race, c(year = 1965, blacks = 20999)) #Add 1964 and 1964 to the data manually
race <- rbind(race, c(year = 1964,  blacks = 20610))

race <- arrange(race, year) #Organize race data by year

head(race) #View first few rows


#GDP

gdp <- read.csv("BEA GDP R Format.csv", header = TRUE) #Read in GDP data files
gdpchange <- read.csv("BEA GDP Growth R Format.csv", header = TRUE)

#https://www.bea.gov/national/index.htm#gdp Website for original data

gdp <- subset(gdp, year >= 1964) #Remove unwanted years
gdpchange <- subset(gdpchange, year >= 1964) #Remove unwanted years

gdp <- data.frame(year = gdp$year, gdp = gdp$real.gdp, gdpgrowth = gdpchange$real.change) #Combine and rename variables

head(gdp) #View first few rows


#CONGRESS VARIABLES

#https://www.senate.gov/history/partydiv.htm   Websites for original data
#http://history.house.gov/Institution/Party-Divisions/Party-Divisions/

year <- as.numeric(c(1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) #Create vector of years
democrat <- data.frame(year = year, dem.house = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)) #Create data frame showing whether the Democrats controlled the House in each year
democrat <- cbind(democrat, dem.senate = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)) #Add variable showing whether the Democrats controlled the Senate in each year
democrat$congressdem <- democrat$dem.house * democrat$dem.senate #Add variable showing whether the Democrats controlled the House and Senate in each year

head(democrat) #View first few rows




# ADD DEMOGRAPHIC & POLITICAL DATA TO YEARLY SPENDING --------------------------------

list(unique(yearlyspending$propyear)) #View years in the spending data
list(unique(unemployed$year)) #View years in the unemployment data
list(unique(poverty$year)) #View years in the unemployment data
list(unique(race$year)) #View years in the unemployment data
list(unique(gdp$year)) #View years in the unemployment data
list(unique(democrat$year)) #View years in the unemployment data

     
yearlyspending <- rbind(NA, NA, NA, NA, NA, yearlyspending) #Add temporary rows to make all datasets to be combined the same length

yearlyspending$unemployed <- ifelse(yearlyspending$propyear == unemployment$year, unemployment$unemployed/1000, "") #Add number of unemployed to dataset
yearlyspending$percentunemployed <- ifelse(yearlyspending$propyear == unemployment$year, unemployment$percentunemployed, "") #Add percent of unemployed to dataset

yearlyspending$poor <- ifelse(yearlyspending$propyear == as.numeric(poverty$year), poverty$poor/1000, "") #Add number of poor to dataset
yearlyspending$percentpoor <- ifelse(yearlyspending$propyear == as.numeric(poverty$year), poverty$percentpoor, "") #Add percent of poor to dataset

yearlyspending$blacks <- ifelse(yearlyspending$propyear == as.numeric(race$year), race$blacks/1000, "") #Add number of blacks to dataset

yearlyspending$gdp <- ifelse(yearlyspending$propyear == gdp$year, gdp$gdp, "") #Add gdp to dataset
yearlyspending$gdpgrowth <- ifelse(yearlyspending$propyear == gdp$year, gdp$gdpgrowth, "") #Add gdp growth to dataset

yearlyspending$congressdem <- ifelse(yearlyspending$propyear == democrat$year, democrat$congressdem, "") #Add Democratic control of Congress to the dataset
yearlyspending$democratpres <- ifelse(yearlyspending$president == "Obama", 1, ifelse(yearlyspending$president == "W. Bush", 0, ifelse(yearlyspending$president == "Clinton", 1, ifelse(yearlyspending$president == "H.W. Bush", 0, ifelse(yearlyspending$president == "Reagan", 0, ifelse(yearlyspending$president == "Carter", 1, ifelse(yearlyspending$president == "Ford", 0, ifelse(yearlyspending$president == "Nixon", 0, ifelse(yearlyspending$president == "Johnson", 1, ""))))))))) #Add indicator for Democrat presidents to the dataset

yearlyspending <- na.omit(yearlyspending) #Remove empty rows from the dataset

print(yearlyspending) #View the dataset
write.csv(yearlyspending, "ObamaDescriptiveYearlySpendingDataVer2.csv") #Create new file





# ADD DEMOGRAPHIC & POLITICAL DATA TO YEARLY RHETORIC ---------------------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricData.csv", header = TRUE) #Read in intial data files
head(yearlyspeeches) #View first few rows of data

list(unique(yearlyspeeches$year)) #View years in the spending data
list(unique(unemployed$year)) #View years in the unemployment data
list(unique(poverty$year)) #View years in the unemployment data
list(unique(race$year)) #View years in the unemployment data
list(unique(gdp$year)) #View years in the unemployment data
list(unique(democrat$year)) #View years in the unemployment data


yearlyspeeches <- rbind(yearlyspeeches, 1969, 1973, 1977, NA) #Add temporary rows to make all datasets to be combined the same length
yearlyspeeches$year <- as.numeric(yearlyspeeches$year) #Make new rows numeric in year column
yearlyspeeches <- arrange(yearlyspeeches, year) #Arrange rows by year
print(yearlyspeeches) #Make sure years are sequential

yearlyspeeches$percentunemployed <- ifelse(yearlyspeeches$demoyear == unemployment$year, unemployment$percentunemployed, "") #Add percent of unemployed to dataset

yearlyspeeches$percentpoor <- ifelse(yearlyspeeches$demoyear == as.numeric(poverty$year), poverty$percentpoor, "") #Add percent of poor to dataset

yearlyspeeches$gdp <- ifelse(yearlyspeeches$demoyear == gdp$year, gdp$gdp, "") #Add gdp to dataset
yearlyspeeches$gdpgrowth <- ifelse(yearlyspeeches$demoyear == gdp$year, gdp$gdpgrowth, "") #Add gdp growth to dataset

democrat.sub <- subset(democrat, year != 1964) #Remove unwanted years from the partisan control dataset
democrat.sub <- rbind(democrat.sub, NA) #Add temporary placeholder row to make all datasets the same length
yearlyspeeches$congressdem <- ifelse(yearlyspeeches$year == democrat.sub$year, democrat.sub$congressdem, "") #Add Democratic control of Congress to the dataset
yearlyspeeches$democratpres <- ifelse(yearlyspeeches$president == "Obama", 1, ifelse(yearlyspeeches$president == "W. Bush", 0, ifelse(yearlyspeeches$president == "Clinton", 1, ifelse(yearlyspeeches$president == "H.W. Bush", 0, ifelse(yearlyspeeches$president == "Reagan", 0, ifelse(yearlyspeeches$president == "Carter", 1, ifelse(yearlyspeeches$president == "Ford", 0, ifelse(yearlyspeeches$president == "Nixon", 0, ifelse(yearlyspeeches$president == "Johnson", 1, ""))))))))) #Add indicator for Democrat presidents to the dataset

yearlyspeeches <- na.omit(yearlyspeeches) #Remove empty rows from the dataset
yearlyspeeches <- subset(yearlyspeeches, year != 1969 & year != 1973 & year != 1977) #Remove placeholder years from the dataset

print(yearlyspeeches) #View the dataset
write.csv(yearlyspeeches, "ObamaDescriptiveYearlyRhetoricDataVer2.csv") #Create new file





# CALCULATE AND ADD VARIABLES FOR YEARLY SPENDING PER PERSON AND AS A PERCENT OF THE DOMESTIC BUDGET (ONLY NEEDS TO BE RUN ONCE) --------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingDataVer2.csv", header = TRUE) #Reread in required data file


#Note that the dataset already contains the following variables.They are recreated in the code below to show how they were calculated and gaurantee their accuracy.

yearlyspending$perpoor <- yearlyspending$povertytot/yearlyspending$poor #Create variable for per poor person poverty spending

yearlyspending$perunemployed <- yearlyspending$povertytot/yearlyspending$unemployed #Create variable for per unemployed person poverty spending

yearlyspending$perblack <- yearlyspending$civil/yearlyspending$blacks #Create variable for per black person civil rights spending

yearlyspending$povertydomestic <- yearlyspending$discstand/yearlyspending$domesticstand #Create variable for proposed poverty spending as a percentage of the proposed domestic budget

yearlyspending$civildomestic <- yearlyspending$civil/yearlyspending$domesticstand #Create variable for proposed civil rights spending as a percentage of the proposed domestic budget


write.csv(yearlyspending, file = "ObamaDescriptiveYearlySpendingDataVer3.csv") #Export updated data file





# CREATE DATASET FOR AVERAGE SPENDING PER PRESIDENTIAL ADMINSITRATION ------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingDataVer3Final.csv", header = TRUE) #Reread in required data file


adminspending <- aggregate(yearlyspending, by= list(yearlyspending$president), FUN = mean) #Aggregate by president (there will be warnings for non-numeric columns returning NA's; these do not affect the aggregation of the rest of the columns and can be ignored).
print(adminspending) #View new dataframe; Note columns with NA's and decimal values for variables meant to be whole numbers (e.g. "budgetid", "congresparty", "budgetyear", "term", etc.). Variables like this will be removed since they are not required to analyze this datset.


adminspending$president <- adminspending$Group.1 #Rename grouping variable

adminspending <- arrange(adminspending, presorder) #Order results chronologically

adminspending <- subset(adminspending, select = c("president", "presidentid", "presorder", "povertytot", "civil", "perpoor", "perunemployed", "perblack", "povertydomestic", "civildomestic", "blacks")) #Subset only required variables

print(adminspending) #View the dataset

write.csv(adminspending, file = "ObamaDescriptiveAdminSpendingData.csv") #Export the new data file





# CALCULATE AND ADD FREQUENCY VARIABLES FOR YEARLY RHETORIC ---------------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricDataVer2.csv", header = TRUE) #Reread in required data file


#Note that the dataset already contains the following variables.They are recreated in the code below to show how they were calculated and gaurantee their accuracy.

yearlyspeeches$pospovword <- yearlyspeeches$povword - yearlyspeeches$negpovword #Calculate positive/neutral poverty words

yearlyspeeches$poscivilword <- yearlyspeeches$civilword - yearlyspeeches$negcivilword #Calculate positive/neutral civil rights words



yearlyspeeches$povwordper <- (yearlyspeeches$povword/yearlyspeeches$words)*10000 #Calculate total poverty keywords per 10,000

yearlyspeeches$negpovper <- (yearlyspeeches$negpovword/yearlyspeeches$words)*10000 #Calculate negative poverty keywords per 10,000

yearlyspeeches$pospovper <- yearlyspeeches$povwordper - yearlyspeeches$negpovper #Calculate positive/neautral poverty keywords per 10,000



yearlyspeeches$middlewordper <- (yearlyspeeches$middleword/yearlyspeeches$words)*10000 #Calculate middle class keywords per 10,000



yearlyspeeches$civilwordper <- (yearlyspeeches$civilword/yearlyspeeches$words)*10000 #Calculate total civil rights keywords per 10,000

yearlyspeeches$negcivilper <- (yearlyspeeches$negcivilword/yearlyspeeches$words)*10000 #Calculate negative civil rights keywords per 10,000

yearlyspeeches$poscivilper <- yearlyspeeches$civilwordper - yearlyspeeches$negcivilper #Calculate positive/neutral civil rights keywords per 10,000


head(yearlyspeeches) #View dataset


write.csv(yearlyspeeches, file = "ObamaDescriptiveYearlyRhetoricDataVer3Final.csv") #Replace old data file with the version including frequency variables





# CREATE DATASET FOR AVERAGE PER TERM RHETORIC -----------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricDataVer3Final.csv", header = TRUE) #Reread in required data file


termspeeches <- aggregate(yearlyspeeches, by= list(yearlyspeeches$president, yearlyspeeches$term), FUN = mean) #Aggregate by term (there will be warnings for non-numeric columns returning NA's; these do not affect the aggregation of the rest of the columns and can be ignored).
print(termspeeches) #View new dataframe; Note columns with NA's and decimal values for variables meant to be whole numbers (e.g. "year", "electionspeech", "congressdem", etc.). Variables like this will either be modified or be removed if they are not required to analyze this datset.

termspeeches$president <- termspeeches$Group.1 #Rename grouping variables
termspeeches$term <- termspeeches$Group.2

termspeeches$congressdem <- ifelse(termspeeches$congressdem >= .5, 1, 0) #Modify "congressdem" to a binary indicator for whether Democrats controlled both the House and Senate for at least half the years of a term
termspeeches$electionspeech <- ifelse(termspeeches$electionspeech > 0, 1, 0) #Modify "electionspeech" to a binary indicator for whether the president was up for reelection at the end of a term

termspeeches <- arrange(termspeeches, presorder, term) #Order results chronologically
print(termspeeches) #View the dataset

termspeeches <- data.frame(subset(termspeeches, select = -c(Group.1, Group.2, X, year))) #Remove the extra grouping variables and those not required for the analyses
print(termspeeches) #View the dataset


write.csv(termspeeches, file = "ObamaDescriptiveTermRhetoricData.csv") #Export data file





# CREATE DATASET FOR AVERAGE PER PRESIDENTIAL ADMINISTRATION RHETORIC -----------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricDataVer3Final.csv", header = TRUE) #Reread in required data file


adminspeeches <- aggregate(yearlyspeeches, by= list(yearlyspeeches$president), FUN = mean) #Aggregate by president and term (there will be warnings for non-numeric columns returning NA's; these do not affect the aggregation of the rest of the columns and can be ignored).
print(adminspeeches)

adminspeeches$president <- adminspeeches$Group.1 #Reassign grouping variables to the correct column

adminspeeches <- arrange(adminspeeches, presorder) #Order results chronologically
print(adminspeeches) #View the dataset

adminspeeches <- data.frame(subset(adminspeeches, select = -c(Group.1, X, year, term, obamadummy, democratpres, electionspeech, congressdem, gdp, gdpgrowth, percentunemployed, percentpoor))) #Remove the extra grouping variables and those not needed for future analyses
print(adminspeeches) #View the dataset

write.csv(adminspeeches, file = "ObamaDescriptiveAdministrationRhetoricData.csv") #Export data file






###################SECTION 4: ANALYSES ###################



# PLOT PER TERM POVERTY AND MIDDLE CLASS RHETORIC (SEE FIGURE 1) ------------------------------------------

speechesterm <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
head(speechesterm) #View dataset


dev.off() #Reset graphical parameters to default
par(mar=c(2,1.8,1,0)) #Set graph margins
par(family = "serif") #Set text


pospov <- speechesterm$pospovper #Name variables to graph
negpov <- speechesterm$negpovper
middle <- speechesterm$middlewordper


mydata <- cbind(rbind(pospov, negpov, 0), rbind(0, 0, middle)) [,c(1, 15, 2, 16, 3, 17, 4, 18, 5, 19, 6, 20, 7, 21, 8, 22, 9, 23, 10, 24, 11, 25, 12, 26, 13, 27, 14, 28)] #Organize the data for stacked and grouped bars


mycolors <- c("gray38", "gray9", "gray99") #Establish colors for graphing


bp <- barplot(mydata, xlim = c(1.1, 39.75), ylim = c(0,40), main = "", space = c(0,0, 1, 0, 1, 0, 1,0, 1,0, 1,0, 1,0, 1,0, 1,0, 1,0, 1,0, 1, 0, 1, 0, 1, 0), ylab = "", yaxt = "n", col = mycolors, cex.axis = .75) #Create bargraph
abline(h=0, col = "black", lty = 1, lwd = 1.5) #Add line for x-axis
axis(2, tck = -.013, at = c(0, 10, 20, 30, 40), c("", "", "", "", "")) #Add line and ticks to y-axis
mtext(side = 2, text = "Keywords Per 10,000", line = 1., cex = .75) #Add y-axis label
mtext(side = 2, at = c(0, 10, 20, 30, 40), text = c("0", "10", "20", "30", "40"), line = .3, cex = .6) #Add y-axis values
mtext(side = 1, line = .4, at = c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40), text = c("Johnson\nTerm 1", "Nixon\nTerm 1","Nixon\nTerm 2", "Ford\nTerm 1", "Carter\nTerm 1", "Reagan\nTerm 1","Reagan\nTerm 2", "H.W. Bush\nTerm 1", "Clinton\nTerm 1","Clinton\nTerm 2", "W. Bush\nTerm 1", "W. Bush\nTerm 2", "Obama\nTerm 1", "Obama\nTerm 2"), cex = .6) #Add x-axis labels
mycolors <- c("gray38", "gray9", "black") #Establish colors for the legend
legend("topleft", c("Positive/Neutral Poverty Mentions", "Negative Poverty Mentions", "Middle Class Mentions"), col = mycolors, pch = c(15, 15, 0), bty = "n", cex = .75) #Add legend

#Figure should be 4.5x6.5





# PLOT PER TERM CIVIL RIGHTS RHETORIC (SEE FIGURE 2) ------------------------------------------

speechesterm <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
head(speechesterm) #View the dataset


dev.off() #Reset graphical parameters to default
par(mar=c(2,1.8,1,0)) #Set graph margins
par(family = "serif") #Set text

civilpos <- speechesterm$poscivilper  #Name variables to graph
civilneg <- speechesterm$negcivilper


mydata <- cbind(rbind(civilpos, civilneg)) #Organize the data for stacked and grouped bars


mycolors <- c("gray38", "gray9") #Establish colors for graphing


bp <- barplot(mydata, xlim = c(1.75, 27.25), ylim = c(0, 10), main = "", yaxt = 'n', col = mycolors, space = c(1)) #Create bargraph
abline(h=0, col = "black", lty = 1) #Add line for x-axis
axis(2, tck = -.015, at = c(0, 2, 4, 6, 8, 10), c("", "", "", "", "", "")) #Add line and ticks for y-axis
mtext(side = 2, text = "Keywords Per 10,000", line = 1.1, cex = .75) #Add y-axis label
mtext(side = 2, at = c(0, 2, 4, 6, 8, 10), text = c("0", "2", "4", "6", "8", "10"), line = .2, cex = .6) #Add y-axis values
mtext(side = 1, line = .4, at = bp, text = c("Johnson\nTerm 1", "Nixon\nTerm 1","Nixon\nTerm 2", "Ford\nTerm 1", "Carter\nTerm 1", "Reagan\nTerm 1","Reagan\nTerm 2", "H.W. Bush\nTerm 1", "Clinton\nTerm 1","Clinton\nTerm 2", "W. Bush\nTerm 1", "W. Bush\nTerm 2", "Obama\nTerm 1", "Obama\nTerm 2"), cex = .6) #Add x-axis labels
mycolors <- c("gray38", "gray9", "black") #Establish colors for the legend
legend("topleft", c("Positive/Neutral African American & Civil Rights Mentions", "Negative African American & Civil Rights Mentions"), col = mycolors, pch = c(15, 15), bty = "n", cex = .75) #Add legend

#Figure should be 6.5x4.5





# CALCUATIONS & SIGNIFICANCE TESTS FOR POVERTY RHETORIC (SEE "RESULTS: PRESIDENTIAL RHETORIC" AND FOOTNOTE 18) ---------------------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricDataVer3Final.csv", header = TRUE) #Reread in necessary datasets
adminspeeches <- read.csv("ObamaDescriptiveAdministrationRhetoricData.csv", header = TRUE)


#Create subsets necessary to run t-tests
johnson <- subset(yearlyspeeches, president == "Johnson") #Create Democratic president subsets
carter <- subset(yearlyspeeches, president == "Carter")
clinton <- subset(yearlyspeeches, president == "Clinton")
obama <- subset(yearlyspeeches, president == "Obama")

all <- subset(adminspeeches, president == "Johnson"| president == "Nixon" | president == "Ford" | president == "Carter" | president == "Reagan" | president == "H.W. Bush" | president == "Clinton" | president == "W. Bush") #Create subset with all presidents except Obama (using per president averages)
reps <- subset(adminspeeches, president == "Nixon" | president == "Ford" | president == "Reagan" | president == "H.W. Bush" | president == "W. Bush") #Create subset with all Republicans
dems <- subset(adminspeeches, president == "Johnson" |president == "Carter"|president == "Clinton") #Create subset with all Democrats except Obama



#Compare Obama to Johnson
johnson <- johnson$povwordper #Create variable for Johnson's poverty rhetoric
obama <- obama$povwordper #Create variable for Obama's poverty rhetoric

johnsonobama <- t.test(obama,johnson) #Two sample t-test
johnsonobama #View results



#Compare Obama to Carter
carter <- carter$povwordper #Create variable for Carter's poverty rhetoric

carterobama <- t.test(obama,carter) #Two sample t-test
carterobama #View results



#Compare Obama to Clinton
clinton <- clinton$povwordper #Create variable for Clinton's poverty rhetoric

clintonobama <- t.test(obama,clinton) #Two sample t-test
clintonobama #View results



#Compare Obama to Democrats
dems <- dems$povwordper #Create variable for Democrat's poverty rhetoric

obamadems <- t.test(dems, mu = mean(obama)) #Clustered one sample t-test
obamadems #View results


#Compare Obama to Republicans
reps <- reps$povwordper #Create variable for Republican's povery rhetoric

obamareps <- t.test(reps, mu = mean(obama)) #Clustered one sample t-test
obamareps #View results





# CALCUATIONS & SIGNIFICANCE TESTS FOR CIVIL RIGHTS RHETORIC (SEE "RESULTS: PRESIDENTIAL RHETORIC")---------------------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricDataVer3Final.csv", header = TRUE) #Reread in necessary datasets
adminspeeches <- read.csv("ObamaDescriptiveAdministrationRhetoricData.csv", header = TRUE)


#Create subsets necessary to run t-tests
johnson <- subset(yearlyspeeches, president == "Johnson") #Create Democratic president subsets
carter <- subset(yearlyspeeches, president == "Carter")
clinton <- subset(yearlyspeeches, president == "Clinton")
obama <- subset(yearlyspeeches, president == "Obama")

all <- subset(adminspeeches, president == "Johnson"| president == "Nixon" | president == "Ford" | president == "Carter" | president == "Reagan" | president == "H.W. Bush" | president == "Clinton" | president == "W. Bush") #Create subset with all presidents except Obama (using per president averages)
reps <- subset(adminspeeches, president == "Nixon" | president == "Ford" | president == "Reagan" | president == "H.W. Bush" | president == "W. Bush") #Create subset with all Republicans
dems <- subset(adminspeeches, president == "Johnson" |president == "Carter"|president == "Clinton") #Create subset with all Democrats except Obama



#Compare Obama to Clinton
clinton <- clinton$civilwordper #Create variable for Clinton's civil rights rhetoric
obama <- obama$civilwordper #Create variable for Obama's civil rights rhetoric

clintonobama <- t.test(obama,clinton) #Two sample t-test
clintonobama #View results



#Compare Obama to Carter
carter <- carter$civilwordper #Create variable for Carter's civil rights rhetoric

carterobama <- t.test(obama,carter) #Two sample t-test
carterobama #View results



#Compare Obama to Johnson
johnson <- johnson$civilwordper #Create variable for Johnson's civil rights rhetoric

johnsonobama <- t.test(obama,johnson) #Two sample t-test
johnsonobama #View results



#Compare Obama to Republicans
reps <- reps$civilwordper #Create variable for Republican's civil rights rhetoric

obamareps <- t.test(reps, mu = mean(obama)) #Clustered one sample t-test
obamareps #View results


#Compare Obama to Democrats
dems <- dems$civilwordper #Create variable for Democrat's civil rights rhetoric

obamadems <- t.test(dems, mu = mean(obama)) #Clustered one sample t-test
obamadems #View results





# CALCUATIONS & SIGNIFICANCE TESTS FOR MIDDLE CLASS RHETORIC (SEE FOOTNOTE 17)---------------------------------

yearlyspeeches <- read.csv("ObamaDescriptiveYearlyRhetoricDataVer3Final.csv", header = TRUE) #Reread in necessary datasets
adminspeeches <- read.csv("ObamaDescriptiveAdministrationRhetoricData.csv", header = TRUE)


#Create subsets necessary to run t-test
johnson <- subset(yearlyspeeches, president == "Johnson") #Create Democrat president subsets
carter <- subset(yearlyspeeches, president == "Carter")
clinton <- subset(yearlyspeeches, president == "Clinton")
obama <- subset(yearlyspeeches, president == "Obama")

all <- subset(adminspeeches, president == "Johnson"| president == "Nixon" | president == "Ford" | president == "Carter" | president == "Reagan" | president == "H.W. Bush" | president == "Clinton" | president == "W. Bush") #Create subset with all presidents except Obama (using per president averages)
reps <- subset(adminspeeches, president == "Nixon" | president == "Ford" | president == "Reagan" | president == "H.W. Bush" | president == "W. Bush") #Create subset with all Republicans
dems <- subset(adminspeeches, president == "Johnson" |president == "Carter"|president == "Clinton") #Create subset with all Democrats except Obama



#Compare Obama to Johnson
johnson <- johnson$middlewordper #Create variable for Johnson's middle class rhetoric
obama <- obama$middlewordper #Create variable for Obama's middle class rhetoric

johnsonobama <- t.test(obama,johnson) #Two sample t-test
johnsonobama #View results



#Compare Obama to Carter
carter <- carter$middlewordper #Create variable for Carter's middle class rhetoric

carterobama <- t.test(obama,carter) #Two sample t-test
carterobama #View results



#Compare Obama to Clinton
clinton <- clinton$middlewordper #Create variable for Clinton's middle class rhetoric

clintonobama <- t.test(obama,clinton) #Two sample t-test
clintonobama #View results



#Compare Obama to Democrats
dems <- dems$middlewordper #Create variable for Democrat's middle class rhetoric

obamadems <- t.test(dems, mu = mean(obama)) #Clustered one sample t-test
obamadems #View results



#Compare Obama to Republicans
reps <- reps$middlewordper #Create variable for Republican's middle class rhetoric

obamareps <- t.test(reps, mu = mean(obama)) #Clustered one sample t-test
obamareps #View results





# REGRESSION ANALYSES FOR POVERTY RHETORIC, REMOVING ONE INSIGNIFICANT CONTROL AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------


#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model

termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor


#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "povwordper ~ obamadummy + gdp + percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m2 <- "povwordper ~ obamadummy + gdp + percentpoor + percentunemployed + electionspeech + congressdem + democratpres"
m3 <- "povwordper ~ obamadummy + gdp + percentpoor + percentunemployed + electionspeech + democratpres"
m4 <- "povwordper ~ obamadummy + percentpoor + percentunemployed + electionspeech + democratpres"
m5 <- "povwordper ~ obamadummy + percentpoor + percentunemployed + democratpres"
m6 <- "povwordper ~ obamadummy + percentunemployed + democratpres"
m7 <- "povwordper ~ obamadummy + percentunemployed"
m8 <- "povwordper ~ obamadummy"


formulas <- rbind(m1,m2, m3, m4, m5,m6, m7, m8)



#Define empty list and iterating variable for loop
povword.regressions <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps= 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  povword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  povword.boot.out$Variable <- rownames(povword.boot.out) #Make a column out of the rownames
  povword.boot.out$se <- (povword.boot.out$ci.CI.higher-povword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  povword.boot.out <- cbind(coeffs, povword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  povword.boot.out <- subset(povword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(povword.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = termspeeches) #Run model using .lm to get adjusted R^2
  povword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  povword.boot.out$N <- nrow(termspeeches) #Add N to dataframe
  povword.boot.out$Temp <- povword.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  povword.boot.out$Temp <- ifelse((povword.boot.out$Variable == "(Intercept)" | povword.boot.out$Variable == "obamadummy"), -1, povword.boot.out$Temp) #Replace p-values with negative one for non-control variables
  povword.boot.out$Remove <- ifelse(((povword.boot.out$Temp == max(povword.boot.out$Temp)) & povword.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  povword.boot.out <- subset(povword.boot.out, select = -c(Temp)) #Remove Temp column
  povword.regressions[[i]] <- povword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
names(povword.regressions) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:8, sep = "") #Name each element of the list
print(povword.regressions)





# REGRESSION ANALYSES FOR CIVIL RIGHTS RHETORIC, REMOVING ONE INSIGNIFICANT CONTROL AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor


#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "civilwordper ~ obamadummy + gdp + percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m2 <- "civilwordper ~ obamadummy + gdp + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m3 <- "civilwordper ~ obamadummy + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m4 <- "civilwordper ~ obamadummy + percentunemployed + electionspeech + congressdem + democratpres"
m5 <- "civilwordper ~ obamadummy + electionspeech + congressdem + democratpres"
m6 <- "civilwordper ~ obamadummy + electionspeech + democratpres"
m7 <- "civilwordper ~ obamadummy"


formulas <- rbind(m1, m2, m3, m4, m5, m6, m7)


#Define empty list and iterating variable for loop
civword.regressions <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civword.boot.out$Variable <- rownames(civword.boot.out) #Make a column out of the rownames
  civword.boot.out$se <- (civword.boot.out$ci.CI.higher-civword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civword.boot.out <- cbind(coeffs, civword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civword.boot.out <- subset(civword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civword.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = termspeeches) #Run model using .lm to get adjusted R^2
  civword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civword.boot.out$N <- nrow(termspeeches) #Add N to dataframe
  civword.boot.out$Temp <- civword.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  civword.boot.out$Temp <- ifelse((civword.boot.out$Variable == "(Intercept)" | civword.boot.out$Variable == "obamadummy"), -1, civword.boot.out$Temp) #Replace p-values with negative one for non-control variables
  civword.boot.out$Remove <- ifelse(((civword.boot.out$Temp == max(civword.boot.out$Temp)) & civword.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  civword.boot.out <- subset(civword.boot.out, select = -c(Temp)) #Remove Temp column
  civword.regressions[[i]] <- civword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
names(civword.regressions) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:7, sep = "") #Name each element of the list
print(civword.regressions)





# REGRESSION ANALYSES FOR MIDDLE CLASS RHETORIC, REMOVING ONE INSIGNIFICANT CONTROL AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor


#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "middlewordper ~ obamadummy + gdp + percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m2 <- "middlewordper ~ obamadummy + gdp + percentpoor + gdpgrowth + electionspeech + congressdem + democratpres"
m3 <- "middlewordper ~ obamadummy + gdp + percentpoor + electionspeech + congressdem + democratpres"
m4 <- "middlewordper ~ obamadummy + gdp + electionspeech + congressdem + democratpres"
m5 <- "middlewordper ~ obamadummy + gdp + congressdem + democratpres"
m6 <- "middlewordper ~ obamadummy + gdp + democratpres"
m7 <- "middlewordper ~ obamadummy + gdp"
m8 <- "middlewordper ~ obamadummy"

formulas <- rbind(m1, m2, m3, m4, m5, m6, m7, m8)


#Define empty list and iterating variable for loop
midword.regressions <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  midword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  midword.boot.out$Variable <- rownames(midword.boot.out) #Make a column out of the rownames
  midword.boot.out$se <- (midword.boot.out$ci.CI.higher-midword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  midword.boot.out <- cbind(coeffs, midword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  midword.boot.out <- subset(midword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(midword.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = termspeeches) #Run model using .lm to get adjusted R^2
  midword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  midword.boot.out$N <- nrow(termspeeches) #Add N to dataframe
  midword.boot.out$Temp <- midword.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  midword.boot.out$Temp <- ifelse((midword.boot.out$Variable == "(Intercept)" | midword.boot.out$Variable == "obamadummy"), -1, midword.boot.out$Temp) #Replace p-values with negative one for non-control variables
  midword.boot.out$Remove <- ifelse(((midword.boot.out$Temp == max(midword.boot.out$Temp)) & midword.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  midword.boot.out <- subset(midword.boot.out, select = -c(Temp)) #Remove Temp column
  midword.regressions[[i]] <- midword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
names(midword.regressions) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:8, sep = "") #Name each element of the list
print(midword.regressions)





# FINAL REGRESSION ANALYSES RESULTS FOR RHETORIC COMPILED (SEE TABLE 1) --------------------

#See regressions in Table 1
print(povword.regressions)
print(civword.regressions)
print(midword.regressions)





# REGRESSION ANALYSES FOR POVERTY RHETORIC FOR OBAMA'S PREDECESSORS, REMOVING ONE INSIGNIFICANT CONTROL AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor

exceptobamaspeeches <- subset(termspeeches, president != "Obama")

#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "povwordper ~ gdp + percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m2 <- "povwordper ~ percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m3 <- "povwordper ~ percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m4 <- "povwordper ~ percentunemployed + gdpgrowth + congressdem + democratpres"
m5 <- "povwordper ~ percentunemployed + gdpgrowth + democratpres"
m6 <- "povwordper ~ percentunemployed + democratpres"
m7 <- "povwordper ~ percentunemployed"

formulas <- rbind(m1,m2,m3,m4,m5,m6,m7)



#Define empty list and iterating variable for loop
povword.regressions2a <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobamaspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobamaspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  povword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  povword.boot.out$Variable <- rownames(povword.boot.out) #Make a column out of the rownames
  povword.boot.out$se <- (povword.boot.out$ci.CI.higher-povword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  povword.boot.out <- cbind(coeffs, povword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  povword.boot.out <- subset(povword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(povword.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobamaspeeches) #Run model using .lm to get adjusted R^2
  povword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  povword.boot.out$N <- nrow(exceptobamaspeeches) #Add N to dataframe
  povword.boot.out$Temp <- povword.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  povword.boot.out$Temp <- ifelse((povword.boot.out$Variable == "(Intercept)" | povword.boot.out$Variable == "obamadummy"), -1, povword.boot.out$Temp) #Replace p-values with negative one for non-control variables
  povword.boot.out$Remove <- ifelse(((povword.boot.out$Temp == max(povword.boot.out$Temp)) & povword.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  povword.boot.out <- subset(povword.boot.out, select = -c(Temp)) #Remove Temp column
  povword.regressions2a[[i]] <- povword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(povword.regressions2a)
names(povword.regressions2a) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:7, sep = "") #Name each element of the list





# REGRESSION ANALYSES FOR CIVIL RIGHTS RHETORIC FOR OBAMA'S PREDECESSORS, REMOVING ONE INSIGNIFICANT CONTROL AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor

exceptobamaspeeches <- subset(termspeeches, president != "Obama")

#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "civilwordper ~ gdp + percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m2 <- "civilwordper ~ gdp + percentpoor + percentunemployed + gdpgrowth + congressdem + democratpres"
m3 <- "civilwordper ~ gdp + percentpoor + percentunemployed + gdpgrowth + democratpres"
m4 <- "civilwordper ~ gdp + percentpoor + percentunemployed + democratpres"
m5 <- "civilwordper ~ gdp + percentpoor + democratpres"
m6 <- "civilwordper ~ gdp + democratpres"


formulas <- rbind(m1,m2,m3,m4,m5,m6)


#Define empty list and iterating variable for loop
civword.regressions2a <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobamaspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobamaspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civword.boot.out$Variable <- rownames(civword.boot.out) #Make a column out of the rownames
  civword.boot.out$se <- (civword.boot.out$ci.CI.higher-civword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civword.boot.out <- cbind(coeffs, civword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civword.boot.out <- subset(civword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civword.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobamaspeeches) #Run model using .lm to get adjusted R^2
  civword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civword.boot.out$N <- nrow(exceptobamaspeeches) #Add N to dataframe
  civword.boot.out$Temp <- civword.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  civword.boot.out$Temp <- ifelse((civword.boot.out$Variable == "(Intercept)" | civword.boot.out$Variable == "obamadummy"), -1, civword.boot.out$Temp) #Replace p-values with negative one for non-control variables
  civword.boot.out$Remove <- ifelse(((civword.boot.out$Temp == max(civword.boot.out$Temp)) & civword.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  civword.boot.out <- subset(civword.boot.out, select = -c(Temp)) #Remove Temp column
  civword.regressions2a[[i]] <- civword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
names(civword.regressions2a) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:6, sep = "") #Name each element of the list
print(civword.regressions2a)





# REGRESSION ANALYSES FOR MIDDLE CLASS RHETORIC FOR OBAMA'S PREDECESSORS, REMOVING ONE INSIGNIFICANT CONTROL AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor

exceptobamaspeeches <- subset(termspeeches, president != "Obama")

#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "middlewordper ~ gdp + percentpoor + percentunemployed + gdpgrowth + electionspeech + congressdem + democratpres"
m2 <- "middlewordper ~ gdp + percentpoor + percentunemployed + electionspeech + congressdem + democratpres"
m3 <- "middlewordper ~ gdp + percentpoor + gdpgrowth + electionspeech + democratpres"
m4 <- "middlewordper ~ gdp + percentpoor + electionspeech + democratpres"
m5 <- "middlewordper ~ gdp + electionspeech + democratpres"
m6 <- "middlewordper ~ gdp + electionspeech"
m7 <- "middlewordper ~ gdp"

formulas <- rbind(m1,m2,m3,m4,m5,m6,m7)


#Define empty list and iterating variable for loop
midword.regressions2a <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobamaspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobamaspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  midword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  midword.boot.out$Variable <- rownames(midword.boot.out) #Make a column out of the rownames
  midword.boot.out$se <- (midword.boot.out$ci.CI.higher-midword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  midword.boot.out <- cbind(coeffs, midword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  midword.boot.out <- subset(midword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(midword.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobamaspeeches) #Run model using .lm to get adjusted R^2
  midword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  midword.boot.out$N <- nrow(exceptobamaspeeches) #Add N to dataframe
  midword.boot.out$Temp <- midword.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  midword.boot.out$Temp <- ifelse((midword.boot.out$Variable == "(Intercept)" | midword.boot.out$Variable == "obamadummy"), -1, midword.boot.out$Temp) #Replace p-values with negative one for non-control variables
  midword.boot.out$Remove <- ifelse(((midword.boot.out$Temp == max(midword.boot.out$Temp)) & midword.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  midword.boot.out <- subset(midword.boot.out, select = -c(Temp)) #Remove Temp column
  midword.regressions2a[[i]] <- midword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
names(midword.regressions2a) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:7, sep = "") #Name each element of the list
print(midword.regressions2a)





# CALCULATE OBAMA'S PREDICTED VERSUS ACTUAL RHEOTORIC-------------------------------------------------

termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in necessary dataset
termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to factor

exceptobamaspeeches <- subset(termspeeches, president != "Obama")

#Store the regression models to be used for prediction; models with the best fit for Obama's predcessors were chosen from above analyses
povwordprediction <- lm(povwordper ~ percentunemployed, data = exceptobamaspeeches) #Store models that will be used for predictions
civwordprediction <- lm(civilwordper ~ gdp + democratpres, data = exceptobamaspeeches)
midwordprediction <- lm(middlewordper ~ gdp, data = exceptobamaspeeches) 


#Subset Obama's years
termspeeches <- read.csv("ObamaDescriptiveTermRhetoricData.csv", header = TRUE) #Reread in required data files
obama <- subset(termspeeches, president == "Obama") #Subset


#Predict Obama's poverty rhetoric using the models
mypovdata <- data.frame(percentunemployed = obama$percentunemployed) #Generate data from Obama's years to put in the model
povwordpredict <- predict.lm(povwordprediction, mypovdata, interval = "confidence", level = .95) #Get predictions
povwordpredict <- data.frame(povwordpredict) #Store predictions in a data frame
povwordpredict$actual <- obama$povwordper #Add Obama's actual spending to the dataframe
povwordpredict$term <- obama$term #Add Obama's budget year to the dataframe
povwordpredict #View the dataframe


#Predict Obama's civil rights rhetoric using the models
mycivdata <- data.frame(democratpres = obama$democratpres, gdp = obama$gdp)
civwordpredict <- predict.lm(civwordprediction, mycivdata, interval = "confidence", level = .95) #Get predictions
civwordpredict <- data.frame(civwordpredict) #Store predictions in a data frame
civwordpredict$actual <- obama$civilwordper #Add Obama's actual spending to the dataframe
civwordpredict$term <- obama$term #Add Obama's budget year to the dataframe
civwordpredict #View the dataframe


#Predict Obama's middle class rhetoric using the models
mymiddata <- data.frame(gdp = obama$gdp)
midwordpredict <- predict.lm(midwordprediction, mymiddata, interval = "confidence", level = .95) #Get predictions
midwordpredict <- data.frame(midwordpredict) #Store predictions in a data frame
midwordpredict$actual <- obama$middlewordper #Add Obama's actual spending to the dataframe
midwordpredict$term <- obama$term #Add Obama's budget year to the dataframe
midwordpredict #View the dataframe





# PLOT OBAMA’S PREDICTED VS. ACTUAL RHETORIC -----------------------------------------


#Plot the predictions and actual rhetoric

dev.off() #Reset graphical parameters to default
par(mfrow=c(3,1), mai = c(.3, 0.5, .5, 0.2), oma = c(.2, 2.1, .1, 0.2)) #Set aesthetic parameters for graph
par(family = "serif") #Set text

#Plot predicted versus actual poverty rhetoric
plotCI(povwordpredict$term, povwordpredict$fit, xlim = c(.75, 2.25), ylim = c(5, 47), xaxt = "n", main = "Poverty Rhetoric", xlab = "", ylab = "Words Per 10,000", li = povwordpredict$lwr, ui = povwordpredict$upr) 
points(povwordpredict$term, povwordpredict$actual, pch = 16)
lines(povwordpredict$term, povwordpredict$fit, lty = 2)
lines(povwordpredict$term, povwordpredict$actual, lty = 1)
legend("topleft", lty = c(1, 1), bty = "n", c("Obama's Predicted Rhetoric", "Obama's Actual Rhetoric"), pch = c(21, 16))
axis(1, at = c(1, 2), line = .25, lwd = 0, c(" \nFirst\nTerm", " \nSecond\nTerm"))
axis(1, at = c(1, 2), tck = -.015, labels = NA)
text(.25, .25, labels = 'Percent of Words', xpd = NA, srt = 90, cex = 1.1)

#Plot predicted versus actual civil rights rhetoric
plotCI(civwordpredict$term, civwordpredict$fit, xlim = c(.5, 2.5), ylim = c(1, 11), xaxt = "n", main = "Minority & Civil Rights Rhetoric", ylab = "Words Per 10,000", xlab = "Obama's Term", li = civwordpredict$lwr, ui = civwordpredict$upr)
points(civwordpredict$term, civwordpredict$actual, pch = 16)
lines(civwordpredict$term, civwordpredict$fit, lty = 2)
lines(civwordpredict$term, civwordpredict$actual, lty = 1)
legend("topleft", lty = c(1, 1), bty = "n", c("Obama's Predicted Rhetoric", "Obama's Actual Rhetoric"), pch = c(21, 16))
axis(1, at = c(1, 2), line = .25, lwd = 0, c(" \nFirst\nTerm", " \nSecond\nTerm"))
axis(1, at = c(1, 2), tck = -.015, labels = NA)

#Plot predicted versus actual middle class rhetoric
plotCI(midwordpredict$term, midwordpredict$fit, ylim = c(15, 35), xlim = c(.5, 2.5), xaxt = "n", main = "Middle Class Rhetoric", ylab = "Words Per 10,000", xlab = "", li = midwordpredict$lwr, ui = midwordpredict$upr) ## Graph interaction effects of national identity for ethnocentrism
points(midwordpredict$term, midwordpredict$actual, pch = 16)
lines(midwordpredict$term, midwordpredict$fit, lty = 2)
lines(midwordpredict$term, midwordpredict$actual, lty = 1)
legend("topleft", lty = c(1, 1), bty = "n", c("Obama's Predicted Rhetoric", "Obama's Actual Rhetoric"), pch = c(21, 16))
axis(1, at = c(1, 2), line = .25, lwd = 0, c(" \nFirst\nTerm", " \nSecond\nTerm"))
axis(1, at = c(1, 2), tck = -.015, labels = NA)





# PLOT YEARLY DISCRETIONARY ANTI-POVERTY SPENDING (SEE FIGURE 3)------------------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingDataVer3Final.csv", header = TRUE) #Reread in required data file


dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text


x <- yearlyspending$budgetyear #Establish variables to graph
povtot <- yearlyspending$povertytot
povdisc <- yearlyspending$povertydisc
poor <- yearlyspending$perpoor
unemploy <- yearlyspending$perunemployed
domestic <- yearlyspending$povertydomestic*100 #Convert from proportion to percentage



#Plot first line graph: total anti-poverty spending
plot(x, povtot, type = 'l', yaxt = 'n', lty=1, ylim = c(-7000, 220000), xlim = c(1970, 2017), ylab = "Millions of 2015 Dollars", xlab = "Year", xaxt="n", cex.axis = .95, main = "Total Proposed Anti-Poverty Spending by Year and President")
points(x, povtot, pch = 16) #Add points for each year

axis(2, at = c(0, 50000, 100000, 150000, 200000), labels = c("0K", "", "100K", "", "200K")) #Create y-axis label
axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label   

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, -6000, "Nixon", cex = .90) #Label each presidential administration
text(1976.5, -6000, "Ford", cex = .90)
text(1979.5, -6000, "Carter", cex = .90)
text(1985.5, -6000, "Reagan", cex = .90)
text(1991.5, -6000, "H.W. Bush", cex = .90)
text(1997.5, -6000, "Clinton", cex = .90)
text(2005.5, -6000, "W. Bush", cex = .90)
text(2013.5, -6000, "Obama", cex = .90)



#Plot second line graph: anti-poverty spending per poor and unemployed persons
plot(x, poor, type = 'l', lty=1, col = "black", ylim = c(-1000, 20000), xlim = c(1970, 2017), ylab = "2015 Dollars", xlab = "Year", xaxt="n", yaxt = "n", cex.axis = .95, main = "Total Proposed Anti-Poverty Spending Per Person by Year and President")
points(x, poor, pch = 16, col = "black") #Add points for per poor person spending for each year

lines(x, unemploy, lty = 2, col = "gray41") #Add line showing per unemployed person anti-poverty spending
points(x, unemploy, pch = 16, col = "gray41") #Add points for per unemployed person spending for each year

legend("topleft", pch = c(16, 16), lty = c(1, 1), col= c("black", "gray41"), c("Per Poor Person", "Per Unemployed Person"), bty = "n") #Create legend

axis(2, at = c(0, 5000, 10000, 15000, 20000), labels = c("0", "", "10K", "", "20K"))
axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, -900, "Nixon", cex = .90) #Label each presidential administration
text(1976.5, -900, "Ford", cex = .90)
text(1979.5, -900, "Carter", cex = .90)
text(1985.5, -900, "Reagan", cex = .90)
text(1991.5, -900, "H.W. Bush", cex = .90)
text(1997.5, -900, "Clinton", cex = .90)
text(2005.5, -900, "W. Bush", cex = .90)
text(2013.5, -900, "Obama", cex = .90)



#Plot third line graph: anti-poverty spending as a percent of the proposed domestic budget
plot(x, domestic, type = 'l', lty=1, ylim = c(3, 35), xlim = c(1970, 2017), ylab = "Percentage", xlab = "Year", xaxt="n", cex.axis = .95, main = "Proposed Discretionary Anti-Poverty Spending as a Percent of the\nProposed Discretionary Domestic Budget by Year and President")
points(x, domestic, pch = 16) #Add points for each year

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Add x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, 3.2, "Nixon", cex = .9) #Label each presidential administration
text(1976.5, 3.2, "Ford", cex = .9)
text(1979.5, 3.2, "Carter", cex = .9)
text(1985.5, 3.2, "Reagan", cex = .9)
text(1991.5, 3.2, "H.W. Bush", cex = .9)
text(1997.5, 3.2, "Clinton", cex = .9)
text(2005.5, 3.2, "W. Bush", cex = .9)
text(2013.5, 3.2, "Obama", cex = .9)


#Figure should be 6.5x8.8





# PLOT YEARLY DISCRETIONARY CIVIL RIGHTS SPENDING (SEE FIGURE 4)------------------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data file


dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text


x <- yearlyspending$budgetyear #Establish variables to graph
tot <- yearlyspending$civil
blacks <- yearlyspending$perblack
domestic <- yearlyspending$civildomestic*100 #Convert from proportion to percentage



#Plot first line graph: total civil rights spending
plot(x, tot, type = 'l', lty=1, ylim = c(0, 2100), xlim = c(1970, 2017), ylab = "Millions of 2015 Dollars", xlab = "Year", xaxt="n", cex.axis = .95, main = "Total Proposed Minority & Civil Rights Spending by Year and President")
points(x, tot, pch =16)

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label   

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, 5, "Nixon", cex = .90) #Create labels for presidents
text(1976.5, 5, "Ford", cex = .90)
text(1979.5, 5, "Carter", cex = .90)
text(1985.5, 5, "Reagan", cex = .90)
text(1991.5, 5, "H.W. Bush", cex = .90)
text(1997.5, 5, "Clinton", cex = .90)
text(2005.5, 5, "W. Bush", cex = .90)
text(2013.5, 5, "Obama", cex = .90)



#Plot second line graph: civil rights spending per black person
plot(x, blacks, type = 'l', lty=1, ylim = c(10, 50), xlim = c(1970, 2017), ylab = "2015 Dollars", xlab = "Year", xaxt="n", cex.axis = .95, main = "Total Proposed Minority & Civil Rights Spending Per African American by Year and President")
points(x, blacks, pch = 16, col = "black") #Add points for per poor person spending for each year

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, 10.1, "Nixon", cex = .90) #Create labels for presidents
text(1976.5, 10.1, "Ford", cex = .90)
text(1979.5, 10.1, "Carter", cex = .90)
text(1985.5, 10.1, "Reagan", cex = .90)
text(1991.5, 10.1, "H.W. Bush", cex = .90)
text(1997.5, 10.1, "Clinton", cex = .90)
text(2005.5, 10.1, "W. Bush", cex = .90)
text(2013.5, 10.1, "Obama", cex = .90)



#Plot third line graph: civil rights spending as a percent of the proposed domestic budget
plot(x, domestic, type = 'l', lty=1, ylim = c(.0, .40), xlim = c(1970, 2017), ylab = "Percentage", xlab = "Year", xaxt="n", cex.axis = .95, main = "Total Proposed Minority & Civil Rights Spending as a Percent of the\nProposed Discretionary Domestic Budget by Year and President")
points(x, domestic, pch = 16) #Add points for each year

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Add x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, .001, "Nixon", cex = .90)
text(1976.5, .001, "Ford", cex = .90)
text(1979.5, .001, "Carter", cex = .90)
text(1985.5, .001, "Reagan", cex = .90)
text(1991.5, .001, "H.W. Bush", cex = .90)
text(1997.5, .001, "Clinton", cex = .90)
text(2005.5, .001, "W. Bush", cex = .90)
text(2013.5, .001, "Obama", cex = .90)






# PLOT PER ADMINISTRTAION AVERAGE ANTI-POVERTY SPENDING (SEE FIGURE 5) -------------------------------

adminspending <- read.csv("ObamaDescriptiveAdminSpendingData.csv", header = TRUE)  #Reread in necessary dataset

dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text


tot <- adminspending$povertytot #Define variables to graph
poor <- adminspending$perpoor
unemploy <- adminspending$perunemployed
domestic <- adminspending$povertydomestic * 100


#Plot first bargraph: per term average of total anti-poverty spending
barplot(tot, ylim = c(0, 200000), yaxt = "n", main = "Yearly Average in Total Proposed Discretionary Anti-Poverty Spending by President", ylab = "Millions of 2015 Dollars", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", "Obama"), col = "gray")
abline(h=0, col = "black", lwd = 1.5) #Create x-axis line
axis(2, at = c(0, 50000, 100000, 150000, 200000), labels = c("0K", "", "100K", "", "200K")) #Create y-axis labels




#Plot second bargraph: per term average for anti-poverty spending per poor and unemployed persons
mydata <- as.matrix(rbind(poor, unemploy)) #Organize data so that bars for per poor and per unemployed can be graphed side-by-side
legcol <- gray.colors(2, start = .8, end = .3, gamma = 2.2, alpha = NULL) #Set colors for bargraph

barplot(mydata, ylim = c(0, 20000), beside = TRUE, yaxt = "n", main = "Yearly Average in Total Proposed Discretionary Anti-Poverty Spending Per Person by President", ylab = "2015 Dollars", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", "Obama"), col = legcol)
abline(h=0, col = "black", lwd = 1.5) #Create x-axis line
legend("topleft", bty = "n", c("Per Poor Person","Per Unemployed Person"), col = legcol, pch = c(16, 16)) #Create legend

axis(2, at = c(0, 5000, 10000, 15000, 20000), labels = c("0K", "", "10K", "", "20K")) #Create y-axis label




#Plot third bargraph: per term average for anti-poverty spending as a percent of the proposed domestic budget
barplot(domestic, ylim = c(0, 30), main = "Yearly Average in Proposed Discretionary Anti-Poverty Spending as a Percent\nof the Proposed Discretionary Domestic Budget by President", ylab = "Percentage", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", "Obama"), col = "gray")
abline(h=0, col = "black", lwd = 1.5) #Create x-axis line





# PLOT PER ADMINISTRTAION AVERAGE CIVIL RIGHTS SPENDING (SEE FIGURE 6) -------------------------------

adminspending <- read.csv("ObamaDescriptiveAdminSpendingData.csv", header = TRUE)  #Reread in necessary dataset


dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text


tot <- adminspending$civil #Define variables to graph
blacks <- adminspending$perblack
domestic <- adminspending$civildomestic * 100

#Plot first bargraph: per term average of total civil rights spending
barplot(tot, ylim = c(0, 2000), main = "Yearly Average in Total Proposed Minority & Civil Rights Spending by President", ylab = "Millions of 2015 Dollars", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", "Obama"), col = "gray")
abline(h=0) #Create x-axis line


#Plot second bargraph: per term average for civil rights spending black person
barplot(blacks, ylim = c(0, 40), main = "Yearly Average in Total Proposed Minority & Civil Rights\nSpending Per African American by President", ylab = "2015 Dollars", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", "Obama"), col = "gray")
abline(h=0) #Create x-axis line



#Plot third bargraph: per term average for civil rights spending as a percent of the proposed domestic budget
barplot(domestic, ylim = c(0, .32), main = "Yearly Average in Total Proposed Minority & Civil Rights Spending as a Percent\nof the Proposed Discretionary Domestic Budget by President", ylab = "Percentage", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", "Obama"), col = "gray")
abline(h=0) #Create x-axis line








# REGRESSION ANALYSES FOR POVERTY SPENDING (SEE TABLE 2; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingDataVer3Final.csv", header = TRUE) #Reread in necessary dataset
yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor


#Run each model using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "povertytot ~ obamadummy"
m2 <- "povertytot ~ obamadummy + gdp + electionbudget"
m3 <- "povertytot ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + democrapres"
m4 <- "povertytot ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + democrapres + congressdem*democratpres"

formulas <- rbind(m1, m2, m3, m4)


#Define empty list and iterating variable for loop
poverty.regressions1 <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  poverty.regressions1[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(poverty.regressions1) #View the list
names(poverty.regressions1) <- paste(c("Obama Dummy ", "Obama Dummy + Sig Controls ", "Obama + Best Fit ", "Obama Dummy + Full Conrols "), 1:4, sep = "") #Name each element of the list







# REGRESSION ANALYSES FOR CIVIL RIGHTS SPENDING (SEE TABLE 2; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in necessary dataset
yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor


#Run each model using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "civil ~ obamadummy"
m2 <- "civil ~ obamadummy + gdp"
m3 <- "civil ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + democrapres + congressdem*democratpres"

formulas <- rbind(m1, m2, m3)


#Define empty list and iterating variable for loop
civil.regressions1 <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  
  civil.regressions1[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(civil.regressions1) #View the list
names(civil.regressions1) <- paste(c("Obama Dummy ", "Obama Dummy + Sig Controls ", "Obama Dummy + Best Fit/Full Controls "), 1:4, sep = "") #Name each element of the list




# REGRESSION ANALYSES FOR OBAMA'S PREDECESSORS' POVERTY SPENDING, REMOVING ONE INSIGNIFICANT VARIABLE AT A TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data files

exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset
exceptobama$presidentid <- as.factor(exceptobama$presidentid) #Make clustering variable a factor


#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "povertytot ~  gdp + percentpoor + percentunemployed + gdpgrowth + electionbudget + congressdem + democratpres + congressdem*democratpres"
m2 <- "povertytot ~  gdp + percentpoor + percentunemployed + electionbudget + congressdem + democratpres + congressdem*democratpres"

formulas <- rbind(m1,m2)


#Define empty list and iterating variable for loop
poverty.regressions2a <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobama) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobama, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobama) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$N <- nrow(exceptobama) #Add N to dataframe
  poverty.boot.out$Temp <- poverty.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  poverty.boot.out["congressdem", "Temp"] <- ifelse((poverty.boot.out["congressdem:democratpres", "PValue"] <  poverty.boot.out["congressdem", "PValue"]), -1, poverty.boot.out["congressdem", "PValue"]) #Replace p-values with negative one if main effects are less significant than interaction since they must remain in the model if the interaction is still there
  poverty.boot.out["democratpres", "Temp"] <- ifelse((poverty.boot.out["congressdem:democratpres", "PValue"] <  poverty.boot.out["democratpres", "PValue"]), -1, poverty.boot.out["democratpres", "PValue"]) #Replace p-values with negative one if main effects are less significant than interaction since they must remain in the model if the interaction is still there
  poverty.boot.out <- subset(poverty.boot.out, Variable != "NA") #Remove blank rows created if the main effects no longer exist in the model; The previous two lines will add a row of NA's if one or both of the main effects are no longer in the model
  poverty.boot.out$Temp <- ifelse((poverty.boot.out$Variable == "(Intercept)" | poverty.boot.out$Variable == "obamadummy"), -1, poverty.boot.out$Temp) #Replace p-values with negative one for non-control variables
  poverty.boot.out$Temp <- ifelse(is.na(poverty.boot.out$Temp) == TRUE, poverty.boot.out$PValue, poverty.boot.out$Temp) #Only replace p-values for main effects if the interaction term is still is in the model; If the interaction term is no longer in the model we need to compare the p-values of the main effects to other controls
  poverty.boot.out$Remove <- ifelse(((poverty.boot.out$Temp == max(poverty.boot.out$Temp)) & poverty.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  poverty.boot.out <- subset(poverty.boot.out, select = -c(Temp)) #Remove Temp column
  poverty.regressions2a[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 



#View the results; Note that the final model is identitical to Table 2, Model 2
names(poverty.regressions2a) <- paste(c("Ver", "Ver"), 1:2, sep = "") #Name each element of the list
print(poverty.regressions2a)





# CALCULATE OBAMAS PREDICTED VS. ACTUAL ANTI-POVERTY SPENDING --------------

#Call the regression model for Obama's predcessors

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data files
exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset

povertypredict <- lm(povertytot ~  gdp + percentpoor + percentunemployed + electionbudget + congressdem + democratpres + congressdem*democratpres, data = exceptobama)


#Subset Obama's years
yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data files
obama <- subset(yearlyspending, president == "Obama") #Subset Obama's spending


#Predict Obama's spending using the model
mydata <- data.frame(gdp = obama$gdp, percentpoor = obama$percentpoor, percentunemployed = obama$percentunemployed, electionbudget = obama$electionbudget, congressdem = obama$congressdem, democratpres = obama$democratpres) #Generate data from Obama's years to use in the model
povertyprediction <- predict.lm(povertypredict, mydata, interval = "confidence", level = .95) #Get predictions
povertyprediction <- data.frame(povertyprediction) #Store predictions in a data frame
povertyprediction$actual <- obama$povertytot #Add Obama's actual spending to the dataframe
povertyprediction$budgetyear <- obama$budgetyear #Add Obama's budget years to the dataframe 
povertyprediction #view the dataframe






# REGRESSION ANALYSES FOR CIVIL RIGHTS SPENDING BY OBAMA'S PREDCESSORS, REMOVING ONE INSIGNIFICANT VARIABLE AT TIME (NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data files

exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset
exceptobama$presidentid <- as.factor(exceptobama$presidentid) #Make clustering variable a factor

#Remove insignificant factors one at a time using a loop

#Create a dataframe of formulas to input into loop; see results to verify that the controls were correctly removed in each model
m1 <- "civil ~  gdp + percentpoor + percentunemployed + gdpgrowth + electionbudget + congressdem + democratpres + congressdem*democratpres"
m2 <- "civil ~  gdp + percentpoor + percentunemployed + electionbudget + congressdem + democratpres + congressdem*democratpres"
m3 <- "civil ~  gdp + percentpoor + percentunemployed + congressdem + democratpres + congressdem*democratpres"
m4 <- "civil ~  gdp + percentunemployed + congressdem + democratpres + congressdem*democratpres"
m5 <- "civil ~  gdp + percentunemployed + congressdem + democratpres"
m6 <- "civil ~  gdp + percentunemployed + congressdem"
m7 <- "civil ~  gdp + percentunemployed"
m8 <- "civil ~  gdp"


formulas <- rbind(m1,m2,m3,m4,m5,m6,m7,m8)


#Define empty list and iterating variable for loop
civil.regressions2a <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobama) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobama, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps=1000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobama) #Run model using .lm to get adjusted R^2
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(exceptobama) #Add N to dataframe
  civil.boot.out$Temp <- civil.boot.out$PValue #Create temporary column to be used to identify the max p-value from the controls
  civil.boot.out["congressdem", "Temp"] <- ifelse((civil.boot.out["congressdem:democratpres", "PValue"] <  civil.boot.out["congressdem", "PValue"]), -1, civil.boot.out["congressdem", "PValue"]) #Replace p-values with negative one if main effects are less significant than interaction since they must remain in the model if the interaction is still there
  civil.boot.out["democratpres", "Temp"] <- ifelse((civil.boot.out["congressdem:democratpres", "PValue"] <  civil.boot.out["democratpres", "PValue"]), -1, civil.boot.out["democratpres", "PValue"]) #Replace p-values with negative one if main effects are less significant than interaction since they must remain in the model if the interaction is still there
  civil.boot.out <- subset(civil.boot.out, Variable != "NA") #Remove blank rows created if the main effects no longer exist in the model; The previous two lines will add a row of NA's if one or both of the main effects are no longer in the model
  civil.boot.out$Temp <- ifelse((civil.boot.out$Variable == "(Intercept)" | civil.boot.out$Variable == "obamadummy"), -1, civil.boot.out$Temp) #Replace p-values with negative one for non-control variables
  civil.boot.out$Temp <- ifelse(is.na(civil.boot.out$Temp) == TRUE, civil.boot.out$PValue, civil.boot.out$Temp) #Only replace p-values for main effects if the interaction term is still is in the model; If the interaction term is no longer in the model we need to compare the p-values of the main effects to other controls
  civil.boot.out$Remove <- ifelse(((civil.boot.out$Temp == max(civil.boot.out$Temp)) & civil.boot.out$Temp > .05), "LeastSig", "") #Identify the control variable with the largest p-value
  civil.boot.out <- subset(civil.boot.out, select = -c(Temp)) #Remove Temp column
  civil.regressions2a[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 



#View the results; Note that the final model is identitical to Table 2, Model 2
names(civil.regressions2a) <- paste(c("Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver", "Ver"), 1:8, sep = "") #Name each element of the list
print(civil.regressions2a)





# CALCULATE OBAMAS PREDICTED VS. ACTUAL CIVIL RIGHTS SPENDING --------------

#Call the regression model for Obama's predcessors
yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data files
exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset

civilpredict <- lm(civil ~ gdp, data = exceptobama)  #Store the model that will be used to generate predictions


#Subset Obama's years
yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingData.csv", header = TRUE) #Reread in required data files
obama <- subset(yearlyspending, president == "Obama") #Subset


#Predict Obama's spending using the model
mydata <- data.frame(gdp = obama$gdp) #Generate data from Obama's years to put in the model
civilprediction <- predict.lm(civilpredict, mydata, interval = "confidence", level = .95) #Get predictions
civilprediction <- data.frame(civilprediction) #Store predictions in a data frame
civilprediction$actual <- obama$civil #Add Obama's actual spending to the dataframe
civilprediction$budgetyear <- obama$budgetyear #Add Obama's budget years to the dataframe
civilprediction #view the dataframe





# PLOT OBAMAS PREDICTED VS. ACTUAL ANTI-POVERTY & CIVIL RIGHTS SPENDING (SEE FIGURE 7) --------

dev.off() #Reset graphical parameters to default
par(mfrow = c(2, 1)) #Plot two stacked graphs
par(family = "serif") #Set text

povertyprediction #View the dataframe that will be used for graphing


plotCI(povertyprediction$budgetyear, povertyprediction$fit, ylim = c(90000, 220000), yaxt = "n", ylab = "Millions of 2015 Dollars", xlab = "Year", li = povertyprediction$lwr, ui = povertyprediction$upr, cex.axis = .85, cex.lab = .85) #Plot predictions with predictions invtervals
points(povertyprediction$budgetyear, povertyprediction$actual, pch = 16) #Add points for predicted spending
lines(povertyprediction$budgetyear, povertyprediction$fit, lty = 2) #Add a line for predicted spending
lines(povertyprediction$budgetyear, povertyprediction$actual, lty = 1) #Add line for spending spending
legend("topleft", bty = "n", lty = c(1, 1), c("Obama's Predicted Spending", "Obama's Actual Spending"), pch = c(21, 16), cex = .85) #Add a legend
axis(2, at = c(100000, 125000, 150000, 175000, 200000, 225000), c("100K", "125K", "150K", "175K", "200K", "225K"), cex.lab = .85, cex.axis = .85) #Add the x-axis labels



civilprediction #View the dataframe that will be used for graphing

plotCI(civilprediction$budgetyear, civilprediction$fit, ylim = c(1300, 1800), ylab = "Millions of 2015 Dollars", xlab = "Year", li = civilprediction$lwr, ui = civilprediction$upr, cex.axis = .85, cex.lab = .85) #Plot predictions with predictions invtervals
points(civilprediction$budgetyear, civilprediction$actual, pch = 16) #Add points for predicted spending
lines(civilprediction$budgetyear, civilprediction$fit, lty = 2) #Add a line for predicted spending
lines(civilprediction$budgetyear, civilprediction$actual, lty = 1) #Add line for actual spending
legend("topleft", bty = "n", lty = c(1, 1), c("Obama's Predicted Spending", "Obama's Actual Spending"), pch = c(21, 16), cex = .85) #Add a legend




# PEll GRANT CALCULATIONS (SEE FOOTNOTE 23) -----------------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingDataVer3Final.csv", header = TRUE) #Reread in required data file

yearlyspending$pellpercent <- yearlyspending$pell/yearlyspending$povertytot #Calculate percent of anti-poverty spending consumed by pell grants
pellpercent <- subset(yearlyspending, select = c("president", "budgetyear", "pellpercent")) #Isolate president, year, and percent pell grant variables for insepection
print(pellpercent)







# HISTORICALLY BLACK COLLEGES CALCULATIONS (SEE FOOTNOTE 30) -----------------------------

yearlyspending <- read.csv("ObamaDescriptiveYearlySpendingDataVer3Final.csv", header = TRUE) #Reread in required data file

yearlyspending$equalcommpercent <- yearlyspending$equalcomm/yearlyspending$civil #Calculate percent of anti-poverty spending consumed by pell grants
equalcommpercent <- subset(yearlyspending, select = c("president", "budgetyear", "equalcommpercent")) #Isolate president, year, and percent pell grant variables for insepection
print(equalcommpercent)


