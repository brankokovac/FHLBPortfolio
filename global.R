# Load required libraries
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)
library(maps)
library(mapproj)
library(plotly)

# # Import data from Federal Home Loan Banks 
# # http://www.fhfa.gov/DataTools/Downloads/Pages/FHLBank-Public-Use-Database-Previous-Years.aspx
# fhlb09 <- read.csv("AMA_PUDB_EXPORT_123109.csv")
# fhlb10 <- read.csv("AMA_PUDB_EXPORT_123110.csv")
# fhlb11 <- read.csv("AMA_PUDB_EXPORT_123111.csv")
# fhlb12 <- read.csv("AMA_PUDB_EXPORT_123112.csv")
# fhlb13 <- read.csv("AMA_PUDB_EXPORT_123113.csv")
# fhlb14 <- read.csv("AMA_PUDB_EXPORT_123114.csv")
# 
# # Import FIPS data
# # https://www.census.gov/geo/reference/codes/cou.html
# fips <- read.csv("FIPS.txt")
# 
# # Save data frames
# save(fhlb09, fhlb10, fhlb11, fhlb12, fhlb13, fhlb14, fips, file = "data/data0.Rds")
# 
# # Clear working memory
# rm(list = ls())

# Load data
load("data/data0.Rds")

# Create vector for state abbreviations and state names
states <- data.frame(
  state=c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
          "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
          "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
          "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
          "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
  full=c("alaska","alabama","arkansas","arizona","california","colorado",
         "connecticut","district of columbia","delaware","florida","georgia",
         "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
         "louisiana","massachusetts","maryland","maine","michigan","minnesota",
         "missouri","mississippi","montana","north carolina","north dakota",
         "nebraska","new hampshire","new jersey","new mexico","nevada",
         "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
         "rhode island","south carolina","south dakota","tennessee","texas",
         "utah","virginia","vermont","washington","wisconsin",
         "west virginia","wyoming")
)


# Correct column names to allow for rbind()
names(fhlb09)[2] <- "Loan.Number"
fhlb09$FHLBankID <- NA
names(fhlb14)[65] <- "Co.Borrower.Credit.Score"

# Bind yearly data sets into a single data frame
fhlb <- rbind(fhlb14, fhlb13, fhlb12, fhlb11, fhlb10, fhlb09)

# Join state and county data from FIPS to FHLB data
fhlb <- left_join(fhlb, fips, by = c("FIPSStateCode" = "STATEFP", "FIPSCountyCode" = "COUNTYFP"))

# Create region code
fhlb$region <- as.numeric(paste0(fhlb$FIPSStateCode, str_pad(fhlb$FIPSCountyCode, 3, pad = "0")))

fhlb$STATE <- as.character(fhlb$STATE)
states$state <- as.character(states$state)

# Join full state name from manual states vector
fhlb <- left_join(fhlb, states, by = c("STATE" = "state"))

# Some minor cleanup
names(fhlb)[names(fhlb) == 'full'] <- "STATENAME"
fhlb$STATENAME <- toupper(fhlb$STATENAME)
fhlb$Seller <- toupper(fhlb$Seller)
fhlb$Amount <- as.numeric(fhlb$Amount)

# Prepare Interest for Weighted Interest
fhlb$annInt <- fhlb$Amount * fhlb$Rate

# Select relevant variables for analaysis
portfolio <- select(fhlb, Year, Loan.Number, FHLBankID, Program, Income, IncRat, UPB, LTV, MortDate, AcquDate, Purpose, Term, AmorTerm, Seller, NumBor, First, BoRace, BoGender, BoAge, Rate, Amount, Borrower.Credit.Score, STATE, COUNTYNAME, region, STATENAME, annInt)

# Factorize Variables
portfolio$Purpose <- as.factor(portfolio$Purpose)
portfolio$Purpose <- factor(portfolio$Purpose, levels = c(1, 2, 3, 4, 5), labels = c("Purchase", "Refinancing", "Second Mortgage", "New Construction", "Rehabilitation"))

portfolio$First <- as.factor(portfolio$First)
portfolio$First <- factor(portfolio$First, levels = c(1, 2), labels = c(TRUE, FALSE))

portfolio$BoRace <- as.factor(portfolio$BoRace)
portfolio$BoRace <- factor(portfolio$BoRace, levels = c(1, 2, 3, 4, 5, 7),
                           labels = c("NativeAmerican", "Asian", "AfricanAmerican", "PacificIslander", "White", "NotSpecified"))

portfolio$BoGender <- as.factor(portfolio$BoGender)
portfolio$BoGender <- factor(portfolio$BoGender, levels = c(1, 2, 3),
                             labels = c("Male", "Female", "NotSpecified"))

portfolio$Borrower.Credit.Score <- as.factor(portfolio$Borrower.Credit.Score)
portfolio$Borrower.Credit.Score <- factor(portfolio$Borrower.Credit.Score,
                                          levels = c(1, 2, 3, 4, 5, 9),
                                          labels = c("< 620", "620 - 660", "660 - 700", "700 - 760", "> 760", "NotSpecified"))

portfolio$BoAge[portfolio$BoAge == 99] <- NA

# Prepare data frame for map
mortByState <- group_by(portfolio, STATE)
mortByState <- summarise(mortByState, totalMort = sum(Amount), totalInt = sum(annInt))
mortByState <- mortByState[mortByState$STATE != "PR", ]
mortByState <- mortByState[!is.na(mortByState$STATE), ]
names(mortByState) <- c('State', 'TotalMortgage', 'TotalInt')
mortByState$WgtRate <- as.numeric(mortByState$TotalInt) / as.numeric(mortByState$TotalMortgage)
mortByState$TotalInt <- NULL

mortByState$hover <- with(mortByState, paste0(
                    State, '<br>', "Total Mortgage Value: $",
                    prettyNum(TotalMortgage, big.mark = ","),
                    '<br>', "Weighted Avg Rate: ",
                    formatC(WgtRate * 100, digits = 4), "%"))

# Calculate total mortgage portfolio by county - used for state map
countySum <- summarise(group_by(portfolio, region), value = sum(Amount))

# Save data frame as Rda file for future loading
save(fhlb, fips, states, portfolio, mortByState, countySum, file = "data/data.Rds")

# Clear working memory
rm(list = ls())

load("data/data.Rds")

