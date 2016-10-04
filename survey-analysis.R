# Los datos fueron extraidos de Kaggle
# Link: https://www.kaggle.com/freecodecamp/2016-new-coder-survey-

PATH <- '~/development/pap-machine-learning/class-01/'
FILENAME <- '2016-FCC-New-Coders-Survey-Data.csv'

survey <- read.csv("~/development/pap-machine-learning/class-01/2016-FCC-New-Coders-Survey-Data.csv")

library(ggplot2)
# people who prefers to work from home
getStudentsWhoWorksInSD <- function() {
  return (survey[survey$EmploymentField == 'software development', ])
}

getStudentsWhoPreferWFH <- function() {
  return (survey[survey$JobWherePref == 'from home', ])
}

getStudentsWhoAreFromUsa <- function() {
  return (survey[survey$CountryLive == 'United States of America', ])
}

getWomansWhoAreDevsAndHaveACompany <- function() {
  return (survey[survey$Gender == 'female' && survey$EmploymentStatus == 'Self-employed business owner', ])
}

getExMilitars <- function() {
  return (survey[survey$HasServedInMilitary == 1, ])
}

getPeopleProgrammingXYears <- function(years) {
  return (survey[survey$MonthsProgramming == (years * 12), ])
}

getUnemployeedAndHaveBeenEnrolledInELearning <- function() {
  return (survey[(survey$ResourceCodeCademy == 1 || survey$ResourceEdX == 1 ||
                 survey$ResourceLynda == 1 || survey$ResourceUdemy == 1 ||
                 survey$ResourceUdacity == 1 || survey$ResourceCodeWars == 1 ||
                 survey$ResourceHackerRank == 1 || survey$ResourceCoursera == 1) &&
            (survey$EmploymentStatus == 'Not working but looking for work' ||
            survey$EmploymentStatus == 'Not working and not looking for work'), ])
}

graphAgeFrequency <- function() {
  hist(survey$Age,
       main = 'Histogram for age frequency in survey',
       xlab = "Age",
       col = "blue",
       breaks = 15,
       xlim=c(15,60)
       )
}

graphGenderPieChart <- function() {
  gender = na.omit(survey$Gender)
  genderFreq = table(gender)
  pie(genderFreq,
      main = "Pie chart of age distribution")
}

graphJobInterestPieChart <- function() {
  jobInterest = survey$JobPref
  freq = table(jobInterest)
  freqDf = as.data.frame(freq)
  pie(freq,
      main = "Pie chart of job preferences distribution",
      col = rainbow(length(freqDf))
  )
}

nrow(getStudentsWhoWorksInSD())
nrow(getStudentsWhoPreferWFH())
nrow(getStudentsWhoAreFromUsa())
nrow(getWomansWhoAreDevsAndHaveACompany())
nrow(getExMilitars())
nrow(getPeopleProgrammingXYears(5))
nrow(getUnemployeedAndHaveBeenEnrolledInELearning())
nrow(survey)

graphAgeFrequency()
graphGenderPieChart()
graphJobInterestPieChart()

monthsProgramming = survey$MonthsProgramming
expectedEarning = survey$ExpectedEarning
actualIncome = survey$Income

plot(monthsProgramming,
     expectedEarning,
     xlab = "Months programming",
     ylab = "Expected earning",
     xlim = c(0, 500),
     main = "Money expectations vs Months programming"
     )

plot(monthsProgramming,
     expectedEarning,
     xlab = "Months programming",
     ylab = "Expected earning",
     xlim = c(0, 100),
     main = "Money expectations vs Months programming"
     )

plot(monthsProgramming,
     actualIncome,
     xlab = "Months programming",
     ylab = "actualIncome",
     xlim = c(0, 150),
     main = "Actual incomes vs Months programming"
     )
