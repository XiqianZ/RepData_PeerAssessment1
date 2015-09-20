setwd("~/GitHub/RepData_PeerAssessment1");
actData = read.csv("activity.csv",header=TRUE,as.is=TRUE);
actData = read.csv("activity.csv",header=TRUE);
actData[,2]=as.Date(actData[,2],"%Y-%m-%d");

#part 2
library(plyr);
dt=na.omit(actData);
stepsAnly=ddply(dt,~date,summarize,meanStep=mean(steps),totalStep=sum(steps));
stepsMedian=median(stepsAnly$totalStep);
stepsMean = mean(stepsAnly$totalStep);
hist(stepsAnly$totalStep,breaks = 10,main="Histogram of total steps per day",
     xlab="Total Steps");

#part 3
#average daily activities
stepPattern = ddply(dt,~interval,summarize,pattern=mean(steps));
plot(stepPattern$interval,stepPattern$pattern,type="l");
stepMaxInterval = stepPattern[which.max(stepPattern[,2]),1];
hour=floor(stepMaxInterval/100);
minute=stepMaxInterval%%100;

#part 4
#Imputing missing values
nNA = sum(is.na(actData[,1]));
dtf = actData;
dtf$steps[is.na(dtf$steps)]<-0;
stepsAnly=ddply(dtf,~date,summarize,meanStep=mean(steps),totalStep=sum(steps));
stepsMedian=median(stepsAnly$totalStep);
stepsMean = mean(stepsAnly$totalStep);
hist(stepsAnly$totalStep,breaks = 10,main="Histogram of total steps per day with NA filled",
     xlab="Total Steps");

#part 5
#weekdays vs. weekends
dtf$TimeOfWeek<-ifelse(weekdays(dtf$date,TRUE)=="“y"|weekdays(dtf$date,TRUE)=="“ú",
                       "weekends","weekdays")
stepPatternWD = ddply(dtf[dtf$TimeOfWeek=="weekdays",],~interval,summarize,pattern=mean(steps));
stepPatternWE = ddply(dtf[dtf$TimeOfWeek=="weekends",],~interval,summarize,pattern=mean(steps));
par(mfrow = c(2,1), mar=c(4,4,2,1));
plot(stepPatternWD$interval,stepPatternWD$pattern,type="l",ylim=c(0,200));
plot(stepPatternWE$interval,stepPatternWE$pattern,type="l",ylim=c(0,200));
