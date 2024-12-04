graphics.off() # clear all previous plots
rm(list=ls()) # clear the environment from previous codes
cat("\014") # clear the console


library(readxl)
df <- read_excel("/Users/Trip/Documents/Clarkson/STAT383ProbabilityAndStatistics/Final Project/consolidated_data.xlsx")
View(df)
head(df)

# change a column name

colnames(df)[3] <- "different"
sum(df$vote=="Donald Trump")
sum(df$vote=="Kamala Harris")
sum(df$vote=="Other")
nrow(df)

# proportion of Donald Trump voters
print(sum(df$vote=="Donald Trump")/nrow(df))

# proportion of Kamala Harris voters
print(sum(df$vote=="Kamala Harris")/nrow(df))

# proportion of Other voters
print(sum(df$vote=="Other")/nrow(df))

# Subsetting a dataset
#
df_males <- df[df$gender==1,]

df_females <- df[df$gender==0,]

# proportion of Donald Trump voters amongst male students
print(sum(df_males$vote=="Donald Trump")/nrow(df_males))
print(sum(df_males$vote=="Kamala Harris")/nrow(df_males))
# proportion of Donald Trump voters amongst female students
print(sum(df_females$vote=="Donald Trump")/nrow(df_females))
print(sum(df_females$vote=="Kamala Harris")/nrow(df_females))


df_same <- df[df$different==0,]
df_different <- df[df$different==1,]
df_smales <- df_same[df_same$gender==1,]
df_sfemales <- df_same[df_same$gender==0,]
df_dmales <- df_different[df_different$gender==1,]
df_dfemales <- df_different[df_different$gender==0,]
a<-(sum(df_same$vote=="Donald Trump"))
b<-(sum(df_same$vote=="Kamala Harris"))
c<-(sum(df_different$vote=="Donald Trump"))
d<-(sum(df_different$vote=="Kamala Harris"))
e<-(sum(df_same$vote=="Other"))
f<-(sum(df_different$vote=="Other"))
g<-(sum(df_smales$vote=="Donald Trump"))
h<-(sum(df_smales$vote=="Kamala Harris"))
i<-(sum(df_dmales$vote=="Donald Trump"))
j<-(sum(df_dmales$vote=="Kamala Harris"))
k<-(sum(df_smales$vote=="Other"))
l<-(sum(df_dmales$vote=="Other"))
m<-(sum(df_sfemales$vote=="Donald Trump"))
n<-(sum(df_sfemales$vote=="Kamala Harris"))
o<-(sum(df_dfemales$vote=="Donald Trump"))
p<-(sum(df_dfemales$vote=="Kamala Harris"))
q<-(sum(df_sfemales$vote=="Other"))
r<-(sum(df_dfemales$vote=="Other"))
TruMp<-2*g+i+.5*j+m+.5*p
TruFEMp<-2*m+o+.5*p+.5*j+g
KaMala<-2*h+j+.5*i+n+.5*o
KaFEMala<-2*n+h+p+.5*o+.5*i
OtherM<-7
OtherF<-5
ToMtal<-(TruMp+KaMala+OtherM)
ToFtal<-(TruFEMp+KaFEMala+OtherF)
ToKal<- (.5091*KaMala/ToMtal+.4909*KaFEMala/ToFtal)
ToTral<- (.5091*TruMp/ToMtal+.4909*TruFEMp/ToFtal)
Total<-ToMtal+ToFtal
p1<-ToTral
p2<-ToKal
p11<-25210/42939
p22<-17502/42939
p3<-p1-p2
p33<-p11-p22
#Splitting up coding the Test Statistic into 2 lines to prevent bugs
botTS<-sqrt(p33*(1-p33)/(Total))
TS<-((p3)-(p33))/botTS
p_value <- 2 * (1 - pt(abs(TS), Total - 1))
print(p_value)
print(TS)
