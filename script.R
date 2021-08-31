
# Required Packages ------------------------------------------------------
listOfPackages <-c("tidyverse","skimr",
                   "magrittr", "modeest", "raster", "moments")

for (i in listOfPackages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  lapply(i, require, character.only = TRUE)
  rm(i)
}

# Importing Data set ------------------------------------------------------

data <- read_csv("dataset/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Exploring The Data -----------------------------------------------------

data %>% View()
summary(data)
str(data)
glimpse(data)
skim(data)


# Transforming Data set ----------------------------------------------------

attrition <- data
attach(attrition)

## Verifying it is a tibble object
class(attrition)

## Checking Na's values
sapply(attrition, function(x) sum(is.na(x)))
str(attrition)

## All type 'character' must be type 'factor'
attrition %<>% 
  mutate_if(is.character,as.factor)
str(attrition)

## We still have to convert some attributes and assign them heir respecting labels
attrition$Education <- as.factor(Education)
attrition$Education <-  factor(Education, levels = c(1,2,3,4,5), labels = c("Below College", "College", "Bachelor", "Master", "Doctor" ))
attrition$EnvironmentSatisfaction <-  factor(EnvironmentSatisfaction, levels = c(1,2,3,4), labels = c("Low", "Medium", "High", "Very High" ))
attrition$JobInvolvement <-  factor(JobInvolvement, levels = c(1,2,3,4), labels = c("Low", "Medium", "High", "Very High" ))
attrition$JobLevel <- as.factor(JobLevel)
attrition$JobSatisfaction <-  factor(JobSatisfaction, levels = c(1,2,3,4), labels = c("Low", "Medium", "High", "Very High" ))
attrition$PerformanceRating <-  factor(PerformanceRating, levels = c(1,2,3,4), labels = c("Low", "Good", "Excellent", "Outstanding" ))
attrition$RelationshipSatisfaction <-  factor(RelationshipSatisfaction, levels = c(1,2,3,4), labels = c("Low", "Medium", "High", "Very High"))
attrition$WorkLifeBalance <-  factor(WorkLifeBalance, levels = c(1,2,3,4), labels = c("Bad", "Good", "Better", "Best"))
str(attrition)

## Creating new attribute, grouping by age range
ageGroup <- rep(NA, nrow(attrition))
ageGroup[which(attrition$Age>50)]="51 and up"
ageGroup[which(attrition$Age>40 & attrition$Age<=50)]="41-50"
ageGroup[which(attrition$Age>30 & attrition$Age<=40)]="31-40"
ageGroup[which(attrition$Age>20 & attrition$Age<=30)]="21-30"
ageGroup[which(attrition$Age<=20)]="20 or Below"
attrition$ageGroup<-as.factor(ageGroup)



# Graphics ----------------------------------------------------------------
ggplot(attrition, aes(MonthlyIncome, fill = Attrition)) +
  geom_histogram(binwidth = 500, color = "black", alpha = .8)+
  geom_vline(aes(xintercept = mean(attrition$MonthlyIncome) ), color = "navyblue", linetype = "dashed", size = 1) +
  labs(title="Monthly Income Distribution", x = "Monthly Income", 
       y = "Quantity", caption = "Intercepting 'x' axis with arithmethic mean")
ggsave("distribution-monthly-income.png", path = "images/")


attrition %>%
  filter(YearsAtCompany < 21) %>%
  ggplot( aes(YearsAtCompany, MonthlyIncome)) +
  geom_jitter( aes(color = Attrition), alpha = .4) +
  geom_smooth() + 
  geom_hline(aes(yintercept = mean(MonthlyIncome)), color = "navyblue", linetype = "solid", size = .8) +
  geom_hline(aes(yintercept = median(MonthlyIncome)), color = "purple4", linetype = "twodash", size = .8) +
  geom_vline(aes(xintercept = 13 ), color = "navyblue", linetype = "dashed", size = .8) + 
  labs(title = "Montlhly Income per Working Years At Company", x = "Years At Company", 
       y = "Monthly Income")
ggsave("yearsAtCompany-vs-monthlyIncome.png", path = "images/")


attrition %>%
  filter(YearsAtCompany < 21 & Attrition == "Yes") %>%
  ggplot( aes(YearsAtCompany, MonthlyIncome)) +
  geom_jitter(color = "#7bf1a8")+
  geom_smooth(se = F) +
  labs(title = "Monthly Income per Working Years At Company", subtitle = "Employees that did leave the company", x = "Years At Company", y = "Monthly Income")
ggsave("YES-yearsAtCompany-vs-montlhyIncome.png", path = "images/")


attrition %>%
  filter(YearsAtCompany < 21 & Attrition == "No") %>%
  ggplot( aes(YearsAtCompany, MonthlyIncome)) +
  geom_jitter(color = "#ffadad") +
  geom_smooth(se = F) +
  labs(title = "Monthly Income per Working Years At Company", subtitle = "Employees that did not leave the company", x = "Years At Company", y = "Monthly Income")
ggsave("NO-yearsAtCompany-vs-montlhyIncome.png", path = "images/")


ggplot(attrition) + 
  geom_boxplot(aes(x = reorder(WorkLifeBalance, YearsAtCompany, FUN = median), y = YearsAtCompany, color = Attrition)) +
  labs(title = "Work Life Balance Vs Years At Company",
       subtitle ="According to 'Attrition' condition",
       x ="Work Life Balance", y ="Years At Comapany")
ggsave("workLifeBalance-vs-yearsAtcompany.png", path = "images/")


ggplot(attrition) + 
  geom_bar(aes(x = ageGroup, fill= JobInvolvement)) +
  labs(title = "Job Involvement By Each Age Group", x = "Age Group",
       y = "Quantity")
ggsave("jobInvolvement-vs-ageGroup.png", path = "images/")


ggplot(attrition) + 
  geom_bar(aes(x = JobInvolvement, fill = JobInvolvement)) +
  facet_wrap(~ageGroup,  scales = "free") + 
  labs(title = "Job Involvement By Each Age Group",
       x = "Job Involvement", y = "Quantity (free scale)") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(hjust = 0.5))
ggsave("FacetWrap-jobInvolvement-vs-ageGroup.png", path = "images/")


wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
#caption <- "Caption"
ggplot(attrition) +
  labs(title = "Job Involvement By Each Age Group",
       subtitle = "According to 'Attirion' condition",
       x = "Job Involvement", y = "Quantity") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),axis.text.x =   element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 7, face = "bold"),
        legend.title = element_text(hjust = 0.5, face = "bold" )) +
  guides(fill=guide_legend("Job Involvement Rating")) +
  geom_bar(aes(x = JobInvolvement, fill= JobInvolvement)) +
  facet_wrap(Attrition~ageGroup,  scales = "free") + 
  theme(text = element_text(size = 8))    
ggsave("FacetWrap-Attrition-jobInvolvement-vs-ageGroup.png", path = "images/")


attrition %>%
  mutate(Education = factor(Education, levels = c("Below College", "College", "Bachelor", "Master", "Doctor"))) %>%
  ggplot( aes(Education, fill = Attrition)) + 
  geom_bar(position = "fill") +
  labs(title="Proportion Of People Leaving The Company \n By Educational Level",
       x = "Educational Level", y = "Proportion")
ggsave("leaversProportion-EducationCategory.png", path = "images/")

ggplot(attrition, aes(HourlyRate)) +
  geom_histogram(bins = 12, color = "black", fill = "#f8f7ff") +
  facet_wrap(~ Gender) +
  labs(title = "Hourly Rate Distribution By Gender",
       x = "Hourly Rate")
ggsave("hourlyRate-Distribution-gender.png", path = "images/")


attrition %>%
  filter(YearsAtCompany < 21) %>%
  ggplot( aes( YearsAtCompany, HourlyRate)) +
  geom_col(fill = "#bdb2ff") +
  facet_wrap(~ Gender) +
  labs(title = "Hourly Rate Distribution Vs. Years At Company",
       subtitle = "According to gender", x = "Years At Company", y = "Hourly Rate")
ggsave("hourlyRateDistribution-yearsAtsCompany-Gender.png", path = "images/")


ggplot(attrition, aes(Gender, HourlyRate, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Hourly Rate Distribution According To Gender",
       x = "Gender", y = "Hourly Rate")
ggsave("BOXPLOT-hourlyRate-Distribution-gender.png", path = "images/")


attritionName <- c(`Yes` = "Did Abandoned Company",`No` = "Did Not Abandoned Company")
ggplot(attrition, aes(Gender, HourlyRate, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Hourly Rate Distribution By Gender", 
       subtitle = "According to 'Attrition' condition", x = "Gender", 
       y ="Hourly Rate") +
  facet_wrap(~ Attrition, labeller = as_labeller(attritionName))
ggsave("BOXPLOT-Attrition-HourlyRateByGenderDistribuition.png", path = "images/")

attrition %>%
  filter(HourlyRate > 55) %>%
  ggplot( aes(x = HourlyRate)) + 
  geom_density(aes(color = Gender)) +
  geom_vline(aes(xintercept = 64.5 ), color = "black", linetype = "dashed", size = .5) +
  labs(title = "Hourly Rate Density By Gender", 
       x = "Hourly Rate") 
ggsave("DENSITY-HourlyRateDistribuitionByGender.png", path = "images/")


ggplot(attrition, aes(BusinessTravel, fill = JobSatisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Job Travel According To Job Satisfaction",
       x = "Job Travel", y = "Quantity")
ggsave("jobTravel-jobSatisfaction.png", path = "images/")


ggplot(attrition, aes(BusinessTravel, fill = Attrition)) +
  geom_bar(position = "fill")  +
  labs(title = "Job Travel According To 'Attrition' Condition",
       x =  "Job Travel", y = "Proportion") 
ggsave("jobTravel-Attrition.png", path = "images/")


ggplot(attrition) +
  aes(JobLevel, fill = Attrition) +
  geom_bar(position = "dodge") +
  labs(title = "Job Level According To 'Attrition' Condition",
        x = "Job Level", y = "Quantity") 
ggsave("jobLevel-Attrition.png", path = "images/")


ggplot(attrition) +
  aes(x = JobLevel, fill = Attrition) +
  geom_bar(position = "fill") + 
  facet_grid( OverTime ~  BusinessTravel) +
  labs(title = "Job Level By Job Travel", subtitle = "According to Attrition' and 'Over Time' condition",
       x = "Job Level", y = "Proportion")
ggsave("jobLevel-jobTravel-Attrition-Overtime.png", path = "images/")



# Other Graphics ----------------------------------------------------------
ggplot(attrition) + geom_bar(aes(x = forcats::fct_infreq(ageGroup),fill= JobInvolvement)) +
  labs(title = "Title", x = "Age", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(attrition) + 
  geom_bar(aes(x = forcats::fct_infreq(JobInvolvement),fill= JobInvolvement)) +
  facet_wrap(~ageGroup,  scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) 


ggplot(attrition) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend("Job Involvement Rating")) +
  geom_bar(aes(x = forcats::fct_infreq(JobInvolvement),fill= JobInvolvement)) +
  facet_wrap(Attrition~ageGroup,  scales = "free", labeller = label_both)


ggplot(attrition) + 
  geom_bar(aes(x = forcats::fct_infreq(WorkLifeBalance),fill= WorkLifeBalance)) +
  facet_wrap(~ageGroup,  scales = "free") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) 


ggplot(attrition, aes(Department, fill = JobSatisfaction)) +
  geom_bar(position = "dodge")


ggplot(attrition, aes(MonthlyIncome, fill = Attrition)) +
  geom_histogram(binwidth = 500, color = "black", alpha = .8) +
  theme(text = element_text(family = "A" ), 
        plot.margin=margin(1.5, 0, 1.5, 0,"cm")) +
  theme(plot.margin=margin(1.5, 0, 1.5, 0,"cm")) +
  theme_bw()

  
  ggplot(attrition, aes(YearsAtCompany, MonthlyIncome)) +
  geom_hex( color = "black") +
  geom_smooth(se = F) + 
  geom_hline(aes(yintercept= 6500), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept=15 ), color="green", linetype = "dashed", size = 1) + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


ggplot(attrition, aes(Age,y = ..density.., color = WorkLifeBalance)) +
  geom_freqpoly(binwidth = 5) 


ggplot(attrition, aes(x = WorkLifeBalance , y = Age)) +
  geom_boxplot() 

ggplot(attrition, aes(Education, Age, fill = Attrition)) +
  geom_boxplot()


ggplot(attrition, aes(col=JobInvolvement , x=Age,y=1)) + 
  stat_summary(geom="line",fun = sum) + 
  geom_vline(aes(xintercept=30 ), color="black", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept=40 ), color="black", linetype = "dashed", size = 1) 


ggplot(attrition, aes(HourlyRate)) +
  geom_histogram(bins = 12, color = "black", fill = "lightblue") +
  facet_wrap(~Gender)


ggplot(attrition, aes(Gender, HourlyRate, fill = Gender)) +
  geom_boxplot()


ggplot(attrition, aes(Gender, fill = Attrition)) +
  geom_bar(position = "fill", alpha = .8)


ggplot(attrition, aes(Gender, HourlyRate, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Attrition) 


ggplot(attrition, aes(x = HourlyRate, y = ..density..)) +
  geom_freqpoly(aes(color = Gender), binwidth = 5) +
  geom_vline(aes(xintercept = 65 ), color = "black", linetype = "dashed", size = 1)


ggplot(attrition, aes(JobSatisfaction, fill = Gender)) +
  geom_bar(position = "fill") +
  coord_flip() 


ggplot(attrition, aes(MaritalStatus, HourlyRate, fill = Gender)) +
  geom_boxplot()


ggplot(attrition, aes(Department, fill = JobSatisfaction)) +
  geom_bar(position = "dodge")


ggplot(attrition, aes(BusinessTravel, fill = JobSatisfaction)) +
  geom_bar(position = "dodge")


ggplot(attrition, aes(BusinessTravel, fill = Attrition)) +
  geom_bar()


ggplot(attrition) +
  aes(JobLevel, fill = Attrition) +
  geom_bar(position = "dodge")


ggplot(attrition) +
  aes(x = JobLevel, fill = factor(Attrition)) +
  geom_bar(position = "fill") + 
  facet_wrap(BusinessTravel ~ OverTime)


attrition %>%
  count(Attrition, JobInvolvement) %>%
  ggplot(aes(Attrition, JobInvolvement)) +
  geom_tile(aes(fill = n))


attrition %>%
  count(Attrition, JobRole) %>%
  ggplot(aes(Attrition, JobRole)) +
  geom_tile(aes(fill = n))


ggplot(attrition) + 
  geom_bar(position = "dodge", aes(x = forcats::fct_infreq(Education), fill = Attrition)) 


ggplot(attrition) +
  aes(WorkLifeBalance, fill = Attrition) +
  geom_bar(position = "fill")


ggplot(attrition, aes(col = Attrition , x = YearsAtCompany, y = 1)) + 
  stat_summary(geom="line",fun = sum) + 
  facet_wrap(~WorkLifeBalance,  scales = "fixed")


ggplot(attrition, aes(Education, Age, fill = Attrition)) +
  geom_boxplot()


ggplot(attrition, aes(MonthlyIncome, fill = Attrition)) +
  geom_histogram(binwidth = 500, color = "black", alpha = .8) +
  geom_vline(aes(xintercept = 6800 ), color = "#A6DBA0", linetype = "dashed", size = 1) 

