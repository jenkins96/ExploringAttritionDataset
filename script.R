library(readr)
# Required Packages ------------------------------------------------------
listOfPackages <-c("readr","ggplot2","dplyr","tidyr", "purrr", "tibble", "stringr",
                  "forcats","skimr",
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
## Attaching it to the session
attach(attrition)

## Verifying it is a tibble object
class(attrition)

## Checking NA's values
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

ggplot(attrition) + geom_bar(aes(x = forcats::fct_infreq(ageGroup),fill= JobInvolvement)) +
  labs(title = "Title", x = "Age", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggplot_global_settings +
  Pastel1

#library(colorspace)
ggplot(attrition) + 
  geom_bar(aes(x = forcats::fct_infreq(JobInvolvement),fill= JobInvolvement)) +
  facet_wrap(~ageGroup,  scales = "free") + 
  labs(title = "Title",x = "X title", 
       y = "Y title", caption = "caption") +  
  ggplot_global_settings +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  Pastel1



ggplot(attrition) +
  labs(title = "High Job Involvment is the most popular Rating amongst all age groups 
  except for 20 year olds for Both Current and Non Current employees.",x = "Job Involvement Rating", 
       y = "Count of Job Involvement Ratings by Age Group", caption = "High Job Involvement is important for employers to consider for employee attrition") +
  ggplot_global_settings +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend("Job Involvement Rating")) +
  geom_bar(aes(x = forcats::fct_infreq(JobInvolvement),fill= JobInvolvement)) +
  facet_wrap(Attrition~ageGroup,  scales = "free", labeller = label_both) + 
  Pastel1

ggplot(attrition) + 
  geom_bar(aes(x = forcats::fct_infreq(WorkLifeBalance),fill= WorkLifeBalance)) +
  facet_wrap(~ageGroup,  scales = "free") + 
  labs(title = "Title",x = "X title", 
       y = "Y title", caption = "caption") +  
  ggplot_global_settings +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  Pastel1

# Análisis Exploratorio ---------------------------------------------------


# matriz de scatterplot
pairs(~x+y+z+...)
pairs(~Age + DailyRate + MonthlyIncome)
# combinacion de plots con par
old.par <- par()
par(mfrow = c(1, 2))
par(old.par) # para restart el display / canvas


ggplot(attrition, aes(Department, fill = JobSatisfaction)) +
  geom_bar(position = "dodge") +
  my_labs() +
  ggplot_global_settings +
  Pastel1

quantile(MonthlyIncome)
cv(MonthlyIncome)
kurtosis(MonthlyIncome)

### About monthly Income
ggplot(attrition, aes(MonthlyIncome, fill = Attrition)) +
  geom_histogram(binwidth = 500, color = "black", alpha = .8) +
  labs(title = "Distribución de Ingresos mensuales", 
       subtitle = "Según condición de 'attrition'", 
       x = "Ingresos Mensuales", y = "Cantidad") + 
  theme(text = element_text(family = "A" ), 
        plot.margin=margin(1.5, 0, 1.5, 0,"cm"))
+
  theme(plot.margin=margin(1.5, 0, 1.5, 0,"cm"))

theme_bw()
theme(text=element_text(family="Times New Roman", face="bold", size=12)) 
theme(plot.margin=margin(1.5, 0, 1.5, 0,"cm")) +
  
  
  
  ggplot(attrition, aes(YearsAtCompany, MonthlyIncome)) +
  geom_hex( color = "black") +
  geom_smooth(se = F) + 
  geom_hline(aes(yintercept= 6500), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept=15 ), color="green", linetype = "dashed", size = 1) + 
  labs(title = "Años en la Compañía vs Ingresos Mensuales",
       x = "Años en la Compañía", y = "Ingresos Mensuales") +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


mean(Age)
quantile(Age)

### About Age
ggplot(attrition, aes(Age,y = ..density.., color = WorkLifeBalance)) +
  geom_freqpoly(binwidth = 5) +
  labs(title = "Distribución de la edad", 
       subtitle = "Según el balance entre vida y trabajo", x = "Edad", y = "Densidad")

ggplot(attrition, aes(x = WorkLifeBalance , y = Age)) +
  geom_boxplot() +
  labs(title = "Distribución de la edad", 
       subtitle = "Según el balance entre vida y trabajo", x = "Edad", y = "Cantidad")

ggplot(attrition, aes(Education, Age, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Diagrama de Cajas entre nivel de educación y edad",
       subtitle = "Según 'attrition'", x = "Nivel de Educación",
       y = "Edad") 

ggplot(attrition, aes(col=JobInvolvement , x=Age,y=1)) + 
  stat_summary(geom="line",fun = sum) + 
  geom_vline(aes(xintercept=30 ), color="black", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept=40 ), color="black", linetype = "dashed", size = 1) + 
  labs(title = "Cuenta del Involucramiento del Trabajo por Edad", x = "Edad", y = "Cantidad")


### About Gender


ggplot(attrition, aes(HourlyRate)) +
  geom_histogram(bins = 12, color = "black", fill = "lightblue") +
  facet_wrap(~Gender) +
  labs(title = "Distribución de tarifa por hora", 
       subtitle = "Según género", 
       x = "Tarifa por hora", y = "Cantidad")

ggplot(attrition, aes(Gender, HourlyRate, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Diagrama de Cajas según género y tarifa por hora", 
       x = "Género",  y = "Tarifa por hora")

ggplot(attrition, aes(Gender, fill = Attrition)) +
  geom_bar(position = "fill", alpha = .8) + # para verlo en proporcion
  labs(title = "Diagrama de Barras según género",
       x = "Género", y = "Proporción")



ggplot(attrition, aes(Gender, HourlyRate, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Diagrama de Cajas según Género y Tarifa por Hora",
       x = "Género", y = "Tarifa por Hora") +
  facet_wrap(~ Attrition) 

ggplot(attrition, aes(x = HourlyRate, y = ..density..)) + # DISPLAYS STANDARIZEED COUNT
  geom_freqpoly(aes(color = Gender), binwidth = 5) +
  geom_vline(aes(xintercept = 65 ), color = "black", linetype = "dashed", size = 1)

ggplot(attrition, aes(JobSatisfaction, fill = Gender)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de satisfacción del trabajo", 
       subtitle = "Según género", x = "Satisfacción del Trabajo",
       y = "Proporción") +
  coord_flip() 

ggplot(attrition, aes(MaritalStatus, HourlyRate, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Diagrama de Cajas según Estado Civil y Tarifa por Hora", 
       x = "Estado Civil", y = "Tarifa por Hora")


### About Others


ggplot(attrition, aes(Department, fill = JobSatisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Diagrama de Barras de acuerdo al Departamento y Satisfación del Trabajo",
       x = "Departamento", y = "Cantidad")

ggplot(attrition, aes(BusinessTravel, fill = JobSatisfaction)) +
  geom_bar(position = "dodge")

ggplot(attrition, aes(BusinessTravel, fill = Attrition)) +
  geom_bar()

ggplot(attrition) +
  aes(JobLevel, fill = Attrition) +
  geom_bar(position = "dodge") +
  labs(title = "Diagrama de Barras segun Nivel de empleo y 'attrition'",
       x = "Nivel de Empleo", y = "Cantidad")

ggplot(attrition) +
  aes(x = JobLevel, fill = factor(Attrition)) +
  geom_bar(position = "fill") + 
  facet_wrap(BusinessTravel~OverTime) +
  labs(title = "Diagrama de Barras de Nivel de Empleo", 
       subtitle = "Según condición de 'attrition'", x = "Nivel de Empleo", y = "Cantidad")



attrition %>%
  count(Attrition, JobInvolvement) %>%
  ggplot(aes(Attrition, JobInvolvement)) +
  geom_tile(aes(fill = n))

attrition %>%
  count(Attrition, JobRole) %>%
  ggplot(aes(Attrition, JobRole)) +
  geom_tile(aes(fill = n))

ggplot(attrition) + 
  geom_bar(position = "dodge", aes(x = forcats::fct_infreq(Education), fill = Attrition)) +
  labs(title = "Diagrama de Barras de acuerdo al Nivel de Estudios", 
       x = "Nivel de Estudio", y = "Cantidad")





ggplot(attrition) +
  aes(WorkLifeBalance, fill = Attrition) +
  geom_bar(position = "fill")

ggplot(attrition, aes(col = Attrition , x = YearsAtCompany,y=1)) + 
  stat_summary(geom="line",fun = sum) + 
  facet_wrap(~WorkLifeBalance,  scales = "fixed")

ggplot(attrition, aes(Education, Age, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Diagrama de Cajas entre nivel de educación y edad",
       subtitle = "Según 'attrition'", x = "Nivel de Educación",
       y = "Edad") +
  ggplot_global_settings +
  Pastel1

# Medidas de Centralización -----------------------------------------------

mean(x)
median(x)
mfv(x) # thanks to modeest library
quantile(x) # thanks to raster library



# Medidas de Dispersión ---------------------------------------------------

var(x)
sd(x)
cv(x) # thanks to raster library


# Medidas de Asimetría ----------------------------------------------------

skewness(data$Age)
kurtosis(x) # thanks to moments library


# For reordering
ggplot(mpg) +
  geom_boxplot(aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

# data2 <- lapply(data, function(x) if(is.character(x)) as.factor(x) else x)

ggplot(attrition, aes(MonthlyIncome, fill = Attrition)) +
  geom_histogram(binwidth = 500, color = "black", alpha = .8) +
  my_labs("titulo","subtitle" , "xt", "ylab", "caption")

my_labs <- function(title, subtitle = NULL, xtitle, ytitle, caption = NULL){
  warning('order: title, subtitle, x, y, caption. If want to skip subtitle leave a space. If you want to skip caption just do not write anything')
  ggplot_labs <- labs(title = title, 
                      subtitle = subtitle, 
                      x = xtitle, 
                      y = ytitle, 
                      caption = caption)
  
  return(ggplot_labs) 
}

+
  labs(title = "Distribución de Ingresos mensuales", 
       subtitle = "Según condición de 'attrition'", 
       x = "Ingresos Mensuales", y = "Cantidad") +
  geom_vline(aes(xintercept = 6800 ), color = "#A6DBA0", linetype = "dashed", size = 1) +
  ggplot_global_settings +
  Pastel1

