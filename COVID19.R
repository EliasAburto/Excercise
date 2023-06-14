library("foreign")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(tidyverse)
library(readr)
library(car) #para analizar vif, colinealidad



rm(list = ls())

#Steps to prove the hypothesis

#A. Model Evaluation

#A1. Regression coeficients and r-square

#Load the database

Mydata=read.spss("data/COVID_data.sav",to.data.frame=T,use.value.labels=FALSE)
View(Mydata)
summary(Mydata$SE_Total)

#Perform the linear regression analysis

Intention_lm <- lm(IN_Total ~ SN_Total + BC_Total + AT_Total, data = Mydata)
summary(Intention_lm)


#A2. Interpretation

#Bs ajusted: 

# For each point obtained in the scale of subjective norms,
# the intenton to COVID-19 vaccination intention will increase 0.36 points.
# Also, subjective norms had a direct effect on COVID-19 vaccination intention 
# and statistical significance.

# For each point obtained in the scale of Behavioral Control,
# the intenton to COVID-19 vaccination intention will increase 0.29 points.
# Also, Behavioral Control had a direct effect on COVID-19 vaccination intention 
# and statistical significance.


# For each point obtained in the scale of Attitude Toward Covid-19 vaccination,
# the intenton to COVID-19 vaccination intention will increase 0.45 points.
# Also, Attitude Toward Covid-19 vaccination had a direct effect on COVID-19 vaccination intention 
# and statistical significance.

# The model with these three determinants explain 78% of variance of COVID-19 vaccination intention.


#B. Regression Model Assumtions

#B1. Lineality between IVs and Dv

plot(IN_Total ~ SN_Total, data = Mydata)
plot(IN_Total ~ BC_Total, data = Mydata)
plot(IN_Total ~ AT_Total, data = Mydata)

#B2. Independence of observations: The observation from our model are independent.

# This is made when each observation was made by one participant.


#B3. Homoscedasticity: The errors from our model have equal variance.

par(mfrow=c(2,2))
plot(Intention_lm)
par(mfrow=c(1,1))


#B4. Normality of Errors: The errors from our model are normally distributed.

```{r}
par(mfrow=c(2,2))
plot(Intention_lm)
par(mfrow=c(1,1))
```

#B5. Multicollinality: evaluate if the IVs are redundant.

vif(Intention_lm)










# Linear Regression with numeric VI

## Step 1: Load the data into R


# Load the database

Mydata=read.spss("COVID_data.sav",to.data.frame=T,use.value.labels=FALSE)
  View(Mydata)
summary(Mydata$SE_Total)



## Step 3: Explore outcome variable (must be numeric)

#We checked if the independient variable `In_Total` (VI) is numeric

glimpse(Mydata$IN_Total)
glimpse(Mydata$SN_Total)
glimpse(Mydata$BC_Total)
glimpse(Mydata$AT_Total)



#### C. Normality (Me parece que no es la normalidad de la variable sino de las observaciones)

hist(Mydata$IN_Total)
hist(Mydata$SN_Total)
hist(Mydata$BC_Total)
hist(Mydata$AT_Total)

# Se puede evaluar la normalidad por  QQplot, Kurtosis



##Evaluar colinealidad entre las variables.

vif(Intention_lm)



#########################################

## Step 4: Check the homocedasticity - 








## Step 1: Load the data into R

```{r}
# Load the database
library(readxl)
data_depression <- read_excel("base_depresion_en_medicos_-_corregida.xlsx")
View(data_depression)
summary(data_depression)
library(readxl)
data_rendimiento <- read_excel("base_rendimiento_academico.xlsx")
View(data_rendimiento)
```

## Step 3: Explore outcome variable (must be numeric)

We checked if the independient variable `nota` (VI) is numeric

```{r}
glimpse(data_rendimiento)
glimpse(data_rendimiento$nota) #Numeric
glimpse(data_rendimiento$edad) # Numeric
glimpse(data_rendimiento$promsecu) #Numeric
```

## Step 4: Make sure data assumptions

#### A. Linearity

```{r}
plot(nota ~ edad, data = data_rendimiento)
plot(nota ~ promsecu, data = data_rendimiento)
```

#### B. Independence of observations

Teoricamente se entiende que las variables son independientes

```{r}
library(car)
cor(data_rendimiento$edad, data_rendimiento$promsecu)
vif(m_rendimiento_lm)
```

#### C. Normality

```{r}
hist(data_rendimiento$nota)
# Se puede evaluar la normalidad por  QQplot, Kurtosis
```

## Step 3: Perform the linear regression analysis

```{r}
m_rendimiento_lm <- lm(nota ~ edad + promsecu + vivesolo, data = data_rendimiento)
summary(m_rendimiento_lm)
```

## Step 4: Check the homocedasticity

```{r}
par(mfrow=c(2,2))
plot(m_rendimiento_lm)
par(mfrow=c(1,1))
```

## Step 5: Perform a graph to visualize the results

```{r}
print(m_rendimiento_lm)
# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")
a <- coef(m_rendimiento_lm)[1]
print(a)
Xedad <- coef(m_rendimiento_lm)[2]
Xprom <- coef(m_rendimiento_lm)[3]
print(Xedad)
print(Xprom)
```

```{r}
plotting.data <- expand.grid(
  edad = seq(min(data_rendimiento$edad), max(data_rendimiento$edad),lenght.out=30),
  promsecu = seq(min(data_rendimiento$promsecu), mean(data_rendimiento$promsecu)))



plotting.data$predicted.y <- predict.lm(m_rendimiento_lm, newdata=plotting.data)
plotting.data$promsecu <- round(plotting.data$promsecu, digits = 2)
plotting.data$promsecu <- as.factor(plotting.data$promsecu)
nota.plot <- ggplot(data_rendimiento, aes(x=edad, y=nota)) +
  geom_point()
nota.plot
```

## Step 6: Report and interpret your results

# Linear Regression with categorical VI

Step 1: Load the data into R

Step 2: Order each variable

Step 3: Explore outcome variable (must be categorical)

Step 4: Make sure data assumptions

Step 5: Perform a graph to visualize the results

Step 6: Report and interpret your results

# Multiple Regression with categorical VI

## Step 1: Load the data into R

We checked if the data has the independent variable `income` (*X*) and
dependent variable `happiness` (*Y*)

```{r}
# Load the database
income_data <- read_csv("income.data.csv")
View(income_data)
summary(income_data) #numeric summary
```

## Step2: Make sure the data meet the assumptions

### Assumptions

#### A. Linearity

```{r}
par (mfrow=c(2, 2))
plot (income_data)
plot(happiness ~ income, data= income_data)
```

#### B. Independence of observations

```{r}
```

#### C. Normality

```{r}
hist(income_data$happiness)
```

#### D. Homoscedasticity

```{r}
```

## Step 3: Perform the linear regression analysis

```{r}
income_happiness_lm <- lm(happiness ~ income, data = income_data)
summary(income_happiness_lm)
```

## Step 4: Check the homocedasticity

```{r}
par(mfrow=c(2,2))
plot(income_happiness_lm)
par(mfrow=c(1,1))
```

## Step 5: Perform a graph to visualize the results

### 1. Plot the data in default graph

```{r}
income_graph<-ggplot(income_data, aes(x=income, y=happiness))+
  geom_point()
income_graph
```

### 2. Add the linear regression

```{r}
income_graph <- income_graph + geom_smooth(method="lm", col="black")
income_graph
```

### 3. Add the equation

```{r}
income_graph <- income_graph +
  stat_regline_equation(label.x = 3, label.y = 7)
income_graph
```

### 4. Make it more beautiful

```{r}
# Is only a copy
income_graph +
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")
```

## Step 6: Report/Interpret your results







