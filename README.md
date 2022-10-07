# RProject
---
title: "Final Project"
author: "Tran Ong | Lena Le"
date: "12/16/2021"
output: 
  html_document:
    code_folding: hide
---

## Introduction

Alcohol use is a serious physical and mental health issue among young adults whose personalities and emotions can be seen as very expressive. We want to find out how people with different types of perfectionism personalities have different drinking frequencies and whether those characteristics are related to certain negatively reinforced motivations to drink. We hope to see if we can predict the risks of excessive or consistent alcohol use and heightened negative drinking motives from perfectionism types. How they feel themselves being supported and connected with their close ones is also indicative of their mental health, so its relations with alcohol use and perfectionistic personalities are also a matter of concern. Sex is also taken into account to get more insights and make predictions about the groups that are more likely to drink excessively and/or consistently. Our big question:

*What personality factors and social factors are related to and contribute to alcohol consumption?*

Are perfectionism types (Rigid, Narcissism, Other Criticalness, and Self-Criticalness) and sex-related to and a good predictor for alcohol consumption and drinking motives (conformity, coping-depression)? 
Do young adults with high drinking frequencies have low perceived social support? Do people with each drinking motive (conformity, coping-depression) have low or high perceived social support? 
The original dataset is provided by OSF - Open Science Framework and contributed by Sean P. Mackinnon, Samantha M. Firth, Cassondra M. Ray,  Roisin M. O’Connor. It was collected via online survey questionnaires over 21 consecutive days in Halifax and Montréal, Canada. The start date depends on the subject. Participants were eligible to participate if they were: (a) between 18–25 years old; (b) consumed 12+ drinks in the past year; and (c) had Internet access at home (Mackinnon et al., 2021). Except for sex and id, other variables we use were the average scores of smaller question items that were measured on Interval/Ratio scales. We decided to only use one day (day 2) out of 21 days because of numerous missing data due to the inconsistency of the subjects in filling out and the fact that day 2 has much fewer missing values compared to other days. We also created 3 more variables based on the existing ones. We created 3 data frames (general data, female, male) as we did our data wrangling. We split 2 sexes for within-sex analyses because their sample sizes are too disproportionate which would make the general dataset misrepresentative and the sex comparison invalid. 

Original Dataset: Total Dataset.csv 
Perfectionism, Negative Motives for Drinking, and Alcohol-Related Problems: A 21-day Diary Study - Sean P. Mackinnon, Cassondra M. Ray, Samantha M. Firth, Roisin M. O’Connor. 



## Ethical Consideration

For ethical considerations, we look at many aspects, especially in data collection and usage, like the anonymity, confidentiality, inclusiveness of demographics, intentions of questions, consequences of the release of findings. For the participants, the dataset has what they provided. The data was provided online and made anonymous and confidential. The participants’ consent and voluntariness were obtained. This is required so that the contributors and OSF can publicly release the data. As data analysts, we are responsible for using the data that’s provided to us and allowed to be used and interpreted. We are also responsible for ensuring that our analyses and conclusions are adequate, representative, respectful, and not misleading. Our methods to use statistical techniques and visualizations need to be correct, transparent, and not misleading. We discussed the data collection and usage on how much it tries to represent the population it targets, its validity, and reliability. Because of the disproportionate number of subjects from different demographic groups, we have to carefully consider the way we interpret and draw conclusions while pointing out that it would possibly misrepresent the overall population. There’s a great disparity between the count of subjects who identify themselves as females and who identify themselves as males. This prevents us from using t-test or correlation tests for sex factors. If not careful, our analyses can misrepresent the young adult population and fail to measure or predict drinking behaviors and social support. Since the advantages and limitations affect our approach in using and analyzing data, we will be extra careful with our conclusions and communication so as to not create bias and mislead the audience. This ethical responsibility is toward finding the most precise truth and avoiding any big and misleading statements. Another thing is our wording that needs to avoid negative sentiments surrounding personality aspects (e.g., perfectionism) and overall alcohol consumption. 

## Data Wrangling

```{r}
library(readr)
library(tidyverse)
library(GGally)
library(tidymodels)

Totaldataset <- read_csv("Total Dataset.csv")
#View(Totaldataset)
```

## Add/Remove/Change variables:

In this part, we start data wrangling. Firstly, we use 'filter()' and 'mutate()' functions to add two new variables to our existing data named 'Total Dataset'. However, because there are only some important columns we will use to analyze, a new data frame called 'Data' is created and it includes all of our essential variables.

```{r}
Totaldataset <- Totaldataset %>% 
  filter(day==2) %>% mutate(narcissistic= (btps.ENT1+btps.ENT2 + btps.ENT3 + btps.ENT4 + btps.GRAN1+ btps.GRAN2 + btps.GRAN3 + btps.GRAN4)/8, other.critical= (btps.OOP1 + btps.OOP2 + btps.OOP3 + btps.OOP4 + btps.OOP5 + btps.HC1 + btps.HC2 + btps.HC3 + btps.HC4)/9)
```

```{r}
Data <- Totaldataset %>% select(id, sex, self.critical.mean, rigid.mean, other.critical, spss.mean, swl.mean, tipm.CONS.mean, tipm.OE.mean, tipm.ES.mean, alc.mean, rdmq.CON.mean, rdmq.CD.mean, narcissistic)
```

```{r}
Data <- Data%>% rename(self.critical=self.critical.mean, rigid = rigid.mean, perceived.social.support=spss.mean, satisfaction=swl.mean, alcohol.consumption=alc.mean, conscientiousness=tipm.CONS.mean, open.to.experience = tipm.OE.mean, emo.stability=tipm.ES.mean, conformity=rdmq.CON.mean, coping.depression=rdmq.CD.mean) 

Data$sex <- factor(Data$sex, levels = c(0,1), labels = c("Male","Female"))

Data <- transform(Data, drinking.frequency = cut(alcohol.consumption, breaks = c(0, 2, 5, 8), labels = c("Low", "Moderate", "High"), include.lowest = TRUE))

Femaledata <- Data %>% filter(sex== "Female")
Maledata <-Data %>% filter(sex=="Male")

Mean <- Data %>% group_by(drinking.frequency,sex) %>% summarize(narcissistic.mean=mean(narcissistic), other.critical.mean=mean(other.critical), rigid.mean=mean(rigid), self.critical.mean=mean(self.critical), conformity.mean=mean(conformity), coping.depression.mean=mean(coping.depression), social.support.mean=mean(perceived.social.support))

```

## Data Explanation and Exploration

```{r}
ggplot(Data, aes(alcohol.consumption)) +
geom_histogram() +
labs(title= "Distribution of alcohol consumption levels from 0 (Never) 8 (Everyday)", x= "Alcohol Consumption", y= "Count", caption= "Never - 0 \n Once-Twice/year - 1 \n 3-6 times/year - 2 \n 7-11 times/year - 3 \n Once/week - 4 \n Twice/week - 5 \n 3-4 times/week - 6 \n Nearly everyday - 7 \n Everyday - 8 ") +
theme(legend.text = element_text(size = 8), axis.line = element_line(size = 0.25, colour = "black"), axis.text = element_text(colour = "blue"))
```
![](https://scontent.xx.fbcdn.net/v/t1.15752-9/301829786_5412656782184178_1830879729861832654_n.png?stp=dst-png_s720x720&_nc_cat=108&ccb=1-7&_nc_sid=aee45a&_nc_ohc=2iXh50e3ny0AX-XFjPI&_nc_ad=z-m&_nc_cid=0&_nc_ht=scontent.xx&oh=03_AVLIj7_73GyLLApkecluXijdfWH3kBVBdaf7WJaW19Zdlg&oe=636627B3)
<br />
The distribution of alcohol consumption levels from 0 to 8 is not normal but skewed to the right. The highest frequency is around 25. And also from the summary of data, we can see the mean of alcohol consumption is about 2.46.

```{r}
par(mfrow=c(2,2))
hist(Data$self.critical)
hist(Data$rigid)
hist(Data$other.critical)
hist(Data$narcissistic)


par(mfrow=c(2,2))
hist(Data$perceived.social.support)
hist(Data$coping.depression)
hist(Data$conformity)

summary(Data)

table(Data$drinking.frequency)
table(Data$sex)
```
	
The first four histograms describe the distributions of the grading scores based on perfectionism. Both of the distribution of self critical and rigid perfectionism is quite normally distributed, with their highest frequencies at around 60 and 50. In the two other plots about other critical and narcissistic, the distribution of them are not normal, but skewed right. The highest frequency in the other critical distribution is roughly 80, while that of narcissistic is nearly 130.

Most of the distributions in the next four histograms are skewed to the right, except for perceived social support. The highest frequencies of alcohol consumption, coping depression, and conformity are nearly 70, 150, and 160, respectively. Meanwhile, the distribution of perceived social support is skewed to the left, with the highest frequency of around 100. 
	
Moreover, after summarizing the data set and calculating the ratios among categories in drinking frequency and sex, we can have the information that is about the median, mean, and standard deviation of each variable, which can help us to determine the next step of our data analysis.
	
However, there is a challenge that most of the distributions are not normall, so we are concerned about the reliability of our conclusions we will make after interpreting the rest of data.
	
After looking at the histograms, we decide to look at the correlation among these variables and then do testing about perfectionism.


## Statistical Analysis and Interpretation

__Insights into pairwise relationships and/ differences among all factors__

```{r}
ggpairs(Data, columns=c("self.critical","rigid","narcissistic", "other.critical"))

ggpairs(Data, columns=c("perceived.social.support","self.critical","rigid","narcissistic", "other.critical", "alcohol.consumption", "conformity", "coping.depression", "drinking.frequency"))

#Females
ggpairs(Femaledata, columns=c("perceived.social.support","self.critical","rigid","narcissistic", "other.critical", "alcohol.consumption", "conformity", "coping.depression", "drinking.frequency")) + labs(title= "Females") + theme(plot.title = element_text(size = 7))

#Males
ggpairs(Maledata, columns=c("perceived.social.support","self.critical","rigid","narcissistic", "other.critical", "alcohol.consumption", "conformity", "coping.depression", "drinking.frequency")) + labs(title= "Males") + theme(plot.title = element_text(size = 7))
```

## Perfectionism types

```{r}
shapiro.test(Femaledata$other.critical)
shapiro.test(Femaledata$self.critical)
t.test(Femaledata$self.critical, Femaledata$other.critical)

ggplot(Femaledata, aes(self.critical)) + geom_density(fill="dark green") +
  labs(title= "Distribution of young adult females' self-critical perfectionism", x= "Self-critical perfectionism", y= "Frequency")
ggplot(Femaledata, aes(other.critical)) + geom_density(fill="dark blue") +
  labs(title= "Distribution of young adult females' other-critical perfectionism", x= "Other-critical perfectionism", y= "Frequency")
```

Since the only 2 distributions that are normal are self-critical perfectionism and other-critical perfectionism in the female dataset according to the Shapiro-Wilk test (p= 0.16 and 0.899), we decided to compare these 2 types to get more insight into the tendency of perfectionism in females. The 2-sample t-test revealed that females tend to have higher self-critical perfectionism (M= 2.816568) than other-critical perfectionism (M= 2.203310); p=2.31e-05). From the result, we can conclude that young adult females tend to be critical to themselves in terms of being perfect more to other people. The density of self-critical perfectionism shows a relatively normal distribution that is unimodal, bell-shaped and quite symmetrical. But the density of the other-critical type is a bit skewed to the right with an almost bimodal shape. This may be why the Shapiro p-value is quite low even though not lower than 0.05. 


## Perfectionism & Alcohol Consumption
__Alcohol consumption & Perfectionism__

*Pearson r correlation and linear regression analyses were conducted between each perfectionism type and alcohol consumption for overall data.*
```{r}
#narcissistic vs. alc
cor.test(Data$alcohol.consumption, Data$narcissistic)
Model1 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~narcissistic, data=Data)
summary(Model1$fit)

#other.critical vs. alc
cor.test(Data$alcohol.consumption, Data$other.critical)
Model2 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~other.critical, data=Data)
summary(Model2$fit)

ggplot(Data,aes(narcissistic, alcohol.consumption,col=sex)) + geom_point() + 
  geom_jitter() + facet_grid(~sex) +
  geom_smooth(method='lm', color='black') + 
  labs(title = "Relationship between \n narcissistic perfectionism and alcohol consumption \n in young adults", x ="Narcissistic perfectionism", y = "Alcohol Consumption") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data,aes(other.critical, alcohol.consumption,col=sex)) + geom_point() + 
  geom_jitter() + facet_grid(~sex) +
  geom_smooth(method='lm', color='black') + 
  labs(title = "Relationship between \n other.critical perfectionism and alcohol consumption \n in young adults", x ="Other critical perfectionism", y = "Alcohol Consumption") + 
  theme(plot.title = element_text(hjust = 0.5))

#rigid vs. alc
cor.test(Data$alcohol.consumption, Data$rigid)
Model3 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~rigid, data=Data)
summary(Model3$fit)

#self.critical vs. alc
cor.test(Data$alcohol.consumption, Data$self.critical)
Model4 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~self.critical, data=Data)
summary(Model4$fit)
```

Between narcissistic type and alcohol consumption, there’s a statistically significant positive correlation; r= 0.24; p<.0001. The relationship is not strong, however. A young adult that reports higher narcissistic perfectionism is likely to report high drinking frequency. The regression model shows that narcissistic type is a significant predictor for drinking but its contribution is very small; F(1 and 239)= 14.65, p<.0001; R^2= 0.058. The median residual is close to 0. This means that the model fits the data quite well. Narcissism accounts for 5.8% in the variation of alcohol consumption frequency in young adults. With a coefficient of 0.33201, 1-point increase in narcissistic score predicts that the drinking frequency increases by 0.33201 point. 

Between other-critical type and alcohol consumption, there’s a statistically significant positive correlation; r= 0.19; p=.003. The relationship is not strong, however. A young adult that reports higher other critical perfectionism is likely to report high drinking frequency. The regression model shows that other critical type is a significant predictor for drinking but its contribution is very small; F(1 and 239)= 8.829, p=.003; R^2= 0.036. The median residual is close to 0. This means that the model fits the data quite well. Other-critical perfectionism accounts for 3.6% in the variation of alcohol consumption frequency in young adults. With a coefficient of 0.25567, 1-point increase in other-critical perfectionism score predicts an increase by 0.25567 in drinking frequency score. 

Between the rigid type and alcohol consumption, there’s no statistically significant correlation; r= -0.013; p=.8. This also indicates that the scores for rigid perfectionism are not a significant predictor for drinking frequency. 

Between the self-critical type and alcohol consumption, there’s no statistically significant correlation; r= .05; p=.46. This also indicates that the scores for self-critical perfectionism are not a significant predictor for drinking frequency. 

Despite the significant results for narcissistic personality and other-critical perfectionism, they may not be a significant predictor for drinking frequency in real life because their statistical contributions and coefficients are very small.

The scatterplot for narcissistic personality and alcohol consumption shows that the slope for females is positive and is more inclined than for males. There’s a bigger cluster for both sexes in the lower end of the narcissism spectrum. The relationship seems very weak for both because all data points are very scattered and stray far from the line. The scatterplot for other-critical perfectionism and drinking shows that the slopes for both sexes are positive and similar in terms of their strength. There’s a bigger cluster for both sexes around score 2 of the spectrum. The relationship seems very weak for both because all data points are very scattered and stray far from the line. Because of this, we will look at the inferential statistics separately for females and males for more insights.

```{r}
#diagnostic
library(ggfortify)
library(lindia)
library(MASS)

autoplot(lm(alcohol.consumption~narcissistic, data=Data))
gg_reshist(lm(alcohol.consumption~narcissistic, data=Data)) + scale_y_log10()

autoplot(lm(alcohol.consumption~other.critical, data=Data))
gg_reshist(lm(alcohol.consumption~other.critical, data=Data)) +
  scale_y_log10()

autoplot(lm(alcohol.consumption~self.critical, data=Data))
gg_reshist(lm(alcohol.consumption~self.critical, data=Data)) +
  scale_y_log10()

autoplot(lm(alcohol.consumption~rigid, data=Data))
gg_reshist(lm(alcohol.consumption~rigid, data=Data)) +
  scale_y_log10()
```

The Residuals vs. Fitted plots for all 4 models show a mostly straight and flat blue line that matches the grey dashed line. This supports that the residuals (difference between an observed point and the regression line) stay the same across the fitted values (alcohol consumption values predicted by the regression line corresponding with the perfectionism scores). There is almost no non-linearity, unequal error variances, and outliers, supporting that our models are valid. Normal Q-Q plots for 4 models show that the points are mostly on or close to the line, indicating that the data is normally distributed. Scale-Location plots for 4 models show no clear pattern and relatively flat lines, indicating homoscedasticity - variances of residuals are equal across fitted values. Residuals vs Leverage plots show several outstanding and influential points for 4 models. Removing these points may be able to change the model coefficients drastically. 

__Females__

```{r}
#Narcissism & alcohol consumption
cor.test(Femaledata$alcohol.consumption, Femaledata$narcissistic)

FemaleModel1 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~narcissistic, data=Femaledata)
summary(FemaleModel1$fit)

#Other critical perfectionism & alcohol consumption
cor.test(Femaledata$alcohol.consumption, Femaledata$other.critical)

FemaleModel2 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~other.critical, data=Femaledata)
summary(FemaleModel2$fit)

```
For females, between narcissistic type and alcohol consumption, there’s a statistically significant positive correlation; r= 0.31; p<.03. The relationship is a bit stronger than the overall young adults. A female young adult that reports higher narcissistic perfectionism is likely to report high drinking frequency. The regression model shows that narcissistic type is a significant predictor for drinking but its contribution is small; F(1 and 45)= 4.912, p<.03; R^2= 0.098. The median residual is close to 0. This means that the model fits the data quite well. Narcissistic perfectionism accounts for 9.8% in the variation of alcohol consumption frequency in young adult females. With a coefficient of 0.4269, 1-point increase in narcissistic score predicts that the drinking frequency increases by 0.4269 point.
Surprisingly, unlike the overall population and probably males, there’s no statistically significant correlation between the other-critical type and alcohol drinking for females; r= 0.153; p= 0.3. This also indicates that the other-critical perfectionism in females doesn’t significantly predict their alcohol consuming frequency. The ggpairs() indicates no significant correlation between the other types (rigid and self-critical perfectionism) and alcohol consumption in females. 

__Males__

```{r}
#Narcissism & alcohol consumption
cor.test(Maledata$alcohol.consumption, Maledata$narcissistic)

MaleModel1 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~narcissistic, data=Maledata)
summary(MaleModel1$fit)

#Other critical perfectionism & alcohol consumption
cor.test(Maledata$alcohol.consumption, Maledata$other.critical)

MaleModel2 <- linear_reg() %>% set_engine("lm") %>%
  fit(alcohol.consumption~other.critical, data=Maledata)
summary(MaleModel2$fit)

#Self critical perfectionism & alcohol consumption
cor.test(Maledata$alcohol.consumption, Maledata$self.critical)
```
With the variable 'narcissistic', in Pearson test, the t-value and degree of freedom are 2.788 and 192, respectively. The p-value is 0.005837, which is much smaller than 0.05, so this is statistically significant. The true correlation coefficient is estimated as 0.197, which means that narcissism is positively correlated to alcohol consumption in Male data. From the linear regression model, the median residual is around -.036, which is quite close to 0. This means that the model fits the data quite well. Both the values of pr|t| are under 0.05, so it is also statistically significant. The multiple R-squared is around 0.039, which also means that narcissism accounts for roughly 3.9 % of the variation in the alcohol consumption of males.

Similarly, with the variable 'other.critical', in Pearson test, the t-value=2.27 and df = 192. The p-value is 0.024, which is much higher than 0.05, so this is not statistically significant. The true correlation coefficient is estimated as 0.162, which means that other.critical is positively correlated to alcohol consumption in Male data. The median residual is around -.02, which is quite close to 0. This means that the model fits the data quite well. The multiple R-squared is around 0.026, which also means that other.critical accounts for roughly 2.6 % of the variation in the alcohol consumption of males.

__Visualization of Narcissistic and Other critical perfectionism on Drinking Frequency levels__

```{r}

ggplot(Data,aes(drinking.frequency, narcissistic, fill=sex)) + geom_boxplot(notch = TRUE) + facet_grid(~sex) +
  labs(title = "Narcissistic perfectionism on drinking frequency", y= "Narcissistic perfectionism", x= "Drinking frequency")
ggplot(Data,aes(drinking.frequency, other.critical, fill=sex)) + geom_boxplot(notch = TRUE) + facet_grid(~sex) +
    labs(title = "Other critical perfectionism on drinking frequency", y= "Other critical perfectionism", x= "Drinking frequency")

```

From previous findings, we would expect that the higher drinking frequency level would come along with higher narcissism and higher other-critical perfectionism. The boxplot for the distribution of narcissism throughout levels of drinking.frequency is shown. The high drinking level is too small for males and has an inside out shape for females, indicating that its distribution is very skewed and its sample is very small for both sexes. We’ll not take into account this drinking level. The moderate drinking level has a higher middle 50% range in narcissism than the low drinking level. For females, the boxplot notches of those 2 levels are wide and overlapped, implying that the 95% confidence intervals for median overlap, indicating their medians are close to each other. The fact that the median confidence intervals are also wide is because there are various hypothesized values for medians. Also, the boxplot for female low drinking level has an inside-out shape, so their sample is skewed and small. Meanwhile for males, the notches are smaller and not overlapped, indicating that their medians are very different. Median is the middle value between the lowest and highest 50% of the values. Therefore, overall, the middle value of narcissism in males with low drinking frequency is significantly lower than that in males with moderate drinking frequency. But it’s not significant for females. 
The boxplot for other-critical perfectionism and drinking frequency also shows a similar pattern. The high drinking level won’t be taken into account because of their inside-out shape that’s indicative of a skewed and small sample. The moderate level shows higher narcissism than the low level with the median confidence intervals don’t overlap for males but not for females. Therefore, overall, the middle score of other-critical perfectionism in males with low drinking frequency is significantly lower than that in males with moderate drinking frequency. But it’s not significant for females. 

# Negatively Reinforced Drinking motives 
__Are 2 negatively reinforced drinking motives related to each other?__ 

The ggpairs() for overall data indicates that conformity motive and coping depression motive have a significant positive correlation; r= 0.643; p<0.001. The correlation is quite strong. The ggpairs() for female data and male data show similar results: strong positive correlation between  conformity motive and coping depression motive; r = 0.753 and 0.621; all p<.001. As young adults have a higher tendency to fit in their social groups by drinking, they also have a higher tendency to cope with their depression by drinking. Social support is not significantly correlated with both drinking motives for females. 

__What other factors are drinking motives more related to among males and among females?__

**`Females`**

ggpairs () for Female dataset tells us that conformity motive has a significant positive correlation with narcissism, r=0.32, p<.05. Coping depression motive has a significant positive correlation with self-critical perfectionism; r= 0.366; p<.05. This indicates that as a young adult female scores higher on narcissism, the person would be more likely to drink as an effort to fit in social groups and avoid negative social sanctions. If the person scores higher on self-critical perfectionism, it’s more likely that the person copes with her depression by drinking. 
Alcohol consumption is not significantly related to coping depression, but to conformity (r=0.34; p<0.05). For females, higher conformity motives for drinking may indicate higher drinking frequency. Lastly, perceived social support is not significantly related to either of two negative drinking motives. 

**`Males`**

ggpairs() for Male dataset tells us that conformity motive has significant positive correlations with self-critical (r= 0.264; p≈0), rigid (r= 0.196; p<.001), narcissistic perfectionism (r=0.192; p<.001), and a significant negative correlation with perceived social support (r= -0.338; p≈0). Self-critical perfectionism is most related to conformity drinking in terms of personality. As a young adult male scores higher on self-critical, or rigid, or narcissistic perfectionism, the person also likely drinks to fit in their social groups and avoid negative social sanctions. In terms of social factor, perception of less social support is moderately related to and indicates higher conformity motive for drinking. 
Coping depression motive has significant positive correlations with all perfectionism types, and a significant negative correlation with perceived social support (r= -.381; p≈0). In terms of personality, self-critical perfectionism is most strongly associated (r=0.388; p≈0), then comes rigid (r= 0.270; p≈0), other-critical (r= 0.230; p<.001), and narcissistic perfectionism (r=0.221; p<.001). This suggests that if a young adult male scores higher on perfectionism for either subtype, the person also has a higher tendency to cope with depression by drinking. 
Alcohol consumption rate is significantly and positively related to coping with depression motive (r=0.246; p≈0) but not for conformity motive. 

**`Takeaways`**

There are many differences in our findings for males’ and females’ negative drinking motives. For one thing, males tend to drink more frequently to cope with their depression while females tend to drink more frequently to conform with their social groups. Another thing is that females with higher narcissism tend to drink due to conformity motivation while males with self-critical perfectionism are most likely to drink to conform. Rigid and narcissistic types also play a part but are weaker. Females who tend to cope with their negative emotions by drinking are self-critical perfectionists, but males who cope with negative emotions are basically perfectionists no matter what type. Lastly, perceived social support is not significantly related to either of two negative drinking motives for females, but significantly for males. Not just that, the relationships are moderately strong for males. If a young adult male perceives lower social support, he tends to be negatively motivated to drink either by the attempt to fit in social groups and avoid social sanctions or to deal with their negative emotions and depression. 


# Perception of social support and Drinking frequency

Both ggpairs for males and females showed that there is no significant correlation between perceived social support and alcohol consumption rate. Despite the fact that there’s no pattern, we are curious about how social support changes on average across different drinking frequency levels for males and for females. 

```{r}
ggplot(Mean,aes(x=drinking.frequency, y= social.support.mean, fill=drinking.frequency)) + geom_col() + facet_grid(~sex) +
    labs(title = "Average perceived social support and Drinking frequency", y= "Mean perceived social support", x= "Drinking frequency") 
```

The column graph revealed that interestingly for males, those with high drinking frequency perceive more social support than those with low level and moderate level (which has the least social support). Meanwhile, for females, those with the moderate drinking level perceive more social support than those with other levels. The high drinking frequency comes with the lowest social support. 



Conclusions
• Provide one or two paragraphs concluding about the data: what does it tell us, what are the limitations to this data/model, and what is one future direction you could envision for future data analysts or data collectors?
9
• Find at least one reference that is relevant to or supports your insights, and cite it in this section. You may cite a reference by linking directly to it in your Rmd [text here](link here), and listing the full citation below the conclusions section. Please ask me if you aren’t sure how to cite references.


# Limitations: 
  
There are some limitations in our data set. Firstly, the difference between sample sizes of the two sexes remains relatively high, so the comparisons we make after interpreting among genders might be negatively affected. We are afraid that because of it, the conclusion will be biased for lack of data on females (in our modified dataset, not the original one). So far, instead of comparing the two sexes, we split the data into two sex groups, and create two new sets of data called ‘Maledata’ and ‘Femaledata’. Within each data set, we look closely at some relationships among variables like narcissistic or other critical perfectionism and alcohol consumption. Because of the sex disproportionate sample sizes, we had to restate our questions regarding sex by asking about relations within each sex instead of comparing the sexes. Secondly, some correlation coefficients are moderately high, but when we use the graphs to make sure the reliability of these coefficients, the data points are more scattered than we expect and don’t fit the line much. Thirdly, we have many variables that we are interested in, so we ran into some problems when designing our approach. We used ggpair() to look at the significant correlations and choose the ones we are most interested in. But then when it comes to the categorical variable - drinking.frequency, we have to think about what kind of variables we want to compare between each drinking level. And it is hard to look at the ggpair for this variable since each plot is too small.

Reference 
Mackinnon, S. P., Ray, C. M., Firth, S. M., & O’Connor, R. M. (2021). Data from “Perfectionism, Negative Motives for Drinking, and Alcohol-Related Problems: A 21-day Diary Study”. Journal of Open Psychology Data, 9(1), 1. DOI: http://doi.org/10.5334/jopd.44 























































































