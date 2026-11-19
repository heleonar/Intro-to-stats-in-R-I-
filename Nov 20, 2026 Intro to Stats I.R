# Title: Intro do Stats in R (I)
# Author: Your Name
# Date: 2026-11-19


# Libraries and data ------------------------------------------------------

library(tidyverse)
library(car) # car package holds the Anova function, used for Type II and III anovas
library(rstatix) # for shapiro test and tukey_hsd
library(palmerpenguins)

df<-penguins


# T-test ------------------------------------------------------------------

# lets compare how male and female penguins compare in body mass. Since we only have two factors for our independent variable and a continuous dependent variable, we can use a T-test.

#Let's take a look at our data

df |> 
  ggplot(aes(x=sex, y=body_mass))+
  geom_boxplot()

#It looks like we have some unassigned individuals, so let's remove them for this analysis.

df |> 
  filter(sex != '') |> 
  ggplot(aes(x=sex, y=body_mass))+
  geom_boxplot()

# It looks like there is a lot of overlap, but the median values are different. Let's proceed by checking if our data is normally distributed

df |> 
  filter(sex != '') |> 
  ggplot(aes(x=body_mass, color=sex))+
  geom_density()

df |> 
  group_by(species) |> 
  summarise(shapiro_test(body_mass))

#Because we have a bimodal distribution going on, so let's see if we have a large enough sample size to justify a t-test. For skewed data we can try applying a transformation (log10 (), sqrt(), etc.), but this usually does not work for biomdal data. However, the central limit theorem can still apply when the same size is large (n = 30-40).

df |> 
  group_by(sex) |> 
  summarise(n())

#It looks like we do, so we can proceed to looking at variance. Note that we should also compare our t-test results with a non-parametric test as well.
  
#Lets check if the variance in male and female body masses are equal or not.

df |> 
  levene_test((body_mass)~sex)

# Our data is not equal in variance (ugh!). But we are in luck, because we can use the Welch's t-test which is robust to unequal variance. This is also the default t-test for the t.test() function

t.test(data=df, body_mass~sex)

#Our p-value is highly significant! We can also see our means for each group do separate nicely. But recall we have a bimodal distribution so we need to dig further into the data and see what underlying factors could be at work


# Anova -------------------------------------------------------------------

#Let's look at our body mass again, but this time let's include species as a second variable.

#first step is to visualize the data

df |> 
  filter(sex != '') |> 
  ggplot(aes(x=sex, y=body_mass))+
  geom_boxplot()+
  facet_wrap(~species)

# Here we can see that sex does appear to impact body mass, but species can also. To test how body mass responds to sex across different species, we can use an anova.

#Again, let's look at the number of observations within each variable to ensure our design is balanced.An alternative way to do this is using the package skimr, which includes some additional descriptive data

skim(df)

#Otion I: using the lm() function

model<- lm(data = na.omit(df), body_mass~sex*species)

#now pass the model into the anova() function to generate an anova table

anova(model)

#Option II: use the aov() function, which generates a Type I anova

model<- aov(data = na.omit(df), body_mass~sex*species)

# now pass the model into the summary() function to generate an anova table
summary(model)

# note that the anova table does not tell use which species are significantly different from each other, only that species are significantly different.

# We can use a post-hoc test to compare each species interaction and see which ones are significantly different from another.

test<-data.frame(tukey_hsd(model))

#Checking model residuals to ensure no deviance from normality

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


#car::qqPlot includes confidence interval for assess outliers, which is an nice additional feature to the above qqplot.

qqPlot(model)

# Another test for unequal variance: Levene test, which is robust to departures from normality (F-test good for normal data and only two factors, Bartlett test for two or more factors but still normal data). We can test our model residuals by inputting the formula into the Levene test.

df |> 
  levene_test(body_mass~sex*species)

# Here we do not have a significant p-value, so this suggests our residual variance is within acceptable margins.


# Regression analysis -----------------------------------------------------

# Let's look at two continuous traits in our penguins and see if they vary across species. Comparison of two coninuous traits is often done with a linear regression (provided you suspect a linear relationship).

# Let's look at the data

ggplot(na.omit(df), aes(x = bill_len, y = body_mass, color = species)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(
    x = "Bill Length (mm)",
    y = "Body Mass (g)",
    color = "Species"
  )

#there appears to be a linear trend of increasing body mass with increasing bill length, and this trend holds for all species.

# We should also look at the count data and variances as we have before, but let's assume we did and there are not deviations from normality. To run a linear regression, we will use the lm() function again.

mod_interaction <- lm(body_mass ~ bill_len * species, data = df)
summary(mod_interaction)

mod_additive <- lm(body_mass ~ bill_len + species, data = penguins)
summary(mod_additive)

anova(mod_additive, mod_interaction)

# Sometimes we may want to center our independent variable to account for real-world conditions. Below is how we can do that.

df_centered<-df |> 
  mutate(bill_len_centered = bill_len - mean(bill_len, na.rm = TRUE))

mod_interaction <- lm(body_mass ~ bill_len_centered * species, data = df_centered)
summary(mod_interaction)

mod_additive <- lm(body_mass ~ bill_len_centered + species, data = df_centered)
summary(mod_additive)

anova(mod_additive, mod_interaction)

# Non-normal data and parametric tests ------------------------------------

# the alternative to a t-test: Wilcoxin Rank Sum Test (or the Mann-Whitney Test)
# This is used as an alternative for a two-tailed t-test

wilcox.test(data= df, body_mass~sex)

# The alternative to an anova with a single independent variable: the Kruskal-Wallis test. Unfortunately this test cannot handle an interaction term.

kruskal.test(data = df, body_mass~species)

# For multiple independent variables, a common approach with right-skewed data is to use a generalized linear model or GLM. There are other alternatives to a GLM, but again this is just a common approach.

model2<-glm(data = na.omit(df), body_mass~sex*species, family = Gamma)

summary(model2)
