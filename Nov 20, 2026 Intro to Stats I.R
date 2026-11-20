# Title: Intro do Stats in R (I)
# Date: 2026-11-19


# Libraries and data ------------------------------------------------------

library(tidyverse)
library(car) # car package holds the Anova function, used for Type II and III anovas
library(rstatix) # for shapiro test and tukey_hsd
library(skimr) #for quick descriptive metrics of a data set
library(palmerpenguins)

df<-penguins


# T-test ------------------------------------------------------------------

# lets compare how male and female penguins compare in body mass. Since we only have two factors for our independent variable and a continuous dependent variable, we can use a t-test.

#Let's take a look at our data

df |> 
  ggplot(aes(x=sex, y=body_mass_g))+
  geom_boxplot()

#It looks like we have some unassigned individuals, so let's remove them for this analysis.

df |> 
  filter(sex != '') |> 
  ggplot(aes(x=sex, y=body_mass_g))+
  geom_boxplot()

# It looks like there is a lot of overlap, but the median values are different. Let's proceed by checking if our data is normally distributed

df |> 
  filter(sex != '') |> 
  ggplot(aes(x=body_mass_g, color=sex))+
  geom_density()

df |> 
  filter(sex != '') |> 
  group_by(sex) |> 
  summarise(shapiro_test(body_mass_g))

#Because we have a bimodal distribution going on, so let's see if we have a large enough sample size to justify a t-test. For skewed data we can try applying a transformation (log10 (), sqrt(), etc.), but this usually does not work for biomdal data. However, the central limit theorem can still apply when the same size is large (n = 30-40).

df |> 
  group_by(sex) |> 
  summarise(n())

#It looks like we do, so we can proceed to looking at variance. Note that we should also compare our t-test results with a non-parametric test as well.
  
# Another test for unequal variance: Levene test, which is robust to departures from normality (F-test good for normal data and only two factors, Bartlett test for two or more factors but still normal data). We can test our model residuals by inputting the formula into the Levene test

df |> 
  levene_test((body_mass_g)~sex)

# Our data is not equal in variance (ugh!). But we are in luck, because we can use the Welch's t-test which is robust to unequal variance. This is also the default t-test for the t.test() function

t.test(data=df, body_mass_g~sex)

#Our p-value is highly significant! We can also see our means for each group do separate nicely. But recall we have a bimodal distribution so we need to dig further into the data and see what underlying factors could be at work


# Anova -------------------------------------------------------------------

#Let's look at our body mass again, but this time let's compare species
#first step is to visualize the data

df |>  
  ggplot(aes(x=species, y=body_mass_g))+
  geom_boxplot()

# Here we can see that body mass is different between species. To test how body mass responds across the three different species, we can use an anova.

#Again, let's look at the number of observations within each variable to ensure our design is balanced. An alternative way to do this is using the package skimr, which includes some additional descriptive data

skim(df)

#Otion I: using the lm() function

model<- lm(data = na.omit(df), body_mass_g~species)

#now pass the model into the anova() function to generate an anova table

anova(model)

#Option II: use the aov() function, which generates a Type I anova

model<- aov(data = na.omit(df), body_mass_g~species)

# now pass the model into the summary() function to generate an anova table
summary(model)

# note that the anova table does not tell use which species are significantly different from each other, only that species are significantly different.

# We can use a post-hoc test to compare each species interaction and see which ones are significantly different from another.

test<-data.frame(tukey_hsd(model))

#Checking model residuals to ensure no deviance from normality

shapiro.test(residuals(model))

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

#car::qqPlot includes confidence interval for assess outliers, which is an nice additional feature to the above qqplot.

qqPlot(model)

#Lets check if the variance in male and female body masses are equal or not.

df |> 
  levene_test(body_mass_g~species)

# Here we do  have a significant p-value, so this suggests our residual variance is outside acceptable margins. Since we have some outliers and differences in sample sizes, we should address these issues before interpreting our results too closely.


# Anova with two independent variables-------------------------------------------------------------------

#Let's look at our body mass again, but this time let's include species as a second variable.

#first step is to visualize the data

df |> 
  filter(sex != '') |> 
  ggplot(aes(x=sex, y=body_mass_g))+
  geom_boxplot()+
  facet_wrap(~species)

# Here we can see that sex does appear to impact body mass, but species can also. To test how body mass responds to sex across different species, we can use an anova.

#Again, let's look at the number of observations within each variable to ensure our design is balanced.An alternative way to do this is using the package skimr, which includes some additional descriptive data

skim(df)

#Otion I: using the lm() function

model2<- lm(data = na.omit(df), body_mass_g~sex*species)

#now pass the model into the anova() function to generate an anova table

anova(model2)

#Option II: use the aov() function, which generates a Type I anova

model2<- aov(data = na.omit(df), body_mass_g~sex*species)

# now pass the model into the summary() function to generate an anova table
summary(model2)

# note that the anova table does not tell use which species are significantly different from each other, only that species are significantly different.

# We can use a post-hoc test to compare each species interaction and see which ones are significantly different from another.

test2<-data.frame(tukey_hsd(model2))

#Checking model residuals to ensure no deviance from normality

shapiro.test(residuals(model2))

par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

#car::qqPlot includes confidence interval for assess outliers, which is an nice additional feature to the above qqplot.

qqPlot(model2)

#Lets check if the variance in male and female body masses are equal or not.

df |> 
  levene_test(body_mass_g~sex*species)

# Here we do not have a significant p-value, so this suggests our residual variance is within acceptable margins.


# Linear Regression -------------------------------------------------------

# Let's look at two continuous traits in our penguins, body mass and bill length, and see if body mass varies with respect to bill length. Comparison of two continuous traits is often done with a linear regression (provided you suspect a linear relationship).

# Let's look at the data

ggplot(na.omit(df), aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(
    x = "Bill Length (mm)",
    y = "Body Mass (g)",
    color = "Species"
  )

#there appears to be a linear trend of increasing body mass with increasing bill length

# We should also look at the count data and variances as we have before, but let's assume we did and there are not deviations from normality. To run a linear regression, we will use the lm() function again.

#first we will run an interaction effect between bill length and species, which assess the body mass response to bill length as independent for each species.

model_lm <- lm(body_mass_g ~ bill_length_mm, data = na.omit(df))
summary(model_lm)

# It looks like we do have a significant effect of bill length on body mass, and our adjusted R-squared value is about 0.35. We can interpret this to mean that about 35% of body mass change is explained by bill length. Looking at our estimates, it seems that each 1mm increase in bill length corresponds to about 86.8 g of body mass increase.
# However, not our residual error is rather high, so we should continue parsing out our data and consider what additional factors may be at work


# Regression analysis with an interaction term -----------------------------------------------------

# Let's look at the same two continuous traits in our penguins and see if they vary across species.

# Let's look at the data

ggplot(na.omit(df), aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(
    x = "Bill Length (mm)",
    y = "Body Mass (g)",
    color = "Species"
  )

# Here we see that our linear trend remains but has a much better fit within each species than for all species combined.


#first we will run an interaction effect between bill length and species, which assess the body mass response to bill length as independent for each species.

mod_interaction <- lm(body_mass_g ~ bill_length_mm * species, data = df)
summary(mod_interaction)

#Next we can run an additive model that assumes body mass responds similarly across all species (i.e. the slopes of each regression line are the same) but still allows for the intercepts to differ.

mod_additive <- lm(body_mass_g ~ bill_length_mm + species, data = df)
summary(mod_additive)

# we can compare models to assess whether an additive effect or interaction effect is a better fit. If the p-value is less than 0.05, this indicates that an interaction effect is a better fit.

anova(mod_additive, mod_interaction)

#since the p-value is less than 0.05, we select the interaction model.

# Sometimes we may want to center our independent variable to account for real-world conditions. What this does is allow the intercept position to be moved from 0 (i.e. a bill length of 0.0 mm) to biologically-relevant x value.

# below we will make a new column of centered data for our bill length, and run the same linear regressions as above.

df_centered<-df |> 
  mutate(bill_length_mm_centered = bill_length_mm - mean(bill_length_mm, na.rm = TRUE))

mod_interaction <- lm(body_mass_g ~ bill_length_mm_centered * species, data = df_centered)
summary(mod_interaction)

mod_additive <- lm(body_mass_g ~ bill_length_mm_centered + species, data = df_centered)
summary(mod_additive)

anova(mod_additive, mod_interaction)

# The results are the same as above for the model overall, but the effect of each species has become significant. 

#finally, we want to make sure our model's main effects (species and bill length) are significant, and for this we run our interaction model through and anova.

anova(mod_interaction)

#the above confirms we have significant main effects and a significant interaction.


# Non-normal data and parametric tests ------------------------------------

# the alternative to a t-test: Wilcoxon Rank Sum Test (or the Mann-Whitney Test)
# This is used as an alternative for a two-tailed t-test

wilcox.test(data= df, body_mass_g~sex)

# The alternative to an anova with a single independent variable: the Kruskal-Wallis test. Unfortunately this test cannot handle an interaction term.

kruskal.test(data = df, body_mass_g~species)

