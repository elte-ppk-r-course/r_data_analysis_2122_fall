install.packages(“patchwork”) library(patchwork) library(dplyr)
library(ggplot2) library(Hmisc) library(corrplot) library(car)
\#\#\#\#\#In this report i am going to examine how investment (var.
spend\_per\_resident\_points) affect different amenities (var.
amenities\_points) in parks. I also examine whether parks are rather
dog-friendly (var. dogpark\_points) or child-friendly (var.
playground\_points)

\#STEPS: \#1. Create hypothesis \#2. Explore data \#3. Assumption
testing \#4. Hypothesis testing & Visualization \#5. Conlusion

\#\#\#\#STEP1: Hypothesis \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\# H1:
Higher amount of investment leads to more amenities. H2: Parks are
rather child-friendly than dog-friendly.

\#\#\#\#STEP2: EDA \#\#\#\#\#\#\#\#\#\#\#
install.packages(“tidytuesdayR”) tuesdata &lt;-
tidytuesdayR::tt\_load(‘2021-06-22’) tuesdata &lt;-
tidytuesdayR::tt\_load(2021, week = 26)

parks &lt;- tuesdata$parks View(parks) head(parks)

\#EDA summary(parks)

\#selected variables are going to be used: year, city,
spend\_per\_resident\_points, basketball\_points, dogpark\_points,
playground\_points, rec\_sr\_points, restroom\_points,
splashground\_points, amenities\_points df&lt;- select(parks, c(year,
city, spend\_per\_resident\_points, dogpark\_points, playground\_points,
amenities\_points))

\#plot: amenities by year df %&gt;% ggplot(aes(x = year, y =
amenities\_points)) + geom\_point(color = “darkblue”) + labs(title =
“Amenities in parks by years”, y = “Amenities (points 0-100)”, x =
“Date”) + theme\_bw(base\_size = 11) \# ——-›as the graph of amenities
shows, amenities are measured since 2015, therefor years 2012-2014 can
be deleted df&lt;- df %&gt;% as.data.frame(filter\_at(vars(1),
any\_vars(. &gt; 2014)))

\#plot: dog-parks by year df %&gt;% ggplot(aes(x = year, y =
dogpark\_points)) + geom\_point(color = “darkblue”) + labs(title =
“Dog-parks by year”, y = “Dog parks (points 0-100)”, x = “Date”) +
theme\_bw(base\_size = 11) \#plot: playgrounds by year  
df %&gt;% ggplot(aes(x = year, y = playground\_points)) +
geom\_point(color = “darkblue”) + labs(title = “Playgrounds by year”, y
= “Plyagrounds (points 0-100)”, x = “Date”) + theme\_bw(base\_size = 11)

\#\#——-› by looking at the graphs it seems like there is no significant
difference between dog-parks and playgrounds, testing H2 will give the
final answear.

\#\#\#\#STEP3: Assumption-check
\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

\#H1 will be tested by linear regression, I start with checking
normalities lm&lt;-
lm(df*s**p**e**n**d*<sub>*p*</sub>*e**r*<sub>*r*</sub>*e**s**i**d**e**n**t*<sub>*p*</sub>*o**i**n**t**s* *d**f*amenities\_points)
hist(lm$resid, main=“Histogram of Residuals”, ylab=“Residuals”)

shapiro.test(lm$resid)

\#\#——-› residuals ara not normally distributed. I stop checking other
assumptions, linear regression cannot be performed. Later I use simple
correlation instead. After checking normality, Spearmant-correlation
will be used
shapiro.test(df*d**o**g**p**a**r**k*<sub>*p*</sub>*o**i**n**t**s*)*s**h**a**p**i**r**o*.*t**e**s**t*(*d**f*spend\_per\_resident\_points)

\#H2 will be tested by t-test, I start with checking normalities
shapiro.test(df*p**l**a**y**g**r**o**u**n**d*<sub>*p*</sub>*o**i**n**t**s*)*s**h**a**p**i**r**o*.*t**e**s**t*(*d**f*spend\_per\_resident\_points)

\#\#——-› data is not normal, still I go with t-test, because it is
robust to non-normality

\#\#\#\#STEP4: testing hypothesis \#H1 by correlation

cor&lt;-
rcorr(cbind(df*s**p**e**n**d*<sub>*p*</sub>*e**r*<sub>*r*</sub>*e**s**i**d**e**n**t*<sub>*p*</sub>*o**i**n**t**s*, *d**f*amenities\_points),
type=“spearman”) cor

\#plotting H1

plot(df*s**p**e**n**d*<sub>*p*</sub>*e**r*<sub>*r*</sub>*e**s**i**d**e**n**t*<sub>*p*</sub>*o**i**n**t**s* *d**f*amenities\_points,
pch = 16, cex = 1.3, col = “darkblue”, main = “Amenities plotted against
investment”)
abline(lm(df*s**p**e**n**d*<sub>*p*</sub>*e**r*<sub>*r*</sub>*e**s**i**d**e**n**t*<sub>*p*</sub>*o**i**n**t**s* *d**f*amenities\_points))

\#\#——-› correlation gives a significant result, there is moderate
positive linear relationship between the two variables. Higher
investment comes with higher points of amenities. p&lt;0,05, r=0,62

\#H2: by paired t-test
t.test(df*d**o**g**p**a**r**k*<sub>*p*</sub>*o**i**n**t**s*, *d**f*playground\_points,
paired = TRUE, alternative = “less”)

\#plotting H2

p1 &lt;-ggplot(df, aes(x = year, y = dogpark\_points)) + geom\_bar(stat
= “summary”, fun=“mean”, fill = “darkblue”) + labs(title = “Dog-parks by
year”, y = “Dog parks (points 0-100)”, x = “Year”)

p2 &lt;-ggplot(df, aes(x = year, y = playground\_points)) +
geom\_bar(stat = “summary”, fun=“mean”, fill = “darkblue”) + labs(title
= “Playgrounds by year”, y = “Playgrounds (points 0-100)”, x = “Year”)

p1+p2

\#\#——-› t-test confirmed what the earlier graph shew: there isn’t any
significal difference between the amount of dog-parks and the amount of
playgrounds. P&gt;0,05

\#STEP5: results \#H1: Higher amount of investment leads to more
amenities. —-› linear regression could not be performed, although
correlation gives significant result: Higher investment comes with
higher points of amenities. However it does not mean that investment
affect the amount of amenities, it is only a orrelation, not causation.
We cannot reject nor support this hypothesis.

\#H2: Parks are rather child-friendly than dog-friendly.—-› according to
t-test, there is no difference between the amount of dog-friendliness
and child-friedliness, we need to reject H2.
