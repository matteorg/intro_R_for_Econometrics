## Introduction to R ----

## IMPORTANT ##
# Make sure you have the most recent version of R installed on your notebook
# Some packages won't be installed/loaded otherwise

## Packages ----
# What are they? A package is just a bunch of codes that is written to make
# our life easier. Some are already installed on R Studio (you can see them in
# the bottom right corner), others you have to install
# To install --> type install.packages("package_name")
# How do you know which package you need? ---> google it! Here I put a list of the 
# most common/useful ones

# Load useful packages
# To load a package use function library("package_name") or p_load(package_name)
# after executing library("pacman")
library(tidyverse)    # probably your best companion for data analysis
library(readxl)       # excel files reader
library(foreign)      # necessary to import datasets in formats like .dta (STATA) etc.
library(stargazer)    # nice way to export tables in LaTex format
library(haven)        # good alternative to "foreign", to read STATA/Sas/Spss data
library(lmtest)       # great to test linear regression models
library(sandwich)     # produces robust standard errors
library(multiwayvcov) # estimators for clustered standard errors
library(car)          # nice estimation and post-estimation commands
library(datasets)     # package containing many datasets
library(Hmisc)        # package to assign labels to variables
library(janitor)      # for nice and easy descriptions of data
library(vtable)       # for tables with different interesting options

## Begin ----
## Always clear the working space before you start
# i.e. everything in the Environment (top-right corner of RStudio) will disappear

rm(list=ls())

## Set your working directory and create subfolders
setwd("~/Directory/Pigs/Dont/Fly/.../or/Do/They")
# Put your own!! You can also click "Session"-->"set working directory"-->"choose directory" 
# and after you choose it, you can just copy and paste the code in the script

## Create subfolder within main directory
dir.create("data")

## Check what's inside folder "data"
list.files("./data")

## Asking R for help ----
# What if you see a function and do not know/understand what that is?

# Access the help file with description of such function
?setwd

# Search help files (there will be the one you are looking for, plus many more)
help.search("setwd")

# Get arguments (not so interesting for 'setwd'.. try with standard normal
# distribution 'rnorm', see what happens)
args("rnorm")

# In general, I prefer using Google to find the answers I need
# Most useful website ----> stackoverflow

## Importing and reading data ----
## Importing a dataset from an external source 

# Store the url as an object
fileUrl = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"

# Download the file. Important parameters: destination file, for example
download.file(fileUrl, destfile = "./data/counties.csv")
list.files("./data")

# Show downloaded date
dateDownloaded = date()
dateDownloaded

## Read dataset into R Studio
# Data come in several formats, and each usually requires a different function
# 'readxl' for '.xlsx' files
# 'read.dta' for '.dta' files
# 'readRDS' for R data
# In this case: 'read.csv' function
countyData = read.csv("./data/counties.csv")
head(countyData) # it shows in the console what the dataset looks like from the top
tail(countyData)

# Browse your data
View(countyData)
# ...or click on the right of the dataset (the small grid simbol) ---->

# Remove dataset
rm(countyData)

# Remove multiple objects --> see what happens there --> (look right)
rm(fileUrl, dateDownloaded)
# What if you repeat this command? Just a WARNING, no error.

## Clean and transform data: first steps ----
# Most functions come from 'tidyverse' package

# Make handle again (not necessary but useful)
fileURL = "https://github.com/DataScienceSpecialization/courses/blob/master/03_GettingData/dplyr/chicago.rds?raw=true" 

# Download data
download.file(fileURL, destfile = "./data/chicago.rds", mode = "wb", extra = '-L')
list.files("./data")

# Read data from R format ('R Data Set')
chicago = readRDS("./data/chicago.rds")

# Check dimension of your matrix (how many rows[i.e. observations]? how many columns[i.e. variables]?)
dim(chicago)

# Label variables (why would you do that? Sometimes variable names suck, e.g. dptp.
# what's dptp? You may want to add a short description. That's the label's purpose)
# Package required: Hmisc
Hmisc::label(chicago[[1]]) <- "City"
Hmisc::label(chicago$dptp) <- "Dew Point Temperature"

# Select which data to display according to variable names
# 'select' allows you to subset your variables
# you can write the variables you want to keep, separated by ','
# if there are many consecutive ones you can use ':'
# Example: you want to keep from var1 to var5 --> select(var1:var5)
# Other example here below
head(select(chicago, city:dptp))

# Create subset of your data by keeping only some variables (e.g. all but city)
chic.s = select(chicago, select=tmpd:no2tmean2) #see!---> 
# chic.s has now indeed 7 variables instead of 8

# Subset your data by keeping only certain observations
# The conditions for subsetting come from 'filter'
chic.f = filter(chicago, pm25tmean2 > 30)

## The pipe operator ----
# You can perform multiple operations at once using the %>% operator (from 'tidyverse')
# This reads 'then'. So we can tell r to create a new dataset in the following way:
# 1- start from the dataset chicago THEN
# 2- keep only rows for which pm25tmean2>30 and tmpd>80 THEN
# 3- keep only the variables pm25tmean2 and tmpd afterwards
chic.f = chicago %>% 
  filter(pm25tmean2 > 30 & tmpd > 80) %>% 
  select(tmpd, pm25tmean2)

# The pipe operator can be easily typed with Ctrl+Shift+M (Windows) or Cmd+Shift+M (Mac)
# You can then type Ctrl+Return (Cmd+Return) at ANY line of the command, and all the lines
# with the pipe operator will be executed

## Sort, Rename and Generate Variables ----
# Sort data by date - choose how to display them (from the top or bottom)
chicago_sort = chicago %>% 
  arrange(date) %>% # sorted in ascending order
  select(date, pm25tmean2)
head(chicago_sort, 3)

chicago_desc = chicago %>% 
  arrange(desc(date)) # sorted in descending order
head(chicago_desc, 3)

## Renaming variables in R
chicago = chicago %>% 
  rename(dewpoint = dptp, pm25 = pm25tmean2)
# As you see, write NEW name = OLD name

## Generating new variables
# The function you want to use is 'mutate' again from 'tidyverse'

# Create 2 variables equal to the double of 2 existing ones
chicago = chicago %>%
  mutate(tmpd2 = 2*tmpd,
         dewpoint2 = 2*dewpoint)
# just separate by a ',' the new variables you want to create

# Create a categorical variable
chicago = chicago %>% 
  mutate(tempcat = ifelse(tmpd>=45, "hot", "cold"))
# REMARK: how does the function 'ifelse' work?
# 'ifelse' has 3 arguments, separated by ',' and reads as follow:
# if a condition holds (first argument), execute a function (second argument), 
# else execute another function (third argument)
# Therefore, in our case: if tmpd>=32, assign text "hot", otherwise assign string "cold"

# See if it worked, with nice function 'tabyl' (from 'janitor' package)
tabyl(chicago$tempcat)
# As you can see, 2815 observations have now label 'cold', 4124 label 'hot'
# We also have 1 missing value (that's <NA>)

# Create a dummy/binary variable, i.e. a 1/0 variable
chicago = chicago %>% 
  mutate(hot = ifelse(tempcat == "hot", 1, 0))
# So the if tempcat is labelled 'hot', assign value 1, otherwise 0

# If you tabulate hot, you'll see the exact same numbers we got
# from tabulating 'tempcat'
tabyl(chicago$hot)

## Important remarks needed here when you set conditions!
# You can use == (exactly equal), >, <, <=, >=, or != (not equal)
# "&" sets joint conditions, "|" means "or"
# HOWEVER: you cannot say chicago$tmpd!=NA to say "if tmpd is not missing"
# Missing values have their own special syntax
# You need to use is.na(variable_name)==TRUE/FALSE
# which reads: [if variable is (TRUE) or is not (FALSE) missing (na)]

# Example with missing values conditions
chicago = chicago %>% 
  mutate(tempcat = ifelse(is.na(tempcat) == TRUE, "missing", tempcat))
# so if tempcat is missing, assign label 'missing', otherwise leave the existing label

# Et voila!
tabyl(chicago$tempcat)

## Summary statistics ----
## Basic summaries

# Let's first create a folder for figures and tables
if(!file.exists("Figures&Tables")){
  dir.create("Figures&Tables")
}

# Simple table of summary statistics (type="text" only useful to see result in console)
stargazer(chicago, type="text")

# Do it also for a subset of your variables and export
stargazer(chicago %>% 
            select(pm25, pm10tmean2, hot), out="./Figures&Tables/Table1.html")
# As you see the default output in the console is in LaTex
# No worries, an html file has been stored safely in the Figures&Tables folder

## Summaries by group
# Example 1: by 'tempcat'
# Function 'st' from package 'vtable'
st(chicago, group='tempcat')
# Preview in Viewer ---> as you see, there's 3 columns for each group!

# If you want to export in a nice format
st(chicago, out='browser') # open table in browser
st(chicago, file='./Figures&Tables/Table2.html') # store it as html file
# You can then take a screenshoot, it you find it easier

## Plots and Figures ----
# From 'tidyverse' we use function 'ggplot'
# There are easy basic plots already integrated in R base package, however I
# think the range of possibilities with 'ggplot' is much wider

# We now use 'gapminder' though
p_load(gapminder)
# Remember p_load? It combines 'install.packages' and 'library' at once

# Load the data
gapminder = gapminder

# BEFORE WE START: ggplot has an EXTENSIVE documentation and examples online
# Whatever you don't find here, you will find easily online

# Basic syntax --> ggplot(data=yourdata, mapping = aes(x=xvar, y=yvar))
# This creates an x/y plane defined by the xvar and yvar you choose from yourdata
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
# From this point on you can start plotting the data simply by
# typing "+ function". See below the examples

# Basic scatter plot: use '+ geom_point()'
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()

# Change the point color, size and transparency
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, col = continent)) + 
  geom_point(alpha = 0.3) # "alpha" controls transparency. Takes a value between 0 and 1
# As you see in this example
# 1- we don't need to write 'data = ' and 'mapping ='
# 2- we can change size of data points according to population values
# 3- we can change color of data points according to continent

# At times it can be convenient to define an intermediate plot object
# that we can re-use.
g = ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))

# You can add regression lines by using: geom_smooth(), stat_smooth() or geom_abline()
# Let's see geom_smooth()
# method : smoothing method to be used. Possible values are lm, glm, gam, loess, rlm
# se : logical value. If TRUE, confidence interval is displayed around smooth (default)
# fullrange : logical value. If TRUE, the fit spans the full range of the plot (default)
# level : level of confidence interval to use. Default value is 0.95
# See example below
g + 
  geom_point(aes(size = pop, col = continent), alpha = 0.3)  +
  geom_smooth(method = "lm")
# it doesn't make much sense here, right?

# Make it fancier. A curve is probably better than a line
g + 
  geom_point(aes(size = pop, col = continent), alpha = 0.3)  +
  geom_smooth(method = "loess", linetype="dashed", color="darkred", fill="blue")

# You can build out some truly impressive complexity and 
# transformation of your visualization through this 'simple' 
# layering process. (no need to transform data)
g2 = g + 
  geom_point(aes(size = pop, col = continent), alpha = 0.3) +
  scale_color_brewer(name = "Continent", palette = "Set1") + ## Different color scale
  scale_size(name = "Population", labels = scales::comma) + ## Different point (i.e. legend) scale
  scale_x_log10(labels = scales::dollar) + ## Switch to logarithmic scale on x-axis. Use dollar units
  labs(x = "Log (GDP per capita)", y = "Life Expectancy") + ## Better axis titles
  theme_minimal() ## Try a minimal (b&w) plot theme

# Display the plot
g2

## Other common plots

# Histogram
# Basic histogram
ggplot(gapminder, aes(x=lifeExp)) + geom_histogram()
# Change the width of bins
ggplot(gapminder, aes(x=lifeExp)) + 
  geom_histogram(binwidth=1)
# Change colors
ggplot(gapminder, aes(x=lifeExp)) + 
  geom_histogram(color="black", fill="white")

# Kernel density
ggplot(gapminder, aes(lifeExp, fill = continent, colour = continent)) +
  geom_density(alpha = 0.1)

## Make side-by-side plots
p_load(cowplot) # install and load library 'cowplot'
gr1 = ggplot(gapminder, aes(x=lifeExp)) + 
  geom_histogram(color="black", fill="white") # store histogram

gr2 = ggplot(gapminder, aes(lifeExp, fill = continent, colour = continent)) +
  geom_density(alpha = 0.1) + # store density plot
  theme(legend.position = c(0.25, 0.76))

plot_grid(gr1, gr2, label = "AUTO",
          align = "hv", nrow = 1, ncol=2) # Click 'zoom' to see them better

## Export plots to files
# use 'save_plot' from package 'cowplot' --> simple syntax

# Simple plots
save_plot("Figures&Tables/scatter.png", g2)

# Side-by-side plots
p = plot_grid(gr1, gr2, label = "AUTO",
              align = "hv", nrow = 1, ncol=2)
save_plot("Figures&Tables/plot.png", p, ncol = 2)

## Animated plots ----
# This is not relevant to solving problem sets or writing Pdf documents
# However it is sometimes (maybe for your future projects) nice to display
# data in dynamic way. The following code should be executed ONLY in 
# R Markdown to PRODUCE AN HTML document (NOT A PDF)
# DON'T RUN THE CODE IN THE SCRIPT

p_load(gganimate, gifski, av)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) + # scatter plot
  scale_colour_manual(values = country_colors) + # produce 1 scatter for each country
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific part
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) + # Show the scatter plots year by year
  ease_aes('linear')

# For those interested, here are some other datasets you can play around with
# These come from the very famous Wooldridge book(s)
# R has a library containing these datasets
p_load(wooldridge)

## Regressions in R ----
# great source: https://raw.githack.com/uo-ec607/lectures/master/08-regression/08-regression.html#High_dimensional_FEs_and_multiway_clustering

# Suppose you want to do the regression analysis in another folder --> change directory 
setwd("~/The/unintended/consequences/of/love")
dir.create("./analysis")

# Remove all plots
dev.off()

## Simple linear regression ---- 
# Base function 'lm'
# Use one of Wooldridge datasets
ceosal = wooldridge::ceosal1

# Syntax --> lm(formula, data)
model = lm(salary ~ roe, ceosal)
stargazer(model, type="text") # interpretation?

# Plot the relationship in a graph
ggplot(ceosal, aes(x = roe, y = salary)) + geom_point() + geom_smooth( method= "lm")

## Multiple linear regression ----
# Still function 'lm'
gpa3 = wooldridge::gpa3 #new dataset, just for fun

# Run a model with many regressors
modelmulti = lm(cumgpa~female + sat + hsperc + tothrs, gpa3) #does it make sense to you?
stargazer(modelmulti, align=TRUE, type="text")

# Add interactions (what are they?)
linear_1 = lm(cumgpa~female*(tothrs) + sat + hsperc, data=gpa3) 
linear_2 = lm(cumgpa~female*(sat+hsperc+tothrs), data=gpa3)

# Display results in same table
stargazer(linear_1, linear_2, title="Results", 
          align=TRUE, type="text") #align argument TRUE, so that coefficients in each column are aligned along the decimal point.

# You can also add lines...
stargazer(linear_1, linear_2, title="Results",
          align=TRUE, type="text", add.lines = list(c("Fixed Effects", "No", "No")))

# ...and keep only the statistics you want with 'keep.stat=c("n", "rsq")'
stargazer(linear_1, linear_2, title="Results", 
          align=TRUE, type="text", 
          add.lines = list(c("Fixed Effects", "No", "No"), c("Matteo", "yes", "no")),
          keep.stat=c("n", "rsq"), out="./Figures&Tables/Table3.html")


## Test of hypothesis ----
# Use package "car" if you want to test hypotesis on interacted (or just any) coefficients
p_load(car)

# Test that all coefficients including female (so female + interactions) are 0
linearHypothesis(linear_1, matchCoefs(linear_1, "female"))

# Only test interactions being equal to each other
linearHypothesis(linear_2, c("female:sat=female:hsperc", "female:sat=female:tothrs"))

# Test 2 coefficients = 0
linearHypothesis(linear_2, c("female=0", "sat=0"))

## IV Regressions ----
ivdata = wooldridge::card # yet another wooldridge dataset

# The OLS estimation
olsmod = lm(wage ~ educ + black + fatheduc + exper + IQ + south 
            + married + momdad14, ivdata)

# The IV estimation --> nearc2 is instrument for educ
# METHOD 1: package 'AER', function 'ivreg'
p_load(AER)
ivmod1 = ivreg(wage ~ educ + black + fatheduc + exper + IQ + south 
              + married + momdad14 | nearc2 + black + fatheduc + exper + IQ + south 
              + married + momdad14, data = ivdata)
# here you MUST write 'data = dataname'. If you forget to write 'data = ', you get an error

# METHOD 2: package 'lfe', function 'felm'
p_load(lfe)
ivmod2 = felm(wage ~ black + fatheduc + exper + IQ + south 
               + married + momdad14 | 0 | 
                (educ ~ nearc2 + black + fatheduc + exper + IQ + south 
               + married + momdad14), data = ivdata)
# here you DON'T write the INSTUMENTED variable in the first formula, just in the
# 'first stage' model

# Function felm works as follows: felm(formula | Fixed Effects | First Stage, data)
# In this case you have no fixed effects, so you just type 0
# Therefore --> felm(formula | 0 | First Stage, data)

# METHOD 3: package 'plm', function 'plm'
p_load(plm)
ivmod3 = plm(wage ~ educ + black + fatheduc + exper + IQ + south 
             + married + momdad14 | nearc2 + black + fatheduc + exper + IQ + south 
             + married + momdad14, data = ivdata, model="pooling")
# Here you must specify model="pooling", otherwise the syntax is == with ivreg

# You'll see here that you get identical results
stargazer(olsmod, ivmod1, ivmod2, ivmod3, keep.stat="n", type = "text")

# First stage
firststage = lm(educ ~ nearc2 + black + fatheduc + exper + IQ + south 
                + married + momdad14, ivdata)
# F-statistic on excluded instruments
Fstat = linearHypothesis(firststage, "nearc2=0")$F[2]

# Show all nicely in one table
stargazer(olsmod, ivmod, firststage, keep.stat = c("n"), type="text",
          add.lines = list(c("R2", 
                             round(summary(olsmod)$r.squared,3),
                             "",
                             round(summary(firststage)$r.squared,3)),
                           c("F","","",round(Fstat,3))),
          keep = c("educ","nearc2"))
# As you I choose not to display IV regression R-squared as it often makes no sense
# It's also often required to add the F-test on the excluded instruments in the table
# This way you have all the important information in the table

## Panel Data Analysis ----

# Open a panel dataset
crime = wooldridge::crime4

# We have COUNTY by YEAR observations --> PANEL STRUCTURE
# In many cases a variable like county should be coded numerically first
# Just use function 'as.factor' within the formula

# Suppose we want to regress crime rate on probability of ending in prison

# Simple linear regression model (no Fixed Effects)
ols = lm(crmrte ~ prbpris, data=crime)

stargazer(ols, title="OLS", align=TRUE, type="text", keep = "prbpris",
          add.lines = list(c("County FE", "N"),c("Year FE","N")), keep.stat=c("n", "rsq"))

# Here we start looking at Fixed Effects estimation functions
# Suppose we want to control for year specific shocks across counties

# METHOD 1: Simply use lm
panel1a = lm(crmrte ~ prbpris + as.factor(year), crime)

# METHOD 2: use again felm
panel1b = felm(crmrte ~ prbpris | year | 0, crime)
# Since there's no First Stage --> felm(formula | fixed effects | 0, data)

# METHOD 3: use again plm
panel1c = plm(crmrte ~ prbpris, crime, index = "year",
    effect = "individual", model = "within")

stargazer(panel1a, panel1b, panel1c, align=TRUE, type="text", keep = "prbpris",
          add.lines = list(c("County FE", "N","N","N"), 
                           c("Year FE", "Y","Y","Y")), 
          keep.stat=c("n", "rsq"))

# Suppose we want to control for time specific characteristics and county specific ones
# Therefore we want both COUNTY and YEAR fixed effects

# METHOD 1: lm
panel2a = lm(crmrte ~ prbpris+factor(county)+factor(year), data=crime)

# METHOD 2: felm
panel2b = felm(crmrte ~ prbpris | county + year | 0, crime)

# METHOD 3: plm
panel2c = plm(crmrte ~ prbpris, crime, index = c("county","year"),
              effect = "twoways", model = "within")

# As you see again, identical results
stargazer(panel2a, panel2b, panel2c, align=TRUE, 
          type="text", keep = "prbpris", keep.stat=c("n", "rsq"),
          add.lines = list(c("County FE", "Y", "Y", "Y"), 
                           c("Year FE", "Y", "Y", "Y")))

# In the end, choose the function you prefer
# You can then have different models in one column
stargazer(ols, panel1b, panel2b, align=TRUE, 
          type="text", keep = "prbpris", keep.stat=c("n", "rsq"),
          add.lines = list(c("County FE", "N", "N", "Y"), 
                           c("Year FE", "N", "Y", "Y")),
          column.labels = c("OLS","ONE FE","TWO FE"))

# Robust Standard Errors as in Stata ----

# METHOD 1: lm + plm
robse1 = lm(crmrte ~ prbpris + factor(year), crime)
lmse = sqrt(diag(vcovHC(robse1, type = "HC1"))) # 'vcovHC' comes from 'plm'

# METHOD 2: felm + plm
robse2 = felm(crmrte ~ prbpris | year | 0, crime)
felmse = sqrt(diag(vcovHC(robse2, type = "HC1")))

# METHOD 3: plm
# I leave this to you as an exercise

# Standard errors are the same
stargazer(robse1, robse2, se=list(lmse, felmse), type="text", keep = "prbpris")

# If you want to compare robust vs non-robust standard errors
stargazer(robse1, robse1, se=list(NULL, lmse), type="text", keep = "prbpris")


## Clustered Standard Errors as in (or similar to) Stata ----
p_load(multiwayvcov)

# One way clustering
# lm
clust1 = lm(crmrte ~ prbpris + factor(county) + factor(year), crime)
lmclust = sqrt(diag(cluster.vcov(clust1, crime$county, df_correction = T)))

# plm
clust2 = plm(crmrte ~ prbpris, crime, index=c("county","year"), 
             effect="twoways", method = "within")
# degrees of freedom adjustment
G = length(unique(crime$county))
N = length(crime$county)
dfa = (G/(G - 1)) * (N - 1)/clust2$df.residual
plmclust = sqrt(diag(dfa*vcovHC(clust2, type="HC0", cluster = "group", adjust = T)))


# felm
clust3 = felm(crmrte ~ prbpris | county + year | 0 | county, crime)

# Results
stargazer(clust1, clust2, clust3, keep="prbpris", type = "text",
          se = list(lmclust, plmclust, NULL), digits=6)

# The first two return cluster standard errors exactly as in Stata 'reg + cluster'
# The third returns cluster standard errors similar to Stata 'reghdfe + cluster'

# Here is an interesting point: https://github.com/lrberge/fixest/issues/6#issuecomment-558997359
# Here one more from reghdfe author: https://github.com/sgaure/lfe/issues/1#issuecomment-530643808
# Here is a comparison between 'feols' and 'felm': https://cran.r-project.org/web/packages/fixest/vignettes/standard_errors.html
# The discussion about comparability across softwares is still ongoing.
# There are many underlying theoretical issues to be solved to have one 'best' way for all

# Two way clustering
# I just show it with 'felm' because it has the easier syntax
twoway = felm(crmrte ~ prbpris | county + year | 0 | county + year, crime)
stargazer(twoway, type="text", digits=6)
# so the type of clustering goes always at the end, just before the data

## Simulate samples drawn from known distributions ----

# Suppose you want to draw a sample of size 100 from a normal distribution
# with mean 2 and sd 1.
normal1 = replicate(n=1, rnorm(100,2,1))

# Let's draw 10 samples now
normal2 = replicate(n=10, rnorm(100,2,1))

# For replicability --> 'set.seed' before simulating samples (google it!)