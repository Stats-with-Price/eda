**Exploratory Data Analysis (eda) Package for R**
The goal of this package is to speed up the data exploration process within R.  
This function returns the variable name, count, missing values, mean, standard devfiation, minimum, median, maximum, and, if the sample size is less than 5,000, the p-value of the Shapiro Wilk test.

**Installation**
To install this library in R, please follow these instructions.  If you already have devtools installed, skip to step 2:

1) Download the devtools package by typing this code:
  install.packages('devtools')

2) Load in the devtools package like so:
  library('devtools')

3) Install the eda package using the install_github command:
   install_github('Stats-with-Price/eda')

4) Load in the library:
   library('eda')

**How to Use**
After loading in the eda package, use the function like so:
eda(dataframe, "variable_name", graphs = FALSE/TRUE)
  where: 
  dataframe = the dataset you are using
  variable_name = each variable you want to analyze written in quotation marks
  graphs = TRUE = Returns a histogram and/or a QQ line plot for numeric variables
