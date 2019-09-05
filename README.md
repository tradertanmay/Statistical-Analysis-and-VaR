# statistical-analysis-and-VaR
Expected Shortfall, VaR, Probability Distribution
For this project, Adjusted Closing Prices for Alphabet Inc. (GOOGL) and for Amazon.com, Inc.
(AMZN) was required for the time period between Nov 11, 2017 and Nov 11, 2018. This data was
extracted from Yahoo Finance. We used the R programming language to perform statistical analysis
on the data. We also used many packages in R to not only understand our data, but to also explore
possibilities outside the instructions given for this project. The data (251 trading days) was used to
obtain the log-returns for the stocks. The 0.05 sample quantiles were calculated using the default
Quantile function in R. We went through an exhaustive list of distributions and fitting methods in
search of a good fit for the given data. We want to show a comparison between two distributions
which have shown a significant fit for both GOOGL and AMZN. The two distributions we will be
using for comparison in our report are Logistic and Johnson Unbounded (SU ).
We have observed that the leptokurtic nature of the Johnson distribution makes it a more suitable
distribution for the Expected Shortfall calculations compared to the logistic. We have also verified
all of our findings by writing our own distribution functions for the families of distributions. For this
report we are only including inbuilt R package methods for all calculations.
