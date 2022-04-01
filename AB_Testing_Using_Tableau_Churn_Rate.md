Bank Churn Rate Using AB Testing In R & Tableau
================
Adam LaFave
2022-03-28

# Introduction

This document covers a case study to try and identify why customer’s
were leaving a bank(churn rate) at a faster rate than they were in the
past. The example is done using the visualizations and AB Testing in
Tableau. The aim of this document is to show my understanding of Tableau
and how it can be used to not only make great visualizations, but also
to mine data through modeling. I chose to use an R Studio Notebook to
create the document so to make sure the data is clean, view previews of
the data in raw form, and present the steps used in Tableau while
modeling. Of course the entire analysis could be done in R, but it’s
meant to showcase the use of Tableau.

#### About the Data

The data used in this case study is from a sample of 10,000 customers
from a bank in Europe that covers the regions of France, Spain, and
Germany. The bank selected 10,000 customers and took a screen shot of
the data and later in 6 months took another screen shot with a column
identifying which customers left.

The data can be found in this github repository. It was 3rd party sourced from Data
Science A-Z by Kirill Eremenko on Udemy.com.

Limitations and Assumptions: Not knowing the original source of the data
we are making the assumption that this is a sufficient representative
sample of the overall bank’s customer population. For the sake of this
study we will assume hypothetically as such.

#### Goal

The stakeholder’s have asked their analysis department(me), to look at
the data set of 10,000 customers that now has a new column, ’Exited”,
where 1 represents they exited and 0 represents they did not exit. They
asked that we use the other factors and demographics about the customer
data to try and identify trends.

#### Let’s get R Studio Ready

\#Importing the necessary R libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(here)
```

    ## here() starts at C:/Users/UserName/Documents/Projects_For_Portfolio/A-B_Test_Using_Tableau-Churning

``` r
library(skimr)
```

\#Importing the data into R

``` r
bank_data <- read_excel("Bank_Data_Churn_Prediction.xlsx")
```

#### Now we can Checkout the data

\#Viewing the first 6 rows of the table

``` r
head(bank_data)
```

    ## # A tibble: 6 x 14
    ##   RowNumber CustomerId Surname CreditScore Geography Gender   Age Tenure Balance
    ##       <dbl>      <dbl> <chr>         <dbl> <chr>     <chr>  <dbl>  <dbl>   <dbl>
    ## 1         1   15634602 Hargra~         619 France    Female    42      2      0 
    ## 2         2   15647311 Hill            608 Spain     Female    41      1  83808.
    ## 3         3   15619304 Onio            502 France    Female    42      8 159661.
    ## 4         4   15701354 Boni            699 France    Female    39      1      0 
    ## 5         5   15737888 Mitche~         850 Spain     Female    43      2 125511.
    ## 6         6   15574012 Chu             645 Spain     Male      44      8 113756.
    ## # ... with 5 more variables: NumOfProducts <dbl>, HasCrCard <dbl>,
    ## #   IsActiveMember <dbl>, EstimatedSalary <dbl>, Exited <dbl>

\#Checking out all the columns in the data

``` r
colnames(bank_data)
```

    ##  [1] "RowNumber"       "CustomerId"      "Surname"         "CreditScore"    
    ##  [5] "Geography"       "Gender"          "Age"             "Tenure"         
    ##  [9] "Balance"         "NumOfProducts"   "HasCrCard"       "IsActiveMember" 
    ## [13] "EstimatedSalary" "Exited"

\#Summarising all of the columns. Checks to make sure there are no NULL
values

``` r
summary(bank_data)
```

    ##    RowNumber       CustomerId         Surname           CreditScore   
    ##  Min.   :    1   Min.   :15565701   Length:10000       Min.   :350.0  
    ##  1st Qu.: 2501   1st Qu.:15628528   Class :character   1st Qu.:584.0  
    ##  Median : 5000   Median :15690738   Mode  :character   Median :652.0  
    ##  Mean   : 5000   Mean   :15690941                      Mean   :650.5  
    ##  3rd Qu.: 7500   3rd Qu.:15753234                      3rd Qu.:718.0  
    ##  Max.   :10000   Max.   :15815690                      Max.   :850.0  
    ##   Geography            Gender               Age            Tenure      
    ##  Length:10000       Length:10000       Min.   :18.00   Min.   : 0.000  
    ##  Class :character   Class :character   1st Qu.:32.00   1st Qu.: 3.000  
    ##  Mode  :character   Mode  :character   Median :37.00   Median : 5.000  
    ##                                        Mean   :38.92   Mean   : 5.013  
    ##                                        3rd Qu.:44.00   3rd Qu.: 7.000  
    ##                                        Max.   :92.00   Max.   :10.000  
    ##     Balance       NumOfProducts    HasCrCard      IsActiveMember  
    ##  Min.   :     0   Min.   :1.00   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:     0   1st Qu.:1.00   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median : 97199   Median :1.00   Median :1.0000   Median :1.0000  
    ##  Mean   : 76486   Mean   :1.53   Mean   :0.7055   Mean   :0.5151  
    ##  3rd Qu.:127644   3rd Qu.:2.00   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :250898   Max.   :4.00   Max.   :1.0000   Max.   :1.0000  
    ##  EstimatedSalary         Exited      
    ##  Min.   :    11.58   Min.   :0.0000  
    ##  1st Qu.: 51002.11   1st Qu.:0.0000  
    ##  Median :100193.91   Median :0.0000  
    ##  Mean   :100090.24   Mean   :0.2037  
    ##  3rd Qu.:149388.25   3rd Qu.:0.0000  
    ##  Max.   :199992.48   Max.   :1.0000

We can take a better statistical look at the data in R by seeing how
many from each age exited

\#Creating a table to show age and whether they stayed or exited

``` r
by_age_exit<- bank_data %>% 
  tabyl(Age, Exited) %>% 
  adorn_totals("row") 
```

\#Renaming the columns from 0 and 1 to Stayed and Exited \#And then
viewing a sample of the the table we will visualize using R

``` r
as_tibble(by_age_exit %>% rename(Exited = "1") %>% 
                rename(Stayed = "0"))
```

    ## # A tibble: 71 x 3
    ##    Age   Stayed Exited
    ##    <chr>  <dbl>  <dbl>
    ##  1 18        20      2
    ##  2 19        26      1
    ##  3 20        38      2
    ##  4 21        50      3
    ##  5 22        72     12
    ##  6 23        93      6
    ##  7 24       118     14
    ##  8 25       148      6
    ##  9 26       186     14
    ## 10 27       196     13
    ## # ... with 61 more rows

Using this table will do a quick analysis by age in R and then move onto
Tableau visualizations

\#Grouping the ages together. Young adult = 18-35, middle age = 36 -55,
older adult 56+

``` r
age_group <- by_age_exit %>% 
  summarise(
    age_group = factor(case_when(
      Age >= 18 & Age <= 35 ~
        "Young Adult",
      Age >= 36 & Age <= 55 ~
        "Middle Aged Adult",
      Age >= 56 ~
        "Older Adult"),
       levels=c("Young Adult", "Middle Aged Adult", "Older Adult")),
      .group = Age)
```

\#Creating a distribution table of the age groups we just created using
ggplot

``` r
 age_group %>% 
  group_by(age_group) %>% 
  summarise(total=n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(age_group) %>% 
  summarise(Total_Percent = total/totals) %>% 
  ggplot(aes(age_group, y = Total_Percent, fill = age_group)) +
    geom_col()+
           scale_y_continuous(labels = scales::percent) +
           #theme(legend.position="none") +
           labs(title = "Distribution of Those That Exited by Age", x=NULL) +
           theme(legend.position="none", text=element_text(size=13), plot.title =
                   element_text(hjust = 0.5))
```
![](https://github.com/atomlafave/images/blob/main/distro_by_age.PNG)

Lets take a look at a few Tableau visualizations breaking the data down.

First, we’ll map where the customers are from by region.

![](https://github.com/atomlafave/images/blob/main/bank_cust_region.PNG)

We can see that most of the customer’s are from France. Germany and
Spain coincidentally have the same amount of customer’s from this sample.

![](AB_Testing_Using_Tableau_Churn_Rate_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Findings: We see that older adults above age 55 are more likely to leave
the bank than those between the ages of 18-55.

Now we can do AB testing. The dependent variable that we are examining
is whether the person left the bank or not. We’ll first look at gender.

![](https://github.com/atomlafave/images/blob/main/Retention_Gender.PNG)

Irrespective the number of female and male customers, the percentage of
male customers that left the bank is 16%, and that is is less than the
25% of female customers that left the bank. The conclusion being that
female customers are more likely to leave the bank than male customer’s
with all other variables held equal. Noting that this is not a full
statistical A/B test because we did not check the statistical
significance.

Now we’ll use Tableau to show a total percentage of those that stayed
vs. those that exited.

![](https://github.com/atomlafave/images/blob/main/Retention_Total.PNG)

We see out of the total 10,000 bank customers in the sample, 20% of them
exited in the last 6 months.

Now that we know 20% is our average exit rate - we will re-visualize
gender bar chart with a reference line to see how they match up to the
overall exit average.

![](https://github.com/atomlafave/images/blob/main/Reference_Line.PNG)

We can see that female customer’s are more likely to exit than
customer’s on average overall and male customer’s are less likely.

With Tableau we can easily duplicate sheets. Now we will go beyond A/B
testing and checkout the 3 regions and observe their customer retention
trends.

![](https://github.com/atomlafave/images/blob/main/Retention_Geo.PNG)

We see that over 32% of customers from Germany have left the bank over
the last 6 months during the period of the observation. France and Spain
are below average.

Checking out the bank’s retention of customers with or without credit
cards.

![](https://github.com/atomlafave/images/blob/main/CCard.PNG)

We see there is not much of a difference in retention rate as to whether
or not the customer had a credit card or not. Both credit card and
non-credit card holders exited at about the overall average.

Now we will look at the retention rate of active or inactive members.

![](https://github.com/atomlafave/images/blob/main/mbr_activity.PNG)

We see here that 27% of inactive members left the company over the last
6 months and 14% of active members left. One assumption here is that
people who are actively invested in the bank do not want to leave. How
can the bank offer more competitive products to turn inactive members
into active members?

Next we will checkout to see the retention rate based on the number of
products that each bank member has.

![](https://github.com/atomlafave/images/blob/main/number_of_products.PNG)

We have some quite shocking results here. It shows that those with the
most number of products exited the bank way above average. Which seems a
bit unusual we we would expect those with the most number of products
would stay with the bank. Every single customer with 4 products left the
bank in the last six months and 83% of customers with 3 products left.
The lowest exit rate were those customers with 2 products which was way
below the 20% average at 8%. Customers with just one product left above
average at 28%. It seems that 2 products is the sweet spot to retain
customers. Further investigation as to why this may be is necessary.

Due to this anomaly, we will investigate further to check the actual
number of people in each category for the number of products they have
to see how big the sample size was for each.

![](https://github.com/atomlafave/images/blob/main/Num_Prod_With_Sample.PNG)

Indeed, we see the sample size for those with 4 products was only 60 and
those with 3 products was 266 out of the 10,000 customers. For 3 or 4
products we would need to investigate this further and can’t make the
assumption these rates will continue as we observed from this sample
size.

Lastly, we are going to run a data validity test on the data. We do so
by creating a unique field and grouping all of the 10,000 customers into
categories by the last digit of their customer ID(which should generally
be uniformly and randomly distributed) from the ’Customer Id field. In
Tableau this is simple to do with a calculations field. Then we graph it
as we have been to make sure there are no irregularities.

![](https://github.com/atomlafave/images/blob/main/Validity.PNG)

Everything looks good from the validity test.

Thanks for checking out my quick analysis using R and Tableau!
