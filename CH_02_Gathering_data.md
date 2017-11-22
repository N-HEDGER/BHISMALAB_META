---
title: "Gathering data"
output:
  html_document: 
    keep_md: yes
---
![](https://images2.imgbox.com/24/71/0KH49y9V_o.png "Title")


## Creating a datset.



In this chapter, we will focus on the nuts and bolts of gathering data for meta analyses.  Empirical papers can vary wildly in terms of the information that they report and so a number of strategies can be needed to estimate an effect size.

This chapter will focus soley on Cohen's d, since it is by far the most commonly used effect size. In principle though, the steps we follow will be more or less the same regardless of the effect size we wish to compute.

Cohen's d actually can actually be defined in a number of ways, but the Generic formula is:

(Mean 1 - Mean 2)/ SD

Cohens d therefore represents the difference between two means in standard deviation units. The denominator of the equation is ambiguous - a standard deviation is referred to - but there are actually a number of possibilities here. We could use:

1. The SD of Group/Condition 1
2. The SD of Group/Condition 2
3. The SD of the difference between Group/Condition 1 and Group/Condition 2.
4. The pooled SD of Group/Condition 1 and Group/Condition 2.

In fact, these are all legitimate options. The consequences of using each standardiser can be a complex issue and this is described excellently in [This](https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full) paper by Lakens (2013). In reality, we only need to ensure that we:

1. Compute the mean difference standardised by some standard deviation.
2. Are clear about which standardiser we used.
3. Are consistent in using the same standardiser for each effect in our analysis.

Therefore *clarity* and *consistency* is key. It **will not do** to simply report that you 'calculated Cohen's d', since d can be defined and operationalized in a number of different ways.

In the repeated measures design, it is often desirable to use the *standard deviation of the difference scores* (option 3 described above) as the standardiser, creating a statistic often referred to as 'Cohens dz'. The reason for this is that the information most reliably reported in papers tends to be a t, or p statistic - and dz can be easily calculated from just the t/p and N. By contrast, you are less likely to find tables of means and standard deviations in every paper - this makes options 1, 2 and 4 less reliable. 

First, let's make up some example repeated measures data.

```r
# Import metafor library
library(metafor)
```

```
## Loading required package: Matrix
```

```
## Loading 'metafor' package (version 2.0-0). For an overview 
## and introduction to the package please type: help(metafor).
```

```r
# Create dummy data values for two conditions (repeated measures) 
set.seed(10)
Condition1=rnorm(20,6,5)
Condition2=rnorm(20,5,7)

# Compute t test to get t and p value
test=t.test(Condition1,Condition2,paired=TRUE)
```

The output of the t test object will give us information similar to that reported in an empirical paper.


```r
test
```

```
## 
## 	Paired t-test
## 
## data:  Condition1 and Condition2
## t = 3.5473, df = 19, p-value = 0.002152
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.452022 9.510131
## sample estimates:
## mean of the differences 
##                5.981076
```

In a published paper, this would typically be written as: "A paired t-test detected significantly larger values in condition 1 than condition 2 (t(19) = 3.54, p=.002"

Now lets define a function for computing Cohen's d, based on the t value and p value. We will also define a function that computes dz based on the mean differences and SD of the difference scores, so you can convince yourself that all methods return the same results. Each function returns [1] dz, [2] the standard error of dz, [3] the lower bound of the confidence interval and [4] the upper bound of the confidence interval.


```r
# Method 1: Define a function for converting from p value and N to Cohen's dz and 95% CI's
ptoDr=function(p,N,r){
T=qt(p/2,N-1)
D=T/sqrt(N)
Da=D*sqrt(2*(1-r))
Da2=sqrt(Da^2)
SE=sqrt(1/N+D^2/(2*N))*sqrt(2*(1-r))
CIp=Da2+(SE*1.96)
CIn=Da2-(SE*1.96)
DSE=c(Da2,SE,CIn,CIp)}

# Method 2: Define a function for converting from t value and N to Cohen's dz and 95% CI's
TtoDr=function(t,N,r)
{D=t/sqrt(N)
 Dp=sqrt(D^2)
 Dp2=Dp*sqrt(2*(1-r))
 SE=sqrt((1/N)+(Dp^2)/(2*N))*sqrt(2*(1-r))
 CIp=Dp2+(SE*1.96)
 CIn=Dp2-(SE*1.96)
 DSE=c(Dp2,SE,CIn,CIp)}

# Method 3: Define a function for converting from original condition data to Cohen's dz and 95% CI's
CtoD=function(G1,G2){
# Mean difference
Mdiff=mean(G1)-mean(G2)
N=length(G1)
# Standard deviation of the difference scores. 
SDdiff=sd(G1-G2)
D=Mdiff/SDdiff
SE=sqrt((1/N)+(D^2)/(2*N))
CIp=D+(SE*1.96)
CIn=D-(SE*1.96)
DSE=c(D,SE,CIn,CIp)}

result1=ptoDr(test$p.value,20,0.5)
result2=TtoDr(as.numeric(test$statistic),20,0.5)
result3=CtoD(Condition1,Condition2)
```





