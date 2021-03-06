---
title: "Gathering data"
output:
  html_document: 
    keep_md: yes
---
![](https://images2.imgbox.com/24/71/0KH49y9V_o.png "Title")


## Creating a dataset.

In this chapter, we will focus on the nuts and bolts of obtaining effect sizes for meta analyses. This won't focus so much on formulas or calculations, but the practical issues associated with estimating effect sizes from the information we have available. Anyone can follow a formula, but a formula alone is just a mechanical rule that doesn't tell you anything about the context in which it should be applied. Unfortunately, empirical papers can vary wildly in terms of the information that they report and so a number of strategies can be required to estimate an effect size. In my opinion, meta analysis/effect size textbooks have too much emphasis on formulas and not enough on the difficulties associated with trying to extract useful information from empirical papers.

## Cohen's *d*

This chapter will focus soley on Cohen's *d*, since it is by far the most commonly used effect size. In principle though, the lessons learned will be the same regardless of the effect size we wish to compute.

Cohen's *d* actually can actually be defined in a number of ways, but the Generic formula is:

(*M1* - *M2*)/ *SD*

Cohens *d* therefore represents the difference between two means in standard deviation units. You can find an interactive visualisation of Cohen's *d* and its relationship with other metrics [here](http://rpsychologist.com/d3/cohend/).

## Standardisers for *d*

The denominator of the equation (*SD*) as I have presented it above is ambiguous - a standard deviation is referred to - but there are actually a number of possible standard deviations I could be refering to. We could use:

1. The *SD* of Group/Condition 1
2. The *SD* of Group/Condition 2
3. The *SD* of the difference between Group/Condition 1 and Group/Condition 2.
4. The pooled SD of Group/Condition 1 and Group/Condition 2.

In fact, these are all perfectly legitimate options. The consequences of using each standardiser can be a complex issue and this is all described excellently in [this](https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full) paper by Lakens (2013).

In practice, all we really need to ensure is that we:

1. Compute the mean difference standardised by some standard deviation.
2. Are clear about which standardiser we used.
3. Are consistent in using the same standardiser for each effect in our analysis.

Therefore *clarity* and *consistency* are much more important than the specific choice of standardiser itself.

If we are *consistent*, then all of our effect sizes will be comparable to one another.
If we are *clear* then this will allow researchers to interpret our results accurately and make any conversions to other forms of Cohen's *d*, if they want to.

It **will not do** to simply report that you 'calculated Cohen's *d*', since, as we have seen, *d* can be defined and operationalized in a number of different ways.

***

## Cohen's *dz*

In the repeated measures design, it is often desirable to use the *standard deviation of the difference scores* (option 3 described above) as the standardiser, creating a statistic often referred to as 'Cohens *dz*'. This is because the information most reliably reported in experimental papers tends to be a *t*, or *p* statistic - and *dz* can be easily calculated from just the *t/p* and *N*. By contrast, you are less likely to find tables of means and standard deviations in every paper - which makes options 1, 2 and 4 less reliable. Of course, you may want to root through your candidate set of papers and base your choice of standardiser on what information is most frequently reported. 

***

## Scenario 1: *dz* from *t* or *p* value and *N*. 

In this first scenario, we will be computing *dz* from the *p* or *t* value and *N*. This is the best case scenario, where everything we need is reported.  

First, let's make up some example repeated measures data.


```r
# Import metafor library
library(metafor)
```

```
## Warning: package 'metafor' was built under R version 3.2.5
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

In a published paper, this would typically be written as:


*"A paired t-test detected significantly higher scores in condition 1 than condition 2 (t(19) = 3.54, p=.002)".*


Now let's define some functions for computing Cohen's *dz*, based on the *t* value and *p* value. We will also define a function that computes dz based on the mean differences and SD of the difference scores so you can convince yourself that all methods return the same results.

Each function returns [1] dz, [2] the standard error of dz, [3] the lower bound of the confidence interval and [4] the upper bound of the confidence interval.


```r
# Method 1: Define a function for converting from p value and N to Cohen's dz and 95% CI's
ptoDr=function(p,N){
T=qt(p/2,N-1)
D=T/sqrt(N)
Da=D*sqrt(2*(1-0.5))
Da2=sqrt(Da^2)
SE=sqrt(1/N+D^2/(2*N))*sqrt(2*(1-0.5))
CIp=Da2+(SE*1.96)
CIn=Da2-(SE*1.96)
DSE=c(Da2,SE,CIn,CIp)}

# Method 2: Define a function for converting from t value and N to Cohen's dz and 95% CI's
TtoDr=function(t,N)
{D=t/sqrt(N)
 Dp=sqrt(D^2)
 Dp2=Dp*sqrt(2*(1-0.5))
 SE=sqrt((1/N)+(Dp^2)/(2*N))*sqrt(2*(1-0.5))
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

result1=ptoDr(test$p.value,20)
result2=TtoDr(as.numeric(test$statistic),20)
result3=CtoD(Condition1,Condition2)

result1
```

```
## [1] 0.7931954 0.2563766 0.2906972 1.2956936
```

```r
result2
```

```
## [1] 0.7931954 0.2563766 0.2906972 1.2956936
```

```r
result3
```

```
## [1] 0.7931954 0.2563766 0.2906972 1.2956936
```

Great, now lets define this as our first effect size to be included in our model.


```r
Effect1=result1
```

***

## Scenario 2: No exact p value is reported.

Scenario 1 described a case where we had all the information we needed to compute an effect size. Unfortunately, this is tends to be the only scenario that is covered by most textbooks. In reality, even in our most optimistic expectations we are going to have a few cases where some of the information we need is missing. 

In this next scenario for instance, we are faced with a very common problem: The author has reported that an effect was 'significant', but has not reported the exact *p* value. For instance: 

*"A paired t-test detected significantly higher scores in condition 1 than condition N = 50 p < .050".*

Strangely, I don't think that any formal convention for how to approach this scenario exists, but I think that the best strategy is to be conservative. Of course, the most conservative possible interpretation of '*p* < .050' is that *p* = .049 and so this should form the basis of our effect size calculation.



```r
result2=ptoDr(.049,20)
```
***

## Scenario 3: A 'non significant' effect

Despite best practices, it is quite common for authors to report that an effect was 'not significant' without providing any further information. As a result, you may encounter an unhelpful sentence like this:

*"There were higher scores in condition 1 than condition 2, but this did not reach significance"*

Here, all we really have to go on is the direction of the effect and the fact that *p* must be greater than .05. In cases such as this, Cooper and Hedges (1994) suggest that assuming *p* =.50 is the best strategy, to allow 'a representative sample of outcomes'. Again, this is a very conservative approach. 


```r
result3=ptoDr(0.5,20)
```
***

## Scenario 4: When to give up

Unfortunately, the situation can sometimes get even worse and an author can instead report something like this:

*"No difference between condition 1 and condition 2 was observed"*

In this case, we *don't even know the direction of the effect* and so here it is best to exclude the effect from your analysis altogether.

***

## Other tips

### Asking for data.

In scenarios 2-4, we have described cases where we are *estimating* an effect size, because we don't have all the information we need to derive the closed-form solution to our problem. Of course, in these cases it is preferable to contact the author to ask for the raw data directly. Some tips on this point:

1. *Be specific* - Be very clear about what data you are asking for. Refer directly to the relevant pages of the paper and/or figures. Without being specific, you run the risk of recieving a large spreadsheet containing all the experimental data. This will then require further clarification and the dialogue will continue for much longer than it needs to.

2. *Be clear about your intentions* - State who you are, where you are from, that you are collecting data for a meta analysis and what your outcome measure is.

3. *Be polite* - Academics lead busy lives. It's not always straightforward to retrieve specific data and send it in a comprehensible format, particularly if it was collected several years ago. You are asking for someone you may not know to do you a favour - and so your email should be written accordingly.

4. *Try again* - If you are unsuccessful the first time you ask for data, wait a suitable period of time (2 weeks) and then send a reminder. Following two unsuccessful attempts, it is probably time to move on.  

### Should I ask for unpublished data?

I'm a little skeptical about this myself, but I am not going to give a 'yes' or 'no' answer to this complex issue. Here are couple of things to consider at least.

1. *Who do you ask?* - Often in meta-analyses, it is reported that 'established authors' were contacted for unpublished data. There is some inherent subjectivity involved here in what constitutes an 'established author'. All this really amounts to is biasing the sample of effect sizes towards particular labs with strong publication records.

2. *How do you assess the quality of unpublished data?* - There may well be a very good reason why the data were never published. With unpublished data we don't have the benefit that the data were peer reviewed - so there is difficulty with quality control. Moreover, in many cases we probably don't have a manuscript to accompany the data, so we often can't meaningfully assess the quality of the data/ experiment ourselves anyway. 


### Be open about your approaches.

Method sections in meta-analysis papers are often quite opaque. For each included study, readers are often left almost clueless about where the corresponding effect size came from and how it was calculated. For this reason, I am a big fan of including supplementary material that describes how each effect size estimate was obtained. [Here](https://www.researchgate.net/publication/296325547_Supplementary_material)  is an example, to show you what I mean (see supplementary material S7, S9 and S11).  


### Where can I find out how to compute effect sizes?

1. *R packages* - There are many R packages available. Some of my favourites are [MBESS](https://cran.r-project.org/web/packages/MBESS/MBESS.pdf) and [compute.es](https://cran.r-project.org/web/packages/compute.es/compute.es.pdf). These contain a number of methods for computing a range of different effect sizes from a range of different inputs.

2. *Papers* - For Cohen's *d*, I highly recommend the paper by [Lakens et al](https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full) that I mentioned earlier on. For an overview of many different effect sizes, I suggest this [JEP General paper](https://www.ncbi.nlm.nih.gov/pubmed/21823805).

3. *Books* - Geoff Cumming's [excellent book](https://www.amazon.co.uk/Understanding-New-Statistics-Multivariate-Applications/dp/041587968X) is a must read.

**A WORD OF WARNING**

In general, clinical psychology and medicine have a much longer history of adopting estimation based approaches (effect sizes/ meta analysis). In these fields, the dominant experimental designs tend to be independent samples (i.e. the differences between a treatment group and control group). The result of all of this is that most textbooks/resources tend to focus almost entirely on independent samples designs with very little coverage of how to approach repeated measures designs - which are much more common in cognitive psychology.

This means that a generic method or formula described as for calculating *"Cohen's d from t value"* almost always means *"from an independent samples t value"*. Again, it doesn't particularly matter if you were to use this method for repeated measures *t* tests, so long as you were *clear* that this is what you did and are *consistent* in using equivalent methods for all effects in the analysis. However, it would present a problem if anyone tried to combine a method for computing from a independent samples design with one intended for a repeated measures design.  

Below for instance, we use a method from the compute.es package to compute *d* from the *t* value.


```r
library(compute.es)

d1=tes(test$statistic,20,20)
```

```
## Mean Differences ES: 
##  
##  d [ 95 %CI] = 1.12 [ 0.43 , 1.81 ] 
##   var(d) = 0.12 
##   p-value(d) = 0 
##   U3(d) = 86.9 % 
##   CLES(d) = 78.62 % 
##   Cliff's Delta = 0.57 
##  
##  g [ 95 %CI] = 1.1 [ 0.42 , 1.77 ] 
##   var(g) = 0.11 
##   p-value(g) = 0 
##   U3(g) = 86.42 % 
##   CLES(g) = 78.15 % 
##  
##  Correlation ES: 
##  
##  r [ 95 %CI] = 0.5 [ 0.21 , 0.71 ] 
##   var(r) = 0.01 
##   p-value(r) = 0 
##  
##  z [ 95 %CI] = 0.55 [ 0.21 , 0.88 ] 
##   var(z) = 0.03 
##   p-value(z) = 0 
##  
##  Odds Ratio ES: 
##  
##  OR [ 95 %CI] = 7.65 [ 2.19 , 26.68 ] 
##   p-value(OR) = 0 
##  
##  Log OR [ 95 %CI] = 2.03 [ 0.79 , 3.28 ] 
##   var(lOR) = 0.38 
##   p-value(Log OR) = 0 
##  
##  Other: 
##  
##  NNT = 2.44 
##  Total N = 40
```

```r
d1$d
```

```
## [1] 1.12
```

This is completely different estimate from that obtained via the means and standard deviations method we defined earlier on.


```r
d2=CtoD(Condition1,Condition2)
d2[1]
```

```
## [1] 0.7931954
```

The reason for this is that the first method uses the pooled standard deviation as the denominator (it is intended for independent samples designs) and the second uses the standard deviation of the difference scores as the denominator (it is intended for repeated measures designs). Again, this is all covered extensively in the [Lakens et al paper](https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full). This is why it is so important to read up on exactly what formula is being used. Before using two different methods, you should verify that they produce the same results on some dummy data.


### Think creatively

1. Are there no values reported in the text, but the data is represented in a figure? The wonderful [graphclick](http://www.arizona-software.ch/graphclick/) software allows you automatically retrieve data values from the image of a graph taken from a paper. This is particularly useful in cases where means and standard deviations are not reported in the text - you can extract them directly from the figure. 

2. Is there no *t* value reported in the paper because the author used a repeated measures ANOVA instead? If the numerator degrees of freedom is 1 (a test of differences between two conditions), then the corresponding *p* value from the *F* test is the same that would be obtained from a *t* test. This is one of the many scenarios where you have all the information you need, just in a different format.

***

## Summary

In this chapter, we walked through the issues involved with obtaining some effect sizes for a meta analysis. We focused on Cohen's *d* and on Cohen's *dz* in particular, but the principles will apply to whatever effect size or experimental design we are interested in. We have found that

1. It is important to be clear and consistent in how we compute an effect size.
2. Sometimes not all information is reported, but we can make some sensible, conservative assumptions to estimate the effect size given the available information.
3. There are a number of resources available to help us compute and interpret effect sizes.

![](https://images2.imgbox.com/24/71/0KH49y9V_o.png "Title")



