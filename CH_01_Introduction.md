---
title: "Introduction"
output: 
  html_document: 
    keep_md: yes
---



# Meta analysis in R.


Meta analysis is a technique that allows us to quantitatively combine research knowledge, improve the precision of our parameter estimates and discover insights that would not be revealed from considering individual studies in isolation.

People can often balk at the mention of the words "meta-analysis". The words conjure up images of large groups of expert statisticians working long hours on intimidatingly large datasets.

In my view, these are significant and harmful misconceptions. Recently, R and in particular, the 'metafor' package has made conducting meta analysis incredibly straightforward. Moreover, the idea that meta-analyis is a tool that should be restricted to use on large scale datasets is incorrect. Even combining the results of just 2 studies can yield valuable increases in precision.

## Meta analysis - 3 examples.

The power of meta analysis is best illustrated with some examples.

### Example 1: Lucky and Unlucky


Lets suppose that we have two studies that are replications of one another. The first study, published by Lucky et al (2012) in Current Biology revealed a large, significant effect (N = 26, p=.0001). The second study, published by Unlucky et al (2015) failed to detect a statistically significant effect (N= 28, p=.12). Since Unlucky failed to replicate Luckys finidng with a larger sample, they questioned the existence of the effect. 

These results appear to be somewhat in conflict with one another. The first study robustly indicated that we should reject the null hypothesis, whereas the latest evidence suggests that we should accept the null. Intuitively, we know that we should have more faith on the outcome of the study with the larger sample, so maybe Unlucky's conclusion should be the one that we should take to the bank.

What happens if we perform a meta analysis on the data?

The code below will define some utility functions, perform a meta analysis on the two studies and output a forest plot. We don't need to understand what all this means just yet, since we will be repeating these steps in more detail in later sections. 


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
# Function for converting from p value and N to Cohen's d and 95% CI's
ptoDr=function(p,N,r)
{T=qt(p/2,N-1)
D=T/sqrt(N)
Da=D*sqrt(2*(1-r))
Da2=sqrt(Da^2)
SE=sqrt(1/N+D^2/(2*N))*sqrt(2*(1-r))
CIp=Da2+(SE*1.96)
CIn=Da2-(SE*1.96)
DSE=c(Da2,SE,CIn,CIp)}

# Function for outputing information from rma object
tidy=function(obj)
{sprintf(c("The pooled effect size is %f","The lower limit of the 95 percent confidence interval is %f","The upper limit of the 95 percent confidence interval is %f","The pooled p value is %f"),c(obj$beta,obj$ci.lb,obj$ci.ub,obj$pval))}
  
  
# Convert the p and N values from the Lucky and Unlucky studies to Cohens d and 95% CIs
Lucky=ptoDr(.0001,26,0.5)
Unlucky=ptoDr(.12,28,0.5)

# Add both studies to a dataframe.
DATA=data.frame(rbind(Lucky,Unlucky))
colnames(DATA)=c("D","SE")

# Estimate random effects model.
META=rma(yi=D,sei=SE,data=DATA)
tidy(META)
```

```
## [1] "The pooled effect size is 0.590644"                               
## [2] "The lower limit of the 95 percent confidence interval is 0.000882"
## [3] "The upper limit of the 95 percent confidence interval is 1.180405"
## [4] "The pooled p value is 0.049658"
```


```r
# Forest plot of the data.
forest(META,slab=c("Lucky (2012)","Unlucky (2015)"))
```

![](CH_01_Introduction_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


The above plot is known as a forest plot. It depicts the effects sizes and confidence intervals for the two individual studies and the meta analytic combination of these effects is depicted by a diamond.

Here, we see that despite the fact that Lucky and Unlucky rejected and retained the null hypothesis respectively, both effects are in the same direction and the overall effect is significant. Perhaps there was not much of a conflict after all.

This illustrates one of the main advantages of meta analysis - if we had considered the results of the individual studies alone, we would be left with a seemingly incoherent picture. Meta analysis allows us to "see the wood for the trees". The findings only appear inconsistent to the extent that they are treated as individual entities. This isn't how science should work. We should apply techniques to quantitatively combine research findings. This is the job of meta analysis. 


### Example 2: A miraculous replication


In 2001, a group of researchers found that a new treatment was effective at reducing depression. 

Unfortunately the authors only had access to a small sample and so the confidence interval around their effect size estimate was very large.

In 2008 they managed to perform a replication study with an independent sample of participants. Miraculously, they found that the effect was identical in magnitude! In 2011, they replicated the study and again found exactly the same effect!


```r
Study2001=ptoDr(.049,26,0.5)
Study2008=ptoDr(.049,26,0.5)
Study2011=ptoDr(.049,26,0.5)


DATA2=data.frame(rbind(Study2001,Study2008,Study2011))
colnames(DATA2)=c("D","SE")

META2=rma(yi=D,sei=SE,data=DATA2)

tidy(META2)
```

```
## [1] "The pooled effect size is 0.405830"                               
## [2] "The lower limit of the 95 percent confidence interval is 0.174951"
## [3] "The upper limit of the 95 percent confidence interval is 0.636709"
## [4] "The pooled p value is 0.000571"
```


```r
# Forest plot of the data.
forest(cumul(META2))
```

![](CH_01_Introduction_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The above plot is again a forest plot, but this time it shows the effect of fitting a meta-analytic model after each successive study is published. 

We see that from study 1 to study 3, our combined confidence interval width has reduced from 0.8 to 0.47. This is an incredibly valuable increase in precision. Moreover, the pooled p value has reduced substantially from 0.49 to .0006. 

The take home message here is that meta analysis greatly improves the precision of our parameter estimates. The example also illustrates that several studies indicating weak evidence for an effect, when combined, can indicate very strong evidence of an effect
