Fitting equations
===================
date: James Scott (UT-Austin) 
autosize: true
font-family: 'Gill Sans'
transition: none


<style>
.small-code pre code {
  font-size: 1em;
}
</style>




Reference: "Data Science" Chapter 2.



Equations from data
========

So far we've concentrated on relatively simple visual and numerical summaries of data sets.

In many cases we will want to go further, by fitting an explicit equation, called a _regression model_ that describes how one variable ($y$) changes as a function of some other variables ($x$).  

This is process is called _regression_ or _curve fitting_: estimating the conditional expected value for $y$, given $x$.  


Equations from data
========

For example, you may have heard the following rule of thumb: to calculate your maximum heart rate, [subtract your age from 220](https://www.active.com/fitness/calculators/heartrate).

We can express this rule as an equation:  

$$
\mbox{MHR} = 220 - \mbox{Age}  
$$


Equations from data
========

This equation comes from _data_.  The study probably went something like this:    
- recruit a bunch of people of varying ages 
- give them heart rate monitors  
- tell them to run so hard on a treadmill that they feel like throwing up      
- record their maximum heart rate  

Data from this kind of study looks like this... 


Equations from data
========

<img src="fig/08_heart_rate.jpeg" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="700px" style="display: block; margin: auto;" />

It turns out that Equation 2 (MHR = 208 - 0.7 $\times$ Age) is a better equation: it makes smaller errors, on average.  



Equations from data
========

Top three reasons for fitting an equation: 
- to make a prediction  
- to summarize the trend in a data set  
- to make fair comparisons that adjust for the systematic effect of some important variable.  (This is kind of like handicapping in golf.)    

Our heart rate example illustrates all three of these concepts.


Making a prediction
========

Alice is 28.  What is her predicted max heart rate?

Our equation expresses the _conditional expected value_ of MHR, given a known value of age:  

$$
\mbox{E(MHR | Age)} = 208 - 0.7 \cdot 28 = 188.4  
$$

This is our best guess without actually putting Alice on a treadmill test until she vomits.  


Summarizing a trend  
========

How does max heart rate tend to change with age?   

$$
\mbox{E(MHR | Age)}  = 208 - 0.7 \cdot \mbox{Age}
$$

So about 0.7 BPM slower, on average, with every additional year we age. 

This isn't a guarantee that _your_ MHR will decline at this rate; it's just a population-level average.  


Making fair comparisons
========

A third, very common use of regression modeling is to make _fair comparisons_ that adjust for the systematic effect of some common variable.

It's kind of like asking: [how big of a head start should The Freeze get?](https://www.youtube.com/watch?v=7asw5Vd8lIY)    

<img src="fig/thefreeze.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="600px" style="display: block; margin: auto;" />


Making fair comparisons
========

<img src="fig/thefreeze_bolt.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="900px" style="display: block; margin: auto;" />


Making fair comparisons
========

This is not a fair race!  
- The Freeze is a former college track star who missed the U.S. Olympic team by 0.02 seconds.  
- The other guy is a random fan in a Braves t-shirt.  

To the make the race "fair" (and thus interesting), we need to adjust for how fast we _expect_ these guys to run, given what we know about them.

__Key fact: regression models are great at estimating conditional expectations.__


Making fair comparisons
========

Let's compare two people whose max heart rates are measured using an actual treadmill test:  
- Alice is 28 with a maximum heart rate of 185.  
- Abigail is 55 with a maximum heart rate of 174.  

Clearly Alice has a higher MHR, but let's make things fair!  We need to give Abigail a "head start," since max heart rate declines with age.  

So who has a higher maximum heart rate _for her age_?  


Making fair comparisons
========

Key idea: compare actual MHR with expected MHR.

Alice's actual MHR is 185, versus an expected MHR of 188.4 

$$
\begin{aligned}
\mbox{Actual} - \mbox{Predicted} &= 185 - (208 - 0.7 \cdot 28) \\
&= 185 - 188.4 \\
& = -3.4
\end{aligned}
$$


Making fair comparisons
========

Key idea: compare actual MHR with expected MHR.  

Abigail's actual MHR is 174, versus an expected MHR of 169.5 

$$
\begin{aligned}
\mbox{Actual} - \mbox{Predicted} &= 174 - (208 - 0.7 \cdot 55) \\
&= 174 - 169.5 \\
& = 4.5
\end{aligned}
$$



Making fair comparisons
========

So Abigail has a lower absolute MHR, but a higher _age-adjusted_ MHR.  Her "head start" was the difference between her and Alice's expected MHRs: 188.4 - 169.5 = 18.9.  

The equation that relates MHR to age shows us how to place everyone on a level playing field, regardless of age.  There are a lot of synonyms for this idea:  
- adjusting for $x$  
- statistically controlling for $x$  
- holding $x$ constant  

It's all just subtraction! __Compare the difference between actual and expected outcomes.__



Fitting straight lines  
=====

The workhorse here is a _linear model_:  

$$
y_i = \beta_0 + \beta_1 x_i + e_i
$$

Notation:  
- $i$ indexes each data point  
- $y_i$ is the response and $x_i$ is the predictor (feature) for data point $i$  
- $\beta_0$ and $\beta_1$ are the _parameters_ of the model.    
- $e_i$ is the _model error_ or _residual_, where $E(e_i) = 0$.  
- The conditional expected value of $y$ is  

$$
\hat{y} = E(y \mid x) = \beta_0 + \beta_1 x  
$$



Fitting straight lines
=====

"Fitting a model" = choosing $\beta_0$ and $\beta_1$ to make the model errors as small as possible on your data set.    In practice "as small as possible" means "least squares."  Define the loss function 

$$
\begin{aligned}
l(\beta_0, \beta_1) &= \sum_{i=1}^N e_i^2 \\
&= \sum_{i=1}^N \left[ y_i - (\beta_0 + \beta_1 x_i) \right]^2
\end{aligned}
$$

__Ordinary least squares__ (OLS): choose $\beta_0$ and $\beta_1$ to make $l(\beta_0, \beta_1)$ as small as possible, i.e. to minimize the sum of squared model errors.  


Fitting straight lines
=====

It's a straightforward calculus problem to show that the OLS solution is:

$$
\begin{aligned}
\hat{\beta}_1 &= \frac{ \sum_{i=1}^n (x_i - \bar{x}) (y_i - \bar{y}) } {\sum_{i=1}^n (x_i - \bar{x})^2 } \\ \nonumber \\
\hat{\beta}_0 &= \bar{y} - \hat{\beta}_1 \bar{x} \,  ,
\end{aligned}
$$

Don't bother memorizing this!  Every statistical package on the planet has it built in.  (Plus, they'll probably make you derive it in Econometrics.)


Fitting straight lines
=====

Instead, let's focus on using the fitted equation for our three goals:  
- making a prediction  
- summarizing the trend in the data  
- statistical adjustment: making fair comparisons that adjust for some systematic effect  



Example: Austin food critics 
=====
class: small-code

Here are a few lines from a data set on Austin restaurants c.2013:


```
                 Name            Type FoodScore FeelScore Price
    Franklin Barbecue        Barbecue       9.5       5.5    15
     Kerbey Lane Cafe      Vegefusion       6.5       8.5    20
   Shoal Creek Saloon        Southern       4.6       8.5    25
                 Uchi Japanese,Modern       9.8       8.5    85
 Second Bar + Kitchen          Modern       8.7       9.0    50
             Lamberts    Southwestern       8.5       9.0    75
```

- Price = average price of dinner and drinks for one
- FoodScore = critics' rating out of 10


Example: Austin food critics 
=====

Our fitted line (from OLS):  

<img src="fig/fearlesscritic1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="660px" style="display: block; margin: auto;" />

$$
\mbox{Price} = -6.2 + 7.9 \cdot \mbox{FoodScore} + \mbox{Error}
$$


Making a prediction
=====

Suppose you're opening a new restaurant and you've hired a chef with a proven track record of cooking at a 7.5 level.  

What price would you expect the Austin market to support for an average meal at your restaurant?  


Making a prediction: the algebra
=====

We know that $x = 7.5$.  Remember that our fitted equation expresses the _conditional expected value_ of $y$, for a known value of $x$.  

So in light of our data on the Austin market, our best guess for price is

$$
E(y \mid x = 7.5) = -6.2 + 7.9 \cdot 7.5 = 53.05
$$

Maybe a sensible starting point for thinking about pricing.  


Making a prediction: the geometry
=====

<img src="fig/fearlesscritic3.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="860px" style="display: block; margin: auto;" />


Summarizing a trend
=====

What dollar value does the Austin restaurant market seem to place on one extra point of food deliciousness?  

Recall our equation

$$
E(y \mid x) = -6.2 + 7.9 \cdot x
$$

That's \$7.90 per point of deliciousness, on average.  


Statistical adjustment
=====

What's the "best value" restaurant in Austin, i.e. the one that offers the most delicious food _for its price_?

Recall our key idea: compare actual with predicted.  


Statistical adjustment
=====

<img src="fig/fearlesscritic2.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="860px" style="display: block; margin: auto;" />

No surprises here: it's Franklin Barbecue!  Actual price is \$15 per person; predicted price is nearly \$70.  



To the code!
=====

Let's dig in to `afc_intro.R` and `afc.csv` on the class website.  


Your turn
=====
type: prompt

Download the data in `creatinine.csv` from the course website.  Each row is a patient in a doctor's office. 
- age: patient's age in years.  
- creatclear: patient's creatine clearance rate in mL/minute, a measure of kidney health (higher is better).  

Load this data into RStudio and start with a blank script.   

Your turn
=====
type: prompt

Use this data, coupled with your knowledge of linear modeling, to answer three questions:  
  1. What creatinine clearance rate should we expect, on average, for a 55-year-old?  
  2. How does creatinine clearance rate change with age?  
  3. Whose creatinine clearance rate is healthier (higher) for their age: a 40-year-old with a rate of 135, or a 60-year-old with a rate of 112?  


Beyond straight lines
========

Ordinary least squares is built for linear models.  However, we can also use OLS to fit certain kinds of _nonlinear_ models.  We'll focus on four:  
- polynomial models    
- piecewise polynomial models (_splines_)
- exponential growth and decay  
- power laws  

These models are special!  (Most nonlinear models cannot be fit using _ordinary_ least squares.)  


Polynomial models
========

Data on gas consumption versus temperature for a single-family house in Minnesota:  

<img src="fig/gas_consumption.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="860px" style="display: block; margin: auto;" />

The linear model doesn't fit so well!  


Polynomial models
========

But a quadratic model does!  

<img src="fig/gas_quadratic_fit.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="600px" style="display: block; margin: auto;" />

$$
\mbox{Gas Bill} = \$289 - 6.4 \cdot \mbox{Temp} + 0.03 \cdot \mbox{Temp}^2 + \mbox{Residual} \, .
$$


Polynomial models
========

In general, a polynomial model of degree $K$ takes the form

$$
E(y \mid x) = \beta_0 + \sum_{j = 1}^K \beta_j x^j  
$$

This model is nonlinear in $x$, but it can still be fit using OLS.


Polynomial models
========

There is a temptation to get a better fit by choosing a larger $K$.  This can get ridiculous:  

<img src="fig/gas_fitoverfit.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" width="900px" style="display: block; margin: auto;" />


Polynomial models: over-fitting
========

For most data sets, beyond $K=2$ (quadratic) or $K=3$ (cubic), we rapidly get into dangerous _over-fitting_ territory:

Over-fitting occurs when a model just memorizes random noise in the data set, rather than describes the systematic relationship between $x$ and $y$.  

Severely overfit models usually have one or two dead giveaways:  
- non-intuitively wiggly interpolation behavior    
- crazy extrapolation behavior!


Polynomial models: over-fitting
========


<img src="fig/gas_extrapolate_poly.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" width="800px" style="display: block; margin: auto;" />

In later courses, you'll learn formal diagnostics for over-fitting.  In the meantime: you'll pretty much know it when you see it. 


Let's dive in to `utilities.csv` and `utilities.R`.  



Polynomial models: splines
========

Another nice extension: piecewise-polynomial models, also known as splines.  

To fit a spline, we divide the range of the $x$ variable into nonoverlapping interals $I_0, I_1, I_2, \cdots, I_K$.  The breakpoints between the intervals are called _knots._  

We then fit a polynomial equation separately on each interval, "gluing" the individual polynomials together so that the overall curve is smooth.  


Polynomial models: splines
========

Here's some data on finishing times from runners in the 10-mile Cherry Blossom Road Race in Washington, D.C., held every April:  


```
  state time  net age sex
1    MD 5100 5032  58   M
2    PA 4675 4400  26   M
3    DC 6564 6564  25   M
4    NJ 5134 5031  35   M
5    NY 5669 5187  37   F
6    MD 4478 4461  23   F
7    VA 7611 7259  34   M
8    VA 6075 5699  25   M
```


Polynomial models: splines
========

<img src="06_fitting_equations-figure/unnamed-chunk-13-1.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto;" />

Three knots create four disjoint intervals (knots at the 25th, 50th, and 75th percentiles of temperature).  



Polynomial models: splines
========

<img src="06_fitting_equations-figure/unnamed-chunk-14-1.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" style="display: block; margin: auto;" />

Separate polynomials on each interval, glued together in a smooth fashion.  __What might explain the non-monotone behavior?__  (Code in `race_splines.R`.)  



Polynomial models: splines
========

There are probably at least two things going on here:  
- actual non-monotonicity: runners reach their physical peak fairly late compared to other athletes (sometime in their 30s or 40s).  
- survivorship bias: only the more serious runners are still running in their late 30s and through their 40s.  

Bottom line: the flexible piece-wise polynomial (spline) model allowed us to __practice good data science__.  We could:  
- see these subtle effects in the data.  
- form theories about what might be causing them.  


Exponential growth and decay  
========

An exponential growth or decay model looks like this:  

$$
y = \alpha e^{\beta_1 x}  
$$

where:
- $\alpha$ is a baseline level of the response $y$ at $x=0$.  
- $\beta_1$ describes the rate of growth ($+$) or decay ($-$) for a one-unit change in $x$.    


Exponential growth and decay  
========

This formula comes from an analogy with continuously compounded interest.  Suppose you start with $\alpha$ dollars, invested at rate $\beta_1$ and compounded $n$ times annually.  Then after $t$ years, your investment is worth

$$
y = \alpha \left(1 + \frac{\beta_1}{n} \right)^{nt}  
$$

If we take the limit as $n$ gets large, we get

$$
y = \alpha e^{\beta_1 t}  
$$


Exponential growth and decay  
========

This is a non-linear model, but it's easy to fit using OLS!

Here's the trick: if $y = \alpha e^{\beta_1 x}$, then

$$
\begin{aligned}
\log(y) &= \log \left( \alpha e^{\beta_1 x} \right) \\
 &= \log \alpha + \beta_1 x  
\end{aligned}
$$

This is a linear function after all: not in $y$ versus $x$, but in $\log(y)$ versus $x$.  



Exponential growth and decay  
========

This gives us a simple recipe: Fit a linear regression model where the $y$ variable has been log-transformed:  

$$
\log(y_i) = \beta_0 + \beta_1 x_i + e_i
$$

This tells us the parameters of our exponential growth or decay model:  
- $\beta_0 = \log \alpha$, so $\alpha = e^{\beta_0}$ is the initial level of $y$ at $x=0$.  
- $\beta_1$ is the growth rate per unit change in $x$.  

__See example in `ebola.R`.__  



Power laws
========

A similar trick works for power laws, where

$$
y = \alpha x^{\beta}  
$$

This is a very common model in microeconomics, where we often use power laws to model change in consumer demand as a function of price:  

$$
Q = K P^E
$$

where $Q$ is quantity demanded, $P$ is price, $E$ is [price elasticity of demand](https://en.wikipedia.org/wiki/Price_elasticity_of_demand#Optimal_pricing) (PED), and $K$ is a constant.  



Exponential growth and decay  
========

We can git a model like this using OLS as well.  

Here's the trick: if $y = \alpha x^{\beta_1}$, then

$$
\begin{aligned}
\log(y) &= \log \left( \alpha x^{\beta_1} \right) \\
 &= \log \alpha + \beta_1 \log(x)  
\end{aligned}
$$

This is also linear function, in $\log(y)$ versus $\log(x)$.  



Exponential growth and decay  
========

Now we fit a linear regression model with _both_ variables log-transformed:  

$$
\log(y_i) = \beta_0 + \beta_1 \log(x_i) + e_i
$$

This tells us the parameters of our exponential growth or decay model:  
- $\beta_0 = \log \alpha$, so $\alpha = e^{\beta_0}$ is our leading constant.  
- $\beta_1$ is the elasticity: if $x$ changes by 1%, then y changes by $\beta_1$%.    

[See example on class website.](https://github.com/jgscott/learnR/blob/master/infmort/infmort.md)  



Power laws
========
type: prompt

Your turn!   In `milk.csv`, you're given a data set on consumer demand for milk, along with price.  Your ultimate goal is to answer the question: how much should the store charge for a carton of milk?  

Let's dive in to the [case study on milk prices on the class website.](https://github.com/jgscott/learnR/blob/master/cases/milk/milk.md)
 
 Remember: demand versus price often follows a power law!  
 
