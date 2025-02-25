# ECO 394D: Probability and Statistics

Quick links:
- [Course syllabus](./ref/ECOS394D_Summer2019_Syllabus.pdf)  
- [Materials and software](materials.md)    
- [Exercises](./exercises/)  

The team:  
- [James Scott](https://jgscott.github.io/), instructor.  Office hours MW 3:20 to 4:20, CBA 6.478.  
- [Molin Li](https://liberalarts.utexas.edu/economics/phd/profile.php?id=ml46274), teaching assistant.  Office hours Friday at 2PM, immediately after TA session.  


## What is this course about?

This class is about gaining knowledge from data and about understanding random phenomena.  We'll cover a mix of practice and principles:  
- In part 1, you'll learn the mathematical foundations of probability, the language for expressing judgments about uncertain outcomes. 
- In part 2, you'll learn some concrete data-crunching skills, using the [R language](https://www.rstudio.com/).  
- In part 3, you'll gain a solid technical understanding of some essential statistical ideas: estimation, hypothesis testing, and a few others.  

There's also an important intermediate-term goal: prep you for Econometrics this upcoming academic year.  


# Topics outline

## Part 1: Probability  

In this first part of the course, we'll learn the mathematical foundations of probability.  

### Introduction to probability  

Slides: [Introduction to Probability](http://rpubs.com/jgscott/intro_probability)  

R scripts and data sets:  
- [predimed_intro.R](R/predimed_intro.R) and [predimed.csv](data/predimed.csv)   

Two short pieces that illustrate the "fallacy of mistaken compounding":  
- [How likely is it that birth control could let you down?](https://www.nytimes.com/interactive/2014/09/14/sunday-review/unplanned-pregnancies.html) from the _New York Times_  
- An excerpt from Chapter 7 of [AIQ: How People and Machines are Smarter Together](https://github.com/jgscott/ECO394D/blob/master/ref/AIQ_excerpt_contraceptive_effectiveness.pdf), by Nick Polson and James Scott.    



### Discrete random variables

Slides: [Discrete random variables](http://rpubs.com/jgscott/discrete_random_variables)  


R scripts:  
- [poisson_binomial.R](./R/poisson_binomial.R)  


### General random variables

Slides: [General random variables](http://rpubs.com/jgscott/general_random_variables)   

R scripts:  
- [testmax_example.R](./R/testmax_example.R)  
- [normal_example.R](./R/normal_example.R) and [annual_returns_since1928.csv](./data/annual_returns_since1928.csv)  
- [wellbore.R](./R/wellbore.R)    


### Multivariate distributions

Slides: [Multivariate distributions](http://rpubs.com/jgscott/multivariate_distributions)   

R scripts:  
- [bivariate_normal.R](./R/bivariate_normal.R)  
- [Case study on stocks and bonds](https://github.com/jgscott/learnR/blob/master/cases/bvnorm/bvnorm.md)  


### Midterm


Midterm statistics: curved grades

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  65.00   83.50   86.80   86.25   92.30   98.90 
```

Midterm statistics: raw grades

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   35.0    70.0    76.0    74.9    86.0    98.0 
```


## Part 2: Data analysis

In this middle part of the course, we spend a lot more time hands-on with data sets and with R.  


### Data exploration and visualization

Topics: data visualization and practice with R.  Bar plots; basic plots for numerical data (scatterplots, boxplots, histograms, line graphs); panel plots.  Introduction to ggplot2.  

Examples of [bad graphics](ref/badgraphics.pdf).  And [an example from the New York Times](https://www.nytimes.com/interactive/2018/08/30/climate/how-much-hotter-is-your-hometown.html).  

Slides: [Introduction to Data Exploration](http://rpubs.com/jgscott/data_exploration)  

R scripts and data:  
- [mpg.R](R/mpg.R)  
- [titanic.R](R/titanic.R) and [TitanicSurvival.csv](data/TitanicSurvival.csv)  
- [toyimports_linegraph.R](R/toyimports_linegraph.R) and [toyimports.csv](data/toyimports.csv)  
- [Green buildings case study](exercises/green_buildings.md)  


Inspiration:  
- [50 ggplots](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html)  
- [A map of average ages in Swiss municipalities](https://github.com/grssnbchr/thematic-maps-ggplot2)  
- [Low-income students in college](https://www.nytimes.com/interactive/2017/01/18/upshot/some-colleges-have-more-students-from-the-top-1-percent-than-the-bottom-60.html)  
- [The French presidential election](https://www.nytimes.com/interactive/2017/04/23/world/europe/french-election-results-maps.html)  
- [LeBron James's playoff scoring record](https://www.nytimes.com/interactive/2017/05/25/sports/basketball/lebron-career-playoff-points-record.html)   



### Fitting equations to data

Slides: [Fitting equations](http://rpubs.com/jgscott/fitting_equations)  
  
R scripts and data:    
- [afc_intro.R](./R/afc_intro.R) and [afc.csv](data/afc.csv)
- [creatinine.csv](data/creatinine.csv)  
- [utilities.R](./R/utilities.R)  
- [race_splines.R](./R/race_splines.R)  
- [ebola.R](./R/ebola.R)  

## Part 3: Statistical inference

### Sampling distributions and the bootstrap

Slides: [Introduction to the bootstrap](http://rpubs.com/jgscott/bootstrap_intro)  
  

R scripts and data:    
- [creatinine_bootstrap.R](./R/creatinine_bootstrap.R) and [creatinine.csv](data/creatinine.csv)   
- [predimed_bootstrap.R](./R/predimed_bootstrap.R)  
- [predimed_plugin.R](./R/predimed_plugin.R)  


### Basics of hypothesis testing  

Slides: [Introduction to hypothesis testing](http://rpubs.com/jgscott/testing)    
  
R scripts and data:    
- [patriots.R](./R/patriots.R)  
- [power.R](./R/power.R)   


### Introduction to asymptotic theory 

Slides: [Introduction to asymptotic theory](http://rpubs.com/jgscott/intro_asymptotic_theory)    
  

R scripts and data:    
- [clt.R](./R/clt.R)   
- [normalCI_examples.R](./R/normalCI_examples.R)  
  
### Constructing estimators  

Slides: [Constructing estimators](http://rpubs.com/jgscott/constructing_estimators)    
  
 