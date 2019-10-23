library(mosaic)
library(tidyverse)

predimed = read.csv('../data/predimed.csv')


###
# Example 1: standard error of the sample mean
###

# calculate sample means, sample sizes, and sample std devs
predimed %>% group_by(group) %>%
	summarize(bmi_mean = mean(bmi), sample_size = n(), std_dev = sd(bmi))

# form the plug-in standard error for mean BMI in control group
se_hat_control = 3.96/sqrt(2042)

# compare it with a bootstrap estimate of the standard error
boot1 = do(1000)*{
	#	resample data with replacement
	predimed_boot = resample(predimed)
	
	# Re-estimate the parameter using the re-sampled data
	mu_hat = mean(bmi ~ group, data= predimed_boot)
	
	# save the result
	mu_hat
}

head(boot1)

# Bootstrapped estimate of standard error
sd(boot1$Control)

# very close to the plug-in standard error
se_hat_control


###
# Example 2: standard error of a difference in proportions  
###

xtabs(~group, data=predimed)

xtabs(~event + group, data=predimed)

xtabs(~event + group, data=predimed) %>%
	prop.table(margin=2)


phat1 = 0.04750245
phat2 = 0.03895509

sqrt(phat1 * (1-phat1)/2042 + phat2*(1-phat2)/2182)
# var(x1-x2) = var(x1) + var(X2)

phat=0.04750245
qhat=(0.03333333+0.03895509)

se_dif_pro = sqrt(phat*(1-phat)/2042+qhat*(1-qhat)/(2100+2182))


alpha=0.05
z_crit = qnorm(1 - alpha/2)

muhat=phat-qhat
lower_bound = muhat - z_crit*se_dif_pro
upper_bound = muhat + z_crit*se_dif_pro
lower_bound
upper_bound

head(predimed)
control = predimed %>% filter(group=="Control") %>% select(group, event)
MedDiet = predimed %>% filter(group!="Control") %>% select(group, event) %>% mutate(group="MedDiet")

xtabs(~event + group, data=MedDiet)
xtabs(~event + group, data=control)
data = bind_rows(control, MedDiet)

xtabs(~event + group, data=data)
phat=97/2042
qhat=155/4282
se_dif_pro = sqrt(phat*(1-phat)/2042+qhat*(1-qhat)/(2100+2182))
se_dif_pro = 0.0056
alpha=0.05
z_crit = qnorm(1 - alpha/2)

muhat=phat-qhat
lower_bound = muhat - z_crit*se_dif_pro
upper_bound = muhat + z_crit*se_dif_pro
lower_bound
upper_bound

boot1 = do(10000)*{
  p = count(resample(control)$event == "Yes")/2042
  q = count(resample(MedDiet)$event == "Yes")/4282
  p-q
}

confint(boot1, level=0.95)

hist(boot1$n_TRUE)
sum(boot1$n_TRUE >= 0)/10000
