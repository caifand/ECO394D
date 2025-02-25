library(mosaic)
library(foreach)


#####
# Example 1: inference for a binomial proportion
#####

# Example goal: estimate P(cured | takes new drug)
# (as in a safety and efficacy trial, FDA phase 1)

# confidence level = 1-alpha
alpha = 0.05

# true probability
p_true = 0.2

# sample size
N = 500

# Let's simulate a single data set
results = rbinom(N, 1, p_true)

# step 1: calculate the estimator
p_hat = sum(results)/N

# step 2: use the plug-in method to estimate the standard error
se_hat = sqrt(p_hat * (1-p_hat)/N)

# step 3: form confidence interval based on asymptotic normality assumption
z_crit = qnorm(1 - alpha/2)
lower_bound = p_hat - z_crit*se_hat
upper_bound = p_hat + z_crit*se_hat
lower_bound
upper_bound

# check whether the interval contained the true value
(p_true > lower_bound) & (p_true < upper_bound)


# compare the normal CI with the bootstrapped CI
boot1 = do(1000)*{
	results_bootstrap = resample(results)
	p_hat_boot = sum(results_bootstrap)/N
}

# really close
confint(boot1)


# Let's repeat this in a Monte Carlo simulation
NMC = 1000
sim1 = do(NMC)*{
	results = rbinom(N, 1, p_true)
	p_hat = sum(results)/N
	se_hat = sqrt(p_hat * (1-p_hat)/N)
	lower_bound = p_hat - z_crit*se_hat
	upper_bound = p_hat + z_crit*se_hat
	# now check whether our interval covers the truth
	(p_true > lower_bound) & (p_true < upper_bound)
}

sum(sim1$result)/NMC

# now repeat for many different values of N
n_grid = seq(5, 200, by=5)
coverage_grid = rep(0, length(n_grid))
for(i in seq_along(n_grid)) {
	N = n_grid[i]
	this_sim = do(NMC)*{
		results = rbinom(N, 1, p_true)
		p_hat = sum(results)/N
		se_hat = sqrt(p_hat * (1-p_hat)/N)
		lower_bound = p_hat - z_crit*se_hat
		upper_bound = p_hat + z_crit*se_hat
		(p_true > lower_bound) & (p_true < upper_bound)
	}
	coverage_grid[i] = sum(this_sim$result)/NMC
}

plot(n_grid, coverage_grid)
abline(h=1-alpha)



#####
# Example 2: inference for a mean
#####


# Goal: estimate mean systolic blood pressure in the population

# population = rnorm(1e7, 130, 15)
population = c(rnorm(1e3, 100, 5), rnorm(1e4, 120, 5), rnorm(5e3, 145, 15))

hist(population, 50)

# true mean and std dev
mu_true = mean(population)
sigma_true = sd(population)

# sample size
N = 30

# Let's simulate a single data set
y = sample(population, N)
hist(y)


# step 1: calculate the estimator
mu_hat = mean(y)

# step 2: use the plug-in method to estimate the standard error
sigma_hat = sd(y)
se_hat = sigma_hat/sqrt(N)

# step 3: form confidence interval based on asymptotic normality assumption
z_crit = qnorm(1 - alpha/2)
lower_bound = mu_hat - z_crit*se_hat
upper_bound = mu_hat + z_crit*se_hat
lower_bound
upper_bound


# compare with a bootstrapped confidence interval
boot2 = do(1000)*{
	y_boot = resample(y)
	mean(y_boot)
}

confint(boot2)


# check coverage of normal confidence intervals
NMC = 1000
this_sim = do(NMC)*{
	# take a real sample from the population
    y = sample(population, N)
    
    # form the estimate and normal-based confidence interval
    mu_hat = mean(y)
    se_hat = sd(y)/sqrt(N)
    lower_bound = mu_hat - z_crit*se_hat
    upper_bound = mu_hat + z_crit*se_hat
    
    # check whether it covered the truth
    (mu_true > lower_bound) & (mu_true < upper_bound)
}

# pretty close to the desired confidence level
sum(this_sim$result)/NMC


