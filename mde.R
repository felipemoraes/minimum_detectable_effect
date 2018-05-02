library(data.table)
library(dplyr)
library(ggplot2)
library(glue)

source("tools.R")


# initial params ----------------------------------------------------------

n_rep <- 10^5
p <- 0.1
p_effect <- 0.11
n <- 10^4

alpha <- 0.05
beta <- 0.2

# plot effects ------------------------------------------------------------

dt_useless_and_super_feature <- createDetecteableEffectsForFeatures()
critical_value <- calculateCriticalValue(dt_useless_and_super_feature)

plotDetectableEffects(dt_useless_and_super_feature[variable == "useless_feature"])

plotUselessFeature(dt_useless_and_super_feature[variable == "useless_feature"])

plotUselessAndSuperFeature(dt_useless_and_super_feature)

calculateMinimumDetectableEffect(p, n, alpha = alpha, beta = beta)

# MDE distribution --------------------------------------------------------

n_seq <- seq.int(1000, 100000, by = 100)
mde_dist <- data.table(n = n_seq, MDE = calculateMinimumDetectableEffect(p, n_seq, alpha = alpha, beta = beta))

ggplot(mde_dist, aes(x = n, y = MDE)) + 
	geom_line() + 
	scale_x_continuous(labels = scales::comma) +
	scale_y_continuous(labels = scales::percent) +
	labs(title = "Distribution of MDE given different sample sizes",
		 subtitle = glue("When base rate is {p * 100}%, with {(1 - alpha) * 100}% significance level and {(1 - beta) * 100}% power"),
		 x = "sample size", y = "Minimum Detectable Effect (%)")


# calculate MDE with larger sample sizes ---------------------------------

n <- 30000

dt_useless_and_super_feature <- createDetecteableEffectsForFeatures(sample_size = n)
critical_value <- calculateCriticalValue(dt_useless_and_super_feature)

plotUselessAndSuperFeature(dt_useless_and_super_feature)


n <- 200000

dt_useless_and_super_feature <- createDetecteableEffectsForFeatures(sample_size = n)
critical_value <- calculateCriticalValue(dt_useless_and_super_feature)

plotUselessAndSuperFeature(dt_useless_and_super_feature)

