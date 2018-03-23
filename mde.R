library(data.table)
library(dplyr)
library(ggplot2)

# initial params ----------------------------------------------------------

n_rep <- 10^5
p <- 0.1
p_effect <- 0.11
n <- 10^5


# plot effects ------------------------------------------------------------

base_rate <- rbinom(n_rep, n/2, p)/(n/2)
no_effect <- rbinom(n_rep, n/2, p)/(n/2)

rate_effect <- rbinom(n_rep, n/2, p_effect)/(n/2)

dt <- data.table(feature_A = no_effect - base_rate, 
				 feature_B = rate_effect - base_rate) %>% 
	melt(measure.vars = c("feature_A", "feature_B"))

p <- ggplot(data = dt, aes(x = value, color = variable)) + 
		geom_freqpoly() + 
		labs(x = "open rate difference (%)", y = NULL, color = NULL) + 
		scale_x_continuous(labels = scales::percent) +
		theme(axis.text.y=element_blank(),
			  axis.ticks.y = element_blank()) + 
		geom_vline(xintercept = 0, color = "lightgreen") + 
		geom_vline(xintercept = 0.01, color = "lightblue")
p

p + geom_segment(aes(x = 0, xend = 0.01, y = 18000, yend = 18000), 
			 	 color = "red",
			 	 arrow = arrow(length=unit(0.30, "cm"), ends = "both", type = "closed")) + 
	annotate("text", x = 0.005, y = 18000, label = "Detected Effect", vjust = 1)


# MDE ---------------------------------------------------------------------

calculateMinimumDetectableEffect <- function(p1, n, prop_of_treatment = 0.5, alpha = 0.05, beta = 0.2) {
    M <- qt(1-alpha/2, n - 1) + qt(1-beta, n - 1)
    variance <- p1 * (1 - p1)
    weight <- (prop_of_treatment * (1 - prop_of_treatment) * n)
    M * sqrt(variance / weight)
}

n_seq <- seq.int(1000, 100000, by = 100)
mde_dist <- data.table(n = n_seq, MDE = calculateMinimumDetectableEffect(0.1, n_seq))

ggplot(mde_dist, aes(x = n, y = MDE)) + 
	geom_line() + 
	scale_x_continuous(labels = scales::comma) +
	scale_y_continuous(labels = scales::percent) +
	labs(title = "Distribution of MDE given different sample sizes",
		 subtitle = "When base rate is 10%, with 95% significance level and 80% power",
		 x = "sample size", y = "Minimum Detectable Effect (%)")