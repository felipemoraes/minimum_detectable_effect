library(data.table)
library(dplyr)
library(ggplot2)
library(glue)

# initial params ----------------------------------------------------------

n_rep <- 10^5
p <- 0.1
p_effect <- 0.11
n <- 10000

alpha <- 0.05
beta <- 0.2

# plot effects ------------------------------------------------------------

base_rate <- rbinom(n_rep, n/2, p)/(n/2)
useless_feature <- rbinom(n_rep, n/2, p)/(n/2)

dt_useless_feature <- data.table(useless_feature_distr = useless_feature - base_rate)

critival_value <- dt_useless_feature[, quantile(useless_feature_distr, 1 - alpha/2)]

ggplot() + 
	geom_density(data = dt_useless_feature, 
		aes(x = useless_feature_distr, alpha = 0.5), 
		color = "lightgreen", fill = "lightgreen") + 
	scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
	labs(x = "open rate difference (%)", y = NULL, fill = NULL) + 
	theme(axis.text.y=element_blank(),
		  axis.ticks.y = element_blank(),
		  legend.position = "none") + 
	geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed") + 
	geom_vline(xintercept = p_effect - p, color = "darkblue", linetype = "dashed") + 
	geom_vline(xintercept = critival_value, color = "red") +
	annotate("text", x = critival_value, y = 0, label = "Critical value", 
			 hjust = 0, vjust = 1, angle = 90) + 
	geom_segment(aes(x = 0, xend = p_effect - p, y = 15, yend = 15),
				 	 arrow = arrow(length=unit(0.30, "cm"), ends = "both", type = "closed")) +
	annotate("text", x = (p_effect - p)/2, y = 15, label = "Measured Effect", vjust = -1) +
	ggtitle("Distribution of Detectable Effects when there is actually no difference in open rates",
			subtitle = glue("n = {n}"))
	

super_feature <- rbinom(n_rep, n/2, p_effect)/(n/2)
dt_super_and_useless_feature <- data.table(useless_feature_distr = useless_feature - base_rate,
							   super_feature_distr = super_feature - base_rate) %>% 
	melt(measure.vars = c("useless_feature_distr", "super_feature_distr"))

ggplot() +
	geom_density(data = dt_super_and_useless_feature, 
		aes(x = value, fill = variable, color = variable), alpha = 0.5) +
	scale_fill_manual(values = c("lightgreen", "lightblue"), 
					  labels = c("useless feature", "super feature")) +
	scale_color_manual(values = c("lightgreen", "lightblue"), guide = FALSE) +
	scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
	labs(x = "open rate difference (%)", y = NULL, fill = NULL) + 
	theme(axis.text.y=element_blank(),
		  axis.ticks.y = element_blank()) + 
	geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed") + 
	geom_vline(xintercept = p_effect - p, color = "darkblue", linetype = "dashed") + 
	geom_vline(xintercept = critival_value, color = "red") +
	annotate("text", x = critival_value, y = 0, label = "Critical value", 
			 hjust = 0, vjust = 1, angle = 90) + 
	geom_segment(aes(x = 0, xend = p_effect - p, y = 15, yend = 15), 
				 	 arrow = arrow(length=unit(0.30, "cm"), ends = "both", type = "closed")) +
	annotate("text", x = (p_effect - p)/2, y = 15, label = "Measured Effect", vjust = -1) +
	ggtitle("Distribution of Detectable Effects",
			subtitle = glue("n = {n}"))
	

# MDE distribution --------------------------------------------------------

calculateMinimumDetectableEffect <- function(p1, n, prop_of_treatment = 0.5, alpha = 0.05, beta = 0.2) {
    M <- qt(1-alpha/2, n - 1) + qt(1-beta, n - 1)
    variance <- p1 * (1 - p1)
    weight <- prop_of_treatment * (1 - prop_of_treatment) * n
    M * sqrt(variance / weight)
}

n_seq <- seq.int(1000, 100000, by = 100)
mde_dist <- data.table(n = n_seq, MDE = calculateMinimumDetectableEffect(p, n_seq, alpha = alpha, beta = beta))

ggplot(mde_dist, aes(x = n, y = MDE)) + 
	geom_line() + 
	scale_x_continuous(labels = scales::comma) +
	scale_y_continuous(labels = scales::percent) +
	labs(title = "Distribution of MDE given different sample sizes",
		 subtitle = glue("When base rate is {p * 100}%, with {(1 - alpha) * 100}% significance level and {(1 - beta) * 100}% power"),
		 x = "sample size", y = "Minimum Detectable Effect (%)")


# calculate MDE for current sample size -----------------------------------

mde <- calculateMinimumDetectableEffect(p, n, alpha = alpha, beta = beta)
