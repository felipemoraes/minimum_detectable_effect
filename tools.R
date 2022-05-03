createDetecteableEffectsForExperiments <- function(base_rate = p, effect_rate = p_effect, sample_size = n, num_repl = n_rep, seed = 123) {
    set.seed(seed)

    base_rate_distr <- rbinom(num_repl, sample_size/2, base_rate)/(sample_size/2)
    inconclusive_experiment_distr <- rbinom(num_repl, sample_size/2, base_rate)/(sample_size/2)
    conclusive_experiment_distr <- rbinom(num_repl, sample_size/2, effect_rate)/(sample_size/2)

    data.table(inconclusive_experiment = inconclusive_experiment_distr - base_rate_distr,
               conclusive_experiment = conclusive_experiment_distr - base_rate_distr) %>% 
        melt(measure.vars = c("inconclusive_experiment", "conclusive_experiment"))
}

calculateMinimumDetectableEffect <- function(p1, n, prop_of_treatment = 0.5, alpha = 0.05, beta = 0.2) {
    M <- qt(1-alpha/2, n - 1) + qt(1-beta, n - 1)
    variance <- p1 * (1 - p1)
    weight <- prop_of_treatment * (1 - prop_of_treatment) * n
    M * sqrt(variance / weight)
}

calculateCriticalValue <- function(dt_inconclusive_and_conclusive_experiment) {
    dt_inconclusive_and_conclusive_experiment[variable == "inconclusive_experiment", quantile(value, 1 - alpha/2)]
}

plotDetectableEffects <- function(dt_inconclusive_experiment, num_contact = n) {
    ggplot() + 
        geom_density(data = dt_inconclusive_experiment, 
            aes(x = value, alpha = 0.5), 
            color = "lightgreen", fill = "lightgreen") + 
        scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
        labs(x = "booking rate difference (%)", y = NULL, fill = NULL) + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none") + 
        geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed") +
        ggtitle("Distribution of Detectable Effects",
                subtitle = glue('n = {format(num_contact, scientific = FALSE, big.mark = ",")}'))
}

plotinconclusiveexperiment <- function(dt_inconclusive_experiment, criticalValue = critical_value, base_rate = p, effect_rate = p_effect, num_contact = n) {
    ggplot() + 
        geom_density(data = dt_inconclusive_experiment, 
            aes(x = value, alpha = 0.5), 
            color = "lightgreen", fill = "lightgreen") + 
        scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
        labs(x = "booking rate difference (%)", y = NULL, fill = NULL) + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none") + 
        geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed") + 
        geom_vline(xintercept = effect_rate - base_rate, color = "darkblue", linetype = "dashed") + 
        geom_vline(xintercept = criticalValue, color = "red") +
        annotate("text", x = criticalValue, y = 0, label = "Critical value", 
                 hjust = 0, vjust = 1, angle = 90) + 
        geom_segment(aes(x = 0, xend = effect_rate - base_rate, y = 15, yend = 15),
                         arrow = arrow(length=unit(0.30, "cm"), ends = "both", type = "closed")) +
        annotate("text", x = (effect_rate - base_rate)/2, y = 15, label = "Measured Effect", vjust = -1) +
        ggtitle("Distribution of Detectable Effects when there is \nactually no difference in booking rates",
                subtitle = glue('n = {format(num_contact, scientific = FALSE, big.mark = ",")}'))
}

plotinconclusiveAndconclusiveExperiment <- function(dt_inconclusive_and_conclusive_experiment, criticalValue = critical_value, base_rate = p, effect_rate = p_effect, num_contact = n){
    ggplot() +
        geom_density(data = dt_inconclusive_and_conclusive_experiment, 
            aes(x = value, fill = variable, color = variable), alpha = 0.5) +
        scale_fill_manual(values = c("lightgreen", "lightblue"), 
                          labels = c("inconclusive experiment", "conclusive experiment")) +
        scale_color_manual(values = c("lightgreen", "lightblue"), guide = FALSE) +
        scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
        labs(x = "booking rate difference (%)", y = NULL, fill = NULL) + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) + 
        geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed") + 
        geom_vline(xintercept = effect_rate - base_rate, color = "darkblue", linetype = "dashed") + 
        geom_vline(xintercept = criticalValue, color = "red") +
        annotate("text", x = criticalValue, y = 0, label = "Critical value", 
                 hjust = 0, vjust = 1, angle = 90) + 
        geom_segment(aes(x = 0, xend = effect_rate - base_rate, y = 15, yend = 15),
                         arrow = arrow(length=unit(0.30, "cm"), ends = "both", type = "closed")) +
        annotate("text", x = (effect_rate - base_rate)/2, y = 15, label = "Measured Effect", vjust = -1) +
        ggtitle("Distribution of Detectable Effects",
                subtitle = glue('n = {format(num_contact, scientific = FALSE, big.mark = ",")}'))
}
