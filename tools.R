createDetecteableEffectsForFeatures <- function(base_rate = p, effect_rate = p_effect, sample_size = n, num_repl = n_rep, seed = 123) {
    set.seed(seed)

    base_rate_distr <- rbinom(num_repl, sample_size/2, base_rate)/(sample_size/2)
    useless_feature_distr <- rbinom(num_repl, sample_size/2, base_rate)/(sample_size/2)
    super_feature_distr <- rbinom(num_repl, sample_size/2, effect_rate)/(sample_size/2)

    data.table(useless_feature = useless_feature_distr - base_rate_distr,
               super_feature = super_feature_distr - base_rate_distr) %>% 
        melt(measure.vars = c("useless_feature", "super_feature"))
}

calculateMinimumDetectableEffect <- function(p1, n, prop_of_treatment = 0.5, alpha = 0.05, beta = 0.2) {
    M <- qt(1-alpha/2, n - 1) + qt(1-beta, n - 1)
    variance <- p1 * (1 - p1)
    weight <- prop_of_treatment * (1 - prop_of_treatment) * n
    M * sqrt(variance / weight)
}

calculateCriticalValue <- function(dt_useless_and_super_feature) {
    dt_useless_and_super_feature[variable == "useless_feature", quantile(value, 1 - alpha/2)]
}

plotDetectableEffects <- function(dt_useless_feature, num_contact = n) {
    ggplot() + 
        geom_density(data = dt_useless_feature, 
            aes(x = value, alpha = 0.5), 
            color = "lightgreen", fill = "lightgreen") + 
        scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
        labs(x = "open rate difference (%)", y = NULL, fill = NULL) + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none") + 
        geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed") +
        ggtitle("Distribution of Detectable Effects when there is actually no difference in open rates",
                subtitle = glue('n = {format(num_contact, scientific = FALSE, big.mark = ",")}'))
}

plotUselessFeature <- function(dt_useless_feature, criticalValue = critical_value, base_rate = p, effect_rate = p_effect, num_contact = n) {
    ggplot() + 
        geom_density(data = dt_useless_feature, 
            aes(x = value, alpha = 0.5), 
            color = "lightgreen", fill = "lightgreen") + 
        scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
        labs(x = "open rate difference (%)", y = NULL, fill = NULL) + 
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
        ggtitle("Distribution of Detectable Effects when there is actually no difference in open rates",
                subtitle = glue('n = {format(num_contact, scientific = FALSE, big.mark = ",")}'))
}

plotUselessAndSuperFeature <- function(dt_useless_and_super_feature, criticalValue = critical_value, base_rate = p, effect_rate = p_effect, num_contact = n){
    ggplot() +
        geom_density(data = dt_useless_and_super_feature, 
            aes(x = value, fill = variable, color = variable), alpha = 0.5) +
        scale_fill_manual(values = c("lightgreen", "lightblue"), 
                          labels = c("useless feature", "super feature")) +
        scale_color_manual(values = c("lightgreen", "lightblue"), guide = FALSE) +
        scale_x_continuous(labels = scales::percent, breaks = seq(-0.02, 0.04, 0.01)) +
        labs(x = "open rate difference (%)", y = NULL, fill = NULL) + 
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