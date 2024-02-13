# Analyse escapement success to the sea
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be

library(tidyverse)


# 1. Load data ####
escape <- read_csv("./data/external/escapement_success.csv")

escape$water_type <- factor(escape$water_type)
escape$barrier_type <- factor(escape$barrier_type, ordered = TRUE, 
                              levels = c("none", "weir_sluice", "shipping_lock", "hydropower", "pump", "pump_sluice"))
escape$barrier_type2 <- factor(escape$barrier_type2, ordered = TRUE, 
                              levels = c("none", "weir", "shipping_lock", "hydropower", "pump"))
escape$barrier_type_when_multiple <- factor(escape$barrier_type_when_multiple, ordered = TRUE, 
                              levels = c("none", "weir_sluice", "shipping_lock", "weir_shipping_lock", "hydropower", "pump_sluice", "pump_shipping_lock"))
escape$water_body_class <- factor(escape$water_body_class, ordered = TRUE, 
                                            levels = c("A", "B", "C", "D", "E"))
escape$fishing <- factor(escape$fishing)
#escape$barrier_impact_score <- as.numeric(escape$barrier_score) * as.numeric(escape$barrier_number)

aggregate(escape$successful_proportion, list(escape$fishing, escape$water_body_class), mean)

# 2. Escapement success and barrier type analysis ####
# Plot in function of barrier types
ggplot(escape, aes(x=barrier_type, y=successful_proportion)) + 
  geom_boxplot() +
  #scale_fill_brewer(palette="Dark2") +
  ylab("Successful proportion") + 
  xlab("Barrier type") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))

# Plot also including fishing
ggplot(escape, aes(x=barrier_type, y=successful_proportion, fill = fishing)) + 
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  ylab("Successful proportion") + 
  xlab("Barrier type") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))

# Plot in function of water body class and fishing
ggplot(escape, aes(x=water_body_class, y=successful_proportion, fill = fishing)) + 
  geom_boxplot() +
  #scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values = c("no" = "blue",
                                "yes" = "red")) +
  ylab("Successful proportion") + 
  xlab("Water body class") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle= 360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16)) 


# Plot in function of barrier impact score and fishing
ggplot(escape, aes(x=barrier_impact_score, y=successful_proportion)) + 
  geom_point(aes(shape = factor(fishing), colour = factor(barrier_type_when_multiple)), size = 4) +
  scale_color_manual(values = c("none" = "blue",
                                "weir_sluice" = "lightblue",
                                "shipping_lock" = "yellow2",
                                "weir_shipping_lock" = "orange",
                                "hydropower" = "purple",
                                "pump_sluice" = "red",
                                "pump_shipping_lock" = "darkred")) +
  ylab("Successful proportion") + 
  xlab("WRS impact score") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16))




# Conduct ANOVA or non-parametric alternative

# Check normality
qqnorm(escape$successful_proportion)
qqline(escape$successful_proportion)

shapiro.test(escape$successful_proportion)

# Check homogeneity of variances
# Levene’s test
# Levene’s test is used to assess whether the variances of two or more populations are equal.
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# When p > 0.05, there is no significant difference between the two variances.
car::leveneTest(successful_proportion ~ barrier_type, data = escape)

# Conduct one-way ANOVA 
aov <- aov(escape$successful_proportion ~ escape$barrier_type)
summary(aov)

anova <- oneway.test(escape$successful_proportion ~ escape$barrier_type, var.equal=FALSE) # var.equal = FALSE when homogeneity of variances is not fulfilled
anova

# Check assumptions
par(mfrow=c(2,2))
plot(aov)
dev.off

# Post-hoc test for equal variances
TukeyHSD(aov)

# Post-hoc test for unequal variances
posthocTGH(migration_speed_nontidal$speed_ms, migration_speed_nontidal$animal_project_code, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

# Kruskal-Wallis test when data is not normally distributed
kruskal.test(escape$successful_proportion ~ escape$barrier_type)
#posthoc.kruskal.dunn.test(x=migration_speed_nontidal$animal_project_code, g=migration_speed_nontidal$speed_ms, p.adjust.method="bonferroni")
FSA::dunnTest(escape$successful_proportion ~ escape$barrier_type, data=escape, method="bonferroni")


