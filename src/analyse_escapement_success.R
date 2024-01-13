# Analyse escapement success to the sea
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be

library(tidyverse)


# 1. Load data ####
escape <- read_csv("./data/external/escapement_success.csv")

escape$barrier_type <- factor(escape$barrier_type)
escape$fishing <- factor(escape$fishing)

aggregate(escape$successful_proportion, list(escape$fishing, escape$barrier_type), mean)

# 2. Escapement success and barrier type analysis ####
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


# Plot
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


