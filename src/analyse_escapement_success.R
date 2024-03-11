# Analyse escapement success to the sea
# By Pieterjan Verhelst (INBO)
# pieterjan.verhelst@inbo.be

library(tidyverse)
library(multcomp) 


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

# 2. Plot escapement success in relation to barrier impacts and fishing ####
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
  scale_fill_manual(values = c("no" = "lightgrey",
                                "yes" = "gray35")) +
  ylab("Successful proportion") + 
  xlab("Water body class") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 2, color = "black", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle= 360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


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



# 3. Statistical analysis: binomial regression ####

# Remove redundant columns to create better overview
escape$barrier_type_when_multiple <- NULL
escape$barrier_type <- NULL
escape$barrier_type2 <- NULL
escape$barrier_number <- NULL
escape$barrier_impact_score <- NULL

# Create new variable as water body class and fishing combined
escape$wbc_fishing <-  paste(escape$water_body_class, escape$fishing, sep = "_")
escape$wbc_fishing <- factor(escape$wbc_fishing)

# Calculate unsuccessful migrants
escape$unsuccessful_migrants <- escape$migrants - escape$successful_migrants

# Remove ordered class from factor
scape$water_body_class <- factor(escape$water_body_class, ordered = FALSE)

# Apply binomial regression
# GLM1: fishing as separate fixed variable
glm1 <- glm(formula = cbind(successful_migrants, unsuccessful_migrants) ~ water_body_class + fishing, data = escape, family = binomial())
summary(glm1)

# GLM2: water body class and fishing combined as one fixed variable
glm2 <- glm(formula = cbind(successful_migrants, unsuccessful_migrants) ~ wbc_fishing, data = escape, family = binomial())
summary(glm2)

# Apply multiple comparisons on the model
posthoc <- glht(glm2, linfct = mcp(wbc_fishing = "Tukey"))
summary(posthoc)
par(mar = c(4, 7, 2, 2))  #par(mar = c(bottom, left, top, right))
plot(posthoc)












