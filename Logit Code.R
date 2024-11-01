##################################
# Lab: Logit, MLogit, and OLogit #
##################################

# Setwd ========================================================================
# setwd("~/Dropbox/pols581 TA/Logits, MLogits, OLogits")

# Clean-up Workspace ===========================================================
rm(list = ls(all = TRUE))

suppressWarnings(
  if (!is.null(names(sessionInfo()$otherPkgs))) {
    invisible(lapply(
      paste0('package:', names(sessionInfo()$otherPkgs)),
      detach,
      character.only = TRUE,
      unload = TRUE
    ))}
)

graphics.off()

# Libraries ====================================================================
library(descr)
library(Kendall)
library(nnet)
library(MASS)
library(dplyr)
library(tidyr)
library(xtable)
library(texreg)
library(ggplot2)

# Set options for ggplot  ======================================================
ggplotTheme <-
  theme_bw() + theme(text = element_text(size = 16),
                     axis.text = element_text(colour = "black"))
theme_set(ggplotTheme)

# Color-blind-friendly palette (if using color). This palette is from
# http://jfly.iam.u-tokyo.ac.jp/color/:
cbbPalette <-
  c(
    "#000000",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

# Load and Clean the Data ======================================================
# Importing data from poliscidata (without loading the library):
df <- poliscidata::gss %>%
  # Identify and Recode Continuous Independent Variables =======================
  # Recode 'educ' into fairly continuous 'educ_years' #
  dplyr::mutate(educ_years = as.numeric(educ)-1,
  # Identify and Recode Dummy Dependent Variables ==============================
  # Recode 'voted08' into a 0/1 dummy variable with 1 = 'Voted' #
                vote08_dummy = if_else(voted08 == "Voted", 1, 0),
  # Recode 'pres08' into a 0/1 dummy variable with 1 = 'Obama' #
                pres08_dummy = if_else(pres08 == "Obama", 1, 0))
  # Identify and clean nominal dependent variables =============================
  # Recode 'bible' #
  table(df$bible)
  attributes(df$bible)
  df$bible <- factor(df$bible,
    levels = c("WORD OF GOD", "INSPIRED WORD", "BOOK OF FABLES"),
    labels = c("Word of God", "Inspired Word","Book of Fables"))
df$bible <- relevel(df$bible, ref = "Inspired Word")
table(df$bible)

  # Identify and clean ordinal dependent variables =============================
  # Recode 'fechld' #
  table(df$fechld)
  attributes(df$fechld)
  df$fechld <- factor(df$fechld,
    levels = c("STRONGLY DISAGREE", "DISAGREE", "AGREE", "STRONGLY AGREE"),
    labels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
    ordered = TRUE)
attributes(df$fechld)
table(df$fechld)

# Check Recodes ================================================================

# Highest Year of School (educ) #
# Before Recode #
table(df$educ)
# After Recode #
table(df$educ_years)

# Voted 2008 #
# Before Recode #
table(df$voted08)
# After Recode #
table(df$vote08_dummy)

# Presidential Choice 2008 #
# Before Recode #
table(df$pres08)
# After Recode #
table(df$pres08_dummy)

# Bivariate Logistic Regression (Logit) ========================================
# Using whether the Respondent voted in 2008 as the DV #
logit1 <- glm(data = df, vote08_dummy ~ educ_years, family = binomial(link = 'logit'))
screenreg(logit1, digits = 3)

logit2 <- glm(data = df, vote08_dummy ~ age, family = binomial(link = 'logit'))
screenreg(logit2, digits = 3)

logit3 <- glm(data = df, pres08_dummy ~ educ_years, family = binomial(link = 'logit'))
screenreg(logit3, digits = 3)

logit4 <- glm(data = df, pres08_dummy ~ age, family = binomial(link = 'logit'))
screenreg(logit4, digits = 3)

# Note: Tells R to put the raw outputs next to each other in the same table #
screenreg(list(logit1, logit2, logit3, logit4))  

# Bivariate Multinomial Logistic Regression ====================================
# Using feelings about the bible as the DV:
mlogit1 <- multinom(data = df, bible ~ age)
screenreg(mlogit1, digits = 3)

mlogit2 <- multinom(data = df, bible ~ educ_years)
screenreg(mlogit2, digits = 3)

screenreg(list(mlogit1, mlogit2))

# Bivariate Ordered Logistic Regression ========================================
# Using beliefs that mother working doesn't hurt children:
ologit1 <- polr(
  data = df,
  fechld ~ educ_years,
  method = c('logistic'),
  Hess = TRUE
)
screenreg(ologit1)

ologit2 <- polr(
  data = df,
  fechld ~ age,
  method = c('logistic'),
  Hess = TRUE
)
screenreg(ologit2)

screenreg(list(ologit1,ologit2))

# Make TeX files of the tables =================================================
# Make a single table of logit output:
texreg(
  list(logit1, logit2),
  stars = 0.05,
  digits = 3,
  booktabs = TRUE,
  dcolumn = TRUE,
  use.packages = FALSE,
  file = "tab-logitModels.tex",
  caption.above = TRUE,
  caption = "Logistic Results of Voting in 2008",
  label = "tab:logit",
  custom.coef.names = c(
    "Constant", 
    "Yrs of Ed",
    "Age"
  ),
  reorder.coef = c(3, 2, 1)
)

# Make a single table of mlogit output:
texreg(
  list(mlogit1, mlogit2),
  stars = 0.05,
  digits = 3,
  booktabs = TRUE,
  dcolumn = TRUE,
  use.packages = FALSE,
  file = "tab-mlogitModels.tex",
  caption.above = TRUE,
  caption = "Logistic Results of Beliefs on the Bible",
  label = "tab:mlogit",
  custom.coef.names = c(
    "Word of God (Intercept)", 
    "Word of God: Age",
    "Book of Fables: (Intercept)",
    "Book of Fables: Age",
    "Word of God: Education Years",
    "Book fo Fables: Education Years"
  ),
  reorder.coef = c(2,5,4,6,1,3)
)

# Make a single table of ologit output:
texreg(
  list(ologit1, ologit2),
  stars = 0.05,
  digits = 3,
  booktabs = TRUE,
  dcolumn = TRUE,
  use.packages = FALSE,
  file = "tab-ologitModels.tex",
  caption.above = TRUE,
  caption = "Ordered Logistic Results of Female Roles: Mother Working Doesn't Hurt Children",
  label = "tab:ologit",
  custom.coef.names = c(
    "Yrs of Ed",
    "Cut point 1",
    "Cut point 2",
    "Cut point 3",
    "Age"
  ),
  reorder.coef = c(1, 5, 2, 3, 4),
)

# Post-estimation Analysis =====================================================
# Post-estimation: Bivariate Logistic Regression ===============================
# For logit1: Making a Classification Table:
# Generate predicted probabilities:
df$pred_pr <-
  predict(logit1, newdata = df, type = "response")
# Generate binary predictions:
df$pred_binary <- NA
df$pred_binary[df$pred_pr < 0.5] <- 0
df$pred_binary[df$pred_pr > 0.5] <- 1
# Generate classification table:
logit1_classification <- crosstab(
  df$vote08_dummy,
  df$pred_binary,
  digits = list(
    expected = 1,
    prop = 3,
    percent = 1,
    others = 3
  ),
  prop.c = TRUE,
  drop.levels = TRUE,
  dnn = c("Actual", "Predicted"),
  plot = FALSE
)
# View the classification table:
print(logit1_classification)
# Calculate the _modal_ prediction (going with the most):
1304/1788*100
# Calculate the _MODEL_ prediction (how well the logit model predicts):
(78+1257)/1788*100
# Make a publication ready version of the classification table:
xtab_logit1_classification <-
  xtable(
    logit1_classification,
    digits = 2,
    multirow = TRUE,
    hline = TRUE,
    row.labels = TRUE,
    percent = TRUE,
    caption = "Classification for Logit Model 1",
    label = "tab:logit1_classification"
  )
print (
  xtab_logit1_classification,
  include.rownames = FALSE,
  table.placement = '!htb',
  caption.placement = 'top',
  sanitize.text.function = function(x)
    x,
  file = "tab_logit1_classification.tex"
)

# For logit2: Making a Classification Table:
# Generate predicted probabilities:
df$pred_pr <-
  predict(logit2, newdata = df, type = "response")
# Generate binary predictions:
df$pred_binary <- NA
df$pred_binary[df$pred_pr < 0.5] <- 0
df$pred_binary[df$pred_pr > 0.5] <- 1
# Generate classification table:
logit2_classification <- crosstab(
  df$vote08_dummy,
  df$pred_binary,
  digits = list(
    expected = 1,
    prop = 3,
    percent = 1,
    others = 3
  ),
  prop.c = TRUE,
  drop.levels = TRUE,
  dnn = c("Actual", "Predicted"),
  plot = FALSE
)
# View the classification table:
print(logit2_classification)
# Calculate the _modal_ prediction (going with the most):
1303/1785
# Calculate the _MODEL_ prediction (how well the logit model predicts):
(0+1303)/1785*100
# Make a publication ready version of the classification table:
xtab_logit2_classification <-
  xtable(
    logit2_classification,
    digits = 2,
    multirow = TRUE,
    hline = TRUE,
    row.labels = TRUE,
    percent = TRUE,
    caption = "Classification for Logit Model 2",
    label = "tab:logit2_classification"
  )
print (
  xtab_logit2_classification,
  include.rownames = FALSE,
  table.placement = '!htb',
  caption.placement = 'top',
  sanitize.text.function = function(x)
    x,
  file = "tab_logit2_classification.tex"
)

# For logit1, generating a predicted probability graph:
# Make a temp dataframe of over the range of the IV:
temp.data <- data.frame(educ_years = 1:21)
# Calculate the predicted probability for each value of the IV:
predicted.data <-
  as.data.frame(predict(
    logit1,
    newdata = temp.data,
    type = "response",
    se.fit = TRUE
  ))
# Bind the two together:
predicted.data <- cbind(predicted.data, temp.data)
rm(temp.data) 
# Calculate the upper and lower edges of the confidence interval:
predicted.data <- mutate(predicted.data, upperFit = fit + (1.96*se.fit))
predicted.data <- mutate(predicted.data, lowerFit = fit - (1.96*se.fit))
# Make the plot:
ggplot(predicted.data, aes(x = educ_years)) +
  geom_ribbon(aes(ymin = lowerFit, ymax = upperFit), color = "gray", fill = "gray") +
  geom_line(aes(y = fit)) +
  ylim(0, 1) +
  labs(x = "Years of Education", y = "Predicted Probability") 
ggsave("fig_logit1_predprob.png", height = 3, width = 5, units = "in")

# For logit2, generating a predicted probability graph:
# Make a temp dataframe of over the range of the IV:
temp.data <- data.frame(age = 18:89)
# Calculate the predicted probability for each value of the IV:
predicted.data <-
  as.data.frame(predict(
    logit2,
    newdata = temp.data,
    type = "response",
    se.fit = TRUE
  ))
# Bind the two together:
predicted.data <- cbind(predicted.data, temp.data)
rm(temp.data) 
# Calculate the upper and lower edges of the confidence interval:
predicted.data <- mutate(predicted.data, upperFit = fit + (1.96*se.fit))
predicted.data <- mutate(predicted.data, lowerFit = fit - (1.96*se.fit))
# Make the plot:
ggplot(predicted.data, aes(x = age)) +
  geom_ribbon(aes(ymin = lowerFit, ymax = upperFit), color = "gray", fill = "gray") +
  geom_line(aes(y = fit)) +
  ylim(0, 1) +
  labs(x = "Age", y = "Predicted Probability")
ggsave("fig_logit2_predprob.png", height = 3, width = 5, units = "in")

# Post-estimation: Bivariate Multinomial Logistic Regression ===================
# For mlogit1:
temp.data <- data.frame(age = 18:89)

mlogit1.predicted.wide <-
  cbind(temp.data,
        predict(
          mlogit1,
          newdata = temp.data,
          type = "probs",
          se = TRUE
        ))

rm(temp.data)

mlogit1.predicted.long <-
  gather(mlogit1.predicted.wide,
         key = "category",
         value = "probability",
         "Inspired Word":"Book of Fables")

ggplot(mlogit1.predicted.long,
       aes(x = age, y = probability, color = category)
) +
  geom_line() +
  labs(x = "Age", y = "Predicted Probability", color = "Category") +
  scale_color_manual(values = cbbPalette) +
  ylim(0, 1)
ggsave("fig_mlogit1_predprob.png", height = 3, width = 5, units = "in")

# For mlogit2:
temp.data <- data.frame(educ_years = 1:21)

mlogit2.predicted.wide <-
  cbind(temp.data,
        predict(
          mlogit2,
          newdata = temp.data,
          type = "probs",
          se = TRUE
        ))

rm(temp.data)

mlogit2.predicted.long <-
  gather(mlogit2.predicted.wide,
         key = "category",
         value = "probability",
         "Inspired Word":"Book of Fables")

ggplot(mlogit2.predicted.long,
       aes(x = educ_years, y = probability, color = category)
) +
  geom_line() +
  labs(x = "Years of Education", y = "Predicted Probability", color = "Category") +
  scale_color_manual(values = cbbPalette) +
  ylim(0, 1)
ggsave("fig_mlogit2_predprob.png", height = 3, width = 5, units = "in")

# Post-estimation: Bivariate Ordered Logistic Regression =======================
# For ologit1:
temp.data <- data.frame(educ_years = 1:21)

ologit1.predicted.wide <-
  cbind(temp.data,
        predict(
          ologit1,
          newdata = temp.data,
          type = "probs",
          se = TRUE
        ))

rm(temp.data)

ologit1.predicted.long <-
  gather(ologit1.predicted.wide,
         key = "category",
         value = "probability",
         "Strongly Disagree":"Strongly Agree")

# I'm specifying this here because we just made the temporary dataframe 
ologit1.predicted.long$category <- 
  factor(ologit1.predicted.long$category,
         levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
         labels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
         ordered = TRUE)

ggplot(ologit1.predicted.long,
       aes(x = educ_years, y = probability, color = category)
) +
  geom_line() +
  labs(
    x = "Years of Education", 
    y = "Predicted Probability", 
    color = "Category"
  ) +
  scale_color_manual(values = cbbPalette) +
  ylim(0, 1)
ggsave("fig_ologit1_predprob.png", height = 3, width = 5, units = "in")

# For ologit2:
temp.data <- data.frame(age = 18:89)

ologit2.predicted.wide <-
  cbind(temp.data,
        predict(
          ologit2,
          newdata = temp.data,
          type = "probs",
          se = TRUE
        ))

rm(temp.data)

ologit2.predicted.long <-
  gather(ologit2.predicted.wide,
         key = "category",
         value = "probability",
         "Strongly Disagree":"Strongly Agree")

# I'm specifying this here because we just made the temporary dataframe 
ologit2.predicted.long$category <- 
  factor(ologit2.predicted.long$category,
         levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
         labels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
         ordered = TRUE)

ggplot(ologit2.predicted.long,
       aes(x = age, y = probability, color = category)
) +
  geom_line() +
  labs(
    x = "Age", 
    y = "Predicted Probability", 
    color = "Category"
  ) +
  scale_color_manual(values = cbbPalette) +
  ylim(0, 1)
ggsave("fig_ologit2_predprob.png", height = 3, width = 5, units = "in")

# End of script ================================================================

