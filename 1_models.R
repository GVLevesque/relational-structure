# SCRIPT 1: Cox models

# Load packages. Install if needed.
library(rem)
library(survival)
library(knitr)
library(dplyr)
library(tibble)
library(ggplot2)
library(dplyr)

# Load data
sdtrem <- read.csv("sdtrem.csv")

# Model 1
stepsurv <- survSplit(Surv(eventTime, eventDummy) ~ ., data = sdtrem,
                      cut = c((1961-1924), (1970-1924)), episode = "tgroup", id = "id")
cox_step_6170 <- coxph(formula = Surv(eventTime, eventDummy) ~ 
                         science:strata(tgroup) +
                         regex:strata(tgroup) +
                         outdegree_scale + tt(outdegree_scale) + 
                         inertia + tt(inertia) + 
                         fourhl5 + tt(fourhl5) + 
                         s_lag_news_count + tt(s_lag_news_count) + 
                         prop, data = stepsurv)
summary(cox_step_6170)

# Model 2
stepsurv <- survSplit(Surv(eventTime, eventDummy) ~ ., data = sdtrem,
                      cut = c((1961-1924), (1965-1924), (1970-1924)), 
                      episode = "tgroup", id = "id")
cox_step_616570 <- coxph(formula = Surv(eventTime, eventDummy) ~ 
                           science:strata(tgroup) +
                           regex:strata(tgroup) +
                           outdegree_scale + tt(outdegree_scale) + 
                           inertia + tt(inertia) + 
                           fourhl5 + tt(fourhl5) + 
                           s_lag_news_count + tt(s_lag_news_count) + 
                           prop, data = stepsurv)
summary(cox_step_616570)

# Model 3
stepsurv <- survSplit(Surv(eventTime, eventDummy) ~ ., data = sdtrem,
                      cut = c((1965-1924), (1970-1924)), episode = "tgroup", id = "id")
cox_step_6570 <- coxph(formula = Surv(eventTime, eventDummy) ~ 
                         science:strata(tgroup) +
                         regex:strata(tgroup) +
                         outdegree_scale + tt(outdegree_scale) + 
                         inertia + tt(inertia) + 
                         fourhl5 + tt(fourhl5) + 
                         s_lag_news_count + tt(s_lag_news_count) + 
                         prop, data = stepsurv)
summary(cox_step_6570)


# Model 4
cox_step_617078 <- coxph(formula = Surv(eventTime, eventDummy) ~ 
                           science:strata(tgroup) +
                           regex:strata(tgroup) +
                           outdegree_scale + tt(outdegree_scale) + 
                           inertia + tt(inertia) + 
                           fourhl5 + tt(fourhl5) + 
                           s_lag_news_count + tt(s_lag_news_count) + 
                           prop, data = stepsurv)

summary(cox_step_617078)

# Model 5
stepsurv <- survSplit(Surv(eventTime, eventDummy) ~ ., data = sdtrem,
                      cut = c((1961-1924), (1965-1924), (1970-1924), (1978-1924)), 
                      episode = "tgroup", id = "id")
cox_step_61657078 <- coxph(formula = Surv(eventTime, eventDummy) ~ 
                             science:strata(tgroup) +
                             regex:strata(tgroup) +
                             outdegree_scale + tt(outdegree_scale) + 
                             inertia + tt(inertia) + 
                             fourhl5 + tt(fourhl5) + 
                             s_lag_news_count + tt(s_lag_news_count) + 
                             prop, data = stepsurv)
summary(cox_step_61657078)

stepsurv <- survSplit(Surv(eventTime, eventDummy) ~ ., data = sdtrem,
                      cut = c((1961-1924), (1970-1924), (1978-1924)), 
                      episode = "tgroup", id = "id")

