
# Cox regression in R

## Load survival package
library(survival)
## List datasets in survival package
data(package = "survival")

## Load lung data
data(lung)

## Show first 6 rows
 head(lung)

# NCCTG Lung Cancer Data
# 
# Description:
#   Survival in patients with advanced lung cancer from the North
# Central Cancer Treatment Group.  Performance scores rate how well
# the patient can perform usual daily activities.
# 
# inst:       Institution code
# time:       Survival time in days
# status:     censoring status 1=censored, 2=dead
# age:        Age in years
# sex:        Male=1 Female=2
# ph.ecog:    ECOG performance score (0=good 5=dead)
# ph.karno:   Karnofsky performance score (bad=0-good=100) rated by physician
# pat.karno:  Karnofsky performance score as rated by patient
# meal.cal:   Calories consumed at meals
# wt.loss:    Weight loss in last six months

## Add survival object. status == 2 is death
lung$SurvObj <- with(lung, Surv(time, status == 2))

## Check data
head(lung)

## Kaplan-Meier estimator. The "log-log" confidence interval is preferred.
km.as.one <- survfit(SurvObj ~ 1, data = lung, conf.type = "log-log")
km.by.sex <- survfit(SurvObj ~ sex, data = lung, conf.type = "log-log")

## Show object
km.as.one

## See survival estimates at given time (lots of outputs)
summary(km.as.one)
summary(km.by.sex)

## Plotting without any specification (# this is a RISK curve!!! MORTALITY RISK, based on the number
# of survivals and deaths)
plot(km.as.one)

plot(km.by.sex)


###########################################################################
# Cox REGRESSION using coxph (COX PROPORTIONAL HAZARD)----------------------

## Fit Cox regression: age, sex, Karnofsky performance score, wt loss
res.cox1 <- coxph(SurvObj ~ age + sex + ph.karno + wt.loss, data =  lung)
res.cox1

## Check for violation of proportional hazard (constant HR over TIME)
(res.zph1 <- cox.zph(res.cox1))

## Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve.
plot(res.zph1)


##############################################################################
# Load libs
library(survival)
library(rms)

# Regular survival
survobj <- with(lung, Surv(time,status))

# Prepare the variables
lung$sex <- factor(lung$sex, levels=1:2, labels=c("Male", "Female"))

# The rms survival
ddist <- datadist(lung)
options(datadist="ddist")
rms_surv_fit <- cph(survobj~rcs(age, 4)+sex, data=lung, x=T, y=T)

termplot(rms_surv_fit, se=T, rug.type="density", rug=T, density.proportion=.05,
         se.type="polygon", yscale="exponential", log="y")

