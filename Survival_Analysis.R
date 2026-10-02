# Part 1
# a)
set.seed(12345)
library(survival)
two_drug_times <- c(85, 32, 38, 45, 4, 84, 49, 180, 87, 75, 
                    102, 39, 12, 11, 80, 35, 6) 
two_drug_event <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 
                    1, 1, 1, 1) 
triple_drug_times <- c(22, 2, 48, 85, 160, 238, 56, 94, 51, 
                       12, 171, 80, 180, 4, 90, 180, 3) 
triple_drug_event <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 
                       1, 1, 1, 0, 1)
# 1 = the patient relapsing obsevation 
# 0 = right-censored observation

time <- c(two_drug_times, triple_drug_times) 
status <- c(two_drug_event, triple_drug_event) 
treatment <- factor(c(rep("Two-drug", length(two_drug_times)), 
                      rep("Triple-drug", length(triple_drug_times))))
data <- data.frame(time, status, treatment) 
KM_estimates <- survfit(Surv(time, status) ~ treatment, data = data)
plot(KM_estimates, conf.int = FALSE, 
     col = c("blue", "blue"), lty = c(2, 1), 
     xlab = "Time (days)", ylab = "Survival function", 
     main = "Kaplan–Meier Survival Curves")
legend("topright", lty = c(2, 1), legend = c("Triple Drug", "Two Drug"), cex = 0.5)

# b)
summary(KM_estimates, times = 120)

# c)
cox_model <- coxph(Surv(time, status) ~ treatment, data = data)
cox_surv <- survfit(cox_model, 
                  newdata = data.frame(treatment = c("Two-drug", 
                                                     "Triple-drug")))
summary(cox_model)
levels(treatment)
plot(cox_surv,
     xlab = "Time (days)",
     ylab = "Survival function",
     lty = c(1, 2),
     col = c("red", "red"))
lines(KM_estimates, lty = c(2, 1), col = c("blue", "blue"))
legend("topright",
       legend = c("Two-drug__Cox", "Triple-drug__Cox",
                  "Two-drug__KM",  "Triple-drug__KM"),
       lty = c(1, 2, 1, 2),
       col = c("red", "red", "blue", "blue"),
       cex = 0.5)

## d)
logmlog <- function(x){log(-log(x))} 
cox_two_drug <- coxph(Surv(time, status) ~ treatment, data = data[data$treatment == "Two-drug", ]) 
cox_triple_drug <- coxph(Surv(time, status) ~ treatment, 
                         data = data[data$treatment == "Triple-drug", ]) 
S_two_drug <- survfit(cox_two_drug) 
S_triple_drug <- survfit(cox_triple_drug) 
plot(S_two_drug, fun = logmlog, conf.int = FALSE, xlab = "Time (days)", ylab = "log (-log (S (t)))", xlim = range(data$time), lty = 1) 
lines(S_triple_drug, fun = logmlog, conf.int = FALSE, lty = 2) 
legend("bottomright", legend = c("Two-drug", "Triple-drug"), lty = c(1, 2), cex = 0.8)

# Part 2  
#a)
set.seed(12345)
DRS_data <- read.csv("C:/Users/huyma/Downloads/DRS.csv")
DRS_cox_model <- coxph(Surv(time, status) ~ treatment + age + riskgp, data = DRS_data) 
summary(DRS_cox_model) 
DRS_preferred_cox_model <- coxph(Surv(time, status) ~ treatment + riskgp, data = DRS_data) 
summary(DRS_preferred_cox_model)

# b) 
event_times <- DRS_data$time[DRS_data$status == 1] 
any(duplicated(event_times)) 
cox_exact <- coxph(Surv(time, status) ~ treatment + riskgp, data = DRS_data, ties = "exact") 
summary(cox_exact) 
cox_efron <- coxph(Surv(time, status) ~ treatment + riskgp, data = DRS_data, ties = "efron") 
summary(cox_efron) 

# c) 
DRS_data$treatment <- factor(DRS_data$treatment)
DRS_AFT_model <- survreg(Surv(time, status) ~ treatment + riskgp, data = DRS_data, dist = "weibull")
coef(DRS_AFT_model)
confint(DRS_AFT_model) 
km_estimates <- survfit(Surv(time, status) ~ 1, data = DRS_data)
log_time <- log(km_estimates$time)
log_log_surv <- log(-log(km_estimates$surv))
weib_lm <- lm(log_log_surv ~ log_time)
plot(log_time, log_log_surv,
     xlab = "log(time)",
     ylab = "log (- log (S (t)))")
abline(weib_lm, col = "blue")


















