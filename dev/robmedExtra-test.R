library("robmedExtra")
data("BSG2014")

## seed to be used for the random number generator
seed <- 20211117

## simple mediation
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_simple <- test_mediation(BSG2014,
                              x = "ValueDiversity",
                              y = "TeamCommitment",
                              m = "TaskConflict")
summary(boot_simple)
to_latex(boot_simple)
foo <- to_flextable(boot_simple)
foo
to_docx(foo, file = "dev/test.docx")


## serial multiple mediators
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_serial <- test_mediation(BSG2014,
                              x = "ValueDiversity",
                              y = "TeamScore",
                              m = c("TaskConflict", "TeamCommitment"),
                              model = "serial")
summary(boot_serial)
to_latex(boot_serial)
to_flextable(boot_serial)


## parallel multiple mediators and control variables
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_parallel <- test_mediation(BSG2014,
                                x = "SharedLeadership",
                                y = "TeamPerformance",
                                m = c("ProceduralJustice",
                                      "InteractionalJustice"),
                                covariates = c("AgeDiversity",
                                               "GenderDiversity"))
summary(boot_parallel)
to_latex(boot_parallel)
to_flextable(boot_parallel)


## multiple independent variables
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_multiple <- test_mediation(BSG2014,
                                x = c("SharedLeadership",
                                      "AgeDiversity",
                                      "GenderDiversity"),
                                y = "TeamPerformance",
                                m = "ProceduralJustice")
summary(boot_multiple)
to_latex(boot_multiple)
to_flextable(boot_multiple)


## multiple independent variables and parallel multiple mediators
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_mult_par <- test_mediation(BSG2014,
                                x = c("SharedLeadership",
                                      "AgeDiversity",
                                      "GenderDiversity"),
                                y = "TeamPerformance",
                                m = c("ProceduralJustice",
                                      "InteractionalJustice"))
summary(boot_mult_par)
to_latex(boot_mult_par)
to_flextable(boot_mult_par)


## multiple independent variables and serial multiple mediators
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_mult_ser <- test_mediation(BSG2014,
                                x = c("ValueDiversity", "AgeDiversity"),
                                y = "TeamScore",
                                m = c("TaskConflict", "TeamCommitment"),
                                model = "serial")
summary(boot_mult_ser)
to_latex(boot_mult_ser)
to_flextable(boot_mult_ser)


## bootstrap test with covariance fit
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
cov_boot <- test_mediation(BSG2014,
                           x = "ValueDiversity",
                           y = "TeamCommitment",
                           m = "TaskConflict",
                           method = "covariance")
summary(cov_boot)
to_latex(cov_boot)
to_flextable(cov_boot)


# sobel test with regression fit
reg_sobel <- test_mediation(BSG2014,
                            x = "ValueDiversity",
                            y = "TeamCommitment",
                            m = "TaskConflict",
                            test = "sobel")
summary(reg_sobel)
to_latex(reg_sobel)
bar <- to_flextable(reg_sobel)
bar
# to_docx(bar, file = "dev/test.docx")


# sobel test with coavariance fit
cov_sobel <- test_mediation(BSG2014,
                            x = "ValueDiversity",
                            y = "TeamCommitment",
                            m = "TaskConflict",
                            test = "sobel",
                            method = "covariance")
summary(cov_sobel)
to_latex(cov_sobel)
to_flextable(cov_sobel)


## multiple objects in one table

library("robmedExtra")
data("BSG2014")

## seed to be used for the random number generator
seed <- 20211117

# apply OLS Sobel test
set.seed(seed)
ols_sobel <- test_mediation(BSG2014,
                            x = "ValueDiversity",
                            y = "TeamCommitment",
                            m = "TaskConflict",
                            test = "sobel",
                            method = "regression",
                            robust = FALSE)

# apply OLS bootstrap
set.seed(seed)
ols_boot <- test_mediation(BSG2014,
                           x = "ValueDiversity",
                           y = "TeamCommitment",
                           m = "TaskConflict",
                           test = "boot",
                           method = "regression",
                           robust = FALSE)

# apply winsorized bootstrap
set.seed(seed)
winsorized_boot <- test_mediation(BSG2014,
                                  x = "ValueDiversity",
                                  y = "TeamCommitment",
                                  m = "TaskConflict",
                                  test = "boot",
                                  method = "covariance",
                                  robust = TRUE)

# apply ROBMED
set.seed(seed)
robust_boot <- test_mediation(BSG2014,
                              x = "ValueDiversity",
                              y = "TeamCommitment",
                              m = "TaskConflict",
                              test = "boot",
                              method = "regression",
                              robust = TRUE)

# combine into list
boot_list <- list(ols_sobel, ols_boot, winsorized_boot, robust_boot)
to_flextable(boot_list, orientation = "portrait")
to_flextable(boot_list, orientation = "landscape")
