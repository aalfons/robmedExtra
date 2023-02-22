library("robmed")
data("BSG2014")

## seed to be used for the random number generator
seed <- 20211117

## simple mediation
# set seed of the random number generator
set.seed(seed)
# The results in Alfons et al. (2021) were obtained with an
# older version of the random number generator.  To reproduce
# those results, uncomment the two lines below.
# RNGversion("3.5.3")
# set.seed(20150601)
# perform mediation analysis
boot_simple <- test_mediation(TeamCommitment ~
                                m(TaskConflict) +
                                ValueDiversity,
                              data = BSG2014)
summary(boot_simple)
to_latex(boot_simple)


## serial multiple mediators
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_serial <- test_mediation(TeamScore ~
                                serial_m(TaskConflict,
                                         TeamCommitment) +
                                ValueDiversity +
                                covariates(AgeDiversity),
                              data = BSG2014)
summary(boot_serial)
to_latex(boot_serial)


## parallel multiple mediators and control variables
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_parallel <- test_mediation(TeamPerformance ~
                                  parallel_m(ProceduralJustice,
                                             InteractionalJustice) +
                                  SharedLeadership +
                                  covariates(AgeDiversity, GenderDiversity),
                                data = BSG2014)
summary(boot_parallel)
to_latex(boot_parallel)


## parallel multiple mediators and control variables
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
boot_multiple <- test_mediation(TeamPerformance ~
                                  m(ProceduralJustice) +
                                  SharedLeadership +
                                  AgeDiversity +
                                  GenderDiversity,
                                data = BSG2014)
summary(boot_multiple)
to_latex(boot_multiple)


# bootstrap test with covariance fit
cov_boot <- test_mediation(TeamCommitment ~
                             m(TaskConflict) +
                             ValueDiversity,
                           data = BSG2014,
                           method = "covariance")
summary(cov_boot)
to_latex(cov_boot)


# sobel test with regression fit
reg_sobel <- test_mediation(TeamCommitment ~
                               m(TaskConflict) +
                               ValueDiversity,
                             data = BSG2014,
                             test = "sobel")
summary(reg_sobel)
to_latex(reg_sobel)


# sobel test with coavariance fit
cov_sobel <- test_mediation(TeamCommitment ~
                              m(TaskConflict) +
                              ValueDiversity,
                            data = BSG2014,
                            test = "sobel",
                            method = "covariance")
summary(cov_sobel)
to_latex(cov_sobel)
