
# data setup --------------------


#setting the seed to obtain the same results
set.seed(2020)

#creating the dataset
x<- data.frame(event=c(rbind(rbinom(100,1,0.4), rbinom(100,1,0.3))),
                treatment=c(rep(0,100), rep(1,100)))



# absolute risks -----------------


#inspecting the frequencies
addmargins(table(x$event))

#fitting a logit model
binom_logit <- glm(event~1, data=x, family=binomial(link = "logit"))

#predicting the link function
binom_logit_link <- predict(binom_logit, type="link")

#exponentiating the link
exp(binom_logit_link)

#fitting a log model
binom_log <- glm(event~1, data=x, family=binomial(link = "log"))

#predicting the link function
binom_log_link <- predict(binom_log, type="link")

#exponentiating the link
exp(binom_log_link)

# relative risks -----------------


#frequencies of events by treatment
with(x, addmargins(table(treatment, event)))

#fitting a logit model
binom_trt_logit <- glm(event~treatment, data=x, family=binomial(link = "logit"))

#extract and exponentiate coefficients
exp(coef(binom_trt_logit))

#fitting a log model
binom_trt_log <- glm(event~treatment, data=x, family=binomial(link = "log"))

#extract and exponentiate coefficients
exp(coef(binom_trt_log))

#predicting the link function
binom_trt_log_link <- predict(binom_trt_log, type="link")

binom_trt_logit_link <- predict(binom_trt_logit, type="link")


