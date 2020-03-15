library(boot)
library(ggplot2)
set.seed(42)

covid <- read.csv(file=paste0(getwd(),"/data.csv"))

day_max <- 20
day_max_curr <- 13
add_period <- day_max-day_max_curr

m1_pred <- as.data.frame(c(covid$day, seq(day_max_curr+1,day_max,1)))
colnames(m1_pred) <- 'day'

m1 <- nls(cases ~ exp(a + b * day), data=covid, start=list(a=0, b=0))
m1_pred$cases <- predict(m1, newdata=m1_pred)


myboot <- boot(covid, function(data, idx, model, newdata) {
  data$cases <- fitted(model) + residuals(model)[idx]
  tryCatch(predict(nls(cases ~ exp(a + b * day), data=data, start = list(a=0, b=0)), newdata=newdata),
           error = function(e) newdata$day * NA)
}, model=m1, newdata=m1_pred, R = 1e4)

CI <- t(sapply(seq_len(nrow(m1_pred)), function(i) boot.ci(myboot, type = "bca", index = i)$bca[4:5]))
colnames(CI) <- c("lwr", "upr")
m1_pred <- cbind(m1_pred, CI) # see this table if want to see the number
m1_pred

ggplot(covid, aes(day, cases)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), data=m1_pred, color='NA', fill = "grey") +
  geom_line(mapping = aes(day, cases), data=m1_pred) +
  geom_point() 

