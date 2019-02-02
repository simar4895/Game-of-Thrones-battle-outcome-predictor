# Load libraries

library(tidyverse)
library(glmulti)
library(car)
library(caret)

# Read the data

battle <- read_csv("battles.csv")

# Data cleaning

battle[!battle$attacker_1 %in% c("Stark","Greyjoy","Lannister","Baratheon","Bolton","Frey"),"attacker_1"] <- "Others"
battle$attacker_1 <- as.factor(battle$attacker_1)
battle[!battle$defender_1 %in% c("Stark","Greyjoy","Lannister","Baratheon","Tully"),"defender_1"] <- "Others"
battle$defender_1 <- as.factor(battle$defender_1)
battle$attacker_outcome <- as.factor(battle$attacker_outcome)
battle$attacker_outcome <- as.logical(battle$attacker_outcome)
battle$attacker_outcome <- as.numeric(battle$attacker_outcome)
battle$battle_type <- as.factor(battle$battle_type)
battle$attacker_collab = 3 - is.na(battle$attacker_4) - is.na(battle$attacker_3) - is.na(battle$attacker_2)
battle$defender_collab = 3 - is.na(battle$defender_4) - is.na(battle$defender_3) - is.na(battle$defender_2)
colSums(is.na(battle))
battle <- battle %>% filter( ! is.na(attacker_outcome),! is.na(defender_1),! is.na(summer))
battle <- battle %>% select(attacker_1,defender_1,attacker_outcome,battle_type,major_capture,major_death,summer,attacker_collab,defender_collab)
colSums(is.na(battle))

# looking at the cleaned data

head(battle)
summary(battle)

# Exploratory Plots

a <- ggplot(data = battle,mapping = aes(x = attacker_1,fill = attacker_outcome))
a <- a + geom_bar(stat = "count", colour = "black",position = "stack")
a + xlab("Great House") + ylab("Number of attacks") + ggtitle("Attacks by major houses") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "Outcome") 
b <- ggplot(data = battle,mapping = aes(x = defender_1,fill = forcats::fct_rev(attacker_outcome)))
b <- b + geom_bar(stat = "count", colour = "black",position = "stack")
b + xlab("Great House") + ylab("Number of attacks defended") + ggtitle("Attacks defended by major houses") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "Outcome", labels = c("failed", "successful"))
battle_type_summary <- battle %>% group_by(battle_type) %>% summarise(n = n())
battle_type_summary
pie(battle_type_summary$n, labels = battle_type_summary$battle_type, main = "Most fought Battle Types",col = c("#8dd3c7","#ffffb3","#bebada","#fb8072"))
summer_summary <- battle %>% group_by(summer,attacker_outcome) %>% summarise(n = n())
summer_summary$summer <- factor(summer_summary$summer,levels = c(0,1),labels = c("Winter","Summer"))
summer_summary$percent <- summer_summary$n/nrow(battle)*100
summer_summary
c <- ggplot(data = summer_summary,mapping = aes(x = summer,y = percent,fill = attacker_outcome))
c <- c + geom_bar(stat = "identity", colour = "black",position = "dodge")
c + xlab("Season") + ylab("Percentage") + ggtitle("Better attacking season") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "Outcome")
war_effects <- battle %>% group_by(attacker_1) %>% summarise(major_captures = sum(major_capture),major_deaths = sum(major_death))
war_effects <- war_effects %>% gather(c('major_captures','major_deaths'),key = "effect",value = "count")
war_effects
d <- ggplot(data = war_effects,mapping = aes(x = attacker_1, y = count,fill = effect))
d <- d + geom_bar(stat = "identity",colour = "black",position = "dodge")
d + xlab("Major attacking house") + ylab("Count") + ggtitle("Major captures and deaths",subtitle = "by attacking house") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) + scale_fill_manual(values = c("#ffffb3","#fb8072"),name = "Effects",labels = c("major captures","major deaths")) 
allies <- battle %>% group_by(attacker_1) %>% summarise(allies = sum(attacker_collab))
allies
e <- ggplot(data = allies,mapping = aes(x = attacker_1,y = allies))
e <- e + geom_bar(stat = "identity",fill = "#FFC125",colour = "black",position = "dodge")
e + xlab("Major attacking house") + ylab("Number of allies in battles") + ggtitle("Collaborations for attack") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

# Best model search

search.1.aic <-glmulti(y = attacker_outcome ~ ., data = battle, fitfunction = "glm", level =1, method = "g", crit = "aic", family = binomial(link="logit"))
search.1.aicc <-glmulti(y = attacker_outcome ~ ., data = battle, fitfunction = "glm", level =1, method = "g", crit = "aicc", family = binomial(link="logit"))
search.1.bic <-glmulti(y = attacker_outcome ~ ., data = battle, fitfunction = "glm", level =1, method = "g", crit = "bic", family = binomial(link="logit"))

search.2.aic <-glmulti(y = attacker_outcome ~ ., data = battle, fitfunction = "glm", level =2, method = "g", crit = "aic", family = binomial(link="logit"),marginality = TRUE)
search.2.aicc <-glmulti(y = attacker_outcome ~ ., data = battle, fitfunction = "glm", level =2, method = "g", crit = "aicc", family = binomial(link="logit"),marginality = TRUE)
search.2.bic <-glmulti(y = attacker_outcome ~ ., data = battle, fitfunction = "glm", level =2, method = "g", crit = "bic", family = binomial(link="logit"),marginality = TRUE)

mod1 <- as.character(eval(weightable(search.1.aic)[1,1]))
mod2 <- as.character(eval(weightable(search.1.aicc)[1,1]))
mod3 <- as.character(eval(weightable(search.1.bic)[1,1]))
mod4 <- as.character(eval(weightable(search.2.aic)[1,1]))
mod5 <- as.character(eval(weightable(search.2.aicc)[1,1]))
mod6 <- as.character(eval(weightable(search.2.bic)[1,1]))

mod.fit1 <- glm(mod1,data = battle,family = binomial(link="logit"))
mod.fit2 <- glm(mod2,data = battle,family = binomial(link="logit"))
mod.fit3 <- glm(mod3,data = battle,family = binomial(link="logit"))
mod.fit4 <- glm(mod4,data = battle,family = binomial(link="logit"))
mod.fit5 <- glm(mod5,data = battle,family = binomial(link="logit"))
mod.fit6 <- glm(mod6,data = battle,family = binomial(link="logit"))

finalmods <- data.frame("model" = c(mod1,mod2,mod3,mod4,mod5,mod6),
                        "aic" = c(aic(mod.fit1),aic(mod.fit2),aic(mod.fit3),aic(mod.fit4),aic(mod.fit5),aic(mod.fit6)),
                        "aicc" = c(aicc(mod.fit1),aicc(mod.fit2),aicc(mod.fit3),aicc(mod.fit4),aicc(mod.fit5),aicc(mod.fit6)),
                        "bic" = c(bic(mod.fit1),bic(mod.fit2),bic(mod.fit3),bic(mod.fit4),bic(mod.fit5),bic(mod.fit6)),
                        "residual deviance" = c(deviance(mod.fit1),deviance(mod.fit2),deviance(mod.fit3),deviance(mod.fit4),deviance(mod.fit5),deviance(mod.fit6)),
                        "degrees of freedom" = c(mod.fit1$df.residual,mod.fit2$df.residual,mod.fit3$df.residual,mod.fit4$df.residual,mod.fit5$df.residual,mod.fit6$df.residual),
                        "criteria" = c("aic","aicc","bic","aic","aicc","bic"),
                        "predictors" = c("main effects","main effects","main effects","main effects + interactions","main effects + interactions","main effects + interactions"))
finalmods
paste("Selected model is ", mod1)
summary(mod.fit1)
Anova(mod.fit1)

# Cross Validation

Train = createDataPartition(battle$attacker_outcome,p=0.75,list = FALSE)
training <- battle[Train,]
testing <- battle[-Train,]
mod.fit.train <- glm(mod1,data = training,family = binomial(link="logit"))
p1 <- round(predict(mod.fit.train,newdata = testing,type = "response"),3)
p <- as.data.frame(p1)
test <- as.data.frame((testing$attacker_outcome))
y <- cbind(test,p)
colnames(y) <- c("Outcome","Prediction")
y

# Residual plots

pi.hat <- round(predict(mod.fit1, newdata = battle, type = "response"),3)
p.res <- round(residuals(mod.fit1, type = "pearson"),3) # regular pearson residuals
s.res <- round(rstandard(mod.fit1, type = "pearson"),3) # standardized
lin.pred <- round(predict(mod.fit1, type = "link"),3)
battle <- data.frame(battle, pi.hat, p.res, s.res, lin.pred)

plot(x = battle$lin.pred, y = battle$s.res, xlab = "Linear predictor", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Linear predictor")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ lin.pred, data = battle)

plot(x = battle$pi.hat, y = battle$s.res, xlab = "Estimated probability of success", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n pi.hat")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ pi.hat, data = battle)

# Goodness of fit

rdev <- mod.fit1$deviance 
rdev
dfr <- mod.fit1$df.residual 
dfr

ddf <- rdev/dfr 
ddf
thresh2 <- 1 + 2*sqrt(2/dfr) 
thresh3 <- 1 + 3*sqrt(2/dfr) 
round(c(rdev, dfr, ddf, thresh2, thresh3),3)
