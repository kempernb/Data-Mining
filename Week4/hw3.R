spam.base <- read.csv("spambase.csv")
dim(spam.base)
t(t(names(spam.base)))
spam.base <- rename(spam.base, semicolon=C.)
spam.base <- rename(spam.base, parentheses=C..1)
spam.base <- rename(spam.base, bracket=C..2)
spam.base <- rename(spam.base, exclamation=C..3)
spam.base <- rename(spam.base, dollar.sign=C..4)
spam.base <- rename(spam.base, pound.sign=C..5)

str(spam.base)
summary(spam.base)

aggregate(.~Spam, data = spam.base,mean)

set.seed(1)
train.index <- sample(nrow(spam.base), nrow(spam.base)*0.6)
spam.train <- spam.base[train.index,]
spam.valid <- spam.base[-train.index,]

lmspambase = glm(Spam ~ ., data=spam.train)
summary(lmspambase)

# Metrics
lmspambase$deviance
AIC(lmspambase)
BIC(lmspambase)


p.seq <- seq(from = 0.01, to = 1, by = 0.01)

# use predict() with type = "response" to compute predicted probabilities
logit.reg.pred <- predict(lmspambase, spam.valid, type = "response")

confusionMatrix(as.factor(ifelse(logit.reg.pred >= 0.75, "1", "0")), as.factor(spam.valid$Spam), 
                positive = "1")

# variable selection
lmspambase.null <- glm(Spam ~ 1, data = spam.train)
lmspambase.full <- glm(Spam ~ ., data = spam.train)

#forward Selection
lmspambase.fwd <- step(lmspambase.null, scope = list(lower = lmspambase.null, upper = lmspambase.full), 
                 direction = "forward")
summary(lmspambase.fwd)
lmspambase.fwd$deviance
AIC(lmspambase.fwd)
BIC(lmspambase.fwd)

#stepwise Selection
lmspambase.step <- step(lmspambase.null, scope = list(lower = lmspambase.null, upper = lmspambase.full),
                  direction = "both")
summary(lmspambase.step)
# Metrics
lmspambase.step$deviance
AIC(lmspambase.step)
BIC(lmspambase.step)