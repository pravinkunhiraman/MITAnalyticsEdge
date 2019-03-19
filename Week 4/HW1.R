
gerber = read.csv("gerber.csv")
str(gerber)
table(gerber$voting)
108696/(108696+235388)

civic = subset(gerber, gerber$civicduty ==1)
hawthorne = subset(gerber, gerber$hawthorne ==1)
self = subset(gerber, gerber$self ==1)
neighbors = subset(gerber, gerber$neighbors ==1)

table(civic$voting)
12021/(12021+26197) # 0.3145377

table(hawthorne$voting) 
12316/(12316+25888) # 0.3223746

table(neighbors$voting)
14438/(14438+23763) # 0.3779482

table(self$voting) # 0.3451515
13191/(13191+25027)

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)

LogModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(LogModel)

LogPredict = predict(LogModel, data=gerber, type="response")
table(gerber$voting, LogPredict > 0.3)
(51966+134513)/(51966+134513+100875+56730) # 0.5419578

table(gerber$voting, LogPredict > 0.5)
235388/(235388+108696) #0.6841004

table(gerber$voting)
235388/(108696+235388) # Baseline model: 0.6841004

library(ROCR)
ROCRpred = prediction(LogPredict, gerber$voting)
as.numeric(performance(ROCRpred,"auc")@y.values) # AUC = 0.5308461

library(rpart)
library(rpart.plot)
CARTTreeModel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber,method = "class")
RegTreeModel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

prp(RegTreeModel)

RegTreeemodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(RegTreeemodel2)

RegTreeemodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(RegTreeemodel3)

RegTreeemodel4 = rpart(voting ~ control, data = gerber, cp=0.0)
prp(RegTreeemodel4, digits = 6)
abs(0.296638-0.34)

RegTreeemodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(RegTreeemodel5,digits=6)

abs(0.290456-0.334176)
abs(0.302795-0.345818)

LogModel2 = glm(voting ~ sex + control, data=gerber, family=binomial)
summary(LogModel2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
Possibilities
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.290456-0.2908065)

LogModel3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel3)

predict(LogModel3, newdata=Possibilities, type="response")
abs(0.290456-0.2904558)
