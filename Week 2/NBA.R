NBA = read.csv("NBA_train.csv")
str(NBA)

table(NBA$W,NBA$Playoffs)

NBA$PTDiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTDiff,NBA$W)

WinsReg = lm(W~PTDiff,data = NBA)
summary(WinsReg)

(41-42)/0.0326 #30.67

PointsReg = lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK, data = NBA)
summary(PointsReg)

SSE = sum(PointsReg$residuals^2)
SSE

RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)

PointsReg2 = lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+STL+BLK, data = NBA)
summary(PointsReg2)

PointsReg3 = lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL+BLK, data = NBA)
summary(PointsReg3)

PointsReg4 = lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL, data = NBA)
summary(PointsReg4)

SSE4 = sum(PointsReg4$residuals^2)
SSE4

RMSE4 = sqrt(SSE4/nrow(NBA))
RMSE4
mean(NBA$PTS)

NBA_test = read.csv("NBA_test.csv")

PointsPrediction = predict(PointsReg4, NBA_test)
PointsPrediction
SSETest = sum((PointsPrediction - NBA_test$PTS)^2)
SSTTest = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSETest/SSTTest
R2

NBA_test
RMSE = sqrt(SSETest/nrow(NBA_test))
RMSE

NBA_test$PTDiff = NBA_test$PTS - NBA_test$oppPTS

WinsPrediction = predict(WinsReg, NBA_test)
WinsPrediction

oppPTSModel = lm(oppPTS ~ FGA+X2PA+FTA+DRB+STL+BLK+TOV, data=NBA)
summary(oppPTSModel)
NBA$X3PA

#oppPointsPrediction = 
  
1.07^52
