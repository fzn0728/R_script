library(caret)
library(e1071)
data(GermanCredit)

Train <- createDataPartition(GermanCredit$Class, p=0.6, list=FALSE)
training <-GermanCredit[Train,]
testing <- GermanCredit[-Train,]

mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + 
                   CreditHistory.Critical, data=training, method='glm', family='binomial')
exp(coef(mod_fit$finalModel))
predict(mod_fit,newdata=testing,type='prob')

mod_fit_one <- glm(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + CreditHistory.Critical, data=training, family='binomial')
mod_fit_wto <- glm(Class ~ Age + ForeignWorker, data=training, family='binomial')
