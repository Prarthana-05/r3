options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("BiocManager")
BiocManager::install("EBImage")
install.packages("keras")

library(keras)
library(EBImage)

install_keras()

setwd('C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_4')
pics <- c('P1.jpeg','P2.jpeg','P3.jpeg','P4.jpeg','P5.jpeg','P6.jpeg','C1.jpeg','C2.jpeg','C3.jpeg','C4.jpeg','C5.jpeg','C6.jpeg')

mypic <- list()
for(i in 1:12) {mypic[[i]] <- readImage(pics[i])}

print(mypic[[1]])

display(mypic[[1]])
summary(mypic[[8]])
hist(mypic[[8]])
str(mypic)

#resize
for(i in 1:12) {mypic[[i]] <- resize(mypic[[i]], 28, 28)}

#reshape
for (i in 1:12) {mypic[[i]] <- array_reshape(mypic[[i]], c(28,28,3))}

trainx <- NULL
for(i in 7:11) {trainx <- rbind(trainx, mypic[[i]])}
str(trainx)
testx <- rbind(mypic[[6]],mypic[[12]])
trainy <- c(0,0,0,0,0,1,1,1,1,1)
testy <- c(0,1)

# one hot encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
trainLabels

#model
model <- keras_model_sequential()
model %>%
        layer_dense(units=256,activation='relu',input_shape = c(2352)) %>%
        layer_dense(units=128,activation='relu') %>%
        layer_dense(units=2,activation='softmax')
summary(model)

#compile
model %>%
          compile(loss='binary_crossentropy',optimizer = optimizer_rmsprop(),metrics = c('accuracy'))

#fit model
history <- model %>%
          fit(trainx,trainLabels,epochs = 30,batch_size = 32,validation_split = 0.2)

plot(history)

# evaluation and prediction - train data
model %>% evaluate(trainx,trainLabels)
# Predict probabilities, get the index of the highest value, and convert to an R vector
pred <- model %>% predict(trainx) %>% k_argmax() %>% as.numeric()
table(Predicted = pred, Actual = trainy)
prob <- model %>% predict(trainx)
cbind(prob, Prected = pred, Actual= trainy)


#evaluation and prediction - test data
model %>% evaluate(testx,testLabels)
pred <- model %>% predict(testx) %>% k_argmax() %>% as.numeric()
table(Predicted = pred,Actual=testy)