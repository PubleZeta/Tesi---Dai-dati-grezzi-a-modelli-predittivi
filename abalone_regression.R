# istallazione librerie

#ABALONE è IL WORKSHOP 5


library(mltools)
library(data.table)
library(dplyr)
library(caret)
library(rpart)
library(Metrics)

#lettura dataset e assegnazione nomi alle colonne
data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", 
                 col.names = c('Sex', 'Lenght', 'Diameter', 'Height', 'Whole weight', 'Shucked weight',
                               'Viscera weight', 'Shell weight', 'Rings')
                 )

#aggiunta temporanea colonna fattore di conversione anelli -> età
data$Age_conversion <- 1.5    
#creazione colonna età
data$Age = rowSums(data[c("Rings", "Age_conversion")])
#rimozione colonna fattore di conversione e colonna anelli 
data <- subset(data, select = -c(Age_conversion, Rings))

View(data)
nrow(data)

unique(data$Sex)

dim(data)

colSums(!is.na(data))

#tabella riassuntiva elementi di statistica descrittiva
summary(data)


boxplot(data[,2:8])

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data[,2:9], pch = 19,  cex = 0.5,
      col = my_cols[data$Sex],
      lower.panel=NULL)

pairs(data[,2:9], pch = 19, lower.panel = NULL)

#correzione tipo (da character a factor)
data$Sex = as.factor(data$Sex)

# Matrice di correlazione
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Grafici a nuvola triangolo superiore matrice
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[data$Sex])
}
# Visualizzazione
pairs(data[,2:9], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

# istogramma età
Età <- data$Age  
hist(Età) 


library(mltools)
library(data.table)

#One Hot Encoding colonna Sex

data$Sex <- as.factor(data$Sex)
newdata <- one_hot(as.data.table(data$Sex))

data4 <- cbind(newdata,data)
data <- subset(data4, select = -c(Sex))
names(data)[names(data) == "V1_F"] <- "F"
names(data)[names(data) == "V1_I"] <- "I"
names(data)[names(data) == "V1_M"] <- "M"
head(data,18)

#regressione lineare con metodo minimi quadrati
#il dato sul sesso dell'abalone non può essere utilizzato perchè variabile non numerica, in one hot encoding
# rende la matrice singola, sunque non può essere invertita e i coefficenti non possono essere calcolati
#utilizzando l'algebra lineare

d <- data[ , 4:11]
View(d)
mod <- lm(Age ~.,   data = d)

summary(mod)

View(mod)
#per riproducibilità: selezione una particolare combinazione casuale
set.seed(1)

y = data$Age
x = data[,-11]



#80% dataset come training set e 20% come test set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train  <- data[sample, ]
test   <- data[!sample, ]

x_train <- train[,-11]
x_test <- test[,-11]
y_train <- train$Age
y_test <- test$Age


View (x_train)
fit <- rpart(Age ~ ., data = train)

plot(fit, uniform = TRUE,
     main = "Age with Decision Tree using Regression")
text(fit, use.n = TRUE, cex = .7)


predicted <- predict(fit, newdata=x_test)

#MAE rappresenta la media della differenza assoluta tra i valori effettivi e previsti nel set di dati
#Misura la media dei residui nel set di dati.
mae(y_test, predicted)
#MSE rappresenta la media della differenza al quadrato tra i valori originali e previsti nel set di dati. 
#Misura la varianza dei residui.
mse(y_test, predicted)
#RMSE è la radice quadrata del MSE.
#Misura la deviazione standard dei residui
rmse(y_test, predicted)

#valutazione tempo esecuzione algoritmi
system.time({ lm(Age ~.,   data = d) })
system.time({ rpart(Age ~ ., data = train) })
