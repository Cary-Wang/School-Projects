library(rpart)

setwd("C://Users/Cary Wang/Documents/NYU/Analytics and the Digital Economy/Lab 5 - Lending Club")

# Part 1
loan_data = read.csv("LoanStats3c.csv",skip=1)
loan_data = loan_data[1:(dim(loan_data)[1]-2),]


# Part 2
loan_data$highgrade = ifelse(loan_data$grade == "A"|loan_data$grade=="B", 1, 0)
proportion = sum(loan_data$highgrade)/dim(loan_data)[1] 

loan_data$med_income = ifelse(loan_data$annual_inc>median(loan_data$annual_inc),1,0)
loan_data$req_above = ifelse(loan_data$loan_amnt>median(loan_data$loan_amnt),1,0)
loan_data$home_rent = ifelse(loan_data$home_ownership=="RENT",1,0)

t.test(loan_data$highgrade,loan_data$med_income)
t.test(loan_data$highgrade,loan_data$req_above)
t.test(loan_data$highgrade,loan_data$home_rent)

# Part 3
fit = glm(highgrade ~ annual_inc + home_ownership + loan_amnt, data=loan_data) 
summary(fit)
probabilities = predict(fit)
pred_highgrade = ifelse(probabilities>0.6,1,0)
error = sum(abs(pred_highgrade - loan_data$highgrade)/dim(loan_data)[1]) #41.48% error

rand_benchmark = sample(0:1,nrow(pred_highgrade),replace=T)
rand_error = sum(abs(rand_benchmark-loan_data$highgrade)/dim(loan_data)[1]) #50.21% error

zero_benchmark = rep.int(0,times = nrow(pred_highgrade)) 
zero_error =  sum(abs(zero_benchmark-loan_data$highgrade)/dim(loan_data)[1]) #41.6% error

# Part 4
fit2 = rpart(highgrade~annual_inc+home_ownership+loan_amnt,data=loan_data,method="class")
plot(fit2,asp=20)
text(fit2)
help(plot)
tree_pred = predict(fit2,type="class")
tree_pred = as.numeric(tree_pred)

tree_error = sum(abs(tree_pred - 1 - loan_data$highgrade)/dim(loan_data)[1]) #39.0% error

#Part 5
test = read.csv("LoanStats3d.csv",skip=1)
test = test[1:(dim(test)[1]-2),]
test$highgrade = ifelse(test$grade == "A"|test$grade=="B", 1, 0)
test_fit = glm(highgrade~annual_inc+home_ownership+loan_amnt,data=test)
test_probabilities = predict(test_fit)
test_highgrade =ifelse(test_probabilities>0.6,1,0)
test_error = sum(abs(test_highgrade-test$highgrade)/dim(test)[1]) #44.9% error

test_fit2 = rpart(highgrade~annual_inc+home_ownership+loan_amnt,data=test,method="class")
tree_pred2 = predict(test_fit2,type="class")
tree_pred2 = as.numeric(tree_pred2)
tree_test_error = sum(abs(tree_pred2-1-test$highgrade)/dim(test)[1]) #38.2% error

test_rand_benchmark = sample(0:1,nrow(test),replace=T)
test_rand_error = sum(abs(test_rand_benchmark-test$highgrade)/dim(test)[1]) #50.0% error

test_zero_benchmark = rep.int(0,times = nrow(test)) 
test_zero_error =  sum(abs(test_zero_benchmark-test$highgrade)/dim(test)[1]) #45.3% error


