
#Lottery Simulator) Please read through the below comments
#-------------------------------------------------------------------------------
#make sure to install the package tidyverse and load it
library(tidyverse)

#load the data 
data  = read_xlsx("behavioural-experiment-data.xlsx", sheet = 1)

#Now you just have to run the whole script. 
#A data frame with the  pay-offs will be returned in the console. 
#-------------------------------------------------------------------------------

#Transforming into list 
list = as.list(data)

#Defining pay off values for lottery A and B 
po_A1 = 2 
po_A2 = 1.6 
po_B1 = 3.85 
po_B2 = 0.1 

#Defining pay-off vectors for lotteries
values_A = c(po_A1, po_A2) 
values_B = c(po_B1, po_B2)

#Defining a function that simulates the lottery
pay_off_calculator = function(x){ 
  pay_offs = c()
  
  for (item in x){
    p = 0.1       #resetting probabilities & sum for each participant
    q = 1 - p
    sum = 0 
    probs = c(p,q)
    
    for (i in 1:length(item)){
      if  (item[i] == "A"){
        y = sample(x = values_A, size = 1, replace = TRUE, prob = probs)
        
        cat("pay-off:",y, "\t", "probability p:", p,"\n")
        p = p + 0.1
        q = 1 - p 
        probs = c(p,q)
        sum = sum + y
      }
      else{
        y = sample(x = values_B, size = 1, replace = TRUE, prob = probs)
        
        cat("pay-off:",y, "\t", "probability p:", p,"\n")
        p = p + 0.1
        q = 1 - p 
        values_B = c(po_B1, po_B2)
        probs = c(p,q)
        sum = sum + y
      }
    }
    cat("Congratulations! Your pay-off is the following:", sum, "CHF", "\n")
    pay_offs = append(pay_offs, sum) #filling pay-off vector
  }
  pay_offs = as.data.frame(pay_offs) 
  pay_offs$participant_Nr = c(1:length(x)) #adding participant Nr. 
  print(pay_offs)
}

#inserting list into function and running function
x = list  
pay_off_calculator(x)


