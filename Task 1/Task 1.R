#1-Read the given Table
data<-read.table("task.txt", header = TRUE, sep = ",")
head(data)

#2-Summary of the data
summary(data)


length(data)
nrow(data)


#3-Find Correlation, variance, covariance, standard deviation
x <- data$sales_total
y <- data$num_of_orders

cor(x,y) 
cov(x,y) 
sd(x)
var(x)


#4-Make another data_frame and add it to the original data 
df2 = data.frame(data$cust_id,data$sales_total,data$num_of_orders,data$gender)
head(df2)



#5-Make a function that loop and find the number of females and sales_total > 300
my_function <- function() 
{ 
  
  ct <- 0
  
  for(value in (data[["gender"]])) 
  {
    for(value2 in (data[["sales_total"]]))
    {
      
      if(value=="F" && value2 >= 300)
      {
        ct <- ct + 1
      }
      else
      {
        break
      }
    }
  }
  
  print(ct)
}
  
my_function()






#6-Print the first 4 rows and the last 4 rows 
head(data)
tail(data)


#7-Make a subset of the data set for gender and number of orders and find the mean, median, mode
subset<-subset(df2,select=c(3,4))
head(subset)

x <- data$num_of_orders
y <- factor((data$gender))
y2<-as.numeric(y)
#print(y2)


mean(x)
mean(y2)
median(x)
median(y2)
mode = function()
{
  print("Mode for x:")
  return(sort(-table(x))[1])
  
  
}
mode()


mode2 = function()
{
  print("Mode for y2:")
  return(sort(-table(y2))[1])
}

mode2()



#End of assignment requirements
