#Question1
rep(1:3, each=3)

seq(1, 10, by=2)

order(10:1, decreasing=F)

print((1:4)>2&(1:4)%%2==0)
print((1:4)>2&&(1:4)%%2==0)

x = c(1:5, NA)
print(mean(x))

#Question2
sex = c(1,1,1,1,1,1,2,2,2,2); 
graduate = c(1,0,1,0,1,0,0,0,0,1); 
score=c(9:1, NA)

zz = data.frame(sex, graduate, score)
zz

apply(zz[-1,], 2, max)

zz[zz[,3]>7,]

which.max(zz$score)

zz[order(zz["graduate"],zz["score"]),]

subset(zz, zz["sex"]==1)

tapply(zz$score, zz$graduate, mean, na.rm=T)

apply(zz[-10, ], 1, function(x){ sum(x) })

#Question3
mylist=list(sex = c(1,1,1,1,2,2,2,2,2,2),
            smoking = c(1,0,1,0,1,0,0,0,1,1), 
            age=c(21:30))
mylist

length(mylist)
lapply(mylist, function(kk){ kk*3 })
sapply(mylist, max)
(mylist$sex-mylist$smoking)^3

#Question4
zz1=matrix(c(c(1,2,NA), c(3,4,5), 6:9), nrow = 2, ncol = 5, byrow = TRUE)
zz1

zz1_avg_col<- apply(zz1, 2, mean, na.rm = TRUE)
zz1_avg_col

zz1_sum_row<- apply(zz1, 1, sum, na.rm = TRUE)
zz1_sum_row


#Question5
geneExpr=data.frame(gene=LETTERS[1:10], 
                    expr1=c(2.1, 4.5, 6.8, 7.9, 8.1, 5.0, 4.6, 3.2, 3.5, 7.8), 
                    expr2=c(6.1, 4.2, 2.8, 0.9, 0.1, 3.0, 2.6, 8.2, 3.4, 6.8))
geneExpr

counter<-0
for (i in 1:length(geneExpr[,1])) {
  if (geneExpr[i,2]<=7){
    counter<-counter+1
  }
}
print(counter)

myMin<-function(x, cut) { min(x[x>cut]) }
apply(geneExpr[,-1],2, myMin, cut=3)


#Question6

#(a)Std Dev
std<- function(x){
  sd=0
  if (length(x)==1) return (NA)
  else
    for (i in 1:length(x)){
      sd= sd + sqrt(sum(x[i]- mean(x))^2/(length(x)-1))
    }
  return (sd)
}

std(1)
std(1:5)
std(c(1,1,1,1,1,1))

#(b)Pi
getPi <- function(k){
  pie <- 0.0
  itr <- 0.0
  while(floor(pie*10^(k-1))!=floor(pi*10^(k-1))){
    pie <- pie +( (4*((-1)^itr))/(2*itr+1))
    itr <- itr+1
  }
  print(as.numeric(substr(pie,1,k+1)))
  #return(itr)
}
getPi(15)

#Question7
phones <- c("2197338965", "+1 219 733 8965", " 219 733 8965", "329-293-8753 ", "595 794 7569", "387 287 6718", "233.398.9187 ", "482 952 3315", "Work: 579-499-7527", "Home:543.355.3679")

grep["(+1 )?([WwOoRrKk]|[HhOoMmEe]: )?([(])?[0-9]{3}([)])?( |.|-)?[0-9]{3}( |.|-)?[0-9]{4}( )", phones]

#(c)
library(dplyr)
library(tidyr)
library(knitr)

getData(state.name)
states<- c("Vermont",
           "Virginia",
           "Washington",
           "West Virginia",
           "Wisconsin",
           "Wyoming"
)
grep("\b(+a)\b", states, value = TRUE)


grep("*a", state.name, value = TRUE)



library(stringr)

hw <- "Harry Potter"
str_sub(hw, end = 8)

str_pad("a", 5, pad = c("*", "_", "0"))

fruit <- c("pinapple", "pear", "banana", "apple");
str_detect(fruit, "^p", negate = TRUE)

x <- c("<a> <b>", "<a> <>", "<a>", "", NA);
str_match(x, "<(.*?)> <(.*?)>")

fruits <- c("one apple", "two pears", "three bananas");
str_replace(fruits, "[aeiou]", c("1", "2", "3"))



#normal distribution
fun_nd<- function(x, m, st){
  y=0
  pi= 3.14
  y= y + (1/sqrt(2*pi)) * exp((-(x-m)^2)/(2*st))
  return (y)
}

fun_nd(1.5,1,4)

fun_nd<-function(x,mean,std){
  y<-0
  y<-exp(-(x-mean)^2/(2*std))/(std*sqrt(2*pi))
  return(y)
}
fun_nd(1.5,1,4)

seq(10, 6, by=3)

order(10:20, decreasing=T)

library("dplyr")
msleep <- read.csv("msleep_ggplot2.csv")
msleep = head(msleep)
msleep

fibo<-function(n){
  Fibonacci <- numeric(n)
  Fibonacci[1] <- Fibonacci[2] <- 1
  for (i in 3:n) Fibonacci[i] <- (2*Fibonacci[i - 1]) + Fibonacci[i - 2]
  print(Fibonacci)
}
fibo(10)

zz=matrix(c(c(1,2,NA,4), seq(4,8), rep(5,6)), nrow = 3, ncol
          = 5)
apply(zz, 1, sum, na.rm = TRUE)
apply(zz, 2, mean, na.rm = TRUE)










