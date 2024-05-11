x <- 5
y <- 5
z <- 7

x <= z && x == z

x == y
x != y

z <- 7

# und-Verknüpfung
x <= z && x == z

if(x <= z || x == z){
  print("aa")
}




vec <- c(1,2,3)
typeof(x = vec)
class(vec)

vec2 <- c(TRUE, TRUE, FALSE)
typeof(x = vec2)
class(vec2)

string <- "Hello World"
string
typeof(x = string)
class(x = string)











x <- c(1, 5, 3, 4, 5)
sum(x)
x[2] <- "hat"
x
sum(x)


longString <- "Coercion: When you call a function with an argument of the wrong type, R will try to coerce values to a different type so that the function will work. R will convert from more specific types to more general types."


















x <- c(FALSE, FALSE, TRUE)
x
typeof(x)
as.numeric(x)

sum(x)
mean(x)








x <- c(1,2,3, NA)

















x <- c(1,2,3,4,5,6,7,8,9,10)
typeof(x)
class(x)






dat <- data.frame(id = 1:26,
                  letters = letters,
                  constant = "Hello World")
dat$nextConstant <- "Hello Students"



View(dat)
head(dat)
dim(dat)
dat$id

