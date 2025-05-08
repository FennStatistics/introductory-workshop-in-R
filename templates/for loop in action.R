for(i in 1:5){
  print(i)
}

vec <- c("Hallo", "wie", "gehts?")
for(i in vec){
  print(i)
}

df <- cars

for(c in 1:2){
  for(r in 1:50){
    cat("c:", c, "r:", r, "\n")
    if(df[r,c] >= 50){
      print("ALARM")
    }
    }
}



####################################
df <- cars

df$speed > 20

for(i in 1:nrow(df)){
  if (df$speed[i] > 20){
    print("Here I am")
  }else{
    print("I am too slow")
  }
}

for(r in 1:nrow(df)){
  print(r)
}
####################################




x < 0
any(x < 0)
all(x < 0)

which(x = x < 0)

x <- c(800, 1200, 670)
if(any(x < 0)){
  print("Problem")
}else if(any(x < 600)){
  print("kleiner 600")
}else{
  print("kein Problem")
}

x <- c(800, 1200, 570)
if(any(x < 0)){
  print("Problem")
}

if(any(x < 600)){
  print("kleiner 600")
}










for(i in 1:10) { # Head of for-loop
  x1 <- i^2 # Code block
  if(x1 > 20){
    print(x1) # Print results
  }

}







x <- 1:10
x %% 2

ifelse(test = x %% 2 == 0, yes = "even", no = "odd")







#####################################

?iris
df <- iris
table(df$Species)

boxplot(df$Sepal.Length ~ df$Species)
aov_out <- aov(formula = Sepal.Length ~ Species, data = df)
summary(aov_out)

unlist(summary(aov_out))[9] < .01

c <- 1
for(c in 1:4) {
  aov_out <- aov(formula = df[,c] ~ df$Species)
  tmp_pvalue <- unlist(summary(aov_out))[9]

  if (tmp_pvalue < (.01 / 4) ) {
    print(aov_out)
    print(c)
  }
}


vec_select <- sample(x = 1:150, size = 50)
vec_select

df_subset <- df[vec_select,]

aov_out <- aov(formula = Sepal.Length ~ Species,
               data = df_subset)
summary(aov_out)




set.seed(seed = 123)
for(i in 1:10000){
  vec_select <- sample(x = 1:150, size = 30)
  df_subset <- df[vec_select,]
  aov_out <- aov(formula = Sepal.Length ~ Species,
                 data = df_subset)

  tmp_pvalue <- unlist(summary(aov_out))[9]
  if (tmp_pvalue >= .01 ) {
    print(i)
  }
}





df <- iris
vec_colnames <- colnames(df)
vec_level_species <- levels(x = df$Species)
for(i in vec_colnames){
  print(i)
}



####### while loop
N <- 10
x <- rnorm(n = N, mean = 0, sd = 1) # inital start
counter = 1
while( abs(x = mean(x)) <= 1){
  x <- rnorm(n = N, mean = 0, sd = 1)
  counter = counter + 1
  # hist(x)
  print(mean(x))
}
counter

hist(x)
abline(v = mean(x), col = "red")

summary(x)


sd_mean <- 1 / sqrt(N)
prob_extreme_mean <- 2 * pnorm(q = -1, mean = 0, sd = sd_mean)
prob_extreme_mean  # chance that the sample mean exceeds ±1
