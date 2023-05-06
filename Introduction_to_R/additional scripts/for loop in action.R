for(i in 1:5){
  print(i)
}


for(c in 1:3){
  for(r in 1:5){
    cat("c:", c, "r:", r, "\n")
    df[r,c]
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
    # print(aov_out)
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
x <- rnorm(n = 10, mean = 0, sd = 1)
counter = 1
while( abs(x = mean(x)) <= 1){
  x <- rnorm(n = 10, mean = 0, sd = 1)
  counter = counter + 1
  # hist(x)
  print(mean(x))
}
counter
hist(x)
summary(x)
dnorm(x = max(abs(x)), mean = 0, sd = 1) * 100



abline(v = mean(x), col = "red")


