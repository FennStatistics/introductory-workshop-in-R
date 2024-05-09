# within the generated for loop show summary statistics and store the data in a list
for(i in 1:10){
    print(i)
    rnorm(n = 10, mean = 10, sd = 1)
}