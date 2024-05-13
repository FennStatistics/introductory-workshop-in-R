

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
x %% 500

ifelse(test = x %% 2 == 0, yes = "even", no = "odd")



