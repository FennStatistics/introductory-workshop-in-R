############################################################################
##### save_graphic()
# save graphic object as png file
############################################################################
save_graphic <- function(filename){
  tmp <- paste(filename, ".png", sep = "")
  Cairo::Cairo(file=tmp,
               type="png",
               units="px",
               width=2500,
               height=1700,
               pointsize=44, # text size is artifically shrunked when saved
               dpi= "auto",
               bg = "white")
}



setwd("outputs")
dev.off()
save_graphic(filename = "myFirstPlot")
plot(cars$speed, cars$dist)
# hist(x = rnorm(n = 1000, mean = 10, 1))
dev.off()




plot(cars$speed, cars$dist)
lm1 <- lm(formula = speed ~ dist, data = cars)
summary(lm1)

stargazer::stargazer(lm1, type = "html", out = "myLM.html")

report::report(lm1)
