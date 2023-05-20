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
               pointsize=44, #text is shrinking by saving graphic
               dpi= "auto",
               bg = "white")
}
