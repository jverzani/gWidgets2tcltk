library(testthat)
library(gWidgets2)
options(guiToolkit="tcltk")

f <- list.files(system.file("tests", package="gWidgets2"), full=T)
f <- Filter(function(x) !grepl("README", x), f)
f <- Filter(function(x) !grepl("html", x), f)
## issues
f <- Filter(function(x) !grepl("df.R", x), f)
f <- Filter(function(x) !grepl("text.R", x), f)
f <- Filter(function(x) !grepl("tree.R", x), f)

sapply(f, function(i) {
  message("testing ", i)
  source(i)
})
