\name{GMenuBar}
\alias{GMenuBar}
\title{Toplevel menu bar}
\usage{
  GMenuBar(...)
}
\arguments{
  \item{...}{passed to constructor}
}
\description{
  The \code{GMenuBar} class provides functionality for a
  top-level menubar. In \pkg{tlctk} the menubars can show
  either \code{gaction} items (proxied as buttons),
  \code{gradio} items or \code{gcheckbox} items. In the
  latter two cases, one uses the \code{parent} argument --
  not the \code{container} argument -- to specify the
  parent container. Such items can also be shared with
  toolbars.
}
\examples{
\dontrun{
w <- gwindow("having fun?")
sb <- gstatusbar("Your message here...", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
f <- function(h,...) message(h$obj$get_value())

l <- list(file=gaction("file", handler=function(h,...) print("file"),
               key.accel="<Control-x><Control-s>", parent=w),
          ok=gaction("ok", icon="ok", handler=function(h,...) print("ok")),
          radio=list(
            rb=gradio(state.name[1:3], parent=w, handler=function(h,...)
               print(h$obj$get_value()))
            )
          sep=gseparator(vertical=TRUE),
          ,cb=gcheckbox("really", parent=w, handler=function(h,...) print(h$obj$get_value()))
          )
mlist <- list(File=l)
mb <- gmenu(mlist, cont=w)
}
}

