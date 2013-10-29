\name{GToolBar}
\alias{GToolBar}
\title{\code{GToolBar} is the base class for toolbars}
\usage{
  GToolBar(...)
}
\arguments{
  \item{...}{passed to constructor}
}
\description{
  The toolbar is a container, so can have other widgets
  added to it as though it were a box container. Buttons
  should be added as action items, so that they are
  rendered in the proper style. Check buttons should be
  given the argument \code{use.togglebutton}. Use
  \code{addSpring} to right align items.
}

