\docType{package}
\name{gWidgets2tcltk-package}
\alias{"gWidgets2tcltk-package"}
\alias{GButton}
\alias{GComponent}
\alias{GContainer}
\alias{GLabel}
\alias{GTimer}
\alias{GWidget}
\alias{gWidgets2tcltk-package}
\alias{GWidgetWithItems}
\title{gWidgets2tcltk}
\description{
  The \pkg{gWidgets2} package provides a programming
  interface for making graphical user interfaces within R.
  The package is a rewrite of the \pkg{gWidgets} package,
  introducing a few external changes but a significant
  number of internal ones. The package relies on one of
  several underlying toolkit packages providing access to
  the graphical libraries. This package provides the
  implementation to interface with the underlying
  \pkg{tcltk} package.

  GComponent is a parent class for both GContainer and
  GWidget and inherits its primary interface from
  gWidgets2::BasicToolkitInterface.

  GContainer is the base class for container objects. The
  main method is \code{add_child}, but there is also
  book-keepingn code to keep track of the child components
  of the container

  GWidget is the Base class for widget objects

  GWidgetWithItems is Base class for selection widgets
  based on a set of items. The key here is the handlers
  apply to each item, but the handler is assigned to the
  class member.

  For RGtk2, the GButton class has the extra reference
  method \code{set_border}. The \code{border} argument has
  been deprecated.

  label class for tcltk

  The main reference methods \code{GTimer} are
  \code{start_timer} and \code{stop_timer}

  GContainer is the base class for container objects. The
  main method is \code{add_child}, but there is also
  book-keepingn code to keep track of the child components
  of the container
}
\author{
  John Verzani \email{jverzani@gmail.com}

  Maintainer: John Verzani \email{jverzani@gmail.com}
}
\keyword{package}

