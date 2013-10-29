\docType{package}
\name{gWidgets2tcltk-package}
\alias{DND}
\alias{GButton}
\alias{GComponent}
\alias{GContainer}
\alias{GDf}
\alias{GLabel}
\alias{GProgressBar}
\alias{GTimer}
\alias{GWidget}
\alias{GWidgetWithItems}
\alias{gWidgets2tcltk-package}
\title{gWidgets2tcltk}
\usage{
  DND(...)

  GComponent(...)

  GWidget(...)

  GWidgetWithItems(...)

  GContainer(...)

  GButton(...)

  GDf(...)

  GLabel(...)

  GTimer(...)

  GProgressBar(...)
}
\arguments{
  \item{...}{passed to constructor}
}
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

  Drag and drop in \pkg{tcltk} is not supported by any
  underlying toolkit functionality. As such, in
  \pkg{gWidgets2tcltk} we roll our own. The result is a
  little limited: a) you can't drop values from other
  applications b) drop targets aren't allowed to be picky
  about what they receive (not by mime-type anyways).

  GComponent is a parent class for both GContainer and
  GWidget and inherits its primary interface from
  gWidgets2::BasicToolkitInterface.

  GWidget is the Base class for widget objects

  GWidgetWithItems is Base class for selection widgets
  based on a set of items. The key here is the handlers
  apply to each item, but the handler is assigned to the
  class member.

  GContainer is the base class for container objects. The
  main method is \code{add_child}, but there is also
  book-keepingn code to keep track of the child components
  of the container

  For RGtk2, the GButton class has the extra reference
  method \code{set_border}. The \code{border} argument has
  been deprecated.

  The \code{GDf} class provides a means to edit a data
  frame. We use the add on TK code provided by tablelist as
  the underlying widget

  label class for tcltk

  The main reference methods \code{GTimer} are
  \code{start_timer} and \code{stop_timer}

  For tcltk, the Gprogressbar class has the extra reference
  method \code{set_border}. The \code{border} argument has
  been deprecated.
}
\details{
  To implement drag and drop, we bind to the toplevel
  window the events: button1, motion and button1 release.
  The binding occurs not to to widget, but to the toplevel
  window containing the widget. This has some advantages,
  most imporantly it is unlikely the binding will be
  overwritten by usual gWidgets2tcltk programs. (Recall
  tkbind will only allow one binding per widget per
  signal).

  The gWidgets2 way of doing drag and drop is implemented
  here. You add a drop source with a handler that returns
  the value of what you want to pass via the dnd process.
  Then you specify a widget as a drop target and give a
  handler. This handler receives the data through the
  \code{dropdata} component of the "\code{h}" argument.
  Here, the value is generated when the drop occurs, not
  when the drag initiates. Not sure this makes any
  difference, but it might.

  Simply click on a row and the editor pops up as a modal
  dialog. The shortcut Shift+Enter will go onto the next
  case, saving changes provided the auto save featuer is
  employed.

  There is no undo/redo support here. There is no support
  for editing rownames (just lazy at the moment, holler if
  you would like that). No support to change the dimensions
  of the data frame or edit factors, ...
}
\author{
  John Verzani \email{jverzani@gmail.com}

  Maintainer: John Verzani \email{jverzani@gmail.com}
}
\keyword{package}

