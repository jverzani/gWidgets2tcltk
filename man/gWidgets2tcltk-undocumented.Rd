\name{.gwindow.guiWidgetsToolkittcltk}
\alias{.gbutton.guiWidgetsToolkittcltk}
\alias{.gcheckbox.guiWidgetsToolkittcltk}
\alias{.gcheckboxgroup.guiWidgetsToolkittcltk}
\alias{.gedit.guiWidgetsToolkittcltk}
\alias{.gexpandgroup.guiWidgetsToolkittcltk}
\alias{.gframe.guiWidgetsToolkittcltk}
\alias{.ggroup.guiWidgetsToolkittcltk}
\alias{.glabel.guiWidgetsToolkittcltk}
\alias{.gnotebook.guiWidgetsToolkittcltk}
\alias{.gpanedgroup.guiWidgetsToolkittcltk}
\alias{.gradio.guiWidgetsToolkittcltk}
\alias{.gslider.guiWidgetsToolkittcltk}
\alias{.gspinbutton.guiWidgetsToolkittcltk}
\alias{.gstackwidget.guiWidgetsToolkittcltk}
\alias{.gstatusbar.guiWidgetsToolkittcltk}
\alias{.gtimer.guiWidgetsToolkittcltk}
\alias{.gwindow.guiWidgetsToolkittcltk}
\title{toolkit constructor for gwindow}
\usage{
  \method{.gwindow}{guiWidgetsToolkittcltk} (toolkit,
  title, visible = visible, name, width, height, parent,
  handler, action, ...)

  \method{.gbutton}{guiWidgetsToolkittcltk} (toolkit, text,
  handler, action, container, ...)

  \method{.gedit}{guiWidgetsToolkittcltk} (toolkit, text =
  "", width = 25, coerce.with = NULL, initial.msg =
  initial.msg, handler = NULL, action = NULL, container =
  NULL, ...)

  \method{.ggroup}{guiWidgetsToolkittcltk} (toolkit,
  horizontal = TRUE, spacing = 5, use.scrollwindow = FALSE,
  container = NULL, ...)

  \method{.gframe}{guiWidgetsToolkittcltk} (toolkit, text,
  markup, pos, horizontal = TRUE, spacing = 5, container =
  NULL, ...)

  \method{.glabel}{guiWidgetsToolkittcltk} (toolkit, text =
  "", markup = FALSE, editable = FALSE, handler = NULL,
  action = NULL, container = NULL, ...)

  \method{.gcheckbox}{guiWidgetsToolkittcltk} (toolkit,
  text, checked = FALSE, use.togglebutton = FALSE, handler
  = NULL, action = NULL, container = NULL, ...)

  \method{.gstatusbar}{guiWidgetsToolkittcltk} (toolkit,
  text = "", container = NULL, ...)

  \method{.gtimer}{guiWidgetsToolkittcltk} (toolkit, ms,
  FUN, data = NULL, one.shot = FALSE, start = TRUE)

  \method{.gslider}{guiWidgetsToolkittcltk} (toolkit, from
  = 0, to = 100, by = 1, value = from, horizontal = TRUE,
  handler = NULL, action = NULL, container = NULL, ...)

  \method{.gspinbutton}{guiWidgetsToolkittcltk} (toolkit,
  from = 0, to = 10, by = 1, value = from, digits = 0,
  handler = NULL, action = NULL, container = NULL, ...)

  \method{.gradio}{guiWidgetsToolkittcltk} (toolkit, items,
  selected = 1, horizontal = FALSE, handler = NULL, action
  = NULL, container = NULL, ...)

  \method{.gcheckboxgroup}{guiWidgetsToolkittcltk} (toolkit
  = NULL, items, checked = FALSE, horizontal = FALSE,
  use.table = FALSE, handler = NULL, action = NULL,
  container = NULL, ...)

  \method{.gnotebook}{guiWidgetsToolkittcltk} (toolkit,
  tab.pos = 3, container = NULL, ...)

  \method{.gstackwidget}{guiWidgetsToolkittcltk} (toolkit,
  container = NULL, ...)

  \method{.gpanedgroup}{guiWidgetsToolkittcltk} (toolkit,
  horizontal = TRUE, container = NULL, ...)

  \method{.gexpandgroup}{guiWidgetsToolkittcltk} (toolkit,
  text, markup, horizontal = TRUE, handler = NULL, action =
  NULL, container = NULL, ...)
}
\arguments{
  \item{initial.msg}{initial.msg}

  \item{digits}{digits}

  \item{toolkit}{toolkit}

  \item{title}{title for window's title bar. This is the
  main property and is accessed via \code{svalue} or
  \code{svalue<-}.}

  \item{visible}{logical. If code{TRUE} window is drawn
  when constructed. Otherwise, window can be drawn later
  using \code{visible<-}. This value can default to
  \code{FALSE} by setting the option:
  \code{options("gWidgets:gwindow-default-visible-is-false"=TRUE)}.
  There are advantages: windows can draw slowly when adding
  many items. With \pkg{gWidgets2RGtk2}, the
  \code{ggraphics} widget can like to be added to an
  undrawn widget as this avoids sizing issue.}

  \item{name}{Name for registry of windows}

  \item{width}{initial width of window}

  \item{height}{initial height of window. This sets height
  before window manager manages the window}

  \item{parent}{If non-NULL, can be used to suggest default
  location of window. The argument name was changed from
  location to parent. This can be a coordinate pair (x,y)
  with (0,0) the upper left corner, or a gwindow instance.
  In the latter case the location is suggested by the
  location of the current window. This is useful for
  placing dialogs near the parent window.}

  \item{handler}{handler for destroy event}

  \item{action}{action passed t handler}

  \item{...}{ignored}

  \item{text}{label text. If text matches a stock icon
  name, that is used as well}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{use.scrollwindow}{logical. If TRUE, places box
  container in scrollwindow. In this case, a size should be
  set.}

  \item{markup}{does label use markup (toolkit specific)}

  \item{pos}{position of label: 0=left, 1=right, some
  toolkit allow values in between}

  \item{editable}{If TRUE, then clicking on label will
  enable editing of value}

  \item{checked}{is button selected}

  \item{use.togglebutton}{Use a toggle button (shows
  depressed) not a check box}

  \item{ms}{interval in milliseconds}

  \item{FUN}{FUnction to call. Has one argument, data
  passed in}

  \item{data}{passed to function}

  \item{one.shot}{logical. If TRUE, called just once, else
  repeats}

  \item{start}{logical. If FALSE, started by
  \code{start_timer} OO method. (Call
  \code{obj$start_time()}).}

  \item{from}{If a number of length one then a starting
  point, in which case to, by are passed to \code{seq}.
  Otherwise a sequence of values for which
  sort(unique(from)) will order}

  \item{to}{ending point when from is starting point}

  \item{by}{step size if not specified by \code{from}}

  \item{value}{initial value}

  \item{items}{items to select from}

  \item{selected}{index of initially selected item}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then uses a table widget with scrollbars}

  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}
}
\description{
  toolkit constructor for gwindow

  Toolkit constructor

  Toolkit gedit constructor

  toolkit constructor for ggroup

  gframe constructor

  Toolkit label constructor

  Toolkit XXX constructor

  Toolkit constructor

  S3 method for gtimer

  Toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  toolkit constructor
}
\seealso{
  The documentation for this is found at
  \code{\link{gbutton}}.
}

