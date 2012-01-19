\name{.gwindow.guiWidgetsToolkittcltk}
\alias{.addStockIcons.guiWidgetsToolkittcltk}
\alias{.gaction.guiWidgetsToolkittcltk}
\alias{.galert.guiWidgetsToolkittcltk}
\alias{.gbasicdialog.guiWidgetsToolkittcltk}
\alias{.gbutton.guiWidgetsToolkittcltk}
\alias{.gcalendar.guiWidgetsToolkittcltk}
\alias{.gcheckbox.guiWidgetsToolkittcltk}
\alias{.gcheckboxgroup.guiWidgetsToolkittcltk}
\alias{.gcombobox.guiWidgetsToolkittcltk}
\alias{.gconfirm.guiWidgetsToolkittcltk}
\alias{.gdf.guiWidgetsToolkittcltk}
\alias{.gedit.guiWidgetsToolkittcltk}
\alias{.getStockIconByName.guiWidgetsToolkittcltk}
\alias{.getStockIcons.guiWidgetsToolkittcltk}
\alias{.gexpandgroup.guiWidgetsToolkittcltk}
\alias{.gframe.guiWidgetsToolkittcltk}
\alias{.ggroup.guiWidgetsToolkittcltk}
\alias{.gimage.guiWidgetsToolkittcltk}
\alias{.ginput.guiWidgetsToolkittcltk}
\alias{.glabel.guiWidgetsToolkittcltk}
\alias{.glayout.guiWidgetsToolkittcltk}
\alias{.gmenu.guiWidgetsToolkittcltk}
\alias{.gmessage.guiWidgetsToolkittcltk}
\alias{.gnotebook.guiWidgetsToolkittcltk}
\alias{.gpanedgroup.guiWidgetsToolkittcltk}
\alias{.gradio.guiWidgetsToolkittcltk}
\alias{.gseparator.guiWidgetsToolkittcltk}
\alias{.gslider.guiWidgetsToolkittcltk}
\alias{.gspinbutton.guiWidgetsToolkittcltk}
\alias{.gstackwidget.guiWidgetsToolkittcltk}
\alias{.gstatusbar.guiWidgetsToolkittcltk}
\alias{.gtable.guiWidgetsToolkittcltk}
\alias{.gtimer.guiWidgetsToolkittcltk}
\alias{.gtoolbar.guiWidgetsToolkittcltk}
\alias{.gwindow.guiWidgetsToolkittcltk}
\title{toolkit constructor for gwindow}
\usage{
  \method{.gwindow}{guiWidgetsToolkittcltk} (toolkit,
  title, visible = visible, name, width, height, parent,
  handler, action, ...)

  \method{.addStockIcons}{guiWidgetsToolkittcltk} (toolkit,
  iconNames, iconFiles, ...)

  \method{.getStockIcons}{guiWidgetsToolkittcltk} (toolkit,
  ...)

  \method{.getStockIconByName}{guiWidgetsToolkittcltk}
  (toolkit, name, ...)

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

  \method{.gmenu}{guiWidgetsToolkittcltk} (toolkit,
  menu.list = list(), popup = FALSE, container = NULL, ...)

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

  \method{.gtable}{guiWidgetsToolkittcltk} (toolkit, items,
  multiple = FALSE, chosen.col = 1, icon.col = NULL,
  tooltip.col = NULL, handler = NULL, action = NULL,
  container = NULL, ...)

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

  \method{.gaction}{guiWidgetsToolkittcltk} (toolkit,
  label, tooltip = NULL, icon = NULL, key.accel = NULL,
  handler = NULL, action = NULL, parent = NULL, ...)

  \method{.gcombobox}{guiWidgetsToolkittcltk} (toolkit,
  items, selected = 1, editable = FALSE, coerce.with =
  NULL, handler = NULL, action = NULL, container = NULL,
  ...)

  \method{.gseparator}{guiWidgetsToolkittcltk} (toolkit,
  horizontal = TRUE, container = NULL, ...)

  \method{.gtoolbar}{guiWidgetsToolkittcltk} (toolkit,
  toolbar.list = list(), style = c("both", "icons", "text",
  "both-horiz"), container = NULL, ...)

  \method{.glayout}{guiWidgetsToolkittcltk} (toolkit,
  homogeneous = FALSE, spacing = 10, container = NULL, ...)

  \method{.gimage}{guiWidgetsToolkittcltk} (toolkit,
  filename = "", dirname = "", stock.id = NULL, size = "",
  handler = NULL, action = NULL, container = NULL, ...)

  \method{.gcalendar}{guiWidgetsToolkittcltk} (toolkit,
  text = "", format = "\%Y-\%m-\%d", handler = NULL, action
  = NULL, container = NULL, ...)

  \method{.gmessage}{guiWidgetsToolkittcltk} (toolkit, msg,
  title = "message", icon = c("info", "warning", "error",
  "question"), parent = NULL, ...)

  \method{.gconfirm}{guiWidgetsToolkittcltk} (toolkit, msg,
  title = "Confirm", icon = c("info", "warning", "error",
  "question"), parent = NULL, ...)

  \method{.ginput}{guiWidgetsToolkittcltk} (toolkit, msg,
  text = "", title = "Input", icon = c("info", "warning",
  "error", "question"), parent = NULL, ...)

  \method{.gbasicdialog}{guiWidgetsToolkittcltk} (toolkit,
  title = "Dialog", parent = NULL, do.buttons = TRUE,
  handler = NULL, action = NULL, ...)

  \method{.galert}{guiWidgetsToolkittcltk} (toolkit, msg,
  title = "message", delay = 3, parent = NULL, ...)

  \method{.gdf}{guiWidgetsToolkittcltk} (toolkit, items =
  NULL, handler = NULL, action = NULL, container = NULL,
  ...)
}
\arguments{
  \item{initial.msg}{initial.msg}

  \item{digits}{digits}

  \item{delay}{delay}

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

  \item{iconNames}{names of icons}

  \item{iconFiles}{path of icons}

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

  \item{menu.list}{A list defining the menu structure.
  Named sub lists determine the submenu titles and
  structure. The list may have components of class:
  \code{GAction}, mapped to a button; \code{GSeparator},
  mapped to a horizontal separator; \code{GRadio}, mapped
  to linked buttons; or \code{GCheckbox}, mapped to a
  checkbox button.}

  \item{popup}{logical. If true, make a popup window to be
  added through a handler call}

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

  \item{multiple}{logical allow multiple selectino}

  \item{chosen.col}{which value from the row is returned by
  selection}

  \item{icon.col}{NULL or integer. If latter, specifies
  column containing stock icon}

  \item{tooltip.col}{NULL or integer. If latter, specifies
  column containing tooltip}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then uses a table widget with scrollbars}

  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{label}{label for action}

  \item{tooltip}{toolktip for actin}

  \item{icon}{icon (stock icon name) for icon}

  \item{key.accel}{keyboard accelerator. If given, parent
  must be specified.}

  \item{coerce.with}{A function of function name to be
  called before selected value is returned by
  \code{svalue}}

  \item{toolbar.list}{list. A one-level list of
  \code{gaction} items, \code{gseparator} items or possibly
  other widgets. In the latter cases the \code{container}
  argument is not specified prior. (XXX Need to work this
  out with gWidgetstcltk)}

  \item{style}{style for icon or text.}

  \item{homogeneous}{are cells all the same size}

  \item{filename}{basename of file}

  \item{dirname}{dirname of file}

  \item{stock.id}{stock id of icon (if non NULL)}

  \item{size}{size of icon when a stock id (toolkit
  dependent)}

  \item{format}{Date format}

  \item{msg}{Character. Message to display.}

  \item{do.buttons}{FALSE to suppress buttons when no
  parent}
}
\value{
  NULL
}
\description{
  toolkit constructor for gwindow

  add stock icons

  Returns list of stock ids

  return stock id

  Toolkit constructor

  Toolkit gedit constructor

  toolkit constructor for ggroup

  gframe constructor

  Toolkit label constructor

  Toolkit constructor

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

  Toolkit constructor

  toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  toolkit implementation for gmessage

  toolkit implementation for gconfirm

  toolkit implmentation of ginput

  toolkit implementation

  toolkit implementation of galert

  Toolkit constructor
}
\seealso{
  The documentation for this is found at
  \code{\link{gbutton}}.
}

