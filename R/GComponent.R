##' @include tcltk-misc.R
##' @include dnd.R
NULL


## Base classes. These are *not* exported, rather each toolkit implementation needs
## to (mostly) provide these same basic classes:
## GComponent
##   - GWidget
##     - GButton
##     - GLabel
##     - Others matching the constructors
##   -GContainer
##     - GWindow
##     - GGroup
##       - GFrame
##         - GExpandGroup
##     - GLayout
##     - GNotebook
##       - GStacked
##     - GPanedGroup



##' Base Class for widgets and containers
##' 
##' GComponent is a parent class for both GContainer and GWidget and
##' inherits its primary interface from
##' gWidgets2::BasicToolkitInterface.
##' @rdname gWidgets2tcltk-package
GComponent <- setRefClass("GComponent",
                               contains="BasicToolkitInterface",
                               fields=list(
                                 handler_id="ANY",
                                 .e="environment", # for tag
                                 .invalid="logical",
                                 .invalid_reason="character"
                                 ),
                               methods=list(
                                 initialize=function(toolkit=guiToolkit(), ..., expand, fill, anchor, label) {
                                   initFields(toolkit=toolkit,
                                              .e=new.env()
                                              )
                                   if(is(handler_id, "uninitializedField"))
                                     handler_id <<- NULL

                                   if(is(default_expand, "uninitializedField"))
                                     default_expand <<- FALSE

                                   if(is(default_fill, "uninitializedField"))
                                     default_fill <<- FALSE

                                   callSuper(...)
                                 },
                                 is_ttkwidget=function() {
                                   "Is widget new style widget?. Override in subclass if not"
                                   TRUE
                                 },
                                 is_tkwidget=function() {
                                   "Is widget older style widget"
                                   !is_ttkwidget()
                                 },
                                 get_tk_id=function() {
                                   "Return tk ID"
                                   block$ID
                                 },
                                 get_toplevel_tk_id=function() {
                                   "return id of toplevel"
                                   as.character(tkwinfo("toplevel", get_block()))
                                 },
                                 show = function() {
                                   cat(sprintf("Object of class %s\n", class(.self)[1]))
                                 },
                                 get_value = function(...) {
                                   ## no default
                                 },
                                 get_index=function(...) get_value(),
                                 set_value = function(value, ...) {
                                   ## no default
                                 },
                                 set_index=function(value, ...) set_value(value, ...),
                                 ## length
                                 get_length = function(...) {
                                   "Get length of object. Needed for sapply."
                                   1
                                 },
                                 ## visible
                                 get_visible = function()  {
                                   as.logical(tkwinfo("viewable", get_widget()))
                                 },
                                 set_visible = function(value) {
                                   message("Default visible<- method is not implemented")
                                 },
                                 ## focus
                                 get_focus = function() {
                                   if(is.tkwin(widget))
                                     x <- widget$ID
                                   else
                                     x <- get_widget()

                                   if(is_ttkwidget()) {
                                     tl <- tkwinfo("toplevel", x)
                                     cur <- as.character(tcl("focus", displayof=tl))
                                     return(cur == x)
                                   } else {
                                     as.logical(tcl(x, "instate", "focus"))
                                   }
                                 },
                                 set_focus = function(value) {
                                   "Focus widget if TRUE"
                                   if(value) {
                                     tkfocus(get_widget()) ## of is it set focus state?
                                   }
                                 },
                                 ## enabled 
                                 get_enabled = function() {
                                   if(is_ttkwidget())
                                     !as.logical(tcl(get_widget(), "instate", "disabled"))
                                   else
                                     as.character(tkcget(get_widget(), "-state")) == "normal"
                                 },
                                 set_enabled = function(value) {
                                   if(is_ttkwidget())
                                     tcl(get_widget(), "state", ifelse(value, "!disabled", "disabled"))
                                   else
                                     tkconfigure(get_widget(), state=ifelse(as.logical(value), "normal", "disabled"))
                                 },
                                 ## tooltip
                                 get_tooltip = function(...) {
                                   message("tooltip not implemented")
                                 },
                                 set_tooltip = function(value) {
                                   if(!missing(value) && !is.null(value))
                                     tk2tip(get_widget(), paste(value, collapse="\n"))
                                 },
                                 ## font
                                 set_font = function(value) {
                                   ## We use styles if ttk else a font
                                   value <- as.list(value)
                                   if(is_ttkwidget()) {
                                     set_font_ttk(value)
                                   } else {
                                     set_font_tk(value)
                                   }
                                 },
                                 set_font_ttk = function(value, obj=get_widget()) {
                                   ## we create a style
                                   color <- value$color
                                   spec <- map_font_to_spec(value)
                                   spec <- gsub("\\s*$", "", spec)
                                   kls <- as.character(tkwinfo("class", obj))
                                   style_name <- sprintf("%s_%s.%s", gsub(" ", "", spec), ifelse(is.null(color), "black", color), kls)
                                   if(is.null(color)) 
                                       tcl("ttk::style", "configure", style_name, font=spec)
                                   else 
                                     tcl("ttk::style", "configure", style_name, font=spec, foreground=color)
                                   
                                   tkconfigure(obj, style=style_name)
                                 },
                                 set_font_tk=function(value, obj=get_widget()) {
                                   color <- value$color
                                   spec <- map_font_to_spec(value)
                                   ## just try it?
                                   tkconfigure(get_widget(), font=spec)
                                   if(!is.null(color))
                                     tkconfigure(obj, color=color)
                                 },
                                 font_family=function(family=NULL) {
                                   is.null(family) && return("")
                                   switch(family,
                                          "normal"="times",
                                          "sans" = "helvetica",
                                          "serif" = "times",
                                          "monospace"="courier",
                                          "helvetica")
                                 },
                                 font_style=function(style=NULL) {
                                   is.null(style) && return("")
                                   switch(style,
                                          "normal"="roman",
                                          "oblique"="roman",
                                          "italic"="italic",
                                          "")
                                 },
                                 font_weight = function(weight=NULL) {
                                   is.null(weight) && return("")                                   
                                   switch(weight,
                                          "heavy"="bold",
                                          "ultra-bold"="bold",
                                          "bold"="bold",
                                          "normal"="normal",
                                          "light"="normal",
                                          "ultra-light" = "normal",
                                          "")
                                 },
                                 font_size=function(size=NULL) {
                                   is.null(size) && return("")
                                   if(is.numeric(size))
                                     return(size)
                                   else
                                     switch(size,
                                            "xxx-large"=24,
                                            "xx-large"=20,
                                            "x-large"=18,
                                            "large"=16,
                                            "medium"=12,
                                            "small"=10,
                                            "x-small"=8,
                                            "xx-small"=6,
                                            "")
                                 },
                                 
                                 map_font_to_spec = function(markup, return_list=FALSE) {
                                   ## TODO tidy up using functions from above
                                   fontList <- list()
                                   if(!is.null(markup$family))
                                     fontList <- merge_list(fontList, list(family=switch(markup$family,
                                                                        "normal"="times",
                                                                        "sans" = "helvetica",
                                                                        "serif" = "times",
                                                                        "monospace"="courier",
                                                                        markup$family)))
                                   else
                                     fontList$family <- "times"
                                   
                                   if(!is.null(markup$style))
                                     fontList <- merge_list(fontList, list(slant=switch(markup$style,
                                                                        "normal"="roman",
                                                                        "oblique"="roman",
                                                                        "italic"="italic",
                                                                        "roman")))
                                   if(!is.null(markup$weight))
                                     fontList <- merge_list(fontList, list(weight=switch(markup$weight,
                                                                        "heavy"="bold",
                                                                        "ultra-bold"="bold",
                                                                        "bold"="bold",
                                                                        "normal"="normal",
                                                                        "light"="normal",
                                                                        "ultra-light" = "normal",
                                                                        "normal")))

                                     if(!is.null(markup$size)) {
                                         if(is.numeric(markup$size))
                                             fontList <- merge_list(fontList, list(size=markup$size))
                                         else
                                             fontList <- merge_list(fontList,list(size = switch(markup$size,
                                                                                                "xxx-large"=24,
                                                                                                "xx-large"=20,
                                                                                                "x-large"=18,
                                                                                                "large"=16,
                                                                                                "medium"=12,
                                                                                                "small"=10,
                                                                                                "x-small"=8,
                                                                                                "xx-small"=6,
                                                                                                as.integer(markup$size))))
                                     }
                                   if(return_list) {
                                       fontList
                                   } else {
                                     ## return "name [size [options]]"
                                     if(!is.null(fontList$slant) || !is.null(fontList$weight))
                                       fontList$size <- getWithDefault(fontList$size, 12)

                                     paste(fontList$family, fontList$size, fontList$weight, fontList$slant)
                                   }
                                 },
                                 ## tag
                                 get_attr = function(key) {
                                   if(missing(key))
                                     ls(.e)
                                   else
                                     attr(.e, key)
                                 },
                                 set_attr = function(key, value) {
                                   tmp <- .e
                                   attr(tmp, key) <- value
                                 },
                                 ##
                                 ## still there?
                                 is_extant = function() {
                                   "Is widget still available?"
                                    as.logical(as.numeric(tkwinfo("exists", get_widget())))
                                 },
                                 ## sizer
                                 get_size=function(...) {
                                   width <- tclvalue(tkwinfo("width", get_block()))
                                   height <- tclvalue(tkwinfo("height", get_block()))
                                   return(as.numeric(c(width=width, height=height)))
                                 },
                                 set_size=function(value, ...) {
                                   "Set widget size (size request), value=c(width=-1, height=-1)"
                                   if(!is.list(value))
                                     value <- setNames(as.list(value), c("width", "height")[1:length(value)])
                                   value$widget <- block
                                   do.call(tkconfigure, value)
                                 },
                                 get_widget=function() {
                                   "Return widget (not block)"
                                   widget
                                 },
                                 get_block=function() {
                                   "Return surround block"
                                   block
                                 },
                                 set_invalid=function(value, msg) {
                                   "Set widget as invalid or not"
                                   if(as.logical(value)) {
                                     .invalid <<- TRUE
                                     .invalid_reason <<- msg
                                   } else {
                                     .invalid <<- FALSE
                                     .invalid_reason <<- ""
                                   }
                                 },
                                 is_invalid=function(...) {
                                   "Is widget in an invalid state"
                                   if(length(.invalid) == 0)
                                     .invalid <<- FALSE
                                   .invalid
                                 },
                                 ##
                                 ## Work with containers
                                 ##
                                 set_parent = function(parent) {
                                   "Assign parent to parent property"
                                   parent <<- parent
                                 },
                                 add_to_parent = function(parent, child, expand=NULL, fill=NULL, anchor=NULL, ...) {
                                   "Add a child to parent if it is ia container and non null. Dispatches to add_child method of parent"
                                   
                                   if(missing(parent) || is.null(parent))
                                     return()

                                   ## return here. This is for tcltk compliance
                                   if(is(parent, "GLayout"))
                                     return()
                                   
                                   if(!is(parent, "GContainer") && is.logical(parent) && parent) {
                                     tmp <- gwindow(toolkit=toolkit)
                                     tmp$add_child(child, expand, fill, anchor, ...)
                                     return()
                                   }

                                   if(is(parent, "tkwin")) {
                                     ## we just pack in
                                     if(is.null(expand))
                                       expand <- FALSE
                                     if(is.logical(fill))
                                       fill <- ifelse(fill, "both", "none")
                                     tkpack(child$get_block(), expand=expand, fill=fill)
                                     return()
                                   }
                                   
                                   if(!is(parent,  "GContainer")) {
                                     message("parent is not a container")
                                     return()
                                   }

                                   parent$add_child(child, expand, fill, anchor, ...)
                                 },
                                 ##
                                 ## Drag and drop
                                 ##
                                 add_drop_source=function(handler, action=NULL, data.type="text", ..., connect) {
                                   "Specify widget is a drag source"
                                   add_handler("<<DragRequest>>", handler, action, connect=FALSE, ...) 
                                   ..dnd..$add_drag_source(.self)
                                 },
                                 add_drop_target=function(handler, action=NULL, ..., connect) {
                                   "Specify that widget is a drop target"
                                   add_handler("<<DropEvent>>", handler, action, connect=FALSE, ...) 
                                   ..dnd..$add_drop_target(.self)
                                 },
                                 is_dragging=function() {
                                   ..dnd..$is_dragging()
                                 },
                                 add_drag_motion=function(handler, action=NULL, ...) {
                                   "Called when motion over widget occurs"
                                   ## How to implement: bind to motion and use decorator to check if draggable
                                   XXX("implement me")
                                 }
                                 ))

## GComponentObservable adds the observable interface
##
## @param ... passed to constructor
## @aliases GComponentObservable
GComponentObservable <- setRefClass("GComponentObservable",
                                    fields=list(
                                      change_signal="character", # what signal is default change signal
                                      connected_signals="list"
                                      ),
                                    contains="GComponent",
                                    methods=list(
                                      ## Some decorators for handlers
                                      ## these wrap the handler to satisfy or fill the h object or return value
                                      event_decorator=function(FUN) {
                                        "Decorator for basic event"
                                        force(FUN) # impt to get proper f from within connect_to_toolkit
                                        f <- function(W) {
                                          FUN(.self, extra_args=list())
                                        }
                                        f
                                      },
                                      key_release_decorator=function(FUN) {
                                        force(FUN)
                                        f <- function(w, k, K, N, s, x, y, X, Y, ...) {
                                          FUN(.self, key=K, Key=k, x=x, y=y, X=X, Y=Y)
                                        }
                                        f
                                      },
                                      ## code for integrating observable interface with RGtk2
                                      handler_widget = function() widget, # allow override for block (e.g., glabel)
                                      is_handler=function(handler) {
                                        "Helper to see if handler is a handler"
                                        !missing(handler) && !is.null(handler) && is.function(handler)
                                      },
                                      ##
                                      ## Adding a handler means to
                                      ## a) create an observer and add an observer for the given signal
                                      ## 
                                      ## b) create a call back which
                                      ## calls the notify observer
                                      ## method when the widget
                                      ## actualy emits the signal
                                      add_handler=function(signal, handler, action=NULL, decorator, emitter=handler_widget(), ...) {
                                        "Uses Observable framework for events. Adds observer, then call connect signal method. Override last if done elsewhere"
                                        if(is_handler(handler)) {
                                          o <- gWidgets2:::observer(.self, handler, action)
                                          id <- add_observer(o, signal)
                                          connect_to_toolkit_signal(signal, decorator=decorator, emitter=emitter, ...)
                                          return(id)
                                        }
                                      },
                                      connect_to_toolkit_signal=function(
                                        signal, # which signal (tkbind)
                                        decorator, 
                                        emitter=handler_widget(),
                                        ...
                                        ) {
                                        "Connect signal of toolkit to notify observer"

                                        ## finds .self, signal through enclosing environment
                                        f <- function(...) {
                                          .self$notify_observers(signal=signal, ...)
                                        }
                                        ## only connect once
                                        if(is.null(connected_signals[[signal, exact=TRUE]])) {
                                          if(!missing(decorator))
                                            f <- decorator(f)
                                          if(signal == "command")
                                            tkconfigure(emitter, command=f)
                                          else
                                            tkbind(emitter, signal, f)
                                        }
                                        connected_signals[[signal]] <<- TRUE
                                      },
                                      ## initiate a handler (emit signal)
                                      invoke_handler=function(signal, ...) {
                                        "Invoke observers listening to signal"
                                        notify_observers(..., signal=signal)
                                      },
                                      invoke_change_handler=function(...) {
                                        "Generic change handler invoker."
                                        if(!is(change_signal, "uninitializedField") && length(change_signal))
                                          invoke_handler(signal=change_signal, ...)
                                      },
                                      ## block and unblock
                                      block_handlers=function() {
                                        "Block all handlers."
                                        ## default is to block the observers. 
                                        block_observers()
                                      },
                                      block_handler=function(ID) {
                                        "Block a handler by ID"
                                        block_observer(ID)
                                      },
                                      unblock_handlers=function() {
                                        "unblock blocked observer. May need to be called more than once to clear block"
                                        unblock_observers()
                                      },
                                      unblock_handler=function(ID) {
                                        "unblock a handler by ID"
                                        unblock_observer(ID)
                                      },
                                      remove_handlers=function() {
                                        "Remove all observers"
                                        remove_observers()
                                      }, 
                                      remove_handler=function(ID) {
                                        "remove a handler by ID"
                                        remove_observer(ID)
                                      },
                                      
                                      ## basic set of handlers
                                      add_handler_changed=function(handler, action=NULL,...) {
                                        if(!is(change_signal, "uninitializedField") && length(change_signal)) {
                                          add_handler(change_signal, handler, action, ...)
                                        } else {
                                          stop("No change_signal defined for widget")
                                        }
                                      },
                                      
                                      ## Basic event handlers
                                      ## Define decorators here
                                      click_decorator=function(FUN) {
                                        force(FUN)
                                        f <- function(W, x, y, X, Y) {
                                          FUN(.self,
                                              x=as.numeric(x),
                                              y=as.numeric(y),
                                              X=as.numeric(X),
                                              Y=as.numeric(Y)
                                              )
                                        }
                                        f
                                      },
                                      add_handler_clicked = function(handler, action=NULL, ...) {
                                        add_handler("<Button-1>", handler, action, decorator=.self$click_decorator,...)
                                      },
                                      add_handler_doubleclick = function(handler, action=NULL, ...) {
                                        add_handler("<Double-Button1>", handler, action, decorator=.self$click_decorator, ...)
                                      },
                                        add_handler_shift_clicked = function(handler, action=NULL, ...) {
                                            add_handler("<Shift-Button-1>", handler, action, decorator=.self$click_decorator, ...)
                                        },
                                        add_handler_control_clicked = function(handler, action=NULL, ...) {
                                            add_handler("<Control-Button-1>", handler, action, decorator=.self$click_decorator, ...)
                                        },
                                      add_handler_button_press=function(handler, action=NULL, ...) {
                                        add_handler("<ButtonPress>", handler, action, ...)
                                      },
                                      add_handler_button_release=function(handler, action=NULL, ...) {
                                        add_handler("<ButtonRelease>", handler, action, ...)
                                      },
                                      add_handler_focus=function(handler, action=NULL, ...) {
                                        add_handler("<FocusIn>", handler, action, .self$event_decorator, ...)
                                      },
                                      add_handler_blur=function(handler, action=NULL, ...) {
                                        add_handler("<FocusOut>", handler, action, .self$event_decorator, ...)
                                      },
                                      add_handler_keystroke=function(handler, action=NULL, ...) {
                                        "Keystroke handler."
                                        add_handler("<KeyRelease>", handler, action, decorator=.self$key_release_decorator, ...)
                                      },
                                      motion_decorator=function(FUN) {
                                        force(FUN)
                                        f <- function(W, s, x, y, X, Y, ...) {
                                          ## ??? consult state via s?
                                          FUN(.self, x=x, X=X, y=y, Y=Y)
                                        }
                                        f
                                      },
                                      add_handler_mouse_motion=function(handler, action=NULL, ...) {
                                        "Keystroke handler."
                                        add_handler("<Motion>", handler, action, decorator=motion_decorator, ...)
                                      },

                                      ## popup menus
                                      add_popup_menu = function(mb, action=NULL, ...) {
                                        if(is.list(mb))
                                          mb = gmenu(mb, popup=TRUE, container=.self)
                                        tkbind(widget, "<Button-1>", function(X, Y) tkpopup(mb$widget, X, Y))
                                      },
                                      add_3rd_mouse_popup_menu=function(mb, action=NULL, ...) {
                                        if(is.list(mb))
                                          mb = gmenu(mb, popup=TRUE, container=.self)
                                        events = ifelse(using_Mac(), c("<Button-2>", "<Control-1>"),  c("<Button-3>"))
                                        QT <- Map(function(event) tkbind(widget, event, function(X, Y) {
                                          tkpopup(mb$widget, as.integer(X), as.integer(Y))
                                          }), events)
                                      }


                                      ))

