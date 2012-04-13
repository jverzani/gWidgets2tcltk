##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcombobox
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gcombobox guiWidgetsToolkittcltk
##' @S3method .gcombobox guiWidgetsToolkittcltk
.gcombobox.guiWidgetsToolkittcltk <-  function(toolkit,
                                               items, selected = 1, editable = FALSE, coerce.with = NULL,
                                               handler = NULL,action = NULL, container = NULL, ... ) {

  if(editable)
    GComboBoxWithEntry$new(toolkit,
                           items, selected = selected, coerce.with = coerce.with,
                           handler = handler,action = action, container = container, ...)
  else
    GComboBoxNoEntry$new(toolkit,
                         items, selected = selected, coerce.with = coerce.with,
                         handler = handler,action = action, container = container, ...)
  
}


## We create two subclasses of this to handle editable and
## non-editable situation. These methods end up being in common for
## both.
GComboBox <- setRefClass("GComboBox",
                         contains="GWidgetWithTclVariable",
                         fields=list(
                           items="ANY"
                           ),
                           
                         methods=list(
                           normalize_items=function(value) {
                              ## no icons, tooltips, ... in tcltk
                             if(inherits(value,"data.frame")) {
                               value <- value[,1, drop=TRUE]
                             }
                             value <- unique(as.vector(value))
                             value
                           },                           
                           get_value=function(...) {
                             val <- as.character(tclvalue(t_var))
                             if(nchar(val) == 0)
                               val <- get_items(integer(0)) # 0-length object with same class as items
                             val
                           },
                           ## set value in subclass
                           get_index = function(...) {
                             val <- get_value()
                             if(length(val) == 0)
                               return(0L)
                             ind <- match(val, items)
                             ind
                           },
                           set_index = function(value,...) {
                             value <- min(get_length(), max(0, as.integer(value)))
                             set_value(items[value])
                           },
                           set_items=function(value, i, ...) {
                            
                             value <- normalize_items(value)
                             
                             items <<- value
                             ## careful with length 1
                             if(length(value) == 1)
                                values <- as.tclObj(as.character(value))
                              else
                                values <- as.character(value)

                             tkconfigure(widget, values=values)
                             set_value("")
                           },
                           get_items=function(i, ...) {
                             items[i]
                           },
                           ## ,
                           ## add_handler_changed=function(handler, action=NULL, ...) {
                           ##   add_handler_clicked(handler, action=NULL, ...)
                           ## },
                           get_length=function() {
                             base:::length(items)
                           },
                           add_handler_clicked = function(handler, action=NULL, ...) {
                             add_handler("changed", handler, action=action, ...)
                           }
                           ))

## combobox without entry can have icons, use rGtkDataFrame
GComboBoxNoEntry <- setRefClass("GComboBoxNoEntry",
                                contains="GComboBox",
                                methods=list(
                                  initialize=function(toolkit,
                                    items, selected = 1,  coerce.with = NULL,
                                    handler = NULL,action = NULL, container = NULL, ...) {
                                    
                                    value <- normalize_items(items)
                                    t_var <<- tclVar(value[selected])
                                    
                                    widget <<- ttkcombobox(container$get_widget(),
                                                           values="",
                                                           textvariable=t_var,
                                                           state="readonly"
                                                           )
                                    
                                    initFields(block=widget,
                                               coerce_with=coerce.with,
                                               change_signal="<<ComboboxSelected>>"
                                               )
                                    
                                    set_items(value)
                                    set_index(selected)
                                    
                                    add_to_parent(container, .self, ...)
                                    handler_id <<- add_handler_changed(handler, action)
                                    callSuper(toolkit)
                                  },
                                  set_value=function(value, ...) {
                                    if(is.factor(value))
                                      value <- as.character(value)

                                    tmp <- t_var
                                    if(length(value) == 0 || value == "")
                                      tclvalue(tmp) <- ""
                                    else if(value %in% items) {
                                      tclvalue(tmp) <- value
                                    }
                                  }
                                  ))

## The editable code is *different* from the non-editable code, as the
## gtkComboBoxNewWithEntry method isn't there yet. Instead we need to
## use a convenience function and manipulate the values with that.
## This method is deprecated as of 2.24, but that isn't what I have
## installed
GComboBoxWithEntry <- setRefClass("GComboBoxWithEntry",
                                contains="GComboBox",
                                  fields=list(
                                    poss_items="ANY"
                                    ),
                                  methods=list(
                                    initialize=function(toolkit=NULL,
                                      items,
                                      selected = 1, # use 0 for blank
                                      coerce.with = NULL,
                                      handler, action, container, ...) {
                                      ## mostly repeats above
                                      
                                      value <- normalize_items(items)
                                      t_var <<- tclVar(value[selected])
                                      
                                      widget <<- ttkcombobox(container$get_widget(),
                                                             values="",
                                                             textvariable=t_var,
                                                             state="normal"
                                                             )
                                      
                                      initFields(block=widget,
                                                 coerce_with=coerce.with,
                                                 change_signal="<<ComboboxSelected>>"
                                                 )
                                      
                                      set_items(value)
                                      set_index(selected)
                                      
                                      add_to_parent(container, .self, ...)
                                      handler_id <<- add_handler_changed(handler, action)
                                    callSuper(toolkit)

                                      
                                    },
                                    set_value=function(value, ...) {
                                      if(length(value) == 0)
                                        value <- ""
                                      tmp <- t_var
                                      tclvalue(tmp) <- value
                                    },
                                    ## if editable, allow us to toggle
                                    get_editable=function(...) {
                                      as.character(tkcget(widget, "state")) == "normal"
                                    },
                                    set_editable=function(value) {
                                      tkconfigure(widget, state=ifelse(value, "normal", "readonly"))
                                    },
                                    
                                    ## does this map to addHandlerEnter??
                                    add_handler_edited = function(handler, action=NULL, ...) {
                                      "For editing -- need a better name XXX"
                                      add_handler("<Return>", handler, action, ...)
                                    }
                                    ))

