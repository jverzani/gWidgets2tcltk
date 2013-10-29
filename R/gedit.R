##' @include GWidget.R
NULL

## TODO: XXX drophandler -- doubles up!

##' Toolkit gedit constructor
##'
##' @param initial.msg initial.msg
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gedit guiWidgetsToolkittcltk
##' @S3method .gedit guiWidgetsToolkittcltk
.gedit.guiWidgetsToolkittcltk <-  function(toolkit,
                                           text = "", width = 25,
                                           coerce.with = NULL, initial.msg=initial.msg,                    handler = NULL,action = NULL, container = NULL, ... ) {
  GEdit$new( toolkit, text = text, width = width, coerce.with = coerce.with, initial.msg=initial.msg,
                    handler = handler,action = action, container = container, ...)
}


##' The GEdit class adds some methods beyond the spec: \code{set_error}, \code{clear_error}, \code{validate_value}
##'
##' @param ... passed to constructor
GEdit <- setRefClass("GEdit",
                            contains="GWidgetWithTclVariable",
                            fields=list(
                              init_msg="character",
                              init_msg_flag="logical",
                              completion="ANY",
                              validator="ANY",
                              popup="ANY", # popup
                              word_entry = "ANY",    # text
                              lindex = "numeric",  # index of selection widget
                              no.wds = "numeric",  # track number of possible wds to choose from
                              words="character",
                              max.words="numeric"
                              ),
                            methods=list(
                              initialize=function( toolkit=NULL,
                                text = "", width = 25, coerce.with = NULL,
                                initial.msg="",
                                handler = NULL, action = NULL, container = NULL, ...) {

                                t_var <<- tclVar(text)
                                widget <<- ttkentry(container$get_widget(),
                                                    textvariable=t_var)

                                initFields(block=widget,
                                           init_msg=initial.msg,
                                           init_msg_flag=FALSE,
                                           change_signal="<<Changed>>",
                                           max.words=20L,
                                           lindex=0,
                                           words=character(0)
                                           
                                           )
                                set_coerce_with(coerce.with)
                                init_popup()
                                make_styles()
                                ## initFields(block=widget,
                                ##            coerce_with=coerce.with,
                                ##            init_msg=initial.msg,
                                ##            init_msg_flag=FALSE,
                                ##            completion=NULL,
                                ##            validator=NULL,
                                ##            change_signal="activate"
                                ##            )

                                ## overwrite?
                                if(nchar(text) > 0) {
                                  set_value(text)
                                } else if(nchar(initial.msg) > 0) {
                                  set_init_txt(initial.msg)
                                }


                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)
                                ## change on blur or return
                                add_handler_blur(function(h,...) invoke_change_handler())
                                add_handler_keystroke(function(h,...) if(h$key == "Return") invoke_change_handler())

                                callSuper(toolkit)
                              },
                              init_popup=function() {
                                tclServiceMode(FALSE)
                                on.exit(tclServiceMode(TRUE))
                                popup <<- tktoplevel()
                                tkwm.transient(popup, widget)
                                tkwm.overrideredirect(popup, TRUE)
                                tkwm.withdraw(popup)

                                word_entry <<- tktext(popup)
                                tkpack(word_entry, expand=TRUE, fill="both", side="left")
                                add_bindings()
                              },
                              set_words = function(words) {
                                words <<- unique(as.character(words))
                              },
                              find_match = function(x) {
                                "Find match in word list"
                                ind <- grepl(sprintf("%s", tolower(x)), tolower(words), fixed=TRUE)
                                words[ind]
                              },
                              show_word_list = function(str=get_value()) {
                                ##' show the word list
                                ##' @param str a string. If
                                ##' missing do nothing, otherwise match against
                                ##' string to generate word list. Popup menu
                                ##' depending on length
                                
                                char.height <- 16 ## or compute from font metrics
                                wds <- find_match(str)
                                if(length(wds) == 0) {
                                  no.wds <<- 0
                                  hide_word_list()
                                  return()
                                }
                                 ## compute max.height -- number of words that can be shown
                                screenheight <- as.numeric(tkwinfo("screenheight", widget))
                                y <- as.numeric(tclvalue(tkwinfo("rooty",widget)))
                                max.words <<- min(max.words, floor((screenheight - y)/char.height))
                                if(length(wds) > max.words)
                                  wds <- c(wds[1:max.words], "...")
                                tkdelete(word_entry, "0.0", "end")
                                tkinsert(word_entry, "end", paste(wds, collapse="\n"))
                                lindex <<- 1;
                                no.wds <<- length(wds)
                                
                                ## set geometry
                                x <- as.numeric(tclvalue(tkwinfo("rootx", widget)))
                                y <- as.numeric(tclvalue(tkwinfo("rooty", widget)))
                                geo <- as.character(tkwinfo("geometry", widget))
                                geo <- as.numeric(strsplit(geo, "[x+]")[[1]])
                                tkwm.geometry(popup, sprintf("%sx%s+%s+%s", geo[1], 10 + char.height*length(wds), x, y + geo[2]))
                                ## popup
                                tcl("wm","attributes", popup, "topmost"=TRUE) 
                                tcl("wm","attributes", popup, "alpha"=0.8)
                                tkwm.deiconify(popup)
                                tcl("raise", popup)
                                highlight_word_list()
                              },
                              hide_word_list = function(...) {
                                tcl("wm","attributes", popup, "topmost"=FALSE) # not working!
                                tkwm.withdraw(popup)
                              },
                              ## highlight word on lindex
                              highlight_word_list = function() {
                                if(lindex > 0) {
                                  tktag.remove(word_entry, "selectedWord", "0.0", "end")
                                  tktag.add(word_entry,"selectedWord",sprintf("%s.0", lindex), sprintf("%s.end", lindex))
                                  tktag.configure(word_entry, "selectedWord", font="bold")
                                }
                              },
                              ## get current word. From lineindex if applicable, or from entry widget itself
                              get_current_word = function() {
                                if(no.wds > 0)
                                  if(lindex > 0) {
                                    tclvalue(tkget(word_entry, sprintf("%s.0", lindex), sprintf("%s.end", lindex)))
                                  } else {
                                    ""
                                  }
                                else
                                  tclvalue(t_var)
                              },
                              add_bindings = function() {
                                "Add bindings to the entry box"
                                tkbind(widget, "<KeyRelease>", function(W, K) {
                                  ## Main bindings
                                  if(nchar(K) == 1 || K == "BackSpace") {
                                    ## single letter, popup menu
                                    val <- tclvalue(tcl(W, "get"))
                                    show_word_list(val)
                                  } else if(K == "Down") {
                                    ## down arrow. Open if empty, but also scroll down list
                                    if(nchar(val <- get_current_word()) == 0) {
                                      show_word_list(".")
                                      lindex <<- 0
                                    }
                                    lindex <<- min(lindex + 1, no.wds)
                                    highlight_word_list()
                                  } else if(K == "Up") {
                                    ## move up list
                                    lindex <<- max(lindex - 1, 1)
                                    highlight_word_list()
                                  } else if(K == "Return") {
                                    ## get value and put into e widget
                                    hide_word_list()
                                    if(lindex > 0) {
                                      set_value(get_current_word())
                                      invoke_change_handler()
                                    } else {
                                      invoke_change_handler()
                                    }
                                  } else if(K == "Escape") {
                                    ## close the word list
                                    hide_word_list()
                                    lindex <<- 0
                                  }
                                })
                                ## show or hide, depending
                                tkbind(widget, "<Map>", show_word_list)
                                tkbind(tcl("winfo", "toplevel", widget), "<Configure>", hide_word_list)
                                add_handler("<Destroy>", hide_word_list)
                                add_handler("<FocusIn>", clear_init_txt)
                                add_handler("<FocusOut>", function(...) {
                                  hide_word_list()
                                })
                                add_handler("<FocusOut>", function(...) {
                                  if(nchar(get_value()) == 0 && nchar(init_msg) > 0)
                                    set_init_txt(init_msg)
                                })
                                add_handler("<FocusOut>", function(...) {
                                  invoke_change_handler()
                                })
                                add_handler("<Unmap>", hide_word_list)
                                
                                tkbind(word_entry, "<Motion>", function(x, y) {
                                  tmp <- as.character(tcl(word_entry, "index", sprintf("@%s,%s", x, y)))
                                  lindex <<- as.numeric(strsplit(tmp, "\\.")[[1]][1])
                                  highlight_word_list()
                                })
                      
                                ## bind to text widget
                                tkbind(word_entry, "<Button-1>", function(x,y) {
                                  wd <- get_current_word()
                                  hide_word_list()
                                  if(wd != "...") {
                                    set_value(get_current_word())
                                  }
                                })
                                ## we don't want focus on l
                                tkbind(word_entry, "<FocusIn>", function() {
                                  tkfocus(widget)
                                })
                              },
                              set_value=function(value,  drop=TRUE, ...) {
                                if(init_msg_flag)
                                  clear_init_txt()
                                callSuper(value)
                              },
                              get_value=function(...) {
                                if(!init_msg_flag)
                                  callSuper(...)
                                else
                                  ""
                              },
                              ## initial text until widge has focus
                              set_init_txt=function(msg) {
                                "set initial text, gray out"
                                tkconfigure(widget, foreground="gray")
                                tclvalue(t_var) <<- msg
                                init_msg_flag <<- TRUE
                              },
                              clear_init_txt=function(...) {
                                "clear out init text, set back to black"
                                tkconfigure(widget, foreground="black")
                                if(init_msg_flag)
                                  tclvalue(t_var) <<- ""
                                init_msg_flag <<- FALSE
                              },
                              ## type ahead
                              get_items=function(i, j, ..., drop=TRUE) {
                                "i for index"
                                words[i]
                              },
                              set_items=function(value, i, j, ...) {
                                set_words(value)
                              },
                              get_visible = function() {
                                ## visibility is whether password character is being used
                                as.character(tkcget(e, "-show")) != "*"
                              },

                              set_visible = function(value) {
                                tkconfigure(widget, show=ifelse(value, "", "*"))
                              },

                              get_editable=function() {
                                "Can we actually edit widget?"
                                as.character(tkcget(widget, "-state")) == "normal"
                              },
                              set_editable = function(value, j) {
                                tkconfigure(widget, state=ifelse(value, "readonly", "normal"))
                              },

                              ## Extra methods
                              set_icon  = function(stock, where="start") {
                                ## set an inline icon XXX implement
                              },
                              set_icon_handler = function(handler, where="start") {
                                ## set handler for icon
                              },
                              set_validator = function(FUN) {
                                "Set a function to do the validation"
                                validator <<- FUN
                              },
                              validate_input = function() {
                                "Return logical indicating if input is valid"
                                if(is.null(validator))
                                  TRUE
                                else 
                                  validator(get_value())
                              },
                              set_invalid=function(value, msg) {
                                if(value)
                                  set_error(msg)
                                else
                                  clear_error()
                                callSuper(value, msg)
                              },
                              set_error = function(msg) {
                                "Add error state and message to widget"
                                if(!missing(msg) && !is.null(msg)) {
                                  tkconfigure(widget, style="Error.Plain.TEntry")
                                  set_tooltip(msg)
                                }
                              },
                              clear_error = function() {
                                "Clear error message"
                                tkconfigure(widget, style="TEntry")
                                set_tooltip("")

                              },
                              make_styles=function(bg="#ff6622") {
                                "Create tcl styles, cf http://paste.tclers.tk/506"
                                
                                plain_style <- "
ttk::style layout Plain.TEntry {
    Entry.field -sticky nswe -border 0 -children {
        Entry.border -sticky nswe -border 0 -children {
            Entry.padding -sticky nswe -children {
                Entry.plain.background -sticky nswe -children {
                    Entry.textarea -sticky nswe
                }
            }
        }
    }
}
"
.Tcl(plain_style)

                                ## error styles
                                error_style <- sprintf("
ttk::style configure Error.Plain.TEntry -background %s -padding 0 -borderwidth 2
", bg)
                                .Tcl(error_style)
                              }

                              ))

