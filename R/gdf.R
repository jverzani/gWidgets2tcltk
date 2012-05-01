##' @include GWidget.R
##' @include dialogs.R
##' @include gtable.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gdf
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gdf guiWidgetsToolkittcltk
##' @S3method .gdf guiWidgetsToolkittcltk
.gdf.guiWidgetsToolkittcltk <-  function(toolkit,
                                         items = NULL,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GDf$new(toolkit,
           items=items, 
           handler = handler, action = action, container = container, ...)
}


##' Class to provide means to edit data frames
##'
##' The \code{GDf} class provides a means to edit a data frame. Unlike
##' \pkg{RGtk2} and \pkg{qtbase}, there is no editable table widget
##' include in \pkg{tctlk}. What to do? Well, the add-on \pkg{tktable}
##' package could be used, and was. However, that never really seemed
##' that usable. As such, we try implementing a case-by-case editor
##' here, allowing fairly convenient row editing.
##'
##' Simply click on a row and the editor pops up as a modal
##' dialog. The shortcut Shift+Enter will go onto the next case,
##' saving changes provided the auto save featuer is employed.
##'
##' There is no undo/redo support here. There is no support for
##' editing rownames (just lazy at the moment, holler if you would
##' like that). No support to change the dimensions of the data frame
##' or edit factors, ...
##' @rdname gWidgets2tcltk-package
GDf <- setRefClass("GDf",
                   contains="BaseTableClass",
                    fields=list(
                      ),
                    methods=list(
                      initialize=function(toolkit,
                        items,
                        name=deparse(substitute(df)),
                        handler=NULL, action=NULL,
                        container=NULL,
                        ...) {

                        ## what is 
                        initFields(change_signal="<<TreeviewSelect>>")
                        
                        init_widget(container$get_widget(), ...)

                        items <- as.data.frame(items)
                        n <<- ncol(items)

                        ## configure size
                        tkconfigure(widget, columns=1:n)

                        set_selectmode("none")

                        ## populate
                        set_DF(items)
                        set_column_headings(names(get_data()))

                        add_to_parent(container, .self, ...)

                        ## bind to button-1 to open editor
                        tkbind(widget, "<Button-1>", function(W, x, y) {
                          row <- as.character(tcl(W, "identify", "row", x, y))
                          column <- as.numeric(gsub("^#", "", as.character(tcl(W, "identify", "column", x, y))))
                          ind <- match(row, child_ids)
                          if(length(ind))
                            make_row_editor(ind, column)
                        }) 

                        ## change handler is row updated
                        handler_id <<- add_handler_changed(handler, action)
                        
                        callSuper(toolkit)
                      },
                      get_items=function(i, j, ...) {
                        m <- get_data()
                        m[i,j, ...]
                      },
                      replace_row_data=function(i, value) {
                        callSuper(i, value)
                        invoke_change_handler()
                      },                        
                      make_row_editor=function(ind=1, column=NULL) {
                        w <- gwindow(gettext("Case editor"), visible=FALSE, parent=block)
                        g <- ggroup(cont=w, horizontal=FALSE)

                        use.scrollwindow <- get_dim()[1] > 20
                        ed <- Editor$new(.self, rownames=NULL, ind=ind, column=column, container=g, use.scrollwindow=use.scrollwindow)
                        ed$dirty <- FALSE
                        
                        paging_bar <- PagingBar$new(ed=ed, cur_page=ind, parent=g$widget)
                        tkpack(paging_bar$gp, side="top", fill="x")
                        gseparator(cont=g)
                        button_group <- ggroup(cont=g, horizontal=TRUE)
                        gcheckbox(gettext("Auto save"), checked=TRUE, cont=button_group, handler=function(h,...) {
                          val <- h$obj$get_value()
                          save_btn$set_enabled(!val)
                          ed$save_on_page_change <- val
                        })
                        save_btn <- gbutton("Save changes", cont=button_group, handler=function(h,...) {
                          ed$save_values()
                        })
                        save_btn$set_enabled(FALSE)
                        gbutton("dismiss", cont=button_group, handler=function(h,...) {
                          ed$save_values()
                          w$set_modal(FALSE)
                          w$dispose_window()
                        })
                        w$set_visible(TRUE)
                        tkbind(w$block, "<Shift-Return>", function() paging_bar$next_page())
                        w$set_modal(TRUE)
                      }
                      ))


## Editor
make_editors <- function(x, i, nm, lyt) UseMethod("make_editors")
make_editors.numeric <- function(x, i, nm, lyt) {
  lyt[i, 1] <- nm
  lyt[i, 2] <- gedit("", container=lyt, coerce.with=as.numeric)
}
make_editors.integer <- function(x, i, nm, lyt) {
  lyt[i, 1] <- nm
  lyt[i, 2] <- gedit("", container=lyt, coerce.with=as.integer)
}
make_editors.character <- function(x, i, nm, lyt) {
  lyt[i, 1] <- nm
  lyt[i, 2] <- gedit("", container=lyt)
}
make_editors.logical <- function(x, i, nm, lyt) {
  lyt[i,1] <- nm
  lyt[i,2] <- gcheckbox("", container=lyt)
}
make_editors.factor <- function(x, i, nm, lyt) {
  lyt[i,1] <- nm
  lyt[i,2] <- gcombobox(levels(x), container=lyt)
}
  

## Case editor. Tied to DF above
Editor <- setRefClass("Editor",
                    fields=list(
                      DF="ANY",
                      rownames="ANY",
                      editors="list",
                      cur_page="integer",
                      dirty="logical",
                      save_on_page_change="logical"
                      ),
                    methods=list(
                      initialize=function(DF, rownames, ind=1, column=NULL, container, use.scrollwindow=FALSE) {
                        initFields(DF=DF,
                                   rownames=rownames,
                                   save_on_page_change=TRUE,
                                   cur_page=0L,
                                   dirty=FALSE
                                   )

                        g <- ggroup(cont=container, horizontal=FALSE, expand=TRUE, fill=TRUE,
                                    use.scrollwindow=use.scrollwindow)
                        lyt <- glayout(cont=g, spacing=c(5,2), expand=TRUE)

                        m <- DF$get_data()
                        nms <- names(m)
                        editors <<- lapply(seq_along(m), function(i) {
                          make_editors(m[[i]], i, nms[i],  lyt)
                        })
                        names(editors) <<- nms
                        lapply(editors, addHandlerChanged, function(...) {
                          dirty <<- TRUE
                        })

                        set_page(ind, column)

                      },
                      total_pages=function() {
                        DF$get_dim()[1]
                      },
                      set_page=function(i, column) {
                        "initialize values from row i"
                        if(cur_page > 0 &&
                           (save_on_page_change ||
                            gconfirm(c("Unsaved changes.", "Save them?")))
                           )
                          save_values()

                        cur_page  <<- as.integer(i)
                        dirty <<- FALSE
                        values <- DF$get_items(i)
                        f=function(editor, value) editor$set_value(value)
                        mapply(f, editors, values)
                        if(!missing(column) && !is.null(column))
                          editors[[column]]$set_focus(TRUE)
                        DF$scroll_to(i)
                      },
                      save_values=function() {
                        "save values into DF"
                        vals <- lapply(editors, function(i) i$get_value())
                        DF$replace_row_data(cur_page, vals)
                      },
                      get_dim=function() DF$get_dim()
                      ))

## configure_paging
PagingBar <- setRefClass("PagingBar",
                            fields=list(
                              cur_page="numeric",
                              total_pages="numeric",
                              ed="ANY",
                              gp="ANY",
                              ll_button="ANY",
                              l_button="ANY",
                              r_button="ANY",
                              rr_button="ANY",
                              page_label_var="ANY",
                              page_label="ANY",
                              no_pages="ANY"
                              ),
                            methods=list(
                              initialize=function(ed, cur_page=1L, parent) {
                                gp <<- ttkframe(parent)
                                page_label_var <<- tclVar("1")
                                initFields(cur_page=cur_page,
                                           total_pages=0L,
                                           ed=ed,
                                           ll_button=ttkbutton(gp, text="<<", style="Toolbutton", command=.self$first_page),
                                           l_button = ttkbutton(gp, text="<", style="Toolbutton",  command=.self$prev_page),
                                           r_button = ttkbutton(gp, text=">", style="Toolbutton",  command=.self$next_page),
                                           rr_button = ttkbutton(gp, text=">>", style="Toolbutton",  command=.self$last_page),
                                           page_label = ttkentry(gp, textvariable=page_label_var),
                                           no_pages = ttklabel(gp, text="")
                                           )
                                layout_widget()
                                set_total_pages(ed$get_dim()[1])
                                set_page(cur_page)
                              },
                              layout_widget=function() {
                                "Layout widget"
                                sapply(list(ll_button, l_button,
                                            ttklabel(gp, text=gettext("Showing case")),
                                            page_label,
                                            ttklabel(gp, text=gettext("of")),
                                            no_pages,
                                            r_button, rr_button), tkpack, padx=2, side="left")
                                tkbind(page_label, "<FocusOut>", .self$goto_page)
                                tkbind(page_label, "<KeyRelease>", function(k, K) {
                                  val <- as.integer(tclvalue(page_label_var))
                                  set_tooltip(val)
                                  if(as.character(K) == "Return")
                                    .self$goto_page()
                                })
                              },
                              set_tooltip=function(val) {
                                if(1 <= val && val <= total_pages)
                                  out <- ""
                                else
                                  out <- gettext("Number out of range")
                                tcltk2:::tk2tip(page_label, out)
                              },
                              set_page = function(i) {
                                "Set page through DF method"
                                prev_page <- cur_page
                                if(missing(i))
                                  i <- cur_page
                                cur_page <<- as.numeric(i)
                                tclvalue(page_label_var) <<- i
                                if(prev_page != i)
                                  ed$set_page(i)
                                tcltk2:::tk2tip(page_label, "")
                              },
                              set_total_pages = function(n) {
                                total_pages <<- as.integer(n)
                                tkconfigure(no_pages, text=total_pages)
                                tkconfigure(page_label, width=3 + ceiling(log(total_pages + 1)))
                              },
                              first_page=function() {
                                set_page(1L)
                              },
                              prev_page=function() {
                                set_page(max(1, cur_page - 1))
                              },
                              next_page=function() {
                                set_page(min(total_pages, cur_page + 1))
                              },
                              last_page=function() {
                                set_page(total_pages)
                              },
                              goto_page=function(k) {
                                if(missing(k))
                                  k <- as.integer(tclvalue(page_label_var))
                                if(1 <= k && k <= total_pages) {
                                  set_page(k)
                                } 
                              }
                            
                              ))
                                
