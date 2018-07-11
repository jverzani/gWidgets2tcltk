##      Copyright (C) 2011, 2012  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' A list extension class.
##'
##' Like a list, but has some methods. Completely superflous, but
##' makes copying some code algorithms easier. We implement methods
##' such as \code{append}, \code{push}, \code{pop} and \code{each} for
##' iteration. As well, there are some lookup methods.
##' @exportClass List
##' @name List-class
##' @param ... passed to constructor
##' @aliases List
List <- setRefClass("List",
                    fields=list(
                      "l"="list",
                      "id_ctr"="integer"
                      ),
                    methods=list(
                      initialize = function(...) {
                        if(nargs()) {
                          if(is.list(..1))
                            lst <- ..1
                        } else {
                          lst <- list(...)
                        }
                        initFields(l=lst,
                                   id_ctr=0L)
                        callSuper()
                      },
                      core = function() {
                        "return list"
                        l
                      },
                      flush = function(...) {
                        "Reset array, return contents as list"
                        tmp <- core()
                        l <<- list()
                        tmp
                      },
                      get_id = function() {
                        "Return an id, or name, for an object"
                        id_ctr <<- id_ctr + 1L
                        sprintf("%s", id_ctr)
                      },
                      contains = function(name) {
                        "TRUE if name is key in array"
                        !is.null(get_by_name(name))
                      },
                      contains_item = function(item) {
                        length(Filter(function(x) identical(x, item), l)) > 0
                      },
                      get_by_name = function(name) {
                        "get item under name"
                        l[[name, exact=TRUE]]
                      },
                      get_item = function(index) {
                        "Get item by index"
                        core()[[index]]
                      },
                      append = function(...) {
                        ## append values to array
                        if(nargs()) {
                          if(is.list(..1))
                            lst <- ..1
                          else
                            lst <- list(...)
                          nms <- names(lst)
                          if(is.null(nms))
                            nms <- sapply(seq_len(lst), get_id)
                          mapply(push, lst, nms)
                          invisible()
                        }
                      },
                      push = function(x, name) {
                        "Append x with optional name. If name not specified new id created. Returns name"
                        if(missing(name))
                          name <- get_id()
                        l[[name]] <<- x
                        name
                      },
                      pop = function() {
                        "pop last element of list"
                        out <- l[length(l)]
                        l <<- l[-length(l)]
                        out
                      },
                      insert=function(x, name, index) {
                        "Insert item into List with 0 the head and index=len() the tail"
                        n <- len()
                        
                        if(missing(index))
                          index <- n
                        index <- as.integer(max(0, min(index, n)))

                        if(missing(name))
                          name <- get_id()
                        name <- as.character(name)
                        if(index <= 0) {
                          l <<- c(x, l)
                        } else if(index >= n) {
                          l <<- c(l, x)
                        } else {
                          l <<- c(l[1:index], x, l[(index+1):n])
                        }
                        names(l)[index + 1] <<- name
                          
                      },
                      remove_by_name = function(name) {
                        ## remove item by id key
                        l[[name]] <<- NULL
                      },
                      len = function() {
                        "length"
                        base::length(l)
                      },
                      ## iterators
                      each = function(FUN, ...) {
                        "Iterator for lists, like sapply, but FUN gets passed index, key, and value"
                        nms <- names(l)
                        sapply(seq_along(l), function(i, ...) {
                          key <- nms[i]
                          value <- l[[i]]
                          FUN(i, key, value, ...)
                        },
                               ...)
                      },
                      pluck = function(id, FUN, ...) {
                        "Like ext.pluck. Returns array with 'id' extracted from each item in the List"
                        if(missing(FUN))
                          sapply(l, function(i) i[[id]])
                        else
                          sapply(l, function(i) FUN(i[[id]], ...))
                      }
                      ))
