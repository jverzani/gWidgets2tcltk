##' @include icons.R
NULL

.onLoad <- function(libname,pkgname,...) {
  ## methods isn't loaded yet, so we try calling through :::
  oldClasses <- c("tkwin", "tclVar", "tclObj")
  methods::setClass("tcltkObject")
  lapply(oldClasses, function(i) {
    methods::setOldClass(i)
    methods::setIs(i,"tcltkObject")
  })


  
}
         


.onAttach <- function(...) {
  ## version check
  if(as.numeric(.Tcl("info tclversion")) < 8.5) {
    packageStartupMessage("\n\n *** gWidgetstcltk needs tcl/tk version 8.5 or newer ***\n\n")
  }
  
  ## some configuration
  .Tcl("option add *tearOff 0")         # disable tearoff menus

  ## fix selection color for treeview
  if(.Platform$OS.type == "windows")
    .Tcl(sprintf("ttk::style map Treeview.Row  -background [ list selected %s ]", "gray"))

  ## read in tklibs (from tcltk2 pacakge)
  
  addTclPath(system.file("tklibs", "tablelist5.6", package="gWidgets2tcltk"))
  tclRequire("tablelist")
  sapply(c("tablelistConfig.tcl", "tablelistBind.tcl", "tablelistBind.tcl",
           "tablelistUtil.tcl", "tablelistEdit.tcl"), function(i) {
             tcl("source", system.file("tklibs", "tablelist5.6", "scripts", i, package="gWidgets2tcltk"))
          })


  
  addTclPath(system.file("tklibs", "tooltip1.4", package="gWidgets2tcltk"))
  tclRequire("tooltip")
  tcl("source", system.file("tklibs", "autoscroll.tcl", package="gWidgets2tcltk"))
  

  
  ## load in editors

.Tcl('

#------------------------------------------------------------------------------
# editStartCmd
#
# Applies some configuration options to the edit window; if the latter is a
# ComboBox, the procedure populates it.
#------------------------------------------------------------------------------
proc editStartCmd {tbl row col text} {
    set w [$tbl editwinpath]

    switch [$tbl columncget $col -name] {
	lineName {
	    #
	    # Set an upper limit of 20 for the number of characters
	    #
	    $w configure -invalidcommand bell -validate key \
			 -validatecommand {expr {[string length %P] <= 20}}
	}

	baudRate {
	    #
	    # Populate the ComboBox and allow no more
	    # than 6 digits in its Entry component
	    #
	    $w configure -values {50 75 110 300 1200 2400 4800 9600 19200 38400
				  57600 115200 230400 460800 921600}
	    $w configure -invalidcommand bell -validate key -validatecommand \
		{expr {[string length %P] <= 6 && [regexp {^[0-9]*$} %S]}}
	}

	dataBits {
	    #
	    # Configure the SpinBox
	    #
	    $w configure -range {5 8 1} -editable no
	}

	parity {
	    #
	    # Populate the ComboBox and make it non-editable
	    #
	    $w configure -values {None Even Odd Mark Space} -editable no
	}

	stopBits {
	    #
	    # Populate the ComboBox and make it non-editable
	    #
	    $w configure -values {1 1.5 2} -editable no
	}

	handshake {
	    #
	    # Populate the ComboBox and make it non-editable
	    #
	    $w configure -values {XON/XOFF RTS/CTS None} -editable no
	}

	actDate {
	    #
	    # Set an upper limit of 10 for the number of characters
	    # and allow only digits and the "-" character in it
	    #
	    $w configure -invalidcommand bell -validate key -validatecommand \
		{expr {[string length %P] <= 10 && [regexp {^[0-9-]*$} %S]}}
	}

	actTime {
	    #
	    # Set an upper limit of 8 for the number of characters
	    # and allow only digits and the ":" character in it
	    #
	    $w configure -invalidcommand bell -validate key -validatecommand \
		{expr {[string length %P] <= 8 && [regexp {^[0-9:]*$} %S]}}
	}

	color {
	    #
	    # Populate the menu
	    #
	    set menu [$w cget -menu]
	    foreach name $::colorNames {
		set img img$::colors($name)
		$menu add radiobutton -compound left -image $img -label $name \
		    -command [list $w configure -compound left -image $img]
	    }
	    $menu entryconfigure 8 -columnbreak 1
	}
    }

    return $text
}
proc editEndCmd {tbl row col text} {
    switch [$tbl columncget $col -name] {
	available {
	    #
	    # Update the image contained in the cell
	    #
	    set img [expr {$text ? "checkedImg" : "uncheckedImg"}]
	    $tbl cellconfigure $row,$col -image $img
	}

	baudRate {
	    #
	    # Check whether the baud rate is an integer in the range 50..921600
	    #
	    if {![regexp {^[0-9]+$} $text] || $text < 50 || $text > 921600} {
		bell
		tk_messageBox -title "Error" -icon error -message \
		    "The baud rate must be an integer in the range 50..921600"
		$tbl rejectinput
	    }
	}

	actDate {
	    #
	    # Get the activation date in seconds from the last argument 
	    #
	    if {[catch {clock scan $text} actDate] != 0} {
		bell
		tk_messageBox -title "Error" -icon error -message "Invalid date"
		$tbl rejectinput
		return ""
	    }

	    #
	    # Check whether the activation clock value is later than the
	    # current one; if this is the case then make sure the cells
	    # "actDate" and "actTime" will have the same internal value
	    #
	    set actTime [$tbl cellcget $row,actTime -text]
	    set actClock [clock scan [formatTime $actTime] -base $actDate]
	    if {$actClock <= [clock seconds]} {
		bell
		tk_messageBox -title "Error" -icon error -message \
		    "The activation date & time must be in the future"
		$tbl rejectinput
	    } else {
		$tbl cellconfigure $row,actTime -text $actClock
		return $actClock
	    }
	}

	actTime {
	    #
	    # Get the activation clock value in seconds from the last argument 
	    #
	    set actDate [$tbl cellcget $row,actDate -text]
	    if {[catch {clock scan $text -base $actDate} actClock] != 0} {
		bell
		tk_messageBox -title "Error" -icon error -message "Invalid time"
		$tbl rejectinput
		return ""
	    }

	    #
	    # Check whether the activation clock value is later than the
	    # current one; if this is the case then make sure the cells
	    # "actDate" and "actTime" will have the same internal value
	    #
	    if {$actClock <= [clock seconds]} {
		bell
		tk_messageBox -title "Error" -icon error -message \
		    "The activation date & time must be in the future"
		$tbl rejectinput
	    } else {
		$tbl cellconfigure $row,actDate -text $actClock
		return $actClock
	    }
	}

	color {
	    #
	    # Update the image contained in the cell
	    #
	    $tbl cellconfigure $row,$col -image img$::colors($text)
	}
    }

    return $text
}

')

  
  ## Icons
  tcltkStockIcons$load_gWidgets_icons()
  ## images from http://ryanfait.com/resources/custom-checkboxes-and-radio-buttons/. Thanks
  tkimage.create("photo", "::image::off", file=system.file("images", "checkbutton-off.gif", package="gWidgetstcltk"))
  tkimage.create("photo", "::image::on",  file=system.file("images", "checkbutton-on.gif",  package="gWidgetstcltk"))
}
