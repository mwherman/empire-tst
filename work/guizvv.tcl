#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {
    # Provoke name search
    catch {package require bogus-package-name}
    set packageNames [package names]

    switch $tcl_platform(platform) {
	windows {
	}
	default {
	    option add *Scrollbar.width 10
	}
    }
    
    # Check if Itcl is available
    if {[lsearch -exact $packageNames Itcl] != -1} {
	package require Itcl 3.0
    }

    # Check if Itk is available
    if {[lsearch -exact $packageNames Itk] != -1} {
	package require Itk 3.0
    }

    # Check if Iwidgets is available
    if {[lsearch -exact $packageNames Iwidgets] != -1} {
	package require Iwidgets 3.0

	switch $tcl_platform(platform) {
	    windows {
		option add *Scrolledhtml.sbWidth    16
		option add *Scrolledtext.sbWidth    16
		option add *Scrolledlistbox.sbWidth 16
	    }
	    default {
		option add *Scrolledhtml.sbWidth    10
		option add *Scrolledtext.sbWidth    10
		option add *Scrolledlistbox.sbWidth 10
	    }
	}
    }
    
    # Check if Tix is available
    if {[lsearch -exact $packageNames Tix] != -1} {
	package require Tix
    }
    
}
#############################################################################
# Visual Tcl v1.51 Project
#

#################################
# VTCL LIBRARY PROCEDURES
#

if {![info exists vTcl(sourcing)]} {
proc Window {args} {
    global vTcl
    set cmd     [lindex $args 0]
    set name    [lindex $args 1]
    set newname [lindex $args 2]
    set rest    [lrange $args 3 end]
    if {$name == "" || $cmd == ""} { return }
    if {$newname == ""} { set newname $name }
    if {$name == "."} { wm withdraw $name; return }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists} { wm deiconify $newname; return }
            if {[info procs vTclWindow(pre)$name] != ""} {
                eval "vTclWindow(pre)$name $newname $rest"
            }
            if {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                eval "vTclWindow(post)$name $newname $rest"
            }
        }
        hide    { if $exists {wm withdraw $newname; return} }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}
}

if {![info exists vTcl(sourcing)]} {
proc {vTcl:DefineAlias} {target alias widgetProc top_or_alias cmdalias} {
    global widget

    set widget($alias) $target
    set widget(rev,$target) $alias

    if {$cmdalias} {
        interp alias {} $alias {} $widgetProc $target
    }

    if {$top_or_alias != ""} {
        set widget($top_or_alias,$alias) $target

        if {$cmdalias} {
            interp alias {} $top_or_alias.$alias {} $widgetProc $target
        }
    }
}

proc {vTcl:Toplevel:WidgetProc} {w args} {
    if {[llength $args] == 0} {
        return -code error "wrong # args: should be \"$w option ?arg arg ...?\""
    }

    ## The first argument is a switch, they must be doing a configure.
    if {[string index $args 0] == "-"} {
        set command configure

        ## There's only one argument, must be a cget.
        if {[llength $args] == 1} {
            set command cget
        }
    } else {
        set command [lindex $args 0]
        set args [lrange $args 1 end]
    }

    switch -- $command {
        "hide" -
        "Hide" {
            Window hide $w
        }

        "show" -
        "Show" {
            Window show $w
        }

        "ShowModal" {
            Window show $w
            raise $w
            grab $w
            tkwait window $w
            grab release $w
        }

        default {
            eval $w $command $args
        }
    }
}

proc {vTcl:WidgetProc} {w args} {
    if {[llength $args] == 0} {
        return -code error "wrong # args: should be \"$w option ?arg arg ...?\""
    }

    ## The first argument is a switch, they must be doing a configure.
    if {[string index $args 0] == "-"} {
        set command configure

        ## There's only one argument, must be a cget.
        if {[llength $args] == 1} {
            set command cget
        }
    } else {
        set command [lindex $args 0]
        set args [lrange $args 1 end]
    }

    eval $w $command $args
}
}

if {[info exists vTcl(sourcing)]} {
proc vTcl:project:info {} {
    namespace eval ::widgets::.topwindow {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::.topwindow.title {
        array set save {-background 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.project-lab {
        array set save {-background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.project {
        array set save {-background 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.mtframe {
        array set save {-background 1 -foreground 1 -labeltext 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite {
        array set save {-background 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbtot {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbel {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbinel {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbn2n {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbn3n {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbnna {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbnnp {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbnnpa {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbng {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbnp {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbna {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.mtframe.childsite.chkbnpa {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.show {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.merge {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.exit {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.set1-entry {
        array set save {-background 1 -label 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.suf-lab {
        array set save {-background 1 -foreground 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.sufix {
        array set save {-background 1 -highlightbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.dir-lab {
        array set save {-background 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.dir2-entry {
        array set save {-label 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.dir3-entry {
        array set save {-label 1 -variable 1}
    }
    namespace eval ::widgets::.topwindow.creatszvd {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.creatazvd {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.showall {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.sep-lab {
        array set save {-background 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.curv1-lab {
        array set save {-background 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.label53 {
        array set save {-background 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.label54 {
        array set save {-background 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::.topwindow.curv1-ent {
        array set save {-background 1 -foreground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.entry55 {
        array set save {-background 1 -foreground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::.topwindow.entry56 {
        array set save {-background 1 -foreground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets_bindings {
        set tagslist {}
    }
}
}
#################################
# USER DEFINED PROCEDURES
#

proc {main} {argc argv} {
wm protocol .topwindow WM_DELETE_WINDOW {exit}
}

proc init {argc argv} {

}

init $argc $argv

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base {container 0}} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    if {!$container} {
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0; update
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vtcl.tcl"
    bindtags $base "$base Vtcl.tcl all"
    }
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.topwindow {base {container 0}} {
    if {$base == ""} {
        set base .topwindow
    }
    if {[winfo exists $base] && (!$container)} {
        wm deiconify $base; return
    }

    global widget

    ###################
    # CREATING WIDGETS
    ###################
    if {!$container} {
    toplevel $base -class Toplevel \
        -background #4b7b82 -highlightbackground #4b7b82 \
        -highlightcolor #4b7b82 
    wm focusmodel $base passive
    wm geometry $base 331x679+239+234; update
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Show ZVV plots"
    }
    label $base.title \
        -background #4b7b82 -disabledforeground #4b7b82 \
        -font {Helvetica -18 bold} -foreground white \
        -highlightbackground #4b7b82 -highlightcolor #4b7b82 -pady 1 \
        -text {Create/Show ZVV Plots} 
    label $base.project-lab \
        -background #4b7b82 -foreground white -highlightbackground #4b7b82 \
        -highlightcolor #4b7b82 -pady 1 -text {Project root name:} 
    entry $base.project \
        -background white -textvariable root -width 17 
    ::iwidgets::labeledframe $base.mtframe \
        -background #4b7b82 -foreground white -labeltext {Select MT} 
    bindtags $base.mtframe "itk-delete-.topwindow.mtframe $base.mtframe Labeledframe $base all"
    checkbutton $base.mtframe.childsite.chkbtot \
        -activebackground #609498 -activeforeground white -background #4b7b82 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor white -justify left -offvalue {} -text {  1 total} \
        -variable tot 
    checkbutton $base.mtframe.childsite.chkbel \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 2 \
        -text {  2 elastic} -variable el 
    checkbutton $base.mtframe.childsite.chkbinel \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 4 \
        -text {  4 inelastic} -variable inel 
    checkbutton $base.mtframe.childsite.chkbn2n \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 16 \
        -text { 16 (n,2n)} -variable n2n 
    checkbutton $base.mtframe.childsite.chkbn3n \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 17 \
        -text { 17 (n,3n)} -variable n3n 
    checkbutton $base.mtframe.childsite.chkbnna \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 22 \
        -text { 22 (n,na)} -variable nna 
    checkbutton $base.mtframe.childsite.chkbnnp \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 28 \
        -text { 28 (n,np)} -variable nnp 
    checkbutton $base.mtframe.childsite.chkbnnpa \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -command {} -foreground #ffffff \
        -highlightbackground #4b7b82 -highlightcolor #ffffff -justify left \
        -offvalue {} -onvalue 45 -text { 45 (n,npa)} -variable nnpa 
    checkbutton $base.mtframe.childsite.chkbng \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 102 \
        -text {102 (n,g)} -variable ng 
    checkbutton $base.mtframe.childsite.chkbnp \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 103 \
        -text {103 (n,p)} -variable np 
    checkbutton $base.mtframe.childsite.chkbna \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 107 \
        -text {107 (n,a)} -variable na 
    checkbutton $base.mtframe.childsite.chkbnpa \
        -activebackground #609498 -activeforeground #ffffff \
        -background #4b7b82 -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 112 \
        -text {112 (n,pa)} -variable npa 
    grid $base.mtframe.childsite.chkbtot \
        -in $base.mtframe.childsite -column 0 -row 0 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbel \
        -in $base.mtframe.childsite -column 0 -row 1 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbinel \
        -in $base.mtframe.childsite -column 0 -row 2 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbn2n \
        -in $base.mtframe.childsite -column 0 -row 3 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbn3n \
        -in $base.mtframe.childsite -column 0 -row 4 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbnna \
        -in $base.mtframe.childsite -column 0 -row 5 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbnnp \
        -in $base.mtframe.childsite -column 0 -row 6 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbnnpa \
        -in $base.mtframe.childsite -column 0 -row 7 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbng \
        -in $base.mtframe.childsite -column 0 -row 8 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbnp \
        -in $base.mtframe.childsite -column 0 -row 9 -columnspan 1 -rowspan 1 \
        -sticky w 
    grid $base.mtframe.childsite.chkbna \
        -in $base.mtframe.childsite -column 0 -row 10 -columnspan 1 \
        -rowspan 1 -sticky w 
    grid $base.mtframe.childsite.chkbnpa \
        -in $base.mtframe.childsite -column 0 -row 11 -columnspan 1 \
        -rowspan 1 -sticky w 
    button $base.show \
        -activebackground #007900 -activeforeground white -background #005a00 \
        -command {set blank " "
set zvd ""
set suff ""
if {$suf != ""} {set suff -$suf}
set mts [list $tot $el $inel $n2n $n3n $nna $nnp $nnpa $ng $np $na $npa]
foreach i $mts {
    if {$i != ""} {lappend  zvd $root-$i$suff.zvd}
}
exec xterm -e showzvd $zvd} \
        -foreground white -highlightbackground #4b7b82 -highlightcolor white \
        -text {Show selected plots} -width 15 
    button $base.merge \
        -activebackground #007900 -activeforeground white -background #005a00 \
        -command {set blank " "
set zvd ""
set suff ""
if {$suf != ""} {set suff -$suf}
set mts [list $tot $el $inel $n2n $n3n $nna $nnp $nnpa $ng $np $na $npa]
foreach i $mts {
    if {$i != ""} {lappend  zvd $root-$i$suff.zvd}
}
exec xterm -e zvv $zvd &
#exec showzvd $zvd} \
        -foreground white -highlightbackground #4b7b82 -highlightcolor white \
        -text {Merge selected plots} -width 15 
    button $base.exit \
        -activebackground #f00000 -activeforeground white -background darkred \
        -command {set rcfl [open .guizvvrc w]
puts $rcfl "$root"
puts $rcfl "$suf"
puts $rcfl "$name1" 
puts $rcfl "$dir1"
puts $rcfl "$name2" 
puts $rcfl "$dir2"
puts $rcfl "$name3" 
puts $rcfl "$dir3"
# Flush and close the file
flush $rcfl
close $rcfl
exit} \
        -foreground white -highlightcolor white -text Exit \
        -width 15 
    tixFileEntry $base.set1-entry \
        -value . -variable dir1 -label {Directory 1:} \
        -options {label.anchor e} 
    bind $base.set1-entry <FocusIn> {
        focus .topwindow.set1-entry.frame.entry
    }
.topwindow.set1-entry subwidget frame configure -highlightthickness 2
    label $base.suf-lab \
        -background #4b7b82 -foreground white -pady 1 \
        -text {Sufix for plots:} 
    entry $base.sufix \
        -background white -highlightbackground white -textvariable suf \
        -width 17 
    label $base.dir-lab \
        -background #4b7b82 -foreground white \
        -text {Path to directories with ENDF file:} 
    tixFileEntry $base.dir2-entry \
        -variable dir2 -label {Directory 2:} -options {label.anchor e} 
    bind $base.dir2-entry <FocusIn> {
        focus .topwindow.dir2-entry.frame.entry
    }
.topwindow.dir2-entry subwidget frame configure -highlightthickness 2
    tixFileEntry $base.dir3-entry \
        -variable dir3 -label {Directory 3:} -options {label.anchor e} 
    bind $base.dir3-entry <FocusIn> {
        focus .topwindow.dir3-entry.frame.entry
    }
.topwindow.dir3-entry subwidget frame configure -highlightthickness 2
    button $base.creatszvd \
        -activebackground #df5130 -activeforeground white -background #be5a41 \
        -command {set suff -$suf
set mts [list $tot $el $inel $n2n $n3n $nna $nnp $nnpa $ng $np $na $npa]
foreach i $mts {
if {$i != ""} {exec xterm -e mtacomp $i $suff $root $dir1 $name1 $dir2 $name2 $dir3 $name3}
}} \
        -foreground white -highlightbackground #4b7b82 -highlightcolor white \
        -pady 1m -text {Create selected} -width 15 
    button $base.creatazvd \
        -activebackground #df5130 -activeforeground white -background #be5a41 \
        -command set\ suff\ -\$suf\nexec\ xterm\ -e\ acomp\ \$suff\ \$root\ \$dir1\ \$name1\ \$dir2\ \$name2\ \$dir3\ \$name3\} \
        -foreground white -highlightbackground #4b7b82 -highlightcolor white \
        -text {Create all} -width 15 
    button $base.showall \
        -activebackground #007900 -activeforeground white -background #005a00 \
        -command {set zvd ""
set suff ""
if {$suf != ""} {set suff -$suf}
set mts [list 1 2 4 16 17 22 28 45 102 103 107 112]
foreach i $mts {
    if {$i != ""} {lappend  zvd $root-$i$suff.zvd}
}
exec xterm -e showzvd $zvd} \
        -foreground white -highlightbackground #4b7b82 -highlightcolor white \
        -text {Show all} -width 15 
    label $base.sep-lab \
        -background #4b7b82 -text {  } 
    label $base.curv1-lab \
        -background #4b7b82 -foreground white -text {Label for curve 1:} 
    label $base.label53 \
        -background #4b7b82 -foreground white -text {Label for curve 2:} 
    label $base.label54 \
        -background #4b7b82 -foreground white -text {Label for curve 3:} 
    entry $base.curv1-ent \
        -background white -foreground darkgreen -textvariable name1 -width 17 
    entry $base.entry55 \
        -background white -foreground darkred -textvariable name2 -width 17 
    entry $base.entry56 \
        -background white -foreground darkblue -textvariable name3 -width 17 
    ###################
    # SETTING GEOMETRY
    ###################
    grid $base.title \
        -in $base -column 0 -row 0 -columnspan 2 -rowspan 1 -pady 10 
    grid $base.project-lab \
        -in $base -column 0 -row 1 -columnspan 1 -rowspan 1 -pady 5 -sticky w 
    grid $base.project \
        -in $base -column 1 -row 1 -columnspan 1 -rowspan 1 -pady 5 -sticky w 
    grid $base.mtframe \
        -in $base -column 0 -row 12 -columnspan 1 -rowspan 6 -sticky w 
    grid columnconf $base.mtframe 1 -weight 1
    grid rowconf $base.mtframe 1 -weight 1
    grid rowconf $base.mtframe 0 -minsize 8
    grid $base.show \
        -in $base -column 1 -row 14 -columnspan 1 -rowspan 1 -sticky s 
    grid $base.merge \
        -in $base -column 1 -row 16 -columnspan 1 -rowspan 1 
    grid $base.exit \
        -in $base -column 1 -row 17 -columnspan 1 -rowspan 1 
    grid $base.set1-entry \
        -in $base -column 0 -row 5 -columnspan 2 -rowspan 1 -sticky w 
    grid $base.suf-lab \
        -in $base -column 0 -row 3 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.sufix \
        -in $base -column 1 -row 3 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.dir-lab \
        -in $base -column 0 -row 4 -columnspan 2 -rowspan 1 -pady 10 \
        -sticky w 
    grid $base.dir2-entry \
        -in $base -column 0 -row 7 -columnspan 2 -rowspan 1 -sticky w 
    grid $base.dir3-entry \
        -in $base -column 0 -row 9 -columnspan 2 -rowspan 1 -sticky w 
    grid $base.creatszvd \
        -in $base -column 1 -row 12 -columnspan 1 -rowspan 1 -sticky s 
    grid $base.creatazvd \
        -in $base -column 1 -row 13 -columnspan 1 -rowspan 1 -sticky n 
    grid $base.showall \
        -in $base -column 1 -row 15 -columnspan 1 -rowspan 1 -sticky n 
    grid $base.sep-lab \
        -in $base -column 0 -row 11 -columnspan 2 -rowspan 1 
    grid $base.curv1-lab \
        -in $base -column 0 -row 6 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.label53 \
        -in $base -column 0 -row 8 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.label54 \
        -in $base -column 0 -row 10 -columnspan 1 -rowspan 1 -sticky w 
    grid $base.curv1-ent \
        -in $base -column 1 -row 6 -columnspan 1 -rowspan 1 
    grid $base.entry55 \
        -in $base -column 1 -row 8 -columnspan 1 -rowspan 1 
    grid $base.entry56 \
        -in $base -column 1 -row 10 -columnspan 1 -rowspan 1 
}

Window show .
Window show .topwindow

main $argc $argv
set rcfl [open .guizvvrc r+]
gets $rcfl root
gets $rcfl suf
gets $rcfl name1 
gets $rcfl dir1
gets $rcfl name2 
gets $rcfl dir2
gets $rcfl name3 
gets $rcfl dir3
close $rcfl
if {$argc > 0} {set root [lindex $argv 0]}
if {$argc > 1} {set suf [lindex $argv 1]}
if {$suf == "" && $name2 != ""} {set suf "comp"}
if {$name2 == ""} {set suf ""}
if {$name1 == ""} {set name1 "Empire-II"}
if {$dir1 == ""} {set dir1 [pwd]}
