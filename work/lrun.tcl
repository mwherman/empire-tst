#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {
    switch $tcl_platform(platform) {
	windows {
	}
	default {
	    option add *Scrollbar.width 10
	}
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
    namespace eval ::widgets::.srun {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1 -menu 1}
    }
    namespace eval ::widgets::.srun.input {
        array set save {-background 1 -borderwidth 1 -closeenough 1 -confine 1 -height 1 -highlightbackground 1 -highlightcolor 1 -relief 1 -selectbackground 1 -width 1}
    }
    namespace eval ::widgets::.srun.input.project {
        array set save {-background 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::.srun.input.but31 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but18 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but17 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but36 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but19 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but20 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.viewlong {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.srun.input.but27 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but22 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but23 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but28 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but25 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but21 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.titel {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.source {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but26 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.modulist {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -indicatoron 1 -menu 1 -padx 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.modulist.m {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.srun.input.sourcemenu {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -indicatoron 1 -menu 1 -padx 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.sourcemenu.m {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1}
    }
    namespace eval ::widgets::.srun.input.but30 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.input {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.lab34 {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.lab35 {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.lab37 {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but29 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.browse {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.men19 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.men19.m {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1}
    }
    namespace eval ::widgets::.srun.input.but33 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but32 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.lab33 {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but34 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but35 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but24 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but37 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but38 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.lab17 {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.lab18 {
        array set save {-background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.but39 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.srun.input.but40 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.button53 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.srun.input.button54 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.input.zvvgui {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -height 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.srun.cpd32 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -highlightcolor 1 -width 1}
    }
    namespace eval ::widgets::.srun.cpd32.05 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.cpd32.05.06 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.srun.cpd32.01 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::.srun.cpd32.01.02 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.srun.cpd32.men40 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::.srun.cpd32.men40.01 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.srun.cpd32.men40.01.02 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.srun.cpd32.men40.01.men41 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
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

proc vTclWindow.srun {base {container 0}} {
    if {$base == ""} {
        set base .srun
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
        -background #dcdcdc -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -menu "$base.m17" 
    wm focusmodel $base passive
    wm geometry $base 211x947+4+64; update
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Empire control"
    }
    canvas $base.input \
        -background #dcdcdc -borderwidth 2 -closeenough 1.0 -confine 0 \
        -height 207 -highlightbackground #304d49 -highlightcolor black \
        -relief groove -selectbackground #365e66 -width 296 
    entry $base.input.project \
        -background #ffffff \
        -font -adobe-helvetica-medium-r-normal--16-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #374d4e \
        -highlightcolor #ffffff -justify right -selectbackground red \
        -selectforeground #ffffff -textvariable file 
    button $base.input.but31 \
        -activebackground #283cb4 -activeforeground #ffffff \
        -background #283e76 \
        -command {file copy skel.inp $file.inp
exec $editor $file.inp &} \
        -font -adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Create } 
    button $base.input.but18 \
        -activebackground #283cb4 -activeforeground #fffefe \
        -background #283e76 -command {exec $editor $file.inp &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Edit } 
    button $base.input.but17 \
        -activebackground #df5130 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e run $file &} \
        -font -adobe-helvetica-medium-r-bold--14-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Run+Format+Plot 
    button $base.input.but36 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e runE $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Run 
    button $base.input.but19 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e format $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Format 
    button $base.input.but20 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e plot $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Plot 
    button $base.input.viewlong \
        -activebackground #007900 -activeforeground #fefefe \
        -background #005a00 -command {exec $editor $file.lst &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Emp-full -width 0 
    button $base.input.but27 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.out  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Emp-short } 
    button $base.input.but22 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.exf &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text EXFOR 
    button $base.input.but23 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.c4 &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {C4 file} 
    button $base.input.but28 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.endf  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text ENDF 
    button $base.input.but25 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec gv $file-cum.ps &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Levels 
    button $base.input.but21 \
        -activebackground #f00000 -activeforeground #fffefe \
        -background #860000 -command {exec clean $file core ../util/*/core &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Clean 
    label $base.input.titel \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text {EMPIRE } 
    label $base.input.source \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text {Modify source} 
    button $base.input.but26 \
        -activebackground #f00000 -activeforeground #fefefe \
        -background #860000 -command {exec $editor ../source/dimension.h &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Edit dimensions} 
    menubutton $base.input.modulist \
        -activebackground #f00000 -activeforeground #fefefe \
        -background #860000 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -indicatoron 1 -menu "$base.input.modulist.m" \
        -padx 4 -pady 3 -relief raised -text {Edit module} 
    menu $base.input.modulist.m \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    menubutton $base.input.sourcemenu \
        -activebackground #f00000 -activeforeground black \
        -background #860000 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -indicatoron 1 \
        -menu "$base.input.sourcemenu.m" -padx 4 -pady 3 -relief raised \
        -text {Edit  module} 
    menu $base.input.sourcemenu.m \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/main.f &} -font {} \
        -foreground {} -image {} -label Main 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/input.f &} -font {} \
        -foreground {} -image {} -label Input 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/OM-scat2.f &} \
        -font {} -foreground {} -image {} -label OM-scat2 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/MSD-orion.f &} \
        -font {} -foreground {} -image {} -label MSD-orion 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/MSD-tristan.f &} \
        -font {} -foreground {} -image {} -label MSD-tristan 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/MSC-NVWY.f &} \
        -font {} -foreground {} -image {} -label MSC-NVWY 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/HF-comp.f &} -font {} \
        -foreground {} -image {} -label HF-comp 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/HRTW-comp.f &} \
        -font {} -foreground {} -image {} -label HRTW-comp 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/ddhms.f &} -font {} \
        -foreground {} -image {} -label HMS 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/degas.f &} -font {} \
        -foreground {} -image {} -label DEGAS 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/ecis.f &} -font {} \
        -foreground {} -image {} -label ECIS 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/ccfus.f &} -font {} \
        -foreground {} -image {} -label CCFUS 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/fusion.f &} -font {} \
        -foreground {} -image {} -label Fusion 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/gamma-strgth.f &} \
        -font {} -foreground {} -image {} -label gamma-strgth 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/lev-dens.f &} \
        -font {} -foreground {} -image {} -label Lev-dens 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/ph-lev-dens.f &} \
        -font {} -foreground {} -image {} -label ph-lev-dens 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/print.f &} -font {} \
        -foreground {} -image {} -label Print 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/tl.f &} -font {} \
        -foreground {} -image {} -label Tl 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/auxiliary.f &} \
        -font {} -foreground {} -image {} -label Auxiliary 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/scnd-preeq.f &} \
        -font {} -foreground {} -image {} -label Scnd-preeq 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/global.h &} -font {} \
        -foreground {} -image {} -label global.h 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/io.h &} -font {} \
        -foreground {} -image {} -label io.h 
    $base.input.sourcemenu.m add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/ddhms.cmb &} -font {} \
        -foreground {} -image {} -label ddhms.cmb 
    button $base.input.but30 \
        -activebackground #df5130 -activeforeground #fffefe \
        -background #be5a41 \
        -command {cd ../source
exec xterm -e make &
cd ../work} \
        -font -adobe-helvetica-medium-r-bold--14-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Make 
    label $base.input.input \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text Input: 
    label $base.input.lab34 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text Execute: 
    label $base.input.lab35 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-normal--16-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text {or :} 
    label $base.input.lab37 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text Outputs: 
    button $base.input.but29 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.int  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Int. omp} 
    button $base.input.browse \
        -activebackground #283cb4 -activeforeground #ffffff \
        -background #283e76 \
        -command {set defile  [exec xgetfile -title "select input file" -pattern *.inp ] 
set dfile [file rootname $defile]
set file [file tail $dfile]} \
        -font -adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #283cb4 \
        -highlightcolor #283e76 -padx 9 -pady 3 -text Project: 
    menubutton $base.input.men19 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -menu "$base.input.men19.m" -padx 4 -pady 3 \
        -relief raised -text {Select MT and >} 
    menu $base.input.men19.m \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black 
    $base.input.men19.m add radiobutton \
        -value {MT=1 (total)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {1}} -font {} -foreground {} -image {} \
        -label {MT=1 (total)} 
    $base.input.men19.m add radiobutton \
        -value {MT=2 (elas)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {2}} -font {} -foreground {} -image {} \
        -label {MT=2 (elas)} 
    $base.input.men19.m add radiobutton \
        -value {MT=4 (n,n')} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {4}} -font {} -foreground {} -image {} \
        -label {MT=4 (n,n')} 
    $base.input.men19.m add radiobutton \
        -value {MT=16 (n,2n)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {16}} -font {} -foreground {} -image {} \
        -label {MT=16 (n,2n)} 
    $base.input.men19.m add radiobutton \
        -value {MT=17 (n,3n)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {17}} -font {} -foreground {} -image {} \
        -label {MT=17 (n,3n)} 
    $base.input.men19.m add radiobutton \
        -value {MT=18 (n,f)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {18}} -font {} -foreground {} -image {} \
        -label {MT=18 (n,f)} 
    $base.input.men19.m add radiobutton \
        -value {MT=22 (n,na)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {22}} -font {} -foreground {} -image {} \
        -label {MT=22 (n,na)} 
    $base.input.men19.m add radiobutton \
        -value {MT=28 (n,np)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {28}} -font {} -foreground {} -image {} \
        -label {MT=28 (n,np)} 
    $base.input.men19.m add radiobutton \
        -value {MT=45 (n,npa)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {45}} -font {} -foreground {} -image {} \
        -label {MT=45 (n,npa)} 
    $base.input.men19.m add radiobutton \
        -value {MT=102 (n,g)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {102}} -font {} -foreground {} -image {} \
        -label {MT=102 (n,g)} 
    $base.input.men19.m add radiobutton \
        -value {MT=103 (n,p)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {103}} -font {} -foreground {} -image {} \
        -label {MT=103 (n,p)} 
    $base.input.men19.m add radiobutton \
        -value {MT=107 (n,a)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {107}} -font {} -foreground {} -image {} \
        -label {MT=107 (n,a)} 
    $base.input.men19.m add radiobutton \
        -value {MT=112 (n,pa)} -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set MT {112}} -font {} -foreground {} -image {} \
        -label {MT=112 (n,pa)} 
    button $base.input.but33 \
        -activebackground #dc5032 -activeforeground #ffffff \
        -background #be5a41 -command {exec xterm -e zvd $file $MT &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text ZVV 
    button $base.input.but32 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-ecis.out  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Ecis 
    label $base.input.lab33 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text Out/In: 
    button $base.input.but34 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-lev.col  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Coll. levels} 
    button $base.input.but35 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.lev  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Levels 
    button $base.input.but24 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.ripl &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {RIPL omp} 
    button $base.input.but37 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-ecis.in &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {ECIS inp} 
    button $base.input.but38 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.dir &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Dir. omp} 
    label $base.input.lab17 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text Plots: 
    label $base.input.lab18 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc \
        -highlightcolor black -text Outputs: 
    button $base.input.but39 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e zvpl $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Create ZVV} -width 0 
    button $base.input.but40 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e zvcomb &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Merge ZVV} 
    button $base.input.button53 \
        -activebackground #007900 -activeforeground orange \
        -background #005a00 \
        -command {exec xterm -bg darkorange -title WARNINGS -e less $file.war &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground orange -height 26 -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Warnings -width 73 
    button $base.input.button54 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec gv -landscape $file.ps &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Plotc4 
    button $base.input.zvvgui \
        -activebackground #007900 -activeforeground white -background #005a00 \
        -command {exec guizvv.tcl $file &} -font {Helvetica -12} -foreground white \
        -height 28 -text {Compare ENDF files} -width 143 
    frame $base.cpd32 \
        -background #dcdcdc -borderwidth 1 -height 30 \
        -highlightbackground #dcdcdc -highlightcolor #ffffff -width 30 
    menubutton $base.cpd32.05 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -height 1 -highlightbackground #dcdcdc \
        -highlightcolor black -menu "$base.cpd32.05.06" -padx 4 -pady 3 \
        -text Help 
    menu $base.cpd32.05.06 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor inplist &} -font {} \
        -foreground {} -image {} -label {EMPIRE input} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../RIPL-2/OPTICAL/OM-DATA/OM-INDEX.TXT &} \
        -font {} -foreground {} -image {} -label RIPL-omp 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../hints.txt &} -font {} \
        -foreground {} -image {} -label FAQ 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec gv ../doc/empire.ps &} \
        -font {} -foreground {} -image {} -label {EMPIRE manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/empend/manual.txt &} \
        -font {} -foreground {} -image {} -label {EMPEND manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/c4sort/manual.txt &} \
        -font {} -foreground {} -image {} -label {C4SORT manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/legend/manual.txt &} \
        -font {} -foreground {} -image {} -label {LEGEND manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/plotc4/manual.txt &} \
        -font {} -foreground {} -image {} -label {PLOTC4 manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/x4toc4/manual.txt &} \
        -font {} -foreground {} -image {} -label {X4TOC4 manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/fixup/manual.txt &} \
        -font {} -foreground {} -image {} -label {FIXUP manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/lsttab/manual.txt &} \
        -font {} -foreground {} -image {} -label {LSTTAB manual} 
    $base.cpd32.05.06 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/sixtab/manual.txt &} \
        -font {} -foreground {} -image {} -label {SIXTAB manual} 
    menubutton $base.cpd32.01 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -height 1 -highlightbackground #dcdcdc \
        -highlightcolor black -menu "$base.cpd32.01.02" -padx 4 -pady 3 \
        -text File 
    menu $base.cpd32.01.02 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $base.cpd32.01.02 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} \
        -command {set ifile [open listpro w+]
set inp_files [glob -nocomplain *.inp]

foreach name $inp_files {
  puts  $ifile [ format "%-15s " $name ]
  }
  
close $ifile  
exec $editor listpro &} \
        -font {} -foreground {} -image {} -label {List projects} 
    $base.cpd32.01.02 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec rm  $file\.* $file-*\.*} -font {} \
        -foreground {} -image {} -label {Remove project} 
    $base.cpd32.01.02 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command exit -font {} -foreground {} -image {} \
        -label Exit 
    menubutton $base.cpd32.men40 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -height 1 -highlightbackground #dcdcdc \
        -highlightcolor black -menu "$base.cpd32.men40.01" -padx 4 -pady 3 \
        -text Options -width 6 
    menu $base.cpd32.men40.01 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $base.cpd32.men40.01 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor skel.inp &} -font {} \
        -foreground {} -image {} -label {Default input} 
    $base.cpd32.men40.01 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor EMPEND.INP &} -font {} \
        -foreground {} -image {} -label {EMPEND input} 
    $base.cpd32.men40.01 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/plotc4/PLOTC4.INP &} \
        -font {} -foreground {} -image {} -label {PLOTC4 input} 
    $base.cpd32.men40.01 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/fixup/FIXUP.INP &} \
        -font {} -foreground {} -image {} -label {FIXUP input} 
    $base.cpd32.men40.01 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../util/c4sort/C4SORT.INP &} \
        -font {} -foreground {} -image {} -label {C4SORT input} 
    $base.cpd32.men40.01 add command \
        -activebackground {} -activeforeground {} -accelerator {} \
        -background {} -command {exec $editor ../source/Makefile &} -font {} \
        -foreground {} -image {} -label {Edit Makefile} 
    $base.cpd32.men40.01 add cascade \
        -menu "$base.cpd32.men40.01.men41" -activebackground {} \
        -activeforeground {} -accelerator {} -background {} -command {} \
        -font {} -foreground {} -image {} -label {Select editor} 
    menu $base.cpd32.men40.01.02 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $base.cpd32.men40.01.02 add radiobutton \
        -value gvim -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {gvim}} -font {} -foreground {} -image {} \
        -label gvim 
    $base.cpd32.men40.01.02 add radiobutton \
        -value emacs -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {emacs}} -font {} -foreground {} -image {} \
        -label emacs 
    $base.cpd32.men40.01.02 add radiobutton \
        -value jove -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {jove}} -font {} -foreground {} -image {} \
        -label jove 
    $base.cpd32.men40.01.02 add radiobutton \
        -value nedit -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor nedit} -font {} -foreground {} -image {} \
        -label nedit 
    $base.cpd32.men40.01.02 add radiobutton \
        -value kedit -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {kedit}} -font {} -foreground {} -image {} \
        -label kedit 
    $base.cpd32.men40.01.02 add radiobutton \
        -value GXedit -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {gxedit}} -font {} -foreground {} -image {} \
        -label GXedit 
    $base.cpd32.men40.01.02 invoke 0

    menu $base.cpd32.men40.01.men41 \
        -activebackground #dcdcdc -activeforeground black \
        -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value gvim -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {gvim}} -font {} -foreground {} -image {} \
        -label gvim 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value emacs -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {emacs}} -font {} -foreground {} -image {} \
        -label emacs 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value nedit -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {nedit}} -font {} -foreground {} -image {} \
        -label nedit 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value kedit -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {kedit}} -font {} -foreground {} -image {} \
        -label kedit 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value jove -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {jove}} -font {} -foreground {} -image {} \
        -label jove 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value GXedit -variable selectedButton -activebackground {} \
        -activeforeground {} -accelerator {} -background {} \
        -command {set editor {gxedit}} -font {} -foreground {} -image {} \
        -label GXedit 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.input \
        -x 10 -y 35 -width 192 -height 898 -anchor nw -bordermode ignore 
    place $base.input.project \
        -x 80 -y 30 -width 83 -height 27 -anchor nw -bordermode ignore 
    place $base.input.but31 \
        -x 20 -y 85 -width 73 -height 26 -anchor nw 
    place $base.input.but18 \
        -x 93 -y 85 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but17 \
        -x 20 -y 145 -width 146 -height 41 -anchor nw -bordermode ignore 
    place $base.input.but36 \
        -x 20 -y 210 -width 146 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but19 \
        -x 20 -y 235 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but20 \
        -x 93 -y 235 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.viewlong \
        -x 20 -y 440 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but27 \
        -x 20 -y 465 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but22 \
        -x 93 -y 465 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but23 \
        -x 20 -y 300 -width 146 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but28 \
        -x 93 -y 490 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but25 \
        -x 20 -y 555 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but21 \
        -x 70 -y 675 -width 53 -height 53 -anchor nw -bordermode ignore 
    place $base.input.titel \
        -x 55 -y 5 -anchor nw -bordermode ignore 
    place $base.input.source \
        -x 20 -y 730 -width 146 -height 33 -anchor nw -bordermode ignore 
    place $base.input.but26 \
        -x 25 -y 765 -width 138 -height 26 -anchor nw -bordermode ignore 
    place $base.input.sourcemenu \
        -x 25 -y 790 -width 137 -height 24 -anchor nw -bordermode ignore 
    place $base.input.but30 \
        -x 70 -y 825 -width 53 -height 53 -anchor nw -bordermode ignore 
    place $base.input.input \
        -x 70 -y 60 -anchor nw -bordermode ignore 
    place $base.input.lab34 \
        -x 60 -y 115 -anchor nw -bordermode ignore 
    place $base.input.lab35 \
        -x 85 -y 185 -anchor nw -bordermode ignore 
    place $base.input.lab37 \
        -x 55 -y 410 -anchor nw -bordermode ignore 
    place $base.input.but29 \
        -x 20 -y 325 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.browse \
        -x 20 -y 30 -width 62 -height 27 -anchor nw -bordermode ignore 
    place $base.input.men19 \
        -x 20 -y 580 -width 103 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but33 \
        -x 125 -y 580 -width 41 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but32 \
        -x 20 -y 490 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.lab33 \
        -x 60 -y 270 -anchor nw -bordermode ignore 
    place $base.input.but34 \
        -x 93 -y 350 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but35 \
        -x 93 -y 325 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but24 \
        -x 20 -y 350 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but37 \
        -x 93 -y 375 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but38 \
        -x 20 -y 375 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.lab17 \
        -x 70 -y 525 -anchor nw -bordermode ignore 
    place $base.input.lab18 \
        -x 55 -y 410 -anchor nw -bordermode ignore 
    place $base.input.but39 \
        -x 20 -y 605 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but40 \
        -x 93 -y 605 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.button53 \
        -x 93 -y 440 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.button54 \
        -x 93 -y 555 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.zvvgui \
        -x 20 -y 630 -width 146 -height 26 -anchor nw -bordermode ignore 
    place $base.cpd32 \
        -x 5 -y 5 -width 197 -height 26 -anchor nw 
    pack $base.cpd32.05 \
        -in $base.cpd32 -anchor center -expand 0 -fill none -side right 
    pack $base.cpd32.01 \
        -in $base.cpd32 -anchor nw -expand 0 -fill none -side left 
    place $base.cpd32.men40 \
        -x 36 -y 1 -width 59 -height 24 -anchor nw 
}

Window show .
Window show .srun

main $argc $argv
