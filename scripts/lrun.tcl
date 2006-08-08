#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {

    package require Tk
    switch $tcl_platform(platform) {
	windows {
            option add *Button.padY 0
	}
	default {
            option add *Scrollbar.width 10
            option add *Scrollbar.highlightThickness 0
            option add *Scrollbar.elementBorderWidth 2
            option add *Scrollbar.borderWidth 2
	}
    }
    
}

#############################################################################
# Visual Tcl v1.60 Project
#


#################################
# VTCL LIBRARY PROCEDURES
#

if {![info exists vTcl(sourcing)]} {
#############################################################################
## Library Procedure:  Window

proc ::Window {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    global vTcl
    foreach {cmd name newname} [lrange $args 0 2] {}
    set rest    [lrange $args 3 end]
    if {$name == "" || $cmd == ""} { return }
    if {$newname == ""} { set newname $name }
    if {$name == "."} { wm withdraw $name; return }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists} {
                wm deiconify $newname
            } elseif {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[winfo exists $newname] && [wm state $newname] == "normal"} {
                vTcl:FireEvent $newname <<Show>>
            }
        }
        hide    {
            if {$exists} {
                wm withdraw $newname
                vTcl:FireEvent $newname <<Hide>>
                return}
        }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}
#############################################################################
## Library Procedure:  vTcl:DefineAlias

proc ::vTcl:DefineAlias {target alias widgetProc top_or_alias cmdalias} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

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
#############################################################################
## Library Procedure:  vTcl:DoCmdOption

proc ::vTcl:DoCmdOption {target cmd} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    ## menus are considered toplevel windows
    set parent $target
    while {[winfo class $parent] == "Menu"} {
        set parent [winfo parent $parent]
    }

    regsub -all {\%widget} $cmd $target cmd
    regsub -all {\%top} $cmd [winfo toplevel $parent] cmd

    uplevel #0 [list eval $cmd]
}
#############################################################################
## Library Procedure:  vTcl:FireEvent

proc ::vTcl:FireEvent {target event {params {}}} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    ## The window may have disappeared
    if {![winfo exists $target]} return
    ## Process each binding tag, looking for the event
    foreach bindtag [bindtags $target] {
        set tag_events [bind $bindtag]
        set stop_processing 0
        foreach tag_event $tag_events {
            if {$tag_event == $event} {
                set bind_code [bind $bindtag $tag_event]
                foreach rep "\{%W $target\} $params" {
                    regsub -all [lindex $rep 0] $bind_code [lindex $rep 1] bind_code
                }
                set result [catch {uplevel #0 $bind_code} errortext]
                if {$result == 3} {
                    ## break exception, stop processing
                    set stop_processing 1
                } elseif {$result != 0} {
                    bgerror $errortext
                }
                break
            }
        }
        if {$stop_processing} {break}
    }
}
#############################################################################
## Library Procedure:  vTcl:Toplevel:WidgetProc

proc ::vTcl:Toplevel:WidgetProc {w args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[llength $args] == 0} {
        ## If no arguments, returns the path the alias points to
        return $w
    }
    set command [lindex $args 0]
    set args [lrange $args 1 end]
    switch -- [string tolower $command] {
        "setvar" {
            foreach {varname value} $args {}
            if {$value == ""} {
                return [set ::${w}::${varname}]
            } else {
                return [set ::${w}::${varname} $value]
            }
        }
        "hide" - "show" {
            Window [string tolower $command] $w
        }
        "showmodal" {
            ## modal dialog ends when window is destroyed
            Window show $w; raise $w
            grab $w; tkwait window $w; grab release $w
        }
        "startmodal" {
            ## ends when endmodal called
            Window show $w; raise $w
            set ::${w}::_modal 1
            grab $w; tkwait variable ::${w}::_modal; grab release $w
        }
        "endmodal" {
            ## ends modal dialog started with startmodal, argument is var name
            set ::${w}::_modal 0
            Window hide $w
        }
        default {
            uplevel $w $command $args
        }
    }
}
#############################################################################
## Library Procedure:  vTcl:WidgetProc

proc ::vTcl:WidgetProc {w args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[llength $args] == 0} {
        ## If no arguments, returns the path the alias points to
        return $w
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
    uplevel $w $command $args
}
#############################################################################
## Library Procedure:  vTcl:toplevel

proc ::vTcl:toplevel {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    uplevel #0 eval toplevel $args
    set target [lindex $args 0]
    namespace eval ::$target {set _modal 0}
}
}


if {[info exists vTcl(sourcing)]} {

proc vTcl:project:info {} {
    set base .srun
    namespace eval ::widgets::$base {
    }
    namespace eval ::widgets::$base.input {
        array set save {-background 1 -borderwidth 1 -closeenough 1 -confine 1 -height 1 -highlightbackground 1 -highlightcolor 1 -relief 1 -selectbackground 1 -width 1}
    }
    namespace eval ::widgets::$base.cpd32 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -highlightcolor 1 -width 1}
    }
    set site_3_0 $base.cpd32
    namespace eval ::widgets::$site_3_0.05 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.05.06 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::$site_3_0.01 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.01.02 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::$site_3_0.men40 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -height 1 -highlightbackground 1 -highlightcolor 1 -menu 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_3_0.men40.01 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1}
        }
    }
    set site_5_0 $site_3_0.men40.01
    namespace eval ::widgets::$site_5_0.men41 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -font 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -value 1}
        }
    }
    set site_5_0 $site_3_0.men40.01
    namespace eval ::widgets::$site_5_0.men47 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    namespace eval ::widgets_bindings {
        set tagslist _TopLevel
    }
    namespace eval ::vTcl::modules::main {
        set procs {
            init
            main
            vTclWindow.
            vTclWindow.srun
        }
        set compounds {
        }
        set projectType single
    }
}
}

#################################
# USER DEFINED PROCEDURES
#
#############################################################################
## Procedure:  main

proc ::main {argc argv} {

}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {

}

init $argc $argv

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $top passive
    wm geometry $top 1x1+0+0; update
    wm maxsize $top 1585 1170
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm withdraw $top
    wm title $top "vtcl.tcl"
    bindtags $top "$top Vtcl.tcl all"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    ###################
    # SETTING GEOMETRY
    ###################

    vTcl:FireEvent $base <<Ready>>
}

proc vTclWindow.srun {base} {
    if {$base == ""} {
        set base .srun
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $top -class Toplevel \
        -menu "$top.m17" -background #dcdcdc -highlightbackground #dcdcdc \
        -highlightcolor #ffffff 
    wm focusmodel $top passive
    wm geometry $top 211x947+4+64; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Empire control"
    bindtags $top "$top Toplevel all _TopLevel"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    canvas $top.input \
        -background #dcdcdc -borderwidth 2 -closeenough 1.0 -confine 0 \
        -height 207 -highlightbackground #304d49 -highlightcolor black \
        -relief groove -selectbackground #365e66 -width 296 
    entry $top.input.project \
        -background #ffffff \
        -font -adobe-helvetica-medium-r-normal--16-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #374d4e \
        -highlightcolor #ffffff -justify right -selectbackground red \
        -selectforeground #ffffff -textvariable file 
    button $top.input.but31 \
        -activebackground #283cb4 -activeforeground #ffffff \
        -background #283e76 \
        -command {file copy skel.inp $file.inp
exec $editor $file.inp &} \
        -font -adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Create } 
    button $top.input.but18 \
        -activebackground #283cb4 -activeforeground #fffefe \
        -background #283e76 -command {exec $editor $file.inp &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Edit } 
    button $top.input.but17 \
        -activebackground #df5130 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e ../scripts/run $file &} \
        -font -adobe-helvetica-medium-r-bold--14-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Run+Format+Plot 
    button $top.input.but36 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e ../scripts/runE $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Run 
    button $top.input.but19 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 \
        -command {exec xterm -e ../scripts/format $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Format 
    button $top.input.but20 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e ../scripts/plot $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Plot 
    button $top.input.viewlong \
        -activebackground #007900 -activeforeground #fefefe \
        -background #005a00 -command {exec $editor $file.lst &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Emp-full -width 0 
    button $top.input.but27 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.out  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Emp-short } 
    button $top.input.but22 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.exf &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text EXFOR 
    button $top.input.but23 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.c4 &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {C4 file} 
    button $top.input.but28 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.endf  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text ENDF 
    button $top.input.but25 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec gv $file-cum.ps &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Levels 
    button $top.input.but21 \
        -activebackground #f00000 -activeforeground #fffefe \
        -background #860000 \
        -command {exec ../scripts/clean $file core ../util/*/core &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Clean 
    label $top.input.titel \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text {EMPIRE } 
    label $top.input.source \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text {Modify source} 
    button $top.input.but26 \
        -activebackground #f00000 -activeforeground #fefefe \
        -background #860000 -command {exec $editor ../source/dimension.h &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Edit dimensions} 
    menubutton $top.input.modulist \
        -activebackground #f00000 -activeforeground #fefefe \
        -background #860000 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -indicatoron 1 -menu "$top.input.modulist.m" \
        -padx 4 -pady 3 -relief raised -text {Edit module} 
    menu $top.input.modulist.m \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    menubutton $top.input.sourcemenu \
        -activebackground #f00000 -activeforeground black -background #860000 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -indicatoron 1 \
        -menu "$top.input.sourcemenu.m" -padx 4 -pady 3 -relief raised \
        -text {Edit  module} 
    menu $top.input.sourcemenu.m \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/main.f &} -label Main 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/input.f &} -label Input 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/OM-scat2.f &} -label OM-scat2 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/MSD-orion.f &} -label MSD-orion 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/MSD-tristan.f &} -label MSD-tristan 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/MSC-NVWY.f &} -label MSC-NVWY 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/HF-comp.f &} -label HF-comp 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/HRTW-comp.f &} -label HRTW-comp 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/ddhms.f &} -label HMS 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/degas.f &} -label DEGAS 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/ecis.f &} -label ECIS 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/ccfus.f &} -label CCFUS 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/fusion.f &} -label Fusion 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/gamma-strgth.f &} \
        -label gamma-strgth 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/lev-dens.f &} -label Lev-dens 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/ph-lev-dens.f &} -label ph-lev-dens 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/print.f &} -label Print 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/tl.f &} -label Tl 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/auxiliary.f &} -label Auxiliary 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/scnd-preeq.f &} -label Scnd-preeq 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/global.h &} -label global.h 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/io.h &} -label io.h 
    $top.input.sourcemenu.m add command \
        -command {exec $editor ../source/ddhms.cmb &} -label ddhms.cmb 
    button $top.input.but30 \
        -activebackground #df5130 -activeforeground #fffefe \
        -background #be5a41 \
        -command {cd ../source
exec xterm -e make &
cd ../work} \
        -font -adobe-helvetica-medium-r-bold--14-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Make 
    label $top.input.input \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text Input: 
    label $top.input.lab34 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text Execute: 
    label $top.input.lab35 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-normal--16-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text {or :} 
    label $top.input.lab37 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text Outputs: 
    button $top.input.but29 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.int  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Int. omp} 
    button $top.input.browse \
        -activebackground #283cb4 -activeforeground #ffffff \
        -background #283e76 \
        -command {set defile  [exec ../scripts/xgetfile -title "select input file" -pattern *.inp ] 
set dfile [file rootname $defile]
set file [file tail $dfile]} \
        -font -adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #283cb4 \
        -highlightcolor #283e76 -padx 9 -pady 3 -text Project: 
    menubutton $top.input.men19 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -menu "$top.input.men19.m" -padx 4 -pady 3 \
        -relief raised -text {Select MT and >} 
    menu $top.input.men19.m \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black 
    $top.input.men19.m add radiobutton \
        -value {MT=1 (total)} -command {set MT {1}} -label {MT=1 (total)} 
    $top.input.men19.m add radiobutton \
        -value {MT=2 (elas)} -command {set MT {2}} -label {MT=2 (elas)} 
    $top.input.men19.m add radiobutton \
        -value {MT=4 (n,n')} -command {set MT {4}} -label {MT=4 (n,n')} 
    $top.input.men19.m add radiobutton \
        -value {MT=16 (n,2n)} -command {set MT {16}} -label {MT=16 (n,2n)} 
    $top.input.men19.m add radiobutton \
        -value {MT=17 (n,3n)} -command {set MT {17}} -label {MT=17 (n,3n)} 
    $top.input.men19.m add radiobutton \
        -value {MT=18 (n,f)} -command {set MT {18}} -label {MT=18 (n,f)} 
    $top.input.men19.m add radiobutton \
        -value {MT=22 (n,na)} -command {set MT {22}} -label {MT=22 (n,na)} 
    $top.input.men19.m add radiobutton \
        -value {MT=28 (n,np)} -command {set MT {28}} -label {MT=28 (n,np)} 
    $top.input.men19.m add radiobutton \
        -value {MT=45 (n,npa)} -command {set MT {45}} -label {MT=45 (n,npa)} 
    $top.input.men19.m add radiobutton \
        -value {MT=102 (n,g)} -command {set MT {102}} -label {MT=102 (n,g)} 
    $top.input.men19.m add radiobutton \
        -value {MT=103 (n,p)} -command {set MT {103}} -label {MT=103 (n,p)} 
    $top.input.men19.m add radiobutton \
        -value {MT=107 (n,a)} -command {set MT {107}} -label {MT=107 (n,a)} 
    $top.input.men19.m add radiobutton \
        -value {MT=112 (n,pa)} -command {set MT {112}} -label {MT=112 (n,pa)} 
    button $top.input.but33 \
        -activebackground #dc5032 -activeforeground #ffffff \
        -background #be5a41 \
        -command {exec xterm -e ../scripts/zvd $file $MT &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text ZVV 
    button $top.input.but32 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-ecis.out  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Ecis 
    label $top.input.lab33 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text Out/In: 
    button $top.input.but34 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-lev.col  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Coll. levels} 
    button $top.input.but35 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.lev  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Levels 
    button $top.input.but24 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.ripl &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {RIPL omp} 
    button $top.input.but37 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-ecis.in &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {ECIS inp} 
    button $top.input.but38 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.dir &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Dir. omp} 
    label $top.input.lab17 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text Plots: 
    label $top.input.lab18 \
        -background #dcdcdc -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground black -highlightbackground #dcdcdc -highlightcolor black \
        -text Outputs: 
    button $top.input.but39 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e ../scripts/zvpl $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Create ZVV} -width 0 
    button $top.input.but40 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e ../scripts/zvcomb &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Merge ZVV} 
    button $top.input.button53 \
        -activebackground #007900 -activeforeground orange \
        -background #005a00 \
        -command {exec xterm -bg darkorange -title WARNINGS -e less $file.war &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground orange -height 26 -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Warnings -width 73 
    button $top.input.button54 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec gv -landscape $file.ps &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Plotc4 
    button $top.input.zvvgui \
        -activebackground #007900 -activeforeground white -background #005a00 \
        -command {exec ../scripts/guizvv.tcl $file &} -font {Helvetica -12} \
        -foreground white -height 28 -text {Compare ENDF files} -width 143 
    frame $top.cpd32 \
        -borderwidth 1 -background #dcdcdc -height 30 \
        -highlightbackground #dcdcdc -highlightcolor #ffffff -width 30 
    set site_3_0 $top.cpd32
    menubutton $site_3_0.05 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -height 1 -highlightbackground #dcdcdc \
        -highlightcolor black -menu "$site_3_0.05.06" -padx 4 -pady 3 \
        -text Help 
    menu $site_3_0.05.06 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $site_3_0.05.06 add command \
        -command {exec $editor inplist &} -label {EMPIRE input} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../RIPL-2/OPTICAL/OM-DATA/OM-INDEX.TXT &} \
        -label {RIPL o.m.p.} 
    $site_3_0.05.06 add command \
        \
        -command {exec $editor ../RIPL-2/OPTICAL/OM-DATA/OM-DEFORMATIONS.DAT &} \
        -label {RIPL coll. levels} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../hints.txt &} -label FAQ 
    $site_3_0.05.06 add command \
        -command {exec gv ../doc/empire.ps &} -label {EMPIRE manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/empend/manual.txt &} \
        -label {EMPEND manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/c4sort/manual.txt &} \
        -label {C4SORT manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/legend/manual.txt &} \
        -label {LEGEND manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/plotc4/manual.txt &} \
        -label {PLOTC4 manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/x4toc4/manual.txt &} \
        -label {X4TOC4 manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/fixup/manual.txt &} \
        -label {FIXUP manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/lsttab/manual.txt &} \
        -label {LSTTAB manual} 
    $site_3_0.05.06 add command \
        -command {exec $editor ../util/sixtab/manual.txt &} \
        -label {SIXTAB manual} 
    menubutton $site_3_0.01 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -height 1 -highlightbackground #dcdcdc \
        -highlightcolor black -menu "$site_3_0.01.02" -padx 4 -pady 3 \
        -text File 
    menu $site_3_0.01.02 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $site_3_0.01.02 add command \
        \
        -command {set ifile [open listpro w+]
set inp_files [glob -nocomplain *.inp]

foreach name $inp_files {
  puts  $ifile [ format "%-15s " $name ]
  }
  
close $ifile  
exec $editor listpro &} \
        -label {List projects} 
    $site_3_0.01.02 add command \
        -command {exec rm  $file\.* $file-*\.*} -label {Remove project} 
    $site_3_0.01.02 add command \
        -command exit -label Exit 
    menubutton $site_3_0.men40 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -height 1 -highlightbackground #dcdcdc \
        -highlightcolor black -menu "$site_3_0.men40.01" -padx 4 -pady 3 \
        -text Options -width 6 
    menu $site_3_0.men40.01 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $site_3_0.men40.01 add command \
        -command {exec $editor skel.inp &} -label {Default input} 
    $site_3_0.men40.01 add command \
        -command {exec $editor EMPEND.INP &} -label {EMPEND input} 
    $site_3_0.men40.01 add command \
        -command {exec $editor ../util/plotc4/PLOTC4.INP &} \
        -label {PLOTC4 input} 
    $site_3_0.men40.01 add command \
        -command {exec $editor ../util/fixup/FIXUP.INP &} \
        -label {FIXUP input} 
    $site_3_0.men40.01 add command \
        -command {exec $editor ../util/c4sort/C4SORT.INP &} \
        -label {C4SORT input} 
    $site_3_0.men40.01 add command \
        -command {exec $editor ../source/Makefile &} -label {Edit Makefile} 
    $site_3_0.men40.01 add cascade \
        -menu "$site_3_0.men40.01.men41" -command {} -label {Select editor} 
    set site_5_0 $site_3_0.men40.01
    menu $site_5_0.men41 \
        -activebackground #dcdcdc -activeforeground black -background #dcdcdc \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground black -tearoff 0 
    $site_5_0.men41 add radiobutton \
        -value gvim -command {set editor {gvim}} -label gvim 
    $site_5_0.men41 add radiobutton \
        -value emacs -command {set editor {emacs}} -label emacs 
    $site_5_0.men41 add radiobutton \
        -value nedit -command {set editor {nedit}} -label nedit 
    $site_5_0.men41 add radiobutton \
        -value kedit -command {set editor {kedit}} -label kedit 
    $site_5_0.men41 add radiobutton \
        -value jove -command {set editor {jove}} -label jove 
    $site_5_0.men41 add radiobutton \
        -value GXedit -command {set editor {gxedit}} -label GXedit 
    pack $site_3_0.05 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side right 
    pack $site_3_0.01 \
        -in $site_3_0 -anchor nw -expand 0 -fill none -side left 
    place $site_3_0.men40 \
        -in $site_3_0 -x 36 -y 1 -width 59 -height 24 -anchor nw \
        -bordermode inside 
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.input \
        -in $top -x 10 -y 35 -width 192 -height 898 -anchor nw \
        -bordermode ignore 
    place $top.input.project \
        -in $top.input -x 80 -y 30 -width 83 -height 27 -anchor nw \
        -bordermode ignore 
    place $top.input.but31 \
        -in $top.input -x 20 -y 85 -width 73 -height 26 -anchor nw \
        -bordermode inside 
    place $top.input.but18 \
        -in $top.input -x 93 -y 85 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but17 \
        -in $top.input -x 20 -y 145 -width 146 -height 41 -anchor nw \
        -bordermode ignore 
    place $top.input.but36 \
        -in $top.input -x 20 -y 210 -width 146 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but19 \
        -in $top.input -x 20 -y 235 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but20 \
        -in $top.input -x 93 -y 235 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.viewlong \
        -in $top.input -x 20 -y 440 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but27 \
        -in $top.input -x 20 -y 465 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but22 \
        -in $top.input -x 93 -y 465 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but23 \
        -in $top.input -x 20 -y 300 -width 146 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but28 \
        -in $top.input -x 93 -y 490 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but25 \
        -in $top.input -x 20 -y 555 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but21 \
        -in $top.input -x 70 -y 675 -width 53 -height 53 -anchor nw \
        -bordermode ignore 
    place $top.input.titel \
        -in $top.input -x 55 -y 5 -anchor nw -bordermode ignore 
    place $top.input.source \
        -in $top.input -x 20 -y 730 -width 146 -height 33 -anchor nw \
        -bordermode ignore 
    place $top.input.but26 \
        -in $top.input -x 25 -y 765 -width 138 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.sourcemenu \
        -in $top.input -x 25 -y 790 -width 137 -height 24 -anchor nw \
        -bordermode ignore 
    place $top.input.but30 \
        -in $top.input -x 70 -y 825 -width 53 -height 53 -anchor nw \
        -bordermode ignore 
    place $top.input.input \
        -in $top.input -x 70 -y 60 -anchor nw -bordermode ignore 
    place $top.input.lab34 \
        -in $top.input -x 60 -y 115 -anchor nw -bordermode ignore 
    place $top.input.lab35 \
        -in $top.input -x 85 -y 185 -anchor nw -bordermode ignore 
    place $top.input.lab37 \
        -in $top.input -x 55 -y 410 -anchor nw -bordermode ignore 
    place $top.input.but29 \
        -in $top.input -x 20 -y 325 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.browse \
        -in $top.input -x 20 -y 30 -width 62 -height 27 -anchor nw \
        -bordermode ignore 
    place $top.input.men19 \
        -in $top.input -x 20 -y 580 -width 103 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but33 \
        -in $top.input -x 125 -y 580 -width 41 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but32 \
        -in $top.input -x 20 -y 490 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.lab33 \
        -in $top.input -x 60 -y 270 -anchor nw -bordermode ignore 
    place $top.input.but34 \
        -in $top.input -x 93 -y 350 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but35 \
        -in $top.input -x 93 -y 325 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but24 \
        -in $top.input -x 20 -y 350 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but37 \
        -in $top.input -x 93 -y 375 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but38 \
        -in $top.input -x 20 -y 375 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.lab17 \
        -in $top.input -x 70 -y 525 -anchor nw -bordermode ignore 
    place $top.input.lab18 \
        -in $top.input -x 55 -y 410 -anchor nw -bordermode ignore 
    place $top.input.but39 \
        -in $top.input -x 20 -y 605 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.but40 \
        -in $top.input -x 93 -y 605 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.button53 \
        -in $top.input -x 93 -y 440 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.button54 \
        -in $top.input -x 93 -y 555 -width 73 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.input.zvvgui \
        -in $top.input -x 20 -y 630 -width 146 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.cpd32 \
        -in $top -x 5 -y 5 -width 197 -height 26 -anchor nw \
        -bordermode inside 

    vTcl:FireEvent $base <<Ready>>
}

#############################################################################
## Binding tag:  _TopLevel

bind "_TopLevel" <<Create>> {
    if {![info exists _topcount]} {set _topcount 0}; incr _topcount
}
bind "_TopLevel" <<DeleteWindow>> {
    if {[set ::%W::_modal]} {
                vTcl:Toplevel:WidgetProc %W endmodal
            } else {
                destroy %W; if {$_topcount == 0} {exit}
            }
}
bind "_TopLevel" <Destroy> {
    if {[winfo toplevel %W] == "%W"} {incr _topcount -1}
}

Window show .
Window show .srun

main $argc $argv
