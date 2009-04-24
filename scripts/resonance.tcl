#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {

    # Provoke name search
    catch {package require bogus-package-name}
    set packageNames [package names]

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
    
    # Needs Itcl
    package require Itcl

    # Needs Itk
    package require Itk

    # Needs Iwidgets
    package require Iwidgets

    switch $tcl_platform(platform) {
	windows {
            option add *Pushbutton.padY         0
	}
	default {
	    option add *Scrolledhtml.sbWidth    10
	    option add *Scrolledtext.sbWidth    10
	    option add *Scrolledlistbox.sbWidth 10
	    option add *Scrolledframe.sbWidth   10
	    option add *Hierarchy.sbWidth       10
            option add *Pushbutton.padY         2
        }
    }
    
}

#############################################################################
# Visual Tcl v1.60 Project
#


#############################################################################
# vTcl Code to Load Stock Fonts


if {![info exist vTcl(sourcing)]} {
set vTcl(fonts,counter) 0
#############################################################################
## Procedure:  vTcl:font:add_font

proc ::vTcl:font:add_font {font_descr font_type {newkey {}}} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[info exists ::vTcl(fonts,$font_descr,object)]} {
        ## cool, it already exists
        return $::vTcl(fonts,$font_descr,object)
    }

     incr ::vTcl(fonts,counter)
     set newfont [eval font create $font_descr]
     lappend ::vTcl(fonts,objects) $newfont

     ## each font has its unique key so that when a project is
     ## reloaded, the key is used to find the font description
     if {$newkey == ""} {
          set newkey vTcl:font$::vTcl(fonts,counter)

          ## let's find an unused font key
          while {[vTcl:font:get_font $newkey] != ""} {
             incr ::vTcl(fonts,counter)
             set newkey vTcl:font$::vTcl(fonts,counter)
          }
     }

     set ::vTcl(fonts,$newfont,type)       $font_type
     set ::vTcl(fonts,$newfont,key)        $newkey
     set ::vTcl(fonts,$newfont,font_descr) $font_descr
     set ::vTcl(fonts,$font_descr,object)  $newfont
     set ::vTcl(fonts,$newkey,object)      $newfont

     lappend ::vTcl(fonts,$font_type) $newfont

     ## in case caller needs it
     return $newfont
}

#############################################################################
## Procedure:  vTcl:font:getFontFromDescr

proc ::vTcl:font:getFontFromDescr {font_descr} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[info exists ::vTcl(fonts,$font_descr,object)]} {
        return $::vTcl(fonts,$font_descr,object)
    } else {
        return ""
    }
}

vTcl:font:add_font \
    "-family helvetica -size 12 -weight bold" \
    stock \
    vTcl:font5
}
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

    set command [lindex $args 0]
    set args [lrange $args 1 end]
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
    set base .top71
    namespace eval ::widgets::$base {
        set set,origin 1
        set set,size 1
        set runvisible 1
    }
    namespace eval ::widgets::$base.cpd84 {
        array set save {-borderwidth 1 -height 1 -relief 1 -width 1}
    }
    set site_3_0 $base.cpd84
    namespace eval ::widgets::$site_3_0.01 {
        array set save {-anchor 1 -menu 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_3_0.01.02 {
        array set save {-activeborderwidth 1 -borderwidth 1 -font 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-accelerator 1 -command 1 -label 1}
        }
    }
    namespace eval ::widgets::$site_3_0.05 {
        array set save {-anchor 1 -menu 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_3_0.05.06 {
        array set save {-activeborderwidth 1 -borderwidth 1 -font 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    namespace eval ::widgets::$site_3_0.men72 {
        array set save {-menu 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.men72.m {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1}
        }
    }
    set site_5_0 $site_3_0.men72.m
    namespace eval ::widgets::$site_5_0.men72 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_5_0 $site_3_0.men72.m
    namespace eval ::widgets::$site_5_0.men71 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_5_0 $site_3_0.men72.m
    namespace eval ::widgets::$site_5_0.men73 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_5_0 $site_3_0.men72.m
    namespace eval ::widgets::$site_5_0.men74 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    namespace eval ::widgets::$site_3_0.men67 {
        array set save {-menu 1 -padx 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.men67.m {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-accelerator 1 -command 1 -label 1}
        }
    }
    namespace eval ::widgets::$base.tab85 {
        array set save {-height 1 -tabpos 1}
        namespace eval subOptions {
            array set save {-label 1}
        }
    }
    set site_8_0 [lindex [$base.tab85 childsite] 0]
    namespace eval ::widgets::$site_8_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_0
    namespace eval ::widgets::$site_8_0.lab89 {
        array set save {-_tooltip 1 -anchor 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.ent92 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -state 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab90 {
        array set save {-_tooltip 1 -anchor 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.ent93 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -state 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.but76 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.lab75 {
        array set save {-text 1}
    }
    namespace eval ::widgets::$site_8_0.lab76 {
        array set save {-text 1}
    }
    namespace eval ::widgets::$site_8_0.lab78 {
        array set save {-text 1}
    }
    namespace eval ::widgets::$site_8_0.lab72 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.ent73 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.ent79 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.ent80 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab91 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.ent94 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.ent98 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.ent81 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab82 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.ent83 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.cpd71 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.cpd72 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab71 {
        array set save {-foreground 1 -highlightcolor 1 -labelpos 1 -labeltext 1 -text 1}
    }
    set site_10_0 [$site_8_0.lab71 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.cpd75 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.cpd76 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.lab71 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent72 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd71 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.cpd72 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd73 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.cpd74 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab77 {
        array set save {-foreground 1 -highlightcolor 1 -labelpos 1 -labeltext 1 -text 1}
    }
    set site_10_0 [$site_8_0.lab77 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.lab89 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent90 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd78 {
        array set save {-activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.cpd79 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.lab86 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent88 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.lab73 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent74 {
        array set save {-_tooltip 1 -background 1 -foreground 1 -highlightcolor 1 -insertbackground 1 -selectbackground 1 -selectforeground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.che71 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.che72 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.che74 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -anchor 1 -foreground 1 -highlightcolor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_8_0.but75 {
        array set save {-_tooltip 1 -command 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.but74 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but65 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but66 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but67 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but68 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but69 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but70 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but71 {
        array set save {-command 1 -text 1}
    }
    set site_8_1 [lindex [$base.tab85 childsite] 1]
    namespace eval ::widgets::$site_8_1 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_1
    namespace eval ::widgets::$site_8_0.lab65 {
        array set save {-labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab65 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.lab71 {
        array set save {-anchor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent72 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.lab73 {
        array set save {-anchor 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent74 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.but73 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.lab65 {
        array set save {-anchor 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent66 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.tex65 {
        array set save {-background 1 -insertbackground 1}
    }
    namespace eval ::widgets::$site_10_0.che66 {
        array set save {-_tooltip 1 -anchor 1 -command 1 -justify 1 -overrelief 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.che67 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.che68 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.but65 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.lab66 {
        array set save {-anchor 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.rad65 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.rad66 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.rad67 {
        array set save {-_tooltip 1 -text 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.rad68 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.rad69 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.rad70 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -value 1 -variable 1}
    }
    namespace eval ::widgets::$site_8_0.lab66 {
        array set save {-labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab66 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.cpd67 {
        array set save {-_tooltip 1 -anchor 1 -command 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd68 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd69 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd70 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_10_0.cpd65 {
        array set save {-labelpos 1 -labeltext 1}
    }
    set site_12_0 [$site_10_0.cpd65 childsite]
    namespace eval ::widgets::$site_12_0 {
        array set save {-background 1 -height 1 -highlightcolor 1 -width 1}
    }
    set site_12_0 $site_12_0
    namespace eval ::widgets::$site_12_0.cpd65 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.cpd66 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.lab65 {
        array set save {-labelpos 1 -labeltext 1}
    }
    set site_12_0 [$site_10_0.lab65 childsite]
    namespace eval ::widgets::$site_12_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_12_0 $site_12_0
    namespace eval ::widgets::$site_12_0.cpd66 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.cpd67 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.cpd68 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.cpd69 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_12_0.cpd70 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_12_0.cpd71 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_10_0.cpd71 {
        array set save {-_tooltip 1 -command 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.cpd72 {
        array set save {-_tooltip 1 -command 1 -font 1 -text 1}
    }
    set site_8_2 [lindex [$base.tab85 childsite] 2]
    namespace eval ::widgets::$site_8_2 {
        array set save {-background 1 -highlightcolor 1}
    }
    $base.tab85 select 0
    namespace eval ::widgets_bindings {
        set tagslist {_TopLevel _vTclBalloon}
    }
    namespace eval ::vTcl::modules::main {
        set procs {
            init
            main
            InitVars
            LoadVars
            Cleanup
            PlotXS
            RunKALMAN
            RunCodes
            GetSymbol
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
  global argv0
  global m_nZA
  global m_nMAT
  global m_szZAname
  global m_szFile m_szEditor m_szPsviewer
  global m_szBaseDir m_szWorkingDir m_szCodeDir m_szAtlasDir m_szENDFDir

  set m_szWorkingDir [pwd]
  if {[file pathtype $argv0] == "relative"} {
    set m_szBaseDir [file dirname [file join [pwd] $argv0]]
  } else {
    set m_szBaseDir [file dirname $argv0]
  }
  set m_szBaseDir $m_szBaseDir/..
  cd $m_szBaseDir
  set m_szBaseDir [pwd]
  cd $m_szWorkingDir

  set m_szCodeDir $m_szBaseDir/util/resonance
  set m_szAtlasDir $m_szBaseDir/Atlas
  set m_szENDFDir $m_szBaseDir/ENDF

  set m_nZA [expr int([lindex $argv 0])]
  set m_nMAT [lindex $argv 1]
  set flag [lindex $argv 2]

  if {$flag == "true"} {
    Entry1 configure -state  disabled
    Entry2 configure -state  disabled
  }

  set m_szFile ""
  set m_szEditor "gvim"
  set m_szPsviewer "kghostview"
  if {[file exists $m_szBaseDir/.Xrunrc] == 1} {
    set rcfl [open $m_szBaseDir/.Xrunrc r]
    gets $rcfl m_szFile
    gets $rcfl m_szEditor
    gets $rcfl dummy
    gets $rcfl m_szPsviewer
  }
  if {$m_szFile == "" || $flag != "true"} {
    set m_szFile [format "%06d" $m_nZA]
  }
  set m_szZAname [format "za%06d" $m_nZA]

  InitVars
  LoadVars
}
#############################################################################
## Procedure:  InitVars

proc ::InitVars {} {
  global m_fGPower
  global m_fErmax
  global m_fDe
  global m_bGotPTANAL m_bGotWRIURR m_bGotRECENT
  global m_bGotSensitivity
  global m_nKALMT

  set m_bGotPTANAL 0
  set m_bGotWRIURR 0
  set m_bGotRECENT 0
  set m_bGotSensitivity 0

  set m_fGPower 2.5
  set m_fErmax 500000
  set m_fDe 30

  set m_nKALMT  9999
}
#############################################################################
## Procedure:  LoadVars

proc ::LoadVars {} {
  global m_nZA m_nZ m_nA
  global m_szZAname
  global m_fAwt m_fAbun m_fBn m_fSpin
  global m_fD0 m_fD1 m_fD2
  global m_fSf0 m_fSf1 m_fSf2
  global m_fGg0 m_fGg1 m_fGg2
  global m_fErmax m_fEumax
# m_fR, m_fDR : scattering radius and its error
  global m_fR m_fDR
# m_fSS, m_fDSS : scattering cross section and its error
  global m_fSS m_fDSS
# m_fCS, m_fDCS : capture cross section and its error
  global m_fCS m_fDCS
# m_fLdp: level density parameter
  global m_fLdp m_fCutoff
  global m_nNoResToBeAdjusted
  global m_nNoResToBeEvaluated
  global m_nNoExtraRes

  global m_bGotPTANAL m_bGotWRIURR m_bGotRECENT
  global m_bGotSensitivity

  global m_szBaseDir m_szCodeDir m_szFile m_szWorkingDir

  set m_nNoResToBeAdjusted 10
  set m_nNoResToBeEvaluated 3
  set m_nNoExtraRes 5

  set m_nZA [expr int($m_nZA)]
  set m_nZ [expr $m_nZA/1000]
  set m_nA [expr $m_nZA%1000]

  set m_bGotPTANAL 0
  set m_bGotWRIURR 0
  set m_bGotRECENT 0
  set m_bGotSensitivity 0

  cd $m_szWorkingDir

  set atlasfn [format "%s.atlas" $m_szZAname]
  if {[file exists $m_szFile.atlas]} {
    exec mv -f $m_szFile.atlas $atlasfn
  }

  set output [exec $m_szCodeDir/readrp $m_nZA $m_szBaseDir]
  regsub -all "\n" $output " " output
  regsub -all " +" $output " " output
  set output [string trim $output]
  set output [split $output]

  set m_fAbun [lindex $output 0]
  if {$m_fAbun==0} {puts "Warning: abundance is zero"}
  set m_fAwt [lindex $output 1]
  if {$m_fAwt==0} {puts "Warning: atomic mass is zero"}
  set m_fBn [lindex $output 2]
  if {$m_fBn==0} {puts "Warning: binding energy is zero"}
  set m_fSpin [format "%.1f" [lindex $output 3]]
  if {$m_fSpin==-1} {puts "Warning: spin is not given"}
  set m_fD0 [format "%.2f" [lindex $output 4]]
  set m_fD1 [format "%.2f" [lindex $output 5]]
  set m_fD2 [format "%.2f" [lindex $output 6]]
  set m_fSf0 [format "%.2e" [lindex $output 7]]
  set m_fSf1 [format "%.2e" [lindex $output 8]]
  set m_fSf2 [format "%.2e" [lindex $output 9]]
  set m_fGg0 [format "%.2f" [lindex $output 10]]
  set m_fGg1 [format "%.2f" [lindex $output 11]]
  set m_fGg2 [format "%.2f" [lindex $output 12]]
  set m_fR [format "%.2f" [lindex $output 13]]
  set m_fDR [format "%.2f" [lindex $output 14]]
  set m_fSS [format "%f" [lindex $output 15]]
  set m_fDSS [format "%f" [lindex $output 16]]
  set m_fCS [format "%f" [lindex $output 17]]
  set m_fDCS [format "%f" [lindex $output 18]]
  set m_fErmax [lindex $output 19]
  set m_fEumax [lindex $output 20]
  if {$m_fEumax==0} {set m_fEumax 20e6}

  set m_fLdp [format "%.2f" [expr $m_nA/8.0]]

  if {$m_nZ/2*2 == $m_nZ && ($m_nA-$m_nZ)/2*2==$m_nA-$m_nZ} {
#   even-even nuclide
    set pair [expr 24/sqrt($m_nA)]
  } elseif {$m_nZ/2*2 != $m_nZ && ($m_nA-$m_nZ)/2*2 != $m_nA-$m_nZ} {
#   odd-odd nuclide
    set pair 0
  } else {
#   odd nuclide
    set pair [expr 12/sqrt($m_nA)]
  }
  if {$m_fLdp > 0 && $m_fBn-$pair > 0} {
    set m_fCutoff [format "%.2f" [expr sqrt(0.0888*pow($m_nA,2./3)*sqrt($m_fLdp*($m_fBn-$pair)))]]
  }

  if {[file exists $atlasfn]} {
    exec mv -f $atlasfn $m_szFile.atlas
  } else {
    puts [format "Error: Local resonance parameter table '%s' not found" $atlasfn]
  }

  if {[file exists $m_szFile.exf]} { exec cp $m_szFile.exf EXFOR.dat }
  exec xterm -T readx4 -e sh -c "$m_szCodeDir/readx4 $m_nZA $m_szBaseDir"
  if {[file exists "EXFOR.dat"]} { exec mv EXFOR.dat $m_szFile.exf }
  if {[file exists $m_szFile.exf]} {
    if {[file exists $m_szFile.c4] != 1} {
#     RUN X4TOC4 TO TRANSLATE EXFOR FILE INTO COMPUTATIONAL FORMAT
      exec xterm -T c4 -e sh -c "$m_szBaseDir/scripts/c4 $m_szFile"
#     RUN c4sort TO SORT C4 DATA
      exec xterm -T sortc4 -e sh -c "$m_szBaseDir/scripts/sortc4 $m_szFile"
    }
  }
  exec rm -f EXFOR.dat EXFOR.datp
}
#############################################################################
## Procedure:  Cleanup

proc ::Cleanup {} {
  global m_nZA
  global m_szZAname
#  exec rm -f ptanal.inp ptanal.lis ptanal.std
  exec rm -f wriurr.inp wriurr.lis wriurr.std
  exec rm -f RECENT.INP RECENT.LST recent.std
  exec rm -f SIGMA1.INP SIGMA1.LST sigma1.std
# I don't use something like "rm -f pt*" for safety
  exec rm -f ptdist0.dat ptdist0.fit ptdist1.dat ptdist1.fit ptdist.dat ptdist.fit ptdist.gp ptdist.ps
  exec rm -f l0.txt l1.txt l2.txt scanr.gp scanr.ps scanr.std
  exec rm -f endfr.txt endfa.txt endfu.txt endfu.pw endfu.bpw
  exec rm -f [format "%s.atlas" $m_szZAname]
  puts "Cleanup completed"
}
#############################################################################
## Procedure:  PlotXS

proc ::PlotXS {nMT bENDF bJENDL bJEFF} {
  global m_szFile
  global m_szBaseDir m_szWorkingDir
  global m_nZA m_nZ m_nA
  global m_bGotRECENT
 
  cd $m_szWorkingDir 
  if {[file exists $m_szFile-res.pendf] && [file exists $m_szFile.c4]} {
    cd $m_szBaseDir/util/c4zvd
    exec ln -fs $m_szWorkingDir/$m_szFile-res.pendf Present
    exec ln -fs $m_szWorkingDir/$m_szFile.c4 c4
    exec rm -f endf.zvd temp.dat c4.zvd $m_szFile-$nMT.zvd
    exec xterm -T endzvdl -e sh -c "endzvdl.exe $m_nZA $nMT Present Present.zvd"
    exec xterm -T c4dat4l -e sh -c "c4dat4l.exe c4 temp.dat MF=3 MT=$nMT"
    exec xterm -T datzvdl -e sh -c "datzvdl.exe temp.dat c4.zvd"
    exec cat c4.zvd > $m_szFile-$nMT.zvd
    exec cat Present.zvd >> $m_szFile-$nMT.zvd
    if {$bENDF} {
      set fn [format "%s/n-%03d_%s_%03d.endf" $m_szBaseDir/ENDF/ENDF-VII.0 $m_nZ [GetSymbol $m_nZ] $m_nA]
      if {[file exists $fn]} {
        exec ln -fs $fn ENDF-VII.0
        exec xterm -T endzvdl -e sh -c "endzvdl.exe $m_nZA $nMT ENDF-VII.0 ENDF.zvd"
        exec cat ENDF.zvd >> $m_szFile-$nMT.zvd
      } else {
        puts "Error: ENDF file '$fn' not found"
      }
    }
    if {$bJENDL} {
      set fn [format "%s/n-%03d_%s_%03d.endf" $m_szBaseDir/ENDF/JENDL-3.3 $m_nZ [GetSymbol $m_nZ] $m_nA]
      if {[file exists $fn]} {
        exec ln -fs $fn JENDL-3.3
        exec xterm -T endzvdl -e sh -c "endzvdl.exe $m_nZA $nMT JENDL-3.3 JENDL.zvd"
        exec cat JENDL.zvd >> $m_szFile-$nMT.zvd
      } else {
        puts "Error: JENDL file '$fn' not found"
      }
    }
    if {$bJEFF} {
      set fn [format "%s/n-%03d_%s_%03d.endf" $m_szBaseDir/ENDF/JEFF-3.1 $m_nZ [GetSymbol $m_nZ] $m_nA]
      if {[file exists $fn]} {
        exec ln -fs $fn JEFF-3.1
        exec xterm -T endzvdl -e sh -c "endzvdl.exe $m_nZA $nMT JEFF-3.1 JEFF.zvd"
        exec cat JEFF.zvd >> $m_szFile-$nMT.zvd
      } else {
        puts "Error: JEFF file '$fn' not found"
      }
    }
    exec rm -f Present c4 Present.zvd temp.dat c4.zvd ENDF-VII.0 JENDL-3.3 JEFF-3.1 ENDF.zvd JENDL.zvd JEFF.zvd
    exec mv -f $m_szFile-$nMT.zvd $m_szWorkingDir
    cd $m_szWorkingDir
    exec $m_szBaseDir/scripts/showzvd $m_szFile-$nMT.zvd 2> /dev/null &
  } else {
    if {![file exists $m_szFile-res.pendf]} {
      if {$m_bGotRECENT == 0} {
        tk_dialog .msgbox "Error" "Run PTANAL,WRIURR and RECENT first" info 0 OK
        return
      }
      tk_dialog .msgbox "Error" "File '$m_szFile-res.pendf' not found" info 0 OK
    }
    if {![file exists $m_szFile.c4]} {
      tk_dialog .msgbox "Error" "File '$m_szFile.c4' not found\nProbably X4 not installed" info 0 OK
    }
  }
}
#############################################################################
## Procedure:  RunKALMAN

proc ::RunKALMAN {} {
  global m_szBaseDir m_szFile m_szWorkingDir m_szCodeDir
  global m_bAllcodes1 m_bSensitivity m_bKALMAN
  global m_nZ m_nA m_nZA m_nMAT
  global m_szZAname
  global m_fAwt m_fAbun m_fBn m_fSpin
  global m_fD0 m_fD1 m_fD2
  global m_fSf0 m_fSf1 m_fSf2
  global m_fGg0 m_fGg1 m_fGg2
  global m_fErmax m_fEumax
  global m_fR m_fDR m_fCutoff m_fDe m_fLdp m_fGPower
  global m_fSS m_fDSS m_fCS m_fDCS
  global m_fGn0_cut m_fGn1_cut
  global m_nNoResToBeAdjusted m_nNoResToBeEvaluated m_nNoExtraRes
  global m_bGotSensitivity
  global m_nKALMT

# m_nNoResToBeAdjusted: number of resonances to be adjusted for sensitivy calculation
# m_nNoResToBeEvaluated: number of resonance energies to be added to the energy grid of THERMX(revised RECENT)

  cd $m_szWorkingDir
  if {$m_bAllcodes1 ==0 && $m_bSensitivity == 0 &&
      $m_bKALMAN == 0} {
    tk_dialog .msgbox "Error" "No job specified" info 0 OK
  }

#   verify the integrity of input befor run
  if {$m_bAllcodes1 != 0 || $m_bSensitivity != 0} {
    if {$m_nNoResToBeAdjusted <= 0} {
      tk_dialog .msgbox "Error" "No. of resonances to be adjusted should be larger than 0" info 0 OK
      return
    } elseif {$m_nNoResToBeEvaluated <= 0} {
      tk_dialog .msgbox "Error" "No. of resonances to be evalulated should be larger than 0" info 0 OK
      return
    } elseif {$m_nNoExtraRes < 0} {
      tk_dialog .msgbox "Error" "No. of extra resonances should be equal or larger than 0" info 0 OK
      return
    }
  }
  if {$m_bAllcodes1 != 0 || $m_bKALMAN != 0} {
    if {$m_bAllcodes1 == 0 && $m_bSensitivity == 0 && $m_bGotSensitivity == 0} {
      tk_dialog .msgbox "Error" "Run sensitivity calculation first" info 0 OK
      return
    }
  }

  if {$m_bAllcodes1 != 0 || $m_bSensitivity != 0} {

    set files [glob -nocomplain "$m_szZAname*.kal"]
    foreach fn $files {
      catch {file delete $fn}
    }

    set atlasfn [format "%s.atlas" $m_szZAname]
    exec cp -f $m_szFile.atlas $atlasfn

    set fn [format "%s-inp.sen" $m_szZAname]
    set inpfile [open $fn w]
    set fn [format "%s-mat.sen" $m_szZAname]
    set senfile [open $fn w]
    
    # range(-3...) takes R' into account
    for {set i -3} {$i< [expr 2*6*$m_nNoResToBeAdjusted]} {incr i} {
      if {![file exists $m_szFile.atlas]} {
        tk_dialog .msgbox "Error" "Local resonance parameter table '$m_szFile.atlas' not found" info 0 OK
        return
      }
#     make a input file for PTANAL
      exec rm -f ptanal.std
      set file [open "ptanal.inp" w]
      puts $file "&data"
      puts $file " zam=$m_nZA mat=$m_nMAT awt=$m_fAwt spin=$m_fSpin"
      if {$m_fAbun !=0} {puts $file " abun=$m_fAbun"}
      puts $file " sf0=$m_fSf0\n sf1=$m_fSf1,$m_fSf1,$m_fSf1\n D0=$m_fD0"
      puts $file " ggavg=$m_fGg0, $m_fGg1"
      if {$m_fErmax != ""} {puts $file " ecut=$m_fErmax"}
      if {$m_fGn0_cut != ""} {puts $file " gncut=$m_fGn0_cut,$m_fGn1_cut"}
      puts $file [format " ap=%f dap=%f" [expr $m_fR/10] [expr $m_fDR/10]]
      if {$i == -3} {
        puts $file [format " adjust='K0001/%04d %2d%4d'" $m_nNoResToBeAdjusted $m_nNoResToBeEvaluated $m_nNoExtraRes]
      } elseif {$i == -2} {
        puts $file [format " adjust='R0001/%04d-%2d%4d'" $m_nNoResToBeAdjusted $m_nNoResToBeEvaluated $m_nNoExtraRes]
      } elseif {$i == -1} {
        puts $file [format " adjust='R0001/%04d+%2d%4d'" $m_nNoResToBeAdjusted $m_nNoResToBeEvaluated $m_nNoExtraRes]
      } elseif {[expr $i%6] == 0} {
        puts $file [format " adjust='E%04d/%04d-  %4d'" [expr $i/6+1] $m_nNoResToBeAdjusted $m_nNoExtraRes]
      } elseif {[expr $i%6] == 1} {
        puts $file [format " adjust='E%04d/%04d+  %4d'" [expr $i/6+1] $m_nNoResToBeAdjusted $m_nNoExtraRes]
      } elseif {[expr $i%6] == 2} {
        puts $file [format " adjust='N%04d/%04d-  %4d'" [expr $i/6+1] $m_nNoResToBeAdjusted $m_nNoExtraRes]
      } elseif {[expr $i%6] == 3} {
        puts $file [format " adjust='N%04d/%04d+  %4d'" [expr $i/6+1] $m_nNoResToBeAdjusted $m_nNoExtraRes]
      } elseif {[expr $i%6] == 4} {
        puts $file [format " adjust='G%04d/%04d-  %4d'" [expr $i/6+1] $m_nNoResToBeAdjusted $m_nNoExtraRes]
      } else {
        puts $file [format " adjust='G%04d/%04d+  %4d'" [expr $i/6+1] $m_nNoResToBeAdjusted $m_nNoExtraRes]
      }
      puts $file "&end"
      close $file
#     run PTANAL
      exec $m_szCodeDir/ptanal > ptanal.std
      if {![file exists endfa.txt]} {
        tk_dialog .msgbox "Error" "Failed to run PTANAL.\nCheck the standard output of PTANAL" info 0 OK
        return
      }
      set count 0
      set file [open "ptanal.unc" r]
      gets $file line
      scan $line "%s%d%f" pname count uncert
      close $file
      if {$pname == "DOSTOP"} { break }
      if {$i == -3} { set ulimit $uncert }
#      exec cp ptanal.inp [format "INP%03d-%d" $count [expr $i%6+1]]
#      exec cp ptanal.std [format "STD%03d-%d" $count [expr $i%6+1]]
#      exec cp ptanal.lis [format "LIS%03d-%d" $count [expr $i%6+1]]
#      exec cp endfa.txt [format "ENDF%03d-%d" $count [expr $i%6+1]]
#      exec cp ptanal.unc [format "UNC%03d-%d" $count [expr $i%6+1]]
#     make a input file for THERMX
      set file [open "THERMX.INP" w]
      puts $file [format "%11d%11.4E%11d%11d%11d%11d" 0 1e-10 1 1 1 1]
      puts $file "endfa.txt\nendfu.pw"
      puts $file [format "%11d%11d%11.4e%11.4e\n" 1 9999 0 $ulimit]
      puts $file [format "%11.4E%11.4E\n" 0 0.01]
      close $file
#     run THERMX
      exec $m_szCodeDir/thermx > thermx.std

#      exec cp endfu.pw [format "PWD%03d-%d" $count [expr $i%6+1]]
      set file [open "endfu.pw" r]
      set n 0
      while {[gets $file line] >= 0} {
        if {[string index $line 0] == "#"} {continue}
        set en($n) 0
        set total($n) 0
        set elastic($n) 0
        set capture($n) 0
        set fission($n) 0
        scan $line "%f%f%f%f" en($n) total($n) elastic($n) capture($n)
#       puts [format "%f %f %f %f" $en($n) $total($n) $elastic($n) $capture($n)]
        set n [expr $n+1]
      }
      close $file
      if {$i == -3} {
        set fn [format "%s.xsc" $m_szZAname]
        set xsfile [open $fn w]
        puts $xsfile [format "#  5          %3d-%-2s-%3d" $m_nZ [GetSymbol $m_nZ] $m_nA]
        puts $xsfile "#  Einc       Total       Elastic     Reaction    Fission   (z,gamma)"
        set ndata $n
#       <Units>
#       Energy : MeV
#       Cross section : mb
        for {set n 0} {$n<$ndata} {incr n} {
          set en0($n) $en($n)
          set total0($n) $total($n)
          set elastic0($n) $elastic($n)
          set capture0($n) $capture($n)
          set fission0($n) $fission($n)
          puts $xsfile [format "%10.4E%12.5E%12.5E%12.5E%12.5E%12.5E"  [expr $en($n)*1E-6]  [expr $total0($n)*1E3]  [expr $elastic0($n)*1E3]  0.0  0.0  [expr $capture0($n)*1E3]]
        }
        close $xsfile
      } elseif {$i == -2 || [expr $i%2] == 0} {
        set ndata $n
        for {set n 0} {$n<$ndata} {incr n} {
          set en1($n) $en($n)
          set total1($n) $total($n)
          set elastic1($n) $elastic($n)
          set capture1($n) $capture($n)
          set fission1($n) $fission($n)
        }
      } else {
        puts $senfile [format "#  5          %3d-%-2s-%3d" $m_nZ [GetSymbol $m_nZ] $m_nA]
        if {$pname == "SCATRD"} {
          puts $inpfile [format "%6s    %8.5f    00  00   0           ! scattering radius" "SCATRD" $uncert]
          puts $senfile [format "# Parameter: %6s  %3d%3d%3d%3d  variation: +-%.3f     Sensitivity matrix"  $pname $count 1 1 0 $uncert]
        } elseif {$pname == "ENERGY"} {
          puts $inpfile [format "%2s%04d    %8.5f    00  00   0           ! resonance energy" "EN" $count $uncert]
          puts $senfile [format "# Parameter: %2s%04d  %3d%3d%3d%3d  variation: +-%.3f     Sensitivity matrix"  [string range $pname 0 1] $count $count 1 1 0 $uncert]
        } elseif {$pname == "NWIDTH"} {
          puts $inpfile [format "%2s%04d    %8.5f    00  00   0           ! neutron width" "NW" $count $uncert]
          puts $senfile [format "# Parameter: %2s%04d  %3d%3d%3d%3d  variation: +-%.3f     Sensitivity matrix"  [string range $pname 0 1] $count $count 1 1 0 $uncert]
        } elseif {$pname == "GWIDTH"} {
          puts $inpfile [format "%2s%04d    %8.5f    00  00   0           ! gamma width" "GW" $count $uncert]
          puts $senfile [format "# Parameter: %2s%04d  %3d%3d%3d%3d  variation: +-%.3f     Sensitivity matrix"  [string range $pname 0 1] $count $count 1 1 0 $uncert]
        }
        puts $senfile "#  Einc       Total       Elastic     Reaction    Fission   (z,gamma)"
        set ndata $n
#       <Units>
#       Energy : MeV
        for {set n 0} {$n<$ndata} {incr n} {
          puts $senfile [format "%10.4E%12.4E%12.4E%12.4E%12.4E%12.4E" [expr $en1($n)*1E-6]  [expr ($total($n)-$total1($n))/$total0($n)]  [expr ($elastic($n)-$elastic1($n))/$elastic0($n)]  0.0 0.0  [expr ($capture($n)-$capture1($n))/$capture0($n)]]
        }
        puts $senfile ""
      }
    }
#    exec rm -f THERMX.INP endfa.txt endfu.pw
##    exec rm -f ptanal.inp $atlasfn
    close $inpfile
    close $senfile

    set fn [format "%s.c4" $m_szZAname]
    set c4file [open $fn w]
    puts $c4file [format "%5i%6i%4i%4i   %9f%9f%9f%9f%9f%9f%9f%9f   %-25s%5d%3d" 1 $m_nZA 3 2 0.0253 0 $m_fSS $m_fDSS 0 0 0 0 "S.F.Mughabghab (06)" 10000 1]
    puts $c4file [format "%5i%6i%4i%4i   %9f%9f%9f%9f%9f%9f%9f%9f   %-25s%5d%3d" 1 $m_nZA 3 102 0.0253 0 $m_fCS $m_fDCS 0 0 0 0 "S.F.Mughabghab (06)" 10000 2]
    close $inpfile

    set m_bGotSensitivity 1
  }
  if {$m_bAllcodes1 != 0 || $m_bKALMAN != 0} {
#     run KALMAN
    if {$m_nKALMT == 0} {
      set MT 1
      set EXPDAT 0
    } elseif {$m_nKALMT == 9999} {
      set MT 1
      set EXPDAT 2
    } else {
      set MT $m_nKALMT
      set EXPDAT 1
    }
    exec $m_szBaseDir/scripts/kalmanResonance $m_szZAname $MT $m_nMAT $EXPDAT
  
#    set fn [format "%s.atlas" $m_szZAname]
#    set atlas1 [open $fn r]
#    set fn [format "%s.atlas~" $m_szZAname]
#    set atlas2 [open $fn w]
#    while {[gets $atlas1 line] >= 0} {
#      if {[string index $line 0] == "#"} {continue}
#        set iza   [string range $line 0 5]
#        set sym   [string range $line 7 8]
#        set e0    [string range $line 26 35]
#        set de0   [string range $line 37 43]
#        set jflag [string range $line 45 45]
#        set ajx   [string range $line 47 49]
#        set lflag [string range $line 51 51]
#        set lw    [string range $line 53 53]
#        set nflag [string range $line 80 82]
#        set ggn   [string range $line 86 94]
#        set dggn  [string range $line 96 103]
#        set gflag [string range $line 105 107]
#        set gg    [string range $line 111 119]
#        set dgg   [string range $line 121 128]
#        set aflag [string range $line 155 157]
#        set area  [string range $line 161 169]
#        catch {puts $atlas2 [format "%6d;%2s;%3s;%11s;%10.4e;%7.2e;%1s;%3.1f;%1s;%1d;%3s;%1s;%9s;%8s;%3s;%1s;%9e;%8e;%3s;%1s;%9e;%8e%3s%1s%9s%8s%3s%1s%9e" $iza $sym "" "" $e0 $de0 $jflag $ajx $lflag $lw "" "" "" "" $nflag "" $ggn $dggn $gflag "" $gg $dgg "" "" "" "" $aflag "" $area]}
#    }
#    close $atlas1
#    close $atlas2
  }
}
#############################################################################
## Procedure:  RunCodes

proc ::RunCodes {} {
  global m_szBaseDir m_szFile m_szWorkingDir m_szCodeDir
  global m_bAllcodes2 m_bPTANAL m_bWRIURR m_bRECENT
  global m_nZ m_nA m_nZA m_nMAT
  global m_szZAname
  global m_fAwt m_fAbun m_fBn m_fSpin
  global m_fD0 m_fD1 m_fD2
  global m_fSf0 m_fSf1 m_fSf2
  global m_fGg0 m_fGg1 m_fGg2
  global m_fErmax m_fEumax
  global m_fR m_fDR m_fCutoff m_fDe m_fLdp m_fGPower
  global m_fGn0_cut m_fGn1_cut
  global m_bIcon1 m_bIcon2 m_bIcon3
  global m_bGotPTANAL m_bGotWRIURR m_bGotRECENT

  cd $m_szWorkingDir
  if {$m_bAllcodes2 ==0 && $m_bPTANAL == 0 &&
      $m_bWRIURR == 0 && $m_bRECENT == 0} {
    tk_dialog .msgbox "Error" "No job specified" info 0 OK
  }


#   verify the integrity of input befor run
  if {$m_bAllcodes2 != 0 || $m_bPTANAL != 0} {
  }
  if {$m_bAllcodes2 != 0 || $m_bWRIURR != 0} {
    if {$m_bAllcodes2 == 0 && $m_bPTANAL == 0 && $m_bGotPTANAL == 0} {
      tk_dialog .msgbox "Error" "Run PTANAL first" info 0 OK
      return
    }
    if {$m_fEumax == ""} {
      tk_dialog .msgbox "Error" "Enter Emax for unresolved region" info 0 OK
      return
    }
    if {$m_fDe == ""} {
      tk_dialog .msgbox "Error" "Enter energies (# of energies)" info 0 OK
      return
    }
    if {$m_fCutoff == ""} {
      tk_dialog .msgbox "Error" "Enter spin cut-off (spin cut-off parameter)" info 0 OK
      return
    }
    if {$m_fLdp == ""} {
      tk_dialog .msgbox "Error" "Enter a l.d.p. (level density parameter)" info 0 OK
      return
    }
  }
  if {$m_bAllcodes2 != 0 || $m_bRECENT != 0} {
    if {$m_bAllcodes2 == 0 && $m_bWRIURR == 0 && $m_bGotWRIURR == 0} {
      tk_dialog .msgbox "Error" "Run PTANAL and WRIURR first" info 0 OK
      return
    }
  }
  if {$m_bAllcodes2 != 0 || $m_bPTANAL != 0} {
    if {![file exists $m_szFile.atlas]} {
      tk_dialog .msgbox "Error" "Local resonance parameter table '$m_szFile.atlas' not found" info 0 OK
      return
    }
    set atlasfn [format "%s.atlas" $m_szZAname]
    exec cp -f $m_szFile.atlas $atlasfn
    exec rm -f ptanal.std
#   make a input file for PTANAL
    set file [open "ptanal.inp" w]
    puts $file "&data"
    puts $file " zam=$m_nZA mat=$m_nMAT awt=$m_fAwt spin=$m_fSpin"
    if {$m_fAbun !=0} {puts $file " abun=$m_fAbun"}
    puts $file " sf0=$m_fSf0\n sf1=$m_fSf1,$m_fSf1,$m_fSf1\n D0=$m_fD0"
    puts $file " ggavg=$m_fGg0, $m_fGg1"
    if {$m_fErmax != ""} {puts $file " ecut=$m_fErmax"}
    if {$m_fGn0_cut != ""} {puts $file " gncut=$m_fGn0_cut,$m_fGn1_cut"}
    puts $file [format " ap=%f" [expr $m_fR/10]]
    puts $file "&end"
    close $file
#   run PTANAL
    if {[file exists /usr/bin/tee]} {
      exec xterm -T PTANAL -e sh -c "$m_szCodeDir/ptanal | /usr/bin/tee ptanal.std"
    } else {
      exec xterm -T PTANAL -e sh -c "$m_szCodeDir/ptanal > ptanal.std"
    }
#    exec rm -f ptanal.inp $atlasfn
    exec mv -f ptanal.lis $m_szFile-log.ptanal
    if {![file exists endfa.txt]} {
      tk_dialog .msgbox "Error" "Failed to run PTANAL.\nCheck the standard output of PTANAL" info 0 OK
      return
    } else {
      exec mv -f endfa.txt endfr.txt
    }
    set m_bGotPTANAL 1
  }
  if {$m_bAllcodes2 != 0 || $m_bWRIURR != 0} {
    if {$m_fEumax < $m_fErmax} {
      tk_dialog .msgbox "Error" "Emax of unresolved region should be larger than Emax of resolved region" info 0 OK
      return
    }
    exec rm -f wriurr.std
#   make a input file for WRIURR
    set file [open "wriurr.inp" w]
    puts $file "&urr"
    puts $file " bn=$m_fBn dena=$m_fLdp disp=$m_fCutoff"
    puts $file " sf=[expr 1e4*$m_fSf0],[expr 1e4*$m_fSf1],[expr 1e4*$m_fSf2]"
    puts $file " gg=$m_fGg0,$m_fGg1,$m_fGg2"
#   I will not use level spacings for p- and d-waves at the moment.
#   It will be automatically calculated in the code WRIURR
    puts $file " ds0=$m_fD0"
    puts $file " icon=$m_bIcon1,$m_bIcon2,$m_bIcon3"
    if {$m_fGPower == ""} {set m_fGPower 1}
    puts $file " gpow=$m_fGPower"
    puts $file " e0=$m_fErmax"
    if {[string first "\*" $m_fDe] == -1} {
      if {$m_fEumax == 0} {set m_fEumax 20e6}
      puts -nonewline $file " de="
      for {set i 1; set e0 1} {$i<=$m_fDe} {incr i} {
        set e1 [expr pow(10,($i*log10($m_fEumax-$m_fErmax)/$m_fDe))]
        if {$i>1} {puts -nonewline $file ","}
        if {$i>1 && [expr int(($i-1)/7)*7] == $i-1} {puts $file ""}
        puts -nonewline $file [format " %9.2f" [expr $e1-$e0]]
        set e0 $e1
      }
      puts $file ""
    } else {
      puts $file " de=$m_fDe"
    }
    puts $file "&end"
    close $file
#   run WRIURR
    if {[file exists /usr/bin/tee]} {
      exec xterm -T WRIURR -e sh -c "$m_szCodeDir/wriurr | /usr/bin/tee wriurr.std"
    } else {
      exec xterm -T WRIURR -e sh -c "$m_szCodeDir/wriurr > wriurr.std"
    }
#    exec rm -f wriurr.inp
    if {![file exists endfu.txt]} {
      tk_dialog .msgbox "Error" "Failed to run WRIURR.\nCheck the standard output of WRIURR" info 0 OK
      return
    } else {
      exec cp -af endfu.txt $m_szFile-res.endf
    }
    exec mv -f wriurr.lis $m_szFile-log.wriurr
    set m_bGotWRIURR 1
  }
  if {$m_bAllcodes2 != 0 || $m_bRECENT != 0} {
    exec rm -f recent.std
    exec rm -f sigma1.std
#   make a input file for RECENT
    set file [open "RECENT.INP" w]
    puts $file [format "%11d%11.4E%11d%11d%11d%11d" 0 1e-10 1 1 1 1]
    puts $file "endfu.txt\nendfu.pw"
    puts $file [format "%11d%11d\n" 1 9999]
    puts $file [format "%11.4E%11.4E\n" 0 0.01]
    close $file
#   run RECENT
    if {[file exists /usr/bin/tee]} {
      exec xterm -T RECENT -e sh -c "$m_szBaseDir/util/recent/recent | /usr/bin/tee recent.std"
    } else {
      exec xterm -T RECENT -e sh -c "$m_szBaseDir/util/recent/recent > recent.std"
    }
    exec rm -f RECENT.INP endfu.txt
#   make a input file for SIGMA1
    set file [open "SIGMA1.INP" w]
    puts $file [format "%11d%11d%11.5E%11.5E%11d" 0 2 300.0 1e-10 1]
    puts $file "endfu.pw\nendfu.bpw"
    puts $file [format "%11d%11d\n" 1 9999]
    puts $file [format "%11.5E%11.5E\n" 0.0 1e-3]
    close $file
#   run SIGMA1
    if {[file exists /usr/bin/tee]} {
      exec xterm -T SIGMA1 -e sh -c "$m_szBaseDir/util/sigma1/sigma1 | /usr/bin/tee sigma1.std"
    } else {
      exec xterm -T SIGMA1 -e sh -c "$m_szBaseDir/util/sigma1/sigma1 > sigma1.std"
    }
    exec rm -f SIGMA1.INP endfu.pw
    exec mv -f endfu.bpw $m_szFile-res.pendf
    set m_bGotRECENT 1
  }
}
#############################################################################
## Procedure:  GetSymbol

proc ::GetSymbol {nZ} {
  set Symbol [split "H He Li Be B C N O F Ne Na Mg Al Si P S Cl Ar K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn Fr Ra Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf Db Sg Ns Hs Mt ??"]
  if {$nZ > 109} {
    return [lindex $Symbol 109]
  } else {
    return [lindex $Symbol [expr $nZ-1]]
  }
}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {
  if {$argc < 2} {
    puts "usage: resonance.tcl ZA MAT"
    exit
  }
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
    wm maxsize $top 1425 848
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

proc vTclWindow.top71 {base} {
    if {$base == ""} {
        set base .top71
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $top -class Toplevel \
        -highlightcolor black 
    wm focusmodel $top passive
    wm geometry $top 617x543+623+144; update
    wm maxsize $top 1585 1120
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Resonance module"
    vTcl:DefineAlias "$top" "Toplevel1" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd84 \
        -borderwidth 1 -relief sunken -height 25 -width 225 
    vTcl:DefineAlias "$top.cpd84" "Frame2" vTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.cpd84
    menubutton $site_3_0.01 \
        -anchor w -menu "$site_3_0.01.02" -padx 4 -pady 3 -text File -width 4 
    vTcl:DefineAlias "$site_3_0.01" "Menubutton4" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.01.02 \
        -activeborderwidth 1 -borderwidth 1 -font {Helvetica -12 bold} \
        -tearoff 0 
    vTcl:DefineAlias "$site_3_0.01.02" "Menu1" vTcl:WidgetProc "" 1
    $site_3_0.01.02 add command \
        -accelerator Ctrl+L -command LoadVars -label Load 
    $site_3_0.01.02 add command \
        -accelerator Ctrl+X -command {Cleanup; exit} -label Exit 
    menubutton $site_3_0.05 \
        -anchor w -menu "$site_3_0.05.06" -padx 4 -pady 3 -text Help -width 4 
    vTcl:DefineAlias "$site_3_0.05" "Menubutton6" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.05.06 \
        -activeborderwidth 1 -borderwidth 1 -font {Tahoma 8} -tearoff 0 
    vTcl:DefineAlias "$site_3_0.05.06" "Menu1" vTcl:WidgetProc "" 1
    $site_3_0.05.06 add command \
        \
        -command {
  tk_dialog .msgbox "About" "Resonance module for EMPIRE\nWritten by Y.S.Cho\n (Last updated on 02/06/2007)\nwith the help of M.Herman and S.F.Mughabghab" info 0 OK
        } \
        -label About 
    menubutton $site_3_0.men67 \
        -menu "$site_3_0.men67.m" -padx 6 -pady 4 -text Input 
    vTcl:DefineAlias "$site_3_0.men67" "Menubutton3" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.men67.m \
        -tearoff 0 
    $site_3_0.men67.m add command \
        -accelerator Ctrl+C -command {exec $m_szEditor $m_szZAname.c4} \
        -label {Edit C4} 
    $site_3_0.men67.m add command \
        -accelerator Ctrl+E -command {exec $m_szEditor $m_szFile.exf} \
        -label {Edit EXFOR} 
    $site_3_0.men67.m add command \
        -command {exec $m_szEditor $m_szZAname-inp.sen} \
        -label {Sensitivity input} 	
    $site_3_0.men67.m add command \
        -accelerator {} \
        -command {if {[file exists $m_szZAname-expcorr.kal]} {
    exec $m_szEditor $m_szZAname-expcorr.kal &
  } else {
    tk_dialog .msgbox "Error" "KALMAN experimental correlations not found. Run KALMAN first" info 0 OK
  }} \
        -label {Experimental correlations} 
    $site_3_0.men67.m add command \
        -accelerator {} \
        -command {if {[file exists $m_szZAname-expxsc.kal]} {
    exec $m_szEditor $m_szZAname-expxsc.kal &
  } else {
    tk_dialog .msgbox "Error" "KALMAN experimental cross-sections not found. Run KALMAN first" info 0 OK
  }} \
        -label {Experimental x-sec} 
    $site_3_0.men67.m add command \
        -accelerator {} \
        -command {if {[file exists $m_szZAname-parcorr.kal]} {
    exec $m_szEditor $m_szZAname-parcorr.kal &
  } else {
    tk_dialog .msgbox "Error" "Parameter correlations for KALMAN not found. Run KALMAN first" info 0 OK
  }} \
        -label {Kalman parameter unc.} 
    menubutton $site_3_0.men72 \
        -menu "$site_3_0.men72.m" -padx 6 -pady 4 -text Outputs 
    vTcl:DefineAlias "$site_3_0.men72" "Menubutton1" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.men72.m \
        -tearoff 0 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men72" -command {} -label PTANAL 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men72 \
        -tearoff 0 
    $site_5_0.men72 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists ptanal.std]} {
    exec $m_szEditor ptanal.std &
  }
        } \
        -label log 
    $site_5_0.men72 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists $m_szFile-log.ptanal]} {
    exec $m_szEditor $m_szFile-log.ptanal &
  }
        } \
        -label output 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men71" -command {} -label WRIURR 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men71 \
        -tearoff 0 
    $site_5_0.men71 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists wriurr.std]} {
    exec $m_szEditor wriurr.std &
  }
        } \
        -label log 
    $site_5_0.men71 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists $m_szFile-log.wriurr]} {
      exec $m_szEditor $m_szFile-log.wriurr &
  }
        } \
        -label output 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men73" -command {} -label RECENT 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men73 \
        -tearoff 0 
    $site_5_0.men73 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists recent.std]} {
    exec $m_szEditor recent.std &
  }
        } \
        -label log 
    $site_5_0.men73 add command \
        \
        -command {
  if {$m_bGotRECENT} {
    cd $m_szWorkingDir
    exec $m_szEditor RECENT.LST &
  }
         } \
        -label output 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men74" -command {} -label KALMAN 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men74 \
        -tearoff 1 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szZAname-mat.sen]} {
    exec $m_szEditor $m_szZAname-mat.sen &
  } else {
    tk_dialog .msgbox "Error" "Sensitivity matrix not found. Run 'Sensitivity' first" info 0 OK
  }
        } \
        -label {Sensitivity matrix} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szZAname-out.kal]} {
    exec $m_szEditor $m_szZAname-out.kal &
  } else {
    tk_dialog .msgbox "Error" "KALMAN output not found. Run KALMAN first" info 0 OK
  }
        } \
        -label {Kalman output} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szZAname-xsc.kal]} {
    exec $m_szEditor $m_szZAname-xsc.kal &
  } else {
    tk_dialog .msgbox "Error" "KALMAN x-sections not found. Run KALMAN first" info 0 OK
  }
        } \
        -label {Calculated cross-sections} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szZAname-cov.kal]} {
    exec $m_szEditor $m_szZAname-cov.kal &
  } else {
    tk_dialog .msgbox "Error" "KALMAN covariance matrices not found. Run KALMAN first" info 0 OK
  }
        } \
        -label {Covariance matrices} 
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.05 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side right 
    pack $site_3_0.men67 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.men72 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    ::iwidgets::tabnotebook $top.tab85 \
        -height 2000 -tabpos n 
    vTcl:DefineAlias "$top.tab85" "Tabnotebook2" vTcl:WidgetProc "Toplevel1" 1
    $top.tab85 add \
        -label Input 
    $top.tab85 add \
        -label Execute 
    $top.tab85 add \
        -label {} 
    set site_8_0 [lindex [$top.tab85 childsite] 0]
    label $site_8_0.lab89 \
        -anchor w -text ZA: 
    vTcl:DefineAlias "$site_8_0.lab89" "Label3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab89 "$site_8_0.lab89 Label $top all _vTclBalloon"
    bind $site_8_0.lab89 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Z*1000+A of target}
    }
    entry $site_8_0.ent92 \
        -background white -insertbackground black -state normal \
        -textvariable m_nZA 
    vTcl:DefineAlias "$site_8_0.ent92" "Entry1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent92 "$site_8_0.ent92 Entry $top all _vTclBalloon"
    bind $site_8_0.ent92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Z*1000+A of target}
    }
    label $site_8_0.lab90 \
        -anchor w -text MAT: 
    vTcl:DefineAlias "$site_8_0.lab90" "Label4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab90 "$site_8_0.lab90 Label $top all _vTclBalloon"
    bind $site_8_0.lab90 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Material number}
    }
    entry $site_8_0.ent93 \
        -background white -insertbackground black -state normal \
        -textvariable m_nMAT 
    vTcl:DefineAlias "$site_8_0.ent93" "Entry2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent93 "$site_8_0.ent93 Entry $top all _vTclBalloon"
    bind $site_8_0.ent93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Material number}
    }
    button $site_8_0.but76 \
        \
        -command {set m_szFile [format "%06d" $m_nZA]; set m_szZAname [format "za%06d" $m_nZA]; LoadVars;} \
        -text Reload 
    vTcl:DefineAlias "$site_8_0.but76" "Button7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but76 "$site_8_0.but76 Button $top all _vTclBalloon"
    bind $site_8_0.but76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Reload data}
    }
    label $site_8_0.lab75 \
        -text s-wave 
    vTcl:DefineAlias "$site_8_0.lab75" "Label13" vTcl:WidgetProc "Toplevel1" 1
    label $site_8_0.lab76 \
        -text p-wave 
    vTcl:DefineAlias "$site_8_0.lab76" "Label14" vTcl:WidgetProc "Toplevel1" 1
    label $site_8_0.lab78 \
        -text d-wave 
    vTcl:DefineAlias "$site_8_0.lab78" "Label15" vTcl:WidgetProc "Toplevel1" 1
    label $site_8_0.lab72 \
        -anchor w -justify left -text SF: 
    vTcl:DefineAlias "$site_8_0.lab72" "Label11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab72 "$site_8_0.lab72 Label $top all _vTclBalloon"
    bind $site_8_0.lab72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Strength function}
    }
    entry $site_8_0.ent73 \
        -background white -insertbackground black -textvariable m_fSf0 
    vTcl:DefineAlias "$site_8_0.ent73" "Entry9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent73 "$site_8_0.ent73 Entry $top all _vTclBalloon"
    bind $site_8_0.ent73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {s-wave neutron strength function}
    }
    entry $site_8_0.ent79 \
        -background white -insertbackground black -textvariable m_fSf1 
    vTcl:DefineAlias "$site_8_0.ent79" "Entry10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent79 "$site_8_0.ent79 Entry $top all _vTclBalloon"
    bind $site_8_0.ent79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {p-wave neutron strength function}
    }
    entry $site_8_0.ent80 \
        -background white -insertbackground black -textvariable m_fSf2 
    vTcl:DefineAlias "$site_8_0.ent80" "Entry11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent80 "$site_8_0.ent80 Entry $top all _vTclBalloon"
    bind $site_8_0.ent80 <<SetBalloon>> {
        set ::vTcl::balloon::%W {d-wave neutron strength function}
    }
    label $site_8_0.lab91 \
        -anchor w -justify left -text Gg: 
    vTcl:DefineAlias "$site_8_0.lab91" "Label5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab91 "$site_8_0.lab91 Label $top all _vTclBalloon"
    bind $site_8_0.lab91 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Average gamma width [meV]}
    }
    entry $site_8_0.ent94 \
        -background white -insertbackground black -textvariable m_fGg0 
    vTcl:DefineAlias "$site_8_0.ent94" "Entry3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent94 "$site_8_0.ent94 Entry $top all _vTclBalloon"
    bind $site_8_0.ent94 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Average gamma width for s-wave [meV]}
    }
    entry $site_8_0.ent98 \
        -background white -insertbackground black -textvariable m_fGg1 
    vTcl:DefineAlias "$site_8_0.ent98" "Entry4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent98 "$site_8_0.ent98 Entry $top all _vTclBalloon"
    bind $site_8_0.ent98 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Average gamma width for p-wave [meV]}
    }
    entry $site_8_0.ent81 \
        -background white -insertbackground black -textvariable m_fGg2 
    vTcl:DefineAlias "$site_8_0.ent81" "Entry12" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent81 "$site_8_0.ent81 Entry $top all _vTclBalloon"
    bind $site_8_0.ent81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Average gamma width for d-wave [meV]}
    }
    label $site_8_0.lab82 \
        -anchor w -justify left -text {D :} 
    vTcl:DefineAlias "$site_8_0.lab82" "Label6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab82 "$site_8_0.lab82 Label $top all _vTclBalloon"
    bind $site_8_0.lab82 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Average level spacings [eV]}
    }
    entry $site_8_0.ent83 \
        -background white -insertbackground black -textvariable m_fD0 
    vTcl:DefineAlias "$site_8_0.ent83" "Entry13" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent83 "$site_8_0.ent83 Entry $top all _vTclBalloon"
    bind $site_8_0.ent83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Average level spacing of s-wave resonances [eV]}
    }
    label $site_8_0.cpd71 \
        -anchor w -justify left -text {spin cut-off :} 
    vTcl:DefineAlias "$site_8_0.cpd71" "Label19" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.cpd71 "$site_8_0.cpd71 Label $top all _vTclBalloon"
    bind $site_8_0.cpd71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Spin cut-off parameter}
    }
    entry $site_8_0.cpd72 \
        -background white -insertbackground black -textvariable m_fCutoff 
    vTcl:DefineAlias "$site_8_0.cpd72" "Entry20" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.cpd72 "$site_8_0.cpd72 Entry $top all _vTclBalloon"
    bind $site_8_0.cpd72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Spin cut-off parameter}
    }
    ::iwidgets::labeledframe $site_8_0.lab71 \
        -foreground #000000 -labelpos nw -labeltext {Resolved region} 
    vTcl:DefineAlias "$site_8_0.lab71" "Labelframe1" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_0.lab71 childsite]
    label $site_10_0.cpd75 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left \
        -text {Emax     :} 
    vTcl:DefineAlias "$site_10_0.cpd75" "Label9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd75 "$site_10_0.cpd75 Label $top all _vTclBalloon"
    bind $site_10_0.cpd75 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Upper energy limit for resolved region [eV]}
    }
    entry $site_10_0.cpd76 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fErmax 
    vTcl:DefineAlias "$site_10_0.cpd76" "Entry7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd76 "$site_10_0.cpd76 Entry $top all _vTclBalloon"
    bind $site_10_0.cpd76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Upper energy limit for resolved region [eV]}
    }
    label $site_10_0.lab71 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left \
        -text {R'         :} 
    vTcl:DefineAlias "$site_10_0.lab71" "Label18" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.lab71 "$site_10_0.lab71 Label $top all _vTclBalloon"
    bind $site_10_0.lab71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Scattering radius [fm]}
    }
    entry $site_10_0.ent72 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fR 
    vTcl:DefineAlias "$site_10_0.ent72" "Entry19" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent72 "$site_10_0.ent72 Entry $top all _vTclBalloon"
    bind $site_10_0.ent72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Scattering radius [fm]}
    }
    label $site_10_0.cpd71 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left -text Gn0_cut: 
    vTcl:DefineAlias "$site_10_0.cpd71" "Label7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd71 "$site_10_0.cpd71 Label $top all _vTclBalloon"
    bind $site_10_0.cpd71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Weak resonance cutoff width for s-wave [meV]}
    }
    entry $site_10_0.cpd72 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fGn0_cut 
    vTcl:DefineAlias "$site_10_0.cpd72" "Entry5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd72 "$site_10_0.cpd72 Entry $top all _vTclBalloon"
    bind $site_10_0.cpd72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {weak resonance cutoff width for s-wave [meV]}
    }
    label $site_10_0.cpd73 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left -text Gn1_cut: 
    vTcl:DefineAlias "$site_10_0.cpd73" "Label8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd73 "$site_10_0.cpd73 Label $top all _vTclBalloon"
    bind $site_10_0.cpd73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {weak resonance cutoff width for p-wave [meV]}
    }
    entry $site_10_0.cpd74 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fGn1_cut 
    vTcl:DefineAlias "$site_10_0.cpd74" "Entry6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd74 "$site_10_0.cpd74 Entry $top all _vTclBalloon"
    bind $site_10_0.cpd74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {weak resonance cutoff width for p-wave [meV]}
    }
    place $site_10_0.cpd75 \
        -in $site_10_0 -x 10 -y 5 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd76 \
        -in $site_10_0 -x 80 -y 5 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab71 \
        -in $site_10_0 -x 185 -y 5 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent72 \
        -in $site_10_0 -x 256 -y 5 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd71 \
        -in $site_10_0 -x 10 -y 35 -width 60 -anchor nw -bordermode inside 
    place $site_10_0.cpd72 \
        -in $site_10_0 -x 80 -y 35 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd73 \
        -in $site_10_0 -x 185 -y 35 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd74 \
        -in $site_10_0 -x 256 -y 35 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    ::iwidgets::labeledframe $site_8_0.lab77 \
        -foreground #000000 -labelpos nw -labeltext {Unresolved region} 
    vTcl:DefineAlias "$site_8_0.lab77" "Labelframe2" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_0.lab77 childsite]
    label $site_10_0.lab89 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left \
        -text {a  l.d.p. :} 
    vTcl:DefineAlias "$site_10_0.lab89" "Label16" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.lab89 "$site_10_0.lab89 Label $top all _vTclBalloon"
    bind $site_10_0.lab89 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Level density parameter [1/MeV]}
    }
    entry $site_10_0.ent90 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fLdp 
    vTcl:DefineAlias "$site_10_0.ent90" "Entry17" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent90 "$site_10_0.ent90 Entry $top all _vTclBalloon"
    bind $site_10_0.ent90 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Level density parameter [1/MeV]}
    }
    label $site_10_0.cpd78 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left \
        -text {gPower  :} 
    vTcl:DefineAlias "$site_10_0.cpd78" "Label10" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.cpd79 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fGPower 
    vTcl:DefineAlias "$site_10_0.cpd79" "Entry8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd79 "$site_10_0.cpd79 Entry $top all _vTclBalloon"
    bind $site_10_0.cpd79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Power on energy dependence of gamma width}
    }
    label $site_10_0.lab86 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -justify left \
        -text {Emax    :} 
    vTcl:DefineAlias "$site_10_0.lab86" "Label12" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.lab86 "$site_10_0.lab86 Label $top all _vTclBalloon"
    bind $site_10_0.lab86 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Upper energy boundary for unresolved region [eV]}
    }
    entry $site_10_0.ent88 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fEumax 
    vTcl:DefineAlias "$site_10_0.ent88" "Entry16" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent88 "$site_10_0.ent88 Entry $top all _vTclBalloon"
    bind $site_10_0.ent88 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Upper energy boundary for unresolved region [eV]}
    }
    label $site_10_0.lab73 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -text energies: 
    vTcl:DefineAlias "$site_10_0.lab73" "Label20" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.lab73 "$site_10_0.lab73 Label $top all _vTclBalloon"
    bind $site_10_0.lab73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Number of energy intervals or user-defined energy intervals}
    }
    entry $site_10_0.ent74 \
        -background white -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable m_fDe 
    vTcl:DefineAlias "$site_10_0.ent74" "Entry21" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent74 "$site_10_0.ent74 Entry $top all _vTclBalloon"
    bind $site_10_0.ent74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Number of energy intervals or user-defined energy intervals}
    }
    checkbutton $site_10_0.che71 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black \
        -text {Energy dependent D and neutron width for s-wave} \
        -variable m_bIcon1 
    vTcl:DefineAlias "$site_10_0.che71" "Checkbutton5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.che71 "$site_10_0.che71 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.che71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Energy dependent level spacing and neutron width for s-wave}
    }
    checkbutton $site_10_0.che72 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black \
        -text {Energy dependent D and gamma width for p-wave} \
        -variable m_bIcon2 
    vTcl:DefineAlias "$site_10_0.che72" "Checkbutton6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.che72 "$site_10_0.che72 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.che72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Energy dependent level spacing and gamma width for p-wave}
    }
    checkbutton $site_10_0.che74 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black \
        -text {Energy dependent D and gamma width for d-wave} \
        -variable m_bIcon3 
    vTcl:DefineAlias "$site_10_0.che74" "Checkbutton8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.che74 "$site_10_0.che74 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.che74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Energy dependent level spacing and gamma width for d-wave}
    }
    place $site_10_0.lab89 \
        -in $site_10_0 -x 10 -y 10 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent90 \
        -in $site_10_0 -x 80 -y 10 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd78 \
        -in $site_10_0 -x 185 -y 10 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd79 \
        -in $site_10_0 -x 260 -y 10 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab86 \
        -in $site_10_0 -x 10 -y 42 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent88 \
        -in $site_10_0 -x 80 -y 42 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab73 \
        -in $site_10_0 -x 185 -y 42 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent74 \
        -in $site_10_0 -x 260 -y 42 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.che71 \
        -in $site_10_0 -x 11 -y 72 -width 329 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.che72 \
        -in $site_10_0 -x 11 -y 97 -width 327 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.che74 \
        -in $site_10_0 -x 11 -y 122 -width 327 -height 22 -anchor nw \
        -bordermode ignore 
    button $site_8_0.but75 \
        \
        -command {
  cd $m_szWorkingDir
  if {![file exists $m_szFile.atlas]} {
    tk_dialog .msgbox "Error" [format "Local resonance parameter table '%s' not found" $m_szFile.atlas] info 0 OK
    return
  }
  exec $m_szEditor $m_szFile.atlas &
    } \
        -text {Edit resonance parameters} -width 211 
    vTcl:DefineAlias "$site_8_0.but75" "Button6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but75 "$site_8_0.but75 Button $top all _vTclBalloon"
    bind $site_8_0.but75 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View local resonance parameter table}
    }
    button $site_8_0.but74 \
        \
        -command {
  set psfn [format "z%03d.ps" $m_nZ]
  if {[file exists $m_szAtlasDir/post-script/$psfn]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/$psfn &
  } else {
    tk_dialog .msgbox "Error" "Atlas of Neutron Resonances not found" info 0 OK
  }
    } \
        -text {Atlas of Neutron Resonances} 
    vTcl:DefineAlias "$site_8_0.but74" "Button5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but74 "$site_8_0.but74 Button $top all _vTclBalloon"
    bind $site_8_0.but74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View Atlas of Neutron Resonances}
    }
    button $site_8_0.but65 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/rprime.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/rprime.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of R' not found" info 0 OK
  }
    } \
        -text {Scattering radius} 
    vTcl:DefineAlias "$site_8_0.but65" "Button13" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but66 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/density.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/density.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of level density parameter not found" info 0 OK
  }
    } \
        -text {Level density parameter} 
    vTcl:DefineAlias "$site_8_0.but66" "Button14" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but67 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/swave.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/swave.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of strength function for s-wave not found" info 0 OK
  }
    } \
        -text {Strength function for s-wave} 
    vTcl:DefineAlias "$site_8_0.but67" "Button15" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but68 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/pwave.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/pwave.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of strength function for p-wave not found" info 0 OK
  }
    } \
        -text {Strength function for p-wave} 
    vTcl:DefineAlias "$site_8_0.but68" "Button16" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but69 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/dwave.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/dwave.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of strength function for d-wave not found" info 0 OK
  }
    } \
        -text {Strength function for d-wave} 
    vTcl:DefineAlias "$site_8_0.but69" "Button17" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but70 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/gammagamma0.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/gammagamma0.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of s-wave average capture width not found" info 0 OK
  }
    } \
        -text {s-wave average capture width} 
    vTcl:DefineAlias "$site_8_0.but70" "Button18" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but71 \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/gammagamma1.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/gammagamma1.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of p-wave average capture width not found" info 0 OK
  }
    } \
        -text {p-wave average capture width} 
    vTcl:DefineAlias "$site_8_0.but71" "Button19" vTcl:WidgetProc "Toplevel1" 1
    place $site_8_0.lab89 \
        -in $site_8_0 -x 20 -y 14 -width 25 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent92 \
        -in $site_8_0 -x 57 -y 13 -width 60 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab90 \
        -in $site_8_0 -x 144 -y 14 -width 37 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent93 \
        -in $site_8_0 -x 184 -y 13 -width 60 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but76 \
        -in $site_8_0 -x 279 -y 10 -width 93 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab75 \
        -in $site_8_0 -x 81 -y 47 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab76 \
        -in $site_8_0 -x 193 -y 47 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab78 \
        -in $site_8_0 -x 302 -y 47 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab72 \
        -in $site_8_0 -x 20 -y 74 -width 29 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent73 \
        -in $site_8_0 -x 70 -y 74 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent79 \
        -in $site_8_0 -x 180 -y 74 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent80 \
        -in $site_8_0 -x 290 -y 74 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab91 \
        -in $site_8_0 -x 20 -y 101 -width 34 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent94 \
        -in $site_8_0 -x 70 -y 101 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent98 \
        -in $site_8_0 -x 180 -y 101 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent81 \
        -in $site_8_0 -x 290 -y 101 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab82 \
        -in $site_8_0 -x 20 -y 128 -width 33 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent83 \
        -in $site_8_0 -x 70 -y 128 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.cpd71 \
        -in $site_8_0 -x 180 -y 128 -width 84 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.cpd72 \
        -in $site_8_0 -x 290 -y 128 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab71 \
        -in $site_8_0 -x 7 -y 163 -width 372 -height 96 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab77 \
        -in $site_8_0 -x 7 -y 261 -width 372 -height 184 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but75 \
        -in $site_8_0 -x 388 -y 381 -width 211 -height 55 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but74 \
        -in $site_8_0 -x 387 -y 301 -width 211 -height 55 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but65 \
        -in $site_8_0 -x 387 -y 10 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but66 \
        -in $site_8_0 -x 387 -y 45 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but67 \
        -in $site_8_0 -x 387 -y 95 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but68 \
        -in $site_8_0 -x 387 -y 130 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but69 \
        -in $site_8_0 -x 387 -y 164 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but70 \
        -in $site_8_0 -x 387 -y 214 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but71 \
        -in $site_8_0 -x 387 -y 251 -width 211 -height 26 -anchor nw \
        -bordermode ignore 
    set site_8_1 [lindex [$top.tab85 childsite] 1]
    ::iwidgets::labeledframe $site_8_1.lab65 \
        -labelpos nw -labeltext {Uncertainty calc.} 
    vTcl:DefineAlias "$site_8_1.lab65" "Labeledframe2" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_1.lab65 childsite]
    label $site_10_0.lab71 \
        -anchor w -justify left -text {No. of resonances to be varied:} 
    vTcl:DefineAlias "$site_10_0.lab71" "Label1" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.ent72 \
        -background white -insertbackground black \
        -textvariable m_nNoResToBeAdjusted 
    vTcl:DefineAlias "$site_10_0.ent72" "Entry14" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent72 "$site_10_0.ent72 Entry $top all _vTclBalloon"
    bind $site_10_0.ent72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {No. of resonances that will be varied}
    }
    label $site_10_0.lab73 \
        -anchor w -justify left \
        -text {No. of additional resonances held fixed:} 
    vTcl:DefineAlias "$site_10_0.lab73" "Label2" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.ent74 \
        -background white -insertbackground black -textvariable m_nNoExtraRes 
    vTcl:DefineAlias "$site_10_0.ent74" "Entry15" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent74 "$site_10_0.ent74 Entry $top all _vTclBalloon"
    bind $site_10_0.ent74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {No. of additional resonances that will be held fixed (Enter 9999 to include all resonances)}
    }
    button $site_10_0.but73 \
        -command { RunKALMAN } -text {Run codes} 
    vTcl:DefineAlias "$site_10_0.but73" "Button4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but73 "$site_10_0.but73 Button $top all _vTclBalloon"
    bind $site_10_0.but73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run the selected codes}
    }
    label $site_10_0.lab65 \
        -anchor w -text {No. of resonances to write out:} 
    vTcl:DefineAlias "$site_10_0.lab65" "Label17" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.ent66 \
        -background white -insertbackground black \
        -textvariable m_nNoResToBeEvaluated 
    vTcl:DefineAlias "$site_10_0.ent66" "Entry18" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent66 "$site_10_0.ent66 Entry $top all _vTclBalloon"
    bind $site_10_0.ent66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {No. of resonances to write out}
    }
    text $site_10_0.tex65 \
        -background white -insertbackground black 
    vTcl:DefineAlias "$site_10_0.tex65" "Text1" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $site_10_0.che66 \
        -anchor w \
        -command {if {$m_bAllcodes1 ==1} {
    set m_bSensitivity 1
    set m_bKALMAN 1
   } else {
    set m_bSensitivity 0
    set m_bKALMAN 0
   }} \
        -justify left -overrelief flat -text {All codes} \
        -variable m_bAllcodes1 
    vTcl:DefineAlias "$site_10_0.che66" "Checkbutton7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.che66 "$site_10_0.che66 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.che66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {All codes}
    }
    checkbutton $site_10_0.che67 \
        -anchor w -justify left -text Sensitivity -variable m_bSensitivity 
    vTcl:DefineAlias "$site_10_0.che67" "Checkbutton9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.che67 "$site_10_0.che67 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.che67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Generate the sensitivity matrix for KALMAN}
    }
    checkbutton $site_10_0.che68 \
        -anchor w -justify left -text KALMAN -variable m_bKALMAN 
    vTcl:DefineAlias "$site_10_0.che68" "Checkbutton10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.che68 "$site_10_0.che68 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.che68 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for uncertainty and covariance estimation}
    }
    button $site_10_0.but65 \
        -command {exec $m_szEditor $m_szZAname-parcorr.kal} \
        -text {Parameter unc.} 
    vTcl:DefineAlias "$site_10_0.but65" "Button9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but65 "$site_10_0.but65 Button $top all _vTclBalloon"
    bind $site_10_0.but65 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit uncertainties of resonance parameter for KALMAN}
    }
    label $site_10_0.lab66 \
        -anchor w -text {Reaction to be considered in KALMAN:} 
    vTcl:DefineAlias "$site_10_0.lab66" "Label21" vTcl:WidgetProc "Toplevel1" 1
    radiobutton $site_10_0.rad65 \
        -anchor w -text None -value 0 -variable m_nKALMT 
    vTcl:DefineAlias "$site_10_0.rad65" "Radiobutton4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.rad65 "$site_10_0.rad65 Radiobutton $top all _vTclBalloon"
    bind $site_10_0.rad65 <<SetBalloon>> {
        set ::vTcl::balloon::%W {None}
    }
    radiobutton $site_10_0.rad66 \
        -anchor w -text 1 -value 1 -variable m_nKALMT 
    vTcl:DefineAlias "$site_10_0.rad66" "Radiobutton5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.rad66 "$site_10_0.rad66 Radiobutton $top all _vTclBalloon"
    bind $site_10_0.rad66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Total reaction}
    }
    radiobutton $site_10_0.rad67 \
        -text 2 -value 2 -variable m_nKALMT 
    vTcl:DefineAlias "$site_10_0.rad67" "Radiobutton6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.rad67 "$site_10_0.rad67 Radiobutton $top all _vTclBalloon"
    bind $site_10_0.rad67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Elastic scattering}
    }
    radiobutton $site_10_0.rad68 \
        -anchor w -text 18 -value 18 -variable m_nKALMT 
    vTcl:DefineAlias "$site_10_0.rad68" "Radiobutton1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.rad68 "$site_10_0.rad68 Radiobutton $top all _vTclBalloon"
    bind $site_10_0.rad68 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Fission reaction}
    }
    radiobutton $site_10_0.rad69 \
        -anchor w -text 102 -value 102 -variable m_nKALMT 
    vTcl:DefineAlias "$site_10_0.rad69" "Radiobutton2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.rad69 "$site_10_0.rad69 Radiobutton $top all _vTclBalloon"
    bind $site_10_0.rad69 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Capture reaction}
    }
    radiobutton $site_10_0.rad70 \
        -anchor w -text All -value 9999 -variable m_nKALMT 
    vTcl:DefineAlias "$site_10_0.rad70" "Radiobutton3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.rad70 "$site_10_0.rad70 Radiobutton $top all _vTclBalloon"
    bind $site_10_0.rad70 <<SetBalloon>> {
        set ::vTcl::balloon::%W {All reactions}
    }
    place $site_10_0.lab71 \
        -in $site_10_0 -x 10 -y 10 -width 218 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent72 \
        -in $site_10_0 -x 255 -y 9 -width 47 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab73 \
        -in $site_10_0 -x 10 -y 36 -width 243 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent74 \
        -in $site_10_0 -x 255 -y 35 -width 47 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.but73 \
        -in $site_10_0 -x 413 -y 9 -width 110 -height 30 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab65 \
        -in $site_10_0 -x 10 -y 62 -width 214 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent66 \
        -in $site_10_0 -x 255 -y 61 -width 47 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.tex65 \
        -in $site_10_0 -x 540 -y 92 -anchor nw -bordermode ignore 
    place $site_10_0.che66 \
        -in $site_10_0 -x 311 -y 9 -width 82 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.che67 \
        -in $site_10_0 -x 314 -y 34 -width 94 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.che68 \
        -in $site_10_0 -x 314 -y 59 -width 82 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.but65 \
        -in $site_10_0 -x 413 -y 47 -width 110 -height 30 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab66 \
        -in $site_10_0 -x 10 -y 89 -width 241 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.rad65 \
        -in $site_10_0 -x 253 -y 88 -width 58 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.rad66 \
        -in $site_10_0 -x 313 -y 88 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.rad67 \
        -in $site_10_0 -x 348 -y 88 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.rad68 \
        -in $site_10_0 -x 384 -y 89 -width 42 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.rad69 \
        -in $site_10_0 -x 428 -y 89 -width 48 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.rad70 \
        -in $site_10_0 -x 478 -y 88 -width 43 -height 22 -anchor nw \
        -bordermode ignore 
    ::iwidgets::labeledframe $site_8_1.lab66 \
        -labelpos nw -labeltext Evaluation 
    vTcl:DefineAlias "$site_8_1.lab66" "Labeledframe3" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_1.lab66 childsite]
    checkbutton $site_10_0.cpd67 \
        -anchor w \
        -command {if {$m_bAllcodes2 ==1} {
    set m_bPTANAL 1
    set m_bWRIURR 1
    set m_bRECENT 1
  } else {
    set m_bPTANAL 0
    set m_bWRIURR 0
    set m_bRECENT 0
  }} \
        -justify left -text {All codes} -variable m_bAllcodes2 
    vTcl:DefineAlias "$site_10_0.cpd67" "Checkbutton1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd67 "$site_10_0.cpd67 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.cpd67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {All codes}
    }
    checkbutton $site_10_0.cpd68 \
        -anchor w -text PTANAL -variable m_bPTANAL 
    vTcl:DefineAlias "$site_10_0.cpd68" "Checkbutton2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd68 "$site_10_0.cpd68 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.cpd68 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for the resolved region}
    }
    checkbutton $site_10_0.cpd69 \
        -anchor w -text WRIURR -variable m_bWRIURR 
    vTcl:DefineAlias "$site_10_0.cpd69" "Checkbutton3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd69 "$site_10_0.cpd69 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.cpd69 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for the unresolved region}
    }
    checkbutton $site_10_0.cpd70 \
        -anchor w -text RECENT/SIGMA -variable m_bRECENT -wraplength 54 
    vTcl:DefineAlias "$site_10_0.cpd70" "Checkbutton4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd70 "$site_10_0.cpd70 Checkbutton $top all _vTclBalloon"
    bind $site_10_0.cpd70 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for the pointwise cross sections}
    }
    ::iwidgets::labeledframe $site_10_0.cpd65 \
        -labelpos nw -labeltext Analysis 
    vTcl:DefineAlias "$site_10_0.cpd65" "Labeledframe1" vTcl:WidgetProc "Toplevel1" 1
    set site_12_0 [$site_10_0.cpd65 childsite]
    button $site_12_0.cpd65 \
        \
        -command {
  if {$m_bGotPTANAL == 1} {
    cd $m_szWorkingDir
    if {[file exists /usr/bin/tee]} {
      exec xterm -T scanr -e sh -c "$m_szCodeDir/scanr | /usr/bin/tee scanr.std"
    } else {
      exec xterm -T scanr -e sh -c "$m_szCodeDir/scanr > scanr.std"
    }
    if {![file exists scanr.ps]} {
      tk_dialog .msgbox "Error" "Plot not found\nProbably your GNUPLOT is outdated" info 0 OK
      return
    }
    exec mv -f scanr.ps $m_szFile-res.ps
    exec $m_szPsviewer $m_szFile-res.ps &
  } else {
    tk_dialog .msgbox "Error" "Run PTANAL first" info 0 OK
  }
    } \
        -text {Cumulative plot} 
    vTcl:DefineAlias "$site_12_0.cpd65" "Button3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd65 "$site_12_0.cpd65 Button $top all _vTclBalloon"
    bind $site_12_0.cpd65 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot cumulative plot of resonance energies}
    }
    button $site_12_0.cpd66 \
        \
        -command {
  if {$m_bGotPTANAL} {
    cd $m_szWorkingDir
    if {![file exists ptdist.ps]} {
      tk_dialog .msgbox "Error" "Plot not found\nProbably your GNUPLOT is outdated" info 0 OK
      return
    }
    exec $m_szPsviewer ptdist.ps &
  } else {
    tk_dialog .msgbox "Error" "Run PTANAL first" info 0 OK
  }
        } \
        -text {Porter-Thomas analysis} 
    vTcl:DefineAlias "$site_12_0.cpd66" "Button10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd66 "$site_12_0.cpd66 Button $top all _vTclBalloon"
    bind $site_12_0.cpd66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Porter-Thomas analysis}
    }
    place $site_12_0.cpd65 \
        -in $site_12_0 -x 15 -y 11 -width 341 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_12_0.cpd66 \
        -in $site_12_0 -x 15 -y 43 -width 341 -height 26 -anchor nw \
        -bordermode ignore 
    ::iwidgets::labeledframe $site_10_0.lab65 \
        -labelpos nw -labeltext Comparison 
    vTcl:DefineAlias "$site_10_0.lab65" "Labeledframe4" vTcl:WidgetProc "Toplevel1" 1
    set site_12_0 [$site_10_0.lab65 childsite]
    button $site_12_0.cpd66 \
        -command {PlotXS 1 $m_bENDF $m_bJENDL $m_bJEFF} -text Total 
    vTcl:DefineAlias "$site_12_0.cpd66" "Button11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd66 "$site_12_0.cpd66 Button $top all _vTclBalloon"
    bind $site_12_0.cpd66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot total cross sections at resonance region}
    }
    button $site_12_0.cpd67 \
        -command {PlotXS 2 $m_bENDF $m_bJENDL $m_bJEFF} -text Scattering 
    vTcl:DefineAlias "$site_12_0.cpd67" "Button8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd67 "$site_12_0.cpd67 Button $top all _vTclBalloon"
    bind $site_12_0.cpd67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot scattering cross sections at resonance region}
    }
    button $site_12_0.cpd68 \
        -command {PlotXS 102 $m_bENDF $m_bJENDL $m_bJEFF} -text Capture 
    vTcl:DefineAlias "$site_12_0.cpd68" "Button12" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd68 "$site_12_0.cpd68 Button $top all _vTclBalloon"
    bind $site_12_0.cpd68 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot capture cross sections at resonance region}
    }
    checkbutton $site_12_0.cpd69 \
        -anchor w -text ENDF/B-VII -variable m_bENDF 
    vTcl:DefineAlias "$site_12_0.cpd69" "Checkbutton13" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd69 "$site_12_0.cpd69 Checkbutton $top all _vTclBalloon"
    bind $site_12_0.cpd69 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compare with ENDF/B VII}
    }
    checkbutton $site_12_0.cpd70 \
        -anchor w -justify left -text JENDL-3.3 -variable m_bJENDL 
    vTcl:DefineAlias "$site_12_0.cpd70" "Checkbutton12" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd70 "$site_12_0.cpd70 Checkbutton $top all _vTclBalloon"
    bind $site_12_0.cpd70 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compare with JENDL-3.3}
    }
    checkbutton $site_12_0.cpd71 \
        -anchor w -justify left -text JEFF-3.1 -variable m_bJEFF 
    vTcl:DefineAlias "$site_12_0.cpd71" "Checkbutton11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.cpd71 "$site_12_0.cpd71 Checkbutton $top all _vTclBalloon"
    bind $site_12_0.cpd71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compare with JEFF-3.1}
    }
    place $site_12_0.cpd66 \
        -in $site_12_0 -x 15 -y 10 -width 100 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_12_0.cpd67 \
        -in $site_12_0 -x 15 -y 41 -width 100 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_12_0.cpd68 \
        -in $site_12_0 -x 15 -y 72 -width 100 -height 26 -anchor nw \
        -bordermode ignore 
    place $site_12_0.cpd69 \
        -in $site_12_0 -x 140 -y 12 -width 93 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_12_0.cpd70 \
        -in $site_12_0 -x 140 -y 43 -width 89 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_12_0.cpd71 \
        -in $site_12_0 -x 140 -y 74 -width 78 -height 22 -anchor nw \
        -bordermode ignore 
    button $site_10_0.cpd71 \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists $m_szFile-res.endf]} {
    exec $m_szEditor $m_szFile-res.endf &
  } elseif {$m_bGotPTANAL == 1} {
    exec $m_szEditor endfr.txt &
   }
    } \
        -text ENDF -width 112 
    vTcl:DefineAlias "$site_10_0.cpd71" "Button2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd71 "$site_10_0.cpd71 Button $top all _vTclBalloon"
    bind $site_10_0.cpd71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View ENDF file}
    }
    button $site_10_0.cpd72 \
        -command { RunCodes } \
        -font [vTcl:font:getFontFromDescr "-family helvetica -size 12 -weight bold"] \
        -text {Run codes} 
    vTcl:DefineAlias "$site_10_0.cpd72" "Button1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd72 "$site_10_0.cpd72 Button $top all _vTclBalloon"
    bind $site_10_0.cpd72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run the selected codes}
    }
    place $site_10_0.cpd67 \
        -in $site_10_0 -x 10 -y 11 -width 90 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd68 \
        -in $site_10_0 -x 17 -y 39 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd69 \
        -in $site_10_0 -x 17 -y 61 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd70 \
        -in $site_10_0 -x 17 -y 83 -width 80 -height 36 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd65 \
        -in $site_10_0 -x 132 -y 1 -width 396 -height 110 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab65 \
        -in $site_10_0 -x 132 -y 111 -width 266 -height 139 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd71 \
        -in $site_10_0 -x 410 -y 118 -width 112 -height 123 -anchor nw \
        -bordermode ignore 
    place $site_10_0.cpd72 \
        -in $site_10_0 -x 17 -y 131 -width 97 -height 110 -anchor nw \
        -bordermode ignore 
    place $site_8_1.lab65 \
        -in $site_8_1 -x 1 -y 299 -width 561 -height 154 -anchor nw \
        -bordermode ignore 
    place $site_8_1.lab66 \
        -in $site_8_1 -x 1 -y 11 -width 561 -height 284 -anchor nw \
        -bordermode ignore 
    set site_8_2 [lindex [$top.tab85 childsite] 2]
    $top.tab85 select 0
    ###################
    # SETTING GEOMETRY
    ###################
    pack $top.cpd84 \
        -in $top -anchor n -expand 1 -fill x -side top 
    pack $top.tab85 \
        -in $top -anchor n -expand 1 -fill both -side top 

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
#############################################################################
## Binding tag:  _vTclBalloon


if {![info exists vTcl(sourcing)]} {
bind "_vTclBalloon" <<KillBalloon>> {
    namespace eval ::vTcl::balloon {
        after cancel $id
        if {[winfo exists .vTcl.balloon]} {
            destroy .vTcl.balloon
        }
        set set 0
    }
}
bind "_vTclBalloon" <<vTclBalloon>> {
    if {$::vTcl::balloon::first != 1} {break}

    namespace eval ::vTcl::balloon {
        set first 2
        if {![winfo exists .vTcl]} {
            toplevel .vTcl; wm withdraw .vTcl
        }
        if {![winfo exists .vTcl.balloon]} {
            toplevel .vTcl.balloon -bg black
        }
        wm overrideredirect .vTcl.balloon 1
        label .vTcl.balloon.l  -text ${%W} -relief flat  -bg #ffffaa -fg black -padx 2 -pady 0 -anchor w
        pack .vTcl.balloon.l -side left -padx 1 -pady 1
        wm geometry  .vTcl.balloon  +[expr {[winfo rootx %W]+[winfo width %W]/2}]+[expr {[winfo rooty %W]+[winfo height %W]+4}]
        set set 1
    }
}
bind "_vTclBalloon" <Button> {
    namespace eval ::vTcl::balloon {
        set first 0
    }
    vTcl:FireEvent %W <<KillBalloon>>
}
bind "_vTclBalloon" <Enter> {
    namespace eval ::vTcl::balloon {
        ## self defining balloon?
        if {![info exists %W]} {
            vTcl:FireEvent %W <<SetBalloon>>
        }
        set set 0
        set first 1
        set id [after 500 {vTcl:FireEvent %W <<vTclBalloon>>}]
    }
}
bind "_vTclBalloon" <Leave> {
    namespace eval ::vTcl::balloon {
        set first 0
    }
    vTcl:FireEvent %W <<KillBalloon>>
}
bind "_vTclBalloon" <Motion> {
    namespace eval ::vTcl::balloon {
        if {!$set} {
            after cancel $id
            set id [after 500 {vTcl:FireEvent %W <<vTclBalloon>>}]
        }
    }
}
}

Window show .
Window show .top71

main $argc $argv
