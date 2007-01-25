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
    }
    namespace eval ::widgets::$site_3_0.03 {
        array set save {-anchor 1 -menu 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_3_0.03.04 {
        array set save {-activeborderwidth 1 -borderwidth 1 -font 1 -tearoff 1}
    }
    namespace eval ::widgets::$site_3_0.05 {
        array set save {-anchor 1 -menu 1 -padx 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_3_0.05.06 {
        array set save {-activeborderwidth 1 -borderwidth 1 -font 1 -tearoff 1}
    }
    namespace eval ::widgets::$base.tab85 {
        array set save {-height 1 -tabpos 1}
    }
    set site_8_0 [lindex [$base.tab85 childsite] 0]
    namespace eval ::widgets::$site_8_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_0
    namespace eval ::widgets::$site_8_0.lab87 {
        array set save {-font 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.lab88 {
        array set save {-font 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.lab89 {
        array set save {-text 1}
    }
    namespace eval ::widgets::$site_8_0.ent92 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -state 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab90 {
        array set save {-text 1}
    }
    namespace eval ::widgets::$site_8_0.ent93 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -state 1 -textvariable 1}
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
        array set save {-_tooltip 1 -justify 1 -text 1}
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
        array set save {-_tooltip 1 -justify 1 -text 1}
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
        array set save {-_tooltip 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.ent83 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.ent84 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.ent85 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab71 {
        array set save {-foreground 1 -highlightcolor 1 -text 1}
    }
    set site_9_0 $site_8_0.lab71
    namespace eval ::widgets::$site_9_0.cpd71 {
        array set save {-justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.cpd72 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.cpd73 {
        array set save {-justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.cpd74 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.cpd75 {
        array set save {-justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.cpd76 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.lab71 {
        array set save {-justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.ent72 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_8_0.lab77 {
        array set save {-foreground 1 -highlightcolor 1 -text 1}
    }
    set site_9_0 $site_8_0.lab77
    namespace eval ::widgets::$site_9_0.cpd78 {
        array set save {-justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.cpd79 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.lab86 {
        array set save {-justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.ent88 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.lab73 {
        array set save {-_tooltip 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.ent74 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.lab89 {
        array set save {-_tooltip 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.ent90 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.lab71 {
        array set save {-_tooltip 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.ent72 {
        array set save {-_tooltip 1 -background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.che71 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che72 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che74 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$site_8_0.fra106 {
        array set save {-borderwidth 1 -height 1 -relief 1 -width 1}
    }
    set site_9_0 $site_8_0.fra106
    namespace eval ::widgets::$site_9_0.che108 {
        array set save {-anchor 1 -command 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che109 {
        array set save {-anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che110 {
        array set save {-anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che111 {
        array set save {-anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.but107 {
        array set save {-command 1 -font 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but73 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but71 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but72 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but75 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but74 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but76 {
        array set save {-command 1 -text 1}
    }
    set site_8_1 [lindex [$base.tab85 childsite] 1]
    namespace eval ::widgets::$site_8_1 {
        array set save {-background 1 -highlightcolor 1}
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

proc ::loadvars {} {
  global m_nZA
  global m_nZ
  global m_fAwt
  global m_fAbun
  global m_fBn
  global m_fSpin
  global m_fD0
  global m_fD1
  global m_fD2
  global m_fSf0
  global m_fSf1
  global m_fSf2
  global m_fGg0
  global m_fGg1
  global m_fGg2
  global m_fLevel2
  global m_fR

  global m_szCodeDir

  set m_nZ [expr int($m_nZA/1000)]

  set output [exec $m_szCodeDir/readrp $m_nZA]
  set output [regsub -all "\n" $output " "]
  set output [regsub -all " +" $output " "]
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
  set m_fLevel2 [lindex $output 13]
  if {$m_fLevel2==0} {puts "Warning: 2nd level energy is not given"}
  set m_fR [format "%.2f" [lindex $output 14]]
}

proc ::main {argc argv} {
}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {
  if {$argc < 2} {
    puts "usage: resonance.tcl ZA MAT"
    exit
  }

  global m_szCodeDir
  global m_szAtlasDir

  global m_bGotPTANAL
  global m_bGotWRIURR
  global m_bGotRECENT

  global m_fGPower
  global m_nZA
  global m_nMAT

  set m_szCodeDir ../util/resonance
  set m_szAtlasDir ../Atlas

  set m_bGotPTANAL 0
  set m_bGotWRIURR 0
  set m_bGotRECENT 0

  set m_fGPower 2.5
  
  set m_nZA [lindex $argv 0]
  set m_nMAT [lindex $argv 1]

  loadvars
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
    wm maxsize $top 1585 1120
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
    wm geometry $top 636x562+36+223; update
    wm maxsize $top 1585 1120
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Resonance region"
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
        -activeborderwidth 1 -borderwidth 1 -font {Tahoma 8} -tearoff 0 
    vTcl:DefineAlias "$site_3_0.01.02" "Menu1" vTcl:WidgetProc "" 1
    $site_3_0.01.02 add command \
        -accelerator Ctrl+O -label Open 
    $site_3_0.01.02 add command \
        -accelerator Ctrl+W -label Close 
    menubutton $site_3_0.03 \
        -anchor w -menu "$site_3_0.03.04" -padx 4 -pady 3 -text Edit -width 4 
    vTcl:DefineAlias "$site_3_0.03" "Menubutton5" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.03.04 \
        -activeborderwidth 1 -borderwidth 1 -font {Tahoma 8} -tearoff 0 
    vTcl:DefineAlias "$site_3_0.03.04" "Menu1" vTcl:WidgetProc "" 1
    $site_3_0.03.04 add command \
        -accelerator Ctrl+X -label Cut 
    $site_3_0.03.04 add command \
        -accelerator Ctrl+C -label Copy 
    $site_3_0.03.04 add command \
        -accelerator Ctrl+V -label Paste 
    $site_3_0.03.04 add command \
        -accelerator Del -label Delete 
    menubutton $site_3_0.05 \
        -anchor w -menu "$site_3_0.05.06" -padx 4 -pady 3 -text Help -width 4 
    vTcl:DefineAlias "$site_3_0.05" "Menubutton6" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.05.06 \
        -activeborderwidth 1 -borderwidth 1 -font {Tahoma 8} -tearoff 0 
    vTcl:DefineAlias "$site_3_0.05.06" "Menu1" vTcl:WidgetProc "" 1
    $site_3_0.05.06 add command \
        -label About 
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.03 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.05 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side right 
    ::iwidgets::tabnotebook $top.tab85 \
        -height 2000 -tabpos n 
    vTcl:DefineAlias "$top.tab85" "Tabnotebook2" vTcl:WidgetProc "Toplevel1" 1
    $top.tab85 add \
        -label {Page 1} 
    $top.tab85 add \
        -label {Page 2} 
    $top.tab85 add \
        -label {Page 3} 
    set site_8_0 [lindex [$top.tab85 childsite] 0]
    label $site_8_0.lab87 \
        \
        -font [vTcl:font:getFontFromDescr "-family helvetica -size 12 -weight bold"] \
        -text Input: 
    vTcl:DefineAlias "$site_8_0.lab87" "Label1" vTcl:WidgetProc "Toplevel1" 1
    label $site_8_0.lab88 \
        \
        -font [vTcl:font:getFontFromDescr "-family helvetica -size 12 -weight bold"] \
        -text Output: 
    vTcl:DefineAlias "$site_8_0.lab88" "Label2" vTcl:WidgetProc "Toplevel1" 1
    label $site_8_0.lab89 \
        -text ZA: 
    vTcl:DefineAlias "$site_8_0.lab89" "Label3" vTcl:WidgetProc "Toplevel1" 1
    entry $site_8_0.ent92 \
        -background white -insertbackground black -state normal \
        -textvariable m_nZA 
    vTcl:DefineAlias "$site_8_0.ent92" "Entry1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent92 "$site_8_0.ent92 Entry $top all _vTclBalloon"
    bind $site_8_0.ent92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {target Z*1000+A}
    }
    label $site_8_0.lab90 \
        -text MAT: 
    vTcl:DefineAlias "$site_8_0.lab90" "Label4" vTcl:WidgetProc "Toplevel1" 1
    entry $site_8_0.ent93 \
        -background white -insertbackground black -state normal \
        -textvariable m_nMAT 
    vTcl:DefineAlias "$site_8_0.ent93" "Entry2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent93 "$site_8_0.ent93 Entry $top all _vTclBalloon"
    bind $site_8_0.ent93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {material number}
    }
    button $site_8_0.but76 \
        -command {loadvars} -text Reload 
    vTcl:DefineAlias "$site_8_0.but76" "Button7" vTcl:WidgetProc "Toplevel1" 1
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
        -justify left -text SF: 
    vTcl:DefineAlias "$site_8_0.lab72" "Label11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab72 "$site_8_0.lab72 Label $top all _vTclBalloon"
    bind $site_8_0.lab72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Strength Function}
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
        -justify left -text Gg: 
    vTcl:DefineAlias "$site_8_0.lab91" "Label5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab91 "$site_8_0.lab91 Label $top all _vTclBalloon"
    bind $site_8_0.lab91 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Gamma width}
    }
    entry $site_8_0.ent94 \
        -background white -insertbackground black -textvariable m_fGg0 
    vTcl:DefineAlias "$site_8_0.ent94" "Entry3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent94 "$site_8_0.ent94 Entry $top all _vTclBalloon"
    bind $site_8_0.ent94 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average gamma width for s-wave}
    }
    entry $site_8_0.ent98 \
        -background white -insertbackground black -textvariable m_fGg1 
    vTcl:DefineAlias "$site_8_0.ent98" "Entry4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent98 "$site_8_0.ent98 Entry $top all _vTclBalloon"
    bind $site_8_0.ent98 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average gamma width for p-wave}
    }
    entry $site_8_0.ent81 \
        -background white -insertbackground black -textvariable m_fGg2 
    vTcl:DefineAlias "$site_8_0.ent81" "Entry12" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent81 "$site_8_0.ent81 Entry $top all _vTclBalloon"
    bind $site_8_0.ent81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average gamma width for d-wave}
    }
    label $site_8_0.lab82 \
        -justify left -text {D :} 
    vTcl:DefineAlias "$site_8_0.lab82" "Label6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.lab82 "$site_8_0.lab82 Label $top all _vTclBalloon"
    bind $site_8_0.lab82 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average level spacings}
    }
    entry $site_8_0.ent83 \
        -background white -insertbackground black -textvariable m_fD0 
    vTcl:DefineAlias "$site_8_0.ent83" "Entry13" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent83 "$site_8_0.ent83 Entry $top all _vTclBalloon"
    bind $site_8_0.ent83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average level spacing of s-wave resonances}
    }
    entry $site_8_0.ent84 \
        -background white -insertbackground black -textvariable m_fD1 
    vTcl:DefineAlias "$site_8_0.ent84" "Entry14" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent84 "$site_8_0.ent84 Entry $top all _vTclBalloon"
    bind $site_8_0.ent84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average level spacing of p-wave resonances}
    }
    entry $site_8_0.ent85 \
        -background white -insertbackground black -textvariable m_fD2 
    vTcl:DefineAlias "$site_8_0.ent85" "Entry15" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.ent85 "$site_8_0.ent85 Entry $top all _vTclBalloon"
    bind $site_8_0.ent85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {average level spacing of d-wave resonances}
    }
    labelframe $site_8_0.lab71 \
        -foreground black -text {Resolved region} -highlightcolor black 
    vTcl:DefineAlias "$site_8_0.lab71" "Labelframe1" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_0.lab71
    label $site_9_0.cpd71 \
        -justify left -text Gn0_cut: 
    vTcl:DefineAlias "$site_9_0.cpd71" "Label7" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.cpd72 \
        -background white -insertbackground black -textvariable m_fGn0_cut 
    vTcl:DefineAlias "$site_9_0.cpd72" "Entry5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd72 "$site_9_0.cpd72 Entry $top all _vTclBalloon"
    bind $site_9_0.cpd72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {weak resonance cutoff width for s-wave}
    }
    label $site_9_0.cpd73 \
        -justify left -text Gn1_cut: 
    vTcl:DefineAlias "$site_9_0.cpd73" "Label8" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.cpd74 \
        -background white -insertbackground black -textvariable m_fGn1_cut 
    vTcl:DefineAlias "$site_9_0.cpd74" "Entry6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd74 "$site_9_0.cpd74 Entry $top all _vTclBalloon"
    bind $site_9_0.cpd74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {weak resonance cutoff width for p-wave}
    }
    label $site_9_0.cpd75 \
        -justify left -text {Ecut    :} 
    vTcl:DefineAlias "$site_9_0.cpd75" "Label9" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.cpd76 \
        -background white -insertbackground black -textvariable m_fEcut 
    vTcl:DefineAlias "$site_9_0.cpd76" "Entry7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd76 "$site_9_0.cpd76 Entry $top all _vTclBalloon"
    bind $site_9_0.cpd76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {upper energy limit for analysis}
    }
    label $site_9_0.lab71 \
        -justify left -text {R'        :} 
    vTcl:DefineAlias "$site_9_0.lab71" "Label18" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.ent72 \
        -background white -insertbackground black -textvariable m_fR 
    vTcl:DefineAlias "$site_9_0.ent72" "Entry19" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.ent72 "$site_9_0.ent72 Entry $top all _vTclBalloon"
    bind $site_9_0.ent72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {scattering radius}
    }
    place $site_9_0.cpd71 \
        -in $site_9_0 -x 10 -y 10 -width 60 -anchor nw -bordermode inside 
    place $site_9_0.cpd72 \
        -in $site_9_0 -x 80 -y 10 -width 80 -anchor nw -bordermode inside 
    place $site_9_0.cpd73 \
        -in $site_9_0 -x 182 -y 26 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.cpd74 \
        -in $site_9_0 -x 255 -y 10 -width 80 -anchor nw -bordermode inside 
    place $site_9_0.cpd75 \
        -in $site_9_0 -x 12 -y 53 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.cpd76 \
        -in $site_9_0 -x 82 -y 53 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab71 \
        -in $site_9_0 -x 183 -y 52 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.ent72 \
        -in $site_9_0 -x 257 -y 53 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    labelframe $site_8_0.lab77 \
        -foreground black -text {Unresolved region} -highlightcolor black 
    vTcl:DefineAlias "$site_8_0.lab77" "Labelframe2" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_0.lab77
    label $site_9_0.cpd78 \
        -justify left -text gPower: 
    vTcl:DefineAlias "$site_9_0.cpd78" "Label10" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.cpd79 \
        -background white -insertbackground black -textvariable m_fGPower 
    vTcl:DefineAlias "$site_9_0.cpd79" "Entry8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd79 "$site_9_0.cpd79 Entry $top all _vTclBalloon"
    bind $site_9_0.cpd79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {power on energy dependence of gamma width}
    }
    label $site_9_0.lab86 \
        -justify left -text {e0        :} 
    vTcl:DefineAlias "$site_9_0.lab86" "Label12" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.ent88 \
        -background white -insertbackground black -textvariable m_fE0 
    vTcl:DefineAlias "$site_9_0.ent88" "Entry16" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.ent88 "$site_9_0.ent88 Entry $top all _vTclBalloon"
    bind $site_9_0.ent88 <<SetBalloon>> {
        set ::vTcl::balloon::%W {low energy boundary}
    }
    label $site_9_0.lab73 \
        -text energies: 
    vTcl:DefineAlias "$site_9_0.lab73" "Label20" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.lab73 "$site_9_0.lab73 Label $top all _vTclBalloon"
    bind $site_9_0.lab73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {number of energy interval}
    }
    entry $site_9_0.ent74 \
        -background white -insertbackground black -textvariable m_fDe 
    vTcl:DefineAlias "$site_9_0.ent74" "Entry21" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.ent74 "$site_9_0.ent74 Entry $top all _vTclBalloon"
    bind $site_9_0.ent74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Number of energy interval}
    }
    label $site_9_0.lab89 \
        -justify left -text {a  l.d.p. :} 
    vTcl:DefineAlias "$site_9_0.lab89" "Label16" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.lab89 "$site_9_0.lab89 Label $top all _vTclBalloon"
    bind $site_9_0.lab89 <<SetBalloon>> {
        set ::vTcl::balloon::%W {level density parameter}
    }
    entry $site_9_0.ent90 \
        -background white -insertbackground black -textvariable m_fDena 
    vTcl:DefineAlias "$site_9_0.ent90" "Entry17" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.ent90 "$site_9_0.ent90 Entry $top all _vTclBalloon"
    bind $site_9_0.ent90 <<SetBalloon>> {
        set ::vTcl::balloon::%W {level density parameter}
    }
    label $site_9_0.lab71 \
        -justify left -text {spin     :} 
    vTcl:DefineAlias "$site_9_0.lab71" "Label19" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.lab71 "$site_9_0.lab71 Label $top all _vTclBalloon"
    bind $site_9_0.lab71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {spin cut-off parameter}
    }
    entry $site_9_0.ent72 \
        -background white -insertbackground black -textvariable m_fDisp 
    vTcl:DefineAlias "$site_9_0.ent72" "Entry20" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.ent72 "$site_9_0.ent72 Entry $top all _vTclBalloon"
    bind $site_9_0.ent72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {spin cut-off parameter}
    }
    checkbutton $site_9_0.che71 \
        -text {energy dependent D and neutron width for s-wave} \
        -variable m_bIcon1 
    vTcl:DefineAlias "$site_9_0.che71" "Checkbutton5" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $site_9_0.che72 \
        -text {energy dependent D and gamma width for p-wave} \
        -variable m_bIcon2 
    vTcl:DefineAlias "$site_9_0.che72" "Checkbutton6" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $site_9_0.che74 \
        -text {energy dependent D and gamma width for d-wave} \
        -variable m_bIcon3 
    vTcl:DefineAlias "$site_9_0.che74" "Checkbutton8" vTcl:WidgetProc "Toplevel1" 1
    place $site_9_0.cpd78 \
        -in $site_9_0 -x 10 -y 10 -width 60 -anchor nw -bordermode inside 
    place $site_9_0.cpd79 \
        -in $site_9_0 -x 80 -y 26 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab86 \
        -in $site_9_0 -x 183 -y 25 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.ent88 \
        -in $site_9_0 -x 257 -y 25 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab73 \
        -in $site_9_0 -x 10 -y 54 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.ent74 \
        -in $site_9_0 -x 80 -y 54 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab89 \
        -in $site_9_0 -x 184 -y 55 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.ent90 \
        -in $site_9_0 -x 257 -y 55 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab71 \
        -in $site_9_0 -x 10 -y 83 -width 60 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.ent72 \
        -in $site_9_0 -x 80 -y 83 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che71 \
        -in $site_9_0 -x 13 -y 110 -width 329 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che72 \
        -in $site_9_0 -x 13 -y 135 -width 327 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che74 \
        -in $site_9_0 -x 13 -y 161 -width 327 -height 22 -anchor nw \
        -bordermode ignore 
    frame $site_8_0.fra106 \
        -borderwidth 2 -relief groove -height 180 -width 200 
    vTcl:DefineAlias "$site_8_0.fra106" "Frame3" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_0.fra106
    checkbutton $site_9_0.che108 \
        -anchor w \
        -command {
  if {$m_bAllcodes ==1} {
    set m_bPTANAL 1
    set m_bWRIURR 1
    set m_bRECENT 1
  } else {
    set m_bPTANAL 0
    set m_bWRIURR 0
    set m_bRECENT 0
  }} \
        -justify left -text All -variable m_bAllcodes 
    vTcl:DefineAlias "$site_9_0.che108" "Checkbutton1" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $site_9_0.che109 \
        -anchor w -text {Resolved region} -variable m_bPTANAL 
    vTcl:DefineAlias "$site_9_0.che109" "Checkbutton2" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $site_9_0.che110 \
        -anchor w -text {Unresolved region} -variable m_bWRIURR 
    vTcl:DefineAlias "$site_9_0.che110" "Checkbutton3" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $site_9_0.che111 \
        -anchor w -text RECENT -variable m_bRECENT 
    vTcl:DefineAlias "$site_9_0.che111" "Checkbutton4" vTcl:WidgetProc "Toplevel1" 1
    button $site_9_0.but107 \
        \
        -command {
  
  global m_nZA
  global m_nMAT
  global m_fAwt
  global m_fAbun
  global m_fSpin
  global m_fSf0
  global m_fSf1
  global m_fD0
  global m_fD1
  global m_fD2
  global m_fSf0
  global m_fSf1
  global m_fSf2
  global m_fGg0
  global m_fGg1
  global m_fGg2
  global m_fEcut
  global m_fGn0_cut
  global m_fGn1_cut
  global m_fLevel2

  global m_bAllcodes
  global m_bPTANAL
  global m_bGotPTANAL
  global m_bGotWRIURR
  global m_bGotRECENT

  if {$m_bAllcodes ==0 && $m_bPTANAL == 0 &&
      $m_bWRIURR == 0 && $m_bRECENT == 0} {
    wm withdraw .
    tk_dialog .msgbox "Error" "No job specified" info 0 OK
  }
#   verify the integrity of input befor run
  if {$m_bAllcodes != 0 || $m_bPTANAL != 0} {
  }
  if {$m_bAllcodes != 0 || $m_bWRIURR != 0} {
    if {$m_fE0 == ""} {
      tk_dialog .msgbox "Error" "Enter e0 (lower energy boundary)" info 0 OK
      return
    }
    if {$m_fDe == ""} {
      tk_dialog .msgbox "Error" "Enter de (energy intervals)" info 0 OK
      return
    }
    if {$m_fDena == ""} {
      tk_dialog .msgbox "Error" "Enter dena (level density parameter)" info 0 OK
      return
    }
    if {$m_fDisp == ""} {
      tk_dialog .msgbox "Error" "Enter disp (spin dispersion parameter)" info 0 OK
      return
    }
  }
  if {$m_bAllcodes != 0 || $m_bRECENT != 0} {
  }
  if {$m_bAllcodes != 0 || $m_bPTANAL != 0} {
#   make a input file
    set file [open "ptanal.inp" w]
    puts $file "&data"
    if {$m_fAbun !=0} {puts $file " abun=$m_fAbun"}
    puts $file " zam=$m_nZA mat=$m_nMAT awt=$m_fAwt spin=$m_fSpin"
    puts $file " sf0=$m_fSf0\n sf1=$m_fSf1,$m_fSf1,$m_fSf1\n D0=$m_fD0"
    puts $file " ggavg=$m_fGg0, $m_fGg1"
    if {$m_fEcut != ""} {puts $file " ecut=$m_fEcut"}
    if {$m_fGn0_cut != ""} {puts $file " gncut=$m_fGn0_cut,$m_fGn1_cut"}
    puts $file " ap=[format "%.2f" [expr $m_fR/10]]"
    puts $file "&end"
    close $file
#   run PTANAL
    exec $m_szCodeDir/ptanal > ptanal.std
    exec kedit ptanal.std 2> /dev/null &
    exec cp -a endfa.txt endfr.txt
    set m_bGotPTANAL 1
  }
  if {$m_bAllcodes != 0 || $m_bWRIURR != 0} {
#   make a input file
    set file [open "wriurr.inp" w]
    puts $file "&urr"
    puts $file " bn=$m_fBn dena=$m_fDena disp=$m_fDisp"
    puts $file " sf=$m_fSf0,$m_fSf1,$m_fSf2"
    puts $file " gg=$m_fGg0,$m_fGg1,$m_fGg2"
    puts $file " ds0=$m_fD0,$m_fD1,$m_fD2"
    puts $file " icon=$m_bIcon1,$m_bIcon2,$m_bIcon3"
    puts $file " gpow=$m_fGPower"
    puts $file " e0=$m_fE0"
    if {[string first "\*" $m_fDe] == -1} {
      if {$m_fLevel2 == 0} {set m_fLevel2 20}
      puts -nonewline $file " de="
      for {set i 1; set e0 1} {$i<=$m_fDe} {incr i} {
        set e1 [expr pow(10,($i*log10($m_fLevel2*1e6-$m_fE0)/$m_fDe))]
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
    exec $m_szCodeDir/wriurr > wriurr.std
    exec kedit wriurr.std 2> /dev/null &
    set m_bGotWRIURR 1
  }
  if {$m_bAllcodes != 0 || $m_bRECENT != 0} {
#   make a input file
    set file [open "RECENT.INP" w]
    puts $file [format "%11d%11.4E%11d%11d%11d%11d" 0 1e-10 1 1 1 1]
    puts $file "endfu.txt"
    puts $file "endfu.pw"
    puts $file [format "%11d%11d\n" 1 9999]
    puts $file [format "%11.4e%11.4e\n" 0 0.01]
    close $file
#   run RECENT
    exec ../util/recent/recent > recent.std
    exec kedit recent.std 2> /dev/null &
    set m_bGotRECENT 1
  }
  } \
        -font [vTcl:font:getFontFromDescr "-family helvetica -size 12 -weight bold"] \
        -text Run 
    vTcl:DefineAlias "$site_9_0.but107" "Button1" vTcl:WidgetProc "Toplevel1" 1
    place $site_9_0.che108 \
        -in $site_9_0 -x 20 -y 11 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che109 \
        -in $site_9_0 -x 20 -y 34 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che110 \
        -in $site_9_0 -x 20 -y 57 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che111 \
        -in $site_9_0 -x 20 -y 80 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.but107 \
        -in $site_9_0 -x 23 -y 112 -width 153 -height 58 -anchor nw \
        -bordermode ignore 
    button $site_8_0.but73 \
        \
        -command {
  global m_bGotPTANAL
  global m_bGotWRIURR
  global m_bGotRECENT
  if {$m_bGotPTANAL == 1} {exec kedit ptanal.lis 2> /dev/null &}
  if {$m_bGotWRIURR == 1} {exec kedit wriurr.lis 2> /dev/null &}
  if {$m_bGotRECENT == 1} {exec kedit RECENT.LST 2> /dev/null &}
    } \
        -text Summary 
    vTcl:DefineAlias "$site_8_0.but73" "Button4" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but71 \
        \
        -command {
  if {$m_bGotWRIURR == 1} {
    exec kedit endfu.txt 2> /dev/null &
  } elseif {$m_bGotPTANAL == 1} {
    exec kedit endfa.txt 2> /dev/null &
   }
    } \
        -text ENDF 
    vTcl:DefineAlias "$site_8_0.but71" "Button2" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but72 \
        \
        -command {
  if {$m_bGotPTANAL == 1} {
    exec $m_szCodeDir/scanr > scanr.std
    exec gnuplot -persist scanr.gp 2> /dev/null &
#    exec kedit scanr.std 2> /dev/null &
  }
    } \
        -text PLOT 
    vTcl:DefineAlias "$site_8_0.but72" "Button3" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but75 \
        \
        -command {
  exec kedit [format "za%06d.atlas" $m_nZA] 2> /dev/null &
    } \
        -text Parameters 
    vTcl:DefineAlias "$site_8_0.but75" "Button6" vTcl:WidgetProc "Toplevel1" 1
    button $site_8_0.but74 \
        \
        -command {
  set fn [format "z%03d.ps" $m_nZ]
  exec kghostview $m_szAtlasDir/post-script/$fn 2> /dev/null &
    } \
        -text Atlas 
    vTcl:DefineAlias "$site_8_0.but74" "Button5" vTcl:WidgetProc "Toplevel1" 1
    place $site_8_0.lab87 \
        -in $site_8_0 -x 176 -y 10 -width 51 -height 24 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab88 \
        -in $site_8_0 -x 465 -y 10 -width 66 -height 24 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab89 \
        -in $site_8_0 -x 20 -y 51 -width 25 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent92 \
        -in $site_8_0 -x 57 -y 50 -width 60 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab90 \
        -in $site_8_0 -x 145 -y 51 -width 37 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent93 \
        -in $site_8_0 -x 184 -y 50 -width 60 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab75 \
        -in $site_8_0 -x 81 -y 82 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab76 \
        -in $site_8_0 -x 193 -y 84 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab78 \
        -in $site_8_0 -x 302 -y 84 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab72 \
        -in $site_8_0 -x 20 -y 104 -width 29 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent73 \
        -in $site_8_0 -x 70 -y 105 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent79 \
        -in $site_8_0 -x 180 -y 104 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent80 \
        -in $site_8_0 -x 290 -y 104 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab91 \
        -in $site_8_0 -x 20 -y 131 -width 34 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent94 \
        -in $site_8_0 -x 70 -y 130 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent98 \
        -in $site_8_0 -x 180 -y 130 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent81 \
        -in $site_8_0 -x 290 -y 130 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab82 \
        -in $site_8_0 -x 20 -y 158 -width 33 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent83 \
        -in $site_8_0 -x 70 -y 156 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent84 \
        -in $site_8_0 -x 180 -y 156 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent85 \
        -in $site_8_0 -x 290 -y 156 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab71 \
        -in $site_8_0 -x 15 -y 189 -width 355 -height 86 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab77 \
        -in $site_8_0 -x 15 -y 285 -width 355 -height 191 -anchor nw \
        -bordermode ignore 
    place $site_8_0.fra106 \
        -in $site_8_0 -x 400 -y 293 -width 200 -height 183 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but73 \
        -in $site_8_0 -x 420 -y 47 -width 160 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but71 \
        -in $site_8_0 -x 420 -y 84 -width 160 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but72 \
        -in $site_8_0 -x 420 -y 120 -width 160 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but75 \
        -in $site_8_0 -x 420 -y 208 -width 160 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but74 \
        -in $site_8_0 -x 420 -y 245 -width 160 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but76 \
        -in $site_8_0 -x 277 -y 46 -width 93 -height 28 -anchor nw \
        -bordermode ignore 
    set site_8_1 [lindex [$top.tab85 childsite] 1]
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
