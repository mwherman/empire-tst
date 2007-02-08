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
    namespace eval ::widgets::$site_8_0.fra106 {
        array set save {-borderwidth 1 -height 1 -relief 1 -width 1}
    }
    set site_9_0 $site_8_0.fra106
    namespace eval ::widgets::$site_9_0.che108 {
        array set save {-_tooltip 1 -anchor 1 -command 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che109 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che110 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.che111 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_9_0.but107 {
        array set save {-_tooltip 1 -command 1 -font 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but71 {
        array set save {-_tooltip 1 -command 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.but72 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but79 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but73 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but77 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.but78 {
        array set save {-_tooltip 1 -command 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.che71 {
        array set save {-_tooltip 1 -anchor 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_8_0.che72 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_8_0.che73 {
        array set save {-_tooltip 1 -anchor 1 -justify 1 -text 1 -variable 1}
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
            InitVars
            LoadVars
            Cleanup
            PlotXS
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

  set m_bGotPTANAL 0
  set m_bGotWRIURR 0
  set m_bGotRECENT 0

  set m_fGPower 2.5
  set m_fErmax 500000
  set m_fDe 30
}

#############################################################################
## Procedure:  LoadVars

proc ::LoadVars {} {
  global m_nZA m_nZ m_nA
  global m_fAwt m_fAbun m_fBn m_fSpin
  global m_fD0 m_fD1 m_fD2
  global m_fSf0 m_fSf1 m_fSf2
  global m_fGg0 m_fGg1 m_fGg2
  global m_fErmax m_fEumax
  global m_fR
  global m_fLdp m_fCutoff

  global m_bGotPTANAL m_bGotWRIURR m_bGotRECENT

  global m_szBaseDir m_szCodeDir m_szFile m_szWorkingDir

  set m_nZA [expr int($m_nZA)]
  set m_nZ [expr $m_nZA/1000]
  set m_nA [expr $m_nZA%1000]

  set m_bGotPTANAL 0
  set m_bGotWRIURR 0
  set m_bGotRECENT 0

  cd $m_szWorkingDir

  set fn [format "za%06d.atlas" $m_nZA]
  if {[file exists $m_szFile.atlas]} {
    exec mv -f $m_szFile.atlas $fn
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
  set m_fErmax [lindex $output 14]
  set m_fEumax [lindex $output 15]
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

  if {[file exists $fn]} {
    exec mv -f $fn $m_szFile.atlas
  } else {
    puts [format "Error: Local resonance parameter table '%s' not found" $fn]
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
  exec rm -f ptanal.inp ptanal.lis ptanal.std
  exec rm -f wriurr.inp wriurr.lis wriurr.std
  exec rm -f RECENT.INP RECENT.LST recent.std
  exec rm -f SIGMA1.INP SIGMA1.LST sigma1.std
# I don't use something like "rm -f pt*" for safety
  exec rm -f ptdist0.dat ptdist0.fit ptdist1.dat ptdist1.fit ptdist.dat ptdist.fit ptdist.gp ptdist.ps
  exec rm -f l0.txt l1.txt l2.txt scanr.gp scanr.ps scanr.std
  exec rm -f endfr.txt endfa.txt endfu.txt endfu.pw endfu.bpw
  exec rm -f [format "za%06d.atlas" $m_nZA]
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
## Procedure:  RunCodes

proc ::RunCodes {} {
  global m_szBaseDir m_szFile m_szWorkingDir m_szCodeDir
  global m_bAllcodes m_bPTANAL m_bWRIURR m_bRECENT
  global m_nZA m_nMAT
  global m_fAwt m_fAbun m_fBn m_fSpin
  global m_fD0 m_fD1 m_fD2
  global m_fSf0 m_fSf1 m_fSf2
  global m_fGg0 m_fGg1 m_fGg2
  global m_fErmax m_fEumax
  global m_fR m_fCutoff m_fDe m_fLdp m_fGPower
  global m_fGn0_cut m_fGn1_cut
  global m_bIcon1 m_bIcon2 m_bIcon3
  global m_bGotPTANAL m_bGotWRIURR m_bGotRECENT

  cd $m_szWorkingDir
  if {$m_bAllcodes ==0 && $m_bPTANAL == 0 &&
      $m_bWRIURR == 0 && $m_bRECENT == 0} {
    tk_dialog .msgbox "Error" "No job specified" info 0 OK
  }
#   verify the integrity of input befor run
  if {$m_bAllcodes != 0 || $m_bPTANAL != 0} {
  }
  if {$m_bAllcodes != 0 || $m_bWRIURR != 0} {
    if {$m_bAllcodes == 0 && $m_bPTANAL == 0 && $m_bGotPTANAL == 0} {
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
  if {$m_bAllcodes != 0 || $m_bRECENT != 0} {
    if {$m_bAllcodes == 0 && $m_bWRIURR == 0 && $m_bGotWRIURR == 0} {
      tk_dialog .msgbox "Error" "Run PTANAL and WRIURR first" info 0 OK
      return
    }
  }
  if {$m_bAllcodes != 0 || $m_bPTANAL != 0} {
    if {![file exists $m_szFile.atlas]} {
      tk_dialog .msgbox "Error" "Local resonance parameter table '$m_szFile.atlas' not found" info 0 OK
      return
    }
    set fn [format "za%06d.atlas" $m_nZA]
    exec cp -f $m_szFile.atlas $fn
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
    puts $file " ap=[format "%.2f" [expr $m_fR/10]]"
    puts $file "&end"
    close $file
#   run PTANAL
    if {[file exists /usr/bin/tee]} {
      exec xterm -T PTANAL -e sh -c "$m_szCodeDir/ptanal | /usr/bin/tee ptanal.std"
    } else {
      exec xterm -T PTANAL -e sh -c "$m_szCodeDir/ptanal > ptanal.std"
    }
    exec rm -f ptanal.inp $fn
    exec mv -f ptanal.lis $m_szFile-log.ptanal
    if {![file exists endfa.txt]} {
      tk_dialog .msgbox "Error" "Failed to run PTANAL.\nCheck the standard output of PTANAL" info 0 OK
      return
    } else {
      exec mv -f endfa.txt endfr.txt
    }
    set m_bGotPTANAL 1
  }
  if {$m_bAllcodes != 0 || $m_bWRIURR != 0} {
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
    exec rm -f wriurr.inp
    if {![file exists endfu.txt]} {
      tk_dialog .msgbox "Error" "Failed to run WRIURR.\nCheck the standard output of WRIURR" info 0 OK
      return
    } else {
      exec cp -af endfu.txt $m_szFile-res.endf
    }
    exec mv -f wriurr.lis $m_szFile-log.wriurr
    set m_bGotWRIURR 1
  }
  if {$m_bAllcodes != 0 || $m_bRECENT != 0} {
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
    wm geometry $top 617x543+36+223; update
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
        -activeborderwidth 1 -borderwidth 1 -tearoff 0 
    vTcl:DefineAlias "$site_3_0.01.02" "Menu1" vTcl:WidgetProc "" 1
    $site_3_0.01.02 add command \
        -accelerator Ctrl+L -command LoadVars -label Load 
    $site_3_0.01.02 add command \
        -accelerator Ctrl+C -command {exec $m_szEditor $m_szFile.c4} -label {Edit C4}
    $site_3_0.01.02 add command \
        -accelerator Ctrl+E -command {exec $m_szEditor $m_szFile.exf} -label {Edit EXFOR}
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
    menubutton $site_3_0.men72 \
        -menu "$site_3_0.men72.m" -padx 6 -pady 4 -text View 
    vTcl:DefineAlias "$site_3_0.men72" "Menubutton1" vTcl:WidgetProc "Toplevel1" 1
    menu $site_3_0.men72.m \
        -tearoff 0 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men72" -command {} -label {Standard output} 
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
        -label PTANAL 
    $site_5_0.men72 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists wriurr.std]} {
    exec $m_szEditor wriurr.std &
  }
        } \
        -label WRIURR 
    $site_5_0.men72 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists recent.std]} {
    exec $m_szEditor recent.std &
  }
        } \
        -label RECENT 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men71" -command {} -label {List output} 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men71 \
        -tearoff 0 
    $site_5_0.men71 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists $m_szFile-log.ptanal]} {
    exec $m_szEditor $m_szFile-log.ptanal &
  }
        } \
        -label PTANAL 
    $site_5_0.men71 add command \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists $m_szFile-log.wriurr]} {
      exec $m_szEditor $m_szFile-log.wriurr &
  }
        } \
        -label WRIURR 
    $site_5_0.men71 add command \
        \
        -command {
  if {$m_bGotRECENT} {
    cd $m_szWorkingDir
    exec $m_szEditor RECENT.LST &
  }
        } \
        -label RECENT 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men73" -command {} -label Plot 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men73 \
        -tearoff 0 
    $site_5_0.men73 add command \
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
        -label {Porter-Thomas analysis} 
    $site_5_0.men73 add command \
        \
        -command {
  if {$m_bGotPTANAL} {
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
        -label {Cumulative plot} 
    $site_3_0.men72.m add cascade \
        -menu "$site_3_0.men72.m.men74" -command {} -label Systematics 
    set site_5_0 $site_3_0.men72.m
    menu $site_5_0.men74 \
        -tearoff 0 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/rprime.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/rprime.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of R' not found" info 0 OK
  }
        } \
        -label R' 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/density.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/density.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of level density parameter not found" info 0 OK
  }
        } \
        -label {Level density parameter} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/swave.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/swave.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of strength function for s-wave not found" info 0 OK
  }
        } \
        -label {Strength function for s-wave} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/pwave.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/pwave.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of strength function for p-wave not found" info 0 OK
  }
        } \
        -label {Strength function for p-wave} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/dwave.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/dwave.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of strength function for d-wave not found" info 0 OK
  }
        } \
        -label {Strength function for d-wave} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/gammagamma0.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/gammagamma0.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of s-wave average capture width not found" info 0 OK
  }
        } \
        -label {s-wave average capture width} 
    $site_5_0.men74 add command \
        \
        -command {
  if {[file exists $m_szAtlasDir/post-script/gammagamma1.eps]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/gammagamma1.eps &
  } else {
    tk_dialog .msgbox "Error" "Plot of p-wave average capture width not found" info 0 OK
  }
        } \
        -label {p-wave average capture width} 
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.05 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side right 
    pack $site_3_0.men72 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    ::iwidgets::tabnotebook $top.tab85 \
        -height 2000 -tabpos n 
    vTcl:DefineAlias "$top.tab85" "Tabnotebook2" vTcl:WidgetProc "Toplevel1" 1
    $top.tab85 add \
        -label Main 
    $top.tab85 add \
        
    $top.tab85 add \
        
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
        -command {set m_szFile [format "%06d" $m_nZA]; LoadVars;} \
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
        -text {Resonance parameters} -width 201 
    vTcl:DefineAlias "$site_8_0.but75" "Button6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but75 "$site_8_0.but75 Button $top all _vTclBalloon"
    bind $site_8_0.but75 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View local resonance parameter table}
    }
    button $site_8_0.but74 \
        \
        -command {
  set fn [format "z%03d.ps" $m_nZ]
  if {[file exists $m_szAtlasDir/post-script/$fn]} {
    exec $m_szPsviewer $m_szAtlasDir/post-script/$fn &
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
    frame $site_8_0.fra106 \
        -borderwidth 2 -relief groove -height 153 -width 200 
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
    bindtags $site_9_0.che108 "$site_9_0.che108 Checkbutton $top all _vTclBalloon"
    bind $site_9_0.che108 <<SetBalloon>> {
        set ::vTcl::balloon::%W {All codes}
    }
    checkbutton $site_9_0.che109 \
        -anchor w -text PTANAL -variable m_bPTANAL 
    vTcl:DefineAlias "$site_9_0.che109" "Checkbutton2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.che109 "$site_9_0.che109 Checkbutton $top all _vTclBalloon"
    bind $site_9_0.che109 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for the resolved region}
    }
    checkbutton $site_9_0.che110 \
        -anchor w -text WRIURR -variable m_bWRIURR 
    vTcl:DefineAlias "$site_9_0.che110" "Checkbutton3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.che110 "$site_9_0.che110 Checkbutton $top all _vTclBalloon"
    bind $site_9_0.che110 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for the unresolved region}
    }
    checkbutton $site_9_0.che111 \
        -anchor w -text RECENT -variable m_bRECENT 
    vTcl:DefineAlias "$site_9_0.che111" "Checkbutton4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.che111 "$site_9_0.che111 Checkbutton $top all _vTclBalloon"
    bind $site_9_0.che111 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Code for the pointwise cross sections}
    }
    button $site_9_0.but107 \
        -command { RunCodes } \
        -font [vTcl:font:getFontFromDescr "-family helvetica -size 12 -weight bold"] \
        -text Run 
    vTcl:DefineAlias "$site_9_0.but107" "Button1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but107 "$site_9_0.but107 Button $top all _vTclBalloon"
    bind $site_9_0.but107 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run the specified codes}
    }
    place $site_9_0.che108 \
        -in $site_9_0 -x 20 -y 7 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che109 \
        -in $site_9_0 -x 20 -y 29 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che110 \
        -in $site_9_0 -x 20 -y 51 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.che111 \
        -in $site_9_0 -x 20 -y 73 -width 153 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.but107 \
        -in $site_9_0 -x 23 -y 101 -width 153 -height 40 -anchor nw \
        -bordermode ignore 
    button $site_8_0.but71 \
        \
        -command {
  cd $m_szWorkingDir
  if {[file exists $m_szFile-res.endf]} {
    exec $m_szEditor $m_szFile-res.endf &
  } elseif {$m_bGotPTANAL == 1} {
    exec $m_szEditor endfr.txt &
   }
    } \
        -text ENDF -width 92 
    vTcl:DefineAlias "$site_8_0.but71" "Button2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but71 "$site_8_0.but71 Button $top all _vTclBalloon"
    bind $site_8_0.but71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View ENDF file}
    }
    button $site_8_0.but72 \
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
    vTcl:DefineAlias "$site_8_0.but72" "Button3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but72 "$site_8_0.but72 Button $top all _vTclBalloon"
    bind $site_8_0.but72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot cumulative plot of resonance energies}
    }
    button $site_8_0.but79 \
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
    vTcl:DefineAlias "$site_8_0.but79" "Button10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but79 "$site_8_0.but79 Button $top all _vTclBalloon"
    bind $site_8_0.but79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Porter-Thomas analysis}
    }
    button $site_8_0.but73 \
        -command {PlotXS 1 $m_bENDF $m_bJENDL $m_bJEFF} -text Total 
    vTcl:DefineAlias "$site_8_0.but73" "Button4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but73 "$site_8_0.but73 Button $top all _vTclBalloon"
    bind $site_8_0.but73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot total cross sections at resonance region}
    }
    button $site_8_0.but77 \
        -command {PlotXS 2 $m_bENDF $m_bJENDL $m_bJEFF} -text Scattering 
    vTcl:DefineAlias "$site_8_0.but77" "Button8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but77 "$site_8_0.but77 Button $top all _vTclBalloon"
    bind $site_8_0.but77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot scattering cross sections at resonance region}
    }
    button $site_8_0.but78 \
        -command {PlotXS 102 $m_bENDF $m_bJENDL $m_bJEFF} -text Capture 
    vTcl:DefineAlias "$site_8_0.but78" "Button9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.but78 "$site_8_0.but78 Button $top all _vTclBalloon"
    bind $site_8_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Plot capture cross sections at resonance region}
    }
    checkbutton $site_8_0.che71 \
        -anchor w -text ENDF/B-VII -variable m_bENDF 
    vTcl:DefineAlias "$site_8_0.che71" "Checkbutton7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.che71 "$site_8_0.che71 Checkbutton $top all _vTclBalloon"
    bind $site_8_0.che71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compare with ENDF/B VII}
    }
    checkbutton $site_8_0.che72 \
        -anchor w -justify left -text JENDL-3.3 -variable m_bJENDL 
    vTcl:DefineAlias "$site_8_0.che72" "Checkbutton9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.che72 "$site_8_0.che72 Checkbutton $top all _vTclBalloon"
    bind $site_8_0.che72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compare with JENDL-3.3}
    }
    checkbutton $site_8_0.che73 \
        -anchor w -justify left -text JEFF-3.1 -variable m_bJEFF 
    vTcl:DefineAlias "$site_8_0.che73" "Checkbutton10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_8_0.che73 "$site_8_0.che73 Checkbutton $top all _vTclBalloon"
    bind $site_8_0.che73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compare with JEFF-3.1}
    }
    place $site_8_0.lab87 \
        -in $site_8_0 -x 176 -y 7 -width 51 -height 24 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab88 \
        -in $site_8_0 -x 456 -y 7 -width 66 -height 24 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab89 \
        -in $site_8_0 -x 20 -y 46 -width 25 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent92 \
        -in $site_8_0 -x 57 -y 45 -width 60 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab90 \
        -in $site_8_0 -x 144 -y 46 -width 37 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent93 \
        -in $site_8_0 -x 184 -y 45 -width 60 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but76 \
        -in $site_8_0 -x 279 -y 41 -width 93 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab75 \
        -in $site_8_0 -x 81 -y 77 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab76 \
        -in $site_8_0 -x 193 -y 79 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab78 \
        -in $site_8_0 -x 302 -y 79 -width 54 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab72 \
        -in $site_8_0 -x 20 -y 99 -width 29 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent73 \
        -in $site_8_0 -x 70 -y 99 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent79 \
        -in $site_8_0 -x 180 -y 99 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent80 \
        -in $site_8_0 -x 290 -y 99 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab91 \
        -in $site_8_0 -x 20 -y 126 -width 34 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent94 \
        -in $site_8_0 -x 70 -y 126 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent98 \
        -in $site_8_0 -x 180 -y 126 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent81 \
        -in $site_8_0 -x 290 -y 126 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab82 \
        -in $site_8_0 -x 20 -y 153 -width 33 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.ent83 \
        -in $site_8_0 -x 70 -y 153 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.cpd71 \
        -in $site_8_0 -x 180 -y 153 -width 84 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_8_0.cpd72 \
        -in $site_8_0 -x 290 -y 153 -width 80 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab71 \
        -in $site_8_0 -x 7 -y 188 -width 372 -height 96 -anchor nw \
        -bordermode ignore 
    place $site_8_0.lab77 \
        -in $site_8_0 -x 7 -y 286 -width 372 -height 184 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but75 \
        -in $site_8_0 -x 387 -y 241 -width 211 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but74 \
        -in $site_8_0 -x 387 -y 273 -width 211 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.fra106 \
        -in $site_8_0 -x 388 -y 309 -width 210 -height 153 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but71 \
        -in $site_8_0 -x 387 -y 41 -width 211 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but72 \
        -in $site_8_0 -x 387 -y 73 -width 211 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but79 \
        -in $site_8_0 -x 387 -y 105 -width 211 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but73 \
        -in $site_8_0 -x 387 -y 140 -width 100 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but77 \
        -in $site_8_0 -x 387 -y 172 -width 100 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.but78 \
        -in $site_8_0 -x 387 -y 204 -width 100 -height 28 -anchor nw \
        -bordermode ignore 
    place $site_8_0.che71 \
        -in $site_8_0 -x 496 -y 145 -width 100 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.che72 \
        -in $site_8_0 -x 496 -y 177 -width 100 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_8_0.che73 \
        -in $site_8_0 -x 496 -y 209 -width 100 -height 22 -anchor nw \
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
