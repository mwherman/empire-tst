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
## vTcl Code to Load Stock Images


if {![info exist vTcl(sourcing)]} {
#############################################################################
## Procedure:  vTcl:rename

proc ::vTcl:rename {name} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    regsub -all "\\." $name "_" ret
    regsub -all "\\-" $ret "_" ret
    regsub -all " " $ret "_" ret
    regsub -all "/" $ret "__" ret
    regsub -all "::" $ret "__" ret

    return [string tolower $ret]
}

#############################################################################
## Procedure:  vTcl:image:create_new_image

proc ::vTcl:image:create_new_image {filename {description {no description}} {type {}} {data {}}} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    # Does the image already exist?
    if {[info exists ::vTcl(images,files)]} {
        if {[lsearch -exact $::vTcl(images,files) $filename] > -1} { return }
    }

    if {![info exists ::vTcl(sourcing)] && [string length $data] > 0} {
        set object [image create  [vTcl:image:get_creation_type $filename]  -data $data]
    } else {
        # Wait a minute... Does the file actually exist?
        if {! [file exists $filename] } {
            # Try current directory
            set script [file dirname [info script]]
            set filename [file join $script [file tail $filename] ]
        }

        if {![file exists $filename]} {
            set description "file not found!"
            ## will add 'broken image' again when img is fixed, for now create empty
            set object [image create photo -width 1 -height 1]
        } else {
            set object [image create  [vTcl:image:get_creation_type $filename]  -file $filename]
        }
    }

    set reference [vTcl:rename $filename]
    set ::vTcl(images,$reference,image)       $object
    set ::vTcl(images,$reference,description) $description
    set ::vTcl(images,$reference,type)        $type
    set ::vTcl(images,filename,$object)       $filename

    lappend ::vTcl(images,files) $filename
    lappend ::vTcl(images,$type) $object

    # return image name in case caller might want it
    return $object
}

#############################################################################
## Procedure:  vTcl:image:get_image

proc ::vTcl:image:get_image {filename} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    set reference [vTcl:rename $filename]

    # Let's do some checking first
    if {![info exists ::vTcl(images,$reference,image)]} {
        # Well, the path may be wrong; in that case check
        # only the filename instead, without the path.

        set imageTail [file tail $filename]

        foreach oneFile $::vTcl(images,files) {
            if {[file tail $oneFile] == $imageTail} {
                set reference [vTcl:rename $oneFile]
                break
            }
        }
    }
    return $::vTcl(images,$reference,image)
}

#############################################################################
## Procedure:  vTcl:image:get_creation_type

proc ::vTcl:image:get_creation_type {filename} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    switch [string tolower [file extension $filename]] {
        .ppm -
        .jpg -
        .bmp -
        .gif    {return photo}
        .xbm    {return bitmap}
        default {return photo}
    }
}

foreach img {

        {{[file join / usr local vtcl images edit open.gif]} {} stock {
R0lGODlhFAAUAPcAAAAAAIAAAACAAICAAAAAgIAAgACAgMDAwMDcwKbK8AAA
AAAAKgAAVQAAfwAAqgAA1AAqAAAqKgAqVQAqfwAqqgAq1ABVAABVKgBVVQBV
fwBVqgBV1AB/AAB/KgB/VQB/fwB/qgB/1ACqAACqKgCqVQCqfwCqqgCq1ADU
AADUKgDUVQDUfwDUqgDU1CoAACoAKioAVSoAfyoAqioA1CoqACoqKioqVSoq
fyoqqioq1CpVACpVKipVVSpVfypVqipV1Cp/ACp/Kip/VSp/fyp/qip/1Cqq
ACqqKiqqVSqqfyqqqiqq1CrUACrUKirUVSrUfyrUqirU1FUAAFUAKlUAVVUA
f1UAqlUA1FUqAFUqKlUqVVUqf1UqqlUq1FVVAFVVKlVVVVVVf1VVqlVV1FV/
AFV/KlV/VVV/f1V/qlV/1FWqAFWqKlWqVVWqf1WqqlWq1FXUAFXUKlXUVVXU
f1XUqlXU1H8AAH8AKn8AVX8Af38Aqn8A1H8qAH8qKn8qVX8qf38qqn8q1H9V
AH9VKn9VVX9Vf39Vqn9V1H9/AH9/Kn9/VX9/f39/qn9/1H+qAH+qKn+qVX+q
f3+qqn+q1H/UAH/UKn/UVX/Uf3/Uqn/U1KoAAKoAKqoAVaoAf6oAqqoA1Koq
AKoqKqoqVaoqf6oqqqoq1KpVAKpVKqpVVapVf6pVqqpV1Kp/AKp/Kqp/Vap/
f6p/qqp/1KqqAKqqKqqqVaqqf6qqqqqq1KrUAKrUKqrUVarUf6rUqqrU1NQA
ANQAKtQAVdQAf9QAqtQA1NQqANQqKtQqVdQqf9QqqtQq1NRVANRVKtRVVdRV
f9RVqtRV1NR/ANR/KtR/VdR/f9R/qtR/1NSqANSqKtSqVdSqf9SqqtSq1NTU
ANTUKtTUVdTUf9TUqtTU1AAAAAwMDBkZGSYmJjMzMz8/P0xMTFlZWWZmZnJy
cn9/f4yMjJmZmaWlpbKysr+/v8zMzNjY2OXl5fLy8v/78KCgpICAgP8AAAD/
AP//AAAA//8A/wD//////yH5BAEAAPMALAAAAAAUABQAAAhiAOcJHEiwoMGD
CBMqRIivIb6FBPH9m/jPoUWGFDNmxAfAoMSKFkM27DhQIj5uKFOq5MYxYsWV
MFuWfAlTpUyBDWvaJFnypE6UN3H6/Bl0nsijAHgOTMq0aVOIUKNKnUqVakAA
Ow==}}

            } {
    eval set _file [lindex $img 0]
    vTcl:image:create_new_image\
        $_file [lindex $img 1] [lindex $img 2] [lindex $img 3]
}

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
    set base .topwindow
    namespace eval ::widgets::$base {
        set set,origin 1
        set set,size 1
        set runvisible 1
    }
    namespace eval ::widgets::$base.title {
        array set save {-background 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$base.project-lab {
        array set save {-background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$base.project {
        array set save {-background 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.mtframe {
        array set save {-background 1 -borderwidth 1 -foreground 1 -labeltext 1 -relief 1}
    }
    set site_3_0 $base.mtframe
    namespace eval ::widgets::$site_3_0.chkbtot {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbel {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbinel {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbn2n {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbn3n {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbnna {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbnnp {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbnnpa {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbng {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbnp {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbna {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.chkbnpa {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -justify 1 -offvalue 1 -onvalue 1 -text 1 -variable 1}
    }
    namespace eval ::widgets::$site_3_0.selectmt {
        array set save {-background 1 -padx 1 -text 1}
    }
    namespace eval ::widgets::$base.show {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.merge {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.exit {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.suf-lab {
        array set save {-background 1 -foreground 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$base.sufix {
        array set save {-background 1 -highlightbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.dir-lab {
        array set save {-background 1 -foreground 1 -pady 1 -text 1}
    }
    namespace eval ::widgets::$base.creatszvd {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -pady 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.creatazvd {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.showall {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.sep-lab {
        array set save {-background 1 -text 1}
    }
    namespace eval ::widgets::$base.curv1-lab {
        array set save {-activebackground 1 -anchor 1 -background 1 -foreground 1 -justify 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.label53 {
        array set save {-anchor 1 -background 1 -foreground 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.label54 {
        array set save {-anchor 1 -background 1 -foreground 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$base.curv1-ent {
        array set save {-background 1 -foreground 1 -selectbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.entry55 {
        array set save {-background 1 -foreground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.entry56 {
        array set save {-background 1 -foreground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.ent67 {
        array set save {-background 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.cpd68 {
        array set save {-background 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.cpd69 {
        array set save {-background 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$base.but70 {
        array set save {-activeforeground 1 -command 1 -disabledforeground 1 -font 1 -image 1 -text 1}
    }
    namespace eval ::widgets::$base.cpd71 {
        array set save {-activeforeground 1 -command 1 -disabledforeground 1 -font 1 -image 1 -text 1}
    }
    namespace eval ::widgets::$base.cpd72 {
        array set save {-activeforeground 1 -command 1 -disabledforeground 1 -font 1 -image 1 -text 1}
    }
    namespace eval ::widgets::$base.m67 {
        array set save {-disabledforeground 1 -tearoff 1}
    }
    namespace eval ::widgets::$base.__tk_filedialog {
    }
    set site_4_0 $base.__tk_filedialog.bot
    set site_4_0 $base.__tk_filedialog.top
    set site_5_0 $site_4_0.f1
    set site_5_0 $site_4_0.f3
    set site_5_0 $site_4_0.f2
    set site_6_0 $site_5_0.a
    set site_6_0 $site_5_0.b
    namespace eval ::widgets_bindings {
        set tagslist _TopLevel
    }
    namespace eval ::vTcl::modules::main {
        set procs {
            init
            main
            vTclWindow.
            vTclWindow.topwindow
            fileDialog
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
wm protocol .topwindow WM_DELETE_WINDOW {exit}
}
#############################################################################
## Procedure:  fileDialog

proc ::fileDialog {w} {
global widget file

    #   Type names		Extension(s)	Mac File Type(s)
    #
    #---------------------------------------------------------
    set types {
	{"ENDF Files"		{.endf}		}
	{"All   Files"		{*}		}
    }
     
set file [tk_getOpenFile -filetypes $types  -parent $w -title "Select ENDF file"]
#set dfile [file rootname $defile]
#set file [file tail $dfile]
#set zvfilter $file
#set profilter $file
#set archfilter $file 
}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {
global root suf name1 dir1 name2 dir2 name3 dir3
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
    wm maxsize $top 1265 994
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

proc vTclWindow.topwindow {base} {
    if {$base == ""} {
        set base .topwindow
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $top -class Toplevel \
        -background #dcdcdc -highlightbackground #dcdcdc \
        -highlightcolor #000000 -menu "$top.m67" 
    wm focusmodel $top passive
    wm geometry $top 331x679+122+252; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Show ZVV plots"
    bindtags $top "$top Toplevel all _TopLevel"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    label $top.title \
        -background #dcdcdc -disabledforeground #a3a3a3 \
        -font {Helvetica -18 bold} -foreground black \
        -highlightbackground #dcdcdc -highlightcolor #000000 -pady 1 \
        -text {Create/Show ZVV Plots} 
    label $top.project-lab \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #000000 -pady 1 -text {Project root name:} 
    entry $top.project \
        -background white -textvariable root -width 17 
    frame $top.mtframe \
        -background #dcdcdc -borderwidth 1 -relief groove 
    set site_3_0 $top.mtframe
    checkbutton $site_3_0.chkbtot \
        -activebackground #609498 -activeforeground white -background #dcdcdc \
        -foreground black -highlightbackground #dcdcdc -highlightcolor white \
        -justify left -offvalue {} -text {  1 total} -variable tot 
    checkbutton $site_3_0.chkbel \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 2 \
        -text {  2 elastic} -variable el 
    checkbutton $site_3_0.chkbinel \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 4 \
        -text {  4 inelastic} -variable inel 
    checkbutton $site_3_0.chkbn2n \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 16 \
        -text { 16 (x,2n)} -variable n2n 
    checkbutton $site_3_0.chkbn3n \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 17 \
        -text { 17 (x,3n)} -variable n3n 
    checkbutton $site_3_0.chkbnfis \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 18 \
        -text { 18 (x,f)} -variable nf 
    checkbutton $site_3_0.chkbnna \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 22 \
        -text { 22 (x,na)} -variable nna 
    checkbutton $site_3_0.chkbnnp \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 28 \
        -text { 28 (x,np)} -variable nnp 
    checkbutton $site_3_0.chkbnnpa \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -command {} -foreground black \
        -highlightbackground #dcdcdc -highlightcolor #ffffff -justify left \
        -offvalue {} -onvalue 45 -text { 45 (x,npa)} -variable nnpa 
    checkbutton $site_3_0.chkbng \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 102 \
        -text {102 (x,g)} -variable ng 
    checkbutton $site_3_0.chkbnp \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 103 \
        -text {103 (x,p)} -variable np 
    checkbutton $site_3_0.chkbna \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 107 \
        -text {107 (x,a)} -variable na 
    checkbutton $site_3_0.chkbnpa \
        -activebackground #609498 -activeforeground #ffffff \
        -background #dcdcdc -foreground black -highlightbackground #dcdcdc \
        -highlightcolor #ffffff -justify left -offvalue {} -onvalue 112 \
        -text {112 (x,pa)} -variable npa 
    label $site_3_0.selectmt \
        -background #dcdcdc -padx 1 -text {Select MT:} 
    grid $site_3_0.chkbtot \
        -in $site_3_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbel \
        -in $site_3_0 -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbinel \
        -in $site_3_0 -column 0 -row 3 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbn2n \
        -in $site_3_0 -column 0 -row 4 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbn3n \
        -in $site_3_0 -column 0 -row 5 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbnfis \
        -in $site_3_0 -column 0 -row 6 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbnna \
        -in $site_3_0 -column 0 -row 7 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbnnp \
        -in $site_3_0 -column 0 -row 8 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbnnpa \
        -in $site_3_0 -column 0 -row 9 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbng \
        -in $site_3_0 -column 0 -row 10 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbnp \
        -in $site_3_0 -column 0 -row 11 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbna \
        -in $site_3_0 -column 0 -row 12 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.chkbnpa \
        -in $site_3_0 -column 0 -row 13 -columnspan 1 -rowspan 1 -sticky w 
    grid $site_3_0.selectmt \
        -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 
    button $top.show \
        -activebackground #cccccc -activeforeground #00ff00 \
        -background #dcdcdc \
        -command {set blank " "
set zvd ""
set suff ""
if {$suf != ""} {set suff -$suf}
set mts [list $tot $el $inel $n2n $n3n $nf $nna $nnp $nnpa $ng $np $na $npa]
foreach i $mts {
    if {$i != ""} {lappend  zvd $root-$i$suff.zvd}
}
exec xterm -e ./showzvd $zvd} \
        -font {Helvetica -12} -foreground #009900 \
        -highlightbackground #dcdcdc -highlightcolor white \
        -text {Show selected plots} -width 15 
    button $top.merge \
        -activebackground #cccccc -activeforeground #00ff00 \
        -background #dcdcdc \
        -command {set blank " "
set zvd ""
set suff ""
if {$suf != ""} {set suff -$suf}
set mts [list $tot $el $inel $n2n $n3n $nf $nna $nnp $nnpa $ng $np $na $npa]
foreach i $mts {
    if {$i != ""} {lappend  zvd $root-$i$suff.zvd}
}
exec xterm -e ./zvv $zvd &
#exec showzvd $zvd} \
        -font {Helvetica -12} -foreground #009900 \
        -highlightbackground #dcdcdc -highlightcolor white \
        -text {Merge selected plots} -width 15 
    button $top.exit \
        -activebackground #f00000 -activeforeground #ffffff \
        -background #dcdcdc \
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
        -font {Helvetica -12} -foreground #ff0000 -highlightcolor black \
        -text Exit -width 0 
    label $top.suf-lab \
        -background #dcdcdc -foreground black -pady 1 \
        -text {Suffix for plots:} 
    entry $top.sufix \
        -background white -highlightbackground white -textvariable suf \
        -width 17 
    label $top.dir-lab \
        -background #dcdcdc -foreground black -pady 1 \
        -text {Project directory and other ENDF files:} 
    button $top.creatszvd \
        -activebackground #cccccc -activeforeground #f709896d3a0e \
        -background #dcdcdc \
        -command {set suff -$suf
set mts [list $tot $el $inel $n2n $n3n $nf $nna $nnp $nnpa $ng $np $na $npa]
foreach i $mts {
if {$i != ""} {exec xterm -e ./mtacomp $i $suff $root $dir1 $name1 $dir2 $name2 $dir3 $name3}
}} \
        -font {Helvetica -12} -foreground #c7ad87623be7 \
        -highlightbackground #dcdcdc -highlightcolor white -pady 1m \
        -text {Create selected} -width 15 
    button $top.creatazvd \
        -activebackground #cccccc -activeforeground #f709896d3a0e \
        -background #dcdcdc \
        -command set\ suff\ -\$suf\nexec\ xterm\ -e\ acomp\ \$suff\ \$root\ \$dir1\ \$name1\ \$dir2\ \$name2\ \$dir3\ \$name3\} \
        -font {Helvetica -12} -foreground #c7ad87623be7 \
        -highlightbackground #dcdcdc -highlightcolor white -text {Create all} \
        -width 15 
    button $top.showall \
        -activebackground #cccccc -activeforeground #00ff00 \
        -background #dcdcdc \
        -command {set zvd ""
set suff ""
if {$suf != ""} {set suff -$suf}
set mts [list 1 2 4 16 17 18 22 28 45 102 103 107 112]
foreach i $mts {
    if {$i != ""} {lappend  zvd $root-$i$suff.zvd}
}
exec xterm -e ./showzvd $zvd} \
        -font {Helvetica -12} -foreground #009900 \
        -highlightbackground #dcdcdc -highlightcolor white -text {Show all} \
        -width 15 
    label $top.sep-lab \
        -background #dcdcdc -text {  } 
    label $top.curv1-lab \
        -activebackground #dcdcdc -anchor w -background #dcdcdc \
        -foreground black -justify left -text {Label for curve 1:} -width 17 
    label $top.label53 \
        -anchor w -background #dcdcdc -foreground black \
        -text {Label for curve 2:} -width 17 
    label $top.label54 \
        -anchor w -background #dcdcdc -foreground black \
        -text {Label for curve 3:} -width 17 
    entry $top.curv1-ent \
        -background #ffffff -foreground darkgreen -selectbackground #cccccc \
        -textvariable name1 -width 17 
    entry $top.entry55 \
        -background #ffffff -foreground darkred -textvariable name2 -width 17 
    entry $top.entry56 \
        -background #ffffff -foreground darkblue -textvariable name3 \
        -width 17 
    ::iwidgets::entryfield $top.ent67 \
        -background #dcdcdc -labeltext {File 1:} -textbackground #ffffff \
        -textvariable dir1 -width 29 
    vTcl:DefineAlias "$top.ent67" "Entryfield1" vTcl:WidgetProc "$top" 1
    ::iwidgets::entryfield $top.cpd68 \
        -background #dcdcdc -labeltext {File 2:} -textbackground #ffffff \
        -textvariable dir2 -width 29 
    vTcl:DefineAlias "$top.cpd68" "Entryfield2" vTcl:WidgetProc "$top" 1
    ::iwidgets::entryfield $top.cpd69 \
        -background #dcdcdc -labeltext {File 3:} -textbackground #ffffff \
        -textvariable dir3 -width 29 
    vTcl:DefineAlias "$top.cpd69" "Entryfield3" vTcl:WidgetProc "$top" 1
    button $top.but70 \
        -activeforeground #666666 \
        -command {fileDialog .topwindow 
set dir1 [file dirname $file]} \
        -disabledforeground #a1a1a1 -font {Helvetica -12} \
        -image [vTcl:image:get_image [file join / usr local vtcl images edit open.gif]] \
        -text button 
    vTcl:DefineAlias "$top.but70" "Button1" vTcl:WidgetProc "$top" 1
    button $top.cpd71 \
        -activeforeground #666666 \
        -command {fileDialog .topwindow 
set dir2 $file} \
        -disabledforeground #a1a1a1 -font {Helvetica -12} \
        -image [vTcl:image:get_image [file join / usr local vtcl images edit open.gif]] \
        -text button 
    vTcl:DefineAlias "$top.cpd71" "Button2" vTcl:WidgetProc "$top" 1
    button $top.cpd72 \
        -activeforeground #666666 \
        -command {fileDialog .topwindow 
set dir3 $file} \
        -disabledforeground #a1a1a1 -font {Helvetica -12} \
        -image [vTcl:image:get_image [file join / usr local vtcl images edit open.gif]] \
        -text button 
    vTcl:DefineAlias "$top.cpd72" "Button3" vTcl:WidgetProc "$top" 1
    menu $top.m67 \
        -disabledforeground #a1a1a1 -tearoff 1 
    ###################
    # SETTING GEOMETRY
    ###################
    grid $top.title \
        -in $top -column 0 -row 0 -columnspan 2 -rowspan 1 -pady 10 
    grid $top.project-lab \
        -in $top -column 0 -row 1 -columnspan 1 -rowspan 1 -pady 5 -sticky w 
    grid $top.project \
        -in $top -column 1 -row 1 -columnspan 1 -rowspan 1 -pady 5 -sticky w 
    grid $top.mtframe \
        -in $top -column 0 -row 12 -columnspan 1 -rowspan 6 -ipadx 10 \
        -ipady 20 -padx 5 -sticky w 
    grid $top.show \
        -in $top -column 1 -row 14 -columnspan 1 -rowspan 1 -sticky s 
    grid $top.merge \
        -in $top -column 1 -row 16 -columnspan 1 -rowspan 1 
    grid $top.exit \
        -in $top -column 1 -row 17 -columnspan 1 -rowspan 1 
    grid $top.suf-lab \
        -in $top -column 0 -row 3 -columnspan 1 -rowspan 1 -sticky w 
    grid $top.sufix \
        -in $top -column 1 -row 3 -columnspan 1 -rowspan 1 -sticky w 
    grid $top.dir-lab \
        -in $top -column 0 -row 4 -columnspan 2 -rowspan 1 -pady 10 -sticky w 
    grid $top.creatszvd \
        -in $top -column 1 -row 12 -columnspan 1 -rowspan 1 -sticky s 
    grid $top.creatazvd \
        -in $top -column 1 -row 13 -columnspan 1 -rowspan 1 -sticky n 
    grid $top.showall \
        -in $top -column 1 -row 15 -columnspan 1 -rowspan 1 -sticky n 
    grid $top.sep-lab \
        -in $top -column 0 -row 11 -columnspan 2 -rowspan 1 
    grid $top.curv1-lab \
        -in $top -column 0 -row 6 -columnspan 1 -rowspan 1 -sticky e 
    grid $top.label53 \
        -in $top -column 0 -row 8 -columnspan 1 -rowspan 1 -sticky e 
    grid $top.label54 \
        -in $top -column 0 -row 10 -columnspan 1 -rowspan 1 -sticky e 
    grid $top.curv1-ent \
        -in $top -column 1 -row 6 -columnspan 1 -rowspan 1 
    grid $top.entry55 \
        -in $top -column 1 -row 8 -columnspan 1 -rowspan 1 
    grid $top.entry56 \
        -in $top -column 1 -row 10 -columnspan 1 -rowspan 1 
    grid $top.ent67 \
        -in $top -column 0 -row 5 -columnspan 2 -rowspan 1 
    grid $top.cpd68 \
        -in $top -column 0 -row 7 -columnspan 2 -rowspan 1 
    grid $top.cpd69 \
        -in $top -column 0 -row 9 -columnspan 2 -rowspan 1 
    grid $top.but70 \
        -in $top -column 2 -row 5 -columnspan 1 -rowspan 1 
    grid $top.cpd71 \
        -in $top -column 2 -row 7 -columnspan 1 -rowspan 1 
    grid $top.cpd72 \
        -in $top -column 2 -row 9 -columnspan 1 -rowspan 1 

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
Window show .topwindow

main $argc $argv
