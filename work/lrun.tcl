#!/usr/bin/wish
#############################################################################
# Visual Tcl v1.20 Project
#

#################################
# GLOBAL VARIABLES
#
global file; 
global selectedButton; 
global widget; 

#################################
# USER DEFINED PROCEDURES
#
proc init {argc argv} {

}

init $argc $argv


proc {main} {argc argv} {

}

proc {Window} {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set newname [lindex $args 2]
    set rest [lrange $args 3 end]
    if {$name == "" || $cmd == ""} {return}
    if {$newname == ""} {
        set newname $name
    }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists == "1" && $name != "."} {wm deiconify $name; return}
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
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt.tcl"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.srun {base} {
    if {$base == ""} {
        set base .srun
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -background #4b7b82 -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -menu .srun.m17 
    wm focusmodel $base passive
    wm geometry $base 211x947+8+68
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "Empire control"
    canvas $base.input \
        -background #4b7b82 -borderwidth 2 -confine 0 -height 207 \
        -highlightbackground #304d49 -highlightcolor #ffffff -relief groove \
        -selectbackground #365e66 -width 296 
    entry $base.input.project \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--16-120-75-75-p-67-iso8859-1 \
        -foreground #fefefe -highlightbackground #374d4e \
        -highlightcolor #ffffff -justify right -selectbackground #41525c \
        -selectforeground #ffffff -textvariable file 
    button $base.input.but31 \
        -activebackground #283cb4 -activeforeground #ffffff \
        -background #283e76 \
        -command {file copy skel.inp $file.inp
exec $editor $file.inp &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Create } 
    button $base.input.but18 \
        -activebackground #283cb4 -activeforeground #fffefe \
        -background #283e76 -command {exec $editor $file.inp &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Edit } 
    button $base.input.but17 \
        -activebackground #df5130 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e run $file &} \
        -font -adobe-helvetica-medium-r-bold--14-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Run+Format+Plot 
    button $base.input.but36 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e runE $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Run 
    button $base.input.but19 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e format $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Format 
    button $base.input.but20 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e plot $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Plot 
    button $base.input.viewlong \
        -activebackground #007900 -activeforeground #fefefe \
        -background #005a00 -command {exec $editor $file.lst &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Emp-full 
    button $base.input.but27 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.out  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Emp-short } 
    button $base.input.but22 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.exf &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text EXFOR 
    button $base.input.but23 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.c4 &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {C4 file} 
    button $base.input.but28 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.endf  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text ENDF 
    button $base.input.but25 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec gv $file.ps &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Plotc4 
    button $base.input.but21 \
        -activebackground #f00000 -activeforeground #fffefe \
        -background #860000 -command {exec clean $file core ../util/*/core &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Clean 
    label $base.input.titel \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text {EMPIRE } 
    label $base.input.source \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #fefefe -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text {Modify source} 
    button $base.input.but26 \
        -activebackground #f00000 -activeforeground #fefefe \
        -background #860000 -command {exec $editor ../source/dimension.h &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Edit dimensions} 
    menubutton $base.input.modulist \
        -activebackground #f00000 -activeforeground #ffffff \
        -background #860000 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -indicatoron 1 -menu .srun.input.modulist.m \
        -padx 4 -pady 3 -relief raised -text {Edit module} 
    menu $base.input.modulist.m \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -tearoff 0 
    menubutton $base.input.sourcemenu \
        -activebackground #f00000 -activeforeground #ffffff \
        -background #860000 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -indicatoron 1 -menu .srun.input.sourcemenu.m \
        -padx 4 -pady 3 -relief raised -text {Edit  module} 
    menu $base.input.sourcemenu.m \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/main.f &} -label main 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/input.f &} -label input 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/OM-scat2.f &} -label OM-scat2 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/MSD-orion.f &} -label MSD-orion 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/MSD-tristan.f &} -label MSD-tristan 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/MSC-NVWY.f &} -label MSC-NVWY 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/HF-comp.f &} -label HF-comp 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/HRTW-comp.f &} -label HRTW-comp 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/ddhms.f &} -label hms 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/degas.f &} -label degas 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/ecis.f &} -label ECIS 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/ccfus.f &} -label ccfus 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/fusion.f &} -label fusion 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/gamma-strgth.f &} \
        -label gamma-strgth 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/lev-dens.f &} -label lev-dens 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/ph-lev-dens.f &} -label ph-lev-dens 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/print.f &} -label print 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/tl.f &} -label tl 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/auxiliary.f &} -label auxiliary 
    $base.input.sourcemenu.m add command \
        -command {exec $editor ../source/scnd-preeq.f &} -label scnd-preeq 
    button $base.input.but30 \
        -activebackground #df5130 -activeforeground #fffefe \
        -background #be5a41 \
        -command {cd ../source
exec xterm -e make &
cd ../work} \
        -font -adobe-helvetica-medium-r-bold--14-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Make 
    label $base.input.input \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text Input: 
    label $base.input.lab34 \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text Execute: 
    label $base.input.lab35 \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-normal--16-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text {or :} 
    label $base.input.lab37 \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text Outputs: 
    button $base.input.but29 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.int  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Int. omp} 
    button $base.input.browse \
        -activebackground #8aa6a6 -activeforeground #ffffff \
        -background #4b7b82 \
        -command {set defile  [exec xgetfile -title "select input file" -pattern *.inp ] 
set dfile [file rootname $defile]
set file [file tail $dfile]} \
        -font -adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Project: 
    menubutton $base.input.men19 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -menu .srun.input.men19.m -padx 4 -pady 3 \
        -relief raised -text {Select MT and >} 
    menu $base.input.men19.m \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff 
    $base.input.men19.m add radiobutton \
        -value {MT=1 (total)} -command {set MT {1}} -label {MT=1 (total)} 
    $base.input.men19.m add radiobutton \
        -value {MT=2 (elas)} -command {set MT {2}} -label {MT=2 (elas)} 
    $base.input.men19.m add radiobutton \
        -value {MT=4 (n,n')} -command {set MT {4}} -label {MT=4 (n,n')} 
    $base.input.men19.m add radiobutton \
        -value {MT=16 (n,2n)} -command {set MT {16}} -label {MT=16 (n,2n)} 
    $base.input.men19.m add radiobutton \
        -value {MT=17 (n,3n)} -command {set MT {17}} -label {MT=17 (n,3n)} 
    $base.input.men19.m add radiobutton \
        -value {MT=18 (n,f)} -command {set MT {18}} -label {MT=18 (n,f)} 
    $base.input.men19.m add radiobutton \
        -value {MT=22 (n,na)} -command {set MT {22}} -label {MT=22 (n,na)} 
    $base.input.men19.m add radiobutton \
        -value {MT=28 (n,np)} -command {set MT {28}} -label {MT=28 (n,np)} 
    $base.input.men19.m add radiobutton \
        -value {MT=45 (n,npa)} -command {set MT {45}} -label {MT=45 (n,npa)} 
    $base.input.men19.m add radiobutton \
        -value {MT=102 (n,g)} -command {set MT {102}} -label {MT=102 (n,g)} 
    $base.input.men19.m add radiobutton \
        -value {MT=103 (n,p)} -command {set MT {103}} -label {MT=103 (n,p)} 
    $base.input.men19.m add radiobutton \
        -value {MT=107 (n,a)} -command {set MT {107}} -label {MT=107 (n,a)} 
    $base.input.men19.m add radiobutton \
        -value {MT=112 (n,pa)} -command {set MT {112}} -label {MT=112 (n,pa)} 
    button $base.input.but33 \
        -activebackground #dc5032 -activeforeground #ffffff \
        -background #be5a41 -command {exec xterm -e zvd $file $MT &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text ZVV 
    button $base.input.but32 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-ecis.out  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Ecis 
    label $base.input.lab33 \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text Out/In: 
    button $base.input.but34 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-lev.col  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Coll. levels} 
    button $base.input.but35 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file.lev  &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text Levels 
    button $base.input.but24 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.ripl &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {RIPL omp} 
    button $base.input.but37 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-ecis.in &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {ECIS inp} 
    button $base.input.but38 \
        -activebackground #007900 -activeforeground #ffffff \
        -background #005a00 -command {exec $editor $file-omp.dir &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Dir. omp} 
    label $base.input.lab17 \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text Plots: 
    label $base.input.lab18 \
        -background #4b7b82 -borderwidth 1 \
        -font -adobe-helvetica-medium-r-bold--18-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -text Outputs: 
    button $base.input.but39 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e zvpl $file &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Create ZVV} 
    button $base.input.but40 \
        -activebackground #dc5032 -activeforeground #fffefe \
        -background #be5a41 -command {exec xterm -e zvcomb &} \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -padx 9 -pady 3 -text {Merge ZVV} 
    frame $base.cpd32 \
        -background #4b7b82 -borderwidth 1 -height 30 \
        -highlightbackground #4b7b82 -highlightcolor #ffffff -width 30 
    menubutton $base.cpd32.05 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -height 1 -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -menu .srun.cpd32.05.06 -padx 4 -pady 3 \
        -text Help 
    menu $base.cpd32.05.06 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -tearoff 0 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../hints.txt &} -label {Hints} 
    $base.cpd32.05.06 add command \
        -command {exec $editor inplist &} -label {EMPIRE input} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../data/ripl-omp.index &} -label RIPL-omp 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/empend/manual.txt &} -label {EMPEND manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/c4sort/manual.txt &} -label {C4SORT manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/legend/manual.txt &} -label {LEGEND manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/plotc4/manual.txt &} -label {PLOTC4 manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/x4toc4/manual.txt &} -label {X4TOC4 manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/fixup/manual.txt &}  -label {FIXUP manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/lsttab/manual.txt &} -label {LSTTAB manual} 
    $base.cpd32.05.06 add command \
        -command {exec $editor ../util/sixtab/manual.txt &} -label {SIXTAB manual} 
    menubutton $base.cpd32.01 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -height 1 -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -menu .srun.cpd32.01.02 -padx 4 -pady 3 \
        -text File 
    menu $base.cpd32.01.02 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -tearoff 0 
    $base.cpd32.01.02 add command \
        \
        -command {set ifile [open listpro w+]
set inp_files [glob -nocomplain *.inp]

foreach name $inp_files {
  puts  $ifile [ format "%-15s " $name ]
  }
  
close $ifile  
exec $editor listpro &} \
        -label {List projects} 
    $base.cpd32.01.02 add command \
        -command {exec rm  $file\.* $file-*\.*} -label {Remove project} 
    $base.cpd32.01.02 add command \
        -command exit -label Exit 
    menubutton $base.cpd32.men40 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -height 1 -highlightbackground #4b7b82 \
        -highlightcolor #ffffff -menu .srun.cpd32.men40.01 -padx 4 -pady 3 \
        -text Options -width 6 
    menu $base.cpd32.men40.01 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -tearoff 0 
    $base.cpd32.men40.01 add command \
        -command {exec $editor skel.inp &} -label {Default input} 
    $base.cpd32.men40.01 add command \
        -command {exec $editor EMPEND.INP &} -label {EMPEND input} 
    $base.cpd32.men40.01 add command \
        -command {exec $editor ../util/plotc4/PLOTC4.INP &} \
        -label {PLOTC4 input} 
    $base.cpd32.men40.01 add command \
        -command {exec $editor ../util/fixup/FIXUP.INP &} \
        -label {FIXUP input} 
    $base.cpd32.men40.01 add command \
        -command {exec $editor ../util/c4sort/C4SORT.INP &} \
        -label {C4SORT input} 
    $base.cpd32.men40.01 add command \
        -command {exec $editor ../source/Makefile &} \
        -label {Edit Makefile} 
    $base.cpd32.men40.01 add cascade \
        -menu .srun.cpd32.men40.01.men41 -label {Select editor} 
    menu $base.cpd32.men40.01.02 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -tearoff 0 
    $base.cpd32.men40.01.02 add radiobutton \
        -value gvim -command {set editor {gvim}} -label gvim 
    $base.cpd32.men40.01.02 add radiobutton \
        -value emacs -command {set editor {emacs}} -label emacs 
    $base.cpd32.men40.01.02 add radiobutton \
        -value jove -command {set editor {jove}} -label jove 
    $base.cpd32.men40.01.02 add radiobutton \
        -value nedit -command {set editor nedit} -label nedit 
    $base.cpd32.men40.01.02 add radiobutton \
        -value kedit -command {set editor {kedit}} -label kedit 
    $base.cpd32.men40.01.02 add radiobutton \
        -value GXedit -command {set editor {gxedit}} -label GXedit 
    menu $base.cpd32.men40.01.men41 \
        -activebackground #4b7b82 -activeforeground #ffffff \
        -background #4b7b82 \
        -font -adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1 \
        -foreground #ffffff -tearoff 0 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value gvim -command {set editor {gvim}} -label gvim -state active 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value emacs -command {set editor {emacs}} -label emacs 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value nedit -command {set editor {nedit}} -label nedit 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value kedit -command {set editor {kedit}} -label kedit 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value jove -command {set editor {jove}} -label jove 
    $base.cpd32.men40.01.men41 add radiobutton \
        -value GXedit -command {set editor {gxedit}} -label GXedit 
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
        -x 20 -y 440 -width 146 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but27 \
        -x 20 -y 465 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but22 \
        -x 93 -y 465 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but23 \
        -x 20 -y 300 -width 146 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but28 \
        -x 93 -y 490 -width 73 -height 26 -anchor nw -bordermode ignore 
    place $base.input.but25 \
        -x 20 -y 555 -width 146 -height 26 -anchor nw -bordermode ignore 
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
    place $base.cpd32 \
        -x 5 -y 5 -width 197 -height 26 -anchor nw 
    pack $base.cpd32.05 \
        -in .srun.cpd32 -anchor center -expand 0 -fill none -side right 
    pack $base.cpd32.01 \
        -in .srun.cpd32 -anchor nw -expand 0 -fill none -side left 
    place $base.cpd32.men40 \
        -x 36 -y 1 -width 59 -height 24 -anchor nw 
}

Window show .
Window show .srun

main $argc $argv
