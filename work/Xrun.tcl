#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {

    # Provoke name search
    catch {package require bogus-package-name}
    set packageNames [package names]

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

proc {vTcl:rename} {name} {
    regsub -all "\\." $name "_" ret
    regsub -all "\\-" $ret "_" ret
    regsub -all " " $ret "_" ret
    regsub -all "/" $ret "__" ret
    regsub -all "::" $ret "__" ret

    return [string tolower $ret]
}

#############################################################################
## Procedure:  vTcl:image:create_new_image

proc {vTcl:image:create_new_image} {filename {description {no description}} {type {}} {data {}}} {
    global vTcl env

    # Does the image already exist?
    if {[info exists vTcl(images,files)]} {
        if {[lsearch -exact $vTcl(images,files) $filename] > -1} { return }
    }

    if {![info exists vTcl(sourcing)] && [string length $data] > 0} {
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
            set object [image create photo -data [vTcl:image:broken_image] ]
        } else {
            set object [image create  [vTcl:image:get_creation_type $filename]  -file $filename]
        }
    }

    set reference [vTcl:rename $filename]

    set vTcl(images,$reference,image)       $object
    set vTcl(images,$reference,description) $description
    set vTcl(images,$reference,type)        $type
    set vTcl(images,filename,$object)       $filename

    lappend vTcl(images,files) $filename
    lappend vTcl(images,$type) $object

    # return image name in case caller might want it
    return $object
}

#############################################################################
## Procedure:  vTcl:image:get_image

proc {vTcl:image:get_image} {filename} {
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

proc {vTcl:image:get_creation_type} {filename} {
    switch [string tolower [file extension $filename]] {
        .ppm -
        .jpg -
        .bmp -
        .gif    {return photo}
        .xbm    {return bitmap}
        default {return photo}
    }
}

#############################################################################
## Procedure:  vTcl:image:broken_image

proc {vTcl:image:broken_image} {} {
    return {
        R0lGODdhFAAUAPcAAAAAAIAAAACAAICAAAAAgIAAgACAgMDAwICAgP8AAAD/
        AP//AAAA//8A/wD//////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAACwAAAAAFAAUAAAIhAAPCBxIsKDBgwgPAljIUOBC
        BAkBPJg4UeBEBBAVPkCI4EHGghIHChAwsKNHgyEPCFBA0mFDkBtVjiz4AADK
        mAds0tRJMCVBBkAl8hwYMsFPBwyE3jzQwKhAoASUwmTagCjDmksbVDWIderC
        g1174gQ71CHFigfOhrXKUGfbrwnjyp0bEAA7
    }
}

foreach img {


            } {
    eval set _file [lindex $img 0]
    vTcl:image:create_new_image\
        $_file [lindex $img 1] [lindex $img 2] [lindex $img 3]
}

}
#############################################################################
## vTcl Code to Load User Images

catch {package require Img}

foreach img {

        {{[file join / zip Projects fileopen.gif]} {file not found!} user {
R0lGODlhGAAYAOcAAAAAACYZDiccEScaDzsnFPStav7CgP7Cf/23c+WlZv7L
k/7Tmv7Ok/7Ii/a2dRUPCf7Dhf7VnP7OkvyyafmsYvisYj0oFf7IiP7Rmf7C
gu6gWYBTLEsxGUowGUUsF0ApFTwnFDcjEjIgES4dDykaDiUXDEYwG/7Kj/21
bqhvPFNQTf////f39/X19fHx8erq6uPj493d3NbW1dDQzsjIx8LCwbu7urS0
s6+vr66urjQ0NFY9Jf7Ghf7BgvKlXOfn5+Tk5OHh4dnZ2NHR0MfHxsDAv7i4
t7Cwr6enpp6enZWVlYuLiYGBgF5eXRkZGV9FKv7Cg/uyacOJVFhTTvLy8uLi
4erq6evr6t3d29PT0crKycHBwK+vraWlo5ubmo6OjXt7eTY2NXhaOv65cuqe
WG9PL4+OjNTU0+Hh4Onp6Ofn5uXl497e3dXV1M3NzMTEwrq6ubGxsJ6enIyM
i21tbGlNMf60ab6FUF1WT+Tk4+Dg3+Xl5NjY1s/PzsDAvre3tq6uraSko5eX
ligoJ1lAJ/OkW2hKLYiIiNzc29DQz8nJyLm5uLKysampqKCgnpCQj15eXBUV
FEYwGrV3QFVQTMrKytLS0c3Ny7Ozsqysq6OjopqamIGBgUQsF5GRkb29u8PD
wcfHxcjIxsbGxcPDwr+/vrm5t6WlpJ2dm42NjGdnZgsHBG1oZK2trLa2tamp
p6KioZ2dnJGRkH9/fiwsKxcXFpqampmZmJycm5+fnZqamZOTk4uLioqKiH19
fFpaWBQUExMTEzIyMisrKy0tLC0tLSoqKigoKCcnJyYmJSUlJBAQEP//////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
/////////////////////yH5BAEKAP8ALAAAAAAYABgAAAj+AP8JHEiwoMGD
BgEoVIiw4cKHDAE0HMiw4MOJ/yQiXIjRIceDAQQIGNDxIIECBg4gIFCSIIEE
ChYwaOCAwIObOG8iJAAhggQDEyhUGEqUKEuDFi5gyKBhA4cOUD18ABFCxAgS
JS7+M3HhBIoUKlawaOHiBYwYMmbQqGHjBo4cOgTu4NHDRwcaP4AEETKESBEj
R5AkUbKESRMnAp9AiSJlCpUqVq7AwJJFyxYjXLp4+QImzEAxY8iUMXMGTRo1
a9i0cfMGThwkcubQ0fivjp07ePK00bOnSgw+fWj4+QMokCAmgwgSKmTokJYz
MRAJaZNIUY1FjBo5egQpEkFJkyhxxajU54ylRJdobLGBKZOmTZw8u+zk6c8n
UKFEjSJVylT7U6ikogptAq3CCg2tYOKKKTbA4QomgLwCSyyyzEKLQbV4Ysst
uOTiISq6CLLLI7z04ssvwBgUjDDD0EJMMTASQ8swxhyDTDLKLNPSjjw2FBAA
Ow==}}
        {{[file join / zip Projects launch.gif]} {file not found!} user {
R0lGODlhGAAYAIQAAAAAAGsAAMoAAdMAAfwCAvsAEP9bN/xoLPxlNPhwMvxk
QPyZAf2ZEPyYKv2VN/ujHv+gNP39Av/CV/3KZv/Md//QZ/v+/P//////////
/////////////////////////yH5BAEBAB8ALAAAAAAYABgAAAW34CeOZDkG
aACYLIk6k6O2bWA104PMdBkgDosFUeD1RDaIREhQrY6fwKEhRBAsTlYqZYEx
BVlfwNFo7BBUi+EadikQ8J0Bxxy0T4LBQJCQNio/bABPLlsoCThSBGCDLIOP
P4lWYHcjjwAvFIFgfYQ0mBOBdkBGNAwRAQmLUqSeJgwLEbJbAq00C7GyDJiY
AzuNLbiyC5cBvqUlwrjEeMjJEcvAUc4ksbiPLtIs0Ngl2o7fUCTh4iMhADs=
}}
        {{[file join / zip Projects editcopy.gif]} {file not found!} user {
R0lGODlhGAAYAMYAAI9/bot8a4h5aYV2ZoFzZH5wYnttX3drXXRoWop7av//
/3NnWomJiWpgVIZ3Z29jV8nJyYJ0ZIKCgsbGxl1USn5wYWZcUWNZTmBWTFxU
SVlRR3lsXlxbW/78+6SkpHR0dFVNRGVbUHVpW/37+fz38/r07vDq5sG6s1BJ
QcPDw3FlWFpVUn54c5+VjX10btjQyExGPm1iVfz49fv18Pnx6vju5ffq3/Xn
2tbKv0hCO2heUltZWcTAvYB8eVpWU8C1rJmHeHtvZXptYtPDtEQ/OGRaT/v2
8vry7Pnv5/fr4fbo3PTk1vPg0PLdy9G8qj87NU1NTWBXTH96dllVUXtvZFZM
RXhqXM62oDs3MlxTSfjt4/bp3fXl2PPi0vLezfHax+/Xwu7TvMyvlzc0L1dP
RtfMwdXHutPCs9G+rM+5pc60n8ywl8qrkMmojDMwLFNMQ1BJQElDPEZAOUI+
Nz87NDw4Mjg1MDUyLTIvKy8tKUtFPf///////////////////yH5BAEKAH8A
LAAAAAAYABgAAAf+gH+Cg4SFhoeIiYUAAQIDBAUGBwiKhQkKmJkLDA2Vgw6Z
mQ8KEJ2eEQoSEBITCg0QCgwUnhWhmBYXGBkanhupHKsQHRMeHyAhkZOHIrYj
JCUmJygpmJumhCoKHKwSKywtLi8w1AqjpYUxmB0yMzQ1Njc4OayusLKEOgo7
PD0+P0BBhAwhQg6XLl6DiigwcgRJEiVLmDRx8gSKqmHFQBCKokDKlH9UqgCx
cgULNWfQpBHKokDLFi5dvHwBE0bMGA/dvoWDQYjMizJmzqBJo2YNmzZuUrR7
Fy8HoTdwYMSRM4dOHTt38OSR4A+gQCKe9KQYm+JhxIlPPHH0CFIkSSwenli6
hCmTpk1PPoEKJWoUqSeoUqlaxarVk+HDlQIBADs=}}
        {{[file join / zip Projects edit.gif]} {file not found!} user {
R0lGODlhFgAWAPZQAAEBARQUFDImHi4qIjYyLjYyMkIuJlhLOHJOOk9PT1dX
V3JmTndqVmdnZ352a3d3d4ZKHpxIBo5SKo5eNpZaOpJiLppmNqJOBq5iMrp6
Poh6Z8JeEsxkBNZqBtpuPvJ+DuJqLpOIeJ2QfaSPb7aKarqedL2hePuFBf6W
MsKCRsqCXtqSTsqEZOqOWuKaVuqabvquXu6ydoODg52TgqaahK6ijbqmibao
lbyxnMqugsS0ndC6msO6qcS+sPW1lsfCudjDqdrNt+PLp+fPs/zMp/7Zuf7q
rsnJyNLNydXV1e/j0/7kyPjo1PTs4/jz6P39/QAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEBAFAA
LAAAAAAWABYAAAf+gFBQMjIyDw8PDw8NDQ0NDQoKCQkAEjBQUDJHR0dIR0dH
R0dHR0dHRz8JEjBGUFAyR09PT09PT0lPT09PT089Ei5GKFBQMkdPT09PT09J
T09PT09PEy5FKCdQUA9IT09PT09PSU9PT09PFS5FKCcnUFAPR0lJSUlJSUlJ
SUlIFoAuRCgnJydQUA9HT09PT09PSU9PTxkuRCgnJycdUFAPR09PT09PT0lP
TykrRCgnJyccF1BQD0dPT09PT09JTz4xRCgnJyccEQFQUA1HSUlJSUlJST5F
TC8nJyccEQEAUFAPR09PT09PT0lETkQtHycdEQEAAFBQDUdPT09PT07rgD5L
RS8eIBcRAQAAAFBQDUdPT09PT05ETD4qGBIQAAEAAABQUA1HSUlJSEg+Sz4s
FBIHAQwBAAAAUFANR09PT05NREQsFAcDBCEaAQAAUFBQCkdPT05NCyQsCAcE
ISI0IwEAUFBQUApIT05OOQsDAgohMzU6OyUBUFBQUFAKR4BIPz8LAwU5Gho0
NTY2JQFQUFBQUApHTk1KBzkzGjU8QUNCQjYBUFBQUFAKR01MSkg4NzRAQ0JC
QjssAFBQUFBQCT89PDw4NTU2Njk2NiYlJQBQUFBQUAAAAAAAAAAAAAAAAAAA
AAAAUFBQgQA7}}
        {{[file join / zip Projects stop.gif]} {file not found!} user {
R0lGODlhGAAYAOcAAJIcHIgfHX0iHnMkH2gnIF4pIV4qIZMdHd2Li/jo6M+/
v4lgX////+edleCWj9ySi9iHh9F+frRZT2Y0LAAAAOmfl9x9d9Z6ctJzatFx
atFwasxfWc1gYLUVFd2QidV5dt+BfdyCfNl/edd8dNV4b8VlXcxdXcVNRLMP
D9V5c9F3b62treeSjOCLht2Hgs97ds9za9F0a8I3KLQSEpEaGuGDfuCBfeaU
kNWGgcdWSr0YE6ERDtmIhN6EftR2bv35+dByZ8pbU70ZFKESD9V+d9mAetF0
actiWctfV92TjNR7dNd7dNF3cc9vZcxnXcZRSNeDe9B4cdNwZs9vZslaUsg5
OcxxbslwaMJZUdF4cslPQscuLrwWEaEQDeDQ0MpoZclyas1mX81pXstiWMAy
I7wUD6AOC1wpIMd9fbBORMxiYs9kWc9vZ9Bxac1pYs1eUshKPbwVEJAWFslW
Vs1hVc9jWc9oXc1gVcxISMRAMsIaGsAxIpAXF7UXF8c8OMY5NcIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIaGsIa
GsIaGsIaGsIaGsIaGsIaGiH5BAEKAHsALAAAAAAYABgAAAj+APcIHEiwoMGD
CBMqBBBAwAACBQxIVEjwAIIEGDMqWGCA4h6LDBo0cPDgAYQIEiZQUAiygoUL
GDJo2MChwwEDKw+C9PABRAgRI0iUMHECBQCcBgEgYFAhBYOnDFy8gMEkBlSk
BAEkaFDjKRMmXr+CfYp1oNYGYb+GhcqgrMAACRw8bXFDrFoGSMjmHCggQZKn
Lpj4sMvAyRO9BQckgPJ0KmEGVKogJkgggZWnVO+yhbpXoAEvX8CsZTDmqx4y
kweeQZNGzRq2bt7A0SMkTuqBEg/MoVPHzh08efToidOFj1uCFAwA6OPnj3Md
XMoUP14wOY0dQ7Lv6GJGDnWDySUSihffWSGF8+jRe1zPvr17hQEBADs=}}
        {{[file join / zip Projects imagegallery.gif]} {file not found!} user {
R0lGODlhGQAXAKUAAAAAABkZGSoqKjc3NxZHAh5ZBSBbByNjCCt4Cj5zJh1I
aSBPcyFRdTJcfDdgflF9PkhISFtbW2ZmZnl5eSlbgilkkThjiDZrmDZ1pjl8
sERrjEJ0nUh8pTGGDDaWDT2ZFkaKKWuxTTyBt0mHuVyAol6KtFOQv2qQq3aX
sEuMwliTwmWZyG6izm2i0nCl1oJ0GaiWRs3IfoaGhpWVlbq6uoGtzoq01pS5
2JS84q/L4LTP5MvHjsjb6v///////////yH5BAEBAD8ALAAAAAAZABcAAAb+
QNhrCNsBALsd7EGMxY7K4UsorVqvWClsy+16u9ev+EucwhRoxWLRUDAYDYfc
8XZOd2ZFw1Lp+ysbJyczJRsVdkZ5FRgYGRmMGSk1Ojo8OTUbdkdmDRccGRwc
KiqiNzw8PTw0HJpLWw4XGD0XFxkqKSo2lDw6N6wxD1vBMLCytLa4upW9v8FE
MBobPTQ9PRaRFys2Njc3LJkxVVsaABEz5z0cALMrKy0rpHZaMAAQExESEhAB
ERPpHCtc1PhVZgsAGRAGQIBQTgYNfftwDJBgZ4uZgwkXNny4MIBEiuHomRHA
sB6EfhEYChAQAECAAJoAmDlgYIDChQNcAiBA4ABGgg4ENIVcAgLBgSM7Dfjs
0MFDiA8IEAkR5qEq0w4Ifjat+sFDVGBLzDgZS7bs2GpDmGwxy5Ys2ixw4VK5
AyUKkSRQ5sIIAgA7}}
        {{[file join / zip Projects kthememgr.gif]} {file not found!} user {
R0lGODlhFQAWAOcAAAICBhZOCt7SUiqSBq6uBirWHpaSBmpmBpKSCLbyYj7K
Dx5iBvr2zrayVg4uBmKiBoLeZipqGjayCurmfhsaC2JeBtbaanJyBlaWBnba
UiJaEoqyPo6Ojra2at7amiyaBpbOgip6EkbiHsLCJqrWduTennqmZnp6Btrm
0hYWCPLykkbaGk6OBkI+HmLGEjKmBhI2CrSuitra1v7ycL7Klv760uLmqoKC
CFJyCrLyjrrWVkpKBjqWEkKiFi4qBqKiBj7GDlbOEvLyqnryPkpGMiaOBkDq
DILOMmbWChISEoaCOrKukqbSmma6Lv761nrCWsbORvLufj6mHiqGBjo2KlpW
BjqiGvrypk7aLra2GsLCjv763lDmIqKeKjY2BkaSFj6yIkbyDjZiBo2KBv72
vELWEkKuGq7ufpqaBj6KIh4eCh5uBkbKFv7yft7aYP32k9LOSv7+6q6uomq+
NlbKNiqeBmqqBja+Cr7CRjaiGv76r05KHlqeBjKuBjKGGiaCBkC6FCpyDlra
KkpSMqbeknJuBjrODCZiEubebvL2uqqqHkrSLl7aPiIiCX7SaobGSv7yhlJO
BsK+KlKSBgYGBkLOGv7+7oJ6DomGBjauDnp2Bip+EvLyjraufmKuGjKOEvz2
tCx2GmJeEj6uHjKiDo6KEv72nZaSEqKiEq6upn5+ErHua5KSFu7qjE7yGU6K
Fv769oqGEpCOBiqCBx5KEjaqBkayFtrSTG5qEpqaEja2FnJyEi6KEkLuDlaa
ErLymVpaElZSEjKWEr62UpK2Qk7eHkbGEmxqBiqiBjKqB/72qyp+Dv7+8iJi
DvLynD6eIkrOIi6uEnp2EoF+BvL2op6eBiZ2DyYmCj7OD/76xmamBjK2CmZi
BnZ2Bv76wipmFv72jP76tq6urY6KDv7++YqGDrLucuLaYjquFvL2qpaWBlqa
BqamBj6qIjo6Kl5aBjo6Bj7aDkbOGl6iBi6KFubicq6uFlZSBmZiEqamEh5O
FiqCDiJmEj6iHv///////yH5BAEKAP8ALAAAAAAVABYAAAj+AP8J/IeunxV5
A3vFUwBooMOBsw79e8blnytsVv6FovfQ4aeBUv5B8yMwgrVZHQVGCyUwRDsq
Ar99s5byHwB+uspwwQLm0ZxPOFLUBABohat2oxjlMXNEB5QuxwA8TKJAECFH
vNZIMJJmgwU3cLJUozQQAAc6KCBUysSjiSBDH74Qw6MIQaOy4piAuJPpCQln
q4wY61Hk1RhZdwUCEEful4JkNMjogdQriK86U9DYxdv4sQ0tHt70QvIA2Z8x
Y9Tg3ZLjcTploFqB8aQN2axpJ1QrTrXljKFk6fToaUXgR+1ZJ7rptimnRus+
QoRP+IGGj+lCxyiUXeJtlQLoV6531FNnwPqsYwe0K15CxncyFW/0IFI3Zp3p
A9xSSLUZA5T395CAc44sN2BgWwXv6AfAgp0o858K4AiIyTSTILNMFffo91Az
urwQBSQzCHBCIa8MsAYlZKUEwAINzBDiBRWI4cB+NQlEgRLDCACPDzTW6FAL
e/j4T0AAOw==}}
        {{[file join / zip Projects mini-run.gif]} {file not found!} user {
R0lGODlhGwAYAIAAAAAAAP///yH5BAEKAAEALAAAAAAbABgAAAI6jI+py+0P
owJUNlrtwprxPmUaRooScJQm6rCB6r6W+n6nHdnlKO43jPtVfCADsXgEJZGm
orHJjDkfBQA7}}
        {{[file join / zip Projects kleandisk.gif]} {file not found!} user {
R0lGODlhHAAYAIQAAAAAAC0qHPO5U7aRSA8ODY12QYdxP3loPXJkPMueS8SZ
SreQR7CMRqKDRJt/Q+CrTtmnTWRbOjYyIvO9Z+evTz44JDo1I15XOaSFRUhC
K1FKL//sXv+1ZVpSNc6IM////yH5BAEKAB8ALAAAAAAcABgAAAXS4CeO5Aic
ZaqqgTAEa8y6sEgQcty+Np7PvN4PWLv9AsiaKPAq+lbIguGASO6cskBCsWA0
HFIqDfAx6h6QbfcrdUUkJyR0QqlT0GpaxUKNXJIlVnZ3aTRsUwh+KCMBGI5W
AXZ4XF4OCAQZZCYnjp0YgpNdmBqaJhsnqJ6fVg8EGpmlIgCnHLOonKoBHaQe
sR8AtbW0trcAnx0nvWa/wrbNwcCovcpm0cOnw9DTANTV0c3Ytqfb3U+/qNfi
3OsAN8spt9Dj6+7mMsXsBItD8O36KSEAADs=}}
        {{[file join / zip Projects x4.gif]} {file not found!} user {
R0lGODlhGAAYAOcAAJIcHIgfHX0iHnMkH2gnIF4pIV4qIZMdHd2Li/jo6M+/
v4lgX////+edleCWj9ySi9iHh9F+frRZT2Y0LAAAAOmfl9x9d9Z6ctJzatFx
atFwasxfWc1gYLUVFd2QidV5dt+BfdyCfNl/edd8dNV4b8VlXcxdXcVNRLMP
D9V5c9F3b62treeSjOCLht2Hgs97ds9za9F0a8I3KLQSEpEaGuGDfuCBfeaU
kNWGgcdWSr0YE6ERDtmIhN6EftR2bv35+dByZ8pbU70ZFKESD9V+d9mAetF0
actiWctfV92TjNR7dNd7dNF3cc9vZcxnXcZRSNeDe9B4cdNwZs9vZslaUsg5
OcxxbslwaMJZUdF4cslPQscuLrwWEaEQDeDQ0MpoZclyas1mX81pXstiWMAy
I7wUD6AOC1wpIMd9fbBORMxiYs9kWc9vZ9Bxac1pYs1eUshKPbwVEJAWFslW
Vs1hVc9jWc9oXc1gVcxISMRAMsIaGsAxIpAXF7UXF8c8OMY5NbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwWEbwW
EbwWEbwWEbwWEbwWEbwWESH5BAEKAHgALAAAAAAYABgAAAj+APEIHEiwoMGD
CBMqBBBAwAACBQxIVEjwAIIEGDMqWGCAoooVLFq4eAEjSAwIESRMoKBwhRgx
N3CUfLmBQ4cDBlgePMDipRgfQV4CKbHlhNGcBgEgaMGAgc+mR4IEbcoAKUEA
CVxQFUNV6kuqVgdiJUm16ZSgQcqGFRggwcytYrRwZcCl6Vo8AhKsmNrUJ1cu
davqHDggQZi5fb86DXyXQAIrc9MykFqW6mCBBrx8acp38kvAdi/jOYMmTV/K
DLao5jL3Lh6JB+aIqWPnjpg8evYA5uNaIAUDAPr4+UNcB5cyXXiLLvibxo4h
0Hd0MSOnN3OJ2LMvR0ihu3fvFMMGix9PnnxAADs=}}
        {{[file join / zip Projects reload.gif]} {user image} user {
R0lGODlhEgASAMYAACZcKB5UIRVEAClfLLPYsiCKFy+lIhE4AAwmACFYJNvs
2gAAAAQNACNTJqnXrRxOH268Zj++MRtSHhxQG1amT+Tk5BtXGrXXsyibGSSS
FS94P0bPNhU8FxYWFiRJJsbgxC96QUTNNiMjI7i4uBJFFUTLNQUFBbq6ulNT
U1JSUqmpqVhYWC0tLTY2NhEREQQEBHt7ezExMTMzM5+fnzc3N4GBgUxMTBkZ
GQEBAbKyskVFRRISEhoaGgICApKSkl1dXT09PQ0NDSYmJh4eHv//////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
/////////////////////////////////////////////////yH5BAEKAH8A
LAAAAAASABIAAAeagH+CggABhgECg4qKAwQFBgIHCAKJi4IJCo+VfwsMm4oN
DpCWC5+CDxARppYSExIPFKqUlRWWFhcQGBkaGxyCHZaCHh8ZICG+fyIjC8F/
JBclyCYnKCnMwRzICyorLC3WzYIuLi8wMTDjLuGCMjM014oL8IMLNTY3wTg4
8As5KzrzFO3gsaAHCh8/gARZ98eFEHlDAjKcSFFQIAA7}}
        {{[file join / zip Projects shortout.gif]} {user image} user {
R0lGODlhGAAYAMYAAI9/bot8a4h5aYV2ZoFzZH5wYnttX3drXXRoWop7av//
/3NnWomJiWpgVIZ3Z29jV8nJyYJ0ZIKCgsbGxl1USn5wYWZcUWNZTmBWTFxU
SVlRR3lsXlxbW/78+6SkpHR0dFVNRHVpW/37+fz38/r07vDq5sG6s1BJQXFl
WFpVUn54c5+VjX10btjQyExGPm1iVfz49fv18Pnx6vju5ffq3/Xn2tbKv0hC
O2heUltZWcTAvYB8eVpWU8C1rJmHeHtvZXptYtPDtEQ/OMPDw2RaT/v28vry
7Pnv5/fr4fbo3PTk1vPg0PLdy9G8qj87NWBXTH96dllVUXtvZFZMRXhqXM62
oDs3MlxTSfjt4/bp3fXl2PPi0vLezfHax+/Xwu7TvMyvlzc0L1dPRtfMwdXH
utPCs9G+rM+5pc60n8ywl8qrkMmojDMwLFNMQ1BJQElDPEZAOUI+Nz87NDw4
Mjg1MDUyLTIvKy8tKUtFPf///////////////////////////yH5BAEKAH8A
LAAAAAAYABgAAAfOgH+Cg4SFhoeIiYUAAQIDBAUGBwiKhQkKmJkLDA2Vgw6Z
mQ8KEJ2eEQoSEBITCg0QCgwUnhWhmBYXGBkanhupHKsQHRMeHyCeIbYiIyQl
JieeKAocrBIpKissLS6eL5gdMDEyMzQ1NjcSlTgKOTo7PD0+P0BBQkOVRApF
RkdISUpLmDRx4umJAihR4kmZ4oNKFSuerijAkkXLFi5dvHwBE8aTmBZjyJQx
cwZNGjVr2Hhq48bFGzhx5MyhU8fOHU+I8ODcybOnz59AgwolFAgAOw==}}

            } {
    eval set _file [lindex $img 0]
    vTcl:image:create_new_image\
        $_file [lindex $img 1] [lindex $img 2] [lindex $img 3]
}

#############################################################################
# vTcl Code to Load Stock Fonts


if {![info exist vTcl(sourcing)]} {
set vTcl(fonts,counter) 0
#############################################################################
## Procedure:  vTcl:font:add_font

proc {vTcl:font:add_font} {font_descr font_type {newkey {}}} {
     global vTcl

     incr vTcl(fonts,counter)
     set newfont [eval font create $font_descr]

     lappend vTcl(fonts,objects) $newfont

     # each font has its unique key so that when a project is
     # reloaded, the key is used to find the font description

     if {$newkey == ""} {
          set newkey vTcl:font$vTcl(fonts,counter)

          # let's find an unused font key
          while {[vTcl:font:get_font $newkey] != ""} {
             incr vTcl(fonts,counter)
             set newkey vTcl:font$vTcl(fonts,counter)
          }
     }

     set vTcl(fonts,$newfont,type)                      $font_type
     set vTcl(fonts,$newfont,key)                       $newkey
     set vTcl(fonts,$vTcl(fonts,$newfont,key),object)   $newfont

     lappend vTcl(fonts,$font_type) $newfont

     # in case caller needs it
     return $newfont
}

#############################################################################
## Procedure:  vTcl:font:get_font

proc {vTcl:font:get_font} {key} {
    global vTcl
    if {[info exists vTcl(fonts,$key,object)]} then {
        return $vTcl(fonts,$key,object)
    } else {
        return ""
    }
}

vTcl:font:add_font \
    "-family times -size 12 -weight normal -slant roman -underline 0 -overstrike 0" \
    stock \
    vTcl:font4
}
#################################
# VTCL LIBRARY PROCEDURES
#

if {![info exists vTcl(sourcing)]} {
#############################################################################
## Library Procedure:  Window

proc {Window} {args} {
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
## Library Procedure:  ::mclistbox::AdjustColumns

namespace eval ::mclistbox {

proc {::mclistbox::AdjustColumns} {w {height {}}} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    if {[string length $height] == 0} {
	set height [winfo height $widgets(text)]
    }

    # resize the height of each column so it matches the height
    # of the text widget, minus a few pixels. 
    incr height -4
    foreach id $misc(columns) {
	$widgets(frame$id) configure -height $height
    }
    
    # if we have a fillcolumn, change its width accordingly
    if {$options(-fillcolumn) != ""} {

	# make sure the column has been defined. If not, bail (?)
	if {![info exists widgets(frame$options(-fillcolumn))]} {
	    return
	}
	set frame $widgets(frame$options(-fillcolumn))
	set minwidth $misc(min-$frame)

	# compute current width of all columns
	set colwidth 0
	set col 0
	foreach id $misc(columns) {
	    if {![ColumnIsHidden $w $id] && $id != $options(-fillcolumn)} {
		incr colwidth [winfo reqwidth $widgets(frame$id)]
	    }
	}

	# this is just shorthand for later use...
	set id $options(-fillcolumn)

	# compute optimal width
	set optwidth [expr {[winfo width $widgets(text)] -  (2 * [$widgets(text) cget -padx])}]

	# compute the width of our fill column
	set newwidth [expr {$optwidth - $colwidth}]

	if {$newwidth < $minwidth} {
	    set newwidth $minwidth
	}

	# adjust the width of our fill column frame
	$widgets(frame$id) configure -width $newwidth
	    
    }
    InvalidateScrollbars $w
}

}
#############################################################################
## Library Procedure:  ::mclistbox::Build

namespace eval ::mclistbox {

proc {::mclistbox::Build} {w args} {
    variable widgetOptions

    # create the namespace for this instance, and define a few
    # variables
    namespace eval ::mclistbox::$w {

	variable options
	variable widgets
	variable misc 
    }

    # this gives us access to the namespace variables within
    # this proc
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    # initially we start out with no columns
    set misc(columns) {}

    # this is our widget -- a frame of class Mclistbox. Naturally,
    # it will contain other widgets. We create it here because
    # we need it to be able to set our default options.
    set widgets(this)   [frame  $w -class Mclistbox -takefocus 1]

    # this defines all of the default options. We get the
    # values from the option database. Note that if an array
    # value is a list of length one it is an alias to another
    # option, so we just ignore it
    foreach name [array names widgetOptions] {
	if {[llength $widgetOptions($name)] == 1} continue
	set optName  [lindex $widgetOptions($name) 0]
	set optClass [lindex $widgetOptions($name) 1]
	set options($name) [option get $w $optName $optClass]
    }

    # now apply any of the options supplied on the command
    # line. This may overwrite our defaults, which is OK
    if {[llength $args] > 0} {
	array set options $args
    }
    
    # the columns all go into a text widget since it has the 
    # ability to scroll.
    set widgets(text) [text $w.text  -width 0  -height 0  -padx 0  -pady 0  -wrap none  -borderwidth 0  -highlightthickness 0  -takefocus 0  -cursor {}  ]

    $widgets(text) configure -state disabled

    # here's the tricky part (shhhh... don't tell anybody!)
    # we are going to create column that completely fills
    # the base frame. We will use it to control the sizing
    # of the widget. The trick is, we'll pack it in the frame 
    # and then place the text widget over it so it is never
    # seen.

    set columnWidgets [NewColumn $w {__hidden__}]
    set widgets(hiddenFrame)   [lindex $columnWidgets 0]
    set widgets(hiddenListbox) [lindex $columnWidgets 1]
    set widgets(hiddenLabel)   [lindex $columnWidgets 2]

    # by default geometry propagation is turned off, but for this
    # super-secret widget we want it turned on. The idea is, we 
    # resize the listbox which resizes the frame which resizes the
    # whole shibang.
    pack propagate $widgets(hiddenFrame) on

    pack $widgets(hiddenFrame) -side top -fill both -expand y
    place $widgets(text) -x 0 -y 0 -relwidth 1.0 -relheight 1.0
    raise $widgets(text)

    # we will later rename the frame's widget proc to be our
    # own custom widget proc. We need to keep track of this
    # new name, so we'll define and store it here...
    set widgets(frame) ::mclistbox::${w}::$w

    # this moves the original frame widget proc into our
    # namespace and gives it a handy name
    rename ::$w $widgets(frame)

    # now, create our widget proc. Obviously (?) it goes in
    # the global namespace. All mclistbox widgets will actually
    # share the same widget proc to cut down on the amount of
    # bloat. 
    proc ::$w {command args}  "eval ::mclistbox::WidgetProc {$w} \$command \$args"

    # ok, the thing exists... let's do a bit more configuration. 
    if {[catch "Configure $widgets(this) [array get options]" error]} {
	catch {destroy $w}
    }

    # and be prepared to handle selections.. (this, for -exportselection
    # support)
    selection handle $w [list ::mclistbox::SelectionHandler $w get]

    return $w
}

}
#############################################################################
## Library Procedure:  ::mclistbox::Canonize

namespace eval ::mclistbox {

proc {::mclistbox::Canonize} {w object opt} {
    variable widgetOptions
    variable columnOptions
    variable widgetCommands
    variable columnCommands
    variable labelCommands

    switch $object {
	command {
	    if {[lsearch -exact $widgetCommands $opt] >= 0} {
		return $opt
	    }

	    # command names aren't stored in an array, and there
	    # isn't a way to get all the matches in a list, so
	    # we'll stuff the columns in a temporary array so
	    # we can use [array names]
	    set list $widgetCommands
	    foreach element $list {
		set tmp($element) ""
	    }
	    set matches [array names tmp ${opt}*]
	}

	{label command} {
	    if {[lsearch -exact $labelCommands $opt] >= 0} {
		return $opt
	    }

	    # command names aren't stored in an array, and there
	    # isn't a way to get all the matches in a list, so
	    # we'll stuff the columns in a temporary array so
	    # we can use [array names]
	    set list $labelCommands
	    foreach element $list {
		set tmp($element) ""
	    }
	    set matches [array names tmp ${opt}*]
	}

	{column command} {
	    if {[lsearch -exact $columnCommands $opt] >= 0} {
		return $opt
	    }

	    # command names aren't stored in an array, and there
	    # isn't a way to get all the matches in a list, so
	    # we'll stuff the columns in a temporary array so
	    # we can use [array names]
	    set list $columnCommands
	    foreach element $list {
		set tmp($element) ""
	    }
	    set matches [array names tmp ${opt}*]
	}

	option {
	    if {[info exists widgetOptions($opt)]  && [llength $widgetOptions($opt)] == 3} {
		return $opt
	    }
	    set list [array names widgetOptions]
	    set matches [array names widgetOptions ${opt}*]
	}

	{column option} {
	    if {[info exists columnOptions($opt)]} {
		return $opt
	    }
	    set list [array names columnOptions]
	    set matches [array names columnOptions ${opt}*]
	}

	column {
	    upvar ::mclistbox::${w}::misc    misc

	    if {[lsearch -exact $misc(columns) $opt] != -1} {
		return $opt
	    }
	    
	    # column names aren't stored in an array, and there
	    # isn't a way to get all the matches in a list, so
	    # we'll stuff the columns in a temporary array so
	    # we can use [array names]
	    set list $misc(columns)
	    foreach element $misc(columns) {
		set tmp($element) ""
	    }
	    set matches [array names tmp ${opt}*]
	}
    }
    if {[llength $matches] == 0} {
	set choices [HumanizeList $list]
	return -code error "unknown $object \"$opt\"; must be one of $choices"

    } elseif {[llength $matches] == 1} {
	# deal with option aliases
	set opt [lindex $matches 0]
	switch $object {
	    option {
		if {[llength $widgetOptions($opt)] == 1} {
		    set opt $widgetOptions($opt)
		}
	    }

	    {column option} {
		if {[llength $columnOptions($opt)] == 1} {
		    set opt $columnOptions($opt)
		}
	    }
	}

	return $opt

    } else {
	set choices [HumanizeList $list]
	return -code error "ambiguous $object \"$opt\"; must be one of $choices"
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::CheckColumnID

namespace eval ::mclistbox {

proc {::mclistbox::CheckColumnID} {w id} {
    upvar ::mclistbox::${w}::misc    misc

    set id [::mclistbox::Canonize $w column $id]
    set index [lsearch -exact $misc(columns) $id]
    return $index
}

}
#############################################################################
## Library Procedure:  ::mclistbox::Column-add

namespace eval ::mclistbox {

proc {::mclistbox::Column-add} {w args} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    variable widgetOptions

    set id "column-[llength $misc(columns)]" ;# a suitable default

    # if the first argument doesn't have a "-" as the first
    # character, it is an id to associate with this column
    if {![string match {-*} [lindex $args 0]]} {
	# the first arg must be an id.
	set id [lindex $args 0]
	set args [lrange $args 1 end]
	if {[lsearch -exact $misc(columns) $id] != -1} {
	    return -code error "column \"$id\" already exists"
	}
    }

    # define some reasonable defaults, then add any specific
    # values supplied by the user
    set opts(-bitmap)     {}
    set opts(-image)      {}
    set opts(-labelrelief) raised
    set opts(-visible)    1
    set opts(-resizable)  1
    set opts(-position)   "end"
    set opts(-width)      20
    set opts(-background) $options(-background)
    set opts(-foreground) $options(-foreground)
    set opts(-font)       $options(-font)
    set opts(-label)      $id

    if {[expr {[llength $args]%2}] == 1} {
	# hmmm. An odd number of elements in args
	# if the last item is a valid option we'll give a different
	# error than if its not
	set option [::mclistbox::Canonize $w "column option" [lindex $args end]]
	return -code error "value for \"[lindex $args end]\" missing"
    }
    array set opts $args

    # figure out if we have any data in the listbox yet; we'll need
    # this information in a minute...
    if {[llength $misc(columns)] > 0} {
	set col0 [lindex $misc(columns) 0]
	set existingRows [$widgets(listbox$col0) size]
    } else {
	set existingRows 0
    }

    # create the widget and assign the associated paths to our array
    set widgetlist [NewColumn $w $id]

    set widgets(frame$id)   [lindex $widgetlist 0]
    set widgets(listbox$id) [lindex $widgetlist 1]
    set widgets(label$id)   [lindex $widgetlist 2]
    
    # add this column to the list of known columns
    lappend misc(columns) $id

    # configure the options. As a side effect, it will be inserted
    # in the text widget
    eval ::mclistbox::Column-configure {$w} {$id} [array get opts]

    # now, if there is any data already in the listbox, we need to
    # add a corresponding number of blank items. At least, I *think*
    # that's the right thing to do.
    if {$existingRows > 0} {
	set blanks {}
	for {set i 0} {$i < $existingRows} {incr i} {
	    lappend blanks {}
	}
	eval {$widgets(listbox$id)} insert end $blanks
    }

    InvalidateScrollbars $w
    return $id
}

}
#############################################################################
## Library Procedure:  ::mclistbox::Column-configure

namespace eval ::mclistbox {

proc {::mclistbox::Column-configure} {w id args} {
    variable widgetOptions
    variable columnOptions

    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    # bail if they gave us a bogus id
    set index [CheckColumnID $w $id]

    # define some shorthand
    set listbox $widgets(listbox$id)
    set frame   $widgets(frame$id)
    set label   $widgets(label$id)

    if {[llength $args] == 0} {
	# hmmm. User must be wanting all configuration information
	# note that if the value of an array element is of length
	# one it is an alias, which needs to be handled slightly
	# differently
	set results {}
	foreach opt [lsort [array names columnOptions]] {
	    if {[llength $columnOptions($opt)] == 1} {
		set alias $columnOptions($opt)
		set optName $columnOptions($alias)
		lappend results [list $opt $optName]
	    } else {
		set optName  [lindex $columnOptions($opt) 0]
		set optClass [lindex $columnOptions($opt) 1]
		set default [option get $frame $optName $optClass]
		lappend results [list $opt $optName $optClass  $default $options($id:$opt)]
	    }
	}

	return $results


    } elseif {[llength $args] == 1} {

	# the user must be querying something... I need to get this
	# to return a bona fide list like the "real" configure 
	# command, but it's not a priority at the moment. I still
	# have to work on the option database support foo.
	set option [::mclistbox::Canonize $w "column option" [lindex $args 0]]

	set value $options($id:$option)
	set optName  [lindex $columnOptions($option) 0]
	set optClass [lindex $columnOptions($option) 1]
	set default  [option get $frame $optName $optClass]
	set results  [list $option $optName $optClass $default $value]
	return $results

    }

    # if we have an odd number of values, bail. 
    if {[expr {[llength $args]%2}] == 1} {
	# hmmm. An odd number of elements in args
	return -code error "value for \"[lindex $args end]\" missing"
    }
    
    # Great. An even number of options. Let's make sure they 
    # are all valid before we do anything. Note that Canonize
    # will generate an error if it finds a bogus option; otherwise
    # it returns the canonical option name
    foreach {name value} $args {
	set name [::mclistbox::Canonize $w "column option" $name]
	set opts($name) $value
    }

    # if we get to here, the user is wanting to set some options
    foreach option [array names opts] {
	set value $opts($option)
	set options($id:$option) $value

	switch -- $option {
	    -label {
		$label configure -text $value
	    }
	    
	    -image -
	    -bitmap {
		$label configure $option $value
	    }

	    -width {
		set font [$listbox cget -font]
		set factor [font measure $options(-font) "0"]
		set width [expr {$value * $factor}]

		$widgets(frame$id) configure -width $width
		set misc(min-$widgets(frame$id)) $width
		AdjustColumns $w
	    }
	    -font -
	    -foreground -
	    -background {
		if {[string length $value] == 0} {set value $options($option)}
		$listbox configure $option $value
	    }

	    -labelrelief {
		$widgets(label$id) configure -relief $value
	    }

	    -resizable {
		if {[catch {
		    if {$value} {
			set options($id:-resizable) 1
		    } else {
			set options($id:-resizable) 0
		    }
		} msg]} {
		    return -code error "expected boolean but got \"$value\""
		}
	    }

	    -visible {
		if {[catch {
		    if {$value} {
			set options($id:-visible) 1
			$widgets(text) configure -state normal
			$widgets(text) window configure 1.$index -window $frame
			$widgets(text) configure -state disabled

		    } else {
			set options($id:-visible) 0
			$widgets(text) configure -state normal
			$widgets(text) window configure 1.$index -window {}
			$widgets(text) configure -state disabled
		    }
		    InvalidateScrollbars $w
		} msg]} {
		    return -code error "expected boolean but got \"$value\""
		}

	    }

	    -position {
		if {[string compare $value "start"] == 0} {
		    set position 0

		} elseif {[string compare $value "end"] == 0} {

		    set position [expr {[llength $misc(columns)] -1}]
		} else {

		    # ought to check for a legal value here, but I'm 
		    # lazy
		    set position $value
		}

		if {$position >= [llength $misc(columns)]} {
		    set max [expr {[llength $misc(columns)] -1}]
		    return -code error "bad position; must be in the range of 0-$max"
		}

		# rearrange misc(columns) to reflect the new ordering
		set current [lsearch -exact $misc(columns) $id]
		set misc(columns) [lreplace $misc(columns) $current $current]
		set misc(columns) [linsert $misc(columns) $position $id]
		
		set frame $widgets(frame$id)
		$widgets(text) configure -state normal
		$widgets(text) window create 1.$position  -window $frame -stretch 1
		$widgets(text) configure -state disabled
 	    }

	}
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::ColumnIsHidden

namespace eval ::mclistbox {

proc {::mclistbox::ColumnIsHidden} {w id} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::misc    misc
    
    set retval 1
    set col [lsearch -exact $misc(columns) $id]

    if {$col != ""} {
	set index "1.$col"
	catch {
	    set window [$widgets(text) window cget $index -window]
	    if {[string length $window] > 0 && [winfo exists $window]} {
		set retval 0
	    }
	}
    }
    return $retval
}

}
#############################################################################
## Library Procedure:  ::mclistbox::Configure

namespace eval ::mclistbox {

proc {::mclistbox::Configure} {w args} {
    variable widgetOptions

    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc
    
    if {[llength $args] == 0} {
	# hmmm. User must be wanting all configuration information
	# note that if the value of an array element is of length
	# one it is an alias, which needs to be handled slightly
	# differently
	set results {}
	foreach opt [lsort [array names widgetOptions]] {
	    if {[llength $widgetOptions($opt)] == 1} {
		set alias $widgetOptions($opt)
		set optName $widgetOptions($alias)
		lappend results [list $opt $optName]
	    } else {
		set optName  [lindex $widgetOptions($opt) 0]
		set optClass [lindex $widgetOptions($opt) 1]
		set default [option get $w $optName $optClass]
		lappend results [list $opt $optName $optClass  $default $options($opt)]
	    }
	}

	return $results
    }
    
    # one argument means we are looking for configuration
    # information on a single option
    if {[llength $args] == 1} {
	set opt [::mclistbox::Canonize $w option [lindex $args 0]]

	set optName  [lindex $widgetOptions($opt) 0]
	set optClass [lindex $widgetOptions($opt) 1]
	set default [option get $w $optName $optClass]
	set results [list $opt $optName $optClass  $default $options($opt)]
	return $results
    }

    # if we have an odd number of values, bail. 
    if {[expr {[llength $args]%2}] == 1} {
	# hmmm. An odd number of elements in args
	return -code error "value for \"[lindex $args end]\" missing"
    }
    
    # Great. An even number of options. Let's make sure they 
    # are all valid before we do anything. Note that Canonize
    # will generate an error if it finds a bogus option; otherwise
    # it returns the canonical option name
    foreach {name value} $args {
	set name [::mclistbox::Canonize $w option $name]
	set opts($name) $value
    }

    # process all of the configuration options
    foreach option [array names opts] {

	set newValue $opts($option)
	if {[info exists options($option)]} {
	    set oldValue $options($option)
#	    set options($option) $newValue
	}

	switch -- $option {
	    -exportselection {
		if {$newValue} {
		    SelectionHandler $w own
		    set options($option) 1
		} else {
		    set options($option) 0
		}
	    }

	    -fillcolumn {
		# if the fill column changed, we need to adjust
		# the columns.
		AdjustColumns $w
		set options($option) $newValue
	    }

	    -takefocus {
		$widgets(frame) configure -takefocus $newValue
		set options($option) [$widgets(frame) cget $option]
	    }

	    -background {
		foreach id $misc(columns) {
		    $widgets(listbox$id) configure -background $newValue
		    $widgets(frame$id) configure   -background $newValue
		}
		$widgets(frame) configure -background $newValue

		$widgets(text) configure -background $newValue
		set options($option) [$widgets(frame) cget $option]
	    }

	    # { the following all must be applied to each listbox }
	    -foreground -
	    -font -
	    -selectborderwidth -
	    -selectforeground -
	    -selectbackground -
	    -setgrid {
		foreach id $misc(columns) {
		    $widgets(listbox$id) configure $option $newValue
		}
		$widgets(hiddenListbox) configure $option $newValue
		set options($option) [$widgets(hiddenListbox) cget $option]
	    }

	    # { the following all must be applied to each listbox and frame }
	    -cursor {
		foreach id $misc(columns) {
		    $widgets(listbox$id) configure $option $newValue
		    $widgets(frame$id) configure -cursor $newValue
		}

		# -cursor also needs to be applied to the 
		# frames of each column
		foreach id $misc(columns) {
		    $widgets(frame$id) configure -cursor $newValue
		}

		$widgets(hiddenListbox) configure $option $newValue
		set options($option) [$widgets(hiddenListbox) cget $option]
	    }

	    # { this just requires to pack or unpack the labels }
	    -labels {
		if {$newValue} {
		    set newValue 1
		    foreach id $misc(columns) {
			pack $widgets(label$id)  -side top -fill x -expand n  -before $widgets(listbox$id)
		    }
		    pack $widgets(hiddenLabel)  -side top -fill x -expand n  -before $widgets(hiddenListbox)

		} else {
		    set newValue 
		    foreach id $misc(columns) {
			pack forget $widgets(label$id)
		    }
		    pack forget $widgets(hiddenLabel)
		}
		set options($option) $newValue
	    }

	    -height {
		$widgets(hiddenListbox) configure -height $newValue
		InvalidateScrollbars $w
		set options($option) [$widgets(hiddenListbox) cget $option]
	    }

	    -width {
		if {$newValue == 0} {
		    return -code error "a -width of zero is not supported. "
		}

		$widgets(hiddenListbox) configure -width $newValue
		InvalidateScrollbars $w
		set options($option) [$widgets(hiddenListbox) cget $option]
	    }

	    # { the following all must be applied to each column frame }
	    -columnborderwidth -
	    -columnrelief {
		regsub {column} $option {} listboxoption
		foreach id $misc(columns) {
		    $widgets(listbox$id) configure $listboxoption $newValue
		}
		$widgets(hiddenListbox) configure $listboxoption $newValue
		set options($option) [$widgets(hiddenListbox) cget  $listboxoption]
	    }

	    -resizablecolumns {
		if {$newValue} {
		    set options($option) 1
		} else {
		    set options($option) 0
		}
	    }
	    
	    # { the following all must be applied to each column header }
	    -labelimage -
	    -labelheight -
	    -labelrelief -
	    -labelfont -
	    -labelanchor -
	    -labelbackground -
	    -labelforeground -
	    -labelborderwidth {
		regsub {label} $option {} labeloption
		foreach id $misc(columns) {
		    $widgets(label$id) configure $labeloption $newValue
		}
		$widgets(hiddenLabel) configure $labeloption $newValue
		set options($option) [$widgets(hiddenLabel) cget $labeloption]
	    }

	    # { the following apply only to the topmost frame}
	    -borderwidth -
	    -highlightthickness -
	    -highlightcolor -
	    -highlightbackground -
	    -relief {
		$widgets(frame) configure $option $newValue
		set options($option) [$widgets(frame) cget $option]
	    }

	    -selectmode {
		set options($option) $newValue
	    }

	    -selectcommand {
		set options($option) $newValue
	    }

	    -xscrollcommand {
		InvalidateScrollbars $w
		set options($option) $newValue
	    }

	    -yscrollcommand {
		InvalidateScrollbars $w
		set options($option) $newValue
	    }
	}
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::DestroyHandler

namespace eval ::mclistbox {

proc {::mclistbox::DestroyHandler} {w} {

    # kill off any idle event we might have pending
    if {[info exists ::mclistbox::${w}::misc(afterid)]} {
	catch {
	    after cancel $::mclistbox::${w}::misc(afterid)
	    unset ::mclistbox::${w}::misc(afterid)
	}
    }

    # if the widget actually being destroyed is of class Mclistbox,
    # crush the namespace and kill the proc. Get it? Crush. Kill. 
    # Destroy. Heh. Danger Will Robinson! Oh, man! I'm so funny it
    # brings tears to my eyes.
    if {[string compare [winfo class $w] "Mclistbox"] == 0} {
	namespace delete ::mclistbox::$w
	rename $w {}
    }

}

}
#############################################################################
## Library Procedure:  ::mclistbox::FindResizableNeighbor

namespace eval ::mclistbox {

proc {::mclistbox::FindResizableNeighbor} {w id {direction right}} {
    upvar ::mclistbox::${w}::widgets       widgets
    upvar ::mclistbox::${w}::options       options
    upvar ::mclistbox::${w}::misc          misc


    if {$direction == "right"} {
	set incr 1
	set stop [llength $misc(columns)]
	set start [expr {[lsearch -exact $misc(columns) $id] + 1}]
    } else {
	set incr -1
	set stop -1
	set start [expr {[lsearch -exact $misc(columns) $id] - 1}]
    } 

    for {set i $start} {$i != $stop} {incr i $incr} {
	set col [lindex $misc(columns) $i]
	if {![ColumnIsHidden $w $col] && $options($col:-resizable)} {
	    return $col
	}
    }
    return ""
}

}
#############################################################################
## Library Procedure:  ::mclistbox::HumanizeList

namespace eval ::mclistbox {

proc {::mclistbox::HumanizeList} {list} {

    if {[llength $list] == 1} {
	return [lindex $list 0]
    } else {
	set list [lsort $list]
	set secondToLast [expr {[llength $list] -2}]
	set most [lrange $list 0 $secondToLast]
	set last [lindex $list end]

	return "[join $most {, }] or $last"
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::Init

namespace eval ::mclistbox {

proc {::mclistbox::Init} {} {
    variable widgetOptions
    variable columnOptions
    variable widgetCommands
    variable columnCommands
    variable labelCommands

    # here we match up command line options with option database names
    # and classes. As it turns out, this is a handy reference of all of the
    # available options. Note that if an item has a value with only one
    # item (like -bd, for example) it is a synonym and the value is the
    # actual item.

    array set widgetOptions [list  -background          {background          Background}  -bd                  -borderwidth  -bg                  -background  -borderwidth         {borderWidth         BorderWidth}  -columnbd            -columnborderwidth  -columnborderwidth   {columnBorderWidth   BorderWidth}  -columnrelief        {columnRelief        Relief}  -cursor              {cursor              Cursor}  -exportselection     {exportSelection     ExportSelection}  -fg                  -foreground  -fillcolumn          {fillColumn          FillColumn}  -font                {font                Font}  -foreground          {foreground          Foreground}  -height              {height              Height}  -highlightbackground {highlightBackground HighlightBackground}  -highlightcolor      {highlightColor      HighlightColor}  -highlightthickness  {highlightThickness  HighlightThickness}  -labelanchor         {labelAnchor         Anchor}  -labelbackground     {labelBackground     Background}  -labelbd             -labelborderwidth  -labelbg             -labelbackground  -labelborderwidth    {labelBorderWidth    BorderWidth}  -labelfg             -labelforeground  -labelfont           {labelFont           Font}  -labelforeground     {labelForeground     Foreground}  -labelheight         {labelHeight         Height}  -labelimage          {labelImage          Image}  -labelrelief         {labelRelief         Relief}  -labels              {labels              Labels}  -relief              {relief              Relief}  -resizablecolumns    {resizableColumns    ResizableColumns}  -selectbackground    {selectBackground    Foreground}  -selectborderwidth   {selectBorderWidth   BorderWidth}  -selectcommand       {selectCommand       Command}  -selectforeground    {selectForeground    Background}  -selectmode          {selectMode          SelectMode}  -setgrid             {setGrid             SetGrid}  -takefocus           {takeFocus           TakeFocus}  -width               {width               Width}  -xscrollcommand      {xScrollCommand      ScrollCommand}  -yscrollcommand      {yScrollCommand      ScrollCommand}  ]

    # and likewise for column-specific stuff. 
    array set columnOptions [list  -background         {background           Background}  -bitmap		{bitmap               Bitmap}  -font               {font                 Font}  -foreground         {foreground           Foreground}  -image              {image                Image}  -label 		{label                Label}  -position           {position             Position}  -labelrelief        {labelrelief          Labelrelief}  -resizable          {resizable            Resizable}  -visible            {visible              Visible}  -width              {width                Width}  ]

    # this defines the valid widget commands. It's important to
    # list them here; we use this list to validate commands and
    # expand abbreviations.
    set widgetCommands [list  activate	 bbox       cget     column    configure   curselection delete     get      index     insert  label        nearest    scan     see       selection   size         xview      yview
    ]

    set columnCommands [list add cget configure delete names nearest]
    set labelCommands  [list bind]

    ######################################################################
    #- this initializes the option database. Kinda gross, but it works
    #- (I think). 
    ######################################################################

    set packages [package names]

    # why check for the Tk package? This lets us be sourced into 
    # an interpreter that doesn't have Tk loaded, such as the slave
    # interpreter used by pkg_mkIndex. In theory it should have no
    # side effects when run 
    if {[lsearch -exact [package names] "Tk"] != -1} {

	# compute a widget name we can use to create a temporary widget
	set tmpWidget ".__tmp__"
	set count 0
	while {[winfo exists $tmpWidget] == 1} {
	    set tmpWidget ".__tmp__$count"
	    incr count
	}

	# steal options from the listbox
	# we want darn near all options, so we'll go ahead and do
	# them all. No harm done in adding the one or two that we
	# don't use.
	listbox $tmpWidget 
	foreach foo [$tmpWidget configure] {
	    if {[llength $foo] == 5} {
		set option [lindex $foo 1]
		set value [lindex $foo 4]
		option add *Mclistbox.$option $value widgetDefault

		# these options also apply to the individual columns...
		if {[string compare $option "foreground"] == 0  || [string compare $option "background"] == 0  || [string compare $option "font"] == 0} {
		    option add *Mclistbox*MclistboxColumn.$option $value  widgetDefault
		}
	    }
	}
	destroy $tmpWidget

	# steal some options from label widgets; we only want a subset
	# so we'll use a slightly different method. No harm in *not*
	# adding in the one or two that we don't use... :-)
	label $tmpWidget
	foreach option [list Anchor Background Font  Foreground Height Image  ] {
	    set values [$tmpWidget configure -[string tolower $option]]
	    option add *Mclistbox.label$option [lindex $values 3]
	}
	destroy $tmpWidget

	# these are unique to us...
	option add *Mclistbox.columnBorderWidth   0      widgetDefault
	option add *Mclistbox.columnRelief        flat   widgetDefault
	option add *Mclistbox.labelBorderWidth    1      widgetDefault
	option add *Mclistbox.labelRelief         raised widgetDefault
	option add *Mclistbox.labels              1      widgetDefault
	option add *Mclistbox.resizableColumns    1      widgetDefault
	option add *Mclistbox.selectcommand       {}     widgetDefault
	option add *Mclistbox.fillcolumn          {}     widgetDefault

	# column options
	option add *Mclistbox*MclistboxColumn.visible       1      widgetDefault
	option add *Mclistbox*MclistboxColumn.resizable     1      widgetDefault
	option add *Mclistbox*MclistboxColumn.position      end    widgetDefault
	option add *Mclistbox*MclistboxColumn.label         ""     widgetDefault
	option add *Mclistbox*MclistboxColumn.width         0      widgetDefault
	option add *Mclistbox*MclistboxColumn.bitmap        ""     widgetDefault
	option add *Mclistbox*MclistboxColumn.image         ""     widgetDefault
    }

    ######################################################################
    # define the class bindings
    ######################################################################
    
    SetClassBindings

}

}
#############################################################################
## Library Procedure:  ::mclistbox::Insert

namespace eval ::mclistbox {

proc {::mclistbox::Insert} {w index arglist} {

    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    foreach list $arglist {
	# make sure we have enough elements for each column
	for {set i [llength $list]} {$i < [llength $misc(columns)]} {incr i} {
	    lappend list {}
	}

	set column 0
	foreach id $misc(columns) {
	    $widgets(listbox$id) insert $index [lindex $list $column]
	    incr column
	}

	# we also want to add a bogus item to the hidden listbox. Why?
	# For standard listboxes, if you specify a height of zero the
	# listbox will resize to be just big enough to hold all the lines.
	# Since we use a hidden listbox to regulate the size of the widget
	# and we want this same behavior, this listbox needs the same number
	# of elements as the visible listboxes
	#
	# (NB: we might want to make this listbox contain the contents
	# of all columns as a properly formatted list; then the get 
	# command can query this listbox instead of having to query
	# each individual listbox. The disadvantage is that it doubles
	# the memory required to hold all the data)
	$widgets(hiddenListbox) insert $index "x"
    }
    return ""
}

}
#############################################################################
## Library Procedure:  ::mclistbox::InvalidateScrollbars

namespace eval ::mclistbox {

proc {::mclistbox::InvalidateScrollbars} {w} {

    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    if {![info exists misc(afterid)]} {
	set misc(afterid)  [after idle "catch {::mclistbox::UpdateScrollbars $w}"]
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::LabelEvent

namespace eval ::mclistbox {

proc {::mclistbox::LabelEvent} {w id code} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options

    # only fire the binding if the cursor is our default cursor
    # (ie: if we aren't in a "resize zone")
    set cursor [$widgets(label$id) cget -cursor]
    if {[string compare $cursor $options(-cursor)] == 0} {
	uplevel \#0 $code
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::MassageIndex

namespace eval ::mclistbox {

proc {::mclistbox::MassageIndex} {w index} {
    upvar ::mclistbox::${w}::widgets   widgets
    upvar ::mclistbox::${w}::misc      misc

    if {[regexp {@([0-9]+),([0-9]+)} $index matchvar x y]} {
	set id [lindex $misc(columns) 0]
	
	incr y -[winfo y $widgets(listbox$id)]
	incr y -[winfo y $widgets(frame$id)]
	incr x [winfo x $widgets(listbox$id)]
	incr x [winfo x $widgets(frame$id)]

	set index @${x},${y}
    }

    return $index
}

}
#############################################################################
## Library Procedure:  ::mclistbox::NewColumn

namespace eval ::mclistbox {

proc {::mclistbox::NewColumn} {w id} {
    upvar ::mclistbox::${w}::widgets   widgets
    upvar ::mclistbox::${w}::options   options
    upvar ::mclistbox::${w}::misc      misc
    upvar ::mclistbox::${w}::columnID  columnID

    # the columns are all children of the text widget we created... 
    set frame      [frame $w.frame$id  -takefocus 0  -highlightthickness 0  -class MclistboxColumn  -background $options(-background)  ]

    set listbox    [listbox $frame.listbox  -takefocus 0  -bd 0  -setgrid $options(-setgrid)  -exportselection false  -selectmode $options(-selectmode)  -highlightthickness 0  ]

    set label      [label $frame.label  -takefocus 0  -relief raised  -bd 1  -highlightthickness 0  ]

    # define mappings from widgets to columns
    set columnID($label) $id
    set columnID($frame) $id
    set columnID($listbox) $id

    # we're going to associate a new bindtag for the label to
    # handle our resize bindings. Why? We want the bindings to
    # be specific to this widget but we don't want to use the
    # widget name. If we use the widget name then the bindings
    # could get mixed up with user-supplied bindigs (via the 
    # "label bind" command). 
    set tag MclistboxLabel
    bindtags $label  [list MclistboxMouseBindings $label]

    # reconfigure the label based on global options
    foreach option [list bd image height relief font anchor  background foreground borderwidth] {
	if {[info exists options(-label$option)]  && $options(-label$option) != ""} {
	    $label configure -$option $options(-label$option)
	}
    }

    # reconfigure the column based on global options
    foreach option [list borderwidth relief] {
	if {[info exists options(-column$option)]  && $options(-column$option) != ""} {
	    $frame configure -$option $options(-column$option)
	}
    }

    # geometry propagation must be off so we can control the size
    # of the listbox by setting the size of the containing frame
    pack propagate $frame off

    pack $label   -side top -fill x -expand n
    pack $listbox -side top -fill both -expand y -pady 2

    # any events that happen in the listbox gets handled by the class
    # bindings. This has the unfortunate side effect 
    bindtags $listbox [list $w Mclistbox all]

    # return a list of the widgets we created.
    return [list $frame $listbox $label]
}

}
#############################################################################
## Library Procedure:  ::mclistbox::ResizeEvent

namespace eval ::mclistbox {

proc {::mclistbox::ResizeEvent} {w type widget x X Y} {

    upvar ::mclistbox::${w}::widgets       widgets
    upvar ::mclistbox::${w}::options       options
    upvar ::mclistbox::${w}::misc          misc
    upvar ::mclistbox::${w}::columnID      columnID

    # if the widget doesn't allow resizable cursors, there's
    # nothing here to do...
    if {!$options(-resizablecolumns)} {
	return
    }

    # this lets us keep track of some internal state while
    # the user is dragging the mouse
    variable drag

    # this lets us define a small window around the edges of
    # the column. 
    set threshold [expr {$options(-labelborderwidth) + 4}]

    # this is what we use for the "this is resizable" cursor
    set resizeCursor sb_h_double_arrow

    # if we aren't over an area that we care about, bail.
    if {![info exists columnID($widget)]} {
	return
    }

    # id refers to the column name
    set id $columnID($widget)

    switch $type {

	buttonpress {
	    # we will do all the work of initiating a drag only if
	    # the cursor is not the defined cursor. In theory this
	    # will only be the case if the mouse moves over the area
	    # in which a drag can happen.
	    if {[$widgets(label$id) cget -cursor] == $resizeCursor} {
		if {$x <= $threshold} {
		    set lid [::mclistbox::FindResizableNeighbor $w $id left]
		    if {$lid == ""} return
		    set drag(leftFrame)  $widgets(frame$lid)
		    set drag(rightFrame) $widgets(frame$id)

		    set drag(leftListbox)  $widgets(listbox$lid)
		    set drag(rightListbox) $widgets(listbox$id)

		} else {
		    set rid [::mclistbox::FindResizableNeighbor $w $id right]
		    if {$rid == ""} return
		    set drag(leftFrame)  $widgets(frame$id)
		    set drag(rightFrame) $widgets(frame$rid)

		    set drag(leftListbox)  $widgets(listbox$id)
		    set drag(rightListbox) $widgets(listbox$rid)

		}
		

		set drag(leftWidth)  [winfo width $drag(leftFrame)]
		set drag(rightWidth) [winfo width $drag(rightFrame)]

		# it seems to be a fact that windows can never be 
		# less than one pixel wide. So subtract that one pixel
		# from our max deltas...
		set drag(maxDelta)   [expr {$drag(rightWidth) - 1}]
		set drag(minDelta)  -[expr {$drag(leftWidth) - 1}]

		set drag(x) $X
	    }
	}

	motion {
	    if {[info exists drag(x)]} {return}

	    # this is just waaaaay too much work for a motion 
	    # event, IMO.

	    set resizable 0

	    # is the column the user is over resizable?
	    if {!$options($id:-resizable)} {return}

	    # did the user click on the left of a column? 
	    if {$x < $threshold} {
		set leftColumn [::mclistbox::FindResizableNeighbor $w $id left]
		if {$leftColumn != ""} {
		    set resizable 1
		}

	    } elseif {$x > [winfo width $widget] - $threshold} {
		set rightColumn [::mclistbox::FindResizableNeighbor $w $id  right]
		if {$rightColumn != ""} {
		    set resizable 1
		}
	    }

	    # if it's resizable, change the cursor 
	    set cursor [$widgets(label$id) cget -cursor]
	    if {$resizable && $cursor != $resizeCursor} {
		$widgets(label$id) configure -cursor $resizeCursor

	    } elseif {!$resizable && $cursor == $resizeCursor} {
		$widgets(label$id) configure -cursor $options(-cursor)
	    }
	}

	drag {
	    # if the state is set up, do the drag...
	    if {[info exists drag(x)]} {

		set delta [expr {$X - $drag(x)}]
		if {$delta >= $drag(maxDelta)} {
		    set delta $drag(maxDelta)

		} elseif {$delta <= $drag(minDelta)} {
		    set delta $drag(minDelta)
		}

		set lwidth [expr {$drag(leftWidth) + $delta}]
		set rwidth [expr {$drag(rightWidth) - $delta}]

		$drag(leftFrame)   configure -width $lwidth
		$drag(rightFrame)  configure -width $rwidth

	    }
	}

	buttonrelease {
	    set fillColumnID $options(-fillcolumn)
	    if {[info exists drag(x)] && $fillColumnID != {}} {
		set fillColumnFrame $widgets(frame$fillColumnID)
		if {[string compare $drag(leftFrame) $fillColumnFrame] == 0  || [string compare $drag(rightFrame) $fillColumnFrame] == 0} {
		    set width [$fillColumnFrame cget -width]
		    set misc(minFillColumnSize) $width
		}
		set misc(min-$drag(leftFrame))  [$drag(leftFrame) cget -width]
		set misc(min-$drag(rightFrame)) [$drag(rightFrame) cget -width]
	    }

	    # reset the state and the cursor
	    catch {unset drag}
	    $widgets(label$id) configure -cursor $options(-cursor)

	}
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::SelectionHandler

namespace eval ::mclistbox {

proc {::mclistbox::SelectionHandler} {w type {offset {}} {length {}}} {
    upvar ::mclistbox::${w}::options   options
    upvar ::mclistbox::${w}::misc      misc
    upvar ::mclistbox::${w}::widgets   widgets

    switch -exact $type {

	own {
	    selection own  -command [list ::mclistbox::SelectionHandler $w lose]  -selection PRIMARY  $w
	}

	lose {
	    if {$options(-exportselection)} {
		foreach id $misc(columns) {
		    $widgets(listbox$id) selection clear 0 end
		}
	    }
	}

	get {
	    set end [expr {$length + $offset - 1}]

	    set column [lindex $misc(columns) 0]
	    set curselection [$widgets(listbox$column) curselection]

	    # this is really, really slow (relatively speaking).
	    # but the only way I can think of to speed this up
	    # is to duplicate all the data in our hidden listbox,
	    # which I really don't want to do because of memory
	    # considerations.
	    set data ""
	    foreach index $curselection {
		set rowdata [join [::mclistbox::WidgetProc-get $w $index]  "\t"]
		lappend data $rowdata
	    }
	    set data [join $data "\n"]
	    return [string range $data $offset $end]
	}

    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::SetBindings

namespace eval ::mclistbox {

proc {::mclistbox::SetBindings} {w} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    # we must do this so that the columns fill the text widget in
    # the y direction
    bind $widgets(text) <Configure>  [list ::mclistbox::AdjustColumns $w %h]

}

}
#############################################################################
## Library Procedure:  ::mclistbox::SetClassBindings

namespace eval ::mclistbox {

proc {::mclistbox::SetClassBindings} {} {
    # this allows us to clean up some things when we go away
    bind Mclistbox <Destroy> [list ::mclistbox::DestroyHandler %W]

    # steal all of the standard listbox bindings. Note that if a user
    # clicks in a column, %W will return that column. This is bad,
    # so we have to make a substitution in all of the bindings to
    # compute the real widget name (ie: the name of the topmost 
    # frame)
    foreach event [bind Listbox] {
	set binding [bind Listbox $event]
	regsub -all {%W} $binding {[::mclistbox::convert %W -W]} binding
	regsub -all {%x} $binding {[::mclistbox::convert %W -x %x]} binding
	regsub -all {%y} $binding {[::mclistbox::convert %W -y %y]} binding
	bind Mclistbox $event $binding
    }

    # these define bindings for the column labels for resizing. Note
    # that we need both the name of this widget (calculated by $this)
    # as well as the specific widget that the event occured over.
    # Also note that $this is a constant string that gets evaluated
    # when the binding fires.
    # What a pain.
    set this {[::mclistbox::convert %W -W]}
    bind MclistboxMouseBindings <ButtonPress-1>  "::mclistbox::ResizeEvent $this buttonpress %W %x %X %Y"
    bind MclistboxMouseBindings <ButtonRelease-1>  "::mclistbox::ResizeEvent $this buttonrelease %W %x %X %Y"
    bind MclistboxMouseBindings <Enter>  "::mclistbox::ResizeEvent $this motion %W %x %X %Y"
    bind MclistboxMouseBindings <Motion>  "::mclistbox::ResizeEvent $this motion %W %x %X %Y"
    bind MclistboxMouseBindings <B1-Motion>  "::mclistbox::ResizeEvent $this drag %W %x %X %Y"
}

}
#############################################################################
## Library Procedure:  ::mclistbox::UpdateScrollbars

namespace eval ::mclistbox {

proc {::mclistbox::UpdateScrollbars} {w} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    if {![winfo ismapped $w]} {
	catch {unset misc(afterid)}
	return
    }

    update idletasks
    if {[llength $misc(columns)] > 0} {
	if {[string length $options(-yscrollcommand)] != 0} {
	    set col0 [lindex $misc(columns) 0]
	    set yview [$widgets(listbox$col0) yview]
	    uplevel #0 $options(-yscrollcommand) $yview
	}

	if {[string length $options(-xscrollcommand)] != 0} {
	    set col0 [lindex $misc(columns) 0]
	    set xview [$widgets(text) xview]
	    uplevel #0 $options(-xscrollcommand) $xview
	}
    }
    catch {unset misc(afterid)}
}

}
#############################################################################
## Library Procedure:  ::mclistbox::WidgetProc

namespace eval ::mclistbox {

proc {::mclistbox::WidgetProc} {w command args} {
    variable widgetOptions

    upvar ::mclistbox::${w}::widgets   widgets
    upvar ::mclistbox::${w}::options   options
    upvar ::mclistbox::${w}::misc      misc
    upvar ::mclistbox::${w}::columnID  columnID

    set command [::mclistbox::Canonize $w command $command]

    # some commands have subcommands. We'll check for that here 
    # and mung the command and args so that we can treat them as 
    # distinct commands in the following switch statement
    if {[string compare $command "column"] == 0} {
	set subcommand [::mclistbox::Canonize $w "column command"  [lindex $args 0]]
	set command "$command-$subcommand"
	set args [lrange $args 1 end]

    } elseif {[string compare $command "label"] == 0} {
	set subcommand [::mclistbox::Canonize $w "label command"  [lindex $args 0]]
	set command "$command-$subcommand"
	set args [lrange $args 1 end]
    }

    set result ""
    catch {unset priorSelection}

    # here we go. Error checking be damned!
    switch $command {
	xview {
	    # note that at present, "xview <index>" is broken. I'm
	    # not even sure how to do it. Unless I attach our hidden
	    # listbox to the scrollbar and use it. Hmmm..... I'll
	    # try that later. (FIXME)
	    set result [eval {$widgets(text)} xview $args]
	    InvalidateScrollbars $w
	}

	yview {
	    if {[llength $args] == 0} {
		# length of zero means to fetch the yview; we can
		# get this from a single listbox
		set result [$widgets(hiddenListbox) yview]

	    } else {

		# if it's one argument, it's an index. We'll pass that 
		# index through the index command to properly translate
		# @x,y indicies, and place the value back in args
		if {[llength $args] == 1} {
		    set index [::mclistbox::MassageIndex $w [lindex $args 0]]
		    set args [list $index]
		}

		# run the yview command on every column.
		foreach id $misc(columns) {
		    eval {$widgets(listbox$id)} yview $args
		}
		eval {$widgets(hiddenListbox)} yview $args

		InvalidateScrollbars $w

		set result ""
	    }
	}

	activate {
	    if {[llength $args] != 1} {
		return -code error "wrong \# of args: should be $w activate index"
	    }
	    set index [::mclistbox::MassageIndex $w [lindex $args 0]]

	    foreach id $misc(columns) {
		$widgets(listbox$id) activate $index
	    }
	    set result ""
	}

	bbox {
	    if {[llength $args] != 1} {
		return -code error "wrong \# of args: should be $w bbox index"
	    }
	    # get a real index. This will adjust @x,y indicies
	    # to account for the label, if any.
	    set index [::mclistbox::MassageIndex $w [lindex $args 0]]

	    set id [lindex $misc(columns) 0]

	    # we can get the x, y, and height from the first 
	    # column.
	    set bbox [$widgets(listbox$id) bbox $index]
	    if {[string length $bbox] == 0} {return ""}

	    foreach {x y w h} $bbox {}
	    
	    # the x and y coordinates have to be adjusted for the
	    # fact that the listbox is inside a frame, and the 
	    # frame is inside a text widget. All of those add tiny
	    # offsets. Feh.
	    incr y [winfo y $widgets(listbox$id)]
	    incr y [winfo y $widgets(frame$id)]
	    incr x [winfo x $widgets(listbox$id)]
	    incr x [winfo x $widgets(frame$id)]

	    # we can get the width by looking at the relative x 
	    # coordinate of the right edge of the last column
	    set id [lindex $misc(columns) end]
	    set w [expr {[winfo width $widgets(frame$id)] +  [winfo x $widgets(frame$id)]}]
	    set bbox [list $x $y [expr {$x + $w}] $h]
	    set result $bbox
	}

	label-bind {
	    # we are just too clever for our own good. (that's a 
	    # polite way of saying this is more complex than it
	    # needs to be)

	    set id [lindex $args 0]
	    set index [CheckColumnID $w $id]

	    set args [lrange $args 1 end]
	    if {[llength $args] == 0} {
		set result [bind $widgets(label$id)]
	    } else {

		# when we create a binding, we'll actually have the 
		# binding run our own command with the user's command
		# as an argument. This way we can do some sanity checks
		# before running the command. So, when querying a binding
		# we need to only return the user's code
		set sequence [lindex $args 0]
		if {[llength $args] == 1} {
		    set result [lindex [bind $widgets(label$id) $sequence] end]
		} else {
		
		    # replace %W with our toplevel frame, then
		    # do the binding
		    set code [lindex $args 1]
		    regsub -all {%W} $code $w code
		    
		    set result [bind $widgets(label$id) $sequence  [list ::mclistbox::LabelEvent $w $id $code]]
		}
	    }
	}

	column-add {
	    eval ::mclistbox::Column-add {$w} $args
	    AdjustColumns $w
	    set result ""
	}

	column-delete {
	    foreach id $args {
		set index [CheckColumnID $w $id]

		# remove the reference from our list of columns
		set misc(columns) [lreplace $misc(columns) $index $index]

		# whack the widget
		destroy $widgets(frame$id)

		# clear out references to the individual widgets
		unset widgets(frame$id)
		unset widgets(listbox$id)
		unset widgets(label$id)
	    }
	    InvalidateScrollbars $w
	    set result ""
	}

	column-cget {
	    if {[llength $args] != 2} {
		return -code error "wrong # of args: should be \"$w column cget name option\""
	    }
	    set id [::mclistbox::Canonize $w column [lindex $args 0]]
	    set option [lindex $args 1]
	    set data [::mclistbox::Column-configure $w $id $option]
	    set result [lindex $data 4]
	}

	column-configure {
	    set id [::mclistbox::Canonize $w column [lindex $args 0]]
	    set args [lrange $args 1 end]
	    set result [eval ::mclistbox::Column-configure {$w} {$id} $args]
	}

	column-names {
	    if {[llength $args] != 0} {
		return -code error "wrong # of args: should be \"$w column names\""
	    }
	    set result $misc(columns)
	}

	column-nearest {
	    if {[llength $args] != 1} {
		return -code error "wrong # of args: should be \"$w column nearest x\""
	    }

	    set x [lindex $args 0]
	    set tmp [$widgets(text) index @$x,0]
	    set tmp [split $tmp "."]
	    set index [lindex $tmp 1]

	    set result [lindex $misc(columns) $index]
	}

	cget {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be $w cget option"
	    }
	    set opt [::mclistbox::Canonize $w option [lindex $args 0]]

	    set result $options($opt)
	}


	configure {
	    set result [eval ::mclistbox::Configure {$w} $args]

	}

	curselection {
	    set id [lindex $misc(columns) 0]
	    set result [$widgets(listbox$id) curselection]
	}

	delete {
	    if {[llength $args] < 1 || [llength $args] > 2} {
		return -code error "wrong \# of args: should be $w delete first ?last?"
	    }

	    # it's possible that the selection will change because
	    # of something we do. So, grab the current selection before
	    # we do anything. Just before returning we'll see if the
	    # selection has changed. If so, we'll call our selectcommand
	    if {$options(-selectcommand) != ""} {
		set col0 [lindex $misc(columns) 0]
		set priorSelection [$widgets(listbox$col0) curselection]
	    }

	    set index1 [::mclistbox::MassageIndex $w [lindex $args 0]]
	    if {[llength $args] == 2} {
		set index2 [::mclistbox::MassageIndex $w [lindex $args 1]]
	    } else {
		set index2 ""
	    }

	    # note we do an eval here to make index2 "disappear" if it
	    # is set to an empty string.
	    foreach id $misc(columns) {
		eval {$widgets(listbox$id)} delete $index1 $index2
	    }
	    eval {$widgets(hiddenListbox)} delete $index1 $index2

	    InvalidateScrollbars $w

	    set result ""
	}

	get {
	    if {[llength $args] < 1 || [llength $args] > 2} {
		return -code error "wrong \# of args: should be $w get first ?last?"
	    }
	    set index1 [::mclistbox::MassageIndex $w [lindex $args 0]]
	    if {[llength $args] == 2} {
		set index2 [::mclistbox::MassageIndex $w [lindex $args 1]]
	    } else {
		set index2 ""
	    }

	    set result [eval ::mclistbox::WidgetProc-get {$w} $index1 $index2]

	}

	index {

	    if {[llength $args] != 1} {
		return -code error "wrong \# of args: should be $w index index"
	    }

	    set index [::mclistbox::MassageIndex $w [lindex $args 0]]
	    set id [lindex $misc(columns) 0]

	    set result [$widgets(listbox$id) index $index]
	}

	insert {
	    if {[llength $args] < 1} {
		return -code error "wrong \# of args: should be $w insert ?element  element...?"
	    }

	    # it's possible that the selection will change because
	    # of something we do. So, grab the current selection before
	    # we do anything. Just before returning we'll see if the
	    # selection has changed. If so, we'll call our selectcommand
	    if {$options(-selectcommand) != ""} {
		set col0 [lindex $misc(columns) 0]
		set priorSelection [$widgets(listbox$col0) curselection]
	    }

	    set index [::mclistbox::MassageIndex $w [lindex $args 0]]

	    ::mclistbox::Insert $w $index [lrange $args 1 end]

	    InvalidateScrollbars $w
	    set result ""
	}

	nearest {
	    if {[llength $args] != 1} {
		return -code error "wrong \# of args: should be $w nearest y"
	    }

	    # translate the y coordinate into listbox space
	    set id [lindex $misc(columns) 0]
	    set y [lindex $args 0]
	    incr y -[winfo y $widgets(listbox$id)]
	    incr y -[winfo y $widgets(frame$id)]

	    set col0 [lindex $misc(columns) 0]

	    set result [$widgets(listbox$col0) nearest $y]
	}

	scan {
	    foreach {subcommand x y} $args {}
	    switch $subcommand {
		mark {
		    # we have to treat scrolling in x and y differently;
		    # scrolling in the y direction affects listboxes and
		    # scrolling in the x direction affects the text widget.
		    # to facilitate that, we need to keep a local copy
		    # of the scan mark.
		    set misc(scanmarkx) $x
		    set misc(scanmarky) $y
		    
		    # set the scan mark for each column
		    foreach id $misc(columns) {
			$widgets(listbox$id) scan mark $x $y
		    }

		    # we can't use the x coordinate given us, since it 
		    # is relative to whatever column we are over. So,
		    # we'll just usr the results of [winfo pointerx].
		    $widgets(text) scan mark [winfo pointerx $w]  $y
		}
		dragto {
		    # we want the columns to only scan in the y direction,
		    # so we'll force the x componant to remain constant
		    foreach id $misc(columns) {
			$widgets(listbox$id) scan dragto $misc(scanmarkx) $y
		    }

		    # since the scan mark of the text widget was based
		    # on the pointer location, so must be the x
		    # coordinate to the dragto command. And since we
		    # want the text widget to only scan in the x
		    # direction, the y componant will remain constant
		    $widgets(text) scan dragto  [winfo pointerx $w] $misc(scanmarky)

		    # make sure the scrollbars reflect the changes.
		    InvalidateScrollbars $w
		}

		set result ""
	    }
	}

	see {
	    if {[llength $args] != 1} {
		return -code error "wrong \# of args: should be $w see index"
	    }
	    set index [::mclistbox::MassageIndex $w [lindex $args 0]]

	    foreach id $misc(columns) {
		$widgets(listbox$id) see $index
	    }
	    InvalidateScrollbars $w
	    set result {}
	}

	selection {
	    # it's possible that the selection will change because
	    # of something we do. So, grab the current selection before
	    # we do anything. Just before returning we'll see if the
	    # selection has changed. If so, we'll call our selectcommand
	    if {$options(-selectcommand) != ""} {
		set col0 [lindex $misc(columns) 0]
		set priorSelection [$widgets(listbox$col0) curselection]
	    }

	    set subcommand [lindex $args 0]
	    set args [lrange $args 1 end]

	    set prefix "wrong \# of args: should be $w"
	    switch $subcommand {
		includes {
		    if {[llength $args] != 1} {
			return -code error "$prefix selection $subcommand index"
		    }
		    set index [::mclistbox::MassageIndex $w [lindex $args 0]]
		    set id [lindex $misc(columns) 0]
		    set result [$widgets(listbox$id) selection includes $index]
		}

		set {
		    switch [llength $args] {
			1 {
			    set index1 [::mclistbox::MassageIndex $w  [lindex $args 0]]
			    set index2 ""
			}
			2 {
			    set index1 [::mclistbox::MassageIndex $w  [lindex $args 0]]
			    set index2 [::mclistbox::MassageIndex $w  [lindex $args 1]]
			}
			default {
			    return -code error "$prefix selection clear first ?last?"
			}
		    }

		    if {$options(-exportselection)} {
			SelectionHandler $w own
		    }
		    if {$index1 != ""} {
			foreach id $misc(columns) {
			    eval {$widgets(listbox$id)} selection set  $index1 $index2
			}
		    }

		    set result ""
		}

		anchor {
		    if {[llength $args] != 1} {
			return -code error "$prefix selection $subcommand index"
		    }
		    set index [::mclistbox::MassageIndex $w [lindex $args 0]]

		    if {$options(-exportselection)} {
			SelectionHandler $w own
		    }
		    foreach id $misc(columns) {
			$widgets(listbox$id) selection anchor $index
		    }
		    set result ""
		}

		clear {
		    switch [llength $args] {
			1 {
			    set index1 [::mclistbox::MassageIndex $w  [lindex $args 0]]
			    set index2 ""
			}
			2 {
			    set index1 [::mclistbox::MassageIndex $w  [lindex $args 0]]
			    set index2 [::mclistbox::MassageIndex $w  [lindex $args 1]]
			}
			default {
			    return -code error "$prefix selection clear first ?last?"
			}
		    }

		    if {$options(-exportselection)} {
			SelectionHandler $w own
		    }
		    foreach id $misc(columns) {
			eval {$widgets(listbox$id)} selection clear  $index1 $index2
		    }
		    set result ""
		}
	    }
	}

	size {
	    set id [lindex $misc(columns) 0]
	    set result [$widgets(listbox$id) size]
	}
    }

    # if the user has a selectcommand defined and the selection changed,
    # run the selectcommand
    if {[info exists priorSelection] && $options(-selectcommand) != ""} {
	set column [lindex $misc(columns) 0]
	set currentSelection [$widgets(listbox$column) curselection]
	if {[string compare $priorSelection $currentSelection] != 0} {
	    # this logic keeps us from getting into some sort of
	    # infinite loop of the selectcommand changes the selection
	    # (not particularly well tested, but it seems like the
	    # right thing to do...)
	    if {![info exists misc(skipRecursiveCall)]} {
		set misc(skipRecursiveCall) 1
		uplevel \#0 $options(-selectcommand) $currentSelection
		catch {unset misc(skipRecursiveCall)}
	    }
	}
    }

    return $result
}

}
#############################################################################
## Library Procedure:  ::mclistbox::WidgetProc-get

namespace eval ::mclistbox {

proc {::mclistbox::WidgetProc-get} {w args} {
    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    set returnType "list"
    # the listbox "get" command returns different things
    # depending on whether it has one or two args. Internally
    # we *always* want a valid list, so we'll force a second
    # arg which in turn forces the listbox to return a list,
    # even if its a list of one element
    if {[llength $args] == 1} {
	lappend args [lindex $args 0]
	set returnType "listOfLists"
    }

    # get all the data from each column
    foreach id $misc(columns) {
	set data($id) [eval {$widgets(listbox$id)} get $args]
    }

    # now join the data together one row at a time. Ugh.
    set result {}
    set rows [llength $data($id)]
    for {set i 0} {$i < $rows} {incr i} {
	set this {}
	foreach column $misc(columns) {
	    lappend this [lindex $data($column) $i]
	}
	lappend result $this
    }
    
    # now to unroll the list if necessary. If the user gave
    # us only one indicie we want to return a single list
    # of values. If they gave use two indicies we want to return
    # a list of lists.
    if {[string compare $returnType "list"] == 0} {
	return $result
    } else {
	return [lindex $result 0]
    }
}

}
#############################################################################
## Library Procedure:  ::mclistbox::convert

namespace eval ::mclistbox {

proc {::mclistbox::convert} {w args} {
    set result {}
    if {![winfo exists $w]} {
	return -code error "window \"$w\" doesn't exist"
    }

    while {[llength $args] > 0} {
	set option [lindex $args 0]
	set args [lrange $args 1 end]

	switch -exact -- $option {
	    -x {
		set value [lindex $args 0]
		set args [lrange $args 1 end]
		set win $w
		while {[winfo class $win] != "Mclistbox"} {
		    incr value [winfo x $win]
		    set win [winfo parent $win]
		    if {$win == "."} break
		}
		lappend result $value
	    }

	    -y {
		set value [lindex $args 0]
		set args [lrange $args 1 end]
		set win $w
		while {[winfo class $win] != "Mclistbox"} {
		    incr value [winfo y $win]
		    set win [winfo parent $win]
		    if {$win == "."} break
		}
		lappend result $value
	    }

	    -w -
	    -W {
		set win $w
		while {[winfo class $win] != "Mclistbox"} {
		    set win [winfo parent $win]
		    if {$win == "."} break;
		}
		lappend result $win
	    }
	}
    }
    return $result
}

}
#############################################################################
## Library Procedure:  ::mclistbox::mclistbox

namespace eval ::mclistbox {

proc {::mclistbox::mclistbox} {args} {
    variable widgetOptions

    # perform a one time initialization
    if {![info exists widgetOptions]} {
      __mclistbox_Setup
	Init
    }

    # make sure we at least have a widget name
    if {[llength $args] == 0} {
	return -code error "wrong # args: should be \"mclistbox pathName ?options?\""
    }

    # ... and make sure a widget doesn't already exist by that name
    if {[winfo exists [lindex $args 0]]} {
	return -code error "window name \"[lindex $args 0]\" already exists"
    }

    # and check that all of the args are valid
    foreach {name value} [lrange $args 1 end] {
	Canonize [lindex $args 0] option $name
    }

    # build it...
    set w [eval Build $args]

    # set some bindings...
    SetBindings $w

    # and we are done!
    return $w
}

}
#############################################################################
## Library Procedure:  __mclistbox_Setup

proc {__mclistbox_Setup} {} {

    namespace eval ::mclistbox {

        # this is the public interface
        namespace export mclistbox

        # these contain references to available options
        variable widgetOptions
        variable columnOptions

        # these contain references to available commands and subcommands
        variable widgetCommands
        variable columnCommands
        variable labelCommands
    }
}
#############################################################################
## Library Procedure:  vTcl:DefineAlias

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
#############################################################################
## Library Procedure:  vTcl:DoCmdOption

proc {vTcl:DoCmdOption} {target cmd} {

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

proc {vTcl:FireEvent} {target event {params {}}} {
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

proc {vTcl:Toplevel:WidgetProc} {w args} {
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
    switch -- $command {
        "hide" - "Hide" - "show" - "Show" {
            Window [string tolower $command] $w
        }
        "ShowModal" {
            Window show $w
            raise $w
            grab $w
            tkwait window $w
            grab release $w
        }
        default {
            uplevel $w $command $args
        }
    }
}
#############################################################################
## Library Procedure:  vTcl:WidgetProc

proc {vTcl:WidgetProc} {w args} {
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

proc {vTcl:toplevel} {args} {
    uplevel #0 eval toplevel $args
    set target [lindex $args 0]
    namespace eval ::$target {}
}
}


if {[info exists vTcl(sourcing)]} {

proc vTcl:project:info {} {
    namespace eval ::widgets::.top75 {
        array set save {-background 1 -menu 1}
        set set,origin 1
        set set,size 1
    }
    namespace eval ::widgets::.top75.fra77 {
        array set save {-borderwidth 1 -height 1 -relief 1 -width 1}
    }
    namespace eval ::widgets::.top75.fra77.but78 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.ent79 {
        array set save {-_tooltip 1 -background 1 -font 1 -foreground 1 -justify 1 -textvariable 1 -validate 1 -vcmd 1}
    }
    namespace eval ::widgets::.top75.fra77.but80 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but86 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but87 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but81 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but77 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but82 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but83 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but84 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but85 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.but76 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.fra77.lab77 {
        array set save {-width 1}
    }
    namespace eval ::widgets::.top75.fra77.button77 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::.top75.tab88 {
        array set save {-angle 1 -bevelamount 1 -raiseselect 1 -tabborders 1 -tabforeground 1 -tabpos 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -width 1}
        }
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab90 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab90] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab90] 0].but92 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab90] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94] 0].but95 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94] 0].lab96 {
        array set save {-activebackground 1 -font 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94] 0].but97 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94] 0].but98 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab94] 0].but99 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab100 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab100] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab100] 0].but92 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab100] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab100] 0].but101 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab102 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab102] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab102] 0].but92 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab102] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab102] 0].but101 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab103 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab103] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab103] 0].but92 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab103] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 0].lab103] 0].but101 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab104 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab104] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab104] 0].but95 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab104] 0].but97 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab105 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab105] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab105] 0].but92 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab105] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab106 {
        array set save {-background 1 -ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab106] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab106] 0].but92 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab106] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab107 {
        array set save {-background 1 -ipadx 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab107] 0] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab107] 0].lab108 {
        array set save {-font 1 -foreground 1 -justify 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 1].lab107] 0].but93 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].scr82 {
        array set save {-background 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84 {
        array set save {-borderwidth 1 -height 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.ent85 {
        array set save {-background 1 -command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.fra90 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.fra90.men91 {
        array set save {-_tooltip 1 -disabledforeground 1 -font 1 -indicatoron 1 -menu 1 -padx 1 -pady 1 -relief 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.fra90.men91.m {
        array set save {-activebackground 1 -activeforeground 1 -disabledforeground 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.fra90.ent92 {
        array set save {-background 1 -insertbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.fra90.lab95 {
        array set save {-activebackground 1 -disabledforeground 1 -font 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.but96 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.but88 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra84.but89 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra76 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra76.lab81 {
        array set save {-_tooltip 1 -anchor 1 -font 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra76.mcl78 {
        array set save {-background 1 -font 1 -height 1 -labelfont 1 -selectborderwidth 1 -selectcommand 1 -selectmode 1 -width 1 -yscrollcommand 1}
        namespace eval subOptions {
            array set save {-background 1 -font 1 -label 1 -labelrelief 1 -resizable 1 -visible 1 -width 1}
        }
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra76.scr80 {
        array set save {-command 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra79 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra79.ent80 {
        array set save {-background 1 -labelfont 1 -labelpos 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra79.ent82 {
        array set save {-background 1 -justify 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra79.ent83 {
        array set save {-background 1 -justify 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra79.but81 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 2].fra79.com77 {
        array set save {-background 1 -command 1 -justify 1 -labelfont 1 -labelpos 1 -labeltext 1 -selectioncommand 1 -textbackground 1 -textvariable 1 -unique 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].lab111 {
        array set save {-font 1 -justify 1 -padx 1 -pady 1 -relief 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra112 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra112.but113 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra112.but114 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra112.but115 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra117 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra117.button78 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra117.but113 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].fra117.but114 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].frame77 {
        array set save {-borderwidth 1 -height 1 -highlightbackground 1 -highlightcolor 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].frame77.but115 {
        array set save {-activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -image 1 -padx 1 -relief 1 -state 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 3].frame77.but116 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4] {
        array set save {-highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].che119 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].che120 {
        array set save {-labelfont 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -padx 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].fra122 {
        array set save {-height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].fra122.but123 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].fra122.but124 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].fra122.but125 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].scr77 {
        array set save {-background 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].ent78 {
        array set save {-background 1 -command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].lab81 {
        array set save {-background 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].lab81] 0] {
        array set save {-highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].lab81] 0].ent82 {
        array set save {-background 1 -insertbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].lab81] 0].but83 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -padx 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].lab81] 0].but84 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 4].lab81] 0].but85 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5] {
        array set save {-highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].scr77 {
        array set save {-background 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].scr78 {
        array set save {-background 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.ent79 {
        array set save {-background 1 -command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.ent80 {
        array set save {-background 1 -command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.lab81 {
        array set save {-background 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.lab81] 0] {
        array set save {-highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.lab81] 0].ent82 {
        array set save {-background 1 -insertbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.lab81] 0].but84 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra78.lab81] 0].but85 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra82 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra82.but84 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 5].fra82.but86 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6] {
        array set save {-background 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83 {
        array set save {-highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0] {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra86 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra86.scr87 {
        array set save {-hscrollmode 1 -labelpos 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -visibleitems 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra86.but88 {
        array set save {-activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra86.but76 {
        array set save {-activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra89 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra89.scr87 {
        array set save {-hscrollmode 1 -labelfont 1 -labelpos 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -visibleitems 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra89.but88 {
        array set save {-command 1 -font 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra89.ent90 {
        array set save {-_tooltip 1 -background 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra76 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra76.but77 {
        array set save {-activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra76.ent77 {
        array set save {-justify 1 -labelfont 1 -labelpos 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra76.but78 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra76.but79 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].fra76.but76 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].opt86 {
        array set save {-command 1 -font 1 -labelfont 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].che79 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].che76 {
        array set save {-labelfont 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab76 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab76] 0] {
        array set save {}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab76] 0].ent77 {
        array set save {-_tooltip 1 -background 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab76] 0].but78 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab79 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab79] 0] {
        array set save {}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab79] 0].ent77 {
        array set save {-_tooltip 1 -background 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 6].fra83.lab84] 0].lab79] 0].but78 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7] {
        array set save {-background 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84 {
        array set save {-highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.scrolledlistbox83 {
        array set save {-background 1 -cursor 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -textbackground 1 -textfont 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.fra79 {
        array set save {-borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.fra79.but80 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.fra79.but81 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.fra79.but79 {
        array set save {-_tooltip 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.lab80 {
        array set save {-labelfont 1 -labeltext 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.lab80] 0] {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.lab80] 0].but81 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.lab80] 0].but82 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::[lindex [vTcl:widget:labeledframe:treeChildrenChildsite [lindex [vTcl::widgets::iwidgets::tabnotebook::treeChildrenChildsite .top75.tab88] 7].fra84.lab80] 0].but83 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -command 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::.top75.m88 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -cursor 1 -foreground 1}
    }
    namespace eval ::widgets::.top75.m88.menu89 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu90 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu90.menu97 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu92 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu93 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu94 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu95 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.menu96 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.men77 {
        array set save {-disabledforeground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m88.men78 {
        array set save {-disabledforeground 1 -tearoff 1}
    }
    namespace eval ::widgets::.top75.m76 {
        array set save {-disabledforeground 1 -tearoff 1}
    }
    namespace eval ::widgets_bindings {
        set tagslist _vTclBalloon
    }
}
}

#################################
# USER DEFINED PROCEDURES
#
#############################################################################
## Procedure:  ArchiveList

proc {ArchiveList} {archdir stablist mulstname} {
global widget

#foreach el $stablist {
#   if {$el == ""} continue
#   set elspl [split $el -]
#   set Ztarget [lindex $elspl 0]
#   set Starget [lindex $elspl 1]
#   set Atarget [lindex $elspl 2]
#   set mulinputn za[expr $Ztarget*1000+$Atarget]$mulstname
#   exec xterm -e store $archdir $mulinputn
#}
exec xterm -e storemul $archdir/ $mulstname
if {$mulstname == ""} {
    exec mv default.inp $archdir/
    } else {
    exec mv $mulstname.inp $archdir/
}
adjourn .top75
}
#############################################################################
## Procedure:  ViewAll

proc {ViewAll} {} {
   global widget editor stablist mulstname
   
   set what [Optionmenu3 get]
   if {$what == "View:"} return 

   set suf(View:) " "
   set suf(inputs) .inp
   set suf(warnings) .war
   set suf(PLOTC4-log) -log.plotc4
   set suf(X4TOC4-log) -log.x4toc4
   set suf(EMPEND-log) -log.empend
   set suf(int-omp) -omp.int
   set suf(dir-omp) -omp.dir
   set suf(RIPL-omp) -omp.ripl
   set suf(levels) .lev
   set suf(coll-levels) -lev.col
   set suf(EXFORs) .exf
   set suf(C4s) .c4
   set suf(ENDFs) .endf
   set suf(short-outputs) .out
   set suf(full-outputs) .lst
   set suf(PLOTC4-plots) .ps
   

   
   foreach el $stablist {
      if {$el == ""} continue
      set elspl [split $el -]
      set Ztarget [lindex $elspl 0]
      set Starget [lindex $elspl 1]
      set Atarget [lindex $elspl 2]
      set mulinputn za[expr $Ztarget*1000+$Atarget]$mulstname$suf($what)
      if {[file exists $mulinputn] == 0} continue
      if {$what == "PLOTC4-plots"} {
         exec gv -landscape $mulinputn
      } else {exec $editor $mulinputn
      }
   }
   Optionmenu3 select "View:"
}
#############################################################################
## Procedure:  adjourn

proc {adjourn} {w} {
global widget filelist archdirlist archfilelist zvvplots profilter zvfilter archfilter archdir

# list of zvv plots 
set zvvplots [glob -nocomplain *$zvfilter*.zvd]

# file list
set filetmp [glob -nocomplain *$profilter*]
set filelist ""
foreach el $filetmp {
  if {[file isfile $el] == 1} {
  lappend filelist $el
 }
}

# list of archive directories
set archdirlist [glob -nocomplain */]

#list of files in the selected archive directory
set archfiletmp [glob -nocomplain $archdir/*$archfilter*]
set archfilelist ""
foreach el $archfiletmp {
  if {[file isfile $el] == 1} {
  lappend archfilelist [file tail $el]
 }
}
}
#############################################################################
## Procedure:  ddlist

proc {ddlist} {} {
global widget file


   Mclistbox1 delete 0 end
   if {[file exists $file-log.plotc4] == 0} return
   set plotc4log [open $file-log.plotc4 r]
   while {[gets $plotc4log line] >= 0} {
      if [regexp MATERIAL $line] break
      }
   gets $plotc4log line
   gets $plotc4log line
   while {[gets $plotc4log line] >= 0} {
      if [regexp ==== $line] {break}
   set num [string range $line 73 75]
   set ej  [string range $line 13 16]
   set mff [string range $line 18 20]
   set mtt [string range $line 22 25]
   set ein [string range $line 45 53]
   set ang [string range $line 56 58]
   set elv [string range $line 63 72]
   if { $ej == " 0" } {set ejc g     
   } elseif {$ej == "   1"} {set ejc n 
   } elseif {$ej == "1001"} {set ejc p
   } elseif {$ej == "2004"} {set ejc a
   } else {set ejc ""
   } 
   if { $mtt == "   2" } {set mt ELAS    
   } elseif {$mtt == "   4"} {set mt INEL
   } elseif {$mtt == "  51"} {set mt INEL
   } elseif {$mtt >= 9000} {set mt PROD
   } else {set mt $mtt
   } 
   if { $mff == "  3" } {set mf XS    
   } elseif {$mff == "  4"} {set mf DA 
   } elseif {$mff == " 5"} {set mf DE
   } elseif {$mff == "  6"} {set mf DD
   } else {set mf $mff
   } 
   if { $mff == "  3" } {set ejc " "}
   Mclistbox1 insert end [list $num  $mf $ejc $mt $ein $elv $ang "#" ]
   }
   close $plotc4log
}
#############################################################################
## Procedure:  fileDialog

proc {fileDialog} {w} {
global widget
global file zvfilter zvvplots profilter filelist archfilter 

    #   Type names		Extension(s)	Mac File Type(s)
    #
    #---------------------------------------------------------
    set types {
	{"Input Files"		{.inp}		}
	{"All   Files"		{*}		}
    }
     
set defile [tk_getOpenFile -filetypes $types  -parent $w -title "Select project input file"]
set dfile [file rootname $defile]
set file [file tail $dfile]
set zvfilter $file
set profilter $file
set archfilter $file 
Combobox1 clear
# create list of possible ddx plots 
ddlist
}
#############################################################################
## Procedure:  readabun

proc {readabun} {nucfile} {
global widget

Scrolledlistbox2 delete 0 end
if {[file exists $nucfile] == 0} return
set abundance [open $nucfile r]
gets $abundance line
gets $abundance line
gets $abundance line
gets $abundance line
while {[gets $abundance line] >= 0} {
set Zab [string trim [string range $line 1 3]]
set Aab [string trim [string range $line 5 7]]
set Sab [string trim [string range $line 9 10]]
set abu [string trim [string range $line 11 18]]
set el $Zab-$Sab-$Aab=($abu%)
Scrolledlistbox2 insert end $el
}
close $abundance
}
#############################################################################
## Procedure:  runlist

proc {runlist} {stablist mulstname} {
global widget ckmlo ckmsh ckmlog ckmendf ckmplots ckmx4 ckmc4 ckmintomp ckmriplomp  ckmdiromp ckmlev ckmcollev ckminp
set checkept $ckmlo$ckmsh$ckmendf$ckmplots
if {$checkept == ".lst.out*.endf.ps"} return
foreach el $stablist {
   if {$el == ""} continue
   set elspl [split $el -]
   set Ztarget [lindex $elspl 0]
   set Starget [lindex $elspl 1]
   set Atarget [lindex $elspl 2]
   set mulinputn za[expr $Ztarget*1000+$Atarget]$mulstname
   set skelinp [open skel.inp r]
   set mulinput [open $mulinputn.inp w]
   while {[gets $skelinp line] >=0} {
      if [regexp xxx $line] {
         puts $mulinput [format "%5.1f %5.1f          ;TARGET A, Z" $Atarget $Ztarget]
      } else {puts $mulinput $line
      }
   }
   close $skelinp
   close $mulinput
   exec xterm -e run $mulinputn
   set delistmul ""
   lappend delistmul $ckmlo $ckmsh $ckmlog $ckmendf  $ckmplots $ckmx4 $ckmc4  $ckmintomp  $ckmriplomp  $ckmdiromp  $ckmlev  $ckmcollev $ckminp
   foreach el $delistmul {
   if {$el == ""} continue
   eval exec cleansel $mulinputn $el
      if {$el == $ckmlog} {
         exec rm -f $mulinputn.x42c4_errs
         exec rm -f $mulinputn.x42c4_lst
         exec rm -f $mulinputn.war
      }
   }
}
if {$mulstname == ""} {
    exec cp skel.inp default.inp
    } else {
    exec cp skel.inp $mulstname.inp
    }
}
#############################################################################
## Procedure:  setmulpro

proc {setmulpro} {selmulitem mulstname} {
global widget file profilter zvfilter archfilter

   if {$selmulitem == ""} return
   set elspl [split $selmulitem -]
   set Ztarget [lindex $elspl 0]
   set Starget [lindex $elspl 1]
   set Atarget [lindex $elspl 2]
   set file za[expr $Ztarget*1000+$Atarget]$mulstname
   set profilter $file
   set zvfilter $file
   set archfilter $file
   adjourn .top75
}
#############################################################################
## Procedure:  main

proc {main} {argc argv} {

}

#############################################################################
## Initialization Procedure:  init

proc {init} {argc argv} {
global editor modules zvvplots filelist archdirlist nsh eres


set nsh 1
set eres 0.02
set editor gvim
set modules [list main.f HF-comp.f  HRTW-comp.f MSC-NVWY.f MSD-orion.f MSD-tristan.f \
                  OM-scat2.f  auxiliary.f bar_mom.f ccfus.f  degas.f  ddhms.f \
                  fusion.f gamma-strgth.f input.f   lev-dens.f  ph-lev-dens.f pipe.f \
                  print.f  scnd-preeq.f  tl.f dimension.h   global.h  io.h ddhms.cmb]
set zvvplots [glob -nocomplain *.zvd]
set filelist [glob -nocomplain *.inp]
set archdirlist [glob -nocomplain */]
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
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0; update
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vtcl.tcl"
    bindtags $base "$base Vtcl.tcl all"
    vTcl:FireEvent $base <<Create>>
    wm protocol $base WM_DELETE_WINDOW "vTcl:FireEvent $base <<DeleteWindow>>"

    ###################
    # SETTING GEOMETRY
    ###################

    vTcl:FireEvent $base <<Ready>>
}

proc vTclWindow.top75 {base} {
    if {$base == ""} {
        set base .top75
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }

    global widget
    vTcl:DefineAlias "$base" "Toplevel1" vTcl:Toplevel:WidgetProc "" 1
    vTcl:DefineAlias "$base.fra77" "Frame2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but76" "Button132" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but77" "Button133" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but78" "Button1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but80" "Button2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but81" "Button3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but82" "Button4" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but83" "Button5" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but84" "Button6" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but85" "Button7" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but86" "Button8" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.but87" "Button9" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.ent79" "Entry1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.fra77.lab77" "Label9" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88" "Tabnotebook1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab100" "Labeledframe3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab100.childsite.but101" "Button21" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab100.childsite.but92" "Button19" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab100.childsite.but93" "Button20" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab102" "Labeledframe4" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab102.childsite.but101" "Button24" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab102.childsite.but92" "Button22" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab102.childsite.but93" "Button23" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab103" "Labeledframe5" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab103.childsite.but101" "Button27" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab103.childsite.but92" "Button25" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab103.childsite.but93" "Button26" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab90" "Labeledframe1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab90.childsite.but92" "Button13" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab90.childsite.but93" "Button14" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab94" "Labeledframe2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab94.childsite.but95" "Button15" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab94.childsite.but97" "Button16" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab94.childsite.but98" "Button17" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab94.childsite.but99" "Button18" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page1.cs.lab94.childsite.lab96" "Label1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab104" "Labeledframe6" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab104.childsite.but95" "Button28" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab104.childsite.but97" "Button29" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab105" "Labeledframe7" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab105.childsite.but92" "Button30" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab105.childsite.but93" "Button31" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab106" "Labeledframe8" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab106.childsite.but92" "Button33" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab106.childsite.but93" "Button34" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab107" "Labeledframe9" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab107.childsite.but93" "Button36" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page2.cs.lab107.childsite.lab108" "Label2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra76" "Frame13" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra76.lab81" "Label8" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra76.mcl78" "Mclistbox1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra76.scr80" "Scrollbar1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra79" "Frame14" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra79.but81" "Button131" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra79.com77" "Combobox1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra79.ent80" "Entryfield3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra79.ent82" "Entryfield4" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra79.ent83" "Entryfield5" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.but88" "Button129" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.but89" "Button130" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.but96" "Button122" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.ent85" "Entryfield1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.fra90.ent92" "Entry2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.fra90.lab95" "Label7" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.fra90.men91" "Menubutton1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page3.cs.fra84.fra90.men91.m" "Menu14" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra112" "Frame3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra112.but113" "Button43" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra112.but114" "Button44" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra112.but115" "Button45" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra117" "Frame4" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra117.but113" "Button47" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra117.but114" "Button48" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.fra117.button78" "Button46" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.frame77" "Frame12" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.frame77.but115" "Button69" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.frame77.but116" "Button70" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page4.cs.lab111" "Label3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.che119" "Checkbox1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.che120" "Checkbox2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.ent78" "Entryfield2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.fra122" "Frame5" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.fra122.but123" "Button51" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.fra122.but124" "Button52" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.fra122.but125" "Button53" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.lab81" "Labeledframe10" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.lab81.childsite.but83" "Button126" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.lab81.childsite.but84" "Button127" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.lab81.childsite.but85" "Button128" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.lab81.childsite.ent82" "Entry7" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page5.cs.scr77" "Scrolledlistbox1" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84" "Labeledframe12" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.che76" "Checkbox4" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.che79" "Checkbox3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra76" "Frame17" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra76.but76" "Button143" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra76.but77" "Button141" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra76.but78" "Button144" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra76.but79" "Button145" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra76.ent77" "Entryfield6" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra86" "Frame15" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra86.but76" "Button142" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra86.but88" "Button139" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra86.scr87" "Scrolledlistbox2" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra89" "Frame16" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra89.but88" "Button140" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra89.ent90" "Entry8" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra89.scr87" "Scrolledlistbox3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.lab76" "Labeledframe13" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.lab76.childsite.but78" "Button149" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.lab76.childsite.ent77" "Entry9" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.lab79" "Labeledframe14" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.lab79.childsite.but78" "Button150" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.lab79.childsite.ent77" "Entry10" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.opt86" "Optionmenu3" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page8.cs.fra84.fra79.but79" "Button135" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page8.cs.fra84.lab80" "Labeledframe11" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page8.cs.fra84.lab80.childsite.but81" "Button146" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page8.cs.fra84.lab80.childsite.but82" "Button147" vTcl:WidgetProc "Toplevel1" 1
    vTcl:DefineAlias "$base.tab88.canvas.notebook.cs.page8.cs.fra84.lab80.childsite.but83" "Button148" vTcl:WidgetProc "Toplevel1" 1

    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $base -class Toplevel \
        -background #ffffff -menu "$base.m88" 
    wm focusmodel $base passive
    wm geometry $base 796x297+50+685; update
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "EMPIRE GUI"
    bindtags $base "$base Toplevel all _TopLevel"
    vTcl:FireEvent $base <<Create>>
    wm protocol $base WM_DELETE_WINDOW "vTcl:FireEvent $base <<DeleteWindow>>"

    frame $base.fra77 \
        -borderwidth 2 -height 75 -relief groove -width 125 
    button $base.fra77.but78 \
        -activeforeground limegreen \
        -command {fileDialog .top75
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects fileopen.gif]] \
        -relief flat -text Project: 
    bindtags $base.fra77.but78 "$base.fra77.but78 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Select  project}
    }
    entry $base.fra77.ent79 \
        -background #ffffff -font {Helvetica -12 bold} -foreground #0000ff \
        -justify right -textvariable file -validate none -vcmd {} 
    bindtags $base.fra77.ent79 "$base.fra77.ent79 Entry $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.ent79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Project root name}
    }
    button $base.fra77.but80 \
        -activeforeground limegreen -command {exec $editor $file.inp &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects edit.gif]] \
        -relief flat -text Input 
    bindtags $base.fra77.but80 "$base.fra77.but80 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but80 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit EMPIRE input file}
    }
    button $base.fra77.but86 \
        -activeforeground red \
        -command {exec xterm -e run $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects launch.gif]] \
        -relief flat -text R+F+P 
    bindtags $base.fra77.but86 "$base.fra77.but86 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but86 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Launch full sequence of calculations (EMPIRE+EMPEND+...+PLOTC4) }
    }
    button $base.fra77.but87 \
        -activeforeground red \
        -command {exec xterm -e runE $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects mini-run.gif]] \
        -relief flat -text Run 
    bindtags $base.fra77.but87 "$base.fra77.but87 Button $base all _vTclBalloon _vTclBalloon"
    bind $base.fra77.but87 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run EMPIRE only}
    }
    button $base.fra77.but81 \
        -activeforeground limegreen -command {exec $editor $file.lst &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects editcopy.gif]] \
        -relief flat -text Output 
    bindtags $base.fra77.but81 "$base.fra77.but81 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View main output from EMPIRE}
    }
    button $base.fra77.but77 \
        -activeforeground limegreen -command {exec $editor $file.out &} \
        -font {Helvetica -12} -foreground darkgreen \
        -image [vTcl:image:get_image [file join / zip Projects shortout.gif]] \
        -relief flat -text Output 
    bindtags $base.fra77.but77 "$base.fra77.but77 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View short output from EMPIRE}
    }
    button $base.fra77.but82 \
        -activeforeground limegreen -command {exec $editor $file.endf &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects kthememgr.gif]] \
        -relief flat -text ENDF 
    bindtags $base.fra77.but82 "$base.fra77.but82 Button $base all _vTclBalloon _vTclBalloon"
    bind $base.fra77.but82 <<SetBalloon>> {
        set ::vTcl::balloon::%W {ENDF-6 formatted file}
    }
    button $base.fra77.but83 \
        -activeforeground limegreen -command {exec $editor $file.exf &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects x4.gif]] \
        -relief flat -text EXFOR 
    bindtags $base.fra77.but83 "$base.fra77.but83 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Experimental data (EXFOR)}
    }
    button $base.fra77.but84 \
        -activeforeground limegreen -command {exec $editor $file.c4 &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects stop.gif]] \
        -relief flat -text {C4 file} 
    bindtags $base.fra77.but84 "$base.fra77.but84 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {EXFOR data translated into computational format}
    }
    button $base.fra77.but85 \
        -activeforeground limegreen -command {exec gv -landscape $file.ps &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / zip Projects imagegallery.gif]] \
        -relief flat -text Plots 
    bindtags $base.fra77.but85 "$base.fra77.but85 Button $base all _vTclBalloon _vTclBalloon"
    bind $base.fra77.but85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View PLOTC4 plots}
    }
    button $base.fra77.but76 \
        -activeforeground limegreen -command {adjourn .top75
ddlist} \
        -font {Helvetica -12} -foreground darkgreen \
        -image [vTcl:image:get_image [file join / zip Projects reload.gif]] \
        -relief flat -text Update 
    bindtags $base.fra77.but76 "$base.fra77.but76 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.but76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Update lists of files after running any code }
    }
    label $base.fra77.lab77 \
        -width 10 
    button $base.fra77.button77 \
        -activebackground red -activeforeground White \
        -command {exec clean $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #dcdcdc -highlightcolor #000000 \
        -image [vTcl:image:get_image [file join / zip Projects kleandisk.gif]] \
        -relief flat -text Clean 
    bindtags $base.fra77.button77 "$base.fra77.button77 Button $base all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $base.fra77.button77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Clean project (delete all project files except input)}
    }
    ::iwidgets::tabnotebook $base.tab88 \
        -angle 20 -bevelamount 2 -raiseselect 1 -tabborders 1 \
        -tabforeground #666666 -tabpos n 
    $base.tab88 add \
        -command {} -label Main -width 0 
    $base.tab88 add \
        -command {} -label EXFOR -width 0 
    $base.tab88 add \
        -command {} -label {ZVV plots} -width 0 
    $base.tab88 add \
        -command {} -label Logs -width 0 
    $base.tab88 add \
        -command {} -label Files -width 0 
    $base.tab88 add \
        -command {} -label Archive -width 0 
    $base.tab88 add \
        -command {} -label Multi-run -width 0 
    $base.tab88 add \
        -command {} -label Source -width 0 
    set site_8_0 [lindex [$base.tab88 childsite] 0]
    ::iwidgets::labeledframe $site_8_0.lab90 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos ne -labeltext {EMPIRE input} 
    set site_10_0 [$site_8_0.lab90 childsite]
    button $site_10_0.but92 \
        -activeforeground limegreen \
        -command {exec xterm -bg red -fg white -fn 10x20 -geometry 40x2+500+500 -e cp -i skel.inp $file.inp 
adjourn .top75
exec $editor $file.inp &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -pady 1m \
        -relief raised -text Create 
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $base all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create input file for the new project}
    }
    button $site_10_0.but93 \
        -activeforeground limegreen -command {exec $editor $file.inp &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -pady 1m \
        -relief raised -text Edit 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit input file}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -ipady 20 -pady 5 \
        -side top 
    ::iwidgets::labeledframe $site_8_0.lab94 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext Execute 
    set site_10_0 [$site_8_0.lab94 childsite]
    button $site_10_0.but95 \
        -activeforeground red \
        -command {exec xterm -e run $file &
adjourn .top75
# create list of possible ddx plots 
ddlist} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -pady 1m \
        -relief raised -text Run+Format+Plot 
    bindtags $site_10_0.but95 "$site_10_0.but95 Button $base all _vTclBalloon"
    bind $site_10_0.but95 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Launch full sequence (EMPIRE + ENDF + ... +PLOTC4)}
    }
    label $site_10_0.lab96 \
        -activebackground #dcdcdc -font {Helvetica -12 } -text or 
    button $site_10_0.but97 \
        -activeforeground red \
        -command {exec xterm -e runE $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -relief raised \
        -text Run 
    bindtags $site_10_0.but97 "$site_10_0.but97 Button $base all _vTclBalloon"
    bind $site_10_0.but97 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run EMPIRE only}
    }
    button $site_10_0.but98 \
        -activeforeground red \
        -command {exec xterm -e format $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -relief raised \
        -text Format 
    bindtags $site_10_0.but98 "$site_10_0.but98 Button $base all _vTclBalloon"
    bind $site_10_0.but98 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run EMPEND to create an ENDF-6 file}
    }
    button $site_10_0.but99 \
        -activeforeground red \
        -command {exec xterm -e plot $file &
adjourn .top75
# create list of possible ddx plots 
ddlist} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -relief raised \
        -text Plot 
    bindtags $site_10_0.but99 "$site_10_0.but99 Button $base all _vTclBalloon"
    bind $site_10_0.but99 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run PLOTC4 to create plots comparing calculations with experiment}
    }
    pack $site_10_0.but95 \
        -in $site_10_0 -anchor center -expand 0 -fill x -side top 
    pack $site_10_0.lab96 \
        -in $site_10_0 -anchor center -expand 0 -fill none -side top 
    pack $site_10_0.but97 \
        -in $site_10_0 -anchor center -expand 0 -fill x -side top 
    pack $site_10_0.but98 \
        -in $site_10_0 -anchor center -expand 1 -fill x -side left 
    pack $site_10_0.but99 \
        -in $site_10_0 -anchor center -expand 1 -fill x -side right 
    ::iwidgets::labeledframe $site_8_0.lab100 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext Output 
    set site_10_0 [$site_8_0.lab100 childsite]
    button $site_10_0.but92 \
        -activeforeground limegreen -command {exec $editor $file.lst &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text Full 
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $base all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Full output from EMPIRE}
    }
    button $site_10_0.but93 \
        -activeforeground limegreen -command {exec $editor $file.out &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text Short 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Short output from EMPIRE}
    }
    button $site_10_0.but101 \
        -activeforeground limegreen -command {exec $editor $file.endf &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text ENDF 
    bindtags $site_10_0.but101 "$site_10_0.but101 Button $base all _vTclBalloon"
    bind $site_10_0.but101 <<SetBalloon>> {
        set ::vTcl::balloon::%W {ENDF-6 file}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but101 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_0.lab102 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext {Out/In: Levels} 
    set site_10_0 [$site_8_0.lab102 childsite]
    button $site_10_0.but92 \
        -activeforeground limegreen -command {exec $editor $file.lev &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text All 
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $base all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Discrete level for all nuclei  involved in the calculation}
    }
    button $site_10_0.but93 \
        -activeforeground limegreen -command {exec $editor $file-lev.col &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text Collective 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Collective levels used for direct calculations with ECIS}
    }
    button $site_10_0.but101 \
        -activeforeground limegreen \
        -command {exec gv -landscape $file-cum.ps &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {Cumul. plot} 
    bindtags $site_10_0.but101 "$site_10_0.but101 Button $base all _vTclBalloon"
    bind $site_10_0.but101 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Cumulative plots of discrete levels along with lev. dens.}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but101 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_0.lab103 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext {Out/In: OMP} 
    set site_10_0 [$site_8_0.lab103 childsite]
    button $site_10_0.but92 \
        -activeforeground limegreen -command {exec $editor $file-omp.int &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text Internal 
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $base all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Optical model parameters from potentials coded inside EMPIRE}
    }
    button $site_10_0.but93 \
        -activeforeground limegreen -command {exec $editor $file-omp.ripl &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text RIPL 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Optical model parameters taken from RIPL}
    }
    button $site_10_0.but101 \
        -activeforeground limegreen -command {exec $editor $file-omp.dir &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text Direct 
    bindtags $site_10_0.but101 "$site_10_0.but101 Button $base all _vTclBalloon"
    bind $site_10_0.but101 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Optical model parameters used for direct calculations with ECIS}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but101 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_8_0.lab90 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    pack $site_8_0.lab94 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -ipady 5 \
        -padx 5 -pady 2 -side left 
    pack $site_8_0.lab100 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    pack $site_8_0.lab102 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    pack $site_8_0.lab103 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    set site_8_1 [lindex [$base.tab88 childsite] 1]
    ::iwidgets::labeledframe $site_8_1.lab104 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext Execute 
    set site_10_0 [$site_8_1.lab104 childsite]
    button $site_10_0.but95 \
        -activeforeground red \
        -command {exec xterm -e c4 $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -pady 1m \
        -relief raised -text X4TOC4 
    bindtags $site_10_0.but95 "$site_10_0.but95 Button $base all _vTclBalloon"
    bind $site_10_0.but95 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run X4TOC4 code to convert EXFOR data into computational format}
    }
    button $site_10_0.but97 \
        -activeforeground red \
        -command {exec xterm -e sortc4 $file &
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -pady 1m \
        -relief raised -text {Sort C4} 
    bindtags $site_10_0.but97 "$site_10_0.but97 Button $base all _vTclBalloon"
    bind $site_10_0.but97 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Sort C4 file resulting from X4TOC4 run}
    }
    pack $site_10_0.but95 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but97 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_1.lab105 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext View/Edit 
    set site_10_0 [$site_8_1.lab105 childsite]
    button $site_10_0.but92 \
        -activeforeground limegreen -command {exec $editor $file.exf &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text EXFOR 
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $base all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Experimental data in EXFOR  format}
    }
    button $site_10_0.but93 \
        -activeforeground limegreen -command {exec $editor $file.c4 &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {C4 file} 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Experimental data in the computational format}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_1.lab106 \
        -background #d9d9d9 -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext {Conversion logs} 
    set site_10_0 [$site_8_1.lab106 childsite]
    button $site_10_0.but92 \
        -activeforeground limegreen -command {exec $editor $file.x42c4_lst &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {X4TOC4 output} 
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $base all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Output of the X4TOC4 code}
    }
    button $site_10_0.but93 \
        -activeforeground limegreen \
        -command {exec $editor $file.x42c4_errs &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {X4TOC4 error} 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {List of EXFOR entries which were not translated into C4 format}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_1.lab107 \
        -background #d9d9d9 -ipadx 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext {Conversion table} 
    set site_10_0 [$site_8_1.lab107 childsite]
    label $site_10_0.lab108 \
        -font {Helvetica -12} -foreground Red -justify left \
        -text {WARNING: changing conversion table will affect all further X4TOC4 runs. Be sure you know what you are doing.} \
        -wraplength 230 
    button $site_10_0.but93 \
        -activeforeground limegreen \
        -command {exec $editor ../util/x4toc4/reaction &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {Edit X4TOC4 table} 
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $base all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Correspondence between  EXFOR codes and  ENDF-6 ones}
    }
    pack $site_10_0.lab108 \
        -in $site_10_0 -anchor center -expand 0 -fill none -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill none -pady 5 -side top 
    pack $site_8_1.lab104 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    pack $site_8_1.lab105 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    pack $site_8_1.lab106 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 15 -padx 5 \
        -pady 2 -side left 
    pack $site_8_1.lab107 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 5 -padx 5 \
        -pady 2 -side left 
    set site_8_2 [lindex [$base.tab88 childsite] 2]
    ::iwidgets::scrolledlistbox $site_8_2.scr82 \
        -background #d9d9d9 \
        -dblclickcommand {exec xterm -e zvcomb [selection get] &} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {Available ZVV plots} -listvariable zvvplots \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12 } -vscrollmode dynamic -width 170 
    frame $site_8_2.fra84 \
        -borderwidth 2 -height 75 
    ::iwidgets::entryfield $site_8_2.fra84.ent85 \
        -background #d9d9d9 \
        -command {set zvvplots [glob -nocomplain *$zvfilter*.zvd]} \
        -labelfont {Helvetica -12 } -labeltext Filter: -textbackground white \
        -textvariable zvfilter -width 15 
    frame $site_8_2.fra84.fra90 \
        -borderwidth 2 -height 75 -width 125 
    menubutton $site_8_2.fra84.fra90.men91 \
        -disabledforeground #a1a4a1 -font {Helvetica -12} -indicatoron 1 \
        -menu "$site_8_2.fra84.fra90.men91.m" -padx 5 -pady 2 -relief raised \
        -textvariable mt -width 3 
    bindtags $site_8_2.fra84.fra90.men91 "$site_8_2.fra84.fra90.men91 Menubutton $base all _vTclBalloon"
    bind $site_8_2.fra84.fra90.men91 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Click to select predefined MT}
    }
    menu $site_8_2.fra84.fra90.men91.m \
        -activebackground #f7fbf7 -activeforeground black \
        -disabledforeground #a1a4a1 -foreground black -tearoff 0 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 1 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=1 (total)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 2 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=2 (elastic)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 4 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=4 (inelastic)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 16 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=16 (x,2n)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 17 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=17 (x,3n)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 18 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=18 (x,f)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 22 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=22 (x,na)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 28 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=28 (x,np)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 45 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=45 (x,npa)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 102 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=102 (x,g)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 103 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=103 (x,p)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 107 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=107 (x,a)} 
    $site_8_2.fra84.fra90.men91.m add radiobutton \
        -value 112 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:get_font "vTcl:font4"] -label {MT=112 (n,pa)} 
    entry $site_8_2.fra84.fra90.ent92 \
        -background #ffffff -insertbackground black -textvariable mt -width 5 
    label $site_8_2.fra84.fra90.lab95 \
        -activebackground #dcdcdc -disabledforeground #a1a4a1 \
        -font {Helvetica -12 } -text {Select MT:} 
    button $site_8_2.fra84.but96 \
        -activeforeground red \
        -command {exec xterm -e zvd $file $mt &
adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Plot  selected MT} -wraplength 120 
    bindtags $site_8_2.fra84.but96 "$site_8_2.fra84.but96 Button $base all _vTclBalloon"
    bind $site_8_2.fra84.but96 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create, veiew and store ZVV plot for the selected MT number}
    }
    button $site_8_2.fra84.but88 \
        -activeforeground red \
        -command {exec guizvv.tcl $file &
adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Launch ZVV interface } -wraplength 72 
    bindtags $site_8_2.fra84.but88 "$site_8_2.fra84.but88 Button $base all _vTclBalloon"
    bind $site_8_2.fra84.but88 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create comparison of up to 3 evaluations as well as single plot}
    }
    button $site_8_2.fra84.but89 \
        -activeforeground red \
        -command {exec xterm -e zvpl $file &
adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {ZVV plot from EMPIRE} -wraplength 72 
    bindtags $site_8_2.fra84.but89 "$site_8_2.fra84.but89 Button $base all _vTclBalloon"
    bind $site_8_2.fra84.but89 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Any excitation function can be plotted; no need for ENDF file}
    }
    frame $site_8_2.fra76 \
        -borderwidth 2 -height 75 -width 227 
    label $site_8_2.fra76.lab81 \
        -anchor w -font {Helvetica -12 } -justify left \
        -text {Select spectra for ZVV plotting} 
    bindtags $site_8_2.fra76.lab81 "$site_8_2.fra76.lab81 Label $base all _vTclBalloon"
    bind $site_8_2.fra76.lab81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Click to create a list of spectra to be plotted }
    }
    ::mclistbox::mclistbox $site_8_2.fra76.mcl78 \
        -background #ffffff -font {Helvetica -10} -height 0 \
        -labelfont {Helvetica -10} -selectborderwidth 0 \
        -selectcommand {set dd ""
set ddxlist [selection get]
set ddx ""
set prev "#"
set last "#"
foreach el $ddxlist {
   if { $prev == $last } {
   lappend ddx $el
   }
   set prev $el 
}
lappend dd} \
        -selectmode multiple -width 44 -yscrollcommand {Scrollbar1 set} 
    $site_8_2.fra76.mcl78 column add col1 \
        -background #ffffff -font {Helvetica -10} -label # \
        -labelrelief raised -resizable 1 -visible 1 -width 5 
    $site_8_2.fra76.mcl78 column add col2 \
        -background #f999f999f999 -font {Helvetica -10 } -label MF \
        -labelrelief raised -resizable 1 -visible 1 -width 5 
    $site_8_2.fra76.mcl78 column add col3 \
        -background #ffffff -font {Helvetica -10} -label p \
        -labelrelief raised -resizable 1 -visible 1 -width 3 
    $site_8_2.fra76.mcl78 column add col4 \
        -background #f999f999f999 -font {Helvetica -10} -label MT \
        -labelrelief raised -resizable 1 -visible 1 -width 6 
    $site_8_2.fra76.mcl78 column add col5 \
        -background #ffffff -font {Helvetica -10} -label Einc \
        -labelrelief raised -resizable 1 -visible 1 -width 10 
    $site_8_2.fra76.mcl78 column add col6 \
        -background #f999f999f999 -font {Helvetica -10 } -label Elev \
        -labelrelief raised -resizable 1 -visible 1 -width 10 
    $site_8_2.fra76.mcl78 column add col7 \
        -background #ffffff -font {Helvetica -10} -label Ang \
        -labelrelief raised -resizable 1 -visible 1 -width 5 
    $site_8_2.fra76.mcl78 column add col8 \
        -background #ffffff -font {Helvetica -10} -label init \
        -labelrelief raised -resizable 0 -visible 0 -width 3 
    scrollbar $site_8_2.fra76.scr80 \
        -command {Mclistbox1 yview} 
    frame $site_8_2.fra79 \
        -borderwidth 2 -height 75 -width 125 
    ::iwidgets::entryfield $site_8_2.fra79.ent80 \
        -background #d9d9d9 -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {List of selected } -textbackground #ffffff \
        -textvariable ddx -width 0 
    ::iwidgets::entryfield $site_8_2.fra79.ent82 \
        -background #d9d9d9 -justify right -labelfont {Helvetica -12 } \
        -labeltext {Shift 10**} -textbackground #ffffff -textvariable nsh 
    ::iwidgets::entryfield $site_8_2.fra79.ent83 \
        -background #d9d9d9 -justify right -labelfont {Helvetica -12 } \
        -labeltext {Eres (rel)} -textbackground #ffffff -textvariable eres 
    button $site_8_2.fra79.but81 \
        -activeforeground red \
        -command {set memlist($multi) $ddx
Combobox1 insert list end $multi
set ddxsh ""
set i 0
foreach el $ddx {
   lappend ddxsh [expr $i*$nsh]
   incr i +1
}

set lsttab [open LSTTAB.INP w+]
puts $lsttab ""
puts $lsttab ""
puts $lsttab "ENDF.DAT"
puts $lsttab "-"
puts $lsttab "Empire"
puts $lsttab $eres
set i 0
foreach el $ddx {
   puts $lsttab $el
   puts $lsttab [expr pow(10,[lindex $ddxsh $i])]
   incr i +1
}
puts $lsttab ""
close $lsttab
#exec gvim LSTTAB.INP
exec mv LSTTAB.INP ../util/lsttab/LSTTAB.INP 
exec xterm -e zvvddx $file $multi &} \
        -font {Helvetica -12 } -foreground darkred -text {Plot the list } 
    bindtags $site_8_2.fra79.but81 "$site_8_2.fra79.but81 Button $base all _vTclBalloon"
    bind $site_8_2.fra79.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create ZVV plot for the list of selected spectra }
    }
    ::iwidgets::combobox $site_8_2.fra79.com77 \
        -background #d9d9d9 \
        -command {namespace inscope ::iwidgets::Combobox {::.top75.tab88.canvas.notebook.cs.page3.cs.fra79.com77 _addToList}} \
        -justify right -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {List name} -selectioncommand {set ddx $memlist($multi)} \
        -textbackground #ffffff -textvariable multi -unique 0 -width 10 
    pack $site_8_2.scr82 \
        -in $site_8_2 -anchor nw -expand 0 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_2.fra84 \
        -in $site_8_2 -anchor nw -expand 0 -fill y -padx 5 -pady 10 \
        -side left 
    pack $site_8_2.fra84.ent85 \
        -in $site_8_2.fra84 -anchor nw -expand 0 -fill x -pady 20 -side top 
    pack $site_8_2.fra84.fra90 \
        -in $site_8_2.fra84 -anchor center -expand 0 -fill x -side top 
    pack $site_8_2.fra84.fra90.men91 \
        -in $site_8_2.fra84.fra90 -anchor center -expand 0 -fill none \
        -side right 
    pack $site_8_2.fra84.fra90.ent92 \
        -in $site_8_2.fra84.fra90 -anchor center -expand 0 -fill none \
        -side right 
    pack $site_8_2.fra84.fra90.lab95 \
        -in $site_8_2.fra84.fra90 -anchor center -expand 0 -fill none \
        -side left 
    pack $site_8_2.fra84.but96 \
        -in $site_8_2.fra84 -anchor center -expand 0 -fill x -side top 
    pack $site_8_2.fra84.but88 \
        -in $site_8_2.fra84 -anchor se -expand 0 -fill none -pady 3 \
        -side right 
    pack $site_8_2.fra84.but89 \
        -in $site_8_2.fra84 -anchor sw -expand 0 -fill none -pady 3 \
        -side left 
    pack $site_8_2.fra76 \
        -in $site_8_2 -anchor center -expand 0 -fill both -side left 
    pack $site_8_2.fra76.lab81 \
        -in $site_8_2.fra76 -anchor n -expand 0 -fill x -side top 
    pack $site_8_2.fra76.mcl78 \
        -in $site_8_2.fra76 -anchor w -expand 0 -fill y -side left 
    pack $site_8_2.fra76.scr80 \
        -in $site_8_2.fra76 -anchor center -expand 1 -fill y -side right 
    pack $site_8_2.fra79 \
        -in $site_8_2 -anchor w -expand 1 -fill both -padx 5 -side left 
    pack $site_8_2.fra79.ent80 \
        -in $site_8_2.fra79 -anchor nw -expand 0 -fill x -side top 
    pack $site_8_2.fra79.ent82 \
        -in $site_8_2.fra79 -anchor center -expand 0 -fill none -side top 
    pack $site_8_2.fra79.ent83 \
        -in $site_8_2.fra79 -anchor center -expand 0 -fill none -side top 
    pack $site_8_2.fra79.but81 \
        -in $site_8_2.fra79 -anchor center -expand 0 -fill x -side top 
    pack $site_8_2.fra79.com77 \
        -in $site_8_2.fra79 -anchor center -expand 0 -fill x -side top 
    set site_8_3 [lindex [$base.tab88 childsite] 3]
    label $site_8_3.lab111 \
        -font {Helvetica -12 } -justify left -padx 1 -pady 1 -relief ridge \
        -text {Outputs and log files produced by EMPIRE and utility codes. These files should be checked for possible problems during the execution.} \
        -wraplength 200 
    frame $site_8_3.fra112 \
        -borderwidth 2 -height 75 -width 250 
    button $site_8_3.fra112.but113 \
        -activeforeground limegreen \
        -command {exec xterm -bg darkorange -title WARNINGS -e less $file.war &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {EMPIRE warnings} 
    bindtags $site_8_3.fra112.but113 "$site_8_3.fra112.but113 Button $base all _vTclBalloon"
    bind $site_8_3.fra112.but113 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Warnigs produced by EMPIRE}
    }
    button $site_8_3.fra112.but114 \
        -activeforeground limegreen -command {exec $editor $file.x42c4_lst &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {X4TOC4 log} 
    bindtags $site_8_3.fra112.but114 "$site_8_3.fra112.but114 Button $base all _vTclBalloon"
    bind $site_8_3.fra112.but114 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Control output of the X4TOC4 code}
    }
    button $site_8_3.fra112.but115 \
        -activeforeground limegreen \
        -command {exec $editor $file.x42c4_errs &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {X4TOC4 errors} 
    bindtags $site_8_3.fra112.but115 "$site_8_3.fra112.but115 Button $base all _vTclBalloon"
    bind $site_8_3.fra112.but115 <<SetBalloon>> {
        set ::vTcl::balloon::%W {List of EXFOR entries which were not translated into C4 format}
    }
    frame $site_8_3.fra117 \
        -borderwidth 2 -height 75 -width 250 
    button $site_8_3.fra117.button78 \
        -activebackground #dcdcdc -activeforeground limegreen \
        -command {exec $editor $file-log.empend &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -highlightcolor #000000 -image {} -padx 1m -relief raised \
        -text {EMPEND log} 
    bindtags $site_8_3.fra117.button78 "$site_8_3.fra117.button78 Button $base all _vTclBalloon"
    bind $site_8_3.fra117.button78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Control output of the EMPEND code}
    }
    button $site_8_3.fra117.but113 \
        -activeforeground limegreen -command {exec $editor $file-log.fixup &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {FIXUP log} 
    bindtags $site_8_3.fra117.but113 "$site_8_3.fra117.but113 Button $base all _vTclBalloon"
    bind $site_8_3.fra117.but113 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Control output of the FIXUP code}
    }
    button $site_8_3.fra117.but114 \
        -activeforeground limegreen \
        -command {exec $editor $file-log.legend &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 -image {} -padx 1m \
        -relief raised -text {LEGEND log} 
    bindtags $site_8_3.fra117.but114 "$site_8_3.fra117.but114 Button $base all _vTclBalloon"
    bind $site_8_3.fra117.but114 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Control output of the LEGEND code}
    }
    frame $site_8_3.frame77 \
        -borderwidth 2 -height 75 -highlightbackground #dcdcdc \
        -highlightcolor #000000 -width 250 
    button $site_8_3.frame77.but115 \
        -activebackground #dcdcdc -activeforeground limegreen -command {} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -highlightcolor #000000 -image {} -padx 1m -relief raised \
        -state disabled -text {SIXTAB log} 
    button $site_8_3.frame77.but116 \
        -activebackground #dcdcdc -activeforeground limegreen \
        -command {exec $editor $file-log.plotc4 &} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -highlightcolor #000000 -image {} -padx 1m -relief raised \
        -text {PLOTC4 log} 
    bindtags $site_8_3.frame77.but116 "$site_8_3.frame77.but116 Button $base all _vTclBalloon"
    bind $site_8_3.frame77.but116 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Control output of the PLOTC4 code}
    }
    pack $site_8_3.lab111 \
        -in $site_8_3 -anchor center -expand 0 -fill none -ipadx 2 -ipady 5 \
        -padx 15 -pady 10 -side left 
    pack $site_8_3.fra112 \
        -in $site_8_3 -anchor center -expand 0 -fill none -ipadx 2 -ipady 2 \
        -padx 2 -pady 2 -side left 
    pack $site_8_3.fra112.but113 \
        -in $site_8_3.fra112 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_3.fra112.but114 \
        -in $site_8_3.fra112 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_3.fra112.but115 \
        -in $site_8_3.fra112 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_3.fra117 \
        -in $site_8_3 -anchor center -expand 0 -fill none -ipadx 2 -ipady 2 \
        -padx 2 -pady 2 -side left 
    pack $site_8_3.fra117.button78 \
        -in $site_8_3.fra117 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_3.fra117.but113 \
        -in $site_8_3.fra117 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_3.fra117.but114 \
        -in $site_8_3.fra117 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_3.frame77 \
        -in $site_8_3 -anchor center -expand 0 -fill none -ipadx 2 -ipady 2 \
        -padx 2 -pady 2 -side left 
    pack $site_8_3.frame77.but115 \
        -in $site_8_3.frame77 -anchor center -expand 0 -fill x -padx 5 \
        -pady 5 -side top 
    pack $site_8_3.frame77.but116 \
        -in $site_8_3.frame77 -anchor center -expand 0 -fill x -padx 5 \
        -pady 5 -side top 
    set site_8_4 [lindex [$base.tab88 childsite] 4]
    ::iwidgets::checkbox $site_8_4.che119 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos n -labeltext {Select files:} -relief flat 
    $site_8_4.che119 add chk0 \
        -activebackground #dcdcdc -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .lst -selectcolor green \
        -text {full output} -variable cklo 
    $site_8_4.che119 add chk1 \
        -activebackground #dcdcdc -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .out -selectcolor green \
        -text {short output} -variable cksh 
    $site_8_4.che119 add chk2 \
        -activebackground #dcdcdc -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue -log* -selectcolor green \
        -text {log files} -variable cklog 
    $site_8_4.che119 add chk3 \
        -activebackground #dcdcdc -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue *.endf -selectcolor green \
        -text ENDF -variable ckendf 
    $site_8_4.che119 add chk4 \
        -activebackground #dcdcdc -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .ps -selectcolor green \
        -text {PLOTC4 plots} -variable ckplots 
    $site_8_4.che119 add chk5 \
        -activebackground #dcdcdc -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .exf -selectcolor orange \
        -text EXFOR -variable ckx4 
    $site_8_4.che119 add chk6 \
        -activebackground #dcdcdc -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .c4 -selectcolor orange \
        -text {C4 file} -variable ckc4 
    ::iwidgets::checkbox $site_8_4.che120 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -relief flat 
    $site_8_4.che120 add chk0 \
        -activebackground #dcdcdc -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue -omp.int -padx 1 \
        -selectcolor orange -text {internal omp} -variable ckintomp 
    $site_8_4.che120 add chk1 \
        -activebackground #dcdcdc -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue -omp.ripl -padx 1 \
        -selectcolor orange -text {RIPL omp} -variable ckriplomp 
    $site_8_4.che120 add chk2 \
        -activebackground #dcdcdc -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue -omp.dir -padx 1 \
        -selectcolor orange -text {direct omp} -variable ckdiromp 
    $site_8_4.che120 add chk3 \
        -activebackground #dcdcdc -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue *.zvd -padx 1 -selectcolor orange \
        -text {ZVV plots} -variable ckzvv 
    $site_8_4.che120 add chk4 \
        -activebackground #dcdcdc -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .lev -padx 1 -selectcolor red \
        -text levels -variable cklev 
    $site_8_4.che120 add chk5 \
        -activebackground #dcdcdc -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue -lev.col -padx 1 -selectcolor red \
        -text {collective levels} -variable ckcollev 
    $site_8_4.che120 add chk6 \
        -activebackground #dcdcdc -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue {} -onvalue .inp -padx 1 -selectcolor red \
        -text {EMPIRE input} -variable ckinp 
    frame $site_8_4.fra122 \
        -height 75 -width 125 
    button $site_8_4.fra122.but123 \
        -activeforeground Red \
        -command set\ delist\ \"\"\nlappend\ delist\ \$cklo\ \$cksh\ \$cklog\ \$ckendf\ \ \$ckplots\ \$ckx4\ \$ckc4\ \\\n\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \$ckintomp\ \ \$ckriplomp\ \ \$ckdiromp\ \ \$ckzvv\ \ \$cklev\ \\\ \n\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \$ckcollev\ \ \$ckinp\nforeach\ el\ \$delist\ \{\n\ \ \ if\ \{\$el\ ==\ \"\"\}\ continue\n\ \ \ eval\ exec\ cleansel\ \$file\ \$el\n\ \ \ if\ \{\$el\ ==\ \$cklog\}\ \{\n\ \ \ \ \ \ exec\ rm\ \$file.x42c4_errs\n\ \ \ \ \ \ exec\ rm\ \$file.x42c4_lst\n\ \ \ \ \ \ exec\ rm\ \$file.war\n\ \ \ \ \ \ \}\n\}\n\nadjourn\ .top75 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 0 -relief raised \
        -text {Delete selected files} -wraplength 60 
    bindtags $site_8_4.fra122.but123 "$site_8_4.fra122.but123 Button $base all _vTclBalloon"
    bind $site_8_4.fra122.but123 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete all project related files of the types selected with check button}
    }
    button $site_8_4.fra122.but124 \
        -activeforeground Red -command {exec clean $file
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -pady 2m \
        -relief raised -text {Clean project} -wraplength 60 
    bindtags $site_8_4.fra122.but124 "$site_8_4.fra122.but124 Button $base all _vTclBalloon"
    bind $site_8_4.fra122.but124 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Clean project (delete all project files except input)}
    }
    button $site_8_4.fra122.but125 \
        -activebackground Red -activeforeground White \
        -command {exec clean $file
exec rm -f $file.inp
adjourn .top75} \
        -disabledforeground #a3a3a3 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #d9d9d9 -image {} -padx 1m -pady 2m \
        -relief raised -text {Remove project} -wraplength 60 
    bindtags $site_8_4.fra122.but125 "$site_8_4.fra122.but125 Button $base all _vTclBalloon"
    bind $site_8_4.fra122.but125 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete all project related files (including input)}
    }
    ::iwidgets::scrolledlistbox $site_8_4.scr77 \
        -background #d9d9d9 \
        -dblclickcommand {set selecfilelist [selection get]
set selecfile [lindex $selecfilelist 0]
set exten [file extension $selecfile]
if {$exten == ".ps"} {
  exec gv -landscape $selecfile &
} elseif {$exten == ".zvd"} {
  exec xterm -e zvcomb $selecfile &
} else {
  exec $editor $selecfile &
}} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {Available files:} -listvariable filelist \
        -selectioncommand {set selecfilelist [selection get]
set selecfile [lindex $selecfilelist 0]} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12 } -vscrollmode dynamic -width 150 
    ::iwidgets::entryfield $site_8_4.ent78 \
        -background #d9d9d9 \
        -command {#set filelist [glob -nocomplain *$profilter*]
adjourn .top75} \
        -labelfont {Helvetica -12 } -labeltext Filter: -textbackground white \
        -textvariable profilter -width 14 
    ::iwidgets::labeledframe $site_8_4.lab81 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext {Selected file} 
    set site_10_0 [$site_8_4.lab81 childsite]
    entry $site_10_0.ent82 \
        -background #ffffff -insertbackground black -textvariable selecfile \
        -width 15 
    button $site_10_0.but83 \
        -activeforeground limegreen \
        -command {set file [file root $selecfile]
set zvfilter $file
set profilter $file
set archfilter $file
adjourn .top75
Combobox1 clear
# create list of possible ddx plots 
ddlist} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -padx 1m -text {Change to its project} 
    bindtags $site_10_0.but83 "$site_10_0.but83 Button $base all _vTclBalloon"
    bind $site_10_0.but83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Make project to which the file belongs a current project}
    }
    button $site_10_0.but84 \
        -activeforeground limegreen -command {exec $editor $selecfile &} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -text Edit 
    bindtags $site_10_0.but84 "$site_10_0.but84 Button $base all _vTclBalloon"
    bind $site_10_0.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit selected file with the default editor}
    }
    button $site_10_0.but85 \
        -activeforeground red \
        -command {foreach el $selecfilelist {
   if {$el == ""} continue
   exec rm -f $el
}
set selecfile ""

adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Delete all selected} 
    bindtags $site_10_0.but85 "$site_10_0.but85 Button $base all _vTclBalloon"
    bind $site_10_0.but85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete ALL selected files}
    }
    pack $site_10_0.ent82 \
        -in $site_10_0 -anchor nw -expand 0 -fill x -padx 5 -side top 
    pack $site_10_0.but83 \
        -in $site_10_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 -side top 
    pack $site_10_0.but84 \
        -in $site_10_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 -side top 
    pack $site_10_0.but85 \
        -in $site_10_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 \
        -side bottom 
    pack $site_8_4.che119 \
        -in $site_8_4 -anchor nw -expand 0 -fill none -pady 2 -side left 
    pack $site_8_4.che120 \
        -in $site_8_4 -anchor nw -expand 0 -fill none -pady 2 -side left 
    pack $site_8_4.fra122 \
        -in $site_8_4 -anchor nw -expand 0 -fill none -ipadx 2 -ipady 2 \
        -padx 10 -pady 10 -side left 
    pack $site_8_4.fra122.but123 \
        -in $site_8_4.fra122 -anchor center -expand 0 -fill x -pady 4 \
        -side top 
    pack $site_8_4.fra122.but124 \
        -in $site_8_4.fra122 -anchor center -expand 0 -fill x -pady 4 \
        -side top 
    pack $site_8_4.fra122.but125 \
        -in $site_8_4.fra122 -anchor center -expand 0 -fill x -pady 4 \
        -side top 
    pack $site_8_4.scr77 \
        -in $site_8_4 -anchor nw -expand 1 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_4.ent78 \
        -in $site_8_4 -anchor nw -expand 0 -fill none -pady 10 -side top 
    pack $site_8_4.lab81 \
        -in $site_8_4 -anchor nw -expand 1 -fill y -side top 
    set site_8_5 [lindex [$base.tab88 childsite] 5]
    ::iwidgets::scrolledlistbox $site_8_5.scr77 \
        -background #d9d9d9 -hscrollmode dynamic -labelfont {Helvetica -12 } \
        -labelpos nw -labeltext {Archive directories:} \
        -listvariable archdirlist \
        -selectioncommand {set archdir [selection get]
set  archfiletmp [glob -nocomplain $archdir/*$archfilter*]
set archfilelist ""
foreach el $archfiletmp {
  if {[file isfile $el] == 1} {
  lappend archfilelist [file tail $el]
 }
}
set archfile ""} \
        -selectmode single -textbackground #ffffff -textfont {Helvetica -12 } \
        -vscrollmode dynamic -width 150 
    ::iwidgets::scrolledlistbox $site_8_5.scr78 \
        -background #d9d9d9 \
        -dblclickcommand {set selarchfilelist [selection get]
set archfile [lindex $selarchfilelist 0]
set archexten [file extension $archfile]
if {$archexten == ".ps"} {
  exec gv -landscape $archdir/$archfile &
} elseif {$archexten == ".zvd"} {
  exec xterm -e zvcomb $archdir/$archfile &
} else {
  exec $editor $archdir/$archfile &
}} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {Available files:} -listvariable archfilelist \
        -selectioncommand {set selarchfilelist [selection get]
set archfile [lindex $selarchfilelist 0]} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12 } -vscrollmode dynamic -width 150 
    frame $site_8_5.fra78 \
        -borderwidth 2 -height 75 -width 125 
    ::iwidgets::entryfield $site_8_5.fra78.ent79 \
        -background #d9d9d9 -command {adjourn .top75} \
        -labelfont {Helvetica -12 } -labeltext Archive: -textbackground white \
        -textvariable archdir -width 14 
    ::iwidgets::entryfield $site_8_5.fra78.ent80 \
        -background #d9d9d9 -command {adjourn .top75} \
        -labelfont {Helvetica -12 } -labeltext Filter: -textbackground white \
        -textvariable archfilter -width 14 
    ::iwidgets::labeledframe $site_8_5.fra78.lab81 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -labeltext {Selected file} 
    set site_11_0 [$site_8_5.fra78.lab81 childsite]
    entry $site_11_0.ent82 \
        -background #ffffff -insertbackground black -textvariable archfile \
        -width 15 
    button $site_11_0.but84 \
        -activeforeground limegreen \
        -command {exec $editor $archdir/$archfile &} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -text {Edit file} 
    bindtags $site_11_0.but84 "$site_11_0.but84 Button $base all _vTclBalloon"
    bind $site_11_0.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit selected file with the default editor}
    }
    button $site_11_0.but85 \
        -activeforeground red \
        -command {foreach el $selarchfilelist {
   if {$el == ""} continue
   exec rm -f $archdir/$el
}
set archfile ""
adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Delete selected} 
    bindtags $site_11_0.but85 "$site_11_0.but85 Button $base all _vTclBalloon"
    bind $site_11_0.but85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete all selected file}
    }
    pack $site_11_0.ent82 \
        -in $site_11_0 -anchor nw -expand 0 -fill x -padx 5 -side top 
    pack $site_11_0.but84 \
        -in $site_11_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 -side top 
    pack $site_11_0.but85 \
        -in $site_11_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 \
        -side bottom 
    frame $site_8_5.fra82 \
        -borderwidth 2 -height 75 -width 125 
    button $site_8_5.fra82.but84 \
        -activeforeground limegreen \
        -command {exec xterm -e store $archdir $file 
adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -text {<= Store project} 
    bindtags $site_8_5.fra82.but84 "$site_8_5.fra82.but84 Button $base all _vTclBalloon"
    bind $site_8_5.fra82.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store current project files in the selected directory (archieve)}
    }
    button $site_8_5.fra82.but86 \
        -activebackground red -activeforeground white \
        -command {exec rm -r -f $archdir
set archdirlist [glob -nocomplain */]
set archfilelist ""} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Delete archive} 
    bindtags $site_8_5.fra82.but86 "$site_8_5.fra82.but86 Button $base all _vTclBalloon"
    bind $site_8_5.fra82.but86 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delet whole archieve directory}
    }
    pack $site_8_5.scr77 \
        -in $site_8_5 -anchor nw -expand 1 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_5.scr78 \
        -in $site_8_5 -anchor nw -expand 1 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_5.fra78 \
        -in $site_8_5 -anchor nw -expand 0 -fill y -side left 
    pack $site_8_5.fra78.ent79 \
        -in $site_8_5.fra78 -anchor ne -expand 0 -fill none -padx 10 -pady 15 \
        -side top 
    pack $site_8_5.fra78.ent80 \
        -in $site_8_5.fra78 -anchor ne -expand 0 -fill none -padx 10 \
        -side top 
    pack $site_8_5.fra78.lab81 \
        -in $site_8_5.fra78 -anchor ne -expand 1 -fill y -ipady 5 -padx 5 \
        -pady 5 -side top 
    pack $site_8_5.fra82 \
        -in $site_8_5 -anchor ne -expand 1 -fill y -side top 
    pack $site_8_5.fra82.but84 \
        -in $site_8_5.fra82 -anchor nw -expand 0 -fill x -padx 5 -pady 12 \
        -side top 
    pack $site_8_5.fra82.but86 \
        -in $site_8_5.fra82 -anchor nw -expand 0 -fill x -padx 5 -pady 17 \
        -side bottom 
    set site_8_6 [lindex [$base.tab88 childsite] 6]
    frame $site_8_6.fra83 \
        -highlightcolor black 
    bindtags $site_8_6.fra83 "itk-destroy-.top75.tab88.canvas.notebook.cs.page9.cs.fra83 .top75.tab88.canvas.notebook.cs.page6.cs Frame $base all"
    bind $site_8_6.fra83 <Configure> {
        namespace inscope ::iwidgets::Tabnotebook {::.top75.tab88 _pageReconfigure .top75.tab88.canvas.notebook.cs.page9.cs.fra83 5 %w %h}
    }
    ::iwidgets::labeledframe $site_8_6.fra83.lab84 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -labeltext {Multiple run} 
    set site_11_0 [$site_8_6.fra83.lab84 childsite]
    frame $site_11_0.fra86 \
        -borderwidth 2 -height 75 -width 125 
    ::iwidgets::scrolledlistbox $site_11_0.fra86.scr87 \
        -hscrollmode none -labelpos nw \
        -selectioncommand {set stablist ""
set temp [selection get]
foreach el $temp {
set elf [split $el =]
lappend stablist [lindex $elf 0]
}} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12} -visibleitems 20x8 
    bindtags $site_11_0.fra86.scr87 "itk-delete-.top75.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra86.scr87 $site_11_0.fra86.scr87 Scrolledlistbox $base all _vTclBalloon"
    bind $site_11_0.fra86.scr87 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Load list stored under 'List name'}
    }
    button $site_11_0.fra86.but88 \
        -activeforeground limegreen -command {readabun } \
        -font {Helvetica -12 } -foreground darkgreen -text {Load all} 
    button $site_11_0.fra86.but76 \
        -activeforeground limegreen \
        -command {readabun ../RIPL-2/masses/abundance.dat} \
        -font {Helvetica -12 } -foreground darkgreen -text {Load stable} 
    frame $site_11_0.fra89 \
        -borderwidth 2 -height 75 -width 125 
    ::iwidgets::scrolledlistbox $site_11_0.fra89.scr87 \
        -hscrollmode none -labelfont {Helvetica -12 } -labelpos nw \
        -listvariable stablist \
        -selectioncommand {set selmulitem [selection get]} -selectmode single \
        -textbackground #ffffff -textfont {Helvetica -12 } -visibleitems 10x8 
    button $site_11_0.fra89.but88 \
        -command {lappend stablist $adnuc} -font {Helvetica -12 } -text ^ 
    entry $site_11_0.fra89.ent90 \
        -background white -textvariable adnuc -width 8 
    bindtags $site_11_0.fra89.ent90 "$site_11_0.fra89.ent90 Entry $base all _vTclBalloon"
    bind $site_11_0.fra89.ent90 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Additional nucleus (e.g., 26-Fe-56) to be included in the list}
    }
    frame $site_11_0.fra76 \
        -borderwidth 2 -height 75 -width 125 
    button $site_11_0.fra76.but77 \
        -activeforeground limegreen -command {set stablist ""} \
        -font {Helvetica -12 } -foreground darkgreen -text {Clear list} 
    ::iwidgets::entryfield $site_11_0.fra76.ent77 \
        -justify right -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {List name:} -textbackground #ffffff \
        -textvariable mulstname -width 12 
    button $site_11_0.fra76.but78 \
        -activeforeground limegreen \
        -command {set mulfile [open $mulstname.mulst w]
foreach el $stablist {
puts $mulfile $el
}
close $mulfile} \
        -font {Helvetica -12 } -foreground darkgreen -text {Save list} 
    bindtags $site_11_0.fra76.but78 "$site_11_0.fra76.but78 Button $base all _vTclBalloon"
    bind $site_11_0.fra76.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store list under "List name".mulst}
    }
    button $site_11_0.fra76.but79 \
        -activeforeground limegreen \
        -command {set mulfile [open $mulstname.mulst r]
while {[gets $mulfile line] >= 0} {
lappend  stablist $line 
}
close $mulfile} \
        -font {Helvetica -12 } -foreground darkgreen -text {Load list} 
    bindtags $site_11_0.fra76.but79 "$site_11_0.fra76.but79 Button $base all _vTclBalloon"
    bind $site_11_0.fra76.but79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Load list stored under 'List name'}
    }
    button $site_11_0.fra76.but76 \
        -activeforeground red -command {runlist $stablist $mulstname} \
        -font {Helvetica -12 } -foreground darkred -text {Run list} 
    bindtags $site_11_0.fra76.but76 "$site_11_0.fra76.but76 Button $base all _vTclBalloon"
    bind $site_11_0.fra76.but76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Launch full sequence of calculations for each element of the list}
    }
    ::iwidgets::optionmenu $site_11_0.opt86 \
        -command ViewAll -font {Helvetica -12 } -labelfont {Helvetica -12 } \
        -labeltext {} 
    $site_11_0.opt86 insert 1 {View:}
    $site_11_0.opt86 insert 2 {inputs}
    $site_11_0.opt86 insert 3 {warnings}
    $site_11_0.opt86 insert 4 {PLOTC4-log}
    $site_11_0.opt86 insert 5 {X4TOC4-log}
    $site_11_0.opt86 insert 6 {EMPEND-log}
    $site_11_0.opt86 insert 7 {int-omp}
    $site_11_0.opt86 insert 8 {dir-omp}
    $site_11_0.opt86 insert 9 {RIPL-omp}
    $site_11_0.opt86 insert 10 {levels}
    $site_11_0.opt86 insert 11 {coll-levels}
    $site_11_0.opt86 insert 12 {EXFORs}
    $site_11_0.opt86 insert 13 {C4s}
    $site_11_0.opt86 insert 14 {ENDFs}
    $site_11_0.opt86 insert 15 {short-outputs}
    $site_11_0.opt86 insert 16 {full-outputs}
    $site_11_0.opt86 insert 17 {PLOTC4-plots}
    ::iwidgets::checkbox $site_11_0.che79 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos n -labeltext {Keep only:} -relief flat 
    $site_11_0.che79 add chk0 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .lst -onvalue {} -selectcolor #00ff00 \
        -text {full output} -variable ckmlo 
    $site_11_0.che79 add chk1 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .out -onvalue {} -selectcolor #00ff00 \
        -text {short output} -variable ckmsh 
    $site_11_0.che79 add chk2 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -log* -onvalue {} -selectcolor #ffff00 \
        -text {log files} -variable ckmlog 
    $site_11_0.che79 add chk3 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue *.endf -onvalue {} -selectcolor #00ff00 \
        -text ENDF -variable ckmendf 
    $site_11_0.che79 add chk4 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .ps -onvalue {} -selectcolor #00ff00 \
        -text {PLOTC4 plots} -variable ckmplots 
    $site_11_0.che79 add chk5 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .exf -onvalue {} -selectcolor #ffff00 \
        -text EXFOR -variable ckmx4 
    $site_11_0.che79 add chk6 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .c4 -onvalue {} -selectcolor #ffff00 \
        -text {C4 file} -variable ckmc4 
    ::iwidgets::checkbox $site_11_0.che76 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -relief flat 
    $site_11_0.che76 add chk0 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -omp.int -onvalue {} -selectcolor #ffff00 \
        -text {int. omp} -variable ckmintomp 
    $site_11_0.che76 add chk1 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -omp.ripl -onvalue {} -selectcolor #ffff00 \
        -text {RIPL omp} -variable ckmriplomp 
    $site_11_0.che76 add chk2 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -omp.dir -onvalue {} -selectcolor #ffff00 \
        -text {direct omp} -variable ckmdiromp 
    $site_11_0.che76 add chk3 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .lev -onvalue {} -selectcolor #ffff00 \
        -text levels -variable ckmlev 
    $site_11_0.che76 add chk4 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -lev.col -onvalue {} -selectcolor #ffff00 \
        -text {coll. levels} -variable ckmcollev 
    $site_11_0.che76 add chk5 \
        -activebackground #dcdcdc -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground Black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .inp -onvalue {} -selectcolor #ffff00 \
        -text input -variable ckminp 
    ::iwidgets::labeledframe $site_11_0.lab76 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -labeltext {Selected item} 
    set site_13_0 [$site_11_0.lab76 childsite]
    entry $site_13_0.ent77 \
        -background white -textvariable selmulitem -width 15 
    bindtags $site_13_0.ent77 "$site_13_0.ent77 Entry $base all _vTclBalloon"
    bind $site_13_0.ent77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Selected element of the list}
    }
    button $site_13_0.but78 \
        -activeforeground limegreen \
        -command {setmulpro $selmulitem $mulstname} -font {Helvetica -12 } \
        -foreground darkgreen -text {Set as project} 
    bindtags $site_13_0.but78 "$site_13_0.but78 Button $base all _vTclBalloon"
    bind $site_13_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Make the selected list item the current project (input must have been saved)}
    }
    pack $site_13_0.ent77 \
        -in $site_13_0 -anchor center -expand 0 -fill none -padx 4 -side top 
    pack $site_13_0.but78 \
        -in $site_13_0 -anchor center -expand 0 -fill x -padx 5 -side top 
    ::iwidgets::labeledframe $site_11_0.lab79 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -labeltext Archive 
    set site_13_0 [$site_11_0.lab79 childsite]
    entry $site_13_0.ent77 \
        -background white -textvariable archdir -width 15 
    bindtags $site_13_0.ent77 "$site_13_0.ent77 Entry $base all _vTclBalloon"
    bind $site_13_0.ent77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Enetr name of the work subdirectory to store the results for the current list}
    }
    button $site_13_0.but78 \
        -activeforeground limegreen \
        -command {ArchiveList $archdir $stablist $mulstname
adjourn .top75} \
        -font {Helvetica -12 } -foreground darkgreen \
        -text {Store list results} 
    bindtags $site_13_0.but78 "$site_13_0.but78 Button $base all _vTclBalloon"
    bind $site_13_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store all files related to the list in the directory specified above }
    }
    pack $site_13_0.ent77 \
        -in $site_13_0 -anchor center -expand 0 -fill none -padx 4 -side top 
    pack $site_13_0.but78 \
        -in $site_13_0 -anchor center -expand 0 -fill x -padx 5 -side top 
    pack $site_11_0.fra86 \
        -in $site_11_0 -anchor center -expand 0 -fill y -padx 2 -side left 
    pack $site_11_0.fra86.scr87 \
        -in $site_11_0.fra86 -anchor n -expand 1 -fill y -side top 
    pack $site_11_0.fra86.but88 \
        -in $site_11_0.fra86 -anchor e -expand 0 -fill x -side right 
    pack $site_11_0.fra86.but76 \
        -in $site_11_0.fra86 -anchor w -expand 0 -fill x -side left 
    pack $site_11_0.fra89 \
        -in $site_11_0 -anchor center -expand 0 -fill y -side left 
    pack $site_11_0.fra89.scr87 \
        -in $site_11_0.fra89 -anchor n -expand 1 -fill y -side top 
    pack $site_11_0.fra89.but88 \
        -in $site_11_0.fra89 -anchor center -expand 0 -fill none -side right 
    pack $site_11_0.fra89.ent90 \
        -in $site_11_0.fra89 -anchor center -expand 0 -fill both -side left 
    pack $site_11_0.fra76 \
        -in $site_11_0 -anchor center -expand 0 -fill y -ipadx 2 -padx 2 \
        -side left 
    pack $site_11_0.fra76.but77 \
        -in $site_11_0.fra76 -anchor center -expand 0 -fill x -side top 
    pack $site_11_0.fra76.ent77 \
        -in $site_11_0.fra76 -anchor center -expand 0 -fill none -side top 
    pack $site_11_0.fra76.but78 \
        -in $site_11_0.fra76 -anchor center -expand 0 -fill x -side top 
    pack $site_11_0.fra76.but79 \
        -in $site_11_0.fra76 -anchor center -expand 0 -fill x -side top 
    pack $site_11_0.fra76.but76 \
        -in $site_11_0.fra76 -anchor center -expand 0 -fill x -side bottom 
    pack $site_11_0.opt86 \
        -in $site_11_0 -anchor w -expand 0 -fill none -side bottom 
    pack $site_11_0.che79 \
        -in $site_11_0 -anchor nw -expand 0 -fill none -side left 
    pack $site_11_0.che76 \
        -in $site_11_0 -anchor ne -expand 0 -fill none -side left 
    pack $site_11_0.lab76 \
        -in $site_11_0 -anchor n -expand 0 -fill none -ipady 3 -side top 
    pack $site_11_0.lab79 \
        -in $site_11_0 -anchor n -expand 0 -fill none -ipady 5 -side top 
    pack $site_8_6.fra83 \
        -in $site_8_6 -anchor center -expand 1 -fill both -side top 
    pack $site_8_6.fra83.lab84 \
        -in $site_8_6.fra83 -anchor center -expand 1 -fill both -side left 
    set site_8_7 [lindex [$base.tab88 childsite] 7]
    frame $site_8_7.fra84 \
        -highlightcolor black 
    bindtags $site_8_7.fra84 "itk-destroy-.top75.tab88.canvas.notebook.cs.page10.cs.fra84 .top75.tab88.canvas.notebook.cs.page7.cs Frame $base all"
    bind $site_8_7.fra84 <Configure> {
        namespace inscope ::iwidgets::Tabnotebook {::.top75.tab88 _pageReconfigure .top75.tab88.canvas.notebook.cs.page10.cs.fra84 6 %w %h}
    }
    ::iwidgets::scrolledlistbox $site_8_7.fra84.scrolledlistbox83 \
        -background #d9d9d9 -cursor {} \
        -dblclickcommand {exec $editor ../source/[selection get] &} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {Double click to edit} -listvariable modules \
        -textbackground #ffffff -textfont {Helvetica -12 } -width 180 
    frame $site_8_7.fra84.fra79 \
        -borderwidth 2 -height 75 -width 125 
    button $site_8_7.fra84.fra79.but80 \
        -activeforeground red -command {exec $editor ../source/dimension.h &} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Edit dimensions} -wraplength 70 
    bindtags $site_8_7.fra84.fra79.but80 "$site_8_7.fra84.fra79.but80 Button $base all _vTclBalloon"
    bind $site_8_7.fra84.fra79.but80 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit dimension.h file setting dimensions in EMPIRE}
    }
    button $site_8_7.fra84.fra79.but81 \
        -activeforeground red -command {cd ../
exec xterm -e Compile
cd work} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text {Make all} 
    bindtags $site_8_7.fra84.fra79.but81 "$site_8_7.fra84.fra79.but81 Button $base all _vTclBalloon"
    bind $site_8_7.fra84.fra79.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compile whole the package (including utility codes)}
    }
    button $site_8_7.fra84.fra79.but79 \
        -activeforeground red \
        -command {cd ../source
exec xterm -e make
cd ../work} \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -text Make 
    bindtags $site_8_7.fra84.fra79.but79 "$site_8_7.fra84.fra79.but79 Button $base all _vTclBalloon"
    bind $site_8_7.fra84.fra79.but79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compile EMPIRE}
    }
    ::iwidgets::labeledframe $site_8_7.fra84.lab80 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* \
        -labeltext ECIS 
    set site_11_0 [$site_8_7.fra84.lab80 childsite]
    button $site_11_0.but81 \
        -activebackground #dcdcdc -activeforeground limegreen \
        -command {exec $editor $file-ecis.in &} -disabledforeground #a1a4a1 \
        -font {Helvetica -12} -foreground darkgreen \
        -highlightbackground #dcdcdc -highlightcolor #000000 \
        -text {Edit input} 
    bindtags $site_11_0.but81 "$site_11_0.but81 Button $base all _vTclBalloon"
    bind $site_11_0.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {ECIS input for the last incident energy in the last EMPIRE run}
    }
    button $site_11_0.but82 \
        -activebackground #dcdcdc -activeforeground red \
        -command {exec xterm -e ../source/ecis <$file-ecis.in >$file-ecis.out &
adjourn .top75} \
        -disabledforeground #a1a4a1 -font {Helvetica -12} -foreground darkred \
        -highlightbackground #dcdcdc -highlightcolor #000000 -text {Run ECIS} 
    bindtags $site_11_0.but82 "$site_11_0.but82 Button $base all _vTclBalloon"
    bind $site_11_0.but82 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run ECIS as a stand alone code}
    }
    button $site_11_0.but83 \
        -activebackground #dcdcdc -activeforeground limegreen \
        -command {exec $editor $file-ecis.out &} -disabledforeground #a1a4a1 \
        -font {Helvetica -12} -foreground darkgreen \
        -highlightbackground #dcdcdc -highlightcolor #000000 \
        -text {View output} 
    bindtags $site_11_0.but83 "$site_11_0.but83 Button $base all _vTclBalloon"
    bind $site_11_0.but83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View ECIS output}
    }
    pack $site_11_0.but81 \
        -in $site_11_0 -anchor center -expand 0 -fill x -padx 10 -pady 5 \
        -side top 
    pack $site_11_0.but82 \
        -in $site_11_0 -anchor center -expand 0 -fill x -padx 10 -pady 5 \
        -side top 
    pack $site_11_0.but83 \
        -in $site_11_0 -anchor center -expand 0 -fill x -padx 10 -pady 5 \
        -side top 
    pack $site_8_7.fra84 \
        -in $site_8_7 -anchor center -expand 1 -fill both -side top 
    pack $site_8_7.fra84.scrolledlistbox83 \
        -in $site_8_7.fra84 -anchor nw -expand 0 -fill y -padx 10 -pady 5 \
        -side left 
    pack $site_8_7.fra84.fra79 \
        -in $site_8_7.fra84 -anchor nw -expand 0 -fill none -ipady 5 -pady 27 \
        -side left 
    pack $site_8_7.fra84.fra79.but80 \
        -in $site_8_7.fra84.fra79 -anchor center -expand 0 -fill x -ipady 2 \
        -side top 
    pack $site_8_7.fra84.fra79.but81 \
        -in $site_8_7.fra84.fra79 -anchor center -expand 0 -fill x -ipady 10 \
        -pady 5 -side bottom 
    pack $site_8_7.fra84.fra79.but79 \
        -in $site_8_7.fra84.fra79 -anchor center -expand 0 -fill x -ipady 10 \
        -side bottom 
    pack $site_8_7.fra84.lab80 \
        -in $site_8_7.fra84 -anchor n -expand 0 -fill none -padx 5 -pady 10 \
        -side right 
    $base.tab88 select 0
    menu $base.m88 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -cursor {} -foreground #000000 
    $base.m88 add cascade \
        -menu "$base.m88.menu89" -label File 
    $base.m88 add cascade \
        -menu "$base.m88.menu90" -label Options 
    $base.m88 add cascade \
        -menu "$base.m88.menu92" -label Inputs 
    $base.m88 add cascade \
        -menu "$base.m88.menu93" -label Execute 
    $base.m88 add cascade \
        -menu "$base.m88.menu94" -label Outputs 
    $base.m88 add cascade \
        -menu "$base.m88.menu95" -label Plots 
    $base.m88 add cascade \
        -menu "$base.m88.men77" -label Clean 
    $base.m88 add cascade \
        -menu "$base.m88.men78" -label Source 
    $base.m88 add separator
    $base.m88 add cascade \
        -menu "$base.m88.menu96" -label Help 
    menu $base.m88.menu89 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu89 add command \
        -command exit -label Exit 
    menu $base.m88.menu90 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu90 add command \
        -command {exec $editor skel.inp &} -label {Default input} 
    $base.m88.menu90 add command \
        -command {exec $editor EMPEND.INP &} -label {EMPEND input} 
    $base.m88.menu90 add command \
        -command {exec $editor ../util/plotc4/PLOTC4.INP &} \
        -label {PLOTC4 input} 
    $base.m88.menu90 add command \
        -command {exec $editor ../util/fixup/FIXUP.INP &} \
        -label {FIXUP input} 
    $base.m88.menu90 add command \
        -command {exec $editor ../util/c4sort/C4SORT.INP &} \
        -label {C4SORT input} 
    $base.m88.menu90 add command \
        -command {exec $editor ../source/Makefile &} -label {Edit Makefile} 
    $base.m88.menu90 add cascade \
        -menu "$base.m88.menu90.menu97" -label {Select editor} 
    menu $base.m88.menu90.menu97 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu90.menu97 add radiobutton \
        -value gvim -variable editor -label gvim 
    $base.m88.menu90.menu97 add radiobutton \
        -value emacs -variable editor -label emacs 
    $base.m88.menu90.menu97 add radiobutton \
        -value xemacs -variable editor -label xemacs 
    $base.m88.menu90.menu97 add radiobutton \
        -value kedit -variable editor -label kedit 
    $base.m88.menu90.menu97 add radiobutton \
        -value kate -variable editor -label kate 
    $base.m88.menu90.menu97 add radiobutton \
        -value nedit -variable editor -label nedit 
    $base.m88.menu90.menu97 add radiobutton \
        -value gedit -variable editor -label gedit 
    menu $base.m88.menu92 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu92 add command \
        -command {exec {cp skelet.inp
$file.inp &}} -label Create 
    $base.m88.menu92 add command \
        -command {exec $editor $file.inp &} -label Edit 
    $base.m88.menu92 add separator
    $base.m88.menu92 add command \
        -command {exec $editor $file-omp.int &} -label {Internal omp} 
    $base.m88.menu92 add command \
        -command {exec $editor $file-omp.ripl &} -label {RIPL omp} 
    $base.m88.menu92 add command \
        -command {exec $editor $file-omp.dir &} -label {Direct omp} 
    $base.m88.menu92 add separator
    $base.m88.menu92 add command \
        -command {exec $editor $file.lev &} -label {All levels} 
    $base.m88.menu92 add command \
        -command {exec $editor $file-lev.col &} -label {Coll. levels} 
    menu $base.m88.menu93 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu93 add command \
        -command {exec xterm -e run $file &} -label Run+Form+Plot 
    $base.m88.menu93 add separator
    $base.m88.menu93 add command \
        -command {exec xterm -e runE $file &} -label Run 
    $base.m88.menu93 add command \
        -command {exec xterm -e format $file &} -label Format 
    $base.m88.menu93 add command \
        -command {exec xterm -e plot $file &} -label Plot 
    $base.m88.menu93 add separator
    $base.m88.menu93 add command \
        -command {exec xterm -e c4 $file &} -label X4TOC4 
    $base.m88.menu93 add command \
        -command {exec xterm -e sortc4 $file &} -label SORTC4 
    $base.m88.menu93 add separator
    $base.m88.menu93 add command \
        \
        -command {exec xterm -e ../source/ecis <$file-ecis.in >$file-ecis.out &} \
        -label ECIS 
    menu $base.m88.menu94 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu94 add command \
        -command {exec $editor $file.lst &} -label {EMPIRE full} 
    $base.m88.menu94 add command \
        -command {exec $editor $file.out &} -label {EMPIRE short} 
    $base.m88.menu94 add separator
    $base.m88.menu94 add command \
        -command {exec $editor $file-ecis.out &} -label {ECIS } 
    $base.m88.menu94 add separator
    $base.m88.menu94 add command \
        -command {exec $editor $file.exf &} -label EXFOR 
    $base.m88.menu94 add command \
        -command {exec $editor $file.c4 &} -label {C4 file} 
    $base.m88.menu94 add separator
    $base.m88.menu94 add command \
        -command {exec $editor $file.endf &} -label {ENDF file} 
    $base.m88.menu94 add command \
        -command {exec $editor $file-f.endf &} -label {ENDF fixuped} 
    $base.m88.menu94 add command \
        -command {exec $editor $file-s.endf &} -label {ENDF sixtabed} 
    menu $base.m88.menu95 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu95 add command \
        -command {exec gv $file.ps &} -label {PLOTC4 plots} 
    $base.m88.menu95 add separator
    $base.m88.menu95 add command \
        -command {exec xterm -e zvpl $file &} -label {Create ZVV plot} 
    $base.m88.menu95 add command \
        -command {exec xterm -e zvcomb &} -label {Merge ZVV plots} 
    $base.m88.menu95 add command \
        -command {exec guizvv.tcl $file &} -label {Compare ZVV} 
    $base.m88.menu95 add separator
    $base.m88.menu95 add command \
        -command {exec gv -landscape $file-cum.ps &} -label {Cumul. levels} 
    menu $base.m88.menu96 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $base.m88.menu96 add command \
        -command {exec $editor inplist &} -label {EMPIRE input} 
    $base.m88.menu96 add command \
        -command {exec $editor ../RIPL-2/optical/om-data/om-index.txt &} \
        -label {RIPL omp} 
    $base.m88.menu96 add command \
        -command {exec $editor ../hints.txt &} -label FAQ 
    $base.m88.menu96 add command \
        -command {exec gv -portrait ../doc/empire.ps &} \
        -label {EMPIRE manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/empend/manual.txt &} \
        -label {EMPEND manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/c4sort/manual.txt &} \
        -label {C4SORT manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/legend/manual.txt &} \
        -label {LEGEND manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/plotc4/manual.txt &} \
        -label {PLOTC4 manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/x4toc4/manual.txt &} \
        -label {X4TOC4 manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/fixup/manual.txt &} \
        -label {FIXUP manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/lsttab/manual.txt &} \
        -label {LSTTAB manual} 
    $base.m88.menu96 add command \
        -command {exec $editor ../util/sixtab/manual.txt &} \
        -label {SIXTAB manual} 
    menu $base.m88.men77 \
        -disabledforeground #a1a4a1 -tearoff 0 
    $base.m88.men77 add command \
        -command {exec clean $file &} -label {Clean project} 
    $base.m88.men77 add command \
        -command {exec clean $file
exec rm -f $file.inp} -label {Delete project} 
    menu $base.m88.men78 \
        -disabledforeground #a1a4a1 -tearoff 0 
    $base.m88.men78 add command \
        -command {exec $editor ../source/dimension.h &} -label Dimensions 
    $base.m88.men78 add command \
        -command {exec cd ../source
exec xterm -e make &
exec cd ../work} \
        -label Compile 
    menu $base.m76 \
        -disabledforeground #a1a4a1 -tearoff 1 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.fra77 \
        -in $base -anchor center -expand 0 -fill x -side top 
    pack $base.fra77.but78 \
        -in $base.fra77 -anchor center -expand 0 -fill none -side left 
    pack $base.fra77.ent79 \
        -in $base.fra77 -anchor center -expand 0 -fill y -side left 
    pack $base.fra77.but80 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 3 \
        -side left 
    pack $base.fra77.but86 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 5 \
        -side left 
    pack $base.fra77.but87 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 3 \
        -side left 
    pack $base.fra77.but81 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 3 \
        -side left 
    pack $base.fra77.but77 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 3 \
        -side left 
    pack $base.fra77.but82 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 3 \
        -side left 
    pack $base.fra77.but83 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 5 \
        -side left 
    pack $base.fra77.but84 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 5 \
        -side left 
    pack $base.fra77.but85 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 5 \
        -side left 
    pack $base.fra77.but76 \
        -in $base.fra77 -anchor center -expand 0 -fill none -ipadx 3 \
        -side left 
    pack $base.fra77.lab77 \
        -in $base.fra77 -anchor center -expand 0 -fill x -side left 
    pack $base.fra77.button77 \
        -in $base.fra77 -anchor center -expand 0 -fill none -side left 
    pack $base.tab88 \
        -in $base -anchor center -expand 1 -fill both -side top 

    vTcl:FireEvent $base <<Ready>>
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
Window show .top75

main $argc $argv
