# $Rev: 2750 $
# $Author: mherman $
# $Date: 2012-03-27 00:09:36 +0200 (Di, 27 Mär 2012) $
#
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

        {{[file join / home herman empire work Projects fileopen.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects launch.gif]} {file not found!} user {
R0lGODlhGAAYAIQAAAAAAGsAAMoAAdMAAfwCAvsAEP9bN/xoLPxlNPhwMvxk
QPyZAf2ZEPyYKv2VN/ujHv+gNP39Av/CV/3KZv/Md//QZ/v+/P//////////
/////////////////////////yH5BAEBAB8ALAAAAAAYABgAAAW34CeOZDkG
aACYLIk6k6O2bWA104PMdBkgDosFUeD1RDaIREhQrY6fwKEhRBAsTlYqZYEx
BVlfwNFo7BBUi+EadikQ8J0Bxxy0T4LBQJCQNio/bABPLlsoCThSBGCDLIOP
P4lWYHcjjwAvFIFgfYQ0mBOBdkBGNAwRAQmLUqSeJgwLEbJbAq00C7GyDJiY
AzuNLbiyC5cBvqUlwrjEeMjJEcvAUc4ksbiPLtIs0Ngl2o7fUCTh4iMhADs=
}}
        {{[file join / home herman empire work Projects editcopy.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects edit.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects stop.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects imagegallery.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects kthememgr.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects mini-run.gif]} {file not found!} user {
R0lGODlhGwAYAIAAAAAAAP///yH5BAEKAAEALAAAAAAbABgAAAI6jI+py+0P
owJUNlrtwprxPmUaRooScJQm6rCB6r6W+n6nHdnlKO43jPtVfCADsXgEJZGm
orHJjDkfBQA7}}
        {{[file join / home herman empire work Projects kleandisk.gif]} {file not found!} user {
R0lGODlhHAAYAIQAAAAAAC0qHPO5U7aRSA8ODY12QYdxP3loPXJkPMueS8SZ
SreQR7CMRqKDRJt/Q+CrTtmnTWRbOjYyIvO9Z+evTz44JDo1I15XOaSFRUhC
K1FKL//sXv+1ZVpSNc6IM////yH5BAEKAB8ALAAAAAAcABgAAAXS4CeO5Aic
ZaqqgTAEa8y6sEgQcty+Np7PvN4PWLv9AsiaKPAq+lbIguGASO6cskBCsWA0
HFIqDfAx6h6QbfcrdUUkJyR0QqlT0GpaxUKNXJIlVnZ3aTRsUwh+KCMBGI5W
AXZ4XF4OCAQZZCYnjp0YgpNdmBqaJhsnqJ6fVg8EGpmlIgCnHLOonKoBHaQe
sR8AtbW0trcAnx0nvWa/wrbNwcCovcpm0cOnw9DTANTV0c3Ytqfb3U+/qNfi
3OsAN8spt9Dj6+7mMsXsBItD8O36KSEAADs=}}
        {{[file join / home herman empire work Projects x4.gif]} {file not found!} user {
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
        {{[file join / home herman empire work Projects reload.gif]} {user image} user {
R0lGODlhGAAYAOcAABNRDaG9mai/oavApZ68lh9aGRhTEhlVE4u4f425gYq4
f422hRxYFoS2e4K0eXmrcIGzeIO1eoCzd3mochlMEXyydAw2UQsmA1mGUVyO
VWygZXKoanetb4GzeWWaXRxGE3Svba7d6Uh2QVOFTFWKTluTVGqlY3evb1eP
UBc6DmurZofD1wwoBUh6REZ5P0uHRlmXVGemYWqmZAsmBH+80T9xOUJ8PU2M
SleXUmKlX2OoYGWpYjxtNxE1CHe1zDVrMUeFRE+PS1mcVlukWU6LSm+txjl0
NkqKRlOYUFKhUlKVTw09XJHF12alwGemwWOjwFmYtQJCZjqIOT6IP0eLRk6Y
TEqdS0+bTyBeFQguQHWtw1ycuV+fvFOTsEJ4kzWANj6TQUGUQkKRRESXRUKa
RTmNOTeMOQcqO0yFoVaWtleXtk+NrD52kh1WdgwoCiltKzaNOjqUPTqWPzmW
PjeTPDKLNit5LQs0TjdvjVKRslCQsUuLrD98nS5hfSRtJS2GMTOSODGQNi2L
MyZ6KQcvOlKMq0aGqkiIq0OCpTZzlC5nhx9rICeEKyeJLh5rICtlhChffzh6
nz5/pECBpj19oTFsjyllhxx5Ihx3IStoiDN0mzd5oDh6oTl6oTRzmCxqjSVh
hRxSbydjhTBxmjBymy5ulSlojyNghSNegSBdgChrliZnkSZpkQ0zPhlTdx5e
iSBjkB5fixxaggwyPf//////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
/////////////////////yH5BAEKAP8ALAAAAAAYABgAAAj+AP8JHEiwoMGD
BAEoVIiwocIAAgYMIFDAQMOCByQiSMBRAYEFDC4KBDCgQQMHDyA0iBBBwgQK
F0lWsHABQwYNGzhUqNDBwweHICyECHFBxAgSJUyAAHECRYqDAFQIXUGVRQsX
L2DEUKFCxgyoQmmIFXuhho0bOHLo2MGjh0GhPuLK9XHhB5AgQoYMIXKhoNAi
gAMHvmDkCJIkSZS4JWjBwhImTZw4eQIlCgApU6hUsWLlChaGBLNo2cKFSxcv
F76ACSNmDBkyZcx86UvwDJo0atSsYdPGzRs4ceTMoVPHDm2Cd/Dk0aNnD58+
bS74+QMokKBBFwgdtFDI0KFDiBJhKYq+iFEjRxceaX8LKZKkSZMoVbIU/RKm
C23Wb8+kaROnTp58AkooF+Cn3kWEiDIKKQyWYsopqLQhin4NEZKKKhhiuAor
bbQi0kCEEOLKK7DEIkuIHxY0S4gopujiiy4GBAA7}}
        {{[file join / home herman empire work Projects shortout.gif]} {user image} user {
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
    "-family times -size 12" \
    stock \
    vTcl:font4
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
## Library Procedure:  ::combobox2::Build

namespace eval ::combobox2 {
proc Build {w args} {
    variable widgetOptions

    if {[winfo exists $w]} {
    error "window name \"$w\" already exists"
    }

    # create the namespace for this instance, and define a few
    # variables
    namespace eval ::combobox2::$w {

    variable ignoreTrace 0
    variable oldFocus    {}
    variable oldGrab     {}
    variable oldValue    {}
    variable options
    variable this
    variable widgets

    set widgets(foo) foo  ;# coerce into an array
    set options(foo) foo  ;# coerce into an array

    unset widgets(foo)
    unset options(foo)
    }

    # import the widgets and options arrays into this proc so
    # we don't have to use fully qualified names, which is a
    # pain.
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

    # this is our widget -- a frame of class Combobox2. Naturally,
    # it will contain other widgets. We create it here because
    # we need it to be able to set our default options.
    set widgets(this)   [frame  $w -class Combobox2 -takefocus 0]
    set widgets(entry)  [entry  $w.entry -takefocus 1 -width 8]
    set widgets(button) [label  $w.button -takefocus 0] 

    # this defines all of the default options. We get the
    # values from the option database. Note that if an array
    # value is a list of length one it is an alias to another
    # option, so we just ignore it
    foreach name [array names widgetOptions] {
    if {[llength $widgetOptions($name)] == 1} continue
    set optName  [lindex $widgetOptions($name) 0]
    set optClass [lindex $widgetOptions($name) 1]
    set value [option get $w $optName $optClass]
    set options($name) $value
    }

    # if -value is set to null, we'll remove it from our
    # local array. The assumption is, if the user sets it from
    # the option database, they will set it to something other
    # than null (since it's impossible to determine the difference
    # between a null value and no value at all).
    if {[info exists options(-value)]  && [string length $options(-value)] == 0} {
    unset options(-value)
    }

    # we will later rename the frame's widget proc to be our
    # own custom widget proc. We need to keep track of this
    # new name, so we'll define and store it here...
    set widgets(frame) ::combobox2::${w}::$w

    # gotta do this sooner or later. Might as well do it now
    pack $widgets(entry)  -side left  -fill both -expand yes
    pack $widgets(button) -side right -fill y    -expand no

    # I should probably do this in a catch, but for now it's
    # good enough... What it does, obviously, is put all of
    # the option/values pairs into an array. Make them easier
    # to handle later on...
    array set options $args

    # now, the dropdown list... the same renaming nonsense
    # must go on here as well...
    set widgets(popup)   [toplevel  $w.top]
    set widgets(listbox) [listbox   $w.top.list]
    set widgets(vsb)     [scrollbar $w.top.vsb]

    pack $widgets(listbox) -side left -fill both -expand y

    # fine tune the widgets based on the options (and a few
    # arbitrary values...)

    # NB: we are going to use the frame to handle the relief
    # of the widget as a whole, so the entry widget will be 
    # flat. This makes the button which drops down the list
    # to appear "inside" the entry widget.

    $widgets(vsb) configure  -command "$widgets(listbox) yview"  -highlightthickness 0

    $widgets(button) configure  -highlightthickness 0  -borderwidth 1  -relief raised  -width [expr {[winfo reqwidth $widgets(vsb)] - 2}]

    $widgets(entry) configure  -borderwidth 0  -relief flat  -highlightthickness 0 

    $widgets(popup) configure  -borderwidth 1  -relief sunken

    $widgets(listbox) configure  -selectmode browse  -background [$widgets(entry) cget -bg]  -yscrollcommand "$widgets(vsb) set"  -exportselection false  -borderwidth 0


#    trace variable ::combobox2::${w}::entryTextVariable w  #       [list ::combobox2::EntryTrace $w]
    
    # do some window management foo on the dropdown window
    wm overrideredirect $widgets(popup) 1
    wm transient        $widgets(popup) [winfo toplevel $w]
    wm group            $widgets(popup) [winfo parent $w]
    wm resizable        $widgets(popup) 0 0
    wm withdraw         $widgets(popup)
    
    # this moves the original frame widget proc into our
    # namespace and gives it a handy name
    rename ::$w $widgets(frame)

    # now, create our widget proc. Obviously (?) it goes in
    # the global namespace. All combobox2 widgets will actually
    # share the same widget proc to cut down on the amount of
    # bloat. 
    proc ::$w {command args}  "eval ::combobox2::WidgetProc $w \$command \$args"


    # ok, the thing exists... let's do a bit more configuration. 
    if {[catch "::combobox2::Configure $widgets(this) [array get options]" error]} {
    catch {destroy $w}
    error $error
    }

    return ""

}
}
#############################################################################
## Library Procedure:  ::combobox2::CallCommand

namespace eval ::combobox2 {
proc CallCommand {w newValue} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options
    
    # call the associated command, if defined and -commandstate is
    # set to "normal"
    if {$options(-commandstate) == "normal" &&  [string length $options(-command)] > 0} {
    set args [list $widgets(this) $newValue]
    uplevel \#0 $options(-command) $args
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::Canonize

namespace eval ::combobox2 {
proc Canonize {w object opt} {
    variable widgetOptions
    variable columnOptions
    variable widgetCommands
    variable listCommands
    variable scanCommands

    switch $object {
    command {
        if {[lsearch -exact $widgetCommands $opt] >= 0} {
        return $opt
        }

        # command names aren't stored in an array, and there
        # isn't a way to get all the matches in a list, so
        # we'll stuff the commands in a temporary array so
        # we can use [array names]
        set list $widgetCommands
        foreach element $list {
        set tmp($element) ""
        }
        set matches [array names tmp ${opt}*]
    }

    {list command} {
        if {[lsearch -exact $listCommands $opt] >= 0} {
        return $opt
        }

        # command names aren't stored in an array, and there
        # isn't a way to get all the matches in a list, so
        # we'll stuff the commands in a temporary array so
        # we can use [array names]
        set list $listCommands
        foreach element $list {
        set tmp($element) ""
        }
        set matches [array names tmp ${opt}*]
    }

    {scan command} {
        if {[lsearch -exact $scanCommands $opt] >= 0} {
        return $opt
        }

        # command names aren't stored in an array, and there
        # isn't a way to get all the matches in a list, so
        # we'll stuff the commands in a temporary array so
        # we can use [array names]
        set list $scanCommands
        foreach element $list {
        set tmp($element) ""
        }
        set matches [array names tmp ${opt}*]
    }

    option {
        if {[info exists widgetOptions($opt)]  && [llength $widgetOptions($opt)] == 2} {
        return $opt
        }
        set list [array names widgetOptions]
        set matches [array names widgetOptions ${opt}*]
    }

    }

    if {[llength $matches] == 0} {
    set choices [HumanizeList $list]
    error "unknown $object \"$opt\"; must be one of $choices"

    } elseif {[llength $matches] == 1} {
    set opt [lindex $matches 0]

    # deal with option aliases
    switch $object {
        option {
        set opt [lindex $matches 0]
        if {[llength $widgetOptions($opt)] == 1} {
            set opt $widgetOptions($opt)
        }
        }
    }

    return $opt

    } else {
    set choices [HumanizeList $list]
    error "ambiguous $object \"$opt\"; must be one of $choices"
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::ComputeGeometry

namespace eval ::combobox2 {
proc ComputeGeometry {w} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

    if {$options(-height) == 0 && $options(-maxheight) != "0"} {
    # if this is the case, count the items and see if
    # it exceeds our maxheight. If so, set the listbox
    # size to maxheight...
    set nitems [$widgets(listbox) size]
    if {$nitems > $options(-maxheight)} {
        # tweak the height of the listbox
        $widgets(listbox) configure -height $options(-maxheight)
    } else {
        # un-tweak the height of the listbox
        $widgets(listbox) configure -height 0
    }
    update idletasks
    }

    # compute height and width of the dropdown list
    set bd [$widgets(popup) cget -borderwidth]
    set height [expr {[winfo reqheight $widgets(popup)] + $bd + $bd}]
    set width [winfo width $widgets(this)]

    # figure out where to place it on the screen, trying to take into
    # account we may be running under some virtual window manager
    set screenWidth  [winfo screenwidth $widgets(this)]
    set screenHeight [winfo screenheight $widgets(this)]
    set rootx        [winfo rootx $widgets(this)]
    set rooty        [winfo rooty $widgets(this)]
    set vrootx       [winfo vrootx $widgets(this)]
    set vrooty       [winfo vrooty $widgets(this)]

    # the x coordinate is simply the rootx of our widget, adjusted for
    # the virtual window. We won't worry about whether the window will
    # be offscreen to the left or right -- we want the illusion that it
    # is part of the entry widget, so if part of the entry widget is off-
    # screen, so will the list. If you want to change the behavior,
    # simply change the if statement... (and be sure to update this
    # comment!)
    set x  [expr {$rootx + $vrootx}]
    if {0} { 
    set rightEdge [expr {$x + $width}]
    if {$rightEdge > $screenWidth} {
        set x [expr {$screenWidth - $width}]
    }
    if {$x < 0} {set x 0}
    }

    # the y coordinate is the rooty plus vrooty offset plus
    # the height of the static part of the widget plus 1 for a 
    # tiny bit of visual separation...
    set y [expr {$rooty + $vrooty + [winfo reqheight $widgets(this)] + 1}]
    set bottomEdge [expr {$y + $height}]

    if {$bottomEdge >= $screenHeight} {
    # ok. Fine. Pop it up above the entry widget isntead of
    # below.
    set y [expr {($rooty - $height - 1) + $vrooty}]

    if {$y < 0} {
        # this means it extends beyond our screen. How annoying.
        # Now we'll try to be real clever and either pop it up or
        # down, depending on which way gives us the biggest list. 
        # then, we'll trim the list to fit and force the use of
        # a scrollbar

        # (sadly, for windows users this measurement doesn't
        # take into consideration the height of the taskbar,
        # but don't blame me -- there isn't any way to detect
        # it or figure out its dimensions. The same probably
        # applies to any window manager with some magic windows
        # glued to the top or bottom of the screen)

        if {$rooty > [expr {$screenHeight / 2}]} {
        # we are in the lower half of the screen -- 
        # pop it up. Y is zero; that parts easy. The height
        # is simply the y coordinate of our widget, minus
        # a pixel for some visual separation. The y coordinate
        # will be the topof the screen.
        set y 1
        set height [expr {$rooty - 1 - $y}]

        } else {
        # we are in the upper half of the screen --
        # pop it down
        set y [expr {$rooty + $vrooty +  [winfo reqheight $widgets(this)] + 1}]
        set height [expr {$screenHeight - $y}]

        }

        # force a scrollbar
        HandleScrollbar $widgets(this) crop
    }
    }

    if {$y < 0} {
    # hmmm. Bummer.
    set y 0
    set height $screenheight
    }

    set geometry [format "=%dx%d+%d+%d" $width $height $x $y]

    return $geometry
}
}
#############################################################################
## Library Procedure:  ::combobox2::Configure

namespace eval ::combobox2 {
proc Configure {w args} {
    variable widgetOptions
    variable defaultEntryCursor

    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

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
            # modif by Christian Gavin 08/02/2000
            # if an option has been removed from the list
            # (e.g. -value), don't try to access it
            if [info exists options($opt)] {
            set optName  [lindex $widgetOptions($opt) 0]
            set optClass [lindex $widgetOptions($opt) 1]
            set default [option get $w $optName $optClass]
            lappend results [list $opt $optName $optClass  $default $options($opt)]
        }
        }
    }

    return $results
    }
    
    # one argument means we are looking for configuration
    # information on a single option
    if {[llength $args] == 1} {
    set opt [::combobox2::Canonize $w option [lindex $args 0]]

    set optName  [lindex $widgetOptions($opt) 0]
    set optClass [lindex $widgetOptions($opt) 1]
    set default [option get $w $optName $optClass]
    set results [list $opt $optName $optClass  $default $options($opt)]
    return $results
    }

    # if we have an odd number of values, bail. 
    if {[expr {[llength $args]%2}] == 1} {
    # hmmm. An odd number of elements in args
    error "value for \"[lindex $args end]\" missing"
    }
    
    # Great. An even number of options. Let's make sure they 
    # are all valid before we do anything. Note that Canonize
    # will generate an error if it finds a bogus option; otherwise
    # it returns the canonical option name
    foreach {name value} $args {
    set name [::combobox2::Canonize $w option $name]
    set opts($name) $value
    }

    # process all of the configuration options
    # some (actually, most) options require us to
    # do something, like change the attributes of
    # a widget or two. Here's where we do that...
    foreach option [array names opts] {
    set newValue $opts($option)
    if {[info exists options($option)]} {
        set oldValue $options($option)
    }

    switch -- $option {
        -background {
        $widgets(frame)   configure -background $newValue
        $widgets(entry)   configure -background $newValue
        $widgets(listbox) configure -background $newValue
        # let's keep the scrollbar good-looking
        # $widgets(vsb)     configure -background $newValue
        # $widgets(vsb)     configure -troughcolor $newValue
        set options($option) $newValue
        }

        -borderwidth {
        $widgets(frame) configure -borderwidth $newValue
        set options($option) $newValue
        }

        -command {
        # nothing else to do...
        set options($option) $newValue
        }

        -commandstate {
        # do some value checking...
        if {$newValue != "normal" && $newValue != "disabled"} {
            set options($option) $oldValue
            set message "bad state value \"$newValue\";"
            append message " must be normal or disabled"
            error $message
        }
        set options($option) $newValue
        }

        -cursor {
        $widgets(frame) configure -cursor $newValue
        $widgets(entry) configure -cursor $newValue
        $widgets(listbox) configure -cursor $newValue
        set options($option) $newValue
        }

        -editable {
        if {$newValue} {
            # it's editable...
            $widgets(entry) configure  -state normal  -cursor $defaultEntryCursor
        } else {
            $widgets(entry) configure  -state disabled  -cursor $options(-cursor)
        }
        set options($option) $newValue
        }

        -font {
        $widgets(entry) configure -font $newValue
        $widgets(listbox) configure -font $newValue
        set options($option) $newValue
        }

        -foreground {
        $widgets(entry)   configure -foreground $newValue
        $widgets(button)  configure -foreground $newValue
        $widgets(listbox) configure -foreground $newValue
        set options($option) $newValue
        }

        -height {
        $widgets(listbox) configure -height $newValue
        HandleScrollbar $w
        set options($option) $newValue
        }

        -highlightbackground {
        $widgets(frame) configure -highlightbackground $newValue
        set options($option) $newValue
        }

        -highlightcolor {
        $widgets(frame) configure -highlightcolor $newValue
        set options($option) $newValue
        }

        -highlightthickness {
        $widgets(frame) configure -highlightthickness $newValue
        set options($option) $newValue
        }
        
        -image {
        if {[string length $newValue] > 0} {
            $widgets(button) configure -image $newValue
        } else {
            $widgets(button) configure -image ::combobox2::bimage
        }
        set options($option) $newValue
        }

        -maxheight {
        # ComputeGeometry may dork with the actual height
        # of the listbox, so let's undork it
        $widgets(listbox) configure -height $options(-height)
        HandleScrollbar $w
        set options($option) $newValue
        }

        -relief {
        $widgets(frame) configure -relief $newValue
        set options($option) $newValue
        }

        -selectbackground {
        $widgets(entry) configure -selectbackground $newValue
        $widgets(listbox) configure -selectbackground $newValue
        set options($option) $newValue
        }

        -selectborderwidth {
        $widgets(entry) configure -selectborderwidth $newValue
        $widgets(listbox) configure -selectborderwidth $newValue
        set options($option) $newValue
        }

        -selectforeground {
        $widgets(entry) configure -selectforeground $newValue
        $widgets(listbox) configure -selectforeground $newValue
        set options($option) $newValue
        }

        -state {
        if {$newValue == "normal"} {
            # it's enabled
            set editable [::combobox2::GetBoolean  $options(-editable)]
            if {$editable} {
            $widgets(entry) configure -state normal
            $widgets(entry) configure -takefocus 1
            }
        } elseif {$newValue == "disabled"}  {
            # it's disabled
            $widgets(entry) configure -state disabled
            $widgets(entry) configure -takefocus 0

        } else {
            set options($option) $oldValue
            set message "bad state value \"$newValue\";"
            append message " must be normal or disabled"
            error $message
        }

        set options($option) $newValue
        }

        -takefocus {
        $widgets(entry) configure -takefocus $newValue
        set options($option) $newValue
        }

        -textvariable {
        $widgets(entry) configure -textvariable $newValue
        set options($option) $newValue
        }

        -value {
        ::combobox2::SetValue $widgets(this) $newValue
        set options($option) $newValue
        }

        -width {
        $widgets(frame) configure -width $newValue
        $widgets(listbox) configure -width $newValue
        set options($option) $newValue
        }

        -xscrollcommand {
        $widgets(entry) configure -xscrollcommand $newValue
        set options($option) $newValue
        }

    }
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::DestroyHandler

namespace eval ::combobox2 {
proc DestroyHandler {w} {

    # If the widget actually being destroyed is of class Combobox2,
    # crush the namespace and kill the proc. Get it? Crush. Kill. 
    # Destroy. Heh. Danger Will Robinson! Oh, man! I'm so funny it
    # brings tears to my eyes.
    if {[string compare [winfo class $w] "Combobox2"] == 0} {
    upvar ::combobox2::${w}::widgets  widgets
    upvar ::combobox2::${w}::options  options

    # delete the namespace and the proc which represents
    # our widget
    namespace delete ::combobox2::$w
    rename ::$w {}
    }   
    return
}
}
#############################################################################
## Library Procedure:  ::combobox2::DoInternalWidgetCommand

namespace eval ::combobox2 {
proc DoInternalWidgetCommand {w subwidget command args} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

    set subcommand $command
    set command [concat $widgets($subwidget) $command $args]
    if {[catch $command result]} {
    # replace the subwidget name with the megawidget name
    regsub $widgets($subwidget) $result $widgets(this) result

    # replace specific instances of the subwidget command
    # with out megawidget command
    switch $subwidget,$subcommand {
        listbox,index  {regsub "index"  $result "list index"  result}
        listbox,insert {regsub "insert" $result "list insert" result}
        listbox,delete {regsub "delete" $result "list delete" result}
        listbox,get    {regsub "get"    $result "list get"    result}
        listbox,size   {regsub "size"   $result "list size"   result}
    }
    error $result

    } else {
    return $result
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::Find

namespace eval ::combobox2 {
proc Find {w {exact 0}} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

    ## *sigh* this logic is rather gross and convoluted. Surely
    ## there is a more simple, straight-forward way to implement
    ## all this. As the saying goes, I lack the time to make it
    ## shorter...

    # use what is already in the entry widget as a pattern
    set pattern [$widgets(entry) get]

    if {[string length $pattern] == 0} {
    # clear the current selection
    $widgets(listbox) see 0
    $widgets(listbox) selection clear 0 end
    $widgets(listbox) selection anchor 0
    $widgets(listbox) activate 0
    return
    }

    # we're going to be searching this list...
    set list [$widgets(listbox) get 0 end]

    # if we are doing an exact match, try to find,
    # well, an exact match
    set exactMatch -1
    if {$exact} {
    set exactMatch [lsearch -exact $list $pattern]
    }

    # search for it. We'll try to be clever and not only
    # search for a match for what they typed, but a match for
    # something close to what they typed. We'll keep removing one
    # character at a time from the pattern until we find a match
    # of some sort.
    set index -1
    while {$index == -1 && [string length $pattern]} {
    set index [lsearch -glob $list "$pattern*"]
    if {$index == -1} {
        regsub {.$} $pattern {} pattern
    }
    }

    # this is the item that most closely matches...
    set thisItem [lindex $list $index]

    # did we find a match? If so, do some additional munging...
    if {$index != -1} {

    # we need to find the part of the first item that is 
    # unique WRT the second... I know there's probably a
    # simpler way to do this... 

    set nextIndex [expr {$index + 1}]
    set nextItem [lindex $list $nextIndex]

    # we don't really need to do much if the next
    # item doesn't match our pattern...
    if {[string match $pattern* $nextItem]} {
        # ok, the next item matches our pattern, too
        # now the trick is to find the first character
        # where they *don't* match...
        set marker [string length $pattern]
        while {$marker <= [string length $pattern]} {
        set a [string index $thisItem $marker]
        set b [string index $nextItem $marker]
        if {[string compare $a $b] == 0} {
            append pattern $a
            incr marker
        } else {
            break
        }
        }
    } else {
        set marker [string length $pattern]
    }
    
    } else {
    set marker end
    set index 0
    }

    # ok, we know the pattern and what part is unique;
    # update the entry widget and listbox appropriately
    if {$exact && $exactMatch == -1} {
    # this means we didn't find an exact match
    $widgets(listbox) selection clear 0 end
    $widgets(listbox) see $index

    } elseif {!$exact}  {
    # this means we found something, but it isn't an exact
    # match. If we find something that *is* an exact match we
    # don't need to do the following, since it would merely 
    # be replacing the data in the entry widget with itself
    set oldstate [$widgets(entry) cget -state]
    $widgets(entry) configure -state normal
    $widgets(entry) delete 0 end
    $widgets(entry) insert end $thisItem
    $widgets(entry) selection clear
    $widgets(entry) selection range $marker end
    $widgets(listbox) activate $index
    $widgets(listbox) selection clear 0 end
    $widgets(listbox) selection anchor $index
    $widgets(listbox) selection set $index
    $widgets(listbox) see $index
    $widgets(entry) configure -state $oldstate
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::GetBoolean

namespace eval ::combobox2 {
proc GetBoolean {value {errorValue 1}} {
    if {[catch {expr {([string trim $value])?1:0}} res]} {
    return $errorValue
    } else {
    return $res
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::HandleEvent

namespace eval ::combobox2 {
proc HandleEvent {w event} {
    upvar ::combobox2::${w}::widgets  widgets
    upvar ::combobox2::${w}::options  options
    upvar ::combobox2::${w}::oldValue oldValue

    # for all of these events, if we have a special action we'll
    # do that and do a "return -code break" to keep additional
    # bindings from firing. Otherwise we'll let the event fall
    # on through.
    switch $event {

    "<Any-KeyPress>" {
        # if the widget is editable, clear the selection. 
        # this makes it more obvious what will happen if the 
        # user presses <Return> (and helps our code know what
        # to do if the user presses return)
        if {$options(-editable)} {
        $widgets(listbox) see 0
        $widgets(listbox) selection clear 0 end
        $widgets(listbox) selection anchor 0
        $widgets(listbox) activate 0
        }
    }

    "<FocusIn>" {
        set oldValue [$widgets(entry) get]
    }

    "<FocusOut>" {
        if {![winfo ismapped $widgets(popup)]} {
        # did the value change?
#       set newValue [set ::combobox2::${w}::entryTextVariable]
        set newValue [$widgets(entry) get]
        if {$oldValue != $newValue} {
            CallCommand $widgets(this) $newValue
        }
        }
    }

    "<1>" {
        set editable [::combobox2::GetBoolean $options(-editable)]
        if {!$editable} {
        if {[winfo ismapped $widgets(popup)]} {
            $widgets(this) close
            return -code break;

        } else {
            if {$options(-state) != "disabled"} {
            $widgets(this) open
            return -code break;
            }
        }
        }
    }

    "<Double-1>" {
        if {$options(-state) != "disabled"} {
        $widgets(this) toggle
        return -code break;
        }
    }

    "<Tab>" {
        if {[winfo ismapped $widgets(popup)]} {
        ::combobox2::Find $widgets(this) 0
        return -code break;
        } else {
        ::combobox2::SetValue $widgets(this) [$widgets(this) get]
        }
    }

    "<Escape>" {
#       $widgets(entry) delete 0 end
#       $widgets(entry) insert 0 $oldValue
        if {[winfo ismapped $widgets(popup)]} {
        $widgets(this) close
        return -code break;
        }
    }

    "<Return>" {
        # did the value change?
#       set newValue [set ::combobox2::${w}::entryTextVariable]
        set newValue [$widgets(entry) get]
        if {$oldValue != $newValue} {
        CallCommand $widgets(this) $newValue
        }

        if {[winfo ismapped $widgets(popup)]} {
        ::combobox2::Select $widgets(this)  [$widgets(listbox) curselection]
        return -code break;
        } 

    }

    "<Next>" {
        $widgets(listbox) yview scroll 1 pages
        set index [$widgets(listbox) index @0,0]
        $widgets(listbox) see $index
        $widgets(listbox) activate $index
        $widgets(listbox) selection clear 0 end
        $widgets(listbox) selection anchor $index
        $widgets(listbox) selection set $index

    }

    "<Prior>" {
        $widgets(listbox) yview scroll -1 pages
        set index [$widgets(listbox) index @0,0]
        $widgets(listbox) activate $index
        $widgets(listbox) see $index
        $widgets(listbox) selection clear 0 end
        $widgets(listbox) selection anchor $index
        $widgets(listbox) selection set $index
    }

    "<Down>" {
        if {[winfo ismapped $widgets(popup)]} {
        tkListboxUpDown $widgets(listbox) 1
        return -code break;

        } else {
        if {$options(-state) != "disabled"} {
            $widgets(this) open
            return -code break;
        }
        }
    }
    "<Up>" {
        if {[winfo ismapped $widgets(popup)]} {
        tkListboxUpDown $widgets(listbox) -1
        return -code break;

        } else {
        if {$options(-state) != "disabled"} {
            $widgets(this) open
            return -code break;
        }
        }
    }
    }

    return ""
}
}
#############################################################################
## Library Procedure:  ::combobox2::HandleScrollbar

namespace eval ::combobox2 {
proc HandleScrollbar {w {action unknown}} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

    if {$options(-height) == 0} {
    set hlimit $options(-maxheight)
    } else {
    set hlimit $options(-height)
    }

    switch $action {
    "grow" {
        if {$hlimit > 0 && [$widgets(listbox) size] > $hlimit} {
        pack $widgets(vsb) -side right -fill y -expand n
        }
    }

    "shrink" {
        if {$hlimit > 0 && [$widgets(listbox) size] <= $hlimit} {
        pack forget $widgets(vsb)
        }
    }

    "crop" {
        # this means the window was cropped and we definitely 
        # need a scrollbar no matter what the user wants
        pack $widgets(vsb) -side right -fill y -expand n
    }

    default {
        if {$hlimit > 0 && [$widgets(listbox) size] > $hlimit} {
        pack $widgets(vsb) -side right -fill y -expand n
        } else {
        pack forget $widgets(vsb)
        }
    }
    }

    return ""
}
}
#############################################################################
## Library Procedure:  ::combobox2::HumanizeList

namespace eval ::combobox2 {
proc HumanizeList {list} {

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
## Library Procedure:  ::combobox2::Init

namespace eval ::combobox2 {
proc Init {} {
    variable widgetOptions
    variable widgetCommands
    variable scanCommands
    variable listCommands
    variable defaultEntryCursor

    array set widgetOptions [list  -background          {background          Background}  -bd                  -borderwidth  -bg                  -background  -borderwidth         {borderWidth         BorderWidth}  -command             {command             Command}  -commandstate        {commandState        State}  -cursor              {cursor              Cursor}  -editable            {editable            Editable}  -fg                  -foreground  -font                {font                Font}  -foreground          {foreground          Foreground}  -height              {height              Height}  -highlightbackground {highlightBackground HighlightBackground}  -highlightcolor      {highlightColor      HighlightColor}  -highlightthickness  {highlightThickness  HighlightThickness}  -image               {image               Image}  -maxheight           {maxHeight           Height}  -relief              {relief              Relief}  -selectbackground    {selectBackground    Foreground}  -selectborderwidth   {selectBorderWidth   BorderWidth}  -selectforeground    {selectForeground    Background}  -state               {state               State}  -takefocus           {takeFocus           TakeFocus}  -textvariable        {textVariable        Variable}  -value               {value               Value}  -width               {width               Width}  -xscrollcommand      {xScrollCommand      ScrollCommand}  ]


    set widgetCommands [list  bbox      cget     configure    curselection  delete    get      icursor      index         insert    list     scan         selection     xview     select   toggle       open          close  ]

    set listCommands [list  delete       get       index        insert       size  ]

    set scanCommands [list mark dragto]

    # why check for the Tk package? This lets us be sourced into 
    # an interpreter that doesn't have Tk loaded, such as the slave
    # interpreter used by pkg_mkIndex. In theory it should have no
    # side effects when run
    if {[lsearch -exact [package names] "Tk"] != -1} {

    ##################################################################
    #- this initializes the option database. Kinda gross, but it works
    #- (I think). 
    ##################################################################

    # the image used for the button...
    if {$::tcl_platform(platform) == "windows"} {
        image create bitmap ::combobox2::bimage -data {
        #define down_arrow_width 12
        #define down_arrow_height 12
        static char down_arrow_bits[] = {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0xfc,0xf1,0xf8,0xf0,0x70,0xf0,0x20,0xf0,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00;
        }
        }
    } else {
        image create bitmap ::combobox2::bimage -data  {
        #define down_arrow_width 15
        #define down_arrow_height 15
        static char down_arrow_bits[] = {
            0x00,0x80,0x00,0x80,0x00,0x80,0x00,0x80,
            0x00,0x80,0xf8,0x8f,0xf0,0x87,0xe0,0x83,
            0xc0,0x81,0x80,0x80,0x00,0x80,0x00,0x80,
            0x00,0x80,0x00,0x80,0x00,0x80
        }
        }
    }

    # compute a widget name we can use to create a temporary widget
    set tmpWidget ".__tmp__"
    set count 0
    while {[winfo exists $tmpWidget] == 1} {
        set tmpWidget ".__tmp__$count"
        incr count
    }

    # get the scrollbar width. Because we try to be clever and draw our
    # own button instead of using a tk widget, we need to know what size
    # button to create. This little hack tells us the width of a scroll
    # bar.
    #
    # NB: we need to be sure and pick a window  that doesn't already
    # exist... 
    scrollbar $tmpWidget
    set sb_width [winfo reqwidth $tmpWidget]
    destroy $tmpWidget

    # steal options from the entry widget
    # we want darn near all options, so we'll go ahead and do
    # them all. No harm done in adding the one or two that we
    # don't use.
    entry $tmpWidget 
    foreach foo [$tmpWidget configure] {
        # the cursor option is special, so we'll save it in
        # a special way
        if {[lindex $foo 0] == "-cursor"} {
        set defaultEntryCursor [lindex $foo 4]
        }
        if {[llength $foo] == 5} {
        set option [lindex $foo 1]
        set value [lindex $foo 4]
        option add *Combobox2.$option $value widgetDefault

        # these options also apply to the dropdown listbox
        if {[string compare $option "foreground"] == 0  || [string compare $option "background"] == 0  || [string compare $option "font"] == 0} {
            option add *Combobox2*ComboboxListbox.$option $value  widgetDefault
        }
        }
    }
    destroy $tmpWidget

    # these are unique to us...
    option add *Combobox2.cursor              {}
    option add *Combobox2.commandState        normal widgetDefault
    option add *Combobox2.editable            1      widgetDefault
    option add *Combobox2.maxHeight           10     widgetDefault
    option add *Combobox2.height              0
    }

    # set class bindings
    SetClassBindings
}
}
#############################################################################
## Library Procedure:  ::combobox2::Select

namespace eval ::combobox2 {
proc Select {w index} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options

    catch {
    set data [$widgets(listbox) get [lindex $index 0]]
    ::combobox2::SetValue $widgets(this) $data

    $widgets(listbox) selection clear 0 end
    $widgets(listbox) selection anchor $index
    $widgets(listbox) selection set $index

    $widgets(entry) selection range 0 end
    }

    $widgets(this) close

    return ""
}
}
#############################################################################
## Library Procedure:  ::combobox2::SetBindings

namespace eval ::combobox2 {
proc SetBindings {w} {
    upvar ::combobox2::${w}::widgets  widgets
    upvar ::combobox2::${w}::options  options

    # juggle the bindtags. The basic idea here is to associate the
    # widget name with the entry widget, so if a user does a bind
    # on the combobox2 it will get handled properly since it is
    # the entry widget that has keyboard focus.
    bindtags $widgets(entry)  [concat $widgets(this) [bindtags $widgets(entry)]]

    bindtags $widgets(button)  [concat $widgets(this) [bindtags $widgets(button)]]

    # override the default bindings for tab and shift-tab. The
    # focus procs take a widget as their only parameter and we
    # want to make sure the right window gets used (for shift-
    # tab we want it to appear as if the event was generated
    # on the frame rather than the entry. I

    bind $widgets(entry) <Tab>  "::combobox2::tkTabToWindow \[tk_focusNext $widgets(entry)\]; break"
    bind $widgets(entry) <Shift-Tab>  "::combobox2::tkTabToWindow \[tk_focusPrev $widgets(this)\]; break"
    
    # this makes our "button" (which is actually a label)
    # do the right thing
    bind $widgets(button) <ButtonPress-1> [list $widgets(this) toggle]

    # this lets the autoscan of the listbox work, even if they
    # move the cursor over the entry widget.
    bind $widgets(entry) <B1-Enter> "break"

    bind $widgets(listbox) <ButtonRelease-1>  "::combobox2::Select $widgets(this) \[$widgets(listbox) nearest %y\]; break"

    bind $widgets(vsb) <ButtonPress-1>   {continue}
    bind $widgets(vsb) <ButtonRelease-1> {continue}

    bind $widgets(listbox) <Any-Motion> {
    %W selection clear 0 end
    %W activate @%x,%y
    %W selection anchor @%x,%y
    %W selection set @%x,%y @%x,%y
    # need to do a yview if the cursor goes off the top
    # or bottom of the window... (or do we?)
    }

    # these events need to be passed from the entry
    # widget to the listbox, or need some sort of special
    # handling....
    foreach event [list <Up> <Down> <Tab> <Return> <Escape>  <Next> <Prior> <Double-1> <1> <Any-KeyPress>  <FocusIn> <FocusOut>] {
    bind $widgets(entry) $event  "::combobox2::HandleEvent $widgets(this) $event"
    }

}
}
#############################################################################
## Library Procedure:  ::combobox2::SetClassBindings

namespace eval ::combobox2 {
proc SetClassBindings {} {

    # make sure we clean up after ourselves...
    bind Combobox2 <Destroy> [list ::combobox2::DestroyHandler %W]

    # this will (hopefully) close (and lose the grab on) the
    # listbox if the user clicks anywhere outside of it. Note
    # that on Windows, you can click on some other app and
    # the listbox will still be there, because tcl won't see
    # that button click
    set this {[::combobox2::convert %W -W]}
    bind Combobox2 <Any-ButtonPress>   "$this close"
    bind Combobox2 <Any-ButtonRelease> "$this close"

    # this helps (but doesn't fully solve) focus issues. The general
    # idea is, whenever the frame gets focus it gets passed on to
    # the entry widget
    bind Combobox2 <FocusIn> {::combobox2::tkTabToWindow [::combobox2::convert %W -W].entry}

    # this closes the listbox if we get hidden
    bind Combobox2 <Unmap> {[::combobox2::convert %W -W] close}

    return ""
}
}
#############################################################################
## Library Procedure:  ::combobox2::SetValue

namespace eval ::combobox2 {
proc SetValue {w newValue} {

    upvar ::combobox2::${w}::widgets     widgets
    upvar ::combobox2::${w}::options     options
    upvar ::combobox2::${w}::ignoreTrace ignoreTrace
    upvar ::combobox2::${w}::oldValue    oldValue

    if {[info exists options(-textvariable)]  && [string length $options(-textvariable)] > 0} {
    set variable ::$options(-textvariable)
    set $variable $newValue
    } else {
    set oldstate [$widgets(entry) cget -state]
    $widgets(entry) configure -state normal
    $widgets(entry) delete 0 end
    $widgets(entry) insert 0 $newValue
    $widgets(entry) configure -state $oldstate
    }

    # set our internal textvariable; this will cause any public
    # textvariable (ie: defined by the user) to be updated as
    # well
#    set ::combobox2::${w}::entryTextVariable $newValue

    # redefine our concept of the "old value". Do it before running
    # any associated command so we can be sure it happens even
    # if the command somehow fails.
    set oldValue $newValue


    # call the associated command. The proc will handle whether or 
    # not to actually call it, and with what args
    CallCommand $w $newValue

    return ""
}
}
#############################################################################
## Library Procedure:  ::combobox2::VTrace

namespace eval ::combobox2 {
proc VTrace {w args} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options
    upvar ::combobox2::${w}::ignoreTrace ignoreTrace

    if {[info exists ignoreTrace]} return
    ::combobox2::SetValue $widgets(this) [set ::$options(-textvariable)]

    return ""
}
}
#############################################################################
## Library Procedure:  ::combobox2::WidgetProc

namespace eval ::combobox2 {
proc WidgetProc {w command args} {
    upvar ::combobox2::${w}::widgets widgets
    upvar ::combobox2::${w}::options options
    upvar ::combobox2::${w}::oldFocus oldFocus
    upvar ::combobox2::${w}::oldFocus oldGrab

    set command [::combobox2::Canonize $w command $command]

    # this is just shorthand notation...
    set doWidgetCommand  [list ::combobox2::DoInternalWidgetCommand $widgets(this)]

    if {$command == "list"} {
    # ok, the next argument is a list command; we'll 
    # rip it from args and append it to command to
    # create a unique internal command
    #
    # NB: because of the sloppy way we are doing this,
    # we'll also let the user enter our secret command
    # directly (eg: listinsert, listdelete), but we
    # won't document that fact
    set command "list-[lindex $args 0]"
    set args [lrange $args 1 end]
    }

    set result ""

    # many of these commands are just synonyms for specific
    # commands in one of the subwidgets. We'll get them out
    # of the way first, then do the custom commands.
    switch $command {
    bbox -
    delete -
    get -
    icursor -
    index -
    insert -
    scan -
    selection -
    xview {
        set result [eval $doWidgetCommand entry $command $args]
    }
    list-get    {set result [eval $doWidgetCommand listbox get $args]}
    list-index  {set result [eval $doWidgetCommand listbox index $args]}
    list-size   {set result [eval $doWidgetCommand listbox size $args]}

    select {
        if {[llength $args] == 1} {
        set index [lindex $args 0]
        set result [Select $widgets(this) $index]
        } else {
        error "usage: $w select index"
        }
    }

    subwidget {
        set knownWidgets [list button entry listbox popup vsb]
        if {[llength $args] == 0} {
        return $knownWidgets
        }

        set name [lindex $args 0]
        if {[lsearch $knownWidgets $name] != -1} {
        set result $widgets($name)
        } else {
        error "unknown subwidget $name"
        }
    }

    curselection {
        set result [eval $doWidgetCommand listbox curselection]
    }

    list-insert {
        eval $doWidgetCommand listbox insert $args
        set result [HandleScrollbar $w "grow"]
    }

    list-delete {
        eval $doWidgetCommand listbox delete $args
        set result [HandleScrollbar $w "shrink"]
    }

    toggle {
        # ignore this command if the widget is disabled...
        if {$options(-state) == "disabled"} return

        # pops down the list if it is not, hides it
        # if it is...
        if {[winfo ismapped $widgets(popup)]} {
        set result [$widgets(this) close]
        } else {
        set result [$widgets(this) open]
        }
    }

    open {

        # if this is an editable combobox2, the focus should
        # be set to the entry widget
        if {$options(-editable)} {
        focus $widgets(entry)
        $widgets(entry) select range 0 end
        $widgets(entry) icur end
        }

        # if we are disabled, we won't allow this to happen
        if {$options(-state) == "disabled"} {
        return 0
        }

        # compute the geometry of the window to pop up, and set
        # it, and force the window manager to take notice
        # (even if it is not presently visible).
        #
        # this isn't strictly necessary if the window is already
        # mapped, but we'll go ahead and set the geometry here
        # since its harmless and *may* actually reset the geometry
        # to something better in some weird case.
        set geometry [::combobox2::ComputeGeometry $widgets(this)]
        wm geometry $widgets(popup) $geometry
        update idletasks

        # if we are already open, there's nothing else to do
        if {[winfo ismapped $widgets(popup)]} {
        return 0
        }

        # save the widget that currently has the focus; we'll restore
        # the focus there when we're done
        set oldFocus [focus]

        # ok, tweak the visual appearance of things and 
        # make the list pop up
        $widgets(button) configure -relief sunken
        raise $widgets(popup) [winfo parent $widgets(this)]
        wm deiconify $widgets(popup)

        # force focus to the entry widget so we can handle keypress
        # events for traversal
        focus -force $widgets(entry)

        # select something by default, but only if its an
        # exact match...
        ::combobox2::Find $widgets(this) 1

        # save the current grab state for the display containing
        # this widget. We'll restore it when we close the dropdown
        # list
        set status "none"
        set grab [grab current $widgets(this)]
        if {$grab != ""} {set status [grab status $grab]}
        set oldGrab [list $grab $status]
        unset grab status

        # *gasp* do a global grab!!! Mom always told not to
        # do things like this, but these are desparate times.
        grab -global $widgets(this)

        # fake the listbox into thinking it has focus. This is 
        # necessary to get scanning initialized properly in the
        # listbox.
        event generate $widgets(listbox) <B1-Enter>

        return 1
    }

    close {
        # if we are already closed, don't do anything...
        if {![winfo ismapped $widgets(popup)]} {
        return 0
        }

        # restore the focus and grab, but ignore any errors...
        # we're going to be paranoid and release the grab before
        # trying to set any other grab because we really really
        # really want to make sure the grab is released.
        catch {focus $oldFocus} result
        catch {grab release $widgets(this)}
        catch {
        set status [lindex $oldGrab 1]
        if {$status == "global"} {
            grab -global [lindex $oldGrab 0]
        } elseif {$status == "local"} {
            grab [lindex $oldGrab 0]
        }
        unset status
        }

        # hides the listbox
        $widgets(button) configure -relief raised
        wm withdraw $widgets(popup) 

        # select the data in the entry widget. Not sure
        # why, other than observation seems to suggest that's
        # what windows widgets do.
        set editable [::combobox2::GetBoolean $options(-editable)]
        if {$editable} {
        $widgets(entry) selection range 0 end
        $widgets(button) configure -relief raised
        }


        # magic tcl stuff (see tk.tcl in the distribution 
        # lib directory)
        tkCancelRepeat

        return 1
    }

    cget {
        if {[llength $args] != 1} {
        error "wrong # args: should be $w cget option"
        }
        set opt [::combobox2::Canonize $w option [lindex $args 0]]

        if {$opt == "-value"} {
        set result [$widget(entry) get]
        } else {
        set result $options($opt)
        }
    }

    configure {
        set result [eval ::combobox2::Configure {$w} $args]
    }

    default {
        error "bad option \"$command\""
    }
    }

    return $result
}
}
#############################################################################
## Library Procedure:  ::combobox2::combobox2

namespace eval ::combobox2 {
proc combobox2 {w args} {
    variable widgetOptions
    variable widgetCommands
    variable scanCommands
    variable listCommands

    # perform a one time initialization
    if {![info exists widgetOptions]} {
    __combobox2_Setup
        Init
    }

    # build it...
    eval Build $w $args

    # set some bindings...
    SetBindings $w

    # and we are done!
    return $w
}
}
#############################################################################
## Library Procedure:  ::combobox2::convert

namespace eval ::combobox2 {
proc convert {w args} {
    set result {}
    if {![winfo exists $w]} {
    error "window \"$w\" doesn't exist"
    }

    while {[llength $args] > 0} {
    set option [lindex $args 0]
    set args [lrange $args 1 end]

    switch -exact -- $option {
        -x {
        set value [lindex $args 0]
        set args [lrange $args 1 end]
        set win $w
        while {[winfo class $win] != "Combobox2"} {
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
        while {[winfo class $win] != "Combobox2"} {
            incr value [winfo y $win]
            set win [winfo parent $win]
            if {$win == "."} break
        }
        lappend result $value
        }

        -w -
        -W {
        set win $w
        while {[winfo class $win] != "Combobox2"} {
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
## Library Procedure:  ::combobox2::tkCancelRepeat

namespace eval ::combobox2 {
proc tkCancelRepeat {} {
    global tk_version
    if {$tk_version >= 8.4} {
        ::tk::CancelRepeat
    } else {
        ::tkCancelRepeat
    }
}
}
#############################################################################
## Library Procedure:  ::combobox2::tkTabToWindow

namespace eval ::combobox2 {
proc tkTabToWindow {w} {
    global tk_version
    if {$tk_version >= 8.4} {
        ::tk::TabToWindow $w
    } else {
        ::tkTabToWindow $w
    }
}
}
#############################################################################
## Library Procedure:  ::mclistbox::AdjustColumns

namespace eval ::mclistbox {
proc AdjustColumns {w {height {}}} {
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
proc Build {w args} {
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
proc Canonize {w object opt} {
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
proc CheckColumnID {w id} {
    upvar ::mclistbox::${w}::misc    misc

    set id [::mclistbox::Canonize $w column $id]
    set index [lsearch -exact $misc(columns) $id]
    return $index
}
}
#############################################################################
## Library Procedure:  ::mclistbox::Column-add

namespace eval ::mclistbox {
proc Column-add {w args} {
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
proc Column-configure {w id args} {
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
proc ColumnIsHidden {w id} {
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
proc Configure {w args} {
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
#       set options($option) $newValue
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
proc DestroyHandler {w} {

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
proc FindResizableNeighbor {w id {direction right}} {
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
proc HumanizeList {list} {

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
proc Init {} {
    variable widgetOptions
    variable columnOptions
    variable itemConfigureOptions
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
    array set columnOptions [list  -background         {background           Background}  -bitmap       {bitmap               Bitmap}  -font               {font                 Font}  -foreground         {foreground           Foreground}  -image              {image                Image}  -label         {label                Label}  -position           {position             Position}  -labelrelief        {labelrelief          Labelrelief}  -resizable          {resizable            Resizable}  -visible            {visible              Visible}  -width              {width                Width}  ]

    # and likewise for item-specific stuff. 
    array set itemConfigureOptions [list  -background         {background           Background}  -foreground         {foreground           Foreground}  -selectbackground   {selectbackground     SelectBackground}  -selectforeground   {selectforeground     SelectForeground}  ]
        
    # this defines the valid widget commands. It's important to
    # list them here; we use this list to validate commands and
    # expand abbreviations.
    set widgetCommands [list  activate   bbox       cget     column    configure   curselection delete     get      index     insert    itemconfigure  label        nearest    scan     see       selection   size         xview      yview
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
proc Insert {w index arglist} {

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
proc InvalidateScrollbars {w} {

    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc

    if {![info exists misc(afterid)]} {
    set misc(afterid)  [after idle "catch {::mclistbox::UpdateScrollbars $w}"]
    }
}
}
#############################################################################
## Library Procedure:  ::mclistbox::ItemConfigure

namespace eval ::mclistbox {
proc ItemConfigure {w args} {
    variable itemConfigureOptions

    upvar ::mclistbox::${w}::widgets widgets
    upvar ::mclistbox::${w}::options options
    upvar ::mclistbox::${w}::misc    misc
    
    if {[llength $args] == 0} {
    # hmmm. User must be wanting all configuration information
    # note that if the value of an array element is of length
    # one it is an alias, which needs to be handled slightly
    # differently
    #
    # Broken! This seems to return options from hidden listbox. Not the
    # changed item. 
    #
    set results {}
    foreach opt [lsort [array names itemConfigureOptions]] {
        if {[llength $itemConfigureOptions($opt)] == 1} {
        set alias $itemConfigureOptions($opt)
        set optName $itemConfigureOptions($alias)
        lappend results [list $opt $optName]
        } else {
        set optName  [lindex $itemConfigureOptions($opt) 0]
        set optClass [lindex $itemConfigureOptions($opt) 1]
        set default [option get $w $optName $optClass]
        lappend results [list $opt $optName $optClass  $default $options($opt)]
        }
    }
    return $results
    }
    
    # one argument means we are looking for configuration
    # information on a single option
    #
    # Broken! This seems to return options from hidden listbox. Not the
    # changed item. 
    #
    if {[llength $args] == 2} {
    set opt [::mclistbox::Canonize $w option [lindex $args 1]]

    set optName  [lindex $itemConfigureOptions($opt) 0]
    set optClass [lindex $itemConfigureOptions($opt) 1]
    set default [option get $w $optName $optClass]
    set results [list $opt $optName $optClass  $default $options($opt)]
    return $results
    }

    # The itemconfigure option should have an odd number of options. 
    if {[expr {[llength $args]%2}] == 1} {
    # hmmm. An odd number of elements in args
    
    set itemIndex [lindex $args 0 ]
    set newArgs [ lrange $args 1 end ]
    }
    
    # Great. An odd number of options. Let's make sure they 
    # are all valid before we do anything. Note that Canonize
    # will generate an error if it finds a bogus option; otherwise
    # it returns the canonical option name
    foreach {name value} $newArgs {
    set name [::mclistbox::Canonize $w option $name]
    set opts($name) $value
    }

    # process all of the configuration options
    foreach option [array names opts] {

    set newValue $opts($option)
    if {[info exists options($option)]} {
        set oldValue $options($option)
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

        # { the following all must be applied to each listbox }
        -background -
        -foreground -
        -selectforeground -
        -selectbackground {
        foreach id $misc(columns) {
            $widgets(listbox$id) itemconfigure $itemIndex $option $newValue
        }
        $widgets(hiddenListbox) itemconfigure $itemIndex $option $newValue
        set options($option) [$widgets(hiddenListbox) cget $option]
        }
    }
    }
}
}
#############################################################################
## Library Procedure:  ::mclistbox::LabelEvent

namespace eval ::mclistbox {
proc LabelEvent {w id code} {
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
proc MassageIndex {w index} {
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
proc NewColumn {w id} {
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
proc ResizeEvent {w type widget x X Y} {

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
proc SelectionHandler {w type {offset {}} {length {}}} {
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
proc SetBindings {w} {
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
proc SetClassBindings {} {
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
proc UpdateScrollbars {w} {
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
proc WidgetProc {w command args} {
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
    
    itemconfigure {
        set result [eval ::mclistbox::ItemConfigure {$w} $args]
        
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
proc WidgetProc-get {w args} {
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
proc convert {w args} {
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
proc mclistbox {args} {
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
## Library Procedure:  __combobox2_Setup

proc ::__combobox2_Setup {} {

    namespace eval ::combobox2 {

        # this is the public interface
        namespace export combobox2

        # these contain references to available options
        variable widgetOptions

        # these contain references to available commands and subcommands
        variable widgetCommands
        variable scanCommands
        variable listCommands
    }
}
#############################################################################
## Library Procedure:  __mclistbox_Setup

proc ::__mclistbox_Setup {} {

    namespace eval ::mclistbox {

        # this is the public interface
        namespace export mclistbox

        # these contain references to available options
        variable widgetOptions
        variable columnOptions
    variable itemConfigureOptions

        # these contain references to available commands and subcommands
        variable widgetCommands
        variable columnCommands
        variable labelCommands
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
    set base .top75
    namespace eval ::widgets::$base {
        set set,origin 1
        set set,size 1
        set runvisible 1
    }
    namespace eval ::widgets::$base.fra77 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -relief 1 -width 1}
    }
    set site_3_0 $base.fra77
    namespace eval ::widgets::$site_3_0.but78 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.ent79 {
        array set save {-_tooltip 1 -background 1 -font 1 -foreground 1 -justify 1 -textvariable 1 -validate 1 -vcmd 1}
    }
    namespace eval ::widgets::$site_3_0.but80 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but86 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but87 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but81 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but77 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but82 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but83 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but84 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but85 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.but76 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.cpd68 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.cpd67 {
        array set save {-_tooltip 1 -background 1 -borderwidth 1 -font 1 -foreground 1 -highlightbackground 1 -relief 1 -state 1 -textvariable 1 -validate 1 -vcmd 1 -width 1}
    }
    namespace eval ::widgets::$site_3_0.button77 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$base.tab88 {
        array set save {-angle 1 -bevelamount 1 -raiseselect 1 -tabborders 1 -tabforeground 1 -tabpos 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -width 1}
        }
    }
    set site_8_0 [lindex [$base.tab88 childsite] 0]
    namespace eval ::widgets::$site_8_0 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_0
    namespace eval ::widgets::$site_8_0.lab69 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab69 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.fra70 {
        array set save {-background 1 -borderwidth 1 -height 1 -width 1}
    }
    set site_11_0 $site_10_0.fra70
    namespace eval ::widgets::$site_11_0.cpd72 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.cpd73 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -pady 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.cpd78 {
        array set save {-labelfont 1 -labeltext 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_11_0.cpd75 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.che77 {
        array set save {-background 1 -labelfont 1 -labeltext 1 -relief 1}
        namespace eval subOptions {
            array set save {-anchor 1 -background 1 -highlightbackground 1 -justify 1 -selectcolor 1 -text 1 -textvariable 1 -variable 1}
        }
    }
    namespace eval ::widgets::$site_8_0.lab100 {
        array set save {-ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab100 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.but92 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but93 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but101 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.lab70 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab70 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.cpd71 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_11_0 $site_10_0.cpd71
    namespace eval ::widgets::$site_11_0.but92 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.but93 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.but101 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.cpd72 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_11_0 $site_10_0.cpd72
    namespace eval ::widgets::$site_11_0.but93 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.but101 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.cpd67 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.fra69 {
        array set save {-background 1 -borderwidth 1 -height 1 -width 1}
    }
    set site_9_0 $site_8_0.fra69
    namespace eval ::widgets::$site_9_0.fra76 {
        array set save {-background 1 -borderwidth 1 -height 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.cpd73 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -width 1 -wraplength 1}
    }
    set site_8_1 [lindex [$base.tab88 childsite] 1]
    namespace eval ::widgets::$site_8_1 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_1
    namespace eval ::widgets::$site_8_0.lab105 {
        array set save {-ipadx 1 -ipady 1 -labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab105 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.but74 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -padx 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but73 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -padx 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but71 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -padx 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.lab71 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab71 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.cpd81 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.cpd82 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.cpd83 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -padx 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.cpd84 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.cpd85 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -padx 1 -text 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.lab75 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab75 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.but81 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but83 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -highlightcolor 1 -text 1}
    }
    set site_8_2 [lindex [$base.tab88 childsite] 2]
    namespace eval ::widgets::$site_8_2 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_2
    namespace eval ::widgets::$site_8_0.scr82 {
        array set save {-activebackground 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.fra84 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1}
    }
    set site_9_0 $site_8_0.fra84
    namespace eval ::widgets::$site_9_0.ent85 {
        array set save {-command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.fra90 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_10_0 $site_9_0.fra90
    namespace eval ::widgets::$site_10_0.men91 {
        array set save {-_tooltip 1 -activebackground 1 -background 1 -disabledforeground 1 -font 1 -indicatoron 1 -menu 1 -padx 1 -pady 1 -relief 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.men91.m {
        array set save {-activebackground 1 -activeforeground 1 -disabledforeground 1 -foreground 1 -tearoff 1}
    }
    namespace eval ::widgets::$site_10_0.ent92 {
        array set save {-background 1 -highlightbackground 1 -insertbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.lab95 {
        array set save {-activebackground 1 -background 1 -disabledforeground 1 -font 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.but96 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_9_0.cpd69 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_9_0.but88 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_9_0.but89 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_8_0.fra76 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_9_0 $site_8_0.fra76
    namespace eval ::widgets::$site_9_0.lab81 {
        array set save {-_tooltip 1 -activebackground 1 -anchor 1 -background 1 -font 1 -justify 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.mcl78 {
        array set save {-background 1 -font 1 -height 1 -highlightbackground 1 -labelbackground 1 -labelfont 1 -selectborderwidth 1 -selectcommand 1 -selectmode 1 -width 1 -yscrollcommand 1}
        namespace eval subOptions {
            array set save {-background 1 -font 1 -label 1 -labelrelief 1 -resizable 1 -visible 1 -width 1}
        }
    }
    namespace eval ::widgets::$site_9_0.scr80 {
        array set save {-command 1}
    }
    namespace eval ::widgets::$site_8_0.fra79 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_9_0 $site_8_0.fra79
    namespace eval ::widgets::$site_9_0.ent80 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.ent82 {
        array set save {-justify 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.ent83 {
        array set save {-justify 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.ent84 {
        array set save {-justify 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.but81 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.com77 {
        array set save {-command 1 -justify 1 -labelfont 1 -labelpos 1 -labeltext 1 -selectioncommand 1 -textbackground 1 -textvariable 1 -unique 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.fra69 {
        array set save {-background 1 -borderwidth 1 -height 1 -relief 1 -width 1}
    }
    set site_10_0 $site_9_0.fra69
    namespace eval ::widgets::$site_10_0.cpd70 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -relief 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.cpd71 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    set site_8_3 [lindex [$base.tab88 childsite] 3]
    set site_8_4 [lindex [$base.tab88 childsite] 4]
    namespace eval ::widgets::$site_8_4 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_4
    namespace eval ::widgets::$site_8_0.che119 {
        array set save {-background 1 -labelfont 1 -labelpos 1 -labeltext 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::$site_8_0.che120 {
        array set save {-background 1 -labelfont 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -padx 1 -relief 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::$site_8_0.fra122 {
        array set save {-background 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_9_0 $site_8_0.fra122
    namespace eval ::widgets::$site_9_0.but123 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -relief 1 -state 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_9_0.but124 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_9_0.but125 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -image 1 -padx 1 -pady 1 -relief 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_8_0.scr77 {
        array set save {-activebackground 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.ent78 {
        array set save {-command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.lab81 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_10_0 [$site_8_0.lab81 childsite]
    namespace eval ::widgets::$site_10_0 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_10_0 $site_10_0
    namespace eval ::widgets::$site_10_0.ent82 {
        array set save {-background 1 -highlightbackground 1 -insertbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_10_0.but83 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -padx 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but84 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but85 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    set site_8_5 [lindex [$base.tab88 childsite] 5]
    namespace eval ::widgets::$site_8_5 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_5
    namespace eval ::widgets::$site_8_0.cpd67 {
        array set save {-activebackground 1 -highlightthickness 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -relief 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1}
    }
    namespace eval ::widgets::$site_8_0.fra68 {
        array set save {-background 1 -borderwidth 1 -height 1 -relief 1 -width 1}
    }
    set site_9_0 $site_8_0.fra68
    namespace eval ::widgets::$site_9_0.fra68 {
        array set save {-background 1 -borderwidth 1 -height 1 -relief 1 -width 1}
    }
    set site_10_0 $site_9_0.fra68
    namespace eval ::widgets::$site_10_0.but75 {
        array set save {-_tooltip 1 -activebackground 1 -background 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but76 {
        array set save {-_tooltip 1 -activebackground 1 -background 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.lab65 {
        array set save {-background 1 -font 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent66 {
        array set save {-_tooltip 1 -background 1 -font 1 -highlightbackground 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.men67 {
        array set save {-background 1 -font 1 -indicatoron 1 -menu 1 -padx 1 -pady 1 -relief 1}
    }
    namespace eval ::widgets::$site_10_0.men67.m {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -value 1 -variable 1}
        }
    }
    namespace eval ::widgets::$site_10_0.lab66 {
        array set save {-background 1 -font 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent67 {
        array set save {-_tooltip 1 -background 1 -font 1 -highlightbackground 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_10_0.lab68 {
        array set save {-background 1 -font 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.ent69 {
        array set save {-_tooltip 1 -background 1 -font 1 -highlightbackground 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.cpd70 {
        array set save {-_tooltip 1 -background 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.cpd71 {
        array set save {-_tooltip 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.cpd72 {
        array set save {-_tooltip 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.lab65 {
        array set save {-background 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.ent66 {
        array set save {-_tooltip 1 -background 1 -disabledbackground 1 -font 1 -highlightbackground 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$site_9_0.lab66 {
        array set save {-font 1 -foreground 1 -height 1 -text 1 -width 1}
    }
    set site_10_0 $site_9_0.lab66
    namespace eval ::widgets::$site_10_0.scr67 {
        array set save {-listvariable 1 -textbackground 1}
    }
    namespace eval ::widgets::$site_9_0.but65 {
        array set save {-_tooltip 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.but66 {
        array set save {-_tooltip 1 -command 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.scr66 {
        array set save {-highlightthickness 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -textbackground 1 -textfont 1 -vscrollmode 1}
    }
    set site_8_6 [lindex [$base.tab88 childsite] 6]
    namespace eval ::widgets::$site_8_6 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_6
    namespace eval ::widgets::$site_8_0.scr77 {
        array set save {-activebackground 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.scr78 {
        array set save {-activebackground 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -vscrollmode 1 -width 1}
    }
    namespace eval ::widgets::$site_8_0.fra78 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_9_0 $site_8_0.fra78
    namespace eval ::widgets::$site_9_0.ent79 {
        array set save {-command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.ent80 {
        array set save {-command 1 -labelfont 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.lab81 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_11_0 [$site_9_0.lab81 childsite]
    namespace eval ::widgets::$site_11_0 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_11_0 $site_11_0
    namespace eval ::widgets::$site_11_0.ent82 {
        array set save {-background 1 -highlightbackground 1 -insertbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_11_0.but84 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.but85 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_8_0.fra82 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_9_0 $site_8_0.fra82
    namespace eval ::widgets::$site_9_0.but84 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_9_0.but86 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    set site_8_7 [lindex [$base.tab88 childsite] 7]
    namespace eval ::widgets::$site_8_7 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_7
    namespace eval ::widgets::$site_8_0.fra83 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_9_0 $site_8_0.fra83
    namespace eval ::widgets::$site_9_0.lab84 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_11_0 [$site_9_0.lab84 childsite]
    namespace eval ::widgets::$site_11_0 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_11_0 $site_11_0
    namespace eval ::widgets::$site_11_0.fra86 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_12_0 $site_11_0.fra86
    namespace eval ::widgets::$site_12_0.scr87 {
        array set save {-activebackground 1 -hscrollmode 1 -labelpos 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -visibleitems 1}
    }
    namespace eval ::widgets::$site_12_0.but88 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -state 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.but76 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.fra89 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_12_0 $site_11_0.fra89
    namespace eval ::widgets::$site_12_0.scr87 {
        array set save {-activebackground 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -listvariable 1 -selectioncommand 1 -selectmode 1 -textbackground 1 -textfont 1 -visibleitems 1}
    }
    namespace eval ::widgets::$site_12_0.but88 {
        array set save {-activebackground 1 -background 1 -command 1 -cursor 1 -font 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.ent90 {
        array set save {-_tooltip 1 -background 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_11_0.fra76 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_12_0 $site_11_0.fra76
    namespace eval ::widgets::$site_12_0.but77 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.ent77 {
        array set save {-justify 1 -labelfont 1 -labelpos 1 -labeltext 1 -textbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_12_0.but78 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.but79 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_12_0.but76 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.opt86 {
        array set save {-activeforeground 1 -command 1 -font 1 -foreground 1 -labelfont 1 -labeltext 1}
    }
    namespace eval ::widgets::$site_11_0.che79 {
        array set save {-background 1 -labelfont 1 -labelpos 1 -labeltext 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::$site_11_0.che76 {
        array set save {-background 1 -labelfont 1 -labelpos 1 -labeltext 1 -relief 1}
        namespace eval subOptions {
            array set save {-activebackground 1 -activeforeground 1 -anchor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightcolor 1 -highlightthickness 1 -justify 1 -offvalue 1 -onvalue 1 -selectcolor 1 -text 1 -variable 1}
        }
    }
    namespace eval ::widgets::$site_11_0.lab76 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_13_0 [$site_11_0.lab76 childsite]
    namespace eval ::widgets::$site_13_0 {
        array set save {-background 1 -highlightbackground 1}
    }
    set site_13_0 $site_13_0
    namespace eval ::widgets::$site_13_0.ent77 {
        array set save {-_tooltip 1 -background 1 -highlightbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_13_0.but78 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_11_0.lab79 {
        array set save {-labelfont 1 -labelpos 1 -labeltext 1}
    }
    set site_13_0 [$site_11_0.lab79 childsite]
    namespace eval ::widgets::$site_13_0 {
        array set save {-background 1 -highlightbackground 1}
    }
    set site_13_0 $site_13_0
    namespace eval ::widgets::$site_13_0.ent77 {
        array set save {-_tooltip 1 -background 1 -highlightbackground 1 -textvariable 1 -width 1}
    }
    namespace eval ::widgets::$site_13_0.but78 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    set site_8_8 [lindex [$base.tab88 childsite] 8]
    namespace eval ::widgets::$site_8_8 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_8_0 $site_8_8
    namespace eval ::widgets::$site_8_0.fra84 {
        array set save {-background 1 -highlightbackground 1 -highlightcolor 1}
    }
    set site_9_0 $site_8_0.fra84
    namespace eval ::widgets::$site_9_0.scrolledlistbox83 {
        array set save {-activebackground 1 -cursor 1 -dblclickcommand 1 -hscrollmode 1 -labelfont 1 -labelpos 1 -labeltext 1 -listvariable 1 -textbackground 1 -textfont 1 -width 1}
    }
    namespace eval ::widgets::$site_9_0.fra79 {
        array set save {-background 1 -borderwidth 1 -height 1 -highlightbackground 1 -width 1}
    }
    set site_10_0 $site_9_0.fra79
    namespace eval ::widgets::$site_10_0.but80 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1 -wraplength 1}
    }
    namespace eval ::widgets::$site_10_0.but79 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    namespace eval ::widgets::$site_10_0.but81 {
        array set save {-_tooltip 1 -activebackground 1 -activeforeground 1 -background 1 -command 1 -cursor 1 -disabledforeground 1 -font 1 -foreground 1 -highlightbackground 1 -text 1}
    }
    $base.tab88 select 0
    namespace eval ::widgets::$base.m88 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -cursor 1 -foreground 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu89 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu90 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1}
        }
    }
    set site_4_0 $site_3_0.menu90
    namespace eval ::widgets::$site_4_0.menu77 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    set site_4_0 $site_3_0.menu90
    namespace eval ::widgets::$site_4_0.menu97 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1 -value 1 -variable 1}
        }
    }
    set site_4_0 $site_3_0.menu90
    namespace eval ::widgets::$site_4_0.men87 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -value 1 -variable 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu92 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu93 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_4_0 $site_3_0.menu93
    namespace eval ::widgets::$site_4_0.men87 {
        array set save {-activebackground 1 -activeforeground 1 -foreground 1 -tearoff 1}
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu94 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.men70 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -font 1 -label 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu95 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.men77 {
        array set save {-disabledforeground 1 -tearoff 1}
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.men78 {
        array set save {-disabledforeground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    set site_3_0 $base.m88
    namespace eval ::widgets::$site_3_0.menu96 {
        array set save {-activebackground 1 -activeforeground 1 -background 1 -foreground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1}
        }
    }
    namespace eval ::widgets::$base.m76 {
        array set save {-disabledforeground 1 -tearoff 1}
    }
    namespace eval ::widgets_bindings {
        set tagslist {_vTclBalloon _TopLevel}
    }
    namespace eval ::vTcl::modules::main {
        set procs {
            editFile
            pspdfView
            RepWriter
            refreshsvndirectory
            init
            main
            ArchiveList
            ViewAll
            adjourn
            ddlist
            fileDialog
            readabun
            runlist
            setmulpro
            vTclWindow.
            vTclWindow.top75
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
## Procedure:  editFile

proc ::editFile {filename} {
        ## make empire flexible enough to handle opening files
        ## on various platforms. C.Mattoon, Nov 13 2008
        if {$::editor == "specify editor" || $::editor == ""} {
                set ::editor [tk_getOpenFile  -parent .top75 -title "Select editor"]
        }
        
        if {$::tcl_platform(os)=="Darwin"} {
                if [regexp {\.app} $::editor] {
                        exec open -a $::editor $filename
                } else {
                        # hopefully command is recognized:
                        exec $::editor $filename &
                }
        } elseif {$::tcl_platform(os)=="Linux"} {
                exec $::editor $filename &
        } else {
                # most likely windows, I don't know how to handle this yet
                # need a windows machine to test with...
                exec $::editor $filename &
        }
}
#############################################################################
## Procedure:  pspdfView

proc ::pspdfView {filename} {
        ## make empire flexible enough to handle opening files
        ## on various platforms. C.Mattoon, Nov 13 2008
        if {$::psviewer == ""} {
                set ::psviewer [tk_getOpenFile  -parent .top75 -title "Select ps viewer"]
        }

        if {$::tcl_platform(os)=="Darwin"} {
                if [regexp {\.app} $::psviewer] {
                        exec open -a $::psviewer $filename      
                } else {
                        # hopefully command is recognized:
                        exec $::psviewer $filename &
                }
        } elseif {$::tcl_platform(os)=="Linux"} {
                exec $::psviewer $filename &
        } else {
                # most likely windows, I don't know how to handle this yet
                # need a windows machine to test with
                exec $::psviewer $filename &
        }
}
#############################################################################
## Procedure:  RepWriter

proc ::RepWriter {filename} {
        ## make empire flexible enough to handle opening files
        ## on various platforms. C.Mattoon, Nov 13 2008
        if {$::RepWriter == ""} {
                set ::RepWriter [tk_getOpenFile  -parent .top75 -title "Select Report Editor"]
        }

        if {$::tcl_platform(os)=="Darwin"} {
                if [regexp {\.app} $::RepWriter] {
                        exec open -a $::RepWriter $filename      
                } else {
                        # hopefully command is recognized:
                        exec $::RepWriter $filename &
                }
        } elseif {$::tcl_platform(os)=="Linux"} {
                exec $::RepWriter $filename &
        } else {
                # most likely windows, I don't know how to handle this yet
                # need a windows machine to test with
                exec $::RepWriter $filename &
        }
}
#############################################################################
## Procedure:  refreshsvndirectory

proc ::refreshsvndirectory {} {
  global svnfilelist
  set selsvnfilelist ""
  set svnfilelist [lsort [glob -nocomplain *]]
}
#############################################################################
## Procedure:  main

proc ::main {argc argv} {

}
#############################################################################
## Procedure:  ArchiveList

proc ::ArchiveList {archdir stablist mulstname} {
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
exec xterm -e $::env(EMPIREDIR)/scripts/storemul $archdir/ $mulstname
if {$mulstname == ""} {
    exec mv default.inp $archdir/
    } else {
    exec mv $mulstname.inp $archdir/
}
adjourn .top75
}
#############################################################################
## Procedure:  ViewAll

proc ::ViewAll {} {
   global widget editor stablist mulstname psviewer

   set what [Optionmenu3 get]
   if {$what == "View:"} return

   set suf(View:) " "
   set suf(inputs) .inp
   set suf(warnings) .war
   set suf(PLOTC4-log) -log.plotc4
   set suf(X4TOC4-log) -log.x4toc4
   set suf(EMPEND-log) -log.empend
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
         pspdfView $mulinputn
      } else { editFile $mulinputn
      }
   }
   Optionmenu3 select "View:"
}
#############################################################################
## Procedure:  adjourn

proc ::adjourn {w} {
global widget filelist archdirlist archfilelist zvvplots profilter zvfilter archfilter archdir

# list of zvv plots
set zvvplots [glob -nocomplain *$zvfilter*.zvd]
set zvvplots [lsort -dictionary $zvvplots]

# file list
set filetmp [glob -nocomplain *$profilter*]
set filelist ""
foreach el $filetmp {
  if {[file isfile $el] == 1} {
  lappend filelist $el
 }
}
set filelist [lsort -dictionary $filelist]

# list of archive directories
set archdirlistmp [glob -nocomplain $::env(EMPIREDIR)/*/]
set archdirlist {}
foreach elm $archdirlistmp {
if {$elm != "$::env(EMPIREDIR)/RIPL/" && $elm != "$::env(EMPIREDIR)/data/" &&  $elm != "$::env(EMPIREDIR)/scripts/"  && $elm != "$::env(EMPIREDIR)/CVS/"  && $elm != "$::env(EMPIREDIR)/doc/"  && $elm != "$::env(EMPIREDIR)/EXFOR/"  && $elm != "$::env(EMPIREDIR)/source/"  && $elm != "$::env(EMPIREDIR)/util/"  && $elm != "$::env(EMPIREDIR)/x4cd/" } then {
lappend archdirlist $elm }
}
set archdirlist [lsort -dictionary $archdirlist]

#list of files in the selected archive directory
set archfiletmp [glob -nocomplain $archdir/*$archfilter*]
set archfilelist ""
foreach el $archfiletmp {
  if {[file isfile $el] == 1} {
  lappend archfilelist [file tail $el]
 }
}
set archfilelist [lsort -dictionary $archfilelist]
}
#############################################################################
## Procedure:  ddlist

proc ::ddlist {} {
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

proc ::fileDialog {w} {
global widget
global file zvfilter zvvplots profilter filelist archfilter workdir

    #   Type names          Extension(s)  Mac File Type(s)
    #
    #---------------------------------------------------------
    set types {
       {"Input Files"              {.inp}        }
       {"Output (lst) Files"       {.lst}        }
       {"Output (out) Files"       {.out}        }
       {"EXFOR Files"              {.exf}        }
       {"C4 Files"                 {.c4}         }
       {"ENDF Files"               {.endf}       }
       {"All   Files"              {*}           }
    }

set defile [tk_getOpenFile -filetypes $types  -parent $w -title "Select project directory and input file"]
puts $defile
set workdirt [file dirname $defile]
set pdir [lindex [file split $workdirt] end-1]
if { $pdir != "empire" && $::env(EMPIREDIR) == ".." } {
  if {[tk_dialog .dialogsi Confirm "To run EMPIRE outside the install directory, please set EMPIREDIR variable. Currently you can't run calculations in this directory. OK?" "" 0 No Yes ] == 1} {
    set workdir $workdirt
    cd $workdir
    set dfile [file rootname $defile]
    set file [file tail $dfile]
    set zvfilter $file
    set profilter $file
    set archfilter $file
    Combobox1 clear
    # create list of possible ddx plots
    ddlist
    # update all file lists
    adjourn .top75
  }
} else {
    set workdir $workdirt
    cd $workdir
    set dfile [file rootname $defile]
    set file [file tail $dfile]
    set zvfilter $file
    set profilter $file
    set archfilter $file
    Combobox1 clear
    # create list of possible ddx plots
    ddlist
    # update all file lists
    adjourn .top75
}
}
#############################################################################
## Procedure:  readabun

proc ::readabun {nucfile} {
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

proc ::runlist {stablist mulstname} {
global widget ckmlo ckmsh ckmlog ckmendf ckmplots ckmx4 ckmc4 ckmriplomp  ckmdiromp ckmlev ckmcollev ckminp cempire cformat cverify cprepro cplot
set checkept $ckmlo$ckmsh$ckmendf$ckmplots
if {$checkept == ".lst.out*.endf.ps"} return
foreach el $stablist {
   if {$el == ""} continue
   set elspl [split $el -]
   set Ztarget [lindex $elspl 0]
   set Starget [lindex $elspl 1]
   set Atarget [lindex $elspl 2]
   set mulinputn za[expr $Ztarget*1000+$Atarget]$mulstname
   set inpexists [file exists $mulinputn.inp]
   if { $inpexists == 0 } {
      set skelinp [open $::env(EMPIREDIR)/scripts/skel.inp r]
      set mulinput [open $mulinputn.inp w]
      while {[gets $skelinp line] >=0} {
         if [regexp xxx $line] {
            puts $mulinput [format "%5.1f %5.1f          ;TARGET A, Z" $Atarget $Ztarget]
         } else {puts $mulinput $line
         }
      }
      close $skelinp
      close $mulinput
   }
   set seninpexists [file exists $mulinputn-inp.sen]
   if {$seninpexists == 0 } {file copy $::env(EMPIREDIR)/scripts/skel-inp.sen $mulinputn-inp.sen}
   if {$cempire == 1 && [file exists $mulinputn.inp ]} {exec xterm -e $::env(EMPIREDIR)/scripts/runE $mulinputn}
   if {$cformat == 1 && [file exists $mulinputn.out ]} {exec xterm -e $::env(EMPIREDIR)/scripts/format $mulinputn 1111 }
   if {$cverify == 1 && [file exists $mulinputn.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/verify $mulinputn}
   if {$cprepro == 1 && [file exists $mulinputn.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/process $mulinputn 1111 }
   if {$cplot == 1 && [file exists $mulinputn.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/addresonances $mulinputn}
   #if {$cplot == 1 && [file exists $mulinputn-s.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/plot $mulinputn}

   exec xterm -e $::env(EMPIREDIR)/scripts/run $mulinputn 1111 1
   set delistmul ""
   lappend delistmul $ckmlo $ckmsh $ckmlog $ckmendf  $ckmplots $ckmx4 $ckmc4  $ckmriplomp  $ckmdiromp  $ckmlev  $ckmcollev $ckminp
   foreach el $delistmul {
   if {$el == ""} continue
   eval exec $::env(EMPIREDIR)/scripts/cleansel $mulinputn $el
      if {$el == $ckmlog} {
         exec rm -f $mulinputn.x42c4_errs
         exec rm -f $mulinputn.x42c4_lst
         exec rm -f $mulinputn.war
      }
   }
}
if {$mulstname == ""} {
    exec cp $::env(EMPIREDIR)/scripts/skel.inp default.inp
    exec cp $::env(EMPIREDIR)/scripts/skel-inp.sen default-inp.sen
    } else {
    exec cp $::env(EMPIREDIR)/scripts/skel.inp $mulstname.inp
    exec cp $::env(EMPIREDIR)/scripts/skel-inp.sen $mulstname-inp.sen
    }
}
#############################################################################
## Procedure:  setmulpro

proc ::setmulpro {selmulitem mulstname} {
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
## Initialization Procedure:  init

proc ::init {argc argv} {
global editor modules zvvplots filelist archdirlist nsh eres file profilter zvfilter archfilter workdir psviewer wwwviewer compeval mat EXPDAT
global svnfilelist selsvnfilelist svnlog repository maxwelltemp

if {[file exists $::env(EMPIREDIR)/.Xrunrc] == 1} {
   set rcfl [open $::env(EMPIREDIR)/.Xrunrc r+]
   gets $rcfl file
   gets $rcfl editor
   gets $rcfl workdir
   gets $rcfl psviewer
   gets $rcfl wwwviewer
   gets $rcfl compeval
   gets $rcfl mat
   gets $rcfl EXPDAT
   gets $rcfl RepWriter
close $rcfl
      } else {
#  set rcfl [open $::env(EMPIREDIR)/.Xrunrc a+]
   set workdir [pwd]
   set file ""
   set editor ""
   set psviewer ""
   set wwwviewer ""
   set compeval ""
   set mat 1111
   set EXPDAT 1
   set RepWriter ""

   }

if {[file isdirectory $workdir] == 0} {set workdir [pwd]}
cd $workdir
set profilter $file
set zvfilter $file
set archfilter $file
set nsh 1
set eres 0.02
set maxwelltemp 1.382E+6
if {$mat == ""} {set mat 1111}
if {$editor == ""} {set editor "specify editor"}
if {$profilter == ""} {set profilter *.inp}
set modules [list Makefile dimension.h main.f input.f fusion.f tl.f empire_ctl.f ccfus.f readMSD-orion.f MSD-tristan.f MSC-NVWY.f subecis06m.f fis_io.f fitbarrier.f plot-zvv.f ddhms.f  pcross.f HF-comp.f  HRTW-comp.f bar_mom.f gamma-strgth.f  gamma-strength-analytic.f lev-dens.f read_nubar.f90 ph-lev-dens.f  print.f  auxiliary.f  thora.f pipe.f systematics.f pfns.f dtrans.f optmand.f kailas07emp.f global.h io.h ddhms.cmb]
set zvvplots [glob -nocomplain $zvfilter*.zvd]
set zvvplots [lsort -dictionary $zvvplots]
set filelist [glob -nocomplain $profilter*]
set filelist [lsort -dictionary $filelist]
set archdirlistmp [glob -nocomplain $::env(EMPIREDIR)/*/]
set archdirlist {}
foreach elm $archdirlistmp {
if {$elm != "$::env(EMPIREDIR)/RIPL/" && $elm != "$::env(EMPIREDIR)/data/" &&  $elm != "$::env(EMPIREDIR)/scripts/"  && $elm != "$::env(EMPIREDIR)/CVS/"  && $elm != "$::env(EMPIREDIR)/doc/"  && $elm != "$::env(EMPIREDIR)/EXFOR/"  && $elm != "$::env(EMPIREDIR)/source/"  && $elm != "$::env(EMPIREDIR)/util/"  && $elm != "$::env(EMPIREDIR)/x4cd/" } then {
lappend archdirlist $elm }
}
set archdirlist [lsort -dictionary $archdirlist]
#set repository "https://ndclx4.bnl.gov/svn/$file-covariance/trunk"
set repository "https://ndclx4.bnl.gov/svn/put-your-svn-repository"
set selsvnfilelist ""
set svnfilelist ""
#set svnfilelist [lsort [glob -nocomplain *]]
#set output [exec svn log]
set output ""
set output [split $output "\n"]
foreach elm $output {
  if {[string index $elm 0] != ""} {
    lappend svnlog $elm
  }
}
}

# before calling init, check for EMPIREDIR at global scope
if {[info exists env(EMPIREDIR)]} {
        puts "Empire found in $::env(EMPIREDIR)"
    } else {
        puts "Assuming empire root directory is at '..'"
        set env(EMPIREDIR) ".."
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
    wm geometry $top 1x1+5+27; update
    wm maxsize $top 1440 803
    wm minsize $top 72 15
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

proc vTclWindow.top75 {base} {
    if {$base == ""} {
        set base .top75
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $top -class Toplevel \
        -menu "$top.m88" -background #ffffff -highlightbackground #d9d9d9 \
        -highlightcolor black 
    wm focusmodel $top passive
    wm geometry $top 957x465+155+363; update
    wm maxsize $top 1265 994
    wm minsize $top 72 15
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "EMPIRE-3.1.1 (Rivoli), March 2012, Graphical User Interface (GUI) "
    vTcl:DefineAlias "$top" "Toplevel1" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.fra77 \
        -borderwidth 2 -relief groove -background #d9d9d9 -height 75 \
        -highlightbackground #d9d9d9 -width 125 
    vTcl:DefineAlias "$top.fra77" "Frame2" vTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.fra77
    button $site_3_0.but78 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #d9d9d9 -command {fileDialog .top75
adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #d9d9d9 \
        -image [vTcl:image:get_image [file join / Users herman empire scripts fileopen.gif]] \
        -relief flat -text Project: 
    vTcl:DefineAlias "$site_3_0.but78" "Button1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but78 "$site_3_0.but78 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Select  project (implie working directory)}
    }
    entry $site_3_0.ent79 \
        -background white -font {Helvetica -12 bold} -foreground #0000ff \
        -justify right -textvariable file -validate focus -vcmd {} 
    vTcl:DefineAlias "$site_3_0.ent79" "Entry1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.ent79 "$site_3_0.ent79 Entry $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.ent79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Root name of the project}
    }
    button $site_3_0.but80 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #d9d9d9 -command { editFile $file.inp } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts edit.gif]] \
        -relief flat -text Input 
    vTcl:DefineAlias "$site_3_0.but80" "Button2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but80 "$site_3_0.but80 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but80 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit EMPIRE input file}
    }
    button $site_3_0.but86 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #dcdcdc \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/run $file $mat &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts launch.gif]] \
        -relief flat -text R+F+P 
    vTcl:DefineAlias "$site_3_0.but86" "Button8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but86 "$site_3_0.but86 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but86 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Launch full sequence (calculations + formatting + preprocessing) }
    }
    button $site_3_0.but87 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #dcdcdc \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/runE $file &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts mini-run.gif]] \
        -relief flat -text Run 
    vTcl:DefineAlias "$site_3_0.but87" "Button9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but87 "$site_3_0.but87 Button $top all _vTclBalloon _vTclBalloon"
    bind $site_3_0.but87 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run EMPIRE only}
    }
    button $site_3_0.but81 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { editFile $file.lst } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts editcopy.gif]] \
        -relief flat -text Output 
    vTcl:DefineAlias "$site_3_0.but81" "Button3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but81 "$site_3_0.but81 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View main output from EMPIRE}
    }
    button $site_3_0.but77 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { editFile $file.out } -cursor hand2 \
        -font {Helvetica -12} -foreground darkgreen \
        -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts shortout.gif]] \
        -relief flat -text Output 
    vTcl:DefineAlias "$site_3_0.but77" "Button133" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but77 "$site_3_0.but77 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View short output from EMPIRE}
    }
    button $site_3_0.but82 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { editFile $file.endf } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts kthememgr.gif]] \
        -relief flat -text ENDF 
    vTcl:DefineAlias "$site_3_0.but82" "Button4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but82 "$site_3_0.but82 Button $top all _vTclBalloon _vTclBalloon"
    bind $site_3_0.but82 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View ENDF-6 formatted file}
    }
    button $site_3_0.but83 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { editFile $file.exf } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts x4.gif]] \
        -relief flat -text EXFOR 
    vTcl:DefineAlias "$site_3_0.but83" "Button5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but83 "$site_3_0.but83 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View experimental data (EXFOR)}
    }
    button $site_3_0.but84 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { editFile $file.c4 } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts stop.gif]] \
        -relief flat -text {C4 file} 
    vTcl:DefineAlias "$site_3_0.but84" "Button6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but84 "$site_3_0.but84 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View EXFOR data translated into computational format}
    }
    button $site_3_0.but85 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { pspdfView $file.ps } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts imagegallery.gif]] \
        -relief flat -text Plots 
    vTcl:DefineAlias "$site_3_0.but85" "Button7" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but85 "$site_3_0.but85 Button $top all _vTclBalloon _vTclBalloon"
    bind $site_3_0.but85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View PLOTC4 plots}
    }
    button $site_3_0.but76 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command {adjourn .top75
ddlist} -cursor hand2 \
        -font {Helvetica -12} -foreground darkgreen \
        -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts reload.gif]] \
        -relief flat -text Update 
    vTcl:DefineAlias "$site_3_0.but76" "Button132" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.but76 "$site_3_0.but76 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.but76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Update lists of files (do after running any code) }
    }
    button $site_3_0.cpd68 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc \
        -command {set workdir [ tk_chooseDirectory -initialdir $workdir -title "Choose working directory"  ]
if {[file isdirectory $workdir] == 0} {
   file mkdir $workdir
   file copy $::env(EMPIREDIR)/work/ho $workdir
   }
cd $workdir

adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts fileopen.gif]] \
        -relief flat -text Project: 
    vTcl:DefineAlias "$site_3_0.cpd68" "Button11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.cpd68 "$site_3_0.cpd68 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.cpd68 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Select  working directory}
    }
    entry $site_3_0.cpd67 \
        -background #efefef -borderwidth 0 -font {Helvetica -12 bold} \
        -foreground #333333 -highlightbackground #cccccc -relief flat \
        -state disabled -textvariable workdir -validate none -vcmd {} \
        -width 0 
    vTcl:DefineAlias "$site_3_0.cpd67" "Entry3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_3_0.cpd67 "$site_3_0.cpd67 Entry $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.cpd67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {current working directory}
    }
    button $site_3_0.button77 \
        -activebackground #ff0000 -activeforeground White -background #dcdcdc \
        -command {if {[tk_dialog .dialogsi Confirm "Are you sure you want to delete all files related to the project except input?" "" 0 No Yes ] == 1} {
exec $::env(EMPIREDIR)/scripts/clean $file
adjourn .top75}} \
        -cursor X_cursor -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc \
        -highlightcolor #000000 \
        -image [vTcl:image:get_image [file join / Users herman empire scripts kleandisk.gif]] \
        -relief flat -text Clean 
    bindtags $site_3_0.button77 "$site_3_0.button77 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_3_0.button77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Clean project (delete all project files except input)}
    }
    pack $site_3_0.but78 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.ent79 \
        -in $site_3_0 -anchor center -expand 0 -fill y -side left 
    pack $site_3_0.but80 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 3 -side left 
    pack $site_3_0.but86 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 5 -side left 
    pack $site_3_0.but87 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 3 -side left 
    pack $site_3_0.but81 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 3 -side left 
    pack $site_3_0.but77 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 3 -side left 
    pack $site_3_0.but82 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 3 -side left 
    pack $site_3_0.but83 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 5 -side left 
    pack $site_3_0.but84 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 5 -side left 
    pack $site_3_0.but85 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 5 -side left 
    pack $site_3_0.but76 \
        -in $site_3_0 -anchor center -expand 0 -fill none -ipadx 3 -side left 
    pack $site_3_0.cpd68 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    pack $site_3_0.cpd67 \
        -in $site_3_0 -anchor center -expand 1 -fill both -padx 5 -side left 
    pack $site_3_0.button77 \
        -in $site_3_0 -anchor center -expand 0 -fill none -side left 
    ::iwidgets::tabnotebook $top.tab88 \
        -angle 20 -bevelamount 2 -raiseselect 0 -tabborders 1 \
        -tabforeground #666666 -tabpos n 
    vTcl:DefineAlias "$top.tab88" "Tabnotebook1" vTcl:WidgetProc "Toplevel1" 1
    $top.tab88 add \
        -command {} -label {Main 1} -width 0 
    $top.tab88 add \
        -command {} -label {Main 2} -width 0 
    $top.tab88 add \
        -command {} -label {ZVV plots} -width 0 
    $top.tab88 add \
        -command {} -label {} -width 0 
    $top.tab88 add \
        -command {} -label Files -width 0 
    $top.tab88 add \
        -command {} -label Archive -width 0 
    $top.tab88 add \
        -command {} -label Folders -width 0 
    $top.tab88 add \
        -command {} -label Multi-run -width 0 
    $top.tab88 add \
        -command {} -label Source -width 0 
    set site_8_0 [lindex [$top.tab88 childsite] 0]
    ::iwidgets::labeledframe $site_8_0.lab69 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext Execute 
    vTcl:DefineAlias "$site_8_0.lab69" "Labeledframe15" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_0.lab69 childsite]
    frame $site_10_0.fra70 \
        -borderwidth 2 -background #d9d9d9 -height 75 -width 125 
    vTcl:DefineAlias "$site_10_0.fra70" "Frame6" vTcl:WidgetProc "Toplevel1" 1
    set site_11_0 $site_10_0.fra70
    button $site_11_0.cpd72 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {exec xterm -e cp -i $::env(EMPIREDIR)/scripts/skel.inp $file.inp
adjourn .top75
editFile $file.inp } \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -pady 1m \
        -relief raised -text {Create input} 
    vTcl:DefineAlias "$site_11_0.cpd72" "Button28" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.cpd72 "$site_11_0.cpd72 Button $top all _vTclBalloon"
    bind $site_11_0.cpd72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create input file for the new project}
    }
    button $site_11_0.cpd73 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.inp } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -pady 1m \
        -relief raised -text {Edit input} 
    vTcl:DefineAlias "$site_11_0.cpd73" "Button29" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.cpd73 "$site_11_0.cpd73 Button $top all _vTclBalloon"
    bind $site_11_0.cpd73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit input file}
    }
    ::iwidgets::entryfield $site_11_0.cpd78 \
        -labelfont {Helvetica -12} -background #d9d9d9 -labeltext MAT -textvariable mat -width 4 
    vTcl:DefineAlias "$site_11_0.cpd78" "Entryfield9" vTcl:WidgetProc "Toplevel1" 1
    button $site_11_0.cpd75 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {if {$cempire == 1 && [file exists $file.inp ]} {exec xterm -e $::env(EMPIREDIR)/scripts/runE $file}
if {$cformat == 1 && [file exists $file.out ]} {exec xterm -e $::env(EMPIREDIR)/scripts/format $file $mat }
if {$cverify == 1 && [file exists $file.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/verify $file}
if {$cprepro == 1 && [file exists $file.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/process $file $mat }
if {$cplot == 1 && [file exists $file.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/addresonances $file}
#if {$cplot == 1 && [file exists $file-s.endf]} {exec xterm -e $::env(EMPIREDIR)/scripts/plot $file}

# create list of possible ddx plots
ddlist

adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -relief raised -text {Run selected:} 
    vTcl:DefineAlias "$site_11_0.cpd75" "Button32" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.cpd75 "$site_11_0.cpd75 Button $top all _vTclBalloon"
    bind $site_11_0.cpd75 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Be aware that EMPIRE and Formatting must be run before the remaining ones}
    }
    pack $site_11_0.cpd72 \
        -in $site_11_0 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_11_0.cpd73 \
        -in $site_11_0 -anchor center -expand 0 -fill x -padx 5 -side top 
    pack $site_11_0.cpd78 \
        -in $site_11_0 -anchor center -expand 0 -fill none -pady 3 -side top 
    pack $site_11_0.cpd75 \
        -in $site_11_0 -anchor center -expand 1 -fill both -padx 5 -pady 5 \
        -side top 
    ::iwidgets::checkbox $site_10_0.che77 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* \
        -labeltext {Select for running} -relief flat 
    vTcl:DefineAlias "$site_10_0.che77" "Checkbox5" vTcl:WidgetProc "Toplevel1" 1
    $site_10_0.che77 add chk0 \
        -anchor w -background #d9d9d9 -highlightbackground #d9d9d9 \
        -justify left -selectcolor #00ff00 -text { EMPIRE} -textvariable {} \
        -variable cempire 
    $site_10_0.che77 add chk1 \
        -anchor w -background #d9d9d9 -highlightbackground #d9d9d9 \
        -justify left -selectcolor #00ff00 -text { Formatting (EMPEND)} \
        -textvariable {} -variable cformat 
    $site_10_0.che77 add chk2 \
        -anchor w -background #d9d9d9 -highlightbackground #d9d9d9 \
        -justify left -selectcolor #00ff00 -text { Verification} \
        -textvariable {} -variable cverify 
    $site_10_0.che77 add chk3 \
        -anchor w -background #d9d9d9 -highlightbackground #d9d9d9 \
        -justify left -selectcolor #00ff00 -text { Preparing for plotting}\
        -textvariable {} -variable cprepro 
    $site_10_0.che77 add chk4 \
        -anchor w -background #d9d9d9 -highlightbackground #d9d9d9 \
        -justify left -selectcolor #00ff00 -text { Adding resonances} \
        -textvariable {} -variable cplot 
    pack $site_10_0.fra70 \
        -in $site_10_0 -anchor center -expand 1 -fill both -side left 
    pack $site_10_0.che77 \
        -in $site_10_0 -anchor center -expand 1 -fill none -side right 
    ::iwidgets::labeledframe $site_8_0.lab100 \
        -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos nw -background #d9d9d9 -labeltext Output 
    vTcl:DefineAlias "$site_8_0.lab100" "Labeledframe3" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_0.lab100 childsite]
    button $site_10_0.but92 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.lst } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 1m \
        -relief raised -text Full 
    vTcl:DefineAlias "$site_10_0.but92" "Button19" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but92 "$site_10_0.but92 Button $top all _vTclBalloon"
    bind $site_10_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View full output of EMPIRE}
    }
    button $site_10_0.but93 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.out } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 1m \
        -relief raised -text Short 
    vTcl:DefineAlias "$site_10_0.but93" "Button20" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but93 "$site_10_0.but93 Button $top all _vTclBalloon"
    bind $site_10_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View short output of EMPIRE (used for ENDF-6 formatting)}
    }
    button $site_10_0.but101 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.endf } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 1m \
        -relief raised -text ENDF 
    vTcl:DefineAlias "$site_10_0.but101" "Button21" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but101 "$site_10_0.but101 Button $top all _vTclBalloon"
    bind $site_10_0.but101 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View ENDF-6 file}
    }
    pack $site_10_0.but92 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but93 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but101 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_0.lab70 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext Output/Input 
    vTcl:DefineAlias "$site_8_0.lab70" "Labeledframe1" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_0.lab70 childsite]
    frame $site_10_0.cpd71 \
        -background #d9d9d9 -highlightbackground #d9d9d9 \
        -highlightcolor black 
    bindtags $site_10_0.cpd71 "itk-destroy-.top75.tab88.canvas.notebook.cs.page1.cs.lab102.childsite $site_10_0.cpd71 Frame $top all"
    set site_11_0 $site_10_0.cpd71
    button $site_11_0.but92 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.lev } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 3m \
        -relief raised -text {Discrete levels} 
    vTcl:DefineAlias "$site_11_0.but92" "Button25" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.but92 "$site_11_0.but92 Button $top all _vTclBalloon"
    bind $site_11_0.but92 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit discrete levels for all nuclei involved in the calculation}
    }
    button $site_11_0.but93 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file-lev.col } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 3m \
        -relief raised -text {Collective levels} 
    vTcl:DefineAlias "$site_11_0.but93" "Button42" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.but93 "$site_11_0.but93 Button $top all _vTclBalloon"
    bind $site_11_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit collective levels used for direct calculations with ECIS/OPTMAN}
    }
    button $site_11_0.but101 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { pspdfView $file-cum.ps } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 3m \
        -relief raised -text {Cumul. plot} 
    vTcl:DefineAlias "$site_11_0.but101" "Button49" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.but101 "$site_11_0.but101 Button $top all _vTclBalloon"
    bind $site_11_0.but101 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View cumulative plots of discrete levels along with lev. dens.}
    }
    pack $site_11_0.but92 \
        -in $site_11_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_11_0.but93 \
        -in $site_11_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_11_0.but101 \
        -in $site_11_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    frame $site_10_0.cpd72 \
        -background #d9d9d9 -highlightbackground #d9d9d9 \
        -highlightcolor black 
    bindtags $site_10_0.cpd72 "itk-destroy-.top75.tab88.canvas.notebook.cs.page1.cs.lab103.childsite $site_10_0.cpd72 Frame $top all"
    set site_11_0 $site_10_0.cpd72
    button $site_11_0.but93 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file-omp.ripl } \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 3m \
        -relief raised -text {OM parameters} 
    vTcl:DefineAlias "$site_11_0.but93" "Button50" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.but93 "$site_11_0.but93 Button $top all _vTclBalloon"
    bind $site_11_0.but93 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit optical model parameters taken from RIPL}
    }
    button $site_11_0.but101 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file-omp.dir } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 3m \
        -relief raised -text {OMP for direct} 
    vTcl:DefineAlias "$site_11_0.but101" "Button54" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.but101 "$site_11_0.but101 Button $top all _vTclBalloon"
    bind $site_11_0.but101 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit optical model parameters used for direct calculations with ECIS/OPTMAN}
    }
    button $site_11_0.cpd67 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file-inp.fis } -cursor hand2 \
        -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -image {} -padx 3m \
        -relief raised -text {Fission input} 
    vTcl:DefineAlias "$site_11_0.cpd67" "Button55" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_11_0.cpd67 "$site_11_0.cpd67 Button $top all _vTclBalloon"
    bind $site_11_0.cpd67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit fission input created by EMPIRE in the first run}
    }
    pack $site_11_0.but93 \
        -in $site_11_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_11_0.but101 \
        -in $site_11_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_11_0.cpd67 \
        -in $site_11_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.cpd71 \
        -in $site_10_0 -anchor center -expand 0 -fill none -padx 5 -pady 5 \
        -side left 
    pack $site_10_0.cpd72 \
        -in $site_10_0 -anchor center -expand 0 -fill none -padx 5 -pady 5 \
        -side right 
    frame $site_8_0.fra69 \
        -borderwidth 2 -background #d9d9d9 -height 75 -width 125 
    vTcl:DefineAlias "$site_8_0.fra69" "Frame4" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_0.fra69
    frame $site_9_0.fra76 \
        -borderwidth 2 -background #d9d9d9 -height 74 -width 125 
    vTcl:DefineAlias "$site_9_0.fra76" "Frame7" vTcl:WidgetProc "Toplevel1" 1
    button $site_9_0.cpd73 \
        -activebackground #ff0000 -activeforeground White -background #efefef \
        -command {set rcfl [open $::env(EMPIREDIR)/.Xrunrc w+]
puts $rcfl $file
puts $rcfl $editor
puts $rcfl $workdir
puts $rcfl $psviewer
puts $rcfl $wwwviewer
puts $rcfl $compeval
puts $rcfl $mat
puts $rcfl $EXPDAT
close $rcfl


exit} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -pady 2m -relief raised -text {EXIT / SAVE SETTINGS} -width 14 -wraplength 70 
    vTcl:DefineAlias "$site_9_0.cpd73" "Button58" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd73 "$site_9_0.cpd73 Button $top all _vTclBalloon"
    bind $site_9_0.cpd73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Save settings and close GUI}
    }
    pack $site_9_0.fra76 \
        -in $site_9_0 -anchor center -expand 0 -fill none -side top 
    pack $site_9_0.cpd73 \
        -in $site_9_0 -anchor center -expand 0 -fill none -pady 40 -side top 
    pack $site_8_0.lab69 \
        -in $site_8_0 -anchor center -expand 0 -fill none -side left 
    pack $site_8_0.lab100 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -padx 2 \
        -pady 2 -side left 
    pack $site_8_0.lab70 \
        -in $site_8_0 -anchor center -expand 0 -fill none -ipadx 15 -padx 2 \
        -pady 2 -side left 
    pack $site_8_0.fra69 \
        -in $site_8_0 -anchor center -expand 0 -fill none -side left 
    set site_8_1 [lindex [$top.tab88 childsite] 1]
    ::iwidgets::labeledframe $site_8_1.lab105 \
        -ipadx 5 -ipady 5 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -background #d9d9d9 -labelpos nw -labeltext {OMP fit} 
    vTcl:DefineAlias "$site_8_1.lab105" "Labeledframe7" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_1.lab105 childsite]
    button $site_10_0.but74 \
        -activebackground #d9d9d9 -activeforeground limegreen \
        -background #d9d9d9 -command { editFile $file-omp.ripl } \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -padx 1m \
        -text {Edit RIPL OMP} 
    vTcl:DefineAlias "$site_10_0.but74" "Button155" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but74 "$site_10_0.but74 Button $top all _vTclBalloon"
    bind $site_10_0.but74 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit optical model parameters extracted from RIPL}
    }
    button $site_10_0.but73 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {exec rm -r $file-tl
exec xterm -e $::env(EMPIREDIR)/scripts/run $file
adjourn .top75
# create list of possible ddx plots
ddlist
set ddx $memlist(omp1)
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
puts $lsttab $maxwelltemp
set i 0
foreach el $ddx {
   puts $lsttab $el
   puts $lsttab [expr pow(10,[lindex $ddxsh $i])]
   incr i +1
}
puts $lsttab ""
close $lsttab
#exec gvim LSTTAB.INP
#exec mv LSTTAB.INP $::env(EMPIREDIR)/util/lsttab/LSTTAB.INP
exec xterm -e $::env(EMPIREDIR)/scripts/zvvddx $file omp1 1
exec xterm -e $::env(EMPIREDIR)/scripts/zvv $file-omp1.zvd $file-omp1R.zvd &


set ddx $memlist(omp2)
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
puts $lsttab $maxwelltemp
set i 0
foreach el $ddx {
   puts $lsttab $el
   puts $lsttab [expr pow(10,[lindex $ddxsh $i])]
   incr i +1
}
puts $lsttab ""
close $lsttab
#exec mv LSTTAB.INP $::env(EMPIREDIR)/util/lsttab/LSTTAB.INP
exec xterm -e $::env(EMPIREDIR)/scripts/zvvddx $file omp2 1
exec xterm -e $::env(EMPIREDIR)/scripts/zvv $file-omp2.zvd $file-omp2R.zvd &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -padx 1m \
        -text {Run fit + Plot} 
    vTcl:DefineAlias "$site_10_0.but73" "Button154" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but73 "$site_10_0.but73 Button $top all _vTclBalloon"
    bind $site_10_0.but73 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run manual fit of optical model parameters }
    }
    button $site_10_0.but71 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {exec mv $file-omp1.zvd $file-omp1R.zvd
exec mv $file-omp2.zvd $file-omp2R.zvd
adjourn .top75} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -padx 1m \
        -text {Store as ref.} 
    vTcl:DefineAlias "$site_10_0.but71" "Button156" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but71 "$site_10_0.but71 Button $top all _vTclBalloon"
    bind $site_10_0.but71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store last result as a reference for further plots }
    }
    pack $site_10_0.but74 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but73 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_10_0.but71 \
        -in $site_10_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    ::iwidgets::labeledframe $site_8_1.lab71 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext EXFOR 
    vTcl:DefineAlias "$site_8_1.lab71" "Labeledframe6" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_1.lab71 childsite]
    button $site_10_0.cpd81 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #d9d9d9 \
        -state disabled \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/EXFOR-web &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -pady 1m -relief raised -text {Web EXFOR retrieval} -width 26 
    vTcl:DefineAlias "$site_10_0.cpd81" "Button40" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd81 "$site_10_0.cpd81 Button $top all _vTclBalloon"
    bind $site_10_0.cpd81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Fire up internet browser and go to the EXFOR website}
    }
    button $site_10_0.cpd82 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/c4 $file &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -pady 1m -relief raised -text {Run X4TOC4} -width 12 
    vTcl:DefineAlias "$site_10_0.cpd82" "Button34" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd82 "$site_10_0.cpd82 Button $top all _vTclBalloon"
    bind $site_10_0.cpd82 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Run X4TOC4 code to convert EXFOR data into computational format (C4)}
    }
    button $site_10_0.cpd83 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.exf } -cursor hand2 \
        -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -padx 1m \
        -text {View EXFOR} -width 12 
    vTcl:DefineAlias "$site_10_0.cpd83" "Button171" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd83 "$site_10_0.cpd83 Button $top all _vTclBalloon"
    bind $site_10_0.cpd83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View experimental data extracted from EXFOR}
    }
    button $site_10_0.cpd84 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/sortc4 $file &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -pady 1m -relief raised -text {Sort C4} -width 12 
    vTcl:DefineAlias "$site_10_0.cpd84" "Button37" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd84 "$site_10_0.cpd84 Button $top all _vTclBalloon"
    bind $site_10_0.cpd84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Sort C4 file resulting from the X4TOC4 run}
    }
    button $site_10_0.cpd85 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file.c4 } -cursor hand2 \
        -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc -padx 1m \
        -text {View C4} -width 12 
    vTcl:DefineAlias "$site_10_0.cpd85" "Button173" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd85 "$site_10_0.cpd85 Button $top all _vTclBalloon"
    bind $site_10_0.cpd85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View experimental data in computational format}
    }
    grid $site_10_0.cpd81 \
        -in $site_10_0 -column 0 -row 0 -columnspan 2 -rowspan 1 -pady 5 
    grid $site_10_0.cpd82 \
        -in $site_10_0 -column 0 -row 2 -columnspan 1 -rowspan 1 -pady 5 
    grid $site_10_0.cpd83 \
        -in $site_10_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -pady 5 
    grid $site_10_0.cpd84 \
        -in $site_10_0 -column 1 -row 1 -columnspan 1 -rowspan 1 -pady 5 
    grid $site_10_0.cpd85 \
        -in $site_10_0 -column 1 -row 2 -columnspan 1 -rowspan 1 -pady 5 
    ::iwidgets::labeledframe $site_8_1.lab75 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext ECIS 
    vTcl:DefineAlias "$site_8_1.lab75" "Labeledframe11" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_1.lab75 childsite]
    button $site_10_0.but81 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #d9d9d9 -command { editFile $file-ecis.in } -cursor hand2 \
        -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -highlightcolor #000000 -text {Edit input} 
    vTcl:DefineAlias "$site_10_0.but81" "Button161" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but81 "$site_10_0.but81 Button $top all _vTclBalloon"
    bind $site_10_0.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {ECIS input for the last incident energy in the last EMPIRE run}
    }
    button $site_10_0.but83 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $file-ecis.out } \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -highlightcolor #000000 -text {View output} 
    vTcl:DefineAlias "$site_10_0.but83" "Button163" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but83 "$site_10_0.but83 Button $top all _vTclBalloon"
    bind $site_10_0.but83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {View ECIS output}
    }
    pack $site_10_0.but81 \
        -in $site_10_0 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_10_0.but83 \
        -in $site_10_0 -anchor center -expand 0 -fill x -padx 5 -pady 5 \
        -side top 
    pack $site_8_1.lab105 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 10 -padx 5 \
        -pady 2 -side left 
    pack $site_8_1.lab71 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 5 -padx 5 \
        -pady 2 -side left 
    pack $site_8_1.lab75 \
        -in $site_8_1 -anchor center -expand 0 -fill none -ipadx 10 -ipady 20 \
        -padx 5 -pady 2 -side left 
    set site_8_2 [lindex [$top.tab88 childsite] 2]
    ::iwidgets::scrolledlistbox $site_8_2.scr82 \
        -activebackground #dcdcdc \
        -dblclickcommand {exec xterm -e $::env(EMPIREDIR)/scripts/zvcomb $file [selection get] &} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -background #d9d9d9 -labeltext {Available ZVV plots} -listvariable zvvplots \
        -selectioncommand {set seleczvvlist [selection get]} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12 } -vscrollmode dynamic -width 250 
    frame $site_8_2.fra84 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc 
    set site_9_0 $site_8_2.fra84
    ::iwidgets::entryfield $site_9_0.ent85 \
        -command {set zvvplots [glob -nocomplain *$zvfilter*.zvd]} \
        -labelfont {Helvetica -12 } -background #d9d9d9 -labeltext Filter: -textbackground white \
        -textvariable zvfilter -width 15 
    vTcl:DefineAlias "$site_9_0.ent85" "Entryfield1" vTcl:WidgetProc "Toplevel1" 1
    frame $site_9_0.fra90 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #d9d9d9 -width 125 
    set site_10_0 $site_9_0.fra90
    menubutton $site_10_0.men91 \
        -activebackground #dcdcdc -background #d9d9d9 \
        -disabledforeground #a1a4a1 -font {Helvetica -12} -indicatoron 1 \
        -menu "$site_10_0.men91.m" -padx 5 -pady 2 -relief raised \
        -textvariable mt -width 3 
    vTcl:DefineAlias "$site_10_0.men91" "Menubutton1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.men91 "$site_10_0.men91 Menubutton $top all _vTclBalloon"
    bind $site_10_0.men91 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Click to select predefined MT (reactions)}
    }
    menu $site_10_0.men91.m \
        -activebackground #f7fbf7 -activeforeground black \
        -disabledforeground #a1a4a1 -foreground black -tearoff 0 
    vTcl:DefineAlias "$site_10_0.men91.m" "Menu14" vTcl:WidgetProc "" 1
    $site_10_0.men91.m add radiobutton \
        -value 1 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=1 (total)} -state active 
    $site_10_0.men91.m add radiobutton \
        -value 2 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=2 (elastic)} 
    $site_10_0.men91.m add radiobutton \
        -value 4 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=4 (inelastic)} 
    $site_10_0.men91.m add radiobutton \
        -value 16 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=16 (x,2n)} 
    $site_10_0.men91.m add radiobutton \
        -value 17 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=17 (x,3n)} 
    $site_10_0.men91.m add radiobutton \
        -value 18 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=18 (x,f)} 
    $site_10_0.men91.m add radiobutton \
        -value 22 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=22 (x,na)} 
    $site_10_0.men91.m add radiobutton \
        -value 28 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=28 (x,np)} 
    $site_10_0.men91.m add radiobutton \
        -value 45 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=45 (x,npa)} 
    $site_10_0.men91.m add radiobutton \
        -value 102 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=102 (x,g)} 
    $site_10_0.men91.m add radiobutton \
        -value 103 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=103 (x,p)} 
    $site_10_0.men91.m add radiobutton \
        -value 107 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=107 (x,a)} 
    $site_10_0.men91.m add radiobutton \
        -value 112 -variable mt -command {# TODO: Your menu handler here} \
        -font [vTcl:font:getFontFromDescr "-family times -size 12"] \
        -label {MT=112 (n,pa)} 
    entry $site_10_0.ent92 \
        -background #ffffff -highlightbackground #d9d9d9 \
        -insertbackground black -textvariable mt -width 5 
    vTcl:DefineAlias "$site_10_0.ent92" "Entry2" vTcl:WidgetProc "Toplevel1" 1
    label $site_10_0.lab95 \
        -activebackground #d9d9d9 -background #d9d9d9 \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } -text {Select MT:} 
    vTcl:DefineAlias "$site_10_0.lab95" "Label7" vTcl:WidgetProc "Toplevel1" 1
    pack $site_10_0.men91 \
        -in $site_10_0 -anchor center -expand 0 -fill none -side right 
    pack $site_10_0.ent92 \
        -in $site_10_0 -anchor center -expand 0 -fill none -side right 
    pack $site_10_0.lab95 \
        -in $site_10_0 -anchor center -expand 0 -fill none -side left 
    button $site_9_0.but96 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #e6e6e6 \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/zvd $file $mt &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -text {Plot  selected MT} -wraplength 120 
    vTcl:DefineAlias "$site_9_0.but96" "Button122" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but96 "$site_9_0.but96 Button $top all _vTclBalloon"
    bind $site_9_0.but96 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create, view and store ZVV plot for the selected MT number}
    }
    button $site_9_0.cpd69 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {if {[tk_dialog .dialogsi Confirm "Confirm deleting all selected files" "" 0 No Yes ] == 1} {
foreach el $seleczvvlist {
   if {$el == ""} continue
   exec rm -f $el
}
adjourn .top75 }} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc \
        -text {<= Delete selected} -wraplength 120 
    vTcl:DefineAlias "$site_9_0.cpd69" "Button123" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd69 "$site_9_0.cpd69 Button $top all _vTclBalloon"
    bind $site_9_0.cpd69 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete ZVView plots selected in the window to the left}
    }
    button $site_9_0.but88 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {exec $::env(EMPIREDIR)/scripts/guizvv.tcl $file  &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -text {Launch ZVV interface } -wraplength 72 
    vTcl:DefineAlias "$site_9_0.but88" "Button129" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but88 "$site_9_0.but88 Button $top all _vTclBalloon"
    bind $site_9_0.but88 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create plots comparing up to 3 evaluations (can also be used for single plots)}
    }
    button $site_9_0.but89 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/zvpl $file &
adjourn .top75} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -text {ZVV plot from EMPIRE} -wraplength 90
    vTcl:DefineAlias "$site_9_0.but89" "Button130" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but89 "$site_9_0.but89 Button $top all _vTclBalloon"
    bind $site_9_0.but89 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Any excitation function can be plotted; no need for the ENDF file}
    }
    pack $site_9_0.ent85 \
        -in $site_9_0 -anchor nw -expand 0 -fill x -pady 20 -side top 
    pack $site_9_0.fra90 \
        -in $site_9_0 -anchor center -expand 0 -fill x -side top 
    pack $site_9_0.but96 \
        -in $site_9_0 -anchor center -expand 0 -fill x -side top 
    pack $site_9_0.cpd69 \
        -in $site_9_0 -anchor center -expand 0 -fill x -pady 3 -side top 
    pack $site_9_0.but88 \
        -in $site_9_0 -anchor ne -expand 0 -fill none -side right 
    pack $site_9_0.but89 \
        -in $site_9_0 -anchor nw -expand 0 -fill none -side left 
    frame $site_8_2.fra76 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 227 
    vTcl:DefineAlias "$site_8_2.fra76" "Frame13" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_2.fra76
    label $site_9_0.lab81 \
        -activebackground #d9d9d9 -anchor w -background #d9d9d9 \
        -font {Helvetica -12 } -justify left \
        -text {Select data for ZVV plotting (multiple allowed)} 
    vTcl:DefineAlias "$site_9_0.lab81" "Label8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.lab81 "$site_9_0.lab81 Label $top all _vTclBalloon"
    bind $site_9_0.lab81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Click to create a list of spectra to be plotted }
    }
    ::mclistbox::mclistbox $site_9_0.mcl78 \
        -background #ffffff -font {Helvetica -10} -height 0 \
        -highlightbackground #dcdcdc -labelbackground #dcdcdc \
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
        -selectmode extended -width 44 -yscrollcommand {Scrollbar1 set} 
    vTcl:DefineAlias "$site_9_0.mcl78" "Mclistbox1" vTcl:WidgetProc "Toplevel1" 1
    $site_9_0.mcl78 column add col1 \
        -background #ffffff -font {Helvetica -10} -label # -labelrelief flat \
        -resizable 1 -visible 1 -width 5 
    $site_9_0.mcl78 column add col2 \
        -background #f999f999f999 -font {Helvetica -10 } -label MF \
        -labelrelief flat -resizable 1 -visible 1 -width 5 
    $site_9_0.mcl78 column add col3 \
        -background #ffffff -font {Helvetica -10} -label p -labelrelief flat \
        -resizable 1 -visible 1 -width 3 
    $site_9_0.mcl78 column add col4 \
        -background #f999f999f999 -font {Helvetica -10} -label MT \
        -labelrelief flat -resizable 1 -visible 1 -width 6 
    $site_9_0.mcl78 column add col5 \
        -background #ffffff -font {Helvetica -10} -label Einc \
        -labelrelief flat -resizable 1 -visible 1 -width 10 
    $site_9_0.mcl78 column add col6 \
        -background #f999f999f999 -font {Helvetica -10 } -label Elev \
        -labelrelief flat -resizable 1 -visible 1 -width 10 
    $site_9_0.mcl78 column add col7 \
        -background #ffffff -font {Helvetica -10} -label Ang \
        -labelrelief flat -resizable 1 -visible 1 -width 5 
    $site_9_0.mcl78 column add col8 \
        -background #ffffff -font {Helvetica -10} -label init \
        -labelrelief flat -resizable 0 -visible 0 -width 3 
    scrollbar $site_9_0.scr80 \
        -command {Mclistbox1 yview} 
    vTcl:DefineAlias "$site_9_0.scr80" "Scrollbar1" vTcl:WidgetProc "Toplevel1" 1
    pack $site_9_0.lab81 \
        -in $site_9_0 -anchor n -expand 0 -fill x -side top 
    pack $site_9_0.mcl78 \
        -in $site_9_0 -anchor w -expand 0 -fill y -side left 
    pack $site_9_0.scr80 \
        -in $site_9_0 -anchor center -expand 1 -fill y -side right 
    frame $site_8_2.fra79 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    vTcl:DefineAlias "$site_8_2.fra79" "Frame14" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_2.fra79
    ::iwidgets::entryfield $site_9_0.ent80 \
        -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {List of selected } -textbackground #ffffff \
        -background #d9d9d9 -textvariable ddx -width 0 
    vTcl:DefineAlias "$site_9_0.ent80" "Entryfield3" vTcl:WidgetProc "Toplevel1" 1
    ::iwidgets::entryfield $site_9_0.ent82 \
        -justify right -labelfont {Helvetica -12 } -labeltext {Shift 10**} \
        -background #d9d9d9 -textbackground #ffffff -textvariable nsh 
    vTcl:DefineAlias "$site_9_0.ent82" "Entryfield4" vTcl:WidgetProc "Toplevel1" 1
    ::iwidgets::entryfield $site_9_0.ent83 \
        -justify right -labelfont {Helvetica -12 } -labeltext {Eres (rel)} \
        -background #d9d9d9 -textbackground #ffffff -textvariable eres 
    vTcl:DefineAlias "$site_9_0.ent83" "Entryfield5" vTcl:WidgetProc "Toplevel1" 1
    ::iwidgets::entryfield $site_9_0.ent84 \
        -justify right -labelfont {Helvetica -12 } -labeltext {Maxwellian Temp.} \
        -background #d9d9d9 -textbackground #ffffff -textvariable maxwelltemp 
    button $site_9_0.but81 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #e6e6e6 \
        -command {set memlist($multi) $ddx
Combobox1 insert list end $multi
set ddxsh ""
set i 0
foreach el $ddx {
   lappend ddxsh [expr $i*$nsh]
   incr i +1
}

#if {$compeval != ""} {file delete $::env(EMPIREDIR)/util/lsttab/COMPARE.DAT
#if {$compeval != ""} {file copy -force $compeval $::env(EMPIREDIR)/util/lsttab/COMPARE.DAT }
#file link -symbolic $::env(EMPIREDIR)/util/lsttab/COMPARE.DAT $compeval }

if {$compeval != ""} {file delete COMPARE.DAT
if {$compeval != ""} {file copy -force $compeval COMPARE.DAT }
#file link -symbolic $::env(EMPIREDIR)/util/lsttab/COMPARE.DAT $compeval }

set lsttab [open LSTTAB.INP w+]
puts $lsttab ""
puts $lsttab ""
puts $lsttab "ENDF.DAT"
if {$compeval != ""} {puts $lsttab "COMPARE.DAT"}
puts $lsttab "-"
puts $lsttab "Empire"
if {$compeval != ""} {puts $lsttab [file tail $compeval]}
puts $lsttab $eres
puts $lsttab $maxwelltemp
set i 0
foreach el $ddx {
   puts $lsttab $el
   puts $lsttab [expr pow(10,[lindex $ddxsh $i])]
   incr i +1
}
puts $lsttab ""
close $lsttab
#exec mv LSTTAB.INP $::env(EMPIREDIR)/util/lsttab/LSTTAB.INP
exec xterm -e $::env(EMPIREDIR)/scripts/zvvddx $file $multi &} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -text {Plot the list } 
    vTcl:DefineAlias "$site_9_0.but81" "Button131" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but81 "$site_9_0.but81 Button $top all _vTclBalloon"
    bind $site_9_0.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Create ZVV plot for the items selected in the window to the left }
    }
    ::iwidgets::combobox $site_9_0.com77 \
        \
        -command {namespace inscope ::iwidgets::Combobox {::.top75.tab88.canvas.notebook.cs.page3.cs.fra79.com77 _addToList}} \
        -justify right -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {List name} -selectioncommand {set ddx $memlist($multi)} \
        -background #d9d9d9 -textbackground #ffffff -textvariable multi -unique 1 -width 37 
    vTcl:DefineAlias "$site_9_0.com77" "Combobox1" vTcl:WidgetProc "Toplevel1" 1
    frame $site_9_0.fra69 \
        -borderwidth 2 -relief groove -background #d9d9d9 -height 85 \
        -width 131 
    vTcl:DefineAlias "$site_9_0.fra69" "Frame1" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 $site_9_0.fra69
    button $site_10_0.cpd70 \
        -activebackground #d9d9d9 -activeforeground limegreen \
        -background #efefef \
        -command {#   Type names          Extension(s)  Mac File Type(s)
    #
    #---------------------------------------------------------
    set types {
       {"ENDF Files"              {.endf}              }
       {"All   Files"              {*}           }
    }

set compeval [tk_getOpenFile -filetypes $types  -parent .top75 -title "Select ENDF file to be compared"]} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -image [vTcl:image:get_image [file join / Users herman empire scripts fileopen.gif]] \
        -relief flat -text Project: 
    vTcl:DefineAlias "$site_10_0.cpd70" "Button10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.cpd70 "$site_10_0.cpd70 Button $top all _vTclBalloon _vTclBalloon _vTclBalloon"
    bind $site_10_0.cpd70 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Select  ENDF file to be plotted along with the current project}
    }
    ::iwidgets::entryfield $site_10_0.cpd71 \
        -labelfont {Helvetica -12 } -labelpos nw -labeltext {Compare to:} \
        -background #d9d9d9 -textbackground #ffffff -textvariable compeval -width 0 
    vTcl:DefineAlias "$site_10_0.cpd71" "Entryfield7" vTcl:WidgetProc "Toplevel1" 1
    pack $site_10_0.cpd70 \
        -in $site_10_0 -anchor center -expand 0 -fill none -side left 
    pack $site_10_0.cpd71 \
        -in $site_10_0 -anchor center -expand 1 -fill x -padx 5 -side right 
    pack $site_9_0.ent80 \
        -in $site_9_0 -anchor nw -expand 0 -fill x -side top 
    pack $site_9_0.ent82 \
        -in $site_9_0 -anchor center -expand 0 -fill none -side top 
    pack $site_9_0.ent83 \
        -in $site_9_0 -anchor center -expand 0 -fill none -side top 
    pack $site_9_0.ent84 \
        -in $site_9_0 -anchor center -expand 0 -fill none -side top 
    pack $site_9_0.but81 \
        -in $site_9_0 -anchor center -expand 0 -fill x -side top 
    pack $site_9_0.com77 \
        -in $site_9_0 -anchor center -expand 0 -fill x -side top 
    pack $site_9_0.fra69 \
        -in $site_9_0 -anchor center -expand 1 -fill x -pady 5 -side left 
    pack $site_8_2.scr82 \
        -in $site_8_2 -anchor nw -expand 0 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_2.fra84 \
        -in $site_8_2 -anchor nw -expand 0 -fill y -padx 5 -pady 10 \
        -side left 
    pack $site_8_2.fra76 \
        -in $site_8_2 -anchor center -expand 0 -fill both -side left 
    pack $site_8_2.fra79 \
        -in $site_8_2 -anchor w -expand 1 -fill both -padx 5 -side left 
    set site_8_3 [lindex [$top.tab88 childsite] 3]
    ::mclistbox::mclistbox $site_8_3.cpd70 \
        -background #ffffff -font {Helvetica -10} -height 0 \
        -highlightbackground #dcdcdc -labelbackground #dcdcdc \
        -labelfont {Helvetica -10} -selectborderwidth 0 \
        -selectcommand {set dd ""
}
    set site_8_4 [lindex [$top.tab88 childsite] 4]
    ::iwidgets::checkbox $site_8_4.che119 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos n -labeltext {Select file types:} -relief flat 
    vTcl:DefineAlias "$site_8_4.che119" "Checkbox1" vTcl:WidgetProc "Toplevel1" 1
    $site_8_4.che119 add chk0 \
        -activebackground #f7fbf7 -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .lst -selectcolor green \
        -text { full output} -variable cklo 
    $site_8_4.che119 add chk1 \
        -activebackground #f7fbf7 -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .out -selectcolor green \
        -text { short output} -variable cksh 
    $site_8_4.che119 add chk2 \
        -activebackground #f7fbf7 -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue -log* -selectcolor green \
        -text { log files} -variable cklog 
    $site_8_4.che119 add chk3 \
        -activebackground #f7fbf7 -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue *.endf -selectcolor green \
        -text { ENDF} -variable ckendf 
    $site_8_4.che119 add chk4 \
        -activebackground #f7fbf7 -activeforeground #009900 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .ps -selectcolor green \
        -text { PLOTC4 plots} -variable ckplots 
    $site_8_4.che119 add chk5 \
        -activebackground #f7fbf7 -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .exf -selectcolor orange \
        -text { EXFOR} -variable ckx4 
    $site_8_4.che119 add chk6 \
        -activebackground #f7fbf7 -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .c4 -selectcolor orange \
        -text { C4 file} -variable ckc4 
    $site_8_4.che119 add chk7 \
        -activebackground #f9f9f9 -activeforeground #009900 -anchor w \
        -disabledforeground #a3a3a3 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor black -highlightthickness 0 \
        -justify left -offvalue n -onvalue 000001_ -selectcolor green \
        -text { neutron Tls} -variable ctln 
    $site_8_4.che119 add chk8 \
        -activebackground #f9f9f9 -activeforeground #009900 -anchor w \
        -disabledforeground #a3a3a3 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor black -highlightthickness 0 \
        -justify left -offvalue n -onvalue 001001_ -selectcolor green \
        -text { proton Tls} -variable ctlp 
    ::iwidgets::checkbox $site_8_4.che120 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -relief flat 
    vTcl:DefineAlias "$site_8_4.che120" "Checkbox2" vTcl:WidgetProc "Toplevel1" 1
    $site_8_4.che120 add chk0 \
        -activebackground #f7fbf7 -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue -omp.ripl -padx 1 -relief flat \
        -selectcolor orange -text { OM parameters} -variable ckriplomp 
    $site_8_4.che120 add chk1 \
        -activebackground #f7fbf7 -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue -omp.dir -padx 1 -relief flat \
        -selectcolor orange -text { OMP for direct} -variable ckdiromp 
    $site_8_4.che120 add chk2 \
        -activebackground #f7fbf7 -activeforeground #dfff8ac119c2 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue *.zvd -padx 1 -relief flat \
        -selectcolor orange -text { ZVV plots} -variable ckzvv 
    $site_8_4.che120 add chk3 \
        -activebackground #f7fbf7 -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .lev -padx 1 -relief flat \
        -selectcolor red -text { levels} -variable cklev 
    $site_8_4.che120 add chk4 \
        -activebackground #f7fbf7 -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue -lev.col -padx 1 -relief flat \
        -selectcolor red -text { collective levels} -variable ckcollev 
    $site_8_4.che120 add chk5 \
        -activebackground #f7fbf7 -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue { } -onvalue .inp -padx 1 -relief flat \
        -selectcolor red -text { EMPIRE input} -variable ckinp 
    $site_8_4.che120 add chk6 \
        -activebackground #f6f7f6 -activeforeground #ff0000 -anchor w \
        -disabledforeground #a1a1a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor black -highlightthickness 0 \
        -justify left -offvalue { } -onvalue -inp.fis -padx 1 -relief flat \
        -selectcolor red -text { fission input} -variable ckfisinp 
    $site_8_4.che120 add chk7 \
        -activebackground #f9f9f9 -activeforeground #009900 -anchor w \
        -disabledforeground #a3a3a3 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor black -highlightthickness 0 \
        -justify left -offvalue n -onvalue 002004_ -padx 1 -relief flat \
        -selectcolor green -text { alpha Tls} -variable ctla 
    $site_8_4.che120 add chk8 \
        -activebackground #f9f9f9 -activeforeground #009900 -anchor w \
        -disabledforeground #a3a3a3 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor black -highlightthickness 0 \
        -justify left -offvalue 0 -onvalue 1 -padx 1 -relief flat \
        -selectcolor green -text { Tl directory} -variable ctldir 
    frame $site_8_4.fra122 \
        -background #d9d9d9 -height 75 -highlightbackground #dcdcdc \
        -width 125 
    vTcl:DefineAlias "$site_8_4.fra122" "Frame5" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_4.fra122
    button $site_9_0.but123 \
        -activebackground #eccceccceccc -activeforeground Red \
        -background #d9d9d9 \
        -command if\ \{\[tk_dialog\ .dialogsi\ Confirm\ \"Are\ you\ sure\ you\ want\ to\ delete\ all\ selected\ files?\"\ \"\"\ 0\ No\ Yes\ \]\ ==\ 1\}\ \{\nset\ delist\ \"\"\nlappend\ delist\ \$cklo\ \$cksh\ \$cklog\ \$ckendf\ \ \$ckplots\ \$ckx4\ \$ckc4\ \\\n\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \$ckriplomp\ \ \$ckdiromp\ \ \$ckzvv\ \ \$cklev\ \$ckcollev\ \ \\\n\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \$ckinp\ \$ckfisinp\ \n\nforeach\ el\ \$delist\ \{\n\ \ \ if\ \{\$el\ ==\ \"\"\}\ continue\n\ \ \ eval\ exec\ $::env(EMPIREDIR)/scripts/cleansel\ \$file\ \$el\n\ \ \ if\ \{\$el\ ==\ \$cklog\}\ \{\n\ \ \ \ \ \ exec\ rm\ -f\ \$file.x42c4_errs\n\ \ \ \ \ \ exec\ rm\ -f\ \$file.x42c4_lst\n\ \ \ \ \ \ exec\ rm\ -f\ \$file.war\n\ \ \ \ \ \ \}\n\}\n\nset\ work\ \[pwd\]\nset\ detlist\ \"\"\nlappend\ detlist\ \$ctln\ \$ctlp\ \$ctla\n\nforeach\ el\ \$detlist\ \{\n\ \ \ if\ \{\$el\ ==\ \"n\"\ \}\ continue\n\ \ \ set\ detl\ \[glob\ -path\ \$work/\$file-tl/\ \$el*\]\n\ \ \ set\ detll\ \[split\ \$detl\ \"\ \"\]\n\ \ \ foreach\ edel\ \$detll\ \{\n\ \ \ file\ delete\ \$edel\n\ \ \ \}\n\}\nif\ \{\$ctldir\ ==\ 1\}\ \{\n\ \ \ file\ delete\ -force\ \$work/\$file-tl\n\}\n\nadjourn\ .top75\n\} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 0 \
        -relief raised -state normal -text {Delete selected files} \
        -wraplength 80 
    vTcl:DefineAlias "$site_9_0.but123" "Button51" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but123 "$site_9_0.but123 Button $top all _vTclBalloon"
    bind $site_9_0.but123 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete all project related files of the types selected with check buttons}
    }
    button $site_9_0.but124 \
        -activebackground #eccceccceccc -activeforeground Red \
        -background #efefef \
        -command {if {[tk_dialog .dialogsi Confirm "Are you sure you want to clean the project?" "" 0 No Yes ] == 1} {
exec $::env(EMPIREDIR)/scripts/clean $file
adjourn .top75
}} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -pady 2m -relief raised -text {Clean project} -wraplength 80 
    vTcl:DefineAlias "$site_9_0.but124" "Button52" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but124 "$site_9_0.but124 Button $top all _vTclBalloon"
    bind $site_9_0.but124 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Clean project (delete all project files except input)}
    }
    button $site_9_0.but125 \
        -activebackground #ff0000 -activeforeground White -background #efefef \
        -command {if {[tk_dialog .dialogsi Confirm "Are you sure you want to delete the project?" "" 0 No Yes ] == 1} {
exec $::env(EMPIREDIR)/scripts/clean $file
exec rm -f $file.inp
adjourn .top75 }} \
        -cursor hand2 -disabledforeground #a3a3a3 -font {Helvetica -12} \
        -foreground darkred -highlightbackground #dcdcdc -image {} -padx 1m \
        -pady 2m -relief raised -text {Remove project} -wraplength 80 
    vTcl:DefineAlias "$site_9_0.but125" "Button53" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but125 "$site_9_0.but125 Button $top all _vTclBalloon"
    bind $site_9_0.but125 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete all project related files (including input)}
    }
    pack $site_9_0.but123 \
        -in $site_9_0 -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $site_9_0.but124 \
        -in $site_9_0 -anchor center -expand 0 -fill x -pady 4 -side top 
    pack $site_9_0.but125 \
        -in $site_9_0 -anchor center -expand 0 -fill x -pady 4 -side top 
    ::iwidgets::scrolledlistbox $site_8_4.scr77 \
        -activebackground #dcdcdc \
        -dblclickcommand {set selecfilelist [selection get]
set selecfile [lindex $selecfilelist 0]
set exten [file extension $selecfile]
if {$exten == ".ps"} {
  pspdfView $selecfile
} elseif {$exten == ".eps"} {
  pspdfView $selecfile
} elseif {$exten == ".zvd"} {
  exec xterm -e $::env(EMPIREDIR)/scripts/zvcomb $selecfile &
} elseif {$exten == ".gnudat"} {
  exec xterm -e cp $selecfile  corrplot.dat
  exec xterm -e gnuplot $::env(EMPIREDIR)/util/kalman/corr.plt
  exec xterm -e rm corrplot.dat
} else {
  editFile $selecfile
}} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -background #d9d9d9 -labeltext {Available files:} -listvariable filelist \
        -selectioncommand {set selecfilelist [selection get]
set selecfile [lindex $selecfilelist 0]} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12 } -vscrollmode dynamic -width 150 
    vTcl:DefineAlias "$site_8_4.scr77" "Scrolledlistbox1" vTcl:WidgetProc "Toplevel1" 1
    ::iwidgets::entryfield $site_8_4.ent78 \
        \
        -command {#set filelist [glob -nocomplain *$profilter*]
adjourn .top75} \
        -labelfont {Helvetica -12 } -labeltext Filter: -textbackground white \
        -background #d9d9d9 -textvariable profilter -width 14 
    vTcl:DefineAlias "$site_8_4.ent78" "Entryfield2" vTcl:WidgetProc "Toplevel1" 1
    ::iwidgets::labeledframe $site_8_4.lab81 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -background #d9d9d9 -labelpos nw -labeltext {Selected file} 
    vTcl:DefineAlias "$site_8_4.lab81" "Labeledframe10" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 [$site_8_4.lab81 childsite]
    entry $site_10_0.ent82 \
        -background #ffffff -highlightbackground #d9d9d9 \
        -insertbackground black -textvariable selecfile -width 15 
    vTcl:DefineAlias "$site_10_0.ent82" "Entry7" vTcl:WidgetProc "Toplevel1" 1
    button $site_10_0.but83 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc \
        -command {set file [file root $selecfile]
set zvfilter $file
set profilter $file
set archfilter $file
adjourn .top75
Combobox1 clear
# create list of possible ddx plots
ddlist} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc -padx 1m \
        -text {Change to its project} 
    vTcl:DefineAlias "$site_10_0.but83" "Button126" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but83 "$site_10_0.but83 Button $top all _vTclBalloon"
    bind $site_10_0.but83 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Take root of the file name and  make it a current project}
    }
    button $site_10_0.but84 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #dcdcdc -command { editFile $selecfile } -cursor hand2 \
        -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc -text Edit 
    vTcl:DefineAlias "$site_10_0.but84" "Button127" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but84 "$site_10_0.but84 Button $top all _vTclBalloon"
    bind $site_10_0.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit selected file with the default editor}
    }
    button $site_10_0.but85 \
        -activebackground red -activeforeground white -background #dcdcdc \
        -command {if {[tk_dialog .dialogsi Confirm "Are you sure you want to delete all selected files?" "" 0 No Yes ] == 1} {
foreach el $selecfilelist {
   if {$el == ""} continue
   exec rm -f $el
}
set selecfile ""

adjourn .top75 }} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc \
        -text {Delete all selected} 
    vTcl:DefineAlias "$site_10_0.but85" "Button128" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but85 "$site_10_0.but85 Button $top all _vTclBalloon"
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
    pack $site_8_4.scr77 \
        -in $site_8_4 -anchor nw -expand 1 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_4.ent78 \
        -in $site_8_4 -anchor nw -expand 0 -fill none -pady 10 -side top 
    pack $site_8_4.lab81 \
        -in $site_8_4 -anchor nw -expand 1 -fill y -side top 
    set site_8_5 [lindex [$top.tab88 childsite] 5]
    ::iwidgets::scrolledlistbox $site_8_5.cpd67 \
        -activebackground #d9d9d9 -highlightthickness 5 -hscrollmode dynamic \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -background #d9d9d9 -labelpos nw -labeltext {Files in local directory:} \
        -listvariable svnfilelist -relief groove \
        -selectioncommand {set selsvnfilelist [selection get]} \
        -selectmode extended -textbackground #ffffff \
        -textfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -vscrollmode dynamic 
    vTcl:DefineAlias "$site_8_5.cpd67" "Scrolledlistbox11" vTcl:WidgetProc "Toplevel1" 1
    frame $site_8_5.fra68 \
        -borderwidth 2 -relief groove -background #d9d9d9 -height 75 \
        -width 288 
    vTcl:DefineAlias "$site_8_5.fra68" "Frame1" vTcl:WidgetProc "Toplevel1" 1
    set site_9_0 $site_8_5.fra68
    frame $site_9_0.fra68 \
        -borderwidth 2 -relief groove -background #d9d9d9 -height 87 \
        -width 260 
    vTcl:DefineAlias "$site_9_0.fra68" "Frame3" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 $site_9_0.fra68
    button $site_10_0.but75 \
        -activebackground #d9d9d9 -background #d9d9d9 \
        -command {
    if { $mt == "" } {
      tk_dialog .msgbox "Error" "Please enter a MT number for comparison." info 0 OK
    } else {
      if { $rev1 == "" || $rev2 == "" } {
        tk_dialog .msgbox "Error" "Please enter a revision number." info 0 OK
      } else {
        if {[file isdirectory rev$rev1] == 0} {
          set output [exec svn checkout $repository@$rev1 rev$rev1]
          set output [split $output "\n"]
          foreach elm $output {
            lappend svnoutput $elm
          }
          refreshsvndirectory
        }
        if {[file isdirectory rev$rev2] == 0} {
          set output [exec svn checkout $repository@$rev2 rev$rev2]
          set output [split $output "\n"]
          foreach elm $output {
            lappend svnoutput $elm
          }
          refreshsvndirectory
        }
        exec $::env(EMPIREDIR)/scripts/mtacomp $mt comp $file rev$rev1 Rev$rev1 rev$rev2/$file-s.endf Rev$rev2
        exec $::env(EMPIREDIR)/scripts/zvv $file-${mt}comp.zvd &
      }
    }
        } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text Plot 
    vTcl:DefineAlias "$site_10_0.but75" "Button4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but75 "$site_10_0.but75 Button $top all _vTclBalloon"
    bind $site_10_0.but75 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Produce ZVV plot for selected MT comparing the two revisions}
    }
    button $site_10_0.but76 \
        -activebackground #d9d9d9 -background #d9d9d9 \
        -command {
    if { $rev1 == "" && $rev2 == "" } {
      tk_dialog .msgbox "Error" "Please enter a revision number." info 0 OK
    } else {
      if {$rev1 != ""} {
        set output [exec svn checkout $repository@$rev1 rev$rev1]
        set output [split $output "\n"]
        foreach elm $output {
          lappend svnoutput $elm
        }
        refreshsvndirectory
      }
      if {$rev2 != ""} {
        set output [exec svn checkout $repository@$rev2 rev$rev2]
        set output [split $output "\n"]
        foreach elm $output {
          lappend svnoutput $elm
        }
        refreshsvndirectory
      }
    }
        } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text Retrieve 
    vTcl:DefineAlias "$site_10_0.but76" "Button5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but76 "$site_10_0.but76 Button $top all _vTclBalloon"
    bind $site_10_0.but76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Check out working copy of the revision(s) from the repository}
    }
    label $site_10_0.lab65 \
        -background #d9d9d9 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* -text MT 
    vTcl:DefineAlias "$site_10_0.lab65" "Label2" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.ent66 \
        -background white \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -insertbackground black -textvariable mt 
    vTcl:DefineAlias "$site_10_0.ent66" "Entry4" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent66 "$site_10_0.ent66 Entry $top all _vTclBalloon"
    bind $site_10_0.ent66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {MT number}
    }
    menubutton $site_10_0.men67 \
        -background #d9d9d9 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* -indicatoron 1 \
        -menu "$site_10_0.men67.m" -padx 4 -pady 3 -relief raised 
    vTcl:DefineAlias "$site_10_0.men67" "Menubutton2" vTcl:WidgetProc "Toplevel1" 1
    menu $site_10_0.men67.m \
        -tearoff 0 
    $site_10_0.men67.m add radiobutton \
        -value 1 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=1 (total)} 
    $site_10_0.men67.m add radiobutton \
        -value 2 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=2 (elastic)} 
    $site_10_0.men67.m add radiobutton \
        -value 4 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=4 (inelastic)} 
    $site_10_0.men67.m add radiobutton \
        -value 16 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=16 (x,2n)} 
    $site_10_0.men67.m add radiobutton \
        -value 17 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=17 (x,3n)} 
    $site_10_0.men67.m add radiobutton \
        -value 18 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=18 (x,f)} 
    $site_10_0.men67.m add radiobutton \
        -value 22 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=22 (x,na)} 
    $site_10_0.men67.m add radiobutton \
        -value 28 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=28 (x,np)} 
    $site_10_0.men67.m add radiobutton \
        -value 45 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=45 (x,npa)} 
    $site_10_0.men67.m add radiobutton \
        -value 102 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=102 (x,g)} 
    $site_10_0.men67.m add radiobutton \
        -value 103 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=103 (x,p)} 
    $site_10_0.men67.m add radiobutton \
        -value 107 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=107 (x,a)} 
    $site_10_0.men67.m add radiobutton \
        -value 112 -variable mt -command {# TODO: Your menu handler here} \
        -label {MT=112 (n,pa)} 
    label $site_10_0.lab66 \
        -background #d9d9d9 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* -text {Rev #1} 
    vTcl:DefineAlias "$site_10_0.lab66" "Label3" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.ent67 \
        -background white \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -insertbackground black \
        -textvariable rev1 
    vTcl:DefineAlias "$site_10_0.ent67" "Entry5" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent67 "$site_10_0.ent67 Entry $top all _vTclBalloon"
    bind $site_10_0.ent67 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Revision number}
    }
    label $site_10_0.lab68 \
        -background #d9d9d9 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* -text {Rev #2} 
    vTcl:DefineAlias "$site_10_0.lab68" "Label4" vTcl:WidgetProc "Toplevel1" 1
    entry $site_10_0.ent69 \
        -background white \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -insertbackground black \
        -textvariable rev2 
    vTcl:DefineAlias "$site_10_0.ent69" "Entry6" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.ent69 "$site_10_0.ent69 Entry $top all _vTclBalloon"
    bind $site_10_0.ent69 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Revision number}
    }
    place $site_10_0.but75 \
        -in $site_10_0 -x 135 -y 54 -width 100 -height 21 -anchor nw \
        -bordermode ignore 
    place $site_10_0.but76 \
        -in $site_10_0 -x 15 -y 54 -width 100 -height 21 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab65 \
        -in $site_10_0 -x 136 -y 5 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent66 \
        -in $site_10_0 -x 136 -y 23 -width 50 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.men67 \
        -in $site_10_0 -x 193 -y 21 -width 59 -height 24 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab66 \
        -in $site_10_0 -x 15 -y 5 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent67 \
        -in $site_10_0 -x 15 -y 23 -width 50 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_10_0.lab68 \
        -in $site_10_0 -x 73 -y 5 -width 50 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_10_0.ent69 \
        -in $site_10_0 -x 74 -y 23 -width 50 -height 22 -anchor nw \
        -bordermode ignore 
    button $site_9_0.cpd70 \
        -background #d9d9d9 \
        -command {
#  set addfiles {}
  foreach el $selsvnfilelist {
      lappend svnoutput [exec svn add $el]
#      lappend addfiles $el
  }
#  set output [open "| svn add $addfiles"]
#  set svnoutput $svnoutput$output

  if {[tk_dialog .dialogsi Confirm "Do you want to commit your changes right now?" "" 0 No Yes ] == 1} {
    set output [exec svn commit --editor-cmd $editor]
    set output [split $output "\n"]
    foreach elm $output {
      lappend svnoutput $elm
    }
  }
    } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text Add 
    vTcl:DefineAlias "$site_9_0.cpd70" "Button1" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd70 "$site_9_0.cpd70 Button $top all _vTclBalloon"
    bind $site_9_0.cpd70 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Schedule files or directories for addition to the repository}
    }
    button $site_9_0.cpd71 \
        \
        -command {
    set output [exec svn commit --editor-cmd $editor]
    set output [split $output "\n"]
    foreach elm $output {
      lappend svnoutput $elm
    }
        } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text Commit 
    vTcl:DefineAlias "$site_9_0.cpd71" "Button2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd71 "$site_9_0.cpd71 Button $top all _vTclBalloon"
    bind $site_9_0.cpd71 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Send changes from your working copy to the repository}
    }
    button $site_9_0.cpd72 \
        \
        -command {
    set output [exec svn update]
    set output [split $output "\n"]
    foreach elm $output {
      lappend svnoutput $elm
    }
        } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text Update 
    vTcl:DefineAlias "$site_9_0.cpd72" "Button3" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.cpd72 "$site_9_0.cpd72 Button $top all _vTclBalloon"
    bind $site_9_0.cpd72 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Update your working copy}
    }
    label $site_9_0.lab65 \
        -background #d9d9d9 -text Repository: 
    vTcl:DefineAlias "$site_9_0.lab65" "Label5" vTcl:WidgetProc "Toplevel1" 1
    entry $site_9_0.ent66 \
        -disabledbackground white \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -insertbackground black \
        -textvariable repository 
    vTcl:DefineAlias "$site_9_0.ent66" "Entry11" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.ent66 "$site_9_0.ent66 Entry $top all _vTclBalloon"
    bind $site_9_0.ent66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Enter here link to the SVN repository for the project}
    }
    labelframe $site_9_0.lab66 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -background #d9d9d9 -foreground black -text Output -height 185 -width 260 
    vTcl:DefineAlias "$site_9_0.lab66" "Labelframe1" vTcl:WidgetProc "Toplevel1" 1
    set site_10_0 $site_9_0.lab66
    ::iwidgets::scrolledlistbox $site_10_0.scr67 \
        -listvariable svnoutput -textbackground #ffffff 
    vTcl:DefineAlias "$site_10_0.scr67" "Scrolledlistbox4" vTcl:WidgetProc "Toplevel1" 1
    place $site_10_0.scr67 \
        -in $site_10_0 -x 15 -y 21 -width 232 -height 150 -anchor nw \
        -bordermode ignore 
    button $site_9_0.but65 \
        \
        -command {
    if {[tk_dialog .dialogsi Confirm "Do you really want to delete the selected directories or files?" "" 0 No Yes ] == 1} {
      foreach el $selsvnfilelist {
        lappend svnoutput [exec rm -rf $el]
      }
      refreshsvndirectory
    }
        } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text {Delete local files} 
    vTcl:DefineAlias "$site_9_0.but65" "Button12" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but65 "$site_9_0.but65 Button $top all _vTclBalloon"
    bind $site_9_0.but65 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete local directories or file}
    }
    button $site_9_0.but66 \
        -command { refreshsvndirectory } \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -highlightbackground #d9d9d9 -text Refresh 
    vTcl:DefineAlias "$site_9_0.but66" "Button13" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_9_0.but66 "$site_9_0.but66 Button $top all _vTclBalloon"
    bind $site_9_0.but66 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Refresh local directory}
    }
    place $site_9_0.fra68 \
        -in $site_9_0 -x 15 -y 31 -width 260 -height 87 -anchor nw \
        -bordermode ignore 
    place $site_9_0.cpd70 \
        -in $site_9_0 -x 15 -y 125 -width 80 -height 21 -anchor nw \
        -bordermode ignore 
    place $site_9_0.cpd71 \
        -in $site_9_0 -x 105 -y 125 -width 80 -height 21 -anchor nw \
        -bordermode ignore 
    place $site_9_0.cpd72 \
        -in $site_9_0 -x 195 -y 125 -width 80 -height 21 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab65 \
        -in $site_9_0 -x 15 -y 5 -width 75 -height 20 -anchor nw \
        -bordermode ignore 
    place $site_9_0.ent66 \
        -in $site_9_0 -x 93 -y 6 -width 183 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_9_0.lab66 \
        -in $site_9_0 -x 15 -y 185 -width 260 -height 185 -anchor nw \
        -bordermode ignore 
    place $site_9_0.but65 \
        -in $site_9_0 -x 150 -y 155 -width 126 -height 21 -anchor nw \
        -bordermode ignore 
    place $site_9_0.but66 \
        -in $site_9_0 -x 15 -y 155 -width 126 -height 21 -anchor nw \
        -bordermode ignore 
    ::iwidgets::scrolledlistbox $site_8_5.scr66 \
        -highlightthickness 5 -hscrollmode dynamic \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -background #d9d9d9 -labelpos nw -labeltext {Local directory commit history:} \
        -listvariable svnlog -textbackground #ffffff \
        -textfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -vscrollmode dynamic 
    vTcl:DefineAlias "$site_8_5.scr66" "Scrolledlistbox12" vTcl:WidgetProc "Toplevel1" 1
    pack $site_8_5.cpd67 \
        -in $site_8_5 -anchor nw -expand 1 -fill both -side left 
    pack $site_8_5.fra68 \
        -in $site_8_5 -anchor center -expand 0 -fill y -side left 
    pack $site_8_5.scr66 \
        -in $site_8_5 -anchor ne -expand 1 -fill both -side right 
    set site_8_6 [lindex [$top.tab88 childsite] 6]
    ::iwidgets::scrolledlistbox $site_8_6.scr77 \
        -activebackground #dcdcdc -hscrollmode dynamic \
        -labelfont {Helvetica -12 } -labelpos nw \
        -background #d9d9d9 -labeltext {Other working folders:} -listvariable archdirlist \
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
    ::iwidgets::scrolledlistbox $site_8_6.scr78 \
        -activebackground #dcdcdc \
        -dblclickcommand {set selarchfilelist [selection get]
set archfile [lindex $selarchfilelist 0]
set archexten [file extension $archfile]
if {$archexten == ".ps"} {
  pspdfView $archdir/$archfile
} elseif {$archexten == ".eps"} {
  pspdfView $archdir/$archfile
} elseif {$archexten == ".zvd"} {
  exec xterm -e $::env(EMPIREDIR)/scripts/zvcomb $archdir/$archfile &
} else {
  editFile $archdir/$archfile
}} \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -background #d9d9d9 -labeltext {Available files:} -listvariable archfilelist \
        -selectioncommand {set selarchfilelist [selection get]
set archfile [lindex $selarchfilelist 0]} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12 } -vscrollmode dynamic -width 150 
    frame $site_8_6.fra78 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    set site_9_0 $site_8_6.fra78
    ::iwidgets::entryfield $site_9_0.ent79 \
        -command {adjourn .top75} -labelfont {Helvetica -12 } \
        -labeltext Folder: -textbackground white -textvariable archdir \
        -background #d9d9d9 -width 14 
    ::iwidgets::entryfield $site_9_0.ent80 \
        -command {adjourn .top75} -labelfont {Helvetica -12 } \
        -labeltext Filter: -textbackground white -textvariable archfilter \
        -background #d9d9d9 -width 14 
    ::iwidgets::labeledframe $site_9_0.lab81 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -background #d9d9d9 -labelpos nw -labeltext {Selected file} 
    set site_11_0 [$site_9_0.lab81 childsite]
    entry $site_11_0.ent82 \
        -background white -highlightbackground #d9d9d9 \
        -insertbackground black -textvariable archfile -width 15 
    button $site_11_0.but84 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command { editFile $archdir/$archfile } \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc -text {Edit file} 
    bindtags $site_11_0.but84 "$site_11_0.but84 Button $top all _vTclBalloon"
    bind $site_11_0.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit selected file with the default editor}
    }
    button $site_11_0.but85 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {if {[tk_dialog .dialogsi Confirm "Are you sure you want to delete all selected files?" "" 0 No Yes ] == 1} {
foreach el $selarchfilelist {
   if {$el == ""} continue
   exec rm -f $archdir/$el
}
set archfile ""
adjourn .top75 }} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc \
        -text {Delete selected} 
    bindtags $site_11_0.but85 "$site_11_0.but85 Button $top all _vTclBalloon"
    bind $site_11_0.but85 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete all selected files}
    }
    pack $site_11_0.ent82 \
        -in $site_11_0 -anchor nw -expand 0 -fill x -padx 5 -side top 
    pack $site_11_0.but84 \
        -in $site_11_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 -side top 
    pack $site_11_0.but85 \
        -in $site_11_0 -anchor nw -expand 0 -fill x -padx 5 -pady 2 \
        -side bottom 
    pack $site_9_0.ent79 \
        -in $site_9_0 -anchor ne -expand 0 -fill none -padx 10 -pady 15 \
        -side top 
    pack $site_9_0.ent80 \
        -in $site_9_0 -anchor ne -expand 0 -fill none -padx 10 -side top 
    pack $site_9_0.lab81 \
        -in $site_9_0 -anchor ne -expand 1 -fill y -ipady 5 -padx 5 -pady 5 \
        -side top 
    frame $site_8_6.fra82 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    set site_9_0 $site_8_6.fra82
    button $site_9_0.but84 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/store $archdir $file
adjourn .top75} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkgreen -highlightbackground #dcdcdc \
        -text {<= Store project} 
    bindtags $site_9_0.but84 "$site_9_0.but84 Button $top all _vTclBalloon"
    bind $site_9_0.but84 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store current project in the folder. NOTE: folder name should start with ../ }
    }
    button $site_9_0.but86 \
        -activebackground #ff0000 -activeforeground white -background #efefef \
        -command {if {[tk_dialog .dialogsi Confirm "Are you sure you want to delete the folder?" "" 0 No Yes ] == 1} {
exec rm -r -f $archdir
set archdirlist [glob -nocomplain $::env(EMPIREDIR)/*/]
set archfilelist "" }} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc \
        -text {Delete folder} 
    bindtags $site_9_0.but86 "$site_9_0.but86 Button $top all _vTclBalloon"
    bind $site_9_0.but86 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Delete folder including its content}
    }
    pack $site_9_0.but84 \
        -in $site_9_0 -anchor nw -expand 0 -fill x -padx 5 -pady 12 -side top 
    pack $site_9_0.but86 \
        -in $site_9_0 -anchor nw -expand 0 -fill x -padx 5 -pady 17 \
        -side bottom 
    pack $site_8_6.scr77 \
        -in $site_8_6 -anchor nw -expand 1 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_6.scr78 \
        -in $site_8_6 -anchor nw -expand 1 -fill both -padx 10 -pady 5 \
        -side left 
    pack $site_8_6.fra78 \
        -in $site_8_6 -anchor nw -expand 0 -fill y -side left 
    pack $site_8_6.fra82 \
        -in $site_8_6 -anchor ne -expand 1 -fill y -side top 
    set site_8_7 [lindex [$top.tab88 childsite] 7]
    frame $site_8_7.fra83 \
        -background #d9d9d9 -highlightbackground #dcdcdc \
        -highlightcolor black 
    bindtags $site_8_7.fra83 "itk-destroy-.top75.tab88.canvas.notebook.cs.page9.cs.fra83 .top75.tab88.canvas.notebook.cs.page6.cs Frame $top all"
    bind $site_8_7.fra83 <Configure> {
        namespace inscope ::iwidgets::Tabnotebook {::.top75.tab88 _pageReconfigure .top75.tab88.canvas.notebook.cs.page9.cs.fra83 5 %w %h}
    }
    set site_9_0 $site_8_7.fra83
    ::iwidgets::labeledframe $site_9_0.lab84 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext {Multiple run} 
    vTcl:DefineAlias "$site_9_0.lab84" "Labeledframe12" vTcl:WidgetProc "Toplevel1" 1
    set site_11_0 [$site_9_0.lab84 childsite]
    frame $site_11_0.fra86 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    vTcl:DefineAlias "$site_11_0.fra86" "Frame15" vTcl:WidgetProc "Toplevel1" 1
    set site_12_0 $site_11_0.fra86
    ::iwidgets::scrolledlistbox $site_12_0.scr87 \
        -activebackground #dcdcdc -hscrollmode none -labelpos nw \
        -selectioncommand {set stablist ""
set temp [selection get]
foreach el $temp {
set elf [split $el =]
lappend stablist [lindex $elf 0]
}} \
        -selectmode extended -textbackground #ffffff \
        -textfont {Helvetica -12} -visibleitems 20x8 
    vTcl:DefineAlias "$site_12_0.scr87" "Scrolledlistbox2" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.scr87 "itk-delete-.top75.tab88.canvas.notebook.cs.page7.cs.fra83.lab84.childsite.fra86.scr87 $site_12_0.scr87 Scrolledlistbox $top all _vTclBalloon"
    bind $site_12_0.scr87 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Load list stored under 'List name'}
    }
    button $site_12_0.but88 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef -command {readabun } -cursor hand2 \
        -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -state disabled -text {Load all} 
    vTcl:DefineAlias "$site_12_0.but88" "Button139" vTcl:WidgetProc "Toplevel1" 1
    button $site_12_0.but76 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {readabun $::env(EMPIREDIR)/RIPL/masses/abundance.dat} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -text {Load stable} 
    vTcl:DefineAlias "$site_12_0.but76" "Button142" vTcl:WidgetProc "Toplevel1" 1
    pack $site_12_0.scr87 \
        -in $site_12_0 -anchor n -expand 1 -fill y -side top 
    pack $site_12_0.but88 \
        -in $site_12_0 -anchor e -expand 0 -fill x -side right 
    pack $site_12_0.but76 \
        -in $site_12_0 -anchor w -expand 0 -fill x -side left 
    frame $site_11_0.fra89 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    vTcl:DefineAlias "$site_11_0.fra89" "Frame16" vTcl:WidgetProc "Toplevel1" 1
    set site_12_0 $site_11_0.fra89
    ::iwidgets::scrolledlistbox $site_12_0.scr87 \
        -activebackground #dcdcdc \
        -dblclickcommand {setmulpro $selmulitem $mulstname
adjourn .top75
ddlist} \
        -hscrollmode none -labelfont {Helvetica -12 } -labelpos nw \
        -listvariable stablist \
        -selectioncommand {set selmulitem [selection get]} -selectmode single \
        -textbackground #ffffff -textfont {Helvetica -12 } -visibleitems 10x8 
    vTcl:DefineAlias "$site_12_0.scr87" "Scrolledlistbox3" vTcl:WidgetProc "Toplevel1" 1
    button $site_12_0.but88 \
        -activebackground #eccceccceccc -background #efefef \
        -command {lappend stablist $adnuc} -cursor hand2 \
        -font {Helvetica -12 } -highlightbackground #dcdcdc -text ^ 
    vTcl:DefineAlias "$site_12_0.but88" "Button140" vTcl:WidgetProc "Toplevel1" 1
    entry $site_12_0.ent90 \
        -background white -textvariable adnuc -width 8 
    vTcl:DefineAlias "$site_12_0.ent90" "Entry8" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.ent90 "$site_12_0.ent90 Entry $top all _vTclBalloon"
    bind $site_12_0.ent90 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Additional nucleus (e.g., 26-Fe-56) to be included in the list}
    }
    pack $site_12_0.scr87 \
        -in $site_12_0 -anchor n -expand 1 -fill y -side top 
    pack $site_12_0.but88 \
        -in $site_12_0 -anchor center -expand 0 -fill none -side right 
    pack $site_12_0.ent90 \
        -in $site_12_0 -anchor center -expand 0 -fill both -side left 
    frame $site_11_0.fra76 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    vTcl:DefineAlias "$site_11_0.fra76" "Frame17" vTcl:WidgetProc "Toplevel1" 1
    set site_12_0 $site_11_0.fra76
    button $site_12_0.but77 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef -command { set stablist "" } -cursor hand2 \
        -font {Helvetica -12 } -foreground darkred \
        -highlightbackground #dcdcdc -text {Clear list} 
    vTcl:DefineAlias "$site_12_0.but77" "Button141" vTcl:WidgetProc "Toplevel1" 1
    ::iwidgets::entryfield $site_12_0.ent77 \
        -justify right -labelfont {Helvetica -12 } -labelpos nw \
        -labeltext {List name:} -textbackground #ffffff \
        -background #d9d9d9 -textvariable mulstname -width 12 
    vTcl:DefineAlias "$site_12_0.ent77" "Entryfield6" vTcl:WidgetProc "Toplevel1" 1
    button $site_12_0.but78 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {set mulfile [open $mulstname.mulst w]
foreach el $stablist {
puts $mulfile $el
}
close $mulfile} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -text {Save list} 
    vTcl:DefineAlias "$site_12_0.but78" "Button144" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.but78 "$site_12_0.but78 Button $top all _vTclBalloon"
    bind $site_12_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store list under "List name".mulst}
    }
    button $site_12_0.but79 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {set mulfile [open $mulstname.mulst r]
while {[gets $mulfile line] >= 0} {
lappend  stablist $line
}
close $mulfile} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -text {Load list} 
    vTcl:DefineAlias "$site_12_0.but79" "Button145" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.but79 "$site_12_0.but79 Button $top all _vTclBalloon"
    bind $site_12_0.but79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Load list stored under 'List name'}
    }
    button $site_12_0.but76 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef -command {runlist $stablist $mulstname} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkred \
        -highlightbackground #dcdcdc -text {Run list} 
    vTcl:DefineAlias "$site_12_0.but76" "Button143" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_12_0.but76 "$site_12_0.but76 Button $top all _vTclBalloon"
    bind $site_12_0.but76 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Launch full sequence of calculations for each element of the list}
    }
    pack $site_12_0.but77 \
        -in $site_12_0 -anchor center -expand 0 -fill x -side top 
    pack $site_12_0.ent77 \
        -in $site_12_0 -anchor center -expand 0 -fill none -side top 
    pack $site_12_0.but78 \
        -in $site_12_0 -anchor center -expand 0 -fill x -pady 5 -side top 
    pack $site_12_0.but79 \
        -in $site_12_0 -anchor center -expand 0 -fill x -side top 
    pack $site_12_0.but76 \
        -in $site_12_0 -anchor center -expand 0 -fill x -side bottom 
    ::iwidgets::optionmenu $site_11_0.opt86 \
        -activeforeground limegreen -command ViewAll -font {Helvetica -12 } \
        -background #d9d9d9 -foreground darkgreen -labelfont {Helvetica -12 } -labeltext {} 
    vTcl:DefineAlias "$site_11_0.opt86" "Optionmenu3" vTcl:WidgetProc "Toplevel1" 1
    $site_11_0.opt86 insert 1 {View:}
    $site_11_0.opt86 insert 2 {inputs}
    $site_11_0.opt86 insert 3 {warnings}
    $site_11_0.opt86 insert 4 {PLOTC4-log}
    $site_11_0.opt86 insert 5 {X4TOC4-log}
    $site_11_0.opt86 insert 6 {EMPEND-log}
    $site_11_0.opt86 insert 7 {dir-omp}
    $site_11_0.opt86 insert 8 {RIPL-omp}
    $site_11_0.opt86 insert 9 {levels}
    $site_11_0.opt86 insert 10 {coll-levels}
    $site_11_0.opt86 insert 11 {EXFORs}
    $site_11_0.opt86 insert 12 {C4s}
    $site_11_0.opt86 insert 13 {ENDFs}
    $site_11_0.opt86 insert 14 {short-outputs}
    $site_11_0.opt86 insert 15 {full-outputs}
    $site_11_0.opt86 insert 17 {PLOTC4-plots}
    ::iwidgets::checkbox $site_11_0.che79 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos n -labeltext {Keep only:} -relief flat 
    vTcl:DefineAlias "$site_11_0.che79" "Checkbox3" vTcl:WidgetProc "Toplevel1" 1
    $site_11_0.che79 add chk0 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .lst -onvalue {} -selectcolor #00ff00 \
        -text { full output} -variable ckmlo 
    $site_11_0.che79 add chk1 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .out -onvalue {} -selectcolor #00ff00 \
        -text { short output} -variable ckmsh 
    $site_11_0.che79 add chk2 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -log* -onvalue {} -selectcolor #ffff00 \
        -text { log files} -variable ckmlog 
    $site_11_0.che79 add chk3 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue *.endf -onvalue {} -selectcolor #00ff00 \
        -text { ENDF} -variable ckmendf 
    $site_11_0.che79 add chk4 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .ps -onvalue {} -selectcolor #00ff00 \
        -text { PLOTC4 plots} -variable ckmplots 
    $site_11_0.che79 add chk5 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .exf -onvalue {} -selectcolor #ffff00 \
        -text { EXFOR} -variable ckmx4 
    $site_11_0.che79 add chk6 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .c4 -onvalue {} -selectcolor #ffff00 \
        -text { C4 file} -variable ckmc4 
    ::iwidgets::checkbox $site_11_0.che76 \
        -background #d9d9d9 \
        -labelfont -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -labelpos n -labeltext {  (need green)} -relief flat 
    vTcl:DefineAlias "$site_11_0.che76" "Checkbox4" vTcl:WidgetProc "Toplevel1" 1
    $site_11_0.che76 add chk0 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -omp.ripl -onvalue {} -selectcolor #ffff00 \
        -text { RIPL omp} -variable ckmriplomp 
    $site_11_0.che76 add chk1 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -omp.dir -onvalue {} -selectcolor #ffff00 \
        -text { direct omp} -variable ckmdiromp 
    $site_11_0.che76 add chk2 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .lev -onvalue {} -selectcolor #ffff00 \
        -text { levels} -variable ckmlev 
    $site_11_0.che76 add chk3 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue -lev.col -onvalue {} -selectcolor #ffff00 \
        -text { coll. levels} -variable ckmcollev 
    $site_11_0.che76 add chk4 \
        -activebackground #f7fbf7 -activeforeground #000000 -anchor w \
        -disabledforeground #a1a4a1 \
        -font -Adobe-Helvetica-Bol-R-Normal--*-120-*-*-*-*-*-* \
        -foreground black -highlightcolor #000000 -highlightthickness 0 \
        -justify left -offvalue .inp -onvalue {} -selectcolor #ffff00 \
        -text { input} -variable ckminp 
    ::iwidgets::labeledframe $site_11_0.lab76 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext {Selected item} 
    vTcl:DefineAlias "$site_11_0.lab76" "Labeledframe13" vTcl:WidgetProc "Toplevel1" 1
    set site_13_0 [$site_11_0.lab76 childsite]
    entry $site_13_0.ent77 \
        -background white -highlightbackground #d9d9d9 \
        -textvariable selmulitem -width 15 
    vTcl:DefineAlias "$site_13_0.ent77" "Entry9" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_13_0.ent77 "$site_13_0.ent77 Entry $top all _vTclBalloon"
    bind $site_13_0.ent77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Selected element of the list}
    }
    button $site_13_0.but78 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {setmulpro $selmulitem $mulstname
adjourn .top75
ddlist} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -text {Set as project} 
    vTcl:DefineAlias "$site_13_0.but78" "Button149" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_13_0.but78 "$site_13_0.but78 Button $top all _vTclBalloon"
    bind $site_13_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Make the selected list item the current project}
    }
    pack $site_13_0.ent77 \
        -in $site_13_0 -anchor center -expand 0 -fill none -padx 4 -side top 
    pack $site_13_0.but78 \
        -in $site_13_0 -anchor center -expand 0 -fill x -padx 5 -side top 
    ::iwidgets::labeledframe $site_11_0.lab79 \
        -labelfont -Adobe-Helvetica--R-Normal--*-120-*-*-*-*-*-* -labelpos nw \
        -background #d9d9d9 -labeltext Folder 
    vTcl:DefineAlias "$site_11_0.lab79" "Labeledframe14" vTcl:WidgetProc "Toplevel1" 1
    set site_13_0 [$site_11_0.lab79 childsite]
    entry $site_13_0.ent77 \
        -background white -highlightbackground #d9d9d9 -textvariable archdir \
        -width 15 
    vTcl:DefineAlias "$site_13_0.ent77" "Entry10" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_13_0.ent77 "$site_13_0.ent77 Entry $top all _vTclBalloon"
    bind $site_13_0.ent77 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Enetr name of the folder to store results for the current list}
    }
    button $site_13_0.but78 \
        -activebackground #eccceccceccc -activeforeground limegreen \
        -background #efefef \
        -command {ArchiveList $archdir $stablist $mulstname
adjourn .top75} \
        -cursor hand2 -font {Helvetica -12 } -foreground darkgreen \
        -highlightbackground #dcdcdc -text {Store list results} 
    vTcl:DefineAlias "$site_13_0.but78" "Button150" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_13_0.but78 "$site_13_0.but78 Button $top all _vTclBalloon"
    bind $site_13_0.but78 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Store all files related to the list in the folder specified above }
    }
    pack $site_13_0.ent77 \
        -in $site_13_0 -anchor center -expand 0 -fill none -padx 4 -side top 
    pack $site_13_0.but78 \
        -in $site_13_0 -anchor center -expand 0 -fill x -padx 5 -side top 
    pack $site_11_0.fra86 \
        -in $site_11_0 -anchor center -expand 0 -fill y -padx 2 -side left 
    pack $site_11_0.fra89 \
        -in $site_11_0 -anchor center -expand 0 -fill y -side left 
    pack $site_11_0.fra76 \
        -in $site_11_0 -anchor center -expand 0 -fill y -ipadx 2 -padx 2 \
        -side left 
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
    pack $site_9_0.lab84 \
        -in $site_9_0 -anchor center -expand 1 -fill both -side left 
    pack $site_8_7.fra83 \
        -in $site_8_7 -anchor center -expand 1 -fill both -side top 
    set site_8_8 [lindex [$top.tab88 childsite] 8]
    frame $site_8_8.fra84 \
        -background #d9d9d9 -highlightbackground #dcdcdc \
        -highlightcolor black 
    bindtags $site_8_8.fra84 "itk-destroy-.top75.tab88.canvas.notebook.cs.page10.cs.fra84 .top75.tab88.canvas.notebook.cs.page7.cs Frame $top all"
    bind $site_8_8.fra84 <Configure> {
        namespace inscope ::iwidgets::Tabnotebook {::.top75.tab88 _pageReconfigure .top75.tab88.canvas.notebook.cs.page10.cs.fra84 6 %w %h}
    }
    set site_9_0 $site_8_8.fra84
    ::iwidgets::scrolledlistbox $site_9_0.scrolledlistbox83 \
        -activebackground #dcdcdc -cursor {} \
        -dblclickcommand { editFile $::env(EMPIREDIR)/source/[selection get] } \
        -hscrollmode dynamic -labelfont {Helvetica -12 } -labelpos nw \
        -background #d9d9d9 -labeltext {Doubleclick to edit} -listvariable modules \
        -textbackground White -textfont {Helvetica -12 } -width 180 
    frame $site_9_0.fra79 \
        -borderwidth 2 -background #d9d9d9 -height 75 \
        -highlightbackground #dcdcdc -width 125 
    set site_10_0 $site_9_0.fra79
    button $site_10_0.but80 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command { editFile $::env(EMPIREDIR)/source/dimension.h } \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc \
        -text {Edit dimensions} -wraplength 0 
    bindtags $site_10_0.but80 "$site_10_0.but80 Button $top all _vTclBalloon"
    bind $site_10_0.but80 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Edit dimension.h file setting dimensions in EMPIRE}
    }
    button $site_10_0.but79 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {cd $::env(EMPIREDIR)/source
exec xterm -e make
cd $workdir} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc -text Make 
    vTcl:DefineAlias "$site_10_0.but79" "Button135" vTcl:WidgetProc "Toplevel1" 1
    bindtags $site_10_0.but79 "$site_10_0.but79 Button $top all _vTclBalloon"
    bind $site_10_0.but79 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compile EMPIRE core (no utility codes)}
    }
    button $site_10_0.but81 \
        -activebackground #eccceccceccc -activeforeground red \
        -background #efefef \
        -command {cd $::env(EMPIREDIR)/
exec xterm -e make
cd $workdir} \
        -cursor hand2 -disabledforeground #a1a4a1 -font {Helvetica -12 } \
        -foreground darkred -highlightbackground #dcdcdc -text {Make all} 
    bindtags $site_10_0.but81 "$site_10_0.but81 Button $top all _vTclBalloon"
    bind $site_10_0.but81 <<SetBalloon>> {
        set ::vTcl::balloon::%W {Compile whole the package (including utility codes)}
    }
    pack $site_10_0.but80 \
        -in $site_10_0 -anchor center -expand 0 -fill x -ipady 5 -pady 5 \
        -side top 
    pack $site_10_0.but79 \
        -in $site_10_0 -anchor center -expand 0 -fill x -ipady 10 -side top 
    pack $site_10_0.but81 \
        -in $site_10_0 -anchor center -expand 0 -fill x -ipady 10 -pady 5 \
        -side top 
    pack $site_9_0.scrolledlistbox83 \
        -in $site_9_0 -anchor nw -expand 0 -fill y -padx 10 -pady 5 \
        -side left 
    pack $site_9_0.fra79 \
        -in $site_9_0 -anchor nw -expand 0 -fill none -ipady 5 -padx 5 \
        -pady 27 -side left 
    pack $site_8_8.fra84 \
        -in $site_8_8 -anchor center -expand 1 -fill both -side top 
    $top.tab88 select 0
    menu $top.m88 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -cursor {} -foreground #000000 
    $top.m88 add cascade \
        -menu "$top.m88.menu89" -command {} -label File 
    set site_3_0 $top.m88
    menu $site_3_0.menu89 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $site_3_0.menu89 add command \
        \
        -command {set rcfl [open $::env(EMPIREDIR)/.Xrunrc w+]
puts $rcfl $file
puts $rcfl $editor
puts $rcfl $workdir
puts $rcfl $psviewer
puts $rcfl $wwwviewer
puts $rcfl $compeval
puts $rcfl $mat
puts $rcfl $EXPDAT
close $rcfl


exit} \
        -label Exit 
    $top.m88 add cascade \
        -menu "$top.m88.menu90" -command {} -label Options 
    set site_3_0 $top.m88
    menu $site_3_0.menu90 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/scripts/skel.inp } \
        -label {Default input} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/scripts/skel-inp.sen } \
        -label {Default sensitivity input} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/util/empend/EMPEND.INP } \
        -label {EMPEND input} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/util/endres/ENDRES.INP } \
        -label {ENDRES input} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/util/fixup/FIXUP.INP } \
        -label {FIXUP input} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/util/c4sort/C4SORT.INP } \
        -label {C4SORT input} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/util/c4zvd/ps01.tit } \
        -label {ZVView options} 
    $site_3_0.menu90 add command \
        -command { editFile $::env(EMPIREDIR)/source/Makefile } \
        -label {Edit Makefile} 
    $site_3_0.menu90 add cascade \
        -menu "$site_3_0.menu90.menu77" -command {} -label {KALMAN option} 
    set site_4_0 $site_3_0.menu90
    menu $site_4_0.menu77 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 1 
    $site_4_0.menu77 add radiobutton \
        -value 0 -variable EXPDAT -label {No exp. data} 
    $site_4_0.menu77 add radiobutton \
        -value 1 -variable EXPDAT -label {Exp. data for selected reaction} 
    $site_4_0.menu77 add radiobutton \
        -value 2 -variable EXPDAT -label {All exp. data} 
    $site_3_0.menu90 add cascade \
        -menu "$site_3_0.menu90.menu97" -command {} -label {Select editor} 
    set site_4_0 $site_3_0.menu90
    menu $site_4_0.menu97 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $site_4_0.menu97 add radiobutton \
        -value gedit -variable editor -command {} -label gedit 
    $site_4_0.menu97 add radiobutton \
        -value gvim -variable editor -command {} -label gvim 
    $site_4_0.menu97 add radiobutton \
        -value emacs -variable editor -command {} -label emacs 
    $site_4_0.menu97 add radiobutton \
        -value xemacs -variable editor -command {} -label xemacs 
    $site_4_0.menu97 add radiobutton \
        -value kedit -variable editor -command {} -label kedit 
    $site_4_0.menu97 add radiobutton \
        -value kate -variable editor -command {} -label kate 
    $site_4_0.menu97 add radiobutton \
        -value nedit -variable editor -command {} -label nedit 
    $site_4_0.menu97 add radiobutton \
        -value gotfile -variable editor \
        -command {
set editor [tk_getOpenFile -parent .top75 -title "Select editor"]} \
        -label other 
    $site_3_0.menu90 add cascade \
        -menu "$site_3_0.menu90.men87" -command {} -label {Select PS/pdf viewer} 
    set site_4_0 $site_3_0.menu90
    menu $site_4_0.men87 \
        -tearoff 0 

    $site_4_0.men87 add radiobutton \
        -value evince -variable psviewer -command {} -label evince 
    $site_4_0.men87 add radiobutton \
        -value kpdf -variable psviewer -command {} -label kpdf 
    $site_4_0.men87 add radiobutton \
        -value kghostview -variable psviewer -command {} -label kghostview 
    $site_4_0.men87 add radiobutton \
        -value kghostview -variable psviewer -command {} -label kghostview 
    $site_4_0.men87 add radiobutton \
        -value gv -variable psviewer -command {} -label gv 
    $site_4_0.men87 add radiobutton \
        -value ggv -variable psviewer \
        -command {# TODO: Your menu handler here} -label ggv 
    $site_4_0.men87 add radiobutton \
        -value gotfile -variable psviewer \
        -command {
set psviewer [tk_getOpenFile -parent .top75 -title "Select PS/pdf viewer"]} \
        -label other 
    $top.m88 add cascade \
        -menu "$top.m88.menu92" -command {} -label Inputs 
    set site_3_0 $top.m88
    menu $site_3_0.menu92 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 1 
    $site_3_0.menu92 add command \
        -command {file copy -force $::env(EMPIREDIR)/scripts/skel.inp $file.inp} -label {Create input} 
    $site_3_0.menu92 add command \
        -command { editFile $file.inp } -label {Edit input} 
    $site_3_0.menu92 add command \
        -command { editFile $file-inp.sen } -label {Sensitivity input} 
    $site_3_0.menu92 add separator \
        
    $site_3_0.menu92 add command \
        -command { editFile $file-omp.ripl } -label {OM parameters} 
    $site_3_0.menu92 add command \
        -command { editFile $file-omp.dir } -label {OM par. for direct} 
    $site_3_0.menu92 add separator \
        
    $site_3_0.menu92 add command \
        -command { editFile $file.lev } -label {Discrete levels} 
    $site_3_0.menu92 add command \
        -command { editFile $file-lev.col } -label {Collective levels} 
    $site_3_0.menu92 add separator \
        
    $site_3_0.menu92 add command \
        -command { editFile $file-inp.fis } -label {Fission input} 
    $top.m88 add cascade \
        -menu "$top.m88.menu93" -command {} -label Execute 
    set site_3_0 $top.m88
    menu $site_3_0.menu93 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 1 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/run $file $mat &} \
        -label {Full run} 
    $site_3_0.menu93 add separator \
        
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/runE $file &} \
        -label EMPIRE 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/format $file &} \
        -label Format 
    $site_3_0.menu93 add command \
        \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/addresonances $file &} \
        -label {Add resonances} 
    $site_3_0.menu93 add command \
        \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/rec-elastic $file &} \
        -label {Reconstruct elastic} 
    $site_3_0.menu93 add command \
        \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/rec-total $file &} \
        -label {Reconstruct total} 
    $site_3_0.menu93 add command \
        \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/rec-ch-part $file &} \
        -label {Reconstruct (n,p), (n,a) and inel} 
    $site_3_0.menu93 add command \
        \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/accept-omp-fit $file &} \
        -label {Accept last OMP fit} 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/verify $file &} \
        -label Verify 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/process $file 1 &} \
        -label PreProcess 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/plotlst $file &} \
        -label {Plot list} 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/plot $file &} \
        -label PLOTC4 
    $site_3_0.menu93 add separator \
        
    $site_3_0.menu93 add command \
        \
        -command {set emp_inp [open $file.inp r]
  gets $emp_inp line
  gets $emp_inp line
  set line [regsub -all " +" $line " "]
  set line [string trim $line]
  set line [split $line]
  set Ainp [lindex $line 0]
  set Zinp [lindex $line 1]
  set ZAinp [expr int($Zinp*1000+$Ainp)]
  set Mass $Ainp
  exec $::env(EMPIREDIR)/scripts/resonance.tcl $ZAinp $mat $Mass &} \
        -label {Resonance Module} 
    $site_3_0.menu93 add separator \
        
    $site_3_0.menu93 add cascade \
        -menu "$site_3_0.menu93.men87" \
        -command {} -label {KALMAN for} 
    set site_4_0 $site_3_0.menu93
    menu $site_4_0.men87 \
        -activebackground #f9f9f9 -activeforeground black -foreground black \
        -tearoff 1 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 1 $mat $EXPDAT} \
        -label {Total MT=1} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 2 $mat $EXPDAT} \
        -label {Elastic MT=2} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 4 $mat $EXPDAT} \
        -label {Inelastic MT=4} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 16 $mat $EXPDAT} \
        -label {(z,2n) MT=16} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 17 $mat $EXPDAT} \
        -label {(z,3n) MT=17} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 18 $mat $EXPDAT} \
        -label {(z,f) MT=18} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 102 $mat $EXPDAT} \
        -label {(n,g) MT=102} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 103 $mat $EXPDAT} \
        -label {(n,p) MT=103} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 107 $mat $EXPDAT} \
        -label {(n,a) MT=107} 
    $site_4_0.men87 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/kalman  $file 0 $mat $EXPDAT} \
        -label {all MTs} 
    $site_3_0.menu93 add cascade \
        -menu "$site_3_0.menu93.men89" \
        -command {} -label {Plot Sensitivity Matrix for} 
    set site_4_0 $site_3_0.menu93
    menu $site_4_0.men89 \
        -activebackground #f9f9f9 -activeforeground black -foreground black \
        -tearoff 1 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 1 $mat $EXPDAT} \
        -label {Total MT=1} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 2 $mat $EXPDAT} \
        -label {Elastic MT=2} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 3 $mat $EXPDAT} \
        -label {Nonelastic MT=3} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 4 $mat $EXPDAT} \
        -label {Inelastic MT=4} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 11 $mat $EXPDAT} \
        -label {(z,2nd) MT=11} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 16 $mat $EXPDAT} \
        -label {(z,2n) MT=16} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 18 $mat $EXPDAT} \
        -label {Fission MT=18} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 22 $mat $EXPDAT} \
        -label {(z,na) MT=22} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 24 $mat $EXPDAT} \
        -label {(z,2na) MT=24} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 45 $mat $EXPDAT} \
        -label {(z,npa) MT=45} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 102 $mat $EXPDAT} \
        -label {(z,gamma) MT=102} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 103 $mat $EXPDAT} \
        -label {(z,p) MT=103} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 104 $mat $EXPDAT} \
        -label {(z,d) MT=104} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 105 $mat $EXPDAT} \
        -label {(z,t) MT=105} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 107 $mat $EXPDAT} \
        -label {(z,a) MT=107} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 112 $mat $EXPDAT} \
        -label {(z,pa) MT=112} 
    $site_4_0.men89 add command \
        \
        -command {exec  xterm -e $::env(EMPIREDIR)/scripts/zvvsenmat  $file 115 $mat $EXPDAT} \
        -label {(z,pd) MT=115} 

    $site_3_0.menu93 add command \
        \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/mergeMF33 $file 
exec xterm -e mv $file-m.endf $file.endf
exec  xterm -e $::env(EMPIREDIR)/scripts/stanef $file & } \
        -label {Insert covariances} 
    $site_3_0.menu93 add separator \

    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/sampling.sh $file &} \
        -label {Monte Carlo sampling (repeated calls to runE} 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/calc_cov.sh $file &} \
        -label {Processing Monte Carlo samples} 
    $site_3_0.menu93 add separator \
       
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/stanef $file &} \
        -label STANEF 
    $site_3_0.menu93 add separator \
        
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/runjoy $file &} \
        -label NJOY 
    $site_3_0.menu93 add separator \
        
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/c4 $file &} \
        -label X4TOC4 
    $site_3_0.menu93 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/sortc4 $file &} \
        -label SORTC4 
    $top.m88 add cascade \
        -menu "$top.m88.menu94" -command {} -label Outputs 
    set site_3_0 $top.m88
    menu $site_3_0.menu94 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $site_3_0.menu94 add command \
        -command { editFile $file.lst } -label {EMPIRE full} 
    $site_3_0.menu94 add command \
        -command { editFile $file.out } -label {EMPIRE short} 
    $site_3_0.menu94 add command \
        -command { editFile $file.xsc } -label Cross-sections 
    $site_3_0.menu94 add command \
        -command { editFile $file-fiss.xsc } -label {Fission chances} 
    $site_3_0.menu94 add command \
        -command { editFile $file-pfnm.out } \
        -label {Fiss. neutr. multiplicities (nubar) } 
    $site_3_0.menu94 add command \
        -command { editFile $file-pfns.out } -label {Fiss. neutr. spectra (PFNS)} 
    $site_3_0.menu94 add command \
        -command { editFile $file.sys } -label {x-sec systematics} 
    $site_3_0.menu94 add command \
        -command { editFile $file-ompfit.lst } -label {OMP fit output} 
    $site_3_0.menu94 add separator \
        
    $site_3_0.menu94 add command \
        -command { editFile $file-ecis.out } -label {ECIS } 
    $site_3_0.menu94 add separator \

    $site_3_0.menu94 add command \
        -command { editFile $file-optman.out } -label {OPTMAN } 
    $site_3_0.menu94 add separator \
        
    $site_3_0.menu94 add command \
        -command { editFile $file.exf } -label EXFOR 
    $site_3_0.menu94 add command \
        -command { editFile $file.c4 } -label {C4 file} 
    $site_3_0.menu94 add separator \
        
    $site_3_0.menu94 add command \
        -command { editFile $file.endf } -label {ENDF final} 
    $site_3_0.menu94 add command \
        -command { editFile $file-s.endf } -label {ENDF plotted} 
    $site_3_0.menu94 add command \
        -command { editFile $file-e.endf } -label {ENDF empend} 
    $site_3_0.menu94 add separator \
        
    $site_3_0.menu94 add command \
        -command { editFile $file-mat.sen } -label {Sensitivity matrix} 
    $site_3_0.menu94 add command \
        -command { editFile $file-out.kal } -label {KALMAN output} 
    $site_3_0.menu94 add command \
        -command { editFile $file-par.kal } -label {KALMAN adj. parameters} 
    $site_3_0.menu94 add command \
        -command { editFile $file-xsc.kal } -label {KALMAN x-sections} 
    $site_3_0.menu94 add command \
        -command { editFile $file-cov.kal } -label {Covariance matrices} 
    $site_3_0.menu94 add command \
        -command {exec gnuplot $::env(EMPIREDIR)/util/kalman/corr.plt &} \
        -label {Covariance plot} 
    $site_3_0.menu94 add separator \
        
    $site_3_0.menu94 add command \
        -command { editFile $file-njoy.out } -label {NJOY output} 
    $site_3_0.menu94 add command \
        -command { editFile $file.ace } -label {NJOY/ACER file} 
    $site_3_0.menu94 add command \
        -command { pspdfView $file-njoy.ps } -label {NJOY plots} 
    $site_3_0.menu94 add command \
        -command { pspdfView $file-acer.ps } -label {NJOY/ACER plots} 
    $top.m88 add cascade \
        -menu "$top.m88.men70" -command {} -label Logs 
    set site_3_0 $top.m88
    menu $site_3_0.men70 \
        -tearoff 0 
    $site_3_0.men70 add command \
        -command { RepWriter $file.lyx } -font {} -label Report 
    $site_3_0.men70 add command \
        \
        -command {exec xterm -bg darkorange -title WARNINGS -e less $file.war &} \
        -font {} -label {EMPIRE warnings} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.empend } -font {} -label {EMPEND Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.endres } -font {} -label {ENDRES Log} 
    $site_3_0.men70 add separator \
        
    $site_3_0.men70 add command \
        -command { editFile $file.x42c4_lst } -font {} -label {X4TOC4 Log} 
    $site_3_0.men70 add command \
        -command { editFile $file.x42c4_errs } -font {} \
        -label {X4TOC4 errors} 
    $site_3_0.men70 add separator \
        
    $site_3_0.men70 add command \
        -command { editFile $file-log.checkr } -font {} -label {CHECKR Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.fizcon } -font {} -label {FIZCON Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.psyche } -font {} -label {PSYCHE Log} 
    $site_3_0.men70 add separator \
        
    $site_3_0.men70 add command \
        -command { editFile $file-log.fixup } -font {} -label {FIXUP Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.fixup2 } -font {} -label {FIXUP-2 Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.linear } -font {} -label {LINEAR Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.recent } -font {} -label {RECENT Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.sigma1 } -font {} -label {SIGMA1 Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.legend } -font {} -label {LEGEND Log} 
    $site_3_0.men70 add command \
        -command { editFile $file-log.plotc4 } -font {} -label {PLOTC4 Log} 
    $top.m88 add cascade \
        -menu "$top.m88.menu95" -command {} -label Plots 
    set site_3_0 $top.m88
    menu $site_3_0.menu95 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $site_3_0.menu95 add command \
        -command { pspdfView $file.ps } -label {PLOTC4 plots} 
    $site_3_0.menu95 add separator \
        
    $site_3_0.menu95 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/zvpl $file &} \
        -label {Create ZVV plot} 
    $site_3_0.menu95 add command \
        -command {exec xterm -e $::env(EMPIREDIR)/scripts/zvcomb &} \
        -label {Merge ZVV plots} 
    $site_3_0.menu95 add command \
        -command {exec $::env(EMPIREDIR)/scripts/guizvv.tcl $file &} \
        -label {Compare ZVV} 
    $site_3_0.menu95 add separator \
        
    $site_3_0.menu95 add command \
        -command { pspdfView $file-cum.ps } -label {Cumul. levels} 
    $top.m88 add cascade \
        -menu "$top.m88.men77" -command {} -label Clean 
    set site_3_0 $top.m88
    menu $site_3_0.men77 \
        -disabledforeground #a1a4a1 -tearoff 0 
    $site_3_0.men77 add command \
        -command {exec $::env(EMPIREDIR)/scripts/clean $file &} \
        -label {Clean project} 
    $site_3_0.men77 add command \
        \
        -command {exec $::env(EMPIREDIR)/scripts/clean $file
exec rm -f $file.inp} \
        -label {Delete project} 
    $top.m88 add cascade \
        -menu "$top.m88.men78" -command {} -label Source 
    set site_3_0 $top.m88
    menu $site_3_0.men78 \
        -disabledforeground #a1a4a1 -tearoff 0 
    $site_3_0.men78 add command \
        -command { editFile $::env(EMPIREDIR)/source/dimension.h } \
        -label Dimensions 
    $site_3_0.men78 add command \
        \
        -command {cd $::env(EMPIREDIR)/source
exec xterm -e make &
cd $workdir} \
        -label Compile 
    $top.m88 add separator \
        
    $top.m88 add cascade \
        -menu "$top.m88.menu96" -command {} -label Help 
    set site_3_0 $top.m88
    menu $site_3_0.menu96 \
        -activebackground #dcdcdc -activeforeground #000000 \
        -background #dcdcdc -foreground #000000 -tearoff 0 
    $site_3_0.menu96 add command \
        -command { pspdfView $::env(EMPIREDIR)/doc/inplist.pdf } \
        -label {EMPIRE input} 
    $site_3_0.menu96 add command \
        \
        -command { editFile $::env(EMPIREDIR)/RIPL/optical/om-data/om-index-by-Z.txt} \
        -label {RIPL omp} 
    $site_3_0.menu96 add command \
        \
        -command { editFile $::env(EMPIREDIR)/RIPL/optical/om-data/om-references.txt} \
        -label {RIPL omp Refs.} 
    $site_3_0.menu96 add command \
        -command { editFile $::env(EMPIREDIR)/doc/hints.txt } -label FAQ 
    $site_3_0.menu96 add command \
        -command { pspdfView $::env(EMPIREDIR)/doc/empire.pdf } \
        -label {EMPIRE manual} 
    $site_3_0.menu96 add command \
        -command { editFile $::env(EMPIREDIR)/util/empend/manual.txt } \
        -label {EMPEND manual} 
    $site_3_0.menu96 add command \
        -command { editFile $::env(EMPIREDIR)/util/c4sort/manual.txt } \
        -label {C4SORT manual} 
    $site_3_0.menu96 add command \
        -command { editFile $::env(EMPIREDIR)/util/x4toc4/manual.txt } \
        -label {X4TOC4 manual} 
    $site_3_0.menu96 add command \
        -command { editFile $::env(EMPIREDIR)/util/fixup/manual.txt } \
        -label {FIXUP manual} 
    menu $top.m76 \
        -disabledforeground #a1a4a1 -tearoff 1 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $top.fra77 \
        -in $top -anchor center -expand 0 -fill x -side top 
    pack $top.tab88 \
        -in $top -anchor center -expand 1 -fill both -side top 

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
Window show .top75

main $argc $argv
