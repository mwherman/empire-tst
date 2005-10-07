#!/usr/bin/perl

$zid=@ARGV[0];

if($zid eq ''){
    $zid = 'gd158';
}
$filexsc="$zid".'.xsc';
$fileinp="$zid".'-inp.sen';
$filemat="$zid".'-mat.sen';

@reaction=(' Total     ',' Elastic   ',' Fission   ',' (z,gamma) ',' (z,n)     ',
           ' (z,2n)    ',' (z,3n)    ',' (z,p)     ',' (z,np)    ',' (z,2np)   ',
           ' (z,3np)   ',' (z,a)     ',' (z,na)    ',' (z,2na)   ',' (z,3na)   ',
           ' (z,pa)    ',' (z,npa)   ',' (z,2npa)  ',' (z,3npa)  ');

open(FORT50,'> fort.50') || die 'output file cannot open';
open(FORT52,'> fort.52') || die 'output file cannot open';


##########################################################
#
#  indices
#
#     $i for energy    0 - $n
#     $j for reaction, 0 - $#reaction-1
#     $k for parameter 0 - $m
#
##########################################################



##########################################################
#  generate cross section file - unit 50
##########################################################

open(XFILE,$filexsc) || die 'cross section data file not found';
$i=0;
while(<XFILE>){
    if(/^\#/){
        next;
    }
    split;
    $e[$i] = $_[0];
    for($j=1;$j<=$#reaction;$j++){
        $csec[$j-1][$i] = $_[$j];
    }
    $i++;
}
$n=$i;
close(XFILE);

select(FORT50);

for($j=0;$j<=$#reaction;$j++){
    printf("%-11s%37d\n",$reaction[$j],$n);

    for($i=0;$i<$n;$i++){
        printf("% 11.4e% 11.4e",$e[$i],$csec[$j][$i]);
        if(($i-2)%3==0){
            print "\n";
        }
    }
    if($n%3!=0){
        print "\n";
    }
}
close(FORT50);



##########################################################
#  generate sensitivity file - unit 52
##########################################################

open(XFILE,$fileinp) || die 'parameter file not found';
$k=0;
while(<XFILE>){
    if(/^\#/){
        next;
    }
    split;
    $pn = $_[0];
    $pz = $_[2];
    $pa = $_[3];
    $pp = $_[4];

    $purtb[$k] = $_[1];
    if($pz ne '!'){
        if($pp ne '!'){
            $pname[$k] = $pn.$pa.$pp;
        }else{
            $pname[$k] = $pn.$pa;
        }
    }else{
        $pname[$k] = $pn;
    }
    $k++;
}
$m=$k;
close(XFILE);


open(XFILE,$filemat) || die 'sensitivity file not found';

select(FORT52);
#select(STDOUT);

#########################
#  Header Part

#  Parameter names
printf("%30d\n",$m);
for($k=0;$k<$m;$k++){
    printf("%-12s",$pname[$k]);
    if(($k-10)%11==0){
        print "\n";
    }
}
if($m%11!=0){
    print "\n";
}

#  Parameter values
for($k=0;$k<$m;$k++){
    printf("% 12.5e",1.0);
    if(($k-10)%11==0){
        print "\n";
    }
}
if($m%11!=0){
    print "\n";
}

#  Parameter uncertainties
for($k=0;$k<$m;$k++){
#   printf("%12.5e",$purtb[$k]);
    printf("%12.5e",0.1);
    if(($k-10)%11==0){
        print "\n";
    }
}
if($m%11!=0){
    print "\n";
}

#  Parameter correlation
for($k1=0;$k1<$m;$k1++){
    for($k2=0;$k2<$m;$k2++){
        if($k1==$k2){
            printf("%6.3f",1.0);
        }else{
            printf("%6.3f",0.0);
        }
        if(($k2-19)%20==0){
            print "\n";
        }
    }
    if($m%20!=0){
        print "\n";
    }
}

#########################
#  Main Part

for($j=0;$j<=$#reaction;$j++){

    seek(XFILE,0,0);               # rewind file

    for($k=0;$k<$m;$k++){
        <XFILE>;<XFILE>;<XFILE>;   # skip 3 lines

        for($i=0;$i<$n;$i++){
            $_=<XFILE>;
            @item = split;
            $sens[$i][$k] = $item[$j+1] * $csec[$j][$i] / (2*$purtb[$k]);
        }
        <XFILE>;                   # skip one line
    }

    printf("%-11s%19d\n",$reaction[$j],$n);

    for($i=0;$i<$n;$i++){
        for($k=0;$k<$m;$k++){
            printf("% 12.5e",$sens[$i][$k]);
            if(($k-10)%11==0){
                print "\n";
            }
        }
        if($m%11!=0){
            print "\n";
        }
    }
}
close(XFILE);
close(FORT52);




1;
