#!/usr/bin/perl

$mat=@ARGV[0];
if($mat eq ''){
    $mat = '1111';
}

$mt=@ARGV[1];
if($mt eq ''){
    $mt = '102';
}

open(KALEND,"../util/kalman/kalend |");

$cnt =    1;
while(<KALEND>){
    chop;
    if(/^$/){
        $cnt=1;
        $mt++;
        printf("                                                                  %4d33  099999\n",$mat);
        next;
    }
    s/E\+0/\+/g;
    s/E\-0/\-/g;
    printf("%-66s%4d%2d%3d%5d\n",$_,$mat,33,$mt,$cnt);
    $cnt++;
}

close(KALEND);
