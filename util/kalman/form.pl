#!/usr/bin/perl

$mat=@ARGV[0];
if($mat eq ''){
    $mat = '1111';
}

$mt=@ARGV[1];
if($mt eq ''){
    $mt = '102';
}

#$mat = 6425; # Gd152
#$mat = 6428; # Gd153
#$mat = 6431; # Gd154
#$mat = 6434; # Gd155
#$mat = 6437; # Gd156
#$mat = 6440; # Gd157
#$mat = 6443; # Gd158
#$mat = 6449; # Gd160

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
