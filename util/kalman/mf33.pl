#!/usr/bin/perl


$MFsearch = 33;

if(@ARGV[0] eq ''){ $MTsearch = 1;       }
else{               $MTsearch = @ARGV[0];}

$s = 0;
if($MTsearch < 0){
    $s = 1;
    $MTsearch *= -1;
}


$finddata = 0;

while(<STDIN>){
   $line      = $_; 
   $MATnumber = substr($line,66, 4);
   $MFnumber  = substr($line,70, 2);
   $MTnumber  = substr($line,72, 3);
   if(($MFnumber == $MFsearch) && ($MTnumber == $MTsearch )){last;}
}

for( $i = 0; $i < 2; $i++ ){ 
   $line=<STDIN>;
}
&extract($line);
$npoint = $data[4];
$ne     = $data[5];
$nline  = int($npoint/6)+1; if($npoint%6 == 0){$nline--;}

for( $j = 0; $j < $nline; $j++ ){
    $line = <STDIN>; 
    &extract($line);
    for( $n = 0; $n < 6; $n++){
         if($data[$n] ne '           '){
             $buff[$k++] = &padexp($data[$n]);
         }
     }
}

$k=0;
for( $i = 0; $i < $ne; $i++ ){
    $ene[$i] = $buff[$k++];
}

$n=0;
for( $i = 0; $i < $ne-1; $i++ ){
    for( $j = $i; $j < $ne-1; $j++ ){
        $c = $buff[$k++];
        $d[$n++] = $c;
        if($i==$j){
            $err[$i] = sqrt($c);
        }
    }
    $d[$n++] = $c;
}
$d[$n++] = $c;


$finemesh=2;

if($s){
    for( $i = 1; $i < $ne-1; $i++ ){
        $ei  = $ene[$i]*1e-6;
        $dei = (($ene[$i+1]-$ene[$i])*1e-6)/$finemesh;
        for( $ip = 0; $ip < $finemesh; $ip++ ){
            for( $j = 1; $j < $ne-1; $j++ ){
                $ej = $ene[$j]*1e-6;
                $dej = (($ene[$j+1]-$ene[$j])*1e-6)/$finemesh;
                if($i>$j){
                    $k = $i-$j + $j*$ne - $j*($j-1)/2;
                }else{
                    $k =    $j + $i*$ne - $i*($i+1)/2;
                }
                for( $jp = 0; $jp < $finemesh; $jp++ ){
                    printf(" % 11.4e % 11.4e % 11.4e\n",
                           $ei+$ip*$dei,$ej+$jp*$dej,$d[$k]/$err[$j]/$err[$i]);
                }
            }
            print "\n";
        }
    }

}else{
    for( $i = 0; $i < $ne; $i++ ){
        printf(" % 11.4e % 11.4e\n",$ene[$i]*1e-6,$err[$i]);
    }
}

sub padexp{
   $str = $_[0];
   if($str ne '           '){
       if(index($str,'E') < 0){
           $s1 = substr($str,8,1);
           $s2 = substr($str,9,1);
           if($s1 eq '+' || $s1 eq '-'){
                $num[0]  = substr($str, 0,8);
                $num[1]  = 'E';
                $num[2]  = $s1;
                $num[3]  = substr($str, 9,1); if($num[3] eq ' '){$num[3] = '0';}
                $num[4]  = substr($str,10,1);
                $str=join('',@num);
           }
           elsif($s2 eq '+' || $s2 eq '-'){
                $num[0]  = substr($str, 0,9);
                $num[1]  = 'E';
                $num[2]  = $s2;
                $num[3]  = substr($str,10,1);
                $num[4]  = '';
                $str=join('',@num);
           }
       }
   }
   $str;
}


sub extract{
   $str = $_[0];
   $i=0;
   $data[$i++] = substr($str, 0,11);
   $data[$i++] = substr($str,11,11);
   $data[$i++] = substr($str,22,11);
   $data[$i++] = substr($str,33,11);
   $data[$i++] = substr($str,44,11);
   $data[$i++] = substr($str,55,11);
}
