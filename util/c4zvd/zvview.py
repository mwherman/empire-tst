#! /usr/bin/env python
import argparse, subprocess, os, copy

# Set up the command line parser
parser = argparse.ArgumentParser(description='Plot a zvd file using zvview')
parser.add_argument( 'zvd', metavar='zvd', type=str, nargs='+', help='zvd file(s) to plot' )
#parser.add_argument( )
args = parser.parse_args()

myEnv = copy.copy( os.environ )

def X_is_running():
    # This is a simple test to get the X server properties.  On the newer Macs, which launch
    # X11 whenever libX11.so is accessed, this will trigger the X11.app if it is off.
    p = subprocess.Popen(["xset", "-q"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p.communicate()
    return p.returncode == 0
    
if __name__ == "__main__":
    if os.uname()[0] == "Linux":    zvviewExe = "./zvv2-1.005-lin.exe"
    elif os.uname()[0] == "Darwin": zvviewExe = "./zvv2-1.005-mac.exe"
    else:                           zvviewExe = "./zvv2-1.005-win.exe"
                                    
    if os.uname()[0] in [ "Darwin", "Linux" ]:
        if not X_is_running(): raise RuntimeError( "X11 is not running, please turn it on" ) 
        if os.uname()[0] == 'Darwin': myEnv.update( {'DISPLAY':':0.0'} )
    
    subprocess.call( [ zvviewExe ] + args.zvd, env=myEnv )
