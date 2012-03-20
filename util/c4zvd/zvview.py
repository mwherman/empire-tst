#! /usr/bin/env python
import argparse, subprocess, os, copy


# Set up the command line parser
parser = argparse.ArgumentParser( description='Plot a zvd file using ZVView' )
parser.add_argument( 'zvd', metavar='zvd', type=str, nargs='+', help='The zvd file(s) to plot or other ZVView command line arguments' )
parser.add_argument( '-p', dest='pflag', default=None, help='Whatever ZVView takes as an argument for the -p flag' )
args = parser.parse_args()


# Set up ZVView's run-time environment
zvviewEnv = copy.copy( os.environ )
if not 'EMPIREDIR' in zvviewEnv: zvviewEnv[ 'EMPIREDIR' ] = os.getcwd()


def X_is_running():
    ''' 
    This is a simple test to get the X server properties.  On the newer Macs, which launch
    X11 whenever libX11.so is accessed, this will trigger the X11.app if it is off.
    '''
    p = subprocess.Popen(["xset", "-q"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p.communicate()
    return p.returncode == 0
    
    
    
if __name__ == "__main__":
    
    # Get correct version of ZVView for this machine
    if os.uname()[0] == "Linux":    zvviewExe = os.sep.join( [ zvviewEnv[ 'EMPIREDIR' ], 'util', 'c4zvd', "zvv2-1.005-lin.exe" ] )
    elif os.uname()[0] == "Darwin": zvviewExe = os.sep.join( [ zvviewEnv[ 'EMPIREDIR' ], 'util', 'c4zvd', "zvv2-1.005-mac.exe" ] )
    else:                           zvviewExe = os.sep.join( [ zvviewEnv[ 'EMPIREDIR' ], 'util', 'c4zvd', "zvv2-1.005-win.exe" ] )
                                    
    # Make any adjustments to the run-time environment & make sure X11 is running
    if os.uname()[0] in [ "Darwin", "Linux" ]:
        if not X_is_running(): raise RuntimeError( "X11 is not running, please turn it on" ) 
        if os.uname()[0] == 'Darwin': zvviewEnv.update( {'DISPLAY':':0.0'} )
    
    # Compute the command used to invoke ZVView
    if args.pflag != None: commandList = [ zvviewExe, '-p'+args.pflag ] + args.zvd
    else: commandList = [ zvviewExe ] + args.zvd
    
    # Actually call ZVView
    subprocess.call( commandList, env=zvviewEnv )
