import os, argparse, tempfile, shutil, glob

PACKAGEDIR = os.path.dirname( os.path.abspath( __file__ ) )
PACKAGENAMEROOT = 'EMPIRE'
EXCLUDEFILE = PACKAGEDIR + os.sep + 'exclude.txt'
AVAILABLEFORMATS = [ "zip", "tar", "bztar", "gztar" ]

# Set up the command line parser
parser = argparse.ArgumentParser(description='Make a tarball of the ' + PACKAGENAMEROOT + ' package' )
parser.add_argument( '--verbose',  dest='verbose',  default=False, action='store_true', help='Turn on verbose output' )
parser.add_argument( '--release',  dest='release',  default=False, action='store_true', help='Make tarball with release naming convention' )
parser.add_argument( '--latest',   dest='latest',   default=False, action='store_true', help='Make tarball with latest svn update naming convention' )
parser.add_argument( '--full',     dest='full',     default=False, action='store_true', help='Include everything in the tarball (default=False)' )
parser.add_argument( '--noRIPL',   dest='noRIPL',   default=True,  action='store_true', help='Exclude (all but one set from) the RIPL/densities/total/level-densities-hfb directory from the archive' )
parser.add_argument( '--docOnly',  dest='docOnly',  default=False, action='store_true', help='Include only the documentation in the final tarball' )
parser.add_argument( '--riplOnly', dest='riplOnly', default=False, action='store_true', help='Archive only the RIPL/densities/total/level-densities-hfb directory' )
parser.add_argument( '--format',   dest='format',   default='gztar', type=str, choices = AVAILABLEFORMATS, help='Set the archive format (default: gztar)' )
parser.add_argument( '--Z',        dest='Z',        default=13,      type=int,          help='Set the Z to save from RIPL/densities/total/level-densities-hfb directory from the archive (default=13)' )



if __name__ == '__main__':

    args = parser.parse_args()
    
    if args.verbose: print "Working directory:", PACKAGEDIR

    # Packaging area   
    tmpDir = tempfile.mkdtemp()
    if args.verbose: print "Make temporary directory:",tmpDir
    
    # Compute base file name for the tarball and the package directory
    packageName = PACKAGENAMEROOT
    if args.release:
        for line in open( PACKAGEDIR + os.sep + 'version' ).readlines(): 
            if line.startswith( 'VERSIONNUMBER' ): VERSIONNUMBER = line.split( )[-1]
            if line.startswith( 'VERSIONNAME' ): VERSIONNAME = line.split( )[-1]
        packageName = '-'.join( [ packageName, VERSIONNUMBER, VERSIONNAME ] )
    elif args.latest: 
        import subprocess 
        for line in subprocess.check_output( [ 'svn', 'info' ] ).split('\n'):
            if line.startswith( 'Revision:' ): 
                revision = line.split( )[-1]
                break
        packageName += '-latest' #'-r' + revision
        
    # Compute additional modifiers and the package directory name (the tarball unpacks to this directory)
    if args.docOnly: 
        packageName += '-documentation'
        packageDirName += '-documentation'
    elif args.riplOnly: 
        packageName = 'level-densities-hfb'
        packageDirName = packageName
    elif args.full: 
        packageDirName = packageName
        packageName += '-fullRIPL'
    else:
        packageDirName = packageName
    
    # Set up exclude list (this defines what eventually ends up in the archive in most cases)
    stuffToExclude = [ x.rstrip() for x in open( EXCLUDEFILE ).readlines() ]
    itemsToExclude = filter( lambda x: os.sep in x, stuffToExclude )
    patternsToExclude = filter( lambda x: os.sep not in x, stuffToExclude )
    allRIPLHFBDensities = glob.glob( 'RIPL/densities/total/level-densities-hfb/*' )
    selectRIPLHFBDensities = glob.glob( 'RIPL/densities/total/level-densities-hfb/z'+str(args.Z).zfill(3)+'*' )
#    print selectRIPLHFBDensities
#    print args.Z, 
#    print allRIPLHFBDensities
    if args.noRIPL: itemsToExclude += filter( lambda x: x not in selectRIPLHFBDensities, allRIPLHFBDensities )
    if args.riplOnly: itemsToExclude = []
    def ignoreTheseGuys( adir, filenames ):
        items = set()
        for f in filenames:
            testItem = adir+os.sep+f
            for i in itemsToExclude: 
                if testItem.endswith( i ): items.add( f )
        return items | shutil.ignore_patterns( *patternsToExclude )( adir, filenames )
    
    # Compute the source directory
    sourceDir = PACKAGEDIR
    if args.docOnly: sourceDir = PACKAGEDIR + os.sep + "doc"
    elif args.riplOnly: sourceDir = PACKAGEDIR + os.sep + "RIPL/densities/total/level-densities-hfb"
    
    # Copy everything to packaging area
    if args.verbose: print "Copy distribution to", tmpDir
    if args.verbose: print "    ignoring files that match these patterns:", patternsToExclude
    if args.verbose: print "    ignoring files that match these items:", itemsToExclude
    shutil.copytree( sourceDir, tmpDir + os.sep + packageDirName, ignore = ignoreTheseGuys )

    # Make the package
    if args.verbose: print "Create archive"
    shutil.make_archive( PACKAGEDIR + os.sep + packageName, format = args.format, root_dir = tmpDir, base_dir = packageDirName, verbose = args.verbose )
    if args.format == 'gztar': 
        shutil.move( PACKAGEDIR + os.sep + packageName + '.tar.gz', PACKAGEDIR + os.sep + packageName + '.tgz' )
    
    # Clean up
    if args.verbose: print "Delete temporary directory:",tmpDir
    shutil.rmtree( tmpDir )
    
