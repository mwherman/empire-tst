import os, argparse, tempfile, shutil

PACKAGEDIR = os.path.dirname( os.path.abspath( __file__ ) )
PACKAGENAMEROOT = 'Empire'
EXCLUDEFILE = PACKAGEDIR + os.sep + 'exclude.txt'

# Set up the command line parser
parser = argparse.ArgumentParser(description='Make a tarball of the ' + PACKAGENAMEROOT + ' package' )
parser.add_argument( '--verbose',  dest='verbose',  default=False, action='store_true', help='Turn on verbose output' )
parser.add_argument( '--release',  dest='release',  default=False, action='store_true', help='Make release tarball' )
parser.add_argument( '--latest',   dest='latest',   default=False, action='store_true', help='Make tarball of latest update (stable or not)' )
parser.add_argument( '--full',     dest='full',     default=True,  action='store_true', help='Include everything in the tarball (default=True)' )
parser.add_argument( '--noRIPL',   dest='noRIPL',   default=False, action='store_true', help='Exclude the RIPL directory from the final tarball' )
parser.add_argument( '--docOnly',  dest='docOnly',  default=False, action='store_true', help='Include only the documentation in the final tarball' )
parser.add_argument( '--riplOnly', dest='riplOnly', default=False, action='store_true', help='Include only the RIPL directory in the final tarball' )



if __name__ == '__main__':

    args = parser.parse_args()
    
    if args.verbose: print "Working directory:", PACKAGEDIR

    # Packaging area   
    tmpDir = tempfile.mkdtemp()
    if args.verbose: print "Make temporary directory:",tmpDir
    
    # Compute base file name
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
        packageName += '-svnRevision' + revision
        
    # Compute additional modifiers
    if args.docOnly: packageName += '-documentation'
    elif args.riplOnly: packageName += '-onlyRIPL'
    elif args.noRIPL: packageName += '-noRIPL'
    
    # Set up exclude list
    stuffToExclude = [ x.rstrip() for x in open( EXCLUDEFILE ).readlines() ]
    itemsToExclude = filter( lambda x: os.sep in x, stuffToExclude )
    patternsToExclude = filter( lambda x: os.sep not in x, stuffToExclude )
    if args.noRIPL: stuffToExclude += [ 'RIPL' ]
    def ignoreTheseGuys( adir, filenames ):
        items = set()
        for f in filenames:
            testItem = adir+os.sep+f
            for i in itemsToExclude: 
                if testItem.endswith( i ): items.add( f )
        return items | shutil.ignore_patterns( *stuffToExclude )( adir, filenames )
    
    # Compute the source directory
    sourceDir = PACKAGEDIR
    if args.docOnly: pass
    elif args.riplOnly: pass
    
    # Copy everything to packaging area
    if args.verbose: print "Copy distribution to", tmpDir
    if args.verbose: print "    ignoring files that match these patterns:", patternsToExclude
    if args.verbose: print "    ignoring files that match these items:", itemsToExclude
    if args.verbose: print "    ignoring files that match these thingies:", stuffToExclude
    
    shutil.copytree( sourceDir, tmpDir + os.sep + packageName, ignore = ignoreTheseGuys )

    # Make the package
    if args.verbose: print "Create archive"
    shutil.make_archive( PACKAGEDIR + os.sep + packageName, format = 'gztar', root_dir = tmpDir, base_dir = packageName, verbose = args.verbose )
    
    # Clean up
    if args.verbose: print "Delete temporary directory:",tmpDir
    shutil.rmtree( tmpDir )
    
