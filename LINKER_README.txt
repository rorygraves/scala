This is a branch from containing 2.11.x.  It contains a bunch of minor optimisations and the test environment for the linker/jar format.
Key items:

The linker (formally known as jarshrink) is looking to show a better jar format for dependency loading (skipping jar exploration and class parsing during (currently done lazily during the typer phase).  
performance/  Test package for the linker tools - see scripts etc inside this pacakge for for information

Actual files are in two places 

src/compiler/tools/linker - the shared files between command line tools and in compiler use
src/linkertool/ The command line utility 'linkertool' for creating archives with linker data embedded.

Todo:

1) Complete the performance package to do a build, apply linkertool to dependencies and run a test compile.
2) Integrate using linker information in classpath loading
2) Add a phase to output linker information later in the compilation (before jar writing).

use 'sbt dist/mkPack dist/mkBin'



