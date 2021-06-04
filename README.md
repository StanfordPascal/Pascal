# Pascal

New Stanford Pascal Compiler

This is the new Stanford Pascal Compiler.
It runs on Windows, OS/2, Linux, MacOs and
probably on every other system that has a C Compiler,
because the generated P-Code is interpreted by the
P-Code interpreter PCINT.

It also runs on the IBM-Mainframe, on the operating systems
MVS and CMS (on Hercules) and on today's z/OS, too, although
limited to AMODE 24, at the moment. The P-Code is translated
to 370 machine code, there.

For more information, see the Stanford Pascal compiler website:
http://bernd-oppolzer.de/job9.htm
or the New Stanford Pascal compiler Facebook page:
https://www.facebook.com/StanfordPascal/

--------------------------------------------------------------

Windows users:

There will be installation scripts and helpfiles in the near future.

For the moment:

The P-Code interpreter PCINT is located in the bin subdirectory.
The compiler files (Pascal and P-Code) are in the src subdirectory.
The message repository is in the etc subdirectory.

If you combine these directories, you have all you need to run
the compiler. PASCAL.CMD in subdirectory script will run the
compiler, and PRUN.CMD in the same subdirectory will run your
compiled P-Code files.

--------------------------------------------------------------

Unix and Linux (and other systems) users:

You will have to build the P-Code interpreter PCINT from the
source code first. The PCINT source code and all related files
are in the bin subdirectory. There is no makefile; simply compile
all the sources (.c, .h) in the bin subdirectory; the executable
should be named pcint.

After that, use the same subdirectories as the Windows users.
Script pp in subdirectory script_ix shows how to call the compiler,
and script prun calls the compiled P-Code files.

Have fun :-)

