# Directions for building Grace.app

1. Download the latest JUCE, sndlib, and grace sources into a common parent directory:

    $ git clone https://github.com/WeAreROLI/JUCE.git juce
    $ curl ftp://ccrma-ftp.stanford.edu/pub/Lisp/sndlib.tar.gz | tar -zx
    $ git clone https://github.com/ricktaube/grace.git grace

2. Cd into the sndlib directory and make the static sndlib library (requires premake4). 
MacOS:

    $ cd sndlib
    $ premake4 --with-g++
    $ make
Linux:

    $ cd sndlib
    $ premake4 --with-g++ --with-jack
    $ make
    
 Windows (after premake open the sndlib.sln in VisualStudio to make the lib):
    $ cd sndlib
    $ premake4 --with-g++

If you are successful the result of this process will be a static sndlib library saved in the
sndlib/lib folder,  e.g. sndlib/lib/libsndlib.a.

3. Cd into the grace/Builds directory and open one of the premade projects to compile
Grace.app.  Projects for Xcode, VisualStudio2019 and Linux are already provided. To 
add a different build or customize an existing one, open Grace.projucer in JUCE's 
Projucer.app and generate the new project.


