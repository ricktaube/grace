# Directions for building Grace

1. Download the latest JUCE sources into the parent directory of the grace directory:

       $ git clone https://github.com/WeAreROLI/JUCE.git juce
       $ ls -l
       drwxr-xr-x  109 taube  staff   3488 Feb 29 20:24 juce
       drwxr-xr-x  109 taube  staff   3488 Feb 29 20:24 grace


2. Build sndlib as a static library (requires curl and premake4):
       $ Scripts/sndlib.sh

3. Open one of the premade projects in Builds/ and build the app.  Builds for Xcode, VisualStudio2019 
and Linux are currently provided. To make a different build load the Grace.projucer file into the Projucer
to generate the new project.
