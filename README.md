# Directions for Building Grace.app

**Step 1:** Download the latest JUCE, sndlib, and grace sources into a common parent directory:
```bash
$ cd parent/
$ git clone https://github.com/WeAreROLI/JUCE.git juce
$ curl ftp://ccrma-ftp.stanford.edu/pub/Lisp/sndlib.tar.gz | tar -zx
$ git clone https://github.com/ricktaube/grace.git grace
```

**Step 2:** Cd into the sndlib directory and make the static sndlib library (requires premake4). 

MacOS:
```bash
$ cd sndlib
$ premake4 --with-g++
$ make
```

Linux:
```bash
$ cd sndlib
$ premake4 --with-g++ --with-jack
$ make
```

Windows (after premake open the sndlib.sln in VisualStudio to make the lib):
```bash
$ cd sndlib
$ premake4 --with-g++
```

If you are successful the result of this process will be a static sndlib library saved in the
sndlib/lib folder,  e.g. sndlib/lib/libsndlib.a.


**Step 3:** Cd into the grace/Builds directory and use one of the premade projects to build
Grace.app.  Projects for Xcode, VisualStudio2019 and Linux are already provided. To 
add a different build or customize an existing one, open Grace.projucer in JUCE's 
Projucer.app and generate the new project.
