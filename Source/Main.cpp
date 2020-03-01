/*
  ==============================================================================

  Copyright 1999-2013 Rick Taube.  All rights reserved.

  Licensed under the "Attribution-NonCommercial-ShareAlike" Vizsage
  Public License, which says that non-commercial users may share and
  modify this code but must give credit and share improvements. For
  complete terms please read the text of the full license available at
  this link: http://vizsage.com/license/Vizsage-License-BY-NC-SA.html

  ==============================================================================
*/

#include "Libraries.h"
#include "Enumerations.h"
#include "CmSupport.h"
#include "Console.h"
#include "Scheme.h"
#include "Midi.h"
#include "Main.h"
#include "Audio.h"
#include "Syntax.h"
#include "CodeEditor.h"
#include "PlotWindow.h"
#include "Skin.h"
#include "Preferences.h"
#include "OpenSoundControl.h"
#include "Csound.h"
#include "Metronome.h"
#ifdef WITH_FOMUS
#include "Fomus.h"
#endif
#include "Help.h"
#include "SndLib.h"

extern Console* globalConsole;

const juce::String Grace::getApplicationName(void)
{
  return "Grace";
}

const juce::String Grace::getApplicationVersion(void)
{
  return SysInfo::getVersionString(SysInfo::CM_VERSION);
}

bool Grace::moreThanOneInstanceAllowed(void)
{
  return SysInfo::isMac();
}

void Grace::anotherInstanceStarted(const juce::String& commandLine)
{
  juce::StringArray droppedfiles;
  // tokenize input with whitespace inside quoted file names preserved
  droppedfiles.addTokens(commandLine, true);
  int size = droppedfiles.size();
  if (size > 0)
  {
    // strip explicit quotes from file names
    for (int i = 0; i < size; i++)
      droppedfiles.set(i, droppedfiles[i].unquoted());
    if (Console::getInstance()->isInterestedInFileDrag(droppedfiles))
      Console::getInstance()->filesDropped(droppedfiles, 0, 0);
  }
}

void Grace::initialise(const juce::String& commandLine)
{
  std::cout << "Starting Grace...\n";
  std::cout << SysInfo::getCMVersion() << " [Config=" << SysInfo::getBuildConfig() << "]\n";
  std::cout << "Creating Windows Skin..." << std::flush;
  juce::LookAndFeel::setDefaultLookAndFeel(lookAndFeel = new Skin());

  // Console must exist before any components attempt to write to it!
  std::cout << "Creating Console..." << std::flush;
  Console::globalInstance = new Console;
  std::cout << "OK!\n";

  // Load preference file.
  std::cout << "Creating Preferences..." << std::flush;
  Preferences::getInstance();
  std::cout << "OK!\n";
  applicationSupportDirectory = Preferences::getInstance()->getProps().getFile().getParentDirectory();
  std::cout << "applicationSupportDirectory=" << applicationSupportDirectory.getFullPathName() << "\n";

  // Initialize Liblo
  std::cout << "Creating OpenSoundControl..." << std::flush;
  OpenSoundControl::getInstance();
  std::cout << "OK!\n";


#ifdef WITH_FOMUS  
  // Initialize fomus
  std::cout << "Creating Fomus..." << std::flush;
  initfomus();
  std::cout << "OK!\n";
#endif  

#ifdef WITH_SDIF
  std::cout << "Creating SDIF..." << std::flush;
  SdifGenInit("");
  std::cout << "OK!\n";
#endif

  // Scheme
  std::cout << "Creating Scheme..." << std::flush;
  SchemeThread* scm = SchemeThread::getInstance();
  scm->setPriority(10);
  scm->startThread();
  std::cout << "OK!\n";

  // Audio Manager
  std::cout << "Creating AudioManager..." << std::flush;
  AudioManager::getInstance();
  std::cout << "OK!\n";

  // MidiOut 
  std::cout << "Creating MidiOutPort..." << std::flush;
  MidiOutPort* mid = MidiOutPort::getInstance();
  mid->setPriority(9);
  mid->startThread();
  std::cout << "OK!\n";

  // FIXME: COMMAND MANAGER DEPENDS ON PREFS CONSOLE AUDIO AND MIDI
  // create application command manager

  std::cout << "Creating MenuBarModel..." << std::flush;
  menuBarModel = new GraceMenuBarModel();
  std::cout << "OK!\n";
  // If we are on the mac then add our App menu bar 
  // create the console window (console may have output in it already)
  std::cout << "Creating ConsoleWindow..." << std::flush;
  ConsoleWindow* win = new ConsoleWindow();
  std::cout << "OK!\n";
  win->getConsole()->toFront(true);

#ifdef JUCE_MAC
  juce::MenuBarModel::setMacMainMenu(menuBarModel);
#endif

  std::cout << "Registering CommandManager Commands..." << std::flush;
  commandManager.registerAllCommandsForTarget(this);
  menuBarModel->setApplicationCommandManagerToWatch(&commandManager);
  std::cout << "OK!\n";




  juce::String str = Preferences::getInstance()->getStringProp("LispInitFile");
  if (str != juce::String())
  {
    std::cout << "Loading LispInitFile..." << std::flush;
    scm->load(juce::File(str),false);
    std::cout << "OK!\n";
  }
}

void Grace::systemRequestedQuit()
{
  int flag = -1; 
  int unsaved = getNumUnsavedWindows();
  if (unsaved > 0)
  {
    juce::String m ("You have ");
    m << unsaved << " document";
    if (unsaved > 1) m << "s";
    m << " with unsaved changes. Do you want to review these changes before quitting?";
    flag = juce::AlertWindow::showYesNoCancelBox(juce::AlertWindow::WarningIcon,
                                                 "Quit Grace", m, "Discard & Quit",
                                                 "Review Changes...", "Cancel");
  }
  //  std::cout << "query return flag=" << flag << "\n";
  // Flag -1: no changed windows, proceed to quit
  // Flag 0: Cancel
  // Flag 1: Discard changes (proceed to quit)
  // Flag 2: Review Changes (and possibly quit)
  if (flag == -1 || flag == 1 || (flag == 2 && queryUnsavedWindows()))
    quit();
}

int Grace::getNumUnsavedWindows()
{
  // return the number of windows that need saving
  int n = 0;
  for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    juce::TopLevelWindow* w = juce::TopLevelWindow::getTopLevelWindow(i);
    if (CodeEditorWindow* e = dynamic_cast<CodeEditorWindow*>(w))
    {
      if (e->hasUnsavedChanges())
        n++;
    }
    else if (PlotWindow* p = dynamic_cast<PlotWindow*>(w))
    {
      if (p->hasUnsavedChanges())
        n++;
    }
  }
  return n;
}

bool Grace::queryUnsavedWindows()
{
  for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    juce::TopLevelWindow* w = juce::TopLevelWindow::getTopLevelWindow(i);
    if (!(WindowTypes::isWindowType(w, WindowTypes::CodeEditor) ||
          (WindowTypes::isWindowType(w, WindowTypes::PlotWindow))))
      continue;
    juce::String m = "Save changes you made to document ";
    m << "\"" << w->getName() << "\"?";
    int flag = juce::AlertWindow::showYesNoCancelBox(juce::AlertWindow::QuestionIcon,
                                                     juce::String(),
                                                     m, "Don't Save", "Save", "Cancel");
    //    std::cout << "query return flag=" << flag << "\n";
    if (flag == 0) return false;
    if (flag == 1) continue; 
    if (WindowTypes::isWindowType(w, WindowTypes::CodeEditor))
    {
      w->toFront(false);
      CodeEditorWindow* c=(CodeEditorWindow*)w;
      c->saveFile(false);
    }
    else if (WindowTypes::isWindowType(w, WindowTypes::PlotWindow))
    {
    }
  }
  return true;
}

void Grace::shutdown()
{
  // Typing COMMAND-Q on mac brings us here WITHOUT closing any windows.
  std::cout << "Quitting Grace...\n";
  activeWindow = 0;
  ConsoleWindow* cw = (ConsoleWindow *)Console::getInstance()->getParentComponent();
  // save console window's position and size as preference
  Preferences::getInstance()->setStringProp("ConsoleState", cw->getWindowStateAsString()); 	
  // delete all open windows except the console (which might still be
  // in use by scheme)
  {
    juce::Array<juce::TopLevelWindow*> windows;
    for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
    {
      juce::TopLevelWindow* t = juce::TopLevelWindow::getTopLevelWindow(i);
      if (t != cw) windows.add(t);
    }
    for (int i = 0; i < windows.size(); i++)
      delete windows[i];
  }
  Preferences::getInstance()->getProps().saveIfNeeded();
  // Stop all scheme processing but do NOT delete scheme thread (or
  // app crashes...)
  std::cout << "Flushing Scheme...clearing midi hooks..." << std::flush;
  SchemeThread::getInstance()->clearMidiHook(-1);
  std::cout << "clearing osc hooks..."  << std::flush;
  SchemeThread::getInstance()->clearOscHook("*");
  std::cout << "clearing scheduler..."  << std::flush;
  SchemeThread::getInstance()->schemeNodes.clear(); // remove any running nodes from scheduler
  //  SchemeThread::getInstance()->stop(0, true);
  // Interrupt any scheme eval going on
  std::cout << "interrupting scheme..."  << std::flush;
  SchemeThread::getInstance()->setSchemeInterrupt(true);
  std::cout << "OK!\n";

//  std::cout << "stopping scheme thread..."  << std::flush;
//  SchemeThread::getInstance()->stopThread(1000);
//  std::cout << "OK!\n";

  // Delete console window, this will delete Console
  std::cout << "Deleting Console Window..."  << std::flush;
  delete cw;
  std::cout << "OK!\n";

  // Flush midi output queue
  std::cout << "Flushing MidiOut..." << std::flush;
  MidiOutPort::getInstance()->clear();
  std::cout << "OK!\n";

  // delete audio manager before midi out because of internal synth
  std::cout << "Deleting AudioManager..." << std::flush;
  AudioManager::deleteInstance();
  std::cout << "OK!\n";

  std::cout << "Deleting MidiOut..." << std::flush;
  MidiOutPort::deleteInstance();
  std::cout << "OK!\n";

  std::cout << "Deleting MidiIn..." << std::flush;
  MidiInPort::deleteInstance();
  std::cout << "OK!\n";

  std::cout << "Deleting OpenSoundControl..." << std::flush;
  OpenSoundControl::deleteInstance();
  std::cout << "OK!\n";

  std::cout << "Deleting Csound..." << std::flush;
  Csound::deleteInstance();
  std::cout << "OK!\n";

#ifdef WITH_FOMUS
  std::cout << "Deleting Fomus..." << std::flush;
  Fomus::deleteInstance();
  std::cout << "OK!\n";
#endif

#ifdef WITH_SDIF
  std::cout << "Deleting SDIF..." << std::flush;
  SdifGenKill();
  std::cout << "OK!\n";
#endif

  std::cout << "Deleting Preferences..." << std::flush;
  Preferences::deleteInstance();
  std::cout << "OK!\n";

  std::cout << "Deleting Menu Bar Model..." << std::flush;
#if JUCE_MAC  // ..and also the main bar if we're using that on a Mac...
  juce::MenuBarModel::setMacMainMenu (0);
#endif
  delete menuBarModel;
  std::cout << "OK!\n";

  std::cout << "Deleting LookAndFeel..." << std::flush;
  delete lookAndFeel;
  std::cout << "OK!\n";


  std::cout << "Deleting SchemeThread..." << std::flush;
  SchemeThread::deleteInstance();
  std::cout << "OK!\n";
  std::cout << "Deleting Help..." << std::flush;
  Help::deleteInstance();
  std::cout << "OK!\n";
  std::cout << "Deleting SndLib..." << std::flush;
  SndLib::deleteInstance();
  std::cout << "OK!\n";
  std::cout << "Deleting TextSyntax..." << std::flush;
  TextSyntax::deleteInstance();
  std::cout << "OK!\n";
  std::cout << "Deleting LispSyntax..." << std::flush;
  LispSyntax::deleteInstance();
  std::cout << "OK!\n";
  std::cout << "Deleting SalSyntax..." << std::flush;
  SalSyntax::deleteInstance();
  std::cout << "OK!\n";
  std::cout << "Deleting Sal2Syntax..." << std::flush;
  Sal2Syntax::deleteInstance();
  std::cout << "OK!\n";  
  std::cout << "Bye!\n";
}

/** flush any musical processes currently running, interrupt any lisp
    eval taking place and silence any audio/midi output **/

void Grace::reset()
{
  std::cout << "RESET!\n";
}

START_JUCE_APPLICATION(Grace)

