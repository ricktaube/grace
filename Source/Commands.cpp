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

#include "Enumerations.h"
#include "Midi.h"
#include "Scheme.h"
#include "Syntax.h"
#include "Console.h"
#include "Help.h"
#include "Main.h"
#include "Preferences.h"
#include "Audio.h"
#include "AudioFilePlayer.h"
#include "MidiFilePlayer.h"
#include "Csound.h"
#include "PlotWindow.h"
#include "CodeEditor.h"
#include "SndLibLoad.h"
#include "OpenSoundControl.h"
#ifdef WITH_FOMUS
#include "Fomus.h"
#endif

void Grace::getAllCommands(juce::Array<juce::CommandID>& commands)
{
  const juce::CommandID ids[] = 
    {
      //----//
      //File//
      //----//

      CommandIDs::FileNew,
      CommandIDs::FileNewPlotter,
      CommandIDs::FileOpen,
      CommandIDs::FileOpenRecent + 0,
      CommandIDs::FileOpenRecent + 1,
      CommandIDs::FileOpenRecent + 2,
      CommandIDs::FileOpenRecent + 3,
      CommandIDs::FileOpenRecent + 4,
      CommandIDs::FileOpenRecent + 5,
      CommandIDs::FileOpenRecent + 6,
      CommandIDs::FileOpenRecent + 7,
      CommandIDs::FileClearOpenRecent,
      CommandIDs::FileClose,
      CommandIDs::FileSave,
      CommandIDs::FileSaveAs,
      CommandIDs::FileSaveVersion,
      CommandIDs::FilePlayAudioFile,
      CommandIDs::FilePlayMidiFile,
      CommandIDs::FileLoad,
      CommandIDs::FileLoadRecent + 0,
      CommandIDs::FileLoadRecent + 1,
      CommandIDs::FileLoadRecent + 2,
      CommandIDs::FileLoadRecent + 3,
      CommandIDs::FileLoadRecent + 4,
      CommandIDs::FileLoadRecent + 5,
      CommandIDs::FileLoadRecent + 6,
      CommandIDs::FileLoadRecent + 7,
      CommandIDs::FileClearLoadRecent,
      CommandIDs::FileSetInit,
      CommandIDs::FileClearInit,  
      CommandIDs::FileOpenInit,  
      CommandIDs::FileShowDirectory,
      CommandIDs::FileSetDirectory,
      CommandIDs::FileQuit,

      //----//
      //Edit//
      //----//

      CommandIDs::EditUndo,
      CommandIDs::EditRedo,
      CommandIDs::EditCut,
      CommandIDs::EditCopy,
      CommandIDs::EditPaste,
      CommandIDs::EditDelete,
      CommandIDs::EditSelectAll,
      CommandIDs::EditDeselectAll,
      CommandIDs::EditFind,
      CommandIDs::EditSyntax + TextIDs::Text,
      CommandIDs::EditSyntax + TextIDs::Lisp,
      CommandIDs::EditSyntax + TextIDs::Sal2,
      CommandIDs::EditDefaultSyntax,
      CommandIDs::EditIndent,
      CommandIDs::EditParensMatching,
      CommandIDs::EditEmacsMode,

      //----//
      //View//
      //----//

      CommandIDs::ViewFontSize + 10,
      CommandIDs::ViewFontSize + 12,
      CommandIDs::ViewFontSize + 14,
      CommandIDs::ViewFontSize + 16,
      CommandIDs::ViewFontSize + 18,
      CommandIDs::ViewFontSize + 20,
      CommandIDs::ViewFontSize + 22,
      CommandIDs::ViewFontSize + 24,
      CommandIDs::ViewFontSize + 26,
      CommandIDs::ViewFontSize + 28,
      CommandIDs::ViewFontSize + 30,
      CommandIDs::ViewFontSize + 32,
      CommandIDs::ViewDefaultFontSize,
      CommandIDs::ViewFontBigger,
      CommandIDs::ViewFontSmaller,
      CommandIDs::ViewLineNumbersVisible,
      CommandIDs::ViewReadCustomComment,
      CommandIDs::ViewWriteCustomComment,
      CommandIDs::ViewClearConsole,
      CommandIDs::ViewConsoleBeepOnError,

      //----------//
      //Audio Menu//
      //----------//

      // Midi Out Port
      CommandIDs::MidiOutTest,
      CommandIDs::MidiOutHush,
      CommandIDs::MidiOutTuning + 1, // tuning nums are 1-16 inclusive
      CommandIDs::MidiOutTuning + 2,
      CommandIDs::MidiOutTuning + 3,
      CommandIDs::MidiOutTuning + 4,
      CommandIDs::MidiOutTuning + 5,
      CommandIDs::MidiOutTuning + 6,
      CommandIDs::MidiOutTuning + 7,
      CommandIDs::MidiOutTuning + 8,
      CommandIDs::MidiOutTuning + 9,
      CommandIDs::MidiOutTuning + 10,
      CommandIDs::MidiOutTuning + 11,
      CommandIDs::MidiOutTuning + 12,
      CommandIDs::MidiOutTuning + 13,
      CommandIDs::MidiOutTuning + 14,
      CommandIDs::MidiOutTuning + 15,
      CommandIDs::MidiOutTuning + 16,
      CommandIDs::MidiOutDrumTrack,
      CommandIDs::MidiOutPitchBend,
      CommandIDs::MidiOutInstruments,
      CommandIDs::MidiOutFileSettings,
      //      CommandIDs::MidiOutClosed,

      // Midi In Port

      CommandIDs::MidiInTrace,
      CommandIDs::MidiInChannelFilter + 0,
      CommandIDs::MidiInChannelFilter + 1,
      CommandIDs::MidiInChannelFilter + 2,
      CommandIDs::MidiInChannelFilter + 3,
      CommandIDs::MidiInChannelFilter + 4,
      CommandIDs::MidiInChannelFilter + 5,
      CommandIDs::MidiInChannelFilter + 6,
      CommandIDs::MidiInChannelFilter + 7,
      CommandIDs::MidiInChannelFilter + 8,
      CommandIDs::MidiInChannelFilter + 9,
      CommandIDs::MidiInChannelFilter + 10,
      CommandIDs::MidiInChannelFilter + 11,
      CommandIDs::MidiInChannelFilter + 12,
      CommandIDs::MidiInChannelFilter + 13,
      CommandIDs::MidiInChannelFilter + 14,
      CommandIDs::MidiInChannelFilter + 15,
      CommandIDs::MidiInChannelFilter + 16, // All
      CommandIDs::MidiInOpcodeFilter + 0,
      CommandIDs::MidiInOpcodeFilter + 1,
      CommandIDs::MidiInOpcodeFilter + 2,
      CommandIDs::MidiInOpcodeFilter + 3,
      CommandIDs::MidiInOpcodeFilter + 4,
      CommandIDs::MidiInOpcodeFilter + 5,
      CommandIDs::MidiInOpcodeFilter + 6,
      CommandIDs::MidiInOpcodeFilter + 7, // All

      // Audio
      CommandIDs::AudioSettings,
      CommandIDs::MidiDeviceSettings,
      CommandIDs::SndLibSrate + 0,
      CommandIDs::SndLibSrate + 1,
      CommandIDs::SndLibSrate + 2,

      CommandIDs::SndLibChannels + 0,
      CommandIDs::SndLibChannels + 1,
      CommandIDs::SndLibChannels + 2,
      CommandIDs::SndLibChannels + 3,

      CommandIDs::SndLibAudioFormat + 0,
      CommandIDs::SndLibAudioFormat + 1,
      CommandIDs::SndLibAudioFormat + 2,
      CommandIDs::SndLibAudioFormat + 3,

      CommandIDs::SndLibAutoPlay,
      CommandIDs::SndLibInsDialog,

      CommandIDs::CsoundPrefWriteAfter,
      CommandIDs::CsoundPrefPlayAfter,
      CommandIDs::CsoundExportScore,
      CommandIDs::CsoundClearScore,
      CommandIDs::CsoundOpenSettings,

      CommandIDs::OscOpen,
      CommandIDs::OscTraceInput,
      CommandIDs::OscTestOutput,

      //---------//
      //Eval Menu//
      //---------//

      CommandIDs::EvalExecute,
      CommandIDs::EvalExpand,
      CommandIDs::EvalBacktrace,
      CommandIDs::EvalAbort,

      //-----------//
      //Window Menu//
      //-----------//

      CommandIDs::WindowSelect + 0,  // 0==Select Console
      CommandIDs::WindowSelect + 1,
      CommandIDs::WindowSelect + 2,
      CommandIDs::WindowSelect + 3,
      CommandIDs::WindowSelect + 4,
      CommandIDs::WindowSelect + 5,
      CommandIDs::WindowSelect + 6,
      CommandIDs::WindowSelect + 7,
      CommandIDs::WindowPlottingControls,

      //----------//
      // Help Menu//
      //----------//

      CommandIDs::HelpManual + 0,
      CommandIDs::HelpManual + 1,
      CommandIDs::HelpManual + 2,
      CommandIDs::HelpManual + 3,
      CommandIDs::HelpManual + 4,
      CommandIDs::HelpManual + 5,
      CommandIDs::HelpManual + 6,
      CommandIDs::HelpManual + 7,

      CommandIDs::HelpSalExample + 0,
      CommandIDs::HelpSalExample + 1,
      CommandIDs::HelpSalExample + 2,
      CommandIDs::HelpSalExample + 3,
      CommandIDs::HelpSalExample + 4,
      CommandIDs::HelpSalExample + 5,
      CommandIDs::HelpSalExample + 6,
      CommandIDs::HelpSalExample + 7,
      CommandIDs::HelpSalExample + 8,
      CommandIDs::HelpSalExample + 9,
      CommandIDs::HelpSalExample + 10,
      CommandIDs::HelpSalExample + 11,
      CommandIDs::HelpSalExample + 12,
      CommandIDs::HelpSalExample + 13,
      CommandIDs::HelpSalExample + 14,
      CommandIDs::HelpSalExample + 15,
      CommandIDs::HelpSalExample + 16,
      CommandIDs::HelpSalExample + 17,
      CommandIDs::HelpSalExample + 18,
      CommandIDs::HelpSalExample + 19,
      CommandIDs::HelpSalExample + 20,
      CommandIDs::HelpSalExample + 21,
      CommandIDs::HelpSalExample + 22,
      CommandIDs::HelpSalExample + 23,
      CommandIDs::HelpSalExample + 24,
      CommandIDs::HelpSalExample + 25,
      CommandIDs::HelpSalExample + 26,
      CommandIDs::HelpSalExample + 27,
      CommandIDs::HelpSalExample + 28,
      CommandIDs::HelpSalExample + 29,
      CommandIDs::HelpSalExample + 30,
      CommandIDs::HelpSalExample + 31,

      CommandIDs::HelpSchemeExample + 0,
      CommandIDs::HelpSchemeExample + 1,
      CommandIDs::HelpSchemeExample + 2,
      CommandIDs::HelpSchemeExample + 3,
      CommandIDs::HelpSchemeExample + 4,
      CommandIDs::HelpSchemeExample + 5,
      CommandIDs::HelpSchemeExample + 6,
      CommandIDs::HelpSchemeExample + 7,
      CommandIDs::HelpSchemeExample + 8,
      CommandIDs::HelpSchemeExample + 9,
      CommandIDs::HelpSchemeExample + 10,
      CommandIDs::HelpSchemeExample + 11,
      CommandIDs::HelpSchemeExample + 12,
      CommandIDs::HelpSchemeExample + 13,
      CommandIDs::HelpSchemeExample + 14,
      CommandIDs::HelpSchemeExample + 15,
      CommandIDs::HelpSchemeExample + 16,
      CommandIDs::HelpSchemeExample + 17,
      CommandIDs::HelpSchemeExample + 18,
      CommandIDs::HelpSchemeExample + 19,
      CommandIDs::HelpSchemeExample + 20,
      CommandIDs::HelpSchemeExample + 21,
      CommandIDs::HelpSchemeExample + 22,
      CommandIDs::HelpSchemeExample + 23,
      CommandIDs::HelpSchemeExample + 24,
      CommandIDs::HelpSchemeExample + 25,
      CommandIDs::HelpSchemeExample + 26,
      CommandIDs::HelpSchemeExample + 27,
      CommandIDs::HelpSchemeExample + 28,
      CommandIDs::HelpSchemeExample + 29,
      CommandIDs::HelpSchemeExample + 30,
      CommandIDs::HelpSchemeExample + 31,

      CommandIDs::HelpSalTutorial + 0,
      CommandIDs::HelpSalTutorial + 1,
      CommandIDs::HelpSalTutorial + 2,
      CommandIDs::HelpSalTutorial + 3,
      CommandIDs::HelpSalTutorial + 4,
      CommandIDs::HelpSalTutorial + 5,
      CommandIDs::HelpSalTutorial + 6,
      CommandIDs::HelpSalTutorial + 7,
      CommandIDs::HelpSalTutorial + 8,
      CommandIDs::HelpSalTutorial + 9,
      CommandIDs::HelpSalTutorial + 10,
      CommandIDs::HelpSalTutorial + 11,
      CommandIDs::HelpSalTutorial + 12,
      CommandIDs::HelpSalTutorial + 13,
      CommandIDs::HelpSalTutorial + 14,
      CommandIDs::HelpSalTutorial + 15,

      CommandIDs::HelpSchemeTutorial + 0,
      CommandIDs::HelpSchemeTutorial + 1,
      CommandIDs::HelpSchemeTutorial + 2,
      CommandIDs::HelpSchemeTutorial + 3,
      CommandIDs::HelpSchemeTutorial + 4,
      CommandIDs::HelpSchemeTutorial + 5,
      CommandIDs::HelpSchemeTutorial + 6,
      CommandIDs::HelpSchemeTutorial + 7,
      CommandIDs::HelpSchemeTutorial + 8,
      CommandIDs::HelpSchemeTutorial + 9,
      CommandIDs::HelpSchemeTutorial + 10,
      CommandIDs::HelpSchemeTutorial + 11,
      CommandIDs::HelpSchemeTutorial + 12,
      CommandIDs::HelpSchemeTutorial + 13,
      CommandIDs::HelpSchemeTutorial + 14,
      CommandIDs::HelpSchemeTutorial + 15,

      CommandIDs::HelpSchemeExtra + 0,
      CommandIDs::HelpSchemeExtra + 1,
      CommandIDs::HelpSchemeExtra + 2,
      CommandIDs::HelpSchemeExtra + 3,
      CommandIDs::HelpSchemeExtra + 4,
      CommandIDs::HelpSchemeExtra + 5,
      CommandIDs::HelpSchemeExtra + 6,
      CommandIDs::HelpSchemeExtra + 7,

      CommandIDs::HelpExport + 1,  //Sal Examples
      CommandIDs::HelpExport + 2,  //Sal Tutorials
      CommandIDs::HelpExport + 3,  //Scheme Examples
      CommandIDs::HelpExport + 4,  //Scheme Tutorials
      CommandIDs::HelpExport + 5,  //Extras

      CommandIDs::HelpFomus,

      CommandIDs::HelpWebSite + 0,
      CommandIDs::HelpWebSite + 1,
      CommandIDs::HelpWebSite + 2,
      CommandIDs::HelpWebSite + 3,
      CommandIDs::HelpWebSite + 4,
      CommandIDs::HelpWebSite + 5,
      CommandIDs::HelpWebSite + 6,
      CommandIDs::HelpWebSite + 7,
      CommandIDs::HelpDocumentationLookup
    };
  commands.addArray(ids, sizeof(ids) / sizeof(juce::CommandID));
}

/*=======================================================================*
  MenuBar Management
 *=======================================================================*/

juce::StringArray Grace::GraceMenuBarModel::getMenuBarNames()
{
  const char* const names[] = {"File", "Edit", "View", "Audio", "Eval", "Window", "Help", 0 };
  return juce::StringArray(names);
}

void Grace::setActiveWindow(juce::TopLevelWindow* window)
{
  if (window == activeWindow)
    return;
  activeWindow = window;
  refreshMenuBar();
}

void Grace::getApplicationWindows(juce::Array<juce::TopLevelWindow*>& appWindows)
{
  // gather all non-tool windows
  for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    juce::TopLevelWindow* w = juce::TopLevelWindow::getTopLevelWindow(i);
    if (WindowTypes::isWindowType(w, WindowTypes::PlottingControls)) 
      continue;
    if (WindowTypes::isWindowType(w, WindowTypes::FindAndReplace)) 
      continue;
    appWindows.add(w);
  }
}

void Grace::refreshMenuBar()
{
  //MACMENU
#ifdef JUCE_MAC
  menuBarModel->menuItemsChanged();
#endif
}

juce::PopupMenu Grace::GraceMenuBarModel::getMenuForIndex(int idx, const juce::String &name)
{
  juce::ApplicationCommandManager* m = &(Grace::getApp().commandManager);
  juce::PopupMenu menu;
  //#ifndef MACAPPSTORE
  Preferences* p = Preferences::getInstance();
  //#endif
  if (name == "File")
  {
    menu.addCommandItem(m, CommandIDs::FileNew);
    menu.addCommandItem(m, CommandIDs::FileNewPlotter);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::FileOpen);
    //#ifndef MACAPPSTORE
    if (p->recentlyOpened.getNumFiles() > 0)
    {
      juce::PopupMenu s;
      int n = juce::jmin(p->recentlyOpened.getNumFiles(), 8);
      for (int i = 0; i < n; i++) 
        s.addCommandItem(m, CommandIDs::FileOpenRecent + i);  
      s.addSeparator();
      s.addCommandItem(m, CommandIDs::FileClearOpenRecent);  
      menu.addSubMenu("Open Recent", s);
    }
    //#endif
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::FileClose);
    menu.addCommandItem(m, CommandIDs::FileSave);
    menu.addCommandItem(m, CommandIDs::FileSaveAs);
    menu.addCommandItem(m, CommandIDs::FileSaveVersion);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::FilePlayAudioFile);
    menu.addCommandItem(m, CommandIDs::FilePlayMidiFile);
    menu.addSeparator();
    menu.addCommandItem(m,CommandIDs::FileLoad);
#ifndef MACAPPSTORE
    if (p->recentlyLoaded.getNumFiles() > 0)
    {
      juce::PopupMenu s;
      int n = juce::jmin(p->recentlyLoaded.getNumFiles(), 8);
      for (int i = 0; i < n; i++)
        s.addCommandItem(m, CommandIDs::FileLoadRecent + i);  
      s.addSeparator();
      s.addCommandItem(m, CommandIDs::FileClearLoadRecent);  
      menu.addSubMenu("Load Recent", s);
    }
#endif
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::FileShowDirectory);
    menu.addCommandItem(m, CommandIDs::FileSetDirectory);  
    menu.addSeparator();
    menu.addCommandItem(m,CommandIDs::FileSetInit);
    menu.addCommandItem(m,CommandIDs::FileClearInit);
    menu.addCommandItem(m,CommandIDs::FileOpenInit);
#ifndef JUCE_MAC
    // on the mac Quit is in the App menu (not the file menu)
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::FileQuit);
#endif
  }
  else if (name == "Edit")
  {
    menu.addCommandItem(m, CommandIDs::EditUndo);
    menu.addCommandItem(m, CommandIDs::EditRedo);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::EditCut);
    menu.addCommandItem(m, CommandIDs::EditCopy);
    menu.addCommandItem(m, CommandIDs::EditPaste);
    menu.addCommandItem(m, CommandIDs::EditDelete);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::EditSelectAll);
    menu.addCommandItem(m, CommandIDs::EditDeselectAll);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::EditFind);
    menu.addSeparator();
    {
      juce::PopupMenu s;
      s.addCommandItem(m, CommandIDs::EditSyntax + TextIDs::Text);
      s.addCommandItem(m, CommandIDs::EditSyntax + TextIDs::Lisp);
      s.addCommandItem(m, CommandIDs::EditSyntax + TextIDs::Sal2);
      s.addSeparator();
      s.addCommandItem(m, CommandIDs::EditDefaultSyntax);
      menu.addSubMenu("Editing Syntax", s);
    } 
    menu.addCommandItem(m, CommandIDs::EditIndent);
    menu.addCommandItem(m, CommandIDs::EditParensMatching);
    menu.addCommandItem(m, CommandIDs::EditEmacsMode);
  }
  else if (name == "View")
  {
    menu.addCommandItem(m, CommandIDs::ViewFontBigger);
    menu.addCommandItem(m, CommandIDs::ViewFontSmaller);
    juce::PopupMenu s;
    for (int i = 10; i <= 32; i = i + 2)
      s.addCommandItem(m, CommandIDs::ViewFontSize + i); // min fontsize is 9pt
    s.addSeparator();
    s.addCommandItem(m, CommandIDs::ViewDefaultFontSize);
    menu.addSubMenu("Font Sizes", s);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::ViewLineNumbersVisible);
    menu.addCommandItem(m, CommandIDs::ViewReadCustomComment);
    menu.addCommandItem(m, CommandIDs::ViewWriteCustomComment);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::ViewClearConsole);
    menu.addCommandItem(m, CommandIDs::ViewConsoleBeepOnError);
  }
  else if (name == "Audio")
  {
    // Midi Out
    {
      juce::PopupMenu midi;
      juce::PopupMenu s;
      juce::String title ("MIDI Out: ");
      if (MidiOutPort::getInstance()->isOpen())
      {
        title << MidiOutPort::getInstance()->getOpenDeviceName();
        midi.addCommandItem(m, CommandIDs::MidiOutTest);
        midi.addSeparator();
        for (int i = 1; i <= 16; i++) // tunings 1 based!
          s.addCommandItem(m, CommandIDs::MidiOutTuning + i);
        midi.addSubMenu("Tuning", s);
        midi.addCommandItem(m, CommandIDs::MidiOutInstruments);  
        menu.addSubMenu(title, midi);
      }
      else
      {
        juce::StringArray devices = MidiOutPort::getInstance()->getDevices();
        if (devices.size() == 0)
          title << "(no device available)";
        else
          title << "(no device selected)";
        menu.addItem(CommandIDs::MidiOutClosed, title, false);
      }
    }
    // Midi In
    {
      juce::PopupMenu midi;
      juce::PopupMenu s;
      juce::String title ("MIDI In: ");
      if (MidiInPort::getInstance()->isOpen())
      {
        title << MidiInPort::getInstance()->getOpenDeviceName();
        s.addCommandItem(m, CommandIDs::MidiInChannelFilter + 16); // All
        for (int i = 0; i < 16; i++)
          s.addCommandItem(m, CommandIDs::MidiInChannelFilter + i);
        midi.addSubMenu("Channels Allowed", s);
        s.clear();
        s.addCommandItem(m, CommandIDs::MidiInOpcodeFilter + 7); // All
        s.addSeparator();
        for (int i = 0; i < 7; i++)
          s.addCommandItem(m, CommandIDs::MidiInOpcodeFilter + i);
        midi.addSubMenu("Messages Allowed", s);
        midi.addSeparator();
        midi.addCommandItem(m, CommandIDs::MidiInTrace);
        menu.addSubMenu(title, midi);
      }
      else
      {
        juce::StringArray devices = MidiInPort::getInstance()->getDevices();
        if (devices.size() == 0)
          title << "(no device available)";
        else
          title << "(no device selected)";
        menu.addItem(CommandIDs::MidiInClosed, title, false);
      }
    }
    menu.addCommandItem(m, CommandIDs::MidiDeviceSettings);
    menu.addSeparator();
    // Osc
    {
      juce::String t ("OSC Out: ");
      juce::PopupMenu s;
      if (int p = OpenSoundControl::getInstance()->getOutputPort())
      {
        t << "port " << juce::String(p);
        s.addCommandItem(m, CommandIDs::OscTestOutput);
        menu.addSubMenu(t, s);
      }
      else
      {
        t << "(not connected)";
        menu.addItem(CommandIDs::OscOutputClosed, t, false);
      }
      s.clear();
      t = "OSC In: ";
      if (int p = OpenSoundControl::getInstance()->getInputPort())
      {
        t << "port " << juce::String(p);
        s.addCommandItem(m, CommandIDs::OscTraceInput);
        menu.addSubMenu(t, s);
      }
      else
      {
        t << "(not connected)";
        menu.addItem(CommandIDs::OscInputClosed, t, false);
      }
      menu.addCommandItem(m, CommandIDs::OscOpen);
    }
    // Sndlib
    {
      menu.addSeparator();
      juce::PopupMenu clm;
      juce::PopupMenu s;
      for (int i = 0; i < SrateIDs::NumSrates; i++)
        s.addCommandItem(m, CommandIDs::SndLibSrate + i);
      clm.addSubMenu("Srate", s);
      s.clear();
      for (int i = 0; i < ChannelIDs::NumChannels; i++) 
        s.addCommandItem(m, CommandIDs::SndLibChannels + i);
      clm.addSubMenu("Channels", s);
      s.clear();
      for (int i = 0; i < AudioFormatIDs::NumAudioFormats; i++)
        s.addCommandItem(m, CommandIDs::SndLibAudioFormat + i);
      clm.addSubMenu("Audio Format", s);
      clm.addCommandItem(m, CommandIDs::SndLibAutoPlay);
      menu.addSubMenu("CLM Output Settings", clm);
      menu.addCommandItem(m, CommandIDs::SndLibInsDialog);
    }
    // CSound
    {
      menu.addSeparator();
      juce::PopupMenu csound;
      csound.addCommandItem(m, CommandIDs::CsoundPrefWriteAfter);
      csound.addCommandItem(m, CommandIDs::CsoundPrefPlayAfter);
      csound.addSeparator();
      csound.addCommandItem(m, CommandIDs::CsoundExportScore);
      csound.addCommandItem(m, CommandIDs::CsoundClearScore);
      csound.addSeparator();
      csound.addCommandItem(m, CommandIDs::CsoundOpenSettings);
      menu.addSubMenu("Csound", csound);
    }
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::AudioSettings);
    menu.addCommandItem(m, CommandIDs::MidiOutFileSettings);
  }
  else if (name == "Eval")
  {
    menu.addCommandItem(m, CommandIDs::EvalExecute);
    menu.addCommandItem(m, CommandIDs::EvalExpand);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::EvalBacktrace);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::EvalAbort);
  }
  else if (name == "Window")
  {
    juce::Array<juce::TopLevelWindow*> windows;
    Grace::getApp().getApplicationWindows(windows);
    // Console Window is always open and always at zero.
    menu.addCommandItem(m, CommandIDs::WindowSelect + 0);
    int n = juce::jlimit(0, 8, windows.size());
    for (int i = 1; i < n; i++)
    {
      menu.addCommandItem(m, CommandIDs::WindowSelect + i);
    }
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::WindowPlottingControls);
  }
  else if (name == "Help")
  {
    Help* help = Help::getInstance();
    menu.addSectionHeader("Sal Coding");
    {
      juce::PopupMenu e, t;
      help->addHelpMenuItems(e, "Sal Examples", CommandIDs::HelpSalExample, 32, m, 1);
      help->addHelpMenuItems(t, "Sal Tutorials", CommandIDs::HelpSalTutorial, 16, m,2);
      menu.addSubMenu("Examples", e);
      menu.addSubMenu("Tutorials", t);
    }
    menu.addSeparator();
    menu.addSectionHeader("Scheme Coding");
    {
      juce::PopupMenu e, t, x;
      help->addHelpMenuItems(e, "Scheme Examples", CommandIDs::HelpSchemeExample, 32, m, 3);
      help->addHelpMenuItems(t, "Scheme Tutorials", CommandIDs::HelpSchemeTutorial, 16, m, 4);
      help->addHelpMenuItems(x, "Scheme Extras", CommandIDs::HelpSchemeExtra, 8, m, 5);
      menu.addSubMenu("Examples", e);
      menu.addSubMenu("Tutorials", t);
      menu.addSubMenu("Extras", x);
    }
    menu.addSeparator();
    menu.addSectionHeader("Documentation & Reference");
    help->addHelpMenuItems(menu, "Reference", CommandIDs::HelpManual, 8, m, 0);
#ifdef WITH_FOMUS
    menu.addCommandItem(m, CommandIDs::HelpFomus);
#endif
    menu.addSeparator();
    menu.addSectionHeader("Websites");
    help->addHelpMenuItems(menu, "Web Sites", CommandIDs::HelpWebSite, 8, m, 0);
    menu.addSeparator();
    menu.addCommandItem(m, CommandIDs::HelpDocumentationLookup);
  }
  return menu;
}

/*=======================================================================*
  Application Commands             
  *=======================================================================*/

juce::ApplicationCommandTarget* Grace::getNextCommandTarget()
{
  return 0;
  //  return Grace::getInstance();
}


void Grace::getCommandInfo(const juce::CommandID id, juce::ApplicationCommandInfo& info)
{
  int comm = CommandIDs::getCommand(id);
  int data = CommandIDs::getCommandData(id);
  Preferences* pref = Preferences::getInstance();
  //  int ctrl = juce::ModifierKeys::ctrlModifier;
  //  int meta = juce::ModifierKeys::altModifier;
  int comk = juce::ModifierKeys::commandModifier;
  // FIXME: on Mac the command key is always active because it never
  // conflicts with Emacs Mode. But on Windows or Linux if an editor
  // window is active this should allow the command key shortcut to be
  // installed if the window is not in Emacs Mode.
  //MACMENU
#ifdef JUCE_MAC
  static const bool comKeyActive = true;
#else
  bool comKeyActive = true;
  if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    comKeyActive = !w->getCodeEditor()->isEmacsMode();
#endif

  //Set default values for the menu flags.
  info.setActive(true);
  info.setTicked(false);

  switch (comm)
  {

    //----//
    //File//
    //----//

  case CommandIDs::FileNew:
    info.shortName = "New Editor";
    info.addDefaultKeypress('N', comk);
    break;
  case CommandIDs::FileNewPlotter:
    info.shortName = "New Plot Window";
    info.addDefaultKeypress('N', comk | juce::ModifierKeys::shiftModifier);
    break;
  case CommandIDs::FileOpen:
    info.shortName = "Open...";
    info.addDefaultKeypress('O', comk);
    break;
  case CommandIDs::FileOpenRecent:
    {
      juce::File f = pref->recentlyOpened.getFile(data);
      if (f == juce::File())
        info.shortName = "<unknown file>";
      else
        info.shortName = f.getFileName();
    }
    break;
  case CommandIDs::FileClearOpenRecent:
    info.shortName = "Clear Menu";
    break;
  case CommandIDs::FileClose:
    info.shortName = "Close";
    info.setActive(dynamic_cast<CodeEditorWindow*>(activeWindow) != 0);
    info.addDefaultKeypress('W', comk);
    break;
  case CommandIDs::FileSave:
    {
      info.shortName = "Save";
      info.addDefaultKeypress('S', comk);
      info.setActive(dynamic_cast<CodeEditorWindow*>(activeWindow) != 0);
    }
    break;
  case CommandIDs::FileSaveAs:
    info.shortName = "Save As...";
    if (comKeyActive)
      info.addDefaultKeypress('S', comk | juce::ModifierKeys::shiftModifier);
    info.setActive(dynamic_cast<CodeEditorWindow*>(activeWindow) != 0);
    break;
  case CommandIDs::FileSaveVersion:
    info.shortName = "Save As Version";
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->getSourceFile().existsAsFile());
    else
      info.setActive(false);
    break;
  case CommandIDs::FilePlayAudioFile:
    info.shortName = "Play Audio File...";
    break;
  case CommandIDs::FilePlayMidiFile:
    info.shortName = "Play Midi File...";
    info.setActive(true);
    break;
  case CommandIDs::FileLoad:
    info.shortName = "Load File...";
    info.setActive(true);
    break;
  case CommandIDs::FileLoadRecent:
    {
      juce::File f = pref->recentlyLoaded.getFile(data);
      if (f == juce::File())
        info.shortName = "<Unknown File>";
      else
        info.shortName = f.getFileName();
    }
    break;
  case CommandIDs::FileClearLoadRecent:
    info.shortName = "Clear Menu";
    break;
  case CommandIDs::FileSetInit:
    info.shortName = "Set Init File...";
    break;
  case CommandIDs::FileClearInit:
    info.shortName = "Clear Init File";
    info.setActive(pref->getStringProp("LispInitFile") != juce::String());
    break;
  case CommandIDs::FileOpenInit:
    info.shortName = "Open Init File";
    info.setActive(pref->getStringProp("LispInitFile") != juce::String());
    break;
  case CommandIDs::FileShowDirectory:
    info.shortName = "Show Working Directory";
    break;
  case CommandIDs::FileSetDirectory:
    info.shortName = "Set Working Directory...";
    break;
  case CommandIDs::FileQuit:
    info.shortName = "Quit";
    info.addDefaultKeypress('Q', comk);
    break;

    //----//
    //Edit//
    //----//

  case CommandIDs::EditUndo:
    info.shortName = "Undo";
    if (comKeyActive)
      info.addDefaultKeypress('Z', comk);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->document.getUndoManager().canUndo());
    else 
      info.setActive(false);
    break;
  case CommandIDs::EditRedo:
    info.shortName = "Redo";
    if (comKeyActive)
      info.addDefaultKeypress('Z', comk | juce::ModifierKeys::shiftModifier);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->document.getUndoManager().canRedo());
    else 
      info.setActive(false);
    break;
  case CommandIDs::EditCut:
    info.shortName = "Cut";
    if (comKeyActive)
      info.addDefaultKeypress('X', comk);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->getCodeEditor()->getHighlightedRegion().getLength() > 0);
    else 
      info.setActive(false);
    break;
  case CommandIDs::EditCopy:
    info.shortName = "Copy";
    if (comKeyActive)
      info.addDefaultKeypress('C', comk);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->getCodeEditor()->getHighlightedRegion().getLength() > 0);
    else if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
      info.setActive(w->getTextBuffer()->getHighlightedRegion().getLength() > 0);
    else
      info.setActive(false);
    break;
  case CommandIDs::EditPaste:
    info.shortName = "Paste";
    if (comKeyActive)
      info.addDefaultKeypress('V', comk);
    if (dynamic_cast<CodeEditorWindow*>(activeWindow) != 0)
      info.setActive(juce::SystemClipboard::getTextFromClipboard().length() > 0);
    else
      info.setActive(false);
    break;
  case CommandIDs::EditDelete:
    info.shortName = "Delete";
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->getCodeEditor()->getHighlightedRegion().getLength() > 1);
    else 
      info.setActive(false);
    break;
  case CommandIDs::EditSelectAll:
    info.shortName = "Select All";
    if (comKeyActive)
      info.addDefaultKeypress('A', comk);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->document.getNumCharacters() > 0);
    else if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
      info.setActive(!w->getTextBuffer()->isEmpty());
    else 
      info.setActive(false);
    break;
  case CommandIDs::EditDeselectAll:
    info.shortName = "Deselect All";
    if (comKeyActive)
      info.addDefaultKeypress('A', comk | juce::ModifierKeys::shiftModifier);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->getCodeEditor()->getHighlightedRegion().getLength() > 0);
    else if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
      info.setActive(w->getTextBuffer()->getHighlightedRegion().getLength() > 0);
    else 
      info.setActive(false);
    break;
  case CommandIDs::EditFind:
    info.shortName = "Find...";
    if (comKeyActive)
      info.addDefaultKeypress('F', comk);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->document.getNumCharacters() > 0);
    else
      info.setActive(false);
    break;
  case CommandIDs::EditSyntax:
    info.shortName = TextIDs::toString(data);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      info.setActive(true);
      info.setTicked(w->getCodeEditor()->isTextType(data));
    }
    else
      info.setActive(false);
    break;
  case CommandIDs::EditDefaultSyntax:
    info.shortName = "Make Default For New Editors";
    info.setActive(dynamic_cast<CodeEditorWindow*>(activeWindow) != 0);
    break;
  case CommandIDs::EditIndent:
    info.shortName = "Indent";
    info.addDefaultKeypress('\t', 0);
    info.setActive(dynamic_cast<CodeEditorWindow*>(activeWindow) != 0);
    break;
  case CommandIDs::EditParensMatching:
    info.shortName = "Parentheses Matching";
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setTicked(w->getCodeEditor()->isParensMatching());
    else
      info.setActive(false);
    break;
  case CommandIDs::EditEmacsMode:
    info.shortName="Emacs Keys";
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setTicked(w->getCodeEditor()->isEmacsMode());
    else
      info.setActive(false);
    break;
            
    //----//
    //View//
    //----//

  case CommandIDs::ViewFontSize:
    {
      info.shortName = juce::String(data);
      if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
        info.setTicked(w->getConsole()->getFontSize() == data);
      else if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
        info.setTicked(w->getCodeEditor()->getFontSize() == data);
      else
        info.setActive(false);
    }
    break;
  case CommandIDs::ViewDefaultFontSize:
    //FIXME
    info.shortName="Make Default Font Size";
    //    info.setActive(data!=Preferences::getInstance()->getIntProp("EditorFontSize"));
    info.setActive((dynamic_cast<ConsoleWindow*>(activeWindow) != 0) ||
                   (dynamic_cast<CodeEditorWindow*>(activeWindow) != 0));
    break;
  case CommandIDs::ViewFontBigger:
    // FIXME: CHECK BOUNDS
    info.shortName = "Bigger Font";
    if (comKeyActive)
      info.addDefaultKeypress('=', comk);
    info.setActive((dynamic_cast<ConsoleWindow*>(activeWindow) != 0) ||
                   (dynamic_cast<CodeEditorWindow*>(activeWindow) != 0));
    break;
  case CommandIDs::ViewFontSmaller:
    // FIXME: CHECK BOUNDS
    info.shortName = "Smaller Font";
    if (comKeyActive)
      info.addDefaultKeypress('-', juce::ModifierKeys::commandModifier);
    info.setActive((dynamic_cast<ConsoleWindow*>(activeWindow) != 0) ||
                   (dynamic_cast<CodeEditorWindow*>(activeWindow) != 0));
    break;
  case CommandIDs::ViewClearConsole:
    info.shortName="Clear Console";
    info.addDefaultKeypress('C',  comk | juce::ModifierKeys::shiftModifier);
    break;
  case CommandIDs::ViewConsoleBeepOnError:
    info.shortName="Console Beep On Error";
    info.setTicked(Console::getInstance()->getBeepOnError());
    break;
  case CommandIDs::ViewLineNumbersVisible:
    info.shortName = "Show Line Numbers";
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setTicked(w->getCodeEditor()->isLineGutterVisible());
    else
      info.setActive(false);
    break;
  case CommandIDs::ViewReadCustomComment:
    info.shortName = "Read Editor Customizations";
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(w->isCustomComment());
    else
      info.setActive(false);
    break;
  case CommandIDs::ViewWriteCustomComment:
    info.shortName = "Write Editor Customizations";
    info.setActive((dynamic_cast<CodeEditorWindow*>(activeWindow) != 0));
    break;

    //-----------------//
    //Audio Midi Output//
    //-----------------//

    /*  case CommandIDs::MidiOutOpen:
    {
      juce::StringArray devices = MidiOutPort::getInstance()->getDevices();
      if (data < devices.size())
        info.shortName = devices[data];
      else
        info.shortName = "No MIDI Device";
      info.setTicked(MidiOutPort::getInstance()->isOpen(data));
    }
    break;
    */
  case CommandIDs::MidiOutTest:
    info.shortName = "Test Output";
    info.addDefaultKeypress('T', comk);
    info.setActive(MidiOutPort::getInstance()->isOpen());
    break;
  case CommandIDs::MidiOutHush:
    info.shortName = "Hush";
    info.setActive(MidiOutPort::getInstance()->isOpen());
    break;
  case CommandIDs::MidiOutTuning:
    {
      MidiOutPort* port = MidiOutPort::getInstance();
      info.shortName = port->getTuningName(data);
      info.setTicked(port->isCurrentTuning(data));
    }
    break;
  case CommandIDs::MidiOutDrumTrack:
    info.shortName = "Avoid Drum Track";
    break;
  case CommandIDs::MidiOutPitchBend:
    info.shortName = "Pitch Bend Size";
    break;
  case CommandIDs::MidiOutInstruments:
    info.shortName = "MIDI Instruments...";
    break;
  case CommandIDs::MidiOutFileSettings:
    info.shortName = "Midifile Settings...";
    break;

    //----------------//
    //Audio Midi Input//
    //----------------//

    /*  case CommandIDs::MidiInOpen:
    {
      juce::StringArray devices = juce::MidiInput::getDevices();
      if (data < devices.size())
        info.shortName = devices[data];
      else
        info.shortName = "No MIDI Device";
      info.setTicked(MidiInPort::getInstance()->isOpen(data));
    }
    break;
    */
  case CommandIDs::MidiInTrace:
    info.shortName = "Trace Device Input";
    info.setActive(MidiInPort::getInstance()->isOpen());
    info.setTicked(MidiInPort::getInstance()->isTracing());
    break;
  case CommandIDs::MidiInChannelFilter:
    {
      MidiInPort* p = MidiInPort::getInstance();
      if (data == 16)
      {
        info.shortName = "All";
        if (p->getInputChannelMask() == MidiInPort::AllChannels)
          info.setTicked(true);
      }
      else
      {
        info.shortName = juce::String(data);
        if ((p->getInputChannelMask() != MidiInPort::AllChannels) &&
            p->isInputChannelActive(data))
          info.setTicked(true);
      }
    }
    break;
  case CommandIDs::MidiInOpcodeFilter:
    {
      MidiInPort* p = MidiInPort::getInstance();
      if (data == 7)
      {
        info.shortName = "All";
        if (p->getOpcodeMask() == MidiInPort::AllOpcodes)
          info.setTicked(true);
      }
      else
      {
        info.shortName = MidiOps::toPrettyString(MidiOps::Off + data);
        if ((p->getOpcodeMask() != MidiInPort::AllOpcodes) &&
            p->isOpcodeActive(data))
          info.setTicked(true);
      }
    }
    break;

    //------------//
    //Audio SndLib//
    //------------//

  case CommandIDs::SndLibSrate:
    info.shortName = SrateIDs::toString(data);
    info.setTicked(SrateIDs::toSrate(data) == SndLib::getInstance()->getSrate());
    break;
  case CommandIDs::SndLibChannels:
    info.shortName = ChannelIDs::toString(data);
    info.setTicked(ChannelIDs::toChannels(data) == SndLib::getInstance()->getChannels());
    break;
  case CommandIDs::SndLibAudioFormat:
    {
      juce::String name = AudioFormatIDs::toString(data);
      info.shortName = name;
      info.setTicked(name == SndLib::getInstance()->getAudioFormat());
    }
    break;
  case CommandIDs::SndLibAutoPlay:
    info.shortName = "Auto Play";
    info.setTicked(pref->getBoolProp("SndLibAutoPlay", true));
    break;
  case CommandIDs::SndLibInsDialog:
    info.addDefaultKeypress('I', comk);
    info.shortName = "Instrument Browser..";
    break;

    //------//
    //Csound//
    //------//

  case CommandIDs::CsoundPrefWriteAfter:
    info.shortName = "Write Scorefiles";
    info.setTicked(Csound::getInstance()->getWriteAfter());
    break;
  case CommandIDs::CsoundPrefPlayAfter:
    info.shortName = "Play Scorefiles";
    info.setTicked(Csound::getInstance()->getPlayAfter());
    // can't play unless you first write!
    info.setActive(Csound::getInstance()->getWriteAfter());
    break;
  case CommandIDs::CsoundExportScore:
    info.shortName = "Export Score...";
    info.setActive(!Csound::getInstance()->isScoreEmpty());
    break;
  case CommandIDs::CsoundClearScore:
    info.shortName = "Clear Score";
    info.setActive(!Csound::getInstance()->isScoreEmpty());
    break;
  case CommandIDs::CsoundOpenSettings:
    info.shortName = "Settings...";
    break;

    //---------//
    //Audio OSC//
    //---------//

  case CommandIDs::OscOpen:
    info.shortName = "OSC Connections...";
    break;
  case CommandIDs::OscTraceInput:
    info.shortName = "Trace Input";
    info.setTicked(OpenSoundControl::getInstance()->isTracing());
    break;
  case CommandIDs::OscTestOutput:
    info.shortName = "Test Output";
    info.setActive(OpenSoundControl::getInstance()->getOutputPort());
    break;
  case CommandIDs::AudioSettings:
    info.shortName = "Audio Settings...";
    break;
  case CommandIDs::MidiDeviceSettings:
    info.shortName = "MIDI Connections...";
    info.setActive(true);
    info.addDefaultKeypress('M', comk);
    break;

    //----//
    //Eval//
    //----//

  case CommandIDs::EvalExecute:
    info.shortName = "Execute";
    if (comKeyActive)
      info.addDefaultKeypress(juce::KeyPress::returnKey, comk);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(TextIDs::canExecute(w->getCodeEditor()->getTextType()));
    else
      info.setActive(false);
    break;
  case CommandIDs::EvalExpand:
    info.shortName = "Expand";
    if (comKeyActive)
      info.addDefaultKeypress(juce::KeyPress::returnKey, comk | juce::ModifierKeys::shiftModifier);
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      info.setActive(TextIDs::canExpand(w->getCodeEditor()->getTextType()));
    else
      info.setActive(false);
    break;
  case CommandIDs::EvalAbort:
    info.shortName = "Stop";
    info.addDefaultKeypress('.', comk);
    break;
  case CommandIDs::EvalBacktrace:
    info.shortName = "Show Backtrace on Errors";
    //    info.addDefaultKeypress('B', comk);
    info.setTicked(SchemeThread::getInstance()->showBacktrace());
    break;

    //------//
    //Window//
    //------//

  case CommandIDs::WindowSelect:
    {
      juce::Array<juce::TopLevelWindow*> windows;
      getApplicationWindows(windows);
      int digit = 48 + data + 1;
      if (data < windows.size())
      {
        info.shortName = windows[data]->getName();
      }
      else
      {
        info.shortName = "<Window>";
        info.setActive(false);
      }
      info.addDefaultKeypress(digit, comk);
    }
    break;
  case CommandIDs::WindowPlottingControls:
    if (PlottingControlsWindow::getPlottingControlsWindow() == 0) 
      info.shortName = "Show Plotting Controls";
    else
      info.shortName = "Hide Plotting Controls";
    info.addDefaultKeypress('P',  comk | juce::ModifierKeys::shiftModifier);
    info.setActive(true);
    break;

    //----//
    //Help//
    //----//

  case CommandIDs::HelpManual:
    // names are overridden in menu
    info.shortName = "Help Manual "+ juce::String(data);
    //    if (comKeyActive && (data == 0)) // Common Music Manual
    //      info.addDefaultKeypress('D', comk);
    break;

  case CommandIDs::HelpExport:
    info.shortName = "Export ";
    switch(data)
    {
    case 1: info.shortName <<  "Sal Examples..."; break;
    case 2: info.shortName <<  "Sal Tutorials..."; break;
    case 3: info.shortName <<  "Scheme Examples..."; break;
    case 4: info.shortName <<  "Scheme Examples..."; break;
    case 5: info.shortName <<  "Scheme Extras..."; break;
    default: info.shortName <<  juce::String(data); break;
    }
    break;

  case CommandIDs::HelpWebSite:
    // names are overridden in menu
    info.shortName = "Web Site " + juce::String(data);
    break;

  case CommandIDs::HelpSalExample:
    // names are overridden in menu
    info.shortName = "Sal Example " + juce::String(data);
    break;
  case CommandIDs::HelpSchemeExample:
    // names are overridden in menu
    info.shortName = "Scheme Example " + juce::String(data);
    break;
  case CommandIDs::HelpSalTutorial:
    // names are overridden in menu
    info.shortName = "Sal Tutorial " + juce::String(data);
    break;
  case CommandIDs::HelpSchemeTutorial:
    // names are overridden in menu
    info.shortName = "Scheme Tutorial " + juce::String(data);
    break;
  case CommandIDs::HelpSchemeExtra:
    // names are overridden in menu
    info.shortName = "Scheme Extra " + juce::String(data);
    break;
  case CommandIDs::HelpFomus:
    info.shortName = "Fomus Help";
    break;
  case CommandIDs::HelpDocumentationLookup:
    info.shortName = "Lookup Documentation";
    if (comKeyActive)
      info.addDefaultKeypress('D', comk);
    //   info.setActive(dynamic_cast<CodeEditorWindow*>(activeWindow) != 0);
    break;

  default:
    //      std::cout << "Grace commands: missing info for " << 
    //	CommandIDs::toString(id, true).toUTF8() << "\n";
    break;
  }
}

bool Grace::perform(const juce::ApplicationCommandTarget::InvocationInfo& info)
{
  int comm = CommandIDs::getCommand(info.commandID);
  int data = CommandIDs::getCommandData(info.commandID);
  //std::cout << "In Grace::perform -> comm=" << comm << " data=" << data << "\n";

  switch (comm)
  {

    //---//
    //File//
    //----//

  case CommandIDs::FileNew:
    CodeEditorWindow::newFile();
    break;
  case CommandIDs::FileNewPlotter:
    new PlotWindow(0);
    PlotWindow::openPlottingControlsWindow();
    break;
  case CommandIDs::FileOpen:
    CodeEditorWindow::openFile();
    break;
  case CommandIDs::FileOpenRecent:
    CodeEditorWindow::openFile(Preferences::getInstance()->recentlyOpened.getFile(data));
    break;
  case CommandIDs::FileClearOpenRecent:
    Preferences::getInstance()->recentlyOpened.clear();
    break;
  case CommandIDs::FileClose:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->closeButtonPressed();
    break;
  case CommandIDs::FileSave:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->saveFile(false);
    break;
  case CommandIDs::FileSaveAs:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->saveFile(true);
    break;
  case CommandIDs::FileSaveVersion:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->saveFileVersion();
    break;
  case CommandIDs::FilePlayAudioFile:
    AudioFilePlayer::openAudioFilePlayer(juce::File(), false);
    break;
  case CommandIDs::FilePlayMidiFile:
    MidiFilePlayer::openMidiFilePlayer(juce::File());
    break;
  case CommandIDs::FileLoad:
    SchemeThread::getInstance()->load(juce::File(),true);	
    break;
  case CommandIDs::FileLoadRecent:
    SchemeThread::getInstance()->load(Preferences::getInstance()->recentlyLoaded.getFile(data), true);
    break;
  case CommandIDs::FileClearLoadRecent:
    Preferences::getInstance()->recentlyLoaded.clear();
    break;
  case CommandIDs::FileSetInit:
    {
      juce::File d = juce::File::getCurrentWorkingDirectory();
      juce::FileChooser choose ("Select lisp file to load at startup", d);
      if (choose.browseForFileToOpen())
        Preferences::getInstance()->setStringProp("LispInitFile", choose.getResult().getFullPathName());
    }
    break;
  case CommandIDs::FileClearInit:
    Preferences::getInstance()->setStringProp("LispInitFile", juce::String());
    break;
  case CommandIDs::FileOpenInit:
    CodeEditorWindow::openFile(Preferences::getInstance()->getStringProp("LispInitFile"));
    break;
  case CommandIDs::FileShowDirectory:
    {
      juce::String s ("Working directory: ");
      s << juce::File::getCurrentWorkingDirectory().getFullPathName().quoted() 
        << "\n";
      Console::getInstance()->printOutput(s);
    }
    break;
  case CommandIDs::FileSetDirectory:
    {
      juce::File d = juce::File::getCurrentWorkingDirectory();
      juce::FileChooser choose ("Set Working Directory", d);
      if (choose.browseForDirectory())
      {
        choose.getResult().setAsCurrentWorkingDirectory();
        juce::String s ("Working directory: ");
        s << juce::File::getCurrentWorkingDirectory().getFullPathName().quoted() 
          << "\n";
        Console::getInstance()->printOutput(s);
      }
    }
    break;
  case CommandIDs::FileQuit:
    systemRequestedQuit();
    break;

    //----//
    //Edit//
    //----//

  case CommandIDs::EditUndo:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      w->getCodeEditor()->undo();
      w->getCodeEditor()->isChanged(true);
    }
    break;
  case CommandIDs::EditRedo:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      w->getCodeEditor()->redo();
      w->getCodeEditor()->isChanged(true);
    }
    break;
  case CommandIDs::EditCut:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      w->getCodeEditor()->cutToClipboard();
      w->getCodeEditor()->isChanged(true);
    }
    break;
  case CommandIDs::EditCopy:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->copyToClipboard();
    else if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
      w->getConsole()->copyToClipboard();
    break;
  case CommandIDs::EditPaste:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      w->getCodeEditor()->pasteFromClipboard();
      w->getCodeEditor()->isChanged(true);
    }
    break;
  case CommandIDs::EditDelete:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->insertTextAtCaret(juce::String());
    break;
  case CommandIDs::EditSelectAll:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->selectAll();
    else if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
      w->getConsole()->selectAll();
    break;
  case CommandIDs::EditDeselectAll:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->deselectAll();
    else if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
    {
      w->getConsole()->deselectAll();
    }
    break;
  case CommandIDs::EditFind:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->openFindAndReplaceDialog();
    break;
  case CommandIDs::EditSyntax:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->switchBufferSyntax(data);
    break;
  case CommandIDs::EditDefaultSyntax:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      Preferences::getInstance()->setIntProp("EditorSyntax", w->getCodeEditor()->getTextType());
    break;
  case CommandIDs::EditIndent:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->indent();
    break;
  case CommandIDs::EditParensMatching:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      CodeEditor* b = w->getCodeEditor();
      b->isParensMatching(!b->isParensMatching());
    }
    break;
  case CommandIDs::EditEmacsMode:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      CodeEditor* b = w->getCodeEditor();
      b->isEmacsMode(!b->isEmacsMode()); 
      Preferences::getInstance()->setBoolProp("EditorEmacsMode", b->isEmacsMode());
      w->updateKeyPressesForEditMode();
    }
    break;
            
    //----//
    //View//
    //----//

  case CommandIDs::ViewFontSize:
    if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
      w->getConsole()->setFontSize(data);
    else if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      w->getCodeEditor()->setFontSize(data);
      //      w->resizeForColumnsAndLines();
    }
    break;
  case CommandIDs::ViewDefaultFontSize:
    if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
    {
      int n = w->getConsole()->getFontSize();
      Preferences::getInstance()->setIntProp("ConsoleFontSize", n);
    }    
    else if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      int n = w->getCodeEditor()->getFontSize();
      Preferences::getInstance()->setIntProp("EditorFontSize", n);
    }
    break;
  case CommandIDs::ViewFontBigger:
    if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
    {
      int n = w->getConsole()->getFontSize();
      w->getConsole()->setFontSize(juce::jlimit(10, 64, n + 2));
    }
    else if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      int n = w->getCodeEditor()->getFontSize();
      w->getCodeEditor()->setFontSize(juce::jlimit(10, 64, n + 2));
    }
    break;
  case CommandIDs::ViewFontSmaller:
    if (ConsoleWindow* w = dynamic_cast<ConsoleWindow*>(activeWindow))
    {
      int n = w->getConsole()->getFontSize();
      w->getConsole()->setFontSize(juce::jlimit(10, 64, n - 2));
    }
    else if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      int n = w->getCodeEditor()->getFontSize();
      w->getCodeEditor()->setFontSize(juce::jlimit(10, 64, n - 2));
    }
    break;
  case CommandIDs::ViewLineNumbersVisible:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
    {
      CodeEditor* e = w->getCodeEditor();
      e->setLineGutterVisible(!e->isLineGutterVisible());
    }
    break;
  case CommandIDs::ViewReadCustomComment:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->readCustomComment();
    break;
  case CommandIDs::ViewWriteCustomComment:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->writeCustomComment(true);
    break;
  case CommandIDs::ViewClearConsole:
    Console::getInstance()->clearConsole();
    break;
  case CommandIDs::ViewConsoleBeepOnError:
    Console::getInstance()->setBeepOnError(!Console::getInstance()->getBeepOnError());
    break;


    //--------//
    //Midi Out//
    //--------//

    /*  case CommandIDs::MidiOutOpen:
    if (MidiOutPort::getInstance()->isOpen(data))
      MidiOutPort::getInstance()->close(data);
    else
      MidiOutPort::getInstance()->open(data);
    break;
    */
  case CommandIDs::MidiOutTest:
    MidiOutPort::getInstance()->testOutput();
    break;
  case CommandIDs::MidiOutHush:
    SchemeThread::getInstance()->stop(0, true);
    break;
  case CommandIDs::MidiOutTuning:
    MidiOutPort::getInstance()->setTuning(data);
    break;
  case CommandIDs::MidiOutInstruments:
    MidiOutPort::getInstance()->openInstrumentsDialog();
    break;
  case CommandIDs::MidiOutFileSettings:
    MidiOutPort::getInstance()->openFileSettingsDialog();
    break;

    //-------//
    //Midi In//
    //-------//

    /*  case CommandIDs::MidiInOpen:
    if (MidiInPort::getInstance()->isOpen(data))
      MidiInPort::getInstance()->close(data);
    else
      MidiInPort::getInstance()->open(data);      
    break;
    */
  case CommandIDs::MidiInTrace:
    MidiInPort::getInstance()->toggleTracing();
    break;
  case CommandIDs::MidiInChannelFilter:
    {
      MidiInPort* p = MidiInPort::getInstance();
      if (data == 16)
        if (p->getInputChannelMask() == MidiInPort::AllChannels)
          p->setInputChannelMask(0);
        else
          p->setInputChannelMask(MidiInPort::AllChannels);
      else
        if (p->getInputChannelMask() == MidiInPort::AllChannels)
          p->setInputChannelMask(1 << data);
        else
          p->setInputChannelActive(data, ! p->isInputChannelActive(data));
    }
    break;
  case CommandIDs::MidiInOpcodeFilter:
    {
      MidiInPort* p = MidiInPort::getInstance();
      if (data == 7)
        if (p->getOpcodeMask() == MidiInPort::AllOpcodes)
          p->setOpcodeMask(0);
        else
          p->setOpcodeMask(MidiInPort::AllOpcodes);
      else
        if (p->getOpcodeMask() == MidiInPort::AllOpcodes)
          p->setOpcodeMask(1 << data);
        else
          p->toggleOpcodeActive(data);
    } 
    break;
  case CommandIDs::AudioSettings:
    AudioManager::getInstance()->openAudioSettings();
    break;
  case CommandIDs::MidiDeviceSettings:
    MidiOutPort::getInstance()->openDeviceSettings();
    break;

    //------//
    //Sndlib//
    //------//

  case CommandIDs::SndLibSrate:
    SndLib::getInstance()->setSrate(SrateIDs::toSrate(data));
    break;
  case CommandIDs::SndLibChannels:
    SndLib::getInstance()->setChannels(ChannelIDs::toChannels(data));
    break;
  case CommandIDs::SndLibAudioFormat:
    SndLib::getInstance()->setAudioFormat(AudioFormatIDs::toString(data));
    break;
  case CommandIDs::SndLibAutoPlay:
    if (SndLib::getInstance()->getAutoPlay())
      SndLib::getInstance()->setAutoPlay(false);
    else
      SndLib::getInstance()->setAutoPlay(true);
    break;
  case CommandIDs::SndLibInsDialog:
    SndLib::getInstance()->openInstrumentBrowser();
    break;

    //------//
    //Csound//
    //------//

  case CommandIDs::CsoundPrefWriteAfter:
    Csound::getInstance()->setWriteAfter(!Csound::getInstance()->getWriteAfter());
    break;
  case CommandIDs::CsoundPrefPlayAfter:
    Csound::getInstance()->setPlayAfter(!Csound::getInstance()->getPlayAfter());
    break;
  case CommandIDs::CsoundExportScore:
    Csound::getInstance()->exportScore();
    break;
  case CommandIDs::CsoundClearScore:
    Csound::getInstance()->clearScore();
    break;
  case CommandIDs::CsoundOpenSettings:
    Csound::getInstance()->openSettings();
    break;

    //---//
    //OSC//
    //---//

  case CommandIDs::OscOpen:
    OpenSoundControl::getInstance()->openConnectionDialog();
    break;
  case CommandIDs::OscTraceInput:
    {
      bool a = OpenSoundControl::getInstance()->isTracing();
      OpenSoundControl::getInstance()->setTracing(!a);
    }
    break;
  case CommandIDs::OscTestOutput:
    OpenSoundControl::getInstance()->testOutput();
    break;

    //----//
    //Eval//
    //----//
    
   case CommandIDs::EvalExecute:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->eval(false);
    break;
  case CommandIDs::EvalExpand:
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->eval(true);
    break; 
  case CommandIDs::EvalAbort:
    // Stop all scheme processes
    SchemeThread::getInstance()->stop(0, true);
    // Flush midi output queue
    MidiOutPort::getInstance()->clear();
    // Interrupt any scheme eval
    SchemeThread::getInstance()->setSchemeInterrupt(true);
    break;
  case CommandIDs::EvalBacktrace:
    SchemeThread::getInstance()->setShowBacktrace(!SchemeThread::getInstance()->showBacktrace());
    break;

    //------//
    //Window//
    //------//

  case CommandIDs::WindowSelect:
    {
      juce::Array<juce::TopLevelWindow*> windows;
      getApplicationWindows(windows);
      if (data < windows.size())
      {
        juce::TopLevelWindow* w = windows[data];
        w->grabKeyboardFocus();
        w->toFront(true);  // This will trigger refresh
      }
    }
    break;
  case CommandIDs::WindowPlottingControls:
    if (PlottingControlsWindow* w = PlottingControlsWindow::getPlottingControlsWindow())
      w->closeButtonPressed();
    else
      PlotWindow::openPlottingControlsWindow();
    break;

    //----//
    //Help//
    //----//

  case CommandIDs::HelpManual:
  case CommandIDs::HelpSalExample:
  case CommandIDs::HelpSchemeExample:
  case CommandIDs::HelpSalTutorial:
  case CommandIDs::HelpSchemeTutorial:
  case CommandIDs::HelpSchemeExtra:
  case CommandIDs::HelpWebSite:
    Help::getInstance()->openHelp(info.commandID);
    break;
  case CommandIDs::HelpExport:
    {
      juce::CommandID i;
      switch (data)
      {
      case 1: i = CommandIDs::HelpSalExample; break;
      case 2: i = CommandIDs::HelpSalTutorial; break;
      case 3: i = CommandIDs::HelpSchemeExample; break;
      case 4: i = CommandIDs::HelpSchemeTutorial; break;
      case 5: i = CommandIDs::HelpSchemeExtra; break;      
      default: return true;
      }
      Help::getInstance()->exportFilesToDirectory(i);
    }
    break;
  case CommandIDs::HelpFomus:
#ifdef WITH_FOMUS
    Fomus::getInstance()->documentationWindow();
#endif
    break;
  case CommandIDs::HelpDocumentationLookup:
    // either lookup help or open Cm Reference
    if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(activeWindow))
      w->getCodeEditor()->lookupHelpAtPoint();
    else
      Help::getInstance()->openHelp(CommandIDs::HelpManual + 0);
    break;

  default:
    break;
  }
  return true;
}

