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

#pragma once

#include "Libraries.h"

class Grace : public juce::JUCEApplication
{
  juce::LookAndFeel* lookAndFeel;
  void initialise (const juce::String& commandLine);
  void shutdown();
  void reset();
  void systemRequestedQuit();
  void anotherInstanceStarted(const juce::String& commandLine);
  const juce::String getApplicationName();
  const juce::String getApplicationVersion();
  bool moreThanOneInstanceAllowed();
  void getAllCommands(juce::Array<juce::CommandID>& commands);
  void getCommandInfo(juce::CommandID id, juce::ApplicationCommandInfo& info);
  bool perform(const juce::ApplicationCommandTarget::InvocationInfo& info);

public:

  struct GraceMenuBarModel : public juce::MenuBarModel
  {
    GraceMenuBarModel() {};
    ~GraceMenuBarModel() {};
    ///JUCE callback to retrieve menu bar names in a string array. Defined in Command.cpp
    juce::StringArray getMenuBarNames();
    ///JUCE callback to populate a menu with menu items in a popup menu. Defined in Command.cpp
    juce::PopupMenu getMenuForIndex(int index, const juce::String& name);
    ///Unused (replaced by commands)
    void menuItemSelected(int ID, int TopLevelMenuIndex) {}
  };

  GraceMenuBarModel* menuBarModel;
  juce::TopLevelWindow* activeWindow;
  //Gather all application (non-tool) windows 
  void getApplicationWindows(juce::Array<juce::TopLevelWindow*>& appWindows);
  juce::File applicationSupportDirectory;
  Grace()
    : lookAndFeel (0),
      menuBarModel (0),
      activeWindow (0),
      applicationSupportDirectory (juce::File())
  {
  }

  ~Grace()
  {
  }

  // these are implemented in Command.cpp
  void setActiveWindow(juce::TopLevelWindow* window);
  void refreshMenuBar();
  juce::ApplicationCommandManager commandManager;
  juce::ApplicationCommandTarget* getNextCommandTarget();
  static Grace& getApp() {return dynamic_cast<Grace&>(*juce::JUCEApplication::getInstance());}
  void openFile(juce::File file=juce::File());
  int getNumUnsavedWindows();
  bool queryUnsavedWindows();
};
