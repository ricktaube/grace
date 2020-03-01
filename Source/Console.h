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
#include "Messages.h"

class Console : public juce::Component, 
                public juce::FileDragAndDropTarget,
                public juce::AsyncUpdater
{
  ///The color of text printout.
  juce::Colour consolePrintoutColor;
  ///The color of error messages.
  juce::Colour consoleErrorColor;
  ///The color of warning messages.
  juce::Colour consoleWarningColor;
  ///The color of return values from scheme.
  juce::Colour consoleValuesColor;
  ///The background color.
  juce::Colour consoleBackgroundColor;
  ///The region highlight color
  juce::Colour consoleHighlightColor;
  ///The color of selected text in the highlighted region
  juce::Colour consoleSelectionColor;
  ///The color of the caret.
  juce::Colour consoleCaretColor;
  ///The color theme UNUSED
  juce::XmlElement* colorTheme;
  juce::CriticalSection lock;
  juce::String prompt;
  /**Two byte value keeps track of region hightlighting across mouse
     down and up; upper byte holds region test for MouseDown, upper
     byte holds region test for MouseUp. If there is a change then the
     Edit menu items get updated.*/
  juce::Range<bool> regionState;
  juce::OwnedArray<AsyncMessage, juce::CriticalSection> messages;
  void mouseDown(const juce::MouseEvent& event);
  void mouseUp(const juce::MouseEvent& event);
  void mouseDoubleClick(const juce::MouseEvent& event);

public:

  Console();
  ~Console();
  static Console* globalInstance;
  static Console* getInstance(void)
  {
    return globalInstance;
  }
  juce::TextEditor* buffer;
  /**Prints string to the console window in the specified color. This
     function is not thread safe.*/
  void display(juce::String string, juce::Colour color);
  void clearConsole();
  void printPrompt(bool trigger = true);
  void setPrompt(juce::String str);
  void printOutput(juce::String str, bool trigger=true);
  void printValues(juce::String str, bool trigger=true); 
  void printWarning(juce::String str, bool trigger=true);
  void printError(juce::String str, bool trigger=true);
  void printPrompt(juce::String str, bool trigger=true);
  void postAsyncMessage(int typ, bool trigger);
  void postAsyncMessage(int typ, juce::String msg, bool trigger);
  void postAsyncMessage(int typ, int msg,  bool trigger);
  void handleAsyncUpdate() ;
  bool isSupportedFileType(juce::String path);
  juce::XmlElement* getColorTheme();
  void setColorTheme(juce::XmlElement* theme);
  juce::Font getFont();
  void setFont(juce::Font font);
  int getFontSize();
  void setFontSize(int size);
  void resized();
  bool getBeepOnError();
  void setBeepOnError(bool b);
  bool isInterestedInFileDrag (const juce::StringArray &files);
  void filesDropped (const juce::StringArray &files, int x, int y);
  void copyToClipboard();
  void selectAll();
  void deselectAll();
//  void paint (juce::Graphics &g);
};

class ConsoleWindow : public juce::DocumentWindow
{
  // David Psenicka
  // This must be here for FOMUS's documentation to work!
  juce::TooltipWindow xxx;

public:
  ConsoleWindow();
  virtual ~ConsoleWindow();
  void closeButtonPressed();
  void activeWindowStatusChanged();
  ///Returns the text buffer of messages printed to the Console window
  juce::TextEditor* getTextBuffer();
  Console* getConsole();
};
