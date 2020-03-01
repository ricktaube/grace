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
#include "Main.h"
#include "Console.h"
#include "Preferences.h"
#include "PlotWindow.h"
#include "CodeEditor.h"
#include "AudioFilePlayer.h"
#include "MidiFilePlayer.h"

Console* Console::globalInstance = 0;

Console::Console()
  : consolePrintoutColor (juce::Colours::antiquewhite),
    consoleErrorColor (juce::Colours::orangered),
    consoleWarningColor (juce::Colours::palegoldenrod),
    consoleValuesColor (juce::Colours::palegreen),
    consoleBackgroundColor (juce::Colours::black),
    consoleHighlightColor (juce::Colour(0xffbebebe)),
    consoleSelectionColor (juce::Colours::white),
    consoleCaretColor (juce::Colours::yellow),
    colorTheme (0),
    prompt (juce::String()),
    regionState (juce::Range<bool>(false, false)),
    buffer (0)
{
}
/* original
    consolePrintoutColor (juce::Colours::lightsalmon),
    consoleErrorColor (juce::Colour(0xffcd0000)),
    consoleWarningColor (juce::Colours::darkorange),
    consoleValuesColor (juce::Colour(0xff00cd00)),
    consoleBackgroundColor (juce::Colours::black),
    consoleHighlightColor (juce::Colour(0xffbebebe)),
    consoleSelectionColor (juce::Colours::white),
    consoleCaretColor (juce::Colours::yellow),
*/
Console::~Console() 
{
  messages.clear();
}

//void Console::paint(juce::Graphics& g)
//{
//  std::cout << "Console paint!\n";
//}

bool Console::isSupportedFileType(juce::String path)
{
  const juce::String supportedfiletypes = ".scm.lisp.sal1.sal2.sal.ins.clm.fms.xml.mid.text.txt";
  int dot = path.lastIndexOf(".");
  return (dot < 0) ? false : supportedfiletypes.contains(path.substring(dot));
}

void Console::setPrompt(juce::String str)
{
  prompt = str;
}

void Console::display(juce::String str, juce::Colour color)
{
  buffer->moveCaretToEnd();
//  buffer->setColour(juce::TextEditor::textColourId, color);
  buffer->setColour(juce::TextEditor::textColourId, consolePrintoutColor);
  // JUCE BUG: single \n does not get added to buffer so use crlf.
  if ((str.length() == 1) && (str[0] == '\n'))
    buffer->insertTextAtCaret("\r\n");
  else
    buffer->insertTextAtCaret(str);
}

void Console::printOutput(juce::String s, bool t)
{
  postAsyncMessage(CommandIDs::ConsolePrintOutput, s, t);
}

void Console::printValues(juce::String s, bool t)
{
  postAsyncMessage(CommandIDs::ConsolePrintValues, s, t);
}

void Console::printWarning(juce::String s, bool t)
{
  postAsyncMessage(CommandIDs::ConsolePrintWarning, s, t);
}

void Console::printError(juce::String s, bool t)
{
  postAsyncMessage(CommandIDs::ConsolePrintError, s, t);
}

void Console::clearConsole()
{
  postAsyncMessage(CommandIDs::ConsoleClearConsole, juce::String(), true);
}

void Console::printPrompt(bool t)
{
  if (prompt.isNotEmpty())
    postAsyncMessage(CommandIDs::ConsolePrintPrompt, juce::String(), t);
}

/*
 * Async message handling
 */

void Console::postAsyncMessage(int typ, juce::String msg, bool trigger)
{
  //  std::cout << "Posting console message " << msg << "\n";
  // messages.lock();
  const juce::ScopedLock lock (messages.getLock());
  messages.add(new AsyncMessage(typ, msg));
  if (trigger)
    triggerAsyncUpdate();
}

void Console::postAsyncMessage(int typ, bool trigger)
{
  //  messages.lockArray();
  messages.add(new AsyncMessage(typ));
  // messages.unlockArray();
  if (trigger)
    triggerAsyncUpdate();
}

void Console::postAsyncMessage(int typ, int msg, bool trigger)
{
  // messages.lockArray();
  messages.add(new AsyncMessage(typ, msg));
  // messages.unlockArray();
  if (trigger)
    triggerAsyncUpdate();
}

void Console::handleAsyncUpdate()
{
  //std::cout << "Console: handling " << messages.size() << " messages\n";
  const juce::ScopedLock lock (messages.getLock());
  int index = 0;
  int limit = 1000;
  int size = messages.size();
  // if we have many pending messages then we very likely have an
  // infinite printing loop of some sort. in this case skip messages
  // in the hope that the message thread can stay alive the user can
  // abort the lisp command that started it.
  if (size > limit)
  {
    juce::String msg ("Warning: more than ");
      msg << limit
          << " messages suddenly posted to Console.  "
          << "Dropping all but the last in case of runaway loop.\n";
    display(msg, consoleWarningColor);
    index = size - 1;
  }

  for ( ; index < size; index++)
  {
    AsyncMessage* msg = messages.getUnchecked(index);
    int cid = msg->type;
    int cmd = CommandIDs::getCommand(cid);
    //    int arg = CommandIDs::getCommandData(cid);
    //    int dat = msg->data;
    switch (cmd) 
    {
    case CommandIDs::ConsolePrintOutput:
      display(msg->text, consolePrintoutColor);
      break;
    case CommandIDs::ConsolePrintValues:
      display(msg->text, consoleValuesColor);
      break;
    case CommandIDs::ConsolePrintWarning:
      display(msg->text, consoleWarningColor);
      break;
    case CommandIDs::ConsolePrintError:
      display(msg->text, consoleErrorColor);
      getTopLevelComponent()->toFront(true); 
      if (getBeepOnError() )
        juce::LookAndFeel::getDefaultLookAndFeel().playAlertSound();
      break;
    case CommandIDs::ConsolePrintPrompt:
      if (prompt != juce::String())
        display(prompt, consoleErrorColor);
      break;
    case CommandIDs::AudioFilePlayer:
    case CommandIDs::MidiFilePlayer:
      {
        juce::String path = msg->text;
        juce::File file;
        if (juce::File::isAbsolutePath(path))
          file = juce::File(path);
        else
          file = juce::File::getCurrentWorkingDirectory().getChildFile(path).getFullPathName();
        if (cmd == CommandIDs::AudioFilePlayer)
          AudioFilePlayer::openAudioFilePlayer(file, true);
        else
          MidiFilePlayer::openMidiFilePlayer(file, true);          
      }
      break;
    case CommandIDs::ConsoleClearConsole :
      buffer->clear();
      break;

    default:
      {
        juce::String text="Unimplemented message: ";
        text << CommandIDs::toString(cmd) << "\n";
        display(text, consoleWarningColor);
      }
      break;
    }
  }
  messages.clear();
}

juce::XmlElement* Console::getColorTheme()
{
  return colorTheme;
}

void Console::setColorTheme(juce::XmlElement* theme)
{
  colorTheme = theme;
  buffer->setColour(juce::TextEditor::backgroundColourId, consoleBackgroundColor);
  buffer->setColour(juce::TextEditor::textColourId, consolePrintoutColor);
  buffer->setColour(juce::TextEditor::highlightColourId, consoleHighlightColor);
  buffer->setColour(juce::TextEditor::highlightedTextColourId, consoleSelectionColor);
  buffer->setColour(juce::CaretComponent::caretColourId, consoleCaretColor);
  buffer->applyFontToAllText(buffer->getFont());
}

int Console::getFontSize()
{
  return (int)buffer->getFont().getHeight();
}

void Console::setFontSize(int size)
{
  juce::Font font = buffer->getFont();
  font.setHeight((float)size);
  buffer->applyFontToAllText(font);
}

juce::Font Console::getFont()
{
  return buffer->getFont();
}

void Console::setFont(juce::Font f)
{
  buffer->setFont(f);
}

void Console::setBeepOnError(bool b)
{
  Preferences::getInstance()->setBoolProp("ConsoleBeepOnError",b);
}

bool Console::getBeepOnError()
{
  return Preferences::getInstance()->getBoolProp("ConsoleBeepOnError", true);
}

bool Console::isInterestedInFileDrag(const juce::StringArray &files)
{
  // return true if all the files are allowed
  for (int i = 0; i < files.size(); i++)
    if (!isSupportedFileType(files[i]))
      return false;
  return true;
}

void Console::filesDropped(const juce::StringArray &files, int x, int y)
{
  for (int i = 0; i < files.size(); i++)
  {
    juce::File file (files[i]);
    juce::String type = file.getFileExtension();
    /**    if (type == ".xml")
      PlotWindow::openXmlFile(file);
    else if (type == ".mid")
    PlotWindow::openMidiFile(file);
    else **/
    {
      // cycle through open editor windows, if the file is already
      // open then re-focus that window else open the file in a new
      // editor window
      for (int j = 0; j < juce::TopLevelWindow::getNumTopLevelWindows(); j++)
      {
        if (CodeEditorWindow* w = dynamic_cast<CodeEditorWindow*>(juce::TopLevelWindow::getTopLevelWindow(j)))
        {
          if (w->sourcefile == file)
            return w->toFront(true);
        }
      }
      CodeEditorWindow::openFile(file);
      Preferences::getInstance()->recentlyOpened.addFile(file);
    }
  }
}

void Console::resized()
{
  buffer->setTopLeftPosition(1, 1);
  buffer->setSize(getWidth() - 2, getHeight() - 2);
}

void Console::copyToClipboard()
{
  buffer->copyToClipboard();
}

void Console::selectAll()
{
  bool wasRegion = !buffer->getHighlightedRegion().isEmpty();
  regionState.setStart(true);
  regionState.setEnd(true);
  buffer->selectAll();
  if (!wasRegion)
  {
    //    std::cout << "selectAll: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }  
}

void Console::deselectAll()
{
  //buffer->deselectAll(); // juce::TextEditor has no deselectAll() !
  bool wasRegion = !buffer->getHighlightedRegion().isEmpty();
  regionState.setStart(false);
  regionState.setEnd(false);
  buffer->setHighlightedRegion(juce::Range<int>::emptyRange(buffer->getCaretPosition()));
  buffer->setCaretPosition(1000000);
  if (wasRegion)
  {
    //    std::cout << "deselectAll: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }
}

void Console::mouseDown(const juce::MouseEvent& event)
{
  bool wasRegionOnUp = regionState.getEnd();
  bool isRegionOnDown = !buffer->getHighlightedRegion().isEmpty();
  if (wasRegionOnUp != isRegionOnDown)
  {
    //    std::cout << "MouseDown: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }
  regionState.setStart(isRegionOnDown);
}

void Console::mouseUp(const juce::MouseEvent& event)
{
  bool wasRegionOnDown = regionState.getStart(); 
  bool isRegionOnUp = !buffer->getHighlightedRegion().isEmpty();
  if (wasRegionOnDown != isRegionOnUp)
  {
    //    std::cout << "MouseUp: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }
  regionState.setEnd(isRegionOnUp);
}

void Console::mouseDoubleClick (const juce::MouseEvent &e)
{
  buffer->mouseDoubleClick(e);
  if (!buffer->getHighlightedRegion().isEmpty())
  {
    regionState.setEnd(true);      
    Grace::getApp().refreshMenuBar();
  }
}

/*=======================================================================*
  Console Window
  *=======================================================================*/

ConsoleWindow::ConsoleWindow ()
  : juce::DocumentWindow (juce::String() , juce::Colour (227,227,227), //juce::Colours::silver,
                          juce::DocumentWindow::allButtons, true )
{
  setUsingNativeTitleBar(true);
  setDropShadowEnabled(true);
  setName(SysInfo::getApplicationName() + " Console");
  Preferences* prefs=Preferences::getInstance();
  // MACMENU
#ifndef JUCE_MAC
  setMenuBar(Grace::getApp().menuBarModel);
#endif
  Console* cons = Console::getInstance();
  cons->buffer = new juce::TextEditor(juce::String()) ;
  // MACMENU
  addMouseListener(cons, true);
  cons->buffer->setMultiLine(true);
  cons->buffer->setScrollToShowCursor(true);
  cons->buffer->setReadOnly(true);
  cons->buffer->setCaretVisible(false);
  cons->buffer->setFont(juce::Font((SysInfo::isMac() ? "Monaco" : juce::Font::getDefaultMonospacedFontName()),
                                   (float)Preferences::getInstance()->getIntProp("ConsoleFontSize", 16),
                                   juce::Font::plain));
  cons->buffer->setVisible(true);
  // add buffer to Console component
  cons->addChildComponent(cons->buffer);
  // set the theme after buffer exists
  cons->setColorTheme(0);
  cons->addKeyListener(Grace::getApp().commandManager.getKeyMappings());
  cons->setWantsKeyboardFocus(true);  
  cons->setVisible(true);
  cons->setSize(450,350);
  //EXTERNAL CONSOLE POINTER IS MANAGED CONTENT OF THE WINDOW
  setContentOwned(cons, true);
  setResizable(true, true); 
  juce::String wstate=prefs->getStringProp("ConsoleState");
  if (wstate.isNotEmpty())
    restoreWindowStateFromString(wstate);
  else
    centreWithSize (566, 350);  // default to Golden Rect :)
  // This has to be at the end or the window appears in the upper left
  // and then jumps to the propoer position.
  setVisible(true);
  setWantsKeyboardFocus(false);  // Console component has it.
}

ConsoleWindow::~ConsoleWindow ()
{
  // if not Mac remove the window's MenuBar
  // MACMENU
#ifndef JUCE_MAC
  setMenuBar(0);
#endif
  getContentComponent()->deleteAllChildren();
}

void ConsoleWindow::closeButtonPressed ()
{
  juce::JUCEApplication::getInstance()->systemRequestedQuit();
}

juce::TextEditor* ConsoleWindow::getTextBuffer()
{
  // returns the textbuffer associated with the console window.
  return (dynamic_cast<Console*>(getContentComponent()))->buffer;
}

Console* ConsoleWindow::getConsole()
{
  // returns the textbuffer associated with the console window.
  return dynamic_cast<Console*>(getContentComponent());
}

void ConsoleWindow::activeWindowStatusChanged()
{
  //MACMENU
  if (isActiveWindow())
  {
    Grace::getApp().setActiveWindow(this);
  }
  else
  {
  }
}

