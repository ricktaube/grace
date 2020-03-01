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
#include "Preferences.h"
#include "Syntax.h"
#include "CodeEditor.h"
#include "Console.h"
#include "Scheme.h"
#include "Main.h"
#include "Help.h"

/*=======================================================================*
  CodeEditorWindow
  *=======================================================================*/

CodeEditorWindow::CodeEditorWindow (juce::File file, juce::String text, int synt, juce::String title)
  : juce::DocumentWindow(juce::String(), juce::Colours::white, juce::DocumentWindow::allButtons, true)
{
  // On windows and linux add the global MenuBar to the window (on Mac
  // its at the top of the screen.)
  //MACMENU
#ifndef JUCE_MAC
  setMenuBar(Grace::getApp().menuBarModel);
#endif

  // Hack to make TextEditor text visible on Mac 10.6. 
#if JUCE_MAC && ((! defined(MAC_OS_X_VERSION_10_7)) || MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_7)
  static bool canUseCoreText = juce::SystemStats::getOperatingSystemType() >= juce::SystemStats::MacOSX_10_7;
  if (! canUseCoreText)
  {
    if (juce::ComponentPeer* peer = getPeer())
      peer->setCurrentRenderingEngine(0);
  }
#endif

  Preferences* prefs=Preferences::getInstance();
  if (file.existsAsFile())
  {
    if (synt == TextIDs::Empty)
      synt = TextIDs::fromFileType(file.getFileExtension());
    if (synt < TextIDs::Text || synt > TextIDs::Sal2)
      synt = TextIDs::Text;
    text = file.loadFileAsString();
  }
  else
  {
    if (synt == TextIDs::Empty)
      synt = prefs->getIntProp("EditorSyntax", TextIDs::Lisp);
    file = juce::File();
  }
  sourcefile = file;
  document.setNewLineCharacters("\n");
  document.replaceAllContent(juce::String(text));
  document.setSavePoint();
  document.clearUndoHistory();
  // parse optional first-line buffer customizations comment. we have
  // to parse customizations before the buffer is created since the
  // comment may declare a specific buffer syntax and this cannot be
  // altered after the buffer is created. if no comment then customs
  // will be null.
  juce::XmlElement* customs = getCustomizations();
  if (customs && customs->hasAttribute("syntax:"))
    synt = customs->getIntAttribute("syntax:"); 
  Syntax* syntax;
  switch (synt)
  {
  case TextIDs::Text: syntax = TextSyntax::getInstance(); break;
  case TextIDs::Lisp: syntax = LispSyntax::getInstance(); break;
  case TextIDs::Sal1: syntax = SalSyntax::getInstance(); break;
  case TextIDs::Sal2: syntax = Sal2Syntax::getInstance(); break;
  default: syntax = TextSyntax::getInstance(); break;
  }
  // create the code buffer and add it to the content component (the
  // buffer is not the content component so the window can contain
  // other components eg a mode line, toolbar, etc)
  CodeEditor* buffer = new CodeEditor(document, syntax, customs);
  // create the window's content component 
  EditorComponent* edcomp = new EditorComponent();
  edcomp->setSize(buffer->getWidth(), buffer->getHeight());
  // MACMENU (listener to update menubar if region changes)
  ////  addMouseListener(edcomp, true); 
  setContentOwned(edcomp, true);
  // now set window and content component's size to buffer's optimum
  // size before adding the buffer (so buffer doesnt get resized by
  // the window sizing)
  centreWithSize(juce::jmin(800, buffer->getWidth()), 
                 juce::jmin(800, buffer->getHeight()));
  // add the buffer to the content component
  edcomp->setCodeEditor(buffer);
  // add customization comment to new empty buffers (???)
  if (text.isEmpty())
  {
    writeCustomComment(false);
    buffer->isChanged(false);
  }
  setWantsKeyboardFocus(false); // buffer has focus
  setWindowTitle(title);
  setResizable(true, true); 
  setUsingNativeTitleBar(true);
  setDropShadowEnabled(true);
  if (customs) delete customs;
  WindowTypes::setWindowType(this, WindowTypes::CodeEditor);
  setVisible(true);
}

CodeEditorWindow::~CodeEditorWindow ()
{
  // deleteing an zeroing the buffer first seems to take care of the
  // asserts in juce_amalgamated.
#ifndef JUCE_MAC
  setMenuBar(0);
#endif
  EditorComponent* comp=(EditorComponent*)getContentComponent();
  comp->deleteCodeEditor();
}

bool CodeEditorWindow::hasUnsavedChanges()
{
  return getCodeEditor()->isChanged();
}

void CodeEditorWindow::closeButtonPressed ()
{
  if (getCodeEditor()->isChanged())
  {
    int v = juce::AlertWindow::showYesNoCancelBox(juce::AlertWindow::QuestionIcon,
                                                  "Close",
                                                  "Save changes before closing?",
                                                  "Save and Close",
                                                  "Close Without Saving",
                                                  "Cancel");
    if (v == 0)
      return;
    if (v == 1)
      saveFile(false);
  }
  if (Grace::getApp().activeWindow == this)
  {
    //    std::cout << "CodeEditorWindow: clearing activeWindow before deleteing\n";
    Grace::getApp().setActiveWindow(0);
  }
  delete this;
}

void CodeEditorWindow::activeWindowStatusChanged()
{
  // MACMENU
  if (isActiveWindow())
  {
    Grace::getApp().setActiveWindow(this);
  }
  else
  {
  }
}

//
///  Implementation
//

CodeEditor* CodeEditorWindow::getCodeEditor()
{
  return ((EditorComponent*)getContentComponent())->getCodeEditor();
}

void CodeEditorWindow::updateKeyPressesForEditMode()
{
}

void CodeEditorWindow::switchBufferSyntax(int newtype)
{
  if (getCodeEditor()->isTextType(newtype))
    return;
  juce::XmlElement* customs = getCustomizations();  // buffer customizations or null
  EditorComponent* comp = (EditorComponent*)getContentComponent();

  // remove and delete current buffer.
  comp->deleteCodeEditor();
  // add new buffer
  Syntax* syntax=0;
  switch (newtype)
  {
  case TextIDs::Text: syntax = TextSyntax::getInstance(); break;
  case TextIDs::Lisp: syntax = LispSyntax::getInstance(); break;
  case TextIDs::Sal1: syntax = SalSyntax::getInstance(); break;
  case TextIDs::Sal2: syntax = Sal2Syntax::getInstance(); break;
  default: syntax=TextSyntax::getInstance(); break;
  }
  // pass in any existing customizations, the buffer does NOT look at
  // the syntax anymore
  CodeEditor* buffer=new CodeEditor(document, syntax, customs);
  // maintain current size of window.
  buffer->setSize(comp->getWidth(), comp->getHeight());
  comp->setCodeEditor(buffer);
  if (customs)
  {
    writeCustomComment(false); // update comment with new syntax
    delete customs;
  }
  setWindowTitle();
}

void CodeEditorWindow::resizeForColumnsAndLines()
{
  setSize(juce::jmin(800, getCodeEditor()->getWidth()), //28
          juce::jmin(800, getCodeEditor()->getHeight()) );
}

void CodeEditorWindow::setWindowTitle(juce::String title)
{
  if (title.isEmpty())
  {
    if (sourcefile == juce::File())
    {
      int num = WindowTypes::getHighestUntitledWindowOfType(WindowTypes::CodeEditor);
      if (num == 0)
      {
        title = "Untitled";
      }
      else
      {
        title << "Untitled " << juce::String(num + 1);
      }
    }
    else
      title = sourcefile.getFileName();
  }
  juce::String ext = title.fromLastOccurrenceOf(".", false, true);
  TextID type = getCodeEditor()->getTextType();
  // if the filename doesnt have an extension, or the extension isnt
  // recognized, then display the buffer syntax in the title
  if (TextIDs::fromFileType(ext) != type)
    title << " (" << TextIDs::toString(type) << ")";
  setName(title);
}

juce::File CodeEditorWindow::getSourceFileDirectory()
{
  if (sourcefile.existsAsFile())
    return sourcefile.getParentDirectory();
  return juce::File::getCurrentWorkingDirectory();
}

void CodeEditorWindow::openFile(juce::File file)
{
  if (file==juce::File() || file.isDirectory())
  {
    juce::FileChooser choose ("Open File",
                              (file==juce::File()) ? juce::File::getCurrentWorkingDirectory() : file,
                              juce::String(), true);
    if (choose.browseForFileToOpen())
      file=choose.getResult();
    else return;
  }
  new CodeEditorWindow(file);
  Preferences::getInstance()->recentlyOpened.addFile(file);
}

void CodeEditorWindow::newFile(juce::String title, int synt, juce::String content)
{
  new CodeEditorWindow(juce::File(), content, synt, title);
}

void CodeEditorWindow::saveFile(bool saveas)
{
  bool exists=sourcefile.existsAsFile();
  // get file name if its an new buffer or its a Save As
  if (saveas || !exists)
  {
    juce::File saveto ((exists) ? sourcefile.getParentDirectory() : juce::File::getCurrentWorkingDirectory());
    juce::FileChooser choose ("Save File", saveto, "*.*", true);
    if (choose.browseForFileToSave(true))
      sourcefile=choose.getResult();
    else
      return;
  }
  // otherwise abort if the code buffer isnt changed
  else if (!getCodeEditor()->isChanged())
  {
    //std::cout << "aborting save, buffer not changed\n";
    return;
  }
  // save the file
  if (sourcefile.replaceWithText(document.getAllContent()))
  {
    getCodeEditor()->isChanged(false);
    document.setSavePoint();
    if (saveas) document.clearUndoHistory(); // SHOULD I DO THIS?
    setWindowTitle();
    Preferences::getInstance()->recentlyOpened.addFile(sourcefile);
  }
  else
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon, "Save File", 
                                      "Error saving " + sourcefile.getFullPathName() + ". File not saved.");
}

// save current buffer contents to a version of file and never
// overwriting a existing version. the buffer must already exist as a
// file.

void CodeEditorWindow::saveFileVersion()
{
  juce::File clone;
  int vers=1;
  while (true)
  {
    clone=sourcefile.getSiblingFile(sourcefile.getFileNameWithoutExtension() + 
                                    "-" +
                                    juce::String(vers) +
                                    sourcefile.getFileExtension());
    if (!clone.existsAsFile())
      break;
    vers++;
  }
  // save the buffer contents to the version. dont clear the changed
  // bit since buffer is still the original file.
  if (clone.replaceWithText(document.getAllContent()))
  {
    juce::String message ("Editor contents saved as ");
    message << clone.getFullPathName() << "\n";
    Console::getInstance()->printOutput(message, true);
  }
  else
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon, "Save File Version", 
                                      "Error saving file version, editor contents not saved.");
}

bool CodeEditorWindow::isCustomComment ()
{
  // true if first line in document is a comment line starting with -*-
  // example:  ;; -*- syntax: lisp, theme: "clarity and beauty", -*-
  juce::String line = document.getLine(0);
  //std::cout << "isCustomComment(), first line='" << line.toUTF8() << "'\n";

  if (line.isEmpty()) return false;
  int len = line.length();
  int pos = 0;
  while (pos<len && juce::CharacterFunctions::isWhitespace(line[pos])) pos++;
  if (pos == len || line[pos] != ';') return false;
  while (pos < len && line[pos] == ';') pos++;
  while (pos < len && juce::CharacterFunctions::isWhitespace(line[pos])) pos++;
  if (pos == len || line.substring(pos, pos + 3) != "-*-") return false;
  return true;
}

juce::XmlElement* CodeEditorWindow::getCustomizations ()
{
  // parse customization comment into xml, return null if there is
  // none. make sure you delete the xlm element after you use it!
  if (!isCustomComment())
    return 0;
  juce::String line = document.getLine(0).
    fromFirstOccurrenceOf("-*-", false, false).
    upToLastOccurrenceOf("-*-", false, false).trim();
  juce::StringArray customizations;
  customizations.addTokens(line, ";", "\"");
  customizations.trim(); // trim white from each customization
  customizations.removeEmptyStrings();
  if (customizations.size() == 1)
    return 0;
  //  for (int i = 0; i < customizations.size(); i++) 
  //    std::cout << "customizations["<<i<<"]='" << customizations[i] << "'\n";
  juce::XmlElement* xml = new juce::XmlElement("customizations");
  for (int i = 0; i < customizations.size(); i++)
  {
    juce::StringArray tokens;
    tokens.addTokens(customizations[i], ": ", "\"");
    tokens.trim(); // trim white from each token
    tokens.removeEmptyStrings();
    if (tokens.size() != 2)
      continue;
    //    for (int i = 0; i < tokens.size(); i++) 
    //      std::cout << "tokens["<<i<<"]='" << tokens[i] << "'\n";

    // customizations are case insensitive
    if (tokens[0].equalsIgnoreCase("syntax"))
    {
      int n = TextIDs::fromString(tokens[1]);
      if (TextIDs::Text <= n && n <= TextIDs::Sal2)
        xml->setAttribute("syntax", n);
    }
    else if (tokens[0].equalsIgnoreCase("font-size" ))
    {
      float n = tokens[1].getFloatValue();
      if (8.0 <= n && n <= 80.0)
        xml->setAttribute("font-size" , floor(n));
    }
    else if (tokens[0].equalsIgnoreCase("theme"))
    {
      ////      juce::String s=tokens[1];
      ////      if (s.isQuotedString())
      ////        s=s.unquoted();
      ////      if (Preferences::getInstance()->getColorTheme(s))
      ////        xml->setAttribute("theme", s);
    }
    else if (tokens[0].equalsIgnoreCase("columns"))
    {
      int n = tokens[1].getIntValue();
      if (18 <= n && n < 144)
        xml->setAttribute("columns", n);
    }
    else if (tokens[0].equalsIgnoreCase("lines"))
    {
      int n = tokens[1].getIntValue();
      if (10 <= n && n <= 120)
        xml->setAttribute("lines", n);
    }
    else if (tokens[0].equalsIgnoreCase("line-numbers"))
    {
      bool b = (tokens[1].equalsIgnoreCase("yes") || tokens[1].equalsIgnoreCase("true"));
      xml->setAttribute("line-numbers", (b) ? 1 : 0);
    }
  }

  if (xml->getNumAttributes() == 0) 
  {
    delete xml;
    xml=0;
  }
  return xml;
}

void CodeEditorWindow::readCustomComment()
{
  juce::XmlElement* xml = getCustomizations();
  if (!xml)
    return;
  CodeEditor* buff = getCodeEditor();
  //std::cout << "Customizations:\n";
  for (int i = 0; i < xml->getNumAttributes(); i++)
  {
    juce::String name = xml->getAttributeName(i);
    std::cout << "  " << name << " " << xml->getAttributeValue(i) << "\n";
    if (name == "syntax")
      ;
    else if (name == "theme")
      buff->setColorTheme(0);
    else if (name == "font-size")
      buff->setFontSize(xml->getIntAttribute(name));
    else if (name == "columns")
      buff->setColumns(xml->getIntAttribute(name));
    else if (name == "lines")
      buff->setLines(xml->getIntAttribute(name));
    else if (name == "line-numbers")
      buff->setLineGutterVisible(xml->getIntAttribute(name) == 1);
  }
  delete xml;
}

void CodeEditorWindow::writeCustomComment(bool select)
{
  juce::String custom (";;; -*-");
  CodeEditor* buffer = getCodeEditor();
  custom << " syntax: " << TextIDs::toString(getCodeEditor()->getTextType()) << ";";
  custom << " font-size: " << buffer->getFontSize() << ";"; 
  custom << " line-numbers: " << ((buffer->isLineGutterVisible()) ? "yes" : "no") << ";";
  //custom << " theme: "<< buffer->getColorTheme()->getStringAttribute("name").quoted() << ";";
  int n = buffer->getColumns();
  if (n != 72) 
    custom << " columns: " << n << ";";
  n = buffer->getLines();
  if (n != 30)
    custom << " lines: " << n << ";";
  custom << " -*-" << document.getNewLineCharacters();
  juce::CodeDocument::Position a ((const juce::CodeDocument&)document, 0);
  if (isCustomComment())  // delete existing comment line including eol
  {
    juce::CodeDocument::Position e ((const juce::CodeDocument&)document, 1, 0); //0, INT_MAX);
    document.deleteSection(a,e);
  }
  buffer->moveCaretToTop(false);

  //std::cout << "after delete, looking at (" << buffer->getCaretPos().getPosition() << "): '" << document.getTextBetween(buffer->getCaretPos(), buffer->getCaretPos().movedBy(1)).toUTF8() << "'\n";
  /*
  if (buffer->isEOB())
    custom << document.getNewLineCharacters();
  if (!juce::CharacterFunctions::isWhitespace(a.getCharacter())) //.movedByLines(1)
    custom << document.getNewLineCharacters();
  */
  //  std::cout << "custom='" << custom << "'\n";
  buffer->insertTextAtCaret(custom);
  a.setLineAndIndex(1, 0);
  buffer->moveCaretTo(a, false);
}

CodeEditorWindow* CodeEditorWindow::getFocusCodeEditor()
{
  for (int i=0; i<juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    juce::TopLevelWindow* w=juce::TopLevelWindow::getTopLevelWindow(i);
    juce::NamedValueSet& s=w->getProperties();
    if (s[ juce::Identifier("FocusEditor") ]==juce::var(true))
      return (CodeEditorWindow*)w;
  }
  return (CodeEditorWindow*)NULL;
}

/*=======================================================================*
  Find and Replace Window
  *=======================================================================*/

class FindAndReplaceDialog  : public juce::Component,
public juce::Button::Listener,
public juce::TextEditor::Listener
{
public:
  FindAndReplaceDialog ();
  ~FindAndReplaceDialog();
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  void textEditorReturnKeyPressed(juce::TextEditor& editor);
  void textEditorTextChanged(juce::TextEditor& editor){}
  void textEditorEscapeKeyPressed(juce::TextEditor& editor){}
  void textEditorFocusLost(juce::TextEditor& editor){}
private:
  juce::Label* label1;
  juce::TextEditor* textEditor1;
  juce::Label* label2;
  juce::TextEditor* textEditor2;
  juce::TextButton* textButton1;
  juce::TextButton* textButton2;
  juce::TextButton* textButton3;
  juce::TextButton* textButton4;
  juce::TextButton* textButton5;
};

class FindAndReplaceWindow : public juce::DocumentWindow
{
public:
  void closeButtonPressed() {delete this;}
  FindAndReplaceWindow(FindAndReplaceDialog* comp) :
  juce::DocumentWindow("Find & Replace", 
                       ColorThemeIDs::getWindowBackgroundColor(),
                       juce::DocumentWindow::closeButton)
  {
    setResizable(false, false); 
    setUsingNativeTitleBar(true);
    setDropShadowEnabled(true);
    WindowTypes::setWindowType(this, WindowTypes::FindAndReplace);
    comp->setVisible(true);
    centreWithSize(comp->getWidth(),comp->getHeight());
    //    setContentComponent(comp);
    setContentOwned(comp,true);
    setVisible(true);
  }
  ~FindAndReplaceWindow() {}
};

FindAndReplaceDialog::FindAndReplaceDialog ()
  : label1 (0),
    textEditor1 (0),
    label2 (0),
    textEditor2 (0),
    textButton1 (0),
    textButton2 (0),
    textButton3 (0),
    textButton4 (0),
    textButton5 (0)
{
  addAndMakeVisible (label1 = new juce::Label (juce::String(), "Find:"));
  label1->setFont (juce::Font (15.0000f, juce::Font::plain));
  label1->setJustificationType (juce::Justification::centredRight);
  label1->setEditable (false, false, false);
  label1->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  label1->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  
  addAndMakeVisible (textEditor1 = new juce::TextEditor (juce::String()));
  textEditor1->setMultiLine (false);
  textEditor1->setReturnKeyStartsNewLine (false);
  textEditor1->setReadOnly (false);
  textEditor1->setScrollbarsShown (true);
  textEditor1->setCaretVisible (true);
  textEditor1->setPopupMenuEnabled (true);
  textEditor1->setText (juce::String());
  textEditor1->setWantsKeyboardFocus(true);
  textEditor1->addListener(this);

  addAndMakeVisible (label2 = new juce::Label (juce::String(), "Replace:"));
  label2->setFont (juce::Font (15.0000f, juce::Font::plain));
  label2->setJustificationType (juce::Justification::centredRight);
  label2->setEditable (false, false, false);
  label2->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  label2->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  
  addAndMakeVisible (textEditor2 = new juce::TextEditor (juce::String()));
  textEditor2->setMultiLine (false);
  textEditor2->setReturnKeyStartsNewLine (false);
  textEditor2->setReadOnly (false);
  textEditor2->setScrollbarsShown (true);
  textEditor2->setCaretVisible (true);
  textEditor2->setPopupMenuEnabled (true);
  textEditor2->setText (juce::String());
  textEditor2->addListener(this);
  
  addAndMakeVisible (textButton1 = new juce::TextButton (juce::String()));
  textButton1->setButtonText ("Replace All");
  textButton1->setConnectedEdges (juce::Button::ConnectedOnRight);
  textButton1->addListener (this);
  
  addAndMakeVisible (textButton2 = new juce::TextButton (juce::String()));
  textButton2->setButtonText ("Replace");
  textButton2->setConnectedEdges (juce::Button::ConnectedOnLeft | juce::Button::ConnectedOnRight);
  textButton2->addListener (this);
  
  addAndMakeVisible (textButton3 = new juce::TextButton (juce::String()));
  textButton3->setButtonText ("Replace & Find");
  textButton3->setConnectedEdges (juce::Button::ConnectedOnLeft | juce::Button::ConnectedOnRight);
  textButton3->addListener (this);
  
  addAndMakeVisible (textButton4 = new juce::TextButton (juce::String()));
  textButton4->setButtonText ("Previous");
  textButton4->setConnectedEdges (juce::Button::ConnectedOnLeft | juce::Button::ConnectedOnRight);
  textButton4->addListener (this);
  
  addAndMakeVisible (textButton5 = new juce::TextButton (juce::String()));
  textButton5->setButtonText ("Next");
  textButton5->setConnectedEdges (juce::Button::ConnectedOnLeft);
  textButton5->addListener (this);
  
  setSize (414, 104);
}

FindAndReplaceDialog::~FindAndReplaceDialog()
{
  deleteAndZero (label1);
  deleteAndZero (textEditor1);
  deleteAndZero (label2);
  deleteAndZero (textEditor2);
  deleteAndZero (textButton1);
  deleteAndZero (textButton2);
  deleteAndZero (textButton3);
  deleteAndZero (textButton4);
  deleteAndZero (textButton5);
}

void FindAndReplaceDialog::resized()
{
  label1->setBounds (8, 8, 56, 24);
  textEditor1->setBounds (72, 8, 336, 24);
  label2->setBounds (8, 40, 56, 24);
  textEditor2->setBounds (72, 40, 336, 24);
  textButton1->setBounds (8, 72, 80, 24);
  textButton2->setBounds (88, 72, 80, 24);
  textButton3->setBounds (168, 72, 96, 24);
  textButton4->setBounds (264, 72, 64, 24);
  textButton5->setBounds (328, 72, 80, 24);
}

void FindAndReplaceDialog::textEditorReturnKeyPressed (juce::TextEditor& editor)
{
  if (&editor == textEditor1)
  {
    if (!editor.isEmpty()) 
      textButton5->triggerClick();
  }
  //else if (&editor == textEditor2) {}
}

void FindAndReplaceDialog::buttonClicked (juce::Button* buttonThatWasClicked)
{
  CodeEditorWindow* win=CodeEditorWindow::getFocusCodeEditor();

  if (!win)  // no editor open
  {
    //      PlatformUtilities::beep();
    return;
  }
  CodeEditor* buf=win->getCodeEditor();
  // no find string
  juce::String str=textEditor1->getText();
  if (str.isEmpty())
  {
    //      PlatformUtilities::beep();
    textEditor1->grabKeyboardFocus();
    return;
  }

  if (buttonThatWasClicked == textButton1)       // Replace All
  {
    if (!buf->replaceAll(str, textEditor2->getText()))
    {}//PlatformUtilities::beep();
      
  }
  else if (buttonThatWasClicked == textButton2)  // Replace
  {
    if (!buf->replace(textEditor2->getText()))
    {}//PlatformUtilities::beep();
  }
  else if (buttonThatWasClicked == textButton3) // Find & Replace
  {
    if (!buf->replaceAndFind(str, textEditor2->getText()))
    {}//PlatformUtilities::beep();	
  }
  else if (buttonThatWasClicked == textButton4) // Previous
  {
    if (!buf->findPrevious(str))
    {}//PlatformUtilities::beep();
  }
  else if (buttonThatWasClicked == textButton5) // Next
  {
    if (!buf->findNext(str))
    {}//PlatformUtilities::beep();
  }
}

void CodeEditorWindow::openFindAndReplaceDialog()
{
  const juce::String title = "Find & Replace";
  for (int i = 0; i < getNumTopLevelWindows(); i++)
  {
    juce::TopLevelWindow* w = juce::TopLevelWindow::getTopLevelWindow(i);
    if (WindowTypes::isWindowType(w, WindowTypes::FindAndReplace))
    {
      w->grabKeyboardFocus();
      w->toFront(true);
      return;
    }
  }
  FindAndReplaceDialog* d = new FindAndReplaceDialog();
  d->grabKeyboardFocus();
  new FindAndReplaceWindow(d);
}

/*=======================================================================*
  CodeEditor
  *=======================================================================*/

CodeEditor::CodeEditor(juce::CodeDocument& doc, Syntax* tokenizer,
                       juce::XmlElement* customs)
  : juce::CodeEditorComponent(doc, tokenizer),
    document (doc),
    matchpos (-1),
    fontsize (16),
    tabwidth (2),
    columns (juce::jlimit(72, 100, doc.getMaximumLineLength())),
    lines (30),
    changed (false),
    parensmatching (true),
    colorTheme (0),
    regionState(juce::Range<bool>(false, false))
{
  Preferences* prefs = Preferences::getInstance();
  syntax = tokenizer;
  fontsize = prefs->getIntProp("EditorFontSize", 16);
  emacsmode = prefs->getBoolProp("EditorEmacsMode", SysInfo::isMac());
  setLineGutterVisible(false);
  if (customs) // file has customizations
  {
    fontsize = (int)customs->getDoubleAttribute("font-size:", fontsize);
    columns = customs->getIntAttribute("columns:", columns);
    lines = customs->getIntAttribute("lines:", lines);
    if (customs->hasAttribute("theme:"))
      colorTheme = 0;////prefs->getColorTheme(customs->getStringAttribute("theme:"));
    setLineGutterVisible(customs->getBoolAttribute("line-numbers:"));
  }
  juce::Font mono (juce::Font::getDefaultMonospacedFontName(),
                   (float)fontsize, juce::Font::plain);
  setFont(mono);
  setTabSize(tabwidth, true);
  // view width is width of columized text plus width of yscroller and
  // line number gutter (which seems to be 2x scrollbar)
  //  std::cout << "doc=" << document.getMaximumLineLength() << " col=" << columns << "\n";
  //  int w = (getCharWidth() * columns) + (getScrollbarThickness() * 2);
  int w = (int)(getCharWidth() * columns) + 56;
  int h = getLineHeight() * lines;
  //  setSize((getCharWidth() * columns)+16+4, (getLineHeight() * lines)+16+4);
  setSize(w, h);
  setWantsKeyboardFocus(true);
  setVisible(true);
}

CodeEditor::~CodeEditor()
{
}

void CodeEditor::insertTextAtCaret (const juce::String& textToInsert)
{
  bool add=true;
  bool match=false;
  if (isParensMatchingActive()) 
    stopParensMatching();
  if (textToInsert.length()==1)
  {
    const juce::juce_wchar c=textToInsert[0];
    const bool e=isEmacsMode();
    //    std::cout << "CodeEditor::insertTextAtCaret='" << c << "'\n"; 

    add=false;
    switch (c)
    {
    case ')' :
    case '}' :
    case ']' :
      add=true;
      match=(syntax->isCloseChar(c) && isParensMatching()) ;
      break;

      // unfortunately some meta key combinations don't get passed to
      // KeyPressed() but are passed directly to this method instead so
      // we have to explicitly test for these meta commands here...

    case 8747: // Meta-b
      if (e) moveWordBackward();
      break;
    case 231:  // Meta-c
      if (e) changeCase(2);
      break;
    case 8706: // Meta-d
      if (e) killWordForward();
      break;
    case 402:  // Meta-f
      if (e) moveWordForward();
      break;
    case 172:  // Meta-l
      if (e) changeCase(0);
      break;
    case 168:  // Meta-u
      if (e) changeCase(1);
      break;
    case 8730: // Meta-v
      if (e) movePageBackward();
      break;
    case 160:  // Meta-spaceKey
      if (e) killWhite();  
      break;
    default:
      // otherwise only add newlines or standard ascii chars to the buffer!
      add=(c==10)||(c>=32 && c<=126);
      if (!add)
        std::cout << "Refusing to insert char '" << c << "'\n";
      break;
    }
  }
  if (add)
  {
    juce::CodeEditorComponent::insertTextAtCaret(textToInsert);        
    if (match)
      matchParens();
    isChanged(true);
  }
}

bool CodeEditor::keyPressed (const juce::KeyPress& key)
{
  const int ctrl = juce::ModifierKeys::ctrlModifier;
  const int meta = juce::ModifierKeys::altModifier;
  const int both = (juce::ModifierKeys::ctrlModifier | juce::ModifierKeys::altModifier);
  const bool emacs=isEmacsMode();

  //  std::cout << "CodeEditor::keyPressed key=" << key.getTextDescription() << "\n";
  if (isParensMatchingActive()) 
    stopParensMatching();

  if (key == juce::KeyPress('\t'))
    indent();
  // Control commands
  else if (emacs && key == juce::KeyPress('a', ctrl, 0))
    gotoBOL();
  else if (emacs && key == juce::KeyPress('b', ctrl, 0))
    moveCharBackward();
  else if (emacs && key == juce::KeyPress('d', ctrl, 0))
    killCharForward();
  else if (emacs && key == juce::KeyPress('e', ctrl, 0))
  {
    if (prevkey == juce::KeyPress('x', ctrl, 0))
      eval(false);
    else
      gotoEOL();
  }
  else if (emacs && key == juce::KeyPress('f', ctrl, 0))
  {
    if (prevkey == juce::KeyPress('x', ctrl, 0))
    {
      CodeEditorWindow* win=(CodeEditorWindow *)getTopLevelComponent();
      win->openFile(win->getSourceFileDirectory());
    }
    else
      moveCharForward();
  }
  else if (emacs && key == juce::KeyPress('h', ctrl, 0))
    lookupHelpAtPoint();
  else if (emacs && key == juce::KeyPress('k', ctrl, 0))
    killLine();
  else if (emacs && key == juce::KeyPress('n', ctrl, 0))
    moveLineForward();
  else if (emacs && key == juce::KeyPress('o', ctrl, 0))
    openLine();
  else if (emacs && key == juce::KeyPress('p', ctrl, 0))
    moveLineBackward();
  else if (emacs && key == juce::KeyPress('s', ctrl, 0)
           && (prevkey==juce::KeyPress('x', ctrl, 0)))
    ((CodeEditorWindow *)getTopLevelComponent())->saveFile(false);
  else if (emacs && key == juce::KeyPress('v', ctrl, 0))
    movePageForward();
  else if (emacs && key == juce::KeyPress('y', ctrl, 0))
    pasteFromClipboard();
  else if (emacs && key == juce::KeyPress('c', ctrl, 0))
  {
    if (prevkey == juce::KeyPress('c', ctrl, 0))
      SchemeThread::getInstance()->setSchemeInterrupt(true);
    else
      ;
  }
  else if (emacs && key == juce::KeyPress('x', ctrl, 0))
    ;  
  // Meta commands  
  else if (emacs && key == juce::KeyPress('b', meta, 0))
    moveWordBackward();
  else if (emacs && key == juce::KeyPress('c', meta, 0))
    changeCase(2);
  else if (emacs && key == juce::KeyPress('d', meta, 0))
    killWordForward();
  else if (emacs && key == juce::KeyPress('f', meta, 0))
    moveWordForward();
  else if (emacs && key == juce::KeyPress('l', meta, 0))
    changeCase(0);
  else if (emacs && key == juce::KeyPress('u', meta, 0))
    changeCase(1);
  else if (emacs && key == juce::KeyPress('v', meta, 0))
    movePageBackward();
  else if (emacs && key == juce::KeyPress(juce::KeyPress::backspaceKey, meta, 0))
    killWordBackward();
  else if (emacs && key == juce::KeyPress(juce::KeyPress::spaceKey, meta, 0))
    killWhite();  
  // Control-Meta commands  
  else if (emacs && key == juce::KeyPress('b', both, 0))
    moveExprBackward();  
  else if (emacs && key == juce::KeyPress('f', both, 0))
    moveExprForward();
  else if (emacs && key == juce::KeyPress('k', both, 0))
    killExprForward();
  // NON emacs additions
  else if (SysInfo::isMac() && key.isKeyCode(juce::KeyPress::rightKey))
  {
    const bool moveInWholeWordSteps = key.getModifiers().isCtrlDown() || key.getModifiers().isAltDown();
    if (key.getModifiers().isCommandDown())
      moveCaretToEndOfLine(key.getModifiers().isShiftDown());
    else
      moveCaretRight(moveInWholeWordSteps, key.getModifiers().isShiftDown());
  }
  else if (SysInfo::isMac() && key.isKeyCode(juce::KeyPress::leftKey))
  {
    const bool moveInWholeWordSteps = key.getModifiers().isCtrlDown() || key.getModifiers().isAltDown();
    if (key.getModifiers().isCommandDown())
      moveCaretToStartOfLine(key.getModifiers().isShiftDown());
    else
      moveCaretLeft(moveInWholeWordSteps, key.getModifiers().isShiftDown());
  }
  else 
  {
    // else try other keypress handlers in this order: (1) Juce
    // CodeEditorComponent (2) the CodeEditorWindow and (3) the
    // global command manager
    prevkey = juce::KeyPress(key);
    if (juce::CodeEditorComponent::keyPressed(key)) // search component's keypress
    {
      return true;
    }
    // Otherwise try Global command manager
    juce::CommandID id = Grace::getApp().commandManager.getKeyMappings()->findCommandForKeyPress(key);
    if (id)
    { 
      //      std::cout << "found " << id << " in global command mananger\n";
      Grace::getApp().commandManager.invokeDirectly(id, true);
      return true;
    }
    // FIXME!
    //    std::cout << "no handler for keypress, setting buffer changed=true\n";
    isChanged(true); // unhandled so adding chars to the buffer
    return false;     
  }
  prevkey = juce::KeyPress(key);
  return true;
} 

void CodeEditor::mouseDown(const juce::MouseEvent& e)
{
  //  std::cout << "MouseDown!\n";
  // call juce method to do whatever it needs to do
  juce::CodeEditorComponent::mouseDown(e);
  bool wasRegionOnUp = regionState.getEnd();
  bool isRegionOnDown = !getHighlightedRegion().isEmpty();
  if (wasRegionOnUp != isRegionOnDown)
  {
    //    std::cout << "CodeEditor::MouseDown: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }
  regionState.setStart(isRegionOnDown);
}

void CodeEditor::mouseUp(const juce::MouseEvent& e)
{
  //  std::cout << "MouseUp!\n";
  // call juce method to do whatever it wants to do
  juce::CodeEditorComponent::mouseUp(e);
  bool wasRegionOnDown = regionState.getStart(); 
  bool isRegionOnUp = !getHighlightedRegion().isEmpty();
  if (wasRegionOnDown != isRegionOnUp)
  {
    //    std::cout << "CodeEditor::MouseUp: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }
  regionState.setEnd(isRegionOnUp);
}

void CodeEditor::mouseDoubleClick (const juce::MouseEvent &e)
{
  //  std::cout << "CodeEditor::MouseDoubleClick!\n";
  juce::juce_wchar c = getCaretPos().getCharacter();
  switch (c)
  {
  case '(' :
  case '{' :
    {
      juce::CodeDocument::Position a (getCaretPos());
      moveExprForward();
      juce::CodeDocument::Position b (getCaretPos());
      if (b != a)
      {
        moveCaretTo(a,true);
        regionState.setEnd(true);
        //        std::cout << "MouseDoubleClick: region changed!\n";
        Grace::getApp().refreshMenuBar();
      }
      break;
    }
  case ')' :
  case '}' :
    {
      juce::CodeDocument::Position a (getCaretPos());
      moveExprBackward();
      juce::CodeDocument::Position b (getCaretPos());
      if (b != a)
      {
        moveCaretTo(a, true);
        regionState.setEnd(true);
        //        std::cout << "MouseDoubleClick: region changed!\n";
        Grace::getApp().refreshMenuBar();
      }
      break;
    }
  case '\"' :
    {
      // search right within same line for end-of-string
      juce::CodeDocument::Position a = getCaretPos();
      juce::CodeDocument::Position b = a.movedBy(1);
      juce::CodeDocument::Position z = getEOL(a);
      bool f = false;
      while (b.getPosition() < z.getPosition())
      {
        if ((c == '\"') && (b.getCharacter() == '\"'))
        {
          f = true;
          break;
        }
        else
          b.moveBy(1);
      }
      if (f)
      {
        b.moveBy(1);  // move one past found char
        moveCaretTo(b, true);
        regionState.setEnd(true);
        //        std::cout << "MouseDoubleClick: region changed!\n";
        Grace::getApp().refreshMenuBar();
      }
      break;
    }
  default:
    juce::CodeEditorComponent::mouseDoubleClick(e);
    if (!getHighlightedRegion().isEmpty())
    {
      regionState.setEnd(true);      
      Grace::getApp().refreshMenuBar();
    }
    break;
  }
}  	

void CodeEditor::selectAll()
{
  bool wasRegion = !getHighlightedRegion().isEmpty();
  regionState.setStart(true);
  regionState.setEnd(true);
  juce::CodeEditorComponent::selectAll();
  if (!wasRegion)
  {
    //    std::cout << "CodeEditor::selectAll: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }  
}

void CodeEditor::deselectAll()
{
  bool wasRegion = !getHighlightedRegion().isEmpty();
  regionState.setStart(false);
  regionState.setEnd(false);
  juce::CodeEditorComponent::deselectAll();
  if (wasRegion)
  {
    //    std::cout << "CodeEditor::deselectAll: region changed!\n";
    Grace::getApp().refreshMenuBar();
  }
}

void CodeEditor::matchParens()
{
  juce::CodeDocument::Position pos (getCaretPos());
  pos.moveBy(-1);
  int b = pos.getPosition();
  int scan = syntax->scanCode(document, pos, false, ScanIDs::MoveExpressions);
  if (scan <= ScanIDs::SCAN_EMPTY)
    return;
  int a = pos.getPosition();
  if (a < b)
    startParensMatching(pos,getCaretPos());
}

bool CodeEditor::isParensMatching()
{
  return parensmatching; 
}

void CodeEditor::isParensMatching(bool val)
{
  parensmatching=val; 
}

bool CodeEditor::isParensMatchingActive()
{
  // true if we are matching right now
  return (matchpos > -1); 
}

void CodeEditor::startParensMatching(const juce::CodeDocument::Position pos1, const juce::CodeDocument::Position pos2)
{
  moveCaretTo(pos1, false);
  //setjuce::Colour(juce::TextEditor::caretColourId, juce::Colours::red);
  matchpos=pos2.getPosition();
  startTimer(1000);
}

void CodeEditor::timerCallback()
{
  stopParensMatching();
}

void CodeEditor::stopParensMatching()
{
  if (matchpos > -1)
  {
    moveCaretTo(juce::CodeDocument::Position((const juce::CodeDocument&)document, matchpos), false);
    //setColour(juce::TextEditor::caretColourId, juce::Colours::black);
    matchpos=-1;
    if (isTimerRunning())
      stopTimer();
  }
}

void CodeEditor::focusGained(juce::Component::FocusChangeType cause)
{
  // When a code buffer is selected is becomes the "focus" and only
  // loses focus when a different code buffer is selected, i.e. the
  // editing focus is maintained even when the window itself is not
  // the active window
  juce::TopLevelWindow* w=NULL;
  // clear any other editing window of the focus
  for (int i=0; i<juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    w=juce::TopLevelWindow::getTopLevelWindow(i);
    juce::NamedValueSet& s=w->getProperties();
    if (s.contains(juce::Identifier("FocusEditor")))
      s.set(juce::Identifier("FocusEditor"), juce::var(false)) ;
    // FIXME
    //if (w->getComponentPropertyBool("FocusEditor", false))
    //	w->setComponentProperty("FocusEditor", false);
  }
  // select this one
  // FIXME
  //  getTopLevelComponent()->setComponentProperty("FocusEditor", true);
  getTopLevelComponent()->getProperties().set(juce::Identifier("FocusEditor"), juce::var(true));
}

//
// Implemenation
//

juce::XmlElement* CodeEditor::getColorTheme()
{
  return colorTheme;
}

void CodeEditor::setColorTheme(juce::XmlElement* theme)
{
  colorTheme = theme;
}

bool CodeEditor::isChanged()
{
  return changed;
}

void CodeEditor::isChanged(bool ch)
{
  changed=ch;
}

int CodeEditor::getTextType()
{
  return syntax->getTextType();
}

bool CodeEditor::isTextType(int type)
{
  return (getTextType()==type);
}

int CodeEditor::getFontSize()
{
  return fontsize;
}

void CodeEditor::setFontSize(int size)
{
  fontsize=size;
  juce::Font mono (getFont().getTypefaceName(), (float)size, juce::Font::plain);
  setFont(mono);

}

int CodeEditor::getTabWidth()
{
  return getTabSize();
}

void CodeEditor::setTabWidth(int siz)
{
  setTabSize(siz, true);
}

int CodeEditor::getColumns()
{
  return columns;
}

void CodeEditor::setColumns(int cols, bool redisplay)
{
  columns=juce::jmax(cols, 1);
  if (redisplay)
  {
    juce::Font mono (juce::Font::getDefaultMonospacedFontName(), (float)fontsize, juce::Font::plain);
    setSize((int)getCharWidth() * getColumns(), getLineHeight() * getLines() );
    // RESIZE WINDOW
    ((CodeEditorWindow*)getTopLevelComponent())->resizeForColumnsAndLines();
  }
}

int CodeEditor::getLines()
{
  return lines;
}

void CodeEditor::setLines(int num, bool redisplay)
{
  lines=juce::jmax(num,1);
  if (redisplay)
  {
    ((CodeEditorWindow*)getTopLevelComponent())->resizeForColumnsAndLines();
  }
}  

bool CodeEditor::isLineGutterVisible()
{
  return lineGutterVisible;
}

void CodeEditor::setLineGutterVisible(bool visible)
{
  lineGutterVisible = visible;
  setLineNumbersShown(lineGutterVisible);
}

bool CodeEditor::isEmacsMode()
{
  return emacsmode;
}

void CodeEditor::isEmacsMode(bool mode)
{
  emacsmode=mode;
}

///
///  Point Functions
///

juce::CodeDocument::Position CodeEditor::getBOL(const juce::CodeDocument::Position pos)
{
  return juce::CodeDocument::Position((const juce::CodeDocument&)document, pos.getLineNumber(), 0);
}

juce::CodeDocument::Position CodeEditor::getBOL()
{
  return getBOL(getCaretPos());
}

juce::CodeDocument::Position CodeEditor::getEOL(const juce::CodeDocument::Position pos)
{
  return juce::CodeDocument::Position((const juce::CodeDocument&)document, pos.getLineNumber(), INT_MAX);
}

juce::CodeDocument::Position CodeEditor::getEOL()
{
  return getEOL(getCaretPos());
}

juce::CodeDocument::Position CodeEditor::getEOB() 
{
  return juce::CodeDocument::Position((const juce::CodeDocument&)document, INT_MAX);
}

juce::CodeDocument::Position CodeEditor::getBOB() 
{
  return juce::CodeDocument::Position((const juce::CodeDocument&)document, 0);
}

bool CodeEditor::isBOB()
{
  return (getCaretPos().getPosition()==0);
}

bool CodeEditor::isEOB() 
{
  juce::CodeDocument::Position eob ((const juce::CodeDocument&)document, INT_MAX);
  return (getCaretPos() == eob);
}

bool CodeEditor::isBOL() 
{
  return (getCaretPos().getIndexInLine() == 0);
}

bool CodeEditor::isEOL() 
{
  juce::CodeDocument::Position here=getCaretPos();
  juce::CodeDocument::Position there ((const juce::CodeDocument&)document, here.getLineNumber(), INT_MAX);
  return (here.getIndexInLine()==there.getIndexInLine());
}

/*=======================================================================*
  Emacs Cursor Motion Functions
  *=======================================================================*/

juce::CodeDocument::Position CodeEditor::gotoBOB(bool sel)
{
  moveCaretToTop(sel);
  return getCaretPos();
}

juce::CodeDocument::Position CodeEditor::gotoEOB(bool sel)
{
  moveCaretToEnd(sel);
  return getCaretPos();
}

juce::CodeDocument::Position CodeEditor::gotoBOL(bool sel)
{
  //goToStartOfLine(false); // this stops at whitespace
  juce::CodeDocument::Position here=getCaretPos();
  juce::CodeDocument::Position there ((const juce::CodeDocument&)document, here.getLineNumber(), 0);
  moveCaretTo(there, sel);
  return getCaretPos();
}

juce::CodeDocument::Position CodeEditor::gotoEOL(bool sel)
{
  moveCaretToEndOfLine(sel);
  return getCaretPos();
}

void CodeEditor::moveCharForward(bool sel)
{
  moveCaretRight(false, sel);
}

void CodeEditor::moveCharBackward(bool sel)
{
  moveCaretLeft(false, sel);
}

void CodeEditor::moveWordForward(bool sel)
{
  //cursorRight(true,false); 
  juce::CodeDocument::Position pos(getCaretPos());
  int loc=pos.getPosition();
  int eob=getEOB().getPosition();
  // skip over non word, non token characters
  while (loc<eob && !(syntax->isWordChar(pos.getCharacter()) ||
                      syntax->isSymbolChar(pos.getCharacter())))
    pos.setPosition(++loc);
  // skip over word or token characters
  while (loc<eob && (syntax->isWordChar(pos.getCharacter()) ||
                     syntax->isSymbolChar(pos.getCharacter())))
    pos.setPosition(++loc);
  moveCaretTo(pos, sel);
}

void CodeEditor::moveWordBackward(bool sel)
{
  //cursorLeft(true,false);
  juce::CodeDocument::Position pos(getCaretPos());
  int loc=pos.getPosition();
  int bob=-1;
  // in backwards scanning cursor is always 1 past starting position
  pos.setPosition(--loc); 
  // skip over non word, non token characters
  while (loc>bob && !(syntax->isWordChar(pos.getCharacter()) ||
                      syntax->isSymbolChar(pos.getCharacter())))
    pos.setPosition(--loc);
  // skip over word or token characters
  while (loc>bob && (syntax->isWordChar(pos.getCharacter()) ||
                     syntax->isSymbolChar(pos.getCharacter())))
    pos.setPosition(--loc);
  // loc is is now 1 BELOW position.
  pos.setPosition(loc+1);
  moveCaretTo(pos, sel);
}

void CodeEditor::moveExprForward(bool sel)
{
  juce::CodeDocument::Position pos (getCaretPos());
  int scan=syntax->scanCode(document, pos, true, ScanIDs::MoveExpressions);
  if (scan<0)
  {}//PlatformUtilities::beep();
  else
    moveCaretTo(pos, sel);
}

void CodeEditor::moveExprBackward(bool sel)
{
  juce::CodeDocument::Position pos (getCaretPos());
  // if moving backwards the cursor is one char beyond start of scan
  pos.moveBy(-1);
  int scan=syntax->scanCode(document, pos, false, ScanIDs::MoveExpressions);
  if (scan<0)
  {}//PlatformUtilities::beep();
  else
    moveCaretTo(pos, sel);
}

void CodeEditor::moveLineForward(bool sel)
{
  moveCaretDown(sel);
}

void CodeEditor::moveLineBackward(bool sel)
{
  moveCaretUp(sel);
}

void CodeEditor::movePageForward(bool sel)
{
  pageDown(sel);
}

void CodeEditor::movePageBackward(bool sel)
{
  pageUp(sel);
}

/*=======================================================================*
  Find And Replace Methods
  *=======================================================================*/

bool CodeEditor::findPrevious(juce::String str, bool wrap)
{
  juce::CodeDocument::Position pos(getCaretPos());
  pos.moveBy(-getHighlightedRegion().getLength());
  int wid=str.length();
  int got=-1;
  while (true)
  {
    if (pos.getPosition()<=wid) // not enough chars to make a match
    {
      if (wrap)
      {
        wrap=false;
        pos.setPosition(INT_MAX);
      }
      else
        break;
    }
    juce::CodeDocument::Position bot=pos.movedBy(-100);
    juce::String tmp=document.getTextBetween(bot, pos);
    int len=tmp.length();
    got=tmp.lastIndexOf(str);
    if (got>-1)
    {
      pos.moveBy(-(len-got));
      break;
    }
    // next search position must overlapping current end
    pos.setPosition(bot.getPosition()+wid);
  }
  if (got>-1)
  {
    juce::Range<int> r (pos.getPosition(), pos.getPosition()+wid);
    setHighlightedRegion(r);
    return true;
  }
  return false;
}

bool CodeEditor::findNext(juce::String str, bool wrap)
{
  // start looking at current cursor position (if region exists caret
  // position is end of region)
  juce::CodeDocument::Position pos (getCaretPos());
  int eob=getEOB().getPosition();
  int wid=str.length();
  int got=-1;
  while (true)
  {
    //std::cout << "Top of loop, pos="<<pos.getPosition() 
    //          << ", pos+wid=" << pos.getPosition()+wid << ", eob=" << eob << "\n";
    if (pos.getPosition()+wid>eob) // not enough chars to make a match
    {
      if (wrap)
      {
        wrap=false;
        pos.setPosition(0);
      }
      else
        break;
    }
    const juce::String tmp=document.getTextBetween(pos, pos.movedBy(100));
    got=tmp.indexOf(str);
    if (got>-1)
    {
      pos.moveBy(got);
      break;
    }
    int len=tmp.length();
    // next search position must overlapping current end
    //std::cout<< "bottom of loop, moving pos by " << len-wid+1 << "\n";
    pos.moveBy(len-wid+1);
  }
  if (got>-1)
  {
    juce::Range<int> r (pos.getPosition(), pos.getPosition()+wid);
    setHighlightedRegion(r);
    return true;
  }
  return false;
}
bool CodeEditor::replaceAll(juce::String str, juce::String rep)
{
  int n=0;
  while (findNext(str, false))
  {
    n++;
    replace(rep);
  }
  return (n>0);
}

bool CodeEditor::replace(juce::String rep)
{
  if (!(getHighlightedRegion().getLength()>0))
    return false;
  insertTextAtCaret(rep);
  isChanged(true);
  return true;
}

bool CodeEditor::replaceAndFind(juce::String str, juce::String rep)
{
  if (replace(rep))
    return findNext(str);
  return false;
}

/*=======================================================================*
  Symbol Help
  *=======================================================================*/

void CodeEditor::lookupHelpAtPoint()
{
  bool region = (getHighlightedRegion().getLength() > 0);
  juce::String text;
  juce::String helppath;

  switch (getTextType())
  {
  case TextIDs::Sal1:
    helppath = "Sal:CM";
    break;
  case TextIDs::Sal2:
    helppath = "Sal:CM";
    break;
  case TextIDs::Lisp:
    helppath = "CM";
    break;
  default:
    return;
  }
  helppath << ":SndLib:Scheme";
  juce::CodeDocument::Position a;
  juce::CodeDocument::Position e;
  if (region)
  {
    a = juce::CodeDocument::Position((const juce::CodeDocument&)document, getHighlightedRegion().getStart());
    e = juce::CodeDocument::Position((const juce::CodeDocument&)document, getHighlightedRegion().getEnd());
    // e is one past last selected char
    if (a.getCharacter() == '\"' && e.movedBy(-1).getCharacter() == '\"')
    {
      a.moveBy(1);
      e.moveBy(-1);
    }
    text = document.getTextBetween(a, e);
  }
  else
  {
    a = getCaretPos();
    e = getCaretPos();
    if (syntax->isWordChar(a.getCharacter()) || syntax->isSymbolChar(a.getCharacter()))
    {
      while (a.getPosition() >=0 &&
             (syntax->isWordChar(a.getCharacter()) || syntax->isSymbolChar(a.getCharacter())))
        a.moveBy(-1);
      // a is now one below start of word
      a.moveBy(1);
      while (e.getPosition()<document.getNumCharacters() &&
             (syntax->isWordChar(e.getCharacter()) || syntax->isSymbolChar(e.getCharacter())))
        e.moveBy(1);
      // e is now one past end of word
      if (a.getPosition() < e.getPosition())
        text = document.getTextBetween(a, e);
    }
  }
  //std::cout << "help target='" << text.toUTF8() << "'\n";
  if (text.isNotEmpty())
    Help::getInstance()->symbolHelp(text, helppath);
  else // open CM dictionary if no symbol
    Help::getInstance()->openHelp(CommandIDs::HelpManual + 0);
}

/*=======================================================================*
  Emacs Editing Commands
  *=======================================================================*/

void CodeEditor::killCharForward()
{
  deleteForwards(false);
  isChanged(true);
}

void CodeEditor::killCharBackward()
{
  deleteBackwards(false);
  isChanged(true);
}

void CodeEditor::killWordForward()
{
  if (getCaretPos()!=getEOB())
  {
    moveWordForward(true);
    insertTextAtCaret (juce::String());
    isChanged(true);
  }
}

void CodeEditor::killWordBackward()
{
  if (getCaretPos() != getBOB())
  {
    moveWordBackward(true);
    insertTextAtCaret(juce::String());
    isChanged(true);
  }
}

void CodeEditor::killExprForward()
{
  juce::CodeDocument::Position here (getCaretPos());
  if (here != getEOB())
  {
    moveExprForward(true);
    // scanning could fail so test if we moved
    if (here != getCaretPos())
    {
      cutToClipboard();
      isChanged(true);
    }
  }
}

void CodeEditor::killExprBackward()
{
  //  std::cout << "killExprBackward\n";
}

void CodeEditor::killLine()
{
  // Kill line adding EOL+1 if line is only white space after point
  juce::CodeDocument::Position to=getEOL();
  juce::String kill=document.getTextBetween(getCaretPos(),to);
  if (!kill.containsNonWhitespaceChars()) // all white or empty
  {
    juce::String eol=document.getNewLineCharacters();
    kill << eol;
    to.moveBy(eol.length());
  }
  moveCaretTo(to, true);
  deleteBackwards(false);
  isChanged(true);
  // consecutive kills ADD the kill to clipboard
  if (prevkey==juce::KeyPress('k', juce::ModifierKeys::ctrlModifier, 0))
    kill = juce::SystemClipboard::getTextFromClipboard() + kill;
  juce::SystemClipboard::copyTextToClipboard(kill);
}

void CodeEditor::killWhite()
{
  juce::CodeDocument::Position p(getCaretPos());
  juce::CodeDocument::Position e=getEOB();
  while (p != e && juce::CharacterFunctions::isWhitespace(p.getCharacter()))
    p.moveBy(1);
  if (getCaretPos()!=p)
  {
    moveCaretTo(p,true);
    deleteBackwards(false);
    isChanged(true);
  }
}

void CodeEditor::changeCase(int flag)
{
  // change case of word at point. 0=lower,1=upper,2=capitalize
  juce::CodeDocument::Position beg=getCaretPos();
  moveCaretRight(true, true);
  juce::String text=document.getTextBetween(beg, getCaretPos());
  if (text==juce::String())
    return;
  deleteBackwards(false);
  if (flag==0)
    text=text.toLowerCase();
  else if (flag==1)
    text=text.toUpperCase();
  else if (flag==2)
    for (int i=0; i<text.length(); i++)
      if (juce::CharacterFunctions::isLetter(text[i]))
      {
        //          text[i]=juce::CharacterFunctions::toUpperCase(text[i]);
        text=text.substring(0,i)+text.substring(i,i+1).toUpperCase()+text.substring(i+1);
        break;
      }
  insertTextAtCaret(text);
  isChanged(true);
}

void CodeEditor::killRegion()
{
  //  std::cout << "killRegion\n";
}

void CodeEditor::openLine()
{
  juce::CodeDocument::Position here=getCaretPos();
  insertTextAtCaret(document.getNewLineCharacters());
  moveCaretTo(here, false);
  isChanged(true);
}

void CodeEditor::posInfo(const juce::CodeDocument::Position p)
{
  juce::String s;
  s << p.getCharacter();
  //  std::cout << "curs=" << p.getPosition() << " (" << p.getLineNumber() << "," << p.getIndexInLine() << "), char='" << s << "'\n"
  //            << "line='" << p.getLineText() << "'\n"
  //            << ", bol?="<< isBOL() << ", eol?=" << isEOL() << ", bob?="<< isBOB() << ", eob?=" << isEOB() << "\n";
}

void CodeEditor::test(bool forward)
{
}

/*=======================================================================*
  Evaluation and Syntactic Backward Expression
  *=======================================================================*/

void CodeEditor::eval(bool expandonly)
{
  if (!TextIDs::canExecute(getTextType()))
    return;
  int regn=getHighlightedRegion().getLength();
  int scan=ScanIDs::SCAN_EMPTY;
  juce::Array<int> epos; // positions of backward exprs
  juce::CodeDocument::Position bot (getCaretPos());
  juce::CodeDocument::Position top (getCaretPos());
  int end=0;  // lowest position possible

  if (regn>0) // if region use its bounds
  {
    bot=juce::CodeDocument::Position((const juce::CodeDocument&)document, getHighlightedRegion().getStart());
    top=bot.movedBy(regn);
    end=bot.getPosition();
  }
  // process region backwards recording the starting and ending
  // positions of expressions
  //// int n=0;
  while (true /*&& (n<100) */)
  {
    ////std::cout << n++ << " before backwardExpr, bot=" << bot.getPosition() << ", top=" << top.getPosition() << "\n";

    scan=syntax->backwardExpr(document, bot, top);

    //      std::cout << "after backwardExpr, scan=" << scan << ", bot="
    //            << bot.getPosition() << ", top=" << top.getPosition() 
    //            << ", expr='" << document.getTextBetween(bot, top).toUTF8() << "'"
    //            << "\n";
      
    // break on error or nothing new to add
    if (scan<=0) 
    {
      //std::cout << "breaking scan<0\n";
      break;
    }
    // break if past lower bounds
    if (bot.getPosition()<end)
    {
      //std::cout << "breaking pos<end\n";
      break;
    }
    // push current expr bounds onto array
    epos.insert(0, top.getPosition());
    epos.insert(0, bot.getPosition());
    // break if we are evalling just one backward expr
    if (regn==0) 
    {
      //std::cout << "breaking (regn=false)\n";
      break; 
    }
    // break if we added last possible expr
    if (bot.getPosition()==end)
    {
      //std::cout << "breaking pos=end\n";
      break;
    }
    // move top to bot (ie 1 above next start)
    top=juce::CodeDocument::Position(bot);
  }

  //std::cout << "after loop, epos.size()=" << epos.size() << "\n";
  if (scan<0)
  {
    juce::String text;
    if (scan==ScanIDs::SCAN_UNLEVEL)
    {
      text=">> Error: unbalanced delimiter, line ";
      text << bot.getLineNumber() << ":\n" 
           << bot.getLineText() << "\n";
    }
    else if (scan == ScanIDs::SCAN_UNMATCHED) // HKT FIXED SEPT 11
    {
      text=">>> Error: unmatched delimiter, line ";
      // line is most recent upper bounds
      text << top.getLineNumber() << ":\n" 
           << top.getLineText() << "\n";
    }
    ////      PlatformUtilities::beep();
    Console::getInstance()->printError(text, true);
  }
  else if (epos.size()>0) // got expr(s)
  {
    bot.setPosition(epos.getFirst());
    top.setPosition(epos.getLast());
    syntax->eval(document, bot, top, expandonly, regn > 0);
  }
  else
  {
    //std::cout << "empty!\n";
  }
}

void CodeEditor::indent()
{
  juce::CodeDocument::Position bol=getBOL();
  juce::CodeDocument::Position pos (getCaretPos());
  juce::String all=pos.getLineText();
  int col=syntax->getIndentation(document, getCaretPos().getLineNumber());
  if (col<0)
  {
    insertTabAtCaret();
    isChanged(true);
    return;
  }

  int old=0;
  while (old<all.length() && (all[old]==' ' || all[old]=='\t'))
    old++;
  //std::cout << "old=" << old << ", col=" << col << ", len=" << all.length() << "\n";
  //if (col==old) return;
  moveCaretTo(bol,false);
  if (old>col)
  {
    //std::cout <<"cursor at col="<<getCaretPos().getIndexInLine()<<"\n";
    //std::cout << "removing " << (old-col)<<"\n";
    for (int i=0; i<(old-col); i++)
      moveCaretRight(false,true);
    deleteForwards(false);
    isChanged(true);
  }
  else if (old<col)
  {
    juce::String pad = juce::String();  
    for (int i=0; i<(col-old); i++)
      pad<<" ";
    //std::cout << "adding " << (col-old)<< "pad=='" << pad.toUTF8() << "'\n";
    insertTextAtCaret(pad);
    isChanged(true);
  }
  while (bol!=getEOL() && (bol.getCharacter()==' ' || bol.getCharacter()=='\t'))
    bol.moveBy(1);
  moveCaretTo(bol, false);
}

