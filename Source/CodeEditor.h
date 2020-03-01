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

#ifndef CM_CODEEDITOR_H
#define CM_CODEEDITOR_H

#include "Libraries.h"

class Syntax;
class SynTok;

/*=======================================================================*/

class CodeEditor : public juce::CodeEditorComponent , public juce::Timer
{
public:
  Syntax* syntax;
  juce::CodeDocument& document;

  CodeEditor(juce::CodeDocument& _document, Syntax* _tokenizer, juce::XmlElement* custom);
  ~CodeEditor() ;
  bool keyPressed (const juce::KeyPress& key);
  void insertTextAtCaret (const juce::String& textToInsert); 
  void mouseDoubleClick (const juce::MouseEvent &e);
  void mouseDown(const juce::MouseEvent &e);
  void mouseUp(const juce::MouseEvent &e);
  void selectAll();
  void deselectAll();
  void focusGained(juce::Component::FocusChangeType cause);

  juce::KeyPress prevkey;
  int matchpos;
  int fontsize;
  int tabwidth;
  int columns;
  int lines;
  bool lineGutterVisible;
  bool emacsmode;
  bool changed;
  bool parensmatching;
  juce::XmlElement* colorTheme;
  juce::Range<bool> regionState;
  int getTextType();
  bool isTextType(int type);
  int getFontSize();
  void setFontSize(int size);
  int getTabWidth();
  void setTabWidth(int siz);
  int getColumns();
  void setColumns(int num, bool redisplay=true);
  int getLines();
  void setLines(int num, bool redisplay=true);
  bool isLineGutterVisible();
  void setLineGutterVisible(bool visible);
  void setColorTheme(juce::XmlElement* theme);
  juce::XmlElement* getColorTheme();

  bool isChanged();
  void isChanged(bool changed);
  bool isEmacsMode();
  void isEmacsMode(bool mode);

  bool isParensMatching();
  void isParensMatching(bool matching);
  bool isParensMatchingActive();
  void matchParens();
  void startParensMatching(const juce::CodeDocument::Position pos1, const juce::CodeDocument::Position pos2);
  void stopParensMatching();
  void timerCallback();
  // cursor postion functions
  juce::CodeDocument::Position getBOB();
  juce::CodeDocument::Position getEOB();
  juce::CodeDocument::Position getBOL(); 
  juce::CodeDocument::Position getEOL();
  juce::CodeDocument::Position getBOL(const juce::CodeDocument::Position pos); 
  juce::CodeDocument::Position getEOL(const juce::CodeDocument::Position pos);
  bool lookingAt(const juce::CodeDocument::Position pos, const juce::String text, const bool forward, const bool delimited);
  bool isBOB();
  bool isEOB();
  bool isBOL();
  bool isEOL();
  juce::CodeDocument::Position gotoBOL(bool sel=false);
  juce::CodeDocument::Position gotoEOL(bool sel=false);
  juce::CodeDocument::Position gotoBOB(bool sel=false);
  juce::CodeDocument::Position gotoEOB(bool sel=false);
  void moveCharForward(bool sel=false);
  void moveCharBackward(bool sel=false);
  void moveWordForward(bool sel=false);
  void moveWordBackward(bool sel=false);
  void moveExprForward(bool sel=false);
  void moveExprBackward(bool sel=false);
  void moveLineForward(bool sel=false);
  void moveLineBackward(bool sel=false);
  void movePageForward(bool sel=false);
  void movePageBackward(bool sel=false);
  // Emacs editing
  void killCharForward();
  void killCharBackward();
  void killWordForward();
  void killWordBackward();
  void killExprForward();
  void killExprBackward();
  void killLine();
  void killWhite();
  void killRegion();
  void changeCase(int change);
  void openLine();
  // Find/Replace
  bool replaceAll(juce::String str, juce::String rep);
  bool replace(juce::String rep);
  bool replaceAndFind(juce::String str, juce::String rep);
  bool findPrevious(juce::String str, bool wrap=true);
  bool findNext(juce::String str, bool wrap=true);
  // Help
  void lookupHelpAtPoint();
  // Evaluation
  void eval(bool expandonly);
  void evalLisp(const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region);
  void evalSal(const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region);
  void indent();
  //  int lastIndented(juce::Array<int>& ary, bool index);
  //  bool isCommaTerminatedLine(int line);
  bool tokenizeSal(const juce::CodeDocument::Position start, const juce::CodeDocument::Position end,
                   juce::OwnedArray<SynTok>& tokens);
  int isNumberToken(const juce::String str);

  // testing
  void test(bool forward);
  void posInfo(const juce::CodeDocument::Position pos);
};
 
/*=======================================================================*/

class CodeEditorWindow : public juce::DocumentWindow//////, public juce::MenuBarModel,
                         //////                         public juce::ApplicationCommandTarget
{
  // window's content component
  class EditorComponent : public juce::Component 
  {
    void resized () 
    {
      if (buffer)
        buffer->setBounds(0, 0, getWidth(), getHeight());
    }
  //MACMENU
  //  void mouseDrag(const juce::MouseEvent& event){std::cout << "MouseDrag!\n";}
  //  void mouseDown(const juce::MouseEvent& event){std::cout << "MouseDown!\n";}
  public:
    CodeEditor* buffer;
    EditorComponent ()
      : buffer (NULL)
    {
    }
    ~EditorComponent()
    {
      //deleteAllChildren();
      //deleteCodeEditor();
    }
    CodeEditor* getCodeEditor() 
    {
      return buffer;
    }
    void setCodeEditor(CodeEditor* buf) 
    {
      buffer=buf;
      buffer->setTopLeftPosition(0,0);
      addChildComponent(buffer, -1);
    }
    void deleteCodeEditor()
    {
      CodeEditor* buf=buffer;
      buffer=0;
      removeChildComponent(buf);
      delete buf;
    }
  };

public:
  juce::CodeDocument document;
  juce::File sourcefile;
  CodeEditorWindow(juce::File file, juce::String text=juce::String(), int synt=0, juce::String title=juce::String()) ;
  ~CodeEditorWindow () ;
  void closeButtonPressed () ;
  void activeWindowStatusChanged();

  void getAllCommands(juce::Array<juce::CommandID>& commands);
  juce::ApplicationCommandTarget* getNextCommandTarget();
  void getCommandInfo( juce::CommandID id, juce::ApplicationCommandInfo& info);
  bool perform(const juce::ApplicationCommandTarget::InvocationInfo& info);
  //////  juce::StringArray getMenuBarNames ();
  //////  juce::PopupMenu getMenuForIndex(int menuIndex, const juce::String& menuName);
  //////  void menuItemSelected (int menuItemID, int topLevelMenuIndex);

  void setWindowTitle(juce::String title=juce::String());
  void resizeForColumnsAndLines();
  CodeEditor* getCodeEditor();
  juce::File& getSourceFile()
  {
    return sourcefile;
  }
  bool isCustomComment();
  juce::XmlElement* getCustomizations();
  void writeCustomComment(bool select);
  void readCustomComment();
  void switchBufferSyntax(int newtype);
  void updateKeyPressesForEditMode();
  void isEmacsMode(bool mode);
  bool isEmacsMode();

  bool hasUnsavedChanges();
  void saveFile(bool saveas);
  void saveFileAs();
  void saveFileVersion();
  juce::File getSourceFileDirectory();
  // static methods
  static void openFile(juce::File file=juce::File());
  static void newFile(juce::String title=juce::String(), int synt=TextIDs::Empty, juce::String content=juce::String());
  static CodeEditorWindow* getFocusCodeEditor();
  static void openFindAndReplaceDialog();

};

#endif
