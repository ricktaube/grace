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

class Plotter;
class Layer;
class Transport;

/*=======================================================================*
  Editor Components
  *=======================================================================*/

class Graphics
{
public:
  static const int Margin = 8;
  static const int HalfMargin = Margin / 2;
  static const int LineHeight = 24;
  static const int LineAndSpace = LineHeight+Margin;
  static const int ItemSpacer = Margin;
  static const int FontSize = 15;
};

/*=======================================================================*
  Label
  *=======================================================================*/

/** Label component with consistent self-sizing of label around
    text. **/

class EditorLabel : public juce::Label
{
public:
  EditorLabel(juce::String text) : juce::Label(juce::String(), text)
  {
    juce::Font font ((float)Graphics::FontSize, juce::Font::plain);
    setFont (font);
    setJustificationType (juce::Justification::centredLeft);
    setEditable (false, false, false);
    ////    setColour (juce::Label::textColourId, juce::Colours::black);
    ////    setColour (juce::Label::backgroundColourId, juce::Colour(0x0));
    setSize((int)(font.getStringWidthFloat(getText()+"  ")), Graphics::LineHeight);
  }
  virtual ~EditorLabel()
  {
  }
};

/*=======================================================================*
  Button
  *=======================================================================*/

/** juce::TextButton component with consistent self-sizing around text. **/

class EditorButton : public juce::TextButton
{
public:

  EditorButton(juce::String text, int width=0, int height=0) : juce::TextButton(text)
  {
    if (height==0)
      height=Graphics::LineHeight;
    if (width==0)
    {
      changeWidthToFitText(height);
      // add a bit more width to what Juce otherwise does
      width=getWidth() + 16; 
    }
    setSize(width, height);
  }

  virtual ~EditorButton(){}
};

/*=======================================================================*
  CheckBox
  *=======================================================================*/

/** juce::ToggleButton component without focus outline. **/

class EditorCheckBox : public juce::ToggleButton
{
public:
  EditorCheckBox(juce::String text)
    : juce::ToggleButton(text)
  {
    juce::Font font ((float)Graphics::FontSize, juce::Font::plain);
    setSize((int)(font.getStringWidthFloat(getButtonText() + "     ")), 
            Graphics::LineHeight);
    ////    setColour(juce::TextEditor::focusedOutlineColourId, juce::Colour(0x00));
  }
  virtual ~EditorCheckBox(){}
};

/*=======================================================================*
  TextBox
  *=======================================================================*/

/** A single line TextEditor with some extra methods for text
    handling **/
class EditorTextBox : public juce::TextEditor
{
private:

  bool textChanged;
  bool enterOnLoss;

public:

  EditorTextBox(juce::String text=juce::String())
    : juce::TextEditor(),
      textChanged (false),
      enterOnLoss(true)
  {
    setWantsKeyboardFocus(true);
    setText(text);
    setMultiLine(false);
    setReturnKeyStartsNewLine(false);
    setReadOnly(false);
    setScrollbarsShown (false);
    setCaretVisible(true);
    setPopupMenuEnabled(true);
    setFont(juce::Font(15.0000f, juce::Font::plain));
  }

  virtual ~EditorTextBox() {}

  juce::String getTrimmedText()
  {
    juce::String txt=getText();
    if (txt.isEmpty()) return txt;
    int siz=txt.length();
    if (txt[0]==' ' || txt[0]=='\t' )
      txt=txt.trim();
    else if (siz>1 && (txt[siz-1]==' ' || txt[siz-1]=='\t' ))
      txt=txt.trim();
    return txt;
  }

  bool isEmpty()
  {
    return getTrimmedText().isEmpty();
  }

  /** Reimplemented juce method dims disabled text in edit box. **/

  void setEnabled(bool enabled)
  {
    juce::TextEditor::setEnabled(enabled);
    ////    setColour(juce::TextEditor::textColourId, ((enabled) ? juce::Colours::black :  juce::Colours::grey));
    applyFontToAllText(getFont());
  }

  /** Returns a double value from the text box skipping white space. **/

  double getDoubleValue()
  {
    return getTrimmedText().getDoubleValue();
  }

  /** Returns an int value from the text box skipping white space. **/

  int getIntValue()
  {
    return getTrimmedText().getIntValue();
  }

  /** Returns true if editor text contains a valid int or a valid
      float (if floatOk is true.) **/

  bool isNumericText(bool floatOk=true)
  {
    const juce::String num=getTrimmedText();
    int dots=0;
    int sign=0;
    int digi=0;
    int size=num.length();
    for (int i=0; i<size; i++)
    {
      if (num[i]=='-') 
        if (i==0) sign=1;
        else return false; // bad sign
      else if (num[i]=='.')
        if (dots==0 && floatOk) dots++;
        else return false; // too many dots
      else if (num[i]<'0' || num[i]>'9') 
        return false;
      else digi++;
    }
    return digi>0;
  }

  /** True if loss of focus enters text. **/

  bool doesLossOfFocusEnterText(){return enterOnLoss;}
  void setDoesLossOfFocusEnterText(bool enters){enterOnLoss=enters;}

  /** True if text marked as changed (set by a juce::TextEditor::Listener). **/

  bool isTextChanged(){return textChanged;}

  /** Sets textChanged status in editor (used by juce::TextEditor::Listener) methods. **/

  void setTextChanged(bool changed){textChanged=changed;}

};

/*=======================================================================*

  PlotEditor

  *=======================================================================*/

/**Base class of the various tabbed editors. Subclasses override the
   listener definitions they need to implement.*/
class PlotEditor : public juce::Component,
                   public juce::Button::Listener, public juce::TextEditor::Listener,
                   public juce::Slider::Listener, public juce::ComboBox::Listener
{
public:
  static const int margin=8;
  static const int lineheight=24;
  static const int fontsize=15;
  enum EditorType {Empty=0, WindowEditor, AudioEditor, ExportEditor, AxisEditor,
                   LayerEditor, PointsEditor};
  PlottingControls* controls;
  juce::TabBarButton* tabButton;
  int tabType;
  PlotEditor (PlottingControls* c) 
  : controls(c),
    tabButton (0),
    tabType(Empty) 
  {
  }

  virtual ~PlotEditor ()
  {
  }

  void buttonClicked (juce::Button* buttonThatWasClicked) {}
  void comboBoxChanged(juce::ComboBox* combo) {}
  void sliderValueChanged (juce::Slider* sliderThatWasMoved) {}
  void textEditorReturnKeyPressed(juce::TextEditor& editor) {}

  /**The default text changed callback marks the textbox it was called
      from as 'changed'.*/
  void textEditorTextChanged(juce::TextEditor& editor) 
  {
    if (EditorTextBox* ed = dynamic_cast<EditorTextBox*>(&editor))
      ed->setTextChanged(true);
  }

  void textEditorFocusLost(juce::TextEditor& editor) 
  {
    //std::cout << "EditorTextBox::textEditorFocusLost\n";
    EditorTextBox* ed = dynamic_cast<EditorTextBox*>(&editor);
    if (ed  && ed->doesLossOfFocusEnterText() && ed->isTextChanged())
    {
      textEditorReturnKeyPressed(editor);
      ed->setTextChanged(false);
    }
  }

  void textEditorEscapeKeyPressed(juce::TextEditor& editor) {}

  PlottingControls* getPlottingControls()
  {
    return (PlottingControls*)getParentComponent();
  }

  Plotter* getPlotter();

 juce::TabBarButton* getTabButton()
  {
    return tabButton;
  }
  /**Called from the PlotControl's sniffer to update editor values to
     reflect the currently active plotwindow. Plotter is the currently
     active plot or null if there's none. If statusHasChanged is true
     the the plotter value is 'new' ie its not the same value as the
     previous invocation of the function.*/
  virtual void updateFromActivePlot(Plotter* plotter, bool statusHasChanged) = 0;
};

/*=======================================================================*
  PlotWindowEditor  THE "PLOT TAB"
  *=======================================================================*/

/** The Window tab editor **/

class PlotWindowEditor : public PlotEditor, public juce::ChangeListener
{
public:
  PlotWindowEditor (PlottingControls* controls);
  virtual ~PlotWindowEditor();
private:
  EditorLabel* namelabel;
  EditorTextBox* namebuffer;
  EditorButton* savebutton;
  EditorButton* layerbutton;
  juce::GroupComponent* bggroup;
  EditorCheckBox* bggridcheckbox;
  EditorCheckBox* bgplottingcheckbox;
  EditorCheckBox* bgmouseablecheckbox;
  //  EditorLabel* bgcolorlabel;
  juce::ColourSelector* bgcolorpicker;

  juce::GroupComponent* fieldgroup;
  EditorLabel* fieldxlabel;
  EditorLabel* fieldylabel;
  juce::ComboBox* fieldxmenu;
  juce::ComboBox* fieldymenu;
  void resized ();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  void textEditorReturnKeyPressed(juce::TextEditor& editor);
  void changeListenerCallback (juce::ChangeBroadcaster* source);
  void comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged);
  void updateFromActivePlot(Plotter* plotter, bool statusHasChanged);
};

/*=======================================================================*
  PlotAudioEditor
  *=======================================================================*/

/** The Audio tab editor. **/
 
class PlotAudioEditor : public PlotEditor
{
public:
  PlotAudioEditor(PlottingControls* controls);
  virtual  ~PlotAudioEditor();
private:
  EditorCheckBox* ycheckbox;
  EditorLabel* y0label;
  EditorTextBox* y0typein;
  EditorLabel* y1label;
  EditorTextBox* y1typein;
  Transport* transport;
  EditorLabel* tuninglabel;
  juce::ComboBox* tuningmenu;
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  void comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged);
  void textEditorReturnKeyPressed(juce::TextEditor& editor);
  void updateFromActivePlot(Plotter* plotter, bool statusHasChanged);
};

/*=======================================================================*
  PlotAxisEditor
  *=======================================================================*/

/** The Axis tab editor **/

class PlotAxisEditor : public PlotEditor 
{
  int orientation;
  EditorLabel* fromlabel;
  EditorLabel* tolabel;
  EditorLabel* bylabel;
  EditorLabel* typelabel;
  EditorLabel* tickslabel;
  juce::ComboBox* typemenu;
  EditorTextBox* namebuffer;
  EditorTextBox* frombuffer;
  EditorTextBox* tobuffer;
  EditorTextBox* bybuffer;
  EditorTextBox* ticksbuffer;
  juce::ComboBox* decimalsmenu;
  EditorLabel* zoomlabel;
  juce::Slider* zoomslider;
  EditorCheckBox* fitcheckbox;
  void updateFromActivePlot(Plotter* plotter, bool statusHasChanged);
  void updateForAxisType(int type);
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  void comboBoxChanged(juce::ComboBox* combo);
  void sliderValueChanged (juce::Slider* sliderThatWasMoved);
  void textEditorReturnKeyPressed(juce::TextEditor& editor);
public:
  PlotAxisEditor (PlottingControls* controls, int orient);
  virtual ~PlotAxisEditor();
};

/*=======================================================================*
  PlotLayerEditor
  *=======================================================================*/

/** The Layer tab editor **/

class PlotLayerEditor : public PlotEditor, public juce::ChangeListener
{
public:
  PlotLayerEditor (PlottingControls* controls, Layer* layr);
  virtual ~PlotLayerEditor();
  EditorButton* deletebutton;  // for enable/disable 
  Layer* layer;
private:
  EditorLabel* namelabel;
  EditorTextBox* namebuffer;
  EditorLabel* stylelabel;
  juce::ComboBox* stylemenu;
  EditorLabel* linelabel;
  EditorTextBox* linetextbox;
  EditorLabel* pointlabel;
  EditorTextBox* pointtextbox;
  EditorLabel* barlabel;
  EditorTextBox* bartextbox;
  juce::GroupComponent* audiogroup;
  EditorLabel* durlabel;
  EditorTextBox* durtypein;
  EditorLabel* amplabel;
  EditorTextBox* amptypein;
  EditorLabel* chanlabel;
  EditorTextBox* chantypein;
  EditorLabel* colorlabel;
  juce::ColourSelector* colorpicker;
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  void comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged);
  void changeListenerCallback(juce::ChangeBroadcaster* source);
  void textEditorReturnKeyPressed(juce::TextEditor& editor);
  //  void textEditorFocusLost(juce::TextEditor& editor);
  //  void textEditorTextChanged(juce::TextEditor& editor);
  void updateFromActivePlot(Plotter* plotter, bool statusHasChanged);
};

/*=======================================================================*
  PlotExportEditor
  *=======================================================================*/

/** The Export tab editor. **/

class PlotExportEditor : public PlotEditor
{
  static const int ExportToNewEditor = 1;
  static const int ExportToClipboard = 2;
  static const int ExportPoints  = 1;
  static const int ExportRecords = 2;
  static const int ExportLayer   = 3;
  static const int ExportPlot    = 4;
public:
  PlotExportEditor(PlottingControls* controls);
  virtual ~PlotExportEditor();
  int numfields;
  juce::Array<bool> include;
  EditorLabel* exportLabel;
  EditorCheckBox* focusButton;
  EditorCheckBox* allButton;
  EditorLabel* formatlabel;
  juce::ComboBox* formatmenu;
  juce::TextButton* fieldsbutton;
  EditorLabel* decimalslabel;
  juce::ComboBox* decimalsmenu;
  EditorLabel* destlabel;
  juce::ComboBox* destmenu;
  EditorButton* exportbutton;
  void resized();
  void buttonClicked (juce::Button* button);
  void comboBoxChanged (juce::ComboBox* combobox);
  void exportPoints();
  void exportPlot();
  void updateFromActivePlot(Plotter* plotter, bool statusHasChanged);
};

/*=======================================================================*
  PlotControls
  *=======================================================================*/

///The main component, holds various tabbed editors for plotting.

class PlottingControls : public juce::TabbedComponent, public juce::Timer
{
  ///Updates tabbed editors with values from current plot window.
  void timerCallback();
 
public:
  ///The current plot to control (can be null pointer)
  Plotter* plotter;
  PlottingControls();
  virtual ~PlottingControls();

  void addEditor(PlotEditor* editor)
  {
    addTab(editor->getName(), ColorThemeIDs::getWindowBackgroundColor(), editor, true);
    // cache the new editor's tab button in the editor
    for (int i = 0; i < getNumTabs(); i++)
      if (editor == getTabContentComponent(i))
      {
        editor->tabButton=getTabbedButtonBar().getTabButton(i);
        break;
      }
  }

  PlotEditor* getEditor(int index)
  {
    return static_cast<PlotEditor*>(getTabContentComponent(index));
  }

  void deleteEditor(PlotEditor* editor)
  {
  }

  int getTabIndex(juce::Component* ed)
  {
    for (int i=0; i<getNumTabs(); i++)
      if (getTabContentComponent(i)==ed)
        return i;
    return -1;
  }

  void currentTabChanged (int newCurrentTabIndex, const juce::String &newCurrentTabName);
};
