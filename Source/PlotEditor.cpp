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
#include "Plot.h"
#include "PlotEditor.h"
#include "PlotWindow.h"
#include "CodeEditor.h"
#include "Midi.h"
#include "Preferences.h"

/*=======================================================================*
  PlottingControls, a tabbled component holiding the various plot editors
 *=======================================================================*/

PlottingControls::PlottingControls()
  : juce::TabbedComponent(juce::TabbedButtonBar::TabsAtTop) ,
    plotter (0)
{
  static const int EditorWidth = 500;
  // total height of tabbed editor is 5 lineheight plus 5 margins: tab
  // is 1 line, content is 4 lines and 5 margins
  static const int EditorHeight = (PlotEditor::lineheight * 5) + (PlotEditor::margin * 5);
  setVisible(true);
  setSize(EditorWidth, EditorHeight);
  addEditor(new PlotWindowEditor(this));
  //  addEditor(new PlotAudioEditor(this));
  addEditor(new PlotExportEditor(this));
  addEditor(new PlotAxisEditor(this, Plotter::horizontal));
  addEditor(new PlotAxisEditor(this, Plotter::vertical));
  startTimer(200);
}

PlottingControls::~PlottingControls()
{
  // editors are owned by juce::TabbedComponent
  stopTimer();
  plotter = 0;
}
 
Plotter* PlotEditor::getPlotter()
{
  if (PlottingControls* controls = getPlottingControls())
    return controls->plotter;
  return 0;
}

void PlottingControls::timerCallback()
{
  PlotWindow* active = dynamic_cast<PlotWindow*>(Grace::getApp().activeWindow);
  bool changed = false;
  // determine if there is a change in plot status since the last
  // sniff. active is the current plot window (which may be null).
  if (active) // have a window
  {
    Plotter* p = active->getPlotter();
    changed = (plotter != p);
    plotter = p;
  }
  else
  {
    if (plotter) // dont have a plotter any more
      changed = true;
    plotter = 0;
  }

  if (changed)
  {
    ////    std::cout << "Plotter changed to #<" << plotter << ">\n";
    // if current tab is a layer editor switch to Tab 0.
    if (dynamic_cast<PlotLayerEditor*>(getCurrentContentComponent()) != 0)
    {
      setCurrentTabIndex(0, false);
    }
    // delete all layer tabs. these are at the end of the array so
    // process in reverse order to keep index valid during deletion
    for (int i = getNumTabs() - 1; i >= 0; i--)
    {
      PlotEditor* e = getEditor(i);
      if (e->tabType == PlotEditor::LayerEditor)
        removeTab(i);
      else
        break;
    }
    // install layer tabs for a new plotter
    if (plotter)
    {
      for (int i = 0; i < plotter->numLayers(); i++)
        addEditor(new PlotLayerEditor(this, plotter->getLayer(i)));
    }
    else
    {
    }
    // update all tabs with current plotter (which may be null)
    for (int i = 0; i < getNumTabs(); i++)
    {
      PlotEditor* e = getEditor(i);
      e->updateFromActivePlot(plotter, true);
    }
    ////    std::cout << "Done PlottingControls::timerCallback\n";
  }
  else // plotter is same value as it was
  {
    /*
    if (plotter)
    {
      // update all tabs with current plotter
      for (int i = 0; i < getNumTabs(); i++)
      {
        PlotEditor* e = getEditor(i);
        e->updateFromActivePlot(plotter, false);
      }
    }
    */
  }
}
 
void PlottingControls::currentTabChanged (int newCurrentTabIndex, const juce::String &newCurrentTabName)
{
  ////  std::cout << "PlottingControls::currentTabChanged\n";
  PlotEditor* editor = dynamic_cast<PlotEditor*>(getTabContentComponent(newCurrentTabIndex));
  if (!editor)
  {
    std::cout << "PlottingControls::currentTabChanged: No editor at new current tab index "
              << newCurrentTabIndex << "\n";
    return;
  }
  switch (editor->tabType)
  {
  case PlotEditor::WindowEditor:
    //    std::cout << "window editor!\n";    
    break;
  case PlotEditor::AudioEditor:
    //    std::cout << "audio editor!\n";    
    break;
  case PlotEditor::AxisEditor:
    //    std::cout << "axis editor!\n";    
    break;
  case PlotEditor::LayerEditor:
    //      std::cout << "layer editor!\n";  
    if (PlotLayerEditor* e = dynamic_cast<PlotLayerEditor*>(editor))
    {
      if (plotter)
      {
        e->deletebutton->setEnabled(plotter->numLayers() > 1);
        plotter->setFocusLayer(e->layer);
        plotter->redrawAll();
      }
    }
    break;
  case PlotEditor::ExportEditor:
    //    std::cout << "export editor!\n";    
    break;
  case PlotEditor::PointsEditor:
    //    std::cout << "points editor!\n";    
    break;
  default:
    //    std::cout << "unknown editor!\n";
    break;
  }
}

/*=======================================================================*
  Window Editor
  *=======================================================================*/

PlotWindowEditor::PlotWindowEditor (PlottingControls* controls)
  : PlotEditor(controls),
    namelabel(0),
    namebuffer (0),
    savebutton (0),
    layerbutton (0),
    bggroup (0),
    bggridcheckbox (0),
    bgplottingcheckbox (0),
    bgmouseablecheckbox (0),
    bgcolorpicker (0),
    fieldgroup (0),
    fieldxlabel (0),
    fieldylabel (0),
    fieldxmenu (0),
    fieldymenu (0)
{
  tabType = WindowEditor;
  setVisible(true);
  setName("Window");

  addAndMakeVisible(namelabel = new EditorLabel("Title:"));
  addAndMakeVisible(namebuffer = new EditorTextBox());
  namebuffer->addListener(this);

  addAndMakeVisible(layerbutton = new EditorButton ("Add Plot Layer"));
  layerbutton->addListener(this);

  addAndMakeVisible(savebutton = new EditorButton ("Save..."));
  savebutton->addListener(this);

  addAndMakeVisible (bggroup = new juce::GroupComponent(juce::String(), "Background"));

  addAndMakeVisible(bggridcheckbox = new EditorCheckBox("Draw grid"));
  bggridcheckbox->addListener(this);

  addAndMakeVisible(bgplottingcheckbox = new EditorCheckBox("Back plots visible"));
  bgplottingcheckbox->addListener(this);

  addAndMakeVisible(bgmouseablecheckbox = new EditorCheckBox("Back plots mouseable"));
  bgmouseablecheckbox->addListener(this);

  addAndMakeVisible(bgcolorpicker = new juce::ColourSelector(juce::ColourSelector::showColourspace, 1, 0));
  bgcolorpicker->addChangeListener(this);

  addAndMakeVisible(fieldgroup=new juce::GroupComponent(juce::String(),"Point Fields"));

  addAndMakeVisible(fieldxlabel = new EditorLabel("X Axis:"));
  addAndMakeVisible (fieldxmenu = new juce::ComboBox (juce::String()));
  fieldxmenu->setEditableText(false);
  fieldxmenu->setJustificationType (juce::Justification::centredLeft);

  addAndMakeVisible(fieldylabel = new EditorLabel("Y Axis:"));
  addAndMakeVisible (fieldymenu = new juce::ComboBox (juce::String()));
  fieldymenu->setEditableText(false);
  fieldymenu->setJustificationType (juce::Justification::centredLeft);
  fieldxmenu->addListener(this);
  fieldymenu->addListener(this);
  setEnabled(false);
}

PlotWindowEditor::~PlotWindowEditor ()
{
  deleteAllChildren();
}

void PlotWindowEditor::updateFromActivePlot(Plotter* plotter, bool statusHasChanged)
{
  if (!statusHasChanged) return;
  ////  std::cout << "PlotWindowEditor::updateFromActivePlot\n";
  if (plotter)
  {
    setEnabled(true);
    namebuffer->setText(plotter->getWindow()->getWindowTitle(), false);
    bggridcheckbox->setToggleState(plotter->bgGrid, juce::dontSendNotification);
    int n = plotter->numLayers();
    bgplottingcheckbox->setToggleState(plotter->bgPlotting, juce::dontSendNotification);
    bgplottingcheckbox->setEnabled((n > 1));
    bgmouseablecheckbox->setToggleState(plotter->bgMouseable, juce::dontSendNotification);
    bgmouseablecheckbox->setEnabled((n > 1) && plotter->bgPlotting);
    bgcolorpicker->setCurrentColour(plotter->bgColor);
    fieldxmenu->clear(juce::dontSendNotification);
    fieldymenu->clear(juce::dontSendNotification);
    for (int i = 0; i < plotter->numFields(); i++)
    {
      if (plotter->getHorizontalAxis() == plotter->getFieldAxis(i))
      {
        fieldxmenu->addItem(plotter->getFieldName(i), i + 1);
        if (plotter->isSharedField(i))
          fieldxmenu->setItemEnabled(i + 1, false);
        else
          fieldxmenu->setSelectedId(i + 1, juce::dontSendNotification);
      }
      else
      {
        fieldymenu->addItem(plotter->getFieldName(i), i + 1);
        if (plotter->isSharedField(i))
          fieldymenu->setItemEnabled(i + 1, false);
        else if (plotter->getVerticalAxis() == plotter->getFieldAxis(i))
          fieldymenu->setSelectedId(i + 1, juce::dontSendNotification);
      }
    }
    bgcolorpicker->addChangeListener(this);
  }
  else
  {
    namebuffer->setText("", false);
    bggridcheckbox->setToggleState(false, juce::dontSendNotification);
    bgplottingcheckbox->setToggleState(false, juce::dontSendNotification);
    bgmouseablecheckbox->setToggleState(false, juce::dontSendNotification);
    bgcolorpicker->removeAllChangeListeners();
    setEnabled(false);
  }
}

void PlotWindowEditor::resized()
{
  int m = Graphics::Margin;
  int l = Graphics::LineHeight;
  int x = m;
  int y = m;
  int z = 0;
  int s = Graphics::ItemSpacer;
  int w = 0;
  namelabel->setTopLeftPosition(x, y);
  namebuffer->setBounds(namelabel->getRight() + z, y, 220, Graphics::LineHeight);
  layerbutton->setTopLeftPosition(namebuffer->getRight() + s, y);
  savebutton->setTopLeftPosition(layerbutton->getRight() + s, y);
  x = m;
  y += Graphics::LineAndSpace;
  bggroup->setTopLeftPosition(x, y);
  y += m * 2;
  x += m;
  bggridcheckbox->setTopLeftPosition(x, y);
  y += m * 2 + 3;
  bgplottingcheckbox->setTopLeftPosition(x, y);
  y += m * 2 + 3;
  bgmouseablecheckbox->setTopLeftPosition(x, y);
  bgcolorpicker->setBounds(bgmouseablecheckbox->getRight() + Graphics::ItemSpacer, 
                           bggridcheckbox->getY() - 4, 120, lineheight * 2 + m * 2); 
  bggroup->setSize(bgcolorpicker->getRight()-bggridcheckbox->getX()+(m * 2), 
                   bgmouseablecheckbox->getBottom() - bggroup->getY() + m - 4);
  x = bggroup->getRight() + m;
  y = bggroup->getY();
  fieldgroup->setBounds(x, y, getWidth() - x - m, bggroup->getHeight());
  y += m * 2;
  x += m;
  fieldxlabel->setTopLeftPosition(x, y);
  w = fieldgroup->getRight() - fieldxlabel->getRight() - m;
  fieldxmenu->setBounds(fieldxlabel->getRight() + z, y, w, l);
  y += Graphics::LineAndSpace;
  fieldylabel->setTopLeftPosition(x, y);
  fieldymenu->setBounds(fieldxlabel->getRight() + z, y, w, l);
}

void PlotWindowEditor::buttonClicked (juce::Button* buttonThatWasClicked)
{
  ////  std::cout << "PlotWindowEditor::buttonClicked\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotWindowEditor::buttonClicked: no plotter!\n"; return;}
  if (buttonThatWasClicked == savebutton)
  {
    PlotWindow* window = plotter->getWindow();
    window->save();
  }
  else if (buttonThatWasClicked == layerbutton)
  {
    if (Layer* layer = plotter->newLayer(0, true))
    {
      controls->addEditor(new PlotLayerEditor(controls, layer));
      updateFromActivePlot(plotter, true);
    }
  }
  else if (buttonThatWasClicked == bggridcheckbox)
  {
    plotter->bgGrid=bggridcheckbox->getToggleState();
    plotter->redrawBackView();
  }
  else if (buttonThatWasClicked == bgplottingcheckbox)
  {
    plotter->bgPlotting = bgplottingcheckbox->getToggleState();
    bgmouseablecheckbox->setEnabled(bgplottingcheckbox->getToggleState());
    plotter->redrawBackView();
  }
  else if (buttonThatWasClicked == bgmouseablecheckbox)
  {
    plotter->bgMouseable = bgmouseablecheckbox->getToggleState();
  }
}

void PlotWindowEditor::textEditorReturnKeyPressed(juce::TextEditor& editor) 
{
  ////  std::cout << "PlotWindowEditor::textEditorReturnKeyPressed\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotWindowEditor::textEditorReturnKeyPressed: no plotter!\n"; return;}
  EditorTextBox* ed = dynamic_cast<EditorTextBox*>(&editor);
  if (ed)
    ed->setTextChanged(false);
  else
    return;
  if (ed == namebuffer)
  {
    PlotWindow* w = plotter->getWindow();
    juce::String n = namebuffer->getTrimmedText();
    if (n.isNotEmpty())
      w->setWindowTitle(n);
  }
}

void PlotWindowEditor::changeListenerCallback (juce::ChangeBroadcaster* source)
{
  ////  std::cout << "PlotWindowEditor::changeListenerCallback\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotWindowEditor::changeListenerCallback: no plotter!\n"; return;}
  if (source == bgcolorpicker)
  {
    juce::Colour color = bgcolorpicker->getCurrentColour();
    plotter->bgColor = color;
    plotter->redrawBackView();
  }
}

void PlotWindowEditor::comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged)
{
  ////  std::cout << "PlotWindowEditor::comboBoxChanged\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotWindowEditor::comboBoxChanged: no plotter!\n"; return;}
  if (comboBoxThatHasChanged == fieldxmenu)
  {
  }
  else if (comboBoxThatHasChanged == fieldymenu)
  {
  }
}

/*=======================================================================*
  Audio Editor
  *=======================================================================*/

PlotAudioEditor::PlotAudioEditor(PlottingControls* controls)
  : PlotEditor(controls),
    ycheckbox (0),
    y0label (0),
    y0typein (0),
    y1label (0),
    y1typein (0),
    transport (0),
    tuninglabel (0),
    tuningmenu(0)
{
  tabType = AudioEditor;
  setVisible(true);
  setName("Audio");

  addAndMakeVisible(ycheckbox = new EditorCheckBox("Vertical scaling"));
  ycheckbox->addListener(this);
  ycheckbox->setColour(juce::TextEditor::focusedOutlineColourId, juce::Colour(0x00));

  addAndMakeVisible (y0label = new EditorLabel( "min:"));
  addAndMakeVisible (y0typein = new EditorTextBox (""));
  y0typein->addListener(this);

  addAndMakeVisible (y1label = new EditorLabel( "max:"));
  addAndMakeVisible (y1typein = new EditorTextBox (""));
  y1typein->addListener(this);

  addAndMakeVisible(tuninglabel = new EditorLabel("Tuning:"));
  addAndMakeVisible (tuningmenu = new juce::ComboBox (juce::String()));
  tuningmenu->setEditableText (false);
  tuningmenu->setJustificationType (juce::Justification::centredLeft);
  tuningmenu->addItem ("Semitone", 1);
  tuningmenu->addItem ("Quartertone", 2);
  tuningmenu->setSelectedId(1, juce::dontSendNotification);
  tuningmenu->addListener (this);
  setEnabled(false);
}

PlotAudioEditor::~PlotAudioEditor()
{
  deleteAndZero(ycheckbox);
  deleteAndZero(y0label);
  deleteAndZero(y0typein);
  deleteAndZero(y1label);
  deleteAndZero(y1typein);
  deleteAndZero(tuninglabel);
  deleteAndZero(tuningmenu);
  deleteAndZero(transport);
}

void PlotAudioEditor::updateFromActivePlot(Plotter* plotter, bool statusHasChanged)
{
  if (!statusHasChanged) return;
  ////  std::cout << "PlotAudioEditor::updateFromActivePlot\n";
  setEnabled(false);
// commented out to stop 'Code will never be executed message.'
// if (1) return;
//  if (plotter)
//  {
//    ycheckbox->setToggleState(plotter->pbVerticalRescale, juce::dontSendNotification);
//    y0typein->setText(juce::String(plotter->pbMinKey), false);
//    y1typein->setText(juce::String(plotter->pbMaxKey), false);
//    // create a Transport for the Plotter
//    //    double tempo= (plotter->getHorizontalAxis()->isType(Axis::percentage)) ? 500.0 : 60.0;
//    /*
//      FIXME
//      addAndMakeVisible (transport = new Transport(plotter, true, true));
//    */
//    // configure the playback thread for running
//    plotter->pbThread->setTransport(transport);
//    plotter->pbThread->setPlaybackLimit( plotter->getHorizontalAxis()->getMaximum());
//    plotter->pbThread->startThread();
//  }
//  else
//  {
//  }

}

void PlotAudioEditor::resized()
{
  int l = Graphics::LineHeight;
  int m = Graphics::Margin;
  int x = m;
  int y = m;
  int s = Graphics::LineAndSpace;
  int h = Graphics::HalfMargin;
  int w = (int)y0typein->getFont().getStringWidthFloat("XXXXX");
  ////transport->setTopLeftPosition((getWidth()/2)-(Transport::TransportWidthNoTempo/2), y);
  ycheckbox->setTopLeftPosition(x, y);
  x = ycheckbox->getRight()+0;
  y0label->setTopLeftPosition(x,y);
  x = y0label->getRight()+0;
  y0typein->setBounds(x,y,w,l);
  x = y0typein->getRight()+0;
  y1label->setTopLeftPosition(x,y);
  x = y1label->getRight()+0;
  y1typein->setBounds(x,y,w,l);  
  x = y1typein->getRight()+(Graphics::ItemSpacer*2);
  x=m;
  y += s;
  tuninglabel->setTopLeftPosition(x,y);
  x=tuninglabel->getRight()+h;
  tuningmenu->setBounds(x,y, 120, l);
  // transport display centered buttons
  y += s;
  transport->setTopLeftPosition((getWidth()/2)-(transport->getWidth()/2), y);
}

void PlotAudioEditor::buttonClicked (juce::Button* buttonThatWasClicked)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAudioEditor::buttonClicked: no plotter!\n"; return;}

  if (buttonThatWasClicked == ycheckbox)
  {
    juce::ScopedLock mylock (plotter->pbLock);
    bool state=ycheckbox->getToggleState();
    plotter->pbVerticalRescale=state;
    y0label->setEnabled(state);
    y0typein->setEnabled(state);
    y1label->setEnabled(state);
    y1typein->setEnabled(state);
  }
}

void PlotAudioEditor::comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAudioEditor::comboBoxChanged: no plotter!\n"; return;}

  if (comboBoxThatHasChanged == tuningmenu)
  {
    juce::ScopedLock mylock (plotter->pbLock);
    plotter->pbTuning=tuningmenu->getSelectedId();
    plotter->pbThread->sendMicrotuning(plotter->pbTuning);
  }
}

void PlotAudioEditor::textEditorReturnKeyPressed(juce::TextEditor& editor)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAudioEditor::textEditorReturnKeyPressed: no plotter!\n"; return;}

  ////  std::cout << "PlotAudioEditor::textEditorReturnKeyPressed\n";
  EditorTextBox* ed=dynamic_cast<EditorTextBox*>(&editor);
  if (ed) ed->setTextChanged(false); else return;

  if (&editor == y0typein)
  {
    juce::ScopedLock mylock (plotter->pbLock);
    double key=y0typein->getDoubleValue();
    if (y0typein->isNumericText() && (key >= 0.0) && (key <= 127.0))
      plotter->pbMinKey=key;
    else
      y0typein->setText(juce::String(plotter->pbMinKey), false);      
  }
  else if (&editor == y1typein)
  {
    juce::ScopedLock mylock (plotter->pbLock);
    double key=y1typein->getDoubleValue();
    if (y1typein->isNumericText() && (key >= 0.0) && (key <= 127.0))
      plotter->pbMaxKey=key;
    else
      y1typein->setText(juce::String(plotter->pbMaxKey), false);
  }
}

/*=======================================================================*
  Axis Tab
  *=======================================================================*/

PlotAxisEditor::PlotAxisEditor (PlottingControls* controls, int orient)
  : PlotEditor(controls),
    orientation(orient),
    fromlabel (0),
    tolabel (0),
    bylabel (0),
    tickslabel (0),
    typemenu (0),
    namebuffer (0),
    frombuffer (0),
    tobuffer (0),
    bybuffer (0),
    ticksbuffer (0),
    decimalsmenu (0),
    zoomlabel (0),
    zoomslider (0),
    fitcheckbox (0)
{
  tabType = AxisEditor;
  setVisible(true);
  setName((orientation == Plotter::horizontal) ? "X Axis" : "Y Axis");

  addAndMakeVisible(fromlabel = new EditorLabel("From:"));
  addAndMakeVisible(frombuffer = new EditorTextBox(""));
  frombuffer->addListener(this);

  addAndMakeVisible(tolabel = new EditorLabel("To:"));
  addAndMakeVisible(tobuffer = new EditorTextBox(""));
  tobuffer->addListener(this);

  addAndMakeVisible (bylabel = new EditorLabel ("By:"));
  addAndMakeVisible (bybuffer = new EditorTextBox(""));
  bybuffer->addListener(this);

  addAndMakeVisible(typelabel = new EditorLabel("Type:"));
  addAndMakeVisible (typemenu = new juce::ComboBox (juce::String()));
  typemenu->setEditableText (false);
  typemenu->setJustificationType (juce::Justification::centredLeft);
  typemenu->addItem ("Normalized", Axis::normalized);
  typemenu->addItem ("Percentage", Axis::percentage);
  typemenu->addItem ("Unit", Axis::circle);
  if (orientation == Plotter::horizontal)
  {
    typemenu->addItem ("Ordinal", Axis::ordinal);
    typemenu->addItem ("Seconds", Axis::seconds);
  }
  if (orientation == Plotter::vertical)
  {
    typemenu->addItem ("Note",  Axis::keynum);
    typemenu->addItem ("Midi", Axis::midi);
  }
  typemenu->addItem ("Custom", Axis::generic);
  typemenu->addListener (this);

  addAndMakeVisible(tickslabel = new EditorLabel("Ticks:"));
  addAndMakeVisible(ticksbuffer = new EditorTextBox(""));
  ticksbuffer->addListener(this);

  //  addAndMakeVisible(decimalslabel=new EditorLabel("Decimals:"));
  //  addAndMakeVisible (decimalsmenu = new juce::ComboBox (juce::String()));
  //  decimalsmenu->setEditableText (false);
  //  decimalsmenu->setJustificationType (juce::Justification::centredLeft);
  //  decimalsmenu->setTextWhenNothingSelected (juce::String());
  //  decimalsmenu->setTextWhenNoChoicesAvailable ("(no choices)");
  //  decimalsmenu->addItem ("0", 1);
  //  decimalsmenu->addItem ("1", 2);
  //  decimalsmenu->addItem ("2", 3);
  //  decimalsmenu->addItem ("3", 4);
  //  decimalsmenu->addListener (this);

  addAndMakeVisible(zoomlabel = new EditorLabel ("Zoom:"));
  addAndMakeVisible(zoomslider = new juce::Slider (juce::String()));
  zoomslider->setSliderStyle(juce::Slider::LinearHorizontal);
  zoomslider->setRange (0.1, 10, .05);
  zoomslider->setTextBoxStyle(juce::Slider::TextBoxLeft, false, 80, 20);
  zoomslider->addListener(this);

  addAndMakeVisible(fitcheckbox = new EditorCheckBox("Fit in view"));
  fitcheckbox->addListener(this);
  setEnabled(false);
}

PlotAxisEditor::~PlotAxisEditor()
{
  deleteAllChildren();
}

void PlotAxisEditor::updateFromActivePlot(Plotter* plotter, bool statusHasChanged)
{
  if (!statusHasChanged) return;
  ////  std::cout << "PlotAxisEditor::updateFromActivePlot\n";
  if (plotter)
  {
    setEnabled(true);
    AxisView* axisview = plotter->getAxisView(orientation);
    Axis* axis = axisview->getAxis();
    //    namebuffer->setText(axis->getName(), false);
    typemenu->setSelectedId(axis->getType(), juce::dontSendNotification);
    // no changing types if points have been added!
    //    typemenu->setEnabled(plotter->isEmpty());
    frombuffer->setText(juce::String(axis->getMinimum()), false);
    tobuffer->setText(juce::String(axis->getMaximum()), false);
    bybuffer->setText(juce::String(axis->getIncrement()), false);
    ticksbuffer->setText(juce::String(axis->getTicks()), false);
    //    decimalsmenu->setSelectedId(axis->getDecimals() + 1, juce::dontSendNotification);
    zoomslider->setValue(axisview->getSpread(), juce::dontSendNotification);
    zoomslider->setEnabled(!axisview->isFitInView());
    fitcheckbox->setToggleState(axisview->isFitInView(), juce::dontSendNotification);
    //FIXME this has to be cleaned up!!!
    //non-fit in view has to get fixed
    fitcheckbox->setEnabled(false);
    updateForAxisType(axis->getType());
  }
  else
  {
    setEnabled(false);
  }
}

void PlotAxisEditor::updateForAxisType(int type)
{
  switch (type)
  {
  case Axis::normalized:
  case Axis::percentage:
  case Axis::circle:
    frombuffer->setEnabled(false);
    tobuffer->setEnabled(false);
    bybuffer->setEnabled(true);
    break;
  case Axis::keynum:
  case Axis::midi:
  case Axis::generic:
  default:
    frombuffer->setEnabled(true);
    tobuffer->setEnabled(true);
    bybuffer->setEnabled(true);
  }
}

void PlotAxisEditor::resized()
{
  int m = Graphics::Margin;
  int x = m;
  int y = m;
  int z = 0;
  int w = (int)frombuffer->getFont().getStringWidthFloat("XXXXX");
  int s = m;
  int l = Graphics::LineHeight;
  //  namelabel->setTopLeftPosition(x, y);
  //  namebuffer->setBounds(namelabel->getRight() + z, y, 120, l);
  typelabel->setTopLeftPosition(x, y);//namebuffer->getRight() + s
  typemenu->setBounds(typelabel->getRight() + z, y, 96, l);
  //  decimalslabel->setTopLeftPosition(typemenu->getRight() + s, y);
  //  decimalsmenu->setBounds(decimalslabel->getRight() + z, y, w, l);
  // line 2
  y += Graphics::LineAndSpace;
  fromlabel->setTopLeftPosition(x, y);
  frombuffer->setBounds(fromlabel->getRight() + z, y, w, l);
  tolabel->setTopLeftPosition(frombuffer->getRight() + s, y);
  tobuffer->setBounds(tolabel->getRight() + z, y, w, l);
  bylabel->setTopLeftPosition(tobuffer->getRight() + s, y);
  bybuffer->setBounds(bylabel->getRight() + z, y, w, l);
  tickslabel->setTopLeftPosition(bybuffer->getRight() + s, y);
  ticksbuffer->setBounds(tickslabel->getRight() + z, y, w, l);
  // line 3
  y += Graphics::LineAndSpace;
  zoomlabel->setTopLeftPosition(x, y);
  zoomslider->setBounds(56, y, 280, 24);
  fitcheckbox->setBounds(zoomslider->getRight() + s, y, 180, 24);
}

void PlotAxisEditor::comboBoxChanged (juce::ComboBox* cbox)
{
  ////  std::cout << "PlotAxisEditor::comboBoxChanged\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAxisEditor::comboBoxChanged: no plotter!\n"; return;}
  if (cbox == typemenu)
  {
    if (plotter->isNotEmpty())
    {
      juce::String text ("You cannot change the axis type of a plot that contains points. Delete all points and try again.");
      juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                        "Change Axis", text);
      return;
    }
    // selecting from type menu installs the defaults
    Axis a(0);
    a.init((Axis::AxisType)cbox->getSelectedId());
    //    namebuffer->setText(juce::String(a.getName()), juce::dontSendNotification);
    frombuffer->setText(juce::String(a.getMinimum()), juce::dontSendNotification);
    tobuffer->setText(juce::String(a.getMaximum()), juce::dontSendNotification);
    bybuffer->setText(juce::String(a.getIncrement()), juce::dontSendNotification);
    ticksbuffer->setText(juce::String(a.getTicks()), juce::dontSendNotification);
    //    decimalsmenu->setSelectedId(ax.getDecimals()+1, juce::dontSendNotification);
    // enable or disable buffers based on new axis type
    updateForAxisType(a.getType());
    AxisView* axisview = plotter->getAxisView(orientation);
    Axis* axis = axisview->getAxis();
    axis->setType(a.getType());
    axis->setMinimum(a.getMinimum());
    axis->setMaximum(a.getMaximum());
    axis->setIncrement(a.getIncrement());
    axis->setTicks(a.getTicks());
    axisview->setSpreadToFit((orientation == Plotter::horizontal) ? axisview->getWidth() : axisview->getHeight());
    //    plotter->fitInView(plotter->getWidth()-80, plotter->getWidth()-60);
    //    plotter->resized();
    plotter->fitInView();
    plotter->redrawAll();
    //    plotter->getWindow()->repaint();
  }
  else if (cbox == decimalsmenu)
  {
    //    AxisView* axisview = plotter->getAxisView(orientation);
    //    axisview->getAxis()->setDecimals(cbox->getSelectedId() - 1);
    //    axisview->repaint();
  } 
}

void PlotAxisEditor::sliderValueChanged (juce::Slider* sliderThatWasMoved)
{
  ////  std::cout << "PlotAxisEditor::comboBoxChanged\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAxisEditor::sliderValueChanged: no plotter!\n"; return;}
  AxisView* axisview = plotter->getAxisView(orientation);
  if (sliderThatWasMoved == zoomslider)
  {
    axisview->setSpread(sliderThatWasMoved->getValue());
    plotter->resizeForSpread();
    plotter->redrawAll();
    //plotter->getWindow()->repaint();
  }
}

void PlotAxisEditor::buttonClicked (juce::Button* buttonThatWasClicked)
{
  ////  std::cout << "PlotAxisEditor::buttonClicked\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAxisEditor::buttonClicked: no plotter!\n"; return;}
  AxisView* axisview = plotter->getAxisView(orientation);
  if (buttonThatWasClicked == fitcheckbox)
  {
    if (fitcheckbox->getToggleState())
    {
      axisview->setFitInView(true);
      zoomlabel->setEnabled(false);
      zoomslider->setValue(axisview->getSpread(), juce::dontSendNotification);
      zoomslider->setEnabled(false);
      plotter->fitInView(plotter->getWidth()-80, plotter->getWidth()-60);
      plotter->resized();
    }
    else
    {
      axisview->setFitInView(false);
      zoomlabel->setEnabled(true);
      zoomslider->setEnabled(true);
      zoomslider->setValue(axisview->getSpread(), juce::dontSendNotification);
    }
  }
}

void PlotAxisEditor::textEditorReturnKeyPressed (juce::TextEditor& editor)
{
  ////  std::cout << "PlotAxisEditor::textEditorReturnKeyPressed\n";
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotAxisEditor::textEditorReturnKeyPressed: no plotter!\n"; return;}
  EditorTextBox* ed=dynamic_cast<EditorTextBox*>(&editor);
  if (ed) ed->setTextChanged(false); else return;

  double val;
  AxisView* axisview = plotter->getAxisView(orientation);
  Axis* axis=axisview->getAxis();
  bool redraw=false;

  if (&editor == namebuffer)
  {
    //    axis->setName(editor.getText());
  }
  else if (&editor == frombuffer)
  {
    val = editor.getText().getDoubleValue();
    if (val < axis->getMaximum())
    {
      axis->setMinimum(val);
      redraw = true;
    }
    else
      editor.setText(juce::String(axis->getMinimum()), true);
  }
  else if (&editor == tobuffer)
  {
    val = editor.getText().getDoubleValue();
    if (val > axis->getMinimum())
    {
      axis->setMaximum(val);
      redraw = true;
    }
    else
      editor.setText(juce::String(axis->getMaximum()), true);
  } 
  else if (&editor == bybuffer)
  {
    val = editor.getText().getDoubleValue();
    if (val > 0.0)
    {
      axis->setIncrement(val);
      redraw = true;
    }
    else
      editor.setText(juce::String(axis->getIncrement()), true);
  } 
  else if (&editor == ticksbuffer)
  {
    int num = editor.getText().getIntValue();
    if (num >=0)
    {
      axis->setTicks(num);
      redraw = true;
    }
    else
      editor.setText(juce::String(axis->getTicks()), true);
  } 
  if (redraw)
  {
    //    axisview->repaint();
    //plotter->resizeForSpread();
    plotter->fitInView(); // make sure value keeps it pinned
    plotter->redrawAll();
  }
}

/*=======================================================================*
  Layer Tab
  *=======================================================================*/

PlotLayerEditor::PlotLayerEditor (PlottingControls* controls, Layer* layer)
  : PlotEditor(controls),
    deletebutton (0),
    layer (layer),
    namelabel (0),
    namebuffer (0),
    stylelabel (0),
    stylemenu (0),
    linelabel (0),
    linetextbox (0),
    pointlabel (0),
    pointtextbox (0),
    barlabel (0),
    bartextbox (0),
    audiogroup (0),
    durlabel (0),
    durtypein(0),
    amplabel (0),
    amptypein (0),
    chanlabel (0),
    chantypein (0),
    colorlabel (0),
    colorpicker (0)
{
  tabType = LayerEditor;
  setVisible(true);
  setName(layer->getLayerName());

  addAndMakeVisible (namelabel = new EditorLabel( "Name:"));
  addAndMakeVisible (namebuffer = new EditorTextBox (""));
  namebuffer->addListener(this);
  addAndMakeVisible (deletebutton = new EditorButton ("Delete Plot..."));
  deletebutton->addListener(this);

  // point line bar editors need to exist when style menu configs
  addAndMakeVisible(linelabel = new EditorLabel( "Line:"));
  addAndMakeVisible(linetextbox = new EditorTextBox (""));
  linetextbox->addListener(this);
  addAndMakeVisible(pointlabel = new EditorLabel( "Point:"));
  addAndMakeVisible (pointtextbox = new EditorTextBox (""));
  pointtextbox->addListener(this);
  addAndMakeVisible(barlabel = new EditorLabel( "Bar:"));
  addAndMakeVisible(bartextbox = new EditorTextBox (""));
  bartextbox->addListener(this);

  addAndMakeVisible (stylelabel = new EditorLabel ("Style:"));
  addAndMakeVisible(stylemenu = new juce::ComboBox (juce::String()));
  stylemenu->setEditableText(false);
  stylemenu->setJustificationType(juce::Justification::centredLeft);
  stylemenu->addItem("Envelope", Layer::lineandpoint);
  stylemenu->addItem("Line", Layer::line);
  stylemenu->addItem("Point", Layer::point);
  stylemenu->addItem("Impulse", Layer::impulse);
  stylemenu->addItem("Aerial", Layer::vlineandpoint);
  stylemenu->addItem("Bar", Layer::vbar);
  //  stylemenu->addItem("Horizontal Bar", Layer::hbar);
  stylemenu->addItem("Piano Roll", Layer::hbox);
  //  stylemenu->addItem("Vertical Box", Layer::vbox);
  // add the listener before choosing so the selection will update the
  // line, point and bar sizes
  stylemenu->addListener (this);

  addAndMakeVisible (audiogroup = new juce::GroupComponent(juce::String(), "Audio Defaults"));
  addAndMakeVisible (durlabel = new EditorLabel( "Dur:"));
  addAndMakeVisible (durtypein = new EditorTextBox (""));
  durtypein->setEnabled(false); // FIXME: audio disabled
  durtypein->addListener(this);
  addAndMakeVisible (amplabel = new EditorLabel( "Amp:"));
  addAndMakeVisible (amptypein = new EditorTextBox (""));
  amptypein->setEnabled(false); // FIXME: audio disabled
  amptypein->addListener(this);
  addAndMakeVisible (chanlabel = new EditorLabel( "Chan:"));
  addAndMakeVisible (chantypein = new EditorTextBox (""));
  chantypein->setEnabled(false); // FIXME: audio disabled
  chantypein->addListener(this);

  addAndMakeVisible (colorlabel = new EditorLabel( "Point Color:"));
  addAndMakeVisible(colorpicker = new juce::ColourSelector( juce::ColourSelector::showColourspace, 1, 0));
  colorpicker->setCurrentColour(layer->getLayerColor());
  colorpicker->addChangeListener(this);
  setEnabled(true);
  // the remainer was part of updateFromActivePlot
  setName(layer->getLayerName());
  deletebutton->setEnabled(controls->plotter->numLayers() > 1);
  linetextbox->setText(juce::String(layer->lineWidth), false);
  pointtextbox->setText(juce::String(layer->pointWidth), false);
  bartextbox->setText(juce::String(layer->barWidth), false);
  stylemenu->setSelectedId(layer->getLayerStyle(), juce::dontSendNotification);
  if (controls->plotter->numFields() <= 2)
  {
    stylemenu->setItemEnabled(Layer::hbox, false);
  }
  durtypein->setText(juce::String(layer->pbDur), false);
  amptypein->setText(juce::String(layer->pbAmp), false);
  chantypein->setText(juce::String(layer->pbChan), false);
  // FIXME THIS SHOULD NOT COLORIZE PLOT!
  //  colorpicker->setCurrentColour(layer->getLayerColor());
}

PlotLayerEditor::~PlotLayerEditor()
{
  deleteAllChildren();
}

void PlotLayerEditor::updateFromActivePlot(Plotter* plotter, bool statusHasChanged)
{
  ////  std::cout << "PlotLayerEditor::updateFromActivePlot\n";
  if (plotter)
  {
    setEnabled(true);
  }
  else
  {
    setEnabled(false);
  }
}

void PlotLayerEditor::resized()
{
  int m = Graphics::Margin;
  int w = (int)namebuffer->getFont().getStringWidthFloat("XXXXX");
  int x = m;
  int y = m;
  int h = Graphics::HalfMargin;
  int z = 0;

  namelabel->setBounds(x, y, 48, lineheight);
  x = namelabel->getRight() + h;
  namebuffer->setBounds(x, y, 120, lineheight);
  x = namebuffer->getRight() + m;
  deletebutton->setTopLeftPosition(x,y);

  //  line 2
  x = m;
  y += Graphics::LineAndSpace;
  stylelabel->setTopLeftPosition(x, y);
  x = stylelabel->getRight() + h;
  stylemenu->setBounds(x, y, 120, lineheight);
 
  //  line 3
  x = stylemenu->getRight() + m;
  linelabel->setTopLeftPosition(x, y);
  x = linelabel->getRight() + z;
  linetextbox->setBounds(x, y, w, lineheight);
  x = linetextbox->getRight() + h;
  pointlabel->setTopLeftPosition(x, y);
  x = pointlabel->getRight() + z;
  pointtextbox->setBounds(x, y, w, lineheight);
  x = pointtextbox->getRight() + h;  
  barlabel->setTopLeftPosition(x, y);
  x = barlabel->getRight() + z;
  bartextbox->setBounds(x, y, w, lineheight);
  x = bartextbox->getRight() + Graphics::ItemSpacer;

  //  line 4
  y += Graphics::LineAndSpace;
  x = m;
  audiogroup->setTopLeftPosition(x, y);
  x += m;
  y += m*2;
  // dur
  durlabel->setTopLeftPosition(x, y);
  x = durlabel->getRight() + z;
  durtypein->setBounds(x, y, w, lineheight);  
  x = durtypein->getRight() + (m * 1);
  // amp
  amplabel->setTopLeftPosition(x, y);
  x = amplabel->getRight() + z;
  amptypein->setBounds(x, y, w, lineheight);  
  x = amptypein->getRight() + (m * 1);
  // chan
  chanlabel->setTopLeftPosition(x, y);
  x = chanlabel->getRight() + z;
  chantypein->setBounds(x, y, w, lineheight);  
  x = chantypein->getRight() + (m * 1);
  audiogroup->setSize(x-audiogroup->getX(), lineheight+m * 3);
  colorlabel->setTopLeftPosition(audiogroup->getRight() + m, chanlabel->getY());
  // point color picker is large squre and is itself in dented
  colorpicker->setBounds(370, audiogroup->getY() - h, 120,
                         lineheight * 2 + m); //170*.66, space*.66
}

void PlotLayerEditor::buttonClicked (juce::Button* button)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotLayerEditor::buttonClicked: no plotter!\n"; return;}
  if (button == deletebutton)
  {
    juce::String text="Really delete layer '" + layer->getLayerName() + "'?";
    if (layer->numPoints() > 0) 
      text<<"\nAll point data will be lost.";
    if (!juce::AlertWindow::showOkCancelBox(juce::AlertWindow::WarningIcon, "Delete Layer",
                                            text, "Delete Layer", "Cancel", this))
      return;
    PlottingControls* controls = getPlottingControls();
    int index = controls->getTabIndex(this);
    //std::cout << "deleting plotter layer\n";
    plotter->removeLayer(layer);
    plotter->redrawBackView();
    plotter->redrawPlotView();
    delete layer;
    // select main tab because this one is going away!
    //std::cout << "selecting main tab\n";
    controls->setCurrentTabIndex(0);
    if (dynamic_cast<PlotWindowEditor*>(controls->getTabContentComponent(0)) != 0)
      updateFromActivePlot(plotter, true);
    // remove ourselves
    //std::cout << "removing ourselves\n";
    controls->removeTab(index);
    //std::cout << "updating layer editors\n";
    // delete ourselves!
    //std::cout << "deleting ourselves\n";
    delete this;
  }
}

void PlotLayerEditor::comboBoxChanged (juce::ComboBox* cbox)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotLayerEditor::comboBoxChanged: no plotter!\n"; return;}
  if (cbox == stylemenu)
  {
    int style = stylemenu->getSelectedId();
    layer->setLayerStyle(style);
    linelabel->setEnabled((style & Layer::line) == 0 ? false : true );
    linetextbox->setEnabled(linelabel->isEnabled() );
    pointlabel->setEnabled((style & Layer::point) == 0 ? false : true);
    pointtextbox->setEnabled(pointlabel->isEnabled() );
    barlabel->setEnabled((style & (Layer::bar | Layer::box)) == 0 ? false : true);
    bartextbox->setEnabled(barlabel->isEnabled());
    plotter->redrawPlotView();
  }
}

void PlotLayerEditor::changeListenerCallback (juce::ChangeBroadcaster* source)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotLayerEditor::changeListenerCallback: no plotter!\n"; return;}
  // FIXME: this should only color the layer if the USER is setting
  // it, not updateForActivePlot()
  if (source == colorpicker)
  {
    juce::Colour color=colorpicker->getCurrentColour();
    layer->setLayerColor(color);
    if (layer->isPoints())
      plotter->redrawPlotView();
    plotter->redrawHorizontalAxisView();
    plotter->redrawVerticalAxisView();
  }
}

void PlotLayerEditor::textEditorReturnKeyPressed(juce::TextEditor& editor)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotLayerEditor::textEditorReturnKeyPressed: no plotter!\n"; return;}
  EditorTextBox* ed=dynamic_cast<EditorTextBox*>(&editor);
  if (ed) ed->setTextChanged(false); else return;

  if (ed == namebuffer)
  {
    juce::String name=namebuffer->getTrimmedText();
    if (name.isEmpty())
      namebuffer->setText(layer->getLayerName(), juce::dontSendNotification);
    else
    {
      setName(name);
      layer->setLayerName(name);
      tabButton->setButtonText(name);
    }
  }
  else if (ed == linetextbox)
  {
    double val=linetextbox->getDoubleValue();
    //std::cout << "linetextbox, val=" << val << "\n";
    if (linetextbox->isNumericText() && (val > 0.0) && (val <= 16.0))
    {
      layer->lineWidth=val;
      plotter->redrawPlotView();
    }
    else
      linetextbox->setText(juce::String(layer->lineWidth), false);
  }
  else if (ed == pointtextbox)
  {
    double val=pointtextbox->getDoubleValue();
    //std::cout << "linetextbox, val=" << val << "\n";
    if (pointtextbox->isNumericText() && (val > 0.0) && (val <= 24.0))
    {
      layer->pointWidth=val;
      plotter->redrawPlotView();
    }
    else
      pointtextbox->setText(juce::String(layer->pointWidth), false);
  }
  else if (ed == bartextbox)
  {
    double val=bartextbox->getDoubleValue();
    //std::cout << "linetextbox, val=" << val << "\n";
    if (bartextbox->isNumericText() && (val > 0.0) && (val <= 48.0))
    {
      layer->barWidth=val;
      plotter->redrawPlotView();
    }
    else
      bartextbox->setText(juce::String(layer->barWidth), false);
  }
  else if (ed == durtypein)
  {
    juce::ScopedLock mylock(plotter->pbLock);
    double dur=durtypein->getDoubleValue();
    if (durtypein->isNumericText() && (dur > 0.0))
    {
      //std::cout << "setting layer dur to " << dur << "\n";
      layer->pbDur=dur;
    }
    else
      durtypein->setText(juce::String(layer->pbDur), false);
  }
  else if (ed == amptypein)
  {
    juce::ScopedLock mylock(plotter->pbLock);
    double amp=amptypein->getDoubleValue();
    if (amptypein->isNumericText() && (amp >= 0.0))
    {
      //std::cout << "setting layer amp to " << amp << "\n";

      layer->pbAmp=amp;
    }
    else
      amptypein->setText(juce::String(layer->pbAmp), false);
  }
  else if (ed == chantypein)
  {
    juce::ScopedLock mylock(plotter->pbLock);
    int chan=chantypein->getIntValue();
    if (chantypein->isNumericText(false) && (chan >= 0 && chan <= 15))
      layer->pbChan=chan;
    else
      chantypein->setText(juce::String(layer->pbChan), false);
  }
}

/*=======================================================================*
  Export Editor
  *=======================================================================*/

PlotExportEditor::PlotExportEditor (PlottingControls* controls)
  : PlotEditor(controls),
    exportLabel(0),
    focusButton(0),
    allButton(0),
    formatlabel (0),
    formatmenu (0),
    fieldsbutton (0),
    decimalslabel (0),
    decimalsmenu (0),
    destlabel (0),
    destmenu (0),
    exportbutton (0)
{
  tabType = ExportEditor;
  setVisible(true);
  setName("Export");

  addAndMakeVisible(exportLabel = new EditorLabel("Export from:"));
  addAndMakeVisible(focusButton = new EditorCheckBox( "Front plot"));
  focusButton->setRadioGroupId(1);
  addAndMakeVisible(allButton = new EditorCheckBox( "All plots"));
  allButton->setRadioGroupId(1);
  allButton->setToggleState(true, juce::dontSendNotification);
  addAndMakeVisible(formatlabel = new EditorLabel( "Export as:"));
  addAndMakeVisible (formatmenu = new juce::ComboBox (juce::String()));
  formatmenu->setEditableText (false);
  formatmenu->addItem ("Point Values", ExportPoints);
  formatmenu->addItem ("Records", ExportRecords);

  formatmenu->setSelectedId(ExportPoints, juce::dontSendNotification);
  formatmenu->addListener(this);

  addAndMakeVisible(fieldsbutton = new EditorButton ("Include Fields..."));
  fieldsbutton->addListener(this);

  addAndMakeVisible(decimalslabel=new EditorLabel("Precision:"));
  addAndMakeVisible(decimalsmenu = new juce::ComboBox(juce::String()));
  decimalsmenu->addItem("No decimals", 1);
  decimalsmenu->addItem("One decimal", 2);
  decimalsmenu->addItem("Two decimals", 3);
  decimalsmenu->addItem("Three decimals", 4);
  decimalsmenu->setSelectedId(3, juce::dontSendNotification);

  addAndMakeVisible(destlabel=new EditorLabel("Destination:"));
  addAndMakeVisible(destmenu = new juce::ComboBox(juce::String()));
  destmenu->addItem("New Editor", ExportToNewEditor);
  destmenu->addItem("Clipboard", ExportToClipboard);
  destmenu->setSelectedId(ExportToNewEditor, juce::dontSendNotification);

  addAndMakeVisible(exportbutton = new EditorButton("Export"));
  exportbutton->addListener(this);
  setEnabled(false);
}

PlotExportEditor::~PlotExportEditor()
{
  include.clear();
  deleteAllChildren();
}

void PlotExportEditor::updateFromActivePlot(Plotter* plotter, bool statusHasChanged)
{
  if (!statusHasChanged) return;
  ////  std::cout << "PlotLayerEditor::updateFromActivePlot\n";
  if (plotter)
  {
    setEnabled(true);
    include.clear();
    numfields = plotter->numFields();
    for (int i = 0; i < numfields; i++)
      include.add(true);
  }
  else
  {
    setEnabled(false);
  }
}

void PlotExportEditor::resized()
{
  int m = Graphics::Margin;
  int x = m;
  int y = m;
  exportLabel->setTopLeftPosition(x, y);
  focusButton->setTopLeftPosition(exportLabel->getRight() + m, y);
  allButton->setTopLeftPosition(focusButton->getRight() + m, y);
  fieldsbutton->setTopLeftPosition(allButton->getRight() + (m * 3), y);
  // line 2
  x = m;
  y += Graphics::LineAndSpace;
  formatlabel->setTopLeftPosition(x, y);
  formatmenu->setBounds(formatlabel->getRight() + m, y, 115, lineheight);
  decimalslabel->setTopLeftPosition(formatmenu->getRight() + (m * 3), y);
  decimalsmenu->setBounds (decimalslabel->getRight() + m, y, 120, lineheight);
 
  // line 3
  x = m;
  y += Graphics::LineAndSpace;
  destlabel->setTopLeftPosition(x, y);
  destmenu->setBounds(destlabel->getRight() + m, y, 160, lineheight);
  exportbutton->setBounds (destmenu->getRight() + (m * 3), y, 90, lineheight);
}


void PlotExportEditor::buttonClicked (juce::Button* button)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotExportEditor::buttonClicked: no plotter!\n"; return;}
  if (button == exportbutton)
  {
    exportPoints();
  }
  else if (button == fieldsbutton)
  {
    int i;
    juce::PopupMenu m;
    for (i = 0; i < numfields; i++)
      m.addItem(i + 1, plotter->getFieldName(i), true, include[i]);
    i = m.showAt(button);
    if (i > 0)
      include.set(i - 1, !include[i - 1]);
  }
}

void PlotExportEditor::comboBoxChanged(juce::ComboBox* cbox)
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotExportEditor::comboBoxChanged: no plotter!\n"; return;}
  if (cbox == formatmenu)
  {
    int id = cbox->getSelectedId();
    if (id == ExportPoints || ExportRecords) // export layer
    {
      fieldsbutton->setEnabled(true);
      decimalslabel->setEnabled(true);
      decimalsmenu->setEnabled(true);
    }
    else // export plot as xml
    {
      fieldsbutton->setEnabled(false);
      decimalslabel->setEnabled(false);
      decimalsmenu->setEnabled(false);
    }
  }
  else
  {
  }      
}


void PlotExportEditor::exportPoints() 
{
  Plotter* plotter = getPlotter();
  if (!plotter){std::cout << "PlotExportEditor::exportPoints: no plotter!\n"; return;}
  int decimals = decimalsmenu->getSelectedId() - 1;
  int exportwhat = formatmenu->getSelectedId();
  int target = destmenu->getSelectedId();
  int numparams = 0;
  int parammask = 0;
  
  // turn included fields into bitmask
  for (int i = 0; i < numfields; i++) 
    if (include[i])
    {
      parammask += 1 << i;
      numparams++;
    }
  // do nothing if user deselected all fields!
  if (numparams == 0)
    return;
  
  juce::String text ("");
  int exportid = Preferences::getInstance()->getIntProp("EditorSyntax", TextIDs::Lisp);
  bool asrecords = (exportwhat == ExportRecords);

  if (1)
  {
    text << plotter->getFocusLayer()->toString(exportid, decimals, asrecords, parammask)
         << "\n";
  }
/*
 else
  {
    for (int i = 0; i < plotter->numLayers(); i++)
      text << plotter->getLayer(i)->toString(exportid, decimals, asrecords, parammask)
           << "\n";
  }
*/
  if (target == ExportToNewEditor)
  {
    new CodeEditorWindow(juce::File(), text, exportid);
  }
  else if (target == ExportToClipboard)
  {
    juce::SystemClipboard::copyTextToClipboard(text);
  }
}

