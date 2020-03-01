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

#ifndef CM_PLOTWINDOW_H
#define CM_PLOTWINDOW_H

#include "Libraries.h"

///
/// Plot Window
///

class Plotter;
class PlottingControls;

class PlotWindow : public juce::DocumentWindow
{
public:

  struct PlotMessage : public juce::Message
  {
    int type;
    juce::String text;
    PlotMessage(int type, juce::String text) : type(type), text(text) {}
    ~PlotMessage(){}
  };

  class PlotWindowListener : public juce::MessageListener
  {
  public:
    void handleMessage (const juce::Message &message);
    PlotWindow* window;
    PlotWindowListener() : window(0) {}
  };
  PlotWindowListener listener;
  Plotter* plotter;
  juce::File plotfile;
  PlotWindow (juce::XmlElement* plot);
  PlotWindow (juce::String title, juce::MidiFile& midifile);
  ~PlotWindow ();
  juce::String getWindowTitle();
  void setWindowTitle(juce::String title);
  void init();
  Plotter* getPlotter() {return plotter;}
  void closeButtonPressed() ;
  void activeWindowStatusChanged();
  void addXmlPoints(juce::String xml);
  bool save(bool saveas = false);
  bool hasUnsavedChanges();
  juce::String toXmlString();
  juce::String getPlotTitle() {return getName();}
  void setPlotTitle(juce::String title){setName(title);}
  juce::File getPlotFile() {return plotfile;}
  void setPlotFile(juce::File fil) {plotfile=fil;}
  static PlotWindow* getPlotWindow(juce::String title) ;
  static PlotWindow* getPlotWindow() ;
  static void openWindowFromXml(void* str);
  static void browseForFileToOpen();
  static void openXmlFile(juce::File file);
  static void openMidiFile(juce::File file);
  static void openPlottingControlsWindow();
};

/*=======================================================================*
  PlottingControlsWindow
 *=======================================================================*/

///A singleton class that has a tabbed editor for working on plots.

struct PlottingControlsWindow : public juce::DocumentWindow
{
  ///Current X of window on screen
  static int screenX;
  ///Current Y of window on screen
  static int screenY;

  PlottingControlsWindow();

  ~PlottingControlsWindow()
  {
  }

  void closeButtonPressed()
  {
    delete this;
  }

  ///Cache the current position of the window so it will appear at the
  ///same location next time its opened
  void moved()
  {
    screenX = getX();
    screenY = getY();
  }

  static PlottingControlsWindow* getPlottingControlsWindow()
  {
    for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
    {
      if (PlottingControlsWindow* w = dynamic_cast<PlottingControlsWindow*>(juce::TopLevelWindow::getTopLevelWindow(i)))
        return w;
    }
    return 0;
  }

  
};


#endif

