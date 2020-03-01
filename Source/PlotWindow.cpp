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
#include "CodeEditor.h"
#include "Plot.h"
#include "PlotEditor.h"
#include "PlotWindow.h"
#include "Midi.h"
#include "Scheme.h"
#include "CmSupport.h"

int PlottingControlsWindow::screenX = -1;
int PlottingControlsWindow::screenY = -1;

PlottingControlsWindow::PlottingControlsWindow()
  : juce::DocumentWindow("", ColorThemeIDs::getWindowBackgroundColor(), DocumentWindow::closeButton)
{
  //    setUsingNativeTitleBar(true);
  setTitleBarHeight(16);
  setAlwaysOnTop(true);
  setContentOwned(new PlottingControls(), true);
  WindowTypes::setWindowType(this, WindowTypes::PlottingControls);
  addKeyListener(Grace::getApp().commandManager.getKeyMappings());
  if (screenX < 0 || screenY < 0)
  {
    // centreAroundComponent handles null plot window
    PlotWindow* p = dynamic_cast<PlotWindow*>(Grace::getApp().activeWindow);
    centreAroundComponent(p, getWidth(), getHeight());
  }
  else 
    setTopLeftPosition(screenX, screenY);
  setVisible(true);
}


void PlotWindow::openPlottingControlsWindow()
{
  PlottingControlsWindow* w = PlottingControlsWindow::getPlottingControlsWindow();
  if (w) return;
  w = new PlottingControlsWindow();
}


/*=======================================================================*
  PlotWindowComponent: window's content component that
  contains the plotter and the tabview
  *=======================================================================*/

class PlotWindowComponent : public juce::Component
{
  PlotWindow* plotwin;
public:
  static const int plotviewwidth=500;

  // total height of tabbed editor is 5 lineheight plus 5 margins: tab
  // is 1 line, content is 4 lines and 5 margins
  static const int tabviewheight=0;//(PlotEditor::lineheight*5)+(PlotEditor::margin*5);

  PlotWindowComponent (PlotWindow* win) : plotwin (win) {}

  ~PlotWindowComponent () 
  {
    deleteAllChildren();
  }
  void resized ()
  {
    int w = getWidth();
    int h = getHeight();
    //    std::cout << "PlotWindowComponent::resized(" << w << ", " << h << ")\n";
    plotwin->plotter->fitInView(w - 80, h - 60);
    plotwin->plotter->setBounds(0, 0, w, h);
    //    plotwin->tabview->setBounds(0,h-tabviewheight,w,tabviewheight);
  }
};

/*=======================================================================*
  PlotWindow: top level window for plotting
  *=======================================================================*/

PointClipboard pointClipboard;

PlotWindow::PlotWindow(juce::XmlElement* plot)
  : juce::DocumentWindow ("", juce::Colours::white, juce::DocumentWindow::allButtons)
{
  listener.window = this;
  juce::String title ("");
  if (plot != 0)
    title = plot->getStringAttribute("title");
  setWindowTitle(title);
  plotter = new Plotter(plot) ;
  init();
  setVisible(true);
}

PlotWindow::PlotWindow(juce::String title, juce::MidiFile& midifile)
  : juce::DocumentWindow (title, juce::Colours::white, juce::DocumentWindow::allButtons, true)
{
  listener.window = this;
  setWindowTitle(title);
  plotter = new Plotter(midifile);
  init();
  setVisible(true);
}

PlotWindow::~PlotWindow ()
{
}

juce::String PlotWindow::getWindowTitle()
{
  juce::String title = getName();
  if (title.endsWith("(Plot)"))
    title = title.upToFirstOccurrenceOf("(Plot)", false, false).trim();
  return title;
}

void PlotWindow::setWindowTitle(juce::String title)
{
  if (title.isEmpty())
  {
    int num = WindowTypes::getHighestUntitledWindowOfType(WindowTypes::PlotWindow);
    if (num == 0)
      title << "Untitled" ;
    else
      title << "Untitled " << juce::String(num + 1);
  }
  if (title.startsWith("Untitled"))
    title << " (Plot)";
  setName(title);
}


void PlotWindow::init()
{
  //  menubar = new MenuBarComponent(this);
  //  setMenuBar(this);

  setUsingNativeTitleBar(true);    

  //  std::cout << "PlotWindow::init (creating PlotWindowComponent)\n";
  PlotWindowComponent* content=new PlotWindowComponent(this);
  //  std::cout << "PlotWindow::init (set plotter visible)\n";
  plotter->setVisible(true);
  //  std::cout << "PlotWindow::init (set content visible)\n";
  content->setVisible(true);
  //  std::cout << "PlotWindow::init (add child plotter)\n";
  content->addChildComponent(plotter);
  //  std::cout << "PlotWindow::init (add child tabview)\n";
  setResizable(true, true); 
  //  std::cout << "PlotWindow::init (setContentComponent)\n";  
  //  setContentComponent(content, false, false);
  setContentOwned(content, false);
  //  std::cout << "PlotWindow::init (center with size)\n";
  //  std::cout << "PlotWindow::init (set resizable)\n";
  WindowTypes::setWindowType(this, WindowTypes::PlotWindow);
  //  std::cout << "PlotWindow::init (insure points visible)\n";
  plotter->insurePointsVisible();
  //  std::cout << "PlotWindow::init (resizeForSpread)\n";
  plotter->resizeForSpread();
  //  std::cout << "PlotWindow::init (checkFitInView)\n";
  plotter->checkFitInView();
  //  std::cout << "PlotWindow::init (fitInView)\n";
  plotter->fitInView();
  // + PlotWindowComponent::tabviewheight
  centreWithSize(PlotWindowComponent::plotviewwidth, PlotWindowComponent::plotviewwidth);
  //  std::cout << "PlotWindow::init (DONE)\n";
  addKeyListener(Grace::getApp().commandManager.getKeyMappings());
}

PlotWindow* PlotWindow::getPlotWindow(juce::String title)
{
  for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    if (PlotWindow* w = dynamic_cast<PlotWindow*>(juce::TopLevelWindow::getTopLevelWindow(i)))
      if (w->getName() == title)
        return w;
  }
  return nullptr;
}

bool PlotWindow::hasUnsavedChanges()
{
  return plotter->hasUnsavedChanges();
}

// This is called from Scheme to open a new plot window.  in this case
// we want to set it to changed if there are any points at all

void PlotWindow::openWindowFromXml(void* ptr) //(juce::String str)
{
  //std::cout << str.toUTF8() << "\n";
  juce::XmlDocument doc (juce::String((char *)ptr));
  // HKT FIXME (ok but cleanup)
  std::unique_ptr<juce::XmlElement> xml = doc.getDocumentElement();
  if (xml && xml->getChildByName("fields") &&
      xml->getChildByName("layers"))
  {
    PlotWindow* w = new PlotWindow(xml.get());
    w->plotter->setUnsavedChanges(true);
//    delete xml;
    openPlottingControlsWindow();
  }
  else
  {
    juce::String err=">>> Error ";
    if (!xml)
      err << doc.getLastParseError() << "\n";
    else
    {
      err << "invalid xml plot data\n";
//      delete xml;
    }
    Console::getInstance()->printError(err);
  }
}

void PlotWindow::browseForFileToOpen()
{
  juce::File d = juce::File::getCurrentWorkingDirectory();
  juce::FileChooser ch ("Choose xml or midi file to plot", d, "*.xml;*.mid;*.midi;");
  if (ch.browseForFileToOpen())
  {
    if (ch.getResult().hasFileExtension("xml"))
      openXmlFile(ch.getResult());
    else
      openMidiFile(ch.getResult());
  }
}

void PlotWindow::openXmlFile(juce::File file)
{
  juce::XmlDocument doc (file);
  std::unique_ptr<juce::XmlElement> xml = doc.getDocumentElement();
  if (xml && xml->getChildByName("fields") &&
      xml->getChildByName("layers"))
  {
    PlotWindow* w=new PlotWindow(xml.get());
    w->setPlotFile(file);
//    delete xml;
    openPlottingControlsWindow();
  }
  else
  {
    juce::String err=">>> Error: ";
    if (!xml)
      err << doc.getLastParseError() << "\n";
    else
    {
      err << file.getFullPathName() 
          << " is not a valid plot (xml) file.\n";
//      delete xml;
    }
    Console::getInstance()->printError(err);
  }
}

void PlotWindow::openMidiFile(juce::File file)
{
  juce::String title=file.getFileNameWithoutExtension();
  juce::FileInputStream input (file);
  juce::MidiFile midifile;
  if (midifile.readFrom(input))
  {
    new PlotWindow(title, midifile);
    openPlottingControlsWindow();
  }
  else
  {
    juce::String err=">>> Error: ";
    err << file.getFullPathName() 
        << " is not a valid midi file.";
    Console::getInstance()->printError(err);
  }
}

bool PlotWindow::save(bool saveas)
{
  juce::File f = getPlotFile();
  if (saveas || (f == juce::File()))
  {
    juce::String t = ((saveas) ? "Save Plot As" : "Save Plot");
    if (f == juce::File())
      f = juce::File::getCurrentWorkingDirectory().getChildFile(getName() + ".xml");
    juce::FileChooser ch (t, f, "*.xml");
    if (ch.browseForFileToSave(true))
      setPlotFile(ch.getResult());
    else
      return false;
  }
  juce::String text = toXmlString();
  if (!getPlotFile().replaceWithText(text))
  {
    text = ">> Error: file "+getPlotFile().getFullPathName() + " not writable.\n";
    Console::getInstance()->printError(text);
    return false;
  }
  plotter->setUnsavedChanges(false);
  return true;
}

void PlotWindow::closeButtonPressed ()
{
  int x = 2;
  if (hasUnsavedChanges())
  {
    x = juce::AlertWindow::showYesNoCancelBox(juce::AlertWindow::QuestionIcon,
                                              "Close",
                                              "Save changes before closing?",
                                              "Save",
                                              "Don't Save",
                                              "Cancel" );
  }
  if (x == 0)
    return;
  if (x == 2 || save())
  {
    // FIXME ATTEMPT TO STOP CRASH ON CLOSING PLOT WINDOWS
    if (Grace::getApp().activeWindow == this)
    {
      std::cout << "PlotWindow: clearing activeWindow before deleteing\n";
      Grace::getApp().setActiveWindow(0);
    }
    delete this;
  }
}

/*=======================================================================*
  Edit Actions
  *=======================================================================*/

class DeleteLayerAction : public juce::UndoableAction 
{
  // this should probably only be applicable on the focus layer
  Layer* layer;
  Plotter* plotter;
public:
  DeleteLayerAction(Plotter* p, Layer* l) 
  {
    plotter=p;
    layer=l;
  }
  ~DeleteLayerAction() {delete layer;}
  bool perform() 
  {
    // plotter must hold (at least) 1 layer.
    if (plotter->numLayers() == 1) return false;
    plotter->removeLayer(layer);
    plotter->redrawBackView();
    plotter->redrawPlotView();
    return true;
  }
  bool undo () 
  {
    plotter->addLayer(layer);
    plotter->redrawBackView();
    plotter->redrawPlotView();
    return true;
  }
  int getSizeInUnits(){return 1;}
};

class RenameLayerAction : public juce::UndoableAction 
{
  juce::String name;
  Layer* layer;
  void rename() {
    juce::String s=layer->getLayerName();
    layer->setLayerName(name);
    name=s;   // cache old name
  }
public:
  RenameLayerAction(Plotter* p, Layer* l, juce::String n) 
  {
    layer=l;
    name=n;
  }
  ~RenameLayerAction() {}
  bool perform() {rename(); return true; }
  bool undo () {rename(); return true; }
  int getSizeInUnits(){return 1;}
};





void PlotWindow::PlotWindowListener::handleMessage(const juce::Message &m)
{
  if (const PlotMessage* pm = dynamic_cast<const PlotMessage*>(&m))
  {
    switch (pm->type)
    {
    case CommandIDs::PlotterAddXmlPoints:
      {
        juce::XmlDocument doc (pm->text);
        std::unique_ptr<juce::XmlElement> xml = doc.getDocumentElement();
        if (xml)
        {
          if (xml->hasTagName("points"))
          {
            window->plotter->getFocusLayer()->addXmlPoints(xml.get());
            window->plotter->repaint();
          }
//          delete xml;
        }
        else
        {
          juce::String err=">>> Error ";
          if (!xml)
            err << doc.getLastParseError() << "\n";
          else
          {
            err << "invalid xml plot data\n";
//            delete xml;
          }
          Console::getInstance()->printError(err);
        }
      }
      break;
    default:
      break;
    } 
  }
}

juce::String PlotWindow::toXmlString()
{
  juce::String text=juce::String();
  text << "<plot title=" << getName().quoted() << ">\n" ;
  // output axis definitions
  text << "  <fields>\n";
  for (int i=0; i<plotter->numFields(); i++)
  {
    Axis* a=plotter->getFieldAxis(i);
    text << "    "
         << "<field"
         << " name=" << plotter->getFieldName(i).quoted()
         << " axis=" ;
    if (plotter->isSharedField(i))
      text << plotter->getSharedFieldName(i).quoted();
    else
      text << a->toString().quoted();
    text << "/>\n";
  }
  text << "  </fields>\n";
  text << "  <layers>\n";
  for (int i=0; i<plotter->numLayers(); i++)
    text << "    "
	 << plotter->getLayer(i)->toString(TextIDs::Xml, 2, false, 0xFF);
  text << "\n";
  text << "  </layers>\n";
  text<<"</plot>\n";
  return text;
}

void PlotWindow::activeWindowStatusChanged()
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

































