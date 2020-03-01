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
#include "Cells.h"
#include "Console.h"

CellView::CellView(juce::String statesandcolors, int rows, int cols, int cellsize, int bordersize, juce::Colour bgcolor, int gens)
  : cellRows (rows),
    cellColumns (cols),
    cellSize (cellsize),
    cellBorderSize (bordersize),
    backgroundColor (bgcolor),
    maxGenerations (gens),
    generation (0),
    noState (-99)
{ 
  setStatesAndColors(statesandcolors);
  setVisible(true);
  // determine a "noState" value by finding an integer that is not in
  // the state array
  while (states.contains(noState)) noState--;
  // fill the data array with noState
  for (int i=0; i<(rows*cols); i++)
    data.add(noState);
}
void CellView::resized()
{
}

CellView::~CellView()
{
  colors.clear();
  states.clear();
  data.clear();
}

int CellView::XY(int x, int y)
{
  if (x < 0) x += 0x10000;
  if (y < 0) y += 0x10000;
  return ((y & 0xffff) << 16) | (x & 0xffff);
}

int CellView::XY_X(int xy)
{
  int x = (xy & 0xffff);
  return (x >= 0x8000) ? (x - 0x10000) : x;
}

int CellView::XY_Y(int xy)
{
  int y = ((xy & 0xffff0000) >> 16);
  return (y >= 0x8000) ? (y - 0x10000) : y;
}

int CellView::XY_ADD(int xy1, int xy2)
{
  return XY(XY_X(xy1) + XY_X(xy2), XY_Y(xy1) + XY_Y(xy2));
}

void CellView::paint (juce::Graphics& g)
{
  g.setColour(backgroundColor);
  g.fillAll();

  //data.lockArray();
  const juce::ScopedLock lock (data.getLock());

  int  x, y, i=0, s=data.size();
  y=cellBorderSize;
  for (int r=0; r<cellRows; r++)
  {
    x=cellBorderSize;
    for (int c=0; c<cellColumns; c++)      
    {
      if (i<s)
      {
        int q=data.getUnchecked(i);
        if (q==noState)
          break;
        else
        {
          int j=states.indexOf(q);
          if (j<0)
            g.setColour(juce::Colours::black);
          else
            g.setColour(colors.getUnchecked(j));
        }
      }
      else
        break;
      g.fillRect(x, y, cellSize, cellSize);
      x += (cellSize+cellBorderSize);
      i++;
    }
    y += (cellSize+cellBorderSize);
  }
  //data.unlockArray();
}

void CellView::setStatesAndColors(juce::String statesandcolors)
{
  // parse state and color pairs and fill arrays
  states.clear();
  colors.clear();
  juce::StringArray pairs;
  pairs.addTokens(statesandcolors, false);
  for (int i=0; i<pairs.size()-1; i+=2)
  {
    int state=pairs[i].getIntValue();
    juce::Colour color=juce::Colours::findColourForName( pairs[i+1], juce::Colours::black);
    states.add(state);
    colors.add(color);
  }
  //std::cout << "CellView colormap: " << states.size() << "\n";
  //for (int i=0; i<states.size(); i++)
  //  std::cout <<  "  [" << i << "] " << states[i] << " -> " << colors[i].toString().toUTF8() << "\n";
}

StateWindow::StateWindow (juce::String title, juce::String statesandcolors, int rows, int cols, int cellsize, int bordersize, juce::Colour bgcolor)
  : juce::DocumentWindow (title, juce::Colours::white, juce::DocumentWindow::allButtons, true)

{
  listener.window=this;
  CellView* cv=new CellView(statesandcolors, rows, cols, cellsize, bordersize, bgcolor, 1);
  setResizable(true, true); 
  //  setContentComponent(cv);
  setContentOwned(cv,true);
  int width=(cellsize*cols) + (bordersize*(cols+1)) ;
  int height=(cellsize*rows) + (bordersize*(rows+1)) ;
  //std::cout << "width=" << width << " height=" << height << "\n";
  setContentComponentSize(width,height);
  setUsingNativeTitleBar(true);
  setDropShadowEnabled(true);
  centreWithSize(getWidth(), getHeight());
  //  setComponentProperty("WindowType", WindowTypes::StateWindow);
  WindowTypes::setWindowType(this, WindowTypes::StateWindow);
  setVisible(true);
}

StateWindow::~StateWindow ()
{
}

void StateWindow::closeButtonPressed()
{
  delete this;
}

CellView* StateWindow::getCellView() 
{
  return (CellView*)getContentComponent();
}

StateWindow* StateWindow::findStateWindow(juce::String title)
{
  for (int i=0; i<juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    juce::TopLevelWindow* w=juce::TopLevelWindow::getTopLevelWindow(i);
    if (WindowTypes::isWindowType(w, WindowTypes::StateWindow) &&
        w->getName()==title)
      return (StateWindow*)w;
  }
  return (StateWindow*)NULL;
}

StateWindow::StateWindowListener::StateWindowListener() 
  : window (0)
{
}

StateWindow::StateWindowListener::~StateWindowListener()
{
}

void StateWindow::openWindowFromXml(void* ptr)
{
  juce::XmlElement* xml = (juce::XmlElement *)ptr;  
  if (!xml) return;
  juce::String title = xml->getStringAttribute("title");
  juce::String colormap = xml->getStringAttribute("colormap");
  int rows = xml->getIntAttribute("rows", 1);
  int columns = xml->getIntAttribute("columns", 1);
  int cellsize = xml->getIntAttribute("cellsize", 50);
  int cellbordersize = xml->getIntAttribute("cellbordersize", 1);
  juce::String bgcolor = xml->getStringAttribute("backgroundcolor", "white");
  new StateWindow(title, colormap, rows, columns, cellsize, cellbordersize,
                  juce::Colours::findColourForName(bgcolor, juce::Colours::white));
}

void StateWindow::StateWindowListener::handleMessage(const juce::Message &m)
{
  CellView* cv = window->getCellView();
  if (!cv) return;
  if (const StateMessage* sm = dynamic_cast<const StateMessage*>(&m))
  {
    switch (sm->type)
    {

    case CommandIDs::StateWindowSetCell:
      {
	int val = sm->data1;
	int row = sm->data2;
        int col = sm->data3;
        row = row % cv->cellRows;
        col = col % cv->cellColumns;
        cv->data.setUnchecked((row * cv->cellColumns) + col, val);
	cv->repaint();
	break;
      }

    case CommandIDs::StateWindowSetCells:
      {
        int row = sm->data1 % cv->cellRows;
        int ind = row * cv->cellColumns;
	int len = cv->data.size();
	for (int i = 0; i < sm->array.size(); i++)
          cv->data.setUnchecked((ind + i) % len, sm->array[i]);
	cv->repaint();
	break;
      }
    default:
      break;
    }
  }
}

