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

#ifndef CM_CELLS_H
#define CM_CELLS_H

#include "Libraries.h"

class CellView : public juce::Component 
{
public:
  int cellRows;
  int cellColumns;
  int cellSize;
  int cellBorderSize;
  juce::Colour backgroundColor;
  int maxGenerations;
  int generation;
  int noState;
  juce::Array<int, juce::CriticalSection> data;
  juce::Array<int> states;
  juce::Array<juce::Colour> colors;
  CellView(juce::String statesandcolors, int rows, int cols, int cellsize, int bordersize, juce::Colour bgcolor, int gens);
  ~CellView();

  void resized();
  void paint(juce::Graphics& g);

  void drawStates(int numstates, int* states);
  void setStatesAndColors(juce::String statesandcolors);
  void setRowsAndColumns(int rows, int cols);
  void setCellSize(int size, int bordersize=1);

  static int XY(int x, int y=0);
  static int XY_X(int xy);
  static int XY_Y(int xy);
  static int XY_ADD(int xy1, int xy2);
};

class StateWindow : public juce::DocumentWindow 
{
public:

  struct StateMessage : public juce::Message
  {
    int type;
    int data1;
    int data2;
    int data3;
    juce::Array<int> array;
    StateMessage(int type, int data1, int data2, int data3)
      : type (type),
        data1 (data1),
        data2 (data2),
        data3 (data3)
    {
    }
    ~StateMessage(){array.clear();}
  };

  class StateWindowListener : public juce::MessageListener
  {
  public:
    StateWindow* window;
    StateWindowListener();
    ~StateWindowListener();
    void handleMessage (const juce::Message &message);  
  };
  StateWindowListener listener;
  StateWindow (juce::String title, juce::String statesandcolors, int rows, int cols, int cellsize=50, int bordersize=1, juce::Colour bgcolor=juce::Colours::white) ;
  ~StateWindow () ;
  void closeButtonPressed();
  static StateWindow* findStateWindow(juce::String title);
  //static void openWindowFromXml(char* xml);
  static void openWindowFromXml(void* xml);
  CellView* getCellView();
};

#endif

