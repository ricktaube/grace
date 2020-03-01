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

class SndLib
{
public:
  SndLib();
  ~SndLib();

  bool getAutoPlay();
  void setAutoPlay(bool ap);
  juce::String getAudioFormat();
  void setAudioFormat(juce::String name);
  int getSrate();
  void setSrate(int sr);
  int getChannels();
  void setChannels(int ch);

  std::unique_ptr<juce::XmlElement> instable;
  int performCommand(int type, int data=0, juce::String text=juce::String());
  juce::String getClmDefault(int id);
  juce::String getInstrumentCode(juce::String file, bool forEval);
  void loadInstrumentCode(juce::XmlElement* ins);  
  void getInstrumentFilesToLoad(juce::XmlElement* ins, juce::StringArray& ary);
  juce::XmlElement* getInstrumentTable();
  juce::XmlElement* getInstrumentTableInstruments();
  juce::XmlElement* getInstrumentElement(juce::String filename);
  bool isInstrumentLoaded(juce::String filename);
  void updateAutoLoaded();
  void autoLoadInstruments();
  void openInstrumentBrowser();
  void restoreInstruments();
  void restoreInstrument(juce::XmlElement* ins);
  juce_DeclareSingleton (SndLib, true)
};
