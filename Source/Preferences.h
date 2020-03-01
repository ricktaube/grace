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

#ifndef CM_PREFERENCES_H
#define CM_PREFERENCES_H

#include "Libraries.h"

class Preferences
{
private:
  juce::PropertiesFile* props;
public:
  Preferences();
  ~Preferences();
  juce::PropertiesFile& getProps();

  bool getBoolProp(juce::String name, bool def=false);
  void setBoolProp(juce::String name, bool val);

  int getIntProp(juce::String name, int def=0);
  void setIntProp(juce::String name, int val);

  double getDoubleProp(juce::String name, double def=0.0);
  void setDoubleProp(juce::String name, double val);

  juce::String getStringProp(juce::String name, juce::String def="");
  void setStringProp(juce::String name, juce::String val);

  void removeProp(juce::String name);

  std::unique_ptr<juce::XmlElement> getXmlProp(juce::String name);
  void setXmlProp(juce::String name, juce::XmlElement* val);

  void flush();

  juce::RecentlyOpenedFilesList recentlyOpened;
  juce::RecentlyOpenedFilesList recentlyLoaded;
  juce::RecentlyOpenedFilesList recentlyLoadedGraphs;
  std::unique_ptr<juce::XmlElement> colorThemes;
  int numColorThemes();
  juce::XmlElement* getColorTheme(int index);
  juce::XmlElement* getColorTheme(const juce::String name);
  juce_DeclareSingleton (Preferences, false);
};

#endif
