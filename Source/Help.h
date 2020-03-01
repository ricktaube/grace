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

class Help
{
  juce::File documentationDirectory;
  std::unique_ptr<juce::XmlElement> documentationTable;
  juce::StringPairArray roots;
  juce::StringPairArray sal;
  juce::StringPairArray cm;
  juce::StringPairArray clm;
  juce::StringPairArray scheme;
  
  juce::String helpKey(juce::StringPairArray& a, int i) {return a.getAllKeys()[i];}
  juce::String helpValue(juce::StringPairArray& a, int i) {return a.getAllValues()[i];}
  void addSalSymbolHelp();
  void addCommonMusicSymbolHelp();
  void addSndLibSymbolHelp();
  void addSchemeSymbolHelp();
  void restoreHelpFiles();

public:
  Help ();
  ~Help();
  void openHelp(juce::CommandID id);
  void openHelpInBrowser(juce::String url);
  void openHelpInEditor(juce::String path, juce::String code); 
  void exportFilesToDirectory(juce::CommandID id);
  void symbolHelp(juce::String sym, juce::String helpPath) ;
  juce::String getHelpFileText(juce::String filename);
  juce::XmlElement* getXmlMenu(juce::String title);
  juce::XmlElement* getXmlMenuItem(juce::String title, int index);
  void addHelpMenuItems(juce::PopupMenu& menu, juce::String menuName, 
                        juce::CommandID cmdId, int maxItems, 
                        juce::ApplicationCommandManager* manager,
                        int exportId);
  juce_DeclareSingleton (Help, true)
};
