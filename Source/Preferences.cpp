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
#include "Preferences.h"

juce_ImplementSingleton (Preferences);

// comment= old: firebrick  new: #ce001a
// string=  old: rosybrown  new: #a11153

static char colorthemedata [] = "<colorthemes>\
<colortheme name=\"Clarity and Beauty\"\
            plaintext=\"#aac5e0\" comment=\"#ce001a\" string=\"rosybrown\"\
            keyword1=\"cadetblue\" keyword2=\"#8a2f8f\" keyword3=\"orchid\"\
            literal1=\"#00ffff\" literal2=\"forestgreen\" literal3=\"blue\"\
            values=\"#00cd00\" output=\"lightsalmon\" warning=\"darkorange\"\
            error=\"#cd0000\" cursor=\"yellow\" background=\"black\"/>\
<colortheme name=\"Deep Blue\"\
            plaintext=\"#eeeeee\" comment=\"#ce001a\" string=\"rosybrown\"\
            keyword1=\"cadetblue\" keyword2=\"#8a2f8f\" keyword3=\"orchid\"\
            literal1=\"#950083\" literal2=\"forestgreen\" literal3=\"blue\"\
            values=\"#00cd00\" output=\"#deb887\" warning=\"darkorange\"\
            error=\"#cd0000\" cursor=\"#00ff00\" background=\"#102e4e\"/>\
<colortheme name=\"Gnome\"\
            plaintext=\"#f5deb3\" comment=\"#ce001a\" string=\"rosybrown\"\
            keyword1=\"cadetblue\" keyword2=\"#8a2f8f\" keyword3=\"orchid\"\
            literal1=\"#950083\" literal2=\"forestgreen\" literal3=\"blue\"\
            values=\"#00cd00\" output=\"lightsalmon\" warning=\"darkorange\"\
            error=\"#cd0000\" cursor=\"#d3d3d3\" background=\"#2f4f4f\"/>\
<colortheme name=\"Snowish\"\
            plaintext=\"#2f4f4f\" comment=\"#ce001a\" string=\"rosybrown\"\
            keyword1=\"cadetblue\" keyword2=\"#8a2f8f\" keyword3=\"orchid\"\
            literal1=\"#950083\" literal2=\"forestgreen\" literal3=\"blue\"\
            values=\"#00cd00\" output=\"#9400d3\" warning=\"darkorange\"\
            error=\"#cd0000\" cursor=\"#cd0000\" background=\"#eee9e9\"/>\
<colortheme name=\"Emacs\"\
            plaintext=\"black\" comment=\"#ce001a\" string=\"rosybrown\"\
            keyword1=\"cadetblue\" keyword2=\"#8a2f8f\" keyword3=\"orchid\"\
            literal1=\"#950083\" literal2=\"forestgreen\" literal3=\"blue\"\
            values=\"#00cd00\" output=\"rosybrown\" warning=\"darkorange\"\
            error=\"#cd0000\" cursor=\"black\" background=\"white\"/>\
<colortheme name=\"XEmacs\"\
            plaintext=\"black\" comment=\"#ce001a\" string=\"rosybrown\"\
            keyword1=\"cadetblue\" keyword2=\"#8a2f8f\" keyword3=\"orchid\"\
            literal1=\"#950083\" literal2=\"forestgreen\" literal3=\"blue\"\
            values=\"#00cd00\" output=\"#008b00\" warning=\"darkorange\"\
            error=\"#cd0000\" cursor=\"#cd0000\" background=\"#cccccc\"/>\
</colorthemes>";

Preferences::Preferences()
  : props (0)
{
  juce::PropertiesFile::Options options;
  options.applicationName = juce::JUCEApplication::getInstance()->getApplicationName();
  options.filenameSuffix = ".xml";
  options.millisecondsBeforeSaving = 3000;
  if (SysInfo::isMac())
  {
    options.osxLibrarySubFolder="Application Support";
    options.folderName = "org.commonmusic.grace.mac"; //Must be bundle identifier.
  }
  else
    options.folderName = "Grace";

  props=new juce::PropertiesFile(options);
  recentlyOpened.setMaxNumberOfItems(16);
  recentlyOpened.restoreFromString(props->getValue("RecentlyOpenedFiles"));
  recentlyOpened.removeNonExistentFiles();
  recentlyLoaded.setMaxNumberOfItems(8);
  recentlyLoaded.restoreFromString(props->getValue("RecentlyLoadedFiles"));
  recentlyLoaded.removeNonExistentFiles();
  recentlyLoadedGraphs.setMaxNumberOfItems(8);
  recentlyLoadedGraphs.restoreFromString(props->getValue("RecentlyLoadedGraphs"));
  recentlyLoadedGraphs.removeNonExistentFiles();

  juce::String working=props->getValue("WorkingDirectory");
  if (working.isNotEmpty())
    juce::File(working).setAsCurrentWorkingDirectory();
  else
    if (SysInfo::isWindows())
      juce::File::getSpecialLocation(juce::File::userDesktopDirectory).setAsCurrentWorkingDirectory();
    else
      juce::File::getSpecialLocation(juce::File::userHomeDirectory).setAsCurrentWorkingDirectory();
  juce::String str=juce::String(colorthemedata);
  juce::XmlDocument xmldoc (str);
  colorThemes=xmldoc.getDocumentElement();
}

int Preferences::numColorThemes()
{
  if (colorThemes) return colorThemes->getNumChildElements();
  return 0;
}

juce::XmlElement* Preferences::getColorTheme(int i)
{
  if (colorThemes) return colorThemes->getChildElement(i);
  return 0;
}

juce::XmlElement* Preferences::getColorTheme(const juce::String name)
{
  for (int i=0; i<numColorThemes(); i++)
  {
    juce::XmlElement* e=getColorTheme(i);
    if (name.equalsIgnoreCase(ColorThemeIDs::getColorThemeName(e)))
      return e;
  }
  return 0;
}

Preferences::~Preferences()
{
  flush();
  //deleteAndZero (colorThemes);
  deleteAndZero (props);
  clearSingletonInstance();
}

juce::PropertiesFile& Preferences::getProps()
{
  return *props;
}

void Preferences::flush()
{
  // flush exiting
  if (props != 0) 
  {
    props->setValue("RecentlyOpenedFiles", recentlyOpened.toString());
    props->setValue("RecentlyLoadedFiles", recentlyLoaded.toString());
    props->setValue("RecentlyLoadedGraphs", recentlyLoadedGraphs.toString());
    props->setValue("WorkingDirectory", 
                    juce::File::getCurrentWorkingDirectory().getFullPathName());
  }
}

// Bool Props

bool Preferences::getBoolProp(juce::String name, bool def)
{
  return props->getBoolValue(name, def);
}

void Preferences::setBoolProp(juce::String name, bool val)
{
  props->setValue(name, val);
}

// Int Props

int Preferences::getIntProp(juce::String name, int def)
{
  return props->getIntValue(name, def);
}

void Preferences::setIntProp(juce::String name, int val)
{
  props->setValue(name, val);
}

// juce::String Props

double Preferences::getDoubleProp(juce::String name, double def)
{
  return props->getDoubleValue(name, def);
}

void Preferences::setDoubleProp(juce::String name, double val)
{
  props->setValue(name, val);
}

// juce::String Props

juce::String Preferences::getStringProp(juce::String name, juce::String def)
{
  return props->getValue(name, def);
}


void Preferences::setStringProp(juce::String name, juce::String val)
{
  props->setValue(name, val);
}

// XML Props

std::unique_ptr<juce::XmlElement> Preferences::getXmlProp(juce::String name) {
  return props->getXmlValue(name);
}

void Preferences::setXmlProp(juce::String name, juce::XmlElement* val)
{
  props->setValue(name, val);
}

void Preferences::removeProp(juce::String name)
{
  return props->removeValue(name);
}
