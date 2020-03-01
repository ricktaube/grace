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

class CsoundScoreEv
{
public:
  int type;
  int name;
  double time;
  juce::String pars;
  CsoundScoreEv(int type, int name, double time, juce::String pars);
  ~CsoundScoreEv() ;
  juce::String pfieldsToString(juce::String delim);
  juce::String toString(int fmat) ;
};

class Csound
{
private:

  class CsoundScoreEvComparator
  {
  public:
    static int compareElements(CsoundScoreEv* e1, CsoundScoreEv* e2) {
      if ( (e1->type==Csound::IStatement) &&
	   (e2->type==Csound::FStatement) )
	return (e1->time < e2->time) ? -1 : 1;
      else
	return (e1->time <= e2->time) ? -1 : 1;   
    }
  };
  juce::File scorefile;
  juce::File application;
  std::unique_ptr<juce::XmlElement> commandargs;
  juce::String scoreheader;

  bool playafter;
  bool writeafter;
  juce::CriticalSection lock;
  CsoundScoreEvComparator comparator;
  juce::OwnedArray<CsoundScoreEv,juce::CriticalSection> score;
public:

  enum {IStatement=1, FStatement};

  Csound();
  ~Csound();

  void initPrefs();
  void savePrefs();
  void openSettings();

  juce::File getScoreFile();
  void setScoreFile(juce::File file);

  juce::File getApplication();
  void setApplication(juce::File app);
  bool canRunApplication();
  void runApplication();
  juce::String getApplicationArgs();
  int findApplicationArgs(juce::String args);
  void setApplicationArgs(juce::String args);
  juce::XmlElement* getCommandArgs();

  juce::RecentlyOpenedFilesList orchestras;

  juce::File getOrcFile();
  void setOrcFile(juce::File file);

  juce::String getScoreHeader();
  void setScoreHeader(juce::String str);

  bool getPlayAfter();
  void setPlayAfter(bool flag);

  bool getWriteAfter();  
  void setWriteAfter(bool flag);  

  void initScore(juce::String inits);
  void saveScore();
  void sortScore();
  bool isScoreEmpty();
  void clearScore();
  void addToScore(int typ, int id, double time, juce::String vals);
  void writeScore(int dest, int fmat, double from=0.0, double to=-1.0,
		  bool istatements=true, bool fstatements=true);
  void exportScore();

  juce_DeclareSingleton (Csound, true)

};

class CsoundSettingsDialog : public juce::Component,
                             //public juce::LabelListener,
                             public juce::FilenameComponentListener,
public juce::Button::Listener,
public juce::ComboBox::Listener,
public juce::TextEditor::Listener
{
public:
  int mode;
  juce::Label* csolab;
  juce::FilenameComponent* csound;
  juce::Label* optlab;
  juce::ComboBox* options;
  juce::Label* orclab;
  juce::FilenameComponent* orcfile;
  juce::Label* hdrlab;
  juce::TextEditor* header;
  CsoundSettingsDialog();
  ~CsoundSettingsDialog();
  //  void paint (juce::Graphics& g);
  void resized();
  void filenameComponentChanged (juce::FilenameComponent* changed);
  //void labelTextChanged (juce::Label *changed);
  void comboBoxChanged (juce::ComboBox *comboBoxThatHasChanged);
  void buttonClicked (juce::Button *clicked);
  void textEditorTextChanged(juce::TextEditor& ed);
  void textEditorReturnKeyPressed(juce::TextEditor& ed);
  void textEditorEscapeKeyPressed(juce::TextEditor& ed);
  void textEditorFocusLost(juce::TextEditor& ed);
  void updateOptions();
};

class ExportCsoundDialog : public juce::Component,
public juce::Label::Listener,
public juce::ComboBox::Listener,
public juce::FilenameComponentListener,
public juce::Button::Listener
{
public:
  //    void paint (juce::Graphics& g);
  juce::GroupComponent* scoregroup;
  juce::Label* fromlabel;
  juce::Label* tolabel;
  juce::Label* frombuffer;
  juce::Label* tobuffer;
  juce::Label* formatlabel;
  juce::ComboBox* formatmenu;
  juce::GroupComponent* exportgroup;
  juce::ToggleButton* consoletoggle;
  juce::ToggleButton* clipboardtoggle;
  juce::ToggleButton* filetoggle;
  juce::ToggleButton* itoggle;
  juce::ToggleButton* ftoggle;
  juce::TextButton* exportbutton;
  juce::FilenameComponent* filechooser;
  ExportCsoundDialog () ;
  ~ExportCsoundDialog () ;
  void resized() ;
  void filenameComponentChanged (juce::FilenameComponent* changed);
  void labelTextChanged (juce::Label* labelThatHasChanged) ;
  void comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged) ;
  void buttonClicked (juce::Button* buttonThatWasClicked) ;
};
