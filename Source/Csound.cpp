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
#include "Csound.h"
#include "Console.h"
#include "Preferences.h"
#include "CmSupport.h"
//#include "Alerts.h"

/*=======================================================================*
  Csound Instance
  *=======================================================================*/

juce_ImplementSingleton (Csound);

Csound::Csound () 
  : scorefile (juce::File()),
    application (juce::File()),
    scoreheader (juce::String()),
    playafter (false),
    writeafter (true)
{
  orchestras.setMaxNumberOfItems(8);
  initPrefs();
  //for (int i=0;i<20;i++)
  //    addToScore(IStatement, 1, i, "20 304 1");
  //  std::cout << "score size=" << score.size() << "\n";
}

Csound::~Csound ()
{
  //flushPrefs();
  orchestras.clear();
  score.clear();
}

void Csound::initPrefs()
{
  Preferences* pref=Preferences::getInstance();
  juce::String path=pref->getStringProp("CsoundApplication");
  if (!path.isEmpty())
  {
    juce::File app=completeFile(path);
    if (!app.existsAsFile())
    {
      juce::String warn="Warning: Csound application ";
      warn << path.quoted() << " does not exist.\n";
      Console::getInstance()->printWarning(warn);
    }
    else
      application=app;
  }
  //pref->setStringProp("CsoundCommandArgs", "-W -o test.wav -d");

  auto commandargs = pref->getXmlProp("CsoundCommandArgs");
  if (!commandargs)
  {
    juce::XmlElement* child=0;
    commandargs.reset(new juce::XmlElement("CsoundCommandArgs"));
    // index of currently selected args
    commandargs->setAttribute("index", 0);
    child=commandargs->createNewChildElement("CommandArgs");
    child->setAttribute("text","-W -odac -d");
    child=commandargs->createNewChildElement("CommandArgs");
    child->setAttribute("text","-W -o test.wav -d");
  }
  scoreheader=pref->getStringProp("CsoundScoreHeader");
  orchestras.restoreFromString(pref->getStringProp("CsoundOrchestras"));
  playafter=pref->getBoolProp("CsoundPlayAfter", false);
}

void Csound::savePrefs()
{
  Preferences* pref=Preferences::getInstance();
  pref->setStringProp("CsoundApplication",  application.getFullPathName());
  pref->setXmlProp("CsoundCommandArgs", commandargs.get());
  pref->setStringProp("CsoundOrchestras", orchestras.toString());
  pref->setStringProp("CsoundScoreHeader", scoreheader);
  pref->setBoolProp("CsoundPlayAfter", playafter);
  // WriteAfter is NOT a preference
}

juce::File Csound::getScoreFile()
{
  return scorefile;
}

void Csound::setScoreFile(juce::File file)
{
  // NOT a preference
  scorefile=file;
}

juce::File Csound::getApplication()
{
  return application;
}

void Csound::setApplication(juce::File file)
{
  application=file;
  savePrefs();
}

juce::XmlElement* Csound::getCommandArgs()
{
  return commandargs.get();
}

juce::String Csound::getApplicationArgs()
{
  // return current application args
  int index=commandargs->getIntAttribute("index"); // index of current
  juce::XmlElement* sub=commandargs->getChildElement(index);
  if (sub)
    return sub->getStringAttribute("text");
  return juce::String();
}

int Csound::findApplicationArgs(juce::String str)
{
  // return index of str or -1 if not there
  int index=0;
  forEachXmlChildElement (*commandargs, e)
  {
    if (e->getStringAttribute("text")==str)
      return index;
    index++;
  }
  return -1;
}

void Csound::setApplicationArgs(juce::String str)
{
  // add or select str as the application's command args
  int i=findApplicationArgs(str);
  if (i==-1) // add new command args
  {
    juce::XmlElement* e=commandargs->createNewChildElement("CommandArgs");
    e->setAttribute("text", str);
    i=commandargs->getNumChildElements()-1;
  }
  // select str as current args
  commandargs->setAttribute("index", i);
  savePrefs();
}

bool Csound::canRunApplication()
{
  return application.existsAsFile();
}

void Csound::runApplication()
{
  if (canRunApplication())
  {
    juce::File app=getApplication();
    juce::String orc=getOrcFile().getFullPathName().quoted();
    juce::String sco=getScoreFile().getFullPathName().quoted();
    juce::String args=getApplicationArgs();
    juce::String msg=juce::String();
    args << " " << orc
         << " " << sco;
    msg << "Starting " << app.getFullPathName()
        << " " << args << "\n";
    Console::getInstance()->printOutput(msg);
    app.startAsProcess(args);
  }
  else
    Console::getInstance()->printWarning("Warning: Can't play Csound score, see Csound Settings dialog to set Csound application path.");
}

juce::String Csound::getScoreHeader()
{
  return scoreheader;
}

void Csound::setScoreHeader(juce::String str)
{
  scoreheader=str;
  savePrefs();
}

juce::File Csound::getOrcFile()
{
  if (orchestras.getNumFiles() > 0)
    return orchestras.getFile(0);
  else
    return juce::File();
}

void Csound::setOrcFile(juce::File file) 
{
  orchestras.addFile(file);
  savePrefs();
}

bool Csound::getPlayAfter()
{
  return playafter;
}

void Csound::setPlayAfter(bool b)
{
  playafter=b;
  savePrefs();
}

bool Csound::getWriteAfter()
{
  return writeafter;
}

void Csound::setWriteAfter(bool b)
{
  // NOT a preference
  writeafter=b;
}

/*=======================================================================*
  Score Events
  *=======================================================================*/

CsoundScoreEv::CsoundScoreEv(int typ, int id, double tim, juce::String vals)
{
  type=typ;
  name=id;
  time=tim;
  pars=vals;
}

CsoundScoreEv::~CsoundScoreEv()
{
}

juce::String CsoundScoreEv::pfieldsToString(juce::String delim)
{
  juce::String text = juce::String();
  text << name
       << delim << juce::String(time, 3);
  if (delim==" ")
    text << delim << pars;
  else
    text << delim << pars.replace(" ", delim);
  return text;
}

juce::String CsoundScoreEv::toString(int fmat)
{
  juce::String text = juce::String();
  juce::String evid;
  if (type==Csound::IStatement)
    evid="i";
  else if (type==Csound::FStatement)
    evid="f";
  else
    return "<unknown csound statement>";
  switch (fmat)
  {
  case ExportIDs::CsoundScore :
    text << evid << " " << pfieldsToString( " " );
    break;
  case ExportIDs::SalData :
    text << "{" << pfieldsToString( " " ) << "}";
    break;
  case ExportIDs::LispData :
    text << "(" << pfieldsToString( " " ) << ")";
    break;
  case ExportIDs::SalSend :
    text << "send \"cs:" << evid << "\", " 
         << pfieldsToString( ", ");
    break;
  case ExportIDs::LispSend :
    text << "(send \"cs:" << evid << "\" " 
         << pfieldsToString( " " ) << ")";
    break;
  default:
    text="<unknown export format>";
    break;
  }
  return text;
}

/*=======================================================================*
  Score Interface
  *=======================================================================*/

void Csound::initScore(juce::String inits)
{
  juce::StringArray array;
  array.addTokens(inits, true);
  if (array.size()==0)
    return;
  setScoreFile(completeFile(array[0].unquoted()));
  for (int i=1; i<array.size(); i+=2)
  {
    if (array[i]==":play")
      setPlayAfter(array[i+1]=="#t");
    else if (array[i]==":write")
      setWriteAfter(array[i+1]=="#t");
    else if (array[i]==":options")
      setApplicationArgs(array[i+1].unquoted());
    else if (array[i]==":header")
      setScoreHeader(array[i+1].unquoted());
    else if (array[i]==":orchestra")
      setOrcFile(array[i+1].unquoted());
  }
}

void Csound::saveScore()
{
  if (getWriteAfter())
  {
    writeScore(ExportIDs::ToFile, ExportIDs::CsoundScore);
    clearScore();
    if (getPlayAfter())
    {
      runApplication();
    }
  }
}

bool Csound::isScoreEmpty()
{
  return score.size() == 0;
}

void Csound::addToScore(int type, int name, double time, juce::String pars)
{
  score.add(new CsoundScoreEv(type, name, time, pars));
}

void Csound::sortScore() {
  score.sort(comparator,true);
}

void Csound::clearScore() {
  score.clear();
}

void Csound::writeScore(int dest, int format, double start, double endtime,
			bool addi, bool addf)
{
  if (endtime<=0.0) endtime=3600.0*24.0;
  int i=0;
  juce::String text, indent, after;
  switch (format)
  {
  case ExportIDs::CsoundScore :
    text="s\n";
    indent=juce::String();
    after="e\n";
    break;
  case ExportIDs::LispData :
    text="(define csound-score\n  '(\n";
    indent="    ";
    after="  ))\n";
    break;
  case ExportIDs::SalData :
    text="define variable csound-score = \n  {\n"; 
    indent="   ";
    after="  }\n";
    break;
  case ExportIDs::LispSend :
    text="(begin\n"; 
    indent="  "; 
    after="  )\n";
    break;
  case ExportIDs::SalSend :
    text="begin\n";
    indent="  ";
    after="end\n";
    break;
  }
  sortScore();

  if (start > 0.0)
  { 
    // fast forward to user's start time
    for ( ; i<score.size(); i++)
      if ( start <= (double)(score[i]->time) )
        break;
  }
  for ( ; i<score.size(); i++)
  {
    if (score[i]->time > endtime)
      break;   // stop after endtime
    if ( (score[i]->type == Csound::IStatement && addi) || 
         (score[i]->type == Csound::FStatement && addf) )
      text << indent << score[i]->toString(format) << "\n";
  }
  text << after;
  // output to file, console or clipboard
  if (dest==ExportIDs::ToFile)
  {
    juce::String comm="; Common Music output ";
    comm << juce::Time::getCurrentTime().toString(true,true) 
         << "\n\n";
    if (!getScoreHeader().isEmpty())
      comm << getScoreHeader() << "\n\n";
    scorefile.replaceWithText(comm+text);
    juce::String msg="Scorefile: ";
    msg << scorefile.getFullPathName() << "\n";
    Console::getInstance()->printOutput(msg);
  }
  else if (dest==ExportIDs::ToConsole)
    Console::getInstance()->printOutput(text);
  else if ( dest==ExportIDs::ToClipboard)
    juce::SystemClipboard::copyTextToClipboard(text);
}

/*=======================================================================*
  Dialogs
  *=======================================================================*/

CsoundSettingsDialog::CsoundSettingsDialog ()
  : csolab (0),
    csound (0),
    optlab (0),
    options (0),
    orclab (0),
    orcfile (0),
    hdrlab (0),
    header (0)
{
  Csound* cs=Csound::getInstance();
  addAndMakeVisible(csolab = new juce::Label(juce::String(), "Csound:"));
  csolab->setFont(juce::Font(15.0000f, juce::Font::plain));
  csolab->setJustificationType(juce::Justification::centredRight);
  csolab->setEditable(false, false, false);
  csolab->setColour(juce::TextEditor::textColourId, juce::Colours::black);
  csolab->setColour(juce::TextEditor::backgroundColourId, juce::Colour (0x0));

  csound = new juce::FilenameComponent(juce::String(), 
                                       cs->getApplication(),
                                       true, 
                                       false, 
                                       false, 
                                       juce::String(),
                                       juce::String(),
                                       "(choose csound executable)");
  addAndMakeVisible(csound);
  csound->setBrowseButtonText("Browse...");
  csound->addListener(this);

  addAndMakeVisible(optlab = new juce::Label(juce::String(), "Options:"));
  optlab->setFont(juce::Font(15.0000f, juce::Font::plain));
  optlab->setJustificationType(juce::Justification::centredRight);
  optlab->setEditable(false, false, false);
  optlab->setColour(juce::TextEditor::textColourId, juce::Colours::black);
  optlab->setColour(juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  
  addAndMakeVisible(options = new juce::ComboBox("Options"));
  options->setEditableText(true);
  updateOptions();
  options->addListener(this);

  addAndMakeVisible(orclab = new juce::Label(juce::String(), "Orchestra:"));
  orclab->setFont(juce::Font (15.0000f, juce::Font::plain));
  orclab->setJustificationType (juce::Justification::centredRight);
  orclab->setEditable (false, false, false);

  orcfile = new juce::FilenameComponent(juce::String(), 
                                        juce::String(),
                                        true, 
                                        false, 
                                        false, 
                                        "*.orc;*.csd",
                                        juce::String(),
                                        "(choose .orc file)");
  orcfile->setBrowseButtonText("Browse...");
  orcfile->addListener(this);
  juce::StringArray files=cs->orchestras.getAllFilenames();
  orcfile->setRecentlyUsedFilenames(files);
  if (files.size() > 0)
    orcfile->setCurrentFile(cs->orchestras.getFile(0), false, juce::dontSendNotification);
  addAndMakeVisible(orcfile);

  addAndMakeVisible(hdrlab = new juce::Label(juce::String(), "Header:"));
  hdrlab->setFont(juce::Font(15.0000f, juce::Font::plain));
  hdrlab->setJustificationType(juce::Justification::centredRight);
  hdrlab->setEditable(false, false, false);
  hdrlab->setColour(juce::TextEditor::textColourId, juce::Colours::black);
  hdrlab->setColour(juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  
  addAndMakeVisible(header = new juce::TextEditor(juce::String()));
  header->setColour(juce::TextEditor::backgroundColourId, juce::Colours::white);
  header->setColour(juce::TextEditor::outlineColourId,  juce::Colour (0xb2808080));
  header->setMultiLine(true);
  header->setReturnKeyStartsNewLine(true);
  header->setText(cs->getScoreHeader(), juce::dontSendNotification );
  header->addListener(this);
  int m=8, l=24;
  //  setSize (600, 96 + 24 + 16);
  setSize (600,m+l+m+l+m+l+m+l+l+m);
}

CsoundSettingsDialog::~CsoundSettingsDialog()
{
  deleteAndZero(csolab);
  deleteAndZero(csound);
  deleteAndZero(optlab);
  deleteAndZero(options);
  deleteAndZero(orclab);
  deleteAndZero(orcfile);
  deleteAndZero(hdrlab);
  deleteAndZero(header);
}


void Csound::openSettings()
{
  CsoundSettingsDialog* dialog = new CsoundSettingsDialog();
  //  juce::DialogWindow::showModalDialog("Csound Settings",
  //                                      dialog,
  //                                      NULL,
  //                                      juce::Colour(0xffe5e5e5),
  //                                      true,
  //                                      true,
  //                                      true);
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "Audio Settings";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.setOwned(dialog);
  dw.runModal();
  savePrefs();
}

void CsoundSettingsDialog::resized()
{
  int m = 8;
  int x1 = m;
  int m1 = 80;
  int x2 = 100;
  int y = m;
  int w = getRight();
  int h = getHeight();

  csolab->setBounds(x1, y, m1, 24);
  csound->setBounds(x2, y, w - x2 - m, 24);

  y = y + 24 + m;
  optlab->setBounds (x1, y, m1, 24);
  options->setBounds(x2, y, w-x2-m, 24);

  y = y + 24 + m;
  orclab->setBounds(x1, y, m1, 24);
  orcfile->setBounds(x2, y, w - x2 - m, 24);

  y = y + 24 + m;
  hdrlab->setBounds(x1, y, m1, 24);
  header->setBounds(x2, y, w - x2 - m, h - y - m);
}

void CsoundSettingsDialog::textEditorTextChanged(juce::TextEditor& ed) {}
void CsoundSettingsDialog::textEditorReturnKeyPressed(juce::TextEditor& ed) {}
void CsoundSettingsDialog::textEditorEscapeKeyPressed(juce::TextEditor& ed) {}
void CsoundSettingsDialog::textEditorFocusLost(juce::TextEditor& ed)
{
  // if NOT modal
  Csound::getInstance()->setScoreHeader(ed.getText());
}

void CsoundSettingsDialog::updateOptions()
{
  options->clear();
  juce::XmlElement* args = Csound::getInstance()->getCommandArgs();
  int num = args->getNumChildElements();
  // create a combo item for each arg with id set to index+1;
  for (int index = 0; index < num; index++)
    options->addItem(args->getChildElement(index)->getStringAttribute("text"), index + 1);
  options->addSeparator();
  options->addItem("New", 1000);
  options->addItem("Delete", 1001);
  // disable Delete if only one item
  if (num < 2)
    options->setItemEnabled(1001, false);
  int sel = args->getIntAttribute("index"); // selected arg
  options->setSelectedId(sel + 1, juce::dontSendNotification);
  options->setText(Csound::getInstance()->getApplicationArgs(), juce::dontSendNotification);
  //  std::cout << "updateOptions: selected item is " << sel << " (id " << sel+1 << ")\n";
}

void CsoundSettingsDialog::comboBoxChanged(juce::ComboBox* combobox)
{
  juce::XmlElement* commandargs = Csound::getInstance()->getCommandArgs();
  int id = combobox->getSelectedId();
  if (id == 1000) // New item
  {
    Csound::getInstance()->setApplicationArgs("(add text)");
    updateOptions();
  }
  else if (id == 1001) // Delete item
  {
    int ind=commandargs->getIntAttribute("index");
    juce::XmlElement* sel=commandargs->getChildElement(ind);
    commandargs->removeChildElement(sel, true);
    commandargs->setAttribute("index", 0);
    updateOptions();
  }
  else if (id > 0) // selected an arg
  {
    int index = combobox->indexOfItemId(id);
    commandargs->setAttribute("index", index);
  }
  else // edited current arg
  {
    int index = commandargs->getIntAttribute("index");
    if (juce::XmlElement* xml = commandargs->getChildElement(index))
    {
      juce::String text = combobox->getText();
      xml->setAttribute("text", text);
      combobox->changeItemText(index + 1, text);
    }
    else std::cout << "no CommandArgs xml element for index=" << index << "\n";
  }
}

void CsoundSettingsDialog::filenameComponentChanged(juce::FilenameComponent* comp)
{
  if (comp == orcfile)
    Csound::getInstance()->setOrcFile(comp->getCurrentFile());
  else if (comp == csound)
    Csound::getInstance()->setApplication(comp->getCurrentFile());
}

void CsoundSettingsDialog::buttonClicked(juce::Button* clicked) 
{
}

/*=======================================================================*
  Export Score Dialog
  *=======================================================================*/

void Csound::exportScore()
{
  ExportCsoundDialog* dlg = new ExportCsoundDialog();
  //  int flag=juce::DialogWindow::showModalDialog("Export Csound", 
  //                                               dlg,
  //                                               NULL,
  //                                               juce::Colour(0xffe5e5e5),
  //                                               true,
  //                                               false,
  //                                               false);
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "Export Csound";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.set(dlg, false);
  int flag = dw.runModal();

  if (flag != 1)
  {
    delete dlg;
    return;
  }
  double start = dlg->frombuffer->getText().getDoubleValue();
  double endtime = dlg->tobuffer->getText().getDoubleValue();
  int format = dlg->formatmenu->getSelectedId();
  bool addi = dlg->itoggle->getToggleState();
  bool addf = dlg->ftoggle->getToggleState();
  int dest;
  if (dlg->consoletoggle->getToggleState())
    dest = ExportIDs::ToConsole;
  else if (dlg->clipboardtoggle->getToggleState())
    dest = ExportIDs::ToClipboard;
  else 
    dest = ExportIDs::ToFile;
  delete dlg;
  writeScore(dest, format, start, endtime, addi, addf);
}

ExportCsoundDialog::ExportCsoundDialog ()
  : scoregroup (0),
    fromlabel (0),
    tolabel (0),
    frombuffer (0),
    tobuffer (0),
    formatlabel (0),
    formatmenu (0),
    exportgroup (0),
    consoletoggle (0),
    clipboardtoggle (0),
    filetoggle (0),
    itoggle (0),
    ftoggle (0),
    exportbutton (0),
    filechooser (0) 
{
  scoregroup = new juce::GroupComponent (juce::String(), "Score");
  addAndMakeVisible (scoregroup);
  addAndMakeVisible (fromlabel = new juce::Label (juce::String(), "From:"));
  fromlabel->setFont (juce::Font (15.0000f, juce::Font::plain));
  fromlabel->setJustificationType (juce::Justification::centredRight);
  fromlabel->setEditable (false, false, false);
  fromlabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  fromlabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
 
  frombuffer = new juce::Label ("From", juce::String());
  addAndMakeVisible (frombuffer);
  frombuffer->setFont (juce::Font (15.0000f, juce::Font::plain));
  frombuffer->setEditable (true, true, false);
  frombuffer->setColour (juce::Label::outlineColourId,  juce::Colour (0xb2808080));
  frombuffer->setColour (juce::Label::backgroundColourId, juce::Colours::white);
  frombuffer->addListener (this);

  tolabel = new juce::Label (juce::String(), "To:");
  addAndMakeVisible (tolabel);
  tolabel->setFont (juce::Font (15.0000f, juce::Font::plain));
  tolabel->setJustificationType (juce::Justification::centredLeft);
  tolabel->setEditable (false, false, false);
  tolabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  tolabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  
  tobuffer = new juce::Label ("To", juce::String());
  addAndMakeVisible (tobuffer);
  tobuffer->setFont (juce::Font (15.0000f, juce::Font::plain));
  tobuffer->setJustificationType (juce::Justification::centredLeft);
  tobuffer->setEditable (true, true, false);
  tobuffer->setColour (juce::Label::outlineColourId, juce::Colour (0xb2808080));
  tobuffer->setColour (juce::Label::backgroundColourId, juce::Colours::white);
  tobuffer->addListener (this);
  
  formatlabel = new juce::Label (juce::String(), "Export Format:");
  addAndMakeVisible (formatlabel);
  formatlabel->setFont (juce::Font (15.0000f, juce::Font::plain));
  formatlabel->setJustificationType (juce::Justification::centredRight);
  formatlabel->setEditable (false, false, false);
  formatlabel->setColour (juce::Label::textColourId, juce::Colours::black);
  formatlabel->setColour (juce::Label::backgroundColourId, juce::Colour (0x0));
  
  formatmenu = new juce::ComboBox (juce::String());
  addAndMakeVisible (formatmenu);
  formatmenu->setEditableText (false);
  formatmenu->setJustificationType (juce::Justification::centredLeft);
  formatmenu->setTextWhenNothingSelected (juce::String());
  formatmenu->addItem ("Csound score", ExportIDs::CsoundScore);
  formatmenu->addItem ("SAL data", ExportIDs::SalData);
  formatmenu->addItem ("Lisp data", ExportIDs::LispData);
  formatmenu->addItem ("SAL sends", ExportIDs::SalSend);
  formatmenu->addItem ("Lisp sends", ExportIDs::LispSend);
  formatmenu->addListener (this);
  formatmenu->setSelectedId(ExportIDs::CsoundScore, juce::sendNotification);
 
  exportgroup = new juce::GroupComponent (juce::String(), "Export To");
  addAndMakeVisible (exportgroup);
  
  consoletoggle = new juce::ToggleButton (juce::String());
  addAndMakeVisible (consoletoggle);
  consoletoggle->setButtonText ("Console");
  consoletoggle->setRadioGroupId (1);
  consoletoggle->setToggleState (true, juce::dontSendNotification);
  consoletoggle->addListener (this);

  clipboardtoggle = new juce::ToggleButton (juce::String());
  addAndMakeVisible (clipboardtoggle);
  clipboardtoggle->setButtonText ("Clipboard");
  clipboardtoggle->setRadioGroupId (1);
  clipboardtoggle->addListener (this);
  
  filetoggle = new juce::ToggleButton (juce::String());
  addAndMakeVisible (filetoggle);
  filetoggle->setButtonText ("File:");
  filetoggle->setRadioGroupId (1);
  filetoggle->addListener (this);
  
  itoggle = new juce::ToggleButton (juce::String());
  addAndMakeVisible (itoggle);
  itoggle->setButtonText (" i statements");
  itoggle->setToggleState (true, juce::dontSendNotification);
  
  ftoggle = new juce::ToggleButton (juce::String());
  addAndMakeVisible (ftoggle);
  ftoggle->setButtonText ("f statements");
  ftoggle->setToggleState (true, juce::dontSendNotification);
  
  exportbutton = new juce::TextButton ("Export");
  addAndMakeVisible (exportbutton);
  exportbutton->addListener (this);
  
  filechooser = new juce::FilenameComponent("File", 
                                            juce::File::getSpecialLocation(juce::File::userHomeDirectory).getChildFile("test.sco"),
                                            true, 
                                            false, 
                                            true, 
                                            juce::String(),
                                            juce::String(),
                                            "(choose file)");
  filechooser->setEnabled(false);
  filechooser->addListener(this);
  addAndMakeVisible(filechooser);

  setSize (568, 248);
}

ExportCsoundDialog::~ExportCsoundDialog()
{
  deleteAndZero (scoregroup);
  deleteAndZero (fromlabel);
  deleteAndZero (tolabel);
  deleteAndZero (frombuffer);
  deleteAndZero (tobuffer);
  deleteAndZero (formatlabel);
  deleteAndZero (formatmenu);
  deleteAndZero (exportgroup);
  deleteAndZero (consoletoggle);
  deleteAndZero (clipboardtoggle);
  deleteAndZero (filetoggle);
  deleteAndZero (itoggle);
  deleteAndZero (ftoggle);
  deleteAndZero (exportbutton);
  deleteAndZero (filechooser);
}

void ExportCsoundDialog::resized()
{
  scoregroup->setBounds (16, 8, 536, 80);
  fromlabel->setBounds (25, 28, 48, 24);
  tolabel->setBounds (161, 28, 32, 24);
  frombuffer->setBounds (81, 28, 64, 24);
  tobuffer->setBounds (201, 28, 64, 24);
  formatlabel->setBounds (281, 28, 95, 24);
  formatmenu->setBounds (385, 28, 150, 24);
  exportgroup->setBounds (16, 101, 536, 80);
  consoletoggle->setBounds (32, 128, 80, 24);
  clipboardtoggle->setBounds (118, 128, 88, 24);
  filetoggle->setBounds (216, 128, 56, 24);
  itoggle->setBounds (32, 56, 104, 24);
  ftoggle->setBounds (144, 56, 104, 24);
  exportbutton->setBounds (216, 200, 150, 24);
  filechooser->setBounds (280, 128, 256, 24);
}

void ExportCsoundDialog::labelTextChanged (juce::Label* labelThatHasChanged)
{
}

void ExportCsoundDialog::comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged)
{
}

void ExportCsoundDialog::buttonClicked(juce::Button* buttonThatWasClicked)
{
  if (buttonThatWasClicked == filetoggle)
    if ( filetoggle->getToggleState() )
      filechooser->setEnabled(true);
    else
      filechooser->setEnabled(false);
  else if (buttonThatWasClicked == exportbutton)
    ((juce::DialogWindow *)getTopLevelComponent())->exitModalState(1);
  else
    filechooser->setEnabled(false);
}

void ExportCsoundDialog::filenameComponentChanged(juce::FilenameComponent* comp)
{
}

