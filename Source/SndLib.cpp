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

#include "Libraries.h"
// //FIXME!!!
// #include "../sndlib/sndlib.h"
// #include "s7.h"
// #include "clm.h"
// #include "xen.h"
// #include "clm2xen.h"

#include "../sndlib/sndlib.h"
#include "s7.h"
#include "clm.h"
#include "xen.h"
#include "clm2xen.h"


#include "Enumerations.h"
#include "SndLibBridge.h"
#include "CmSupport.h"
#include "SchemeSources.h"
#include "Scheme.h"
#include "Console.h"
#include "SndLib.h"
#include "Preferences.h"
#include "CodeEditor.h"
#include "Instruments.h"

// Singleton instance for SndLib...

juce_ImplementSingleton(SndLib)
// HKT FIXME
//s7_scheme *s7;
//#ifdef _MSC_VER
//#define unlink _unlink
//s7_scheme *s7;
//XEN xen_false, xen_true, xen_nil, xen_undefined;
//#endif

SndLib::SndLib()
{
  juce::XmlDocument dataDoc (juce::String((const char*)Instruments::ins_xml)); 
  instable=dataDoc.getDocumentElement();
  juce::String autos=Preferences::getInstance()->getStringProp("SndLibAutoLoad");  
  if (autos.isNotEmpty())
    forEachXmlChildElement (*(instable->getChildByName("instruments")),
			    e)
    {
      if (autos.containsWholeWord(e->getStringAttribute("File")))
      {
        //std::cout << "initing as autoloaded: " 
        //<< e->getStringAttribute("File").toUTF8() << "\n";
        e->setAttribute("AutoLoad", "yes");
      }
    }
}

SndLib::~SndLib()
{
  //deleteAndZero(instable);
}

/*=======================================================================*
  Scheduler Support
  *=======================================================================*/

int SndLib::performCommand(int id, int data, juce::String text)
{
  // let other components send commands to sndlib. right now its
  // running synchronously, not sure if this will work in grace...
  juce::CommandID cmd = CommandIDs::getCommand(id);
  //  int arg = CommandIDs::getCommandData(id);  
  int res=0;

  switch (cmd)
  {
  case CommandIDs::SchedulerScoreComplete :
    // this is really gross, i need to figure out how to do the
    // with-sound file opening closing in c so i dont have to call
    // back into lisp to do (finalize-with-sound)
    SchemeThread::getInstance()->eval("(snd:close-output-file)", false);
    break;
  default:
    break;
  }
  return res;
}

/*=======================================================================*
  CM Support
  *=======================================================================*/

// s7 (values) returns (), we add void to signify no return values

s7_pointer s7_void_value(s7_scheme *sc, s7_pointer args)
{
  return s7_unspecified(sc);
}

// custom port handers route stdout and stderr to CM displays

#define OUTBUFSIZ 1024
#define MAXBUFPOS OUTBUFSIZ-2 // leave room for null char

static void cm_stdout(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  static char stdoutbuf[OUTBUFSIZ];
  //  static int stdoutpos = 0;
  stdoutbuf[0] = c;
  stdoutbuf[1] = (char)NULL;
  if (c == '\n')
  {
    stdoutbuf[2] = (char)NULL;
    stdoutbuf[1] = stdoutbuf[0];
    stdoutbuf[0] = '\r';
  }    
  Console::getInstance()->printOutput(stdoutbuf);
}

/*
static void cm_stderr(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  static char stderrbuf[OUTBUFSIZ];
  //  static int stderrpos = 0;
  stderrbuf[0] = c;
  stderrbuf[1] = (char)NULL;
  cm_print_error(stderrbuf);
}
*/

void mus_error_to_grace(int type, char *msg)
{
  /* Some errors are raised in the CLM C code that does not assume
     there's an extension language around, so you have to catch such
     errors via mus_error_set_handler, then call s7_error (or
     whatever) at that point. */
  s7_error(s7, 
           s7_make_symbol(s7, "mus-error"), 
           s7_cons(s7,
                   s7_make_string(s7, msg),
                   s7_nil(s7)));
}

/*=======================================================================*
  Scheme Runtime Support
  *=======================================================================*/

void loadCode(juce::String file, const char* code, int size, bool trace)
{
  if (trace)
    Console::getInstance()->printOutput(file + "\n");
  char str [size + 7 + 1 + 1]; // last 1 is for adding null char
  strcpy(str, "(begin ");
  strcat(str, code);
  strcat(str, ")");
  s7_eval_c_string(s7, str);
}

bool SchemeThread::init()
{
  /* initialize the interpreter; s7 is declared in xen.h */
  s7 = s7_init();
  if (!s7) 
    return false;

  scheme=s7;

  /* initialize the xen stuff (hooks and the xen s7 FFI) */
// HKT FIXME
  s7_xen_initialize(s7);

  /* initialize sndlib with all the functions linked into s7 */
  Init_sndlib(); 

  /* install hander to route lowlevel sndlib C errors up to Grace */
  mus_error_set_handler(mus_error_to_grace);

  /* Install custom port handlers for stdout and stderr */
  s7_set_current_output_port(s7, s7_open_output_function(s7, cm_stdout));
  // this is handled by *error-hook* now
  //  s7_set_current_error_port(s7, s7_open_output_function(s7, cm_stderr));

  // Cache the common constants
  schemeVoid=s7_unspecified(s7);
  schemeTrue=s7_t(s7);
  schemeFalse=s7_f(s7);
  schemeNil=s7_nil(s7);
  schemeError=s7_gensym(s7, "s7-error");
  s7_define_constant(s7, "+s7-error+", schemeError); // returned by *error-hook*
  s7_define_function(s7, "void", s7_void_value, 0, 0, false, "void value");

  // initalize CM 
  cm_init(s7);
  bool tr=false;
  loadCode("s7.scm", SchemeSources::s7_scm, 
	   SchemeSources::s7_scmSize, tr);
  schemeError=s7_name_to_value(s7,"+s7-error+");
  loadCode("utilities.scm", SchemeSources::utilities_scm, SchemeSources::utilities_scmSize, tr);
  loadCode("loop.scm", SchemeSources::loop_scm, SchemeSources::loop_scmSize, tr);
  loadCode("patterns.scm", SchemeSources::patterns_scm, SchemeSources::patterns_scmSize, tr);
  loadCode("toolbox.scm", SchemeSources::toolbox_scm, SchemeSources::toolbox_scmSize, tr);
  loadCode("envelopes.scm", SchemeSources::envelopes_scm, SchemeSources::envelopes_scmSize, tr);
  loadCode("spectral.scm", SchemeSources::spectral_scm, SchemeSources::spectral_scmSize, tr);
  loadCode("sal.scm", SchemeSources::sal_scm, SchemeSources::sal_scmSize, tr);
  loadCode("ports.scm", SchemeSources::ports_scm, SchemeSources::ports_scmSize, tr);
  loadCode("fomus.scm", SchemeSources::fomus_scm, SchemeSources::fomus_scmSize, tr);
  loadCode("processes.scm", SchemeSources::processes_scm, SchemeSources::processes_scmSize, tr);
  loadCode("plot.scm", SchemeSources::plot_scm, SchemeSources::plot_scmSize, tr);
  loadCode("automata.scm", SchemeSources::automata_scm, SchemeSources::automata_scmSize, tr);
  loadCode("sndlib-ws.scm", SchemeSources::sndlibws_scm, SchemeSources::sndlibws_scmSize, tr);
  loadCode("osc.scm", SchemeSources::osc_scm, SchemeSources::osc_scmSize, tr);
  loadCode("sc.scm", SchemeSources::sc_scm, SchemeSources::sc_scmSize, tr);
  // need this for some .ins files...
  XEN_EVAL_C_STRING("(provide 'snd-ws.scm)");
 
  juce::String str=juce::String();

  // Initialize CLM variables. Make WAV the default audio format
  // everywhere since all the OSes support it and the built in player
  // can handle it. For Grace initialize *clm-player* to our built in
  // play() function, install the user's preferences for :srate,
  // :channels and :play values and autoload instruments.

  // TODO: this code actually duplicates the menu Commands that do
  // these things because this function is called before the Scheme
  // process is actually running.  The fix would be to break this
  // function into two parts: a startScheme() that does then generic
  // start up and then an init() that customizes. the latter function
  // could then call SchemeThread::getIntance() function.s

  Preferences* pref=Preferences::getInstance();
  str << "(begin"
      << "  (set! *clm-player* play)"
      << "  (set! *clm-srate* " << pref->getIntProp("SndLibSrate", 44100) << ")"
      << "  (set! *clm-channels* " << pref->getIntProp("SndLibChannels", 1) << ")";
  if (pref->getBoolProp("SndLibAutoPlay", true))
    str << "  (set! *clm-play* #t)";
  else 
    str << "  (set! *clm-play* #f)";
  str << ")";
  s7_eval_c_string(s7,str.toUTF8());

  juce::String fmat=SndLib::getInstance()->getAudioFormat();
  if (fmat.equalsIgnoreCase("WAV"))
    s7_eval_c_string(s7,"(begin (set! *clm-header-type* mus-riff) (set! *clm-sample-type* mus-lshort) (set! *clm-file-name* \"test.wav\"))");
  else if (fmat.equalsIgnoreCase("AIFF"))
    s7_eval_c_string(s7,"(begin (set! *clm-header-type* mus-aifc) (set! *clm-sample-type* mus-bshort) (set! *clm-file-name* \"test.aif\"))");
  else if (fmat.equalsIgnoreCase("SND"))
    s7_eval_c_string(s7,"(begin (set! *clm-header-type* mus-next) (set! *clm-sample-type* mus-bshort) (set! *clm-file-name* \"test.snd\"))");
  SndLib::getInstance()->autoLoadInstruments();

  return true;
}

void SchemeThread::cleanup()
{
  free(s7);
}

juce::String SchemeThread::getLispVersion()
{
  juce::String str=juce::String();
  str << "S7 Scheme" << " " << juce::String(S7_VERSION) << " (" << juce::String(S7_DATE)<< ")"
      << ", Sndlib " << juce::String(SNDLIB_VERSION)
      << " " << SysInfo::getCopyright( "William Schottstaedt");
  return str;
}

void SndLib::restoreInstruments()
{
  juce::FileChooser chooser ("Export Instruments",
                             juce::File::getSpecialLocation(juce::File::userHomeDirectory));
  if (!chooser.browseForDirectory() )
    return;
  juce::File directory (chooser.getResult());  
  juce::MemoryInputStream zipstream (Instruments::ins_zip,
                                     Instruments::ins_zipSize,
                                     false);
  juce::ZipFile archive(&zipstream, false);
  archive.uncompressTo(directory, true);
  int num = archive.getNumEntries();
  juce::String msg ("Exported ");
  msg << num << " files to '" << directory.getFullPathName() << "'.\n";
  Console::getInstance()->printOutput(msg);
}

void SndLib::restoreInstrument(juce::XmlElement* ins)
{
  juce::FileChooser chooser
    ("Export Instrument", juce::File::getSpecialLocation(juce::File::userHomeDirectory));
  if (!chooser.browseForDirectory() )
    return;
  juce::File directory (chooser.getResult());  
  juce::StringArray names;
  juce::String text = ins->getStringAttribute("File");
  names.add(text);
  text = ins->getStringAttribute("Depend");
  if (text.isNotEmpty())
    names.addTokens(text, false);
  text = ins->getStringAttribute("Synthdef");
  if (text.isNotEmpty())
    names.add(text);
  text = ins->getStringAttribute("Examples");
  if (text.isNotEmpty())
    names.addTokens(text, false);
  for (int i = 0; i < names.size(); i++)
  {
    juce::File file (directory.getChildFile(names[i]));
    text = getInstrumentCode(names[i], false);
    file.replaceWithText(text);

  }
  juce::String msg ("Exported 1 file");
  if (names.size() == 2)
    msg << " and 1 auxiliary file";
  else if (names.size() > 2) 
    msg << " and " << names.size() << "auxiliary files";
  msg << " to '" << directory.getFullPathName() << "'.\n";
  Console::getInstance()->printOutput(msg);
}

bool SndLib::getAutoPlay()
{
  return Preferences::getInstance()->getBoolProp("SndLibAutoPlay", true);
}

void SndLib::setAutoPlay(bool ap)
{
  juce::String st="(begin (set! *clm-play* ";
  st << ((ap) ? "#t" : "#f") << ") (void))";
  Preferences::getInstance()->setIntProp("SndLibAutoPlay", ap);
  SchemeThread::getInstance()->eval(st, false);
} 

int SndLib::getSrate()
{
  return Preferences::getInstance()->getIntProp("SndLibSrate", 44100);
}

void SndLib::setSrate(int sr)
{
  juce::String st="(begin (set! *clm-srate* " + juce::String(sr) + ") (void))";
  Preferences::getInstance()->setIntProp("SndLibSrate", sr);
  SchemeThread::getInstance()->eval(st, false);
}

int SndLib::getChannels()
{
  return Preferences::getInstance()->getIntProp("SndLibChannels", 1);
}

void SndLib::setChannels(int ch)
{
  juce::String st="(begin (set! *clm-channels* " + juce::String(ch) + ") (void))";
  Preferences::getInstance()->setIntProp("SndLibChannels", ch);
  SchemeThread::getInstance()->eval(st, false);
}

juce::String SndLib::getAudioFormat()
{
  return Preferences::getInstance()->getStringProp("SndLibAudioFormat", "WAV");
}

void SndLib::setAudioFormat(juce::String format)
{
  juce::String head, data, file, code;
  if (format.equalsIgnoreCase("WAV"))
  {
    head="mus-riff";
    data="mus-lshort";
    file="test.wav";
  }
  else if (format.equalsIgnoreCase("AIFF"))
  {
    head="mus-aifc";
    data="mus-bshort";
    file="test.aif";
  }
  else if (format.equalsIgnoreCase("SND"))
  {
    head="mus-next";
    data="mus-bshort";
    file="test.snd";
  }
  else 
    return;

  Preferences::getInstance()->setStringProp("SndLibAudioFormat", format);

  code << "(begin"
       << " (set! *clm-header-type* " << head << ")"
       << " (set! *clm-sample-type* " << data << ")"
       << " (set! *clm-file-name* " << file.quoted() << ")"
       << "(void))";
  SchemeThread::getInstance()->eval(code, false);
}

/*=======================================================================*
  Instrument Browser
  *=======================================================================*/

class InstrumentWindow : public juce::DocumentWindow
{
public:
  InstrumentWindow()
    : juce::DocumentWindow("Instruments", ColorThemeIDs::getWindowBackgroundColor(),
                           juce::DocumentWindow::allButtons)
  {
  }
  ~InstrumentWindow()
  {
  }
  void closeButtonPressed()
  {
    delete this;
  }
};

class InstrumentTable : public juce::Component,
                        public juce::TableListBoxModel,
public juce::Button::Listener
{
  juce::TableListBox* table; 
  juce::Font font;
  juce::XmlElement* insData;
  juce::XmlElement* columnList;
  juce::XmlElement* dataList;
  juce::TextButton* loadInstButton;
  juce::ToggleButton* autoLoadButton;
  juce::TextButton* editInstButton;
//  juce::TextButton* examplesButton;
  juce::TextButton* exportButton;
  juce::TextButton* exportAllButton;
  int selRow;
  int numRows; 
  static const int AutoloadColumn=5;
  //Column sorting class

  class DemoDataSorter
  {
    const juce::String attributeToSort;
    const int direction;
  public:
    static const int BrowserMode = 1;
    static const int ExportMode = 2;    

    DemoDataSorter(const juce::String attributeToSort_, bool forwards)
      : attributeToSort(attributeToSort_), 
        direction(forwards ? 1 : -1)
    {
    }
    int compareElements(juce::XmlElement* first, juce::XmlElement* second) const
    {
      int result = first->getStringAttribute(attributeToSort).
        compareNatural(second->getStringAttribute(attributeToSort));
      juce::String indexString ("Name");
      if (result == 0) //Tie breaker
        result = first->getStringAttribute(indexString).compareNatural(second->getStringAttribute(indexString));
      return direction * result;
    }
  };

  void loadInstrument(juce::String file);
  void openInstrument(juce::String file);
  void loadTableData();
  const juce::String getAttributeNameForColumnId(const int columnId);

public:

  InstrumentTable();
  ~InstrumentTable();
  int getNumRows();
  void selectedRowsChanged(int r);
  void paintRowBackground(juce::Graphics& g, int rowNumber, int width, 
			  int height, bool rowIsSelected);
  void paintCell(juce::Graphics& g, int rowNumber, int columnId, 
		 int width, int height, bool rowIsSelected);
  void sortOrderChanged(int newSortColumnId, bool isForwards);
  int getColumnAutoSizeWidth(int columnId);
  void resized();
  void cellDoubleClicked(int rowNumber,	int columnId, const juce::MouseEvent &e);
  void returnKeyPressed(int row);
  void buttonClicked (juce::Button* button);
  static void openInstrumentTable();
};

void SndLib::openInstrumentBrowser()
{
  InstrumentTable* ins = new InstrumentTable();
  InstrumentWindow* win = new InstrumentWindow();
  ins->setVisible(true);
  win->setUsingNativeTitleBar(true);
  win->centreWithSize(ins->getWidth(), ins->getHeight());
  //  win->setContentComponent(ins);
  win->setContentOwned(ins, true);
  win->setDropShadowEnabled(true);
  win->setWantsKeyboardFocus(false);
  win->setResizable(true, true); 
  win->setVisible(true);
}

juce::XmlElement* SndLib::getInstrumentTable()
{
  // return instrument table or list of instruments in it
  return instable.get();
}

juce::XmlElement* SndLib::getInstrumentTableInstruments()
{
  // return instrument table or list of instruments in it
  return instable->getChildByName("instruments");
}


juce::XmlElement* SndLib::getInstrumentElement(juce::String filename)
{
  juce::XmlElement* insts = getInstrumentTableInstruments();
  forEachXmlChildElement (*insts, e)
  {
    if (e->getStringAttribute("File") == filename)
      return e;
  }
  return NULL;
}

//
// Instrument Loading and AutoLoading
// 

juce::String SndLib::getInstrumentCode(juce::String filename, bool forEval)
{
  // filename can be a single file or list of space delimited files
  // create zip archive from embedded resource
  juce::MemoryInputStream zipstream (Instruments::ins_zip, Instruments::ins_zipSize, false);
  juce::StringArray filenames;
  filenames.addTokens(filename, false);
  juce::ZipFile archive(&zipstream, false);
  juce::InputStream* inst = 0;
  juce::String text = (forEval) ? "(begin " : juce::String();
  for (int i = 0; i < filenames.size(); i++)
  {
    // create file path in zip file
    juce::String path = filenames[i];
    int index = archive.getIndexOfFileName(path);
    if (index < 0)
      continue;
    inst = archive.createStreamForEntry(index);
    if (!inst)
      continue;
    text << inst->readEntireStreamAsString();
    delete inst;
  }
  if (forEval) text << ")";
  inst = 0;
  return text;
}

void SndLib::getInstrumentFilesToLoad(juce::XmlElement* ins, juce::StringArray& files)
{
  // return files to load for ins, including unloaded dependencies
  juce::StringArray depends;
  depends.addTokens(ins->getStringAttribute("Depend"), false);
  for (int i = 0; i < depends.size(); i++)
  {
    juce::XmlElement* s = getInstrumentElement(depends[i]);
    if (s && !s->getBoolAttribute("Loaded"))
    {
      juce::String f = s->getStringAttribute("File");
      if (!files.contains(f) )
        files.add(f);
    }
  }
  files.add(ins->getStringAttribute("File")); // always load this one
}

void SndLib::updateAutoLoaded()
{
  juce::XmlElement* insts = getInstrumentTableInstruments();
  juce::String s = juce::String();
  forEachXmlChildElement (*insts, e)
  {
    if (e->getBoolAttribute("AutoLoad"))
    {
      if (s.isEmpty())
        s=e->getStringAttribute("File");
      else
        s << " " << e->getStringAttribute("File");
    }
  }
  //std::cout << "setting prefs=" << s.toUTF8() << "\n";
  Preferences::getInstance()->setStringProp("SndLibAutoLoad",s);
}

void SndLib::autoLoadInstruments()
{
  juce::String str=Preferences::getInstance()->getStringProp("SndLibAutoLoad");
  if (str.isEmpty()) return;
  // fill array of instruments to autoload
  juce::StringArray autos;
  autos.addTokens(str, false);
  if (autos.size() == 0) return;
  // fill array of all files to load 
  juce::StringArray loads;
  for (int i = 0; i < autos.size(); i++)
  {
    // get xml instrument element
    juce::XmlElement* ins = SndLib::getInstance()->getInstrumentElement(autos[i]);
    getInstrumentFilesToLoad(ins, loads);
  }
  
  for (int i = 0; i < loads.size(); i++)  
  {
    juce::String str = getInstrumentCode(loads[i], true);
    //      std::cout << "loading: " << loads[i].toUTF8() << "\n";
    s7_eval_c_string(s7, str.toUTF8());
    juce::XmlElement* ins = SndLib::getInstance()->getInstrumentElement(loads[i]);
    ins->setAttribute("Loaded", "yes");
  }  
}

void SndLib::loadInstrumentCode(juce::XmlElement* ins)
{
  juce::StringArray loads;
  getInstrumentFilesToLoad(ins, loads);
  for (int i = 0; i < loads.size(); i++)  
  {
    juce::String msg = "Loading " + loads[i] + "\n";
    juce::String str = getInstrumentCode(loads[i], true);
    Console::getInstance()->printOutput(msg);
    SchemeThread::getInstance()->eval(str, true);
    juce::XmlElement* x = SndLib::getInstance()->getInstrumentElement(loads[i]);
    x->setAttribute("Loaded", "yes");
  }  
}

/*=======================================================================*
  The XML Table Display
  *=======================================================================*/

InstrumentTable::InstrumentTable()
  : table(0),
    font (14.0f),
    insData (0),
    columnList(0),
    dataList(0),
    loadInstButton (0),
    autoLoadButton (0),
    editInstButton (0),
//    examplesButton (0),
    exportButton (0),
    exportAllButton (0),
    selRow(-1)
{
  // Load instrument data from the embedded XML file..
  loadTableData();
  // Create table and add it to this component..
  addAndMakeVisible (table = new juce::TableListBox ("SndLib Instruments", this));
  // give it a border
  table->setColour(juce::ListBox::outlineColourId, juce::Colours::grey);
  table->setOutlineThickness(1);
  table->getHeader().setStretchToFitActive(true);
  // Add the columns to the table header and calculate widths of
  // columns and table
  int columnid=1, tablewidth=0;
  forEachXmlChildElement(*columnList, columnXml)
  {
    // heading width includes min space for sorting triangle
    int width=juce::jmax(font.getStringWidth
                         (columnXml->getStringAttribute("Name")),
                         75); 
    // column width is data width max heading width
    int columnwidth = juce::jmax( getColumnAutoSizeWidth(columnid), width);
    table->getHeader().addColumn(columnXml->getStringAttribute("Name"),
                                 columnXml->getIntAttribute("Id"),
                                 columnwidth, 50, 400,
                                 juce::TableHeaderComponent::defaultFlags);
    tablewidth += columnwidth;
    columnid++;
  }
  // sort forward by the ID column
  table->getHeader().setSortColumnId (1, true);
  addAndMakeVisible(loadInstButton = new juce::TextButton ("Load Instrument"));
  loadInstButton->addListener (this);
  loadInstButton->setEnabled(false);

  addAndMakeVisible (autoLoadButton = new juce::ToggleButton("Load at startup"));
  autoLoadButton->addListener (this);
  autoLoadButton->setEnabled(false);

  addAndMakeVisible(editInstButton = new juce::TextButton ("Edit Instrument"));
  editInstButton->addListener (this);
  editInstButton->setEnabled(false);

/*
  addAndMakeVisible (examplesButton = new juce::TextButton ("Open Examples"));
  examplesButton->addListener (this);
  examplesButton->setEnabled(false);
*/
  
  addAndMakeVisible (exportButton = new juce::TextButton ("Export..."));
  exportButton->addListener(this);
  exportButton->setEnabled(false);

  addAndMakeVisible (exportAllButton = new juce::TextButton ("Export All..."));
  exportAllButton->addListener(this);
  exportAllButton->setEnabled(true);

  setSize(tablewidth + 60, 16 + 300 + 16 + 24 + 16);
}

InstrumentTable::~InstrumentTable()
{
  deleteAllChildren();
}

void InstrumentTable::resized()
{
  table->setBounds(16, 16, getWidth() - 32, getHeight() - 16 - 24 - 16 - 16);
  int y = getBottom() - 16 - 24;
  int bw = 116; // 132
  loadInstButton->setBounds (16, y, bw, 24);
  autoLoadButton->setBounds(loadInstButton->getRight() + 8, y, bw, 24);
  editInstButton->setBounds(autoLoadButton->getRight() + 8, y, bw, 24);
//  examplesButton->setBounds(editInstButton->getRight() + 16, y, bw, 24);
  exportAllButton->setBounds(table->getRight() - bw, y, bw, 24);
  exportButton->setBounds(exportAllButton->getX() - 16 - bw, y, bw, 24);
}

void InstrumentTable::buttonClicked (juce::Button* button)
{
  if (button == exportAllButton)
  {
    SndLib::getInstance()->restoreInstruments();
    return;
  }
  juce::XmlElement* ins = dataList->getChildElement(selRow);
  if (ins == 0) return;
  juce::String path, code;
  if (button == loadInstButton)
  {
    SndLib::getInstance()->loadInstrumentCode(ins);
    // have to set Load flag for all instruments with the same file!
    juce::String file = ins->getStringAttribute("File");
    juce::XmlElement* insts = SndLib::getInstance()->getInstrumentTableInstruments();
    forEachXmlChildElement (*insts, i)
    {
      if (i->getStringAttribute("File") == file)
        i->setAttribute("Loaded", "yes");
    }
    repaint();
  } 
  else if (button == editInstButton)
  {
    path = ins->getStringAttribute("File");
    code = SndLib::getInstance()->getInstrumentCode(path, false);
    if (code.isEmpty())
      return;
//    std::cout << "Editing file '" << path << "'\n";
    juce::File file = juce::File::getSpecialLocation(juce::File::userHomeDirectory).getChildFile(path);
//    std::cout << "JUCE file '" << file.getFullPathName() << "'\n";    
    CodeEditorWindow::newFile(file.getFullPathName(), TextIDs::Lisp, code);
  } 
/*
  else if (button == examplesButton)
  {
    path = ins->getStringAttribute("Examples");
    code = SndLib::getInstance()->getInstrumentCode(path);
    if (code.isEmpty())
      return;
    CodeEditorWindow::newFile(path, TextIDs::Lisp, code);
  }
*/
  else if (button == autoLoadButton)
  {
    if (button->getToggleState())
      ins->setAttribute("AutoLoad", "yes");
    else 
      ins->setAttribute("AutoLoad", "no");
    repaint();
    SndLib::getInstance()->updateAutoLoaded(); // update preferences
  }
  else if (button == exportButton)
  {
    SndLib::getInstance()->restoreInstrument(ins);
  }
}

/*=======================================================================*
  XML Table Code
  *=======================================================================*/

int InstrumentTable::getNumRows()
{
  return numRows;
}

void InstrumentTable::loadTableData()
{
  //  juce::String text=juce::String((const char*) Instruments::table_xml);
  //  XmlDocument dataDoc (text );
  //  insData = dataDoc.getDocumentElement();
  insData = SndLib::getInstance()->getInstrumentTable();
  dataList = insData->getChildByName("instruments");
  columnList = insData->getChildByName("columns");
  numRows = dataList->getNumChildElements();
}

void InstrumentTable::selectedRowsChanged(int selectedRow)
{
  selRow = selectedRow;
  bool active = (selRow > -1);

  loadInstButton->setEnabled(active);
  autoLoadButton->setEnabled(false);
  autoLoadButton->setToggleState(false, juce::dontSendNotification);
  editInstButton->setEnabled(active);
//  examplesButton->setEnabled(false);
  exportButton->setEnabled(active);
  exportAllButton->setEnabled(true);

  if (const juce::XmlElement* row = dataList->getChildElement(selRow))
  {
    autoLoadButton->setEnabled(true);
    autoLoadButton->setToggleState(row->getBoolAttribute("AutoLoad"), juce::dontSendNotification);
    // example button only active if entry has Example attribute set
/*    juce::String file = row->getStringAttribute("Examples");
    if (file.isEmpty())
      examplesButton->setEnabled(false);
    else
      examplesButton->setEnabled(true);
*/
  }
}

const juce::String InstrumentTable::getAttributeNameForColumnId (const int colId) 
{
  // search COLUMNS component for the attribute that matches a column ID
  forEachXmlChildElement (*columnList, columnXml) 
  {
    if (columnXml->getIntAttribute("Id") == colId)
      return columnXml->getStringAttribute("Name");
  }
  return juce::String();
}

void InstrumentTable::paintRowBackground (juce::Graphics& g, int rowNumber, 
                                          int width, int height, 
                                          bool rowIsSelected)
{
  if (rowIsSelected)
  {
    g.fillAll(juce::LookAndFeel::getDefaultLookAndFeel()
              .findColour(juce::TextEditor::highlightColourId));
  }
}

void InstrumentTable::paintCell (juce::Graphics& g, int rowNumber,
                                 int columnId, 
                                 int width, int height,
                                 bool rowIsSelected)
{
  const juce::XmlElement* rowElement = dataList->getChildElement(rowNumber);
  if (rowElement->getBoolAttribute("Loaded"))
    g.setColour (juce::Colours::blue);
  else
    g.setColour (juce::Colours::black);
  g.setFont (font);
  if (rowElement != 0)
  {
    const juce::String text 
      (rowElement->
       getStringAttribute( getAttributeNameForColumnId(columnId)));
    g.drawText (text, 2, 0, width - 4, height, juce::Justification::centredLeft,
                true);
  }
  g.setColour (juce::Colours::black.withAlpha (0.2f));
  g.fillRect (width - 1, 0, 1, height);
}


void InstrumentTable::sortOrderChanged (int newSortColumnId, const bool isForwards) 
{
  if (newSortColumnId != 0)
  {
    DemoDataSorter sorter 
      (getAttributeNameForColumnId (newSortColumnId), isForwards);
    dataList->sortChildElements (sorter);
    table->updateContent();
  }
}

int InstrumentTable::getColumnAutoSizeWidth (int columnId)
{
  int widest = 32;
  // find the widest bit of text in this column..
  for (int i = getNumRows(); --i >= 0;)
  {
    const juce::XmlElement* rowElement = dataList->getChildElement (i);
    if (rowElement != 0) 
    {
      const juce::String text 
        (rowElement->getStringAttribute(getAttributeNameForColumnId (columnId)));
      widest = juce::jmax(widest, font.getStringWidth (text));
    }
  }
  return widest + 8;
}

void InstrumentTable::cellDoubleClicked(int rowNumber, int colId, const juce::MouseEvent &e) 
{
  if (selRow > -1)
  {
    loadInstButton->triggerClick();
//    examplesButton->triggerClick();
  }
}

void InstrumentTable::returnKeyPressed(int row) 
{
  if (selRow >- 1)
  {
    loadInstButton->triggerClick();
  }
}


