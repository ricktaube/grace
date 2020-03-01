/*
  ==============================================================================

  Copyright 1999-2014 Rick Taube.  All rights reserved.

  Licensed under the "Attribution-NonCommercial-ShareAlike" Vizsage
  Public License, which says that non-commercial users may share and
  modify this code but must give credit and share improvements. For
  complete terms please read the text of the full license available at
  this link: http://vizsage.com/license/Vizsage-License-BY-NC-SA.html

  ==============================================================================
*/

#include "Enumerations.h"
#include "Main.h"
#include "Help.h"
#include "Console.h"
#include "CodeEditor.h"
#include "Documentation.h"
#include "Preferences.h"
#include "Resources.h"

juce_ImplementSingleton (Help) ;

Help::Help()
  : documentationDirectory (juce::File())
{
  // Create Help Table
  juce::XmlDocument dataDoc (juce::String((const char*)Resources::doc_xml)); 
  documentationTable = dataDoc.getDocumentElement();
  // locate Grace directory, include version info in name
  documentationDirectory = Grace::getApp().applicationSupportDirectory.getChildFile("doc").getChildFile(SysInfo::getCMVersion());
  //  std::cout << "DOCDIR=" << documentationDirectory.getFullPathName() << "\n";
  // create Symbol help
  addSalSymbolHelp();
  addCommonMusicSymbolHelp();
  addSndLibSymbolHelp();
  addSchemeSymbolHelp();
  if (!documentationDirectory.isDirectory())
  {
    juce::String msg ("Warning: couldn't restore documents to ");
    msg << documentationDirectory.getFullPathName() + "\n";
    if (!documentationDirectory.createDirectory())
      Console::getInstance()->printWarning(msg);
  }
  else
    restoreHelpFiles();
}

Help::~Help()
{
//  deleteAndZero(documentationTable);
  roots.clear();
  sal.clear();
  cm.clear();
  scheme.clear();
  clearSingletonInstance();
}

juce::String Help::getHelpFileText(juce::String filename)
{
  juce::MemoryInputStream zipstream (Resources::doc_zip, Resources::doc_zipSize, false);
  juce::ZipFile archive (&zipstream, false);
  juce::String text = juce::String();
  juce::InputStream* inst = 0;
  int index = archive.getIndexOfFileName(filename);
  if (index >= 0)
    inst = archive.createStreamForEntry(index);
  if (inst)
  {
    text << inst->readEntireStreamAsString();
    delete inst;
  }
  inst = 0;
  return text;
}

void Help::restoreHelpFiles()
{
  using namespace Documentation;
  juce::File htm = documentationDirectory.getChildFile("cm.html");
  juce::File css = documentationDirectory.getChildFile("cm.css");
  juce::File ed  = documentationDirectory.getChildFile("editor.html");
  juce::File gm  = documentationDirectory.getChildFile("gm.html");
  if (!htm.replaceWithText(getHelpFileText("cm.html")))
    Console::getInstance()->printWarning("Warning: couldn't restore " + htm.getFullPathName() + "\n");
  if (!css.replaceWithText(getHelpFileText("cm.css")))
    Console::getInstance()->printWarning("Warning: couldn't restore " + css.getFullPathName() + "\n");
  if (!ed.replaceWithText(getHelpFileText("editor.html")))
    Console::getInstance()->printWarning("Warning: couldn't restore " + ed.getFullPathName() + "\n");
  if (!gm.replaceWithText(getHelpFileText("gm.html")))
    Console::getInstance()->printWarning("Warning: couldn't restore " + gm.getFullPathName() + "\n");
}
  
juce::XmlElement* Help::getXmlMenu(juce::String title)
{
  forEachXmlChildElementWithTagName(*documentationTable, menu, "menu")
  {
    if (menu->getStringAttribute("name") == title)
      return menu;
  }
  return 0;
}

juce::XmlElement* Help::getXmlMenuItem(juce::String title, int index)
{
  juce::XmlElement* menu = getXmlMenu(title);
  return (menu) ? menu->getChildElement(index) : 0;
}

void Help::addHelpMenuItems(juce::PopupMenu& menu, juce::String menuName, 
                            juce::CommandID cmdId, int maxItems,
                            juce::ApplicationCommandManager* manager, 
                            int exportId)
{
  juce::XmlElement* xmlMenu = getXmlMenu(menuName);
  if (!xmlMenu) return;

  int i = 0;
  forEachXmlChildElement(*xmlMenu, xmlItem)
  {
    if (i < maxItems)
    {
      juce::String name = xmlItem->getStringAttribute("name");
#ifdef WITH_FOMUS
      menu.addCommandItem(manager, cmdId + i, name);
#else
      if(name != "Fomus") menu.addCommandItem(manager, cmdId + i, name);
#endif 
      i++;
    }
  }
  if (exportId > 0)
  {
    menu.addSeparator();
    menu.addCommandItem(manager, CommandIDs::HelpExport + exportId);
  }
}

void Help::openHelp(juce::CommandID id)
{
  int comm = CommandIDs::getCommand(id);
  int data = CommandIDs::getCommandData(id);
  juce::XmlElement* help = 0;

  if (comm == CommandIDs::HelpManual)
  {
    if ((help = getXmlMenuItem("Reference", data)))
    {
      juce::String url = help->getStringAttribute("url");
      openHelpInBrowser(url);
    }
  }
  else if (comm == CommandIDs::HelpWebSite)
  {
    if ((help = getXmlMenuItem("Web Sites", data)))
    {
      juce::String url = help->getStringAttribute("url");
      openHelpInBrowser(url);
    }
  }
  else if (comm == CommandIDs::HelpSalExample)
  {
    if ((help = getXmlMenuItem("Sal Examples", data)))
    {
      juce::String file = help->getStringAttribute("file");
      openHelpInEditor(file, getHelpFileText(file));
    }
  }
  else if (comm == CommandIDs::HelpSchemeExample)
  {
    if ((help=getXmlMenuItem("Scheme Examples", data)))
    {
      juce::String file = help->getStringAttribute("file");
      openHelpInEditor(file, getHelpFileText(file));
    }
  }
  else if (comm == CommandIDs::HelpSalTutorial)
  {
    if ((help = getXmlMenuItem("Sal Tutorials", data)))
    {
      juce::String file = help->getStringAttribute("file");
      openHelpInEditor(file, getHelpFileText(file));
    }
  }
  else if (comm == CommandIDs::HelpSchemeTutorial)
  {
    if ((help = getXmlMenuItem("Scheme Tutorials", data)))
    {
      juce::String file = help->getStringAttribute("file");
      openHelpInEditor(file, getHelpFileText(file));
    }
  }
  else if (comm == CommandIDs::HelpSchemeExtra)
  {
    if ((help = getXmlMenuItem("Scheme Extras", data)))
    {
      juce::String file = help->getStringAttribute("file");
      openHelpInEditor(file, getHelpFileText(file));
    }
  }
}

void Help::openHelpInBrowser(juce::String url)
{
  // a local url to the resourced document directory
  if (url.startsWith("$DOCDIR:"))
  {
    url = url.replaceSection(0, 8, "file://" + documentationDirectory.getFullPathName());
  }
  if (url.startsWith("file:"))
  { 
    if (SysInfo::isWindows())
    {
      // Chrome lives in a weird place in the application data directory.
      juce::String chromePath = juce::File::getSpecialLocation(juce::File::userDesktopDirectory).getFullPathName();
      // Trim out "Desktop" from the path name to get the user directory...
      chromePath = chromePath.substring(0, chromePath.length() - 8);
      chromePath << "\\Local Settings\\Application Data\\";
      chromePath << "Google\\Chrome\\Application\\chrome.exe";
      //Web browser executables
      juce::File ie("C:\\Program Files\\Internet Explorer\\iexplore.exe");
      juce::File firefox("C:\\Program Files\\Mozilla Firefox\\firefox.exe");
      juce::File chrome(chromePath);
      //Try each web browser until something works (priority: Firefox, Chrome, IE).
      if (firefox.exists())
        firefox.startAsProcess(url.quoted());
      else if (chrome.exists())
        chrome.startAsProcess(url.quoted());
      else if (ie.exists())
        ie.startAsProcess(url.quoted());
      else
      {
        juce::String msg="Can't open local html help file because none of these browsers exist:\n";
        msg << "\n  " << firefox.getFullPathName()
            << "\n  " << chrome.getFullPathName()
            << "\n  " << ie.getFullPathName() << "\n";
        Console::getInstance()->printWarning(msg);
      }
    }
    else
    {
      url = url.replace(" ", "%20");
      //std::cout << "open help url='" << url << "'\n";
      //Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome "file:///Users/taube/Library/Application%20Support/org.commonmusic.grace.mac/doc/Common%20Music%203.10.3/cm.html#processes"
      //Applications/Firefox.app/Contents/MacOS/firefox "file:///Users/taube/Library/Application%20Support/org.commonmusic.grace.mac/doc/Common%20Music%203.10.3/cm.html#processes"
      juce::URL(url).launchInDefaultBrowser();
    }
  }
  else
  {
    url = url.replace(" ", "%20");
    juce::URL(url).launchInDefaultBrowser();
  }
}

void Help::openHelpInEditor(juce::String path, juce::String code)
{
  int ttyp = TextIDs::fromFileType(path.fromLastOccurrenceOf(".", true, true));
  new CodeEditorWindow(juce::File(), code, ttyp, path);
}

void Help::exportFilesToDirectory(juce::CommandID command)
{
  juce::String name;
  switch (command)
  {
  case CommandIDs::HelpSalExample:
    name = "Sal Examples";
    break;
  case CommandIDs::HelpSalTutorial:
    name = "Sal Tutorials";
    break;
  case CommandIDs::HelpSchemeExample:
    name = "Scheme Extras";
    break;
  case CommandIDs::HelpSchemeTutorial:
    name = "Scheme Tutorials";
    break;
  case CommandIDs::HelpSchemeExtra:
    name = "Scheme Extras";
    break;
  default:
    return;
  }

  juce::String title ("Choose Directory for ");
  title << name;
  juce::XmlElement* menu = getXmlMenu(name);
  if (!menu)
    return;
  juce::FileChooser chooser (title, juce::File::getSpecialLocation(juce::File::userHomeDirectory));
  if (!chooser.browseForDirectory() )
    return;
  juce::File directory (chooser.getResult());
  int num = 0;
  forEachXmlChildElement(*menu, item)
  {
    juce::String file = item->getStringAttribute("file");
    juce::String text = getHelpFileText(file);
    directory.getChildFile(file).replaceWithText(text);
    num++;
  }
 juce::String msg ("Exported ");
 msg << num << " ";
 if (num > 1)
   msg << "files";
 else
   msg << "file";
  msg << " to '" << directory.getFullPathName() << "'.\n";
  Console::getInstance()->printOutput(msg);
}

/*=======================================================================*
  Symbol Help
  *=======================================================================*/

void Help::symbolHelp(juce::String symbol, juce::String helppath)
{
  //  std::cout << "SymbolHelp: '" << symbol << "' (\""<< helppath << "\")\n";

  // search for symbol help in colon delimited help path.
  int i = 0;//, p = 0, l = helppath.length();
  juce::String s, h, r, x;
  while (helppath != juce::String())
  {
    x = juce::String();
    i = helppath.indexOfChar(':');
    if (i == -1) 
    {
      h = helppath;
      helppath = juce::String();
    }
    else 
    {
      h = helppath.substring(0,i);
      helppath = helppath.substring(i + 1);
    }
    if (h == "Sal")
      x = sal[symbol];
    else if (h == "CM")
      x = cm[symbol];
    else if (h == "SndLib")
      x = clm[symbol];
    else if (h == "Scheme")
      x = scheme[symbol];
    else
      return;
    if (x.isNotEmpty())
      return openHelpInBrowser(roots[h] + x);
  }
  // inform that we did not find help for symbol
  juce::String title ("Lookup Help At Cursor");
  juce::String text ("No help found for symbol '");
  text << symbol << "'.";
  juce::AlertWindow::showMessageBoxAsync(juce::AlertWindow::WarningIcon, title, text);
}

void Help::addSalSymbolHelp() {
//  roots.set("Sal", "file://" + documentationDirectory.getFullPathName()+"/");
  roots.set("Sal", "http://commonmusic.sourceforge.net/cm/res/doc/");
  sal.set("begin", "cm.html#begin");
  sal.set("file", "cm.html#file");
  sal.set("function", "cm.html#function");
  sal.set("if", "cm.html#if");
  sal.set("loop", "cm.html#loop");
  sal.set("process", "cm.html#processes");
  sal.set("set", "cm.html#set");
  sal.set("variable", "cm.html#variable");
  sal.set("wait", "cm.html#wait");
  sal.set("with", "cm.html#with");

  sal.set("=", "cm.html#set");
  sal.set("&=", "cm.html#set");
  sal.set("^=", "cm.html#set");
  sal.set("*=", "cm.html#set");
  sal.set("+=", "cm.html#set");
  sal.set(">=", "cm.html#set");
  sal.set("<=", "cm.html#set");
}

void Help::addCommonMusicSymbolHelp() {
// roots.set("CM", "file://" + documentationDirectory.getFullPathName()+"/");
  roots.set("CM", "http://commonmusic.sourceforge.net/cm/res/doc/");
  cm.set("between", "cm.html#between");
  cm.set("butlast", "cm.html#butlast");
  cm.set("cell-state", "cm.html#cell-state");
  cm.set("cents->ratio", "cm.html#cents-_ratio");
  cm.set("chdir", "cm.html#chdir");
  cm.set("cont", "cm.html#cont");
  cm.set("concat", "cm.html#concat");
  cm.set("cs:i", "cm.html#cs:i");
  cm.set("cs:f", "cm.html#cs:f");
  cm.set("decimals", "cm.html#decimals");
  cm.set("delete-metro", "cm.html#delete-metro" );
  cm.set("deltas", "cm.html#deltas");
  cm.set("directory", "cm.html#directory");
  cm.set("discrete", "cm.html#discrete");
  cm.set("divide", "cm.html#divide");
  cm.set("drunk", "cm.html#drunk");

  cm.set("eighth", "cm.html#eighth");
  cm.set("eod?", "cm.html#eod_");
  cm.set("eop?", "cm.html#eop_");
  cm.set("elapsed", "cm.html#elapsed");
  cm.set("envelope->list", "cm.html#envelope-_list");
  cm.set("envelope-average", "cm.html#envelope-average");
  cm.set("envelope-max", "cm.html#envelope-max");
  cm.set("envelope-min", "cm.html#envelope-min");
  cm.set("export-spear-frames", "cm.html#export-spear-frames");

  cm.set("fibonacci", "cm.html#fibonacci");
  cm.set("fifth", "cm.html#fifth");
  cm.set("file-version", "cm.html#file-version");
  cm.set("first", "cm.html#first");
  cm.set("fit", "cm.html#fit");
  cm.set("fm-spectrum", "cm.html#fm-spectrum");
  cm.set("fms:clear-score", "cm.html#fms:clear-score");
  cm.set("fms:delete-score", "cm.html#fms:delete-score");
  cm.set("fms:inst", "cm.html#fms:inst");
  cm.set("fms:load-score", "cm.html#fms:load-score");
  cm.set("fms:mark", "cm.html#fms:mark");
  cm.set("fms:meas", "cm.html#fms:meas");
  cm.set("fms:measdef", "cm.html#fms:measdef");
  cm.set("fms:merge-score", "cm.html#fms:merge-score");
  cm.set("fms:metapart", "cm.html#fms:metapart");
  cm.set("fms:new-score", "cm.html#fms:new-score");
  cm.set("fms:note", "cm.html#fms:note");
  cm.set("fms:part", "cm.html#fms:part");
  cm.set("fms:percinst", "cm.html#fms:percinst");
  cm.set("fms:rest", "cm.html#fms:rest");
  cm.set("fms:run", "cm.html#fms:run");
  cm.set("fms:save-score", "cm.html#fms:save-score");
  cm.set("fms:select-score", "cm.html#fms:select-score");
  cm.set("fms:setting", "cm.html#fms:setting");
  cm.set("fourth", "cm.html#fourth");

  cm.set("golden", "cm.html#golden");
  cm.set("greater?", "cm.html#greater_qm");
  cm.set("greater=?", "cm.html#greater_eqm");

  cm.set("harmonics", "cm.html#harmonics");
  cm.set("hertz", "cm.html#hertz");

  cm.set("in-tempo", "cm.html#in-tempo");
  cm.set("int", "cm.html#int");
  cm.set("interp", "cm.html#interp");
  cm.set("invert", "cm.html#invert");
  cm.set("invert-envelope", "cm.html#invert-envelope");

  cm.set("keynum", "cm.html#keynum");

  cm.set("last", "cm.html#last");
  cm.set("less?", "cm.html#less_qm");
  cm.set("less=?", "cm.html#less_eqm");
  cm.set("list-set!", "cm.html#list-set_");
  cm.set("list-intersection", "cm.html#list-intersection");
  cm.set("list-difference", "cm.html#list-difference");
  cm.set("list-union", "cm.html#list-union");


  cm.set("load", "cm.html#load");
  cm.set("log2", "cm.html#log2");
  cm.set("log10", "cm.html#log10");
  cm.set("loop", "cm.html#loop");
  cm.set("make-automata", "cm.html#make-automata");
  cm.set("make-cycle", "cm.html#make-cycle");
  cm.set("make-graph", "cm.html#make-graph");
  cm.set("make-heap", "cm.html#make-heap");
  cm.set("make-line", "cm.html#make-line");
  //  cm.set("make-list", "cm.html#make-list");
  cm.set("make-markov", "cm.html#make-markov");
  cm.set("make-metro", "cm.html#make-metro" );
  cm.set("make-palindrome", "cm.html#make-palindrome");
  cm.set("make-repeater", "cm.html#make-repeater");
  cm.set("make-rotation", "cm.html#make-rotation");
  cm.set("make-spectrum", "cm.html#make-spectrum");
  cm.set("make-weighting", "cm.html#make-weighting");
  cm.set("markov-analyze", "cm.html#markov-analyze");
  cm.set("maximum", "cm.html#maximum");

  cm.set("metro", "cm.html#metro" );
  cm.set("metros", "cm.html#metros" );
  cm.set("metro?", "cm.html#metro_qm" );
  cm.set("metro-beat", "cm.html#metro-beat" );
  cm.set("metro-dur", "cm.html#metro-dur" );
  cm.set("metro-phase", "cm.html#metro-phase" );
  cm.set("metro-tempo", "cm.html#metro-tempo" );
  cm.set("metro-sync", "cm.html#metro-sync" );

  cm.set("midifile-header", "cm.html#midifile-header");
  cm.set("midifile-import", "cm.html#midifile-import");
  cm.set("minimum", "cm.html#minimum");
  cm.set("minus", "cm.html#minus");
  cm.set("mouse-button", "cm.html#mouse-button");
  cm.set("mod", "cm.html#mod");
  cm.set("mouse-x", "cm.html#mouse-x");
  cm.set("mouse-y", "cm.html#mouse-y");
  cm.set("mm:bend", "cm.html#mm:bend");
  cm.set("mm:ctrl", "cm.html#mm:ctrl");
  cm.set("mm:prog", "cm.html#mm:prog");
  cm.set("mm:press", "cm.html#mm:press");
  cm.set("mm:off", "cm.html#mm:off");
  cm.set("mm:on", "cm.html#mm:on");
  cm.set("mm:touch", "cm.html#mm:touch");
  cm.set("most-positive-fixnum", "cm.html#most-positive-fixnum");
  cm.set("most-negative-fixnum", "cm.html#most-negative-fixnum");
  cm.set("mp:bend", "cm.html#mp:bend");
  cm.set("mp:ctrl", "cm.html#mp:ctrl");
  cm.set("mp:inchans", "cm.html#mp:inchans");
  cm.set("mp:inops", "cm.html#mp:inops");
  cm.set("mp:instruments", "cm.html#mp:instruments");
  cm.set("mp:midi", "cm.html#mp:midi");
  cm.set("mp:off", "cm.html#mp:off");
  cm.set("mp:on", "cm.html#mp:on");
  cm.set("mp:press", "cm.html#mp:press");
  cm.set("mp:prog", "cm.html#mp:prog");
  cm.set("mp:receive", "cm.html#mp:receive");
  cm.set("mp:receive?", "cm.html#mp:receive_qm");
  cm.set("mp:touch", "cm.html#mp:touch");
  cm.set("mp:tuning", "cm.html#mp:tuning");

  cm.set("next", "cm.html#next");
  cm.set("ninth", "cm.html#ninth");
  cm.set("normalize-envelope", "cm.html#normalize-envelope");
  cm.set("note", "cm.html#note");
  cm.set("now", "cm.html#now");
  cm.set("nth", "cm.html#nth");

  cm.set("odds", "cm.html#odds");
  cm.set("osc:bundle", "cm.html#osc:bundle");
  cm.set("osc:close", "cm.html#osc:close");
  cm.set("osc:message", "cm.html#osc:message");
  cm.set("osc:open", "cm.html#osc:open");
  cm.set("osc:receive", "cm.html#osc:receive");
  cm.set("osc:receive?", "cm.html#osc:receive_qm");

  cm.set("pause", "cm.html#pause");
  cm.set("pitch-class", "cm.html#pitch-class");
  cm.set("pi", "cm.html#pi");
  cm.set("pick", "cm.html#pick");
  cm.set("plot", "cm.html#plot");
  cm.set("plot-data", "cm.html#plot-data");
  cm.set("plot-envelopes", "cm.html#plot-envelopes");
  cm.set("plot-hook", "cm.html#plot-hook");
  cm.set("plot-lists", "cm.html#plot-lists");
  cm.set("plus", "cm.html#plus");
  cm.set("print", "cm.html#print");
  cm.set("promise", "cm.html#promise");
  cm.set("process", "cm.html#processes");
  cm.set("pwd", "cm.html#pwd");

  cm.set("qsort!", "cm.html#qsort_");
  cm.set("quantize", "cm.html#quantize");

  cm.set("ran", "cm.html#ran");
  cm.set("ranbeta", "cm.html#ranbeta");
  cm.set("ranbrown", "cm.html#ranbrown");
  cm.set("rancauchy", "cm.html#rancauchy");
  cm.set("ranexp", "cm.html#ranexp");
  cm.set("rangamma", "cm.html#rangamma");
  cm.set("rangauss", "cm.html#rangauss");
  cm.set("ranhigh", "cm.html#ranhigh");
  cm.set("ranlow", "cm.html#ranlow");
  cm.set("ranmiddle", "cm.html#ranmiddle");
  cm.set("ranpink", "cm.html#ranpink");
  cm.set("ranpoisson", "cm.html#ranpoisson");
  cm.set("random-seed", "cm.html#random-seed");
  cm.set("ratio->cents", "cm.html#ratio-_cents");
  cm.set("ratio->steps", "cm.html#ratio-_steps");
  cm.set("remove-duplicates", "cm.html#remove-duplicates");
  cm.set("rescale", "cm.html#rescale");
  cm.set("rest", "cm.html#rest");
  cm.set("rest?", "cm.html#rest_qm");
  cm.set("retrograde", "cm.html#retrograde");
  cm.set("reverse-envelope", "cm.html#reverse-envelope");
  cm.set("rhythm", "cm.html#rhythm");
  cm.set("rm-spectrum", "cm.html#rm-spectrum");

  cm.set("scale", "cm.html#scale");
  cm.set("scale-order", "cm.html#scale-order");
  cm.set("sdif-import", "cm.html#sdif-import");
  cm.set("sdif-import-spectra", "cm.html#sdif-import-spectra");
  cm.set("second", "cm.html#second");
  cm.set("segs", "cm.html#segs");
  cm.set("seventh", "cm.html#seventh");
  cm.set("shell", "cm.html#shell");  
  cm.set("shuffle", "cm.html#shuffle");  
  cm.set("shuffle_", "cm.html#shuffle_");  
  cm.set("sixth", "cm.html#sixth");  
  //  cm.set("sort", "cm.html#sort");
  //  cm.set("sort!", "cm.html#sort_");
  cm.set("spear-import-spectra", "cm.html#spear-import-spectra");
  cm.set("spectrum-add!", "cm.html#spectrum-add_");  
  cm.set("spectrum-amps", "cm.html#spectrum-amps");  
  cm.set("spectrum-copy", "cm.html#spectrum-copy");
  cm.set("spectrum-flip!", "cm.html#spectrum-flip_");  
  cm.set("spectrum-freqs", "cm.html#spectrum-freqs");  
  cm.set("spectrum-invert!", "cm.html#spectrum-invert_");  
  cm.set("spectrum-keys", "cm.html#spectrum-keys");  
  cm.set("spectrum-maxamp", "cm.html#spectrum-maxamp");  
  cm.set("spectrum-maxfreq", "cm.html#spectrum-maxfreq");  
  cm.set("spectrum-minamp", "cm.html#spectrum-minamp");  
  cm.set("spectrum-minfreq", "cm.html#spectrum-minfreq");  
  cm.set("spectrum-pairs", "cm.html#spectrum-pairs");  
  cm.set("spectrum-rescale!", "cm.html#spectrum-rescale_");  
  cm.set("spectrum-size", "cm.html#spectrum-size");  
  cm.set("spectrum-time", "cm.html#spectrum-time");  
  cm.set("sprout", "cm.html#sprout");
  cm.set("sprout-hook", "cm.html#sprout-hook");
  cm.set("state", "cm.html#state");
  cm.set("stop", "cm.html#stop");
  cm.set("sync", "cm.html#sync" );

  cm.set("times", "cm.html#times");
  cm.set("tail", "cm.html#tail");
  cm.set("tendency", "cm.html#tendency");
  cm.set("tenth", "cm.html#tenth");
  cm.set("third", "cm.html#third");
  cm.set("transpose", "cm.html#transpose");
  //  cm.set("unique", "cm.html#unique");
  cm.set("vary", "cm.html#vary");
  cm.set("wait", "cm.html#wait");
  cm.set("xy", "cm.html#xy");
}

void Help::addSchemeSymbolHelp() 
{
  roots.set("Scheme", "http://www.schemers.org/Documents/Standards/R5RS/HTML/");
  scheme.set("*", "r5rs-Z-H-9.html#%25_idx_280");
  scheme.set("+", "r5rs-Z-H-9.html#%25_idx_278");
  scheme.set("-", "r5rs-Z-H-9.html#%25_idx_282");
  scheme.set("/", "r5rs-Z-H-9.html#%25_idx_284");
  scheme.set("<", "r5rs-Z-H-9.html#%25_idx_256");
  scheme.set("<=", "r5rs-Z-H-9.html#%25_idx_260");
  scheme.set("=", "r5rs-Z-H-9.html#%25_idx_254");
  scheme.set("=>", "r5rs-Z-H-7.html#%25_idx_110");
  scheme.set(">", "r5rs-Z-H-9.html#%25_idx_258");
  scheme.set(">=", "r5rs-Z-H-9.html#%25_idx_262");
  scheme.set("abs", "r5rs-Z-H-9.html#%25_idx_286");
  scheme.set("acos", "r5rs-Z-H-9.html#%25_idx_326");
  scheme.set("and", "r5rs-Z-H-7.html#%25_idx_118");
  scheme.set("angle", "r5rs-Z-H-9.html#%25_idx_344");
  scheme.set("append", "r5rs-Z-H-9.html#%25_idx_420");
  scheme.set("apply", "r5rs-Z-H-9.html#%25_idx_556");
  scheme.set("asin", "r5rs-Z-H-9.html#%25_idx_324");
  scheme.set("assoc", "r5rs-Z-H-9.html#%25_idx_438");
  scheme.set("assq", "r5rs-Z-H-9.html#%25_idx_434");
  scheme.set("assv", "r5rs-Z-H-9.html#%25_idx_436");
  scheme.set("atan", "r5rs-Z-H-9.html#%25_idx_328");
  scheme.set("#b", "r5rs-Z-H-9.html#%25_idx_228");
  scheme.set("begin", "r5rs-Z-H-7.html#%25_idx_136");
  scheme.set("boolean?", "r5rs-Z-H-6.html#%25_idx_46");
  scheme.set("caar", "r5rs-Z-H-9.html#%25_idx_402");
  scheme.set("cadr", "r5rs-Z-H-9.html#%25_idx_404");
  scheme.set("call-with-current-continuation", "r5rs-Z-H-9.html#%25_idx_566");
  scheme.set("call-with-input-file", "r5rs-Z-H-9.html#%25_idx_588");
  scheme.set("call-with-output-file", "r5rs-Z-H-9.html#%25_idx_590");
  scheme.set("call-with-values", "r5rs-Z-H-9.html#%25_idx_574");
  scheme.set("car", "r5rs-Z-H-9.html#%25_idx_392");
  scheme.set("case", "r5rs-Z-H-7.html#%25_idx_114");
  scheme.set("cdddar", "r5rs-Z-H-9.html#%25_idx_406");
  scheme.set("cddddr", "r5rs-Z-H-9.html#%25_idx_408");
  scheme.set("cdr", "r5rs-Z-H-9.html#%25_idx_396");
  scheme.set("ceiling", "r5rs-Z-H-9.html#%25_idx_304");
  scheme.set("char->integer", "r5rs-Z-H-9.html#%25_idx_480");
  scheme.set("char-alphabetic?", "r5rs-Z-H-9.html#%25_idx_470");
  scheme.set("char-ci<=?", "r5rs-Z-H-9.html#%25_idx_466");
  scheme.set("char-ci<?", "r5rs-Z-H-9.html#%25_idx_462");
  scheme.set("char-ci=?", "r5rs-Z-H-9.html#%25_idx_460");
  scheme.set("char-ci>=?", "r5rs-Z-H-9.html#%25_idx_468");
  scheme.set("char-ci>?", "r5rs-Z-H-9.html#%25_idx_464");
  scheme.set("char-downcase", "r5rs-Z-H-9.html#%25_idx_486");
  scheme.set("char-lower-case?", "r5rs-Z-H-9.html#%25_idx_478");
  scheme.set("char-numeric?", "r5rs-Z-H-9.html#%25_idx_472");
  scheme.set("char-ready?", "r5rs-Z-H-9.html#%25_idx_620");
  scheme.set("char-upcase", "r5rs-Z-H-9.html#%25_idx_484");
  scheme.set("char-upper-case?", "r5rs-Z-H-9.html#%25_idx_476");
  scheme.set("char-whitespace?", "r5rs-Z-H-9.html#%25_idx_474");
  scheme.set("char<=?", "r5rs-Z-H-9.html#%25_idx_456");
  scheme.set("char<?", "r5rs-Z-H-9.html#%25_idx_452");
  scheme.set("char=?", "r5rs-Z-H-9.html#%25_idx_450");
  scheme.set("char>=?", "r5rs-Z-H-9.html#%25_idx_458");
  scheme.set("char>?", "r5rs-Z-H-9.html#%25_idx_454");
  scheme.set("char?", "r5rs-Z-H-6.html#%25_idx_54");
  scheme.set("close-input-port", "r5rs-Z-H-9.html#%25_idx_608");
  scheme.set("close-output-port", "r5rs-Z-H-9.html#%25_idx_610");
  scheme.set("complex?", "r5rs-Z-H-9.html#%25_idx_242");
  scheme.set("cond", "r5rs-Z-H-7.html#%25_idx_106");
  scheme.set("cons", "r5rs-Z-H-9.html#%25_idx_390");
  scheme.set("cos", "r5rs-Z-H-9.html#%25_idx_320");
  scheme.set("current-input-port", "r5rs-Z-H-9.html#%25_idx_596");
  scheme.set("current-output-port", "r5rs-Z-H-9.html#%25_idx_598");
  scheme.set("#d", "r5rs-Z-H-9.html#%25_idx_232");
  scheme.set("define", "r5rs-Z-H-8.html#%25_idx_190");
  scheme.set("define-syntax", "r5rs-Z-H-8.html#%25_idx_198");
  scheme.set("delay", "r5rs-Z-H-7.html#%25_idx_142");
  scheme.set("denominator", "r5rs-Z-H-9.html#%25_idx_300");
  scheme.set("display", "r5rs-Z-H-9.html#%25_idx_624");
  scheme.set("do", "r5rs-Z-H-7.html#%25_idx_138");
  scheme.set("dynamic-wind", "r5rs-Z-H-9.html#%25_idx_576");
  scheme.set("#e", "r5rs-Z-H-9.html#%25_idx_236");
  scheme.set("else", "r5rs-Z-H-7.html#%25_idx_108");
  scheme.set("eof-object?", "r5rs-Z-H-9.html#%25_idx_618");
  scheme.set("eq?", "r5rs-Z-H-9.html#%25_idx_216");
  scheme.set("equal?", "r5rs-Z-H-9.html#%25_idx_218");
  scheme.set("eqv?", "r5rs-Z-H-9.html#%25_idx_210");
  scheme.set("eval", "r5rs-Z-H-9.html#%25_idx_578");
  scheme.set("even?", "r5rs-Z-H-9.html#%25_idx_272");
  scheme.set("exact->inexact", "r5rs-Z-H-9.html#%25_idx_346");
  scheme.set("exact?", "r5rs-Z-H-9.html#%25_idx_250");
  scheme.set("exp", "r5rs-Z-H-9.html#%25_idx_314");
  scheme.set("expt", "r5rs-Z-H-9.html#%25_idx_332");
  scheme.set("#f", "r5rs-Z-H-9.html#%25_idx_356");
  scheme.set("floor", "r5rs-Z-H-9.html#%25_idx_302");
  scheme.set("for-each", "r5rs-Z-H-9.html#%25_idx_560");
  scheme.set("force", "r5rs-Z-H-9.html#%25_idx_562");
  scheme.set("gcd", "r5rs-Z-H-9.html#%25_idx_294");
  scheme.set("#i", "r5rs-Z-H-9.html#%25_idx_238");
  scheme.set("if", "r5rs-Z-H-7.html#%25_idx_98");
  scheme.set("imag-part", "r5rs-Z-H-9.html#%25_idx_340");
  scheme.set("inexact->exact", "r5rs-Z-H-9.html#%25_idx_348");
  scheme.set("inexact?", "r5rs-Z-H-9.html#%25_idx_252");
  scheme.set("input-port?", "r5rs-Z-H-9.html#%25_idx_592");
  scheme.set("integer->char", "r5rs-Z-H-9.html#%25_idx_482");
  scheme.set("integer?", "r5rs-Z-H-9.html#%25_idx_248");
  scheme.set("interaction-environment", "r5rs-Z-H-9.html#%25_idx_584");
  scheme.set("lambda", "r5rs-Z-H-7.html#%25_idx_96");
  scheme.set("lcm", "r5rs-Z-H-9.html#%25_idx_296");
  scheme.set("length", "r5rs-Z-H-9.html#%25_idx_418");
  scheme.set("let", "r5rs-Z-H-7.html#%25_idx_124");
  scheme.set("let*", "r5rs-Z-H-7.html#%25_idx_128");
  scheme.set("let-syntax", "r5rs-Z-H-7.html#%25_idx_180");
  scheme.set("letrec", "r5rs-Z-H-7.html#%25_idx_132");
  scheme.set("letrec-syntax", "r5rs-Z-H-7.html#%25_idx_182");
  scheme.set("list", "r5rs-Z-H-9.html#%25_idx_416");
  scheme.set("list->string", "r5rs-Z-H-9.html#%25_idx_528");
  scheme.set("list->vector", "r5rs-Z-H-9.html#%25_idx_550");
  scheme.set("list-ref", "r5rs-Z-H-9.html#%25_idx_426");
  scheme.set("list-tail", "r5rs-Z-H-9.html#%25_idx_424");
  scheme.set("list?", "r5rs-Z-H-9.html#%25_idx_414");
  scheme.set("load", "r5rs-Z-H-9.html#%25_idx_630");
  scheme.set("log", "r5rs-Z-H-9.html#%25_idx_316");
  scheme.set("magnitude", "r5rs-Z-H-9.html#%25_idx_342");
  scheme.set("make-polar", "r5rs-Z-H-9.html#%25_idx_336");
  scheme.set("make-rectangular", "r5rs-Z-H-9.html#%25_idx_334");
  scheme.set("make-string", "r5rs-Z-H-9.html#%25_idx_492");
  scheme.set("make-vector", "r5rs-Z-H-9.html#%25_idx_538");
  scheme.set("map", "r5rs-Z-H-9.html#%25_idx_558");
  scheme.set("max", "r5rs-Z-H-9.html#%25_idx_274");
  scheme.set("member", "r5rs-Z-H-9.html#%25_idx_432");
  scheme.set("memq", "r5rs-Z-H-9.html#%25_idx_428");
  scheme.set("memv", "r5rs-Z-H-9.html#%25_idx_430");
  scheme.set("min", "r5rs-Z-H-9.html#%25_idx_276");
  scheme.set("modulo", "r5rs-Z-H-9.html#%25_idx_292");
  scheme.set("negative?", "r5rs-Z-H-9.html#%25_idx_268");
  scheme.set("newline", "r5rs-Z-H-9.html#%25_idx_626");
  scheme.set("not", "r5rs-Z-H-9.html#%25_idx_368");
  scheme.set("null-environment", "r5rs-Z-H-9.html#%25_idx_582");
  scheme.set("null?", "r5rs-Z-H-9.html#%25_idx_410");
  scheme.set("number->string", "r5rs-Z-H-9.html#%25_idx_350");
  scheme.set("number?", "r5rs-Z-H-9.html#%_idx_240");
  scheme.set("numerator", "r5rs-Z-H-9.html#%25_idx_298");
  scheme.set("#o", "r5rs-Z-H-9.html#%25_idx_230");
  scheme.set("odd?", "r5rs-Z-H-9.html#%25_idx_270");
  scheme.set("open-input-file", "r5rs-Z-H-9.html#%25_idx_604");
  scheme.set("open-output-file", "r5rs-Z-H-9.html#%25_idx_606");
  scheme.set("or", "r5rs-Z-H-7.html#%25_idx_120");
  scheme.set("output-port?", "r5rs-Z-H-9.html#%25_idx_594");
  scheme.set("pair?", "r5rs-Z-H-6.html#%25_idx_48");
  scheme.set("peek-char", "r5rs-Z-H-9.html#%25_idx_616");
  scheme.set("port?", "r5rs-Z-H-6.html#%25_idx_60");
  scheme.set("positive?", "r5rs-Z-H-9.html#%25_idx_266");
  scheme.set("procedure?", "r5rs-Z-H-6.html#%25_idx_62");
  scheme.set("quasiquote", "r5rs-Z-H-7.html#%25_idx_150");
  scheme.set("quote", "r5rs-Z-H-7.html#%25_idx_86");
  scheme.set("quotient", "r5rs-Z-H-9.html#%25_idx_288");
  scheme.set("rational?", "r5rs-Z-H-9.html#%25_idx_246");
  scheme.set("rationalize", "r5rs-Z-H-9.html#%25_idx_310");
  scheme.set("read", "r5rs-Z-H-9.html#%25_idx_612");
  scheme.set("read-char", "r5rs-Z-H-9.html#%25_idx_614");
  scheme.set("real-part", "r5rs-Z-H-9.html#%25_idx_338");
  scheme.set("real?", "r5rs-Z-H-9.html#%25_idx_244");
  scheme.set("remainder", "r5rs-Z-H-9.html#%25_idx_290");
  scheme.set("reverse", "r5rs-Z-H-9.html#%25_idx_422");
  scheme.set("round", "r5rs-Z-H-9.html#%25_idx_308");
  scheme.set("scheme-report-environment", "r5rs-Z-H-9.html#%25_idx_580");
  scheme.set("set!", "r5rs-Z-H-7.html#%25_idx_102");
  scheme.set("set-car!", "r5rs-Z-H-9.html#%25_idx_398");
  scheme.set("set-cdr!", "r5rs-Z-H-9.html#%25_idx_400");
  scheme.set("setcar", "r5rs-Z-H-10.html#%25_idx_644");
  scheme.set("sin", "r5rs-Z-H-9.html#%25_idx_318");
  scheme.set("sqrt", "r5rs-Z-H-9.html#%25_idx_330");
  scheme.set("string", "r5rs-Z-H-9.html#%25_idx_494");
  scheme.set("string->list", "r5rs-Z-H-9.html#%25_idx_526");
  scheme.set("string->number", "r5rs-Z-H-9.html#%25_idx_352");
  scheme.set("string->symbol", "r5rs-Z-H-9.html#%25_idx_446");
  scheme.set("string-append", "r5rs-Z-H-9.html#%25_idx_524");
  scheme.set("string-ci<=?", "r5rs-Z-H-9.html#%25_idx_518");
  scheme.set("string-ci<?", "r5rs-Z-H-9.html#%25_idx_514");
  scheme.set("string-ci=?", "r5rs-Z-H-9.html#%25_idx_504");
  scheme.set("string-ci>=?", "r5rs-Z-H-9.html#%25_idx_520");
  scheme.set("string-ci>?", "r5rs-Z-H-9.html#%25_idx_516");
  scheme.set("string-copy", "r5rs-Z-H-9.html#%25_idx_530");
  scheme.set("string-fill!", "r5rs-Z-H-9.html#%25_idx_532");
  scheme.set("string-length", "r5rs-Z-H-9.html#%25_idx_496");
  scheme.set("string-ref", "r5rs-Z-H-9.html#%25_idx_498");
  scheme.set("string-set!", "r5rs-Z-H-9.html#%25_idx_500");
  scheme.set("string<=?", "r5rs-Z-H-9.html#%25_idx_510");
  scheme.set("string<?", "r5rs-Z-H-9.html#%25_idx_506");
  scheme.set("string=?", "r5rs-Z-H-9.html#%25_idx_502");
  scheme.set("string>=?", "r5rs-Z-H-9.html#%25_idx_512");
  scheme.set("string>?", "r5rs-Z-H-9.html#%25_idx_508");
  scheme.set("string?", "r5rs-Z-H-9.html#%_idx_490");
  scheme.set("substring", "r5rs-Z-H-9.html#%25_idx_522");
  scheme.set("symbol->string", "r5rs-Z-H-9.html#%25_idx_444");
  scheme.set("symbol?", "r5rs-Z-H-6.html#%25_idx_50");
  scheme.set("syntax-rules", "r5rs-Z-H-7.html#%25_idx_184");
  scheme.set("#t", "r5rs-Z-H-9.html#%25_idx_354");
  scheme.set("tan", "r5rs-Z-H-9.html#%25_idx_322");
  scheme.set("transcript-off", "r5rs-Z-H-9.html#%25_idx_634");
  scheme.set("transcript-on", "r5rs-Z-H-9.html#%25_idx_632");
  scheme.set("truncate", "r5rs-Z-H-9.html#%25_idx_306");
  scheme.set("values", "r5rs-Z-H-9.html#%25_idx_572");
  scheme.set("vector", "r5rs-Z-H-9.html#%25_idx_540");
  scheme.set("vector->list", "r5rs-Z-H-9.html#%25_idx_548");
  scheme.set("vector-fill!", "r5rs-Z-H-9.html#%25_idx_552");
  scheme.set("vector-length", "r5rs-Z-H-9.html#%25_idx_542");
  scheme.set("vector-ref", "r5rs-Z-H-9.html#%25_idx_544");
  scheme.set("vector-set!", "r5rs-Z-H-9.html#%25_idx_546");
  scheme.set("vector?", "r5rs-Z-H-6.html#%25_idx_58");
  scheme.set("with-input-from-file", "r5rs-Z-H-9.html#%25_idx_600");
  scheme.set("with-output-to-file", "r5rs-Z-H-9.html#%25_idx_602");
  scheme.set("write", "r5rs-Z-H-9.html#%25_idx_622");
  scheme.set("write-char", "r5rs-Z-H-9.html#%25_idx_628");
  scheme.set("#x", "r5rs-Z-H-9.html#%25_idx_234");
  scheme.set("zero?", "r5rs-Z-H-9.html#%25_idx_264");
}


void Help::addSndLibSymbolHelp ()
{
  roots.set("SndLib", "http://ccrma.stanford.edu/software/snd/snd/");
  clm.set("format", "s7.html#format1");
  clm.set("define*", "s7.html#define*");
  clm.set("make-vector", "s7.html#multidimensionalvectors");
  clm.set("make-pulse-train", "sndclm.html#make-pulse-train");
  clm.set("make-rand", "sndclm.html#make-rand");
  clm.set("make-rand-interp", "sndclm.html#make-rand-interp");
  clm.set("make-readin", "sndclm.html#make-readin");
  clm.set("enveloped-mix", "sndscm.html#envelopedmix");
  clm.set("snd-debug", "sndscm.html#snddebug");
  clm.set("make-region-frame-reader", "sndscm.html#makeregionframereader");
  clm.set("make-sample->file", "sndclm.html#make-sampletofile");
  clm.set("make-sawtooth-wave", "sndclm.html#make-sawtooth-wave");
  clm.set("oscil", "sndclm.html#oscil");
  clm.set("make-scalar-mixer", "sndclm.html#make-scalar-mixer");
  clm.set("oscil?", "sndclm.html#oscil?");
  clm.set("out-any", "sndclm.html#out-any");
  clm.set("make-sine-summation", "sndclm.html#make-sine-summation");
  clm.set("outa", "sndclm.html#outa");
  clm.set("*output*", "sndclm.html#*output*");
  clm.set("make-square-wave", "sndclm.html#make-square-wave");
  clm.set("make-src", "sndclm.html#make-src");
  clm.set("make-ssb-am", "sndclm.html#make-ssb-am");
  clm.set("make-sum-of-cosines", "sndclm.html#make-sum-of-cosines");
  clm.set("make-sum-of-sines", "sndclm.html#make-sum-of-sines");
  clm.set("pan-mix", "sndscm.html#panmix");
  clm.set("explode-sf2", "sndscm.html#explodesf2");
  clm.set("pan-mix-vct", "sndscm.html#panmixvct");
  clm.set("exponentially-weighted-moving-average", "sndscm.html#exponentiallyweightedmovingaverage");
  clm.set("make-table-lookup", "sndclm.html#make-table-lookup");
  clm.set("partials->polynomial", "sndclm.html#partialstopolynomial");
  clm.set("sndwarp", "sndscm.html#sndwarp");
  clm.set("make-triangle-wave", "sndclm.html#make-triangle-wave");
  clm.set("partials->wave", "sndclm.html#partialstowave");
  clm.set("make-two-pole", "sndclm.html#make-two-pole");
  clm.set("partials->waveshape", "sndclm.html#partialstowaveshape");
  clm.set("sound->amp-env", "sndscm.html#soundtoamp_env");
  clm.set("make-two-zero", "sndclm.html#make-two-zero");
  clm.set("sound->frame", "sndscm.html#soundtoframe");
  clm.set("all-pass", "sndclm.html#all-pass");
  clm.set("sound->sound-data", "sndscm.html#soundtosounddata");
  clm.set("all-pass?", "sndclm.html#all-pass?");
  clm.set("make-wave-train", "sndclm.html#make-wave-train");
  clm.set("fft-smoother", "sndscm.html#fftsmoother");
  clm.set("make-waveshape", "sndclm.html#make-waveshape");
  clm.set("phase-partials->wave", "sndclm.html#phase-partialstowave");
  clm.set("sound-data->file", "sndscm.html#sounddatatofile");
  clm.set("amplitude-modulate", "sndclm.html#amplitude-modulate");
  clm.set("fft-squelch", "sndscm.html#fftsquelch");
  clm.set("phase-vocoder", "sndclm.html#phase-vocoder");
  clm.set("sound-data->frame", "sndscm.html#sounddatatoframe");
  clm.set("analyse-ladspa", "grfsnd.html#analyseladspa");
  clm.set("map-sound", "sndscm.html#mapsound");
  clm.set("phase-vocoder?", "sndclm.html#phase-vocoder?");
  clm.set("sound-data->sound", "sndscm.html#sounddatatosound");
  clm.set("any-env-channel", "sndscm.html#anyenvchannel");
  clm.set("map-sound-files", "sndscm.html#mapsoundfiles");
  clm.set("place-sound", "sndscm.html#placesound");
  clm.set("apply-ladspa", "grfsnd.html#applyladspa");
  clm.set("array->file", "sndclm.html#arraytofile");
  clm.set("array-interp", "sndclm.html#array-interp");
  clm.set("file->array", "sndclm.html#filetoarray");
  clm.set("file->frame", "sndclm.html#filetoframe");
  clm.set("file->frame?", "sndclm.html#filetoframe?");
  clm.set("file->sample", "sndclm.html#filetosample");
  clm.set("asymmetric-fm?", "sndclm.html#asymmetric-fm?");
  clm.set("file->sample?", "sndclm.html#filetosample?");
  clm.set("play-mixes", "sndscm.html#playmixes");
  clm.set("file->sound-data", "sndscm.html#filetosounddata");
  clm.set("file->vct", "sndscm.html#filetovct");
  clm.set("mark-name->id", "sndscm.html#marknametoid");
  clm.set("play-sines", "sndscm.html#playsines");
  clm.set("filter", "sndclm.html#filter");
  clm.set("autocorrelate", "sndclm.html#autocorrelate");
  clm.set("pluck", "sndscm.html#pluck");
  clm.set("sound-interp", "sndscm.html#soundinterp");
  clm.set("sound-let", "sndscm.html#sound-let");
  clm.set("polar->rectangular", "sndclm.html#polartorectangular");
  clm.set("bagpipe", "sndscm.html#bagpipe");
  clm.set("match-sound-files", "sndscm.html#matchsoundfiles");
  clm.set("polynomial", "sndclm.html#polynomial");
  clm.set("polynomial operations", "sndscm.html#polydoc");
  clm.set("sound-property", "sndscm.html#soundproperty");
  clm.set("max-envelope", "sndscm.html#maxenvelope");
  clm.set("polyshape", "sndclm.html#polyshape");
  clm.set("polyshape?", "sndclm.html#polyshape?");
  clm.set("filter?", "sndclm.html#filter?");
  clm.set("filtered-comb", "sndclm.html#filtered-comb");
  clm.set("filtered-comb?", "sndclm.html#filtered-comb?");
  clm.set("power-env", "sndscm.html#powerenv");
  clm.set("spectral-polynomial", "sndscm.html#spectralpolynomial");
  clm.set("bessel filters", "sndscm.html#analogfilterdoc");
  clm.set("previous-frame", "sndscm.html#previousframe");
  clm.set("bigbird", "sndscm.html#bigbird");
  clm.set("find-mix", "sndscm.html#findmix");
  clm.set("bird", "sndscm.html#bird");
  clm.set("fir-filter", "sndclm.html#fir-filter");
  clm.set("mix->vct", "sndscm.html#mixtovct");
  clm.set("brownian-noise", "sndscm.html#browniannoise");
  clm.set("fir-filter?", "sndclm.html#fir-filter?");
  clm.set("butterworth filters", "sndscm.html#analogfilterdoc");
  clm.set("fm-bell", "sndscm.html#fmbell");
  clm.set("mix-channel", "sndscm.html#mixchannel");
  clm.set("pulse-train", "sndclm.html#pulse-train");
  clm.set("spectrum", "sndclm.html#spectrum");
  clm.set("fm-drum", "sndscm.html#fmdrum");
  clm.set("pulse-train?", "sndclm.html#pulse-train?");
  clm.set("spectrum->coeffs", "sndscm.html#spectrumtocoeffs");
  clm.set("fm-noise", "sndscm.html#fmnoise");
  clm.set("cascade->canonical", "sndscm.html#cascadetocanonical");
  clm.set("fm-talker", "sndscm.html#fmvox");
  clm.set("chain-dsps", "sndscm.html#chaindsps");
  clm.set("fm-trumpet", "sndscm.html#fmtrumpet");
  clm.set("radians->degrees", "sndclm.html#radianstodegrees");
  clm.set("fm-violin", "sndscm.html#vdoc");
  clm.set("radians->hz", "sndclm.html#radianstohz");
  clm.set("fm-voice", "sndscm.html#reson");
  clm.set("mix-frame", "sndscm.html#mixframe");
  clm.set("square-wave", "sndclm.html#square-wave");
  clm.set("focus-follows-mouse", "sndscm.html#focusfollowsmouse");
  clm.set("rand", "sndclm.html#rand");
  clm.set("square-wave?", "sndclm.html#square-wave?");
  clm.set("channel-envelope", "sndscm.html#channelenvelope");
  clm.set("rand-interp", "sndclm.html#rand-interp");
  clm.set("channel-polynomial", "sndscm.html#channelpolynomial");
  clm.set("mix-maxamp", "sndscm.html#mixmaxamp");
  clm.set("rand-interp?", "sndclm.html#rand-interp?");
  clm.set("squelch-vowels", "sndscm.html#squelchvowels");
  clm.set("for-each-child", "sndscm.html#foreachchild");
  clm.set("rand?", "sndclm.html#rand?");
  clm.set("channel-property", "sndscm.html#channelproperty");
  clm.set("for-each-sound-file", "sndscm.html#foreachsoundfile");
  clm.set("src", "sndclm.html#src");
  clm.set("channel-rms", "sndscm.html#channelrms");
  clm.set("mix-name->id", "sndscm.html#mixnametoid");
  clm.set("read-frame", "sndscm.html#readframe");
  clm.set("src-duration", "sndscm.html#srcduration");
  clm.set("channel-sync", "sndscm.html#channelsync");
  clm.set("src-mixes", "sndscm.html#srcmixes");
  clm.set("formant", "sndclm.html#formant");
  clm.set("mix-property", "sndscm.html#mixproperty");
  clm.set("formant?", "sndclm.html#formant?");
  clm.set("channels-equal?", "sndscm.html#channelsequal");
  clm.set("src?", "sndclm.html#src?");
  clm.set("ssb-am", "sndclm.html#ssb-am");
  clm.set("channels=?", "sndscm.html#channels=");
  clm.set("fractional-fourier-transform", "sndscm.html#fractionalfouriertransform");
  clm.set("readin", "sndclm.html#readin");
  clm.set("ssb-am?", "sndclm.html#ssb-am?");
  clm.set("frame*", "sndclm.html#frame*");
  clm.set("mix-sound", "sndscm.html#mixsound");
  clm.set("readin?", "sndclm.html#readin?");
  clm.set("ssb-bank", "sndscm.html#ssbbank");
  clm.set("chebyshev filters", "sndscm.html#analogfilterdoc");
  clm.set("frame+", "sndclm.html#frame+");
  clm.set("mix-sound-data", "sndscm.html#mixsounddata");
  clm.set("ssb-bank-env", "sndscm.html#ssbbankenv");
  clm.set("check-for-unsaved-edits", "sndscm.html#checkforunsavededits");
  clm.set("frame->file", "sndclm.html#frametofile");
  clm.set("ssb-fm", "sndscm.html#ssbfm");
  clm.set("clean-channel", "sndscm.html#cleanchannel");
  clm.set("frame->file?", "sndclm.html#frametofile?");
  clm.set("rectangular->polar", "sndclm.html#rectangulartopolar");
  clm.set("clean-sound", "sndscm.html#cleansound");
  clm.set("frame->frame", "sndclm.html#frametoframe");
  clm.set("clear-array", "sndclm.html#clear-array");
  clm.set("frame->list", "sndclm.html#frametolist");
  clm.set("frame->sample", "sndclm.html#frametosample");
  clm.set("frame->sound", "sndscm.html#frametosound");
  clm.set("region->frame", "sndscm.html#regiontoframe");
  clm.set("clear-selection", "sndscm.html#clearselection");
  clm.set("frame->sound-data", "sndscm.html#frametosounddata");
  clm.set("region->sound-data", "sndscm.html#regiontosounddata");
  clm.set("frame->vct", "sndscm.html#frametovct");
  clm.set("stereo->mono", "sndscm.html#stereotomono");
  clm.set("frame-copy", "sndscm.html#framecopy");
  clm.set("frame-reader-at-end?", "sndscm.html#framereaderatendQ");
  clm.set("mixer*", "sndclm.html#mixermultiply");
  clm.set("clm-load", "sndscm.html#clmload");
  clm.set("frame-reader-chans", "sndscm.html#framereaderchans");
  clm.set("mixer as matrix", "sndscm.html#mixerdoc");
  clm.set("frame-reader-home", "sndscm.html#framereaderhome");
  clm.set("mixer+", "sndclm.html#mixeradd");
  clm.set("frame-reader-position", "sndscm.html#framereaderposition");
  clm.set("mixer-copy", "sndscm.html#mixercopy");
  clm.set("frame-reader?", "sndscm.html#framereaderQ");
  clm.set("mixer-determinant", "sndscm.html#mixer-determinant");
  clm.set("stretch-envelope", "sndscm.html#stretchenvelope");
  clm.set("frame-ref", "sndclm.html#frame-ref");
  clm.set("mixer-inverse", "sndscm.html#mixer-inverse");
  clm.set("region-play-list", "sndscm.html#regionplaylist");
  clm.set("sum-of-cosines", "sndclm.html#sum-of-cosines");
  clm.set("frame-reverse!", "sndscm.html#framereverse");
  clm.set("mixer-poly", "sndscm.html#mixer-poly");
  clm.set("sum-of-cosines?", "sndclm.html#sum-of-cosines?");
  clm.set("frame-set!", "sndclm.html#frame-set!");
  clm.set("mixer-ref", "sndclm.html#mixer-ref");
  clm.set("sum-of-sines", "sndclm.html#sum-of-sines");
  clm.set("frame?", "sndclm.html#frame?");
  clm.set("mixer-set!", "sndclm.html#mixer-set!");
  clm.set("sum-of-sines?", "sndclm.html#sum-of-sines?");
  clm.set("mixer-solve", "sndscm.html#mixer-solve");
  clm.set("superimpose-ffts", "sndscm.html#superimposeffts");
  clm.set("color-mixes", "sndscm.html#colormixes");
  clm.set("mixer-transpose", "sndscm.html#mixer-transpose");
  clm.set("free-frame-reader", "sndscm.html#freeframereader");
  clm.set("mixer?", "sndclm.html#mixer?");
  clm.set("swap-selection-channels", "sndscm.html#swapselectionchannels");
  clm.set("remember-sound-state", "sndscm.html#remembersoundstate");
  clm.set("sync-all", "sndscm.html#sync-all");
  clm.set("freeverb", "sndscm.html#freeverb");
  clm.set("mono->stereo", "sndscm.html#monotostereo");
  clm.set("fullmix", "sndscm.html#fullmix");
  clm.set("moog-filter", "sndscm.html#moogfilter");
  clm.set("table-lookup", "sndclm.html#table-lookup");
  clm.set("table-lookup?", "sndclm.html#table-lookup?");
  clm.set("tap", "sndclm.html#tap");
  clm.set("comb", "sndclm.html#comb");
  clm.set("telephone", "sndscm.html#telephone");
  clm.set("comb?", "sndclm.html#comb?");
  clm.set("compand-channel", "sndscm.html#compandchannel");
  clm.set("goertzel", "sndscm.html#goertzel");
  clm.set("*reverb*", "sndclm.html#*reverb*");
  clm.set("compand-sound", "sndscm.html#compandsound");
  clm.set("concatenate-envelopes", "sndscm.html#concatenateenvelopes");
  clm.set("grani", "sndscm.html#grani");
  clm.set("continue-frame->file", "sndclm.html#continue-frametofile");
  clm.set("continue-sample->file", "sndclm.html#continue-sampletofile");
  clm.set("granulate", "sndclm.html#granulate");
  clm.set("contrast-channel", "sndscm.html#contrastchannel");
  clm.set("granulate?", "sndclm.html#granulate?");
  clm.set("move-locsig", "sndclm.html#move-locsig");
  clm.set("granulated-sound-interp", "sndscm.html#granulatedsoundinterp");
  clm.set("move-mixes", "sndscm.html#movemixes");
  clm.set("move-sound", "sndclm.html#move-sound");
  clm.set("move-sound?", "sndclm.html#move-sound?");
  clm.set("moving-average", "sndclm.html#moving-average");
  clm.set("contrast-enhancement", "sndclm.html#contrast-enhancement");
  clm.set("moving-average?", "sndclm.html#moving-average?");
  clm.set("contrast-sound", "sndscm.html#contrastsound");
  clm.set("moving-length", "sndscm.html#movinglength");
  clm.set("reverse-envelope", "sndscm.html#reverseenvelope");
  clm.set("moving-max", "sndscm.html#movingmax");
  clm.set("moving-rms", "sndscm.html#movingrms");
  clm.set("convolution", "sndclm.html#convolution");
  clm.set("moving-sum", "sndscm.html#movingsum");
  clm.set("graphic equalizer", "sndscm.html#grapheq");
  clm.set("mpg", "sndscm.html#mpg");
  clm.set("convolve", "sndclm.html#convolve");
  clm.set("multiply-arrays", "sndclm.html#multiply-arrays");
  clm.set("transpose-mixes", "sndscm.html#transposemixes");
  clm.set("convolve-files", "sndclm.html#convolvefiles");
  clm.set("ring-modulate", "sndclm.html#ring-modulate");
  clm.set("harmonicizer", "sndscm.html#harmonicizer");
  clm.set("rms", "sndscm.html#rmsgain");
  clm.set("triangle-wave", "sndclm.html#triangle-wave");
  clm.set("triangle-wave?", "sndclm.html#triangle-wave?");
  clm.set("convolve?", "sndclm.html#convolve?");
  clm.set("rms-envelope", "sndscm.html#rmsenvelope");
  clm.set("two-pole", "sndclm.html#two-pole");
  clm.set("copy-frame-reader", "sndscm.html#copyframereader");
  clm.set("hello-dentist", "sndscm.html#hellodentist");
  clm.set("rubber-sound", "sndscm.html#rubbersound");
  clm.set("two-pole?", "sndclm.html#two-pole?");
  clm.set("mus-array-print-length", "sndclm.html#musarrayprintlength");
  clm.set("two-zero", "sndclm.html#two-zero");
  clm.set("run", "sndscm.html#run");
  clm.set("two-zero?", "sndclm.html#two-zero?");
  clm.set("correlate", "sndclm.html#correlate");
  clm.set("unclip-channel", "sndscm.html#unclipchannel");
  clm.set("create-ssb-dialog", "sndscm.html#createssbdialog");
  clm.set("cross-fade (amplitude)", "sndscm.html#mixdoc");
  clm.set("hilbert-transform", "sndscm.html#hilberttransform");
  clm.set("sample->file", "sndclm.html#sampletofile");
  clm.set("cross-fade (frequency domain)", "sndscm.html#fadedoc");
  clm.set("hook-member", "sndscm.html#hookmember");
  clm.set("sample->file?", "sndclm.html#sampletofile?");
  clm.set("cross-synthesis", "sndscm.html#crosssynthesis");
  clm.set("sample->frame", "sndclm.html#sampletoframe");
  clm.set("hz->radians", "sndclm.html#hztoradians");
  clm.set("iir-filter", "sndclm.html#iir-filter");
  clm.set("mus-channel", "sndclm.html#mus-channel");
  clm.set("iir-filter?", "sndclm.html#iir-filter?");
  clm.set("mus-channels", "sndclm.html#mus-channels");
  clm.set("in-any", "sndclm.html#in-any");
  clm.set("mus-close", "sndclm.html#mus-close");
  clm.set("samples->seconds", "sndclm.html#samplestoseconds");
  clm.set("ina", "sndclm.html#ina");
  clm.set("mus-cosines", "sndclm.html#mus-cosines");
  clm.set("inb", "sndclm.html#inb");
  clm.set("mus-data", "sndclm.html#mus-data");
  clm.set("variable-display", "sndscm.html#variabledisplay");
  clm.set("init-ladspa", "grfsnd.html#initladspa");
  clm.set("mus-describe", "sndclm.html#mus-describe");
  clm.set("insert-channel", "sndscm.html#insertchannel");
  clm.set("vct->file", "sndscm.html#vcttofile");
  clm.set("insert-frame", "sndscm.html#insertframe");
  clm.set("mus-feedback", "sndclm.html#mus-feedback");
  clm.set("save-mark-properties", "sndscm.html#savemarkproperties");
  clm.set("vct->frame", "sndscm.html#vcttoframe");
  clm.set("mus-feedforward", "sndclm.html#mus-feedforward");
  clm.set("save-mix", "sndscm.html#savemix");
  clm.set("mus-file-buffer-size", "sndclm.html#musfilebuffersize");
  clm.set("save-mixes", "sndscm.html#savemixes");
  clm.set("db->linear", "sndclm.html#dbtolinear");
  clm.set("mus-file-name", "sndclm.html#mus-file-name");
  clm.set("insert-sound-data", "sndscm.html#insertsounddata");
  clm.set("mus-float-equal-fudge-factor", "sndclm.html#musfloatequalfudgefactor");
  clm.set("insert-vct", "sndscm.html#insertvct");
  clm.set("mus-formant-radius", "sndclm.html#mus-formant-radius");
  clm.set("mus-frequency", "sndclm.html#mus-frequency");
  clm.set("mus-generator?", "sndclm.html#musgeneratorp");
  clm.set("def-clm-struct", "sndscm.html#def-clm-struct");
  clm.set("instruments", "sndclm.html#instruments");
  clm.set("integrate-envelope", "sndscm.html#integrateenvelope");
  clm.set("jc-reverb", "sndscm.html#jcreverb");
  clm.set("mus-hop", "sndclm.html#mus-hop");
  clm.set("vct-polynomial", "sndscm.html#vctpolynomial");
  clm.set("jc-reverb", "sndscm.html#jcrevdoc");
  clm.set("mus-increment", "sndclm.html#mus-increment");
  clm.set("savitzky-golay-filter", "sndscm.html#sgfilter");
  clm.set("mus-input?", "sndclm.html#mus-input?");
  clm.set("sawtooth-wave", "sndclm.html#sawtooth-wave");
  clm.set("define-selection-via-marks", "sndscm.html#defineselectionviamarks");
  clm.set("kalman-filter-channel", "sndscm.html#kalmanfilterchannel");
  clm.set("mus-interp-type", "sndclm.html#mus-interp-type");
  clm.set("sawtooth-wave?", "sndclm.html#sawtooth-wave?");
  clm.set("definstrument", "sndscm.html#definstrument");
  clm.set("mus-interpolate", "sndclm.html#mus-interpolate");
  clm.set("mus-length", "sndclm.html#mus-length");
  clm.set("degrees->radians", "sndclm.html#degreestoradians");
  clm.set("mus-location", "sndclm.html#mus-location");
  clm.set("scale-envelope", "sndscm.html#scaleenvelope");
  clm.set("delay", "sndclm.html#delay");
  clm.set("mus-mix", "sndscm.html#musmix");
  clm.set("scale-mixes", "sndscm.html#scalemixes");
  clm.set("delay-channel-mixes", "sndscm.html#delaychannelmixes");
  clm.set("ladspa-descriptor", "grfsnd.html#ladspadescriptor");
  clm.set("mus-name", "sndclm.html#mus-name");
  clm.set("delay-tick", "sndclm.html#delaytick");
  clm.set("delay?", "sndclm.html#delay?");
  clm.set("mus-offset", "sndclm.html#mus-offset");
  clm.set("scale-sound", "sndscm.html#scalesound");
  clm.set("mus-order", "sndclm.html#mus-order");
  clm.set("scale-tempo", "sndscm.html#scaletempo");
  clm.set("linear->db", "sndclm.html#lineartodb");
  clm.set("linear-src-channel", "sndscm.html#linearsrcchannel");
  clm.set("mus-output?", "sndclm.html#mus-output?");
  clm.set("scan-sound", "sndscm.html#scansound");
  clm.set("mus-phase", "sndclm.html#mus-phase");
  clm.set("scentroid", "sndscm.html#scentroid");
  clm.set("mus-ramp", "sndclm.html#mus-ramp");
  clm.set("list-ladspa", "grfsnd.html#listladspa");
  clm.set("mus-random", "sndclm.html#mus-random");
  clm.set("delete-selection-and-smooth", "sndscm.html#deleteselectionandsmooth");
  clm.set("mus-reset", "sndclm.html#mus-reset");
  clm.set("mus-run", "sndclm.html#mus-run");
  clm.set("mus-scaler", "sndclm.html#mus-scaler");
  clm.set("seconds->samples", "sndclm.html#secondstosamples");
  clm.set("describe-hook",  "sndscm.html#describehook");
  clm.set("describe-mark", "sndscm.html#describemark");
  clm.set("voiced->unvoiced", "sndscm.html#voicedtounvoiced");
  clm.set("disable-control-panel", "sndscm.html#disablecontrolpanel");
  clm.set("locsig", "sndclm.html#locsig");
  clm.set("volterra-filter", "sndscm.html#volterrafilter");
  clm.set("display-bark-fft", "sndscm.html#displaybarkfft");
  clm.set("locsig-ref", "sndclm.html#locsig-ref");
  clm.set("wave-train", "sndclm.html#wave-train");
  clm.set("display-db", "sndscm.html#displaydb");
  clm.set("locsig-reverb-ref", "sndclm.html#locsig-reverb-ref");
  clm.set("wave-train?", "sndclm.html#wave-train?");
  clm.set("locsig-reverb-set!", "sndclm.html#locsig-reverb-set!");
  clm.set("display-scanned-synthesis", "sndscm.html#displayscannedsynthesis");
  clm.set("locsig-set!", "sndclm.html#locsig-set!");
  clm.set("waveshape", "sndclm.html#waveshape");
  clm.set("dissolve-fade", "sndscm.html#dissolvefade");
  clm.set("locsig-type", "sndclm.html#locsig-type");
  clm.set("waveshape?", "sndclm.html#waveshape?");
  clm.set("dither-channel", "sndscm.html#ditherchannel");
  clm.set("locsig?", "sndclm.html#locsig?");
  clm.set("selection->sound-data", "sndscm.html#selectiontosounddata");
  clm.set("waveshaping voice", "sndscm.html#pqwvox");
  clm.set("dither-sound", "sndscm.html#dithersound");
  clm.set("dlocsig", "sndscm.html#dlocsig");
  clm.set("loop-between-marks", "sndscm.html#loopbetweenmarks");
  clm.set("do?", "sndscm.html#dop");
  clm.set("lpc-coeffs", "sndscm.html#lpccoeffs");
  clm.set("weighted-moving-average", "sndscm.html#weightedmovingaverage");
  clm.set("lpc-predict", "sndscm.html#lpcpredict");
  clm.set("dot-product", "sndclm.html#dot-product");
  clm.set("make-all-pass", "sndclm.html#make-all-pass");
  clm.set("make-asymmetric-fm", "sndclm.html#make-asymmetric-fm");
  clm.set("make-bandpass", "sndscm.html#makebandpass");
  clm.set("selection-members", "sndscm.html#selectionmembers");
  clm.set("make-bandstop", "sndscm.html#makebandstop");
  clm.set("make-biquad", "sndscm.html#makebiquad");
  clm.set("window-samples", "sndscm.html#windowsamples");
  clm.set("make-birds", "sndscm.html#makebirds");
  clm.set("make-comb", "sndclm.html#make-comb");
  clm.set("make-convolve", "sndclm.html#make-convolve");
  clm.set("set-global-sync", "sndscm.html#setglobalsync");
  clm.set("make-current-window-display", "sndscm.html#makecurrentwindowdisplay");
  clm.set("make-delay", "sndclm.html#make-delay");
  clm.set("shepard-tone", "sndscm.html#shepardtone");
  clm.set("make-differentiator", "sndscm.html#makedifferentiator");
  clm.set("mus-srate", "sndclm.html#mussrate");
  clm.set("with-local-hook", "sndscm.html#withlocalhook");
  clm.set("make-env", "sndclm.html#make-env");
  clm.set("with-mix", "sndscm.html#with-mix");
  clm.set("make-fft-window", "sndclm.html#make-fft-window");
  clm.set("mus-width", "sndclm.html#mus-width");
  clm.set("make-file->frame", "sndclm.html#make-filetoframe");
  clm.set("mus-xcoeff", "sndclm.html#mus-xcoeff");
  clm.set("make-file->sample", "sndclm.html#make-filetosample");
  clm.set("mus-xcoeffs", "sndclm.html#mus-xcoeffs");
  clm.set("show-disk-space", "sndscm.html#showdiskspace");
  clm.set("with-reopen-menu", "sndscm.html#withreopenmenu");
  clm.set("make-filter", "sndclm.html#make-filter");
  clm.set("mus-ycoeff", "sndclm.html#mus-ycoeff");
  clm.set("with-sound", "sndscm.html#withsound");
  clm.set("make-filtered-comb", "sndclm.html#make-filtered-comb");
  clm.set("mus-ycoeffs", "sndclm.html#mus-ycoeffs");
  clm.set("with-temp-sound", "sndscm.html#withtempsound");
  clm.set("make-fir-filter", "sndclm.html#make-fir-filter");
  clm.set("with-temporary-selection", "sndscm.html#withtemporaryselection");
  clm.set("edit-property", "sndscm.html#editproperty");
  clm.set("make-formant", "sndclm.html#make-formant");
  clm.set("make-frame", "sndclm.html#make-frame");
  clm.set("make-frame!", "sndclm.html#make-frame!");
  clm.set("show-selection", "sndscm.html#showselection");
  clm.set("edot-product", "sndclm.html#edot-product");
  clm.set("make-frame->file", "sndclm.html#make-frametofile");
  clm.set("ws-interrupt?", "sndscm.html#wsinterrupt");
  clm.set("make-frame-reader", "sndscm.html#makeframereader");
  clm.set("next-frame", "sndscm.html#nextframe");
  clm.set("show-smpte-label", "sndscm.html#showsmptelabel");
  clm.set("make-granulate", "sndclm.html#make-granulate");
  clm.set("env", "sndclm.html#env");
  clm.set("make-highpass", "sndscm.html#makehighpass");
  clm.set("normalize-envelope", "sndscm.html#normalizeenvelope");
  clm.set("make-hilbert-transform", "sndscm.html#makehilberttransform");
  clm.set("normalize-sound", "sndscm.html#normalizesound");
  clm.set("silence-all-mixes", "sndscm.html#silenceallmixes");
  clm.set("env-expt-channel", "sndscm.html#envexptchannel");
  clm.set("make-iir-filter", "sndclm.html#make-iir-filter");
  clm.set("normalized-mix", "sndscm.html#normalizedmix");
  clm.set("silence-mixes", "sndscm.html#silencemixes");
  clm.set("env-interp", "sndclm.html#env-interp");
  clm.set("make-locsig", "sndclm.html#make-locsig");
  clm.set("notch", "sndclm.html#notch");
  clm.set("env-mixes", "sndscm.html#envmixes");
  clm.set("make-lowpass", "sndscm.html#makelowpass");
  clm.set("notch-channel", "sndscm.html#notchchannel");
  clm.set("sine-bank", "sndclm.html#sine-bank");
  clm.set("notch-out-rumble-and-hiss", "sndscm.html#notchoutrumbleandhiss");
  clm.set("sine-env-channel", "sndscm.html#sineenvchannel");
  clm.set("make-mixer", "sndclm.html#make-mixer");
  clm.set("notch-selection", "sndscm.html#notchselection");
  clm.set("sine-ramp", "sndscm.html#sineramp");
  clm.set("env-sound-interp", "sndscm.html#envsoundinterp");
  clm.set("make-mixer!", "sndclm.html#make-mixer!");
  clm.set("notch-sound", "sndscm.html#notchsound");
  clm.set("sine-summation", "sndclm.html#sine-summation");
  clm.set("env?", "sndclm.html#env?");
  clm.set("make-move-sound", "sndclm.html#make-move-sound");
  clm.set("notch?", "sndclm.html#notch?");
  clm.set("sine-summation?", "sndclm.html#sine-summation?");
  clm.set("z-transform", "sndscm.html#ztransform");
  clm.set("make-moving-average", "sndclm.html#make-moving-average");
  clm.set("nrev", "sndscm.html#nrev");
  clm.set("singer", "sndscm.html#singerdoc");
  clm.set("make-notch", "sndclm.html#make-notch");
  clm.set("offset-channel", "sndscm.html#offsetchannel");
  clm.set("zip-sound", "sndscm.html#zipsound");
  clm.set("make-one-pole", "sndclm.html#make-one-pole");
  clm.set("offset-sound", "sndscm.html#offsetsound");
  clm.set("zipper", "sndscm.html#zipper");
  clm.set("make-one-zero", "sndclm.html#make-one-zero");
  clm.set("one-pole", "sndclm.html#one-pole");
  clm.set("make-oscil", "sndclm.html#make-oscil");
  clm.set("one-pole?", "sndclm.html#one-pole?");
  clm.set("make-phase-vocoder", "sndclm.html#make-phase-vocoder");
  clm.set("one-zero", "sndclm.html#one-zero");
  clm.set("smoothing-filter", "sndscm.html#smoothingfilter");
  clm.set("make-pixmap", "sndscm.html#makepixmap");
  clm.set("one-zero?", "sndclm.html#one-zero?");
  clm.set("SMS synthesis", "sndscm.html#pins");
  clm.set("snap-mark-to-beat", "sndscm.html#snapmarktobeat");
  clm.set("make-polyshape", "sndclm.html#make-polyshape");
  clm.set("snap-mix-to-beat", "sndscm.html#snapmixtobeat");
}

/*
// hack functions to ensure help.cpp and cm.html are in sync and to
// generate the dictionary index.

void print_cm_entries ()
{
  std::cout << "\n(define cm.html '(";
  juce::File file=completeFile("/Users/hkt/Software/cm/res/doc/cm.html");
  if (! file.existsAsFile()) return;
  juce::FileInputStream stream (file);
  bool first=true;
  while (!stream.isExhausted())
  {
    juce::String target="id=\"";
    juce::String line=stream.readNextLine();
    if (line.contains("class=\"entry\""))
    {
      int start=line.indexOf(target);
      if (start>-1)
      {
        start+=target.length();
        int end=line.indexOf(start, "\"");
        if (end>-1)
        {
          juce::String tok=line.substring(start,end);
          if (!first) std::cout << " ";
          std::cout << tok ;
          first=false;
        }
      }
    }
  }
  std::cout << "))\n";
}

void print_help_entries ()
{
  std::cout << "\n(define help.cpp '(";
  juce::File file=completeFile("/Users/hkt/Software/cm/src/Help.cpp");
  if (! file.existsAsFile()) return;
  juce::FileInputStream stream (file);
  bool first=true;
  while (!stream.isExhausted())
  {
    juce::String target="\"cm.html#";
    juce::String line=stream.readNextLine();
    int start=line.indexOf(target);
    if (start>-1)
    {
      start+=target.length();
      int end=line.indexOf(start, "\"");
      if (end>-1)
      {
        juce::String tok=line.substring(start,end);
        if (!first) std::cout << " ";
        std::cout << tok;
        first=false;
      }
    }
  }
  std::cout << "))\n";
}

//  (loop for s in help.cpp if (not (member s cm.html)) collect s)
//  (loop for s in cm.html if (not (member s help.cpp)) collect s)

void print_cm_index ()
{
  std::cout << "\n<h2 id="index">Index</h2>\n";
  juce::File file=completeFile("/Users/hkt/Software/cm/res/doc/cm.html");
  if (! file.existsAsFile()) return;
  juce::FileInputStream stream (file);
  juce::StringArray entries;
  while (!stream.isExhausted())
  {
    juce::String target="id=\"";
    juce::String line=stream.readNextLine();
    juce::String entry;
    if (line.contains("class=\"entry\""))
    {
      int start=line.indexOf(target);
      if (start>-1)
      {
        start+=target.length();
        int end=line.indexOf(start, "\"");
        if (end>-1)
        {
          juce::String tok=line.substring(start,end);
          juce::String target2="class=\"funcname\">";
          int pos1=-1;
          for (int a=0; a<5; a++)
          {
            line=stream.readNextLine();
            pos1=line.indexOf(target2);
            if (pos1>-1) break;
          }
          if (pos1>-1)
          {
            pos1+=target2.length();
            int pos2=line.indexOf(pos1,"<");
            if (pos2>-1)
            {
              juce::String func=line.substring(pos1,pos2);
              entries.add(func + " " + tok);
            }
          }
        }
      }
    }
  }
  entries.sort(true);
  int size=entries.size();
  juce::juce_wchar alpha=0;
  int letter=0;
  for (int i=0; i<size; i++)
  {
    juce::String ent=entries[i];
    int sp=ent.indexOf(" ");
    juce::String name=ent.substring(0,sp);
    juce::String id=ent.substring(sp+1,ent.length());
    if (name[0]!=alpha)
    {
      if (i>0) std::cout << "\n</p>\n";
      std::cout << "\n<h4>" << juce::String::charToString(name[0]).toUTF8() << "</h4>\n<p>\n";
      letter=0;
    }
    if (letter>0) std::cout << ", ";        
    std::cout << "<a href=\"#" << id.toUTF8() << "\">" << name.toUTF8() << "</a>" ; 
    alpha=name[0];
    letter++;
  }
  std::cout << "\n</p>\n\n";
}
*/
