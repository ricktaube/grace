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

#ifndef CM_ENUMERATIONS_H
#define CM_ENUMERATIONS_H

#include "Libraries.h"

class SysInfo
{
public: 
  static const unsigned int MajorVersion = 3;
  static const unsigned int MinorVersion = 11;
  static const unsigned int PatchVersion = 0;
  static const unsigned int VersionMask  = 255; // eight bits
  /// Common Music release version number
  static const unsigned int CM_VERSION = (MajorVersion << 16) | (MinorVersion << 8) | (PatchVersion);

  /// Return string form of CM version 
  static const juce::String getCMVersion()
  {
    juce::String str= "Common Music " + getVersionString(CM_VERSION);
    return str;
  }

  /// Return string form of build configuration
  static const juce::String getBuildConfig()
  {
#if DEBUG
    return "Debug";
#elif SPEED
    return "Speed";
#elif NDEBUG
    return "Release";
#else
    return "Unknown";
#endif
  }

  static const juce::String getApplicationName()
  {
    return "Grace";
  }

  static const juce::String getCMLogo()
  {
    juce::String crlf = "\n";
    juce::String vers = getCMVersion();
    juce::String logo = juce::String();
    logo << " /\\\\\\" << crlf
	 << "---\\\\\\---------" << crlf
	 << "----\\\\\\--------" << crlf
	 << "----/\\\\\\-------" << " " << vers << crlf
	 << "---/--\\\\\\------" << crlf
	 << "--/----\\\\\\-----" << crlf
	 << " /      \\\\\\/";
    return logo;
  }

  static const juce::String getCopyright(const juce::String pers)
  {
    int year=juce::Time::getCurrentTime().getYear();
    return "(c) " + juce::String(year) + " " + pers;
  }

  static const juce::String getVersionString(int vers)
  {
    static const unsigned int a = (vers & (VersionMask << 16)) >> 16 ;
    static const unsigned int b = (vers & (VersionMask << 8)) >> 8; 
    static const unsigned int c = (vers & VersionMask) ; 
    return juce::String(a) + "." + juce::String(b) + "." + juce::String(c);
  }

  /*======================================================================*
    OS Info
    *======================================================================*/

  static bool isWindows()
  {
#ifdef JUCE_WIN32
    return true;
#else
    return false;
#endif
  }
  
  static bool isMac()
  {
#ifdef JUCE_MAC
    return true;
#else
    return false;
#endif
  }
  
  static bool isLinux()
  {
#ifdef JUCE_LINUX
    return true;
#else
    return false;
#endif
  }

  /*======================================================================*
    Target Info
    *======================================================================*/

};

/*
 * TextIds enumerate text syntaxes, the first three are supported by a
 * code syntax in the Editor
 */

typedef int TextID;

class TextIDs
{
public:
  static const TextID Empty=0;
  static const TextID Text=1;
  static const TextID Lisp=2;
  static const TextID Sal1=3;
  static const TextID Sal2=4;
  //  static const TextID Fomus=5;
  static const TextID Csound=6;
  static const TextID Xml=7;
  static const int fromString(const juce::String id)
  {
    if (id.equalsIgnoreCase("text")) return Text;
    if (id.equalsIgnoreCase("lisp")) return Lisp;
    if (id.equalsIgnoreCase("sal")) return Sal2;
    if (id.equalsIgnoreCase("sal2")) return Sal2;
    if (id.equalsIgnoreCase("sal1")) return Sal1;
    //    if (id.equalsIgnoreCase("fomus")) return Fomus;
    if (id.equalsIgnoreCase("csound")) return Csound;
    if (id.equalsIgnoreCase("xml")) return Xml;
    return Empty;
  }
  static const juce::String toString(const int id)
  {
    switch (id)
    {
    case Text:
      return "Text";
    case Lisp:
      return "Lisp";
    case Sal2:
      return "Sal";
    case Sal1:
      return "Sal1";
      //    case Fomus:
      //      return "Fomus";
    case Csound:
      return "Csound";
    case Xml:
      return "Xml";
    default:
      return "Empty";
    }
  }
  static const int fromFileType(const juce::String ext)
  {
    if (juce::String(".lisp.lsp.scm.cm.clm.cmn.ins").contains(ext))
      return Lisp;
    else if (ext.equalsIgnoreCase(".sal")) 
      return Sal2;
    else if (ext.equalsIgnoreCase(".sal2"))
      return Sal2;
    else if (ext.equalsIgnoreCase(".sal1")) 
      return Sal1;
    //    else if (ext.equalsIgnoreCase(".fms")) 
    //      return Fomus;
    else if (ext.equalsIgnoreCase(".sco"))
      return Csound;
    else if (ext.equalsIgnoreCase(".xml"))
      return Xml;
    else if (juce::String(".text.txt").contains(ext))
      return Text;
    return Empty;
  }

  static const bool canExecute(const int ttyp)
  {
    return ((ttyp==Lisp) || (ttyp==Sal2) || (ttyp==Sal1));
  }

  static const bool canExpand(const int ttyp)
  {
    return ((ttyp==Lisp) || (ttyp==Sal2) || (ttyp==Sal1));
  }
};

class ColorThemeIDs
{
public:
  static const int Empty=-1;
  static const int Plaintext=0;
  static const int Keyword1=1;
  static const int Keyword2=2;
  static const int Keyword3=3;
  static const int Literal1=4;
  static const int Literal2=5;
  static const int Literal3=6;
  static const int Comment=7;
  static const int Stryng=8;
  static const int Error=9;
  static const int MAXTOKENCOLORS=10; // from Plaintext to Error
  static const int Warning=10;
  static const int Output=11;
  static const int Values=12;
  static const int Cursor=13;
  static const int Region=14;
  static const int Background=15;
  static const int Name=16;
  
  static const juce::String toString(int id)
  {
    switch (id)
    {
    case Name: return "name";
    case Background: return "background";
    case Cursor: return "cursor";
    case Region: return "region";
    case Values: return "values";
    case Error: return "error";
    case Warning: return "warning";
    case Output: return "output";
    case Comment: return "comment";
    case Stryng: return "string";
    case Plaintext: return "plaintext";
    case Keyword1: return "keyword1";  // scheme sharp #foo
    case Keyword2: return "keyword2";  // scheme keyword :foo
    case Keyword3: return "keyword3";  // sal keyword foo
    case Literal1: return "literal1";  // special form/reserved
    case Literal2: return "literal2";  // sal classname
    case Literal3: return "literal3";  // sal command
    default: return "Empty";   
    }
  }

  static const int fromString(const juce::String s)
  {
    if (s.equalsIgnoreCase("name")) return Name;
    if (s.equalsIgnoreCase("background")) return Background;
    if (s.equalsIgnoreCase("cursor")) return Cursor;
    if (s.equalsIgnoreCase("region")) return Region;
    if (s.equalsIgnoreCase("values")) return Values;
    if (s.equalsIgnoreCase("error")) return Error;
    if (s.equalsIgnoreCase("warning")) return Warning;
    if (s.equalsIgnoreCase("output")) return Output;
    if (s.equalsIgnoreCase("comment")) return Comment;
    if (s.equalsIgnoreCase("string")) return Stryng;
    if (s.equalsIgnoreCase("plaintext")) return Plaintext;
    if (s.equalsIgnoreCase("keyword1")) return Keyword1;
    if (s.equalsIgnoreCase("keyword2")) return Keyword2;
    if (s.equalsIgnoreCase("keyword3")) return Keyword3;
    if (s.equalsIgnoreCase("literal1")) return Literal1;
    if (s.equalsIgnoreCase("literal2")) return Literal2;
    if (s.equalsIgnoreCase("literal3")) return Literal3;
    return Empty;
  }

  static const juce::String toHtmlColorString(const juce::Colour color)
  {
    switch (color.getARGB())  // hacked for standard grace colors
    {
    case 0xff000000 : return "black";
    case 0xffffffff : return "white";
    case 0xff808080 : return "grey";
    case 0xff0000ff : return "blue";
    case 0xff008000 : return "green";
    case 0xffff0000 : return "red";
    case 0xffffff00 : return "yellow";
    case 0xff228b22 : return "forestgreen";
    case 0xffb22222 : return "firebrick";
    case 0xffff8c00 : return "darkorange";
    case 0xffffa07a : return "lightsalmon";
    case 0xff5f9ea0 : return "cadetblue";
    case 0xffbc8f8f : return "rosybrown";
    case 0xffda70d6 : return "orchid";
    default: break;
    }
    juce::String h="#";
    int i;
    if ((i=color.getRed())<16) h<< "0";
    h << juce::String::toHexString(i);
    if ((i=color.getGreen())<16) h<< "0";
    h << juce::String::toHexString(i);
    if ((i=color.getBlue())<16) h<< "0";
    h << juce::String::toHexString(i);
    return h;
  }

  static const juce::Colour fromHtmlColorString(juce::String html, const juce::Colour defc=juce::Colours::black)
  {
    if (html.isQuotedString())
      html=html.unquoted();
    if (html.isEmpty())
      return defc;
    if (html[0]=='#' )
    {
      juce::String num=html.substring(1);
      if ((num.length()==6) && num.containsOnly("0123456789abcdefABCDEF"))
      {
        juce::uint32 coln=0xff000000+num.getHexValue32();
        return juce::Colour(coln);
      }
      else return defc;
    }
    else return juce::Colours::findColourForName(html, defc);
  }

  static const juce::String getColorThemeName (juce::XmlElement* theme)
  {
    return theme->getStringAttribute("name");
  }

  static const juce::Colour getColorThemeColor (juce::XmlElement* theme, const int id, const juce::Colour defc=juce::Colours::black)
  {
    return fromHtmlColorString(theme->getStringAttribute(toString(id)),defc);
  }
  static const juce::Colour getWindowBackgroundColor ()
  {
    //    return juce::Colour(0xffe5e5e5);
    //    return juce::Colours::darkgrey;
    return juce::Colour(118, 118, 118);
  }
};

/** HiliteIDs enumerate logical categories for syntax highlighting **/
  
typedef int HiliteID;
  
class HiliteIDs
{
public:
  static const int NUMHILITES=8;
  static const HiliteID None=0;
  static const HiliteID String=1;
  static const HiliteID Comment=2;
  static const HiliteID Hilite4=3;  
  static const HiliteID Hilite5=4;  
  static const HiliteID Hilite6=5;  
  static const HiliteID Hilite7=6;  
  static const HiliteID Hilite8=7;  
};

/* 
 * ExportIDs describe ways to decode data into text. Upper nibble
 * holds a TextID, lower nibble holds the format of the output
 * (e.g. lists of data, send expressions, score statements, etc)
 */

typedef int ExportID;

class ExportIDs
{
private:
  static const int SHIFT=4;
  static const int UMASK=0xf0;
  static const int LMASK=0x0f;
public:
  
  static ExportID toExportID(TextID textid, int format)
  {
    return (ExportID)(((textid & LMASK) << SHIFT) + (format & LMASK));
  }

  static const TextID getTextID(ExportID f)
  {
    return (TextID)((f & UMASK) >> SHIFT);
  }

  static const int getFormat(ExportID f)
  {
    return (f & LMASK);
  }

  static const int Empty = 0;
  static const int Data = 1;  // event data exported as lists
  static const int Send = 2;  // event data exported as send exprs
  static const int Score = 3; // score file format

  // export destinaions
  static const int ToFile = 1;
  static const int ToConsole = 2;
  static const int ToEditor = 3;
  static const int ToClipboard= 4;

  static const ExportID SalData = (TextIDs::Sal2 << SHIFT) + Data;
  static const ExportID SalSend = (TextIDs::Sal2 << SHIFT) + Send;
  static const ExportID LispData = (TextIDs::Lisp << SHIFT) + Data;
  static const ExportID LispSend = (TextIDs::Lisp << SHIFT) + Send;
  static const ExportID CsoundScore = (TextIDs::Csound << SHIFT) + Score;
  static const ExportID XmlData = (TextIDs::Xml << SHIFT) + Data ;
};


struct MidiOps
{
  static const int Off = 0x8;
  static const int On = 0x9;
  static const int Touch = 0xA;
  static const int Ctrl = 0xB;
  static const int Prog = 0xC;
  static const int Press = 0xD;
  static const int Bend = 0xE;
  static const int Meta = 0xF;

  static const juce::String toString(int val)
  {
    switch(val)
    {
    case Off: return "off";
    case On: return "on";
    case Touch: return "touch";
    case Ctrl: return "ctrl";
    case Prog: return "prog";
    case Press: return "press";
    case Bend: return "bend";
    case Meta: return "meta";
    default: return "";
    }
  }

  static const juce::String toPrettyString(int val)
  {
    switch(val)
    {
    case Off: return "Note Off";
    case On: return "Note On";
    case Touch: return "Aftertouch";
    case Ctrl: return "Control Change";
    case Prog: return "Program Change";
    case Press: return "Channel Pressure";
    case Bend: return "Pitch Bend";
    case Meta: return "Meta Message";
    default: return "";
    }
  }
};

struct ScoreTypes
{
  static const int Any = -1;
  static const int Empty = 0;
  static const int Midi = 1;
  static const int SndLib = 2;
  static const int Csound = 3;
  static const int Fomus = 4;
};

/**Encodes window functionality. The first three are app windows.*/
struct WindowTypes
{
  static const int Empty = 0;
  static const int Console = 1;  //singleton
  static const int CodeEditor = 2;
  static const int PlotWindow = 3;
  static const int StateWindow = 4;
  static const int AudioFilePlayer = 5;
  static const int MidiFilePlayer = 6;
  static const int InstrumentBrowser = 7; //singleton
  static const int PlottingControls = 8; //singleton
  static const int FindAndReplace = 9; //singleton

  static const bool isWindowType(juce::TopLevelWindow* w, int typ)
  {
    if (w)
      return (w->getProperties()[juce::Identifier("WindowType")] == juce::var(typ));
    else 
      return false;
  }

  static const void setWindowType(juce::TopLevelWindow* w, int typ)
  {
    w->getProperties().set(juce::Identifier("WindowType"), juce::var(typ));
  }

  static int getHighestUntitledWindowOfType(int type)
  {
    int num = 0;
    for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
    {
      juce::TopLevelWindow* w = juce::TopLevelWindow::getTopLevelWindow(i);
      if (isWindowType(w, type))
      {        
        if (w->getName() == "Untitled")
          num = 1;
        else if (w->getName().startsWith("Untitled "))
        {
          juce::String text = w->getName().substring(9).trim();
          int j = 0;
          while (j < text.length() && juce::CharacterFunctions::isDigit(text[j])) j++;
          if (j > 0)
            num = text.substring(0, j).getIntValue();
          else
            num = 1;
        }
      }
    }
    return num;
  }
};

class Prefs
{
public:
  static const int Empty = 0;
  static const int ConsoleFontSize = 1;
  static const int EmacsMode = 2;
  static const int EditorFontSize = 3;
  static const int EditorFont = 4;
  static const int MidiOutDevice = 5;
  static const int MidiInDevice = 6;
};

/**CommandIDs enumerate all menu commands and application
   messages. The upper 16 bits holds the command id, the lowest byte
   contains command data.

   76543210 76543210 76543210 76543210
   cccccccc cccccccc 00000000 dddddddd*/


struct CommandIDs
{


  ///Returns the command from the CommandID
  static const juce::CommandID getCommand(juce::CommandID id)
  {
    return (id & 0xffffff00);
  }

  ///Returns the command data from the CommandID.
  static const int getCommandData(juce::CommandID id)
  {
    return (id & 0x0000ff) ;
  }

  /**Returns true if commandId should be active for the current
     top-level window represented by windowType.*/
  static const bool isActiveForWindow(int commandId, int windowType)
  {
    if (windowType == 0)
    {
      std::cout << "warning: isActiveForWindow: windowType==0\n";
      return true;
    }
    int allowed = ((commandId & 0x0000ff00) >> 8);
    if (windowType == 0)
    {
      std::cout << "warning: isActiveForWindow: allowed windows==0\n";
      return true;
    }
    return (allowed & windowType) != 0 ? true : false;
  }

  //
  // Command IDs
  //

  /**Command categories (usually corresponding to menus). Each
     category allows max 100 commands.*/
  static const int File = 100;
  static const int Edit = 200;
  static const int View = 300;
  static const int Audio = 400;
  static const int MidiOut = 500;
  static const int MidiIn = 600;
  static const int Csound = 700;
  static const int SndLib = 800;
  static const int Osc = 900;
  static const int Fomus = 1000;
  static const int Eval = 1100;
  static const int Window = 1200;
  static const int Help = 1300;
  // Messages
  static const int Console = 2000;
  static const int Editor = 2100;
  static const int Plotter = 2200;
  static const int Scheduler = 2300;
  static const int Scheme = 2400;
  static const int Cells = 2500;

  static const juce::String toString(juce::CommandID id, bool data=false)
  {
    juce::String str;
    str << id;
    return str;
  }

#define COMID(a) ((a) << 16)

  static const juce::CommandID Empty = 0;
  static const juce::CommandID FileNew             = COMID(File + 1);
  static const juce::CommandID FileNewPlotter      = COMID(File + 2);
  static const juce::CommandID FileOpen            = COMID(File + 3);
  static const juce::CommandID FileOpenRecent      = COMID(File + 4);
  static const juce::CommandID FileClearOpenRecent = COMID(File + 5);
  static const juce::CommandID FileClose           = COMID(File + 6);
  static const juce::CommandID FileSave            = COMID(File + 7);
  static const juce::CommandID FileSaveAs          = COMID(File + 8);
  static const juce::CommandID FileSaveVersion     = COMID(File + 9);
  static const juce::CommandID FilePlayAudioFile   = COMID(File + 10);
  static const juce::CommandID FilePlayMidiFile    = COMID(File + 11);
  static const juce::CommandID FileLoad            = COMID(File + 12);
  static const juce::CommandID FileLoadRecent      = COMID(File + 13);
  static const juce::CommandID FileClearLoadRecent = COMID(File + 14);
  static const juce::CommandID FileSetInit         = COMID(File + 15);
  static const juce::CommandID FileClearInit       = COMID(File + 16);
  static const juce::CommandID FileOpenInit        = COMID(File + 17);
  static const juce::CommandID FileShowDirectory   = COMID(File + 18);
  static const juce::CommandID FileSetDirectory    = COMID(File + 19);
  static const juce::CommandID FileQuit            = COMID(File + 20);

  //----//
  //Edit//
  //----//

  static const juce::CommandID EditUndo           = COMID(Edit + 1);
  static const juce::CommandID EditRedo           = COMID(Edit + 2);
  static const juce::CommandID EditCut            = COMID(Edit + 3);
  static const juce::CommandID EditCopy           = COMID(Edit + 4);
  static const juce::CommandID EditPaste          = COMID(Edit + 5);  
  static const juce::CommandID EditDelete         = COMID(Edit + 6);
  static const juce::CommandID EditSelectAll      = COMID(Edit + 7);
  static const juce::CommandID EditDeselectAll    = COMID(Edit + 8);
  static const juce::CommandID EditFind           = COMID(Edit + 9);
  static const juce::CommandID EditSyntax         = COMID(Edit + 10);
  static const juce::CommandID EditDefaultSyntax  = COMID(Edit + 11);
  static const juce::CommandID EditIndent         = COMID(Edit + 12);
  static const juce::CommandID EditParensMatching = COMID(Edit + 13);
  static const juce::CommandID EditEmacsMode      = COMID(Edit + 14);

  //----//
  //View//
  //----//

  static const juce::CommandID ViewColorTheme         = COMID(View + 1);
  static const juce::CommandID ViewDefaultColorTheme  = COMID(View + 2); 
  static const juce::CommandID ViewFontSize           = COMID(View + 3);
  static const juce::CommandID ViewDefaultFontSize    = COMID(View + 4);
  static const juce::CommandID ViewFontBigger         = COMID(View + 5);
  static const juce::CommandID ViewFontSmaller        = COMID(View + 6);
  static const juce::CommandID ViewClearConsole       = COMID(View + 7); 
  static const juce::CommandID ViewConsoleBeepOnError = COMID(View + 8); 
  static const juce::CommandID ViewLineNumbersVisible = COMID(View + 9);
  static const juce::CommandID ViewReadCustomComment  = COMID(View + 10);
  static const juce::CommandID ViewWriteCustomComment = COMID(View + 11);

  //-----//
  //Audio//
  //-----//

  static const juce::CommandID AudioFilePlayer    = COMID(Audio + 1);
  static const juce::CommandID MidiFilePlayer     = COMID(Audio + 2);
  static const juce::CommandID AudioSettings      = COMID(Audio + 3);
  static const juce::CommandID AudioSynthSettings = COMID(Audio + 4);
  static const juce::CommandID MidiDeviceSettings = COMID(Audio + 5);

  static const juce::CommandID MidiOutOpen         = COMID(MidiOut + 1);
  static const juce::CommandID MidiOutTest         = COMID(MidiOut + 2);
  static const juce::CommandID MidiOutHush         = COMID(MidiOut + 3);
  static const juce::CommandID MidiOutAllNotesOff  = COMID(MidiOut + 4);
  static const juce::CommandID MidiOutTuning       = COMID(MidiOut + 5);
  static const juce::CommandID MidiOutDrumTrack    = COMID(MidiOut + 6);
  static const juce::CommandID MidiOutPitchBend    = COMID(MidiOut + 7);
  static const juce::CommandID MidiOutInstruments  = COMID(MidiOut + 8);
  static const juce::CommandID MidiOutFileSettings = COMID(MidiOut + 9);
  static const juce::CommandID MidiOutClosed       = COMID(MidiOut + 10);
  static const juce::CommandID MidiOutSetFile      = COMID(MidiOut + 11);  
  static const juce::CommandID MidiInOpen          = COMID(MidiIn + 1);
  static const juce::CommandID MidiInTrace         = COMID(MidiIn + 2);
  static const juce::CommandID MidiInChannelFilter = COMID(MidiIn + 3);
  static const juce::CommandID MidiInOpcodeFilter  = COMID(MidiIn + 4);
  static const juce::CommandID MidiInClosed        = COMID(MidiIn + 5);

  static const juce::CommandID CsoundPrefWriteAfter = COMID(Csound + 1);
  static const juce::CommandID CsoundPrefPlayAfter  = COMID(Csound + 2);
  static const juce::CommandID CsoundExportScore    = COMID(Csound + 3);
  static const juce::CommandID CsoundClearScore     = COMID(Csound + 4);
  static const juce::CommandID CsoundOpenSettings   = COMID(Csound + 5);

  static const juce::CommandID OscOpen         = COMID(Osc + 1);
  static const juce::CommandID OscTraceInput   = COMID(Osc + 2);
  static const juce::CommandID OscTestOutput   = COMID(Osc + 3);
  static const juce::CommandID OscShowStatus   = COMID(Osc + 4);
  static const juce::CommandID OscSetHook      = COMID(Osc + 5);
  static const juce::CommandID OscClearHook    = COMID(Osc + 6);
  static const juce::CommandID OscInputClosed  = COMID(Osc + 7);
  static const juce::CommandID OscOutputClosed = COMID(Osc + 8);  

  static const juce::CommandID SndLibSrate       = COMID(SndLib + 1);
  static const juce::CommandID SndLibChannels    = COMID(SndLib + 2);
  static const juce::CommandID SndLibAudioFormat = COMID(SndLib + 3);
  static const juce::CommandID SndLibAutoPlay    = COMID(SndLib + 4);
  static const juce::CommandID SndLibInsDialog   = COMID(SndLib + 5);
  static const juce::CommandID SndLibAutoLoad    = COMID(SndLib + 6);

  //----//
  //Eval//
  //----//

  static const juce::CommandID EvalExecute   = COMID(Eval + 1);
  static const juce::CommandID EvalExpand    = COMID(Eval + 2);
  static const juce::CommandID EvalAbort     = COMID(Eval + 3);
  static const juce::CommandID EvalBacktrace = COMID(Eval + 4);

  //------//
  //Window//
  //------//

  static const juce::CommandID WindowSelect = COMID(Window + 1);
  static const juce::CommandID WindowPlottingControls   = COMID(Window + 2); 

  //----//
  //Help//
  //----//

  static const juce::CommandID HelpManual         = COMID(Help + 1);
  static const juce::CommandID HelpSalExample     = COMID(Help + 2);
  static const juce::CommandID HelpSalTutorial    = COMID(Help + 3);
  static const juce::CommandID HelpSchemeExample  = COMID(Help + 4);
  static const juce::CommandID HelpSchemeTutorial = COMID(Help + 5);
  static const juce::CommandID HelpSchemeExtra    = COMID(Help + 6);
  static const juce::CommandID HelpWebSite        = COMID(Help + 7);
  static const juce::CommandID HelpExport         = COMID(Help + 8);
  static const juce::CommandID HelpFomus          = COMID(Help + 9);
  static const juce::CommandID HelpDocumentationLookup  = COMID(Help + 10);

  //--------//
  //MESSAGES//
  //--------//

  static const juce::CommandID ConsoleBeepOnError  = COMID(Console + 1);
  static const juce::CommandID ConsoleClearConsole = COMID(Console + 2);
  static const juce::CommandID ConsolePrintOutput  = COMID(Console + 3);
  static const juce::CommandID ConsolePrintValues  = COMID(Console + 4);
  static const juce::CommandID ConsolePrintWarning = COMID(Console + 5);
  static const juce::CommandID ConsolePrintError   = COMID(Console + 6);
  static const juce::CommandID ConsolePrintPrompt  = COMID(Console + 7);

  // Scheduler MESSAGES
  static const juce::CommandID SchedulerStop            = COMID(Scheduler + 1);
  static const juce::CommandID SchedulerPause           = COMID(Scheduler + 2);
  static const juce::CommandID SchedulerError           = COMID(Scheduler + 3);
  static const juce::CommandID SchedulerScoreMode       = COMID(Scheduler + 4);
  static const juce::CommandID SchedulerScoreComplete   = COMID(Scheduler + 5);
  static const juce::CommandID SchedulerInterruptScheme = COMID(Scheduler + 6);

  // Plotter commands
  static const juce::CommandID PlotterOpenMidiFile  = COMID(Plotter + 1);
  static const juce::CommandID PlotterAddXmlPoints  = COMID(Plotter + 2);
  static const juce::CommandID PlotterShiftPoints   = COMID(Plotter + 3);
  static const juce::CommandID PlotterRescalePoints = COMID(Plotter + 4);

  // State Window commands
  static const juce::CommandID StateWindowSetCells           = COMID(Cells + 1);
  static const juce::CommandID StateWindowSetCell            = COMID(Cells + 2);
  static const juce::CommandID StateWindowSetStatesAndColors = COMID(Cells + 3);
  static const juce::CommandID StateWindowSetRowsAndColumns  = COMID(Cells + 4);
  static const juce::CommandID StateWindowSetCellSize        = COMID(Cells + 5);

#undef COMID
};

 
struct Flags 
{  
  static const int get(int flags, int f)
  {
    return (flags & f);
  }
  static const bool test(int flags, int f)
  {
    return (get(flags, f) == f);
  }
  static const void setOn(int& flags, int f)
  {
    flags |= f; 
  }
  static const void setOff(int& flags, int f) 
  {
    flags &= ~f; 
  }
  static const bool toggle(int& flags, int f)
  {
    flags ^= f; 
    return test(flags, f);
  }
};

class EditFlags
{
public:
  static const int Empty = 0;
  static const int NeedsSave = 1 << 0;
  static const int ReadOnly  = 1 << 1;
  static const int HiliteOff = 1 << 2;
  static const int MatchingOff = 1 << 3;
  static const int EmacsMode = 1 << 4;
  static const int Coloring  = 1 << 5;
};

class SrateIDs
{
public:
  static const int NumSrates = 4;
  static const int Srate0 = 22050;  
  static const int Srate1 = 44100;  
  static const int Srate2 = 48000;  
  static const int Srate3 = 96000;  
  static const int toSrate(int id)
  {
    switch (id)
    {
    case 0: return Srate0;
    case 1: return Srate1;
    case 2: return Srate2;
    case 3: return Srate3;
    default: return Srate1;
    }
  } 
  static const juce::String toString(int id)
  {
    return juce::String(toSrate(id));
  }
};

class ChannelIDs
{
public:
  static const int NumChannels = 4;
  static const int Mono = 0;  
  static const int Stereo = 1;  
  static const int Quad= 2;  
  static const int Eight = 3;  
  static const int toChannels(int id)
  {
    switch (id)
    {
    case Mono: return 1;
    case Stereo: return 2;
    case Quad: return 4;
    case Eight: return 8;
    default: return 1;
    }
  } 
  static const juce::String toString(int id)
  {
    return juce::String(toChannels(id));
  }
};

class AudioFormatIDs
{
public:
  static const int NumAudioFormats = 3;
  static const int WAV = 0;
  static const int AIFF = 1;
  static const int SND = 2;
  static const juce::String toString(int id)
  {
    switch (id)
    {
    case WAV: return "WAV";
    case AIFF: return "AIFF";
    case SND: return "SND";
    default: 
      return "No Audio Format";
    }
  }
};

class MidiValues
{
public:
  static const int Empty = 0;
  static const int MidiValueTime = 1;
  static const int MidiValueDelta = 2;
  static const int MidiValueOp = 3;
  static const int MidiValueChannel = 4;
  static const int MidiValueRhythm = 5;
  static const int MidiValueDuration = 6;
  static const int MidiValueKeyNumber = 7;
  static const int MidiValueAmplitude = 8;
  static const int MidiValueVelocity = 9;
  static const int MidiValueTouch = 10;
  static const int MidiValueControlNumber = 11;
  static const int MidiValueControlValue = 12;
  static const int MidiValueProgram = 13;
  static const int MidiValuePressure = 14;
  static const int MidiValueBend = 15;
  // meta message values
  static const int MidiValueSeqNum = 16;
  static const int MidiValueText = 17;
  static const int MidiValueChanPrefix = 18;
  static const int MidiValueTempo = 19;
  static const int MidiValueTimeSig = 20;
  static const int MidiValueKeySig = 21;

  static const bool isOpForValue(int value, int op)
  {
    // return true if the message's opcode is valid for the midivalue
    // if 8<op<16 its a channel message (no offs) else its a meta message
    switch (value)
    {
      // all messages
    case MidiValueTime: 
    case MidiValueDelta:
    case MidiValueOp:            return true;
      // channel events
    case MidiValueChannel:       return ((op>0x08) && (op<0x0F)); // dont recognize offs
    case MidiValueRhythm:
    case MidiValueDuration:
    case MidiValueKeyNumber:
    case MidiValueAmplitude:
    case MidiValueVelocity:      return (op==0x9);
    case MidiValueTouch:         return (op==0xA);
    case MidiValueControlNumber:
    case MidiValueControlValue:  return (op==0xB);
    case MidiValueProgram:       return (op==0xC);
    case MidiValuePressure:      return (op==0xD);
    case MidiValueBend:          return (op==0xE);
      // meta events
    case MidiValueSeqNum:        return (op==0x00);
    case MidiValueText:          return ((op>0x00) && (op<0x08));
    case MidiValueChanPrefix:    return (op==0x20);
    case MidiValueTempo:         return (op==0x51);
    case MidiValueTimeSig:       return (op==0x58);
    case MidiValueKeySig:        return (op==0x59);
    default:                     return false;
    }      
  }
};

class TriggerIDs
{
public:
  static const int Empty=0;
  static const int ButtonTrigger=1;
  static const int CounterTrigger=2;
  static const int SliderTrigger=3;
  static const int MidiKeyboardTrigger=4;
  static const int MidiInTrigger=5;
  static const int NumTriggers=5;
  static const juce::String toString(int id)
  {
    switch (id)
    {
    case ButtonTrigger:
      return "button";
    case CounterTrigger:
      return "counter";
    case SliderTrigger:
      return "slider";
    case MidiKeyboardTrigger:
      return "keyboard";
    case MidiInTrigger:
      return "midiin";
    default:
      return juce::String();
    }
  }
  static const juce::String toPrettyString(int id)
  {
    switch (id)
    {
    case ButtonTrigger:
      return  "Button";
    case CounterTrigger:
      return  "Counter";
    case SliderTrigger:
      return "Slider";
    case MidiKeyboardTrigger:
      return  "Keyboard";
    case MidiInTrigger:
      return "Midi In";
    default:
      return "Empty";
    }  
  }
  static int fromString(const juce::String str)
  {
    if (str.equalsIgnoreCase("button"))
      return ButtonTrigger;
    if (str.equalsIgnoreCase("counter"))
      return CounterTrigger;
    if (str.equalsIgnoreCase("slider"))
      return SliderTrigger;
    if (str.equalsIgnoreCase("keyboard"))
      return MidiKeyboardTrigger;
    if (str.removeCharacters(" ").equalsIgnoreCase("midiin"))
      return MidiInTrigger;
    else
      return Empty;
  }
};

class ScanIDs
{
public:

  // Syntax Char Types
  enum 
    {
      SYN_WHITE = 1,
      SYN_WORD,
      SYN_SYMBOL,
      SYN_COMMENT, 
      SYN_PREFIX,
      SYN_STRING,
      SYN_OPEN,
      SYN_CLOSE,
      SYN_PUNCT,
      SYN_ESCAPE
    };

  // Scan Types
  static const int Empty=0;
  static const int MoveExpressions=1;
  static const int MoveTokens=2;
  static const int MoveWhiteAndComments=3;

  // Scan Results
  enum
    {
      SCAN_MISMATCH = -3,   // Shouldn't happen
      SCAN_UNMATCHED,
      SCAN_UNLEVEL,
      SCAN_EMPTY,    // must be zero
      SCAN_TOKEN,
      SCAN_STRING,
      SCAN_LIST,
      SCAN_COMMENT,
      SCAN_OPEN,
      SCAN_CLOSE,
      SCAN_PUNCT
    };
  static const juce::String scanResultToString(int res)
  {
    switch (res)
    {
    case SCAN_MISMATCH: return "mismatch";
    case SCAN_UNMATCHED: return "unmatched";
    case SCAN_UNLEVEL: return "unlevel";
    case SCAN_EMPTY: return "empty";
    case SCAN_TOKEN: return "token";
    case SCAN_STRING: return "string";
    case SCAN_LIST: return "list";
    case SCAN_COMMENT: return "comment";
    case SCAN_OPEN: return "open";
    case SCAN_CLOSE: return "close";
    case SCAN_PUNCT: return "puctuation";
    default: return "unknown";
    }
  }
};

class SalIDs
{
public:
  enum SalTypeData // Type data (encoded in lower 8 bits)
    {
      // generated by (sal-enums )
      SalBlockOpen = 0x10,
      SalBlockClose = 0x11
    };

  enum SalTypes 
    {
      // generated by (sal-enums )
      SalUntyped = 0x0,
      SAL_TOKEN_BEG = 0x0,
      SAL_DELIM_BEG = 0x0,
      SalComma = 0x100,
      SalLParen = 0x200,
      SalRParen = 0x300,
      SalLCurly = 0x400,
      SalRCurly = 0x500,
      SalLBrace = 0x600,
      SalRBrace = 0x700,
      SAL_DELIM_END = 0x800,
      SAL_DATA_BEG = 0x800,
      SalString = 0x900,
      SAL_NUMBER_BEG = 0xa00,
      SalInteger = 0xb00,
      SalRatio = 0xc00,
      SalFloat = 0xd00,
      SAL_NUMBER_END = 0xe00,
      SalKeyword = 0xf00,
      SAL_BOOL_BEG = 0x1000,
      SAL_HASH_BEG = 0x1000,
      SalTrue = 0x1100,
      SalFalse = 0x1200,
      SAL_BOOL_END = 0x1300,
      SalQMark = 0x1400,
      SalUnquote = 0x1500,
      SalSplice = 0x1600,
      SAL_HASH_END = 0x1700,
      SAL_DATA_END = 0x1700,
      SalKeyparam = 0x1800,
      SalClass = 0x1900,
      SAL_OP_BEG = 0x1a00,
      SalPlus = 0x1b05,
      SalMinus = 0x1c05,
      SalTimes = 0x1d06,
      SalDivide = 0x1e06,
      SalMod = 0x1f05,
      SalExpt = 0x2007,
      SalAnd = 0x2102,
      SalOr = 0x2201,
      SalNot = 0x2303,
      SAL_RELATION_BEG = 0x2400,
      SalLess = 0x2504,
      SalGreater = 0x2604,
      SalNotEqual = 0x2704,
      SalGeneralEqual = 0x2804,
      SAL_ASSIGNMENT_BEG = 0x2900,
      SalEqual = 0x2a04,
      SalLessEqual = 0x2b04,
      SalGreaterEqual = 0x2c04,
      SAL_RELATION_END = 0x2d00,
      SAL_OP_END = 0x2d00,
      SalInc = 0x2e00,
      SalMul = 0x2f00,
      SalCol = 0x3000,
      SalPre = 0x3100,
      SalApp = 0x3200,
      SAL_ASSIGNMENT_END = 0x3300,
      SAL_LITERAL_BEG = 0x3300,
      SAL_STATEMENT_BEG = 0x3300,
      SAL_COMMAND_BEG = 0x3300,
      SalBegin = 0x3410,
      SalChdir = 0x3500,
      SalDefine = 0x3600,
      SalExec = 0x3700,
      SalIf = 0x3800,
      SalLoad = 0x3900,
      SalLoop = 0x3a10,
      SalPlot = 0x3b00,
      SalPrint = 0x3c00,
      SalRun = 0x3d10,
      SalSend = 0x3e00,
      SalSet = 0x3f00,
      SalSoundFile = 0x4010,
      SalFomusFile = 0x4110,
      SalSprout = 0x4200,
      Sal2Variable = 0x4300,
      Sal2Function = 0x4410,
      Sal2Process = 0x4510,
      Sal2If = 0x4610,
      Sal2File = 0x4710,
      Sal2Wait = 0x4800,
      SAL_COMMAND_END = 0x4900,
      SAL_CONSTITUENT_BEG = 0x4900,
      SalElse = 0x4a00,
      SalEnd = 0x4b11,
      SalReturn = 0x4c00,
      SalThen = 0x4d00,
      SalUnless = 0x4e00,
      SalUntil = 0x4f00,
      SalWait = 0x5000,
      SalWhen = 0x5100,
      SalWhile = 0x5200,
      SalWith = 0x5300,
      SalOptKey = 0x5400,
      SAL_CONSTITUENT_END = 0x5500,
      SAL_STATEMENT_END = 0x5500,
      SAL_CLAUSAL_BEG = 0x5500,
      SalAbove = 0x5600,
      SalBelow = 0x5700,
      SalBy = 0x5800,
      SalDownto = 0x5900,
      SalFinally = 0x5a00,
      SalFor = 0x5b00,
      SalFrom = 0x5c00,
      SalIn = 0x5d00,
      SalOver = 0x5e00,
      SalRepeat = 0x5f00,
      SalTo = 0x6000,
      SAL_CLAUSAL_END = 0x6100,
      SAL_LITERAL_END = 0x6100,
      SAL_DEFINE_BEG = 0x6100,
      SalFunction = 0x6200,
      SalProcess = 0x6300,
      SalVariable = 0x6400,
      SAL_DEFINE_END = 0x6500,
      SAL_IDENTIFIER_BEG = 0x6500,
      SalIdentifier = 0x6600,
      SalSlotRef = 0x6700,
      SAL_IDENTIFIER_END = 0x6800,
      SAL_TOKEN_END = 0x6800,
      SAL_RULE_BEG = 0x6800,
      SalNumberRule = 0x6900,
      SalBoolRule = 0x6a00,
      SalAtomRule = 0x6b00,
      SalListRule = 0x6c00,
      SalEltRule = 0x6d00,
      SalArefRule = 0x6e00,
      SalIfExprRule = 0x6f00,
      SalUnquoteRule = 0x7000,
      SalFuncallRule = 0x7100,
      SalFunargsRule = 0x7200,
      SalPargsRule = 0x7300,
      SalKargsRule = 0x7400,
      SalOpRule = 0x7500,
      SalMexprRule = 0x7600,
      SalTermRule = 0x7700,
      SalSexprRule = 0x7800,
      SalBindingsRule = 0x7900,
      SalBindRule = 0x7a00,
      SalAssignmentRule = 0x7b00,
      SalAssignRule = 0x7c00,
      SalAssignerRule = 0x7d00,
      SalSetRule = 0x7e00,
      SalFunctionReturnRule = 0x7f00,
      SalProcessWaitRule = 0x8000,
      SalBlockRule = 0x8100,
      SalConditionalRule = 0x8200,
      SalLoopStatementRule = 0x8300,
      SalRunStatementRule = 0x8400,
      SalSteppingRule = 0x8500,
      SalTerminationRule = 0x8600,
      SalPrintStatementRule = 0x8700,
      SalExecStatementRule = 0x8800,
      SalSendStatementRule = 0x8900,
      SalSoundFileStatementRule = 0x8a00,
      SalFomusFileStatementRule = 0x8b00,
      SalSproutStatementRule = 0x8c00,
      SalLoadStatementRule = 0x8d00,
      SalChdirStatementRule = 0x8e00,
      SalPlotStatementRule = 0x8f00,
      SalDefineStatementRule = 0x9000,
      SalStatementRule = 0x9100,
      SalStatementSequenceRule = 0x9200,
      SalDeclarationRule = 0x9300,
      SalVarDeclRule = 0x9400,
      SalFunDeclRule = 0x9500,
      SalProcDeclRule = 0x9600,
      SalProcessBodyRule = 0x9700,
      Sal2FormalsRule = 0x9800,
      Sal2BeginStatementRule = 0x9900,
      Sal2IfStatementRule = 0x9a00,
      Sal2FunctionStatementRule = 0x9b00,
      Sal2VariableStatementRule = 0x9c00,
      Sal2WithStatementRule = 0x9d00,
      Sal2LoopStatementRule = 0x9e00,
      Sal2ProcessStatementRule = 0x9f00,
      Sal2ProcessWaitRule = 0xa000,
      Sal2FileStatementRule = 0xa100,
      Sal2StatementRule = 0xa200,
      Sal2StatementSequenceRule = 0xa300,
      SAL_RULE_END = 0xa400,
      SAL_TYPE_END = 0xa400
    };

  // sal tokenizer values
  static const int TokenError = 0;
  static const int TokenPlaintext = 1;
  static const int TokenComment = 2;
  static const int TokenString = 3;
  static const int TokenSharpSign = 4;
  static const int TokenLispKeyword = 5;
  static const int TokenSalKeyword = 6;
  static const int TokenSalReserved = 7;
  static const int TokenSalClassname = 8;
  static const int TokenSalCommand = 9;

  static const bool isSalType(int i) {return (0x100 <= i) && (i < SAL_TYPE_END);}
  static const bool isSalTokenType(int i) {return (SAL_TOKEN_BEG < i) && (i < SAL_TOKEN_END);}
  static const bool isSalDelimType(int i) {return (SAL_DELIM_BEG < i) && (i < SAL_DELIM_END);}
  static const bool isSalNumberType(int i) {return (SAL_NUMBER_BEG < i) && (i < SAL_NUMBER_END);}
  static const bool isSalBoolType(int i) {return (SAL_BOOL_BEG < i) && (i < SAL_BOOL_END);}
  static const bool isSalOpType(int i) {return (SAL_OP_BEG < i) && (i < SAL_OP_END);}
  static const int  salOpWeight(int i) {return (i & 0xFF);}
  static const bool isSalRelationType(int i) {return (SAL_RELATION_BEG < i) && (i < SAL_RELATION_END);}
  static const bool isSalAssignmentType(int i) {return (SAL_ASSIGNMENT_BEG < i) && (i < SAL_ASSIGNMENT_END);}
  static const bool isSalLiteralType(int i) {return (SAL_LITERAL_BEG < i) && (i < SAL_LITERAL_END);}
  static const bool isSalDefineType(int i) {return (SAL_DEFINE_BEG < i) && (i < SAL_DEFINE_END);}
  static const bool isSalStatementType(int i) {return (SAL_STATEMENT_BEG < i) && (i < SAL_STATEMENT_END);}
  static const bool isSalCommandType(int i) {return (SAL_COMMAND_BEG < i) && (i < SAL_COMMAND_END);}
  static const bool isSalConstituentType(int i) {return (SAL_CONSTITUENT_BEG < i) && (i < SAL_CONSTITUENT_END);}
  static const bool isSalClausalType(int i) {return (SAL_CONSTITUENT_BEG < i) && (i < SAL_CLAUSAL_END);}
  static const bool isSalRuleType(int i) {return (SAL_RULE_BEG < i) && (i < SAL_RULE_END);}
  static const bool isSalTypeEqual( int a, int b) {return ((a >> 8) == (b >> 8));}
  static const int  SalTypeDataBits(int t) {return (t & 0xff);}
  static const bool isSalBlockOpen(int t) {return SalTypeDataBits(t)==SalBlockOpen;}
  static const bool isSalBlockClose(int t) {return SalTypeDataBits(t)==SalBlockClose;}
};

#endif
