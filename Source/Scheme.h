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

#ifndef CM_SCHEME_H
#define CM_SCHEME_H

#include "Libraries.h"

#include "Syntax.h"
#include "Metronome.h"
#include "OpenSoundControl.h"

class SchemeThread;

/*=======================================================================*
  Input Hook Classes
  *=======================================================================*/

class OscHook
{
public:
  juce::String path;
  s7_pointer proc;
  s7_int protect;
  OscHook (juce::String oscpath, s7_pointer func, s7_int p) : path (oscpath), proc (func), protect(p) {}
  ~OscHook () {}
};

class MidiHook
{
public:
  int op;
  s7_pointer proc;
  s7_int protect;
  MidiHook (int oper, s7_pointer func, s7_int p) : op (oper), proc (func), protect(p) {}
  ~MidiHook () {}
};

/*=======================================================================*
  Scheme Execution Node Classes
  *=======================================================================*/

class XSchemeNode 
{
public:
  double time;
  int nodeid;
  int userid;  // user's id
  XSchemeNode(double qtime);
  virtual ~XSchemeNode(){}
  virtual bool applyNode(SchemeThread* scheme, double curtime)=0;
};

class XControlNode : public XSchemeNode
{
public:
  enum {QueueStop=1, QueueQuit, QueueStopAll};
  int type;
  XControlNode(double qtime, int control, int ident=-1);
  ~XControlNode();
  bool applyNode(SchemeThread* scheme, double curtime);
};

class XEvalNode : public XSchemeNode
{
public:
  juce::String expr;
  bool print;
  XEvalNode(double qtime, juce::String sexpr, bool print);
  ~XEvalNode();
  bool applyNode(SchemeThread* scheme, double curtime);
};

class XSalNode : public XSchemeNode
{
public:
  juce::String expr;
  bool expand;
  bool multi;
  int vers;
  //OwnedArray<SynTok>* toks;
  juce::OwnedArray<Syntax::SynTok> toks;
  XSalNode(double qtime, juce::String input, int id, bool xpand=false, bool mult=false);
  ~XSalNode();
  bool applyNode(SchemeThread* scheme, double curtime);
};

class XProcessNode : public XSchemeNode
{
public:
  double start;
  double elapsed;
  s7_pointer schemeproc;
  s7_int protect;
  //  int id;
  //XProcessNode(double qtime, s7_pointer proc, int qid);
  //****************************************metro use*******************//
  int metroIndex;
  double beatState;
  XProcessNode(double qtime, s7_pointer proc, int qid, double startBeat, int metroIndex = 0);
  //****************************************metro use****************//
  ~XProcessNode();
  bool applyNode(SchemeThread* schemethread, double curtime);
};

class XReplaceNode : public XSchemeNode
{
public: 
  s7_pointer schemeproc;
  XReplaceNode(double qtime, s7_pointer proc, int qid);
  ~XReplaceNode();
  bool applyNode(SchemeThread* schemethread, double curtime);
};

class XMidiNode : public XSchemeNode
{
public:
  const juce::MidiMessage mmess;
  MidiHook* hook;
  XMidiNode(double qtime, const juce::MidiMessage &mess, MidiHook* huk)
    : XSchemeNode (qtime), mmess (mess), hook (huk) {}
  ~XMidiNode(){}
  bool applyNode(SchemeThread* scheme, double curtime);
};

struct XOscNode : public XSchemeNode
{
  const juce::String path;
  const juce::OSCMessage message;
  OscHook* hook;
  XOscNode(double qtime, const juce::String& path, const juce::OSCMessage& message, OscHook* hook) ;
  ~XOscNode() ;
  bool applyNode(SchemeThread* scheme, double curtime) ;
};

class XSchemeNodeComparator
{
public:
  static int compareElements(XSchemeNode* e1, XSchemeNode* e2)
  {
    if (e1->time < e2->time)
      return -1;
    else if (e2->time < e1->time)
      return 1;
    // else both at same time, return node that was added first
    else if (e1->nodeid<e2->nodeid)
      return -1;
    else
      return 1;
  }
};

/*=======================================================================*
  Scheme Thread Singleton
  *=======================================================================*/

class SchemeThread : public juce::Thread
{
public:
  
  SchemeThread() ;
  ~SchemeThread();

  s7_scheme *scheme;
  s7_pointer oscHook;
  s7_pointer schemeFalse;
  s7_pointer schemeTrue;
  s7_pointer schemeNil;  
  s7_pointer schemeError; 
  s7_pointer schemeVoid;
  void signalSchemeError(juce::String text);

  int nextid;
  juce::String voidstring;
  bool quiet;
  bool isQuiet(){ return quiet;}
  void setQuiet(bool q){quiet=q;}


  bool pausing;
  bool sprouted;
  bool backtrace;
  bool interrupted;
  juce::ReadWriteLock interruptlock;

  // called by s7 to see if it should quit currrent execution

  bool isSchemeInterrupt()
  {
    const juce::ScopedReadLock mylock (interruptlock);
    return interrupted;
  }

  bool showBacktrace();
  void setShowBacktrace(bool b);

  /* setSchemeInterrupt(true) called by user from editor thread to
     tell scheme to quit current eval. scheme thread calls it with
     false just before each eval to clear the status. */

  void setSchemeInterrupt(bool flag)
  {
    const juce::ScopedWriteLock mylock (interruptlock);
    interrupted=flag;
  }

  // Midi Receiving
  juce::OwnedArray<MidiHook, juce::CriticalSection> midiHooks;
  void midiin(const juce::MidiMessage &mess);
  bool isMidiHook(int opr);
  MidiHook* getMidiHook(int opr, bool strict=false);
  void removeMidiHook(MidiHook* hook);
  bool clearMidiHook(int opr);
  void addMidiHook(int opr, s7_pointer proc);

  // Midi Receiving
  juce::OwnedArray<OscHook, juce::CriticalSection> oscHooks;
  bool isOscHook(juce::String path=juce::String());
  OscHook* getOscHook(juce::String path, bool strict=false);
  void removeOscHook(OscHook* hook);
  bool clearOscHook(juce::String path);
  void addOscHook(juce::String path, s7_pointer proc);
  void receiveOsc(XOscNode* n);
 
  int scoremode;
  void setScoreMode(int mode);
  bool isScoreMode();
  bool isScoreMode(int mode);
  void closeScore();
  
  bool saleval;
  bool isSalEval();
  void isSalEval(bool sal);

  double scoretime;
  double getScoreTime();
    
  juce::OwnedArray<XSchemeNode, juce::CriticalSection> schemeNodes;
  XSchemeNodeComparator comparator;  

  // These next methods are defined in the scheme implementation files
  // (SndLib.cpp and Chicken.cpp)
  bool init();
  void interruptScheme();
  void printBanner();
  void cleanup();
  juce::String getLispVersion();

  void sprout(double _time, s7_pointer c=0, int _id=0, int metroId = 0, double startBeat = 0);
  void eval(juce::String str, bool printOutput);
  //  void eval(char* str);
  void load(juce::File file, bool addtorecent=false);

  void quit();
  void read();
  void run();
  void clear();
  void addNode(XSchemeNode* node);
  bool isPaused() { return pausing; }
  void setPaused(bool b);
  void stop(int id = 0, bool all = false);
  void stopProcesses(int id);
  void stopAll();
  void performSchedulerCommand(juce::CommandID id);
  bool isQueueEmpty();
  
  //---------//
  //Metronome//
  //---------//

  /** The array of metronomes with index 0 always the default (system)
      metro **/
  juce::OwnedArray<Metronome, juce::CriticalSection> metros;

  /** Creates a new metronome and returns its unique id **/
  int makeMetro(double tempo);

  /** Deletes a user metronome with id. Does nothing if id is not
      valid. **/
  void deleteMetro(int id);

  /** Returns a metronome's index given its id or -1 if the id is not
      valid. **/
  int getMetroIndexFromId(int id);

  /** Forces a "slave" metronome to align with another metronome, the
      masterMetroId.  The coincident downbeats will occur so many beats
      later, according to beatsAhead, and these are beats according to the
      master metronome.  If the two metronomes are at the same tempo, then
      syncing them will make them perfectly in sync.  Note, however, that this
      function does not presume that both metronomes are the same tempo, but
      simply guarantees that in so many beats in the future two beats between
      two metronomes will perfectly align.  If a tempo is indicated, then the
      metronome will also change its tempo within the indicated interval.  A
      tempo indication of 0 means that the metronome will maintain its current
      tempo.  If isbeats = false, then beatsAhead actually indicates the amount
      of time in seconds before the two metronomes will align.  If mode is 0,
      then the metronome will either be slowed or sped up in order to achieve
      the syncing - whichever is shortest.  A mode of -1 will force it to slow
      down the metronome to achieve the sync, and a mode of 1 will force it to
      speed up.**/
  bool syncMetros(int metroId, double beatsAhead, int masterMetroId, double tempo,
                  bool isbeats, int mode);

  /** Iterates through the schemeNodes array and checks the process nodes' 
      scheduled time against its current metronome settings.  This must
      be called when a metronome's tempo is changed so that events scheduled
      before the tempo change will still occur at the correct time.**/
  void updateNodeTimes();
  
  juce_DeclareSingleton (SchemeThread, true)
};

#endif
