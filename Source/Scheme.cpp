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
#include "Enumerations.h"
#include "Scheme.h"
#include "Console.h"
#include "Midi.h"
#include "Csound.h"
#include "CmSupport.h"
#include "SndLibLoad.h"
#include "Scheme.h"

#ifdef WITH_FOMUS
#include "Fomus.h"
#endif

#include "Preferences.h"

#include "Syntax.h"

juce_ImplementSingleton(SchemeThread)

//
// Execution Nodes for Scheme Thread's queue
//

XSchemeNode::XSchemeNode(double qtime)
  : time (qtime),
  userid (-1)
{
  static juce::CriticalSection protect;
  const juce::ScopedLock lock (protect);
  static int nodecounter=0;
  nodeid=++nodecounter;
}

// XControlNode

XControlNode::XControlNode(double qtime, int control, int ident)
  : XSchemeNode(qtime),
    type (control)
{
  userid=ident;
}

XControlNode::~XControlNode()
{
}

bool XControlNode::applyNode(SchemeThread* schemethread, double curtime)
{
  switch (type)
  {
  case QueueStop:
    schemethread->stopProcesses(userid);
    break;
  case QueueQuit:
    schemethread->signalThreadShouldExit();
    break;
  case QueueStopAll:
    schemethread->stopAll();
    break;
  default:
    break;
  }
  return false;
}

// XEvalNode

XEvalNode::XEvalNode(double qtime, juce::String sexpr, bool print)
  : XSchemeNode(qtime),
    expr (sexpr),
    print (print)
{
}

XEvalNode::~XEvalNode()
{
}

bool XEvalNode::applyNode(SchemeThread* schemethread, double curtime) 
{
  Console* console=Console::getInstance();
  s7_scheme* sc=schemethread->scheme;
  //  console->setEvaling(true);
  ////  console->postAsyncMessage(CommandIDs::ConsoleIsEvaling, 1, true);

  // install the begin_hook that allows s7 to quit current eval
  schemethread->setSchemeInterrupt(false);
  s7_set_begin_hook(sc, cm_begin_hook);
  ////  s7_pointer val=s7_eval_c_string(sc, (char *)expr.toUTF8());
  s7_pointer val=s7_eval_c_string(sc, (char *)expr.toUTF8().getAddress());
  s7_set_begin_hook(sc,NULL);
  if (schemethread->isSchemeInterrupt()) 
  {
    console->printWarning("Scheme interrupted!\n");
  }
  else if (print && (val != schemethread->schemeError))
  {
    juce::String str = juce::String(s7_object_to_c_string(sc, val));
    str << "\n";
    console->printValues(str);
  }
  return false; 
}

XSalNode::XSalNode(double qtime, juce::String input, int id, bool xpand, bool mult )
  : XSchemeNode(qtime),
    expr (input),
    expand (xpand),
    multi (mult),
    vers(id)
{
}

XSalNode::~XSalNode()
{
  toks.clear();
}

bool XSalNode::applyNode(SchemeThread* st, double curtime) 
{
  s7_scheme* sc=st->scheme;
  s7_pointer data=st->schemeNil;
  Console* console=Console::getInstance();
  // cons up token list using reverse order of array
  for (int i=toks.size()-1; i>=0; i--)
  {
    data=s7_cons(sc, s7_make_c_pointer(sc, toks.getUnchecked(i)), data);
  }
  // turn data into arglist ("input" ({tok}*) [#t|#f]*)
  data=s7_cons(sc, 
               s7_make_string(sc, expr.toUTF8()),
               s7_cons(sc, 
                       data,
                       s7_cons(sc, 
                               s7_make_boolean(sc, expand),
                               s7_cons(sc, 
                                       s7_make_boolean(sc, multi),
                                       st->schemeNil)))
               );
  // gc protect data cobbled up on the C side
  auto prot = s7_gc_protect(sc, data);
  s7_pointer retn;
  st->isSalEval(true);
  ////  console->postAsyncMessage(CommandIDs::ConsoleIsEvaling, 1, true);
  st->setSchemeInterrupt(false);
  s7_set_begin_hook(sc, cm_begin_hook);
  if (vers==TextIDs::Sal1)
  {
    // sal only side effects so we never process return values
    retn=s7_call(sc, s7_name_to_value(sc, "sal"), data);
  }
  else
  {
    retn=s7_call(sc, s7_name_to_value(sc, "sal2"), data);
    if (retn != st->schemeError)
    {
      juce::String str=juce::String(s7_object_to_c_string(sc, retn)).replaceCharacters("()", "{}");
      str << "\n";
      /*      for (int i=0; i<str.length(); i++)
              if (str[i]=='(')
              str[i]='{';
              else
              if (str[i]==')') 
              str[i]='}'; */
      console->printValues(str);
    }
  }
  s7_gc_unprotect_at(sc, prot);
  s7_set_begin_hook(sc, NULL);
  if (st->isSchemeInterrupt()) 
  {
    console->printWarning("Scheme interrupted!\n");
  }
  st->isSalEval(false);
  ///  console->postAsyncMessage(CommandIDs::ConsoleIsEvaling, 0, true);
  return false;
}

// XProcessNode
//*********************************************************************************************************
XProcessNode::XProcessNode(double qtime, s7_pointer proc, int qid, double startBeat, int metroIndex)
  : XSchemeNode(qtime),
    start(qtime),
    elapsed (0.0),
    schemeproc(proc),
    protect(0),
    metroIndex(metroIndex),
    beatState(startBeat) {
  userid = qid;
  time = qtime;
}

XProcessNode::~XProcessNode()
{
}

//*********************************************************************************************************
bool XProcessNode::applyNode(SchemeThread* st, double curtime)
{
  s7_scheme* sc=st->scheme;
  bool more=false;
  double runtime, delta;
  if (st->isScoreMode())
  {
    // in score mode the scheduler runs in non-real time and
    // node times are in seconds. the node's current time
    // becomes the score time under the callback an is used to
    // set the timestamps of data sent to ports
    runtime=elapsed;
    st->scoretime=time;
  }
  else
  {
    runtime=(time-start)/1000.0;
  }
  
  s7_pointer args = s7_cons(sc,
                            s7_make_real(sc, runtime),
                            s7_nil(sc));
  auto prot = s7_gc_protect(sc, args);
  s7_pointer retn=s7_call(sc, schemeproc, args);
  if (retn==st->schemeError)
    delta=-2;
  else
    delta=s7_number_to_real(sc, retn);

  s7_gc_unprotect_at(sc, prot);
  //std::cout << "after callback, delta is " << delta << "\n";

  // a delta value less than 0 means that the process is done running,
  // where -1 is normal exit, -2 is error exit. in this case gc the
  // proc pointer and do not reschedule the node
  if (delta<0.0)
  {
    s7_gc_unprotect_at(sc,protect);
    more=false;
  }
  else
  {
    //**************************using metro************************************
    beatState += delta;
    //    double metroDelta = st->metros[metroIndex]->getTimeDeltaToBeat(beatState);
    //**************************using metro************************************

    // update the time of the node to the next runttime. the
    // values is in milliseconds if scheduler is running in
    // real time
    more=true;
    if (st->isScoreMode())
    {
      elapsed += delta;  // elapsed now user's next run time
      time += delta;
    }
    else
    {
      //*******************using metro
      //elapsed += metroDelta;  // elapsed now user's next run time
      elapsed += delta; //maintains how many beats have elapsed
      // calculate delta to next runtime, the difference
      // between the last time stamp and the (future) one in
      // elapsed
          
      //delta = elapsed-runtime;
      //if (delta<0.0) delta=0.0;
      //time=Time::getMillisecondCounterHiRes()+(delta*1000.0);
      //******************using metro
      time = st->metros[metroIndex]->getTimeAtBeat(beatState) * 1000;
    }
  }
  st->scoretime=0.0;
  return more;
}

// XReplaceNode

XReplaceNode::XReplaceNode(double qtime, s7_pointer proc, int qid)
  : XSchemeNode(qtime)
{
  userid = qid;
  time = qtime;
  schemeproc = proc;
}

XReplaceNode::~XReplaceNode()
{
}

bool XReplaceNode::applyNode(SchemeThread* schemethread, double curtime)
{
  XProcessNode* n;
  bool firstNode = true;
  s7_scheme* sc = schemethread->scheme;
  
  for (int i = schemethread->schemeNodes.size() - 1; i >= 0; i--)
    if ((n = dynamic_cast<XProcessNode*>(schemethread->schemeNodes[i])))
      if (n->userid == userid)
      {
        if(firstNode)
        {
          s7_gc_unprotect_at(sc, n->protect);
          n->schemeproc = schemeproc;
          n->protect = s7_gc_protect(sc, n->schemeproc);
          firstNode = false;
        }
        else
          schemethread->schemeNodes.remove(i, true);
      }
  return false;
}

//
/// Midi Input Hooks
//

void SchemeThread::midiin(const juce::MidiMessage &msg)
{
  int op = (msg.getRawData()[0] & 0xf0) >> 4;
  // convert noteOn with 0 velocity into NoteOff before processing
  if (op ==  MidiOps::On && msg.getVelocity() == 0)
  {
    if (MidiHook* hook = getMidiHook(MidiOps::Off))
    {
      juce::MidiMessage m = juce::MidiMessage::noteOff(msg.getChannel(), msg.getNoteNumber());
      schemeNodes.addSorted(comparator, new XMidiNode(0.0, m, hook));
      notify();
    }
  }
  else
  {
    if (MidiHook* hook = getMidiHook(op))
    {
      schemeNodes.addSorted(comparator, new XMidiNode(0.0, msg, hook));
      notify();
    }
  }
}

bool SchemeThread::isMidiHook(int opr)
{
  return (getMidiHook(opr, true) != NULL);
}

void SchemeThread::addMidiHook(int op, s7_pointer proc)
{
  auto p = s7_gc_protect(scheme, proc);
  // the "default hook" is always at end
  if (op == 0)
    midiHooks.add(new MidiHook(op, proc, p));
  else
    midiHooks.insert(0, new MidiHook(op, proc, p));
}

MidiHook* SchemeThread::getMidiHook(int opr, bool strict)
{
  MidiHook* hook = NULL;
  //midiHooks.lockArray();
  for (int i = 0; i < midiHooks.size() && !hook; i++)
  {
    MidiHook* h = midiHooks.getUnchecked(i);
    // hook matches oper if its the same or default hook set
    if (h->op == opr || (h->op == 0 && !strict))
      hook = h;
  }
  //  midiHooks.unlockArray();
  return hook;
}

void SchemeThread::removeMidiHook(MidiHook* hook)
{
  // FIX: THIS MUST ALSO REMOVE ANY PENDING XMIDINODE WITH HOOK
  //  midiHooks.lockArray();
  s7_gc_unprotect_at(scheme, hook->protect);
  midiHooks.removeObject(hook);
  //  midiHooks.unlockArray();
}

bool SchemeThread::clearMidiHook(int op)
{
  // if there is a hook under Op remove it and return true
  // if op==-1 remove all hooks
  if (op < 0) 
  {
    int s = midiHooks.size();
    for (int i = 0; i < s; i++)
      removeMidiHook(midiHooks.getLast());
    return (s > 0);
  }
  else
  {
    MidiHook* hook = getMidiHook(op, true);
    if (hook)
    {
      removeMidiHook(hook);
      return true;
    }
    return false;
  }
}

bool XMidiNode::applyNode(SchemeThread* st, double curtime)
{
  s7_scheme* sc = st->scheme;
  // called on Midi message nodes if an input hook is set
  int op = (mmess.getRawData()[0] & 0xf0) >> 4;
  int ch = mmess.getChannel() - 1;
  int d1 = mmess.getRawData()[1] & 0x7f;
  int d2 = 0;
  if (mmess.getRawDataSize() > 2)
    d2 = mmess.getRawData()[2] & 0x7f;
  // convert MidiOns with zero velocity to MidiOff
  if ((op == MidiOps::On) && (d2 == 0))
    op = MidiOps::Off;
  // create list of message data
  s7_pointer args=s7_cons(sc, 
                          s7_make_integer(sc, ch),
                          s7_cons(sc,
                                  s7_make_integer(sc, d1),
                                  s7_cons(sc,
                                          s7_make_integer(sc, d2),
                                          st->schemeNil)));
  // ALWAYS PUSH OPCODE // push status opcode if default hook
  if (true) // (hook->op==0)
    args=s7_cons(sc, s7_make_integer(sc, op), args);

  // create funargs list holding data
  args = s7_cons(sc, args, st->schemeNil);
  auto prot = s7_gc_protect(sc, args);
  s7_call(sc, hook->proc, args);
  s7_gc_unprotect_at(sc, prot);
  // stubbed out because a void return seems to be the same as schemeError ???????
//  if (0)
//  {
//    st->removeMidiHook(hook);
//    Console::getInstance()->printError(">>> Removed Midi receiver.\n");
//  }
  return false;
}

/*=======================================================================*
  Osc Receiving
  *=======================================================================*/

XOscNode::XOscNode(double qTime, const juce::String& path, const juce::OSCMessage& message, OscHook* hook)
  : XSchemeNode(qTime),
    path (path),
    message (message),
    hook (hook)
{
}

XOscNode::~XOscNode()
{
}

bool SchemeThread::isOscHook(juce::String path)
{
  return (getOscHook(path,true) != NULL) ;
}

void SchemeThread::addOscHook(juce::String path, s7_pointer proc)
{
  //oscHooks.lockArray();
  auto protect = s7_gc_protect(scheme, proc);
  // the "default hook" is always at end
  if (path == juce::String())
    oscHooks.add(new OscHook(path, proc, protect));
  else
    oscHooks.insert(0, new OscHook(path, proc, protect));
  //  oscHooks.unlockArray();
}

OscHook* SchemeThread::getOscHook(juce::String path, bool strict)
{
  OscHook* hook = 0;
  for (int i = 0; i < oscHooks.size() && !hook; i++)
  {
    OscHook* h = oscHooks.getUnchecked(i);
    // hook matches oper if its the same or default hook set
    if (h->path == path || (h->path==juce::String() && !strict))
      hook = h;
  }
  return hook;
}

void SchemeThread::removeOscHook(OscHook* hook)
{
  // FIX: THIS MUST ALSO REMOVE ANY PENDING XMIDINODE WITH HOOK
  //  oscHooks.lockArray();
  s7_gc_unprotect_at(scheme, hook->protect);
  oscHooks.removeObject(hook);
  //  oscHooks.unlockArray();
}

bool SchemeThread::clearOscHook(juce::String path)
{
  // if there is a hook under Op remove it and return true
  // if op==* remove all hooks
  if (path == "*") 
  {
    int s = oscHooks.size();
    for (int i = 0; i < s; i++)
      removeOscHook(oscHooks.getLast());
    return (s > 0);
  }
  else
  {
    OscHook* hook=getOscHook(path, true);
    if (hook)
    {
      removeOscHook(hook);
      return true;
    }
    return false;
  }
}

bool XOscNode::applyNode(SchemeThread* st, double curtime)
{
  s7_scheme* sc = st->scheme;
  // the osc message data starts with the string path
  s7_pointer snil = st->schemeNil;
  // initialize data to ("path")
  s7_pointer data = s7_cons(sc, s7_make_string(sc, path.toUTF8()), snil);
  s7_pointer tail = data;
 
  // iterate message args nconc'ing message data onto end of data list
  for (const juce::OSCArgument* arg = message.begin(); arg != message.end(); ++arg)
  {
    if (arg->isInt32())
    {
      s7_set_cdr(tail, s7_cons(sc, s7_make_integer(sc, arg->getInt32()), snil));
    }
    else if (arg->isFloat32())
    {
      s7_set_cdr(tail, s7_cons(sc, s7_make_real(sc, arg->getFloat32()), snil));
    }
    else if (arg->isString())
    {
      s7_set_cdr(tail, s7_cons(sc, s7_make_string(sc, arg->getString().toUTF8()), snil));
    }
    else if (arg->isBlob())
    {
      const juce::MemoryBlock& blob = arg->getBlob();
      {
        s7_pointer m = snil;
        // reverse collect for speed
        for ( long j = blob.getSize() - 1; j >= 0; j--)
          m = s7_cons(sc, s7_make_integer(sc, blob[j]), m);
        s7_set_cdr(tail, s7_cons(sc, s7_cons(sc, m, snil), snil));
      }
    }
    else
      continue;
    tail = s7_cdr(tail);
  }
  // build callback
  int loc;
  s7_pointer args, res;
  // set hook's args to (("path" . data))
  args = s7_cons(sc, data, snil);
  loc = s7_gc_protect(sc, args);
  // call the hook function
  res = s7_call(sc, hook->proc, args);
  s7_gc_unprotect_at(sc, loc);
  return false;
}

//
// Scheduler
//

SchemeThread::SchemeThread() 
  : juce::Thread("Scheme Thread"),
    scheme (0),
    oscHook (0),
    schemeFalse (0),
    schemeTrue (0),
    schemeNil (0),
    schemeError (0),
    schemeVoid (0),
    nextid (0),
    voidstring (juce::String()),
    quiet (false),
    pausing (false),
    sprouted (false),
    backtrace(true),
    interrupted (false),
    scoremode (ScoreTypes::Empty),
    saleval (false),
    scoretime (0.0)
{
  voidstring="<ok>\n";
  // create the default (system) metronome, id and index must be 0.
  Metronome* m = new Metronome();
  m->identifier = 0;
  metros.add(m);
}

SchemeThread::~SchemeThread()
{
  stopThread(1000);
  schemeNodes.clear();
  midiHooks.clear();
  oscHooks.clear();
  metros.clear();
}

void SchemeThread::signalSchemeError(juce::String text)
{
  // use this function to signal errors in C code called by Scheme
  s7_error(scheme,
           schemeError, //s7_make_symbol(scheme, "scheme-error"), 
           s7_make_string(scheme, text.toUTF8())
           );
}

bool SchemeThread::showBacktrace()
{
  return backtrace;
}

void SchemeThread::setShowBacktrace(bool shouldShow)
{
  backtrace = shouldShow;
  juce::String s ("(set! *error-trace* ");
  s << ((shouldShow) ? "#t" : "#f") << ")";
  eval(s, false);
}

void SchemeThread::setScoreMode(int mode)
{
  scoremode=mode;
}

bool SchemeThread::isScoreMode()
{
  // true if any score is in progress
  return (scoremode > ScoreTypes::Empty);
}

bool SchemeThread::isScoreMode(int mode)
{
  return (scoremode == mode);
}

double SchemeThread::getScoreTime()
{
  return scoretime;
}

/// SalEval

bool SchemeThread::isSalEval()
{
  return saleval;
}

void SchemeThread::isSalEval(bool sal)
{
  saleval=sal;
}

// loading

void SchemeThread::load(juce::File file, bool addtorecent)
{
  //std::cout << "SchemeThread::load()\n";
  if (file==juce::File())
  {
    juce::FileChooser choose("Load",
                             juce::File::getCurrentWorkingDirectory());
    if (choose.browseForFileToOpen())
    {
      file=choose.getResult();
    }
    else
      return;
  }
  juce::String text=juce::String();
  if (file.existsAsFile())
  {
    juce::String path=file.getFullPathName().quoted();
    if (SysInfo::isWindows())
      path=path.replace("\\","\\\\");
    text << "(load " << path << ")";
    //std::cout << "load text='" << text.toUTF8() << "'\n";
    eval(text, true);
    if (addtorecent)
    {
      Preferences::getInstance()->recentlyLoaded.addFile(file);
    }
  }
  else
  {
    text << ">>> Error: load file "
         << file.getFullPathName().quoted()
         << " does not exist.";
    Console::getInstance()->printError(text);
  }
}

void SchemeThread::read()
{
  bool more=true;
  bool prompt=true;
  juce::String text=juce::String();
  juce::CodeDocument doc;
  juce::CodeDocument::Position pos((const juce::CodeDocument&)doc,0);
  while (more && !threadShouldExit()) 
  {
    if (prompt)
    {
      Console::getInstance()->printPrompt();
      prompt=false;
    }
    std::string line="";
    getline(std::cin, line);
    if (!text.isEmpty())
      text << "\n";
    text << juce::String(line.c_str());
    int typ;
    doc.replaceAllContent(text);
    pos.setPosition(0);
    typ=LispSyntax::getInstance()->scanCode(doc, pos, true, ScanIDs::MoveExpressions);
    if (typ==ScanIDs::SCAN_LIST || typ==ScanIDs::SCAN_TOKEN || typ==ScanIDs::SCAN_STRING)
      break;
    else if (typ==ScanIDs::SCAN_UNLEVEL)
      break;  // allow too many parens to be passed to lisp?
  }
  if (!text.isEmpty())
    eval(text, true);
}

void SchemeThread::printBanner()
{
  juce::String banner;
  banner << "Grace - Graphical Realtime Algorithmic Composition Environment\n"
    //         << "        "
         << SysInfo::getCopyright("University of Illinois Board of Trustees.") 
         << "\n";
  banner << juce::SystemStats::getJUCEVersion()
         << " " << SysInfo::getCopyright("Julian Storer") << "\n"
         << getLispVersion() 
         << "\n";
#ifdef WITH_LIBLO
  banner << "LIBLO " << SysInfo::getCopyright("Steve Harris & Stephen Sinclair") << "\n";
#endif
#ifdef WITH_OSCPACK
  banner << "oscpack 1.1.0 " << SysInfo::getCopyright("Ross Bencina") << "\n";
#endif
#ifdef WITH_FOMUS
  if (fomus_exists) 
    banner << Fomus::getFomusVersion() 
           << "\n";
#endif
#ifdef WITH_SDIF
  banner << "SDIF " 
         << juce::String(SDIF_VERSION_STRING) << " "
         << SysInfo::getCopyright("IRCAM")
         << "\n";
#endif
  banner << "\n" << SysInfo::getCMLogo() 
         << "\n";
  Console::getInstance()->printOutput(banner);
}
//*********************************************************************************************************

void SchemeThread::run()
{
  double qtime, utime;
  XSchemeNode* node;

  if (!init())
    return;
  if (!isQuiet())
    printBanner();
  pausing=false;
  while (! threadShouldExit())
  {
    while ( true ) 
    {
      //	  schemeNodes.lockArray();
      node=schemeNodes.getFirst();
      //	  schemeNodes.unlockArray();
      if ( node == NULL )
      {
        break;
      }
      // if scoremode is true then qtime will be in seconds else
      // milliseconds.
      qtime=node->time;
      utime = juce::Time::getMillisecondCounterHiRes();
      // this should probably test if the difference between qtime
      // and utime is less that 1ms, if not then it probably
      // shouldn't sleep (?)
      if ( qtime > utime )
      {
        // if scoremode is true then qtime will be in seconds
        // so this will not happen (which is what we want)
        wait(1);
      }
      else
      {
        //	      schemeNodes.lockArray();
        schemeNodes.remove(0, false);
        //	      schemeNodes.unlockArray();
        // NOTE: the node to process has now been popped from the
        // queue.  i did this while trying to debug the random
        // crashing. im not sure if this is the right thing to do or
        // not since this means that flushing the queue, etc will have
        // no effect on this node. Search for the word POP to see the
        // places this affects...
        //lock.enter();
        bool keep=node->applyNode(this, 0.0);
        //lock.exit();
        if (keep)
        {
          //		  schemeNodes.lockArray();
          schemeNodes.addSorted(comparator,node);
          //          schemeNodes.remove(0, false);
          //		  schemeNodes.unlockArray();
        }
        else
        {
          delete node;
        }
      }
      node=NULL;
    }
    // queue is now empty. 
    //    std::cout << "Queue is empty\n";
    if (sprouted && isScoreMode())
      closeScore();
    sprouted=false;
    scoretime=0.0;
    wait(-1);
  }
  // leaving killed process....
}

void SchemeThread::closeScore()
{
  if (!isScoreMode()) return;
  // we just finished running a sprouted process in score
  // mode, send a "score complete" message to the console for
  // processing the file output
  if (isScoreMode(ScoreTypes::Midi))
    MidiOutPort::getInstance()->performCommand(CommandIDs::SchedulerScoreComplete);
  else if (isScoreMode(ScoreTypes::SndLib))
    SndLib::getInstance()->performCommand(CommandIDs::SchedulerScoreComplete);
#ifdef WITH_FOMUS
  else if (isScoreMode(ScoreTypes::Fomus))
    Fomus::getInstance()->closeScore();
#endif	  
  else if (isScoreMode(ScoreTypes::Csound))
    Csound::getInstance()->saveScore();
  setScoreMode(ScoreTypes::Empty);
}

void SchemeThread::clear()
{
  // this should NEVER be called from scheme code (an EVAL node) or
  // else the run() loop will bomb since it assumes index[0] exists
  //  schemeNodes.lockArray();
  schemeNodes.clear();
  //  schemeNodes.unlockArray();
}

void SchemeThread::addNode(XSchemeNode* node)
{
  //  schemeNodes.lockArray();
  schemeNodes.addSorted(comparator, node);
  //  schemeNodes.unlockArray();
  notify();
}

// addNode for processes

void SchemeThread::sprout(double _time, s7_pointer proc, int _id, int metroId, double startBeat)
{
  // this method is only called by scheme code via sprout() under an
  // eval node.  this means that a lock.enter() is in effect so we
  // dont need to lock anything to reference scoremode.
  int metroIndex = getMetroIndexFromId(metroId);
  if (!isScoreMode())
  {
    //_time = (_time * 1000) + juce::Time::getMillisecondCounterHiRes();
    //************metro test****************//
    if(startBeat > 0)
      _time = metros[metroIndex]->getTimeAtBeat(startBeat) * 1000;
    else
    {
      startBeat = metros[metroIndex]->getNowBeat() + _time;
      _time = metros[metroIndex]->getTimeAtBeat(startBeat) * 1000;
    }
    //**************************************//
  }
  else
    _time += scoretime; // shift process to current scoretime under callback
  
  s7_gc_protect(scheme, proc); // don't let gc touch it
  
  XProcessNode* n;
  bool duplicateID = false;
  
  if(!(_id == 0)) //if there is NOT a default id, check for duplicate id's
  {
    for (int i = schemeNodes.size() - 1; i >= 0; i--)
      if ((n = dynamic_cast<XProcessNode*>(schemeNodes[i])))
        if (n->userid == _id)
        {
          duplicateID = true;
          break;
        }
  }
  
  if(duplicateID)
  {
    schemeNodes.insert(0, new XReplaceNode(0.0, proc, _id));
    notify();
  }
  else
  {
    sprouted = true;  // tell scheduler that we have a process running
    schemeNodes.addSorted(comparator, new XProcessNode( _time, proc, _id, startBeat, metroIndex));
    notify();
  }
}

void SchemeThread::eval(juce::String s, bool printOutput)
{
  schemeNodes.addSorted(comparator, new XEvalNode(0.0, s, printOutput));
  notify();
}

void SchemeThread::quit()
{
  schemeNodes.addSorted(comparator, new XControlNode(0.0, XControlNode::QueueQuit));
  notify();
}

void SchemeThread::setPaused(bool p) {}

void SchemeThread::stop(int ident, bool all)
{
  // always add stop nodes to the front of the queue.
  if(all)
    schemeNodes.insert(0, new XControlNode (0.0, XControlNode::QueueStopAll, ident));
  else
    schemeNodes.insert(0, new XControlNode (0.0, XControlNode::QueueStop, ident));
  notify();
}

void SchemeThread::stopProcesses(int ident)
{
  // this is called by a STOP node from process().  stop all processes
  // with id from running. iterate queue in reverse order so removal index
  // remains valid.  NOTE: POP removed the (STOP) node that got us here so this
  // deletes up to and including index 0. otherwise dont include index 0

  // else selectively remove all nodes with id process queue in
  // reverse order so removal index always valid
  for (int i = schemeNodes.size() - 1; i >= 0; i--)
    if (XProcessNode* n = dynamic_cast<XProcessNode*>(schemeNodes[i]))
      if (n->userid == ident) 
        schemeNodes.remove(i, true);
}

void SchemeThread::stopAll()
{
  for (int i = schemeNodes.size() - 1; i >= 0; i--)
    if (dynamic_cast<XProcessNode*>(schemeNodes[i]) != NULL)
      schemeNodes.remove(i,true); 
  // if stopped all processes also clear any pending messages and send
  // all notes off
  // GET RID OF THIS!
  MidiOutPort::getInstance()->clear();
}

bool SchemeThread::isQueueEmpty()
{
  int size = schemeNodes.size();
  return (size == 0);
}

void SchemeThread::interruptScheme()
{
  schemeNodes.clear(); // remove any running nodes from scheduler
  clearMidiHook(-1);
  clearOscHook("*");
}

void SchemeThread::updateNodeTimes()
{
  for(int i=schemeNodes.size()-1; i>=0; i--)
  {
    if(XProcessNode* n = dynamic_cast<XProcessNode*>(schemeNodes[i]))
    {
      n->time = metros[n->metroIndex]->getTimeAtBeat(n->beatState) * 1000.0;
    }
  }
  schemeNodes.sort(comparator);
}

/*=======================================================================*
  Metronomes
  *=======================================================================*/

int SchemeThread::getMetroIndexFromId(int id)
{
  if (id==0) 
    return id;
  else
    for (int i=0; i<metros.size(); i++)
      if (metros.getUnchecked(i)->identifier == id)
        return i;
  return -1;
}

int SchemeThread::makeMetro(double tempo)
{
  static juce::uint32 uid=0;
  Metronome* m = new Metronome(tempo);
  // identifer just a magic number 
  m->identifier = (++uid) + juce::Time::getMillisecondCounter();
  metros.add(m);
  return m->identifier;
}

void SchemeThread::deleteMetro(int id)
{
  int index=getMetroIndexFromId(id);
  if (index>0)
  {
    metros.remove(index);
  }
}

bool SchemeThread::syncMetros(int metroindex, double beatsAhead, int masterindex,
                              double tempo, bool isbeats, int mode)
{
  double targetBeat, targetTime;
  if(!isbeats)
  {
    targetTime = metros[masterindex]->getNowTime() + beatsAhead;
    targetBeat = ceil(metros[masterindex]->getBeatAtTime(targetTime));
    targetTime = metros[masterindex]->getTimeAtBeat(targetBeat);
  }
  else
  {
    targetBeat = ceil(metros[masterindex]->getNowBeat()) + beatsAhead;
    targetTime = metros[masterindex]->getTimeAtBeat(targetBeat);
  }
  if(tempo <= 0)
    tempo = metros[metroindex]->getNowTempo();
  return metros[metroindex]->adjustBeatToTime(targetTime, tempo, mode);
}
