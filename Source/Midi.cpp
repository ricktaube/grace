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
#include "Main.h"
#include "CmSupport.h"
#include "Scheme.h"
#include "Console.h"
#include "Midi.h"
#include "Preferences.h"
#include "CodeEditor.h"
#include "MidiPlaybackThread.h"
#include "Audio.h"

juce_ImplementSingleton(MidiOutPort)
juce_ImplementSingleton(MidiInPort)

//
//  Nodes
//

bool MidiNode::process()
{
  bool flag = false;

  switch (type)
  {
  case MidiOps::On:
    if (data2 > 0.0)  // handle velocity ranges 0.0-1.0 or 0.0-127.0
    {
      float vel = (float)((data2 > 1.0) ? (data2 / 127.0) : data2);
      juce::MidiMessage msg = juce::MidiMessage::noteOn(chan + 1, (int)data1, vel);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
      // convert object to note off and return true to reschedule
      if (duration > 0.0)
      {
        type = MidiOps::Off;
        time += (duration * 1000.0);
        duration = 0.0;
        flag = true;  // signal to reschedule this node
      }
    }
    else 
    {
      juce::MidiMessage msg = juce::MidiMessage::noteOff(chan + 1, (int)data1);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;

  case MidiOps::Off:
    {
      juce::MidiMessage msg = juce::MidiMessage::noteOff(chan + 1, (int)data1);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;
      
  case MidiOps::Prog:
    {
      juce::MidiMessage msg = juce::MidiMessage::programChange(chan + 1, (int)data1);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;
      
  case MidiOps::Ctrl:
    {
      juce::MidiMessage msg = juce::MidiMessage::controllerEvent(chan + 1, (int)data1, (int)data2);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;
      
  case MidiOps::Bend:
    {
      juce::MidiMessage msg = juce::MidiMessage::pitchWheel(chan + 1, (int)data1) ;
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;
      
  case MidiOps::Touch:
    {
      juce::MidiMessage msg = juce::MidiMessage::aftertouchChange(chan + 1, (int)data1, (int)data2);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;
      
  case MidiOps::Press:
    {
      juce::MidiMessage msg = juce::MidiMessage::channelPressureChange(chan + 1, (int)data1);
      msg.setTimeStamp(time);
      midiOutPort->sendOut(msg);
    }
    break;
      
  default:
    break;
  }
  return flag;
}

//Handles all MIDI output, including from other threads
void MidiOutPort::sendOut(juce::MidiMessage& msg)
{
  const juce::ScopedLock lock (outLock);
  if (device)
  {
    device->sendMessageNow(msg);
  }
  else if (isInternalSynth || (openDeviceId == 0))
  {
    AudioManager::getInstance()->sendMessageToPluginGraph(msg);
  }
}

//-------------//
//Midi Out Port//
//-------------//

MidiOutPort::MidiOutPort()
  : juce::Thread("Midi Out Port"),
    openDeviceId (-1),
    isInternalSynth(false),
    internalSynthName ("Internal Synth"),
    device(0),
    microDivisions (1),  // micro initialized for semitone
    microIncrement (1.0),
    microChanCount (16),
    microChanBlock (16),
    avoidDrumTrack (true),
    pitchbendwidth (2)
{	
  // if there is an internal synth then its name is shown at index 0
  // in the list of output devices return by getDevices()
  // HKT FIXME2
  if (true)//AudioManager::getInstance()->internalSynth != 0)
    isInternalSynth = true;

  // initialize program changes to 0 (Grand Piano)
  for (int i = 0; i < 16; i++)
    programChanges[i] = 0;
  // create menu of tuning sizes
  const char* const names[] = {"Semitone", "Quartertone", "33 Cent", "25 Cent", "20 Cent",
                               "17 Cent", "14 Cent", "12 Cent", "11 Cent", "10 Cent", "9 Cent",
                               "8 Cent", "7.7 Cent", "7.1 Cent", "6.6 Cent", "6.25 Cent", 0};
  tuningNames.addArray(juce::StringArray(names));
  // create menu of GM instrument names
  for (int i = 0; i < 128; i++)
    instrumentNames.add(juce::MidiMessage::getGMInstrumentName(i));
  // auto open saved output device
  juce::String name = Preferences::getInstance()->getStringProp("MidiOutDevice");
  // if we've booted without an output device in the preferences then
  // autoselect the internal synth if we can.
  if (name.isEmpty() && isInternalSynth)
    name = internalSynthName;
  if (name.isNotEmpty())
    open(name);
}

MidiOutPort::~MidiOutPort()
{
  stopThread(100);
  // delete without close() so last port stays set in prefs
  if (device != 0)
    delete device;
  device = 0;
  openDeviceId = -1;
  tuningNames.clear();
  instrumentNames.clear();
  outputNodes.clear();
  captureSequence.clear();
}

const juce::StringArray MidiOutPort::getDevices()
{
  //  static int n = 0;
  //  std::cout << "--\ncalling getDevices() [" << ++n << "]\n";
  juce::StringArray outs = juce::MidiOutput::getDevices();
  if (outs.contains("<error>"))
  {
    std::cout << "--\nMIDI DEVICE ERROR:\n";
    for(int i = 0; i < outs.size(); i++)
      std::cout << "devices[" << i << "]=" << outs[i] << "\n";
    std::cout << "\n";
  }
  if (isInternalSynth)
    outs.insert(0, internalSynthName);
  return outs;
}

bool MidiOutPort::open(juce::String name) 
{
  juce::StringArray devices = getDevices();
  int id = -1; // no id
  for (int i = 0; i < devices.size();i++)
    if (devices[i] == name) 
    {
      id = i;
      break;
    }
  // warn the user if we cant find the requested device in the current
  // list of devices
  if (id == -1)
  {
    Preferences::getInstance()->setStringProp("MidiOutDevice", juce::String());
    Console::getInstance()->printWarning("Warning: Midi output device " + name + " does not exist.\n");
    return false;
  }
  else
    return open(id);
}

bool MidiOutPort::open(int id)
{
  // Open the device number id which is an index into the getDevices()
  // array. On the mac id 0 is the internal synth and juce's physical
  // devices start at index 1

  // opening same port, go home
  if (id == openDeviceId) 
    return true;

  //(1) Close any existing open device
  if (device) // have a physical device
  {
    // delete the current open device before opening a new one
    //    std::cout << "deleting physical device (openDeviceId was " << openDeviceId << " and now is -1)\n";
    delete device;
    device = 0;
    openDeviceId = -1;
  }
  else if (isInternalSynth && openDeviceId == 0) // internal synth
  {
    //    std::cout << "'clearing' internal synth device (openDeviceId was " << openDeviceId << " and now is -1)\n";
    device = 0; // no device to delete
    openDeviceId = -1;
  }

  //(2) Open new device
  if (id == 0 && isInternalSynth) // internal synth 
  {
    device = 0;        // no physical device
    openDeviceId = id; // device id == 0
    //    std::cout << "'opening' internal synth device, openDeviceId=" << openDeviceId << "\n";
  }
  else 
  {
    // handle physical devices. if there is an internal synth physical
    // (juce) devices start at index 1 and to open the actual
    // juce::MidiOut device we subtract 1 from its index
    int juceDeviceIndex = (isInternalSynth) ? id - 1 : id;
    // HKT FIXME
    device = juce::MidiOutput::openDevice(juceDeviceIndex).release();
    if (!device)
    {
      openDeviceId = -1;
      //      std::cout << "FAILED to open physical device! juceDeviceIndex=" 
      //                << juceDeviceIndex << " openDeviceId=" << openDeviceId << "\n";
    }
    else
    {
      openDeviceId = id;
      //      std::cout << "opened physical device, juceDeviceIndex=" 
      //                << juceDeviceIndex << " openDeviceId=" << openDeviceId << "\n";
    }
  }

  //(3) Update preference file with current configuration
  if (openDeviceId == -1)
  {
    // no device was opened for the id
    Preferences::getInstance()->setStringProp("MidiOutDevice", juce::String());
    Console::getInstance()->printError(">>> Error: Failed to open midi output device " + juce::String(id) + ".\n");
    openDeviceName = juce::String();
    return false;
  }
  else  // SUCCESS!
  {
    // send current tuning automatically if its microtonal
    if (getTuning() > 1)
      sendTuning();
    // update Preferences with current port's name;
    juce::StringArray devices = getDevices();
    openDeviceName = devices[id];
    Preferences::getInstance()->setStringProp("MidiOutDevice", devices[id]);
    return true;
  }
}

bool MidiOutPort::isOpen(int id) 
{
  // nothing is open
  if (openDeviceId == -1) 
    return false;
  if (id == -1) // asking if ANY port is open
    return ((device != 0) || (openDeviceId == 0 && isInternalSynth));
  return (id == openDeviceId);
}

juce::String MidiOutPort::getOpenDeviceName()
{
  return openDeviceName;
}

void MidiOutPort::close(int id)
{
  if (device)
  {
    Preferences::getInstance()->setStringProp("MidiOutDevice", juce::String());
    //    std::cout << "deleting physical device (openDeviceId was " << openDeviceId << " and now is -1)\n";
    delete device;
    device = 0;
    openDeviceId = -1;
    openDeviceName = juce::String();
  }
  else if (isInternalSynth && (id == 0))
  {
    std::cout << "'clearing' internal synth device (openDeviceId was " << openDeviceId << " and now is -1)\n";
    Preferences::getInstance()->setStringProp("MidiOutDevice", juce::String());
    if (device) delete device;
    device = 0;
    openDeviceId = -1;
    openDeviceName = juce::String();
  }    
}

int MidiOutPort::performCommand(juce::CommandID id, int data, juce::String text)
{
  // lower eight bits of id encode command information
  juce::CommandID cmd = CommandIDs::getCommand(id);
  int arg = CommandIDs::getCommandData(id);  
  int res=0;

  switch (cmd)
  {
  case CommandIDs::MidiOutOpen:
    open(arg);
    break;

  case CommandIDs::MidiOutTest:
    testOutput();
    break;

  case CommandIDs::MidiOutHush :
    clear();
    break;

  case CommandIDs::SchedulerScoreComplete :
    if (captureSequence.getNumEvents() > 0)
    {
      saveSequence(false);
      clearSequence();
    }
    break;

  case CommandIDs::MidiOutTuning :
    setTuning(data, true);
    break;

  case CommandIDs::MidiOutPitchBend :
    setPitchBendWidth(arg);
    break;

  case CommandIDs::MidiOutDrumTrack :
    setAvoidDrumTrack(! getAvoidDrumTrack());
    break;

  case CommandIDs::MidiOutInstruments :
    break;

  case CommandIDs::MidiOutSetFile :
    setOutputFile(text);
    break;

   default:
    break;
  }
  return res;
}

void MidiOutPort::run() 
{
  double qtime, utime;
  MidiNode* node;
  while (true) 
  {
    if (threadShouldExit())
      break;
    while (true)
    {     
      {
        juce::ScopedLock mylock (outputNodes.getLock());
        node=outputNodes.getFirst();
      }
      if (node == NULL)
      {
	break;
      }
      qtime = node->time;
      utime = juce::Time::getMillisecondCounterHiRes() ;
      // this should probably test if the difference between qtime and
      // utime is less that 1ms, if not then it probably shouldn't
      // sleep (?)
      if ((qtime - utime) >= 1.5) 
      { 
	wait(1);
      }
      else 
      {
        juce::ScopedLock mylock (outputNodes.getLock());
        if (node->process())
        {
          outputNodes.remove(0,false);
          outputNodes.addSorted(*this, node); // reinsert at new time
        }
        else
        {
          outputNodes.remove(0, true);
        }
      }
    }
    wait(-1);
  }
}

bool MidiOutPort::isOutputQueueActive() 
{ 
  int n = outputNodes.size();
  return (n > 0);
}

void MidiOutPort::clear()
{
  outputNodes.clear();
  // avoid hanging notes
  /*
  if (device != 0)
    for (int i = 1; i <= 16; i++)
      device->sendMessageNow(juce::MidiMessage::allSoundOff(i));
  */
  if (isOpen())
  {
    for (int i = 1; i <= 16; i++)
    {
      juce::MidiMessage m = juce::MidiMessage::allSoundOff(i);
      sendOut(m);
    }
  }
}

void MidiOutPort::addNode(MidiNode *n) 
{
  n->midiOutPort = this;
  // MILLI
  n->time = (n->time * 1000.0) + juce::Time::getMillisecondCounterHiRes();
  {
    juce::ScopedLock mylock (outputNodes.getLock());
    outputNodes.addSorted(*this, n);
  }
  notify();
}

void MidiOutPort::sendNote(double wait, double duration, double keynum, 
			   double amplitude, double channel, bool toseq) 
{
  juce::jlimit( 0.0, 15.0, channel);
  // only microtune if current tuning is not semitonal and user's
  // channel is a microtonal channel.
  if ((getTuning() > 1) && (channel < microChanBlock)) 
  {
    // insure user's channel a valid microtonal channel, ie a channel
    // within the first zone of microtuned channels and then shift to
    // actual physical channel
    int chan = (((int)channel) % microChanCount) * microDivisions;
    // calculate integer midi key quantize user's keynum to microtonal
    // resolution size. this may round up to the next keynum. if it rounds up (or
    // down) to an integer keynum then we dont need to microtune
    keynum = cm_quantize(keynum, microIncrement);
    int key = (int)keynum;
    // only microtune if we have a fractional keynum
    if (keynum > key) 
    {
      // divide the fractional portion of keynum by the resolution size
      // to see which zone of channels its in
      int zone = (int)((keynum - key) / microIncrement);
      // shift channel to the appropriate zone and set keynum to integer
      chan += zone;
      if ((chan == 9) && avoidDrumTrack)
      {
        chan--;
      }
      channel = chan;
      keynum = key;
    }
  }
  
  if (toseq)
  {
    // JUCE channel message constructors are 1-based channels
    float amp = (float)((amplitude > 1.0) ? (amplitude/127) : amplitude);
    captureSequence.addEvent(juce::MidiMessage::noteOn((int)channel + 1, (int)keynum, amp), wait);
    captureSequence.addEvent(juce::MidiMessage::noteOff((int)channel + 1, (int)keynum), wait+duration);
    // don't call updatematchedpairs until the seq is used
  }
  else
  {
    // otherwise add it to the output queue
    addNode(new MidiNode(MidiOps::On, wait, (int)channel, keynum, amplitude, duration));
  }
}

void MidiOutPort::sendData(int type, double wait, double channel, 
			   double data1, double data2, bool toseq)
{
  if (toseq)
  {
    int ch = (int)channel;
    int d1 = (int)data1;
    int d2 = (int)data2;
    // JUCE channel message constructors are 1 based
    if (type == MidiOps::Off)
      captureSequence.addEvent(juce::MidiMessage::noteOff(ch + 1, d1), wait);
    else if (type == MidiOps::On)
      captureSequence.addEvent(juce::MidiMessage::noteOn(ch + 1, d1, (float)data2), wait);
    else if (type == MidiOps::Touch)
      captureSequence.addEvent(juce::MidiMessage::aftertouchChange(ch + 1, d1, d2), wait);
    else if (type == MidiOps::Ctrl)
      captureSequence.addEvent(juce::MidiMessage::controllerEvent(ch + 1, d1, d2), wait);
    else if (type == MidiOps::Prog)
      captureSequence.addEvent(juce::MidiMessage::programChange(ch + 1, d1), wait);
    else if (type == MidiOps::Press)
      captureSequence.addEvent(juce::MidiMessage::channelPressureChange(ch + 1, d1), wait);
    else if (type == MidiOps::Bend)
      captureSequence.addEvent(juce::MidiMessage::pitchWheel(ch + 1, d1), wait);
  }
  else 
  {

    if ( ((type == MidiOps::Off) || (type == MidiOps::On)) &&
         (getTuning() > 1) && (channel < microChanBlock) ) 
    {
      // insure user's channel a valid microtonal channel, ie a channel
      // within the first zone of microtuned channels and then shift to
      // actual physical channel
      int chan = (((int)channel) % microChanCount) * microDivisions;
      // calculate integer midi key and quantize user's keynum to
      // microtonal resolution size. this may round up to the next
      // keynum quantize keynumber to tuning resolution. if it rounds
      // up (or down) to an integer keynum then we dont need to
      // microtune
      data1 = cm_quantize(data1, microIncrement);
      int key = (int)data1;
      // only microtune if we have a fractional keynum
      if (data1 > key) 
      {
	// divide the fractional portion of keynum by the resolution size
	// to see which zone of channels its in
	int zone=(int)((data1 - key) / microIncrement);
	// shift channel to the appropriate zone and set keynum to integer
	chan += zone;
	if ((chan == 9) && avoidDrumTrack)
        {
	  chan--;
	}
	channel = chan;
	data1 = key;
      }
    }
    addNode(new MidiNode(type, wait, (int)channel, data1, data2));
  }
}

void MidiOutPort::testOutput () 
{
  // Tobias Kunze's little testing algo from cm 1.4 =:)
  double time = 0;
  for (int x = 0; x < 12; x++) 
  {
    double key = x + (12 * (3 + juce::Random::getSystemRandom().nextInt(5) )) ;
    double vel = 32.0 + juce::Random::getSystemRandom().nextInt(64);
    double dur = .1 + (juce::Random::getSystemRandom().nextInt(5) * .1);
    // add jiggle for microtuning testing...
    double jig= juce::Random::getSystemRandom().nextDouble();
    sendNote(time, dur, key + jig, vel, 0.0, false);
    time += ((juce::Random::getSystemRandom().nextInt(5) ^ 2) * (60.0 / 1000.0));
  }
}

///
/// Microtuning
///

/* tuning array (in cents, tunings X channels)

   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
   ----------------------------------------------
   1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
   2  0 50  0 50  0 50  0 50  0 50  0 50  0 50  0 50
   3  0 33 66  0 33 66  0 33 66  0 33 66  0 33 66  0
   4  0 25 50 75  0 25 50 75  0 25 50 75  0 25 50 75
   5  0 20 40 60 80  0 20 40 60 80  0 20 40 60 80  0
   6  0 16 33 50 66 83  0 16 33 50 66 83  0  0  0  0
   7  0 14 28 42 57 71 85  0 14 28 42 57 71 85  0  0
   8  0 12 25 37 50 62 75 87  0 12 25 37 50 62 75 87
   9  0 11 22 33 44 55 66 77 88  0  0  0  0  0  0  0
   10  0 10 20 30 40 50 60 70 80 90  0  0  0  0  0  0
   11  0  9 18 27 36 45 54 63 72 81 90  0  0  0  0  0
   12  0  8 16 25 33 41 50 58 66 75 83 91  0  0  0  0
   13  0  7 15 23 30 38 46 53 61 69 76 84 92  0  0  0
   14  0  7 14 21 28 35 42 50 57 64 71 78 85 92  0  0
   15  0  6 13 20 26 33 42 46 53 60 66 73 80 86 93  0
   16  0  6 12 18 25 31 37 43 50 56 62 68 75 81 87 93

   ;; this calculated the array contents
   (loop for div from 1 to 16 
   for zones = div
   for width = (floor (/ 16 div))
   for uses = (* width zones)
   do
   (let ((a (/ 1.0 div)))
   (format t "~:[{~;{{~]" (= div 1))
   (loop for c below uses by zones
   do< (loop for z below zones 
   do (format t "~:[, ~;~]~f" (= z c 0) (* z a))))
   (loop repeat (- 16 uses) do (format t ", ~f" 0.0))
   (format t "}~:[,~%~;};~%~]" (= div 16))))
*/

double MidiOutPort::channelTunings[16][16] =
  {{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
   {0.0, 0.5, 0.0, 0.5, 0.0, 0.5, 0.0, 0.5, 0.0, 0.5, 0.0, 0.5, 0.0, 0.5, 0.0, 0.5},
   {0.0, 0.33333334, 0.6666667, 0.0, 0.33333334, 0.6666667, 0.0, 0.33333334, 0.6666667, 0.0, 0.33333334, 0.6666667, 0.0, 0.33333334, 0.6666667, 0.0},
   {0.0, 0.25, 0.5, 0.75, 0.0, 0.25, 0.5, 0.75, 0.0, 0.25, 0.5, 0.75, 0.0, 0.25, 0.5, 0.75},
   {0.0, 0.2, 0.4, 0.6, 0.8, 0.0, 0.2, 0.4, 0.6, 0.8, 0.0, 0.2, 0.4, 0.6, 0.8, 0.0},
   {0.0, 0.16666667, 0.33333334, 0.5, 0.6666667, 0.8333334, 0.0, 0.16666667, 0.33333334, 0.5, 0.6666667, 0.8333334, 0.0, 0.0, 0.0, 0.0},
   {0.0, 0.14285715, 0.2857143, 0.42857146, 0.5714286, 0.71428573, 0.8571429, 0.0, 0.14285715, 0.2857143, 0.42857146, 0.5714286, 0.71428573, 0.8571429, 0.0, 0.0},
   {0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875},
   {0.0, 0.11111111, 0.22222222, 0.33333334, 0.44444445, 0.5555556, 0.6666667, 0.7777778, 0.8888889, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
   {0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.90000004, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
   {0.0, 0.09090909, 0.18181819, 0.27272728, 0.36363637, 0.45454547, 0.54545456, 0.6363636, 0.72727275, 0.8181819, 0.90909094, 0.0, 0.0, 0.0, 0.0, 0.0},
   {0.0, 0.083333336, 0.16666667, 0.25, 0.33333334, 0.4166667, 0.5, 0.5833334, 0.6666667, 0.75, 0.8333334, 0.9166667, 0.0, 0.0, 0.0, 0.0},
   {0.0, 0.07692308, 0.15384616, 0.23076925, 0.30769232, 0.3846154, 0.4615385, 0.53846157, 0.61538464, 0.6923077, 0.7692308, 0.84615386, 0.923077, 0.0, 0.0, 0.0},
   {0.0, 0.071428575, 0.14285715, 0.21428573, 0.2857143, 0.35714287, 0.42857146, 0.5, 0.5714286, 0.6428572, 0.71428573, 0.7857143, 0.8571429, 0.92857146, 0.0, 0.0},
   {0.0, 0.06666667, 0.13333334, 0.20000002, 0.26666668, 0.33333334, 0.40000004, 0.4666667, 0.53333336, 0.6, 0.6666667, 0.73333335, 0.8000001, 0.86666673, 0.9333334, 0.0},
   {0.0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375}};

int MidiOutPort::getTuning() {
  return microDivisions;
}

void MidiOutPort::setTuning(int tune, bool send) 
{
  // valid tunings are 1-16, interpreted as divisions per semitone.
  // each division of a tuning claims a physical midi channel tuned to
  // the division's cent value by sending a pitch bend. channels with
  // 0 cents are assignable by users.
  //FIXME???
  juce::jlimit(1, 16, tune);
  microDivisions = tune;  // tuning (1-16) = divisions per semitone.
  microIncrement = 1.0 / tune; // size of microdivision as fraction of 1
  microChanCount = (int)(16.0 / ((double)microDivisions)); // num user addressable microchans
  microChanBlock = microChanCount * microDivisions; // total num chans claimed
  //  printf("Tuning=%d, size=%f, chans=%d, claim=%d\n", microDivisions,
  //	 microIncrement, microChanCount, microChanBlock);
  if (send)
    sendTuning();
}

bool MidiOutPort::isCurrentTuning(int t) 
{
  //FIXME?
  // return true if current tuning
  juce::jlimit(1, 16, t);
  return (microDivisions == t);
}

int MidiOutPort::getTuningDivisions(int t) 
{
  //FIXME?
  juce::jlimit(1, 16, t);
  return t;
}

int MidiOutPort::getTuningChannels(int t) 
{
  // return number of addressable channels
  //return microChanCount;
  //FIXME?
  juce::jlimit(1, 16, t);
  return (int)(16.0 / ((double)t));
}

int MidiOutPort::getTuningChannelsClaimed(int t) 
{
  // return total number of physical channels claimed by tuning
  //FIXME?
  juce::jlimit(1, 16, t);
  return getTuningChannels(t) * getTuningDivisions(t);
}

juce::String MidiOutPort::getTuningName (int t) 
{
  // juce::jlimit(tune,1,16);
  return tuningNames[t - 1];
}

bool MidiOutPort::getAvoidDrumTrack() 
{
  return avoidDrumTrack;
}

void MidiOutPort::setAvoidDrumTrack(bool b) 
{
  avoidDrumTrack = b;
}

int MidiOutPort::getPitchBendWidth() 
{
  return pitchbendwidth;
}

void MidiOutPort::setPitchBendWidth(int b) 
{
  if (b < 1) b = 1;
  pitchbendwidth = b;
}

void MidiOutPort::sendTuning() 
{
  int tunerow = getTuning() - 1;
  for (int c = 0; c < 16; c++)
  {
    int b = (int)round(cm_rescale(channelTunings[tunerow] [c], 
                                  -pitchbendwidth, pitchbendwidth,
                                  0, 
                                  16383,
                                  1));
    //device->sendMessageNow( juce::MidiMessage::pitchWheel(c+1,bendval));
    // FIXME: I THINK THIS IS LEAKING MEMORY
    ////    juce::MidiMessage* m=new juce::MidiMessage((0xe0 | c), (b & 127), ((b >> 7) & 127));
    ////    addNode(new MidiNode(m));
    addNode(new MidiNode(MidiOps::Bend, 0, c, b));
  }
}

void MidiOutPort::getTuningValues(juce::Array<int>& vals)
{
  int tunerow = getTuning()-1;
  for (int chan = 0; chan < 16; chan++)
    vals.add((int)round(cm_rescale(channelTunings[tunerow] [chan], 
				    -pitchbendwidth, 
				    pitchbendwidth,
				    0,
				    16383,
				    1)));
}

/*=======================================================================*
  Instrument Assignment
  *=======================================================================*/

int MidiOutPort::getInstrument(int chan) 
{
  return programChanges[chan];
}

void MidiOutPort::setInstrument(int chan, int prog) 
{
  programChanges[chan] = prog;
}

void MidiOutPort::getInstruments(juce::Array<int>& vals)
{
  for (int chan = 0; chan < 16; chan++)
    vals.add(getInstrument(chan));
}
 
void MidiOutPort::setInstruments(juce::Array<int>& vals, bool send) 
{
  for (int i = 0; i < vals.size(); i++)
    if (vals[i] >= 0 && vals[i] <= 127)
      setInstrument(i, vals[i]);
  if (send)
    sendInstruments();
}

void MidiOutPort::resetInstruments()
{
  for (int c = 0; c < 16; c++)
    programChanges[c] = 0;
}

bool MidiOutPort::isInstrumentChannel(int chan)
{
  // true if physical chan accepts instruments (ie is not a channel
  // claimed by microtuning)
  return (0.0 == channelTunings[getTuning()-1][chan]);
}

void MidiOutPort::sendInstruments() 
{
  if (1)// (isOpen())
  {
    //    std::cout << "sending instruments\n"; 
    for (int c = 0; c < 16; c++)
    {
      // FIXME: I THINK THIS IS LEAKING MEMORY
      ////      juce::MidiMessage* m=new juce::MidiMessage((0xc0 | c), programChanges[c]);
      ////      addNode(new MidiNode(m));
      addNode(new MidiNode(MidiOps::Prog, 0, c, programChanges[c]));
    }
  }
}

/*=======================================================================*
  Midifile Writing
  *=======================================================================*/


bool MidiOutPort::isSequenceEmpty()
{
  return (captureSequence.getNumEvents() == 0);
}

void MidiOutPort::clearSequence()
{
  captureSequence.clear();
}

void MidiOutPort::setOutputFile(juce::String filename)
{
  sequenceFile.file=completeFile(filename);
}

void MidiOutPort::saveSequence(bool ask)
{
  if (isSequenceEmpty()) 
    return;
  sequenceFile.bends = (getTuning() > 1) ? true : false;

  if (ask )//|| (sequenceFile.file==File::nonexistent)
  {
    juce::FileChooser choose ("Save Recording", juce::File::getCurrentWorkingDirectory(), "*.mid");
    if (choose.browseForFileToSave(false))
      setOutputFile(choose.getResult().getFullPathName());
    else
      return;
  }
  
  captureSequence.updateMatchedPairs();
  if (sequenceFile.file.existsAsFile())
    sequenceFile.file.deleteFile();
  
  juce::FileOutputStream outputStream(sequenceFile.file);
  juce::MidiFile* midifile = new juce::MidiFile();

  if (sequenceFile.ismsec)
    midifile->setSmpteTimeFormat(25, 40); // set to 1 msec resolution
  else
    midifile->setTicksPerQuarterNote(sequenceFile.qticks);

  juce::MidiMessageSequence track0;
  track0.addEvent( sequenceFile.getTempoMessage(), 0);
  track0.addEvent( sequenceFile.getTimeSigMessage(), 0);
  track0.addEvent( sequenceFile.getKeySigMessage(), 0);

  // add optional tuning data to track 0
  juce::Array<int> data;
  if (sequenceFile.bends)
  {
    getTuningValues(data);
    for (int c = 0; c < data.size(); c++)
      track0.addEvent(juce::MidiMessage((0xe0 | c), (data[c] & 127), 
                                        ((data[c] >> 7) & 127)));
  }
  // add optional program changes to track 0
  if (sequenceFile.insts)
  {
    data.clear();
    getInstruments(data);
    for (int c = 0; c < data.size(); c++)
      track0.addEvent(juce::MidiMessage::programChange(c + 1, data[c]));
  }  

  midifile->addTrack(track0);
  midifile->addTrack(captureSequence);

  const juce::MidiMessageSequence* seq = midifile->getTrack(1);
  if (sequenceFile.ismsec)
  {
    // If writing milliseconds then convert the file's data from
    // seconds to milliseconds to match our smpte time format
    for (int i = 0; i < seq->getNumEvents(); i++)
    {
      juce::MidiMessageSequence::MidiEventHolder* h = seq->getEventPointer(i);
      h->message.setTimeStamp(h->message.getTimeStamp() * 1000.0);
    }
  }
  else
  {
    // otherwise convert seconds to division per quarter
    double divs = (float)sequenceFile.qticks;
    for (int i = 0; i < seq->getNumEvents(); i++)
    {
      juce::MidiMessageSequence::MidiEventHolder* h = seq->getEventPointer(i);
      h->message.setTimeStamp(h->message.getTimeStamp() * divs);
    }
  }

  // optional clear after save
  //midifile->writeTo(outputStream) && sequenceFile.clear
  if (midifile->writeTo(outputStream)) 
  {
    juce::String msg ("Midifile: ");
    msg << sequenceFile.file.getFullPathName() << "\n";
    Console::getInstance()->printOutput((msg));
    captureSequence.clear();
  }
}

/*=======================================================================*
  Midi In Port
  *=======================================================================*/

MidiInPort::MidiInPort()
  : openDeviceId (-1),
    device (0),
    openDeviceName (""),
    tracing (0),
    channelMask(0),
    opcodeMask(0)
{
  juce::String name = Preferences::getInstance()->getStringProp("MidiInDevice");
  channelMask = Preferences::getInstance()->getIntProp("MidiInChannelMask", AllChannels);
  opcodeMask = Preferences::getInstance()->getIntProp("MidiInOpcodeMask", AllOpcodes);

  // On Mac we attempt to open the default input port at startup. This
  // is a last ditch effort to stop the apparent, intermittent
  // CoreMidi bug that crashes the MidiServer if juce::getDevices() is
  // called without an actual midi device open.
  if (name.isNotEmpty())
    open(name);
#ifdef JUCE_MAC
  else if(getDevices().size() > 0)
  {
    int index = juce::MidiInput::getDefaultDeviceIndex();
    open(index);
  }
#endif
}

MidiInPort::~MidiInPort() 
{
  //close();
  // DOES NOT CALL CLOSE SO THE LAST PORT SETTING IS STILL VALID FOR PREFS
  if (device != NULL)
    delete device;
}

const juce::StringArray MidiInPort::getDevices()
{
  return juce::MidiInput::getDevices();
}

bool MidiInPort::open(juce::String name) 
{
  juce::StringArray devices = getDevices();
  int id = -1;
  for (int i = 0; i < devices.size(); i++)
    if (devices[i] == name)
    {
      id = i;
      break;
    }
  if (id == -1)
  {
    Preferences::getInstance()->setStringProp("MidiInDevice", juce::String());
    Console::getInstance()->printWarning("Warning: Midi input device " + name + " does not exist.\n");
    return false;
  }
  else
    return open(id);
}

bool MidiInPort::open(int id)
{
  // dont do anything if opening same port
  if (id == openDeviceId)
    return true;
  if (device != 0)
    close();
  // HKT FIXME
  device = juce::MidiInput::openDevice(id, this).release();
  if (device == 0)
  {
    openDeviceName = juce::String();
    Preferences::getInstance()->setStringProp("MidiInDevice", juce::String());
    Console::getInstance()->printError(">>> Error: Failed to open midi input device "  + juce::String(id) + ".\n");
    return false;
  }
  else  // SUCCESS!
  {
    openDeviceId = id;
    device->start();
    juce::StringArray devices = getDevices();
    //    std::cout << "updating prefs with " << devices[id] << "\n";
    openDeviceName = devices[id];
    Preferences::getInstance()->setStringProp("MidiInDevice", devices[id]);
    // FIXME: DO I UPDATE AUDIO MANAGER TOO??
    return true;
  }
}

bool MidiInPort::isOpen(int id)
{
  if (!device)
    return false;
  else if (id == -1) // asking if ANY output device is open
    return true;
  else
    return (id == openDeviceId); // asking if specific dev is open
}

juce::String MidiInPort::getOpenDeviceName()
{
  return openDeviceName;
}

void MidiInPort::close(int id)
{
  if (device)
  {
    //      std::cout << "midIIN.CLOSE(), removing MidiINDevice from prefs\n" ;
    Preferences::getInstance()->setStringProp("MidiInDevice", juce::String());
    device->stop();
    delete device;
    device = 0;
    openDeviceId = -1;
    openDeviceName = juce::String();
  }
}

//
/// Channel Testing 
//

int MidiInPort::getInputChannelMask()
{
  return channelMask;
}

void MidiInPort::setInputChannelMask(int mask)
{
  channelMask=mask;
  Preferences::getInstance()->setIntProp("MidiInChannelMask", channelMask);
}

bool MidiInPort::isInputChannelActive(int chan)
{
  chan=juce::jlimit(0, 15, chan);
  int mask=1 << chan;
  return Flags::test(channelMask, mask);
}

void MidiInPort::setInputChannelActive(int chan, bool isActive)
{
  chan = juce::jlimit(0, 15, chan);  
  int mask = 1 << chan;
  if (isActive)
    Flags::setOn(channelMask, mask);
  else
    Flags::setOff(channelMask, mask);
  Preferences::getInstance()->setIntProp("MidiInChannelMask", channelMask);
}

//
/// Opcode Masking
//

int MidiInPort::getOpcodeMask()
{
  return opcodeMask;
}

void MidiInPort::setOpcodeMask(int mask)
{
  opcodeMask=mask;
  Preferences::getInstance()->setIntProp("MidiInOpcodeMask", opcodeMask);
}

//
/// Opcode Testing
//

bool MidiInPort::isOpcodeActive(int val, bool isop)
{
  if (isop)
    val = val - MidiOps::Off; //MidiNode::opcodeToIndex(val);
  int index=juce::jlimit(0, 6, val);
  int mask=1 << index;
  return Flags::test(opcodeMask, mask);
}

void MidiInPort::toggleOpcodeActive(int index)
{
  index=juce::jlimit(0, 6, index);
  int mask=1 << index;
  Flags::toggle(opcodeMask, mask);
  Preferences::getInstance()->setIntProp("MidiInOpcodeMask", opcodeMask);
}

void MidiInPort::setOpcodeActive(int index, bool active)
{
  index=juce::jlimit(0, 6, index);
  int mask=1 << index;
  if (active)
    Flags::setOn(opcodeMask, mask);
  else
    Flags::setOff(opcodeMask, mask);
  Preferences::getInstance()->setIntProp("MidiInOpcodeMask", opcodeMask);
}


//
/// Traceing
//

bool MidiInPort::isTracing()
{
  return tracing;
}

void MidiInPort::toggleTracing()
{
  tracing=!tracing;
}

void MidiInPort::setTracing(bool trace)
{
  tracing=trace;
}

//
/// Receiving
//

bool MidiInPort::isInputMessageActive(const juce::MidiMessage &msg)
{
  int ch = msg.getChannel(); // JUCE: channel messages are 1 based
  return ((ch > 0) && isInputChannelActive(ch - 1) &&
	  ((msg.isNoteOn() && isOpcodeActive(MidiOps::On, true)) ||
	   (msg.isNoteOff() && isOpcodeActive(MidiOps::Off, true)) ||
	   (msg.isController() && isOpcodeActive(MidiOps::Ctrl, true)) ||
	   (msg.isProgramChange() && isOpcodeActive(MidiOps::Prog, true)) ||
	   (msg.isPitchWheel() && isOpcodeActive(MidiOps::Bend, true)) ||
	   (msg.isAftertouch() && isOpcodeActive(MidiOps::Touch, true)) ||
	   (msg.isChannelPressure() && isOpcodeActive(MidiOps::Press, true))));
}

void MidiInPort::handleIncomingMidiMessage(juce::MidiInput *dev, const juce::MidiMessage &msg) 
{
  if (isInputMessageActive(msg))
  {

    SchemeThread::getInstance()->midiin(msg);
    //      if (isThroughActive() && MidiOutPort::getInstance()->device != NULL)
    //        MidiOutPort::getInstance()->device->sendMessageNow(msg);
    if (isTracing())
    {
      int op = (msg.getRawData()[0] & 0xf0) >> 4;
      // JUCE: channel numbers are 1 based
      int ch = msg.getChannel() - 1;
      int sz = msg.getRawDataSize();
      int d1 = msg.getRawData()[1] & 0x7f;
      int d2 = 0;
      if (sz > 2)
        d2 = msg.getRawData()[2] & 0x7f;
      // convert MidiOns with zero velocity to MidiOffs
      if ((op == MidiOps::On) && (d2 == 0))
        op = MidiOps::Off;
      juce::String text = MidiOps::toString(op);
      text << " " << juce::String(ch)
           << " " << juce::String(d1)
           << " " << juce::String(d2)
           << "\n";
      Console::getInstance()->printOutput(text);
    }
  }
}

void MidiInPort::handlePartialSysexMessage(juce::MidiInput *dev,
					   const juce::uint8 *data, 
					   const int num, 
					   const double time)
{
}

/*=======================================================================*
  Midi Device Dialog
  *=======================================================================*/

struct MidiDeviceSettings : public juce::Component,
public juce::Button::Listener
{
  juce::GroupComponent* inGroup;
  juce::GroupComponent* outGroup;
  juce::Label* messageLabel;
  juce::Array<juce::Component*> ins;
  juce::Array<juce::Component*> outs;
  int maxDevices;
  int maxInWidth;
  int maxOutWidth;
  MidiDeviceSettings()
    : inGroup(0),
      outGroup(0),
      messageLabel(0),
      maxDevices(0),
      maxInWidth(0),  // determined in AddDevices
      maxOutWidth(0)
  {
    setVisible(true);
    // Input devices
    addAndMakeVisible(inGroup = new juce::GroupComponent("","Input Connections"));
    juce::StringArray indevs = MidiInPort::getInstance()->getDevices();
    juce::String inopen = MidiInPort::getInstance()->getOpenDeviceName();
    addDevices(true, indevs, inopen); // add input device buttons

    // Output Devices
    addAndMakeVisible(outGroup = new juce::GroupComponent("", "Output Connections"));
    juce::StringArray outdevs = MidiOutPort::getInstance()->getDevices();
    juce::String outopen = MidiOutPort::getInstance()->getOpenDeviceName();
    addDevices(false, outdevs, outopen); // add output device buttons
    // height depends on max size of inputs and outputs list
    maxDevices = juce::jmax(indevs.size(), outdevs.size());
    // width is at least 400 px.
    int width = juce::jmax(juce::jmax(maxInWidth, maxOutWidth) * 3, 400);
    int height = 8 + 16 + (maxDevices * 24) + 0 + 8;
#ifdef JUCE_MAC
    {
      bool iac = false;
      for (int i = 0; i < outdevs.size() && !iac; i++)
      {
        if (outdevs[i].startsWith("IAC Driver"))
          iac = true;
      }
      if (!iac)
      {
        juce::String msg ("To connect MIDI with other Apps activate your Mac's IAC Driver Bus. See: https://www.ableton.com/en/articles/using-virtual-MIDI-buses-live/");
        juce::Font font (12.0f);
        addAndMakeVisible(messageLabel = new juce::Label("", msg));
        messageLabel->setFont(font);
        // messageLabel->setColour(juce::Label::backgroundColourId, juce::Colours::blue);
        // height is 2 lines at font size, width is window less margins
        messageLabel->setSize(width - (8 + 8), font.getHeight()*3); 
        height += (messageLabel->getHeight()); // in add padding below group line and two line message
      }
    }
#endif
    // width is larger of 3 times widest label and 400px
    //    std::cout << "size=" << getWidth() << ", " << getHeight() << "\n";
    setSize(width, height);
  }

  ~MidiDeviceSettings()
  {
    ins.clear();
    outs.clear();
    deleteAllChildren();
  }

  void addDevices (bool isInput, juce::StringArray& devs, juce::String& open)
  {
    juce::Font font (15.0f);
    juce::Array<Component*>& comps = (isInput) ? ins : outs;
    int radio = (isInput) ? 1 : 2;
    int& maxWidth = (isInput) ? maxInWidth : maxOutWidth;
    if (devs.size() > 0)
    {
      for (int i = 0; i < devs.size(); i++)
      {
        juce::String s = devs[i];
        juce::ToggleButton* b = new juce::ToggleButton(s);
        addAndMakeVisible(b);
        comps.add(b);
        b->setSize(font.getStringWidth(s) + 24 + 8, 24);
        b->setRadioGroupId(radio);
        if (open == s)
          b->setToggleState(true, juce::dontSendNotification);
        b->addListener(this);
        maxWidth = juce::jmax(maxWidth, b->getWidth());
      }
    }
    else
    {
      juce::String s ("<no connections>");
      juce::Label* l = new juce::Label(s, s);
      l->setEnabled(false);
      addAndMakeVisible(l);
      comps.add(l);
      l->setSize(font.getStringWidth(s) + 8, 24);
      maxWidth = juce::jmax(maxWidth, l->getWidth());
    }
  }

  void resized()
  {
    int m = 8;
    int x = m;
    int y = m;
    int w = (getWidth() / 2) - (m * 2); // width of groups
    int h = 16 + (maxDevices * 24) + 8;
    inGroup->setBounds(x, y, w, h);
    // center device list inside group rect
    x = inGroup->getX() + ((inGroup->getWidth() - maxInWidth) / 2);
    y = inGroup->getY() + 16;
    for (int i = 0; i < ins.size(); i++)
    {
      ins[i]->setTopLeftPosition(x, y);
      y += 24;
    }
    // move x to output group loc
    x = getWidth() / 2 + m;
    y = m;
    outGroup->setBounds(x, y, w, h);
    // center device list inside group rect
    x = outGroup->getX() + ((outGroup->getWidth() - maxOutWidth) / 2);
    y = outGroup->getY() + 16;
    for (int i = 0; i < outs.size(); i++)
    {
      outs[i]->setTopLeftPosition(x, y);
      y += 24;
    }
    // Display possible 2 line message
    if (messageLabel)
    {
      x = m;
      y = outGroup->getBottom() + 0;
      messageLabel->setTopLeftPosition(x, y);
    }
  }

  void buttonClicked(juce::Button* b)
  {
    juce::String s = b->getButtonText();
    //    std::cout << "buttonClicked: " << s
    //              << " toggleState: " << b->getToggleState() << "\n";
    if (ins.contains(b))
    {
      if (MidiInPort::getInstance()->getOpenDeviceName() != s)
      {
        //        std::cout << "Opening input port " << s << "\n";
        MidiInPort::getInstance()->open(s);
        Grace::getApp().refreshMenuBar();
      }
    }
    else
    {
      if (MidiOutPort::getInstance()->getOpenDeviceName() != s)
      {
        //        std::cout << "Opening output port " << s << "\n";
        MidiOutPort::getInstance()->open(s);
        Grace::getApp().refreshMenuBar();
      }
    }
  }
};

void MidiOutPort::openDeviceSettings ()
{
  //  new MidiInstrumentsWindow(new InstrumentView(this));
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "Midi Connections";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.setOwned(new MidiDeviceSettings());
  dw.launchAsync();
}

/*=======================================================================*
  Instrument Dialog
  *=======================================================================*/

class GMAssignmentList : public juce::ListBox,
			 public juce::ListBoxModel
{
public:
  MidiOutPort *port;
  int instruments[16];
  bool changed;

  GMAssignmentList(MidiOutPort *p) 
  : juce::ListBox( "Assignment List", 0),
    port (0),
    changed(false)
  {
    port=p;
    setModel(this);
    for (int c=0;c<16;c++)
      instruments[c]=port->programChanges[c];
  }

  ~GMAssignmentList() {}

  bool isChanged() {return changed;}

  bool setChanged(bool b) {changed=b; return changed;}

  void setInstrument(int chan, int inst) 
  {
    // chan is a physcial channel 0-15
    instruments[chan]=inst;
    chan++;
    // now set all the microtuned channels to the same instrument!
    while ( (chan<16) && !port->isInstrumentChannel(chan) )
    {
      instruments[chan]=inst;
      chan++;
    }
  }

  int rowToChannel(int r)
  {
    // return physical channel number of row
    int t=port->getTuning();
    if ( r < port->getTuningChannels(t) )
      return r * port->getTuningDivisions(t);
    else
      return port->getTuningChannelsClaimed(t) +
	(r - port->getTuningChannels(t));
  }

  int getNumRows()
  {
    int num=0;
    // any channel with a zero tuning value is an assignable channel
    for (int c=0; c<16; c++)
      if ( port->isInstrumentChannel(c) )
	num++;
    return num;
  }
  
  /*  void paintRowBackground(juce::Graphics& g, int rowNumber,
                          int width, int height, bool rowIsSelected)
  {
    if (rowIsSelected)
      g.fillAll(juce::Colours::lightgoldenrodyellow); 
      }*/

  void paintListBoxItem (int rowNumber, juce::Graphics& g, int width, int height,
			 bool rowIsSelected)
  {
    if (rowIsSelected)
      g.fillAll(juce::Colour(juce::Colours::lightgoldenrodyellow)); //juce::Colours::lightgoldenrodyellow
    g.setColour (juce::Colours::black);
    ////    g.setFont (height * 0.7f);
    
    int tune=port->getTuning(), chan;
    juce::String chantext, tunetext, insttext;
    // set chantext to microchannel number or to physcial channel number
    // if channel is above them
    if (rowNumber < port->getTuningChannels(tune))
    {
      chantext=juce::String(rowNumber);
      tunetext=port->getTuningName(tune);
      // actual channel number
      chan=rowNumber*port->getTuningDivisions(tune);
    }
    else 
    { 
      // row is above microchannels, show its physical channel number
      // and semitonal tuning. physical channels start after the last
      // micochannel used.
      chan=port->getTuningChannelsClaimed(tune) +
        (rowNumber - port->getTuningChannels(tune));
      chantext=juce::String(chan);
      tunetext=port->getTuningName(1);
    }    
    insttext=juce::MidiMessage::getGMInstrumentName(instruments[chan]);
    // these positions need to be kept in same position as the labels
    // in component view
    g.drawText (chantext, 5, 0, 56, height, juce::Justification::centred,true);
    g.drawText (tunetext, 70-5, 0, 80, height, juce::Justification::centredLeft,
		true);
    g.drawText (insttext, 156, 0, width-156, height, 
		juce::Justification::centredLeft,true);
  }

  void revertInstruments()
  {
    for (int c=0;c<16;c++)
      instruments[c]=port->programChanges[c];
  }
  
  void saveInstruments()
  {
    for (int c=0;c<16;c++)
      port->programChanges[c]=instruments[c];
  }
};

class GMInstrumentList : public juce::ListBox, 
			 public juce::ListBoxModel
{
public:
  GMAssignmentList *assignments;

  GMInstrumentList(GMAssignmentList *list)
  : juce::ListBox( "GM Instrument List", 0),
    assignments (0)
  {
    assignments=list;
    setModel(this);
  }

  ~GMInstrumentList() {} 

  int getNumRows() {return 128;}

  void paintListBoxItem (int rowNumber, juce::Graphics& g, int width, int height,
			 bool rowIsSelected)
  {
    if (rowIsSelected)
      //      g.fillAll(juce::Colours::lightgoldenrodyellow);
      g.fillAll(juce::Colour((juce::uint8)17,(juce::uint8)17,(juce::uint8)238,(juce::uint8)64));
    g.setColour (juce::Colours::black);
    g.setFont (height * 0.7f);
    g.drawText (juce::MidiMessage::getGMInstrumentName(rowNumber) ,
		5, 0, width, height,
		juce::Justification::centredLeft, true);
  }
  void listBoxItemClicked(int row, const juce::MouseEvent &e) ;
};

//
// Instrument Dialog View
//

class InstrumentView : public juce::Component, public juce::Button::Listener
{
public:
  MidiOutPort *port;
  juce::Label *label1, *label2, *label3, *label4;
  GMAssignmentList *assignments;
  GMInstrumentList *instruments;
  juce::TextButton *button1, *button2;

  InstrumentView(MidiOutPort *p)
  {
    port=p;
    label1=new juce::Label(juce::String(), "Channel");
    label2=new juce::Label(juce::String(), "Tuning");
    label3=new juce::Label(juce::String(), "Instrument");
    label4=new juce::Label(juce::String(), "Instruments");
    juce::Font f=label1->getFont();
    f.setBold(true);
    label1->setFont(f);
    label2->setFont(f);
    label3->setFont(f);
    label4->setFont(f);
    addAndMakeVisible(label1 );
    addAndMakeVisible(label2 );
    addAndMakeVisible(label3 );
    addAndMakeVisible(label4 );
    addAndMakeVisible(assignments=new GMAssignmentList(p));
    assignments->setColour(juce::ListBox::outlineColourId, juce::Colours::grey);
    assignments->setOutlineThickness(1);
    addAndMakeVisible(instruments=new GMInstrumentList(assignments));
    instruments->setColour(juce::ListBox::outlineColourId, juce::Colours::grey);
    instruments->setOutlineThickness(1);
    //    addAndMakeVisible(button1=new juce::TextButton("Revert") );
    //    button1->addButtonListener(this);
    // revert button initially disabled
    //    button1->setEnabled(false);
    addAndMakeVisible(button2=new juce::TextButton("Send") );
    button2->addListener(this);
    setSize(10 + 320 + 10 + 170 + 10, 450);
    setVisible(true);
  }

  ~InstrumentView() 
  {
    port=0;
    deleteAndZero(label1);
    deleteAndZero(label2);
    deleteAndZero(label3);
    deleteAndZero(label4);
    deleteAndZero(instruments);
    deleteAndZero(assignments);
    //    deleteAndZero(button1);
    deleteAndZero(button2);
  }

  void resized() 
  {
    int asslistwidth=320;
    int inslistwidth=170;
    // 
    label1->setBounds(10, 10, 56, 24); // Channel
    label2->setBounds(70, 10, 72, 24); // Tuning
    label3->setBounds(160, 10, 78, 24); // Instrument
    label4->setBounds(10+320+10, 8, inslistwidth, 24);
    // list boxes
    assignments->setBounds(10, 30, asslistwidth, 
			   assignments->getRowHeight()*16);
    instruments->setBounds(10 + 320 + 10,
			   30,
			   inslistwidth,
			   instruments->getRowHeight()*16);
    int mid=getWidth()/2;
    //    button1->setSize(60,24);
    //    button1->setTopLeftPosition(mid - button1->getWidth() - 16,
    //				assignments->getBottom() + 24);
    button2->setSize(100,24);
    button2->setTopLeftPosition(mid-50, assignments->getBottom() + 20);
  }

  void buttonClicked (juce::Button *button) ;

};

// these are the mouse down functions

void GMInstrumentList::listBoxItemClicked(int row, const juce::MouseEvent &e) 
{
  // row == GM instrument number
  int inst=row;
  int arow=assignments->getSelectedRow(); // selected row in assignments list
  if (arow<0) return;
  int chan=assignments->rowToChannel(arow);
  assignments->setInstrument(chan,inst);
  assignments->repaintRow(arow);
  // Mark assignment list as changed and enable Revert button
  assignments->setChanged(true);
  //  ((InstrumentView *)getParentComponent())->button1->setEnabled(true);
}

void InstrumentView::buttonClicked (juce::Button *button) 
{
  if ( button->getName() == "Send" ) 
  {
    //printf("SENDING!\n");
    // copy/send program changes, clear changed flag, deselect all
    // rows in list boxes and disable Revert button
    assignments->saveInstruments();
    port->sendInstruments();
    assignments->setChanged(false);
    assignments->deselectAllRows();
    instruments->deselectAllRows();
    //      button1->setEnabled(false);
  }
  else if ( button->getName() == "Revert" )
  {
    // restore Midi Port's program changes, clear changed flag,
    // deselect all rows in list boxes and disable Revert button
    assignments->revertInstruments();
    assignments->setChanged(false);
    assignments->deselectAllRows();
    //assignments->updateContent();
    for (int r=0;r<assignments->getNumRows(); r++)
      assignments->repaintRow(r);
    instruments->deselectAllRows();
    //      button1->setEnabled(false);
  }
}

void MidiOutPort::openInstrumentsDialog ()
{
  //  new MidiInstrumentsWindow(new InstrumentView(this));
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "Midi Instruments";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.setOwned(new InstrumentView(this));
  dw.launchAsync();
}

/*=======================================================================*
  MidiFile Settings Dialog
  *=======================================================================*/

class MidiFileInfoComponent : public juce::Component,
			      public juce::FilenameComponentListener,
public juce::ComboBox::Listener,
public juce::Slider::Listener,
public juce::Button::Listener
{
public:
  MidiFileInfoComponent (MidiFileInfo* info);
  ~MidiFileInfoComponent();
  //  void paint (juce::Graphics& g);
  void resized();
  void comboBoxChanged (juce::ComboBox* comboBoxThatHasChanged);
  void sliderValueChanged (juce::Slider* sliderThatWasMoved);
  void buttonClicked (juce::Button* buttonThatWasClicked);
  bool parseTimeSig(int& numer, int& denom);
  void filenameComponentChanged(juce::FilenameComponent* f) ;
private:
  MidiFileInfo* midifileinfo;
  juce::FilenameComponent* fileeditor;
  juce::Label* timelabel;
  juce::ToggleButton* millibutton;
  juce::ToggleButton* ticksbutton;
  juce::Slider* ticksslider;
  juce::ComboBox* keysigmenu;
  juce::Label* keysiglabel;
  juce::Label* filelabel;
  juce::Slider* temposlider;
  juce::Label* timesiglabel;
  juce::Label* tempolabel;
  juce::TextButton* cancelbutton;
  juce::TextEditor* timesigeditor;
  juce::ToggleButton* tuningbutton;
  juce::ToggleButton* instrumentbutton;
  MidiFileInfoComponent (const MidiFileInfoComponent&);
  const MidiFileInfoComponent& operator= (const MidiFileInfoComponent&);
};

bool MidiFileInfoComponent::parseTimeSig (int& n, int& d)
{
  juce::String str=timesigeditor->getText();
  int pos=str.indexOfChar('/');
  if (pos<0) return false;
  int a = str.substring(0,pos).getIntValue();
  if (a<1) return false;
  int b = str.substring(pos+1).getIntValue();
  if (b<1) return false;
  n=a;
  d=b;
  return true;
}

MidiFileInfoComponent::MidiFileInfoComponent(MidiFileInfo* info)
  : midifileinfo (0),
    fileeditor (0),
    timelabel (0),
    millibutton (0),
    ticksbutton (0),
    ticksslider (0),
    keysigmenu (0),
    keysiglabel (0),
    filelabel (0),
    temposlider (0),
    timesiglabel (0),
    tempolabel (0),
    cancelbutton (0),
    timesigeditor (0),
    tuningbutton (0),
    instrumentbutton (0)
{
  midifileinfo = info;
  juce::File file = midifileinfo->file;
  if (file == juce::File())
    file = juce::File::getSpecialLocation(juce::File::userHomeDirectory).getChildFile("test.mid");
  // time format row
  addAndMakeVisible(timelabel = new juce::Label("", "Time format:"));
  timelabel->setFont(juce::Font (15.0000f, juce::Font::plain));
  timelabel->setJustificationType(juce::Justification::centredLeft);
  timelabel->setEditable(false, false, false);
  
  addAndMakeVisible(millibutton = new juce::ToggleButton (""));
  millibutton->setButtonText("Milliseconds");
  millibutton->setRadioGroupId(1);
  
  millibutton->addListener(this);
  
  addAndMakeVisible(ticksbutton = new juce::ToggleButton (""));
  ticksbutton->setButtonText("Ticks per Quarter:");
  ticksbutton->setRadioGroupId(1);
  ticksbutton->addListener(this);
  
  addAndMakeVisible(ticksslider = new juce::Slider (""));
  ticksslider->setRange (0, 4000, 2);
  ticksslider->setSliderStyle (juce::Slider::LinearHorizontal);
  ticksslider->setTextBoxStyle (juce::Slider::TextBoxLeft, false, 40, 24);
  ticksslider->setValue(info->qticks);
  ticksslider->addListener(this);
  
  if (info->ismsec)
  {
    millibutton->setToggleState(true, juce::dontSendNotification);
    ticksbutton->setToggleState(false, juce::dontSendNotification);
    ticksslider->setEnabled(false);
  }
  else
  {
    millibutton->setToggleState(false, juce::dontSendNotification);
    ticksbutton->setToggleState(true, juce::dontSendNotification);
    ticksslider->setEnabled(true);
  }  
  
  addAndMakeVisible (keysigmenu = new juce::ComboBox ("keysigmenu"));
  keysigmenu->setEditableText (false);
  keysigmenu->setJustificationType (juce::Justification::centredLeft);
  keysigmenu->setTextWhenNothingSelected (juce::String());
  keysigmenu->setTextWhenNoChoicesAvailable ("(no choices)");
  keysigmenu->addItem ("7 Flats", 1);
  keysigmenu->addItem ("6 Flats", 2);
  keysigmenu->addItem ("5 Flats", 3);
  keysigmenu->addItem ("4 Flats", 4);
  keysigmenu->addItem ("3 Flats", 5);
  keysigmenu->addItem ("2 Flats", 6);
  keysigmenu->addItem ("1 Flat", 7);
  keysigmenu->addItem ("None", 8);
  keysigmenu->addItem ("1 Sharp", 9);
  keysigmenu->addItem ("2 Sharps", 10);
  keysigmenu->addItem ("3 Sharps", 11);
  keysigmenu->addItem ("4 Sharps", 12);
  keysigmenu->addItem ("5 Sharps", 13);
  keysigmenu->addItem ("6 Sharps", 14);
  keysigmenu->addItem ("7 Sharps", 15);
  keysigmenu->setSelectedId(info->keysig);
  keysigmenu->addListener (this);
  
  addAndMakeVisible (keysiglabel = new juce::Label ("keysiglabel", "Key Signature:"));
  keysiglabel->setFont (juce::Font (15.0000f, juce::Font::plain));
  keysiglabel->setJustificationType (juce::Justification::centredLeft);
  keysiglabel->setEditable (false, false, false);
  
  addAndMakeVisible (temposlider = new juce::Slider ("temposlider"));
  temposlider->setRange (40, 208, 2);
  temposlider->setSliderStyle (juce::Slider::LinearHorizontal);
  temposlider->setTextBoxStyle (juce::Slider::TextBoxLeft, false, 40, 24);
  temposlider->setValue(info->tempo);
  temposlider->addListener (this);
  
  addAndMakeVisible (timesiglabel = new juce::Label ("timesiglabel", "Time Signature:"));
  timesiglabel->setFont (juce::Font (15.0000f, juce::Font::plain));
  timesiglabel->setJustificationType (juce::Justification::centredLeft);
  timesiglabel->setEditable (false, false, false);
  
  addAndMakeVisible (tempolabel = new juce::Label ("tempolabel", "Tempo:"));
  tempolabel->setFont (juce::Font (15.0000f, juce::Font::plain));
  tempolabel->setJustificationType (juce::Justification::centredLeft);
  tempolabel->setEditable (false, false, false);
  
  addAndMakeVisible(timesigeditor = new juce::TextEditor ("timesigeditor"));
  timesigeditor->setMultiLine (false);
  timesigeditor->setReturnKeyStartsNewLine (false);
  timesigeditor->setReadOnly (false);
  timesigeditor->setScrollbarsShown (true);
  timesigeditor->setCaretVisible (true);
  timesigeditor->setPopupMenuEnabled (true);
  juce::String text=juce::String(info->tsig1) + "/" + juce::String(info->tsig2);
  timesigeditor->setText (text);

  addAndMakeVisible(tuningbutton = new juce::ToggleButton ("tuningbutton"));
  tuningbutton->setButtonText ("Include Current Tuning");
  tuningbutton->setToggleState(midifileinfo->bends, juce::dontSendNotification);
  tuningbutton->addListener (this);
  
  addAndMakeVisible(instrumentbutton =
		    new juce::ToggleButton ("instrumentbutton"));
  instrumentbutton->setToggleState(midifileinfo->insts, juce::dontSendNotification);
  instrumentbutton->setButtonText ("Include Current Instruments");
  instrumentbutton->addListener (this);
  
  setSize (600, 120);
}

MidiFileInfoComponent::~MidiFileInfoComponent()
{
  deleteAllChildren();
}

void MidiFileInfoComponent::resized()
{
  int y = 12;
  timelabel->setBounds(12, y,  88, 24);
  millibutton->setBounds(128, y, 104, 24);
  ticksbutton->setBounds(268, y, 136, 24);
  ticksslider->setBounds(412, y, 120, 24);

  y += 36;
  tempolabel->setBounds(12, y,  56, 24);
  temposlider->setBounds(76, y, 120, 24);
  timesiglabel->setBounds(217, y, 104, 24);
  timesigeditor->setBounds(329, y,  40, 24);
  keysigmenu->setBounds(496, y,  80, 24);
  keysiglabel->setBounds(392, y,  96, 24);

  y += 36;
  tuningbutton->setBounds(     120, y, 168, 24);
  instrumentbutton->setBounds( 328, y, 200, 24);  
}

void MidiFileInfoComponent::filenameComponentChanged(juce::FilenameComponent* f) 
{
  if (f == fileeditor)
  {
    midifileinfo->file=fileeditor->getCurrentFile();
  }
}

void MidiFileInfoComponent::comboBoxChanged (juce::ComboBox* combobox)
{
  if (combobox == keysigmenu)
  {
    midifileinfo->keysig=combobox->getSelectedId();
  }
}

void MidiFileInfoComponent::sliderValueChanged (juce::Slider* slider)
{
  if (slider == ticksslider)
  {
    midifileinfo->qticks=(int)slider->getValue();
  }
  else if (slider == temposlider)
  {
    midifileinfo->tempo=(int)slider->getValue();
  }
}

void MidiFileInfoComponent::buttonClicked (juce::Button* button)
{
  if (button == cancelbutton)
  {
    ((juce::DialogWindow *)getTopLevelComponent())->exitModalState(false);
  }
  else if (button == millibutton)
  {
    midifileinfo->ismsec=button->getToggleState();
    if (button->getToggleState())
    {
      ticksslider->setEnabled(false);
    }
    else
    {
      ticksslider->setEnabled(true);
    }
  }
  else if (button == ticksbutton)
  {
    midifileinfo->ismsec=!button->getToggleState();
    if (button->getToggleState())
    {
      ticksslider->setEnabled(true);
    }
    else
    {
      midifileinfo->ismsec=true;
      ticksslider->setEnabled(true);
    }
  }
  else if (button == tuningbutton)
  {
    midifileinfo->bends=button->getToggleState();
  }
  else if (button == instrumentbutton)
  {
    midifileinfo->insts=button->getToggleState();
  }
}

void MidiOutPort::openFileSettingsDialog ()
{
  MidiFileInfoComponent* comp = new MidiFileInfoComponent(&sequenceFile);
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "Midifile Settings";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.set(comp, false);
  dw.runModal();
  int numer, denom;
  if (comp->parseTimeSig(numer, denom))
  {
    sequenceFile.tsig1 = numer;
    sequenceFile.tsig2 = denom;
  }
  delete comp;
}

