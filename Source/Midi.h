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

class MidiOutPort;
class ConsoleWindow;
class MidiReceiveComponent;

struct MidiNode
{
  int type;
  double time;
  int chan;
  double data1;
  double data2;
  double duration;
  MidiOutPort *midiOutPort;
  MidiNode(int type, double time, int chan, double data1, double data2 = 0.0, double duration = 0.0) 
    : type (type),
      time (time),
      chan (chan),
      data1 (data1),
      data2 (data2),
      duration (duration), 
      midiOutPort (0)
  {
  }
  ~MidiNode()
  { 
  }  
  bool process();
};

/*
class MidiNodeComparator
{
public:
  static int compareElements(MidiNode* e1, MidiNode* e2)
  {
    if (e1->time <= e2->time)
      return -1;
    else 
      return 1;
  }
};
*/

struct MidiFileInfo
{
  juce::File file;    // output file
  int keysig;   // 1 to 15 (1=7Flats, 8=CMajor, 15=7Sharps
  bool ismsec;  // if true file data is in milliseconds
  int qticks;   // ticks per quarter (if ismsec==false)
  int tempo;    // beats per minute
  int tsig1;    // timesig numerator
  int tsig2;    // timesig denominator
  bool insts;   // include current program change assigments
  bool bends;   // include current tuning info
  
  MidiFileInfo() 
    : file (juce::File()), 
      keysig (8), 
      ismsec (false), 
      qticks (480),
      tempo (60), 
      tsig1 (4),
      tsig2 (4),
      insts (true),
      bends (false)
  {
  }      
  
  ~MidiFileInfo()
  {
  }
  
  juce::MidiMessage getTempoMessage()
  {
    double micro=1000000.0 * (60.0 / (double)tempo);
    return juce::MidiMessage::tempoMetaEvent((int)micro);
  }
  
  juce::MidiMessage getTimeSigMessage()
  {
    return juce::MidiMessage::timeSignatureMetaEvent(tsig1, tsig2);
  }
  
  // arrrg JUCE is missing keysig meta message!
  juce::MidiMessage getKeySigMessage()
  {
    int sf = keysig - 8;  // sf -7 to 7
    juce::uint8 d[8];
    d[0] = 0xff; // is meta event
    d[1] = 89;   // meta event opcode for keysig
    d[2] = 0x02; // meta event data length
    d[3] = sf;   // two's complement of -7 to 7
    d[4] = 0;    // major mode
    return juce::MidiMessage(d, 5, 0.0); 
  }
};

class MidiOutPort : public juce::Thread 
{
  juce::CriticalSection outLock;
  ///The id of the open device or -1 if none is open
  int openDeviceId;
  ///If true then openDeviceId 0 routes output to the internal synth
  bool isInternalSynth;
  ///The name of the internal synth that appears in the MidiOut menu
  juce::String internalSynthName;
  ///Pointer to the open physical output device or 0 if none is
  ///open. Note that when ((device==0 && isInternalSynth==true &&
  ///openDeviceId==0) then sendOut() will route outgoing messages to
  ///the internal synth
  juce::MidiOutput* device;
  juce::String openDeviceName;
  void run();

public:
  juce::OwnedArray<MidiNode, juce::CriticalSection> outputNodes;
  
  MidiOutPort();
  ~MidiOutPort();
  const juce::StringArray getDevices();
  bool open(int id);
  bool open(juce::String name);
  bool isOpen(int id = -1) ;
  juce::String getOpenDeviceName();
  void close(int id = -1);
  void testOutput();
  bool isOutputQueueActive();

  void clear();
  void addNode(MidiNode *n);
  static int compareElements(MidiNode* e1, MidiNode* e2)
  {
    if (e1->time <= e2->time)
      return -1;
    else 
      return 1;
  }
  //  MidiNodeComparator comparator;

  void sendNote(double wait, double dur, double key, double vel, double chan,
		bool toseq);
  void sendData(int type, double wait, double chan, double data1, double data2,
		bool toseq);

  /// thread safe method sends a message to the device or plugin synth
  void sendOut(juce::MidiMessage& msg);
  //-----------//
  //Microtuning//
  //-----------//
  static double channelTunings [16][16] ;
  int   microDivisions;  // divisions per semitone: 1-16
  double microIncrement; // size of tuning's division (1.0=semitone)
  int   microChanCount; // number of addressable channels in tuning
  int   microChanBlock; // total number of channels used by tuning
  bool  avoidDrumTrack; // if true then avoid channel 9
  int   pitchbendwidth;
  juce::StringArray tuningNames ; // string name for each tuning
  juce::StringArray instrumentNames ; // string name of each GM instrument
  int getTuning() ;
  void setTuning(int tune, bool send=true) ;
  bool isCurrentTuning(int tune);
  juce::String getTuningName(int tune);
  int getTuningDivisions(int tune); // number of division
  int getTuningChannels(int tune); // number of addressable chans
  int getTuningChannelsClaimed(int tune); // total chans used
  bool getAvoidDrumTrack();
  void setAvoidDrumTrack(bool b);
  int getPitchBendWidth();
  void setPitchBendWidth(int w);
  void sendTuning();
  void getTuningValues(juce::Array<int>& vals);

  // instruments
  int programChanges[16];
  int getInstrument(int chan);
  void setInstrument(int chan, int prog);
  void getInstruments(juce::Array<int>& vals);
  void setInstruments(juce::Array<int>& vals, bool send);
  //  void setInstrument(int chan, int pc, bool send=false);
  bool isInstrumentChannel(int chan);
  void sendInstruments();
  void resetInstruments();
  void openInstrumentsDialog();
  void openDeviceSettings();
  void openMidiFilePlayer();

  //-----------------//
  //MIDI file support//
  //-----------------//

  /// MIDI Sequence that captures data sent from Scheme
  juce::MidiMessageSequence captureSequence;
  /// MIDI file information configurable by user
  MidiFileInfo sequenceFile;

  /// Returns true if capture sequence is empty.
  bool isSequenceEmpty();
  /// Clears capture sequence of any data.
  void clearSequence();
  /// Saves capture sequence to file
  void saveSequence(bool ask=false);
  /// Sets the output file for writing the capture sequence
  void setOutputFile(juce::String name);
  /// Opens dialog to configure MidiFile parameters
  void openFileSettingsDialog();

  const juce::PopupMenu getMidiOutMenu();
  const juce::PopupMenu getMidiSeqMenu();

  // async messaging

  int performCommand(juce::CommandID id, int data=0, juce::String text=juce::String());

  juce_DeclareSingleton (MidiOutPort, true)
};

class MidiInPort : public juce::MidiInputCallback
{
private:
  enum {Stopped, Testing, Receiving}; // running mode
  int openDeviceId;
  juce::MidiInput *device;
  juce::String openDeviceName;
  bool tracing;

  ///channelMask has a 1 set for every Midi channel that can be
  ///received.
  int channelMask;

  ///opcodeMask has a 1 set for every MidiOp that can be received. A
  ///MidiOp is converted to a mask position by subtracting #x8 from
  ///the opcode value (so MidiOps::Off is position 0 and Bend is 15)
  int opcodeMask;

public:
  static const int AllOpcodes = 0x7F;
  static const int AllChannels = 0xFFFF;
  MidiInPort();
  ~MidiInPort();
  const juce::StringArray getDevices();
  bool open(int id);
  bool open(juce::String name);
  bool isOpen(int id=-1);
  juce::String getOpenDeviceName();
  void close(int id=-1);

  ///Returns the current input channel mask. The mask has a 1 set for
  ///every channel that is activce (see isInputChannelActive)
  int getInputChannelMask();
  void setInputChannelMask(int mask);

  /// Returns true if messages with the channel are allowed to be
  /// imported.
  bool isInputChannelActive(int channel);

  /// Sets the specified channel to be active or not. If its inactive
  /// then incoming messages with that channel will not be handled.
  void setInputChannelActive(int channel, bool isActive);
  
  int getOpcodeMask();

  void setOpcodeMask(int mask);

  bool isOpcodeActive(int code, bool isop=false);
  void toggleOpcodeActive(int code);
  void setOpcodeActive(int code, bool active);

  bool isTracing();
  void setTracing(bool trace);
  void toggleTracing();

  bool isInputMessageActive(const juce::MidiMessage& msg);
  void handleIncomingMidiMessage (juce::MidiInput *dev, const juce::MidiMessage &msg);
  void handlePartialSysexMessage (juce::MidiInput *dev, const juce::uint8 *data, int num, double time);
  juce_DeclareSingleton (MidiInPort, true)
};
