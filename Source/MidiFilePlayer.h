/*=======================================================================*
  Copyright (C) 2013 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#ifndef CM_MIDIFILEPLAYER_H
#define CM_MIDIFILEPLAYER_H

#include "Libraries.h"
#include "Transport.h"
#include "MidiPlaybackThread.h"

/*==============================================================================
  MidiFilePlayer
  ==============================================================================*/

class MidiFilePlayer : public juce::DocumentWindow,
                       public MidiPlaybackThread::MidiMessageSource,
                       public Transport::Listener
{
  MidiPlaybackThread* playbackThread;
  Transport* transport;
  juce::File midiFile;
  int midiFileLength;
  double midiFileDuration;
  juce::MidiMessageSequence sequence;
  void setFile(juce::File file);
  //Transport Callbacks
  void play(double position);
  void pause();
  void positionChanged(double position, bool isPlaying);
  void gainChanged(double gain, bool isPlaying);
  void tempoChanged(double tempo, bool isPlaying);
  //MidiPlaybackThread Callbacks
  void addMidiPlaybackMessages(MidiPlaybackThread::MidiMessageQueue& queue, 
                               MidiPlaybackThread::PlaybackPosition& position);
  void handleMessage(juce::MidiMessage& midiMessage);

public:
  MidiFilePlayer();
  ~MidiFilePlayer();
  void closeButtonPressed();
  static void openMidiFilePlayer(juce::File midiFile = juce::File(), bool play = false);
};

#endif




