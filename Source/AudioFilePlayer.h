/*=======================================================================*
  Copyright (C) 2013 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#ifndef CM_AUDIOFILEPLAYER_H
#define CM_AUDIOFILEPLAYER_H

#include "Libraries.h"
#include "Transport.h"

/*==============================================================================
  AudioFilePlayer
  ==============================================================================*/

class AudioFilePlayer : public juce::DocumentWindow,
                        public Transport::Listener
{
  friend class AudioPositionTimer;
  //A timer that updates the position slider during playback and stops
  //the transport when the end of the audiofile is reached.
  struct AudioPositionTimer : public juce::Timer
  {
    AudioFilePlayer* player;
    AudioPositionTimer(AudioFilePlayer* player) : player (player) {}
    ~AudioPositionTimer(){}
    void timerCallback();
  };
  Transport* transport;
  AudioPositionTimer* audioPositionTimer;
  juce::AudioDeviceManager& deviceManager;
  juce::AudioFormatManager formatManager;
  juce::AudioSourcePlayer audioSourcePlayer;
  juce::AudioTransportSource transportSource;
  std::unique_ptr<juce::AudioFormatReaderSource> audioFileReader;
  juce::File audioFile;
  void setFile(const juce::File& file);
  void clearTransport();
  void loadIntoTransport(const juce::File& audioFile);
  void setAudioInfo(juce::AudioFormatReader* reader);
  //Transport callbacks
  void play(double position);
  void pause(void);
  void positionChanged(double position, bool isPlaying);
  void gainChanged(double gain, bool isPlaying);
  void tempoChanged(double tempo, bool isPlaying) {} // unused

public:
  AudioFilePlayer();
  ~AudioFilePlayer();
  void closeButtonPressed();
  static void openAudioFilePlayer(juce::File fileToOpen, bool play);
};

#endif

