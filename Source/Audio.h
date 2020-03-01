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

#ifndef CM_AUDIO_H
#define CM_AUDIO_H

#include "Libraries.h"

class AudioManager
{
  /// Internal synth graph (Mac).
  juce::AudioProcessorGraph synthGraph;
  /// Internal synth player (Mac).
  juce::AudioProcessorPlayer synthPlayer;
  juce::AudioPluginFormatManager pluginFormatManager;
  /// Creates internal synth for MIDI playback (Mac).
  bool createInternalSynth(std::unique_ptr<juce::AudioPluginInstance> synth);
  /// Deletes the internal synth's editor if it exists (Mac).
  void deleteInternalSynthEditor();

  /// Prints all known plugins to std::out
  void listAllPlugins();
public:
  juce::AudioDeviceManager audioDeviceManager;
  /// Internal synth instance (Mac)
  //std::unique_ptr<juce::AudioPluginInstance> internalSynth;
  /// Constructor
  AudioManager();
  /// Destructor
  ~AudioManager();
  /// Sends message to internal synth if available
  bool sendMessageToPluginGraph(const juce::MidiMessage &message);
  /// Opens modal editor to change audio settings
  void openAudioSettings();
  /// Opens modal plugin editor on InternalSynth (Mac)
  void openSynthSettings();
  void stopAudioPlayback();
  ///Opens transport window to play audio file. Defined in
  ///AudioFilePlayer.cpp
  void openAudioFilePlayer(juce::File fileToOpen = juce::File(), bool play = false);
  juce_DeclareSingleton (AudioManager, true)
};

#endif
