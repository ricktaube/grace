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
#include "SFZeroAudioProcessor.h"

class AudioManager
{
    
  /// The SFZero soundfont synth.
//  std::unique_ptr<sfzero::SFZeroAudioProcessor> sfZeroAudioProcessor;
  sfzero::SFZeroAudioProcessor sfZeroAudioProcessor;

  /// Internal synth player. This will point to SFZero, DSLMusicDevice (Mac only)
  /// or nullptr if a MidiOutput port is open.
  juce::AudioProcessorPlayer synthPlayer;
#if JUCE_MAC
  /// Internal synth graph for the high-quality DSLMusicDevice (Mac only).
  juce::AudioProcessorGraph macDLSMusicDevice;
  juce::AudioPluginFormatManager macPluginFormatManager;
  /// Creates internal synth for MIDI playback (Mac).
  bool macLoadDLSMusicDevice();
  /// Creates internal synth for MIDI playback (Mac).
  bool macCreateDLSMusicDevice(std::unique_ptr<juce::AudioPluginInstance> synth);
  /// Prints all known plugins to std::out
  void macListAllPlugins();
#endif // JUCE_MAC
  
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
