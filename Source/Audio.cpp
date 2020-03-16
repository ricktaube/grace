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
#include "Preferences.h"
#include "Audio.h"
#include "Console.h"
#include "AudioFilePlayer.h"
#include "Main.h"

/*=======================================================================*
  Global Audio Manager
  *=======================================================================*/
 
juce_ImplementSingleton(AudioManager)

AudioManager::AudioManager() {
  audioDeviceManager.initialise(2, 2, 0, true);
#ifdef JUCE_MAC
  macLoadDLSMusicDevice();
#endif
  
  //  // Call the audio manager's addMidiInputCallback() method
  //  // to add this component as the callback.
  //  audioManager.addMidiInputCallback("", this);
  //
  //  // Connect the synth player to the host's audio manager using its
  //  // addAudioCallback() method.
  
  //  // Create a new sfzero::SFZeroAudioProcessor and assign it to
  //  // the sfZeroAudioProcessor unique_ptr.
  //  sfZeroAudioProcessor.reset(new sfzero::SFZeroAudioProcessor());
  //  // Add the sfZeroAudioProcessor to the sfzero player.
  //  sfZeroPlayer.setProcessor(sfZeroAudioProcessor.get());
  //  // Call loadSoundFont() to load the our sound font file "G800-A112-Piano1d-2-3f.sfz".
  //  // This is stored in the the app's resource directory. See: MainApplication::getRuntimeResourceDirectory()
  auto soundFont = Grace::getApp().getRuntimeResourceDirectory().getChildFile("G800-A112-Piano1d-2-3f.sfz");
  std::cout << "soundfont " << (soundFont != juce::File() ? "EXISTS!\n" : "ERROR!\n") ;
  std::cout << "***NAME = " << sfZeroAudioProcessor.getName() << "\n";
  //  loadSoundFont(soundFont);
  
}

AudioManager::~AudioManager()
{
  // Flush any midi input
  synthPlayer.getMidiMessageCollector().reset(1000);
  audioDeviceManager.removeAudioCallback(&synthPlayer);
  synthPlayer.setProcessor(0);
}

#if JUCE_MAC
bool AudioManager::macLoadDLSMusicDevice() {
  macPluginFormatManager.addDefaultFormats();
  juce::OwnedArray<juce::PluginDescription> buf;
  juce::PluginDescription des;
  // Create the DLSMusicDevice (see: 'auval -a')
  des.pluginFormatName = "AudioUnit";
  des.fileOrIdentifier = "AudioUnit:Synths/aumu,dls ,appl";
  auto lambda = [this] (std::unique_ptr<juce::AudioPluginInstance> synth, const juce::String&) {
    if (synth)
      macCreateDLSMusicDevice(std::move(synth));
    else
      std::cout << "*** Couldn't create internal synth :(\n";
    return false;
  };
  macPluginFormatManager.createPluginInstanceAsync(des, macDLSMusicDevice.getSampleRate(), macDLSMusicDevice.getBlockSize(), lambda);
  audioDeviceManager.addAudioCallback(&synthPlayer);
  synthPlayer.setProcessor(&macDLSMusicDevice);
  return true;
}

bool AudioManager::macCreateDLSMusicDevice(std::unique_ptr<juce::AudioPluginInstance> internalSynth)
{
  //std::cout << "*** IN INTERNALSYNTH\n";
  // These have to be hooked up first or the AudioOutput graph node
  // wont allow connections.
//  audioDeviceManager.addAudioCallback(&synthPlayer);
//  synthPlayer.setProcessor(&macDLSMusicDevice);
  // Create nodes for the synth's AudioProcessorGraph
  std::unique_ptr<juce::AudioProcessor> midiInputProc (new juce::AudioProcessorGraph::AudioGraphIOProcessor(juce::AudioProcessorGraph::AudioGraphIOProcessor::midiInputNode));
  std::unique_ptr<juce::AudioProcessor> audioOutputNode (new juce::AudioProcessorGraph::AudioGraphIOProcessor(juce::AudioProcessorGraph::AudioGraphIOProcessor::audioOutputNode));
  auto input = macDLSMusicDevice.addNode(std::move(midiInputProc));
  auto synth = macDLSMusicDevice.addNode(std::move(internalSynth)); // NOTE: internal synth is at node index 1.
  auto output = macDLSMusicDevice.addNode(std::move(audioOutputNode));
  /// require valid pointers for all three nodes or give up.
  bool success = (input && synth && output);
  //std::cout << "***SUCCESS1=" << success << "\n";
  if (success) {
    int MCI = juce::AudioProcessorGraph::midiChannelIndex;
    // test if we can connect the nodes midiinput -> synth -> audioout.
    // for two channel audio there are two audio connections (0 and 1).
    juce::AudioProcessorGraph::NodeAndChannel node1, node2, node3, node4, node5, node6;
    node1.nodeID = input->nodeID;
    node1.channelIndex = MCI;
    node2.nodeID = synth->nodeID;
    node2.channelIndex = MCI;
    juce::AudioProcessorGraph::Connection con1 (node1, node2);
    node3.nodeID = synth->nodeID;
    node3.channelIndex = 0;
    node4.nodeID = output->nodeID;
    node4.channelIndex = 0;
    juce::AudioProcessorGraph::Connection con2 (node3, node4);
    node5.nodeID = synth->nodeID;
    node5.channelIndex = 1;
    node6.nodeID = output->nodeID;
    node6.channelIndex = 1;
    juce::AudioProcessorGraph::Connection con3 (node5, node6);
    success = (macDLSMusicDevice.canConnect(con1) &&
               macDLSMusicDevice.canConnect(con2) &&
               macDLSMusicDevice.canConnect(con3));
    //std::cout << "***SUCCESS2=" << success << "\n";
    if (success) {
      macDLSMusicDevice.addConnection(con1);
      macDLSMusicDevice.addConnection(con2);
      macDLSMusicDevice.addConnection(con3);
      return true;
    }
    else
      std::cout <<  "*** Couldn't connect nodes in synthGraph :(\n";
  }
  return false;
}

// DEBUGGING
void AudioManager::macListAllPlugins()
{
  juce::KnownPluginList plugins;
  juce::AudioPluginFormatManager* formats = &macPluginFormatManager;
  
  std::cout << "Audio Plugin Formats:\n";
  for(int i = 0; i < formats->getNumFormats(); i++)
  {
    juce::AudioPluginFormat* f = formats->getFormat(i);
    std::cout << f->getName() << " paths:\n";
    std::cout << f->getDefaultLocationsToSearch().toString();
    juce::PluginDirectoryScanner scanner
      (plugins, *f, f->getDefaultLocationsToSearch(), true, juce::File());
    bool Searching = true;
    while(Searching)
    {
      /*Exclude the Sibelius GM module which has an annoying message box that
        pops up, thereby stalling the application. Further plug-ins can be
        excluded from the scan by separating their names with a pipe.*/
      juce::String ExcludedPlugins = "General MIDI Module|";
      juce::String NextPlugin;
      juce::String scannedPluginName;
      NextPlugin << scanner.getNextPluginFileThatWillBeScanned();
      if(ExcludedPlugins.contains(NextPlugin))
        Searching = scanner.skipNextFile();
      else
        Searching = scanner.scanNextFile(true, scannedPluginName);
    }
  }
  
  for(auto desc : plugins.getTypes()) {
    std::cout << desc.name << " " << desc.fileOrIdentifier << "\n";
  }
}
#endif // JUCE_MAC

bool AudioManager::sendMessageToPluginGraph(const juce::MidiMessage &message)
{
  // stop a juce assertion about time stamp 0 with midi
  // collectors. 
  if (message.getTimeStamp() == 0.0)
  {
    juce::MidiMessage fixed = juce::MidiMessage(message, juce::Time::getMillisecondCounterHiRes() * .001);
    synthPlayer.getMidiMessageCollector().addMessageToQueue(fixed);
  }
  else
    synthPlayer.getMidiMessageCollector().addMessageToQueue(message);
  return true;
}

void AudioManager::openAudioSettings()
{
  // Open an AudioDeviceSelectorComponent but without midi selections
  // (these are handled by the Midi Manger)
  juce::AudioDeviceSelectorComponent* comp
    = new juce::AudioDeviceSelectorComponent(audioDeviceManager, 0, 256, 0, 256, false, false, true, false);
  comp->setSize(500, 600);
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = true;
  dw.dialogTitle = "Audio Settings";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.setOwned(comp);
  dw.runModal();
}


void AudioManager::stopAudioPlayback()
{
  // TODO: cycle through audiofileplayer windows and stop them playing
}
