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

/*=======================================================================*
  Global Audio Manager
  *=======================================================================*/
 
juce_ImplementSingleton(AudioManager)

AudioManager::AudioManager() {
  audioDeviceManager.initialise(2, 2, 0, true);
#ifdef JUCE_MAC
  pluginFormatManager.addDefaultFormats();
  juce::OwnedArray<juce::PluginDescription> buf;
  juce::PluginDescription des;
  // Create the DLSMusicDevice (see: 'auval -a')
  des.pluginFormatName = "AudioUnit";
  des.fileOrIdentifier = "AudioUnit:Synths/aumu,dls ,appl";
  auto lambda = [this] (std::unique_ptr<juce::AudioPluginInstance> synth, const juce::String&) {
    if (synth)
      this->createInternalSynth(std::move(synth));
    else
      std::cout << "*** Couldn't create internal synth :(\n";
  };
  pluginFormatManager.createPluginInstanceAsync(des, synthGraph.getSampleRate(), synthGraph.getBlockSize(), lambda);
#endif
}

AudioManager::~AudioManager()
{
  deleteInternalSynthEditor();
  // Flush any midi input
  synthPlayer.getMidiMessageCollector().reset(1000);
  audioDeviceManager.removeAudioCallback(&synthPlayer);
  synthPlayer.setProcessor(0);
}

void AudioManager::deleteInternalSynthEditor() {
  // If we have an internal synth delete its editor if it exists.
  if (synthGraph.getNode(1)) {
    auto* plug = synthGraph.getNode(1)->getProcessor();
    if (juce::Component* comp = plug->getActiveEditor()) {
      comp = comp->getTopLevelComponent();
      delete comp;
    }
  }
}

bool AudioManager::createInternalSynth(std::unique_ptr<juce::AudioPluginInstance> internalSynth)
{
  //std::cout << "*** IN INTERNALSYNTH\n";
  // These have to be hooked up first or the AudioOutput graph node
  // wont allow connections.
  audioDeviceManager.addAudioCallback(&synthPlayer);
  synthPlayer.setProcessor(&synthGraph);
  // Create nodes for the synth's AudioProcessorGraph
  std::unique_ptr<juce::AudioProcessor> midiInputProc (new juce::AudioProcessorGraph::AudioGraphIOProcessor(juce::AudioProcessorGraph::AudioGraphIOProcessor::midiInputNode));
  std::unique_ptr<juce::AudioProcessor> audioOutputNode (new juce::AudioProcessorGraph::AudioGraphIOProcessor(juce::AudioProcessorGraph::AudioGraphIOProcessor::audioOutputNode));
  auto input = synthGraph.addNode(std::move(midiInputProc));
  auto synth = synthGraph.addNode(std::move(internalSynth)); // NOTE: internal synth is at node index 1.
  auto output = synthGraph.addNode(std::move(audioOutputNode));
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
    success = (synthGraph.canConnect(con1) && synthGraph.canConnect(con2) &&
               synthGraph.canConnect(con3));
    //std::cout << "***SUCCESS2=" << success << "\n";
    if (success) {
      synthGraph.addConnection(con1);
      synthGraph.addConnection(con2);
      synthGraph.addConnection(con3);
      return true;
    }
    else
      std::cout <<  "*** Couldn't connect nodes in synthGraph :(\n";
  }
  return false;
}

// ORIGINAL
//bool AudioManager::createInternalSynth()
//{
//#ifdef JUCE_MAC
//  pluginFormatManager.addDefaultFormats();
//  juce::OwnedArray<juce::PluginDescription> buf;
//  juce::String err;
//  juce::PluginDescription des;
//
//  // Create the DLSMusicDevice (see: 'auval -a')
//  des.pluginFormatName = "AudioUnit";
//  des.fileOrIdentifier = "AudioUnit:Synths/aumu,dls ,appl";
//
//  internalSynth = pluginFormatManager.createPluginInstance(des, synthGraph.getSampleRate(), synthGraph.getBlockSize(), err);
//  if(!internalSynth)
//  {
//    //    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon, "Couldn't create Internal Synth", err);
//    std::cout << "\n*** Couldn't create Internal Synth, err='" <<  err << "' **\n";
//    return false;
//  }
//
//  // These have to be hooked up first or the AudioOutput graph node
//  // wont allow connections.
//  audioDeviceManager.addAudioCallback(&synthPlayer);
//  synthPlayer.setProcessor(&synthGraph);
//
//  // Create nodes for the synth's AudioProcessorGraph
//  juce::AudioProcessorGraph::AudioGraphIOProcessor* midiInputNode = new juce::AudioProcessorGraph::AudioGraphIOProcessor(juce::AudioProcessorGraph::AudioGraphIOProcessor::midiInputNode);
//  juce::AudioProcessorGraph::AudioGraphIOProcessor* audioOutputNode = new juce::AudioProcessorGraph::AudioGraphIOProcessor(juce::AudioProcessorGraph::AudioGraphIOProcessor::audioOutputNode);
//  juce::AudioProcessorGraph::Node* N1 = synthGraph.addNode(midiInputNode);
//  juce::AudioProcessorGraph::Node* N2 = synthGraph.addNode(internalSynth);
//  juce::AudioProcessorGraph::Node* N3 = synthGraph.addNode(audioOutputNode);
//  if(!N1 || !N2 || !N3)
//  {
//    std::cout << "failed to create N1 N2 or N3\n";
//    // delete unadded (graph owns added)
//    if(!N1) delete midiInputNode;
//    if(!N2) delete internalSynth;
//    if(!N3) delete audioOutputNode;
//    internalSynth = 0;
//    return false;
//  }
//
//  int MCI = juce::AudioProcessorGraph::midiChannelIndex;
//
///*
//    std::cout << "Midi input node id=" << N1->nodeId << "\n";
//    std::cout << "Internal synth node id=" << N2->nodeId << "\n";
//    std::cout << "Audio output node id=" << N3->nodeId << "\n";
//    std::cout << "Internal synth output chans=" << internalSynth->getNumOutputChannels() << "\n";
//    for (int i = 0; i < internalSynth->getNumOutputChannels(); i++)
//    std::cout << i << ":" << internalSynth->getOutputChannelName(i) << "\n";
//    std::cout << "Output node input chans=" << audioOutputNode->getNumInputChannels() << "\n";
//    for (int i = 0; i < audioOutputNode->getNumInputChannels(); i++)
//    std::cout << i << ":" << audioOutputNode->getInputChannelName(i) << "\n";
//    std::cout << "can connect Midi->Synth=" << synthGraph.canConnect(N1->nodeId, MCI, N2->nodeId, MCI) << "\n";
//    std::cout << "can connect Synth0->Audio0=" << synthGraph.canConnect(N2->nodeId, 0, N3->nodeId, 0) << "\n";
//    std::cout << "can connect Synth1->Audio1=" << synthGraph.canConnect(N2->nodeId, 1, N3->nodeId, 1) << "\n";
//*/
//
//  if(!synthGraph.canConnect(N1->nodeID, MCI, N2->nodeID, MCI) ||
//     !synthGraph.canConnect(N2->nodeID, 0, N3->nodeID, 0) ||
//     !synthGraph.canConnect(N2->nodeID, 1, N3->nodeID, 1))
//  {
//    // set to null but dont delete! (already owned by graph)
//    internalSynth = 0;
//    return false;
//  }
//
//  synthGraph.addConnection(N1->nodeID, MCI, N2->nodeID, MCI);
//  synthGraph.addConnection(N2->nodeID, 0, N3->nodeID, 0);
//  synthGraph.addConnection(N2->nodeID, 1, N3->nodeID, 1);
//  //  std::cout << "is connected=" << synthGraph.isConnected(N2->nodeId, N3->nodeId) << "\n";
//  // End AudioProcessorGraph creation
//#endif
//  return true;
//}


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

void AudioManager::openSynthSettings()
{
  // HKT FIXME this should not run modally!
  if (synthGraph.getNode(1))
  {
    auto* plug = synthGraph.getNode(1)->getProcessor();
    if (juce::AudioProcessorEditor* comp = plug->createEditorIfNeeded())
    {
      juce::DialogWindow::LaunchOptions dw;
      dw.useNativeTitleBar = true;
      dw.resizable = false;
      dw.dialogTitle = "DLSMusicDevice Settings";
      dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
      dw.content.set(comp, false);
      dw.runModal();
    }
//    if (juce::Component* comp = plug->getActiveEditor()) {
//      comp = comp->getTopLevelComponent();
//      delete comp;
//    }
  }
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

// DEBUGGING
void AudioManager::listAllPlugins()
{
  juce::KnownPluginList plugins;
  juce::AudioPluginFormatManager* formats = &pluginFormatManager;
  
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

void AudioManager::stopAudioPlayback()
{
  // TODO: cycle through audiofileplayer windows and stop them playing
}
