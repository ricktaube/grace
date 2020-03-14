#include "SFZeroAudioProcessor.h"

sfzero::SFZeroAudioProcessor::SFZeroAudioProcessor() : loadProgress(0.0), loadThread(this)
{
  formatManager.registerBasicFormats();

  for (int i = 0; i < 128; ++i)
  {
    synth.addVoice(new sfzero::Voice());
  }
  
}

sfzero::SFZeroAudioProcessor::~SFZeroAudioProcessor() {}
const juce::String sfzero::SFZeroAudioProcessor::getName() const {return "SFZero";}
int sfzero::SFZeroAudioProcessor::getNumParameters() { return 0; }
float sfzero::SFZeroAudioProcessor::getParameter(int /*index*/) { return 0.0f; }
void sfzero::SFZeroAudioProcessor::setParameter(int /*index*/, float /*newValue*/) {}
const juce::String sfzero::SFZeroAudioProcessor::getParameterName(int /*index*/) { return ""; }
const juce::String sfzero::SFZeroAudioProcessor::getParameterText(int /*index*/) { return ""; }

void sfzero::SFZeroAudioProcessor::setSfzFile(juce::File *newSfzFile)
{
  sfzFile = *newSfzFile;
  loadSound();
}

void sfzero::SFZeroAudioProcessor::setSfzFileThreaded(juce::File *newSfzFile)
{
  loadThread.stopThread(2000);
  sfzFile = *newSfzFile;
  loadThread.startThread();
}

bool sfzero::SFZeroAudioProcessor::acceptsMidi() const
{
  return true;
}

bool sfzero::SFZeroAudioProcessor::producesMidi() const
{
  return false;
}

int sfzero::SFZeroAudioProcessor::getNumPrograms() {return 1;}
int sfzero::SFZeroAudioProcessor::getCurrentProgram() {return 0;}
void sfzero::SFZeroAudioProcessor::setCurrentProgram(int /*index*/) {}
const juce::String sfzero::SFZeroAudioProcessor::getProgramName(int /*index*/) {return "";}
void sfzero::SFZeroAudioProcessor::changeProgramName(int /*index*/, const juce::String & /*newName*/) {}
void sfzero::SFZeroAudioProcessor::prepareToPlay(double _sampleRate_, int /*samplesPerBlock*/)
{
  synth.setCurrentPlaybackSampleRate(_sampleRate_);
  keyboardState.reset();
}

void sfzero::SFZeroAudioProcessor::releaseResources()
{
  // When playback stops, you can use this as an opportunity to free up any
  // spare memory, etc.
  keyboardState.reset();
}

void sfzero::SFZeroAudioProcessor::processBlock(juce::AudioSampleBuffer &buffer, juce::MidiBuffer &midiMessages)
{
  int numSamples = buffer.getNumSamples();
  keyboardState.processNextMidiBuffer(midiMessages, 0, numSamples, true);
  buffer.clear();
  synth.renderNextBlock(buffer, midiMessages, 0, numSamples);
}

bool sfzero::SFZeroAudioProcessor::hasEditor() const
{
  return false; // (change this to false if you choose to not supply an editor)
}

juce::AudioProcessorEditor *sfzero::SFZeroAudioProcessor::createEditor() {return nullptr;}

void sfzero::SFZeroAudioProcessor::getStateInformation(juce::MemoryBlock &destData)
{
  auto obj = new juce::DynamicObject();
  obj->setProperty("sfzFilePath", sfzFile.getFullPathName());
  auto sound = getSound();
  if (sound)
  {
    int subsound = sound->selectedSubsound();
    if (subsound != 0)
      obj->setProperty("subsound", subsound);
  }

  juce::MemoryOutputStream out(destData, false);
  juce::JSON::writeToStream(out, juce::var(obj));
}

void sfzero::SFZeroAudioProcessor::setStateInformation(const void *data, int sizeInBytes)
{
  juce::MemoryInputStream in(data, sizeInBytes, false);
  juce::var state = juce::JSON::parse(in);
  juce::var pathVar = state["sfzFilePath"];
  if (pathVar.isString())
  {
    auto sfzFilePath = pathVar.toString();
    if (!sfzFilePath.isEmpty())
    {
      juce::File file(sfzFilePath);
      setSfzFile(&file);
      auto sound = getSound();
      if (sound)
      {
        juce::var subsoundVar = state["subsound"];
        if (subsoundVar.isInt())
          sound->useSubsound(int(subsoundVar));
      }
    }
  }
}

sfzero::Sound *sfzero::SFZeroAudioProcessor::getSound()
{
  juce::SynthesiserSound *sound = synth.getSound(0);

  return dynamic_cast<sfzero::Sound *>(sound);
}

int sfzero::SFZeroAudioProcessor::numVoicesUsed() {return synth.numVoicesUsed();}

juce::String sfzero::SFZeroAudioProcessor::voiceInfoString() {return synth.voiceInfoString();}

void sfzero::SFZeroAudioProcessor::loadSound(juce::Thread *thread)
{
  loadProgress = 0.0;
  synth.clearSounds();

  if (!sfzFile.existsAsFile())
  {
    return;
  }

  sfzero::Sound *sound;
  auto extension = sfzFile.getFileExtension();
  if ((extension == ".sf2") || (extension == ".SF2"))
  {
    sound = new sfzero::SF2Sound(sfzFile);
  }
  else
  {
    sound = new sfzero::Sound(sfzFile);
  }
  sound->loadRegions();
  sound->loadSamples(&formatManager, &loadProgress, thread);
  if (thread && thread->threadShouldExit())
  {
    delete sound;
    return;
  }

  synth.addSound(sound);
}

sfzero::SFZeroAudioProcessor::LoadThread::LoadThread(SFZeroAudioProcessor *processorIn)
    : Thread("SFZLoad"), processor(processorIn)
{
}

void sfzero::SFZeroAudioProcessor::LoadThread::run() {processor->loadSound(this);}

juce::AudioProcessor *JUCE_CALLTYPE createPluginFilter() {return new sfzero::SFZeroAudioProcessor();}
