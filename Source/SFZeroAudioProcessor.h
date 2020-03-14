#ifndef INCLUDED_SFZEROAUDIOPROCESSOR_H
#define INCLUDED_SFZEROAUDIOPROCESSOR_H

#include "../JuceLibraryCode/JuceHeader.h"

namespace sfzero
{
class Sound;

  class SFZeroAudioProcessor : public juce::AudioProcessor
{
public:
  SFZeroAudioProcessor();
  ~SFZeroAudioProcessor();

  bool silenceInProducesSilenceOut(void) const override { return false; }
  double getTailLengthSeconds(void) const override { return 0; }
  void prepareToPlay(double sampleRate, int samplesPerBlock) override;
  void releaseResources() override;
  void processBlock(juce::AudioSampleBuffer &buffer, juce::MidiBuffer &midiMessages) override;

  juce::AudioProcessorEditor *createEditor() override;
  bool hasEditor() const override;

  const juce::String getName() const override;

  int getNumParameters() override;

  float getParameter(int index) override;
  void setParameter(int index, float newValue) override;

  const juce::String getParameterName(int index) override;
  const juce::String getParameterText(int index) override;

  void setSfzFile(juce::File *newSfzFile);
  void setSfzFileThreaded(juce::File *newSfzFile);

  juce::File getSfzFile() { return (sfzFile); }
  bool acceptsMidi() const override;
  bool producesMidi() const override;

  int getNumPrograms() override;
  int getCurrentProgram() override;
  void setCurrentProgram(int index) override;
  const juce::String getProgramName(int index) override;
  void changeProgramName(int index, const juce::String &newName) override;

  void getStateInformation(juce::MemoryBlock &destData) override;
  void setStateInformation(const void *data, int sizeInBytes) override;

  juce::MidiKeyboardState keyboardState;
  double loadProgress;

  Sound *getSound();
  int numVoicesUsed();
  juce::String voiceInfoString();

protected:

  class LoadThread : public juce::Thread
  {
  public:
    LoadThread(SFZeroAudioProcessor *processor);
    void run() override;

  protected:
    SFZeroAudioProcessor *processor;
  };
  friend class LoadThread;

  juce::File sfzFile;
  Synth synth;
  juce::AudioFormatManager formatManager;
  LoadThread loadThread;

  void loadSound(juce::Thread *thread = nullptr);

private:
  JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(SFZeroAudioProcessor);
};
}


#endif // INCLUDED_SFZEROAUDIOPROCESSOR_H
