/*=======================================================================*
  Copyright (C) 2013 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#include "AudioFilePlayer.h"
#include "Enumerations.h"
#include "Audio.h"

AudioFilePlayer::AudioFilePlayer()
  : juce::DocumentWindow("", ColorThemeIDs::getWindowBackgroundColor(),
                         juce::DocumentWindow::closeButton),
    transport(nullptr),
    audioPositionTimer(nullptr),
    deviceManager(AudioManager::getInstance()->audioDeviceManager),
    audioFile(juce::File())
{
  // audio init
  formatManager.registerBasicFormats();
  deviceManager.addAudioCallback(&audioSourcePlayer);
  audioSourcePlayer.setSource(&transportSource);
  // window init
  setVisible(true);
  setUsingNativeTitleBar(true);
  setDropShadowEnabled(true);
  setCentreRelative(0.5, 0.5);
  setResizable(false, false);
  // transport init
  transport = new Transport(this, true, false);

  // add a combo box of sound file information
  juce::Font f (12.0f, juce::Font::bold | juce::Font::underlined);
  juce::Label* audioInfo = new juce::Label("", "?");
  audioInfo->setFont(f);
  transport->addAndMakeVisible(audioInfo);
  audioInfo->setComponentID("AudioInfo");
  transport->setSize(transport->getWidth(), transport->getHeight() + 28);
  audioInfo->setBounds((transport->getWidth() - 18) / 2,
                       transport->getBottom() - 28, 140, 18);
  /*  juce::ComboBox* audioInfo = new juce::ComboBox("AudioInfo");
  transport->addAndMakeVisible(audioInfo);
  audioInfo->setEditableText(false);
  audioInfo->setComponentID("AudioInfo");
  audioInfo->setTextWhenNothingSelected("Audio Properties...");
  transport->setSize(transport->getWidth(), transport->getHeight() + 28);
  audioInfo->setBounds((transport->getWidth() - 140) / 2,
                       transport->getBottom() - 28, 140, 18);
  */                                
  setSize(transport->getWidth(), transport->getHeight());
  setContentOwned(transport, false);
  // timer init
  audioPositionTimer = new AudioPositionTimer(this);
  audioPositionTimer->startTimer(50);
  WindowTypes::setWindowType(this, WindowTypes::AudioFilePlayer);
}

AudioFilePlayer::~AudioFilePlayer()
{
  audioPositionTimer->stopTimer();
  // transport will be deleted automatically
  transport->setPausing(true);
  deleteAndZero(audioPositionTimer);
  transportSource.setSource(0);
  audioSourcePlayer.setSource(0);
  deviceManager.removeAudioCallback(&audioSourcePlayer);
}

void AudioFilePlayer::closeButtonPressed()
{
  delete this;
}

void AudioFilePlayer::openAudioFilePlayer(juce::File fileToOpen, bool play)
{
  // check file's audio format to make sure its supported
  juce::AudioFormatManager formatManager;
  formatManager.registerBasicFormats();

  static juce::File lastOpened = juce::File();
  // reuse player if its already open on the file
  for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
  {
    if (AudioFilePlayer* w = dynamic_cast<AudioFilePlayer*>(juce::TopLevelWindow::getTopLevelWindow(i)))
    {
      if (w->audioFile == fileToOpen)
      {
        w->getTopLevelComponent()->toFront(true); 
        w->setFile(fileToOpen); // setFile() again because contents may have changed
        return;
      }
    }
  }
  if (fileToOpen == juce::File())
  {
    juce::String wildcards = formatManager.getWildcardForAllFormats();
//    std::cout << "wildcards=" << wildcards << "\n";
    juce::FileChooser choose ("Play Audio File", lastOpened, 
                              //"*.wav;*.aiff;*.aif;*.aifc;*.snd"
                              wildcards);
    if (choose.browseForFileToOpen())
      fileToOpen = choose.getResult();
    else 
      return;
  }
  else if (!fileToOpen.existsAsFile())
  {
    juce::String msg ("The file ");
    msg << fileToOpen.getFullPathName() << " does not exist.\n";
    return juce::AlertWindow::showMessageBoxAsync
      (juce::AlertWindow::WarningIcon, "Play Audio File", msg);
  }

  if (juce::AudioFormatReader* reader = formatManager.createReaderFor(fileToOpen))
  {
    delete reader;
  }
  else
  {
    juce::String msg ("Unknown or unsupported audio format in file ");
    msg << fileToOpen.getFileName() 
      //        << ". This player handles WAV and AIFF files only." 
        << ".\n";
    return juce::AlertWindow::showMessageBoxAsync(juce::AlertWindow::WarningIcon, "Play Audio File", msg);
   }
  lastOpened = fileToOpen;
  AudioFilePlayer* win = new AudioFilePlayer();
  win->setFile(fileToOpen);
}

/**Timer to draw the transport's position during playback. Stops
   playback when the end of the audiofile is reached.*/
void AudioFilePlayer::AudioPositionTimer::timerCallback()
{
  // if playing then update audio position slider. 
  // FIXME!!!! SEE WHAT THE ORIGNIAL ISPLAYING CHECKED
  if (player->transport->isPlaying()) //transport's isPlaying
  {
    double now = player->transportSource.getCurrentPosition();
    double end = player->transportSource.getLengthInSeconds();
    player->transport->setPlaybackPosition(now, false);
    // auto-pause if at end-of-file
    if (now >= end)
      player->transport->setPausing(true);
  }
}

void AudioFilePlayer::setFile (const juce::File& file)
{
  audioFile = file;
  loadIntoTransport(file);
  setName(file.getFileName());
}

void AudioFilePlayer::clearTransport()
{
  transportSource.stop();
  transportSource.setSource(0);
  if (juce::ComboBox* c = dynamic_cast<juce::ComboBox*>(transport->findChildWithID("AudioInfo")))
  {
    c->clear();
  }
  audioFileReader.release();
  audioFile = juce::File();
  // put transport in default state without triggering callbacks
  transport->setPlaybackRange(0.0, 1.0, false);
  transport->setPlaybackPosition(0.0, false);
  transport->setPlaybackGain(1.0, false);
}

void AudioFilePlayer::loadIntoTransport(const juce::File& file)
{
  // unload the previous file source and delete it..
  clearTransport();
  if (juce::AudioFormatReader* reader = formatManager.createReaderFor(file))
  {
    audioFileReader.reset(new juce::AudioFormatReaderSource(reader, true));
    audioFile = file;
    // NB: could buffer if we really want to play large files...
    transportSource.setSource(audioFileReader.get(), 0, 0, reader->sampleRate);
    // initialize transport for new file without triggering callbacks
    transport->setPlaybackRange(0.0, transportSource.getLengthInSeconds(), false);
    transport->setPlaybackPosition(0.0, false);
    transport->setPlaybackGain(1.0, false);
    setAudioInfo(reader);
  }
}

void AudioFilePlayer::setAudioInfo(juce::AudioFormatReader* reader)
{
  // cache information about audio file in combobox
  if (juce::Label* l = dynamic_cast<juce::Label*>(transport->findChildWithID("AudioInfo")))
  {
    juce::String s;
    s << "Audio file: " << audioFile.getFullPathName() << "\n";
    s << "File size: " << audioFile.descriptionOfSizeInBytes(audioFile.getSize()) << "\n";
    s << "File format: " << reader->getFormatName() << "\n";
    s << "Channels: " << (int)reader->numChannels << "\n";
    s << "Sample rate: " << reader->sampleRate << "\n";
    s << "Sample frames: " << juce::String(reader->lengthInSamples) << "\n";
    s << "Bits per sample: " << (int)reader->bitsPerSample << "\n";
    s << "Floating point data: " << ((reader->usesFloatingPointData) ? "yes" : "no") << "\n";
    const juce::StringArray& keys = reader->metadataValues.getAllKeys();
    const juce::StringArray& vals = reader->metadataValues.getAllValues();
    for(int j = 0; j < keys.size(); j++)
    {
      s << keys[j];
      s << ": " << vals[j] << "\n";
    }
    l->setTooltip(s);
  }
}

//-------------------//
//Transport Callbacks//
//-------------------//

void AudioFilePlayer::play(double position)
{
  //  std::cout << "AudioFilePlayer::play(pos=" << position << ")\n" ;
  if (audioFileReader == 0 || audioFileReader->getTotalLength() == 0)
    return;
  transportSource.start();
  double now = transportSource.getCurrentPosition();
  double end = transportSource.getLengthInSeconds();
  if (now >= end)
  {
    transportSource.setPosition(0);
    // update slider but do not trigger its callback
    transport->setPlaybackPosition(0.0, false);
  }
}

void AudioFilePlayer::pause(void)
{
  //  std::cout << "AudioFilePlayer::pause()\n" ;
  transportSource.stop();
}

void AudioFilePlayer::positionChanged(double position, bool isPlaying)
{
  //  std::cout << "AudioFilePlayer::positionChanged(" << position << ", " << isPlaying << ")\n" ;
  if (position == 0)
  {
    // setSource(0) stops popping on rewind and play
    transportSource.setSource(0);
    transportSource.setSource(audioFileReader.get());
    if (isPlaying)
      transportSource.start();
  }
  else
    transportSource.setPosition(position);
}

void AudioFilePlayer::gainChanged(double gain, bool isPlaying)
{
  //  std::cout << "AudioFilePlayer::gainChanged(" << gain << ", " << isPlaying << ")\n" ;
  transportSource.setGain((float)gain);
}

