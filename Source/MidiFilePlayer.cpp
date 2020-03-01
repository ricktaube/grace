/*=======================================================================*
  Copyright (C) 2013 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#include "MidiFilePlayer.h"
#include "Enumerations.h"
#include "Midi.h"

//==============================================================================
// MidiFilePlayerWindow
//==============================================================================

MidiFilePlayer:: MidiFilePlayer ()
  : juce::DocumentWindow("", ColorThemeIDs::getWindowBackgroundColor(),
                         juce::DocumentWindow::closeButton),
    playbackThread(0),
    transport (0),
    midiFile (juce::File()),
    midiFileLength (0),
    midiFileDuration (0.0)
{
  // window init
  setVisible(true);
  setUsingNativeTitleBar(true);
  setDropShadowEnabled(true);
  setCentreRelative(0.5, 0.5);
  setResizable(false, false);
  // transport init
  transport = new Transport(this, true, true);
  setSize(transport->getWidth(), transport->getHeight());
  setContentOwned(transport, false);
  // playback init
  playbackThread=new MidiPlaybackThread(this, 100, 60.0, transport);
  playbackThread->startThread();
  WindowTypes::setWindowType(this, WindowTypes::MidiFilePlayer);
}

MidiFilePlayer::~MidiFilePlayer()
{
  transport->setPausing(true);
  playbackThread->stopThread(100);
  delete playbackThread;
  sequence.clear();
}

void MidiFilePlayer::closeButtonPressed()
{
  delete this;
}

void MidiFilePlayer::openMidiFilePlayer(juce::File fileToOpen, bool autoPlay)
{
  static juce::File lastOpened = juce::File();
  MidiFilePlayer* win = 0; 

  if (fileToOpen.existsAsFile())
  {
    // reuse a player if its already open for the file
    for (int i = 0; i < juce::TopLevelWindow::getNumTopLevelWindows(); i++)
    {
      if (MidiFilePlayer* w = dynamic_cast<MidiFilePlayer*>(juce::TopLevelWindow::getTopLevelWindow(i)))
      {
        win = w;
        if (w->midiFile == fileToOpen)
          break;
        win = 0;
      }
    }
  }
  else  // select file if one is not provided or is bogus
  {
    juce::FileChooser choose ("Play Midi File", lastOpened, "*.mid;*.midi");
    if (choose.browseForFileToOpen())
    {
      fileToOpen = choose.getResult();
      lastOpened = fileToOpen;
    }
    else 
      return;
  }

  // create new player or raise old one to top of window stack
  if (win == 0)
    win = new MidiFilePlayer();
  else
    win->getTopLevelComponent()->toFront(true); 
  // set the player's file to our file
  win->setFile(fileToOpen);
}

void MidiFilePlayer::setFile(juce::File file)
{
  // pushes the stop and rewind buttons to halt playback and clears
  // any pending messages
  transport->setPausing();
  transport->setPlaybackPosition(0);

  midiFileDuration = 0;
  midiFileLength = 0;
  sequence.clear();
  midiFile = file;
  // create input stream for midi file and read it
  juce::FileInputStream input (midiFile);
  if (!input.openedOk())
    return;
  juce::MidiFile midifile;
  if (!midifile.readFrom(input))
    return;
  setName(file.getFileName());
  int numtracks = midifile.getNumTracks();
  int timeformat = midifile.getTimeFormat();
  double smptetps = 0.0; // smpte format ticks per second
  // juce time format is postive for beats and negative for SMTPE
  if (timeformat > 0)
  {
    midifile.convertTimestampTicksToSeconds();    
  }
  else
  {
    // determine the smpte ticks per second. juce smpte frames per
    // second is negative upper byte
    int fps = 0xFF-((timeformat & 0xFF00) >> 8) + 1;
    int tpf = timeformat & 0xFF;
    smptetps = fps * tpf;
  }
  // iterate all the tracks in the file and merge them into our
  // empty playback sequence.
  for (int track = 0; track < numtracks; track++)
  {
    const juce::MidiMessageSequence* seq = midifile.getTrack(track);
    // if file is in SMPTE FRAMES then convert it to seconds by
    // hand. (juce's convertTimestampTicksToSeconds doesnt work for
    // SMPTE format.)
    if (timeformat < 0) // is smpte
    {
      for (int i = 0; i < seq->getNumEvents(); i++)
      {
        juce::MidiMessageSequence::MidiEventHolder* h = seq->getEventPointer(i);
        double t = h->message.getTimeStamp();
        h->message.setTimeStamp(t / smptetps);
      }
    }
    // merge file track into our single playback sequence
    sequence.addSequence(*seq, 0.0, 0.0, seq->getEndTime() + 1);
    sequence.updateMatchedPairs();
  }

  // the file may include more than the note ons and offs so the
  // file length may not reflect the actual number of events that we
  // play. set file playback duration to the very last note event
  midiFileLength = sequence.getNumEvents();
  // FIXME: REPLACE MIDIFILEDURATON WITH START AND END TIMES
  if(midiFileLength > 0)
  {
    midiFileDuration = sequence.getEndTime();
  }
  // set the playback range to our upper bounds, note that the length
  // may include meta message
  playbackThread->setPlaybackLimit(midiFileDuration, midiFileLength);
  transport->setPlaybackRange(0.0, midiFileDuration);
}

//-------------------//
//Transport Callbacks//
//-------------------//

void MidiFilePlayer::play(double position)
{
  //  std::cout << "MidiFilePlayer::play(pos=" << position << ")\n" ;
  if (playbackThread->isPlaybackAtEnd())
    playbackThread->setPlaybackPosition(0.0, 0);
  playbackThread->setPaused(false);
}

void MidiFilePlayer::pause()
{
  //  std::cout << "MidiFilePlayer::pause()\n" ;
  playbackThread->setPaused(true);
}

void MidiFilePlayer::positionChanged(double position, bool isPlaying)
{
  //  std::cout << "MidiFilePlayer::positionChanged(" << position << ", " << isPlaying << ")\n" ;
  // convert the normalized position into a real position in the
  // playback sequence and update the MidiPlaybackThread's playback
  // position
  double beat = position ; // * midiFileDuration;
  int index = sequence.getNextIndexAtTime(beat);
  if (position >= midiFileDuration)
    index = midiFileLength;
  // pause playback thread if its playing
  if (isPlaying)
    playbackThread->pause();
  playbackThread->clear();
  playbackThread->setPlaybackPosition(beat, index);
  // restart playback if we paused
  if (isPlaying)
    playbackThread->play();
}

void MidiFilePlayer::gainChanged(double gain, bool isPlaying)
{
  //  std::cout << "MidiFilePlayer::gainChanged(" << gain << ", " << isPlaying << ")\n" ;
  playbackThread->setGain(gain);
}

void MidiFilePlayer::tempoChanged(double tempo, bool isPlaying)
{
  //  std::cout << "MidiFilePlayer::tempoChanged(" << tempo << ", " << isPlaying << ")\n" ;
  playbackThread->setTempo(tempo);
}

//----------------------------//
//MidiPlaybackThread Callbacks//
//----------------------------//

void MidiFilePlayer::addMidiPlaybackMessages(MidiPlaybackThread::MidiMessageQueue& queue, 
                                             MidiPlaybackThread::PlaybackPosition& position)
{
  int index = position.index;
  for (; index < position.length; index++)
  {
    juce::MidiMessageSequence::MidiEventHolder* ev=sequence.getEventPointer(index);
    // skip over non-channel messages
    if (ev->message.getChannel() < 1)
      continue;
    // skip over noteOffs because we add by pairs with noteOns
    if (ev->message.isNoteOff())
      continue;
    if (ev->message.getTimeStamp() <= position.beat)
    {
      queue.addMessage(new juce::MidiMessage(ev->message));
      if (ev->noteOffObject)
      {
        queue.addMessage(new juce::MidiMessage(ev->noteOffObject->message));            
      }
    }
    else
      break;
  }
  // index is now the index of the next (future) event or length
  position.index = index;
}

void MidiFilePlayer::handleMessage(juce::MidiMessage& midiMessage)
{
  MidiOutPort::getInstance()->sendOut(midiMessage);
}
