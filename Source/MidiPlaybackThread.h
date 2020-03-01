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
#include "Transport.h"

/** A thread that plays midi messages out a midi port in real time. To
    use a MidiPlaybackThread first call startThread() and then
    setPlaybackLimit() to set its max playback values. Call play() to
    start the midi playback, pause() to pause it and setTempo()
    setPlaybackPosition() to modify it. If you pass a Transport object
    to the thread it will periodically recieve positional updates from
    the thread if you have set a playback limit. **/

class MidiPlaybackThread : public juce::Thread
{

public:

  /** The playback position in a MidiPlayback thread. Holds the current
      playback time in beats, a stop flag and a playback index that the
      caller can use as a running index into its sequence of midi
      data. The PlaybackPostion is passed to the source's
      addMidiPlaybackMessages() function which can safely read the beat
      and read/write the index and stop values.  The beat value is
      incremeted by the thread, the source is responsible for updating
      the index and stop if it is using them.  Playback automatically
      ends as soon as the position's beat is greater than its endbeat,
      or its index is equal to its length and then length is greater
      than 0, or the stop flag is set to true.  **/

  class PlaybackPosition
  {

  public:

    /** The thread's current playback position expressed in beats
        without regard to tempo. The beat is incremented a tick's
        worth of beat time on each iteration. See setTicksPerBeat()
        for an explantion. **/

    double beat;

    /** Specifies the ending time in beats for playback.  Playback
        automatically stops once the the position's beat exceeds this
        value. **/

    double endbeat; 

    /** Specifies the amount of beat the thread waits on each
        iteration **/

    double tick;

    /** An index for the caller to use if it wants to maintain a
        running position in its sequence of data during
        playback. Caller is reponsible for incrementing the index if
        using it. If the caller has specifed a length to
        setPlaybackLimit() then playback will stop once th index is
        equal to or greater than the position's length value. **/

    int index;

    /** Specifies the maximum number of positions the source has in
        its sequenence of midi data. If the length is greater than
        zero then playback will automatically stop once the index
        reaches it. **/

    int length;

    /** Stops playback if set to true. **/

    bool stop;

    /** Internal PlaybackPosition constructor */

    PlaybackPosition()
      : beat (0.0),
        endbeat (0.0),
        tick (0.0),
        index (0),
        length (0),
        stop (false)
    {
    }
  
    /** Internal PlaybackPosition destructor */

    ~PlaybackPosition()
    {
    }
  
    /** Internal function returns true if stop is true, or the
        postion's beat is greater than its endbeat, or its index
        reaches its length and the length is greater than 0. **/

    bool isAtEnd()
    {
      // arrgh! floating point error can make beat>endbeat test
      // not work. fix for now is too use a 1/2 tick fudge
      // factor to allow for some error in the comparison
      return (stop || (beat > (endbeat + (tick/2))) || ((length > 0) && (index >= length)) );
    }
    
    /** Internal function resets the position back to its starting
        values. **/

    void rewind()
    {
      beat=0.0;
      index=0;
      stop=false;
    }
  
  };

  /**A queue of time sorted midi messages passed to
     addMidiPlaybackMessages so messages can be added. Messages are
     owned by the queue and deleted after they have been passed to
     handleMessage().*/
  class MidiMessageQueue : public juce::OwnedArray<juce::MidiMessage>
  {

  public:

    /** Internal MidiMessageQueue constuctor. **/

    MidiMessageQueue(){}

    /** Internal MidiMessageQueue destructor. **/

    ~MidiMessageQueue(){}

    /** Adds a MidiMessage to the thread's playback queue. Once
        messages are added they are owned by the queue and will be
        automatically deleted once they are sent out the midi
        port. Added messages are inserted at the appopriate position in
        the queue according to the message time stamp. Time stamps are
        in beats, not seconds, and this beat time is automatically
        scaled by a tempo factor during playback. Once a message has
        been sent out the midiport it is deleted by the thread. **/
  
    void addMessage(juce::MidiMessage* msg)
    {
      addSorted(*this, msg);
    }

    /** Comparator keeps the queue in time sorted order but places
        note offs before anything else at the same time stamp to avoid
        note clipping. All other message types are added at the last
        possible position for their time. **/

    static int compareElements(juce::MidiMessage* a, juce::MidiMessage* b)
    {
      if (a->getTimeStamp() < b->getTimeStamp())
        return -1;
      else if (b->getTimeStamp() < a->getTimeStamp())
        return 1;
      else
      {
        if (a->isNoteOff())
        {
          if (b->isNoteOff())
            return 0;
          else 
            return -1;
        }
        else
        {
          if (b->isNoteOff())
            return 1;
          else
            return 0;
        }
      }
    } 
  };

  /** A class to receive callbacks from a MidiPlaybackThread to add
      midi messages to its playback queue. **/

  class MidiMessageSource
  {
  public:
    
    /**Provides midi data for the thread to play. Called on every
       tick (quantum of the beat) with a MidiMessageQueue and
       PlaybackPosition so that the source can add midi messsages to
       the MidiMessageQueue at time stamps equal to or later than the
       current beat time in the PlaybackPosition. The position's beat
       time is managed by the thread and should never be set by the
       source. The thread stops calling addMidiPlaybackMessages()
       after the position's beat or index reach their playback limits
       established by MidiPlaybackThread::setPlaybackLimits() or the
       position's stop flag had been set.*/
    virtual ~MidiMessageSource(){}
    virtual void addMidiPlaybackMessages(MidiMessageQueue& queue, PlaybackPosition& position) = 0;
    /**Called to handle message when its time is current.*/
    virtual void handleMessage(juce::MidiMessage& midiMessage) = 0;
  };

private:

  MidiMessageQueue messages; // midi message queue, time is in beats
  PlaybackPosition position; // playback position with current beat and user index
  MidiMessageSource* source; // called every tick to add messages to the queue
  Transport* transport;      // transport to update while running
  juce::CriticalSection pblock; // playback lock for accessing thread state
  bool paused;               // true if thread is paused false if its running
  double tempo;              // metronome speed of beat
  double gain;
  int ticks;                 // number of times per beat the source is called

public:

  /** Creates a MidiPlaybackThread that plays midi messages out a midi
      port in real time. Midisource is the source that will provide
      midi messages. TicksPerBeat is the number of times per beat the
      source will called to add its messages. BeatsPerMinute is the
      initial metronome tempo for playback. Midiout is the MIDI device
      to send MIDI messages to. If Transport is not null the thread
      will periodically send messages during playback to update its
      sliders an buttons. **/

  MidiPlaybackThread(MidiMessageSource* midiSource, int ticksPerBeat, double beatsPerMinute=60.0,
                     Transport* transport = 0)
    : juce::Thread ("Midi Playback Thread"),
      source (midiSource),
      transport (transport),
      paused (true),
      tempo (beatsPerMinute),
      gain(1.0),
      ticks (ticksPerBeat)
  {   
    position.tick= 1.0/ticksPerBeat;
  }
  
  ~MidiPlaybackThread()
  {
    messages.clear();
    std::cout << "deleted MidiPlaybackThread\n";
  }

  /** Sets the transport that will receive update messages from
      the thread. **/

  void setTransport(Transport* transprt)
  {
    juce::ScopedLock mylock(pblock);
    transport=transprt;
  }

  /** Sets the maximum beat and index range for playback. playback
      will autopause when beat>endBeat or index is >= length. **/

  void setPlaybackLimit(double endbeat, int length=0)
  {
    juce::ScopedLock mylock(pblock);
    position.endbeat=endbeat;
    position.length=length;
  }

  /** Sets the playback position to the specified beat. To update the
      position's index as well specify an index value 0 or larger. **/

  void setPlaybackPosition(double beat, int index=-1)
  {
    juce::ScopedLock mylock(pblock);
    position.beat = beat;
    if (index>-1)
      position.index=index;      
  }

  bool isPlaybackAtEnd()
  {
    juce::ScopedLock mylock(pblock);
    return position.isAtEnd();
  }

  /** Sets the thread's playback tempo in beats per minute. This value
      is used to convert playback beat time into seconds. **/

  void setTempo(double bpm)
  {
    juce::ScopedLock mylock(pblock);
    if (bpm<=0) bpm=60;
    tempo=bpm;
  }

  /** Specifies the the number of times per beat that the thread calls
      its MidiMessageSource to add midi messages to the
      queue. TicksPerBeat is converted into a time delta (a tick) that
      is scaled by the thread's tempo value to determine the actual
      amount of time the thread waits between interations. **/

  void setTicksPerBeat(double tpb)
  {
    juce::ScopedLock mylock(pblock);
    if (tpb<1) tpb=1;
    ticks=(int)tpb;
    position.tick=1.0/tpb;
  }

  void setGain(double value)
  {
    juce::ScopedLock mylock(pblock);
    gain = juce::jlimit(0.0, 1.0, value);
  }

  /** Returns true if the thread is currently paused. **/

  bool isPaused()
  {
    juce::ScopedLock mylock(pblock);
    return paused;
  }

  /** Returns true if the thread is currently playing. **/

  bool isPlaying()
  {
    return !isPaused();
  }

  /** A wrapper that starts the thread running if it is paused. **/

  void play()
  {
    if (isPaused())
      setPaused(false);
  }

  /** A wrapper that pauses the thread if it is running. **/

  void pause()
  {
    if (isPlaying())
      setPaused(true);
  }

  /** Puts the thread in pause or play mode. **/

  void setPaused(bool pause)
  {
    juce::ScopedLock mylock(pblock);
    if (paused != pause) 
    {
      paused=pause;
      if (!paused) // wake up thread if we have stopped pausing
        notify();
      else
      {
        // send allNotesOff if pausing with messages still in queue.
        // not sure if we need to lock the queue to check its size...
        if (messages.size() > 0)
          sendAllNotesOff();
      }
    }
  }

  /** Microtunes successive channels in the output device according to
      the number of divisions per semitone, a value 1 to 16
      inclusive. The starting channel is channel offset to begin
      tuning at, a value 0 to 15 inclusive. **/

  void sendMicrotuning(int divs, int startingChannel=0)
  {
    juce::ScopedLock mylock(pblock);
    divs=juce::jlimit(1,16,divs);
    int chan=startingChannel;
    for (; chan<divs && chan<16; chan++)
    {
      double frac=(double)((double)chan/(double)divs);
      // juce pitch bends are 0 to 16383 with slider 0 == 8192 and
      // range -2 to 2 semitones so: bend=rescale(slider,-2,2,0,16383)
      int bend=(int)(4095.75 * (frac + 2));
      std::cout << "sending pitchbend, chan=" << chan << " bend=" << bend << "\n";
      juce::MidiMessage msg =  juce::MidiMessage::pitchWheel(chan + 1, bend);
      processMessage(msg);
    }
    // adjust all higher channels to slider position 0 (no tuning)
    for (; chan<16; chan++)
    {
      juce::MidiMessage msg = juce::MidiMessage::pitchWheel(chan + 1,8192);
      processMessage(msg);
    }
  }

  /** Utility function to microtune a message by shifting it the
      appropiate number of channels for the specified divisions and
      fractional keynum. The message should already have its base
      channel and (integer) keynum values assigned.  **/

  static void microtuneMessage(juce::MidiMessage& msg, int divs, double knum)
  {
    int base=(int)knum;
    double frac=knum-base;
    // how many channels we have to shift for the resolution
    int shift=(int)(frac/divs);
    if (shift>0)
      msg.setChannel(msg.getChannel()+shift);
  }

  /** Clears the queue of pending messages. Don't call this unless the
      thread is currently paused or you will pobably have dangling
      midi notes to deal with!  **/

  void clear()
  {
    messages.clear();
  }

private:

  /** Main loop for the MidiPlaybackTread. On each iteration it calls
      the source's addMidiPlaybackMessages() to get more Midi message
      to play and then plays them out the midi port. The run loop can
      be paused and resumed using the thread's play() and pause()
      methods. If a transport is associate with the thread then the
      transport will be messaged periodically to update its slider
      position to reflect the current playback beat. **/
  void run ()
  {
    int transpcounter = 0;  // send transport updates at a slower rate than the playback rate
    while (!threadShouldExit())
    {
      // lock thread variables and copy to locals
      pblock.enter();
      bool ispaused = paused;
      bool isatend = position.isAtEnd();
      int  pending = messages.size();
      double bpm = tempo;
      pblock.exit();

      // if playback is paused then block until user unblocks us
      if (ispaused)
      {
        //        std::cout << "playback thread is paused\n";
        transpcounter=0;
        wait(-1);
      }
      else
      {
        double pos = 0;
        // if position has reached its limits then dont
        // try to get any more messages from the source
        if (isatend)
        {
          // we are at the end of the source but we may still have
          // pending messages in the queue if future ones were added
          if (pending == 0) 
          {
            // nothing in the queue, rewind the position and autopause
            pblock.enter();
            paused = true;
            pblock.exit();
            if (transport)
            {
              transport->sendMessage(Transport::SetPausing, -1, false);
              transport->sendMessage(Transport::MoveToEnd, -1, false);
            }
            continue;
          }
          else
          {
            // if we are here then the playback position is beyond the
            // end of the source there are still pending messages left
            // in the queue to process
            pblock.enter();
            double now = position.beat;
            pos = now / position.endbeat;
            juce::MidiMessage* msg = 0; 
            while ((msg = messages.getFirst()))
            {
              if (msg->getTimeStamp() <= now)              
              {
                messages.remove(0, false); // pop message off queue
                processMessage(*msg);      // process message
                delete msg;
              }
              else
              {
                break;
              }
            }
            position.beat += position.tick;
            pblock.exit();
          }
        }
        else
        {
          // if we are here then the playback position is still valid
          // so we call source function to add messages to the queue
          // and then process the messages for the current beat
          pblock.enter();
          double now = position.beat;
          pos = now / position.endbeat;

          // call sources's routine to get more messages pblock must
          // be unlocked because otherwise the message thread could
          // cause a deadlock if it called pause or play here...
          /* source->addMidiPlaybackMessages(messages, position);*/
          {
            MidiPlaybackThread::MidiMessageQueue localQueue;
            MidiPlaybackThread::PlaybackPosition localPosition = position;
            pblock.exit();
            source->addMidiPlaybackMessages(localQueue, localPosition);
            pblock.enter();
            for(int i = 0; i < localQueue.size(); i++)
              messages.addMessage(localQueue[i]);
            localQueue.clear(false);
            position = localPosition;
          }
          juce::MidiMessage* msg = messages.getFirst();          
          while (msg && msg->getTimeStamp() <= now)
          {
            messages.remove(0, false); // pop message off queue
            processMessage(*msg);       // send message out port
            delete msg;
            msg=messages.getFirst();   // increment to next
          }
          position.beat += position.tick;  // increment beat
          pblock.exit();
        }
        // update position of transport's slider at 1/10 the playback rate
        if (transport)
        {
          if (transpcounter == 0)
          {
            // update the transport's position slider without triggering its action
            pblock.enter();
            transport->sendMessage(Transport::SetPlaybackPosition, position.beat, false);
            pblock.exit();
          }
          else
            transpcounter = ((transpcounter + 1) % 10);
        }
        // 'tick' is the wait time expressed in beats, without regard
        // to tempo. calculate the amount of time to wait between
        // iterations by converting ticks to milliseconds at the
        // current tempom e.g  wait(1000 * tick * (60/bpm));

        wait((int)(60000 * (position.tick / bpm)));

      }
    }
    //    std::cout << "exiting MidiPlaybackThread\n";
  }
  
  /** Internal function sends a message out the midi port. **/
  void processMessage(juce::MidiMessage& msg)
  {
    if (msg.isNoteOn())
    {
      juce::ScopedLock mylock(pblock);
      if (gain < 1.0)
      {
        msg.setVelocity(msg.getFloatVelocity() * (float)gain);
      }
    }
    if (0) printMidiMessage(msg);
    //   MidiOutPort::getInstance()->sendOut(msg);
    source->handleMessage(msg);
  }
  
  /** Internal function that sends an allNotesOff if we pause or move
      the transport with pending messages in the queue. **/

  void sendAllNotesOff()
  {
    for (int i = 1; i <= 16; i++)
    {
      juce::MidiMessage msg = juce::MidiMessage::allSoundOff(i);
      processMessage(msg);
    }
  }

  void printMidiMessage(juce::MidiMessage& msg)
  {
    int op=(msg.getRawData()[0] & 0xf0)>>4;
    int ch=msg.getChannel()-1;
    int d1=msg.getRawData()[1] & 0x7f;
    int d2=(msg.getRawDataSize() > 2) ? (msg.getRawData()[2] & 0x7f) : 0;
    if (op==9 && d2==0) op=8; // show as note off
    /*    juce::String text = juce::String(msg.getTimeStamp());
    text << " op=" << juce::String(op) << " ch=" << juce::String(ch) << " d1=" << juce::String(d1) << " d2=" << juce::String(d2);
    const char* wtf = text.toUTF8();
    std::cout << wtf << "\n" << std::flush;
    */
    std::cout << " op=" << juce::String(op) << " ch=" << juce::String(ch)
              << " d1=" << juce::String(d1) << " d2=" << juce::String(d2) << "\n";
  }
  
};
