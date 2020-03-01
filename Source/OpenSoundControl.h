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

class OpenSoundControl : private juce::OSCReceiver::Listener<juce::OSCReceiver::MessageLoopCallback>
			 // also possible: juce::OSCReceiver::RealtimeCallback
{
private:
  juce::OSCReceiver receiver;
  juce::OSCSender sender;
  int inputPort;
  juce::String outputHost;
  int outputPort;
  bool tracing;
  juce::CriticalSection oscLock;
  friend class OscConnectionDialog;

public:
 
  enum {
    OSC_INT32 =     'i',	/// 32 bit signed integer.
    OSC_FLOAT =     'f',	/// 32 bit IEEE-754 float.
    OSC_STRING =    's',	/// Standard C, NULL terminated string.
    OSC_BLOB =      'b',	/// OSC binary blob type. 
    OSC_INT64 =     'h',	/// 64 bit signed integer.
    OSC_TIMETAG =   't',	/// OSC TimeTag type.
    OSC_DOUBLE =    'd',	/// 64 bit IEEE-754 double.
    OSC_SYMBOL =    'S',        /// OSC symbol
    OSC_CHAR =      'c',	/// Standard C, 8 bit, char variable.
    OSC_MIDI =      'm',	/// A 4 byte MIDI packet.
    OSC_TRUE =      'T',	/// OSC value True.
    OSC_FALSE =     'F',	/// OSC value False.
    OSC_NIL =       'N',	/// OSC value Nil.
    OSC_INFINITUM = 'I'	        /// OSC value Infinitum.
  };
  ///Constuctor
  OpenSoundControl();

  ///Destructor
  ~OpenSoundControl();

  /// Opens an OSC input connection on the specified port. Returns true if successful otherwise false.
  bool openInput(int port);

  /** Opens an OSC output connection to the specified host and
      port. Returns true if successful otherwise false.
  */
  bool openOutput(juce::String host, int port);

  /// Closes the open input connection.  Returns true if successful otherwise false.
  bool closeInput();

  /// Disconnects from the specified output port.  Returns true if successful otherwise false.
  bool closeOutput();

  /// Returns the active input port or 0 if there is none.
  int getInputPort();

  /// Returns the active output port or 0 is there is none.
  int getOutputPort();

  /// Returns the active output host or "" is there is none.
  const juce::String getOutputHost();

  /// Returns true if input tracing is active.
  bool isTracing();

  /// Sets input tracing active
  void setTracing(bool isActive);

  /// Called by Scheme thread to send a message.
  bool sendMessage(juce::String path, s7_pointer message);

  /// Called by Scheme thread to send a bundle.
  bool sendBundle(double time, s7_pointer bundle);

  /** Parse a Scheme osc message into a juce OSCMessage. Returns
      pointer to data if successful else null. If parsing fails then
      lastOscError will hold the error string.
  */
  juce::OSCMessage* parseMessage(s7_pointer list, juce::String path);

  /** Parse a scheme osc bundle and fills array with message
      data. Returns true if successful.
  */
  juce::OSCBundle* parseBundle(s7_pointer list, double ahead);

  // Callback for OscReceiver::Listener
  void oscMessageReceived (const juce::OSCMessage& message);

  // Callback for OscReceiver::Listener
  void oscBundleReceived (const juce::OSCBundle& bundle);
  
  /** Sending test for osc messages. Should only be called on the
      message thread.
   */
  void testOutput();

  /** Print an incoming osc message to the console. Should only be called
      on the message thread.
   */
  void traceInput(const juce::OSCMessage& message);
  void traceInput(const juce::OSCBundle& bundle);
  
  /// Opens the OSC Connections dialog.
  void openConnectionDialog();

  juce_DeclareSingleton(OpenSoundControl, true)
};
