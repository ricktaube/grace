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

#include "OpenSoundControl.h"
#include "Scheme.h"
#include "Console.h"
#include "Preferences.h"
#include "osc/OscOutboundPacketStream.h"
#include "ip/IpEndpointName.h"
#include "osc/OscReceivedElements.h"
#include "osc/OscPacketListener.h"
#include "ip/UdpSocket.h"

struct OscPackConnection : public OpenSoundControl::Connection,
                           public osc::OscPacketListener,
                           public juce::Thread
{
  static const int OscBufferSize = 1024;
  // Offset for OSC TimeTags (juce clock starts 1970 not 1900)
  static const juce::uint64 JAN_1_1970 = 0x83aa7e80 ;
  UdpTransmitSocket outgoingSocket;
  char buffer[OscBufferSize];
  osc::OutboundPacketStream packetStream;
  UdpListeningReceiveSocket* incomingSocket;
  
  OscPackConnection(OpenSoundControl* osc, int inPort, juce::String outHost, int outPort) 
    : OpenSoundControl::Connection(osc, inPort, outHost, outPort),
      juce::Thread("OscPack Connection"),
      outgoingSocket(IpEndpointName(outHost.toUTF8(), outPort)),
      packetStream(buffer, OscBufferSize)
  {
    // malloc stops vs warning about passing 'this' in initializations
    incomingSocket = new UdpListeningReceiveSocket(IpEndpointName("localhost", inPort), this);
  }

  ~OscPackConnection()
  {
    incomingSocket->AsynchronousBreak();
    stopThread(2500);
    delete incomingSocket;
  }

  void run()
  {
    incomingSocket->Run();
  }

  void ProcessMessage(const osc::ReceivedMessage& m, const IpEndpointName& remoteEndpoint)
  {
    juce::String path (m.AddressPattern());
    OscHook* hook = SchemeThread::getInstance()->getOscHook(path);

    // nothing to do if no hook and not tracing
    if (!hook && !osc->isTracingActive(false))
      return;
    // A subclass of OscMessageData that scheme schedules
    XOscNode* data = new XOscNode(0.0, path, "");
    try 
    {
      for (osc::ReceivedMessage::const_iterator arg = m.ArgumentsBegin();
           arg != m.ArgumentsEnd(); arg++)
      {
        if (arg->IsBool())
        {
          data->oscTypes << (arg->AsBool() ? (char)OpenSoundControl::OSC_TRUE
                             : (char)OpenSoundControl::OSC_FALSE) ;
        }
        else if (arg->IsNil())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_NIL;
        }
        else if (arg->IsInfinitum())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_INFINITUM;
        }
        else if (arg->IsInt32())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_INT32;
          data->oscInts.add((juce::int64)arg->AsInt32());
        }
        else if (arg->IsInt64())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_INT64;
          data->oscInts.add((juce::int64)arg->AsInt64());
        }
        else if (arg->IsTimeTag())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_TIMETAG;
          // give scheme a time tag in second
          double d = (arg->AsInt64() - JAN_1_1970) / 1000.0;
          data->oscFloats.add((float)d);
        }
        else if (arg->IsFloat())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_FLOAT;
          data->oscFloats.add(arg->AsFloat());
        }
        else if (arg->IsDouble())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_DOUBLE;
          data->oscFloats.add(arg->AsDouble());
        }
        else if (arg->IsString())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_STRING;
          data->oscStrings.add(juce::String(arg->AsString()));
        }
        else if (arg->IsSymbol())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_SYMBOL;
          data->oscStrings.add(juce::String(arg->AsSymbol()));
        }
        else if (arg->IsChar())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_CHAR;
          juce::String c;
          c << arg->AsChar();
          data->oscStrings.add(c);
        }
        else if (arg->IsMidiMessage())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_MIDI;
          osc::uint32 m = arg->AsMidiMessage();
          data->oscInts.add(4); // size
          data->oscInts.add((m & 0xFF000000) >> 24);
          data->oscInts.add((m & 0x00FF0000) >> 16);
          data->oscInts.add((m & 0x0000FF00) >>  8);
          data->oscInts.add((m & 0x000000FF));
        }
        else if (arg->IsBlob())
        {
          data->oscTypes << (char)OpenSoundControl::OSC_BLOB;
          const void* blob;
	  //          int size;
	  osc::osc_bundle_element_size_t size;
          arg->AsBlob(blob, size);
          juce::MemoryBlock buff (blob, size);
          data->oscInts.add(size); // size
          for (int i = 0; i < size; i++)
            data->oscInts.add((juce::int64)buff[i]);
        }
      }
    }
    catch (osc::Exception& e)
    {
      // any parsing errors such as unexpected argument types, or
      // missing arguments get thrown as exceptions.
      juce::String err (">>>Error: ");
      err << "error while parsing OSC message: "
          << m.AddressPattern() << ": " << e.what() << "\n";
      delete data;
      Console::getInstance()->printError(err);
      return;
    }

    if (osc->isTracingActive(false))
    {
      juce::String trace = data->toString();
      trace << "\n";
      Console::getInstance()->printOutput(trace);
    }
    if (hook)
    {
      data->hook = hook;
      // data will be owned by scheme
      SchemeThread::getInstance()->addNode(data);
    }
    else
      delete data;
  }

  bool sendMessage(OpenSoundControl::OscMessageData& data) 
  {
    //    std::cout << "in OscPack::sendMessage\n";
    addMessageData(data);
    outgoingSocket.Send(packetStream.Data(), packetStream.Size());
    packetStream.Clear();
    return true;
  }

  bool sendBundle(double ahead, juce::OwnedArray<OpenSoundControl::OscMessageData>& bundle)
  {
    //    std::cout << "in OscPack::sendBundle, bundle size=" << bundle.size() << "\n";
    if (ahead > 0.0)
      packetStream << osc::BeginBundle(OpenSoundControl::OscMessageData::toTimeTag(ahead));
    else
      packetStream << osc::BeginBundleImmediate;
    for (int i = 0; i < bundle.size(); i++)
      addMessageData(*bundle[i]);
    packetStream << osc::EndBundle;
    outgoingSocket.Send(packetStream.Data(), packetStream.Size());
    packetStream.Clear();
    return true;
  }

  void addMessageData(OpenSoundControl::OscMessageData& data)
  {
    //    std::cout << "in OscPack::addMessageData\n";
    packetStream << osc::BeginMessage(data.oscPath.toUTF8());
    int S = 0, I = 0, F = 0;
    for(int i = 0; i < data.oscTypes.length(); i++)
    {
      char c = data.oscTypes[i];
      switch (c)
      {
      case OpenSoundControl::OSC_TRUE:
        packetStream << true;
        break;
      case OpenSoundControl::OSC_FALSE:
        packetStream << false;
        break;
      case OpenSoundControl::OSC_NIL:
        packetStream << osc::OscNil;
        break;
      case OpenSoundControl::OSC_INFINITUM:
        packetStream << osc::Infinitum;
        break;
      case OpenSoundControl::OSC_INT32:
        packetStream << (osc::int32)data.oscInts[I++];
        break;
      case OpenSoundControl::OSC_INT64:
        packetStream << (osc::int64)data.oscInts[I++];
        break;
      case OpenSoundControl::OSC_FLOAT:
        packetStream << (float)data.oscFloats[F++];
        break;
      case OpenSoundControl::OSC_DOUBLE:
        packetStream << (double)data.oscFloats[F++];
        break;
      case OpenSoundControl::OSC_TIMETAG:
        {
          juce::uint64 t = OpenSoundControl::OscMessageData::toTimeTag(data.oscFloats[F++]);
          packetStream << osc::TimeTag((juce::uint64)t);
        }
        break;
      case OpenSoundControl::OSC_STRING:
        packetStream << data.oscStrings[S++].toUTF8();
        break;
      case OpenSoundControl::OSC_SYMBOL:
        packetStream << osc::Symbol(data.oscStrings[S++].toUTF8());
        break;
      case OpenSoundControl::OSC_CHAR:
        {
          char c = data.oscStrings[S++][0];
          packetStream << c;
        }
        break;
      case OpenSoundControl::OSC_BLOB:
        {
          int s = (int)data.oscInts[I++];
	  juce::String b;
          for (int i = 0; i < s ; i++)
            b << (char)(data.oscInts[I++] & 0xFF);
          packetStream << osc::Blob(b.toUTF8(), s);
        }
        break;
      case OpenSoundControl::OSC_MIDI:
        {
          int p = 24;
          osc::uint32 m = 0;
          int s = (int)data.oscInts[I++];
          for(int i = 0; i<s && p >= 0; i++)
          {
            m |= ((data.oscInts[I++] & 0xFF) << p);
            p -= 8;
          }
          packetStream << osc::MidiMessage(m);
        }
        break;
      }
    }
    packetStream << osc::EndMessage;
  }

};

bool OpenSoundControl::openConnection(int inputPort, juce::String outputHost, int outputPort)
{
  lastOscError = "";
  // if called from scheme signal error if no implementation
  if (!isAvailable())
  {
    lastOscError = "OSC is not available in this build of Grace.";
    return false;
  }
  if (connection) return true;
  OscPackConnection* c = 0;
  try
  {
    //    std::cout << "creating OscPackConnection\n";
    c = new OscPackConnection(this, inputPort, outputHost, outputPort);
    c->startThread();
    //    std::cout << "done creating OscPackConnection!\n";
  }
  catch (std::runtime_error& e)
  {
    lastOscError = juce::String(e.what());
    delete c;
    c = 0;
    //    std::cout << "runtime_error: " << lastOscError << "\n";
  }
  connection = c;
  return (c != 0);
}

bool OpenSoundControl::closeConnection()
{
  // if called from scheme signal error if no implementation
  if (!isAvailable())
  {
    lastOscError = "OSC is not available in this build of Grace.";
    return false;
  }
  if (!connection) return true;
  delete connection;
  connection = 0;
  return true;
}
