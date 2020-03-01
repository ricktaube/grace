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

juce_ImplementSingleton(OpenSoundControl) ;

OpenSoundControl::OpenSoundControl()
  : inputPort (0),
    outputHost (""),
    outputPort (0),
    tracing (false)
{
  receiver.registerFormatErrorHandler (
    [this] (const char* data, int dataSize)
    {
      juce::String err ("Received ");
      err << juce::String(dataSize) << " OSC bytes with invalid or unsupported format.\n";
      Console::getInstance()->printError(err);
    });
}

OpenSoundControl::~OpenSoundControl()
{
}

bool OpenSoundControl::openInput(int port)
{
  juce::ScopedLock lock (oscLock);
  // return false if input port is already open
  if (inputPort > 0)
    return false; 
  // try to connect to input port
  if (receiver.connect(port))
  {
    receiver.addListener(this);
    inputPort = port;
  }
  return (inputPort > 0);
}

bool OpenSoundControl::openOutput(juce::String host, int port)
{
  juce::ScopedLock lock (oscLock);
  // return false if output port is already open
  if (outputPort > 0)
    return false;
  // default output host is the local host (127.0.0.1)
  if (host.isEmpty() || host.equalsIgnoreCase("localhost"))
    host = "127.0.0.1";
  if (sender.connect(host, port))
  {
    outputHost = host;
    outputPort = port;
  }
  return (outputPort > 0);
}

bool OpenSoundControl::closeInput()
{
  juce::ScopedLock lock (oscLock);
  // return false if input port is already closed
  if (inputPort == 0)
    return false;
  if (receiver.disconnect())
  {
    receiver.removeListener(this);
    inputPort = 0;
  }
  return (inputPort == 0);
}

bool OpenSoundControl::closeOutput()
{
  juce::ScopedLock lock (oscLock);
  // return false if output port is already closed
  if (outputPort == 0)
    return false;
  if (sender.disconnect())
  {
    outputHost = "";
    outputPort = 0;
  }
  return (outputPort == 0);
}

int OpenSoundControl::getInputPort()
{
  juce::ScopedLock lock (oscLock);
  return inputPort;
}

int OpenSoundControl::getOutputPort()
{
  juce::ScopedLock lock (oscLock);
  return outputPort;
}

const juce::String OpenSoundControl::getOutputHost()
{
  juce::ScopedLock lock (oscLock);
  return outputHost;
}

bool OpenSoundControl::isTracing()
{
  juce::ScopedLock lock (oscLock);
  return tracing;
}

void OpenSoundControl::setTracing(bool isActive)
{
  juce::ScopedLock lock (oscLock);
  tracing = isActive;
}

// Callback for OscReceiver::Listener
void OpenSoundControl::oscMessageReceived (const juce::OSCMessage& message)
{
  if (isTracing())
    traceInput(message);
  juce::String path = message.getAddressPattern().toString();
  if(OscHook* hook = SchemeThread::getInstance()->getOscHook(path))
  {
    XOscNode* data = new XOscNode(0.0, path, message, hook);
    SchemeThread::getInstance()->addNode(data);
  }
}

// Callback for OscReceiver::Listener
void OpenSoundControl::oscBundleReceived (const juce::OSCBundle& bundle)
{
//  std::cout << "oscBundleReceived\n";
}

// This should only be called by scheme thread.
bool OpenSoundControl::sendMessage(juce::String path, s7_pointer schemeMessage)
{
  juce::ScopedLock lock (oscLock);
  if (outputPort == 0)
    return false;
  if (juce::OSCMessage* message = parseMessage(schemeMessage, path))
  {
    bool success = sender.send(*message);
    delete message;
    return success;
  }
  else 
    return false;
}

// This should only be called by scheme thread.
bool OpenSoundControl::sendBundle(double time, s7_pointer schemeBundle)
{
  juce::ScopedLock lock (oscLock);
  if (outputPort == 0)
    return false;
  if (juce::OSCBundle* bundle = parseBundle(schemeBundle, time))
  {
    bool success = sender.send(*bundle);
    delete bundle;
    return success;
  }
  else
    return false;
}

juce::OSCMessage* OpenSoundControl::parseMessage(s7_pointer pair, juce::String oscPath)
{
  juce::OSCMessage* data = new juce::OSCMessage(oscPath);
  for (s7_pointer p = pair; s7_is_pair(p); p = s7_cdr(p))
  {
    char t = 0;               // message type
    s7_pointer x = s7_car(p); // message data
    if (s7_is_keyword(x))     // keyword == explicit type tag
    {
      juce::String s (s7_symbol_name(x));
      if (s.length() == 2 && juce::String("ifsbhtdScmTFNI").containsChar(s[1]))
      {
        // a few OSC tags have no data
        switch (s[1])
        {
        case OSC_TRUE:
	  data->addInt32(1);
	  continue;
        case OSC_FALSE:
        case OSC_NIL:
	  data->addInt32(0);
	  continue;
        case OSC_INFINITUM:
	  data->addInt32(2147483647);
          continue;
        }
        // advance p to start of message data
        p = s7_cdr(p);
        if (s7_is_pair(p))
          x = s7_car(p);
        else
        {
//          lastOscError << "incomplete OSC message after " << s;
          delete data;
          return nullptr;
        }
        t = s[1];
      }
      else
      {
//        lastOscError << "invalid OSC type " <<  s;
        delete data;
        return nullptr;
      }
    }
    // at this point we have data in 'x' and (possibly) an explicit
    // type in 't'. determine t if missing and add array data
    if (s7_is_integer(x))
    {
      // juce only has int32
      juce::int32 i = (juce::int32)s7_integer(x);
      if (t == 0)
	t = OSC_INT32;
      switch (t)
      {
      case OSC_INT32:
      case OSC_INT64:
      case OSC_TIMETAG:
        data->addInt32(i);
        break;
      default:
	//       lastOscError << "invalid data for OSC type " << t << ": " << juce::String(i);
        delete data;
        return 0;
      }
    }
    else if (s7_is_real(x))
    {
      // juce only has float
      float d = (float)s7_real(x);
      if (t == 0)
	t = OSC_FLOAT;
      switch (t)
      {
      case OSC_FLOAT:
      case OSC_DOUBLE:
      case OSC_TIMETAG:
        data->addFloat32(d);
        break;
      default:
//        lastOscError << "invalid data for OSC type " << t << ": " << d;
        delete data;
        return 0;
      }   
    }
    else if (s7_is_string(x))
    {
      juce::String s (s7_string(x));
      if (t == 0)
	t = OSC_STRING;
      if (s.isEmpty()) 
      {
//        lastOscError << "missing data for OSC type " << t;
        delete data;
        return 0;
      }
      switch (t)
      {
      case OSC_STRING:
      case OSC_SYMBOL:
      case OSC_CHAR:
        data->addString(s);
        break;
      default:
//       lastOscError << "invalid data for OSC type " << t << ": \"" << s << "\"";
        delete data;
        return 0;
      }
    }
    else if (s7_is_symbol(x))
    {
      juce::String s (s7_symbol_name(x));
      if (t == 0)
	t = OSC_SYMBOL;
      switch (t)
      {
      case OSC_SYMBOL:
      case OSC_STRING:
        data->addString(s);
        break;
      default:
	//       lastOscError << "invalid data for OSC type " << t << ": \"" << s << "\"";
        delete data;
        return 0;
      }           
    }
    else if (s7_is_character(x))
    {
      juce::String s;
      char c = s7_character(x);
      s << c;
      if (t == 0)
	t = OSC_CHAR;
      switch (t)
      {
      case OSC_CHAR:;
        data->addString(s);
        break;
      default:
//       lastOscError << "invalid data for OSC type " << t << ": '" << s << "'";
        delete data;
        return 0;
      }
    }
    else if (s7_is_boolean(x))
    {
      if (t != 0) 
      {
//        lastOscError << "explicit OSC type " << t << " used with #t or #f";
        delete data;
        return 0;
      }
      else if (x == s7_f(SchemeThread::getInstance()->scheme))
        data->addInt32(0);
      else
        data->addInt32(1);
    }          
    // list is either midi or blob
    else if (s7_is_pair(x)) 
    {
      if ((OSC_MIDI != t) && (OSC_BLOB != t))
      {
//        lastOscError = "invalid OSC data: list without midi or blob tag";
        delete data;
        return 0;
      }
      s7_scheme *sc = SchemeThread::getInstance()->scheme;
      auto siz = s7_list_length(sc, x);
      if (0 == siz)
      {
//        lastOscError = "invalid OSC data: empty list";        
        delete data;
        return 0;
      }
      if ((OSC_MIDI == t) && (siz != 4))
      {
//        lastOscError = "invalid OSC data: midi list is not 4 bytes";
        delete data;
        return 0;
      }
      for (s7_pointer m = x; s7_is_pair(m); m = s7_cdr(m))
      {
        if (s7_is_integer(s7_car(m)))
          data->addInt32( (juce::int32)(s7_integer(s7_car(m)) & 0xFF));
        else 
        {
//          lastOscError << "invalid OSC data: midi list contains non-integer";
          delete data;
          return 0;
        }
      }
    }
    else
    {
//      lastOscError = "unparseable OSC message";
      delete data;
      return 0;
    }
  }
  return data;
}

juce::OSCBundle*  OpenSoundControl::parseBundle(s7_pointer list, double ahead) {
  juce::OSCBundle* bundle = new juce::OSCBundle();
  if(ahead > 0.0) {
    juce::Time time = juce::Time::getCurrentTime() + juce::RelativeTime(ahead);
    bundle->setTimeTag(time);
  }
  for (s7_pointer p = list; s7_is_pair(p); p = s7_cdr(p)) {
    s7_pointer x = s7_car(p);
    // x has to be a valid message list {"path"...}
    if (s7_is_pair(x) && s7_is_string(s7_car(x)))  {
      juce::String path ((char*)s7_string(s7_car(x)));
      s7_pointer list = s7_cdr(x); // possibly empty
      std::unique_ptr<juce::OSCMessage> message (parseMessage(list, path));
      if (message) {
        //        std::cout << "adding data: " << d->toString() <<"\n";
        bundle->addElement(*message);
      }
      else {
        // osc message data was invalid and the error string has been
        // set by parseMessage()
        delete bundle;
        return nullptr;
      }
    }
    else {
      // invalid bundle format, set error string
      delete bundle;
//      lastOscError = "bundle value is not an OSC message";
      return nullptr;
    }
  }
  //  std::cout << "OpenSoundControl::parseBundle, bundle size=" << data.size() << "\n";
  return bundle;
}

void OpenSoundControl::testOutput() {
  if (!getOutputPort()) return;
  juce::String test = "OSC output test " ;
  float f = (juce::Random::getSystemRandom().nextFloat() * 100.0);
  if (sender.send ("/Grace", (float) f))
    std::cout << "OSC send test succeeded!\n";
  else
    std::cout << "OSC send test failed!\n";
}

void OpenSoundControl::traceInput (const juce::OSCMessage& message) {
  juce::String str ("[");
  str << message.getAddressPattern().toString();
  if (!message.isEmpty()) {
    for (const juce::OSCArgument* arg = message.begin(); arg != message.end(); ++arg) {
      str << " ";
      if (arg->isFloat32()) {
        str << juce::String(arg->getFloat32());
      }
      else if (arg->isInt32()) {
        str << juce::String(arg->getInt32());
      }
      else if (arg->isString()) {
        str << arg->getString();
      }
      else if (arg->isBlob()) {
        const juce::MemoryBlock& blob = arg->getBlob();
        str << juce::String::fromUTF8( (const char*)blob.getData(), (int)blob.getSize());
      }
      else {
        str << "(unknown)";
      }
    }
  }
  str << "]\n";
  Console::getInstance()->printOutput(str);
}

/* ==============================================================================
   OSC Connection Dialog
   =============================================================================*/

class OscConnectionDialog : public juce::Component, public juce::Button::Listener,
public juce::TextEditor::Listener {
  OpenSoundControl& osc;
  juce::GroupComponent serverGroup {"", "Input Connection"};
  juce::GroupComponent targetGroup {"", "Output Connection"};
  juce::TextButton inputButton;
  juce::TextButton outputButton;
  juce::Label serverPortLabel {"", "Port:"};
  juce::Label targetHostLabel {"", "Host:"};
  juce::Label targetPortLabel {"", "Port:"};
  juce::Label serverProtocolLabel {"", "Protocol:"};
  juce::TextEditor serverPortEditor;
  juce::TextEditor serverProtocolEditor;
  juce::TextEditor targetHostEditor;
  juce::TextEditor targetPortEditor;

public:
  OscConnectionDialog (OpenSoundControl& o) : osc(o) {
 
    //--- -------//
    //input group//
    //-- --------//
    addAndMakeVisible(serverGroup);
    // protocol label
    addAndMakeVisible(serverProtocolLabel);
    serverProtocolLabel.setFont (juce::Font (15.0f, juce::Font::plain));
    serverProtocolLabel.setJustificationType (juce::Justification::centredLeft);
    serverProtocolLabel.setEditable(false, false, false);
    serverProtocolLabel.setEnabled(false);
    // protocol editor (always disabled...)
    addAndMakeVisible(serverProtocolEditor);
    serverProtocolEditor.setMultiLine(false);
    serverProtocolEditor.setReturnKeyStartsNewLine(false);
    serverProtocolEditor.setReadOnly(true);
    serverProtocolEditor.setScrollbarsShown(true);
    serverProtocolEditor.setCaretVisible(true);
    serverProtocolEditor.setPopupMenuEnabled(false );
    serverProtocolEditor.setSelectAllWhenFocused(false );
    serverProtocolEditor.setColour(juce::TextEditor::textColourId, juce::Colours::grey);
    serverProtocolEditor.setText("UDP");
    serverProtocolEditor.setEnabled(false);
    serverProtocolEditor.setReadOnly(true);
    // server port label
    addAndMakeVisible(serverPortLabel);
    serverPortLabel.setFont(juce::Font (15.0000f, juce::Font::plain));
    serverPortLabel.setJustificationType(juce::Justification::centredLeft);
    serverPortLabel.setEditable(false, false, false);
    // server port editor
    addAndMakeVisible(serverPortEditor);
    serverPortEditor.setMultiLine(false);
    serverPortEditor.setReturnKeyStartsNewLine(false);
    serverPortEditor.setReadOnly(false);
    serverPortEditor.setScrollbarsShown(true);
    serverPortEditor.setCaretVisible(true);
    serverPortEditor.setPopupMenuEnabled(true);
    serverPortEditor.addListener(this);
    // input connection button
    addAndMakeVisible(inputButton);
    inputButton.addListener(this);
    
    //------------//
    //output group//
    //------------//
    
    addAndMakeVisible(targetGroup);
    // output host label
    addAndMakeVisible(targetHostLabel);
    targetHostLabel.setFont(juce::Font(15.0000f, juce::Font::plain));
    targetHostLabel.setJustificationType(juce::Justification::centredLeft);
    targetHostLabel.setEditable(false, false, false);
    // output host editor
    addAndMakeVisible(targetHostEditor);
    targetHostEditor.setMultiLine(false);
    targetHostEditor.setReturnKeyStartsNewLine(false);
    targetHostEditor.setReadOnly(false);
    targetHostEditor.setScrollbarsShown(true);
    targetHostEditor.setCaretVisible(true);
    targetHostEditor.setPopupMenuEnabled(true);
    targetHostEditor.setCaretPosition(0);
    targetHostEditor.addListener(this);
    // output port label
    addAndMakeVisible(targetPortLabel);
    targetPortLabel.setFont(juce::Font(15.0f, juce::Font::plain));
    targetPortLabel.setJustificationType(juce::Justification::centredLeft);
    targetPortLabel.setEditable(false, false, false);
    // output port label
    addAndMakeVisible(targetPortEditor);
    targetPortEditor.setMultiLine(false);
    targetPortEditor.setReturnKeyStartsNewLine (false);
    targetPortEditor.setReadOnly(false);
    targetPortEditor.setScrollbarsShown(true);
    targetPortEditor.setCaretVisible(true);
    targetPortEditor.setPopupMenuEnabled(true);
    targetPortEditor.addListener(this);
    // output connection button
    addAndMakeVisible(outputButton);
    outputButton.addListener(this);

    configureForConnections();
    setSize (392, 144);
  }

  ~OscConnectionDialog() {
  }

private:

  void configureForConnections() {
    // text colors refect connection state
    static const juce::Colour conColor =  juce::Colours::black;
    static const juce::Colour disColor =  juce::Colours::grey;
    int inPort = osc.getInputPort();
    int outPort = osc.getOutputPort();

    // Input Group

    serverPortLabel.setEnabled(inPort == 0);
    // server port editor can only edit the port if its not open
    serverPortEditor.setEnabled(inPort == 0);
    int p = (inPort > 0) ? inPort : 7779; //Preferences::getInstance()->getIntProp("OscServerPort", 7779);
    serverPortEditor.setColour(juce::TextEditor::textColourId, (inPort > 0) ? disColor : conColor);
    serverPortEditor.setText(juce::String(p));
    // if an input port is open the input button is set to "Close"
    inputButton.setButtonText((inPort > 0) ? "Close" : "Open");

    // Output Group

    targetPortLabel.setEnabled(outPort == 0);
    juce::String host = osc.getOutputHost() ; //Preferences::getInstance()->getStringProp("OscTargetHost", "localhost");
    if (host.isEmpty() || host == "127.0.0.1")
      host = "localhost";
    targetHostEditor.setColour(juce::TextEditor::textColourId,  (outPort > 0) ? disColor : conColor);
    targetHostEditor.setText(host);
    // can only edit the host if its not open
    targetHostEditor.setEnabled(outPort == 0);
    // target port label 
    targetPortLabel.setEnabled(outPort == 0);
    // use existing output port if open else use preferences
    p = (outPort > 0) ? outPort : Preferences::getInstance()->getIntProp("OscTargetPort", 57120);
    targetPortEditor.setColour(juce::TextEditor::textColourId, (outPort > 0) ? disColor : conColor);
    targetPortEditor.setText(juce::String(p));
    targetPortEditor.setEnabled(outPort == 0);
    // if an output port is the open output button is set to "Close"
    outputButton.setButtonText((outPort > 0) ? "Close" : "Open");
  }
  
  void resized() {
    int M = 8; // margin
    serverGroup.setBounds (M, M, 184, 96 + 20 + M);
    targetGroup.setBounds (199, M, 185, 96 + 20 + 9);
    
    serverProtocolLabel.setBounds (16, 32, 64, 24);
    serverProtocolEditor.setBounds (88, 32, 88, 24);
    serverPortLabel.setBounds (16, 64, 48, 24);
    serverPortEditor.setBounds (88, 64, 88, 24);
    
    targetHostLabel.setBounds (207, 32, 56, 24);
    targetHostEditor.setBounds (256, 32, 112, 24);
    targetPortLabel.setBounds (207, 64, 48, 24);
    targetPortEditor.setBounds (256, 64, 88, 24);
    
    //   inputButton->setBounds (152, 112, 96, 24);
    inputButton.setBounds ((serverGroup.getWidth() - 96) / 2 + serverGroup.getX(),
                           serverPortEditor.getBottom() + 8, 96, 24);
    outputButton.setBounds ((targetGroup.getWidth() - 96) / 2 + targetGroup.getX(),
                            targetPortEditor.getBottom() + 8, 96, 24);
  }

  void textEditorReturnKeyPressed(juce::TextEditor& editor) {
    inputButton.triggerClick();
  }
  
  void buttonClicked (juce::Button* b) {
    if (b == &inputButton) {
      if (inputButton.getButtonText() == "Close") {
        if (osc.closeInput()) {
          // SUCCESS
          configureForConnections();
        }
        else {
          juce::String err (">>> Error: Failed to close OSC input connection on port ");
          err << osc.getInputPort() << ".\n";
          Console::getInstance()->printError(err);
        }
      }
      else { // "Open"
        int port = serverPortEditor.getText().getIntValue();
        if (port > 0)	{
          if (osc.openInput(port)) {
            // SUCCESS add input port to preferences
            configureForConnections();
          }
          else {
            juce::String err (">>> Error: Failed to open OSC input connection on port ");
            err << port << ".\n";
            Console::getInstance()->printError(err);
          }
        }
        else {
          selectEditorText(&serverPortEditor);
          juce::String err (">>> Error: Invalid port number ");
          err << serverPortEditor.getText() << ".\n";
          Console::getInstance()->printError(err);
        }
      }
    }
    else if (b == &outputButton) {
      if (outputButton.getButtonText() == "Close")  {
        if (osc.closeOutput()) {
          // SUCCESS
          configureForConnections();
        }
        else {
          juce::String err (">>> Error: Failed to close OSC output connection on port ");
          err << osc.getOutputPort() << ".\n";
          Console::getInstance()->printError(err);
        }
      }
      else { // "Open"
        juce::String host = targetHostEditor.getText().trim();
        if (isValidHostString(host)) {
          int port = targetPortEditor.getText().getIntValue();
          if (port > 0) {
            if (osc.openOutput(host, port)) {
              // SUCCESS add output port to preferences
              configureForConnections();
            }
            else {
              juce::String err (">>> Error: Failed to open OSC outputconnection to host ");
              err << host << " and port " << port << ".\n";
              Console::getInstance()->printError(err);
            }
          }
          else {
            selectEditorText(&serverPortEditor);
            juce::String err (">>> Error: Invalid port number ");
            err << serverPortEditor.getText() << ".\n";
            Console::getInstance()->printError(err);
          }
        }
        else {
          selectEditorText(&targetHostEditor);
          juce::String err (">>> Error: Invalid output host ");
          err << host << ". The host name should be 'localhost' or a valid IP address.\n";
          Console::getInstance()->printError(err);
        }
      }
    }
  }

  void selectEditorText(juce::TextEditor* editor) {
    juce::Range<int> reg (0, editor->getTotalNumChars());
    editor->grabKeyboardFocus();
    editor->setHighlightedRegion(reg);
  }
  
  bool isValidHostString(juce::String text) {
    if (text.isEmpty())
      return false;
    else if (text.equalsIgnoreCase("localhost"))
      return true;
    else if (text.containsOnly(".0123456789"))
      return (text.matchesWildcard("*.*.*.*", true));
    return false;
  }
};

void OpenSoundControl::openConnectionDialog() {
  OscConnectionDialog* comp = new OscConnectionDialog(*this);
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "OSC Connections";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.setOwned(comp);
  dw.launchAsync();
}
