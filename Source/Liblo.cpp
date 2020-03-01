/*
  ==============================================================================

  Copyright 1999-2013 Rick Taube and David Psenicka.  All rights reserved.

  Licensed under the "Attribution-NonCommercial-ShareAlike" Vizsage
  Public License, which says that non-commercial users may share and
  modify this code but must give credit and share improvements. For
  complete terms please read the text of the full license available at
  this link: http://vizsage.com/license/Vizsage-License-BY-NC-SA.html

  ==============================================================================
*/

#include "OpenSoundControl.h"
#include "Console.h"
#include "Preferences.h"

/*=======================================================================*
  LibloConnection
  *=======================================================================*/

struct LibloConnection : public OpenSoundControl::Connection
{

  LibloConnection(OpenSoundControl* osc, int inPort, juce::String outHost, int outPort) 
    : OpenSoundControl::Connection(osc, inPort, outHost, outPort)
  {
  }

  ~LibloConnection() {}

  static const int oscerr_incomplete_message = 1;
  static const int oscerr_invalid_type = 2;
  static const int oscerr_invalid_data = 3;
  static const int oscerr_not_message = 4;
  
  bool sendMessage(juce::String path, s7_pointer message) ;
  bool sendBundle(double time, s7_pointer bundle) ;
  void handleMessage(const char *path, const char *types, int argc, void **data);
  lo_message makeLoMessage(s7_pointer pair, juce::String& errstr);
  int sendLoMessage(juce::String path, lo_message msg);
  int sendLoBundle(lo_bundle bndl);
  void toTimeTag(double ahead, lo_timetag& tag);
};

/*=======================================================================*
  Liblo Interface
  *=======================================================================*/

static lo_server_thread loServer = 0;
static lo_address loTarget = 0;

void loErrorHandler(int num, const char *msg, const char *path)
{
  juce::String text ("OSC server error ");
  text << num << " in path " << juce::String(path) << ": " 
       << juce::String(msg) << "\n";
  OpenSoundControl::getInstance()->lastOutputError = text;
}

void loInputHandler(const char *path, const char *types, lo_arg **argv, 
                    int argc, void *data, void *user_data)
{
  LibloConnection* c = (LibloConnection*)user_data;
  c->handleMessage(path, types, argc, (void **)argv);
}

bool OpenSoundControl::openConnection(int inPort, juce::String outHost, int outPort)
{
  lastOutputError = "";
  // if called from scheme signal error if no implementation
  if (!isAvailable())
  {
    lastOutputError = "OSC is not available in this build of Grace.";
    return false;
  }
  if (connection) return true;
  juce::String sourcePort = juce::String(inPort);
  juce::String targetPort = juce::String(outPort);

  LibloConnection* c = new LibloConnection(this, inPort, outHost, outPort);

  //  std::cout << "openConnection: sourcePort=" << sourcePort
  //            << ",  outHost=" << outHost << " targetPort=" << targetPort << "\n";
  loServer = lo_server_thread_new(sourcePort.toUTF8(),
                                  (lo_err_handler)loErrorHandler);
  //  std::cout << "  adding target host...\n";
  loTarget = lo_address_new(outHost.toUTF8(), targetPort.toUTF8());

  //  std::cout << "  adding server method...\n";
  lo_server_thread_add_method(loServer, 0, 0, (lo_method_handler)loInputHandler, c);
  //  std::cout << "  starting server thread...\n";
  int flag = lo_server_thread_start(loServer);

  if (flag < 0)
  {
    lo_server_free(loServer);
    loServer = 0;
    lo_address_free(loTarget);
    loTarget = 0;
    delete c;
    connection = 0;
    lastOutputError = "OSC server failed to start, returned status ";
    lastOutputError << juce::String(flag) << ".";
    return false ;
  }

  // WE ARE OPEN FOR BIZNIZ
  // FIXME THIS SHOULD BE AT OPENSOUNDCONTROL LEVEL
  // update preferences with current port
  Preferences::getInstance()->setIntProp("OscServerPort", inPort); 
  Preferences::getInstance()->setStringProp("OscTargetHost", outHost); 
  Preferences::getInstance()->setIntProp("OscTargetPort", outPort); 
  connection = c;
  return true;
}  

bool OpenSoundControl::closeConnection()
{
  lastOutputError = "";
  // if called from scheme signal error if no implementation
  if (!isAvailable())
  {
    lastOutputError = "OSC is not available in this build of Grace.";
    return false;
  }
  if (connection == 0) return true;
  bool force = false;
  int flag = lo_server_thread_stop(loServer);
  if (flag < 0 && !force)
  {
    lastOutputError = "OSC server thread failed to stop, returned status ";
    lastOutputError << juce::String(flag) << ".";
    return false;
  }
  lo_server_thread_del_method(loServer, 0, 0);
  lo_server_thread_free(loServer);
  loServer = 0;
  lo_address_free(loTarget);
  loTarget = 0;
  delete connection;
  connection = 0;
  return true;
}

/*=======================================================================*
  Sending Osc
  *=======================================================================*/

int LibloConnection::sendLoMessage(juce::String path, lo_message msg)
{
  int status = lo_send_message_from(loTarget,
                                    lo_server_thread_get_server(loServer),
                                    path.toUTF8(),
                                    msg);
  if (status < 0)
  {
    osc->lastOutputError = "lo_send_message_from returned status ";
    osc->lastOutputError << juce::String(status);
  }
  return status;
}

int LibloConnection::sendLoBundle(lo_bundle bndl)
{
  int status = lo_send_bundle_from(loTarget, lo_server_thread_get_server(loServer), bndl);
  if (status < 0)
  {
    osc->lastOutputError = "lo_send_bundle_from returned status ";
    osc->lastOutputError << juce::String(status);
  }
  return status;
}

/*=======================================================================*
  Receiving Osc
 *=======================================================================*/

void LibloConnection::handleMessage(const char *oscpath, const char *types, int argc, void **data)
{
  SchemeThread* st = SchemeThread::getInstance();
  juce::String path (oscpath);
  OscHook* hook = st->getOscHook(path);

  if (hook)
  {
    lo_arg **argv = (lo_arg **)data;
    XOscNode* node = new XOscNode(0.0, path, juce::String(types));
    for (int i = 0; i < argc; i++)
    {     
      char t = types[i];
      switch (t)
      {
      case LO_INT32:
        node->ints.add((s7_Int)argv[i]->i32);
        break;
      case LO_INT64:
        node->ints.add((s7_Int)argv[i]->i64);
        break;
      case LO_FLOAT:
        node->flos.add((double)argv[i]->f32);
        break;
      case LO_DOUBLE:
        node->flos.add(argv[i]->f64);
        break;
      case LO_TIMETAG:
        {
          double t = argv[i]->t.sec + (argv[i]->t.frac / 4294967295.0);
          node->flos.add(t);
        }
        break;
      case LO_STRING:
        node->strs.add(juce::String(&argv[i]->s));
        break;
      case LO_SYMBOL:
        node->strs.add(juce::String(&argv[i]->S));
        break;
      case LO_CHAR:
        node->ints.add(argv[i]->c);
        break;
      case LO_MIDI:
        {
          for (int m = 0; m < 4; m++)
            node->ints.add(argv[i]->m[m]);
        }
        break;
      case LO_BLOB:
        {
          int s = lo_blob_datasize((lo_blob)argv[i]);
          char* b = (char*)lo_blob_dataptr((lo_blob)argv[i]);
          node->ints.add(s); // add size then data
          for (int j = 0; j < s; j++)
            node->ints.add(b[j]);
        }
        break;
      case LO_TRUE:
      case LO_FALSE:
      case LO_NIL:
      case LO_INFINITUM:
        break;
      default:
        {
          juce::String msg="OSC (in): dropped unparsable message with path ";
          msg << path.quoted() << "\n";
          Console::getInstance()->printWarning(msg);
          delete node;
          return;
        }
      }
    }
    node->hook = hook;
    st->addNode(node);
  }
  //  if (traceInput)
  //    Console::getInstance()->printOutput("OSC (in): " + path +  " ...\n");
}

void LibloConnection::toTimeTag(double ahead, lo_timetag& tag)
{
  if (ahead == 0.0)
    tag = LO_TT_IMMEDIATE;
  else
  {
    #define JAN_1970 0x83aa7e80    
    juce::uint64 millis=(juce::uint64)(juce::Time::currentTimeMillis() + ((juce::int64)(ahead * 1000)));
    juce::uint64 seconds=millis/1000;
    juce::uint64 remain=millis-(seconds*1000); // remainder in milliseconds
    tag.sec = seconds + JAN_1970;
    tag.frac = remain * 4294967.295;
  }
}

lo_message LibloConnection::makeLoMessage(s7_pointer pair, juce::String& errstr)
{
  // return an Osc message or NULL on error. if the latter, errstr
  // will hold the error string to report
  lo_message msg = lo_message_new();
  s7_pointer p;
  int errn = 0;  // zero is success
  juce::String errs=juce::String();

  for (p = pair; s7_is_pair(p) && (errn == 0); p = s7_cdr(p))
  {
    char t = 0;               // message type
    s7_pointer x = s7_car(p); // message data
    if (s7_is_keyword(x))     // have explicit type tag
    {
      juce::String s (s7_symbol_name(x));
      if (s.length() == 2 && juce::String("ifsbhtdScmTFNI").containsChar(s[1]))
      {
        // a few OSC tags have no data
        switch (s[1])
        {
        case LO_TRUE:
          errn = lo_message_add_true(msg);
          continue;
        case LO_FALSE:
          errn = lo_message_add_false(msg);
          continue;
        case LO_NIL:
          errn = lo_message_add_nil(msg);
          continue;
        case LO_INFINITUM:
          errn = lo_message_add_infinitum(msg);
          continue;
        }
        // otherwise get the tagged data
        p = s7_cdr(p);
        if (s7_is_pair(p))
          x = s7_car(p);
        else
        {
          errn = oscerr_incomplete_message;
          break;
        }
        t = s[1];
      }
      else
      {
        errs = s;
        errn = oscerr_invalid_type;
        break;
      }
    }
    // at this point we have datum in 'x' and (possibly) a type in
    // 't'. now add t and x to message, possibly including a default
    // type if it was not explicity specified by the user
    if (s7_is_integer(x))
    {
      s7_Int i = s7_integer(x);
      switch (t)
      {
      case 0:
      case LO_INT32:
        errn = lo_message_add_int32(msg, i);
        break;
      case LO_INT64:
        errn = lo_message_add_int64(msg, (juce::int64)i);
        break;
      case LO_TIMETAG:
        {
          lo_timetag tag;
          toTimeTag((double)i, tag);
          errn = lo_message_add_timetag(msg, tag);
        }
        break;
      default:
        errs << t << " and " << juce::String(i);
        errn = oscerr_invalid_data;
        break;
      }
    }
    else if (s7_is_real(x))
    {
      s7_Double d = s7_real(x);
      switch (t)
      {
      case 0:
      case LO_FLOAT:
        errn = lo_message_add_float(msg, (float)d);
        break;
      case LO_DOUBLE:
        errn = lo_message_add_double(msg, (double)d);
        break;
      case LO_TIMETAG:
        {
          lo_timetag tag;
          toTimeTag(d,tag);
          errn = lo_message_add_timetag(msg, tag);
        }
        break;
      default:
        errs << t << " and " << d;
        errn = oscerr_invalid_data;
        break;
      }   
    }
    else if (s7_is_string(x))
    {
      const char* s=s7_string(x);
      if (*s == '\0') // empty string
      {
        errs = "\"\"";
        errn = oscerr_invalid_data;
        break;
      }
      switch (t)
      {
      case 0:
      case LO_STRING:
        errn = lo_message_add_string(msg, s);
        break;
      case LO_SYMBOL:
        errn = lo_message_add_symbol(msg, s);
        break;
      case LO_CHAR:
        errn = lo_message_add_char(msg, s[0]);
        break; 
      default:
        errs << t << " and " << juce::String(s);
        errn = oscerr_invalid_data;
        break;
      }
    }
    else if (s7_is_symbol(x))
    {
      const char* s = s7_symbol_name(x);
      switch (t)
      {
      case 0 :
      case 'S' :
        errn = lo_message_add_symbol(msg, s);
        break;
      case 's' :
        errn = lo_message_add_string(msg, s);
        break;
      default:
        errs << t << " and " << juce::String(s);
        errn = oscerr_invalid_data;
        break;
      }           
    }
    else if (s7_is_character(x))
    {
      char c = s7_character(x);
      switch (t)
      {
      case 0 :
      case LO_CHAR:
        errn = lo_message_add_char(msg, c);
        break;
      default:
        errs << t << " and " << juce::String(c);
        errn = oscerr_invalid_data;
        break;
      }
    }
    else if (s7_is_boolean(x))
    {
      if (t != 0) 
      {
        errs << t << " with #t or #f";
        errn = oscerr_invalid_data;
      }
      else if (x == s7_f(SchemeThread::getInstance()->scheme))
        errn = lo_message_add_false(msg);
      else
        errn = lo_message_add_true(msg);
    }          
    // list is midi or blob, eg :m (a b c d) or :b (...)
    // EMPTY LIST WILL FAIL
    else if (s7_is_pair(x)) 
    {
      s7_scheme *sc = SchemeThread::getInstance()->scheme;
      int siz = s7_list_length(sc,x);
      if (t == 'm')
      {
        if (siz != 4)
        {
          errs = "midi list not 4 byte";
          errn = oscerr_invalid_data;
          break;
        }
        int j = 0;
        s7_pointer m;
        juce::uint8 midi[4];
        for (m = x; s7_is_pair(m) && (errn == 0); m = s7_cdr(m), j++)
          if (s7_is_integer(s7_car(m)))
          {
            juce::uint32 y = (juce::uint32)s7_integer(s7_car(m));
            midi[j] = (juce::uint8)(y & 0xFF);
          }
          else 
            errn = oscerr_invalid_data;
        if (errn == 0)
          errn = lo_message_add_midi(msg, midi);
        else
          errs = "midi not four bytes";
      }
      else if (t == 'b')
      {
        if (siz < 1)
        {
          errs = "blob list not one or more bytes";
          errn = oscerr_invalid_data;
          break;
        }
        s7_pointer m;
        juce::uint8 data[siz];
        int j = 0;
        // get size and check data.
        for (m = x; s7_is_pair(m) && (errn == 0); m = s7_cdr(m), j++)
          if (s7_is_integer(s7_car(m)))
          {
            juce::uint32 y = (juce::uint32)s7_integer(s7_car(m));
            data[j] = (juce::uint8)(y & 0xFF);
          }
          else 
            errn = oscerr_invalid_data;
        if (errn == 0)
        {
          lo_blob blob = lo_blob_new(siz, data);
          errn = lo_message_add_blob(msg, blob);
        }
        else
          errs = "blob list not one or more bytes";
      }
      else
      {
        errs = "list without midi or blob tag";
        errn = oscerr_invalid_data;
      }
    }
    else
    {
      errs = "unparsable message data";
      errn = oscerr_invalid_data;
      break;
    }
  }

  switch (errn)
  {
  case 0:  // success!
    return msg;
  case oscerr_incomplete_message:
    osc->lastOutputError = "incomplete OSC message";
    break;
  case oscerr_invalid_type:
    osc->lastOutputError = "invalid OSC type: " + errs;
    break;
  case oscerr_invalid_data:
    osc->lastOutputError = "invalid osc data: " + errs;              
    break;
  default: // liblo error (< 1)
    osc->lastOutputError = "lo_message_add failed with " + juce::String(errn);
    break;
  }
  lo_message_free(msg);
  return 0;
}

bool LibloConnection::sendMessage(juce::String path, s7_pointer list)
{
  int errn = 0;
  juce::String errs;
  lo_message msg = makeLoMessage(list, errs);
  if (msg)
  {
    errn = sendLoMessage(path, msg);
    if (errn < 0)
      return false;
    else
      return true;
  }
  else
    return false;
}

bool LibloConnection::sendBundle(double time, s7_pointer list)
{
  lo_timetag ttag;
  lo_bundle bndl;
  int errn = 0;  // 0 == not error
  juce::String errs;

  toTimeTag(time, ttag);
  bndl = lo_bundle_new(ttag);
  // list is a list of messages each should be ("/path" . data)
  s7_pointer p;
  for (p = list; s7_is_pair(p) && (errn == 0); p = s7_cdr(p))
  {
    s7_pointer x = s7_car(p);
    if (s7_is_pair(x) && s7_is_string(s7_car(x)))
    {
      lo_message msg = makeLoMessage(s7_cdr(x), errs);
      if (msg)
      {
        char* str = (char*)s7_string(s7_car(x));
        errn = lo_bundle_add_message(bndl, str, msg);
        if (errn != 0)
        {
          errs = "lo_bundle_add_message failed with " + juce::String(errn);
          break;
        }
      }
      else
      {
        errs = "not an OSC message";
        errn = oscerr_not_message;
        break;
      }
    }
    else
    {
      errs = "not an OSC message";
      errn = oscerr_not_message;
      break;
    }
  }
  
  if (errn == 0)
  {
    errn = sendLoBundle(bndl);
    if (errn < 0)
      return false;
    else
      return true;
  }
  else
  {
    osc->lastOutputError = errs;
    lo_bundle_free_messages(bndl);
    return false;
  }
}


