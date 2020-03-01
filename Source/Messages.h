/*=======================================================================*
  Copyright (C) 2012 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#ifndef CM_MESSAGES_H
#define CM_MESSAGES_H

#include "Libraries.h"

class AsyncMessage
{
public:
  int type;
  juce::String text;
  int data;
  AsyncMessage(int typ)
  {
    type=typ;
    text=juce::String();
    data=0;
  }
  AsyncMessage(int typ, int dat)
  {
    type=typ;
    data=dat;
    text=juce::String();
  }
  AsyncMessage(int typ, juce::String txt)
  {
    type=typ;
    text=txt;
    data=0;
  }
  AsyncMessage(int typ, int dat, juce::String txt)
  {
    type=typ;
    text=txt;
    data=dat;
  }
  ~AsyncMessage() {}
};

#endif
