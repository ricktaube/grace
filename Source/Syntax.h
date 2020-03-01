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
#include "Enumerations.h"


class Syntax : public juce::CodeTokeniser
{
public:

  // SynToks hold info about distinguished tokens in each syntax

  class SynTok 
  {
  public:
    SynTok(juce::String n, int t, int d1=0, int d2=0, int d3=0) 
      : name(n), type(t), data1(d1), data2(d2), data3(d3) 
    {
    }
    ~SynTok() {}
    juce::String getName() {return name;}
    void setName(juce::String n) {name=n;}
    int getType() {return type;}
    void setType(int t) {type=t;}
    int getTypeData() {return (type & 0xff);}
    int getData1() {return data1;}
    int getData2() {return data2;}
    int getData3() {return data3;}
    int getStart() {return data1;}
    int getIndent() {return data2;}
    int getLiteralClass() {return data3;}
    const juce::String toString()
    {
      juce::String str="#<";
      str<<getName() << " 0x" << juce::String::toHexString(getType()) << ">";
      return str;
    }
  private:
    juce::String name;       // string to match
    int type;          // token classification table (reverse mapping?)
    int data1;         // data fields (use is syntax specific)
    int data2;    
    int data3;
  };

  typedef std::map <juce::String, SynTok*> SynTokMap;

  Syntax() ;
  ~Syntax();
  int getTextType() {return type;}
  void addSynTok (const juce::String n, int t, int a=0, int b=0, int c=0) ;
  SynTok* getSynTok (juce::String n) ;

  void setCharSyntax(const juce::String chrs, const int syn);
  bool isWhiteChar (const juce::juce_wchar c) ;
  bool isWordChar (const juce::juce_wchar c) ;
  bool isSymbolChar (const juce::juce_wchar c) ;
  bool isTokenChar (const juce::juce_wchar c) ;
  bool isCommentChar (const juce::juce_wchar c) ;
  bool isPrefixChar (const juce::juce_wchar c) ;
  bool isStringChar (const juce::juce_wchar c) ;
  bool isOpenChar (const juce::juce_wchar c) ;
  bool isCloseChar (const juce::juce_wchar c) ;
  bool isPunctuationChar (const juce::juce_wchar c) ;
  bool isEscapeChar (const juce::juce_wchar c) ;
  int  isNumberToken(const juce::String str) ;

  bool scanToken(juce::CodeDocument::Position& pos, const int dir, const juce::CodeDocument::Position end);
  bool scanCharacter(juce::juce_wchar chr, juce::CodeDocument::Position& pos, const int dir, const juce::CodeDocument::Position end);
  bool scanPrefix(juce::CodeDocument::Position& pos, const int dir, const juce::CodeDocument::Position end);
  int  scanCode(juce::CodeDocument& document, juce::CodeDocument::Position& pos, bool forward, int mode, int limit=-1);
  int  lastIndented(juce::CodeDocument& document, juce::Array<int>& starts, bool index);
  bool lookingAt(const juce::CodeDocument::Position pos, const juce::String text, const bool forward, const bool delimited);
  bool isCommaTerminatedLine(juce::CodeDocument& document, int line);
  
  virtual int getIndentation(juce::CodeDocument& document, int line) =0;
  virtual int backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& start, juce::CodeDocument::Position& end) =0;
  virtual void eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region) =0;

protected:
  char syntab [128];
  juce::Colour colors[ColorThemeIDs::MAXTOKENCOLORS];
  SynTokMap tokens;
  int type;
  int numtoks;
  int maxtoklen;
};

/*=======================================================================*
  Text Syntax
  *=======================================================================*/

class TextSyntax : public Syntax
{
public:
  TextSyntax();
  ~TextSyntax() ;

  static const int TokenError = 0;
  static const int TokenPlaintext = 1;

  juce::CodeEditorComponent::ColourScheme getDefaultColourScheme ();
  juce::Colour getDefaultColour ( int tokenType);
  int readNextToken (juce::CodeDocument::Iterator &source);

  int getIndentation(juce::CodeDocument& document, int line);
  int backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& start, juce::CodeDocument::Position& end);
  void eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region);

  juce_DeclareSingleton (TextSyntax, true)
};

/*=======================================================================*
  Lisp Syntax
  *=======================================================================*/

class LispSyntax : public Syntax
{
public:
  LispSyntax();
  ~LispSyntax() ;
  // juce syntax tokens start at 0 or the defaults dont work
  static const int TokenError = 0;
  static const int TokenPlaintext = 1;
  static const int TokenComment = 2;
  static const int TokenString = 3;
  static const int TokenSharpsign = 4;
  static const int TokenLispKeyword = 5;
  static const int TokenLispSpecialForm = 6;

  juce::CodeEditorComponent::ColourScheme getDefaultColourScheme ();
  juce::Colour getDefaultColour ( int tokenType);
  int readNextToken (juce::CodeDocument::Iterator &source);

  int getIndentation(juce::CodeDocument& document, int line) ;
  int backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& start, juce::CodeDocument::Position& end); 
  void eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region);
  
  juce_DeclareSingleton (LispSyntax, true)
};

/*=======================================================================*
  SAL Syntax
  *=======================================================================*/

class SalSyntax : public Syntax
{
private:
  juce::String errormessage;
public:

  SalSyntax(bool init=true);
  ~SalSyntax();
  void init();
  juce::CodeEditorComponent::ColourScheme getDefaultColourScheme ();
  juce::Colour getDefaultColour ( int tokenType);
  int readNextToken (juce::CodeDocument::Iterator &source);

  int getIndentation(juce::CodeDocument& document, int line) ;
  int backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& start, juce::CodeDocument::Position& end); 
  void eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region);

  // SAL/SAL2 lexer
  bool tokenize(juce::String str, juce::OwnedArray<SynTok>& toks);
  bool tokenize(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end,
                juce::OwnedArray<SynTok>& tokens);
  const juce::String& getLastParseError();
  juce_DeclareSingleton (SalSyntax, true)
};

/*=======================================================================*
  SAL2 Syntax
  *=======================================================================*/

class Sal2Syntax : public SalSyntax
{
private:

public:
  Sal2Syntax();
  ~Sal2Syntax();

  juce::CodeEditorComponent::ColourScheme getDefaultColourScheme ();
  juce::Colour getDefaultColour ( int tokenType);
  int readNextToken (juce::CodeDocument::Iterator &source);

  //  juce::String tokenize(juce::String str) {return juce::String();}
  //  bool tokenize(juce::String str, juce::OwnedArray<SynTok>& toks) {return false;}

  int backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& start, juce::CodeDocument::Position& end); 

  juce_DeclareSingleton (Sal2Syntax, true)
};
