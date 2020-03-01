/*=======================================================================*
  Copyright (C) 2012 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#include "Syntax.h"
#include "Console.h"
#include "Scheme.h"
#include "Preferences.h"

juce_ImplementSingleton(TextSyntax) ;
juce_ImplementSingleton(LispSyntax) ;
juce_ImplementSingleton(SalSyntax) ;
juce_ImplementSingleton(Sal2Syntax) ;

Syntax::Syntax() 
  : type (TextIDs::Empty),
    numtoks (0),
    maxtoklen (0)
{
  // initialize table to WHITE
  for (int i=0; i<128; i++) syntab[i]=ScanIDs::SYN_WHITE;
  // alpha and digits default to WORD
  for (int i='0'; i<='9'; i++) syntab[i]=ScanIDs::SYN_WORD;
  for (int i='A'; i<='Z'; i++) syntab[i]=ScanIDs::SYN_WORD;
  for (int i='a'; i<='z'; i++) syntab[i]=ScanIDs::SYN_WORD;
  // initialize colors to black
  for (int i=0; i<ColorThemeIDs::MAXTOKENCOLORS; i++)
    colors[i]=juce::Colours::black;
}

Syntax::~Syntax()
{
}

void Syntax::addSynTok (const juce::String n, int t, int a, int b, int c) 
{
  numtoks++;
  int l=n.length();
  if (l>maxtoklen) maxtoklen=l;
  tokens[n] = new SynTok(n, t, a, b, c);
}

Syntax::SynTok* Syntax::getSynTok (juce::String n) 
{
  SynTokMap::iterator iter = tokens.find(n);
  if ( iter == tokens.end() )
    return NULL;
  return iter->second;
}

//
///  Char Syntax Methods
//

void Syntax::setCharSyntax (const juce::String chrs, const int syn)
{
  for (int i=0; i<chrs.length(); i++)
    if (chrs[i]<128)
      syntab[chrs[i]] = syn;
    else
      std::cout << "setCharSyntax: WARNING: index [" << i << "] in \"" << chrs << "\" is > 127!\n";
}

bool Syntax::isWhiteChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_WHITE);
}

bool Syntax::isWordChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_WORD);
}

bool Syntax::isSymbolChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_SYMBOL);
}

bool Syntax::isTokenChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  int s=syntab[c];
  return (s == ScanIDs::SYN_WORD || s == ScanIDs::SYN_SYMBOL || s == ScanIDs::SYN_ESCAPE);
}

bool Syntax::isCommentChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_COMMENT);
}

bool Syntax::isPrefixChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_PREFIX);
}

bool Syntax::isStringChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_STRING);
}

bool Syntax::isOpenChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_OPEN);
}

bool Syntax::isCloseChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_CLOSE);
}

bool Syntax::isPunctuationChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_PUNCT);
}

bool Syntax::isEscapeChar (const juce::juce_wchar c)
{
  if(c > 127) return false;
  return (syntab[c] == ScanIDs::SYN_ESCAPE);
}

/*=======================================================================*
  Syntactic Text Scanning
  *=======================================================================*/

bool Syntax::lookingAt(const juce::CodeDocument::Position pos, const juce::String text, const bool forward, const bool delimited)
{
  // return true if document matches text starting at pos, which must
  // be ON the first char to check. if delimited is true then the text
  // must match as a delimited word.
  juce::CodeDocument::Position at (pos);
  int len=text.length();
  if (forward)
  {
    juce::CodeDocument::Position end(pos);
    end.setPosition(INT_MAX);
    int i=0;
    for ( ; i<len && at!=end; i++)
    {
      //String str="comparing ";
      //str << at.getCharacter() << "&" << text[i] << "\n";
      //std::cout << str.toUTF8();
      if (at.getCharacter() != text[i])
        return false;
      else
        at.moveBy(1);
    }
    //std::cout << "done, i==len: " << (i==len) << " at==end: "  << (at==end) << "\n";
    if (i==len)
      return (delimited) ? ((at==end) || !isTokenChar(at.getCharacter())) : true;
    return false;
  }
  else
  {
    juce::CodeDocument::Position end (pos);
    end.setPosition(0);
    bool b=false;
    int i=len-1;
    for ( ; i>=0; i--)
      if (at.getCharacter() != text[i])
        return false;
    // at this point we've matched at position i
      else if (at==end)
      {
        i--;    // still increment i
        b=true; // note we matched at bob
        break;
      }
      else at.moveBy(-1);

    if (i<0 )
      return (delimited) ? (b || !isTokenChar(at.getCharacter())) : true;
    return false;
  }
}

bool Syntax::isCommaTerminatedLine(juce::CodeDocument& document, int line)
{
  // quick and dirty test for line ending with comma. doesnt check for
  // () or "" nesting etc
  juce::CodeDocument::Position b ((const juce::CodeDocument&)document, line, 0);
  juce::CodeDocument::Position e ((const juce::CodeDocument&)document, line, INT_MAX);
  bool x=false;
  while (b != e)
  {
    const juce::juce_wchar c=b.getCharacter();
    if (c==',') x=true;
    else if (isCommentChar(c)) break;
    else if (isWhiteChar(c)) ;
    else x=false;
    b.moveBy(1);
  }
  return x;
}  

int Syntax::isNumberToken(const juce::String str) 
{
  // returns 0==nan, 1==int, 2==float, 3==ratio
  int len=str.length();
  int dot=0, div=0, dig=0;
  for (int pos=0; pos<len; pos++) 
    switch ( str[pos] )
    {
    case '-' :
    case '+' :
      if (pos>0) return 0; // not in leading position
      break;
    case '.' :
      if (dot>0) return 0; // too many dots
      if (div>0) return 0; // in ratio
      dot++;
      break;
    case '/' :
      if (div>0) return 0; // too many divs
      if (dig==0) return 0; // no digit yet
      if (dot>0) return 0;  // float already
      if (pos==len-1) return 0; // at end
      div++;
      break;
    case '0' :
    case '1' :
    case '2' :
    case '3' :
    case '4' :
    case '5' :
    case '6' :
    case '7' :
    case '8' :
    case '9' :
      dig++;
      break;
    default:
      return 0;
    }
  if (div) return 3;
  if (dot) return 2; 
  return 1;
}

int Syntax::lastIndented(juce::CodeDocument& document, juce::Array<int>& starts, bool index)
{
  // search array of ordered expression start positions to return the
  // indentation column (OR array index) of the LATEST postion that is
  // the first expression in its line. otherwise return the position
  // of the very first expr if there isn't a latest one
  int size=starts.size();
  if (size==0) return -1; // -1 for error either way
  else if (size==1) // if only one expr use it
  {
    juce::CodeDocument::Position a ((const juce::CodeDocument&)document, starts[0]);
    return (index) ? 0 : a.getIndexInLine();
  }
  else
    // compare line numbers of each start to find latest expr that
    // starts a line
  {
    int i=0;
    for (i=starts.size()-1; i>0; i--)
    {
      juce::CodeDocument::Position a ((const juce::CodeDocument&)document, starts[i-1]);
      juce::CodeDocument::Position b ((const juce::CodeDocument&)document, starts[i]);
      // if the i-1 expr is on an earlier line then THIS expr
      // starts a line
      if (a.getLineNumber() < b.getLineNumber())
        break;
    }
    juce::CodeDocument::Position a ((const juce::CodeDocument&)document, starts[i]);
    return (index) ? i : a.getIndexInLine();
  }
}

bool Syntax::scanToken(juce::CodeDocument::Position& pos, const int dir, const juce::CodeDocument::Position end)
{
  // scan while position is on a token
  bool token=false;
  bool atend=false;
  while (!atend && isTokenChar(pos.getCharacter()))
  {
    token=true;
    pos.moveBy(dir);
    if (pos==end) atend=true;
  }
  // pos is now on delim or at End. If not at end then check to see if
  // the delim is escaped. if it is then continue searching
  if (atend)
    return token;
  if (isEscapeChar(pos.movedBy(-1).getCharacter()))
  {
    pos.moveBy(dir);
    return scanToken(pos,dir, end);
  }
  else
    return token;
}

bool Syntax::scanCharacter(juce::juce_wchar chr, juce::CodeDocument::Position& pos, const int dir, 
                           const juce::CodeDocument::Position end)
{
  // scan for character, if found leave pos on the char's position and
  // return true otherwise return false
  bool found=false;
  bool atend=false;
  juce::CodeDocument::Position check (pos);
  while (!atend)
  {
    if (check.getCharacter()==chr)
    {
      if (check.movedBy(-1).getCharacter()=='\\')
        check.moveBy(dir);
      else
      {
        found=true;
        atend=true;
      }
    }
    else if (check==end)
      atend=true;
    else
      check.moveBy(dir);
  }
  if (found)
    pos.setLineAndIndex(check.getLineNumber(),check.getIndexInLine());
  return found;
}

bool Syntax::scanPrefix(juce::CodeDocument::Position& pos, const int dir, 
                        const juce::CodeDocument::Position end)
{
  bool prefix=false;
  while (isPrefixChar(pos.getCharacter()))
  {
    prefix=true;
    if (pos==end) break;
    pos.moveBy(dir);
  }
  return prefix;
}

int Syntax::scanCode(juce::CodeDocument& document, juce::CodeDocument::Position& pos,
                     bool forward, int mode, int limit)
{
#define ISLEVEL(a,b,c,d) (((a)==0)&&((b)==0)&&((c)==0)&&((d)==0))
#define CURLY_CHAR_P(c) (( (c) == '{') || ( (c) == '}'))
#define SQUARE_CHAR_P(c) (( (c) == '[') || ( (c) == ']'))
#define PAREN_CHAR_P(c) (( (c) == '(') || ( (c) == ')'))

  int typ = ScanIDs::SCAN_EMPTY;
  int dir=(forward) ? 1 : -1;
  int par=0, sqr=0, cur=0, ang=0;
  bool atfirst=false;
  juce::CodeDocument::Position end ((const juce::CodeDocument&)document,0);  // Initialize to start of buffer
  if (limit==-1)
  {
    if (forward)
      end.setPosition(INT_MAX);
  }
  else
    end.setPosition(limit);

  while (true)
  {
    if (pos==end)
    {
      if (forward) // we've moved past the last char
        break;
      else if (!atfirst) // we're checking the first char
        atfirst=true;
      else // we've already checked the first char
        break;
    }
    juce::juce_wchar chr=pos.getCharacter();
    // WHITE SPACE. advance one position
    if (isWhiteChar(chr))
    {  
      pos.moveBy(dir);
    }
    // COMMENT CHAR. advance forward eol or backward one char
    else if (isCommentChar(chr))
    {
      if (forward)
        //scanEOL(pos);
        pos.setLineAndIndex(pos.getLineNumber(),INT_MAX);

      else
        pos.moveBy(dir);
    }
    // MAYBE MOVING BACKWARD IN A COMMENT LINE
    else if ((!forward) &&
             scanCharacter(';', // FIXME!
                           pos, dir, 
                           juce::CodeDocument::Position((const juce::CodeDocument&)document, 
                                                        pos.getLineNumber(),
                                                        0)))
    {
      pos.moveBy(dir); // move one beyond the comment character
    }
    // SKIP WHITE/COMMENTS. char now not white or part of comment
    else if (mode==ScanIDs::MoveWhiteAndComments)
    {
      break;
    }
    // TOKEN CHAR. scan out token
    else if (isTokenChar(chr))
    {
      scanToken(pos,dir,end);
      // pos is now either one beyond token or at BOB and on
      // the token start. quit scanning if at top level
      if (ISLEVEL(par,cur,sqr,ang) )
      {
        if (!forward) scanPrefix(pos,dir,end); // include any prefix chars
        typ = ScanIDs::SCAN_TOKEN;
        break;
      }
    }
    // STRING START.  scan for end of string char
    else if (isStringChar(chr))
    {
      juce::CodeDocument::Position check=pos.movedBy(dir);
      if (check==end)
      {
        typ=ScanIDs::SCAN_MISMATCH;
        break;
      }
      pos.moveBy(dir); // advance position one beyond first "
      if (scanCharacter('\"', pos, dir, end))
      {
        // now on end of string advance pos one past that char
        // 
        if (/*!forward &&*/ (pos!=end))  pos.moveBy(dir);
        if (ISLEVEL(par,cur,sqr,ang)) 
        {
          typ = ScanIDs::SCAN_STRING;
          break;
        }
      }
      else
      {
        // missing end of string char, return error
        typ=ScanIDs::SCAN_UNMATCHED;
        break;
      }
    }
    // PUNCTUATION. keep going unless toplevel (was token mode)
    else if (isPunctuationChar(chr))
    {
      pos.moveBy(dir);
      if (ISLEVEL(par,cur,sqr,ang)) //(mode==ScanIDs::MoveTokens) 
      {
        typ=ScanIDs::SCAN_PUNCT;
        break;
      }
    }
    // PREFIX. keep going
    else if (isPrefixChar(chr) ) 
    {
      pos.moveBy(dir);
    }
    // OPEN PARENS TOKEN
    else if ((mode==ScanIDs::MoveTokens) && isOpenChar(chr) )
    {
      pos.moveBy(dir);
      typ=ScanIDs::SCAN_OPEN;
      break;
    }
    // CLOSE PARENS TOKEN
    else if ((mode==ScanIDs::MoveTokens) && isCloseChar(chr) )
    {
      pos.moveBy(dir);
      typ=ScanIDs::SCAN_CLOSE;
      break;
    }
    // OPEN PARENS EXPRESSION. increment level of paren.
    else if ((forward && isOpenChar(chr) ) ||
             (!forward && isCloseChar(chr)))
    {
      if ( PAREN_CHAR_P(chr) ) par++;
      else if ( CURLY_CHAR_P(chr) ) cur++;
      else if ( SQUARE_CHAR_P(chr) ) sqr++;
      pos.moveBy(dir);
    }
    // CLOSE PARENS EXPRESSION. decrement level, stop if level zero
    else if ((forward && isCloseChar(chr)) ||
             (!forward && isOpenChar(chr))) 
    {
      if (PAREN_CHAR_P(chr)) 
      {
        if (par==0) typ = ScanIDs::SCAN_UNLEVEL; else par--;
      }
      else if (CURLY_CHAR_P(chr)) 
      {
        if (cur==0) typ = ScanIDs::SCAN_UNLEVEL; else cur--;
      }
      else if (SQUARE_CHAR_P(chr)) 
      {
        if (sqr==0) typ = ScanIDs::SCAN_UNLEVEL; else sqr--;
      }
      if (typ==ScanIDs::SCAN_UNLEVEL)
      {
        break;
      }
      else if (ISLEVEL(par,cur,sqr,ang))
      {
        pos.moveBy(dir);
        if (!forward) scanPrefix(pos,dir,end); // include any prefix chars
        typ = ScanIDs::SCAN_LIST;
        break;
      }
      else
      {
        pos.moveBy(dir);
      }
    }
    else
    {
      typ=ScanIDs::SCAN_MISMATCH;
      break;
    } 
  } // end while

  //std::cout << "pos="<<pos.getPosition()<< ",typ="<<typ<<",isfirst="<<atfirst<<"\n";
  //std::cout << "after scan, par=" << par << "\n";
  // move pos back to the first char if backward scan

  if (typ<0) 
    ;
  else if (ISLEVEL(par,cur,sqr,ang))
  {
    // if we are moving backwards then our position is now either
    // one BEFORE the start of the expression OR we are on the first
    // character and that character is part of the expression
    if //(!forward && !atfirst && typ>0) //pos!=end
      (!forward &&  typ>0 &&
       ((pos==end && isWhiteChar(pos.getCharacter())) ||
        (pos!=end )    )      
       )
    {
      //std::cout << "incrementing pos\n";
      pos.moveBy(1);
    }
  }
  else
    typ=ScanIDs::SCAN_UNMATCHED;
  return typ;
}

/*=======================================================================*
  Text Syntax
  *=======================================================================*/

TextSyntax::TextSyntax () 
  : Syntax()
{
  type=TextIDs::Text;

  setCharSyntax("~@#$%^&*-_=+|<>/", ScanIDs::SYN_SYMBOL);
  setCharSyntax("\"", ScanIDs::SYN_STRING);
  setCharSyntax("([{", ScanIDs::SYN_OPEN);
  setCharSyntax(")]}", ScanIDs::SYN_CLOSE);
  setCharSyntax(",.!?;:'`\\", ScanIDs::SYN_PUNCT);
}

TextSyntax::~TextSyntax()
{
  clearSingletonInstance();
}

juce::CodeEditorComponent::ColourScheme TextSyntax::getDefaultColourScheme ()
{ 
  // juce's CodeEditorComponent uses this array to name the
  // Tokenizer's tokenTypes, where each array index is a tokenType and
  // the string at that postion is its name. we use this array to map
  // tokenTypes to ColorTheme attribute names in the Xml ColorTheme.
  // for attributes see ColorThemeIDs class in Enumerations.h

  juce::CodeEditorComponent::ColourScheme cs;
  cs.set("error", juce::Colours::black);
  cs.set("plaintext", juce::Colours::black);
  return cs;
}

juce::Colour TextSyntax::getDefaultColour (int tokenType)
{
  return juce::Colours::black;
}

int TextSyntax::readNextToken (juce::CodeDocument::Iterator &source)
{
  source.skipToEndOfLine(); 
  return TokenPlaintext;
}

int TextSyntax::getIndentation(juce::CodeDocument& document, int line)
{
  // -1 means no special syntactic indentation (normal tabbing)
  return -1;
}

int TextSyntax::backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& start, juce::CodeDocument::Position& end)
{
  // 0 means no backward expression parsing
  return 0;
}
void TextSyntax::eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region)
{
  // no expression evaluation or macro expansion
}

/*=======================================================================*
  Lisp Syntax
  *=======================================================================*/

LispSyntax::LispSyntax () 
  : Syntax()
{
  type=TextIDs::Lisp;

  setCharSyntax("~!@$%^&*-_=+[{]}|:<.>/?", ScanIDs::SYN_SYMBOL);
  setCharSyntax(";", ScanIDs::SYN_COMMENT);
  setCharSyntax("`#',", ScanIDs::SYN_PREFIX);
  setCharSyntax("\"", ScanIDs::SYN_STRING);
  setCharSyntax("(", ScanIDs::SYN_OPEN);
  setCharSyntax(")", ScanIDs::SYN_CLOSE);
  setCharSyntax(",", ScanIDs::SYN_PUNCT);
  setCharSyntax("\\", ScanIDs::SYN_ESCAPE);

  //  addSynTok("and", numtoks, 0, 2);
  addSynTok("begin", numtoks, 1, 1000);
  addSynTok("cond", numtoks, 0, 2);
  addSynTok("define", numtoks, 1, 1);
  addSynTok("define*", numtoks, 1, 1);
  addSynTok("define-process", numtoks, 1, 1);
  addSynTok("definstrument", numtoks, 1, 1);
  addSynTok("do", numtoks, 1, 2);
  addSynTok("if", numtoks, 0, 1);
  addSynTok("lambda", numtoks, 1, 1);
  addSynTok("let", numtoks, 1, 1);
  addSynTok("let*", numtoks, 1, 1);
  addSynTok("letrec", numtoks, 1, 1);
  addSynTok("loop", numtoks, 0, 1000);
  //addSynTok("or", numtoks, 0, 2);
  addSynTok("process", numtoks, 0, 1000);
  addSynTok("run", numtoks, 0, 1000);
  addSynTok("send", numtoks, 0, 1);
  addSynTok("unless", numtoks, 1, 1);
  addSynTok("when", numtoks, 1, 1);
  addSynTok("with-csound", numtoks, 1, 1);
  addSynTok("with-fomus", numtoks, 1, 1);
  addSynTok("with-midi", numtoks, 1, 1);
  addSynTok("with-sound", numtoks, 1, 1);
}

LispSyntax::~LispSyntax()
{
  clearSingletonInstance();
}

juce::CodeEditorComponent::ColourScheme LispSyntax::getDefaultColourScheme ()
{
  // juce's CodeEditorComponent uses this array to name the
  // Tokenizer's tokenTypes, where each array index is a tokenType and
  // the string at that postion is its name. we use this array to map
  // tokenTypes to ColorTheme attribute names in the Xml ColorTheme.
  // for attributes see ColorThemeIDs class in Enumerations.h

  juce::CodeEditorComponent::ColourScheme cs;

  //-------------------------------------------
  cs.set("error",     juce::Colours::red);             // TokenError
  cs.set("plaintext", juce::Colours::black);           // TokenPlaintext
  cs.set("comment",   juce::Colour(0xce, 0x00, 0x1a)); // TokenComment
  cs.set("string",    juce::Colours::rosybrown);       // TokenString
  cs.set("keyword1",  juce::Colours::cadetblue);       // TokenSharpsign
  cs.set("keyword2",  juce::Colour(0x8a, 0x2f, 0x8f)); // TokenLispKeyword
  cs.set("literal1",  juce::Colour(0x95, 0x00, 0x83)); // TokenLispSpecialForm
  return cs;
}

juce::Colour LispSyntax::getDefaultColour (int tokenType)
{
  ////  return Colours::black;
  switch (tokenType)
  {
  case TokenError: return juce::Colours::red;
  case TokenComment: return juce::Colour(0xce, 0x00, 0x1a); //juce::Colours::firebrick;
  case TokenString: return juce::Colour(0xa1, 0x11, 0x53); //juce::Colours::rosybrown;
  case TokenPlaintext: return juce::Colours::black;
  case TokenSharpsign: return juce::Colours::cadetblue; // Sharp Sign
  case TokenLispKeyword: return juce::Colour(0x8a, 0x2f, 0x8f); //juce::Colours::orchid;Lisp Keywordjuce::Colour(0x8a, 0x2f, 0x8f)
  case TokenLispSpecialForm: return juce::Colour(0x95, 0x00, 0x83);  // Special Form/Reserved
  default: return juce::Colours::black;
  }
}

int LispSyntax::readNextToken(juce::CodeDocument::Iterator &source)
{
  static int par = 0;
  int typ = TokenError;
  source.skipWhitespace();
  juce::juce_wchar chr = source.peekNextChar();
  switch (chr)
  {
  case 0:
    source.skip();
    break;
  case ';':
    source.skipToEndOfLine();
    typ = TokenComment;
    break;
  case '"':
    {
      juce::juce_wchar c, q=source.nextChar(); // pop quote char
      for (c = source.nextChar(); (c != q && c != 0); c = source.nextChar())
        if (c=='\\') source.skip(); // advance over \x
      typ = TokenString;
    }
    break;
  case '#':
    source.skip(); // pop sharp sign
    if (isTokenChar(source.peekNextChar())) // #x #? #t #f etc
      source.skip(); // pop macro char
    typ = TokenSharpsign;
    break;
  case ',':
  case '\'':
  case '`':
    source.skip();
    typ = TokenPlaintext;
    break;
  case '(':
    par = source.getPosition();
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
    source.skip();
    typ = TokenPlaintext;
    break;
  default:
    {
      // advance to end of token, check for special words
      int i = 0;
      juce::String check;
      juce::juce_wchar c, k = 0;
      do 
      {
        c = source.nextChar();
        if (i < maxtoklen) // only gather maxtokenlen chars
          check << c;
        i++;
        k = c;
      } while (isTokenChar(source.peekNextChar()));
      // i is now one beyond last constituent character
      if (chr == ':') // first char was lisp keyword 
        typ = TokenLispKeyword;
      else if (i <= maxtoklen) // check for special word
      {
        // highlight only if symbol immediately follows a '('
        bool func = (par + 1 == source.getPosition() - i);
        if (func && getSynTok(check))
        {
          //std::cout << "par=" << par << ",i=" << i << ", pos=" << source.getPosition() -i << "\n";
          typ = TokenLispSpecialForm;
        }
        else
          typ = TokenPlaintext;              
      }
      else
        typ = TokenPlaintext;              
    }
  }
  return typ;
}

int LispSyntax::getIndentation(juce::CodeDocument& document, int line)
{
  juce::CodeDocument::Position pos ((const juce::CodeDocument&)document, line, 0);
  juce::Array<int> subtypes;
  juce::Array<int> substarts;
  int scan=0;

  // if no lines above point indent 0 else move cursor to end of
  // previous line and scan backward until either unlevel OR a
  // balanced expression in column 0. record types and starting
  // positions of subexpressions traversed

  if (line==0) return 0;
  pos.setLineAndIndex(line-1, INT_MAX); // goto eol of previous line
  while (true)
  {
    scan=scanCode(document, pos, false, ScanIDs::MoveExpressions);
    if (scan>0)
      if (pos.getIndexInLine()==0)
      {
        break;
      }
      else
      {
        // prepend since moving backward
        subtypes.insert(0, scan);
        substarts.insert(0, pos.getPosition());
        // arrrgh! the 1st position problem
        pos.moveBy(-1); 
      }
    else
    {
      break;
    }
  }
  //std::cout << "after loop, scan="<<scan<< ", pos=" << pos.getPosition() << ", col=" << pos.getIndexInLine() << "\n";
  // stopped on a balanced expr in column 0 so indent to whatever
  // position the very LATEST subexpr is or use column 0
  if (scan!=ScanIDs::SCAN_UNLEVEL)
  {
    if (substarts.size()==0)
    {
      ////          std::cout << "balanced indent: 0\n";
      return 0;
    }
    juce::CodeDocument::Position sub ((const juce::CodeDocument&)document, substarts.getLast());
    ////std::cout << "balanced indent: " << sub.getIndexInLine() << "\n";
    return sub.getIndexInLine();
  }
  // otherwise we've stopped on an unbalanced open parens.

  //  std::cout << "unlevel: (" << pos.getPosition() << ") -> '" 
  //            << document.getTextBetween(pos, pos.movedBy(1)).toUTF8() 
  //           << "'\nsubexprs:\n";
  //  for (int i=0; i<subtypes.size(); i++)
  //    std::cout << "  " << substarts[i] << ": " << ScanIDs::scanResultToString(subtypes[i]).toUTF8() << "\n";

  // if no subexprs indent 1 position beyond open parens
  if (subtypes.size()==0)
  {
    ////std::cout << "nothing forward, indent column=" << pos.getIndexInLine()+1 << "\n";
    return pos.getIndexInLine()+1;
  }

  // if first subexpr is NOT a token indent to its column position
  if (subtypes.getFirst()!=ScanIDs::SCAN_TOKEN)
  {
    juce::CodeDocument::Position sub ((const juce::CodeDocument&)document, substarts.getFirst());
    ////std::cout << "not a token, indent column=" << sub.getIndexInLine() << "\n";
    return sub.getIndexInLine();
  }

  // first expr after the open parens is a token, get its name and
  // check for special indentation rules
  juce::CodeDocument::Position tokbeg ((const juce::CodeDocument&)document, substarts.getFirst());
  juce::CodeDocument::Position tokend ((const juce::CodeDocument&)document, substarts.getFirst());
  juce::CodeDocument::Position tokeol ((const juce::CodeDocument&)document, tokbeg.getLineNumber(), INT_MAX);
  scanToken(tokend, 1, tokeol);
  juce::String name=document.getTextBetween(tokbeg,tokend);
  int nargs=substarts.size()-1;    // num subexpr args after token

  //std::cout << "num exprs=" << subtypes.size() << ", numstarts=" << substarts.size() << "\n";

  if (Syntax::SynTok* syntok=getSynTok(name)) // is special form
  {
    // car of list is a special form. get num distinguished args to
    // see if we do a body indent
    int body=syntok->getIndent(); // num distinguished args
 
    // if we have no args or exactly body args then do a body indent
    // otherwise indent to the column of the last subexpr.

    ////std::cout << "special form: '" << name.toUTF8() << "', nargs="<< nargs << ", body=" << body;

    //std::cout << "special nargs=" << nargs << "\n";

    if (nargs==0 || nargs==body)
    {
      ////std::cout << "body indent, indent column=" << pos.movedBy(2).getIndexInLine() << "\n";
      return pos.movedBy(2).getIndexInLine();
    }
    else
    {
      //**  juce::CodeDocument::Position sub (&document, substarts.getLast());
      //**//std::cout << "indent to last expr, indent column=" << sub.getIndexInLine() << "\n";
      //** return sub.getIndexInLine();
      substarts.remove(0);  
      int x=lastIndented(document, substarts, false);
      //std::cout << "returning indent col=" << x << "\n";
      return x;
    }
  }
  else // token not special, indent to last expr or to pos+1 if none
  {
    //std::cout << "no special nargs=" << nargs << "\n";
    if (nargs==0)
    {
      ////std::cout << "normal token with no args, indent column=" << pos.movedBy(1).getIndexInLine() << "\n";
      return pos.movedBy(1).getIndexInLine();
    }
    else
    {
      //** juce::CodeDocument::Position sub (&document, substarts.getLast());
      //**//std::cout << "normal token (args), indent column=" << sub.getIndexInLine() << "\n";
      //return sub.getIndexInLine();
      substarts.remove(0);
      int x=lastIndented(document, substarts, false);
      //std::cout << "returning indent col=" << x << "\n";
      return x;
    }
  }
}

int LispSyntax::backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& from, juce::CodeDocument::Position& to)
{
  juce::CodeDocument::Position pos (to);
  // when moving backwards cursor is always one char after scan start
  pos.moveBy(-1);
  // skip over trailing comments and whitespace
  scanCode(document, pos, false, ScanIDs::MoveWhiteAndComments);
  // pos is now ON ending char of expr (or at eob if only white) so
  // set end to one position beyond last char in expr
  juce::CodeDocument::Position end (pos.movedBy(1));
  // scan backwords to locate start of expr
  int scan=scanCode(document, pos, false, ScanIDs::MoveExpressions);
  from.setPosition(pos.getPosition());
  to.setPosition(end.getPosition());
  return scan;
}

void LispSyntax::eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region)
{
  juce::String code=document.getTextBetween(start, end);
  //std::cout << "eval='" << code.toUTF8() << "'\n";
  if (expand)
    code="(macroexpand " + code + ")";
  if (region)
    code="(begin " + code + ")";
  SchemeThread::getInstance()->eval(code, true);
}

/*=======================================================================*
  Sal Syntax
  *=======================================================================*/

SalSyntax::SalSyntax (bool ini) 
  : Syntax()
{
  if (ini) 
    init();
}

void SalSyntax::init()
{
  type=TextIDs::Sal1;

  setCharSyntax("~!@$%^&*-_=+|:<.>/?", ScanIDs::SYN_SYMBOL);
  setCharSyntax(";", ScanIDs::SYN_COMMENT);
  setCharSyntax("#", ScanIDs::SYN_PREFIX);
  setCharSyntax("\"", ScanIDs::SYN_STRING);
  setCharSyntax("([{", ScanIDs::SYN_OPEN);
  setCharSyntax(")]})", ScanIDs::SYN_CLOSE);
  setCharSyntax(",", ScanIDs::SYN_PUNCT);
  setCharSyntax("\\", ScanIDs::SYN_ESCAPE);

  addSynTok( "begin", SalIDs::SalBegin, 1, 0); // 1 = #b01 = indent
  addSynTok( "chdir", SalIDs::SalChdir);
  addSynTok( "define", SalIDs::SalDefine, 1, 3);
  addSynTok( "else", SalIDs::SalElse, 3, 0);  // 3 = #b11 = undent/indent
  addSynTok( "end", SalIDs::SalEnd, 2, 0);      // 2 = #10 = undent
  addSynTok( "exec", SalIDs::SalExec);
  addSynTok( "fomusfile", SalIDs::SalFomusFile, 1, 2);
  addSynTok( "if", SalIDs::SalIf);
  addSynTok( "load", SalIDs::SalLoad);
  addSynTok( "loop", SalIDs::SalLoop, 1, 0);
  addSynTok( "plot", SalIDs::SalPlot);
  addSynTok( "print", SalIDs::SalPrint);
  addSynTok( "return", SalIDs::SalReturn);
  addSynTok( "run", SalIDs::SalRun, 1, 0);
  addSynTok( "send", SalIDs::SalSend);
  addSynTok( "set", SalIDs::SalSet);
  addSynTok( "soundfile", SalIDs::SalSoundFile, 1, 2);
  addSynTok( "sprout", SalIDs::SalSprout);
  addSynTok( "then", SalIDs::SalThen);
  addSynTok( "unless", SalIDs::SalUnless);
  addSynTok( "until", SalIDs::SalUntil);
  addSynTok( "wait", SalIDs::SalWait);
  addSynTok( "when", SalIDs::SalWhen);
  addSynTok( "while", SalIDs::SalWhile);
  addSynTok( "with", SalIDs::SalWith);
  addSynTok( "process", SalIDs::SalProcess);
  addSynTok( "function", SalIDs::SalFunction);
  addSynTok( "variable", SalIDs::SalVariable);
  
  addSynTok( "above", SalIDs::SalAbove);
  addSynTok( "below", SalIDs::SalBelow);
  addSynTok( "by", SalIDs::SalBy);
  addSynTok( "downto", SalIDs::SalDownto);
  addSynTok( "finally", SalIDs::SalFinally);
  addSynTok( "for", SalIDs::SalFor);
  addSynTok( "from", SalIDs::SalFrom);
  addSynTok( "in", SalIDs::SalIn);
  addSynTok( "over", SalIDs::SalOver);
  addSynTok( "repeat", SalIDs::SalRepeat);
  addSynTok( "to", SalIDs::SalTo);
  
  //  Operators, data field is op weight
  addSynTok( "|", SalIDs::SalOr);
  addSynTok( "&", SalIDs::SalAnd);
  addSynTok( "!", SalIDs::SalNot);
  addSynTok( "=", SalIDs::SalEqual); // relation and op
  addSynTok( "!=", SalIDs::SalNotEqual);  
  addSynTok( "<", SalIDs::SalLess);
  addSynTok( ">", SalIDs::SalGreater);
  addSynTok( "<=", SalIDs::SalLessEqual); // relation and op
  addSynTok( ">=", SalIDs::SalGreaterEqual); // relation and op
  addSynTok( "~=", SalIDs::SalGeneralEqual);  
  addSynTok( "+", SalIDs::SalPlus);
  addSynTok( "-", SalIDs::SalMinus);
  addSynTok( "%", SalIDs::SalMod);
  addSynTok( "*", SalIDs::SalTimes);  
  addSynTok( "/", SalIDs::SalDivide);
  addSynTok( "^", SalIDs::SalExpt);
  // assignment (also: = <= >=)
  addSynTok( "+=", SalIDs::SalInc);
  addSynTok( "*=", SalIDs::SalMul);
  addSynTok( "&=", SalIDs::SalCol);
  addSynTok( "@=", SalIDs::SalPre);  
  addSynTok( "^=", SalIDs::SalApp);
  // hash tokens
  addSynTok( "#t", SalIDs::SalTrue);
  addSynTok( "#f", SalIDs::SalFalse);
  addSynTok( "#?", SalIDs::SalQMark);
  addSynTok( "#$", SalIDs::SalUnquote);
  addSynTok( "#^", SalIDs::SalSplice);
}

SalSyntax::~SalSyntax()
{
  clearSingletonInstance();
}

juce::CodeEditorComponent::ColourScheme SalSyntax::getDefaultColourScheme ()
{
  // juce's CodeEditorComponent uses this array to name the
  // Tokenizer's tokenTypes, where each array index is a tokenType and
  // the string at that postion is its name. we use this array to map
  // tokenTypes to ColorTheme attribute names in the Xml ColorTheme.
  // for attributes see ColorThemeIDs class in Enumerations.h

  juce::CodeEditorComponent::ColourScheme cs;

  // Color Theme Attribute       Token Type
  //-------------------------------------------
  cs.set("error",     juce::Colours::red);             // TokenError
  cs.set("plaintext", juce::Colours::black);           // TokenPlaintext
  cs.set("comment",   juce::Colour(0xce, 0x00, 0x1a)); // TokenComment
  cs.set("string",    juce::Colours::rosybrown); // TokenString
  cs.set("keyword1",  juce::Colours::cadetblue);       // TokenSharpsign
  cs.set("keyword2",  juce::Colour(0x8a, 0x2f, 0x8f)); // TokenLispKeyword
  cs.set("keyword3",  juce::Colours::orchid);          // TokenSalKeyword
  cs.set("literal1",  juce::Colour(0x95, 0x00, 0x83)); // TokenSalReserved
  cs.set("literal2",  juce::Colours::forestgreen);     // TokenSalClassname
  cs.set("literal3",  juce::Colours::blue);            // TokenSalCommand
  return cs;
}

juce::Colour SalSyntax::getDefaultColour (int tokenType)
{
  ////  return Colours::black;
  switch (tokenType)
  {
  case SalIDs::TokenError: return juce::Colours::red;
  case SalIDs::TokenPlaintext: return juce::Colours::black;
  case SalIDs::TokenComment: return juce::Colour(0xce, 0x00, 0x1a); //juce::Colours::firebrick;
  case SalIDs::TokenString: return juce::Colours::pink; //juce::Colours::rosybrown; juce::Colour(0xa1, 0x11, 0x53)
  case SalIDs::TokenSharpSign: return juce::Colours::cadetblue; 
  case SalIDs::TokenLispKeyword: return juce::Colour(0x8a, 0x2f, 0x8f); // Lisp Keyword juce::Colours::cadetblue
  case SalIDs::TokenSalKeyword: return juce::Colours::orchid; 
  case SalIDs::TokenSalReserved: return juce::Colour(0x95, 0x00, 0x83);
  case SalIDs::TokenSalClassname: return juce::Colours::forestgreen;
  case SalIDs::TokenSalCommand: return juce::Colours::blue; 
  default: return juce::Colours::black;
  }
}

int SalSyntax::readNextToken(juce::CodeDocument::Iterator &source)
{
  int typ=SalIDs::TokenError;
  juce::juce_wchar chr=0;
  while (source.peekNextChar() && isWhiteChar(source.peekNextChar()))
    chr=source.nextChar();
  bool newline=(chr=='\n');
  chr=source.peekNextChar();
  switch (chr)
  {
  case 0:
    source.skip();
    break;
  case ';':
    source.skipToEndOfLine();
    typ=SalIDs::TokenComment;
    break;
  case '"':
    {
      juce::juce_wchar c, q=source.nextChar(); // pop quote char
      for (c=source.nextChar(); (c!=q && c!=0); c=source.nextChar())
        if (c=='\\') source.skip(); // advance over \x
      typ=SalIDs::TokenString;
    }
    break;
  case '#':
    source.skip(); // pop sharp sign
    if (isTokenChar(source.peekNextChar())) // #x #? #t #f etc
      source.skip(); // pop macro char
    typ=SalIDs::TokenSharpSign;
    break;
  case ',':
    source.skip();
    typ=SalIDs::TokenPlaintext;
    break;
  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
    source.skip();
    typ=SalIDs::TokenPlaintext;
    break;
  case '\'':
    source.skip();
    typ=SalIDs::TokenError;
    break;
  default:
    {
      int i=0;
      // check for special syntax words
      juce::String check;
      juce::juce_wchar c, k=0;
      // read until a non-constituent char is in next position
      // adding consituent chars to check as long as they fit
      do
      {
        c=source.nextChar();
        if (i<maxtoklen) // only gather maxtokenlen chars
          check<<c;
        i++;
        k=c;
      } 
      while (isTokenChar(source.peekNextChar()));

      // i is now one beyond last constituent character
      if (chr==':') // first char was lisp keyword 
        typ=SalIDs::TokenLispKeyword;
      else if (k==':') // sal keyword 
        typ=SalIDs::TokenSalKeyword;
      else if (i<=maxtoklen) // check for special word
      {
        SynTok* tok=getSynTok(check);
        if (tok)
        {
          int ttyp=tok->getType();
          //std::cout << "linestart=" << newline << "literal=" << check.toUTF8() << "\n";
          if (newline && (SalIDs::isSalCommandType(ttyp) || ttyp==SalIDs::SalEnd))
          {
            typ=SalIDs::TokenSalCommand;                    
          }
          else if (SalIDs::isSalDefineType(ttyp)) // classname
            typ=SalIDs::TokenSalClassname;
          else if (SalIDs::isSalLiteralType(ttyp))
            typ=SalIDs::TokenSalReserved;
          else
            typ=SalIDs::TokenPlaintext;
        }
        else
          typ=SalIDs::TokenPlaintext;
      }
      else
        typ=SalIDs::TokenPlaintext;
    }
  }
  return typ;
}

int SalSyntax::getIndentation(juce::CodeDocument& document, int line)
// THIS METHOD IS SAL2 BUT WORKS OK FOR SAL SO DEFINED HERE
{
  bool trace=false;
  int orig=line;
  juce::CodeDocument::Position pos ((const juce::CodeDocument&)document, line, 0);
  juce::Array<int> subtypes;
  juce::Array<int> substarts;
  Syntax::SynTok* cmdtoken=0;
  int scan=0;
  int col=0;

  // if no lines above point indent 0
  if (line==0) return 0;
  // move to EOL of line above cursor
  line--;
  pos.setLineAndIndex(line, INT_MAX);

  // scan backwards until either unlevel OR a command or a balanced
  // expr in column 0. record types and starting positions of
  // subexpressions traversed
  while (true)
  {
    scan=scanCode(document, pos, false, ScanIDs::MoveExpressions);
    if (scan>0)
    {
      // prepend expr type since moving backward
      subtypes.insert(0, scan);
      substarts.insert(0, pos.getPosition());
      if (scan==ScanIDs::SCAN_TOKEN)
      {
        juce::CodeDocument::Position end (pos);
        juce::CodeDocument::Position eol ((const juce::CodeDocument&)document, pos.getLineNumber(), INT_MAX);
        scanToken(end, 1, eol);
        juce::String str=document.getTextBetween(pos,end);
        if (Syntax::SynTok* tok=getSynTok(str))
        {
          // update subtype with actual sal type. if type is a
          // command, 'end' or 'else' then break
          int type=tok->getType();
          subtypes.set(0,type);
          if (SalIDs::isSalCommandType(type) )
          {
            cmdtoken=tok;
            break;
          }
        }
        else if (pos.getIndexInLine()==0)
          break;
      }
      else if (pos.getIndexInLine()==0)
        break;
      pos.moveBy(-1); 
    }
    else
      break;
  }

  if (scan<0) // scan stopped on unlevel parens
  {
    if (substarts.size()>0) // use last subexpr indentation
    {
      col=lastIndented(document, substarts, false);
      if (trace) std::cout << "UNLEVEL indent (last expr), column=" << col << "\n"; 
      return col;
    }
    else
    {
      col=pos.movedBy(1).getIndexInLine();
      if (trace) std::cout << "UNLEVEL indent (no exprs), column=" << col << "\n";
      return col;
    }
  }
  else if (subtypes.size()==0) // no expressions encountered (only white space)
  {
    if (trace) std::cout << "EMPTY indent, column=0\n" ;
    return 0;
  }  

  // at this point we have at least one subexpr and may have stopped
  // on a sal type

  if (!SalIDs::isSalType(subtypes[0]))     // stopped on a non-sal expression
  {
    col=lastIndented(document,substarts, false);
    if (trace) std::cout << "SEXPR indent, column=" << col << "\n";
  }
  else // types[0] is sal entity, hopefully a command
  {
    if (cmdtoken)  // we stopped on a command
    {
      if (trace){std::cout << "CMDTOKEN: " << cmdtoken->getName() << "\n";
        std::cout << "  subtypes:";
        for (int q=0;q<subtypes.size();q++) std::cout << " " << juce::String::toHexString(subtypes[q]);
        std::cout << "\n";
      }

      int cmdline=pos.getLineNumber(); // num of line with cmd
      // last is INDEX of last indented or command we stopped on
      int last=lastIndented(document, substarts, true);
      // IF the VERY last line ends with comma then indent 1 past
      // command name or a subexpr 'with' (if found)
      if (isCommaTerminatedLine(document, line))
      {
        for (last=subtypes[subtypes.size()-1]; last>=0; last--)
          if (subtypes[last]==SalIDs::SalWith)
            break;
        if (last<0) // reached command
          col=pos.getIndexInLine()+cmdtoken->getName().length()+1;
        else // indent relative to 'with'
        {
          juce::CodeDocument::Position p ((const juce::CodeDocument&)document, substarts[last]);
          col=p.getIndexInLine()+4+1;
        }
        if (trace) std::cout << "COMMA indent, column=" << col << "\n";
      }
      // ELSE (the very last line does NOT end with comma) if
      // line-1 is >= cmdline and DOES end with comma then we are
      // done with comma indenting so intent to body or pos
      else if ((cmdline<=line-1) && isCommaTerminatedLine(document, line-1))
      {
        if (cmdtoken->getData1()>0  && (!subtypes.contains(SalIDs::SalEnd)))
        {
          col=pos.getIndexInLine()+2;
          if (trace) std::cout << "*BODY indent (after comma stop), column=" << col << "\n";
        }
        else
        {
          col=pos.getIndexInLine();
          if (trace) std::cout << "RESET indent (after comma stop), column=" << col << "\n";
        }
      }
      // ELSE if the last indented is 'else' then body indent
      // based on position of 'else'
      else if (subtypes[last]==SalIDs::SalElse)
      {
        juce::CodeDocument::Position p ((const juce::CodeDocument&)document, substarts[last]);
        col=p.getIndexInLine()+2;
        if (trace) std::cout << "ELSE indent, column=" << col << "\n";
      }          
      // else if the command is a body indent WITHOUT a closing end, indent to the last
      // expression or to 2 past the first (command) expr
      else if (cmdtoken->getData1()>0 && (!subtypes.contains(SalIDs::SalEnd)))
      {
        if (last==0) // the command, no subexprs on their own lines
        {
          if (subtypes.size()>1 && subtypes[1]==SalIDs::SalVariable)
          {
            col=pos.getIndexInLine();
            if (trace) std::cout << "NOT BODY indent, column=" << col << "\n";
          }
          else
          {
            col=pos.getIndexInLine()+2;
            if (trace) std::cout << "BODY indent, column=" << col << "\n";
          }
        }
        // else indent to the last expression
        else
        {
          juce::CodeDocument::Position p ((const juce::CodeDocument&)document, substarts[last]);
          col=p.getIndexInLine();
          if (trace) std::cout << "LAST indent (body), column=" << col << "\n";
        }
      }
      // else indent to the last expression
      else
      {
        juce::CodeDocument::Position p ((const juce::CodeDocument&)document, substarts[last]);
        col=p.getIndexInLine();
        if (trace) std::cout << "LAST indent, column=" << col << "\n";
      }
    }
    else
    {
      col=lastIndented(document, substarts, false);
      if (trace) std::cout << "non-standard sal indent, column=" << col << "\n";
    }
  }

  // if we are looking at an 'end' or an 'else' in the cursor line
  // then adjust -2

  juce::CodeDocument::Position bol ((const juce::CodeDocument&)document, orig, 0);
  juce::CodeDocument::Position eol ((const juce::CodeDocument&)document, orig, INT_MAX); 
  while (bol!=eol && (bol.getCharacter()==' ' || bol.getCharacter()=='\t')) 
    bol.moveBy(1);
  if (trace) std::cout << "line is "<< bol.getLineText() << "\n";
  if (lookingAt(bol, "end", true, true) || lookingAt(bol, "else", true, true))
  {
    col-=2;
    if (trace) std::cout << "cursor is looking at end or else\n";
  }
  else if (getTextType()==TextIDs::Sal1 && lookingAt(bol, "define", true, true) )
  {
    // looking at define in sal1 keep column wherever it is
    col=bol.getIndexInLine(); 
  }
  return juce::jmax(col,0);
}

int SalSyntax::backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& from, juce::CodeDocument::Position& to)
{
  // in sal1 backward expression finds the nearest command literal
  // that start in 0th column position of a line
  // moving backwards the cursor is always one char past scan start
  to.moveBy(-1);
  // skip trailing comments and whitespace
  scanCode(document, to, false, ScanIDs::MoveWhiteAndComments);
  // to is now ON ending char of expr (or at eob if only white) so set
  // end to one beyond ending char of expr
  to.moveBy(1);
  int line=to.getLineNumber();
  while (line>=0)
  {
    from.setLineAndIndex(line, 0);
    juce::CodeDocument::Position t (from);
    juce::CodeDocument::Position e ((const juce::CodeDocument&)document, line, INT_MAX); // end of current line
    if (scanToken(t, 1, e))
    {
      if (Syntax::SynTok* tok=getSynTok(document.getTextBetween(from, t)))
      {
        if (SalIDs::isSalCommandType(tok->getType()))
        {
          //std::cout << "command=" << tok->getName().toUTF8() << "\n";
          return 1;
        }
      }
    }
    line-=1;
  }
  return 0;
}

const juce::String& SalSyntax::getLastParseError()
{
  return errormessage;
}

bool SalSyntax::tokenize(juce::String str, juce::OwnedArray<SynTok>& toks)
{
  juce::CodeDocument doc;
  doc.replaceAllContent(str);
  juce::CodeDocument::Position start ((const juce::CodeDocument&)doc,0);
  juce::CodeDocument::Position end ((const juce::CodeDocument&)doc,INT_MAX);
  return tokenize(doc,start,end,toks);
}

bool SalSyntax::tokenize(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end,
                         juce::OwnedArray<SynTok>& tokens)
{
  // forward scan sal tokens and call sal on result. if region is true
  // then surround the add 'begin' and 'end' to the string and token
  // array adjust string positions accordingly
 
  //juce::String code=document.getTextBetween(start, end);
  //std::cout << "expr='" << code.toUTF8() << "', start=" << start.getPosition() 
  //          << ", end=" << end.getPosition() << "\n";
  errormessage=juce::String();
  juce::CodeDocument::Position pos (start);
  int beg=pos.getPosition();  // offset of string in document
  int lev=0, par=0, cur=0, squ=0;
  int scan=scanCode(document, pos,true,ScanIDs::MoveWhiteAndComments, end.getPosition());
  while (pos!=end)
  {
    juce::CodeDocument::Position far(pos);
    scan=scanCode(document, far, true, ScanIDs::MoveTokens, end.getPosition());
    if (scan<=0) break;
    int loc=pos.getPosition()-beg;
    if (scan==ScanIDs::SCAN_OPEN)
    {
      juce::juce_wchar c=pos.getCharacter();
      if (c=='(')
      {
        par++;
        tokens.add(new Syntax::SynTok("(", SalIDs::SalLParen, loc));
      }
      else if (c=='{')
      {
        cur++;
        tokens.add(new Syntax::SynTok("{", SalIDs::SalLCurly, loc));
      }
      else if (c=='[')
      {
        squ++;
        tokens.add(new Syntax::SynTok("[", SalIDs::SalLBrace, loc));
      }
    }
    else if (scan==ScanIDs::SCAN_CLOSE)
    {
      juce::juce_wchar c=pos.getCharacter();
      if (c==')')
      {
        if (--par < 0) break;
        tokens.add(new Syntax::SynTok(")", SalIDs::SalRParen, loc));
      }
      else if (c=='}')
      {
        if (--cur < 0) break;
        tokens.add(new Syntax::SynTok("}", SalIDs::SalRCurly, loc));
      }
      else if (c==']')
      {
        if (--squ < 0) break;
        tokens.add(new Syntax::SynTok("]", SalIDs::SalRBrace, loc));
      }
    }
    else if (scan==ScanIDs::SCAN_PUNCT)
      tokens.add(new Syntax::SynTok(",", SalIDs::SalComma, loc));
    else if (scan==ScanIDs::SCAN_STRING)
    {
      juce::String txt=document.getTextBetween(pos,far).unquoted();
      // if we have escaped chars remove first level of escape
      if (txt.containsChar('\\'))
      {
        bool esc=false;
        juce::String unq=juce::String();
        //std::cout << "txt='" << txt.toUTF8() << "'\n";
        for (int e=0;e<txt.length();e++)
          if (txt[e]=='\\')
            if (!esc) 
              esc=true; 
            else
            {
              unq << txt[e]; 
              esc=false;
            }
          else
          {
            unq << txt[e]; 
            esc=false;
          }
        txt=unq;
        //std::cout << "txt='" << txt.toUTF8() << "'\n";
      }
      tokens.add(new Syntax::SynTok(txt, SalIDs::SalString, loc));
    }
    else if (scan==ScanIDs::SCAN_TOKEN)
    {
      juce::String s=document.getTextBetween(pos,far);
      if (Syntax::SynTok* t=getSynTok(s))
      {
        int x=t->getType();
        tokens.add(new Syntax::SynTok(s, x, loc));
        if (SalIDs::isSalBlockOpen(x))
          lev++;
        else if (SalIDs::isSalBlockClose(x))
        {
          if (--lev < 0) break;
        }
      }
      else if (int t=isNumberToken(s))
      {
        int typs[] = {SalIDs::SalInteger, SalIDs::SalFloat, SalIDs::SalRatio};
        tokens.add(new Syntax::SynTok(s, typs[t-1], loc));
      }
      else // is still a valid token!
      {
        if (s.getLastCharacter()==':')
          tokens.add(new Syntax::SynTok(s.dropLastCharacters(1), SalIDs::SalKeyparam, loc));
        else if (s[0]==':')
          tokens.add(new Syntax::SynTok(s.substring(1), SalIDs::SalKeyword, loc));
        else
          tokens.add(new Syntax::SynTok(s, SalIDs::SalIdentifier, loc));
      }
    }
    pos=far;
    scan=scanCode(document, pos,true,ScanIDs::MoveWhiteAndComments, end.getPosition());
  }

  //std::cout << "tokens="; for (int i=0; i<tokens.size(); i++) std::cout << " " << tokens[i]->toString().toUTF8(); std::cout << "\n";

  if (scan<0)
  {
    errormessage << ">>> Error: illegal token, line: " //<< pos.getLineNumber() 
                 << "\n" << pos.getLineText() << "\n";
    return false;
  }
  else if (lev!=0 || par!=0 || cur!=0 || squ!=0)
  {
    Syntax::SynTok* tok=NULL;
    if (lev<0) // too many ends
      errormessage << ">>> Error: extraneous 'end', line: ";
    else if (par<0)
      errormessage << ">>> Error: extraneous ')', line: ";
    else if (cur<0)
      errormessage << ">>> Error: extraneous '}', line: ";
    else if (squ<0)
      errormessage << ">>> Error: extraneous ']', line: ";
    else if (lev>0)
    {
      errormessage << ">>> Error: missing 'end'";
      for (int i=tokens.size()-1, n=0; i>-1; i--)
      {
        int x=SalIDs::SalTypeDataBits(tokens[i]->getType());
        if (x==SalIDs::SalBlockOpen)
          if (n==lev) {tok=tokens[i]; break;} else n--;
        else if (x==SalIDs::SalBlockClose) n++;
      }
      if (tok) errormessage << " for " << tok->getName();
      errormessage << ", line: ";
    }
    else if (par>0)
    {
      errormessage << ">>> Error: missing ')', line: ";
      for (int i=tokens.size()-1, n=0; i>-1; i--)
        if (tokens[i]->getType()==SalIDs::SalLParen) 
          if (n==par) {tok=tokens[i]; break;} else n--;
        else if (tokens[i]->getType()==SalIDs::SalRParen) n++;
    }
    else if (cur>0)
    {
      errormessage << ">>> Error: missing '}', line: ";
      for (int i=tokens.size()-1, n=0; i>-1; i--)
        if (tokens[i]->getType()==SalIDs::SalLCurly) 
          if (n==cur) {tok=tokens[i]; break;} else n--;
        else if (tokens[i]->getType()==SalIDs::SalRCurly) n++;
    }
    else if (squ>0)
    {
      errormessage << ">>> Error: missing ']', line: ";
      for (int i=tokens.size()-1, n=0; i>-1; i--)
        if (tokens[i]->getType()==SalIDs::SalLBrace) 
          if (n==cur) {tok=tokens[i]; break;} else n--;
        else if (tokens[i]->getType()==SalIDs::SalRBrace) n++;
    }
    if (tok) pos.setPosition(beg+tok->getData1());
    errormessage // << pos.getLineNumber() 
      << "\n"  << pos.getLineText() << "\n";
    return false;
  }
  return true;
}

void SalSyntax::eval(juce::CodeDocument& document, const juce::CodeDocument::Position start, const juce::CodeDocument::Position end, bool expand, bool region)
{
  juce::OwnedArray<Syntax::SynTok> tokens;
  if (tokenize(document, start, end, tokens))
  {
    juce::String text=document.getTextBetween(start, end);
    XSalNode* node=new XSalNode(0.0, text, getTextType(), expand, region);
    // swapping moves tokens from the local array to the evalnode's
    // array AND clears the local array in a single
    // operation. swapping must be done or the tokens will be
    // deleted when the local array goes out of scope!
    ////    node->toks.swapWithArray(tokens);

    node->toks.swapWith(tokens);

    //std::cout << "tokens=";
    //for (int i=0; i<node->toks.size(); i++)
    //  std::cout << " " << node->toks[i]->toString().toUTF8();
    //std::cout << "\n";
    SchemeThread::getInstance()->addNode(node);
  }
  else
    Console::getInstance()->printError(getLastParseError());
}

/*=======================================================================*
  Sal2 Syntax
  *=======================================================================*/

Sal2Syntax::Sal2Syntax () 
: SalSyntax(false)
{
  type=TextIDs::Sal2;

  setCharSyntax("~!@$%^&*-_=+|:<.>/?", ScanIDs::SYN_SYMBOL);
  setCharSyntax(";", ScanIDs::SYN_COMMENT);
  setCharSyntax("#", ScanIDs::SYN_PREFIX);
  setCharSyntax("\"", ScanIDs::SYN_STRING);
  setCharSyntax("([{", ScanIDs::SYN_OPEN);
  setCharSyntax("}])", ScanIDs::SYN_CLOSE);
  setCharSyntax(",", ScanIDs::SYN_PUNCT);
  setCharSyntax("\\", ScanIDs::SYN_ESCAPE);

  //        NAME       TYPE              BLOCKINDENT?  NUMDISTINGUISHED
  addSynTok("begin", SalIDs::SalBegin,        1, 0); // #b01 = +body indent
  addSynTok("end", SalIDs::SalEnd,            2, 0); // #b10 = -body indent
  addSynTok("function", SalIDs::Sal2Function, 1, 2);
  addSynTok("process", SalIDs::Sal2Process,   1, 2);
  addSynTok("if", SalIDs::Sal2If,             1, 2);
  addSynTok("loop", SalIDs::SalLoop,          1, 0);
  addSynTok("file", SalIDs::Sal2File,         1, 2);
  ////addSynTok("return", SalIDs::SalReturn, HiliteIDs::Hilite4);
  addSynTok("run", SalIDs::SalRun,            1, 0);
  ////addSynTok("send", SalIDs::SalSet, HiliteIDs::Hilite4);
  addSynTok("set", SalIDs::SalSet);
  addSynTok("variable", SalIDs::Sal2Variable);
  addSynTok("wait", SalIDs::Sal2Wait);
  // loop/run/if clausals
  addSynTok("above", SalIDs::SalAbove);
  addSynTok("below", SalIDs::SalBelow);
  addSynTok("by", SalIDs::SalBy);
  addSynTok("downto", SalIDs::SalDownto);
  addSynTok("else", SalIDs::SalElse,          3, 0); // #b11= -this +next
  addSynTok("finally", SalIDs::SalFinally);
  addSynTok("for", SalIDs::SalFor);
  addSynTok("from", SalIDs::SalFrom);
  addSynTok("in", SalIDs::SalIn);
  addSynTok("over", SalIDs::SalOver);
  addSynTok("repeat", SalIDs::SalRepeat);
  addSynTok("then", SalIDs::SalThen);
  addSynTok("to", SalIDs::SalTo);
  addSynTok("unless", SalIDs::SalUnless);
  addSynTok("until", SalIDs::SalUntil);
  //  addSynTok("wait", SalIDs::SalWait);
  addSynTok("when", SalIDs::SalWhen);
  addSynTok("while", SalIDs::SalWhile);
  addSynTok("with", SalIDs::SalWith);
  addSynTok("&optkey", SalIDs::SalOptKey);
  //  Operators, data field is op weight
  addSynTok("|", SalIDs::SalOr);
  addSynTok("&", SalIDs::SalAnd);
  addSynTok("!", SalIDs::SalNot);
  addSynTok("=", SalIDs::SalEqual);
  addSynTok("!=", SalIDs::SalNotEqual);  
  addSynTok("<", SalIDs::SalLess);
  addSynTok(">", SalIDs::SalGreater);
  addSynTok("<=", SalIDs::SalLessEqual);
  addSynTok(">=", SalIDs::SalGreaterEqual);
  addSynTok("~=", SalIDs::SalGeneralEqual);  
  addSynTok("+", SalIDs::SalPlus);
  addSynTok("-", SalIDs::SalMinus);
  addSynTok("%", SalIDs::SalMod);
  addSynTok("*", SalIDs::SalTimes);  
  addSynTok("/", SalIDs::SalDivide);
  addSynTok("^", SalIDs::SalExpt);
  // assignment (also: = <= >=)
  addSynTok("+=", SalIDs::SalInc);
  addSynTok("*=", SalIDs::SalMul);
  addSynTok("&=", SalIDs::SalCol);
  addSynTok("@=", SalIDs::SalPre);  
  addSynTok("^=", SalIDs::SalApp);
  // hash tokens
  addSynTok("#t", SalIDs::SalTrue);
  addSynTok("#f", SalIDs::SalFalse);
  addSynTok("#?", SalIDs::SalQMark);
  addSynTok("#$", SalIDs::SalUnquote);
  addSynTok("#^", SalIDs::SalSplice);
}

Sal2Syntax::~Sal2Syntax () 
{
  clearSingletonInstance();
}

juce::CodeEditorComponent::ColourScheme Sal2Syntax::getDefaultColourScheme ()
{
  // juce's CodeEditorComponent uses this array to name the
  // Tokenizer's tokenTypes, where each array index is a tokenType and
  // the string at that postion is its name. we use this array to map
  // tokenTypes to ColorTheme attribute names in the Xml ColorTheme.
  // for attributes see ColorThemeIDs class in Enumerations.h

  juce::CodeEditorComponent::ColourScheme cs;

  // Color Theme Attribute       Token Type
  //-------------------------------------------
  cs.set("error",     juce::Colours::red);             // TokenError
  cs.set("plaintext", juce::Colours::black);           // TokenPlaintext
  cs.set("comment",   juce::Colour(0xce, 0x00, 0x1a)); // TokenComment
  cs.set("string",    juce::Colours::rosybrown);       // TokenString juce::Colour(0xa1, 0x11, 0x53)
  cs.set("keyword1",  juce::Colours::cadetblue);       // TokenSharpsign
  cs.set("keyword2",  juce::Colour(0x8a, 0x2f, 0x8f)); // TokenLispKeyword
  cs.set("keyword3",  juce::Colours::orchid);          // TokenSalKeyword
  cs.set("literal1",  juce::Colour(0x95, 0x00, 0x83)); // TokenSalReserved
  return cs;
}

juce::Colour Sal2Syntax::getDefaultColour (int tokenType)
{
  ////  return Colours::black;
  switch (tokenType)
  {
  case SalIDs::TokenError: return juce::Colours::red;
  case SalIDs::TokenPlaintext: return juce::Colours::black;
  case SalIDs::TokenComment: return juce::Colour(0xce, 0x00, 0x1a); //juce::Colours::firebrick;
  case SalIDs::TokenString: return juce::Colour(0xa1, 0x11, 0x53); //juce::Colours::rosybrown;
  case SalIDs::TokenSharpSign: return juce::Colours::cadetblue; 
  case SalIDs::TokenLispKeyword: return juce::Colour(0x8a, 0x2f, 0x8f); // Lisp Keyword juce::Colours::cadetblue
  case SalIDs::TokenSalKeyword: return juce::Colours::orchid; 
  case SalIDs::TokenSalReserved: return juce::Colour(0x95, 0x00, 0x83);
  default: return juce::Colours::black;
  }
}

int Sal2Syntax::readNextToken(juce::CodeDocument::Iterator &source)
{
  int typ=SalIDs::TokenError;
  source.skipWhitespace();
  juce::juce_wchar chr=source.peekNextChar();
  switch (chr)
  {
  case 0:
    source.skip();
    break;
  case ';':
    source.skipToEndOfLine();
    typ=SalIDs::TokenComment;
    break;
  case '"':
    {
      juce::juce_wchar c, q=source.nextChar(); // pop quote char
      for (c=source.nextChar(); (c!=q && c!=0); c=source.nextChar())
        if (c=='\\') source.skip(); // advance over \x
      typ=SalIDs::TokenString;
    }
    break;
  case '#':
    source.skip(); // pop sharp sign
    if (isTokenChar(source.peekNextChar())) // #x #? #t #f etc
      source.skip(); // pop macro char
    typ=SalIDs::TokenSharpSign;
    break;
  case ',':
    source.skip();
    typ=SalIDs::TokenPlaintext;
    break;
  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
    source.skip();
    typ=SalIDs::TokenPlaintext;
    break;
  case '\'':
    source.skip();
    typ=SalIDs::TokenError;
    break;
  default:
    {
      // at this point char is a token char. read until next
      // position is a non-token char, adding consituent chars to
      // check as long as position is not greater than the longest
      // literal to check
      int i=0;
      juce::String check=juce::String();
      juce::juce_wchar c, k=0;
      do
      {
        c=source.nextChar();
        if (i<maxtoklen) // only gather maxtokenlen chars
          check<<c;
        i++;
        k=c;
      } 
      while (isTokenChar(source.peekNextChar()));
      // i is now one beyond last constituent character
      if (chr==':') // lisp style (first char)
        typ=SalIDs::TokenLispKeyword;
      else if (k==':') // sal style (last char)
        typ=SalIDs::TokenSalKeyword;
      else if (i<=maxtoklen)
      {
        SynTok* tok=getSynTok(check);
        if (tok && SalIDs::isSalLiteralType(tok->getType()))
          typ=SalIDs::TokenSalReserved;
        else
          typ=SalIDs::TokenPlaintext;              
      }
      else
        typ=SalIDs::TokenPlaintext;              
    }
  }
  return typ;
}

int Sal2Syntax::backwardExpr(juce::CodeDocument& document, juce::CodeDocument::Position& from, juce::CodeDocument::Position& to)
{
  //juce::CodeDocument::Position pos (getCaretPos());
  juce::CodeDocument::Position pos (to);
  juce::CodeDocument::Position bob ((const juce::CodeDocument&)document, 0);  // beginning of buffer
  int scan=0, last=0, here=-1, level=0, scanned=0;
#define SCAN_CURLY (ScanIDs::SCAN_PUNCT+1)
#define SCAN_SQUARE (ScanIDs::SCAN_PUNCT+2)

  // cursor always 1 past start of backward scan
  pos.moveBy(-1);
  // move backward over white and comments
  scanCode(document, pos, false, ScanIDs::MoveWhiteAndComments);
  // pos now on last constituent char for backward scan (or at
  // bob). set scan's exclusive upper bound to 1 above pos
  juce::CodeDocument::Position top (pos.movedBy(1));

  if (lookingAt(pos, "end", false, true)) // at block end
    while (true)
    {
      // now on last constituent char for scan (or at bob). set the
      // exclusive upper bound of expr to 1 above that
      juce::CodeDocument::Position end (pos.movedBy(1)); 
      scan=scanCode(document, pos, false, ScanIDs::MoveExpressions);
      // quit if error or only white space
      if (scan<=0)
        break;
      juce::String code=document.getTextBetween(pos, end);
      //std::cout << "Code ("<<from.getPosition()<<","<<to.getPosition()<<"): '" << code.toUTF8() << "'\n";
      if (scan==ScanIDs::SCAN_TOKEN)
      {
        scanned++;
        if (Syntax::SynTok* tok=getSynTok(code))
        {
          // token is a literal (e.g. statement, op)
          int typ=tok->getType();
          if (SalIDs::isSalBlockClose(typ))
            level++; 
          else if (SalIDs::isSalBlockOpen(typ))
            level--; 
          // stop on a blockopen word with no pending end. this
          // doesn't mean the statement is actually balanced
          // since the word could be reached without seeing a
          // balancing end (in which case level is negative)
          if (level<=0)
          {
            here=pos.getPosition();
            break;
          }
        }
      }
      if (pos==bob) break;
      pos.moveBy(-1);
      scanCode(document, pos, false, ScanIDs::MoveWhiteAndComments);
    }
  else   
    // else parse expr back, possibly to a command (set or variable)
    // but stopping before any clausal
    while (true)
    {
      // pos is on last consitute char of backwards expr (or at bol)
      // set exclusive end of current expr and scan backwards
      juce::CodeDocument::Position end (pos.movedBy(1)); 
      scan=scanCode(document, pos, false, ScanIDs::MoveExpressions);
      // quit if scan error or only white space
      if (scan<=0)
      {
        //std::cout << "breaking with scan <= 0: " << scan << "\n";;
        here=-1; // FIXED?
        break;
      }
      juce::String code=document.getTextBetween(pos,end);
      //std::cout << "Code ("<<pos.getPosition()<<","<<end.getPosition()<<"): '" << code.toUTF8() << "'\n";
      if (scan==ScanIDs::SCAN_TOKEN)
      {
        scanned++;
        Syntax::SynTok* tok=getSynTok(code);
        if (tok && !SalIDs::isSalBoolType(tok->getType()))  // booleans are values...
        {
          // token is a literal. stop if its the first thing
          // encountered or if its a clausal
          if (last==0 || SalIDs::isSalClausalType(tok->getType()))
          {
            //std::cout<<"literal breaking with last==0"<<code.toUTF8()<<"\n";
            break;
          }
          scan=tok->getType();
          // if its a command then include and stop
          if (SalIDs::isSalCommandType(scan))
          {
            //std::cout << "stopping on command '"<<code.toUTF8()<<"'\n";
            here=pos.getPosition();
            break;
          }
          // otherwise (its a op or comma? ) keep going...
          here=pos.getPosition();
        }
        else 
        {
          // token is a variable or constant.  if its the first
          // expr or if the last was a literal then include it
          // and keep going
          if ((last==0) || SalIDs::isSalType(last))
            here=pos.getPosition();
          // otherwise if its a constant then stop (because last
          // is an expr)
          else if (code.containsOnly("0123456789+-./") ||
                   code.startsWith("#") ||
                   code.startsWith(":") ||
                   code.endsWith(":") )
          {
            //std::cout<<"breaking with constant="<<code.toUTF8()<<"\n";
            break;
          }
          // otherwise its a variable, if last was a () expr its
          // a function call, if last was a [] its a aref
          // otherwise stop
          else if (last==ScanIDs::SCAN_LIST || last==SCAN_SQUARE)
            here=pos.getPosition();
          else
          {
            //std::cout << "token breaking with code='"<<code.toUTF8()<<"'\n";
            break;
          }
        }
      }
      else if (scan==ScanIDs::SCAN_PUNCT)
      {
        scanned++;
        // its a comma, stop if last not an expression
        if (last==0 || SalIDs::isSalType(last))
        {
          //std::cout << "breaking on SCAN_PUNCT=\n";
          break;
        }
        scan=SalIDs::SalComma;
      }
      // otherwise its a {[( or string expression. stop unless this
      // expr its the first or the previous thing was not an expr
      else if (last==0 || SalIDs::isSalType(last))
      {
        scanned++;
        if (pos.getCharacter()=='{')
          scan=SCAN_CURLY;
        else if (pos.getCharacter()=='[')
          scan=SCAN_SQUARE;
        here=pos.getPosition();
      }
      else
      {
        //std::cout << "breaking with scan=" << ScanIDs::scanResultToString(scan).toUTF8() << "\n";
        break;
      }
      if (pos==bob) break;
      // move one below first expr char, skip comments and white
      last=scan;
      pos.moveBy(-1);
      scanCode(document, pos, false, ScanIDs::MoveWhiteAndComments);
    }
  //std::cout << "_______________\n";

  if (scan<0) 
  {
    //std::cout << "returning: " << scan << "\n";
    return scan;
  }
  //std::cout << "after loop, here=" << here << "scan=" << scan << "\n";
  from.setPosition(here);
  //std::cout << "setting from position="<< here << "\n";
  to.setPosition(top.getPosition());
  //std::cout << "setting to position=" << top.getPosition() << "\n";
  
  // stopped without lower bound means no expr encountered
  if (here<0) 
  {
    scan=ScanIDs::SCAN_EMPTY;
    if (scanned>0)
      here=pos.getPosition();
  }
  //std::cout << "returning: "  << ((here>-1) ? 1 : scan) << "\n";
  return (here>-1) ? 1 : scan;
}
