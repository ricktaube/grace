#include <cctype>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <map>
#include <sstream>
#include <stack>
#include <string>
#include <vector>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "../sndlib/sndlib.h"
#include "s7.h"
#include "clm.h"
#include "xen.h"
#include "clm2xen.h"

static s7_pointer main_quit(s7_scheme *sc, s7_pointer args);
bool is_balanced(std::string str);
bool is_not_white(std::string str);

int main(int argc, const char* argv[])
{
  s7_scheme* s7 = s7_init();
  if (!s7)
    return -1;
  // HKT FIXME
  s7_xen_initialize(s7);
  Init_sndlib();

  s7_define_function(s7, "quit", main_quit, 0, 0, false, "(quit) quits the program");
  s7_define_function(s7, "exit", main_quit, 0, 0, false, "(exit) quits the program");
  
  s7_pointer val;
  std::string str;
  try 
  {
    while (std::cin)
    {
      std::cout << "\ns7> ";
      str = "";
      while (true)
      {
        std::string lin;
        std::getline(std::cin, lin);
        str = str + lin + "\n";
        if (is_balanced(str))
          break;
      }
    
      if (is_not_white(str))
      {
        val = s7_eval_c_string(s7, str.c_str());
        std::cout << s7_object_to_c_string(s7, val);
      }
    }
  }
  catch(...)
  {
  }
  std::cout << "Bye!\n";
  free(s7);
  return 0;
}


static s7_pointer main_quit(s7_scheme *sc, s7_pointer args)
{
  throw 0;
  return(s7_nil(sc));
}

bool is_balanced(std::string str)
{
  int parens = 0;
  int quotes = 0;
  unsigned i = 0;
  while (i < str.size())
  {
    if (str[i] == ';')
    {
      for (i = i + 1; i < str.size(); i++)
      {
        if (str[i] == '\n')
          break;
      }
    }
    else if (str[i] == '"')
    {
      if (i == 0 || str[i - 1] != '\\')
      {
        quotes = 1;
        for (i = i + 1; i < str.size(); i++)
        {
          if (str[i] == '"' && str[i - 1] != '\\')
          {
            quotes = 0;
            break;
          }
        }
        if (quotes)
          return false;
      }
    }
    else if (str[i] == '(')
      parens++;
    else if (str[i] == ')')
      parens--;
    i++;
  }
  return (parens == 0) && (quotes == 0);
}

bool is_not_white(std::string str)
{
  for (unsigned i = 0; (i < str.size() && str[i] != ';'); i++)
    if (str[i] != ' ' && str[i] != '\n' && str[i] != '\t')
      return true;
  return false;
}

