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

#include "Libraries.h"
#include "Enumerations.h"
#include "CmSupport.h"
#include "Midi.h"
#include "Csound.h"
#include "SndLibLoad.h"
#include "Console.h"
#include "Syntax.h"
#include "Plot.h"
#include "PlotWindow.h"
#include "Cells.h"
//#ifdef _MSC_VER
//#define strdup _strdup
//#define jn _jn
//double log2(double n) {return log(n)/log(2.0);}
//int round(double n) {return (int)(n+.5);}
//#endif
#ifdef WITH_FOMUS
#include "Fomus.h"
#endif
#include "OpenSoundControl.h"

void cm_quit()
{
  Console::getInstance()->printError(">>> Error: You cannot quit Grace from inside Lisp. Use Console>File>Quit to quit the application.\n");
}

void cm_begin_hook(s7_scheme* sc, bool* result)
{
  *result = SchemeThread::getInstance()->isSchemeInterrupt();
}

void cm_print_output(char* str)
{
  Console::getInstance()->printOutput(str);
}

void cm_print_warning(char* str)
{
  Console::getInstance()->printWarning(str);
}

void cm_print_error(char* str)
{
  Console::getInstance()->printError(str);
}

void cm_print_values(char* str)
{
  Console::getInstance()->printValues(str);
}

void cm_print_stdout(char* str)
{
  std::cout << str;
}

/** record printers **/

char* cm_pattern_print(s7_scheme *sc, void *val)
{
  juce::String str("<pattern #x");
  str << juce::String::toHexString((juce::int64)val) << ">";
  //  return str.toUTF8().getAddress();
  return (char *)strdup(str.toUTF8());  // return copys
}

char* cm_spectrum_print(s7_scheme *sc, void *val)
{
  //  char buf[64];
  //  sprintf(buf,"#<pattern #x%p>", val);
  //  return &buf[0];
  juce::String str("<spectrum #x");
  str << juce::String::toHexString((juce::int64)val) << ">";
  //  return str.toUTF8().getAddress();
  return (char*)strdup(str.toUTF8());
}

void cm_print_markov_table(s7_pointer table, s7_pointer labels, int field, int rowLabelWidth)
{
  s7_scheme* sc = SchemeThread::getInstance()->scheme;
  juce::String sp (" ");
  juce::String ln;
  juce::String port ("\n");
  for (int i = 0; i < field; i++) ln <<"-";
  for (int i = 0; i < rowLabelWidth; i++) port << "*";
  for (s7_pointer p = labels; s7_is_pair(p); p = s7_cdr(p))
  {
    port << sp;
    juce::String s = juce::String((char*)s7_object_to_string(sc, s7_car(p), true));
    int n = s.length();
    // write column pad
    for (int i = 0, m = juce::jmax(field - n, 0); i < m; i++) port << sp; // col separator
    port << s;
  }
  // print each row
  for (s7_pointer tail = table; s7_is_pair(tail); tail = s7_cdr(tail))
  {
    port << "\n";
    juce::String s; // liststring 
    s7_pointer l = s7_car(tail);
    if (l == s7_nil(sc))
      s << "";
    else
    {
    }
  }
}

void cm_prin1(s7_scheme* sc, s7_pointer ptr, bool sal, bool quo, juce::String& str)
{
  if (ptr==s7_f(sc))
  {
    str << "#f";
  }
  else if (ptr==s7_nil(sc))
  {
    if (sal) str << "{}"; else str << "()";
  }
  else if (s7_is_pair(ptr))
  {
    if (sal) str << "{"; else str << "(";
    for (s7_pointer p=ptr; s7_is_pair(p); p=s7_cdr(p))
    {
      if (p!=ptr)
        str<<" ";
      cm_prin1(sc, s7_car(p), sal, true, str);
    }
    if (sal) str << "}"; else str << ")";
  }
  else if (ptr==s7_t(sc))
    str << "#t";
  else if (s7_is_number(ptr))
    str << juce::String(s7_number_to_string(sc, ptr, 10));
  else if (s7_is_string(ptr))
  {
    if (quo)
      str << juce::String(s7_string(ptr)).quoted();
    else
      str << juce::String(s7_string(ptr));
  }
  else
    str << juce::String(s7_object_to_c_string(sc, ptr));
}

s7_pointer cm_print(s7_pointer args)
{
  bool sal=SchemeThread::getInstance()->isSalEval();  // true if we are under a salEval
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  juce::String text=juce::String();
  while (s7_is_pair(args))
  {
    cm_prin1(sc, s7_car(args), sal, false, text);
    args=s7_cdr(args);
  }
  text << "\n";
  Console::getInstance()->printOutput(text);
  return s7_unspecified(sc);
}

int cm_string_hash(char* str)
{
  int temphash = (juce::String(str)).hashCode();
  //  juce::int64 hash = temphash;
  //  std::cout << "temphash is " << temphash << " and hash value is "<< hash << std::endl;
  return temphash;
}

/*=======================================================================*
 *=======================================================================*/

void cm_shell(char* str)
{
  system(str);
}

void cm_play(char* str)
{
  juce::String path (str);
  juce::File file = completeFile(path);

  if (file.hasFileExtension("mid") || file.hasFileExtension("midi"))
  {
    Console::getInstance()->postAsyncMessage(CommandIDs::MidiFilePlayer, path, true);
  }
  else if (file.hasFileExtension("wav") || file.hasFileExtension("aiff") ||
           file.hasFileExtension("aif") || file.hasFileExtension("aifc") ||
           file.hasFileExtension("snd"))
  {
    Console::getInstance()->postAsyncMessage(CommandIDs::AudioFilePlayer, path, true);
  }
}
  
juce::String cm_version()
{
  return SysInfo::getCMVersion();
}

juce::String cm_logo ()
{
  juce::String crlf="\n";
  juce::String vers=cm_version();
  juce::String logo = juce::String();
  logo << " /\\\\\\" << crlf
       << "---\\\\\\---------" << crlf
       << "----\\\\\\--------" << crlf
       << "----/\\\\\\-------" << " " << vers << crlf
       << "---/--\\\\\\------" << crlf
       << "--/----\\\\\\-----" << crlf
       << " /      \\\\\\/";
  return logo;
}

void cm_load(char *path)
{
  juce::String name (path);
  juce::File file = completeFile(name);
  if (file.hasFileExtension(".sal"))
  {
    sal_load(path);
  }
  else
  {
    // allow embedded instrument files to be loaded if (1) the file
    // name has no directory component, (2) *load-path* is null and
    // (3) the file does not exist
    juce::XmlElement* ins = nullptr;
    if (
        // no directory component
        (! name.containsAnyOf(juce::File::getSeparatorString()))
        // file does not exist
        && (! file.existsAsFile())
        // no load path set
        && (s7_load_path(SchemeThread::getInstance()->scheme) == SchemeThread::getInstance()->schemeNil)
        // is embedded instrument
        && (ins = SndLib::getInstance()->getInstrumentElement(name))
        ) 
    {
      juce::String str = SndLib::getInstance()->getInstrumentCode(name, true);
      s7_eval_c_string(SchemeThread::getInstance()->scheme, str.toUTF8());
      ins->setAttribute("Loaded", "yes");
    }
    else
      s7_load(SchemeThread::getInstance()->scheme, path);
  }
}

void cm_edit(char *path)
{
  // REMOVE
}

/*=======================================================================*
 *=======================================================================*/

double cm_log_ten(double num)
{
  if (num>0.0)
    return log10(num);
  else
    return 0.0;
}

double cm_log_two(double num)
{
  if (num>0.0)
    return log2(num);
  else
    return 0.0;
}

// mapping, scaling and offsetting

double cm_rescale(double x, double x1, double x2, double y1, double y2, double b)
{
  if (x >= x2) 
    return y2;
  else if (x <= x1)
    return y1;
  else if (b == 1)
    return (((( y2 - y1) / (x2 - x1)) * (x - x1)) + y1);
  else
  {
    double p = (x - x1) / (x2 - x1);
    return y1 + ( ( (y2 - y1) / (b - 1.0)) * (pow(b, p) - 1.0)); // POWF
  }
}

int cm_discrete(double x, double x1, double x2, int i1, int i2, double b)
{
  // return integers from a to b as n goes from 0.0 to 1.0
  //  return (int)round(cm_rescale(x,x1,x2,(double)i1, (double)i2, b));
  return (int)floor(cm_rescale(x,x1,x2,(double)i1, (double)i2, b));
}

int cm_float_to_fixnum(double f)
{
  return (int)round(f);
}

double cm_decimals(double val, int places)
{
  double p10 = (double)(10 ^ places);
  return round(val*p10) / p10;
}

double cm_quantize(double val, double step)
{
  return floor( (val/step) + .5) * step;
}

double cm_rhythm_to_seconds(double beats, double tempo, double beat)
{
  return (double) ((beats / beat) * (60.0 / tempo)) ;
}

double cm_cents_to_scaler(double cents)
{
  double p = cents/1200.0;
  return pow(2.0,p); //POWF
}

double cm_scaler_to_cents(double scaler)
{
  return (log(scaler)/log(2.0)) * 1200.0; // LOGF
}

double cm_scaler_to_steps(double scaler)
{
  return cm_scaler_to_cents(scaler)/100.0;
}

double cm_expl(double powr, double y0, double y1, double base)
{
  if (powr < 0)
    powr=0.0;
  else if (powr > 1) powr = 1.0;
  if (base == 1.0)
    return y0 + ( powr * (y1 - y0));
  return ( y0 + ( ( (y1 - y0) / (base - 1.0) ) *
		  ( pow(base, powr) - 1.0 )));
}

double cm_explseg( int i, int len, double sum, double powr)
{
  if (i >= len)
    i += -1;
  double x1 = ((double)(i+1)) / ((double)len);
  double x2 = ((double)i) / ((double)len);
  double f1 = cm_expl( x1, 0.0, 1.0, powr);
  double f2 = (i <= 0) ? 0.0 : cm_expl( x2, 0.0, 1.0, powr);
  return ( sum * (f1 - f2) );
}

double cm_geoseg(int i, int len, double sum, double base) 
{
  if (len==0)
    return 0.0;
  double a=sum * ((1.0 - base) / (1.0 - pow(base, len)));
  return  a * pow(base, i);
}

static const double A00 = 6.875;   // keynum 0

double cm_hertz_to_keynum (double hz)
{
  // subtract 3 shifts to A
  return (((log(hz) - log(A00) ) / log(2.0)) * 12.0) - 3; // LOGF
}

double cm_keynum_to_hertz(double kn)
{
  return A00 * pow(2, ((kn + 3) / 12));
}

int cm_keynum_to_pc (double kn)
{
  return ((int)kn) % 12;
}


/* 
// randomness via JUCE Random object
juce::Random ranstate (1000);
void cm_ranseed(int64 s)
{
ranstate.setSeed(s);
}
*/

//
// Randomness class replaces JUCE Random functions with s7's random() function.
//

class Randomness
{
  s7_scheme* scheme;
  s7_Int seed;
  s7_Int carry;
  
public:

  Randomness() : scheme (0), seed (0), carry (0) {};
  ~Randomness() {};

  // initialize random state with current time
  void init(s7_scheme *sc)
  {
    scheme = sc;
    setRandomSeed(juce::Time::getCurrentTime().toMilliseconds());
  }

  // returns a list of the seed and carry values
  s7_pointer getRandomSeed()
  {
    return s7_random_state_to_list(scheme, s7_nil(scheme));
  }

  // initalizes random state to the seed and optional carry
  void setRandomSeed(s7_Int s, s7_Int c=1675393560)
  {
    seed = s;
    carry = c;
    s7_set_default_random_state(scheme, seed, carry);
  }

  void setRandomSeed(s7_pointer arg)
  {
    if (s7_is_integer(arg))
    {
      setRandomSeed(s7_integer(arg));
    }
    else if (s7_is_pair(arg) && s7_list_length(scheme, arg) == 2 &&
             s7_is_integer(s7_car(arg)) && s7_is_integer(s7_car(s7_cdr(arg))))
    {
      setRandomSeed(s7_integer(s7_car(arg)), s7_integer(s7_car(s7_cdr(arg))));
    }
    else
    {
      SchemeThread::getInstance()->signalSchemeError("random seed is not an integer or a list of two integers.");
    }
  }

  s7_Double nextDouble()
  {
    return (s7_Double)s7_random(scheme, 0);
  }

  s7_Int nextInt(s7_Int range)
  {
    return (s7_Int)(s7_random(scheme, 0)*range);
  }
};

static Randomness ranstate;

void cm_init_randomness(s7_scheme* sc)
{
  ranstate.init(sc);
}

s7_pointer cm_get_random_seed()
{
  return ranstate.getRandomSeed();
}

void cm_set_random_seed(s7_pointer args)
{
  ranstate.setRandomSeed(args);
}

double cm_ranfloat(double f)
{
  if (f == 0.0)
    return 0.0;
  if (f == 1.0)
    return ranstate.nextDouble();
  return (ranstate.nextDouble() * f);
}

double cm_ranfloat2(double f1, double f2)
{
  if (f1 == f2)
    return f1;
  if (f1 < f2)
    return (f1 + (ranstate.nextDouble() * (f2-f1)));
  return (double) (f1 - (ranstate.nextDouble() * (f1-f2)));
}

int cm_ranint(int i)
{
  if (i > 0 )
    return (int)ranstate.nextInt(i);
  if (i < 0)
    return (int)(0 - ranstate.nextInt(abs(i)));
  return 0;
}

int cm_ranint2(int i1, int i2)
{
  if (i1 == i2)
    return i1;
  if (i1 < i2)
    return (i1 + (int)ranstate.nextInt(i2-i1));
  return (int)(i1 - ranstate.nextInt(i1-i2));
}

double cm_ranlow()
{
  return juce::jmin( ranstate.nextDouble(), ranstate.nextDouble());
}

double cm_ranhigh()
{
  return juce::jmax( ranstate.nextDouble(), ranstate.nextDouble());
}

double cm_ranmiddle()
{
  return (ranstate.nextDouble() + ranstate.nextDouble()) / 2.0;
}

double cm_ranbeta (double a, double b)
{
  double ra=1.0/a, rb=1.0/b, r1, r2, y1, y2, sum;
  while (true) {
    r1 = ranstate.nextDouble();
    r2 = ranstate.nextDouble();
    y1 = pow(r1,ra); //POWF
    y2 = pow(r2,rb); //POWF
    sum=y1+y2;
    if ( sum <= 1.0) return (y1 / sum);
  }
}

double cm_ranexp (double lambda)
{
  return (- log(1.0 - ranstate.nextDouble())) / lambda; // LOGF
}

double cm_ranexp2 (double lambda)
{
  double ee = (2 * exp(-1.0)); // EXPF
  double u, v;
  while ( true ) {
    u = 1.0 - ranstate.nextDouble();
    v = ee * ranstate.nextDouble();
    if ( v <= (ee * u * log(u)) ) //LOGF
      return (v / u) / lambda;
  }
}

double cm_rangauss (double sigma, double mu)
{
  // sigma=stand dev, mu=mean
  double x, y, r2;
  do
  {
    x = -1 + 2 * ranstate.nextDouble();
    y = -1 + 2 * ranstate.nextDouble();
    r2 = x * x + y * y;
  }
  while (r2 > 1.0 || r2 == 0);
  return (sigma * y * sqrt(-2.0 * log(r2) / r2))+mu; //SQRTF LOGF
}

double cm_rancauchy()
{
  return(tan(juce::double_Pi*(ranstate.nextDouble() - 0.5))); //TANF
}

int cm_ranpoisson (double lambda)
{
  double b = exp( - lambda); // EXPF
  int n = 0;
  double p = 1.0;
  while (true)
  {
    p = p * ranstate.nextDouble();
    n++;
    if ( p < b ) return n;
  }
}

double cm_rangamma (double nu)
{
  double r=1.0;
  int n=(int)round(nu);
  for (int i=0; i<n; i++)
    r = r * (1 - ranstate.nextDouble());
  return - log(r); //LOGF
}

#define POW2 5
#define POWN 32
double pinking[POWN+1];

double one_over_f_aux(int n, double *r, double halfrange)
{
  double sum=0.0;
  for (int i=0; i<POW2; i++) 
  {
    double p = pow(2.0, i); //POWF
    if (! ((n / p) == ((n - 1) / p)))
      r[i]=( (ranstate.nextDouble() * 2 * halfrange) - halfrange) ;
    sum += r[i];
  }
  return sum;
}

double cm_ranpink()
{
  // Based on Gardner (1978) and Dick Moore (1988?)
  static int i=POWN;
  if (i==POWN )
  {
    double r[POW2];
    double h=1.0/POW2;
    for (int n=0; n<POWN; n++)
    {
      pinking[n]=one_over_f_aux(n, r, h);
    }
    i=0;
    //    for (int j=0;j<32;j++)
    //      printf(" %f", pinking[j]);
    //    printf("\n");
  }
  else i++;
  return pinking[i];
}

double cm_ranbrown()
{
  // from http://vellocet.com/dsp/noise/VRand.h
  // but made to generate between -1 1
  
  static double b=0.0;
  while (true) 
  {
    double  r = ranstate.nextDouble()*2-1;
    b += r;
    if (b<-16.0 || b>16.0) b -= r;
    else break;
  }
  // return interval -1 1.
  return (b*0.0625);
}

double cm_bes_jn(int a, double b)
{
  return jn(a,b);
}

//
// scheduler and processes
//

/* Metronome functions. Valid id checking must be performed in Scheme
   before these function are called!  */

bool cm_sched_metro_id_valid(int metroId)
{
  SchemeThread* scm = SchemeThread::getInstance();
  int index = scm->getMetroIndexFromId(metroId);
  return index >= 0;
}

int cm_sched_make_metro(double tempo)
{
  SchemeThread* scm=SchemeThread::getInstance();
  return scm->makeMetro(tempo);
}

void cm_sched_delete_metro(int id)
{
  SchemeThread::getInstance()->deleteMetro(id);
}

s7_pointer cm_sched_get_metros(bool user)
{
  SchemeThread* scm = SchemeThread::getInstance();
  s7_pointer ids=scm->schemeNil;
  int bot=(user ? 1 : 0);  // index to stop at
  // reversed because cons adds elements to the front of a list
  for (int i=scm->metros.size()-1; i>=bot; i--)
    ids=s7_cons(scm->scheme, s7_make_integer(scm->scheme, scm->metros.getUnchecked(i)->identifier), ids);
  return ids;
}

void cm_sched_set_tempo(double tempo, double time, bool issecs, int _id)
{
  SchemeThread* scm = SchemeThread::getInstance();
  int index = scm->getMetroIndexFromId(_id);
  scm->metros[index]->setFutureTempo(tempo, time, issecs);
  scm->updateNodeTimes();
}

double cm_sched_get_metro_beat(int metroId)
{
  SchemeThread* scm = SchemeThread::getInstance();
  int index = scm->getMetroIndexFromId(metroId);
  return scm->metros[index]->getNowBeat();
}

double cm_sched_get_metro_tempo(int metroId)
{
  SchemeThread* scm = SchemeThread::getInstance();
  int index = scm->getMetroIndexFromId(metroId);
  return scm->metros[index]->getNowTempo();
}

bool cm_sched_sync_metros(int metroId, double beatsAhead, int masterMetroId, double tempo, bool isbeats, int mode)
{
  SchemeThread* scm = SchemeThread::getInstance();
  bool changed;
  int metroindex = scm->getMetroIndexFromId(metroId);
  int masterindex = scm->getMetroIndexFromId(masterMetroId);
  changed = scm->syncMetros(metroindex, beatsAhead, masterindex, tempo, isbeats, mode);
  if(changed)
    scm->updateNodeTimes();
  return changed;
}

bool cm_sched_metro_phase(double fitBeats, double beatSpace, int metroId)
{
  SchemeThread* scm = SchemeThread::getInstance();
  int index = scm->getMetroIndexFromId(metroId);
  bool changed;
  changed = scm->metros[index]->phase(fitBeats, beatSpace);
  if(changed)
    scm->updateNodeTimes();
  return changed;
}

double cm_sched_metro_dur(double beats, int metroId)
{
  SchemeThread* scm = SchemeThread::getInstance();
  int index = scm->getMetroIndexFromId(metroId);
  double now = juce::Time::getMillisecondCounterHiRes() / 1000.0;
  return scm->metros[index]->getTimeDeltaBeatsLater(now, beats);
}




double cm_now() 
{
  return juce::Time::getMillisecondCounterHiRes() / 1000.0;
}

void cm_sched_sprout(s7_pointer proc, double time, int id, int metroId, double startBeat)
{
  // if score capture is true AND we are under a process callback then
  // scoretime will be >= 0 else it will be 0
  SchemeThread* scm=SchemeThread::getInstance();
  // if (scm->scoremode)
  //    time=time+scm->scoretime;
  scm->sprout(time, proc, id, metroId, startBeat);
}

bool cm_sched_paused_p()
{
  return SchemeThread::getInstance()->isPaused();
}

void cm_sched_pause() 
{
  SchemeThread::getInstance()->setPaused(true);
}

void cm_sched_continue()
{
  SchemeThread::getInstance()->setPaused(false);
}

void cm_sched_stop(int id)
{
  SchemeThread::getInstance()->stop(id);
}

void cm_sched_stop_all()
{
  SchemeThread::getInstance()->stop(0, true);
}

void cm_sched_hush() 
{
  SchemeThread::getInstance()->stop(-1);
}

bool cm_sched_busy_p()
{
  //return !SchemeThread::getInstance()->isQueueEmpty();
  return SchemeThread::getInstance()->sprouted;
}

void cm_sched_set_score_mode(int val)
{
  // this must only be called if scheduler is empty!
  SchemeThread::getInstance()->setScoreMode(val);
}

bool cm_sched_score_mode_p()
{
  return SchemeThread::getInstance()->isScoreMode();
}

int cm_sched_get_score_mode()
{
  return SchemeThread::getInstance()->scoremode;
}

double cm_sched_score_time()
{
  // should only be called under score mode
  return SchemeThread::getInstance()->getScoreTime();
}

//
// file system and pathname support. CONVERT THESE TO STRINGS or MOve to Lisp
//

juce::File completeFile(juce::String path)
{
  if (juce::File::isAbsolutePath(path))
    return juce::File(path);
  else
    return juce::File::getCurrentWorkingDirectory().getChildFile(path).getFullPathName();
}
char* cm_user_home_directory()
{
  juce::File f=juce::File::getSpecialLocation(juce::File::userHomeDirectory);
  juce::String s=f.getFullPathName();
#ifdef WINDOWS
  s=s.replaceCharacter('\\','/');
#endif
  if (! s.endsWithChar('/'))
    s << "/"; 
  return (char *)strdup(s.toUTF8());  // return copy
}

char* cm_temp_directory()
{
  juce::File f=juce::File::getSpecialLocation(juce::File::tempDirectory);
  juce::String s=f.getFullPathName();
#ifdef WINDOWS
  s=s.replaceCharacter('\\','/');
#endif
  if (! s.endsWithChar('/'))
    s << "/";
  return (char *)strdup(s.toUTF8());
}

char* cm_current_directory() 
{
  juce::String s=juce::File::getCurrentWorkingDirectory().getFullPathName();
  return (char *)strdup(s.toUTF8());
}

bool cm_set_current_directory(char *path) 
{
  juce::File dir=completeFile(path);
  if (! dir.isDirectory())
    return false;
  dir.setAsCurrentWorkingDirectory();
  return true;
}

char* cm_pathname_name(char* path)
{
  juce::String string (path);
#ifdef WINDOWS
  string=string.replaceCharacter('\\','/');
#endif
  // start of name is after the last '/' else 0
  int start=string.lastIndexOfChar('/') + 1;
  // end of name is before the last '.' else length
  int end=string.lastIndexOfChar('.');
  // name can start with dot (position 0)
  if (end<=0) end=string.length();
  if (start<end)
    return (char *)strdup(string.substring(start,end).toUTF8());
  else
    return (char *)NULL;
}

char* cm_pathname_type(char* path)
{
  juce::String string (path);
#ifdef WINDOWS
  string=string.replaceCharacter('\\','/');
#endif
  // file type after the last '.' unless dot belongs to the name
  int start=string.lastIndexOfChar('.');
  if ((start<=0) || string[start-1]=='/')
    return (char *)NULL;
  return (char *)strdup(string.substring(start+1).toUTF8());
}

char* cm_pathname_directory(char* path)
{
  juce::String string=juce::String(path);
#ifdef WINDOWS
  string=string.replaceCharacter('\\','/');
#endif
  int end=string.lastIndexOfChar('/');
  if (end<0)
    return (char *)NULL;
  else 
    return (char *)strdup(string.substring(0,end+1).toUTF8());
}

char* cm_full_pathname(char* path)
{
  juce::String string=completeFile(juce::String(path)).getFullPathName();
#ifdef WINDOWS
  string=string.replaceCharacter('\\','/');
#endif
  return (char *)strdup(string.toUTF8());
}

bool cm_pathname_exists_p(char* path)
{
  juce::File f=completeFile(juce::String(path));
  return f.exists();
}

bool cm_pathname_writable_p(char* path)
{
  juce::File f=completeFile(juce::String(path));
  return f.hasWriteAccess();
}

bool cm_pathname_directory_p(char* path)
{
  juce::File f=completeFile(juce::String(path));
  return f.isDirectory();
}

s7_pointer cm_directory(char* path, bool recurse)
{
  juce::File f=completeFile(juce::String(path));
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  if (f.existsAsFile())
    return s7_cons(sc, s7_make_string(sc, f.getFullPathName().toUTF8()),
                   SchemeThread::getInstance()->schemeNil);
  juce::String s;
  if (f.isDirectory())
    s="*";
  else if (f.existsAsFile())
    return s7_cons(sc, s7_make_string(sc,f.getFullPathName().toUTF8()),
                   SchemeThread::getInstance()->schemeNil);                     
  else if (juce::String(path).containsChar('*'))
  {
    s=f.getFileName();
    f=f.getParentDirectory();
  }
  else
    return SchemeThread::getInstance()->schemeNil;

  juce::Array<juce::File> a;
  int i=(recurse) ? juce::File::findFiles : juce::File::findFilesAndDirectories;
  int n=f.findChildFiles(a, (i | juce::File::ignoreHiddenFiles), recurse, s);
  if (n==0)
    return SchemeThread::getInstance()->schemeNil;
  s7_pointer head=s7_cons(sc, s7_make_string(sc, a[0].getFullPathName().toUTF8()),
                          SchemeThread::getInstance()->schemeNil);
  s7_pointer tail=head;
  for (int j=1; j<n; j++)
  {
    juce::String x=a[j].getFullPathName();
    s7_set_cdr(tail, s7_cons(sc, s7_make_string(sc, x.toUTF8()),
                             SchemeThread::getInstance()->schemeNil));
    tail=s7_cdr(tail);
  }  
  return head;
}

int cm_pathname_to_key(char* path)
{
  // parse filename for a note or key number at the end. returns
  // keynum or -1

  juce::String name=juce::String(path);
  // ignore file extension if it exists
  int len=name.lastIndexOfChar('.');
  if (len<0)
    len=name.length();
  juce::String letts="AaBbCcDdEeFfGg#Ss";
  juce::String digis="0123456789";
  int pos;
  bool let=false;
  // parse backwards as long as constituent is keynum or note name
  for (pos=len-1; pos>=0; pos--)
  {
    if (letts.containsChar(name[pos]))
      let=true; // have a note name letter
    else if (digis.containsChar(name[pos]))
    { // digit not constituent if note name reached
      if (let)
        break;
    }
    else
      break;
  }
  // pos now at end or start of possible note/keynum
  pos++;
  // set name to substring of possible note portion
  if (pos<len)
    name=name.substring(pos,len);
  else
    return -1;
  // name can be just a key number
  if (name.containsOnly(digis))
    return name.getIntValue();
  // otherwise a note must have a least a pitch class and octave
  if (name.length()<2)
    return -1;
  pos=0;
  int pc=0;
  // parse out required case-insensitive note name
  switch (name[pos])
  {
  case 'c': case 'C': pc=0;  break;
  case 'd': case 'D': pc=2;  break;
  case 'e': case 'E': pc=4;  break;
  case 'f': case 'F': pc=5;  break;
  case 'g': case 'G': pc=7;  break;
  case 'a': case 'A': pc=9;  break;
  case 'b': case 'B': pc=11; break;
  default: return -1;
  }
  pos++;
  // parse out optional case-insensitive accidental
  switch (name[pos])
  {
  case '#': case 's': case 'S': pc++; pos++; break;      
  case 'b': case 'f': case 'F': pc--; pos++; break;
  }
  // remainder of name must be octave token
  name=name.substring(pos,len);
  if (name=="00")
    return pc;
  else if (name=="0")
    return pc + 12;
  else if (name=="1")
    return pc + 24;
  else if (name=="2")
    return pc + 36;
  else if (name=="3")
    return pc + 48;
  else if (name=="4")
    return pc + 60;
  else if (name=="5")
    return pc + 72;
  else if (name=="6")
    return pc + 84;
  else if (name=="7")
    return pc + 96;
  else if (name=="8")
    return pc + 108;
  else if (name=="9" && (pc < 8))
    return pc + 120;
  else
    return -1;
}

// return the next file version that does not name an existing file

int cm_insure_new_file_version(juce::String pathname, int vers)
{
  juce::File file=completeFile(pathname);
  while (true)
  {
    juce::File test = file.getSiblingFile(file.getFileNameWithoutExtension() +
                                          "-" + 
                                          juce::String(vers) + 
                                          file.getFileExtension());
    if (!test.existsAsFile())
      break;
    vers++;
  }
  return vers;
}

/*=======================================================================*
  SAL Support
  *=======================================================================*/

s7_pointer sal_allocate_tokens()
{
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  juce::OwnedArray<Syntax::SynTok>* ary=new juce::OwnedArray<Syntax::SynTok>;
  return s7_make_c_pointer(sc, ary);
}

s7_pointer sal_free_tokens(s7_pointer ptr)
{
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  juce::OwnedArray<Syntax::SynTok>* ary=(juce::OwnedArray<Syntax::SynTok>*)s7_c_pointer(ptr);
  delete ary;
  return s7_unspecified(sc);
}

s7_pointer sal_tokenize_string(s7_pointer str, s7_pointer ptr,  s7_pointer sal2)
{
  // str is the lisp string 
  juce::String text (s7_string(str));
  // ptr is a C-pointer to an  juce::OwnedArray<Syntax::SynTok>
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  juce::OwnedArray<Syntax::SynTok>* ary=(juce::OwnedArray<Syntax::SynTok>*)s7_c_pointer(ptr);
  // convert array to lisp list of C tokens  
  s7_pointer toks=SchemeThread::getInstance()->schemeNil;
  bool ok=false;

  //  //  if (s7_boolean(sc, sal2))
  //  //    {
  //  //    ok=Sal2Syntax::getInstance()->tokenize(text, *ary);
  //  //    }
  //  //  else
  //  //    ok=SalSyntax::getInstance()->tokenize(text, *ary);

  if (ok)
  {
    for (int i=ary->size()-1; i>=0; i--)
      toks=s7_cons(sc, s7_make_c_pointer(sc, ary->getUnchecked(i)), toks);
  }
 
  return toks;
}

s7_pointer sal_tokenize_file(s7_pointer fil, s7_pointer ptr, s7_pointer sal2)
{
  // fil is the lisp string holding a valid pathname
  // ptr is the C-pointer to an allocated juce::OwnedArray<Syntax::SynTok>
  juce::File file (juce::String(s7_string(fil)));  // read sal file 
  juce::String text="begin\n"; // wrap text in begin...end
  text << file.loadFileAsString()
       << "\nend";
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  juce::OwnedArray<Syntax::SynTok>* ary=(juce::OwnedArray<Syntax::SynTok>*)s7_c_pointer(ptr);
  // convert array to lisp list of C tokens  
  s7_pointer toks=SchemeThread::getInstance()->schemeNil;
  bool ok=false;

  //  //  if (s7_boolean(sc, sal2))
  //  //    ok=Sal2Syntax::getInstance()->tokenize(text, *ary);
  //  //  else
  //  //    ok=SalSyntax::getInstance()->tokenize(text, *ary);

  if (ok)
  {
    for (int i=ary->size()-1; i>=0; i--)
      toks=s7_cons(sc, s7_make_c_pointer(sc, ary->getUnchecked(i)), toks);
    // sigh. return input string as first arg
    toks=s7_cons(sc, s7_make_string(sc, text.toUTF8()), toks);
  }
  return toks;
}

s7_pointer sal_load(char* path)
{
  // sal_load is always called under a scheme function and the file
  // type is guaranteed to be either .sal or .sal2
  juce::String name (path);
  juce::File file=completeFile(name);
  if (! file.existsAsFile())
  {
    SchemeThread::getInstance()->signalSchemeError("load: file does not exist: " + file.getFullPathName());
  }
  juce::String text=file.loadFileAsString();
  if (text.isEmpty())
  {
    SchemeThread::getInstance()->signalSchemeError("load: file is empty: " + file.getFullPathName());
    //return SchemeThread::getInstance()->schemeFalse;
  }
  int type=TextIDs::fromFileType(file.getFileExtension());
  if (type==TextIDs::Sal1)
  {
    // for a .sal file we have to check if custom comment has syntax
    juce::String firstline=text.upToFirstOccurrenceOf("\n", false, false);
    if (firstline[0]==';' && firstline.contains("-*-"))
    {
      firstline=firstline.fromFirstOccurrenceOf("syntax:", false, true).trimStart();
      if (firstline.substring(0,4).equalsIgnoreCase("sal2"))
        type=TextIDs::Sal2;
    }
  }
  sal_eval(text, type);
  return SchemeThread::getInstance()->schemeVoid;
}

s7_pointer sal_eval(juce::String text, int type)
{
  juce::OwnedArray<Syntax::SynTok> tokens;
  s7_scheme* sc=SchemeThread::getInstance()->scheme;
  s7_pointer func;
  if (type==TextIDs::Sal1)
  {
    std::cout << "load type=sal\n";
    if (!SalSyntax::getInstance()->tokenize(text, tokens))
    {
      tokens.clear();
      SchemeThread::getInstance()->signalSchemeError(SalSyntax::getInstance()->getLastParseError());
    }
    func=s7_name_to_value(sc, "sal");
  }
  else
  {
    std::cout << "load type=sal2\n";
    if (!Sal2Syntax::getInstance()->tokenize(text, tokens))
    {
      tokens.clear();
      SchemeThread::getInstance()->signalSchemeError(Sal2Syntax::getInstance()->getLastParseError());
    }
    func=s7_name_to_value(sc, "sal2");
  }
  // convert tokens to list
  s7_pointer toks=SchemeThread::getInstance()->schemeNil;
  for (int i=tokens.size()-1; i>=0; i--)
    toks=s7_cons(sc, s7_make_c_pointer(sc, tokens.getUnchecked(i)), toks);
  // ("string", (tokens...), false, true)
  s7_pointer args=s7_cons(sc, 
                          s7_make_string(sc, text.toUTF8()),
                          s7_cons(sc,
                                  toks,
                                  s7_cons(sc,
                                          SchemeThread::getInstance()->schemeFalse,
                                          s7_cons(sc,
                                                  SchemeThread::getInstance()->schemeTrue,
                                                  SchemeThread::getInstance()->schemeNil))));
  return s7_call(sc, func, args);
}


s7_pointer sal_token_type(s7_pointer ptr)
{
  Syntax::SynTok* tok=(Syntax::SynTok*)s7_c_pointer(ptr);
  return s7_make_integer(SchemeThread::getInstance()->scheme, tok->getType());
}

s7_pointer sal_token_string(s7_pointer ptr)
{
  Syntax::SynTok* tok=(Syntax::SynTok*)s7_c_pointer(ptr);
  return s7_make_string(SchemeThread::getInstance()->scheme, tok->getName().toUTF8());
}

s7_pointer sal_token_position(s7_pointer ptr)
{
  Syntax::SynTok* tok=(Syntax::SynTok*)s7_c_pointer(ptr);
  return s7_make_integer(SchemeThread::getInstance()->scheme, tok->getData1());
}

// mouse

double cm_mouse_x(double minval, double maxval, double base)
{
  juce::Rectangle<int> monitor = juce::Desktop::getInstance().getDisplays().getTotalBounds(false);
  juce::Point<int> mouse = juce::Desktop::getMousePosition();
  return cm_rescale(mouse.getX(), 0, monitor.getWidth(), minval, maxval, base);
}

double cm_mouse_y(double minval, double maxval, double base)
{
  juce::Rectangle<int> monitor=juce::Desktop::getInstance().getDisplays().getTotalBounds(false);  	
  juce::Point<int> mouse=juce::Desktop::getMousePosition();
  return cm_rescale(mouse.getY(), 0, monitor.getHeight(), minval, maxval, base);
}

s7_pointer cm_mouse_button(s7_pointer upval, s7_pointer downval)
{
  if (juce::Desktop::getInstance().getMainMouseSource().isDragging())
    return downval;
  return upval;
}

/*
 * Midi port
 */

bool mp_open_output(int dev)
{
  return MidiOutPort::getInstance()->open(dev);
}

bool mp_open_input(int dev)
{
  return MidiInPort::getInstance()->open(dev);
}

void mp_close_output(int dev)
{
  MidiOutPort::getInstance()->close(dev);
}

void mp_close_input(int dev)
{
  MidiInPort::getInstance()->close(dev);
}

void mp_open_score(char* path, s7_pointer args)
{
  // args is () or a list of keyword options
  MidiOutPort::getInstance()->performCommand(CommandIDs::MidiOutSetFile, 0, juce::String(path));
}

void mp_close_score()
{
  MidiOutPort::getInstance()->performCommand(CommandIDs::SchedulerScoreComplete);
}

void mp_send_note(s7_pointer time, s7_pointer dur, s7_pointer key, s7_pointer amp, s7_pointer chan) 
{
  SchemeThread* scm=SchemeThread::getInstance();
  // if a Fomus score is open reroute the midi data as an fms:note.
#ifdef WITH_FOMUS
  if (scm->scoremode == ScoreTypes::Fomus)
  {
    if (!check_fomus_exists()) return;
    Fomus* fms = Fomus::getInstance();
    //    std::cout << "fms:note ";
    // keynum
    if (s7_is_real(key))
    {
      fms->fval(fomus_par_pitch, fomus_act_set, s7_number_to_real(scm->scheme,key));
      //      std::cout << "pitch=" << s7_number_to_real(key);
    }
    else
      scm->signalSchemeError("mp:midi: key not a real number");
    // amp
    if (s7_is_real(amp))
    {
      double a = s7_number_to_real(scm->scheme, amp);
      fms->fval(fomus_par_dynlevel, fomus_act_set, ((a >= 1.0) ? (a / 127.0) : a));
      //      std::cout << ", amp=" << a;
    }
    else if (amp == s7_f(scm->scheme))
    {
      fms->fval(fomus_par_dynlevel, fomus_act_set, 0.0);
      //      std::cout << ", amp=0.0";
    }
    else
      scm->signalSchemeError("mp:midi: amp not a real number");
    // time
    if (s7_is_ratio(time))
    {
      fms->rval(fomus_par_time, fomus_act_set, s7_numerator(time), s7_denominator(time));
      //      std::cout << ", time=" << s7_numerator(time) << "/" << s7_denominator(time);
    }
    else if (s7_is_real(time))
    {
      fms->fval(fomus_par_time, fomus_act_set, s7_number_to_real(scm->scheme, time));
      //      std::cout << ", time=" << s7_number_to_real(time);
    }
    else if (time == s7_f(scm->scheme))
    {
      fms->act(fomus_par_time, fomus_act_n);      
      //      std::cout << ", time=#f";
    }
    else
      scm->signalSchemeError("mp:midi: time not a number");
    // dur
    if (s7_is_real(dur))
    {
      fms->fval(fomus_par_duration, fomus_act_set, s7_number_to_real(scm->scheme, dur));
      //      std::cout << ", dur=" << s7_number_to_real(dur);
    }
    else
      scm->signalSchemeError("mp:midi: dur not a real number");
    // chan
    if (s7_is_integer(chan))
    {
      fms->ival(fomus_par_part, fomus_act_set, s7_integer(chan));
      //      std::cout << ", part=" << s7_integer(chan);
    }
    else if (chan != s7_f(scm->scheme))
      scm->signalSchemeError("mp:midi: chan not an int");
    fms->act(fomus_par_noteevent,  fomus_act_add);
    //    std::cout << "\n";
  }
  else
#endif   
    // otherwise send to midiport or midifile 
  {
    bool midifile=(scm->scoremode == ScoreTypes::Midi);
    double f0 = 0.0, f1 = 0.5, f2 = 60.0, f3 = 0.5, f4 = 0.0;
    // time is #f or number
    if (time == s7_f(scm->scheme))
      f0 = 0.0;
    else if (s7_is_number(time))
      f0 = s7_number_to_real(scm->scheme, time);
    else
      scm->signalSchemeError("mp:midi: time not a number");
    // dur is #f or number
    if (dur == s7_f(scm->scheme))
      f1 = 0.5;
    else if (s7_is_number(dur))
      f1 = s7_number_to_real(scm->scheme, dur);
    else
      scm->signalSchemeError("mp:midi: dur not a number");
    // key is #f or number
    if (key == s7_f(scm->scheme))
      f2 = 60.0;
    else if (s7_is_number(key))
      f2=s7_number_to_real(scm->scheme, key);
    else
      scm->signalSchemeError("mp:midi: key not a number");
    // amp is #f or number
    if (amp == s7_f(scm->scheme))
      f3 = 0.5;
    else if (s7_is_number(amp))
      f3 = s7_number_to_real(scm->scheme, amp);
    else
      scm->signalSchemeError("mp:midi: amp not a number");
    // chan is #f or number
    if (chan == s7_f(scm->scheme))
      f4 = 0.0;
    else if (s7_is_number(chan))
      f4 = s7_number_to_real(scm->scheme, chan);
    else
      scm->signalSchemeError("mp:midi: chan not a number");
    // if score capture is true AND we are under a process callback
    // then scoretime will be >= 0 else it will be 0
    if (midifile)
      f0 += scm->scoretime;
    MidiOutPort::getInstance()->sendNote(f0, f1, f2, f3, f4, midifile);
  }
}

void mp_send_data(int type, double time, double chan, double data1, double  data2)
{
  SchemeThread* scm=SchemeThread::getInstance();
  if (scm->scoremode)
  {
    // if score capture is true AND we are under a process callback then
    // scoretime will be >= 0 else it will be 0
    time=time+scm->scoretime;
    MidiOutPort::getInstance()->
      sendData(type, time, chan, data1, data2, true) ;
  }
  else
  {
    MidiOutPort::getInstance()->
      sendData(type, time, chan, data1, data2, false) ;
  }
}

void mp_set_tuning(int div)
{
  MidiOutPort::getInstance()->performCommand(CommandIDs::MidiOutTuning,
					     div);
}

void mp_set_instruments(s7_pointer list)
{
  MidiOutPort* port=MidiOutPort::getInstance();
  SchemeThread* scm=SchemeThread::getInstance();
  for (int chan=0; s7_is_pair(list) && chan<16; list=s7_cdr(list), chan++)
    if (s7_is_integer(s7_car(list)))
      port->setInstrument(chan, (int)s7_integer(s7_car(list)) & 0x7f);      
    else if (s7_car(list) == scm->schemeFalse)
      ;
    else if (s7_is_string(s7_car(list)) || s7_is_symbol(s7_car(list)))
    {
      juce::String name=(s7_is_string(s7_car(list))) ? juce::String(s7_string(s7_car(list))) : juce::String(s7_symbol_name(s7_car(list)));
      int prog=-1;
      for (int i=0; i<128 && prog==-1; i++)
        if (port->instrumentNames[i].containsIgnoreCase(name))
          prog=i;
      if (prog==-1)
        scm->signalSchemeError("mp:instruments: not an instrument name: " + name);
      else 
        port->setInstrument(chan, prog);      
    }
    else
      scm->signalSchemeError("mp:instruments: not a valid program change: " + 
                             juce::String(s7_object_to_c_string(scm->scheme, s7_car(list))));
  port->sendInstruments();
}


void mp_set_channel_mask(int mask)
{
  MidiInPort::getInstance()->setInputChannelMask(mask);
}

void mp_set_message_mask(int mask)
{
  MidiInPort::getInstance()->setOpcodeMask(mask);
}


s7_pointer getMidiValue(s7_scheme* sc, juce::MidiMessageSequence::MidiEventHolder* hold, int midival, int opcode, 
                        double lasttime, double lastontime)
{
  switch (midival)
  {
  case MidiValues::MidiValueTime:
    return s7_make_real(sc, hold->message.getTimeStamp());
  case MidiValues::MidiValueDelta:
    return s7_make_real(sc, hold->message.getTimeStamp() - lasttime);
  case MidiValues::MidiValueOp:
    return s7_make_integer(sc, opcode);
  case MidiValues::MidiValueRhythm:
    return s7_make_real(sc, hold->message.getTimeStamp() - lastontime);
  case MidiValues::MidiValueDuration:
    if (hold->noteOffObject)
      return s7_make_real(sc, hold->noteOffObject->message.getTimeStamp() - hold->message.getTimeStamp());
    else // no dur if no paired off
      return SchemeThread::getInstance()->schemeFalse;
  case MidiValues::MidiValueKeyNumber:
    return s7_make_integer(sc, hold->message.getNoteNumber());
  case MidiValues::MidiValueAmplitude:
    return s7_make_real(sc, hold->message.getFloatVelocity());
  case MidiValues::MidiValueVelocity:
    return s7_make_integer(sc, hold->message.getVelocity());
  case MidiValues::MidiValueChannel:
    return s7_make_integer(sc, hold->message.getChannel()-1);
  case MidiValues::MidiValueTouch:
    return s7_make_integer(sc, hold->message.getAfterTouchValue());
  case MidiValues::MidiValueControlNumber:
    return s7_make_integer(sc, hold->message.getControllerNumber());
  case MidiValues::MidiValueControlValue:
    return s7_make_integer(sc, hold->message.getControllerValue());
  case MidiValues::MidiValueProgram:
    return s7_make_integer(sc, hold->message.getProgramChangeNumber());
  case MidiValues::MidiValuePressure:
    return s7_make_integer(sc, hold->message.getChannelPressureValue());
  case MidiValues::MidiValueBend:
    return s7_make_integer(sc, hold->message.getPitchWheelValue());
  case MidiValues::MidiValueSeqNum:
    {
      int num = -1;
      const juce::uint8* data=hold->message.getMetaEventData();
      if (hold->message.getMetaEventLength()==2)
        num=(int)((data[0]<<8)+data[1]);
      return s7_make_integer(sc, hold->message.getPitchWheelValue());
    }
  case MidiValues::MidiValueText:
    return s7_make_string(sc, hold->message.getTextFromTextMetaEvent().toUTF8());
  case MidiValues::MidiValueChanPrefix:
    return s7_make_integer(sc, hold->message.getMidiChannelMetaEventChannel());
  case MidiValues::MidiValueTempo:
    return s7_make_real(sc, hold->message.getTempoSecondsPerQuarterNote());
  case MidiValues::MidiValueTimeSig:
    {
      int num, den;
      hold->message.getTimeSignatureInfo(num, den);
      return s7_cons(sc, s7_make_integer(sc, num),
                     s7_cons(sc, s7_make_integer(sc, den),
                             SchemeThread::getInstance()->schemeNil));
    }
  case MidiValues::MidiValueKeySig:
    return s7_make_integer(sc, hold->message.getKeySignatureNumberOfSharpsOrFlats());
  default:
    return SchemeThread::getInstance()->schemeFalse;
  }
}

s7_pointer cm_midifile_import(char* path, int track, s7_pointer midivalues)
{
  s7_scheme* scheme=SchemeThread::getInstance()->scheme;
  juce::String name (path);
  juce::File file=completeFile(name);
  if (!file.existsAsFile())
    SchemeThread::getInstance()->signalSchemeError("midifile-import: file does not exist: " + 
                                                   file.getFullPathName());
  juce::FileInputStream input (file);
  if (input.failedToOpen())
    SchemeThread::getInstance()->signalSchemeError("midifile-import: file failed to open: " +
                                                   file.getFullPathName());
  juce::MidiFile midifile;
  midifile.readFrom(input);
  int numtracks=midifile.getNumTracks();
  if (numtracks<1) 
    SchemeThread::getInstance()->signalSchemeError("midifile-import: not a valid midifile: " + 
                                                   file.getFullPathName());
  if (track<0 || track>=numtracks)
    SchemeThread::getInstance()->signalSchemeError("midifile-import: not a valid track: " + 
                                                   juce::String(track));
  int format=midifile.getTimeFormat();
  // get the track's midi data
  juce::MidiMessageSequence* seq=(juce::MidiMessageSequence*)midifile.getTrack(track);
  seq->updateMatchedPairs();
  // juce time format is postive for beats and negative for SMTPE
  if (format>0)
    midifile.convertTimestampTicksToSeconds();    
  else
  {
    // convert SMPTE ticks to seconds. The juce function
    // convertTimestampTicksToSeconds doesnt seem to work for SMPTE
    // ticks.
    int tpf=format & 0xFF;
    // juce smpte frames per second is negative upper byte
    int fps=0xFF-((format & 0xFF00) >> 8)+1;
    double smptetps=fps*tpf; // smpte ticks per second
    for (int i=0; i<seq->getNumEvents(); i++)
    {
      juce::MidiMessageSequence::MidiEventHolder* h=seq->getEventPointer(i);
      h->message.setTimeStamp(h->message.getTimeStamp()/smptetps);
    }
  }

  // format of values list: 1=single value, 2=multiple values, 3=list
  // of lists eg: ("op"), ("op" "key") (("op" "key") ("op" "bend"))
  format=1;
  if (s7_is_pair(s7_car(midivalues)))
    format=3;
  else if (s7_list_length(scheme, midivalues)>1)
    format=2;
  // transfer midi values to array
  juce::Array<int> vals;
  // if not format 3 then load the array just one time
  if (format<3)
  {
    for (s7_pointer p=midivalues; s7_is_pair(p); p=s7_cdr(p))
      vals.add((int)s7_integer(s7_car(p)));
  }
  // initialize return list for rplacd value appending
  s7_pointer empty=SchemeThread::getInstance()->schemeNil;
  s7_pointer head=s7_cons(scheme, SchemeThread::getInstance()->schemeFalse, empty);
  s7_pointer tail=head;  // tail now: (#f)
  double lasttime=0.0;
  double lastontime=0.0;
  // map all events in track
  for (int i=0; i<seq->getNumEvents(); i++)  
  {
    juce::MidiMessageSequence::MidiEventHolder* e=seq->getEventPointer(i);
    int op=-1;
    // only handle channel or meta messages. get the midi opcode of
    // the event. for channel events opcode is in the upper nibble
    // of the status byte
    if (e->message.getChannel()>0)
      op=(e->message.getRawData()[0] & 0xF0)>>4;
    else if (e->message.isMetaEvent())
      op=e->message.getMetaEventType();
    else 
      continue;

    s7_pointer top=midivalues; // top only used by format 3
    // if format 3 set pointer to start of midivals and load the
    // vals array with the first sublist
    if (format==3)
    {
      vals.clear();
      // first values list is car of top
      for (s7_pointer p=s7_car(top); s7_is_pair(p); p=s7_cdr(p))
        vals.add((int)s7_integer(s7_car(p)));
      top=s7_cdr(top); // increment top
    }
    // process all the midivalues for the current message. if format
    // is 1 or 2 then this loop happens only one time
    while (true)
    {
      // check that all values are applicable to the current message.
      bool ok=true;
      for (int j=0; j<vals.size() && ok; j++) //((j<vals.size()) && (op > -1))
        if (!MidiValues::isOpForValue(vals[j], op))
          ok=false;
      // if all the values match then process them and add to return list
      if (ok)
      {
        //std::cout << "event [" << i << "] op=" << op << " values are applicable\n";
        s7_pointer add=NULL;
        if (format==1)
          add=getMidiValue(scheme, e, vals[0], op, lasttime, lastontime);
        else
        {
          // process vals in reverse order to cons up adds
          add=empty;
          for (int j=vals.size(); --j>=0; )
            add=s7_cons(scheme, getMidiValue(scheme, e, vals[j], op, lasttime, lastontime), add);
        }
        // add the new value (or list of values) onto tail.
        s7_set_cdr(tail, s7_cons(scheme, add, empty));
        tail=s7_cdr(tail);
      }
      // if just single value list or no more lists then quit loop
      // and move to next message
      if (format<3 || top == empty)
        break;
      // else format is 3 and more lists to process, increment to next values list
      vals.clear();
      // next values is car of top
      for (s7_pointer p=s7_car(top); s7_is_pair(p); p=s7_cdr(p))
        vals.add((int)s7_integer(s7_car(p)));
      top=s7_cdr(top);  // increment top 
    }

    lasttime=e->message.getTimeStamp();
    if (op==0x9)
      lastontime=lasttime;
  }
  return s7_cdr(head);
}

s7_pointer cm_midifile_header(char* path, s7_pointer midivalues)
{
  s7_scheme* scheme=SchemeThread::getInstance()->scheme;
  juce::String name (path);
  juce::File file=completeFile(name);
  if (!file.existsAsFile())
    SchemeThread::getInstance()->signalSchemeError("File does not exist: " + file.getFullPathName());
  juce::FileInputStream input (file);
  if (input.failedToOpen()) {
    SchemeThread::getInstance()->signalSchemeError("File does failed to open: " + file.getFullPathName());
  }
  juce::MidiFile midifile;
  if (!midifile.readFrom(input))
    SchemeThread::getInstance()->signalSchemeError("Invalid midi file: " + file.getFullPathName());    
  int num=midifile.getNumTracks();
  if (num<1) 
    SchemeThread::getInstance()->signalSchemeError("Invalid midi file: " + file.getFullPathName());
  // add num tracks
  s7_pointer head=s7_cons(scheme, s7_make_integer(scheme, num), s7_nil(scheme));
  s7_pointer data=head;
  // add time format (ticks/quarter or SMPTE)
  num=midifile.getTimeFormat();
  s7_set_cdr(data, s7_cons(scheme, s7_make_integer(scheme, num), s7_nil(scheme)));
  data=s7_cdr(data);
  // add tempo or #f
  juce::MidiMessageSequence seq;
  midifile.convertTimestampTicksToSeconds();
  midifile.findAllTempoEvents(seq);
  if (seq.getNumEvents()>0)
    s7_set_cdr(data, s7_cons(scheme, s7_make_real(scheme, seq.getEventPointer(0)->message.getTempoSecondsPerQuarterNote()), s7_nil(scheme)));
  else
    s7_set_cdr(data, s7_cons(scheme, s7_f(scheme), s7_nil(scheme)));
  data=s7_cdr(data);
  // add max time in file
  s7_set_cdr(data, s7_cons(scheme, s7_make_real(scheme, midifile.getLastTimestamp()), s7_nil(scheme)));
  data=s7_cdr(data);
  return head;
}

//
/// Midi Hooks
//

bool mp_set_midi_hook(int op, s7_pointer proc)
{
  SchemeThread* st=SchemeThread::getInstance();
  if (s7_is_procedure(proc))
  {
    st->clearMidiHook(op);
    st->addMidiHook(op,proc);
    return true;
  }  
  else if (st->clearMidiHook(op))
    return true;
  else
    return false;
}

s7_pointer mp_is_midi_hook(int op)
{
  // 0=default, nnn=op, -1=any
  SchemeThread* st=SchemeThread::getInstance();
  if (op<0) // -1==return a list of all receivers
  {
    // cons up result in reverse order  
    s7_pointer args=st->schemeNil;
    for (int i=MidiOps::Bend; i>=MidiOps::Off; i--)
      if (st->isMidiHook(i))
        args=s7_cons(st->scheme, s7_make_integer(st->scheme, i), args);
    if (st->isMidiHook(0))
      args=s7_cons(st->scheme, s7_make_integer(st->scheme, 0), args);
    return args;
  }
  else
    return (st->isMidiHook(op)) ? s7_make_integer(st->scheme, op) : st->schemeFalse;
}

// Csound support

void cs_open_score(char* args)
{
  //  std::cout << "cm_init_score " << args << "\n";
  Csound::getInstance()->initScore(juce::String(args));
}

void cs_send_score(int typ, int num, double time, char* pars)
{
  //std::cout << "cs_send_score " << typ << " " << num << " "  << time << " " << pars << "\n";
  Csound::getInstance()->addToScore(typ,num,time,juce::String(pars));
}

void cs_close_score()
{
  Csound::getInstance()->saveScore();
}

//
// FOMUS Support
//

#ifdef WITH_FOMUS

void fms_close_score()
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->closeScore();
}

void fms_save(const char* name)
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->saveScore(name, true);
}

void fms_save_as(const char* name)
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->saveScoreAs(juce::String(name));
}

// SAVE ME!
//   double now=(SchemeThread::getInstance()->scoremode) 
//     ? SchemeThread::getInstance()->scoretime : 0.0;
//   // if score capture is true AND we are under a process callback then
//   // scoretime will be >= 0 else it will be 0

void fms_ival(int par, int act, long val) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->ival(par, act, val);
}
void fms_rval(int par, int act, long num, long den) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->rval(par, act, num, den);
}
void fms_fval(int par, int act, double val) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->fval(par, act, val);
}
void fms_sval(int par, int act, const char* val) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->sval(par, act, val);
}
void fms_act(int par, int act) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->act(par, act);
}

void fms_err() {
  if (!check_fomus_exists()) return;
  fomuserr = true;
}

void fms_open_score(bool run) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->setrunwhendone(run);
}

void fms_new(const char* name)
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->newScore(name, true);
}

void fms_select(const char* name)
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->selectScore(name, true);
}

void fms_merge(const char* name, long num, long den, double flt) {
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->mergeScore(name, num, den, flt);  
}

void fms_free()
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->deleteScore();
}

void fms_clear(bool all)
{
  if (!check_fomus_exists()) return;
  fomuserr = false;
  Fomus::getInstance()->clearScore(all);
}

void fms_load(char* filename)
{
  if (!check_fomus_exists()) return;
  Fomus::getInstance()->loadScore(juce::String(filename));
}

void fms_run()
{
  if (!check_fomus_exists()) return;
  fomuserr = false;
  Fomus::getInstance()->runScore(true);
}

int fms_isfiletype(const char* ext) 
{
  if (!check_fomus_exists()) return 0;
  juce::String x(ext);
  if (!x.isEmpty() && x[0] == '.') x = x.substring(1);
  info_extslist f(fapi_info_list_exts());
  juce::String mid("mid"), midi("midi");
  for (const char **i = f.exts, **ie = (f.exts + f.n); i < ie; ++i) 
  {
    if (mid == juce::String(*i) || midi == juce::String(*i)) continue;
    if (x == juce::String(*i)) return 1;
  }
  return 0;
}

#else
int fms_isfiletype(const char* ext) {return 0;}
void fms_open_score(bool x){}
void fms_close_score(){}
void fms_new(const char* name){}
void fms_select(const char* name) {}
void fms_merge(const char* name, long num, long den, double flt) {}
void fms_free(){}
void fms_clear(){}
void fms_load(char* filename){}
void fms_run(){}
void fms_save(const char* name) {}
void fms_save_as(char const* foo) {}
void fms_ival(int par, int act, long val) {}
void fms_rval(int par, int act, long num, long den) {}
void fms_fval(int par, int act, double val) {}
void fms_sval(int par, int act, const char* val) {}
void fms_act(int par, int act) {}
void fms_err() {}
void fms_clear(bool all) {}
#endif

void plot_xml(char* text)
{
  // open new window in message thread. we dont copy Lisp string
  // because this function is blocked until callback returns so string
  // is still valid.
  juce::MessageManager::getInstance()->
    callFunctionOnMessageThread((juce::MessageCallbackFunction*)
				&PlotWindow::openWindowFromXml,
				(void*)text);
} 

void plot_add_xml_points(char* title, char* points)
{
  if (PlotWindow* w = PlotWindow::getPlotWindow(title))
  {
    w->listener.postMessage(new PlotWindow::PlotMessage(CommandIDs::PlotterAddXmlPoints, points));
  }
  else
    Console::getInstance()->printError(">>> Error: no plot named '" + juce::String(title) + "'.\n");
} 

char* plot_data(char* title, int all)
{
  juce::String text=juce::String();
  juce::String wtitle (title);
  PlotWindow* w=PlotWindow::getPlotWindow(wtitle);
  if (w!=NULL)
  {
    int nlayer=w->plotter->numLayers();
    int nfield=w->plotter->numFields();
    if ((nlayer>1) && all)
    {
      text<<"(";
      for (int i=0; i<nlayer; i++)
        text<<w->plotter->getLayer(i)->toString(TextIDs::Lisp,
                                                2,
                                                (nfield>2),
                                                0xFF);
      text<<")";
    }
    else
      text=w->plotter->getFocusLayer()->toString(TextIDs::Lisp,
                                                 2,
                                                 (nfield>2),
                                                 0xFF);
  }
  else
  {
    text="()";
    Console::getInstance()->
      printError(">> Error: no plot named '" + wtitle + "'.\n");
  }
  return (char *)strdup(text.toUTF8());  
}

bool sw_open_from_xml(char* text)
{
  juce::String tmp=juce::String(text);
  juce::XmlDocument doc (tmp);
  std::unique_ptr<juce::XmlElement> xml = doc.getDocumentElement();
  bool ok=true;
  if (xml)
  {
  //  std::cout << "xml=" << xml->toString() << "\n";
    if (xml->hasTagName("statewindow"))
    {
      StateWindow* sw=StateWindow::findStateWindow(xml->getStringAttribute("title"));
      if (sw) 
        ok=false;
      else
        juce::MessageManager::getInstance()->
          callFunctionOnMessageThread((juce::MessageCallbackFunction*)
                                      &StateWindow::openWindowFromXml,
                                      (void*)xml.get()
                                      );
    }
  }
  else
  {
    juce::String err=">>> Error: " + doc.getLastParseError() + "\n";
    Console::getInstance()->printError(err);
    ok=false;
  }
  return ok;
}

void sw_draw(char* title, s7_pointer obj, int data1, int data2)
{
  // if obj is an integer then data1 is the row and data2 is the column
  // if obj is a list then data1 is its length and data2 is the row
  StateWindow* sw = StateWindow::findStateWindow(title);
  if (sw == NULL) return; // no cell window open

  if (s7_is_integer(obj))
  {
    int value = (int)s7_integer(obj);
    // value row column
    sw->listener.postMessage(new StateWindow::StateMessage(CommandIDs::StateWindowSetCell, value, data1, data2));
  }
  else if (s7_is_pair(obj))
  {
    StateWindow::StateMessage* sm = 
      new StateWindow::StateMessage(CommandIDs::StateWindowSetCells, data2, 0, 0);
    int i;
    s7_pointer p;
    for (i = 0, p = obj; i < data1; i++, p = s7_cdr(p)) 
    {
      if (s7_is_integer(s7_car(p)))
        sm->array.add((int)s7_integer(s7_car(p)));
      else
        sm->array.add(0);
    }
    sw->listener.postMessage(sm);
  }
}

/*=======================================================================*
  Open Sound Control
  *=======================================================================*/

bool osc_open_input(int port)
{
  return OpenSoundControl::getInstance()->openInput(port);
}

bool osc_close_input()
{
  return OpenSoundControl::getInstance()->closeInput();
}

bool osc_open_output(char* host, int port)
{
  juce::String outputPort = juce::String(host);
  return OpenSoundControl::getInstance()->openOutput(host, port);
}

bool osc_close_output()
{
  return OpenSoundControl::getInstance()->closeOutput();
}

void osc_send_message(char* oscPath, s7_pointer list)
{
  juce::String path (oscPath);
  OpenSoundControl* osc = OpenSoundControl::getInstance();
  if (!osc->sendMessage(path, list))
  {
    juce::String err ("osc:message: ");
    if (osc->getOutputPort())
      err << "send failed.";
    else
      err << "output port is not open.";
    SchemeThread::getInstance()->signalSchemeError(err);
  }
}

void osc_send_bundle(double time, s7_pointer list)
{
  OpenSoundControl* osc = OpenSoundControl::getInstance();
  if (!osc->sendBundle(time, list))
  {
    juce::String err ("osc:bundle: ");
    if (osc->getOutputPort())
      err << "send failed.";
    else
      err << "output port is not open.";
    SchemeThread::getInstance()->signalSchemeError(err);
  }
}

s7_pointer osc_set_hook(char* oscpath, s7_pointer proc)
{
  juce::String path (oscpath);
  SchemeThread* st = SchemeThread::getInstance();
  if (s7_is_procedure(proc))
  {
    st->clearOscHook(path);
    st->addOscHook(path, proc);
    return st->schemeTrue;
  }  
  else if (st->clearOscHook(path))
    return st->schemeTrue;
  else
    return st->schemeFalse;
}

s7_pointer osc_is_hook(char* oscpath)
{
  // 0 == default, nnn = op, -1 = any
  juce::String path (oscpath);
  SchemeThread* st = SchemeThread::getInstance();
  if (path == "*") // * == return list of all receivers
  {
    // cons up result in reverse order  
    s7_pointer args = st->schemeNil;
    for (int i = st->oscHooks.size() - 1; i >= 0; i--)
    {
      juce::String p = st->oscHooks[i]->path;
      if (p.isEmpty()) // display default path as "/"
        p = "/";
      args = s7_cons(st->scheme, s7_make_string(st->scheme, p.toUTF8()), args);
    }
    return args;
  }
  else
    return (st->isOscHook(path)) ? st->schemeFalse : st->schemeFalse;
}

/*=======================================================================*
  SDIF
  (ffi_sdif_import "/Users/hkt/log-drum-1.sdif" "1TRC")
  1TRC or 1HRM
  ( (SIG TIME (SIG (a b c d) (a b c d) ...) ...) ...)
  *=======================================================================*/

#ifdef WITH_SDIF
s7_pointer sdif_import(char* path,  s7_pointer sig)
{  
  SchemeThread* st = SchemeThread::getInstance();
  juce::String filename = completeFile(juce::String(path)).getFullPathName();
  // check if its a valid SDIF file and contains frames we can handle
  if (!SdifCheckFileFormat(filename.toUTF8()))
    st->signalSchemeError("sdif-import: not a valid SDIF file: " + filename);
  // optional frame type to match
  SdifSignature sigmatch = eEmptySignature;
  if (s7_is_symbol(sig) || s7_is_string(sig)) // string, symbol or keyword
  {
    const char* name = (s7_is_symbol(sig)) ? s7_symbol_name(sig) : s7_string(sig);
    juce::String test (name);
    if (test.length() == 4)
      sigmatch = SdifSignatureConst(name[0], name[1], name[2], name[3]);
    else if (test.length()==5 && test[0] == ':')
      sigmatch = SdifSignatureConst(name[1], name[2], name[3], name[4]);
  }

  // now process file
  size_t bytesread = 0;
  int endoffile = 0;
  SdifErrorTagET sdiferror = eNoError; 
  SdifFileT* sdiffile = SdifFOpen(filename.toUTF8(), eReadFile);
  SdifFReadGeneralHeader(sdiffile); // Read file header
  SdifFReadAllASCIIChunks(sdiffile);  // Read ASCII header info, such as name-value tables

  // rhead is the list of frames to return, each frame is:
  // (<fsig> <ftime> (<msig> (<v1> <v2> ...) (<v1> <v2> ...) ...) ...)
  s7_pointer rhead = st->schemeNil;
  s7_pointer rtail = rhead;

  while (!endoffile && SdifFLastError(sdiffile) == NULL)
  {
    bytesread += SdifFReadFrameHeader(sdiffile);
    // optionally skip frames that dont match what we want
    if (sigmatch==eEmptySignature || SdifFCurrSignature(sdiffile) == sigmatch)
    {
      SdifFloat8 frametime = SdifFCurrTime(sdiffile);
      SdifUInt4 numarrays  = SdifFCurrNbMatrix(sdiffile);
      char* sigstring = SdifSignatureToString(SdifFCurrSignature(sdiffile));
      //      std::cout << "frame " << sigstring << ", time=" << frametime << ", arrays=" << numarrays << "\n";
      // frame = (<type> )
      s7_pointer fhead = s7_cons(st->scheme, s7_make_symbol(st->scheme, sigstring), st->schemeNil);
      s7_pointer ftail = fhead;
      // frame = (<type> <time>)
      s7_set_cdr(ftail, s7_cons(st->scheme, s7_make_real(st->scheme, frametime), st->schemeNil));
      ftail=s7_cdr(ftail);

      for (int m = 0; m < numarrays; m++)
      {
        bytesread += SdifFReadMatrixHeader(sdiffile);
        SdifSignature arraysig  = SdifFCurrMatrixSignature (sdiffile);
        SdifInt4 numrows = SdifFCurrNbRow (sdiffile);
        SdifInt4 numcols = SdifFCurrNbCol (sdiffile);
        ////        SdifDataTypeET datatype  = SdifFCurrDataType (sdiffile);
        // mhead is the pointer to a matrix list (<sig> (row...) (row...))
        s7_pointer mhead = s7_cons(st->scheme, s7_make_symbol(st->scheme, SdifSignatureToString(arraysig)), st->schemeNil);
        s7_pointer mtail = mhead;
        // std::cout << "matrix " << SdifSignatureToString(arraysig) << ", rows=" << numrows << ", cols=" << numcols << "\n";
        for (int r = 0; r < numrows; r++)
        {
          s7_pointer rhead = 0;
          s7_pointer rtail = 0;
          bytesread += SdifFReadOneRow(sdiffile);
          for (int c = 1; c <= numcols; c++)
          {
            SdifFloat8 value = SdifFCurrOneRowCol(sdiffile, c);
            if (rhead == 0)
            {
              // rhead is the pointer to a row list (val1 val2 ...)
              rhead=s7_cons(st->scheme, s7_make_real(st->scheme, value), st->schemeNil);
              rtail=rhead;
            }
            else
            {
              // add another value to end of row
              s7_set_cdr(rtail, s7_cons(st->scheme, s7_make_real(st->scheme, value), st->schemeNil));
              rtail=s7_cdr(rtail);
            }
          }
          // add this row to end of matrix
          s7_set_cdr(mtail, s7_cons(st->scheme, rhead, st->schemeNil));
          mtail=s7_cdr(mtail);
        }
        // add this matrix to end of frame
        s7_set_cdr(ftail, s7_cons(st->scheme, mhead, st->schemeNil));
        ftail=s7_cdr(ftail);
      }
      // add this frame to end of results
      if (rhead == st->schemeNil)
      {
        rhead = s7_cons(st->scheme, fhead, st->schemeNil);
        rtail = rhead;
      }
      else
      {
        s7_set_cdr(rtail, s7_cons(st->scheme, fhead, st->schemeNil));
        rtail=s7_cdr(rtail);
      }
    }
    else
    {
      bytesread += SdifFSkipFrameData(sdiffile);
    }
    endoffile = (SdifFGetSignature(sdiffile, &bytesread) == eEof);
  }
  
  // close file and check for errors
  if (SdifFLastError(sdiffile))  
  {
    sdiferror = SdifFLastErrorTag(sdiffile);
  }
  SdifFClose(sdiffile);
  
  if (sdiferror != eNoError)
    SchemeThread::getInstance()->signalSchemeError("sdif-import: SDIF error: #" + juce::String(sdiferror));
  
  return rhead;
}
#else
s7_pointer sdif_import(char* file,  s7_pointer args)
{
  SchemeThread::getInstance()->signalSchemeError("SDIF not available in this build of Grace.");
  return SchemeThread::getInstance()->schemeNil;
}
#endif
