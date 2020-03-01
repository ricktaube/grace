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

#include "Libraries.h"

#ifdef JUCE_WIN32
#include "loadlibrary.h"
#endif

#include "Enumerations.h"
#include "Fomus.h"
#include "Console.h"
#include "Preferences.h"
#include "CmSupport.h"
//#include "Alerts.h"

// juce defines JUCE_LINUX, JUCE_MAC or JUCE_WIN32 already
#if defined(JUCE_LINUX) || defined(JUCE_MAC)
#include <dlfcn.h>
#endif

/*=======================================================================*
  Fomus Instance
  *=======================================================================*/

juce_ImplementSingleton(Fomus);

bool fomuserr = false;

// some default output file for people who hate dialogue boxes
#define DEFAULT_OUT "out.ly"

bool check_fomus_exists() {
  static bool fomus_printederr = false;
  if (fomus_exists) return true;
  if (!fomus_printederr) {
    fomus_printederr = true;
    Console::getInstance()->printError(">>> Error: Fomus: can't find FOMUS library\n");
  }
  return false;
}

juce::String getwildcards()
{
  juce::String r;
  info_extslist f(fapi_info_list_exts());
  bool fi = true;
  juce::String mid("mid");
  for (const char **i = f.exts, **ie = (f.exts + f.n); i < ie; ++i) 
  {
    if (*i == mid.toUTF8().getAddress()) continue; // skip midi files
    if (fi) 
      fi = false; 
    else
      r << ';';
    r << "*." << *i;
  }
  return r;
}

void Fomus::closeScore()
{
  // called by the scheduler after all processes outputting to the score have stopped.
  // presumably this triggers fomus' score parsing and output handling
  if (scores.getUnchecked(current)->runwhendone) {
    runScore(true);
  }
}

void Fomus::saveScore(const juce::String& fn, const bool fromscm) {
  if (fn.isEmpty()) {
    if (fromscm) {
      Console::getInstance()->printError(">>> Error: Fomus: no output filename specified\n");
      return;
    }
    juce::FileChooser choose("Save Score", juce::File::getCurrentWorkingDirectory(), "*.fms");
    if (choose.browseForFileToSave(true)) {
      fapi_fomus_save(fapi_fomus_copy(getfomusdata()), choose.getResult().getFullPathName().toUTF8());
    }
  } else
  {
    fapi_fomus_save(fapi_fomus_copy(getfomusdata()), fn.toUTF8());
  }
}

void Fomus::saveScoreAs(juce::String filename) 
{
  // if filename is empty we were called from gui, else we were called
  // from scheme with filename guaranteed to end with .ly .xml or .mid
  if (filename.isEmpty()) 
  {
    juce::FileChooser choose("Save Score As", juce::File::getCurrentWorkingDirectory(), "*.ly;*.xml;*.mid;");
    if (!choose.browseForFileToSave(true))
      return;
    filename=choose.getResult().getFullPathName();
  }
  //  std::cout << "save score as:" << filename.toUTF8() << "\n";
  FOMUS tmp = fapi_fomus_copy(getfomusdata());
  fapi_fomus_sval(tmp, fomus_par_setting, fomus_act_set, "filename");
  fapi_fomus_sval(tmp, fomus_par_settingval, fomus_act_set, filename.toUTF8());  
  fapi_fomus_run(tmp);   // calling `run' automatically destroys the copy
}

inline void spitout(const char* str) 
{
  juce::String msg (";; ");
  msg << str;
  Console::getInstance()->printOutput(msg);
}

fomus_err_type fapi_fomus_err;
fomus_version_type fapi_fomus_version;
fomus_init_type fapi_fomus_init;
fomus_new_type fapi_fomus_new;
fomus_free_type fapi_fomus_free;
fomus_clear_type fapi_fomus_clear;
fomus_ival_type fapi_fomus_ival;
fomus_rval_type fapi_fomus_rval;
fomus_fval_type fapi_fomus_fval;
fomus_sval_type fapi_fomus_sval;
fomus_act_type fapi_fomus_act;
fomus_load_type fapi_fomus_load;
fomus_parse_type fapi_fomus_parse;
fomus_run_type fapi_fomus_run;
fomus_copy_type fapi_fomus_copy;
fomus_save_type fapi_fomus_save;
fomus_set_outputs_type fapi_fomus_set_outputs;
fomus_get_ival_type fapi_fomus_get_ival;
fomus_get_rval_type fapi_fomus_get_rval;
fomus_get_fval_type fapi_fomus_get_fval;
fomus_get_sval_type fapi_fomus_get_sval;
info_list_exts_type fapi_info_list_exts;
info_infoapi_version_type fapi_info_infoapi_version;
fomus_api_version_type fapi_fomus_api_version;
fomus_merge_type fapi_fomus_merge;

info_list_modules_type fapi_info_list_modules;
info_list_settings_type fapi_info_list_settings;
info_list_marks_type fapi_info_list_marks;
info_getlastentry_type fapi_info_getlastentry;
info_list_percinsts_type fapi_info_list_percinsts;
info_list_insts_type fapi_info_list_insts;
info_list_parts_type fapi_info_list_parts;
info_list_metaparts_type fapi_info_list_metaparts;
info_list_measdefs_type fapi_info_list_measdefs;
settingloc_to_str_type fapi_settingloc_to_str;
module_id_type fapi_module_id;
modtype_to_str_type fapi_modtype_to_str;

struct dlerr {};
inline void* fdlsym(void *handle, const char *symbol) {
  void* ret = dlsym(handle, symbol);
  if (!ret) throw dlerr();
  return ret;
}

bool fomus_exists = false;

void initfomus() {
  static bool triedit = false;
  if (!fomus_exists) {
    if (triedit) return;
    triedit = true;
    try {
#if defined(JUCE_LINUX)
      void* ha = dlopen((juce::String(FOMUSLIBPATH) + "/libfomus.so").toUTF8(), RTLD_LAZY | RTLD_GLOBAL);
      if (!ha) {
	ha = dlopen("libfomus.so", RTLD_LAZY | RTLD_GLOBAL);
	if (!ha) {
	  ha = dlopen("/usr/local/lib/libfomus.so", RTLD_LAZY | RTLD_GLOBAL);
	  if (!ha) {
	    ha = dlopen("/usr/lib/libfomus.so", RTLD_LAZY | RTLD_GLOBAL);
	    if (!ha) return;
	  }
	}
      }
#elif defined(JUCE_MAC)
      void* ha = dlopen("/usr/local/libfomus.dylib", RTLD_LAZY | RTLD_GLOBAL);
      if (!ha) {
	ha = dlopen("libfomus.dylib", RTLD_LAZY | RTLD_GLOBAL);
	if (!ha) {
	  ha = dlopen("/usr/local/lib/libfomus.dylib", RTLD_LAZY | RTLD_GLOBAL);
	  if (!ha) {
	    ha = dlopen("/usr/lib/libfomus.dylib", RTLD_LAZY | RTLD_GLOBAL);
	    if (!ha) return;
	  }
	}
      }
#elif defined(JUCE_WIN32)
      void* ha = dlopen();
      if (!ha) return;
#endif
      fapi_fomus_api_version = (fomus_api_version_type)fdlsym(ha, "fomus_api_version");
      fapi_info_infoapi_version = (info_infoapi_version_type)fdlsym(ha, "info_infoapi_version");
      if (FOMUS_API_VERSION < fapi_fomus_api_version() || FOMUS_INFOAPI_VERSION < fapi_info_infoapi_version()) {
	Console::getInstance()->printError(">>> Error: Fomus: FOMUS library has changed--upgrade Grace to a newer version that is compatible\n");
	return;	
      }
      if (FOMUS_API_VERSION > fapi_fomus_api_version() || FOMUS_INFOAPI_VERSION > fapi_info_infoapi_version()) {
	Console::getInstance()->printError(">>> Error: Fomus: FOMUS library is too old--upgrade FOMUS to a newer version that is compatible\n");
	return;	
      }
      fapi_fomus_err = (fomus_err_type)fdlsym(ha, "fomus_err");
      fapi_fomus_version = (fomus_version_type)fdlsym(ha, "fomus_version");
      fapi_fomus_init = (fomus_init_type)fdlsym(ha, "fomus_init");
      fapi_fomus_new = (fomus_new_type)fdlsym(ha, "fomus_new");
      fapi_fomus_free = (fomus_free_type)fdlsym(ha, "fomus_free");
      fapi_fomus_clear = (fomus_clear_type)fdlsym(ha, "fomus_clear");
      fapi_fomus_ival = (fomus_ival_type)fdlsym(ha, "fomus_ival");
      fapi_fomus_rval = (fomus_rval_type)fdlsym(ha, "fomus_rval");
      fapi_fomus_fval = (fomus_fval_type)fdlsym(ha, "fomus_fval");
      fapi_fomus_sval = (fomus_sval_type)fdlsym(ha, "fomus_sval");
      fapi_fomus_act = (fomus_act_type)fdlsym(ha, "fomus_act");
      fapi_fomus_load = (fomus_load_type)fdlsym(ha, "fomus_load");
      fapi_fomus_parse = (fomus_parse_type)fdlsym(ha, "fomus_parse");
      fapi_fomus_run = (fomus_run_type)fdlsym(ha, "fomus_run");
      fapi_fomus_copy = (fomus_copy_type)fdlsym(ha, "fomus_copy");
      fapi_fomus_save = (fomus_save_type)fdlsym(ha, "fomus_save");
      fapi_fomus_set_outputs = (fomus_set_outputs_type)fdlsym(ha, "fomus_set_outputs");
      fapi_fomus_get_ival = (fomus_get_ival_type)fdlsym(ha, "fomus_get_ival");
      fapi_fomus_get_rval = (fomus_get_rval_type)fdlsym(ha, "fomus_get_rval");
      fapi_fomus_get_fval = (fomus_get_fval_type)fdlsym(ha, "fomus_get_fval");
      fapi_fomus_get_sval = (fomus_get_sval_type)fdlsym(ha, "fomus_get_sval");
      fapi_info_list_exts = (info_list_exts_type)fdlsym(ha, "info_list_exts");
      fapi_fomus_merge = (fomus_merge_type)fdlsym(ha, "fomus_merge");
      fapi_info_list_modules = (info_list_modules_type)fdlsym(ha, "info_list_modules");
      fapi_info_list_settings = (info_list_settings_type)fdlsym(ha, "info_list_settings");
      fapi_info_list_marks = (info_list_marks_type)fdlsym(ha, "info_list_marks");
      fapi_info_getlastentry = (info_getlastentry_type)fdlsym(ha, "info_getlastentry");
      fapi_info_list_percinsts = (info_list_percinsts_type)fdlsym(ha, "info_get_percinsts");
      fapi_info_list_insts = (info_list_insts_type)fdlsym(ha, "info_get_insts");
      fapi_info_list_parts = (info_list_parts_type)fdlsym(ha, "info_get_parts");
      fapi_info_list_metaparts = (info_list_metaparts_type)fdlsym(ha, "info_get_metaparts");
      fapi_info_list_measdefs = (info_list_measdefs_type)fdlsym(ha, "info_get_measdefs");
      fapi_settingloc_to_str = (settingloc_to_str_type)fdlsym(ha, "info_settingloc_to_str");
      fapi_module_id = (module_id_type)fdlsym(ha, "module_id");
      fapi_modtype_to_str = (modtype_to_str_type)fdlsym(ha, "info_modtype_to_str");
    } catch (const dlerr& e) {
      Console::getInstance()->printError(">>> Error: Fomus: error loading libfomus\n");      
      return;
    }
    fapi_fomus_init();
    fapi_fomus_set_outputs(&spitout, &spitout, true);
    fomus_exists = true;
  }
}


void Fomus::newScore(const juce::String& nam, const bool fromscm)
{
  FomusScore* score = new FomusScore();
  if (!fromscm && nam.isEmpty() && scores.size() > 0)
  {
    juce::FileChooser choose("New Score", juce::File::getCurrentWorkingDirectory(), getwildcards());    
    if (choose.browseForFileToSave(true))
    {
      juce::File fn(choose.getResult());
      juce::String fn0(fn.getFileName());
      score->name = (fn0.isEmpty() ? "(untitled)" : fn0);
      scores.add(score);
      current=scores.size()-1;
      sval(fomus_par_setting, fomus_act_set, "filename");
      sval(fomus_par_settingval, fomus_act_set, fn.getFullPathName());
    }
  } 
  else
  {
    score->name = (nam.isEmpty() ? "(untitled)" : completeFile(nam).getFileName());
    scores.add(score);
    current=scores.size()-1;
    sval(fomus_par_setting, fomus_act_set, "filename");
    sval(fomus_par_settingval, fomus_act_set, nam);
  }
}

void Fomus::selectScore(const juce::String& nam, const bool fromscm)
{
  juce::File fn(completeFile(nam));
  for (int i = 0; i < scores.size(); ++i)
  {
    juce::String fn2(fapi_fomus_get_sval(scores.getUnchecked(i)->getfom(), "filename"));
    if (!fn2.isEmpty() && completeFile(fn2) == fn)
    { // find an exact match
      current = i;
      return;
    }
  }
  newScore(nam, fromscm); // selectScore is always from Scheme
}

void Fomus::mergeScore(const juce::String& nam, fomus_int num, fomus_int den, fomus_float flt)
{
  juce::File fn(completeFile(nam));
  for (int i = 0; i < scores.size(); ++i)
  {
    juce::String fn2 (fapi_fomus_get_sval(scores.getUnchecked(i)->getfom(), "filename"));
    if (!fn2.isEmpty() && completeFile(fn2) == fn)
    { // find an exact match
      switch (den)
      {
      case -1: fval(fomus_par_events, fomus_act_set, flt); break;
      case 0: ival(fomus_par_events, fomus_act_set, num); break;
      default: if (den > 0) rval(fomus_par_events, fomus_act_set, num, den); break;
      }
      fapi_fomus_merge(scores.getUnchecked(current)->getfom(), scores.getUnchecked(i)->getfom());
      return;
    }
  }
  Console::getInstance()->printError(">>> Error: Fomus: can't find score to merge from\n");
}

void Fomus::deleteScore()
{
  // insist on at least one score
  if (scores.size()>1)
  {
    FomusScore* fms=scores.getUnchecked(current);
    scores.removeObject(fms, false);
    current=scores.size()-1;
  }
}

void Fomus::clearScore(const bool all)
{
  // clear out everthing but the file name else just clear out notes
  if (all)
  {
    // cache the filename before clearing!
    juce::String fn(fapi_fomus_get_sval(getfomusdata(), "filename"));
    fapi_fomus_clear(getfomusdata());
    sval(fomus_par_setting, fomus_act_set, "filename");
    sval(fomus_par_settingval, fomus_act_set, fn);
  }
  else
    fapi_fomus_act(getfomusdata(), fomus_par_events, fomus_act_clear);
}

void Fomus::loadScore(juce::String filename)
{
  fapi_fomus_load(getfomusdata(), (char*)filename.toUTF8().getAddress());
  juce::String fn(fapi_fomus_get_sval(getfomusdata(), "filename"));
  scores.getUnchecked(current)->name = (fn.isEmpty() ? "(untitled)" : completeFile(fn).getFileName());
}

void Fomus::runScore(const bool fromscm)
{
  if (fomuserr)
  {
    Console::getInstance()->printError(">>> Error: Fomus: run canceled due to input errors\n");
    return;
  }

  if (juce::String(fapi_fomus_get_sval(getfomusdata(), "filename")).isEmpty())
  {
    if (!fromscm) renameScoreDialog();
    else
    {
      juce::String fn0("out.ly");
      juce::File fn(juce::File::getCurrentWorkingDirectory().getFullPathName() + juce::File::separator + fn0);
      scores.getUnchecked(current)->name = fn0;
      sval(fomus_par_setting, fomus_act_set, "filename");
      sval(fomus_par_settingval, fomus_act_set, fn.getFullPathName());  
    }
  }

  juce::String pa(fapi_fomus_get_sval(getfomusdata(), "lily-exe-path"));
  if (juce::File::isAbsolutePath(pa))
  {
    if (!juce::File(pa).existsAsFile())
    {
      sval(fomus_par_setting, fomus_act_set, "lily-exe-path");
      sval(fomus_par_settingval, fomus_act_set, "");  
    }
  }
  juce::String pa2(fapi_fomus_get_sval(getfomusdata(), "lily-view-exe-path"));
  if (juce::File::isAbsolutePath(pa2))
  {
    if (!juce::File(pa2).existsAsFile())
    {
      sval(fomus_par_setting, fomus_act_set, "lily-view-exe-path");
      sval(fomus_par_settingval, fomus_act_set, "");  
    }
  }
  fapi_fomus_run(fapi_fomus_copy(getfomusdata()));
}

bool Fomus::proctime(int act)
{
  if (act == fomus_act_n)
  {
    if (SchemeThread::getInstance()->isScoreMode())
    {
      fapi_fomus_fval(getfomusdata(), fomus_par_time, fomus_act_set, SchemeThread::getInstance()->scoretime);
      if (fapi_fomus_err())
        fomuserr = true;
    } 
    else
    {
      fomuserr = true;
      Console::getInstance()->printError(">>> Error: Fomus: need a time argument\n");
    }
    return true;
  }
  return false;
}

void Fomus::ival(int par, int act, fomus_int val)
{
  fapi_fomus_ival(getfomusdata(), par, act, val);
  if (fapi_fomus_err())
    fomuserr = true;
}

void Fomus::rval(int par, int act, fomus_int num, fomus_int den)
{
  fapi_fomus_rval(getfomusdata(), par, act, num, den);
  if (fapi_fomus_err()) 
    fomuserr = true;
}

void Fomus::fval(int par, int act, double val)
{
  fapi_fomus_fval(getfomusdata(), par, act, val);
  if (fapi_fomus_err())
    fomuserr = true;
}

void Fomus::sval(int par, int act, const juce::String& val) 
{
  fapi_fomus_sval(getfomusdata(), par, act, (char*)val.toUTF8().getAddress());
  if (fapi_fomus_err()) fomuserr = true;
}

void Fomus::act(int par, int act) 
{
  if (proctime(act)) return;
  fapi_fomus_act(getfomusdata(), par, act);
  if (fapi_fomus_err()) fomuserr = true;
}

// SAVE ME!
// called by sprout to open and/or initialize a (possibly) new score
// instance for receiving data from processes.  scorename is either
// "fomus" or "*.{ly|fms|xml}". if its "fomus" maybe the current
// score instance should be used and if its a file name a new score
// with that output file and appropriate back end should become
// current?  scoreargs is a string containing whatever the user
// passed as to sprout arguments to the score

// SAVE ME!
// if scoretime > 0 then we are being called under a process and the
// user's xml time value needs to be shifted by that value to
// determine the true time of the note in the score. if scoretime is
// 0 then treat the xml time value as the absolute timestamp in the
// score.

/*=======================================================================*
  Remainder of file is Grace GUI code
 *=======================================================================*/

class fomusinfo 
{

protected:
  int num;
  FOMUS fom;
  juce::String id;
  juce::String defstr, chstr;
  bool valid, knvalid, def, kndef;

public:

  fomusinfo(int c, FOMUS fom, const juce::String& id, const juce::String& str)
    : num(c),
      fom(fom),
      id(id),
      defstr(str),
      chstr(str),
      valid(true),
      knvalid(true), 
      kndef(false)
  {
  }
  
  fomusinfo(int c, FOMUS fom, const juce::String& id, const juce::String& defstr, const juce::String& str)
    : num(c),
      fom(fom),
      id(id),
      defstr(defstr),
      chstr(str),
      valid(true),
      knvalid(true),
      kndef(false)
  {
  }

  virtual ~fomusinfo (){}
  juce::String getid() const {return id;}
  const juce::String& getchangedstr() const {return chstr;}

  bool isdef()
  {
    if (kndef) return def;
    kndef = true;
    return (def = (chstr == defstr));
  }

  bool validate()
  {
    if (knvalid)
      return true;
    valid = valid_aux();
    knvalid = true;
    if (!valid)
      juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                        "FOMUS Settings",
                                        "Error in " + what() + " `" + juce::String(id) + "'.");
    return valid;
  }

  int getnum() const {return num;}
  void reset()
  {
    chstr = defstr;
    kndef = def = true;
    knvalid = false; // maybe the default isn't valid
  }
  // call this when focus is lost
  void change(const juce::String& str) {chstr = str; knvalid = kndef = false; }
  void dec() {--num;}
  virtual bool issetting() const {return false;}
  virtual void remove() const {};
  virtual bool needsreset() const {return false;}

protected:
  virtual juce::String what() const = 0;
  virtual bool valid_aux() = 0;

};

// settings
class fomusinfocont_sets;
class fomus_set : public fomusinfo 
{
  juce::String descdoc, modname, loc, uselevel, typedoc;
  bool reset;

public:
  fomus_set(int c, FOMUS fom, struct info_setting& set, const juce::String& def)
    : fomusinfo(c, fom, set.name, def, set.valstr),
      descdoc(set.descdoc),
      modname(set.modname),
      loc(fapi_settingloc_to_str(set.loc)),
      typedoc(set.typedoc),
      reset(set.reset) 
  {
    switch (set.uselevel) 
    {
    case 0: uselevel = "beginner"; break;
    case 1: uselevel = "intermediate"; break;
    case 2: uselevel = "advanced"; break;
    case 3: uselevel = "guru";      
    }
  }
  bool issetting() const {return true;}
  const juce::String& getdescdoc() const {return descdoc;}
  const juce::String& getmodname() const {return modname;}
  const juce::String& getloctext() const {return loc;}
  const juce::String& getuselevel() const {return uselevel;}
  const juce::String& gettypedoc() const {return typedoc;}
  bool needsreset() const {return reset;}

private:
  bool valid_aux()
  {
    fapi_fomus_parse(fom, (id + " = " + (chstr.isEmpty() ? "\"\"" : chstr)).toUTF8());
    return !fapi_fomus_err();
  }
  juce::String what() const {return "setting";}

};

inline juce::String rempar(const char* str0)
{
  juce::String str (str0);
  if (str.length() >= 2 && (str[0] == '(' || str[0] == '<'))
  {
    return str.substring(1, str.length() - 1);
  } 
  else
    return str;
}

class fomus_other : public fomusinfo
{

public:
  fomus_other(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
    : fomusinfo(c, fom, nam, rempar(obj.valstr))
  {
  }
private:
  bool valid_aux();
  virtual juce::String what0() const = 0;
};

bool fomus_other::valid_aux()
{
  fapi_fomus_parse(fom, (what0() + " " + (chstr.isEmpty() ? "<>" : (chstr[0] != '(' && chstr[0] != '<' ? ("<" + chstr + ">") : chstr))).toUTF8());
  if (fapi_fomus_err())
    return false;

  struct info_objinfo x(fapi_info_getlastentry(fom));
  id = fapi_module_id(x.obj);
  return true;
}

class fomus_part : public fomus_other
{

public:
  fomus_part(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
    : fomus_other(c, fom, obj, nam)
  {
  }
  juce::String what() const {return "part";}
  juce::String what0() const {return "part";}
  void remove() const {fapi_fomus_sval(fom, fomus_par_part, fomus_act_remove, id.toUTF8());}
};

class fomus_metapart : public fomus_other
{
public:
  fomus_metapart(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
    : fomus_other(c, fom, obj, nam)
  {
  }
  juce::String what() const {return "metapart";}
  juce::String what0() const {return "metapart";}
  void remove() const {fapi_fomus_sval(fom, fomus_par_metapart, fomus_act_remove, id.toUTF8());}
};

class fomus_measdef : public fomus_other
{
public:
  fomus_measdef(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
    : fomus_other(c, fom, obj, nam) 
  {
  }
  juce::String what() const {return "measure definition";}  
  juce::String what0() const {return "measdef";}
  void remove() const {fapi_fomus_sval(fom, fomus_par_measdef, fomus_act_remove, id.toUTF8());}
};

class fomus_inst : public fomus_other
{
public:
  fomus_inst(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
    : fomus_other(c, fom, obj, nam)
  {
  }
  juce::String what() const {return "instrument";}  
  juce::String what0() const {return "inst";}
  void remove() const {fapi_fomus_sval(fom, fomus_par_inst, fomus_act_remove, id.toUTF8());}
};

class fomus_percinst : public fomus_other
{
public:
  fomus_percinst(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
    : fomus_other(c, fom, obj, nam) 
  {
  }
  juce::String what() const {return "percussion instrument";}  
  juce::String what0() const {return "percinst";}
  void remove() const {fapi_fomus_sval(fom, fomus_par_percinst, fomus_act_remove, id.toUTF8());}
};

// ------------------------------------------------------------------------------------------------------------------------

class fomusinfocont
{
protected:
  FOMUS fom;
  int uselevel;
  std::vector<fomusinfo*> stuff;
public:
  fomusinfocont(FOMUS fom, int uselevel)
    : fom(fom),
      uselevel(uselevel)
  {
  }
  ~fomusinfocont()
  {
    for (std::vector<fomusinfo*>::iterator i(stuff.begin()); i != stuff.end(); ++i)
    {
      (*i)->validate();
      delete *i;
    }
  }
  void clearvect()
  {
    for (std::vector<fomusinfo*>::iterator i(stuff.begin()); i != stuff.end(); ++i) {(*i)->validate(); delete *i;}
    stuff.clear();
  }
  void reset()
  {
    clearvect();
    reset_aux();
  }
  void init() {reset_aux();}
  int getn() const {return (int)stuff.size();}
  fomusinfo* get(int ind) const {return stuff[ind];}
  int getuselevel() const {return uselevel;}
  void setuselevel(int lvl)
  {
    uselevel = lvl;
    reset();
  }
  virtual bool issetting() const {return false;}
  virtual fomusinfo* createnew(const juce::String& txt) {return 0;}
  void remove(int num)
  {
    stuff[num]->remove();
    for (std::vector<fomusinfo*>::iterator i(stuff.begin() + num); i != stuff.end(); ++i)
      (*i)->dec();
    stuff.erase(stuff.begin() + num);
  }
private:
  virtual void reset_aux() = 0;
};

class FOMUSSettings;

class fomusinfocont_sets : public fomusinfocont
{
  std::vector<juce::String> defs;
  FOMUSSettings *comp;
public:
  // ***** pass and store component for refresh
  fomusinfocont_sets(FOMUS fom, int uselevel);
  bool issetting() const {return true;}
  FOMUSSettings* setcomp(FOMUSSettings *c) {return comp = c;}
  void doreset() const;
private:
  void reset_aux();
};

fomusinfocont_sets::fomusinfocont_sets(FOMUS fom, int uselevel)
 : fomusinfocont(fom, uselevel),
   comp(0)
{
  struct info_setfilter se = {0, 0, 0, module_nomodtype, 0, module_noloc, 3, info_none}; 
  struct info_setfilterlist fi = {1, &se};
  info_setlist sets(fapi_info_list_settings(0, &fi, 0, 0, -1)); // config defaults
  defs.resize(sets.n);
  for (info_setting* s(sets.sets), *se(sets.sets + sets.n); s < se; ++s)
  {
    if (s->id < sets.n) defs[s->id] = s->valstr;
  }
}

void fomusinfocont_sets::reset_aux()
{
  struct info_setfilter se = {0, 0, 0, module_nomodtype, 0, module_noloc, uselevel, info_none}; 
  struct info_setfilterlist fi = {1, &se};
  info_setlist sets(fapi_info_list_settings(fom, &fi, 0, 0, -1));
  int c = 0;
  for (info_setting* s(sets.sets), *se(sets.sets + sets.n); s < se; ++s) 
    stuff.push_back(new fomus_set(c++, fom, *s, defs[s->id]));
}

class fomusinfocont_other : public fomusinfocont
{
public:
  fomusinfocont_other(FOMUS fom):fomusinfocont(fom, 0) {}
private:
  void reset_aux();
  virtual info_objinfo_list getem() const = 0;
  virtual fomusinfo* getnew(int c, FOMUS fom, const info_objinfo& obj, const char* nam) = 0;
};

void fomusinfocont_other::reset_aux()
{
  info_objinfo_list li(getem());
  int c = 0;
  for (info_objinfo *i(li.objs), *ie(li.objs + li.n); i < ie; ++i)
  {
    const char* nam = fapi_module_id(i->obj);
    if (nam[0])
      stuff.push_back(getnew(c++, fom, *i, nam));
  }
}

class fomusinfocont_parts : public fomusinfocont_other
{
public:
  fomusinfocont_parts(FOMUS fom)
  : fomusinfocont_other(fom)
  {
  }
private:
  info_objinfo_list getem() const {return fapi_info_list_parts(fom);}
  fomusinfo* getnew(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
  {
    return new fomus_part(c, fom, obj, nam);
  }
  fomusinfo* createnew(const juce::String& txt);
};

fomusinfo* fomusinfocont_parts::createnew(const juce::String& txt) 
{
  juce::String str("part ");
  if (txt.isEmpty())
    str << "<>";
  else if (txt[0] != '(' && txt[0] != '<' )
    str << "<" << txt << ">";
  else 
    str << txt;
  fapi_fomus_parse(fom, str.toUTF8());
  if (fapi_fomus_err())
  {
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                      "FOMUS Settings",
                                      "Error in part");
    return 0;
  }
  else
  {
    struct info_objinfo x(fapi_info_getlastentry(fom));
    fomus_part* y;
    stuff.push_back(y = new fomus_part((int)stuff.size(), fom, x, fapi_module_id(x.obj)));
    return y;
  }
}

class fomusinfocont_metaparts : public fomusinfocont_other
{
public:
  fomusinfocont_metaparts(FOMUS fom):fomusinfocont_other(fom) {}
private:
  info_objinfo_list getem() const {return fapi_info_list_metaparts(fom);}
  fomusinfo* getnew(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
  {
    return new fomus_metapart(c, fom, obj, nam);
  }
  fomusinfo* createnew(const juce::String& txt);
};

fomusinfo* fomusinfocont_metaparts::createnew(const juce::String& txt) 
{
  juce::String str ("metapart ");
  if (txt.isEmpty())
    str << "<>" ;
  else if (txt[0] != '(' && txt[0] != '<')
    str << "<" << txt << ">";
  else 
    str << txt;
  fapi_fomus_parse(fom, str.toUTF8());
  if (fapi_fomus_err()) 
  {
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                      "FOMUS Settings",
                                      "Error in metapart");
    return 0;
  }
  else
  {
    struct info_objinfo x(fapi_info_getlastentry(fom));
    fomus_metapart* y;
    stuff.push_back(y = new fomus_metapart((int)stuff.size(), fom, x, fapi_module_id(x.obj)));
    return y;
  }
}

class fomusinfocont_measdefs : public fomusinfocont_other
{
public:
  fomusinfocont_measdefs(FOMUS fom) 
  : fomusinfocont_other(fom) 
  {
  }
private:
  info_objinfo_list getem() const {return fapi_info_list_measdefs(fom);}
  fomusinfo* getnew(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
  {
    return new fomus_measdef(c, fom, obj, nam);
  }
  fomusinfo* createnew(const juce::String& txt);
};

fomusinfo* fomusinfocont_measdefs::createnew(const juce::String& txt) 
{
  juce::String str ("measdef ");
  if (txt.isEmpty())
    str << "<>";
  else if (txt[0] != '(' && txt[0] != '<')
    str << "<" << txt << ">";
  else 
    str << txt;
  fapi_fomus_parse(fom, str.toUTF8());
  if (fapi_fomus_err())
  {
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                      "FOMUS Settings",
                                      "Error in measure definition");
    return 0;
  }
  else
  {
    struct info_objinfo x(fapi_info_getlastentry(fom));
    fomus_measdef *y;
    stuff.push_back(y = new fomus_measdef((int)stuff.size(), fom, x, fapi_module_id(x.obj)));
    return y;
  }
}

class fomusinfocont_insts:public fomusinfocont_other {
public:
  fomusinfocont_insts(FOMUS fom):fomusinfocont_other(fom) {}
private:
  info_objinfo_list getem() const {return fapi_info_list_insts(fom);}
  fomusinfo* getnew(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
  {
    return new fomus_inst(c, fom, obj, nam);
  }
  fomusinfo* createnew(const juce::String& txt);
};

fomusinfo* fomusinfocont_insts::createnew(const juce::String& txt) 
{
  juce::String str ("inst ");
  if (txt.isEmpty())
    str << "<>";
  else if (txt[0] != '(' && txt[0] != '<')
    str << "<" << txt << ">";
  else
    str << txt;
  fapi_fomus_parse(fom, str.toUTF8());
  if (fapi_fomus_err())
  {
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                      "FOMUS Settings",
                                      "Error in instrument");
    return 0;
  }
  else
  {
    struct info_objinfo x(fapi_info_getlastentry(fom));
    fomus_inst *y;
    stuff.push_back(y = new fomus_inst((int)stuff.size(), fom, x, fapi_module_id(x.obj)));
    return y;
  }
}

class fomusinfocont_percinsts : public fomusinfocont_other
{
public:
  fomusinfocont_percinsts(FOMUS fom)
  : fomusinfocont_other(fom) 
  {
  }
private:
  info_objinfo_list getem() const {return fapi_info_list_percinsts(fom);}
  fomusinfo* getnew(int c, FOMUS fom, const info_objinfo& obj, const char* nam)
  {
    return new fomus_percinst(c, fom, obj, nam);
  }
  fomusinfo* createnew(const juce::String& txt);
};

fomusinfo* fomusinfocont_percinsts::createnew(const juce::String& txt) 
{
  juce::String str ("percinst ");
  if (txt.isEmpty())
    str << "<>";
  else if (txt[0] != '(' && txt[0] != '<')
    str << "<" << txt << ">";
  else
    str << txt;
  fapi_fomus_parse(fom, str.toUTF8());
  if (fapi_fomus_err())
  {
    juce::AlertWindow::showMessageBox(juce::AlertWindow::WarningIcon,
                                      "FOMUS Settings",
                                      "Error in percussion instrument");
    return 0;
  }
  else
  {
    struct info_objinfo x(fapi_info_getlastentry(fom));
    fomus_percinst *y;
    stuff.push_back(y = new fomus_percinst((int)stuff.size(), fom, x, fapi_module_id(x.obj)));
    return y;
  }
}

/*=======================================================================*
  Windows
  *=======================================================================*/

class FOMUSListBoxModel;
class FOMUSSettingsListBoxItem;
class FOMUSSettingsEditor:public juce::TextEditor
{
public:
  FOMUSListBoxModel& boxmodel;
  fomusinfo* inf;

  FOMUSSettingsEditor(fomusinfo* inf, FOMUSListBoxModel& boxmodel, 
                      FOMUSSettingsListBoxItem& lb,
                      const juce::String &componentName = juce::String(),
		      const juce::juce_wchar passwordCharacter = 0)
    : juce::TextEditor(componentName, passwordCharacter),

      boxmodel(boxmodel),
      inf(inf),
      dirty(false),
      lb(lb),
      invalid(false) 
  {
  }
  juce_UseDebuggingNewOperator
  private:
  FOMUSSettingsEditor(const FOMUSSettingsEditor&);
  const FOMUSSettingsEditor& operator= (const FOMUSSettingsEditor&);
  bool dirty;
  FOMUSSettingsListBoxItem& lb;
public:
  void focusLost(FocusChangeType cause)
  {
    validate();
    juce::TextEditor::focusLost(cause);
  }
  void validate();
  void reset()
  {
    if (inf)
    {
      inf->reset();
      setText(inf->getchangedstr(), false);
    }
    else
      setText("id ...", false);
    dirty = false;
  }
  void setinfo(fomusinfo* inf0)
  {
    inf = inf0;
    dirty = false;
  }
  bool isdef() const {return inf ? (bool)inf->isdef() : getText().isEmpty();}
  juce::String getchangedstr() {return inf ? inf->getchangedstr() : juce::String();}
  int getnum() const {return inf ? inf->getnum() : -1;}
  void setdirty() {dirty = true;}
  bool invalid;
};

// SETTINGS LISTBOX ITEM

class FOMUSSettingsListBox;
class FOMUSSettingsListBoxItem : public juce::Component,
                                 public juce::ButtonListener,
                                 public juce::TextEditorListener 
{
  FOMUSListBoxModel& boxmodel;
public:
  FOMUSSettingsListBoxItem(fomusinfo* inf, FOMUSListBoxModel& boxmodel);
  ~FOMUSSettingsListBoxItem();

  void resized();
  void buttonClicked(juce::Button* buttonThatWasClicked);

  void updateset(fomusinfo* set);
  
  void textEditorTextChanged(juce::TextEditor&) {defoff(); ValueText->setdirty();}
  void textEditorReturnKeyPressed(juce::TextEditor&) {validate();}
  void textEditorEscapeKeyPressed(juce::TextEditor&) {defon();}
  void textEditorFocusLost(juce::TextEditor&) {}

  juce_UseDebuggingNewOperator

  //int getid() const {return ValueText->getid();}
  juce::String getstoretext() const {return ValueText->getText();}
  bool getstoredef() const {return DefaultButton ? DefaultButton->getToggleState() : false;}
  
  void validate() {ValueText->validate();}
  int getnum() {return ValueText->getnum();}

  void setname(const juce::String& str) {NameText->setText(str, juce::dontSendNotification);}
private:
  juce::Label* NameLabel;
  juce::Label* NameText;
  //juce::Label* ModuleLabel;
  //juce::Label* ModuleText;
  juce::Label* LocationLabel;
  juce::Label* LocationText;
  juce::Label* UseLevelLabel;
  juce::Label* UseLevelText;
  juce::Label* TypeLabel;
  juce::Label* TypeText;
  juce::Label* ValueLabel;
  juce::ToggleButton* DefaultButton;
  juce::TextButton* DelButton;
  FOMUSSettingsEditor* ValueText;
  FOMUSSettingsListBox* parent;
  
  FOMUSSettingsListBoxItem(const FOMUSSettingsListBoxItem&);
  const FOMUSSettingsListBoxItem& operator= (const FOMUSSettingsListBoxItem&);

  void defoff() 
  {
    if (DefaultButton) DefaultButton->setToggleState(false, juce::dontSendNotification); 
  }

  void defon()
  {
    ValueText->reset();
    ValueText->setText(ValueText->getchangedstr(), juce::dontSendNotification);
    if (DefaultButton) DefaultButton->setToggleState(ValueText->isdef(), juce::dontSendNotification);
  }
};

void FOMUSSettingsListBoxItem::updateset(fomusinfo* set)
{
  ValueText->setinfo(set);
  if (set)
  {
    NameText->setText(set->getid(), juce::dontSendNotification);
    if (set->issetting())
    {
      NameText->setTooltip(((fomus_set*)set)->getdescdoc());
      //ModuleText->setText(((fomus_set*)set)->getmodname(), false);
      LocationText->setText(((fomus_set*)set)->getloctext(), juce::dontSendNotification);
      UseLevelText->setText(((fomus_set*)set)->getuselevel(), juce::dontSendNotification);
      TypeText->setText(((fomus_set*)set)->gettypedoc(), juce::dontSendNotification);
    }
    ValueText->setText(set->getchangedstr(), juce::dontSendNotification);
    if (DefaultButton) DefaultButton->setToggleState(set->isdef(), juce::dontSendNotification);
  } 
  else
    ValueText->setText("id ...");
}

FOMUSSettingsListBoxItem::FOMUSSettingsListBoxItem(fomusinfo* inf, FOMUSListBoxModel& boxmodel)
  : juce::Component("FOMUS Settings ListBox Item"),
    boxmodel(boxmodel),
    NameLabel (0),
    NameText (0),
    //ModuleLabel (0),
    //ModuleText (0),
    LocationLabel (0),
    LocationText (0),
    UseLevelLabel (0),
    UseLevelText (0),
    TypeLabel (0),
    TypeText (0),
    ValueLabel (0),
    DefaultButton (0),
    DelButton(0),
    ValueText (0)
{
  addAndMakeVisible (NameLabel = new juce::Label ("Name Label", "Name:"));
  NameLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
  NameLabel->setJustificationType (juce::Justification::centredLeft);
  NameLabel->setEditable (false, false, false);
  NameLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  NameLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  addAndMakeVisible (NameText = new juce::Label ("Name Text", juce::String()));
  NameText->setFont (juce::Font (13.0000f, juce::Font::bold));
  NameText->setJustificationType (juce::Justification::centredLeft);
  NameText->setEditable (false, false, false);
  NameText->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  NameText->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));

  if (inf && inf->issetting())
  {
    // addAndMakeVisible (ModuleLabel = new juce::Label ("Module Label",
    // 						"Module:"));
    // ModuleLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
    // ModuleLabel->setJustificationType (juce::Justification::centredLeft);
    // ModuleLabel->setEditable (false, false, false);
    // ModuleLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    // ModuleLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    // addAndMakeVisible (ModuleText = new Label ("Module Text",
    // 					       juce::String()));
    // ModuleText->setFont (juce::Font (13.0000f, juce::Font::plain));
    // ModuleText->setJustificationType (juce::Justification::centredLeft);
    // ModuleText->setEditable (false, false, false);
    // ModuleText->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    // ModuleText->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (LocationLabel = new juce::Label ("Location Label", "Location:"));
    LocationLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
    LocationLabel->setJustificationType (juce::Justification::centredLeft);
    LocationLabel->setEditable (false, false, false);
    LocationLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    LocationLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (LocationText = new juce::Label ("Location Text", juce::String()));
    LocationText->setFont (juce::Font (13.0000f, juce::Font::plain));
    LocationText->setJustificationType (juce::Justification::centredLeft);
    LocationText->setEditable (false, false, false);
    LocationText->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    LocationText->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (UseLevelLabel = new juce::Label ("UseLevel Label", "Use Level:"));
    UseLevelLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
    UseLevelLabel->setJustificationType (juce::Justification::centredLeft);
    UseLevelLabel->setEditable (false, false, false);
    UseLevelLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    UseLevelLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (UseLevelText = new juce::Label ("UseLevel Text", juce::String()));
    UseLevelText->setFont (juce::Font (13.0000f, juce::Font::plain));
    UseLevelText->setJustificationType (juce::Justification::centredLeft);
    UseLevelText->setEditable (false, false, false);
    UseLevelText->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    UseLevelText->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (TypeLabel = new juce::Label ("Type Label", "Type:"));
    TypeLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
    TypeLabel->setJustificationType (juce::Justification::centredLeft);
    TypeLabel->setEditable (false, false, false);
    TypeLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    TypeLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (TypeText = new juce::Label ("Type Text", juce::String()));
    TypeText->setFont (juce::Font (13.0000f, juce::Font::plain));
    TypeText->setJustificationType (juce::Justification::centredLeft);
    TypeText->setEditable (false, false, false);
    TypeText->setColour (juce::TextEditor::textColourId, juce::Colours::black);
    TypeText->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
    
    addAndMakeVisible (DefaultButton = new juce::ToggleButton ("Default Button"));
    DefaultButton->setButtonText ("Default");
    DefaultButton->addListener (this);
  }
  else
  {
    addAndMakeVisible (DelButton = new juce::TextButton ("Delete Button"));
    DelButton->setButtonText ("Remove");
    DelButton->addListener (this);    
  }
  
  addAndMakeVisible (ValueLabel = new juce::Label ("Value Label", "Value:"));
  ValueLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
  ValueLabel->setJustificationType (juce::Justification::centredLeft);
  ValueLabel->setEditable (false, false, false);
  ValueLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  ValueLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0)); 
  addAndMakeVisible (ValueText = new FOMUSSettingsEditor(inf, boxmodel, *this, "Value Text"));
  ValueText->setMultiLine (true);
  ValueText->setReturnKeyStartsNewLine (false);
  ValueText->setReadOnly (false);
  ValueText->setScrollbarsShown (true);
  ValueText->setCaretVisible (true);
  ValueText->setPopupMenuEnabled (true);
  ValueText->addListener(this);
  setSize (640 + 32, 16 + 72);
  updateset(inf);
}

FOMUSSettingsListBoxItem::~FOMUSSettingsListBoxItem() 
{
  deleteAndZero (NameLabel);
  deleteAndZero (NameText);
  //deleteAndZero (ModuleLabel);
  //deleteAndZero (ModuleText);
  deleteAndZero (LocationLabel);
  deleteAndZero (LocationText);
  deleteAndZero (UseLevelLabel);
  deleteAndZero (UseLevelText);
  deleteAndZero (TypeLabel);
  deleteAndZero (TypeText);
  deleteAndZero (ValueLabel);
  deleteAndZero (DefaultButton);
  deleteAndZero (ValueText);
  deleteAndZero (DelButton);
}

void FOMUSSettingsListBoxItem::resized() 
{
  NameLabel->setBounds (8 + 0, 8 + 0, 48, 16);
  NameText->setBounds (8 + 40, 8 + 0, 184, 16);
  //if (ModuleLabel) ModuleLabel->setBounds (8 + 232, 8 + 0, 56, 16);
  //if (ModuleText) ModuleText->setBounds (8 + 280, 8 + 0, 83, 16);
  if (LocationLabel) LocationLabel->setBounds (8 + 232, 8 + 0, 56, 16);
  if (LocationText) LocationText->setBounds (8 + 284, 8 + 0, 83, 16);
  if (UseLevelLabel) UseLevelLabel->setBounds (8 + 368, 8 + 0, 56, 16);
  if (UseLevelText) UseLevelText->setBounds (8 + 420, 8 + 0, 75, 16);
  if (TypeLabel) TypeLabel->setBounds (8 + 0, 8 + 16, 40, 16);
  if (TypeText) TypeText->setBounds (8 + 40, 8 + 16, getWidth() - 148, 16);
  if (DefaultButton) DefaultButton->setBounds (8 + getWidth() - 100, 8 + 16, 48, 16);
  if (DelButton)
  {
    ValueLabel->setBounds (8 + 0, 8 + 32 - 14, 40, 16);
    ValueText->setBounds (8 + 40, 8 + 32 - 14, getWidth() - 80, 40 + 14);
    DelButton->setBounds(8 + getWidth() - 90, 8 + 16 - 16, 40, 14);
  }
  else
  {
    ValueLabel->setBounds (8 + 0, 8 + 32, 40, 16);
    ValueText->setBounds (8 + 40, 8 + 32, getWidth() - 80, 40);
  }
}

// LIST BOX MODEL

class FOMUSSettingsListBox;
class FOMUSListBoxModel : public juce::ListBoxModel 
{
private:
  FOMUSSettingsListBox* textlistener;
  bool hasnew;
public:
  fomusinfocont& infos;
  FOMUSListBoxModel(fomusinfocont& infos, FOMUSSettingsListBox* textlistener)
    : juce::ListBoxModel(),
      textlistener(textlistener),
      hasnew(false),
      infos(infos)
  {
  }
  int getNumRows() {return hasnew ? infos.getn() + 1 : infos.getn();}
  void paintListBoxItem(int rowNumber, juce::Graphics &g, int width, int height, bool rowIsSelected) {}
  juce::Component* refreshComponentForRow(int rowNumber, bool isRowSelected, juce::Component *existingComponentToUpdate);
  void sethasnew();
  juce::String createnew(const juce::String& txt, fomusinfo* &inf);
  void deleteone(int id);
};

void FOMUSSettingsEditor::validate()
{
  if (invalid) return;
  if (inf)
  {
    if (dirty) inf->change(getText().toUTF8());
    bool iv = inf->validate();
    if (iv && dirty && inf->needsreset())
    {
      invalid = true;
      juce::AlertWindow::showMessageBox(juce::AlertWindow::InfoIcon,
                                        "FOMUS Settings",
                                        "Other settings are being updated to reflect changes");
      ((const fomusinfocont_sets&)boxmodel.infos).doreset();
      invalid = false;
    }
    else
      lb.setname(inf->getid());
  } 
  else
    lb.setname(boxmodel.createnew(getText(), inf));
  dirty = false;
}

// SETTINGS LISTBOX

class FOMUSSettingsListBox:public juce::ListBox 
{
  fomusinfocont& infos;
public:
  FOMUSSettingsListBox(fomusinfocont& infos)
  : juce::ListBox("FOMUS ListBox", new FOMUSListBoxModel(infos, this)),
    infos(infos) 
  {
    setRowHeight(16 + 72);
  }
  ~FOMUSSettingsListBox() {delete getModel();}
  juce_UseDebuggingNewOperator
  void addnew() {((FOMUSListBoxModel*)getModel())->sethasnew();}
private:
  FOMUSSettingsListBox (const FOMUSSettingsListBox&);
  const FOMUSSettingsListBox& operator= (const FOMUSSettingsListBox&);
};

inline void FOMUSListBoxModel::sethasnew()
{
  hasnew = true;
  textlistener->updateContent();
}

juce::String FOMUSListBoxModel::createnew(const juce::String& txt, fomusinfo* &inf)
{
  inf = infos.createnew(txt);
  if (inf)
  { // success!
    hasnew = false; // inf is now at the end of the list in infos
    textlistener->updateContent();
    return inf->getid();
  }
  return "";
}

void FOMUSListBoxModel::deleteone(int id) 
{
  if (id < 0)
  {
    hasnew = false;
  }
  else
  {
    infos.remove(id);
  }
  textlistener->updateContent();
}

void FOMUSSettingsListBoxItem::buttonClicked(juce::Button* buttonThatWasClicked) {
  if (buttonThatWasClicked == DefaultButton) 
  {
    if (DefaultButton->getToggleState()) defon();
  }
  else if (buttonThatWasClicked == DelButton) 
  {
    boxmodel.deleteone(ValueText->getnum());
  }
}

juce::Component* FOMUSListBoxModel::refreshComponentForRow(int rowNumber, bool isRowSelected, 
                                                           juce::Component *existingComponentToUpdate)
{
  if (hasnew && rowNumber == getNumRows() - 1) 
  {
    return new FOMUSSettingsListBoxItem(0, *this);
  }
  if (rowNumber >= getNumRows())
  { // out of range, delete it
    if (existingComponentToUpdate)
    {
      ((FOMUSSettingsListBoxItem*)existingComponentToUpdate)->validate();
      if (((FOMUSSettingsListBoxItem*)existingComponentToUpdate)->hasKeyboardFocus(true))
        textlistener->grabKeyboardFocus();
      delete existingComponentToUpdate;
    }
    return 0;
  }
  if (existingComponentToUpdate)
  {
    if (((FOMUSSettingsListBoxItem*)existingComponentToUpdate)->getnum() != rowNumber)
    { // switching to different row
      ((FOMUSSettingsListBoxItem*)existingComponentToUpdate)->validate();
      if (((FOMUSSettingsListBoxItem*)existingComponentToUpdate)->hasKeyboardFocus(true))
        textlistener->grabKeyboardFocus();
      ((FOMUSSettingsListBoxItem*)existingComponentToUpdate)->updateset(infos.get(rowNumber));
    } // otherwise it's the same one, reuse it
    return existingComponentToUpdate;
  } 
  else
  { // completely new one
    return new FOMUSSettingsListBoxItem(infos.get(rowNumber), *this);
  }
}

struct FomusMessage : public juce::Message
{
  FOMUSSettings* pointerParameter;
  FomusMessage(FOMUSSettings* param) : pointerParameter(param) {}
  ~FomusMessage(){}
};

class SetMessageListener : public juce::MessageListener
{
  void handleMessage(const juce::Message &message);
};

// SETTINGS COMPONENT
class FOMUSSettings:public juce::Component, public juce::ButtonListener 
{
  fomusinfocont& infos;
public:
  SetMessageListener msg;
  FOMUSSettings (fomusinfocont& infos);
  ~FOMUSSettings();
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  juce_UseDebuggingNewOperator
  void resetsets();
private:
  void switchtouse(int lvl);
  FOMUSSettingsListBox* component;
  juce::ToggleButton* BeginnerButton;
  juce::ToggleButton* IntermediateButton;
  juce::ToggleButton* AdvancedButton;
  juce::ToggleButton* GuruButton;
  juce::TextButton* AddButton;
  juce::TextButton* RefreshButton;
  FOMUSSettings (const FOMUSSettings&);
  const FOMUSSettings& operator= (const FOMUSSettings&);
};

inline void fomusinfocont_sets::doreset() const 
{
  //  comp->msg.postMessage(new juce::Message(0, 0, 0, comp));
  comp->msg.postMessage(new FomusMessage(comp));
}

inline void SetMessageListener::handleMessage(const juce::Message &message)
{
  if(const FomusMessage* f = dynamic_cast<const FomusMessage*>(&(message)))
  {
    //    ((FOMUSSettings*)message.pointerParameter)->resetsets();
    f->pointerParameter->resetsets();
  }
}

void FOMUSSettings::switchtouse(int lvl)
{
  if (lvl != infos.getuselevel())
  {
    removeChildComponent (component);
    delete component;
    infos.setuselevel(lvl);
    addAndMakeVisible (component = new FOMUSSettingsListBox(infos));
    component->setBounds (0, 16, getWidth(), getHeight() - 16);
  }
}

void FOMUSSettings::resetsets()
{
  removeChildComponent (component);
  delete component;
  infos.reset();
  addAndMakeVisible (component = new FOMUSSettingsListBox(infos));
  component->setBounds (0, 16, getWidth(), getHeight() - 16);
}

FOMUSSettings::FOMUSSettings(fomusinfocont& infos)
  :juce::Component("FOMUSSettings" ),
   infos(infos), 
   component (0),
   BeginnerButton (0),
   IntermediateButton (0),
   AdvancedButton (0),
   GuruButton (0),
   AddButton (0),
   RefreshButton (0)
{
  addAndMakeVisible (component = new FOMUSSettingsListBox (infos));
  if (infos.issetting())
  {
    addAndMakeVisible (BeginnerButton = new juce::ToggleButton ("Beginner Button"));
    BeginnerButton->setButtonText ("Beginner");
    BeginnerButton->setRadioGroupId (1);
    BeginnerButton->addListener (this);
    BeginnerButton->setToggleState (true, juce::dontSendNotification);
    
    addAndMakeVisible(IntermediateButton = new juce::ToggleButton ("Intermediate Button"));
    IntermediateButton->setButtonText ("Intermediate");
    IntermediateButton->setRadioGroupId (1);
    IntermediateButton->addListener (this);
    
    addAndMakeVisible (AdvancedButton = new juce::ToggleButton ("Advanced Button"));
    AdvancedButton->setButtonText ("Advanced");
    AdvancedButton->setRadioGroupId (1);
    AdvancedButton->addListener (this);
    
    addAndMakeVisible (GuruButton = new juce::ToggleButton ("Guru Button"));
    GuruButton->setButtonText ("Guru");
    GuruButton->setRadioGroupId (1);
    GuruButton->addListener (this);
  } 
  else
  {
    addAndMakeVisible (AddButton = new juce::TextButton ("New Button"));
    AddButton->setButtonText ("New");
    AddButton->addListener (this);    
    addAndMakeVisible (RefreshButton = new juce::TextButton ("Refresh Button"));
    RefreshButton->setButtonText ("Refresh");
    RefreshButton->addListener (this);    
  }
  //setSize (640 + 32, 400);
}

FOMUSSettings::~FOMUSSettings()
{
  deleteAndZero (component);
  deleteAndZero (BeginnerButton);
  deleteAndZero (IntermediateButton);
  deleteAndZero (AdvancedButton);
  deleteAndZero (GuruButton);
  deleteAndZero (AddButton);
  deleteAndZero (RefreshButton);
}

void FOMUSSettings::resized()
{
  if (infos.issetting())
  {
    component->setBounds (0, 16, getWidth(), getHeight() - 16);
    BeginnerButton->setBounds (0, 0, 56, 16);
    IntermediateButton->setBounds (56, 0, 72, 16);
    AdvancedButton->setBounds (128, 0, 64, 16);
    GuruButton->setBounds (184, 0, 56, 16);
  }
  else
  {
    component->setBounds (0, 0, getWidth(), getHeight() - 18);
    AddButton->setBounds(8, getHeight() - 16, 40, 14);
    RefreshButton->setBounds(60, getHeight() - 16, 40, 14);
  }
}

void FOMUSSettings::buttonClicked (juce::Button* buttonThatWasClicked)
{
  if (buttonThatWasClicked == BeginnerButton)
  {
    switchtouse(0);
  }
  else if (buttonThatWasClicked == IntermediateButton) 
  {
    switchtouse(1);
  }
  else if (buttonThatWasClicked == AdvancedButton) 
  {
    switchtouse(2);
  }
  else if (buttonThatWasClicked == GuruButton) 
  {
    switchtouse(3);
  }
  else if (buttonThatWasClicked == AddButton) 
  {
    component->addnew();
  }
  else if (buttonThatWasClicked == RefreshButton) 
  {
    removeChildComponent (component);
    delete component;
    infos.reset();
    addAndMakeVisible (component = new FOMUSSettingsListBox (infos));
    component->setBounds (0, 0, getWidth(), getHeight() - 18);
  }
}

struct FileTabs : public juce::TabbedComponent
{
  fomusinfocont_sets sets;
  fomusinfocont_parts parts;
  fomusinfocont_metaparts metaparts;
  fomusinfocont_insts insts;
  fomusinfocont_percinsts percinsts;
  
  FileTabs(FOMUS fom)
   : juce::TabbedComponent(juce::TabbedButtonBar::TabsAtTop),
     sets(fom, 0),
     parts(fom),
     metaparts(fom),
     insts(fom),
     percinsts(fom) 
  {
    sets.init();
    parts.init();
    metaparts.init();
    insts.init();
    percinsts.init();
    addTab("Settings", ColorThemeIDs::getWindowBackgroundColor(), sets.setcomp(new FOMUSSettings(sets)), true);
    addTab("Parts", ColorThemeIDs::getWindowBackgroundColor(), new FOMUSSettings(parts), true);
    addTab("Metaparts", ColorThemeIDs::getWindowBackgroundColor(), new FOMUSSettings(metaparts), true);
    addTab("Instruments", ColorThemeIDs::getWindowBackgroundColor(), new FOMUSSettings(insts), true);
    addTab("Percussion Instruments", ColorThemeIDs::getWindowBackgroundColor(), new FOMUSSettings(percinsts), true);
    setSize (900, 600);
  }
};

#define FILE_WIDTH 120

struct makenstring 
{
  int n;
  makenstring(const int n) 
  : n(n)
  {
  }
};

inline std::ostream& operator<<(std::ostream& s, const makenstring x) 
{
  for (int i = 0; i < x.n; ++i) s << ' ';
  return s;
}

void out_justify(std::ostream& f, juce::String s, const int start = 0, bool va = true) 
{
  s.replace("\t", "        ");
  int i = 0;
  int je = s.length();
  const juce::String exc("\"'`{}[]():;,.!?_");
  for (int j = (FILE_WIDTH - 1) - start; j < je; j += (FILE_WIDTH - 1) - start) 
  {
    int js = i;
    while (js < j && s[js] != '\n') ++js;
    if (js < j)
    {
      f << (const char*)s.substring(i, ++js).toUTF8() << makenstring(start);
      i = j = js;
    }
    else
    {
      js = j;
      while (j > i && (va ? s[j] == ' ' : !(isalnum(s[j]) || exc.containsChar(s[j])))) --j;
      while (j > i && (va ? s[j - 1] != ' ' : isalnum(s[j - 1]) || exc.containsChar(s[j - 1]))) --j;
      if (j > i)
      {
	f << (const char*)s.substring(i, j).toUTF8() << "\n" << makenstring(start);
	i = j;
      }
      else
      {
	f << (const char*)s.substring(i, js).toUTF8() << "\n" << makenstring(start);
	i = j = js;
      }
    }
  }
  f << (const char*)s.substring(i).toUTF8();
}

class FomusDialogWindow : public juce::DocumentWindow
{
public:
  FomusDialogWindow(juce::String title)
    : juce::DocumentWindow(title, ColorThemeIDs::getWindowBackgroundColor(), juce::DocumentWindow::allButtons)
  {
    setUsingNativeTitleBar(true);
    setDropShadowEnabled(true);
    setWantsKeyboardFocus(false);
    setResizable(true, true);
  }
  ~FomusDialogWindow()
  {
  }
  void closeButtonPressed()
  {
    delete this;
  }
};


void Fomus::settingsWindow() 
{  
  /* THIS WOULD BE A NON-MODAL VERSION
     FileTabs* t=new FileTabs(getfomusdata());
     t->setVisible(true);
     FomusDialogWindow* w = new FomusDialogWindow("FOMUS Settings");
     w->setContentComponent(t, true, true);
     w->setVisible(true);
     w->centreWithSize(t->getWidth(),t->getHeight());
     w->toFront(true); 
  */

  //  FileTabs* f=new FileTabs(getfomusdata());
  //  juce::DialogWindow::showModalDialog("FOMUS Settings", f, 0, juce::Colour(0xffe5e5e5), true, true, true);
  //  delete f;
  juce::DialogWindow::LaunchOptions dw;
  dw.useNativeTitleBar = true;
  dw.resizable = false;
  dw.dialogTitle = "FOMUS Settings";
  dw.dialogBackgroundColour = ColorThemeIDs::getWindowBackgroundColor();
  dw.content.setOwned(new FileTabs(getfomusdata()));
  dw.runModal();
  juce::String fn(fapi_fomus_get_sval(getfomusdata(), "filename"));
  scores.getUnchecked(current)->name = (fn.isEmpty() ? "(untitled)" : completeFile(fn).getFileName());
}


struct FOMUSViewComponent:public juce::Component 
{
  juce::TextLayout lay;
  FOMUSViewComponent():juce::Component() {}
  FOMUSViewComponent(const juce::String &componentName):juce::Component(componentName) {}
  void paint (juce::Graphics& g)
  {
    juce::Rectangle<float> r (0,0,lay.getWidth(), lay.getHeight());
    //    lay.draw(g, 0, 0);
    lay.draw(g,r);
  }
};

struct FOMUSTabbedComp:public juce::Component 
{
  FOMUSTabbedComp():juce::Component() {}
  FOMUSTabbedComp(const juce::String &componentName):juce::Component(componentName)  {}
  virtual void doupdate(const juce::String& sch, const int but) = 0;
};

class FOMUSSetsView:public FOMUSTabbedComp, public juce::ButtonListener {
  int skill;
  juce::TextEditor& textEditor;
  int& thebut;
public:
  FOMUSSetsView(juce::TextEditor& textEditor, int& thebut);
  ~FOMUSSetsView();
  void paint (juce::Graphics& g);
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  juce_UseDebuggingNewOperator
  private:
  juce::ToggleButton* BegButton;
  juce::ToggleButton* MedButton;
  juce::ToggleButton* AdvButton;
  juce::ToggleButton* GuruButton;
  juce::Viewport* viewport;
  FOMUSSetsView (const FOMUSSetsView&);
  const FOMUSSetsView& operator=(const FOMUSSetsView&);
  void doupdate(const juce::String& sch, const int but);
};

void FOMUSSetsView::doupdate(const juce::String& sch, const int but)
{
  struct info_setfilter ff = {0, 0, 0, module_nomodtype, 0, module_noloc, skill, info_none};
  struct info_setfilterlist fff = {1, &ff};
  struct info_setsearch s = {0, 0, 0, 0, 0, 0};
  switch (but)
  {
  case 0: s.name = sch.toUTF8(); break;
  case 1: s.doc = sch.toUTF8();
  }
  struct info_setlist lst (fapi_info_list_settings(0, &fff, &s, 0, 0));

  juce::AttributedString lay;// = ((FOMUSViewComponent*)(viewport->getViewedComponent()))->lay;
  juce::TextLayout& layout = ((FOMUSViewComponent*)(viewport->getViewedComponent()))->lay;

  juce::Font no (13);
  juce::Font ul (13, juce::Font::underlined);
  juce::Font bo (13, juce::Font::bold);
  juce::Font it (13, juce::Font::italic);

  for (struct info_setting* i(lst.sets), *ie(lst.sets + lst.n); i < ie; ++i)
  {
    lay.append("name:", ul);
    lay.append(" ", no);
    lay.append(i->name, bo);
    lay.append("  ", no);
    lay.append("module:", ul);
    lay.append(" ", no);
    lay.append(i->modname, no); 
    lay.append("  ", no);
    juce::String lo (fapi_settingloc_to_str(i->loc));
    lay.append("location:", ul);
    lay.append(" ", no); 
    lay.append(lo, no); 
    lay.append("  ", no);
    lay.append("use level:", ul);
    lay.append(" ", no);
    switch (i->uselevel)
    {
    case 0: lo = "0 beginner\n"; break;
    case 1: lo = "1 intermediate\n"; break;
    case 2: lo = "2 advanced\n"; break;
    case 3: lo = "3 guru\n";      
    }
    lay.append(lo, no);
    std::ostringstream ou0;
    lay.append("type:", ul);
    lay.append(" ", no);
    out_justify(ou0, i->typedoc, 6, true); 
    lay.append(ou0.str().c_str(), no);
    lay.append("\n", no);
    std::ostringstream ou;
    lay.append("default value:", ul);
    lay.append(" ", no);
    out_justify(ou, i->valstr, 15, true); 
    lay.append(ou.str().c_str(), no); 
    lay.append("\n", no);
    std::ostringstream ou2;
    out_justify(ou2, i->descdoc, 0, false);
    lay.append(ou2.str().c_str(), it); 
    lay.append("\n\n", no);
  }
  layout.createLayout(lay, 1000);
  //  layout.layout(, juce::Justification::topLeft, false);
  viewport->getViewedComponent()->setSize(layout.getWidth(), layout.getHeight());
  viewport->setViewPosition(0, 0);
  viewport->repaint();
}

FOMUSSetsView::FOMUSSetsView(juce::TextEditor& textEditor, int& thebut)
  : FOMUSTabbedComp( "FOMUS Settings Documentation"),
    skill(0), 
    textEditor(textEditor),
    thebut(thebut),
    BegButton(0), 
    MedButton(0),
    AdvButton(0),
    GuruButton(0), 
    viewport(0)
{
  addAndMakeVisible (BegButton = new juce::ToggleButton ("Beginner Button"));
  BegButton->setButtonText ("Beginner");
  BegButton->setRadioGroupId (2);
  BegButton->addListener (this);
  BegButton->setToggleState (true, juce::dontSendNotification);
  
  addAndMakeVisible (MedButton = new juce::ToggleButton ("Intermediate Button"));
  MedButton->setButtonText ("Intermediate");
  MedButton->setRadioGroupId (2);
  MedButton->addListener (this);
  
  addAndMakeVisible (AdvButton = new juce::ToggleButton ("Advanced Button"));
  AdvButton->setButtonText ("Advanced");
  AdvButton->setRadioGroupId (2);
  AdvButton->addListener (this);
  
  addAndMakeVisible (GuruButton = new juce::ToggleButton ("Guru Button"));
  GuruButton->setButtonText ("Guru");
  GuruButton->setRadioGroupId (2);
  GuruButton->addListener (this);
  
  addAndMakeVisible (viewport = new juce::Viewport ("Viewport"));
  viewport->setViewedComponent(new FOMUSViewComponent("View Component"));
}

FOMUSSetsView::~FOMUSSetsView() 
{
  deleteAndZero (BegButton);
  deleteAndZero (MedButton);
  deleteAndZero (AdvButton);
  deleteAndZero (GuruButton);
  deleteAndZero (viewport);
}

inline void FOMUSSetsView::paint (juce::Graphics& g) 
{
  g.fillAll (juce::Colours::white);
  //  g.fillAll (ColorThemeIDs::getWindowBackgroundColor());
}

void FOMUSSetsView::resized() 
{
  BegButton->setBounds (0, 0, 56, 16);
  MedButton->setBounds (60, 0, 72, 16);
  AdvButton->setBounds (136, 0, 64, 16);
  GuruButton->setBounds (200, 0, 56, 16);
  viewport->setBounds (8, 24, getWidth() - 14, getHeight() - 30);
}

void FOMUSSetsView::buttonClicked(juce::Button* buttonThatWasClicked)
{
  if (buttonThatWasClicked == BegButton) 
  {
    skill = 0; 
  }
  else if (buttonThatWasClicked == MedButton) 
  {
    skill = 1;
  }
  else if (buttonThatWasClicked == AdvButton) 
  {
    skill = 2;
  }
  else if (buttonThatWasClicked == GuruButton) 
  {
    skill = 3;
  }
  else return;
  doupdate(textEditor.getText(), thebut);
}

class FOMUSDocView:public FOMUSTabbedComp
{
public:
  FOMUSDocView();
  ~FOMUSDocView();
  void paint (juce::Graphics& g);
  void resized();
  juce_UseDebuggingNewOperator
  juce::Viewport* viewport;
private:
  FOMUSDocView(const FOMUSDocView&);
  const FOMUSDocView& operator=(const FOMUSDocView&);
};

inline FOMUSDocView::FOMUSDocView( )
  : FOMUSTabbedComp("FOMUS Documentation View"),
    viewport(0)
{
  addAndMakeVisible (viewport = new juce::Viewport ("new viewport"));
  viewport->setViewedComponent(new FOMUSViewComponent("View Component"));
}

inline FOMUSDocView::~FOMUSDocView()
{
  deleteAndZero (viewport);
}

inline void FOMUSDocView::paint(juce::Graphics& g)
{
  g.fillAll (juce::Colours::white);
  //  g.fillAll (ColorThemeIDs::getWindowBackgroundColor());
}

inline void FOMUSDocView::resized() 
{
  viewport->setBounds (8, 8, getWidth() - 14, getHeight() - 14);
}

// FIXME!

struct FOMUSMarksView:public FOMUSDocView 
{
  FOMUSMarksView():FOMUSDocView() {}
  void doupdate(const juce::String& sch, const int but);
};

void FOMUSMarksView::doupdate(const juce::String& sch, const int but) 
{
  //  if (sch.isEmpty()) return;
  std::cout << "FOMUSMarksView::doupdate() sch=" << sch << "\n";
  info_marksearch s = {0, 0, 0, 0, 0, 0};
  switch (but)
  {
  case 0: s.name = sch.toUTF8(); break;
  case 1: s.doc = sch.toUTF8();
  }
  info_marklist lst(fapi_info_list_marks(0, 0, &s, 0, 0));
  juce::TextLayout& layout = ((FOMUSViewComponent*)(viewport->getViewedComponent()))->lay;
  juce::AttributedString lay;
  ////  lay.clear();
  juce::Font no (13);
  juce::Font ul (13, juce::Font::underlined);
  juce::Font bo (13, juce::Font::bold);
  juce::Font it (13, juce::Font::italic);
  for (struct info_mark* i(lst.marks), *ie(lst.marks + lst.n); i < ie; ++i)
  {
    //    std::cout << "AAA\n";
    lay.append("name:", ul);
    //    std::cout << "BBB\n";
    lay.append(" ", no);
    //    std::cout << "CCC\n";
    lay.append(i->name, bo);
    //    std::cout << "DDD\n";
    lay.append("  ", no);
    //    std::cout << "EEE\n";
    lay.append("arguments:", ul);
    //    std::cout << "FFF\n";
    lay.append(" ", no); 
    //    std::cout << "GGG\n";
    lay.append(i->argsdoc, no);
    //    std::cout << "HHH\n";
    lay.append("\n", no);
    //    std::cout << "III\n";
    //    std::ostringstream ou;
    //    std::cout << "JJJ\n";
    ////    out_justify(ou, i->descdoc, 0, false);
    //    std::cout << "KKK\n";
    //    lay.append(ou.str().c_str(), it);
    if (1)
    {
      lay.append(juce::CharPointer_UTF8 (i->descdoc), it);
    }

    //    std::cout << "LLL\n";
    lay.append("\n\n", no);
    //    std::cout << "MMM\n";
  }

  ////  lay.layout(std::numeric_limits<int>::max() / 2, juce::Justification::topLeft, false);
  layout.createLayout(lay, 600);
  viewport->getViewedComponent()->setSize(layout.getWidth(), layout.getHeight());
  viewport->setViewPosition(0, 0);
  viewport->repaint();
}

struct FOMUSModsView:public FOMUSDocView
{
  FOMUSModsView() : FOMUSDocView() {}
  void doupdate(const juce::String& sch, const int but);
};

void FOMUSModsView::doupdate(const juce::String& sch, const int but) 
{
  struct info_modsearch s = {0, 0, 0, 0};
  switch (but)
  {
  case 0: s.name = s.longname = sch.toUTF8(); break;
  case 1: s.doc = sch.toUTF8();
  }
  struct info_modlist lst(fapi_info_list_modules(0, &s, 0, 0));
  juce::TextLayout& layout = ((FOMUSViewComponent*)(viewport->getViewedComponent()))->lay;
  juce::AttributedString lay;
  juce::Font no(13);
  juce::Font ul(13, juce::Font::underlined);
  juce::Font bo(13, juce::Font::bold);
  juce::Font it(13, juce::Font::italic);
  for (struct info_module* i(lst.mods), *ie(lst.mods + lst.n); i < ie; ++i)
  {
    lay.append("name:", ul);
    lay.append(" ", no);
    lay.append(i->name, bo);
    lay.append("  ", no);
    lay.append("long name:", ul);
    lay.append(" \"", no);
    lay.append(i->longname, no);
    lay.append("\"  ", no);
    lay.append("author:", ul);
    lay.append(" ", no);
    lay.append(i->author, no);
    lay.append("  ", no);
    juce::String ty(fapi_modtype_to_str(i->type));
    lay.append("type:", ul);
    lay.append(" ", no);
    lay.append(ty, no);
    lay.append("\n", no);
    lay.append("directory:", ul);
    lay.append(" ", no);
    lay.append(i->filename, no); 
    lay.append("\n", no);
    std::ostringstream ou;
    out_justify(ou, i->doc, 0, false);
    lay.append(ou.str().c_str(), it); 
    lay.append("\n\n", no);
  }
  layout.createLayout(lay, 1000);
  //  lay.layout(std::numeric_limits<int>::max() / 2, juce::Justification::topLeft, false);
  viewport->getViewedComponent()->setSize(layout.getWidth(), layout.getHeight());
  viewport->setViewPosition(0, 0);
  viewport->repaint();
}

class FOMUSDocTabs;
struct MyTabbedComponent:public juce::TabbedComponent
{
  FOMUSDocTabs& tt;
  MyTabbedComponent(FOMUSDocTabs& tt, const juce::TabbedButtonBar::Orientation orientation)
    : juce::TabbedComponent(orientation),
      tt(tt)
  {}
  void currentTabChanged(int newCurrentTabIndex, const juce::String &newCurrentTabName);
};

class FOMUSDocTabs : public juce::Component,
		     public juce::ButtonListener, 
		     public juce::TextEditorListener,
		     public juce::Timer
{
  int thebut;

public:
  FOMUSDocTabs();
  ~FOMUSDocTabs();
  void resized();
  void buttonClicked (juce::Button* buttonThatWasClicked);
  juce_UseDebuggingNewOperator

private:
  MyTabbedComponent* Tabs;
  juce::Label* SearchLabel;
  juce::TextEditor* textEditor;
  juce::ToggleButton* SearchName;
  juce::ToggleButton* SearchDoc;
  FOMUSDocTabs(const FOMUSDocTabs&);
  const FOMUSDocTabs& operator=(const FOMUSDocTabs&);

public:
  void updatedocs()
  {
    FOMUSTabbedComp* x = (FOMUSTabbedComp*)Tabs->getCurrentContentComponent();
    if (x)
    {
      x->doupdate(textEditor->getText(), thebut);
    }
  }

private:
  void textEditorTextChanged (juce::TextEditor &editor)
  {
    stopTimer();
    startTimer(500);
  }
  void textEditorReturnKeyPressed (juce::TextEditor &editor) 
  {
    stopTimer();
    updatedocs();
  }
  void textEditorEscapeKeyPressed (juce::TextEditor &editor) {}
  void textEditorFocusLost (juce::TextEditor &editor)
  {
    stopTimer();
    updatedocs();
  }
  void timerCallback ()
  {
    stopTimer();
    updatedocs();
  }
};

inline void MyTabbedComponent::currentTabChanged(const int newCurrentTabIndex, 
						 const juce::String &newCurrentTabName) 
{
  tt.updatedocs();
}  

FOMUSDocTabs::FOMUSDocTabs() 
  : juce::Component("FOMUS Documentation"),
    thebut(0),
    Tabs(0),
    SearchLabel(0),
    textEditor(0),
    SearchName(0),
    SearchDoc(0)
{
  addAndMakeVisible (textEditor = new juce::TextEditor ("new text editor"));
  textEditor->setMultiLine (false);
  textEditor->setReturnKeyStartsNewLine (false);
  textEditor->setReadOnly (false);
  textEditor->setScrollbarsShown (false);
  textEditor->setCaretVisible (true);
  textEditor->setPopupMenuEnabled (true);
  textEditor->setText (juce::String());
  
  addAndMakeVisible(Tabs = new MyTabbedComponent (*this, juce::TabbedButtonBar::TabsAtTop));
  Tabs->setTabBarDepth (30);
  Tabs->addTab ("Settings", juce::Colours::white, new FOMUSSetsView(*textEditor, thebut), true);
  Tabs->addTab ("Marks", juce::Colours::white, new FOMUSMarksView(), true);
  Tabs->addTab ("Modules", juce::Colours::white, new FOMUSModsView(), true);
  Tabs->setCurrentTabIndex (0);
  textEditor->addListener(this);
  
  addAndMakeVisible (SearchLabel = new juce::Label ("Search Label",
                                                    "Search:"));
  SearchLabel->setFont (juce::Font (13.0000f, juce::Font::plain));
  SearchLabel->setJustificationType (juce::Justification::centredLeft);
  SearchLabel->setEditable (false, false, false);
  SearchLabel->setColour (juce::TextEditor::textColourId, juce::Colours::black);
  SearchLabel->setColour (juce::TextEditor::backgroundColourId, juce::Colour (0x0));
  
  addAndMakeVisible (SearchName = new juce::ToggleButton ("Search Name"));
  SearchName->setButtonText ("Name");
  SearchName->setRadioGroupId (1);
  SearchName->addListener (this);
  SearchName->setToggleState (true, juce::dontSendNotification);
  
  addAndMakeVisible (SearchDoc = new juce::ToggleButton ("Search Doc"));
  SearchDoc->setButtonText ("Doc Text");
  SearchDoc->setRadioGroupId (1);
  SearchDoc->addListener (this);
  
  setSize (900, 600);
}

FOMUSDocTabs::~FOMUSDocTabs() 
{
  deleteAndZero (Tabs);
  deleteAndZero (SearchLabel);
  deleteAndZero (textEditor);
  deleteAndZero (SearchName);
  deleteAndZero (SearchDoc);
}

void FOMUSDocTabs::resized() 
{
  Tabs->setBounds (0, 0, getWidth() - 0, getHeight() - 39);
  SearchLabel->setBounds (8, getHeight() - 26, 56, 16);
  textEditor->setBounds (56, getHeight() - 31, getWidth() - 200, 21);
  SearchName->setBounds (getWidth() - 142, getHeight() - 26, 48, 16);
  SearchDoc->setBounds (getWidth() - 90, getHeight() - 26, 80, 16);
}

void FOMUSDocTabs::buttonClicked(juce::Button* buttonThatWasClicked) 
{
  if (buttonThatWasClicked == SearchName)
  {
    thebut = 0;
  }
  else if (buttonThatWasClicked == SearchDoc) 
  {
    thebut = 1;
  }
  updatedocs();
}

void Fomus::documentationWindow() 
{
  FOMUSDocTabs* t=new FOMUSDocTabs();
  t->setVisible(true);
  FomusDialogWindow* w = new FomusDialogWindow("FOMUS Documentation");
  //  w->setContentComponent(t, true, true);
  w->setContentOwned(t,true);
  w->setVisible(true);
  w->centreWithSize(t->getWidth(),t->getHeight());
  w->toFront(true);
}

void Fomus::loadScoreDialog() 
{
  juce::FileChooser choose("Load Score", juce::File::getCurrentWorkingDirectory(), "*.fms");  
  if (choose.browseForFileToOpen())
  {
    loadScore(choose.getResult().getFullPathName());
  }
}

void Fomus::renameScoreDialog() 
{
  juce::FileChooser choose("Rename Score", juce::File::getCurrentWorkingDirectory(), getwildcards());
  if (choose.browseForFileToSave(true))
  {
    juce::File fn(choose.getResult());
    juce::String fn0(fn.getFileName());
    scores.getUnchecked(current)->name = (fn0.isEmpty() ? "(untitled)" : fn0);
    sval(fomus_par_setting, fomus_act_set, "filename");
    sval(fomus_par_settingval, fomus_act_set, fn.getFullPathName());
  }
}
