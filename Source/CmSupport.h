/*=======================================================================*
  Copyright (C) 2012 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#pragma once

#include "Scheme.h"

void cm_quit();
void cm_begin_hook(s7_scheme* sc, bool *result);

void cm_print_output(char* str);
void cm_print_error(char* str);
void cm_print_warning(char* str);
void cm_print_values(char* str);
void cm_print_stdout(char* str);
s7_pointer cm_print(s7_pointer ptr);
char* cm_pattern_print(s7_scheme *sc, void *val);
char* cm_spectrum_print(s7_scheme *sc, void *val);

int cm_string_hash(char* str);

//void cm_buffering_stdout( char c);
//void cm_buffering_stderr( char c);

void cm_shell(char* str);
void cm_play(char* str);
void cm_load(char* path);
void cm_edit(char* path);

#ifdef _MSC_VER
int round(double n);
#endif

double cm_rescale(double x, double x1, double x2, double y1, double y2, double b1) ;
int cm_discrete(double x, double x1, double x2, int y1, int y2, double b=1.0);
int cm_float_to_fixnum(double f) ;
double cm_log_ten(double num);
double cm_log_two(double num);

double cm_expl(double powr, double y0, double y1, double base);
double cm_explseg(int i, int len, double sum, double base);
double cm_geoseg(int i, int len, double sum, double base);

double cm_decimals(double val, int places); //places=3
double cm_quantize(double val, double step);
//tempo=60 beat=.25
double cm_rhythm_to_seconds(double beats, double tempo, double beat);
double cm_cents_to_scaler(double cents);
double cm_scaler_to_cents(double scaler);
double cm_scaler_to_steps(double scaler);
double cm_keynum_to_hertz (double kn);
int cm_keynum_to_pc (double kn);
double cm_hertz_to_keynum (double hz);

//void cm_ranseed(juce::int64 s);
void cm_init_randomness(s7_scheme* sc);
s7_pointer cm_get_random_seed();
void cm_set_random_seed(s7_pointer args);

int cm_ranint(int i);
double cm_ranfloat(double f);
int cm_ranint2(int i1, int i2);
double cm_ranfloat2(double f1, double f2);

double cm_ranlow();
double cm_ranhigh();
double cm_ranmiddle();
double cm_ranbeta(double a5, double b); //a=b=.5
double cm_ranexp(double lambda); //lambda=1
double cm_ranexp2(double lambda);
double cm_rangauss(double sigma, double mu); // sigma=1 mu=0
double cm_rancauchy();
int cm_ranpoisson(double lambda);
double cm_rangamma(double k); // k=1
double cm_ranpink();
double cm_ranbrown();
double cm_bes_jn(int a, double b);

// scheduling

bool cm_sched_metro_id_valid(int metroId);
int cm_sched_make_metro(double tempo);
void cm_sched_delete_metro(int id);
s7_pointer cm_sched_get_metros(bool user);
void cm_sched_set_tempo(double tempo, double time, bool issecs, int metroId);
double cm_sched_get_metro_beat(int metroId);
double cm_sched_get_metro_tempo(int metroId);
bool cm_sched_sync_metros(int metroId, double beatsAhead, int masterMetroId, double tempo, bool isbeats, int mode);
bool cm_sched_metro_phase(double fitBeats, double beatSpace, int metroId);
double cm_sched_metro_dur(double beats, int metroId);

double cm_now();
void cm_sched_sprout(s7_pointer proc, double time, int id, int metroId = 0, double startBeat = 0);
bool cm_sched_paused_p();
void cm_sched_pause();
void cm_sched_continue();
void cm_sched_stop(int id = 0);
void cm_sched_stop_all();
void cm_sched_hush();
bool cm_sched_busy_p();
bool cm_sched_score_mode_p();
int  cm_sched_get_score_mode();
void cm_sched_set_score_mode(int val);
double cm_sched_score_time();

// file system and pathname support

char* cm_user_home_directory();
char* cm_temp_directory();
char* cm_current_directory();
bool cm_set_current_directory (char *path);
char* cm_pathname_directory(char* path);
char* cm_pathname_name(char* path);
char* cm_pathname_type(char* path);
char* cm_full_pathname(char* path);
bool cm_pathname_exists_p(char* path);
bool cm_pathname_writable_p(char* path);
bool cm_pathname_directory_p(char* path);
int cm_pathname_to_key(char* path);
int cm_insure_new_file_version(juce::String pathname, int vers);
s7_pointer cm_directory(char* path, bool recurse=true);

s7_pointer cm_midifile_import(char* path, int track, s7_pointer midivalues);
s7_pointer cm_midifile_header(char* path, s7_pointer midivalues);

// sal support

//char* sal_tokenize(char* str);
s7_pointer sal_allocate_tokens();
s7_pointer sal_free_tokens(s7_pointer ptr);
s7_pointer sal_tokenize_file(s7_pointer fil, s7_pointer ptr, s7_pointer sal2);
s7_pointer sal_tokenize_string(s7_pointer fil, s7_pointer ptr, s7_pointer sal2);
s7_pointer sal_token_type(s7_pointer ptr);
s7_pointer sal_token_string(s7_pointer ptr);
s7_pointer sal_token_position(s7_pointer ptr);
s7_pointer sal_load(char* str);
s7_pointer sal_eval(juce::String str, int typ);

// mouse

double cm_mouse_x(double minval=0.0, double maxval=1.0, double warp=1.0);
double cm_mouse_y(double minval=0.0, double maxval=1.0, double warp=1.0);
s7_pointer cm_mouse_button(s7_pointer upval, s7_pointer downval);

// midi ports

char* cm_port_info();

juce::File completeFile(juce::String path);
bool mp_open_output(int dev);
bool mp_open_input(int dev);
void mp_close_output(int dev);
void mp_close_input(int dev);

void mp_open_score(char* file, s7_pointer args);
void mp_close_score();

void mp_send_note(s7_pointer time, s7_pointer dur, s7_pointer key, s7_pointer vel, s7_pointer chan);
void mp_send_data(int type, double time, double chan, double data1, double data2);

void mp_set_tuning(int div);
void mp_set_instruments(s7_pointer list);
void mp_set_channel_mask(int m);
void mp_set_message_mask(int m);

bool mp_set_midi_hook(int op, s7_pointer proc);
s7_pointer mp_is_midi_hook(int op);

// csound

void cs_open_score(char* args);
void cs_close_score();
void cs_send_score(int typ, int inst, double time, char* pars);

// fomus

void fms_open_score(bool);
void fms_close_score();
void fms_save(const char* name);
void fms_save_as(const char* name);
/* void fms_init(); */
int fms_isfiletype(const char* ext);
void fms_new(const char* name);
void fms_select(const char* name);
void fms_merge(const char* name, long num, long den, double flt);
void fms_free();
void fms_clear(bool all);
void fms_ival(int par, int act, long val);
void fms_rval(int par, int act, long num, long den);
void fms_fval(int par, int act, double val);
void fms_sval(int par, int act, const char* val);
void fms_act(int par, int act);
void fms_load(char* filename);
void fms_run();
void fms_err();

// Plotting

void plot_xml(char* str);
void plot_add_xml_points(char* title, char* points);
char* plot_data(char* window, int layer);

// Cell Window

//void cw_open(char* title, char* statesandcolors, int rows, int cols, int cellsize, int bordersize);

bool sw_open_from_xml(char* xml);
void sw_draw(char* window, s7_pointer arry, int data1, int data2);

// OSC
bool osc_open_input(int port);
bool osc_close_input();
bool osc_open_output(char* host, int port);
bool osc_close_output();
void osc_send_message(char* oscpath, s7_pointer list);
void osc_send_bundle(double time, s7_pointer list);
s7_pointer osc_set_hook(char* oscpath, s7_pointer proc);
s7_pointer osc_is_hook(char* oscpath);

// SDIF

s7_pointer sdif_import(char* file,  s7_pointer args);
