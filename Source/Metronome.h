/*
 *  Metronome.h
 *  MyApp
 *
 *  Created by Halim Beere on 11/2/11.
 *  Copyright 2011 University of Illinois at Urbana-Champaign. All rights reserved.
 *
 */

#ifndef CM_METRONOME_H
#define CM_METRONOME_H

#include "Libraries.h"

struct Metronome
{
  int identifier;
  double beatOffset;
  double preventChangesBeforeTime;
  juce::Array<double, juce::CriticalSection> bpmGraph;
  
  bool floatCompare(double a, double b);
  
  int bpmGraphLength();
    
  /*Give the "point" on the graph and return the tempo of that point. Returns a
    negative number if the point doesn't lie on the graph.*/
  double getTempoPoint(int point);
  
  /*Give the "point" on the graph and return the time of that point. Returns a
    negative number if the point doesn't lie on the graph.*/
  double getTimePoint(int point);
  
  int getPointIndex(int point);
  
  bool removePoint(int point);
  
  void addPoint(double x, double y, bool insertBeforeEqualPoints = true);
  
  double getBeatAtTime(double time);
  
  double getTimeAtBeat(double beat);
  
  double getTimeDeltaToBeat(double beat);
  
  double getNextBeat(double division = 1);
  
  double getNowBeat(void);
    
  double getTimeDeltaBeatsLater(double time, double beatsLater);
  
  double getTimeToNextBeat(double division = 1);
      
  double integrateGraph(double nowVal);
  
  double getNowTime(void);
  
  double getNowTempo(void);
  
  double getTempoFromTime(double time);
  
  void collapseBeatGraph(void);
  
  void addPointNow(void);
  
  void removeDuplicatePoints(void);
    
  void setFutureTempo(double tempo, double time, bool issec = true);
  
  /*This is take a time and force the graph slower or faster such that a beat
    will align with the time given.*/
  bool adjustBeatToTime(double targetTime, double targetTempo, int mode = 0); //-1 slower, 0 smart, 1 faster
    
  bool phase(double fitBeats, double beatSpace);
  
  void reset(double tempo);

  void printGraph(void);
  
  Metronome(double curtime, double tempo);
  Metronome(double tempo);
  Metronome();
};
#endif
