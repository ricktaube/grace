/*
 *  Metronome.cpp
 *  MyApp
 *
 *  Created by Halim Beere on 11/2/11.
 *  Copyright 2011 University of Illinois at Urbana-Champaign. All rights reserved.
 *
 */

#include "Metronome.h"

bool Metronome::floatCompare(double a, double b)
{
  double diff = a - b;
  if(diff < 0)
    diff = diff * -1;
  if(a == b || diff < 0.001)
    return true;
  return false;
}

int Metronome::bpmGraphLength()
{
  return bpmGraph.size() / 2;
}

double Metronome::getTempoPoint(int point)
{
  if(point < 0 || point > bpmGraphLength())
    return -1.0;
  int index = (point - 1) * 2 + 1;
  return bpmGraph[index];
}

double Metronome::getTimePoint(int point)
{
  if(point < 0 || point > bpmGraphLength())
    return -1.0;
  int index = (point - 1) * 2;
  return bpmGraph[index];
}

int Metronome::getPointIndex(int point)
{
  //Note, if the point is invalid, the index will be invalid.
  return (point - 1) * 2;
}

bool Metronome::removePoint(int point)
{
  if(point < 0 || point > bpmGraphLength())
    return false;
  int index = (point - 1) * 2;
  bpmGraph.remove(index + 1);
  bpmGraph.remove(index);
  return true;
}

void Metronome::addPoint(double x, double y, bool insertBeforeEqualPoints)
{
  for(int i = bpmGraphLength(); i >= 1; i--)
  {
    if(insertBeforeEqualPoints)
    {
      if(getTimePoint(i) < x)
      {
        int index = getPointIndex(i + 1);
        bpmGraph.insert(index, x);
        bpmGraph.insert(index + 1, y);
        return;
      }
    }
    else
    {
      if(getTimePoint(i) <= x)
      {
        int index = getPointIndex(i + 1);
        bpmGraph.insert(index, x);
        bpmGraph.insert(index + 1, y);
        return;
      }
    }
  }
}

double Metronome::getBeatAtTime(double time)
{
  return integrateGraph(time);
}

double Metronome::getTimeAtBeat(double beat)
{
  double currentBeat = getNowBeat();
  double currentTime = getNowTime();
  double beatDelta = beat - currentBeat;
  double timeDelta = getTimeDeltaBeatsLater(currentTime, beatDelta);
  return currentTime + timeDelta;  
}

double Metronome::getTimeDeltaToBeat(double beat)
{
  double currentBeat = getNowBeat();
  double currentTime = getNowTime();
  double beatDelta = beat - currentBeat;
  double timeDelta = getTimeDeltaBeatsLater(currentTime, beatDelta);
  return timeDelta;  
}

double Metronome::getTimeToNextBeat(double division)
{
  double nextBeat = getNextBeat(division);
  double currentTime = getNowTime();
  double timeAtNextBeat = getTimeAtBeat(nextBeat);
  return timeAtNextBeat - currentTime;
}

double Metronome::getNextBeat(double division)
{
  double currentBeat = getNowBeat() / division;
  return ceil(currentBeat) * division;
}

double Metronome::getNowBeat()
{
  double time = getNowTime();
  return integrateGraph(time);
}

double Metronome::getTimeDeltaBeatsLater(double startTime, double beatsLater)
{

  //First determine if "time" lies beyond the end of the graph,
  int numberOfPoints = bpmGraphLength();
  if(startTime >= getTimePoint(numberOfPoints)) //lies beyond . . . 
    return (60 * beatsLater) / getTempoPoint(numberOfPoints);
  /*The graph has seconds on the x axis, but beats per minute on the y axis.
    multiplying the "beatsLater" variable by 60 allows the integration to come out
    right.  Otherwise there's an error of a factor of 60 (as in secs/min).*/
  beatsLater = beatsLater * 60;
  
  double x0 = 0, y0 = 0, x1 = 0, y1 = 0;
  double gx0 = 0, gy0 = 0, gx1 = 0, gy1 = 0;
  double slope = 0;
  
  if(startTime < getTimePoint(1)) //lies before . . . 
  {
    slope = 0;
    gx1 = getTimePoint(1);
    gy1 = getTempoPoint(1);
  }
  else
  {
    for(int i = bpmGraphLength(); i >= 2; i--)
    {
      if(startTime <= getTimePoint(i) && startTime >= getTimePoint(i - 1)) //time is between the points
      { //calculate the slope of the line found.
        gx0 = getTimePoint(i - 1);
        gy0 = getTempoPoint(i - 1);
        gx1 = getTimePoint(i);
        gy1 = getTempoPoint(i);
        slope = (gy1 - gy0) / (gx1 - gx0);
        break;
      }
    }
  }
  //Calculate the time and tempo of the target point (some number of beats later).
  x0 = startTime;
  y0 = getTempoFromTime(startTime);
  double temp_y1; //must keep this variable in scope.
  
  if(slope == 0)
  {
    y1 = y0;
    x1 = (beatsLater / y0) + x0;
  }
  else
  {
    double yIntercept = y0 - (slope * x0);
    temp_y1 = ((2 * slope * beatsLater) + (y0 * y0));
    if(temp_y1 >= 0)
    {
      y1 = sqrt(temp_y1);
      x1 = (y1 - yIntercept) / slope;
    }
  }
    
  double result;
  if(x1 > gx1 || temp_y1 < 0) 
  {
    double beatsFulfilled = (gx1 - x0) * (gy1 + y0) / 2;
    //recursively calls itself if the bpmGraph has extra points under the area in question
    double extratime = getTimeDeltaBeatsLater(gx1, (beatsLater - beatsFulfilled) / 60);
    result = (gx1 - x0) + extratime;
  }
  else
    result = x1 - x0;
  return result;
}

double Metronome::getNowTime(void)
{
  return juce::Time::getMillisecondCounterHiRes() / 1000.0;
}

double Metronome::getNowTempo()
{
  //  std::cout << "*** get now tempo" << std::endl;
  printGraph();
  //  std::cout << "*** Tempo graph printed above" << std::endl;
  double time = getNowTime();
  return getTempoFromTime(time);
}

void Metronome::collapseBeatGraph()
{
  addPointNow();
  double nowTime = getNowTime();
  double nowTempo = getTempoFromTime(nowTime);
  beatOffset = getNowBeat();
  
  int i = bpmGraph.size() - 2; //keep i in scope
  for(; i >= 0; i = i - 2)
  {
    if(bpmGraph[i] < nowTime)
    {
      bpmGraph.set(i + 1, nowTempo);
      bpmGraph.set(i, nowTime);
      break;
    }
  }
  for(i--; i >= 0; i--)
    bpmGraph.remove(i);
}

void Metronome::addPointNow()
{
  double x = getNowTime();
  double y = getTempoFromTime(x);
  for(int i = bpmGraph.size() - 2; i >= 0; i = i - 2)
  {
    if(bpmGraph[i] < x)
    {
      bpmGraph.insert(i + 2, x);
      bpmGraph.insert(i + 3, y);
      break;
    }
  }
}

void Metronome::removeDuplicatePoints()
{
  if(bpmGraphLength() < 2)
    return;
  for(int i = 1; i < bpmGraphLength(); i++)
  {
    if(floatCompare(getTimePoint(i), getTimePoint(i + 1)) && bpmGraphLength() >= 2)
    {
      removePoint(i + 1);
      i = 1;
    }
  }
  
  if(bpmGraph.size() < 6)
    return;
  double a, b, c;
  
  for(int i = 5; i < bpmGraph.size(); i = i + 2)
  {
    a = bpmGraph[i - 4];
    b = bpmGraph[i - 2];
    c = bpmGraph[i];
    if(floatCompare(a, b) && floatCompare(b, c))
    {
      bpmGraph.remove(i - 2);
      bpmGraph.remove(i - 3);
      i = 3;//Will be incremented to equal 5 on the next iteration
    }
  }
}

double Metronome::integrateGraph(double nowVal)
{
  double x1, x2;
  double y1, y2;
  double beatTotal = beatOffset;
  int numberOfPoints = (int)(bpmGraph.size() * .5);
  int currentPoint = 1;
  int curIndex = 0;
  while (currentPoint < numberOfPoints && nowVal > bpmGraph[curIndex + 2])
  {
    x1 = bpmGraph[curIndex];
    y1 = bpmGraph[curIndex + 1];
    x2 = bpmGraph[curIndex + 2];
    y2 = bpmGraph[curIndex + 3];
    beatTotal = beatTotal + (((x2 - x1) / 60) * ((y1 + y2) / 2));
    currentPoint = currentPoint + 1;  
    curIndex = (currentPoint - 1) * 2;    
  }
  x1 = bpmGraph[curIndex];
  y1 = bpmGraph[curIndex + 1];
  x2 = nowVal;
  y2 = getTempoFromTime(nowVal);
  beatTotal = beatTotal + (((x2 - x1) / 60) * ((y1 + y2) / 2));
  return beatTotal;
}

double Metronome::getTempoFromTime(double time)
{
  double y1 = 0, y2 = 0;
  double x1 = bpmGraph[0];
  double x2 = bpmGraph[bpmGraph.size() - 2];
  if(time <= x1)
    return bpmGraph[1];
  else if(time >= x2)
    return bpmGraph[bpmGraph.size() - 1];
  else
  {
    for(int i = 0; i < bpmGraph.size(); i = i + 2)
    {
      if(bpmGraph[i] >= time)
      {
        x1 = bpmGraph[i - 2];
        y1 = bpmGraph[i - 1];
        x2 = bpmGraph[i];
        y2 = bpmGraph[i + 1];
        break;
      }
    }
    return (((time - x1) * (y2 - y1)) / (x2 - x1)) + y1;
  }    
}

void Metronome::setFutureTempo(double tempo, double time, bool issec)
{
  /*This adds a point to the graph, maintaining the current tempo at this time,
    and removes all previous points.*/
  collapseBeatGraph();

  //Now remove all succeeding points.
  for(int i = bpmGraphLength(); i > 1; i--)
    removePoint(i);
  
  double currentTime = getNowTime();
  if(!issec)
  {
    double currentBeat = getNowBeat();
    double extra = ceil(currentBeat) - currentBeat;
    double beats = (extra + time) * 60;
    double currentTempo = getNowTempo();
    double targetTime = (beats * 2) / (tempo + currentTempo) + currentTime;
    bpmGraph.add(targetTime);
    bpmGraph.add(tempo);    
  }
  else
  {
    bpmGraph.add(currentTime + time);
    bpmGraph.add(tempo);
  }
}

bool Metronome::adjustBeatToTime(double targetTime, double targetTempo, int mode) //mode -1 slow, 0 smart, 1 fast
{
  /*Prevent calls to this function if a previous call has has not completed.
    Overlapping calls causes strange and unexpected behavior.*/
  if(getNowTime() <= preventChangesBeforeTime)
    return false;
  preventChangesBeforeTime = targetTime;
  //1 - Create a point NOW to cache tempo at current time.
  collapseBeatGraph();
  //2 - Erase any trailing points if they exist.
  double currentTime = getNowTime();
  for(int i = bpmGraphLength(); i > 1; i--)
  {
    if(getTimePoint(i) > currentTime)
      removePoint(i);
    else
      break;
  }
  //3 - Add a new point at the targetTime and targetTempo. This is our temporary graph.
  addPoint(targetTime, targetTempo);
  //4 - determine the exact beat at the target time on our new graph.
  double targetBeat = integrateGraph(targetTime);
  /*5 - Find the delta to the floor or ceiling (depending on slower or faster).
    This gives the beat amount (area) that needs to be added or subtracted.*/
  double slower = (floor(targetBeat) - targetBeat) * 60.0;
  double faster = (ceil(targetBeat) - targetBeat) * 60.0;
  double beatDelta;
  if(mode == 0)
  {
    if(fabs(slower) < fabs(faster))
      mode = -1;
    else
      mode = 1;
  }
  if(mode == -1) // slower
    beatDelta = (floor(targetBeat) - targetBeat) * 60.0;
  else // faster, mode = 1
    beatDelta = (ceil(targetBeat) - targetBeat) * 60.0;
  if(floatCompare(beatDelta, 0.0) || 
     floatCompare(targetBeat, floor(targetBeat)) || 
     floatCompare(targetBeat, ceil(targetBeat))) //Already in sync!
    return false;
  /*6 - Using the line segment between our NOW point and our TARGET point, determine
    if the slope is 0, positive, or negative.*/
  int end = bpmGraphLength();
  int start = end - 1;
  double slope = (getTempoPoint(end) - getTempoPoint(start)) / 
    (getTimePoint(end) - getTimePoint(start));
  //Create the point to add.
  double base, height;
  double newPointX, newPointY;
  //6a - If slope is 0 (line segment is flat):
  if(slope == 0)
  {
    double newPointX2, newPointY2; //in this case I make a rectangle on the graph.
    base = getTimePoint(end) - getTimePoint(start);
    height = beatDelta / base;
    newPointX = getTimePoint(start);
    newPointY = getTempoPoint(start) + height;
    newPointX2 = getTimePoint(end);
    newPointY2 = newPointY;
    addPoint(newPointX, newPointY, false);
    addPoint(newPointX2, newPointY2, true);
  }
  //6b - If slope is positive (line segment rises):
  else if(slope > 0)
  {
    height = (double)(getTempoPoint(end) - getTempoPoint(start));
    base = fabs((double)((2.0 * beatDelta) / height));
    double xDelta = getTimePoint(end) - getTimePoint(start);
    if(base > xDelta)
    {
      base = xDelta;
      height = fabs((double)((2.0 * beatDelta) / base));
      if(beatDelta < 0)
      {
        newPointX = getTimePoint(end);
        newPointY = getTempoPoint(end) - height;
        addPoint(newPointX, newPointY, true);
      }
      else
      {
        newPointX = getTimePoint(start);
        newPointY = getTempoPoint(start) + height;
        addPoint(newPointX, newPointY, false);
      }
    }
    else if(beatDelta < 0)
    {
      newPointX = getTimePoint(start) + base;
      newPointY = getTempoPoint(start);
      addPoint(newPointX, newPointY);
    }
    else
    {
      newPointX = getTimePoint(end) - base;
      newPointY = getTempoPoint(end);
      addPoint(newPointX, newPointY);
    }
  }
  //6c - If slope is negative (line segment falls):
  else if(slope < 0)
  {
    height = (double)(getTempoPoint(start) - getTempoPoint(end));
    base = fabs((double)((2.0 * beatDelta) / height));
    double xDelta = getTimePoint(end) - getTimePoint(start);
    if(base > xDelta)
    {
      base = xDelta;
      height = fabs((double)((2.0 * beatDelta) / base));
      if(beatDelta < 0)
      {
        newPointX = getTimePoint(start);
        newPointY = getTempoPoint(start) - height;
        addPoint(newPointX, newPointY, false);
      }
      else
      {
        newPointX = getTimePoint(end);
        newPointY = getTempoPoint(end) + height;
        addPoint(newPointX, newPointY, true);
      }
    }    
    else if(beatDelta < 0)
    {
      newPointX = getTimePoint(end) - base;
      newPointY = getTempoPoint(end);
      addPoint(newPointX, newPointY);
    }
    else
    {
      newPointX = getTimePoint(start) + base;
      newPointY = getTempoPoint(start);
      addPoint(newPointX, newPointY);
    }
  }
  return true;
}

bool Metronome::phase(double fitBeats, double beatSpace)
{  
  collapseBeatGraph();
  int firstBeat = (int)getNextBeat();
  /*1 - Find the next beat and create a point at that time and tempo.
    2 - Find the future target beat (the above beat plus the beatSpace) and create a point there.*/
  double firstTime = getTimeAtBeat(firstBeat);
  double firstTempo = getTempoFromTime(firstTime);
  int targetBeat = (int)(firstBeat + beatSpace);
  double targetTime = getTimeAtBeat(targetBeat);
  double targetTempo = getTempoFromTime(targetTime);
  if(getNowTime() <= preventChangesBeforeTime)//Prevent doing multiple consecutive phases, which causes strange behavior.
    return false;
  preventChangesBeforeTime = targetTime;
  addPoint(firstTime, firstTempo);
  addPoint(targetTime, targetTempo);
  /*3 - Remember that the AREA under this section of graph is = to beatSpace.
    4 - Erase any points that might lie between the above two points.*/
  bool recalculate = false;
  for(int i = bpmGraphLength(); i > 1; i--)
  {
    if(getTimePoint(i) < targetTime && getTimePoint(i - 1) > firstTime)
    {
      removePoint(i);
      recalculate = true;
    }
  }
  /*5 - Recalculate the AREA under these points now that this might have changed.  Store
    this value as our base line, as we need to add or subtract from this amount.
    6 - Find the delta between fitBeats and beatSpace. This gives the beat amount
    (area) that needs to be added or subtracted.*/
  double beatDelta = 0;
  if(recalculate)
  {
    double newBeatSpace = getBeatAtTime(targetTime) - getBeatAtTime(firstTime);
    beatDelta = (fitBeats - newBeatSpace) * 60;
  }
  else
    beatDelta = (fitBeats - beatSpace) * 60;
  /*7 - Using the line segment between our first point and our TARGET point, determine
    if the slope is 0, positive, or negative.*/
  double slope = (targetTempo - firstTempo) / (targetTime - firstTime);
  //Create the point to add.
  double base, height;
  double newPointX, newPointY;
  //If the slope is 0 (line segment is flat)
  if(slope == 0)
  {
    double newPointX2, newPointY2; //in this case I make a rectangle on the graph.
    base = targetTime - firstTime;
    height = beatDelta / base;
    newPointX = firstTime;
    newPointY = firstTempo + height;
    newPointX2 = targetTime;
    newPointY2 = newPointY;
    addPoint(newPointX, newPointY, false);
    addPoint(newPointX2, newPointY2, true);
  }
  //If the slope is positive (line segment rises)
  else if(slope > 0)
  {
    height = (double)(targetTempo - firstTempo);
    base = fabs((double)((2.0 * beatDelta) / height));
    double xDelta = targetTime - firstTime;
    if(base > xDelta)
    {
      base = xDelta;
      height = fabs((double)((2.0 * beatDelta) / height));
      if(beatDelta < 0)
      {
        newPointX = targetTime;
        newPointY = targetTempo - height;
        addPoint(newPointX, newPointY, true);
      }
      else
      {
        newPointX = firstTime;
        newPointY = firstTempo + height;
        addPoint(newPointX, newPointY, false);
      }
    }
    else if(beatDelta < 0)
    {
      newPointX = firstTime + base;
      newPointY = firstTempo;
      addPoint(newPointX, newPointY);
    }
    else
    {
      newPointX = targetTime - base;
      newPointY = targetTempo;
      addPoint(newPointX, newPointY);
    }
  }
  //If the slope is negative (line segment falls)
  else if(slope < 0)
  {
    height = (double)(firstTempo - targetTempo);
    base = fabs((double)((2 * beatDelta) / height));
    double xDelta = targetTime - firstTime;
    if(base > xDelta)
    {
      base = xDelta;
      height = fabs((double)((2.0 * beatDelta) / base));
      if(beatDelta < 0)
      {
        newPointX = firstTime;
        newPointY = firstTempo - height;
        addPoint(newPointX, newPointY, false);
      }
      else
      {
        newPointX = targetTime;
        newPointY = targetTempo + height;
        addPoint(newPointX, newPointY, true);
      }
    }
    else if(beatDelta < 0)
    {
      newPointX = targetTime - base;
      newPointY = targetTempo;
      addPoint(newPointX, newPointY);
    }
    else
    {
      newPointX = firstTime + base;
      newPointY = firstTempo;
      addPoint(newPointX, newPointY);
    }
  }
  return true;
}

void Metronome::reset(double tempo)
{
  if(tempo < 0)
    return;
  else if(tempo == 0)
    tempo = getNowTempo();
  double time = getNowTime();
  beatOffset = floor(getNowBeat());
  bpmGraph.clear();
  bpmGraph.add(time);
  bpmGraph.add(tempo);
}

void Metronome::printGraph()
{
  for(int i = 0; i < bpmGraph.size(); i = i + 2)
    std::cout << "x = " << bpmGraph[i] << " and y = " << bpmGraph[i + 1] << std::endl;
}

Metronome::Metronome(double curtime, double tempo)
  : identifier(0),
    beatOffset(0),
    preventChangesBeforeTime(0)
{
  bpmGraph.add(curtime);
  bpmGraph.add(tempo);
}

Metronome::Metronome(double tempo) 
  : identifier(0),
    beatOffset(0), 
    preventChangesBeforeTime(0)
{
  bpmGraph.add(getNowTime());
  bpmGraph.add(tempo);
}

Metronome::Metronome() 
  : identifier(0),
    beatOffset(0),
    preventChangesBeforeTime(0)
{
  bpmGraph.add(getNowTime());
  bpmGraph.add(60);
}
