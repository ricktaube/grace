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

/** An audio control component. When the user clicks on a Transport's
    buttons or moves its sliders the Transport::Listener's methods are
    triggered to effect playback changes.  A Transport provides 5
    buttons: Rewind, Back, Play/Pause, Forward, GoToEnd, a slider for
    scrolling through the playback sequence and an optional tempo
    slider with associated bpm label.  A Transport can be triggered
    from the main message thread by calling its underlying
    setPlaying(), setPausing(), setPlaybackPosition() and setTempo()
    methods.  To trigger transport outside of the main thread use its
    asynchronous sendMessage() method. **/

class Transport : public juce::Component, 
                  public juce::AsyncUpdater,
public juce::Button::Listener,
public juce::Slider::Listener
{
  bool playing;
  juce::Colour drawingColor;
  juce::Colour buttonBackgroundColor;
  juce::DrawableButton* buttonPlayPause;
  juce::DrawableButton* buttonRewind;
  juce::DrawableButton* buttonBack;
  juce::DrawableButton* buttonForward;
  juce::DrawableButton* buttonGoToEnd;
  juce::DrawableButton* buttonMinGain;
  juce::DrawableButton* buttonMaxGain;
  juce::Slider*         sliderPosition;
  juce::Slider*         sliderGain;
  juce::Slider*         sliderTempo;
  juce::Label*          labelStartTime;
  juce::Label*          labelEndTime;
  // Transport component geometry.
  static const int PlayButtonWidth = 54;
  static const int PlayButtonHeight = 36;
  static const int AuxButtonWidth = 36;
  static const int AuxButtonHeight = 24;
  static const int TimeLabelWidth = 40;
  static const int TimeLabelHeight = 24;
  static const int TempoSliderWidth = 78;
  static const int TempoSliderHeight = 24;
  static const int GainButtonWidth = 24;
  static const int GainButtonHeight = 24;
  static const int Margin = 0;

public:

  /// Message ids to control transport from other threads, see
  /// sendMessage()
  enum TransportMessageIds
    {
      SetPlaying,
      SetPausing,
      SetPlaybackPosition,
      MoveToFront,
      MoveToEnd,
      MoveForward,
      MoveBackward,
      SetGain,
      SetTempo
    };

  /** Receives callbacks from a Transport. **/
  struct Listener
  {
    virtual ~Listener() {}

    ///Called when the Play button is pressed.
    virtual void play(double position) = 0;

    ///Called when the Pause button is pressed.
    virtual void pause(void) = 0;

    /**Called when position slider is moved. isPlaying is true if the
       transport is currently playing.*/
    virtual void positionChanged(double position, bool isPlaying) = 0;

    /**Called when the tempo slider is moved. Tempo is the new tempo.
       isPlaying is true if the transport is currently playing.*/
    virtual void tempoChanged(double tempo, bool isPlaying) = 0;

    /**Called when the gain slider is moved. Gain is the new gain,
       isPlaying is true if the transport is currently playing.*/
    virtual void gainChanged(double gain, bool isPlaying) = 0;
 };
  

  /// Constructor.
  Transport (Listener* transportListener, bool gainSlider = true, bool tempoSlider = false)
    : playing(false),
      drawingColor (juce::Colours::black), //juce::Colour(90,90,120)
      buttonBackgroundColor (juce::Colours::transparentWhite),
      buttonPlayPause(0),
      buttonRewind(0),
      buttonBack(0),
      buttonForward(0),
      buttonGoToEnd(0),
      buttonMinGain(0),
      buttonMaxGain(0),
      sliderPosition(0),
      sliderGain(0),
      sliderTempo(0),
      labelStartTime(0),
      labelEndTime(0),
      listener(transportListener)
  {
    drawingColor =   juce::LookAndFeel::getDefaultLookAndFeel().findColour(juce::TextButton::textColourOnId);
    setVisible(true);
    int cbs = juce::Button::ConnectedOnLeft | juce::Button::ConnectedOnRight;
    juce::DrawableButton::ButtonStyle bs = juce::DrawableButton::ImageFitted;  

    addAndMakeVisible(buttonRewind = new juce::DrawableButton("Rewind", bs));
    buttonRewind->setColour(juce::DrawableButton::backgroundColourId, buttonBackgroundColor);
    buttonRewind->setConnectedEdges(juce::Button::ConnectedOnRight);
    drawRewind(buttonRewind);
    buttonRewind->addListener(this);
	
    addAndMakeVisible(buttonBack = new juce::DrawableButton("Back", bs));
    buttonBack->setColour(juce::DrawableButton::backgroundColourId, buttonBackgroundColor);
    buttonBack->setConnectedEdges(cbs);
    drawBack(buttonBack);
    buttonBack->addListener(this);
	
    addAndMakeVisible(buttonPlayPause = new juce::DrawableButton("PlayPause", bs));
    buttonPlayPause->setColour(juce::DrawableButton::backgroundColourId, buttonBackgroundColor);
    buttonPlayPause->setConnectedEdges(cbs);
    drawPlay(buttonPlayPause);
    buttonPlayPause->addListener(this);
	
    addAndMakeVisible(buttonForward = new juce::DrawableButton("Forward", bs));
    buttonForward->setColour(juce::DrawableButton::backgroundColourId, buttonBackgroundColor);
    buttonForward->setConnectedEdges(cbs);
    drawForward(buttonForward);
    buttonForward->addListener(this);

    addAndMakeVisible(buttonGoToEnd = new juce::DrawableButton("GoToEnd", bs));
    buttonGoToEnd->setColour(juce::DrawableButton::backgroundColourId, buttonBackgroundColor);
    buttonGoToEnd->setConnectedEdges(juce::Button::ConnectedOnLeft);
    drawGoToEnd(buttonGoToEnd);
    buttonGoToEnd->addListener(this);
	
    addAndMakeVisible(sliderPosition = new juce::Slider(juce::Slider::LinearHorizontal, juce::Slider::NoTextBox));    
    sliderPosition->setRange(0.0, 1.0);
    sliderPosition->addListener(this);

    addAndMakeVisible(labelStartTime = new juce::Label());
    labelStartTime->setJustificationType(juce::Justification(juce::Justification::centredRight));
    labelStartTime->setFont(juce::Font(12.0));

    addAndMakeVisible(labelEndTime = new juce::Label());
    labelEndTime->setJustificationType(juce::Justification(juce::Justification::centredLeft));
    labelEndTime->setFont(juce::Font(12.0));
    
    if (gainSlider)
    {
      addAndMakeVisible(buttonMinGain = new juce::DrawableButton("GainMin", bs));
      drawGain(buttonMinGain, 0.25); // .25 shows one small wave
      buttonMinGain->addListener(this);

      addAndMakeVisible(buttonMaxGain = new juce::DrawableButton("GainMax", bs));
      drawGain(buttonMaxGain, 1.0);
      buttonMaxGain->addListener(this);

      addAndMakeVisible(sliderGain = new juce::Slider(juce::String()));
      sliderGain->setRange(0, 1, 0);
      sliderGain->setValue(1.0, juce::dontSendNotification);
      sliderGain->setSliderStyle(juce::Slider::LinearHorizontal);
      sliderGain->setTextBoxStyle(juce::Slider::NoTextBox, false, 0, 0);
      sliderGain->addListener(this);
    }
    if (tempoSlider)
    {
      addAndMakeVisible(sliderTempo = new juce::Slider(juce::Slider::IncDecButtons, juce::Slider::TextBoxRight));
      //   sliderTempo->setTextValueSuffix(" bpm"); // save extra 32px
      sliderTempo->setTextBoxStyle(juce::Slider::TextBoxRight, false, 36, TempoSliderHeight);
      sliderTempo->setRange(40, 208, 1);
      sliderTempo->setValue(60.0, juce::dontSendNotification);
      sliderTempo->addListener(this);
    }

   int width = (Margin * 2) + (TimeLabelWidth * 2) + PlayButtonWidth + (AuxButtonWidth * 4) ;
   int height = (Margin * 2) + PlayButtonHeight + (AuxButtonHeight * 2);
   setSize(width, height); // 278 84
   //   std::cout << "transport size=" << getWidth() << " " << getHeight() << "\n";
  }
  
  /// Destructor.
  virtual ~Transport()
  {
    if (isPlaying())
    {
      setPausing();
    }
    deleteAllChildren();
  }

  /// Returns true if the transport is playing otherwise false. 
  bool isPlaying()
  {
    return playing;
  }

  /**Puts the transport in play state and calls its play() method if
     triggerAction is true. See sendMessage() for a thread-safe
     version.*/
  void setPlaying(bool triggerAction=true)
  {
    // change is true if we really flip state
    bool change = (playing == false); 
    playing = true;
    // when the transport is playing it displays the pause icon
    drawPause(buttonPlayPause);
    if (triggerAction && change)
      listener->play(sliderPosition->getValue());
  }
  
  /**Puts the transport in pause state and calls the pause() method if
     triggerAction is true. See sendMessage() for a thread-safe
     version.*/
  void setPausing(bool triggerAction=true)
  { 
    bool change = (playing == true);
    playing = false;
    drawPlay(buttonPlayPause);
    if (triggerAction && change)
      listener->pause();
  }

  /**Sets the miniumn and maximum value of the position
     slider. Defaults to 0 and 1.*/
  void setPlaybackRange(double start, double end, double increment = 0)
  {
    sliderPosition->setRange(start, end, increment);
    sliderPosition->setValue(start, juce::dontSendNotification);
    drawCurrentTime();
    drawEndTime();
  }

  /**Sets the playback position and updates the position label. If
     triggerAction is true then the positionChanged() callback is
     triggered. See sendMessage() for a thread-safe version.*/
  void setPlaybackPosition(double position, bool triggerAction = true)
  {
    if (sliderPosition)
    {
      juce::NotificationType notify = (triggerAction) ? juce::sendNotificationAsync : juce::dontSendNotification;
      sliderPosition->setValue(position, notify);
      //always update position label
      drawCurrentTime(); 
    }
  }

  /**Sets the current playback tempo in BPM. If triggerAction is true
     then the listener callback will be triggered. See sendMessage()
     for a thread-safe version.*/
  void setPlaybackTempo(double tempo, bool triggerAction = true)
  {
    // tempo components are optional
    if (sliderTempo)
    {
      juce::NotificationType notify = (triggerAction) ? juce::sendNotificationAsync : juce::dontSendNotification;
      sliderTempo->setValue(tempo, notify);
    }
  }

  /**Sets the playback gain to the specified value. If triggerAction
     is true then the listener callback will be triggered. See
     sendMessage() for a thread-safe version.*/
  void setPlaybackGain(double gain, bool triggerAction = true)
  {
    if (sliderGain)
    {
      juce::NotificationType notify = (triggerAction) ? juce::sendNotificationAsync : juce::dontSendNotification;
      sliderGain->setValue(gain, notify);
    }
  }

  /// Implements thread-safe transport messaging.
  void sendMessage(int id, double d = 0.0, bool b = false)
  {
    juce::ScopedLock mylock(messages.getLock());
    messages.add(new TransportMessage(id, d, b));
    triggerAsyncUpdate();
  }

  private:

  Listener* listener;   // the callback object

  struct TransportMessage
  {
    int type;
    double value; bool triggerAction;
    TransportMessage (int typ, double d1, bool b1) : type(typ), value (d1), triggerAction (b1) {} 
    ~TransportMessage() {}
  };

  juce::OwnedArray<TransportMessage, juce::CriticalSection> messages;

  void handleAsyncUpdate ()
  {
    juce::ScopedLock mylock (messages.getLock());
    int size = messages.size();
    for (int i = 0; i < size; i++)
    {
      TransportMessage* msg = messages.getUnchecked(i);
      switch (msg->type)
      {
      case SetPausing:
        //std::cout << "TransportMessage: SetPausing\n";
        setPausing(msg->triggerAction); // false means dont trigger action
        break;
      case SetPlaying:
        //std::cout << "TransportMessage: SetPlaying\n";
        setPlaying(msg->triggerAction); // false means dont trigger action
        break;
      case MoveForward:
        break;
      case MoveBackward:
        break;
      case MoveToFront:
        setPlaybackPosition(sliderPosition->getMinimum(), msg->triggerAction);
        break;
      case MoveToEnd:
        setPlaybackPosition(sliderPosition->getMaximum(), msg->triggerAction);
        break;
      case SetPlaybackPosition:
        //std::cout << "TransportMessage: SetPosition pos=" << msg->value << "\n";
        setPlaybackPosition(msg->value, msg->triggerAction);
        break;
      case SetGain:
        //std::cout << "TransportMessage: SetPosition pos=" << msg->value << "\n";
        setPlaybackGain(msg->value, msg->triggerAction);
        break;
      case SetTempo:
        //std::cout << "TransportMessage: SetPosition pos=" << msg->value << "\n";
        setPlaybackTempo(msg->value, msg->triggerAction);
        break;
      default:
        break;
      }
    }
    if (size > 0)
      messages.clear();
  }

  void resized()
  {
    int x = Margin;
    int y = Margin;
    int w = PlayButtonWidth + (AuxButtonWidth * 4);
    int h = (PlayButtonHeight - AuxButtonHeight) / 2 ;
    // Line 1: transport buttons centered in view
    x = (getWidth() - w) / 2;
    buttonRewind->setBounds(x, y + h, AuxButtonWidth, AuxButtonHeight);
    buttonBack->setBounds(buttonRewind->getRight(), y + h, AuxButtonWidth, AuxButtonHeight);      
    buttonPlayPause->setBounds(buttonBack->getRight(), y, PlayButtonWidth, PlayButtonHeight);
    buttonForward->setBounds(buttonPlayPause->getRight(), y + h, AuxButtonWidth, AuxButtonHeight);
    buttonGoToEnd->setBounds(buttonForward->getRight(), y + h, AuxButtonWidth, AuxButtonHeight);
    // Line 2: position slider justified to width of buttons
    y = buttonPlayPause->getBottom();
    if (sliderPosition)
    {
      labelStartTime->setBounds(0, y, TimeLabelWidth, TimeLabelHeight);
      labelEndTime->setBounds(getWidth() - TimeLabelWidth, y, TimeLabelWidth, TimeLabelHeight);
      x = labelStartTime->getRight();
      sliderPosition->setBounds(x, y, labelEndTime->getX() - x, AuxButtonHeight);
      y = sliderPosition->getBottom();
    }
    // Line 3: Tempo slider right justified to transport buttons
    //         Gain takes remaining space on line
    if (sliderTempo)
    {
      sliderTempo->setBounds(buttonGoToEnd->getRight() - TempoSliderWidth, y, TempoSliderWidth, TempoSliderHeight);
    }
    if (sliderGain)
    {
      buttonMinGain->setBounds(buttonRewind->getX(), y, GainButtonWidth, GainButtonHeight);
      // if tempo make 8 px space between gain and tempo displays
      x = (sliderTempo ? (sliderTempo->getX() - 8) : buttonGoToEnd->getRight()) - GainButtonWidth;
      buttonMaxGain->setBounds(x, y, GainButtonWidth, GainButtonHeight);
      // gain slider claims all space between gain buttons
      x = buttonMinGain->getRight();
      sliderGain->setBounds(x, y, buttonMaxGain->getX() - x, GainButtonHeight);
    }

  }

  /// juce callback handles button click events
  virtual void buttonClicked(juce::Button* button)
  {
    if (button == buttonPlayPause)
    {
      if (isPlaying())
        setPausing();
      else
        setPlaying();
    }
    else if (button == buttonRewind)
    {
      setPlaybackPosition(sliderPosition->getMinimum(), true);
    }
    else if (button == buttonBack)
    {
      setPlaybackPosition(sliderPosition->getValue() - sliderPosition->proportionOfLengthToValue(0.10), true);
    }
    else if (button == buttonForward)
    {
      setPlaybackPosition(sliderPosition->getValue() + sliderPosition->proportionOfLengthToValue(0.10), true);
    }
    else if (button == buttonGoToEnd)
    {
      setPlaybackPosition(sliderPosition->getMaximum(), true);
    }
    else if (button == buttonMinGain)
    {
      setPlaybackGain(sliderGain->getMinimum(), true);
    }
    else if (button == buttonMaxGain)
    {
      setPlaybackGain(sliderGain->getMaximum(), true);
    }
  }
  
  /// juce callback handles slider moved events
  virtual void sliderValueChanged(juce::Slider* slider)
  {
    if (slider == sliderPosition)
      listener->positionChanged(slider->getValue(), isPlaying());
    else if (slider == sliderTempo)
      listener->tempoChanged(slider->getValue(), isPlaying());
    else if (slider == sliderGain)
      listener->gainChanged(slider->getValue(), isPlaying());
  }
  
  void drawCurrentTime() 
  {
    juce::String t = getTimeLabel(sliderPosition->getValue());
    labelStartTime->setText(t, juce::dontSendNotification);
  }

  void drawEndTime() 
  { 
    juce::String t = getTimeLabel(sliderPosition->getMaximum());
    labelEndTime->setText(t, juce::dontSendNotification);
  }

  void drawRewind(juce::DrawableButton* b)
  {
    juce::DrawablePath imageRewind;
    juce::Path pathRewind;
    pathRewind.addRectangle(-1.8f,0,0.3f,2.0f);
    pathRewind.addTriangle(0,0, 0,2.0f, -1.2f,1.0f);
    imageRewind.setPath(pathRewind);
    juce::FillType ft (drawingColor);
    imageRewind.setFill(ft);
    b->setImages(&imageRewind);
  }

  void drawBack(juce::DrawableButton* b)
  {
    juce::DrawablePath imageBack;
    juce::Path pathBack;
    pathBack.addTriangle(0,0,0,2.0f,-1.2f,1.0f);
    pathBack.addTriangle(1.2f,0,1.2f,2.0f,0,1.0f);
    imageBack.setPath(pathBack);
    juce::FillType ft (drawingColor);
    imageBack.setFill(ft);
    b->setImages(&imageBack);
  }
  
  void drawPlay(juce::DrawableButton* b)
  {
    juce::DrawablePath imagePlay;
    juce::Path pathPlay;
    pathPlay.addTriangle(0,0,0,2.0f,1.2f,1.0f);
    imagePlay.setPath(pathPlay);
    juce::FillType ft (drawingColor);
    imagePlay.setFill(ft);
    b->setImages(&imagePlay);
  }
  
  void drawPause(juce::DrawableButton* b)
  {
    juce::DrawablePath imagePause;
    juce::Path pathPause;
    pathPause.addRectangle(0,0,0.3f,1.0f);
    pathPause.addRectangle(0.6f,0,0.3f,1.0f);
    imagePause.setPath(pathPause);
    juce::FillType ft (drawingColor);
    imagePause.setFill(ft);
    b->setImages(&imagePause);
  }
  
  void drawForward(juce::DrawableButton* b)
  {
    juce::DrawablePath imageForward;
    juce::Path pathForward;
    pathForward.addTriangle(0,0,0,2.0f,1.2f,1.0f);
    pathForward.addTriangle(-1.2f,0,-1.2f,2.0f,0,1.0f);
    imageForward.setPath(pathForward);
    juce::FillType ft (drawingColor);
    imageForward.setFill(ft);
    b->setImages(&imageForward);
  }
    
  void drawGoToEnd(juce::DrawableButton* b)
  {
    juce::DrawablePath imageGoToEnd;
    juce::Path pathGoToEnd;
    pathGoToEnd.addTriangle(-1.4f, 0, -1.4f,2.0f, -0.2f,1.0f);
    pathGoToEnd.addRectangle(0.1f, 0, 0.3f, 2.0f);
    imageGoToEnd.setPath(pathGoToEnd);
    juce::FillType ft (drawingColor);
    imageGoToEnd.setFill(ft);
    b->setImages(&imageGoToEnd);
  }

  void drawGain(juce::DrawableButton* button, double gain)
  {
    // Juce path drawing done in percentage (100x100)
    juce::DrawablePath drawable;
    juce::Path p;
    float s = 1;

    // speaker rect from 0 to 30
    p.addRectangle(s*0.f, s*30.f, s*30.f, s*35.f);
    // speaker cone from 0 to 45
    p.addTriangle(s*0.f, s*50.f, s*40.f, s*0.f, s*40.f, s*100.f);
    // waves start at x=55 spaced 15 apart
    if (gain > 0.1)
      p.addCentredArc(s*55, s*50, s*6, s*20,  0, 0, 3.14159f, true);
    if (gain > 0.4)
      p.addCentredArc(s*70, s*50, s*5, s*35,  0, 0, 3.14159f, true);
    if (gain > 0.7)
      p.addCentredArc(s*85, s*50, s*5, s*50,  0, 0, 3.14159f, true);

    drawable.setPath(p);
    drawable.setFill(drawingColor);
    button->setImages(&drawable);
  }

  static const juce::String getTimeLabel(double seconds)
  {
    div_t d = div(juce::roundToIntAccurate(seconds), 60);
    juce::String t;
    if (d.quot < 10) t << "0";
    t << d.quot << ":";
    if (d.rem < 10) t << "0";
    t << d.rem;
    return t;
  }
};
