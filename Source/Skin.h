/*--------------------------------------------------------------------*\
| Copyright (C) 2008 William Andrew Burnson, Rick Taube.               |
| This program is free software; you can redistribute it and/or modify |
| it under the terms of the GNU General Public License v3. The text of |
| this agreement is available at http://www.gnu.org/copyleft/gpl.html  |
\*--------------------------------------------------------------------*/

#pragma once

#include "Libraries.h"
#include "Enumerations.h"

///An attempt to make the menu system a little more like normal Windows menus.
class Skin : public juce::LookAndFeel_V2
{
public:
  using juce::LookAndFeel_V2::drawPopupMenuItem;
  juce::Font* fontMenuBar;

  Skin()
  {
    fontMenuBar = new juce::Font(16, 0);
    if(SysInfo::isMac())
      fontMenuBar->setTypefaceName("Geneva");
    else
      fontMenuBar->setTypefaceName("Tahoma");
    fontMenuBar->setHeight(16);

    setColour(juce::TextButton::buttonColourId, juce::Colours::grey);
    setColour(juce::TextButton::buttonOnColourId, juce::Colours::grey);
    setColour(juce::Slider::thumbColourId, juce::Colours::white);
    setColour(juce::Label::textColourId, juce::Colours::white);
    setColour(juce::TextButton::textColourOffId, juce::Colours::white);
    setColour(juce::TextButton::textColourOnId, juce::Colours::white);
    
    setColour(juce::ToggleButton::textColourId, juce::Colours::white);
    setColour(juce::GroupComponent::outlineColourId, juce::Colours::white);
    setColour(juce::GroupComponent::textColourId, juce::Colours::white);

    setColour(juce::ComboBox::backgroundColourId, juce::Colours::white);
    setColour(juce::ComboBox::textColourId, juce::Colours::black);
    setColour(juce::ComboBox::outlineColourId, juce::Colours::darkgrey);
    setColour(juce::ComboBox::buttonColourId, juce::Colours::darkgrey);
    setColour(juce::ComboBox::arrowColourId, juce::Colours::white);
    
    setColour(juce::ListBox::backgroundColourId, juce::Colours::white);
    setColour(juce::ListBox::textColourId, juce::Colours::black);
    setColour(juce::ListBox::outlineColourId, juce::Colours::darkgrey);


    
    setColour(juce::TabbedComponent::outlineColourId, juce::Colours::white.withAlpha(0.5f));
    setColour(juce::TabbedButtonBar::tabOutlineColourId, juce::Colours::white);
    setColour(juce::TabbedButtonBar::tabTextColourId, juce::Colours::white);
    setColour(juce::TabbedButtonBar::frontOutlineColourId, juce::Colours::white);
    setColour(juce::TabbedButtonBar::frontTextColourId, juce::Colours::white);
    
    setColour(juce::Slider::thumbColourId, juce::Colours::white);
    setColour(juce::Slider::trackColourId, juce::Colours::grey);
    setColour(juce::Slider::rotarySliderFillColourId, juce::Colours::white);
    setColour(juce::Slider::rotarySliderOutlineColourId, juce::Colours::white);
    
    setColour(juce::TextEditor::highlightColourId, juce::Colours::lightblue.withAlpha(0.3f));
    setColour(juce::CodeEditorComponent::highlightColourId, juce::Colours::lightblue.withAlpha(0.3f));
    setColour(juce::TextEditor::focusedOutlineColourId, juce::Colours::lightblue.withAlpha(0.3f));

    setColour(juce::PopupMenu::headerTextColourId, juce::Colours::white);

    setColour(juce::AlertWindow::backgroundColourId, ColorThemeIDs::getWindowBackgroundColor());
    setColour(juce::AlertWindow::textColourId, juce::Colours::white);
    setColour(juce::AlertWindow::outlineColourId, juce::Colours::lightgrey);
  }


  virtual ~Skin()
  {
    delete fontMenuBar;
  }
  
  virtual void fillTextEditorBackground(juce::Graphics& g, int width, int height,
    juce::TextEditor& textEditor)
  {
    if(textEditor.isOpaque()) textEditor.setOpaque(false);
    //g.fillAll(juce::Colour((juce::uint8)118, (juce::uint8)118,
    //  (juce::uint8)118));
    float Curve = 5.0;
    g.setColour(textEditor.findColour(juce::TextEditor::backgroundColourId)); //juce::Colours::white
    g.fillRoundedRectangle(0, 0, (float)width, (float)height, Curve);
    g.setColour(juce::Colours::lightgrey);
    g.drawRoundedRectangle(0, 0, (float)width, (float)height, Curve, 1.0);
  }
 
  virtual void drawTextEditorOutline(juce::Graphics& g, int width, int height,
    juce::TextEditor& textEditor)
  {
    if(!textEditor.hasKeyboardFocus(true))
      return;
    float Curve = 5.0;
    g.setColour(juce::Colours::lightblue.darker());
    g.drawRoundedRectangle(0.5, 0.5, (float)width - 1, (float)height - 1, Curve, 2.0);
    g.setColour(juce::Colours::lightblue);
    g.drawRoundedRectangle(0.5, 0.5, (float)width - 1, (float)height - 1, Curve, 1.0);
  }

  virtual void drawPopupMenuBackground(juce::Graphics &g, int width, int height)
  {
    float Curve = 1.0;
    g.setColour(juce::Colour((juce::uint8)118, (juce::uint8)118,
      (juce::uint8)118));
    g.fillRoundedRectangle( 0, 0, (float)width, (float)height, Curve);
    g.setColour(juce::Colours::white);
    g.drawRoundedRectangle( 0, 0, (float)width, (float)height, Curve, 1.0);
  }
  
  /***
  virtual juce::Label* createSliderTextBox(juce::Slider& slider)
  {
    juce::Label* originalLabel = juce::LookAndFeel::createSliderTextBox(slider);
    originalLabel->setJustificationType(juce::Justification::centredLeft);
    return originalLabel;
  }
  ***/
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-virtual"
  // Member declaration raising the warning.
  void drawTableHeaderColumn (juce::Graphics& g, const juce::String& columnName, int /*columnId*/,
                                         int width, int height,
                                         bool isMouseOver, bool isMouseDown,
                                         int columnFlags) 
  {
    using namespace juce;
      if (isMouseDown)
          g.fillAll (Colours::darkgrey);
      else if (isMouseOver)
          g.fillAll (Colours::lightgrey);
  
      int rightOfText = width - 4;
  
      if ((columnFlags & (TableHeaderComponent::sortedForwards | TableHeaderComponent::sortedBackwards)) != 0)
      {
          const float top = height * ((columnFlags & TableHeaderComponent::sortedForwards) != 0 ? 0.35f : (1.0f - 0.35f));
          const float bottom = height - top;
  
          const float w = height * 0.5f;
          const float x = rightOfText - (w * 1.25f);
          rightOfText = (int) x;
  
          Path sortArrow;
          sortArrow.addTriangle (x, bottom, x + w * 0.5f, top, x + w, bottom);
  
          g.setColour (Colours::white);
          g.fillPath (sortArrow);
      }
  
      g.setColour (Colours::white);
      g.setFont (Font (height * 0.5f, Font::bold));
      const int textX = 4;
      g.drawFittedText (columnName, textX, 0, rightOfText - textX, height, Justification::centredLeft, 1);
  }
#pragma clang diagnostic pop

  void drawTableHeaderBackground (juce::Graphics& g, juce::TableHeaderComponent& header)
  {
    using namespace juce;
    g.fillAll (Colours::darkgrey);

    const int w = header.getWidth();
    const int h = header.getHeight();

    g.setGradientFill (ColourGradient (Colours::darkgrey, 0.0f, h * 0.5f,
                                       Colours::grey, 0.0f, h - 1.0f,
                                       false));
    g.fillRect (0, h / 2, w, h);

    g.setColour (Colours::grey);
    g.fillRect (0, h - 1, w, 1);

    for (int i = header.getNumColumns (true); --i >= 0;)
        g.fillRect (header.getColumnPosition (i).getRight() - 1, 0, 1, h - 1);
  }


  //  /*
  virtual void drawPopupMenuItem(juce::Graphics& g, int width, int height,
                                 bool isSeparator, bool isActive, bool isHighlighted,
                                 bool isTicked, bool hasSubMenu, const juce::String& text,
                                 const juce::String& shortcutKeyText, juce::Image* image, 
                                 const juce::Colour* const textColourToUse)
  {
    const float halfH = height * 0.5f + 0.5f;
    if(isSeparator)
    {
      const float separatorIndent = 5.5f;
      g.setColour(juce::Colour(157,157,161));
      g.drawLine(separatorIndent, halfH, width - separatorIndent, halfH);
    }
    else
    {
      juce::Colour textColour(juce::Colours::white);

      if(textColourToUse != 0)
        textColour = *textColourToUse;

      if(isHighlighted)
      {
        g.setColour(juce::Colours::lightgrey);
        
        g.fillRect(1, 1, width - 2, height - 2);

        if(SysInfo::isMac())
          g.setColour(juce::Colours::white);
        else
          g.setColour(juce::Colours::black);
      }
      else
        g.setColour(textColour);

      if(!isActive)
        g.setOpacity(0.3f);

      juce::Font font(getPopupMenuFont());

      if(font.getHeight() > height / 1.3f)
        font.setHeight (height / 1.3f);

      g.setFont(font);

      const int leftBorder = (height * 5) / 4;
      const int rightBorder = 4;

      if(image != 0)
      {
         juce::RectanglePlacement r ( juce::RectanglePlacement::centred |  juce::RectanglePlacement::onlyReduceInSize);
        g.drawImageWithin(*image, 2, 1, leftBorder - 4, height - 2, r, false);
      }
      else if(isTicked)
      {
        const  juce::Path tick(getTickShape (1.0f));
        const float th = font.getAscent();
        const float ty = halfH - th * 0.5f;

        g.fillPath(tick, tick.getTransformToScaleToFit (2.0f, ty,
                                                        (float)(leftBorder - 4), th, true));
      }

      g.drawFittedText(text,
                       leftBorder, 0,
                       width - (leftBorder + rightBorder), height,
                       juce::Justification::centredLeft, 1);

      if(shortcutKeyText.isNotEmpty())
      {
        juce::String revised = shortcutKeyText;

        revised = revised.replace(juce::String(" "), juce::String());
        revised = revised.replace(juce::String("shortcut"), juce::String());
        revised = revised.replace(juce::String("'"), juce::String());
        revised = revised.replace(juce::String(":"), juce::String());
        if(SysInfo::isMac())
        {
          //http://www.macosxhints.com/article.php?story=20071028203517911
          //http://web.sabi.net/mockups/source/keychars.html
          revised = revised.replace(juce::String("return"), juce::String("_"), true);
          revised = revised.replace(juce::String("ctrl"), juce::String::charToString(0x2303), true);
          revised = revised.replace(juce::String("command"), juce::String("$"), true);
          revised = revised.replace(juce::String("shift"), juce::String("^"), true);
          revised = revised.replace(juce::String("alt"), juce::String("`"), true);
          revised = revised.replace(juce::String("+"), juce::String());
          revised = revised.replace(juce::String("$^"), juce::String("^$"), true);
          revised = revised.replace(juce::String("$`"), juce::String("`$"), true);
          revised = revised.replace(juce::String("$"), juce::String::charToString(0x2318), true);
          revised = revised.replace(juce::String("`"), juce::String::charToString(0x2325), true);
          revised = revised.replace(juce::String("^"), juce::String::charToString(0x21E7), true);
          revised = revised.replace(juce::String("_"), juce::String::charToString(0x21B5), true);
          revised = revised.replace(juce::String("tab"), juce::String::charToString(0x21E5), true);
        }
        else
        {
          revised = revised.replace(juce::String("delete"), juce::String("Backspace"), true);
          revised = revised.replace(juce::String("return"), juce::String("Enter"), true);
          revised = revised.replace(juce::String("ctrl"), juce::String("Ctrl"), true);
          revised = revised.replace(juce::String("shift"), juce::String("Shift"), true);
          revised = revised.replace(juce::String("alt"), juce::String("Alt"), true);
        }

        int right = width - (rightBorder + 4);
        g.drawText(revised,
                   right - fontMenuBar->getStringWidth(revised),
                   0,
                   right,
                   height,
                   juce::Justification::centredLeft,
                   false);
      }

      if(hasSubMenu)
      {
        const float arrowH = 0.6f * getPopupMenuFont().getAscent();
        const float x = width - height * 0.6f;

        juce::Path p;
        p.addTriangle(x, halfH - arrowH * 0.5f,
                      x, halfH + arrowH * 0.5f,
                      x + arrowH * 0.6f, halfH);

        g.fillPath(p);
      }
    }
  }

  //  */
  virtual void drawMenuBarItem(juce::Graphics &g, int width, int height, 
                               int itemIndex, const juce::String &itemText, bool isMouseOverItem, 
                               bool isMenuOpen, bool isMouseOverBar, juce::MenuBarComponent &menuBar)
  {
    g.setFont(*fontMenuBar);
    
    if(!SysInfo::isMac() && isMouseOverItem)
    {
      g.setColour(juce::Colour(187,183,199));
      g.fillRect(0,0,width,height-2);

      g.setColour(juce::Colour(178,180,191));
      g.drawRect(0,0,width,height-2);
      
      g.setColour(juce::Colours::white);
    }
    else if(SysInfo::isMac() && isMenuOpen)
    {
      juce::ColourGradient gb(juce::Colour(83,112,248), 0, 0, 
                              juce::Colour(30,67,246), 0, (float)height, false);
      g.setGradientFill(gb);
      g.fillRect(0,0,width,height-2);
      
      g.setColour(juce::Colours::white);
    }
    else
      g.setColour(juce::Colours::white);

    g.drawText(itemText,0,0,width,height,juce::Justification::centred,false);
  }

  virtual void drawMenuBarBackground(juce::Graphics &g, int width, int height, 
                                     bool isMouseOverBar, juce::MenuBarComponent &menuBar)
  {
    g.fillAll(juce::Colours::darkgrey);
    
    g.setColour(juce::Colours::darkgrey.brighter((float)0.2));
    g.drawHorizontalLine(height-2,0,(float)width);

    g.setColour(juce::Colours::darkgrey.brighter((float)0.4));
    g.drawHorizontalLine(height-1,0,(float)width);
  }

  virtual juce::Font getMenuBarFont(juce::MenuBarComponent &menuBar,
                                          int itemIndex, const juce::String &itemText)
  {
    return *fontMenuBar;
  }

  virtual juce::Font getPopupMenuFont()
  {
    return *fontMenuBar;
  }

  /*  virtual juce::Button* createSliderButton (const bool isIncrement)
  {
    juce::DrawableButton* b = new juce::DrawableButton((isIncrement ? "Inc" : "Dec"), juce::DrawableButton::ImageOnButtonBackground);
    Graphics::drawIncDecButton(b, isIncrement);
    return b;
    }*/

  ///Draws the button background.
  virtual void drawButtonBackground(juce::Graphics& g, juce::Button& button,
    const juce::Colour& backgroundColour, bool isMouseOverButton,
    bool isButtonDown)
  {
    juce::Path p;
    p.addRoundedRectangle(button.getLocalBounds(), 3.0, 3.0);
    
    juce::Colour color = backgroundColour;
    if(isMouseOverButton && !isButtonDown)
      color = color.brighter();
    else if(isMouseOverButton  && isButtonDown)
      color = color.darker();
    juce::Colour outline = color.brighter();
    g.setColour(color);
    g.fillPath(p);
    g.setColour(outline);
    g.strokePath(p, juce::PathStrokeType(1.0f));
  }
  
  void drawTickBox (juce::Graphics& g,
                               juce::Component& component,
                               float x, float y, float w, float h,
                               bool ticked,
                               bool isEnabled,
                               bool isMouseOverButton,
                               bool isButtonDown)
  {
    const float boxSize = w * 0.7f;

    drawGlassSphere (g, x, y + (h - boxSize) * 0.5f, boxSize,
                     //                     component.findColour (juce::TextButton::buttonColourId).withMultipliedAlpha (isEnabled ? 1.0f : 0.5f),
                     juce::Colours::black.withMultipliedAlpha (isEnabled ? 1.0f : 0.5f),
                     isEnabled ? ((isButtonDown || isMouseOverButton) ? 1.1f : 0.5f) : 0.3f);

    if (ticked)
    {
      juce::Path tick;
      tick.startNewSubPath (1.5f, 3.0f);
      tick.lineTo (3.0f, 6.0f);
      tick.lineTo (6.0f, 0.0f);

      g.setColour (isEnabled ? juce::Colours::white : juce::Colours::lightgrey);

      const juce::AffineTransform trans (juce::AffineTransform::scale (w / 9.0f, h / 9.0f)
                                         .translated (x, y));

      g.strokePath (tick, juce::PathStrokeType (2.5f), trans);
    }
  }





  ///Drags a toggle button.
  /*  virtual void drawToggleButton(juce::Graphics& g, juce::ToggleButton& button,
    bool isMouseOverButton, bool isButtonDown)
  {
    juce::Colour baseColor = (button.getToggleState() ?
      juce::Colours::indianred : findColour(juce::TextButton::buttonColourId));
    drawButtonBackground(g, button, baseColor,
      isMouseOverButton && !button.getToggleState(), button.getToggleState());
    g.setColour(findColour(juce::TextButton::textColourOnId));
    g.drawText(button.getName(), button.getLocalBounds(),
      juce::Justification::centred, false);
  }
  */
};
