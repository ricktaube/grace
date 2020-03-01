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

#include "Enumerations.h"
#include "Console.h"
#include "CodeEditor.h"
#include "Plot.h"
#include "Midi.h"
#include "Scheme.h"
#include "CmSupport.h"
#include "PlotEditor.h"
#include "PlotWindow.h"

/*=======================================================================*
  Plot Viewport (scrolling)
  *=======================================================================*/

class PlotViewport : public juce::Viewport 
{
public:
  Plotter* plotter;
  PlotViewport (Plotter* p) 
  : plotter (p)
  {
    // register plotter to receive callbacks from the viewport's
    // scollers so plotter can scroll axis views whenever the plot is
    // scrolled
    getHorizontalScrollBar().setName("xscroll");
    getHorizontalScrollBar().addListener(p);
    getVerticalScrollBar().setName("yscroll");
    getVerticalScrollBar().addListener(p);

  }
  virtual ~PlotViewport () {}

  // return the width for the axis view. this should track the visible
  // width of the plot view being displayed: if the vertical scroller
  // is visible this will be less than the viewport's width (because
  // the vscroller claims space) and, regardless of the visiblity of
  // the vertical scroller, the plotview's width may be smaller than
  // the width of the viewport
  int getVisibleWidth()
  {
    //return getWidth();
    juce::ScrollBar& sb=getVerticalScrollBar();
    int w=(sb.isVisible() ) ? getWidth()-sb.getWidth() : getWidth();    
    return juce::jmin(w, getViewWidth());
  }
  int getVisibleHeight()
  {
    juce::ScrollBar& sb=getHorizontalScrollBar();
    int h=(sb.isVisible() ) ? getHeight()-sb.getHeight() : getHeight();
    return juce::jmin(h, getViewHeight());
  }
};

/*=======================================================================*
  AxisView methods
  *=======================================================================*/

bool AxisView::isVertical()
{
  return (orient==Plotter::vertical);
}

bool AxisView::isHorizontal() 
{
  return (orient==Plotter::horizontal);
}

void AxisView::paint (juce::Graphics& g) 
{
#define graywidth 13
#define majortick 12
#define minortick 6
  g.fillAll (juce::Colours::white); 
  //    g.fillAll(juce::Colours::lightyellow); // ***showregion

  if (!hasAxis() ) return;
  int w=getWidth();
  int h=getHeight();

  g.setColour(juce::Colours::lightgrey);   
  if ( isVertical() )
    g.fillRect(w-graywidth, 0, graywidth, h);
  else  
    g.fillRect(0, graywidth, w, graywidth);

  g.setColour(juce::Colours::black);  
  // not sure if juce would call this method before SetAxis happens
  // but no reason not to be safe!
  
  if ( viewport==(PlotViewport *)NULL ) 
    return;
  
  // drawing axis ticks: first determine the visible portion of the
  // plot display since this will determine what portion of the axis
  // gets drawn.  then we find the FIRST label position at or above
  // that pixl position and draw all the labels and ticks until the
  // end of the axis view. then we go back to the the first label and
  // REVERSE direction to fill in any ticks that are visible before
  // the first label.

  double isiz=incrementSize();
  double tsiz=tickSize();
  juce::Colour col1=juce::Colours::darkgrey;
  juce::Colour col2=juce::Colours::lightgrey;
  juce::Colour tcol=juce::Colours::black;  // text color
  col2=col1;
  juce::Font font=viewport->plotter->font;
  juce::String labl;
  double lhei=font.getHeight(), lwid, just;
  double pval=getOrigin(); 
  double aval=axis->getMinimum();   
  double save;
  double by=axis->getIncrement();
  double amax=axis->getMaximum();
 
  tcol=viewport->plotter->getFocusLayer()->getLayerColor();
  g.setFont(font);
  
  if ( isHorizontal() ) 
  {
    int left=viewport->getViewPositionX();  // plot's visible left
    int width=viewport->getViewWidth();
    int height=getHeight();   // height of axis view
      
    //g.drawHorizontalLine(height-1,0, getWidth());
    g.drawLine(0.0, (float)(height-1), (float)getWidth(), (float)(height-1), 2.0);
      
    // determine first visible label that is >= plot's visible left
    while ( pval<left ) 
    {
      pval+=isiz;
      aval+=by;
    }
    // subtract out visible left position to make it position of the
    // first major tick+label in the axis view
    save=pval-left;  // save=first label tick
    pval=save;
    // draw labels and ticks till end of view. label drawing is
    // justified so that the text stays witin bounds. at start display
    // labels are Right, in the middle they are centered and at the
    // end they are left justified. dont draw ticks right of maximum value
    while ( pval<=width && aval<=amax)
    {
      g.setColour(col1);
      g.drawVerticalLine((int)pval, (float)(height-majortick), (float)height);
      labl=axis->getLabel(aval);
      lwid=font.getStringWidthFloat(labl);
      just=(int)(-lwid*(pval/width));  // twiddle label justification
      g.setColour(tcol);
      g.drawText(labl,(int)(pval+just),0,(int)lwid,(int)lhei, juce::Justification::topLeft, false);
      // draw minor ticks above each major tick 
      g.setColour(col2);
      for (int i=1;i<numTicks() ;i++)
        g.drawVerticalLine((int)(pval+(tsiz*i)), (float)(height-minortick),
                           (float)height);
      pval+=isiz;
      aval+=by;
    }
    // now draw any ticks left of first increment
    pval=save-tsiz;
    g.setColour(col2);
    while (pval > 0)
    {
      g.drawVerticalLine((int)pval, (float)(height-minortick), (float)height);
      pval-=tsiz;
    }
  }
  else
  {
    int height=viewport->getViewHeight();
    int top=viewport->getViewPositionY();
    int bottom=height+top;
    int width=getWidth();   // width of axis view
    //      float tx,ty;
      
    //g.drawVerticalLine(width-1, 0, getHeight());
    g.drawLine((float)(width-1), 0.0, (float)(width-1), (float)getHeight(), 2.0);

    while (pval>bottom)
    {
      pval-=isiz;
      aval+=by;
    }

    save=pval-top;
    pval=save;
    while (pval>=0 && aval<=amax) 
    {
      g.setColour(col1);
      g.drawHorizontalLine((int)pval, (float)(width-majortick), (float)width);
      //      labl=juce::String(aval, axis->getDecimals());
      labl=axis->getLabel(aval);
      lwid=font.getStringWidthFloat(labl);
	  
      // METHOD1: justify vertically, but now the view has to be wider.
      just=(int)(lhei*(pval/height));  // twiddle label justification
      g.setColour(tcol);
      g.drawText(labl,
                 (int)(width-graywidth-4-lwid),
                 (int)(pval-just),
                 (int)lwid,
                 (int)lhei,
                 juce::Justification::topLeft,
                 false);	  
      g.setColour(col2);
      for (int i=1;i<numTicks();i++)
        g.drawHorizontalLine((int)(pval-(tsiz*i)), (float)(width-minortick), (float)width);
      pval-=isiz;
      aval+=by;
    }
    // now draw any ticks below first increment
    pval=save+tsiz;
    g.setColour(col2);
    while (pval < height) 
    {
      g.drawHorizontalLine((int)pval, (float)(width-minortick), (float)width);
      pval+=tsiz;
    }
  }
  g.setColour(juce::Colours::black);
}

/*=======================================================================*
  Region and selection, sweeping and editing
  *=======================================================================*/

class Region : public juce::Component 
{
public:
  enum {selection = 1, horizontal, vertical};
  int type;
  juce::Colour fillcolor, linecolor;
  int linewidth;
  AxisView* xa;
  AxisView* ya;
  Region ()
    : type(1),
      fillcolor(juce::Colour(0x66dddddd )),
      linecolor(juce::Colour(0x99111111)),
      linewidth(1),
      xa(0),
      ya(0)
  {
  }
  virtual ~Region() { }
  bool isActive() { return isVisible(); }
  void setType(int t) {type=t;}

  void beginSweep (const juce::MouseEvent& e, AxisView* x, AxisView* y) 
  {
    xa=x;
    ya=y;
    setSize(0,0);
  }
  void sweep (const juce::MouseEvent& e) 
  {
    const int x1 = e.getMouseDownX();
    const int y1 = e.getMouseDownY();
    setBounds(juce::jmin(x1, e.x), juce::jmin(y1, e.y), abs(e.x - x1), abs (e.y - y1));
    setVisible(true);
  }
  void endSweep (const juce::MouseEvent& e)
  {
    //printf("region: x1=%f x2=%f, y1=%f, y2=%f\n", minX(),  maxX(), minY(),  maxY());
    setSize(0,0);
    setVisible(false);
  }
  // NB: these cant be called after region has been closed!
  double minX() {return xa->toValue(getX());}
  double maxX() {return xa->toValue(getRight());}
  double minY() {return ya->toValue(getBottom());}
  double maxY() {return ya->toValue(getY());}

  void paint (juce::Graphics& g) 
  {
    g.fillAll(fillcolor);
    g.setColour (linecolor);
    g.drawRect (0, 0, getWidth(), getHeight(), linewidth);
    //printf("region: x=%d y=%d, w=%d, h=%d\n", getX(), getY(), getWidth(), getHeight());
  }
};

/*=======================================================================*
  BackView: view to hold grid and background plots.
  *=======================================================================*/

void drawLayer(juce::Graphics& g, Layer* layer, AxisView* haxview, 
	       AxisView* vaxview, double ppp, double spread, bool isFoc, 
	       juce::SelectedItemSet<Layer::NPoint*> * sel);

void drawGrid(juce::Graphics& g, AxisView* haxview, AxisView* vaxview, juce::Colour c1, juce::Colour c2) ;

class BackView : public juce::Component 
{
public:
  Plotter* plotter;
  BackView (Plotter* p) 
  : plotter (p)
  {
    setBufferedToImage(true);
  }

  //  Plotter::BGStyle getBackViewStyle() {return bgstyle;}
  //  void setBackViewStyle( Plotter::BGStyle b) {bgstyle=b;}
  //  bool isBackViewPlotting() {return bgplots;}
  //  void setBackViewPlotting( bool b) {bgplots=b;}
  void setBackViewCaching(bool b) {setBufferedToImage(b);}
  void paint (juce::Graphics& g) 
  {
    g.fillAll(plotter->bgColor);
    AxisView* haxview = plotter->getHorizontalAxisView();
    AxisView* vaxview = plotter->getVerticalAxisView();
    Layer* layer;
    
    if (!vaxview->hasAxis() || !haxview->hasAxis()) return;

    // draw background grid
    if (plotter->bgGrid)
      drawGrid(g, haxview, vaxview, juce::Colours::darkgrey, juce::Colours::lightgrey);

    // draw non-focus plots
    if (plotter->bgPlotting)
    {
      for (int i=0; i<plotter->numLayers(); i++) 
      {
        layer = plotter->getLayer(i);
        if (!plotter->isFocusLayer(layer)) 
          drawLayer(g, layer, haxview, vaxview, 0, 1.0, false, 
                    (juce::SelectedItemSet<Layer::NPoint*> *)NULL);
      }
    }
  }

  ~BackView () {};
};

/*=======================================================================*
  PlotView: the focus layer's drawing canvas
  *=======================================================================*/

class PlotView : public juce::Component, public juce::SettableTooltipClient
{
public:
  Plotter* plotter;
  BackView* backview;
  double pad; // pix per inc, pix per point, margin pad
  juce::Point<float> mousedown, mousemove;
  juce::SelectedItemSet<Layer::NPoint*> selection;
  Region region;
  Layer* focuslayer; // cached focus layer for fast acesss

  
  PlotView (Plotter* p) : plotter(p), pad (8.0) 
  {
    setTooltip("add points: Control-mouseclick");
  }
  
  virtual ~PlotView () { }


  juce::String getTooltip() 
  {
    juce::String tt=juce::SettableTooltipClient::getTooltip();
    if (plotter->getFocusLayer()->numPoints()==0)
      return tt;
    return juce::String();
  }
  void setTooltip (const juce::String& newTooltip)
  {
    juce::SettableTooltipClient::setTooltip(newTooltip);
  }

  void paint (juce::Graphics& g);
  void repaintFocusPlot() {repaint();}
  void resized() 
  {
    //    std::cout << "Plotview::resized(" << getWidth() << ", " << getHeight() << ")\n";
    plotter->backview->setSize(getWidth(),getHeight());
  }
  void mouseDown(const juce::MouseEvent &e) ;
  int findPoint(Layer* layer, double mousex, double mousey);
  void mouseDrag(const juce::MouseEvent &e) ;
  void mouseUp(const juce::MouseEvent &e) ;
  void mouseDoubleClick(const juce::MouseEvent &e) ;
  bool keyPressed (const juce::KeyPress& key) ;

  void resizeForDrawing();
  /*
    int visiblePixelLeft () { return plotter->viewport->getViewPositionX();}
    int visiblePixelTop () { return plotter->viewport->getViewPositionY();}
    int visiblePixelRight () { 
    return (plotter->viewport->getViewPositionX() +
    plotter->viewport->getViewWidth());
    }
    int visiblePixelBottom () { 
    return (plotter->viewport->getViewPositionY() +
    plotter->viewport->getViewHeight());
    }
    double visibleValueLeft(){return haxview->toValue(visiblePixelLeft());}
    double visibleValueRight(){return haxview->toValue(visiblePixelRight());}
    double visibleValueTop(){return vaxview->toValue(visiblePixelTop()); }
    double visibleValueBottom(){return vaxview->toValue(visiblePixelBottom());}
  */

  bool isSelection() {return (selection.getNumSelected() > 0);}
  int numSelected() {return selection.getNumSelected();}

  bool isSelected(Layer::NPoint* p) {return selection.isSelected(p);}
  bool isSelected(int h) {return isSelected(focuslayer->getPoint(h));}

  void selectAll (bool redraw=true)
  {
    int i;
    selection.deselectAll();
    for (i=0; i<focuslayer->numPoints(); i++)
      selection.addToSelection(focuslayer->getPoint(i));
    if (redraw) repaintFocusPlot();
  }

  void deselectAll(bool redraw=true)
  {
    if (isSelection()) 
    {
      selection.deselectAll();
      if (redraw) repaintFocusPlot();
    }
  }

  void removeSelection(Layer::NPoint* p) {selection.deselect(p);}
  void removeSelection(int h) {removeSelection(focuslayer->getPoint(h));}

  void setSelection(Layer::NPoint* p) ;
  void setSelection(int h) {setSelection(focuslayer->getPoint(h));}

  void addSelection(Layer::NPoint* p) {
    selection.addToSelection(p);
  }
  void addSelection(int h) {addSelection(focuslayer->getPoint(h));}

  void deleteSelection (bool cut=false) 
  {
    for (int i=0; i<numSelected(); i++)
      focuslayer->deletePoint( getSelected(i));
    repaint();
  }

  Layer::NPoint* getSelected(int i) {return selection.getSelectedItem(i);}
  int getSelectedIndex(int i) {
    return focuslayer->getPointIndex(selection.getSelectedItem(i));
  }

  void dragSelection(double x, double y);
  void shiftSelection(int orient, double delta);
  void rescaleSelection (int orient, double newlow, double newhigh);

  void getSelectionRange(int orient, double& low, double& high);
  double getSelectionMin(Plotter::Orientation orient);

  //  bool isInside(double x, double y, double left, double top, double right, double bottom);

  // this shuold be a layer method...
  void selectPointsInside(double left, double top, double right, double bottom) ;

  void printSelection() {
    printf("#<Selection:");
    for (int i=0;i<numSelected(); i++)
      printf(" %d", getSelectedIndex(i));
    printf(">\n");
  }

  static bool isInside(double x, double y, double l, double t, double r, double b) 
  {
    return ((l <= x) && (x <= r) && (b <= y) && (y <= t));
  }

};

void PlotView::setSelection(Layer::NPoint* p) {
  selection.selectOnly(p);
}

double PlotView::getSelectionMin(Plotter::Orientation orient)
{
  double (Layer::*getter) (Layer::NPoint* p) ;
  double lim = std::numeric_limits<double>::max();

  if (orient == Plotter::horizontal)
    getter = &Layer::getPointX;
  else 
    getter = &Layer::getPointY;

  for (int i = 0; i < numSelected(); i++)
  {
    lim = juce::jmin(lim, (focuslayer->*getter)(getSelected(i)));
  }
  return lim;
}

void PlotView::getSelectionRange(int orient, double& low, double& high)
{
  low = std::numeric_limits<double>::max();
  high = std::numeric_limits<double>::min();
  double x;

  if (orient == Plotter::horizontal)
    for (int i = 0; i < numSelected(); i++) 
    {
      x = focuslayer->getPointX(getSelected(i));
      low = juce::jmin(low, x);
      high = juce::jmax(high, x);
    }
  else  
    for (int i = 0; i < numSelected(); i++)
    {
      x = focuslayer->getPointY(getSelected(i));
      low = juce::jmin(low, x);
      high = juce::jmax(high, x);
    }
}
   
void PlotView::dragSelection(double dx, double dy) 
{
  int n=numSelected();
  for (int i=0; i<n; i++)
    focuslayer->incPoint( getSelected(i), dx, dy);
}

void PlotView::shiftSelection(int orient, double delta) 
{
  if (delta==0.0) return;
  int n=numSelected();
  Layer::NPoint* p;
  if (orient==Plotter::vertical)
    for (int i=0; i<n; i++)
    {
      p=getSelected(i);
      focuslayer->incPointY(p, delta);
    }
  else
  {
    for (int i=0; i<n; i++)
    {
      p=getSelected(i);
      focuslayer->incPointX(p, delta);
    }
    focuslayer->sortPoints();
  }
  repaintFocusPlot();
}

void PlotView::rescaleSelection (int orient, double newmin, double newmax)
{
  Layer::NPoint* p;
  double oldmin, oldmax;
  double v;
  getSelectionRange(orient, oldmin, oldmax);
  if ((oldmin==newmin) && (oldmax==newmax))
    return;
  if (orient==Plotter::vertical)
    for (int i=0; i<numSelected(); i++)
    {
      p=getSelected(i);
      v=cm_rescale(focuslayer->getPointY(p), oldmin, oldmax,
                   newmin, newmax, 1);
      focuslayer->setPointY(p,(double)v);
    }
  else 
  {
    for (int i=0; i<numSelected(); i++)
    {
      p=getSelected(i);
      v=cm_rescale(focuslayer->getPointX(p), oldmin, oldmax,
                   newmin, newmax, 1);
      focuslayer->setPointX(p,(double)v);
    }
    focuslayer->sortPoints();
  }      
  repaintFocusPlot();
}

// drawing and mouse

void PlotView::resizeForDrawing() 
{
  // called when spread value changes to reset total size of plotting view
  double xsiz, ysiz, xtot, ytot;
  AxisView * haxview=plotter->getHorizontalAxisView();
  AxisView * vaxview=plotter->getVerticalAxisView();
  // xpad and ypad are margins around the plotting area so points at
  // the edge aren't clipped
  xsiz=haxview->extent();
  ysiz=vaxview->extent();
  xtot=pad+xsiz+pad;
  ytot=pad+ysiz+pad;
  haxview->setOrigin(pad);
  vaxview->setOrigin(ytot-pad);
  setSize( (int)xtot, (int)ytot );
}

void drawLayer(juce::Graphics& g, Layer* layer, AxisView* haxview, AxisView* vaxview, 
	       double ppp, double zoom, bool isFoc, 
	       juce::SelectedItemSet<Layer::NPoint*> * sel) 
{
  double ax, ay, px, py, lx, ly, ox, oy;
  // line width
  double lw = layer->lineWidth;
  // rectangular width is used to draw points, bars and boxes
  double rw = (float)((layer->isDrawStyle(Layer::point)) ? layer->pointWidth : layer->barWidth);
  double half = (rw / 2);

  juce::Colour color, selcolor= juce::Colours::grey;
  int ndraw;
  //printf("visible left=%f, right=%f\n", visibleValueLeft(), visibleValueRight());
  color = layer->color;
  ndraw = layer->numPoints();

  // need pixel origins for vert/horiz lines/bars
  if (vaxview->axisMinimum() < 0.0 && vaxview->axisMaximum() >= 0.0)
    oy = vaxview->toPixel(0.0);
  else
    oy = vaxview->toPixel(vaxview->axisMinimum());

  if (haxview->axisMinimum() < 0.0 && haxview->axisMaximum() >= 0.0)
    ox = haxview->toPixel(0.0);
  else
    ox = haxview->toPixel(haxview->axisMinimum());

  g.setColour(color);
  for (int i = 0; i < ndraw; i++) 
  {
    Layer::NPoint* p = layer->getPoint(i);
    bool isSel = (isFoc && sel->isSelected(p));
    ax = layer->getPointX(p);
    ay = layer->getPointY(p);  
    px = haxview->toPixel(ax);   // pixel coords
    py = vaxview->toPixel(ay);

    // first draw lines in layer color
    if (layer->isDrawStyle(Layer::line))
    {
      if (layer->isDrawStyle(Layer::vertical)) // draw vertical line from orign
	g.drawLine((float)px, (float)oy, (float)px, (float)py, (float)lw);
      else if (layer->isDrawStyle(Layer::horizontal) ) // horizontal line from origin
	g.drawLine((float)ox, (float)py, (float)px, (float)py, (float)lw);
      else if (i>0)  // draw envelope line between points
	g.drawLine((float)lx, (float)ly, (float)px, (float)py, (float)lw);
    }

    // next draw points bars and boxes, maybe in selection color
    if (isSel) g.setColour(selcolor);
    if (layer->isDrawStyle(Layer::point)) 
    {
      g.fillEllipse((float)(px-half), (float)(py-half), (float)rw, (float)rw);
    } 
    else if (layer->isDrawStyle(Layer::hbox))
    {
      // to get pixel width of Z, get absolute axis position of Z,
      // convert to pixel and then subtract out px
      double az = ax + layer->getPointZ(p);
      double pz = (int)haxview->toPixel(az);
      // draw selected boxes gray if moving
      g.fillRect((int)px, (int)(py - half), (int)(pz - px), (int)rw);
    }
    else if (layer->isDrawStyle(Layer::bar))
    {
      if (layer->isDrawStyle(Layer::vertical))
      {	
        // y origin is always greater than 
        ////        std::cout << "vbar: px=" << ((int)(px-half))
        ////		  << " py=" << ((int)py) << " oy=" << ((int)oy)
        ////		  << " w=" << ((int)rw) << " h=" << ((int)(oy-py))
        ////		  << "\n";
	g.fillRect((int)(px - half), (int)py, (int)rw, (int)(oy - py));
        //	g.fillRect((int)(px-half), (int)oy, (int)rw, (int)(py-oy));
      }
      else if (layer->isDrawStyle(Layer::horizontal) )
      {
	g.fillRect((int)(ox), (int)(py - half), (int)px, (int)(py + half));
      }
    }
    // set back to layer's color
    if (isSel) g.setColour(color);
    lx = px;
    ly = py;
  }
}

void PlotView::paint (juce::Graphics& g) 
{
  //g.fillAll(juce::Colours::lightseagreen);  // ***showregion

  drawLayer(g, focuslayer,
	    plotter->getHorizontalAxisView(),
	    plotter->getVerticalAxisView(),
	    plotter->getPointSize(),
	    1.0,
	    true,
	    &selection);
}

void drawGrid(juce::Graphics& g, AxisView* haxview, AxisView* vaxview, 
	      juce::Colour c1, juce::Colour c2) 
{
  double left = haxview->getOrigin();
  double right = left+haxview->extent();
  double bottom = vaxview->getOrigin();
  double top = bottom-vaxview->extent();
  double p,t,d;
  p = haxview->getOrigin();
  d = haxview->incrementSize();
  t = haxview->tickSize();
  while (p <= right) 
  {
    g.setColour(c1);
    g.drawVerticalLine((int)p, (float)top, (float)bottom);
    g.setColour(c2);
    for (int i = 1; i < haxview->numTicks(); i++) 
      g.drawVerticalLine((int)(p + (t * i)), (float)top, (float)bottom);
    p += d;
  }
  p = vaxview->getOrigin();
  d = vaxview->incrementSize();
  t = vaxview->tickSize();
  while (p >= top)
  {
    g.setColour(c1);
    g.drawHorizontalLine((int)p, (float)left, (float)right);
    g.setColour(c2);
    for (int i = 1; i < vaxview->numTicks(); i++) 
      g.drawHorizontalLine((int)(p - (t * i)), (float)left, (float)right);
    p -= d;
  }
  g.setColour(c1);
}

void PlotView::selectPointsInside(double l, double t, double r, double b)
{
  //printf("looking in region: left=%f top=%f, right=%f, bottom=%f\n",x1, y2, x2, y1);
  //deselectAll();
  for (int i = 0; i < focuslayer->numPoints(); i++) 
  {
    double x = focuslayer->getPointX(i);
    double y = focuslayer->getPointY(i);
    if (isInside(x, y, l, t, r, b))
      addSelection(i);
    else
      if (x > r)
        break; // stop looking
  }
}

int PlotView::findPoint(Layer* layer, double mausx, double mausy)
{
  AxisView* haxview = plotter->getHorizontalAxisView();
  AxisView* vaxview = plotter->getVerticalAxisView();
  double size = ((layer->isDrawStyle(Layer::point)) ? layer->pointWidth : layer->barWidth);
  double half = size / 2;
  int index = -1;  // index of selected point
  double left, top, right, bottom, x, y;
  
  if (layer->isDrawStyle(Layer::hbox)) 
  {
    // check point y against a box centered vertically on mouse y
    top = vaxview->toValue(mausy - half);
    bottom = vaxview->toValue(mausy + half);
    x = haxview->toValue(mausx);
    left = haxview->axisMinimum();
    for (int i = 0; index == -1 && left <= x && i < layer->numPoints(); i++)
    {
      left = layer->getPointX(i);
      right = left + layer->getPointZ(i);
      y = layer->getPointY(i);
      if (isInside(x, y, left, top, right, bottom))
        index =i ;
    }
  }
  else if (layer->isDrawStyle(Layer::vbar)) 
  {
    left = haxview->toValue(mausx - half);
    right = haxview->toValue(mausx + half);
    bottom = vaxview->axisMinimum();
    top = vaxview->toValue(mausy);
    x = haxview->axisMinimum();
    for (int i = 0; index == -1 && x <= right && i < layer->numPoints(); i++)
    {
      x = layer->getPointX(i);
      y = layer->getPointY(i);
      if (isInside(x, top, left, y, right, bottom))
        index = i;
    }
    /*{ // Horizontal Bar
      left = haxview->axisMinimum();
      right = haxview->toValue(mausx);
      bottom = vaxview->toValue(mausy + half);
      top = vaxview->toValue(mausy - half);
      x=haxview->axisMinimum();
      std::cout << "mausx=" << haxview->toValue(mausx) << ", mausy=" << vaxview->toValue(mausy) << "\n";
      std::cout << "left=" << left << ", right=" << right << ", top=" << top << ", bottom=" << bottom << "\n";
      for (int i=0; (index==-1) && (x<=left) && i<layer->numPoints(); i++)
      {
      x=layer->getPointX(i);
      y=layer->getPointY(i);
      if (isInside(x, y, left, top, right, bottom))
      index=i;
      }
      }*/
  }
  else
  {
    // center rect around mouse x and y and find a point inside it
    left = haxview->toValue(mausx - half);
    top = vaxview->toValue(mausy - half);
    right = haxview->toValue(mausx + half);
    bottom = vaxview->toValue(mausy + half);
    x = haxview->axisMinimum();
    for (int i = 0; (index == -1) && (x <= right) && i < layer->numPoints(); i++)
    {
      x = layer->getPointX(i);
      y = layer->getPointY(i);
      if (isInside(x, y, left, top , right, bottom)) 
        index = i;
    }
  }
  return index;
}

void PlotView::mouseDown (const juce::MouseEvent &e)
{
  int mausx = e.getMouseDownX();
  int mausy = e.getMouseDownY();
  AxisView* haxview = plotter->getHorizontalAxisView();
  AxisView* vaxview = plotter->getVerticalAxisView();
  
  // cache mouse down position  FIX THIS ISNT NEEDED
  mousedown.setXY((float)mausx, (float)mausy);
  mousemove.setXY((float)mausx, (float)mausy);
  
  // Control-Click: add point make selection
  // Control-Shift-Click: add point add selection.
  // Control-Option-Click: call plot hook
  if (e.mods.isCtrlDown())
  {
    if (e.mods.isAltDown())
    {
      juce::String str="(call-plot-hook";
      str << " " << plotter->getPlotTitle().quoted()
          << " " << haxview->toValue(mausx)
          << " " << vaxview->toValue(mausy) 
          << ")" ; 
      SchemeThread::getInstance()->eval(str, false);
      return;
    }
    else
    {
      int i = focuslayer->addPoint(haxview->toValue(mausx),
                                   vaxview->toValue(mausy),
                                   plotter->defaults);
      if (e.mods.isShiftDown())
        addSelection(i);
      else setSelection(i);
      repaintFocusPlot();
      return;
    }
  }
  // at this point we are not adding points. first look in the focus
  // plot for a moused point. if no point was moused (h==-1) then
  // search the bg plots if bgMouseable is on.
  int h = findPoint(focuslayer, mausx, mausy);
  if (h < 0 && plotter->bgMouseable && plotter->numLayers() > 1)
  {
    Layer* next = 0;
    for (int i = 0; next == 0 && i < plotter->numLayers(); i++)
    {
      Layer* l = plotter->getLayer(i);
      if (l == focuslayer) continue; // skip focus layer
      h = findPoint(l, mausx, mausy);
      if (h >= 0)
        next = l;
    }
    if (next) // chaning focus
    {
      // deselect current section, set new focus and add point
      deselectAll(); 
      plotter->setFocusLayer(next);
      plotter->redrawAll();
    }
  }

  if (h < 0)
  {
    // Mouse down on empty space . Add to selection if shift is down
    if (true) //!(e.mods.isCtrlDown() || e.mods.isAltDown())
    {
      if (isSelection())
      {
        if (!e.mods.isShiftDown())
        {
          deselectAll();
        }
      }
      addChildComponent (&region);
      region.beginSweep(e, haxview, vaxview);
    }
  }
  else if (isSelected(h)) 
  {
    // point is already selected.
    // Mouse-shift-click remove Point from selection
    if (e.mods.isShiftDown())
    {
      removeSelection(h);
      repaintFocusPlot();
    }
  }
  else
  {
    // point is not selected.
    // Mouse-Click: set point as (single) selection
    // Mouse-shift-click:   add Point from selection
    if (e.mods.isShiftDown()) 
      addSelection(h);
    else
      setSelection(h);
    repaintFocusPlot();
  }
}

void PlotView::mouseDrag(const juce::MouseEvent &e) 
{
  AxisView* haxview = plotter->getHorizontalAxisView();
  AxisView* vaxview = plotter->getVerticalAxisView();
  
  if (isSelection())
  {
    double dx = haxview->toValue(e.x) - haxview->toValue(mousemove.getX()) ;
    double dy = vaxview->toValue(e.y) - vaxview->toValue(mousemove.getY()) ;
    //    for (int i=0; i<numSelected(); i++)
    //      focuslayer->incPoint( getSelected(i), dx, dy);
    dragSelection(dx, dy);
    repaintFocusPlot();
    mousemove.setXY((float)e.x, (float)e.y);
  } 
  else 
  {
    region.toFront(false);
    region.sweep(e);
  }
}

void PlotView::mouseUp(const juce::MouseEvent &e)
{
  if (isSelection()) 
  {
    if (mousedown.getX() != mousemove.getX()) 
    {
      focuslayer->sortPoints();
      //printSelection();
    }
    repaintFocusPlot();
  }
  else if (e.getDistanceFromDragStart() == 0) 
  {
    // clicked without drag
  }
  else if (region.xa && region.ya)
  {
    // get region extent before closing region!
    double l = region.minX();
    double t = region.maxY();
    double r = region.maxX();
    double b = region.minY();
    region.endSweep(e);
    selectPointsInside(l, t, r, b);
    removeChildComponent(&region);
    repaintFocusPlot();
  }
}

void PlotView::mouseDoubleClick(const juce::MouseEvent &e)
{
}

bool PlotView::keyPressed (const juce::KeyPress& key)
{
  //FIXME: NEED TO USE THE GLOBAL COMMAND MANAGER for Com-A etc
  if ((key.isKeyCode(juce::KeyPress::backspaceKey) ||
       key.isKeyCode(juce::KeyPress::deleteKey)))
  {
    if (!key.getModifiers().isAnyModifierKeyDown() && isSelection())
      deleteSelection();
  }
  else if (key.isKeyCode('a') && key.getModifiers().isCommandDown())
  {
    selectAll();
  }
  return true;
}

/*=======================================================================*
  Plotter: contains all the subcomponents for plotting
  *=======================================================================*/

Plotter::Plotter (juce::XmlElement* plot) 
  : haxview (0),
    vaxview (0),
    viewport (0),
    plotview (0),
    backview (0),
    editor (0),
    pbThread (0),
    pbVerticalRescale (false),
    pbMidiOut (0),
    pbMinKey (0.0),
    pbMaxKey (127.0),
    pbTuning (0),
    defaults (0),
    zoom (1.0),
    ppp (8.0),   
    bgColor (juce::Colours::white),
    bgGrid (true),
    bgPlotting (true),
    bgMouseable (true),
    changed (false)
{
  createPlottingComponents();
  juce::OwnedArray<juce::XmlElement> xmlfields;
  juce::OwnedArray<juce::XmlElement> xmlplots;
  if (plot!=NULL)
  {
    //    std::cout << plot->createDocument("").toUTF8() << "\n";
    juce::XmlElement* sub=plot->getChildByName("fields");
    forEachXmlChildElement(*sub, e)
      if (e->hasTagName("field"))
        xmlfields.add(e);
    sub=plot->getChildByName("layers");
    forEachXmlChildElement(*sub, e)
      if (e->hasTagName("points"))
        xmlplots.add(e);
  }  
  if (xmlfields.size()==0) // No axes specified
  {
    fields.add(new Field("x", new Axis(NULL)));
    fields.add(new Field("y", new Axis(NULL)));
  }
  else if (xmlfields.size()==1) // Just Y axis specified
  {
    fields.add(new Field("x", new Axis(Axis::ordinal)));
    juce::String yname=xmlfields[0]->getStringAttribute("name", "y");
    fields.add(new Field(yname,new Axis(xmlfields[0])));
  }
  else
    for (int i=0; i<xmlfields.size(); i++)
    {
      char c='a'+((23+i) % 26); // x y z a b c d ...
      juce::String n=xmlfields[i]->getStringAttribute("name", juce::String(c));
      juce::String r=xmlfields[i]->getStringAttribute("axis", juce::String());
      double d=xmlfields[i]->getDoubleAttribute("default", 0.5); // shouldn't be zero
      Axis* a=NULL;
      int s=-1;
      if (r.isNotEmpty())
        for (int j=0; j<i && j<fields.size() && a==NULL; j++)
          // if the field's axis name is a field name
          // <field name="dur" axis="time"> 
          if (fields[j]->name==r)
          {
            a=fields[j]->axis;	
            s=j;
          }
      if (a)
        ;
      else
      {
        a=new Axis(xmlfields[i]);
      }
      fields.add(new Field(n, a, s, d));
    }
  // add the layers
  if (xmlplots.size()>0)
    for (int i=0; i<xmlplots.size(); i++)
      newLayer(xmlplots[i]);
  else
    newLayer(NULL);

  // point defaults for fields above Y
  defaults=new Layer::NPoint(fields.size());
  // insureAxisValues happens AFTER the layers have been added so that
  // underspecified axis can adjust to min/max point values.
  for (int i=0;i<fields.size(); i++)
  {
    if (!isSharedField(i))
      insureAxisValues(i);
    defaults->setVal(i,fields[i]->initval);
    //    std::cout << "defaults["<<i<<"]="<<defaults->getVal(i) << "\n";
  }

  xmlfields.clear(false);
  xmlplots.clear(false);
  pbInitialize();
  setUnsavedChanges(false);
}

Plotter::Plotter(juce::MidiFile& midifile)
  : haxview (0),
    vaxview (0),
    viewport (0),
    plotview (0),
    backview (0),
    editor (0),
    pbThread (0),
    pbVerticalRescale (false),
    pbMidiOut (0),
    pbMinKey (0.0),
    pbMaxKey (127.0),
    pbTuning (0),
    defaults (0),
    zoom (1.0),
    ppp (8.0),
    bgColor (juce::Colours::white),
    bgGrid (true),
    bgPlotting (true),
    bgMouseable (true),
    changed(false)
{
  createPlottingComponents();
  // juce time format is postive for beats and negative for SMTPE
  int format=midifile.getTimeFormat();
  double scaler=1.0; // scale smpte to seconds
  if (format>0)
    midifile.convertTimestampTicksToSeconds();    
  else  // juce smpte frames per second is negative upper byte
    scaler=(0xFF-((format & 0xFF00) >> 8)+1) * (format & 0xFF);
  // start importing at track 1 in level 1 file (?)
  int numtracks=midifile.getNumTracks();
  int track=((numtracks==1) ? 0 : 1);
  int count=0;
  double maxend=0.0;
  double maxkey=-1.0;
  double minkey=128.0;
  juce::OwnedArray<Layer>midilayers;
  int arity=5;
  for ( ; track<numtracks; track++)
  {
    juce::MidiMessageSequence* seq=
      (juce::MidiMessageSequence*)midifile.getTrack(track);
    seq->updateMatchedPairs();
    Layer* lay=new Layer(arity, "Track " + juce::String(track),
                         Layer::defaultColor(count), Layer::hbox);
    //lay->setXField(0);
    //lay->setYField(2);
    //lay->setZField(1);
    lay->setFieldAccess(0,2,1);
    for (int i=0; i<seq->getNumEvents(); i++)
    {
      juce::MidiMessageSequence::MidiEventHolder* h=seq->getEventPointer(i);
      if (h->message.isNoteOn())
      {
        double t=h->message.getTimeStamp()/scaler;
        double d=.5;
        if (h->noteOffObject!=NULL)
          d=(h->noteOffObject->message.getTimeStamp()/scaler)-t;
        double k=h->message.getNoteNumber();
        double a=(double)(h->message.getFloatVelocity());
        double c=h->message.getChannel()-1;
        Layer::NPoint* p=new Layer::NPoint(arity);
        p->setVal(0,t);
        p->setVal(1,d);
        p->setVal(2,k);
        p->setVal(3,a);
        p->setVal(4,c);
        lay->points.add(p);  // add same order as seq
        if (k<minkey) minkey=k;
        if (k>maxkey) maxkey=k;
        t+=d;
        if (t>maxend) maxend=t;
      }
    }
    if (lay->numPoints()==0)
      delete lay;
    else
    {
      midilayers.add(lay);
      count++;
    }
  }
  Axis* a=new Axis(Axis::seconds);
  if (maxend>0.0) a->setMaximum(maxend);
  fields.add(new Field("Time", a, -1, 0.0));
  fields.add(new Field("Duration", a, 0, 0.5));
  fields.add(new Field("Keynum", new Axis(Axis::keynum), -1, 60));
  fields.add(new Field("Amplitude", new Axis(Axis::normalized), -1, 0.5));      
  fields.add(new Field("Channel", new Axis(Axis::generic,0,15,1,1,0), -1, 0.0));
  for (int i=0;i<midilayers.size();i++)
    addLayer(midilayers[i]);
  // point defaults for fields above Y
  defaults=new Layer::NPoint(fields.size());
  for (int i=0; i<fields.size(); i++)
    defaults->setVal(i, fields[i]->initval);
  midilayers.clear(false);
  ///checkFitInView();
  plotview->resizeForDrawing();  // calc plots width/height
  pbInitialize();
  setUnsavedChanges(false);
}

void Plotter::createPlottingComponents()
{
  font=juce::Font(juce::Font::getDefaultSansSerifFontName(), 10.0, juce::Font::bold);
  plotview=new PlotView (this);
  plotview->setVisible(true);
  backview=new BackView(this);
  backview->setVisible(true);
  // plot view is child of backview so that it is in front
  backview->addChildComponent(plotview);
  plotview->setTopLeftPosition(0,0);
  plotview->setWantsKeyboardFocus(true);
  viewport=new PlotViewport (this);
  // add the backview to the viewport. the plotview is a child of the
  // backview and so it will scroll with it
  addChildComponent(viewport);
  haxview=new AxisView(viewport, horizontal);
  haxview->setVisible(true);
  vaxview=new AxisView(viewport, vertical);
  vaxview->setVisible(true);
  addChildComponent(haxview);  
  addChildComponent(vaxview);  
  viewport->setViewedComponent(backview);
  viewport->setScrollBarsShown (true, true);
  viewport->setViewPositionProportionately(0.0,1.0);
  viewport->setVisible(true);
  backview->setVisible(true);
  plotview->setVisible(true);
  haxview->setVisible(true);
  vaxview->setVisible(true);
  // NB: the Plotter owns the playback thread and creates it but the
  // Editor is responsible for providing a transport, an output device
  // and starting playback.
}

Plotter::~Plotter()
{
  if (pbThread->isPlaying())
    pbThread->pause();
  pbThread->stopThread(-1);
  deleteAndZero(pbThread);
  if (pbMidiOut)
    deleteAndZero(pbMidiOut);
  // zero out shared axis before deleting
  deleteAndZero(haxview);
  deleteAndZero(vaxview);
  deleteAndZero(plotview);
  deleteAndZero(backview);
  deleteAndZero(viewport);
  //actions.clear();
  layers.clear();
  fields.clear();
  if (defaults)
    delete defaults;
}

bool Plotter::hasUnsavedChanges()
{
  if (changed)
  {
    //    std::cout << "plotter has unsaved changes\n";
    return true;
  }
  for (int i=0; i<numLayers(); i++)
    if (getLayer(i)->hasUnsavedChanges())
    {
      //      std::cout << "layer " << i << " has unsaved changes\n";
      return true;
    }
  return false;
}

void Plotter::setUnsavedChanges(bool isChanged)
{
  changed=isChanged;
  if (!isChanged)
    for (int i=0; i<numLayers(); i++)
      getLayer(i)->setUnsavedChanges(false);
}

void Plotter::insureAxisValues(int index)
{
  Axis* ax=getFieldAxis(index);
  // look for unspecified axes to autosize
  if (ax->type==Axis::unspecified)
  {
    double lo=0.0, hi=1.0;
    for (int j=0; j<layers.size(); j++)
    {
      Layer* l=layers[j];
      if (l->numPoints()==0) 
        continue;
      if (index==0) // horizontal axis stored sorted
      {
        lo=juce::jmin(lo,(double)(l->getFirstPoint()->getVal(0)));
        hi=juce::jmax(hi,(double)(l->getLastPoint()->getVal(0)));
      }
      else
        for (int k=0; k<l->numPoints(); k++)
        {
          double f=(double)(l->getPoint(k)->getVal(index));
          lo=juce::jmin(lo,f);
          hi=juce::jmax(hi,f);
        }
    }
    ax->setMinimum(lo);
    if (hi<=1.0)
    {
      ax->setMaximum(1.0);
    }
    else if (hi<=10.0)
    {
      ax->setMaximum(ceil(hi));
      ax->setIncrement(1.0);
    }
    else if (hi<=100.0)
    {
      ax->setMaximum(ceil(hi));
      ax->setIncrement(10.0);
      ax->setTicks(2);
    }
    else if (hi<=1000)
    {
      ax->setMaximum(ceil(hi));
      ax->setIncrement(10.0);
      ax->setTicks(2);
    }
    ax->type=Axis::generic; // make it a generic axis
    //std::cout << "autosized axis[" << i <<"]: min=" << ax->getMinimum() << " max=" << ax->getMaximum() << "\n";
  }
  else if (ax->type==Axis::ordinal)
  {
    int siz=1;
    for (int j=0; j<layers.size(); j++)
      siz=juce::jmax(siz,layers[j]->numPoints());
    ax->setMaximum(siz-1);
  }
  //  std::cout << "insureAxisValues [" << index << "]: " << ax->toString().toUTF8() << "\n";
}

void Plotter::insurePointsVisible()
{
  if (getPlotViewport()->isVerticalScrollBarShown())
  {
    // center on average of min max of first 8 notes
    Layer* l=getFocusLayer();
    double y=.5;
    if (l->numPoints()>0)
    {
      Axis* a=getVerticalAxis();
      double ymin=a->getMaximum();
      double ymax=a->getMinimum();
      for (int i=0;i<8 && i<l->numPoints(); i++)
      {
        y=l->getPointY(i);
        if (y<ymin) ymin=y;
        if (y>ymax) ymax=y;
      }
      y=1-((((ymin+ymax)/2)-a->getMinimum())/a->getRange());
    }
    getPlotViewport()->setViewPositionProportionately(0.0, y);
  }
}
 
///
/// Component View Accessing 
///

AxisView* Plotter::getHorizontalAxisView() 
{
  return haxview;
}
AxisView* Plotter::getVerticalAxisView() 
{
  return vaxview;
}

AxisView* Plotter::getAxisView(int orient) 
{
  if (orient==horizontal) return haxview;
  if (orient==vertical) return vaxview;
  return NULL;
}

void Plotter::setHorizontalAxis (Axis* a) {haxview->setAxis(a);}
void Plotter::redrawHorizontalAxisView() {haxview->repaint();}

void Plotter::setVerticalAxis (Axis* a) {vaxview->setAxis(a);}
void Plotter::redrawVerticalAxisView() {vaxview->repaint();}

Axis* Plotter::getVerticalAxis(){return vaxview->getAxis();}
Axis* Plotter::getHorizontalAxis(){return haxview->getAxis();}
 
BackView * Plotter::getBackView()
{
  return backview;
}

void Plotter::setBackViewCaching(bool b)
{
  backview->setBackViewCaching(b);
}

void Plotter::resizeForSpread()
{
  plotview->resizeForDrawing();
}


PlotView* Plotter::getPlotView() 
{
  return plotview;
}

PlotViewport* Plotter::getPlotViewport()
{
  return viewport;
}

int Plotter::getViewportViewingHeight()
{
  return viewport->getViewHeight();
}

int Plotter::getViewportViewingWidth()
{
  return viewport->getViewWidth();
}

void Plotter::redrawPlotView() 
{
  plotview->repaint();
}

void Plotter::redrawBackView() 
{
  backview->repaint();
}

// triggers a repaint of all plotter components

void Plotter::redrawAll()
{
  //resizeForSpread();
  redrawHorizontalAxisView();
  redrawVerticalAxisView();
  redrawBackView();
  redrawPlotView();
}

///
/// layers
///

Layer* Plotter::getLayer(int i) 
{
  return layers[i];
}

Layer* Plotter::findLayer(int id) 
{
  Layer* l;
  for (int i=0; i< layers.size(); i++)
  {
    l=layers[i];
    if (id == l->getLayerID()) return l;
  }
  return (Layer *)NULL;
}

int Plotter::numLayers() 
{
  return layers.size();
}

Layer* Plotter::getFocusLayer()
{
  return plotview->focuslayer;
}

bool Plotter::isFocusLayer(Layer * l)
{
  return (l == getFocusLayer()); 
}

void Plotter::setFocusLayer(Layer* layr)
{
  plotview->deselectAll(false);
  plotview->focuslayer=layr;
  Axis* oldx=getHorizontalAxisView()->getAxis();
  Axis* oldy=getVerticalAxisView()->getAxis();
  Axis* newx=getFieldAxis(layr->getXField());
  Axis* newy=getFieldAxis(layr->getYField());
  //  std::cout << "setFocusLayer xfield=" << layr->getXField() << ", haxis=" << newx->toString().toUTF8() << "\n";
  //  std::cout << "setFocusLayer yfield=" << layr->getYField() << ", vaxis=" << newy->toString().toUTF8() << "\n";

  if ((oldx!=newx)||(oldy!=newy))
  {
    //    std::cout << "updating visible axis\n";
    setHorizontalAxis(newx);
    setVerticalAxis(newy);
    // doesnt work here...
    //      plotview->resizeForDrawing();  // calc plots width/height
  }
}

void Plotter::setVerticalField(int fnum)
{
  Axis* faxis=fields[fnum]->axis;
  Axis* yaxis=getVerticalAxis();
  if (faxis != yaxis)
  {
    for (int i=0; i<numLayers(); i++)
    {
      Layer* layer=getLayer(i);
      layer->setYField(fnum);
    }
    setVerticalAxis(faxis);
    resizeForSpread();
    redrawAll();
  }
}

Layer* Plotter::newLayer(juce::XmlElement* points, bool redraw)
{
  juce::Colour col = Layer::defaultColor(layers.size());
  int sty=Layer::lineandpoint;
  juce::String nam = "Plot " + juce::String(layers.size()+1);
  int num=numFields();
  Layer* layer=NULL;

  if (points)
  {
    juce::String s=points->getStringAttribute("style");
    if (s.isNotEmpty())
      sty=Layer::toLayerType(s, sty);
    s=points->getStringAttribute("color");
    if (s.isNotEmpty())
      if (s.containsOnly("0123456789aAbBcCdDeEfF"))
        col=juce::Colour::fromString(s);	  
    col=juce::Colours::findColourForName(s, col);
    nam=points->getStringAttribute("title", nam);
    layer=new Layer(num, nam, col, sty);
    layer->addXmlPoints(points);
    // parse optional access fields
    juce::StringArray access;
    access.addTokens(points->getStringAttribute("access"), false);
    for (int i=0; i<access.size() && i<4; i++)
    {
      int j=access[i].getIntValue();
      if (j>=layer->getLayerArity()) break;
      else if (i==0) layer->setXField(j);
      else if (i==1) layer->setYField(j);
      else if (i==2) layer->setZField(j);
      //else if (i==3) ; // TODO!
    }
  }
  else
    layer=new Layer(num, nam, col, sty);
  addLayer(layer);

  // optional update of display after adding the plot
  if (redraw)
  {
    redrawBackView();
    redrawHorizontalAxisView();
    redrawVerticalAxisView();
  }

  return layer;
}

void Plotter::addLayer(Layer* layer)
{
  changed=true;
  layers.add(layer);
  setFocusLayer(layer);
}

void Plotter::removeLayer(Layer* layer) 
{

  // THIS CHECK SHOULDNT BE NECESSARY (MENU CHECKS)
  if ( numLayers() < 2 ) return;
  // if removing focus layer then switch focus to other layer.
  if ( isFocusLayer(layer) ) 
  {
    for (int i=0; i<numLayers(); i++) 
    {
      Layer* l=getLayer(i);
      if (l != layer) 
      {
        setFocusLayer(l);
        break;
      }
    }
  }
  layers.removeObject(layer,false);
  changed=true;
}

bool Plotter::isEmpty()
{
  for (int i = 0; i < numLayers(); i++)
    if (getLayer(i)->numPoints() > 0)
      return false;
  return true;
}

bool Plotter::isNotEmpty()
{
  return !isEmpty();
}
void Plotter::selectAll() 
{
  getPlotView()->selectAll();
}

void Plotter::deselectAll()
{
  getPlotView()->deselectAll();
}

bool Plotter::isSelection() 
{
  return getPlotView()->isSelection();
}

int Plotter::numSelected() 
{
  return getPlotView()->numSelected();
}

Layer::NPoint* Plotter::getSelected(int i) 
{
  return getPlotView()->getSelected(i);
}

void Plotter::getSelectionRange(int orient, double& low, double& high)
{
  plotview->getSelectionRange(orient, low, high);
}

void Plotter::shiftSelection(int orient, double delta)
{
  plotview->shiftSelection(orient, delta);
}

void Plotter::rescaleSelection (int orient, double newlow, double newhigh)
{
  plotview->rescaleSelection(orient,newlow,newhigh);
}

void Plotter::deleteSelection(bool cut) 
{
  getPlotView()->deleteSelection(cut);
}

void Plotter::paint(juce::Graphics& g) 
{
  //    g.fillAll(juce::Colours::lightcoral);  // ***showregion
}

PlotWindow* Plotter::getWindow()
{
  return findParentComponentOfClass<PlotWindow>();
}

// FIXME: NEED TO HAVE THE ACTUAL VIEW SIZE FOR AXIS
void Plotter::checkFitInView()
{
  if (haxview->isFitInView())
  {
    double avail = 400;
    double xppt = avail/(haxview->numTicks() * haxview->axisIncrements());
    //    std::cout << "avail=" << avail << ", Xppt=" << xppt << "\n";
    if (xppt < 10)
      haxview->setFitInView(false);
  } 
  if (vaxview->isFitInView())
  {
    double avail = 400;
    double yppt = avail/(vaxview->numTicks() * vaxview->axisIncrements());
    //    std::cout << "avail=" << avail << ", Yppt=" << yppt << "\n";
    if (yppt < 10)
      vaxview->setFitInView(false);
  } 
}

void Plotter::fitInView(double width, double height)
{
  bool myfit = false;
  if (haxview->isFitInView())
  {
    if (width == 0.0)
      width = viewport->getMaximumVisibleWidth() ;
    // during window creation on linux this can be called with
    // negative values
    if (width > 0)
    {
      //      std::cout << "fitInView: setting X spread to fit " << width << "\n";
      haxview->setSpreadToFit(width);
      myfit = true;
    }
  }
  if (vaxview->isFitInView())
  {
    if (height == 0.0)
      height = viewport->getMaximumVisibleHeight();
    if (height > 0)
    {
      //      std::cout << "fitInView: setting Y spread to fit " << height << "\n";
      vaxview->setSpreadToFit(height);
      myfit = true;
    }
  } 
  if (myfit)
    resizeForSpread();    
}

void Plotter::resized () 
{
  viewport->setBounds(70, 50, getWidth() - 80, getHeight() - 60);
  //  std::cout << "plotter: x=" << getX() << ", y=" << getY() << ", w=" 
  //            << getWidth() << " h=" << getHeight() << "\n";
  //  std::cout << "viewport: x=" << viewport->getX() << ", y=" << viewport->getY() 
  //            << ", w=" << viewport->getWidth() << " h=" << viewport->getHeight() 
  //            << ", vw=" << viewport->getViewWidth() << " vh=" << viewport->getViewHeight() 
  //            << ", mvw=" << viewport->getMaximumVisibleWidth() << " mvh="
  //            << viewport->getMaximumVisibleHeight() 
  //            << "\n";
  //  std::cout << "plotview: x=" << plotview->getX() << ", y=" << plotview->getY() 
  //            << ", w=" << plotview->getWidth() << " h=" << plotview->getHeight() 
  //            << "\n";
  haxview->setBounds(70, 20, viewport->getVisibleWidth(), 26);
  vaxview->setBounds(2, 50, 64, viewport->getVisibleHeight());
  //  std::cout << "xaxis: x=" << haxview->getX() << ", y=" << haxview->getY()
  //            << ", w=" << haxview->getWidth() << " h=" << haxview->getHeight() << "\n";
}

void Plotter::scrollBarMoved (juce::ScrollBar * sb, const double nrs) {
  // Scrolling callback. Used to scroll the axis views whenever
  // viewport's scrollbar is scrolled.
  juce::String name = sb->getName();
  
  if ( name == "xscroll" )
    haxview->repaint();
  else
    vaxview->repaint();
}

/*=======================================================================*
  Audio Playback
  *=======================================================================*/

void Plotter::pbInitialize()
{
  pbThread = new MidiPlaybackThread(this,50,60);
  double ymin=getVerticalAxis()->getMinimum();
  double ymax=getVerticalAxis()->getMaximum();
  if ((ymin<0.0) || (ymax>127.0) || (ymax<12.0) )
    pbVerticalRescale=true;
  //  pbThread->startThread();  // moved to Editor
  pbTuning=1;
}

void Plotter::pbSetMidiOut(int id)
{
  // FIXME: REMOVE
}

void Plotter::pause()
{
  //  std::cout << "Plotter::pause()\n";
  pbThread->setPaused(true);
}

void Plotter::play(double pos)
{
  //  std::cout << "Plotter::play(" << pos << ")\n";
  // force refinding playback indexes in layers before playing
  positionChanged(pos, false);
  pbThread->setPaused(false);
}

void Plotter::gainChanged(double gain, bool isPlaying)
{
  //  std::cout << "Plotter::gainChanged(" << gain << ", " << isPlaying << ")\n";
  pbThread->setGain(gain);
}

void Plotter::tempoChanged(double tempo, bool isPlaying)
{
  //  std::cout << "Plotter::tempoChanged(" << tempo << ", " << isPlaying << ")\n";
  pbThread->setTempo(tempo);
}

void Plotter::positionChanged(double position, bool isPlaying)
{
  // NB: POINT, LAYER AND AXIS EDITING SHOULD BE DISABLED DURING THIS OPERATION!
  juce::ScopedLock mylock (pbLock);
  //  std::cout << "Plotter::positionChanged(" << position << ", " << isPlaying << ")\n";
  double tobeat = position * getHorizontalAxis()->getMaximum();
  if (isPlaying)
    pbThread->pause();
  pbThread->clear();
  for (int i = 0; i < numLayers(); i++)
  {
    Layer* layer = getLayer(i);
    int next = layer->pbFindNextIndexForBeat(tobeat);
    //    std::cout << "setting pbIndex in layer " << i << " to " << next <<"\n";
    layer->pbIndex=next;
  } 
  pbThread->setPlaybackPosition(tobeat);
  if (isPlaying)
    pbThread->play();
}

// this is called by our playback thread to add midi messages to the
// queue. the position's beat is an X axis value.

void Plotter::addMidiPlaybackMessages(MidiPlaybackThread::MidiMessageQueue& queue,
                                      MidiPlaybackThread::PlaybackPosition& position)
{
  // NB: POINT, LAYER AND AXIS EDITING SHOULD BE DISABLED DURING THIS OPERATION!
  juce::ScopedLock mylock (pbLock);

  for (int i = 0; i < numLayers(); i++)
  {
    Layer* layer = getLayer(i);
    // skip layer if no points or beat later than all points
    if (layer->pbIndex < 0) continue;
    int num = layer->numPoints();
    // skip layer if pbIndex is beyond end
    if (layer->pbIndex >= num) continue;
    // skip layer if muted 
    double amp = layer->pbAmp;
    if (amp == 0.0) continue;

    double dur = layer->pbDur;
    int chan = layer->pbChan;

    while ((layer->pbIndex < num) && layer->getPointX(layer->pbIndex) <= position.beat)
    {
      double key=(pbVerticalRescale) 
        ? getVerticalAxis()->rescale(layer->getPointY(layer->pbIndex), pbMinKey, pbMaxKey) 
        : layer->getPointY(layer->pbIndex);
      int k;
      if (pbTuning == 1)
        k = juce::jlimit(0, 127, juce::roundToInt(key));
      else
      {
        k = (int)key;
        double frac = key - k;
        // how many channels we have to shift for the resolution
        int shift = (int)(frac / pbTuning);
        chan += shift;
      }
      int a = juce::jlimit(0, 127, juce::roundToInt(amp * 127));
      int c = juce::jlimit(0, 15, chan);
      queue.addMessage(new juce::MidiMessage((0x90 | c), k, a, position.beat));
      queue.addMessage(new juce::MidiMessage((0x80 | c), k, 0, position.beat + dur));
      layer->pbIndex++;
    }      
  }
  //  std::cout << "plotter addMidiPlaybackMessages("<<position.beat<<")\n";
}

void Plotter::handleMessage(juce::MidiMessage& midiMessage)
{
  MidiOutPort::getInstance()->sendOut(midiMessage);
}



