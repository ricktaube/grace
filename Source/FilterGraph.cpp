/*=======================================================================*
  Copyright (C) 2012 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#include "FilterGraph.h"

const int FilterGraph::midiChannelNumber = 0x1000;

FilterGraph::FilterGraph(juce::AudioPluginFormatManager& formatManager)
  : juce::FileBasedDocument (".filtergraph",
                             "*.filtergraph",
                             "Load a filter graph",
                             "Save a filter graph"),
    formatManager (formatManager),
    lastUID (0)
{
  InternalPluginFormat internalFormat;

  addFilter (internalFormat.getDescriptionFor (InternalPluginFormat::audioInputFilter),
             0.5f, 0.1f);

  addFilter (internalFormat.getDescriptionFor (InternalPluginFormat::midiInputFilter),
             0.25f, 0.1f);

  addFilter (internalFormat.getDescriptionFor (InternalPluginFormat::audioOutputFilter),
             0.5f, 0.9f);

  setChangedFlag (false);
}

FilterGraph::~FilterGraph()
{
  graph.clear();
}

juce::uint32 FilterGraph::getNextUID() 
{
  return ++lastUID;
}

//==============================================================================
int FilterGraph::getNumFilters()
{
  return graph.getNumNodes();
}

const juce::AudioProcessorGraph::Node::Ptr FilterGraph::getNode (const int index)
{
  return graph.getNode (index);
}

const juce::AudioProcessorGraph::Node::Ptr FilterGraph::getNodeForId (const juce::uint32 uid)
{
  return graph.getNodeForId (uid);
}

void FilterGraph::addFilter (const juce::PluginDescription* desc, double x, double y)
{
  if (desc != NULL)
  {
    juce::String errorMessage;

    juce::AudioPluginInstance* instance
      = formatManager.createPluginInstance (*desc, errorMessage);

    juce::AudioProcessorGraph::Node* node = NULL;

    if (instance != NULL)
      node = graph.addNode (instance);

    if (node != NULL)
    {
      node->properties.set ("x", x);
      node->properties.set ("y", y);
      changed();
    }
    else
    {
      juce::AlertWindow::showMessageBox (juce::AlertWindow::WarningIcon,
                                         TRANS("Couldn't create filter"),
                                         errorMessage);
    }
  }
}

void FilterGraph::removeFilter (const juce::uint32 id)
{
  //    PluginWindow::closeCurrentlyOpenWindowsFor (id);

  if (graph.removeNode (id))
    changed();
}

void FilterGraph::disconnectFilter (const juce::uint32 id)
{
  if (graph.disconnectNode (id))
    changed();
}

void FilterGraph::removeIllegalConnections()
{
  if (graph.removeIllegalConnections())
    changed();
}

void FilterGraph::setNodePosition (const int nodeId, double x, double y)
{
  const juce::AudioProcessorGraph::Node::Ptr n (graph.getNodeForId (nodeId));

  if (n != NULL)
  {
    n->properties.set ("x", juce::jlimit (0.0, 1.0, x));
    n->properties.set ("y", juce::jlimit (0.0, 1.0, y));
  }
}

void FilterGraph::getNodePosition (const int nodeId, double& x, double& y) 
{
  x = y = 0;

  const juce::AudioProcessorGraph::Node::Ptr n (graph.getNodeForId (nodeId));

  if (n != NULL)
  {
    x = (double) n->properties ["x"];
    y = (double) n->properties ["y"];
  }
}

//==============================================================================
int FilterGraph::getNumConnections()
{
  return graph.getNumConnections();
}

const juce::AudioProcessorGraph::Connection* FilterGraph::getConnection (const int index)
{
  return graph.getConnection (index);
}

const juce::AudioProcessorGraph::Connection* FilterGraph::getConnectionBetween (juce::uint32 sourceFilterUID, int sourceFilterChannel,
                                                                                juce::uint32 destFilterUID, int destFilterChannel)
{
  return graph.getConnectionBetween (sourceFilterUID, sourceFilterChannel,
                                     destFilterUID, destFilterChannel);
}

bool FilterGraph::canConnect (juce::uint32 sourceFilterUID, int sourceFilterChannel,
                              juce::uint32 destFilterUID, int destFilterChannel)
{
  return graph.canConnect (sourceFilterUID, sourceFilterChannel,
                           destFilterUID, destFilterChannel);
}

bool FilterGraph::addConnection (juce::uint32 sourceFilterUID, int sourceFilterChannel,
                                 juce::uint32 destFilterUID, int destFilterChannel)
{
  const bool result = graph.addConnection (sourceFilterUID, sourceFilterChannel,
                                           destFilterUID, destFilterChannel);
  if (result)
  {
    changed();
  }
  return result;
}

void FilterGraph::removeConnection (const int index)
{
  graph.removeConnection (index);
  changed();
}

void FilterGraph::removeConnection (juce::uint32 sourceFilterUID, int sourceFilterChannel,
                                    juce::uint32 destFilterUID, int destFilterChannel)
{
  if (graph.removeConnection (sourceFilterUID, sourceFilterChannel,
                              destFilterUID, destFilterChannel))
    changed();
}

void FilterGraph::clear()
{
  //    PluginWindow::closeAllCurrentlyOpenWindows();
  graph.clear();
  changed();
}

//==============================================================================
juce::String FilterGraph::getDocumentTitle()
{
  if (! getFile().exists())
    return "Unnamed";

  return getFile().getFileNameWithoutExtension();
}

juce::Result FilterGraph::loadDocument (const juce::File& file)
{
  juce::XmlDocument doc (file);
  juce::ScopedPointer<juce::XmlElement> xml (doc.getDocumentElement());

  if (xml == NULL || ! xml->hasTagName ("FILTERGRAPH"))
    return  juce::Result::fail("Not a FilterGraph file");

  restoreFromXml (*xml);
  return juce::Result::ok();
}

juce::Result FilterGraph::saveDocument (const juce::File& file)
{
  juce::ScopedPointer<juce::XmlElement> xml (createXml());

  if (! xml->writeToFile (file, juce::String()))
    return juce::Result::fail("Could not write filter file.");

  return juce::Result::ok();
}

juce::File FilterGraph::getLastDocumentOpened()
{
  //  RecentlyOpenedFilesList recentFiles;
  //  recentFiles.restoreFromString (appProperties->getUserSettings()->getValue ("recentFilterGraphFiles"));
  //  return recentFiles.getFile (0);
  return juce::File();
}

void FilterGraph::setLastDocumentOpened (const juce::File& file)
{
  //  RecentlyOpenedFilesList recentFiles;
  //  recentFiles.restoreFromString (appProperties->getUserSettings()->getValue ("recentFilterGraphFiles"));
  //  recentFiles.addFile (file);
  //  appProperties->getUserSettings()->setValue ("recentFilterGraphFiles", recentFiles.toString());
}

//==============================================================================
static juce::XmlElement* createNodeXml (juce::AudioProcessorGraph::Node* const node)
{
  juce::AudioPluginInstance* plugin = dynamic_cast <juce::AudioPluginInstance*> (node->getProcessor());

  if (plugin == NULL)
  {
    jassertfalse
      return NULL;
  }

  juce::XmlElement* e = new juce::XmlElement ("FILTER");
  e->setAttribute ("uid", (int) node->nodeId); // HKT: is nodeId in later JUCE's
  e->setAttribute ("x", node->properties ["x"].toString());
  e->setAttribute ("y", node->properties ["y"].toString());
  e->setAttribute ("uiLastX", node->properties ["uiLastX"].toString());
  e->setAttribute ("uiLastY", node->properties ["uiLastY"].toString());

  juce::PluginDescription pd;
  plugin->fillInPluginDescription (pd);

  e->addChildElement (pd.createXml());

  juce::XmlElement* state = new juce::XmlElement ("STATE");

  juce::MemoryBlock m;
  node->getProcessor()->getStateInformation (m);
  state->addTextElement (m.toBase64Encoding());
  e->addChildElement (state);

  return e;
}

void FilterGraph::createNodeFromXml (const juce::XmlElement& xml)
{
  juce::PluginDescription pd;

  forEachXmlChildElement (xml, e)
  {
    if (pd.loadFromXml (*e))
      break;
  }

  juce::String errorMessage;

  juce::AudioPluginInstance* instance
    = formatManager.createPluginInstance (pd, errorMessage);

  if (instance == NULL)
  {
    // xxx handle ins + outs
  }

  if (instance == NULL)
    return;

  juce::AudioProcessorGraph::Node::Ptr node (graph.addNode (instance, xml.getIntAttribute ("uid") ));

  const juce::XmlElement* const state = xml.getChildByName ("STATE");

  if (state != NULL)
  {
    juce::MemoryBlock m;
    m.fromBase64Encoding (state->getAllSubText());

    node->getProcessor()->setStateInformation (m.getData(), (int) m.getSize());
  }

  node->properties.set ("x", xml.getDoubleAttribute ("x"));
  node->properties.set ("y", xml.getDoubleAttribute ("y"));
  node->properties.set ("uiLastX", xml.getIntAttribute ("uiLastX"));
  node->properties.set ("uiLastY", xml.getIntAttribute ("uiLastY"));
}

juce::XmlElement* FilterGraph::createXml() const
{
  juce::XmlElement* xml = new juce::XmlElement ("FILTERGRAPH");

  int i;
  for (i = 0; i < graph.getNumNodes(); ++i)
  {
    xml->addChildElement (createNodeXml (graph.getNode (i)));
  }

  for (i = 0; i < graph.getNumConnections(); ++i)
  {
    const juce::AudioProcessorGraph::Connection* const fc = graph.getConnection(i);

    juce::XmlElement* e = new juce::XmlElement ("CONNECTION");

    e->setAttribute ("srcFilter", (int) fc->sourceNodeId);
    e->setAttribute ("srcChannel", fc->sourceChannelIndex);
    e->setAttribute ("dstFilter", (int) fc->destNodeId);
    e->setAttribute ("dstChannel", fc->destChannelIndex);

    xml->addChildElement (e);
  }

  return xml;
}

void FilterGraph::restoreFromXml (const juce::XmlElement& xml)
{
  clear();

  forEachXmlChildElementWithTagName (xml, e, "FILTER")
  {
    createNodeFromXml (*e);
    changed();
  }
  int i=0;
  forEachXmlChildElementWithTagName (xml, e, "CONNECTION")
  {
    addConnection ((juce::uint32) e->getIntAttribute ("srcFilter"),
                   e->getIntAttribute ("srcChannel"),
                   (juce::uint32) e->getIntAttribute ("dstFilter"),
                   e->getIntAttribute ("dstChannel"));
  }
  graph.removeIllegalConnections();
}

