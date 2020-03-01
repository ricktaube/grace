/*=======================================================================*
  Copyright (C) 2012 Rick Taube.                                        
  This program is free software; you can redistribute it and/or modify  
  it under the terms of the Lisp Lesser Gnu Public License. The text of 
  this agreement is available at http://www.cliki.net/LLGPL             
 *=======================================================================*/

#ifndef CM_FILTERGRAPH_H
#define CM_FILTERGRAPH_H

#include "Libraries.h"

class FilterInGraph;
class FilterGraph;

//==============================================================================
// InternalPluginFormat
//==============================================================================

/** Manages the graph's internal IOProcessor nodes. */

class InternalPluginFormat   : public juce::AudioPluginFormat
{

public:

  InternalPluginFormat()
  {
    {
      juce::AudioProcessorGraph::AudioGraphIOProcessor p (juce::AudioProcessorGraph::AudioGraphIOProcessor::audioOutputNode);
      p.fillInPluginDescription (audioOutDesc);
    }

    {
      juce::AudioProcessorGraph::AudioGraphIOProcessor p (juce::AudioProcessorGraph::AudioGraphIOProcessor::audioInputNode);
      p.fillInPluginDescription (audioInDesc);
    }

    {
      juce::AudioProcessorGraph::AudioGraphIOProcessor p (juce::AudioProcessorGraph::AudioGraphIOProcessor::midiInputNode);
      p.fillInPluginDescription (midiInDesc);
    }
  }

  ~InternalPluginFormat()
  {
  }

  enum InternalFilterType
    {
      audioInputFilter = 0,
      audioOutputFilter,
      midiInputFilter,
      endOfFilterTypes
    };

  const juce::PluginDescription* getDescriptionFor (const InternalFilterType type)
  {
    switch (type)
    {
    case audioInputFilter:      return &audioInDesc;
    case audioOutputFilter:     return &audioOutDesc;
    case midiInputFilter:       return &midiInDesc;
    default:                    break;
    }
    return 0;
  }

  void getAllTypes (juce::OwnedArray <juce::PluginDescription>& results)
  {
    for (int i = 0; i < (int) endOfFilterTypes; ++i)
      results.add (new juce::PluginDescription (*getDescriptionFor ((InternalFilterType) i)));
  }

  juce::String getName() const
  {
    return "Internal";
  }

  bool fileMightContainThisPluginType (const juce::String&)         
  {
    return false;
  }

  juce::FileSearchPath getDefaultLocationsToSearch()                
  {
    return juce::FileSearchPath();
  }

  void findAllTypesForFile (juce::OwnedArray <juce::PluginDescription>&, const juce::String&)
  {
  }

  bool doesPluginStillExist (const juce::PluginDescription&)
  {
    return true;
  }

  juce::String getNameOfPluginFromIdentifier (const juce::String& fileOrIdentifier)
  {
    return fileOrIdentifier;
  }

  juce::StringArray searchPathsForPlugins (const juce::FileSearchPath&, bool)
  {
    return juce::StringArray();
  }

  bool canScanForPlugins() const
  {
    return false;
  }

  /** Create internal plugin representations of the processor graph's
      IO nodes. This allows user plugins to connect to audio/midi IO
      using graph node connections.  **/

  juce::AudioPluginInstance* createInstanceFromDescription (const juce::PluginDescription& desc)
  {
    if (desc.name == audioOutDesc.name)
    {
      return new juce::AudioProcessorGraph::AudioGraphIOProcessor (juce::AudioProcessorGraph::AudioGraphIOProcessor::audioOutputNode);
    }
    else if (desc.name == audioInDesc.name)
    {
      return new juce::AudioProcessorGraph::AudioGraphIOProcessor (juce::AudioProcessorGraph::AudioGraphIOProcessor::audioInputNode);
    }
    else if (desc.name == midiInDesc.name)
    {
      return new juce::AudioProcessorGraph::AudioGraphIOProcessor (juce::AudioProcessorGraph::AudioGraphIOProcessor::midiInputNode);
    }
    return 0;
  }

private:

  juce::PluginDescription audioInDesc;
  juce::PluginDescription audioOutDesc;
  juce::PluginDescription midiInDesc;
};

//==============================================================================
// FilterConnection
//==============================================================================

/** Represents a connection between two pins in a FilterGraph. */

class FilterConnection
{

public:

  FilterConnection (FilterGraph& ownr)
  : owner (ownr)
  {
  }

  FilterConnection (const FilterConnection& other)
  : sourceFilterID (other.sourceFilterID),
    sourceChannel (other.sourceChannel),
    destFilterID (other.destFilterID),
    destChannel (other.destChannel),
    owner (other.owner)
  {
  }

  ~FilterConnection()
  {
  }

  juce::uint32 sourceFilterID;
  int sourceChannel;
  juce::uint32 destFilterID;
  int destChannel;
  juce_UseDebuggingNewOperator
  private:
  FilterGraph& owner;
  FilterConnection& operator= (const FilterConnection&);
};

//==============================================================================
// FilterGraph
//==============================================================================

/** A collection of filters and some connections between them. */

class FilterGraph   : public juce::FileBasedDocument
{
  juce::AudioPluginFormatManager& formatManager;
public:

  FilterGraph(juce::AudioPluginFormatManager& formatManager);
  ~FilterGraph();
  juce::AudioProcessorGraph& getGraph() 
  {
    return graph;
  }
  int getNumFilters() ;
  const juce::AudioProcessorGraph::Node::Ptr getNode (const int index) ;
  const juce::AudioProcessorGraph::Node::Ptr getNodeForId (const juce::uint32 uid) ;
  void addFilter (const juce::PluginDescription* desc, double x, double y);
  void removeFilter (const juce::uint32 filterUID);
  void disconnectFilter (const juce::uint32 filterUID);
  void removeIllegalConnections();
  void setNodePosition (const int nodeId, double x, double y);
  void getNodePosition (const int nodeId, double& x, double& y) ;
  int getNumConnections() ;
  const juce::AudioProcessorGraph::Connection* getConnection (const int index) ;
  const juce::AudioProcessorGraph::Connection* getConnectionBetween (juce::uint32 sourceFilterUID, int sourceFilterChannel,
                                                                     juce::uint32 destFilterUID, int destFilterChannel) ;
  bool canConnect (juce::uint32 sourceFilterUID, int sourceFilterChannel, juce::uint32 destFilterUID, int destFilterChannel) ;
  bool addConnection (juce::uint32 sourceFilterUID, int sourceFilterChannel, juce::uint32 destFilterUID, int destFilterChannel);
  void removeConnection (const int index);
  void removeConnection (juce::uint32 sourceFilterUID, int sourceFilterChannel,
                         juce::uint32 destFilterUID, int destFilterChannel);

  void clear();
  juce::XmlElement* createXml() const;
  void restoreFromXml (const juce::XmlElement& xml);

  juce::String getDocumentTitle();
  juce::Result loadDocument (const juce::File& file);
  juce::Result saveDocument (const juce::File& file);
  juce::File getLastDocumentOpened();
  void setLastDocumentOpened (const juce::File& file);

  /** The special channel index used to refer to a filter's midi channel.
   */
  static const int midiChannelNumber;
  juce::AudioProcessorGraph graph;
  juce::AudioProcessorPlayer player;

private:


  juce::uint32 lastUID;
  juce::uint32 getNextUID() ;

  void createNodeFromXml (const juce::XmlElement& xml);

  JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (FilterGraph);
};

#endif
