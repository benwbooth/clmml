package clmml;

import javax.sound.midi.Sequencer;
import javax.sound.midi.Track;
import javax.sound.midi.StreamingTrack;
import javax.sound.midi.InvalidMidiDataException;
import java.util.List;
import java.util.ArrayList;

public class Sequence extends javax.sound.midi.Sequence {
  private Sequencer sequencer;
  private List<StreamAdvanceListener> listeners = new ArrayList<StreamAdvanceListener>();

  public Sequence(Sequencer sequencer, float divisionType, int resolution) 
    throws InvalidMidiDataException 
  {
    super(divisionType, resolution); 
    this.sequencer = sequencer;
  }

  public Sequence(Sequencer sequencer, float divisionType, int resolution, int numTracks) 
    throws InvalidMidiDataException 
  {
    super(divisionType, resolution);
    for (int i = 0; i < numTracks; i++) {
        tracks.addElement(new StreamingTrack(this));
    }
  }

  public Sequencer getSequencer() {
    return sequencer; 
  }
  public synchronized void setSequencer(Sequencer sequencer) {
    this.sequencer = sequencer;
  }

  public synchronized Track createTrack() {
    Track track = new StreamingTrack(this);
    tracks.addElement(track);
    return track;
  }

  public synchronized void addStreamAdvanceListener(StreamAdvanceListener s) {
    listeners.add(s); 
  }
  public synchronized boolean removeStreamAdvanceListener(StreamAdvanceListener s) {
    return listeners.remove(s); 
  }

  public synchronized void update() {
    for (StreamAdvanceListener s : listeners) {
      if (s != null) {
        s.advance(this);
      }
    }
  }
}

