package javax.sound.midi;

public class StreamingTrack extends Track {
  private clmml.Sequence sequence;

  public StreamingTrack(clmml.Sequence sequence) {
    super();
    this.sequence = sequence; 
  }
  public boolean add(MidiEvent event) {
    return super.add(event); 
  }
  public boolean remove(MidiEvent event) {
    return super.remove(event);
  }
  public MidiEvent get(int index) {
    update();
    return super.get(index); 
  }
  public int size() {
    update();
    return super.size(); 
  }
  public long ticks() {
    update();
    return super.ticks();
  }
  private void update() {
    if (sequence != null) {
      sequence.update();
    } 
  }
}

