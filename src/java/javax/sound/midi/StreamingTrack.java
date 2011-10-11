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
      Sequencer sequencer = this.sequence.getSequencer();
      if (sequencer != null) {
        long tickPos = sequencer.getTickPosition(); 
        long tick = super.ticks();
        long updateTick = tickPos - tick;
        if (updateTick > 0) {
          sequence.update(updateTick); 
        }
      }
    } 
  }
}

