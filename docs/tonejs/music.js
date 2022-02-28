const synth = new Tone.Synth().toMaster()
synth.triggerAttackRelease('C4', '8n')

document.getElementById("play-button").addEventListener("click", function() {
  if (Tone.Transport.state !== 'started') {
    
      // create two monophonic synths
      const synthA = new Tone.Synth().toDestination();
      const synthB = new Tone.Synth().toDestination();
      //play a note every quarter-note
      const loopA = new Tone.Loop(time => {
        synthA.triggerAttackRelease("E4", "8n", time);
      }, "4n").start(0);
      //play another note every off quarter-note, by starting it "8n"
      const loopB = new Tone.Loop(time => {
        synthB.triggerAttackRelease("G4", "8n", time);
      }, "4n").start("8n");
      // the loops start when the Transport is started
      Tone.Transport.start()

  } else {
    Tone.Transport.stop();
  }
});