document.getElementById("play-button").addEventListener("click", function() {
  
  if (Tone.Transport.state !== 'started') {
    const synthA = new Tone.Synth().toDestination();
    //play a note every quarter-note
    const loopA = new Tone.Loop(time => {
      synthA.triggerAttackRelease("G4", "8n", time);
    }, "4n").start(0);
    
    Tone.Transport.start();
    
  } else {
    Tone.Transport.stop(); 
  }
  
});

