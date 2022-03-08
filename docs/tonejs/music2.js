var first = true;
document.getElementById("play-button").addEventListener("click", function() {
  if (first) {
      const synthA = new Tone.Synth().toDestination();
      
      //play a note every quarter-note
      const loopA = new Tone.Loop(time => {
        updateStateMachine();
	synthA.triggerAttackRelease(noteToPlay, "8n", time);
      }, "4n").start(0);

      first = false;

  }
  if (Tone.Transport.state !== 'started') {
    
      // the loop starts when the Transport is started
      Tone.Transport.start()

  } else {
    Tone.Transport.stop();
  }
});

var state = 0
var noteToPlay = "E4"
function updateStateMachine() {
  if (state == 0) {
    state = 1
    noteToPlay = "G4"
  }
  else {
    state = 0
    noteToPlay = "E4"
  }
}

