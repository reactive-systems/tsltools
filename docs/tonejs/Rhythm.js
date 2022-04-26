var first = true;
document.getElementById("play-button4").addEventListener("click", function() {
  if (first) {
      const synthA = new Tone.Synth().toDestination();

      //play a note every quarter-note
      const loopA = new Tone.Loop(time => {
        updateStateMachine();
	synthA.triggerAttackRelease(noteToPlay, rhythm, time);
      }, rhythm).start(0);

      first = false;

  }
  if (Tone.Transport.state !== 'started') {

      // the loop starts when the Transport is started
      Tone.Transport.start()

  } else {
    Tone.Transport.stop();
  }
});

var currentState = 0
var noteToPlay = "E4"
var rhythm = "2n"
function updateStateMachine() {
   if (currentState ==  0 ){
        if (true){
            noteToPlay = "E4";
            rhythm = "2n";
            currentState = 1;
        }
    }
    else if (currentState ==  1 ){
        if (true){
            noteToPlay = "G4";
            rhythm = "8n";
            currentState = 0;
        }
    }
}

