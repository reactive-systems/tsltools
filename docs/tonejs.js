var first = true;
document.getElementById("play-button1").addEventListener("click", function() {
  if (first) {

      const synthA = new Tone.Synth().toDestination();
      
      //play a note every quarter-note
      const loopA = new Tone.Loop(time => {
        updateStateMachine();
        console.log(time);
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
