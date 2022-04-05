
// generated code from TSL synthesis

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
