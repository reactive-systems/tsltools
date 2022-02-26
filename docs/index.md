Code - [https://github.com/Barnard-PL-Labs/tsltools](https://github.com/Barnard-PL-Labs/tsltools)

# Basic usage

Write a .tsl specification. As an example, let's write a file called `NoteButton.tsl`:

```
always assume {
    F ! buttonPressed(userActivity);
}

always guarantee {
    buttonPressed(userActivity) -> [play <- noteE];
    F [play <- noteE];
    F [play <- noteG];
}
```

Then, run `./tslsynth NoteButton.tsl --python`. The following code will be generated

```
if (currentState ==  0 ):

    if ((buttonPressed userActivity)):
        play = noteE
        currentState = 0

    if (not((buttonPressed userActivity))):
        play = noteG
        currentState = 1

if (currentState ==  1 ):

    if (True):
        play = noteE
        currentState = 0
```

You can save this in a file called noteButton.py (e.g. by running `./tslsynth NoteButton.tsl --python > noteButton.py`), and run it as you would hand-written python code (e.g. `python noteButton.py`).
