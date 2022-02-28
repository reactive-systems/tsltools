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

```python
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

To generate a visualization of this code as a state machine, run `./tslsynth NoteButton.tsl --xstate`. 
You will get `xstate` code that can be visualized as follows:

<iframe src="https://stately.ai/viz/embed/d3cae950-e307-4e9b-b0fa-a6917207fb96?mode=viz&panel=code&showOriginalLink=1&readOnly=1&pan=0&zoom=0&controls=0"
width="100%" height="500" frameborder="0" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"
allow="accelerometer; ambient-light-sensor; camera; encrypted-media; geolocation; gyroscope; hid; microphone; midi; payment; usb; vr; xr-spatial-tracking"
sandbox="allow-forms allow-modals allow-popups allow-presentation allow-same-origin allow-scripts"
></iframe>

