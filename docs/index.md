Code - https://github.com/Barnard-PL-Labs/tsltools

# Basic usage

Write a .tsl specification. As an example, let's write a file called Simple.tsl:

```
always assume {
  
}
always guarantee {
   F ([x <- y]);
   F ([x <- z]);
}
```

Then, run `./tslsynth Simple.tsl --python`. The following code will be generated

```
if (currentState ==  0 ):

    if (True):
        x = z
        currentState = 1

if (currentState ==  1 ):

    if (True):
        x = y
        currentState = 0
```

You can save this in a file called simple.py, and run it as you would hand written python code.
