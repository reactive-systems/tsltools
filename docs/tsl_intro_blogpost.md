# TSL for Dummies

## What is TSL? 

TSL stands for Temporal Stream Logic. It is a type of temporal logic, which is a logical system containing rules that are propositions of time. Words like “always”, “eventually”, and “until” are common drivers of temporal logic (see [this wikipedia page](https://en.wikipedia.org/wiki/Temporal_logic) for more information on temporal logic). 
Temporal Stream Logic allows for the separation of “control” and “data” (Finkbeiner, Klein, Piskac, Santolucito). The term control refers to the temporal logic of a given environment - for example, the rules for turning on and off heat in a room environment. The term data refers to a specific instance of a room environment with a specific set of parameters that need to be handled by these rules.

Separating control and data means you can write a high-level specification for any logical system that you want, like the heating example, without having to worry about handling real data for a specific instance of this environment. A synthesis tool can then be used to translate this TSL specification into code, by generating a program for a specific instance of incoming data for the environment (note: there could be multiple possibilities for the instantiation of this environment. The synthesis tool will generate only one of these valid possibilities when the TSL specification is synthesized). 

This means that we as creators of this system only have to worry about writing out a set of general rules for an environment - we leave the data processing and code generation to the synthesis tool. This workflow makes editing the logic much easier. 

If we were to directly write out the code handling all cases for our heating example, it would look something like this: 

```
 if (currentState ==  0 ): 
  

    if (not(room.heating.off) and
      room.heating.on and
      not(((gt outside.temperature) room.temperature))):
        [room.heating.ctrl <- turnOn()]
        currentState = 1
    if (room.heating.off and
      not(room.heating.on)):
        [room.heating.ctrl <- turnOn()]
        currentState = 1

    if (not(room.heating.off) and
      not(room.heating.on)):
        [room.heating.ctrl <- turnOn()]
        currentState = 2
    if (room.heating.off and
      room.heating.on):
        [room.heating.ctrl <- turnOn()]
        currentState = 2
    if (not(room.heating.off) and
      not(room.heating.on)):
        [room.heating.ctrl <- room.heating.ctrl]
        currentState = 2
    if (room.heating.off and
      room.heating.on):
        [room.heating.ctrl <- room.heating.ctrl]
        currentState = 2
    if (not(room.heating.off) and
      not(room.heating.on)):
        [room.heating.ctrl <- turnOff()]
        currentState = 2
    if (room.heating.off and
      room.heating.on):
        [room.heating.ctrl <- turnOff()]
        currentState = 2

    if (not(room.heating.off) and
      room.heating.on and
      ((gt outside.temperature) room.temperature)):
        [room.heating.ctrl <- turnOff()]
        currentState = 2
```

This is the code for just one state - there are actually four states in total, resulting in four times the lines of code shown. The program needs to have many conditional statements to handle all possible instances of the room environment. Making changes to the code is therefore tedious and susceptible to error. 

In a TSL specification, we only have to focus on making sure the logic for a general set of rules is correct, so making changes to our environment is much easier. This is the TSL specification for our heating example:

```
always assume {

  (! (room.heating.off <-> room.heating.on)) ;
  ([ room.heating.ctrl <- turnOn() ]
    -> F ([ room.heating.ctrl <- turnOff() ] R room.heating.on)) ;
  ([ room.heating.ctrl <- turnOff() ]
    -> F ([ room.heating.ctrl <- turnOn() ] R room.heating.off))
 }
always guarantee {

  gt outside.temperature room.temperature
    -> F room.heating.off
```

The file is much shorter, and therefore easier to modify. 

## Now let’s look at some examples:

The best way to understand TSL better is to look at examples. Here are four basic TSL specifications that capture how TSL works.

Before we dive into these examples, let’s go over some basic terminology in TSL:

G: The symbol for always, as in “always play note G”.
F: The symbol for finally, as in “finally play note G”. This is the equivalent of “eventually” – so your logic will always eventually play note G.
U: The symbol for until, as in “play note G until play note E”.
X: The symbol for next, as in “next play note G”.

You can refer to [this page](https://en.wikipedia.org/wiki/Linear_temporal_logic) on linear temporal logic for more symbols and more background information.

Assume Block: The assumption block represents the input we get from our environment (e.g. we assume that a note is always played).

Guarantee Block: The guarantee block is a system that we can control through our logic. 


## Example 1:

```
always assume {
        G(p1) ;
 }
always guarantee {

        F([play <- noteE]) ;
        F([play <- noteG]) ;
 }
```

This first example plays the note G only. Line 5 is the main logic of the code, which guarantees – represented by the symbol “G” – that we will play note G. Based on this specification, corresponding code will be generated from our logic that will produce a string of notes such as “GGGGGGGG…”.					

## Example 2: 

```
always assume {
    G (p1);
}

always guarantee {
    F ([play <- noteE]);
    F ([play <- noteG]);
}
```

The next example generates code that produces a random sequence of Gs and Es. In the TSL specification on lines 6 and 7, we say that the system should always finally play an E or a G. This means once a G is played, after some amount of continuous Gs, there will be an E that is played. And vice versa, after some amount Es being continuously played, there will be a G that is played. This results in a loop of random Gs and Es being played, such as a string of notes “GGGEEEEGEEGGGGGE…” or “EGEGEGEEEEEEG…”.

## Example 3:

```
always assume {
    G (p1);
}

always guarantee {
    F [play <- noteE];
    [play <- noteE] -> X [play <- noteG];
}
```

The third example plays an unspecified number of Gs – can be zero – before playing exactly one E, then repeats the unspecified amount of sequence of Gs followed by 1 E exactly. The logic in line 6 says that finally – represented by the symbol “F” – we will play an E after some time. On line 7, it specifies that as soon as a note E is played, next – represented by the symbol “X” – we need to play note G until we again play a single note E as specified on line 6. This logic will produce code that generates a series of notes such as “EGEGEGGGGE”, “GGGGGEGEGE”, or “EGGGGEGGGE”.

## Example 4: 

```
always assume {
    G (p1);
}
always guarantee {
    F [play <- noteE];
    [play <- noteE] -> X [play <- noteG];
    [play <- noteG] -> X [play <- noteE];
}
```

The final example specifies that we should start with either note G or E and alternate 1 note at a time between G and E, but always end on an E. Lines 5-7 represent the TSL specifications for the system. On line 5, it guarantees that we will finally play note E at the end of the series of notes. Lines 6 and 7 specify that as soon as we play note E, the next note we should play is note G, and as soon as we play note G, the following note to be played should be note E. Some possible sequences of notes that can generated from the code are “GEGEGEGEGEGEGE” and “EGEGEGEGE”.

