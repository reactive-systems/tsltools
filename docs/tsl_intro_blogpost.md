# Introduction to TSL

### Authors: Danielle Cai (SEAS '22), Rhea Kothari (CC '22)

## What is TSL? 

TSL stands for Temporal Stream Logic. It is a type of temporal logic, which is a logical system containing rules that are propositions of time. Words like “always”, “eventually”, and “until” are common drivers of temporal logic (see [this wikipedia page](https://en.wikipedia.org/wiki/Temporal_logic) for more information on temporal logic). 

Temporal Stream Logic allows for the separation of “control” and “data” ([Finkbeiner et al. 2019](https://arxiv.org/abs/1712.00246)). The term control refers to the temporal logic of a given environment - for example, the rules for turning on and off heat in a room environment. The term data refers to a specific instance that the TSL specification was created to handle. 

Separating control and data means you can write a high-level specification for any logical system that you want, like the heating example, without having to worry about handling real data for a specific instance of this environment. A synthesis tool can then be used to translate this TSL specification into code, by generating a program for a specific instance of incoming data for the environment (note: there could be multiple possibilities for the instantiation of this environment. The synthesis tool will generate only one of these valid possibilities when the TSL specification is synthesized). 

This means that we as creators of this system only have to worry about writing out a set of general rules for an environment - we leave the data processing and code generation to the synthesis tool. This setup does not only make editing code more accessible, but it also demonstrates how TSL can ultimately be a more intuitive language to learn and create logical systems with.

For example: if we were to directly write out the code handling all cases for our heating example, it would look something like this: 

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
      room.heating.on and
      ((gt outside.temperature) room.temperature)):
        [room.heating.ctrl <- turnOff()]
        currentState = 2
```

This is the code for just one state - there are actually four states in total, resulting in four times the lines of code shown. Just trying to think about this system in terms of the logic in this code feels less accessible and more prone to error. It is a way of thinking that is further from what most people naturally do. 

This would be the corresponding TSL specification for the logic above:

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

Even if you don't understand TSL yet, the main takeaway is that the logic is framed temporally - the general rules for heating in this environment are thought about in terms of _when_ heating should be turned on or off. This way of thinking arguably aligns better with how we create logical systems by default; most people would not think in terms of a lower-level state machine to capture the rules of a system. We more naturally think in terms of time, and which factors in a given scenario need to be taken into account over the course of time. TSL therefore serves as a language that more closely mirrors the way we construct logical systems in our minds, and gives synthesis tools the task of translating to relatively lower-level, executable code - so that we don't have to.

## Now let’s look at some examples:

The best way to understand TSL better is to look at examples. Here are four basic TSL specifications that capture how TSL works.

Before we dive into these examples, let’s go over some basic terminology in TSL:

**G**: The symbol for always, as in “always play note `G`”.\
**F**: The symbol for finally, as in “finally play note `G`”. This is the equivalent of “eventually” – so your logic will always eventually play note `G`.\
**U**: The symbol for until, as in “play note `G` until play note `E`”.\
**X**: The symbol for next, as in “next play note `G`”.

You can refer to [this page](https://en.wikipedia.org/wiki/Linear_temporal_logic) on linear temporal logic for more symbols and more background information.

*Assume Block*: The assumption block represents the input we get from our environment (e.g. we assume that a note is always played).

*Guarantee Block*: The guarantee block is a system that we can control through our logic. 


## Example 1:

<table>
<tr>
<th>TSL Specification</th>
<th>Generated Code</th>
</tr>
<tr>
<td>
<pre>
  1 always assume {
  2    
  3 }
  4 always guarantee {
  5     G [play <- noteG];
  6 }
  7
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ):
  2
  3     if (True):
  4         play = noteG
  5         currentState = 0
  6
</pre>

</td>
</tr>
</table>

[Play Example 1](https://barnard-pl-labs.github.io/tsltools/music.html)

This first example plays the note `G` only. Line 5 is the main logic of the code, which guarantees – represented by the symbol “**G**” – that we will play note `G`. Based on this specification, corresponding code will be generated from our logic that will produce a string of notes such as “GGGGGGGG…”.					

## Example 2: 

<table>
<tr>
<th>TSL Specification</th>
<th>Generated Code</th>
</tr>
<tr>
<td>
<pre>
  1 always assume {
  2     
  3 }
  4 
  5 always guarantee {
  6     F ([play <- noteE]);
  7     F ([play <- noteG]);
  8 }
  9
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ):
  2
  3     if (True):
  4         play = noteG
  5         currentState = 1
  6
  7 if (currentState ==  1 ):
  8 
  9     if (True):
  10        play = noteE
  11        currentState = 0
  12
</pre>

</td>
</tr>
</table>

[Play Example 2](https://barnard-pl-labs.github.io/tsltools/music2.html)

The next example generates code that produces a random sequence of `G`s and `E`s. In the TSL specification on lines 6 and 7, we say that the system should always finally play an `E` or a `G`. This means once a `G` is played, after some amount of continuous `G`s, there will be an `E` that is played. And vice versa, after some amount `E`s being continuously played, there will be a `G` that is played. This results in a loop of random `G`s and `E`s being played, such as a string of notes “GGGEEEEGEEGGGGGE…”, “EGEGEGEEEEEEG…”, "GEGEGEGE...".

## Example 3:

<table>
<tr>
<th>TSL Specification</th>
<th>Generated Code</th>
</tr>
<tr>
<td>
<pre>
  1 always assume {
  2     
  3 }
  4 
  5 always guarantee {
  6     F [play <- noteE];
  7     [play <- noteE] -> X [play <- noteG];
  8 }
  9 
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ):
  2
  3     if (True):
  4        play = noteE
  5        currentState = 1
  6
  7 if (currentState ==  1 ):
  8
  9     if (True):
  10        play = noteG
  11        currentState = 0
  12
</pre>

</td>
</tr>
</table>

[Play Example 3](https://barnard-pl-labs.github.io/tsltools/music2.html)

The third example plays an unspecified number of `G`s – can be zero – before playing exactly one `E`, then repeats the unspecified amount of sequence of `G`s followed by 1 `E` exactly. The logic in line 6 says that finally – represented by the symbol “**F**” – we will play an `E` after some time. On line 7, it specifies that as soon as a note `E` is played, next – represented by the symbol “**X**” – we need to play note `G` until we again play a single note `E` as specified on line 6. This logic will produce code that generates a series of notes such as “EGEGEGGGGE”, “GGGGGEGEGE”, or “GEGEGEGE”.

## Example 4: 

<table>
<tr>
<th>TSL Specification</th>
<th>Generated Code</th>
</tr>
<tr>
<td>
<pre>
  1 always assume {
  2    
  3 }
  4 always guarantee {
  5     F [play <- noteE];
  6     [play <- noteE] -> X [play <- noteG];
  7     [play <- noteG] -> X [play <- noteE];
  8 }
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ):
  2
  3     if (True):
  4         play = noteE
  5         currentState = 1
  6
  7 if (currentState ==  1 ):
  8 
  9     if (True):
  10        play = noteG
  11        currentState = 0
  12
</pre>

</td>
</tr>
</table>

[Play Example 4](https://barnard-pl-labs.github.io/tsltools/music2.html)

The final example specifies that we should start with either note `G` or `E` and alternate 1 note at a time between `G` and `E`, but always end on an `E`. Lines 5-7 represent the TSL specifications for the system. On line 5, it guarantees that we will finally play note `E` at the end of the series of notes. Lines 6 and 7 specify that as soon as we play note `E`, the next note we should play is note `G`, and as soon as we play note `G`, the following note to be played should be note `E`. Some possible sequences of notes that can generated from the code are “GEGEGEGEGEGEGE” and “EGEGEGEGE”.

It is interesting to note that the generated code from the two-note specifications are identical across examples 2-4 because "GEGEGE" and "EGEGEGE" – produced by the generated code – are valid realizable sequences of notes for all three TSL specifications. 

