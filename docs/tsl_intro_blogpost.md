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
  5     G [noteToPlay <- G4];
  6 }
  7
</pre>
</td>
<td>

<pre>
  1 if (state ==  0){
  2
  3     if (true){
  4         noteToPlay = G4;
  5         nextState = 0;
  6     }
  7 }
  8 state = nextState;
  9
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
  6     F ([noteToPlay <- E4]);
  7     F ([noteToPlay <- G4]);
  8 }
  9
</pre>
</td>
<td>

<pre>
  1 if (state ==  0 ){
  2
  3     if (true){
  4         noteToPlay = G4;
  5         nextState = 1;
  6     }
  7 }
  8
  9 if (state ==  1 ){
  10 
  11    if (true){
  12        noteToPlay = E4;
  13        nextState = 0;
  14    }
  15 }
  16 state = nextState;
  17
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
  6     F [noteToPlay <- E4];
  7     [noteToPlay <- E4] -> X [noteToPlay <- G4];
  8 }
  9 
</pre>
</td>
<td>

<pre>
  1 if (state ==  0 ){
  2
  3     if (true){
  4         noteToPlay = E4;
  5         nextState = 1;
  6     }
  7 }
  8
  9 if (state ==  1 ){
  10 
  11    if (true){
  12        noteToPlay = G4;
  13        nextState = 0;
  14    }
  15 }
  16 state = nextState;
  17
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
  5     F [noteToPlay <- E4];
  6     [noteToPlay <- E4] -> X [noteToPlay <- G4];
  7     [noteToPlay <- G4] -> X [noteToPlay <- E4];
  8 }
</pre>
</td>
<td>

<pre>
  1 if (state ==  0 ){
  2
  3     if (true){
  4         noteToPlay = E4;
  5         nextState = 1;
  6     }
  7 }
  8
  9 if (state ==  1 ){
  10 
  11    if (true){
  12        noteToPlay = G4;
  13        nextState = 0;
  14    }
  15 }
  16 state = nextState;
  17
</pre>

</td>
</tr>
</table>

[Play Example 4](https://barnard-pl-labs.github.io/tsltools/music2.html)

The fourth example specifies that we should start with either note `G` or `E` and alternate 1 note at a time between `G` and `E`, but always end on an `E`. Lines 5-7 represent the TSL specification for the system. On line 5, it guarantees that we will finally play note `E` at the end of the series of notes. Lines 6 and 7 specify that as soon as we play note `E`, the next note we should play is note `G`, and as soon as we play note `G`, the following note to be played should be note `E`. Some possible sequences of notes that can generated from the code are “GEGEGEGEGEGEGE” and “EGEGEGEGE”.

It is interesting to note that the generated code from the two-note specifications are identical across examples 2-4 because "GEGEGE" and "EGEGEGE" – produced by the generated code – are valid realizable sequences of notes for all three TSL specifications. 

## Example 5: 

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
  5     F [noteToPlay <- E4];
  6     F [noteToPlay <- G4];
  7     F [noteToPlay <- C4];
  8 }
  9
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ){
  2     if (true){
  3         noteToPlay = C4;
  4         currentState = 1;
  5     }
  6 }
  7 else if (currentState ==  1 ){
  8     if (true){
  9         noteToPlay = E4;
  10        currentState = 2;
  11    }
  12 }
  13 else if (currentState ==  2 ){
  14     if (true){
  15         noteToPlay = G4;
  16         currentState = 0;
  17     }
  18 }
  19
</pre>

</td>
</tr>
</table>

[Play Example 5](https://barnard-pl-labs.github.io/tsltools/ThreeNotesRand.html)

The fifth example generates code that produces a random sequence of `G`s, `E`s, and `C`s. In the TSL specification on lines 5 to 7, we say that the system should always eventually play an `E` or a `G` or a `C`. This means once a `G` is played, after some amount of continuous `G`s, there will be an `E` or a `C` that is played, and after an `E` is played there will then be a `C` or a `G` that is played, and so on and so forth. This results in a loop of random `G`s, `E`s, and `C`s being played, such as a string of notes “GGGEEEECCCCCGEECCCGGGGGE…” or "GGGCCCCEEEECGE...".


## Example 6 [RHEA]: 

<table>
<tr>
<th>TSL Specification</th>
<th>Generated Code</th>
</tr>
<tr>
<td>
<pre>
  1  always assume {
  2 
  3  }
  4  always guarantee {
  5      F [noteToPlay <- E4];
  6      [noteToPlay <- E4] -> X [noteToPlay <- G4];
  7      [noteToPlay <- G4] -> X [noteToPlay <- C4];
  8      [noteToPlay <- C4] -> X [noteToPlay <- E4];
  9  }
</pre>
</td>
<td>

<pre>
  1 
  2 if (state ==  0 ){
  3 
  4     if (true){
  5         noteToPlay = E4;
  6         nextState = 1;
  7 }
  8 }
  9 
 10 if (state ==  1 ){
 11 
 12     if (true){
 13         noteToPlay = G4;
 14         nextState = 2;
 15 }       
 16 }
 17 
 18 if (state ==  2 ){
 19 
 20     if (true){
 21         noteToPlay = C4;
 22         nextState = 0;
 23 }       
 24 }       
 25 state = nextState;
</pre>

</td>
</tr>
</table>

[Play Example 6](https://barnard-pl-labs.github.io/tsltools/ThreeNotesRand.html)

The sixth example generates code that produces an ordered sequence of `E`, `G`, and `C`, . In the TSL specification on lines 5 to 7, we say that eventually play `E`, and once it plays `E` it must play `G` next, and once it plays `G` it must play `C` next. Therefore the resulting sequence continues to play `G` after `E` and `C` after `G`, but it will always return back to `E` and restart this sequence of notes.

## Example 7: 

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
  5     F [noteToPlay <- E4];
  6     [noteToPlay <- E4] -> X [noteToPlay <- G4];
  7     [noteToPlay <- G4] -> X [noteToPlay <- C4];
  8     [noteToPlay <- C4] -> X [noteToPlay <- E4];
  9 }
  10
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ){
  2     if (true){
  3         noteToPlay = E4;
  4         currentState = 1;
  5     }
  6 }
  7 else if (currentState ==  1 ){
  8     if (true){
  9         noteToPlay = G4;
  10        currentState = 2;
  11    }
  12 }
  13 else if (currentState ==  2 ){
  14     if (true){
  15        noteToPlay = C4;
  16        currentState = 0;
  17    }
  18 }
  19
</pre>

</td>
</tr>
</table>

[Play Example 7](https://barnard-pl-labs.github.io/tsltools/ThreeNotesE.html)

The seventh example specifies that we should start with either note `G`, `E`, or `C` and alternate 1 note at a time between the three notes, but always end on an `E`. Lines 5-8 represent the TSL specification for the system. Line 5 guarantees that we will finally play note `E` at the end of the series of notes. Line 6 says as soon as we play note `E`, we should next play note `G`. Line 7 says as soon as we play note `G`, next we must play note `C`. Line 8 says as soon as we play note `C`, note `E` should be played. This generates a sequence of notes such as “GCEGCEGCEGCE” or “CEGCEGCEGE” for example.

## Example 8: 

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
  5     ([noteToPlay <- E4]) -> X [rhythm <- eigthnote];
  6     ([noteToPlay <- G4]) -> X [rhythm <- halfnote];
  7     ([noteToPlay <- E4]) -> X [noteToPlay <- G4];
  8     ([noteToPlay <- G4]) -> X [noteToPlay <- E4];
  9     F ([noteToPlay <- E4]);
  10
</pre>
</td>
<td>

<pre>
  1 if (currentState ==  0 ){
  2    if (true){
  3         noteToPlay = E4;
  4         rhythm = halfnote;
  5         currentState = 1;
  6     }
  7 }
  8 else if (currentState ==  1 ){
  9     if (true){
  10        noteToPlay = G4;
  11        rhythm = eigthnote;
  12        currentState = 0;
  13    }
  14 }
  15
</pre>

</td>
</tr>
</table>

[Play Example 8](https://barnard-pl-labs.github.io/tsltools/Rhythm.html)

The eigth example specifies that we should start with either note `G` or `E` and alternate 1 note at a time between the two notes, but always end on an `E`. Additionally, when we play a `E`, we play a half note, and when we play a `G`, we play an eigth note, specified in lines 5 and 6. Lines 5 and 7 say when we play an `E`, the next note to be played should be a G that is an eigth note. Lines 6 and 8 say that after we play the G, the next note to be played should be an `E` that is a half note. This pattern repeats until we end on an `E`, as specified in line 9 of the TSL Specification.


