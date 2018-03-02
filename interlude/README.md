Yet Another Vertical Scrolling Rhythm Game / Percyqaz

This is the document where I explain what will be in this project and all reasoning behind design choices and stuff.

Most of the finer details on how and why things work will be moved to the github wiki as a more permanent and professional styled location.

Contents:
Why I’d make my own rhythm game
Client
Difficulty rating
Game mechanics as a consequence
Planned features
Editor
Planned features
File format

Planned features (definitely):
Chord cohesion
“SVs” aka scroll speed changes
Some sort of skinning functionality
DDR note colors (and lots of others)
Saving your scores
Rates
Screen covers
The ability for everything above to be customised or optional
Planned features (probably):
Auto loading/conversion of .sm and .osu
Hitsounds
Judgement lines with customisable density
HP meter and drain mechanics
Difficulty graphs, overall and per skillset
Playlists of maps, supporting playing back to back and fancy breakdown score screen
Upscroll as well as downscroll which will be added first
Planned features (maybe):
Online leaderboards (super unlikely, just play etterna or osu)

Why make my own rhythm game?

Osu is not up to scratch because:
Star rating sucks total nuts in every aspect possible
No rates ingame
Accuracy and grading system is too easy, even on scorev2
Levels don’t get a skillset breakdown to tell you what to work on
I can’t edit the code and make just these changes cause it’s closed source
Osu!lazer isn’t coming anytime soon

Etterna/any fork of stepmania is not up to scratch because:
Noone cares about long notes
Noone cares about SVs
Noone cares about other keymodes (like 7k stuff)
Etterna difficulty rating is not for other keymodes 
Etterna difficulty rating AFAIK does not account for long note patterns
The codebase is spaghetti and I don’t want to work with it
Etterna is streamlining into a single playstyle for leaderboards but i want to give players a shit ton of options to mess around with
I like hitsounds when you press the button rather than assisted tick

Things I want to bring to a VSRG that I haven’t seen:
Note colors based on chords (and some others)
Actual careful attention to rating long notes (and maybe SVs?)
Some other funny stuff i’ll think of later
Difficulty Rating

Difficulty rating is a measure of how hard it is to play the patterns in a map (at their corresponding speed)
This is the most important topic to discuss and clear up because it will lay the foundation of the game mechanics and stuff.

This section measures the physical difficulty of a map. The intention is to ignore technical rhythms and coordination stuff for another tech rating system.
I will be presenting both a physical and technical rating to players to measure difficulty.

Here’s my multiple step plan (and why):

Step 1: How difficult is that movement?

For each movement, or row of notes, I split it into (2) hands which are totally independent of each other. (Coordinating them together for rhythms goes in technical rating).
The difficulty calculator will need to know what layout you’re playing (i.e 3k+2 for 5k, which thumb goes on which hand in 7k, if you’re playing index for 4k, etc).
You will be able to make rating higher than it should be by playing the wrong layout. I will make it clear what it thinks you’re playing on the score screen and assume players won’t be retards with it.
Index playstyles might be modelled as 4 hands of a single finger (since it features no trilling on a single hand) but it might be better to model it with a modified version of the calculator algorithm.

So then I will calculate the difficulty of hitting each note (within the hand)

Here’s how I plan to do it:

Break up levels into movements, that is, the transitions between consecutive chords/notes
Rate how difficult this movement is
Divide the rating by the time between the two chords for one data point
Build up a set of data points for the whole map and process them in some way 
This applies to any number of keys

The consequences of this system are:
It’s not based on notes per second so it won’t have naive assumptions about jackhammers or similar other patterns with high note density but very little intensity
It’s not based on looking at the patterns and deciding what is hard and what isn’t - This brings in human factors like deliberately nerfing rolls because you can manipulate them etc that I would rather not deal with.
Each movement difficulty rating is proportional to rate. 1.1x rate means a movement is 1.1x as “difficult”. Depending on how I process the data points, however, means the final difficulty rating may not be proportional in the same way. (Some patterns scale to be a lot more difficult than others)

The bits I underlined are things that need to be decided, and I have them whittled down to a couple of options.

Options for processing the data in some way:

Taking a mean
Gives you the average “difficulty” of all movements. High density patterns will make up more of this value since they contain more movements per the time they span so this will skew the difficulty towards the dense bits. This is also affected by difficulty spikes a lot. Likely to turn out garbage.

Taking a mean but accounting for movement density
So you multiply the data point by the percentage of the map it takes up. Seems fairish because breaks will take down the difficulty in stamina maps, BUT breaks will take down the difficulty in any map since they make up a large portion of the map that is one, very easy movement. Detecting what is and isn’t a break would be a pain to design and probably abused by sticking a note in the middle. Adding a break and then a single note to the end of the map significantly lowers difficulty even though your overall accuracy would be unaffected.
Some weird time-series thing
I’ve been working on some fancy algorithm that looks through the data points in sequence and kinda gives a reading on how much strain your hands are feeling at this point in the map. It could simulate stamina and how people can actually hit short difficulty spikes just fine as long as they don’t last for an extended period of time. These regular readings at intervals form a new set of data and the algorithm can be repeated on the new data over and over until you are left with one data point, the overall rating.

I’ll try and do this one but it will take a while

Rating how difficult a movement is

These calculations will happen for different hands independently, and for each movement the values per hand are combined using an algorithm for the final values

So i’m splitting each movement into:

Strain, that is, how much you have to move your fingers around to hit a pattern. Think 7k brackets or 4k split jumptrills. They have high strain values.

Jacks, that is, how many hands (realistically up to 2) that must repeat a note between movement and therefore do a jackhammer movement.

Long note strain, exact same calculation as Strain, except it’s between the notes you’re holding and the tap notes you have to hit around it. Again, probably hold notes + bracket in 7k is the best example of where this will be high. This will likely have a different multiplier to strain because it’s normally harder to hit these.

Things I am going to assume are true for my calculations:

Faster movements are harder.

So basically I can add these all together to get a rating for movement. The HARD BIT is weighting all of these values correctly to make fair overall ratings. This is where the human decision/possible error enters the process. I think the individual parts, strain/jack/ln strain should work by themselves, but not relative to each other.

Actually calculating values

I’ve mentioned jacks, you just count how many hands have repeated notes. I am therefore saying if both repeated notes are on one hand, it’s no harder than just 1 repeated note (unless you also have to move your fingers like for say, an extra key to the side in 7k).

Strain calculation (this is what I have so far and this is what I probably need most feedback on, however the feedback is only needed for 5k and above) is done like this:

If you swap between one finger and a finger directly next to it, that’s a strain value of 1. If both hands do this it’s a strain value of 2. <- This is all 4k players need to care about

If you swap between fingers with a column between them, the strain value is ½. Two columns between, ⅓.
If you go from [12] to [4], assuming your left hand covers [1234] in 7k, you add the strain values. ⅓ + ½.
I think this is a good idea because most of the actual difficulty in trilling is that the finger muscles next to each other directly affect each other. As you space the fingers further apart on the hand you can kinda trill with your wrist so the actual finger movements are less.

This also applies to long note patterns pretty well, because it is hardest to tap with a finger directly between two fingers that are holding a note and easier if you’re holding with your thumb and tapping with your ring finger. This is my opinion and it is fine for you to think “no this is shit” if you can explain where this system fails.

Note: trills across two hands do NOT have strain value because you normally don’t move your fingers, just your hands up and down.

That’s basically it…

That’s all i’m doing to create these values. So to run down:
Jack value = number of hands with repeated notes
Strain value = measure of strain moving from taps and ends of long notes to taps and starts of long notes
Long note strain value = measure of strain from middles of long notes being currently held to ends of long notes, taps and starts of long notes.
These should only apply to hands individually and then join the values together afterwards

What about technical maps?
I actually have so many ideas for how to rate tech that I’m going to need a section further down dedicated to it.

LN Strain values account for having to release a long note while holding others down. It’s a skill and it’s a measurable skill that deserves a decent rating.

The consequences of the system I’ve just described

All chords and stuff are viewed as a single movement. That means patterns with lots of notes in need to be weighted down or they will inflate accuracy compared to difficulty rating. I have two options:

Chord cohesion

You hit the whole chord as one “note” with one judgement value. I will take the mean timing error of all the notes in the chord (ignoring misses) and put that into hit window values for judgement. Every miss in the chord lowers the rating by 2 levels (marvellous to good, perfect to ok etc) and any miss will break your combo (still wondering if combo should matter, it will prob be cosmetic). The absolute timing error is used so you won’t be exploiting any chords by hitting both early and late to average well timed.

This is a basically mandatory feature because the alternative is to weight each note with less value if it’s in a chord. If I do this it will be a pain to implement mechanic/consequence number two:

I’m calling certain long note patterns difficult, so it must be mandatory that players hit them correctly.

Long note mechanics
If you hit a movement while you’re supposed to be holding notes and I catch you not holding those notes, for each one I will decrease the judgement by 2 levels and combo break. Gets them every time. This is why I will be using chord cohesion, so I can penalise entire chords with less programming.

If you release a note between movements and you were supposed to be holding it, you will combo break. No accuracy lost as long as you put your finger back on it and release at the right time.

Beginning and ends of long notes will have their own hit windows because all the stuff so far has been viewing maps as a list of sets of actions happening at once, rather than a list of hit objects tracking a whole action like holding a note. It just makes sense.

Side note: I will probably be a bit lenient on long note releases, like you can use the perfect hit window to get a marvelous. 

Manipulation (Problem 3)

I’m rating rolls and stuff as if you are hitting every single note perfectly in time as opposed to manipulating patterns. I can either stop doing this, or make players stop manipulating.

The options:
1. Totally nerf manipulation into the ground so that you can only not get misses if you hit notes in these insanely tiny hit windows to show that you’re definitely to the metronome and nailing fast stairs and stuff. Won’t be fun at all, but I think I know how to go about doing this.

2. Account for manipulation in rating. Can do this by calculating how easy it is to manip two notes that are very close together by comparing time apart to hit window sizes. The calculation looks like this: 
 
Time between notes / Hit window size. If this number is below 1 set the rating for this motion to 0, it can be hit as two notes together. For values between 1 and 2, divide by some factor*. Above 2, ignore it, you can’t manipulate the notes without missing.

*May be subject to change for balancing purposes

3. Do nothing. Manipulation is a skill (I don’t think this is a good idea because of how much of a boost this could give players COUGH OSU MANIA COUGH on maps designed heavily around manipulating patterns).

I plan on sticking to option 2 unless anything comes to my attention

What is tech?

It even gets its own bold heading cause I think it’s a decently big deal.

I think the plan is to have a tech rating that comes alongside difficulty rating. The difficulty rating tells you how physically impressive it is to play a level. The tech rating tells you about other notability or impressiveness about getting a good score on the level.

Technical maps refer to:
Weird rhythms like ⅓ and ¼ beats mixed together
Weird rhythms like barely even snapped beats set out in clever ways
I’m gonna bundle SV changes in here too

So I’m gonna go ahead and generalise by saying a level with solid ¼ beat movements the whole way through, i.e a long jumpstream map, has a low technical rating. If you mix rolls and gaps without a note in there though, it becomes more technical and gets a higher rating.
Gaps normally make a level physically easier but more difficult to actually execute because you have to pay careful attention to rhythm and stuff.
I think the tech rating deserves a decent amount of care and attention because it’s about actual rhythm game skill on maps a lot of players could perform poorly on even though it isn’t theoretically very hard.

I have some proposed algorithms for the purpose of determining the technical difficulty of levels that I’ve been sitting on for a few weeks:

Technical rhythms

Compare the difference in time between one movement and the next, and find their ratio. If it’s 1, no tech, and the bigger and more often the difference, the more techy. You could feed numbers into an algorithm similar to the difficulty rater and generate them by taking the log (probably base 2) of the timing ratios and then the absolute value of that result. This means a sudden halving in timing has the same tech thingy as doubling in timing.
