### Data Sonification:
### Who Talks the Most in the Film “The Lord of the Rings: The Fellowship of the Ring?”
### *A Sonic Montage of Spoken Line Instances by Character*

Data Sonification: What is it? Simply put, it is the representation of data through sound.
I’m interested in representing data in creative and intriguing ways — data sonification strikes me as a unique way to convey information in a playful way.

With the aim to create a simple sonification project, I began exploring datasets on Kaggle, and came across a pair of LOTR datasets involving the movie scripts and character classification. I thought it would be interesting to sonify the ethnic group or character distribution across the script — which characters and tribe groups speak the most in the films?
Pulling this data into R, I found that the data was a bit problematic — for example, some characters were missing, and the script lines were out of order, which would not work for my purposes. I decided to procure the data in a different way — using the R rvest package, I scraped the LOTR Fellowship of the Ring script from an IMSDb webpage. Pulling the character name from the beginning of each <b></b> section of the html script, I merged this data with a comprehensive character list. 
With some manipulation and cleaning, I ended up with a dataset of character name and ethnic group classification (“Ainur,” “Elf,” etc.) in the order of spoken lines in the screenplay.

#### Sonification Approach 1: Two Tone
I had heard of Two Tone, a sonification application that creates sonic output from a given uploaded dataset. Uploading my dataset to this application, I experimented with the output, integrating different tones and tempos. However, I was ultimately dissatisfied with the outcome because only one instrument could be selected, and I realized that pitch-character correlation doesn’t have much of a natural correlation in my mind. Instead, I thought it would be more interesting to associate each character with more distinctly different sounds. I imagine Galadriel’s “tone” as an extended, resonant, stringed instrument for example (to cover all those narrative lines in the beginning of the film), and Bilbo’s “tone” as more staccato and warm. With the desire to customize this sonification, I decided to use Logic Pro X.

#### Sonification Approach 2: Logic Pro X
In R, I created a new data frame that would support my tone-character sonic expression in Logic Pro X. It includes a summarization of consecutive line groupings per character, and the length of musical measure that those lines should encompass. Each unique instrument-pitch combination corresponds to a particular character, and each sonic instance is a line (or consecutive lines) being spoken in the film. I decided to give each script line a quarter note length in 4/4 time signature; multiple consecutive lines sound like an elongated note (e.g. 4 lines make a note that last for a measure). To systematize the sound, I kept the sonification within the key of D major (a nod to the D major key of "Concerning Hobbits" in the LOTR film score). For characters that would most often be associated with a negative interaction (e.g. Sauron, Saruman), I assigned a note that would make the key minor. Within an instrument, I ranked pitch by the age of the character; that is, lower pitches were assigned to older characters within an ethnic group.

[Here is the link](https://soundcloud.com/aurawalmer/data-sonification-lotr?utm_source=clipboard&utm_medium=text&utm_campaign=social_sharing#t=2:50 "SoundCloud") to the data sonification that I created in Logic. 

