<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637326-21825033-6ea4-429b-903b-a0560e10e49b.png" />
</p>

**README for Nessun Data!**

It’s no surprise that data is powerful. Data can reinforce our common sense or knowledge about the world, or it can completely uproot our understanding of it.

For this R Shiny App, I have used data to analyze one of my domain knowledges and great loves—classical music. In music history classes, I was taught that western classical music was a story of progression, then deconstruction, and then reconstruction. But how true is that analysis? What does the data show? And can we reveal—through ***objective*** variables in data—this musical path? Or does the data challenge our assumptions about how we believe our ears are hearing the differences between pieces?

Using the Spotify Web API, I analyzed objective variables from **2,066** pieces by **36** different classical composers from **1098** CE to today to understand and answer these questions. These variables include key, tempo, volume, time signature, pitch and timbre, as well as confidences related to those variables. I sought with this R Shiny App to replicate a specific problem in musicology and music theory: Can we determine what period a piece is from or which composer composed it based on its musical characteristics?

**The App:**

The R Shiny App is separated into three parts as shown below:

The first section (represented by the “Book” icon) contains a summary of the R Shiny App, as well as a composer timeline. Users may click on the pictures of this timeline and pull up information regarding the various composers analyzed.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149794061-a14a921c-0c04-486a-b238-373aa5ab72e9.png" />
</p>

The second section (represented by the “Bar Chart” and “Pie Chart” icons) allows a user to filter by composer and song to evaluate pitch class and timbre scores for that song and to compare a composer to the same or different period groups with respect to variables like key, time signature, etc.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/150033115-e09ef70e-0b66-4828-8a3f-c7a1044dcf9c.png" />
</p>

The third section (represented by the “Bar Chart” and “Line Chart” icons) allows a user to filter by period and run a logistic regression to determine whether certain pitch class and timbre scores by section can predict the correct period.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/150033071-0b45e793-30c4-4540-a1a4-ed786fdcf638.png" />
</p>

**Further Discovery:**

-   The predictive value of the model may be improved by different data—perhaps, scraped from sheet music or from audio files. For classical music specifically, sheet music may be better since audio analysis may add “noise” into the data that sheet music would not and could provide a more direct analysis of each composer.
    
-   More advanced machine learning algorithms may improve the model’s predictive capabilities.
    
-   Further spectrographic analysis and audio comparison between sections of pieces may further improve predictive value of the model.

**Defined terms:**

-   “**Key**” means the diatonic key structure relating to a piece of music or some portion of it.
    
-   “**Mode**” means, in very simple terms, the adjustments of steps in a key. Prior to keys, modes were primarily used.
    
-   “**Tempo**” means the speed of that piece or portion.

-   “**Loudness**” means how loud the piece or portion is in relative decibels.
    
-   “**Time Signature**” means the rhythmic structure of metered music. The top number represents the subdivisions of the beat—whether in two, three or some mixture. The bottom number represents the anchor note of that beat.
    
-   “**Pitch**” means how high or low a note sounds. It is similar to frequency but not wholly represented by it. In western classical music, pitches are represented by letters and sharps or flats.
    
-   “**Timbre**” means what the sound *sounds* like. It is a very complex concept that is partially represented by a combination of amplitude,  duration and frequency, usually as represented on a spectrogram. Timbre is what makes a human voice sound like the human voice, and a trumpet sound like a trumpet.

**Historical Background:**

-   The “**Medieval Period**” of western classical music spanned the entire Medieval Period of history (500 – 1400 CE). It generally featured one line of religious vocal music in a mode (not a key), such as Gregorian Chant, though there were also multiple lines of vocal music at the fourth or fifth intervals and instrumental music during this period. **Hildegard von Bingen** (1098 – 1179 CE) is a composer represented in this R Shiny App from this era.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637464-589c4c10-b007-44a7-a39e-8c163425ebc9.png" />
</p>

-   The “**Renaissance Period**” of western classical music spanned the
    entire Renaissance as well (1400 – 1600 CE). The Renaissance Period
    still largely featured modal vocal music, but relaxation of religious rules meant that composers wrote secular vocal music and instrumental music as well. In addition, that modal vocal music involved advanced polyphony (multiple lines of music) and rhythmic variations. **Claudio Monteverdi** (1567–1643 CE) is a composer represented in this R Shiny App from this era.
    
<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637476-33f7c795-590c-4cf0-8ef5-0e633ee698f1.png" />
</p>

-   The “**Baroque Period**” of western classical music lasted from 1600 CE through 1750 CE. Diatonic music as we understand it solidified in this era. Technological advances meant that instrumental music could also advance, and instrumental music became far more common as a result. Sequences, or repeated patterns in music, were very common—as was counterpoint, or competing melodic lines of music in harmony with each other. **Johann Sebastian Bach** (1685 – 1750 CE) is a composer represented in this R Shiny App from this era.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637500-cb6e42c4-9a38-46ba-b776-a6271023d135.png" />
</p>

-   The “**Classical Period**” of western classical music lasted from 1750 CE through roughly 1800 CE. This period is generally what people think of when they think of classical music. Effectively all music was tonal, and to most people, probably had a lighter or less stuffy “feel” than the Baroque Period. Though polyphony was not abandoned, harmony would generally support a single melodic line. Wolfgang Amadeus Mozart (1756 – 1791 CE) is a composer represented in this R Shiny App from this era.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637512-58d7c47f-a99b-44eb-aa62-dd0c0890f00f.png" />
</p>

-   The “**Romantic Period**” of western classical music lasted from roughly 1800 CE through roughly 1900 CE. This period is incredibly varied, and there are many composers and pieces from this era. Composers attempted to push the boundaries of music as an expression of emotion—both in terms of using music as a means to tell a story and in terms of creating music for its own sake. Toward the end of the Romantic Period, composers began to experiment with different tonal and rhythmic structures, and abrupt key changes or lack of key was common. **Richard Wagner** (1813 – 1883 CE) is a composer represented in this R Shiny App from this era.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637530-56d88d25-f99d-4ce3-9806-c9293de5979c.png" />
</p>

-   The “**Modern Era**” began in 1900 CE and have lasted through the end of World War II. Tonality was broken entirely, and new rhythmic structures, including odd meter or no meter, became common. Different composers had different methods of choosing notes for their pieces, including atonality, impressionism and set theory. **Arnold Schoenberg** (1874 – 1951 CE) is a composer represented in this R Shiny App from this era.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637535-5a6a2f3f-157d-4b8f-b326-a97537bbadc6.png" />
</p>

-   The “**Post-Modern Era**” began at the end of World War II and has lasted through today. It is marked by a collection of approaches, including the incorporation of nonstandard techniques to play instruments, nontraditional sounds, electronic music and the musical influences of non-western traditions. The Post-Modern Era has also reflected a slight move back to tonality and modes, particularly with respect to the compositions of minimalism. Arvo Pärt (born 1935 CE) is a composer represented in this R Shiny App from this era.

<p align = "center">
    <img src = "https://user-images.githubusercontent.com/86251317/149637546-73464b66-b7d5-4c4b-aafa-86a829259bfd.png" />
</p>
