# humor_analyzer
A NLP humor analyzer using an Expert System (Prolog's swipl). GUI created in python and linked using subprocess's PIPE

## Developers

* **Jonathan Ishii**     ------ [Godsinred](https://github.com/Godsinred)
* Chaithra Bangalore Venkatesh
* Rob Hackemack


## Prerequisites

Python 3.7
Download:
  * https://www.python.org/downloads/

Prolog (swipl)
  * https://www.swi-prolog.org/download/stable

pip install
  pyqt5

## Getting Started
In the terminal navigate to the folder you have "humor_analyzer.py" located in. Type "python3 humor_analyzer.py" and then hit enter to run the program. Type in any knock knock joke and hit enter or click Send Query for Prolog to analyze the data and for the GUI to return True or False.

## Technical Implementation
Used PyQt5 to implement the GUI in python. This grabs the user input and then this is preprocessed in python to match the needs of Prolog (swipl, pretty much adding the function call to the the front of the user inputted sentence and making sure that everything is lower case and spaced out). This preprocessed data is then sent to swipl as a subprocess in python. Once in swipl, Prolog will break down the input to make sure that the joke is formatted correctly. i.e. Each joke must start with knock knock. To determine if the joke is funny or not the program compiles a list of all homonyms from the third sentence. This list is compared against the last sentence to check if any member of the homonym list exists within the last sentence. If one exists swipl will return true to the subprocess. Any answer T/F will show in the GUI for the user.

## Challenges

How to have the GUI interact with Prolog.
This was difficult because there are no  libraries to interact with Prolog in python without creating a new environment.
Solution: Used subprocess to communicate between python and swipl
List processing of the Knock-knock jokes.

How to process lists of data in Prolog, such that a function can have a list as an argument.
Solution:  Compartmentalized lists into smaller subcomponents to increase the ease of analysis.
Processing of knock-knock jokes for homonyms.

How can you decide whether a knock knock joke is funny or not?
No straightforward way of determining such a thing.
Solution: Generalized homonyms into a list and compared if at least one elements existed in the other list.

## Future Recommendations

Retrieving and suggesting other knock-knock jokes if the output returns true.
Analyzing other varieties of jokes like sarcasm, pun, irony and exaggeration.
