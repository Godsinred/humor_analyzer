import os
import signal
import subprocess
import sys
from PyQt5.QtWidgets import QMainWindow, QApplication, QWidget, QPushButton, QAction, QLineEdit, QMessageBox, QLabel
from PyQt5.QtGui import QIcon
from PyQt5.QtCore import pyqtSlot
import regex as re

class App(QMainWindow):

    def __init__(self):
        super().__init__()
        self.title = 'Humor Analyzer'
        self.left = 10
        self.top = 10
        self.width = 800
        self.height = 200
        self.initUI()

    def initUI(self):
        # creates the UI
        self.setWindowTitle(self.title)
        self.setGeometry(self.left, self.top, self.width, self.height)

        gaps = 20
        # Create textbox
        self.prompt_label = QLabel(self)
        self.prompt_label.setText('Please enter your query to be entered in swipl: ')
        self.prompt_label.move(gaps, gaps)
        self.prompt_label.resize(self.width/2,gaps)

        # Create textbox
        self.textbox = QLineEdit(self)
        # connects the query text to the connect button when 'ENTER' is pressed by the user
        self.textbox.returnPressed.connect(self.on_click)
        self.textbox.move(gaps, gaps*3)
        self.textbox.resize(self.width-100, gaps*2)

        # Create a button in the window
        self.button = QPushButton('Send Query', self)
        self.button.move(gaps,gaps*6)

        # Create result label
        self.result_label = QLabel(self)
        self.result_label.setText('Result: ')
        self.result_label.move(self.width-200, gaps*6)
        self.result_label.resize(gaps*5, gaps*2)

        # Create answer label
        self.answer_label = QLabel(self)
        self.answer_label.setText('None')
        self.answer_label.move(self.width-150, gaps*6)
        self.answer_label.resize(gaps*5, gaps*2)

        # connect button to function on_click
        self.button.clicked.connect(self.on_click)
        # displays the widget on the monitor screen
        self.show()

    @pyqtSlot()
    def on_click(self):
        # gets the text from the text box
        question = self.textbox.text()

        question = preprocess_query(question)

        # sets up the swipl program in a subprocess and feeds it the file to be initiaded with
        # NOTE: stderr=subprocess.PIPE makes the swipl intro in terminal go away
        p = subprocess.Popen(['swipl', 'sentence.pl'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, universal_newlines=True)

        # looks for if the query was true if not then it must be false
        # The communicate() method returns a tuple (stdoutdata, stderrdata).
        response = p.communicate(question)

        answer = 'False'
        # uses regex to see if true was returned by swipl
        if re.search('true', response[0]):
            answer = 'True'

        print(question + '    ' + answer)
        # terminates the subprocess
        p.kill()

        # udpates all the labels and texts boxes with the answer
        # added repaint due to the know compatibility issue with MAC OSx
        # https://stackoverflow.com/questions/56553257/pyinstaller-and-pyqt5-macos-mojave-compatibility-issues
        self.answer_label.setText(answer)
        self.answer_label.repaint()
        self.textbox.setText("")
        self.textbox.repaint()

def preprocess_query(query):
    # funny("knock knock . who is there ? Nobel Prize . Nobel Prize who ? Nobel Winner !").

    answer = r'funny("'
    query = query.lower()
    # removes all excess symbols for prolog to compare
    query = query.replace(', ', '')
    query = query.replace('\'', '')
    query = query.replace('-', '')
    query = query.split()

    symbols = ['.', '?', '!']
    for i in query:
        if i[-1] in symbols:
            answer += i[:-1] + ' ' + i[-1] + ' '
        else:
            answer += i + ' '

    answer = answer.strip() + r'").'
    print(answer)
    return answer

# Knock knock. Who is there? Witch. Witch who? Which one of you will give me some halloween candy?
# Knock knock. Who is there? Nobel Prize. Nobel Prize who? Nobel Winner!

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = App()
    sys.exit(app.exec_())
