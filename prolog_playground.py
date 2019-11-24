import os
import signal
import subprocess
import sys
from PyQt5.QtWidgets import QMainWindow, QApplication, QWidget, QPushButton, QAction, QLineEdit, QMessageBox, QLabel
from PyQt5.QtGui import QIcon
from PyQt5.QtCore import pyqtSlot

class App(QMainWindow):

    def __init__(self):
        super().__init__()
        self.title = 'Humor Analyzer'
        self.left = 10
        self.top = 10
        self.width = 400
        self.height = 140
        self.initUI()

    def initUI(self):
        # creates the UI
        self.setWindowTitle(self.title)
        self.setGeometry(self.left, self.top, self.width, self.height)

        # Create textbox
        self.prompt_label = QLabel(self)
        self.prompt_label.setText('Please enter your query to be entered in swipl: ')
        self.prompt_label.move(20, 20)
        self.prompt_label.resize(280,20)

        # Create textbox
        self.textbox = QLineEdit(self)
        # connects the query text to the connect button when 'ENTER' is pressed by the user
        self.textbox.returnPressed.connect(self.on_click)
        self.textbox.move(20, 50)
        self.textbox.resize(280, 30)

        # Create a button in the window
        self.button = QPushButton('Send Query', self)
        self.button.move(20,90)

        # Create result label
        self.result_label = QLabel(self)
        self.result_label.setText('Result: ')
        self.result_label.move(200, 90)
        self.result_label.resize(50, 30)

        # Create answer label
        self.answer_label = QLabel(self)
        self.answer_label.setText('None')
        self.answer_label.move(250, 90)
        self.answer_label.resize(50, 30)

        # connect button to function on_click
        self.button.clicked.connect(self.on_click)
        # displays the widget on the monitor screen
        self.show()

    @pyqtSlot()
    def on_click(self):
        # gets the text from the text box
        question = self.textbox.text()

        # sets up the swipl program in a subprocess and feeds it the file to be initiaded with
        # NOTE: stderr=subprocess.PIPE makes the swipl intro in terminal go away
        p = subprocess.Popen(['swipl', 'sentence.pl'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, universal_newlines=True)

        # looks for if the query was true if not then it must be false
        # The communicate() method returns a tuple (stdoutdata, stderrdata).
        response = p.communicate(question)
        print(response)
        answer = 'True' if response[0].find('true') == 1 else 'False'
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

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = App()
    sys.exit(app.exec_())




# p.stdin.write('assert(car(honda, 18000, red)).')
# p.stdin.write('assert(car(toyota, 18000, white)).')
# p.stdin.write('assert(car(ford, 25000, red)).')
# p.stdin.write('assert(car(acura, 50000, white)).')
# p.stdin.write('assert(car(bmw, 60000, red)).')
# p.stdin.write('[user]. recommendcar(Age, Car) :- Age =< 25, car(Car, Cost, red), Cost =< 30000.')
# p.stdin.write('recommendcar(20, X).')
# output = p.stdout.read()
# print(output)
# print(p.communicate('assert(car(honda, 18000, red)). [user]. recommendcar(Age, Car) :- Age =< 25, car(Car, Cost, red), Cost =< 30000. recommendcar(20, X).', timeout=3))

# print(p.communicate('assert(car(honda, 18000, red)).', timeout=3))
# print(p.communicate('assert(car(toyota, 18000, white)).', timeout=3))
# print(p.communicate('assert(car(ford, 25000, red)).', timeout=3))
# print(p.communicate('assert(car(acura, 50000, white)).', timeout=3))
# print(p.communicate('assert(car(bmw, 60000, red)).', timeout=3))
# print(p.communicate('[user]. recommendcar(Age, Car) :- Age =< 25, car(Car, Cost, red), Cost =< 30000 .', timeout=3))

# os.killpg(os.getpgid(p.pid), signal.SIGTERM)
# p.kill()
