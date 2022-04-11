from flask import Flask, render_template

app = Flask(__name__)


@app.route('/')
def printTxt():
    with open("../Note.txt", "r") as f:
       return render_template('content.html', text = f.read())

if __name__ == "__main__":
    app.run(debug = True)

