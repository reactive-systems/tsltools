from flask import Flask, render_template, request, url_for, flash, redirect
from urllib import request as rq

app = Flask(__name__)
app.config['SECRET_KEY'] = '8d5f5436048ebaa58e31e632a90a3433516625b9dfb240b0'

messages = [{'title': 'Spec One',
             'content': 'Spec One Content'}
            ]

@app.route('/')
def index():
    return render_template('index.html', messages=messages)

@app.route('/create/', methods=('GET', 'POST'))
def create():
    if request.method == 'POST':
        title = request.form['title']
        content = request.form['content']

        if not title:
            flash('Title is required!')
        elif not content:
            flash('Content is required!')
        else:
            messages.append({'title': title, 'content': content})

            # creating a file in our working directory
            filename = title + '.tsl'
            f = open(filename, 'a')
            f.write(content)
            f.close() 

            return redirect(url_for('index'))
        

    return render_template('create.html')