wget -O words.html "$1"
html2text words.html >words.txt
python3.0 gen.py