rm `find . -iname "*.pyc"`
python3.1 setup.py bdist_egg
rm -rf build
rm -rf Dust.egg-info