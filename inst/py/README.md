This is direct copy of the code from [`httppipe`](https://github.com/richfitz/httppipe) but added to stevedore directly to simplify installation and eventual transition to CRAN

The file [`httppipe.py`](httppipe.py) i  identical to the file in httppipe, except that we load the `docker` package and we find the Adapter modules in different places (rather than coming from our own package, instead find them in the docker package itself.

```
git diff --no-index stevedore/inst/py/httppipe.py httppipe/inst/py/httppipe.py
```
