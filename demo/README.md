# demos

Start with

```
asciinema rec <name>.json -c 'R --quiet --no-save'
```

Record the session

Convert to gif

```
docker run --rm --user=`id -u` -v ${PWD}:/data asciinema/asciicast2gif -S 1 -s 6 <name>.json <name>.gif
```
