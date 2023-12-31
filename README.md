## To build

```
docker build -t jadudm/o2u -f Dockerfile . 
```

## To run

```
docker run -v ${PWD}:/output jadudm/o2u --url <url>
```

## For help

```
docker run jadudm/o2u --help
```

## Input and output 

The tool defaults to the output directory `/output`; hence, mapping 

```
-v <local dir>:/output
```

will set where the output file should be rendered.

```
docker run -v ${PWD}/output jadudm/o2u --url <url>
```

will render the URL through a default template to the file `api-documentation.html`.

The output filename can be changed with `--outfile`.

To use a different template, it needs to be placed in the input directory. You should map this as well.

```
docker run -v ${PWD}:/output -v ${PWD}:/input jadudm/o2u --url <url>
```

It is possible to run against a file instead of a URL, but it, too, should be in the input directory.

```
docker run -v ${PWD}:/output -v ${PWD}:/input jadudm/o2u --file <filename>
```

## Running locally

```
racket main.rkt
```

will run the application. It defaults to the current directory for input and output directories.

```
racket main.rkt --url <url>
```

is the equivalent to the dockerized commands above.

Tested with Racket 8.9. Probably good back to at least Racket 8.6.