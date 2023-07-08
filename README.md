To build

docker build -t jadudm/o2u -f Dockerfile . 

To run

docker run -v ${PWD}:/output jadudm/o2u --url <url>

For help

docker run jadudm/o2u --help

The tool defaults to the output directory /output; hence, mapping 

-v <local dir>:/output

will set where the output file should be rendered.

The output filename can be changed with --outfile.

To use a different template, it needs to be placed in the input directory. You should map this as well.

docker run -v ${PWD}:/output -v ${PWD}:/input jadudm/o2u --url <url>

