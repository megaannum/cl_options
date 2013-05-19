cl_options
==========

Simple Command Line Options management library.

-----

# Introduction

This is just an initial checkin of the code.
In the CLOptions object there is a "def main" with some examples.

Why another Scala Command Line Option parsing library?

There are two things I wanted:

First, the notion of "sub-options" which become enabled and are displayed
as part of 'help' when the "parent-option" is set.

Secondly, rather than have the main routing "understand" all of the
options that all components of the system might need, I wanted a 
library where each component could contribute its own options to
the command line option set. Most, command line option libraries
assume that (forces) the "def main" route to know all. This works 
for small projects but not for large or mutli-developer projects.

Hence, this library.

Some of the ideas initially came from looking at Scala's/PaulP's library in
scala.tools.cmd but have morphed and the DSL aspects removed.

Currently supports:

    Unary:  
        -flag
        help:
            -flag  : Some help message ...
    Binary:
        -debug 4
        help:
            -debug <level:Int=0> : Some help message...
    Property:
        -Pfile=myfile
        help:
            -P<file name:String=Outfile> : Some help message...

TODO:

    Get KeyValue option working

    Create a Formatting Context for both help and error
        this would unify the display or help and error messages.

    FromString
        Extends the Scala/PaulP FromString code

    Create better documentation

    Create some examples


