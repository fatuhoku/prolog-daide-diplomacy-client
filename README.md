SWI-Prolog DAIDE client library
======================================
An SWI-Prolog client library for writing
[Diplomacy](http://en.wikipedia.org/wiki/Diplomacy_(game) artificial
intelligences (bots).

### Diplomacy AI Development Environment (DAIDE Development kit)
If you haven't done so already, check out the [DAIDE main
site](http://www.daide.org.uk/wiki/Main_Page).  The DAIDE project offers a
Diplomacy game server, a number of AIs written by various people, and a custom
TCP-based communications protocol specification the client AIs use to interact
with the game server.

###### DAIDE Development Kits
To develop an AI, there already are client libraries (called '_development kits_')
for Java and .NET to convenience DAIDE client AI developers.

This repository holds some scripts (collectively, a client library) for
developing DAIDE AIs using SWI-Prolog.

About
----------------
This work came out of my third year group project in my studies at Imperial
College. We had to develop the strongest Diplomacy AI we could within the space
of some 3 weeks.

I favoured Prolog over object oriented languages for things like parsing and
complex AI algorithms, so I decided to develop a Prolog binding to the [DAIDE
communications protocol](http://www.daide.org.uk/external/comproto.html) so that
I could write a Diplomacy bot using a logic programming langauge.

In this repository, you will find SWI-Prolog implementations of

 - basic networking layer (see `dip_connect` in `daideconn.pl`)
 - network stream tokenisation (`dpptokens.pl`)
 - DCG-based communcations protocol parser (`dppgrammar.pl`)
 - symbolic domain model (`dipdomain.pl`)
 - a handful of example simple AIs that use the above

I've not really worked on this code since the end of the project, so there may
be a bit of bit-rotting. Documentation is lacking, but there's
quite a lot of comments in the source files themselves.

The code remains untested, but the parser has been seen to perform quite well,
exercising a large section of the protocol. It has been seen deserialising and
serialising messages quite well. In particular, I found a good synergy between
symbols defined in the protocol and Prolog's concept of atoms and complex terms.

With a little bit of love, I think this DCG grammar-parsing based implementation
could make it onto the [DAIDE Development Kits
page](http://www.daide.org.uk/wiki/Development_Kits) page.

Good luck, and feel free to fork, change, distribute, do whatever you like with
this code.

LICENSE
=============
This work is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php).
