log.sh: Lightweight Command Line Note & Logging

================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on October 15, 2013.
> [Read online!](https://blog.jle.im/entry/log-sh-lightweight-command-line-note-logging.html)

What do you use to send off quick one-off notes and logs about a project you are
working on? Found a nice link to a resource you'll want to look up later...want
to jot down a sudden realization?

Maybe you use some external note-taking software, like *Evernote*. But wouldn't
it be nice to have something that is completely in the command line? Do you
really need to fire up an entire GUI just to write down one line, put down one
link? And do you really need these notes to all be thrown in with your others?

You might be using a command line interface to a larger note-taking system like
*[geeknote](http://geeknote.me/)*. But it's kind of a hassle to open up an
entire text editor every time you want to make a small one-liner. Doesn't quite
meld with the [Unix philosophy](http://www.faqs.org/docs/artu/ch01s06.html).
Maybe you are comfortable simply appending to a text file with `>>`...but what
if you want to add things like timestamps?

Here's introducing ***[log.sh](https://github.com/mstksg/log.sh)***.

## [Log.sh](https://github.com/mstksg/log.sh) {#log.shlog.sh}

*[log.sh](https://github.com/mstksg/log.sh)* is intended for these use cases:

-   Project-based notes
    -   Quick links to resources, references
    -   Small local project TODO's
    -   Reminders and gotchas
    -   Logging progress, short micro-journaling to record check points in
        progress.
-   Simple quick references (in the home directory)
    -   Reminders on system configuration todo's
    -   Refreshers on simple tasks that you only do a few times a year

Most of all, it is meant to be quick, fast, efficient, streamlined, simple.

### Usage

#### Entering a note

Don't believe me? Here's how you enter in a note:

``` bash
$ log.sh hey, this is a note!
```

(Note no need to surround your log with quotes or anything.)

That command will write the line

    [Sun Sep 29 16:07:21 PDT 2013]  hey, this is a note!

to the active log file. As a cute trick, you can specify the flag `-t`

``` bash
$ log.sh -t buy milk
```

which will handily add a cute `[ ]` before the item like so:

    [Sun Sep 29 16:14:03 PDT 2013]  [ ] buy milk

#### Editing your log file

The little `[ ]` is handy because you can open up the log file in a text editor
of your choice and "check" it off.

That's actually very easy, because

``` bash
$ log.sh -e
```

will open the active log file in your favorite editor, specified by the
`$EDITOR` environment variable.

#### The Log File

What is the "active" log file? Where is this log being stored? Ideally, it is
either in the root directory of the relevant project, or in your home folder.

To create a log file in your current working directory, enter

``` bash
$ log.sh -c
```

This'll create a file `.log.log` in the directory.

Now, if you are in any subdirectory, the default behavior is that
*[log.sh](https://github.com/mstksg/log.sh)* will first check the current
directory for a log file; if it doesn't find it, it goes up a directory and
checks there, checking up and up until it finds a valid log file. That file is
the "active" one, where all of your adds and edits will refer to.

#### Log Contexts

If you try to enter a note from a subdirectory, you'll get something cool:

    [Sun Sep 29 16:10:38 PDT 2013]  (./subdir) logging from ./subdir!

*[log.sh](https://github.com/mstksg/log.sh)* will automatically include the
context of your log in your note body.

### Installation

Didn't want to bore you right off the bat with implementation/installation
details. But here it is. The entire thing is open source, and written in bash.

Install by either cloning the github repo or downloading [the latest
release](https://github.com/mstksg/log.sh/releases). Put the file `log.sh` into
a directory in your `$PATH`. If you wish, alias it to something short, like `l`
or `n`.

That should be it!

### That's it!

That's really all there is to it! You can customize the filename of the log file
created/searched for, or you can even specify the exact path of the log file you
want to append to or edit using command line flags. More detail on how to do
this in the documentation:

``` bash
$ log.sh -h
```

Basically, the entire thing is meant to be as frictionless, fast, and
thoughtless as possible. Enter in small notes to reference later in only the
time it takes for you to actually write the note --- no need to mungle around
with text editors and picking which file to append to and dealing with
timestamps. *[log.sh](https://github.com/mstksg/log.sh)* has got you covered!

I do recommend, if you use this, aliasing the command to something short. I
personally use `n`, so I write notes by saying:

``` bash
$ n goodbye!
```

Hopefully this script ends up being as useful to you as it has to me. Feel free
to leave any comments on questions/bugs/improvements, and I'm always happy to
take contributions and pull requests.

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

