# filenotes
Persistent local notes across files, suitable for editor integration

## Initial design ideas

### Taking notes
- I'm in a file, reading or writing code
- I want to make a note in the context of the file I'm working on
- I want that note to be tagged with the information I need to get back
  from the note to the context:
  - file path and line number
  - git branch and latest commit, if relevant
  - timestamp
  - my note, with an optional tag at the front

#### target solution
- shift-alt-n "TODO: figure out why this test doesn't fail"
- invokes filenotes and adds a TODO-tagged note for the current file
  and line number, current branch and commit, and timestamp

### Listing notes
- I'm in a file, I remember taking notes on it
- I want to list recorded notes in the file, or in the immediate
  directory tree

#### target solution
- shift-alt-n "list"
- invokes filenotes to list notes for the current file in the current
  branch

- shift-alt-n "treelist 2"
- invokes filenotes to list notes for the current file and anything up
  two directories in the current branch

- shift-alt-n "list all"
- invokes filenotes to list notes for the current file in any branch

### Closing notes
- I'm in a file, I've taken notes in it, and I've figured out what I
  wanted to know
- I want to mark that note as closed

#### target solution
- shift-alt-n "close"
- invokes filenotes to add a "closed" tag to the note on the line I'm on
  - if there's no note on the line I'm on, list nearby notes

## What does filenotes do
- it's aware of the current git branch and commit in the directory its
  run in, if any
- it's aware of a global database of notes, probably don't want to keep
  this global in the long term but w/e
- it takes command-line info about where it's been called, as well as a
  notes string of the form "<optional tag>: <text>"
- if the notes string starts with a special keyword
  - list
  - treelist
  - close
  it does something different from usual

## Why does it do it that way
- we're going to start by calling it using edonet's "command runner"
  vscode extension
- probably a cli version is straightforward to integrate with other
  editors too
