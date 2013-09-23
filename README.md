Screen-keybind
==============
Screen-keybind is a tool for reducing the heavy hand-write task to build a
sophisticated keybind setting of GNU screen. It introducees a concept called
*mode* that works as a separate and indivisual space for key bindings.
Screen-keybind allows to stay in one mode after the execution of one command
through key stroke, even if the command requires input.

Build
-----
Screen-keybind needs Steel Bank Common Lisp, as known as SBCL, to build. If
SBCL has already installed SBCL, then following command builds screen-keybind.

    > sbcl --load make.lisp

If you want to use a Common Lisp implementation other than SBCL,
modify make.lisp. It is the only portion that is depending on SBCL original
extensions (SB-EXT:*POSIX-ARGV* and SB-EXT:SAVE-LISP-AND-DIE).

Usage
-----
    screen-keybind input-file-name [output-file-name]

If the `output-file-name` is omitted, the result is output to standard output.
The input file of screen-keybind is composed of one or more entries. The detail
of entries is described in __Entry of screen-keybind__ section. This project
has an example of input file named "keybind.example".

The tipycal flow of screen-keybind is as follows:

1. Write the input file (and name it "mykeybind" for example)
2. Generate the keybind setting by screen-keybind
    + ./screen-keybind mykeybind ~/.screen/keybind.screenrc
3. Add following line to .screenrc file in order to activate the keybind.
    + source .screen/keybind.screenrc

Entry of screen-keybind
-----------------------
### Entry overview
    {current next [key1 command1 ! command2 ! ...] ...}

A screen-keybind entry is expressed by some components wrapped by a pair of
curly braces({}). An entry is composed of a *current* mode name, *next* mode
name, and one or more key-command sequences. A key-command sequence is wrapped
by a pair of brackets(`[]`) and composed of one *key*, and some *command*s.
Beteween any two *command*s, an exclamation mark (!) must be put as a
separator. Here is an exmaple of an exntry:

    {normal normal [h focus left]}

### Modes
Conceptually, screen-keybind assums that screen has several modes and takes one
of those modes at any moment during its execution. Screen has at least 2 modes:
*raw* and *screen*. Screen starts in *raw* mode and moves to the *screen* mode
when the escape key is input. Each mode has its own key binding table. With
this terminology, the default *raw* mode key binding table has only one entry:
^a for transition to *screen* mode.

Mode transition is the another concept screen-keybind introduces. Screen starts
with the *raw* mode and transitions to *screen* mode if an escape key is input.
After that, if *w* is input for example, screen shows the list of current
windows. At this moment, screen transitions to the *raw* mode (we have to input
^a again when we want to create a new window ,or whatever). These behavior is
called mode transition in the concept of screen-keybind.

Users can introduce any additional mode and key binding table thorough
screen-keybind; screen-keybind is the tool for this purpose.

Please see the __Mode names__ section to know the restriction about mode name.
Roughly speaking, any string comopsed of one or more alphabets and numbers is
acceptable if it includes at least one alphabet.

### How to write screen-keybind entries
Each screen-keybind entry denotes "if the *key* is input when screen is on
*current* mode, then executes these *command*s and transition to *next* mode."

    {current next [key command1 command2 ...]}

Follwing example binds *h,j,k,l* for vim-like window focus movement commands in
*normal* mode. For one pair of current and next modes, users can put multiple
key-command sequences.

    {normal normal [h focus left] [j focus down] [k focus up] [l focus right]}

Since these keybindings are specified to stay at *normal* mode after the
execution of bound commands, these key bindings are able to be used
continuously. For example, stroke `k k k` in *normal* mode moves the window
focus in upper direction for 3 times; no input of escape key is required during this
operation.

The mode transition can be followed with the execution of commands. For
example,It can be convinient if the screen automatically transitions to
*normal* mode when the vim-like window focus movement (introduced above) is
used in *screen* mode because, thanks to above entry, we can continuously move
the window focus in *normal* mode.

    {screen normal [h focus left] [j focus down] [k focus up] [l focus right]}

Next is an example that executes more than 2 commands by one key input.

    {normal normal [v split -v ! focus right ! other ! focus left]}

By this entry, v divides the current region in vertical direction, moves the
newly created region to the most recently used window and keeps the focus to
the current region (without middle 2 commands, the newly created region shows a
special window called blank window). After the execution of the commands, it
stays to normal mode.

### Escape keys
We need a variation of escape key that can trigger the transition to the
*normal* mode in order to utilize key bindings introduced in the previous
subsection. Although such mode transition key has been already introduced
during previous subsections, the most ordinary way is add a transition key
to *screen* mode as follows:

    {screen normal [^r]}

By this entry, *^r* became the key that triggers the transition to *normal*
mode from *screen* mode. Analogically to this example, following entry
registers *^z* as a new escape key.

    {raw screen [^z]}

With this setting, both of ^a and ^z behaves completely same because the
default escape key is ^a. Since screen-keybind is aimed to support all the
things around keybind, a special format that overwrites this default escape key
is supported. Follwing line set ^t as an escape key.

    escape ^t^t

Please be aware of the difference between these 2 approaches. The former adds
an additional escape key, while the latter overwrites the default escape key.

Some users could feel it bothering to stroke 2 keys to enter a insanely great
mode defined by themselves. "Why I cannot have an escape key that directly
transitions to my *great-mode* instead of *screen* mode?" Actually, they can:

    {raw great-mode [^z]}

(Of course, screen has this ability. Screen-keybind does not hack screen. It is
 just a tool for generating a set of screen key bind settings. The great is
 screen.)

### Comments
2 kinds of comment is supported. Semicolon (;) is for end-of-line style. The
text from semicolon to the end of line is ignored. For block-comment, a
combination of sharp (#) and vartical bar (|) is used. The text between #| and
|# is ignored.

Mode names
----------
A string that is composed of alphabets, numbers and some symbols is allowed as
a string for mode name. However, a string that does not include any alphabet is
not acceptable. The symbols that cannot be used as a part of mode name is
listed below. Any symbols that is not listed below can be a component of a mode
name.

+ colon (:)
+ semicolon (;)
+ sharp sign (#)
+ dot (.)
+ backslash (\\)
+ quotation mark (')
+ double quotation mark (")
+ backquote (\`)
+ comma (,)
+ vertical bar (|)
+ open parenthesis (()
+ close parenthesis ())
+ open curly brace ({)
+ close curly brace (})
+ open bracket ([)
+ close bracket (])

Key
---
The *key* field of an entry must be one key specifier that dentoes a character
in ASCII character set. For alphabets and numbers, key specifier is the
character itself.

    {normal normal [w window-list -b] [L layout new]

Above example bind *w* key to *window-list -b* command and *L* (Shift-L) to
*layout new* command in *normal* mode. The key specifier for symbols listed in
**Mode name** section have to be wrapped by a pair of double quotes. On the
other hand, for the symbols not listed in the list, the character itself is the
key specifier for that key. Follwing entry binds *colon (:)* key to *colon*
command and *hyphen-minus (-)* to *select -* command in *normal* mode.

    {normal normal [":" colon] [- select -]}

Control codes are expressed by prepending a circumflex (^).

    {normal normal ["^[" copy]] [^p prev] [^n next]}

Above entry binds *^[* (C-[) to *copy* command, *^P* (C-p) to *prev* command,
and *^N* (C-n) to *next* command in *normal* mode. In the case that the 2nd
character is a symbol listed in the list in Mode name section, whole key
specifier must be wrapped by a pair of double quotes.

One exception is the numerical argument headed by plus (+) symbol. It syould be
wrapped by a pair of double quotes as it is done in the following example.

    {normal normal [> resize -l -h "+1"]}

This entry binds greater than symbol (`>`) to *resize -l -h +1* command. (Please
be careful that above entry is not available if your screen version is not
4.1.0 or newer. The widely used is 4.0.3, I think.)

Hardstatus string
-----------------
Screen-keybind utilize the hardstatus string instead of echo to print the
current mode and message. In order to modify the hardstatus string, another
special format *hardstatus-string* is supported. This format takes one string
as its argument. The argument is passed to hardstatus string command and thus
the capability of hardstatus string is completely available. If you want to
show current time, then following line is for you:

    hardstatus-string "%c"

How to write a more sophisticated hardstatus string is written in man page of
screen(1). Please be sure to write the content of hardstatus string. For
example, if you want to output a single ^ in hardstatus line, you have to write
as follwos:

    hardstatus-string ^^

because ^ have to be escaped by prepending another ^ in screen (this is a
requirement from screen). Thus, the content (^^) is what you have to write. Here
is the default setting of hardstatus string. I think it helps to write your
original hardstatus string:

    hardstatus-string %030=%{B.} %{-}%-w%{=b Mw}%{+u}%{+s}%n %t%{-}%{-}%{-}%+w%{B.} %{-}%=%m/%d %02c

The left most area of hardstatus line is used to print the default messages,
and user specified hardstatus string follows to it. The default message shows
the current mode and followed by most recently used command. Users can use %=
formatter to preserve a fixed space for default messages. 30 characters width is
preservered for it by %030= in the above example.

Insert mode
-----------
Some screen commands like *colon* or *digraph* requires input from the user. In
order to achieve to be able to stay at a mode other than *raw* mode after the
execution of such command, a sub mode called insert mode is implemented. During
the insert mode, [insert] is shown on the hardstatus line as a message (about
tuning of hardstatus line, please see the **Hardstatus string** section).

In the insert mode for the commands other than *digraph* command, ^M and ^J is
recognized as keys for finishing the input and ^G is for canceling the insert mode.
(This insert mode is internally called *layer insert mode*)

Insert mode for the *digraph* command finishes immediately after 2 key storkes.
Thus, ^M and ^J is not available as a finish key. However, ^G is available to
cancel this insert mode. (This insert mode is internally called *transient
insert mode*)


miscs
-----
Author : chiku (Takehiko Nawata, samugari.penguin@gmail.com)
License : MIT License

### For non-LISPers
The input file of screen-keybind is a LISP code. Welcome!
