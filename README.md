# Readme for lyx-gc

A Grammar Checker for LyX and LaTeX. There is an [online demo](http://mccabedj.ucc.asn.au/checktex.html).

A more elaborate readme is found on the [LyX-wiki](http://wiki.lyx.org/Tools/LyX-GrammarChecker)

## Dependencies
For full functionality you will also want the following software

* [ChkTeX](http://www.baruch.ev-en.org/proj/chktex/) (available as debian package and via ctan)
* [LanguageTool](https://www.languagetool.org/)

On Windows, you also need Bash to run the commands. You can use Git bash for this purpose.

## Installation

Clone or unzip the respository, for example:

```
mkdir ~/.local
cd .local
git clone https://github.com/gmatht/lyx-gc.git
```

Add this folder to your ```$PATH``` variable. 

For example, for bash, add to file ```~/.bashrc```: <br>
   ```export PATH=$PATH:"~/.local/lyx-gc/"```

### Note on installing Languagetool
Currently, languagetool has no installer and is not included in any software repository. Suggested installation:

* Download stand-alone version from https://languagetool.org/
* Unpack in ```~/.local/LanguageTool/```

## Configuration
Use LanguageTool as grammar checking in lyx-gc:

 * Adjust in the file ```lyx-gc/path/chktex.pl.JLanguageTool.pl```:
    * Change the LANGUAGETOOL_PATH variable to the path where  ```languagetool-commandline.jar``` is located.
    * Example:  ```my $LANGUAGETOOL_PATH=$ENV{'HOME'}."/.local/LanguageTool-3.6/";```
    
    * On Windows, the path should be written using forward slash. For example, ``` my $LANGUAGETOOL_PATH=$ENV{'HOME'}."C:/LanguageTool-4.9.1"; ```
 
## Usage
Start lyx using the ```lyx-gc``` script, then Menu->Tools->CheckTex.

This command should be run twice when LanguageTool is used as check.


## Keyboard shortcuts
By default, the short-cut for buffer-chktex is ```M-e h``` (e.g ```Alt-e``` then ```h```).
You may wish to change this to ```F8``` by creating a new bind-file,
say ```$HOME/.lyx/bind/myShortcuts.bind``` and include one of the standard files, e.g.:

``` 
# include one of the basic flavours (cua or emacs)
\bind_file	"cua"
# add your own bindings (overwriting the included ones)
\bind		"F8" "buffer-chktex"
```

Then go to the preferences dialog and choose this file to be your bind-file.
See [KeyboardShortcuts](http://wiki.lyx.org/Tips/KeyboardShortcuts)
for more information. 
