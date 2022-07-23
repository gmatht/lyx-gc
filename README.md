lyx-gc
======

A Grammar Checker for LyX and LaTeX

This branch is for running lyx-gc as a cgi-bin webapp. It can be installed on Ubuntu 16.04 by running the following as root.

    apt-get install apache2 libcgi-pm-perl flex aspell

    #Install lyx-gc
    mkdir ~/src ; cd ~/src
    git clone https://github.com/gmatht/lyx-gc.git
    cd lyx-gc/
    git checkout CGI
    cp path/chktex.pl /usr/lib/cgi-bin/checktex.cgi
    chmod +x /usr/lib/cgi-bin/checktex.cgi
    cp checktex.html /var/www/html/

    #Install LanguageTool
    cd /usr/local/share
    wget https://www.languagetool.org/download/LanguageTool-3.7.zip
    unzip LanguageTool-5.8.zip
    ln -s LanguageTool-5.8 LanguageTool

    #Set up detex for spellchecking.
    mkdir ~/src ; cd ~/src
    git clone https://github.com/pkubowicz/opendetex.git
    cd opendetex/
    make
    cp detex /usr/bin/
    
    #Test
    /usr/lib/cgi-bin/checktex.cgi # Should run without errors
    lynx http://localhost/checktex.html # Test that it works
