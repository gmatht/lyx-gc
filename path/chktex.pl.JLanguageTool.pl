#!/usr/bin/env perl
use strict;
use File::Copy;


my $JAVA=`which java`;
my $LANGUAGETOOL_PATH=$ENV{'HOME'}."/.data/LanguageTool-2.5/";
my $LANGUAGETOOL_JAR="languagetool-commandline.jar";

if (! -d $LANGUAGETOOL_PATH) {
	system ("echo LANGUAGETOOL_PATH $LANGUAGETOOL_PATH is not a directory >> ".'$DEBUGFILE');
}
if (! -e $LANGUAGETOOL_PATH) {
	system ("echo LANGUAGETOOL_PATH $LANGUAGETOOL_PATH does not exist >> ".'$DEBUGFILE');
}
	
	system ("echo using LANGUAGETOOL_PATH $LANGUAGETOOL_PATH >> ".'$DEBUGFILE');

my $XTERM=`which xterm konsole eterm gnome-terminal | head -n1`;

print STDERR "XTERM =  $XTERM";

$XTERM=~s/\n//g;
$JAVA=~s/\n//g; 


my $in_file;

if ($ARGV[0] =~ /^\//) {
	$in_file="$ARGV[0]";
    } else {
	$in_file=`pwd`."/$ARGV[0]";
	$in_file=~s/\n//;
}

#print "$in_file in_file\n";

my $out_file="$in_file.languagetool";
# print ( "$out_file" )." A\n"; 

#echo "Will generate $out_file in background if needed."
#if test '(' $in_file -nt $out_file ')' -o '(' ! -e $outfile ')'
#echo if test '(' "$in_file" -nt "$out_file" ')' -o '(' ! -e "$outfile" ')'
#if (system('test "(" "$in_file" -nt "$out_file" ")" -o "(" ! -e "$out_file" ")"')){
#if (system('echo dtrue')){

# print ( -M "$out_file" )." aA\n"; 
# print ( "$out_file" )." A\n"; 


#if ( 1 ){
#system ("Will outpecho wrote $lockfile >> ".'$DEBUGFILE');
system ("echo Will write $out_file if neccesary >> ".'$DEBUGFILE');
#system ("xterm -e \"echo Will write $out_file if neccesary && read f\" >> ".'$DEBUGFILE');

my $use_lock=0;

print STDERR "infile $in_file\n";
print STDERR "outfile $out_file\n";
print STDERR "Before if\n";
system ("echo before if >> ".'$DEBUGFILE');
if ( ( ! -e "$out_file" ) or ( -M "$out_file" > -M "$in_file" ) ) {
print STDERR "After if\n";
system ("echo After >> ".'$DEBUGFILE');
#   print "true\n";
#   print ( -M "$out_file" ) ;
#   print " o\n";
#   print ( -M "$in_file"  ) ;
#   print  " i\n" ;
#   print  "$out_file" . " of\n";
#   print  "$in_file" . " if\n" ;
#   print "true\n";

my $lockfile="$out_file.lock";
if ($use_lock) {
  use Fcntl ':flock';
  print STDERR "Wait for lock to update $out_file: We should not have to wait long here\n"; 
  open (LOCKFILE, ">>$lockfile") or die "cannot open $lockfile for appending";
  flock (LOCKFILE, LOCK_EX);
  print STDERR "Got lock\n";
}
  system ("echo Will write $out_file >> ".'$DEBUGFILE');

  #my $langtoolcmd='cd "'.$LANGUAGETOOL_PATH.'" && "'.$JAVA.'" -jar *LanguageTool.jar \'\-b\' "'.$in_file.'" > "'.$out_file.'.tmp"';
  my $langtoolcmd='cd "'.$LANGUAGETOOL_PATH.'" && "'.$JAVA.'" -jar "'.$LANGUAGETOOL_JAR.'" "'.$in_file.'" > "'.$out_file.'.tmp"';
  system ("echo CMD '$langtoolcmd' >> ".'$DEBUGFILE');
  print STDERR "CMD $langtoolcmd \n";
  #my $langtoolcmd='cd "'.$LANGUAGETOOL_PATH.'" && "'.$JAVA.'" -jar *LanguageTool.jar -b "'.$in_file.'" > "'.$out_file.'.tmp"';

#  system ('cd "'.$LANGUAGETOOL_PATH.'" && "'.$JAVA.'" -jar *LanguageTool.jar \-b "'.$in_file.'" > "'.$out_file.'.tmp"');
  system ("echo 6 >> ".'$DEBUGFILE');
  system ($langtoolcmd);
  system ("echo 5 >> ".'$DEBUGFILE');
  print STDERR $langtoolcmd;
  system ("echo 4 >> ".'$DEBUGFILE');
  #print "Finished JLanguageTool run\n";
  system ("echo 3 >> ".'$DEBUGFILE');
  system ('rm "'.$out_file.'"');
  system ("echo 2 >> ".'$DEBUGFILE');
  move ($out_file.'.tmp', $out_file);
  system ("echo 1 >> ".'$DEBUGFILE');
  #system ('mv "'.$out_file.'.tmp" "'.$out_file.'"');

  system ("echo wrote $out_file >> ".'$DEBUGFILE');
  system ("echo wrote $out_file >> ".'$DEBUGFILE');
if ($use_lock) {
  system ('rm '.$lockfile);
  flock (LOCKFILE, LOCK_UN);
  close (LOCKFILE);
  print STDERR "Released lock\n";
}
}
