#!/usr/bin/perl
# This file should be a drop-in for chktex. It is designed to generate errors that are useful for LyX.
#
# BUGS: Only processes a single file at a time. ChkTeX allows you to pass in more than one filename at
# a time. However, this is not a big problem since LyX does not use this feature of ChkTeX.
#
# Should however run consistency checks on child files: Duplicated Paragraphs etc, Colour vs Color.
#
# FIX: Err descriptions not displaying
# utf-8 bugs.
# "<<" ">>" vs $S, $E

use strict;
#use diagnostics; #Not availiable in Perl 5.6.1, and not really needed
use warnings;
use File::Basename;
#I should also use Regexp::Assemble, with the Track and matched options.
#use utf8;

use Cwd 'abs_path';

################################################################################
# GLOBAL VARIABLES (Bad boy! *Smack*)

our $filename; # the filename of the main file we are checking for errors.

our %child_errors=(); 
our %false_positives;
our %dictionary; # List of all english words.
our $FalsePositivesSuppressed=0;
# ChkTeX and lacheck always check for errors in child files, howevever,
# LyX does not display error in child files correctly, it assumes all errors
# are in the main file.		
# 
# Hence we bundle up all child file errors into a single error.
#
# The values of the hash %child_errors have no meaning,
# (keys %child_errors) contains the errors in the child files.
# we use a hash so that duplicated errors are only displayed once.


################################################################################
# GLOBAL CONSTANTS

# The special chrs we use to represent tokens.
our $start_math_char=chr(1);
our $end_math_char=chr(2);
our $start_math=$start_math_char;
our $end_math=$end_math_char;
our $cBACKSLASH=chr(3);
our $cDOLLAR_SIGN=chr(4);
our $new_error_type=chr(5);

# This is a hack to emulate a chunk of text where the number of braces must match
# This is not possible in a regular language so we just handle a few recursive braces
# Using Perl 5.10* it is possible add recursion to a so called "regex" but it is not
# really necessary.
# * See: http://coding.derkeiler.com/Archive/Perl/comp.lang.perl.misc/2008-03/msg01047.html
our $recursive_brace="[{][^{}]*[}]";
$recursive_brace="[{](?:[^{}]|$recursive_brace)*[}]";
$recursive_brace="[{](?:[^{}]|$recursive_brace)*[}]";
$recursive_brace="[{](?:[^{}]|$recursive_brace)*[}]";
$recursive_brace="[{](?:[^{}]|$recursive_brace)*[}]";

our @token= ( 
		["\\\\",$cBACKSLASH,"BACKSLASH/"],
		['\\$',$cDOLLAR_SIGN,"DOLLAR_SIGN/"],
		['$',$start_math_char,"cMATH"],
		['$',$end_math_char,"/cMATH"],
);

our $m_ ='(?:(?<!\\\\)[$])'; #The edge of mathblock
our $notm_ ='(?:[^$]|\\\\\$)'; #Not an edge ofa mathblock
our $anychar='(?:.|
)';
our $par ='(?:(?m)\A|\n\s*\n|\Z)';

our $fullstop='(?:(?<![.].)[.])';

our $macroblock='\\\\term\\{[^}]*\\}';

our $mathblock=$start_math.'[^'.$end_math.']*'.$end_math;


################################################################################
# Asserts: Here we check that we have not introduced certain bugs into our program

Assert (tokenize('$$ $$\\$\\\\$\\$$\\$'),$start_math_char.$start_math_char." ".$end_math_char.$end_math_char.$cDOLLAR_SIGN.$cBACKSLASH.$start_math_char.$cDOLLAR_SIGN.$end_math_char.$cDOLLAR_SIGN);

{ # check that detokenize exactly reverses tokenize.
	my $test=q! $ $ \\\\!;
	Rassert('^.*$',$test,detokenize(tokenize($test)));
}

Rassert($fullstop,"i.e.",".");
Rassert($fullstop,"i.  e.",".,.");

Rassert("aZ","a\n","");
Rassert("$par b"," b"," b");
Rassert("b $par","b ","b ");
Rassert("b $par","b \n\n","b \n\n");

Rassert($m_,'$\$','$');
Rassert('(?:[^$]|\\\\\$)b','\$b','\$b');
Rassert($notm_.'b','\$b $b','\$b');
Rassert($m_,' \$ $a$ ','$,$');

Rassert("$mathblock","$start_math abd$end_math edf","$start_math abd$end_math");

our $notinmath="(?![^\\$start_math]*$end_math)";

Rassert("X.$notinmath","XY foo $start_math XZ abd$end_math","XY");


##############################################################################
sub Rassert {
	my ($reg,$text,$expectedresult,$j) = @_;
	our $reg_global= qr/$reg/;
	my $result;

	if ($@) {
		#warn _SYSTEM_ERROR_MESSAGE_;
		$result=$@;
	} else {
		
		my @l= ($text =~ /$reg_global/g);
		$result=join(",",@l);
		if (@l>0 and $result eq ""){
			$result="EMPTYSTR";
		}
	}
	if ($result ne $expectedresult) {
 		(my $package, my $filename, my $line) = caller;

		die("regex did not behave as expected:
input          : ".name_tokens($text)."
expected result: ".name_tokens($expectedresult)."
actual   result: ".name_tokens($result)."
at	       : $package:$filename:$line
regex          : ".name_tokens($reg)."\n\n");
	}
}

##############################################################################
sub Assert {
	my ($text,$expectedtext) = @_;
	
	if ($text ne $expectedtext) {
 		(my $package, my $filename, my $line) = caller;

		die("Assert failed.
expected result: ".name_tokens($expectedtext)."
actual   result: ".name_tokens($text)."
at	       : $package:$filename:$line");
	}
}

##############################################################################
sub ReadWholeFile {
# returns text contained in file named from first parameter
my $filename=shift(@_);

my @lines = <STDIN>;

return join ("", @lines);
}


#############################################################################
# The following three functions implement very limited tokenization and 
# Detokenization. Basically they replace \\, $ etc. with tokens such as 
# chr(2) which should not exist in normal text.

sub tokenize {
my $text=shift(@_);
foreach (@token[0..($#token-2)]) {
		my $from=$_->[0];
		my $to=$_->[1];
		#$to=~ s/'\\\\\\\\\'/'\\'/g;
		#print "$from ".$_->[2]."\n";
		$from=~ s/\\\\/\\\\\\\\/g;
		$from=~ s/\$/\\\\\$/g;
		#print "$from b".$_->[2]."\n";
		$text=~ s/$from/$to/g;
} 

$text=~ s/\$\$([^\$]*)\$\$/$start_math_char$start_math_char$1$end_math_char$end_math_char/g;
$text=~ s/\$([^\$]+)\$/$start_math_char$1$end_math_char/g;
return $text;
}

sub tokens_to_user {
# changes tokens into the format we wish the user to see
return detokenize (shift(@_));
}

sub detokenize {
# inverse of tokenize
my $text=shift(@_);
	foreach (reverse @token) {
		my $from=$_->[1];
		my $to=$_->[0];
		#$to=~ s/'\\\\\\\\\'/'\\'/g;
		$text=~ s/$from/$to/g;
	} 
return $text;
}

sub name_tokens {
# replaces tokens with human readible names.
my $text=shift(@_);
	foreach (reverse @token) {
		my $from=$_->[1];
		my $to='<'.$_->[2].'>';
		#$to=~ s/'\\\\\\\\\'/'\\'/g;
		$text=~ s/$from/$to/g;
	} 
return $text;
}


##############################################################################
sub setDiff { #($a,$b)
# Generates the set $a - $b (uses strings as sets of characters)
   my $a = shift(@_);
   my $b = shift(@_);
   $a=~ s/[$b]*//g;
   return $a;
}

##############################################################################
sub GenerateVowelRegex {
# Input: %SetOfVowels (or Consonants) format:
#	l => "aeiou", # lower case vowels
#	U => "FHILMANXAEIO", #upper case vowels i.e. F=eff which is a vowel
#	d => "8", #vowels that are digits
#	funnynumbers => "\b(?:11|18)(?:|..)(?:...)*\b" # e.g. Eighteen(vowel), not One Eight.
# Output: adds the following entries to the set
#	... 
#   $simple: A regex to find simple vowel words like "e..." and "8".
#         Note regex will also accept e.g. "one" even though it isn't really a vowel word,
#           because it sounds like "won".
#   $hidden: A regex to find hidden vowels like "honour", "X-ray" and "$n$".
#
# If you pass in	 a minimal %SetOfConsonants, something similar yet opposite happens.

   my ($SetOfVowels) = @_;

   my $isvowelset=@$SetOfVowels{'isvowelset'}; #Otherwise we are actually dealing with consonants
   my $l=@$SetOfVowels{'l'};
   my $U=@$SetOfVowels{'U'};
   my $d=@$SetOfVowels{'d'};
   my $number=@$SetOfVowels{'number'};

   Rassert($m_,'$','$');
   Rassert($m_,'\$','');

   #the characters that are only vowels if they are upper case/lower case
   my $Uo=setDiff($U,uc $l);
   my $lo=setDiff($l,lc $U);
   #case independant versions of l
   my $li=$l.(uc $l);
   my $Ui=$U.(lc $U);

   #below: a word that starts with [aeiou] or 8.
   my $simple_word='\b['.$li.'][[:alnum:]]+\b'; 
   # A simple word is not really a vowel word if it is an excluded word e.g. UK. which is pronounced "You Kay".
   my $excluded_word='\\b'.@$SetOfVowels{'excludewords'}.'\\b';
   my $good_simple_word="(?!$excluded_word)$simple_word";
   my $complex_word='\b'.@$SetOfVowels{'includewords'}.'\b';
   my $word="(?:(?:$complex_word)|(?:$good_simple_word))";

   #below: a letter which sounds like a vowel, e.g. F=eff.
   my $letter='\b['.$Ui.']\b'; #
   		
   #below: These commands have no sound and should be skipped over.
   my @mathignorelist=("frac","hat","acute","bar","dot",
"check","grave","vec","ddot","breve","tilde");

   my $mathignore=	"\\\\".
				join("\\s*{|\\\\",@mathignorelist).
			"\\s*{" ;


   Rassert($mathignore,"\\frac{","\\frac{");
   Rassert($mathignore,"\\epsilon{",'');
   
   #below, latex commands/symbols which have sound
   #start with [aeiou], end before the first non alpha character and do not
   #match $mathignore.
   my $mathsymbol='(?!'.$mathignore.')\\\\['.$li.'][[:alpha:]]*(?![[:alpha:]])';

   if ($isvowelset) {
   	Rassert($mathsymbol,'\\epsillyon \\frac{\\epsilon','\\epsillyon,\\epsilon' );
   	Rassert($word,'NP','NP');
   } else {
   	Rassert($mathsymbol,'\\frac{\\epsilon','');
	Rassert($mathsymbol,'\\frac{\\sigma','\\sigma');	
   	Rassert($word,'NP','');
   }

   Rassert($start_math.'.'.$end_math,tokenize('$n$ apple'),$start_math.'n'.$end_math);

   #below: a math block that starts with 8 or [FHILMANXAEIO] or \[aeiou].
   my $math=$start_math.                     #starts with '$'
		  '(?:'.$mathignore.'|[(])*'.  #skip over things which do not affect the vowel
		'(?:'. 
			"[$d$Ui]|".   #[8] or #[FHILMANXAEIO] or [fhil...]. 
			"\\[$li]". #\[AEIOU] or \[aeiou]
		')'.'[^'.$end_math.']*['.$end_math.']';		#Ends with '$'
   
   if ($isvowelset) {
	Rassert($math,tokenize('$n$  $m$ $y$ $8$ $(n)$ $(y)$ $\\frac{n}{m}$ $\\frac{y}{z}$'),tokenize('$n$,$m$,$8$,$(n)$,$\\frac{n}{m}$'));
	Rassert($word,"hat UK own","own");
   } else {
	Rassert($math,tokenize('$n$  $m$ $y$ $8$ $(n)$ $(y)$  $\\frac{n}{m}$ $\\frac{y}{z}$'),tokenize('$y$,$(y)$,$\\frac{y}{z}$'));
	Rassert($word,"hat UK own","hat,UK");
   }	

   my $VowelSound="(?:$word|$letter|$math|".'(?:\\\\\\$)?'."$number)";
   
   return ($VowelSound)
}

##############################################################################
sub SimpleRule {
my $bad=shift(@_);
my $good=shift(@_);
my $bad_regex=$bad;
$bad_regex=~s/ /\\s+/g;
my $first_char=substr($bad_regex,0,1);
my $remainder=substr($bad_regex,1);
my $uc_first_char=uc($first_char);
$bad_regex="\\b[$first_char$uc_first_char]$remainder\\b";
$bad_regex=~s/[.].b$/./g;

#system ("echo ___ '$bad_regex'. >> ".'/tmp/asdfasdf');
if (defined($good)) {
	return [$bad, $bad_regex, "", "Perhaps you mean '$good'?"];
} else {
	return [$bad, $bad_regex, "", ""];
}
}

#include "chktex_CONSTLANGUAGE.pl"
my $short_lang=substr($ENV{"LANG"},0,2);
my $lang_file=$0;
$lang_file =~ s/.pl$/_$short_lang.pl/;
our @ErrorTypes=();
#print "$lang_file\n";
do $lang_file;

##############################################################################
sub NumNewlines{
	my $str=shift(@_);
	our $newlinereg="\n";
	my @newlines = ($str=~ /$newlinereg/go);
	return(@newlines+0);
}

##############################################################################
sub max{
	my $a = shift(@_);
	my $b = shift(@_);
	my $c;

	if ($a > $b) {
		$c=$a;
	} else {
		$c=$b;
	}

	return $c;
}

#builddict();
###########################################################################
sub builddict {
 if (not %dictionary) {
  foreach my $d (("master","personal")) {
        #open(DICT, "LANG=en aspell dump $d | grep ^[[:upper:]][[:alnum:]]*$ |")   
        #open(DICT, "LANG=en aspell dump $d | grep ^[[:upper:]] |")            
        open(DICT, "LANG=en aspell dump $d |")                    
                       or die "Cant start aspell dump $d";
{ local $_;
        while (<DICT>) {
                chomp;
                $dictionary{$_}=1;
        }
}

#if defined($dictionary{"In"}) {
#	#"In" can be the code for the element "Indium", in which case it should be captialised
#	# but usually it is "in" as an "inside" and should not be captialised unless it is the 
#	# beginning of a sentance.
#	delete $dictionary{"In"};
#}

        #print %dict;

        close DICT
  }
 }  
}

###########################################################################
sub FindMismatchs {
# This finds things that are not errors of themselves, but do not match
# the style of other parts of the document.
my $OutFiles=shift(@_);
my $text=shift(@_);

#We should pick only one OR the other of these pairs
my @eitheror = ( 
		 # Formulae and Lemmata are both "old" so arguably they should be used together. Not really an error not to though I guess.
		 # [ '', '([Ff]ormulas|[Ll]emmas)', '(?<![\\\\])([Ff]ormulae|Lemmata)' ],
		  [ '', '[Ff]ormulas', '(?<![\\\\])[Ff]ormulae' ],
		  [ '', '[Ll]emmas', '(?<![\\\\])[Ll]emmata' ],
		 [ 'Case Mismatch', '(?<![.]\s)(Lemma|Corollary)\s+.ref', '(lemma|corollary)\s+.ref' ],
		 [ '', '[Cc]olour', '(?<![\\\\])[Cc]olor' ],
		 [ '', 'axiomatisation', 'axiomatization' ],
		 [ 'Case Mismatch[2]', '(?<![.]\s)Theorem\s+.ref', 'theorem\s+.ref' ] );
#		 [ '', '(?<![.]\s)Theorem', 'theorem' ] );


foreach my $r (0..@eitheror-1)
{
  my $type;
  if ($eitheror[$r][0]) {
	$type=$eitheror[$r][0];
  } else {
	$type="Style Mismatch";
  }
  my $prior="";
  foreach my $c (1..@{$eitheror[$r]}-1)
  {
    my $e=$eitheror[$r][$c];
    if ($text=~/($e)/) {
	#print ">$1\n";
	if ($prior) {
		#print "Both $prior and $1 exist; ($type) \n"
		my $linenum = 1 + substr($text,0,$-[0]) =~ y/\n//;
		ReportError($OutFiles,
		$linenum,1,669,$type,
		"Both $prior and $1 exist",
		,"",,$filename);
	}
	$prior=$1;
   }
  }
}

}
###########################################################################
sub FindErrors {
my @ErrorTypes=@{shift(@_)};
my $OutFiles=shift(@_);
my $filetext=shift(@_);
my $min_block_size=shift(@_);
##

my $ErrorRegex;
my $ErrorRegexStr;
my $NotErrorRegexStr;
my $NotErrorRegex;

my $nErrors=0;

my $PrevNewlines=0;

FindMismatchs($OutFiles, $filetext);

$filetext=~ s/(?:(?<!\\)%.*(?:\$|\n))/%\n/g; #remove comments
$filetext= tokenize($filetext);

{
	my @tmmp= split  /\\begin\{document\}/,$filetext;

	if (@tmmp>1) {
		$filetext=$tmmp[1];
		$PrevNewlines=NumNewlines($tmmp[0]);
		if (@tmmp>2) {
			die("There should not be more than one \"\\begin{document}\" in a single TeX file, but there is.");
		}
	}
}

my $BracketRegex='(?<![[])\((?![?])';

Rassert($BracketRegex,'(aad(b(?d','(,(');

 for(my $e=0;$e<@ErrorTypes;$e++){
   $ErrorRegexStr='('.$ErrorTypes[$e][1].')';
   $ErrorTypes[$e][4]=qr/$ErrorRegexStr/;
   my @Brackets=($ErrorRegexStr =~ /$BracketRegex/g);
   $ErrorTypes[$e][5] = @Brackets+0;
}


Rassert($anychar,"a\n","a,\n");

my @blocks= split /($par(?:.|.\n){$min_block_size,}$par)/,$filetext;

my $merged;
my $newlines;
my $errorline;
my $spaceline;

my %old_pars=();
my %old_textchunks=();

foreach (@blocks) {

	my $blocktext=$_;
	my $errnum=0;

	my @pars= split /$par/,$blocktext;

	#Search for duplicated pars
	{my $linenum=1+$PrevNewlines;
	my $old_partext="***no_old_par_text***";
	my $inEmptyParBlock=0;
	foreach (@pars) {

		my $partext=$_;

if (0) { #BUG: commented this out as too many false positives? Fix instead?

		if ($partext =~ /\s*(?:\\par)?\s*[\\]end[{][^}]*[}]/) {
				ReportError($OutFiles,
				$linenum,1,668,"Empty begin/end block ".$linenum,
				"This begin/end block is empty: $old_partext $partext",
				,"",,$filename);
				$inEmptyParBlock=0;
		}

		
			
		if ($partext =~ /[\\]begin[{][^}]*[}]\s*$/) {
			$inEmptyParBlock=1;
		} elsif ($partext !~ /^$/) {
			$inEmptyParBlock=0;
		}
		
}

		# if a par is short, we do not care if it is a dup.
		if (length($partext)>80) {
			if(defined $old_pars{$partext}){
				ReportError($OutFiles,
				$linenum,1,667,"Duplicated paragraph ".$old_pars{$partext},
				#"This paragraph has already occured at line ".$old_pars{$partext}."\n".$partext,
				,"",,$filename);
			} else {
				$old_pars{$partext}=$linenum;
			}

			#The following test is essentially a refinement of the duplicated par test above
			#we should perhaps remove the par test?
			#... but too many false positives, so remove this instead.
		        if(0){
			while ($partext =~ /((?:\n|.){150})[.]/g) {
			    my $chunk=$1;
			    #BUG: following uses line number of paragraph rather than the text chunk
			    if(defined $old_textchunks{$chunk}){
                                ReportError($OutFiles,
                                $linenum,1,669,"Duplicated Chunk of Text ".$linenum,
                                "This chunk has already occured at line ".$old_textchunks{$chunk}."\n".$chunk,
                                ,"",,$filename);
			    } else {
				$old_textchunks{$chunk}=$linenum;
			    }
			}
			}
    		}

		$linenum=$linenum+NumNewlines($partext)+2;
		$old_partext=$partext;
		
	}}

	foreach (@ErrorTypes) {

		my $ErrorName=$_->[0];
		my $nBrackets=$_->[5];
		my $Special=$_->[2];
		my $ErrorDescription=$_->[3];

		my $blocktext_ = $blocktext;
		if ($Special =~ /erase:.*/) {
			my $Erase = $Special;
			$Erase =~ s/erase://;
			$blocktext_ =~ s/$Erase//g;
		}


		#Splits the file up so that every second array element is a
		#violation of the rule ErrorRegex.
		
		
		my @violations = split $_->[4], $blocktext_;
		
		my $linenum=1+$PrevNewlines;

		#Each extra bracket in the regex defines an extra variable the 
		#regex extracts and puts into @violations.
		my $inc=1+$nBrackets; 

		for (my $i=2;$i<@violations;$i+=$inc) {
			my $realerror=1;
			my $trigger_text=$violations[$i-1];
			$merged=$violations[$i-2].$trigger_text;
			$linenum=$linenum+NumNewlines($merged);
			my $ErrorND=$ErrorName.'. ';
			if ($ErrorDescription ne "") {
				$ErrorND .=$ErrorDescription.'. ';
			}

			$trigger_text=tokens_to_user($trigger_text);
			my $amount_of_context=max(0,35-length($trigger_text));
			my $ErrorContextBefore='...'.tokens_to_user(substr($violations[$i-2],-$amount_of_context));

			my $ErrorContext=$ErrorContextBefore.
				$trigger_text.
				tokens_to_user(substr($violations[$i+$inc-2],0,$amount_of_context)).'..';

			my $spaces = $ErrorContextBefore;
			$spaces =~ s/./ /g;

			my $rule_ptrs; # points up at the error. E.g. "         ^^^^^ "
			if ($trigger_text ne "") {
				$rule_ptrs= $trigger_text;
				$rule_ptrs=~ s/./^/g;
			} else {
				$rule_ptrs="^";
			}

			$rule_ptrs=$spaces.$rule_ptrs; #move the ^^^'s underneath the error

			my $ThisErrorDescription = $ErrorDescription;

			for my $n (1 .. ($nBrackets-1)) {
				my $arg=$violations[$i+$n-1];
				if ($ThisErrorDescription =~ /ARG$n.CAP/) {
					builddict();
					
					if (defined($dictionary{$arg})) {
						$realerror=0;
					} else {
						s/ARG$n.CAP/\"$arg\" does not appear to be a name or other word that is usually starts with a capital/g;
					}
				}
				$ThisErrorDescription =~ s/ARG$n/\"$arg\"/g;
			}
				
			#if ($nBrackets>1) {
			if (0) {
				my $arg1=$violations[$i];
				if ($ThisErrorDescription =~ /ARG1.CAP/) {
					#builddict();
					
					if (defined($dictionary{$arg1})) {
						$realerror=0;
					} else {
						s/ARG1.CAP/\"$violations[$i]\" does not appear to be a name or other word that is usually starts with a capital/g;
					}
				}
					
				$ThisErrorDescription =~ s/ARG1/\"$violations[$i]\"/g;
				if ($nBrackets>2) {
				my $arg2=$violations[$i];
					$ThisErrorDescription =~ s/ARG2/\"$violations[$i+1]\"/g;
				}
			}
			if ($realerror) {ReportError($OutFiles,$linenum,1,666,$ErrorName,$ThisErrorDescription,$violations[$i-1],$ErrorContext,$rule_ptrs,$filename);}

			$nErrors++;
		}
	}


	$PrevNewlines+=NumNewlines($blocktext);

}

			#system ("echo  nerrors $nErrors | xless");
return ($nErrors);
}

##############################################################################
sub embed_error_tags {
my $rule_context=shift(@_); # E.g. "We are an lost people."
my $rule_ptrs=shift(@_);    # E.g. "       ^^^^^^^        "  
my $S=shift(@_); #Start tag
my $E=shift(@_); #End tag

my $f= index($rule_ptrs,'^');
my $l= rindex($rule_ptrs,'^');
if ($f>-1) {
	$rule_context=substr($rule_context,0,$f).
		$S.substr($rule_context,$f,$l-$f+1).$E
		.substr($rule_context,$l+1);
	$rule_ptrs="";
}

return ($rule_context)}

###########################################################################
sub Report_Child_Errors {
my $OutFiles=shift(@_); # e.g. (*STDERR)

#our %child_errors;
#our $filename;
my $child_errors_str = join (".\n",keys %child_errors);

if ($child_errors_str ne "") {
	ReportError($OutFiles,1,1,667,"Errors in child file(s)",
		"\n$child_errors_str.  \n\nNote that many errors will not be detected in child files, unless you check each  file individually, you may not detect all errors",
		"","","",$filename);} 
}


###########################################################################
sub ReportError {
# Reports an error to outfiles. 

my @inputarray=@_;
my $OutFiles=shift(@_); # e.g. (*STDERR)
my $line_num=shift(@_); # e.g 123
my $col_num=shift(@_); # e.g. 1
my $rule_id=shift(@_); # e.g. "DUP_WORD" or 32
my $rule_name=shift(@_); #e.g. "Duplicated words."
my $rule_description=shift(@_); # "e.g. You have used rain twice."
my $rule_trigger=shift(@_); # e.g. "rain rain"
my $rule_context=shift(@_); # e.g. "The rain rain in spain"
my $rule_ptrs=shift(@_);    # e.g. "    ^^^^^^^^^         "
my $error_filename=shift(@_);    # e.g. SampleErrors.tex
my $suggestion=shift(@_);    # e.g. SampleErrors.tex

#my $LyX_newline_hack="\t\t\t\t\t\t\t\t\t\t";
my $LyX_newline_hack;
my $LyX_par_hack;
my $LyX_colon_hack = "<COLON/>";

our $filename; #error_filename may differ from global filename if error_filename is a child file.
our %child_errors;

#fflush PF

if (!defined $OutFiles)		{ printf STDERR "Outfiles undefined in ReportError\n"; }
if (!defined $line_num)		{ printf STDERR "line_num undefined in ReportError\n"; }
if (!defined $col_num) 		{ printf STDERR "col_num undefined in ReportError\n"; }
if (!defined $rule_id)		{ printf STDERR "rule_id undefined in ReportError\n"; }
if (!defined $rule_name) 	{ printf STDERR "rule_name undefined in ReportError\n"; }
if (!defined $rule_description)	{ printf STDERR "rule_description undefined in ReportError\n"; }
if (!defined $rule_trigger) 	{ printf STDERR "rule_trigger undefined in ReportError\n"; }
if (!defined $rule_context)	{ printf STDERR "rule_context undefined in ReportError\n"; }
if (!defined $rule_ptrs) 	{ printf STDERR "rule_ptrs undefined in ReportError\n"; }
if (!defined $error_filename)	{ $error_filename=$filename; }

$rule_name =~ s/[ \n]*$//g;
#{ printf STDERR "filename undefined in ReportError\n"; }
$rule_context =~ s/[ \n]$//g;

$rule_id="$rule_id; $rule_name";
my $ErrorText="";

my $lyx_gui= lc($ENV{"LYX_GUI"});
if (!defined $lyx_gui)	{ $lyx_gui ="";}

#printf STDERR ":$lyx_gui:\n";
printf STDERR "LYX_GUI:$lyx_gui:\n";

if ($lyx_gui  =~ /qtXXXXX/) {
	$LyX_newline_hack=" \t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t";
	$LyX_par_hack="$LyX_newline_hack. $LyX_newline_hack";
	#We replaces the ASCII Colon with a UTF-8 Ethiopic, Cherokee colon.
	#$LyX_colon_hack = chr(225).chr(141).chr(161);

	#$LyX_par_hack="$LyX_newline_hack.\t";
} elsif ($lyx_gui  eq "xforms") {
	$LyX_newline_hack=
"                                                                              ".
"                                                                              ".
"                                                                              ".
"                                                                              ";
	$LyX_par_hack=$LyX_newline_hack;

#	$LyX_par_hack="$LyX_newline_hack.$LyX_newline_hack";
#	$LyX_par_hack="$LyX_newline_hack";
} else {
	$LyX_newline_hack="  ";
	$LyX_par_hack=$LyX_newline_hack;
	#$LyX_par_hack="    ";
}

#Useful for debugging
#$LyX_newline_hack='\n';
#$LyX_par_hack='\n\n';
#$ErrorText="(($rule_name)).\n\n";

if ($filename ne $error_filename) {
	$child_errors{"> $rule_name in \"$error_filename\""}=""; # "" is arbitrary, we are just making sure the key $error_filename exists.
} elsif (our $output_format =~ "-v[03]") {
	if ($rule_description ne "") {
		$ErrorText .="$rule_description.\n\n";
	}
#	if ($rule_trigger ne "") {
#		$ErrorText .='The actual text that triggered the error was "'.$rule_trigger."\".  ";
#	}
	if ($rule_context ne "") {
		$rule_context =~ s/\n/ /g;
		$rule_context =~ s/\r/ /g;
		$rule_context =~ s/^\s*/ /g;
		$ErrorText .='> '.embed_error_tags($rule_context,' '.$rule_ptrs,'>>','<<').".\n\n  ";
	}
	if ($error_filename ne $filename) {
		$ErrorText .='Error included from child <FILE>'.$error_filename."</FILE>.  ";
	}

	$ErrorText =~ s/[ \t\n]*$//g;


	if (defined $suggestion) { $ErrorText.="\n$suggestion"; }
	#$ErrorText =~ tr/\n/ /; # ugly hacks to fit error into chktex -v0 mode format.
	$ErrorText =~ s/\n\n/$LyX_par_hack/g; # ugly hacks to fit error into chktex -v0 mode format.
	$ErrorText =~ s/\n/$LyX_newline_hack/g; # ugly hacks to fit error into chktex -v0 mode format.
	#$ErrorText =~ s/[:]/COLON/g;
	$ErrorText =~ s/[:]/$LyX_colon_hack/g;
	$rule_id =~ s/[:]/$LyX_colon_hack/g;

	$ErrorText = tokens_to_user($ErrorText); # or we could use detokenize(ErrorText) if our users were familiar with TeX.

	if  ($output_format eq "-v0") {
		#Example of -v0 error format
		#SampleGrammarErrors.tex:11:48:1:Command terminated with space.
		$error_filename =~ s/[:]/<COLON\/>/g;
		$ErrorText="$error_filename:$line_num:$col_num:$rule_id:$ErrorText\n";
	} else {
		#Example of -v3 (lacheck) error format
		#"SampleGrammarErrors.tex", line 11: Command terminated with space.
		$ErrorText="\"$error_filename\", line $line_num: $ErrorText\n";
	}
} else {

	if ($rule_description ne "") {
		$ErrorText .=$rule_description.'.  ';
	}
	$ErrorText    =~ tr/\n/ /; # hacks to fit error into chktex -v1 mode format.
	$rule_context =~ tr/\n/ /; # hacks to fit error into chktex -v1 mode format.
	$rule_ptrs    =~ tr/\n/ /; # hacks to fit error into chktex -v1 mode format.
			
	$ErrorText = tokens_to_user($ErrorText); # or we could use detokenize(ErrorText) if our users were familiar with TeX.
	
	if ($output_format eq "-v1") {
		#Example of -v1 error format
		#Warning 1 in SampleGrammarErrors.tex line 11: Command terminated with space.
		#\providecommand{\boldsymbol}[1]{\mbox{\boldmath $#1$}}
		#                                               ^

	} else {
		#Example of -v2 error format
		#Warning 1 in SampleGrammarErrors.tex line 11: Command terminated with space.
		#\providecommand{\boldsymbol}[1]{\mbox{\boldmath[7m [27m$#1$}} 
		#
		
#		my $S="[7m"; #tags that start and end inverse mode
#		my $E="[27m";
#		my $S="»"; #tags that start and end inverse mode
#		my $E="«";
#		my $S=chr(0xAB);
#		my $E=chr(0xBB);
		my $S=">>";
		my $E="<<";
		
		$rule_context=embed_error_tags($rule_context,$rule_ptrs,$S,$E);
	}
	$ErrorText="Warning $rule_id in $error_filename line $line_num: $ErrorText\n$rule_context\n$rule_ptrs\n";
}

#$ErrorText =~ tr/\'/\"/;
#$ErrorText =~ tr/\'/ /;

my $error_signature="$rule_id:$rule_name:$rule_trigger:$rule_context";
$error_signature=~ s/\n/ /g;

print PF "$error_signature\n";

if (defined($false_positives{$error_signature})!=1) {
	foreach(@{$OutFiles}) {
		print $_ $ErrorText;
	}
} else {
	$FalsePositivesSuppressed=1;
}

}

###########################################################################
sub Parse_lacheck_Output {
my $filename=shift(@_);
my $OutFiles=shift(@_);
##

#example lacheck output
#"SampleGrammarErrors.tex", line 46: missing `\ ' after "sentance."

my $nErrors=0;
my $line_num=""; 
my $error_filename="";
my $ErrorName="";
my $ErrorDescription="";

my $ErrorContext=""; #These are just stubs as lacheck output is very limited.
my $ErrorPtr="";
my $col_num="1";
my $rule_id="lacheck"; 

my $in_error=0;

my $ignore_reg=qr/(?:^possible unwanted space at|^Could not open|^Whitespace before punctation mark in|^punctuation mark \".\" should be placed after end of math mode|^bad character in label|^-> unmatched \"math begin \$"|^.. unmatched)/o;
#"Could not open" generates false positives in LyX, anyway you are likely to figure it out for your self, soon enough if it were correct
#why should I care if there is a "bad character in label", it doesn't cause any problems... right?

if ( open(INPUT_FILE, "lacheck $filename|") ) {
        ###system ("echo Using lacheck on $filename. >> ".'$DEBUGFILE');
        print (STDERR "running\n  lacheck $filename\n");

	while (<INPUT_FILE>) {
		if ( $_ =~ /\"(.*)\", line ([[:digit:]]+): (.*)/ ) {
			{ #We have to put this in a sub block, or else the "!~" operator below will overwrite $1,$2 and $3
				if ($in_error && $ErrorName !~ $ignore_reg) {
					ReportError($OutFiles,$line_num,$col_num,$rule_id,$ErrorName,"","",$ErrorContext,$ErrorPtr,$error_filename,"");
					$nErrors++;
				}
			}
		
			$error_filename=$1;
			$line_num=$2;
			$ErrorName=$3;
			$ErrorDescription="";
 
			$in_error=1;
		} else {
			$ErrorDescription .= "\n".$_;
		        #print (STDERR "\nUnknown lacheck line from $filename:\n  $_");
		}
	}

	close (INPUT_FILE);
	if ($in_error && $ErrorName !~ $ignore_reg) {
		ReportError($OutFiles,$line_num,$col_num,$rule_id,$ErrorName,$ErrorDescription,"",$ErrorContext,$ErrorPtr,$error_filename);
		$nErrors++;
	}

} else {
        print (STDERR "warning: could not run: \n  lacheck $filename");

}

return ($nErrors)}


###########################################################################
sub Parse_ChkTeX_Output {
my $filename=shift(@_);
my $OutFiles=shift(@_);
##

#We use this function because LyX uses the -v0 output format which is exceptionally terse.
#This function takes normal verbose -v1 format and puts as much information it can get into
#whatever output format you choose.

#example ChkTeX output
#Warning 40 in SampleGrammarErrors.tex line 52: You should put punctuation outside inner math mode.
#$A:$ is a variable.
#  ^
  

my $nErrors=0;
my $line_num; 
my $error_filename;
my $ErrorName;
my $rule_id; 
my $ErrorContext;
my $ErrorPtr;
my $type;

my $col_num="0"; 
my $error_pos=1; # The position within an error report. 

my @ChkTex_Params= grep ( /^-[^ov]/,  @ARGV );

# -n17 number of '[' and ']' dont match. Should by disabled as it generates false positives in math text, e.g. you may have
# "a number in the range [0,1).
# -n16 generates false postives and LyX should always generate correct code... we hope.
#my $ChkTeX_Command="/usr/bin/chktex $filename -n16 -n1 -n31 -n27 -n36 -n40 ".join (" ",@ChkTex_Params).' -v1';
my $ChkTeX_Command="$ENV{ORIG_CHKTEX} $filename -n26 -n24 -n15 -n16 -n1 -n31 -n27 -n36 -n40 -n2 ".join (" ",@ChkTex_Params).' -v1';
# The -v1 ensures that we get the default output type from chktex regardless of the input stream.

if ( $ENV{ORIG_CHKTEX} ne "" &&   open(INPUT_FILE, $ChkTeX_Command.' |') ) {
#if (  open(INPUT_FILE, $ChkTeX_Command.' |') ) {
        ###system ("echo running: $ChkTeX_Command  >> ".'$DEBUGFILE');
        print (STDERR "running:  $ChkTeX_Command\n");

	while (<INPUT_FILE>) {
	        #print (STDERR $error_pos." $_");
		if ($error_pos==1) {
			if ( $_ =~ /(Warning|Error|Message) ([^ ]+) in (.*) line ([1-9][0-9]*): (.*)/ ) {
				$type=$1;  #ATM, we ingore type and just use "Warning" for output
				$rule_id=$2;
				$error_filename=$3;
				$line_num=$4;
				$ErrorName=$5;
				$ErrorName=~ s/\.$//;
				$error_pos=2;
			} else {
			        print (STDERR "\nUnknown ChkTeX line from $filename:\n$_");
			}
		} elsif ($error_pos==2) {
			$ErrorContext=$_;
			$ErrorContext=~ s/\.$//;
			$error_pos=3;
		} elsif ($error_pos==3) {
			$ErrorPtr=$_;
			$nErrors++;
			ReportError($OutFiles,$line_num,$col_num,$rule_id,$ErrorName,"","",$ErrorContext,$ErrorPtr,$error_filename);
			$error_pos=1;
		}
	}
	if ($error_pos>1){
		print (STDERR "ChkTeX output stopped unexpectedly\n");
	}

	close (INPUT_FILE);

} else {
        #system ("echo could not run: \n  lacheck $filename".'$DEBUGFILE');
        print (STDERR "Warning, could not run: $ChkTeX_Command\n");

}

return ($nErrors)}


###########################################################################
sub Parse_JLanguageTool_Output {
my $filename=shift(@_);
my $OutFiles=shift(@_);
##

my $error_pos=-1; # The position within an error report. 
my $nErrors=0;
my $line_num; 
my $col_num; 
my $rule_id; 
my @error_data; # Example of contents follows:
# error_data[0] = "1.) Line 50, column 8, Rule ID: UPPERCASE_SENTENCE_START"
# error_data[1] = "Message: This s entence does not start with an uppercase letter"
# error_data[2] = "...nstructions.             alvaro"
# error_data[3] = "                            ^^^^^^"


#33.) Line 208, column 0, Rule ID: UPPERCASE_SENTENCE_START
#Message: This sentence does not start with an uppercase letter
#Suggestion: Where
#...rma|\eA\phi,\end{align*} where $p$ is any variable.   \...
#                            ^^^^^                         

# These rules are not implemented in a TeX compatible way in JLanguageTool;
# they generate false positives. Also I have implemented them in TeX compatible
# way, so they are not needed.
my %ignore_rules=(
	ARTICLE_MISSING => 1,
#	HE_VERB_AGR => 1,
	NOW => 1,
	EG_NO_COMMA => 1,
	IE_NO_COMMA => 1,
	EN_A_VS_AN => 1,
#	POSSESIVE_APOSTROPHE => /worlds/,
	WHITESPACE_RULE => 1,
	UNPAIRED_BRACKETS => 1,
	EN_UNPAIRED_BRACKETS => 1,
	COMMA_WHITESPACE => 1,
	WORD_REPEAT_RULE => 1,
	COMP_THAN_2 => 1, # I really don't understand what rule COMP_THAN_2 is  
	COMMA_PARENTHESIS_WHITESPACE => 1,
	COMMA_PARENTHESIS_WHITESPACE => 1,
	"EN_QUOTES[3]" => 1, # This suggests that we replace `` with Smart Quotes, but LaTeX does that for us
	DOUBLE_PUNCTUATION => 1);

$ignore_rules{'ENGLISH_WORD_REPEAT_BEGINNING_RULE'} = 1; #Can't be bothered fixing these really.
$ignore_rules{'IN_A_X_MANNER[1]'} = 1; #I think "in a similar way" is valid and useful english
$ignore_rules{'CURRENCY[1]'} = 1;
$ignore_rules{'CURRENCY_SPACE[1]'} = 1;
$ignore_rules{'EN_QUOTES[2]'} = 1; # Complains about e.g. B\"uchi
$ignore_rules{'BIG_IN_SIZE[1]'} = 1; # ... IN casual writing, "Exponential in Length" may be unnecessarily wordy, but in scientific writing it means something very different to, say "quadratic in volume". It is not just unnecessary verbage as LanguageTool suggests. 

my %ignore_regexs=(
	POSSESIVE_APOSTROPHE => "worlds",
	POSSESIVE_APOSTROPHE => "worlds",
);

my $out_file="$filename.languagetool";
if ( open(INPUT_FILE, $out_file) ) {
        ###system ("echo $filename.languagetool found >> ".'$DEBUGFILE');
	my $gotlock=0;

	my $lockfile="$out_file.lock";

	use Fcntl ':flock';
	print STDERR "Wait for lock to read $out_file: We may have to wait several seconds for JLanguageTool to complete\n"; 
	open (LOCKFILE, ">>$lockfile") or die "cannot open $lockfile for appending";
	flock (LOCKFILE, LOCK_SH);
	print STDERR "Got lock to read $out_file\n";

	while (<INPUT_FILE>) {
		if ( $error_pos == -1 ) {
			if ( $_ =~ /Line ([[:digit:]]*), column ([[:digit:]]*),.*Rule ID: (.*)$/ ) {
				$line_num=$1;
				$col_num=$2;
				$rule_id=$3;
				$error_pos=0;
			}
		}
		if ( $error_pos >= 0 ) {
			my $ErrorSuggestion="";
			$error_data[$error_pos]=$_;
			if ($error_pos==2 && $_ =~/^Suggestion:/) {
				$ErrorSuggestion.=$_
			} else {
				$error_pos++;
			}
			if ($error_pos > 3) {
				print STDERR "##$rule_id##$error_data[1]##\n"; # Only for debugging I think.
				if ( ! defined($ignore_rules{$rule_id}) ) {
				if ( ! (defined($ignore_regexs{$rule_id}) && ($error_data[1] =~ /$ignore_regexs{$rule_id}/) ) ) {
					my $ErrorName=$error_data[1];
					$ErrorName=~ s/Message: //;
					#my $ErrorContext=$error_data[3]."\n\n".$error_data[2];
					
					#my $ErrorSuggestion=$error_data[2];
					my $ErrorContext=$error_data[2];
					my $ErrorPtr=$error_data[3];
					print STDERR "ErrorName: $ErrorName\n";
					print STDERR "ErrorSuggestion: $ErrorSuggestion\n";
					print STDERR "ErrorContext: $ErrorContext\n";
					print STDERR "ErrorPtr: $ErrorPtr\n";
					$ErrorSuggestion=~s/:/;/;
					ReportError($OutFiles,$line_num,$col_num,$rule_id,$ErrorName,"","",$ErrorContext,$ErrorPtr,$filename,$ErrorSuggestion);
					$nErrors++;
				}}
				$error_pos=-1;
			}
		}
	}
	close (INPUT_FILE);
  	###system ('rm '.$lockfile);
	unlink $lockfile;
  	flock (LOCKFILE, LOCK_UN);
	close (LOCKFILE);
  	print STDERR "Released lock to read $out_file\n";
} else {
	print STDERR "could not open $out_file for reading\n";
        ###system ("echo $filename.languagetool not found >> ".'$DEBUGFILE');

}

return ($nErrors)}


###########################################################################
# From http://www.perlmonks.org/?node_id=36976
sub Home {
	return $ENV{HOME}        if $ENV{HOME};
	return $ENV{USERPROFILE} if $ENV{USERPROFILE};
	return  "";
}


###########################################################################
#Mainline

#our $filename="";
my $filetext;

our $fileout="";
our $output_format="-v1";
for (my $i=0;$i<@ARGV;$i++) {
	if ($i>0 && $ARGV[$i-1] eq '-o') {
		$fileout=$ARGV[$i];
	} else {
		if ($ARGV[$i] !~ /^-/) {
			$filename=$ARGV[$i];
		} else {
			if ($ARGV[$i] =~ /^-v/) {
				$output_format=$ARGV[$i];
			}
		}
	}
}

my @lines;
if ($filename ne "") {
#	$filename =~ s/.tex$//g;
	$filename=abs_path($filename);

	open(INFILE,  "$filename")   or die "Can't $filename: $!";
	@lines = <INFILE>;
} else {
	$filename = 'stdin';
	print STDERR "Reading from stdin\n";
	@lines = <STDIN>;
}


my $HOME=$ENV{"HOME"};
my $settings_dir="$HOME/.lyx-gc";

system("mkdir -p '$settings_dir'");
#mkdir $settings_dir or die "cannot mkdir $settings_dir\n";

my $basename=basename($filename);
my $falsepositive_filename = $settings_dir . '/' . $basename . '.falsepositive';
my $positive_filename = $settings_dir . '/' . $basename . '.positive';

if (open (FP, '<' , $falsepositive_filename)) {
	while (<FP>) {
		chomp;
		$false_positives{$_}=1;
	}
}

open (PF, '>', $positive_filename) or die "cannot open " . $positive_filename;
my $date=`date`;
print PF "FOO $date\n";


$filetext = join ("", @lines);
@lines={}; # Free memory;

my @OutFiles=(*OUTFILE);
if ($fileout ne "") {
	#system ("rm $fileout");
	#system ("echo -n > $fileout");
	open(OUTFILE, ">$fileout") or die "Can't open $fileout for writing: $!";
	@OutFiles=(*OUTFILE);
} else {
	@OutFiles=(*STDOUT);
}
my @ErrorTypes=GenerateErrorTypes("$filename");


my $nErrors=FindErrors(\@ErrorTypes,\@OutFiles,$filetext,200);

#BUG: this will only work if you do not pass in the input file via stdin
$nErrors+=Parse_JLanguageTool_Output($filename,\@OutFiles);
$nErrors+=Parse_lacheck_Output($filename,\@OutFiles);
$nErrors+=Parse_ChkTeX_Output($filename,\@OutFiles);
Report_Child_Errors(\@OutFiles);

if ($fileout ne "") {
	close OUTFILE;
	# LyX appears to have a bug where if there are no Errors, the last set of errors is displayed. This can be confusing so with display a dummy "All OK" (non)-Error.
	my $suppressed_msg="";
	if ($FalsePositivesSuppressed) {
		$suppressed_msg=", but false positives were suppressed";
	}
	system ("grep : '$fileout' || echo 'X:1:1: All OK (^_^) $suppressed_msg' >> '$fileout'");
}

system("nohup nice perl $0.JLanguageTool.pl $filename & " );
