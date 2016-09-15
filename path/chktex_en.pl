##############################################################################
sub GenerateErrorTypes {
our $endnumber='(?=[^[:digit:]]|$)'; 
our $funnynumber="(?:11|18)(?:[[:digit:]]{2})?(?:[[:digit:]]{3})*$endnumber"; # e.g. Eighteen(vowel), not One Eight.
our $vowelnumber='\b'."(?:8[[:digit:]]*$endnumber|$funnynumber)";
our $consonantnumber='\b(?!'.$funnynumber.")[012345679][[:digit:]]*$endnumber";

Rassert($consonantnumber,"1800 180 a 8 2","180,2");

# include words & exclude words below borrowed from JLanguageTool, Copyright (C) 2005 Daniel Naber, licensed under LGPL.
my %SetOfVowels = (
	l => "aeiou", # lower case vowels
	U => "FHILMANXAEIOS", #upper case vowels i.e. F=eff which is a vowel
	d => "8", #vowels that are digits
	number=> $vowelnumber,
	includewords  => "(?:MF|NP|NL|LP|MPC|RTL|RMS|heir|RME|ME|heirloom|honest|honor|honorable|honorarium|honorary|honorific|honour|hour|hourglass|hourly|HTML|XML|FBI|SGML|SDL|HAA|LTL|SAA|S5|FSA|SSPM)", #RoCTL
	#excludewords => "(?:US.*|Euridyce|Euripides|Euroclydon|Eurocommunism|Eurocrat|Eurodollar|Euromarket|Europa|Europe|European|Europeanisation|Europeanise|Europeanised|Eurovision|Unix|eurhythmic|eurhythmy|euripus|one|unary|uniform|uniformally|uniformisation|uniformise|uniformitarian|uniformitarianism|uniformity|unify|unijugate|unilateral|unilateralisation|unilateralise|unilateralist|unilinear|unilingual|uniliteral|union|unique|unit|united|unity|universal|universalisation|universalise|universalism|universalist|universalistic|universality|universe|university|univocal|US|usage|useful|user|UK|uni.*|unanimous|utrees?|uni[[:alpha:]]*|util[[:alpha:]]*|usual)",
	excludewords => "(?:US[[:alpha:]]*|Eur[[:alpha:]]*|Unix|eurhythmic|eurhythmy|euripus|one|unary|US|usage|useful|user|UK|unanimous|utrees?|uni[[:alpha:]]*|util[[:alpha:]]*|usual)",
	s => "=", #equals starts with a vowel
	isvowelset => 1 # Is a set of vowels
);


my %SetOfConsonants = (
	l => setDiff("abcdefghijklmnopqrstuvwxyz",$SetOfVowels{"l"}),
	U => setDiff("ABCDEFGHIJKLMNOPQRSTUVWXYZ",$SetOfVowels{"U"}),
	d => setDiff("0123456789",$SetOfVowels{"d"}),
	number => $consonantnumber,
	includewords=>$SetOfVowels{"excludewords"},
	excludewords=>$SetOfVowels{"includewords"},
	s => '+-<>#', # These symbols all sound like consonants
 	isvowelset => 0 # Is not a set of vowels
);

my $VowelSound=GenerateVowelRegex(\%SetOfVowels);

my $ConsonantSound=GenerateVowelRegex(\%SetOfConsonants);


#my $AlwaysUpperWord="Khachian"; 

Rassert($ConsonantSound,'apple Apple pear X-ray 1800 180 0','pear,ray,180,0');
Rassert($VowelSound,'apple Apple pear X-ray 1800 180 0','apple,Apple,X,1800');
Rassert("An $ConsonantSound",'An \$500 An \$1800','An \$500');
## OK, we have generated important information on vowels, now we can generate our error list.

#my $nonstoppar='(?<!'.$fullstop.')(?<!\\\\|\}|\{|\\n|\s|\?|[:]|\])(?<=.)\s*(?='.$par.')';
#I don't know why we need the "~" below.
my $nonstoppar='(?<!'.$fullstop.'|\\s|~|!|[)]|;)\\s*(?<!\\\\|\}|\{|\\n|\s|\?|[:]|\])(?<=.)\s*(?='.$par.')';

Rassert($nonstoppar,"aasdsd.","");
Rassert($nonstoppar,"aasdsd}","");
Rassert($nonstoppar,"aasdsd\\","");
Rassert($nonstoppar,"aasdsd","EMPTYSTR");
Rassert($nonstoppar,"dfasfdas

Asdfadsafsd

Adsfasdf.
",",");

#my $names="(?!Saari|Nanson|Condorcet|Borda|Fishburn|Laslier|Dodgson|Tideman)";
my $corpnames="Intel";
my $names="Hilbert|Gentzen|Xu|Priorean|Schwendimann|Achilles|Dam|Mally|Kant|Kamp|Burgess|Rabin|Broersen|Johnsson|Saari|Nanson|Condorcet|Borda|Fishburn|Laslier|Dodgson|Tideman|Pratt|Lei|Clarke|Emerson|Sistla|Wolper|Vardi|Schnoebelen|Turing|Broesen|$corpnames|Until|Since|Hodkinson|Dedekind|Borel|Achilles|Zeno|Zenoness|Läuchlii|Leonard|Stavi|Dedekind|Planck|Hintikka|Lange|Waldmeister";

Rassert($nonstoppar,'we may consider probability spaces to be a pair $(\outcomes,m)$. 

The function.',"");

# Format of an error type: 
# [ErrorName, ErrorRegex, Reserved, ErrorDescription]
# Also note that the text is partially tokenized so you will have to use $start_math or $end_math in place of "$"

# perl -e '$x = "ab"; if ($x =~ /(?<!c)b/) { print "yes\n" };'
my $acronym="\\b(?:[[:upper:]][[:alpha:]]*[[:upper:]][[:alpha:]]*|[[:upper:]][[:alnum:]]*[[:upper:]0-9][[:alnum:]]*)\\b";
my $lowerword="\\b(?<!\\\\)[[:lower:]]+\\b";

my $titletype="\\\\(?:chapter|(?:sub)*section)[[]";
#my $notitle="^(?:(?!$titletype).)*";
#my $notitle=".*";
my $day="(?:Monday|Tuesday|Wednesday|Thursday|Friday)";
$names="$names|Moore|Reynolds|Fisher|Wright|Muller|Belnap|Perloff";
my $LatexLabels="(?:Example|Chapter|Lemma|Corollary|Theorem|Section)s?";
my $inquote="(?:[^'].|.|\\s)*(?='')"; # We can be a bit more forgiving of style inside quotes as it need not match the rest of the text. Also this is a bit of a hack as it only handles double quotes.

my $termdef="(?:(?:[[:upper:]][[:alnum:]]*|in|of|the|a|an|to|for|and)\\s+)+(?:\\\\cite[{][^}]*[}]\\s*)?[(]"; #A term definition e.g. University of Western Australia (UWA) ... actually, maybe this should be called an acronym definition. And it should probably be merged with the $acronym. Oh Well.
my $capword="\\b(SSPM|Case|Ubuntu|Fair B?CTL|Figure|Algorithm|Boolean|Booleans|CTAB|English|Apollo|Caen|Utilitarianism|University|(?<=the\\s)School|Ro(?:B|[(]B[)]|)CTL|(?:Society of )?Social Choice and Welfare|(?:University of )?Western Australia|Boolean|Noetherian|Pentium|Backus-Naur\\s+Form|Euclidean|Deontic\\s+S5|Robustly|Prone|Obligatory|AllPaths|Permissible|Viol|Counter-Free|Deontic S5|State-|Definitions?|Prolog|Bayesian|Voice\\s+over\\s+Internet\\s+Protocol|Internet|Denial[- ]of[- ]Service|Contrary-to-Duty|Modus|Necessitation|Wolter|$names|$LatexLabels|Drinkers'? [Pp]aradox|Distribution|Substitution|Obligatory|McCabe|Streett|Dansted|AllPath|Allpath|Next|Until|Computation|Pair-RoCTL|State-RoCTL|Saul|Hughes|Cresswell|Kripke|[[:upper:]]|(?:[[:upper:]][[:lower:]]+(?:\\s|-)+)+.$acronym|$acronym|[[:upper:]][[:lower:]]*,?\\s+[[:upper:]][[:lower:]]*|$day|$termdef|$inquote|Australia|Dr|Prof|Pawsey|Canberra|Greek|First-order Monadic|Until operator|Untils|Table|Conjecture|EXPTIME)\\b";

#Use of both proof-theory and proof theory.

#--------------#
my $s='(?:\n|\s)';
our @ErrorTypes=(
 #["Error Name",	"(err[[:space:]]*or.)",	"",  "Error Description"],
#SimpleRule("satisfiable"),
SimpleRule("we that", "we see that"), 
SimpleRule("spelt correctly", "spelled correctly"), #Make Americans Happy.
SimpleRule("into to", "into"),
SimpleRule("never-the-less", "nevertheless"),
SimpleRule("the automata", "the automaton"),
SimpleRule("but and", "but"),
SimpleRule("in terms on the", "in terms of the"),
SimpleRule("in terms if the", "in terms of the"),
SimpleRule("psuedo", "pseudo"),
SimpleRule("visa-versa", "vice versa"),
SimpleRule("visa versa", "vice versa"),
SimpleRule("that the exists", ""),
SimpleRule("our selves", "ourselves"),
SimpleRule("number of reason","number of reasons"), 
SimpleRule("the both","both"), 
SimpleRule("we will shown","we show"), 
SimpleRule("this important because","this is important because"), 
SimpleRule("not contains","not contain"), 
SimpleRule("we interested","we are interested"), 
SimpleRule("of he","of the"), 
SimpleRule("is are",""), 
SimpleRule("the in",""), 
SimpleRule("This demonstrations",""), 
SimpleRule("will defined",""), 
SimpleRule("some a",""), 
SimpleRule("a some",""), 
SimpleRule("satisfiable in B?CTL-structure",""), 
SimpleRule("Hintakka","Hintikka"), 
SimpleRule("as assume","assume"), 
SimpleRule("eDecendants","eDescendants"), 
SimpleRule("way point","waypoint"), 
SimpleRule("of there",""),  # may be correct: get out of there`
SimpleRule("is grows",""), 
SimpleRule("non trivial","nontrivial"), 
SimpleRule("million of lines","millions of lines"), 
SimpleRule("that this provides use with a",""), 
SimpleRule("Now that we now that each",""), 
SimpleRule("over view","overview"), 
SimpleRule("an of",""), 
SimpleRule("the following a" ,"the following"), 
SimpleRule("a conservative extensions","a conservative extension"), 
SimpleRule("be use to","be used to"), 
SimpleRule("enquire as how",""), 
SimpleRule("model checker CTL","model checker for"), 
SimpleRule("can efficient",""), 
SimpleRule("polynomial model checking","polynomial-time model checking"), 
SimpleRule("greater motivation","stronger motiviation"), 
#SimpleRule("As will","As with"),  #False Positive: As will be shown...
SimpleRule("may first appear","may at first appear"), 
SimpleRule("comparision to","comparision with"), 
SimpleRule("comparisions to","comparisions with"), 
SimpleRule("polices","policies"), #Personal Rule.
SimpleRule("alteration","alternation"), #Personal Rule.
SimpleRule("a considerable research","considerable research"),
SimpleRule("avoid stay","stay"),
SimpleRule("reasonably to ask","reasonable to ask"),
SimpleRule("would beneficial","would be beneficial"),
SimpleRule("with be","will be"),
SimpleRule("a valid in",""),
SimpleRule("that occur which",""),
SimpleRule("more tractable", "more often tractable"),
SimpleRule("the deviation event", "the deviating event"),
SimpleRule("is no sufficent", "is not sufficent"),
SimpleRule("verify that is satisfies", "verify that it satisfies"),
SimpleRule("provides us with we have", "provides us with"),
SimpleRule("sch that", "such that"),
#["as follows.", "as follows[.]", '', 'Perhaps you should replace the "." with a ":".'], #Recommended by Tim French
#SimpleRule("we know", "we have"), #Recommended by Mark Reynolds, as more formal in a proof (Personal Rule)
SimpleRule("presented at", "presented in"), #Recommended by Tim French, also Google reports "present in" [conference] being twice as common as "presented at"
#I don't think there is a hard and fast rule as to when to use for or to
#SimpleRule("suited for", "suited to"), #Recommended by Tim French (or maybe Mark Reynolds?)
SimpleRule(" (?<!in.the.)tableau are", "tableaux are"), #Personal
#SimpleRule("variables?", "atom"), #Personal
SimpleRule("formally reason these", "formally reason about these"), 
SimpleRule("to presented", "presented"), 
SimpleRule("path-quantifies", "path-quantifiers"), 
SimpleRule("path quantifies", "path-quantifiers"), 
SimpleRule("on of", "one of"), 
SimpleRule("must exists", "must exist"), 
SimpleRule("see e.g.", "see for example"), # recommended by Tim French
SimpleRule("world formula(?:[es])?", "State formula"), 
SimpleRule("For example(?![,:])","For example,"), # source: http://owl.english.purdue.edu/owl/resource/607/02/
#SimpleRule(", that","that"), # source: http://owl.english.purdue.edu/owl/resource/607/02/
SimpleRule("there no","there is no"),
SimpleRule("show that is [^\\s]+ satisfiable","show that ___ is satisfiable"),
#SimpleRule("define (an? [^\\s]+) [^\\s]+ be \\2"),
SimpleRule("a special atoms"),
#["singular both", "[Bb]oth\\s+(?:[^.](?!s\\s|\\band\\b))*[.]", '', "You use both, which implies two items but I don't see 'and' or a plural"],
#["Empty mathblock", "$start_math_char $end_math_char", '', ""], 
["Two Pretty", '(section|Section|theorem|Theorem|Lemma|lemma|Chapter|chapter|Proposition|proposition|corollary|Corollary|fact|Fact) \\\\prettyref', "If you use prettyref you probably don't also need ARG1. ", ''], #Doesn't work
["Either ... and", '[Ee]ither\s+\w+\s+and', "", ''], #Doesn't work
["Plural Mismatch (as)", "s(?<!is)\\s+as\\s+a\\b", "", ''], #Doesn't work
["pspace-compLete", "(NP|PSPACE)[- ][Cc]ompete", "", ''], #Doesn't work
["Bad plural", "A B?CTL[- ][Ss]tructures", "", ''], #Doesn't work
#["A plural", "([Aa]n?)\\s+(?!(?:athletics|basis|bias|dais|diabetes|dialysis|emphasis|gas|iris|lens|mantis|mathematics|news|physics|seriesx|yes))[[:lower:]]*([^usi]s)\\b", "", 'You used ARG1, but words that end in ARG2 are usually plurals'], #Doesn't work
["A plural", '\b([Aa]n?)\s+(?!(?:athletics|basis|bias|dais|diabetes|dialysis|emphasis|gas|iris|lens|mantis|mathematics|news|physics|series|yes))[[:lower:]]*((?!ics)[[:lower:]](?![ius])[[:lower:]]s)\b', "", 'You used ARG1, but words that end in ARG2 are usually plurals'], #Doesn't work
#["A plural", '\b([Aa]n?)\s+es|dialysis|emphasis|gas|iris|lens|mantis|mathematics|news|physics|series|yes))*([[:lower:]][[:lower:]]s)\b', "", 'You used ARG1, but words that end in ARG2 are usually plurals'], #Doesn't work
#["A plural", '\b([Aa]n?)\s+[[:lower:]]*(s)[ .\n]', "", 'You used ARG1, but words that end in ARG2 are usually plurals'], #Doesn't work
#["No fullstop.", "^.*[^.]\$", '', ""], #Doesn't work
["Empty mathblock", "$start_math_char  *$end_math_char", '', ""], 
#["Macro without {}.", "[^\\\\]\\\\[[:alnum:]]+\\s[^\\s]", '', "You should probably put a {} after the macro so as not to swallow the space"], #Only outside math etc. 
["Formula in place of fullpath.", "\\\\(phi|Phi|Psi|psi|theta|alpha|beta)\\s*\\\\forces", '', "I usually use Phi as a formula, not a full path"],  #Personal Rule
["Mixing a and beta.", "aU\\\\beta", '', "I probably mean alpha U beta"],  #Personal Rule
["Thus twice", "Thus[^.]*[.]?[^.]*Thus", '', "Try not to use Thus twice in a row"],  #Personal Rule
["Macro without {}.", "[^\\\\]\\\\[[:alnum:]]+CTL[[:alnum:]]+\\s", '', "You should probably put a {} after the macro so as not to swallow the space"], 
["Second word in sentence is captalised.", "[.]\\s+[[:alnum:]]+\\s+(?!$capword)[[:upper:]]", '', "When I capitalise the second word of a sentance it is usually a mistake."], 
["no full stop after cite that ends paragraph", "[[:alnum:]]\\s+\\\\cite[{][^}]*[}]\\s*$par", '', "I think a fullstop is missing here."], 
#["[cite] note", "\\\\cite[{][^},]*[}]\\s*note\\b", '', "note -> notes"], #If multiple authors or papers, use "note"?
["No space after cite", "\\\\cite[{][^}]*[}][[:alnum:]]", '', ""], 
["No space before cite", "[[:alnum:]]\\\\cite[{][^}]*[}]", '', ""], 
["While while", "\\b[Ww]hile\\b[^.]*\\bwhile\\b", '', "`While' is not the sort of word that is often found twice in the same sentancee."], 
["Plural of acronym ending is S requires apostrophy between the `S'", "Ss\\b", '', ""], 
["Underline starts too early", "\\\\uline[{]\\s", '', ""], 
["Underline ends too late", "\\\\uline[{][^}]\\s[}]", '', ""], 
["Space before foot-note", "(?: %\\s+| )\\\\footnote[{]", '', ""], # BTW footnotes are described at http://www.bristol.ac.uk/arts/exercises/referencing/referencing%20skills/page_07.htm
["Period/Comma after foot-note", "\\\\footnote$recursive_brace".'[.,]', '', "If the foot note refers to the sentance as a whole it is normal to put the foot-note after the fullstop"], # BTW footnotes are described at http://www.bristol.ac.uk/arts/exercises/referencing/referencing%20skills/page_07.htm
#["Period after foot-note", "\\\\footnote[{][}][.]", '', "If the foot note refers to the sentance as a whole it is normal to put the foot-note after the fullstop"], # BTW footnotes are described at http://www.bristol.ac.uk/arts/exercises/referencing/referencing%20skills/page_07.htm
["Singular both", "[Bb]oth\\s+(?:[^.](?!s(?:\\s|[.])|\\band\\b))*[.]", '', "You use both, which implies two items but I don't see 'and' or a plural"],
#["\$ follows", "$end_math_char\\s+follows", '', "When I write '\$math\$ follows' I often mean '\$math\$ as follows"],
["Double [", '\\[\\[','',""],
["Double Punctuation", '(?<!\\\\)[,.:;]\\s+[,.:;]','',""],
["The [logic]", '[Tt]he (?:Ro)?(B)?CTL[*]?\\s+(?!.?formula)[:^[:space:]]','',"CTL is the name of a logic, so 'the' is redundant."],
["Use of implies for forward direction in proof", '(?:(?:\\\\left)?[(]\\\\implies(?:\\\\right)?[)]|[(].\\\\implies.[)])','',"Using (\\implies) leaves an ugly space between the arrow and the brackets. When indicating the forward direction of a proof it is probably better to use '(\\Longrightarrow)' instead"],
["Personal Rule; RoCTL{*}", 'Ro(?:B|[(]B[)]|)CTL.[{][{][*][}].[}]', "", "?"],
["Personal Rule; RoCTL missing *", '\sRo(?:B|[(]B[)]|)CTL\s', "", "?"],
["No space after reference", 'ref[{][^}]*[}][[:alnum:]]', "", ""],
#["No fullstop at end of par", '[[:alpha:]]\n\n', "", "?"],
#["Single char", '\s[b-z]\s(?![&\\\\])(?!', "", "?"],
["Single char", '\s[b-km-z]\s(?![&\\\\])'."(?![^$start_math_char]*[_$end_math_char])", "", "?"],
["A likely as", "\\b[Aa]\\s+likely\\s+as", "", "Perhaps you mean 'as likely as'?"],
["To modelled", "\\b[Tt]o\\s+modelled\\b", "", "Perhaps you mean 'to model'?"],
["A formulae", "\\ba\\s+\\\\?formulae", "", "Formula is plural, why are we using 'a'?"],
["Personal Rule, Robustly operated", "Robustly\\s+operated", "", "presumably I mean operator"],
["it every", "\\b[Ii]t\\s+every\\b", "", "Maybe you just want 'it' or 'every', not both"],
#["lemma above", "(?:[Ll]emma\\s+(?:above|below)|(?:[Aa]bove|[Bb]elow)\\s+lemma)", "", "It may be best to name the formula"],
["A we", "\\b[Aa]\\s+we\\b", "", "A is singular, We is plural, so this doesn't make much sense."],
["Personal Rule: valdities", "[Vv]aliditi(?:y|ies)", "", "I mean something like 'valid in'"],
["blow up", "[Bb]low\\s+up", "", "You may want to join these into a single word 'blowup'"],
["it self", "[Ii]t\\s+self", "", "You may want to join these into a single word 'itself'"],
#["Personal Rule: equivalence", "quivalence", "", "I might mean truth-preserving instead"],
["Personal Rule: ROCTL", "ROCTL", "", "I should write RoCTL instead"],
["is was", "\\b[Ii]s\\s+was\\b", "", "It is unusual to see 'is' and 'was' together. Maybe you only need one of these two words here?"],
# Has lots of false positives in titles:
["Capital without preceeding fullstop", "(?!(?:on|of|to|with)\\b)(?:$lowerword|ref[{][^{}]*[}]),?:?(?:(%.*\n)|\\s)+(?!$capword)([[:upper:]][[:alnum:]]*)", "erase:(?:\\\\(?:chapter|(?:sub)*section[*]?$recursive_brace)|$mathblock)", "ARG2~CAP, 1;ARG1, 2;ARG2"],
["Capital 'In' without preceeding fullstop", "[[:lower:]](?<!\\\\item)(?<!iffalse),?\\s+In\\b", "erase:(?:\\\\(?:chapter|(?:sub)*section[*]?[{](?:[^}]|[{][^}]*[}])*[}])|$mathblock)", '"In" can be the code for the element "Indium", in which case it should be captalised but usually it is "in" as an "inside" and should not be captalised unless it is the beginning of a sentance.'],
["In Practise", "[Ii]n practise", "", "In UK english \`practise' is verb and \`practice' is a noun. In US English \`practice' is used for both forms. Either way, I think you mean \`in practice'."],
["Empty Begin/End Block", "\\\\begin[{][^}]+[}]\\s*(?:\\\\par)?\\\\end[{][^}]*[}]", "Perhaps you mean *is* thus a?"],
["it thus a", "it\\s+thus\\s+a", "Perhaps you mean *is* thus a?"],
["Personal rule: in A", "in $start_math_char?A\\b", "Use \\allworlds instead"],
["Proof starting not on newline", '.+\\\\begin[{]proof[}]', "It seems that you don't have a paragraph break between the Lemma/Theorem and the proof \n (you can use the \"Enter\" key in LyX GUI to add one)"],
["No space between text and reference", '[[:alnum:]]\\\\(pretty)?ref[{]', "Perhaps you should add a non-breaking space \n('~' or Ctrl-Space in the LyX GUI)\n between the text and the reference?"],
["No space between text and reference", '^.(?:pretty)?ref[{]', "Perhaps you should add a non-breaking space \n('~' or Ctrl-Space in the LyX GUI)\n between the text and the reference?"],
[". .", '[.]\s+[.]', "Why do you have more than one '.'?"],
[". .", '[.][ ][.]', "Why do you have more than one '.'(2)?"],
["^.\\s+[[:upper:]]", "^.\\s+[[:upper:]]", "Why do you have more than one '.'(3)?"],
["^.\\s+[[:upper:]]", '[.][ ][.]', "Why do you have more than one '.'(3)?"],
#["it more", '\b[Ii]t\s+more', "Perhaps you meant 'in is more'"],
["in term of", '[Ii]n\s+term\s+of', "Perhaps you meant 'in terms of'"],
["From of", '[Ff]rom\s+of', "Perhaps you meant 'form of'"],
["Will is", '\b[Ww]ill\s+[[:alnum:]]*\s*\bis\b', "Use `Will' or `is', not both"],
["The a", '\b[Tt]he\s+a\b', "Use `The' or `A', not both"],
["Personal rule: forall_", 'forall_' ,"Use \\Forall instead"],
["Personal rule: missing -", '([Pp]air|[Ss]tate)\s+RoCTL' ,"add -"],
["Were [math]", "[Ww]ere\\s+$start_math" ,"Perhaps you meant Where ..."],
["Also and", "[Aa]lso\\s+and" ,"Perhaps you meant 'Also an'"],
#["Personal rule", '\batoms?\b' ,"I may mean variable"],
["Personal rule: found failure free in place of 'failure-free'", 'failure free' ,"use failure-free instead"],
["Personal rule: CTL structures", 'CTL struct' ,"use CTL-structures instead"],
["Personal rule: operator name without capital", 'obligatory\s+operator' ,"use capital letter"],
["Personal rule: operator name without capital", 'robustly\s+operator' ,"use capital letter"],
["with by", '[Ww]ith\s+by' ,"???"],
["with in", '\b[Ww]ith\s+in\b' ,"Perhaps you meant 'within'?"],
["Where as",'[Ww]here\s+as',"Perhaps you meant whereas?", ''],
["space between cite and punctuation","\\\\cite[{][^{}]*[}]\\s+[,.]","", ''],
["there exist phi","[Tt]here exist $start_math.(?:phi:psi)","", 'Perhaps you mean "there exists *a* ..."'],
["formula it","[Ff]ormula it\\b","", 'There appears to be something missing between "formula" and "it"'],
[".Capital",'[.][[:upper:]][^.]',"", 'Perhaps you should put a space between the . and the captial'],
[".word",'[.][[:lower:]]+[[:space:]]',"", 'Perhaps you should put a space between the . and the word'],
["Exists as s..",'[\bEe]xists\s+as\s+s',"", 'Perhaps you meant Exists *a* s...'],
['\\textquotedbl[{][}]','\\\\textquotedbl[{][}]',"", "You should probably use `` or '' instead of \\textquotedbl{} or ".'"'],
["Space between mathblock and punctuation",$mathblock.'\s+[,.]',"", 'Why is there a capital after the mathblock?'],
["Inapproriate capital after mathblock",'[[:alnum:]] '.$mathblock.' (?!'.$capword.')(?!Robustly)[[:upper:]]',"", 'Why is there a capital after the mathblock?'],
["Equals outside mathblock",	$mathblock.'\s*=\s*'.$mathblock,"", 'Perhaps you should put the equals inside the mathblocks'],
#["a mathblock",	'(\b[Aa]\s+'.$mathblock.'[^-])',"", 'You should perhaps put a noun between "a" and the mathblock ARG1'],
#["a mathblock",	'\b[Aa]\s+'.$mathblock,"", 'You should perhaps put a noun between "a" and the mathblock'],
#[". A",	'[.]\sA$',	"", ''],
["a that",	'\b[Aa]\s+[Tt]hat',	"", 'Perhaps you mean "that"'],
["the there",	'[Tt]he\s+[Tt]here',	"", 'Perhaps you mean "there"'],
["there where",	'[Tt]here\s+where',	"", 'Perhaps you mean "there were"'],
["with where",	'[Ww]ith\s+where',	"", 'Perhaps you mean "there were"'],
["[Tt]he proofs that converge",	'[Tt]he\s+proofs\s+that\s+converge',	"", 'Perhaps you mean "The proofs of convergence"'],
["[Aa]nd a presented",	'[Aa]nd\s+a\s+presented',	"", 'Perhaps you mean "And presented"'],
["Cause change the",	'[Cc]ause\s+change\s+the',	"", 'Perhaps you mean "Cause the"'], #This may cause change the winner
["In place voter",	'[Ii]n\s+place\s+voter',	"", 'Perhaps you mean "In place of voter"'],
["We allow only {to}? require",	'[Ww]e\s+allow\s+only\s+require',	"", 'Perhaps you mean "We only require"'],
["Programs can be computed",	'[Pp]rograms\s+can\s+be\s+computed',	"", 'Perhaps you mean "???"'],
["To be converge",	'[Tt]o\s+be\s+converge',	"", 'Perhaps you mean "???"'],
["Factions?",	'[Ff]actions',	"", 'In a mathematical text it is likely this was meant to be "fractions"'],
["hEIrarchy",	'[hH]eirar?chy',	"", 'Perhaps you mean "hIErarchy"'],
["exponential blow up",	'[Ee]xponential\s+blow\s+up',	"", 'Perhaps you mean "exponential blowup"'],
["be behave",	'[Bb]e\s+behave',	"", 'Perhaps you mean "behave"'],
["Anyway of",	'anyway\s+of',	"", 'Perhaps you mean "any way of"'],
["been replace[d]",	'\bbeen\s+replaces?\b',	"", 'Been suggests past tense, but replace suggests present tense. Perhaps you mean "been replaced"'],
["The [noun] may",	'\b[Tt]he\s+may\b',	"", 'There appears to be a noun missing between "The" and "may"'],
["It involve[s]",	'\b[Ii]t\s+involve\b',	"", 'Perhaps you mean "it involves"'],
["At leas[e] as",	'\b[Aa]t\s+lease\s+as\b',	"", 'Perhaps you mean "At leasT as"'],
["Use of An where A is expected",	'\b[Aa]n\s+('.$ConsonantSound.')',	"", 'An should only be used before words that start with a vowel sound (usually A,E,I,O or U), but ARG1 begins with a consonant sound'],
["Use of A where An is expected",	'\b[Aa]\s+('.$VowelSound.')',	"", 'A should only be used before words that do not start with a vowel sound (usually A,E,I,O or U), but ARG1 begins with vowel sound.'],
["Use of the and 's",	'\\b[Tt]he\\s+[[:upper:]][[:lower:]]*\'s',	"", "You should use the or 's, not both"],
["Sentance must begin with capital letter",	'(?<!\..)\.\s+[[:lower:]]',	"", ""],
["No space before math block",	"[[:alnum:]]$start_math(?![_^]|\\\\.dots)",	"", ""],
["No space before citation",	"[^[:space:](~]\\\\cite",	"", ""],
#["Number of (singular)",	"[Nn]umber\\s+of\\s+[[:alnum:]]+[^s\\s]\\b",	"", ""],
["Capital following comma",	",\\s*(?!b|$names|Coomb|Hare\\b|Marquis\\b||Khachian\\b|Dominating\\s+Set\\b\\b|Impartial\\s+Culture\\b)[[:upper:]][[:lower:]]",	'Remove capitals from beginning of ARG1', ""],
["Inappropriate capital",	'[^ 
.?:}](?<![.]\'\')(?<![.]\')(?<![.]")\\s+It',	'Remove capital', ""],
["Footnote is not fullstop",   "[[:alnum:],]\\s*\\\\footnote$recursive_brace\\\\s*[[:upper:]]",'',''],
["Footnote is not fullstop",   "[[:alnum:],](\\s|%
)*\\\\footnote$recursive_brace\\s*[[:upper:]]",'',''],
["No space after math block",	'(?!..sim)'.$mathblock.'(?<!dots.)(?!s\s)[[:alnum:]]',	"..sim", ""],
["No space before macro",	"[[:alnum:]]$macroblock",	"", ""],
["No space after macro",	$macroblock.'[[:alnum:]]',	"", ""],
["A used for plural",	'\\b[Aa]\\s+sequences\\b',	"", ""],
#["Comma following footnote",	'\\\\footnote[{][^}]*[}],',	"", ""],
["Use of : in math-mode",	$start_math_char.'[^'.$end_math_char.']*(?<![\\\\])[[:alpha:]]:',	"", "LaTeX assumes a : in mathmode means division, if you are trying to define a function, you should use \\colon instead."],
["less that",	'\\bless\\s+that\\b',	"", ""],
["Ugly fraction",		"([[:digit:]])/([[:digit:]])(?!n[}])(?!_home)",	"erase:\\\\url$recursive_brace", "Use \\nicefrac\{ARG1\}\{ARG2\} instead"],
["Too many zeros without a comma",		"(?<!.)0000(?![^\\s]*[.]tex[}])",	"", "You should put a comma in there somewhere"],
["Split word","(?:[Ww]ith out|[Ll]ike wise)","ARG1 is a single word", ""],
["Duplicated Words",	'(?i)\b(a|I|[[:alpha:]][[:alpha:]+*]+)\b[,.;]?\s+\b\2\b'.$notinmath,	'ARG1 occurs twice.', ""],
["Duplicated Words (broken)",	'(?i)\b(?![[:alpha:]]+\s+by|l |CTL and CTL[+*]|CTL, CTL[+*]|more\s+and|sets\s+of|that\s+|is\s+|may\s+or|does\s+or\s+does\s+not|neck\s+and\s+neck|as\s+well\s+as|(?:as|of|to)\s)([[:alpha:]]+)\s+(?!to)[[:alpha:]]+\s+\2\b(?![{-])',	'ARG1 occurs twice.', ""],
["Use of I.e.",	'I\\.e\\.',	'At the beginning of a sentance you should use "That is" rather than "I.e."',''],
#["Use of E.g.",	'E\\.g\\.',	'At the beginning of a sentance you should use "For example" rather than "E.g."',''],
["Use of \"we could\".",	'\\b([Ww]e\\s+c(?:an|ould))(?>!\\s+say)\\b', '',	'In formal text, we should not use ARG1 as it implies you cannot'],
["No space between sentances",	'[[:lower:]]\\.[[:upper:]](?![^\\s]*[.]tex[}])', '',	''],
#["Use of \"we could\".",	'\\b[Ww]e\\s+c(?:an|ould)\\b',	'In formal','XXX'],
["Missing hyphen after math block",	"$end_math\\s+(multinomial|Bernoulli|dimensional|binomial|probability)","", 'Add a hyphen before ARG1' ],
["Paragraph should end in fullstop","[[:alnum:]](?<!iffalse)(?<!maketitle)(?<!medskip)(?<!hline)(?<![[:upper:]]{3})(?>!\\\\else)(?<![\\\\]fi)[[:space:]]*\n\n",	"", ""],
["Paragraph should start with capital",$par."[[:lower:]]",	"", ""],
["Paragraph starts with fullstop?",$par."\\.",	"", ""],
["Place punctuation outside mathmode",".([,.:?])$end_math",	"", "Move ARG1 out of math mode"],
["Place . after *", 'CTL\.\*',"","I think you meant CTL*. rather than CTL.*"],
["Double dot", '\. \.',"","You probably only need one dot."],
#["Should exclude belief from technical papers", '[Bb]elieve',"","In a technical or scientific paper it is better to say 'it is not known better' unless the person who holds the belief is very well known."],
["Personal rule - M^{*} may look nicer", 'M\*',"",""],
#["Personal rule - RoCTL*", 'RoCTL(?!\\.p[sd]).[^*sS]',"",""],
["Remove space before punctuation","(?<!\\\\)\\b[[:alnum:]]+\\s+(?:\\\\@)?([,.:;?])(?!=)",	"", "Remove Space before ARG1 "],
["Remove space before punctuation[2]","(?<!\\\\)\\b[[:alnum:]]+\\s+(['])",	"", "Remove Space before ARG1... or perhaps you meant to use ` instead of '?"],
["Index entries should start with capital letters","\\\\index[{][[:lower:]]",	"", ""],
#["Place word between mathblocks","$end_math,\\s+$start_math",	"You should put a word like `and' between mathblocks rather than just a comma.", ""],
#["Move apstrophy after s because next word is plural","\'s\\s+\\w+s\\b",	"", ""],
["Perhaps you mean `is that'","\\bit\\s+that\\b",	"", ""],
["You should use Var()","\\\\[Vv]ari[[]|\\\\sigma[(]|\\\\sigma\\^[{]2[}][(]|\\\\cov[[]","\\bit\\s+that\\b",	"", "In statistics, you should use Var() for variance. Even though you use E[X] instead of E(X). Weird huh?"],
#["In `in sentance' math blocks you should say this in words instead of using symbols","$start_math_char\\\\(forall|exists)",	"", "You should replace the ARG1 symbol with words."],
["Amiguous Terminology",	"\\b(and\\s+but|a\\s+the\\b|the\\s+of|[Bb]inomial\\s+varia|are\\s+is\\b|will\\s+equal|that\\s+the\\s+all|an\\s+impartial\\s+culture|\\ba\\s+one\\s+of\\b)", "", 'ARG1 is a little odd.'],
["Use of pretty ref with out prefix",	"\\\\prettyref[{][^}:]*[}]", "", 'If you are using prettyref to cross reference a chapter the label should start with "cha:", if the label is fore a section the label should start with "sec:" etc.'],
#["Use of pretty ref with cor:",	"\\\\prettyref[{]cor:", "", 'Prettyref does not support cor:'],
['NOT IS is odd.',	'\bnot\s+is\b', "", 'Maybe you meant "is not"?'],
["Lemma label without 'lem:'",	"\\\\begin[{]lem[}]\\s+\\\\label[{](?!lem:)", "", 'If you start a Lemma label without lem, prettyref can get confused.'],
["Corollary label without 'cor:'",	"\\\\begin[{]cor[}]\\s+\\\\label[{](?!cor:)", "", 'If you start a Lemma label without lem, prettyref can get confused.'],
["Theorem label without 'thm:'",	"\\\\begin[{]thm[}]\\s+\\\\label[{](?!thm:)", "", 'If you start a Lemma label without lem, prettyref can get confused.'],
["Lack of Lemma prefix",	"\\b(?!Lemma|and)[^\\s~]+[~\\s]+\\\\ref[{]lem:","",""],
["Lack of Corollary prefix",	"\\b(?!Corollary)[^~\\s]+[~\\s]+\\\\ref[{]cor:","",""],
["Lack of Theorem prefix",	"\\b(?!Theorem|and)[^\\s~]+[~\\s]+\\\\ref[{]thm:","",""],
["Lack of Definition prefix",	"\\b(?!Definition|and)[^\\s~]+[~\\s]+\\\\ref[{]def:","",""],
["Use of lowercase reference",	"(lemma|theorem|table|figure|corollary)[~ ]\\\\ref\\b", "", 'The first letter of ARG1 should be capitalised to adhere to the LaTeX \\prettyref standard.'],
#["Use of bare reference",	"(?<!ble|ary|les|ure|mma|rem|and|ion|thm|..,|tep|.to)[~ ]\\\\ref\\b", "", 'A reference should be prefixed with one of "Table", "Figure", "Theorem", "Lemma" ...'],
["lemma/theorem occurs before prettyref",	"(emma|heorem|xamples|Example|orollary|efinition)\\s+\\\\prettyref\\b", "", 'It is safer to use a Formatted Reference (i.e. \\prettyref), rather than manually format references, as Formatted References will automatically change the reference if you change the object being referenced (e.g. from a lemma to a theorem).'],
#["Function displayed as variables","(?<!\\\\)log\\(","","Perhaps you meant ARG1 to be a function? If so you should write it as \\ARG1 instead."],
["Empty Footnote","\\\\footnote[{](\\s|\n)*[}]","","There is a footnote with nothing in it. Perhaps you should remove it"],
#["Attempt to Pluralize a name","(\\bSaari|Nanson|Condorcet|Borda|Fishburn|Laslier|Dodgson|Tideman)s\\b","","ARG1 is someone's name, so the plural (s) doesn't really make sense. Maybe you meant the possessive ('s) instead?"],
["Attempt to Pluralize a name","(\\b$names)s\\b","","ARG1 is someone's name, so the plural (s) doesn't really make sense. Maybe you meant the possessive ('s) instead?"],
["Unraised 'c' in 'Mc'",	"\\b(?<![/,])(?<!cite[{])Mc[[:upper:]]", "", "The 'c' in 'Mc' should be a raised character."],
["Section ending with '.'","\\\\(?:sub)*section[{].*[.][}](?:\n|\$)","","It is unusual to have a section that ends with '.'"],
["Forge(t) to",	'\bforge\s+to\b', "", 'Perhaps you meant "Forget to".'],
["May the",	'\b[Ww]e\s+may\s+the\b', "", 'This is odd.'],
#['$ there',	'is a.*'.$end_math.'\s+there\b', "", 'This is odd.'],
['$ discuss',	$end_math.' [Dd]iscuss', "", 'This is odd.'],
["He vs the", '\b[Hh]e (?:full)?path\b', 'Perhaps you meant "the".'],
["The we", '\b[Tt]he\s+we\b', 'Perhaps you meant "the".'],
#Should return:
#["Saace before \\label", '(?<!\\\\item) \\\\label', "", "You should delete spaces before labels to maintain correct page references"], #ChKTeX error 24 
["Use of however without comma", '[;.]\\s+[Hh]owever(?!,)', "", "Some suggest only using however to start a sentance, or afer a semi-colon, and that it should be followed with a comma."], 
["Use of Joining word", '[;.]\\s+(?:Hence|Also|Additionally|Likewise)(?!,)', "", "Does this continue on from previous sentance? If so probably add a comma"], 
["Is be", '\b[Ii]s\s+be\b', "", "use of Is and be together usually indicates trouble."], 
["It represent", '\b[Ii]t\s+represent\b', "", "Prehaps you meant \"It representS\"?"], 
["Space before punctuation", $end_math.'\s+(?:\\\\[@])?([;.,])', "", "There is a space before the end of the mathblock and 'ARG1'"], 
["Forces what?", $end_math.'[({]\\\\forces', "", "In modal logic there should be something other than a bracket before \\forces"], 
["Be is", '\b[Bb]s\s+is\b', "", "use of Is and be together usually indicates trouble."], 

# The following rule generates lots of false positives in titles
["Inappropriate Title Capital", '[[:alnum:]](?<!\\\\item)(?<![[:upper:]]{3})(?<!\\\\em)(?<!\\\\fi)\s+(A|An|The|For|And|Nor|But|Or|Yet|For|From|Of|On|To|With)\b\s+[[:alnum:]]'.$notinmath, "", "Used a capital for ARG1, but not first or last word in a title."], 
#["Empty section",	'\\\\(?:sub)*section\\{\\}', "", 'Empty section.'],
#["Elements of vectors are usually not a vectors","vctr[{][^{}1]*[}]_[{]?[[:digit:][:lower:]]","",""],
#["Personal rule -- add apostrophy before s","\\b(Dodgson|Simpson|Tideman)s[']?\\s+[rR]ule","","This is a personal rule I use, I should have disabled it before I published a new version. Sorry."],
#["Replace O with \\bigO","[^dg]O[(]","","This is a personal rule I use, I should have disabled it before I published a new version. Sorry."],
["Personal rule - I probably mean Spatial","pacial","",""],
["Personal rule - I probably through","(path|paths|sigma.|pi.)$s+though","",""],
["Personal rule - structure OR CTL-model","tructure CTL-model","",""],
#["Personal rule - I probably mean fullpath","\\bpath\\b(?! operator)(?!-quantif)(?! \\\\formulae)","",""],
["Personal rule - I probably mean abbreviation",'\\bshort[- ]?hand\\b',"",""],
["Personal rule - Replace log with ln","\\\\log\\b","","I use ln rather than log where possible."],
["We call ... is a", "\\bwe call .*\n?.* is a\\b", "", "Use 'we call' or 'is a', not both"], 
["A One", '\ba\s+one\b', "", "Sentances like 'if a one or fewer ...' are odd"], 
["Can be express", '\b[cC]an\s+be\s+express\b', "", "Maybe you mean 'can be expressed'?"], 
["This paper(')s", '\b[Tt]his\s+papers\b', "", "Maybe you mean \"This paper's\""], 
["The never", '\b[Tt]he\s+never\b', "", "Maybe you mean \"The [noun] never\""], 
["The at", '\b[Tt]he\s+at\b', "", "Maybe you mean \"The [noun] at\""], 
["Personal rule - moral", '\b[Mm]oral(?!\\s+agent)', "", "Use failure free instead"], 
["We expect", '[Ww]e\s+(?:also\s+)?expect', "", "'We expect' is too weak for a scientific paper. Frame this as a conjecture or use 'It is easy to show that'"], 
["Axiom[i->a]tisation", '\b[Aa]xiomiti[zs]ation\b', "", "Replace the first i with an a."], # LyX's (1.4) spellchecker had a bug where it missed the first misspelt word.
["Classic[al] Logic", '\b[Cc]lassic\s+[Ll]ogic\b', "", "Did you mean Classical Logic?"], 
["Space before )",$end_math."[^".$start_math."]*\\s\\)", "", ""],
["Converge [to] the",'onverge\s+the.', "", ""],
["use or used", '\b[Uu]se\s+[^.;]*is used\b', "", "You used both 'use' and 'is used' in the same sentance. This may indicate that you tried to do something silly like 'We use X is used to Y'."], 
["Lonely End Proof",$par.'[\\\\]end{proof}', "", "You should probably delete the paragraph break before the \\end{proof}"], 
["Paragraph following COLON",":\\s*$par\\s*[\\\\]begin{(?:eqnarray|align)", "", "You should probably delete the paragraph break after the COLON"], 
["Personal rule - remove the i","[Aa]utomation","",""],
["no full stop at end of definition","[[:alnum:]$end_math]".'\s*.end{definition}',"",""],
#["[^t][^o]$s+provide a", "[^t][^o]$s+provide$s+a\b","",""],
#["provide a", "[^o]$s+provide$s+a","",""],
#["provide a", "[^o]$s+provide$s+a","",""],
["no full stop at end of .*","[[:alnum:]$end_math]".'(?:\s|%[^\n]*)*.end{(?!algorithmic|enum|item|array|eqnarray|align)',"",""]
);



open(DEBUG_ERROR_TYPES, ">DEBUG_ERROR_TYPES.latexgc") or die "Can't open ".our $filename.".texp for writing: $!";
foreach (@ErrorTypes){
	print DEBUG_ERROR_TYPES $new_error_type.join(chr(0),@$_)."\n";
}
close(DEBUG_ERROR_TYPES);

Rassert('\b([[:alpha:]]+)\b\s+\b\1\b',' det det fab ','det');

return (@ErrorTypes);
}


