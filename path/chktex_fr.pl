#This is presently just chktex_en.pl with most of the obviously english only
#rules removed. It probably isn't much use for checking french yet.
#Also we may have to pass a special flag to LanguageTool.

# Format:
# SimpleRule("Error", "Corrected (optional)"),
#       OR
# ["Error Name:", "Error Regex", '', 'Extra Description'],

#A term definition e.g. du dans Corée du Sud
my $termdef="(?:(?:[[:upper:]][[:alnum:]]*|dans|un|une|le|la|et|pour|les|de|du|en)\\s+)+(?:\\\\cite[{][^}]*[}]\\s*)?[(]";
#A captialized word e.g Monsieur
my $capword="\\b(Monsieur|Madame|M.|Mme.|(Lun|Mar|Mecre|Jeu|Vendre)di|Dimanche)\\b";

my $s='(?:\n|\s)';
our @ErrorTypes=(
["Empty mathblock", "$start_math_char  *$end_math_char", '', ""],
["Macro sans {}.", "[^\\\\]\\\\[[:alnum:]]+CTL[[:alnum:]]+\\s", '', "Une {} est probablement nécessaire après la macro pour ne pas supprimer l'espace"],
["Le second mot de la phrase commence par une majuscule.", "[.]\\s+[[:alnum:]]+\\s+(?!$capword)[[:upper:]]", '', "Quand le second mot possède une majuscule, c'est généralement une erreur."],
["pas de point d'arrêt après une citation en fin de paragraphe", "[[:alnum:]]\\s+\\\\cite[{][^}]*[}]\\s*$par", '', "Je pense qu'un arrêt total manque ici."],
["Espace après citation", "\\\\cite[{][^}]*[}][[:alnum:]]", '', ""],
["Espace avant citation", "[[:alnum:]]\\\\cite[{][^}]*[}]", '', ""],
["Le soulignement commence trop tôt", "\\\\uline[{]\\s", '', ""],
["Le soulignement fini trop loin", "\\\\uline[{][^}]\\s[}]", '', ""],
["Espace avant note de bas de page", "(?: %\\s+| )\\\\footnote[{]", '', ""], # BTW footnotes are described at http://www.bristol.ac.uk/arts/exercises/referencing/referencing%20skills/page_07.htm
["Point/Virgule après note de bas de page", "\\\\footnote$recursive_brace".'[.,]', '', "Si la note de bas de page fait référence à la phrase complète, elle doit se situer après la phrase en question."], # BTW footnotes are described at http://www.bristol.ac.uk/arts/exercises/referencing/referencing%20skills/page_07.htm
["Ponctuation double", '(?<!\\\\)[,.:;]\\s+[,.:;]','',""],
[	"Utilisation d'une implication dans une preuve",
	'(?:(?:\\\\left)?[(]\\\\implies(?:\\\\right)?[)]|[(].\\\\implies.[)])',
	'',
	"L'utilisation de (\\implies) engendre une espace indésirable entre la fléche et les crochets. Pour indiquer la direction d'une preuve, il est préférable d'utiliser '(\\Longrightarrow)'"
],
["Pas d'espace après un référencement", 'ref[{][^}]*[}][[:alnum:]]', "", ""],
["Un seul caractère", '\s[b-z]\s(?![&\\\\])'."(?![^$start_math_char]*[_$end_math_char])", "", "Un caractère unique n'a, a priori, pas de sens ?"],
["Majuscule au milieu d'une phrase", "(?!(?:en|du|de|avec)\\b)(?:$lowerword|ref[{][^{}]*[}]),?:?(?:(%.*\n)|\\s)+(?!$capword)([[:upper:]][[:alnum:]]*)", "erase:(?:\\\\(?:chapter|(?:sub)*section[*]?$recursive_brace)|$mathblock)", "ARG2~CAP, 1;ARG1, 2;ARG2"],
["Block Begin/End vide", "\\\\begin[{][^}]+[}]\\s*(?:\\\\par)?\\\\end[{][^}]*[}]", ""],
["La preuve ne démarre pas sur une nouvelle ligne", '.+\\\\begin[{]proof[}]', "Il semble que le lemme/théorème et sa preuve soit sur la même ligne.\n(utilisez la touche \"Entrée\" dans LyX pour en ajouter une)"],
["Pas d'espace entre une référence et le texte à sa gauche", '[[:alnum:]]\\\\(pretty)?ref[{]', "Peut-être souhaitez-vous une espace insécable\n('~' ou Ctrl-Espace dans LyX)\n entre une référence et le texte ?"],
["Pas d'espace entre une référence et le texte à sa droite", '^.(?:pretty)?ref[{]', "Perhaps you should add a non-breaking space \n('~' or Ctrl-Space in the LyX GUI)\n between the text and the reference?"],
["Trop de points", '[.]\s+[.]', "Pourquoi avoir plusieurs points '.' ?"],
["Trop de points", '[.][ ][.]', "Pourquoi avoir plusieurs points '.' ?"],
["Trop de points", "^.\\s+[[:upper:]]", "Pourquoi avoir plusieurs points '.' ?"],
["Trop de points", '[.][ ][.]', "Pourquoi avoir plusieurs points '.' ?"],
["espace entre la citation et la ponctuation","\\\\cite[{][^{}]*[}]\\s+[,.]","", ''],
["Espace manquant entre point et majuscule",'[.][[:upper:]][^.]',"", 'Un espace est probablement manquant entre le . et la majuscule ?'],
["Espace manquant entre point et mot",'[.][[:lower:]]+[[:space:]]',"", 'Un espace est probablement manquant entre le . et le mot ?'],
['Mauvaise usage des guillements','\\\\textquotedbl[{][}]',"", "Il faut plutôt utiliser `` ou '' à la place de \\textquotedbl{} ou ".'"'],
["Espace entre block de math et ponctuation",$mathblock.'\s+[,.]',"", 'Pourquoi une majuscule est placé après un block de math ?'],
["Majuscule après block de math",'[[:alnum:]] '.$mathblock.' (?!'.$capword.')(?!Robustly)[[:upper:]]',"", 'Pourquoi se trouve une majuscule après un block de math?'],
["Signe égal '=' en dehors d'un block de math",	$mathblock.'\s*=\s*'.$mathblock,"", 'Le signe égal \'=\' devrait peut-être se trouver à l\'intérieur du block de math ?'],
["Pas d'espace avant un block de math",	"[[:alnum:]]$start_math(?![_^]|\\\\.dots)",	"", ""],
["Pas d'espace avant une citation",	"[^[:space:](~]\\\\cite",	"", ""],
["Majuscule après virgule",	",\\s*(?!b|$names|Coomb|Hare\\b|Marquis\\b||Khachian\\b|Dominating\\s+Set\\b\\b|Impartial\\s+Culture\\b)[[:upper:]][[:lower:]]", 'Supprimer la majuscule après la virgule ARG1', ""],
["Majuscule inattendue",	'[^.?:}](?<![.]\'\')(?<![.]\')(?<![.]")\\s',	'Supprimez la majuscule', ""],
["La note de bas de page manque un point",   "[[:alnum:],]\\s*\\\\footnote$recursive_brace\\\\s*[[:upper:]]",'',''],
["La note de bas de page manque un point",   "[[:alnum:],](\\s|%)*\\\\footnote$recursive_brace\\s*[[:upper:]]",'',''],
["Pas d'espace après block de math",	'(?!..sim)'.$mathblock.'(?<!dots.)(?!s\s)[[:alnum:]]',	"..sim", ""],
["Pas d'espace avant une macro",	"[[:alnum:]]$macroblock",	"", ""],
["Pas d'espace après une macro",	$macroblock.'[[:alnum:]]',	"", ""],
["Un(e) utilisé avec un pluriel",	'\\b[Un(|e)]\\s+sequences\\b',	"", ""],
["Utilisation de : dans le mode math",	$start_math_char.'[^'.$end_math_char.']*(?<![\\\\])[[:alpha:]]:',	"", "LaTeX pense qu'un ':' dans le mode math signifie une division, utilisez plutôt \\colon si vous souhaitez définir une fonction."],
["Fraction disgracieuse", "([[:digit:]])/([[:digit:]])(?!n[}])(?!_home)",	"erase:\\\\url$recursive_brace", "Utilisez \\nicefrac\{ARG1\}\{ARG2\} à la place."],
["Trop de zéros sans séparateur",		"(?<!.)0000(?![^\\s]*[.]tex[}])",	"", "Une virgule ou une espace fine est manquante"],
["Mot dupliqué",	'(?i)\b([[:alpha:]]+)\b[,.;]?\s+\b\2\b'.$notinmath,	'ARG1 apparaît deux fois.', ""],
["Un paragraphe doit se terminer par un point","[[:alnum:]](?<!iffalse)(?<!maketitle)(?<!medskip)(?<!hline)(?<![[:upper:]]{3})(?>!\\\\else)(?<![\\\\]fi)[[:space:]]*\n\n",	"", ""],
["Un paragraphe doit commencer par une majuscule",$par."[[:lower:]]",	"", ""],
["Paragraphe démarrant par un point ?",$par."\\.",	"", ""],
["Ponctuation à l'intérieur du mode math",".([,.:?])$end_math",	"", "Déplacez ARG1 en dehors du mode math"],
["Deux points", '\. \.',"","Un seul point suffit probablement."],
["Ajouter une espace avant ponctuation","(?<!\\\\)\\b[[:alnum:]]+(?:\\\\@)?([,.:;?])(?!=)",	"", "Ajouter une espace avant ARG1"],
["Supprimer une espace avant punctuation","(?<!\\\\)\\b[[:alnum:]]+\\s+(['])",	"", "Supprimez l'espace avant ARG1... ou peut-être souhaitez-vous «`» au lieu de «'» ?"],
["Les entrées de l'index commencent par une majuscule","\\\\index[{][[:lower:]]",	"", ""],
["Vous devriez utiliser Var()","\\\\[Vv]ari[[]|\\\\sigma[(]|\\\\sigma\\^[{]2[}][(]|\\\\cov[[]","\\bit\\s+that\\b",	"", "En statistiques, la variance s'utilise avec Var(). Même si vous utilisez E[X] au lieu de E(X). Étonnant hein ?"],
["Utilisation de prettyref sans préfixe",	"\\\\prettyref[{][^}:]*[}]", "", 'Si vous utilisez prettyref pour référencer un chapitre, le label doit commencer par "cha:", "sec:" pour une section, etc.'],
["Label «Lemme» sans «lem:»",	"\\\\begin[{]lem[}]\\s+\\\\label[{](?!lem:)", "", 'Si vous démarrez un label «Lemme» sans lem, prettyref peut-être confus.'],
["Label «Corollaire» sans «cor:»",	"\\\\begin[{]cor[}]\\s+\\\\label[{](?!cor:)", "", 'Si vous démarrez un label «Corollaire» sans cor, prettyref peut-être confus.'],
["Label «Théorème» sans «thm:»",	"\\\\begin[{]thm[}]\\s+\\\\label[{](?!thm:)", "", 'Si vous démarrez un label «Théorème sans thm, prettyref peut-être confus.'],
["Préfixe Lemme manquant",	"\\b(?!Lemme|and)[^\\s~]+[~\\s]+\\\\ref[{]lem:","",""],
["Préfixe Corollaire maquant",	"\\b(?!Corollaire)[^~\\s]+[~\\s]+\\\\ref[{]cor:","",""],
["Préfixe Théorème maquant",	"\\b(?!Théorème|et)[^\\s~]+[~\\s]+\\\\ref[{]thm:","",""],
["Référence en minuscule",	"(lemme|théorème|table|figure|corollaire)[~ ]\\\\ref\\b", "", 'La premiére lettre de ARG1 devrait être en majuscule en accord avec le standard \\prettyref LaTeX.'],
["lemme/théorème apparaît avant prettyref",	"(emme|héorèm|xemples|Exemple|orollaire|éfinition)\\s+\\\\prettyref\\b", "", '
Il vaut mieux utiliser une référence formatté (i.e. \\prettyref), plutôt qu\'un formattage manuel des références, parce qu\'une référence formattée va automatiquement changer la référence si l\'objet référencé est modifié (passage d\'un lemme à un théorème par exemple)."],
["Note de bas de page vide","\\\\footnote[{](\\s|\n)*[}]","","Peut-être devriez-vous la supprimer"],
["Section se terminant par un '.'","\\\\(?:sub)*section[{].*[.][}](?:\n|\$)","","Terminer une section avec un point n\'est pas commun"],
["Pas d\'espace avant ponctuation", $end_math.'+(?:\\\\[@])?([;.,])', "", "Il manque un espace avant la fin du block math et 'ARG1'"],
["Espace avant «)»",$end_math."[^".$start_math."]*\\s\\)", "", ""],
["Fin de preuve sans début",$par.'[\\\\]end{proof}', "", "Vous devriez probablement supprimer le saut de paragraphe avant \\end{proof}"],
["pas de point à la fin de la définition","[[:alnum:]$end_math]".'\s*.end{definition}',"",""],
["pas de point à la fin de .*","[[:alnum:]$end_math]".'(?:\s|%[^\n]*)*.end{(?!algorithmic|enum|item|array|eqnarray|align)',"",""]
);
