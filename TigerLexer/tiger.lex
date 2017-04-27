type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val stringContent = ref ""
val stringStartPos = ref 0
val commentNestingLevel = ref 0

fun err(p1,p2) = ErrorMsg.error p1
(*
fun fail p s = raise LexError(p, s)
*)

fun eof() = let
    val pos = hd(!linePos)
    val unclosedComments = !commentNestingLevel
    val unclosedString = !stringContent
    in
       if unclosedComments <> 0
       then ErrorMsg.error pos ("EOF reached without closing n=" ^ (Int.toString unclosedComments) ^ " comments")
       else if unclosedString <> ""
           then ErrorMsg.error pos  ("EOF reached without closing string=" ^ unclosedString ^ " ")
           else ();
       Tokens.EOF(pos,pos)
    end
%%

chars=[a-zA-Z]+;
letter=[a-zA-Z];
digit=[0-9];
digits={digit}+;
ascii={digit}{3};
escapechar=[nt\"\\]|{ascii};
newline=[\n\r]+;
whitespace=[\t\ \f]+;
formatchar={whitespace}|{newline};
quote=[\"];
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

%s COMMENT STRING ESCAPE FORMATSEQ;
%%

<INITIAL,COMMENT>{newline}                     => (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
<INITIAL,COMMENT>{whitespace}                  => (continue());

<INITIAL>"var"                                 => (Tokens.VAR(yypos,yypos+3));
<INITIAL>","                                   => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"type"                                => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"function"                            => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>"break"                               => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>of      => (Tokens.OF(yypos, yypos+2));
<INITIAL>end      => (Tokens.END(yypos, yypos+3));
<INITIAL>in       => (Tokens.IN(yypos, yypos+2));
<INITIAL>nil      => (Tokens.NIL(yypos, yypos+3));
<INITIAL>let      => (Tokens.LET(yypos, yypos+3));
<INITIAL>do       => (Tokens.DO(yypos, yypos+2));
<INITIAL>to       => (Tokens.TO(yypos, yypos+2));
<INITIAL>for      => (Tokens.FOR(yypos, yypos+3));
<INITIAL>while    => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>else     => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>then     => (Tokens.THEN(yypos, yypos+4));
<INITIAL>if       => (Tokens.IF(yypos, yypos+2));
<INITIAL>array    => (Tokens.ARRAY(yypos, yypos+5));

<INITIAL>":="       => (Tokens.ASSIGN(yypos, yypos+6));
<INITIAL>"|"        => (Tokens.OR(yypos, yypos+2));
<INITIAL>"&"        => (Tokens.AND(yypos, yypos+3));
<INITIAL>">="       => (Tokens.GE(yypos, yypos+2));
<INITIAL>">"        => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<"        => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<="       => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<>"       => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"="        => (Tokens.EQ(yypos, yypos+1));

<INITIAL>"\\"       => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"*"        => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"-"        => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"+"        => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"."        => (Tokens.DOT(yypos, yypos+1));

<INITIAL>"{"        => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}"        => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"["        => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]"        => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"("        => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")"        => (Tokens.RPAREN(yypos, yypos+1));

<INITIAL>";"        => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>":"        => (Tokens.COLON(yypos, yypos+1));
<INITIAL>","        => (Tokens.COMMA(yypos, yypos+1));

<INITIAL,COMMENT>"/*"       => (YYBEGIN COMMENT; commentNestingLevel := !commentNestingLevel+1; continue());
<COMMENT>.                  => (continue());
<COMMENT>"*/"               => (commentNestingLevel := !commentNestingLevel-1;
                                if !commentNestingLevel = 0
                                then (YYBEGIN INITIAL; continue())
                                else continue());

<INITIAL>{quote}       => (YYBEGIN STRING; stringStartPos := yypos; continue());
<STRING>"\\"           => (YYBEGIN ESCAPE;  continue());
<STRING>{quote}        => (YYBEGIN INITIAL; let val s = !stringContent
                           in
                           stringContent := "";
                             Tokens.STRING(s, !stringStartPos, yypos)
                           end );
<STRING>{eol}          => (YYBEGIN INITIAL; continue());
<STRING>.              => (stringContent := !stringContent ^ yytext; continue());


<ESCAPE>"\\"         => (stringContent := !stringContent ^ "\\"; YYBEGIN STRING; continue());
<ESCAPE>"n"          => (stringContent := !stringContent ^ "\n"; YYBEGIN STRING; continue());
<ESCAPE>"r"          => (stringContent := !stringContent ^ "\r"; YYBEGIN STRING; continue());
<ESCAPE>"t"          => (stringContent := !stringContent ^ "\t"; YYBEGIN STRING; continue());
<ESCAPE>"f"          => (stringContent := !stringContent ^ "\f"; YYBEGIN STRING; continue());
<ESCAPE>{ascii}      => (stringContent := !stringContent ^ String.str(Char.chr(valOf(Int.fromString yytext)));
                         YYBEGIN STRING; continue());
<ESCAPE>{formatchar}     => (YYBEGIN FORMATSEQ; continue());

<FORMATSEQ>{formatchar}  => (continue());
<FORMATSEQ>"\\"          => (YYBEGIN STRING; continue());
<FORMATSEQ>.             => (ErrorMsg.error yypos ("illegal format sequence " ^ yytext); continue());

<INITIAL>{letter}({letter}|{digit}|"_")*      => (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL>{digit}+                   => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
