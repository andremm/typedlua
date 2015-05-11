%%
%% This is file `ThesisPUC.cls'.
%%
%% Any modified versions of this file must be renamed
%% with new filenames distinct from ThesisPUC.cls.
%% 
%% ----------------------------------------------------------------------------
%%
%% Thomas Lewiner
%% Marcelo Roberto Jimenez
%%
%% LaTeX document class for typesetting theses of the PUC
%%
%% Copyright (C) 
%%
%% ----------------------------------------------------------------------------
%%
%%
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\newcommand{\docdate}{}  % See the renewcommands below,
\newcommand{\filedate}{} % these contain the content;-))
\newcommand{\fileversion}{}
\renewcommand{\docdate}{2002/07/10}
\renewcommand{\filedate}{2005/06/10}
\renewcommand{\fileversion}{3.0}
\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesClass{ThesisPUC}%
[\filedate\space v\fileversion\space PUC thesis class (portuguese)]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% style options
%

% booleans
\makeatletter
\RequirePackage{ifthen}

% hyphenation
\RequirePackage[brazil,british]{babel}

% PhD thesis or MsC dissertation
\newboolean{puc@phd}
\setboolean{puc@phd}{false}
\DeclareOption{msc}{\setboolean{puc@phd}{false}}
\DeclareOption{phd}{\setboolean{puc@phd}{true}}

% tight or wide page layout
\newboolean{puc@tight}
\setboolean{puc@tight}{false}
\DeclareOption{wide}{\setboolean{puc@tight}{false}}
\DeclareOption{tight}{\setboolean{puc@tight}{true}}

% hyphenation
\DeclareOption{american,english,french,german,brasil,brazil,portugues,portuguese}{%
   \PassOptionsToPackage{\CurrentOption}{babel}}

% twoside
\newboolean{puc@twoside}
\setboolean{puc@twoside}{false}
\DeclareOption{twoside}{\setboolean{puc@twoside}{true}}
\DeclareOption{oneside}{\setboolean{puc@twoside}{false}}

% draft for debugging overful boxes
\newboolean{puc@draft}
\setboolean{puc@draft}{false}
\DeclareOption{draft}{\setboolean{puc@draft}{true}}

% translations
\DeclareOption{dissertacao}{\setboolean{puc@phd}{false}}
\DeclareOption{mestrado}{\setboolean{puc@phd}{false}}
\DeclareOption{tese}{\setboolean{puc@phd}{true}}
\DeclareOption{doutorado}{\setboolean{puc@phd}{true}}
\DeclareOption{modelo1}{\setboolean{puc@tight}{false}}
\DeclareOption{modelo2}{\setboolean{puc@tight}{true}}
\DeclareOption{frenteverso}{\setboolean{puc@twoside}{true}}
\DeclareOption{rascunho}{\setboolean{puc@draft}{true}}

\ProcessOptions\relax


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% base style
%
\ifthenelse{\boolean{puc@twoside}}{%
  \LoadClass[a4paper,12pt\ifpuc@draft,draft\else\fi]{book}%
  \raggedbottom%
}{%
  \LoadClass[a4paper,12pt\ifpuc@draft,draft\else\fi]{report}%
}
\makeatother


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% required packages
%

% fonts and symbols
\RequirePackage{amsfonts}
\RequirePackage[latin1]{inputenc}

% graphics inclusion (in particular the PUC logo)
\RequirePackage[dvipdfm]{graphicx}

% subfigure loaded here for spacing definition
\RequirePackage[FIGBOTCAP,TABBOTCAP]{subfigure}

% Section titles
\RequirePackage[clearempty,compact]{titlesec}

% table of contents
\RequirePackage[subfigure]{tocloft}

% actively change margins
\RequirePackage{chngpage}

% line spacing
\RequirePackage{setspace}

% header and footer
\RequirePackage{fancyhdr}

% front matter inclusion and space in environment
\RequirePackage{atbeginend}

% indent first line of sections and chapters
\RequirePackage{indentfirst}

% number of pages
\RequirePackage{lastpage}

% hyperlink and pdf bookmarks
\RequirePackage[backref,dvipdfm,colorlinks=false,bookmarksopen=true,breaklinks=true]{hyperref}
%linkcolor=blue,anchorcolor=black,citecolor=cyan,urlcolor=red,pagecolor=black,bookmarksnumbered=false,

\selectlanguage{brazil}
\makeatletter


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% page layout
%
\setlength{\parindent}{1cm}
\setlength{\voffset}{-1in}
\setlength{\topmargin}{20pt}
\setlength{\topskip}{0.001cm}
\addtolength{\headheight}{2.5pt}

\newcommand{\puc@setallmargins}[5]{%
 % declare length
 \expandafter\newlength\csname puc@topmargin@#1\endcsname%
 \expandafter\newlength\csname puc@bottommargin@#1\endcsname%
 \expandafter\newlength\csname puc@leftmargin@#1\endcsname%
 \expandafter\newlength\csname puc@rightmargin@#1\endcsname%
 \expandafter\newlength\csname puc@textwidth@#1\endcsname%
 \expandafter\newlength\csname puc@oddsidemargin@#1\endcsname%
 \expandafter\newlength\csname puc@evensidemargin@#1\endcsname%
 \expandafter\newlength\csname puc@textheight@#1\endcsname%
 \expandafter\newlength\csname puc@headsep@#1\endcsname%
%
 % adjust margins
 \expandafter\setlength{\csname puc@topmargin@#1\endcsname}{#2}%
 \expandafter\setlength{\csname puc@bottommargin@#1\endcsname}{#3}%
 \expandafter\setlength{\csname puc@leftmargin@#1\endcsname}{#4}%
 \expandafter\setlength{\csname puc@rightmargin@#1\endcsname}{#5}%
%
 % converts margins to text size
 \expandafter\setlength{\csname puc@textwidth@#1\endcsname}{\paperwidth}%
 \expandafter\addtolength{\csname puc@textwidth@#1\endcsname}{-\csname puc@leftmargin@#1\endcsname}%
 \expandafter\addtolength{\csname puc@textwidth@#1\endcsname}{-\csname puc@rightmargin@#1\endcsname}%
%
 \expandafter\setlength{\csname puc@oddsidemargin@#1\endcsname}{\csname puc@leftmargin@#1\endcsname}%
 \expandafter\addtolength{\csname puc@oddsidemargin@#1\endcsname}{-1in}%
 \expandafter\addtolength{\csname puc@oddsidemargin@#1\endcsname}{-\hoffset}%
%
 \ifthenelse{\boolean{puc@twoside}}{%
   \expandafter\setlength{\csname puc@evensidemargin@#1\endcsname}{\csname puc@rightmargin@#1\endcsname}%
 }{%
   \expandafter\setlength{\csname puc@evensidemargin@#1\endcsname}{\csname puc@leftmargin@#1\endcsname}%
 }%
 \expandafter\addtolength{\csname puc@evensidemargin@#1\endcsname}{-1in}%
 \expandafter\addtolength{\csname puc@evensidemargin@#1\endcsname}{-\hoffset}%
%
 \expandafter\setlength{\csname puc@headsep@#1\endcsname}{\csname puc@topmargin@#1\endcsname}%
 \expandafter\addtolength{\csname puc@headsep@#1\endcsname}{-1in}%
 \expandafter\addtolength{\csname puc@headsep@#1\endcsname}{-\voffset}%
 \expandafter\addtolength{\csname puc@headsep@#1\endcsname}{-\topmargin}%
 \expandafter\addtolength{\csname puc@headsep@#1\endcsname}{-\headheight}%
%
 \expandafter\setlength{\csname puc@textheight@#1\endcsname}{\paperheight}%
 \expandafter\addtolength{\csname puc@textheight@#1\endcsname}{-\csname puc@topmargin@#1\endcsname}%
 \expandafter\addtolength{\csname puc@textheight@#1\endcsname}{-\csname puc@bottommargin@#1\endcsname}%
%
 % dynamic margin adjustment
 \expandafter\newcommand\csname puc@setmargins@#1\endcsname{%
   \setpage%
     {\csname puc@textheight@#1\endcsname}%
     {\csname puc@textwidth@#1\endcsname}%
     {\csname puc@evensidemargin@#1\endcsname}%
     {\csname puc@oddsidemargin@#1\endcsname}%
     {}{}{}%
     {\csname puc@headsep@#1\endcsname}%
     {}%
  }%
}


% text
\ifthenelse{\boolean{puc@tight}}{%
 \puc@setallmargins{text}{2.5cm}{2.5cm}{3.5cm}{7.0cm}%
}{%
 \puc@setallmargins{text}{2.5cm}{2.5cm}{3.0cm}{4.0cm}%
}

% cover
\puc@setallmargins{cover}{7.0cm}{3.0cm}{7.0cm}{2.0cm}

% front
\puc@setallmargins{front}{2.5cm}{2.5cm}{6.0cm}{4.0cm}

% dedication
\puc@setallmargins{dedication}{2.5cm}{2.5cm}{3.0cm}{4.0cm}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% To debug the layout
%
%\newcommand{\puc@leftmargin}[1]{\noindent\fbox{\begin{minipage}[top]{\textwidth}#1\end{minipage}}}
%\newcommand{\puc@rightmargin}[1]{\noindent\fbox{\begin{minipage}[top]{\textwidth}\begin{flushright}#1\end{flushright}\end{minipage}}}

\newcommand{\puc@leftmargin}[1]{\noindent\begin{minipage}[top]{\textwidth}#1\end{minipage}}
\newcommand{\puc@rightmargin}[1]{\puc@leftmargin{\begin{flushright}#1\end{flushright}}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% line spacing
%
\ifthenelse{\boolean{puc@tight}}{}{\onehalfspacing}
\frenchspacing
\clubpenalty=2000
\emergencystretch=20pt
\pretolerance=500
\tolerance=2500
\widowpenalty=2000


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% number of floats per page
%
\renewcommand{\floatpagefraction}{.7}
\renewcommand{\topfraction}{.7}
\renewcommand{\bottomfraction}{.7}
\renewcommand{\textfraction}{.3}
\setcounter{totalnumber}{10}
\setcounter{topnumber}{10}
\setcounter{bottomnumber}{10}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% subfigures
%
\renewcommand{\thesubfigure}{\thefigure (\alph{subfigure})}
\renewcommand{\@thesubfigure}{\thesubfigure:\space}
\renewcommand{\p@subfigure}{}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Fonts
%
\newcommand{\arial}{\sf}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Section titles
%
\newcommand{\noaccent}[1]{%
 \begingroup%
  \inputencoding{noaccent}%
  \renewcommand{\`}[1]{#1}%
  \renewcommand{\'}[1]{#1}%
  \renewcommand{\^}[1]{#1}%
  \renewcommand{\~}[1]{#1}%
  \renewcommand{\=}[1]{#1}%
  \renewcommand{\.}[1]{#1}%
  \renewcommand{\"}[1]{#1}%
  \renewcommand{\c}[1]{#1}%
  \renewcommand{\u}[1]{#1}%
  \renewcommand{\v}[1]{#1}%
  \renewcommand{\H}[1]{#1}%
  \renewcommand{\c}[1]{#1}%
  \renewcommand{\d}[1]{#1}%
  \renewcommand{\b}[1]{#1}%
  \renewcommand{\t}[1]{#1}%
  \renewcommand{\aa}{a}%
  \renewcommand{\AA}{A}%
  \renewcommand{\o}{o}%
  \renewcommand{\O}{o}%
  \renewcommand{\l}{l}%
  \renewcommand{\L}{L}%
  \renewcommand{\i}{i}%
  \renewcommand{\j}{j}%
  \renewcommand{\oe}{oe}%
  \renewcommand{\OE}{OE}%
  \renewcommand{\ae}{ae}%
  \renewcommand{\AE}{AE}%
  \renewcommand{\mathrm}[1]{#1}%
  \renewcommand{\mathit}[1]{#1}%
  \renewcommand{\mathnormal}[1]{#1}%
  \renewcommand{\mathcal}[1]{#1}%
  \renewcommand{\mathfrak}[1]{#1}%
  \renewcommand{\mathbb}[1]{#1}%
  \renewcommand{\textrm}[1]{#1}%
  \renewcommand{\textsf}[1]{#1}%
  \renewcommand{\texttt}[1]{#1}%
  \renewcommand{\textmd}[1]{#1}%
  \renewcommand{\textbf}[1]{#1}%
  \renewcommand{\textup}[1]{#1}%
  \renewcommand{\textit}[1]{#1}%
  \renewcommand{\textsl}[1]{#1}%
  \renewcommand{\textsc}[1]{#1}%
  \renewcommand{\emph}[1]{#1}%
  \renewcommand{\textnormal}[1]{#1}%
  \renewcommand{\rm}{}%
  \renewcommand{\sf}{}%
  \renewcommand{\tt}{}%
  \renewcommand{\bf}{}%
  \renewcommand{\it}{}%
  \renewcommand{\sl}{}%
  \renewcommand{\sc}{}%
  \renewcommand{\bfseries}{}%
  \renewcommand{\itshape}{}%
  \renewcommand{\mdseries}{}%
  \renewcommand{\rmfamily}{}%
  \renewcommand{\scshape}{}%
  \renewcommand{\sffamily}{}%
  \renewcommand{\slshape}{}%
  \renewcommand{\ttfamily}{}%
  \renewcommand{\upshape}{}%
  \renewcommand{\Large}[1]{#1}%
  \renewcommand{\tiny}[1]{#1}%
  \renewcommand{\scriptsize}[1]{#1}%
  \renewcommand{\LARGE}[1]{#1}%
  \renewcommand{\footnotesize}[1]{#1}%
  \renewcommand{\small}[1]{#1}%
  \renewcommand{\huge}[1]{#1}%
  \renewcommand{\normalsize}[1]{#1}%
  \renewcommand{\large}[1]{#1}%
  \renewcommand{\Huge}[1]{#1}%
  \renewcommand{\\}{}%
  \renewcommand{\$}{}%
  \renewcommand{\_}{}%
  \renewcommand{\backslash}{}%
  #1%
 \endgroup%
}
\newcommand{\autolabel}[1]{\noaccent{\label{sec:#1}}#1}

\titleformat{name=\chapter}
   [display]
   {\singlespacing\large\arial\bfseries}
   {\thechapter}
   {0pt}
   {\autolabel}
\titleformat{name=\section}
   [display]
   {\penalty -5000\singlespacing\normalsize\arial\bfseries}
   {\thesection}
   {0pt}
   {\autolabel}
\titleformat{name=\section,numberless}
   [display]
   {\penalty -5000\singlespacing\normalsize\arial\bfseries}
   {}
   {0pt}
   {}
\titleformat{name=\subsection}
   [display]
   {\penalty -4000\singlespacing\normalsize\arial\bfseries}
   {\thesubsection}
   {0pt}
   {\autolabel}
\titleformat{name=\subsection,numberless}
   [display]
   {\penalty -4000\singlespacing\normalsize\arial\bfseries}
   {}
   {0pt}
   {}
\titleformat{name=\subsubsection}
   [display]
   {\penalty -3000\singlespacing\normalsize\arial\bfseries}
   {\thesubsubsection}
   {0pt}
   {\autolabel}
\titleformat{name=\paragraph}
   [runin]
   {\singlespacing\normalsize\arial\bfseries}
   {2\wordsep}
   {}
   {}
\titleformat{name=\subparagraph}
   [runin]
   {\singlespacing\normalsize\arial\bfseries}
   {2\wordsep}
   {}
   {}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% page styles
%
\makeatletter%
\newcommand{\puc@setpagestyle}{%
 \pagestyle{fancy}%
 \fancyhf{}% delete current setting for header and footer
 \fancyfoot{}%
  \ifthenelse{\boolean{puc@twoside}}{%
   \fancyhead[LE]{\arial\footnotesize \nouppercase{\puc@title}}%
   \fancyhead[RE]{\arial\footnotesize \thepage}%
   \fancyhead[RO]{\arial\footnotesize \thepage}%
   \fancyhead[LO]{\arial\footnotesize \nouppercase{\leftmark}}%
  }{
   \fancyhead[L]{\arial\slshape \nouppercase{\leftmark}}%
   \fancyhead[R]{\thepage}%
  }
 \renewcommand{\headrulewidth}{0pt}%
}
\fancypagestyle{empty}{\fancyhead{} \fancyfoot{} \renewcommand{\headrulewidth}{0pt}}%
\fancypagestyle{plain}{\fancyhead{} \fancyfoot{} \renewcommand{\headrulewidth}{0pt}}%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% table of contents
%
\setcounter{tocdepth}{1}
\setcounter{lofdepth}{1}
\setcounter{lotdepth}{1}
\renewcommand{\cfttoctitlefont}{\large \arial \bfseries}
\setlength{\cftbeforetoctitleskip}{0pt}
\renewcommand{\cftloftitlefont}{\large \arial \bfseries}
\setlength{\cftbeforeloftitleskip}{0pt}
\renewcommand{\cftlottitlefont}{\large \arial \bfseries}
\setlength{\cftbeforelottitleskip}{0pt}
\renewcommand{\cftdot}{}
\renewcommand{\cftsecfont}{}
\setlength{\cftbeforesecskip}{\parskip}
\setlength{\cftbeforesubsecskip}{0pt}
\setlength{\cftchapnumwidth}{1.9em}
\setlength{\cftsecnumwidth}{2.5em}
\setlength{\cftsubsecnumwidth}{3.1em}
\renewcommand{\cftchapindent}{0pt}
\renewcommand{\cftsecindent}{0pt}
\renewcommand{\cftsubsecindent}{0pt}
\renewcommand{\cftchapfont}{\arial}
\renewcommand{\cftsecfont}{\arial}
\renewcommand{\cftsubsecfont}{\arial}
\renewcommand{\cftfigfont}{\arial}
\renewcommand{\cfttabfont}{\arial}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% citations
%
\def\@cite#1#2{({#1\if@tempswa , #2\fi})}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% theorems environments
%
\renewcommand{\theequation}{\thechapter-\arabic{equation}}
\newtheorem{definition}{Defini\c{c}\~{a}o}[chapter]
\newtheorem{theorem}[definition]{Teorema}
\newtheorem{proposition}[definition]{Proposi\c{c}\~{a}o}
\newtheorem{corollary}[definition]{Corol\'{a}rio}
\newenvironment{proof}{\par \noindent \textit{Prova}.}{\hfill $\blacksquare$}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% space in environment
%
\AfterBegin{itemize}{\addtolength{\itemsep}{-0.5em}}
\renewcommand{\labelitemi}{--}
\BeforeBegin{equation}{\vspace{-0.5em}}
\BeforeEnd{equation}{\vspace{-0.5em}}
\BeforeBegin{theorem}{\vspace{-0.4em}}
\BeforeEnd{theorem}{\vspace{-0.2em}}
\BeforeBegin{definition}{\vspace{-0.4em}}
\BeforeEnd{definition}{\vspace{-0.2em}}
\BeforeBegin{corollary}{\vspace{-0.4em}}
\BeforeEnd{corollary}{\vspace{-0.2em}}
\BeforeBegin{proposition}{\vspace{-0.4em}}
\BeforeEnd{proposition}{\vspace{-0.2em}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Front matter elements
%
\makeatletter

\newcommand{\puc@author}{}
\renewcommand{\author}[1]{\renewcommand{\puc@author}{#1}}

\newcommand{\puc@authorR}{}
\newcommand{\authorR}[1]{\renewcommand{\puc@authorR}{#1}}

\newcommand{\puc@adviser}{}
\newcommand{\adviser}[1]{\renewcommand{\puc@adviser}{#1}}

\newcommand{\puc@adviserR}{}
\newcommand{\adviserR}[1]{\renewcommand{\puc@adviserR}{#1}}

\newcommand{\puc@adviserInst}{}
\newcommand{\adviserInst}[1]{\renewcommand{\puc@adviserInst}{#1}}

\newcommand{\puc@coadviser}{}
\newcommand{\coadviser}[1]{\renewcommand{\puc@coadviser}{#1}}

\newcommand{\puc@coadviserR}{}
\newcommand{\coadviserR}[1]{\renewcommand{\puc@coadviserR}{#1}}

\newcommand{\puc@coadviserInst}{}
\newcommand{\coadviserInst}[1]{\renewcommand{\puc@coadviserInst}{#1}}

\newcommand{\puc@coadviserII}{}
\newcommand{\coadviserII}[1]{\renewcommand{\puc@coadviserII}{#1}}

\newcommand{\puc@coadviserIIR}{}
\newcommand{\coadviserIIR}[1]{\renewcommand{\puc@coadviserIIR}{#1}}

\newcommand{\puc@coadviserIIInst}{}
\newcommand{\coadviserIIInst}[1]{\renewcommand{\puc@coadviserIIInst}{#1}}

\newcommand{\puc@coadviserIII}{}
\newcommand{\coadviserIII}[1]{\renewcommand{\puc@coadviserIII}{#1}}

\newcommand{\puc@coadviserIIIR}{}
\newcommand{\coadviserIIIR}[1]{\renewcommand{\puc@coadviserIIIR}{#1}}

\newcommand{\puc@coadviserIIIInst}{}
\newcommand{\coadviserIIIInst}[1]{\renewcommand{\puc@coadviserIIIInst}{#1}}

\newcommand{\puc@title}{}
\renewcommand{\title}[1]{\renewcommand{\puc@title}{#1}}

\newcommand{\puc@titleuk}{}
\newcommand{\titleuk}[1]{\renewcommand{\puc@titleuk}{#1}}

\newcommand{\puc@subtitle}{}
\newcommand{\subtitle}[1]{\renewcommand{\puc@subtitle}{#1}}

\newcommand{\puc@day}{}
\renewcommand{\day}[1]{\renewcommand{\puc@day}{#1}}

\newcommand{\puc@month}{}
\renewcommand{\month}[1]{\renewcommand{\puc@month}{#1}}

\newcommand{\puc@year}{}
\renewcommand{\year}[1]{\renewcommand{\puc@year}{#1}}

\newcommand{\puc@city}{Rio de Janeiro}
\newcommand{\city}[1]{\renewcommand{\puc@city}{#1}}

\newcommand{\puc@department}{Matem\'{a}tica}
\newcommand{\department}[1]{\renewcommand{\puc@department}{#1}}

\newcommand{\puc@program}{Matem\'{a}tica Aplicada}
\newcommand{\program}[1]{\renewcommand{\puc@program}{#1}}

\newcommand{\puc@school}{Centro T\'{e}cnico Cient\'{i}fico}
\newcommand{\school}[1]{\renewcommand{\puc@school}{#1}}

\newcommand{\puc@university}{Pontif\'{i}cia Universidade Cat\'{o}lica do Rio de Janeiro}
\newcommand{\university}[1]{\renewcommand{\puc@university}{#1}}

\newcommand{\puc@uni}{PUC--Rio}
\newcommand{\uni}[1]{\renewcommand{\puc@uni}{#1}}

\newcommand{\puc@CDD}{510}
\newcommand{\CDD}[1]{\renewcommand{\puc@CDD}{#1}}

\newcommand{\jurymember}[2]{}
\newcommand{\schoolhead}[1]{\jurymember{#1}{Coordenador Setorial do \puc@school\ --- \puc@uni}}
\newcommand{\puc@jury}{}
\newcommand{\jury}[1]{\renewcommand{\puc@jury}{#1}}

\newcommand{\puc@resume}{}
\newcommand{\resume}[1]{\renewcommand{\puc@resume}{#1}}

\newboolean{hasdedication}
\setboolean{hasdedication}{false}
\newcommand{\puc@dedication}{}
\newcommand{\dedication}[1]{%
  \renewcommand{\puc@dedication}{#1}%
  \setboolean{hasdedication}{true}%
}

\newcommand{\puc@acknowledgment}{}
\newcommand{\acknowledgment}[1]{\renewcommand{\puc@acknowledgment}{#1}}

\newcommand{\key}[1]{}
\newcommand{\puc@keywords}{}
\newcommand{\keywords}[1]{\renewcommand{\puc@keywords}{#1}}

\newcommand{\puc@keywordsuk}{}
\newcommand{\keywordsuk}[1]{\renewcommand{\puc@keywordsuk}{#1}}

\newcommand{\puc@abstract}{}
\ifthenelse{\boolean{puc@twoside}}{%
  \newcommand{\abstract}[1]{\renewcommand{\puc@abstract}{#1}}%
}{%
  \renewcommand{\abstract}[1]{\renewcommand{\puc@abstract}{#1}}%
}

\newcommand{\puc@abstractuk}{}
\newcommand{\abstractuk}[1]{\renewcommand{\puc@abstractuk}{#1}}%

\newcommand{\puc@tablesmode}{figtab}
\newcommand{\tablesmode}[1]{\renewcommand{\puc@tablesmode}{#1}}

\newboolean{hasepigraph}
\setboolean{hasepigraph}{false}
\newcommand{\puc@epigraph}{}
\newcommand{\epigraph}[1]{%
  \renewcommand{\puc@epigraph}{#1}%
  \setboolean{hasepigraph}{true}%
}

\newcommand{\puc@epigraphauthor}{}
\newcommand{\epigraphauthor}[1]{\renewcommand{\puc@epigraphauthor}{#1}}

\newcommand{\puc@epigraphbook}{}
\newcommand{\epigraphbook}[1]{\renewcommand{\puc@epigraphbook}{#1}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% translation
%
\newcommand{\autor}[1]{\author{#1}}
\newcommand{\autorR}[1]{\authorR{#1}}
\newcommand{\orientador}[1]{\adviser{#1}}
\newcommand{\orientadorR}[1]{\adviserR{#1}}
\newcommand{\orientadorInst}[1]{\adviserInst{#1}}
\newcommand{\coorientador}[1]{\coadviser{#1}}
\newcommand{\coorientadorR}[1]{\coadviserR{#1}}
\newcommand{\coorientadorInst}[1]{\coadviserInst{#1}}
\newcommand{\coorientadorII}[1]{\coadviserII{#1}}
\newcommand{\coorientadorIIR}[1]{\coadviserIIR{#1}}
\newcommand{\coorientadorIIInst}[1]{\coadviserIIInst{#1}}
\newcommand{\coorientadorIII}[1]{\coadviserIII{#1}}
\newcommand{\coorientadorIIIR}[1]{\coadviserIIIR{#1}}
\newcommand{\coorientadorIIIInst}[1]{\coadviserIIIInst{#1}}
\newcommand{\titulo}[1]{\title{#1}}
\newcommand{\titulouk}[1]{\titleuk{#1}}
\newcommand{\subtitulo}[1]{\subtitle{#1}}
\newcommand{\dia}[1]{\day{#1}}
\newcommand{\mes}[1]{\month{#1}}
\newcommand{\ano}[1]{\year{#1}}
\newcommand{\cidade}[1]{\city{#1}}
\newcommand{\departamento}[1]{\department{#1}}
\newcommand{\programa}[1]{\program{#1}}
\newcommand{\centro}[1]{\school{#1}}
\newcommand{\universidade}[1]{\university{#1}}
%\newcommand{\uni}[1]{\uni{#1}}
%\newcommand{\CDD}[1]{\CDD{#1}}
\newcommand{\membrodabanca}[2]{}
\newcommand{\coordenador}[1]{\schoolhead{#1}}
\newcommand{\banca}[1]{\jury{#1}}
\newcommand{\curriculo}[1]{\resume{#1}}
\newcommand{\dedicatoria}[1]{\dedication{#1}}
\newcommand{\agradecimentos}[1]{\acknowledgment{#1}}
\newcommand{\chave}[1]{\key{#1}}
\newcommand{\chaves}[1]{\keywords{#1}}
\newcommand{\resumo}[1]{\abstract{#1}}
\newcommand{\chavesuk}[1]{\keywordsuk{#1}}
\newcommand{\resumouk}[1]{\abstractuk{#1}}
\newcommand{\modotabelas}[1]{\tablesmode{#1}}
\newcommand{\epigrafe}[1]{\epigraph{#1}}
\newcommand{\epigrafeautor}[1]{\epigraphauthor{#1}}
\newcommand{\epigrafelivro}[1]{\epigraphbook{#1}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cover
%
\newcommand{\puc@showcover}{%
  \puc@setmargins@cover%
  \thispagestyle{empty}%
  \singlespacing\arial%
  \begin{titlepage}%
    \setcounter{page}{0}%
    \renewcommand{\baselinestretch}{1.2}%
    \puc@rightmargin{\LARGE \arial \textbf{\puc@author} \normalsize}%
    \vspace*{\stretch{4}}%
    \par%
    \puc@rightmargin{\LARGE \arial \textbf{\puc@title\relax} \normalsize}%
    \ifthenelse{\equal{\puc@subtitle}{}}{}{%
      \vspace*{\stretch{0.5}}%
      \par%
      \puc@rightmargin{\LARGE \arial \puc@subtitle\relax \normalsize} }%
    \vspace*{\stretch{5}}%
    \par%
    \puc@rightmargin{\Large \arial \textbf{\ifthenelse{\boolean{puc@phd}}{TESE DE DOUTORADO}{DISSERTA\c C\~AO DE MESTRADO}}}%
    \vspace*{\stretch{1.5}}%
    \par%
    \puc@rightmargin{\textbf{DEPARTAMENTO DE \MakeUppercase{\puc@department}}}%
    \vspace*{\stretch{0.5}}%
    \par%
    \renewcommand{\baselinestretch}{1.0}%
    \puc@rightmargin{\Large \arial Programa de P\'os--gradua\c c\~ao em \puc@program}%
    \vspace*{\stretch{9}}%
    \par%
    \puc@rightmargin{\normalsize \arial \puc@city \\ \puc@month\ de \puc@year}%
  \end{titlepage}%
  \setcounter{page}{1}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% front page
%
\newcommand{\puc@showfront}{%
%  \cleardoublepage%
  \puc@setmargins@front%
  \thispagestyle{empty}%
  \pdfbookmark[0]{\puc@title}{anchor:title}%
  \singlespacing\arial%
  \puc@rightmargin{\includegraphics{puc.ps}}%
  \vspace*{\stretch{1}}%
  \par%
  \puc@rightmargin{\large \arial \textbf{\puc@author} \normalsize}%
  \vspace*{\stretch{4}}%
  \par%
  \puc@rightmargin{\large \arial \textbf{\puc@title\relax} \normalsize}%
  \ifthenelse{\equal{\puc@subtitle}{}}{}{%
    \vspace*{\stretch{0.5}}%
    \par%
    \puc@rightmargin{\arial \textbf{\puc@subtitle} \relax} }%
  \vspace*{\stretch{5}}%
  \par%
  \puc@rightmargin{\arial \textbf{\ifthenelse{\boolean{puc@phd}}{Tese de Doutorado}{Disserta\c c\~ao de Mestrado}}}%
  \vspace*{\stretch{1.5}}%
  \par%
  \puc@leftmargin{\arial%
    \ifthenelse{\boolean{puc@phd}}%
      {Tese apresentada ao Programa de Pós--graduação em \puc@program\ do Departamento de \puc@department\ da \puc@uni\ como requisito parcial para obtenção Do título de Doutor em \puc@program}%
      {Dissertação apresentada como requisito parcial para obtenção do grau de Mestre pelo Programa de Pós--graduação em \puc@program\ do Departamento de \puc@department\ da \puc@uni}%
  }%
  \vspace*{\stretch{1}}%
  \par%
  \puc@rightmargin{\begin{tabular}{l@{: }r}Orientador & Prof. \puc@adviser%
  \ifthenelse{\equal{\puc@coadviser}{}}{}{%
    \\ Co--Orientador & Prof. \puc@coadviser}%
  \ifthenelse{\equal{\puc@coadviserII}{}}{}{%
    \\ Co--Orientador & Prof. \puc@coadviserII}%
  \ifthenelse{\equal{\puc@coadviserIII}{}}{}{%
    \\ Co--Orientador & Prof. \puc@coadviserIII}%
    \end{tabular}}%
  \vspace*{\stretch{6}}%
  \par%
  \puc@rightmargin{\puc@city \\ \puc@month\ de \puc@year}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% jury
%
\newcommand{\puc@showjury}{%
  \cleardoublepage%
  \puc@setmargins@front%
  \thispagestyle{empty}%
  \singlespacing\arial%
  \puc@rightmargin{\includegraphics{puc.ps}}%
  \vspace*{\stretch{0.5}}%
  \par%
  \puc@rightmargin{\large \arial \textbf{\puc@author} \normalsize}%
  \vspace*{\stretch{1}}%
  \par%
  \puc@rightmargin{\large \arial \textbf{\puc@title\relax} \normalsize}%
  \ifthenelse{\equal{\puc@subtitle}{}}{}{
    \vspace*{\stretch{0.1}}%
    \par%
    \puc@rightmargin{\arial \textbf{\puc@subtitle} \relax} }%
  \vspace*{\stretch{1.5}}%
  \par%
  \puc@leftmargin{\arial%
  \ifthenelse{\boolean{puc@phd}}%
  {Tese apresentada ao Programa de Pós--graduação em \puc@program\ do Departamento de \puc@department\ do \puc@school\ da \puc@uni\ como requisito parcial para obtenção Do título de Doutor em \puc@program. }%
  {Dissertação apresentada como requisito parcial para obtenção do grau de Mestre pelo Programa de Pós--graduação em \puc@program\ do Departamento de \puc@department\ do \puc@school\ da \puc@uni. }%
  Aprovada pela Comissão Examinadora abaixo assinada.%
  }%
  \vspace*{\stretch{1}}%
  \renewcommand{\jurymember}[2]{\ifthenelse{\equal{##1}{}}{}{%
    \puc@rightmargin{\textbf{Prof. ##1}\\##2} \vspace*{\stretch{1}}\par}}%
  \par%
  \ifthenelse{\equal{\puc@adviserInst}{}}{%
    \jurymember{\puc@adviser}{Orientador \\ Departamento de \puc@department\ --- \puc@uni}}{%
    \jurymember{\puc@adviser}{Orientador \\\puc@adviserInst}}
  \ifthenelse{\equal{\puc@coadviserInst}{}}{%
    \jurymember{\puc@coadviser}{Co--Orientador \\ Departamento de \puc@department\ --- \puc@uni}}{%
    \jurymember{\puc@coadviser}{Co--Orientador \\\puc@coadviserInst}}
  \ifthenelse{\equal{\puc@coadviserIIInst}{}}{%
    \jurymember{\puc@coadviserII}{Co--Orientador \\ Departamento de \puc@department\ --- \puc@uni}}{%
    \jurymember{\puc@coadviserII}{Co--Orientador \\\puc@coadviserIIInst}}
  \ifthenelse{\equal{\puc@coadviserIIIInst}{}}{%
    \jurymember{\puc@coadviserIII}{Co--Orientador \\ Departamento de \puc@department\ --- \puc@uni}}{%
    \jurymember{\puc@coadviserIII}{Co--Orientador \\\puc@coadviserIIIInst}}%
%-----------------%
    \puc@jury%
%-----------------%
  \puc@rightmargin{\puc@city, \puc@day\ de \puc@month\ de \puc@year}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% copyright
%
\newcommand{\puc@showcopyrights}{%
  \cleardoublepage%
  \puc@setmargins@front%
  \thispagestyle{empty}%
  \singlespacing\arial%
  \puc@leftmargin{\normalfont \normalsize Todos os direitos reservados. É proibida a reprodução total ou parcial do trabalho sem autorização da universidade, do autor e do orientador.}%
  \vspace*{\stretch{2}}%
  \par%
  \puc@rightmargin{\arial \textbf{\puc@author}}%
  \vspace*{\stretch{0.2}}%
  \par%
  \puc@leftmargin{\normalfont \puc@resume}%
  \vspace*{\stretch{2}}%
  \par%
  \puc@rightmargin{\small \arial Ficha Catalogr\'{a}fica}%
  \par%
  \puc@leftmargin{%
    \begin{tabular}{|p{0.95\linewidth}|} \hline%
      \noindent \puc@authorR \\ \\ \par%
      \hspace*{.6cm} \puc@title\relax\ / \puc@author; orientador: \puc@adviser\ifthenelse{\equal{\puc@coadviser}{}}{}{; co--orientador\ifthenelse{\equal{\puc@coadviserII}{}}{}{es}: \puc@coadviser}\ifthenelse{\equal{\puc@coadviserII}{}}{}{, \puc@coadviserII}\ifthenelse{\equal{\puc@coadviserIII}{}}{}{, \puc@coadviserIII}. --- \puc@city\ : \puc@uni, Departamento de \puc@department, \puc@year. \\ \\ \par%
      \hspace*{.6cm} v., \pageref{LastPage} f: il. ; 29,7 cm \\ \\ \par%
      \hspace*{.6cm} 1.   \ifthenelse{\boolean{puc@phd}}{Tese (doutorado)}{Dissertação (mestrado)} - \puc@university, Departamento de \puc@department. \\ \\ \par%
      \hspace*{.6cm} Inclui referências bibliográficas. \\ \\ \par%
      \hspace*{.6cm}%
        \newcounter{keycount}\setcounter{keycount}{1}%
        \renewcommand{\key}[1]{\arabic{keycount}. ##1.\addtocounter{keycount}{1}\ }%
        \key{\puc@department\ -- Tese}%
        \puc@keywords%
        \setcounter{keycount}{1}%
        \renewcommand{\key}[1]{\ifthenelse{\equal{##1}{}}{}{\Roman{keycount}. ##1.\addtocounter{keycount}{1}\ }}%
        \key{\puc@adviserR}%
        \key{\puc@coadviserR}%
        \key{\puc@coadviserIIR}%
        \key{\puc@coadviserIIIR}%
        \key{\puc@university. Departamento de \puc@department}%
        \key{Título}%
      \\%
    \hline \end{tabular}%
    }%
  \renewcommand{\footnoterule}{}\renewcommand{\@makefnmark}{\relax}%
  \footnotetext{\begin{flushright}\normalsize CDD: \puc@CDD \end{flushright}}%
  \renewcommand{\@makefnmark}{\@textsuperscript{\normalfont\@thefnmark}}%
%  \clearpage
  %\ifthenelse{\boolean{puc@tight}}{}{\onehalfspacing}\normalfont%
%  \renewcommand{\footnoterule}{\kern-3\p@\hrule width 2truein \kern 2.6\p@}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dedication
%
\newcommand{\puc@showdedication}{%
  \cleardoublepage%
  \puc@setmargins@dedication%
  \markboth{Dedicatória}{Dedicatória}%
  \thispagestyle{empty}%
  \singlespacing\normalfont%
  \vspace*{\stretch{1}}%
  \puc@rightmargin{%
%-------------%
 \puc@dedication
%-------------%
  }%
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% acknowledgment
%
\newcommand{\puc@showacknowledgment}{%
  \cleardoublepage%
  \puc@setmargins@dedication%
  \markboth{Agradecimentos}{Agradecimentos}%
  \thispagestyle{empty}%
  \onehalfspacing\normalfont%
  \noindent \large \arial \textbf{Agradecimentos}%
  \normalfont \normalsize \vspace*{\stretch{1}} \par%
%----------------%
 \puc@acknowledgment%
%----------------%
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% abstract (br)
%
\newcommand{\puc@showabstract}{%
  \clearpage%
  \puc@setmargins@dedication%
  \markboth{Resumo}{Resumo}%
  \thispagestyle{empty}%
  \singlespacing\normalfont%
  \pdfbookmark[0]{Resumo}{anchor:abstract}%
  \noindent \large \arial \textbf{Resumo} \normalfont \normalsize%
  \vspace{\stretch{3}}%
  \par\indent%
  \begin{minipage}{12cm}\singlespacing%
  \puc@authorR; \puc@adviserR\ifthenelse{\equal{\puc@coadviser}{}}{}{; \puc@coadviserR}\ifthenelse{\equal{\puc@coadviserII}{}}{}{; \puc@coadviserIIR}\ifthenelse{\equal{\puc@coadviserIII}{}}{}{; \puc@coadviserIIIR}. \textbf{\puc@title}. \puc@city, \puc@year. \pageref{LastPage}p.   \ifthenelse{\boolean{puc@phd}}{Tese de Doutorado}{Dissertação de Mestrado} --- Departamento de \puc@department, \puc@university.%
  \end{minipage}\onehalfspacing%
  \vspace*{\stretch{1}}%
  \par%
  \noindent\begin{minipage}{13.5cm}%
%--------%
 \puc@abstract%
%--------%
  \end{minipage}%
  \vspace*{\stretch{3}}%
  \par%
  \noindent \large \arial \textbf{Palavras--chave} \normalfont \normalsize%
  \par%
  \renewcommand{\key}[1]{##1.\hspace{3mm} }%
  \indent \puc@keywords%
  \vspace*{\stretch{4}}%
  \par%
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% abstract (uk)
%
\newcommand{\puc@showabstractuk}{%
  \selectlanguage{british}%
  \clearpage%
  \puc@setmargins@dedication%
  \markboth{Abstract}{Abstract}%
  \thispagestyle{empty}%
  \singlespacing\normalfont%
  \noindent \large \arial \textbf{Abstract} \normalfont \normalsize%
  \vspace{\stretch{3}}%
  \par\indent%
  \begin{minipage}{12cm}\singlespacing%
  \puc@authorR; \puc@adviserR\ifthenelse{\equal{\puc@coadviser}{}}{}{; \puc@coadviserR}\ifthenelse{\equal{\puc@coadviserII}{}}{}{; \puc@coadviserIIR}\ifthenelse{\equal{\puc@coadviserIII}{}}{}{; \puc@coadviserIIIR}. \textbf{\puc@titleuk}. \puc@city, \puc@year. \pageref{LastPage}p.   \ifthenelse{\boolean{puc@phd}}{PhD Thesis}{MsC Thesis} --- Department of \puc@department, \puc@university.%
  \end{minipage}\onehalfspacing%
  \vspace*{\stretch{1}}%
  \par%
  \noindent\begin{minipage}{13.5cm}%
%--------%
 \puc@abstractuk%
%--------%
  \end{minipage}%
  \vspace*{\stretch{3}}%
  \par%
  \noindent \large \arial \textbf{Keywords} \normalfont \normalsize%
  \par%
  \renewcommand{\key}[1]{##1.\hspace{3mm} }%
  \indent \puc@keywordsuk%
  \vspace*{\stretch{4}}%
  \par%
  \selectlanguage{brazil}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% tables
%
\newcommand{\puc@showtables}{%
   \cleardoublepage%
   \puc@setmargins@dedication%
   \renewcommand{\contentsname}{Sumário}%
   \renewcommand{\listfigurename}{Lista de figuras}%
   \renewcommand{\listtablename}{Lista de tabelas}%
   \markboth{\contentsname}{\contentsname}%
   \thispagestyle{empty}%
   \pdfbookmark[0]{\contentsname}{anchor:contents}%
   \singlespacing\normalfont%
   \tableofcontents%
   \ifthenelse{\equal{\puc@tablesmode}{fig} \or \equal{\puc@tablesmode}{figtab}}%
   {%
     \clearpage%
     \markboth{\listfigurename}{\listfigurename}%
     \thispagestyle{empty}%
     \listoffigures%
   }{}%
   \ifthenelse{\equal{\puc@tablesmode}{tab} \or \equal{\puc@tablesmode}{figtab}}%
   {%
     \clearpage%
     \markboth{\listtablename}{\listtablename}%
     \thispagestyle{empty}%
     \listoftables%
   }{}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% epigraph
%
\newcommand{\puc@showepigraph}{%
  \cleardoublepage%
  \puc@setmargins@text%
  \markboth{\contentsname}{\contentsname}%
  \thispagestyle{empty}%
  \singlespacing\normalfont%
  \vspace*{\stretch{1}}\par\indent%
  \hspace*{\stretch{2}}%
  \begin{minipage}[c]{8.2cm}%
  \it \puc@epigraph%
  \end{minipage}%
  \hspace*{\stretch{1}}%
  \par%
  \begin{flushright}%
  \textbf{\puc@epigraphauthor}, \textit{\puc@epigraphbook}.%
  \end{flushright}%
  }


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% index (to have the right bookmark in the pdf)
%
\renewenvironment{theindex}{%
  \if@twocolumn%
    \@restonecolfalse%
  \else%
    \@restonecoltrue%
  \fi%
  \twocolumn%
  \columnseprule \z@%
  \columnsep 35\p@%
  \chapter*{\indexname}%
  \addcontentsline{toc}{chapter}{\indexname}%
  \markboth{\indexname}{\indexname}
  \parskip\z@ \@plus .3\p@\relax%
  \let\item\@idxitem%
}{%
  \if@restonecol\onecolumn\else\clearpage\fi%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% summary of notations (to have the right bookmark in the pdf)
%
\newcommand{\notationsname}{Sumário das notações}%
\newenvironment{thenotations}{%
  \chapter*{\notationsname}%
  \markboth{\notationsname}{\notationsname}
  \addcontentsline{toc}{chapter}{\notationsname}
}{%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% all front matter
%
\newcommand{\puc@showfrontmatter}{%
  \pagestyle{empty}
%  \puc@showcover%
  \puc@showfront%
  \puc@showjury%
  \puc@showcopyrights%
%
  \ifthenelse{\boolean{hasdedication}}{\puc@showdedication}{}%
  \puc@showacknowledgment%
  \puc@showabstract%
  \puc@showabstractuk%
  \puc@showtables%
  \ifthenelse{\boolean{hasepigraph}}{\puc@showepigraph}{}%
%
  \puc@setmargins@text%
  \ifthenelse{\boolean{puc@tight}}{}{\onehalfspacing}%
  \normalfont%
  \puc@setpagestyle%
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\AtBeginDocument{%
  \renewcommand{\bibname}{Referências Bibliográficas}%
  \bibliographystyle{ThesisPUC}%
  \renewcommand{\key}[1]{#1. }
  \special{ pdf: docinfo << /Author (\puc@author) /Title (\puc@title) /Keywords (\puc@keywords) >> }%
}
\AfterBegin{document}{\makeatletter\puc@showfrontmatter\makeatother}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Local Variables:
%% mode: LaTeX
%% End:
\endinput
%%
%% End of file `ThesisPUC.cls'.
