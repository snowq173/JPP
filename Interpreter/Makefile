GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : SollangInterpreter

# Rules for building the parser.

AbsSollang.hs LexSollang.x ParSollang.y PrintSollang.hs TestSollang.hs : Sollang.cf
	bnfc --haskell --functor Sollang.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

SollangInterpreter:
	${GHC} SollangInterpreter.hs -o interpreter

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi interpreter

distclean : clean
	-rm -f AbsSollang.hs AbsSollang.hs.bak ComposOp.hs ComposOp.hs.bak DocSollang.txt DocSollang.txt.bak ErrM.hs ErrM.hs.bak LayoutSollang.hs LayoutSollang.hs.bak LexSollang.x LexSollang.x.bak ParSollang.y ParSollang.y.bak PrintSollang.hs PrintSollang.hs.bak SkelSollang.hs SkelSollang.hs.bak TestSollang.hs TestSollang.hs.bak XMLSollang.hs XMLSollang.hs.bak ASTSollang.agda ASTSollang.agda.bak ParserSollang.agda ParserSollang.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Sollang.dtd Sollang.dtd.bak TestSollang LexSollang.hs ParSollang.hs ParSollang.info ParDataSollang.hs
