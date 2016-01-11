// Generated from /home/andrew/Documents/git/MuBF/refimpl/src/main/antlr4/UIR.g4 by ANTLR 4.5.1
package uvm.ir.textinput.gen;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class UIRParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.5.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, T__25=26, T__26=27, T__27=28, T__28=29, T__29=30, T__30=31, 
		T__31=32, T__32=33, T__33=34, T__34=35, T__35=36, T__36=37, T__37=38, 
		T__38=39, T__39=40, T__40=41, T__41=42, T__42=43, T__43=44, T__44=45, 
		T__45=46, T__46=47, T__47=48, T__48=49, T__49=50, T__50=51, T__51=52, 
		T__52=53, T__53=54, T__54=55, T__55=56, T__56=57, T__57=58, T__58=59, 
		T__59=60, T__60=61, T__61=62, T__62=63, T__63=64, T__64=65, T__65=66, 
		T__66=67, T__67=68, T__68=69, T__69=70, T__70=71, T__71=72, T__72=73, 
		T__73=74, T__74=75, T__75=76, T__76=77, T__77=78, T__78=79, T__79=80, 
		T__80=81, T__81=82, T__82=83, T__83=84, T__84=85, T__85=86, T__86=87, 
		T__87=88, T__88=89, T__89=90, T__90=91, T__91=92, T__92=93, T__93=94, 
		T__94=95, T__95=96, T__96=97, T__97=98, T__98=99, T__99=100, T__100=101, 
		T__101=102, T__102=103, T__103=104, T__104=105, T__105=106, T__106=107, 
		T__107=108, T__108=109, T__109=110, T__110=111, T__111=112, T__112=113, 
		T__113=114, T__114=115, T__115=116, T__116=117, T__117=118, T__118=119, 
		T__119=120, T__120=121, T__121=122, T__122=123, T__123=124, T__124=125, 
		T__125=126, T__126=127, T__127=128, T__128=129, T__129=130, T__130=131, 
		T__131=132, T__132=133, T__133=134, T__134=135, T__135=136, T__136=137, 
		T__137=138, T__138=139, T__139=140, T__140=141, T__141=142, T__142=143, 
		T__143=144, T__144=145, T__145=146, T__146=147, T__147=148, T__148=149, 
		T__149=150, T__150=151, T__151=152, T__152=153, T__153=154, T__154=155, 
		INT_DEC=156, INT_OCT=157, INT_HEX=158, FP_NUM=159, INF=160, NAN=161, GLOBAL_NAME=162, 
		LOCAL_NAME=163, FLAG=164, WS=165, LINE_COMMENT=166;
	public static final int
		RULE_ir = 0, RULE_topLevelDef = 1, RULE_typeDef = 2, RULE_funcSigDef = 3, 
		RULE_constDef = 4, RULE_globalDef = 5, RULE_funcDecl = 6, RULE_funcDef = 7, 
		RULE_funcExpDef = 8, RULE_typeConstructor = 9, RULE_funcSigConstructor = 10, 
		RULE_constConstructor = 11, RULE_type = 12, RULE_funcSig = 13, RULE_constant = 14, 
		RULE_paramList = 15, RULE_funcBody = 16, RULE_basicBlock = 17, RULE_label = 18, 
		RULE_bbParam = 19, RULE_excParam = 20, RULE_instResults = 21, RULE_inst = 22, 
		RULE_instBody = 23, RULE_retVals = 24, RULE_destClause = 25, RULE_bb = 26, 
		RULE_value = 27, RULE_funcCallBody = 28, RULE_excClause = 29, RULE_keepAliveClause = 30, 
		RULE_flagList = 31, RULE_typeList = 32, RULE_funcSigList = 33, RULE_argList = 34, 
		RULE_curStackClause = 35, RULE_newStackClause = 36, RULE_binop = 37, RULE_cmpop = 38, 
		RULE_convop = 39, RULE_memord = 40, RULE_atomicrmwop = 41, RULE_flag = 42, 
		RULE_intLiteral = 43, RULE_floatLiteral = 44, RULE_doubleLiteral = 45, 
		RULE_name = 46;
	public static final String[] ruleNames = {
		"ir", "topLevelDef", "typeDef", "funcSigDef", "constDef", "globalDef", 
		"funcDecl", "funcDef", "funcExpDef", "typeConstructor", "funcSigConstructor", 
		"constConstructor", "type", "funcSig", "constant", "paramList", "funcBody", 
		"basicBlock", "label", "bbParam", "excParam", "instResults", "inst", "instBody", 
		"retVals", "destClause", "bb", "value", "funcCallBody", "excClause", "keepAliveClause", 
		"flagList", "typeList", "funcSigList", "argList", "curStackClause", "newStackClause", 
		"binop", "cmpop", "convop", "memord", "atomicrmwop", "flag", "intLiteral", 
		"floatLiteral", "doubleLiteral", "name"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'.typedef'", "'='", "'.funcsig'", "'.const'", "'<'", "'>'", "'.global'", 
		"'.funcdecl'", "'.funcdef'", "'VERSION'", "'.expose'", "'int'", "'float'", 
		"'double'", "'ref'", "'iref'", "'weakref'", "'struct'", "'array'", "'hybrid'", 
		"'void'", "'funcref'", "'threadref'", "'stackref'", "'tagref64'", "'vector'", 
		"'uptr'", "'ufuncptr'", "'('", "')'", "'->'", "'{'", "'}'", "'NULL'", 
		"':'", "'['", "']'", "'SELECT'", "'BRANCH'", "'BRANCH2'", "'SWITCH'", 
		"'CALL'", "'TAILCALL'", "'RET'", "'THROW'", "'EXTRACTVALUE'", "'INSERTVALUE'", 
		"'EXTRACTELEMENT'", "'INSERTELEMENT'", "'SHUFFLEVECTOR'", "'NEW'", "'NEWHYBRID'", 
		"'ALLOCA'", "'ALLOCAHYBRID'", "'GETIREF'", "'GETFIELDIREF'", "'PTR'", 
		"'GETELEMIREF'", "'SHIFTIREF'", "'GETVARPARTIREF'", "'LOAD'", "'STORE'", 
		"'CMPXCHG'", "'WEAK'", "'ATOMICRMW'", "'FENCE'", "'TRAP'", "'WATCHPOINT'", 
		"'WPEXC'", "'WPBRANCH'", "'CCALL'", "'NEWTHREAD'", "'SWAPSTACK'", "'COMMINST'", 
		"'EXC'", "'KEEPALIVE'", "'<['", "']>'", "'RET_WITH'", "'KILL_OLD'", "'PASS_VALUES'", 
		"'THROW_EXC'", "'ADD'", "'SUB'", "'MUL'", "'UDIV'", "'SDIV'", "'UREM'", 
		"'SREM'", "'SHL'", "'LSHR'", "'ASHR'", "'AND'", "'OR'", "'XOR'", "'FADD'", 
		"'FSUB'", "'FMUL'", "'FDIV'", "'FREM'", "'EQ'", "'NE'", "'SGT'", "'SLT'", 
		"'SGE'", "'SLE'", "'UGT'", "'ULT'", "'UGE'", "'ULE'", "'FTRUE'", "'FFALSE'", 
		"'FUNO'", "'FUEQ'", "'FUNE'", "'FUGT'", "'FULT'", "'FUGE'", "'FULE'", 
		"'FORD'", "'FOEQ'", "'FONE'", "'FOGT'", "'FOLT'", "'FOGE'", "'FOLE'", 
		"'TRUNC'", "'ZEXT'", "'SEXT'", "'FPTRUNC'", "'FPEXT'", "'FPTOUI'", "'FPTOSI'", 
		"'UITOFP'", "'SITOFP'", "'BITCAST'", "'REFCAST'", "'PTRCAST'", "'NOT_ATOMIC'", 
		"'RELAXED'", "'CONSUME'", "'ACQUIRE'", "'RELEASE'", "'ACQ_REL'", "'SEQ_CST'", 
		"'XCHG'", "'NAND'", "'MAX'", "'MIN'", "'UMAX'", "'UMIN'", "'f'", "'bitsf'", 
		"'d'", "'bitsd'", null, null, null, null, null, "'nan'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		"INT_DEC", "INT_OCT", "INT_HEX", "FP_NUM", "INF", "NAN", "GLOBAL_NAME", 
		"LOCAL_NAME", "FLAG", "WS", "LINE_COMMENT"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "UIR.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public UIRParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class IrContext extends ParserRuleContext {
		public List<TopLevelDefContext> topLevelDef() {
			return getRuleContexts(TopLevelDefContext.class);
		}
		public TopLevelDefContext topLevelDef(int i) {
			return getRuleContext(TopLevelDefContext.class,i);
		}
		public IrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ir; }
	}

	public final IrContext ir() throws RecognitionException {
		IrContext _localctx = new IrContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_ir);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(97);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__2) | (1L << T__3) | (1L << T__6) | (1L << T__7) | (1L << T__8) | (1L << T__10))) != 0)) {
				{
				{
				setState(94);
				topLevelDef();
				}
				}
				setState(99);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TopLevelDefContext extends ParserRuleContext {
		public TypeDefContext typeDef() {
			return getRuleContext(TypeDefContext.class,0);
		}
		public FuncSigDefContext funcSigDef() {
			return getRuleContext(FuncSigDefContext.class,0);
		}
		public ConstDefContext constDef() {
			return getRuleContext(ConstDefContext.class,0);
		}
		public GlobalDefContext globalDef() {
			return getRuleContext(GlobalDefContext.class,0);
		}
		public FuncDeclContext funcDecl() {
			return getRuleContext(FuncDeclContext.class,0);
		}
		public FuncDefContext funcDef() {
			return getRuleContext(FuncDefContext.class,0);
		}
		public FuncExpDefContext funcExpDef() {
			return getRuleContext(FuncExpDefContext.class,0);
		}
		public TopLevelDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_topLevelDef; }
	}

	public final TopLevelDefContext topLevelDef() throws RecognitionException {
		TopLevelDefContext _localctx = new TopLevelDefContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_topLevelDef);
		try {
			setState(107);
			switch (_input.LA(1)) {
			case T__0:
				enterOuterAlt(_localctx, 1);
				{
				setState(100);
				typeDef();
				}
				break;
			case T__2:
				enterOuterAlt(_localctx, 2);
				{
				setState(101);
				funcSigDef();
				}
				break;
			case T__3:
				enterOuterAlt(_localctx, 3);
				{
				setState(102);
				constDef();
				}
				break;
			case T__6:
				enterOuterAlt(_localctx, 4);
				{
				setState(103);
				globalDef();
				}
				break;
			case T__7:
				enterOuterAlt(_localctx, 5);
				{
				setState(104);
				funcDecl();
				}
				break;
			case T__8:
				enterOuterAlt(_localctx, 6);
				{
				setState(105);
				funcDef();
				}
				break;
			case T__10:
				enterOuterAlt(_localctx, 7);
				{
				setState(106);
				funcExpDef();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeDefContext extends ParserRuleContext {
		public Token nam;
		public TypeConstructorContext ctor;
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public TypeConstructorContext typeConstructor() {
			return getRuleContext(TypeConstructorContext.class,0);
		}
		public TypeDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeDef; }
	}

	public final TypeDefContext typeDef() throws RecognitionException {
		TypeDefContext _localctx = new TypeDefContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_typeDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(109);
			match(T__0);
			setState(110);
			((TypeDefContext)_localctx).nam = match(GLOBAL_NAME);
			setState(111);
			match(T__1);
			setState(112);
			((TypeDefContext)_localctx).ctor = typeConstructor();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncSigDefContext extends ParserRuleContext {
		public Token nam;
		public FuncSigConstructorContext ctor;
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public FuncSigConstructorContext funcSigConstructor() {
			return getRuleContext(FuncSigConstructorContext.class,0);
		}
		public FuncSigDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcSigDef; }
	}

	public final FuncSigDefContext funcSigDef() throws RecognitionException {
		FuncSigDefContext _localctx = new FuncSigDefContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_funcSigDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(114);
			match(T__2);
			setState(115);
			((FuncSigDefContext)_localctx).nam = match(GLOBAL_NAME);
			setState(116);
			match(T__1);
			setState(117);
			((FuncSigDefContext)_localctx).ctor = funcSigConstructor();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstDefContext extends ParserRuleContext {
		public Token nam;
		public TypeContext ty;
		public ConstConstructorContext ctor;
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ConstConstructorContext constConstructor() {
			return getRuleContext(ConstConstructorContext.class,0);
		}
		public ConstDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constDef; }
	}

	public final ConstDefContext constDef() throws RecognitionException {
		ConstDefContext _localctx = new ConstDefContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_constDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			match(T__3);
			setState(120);
			((ConstDefContext)_localctx).nam = match(GLOBAL_NAME);
			setState(121);
			match(T__4);
			setState(122);
			((ConstDefContext)_localctx).ty = type();
			setState(123);
			match(T__5);
			setState(124);
			match(T__1);
			setState(125);
			((ConstDefContext)_localctx).ctor = constConstructor();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GlobalDefContext extends ParserRuleContext {
		public Token nam;
		public TypeContext ty;
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public GlobalDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_globalDef; }
	}

	public final GlobalDefContext globalDef() throws RecognitionException {
		GlobalDefContext _localctx = new GlobalDefContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_globalDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(127);
			match(T__6);
			setState(128);
			((GlobalDefContext)_localctx).nam = match(GLOBAL_NAME);
			setState(129);
			match(T__4);
			setState(130);
			((GlobalDefContext)_localctx).ty = type();
			setState(131);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncDeclContext extends ParserRuleContext {
		public Token nam;
		public FuncSigContext sig;
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public FuncSigContext funcSig() {
			return getRuleContext(FuncSigContext.class,0);
		}
		public FuncDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcDecl; }
	}

	public final FuncDeclContext funcDecl() throws RecognitionException {
		FuncDeclContext _localctx = new FuncDeclContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_funcDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(133);
			match(T__7);
			setState(134);
			((FuncDeclContext)_localctx).nam = match(GLOBAL_NAME);
			setState(135);
			match(T__4);
			setState(136);
			((FuncDeclContext)_localctx).sig = funcSig();
			setState(137);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncDefContext extends ParserRuleContext {
		public Token nam;
		public NameContext ver;
		public FuncSigContext sig;
		public FuncBodyContext body;
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public FuncSigContext funcSig() {
			return getRuleContext(FuncSigContext.class,0);
		}
		public FuncBodyContext funcBody() {
			return getRuleContext(FuncBodyContext.class,0);
		}
		public FuncDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcDef; }
	}

	public final FuncDefContext funcDef() throws RecognitionException {
		FuncDefContext _localctx = new FuncDefContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_funcDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(139);
			match(T__8);
			setState(140);
			((FuncDefContext)_localctx).nam = match(GLOBAL_NAME);
			setState(141);
			match(T__9);
			setState(142);
			((FuncDefContext)_localctx).ver = name();
			setState(143);
			match(T__4);
			setState(144);
			((FuncDefContext)_localctx).sig = funcSig();
			setState(145);
			match(T__5);
			setState(146);
			((FuncDefContext)_localctx).body = funcBody();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncExpDefContext extends ParserRuleContext {
		public Token nam;
		public Token funcName;
		public FlagContext callConv;
		public Token cookie;
		public List<TerminalNode> GLOBAL_NAME() { return getTokens(UIRParser.GLOBAL_NAME); }
		public TerminalNode GLOBAL_NAME(int i) {
			return getToken(UIRParser.GLOBAL_NAME, i);
		}
		public FlagContext flag() {
			return getRuleContext(FlagContext.class,0);
		}
		public FuncExpDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcExpDef; }
	}

	public final FuncExpDefContext funcExpDef() throws RecognitionException {
		FuncExpDefContext _localctx = new FuncExpDefContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_funcExpDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(148);
			match(T__10);
			setState(149);
			((FuncExpDefContext)_localctx).nam = match(GLOBAL_NAME);
			setState(150);
			match(T__1);
			setState(151);
			((FuncExpDefContext)_localctx).funcName = match(GLOBAL_NAME);
			setState(152);
			((FuncExpDefContext)_localctx).callConv = flag();
			setState(153);
			((FuncExpDefContext)_localctx).cookie = match(GLOBAL_NAME);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeConstructorContext extends ParserRuleContext {
		public TypeConstructorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeConstructor; }
	 
		public TypeConstructorContext() { }
		public void copyFrom(TypeConstructorContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class TypeArrayContext extends TypeConstructorContext {
		public TypeContext ty;
		public IntLiteralContext length;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public TypeArrayContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeFloatContext extends TypeConstructorContext {
		public TypeFloatContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeIRefContext extends TypeConstructorContext {
		public TypeContext ty;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TypeIRefContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeVoidContext extends TypeConstructorContext {
		public TypeVoidContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeRefContext extends TypeConstructorContext {
		public TypeContext ty;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TypeRefContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeStackRefContext extends TypeConstructorContext {
		public TypeStackRefContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeHybridContext extends TypeConstructorContext {
		public TypeContext type;
		public List<TypeContext> fieldTys = new ArrayList<TypeContext>();
		public TypeContext varTy;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TypeHybridContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeTagRef64Context extends TypeConstructorContext {
		public TypeTagRef64Context(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeWeakRefContext extends TypeConstructorContext {
		public TypeContext ty;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TypeWeakRefContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeIntContext extends TypeConstructorContext {
		public IntLiteralContext length;
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public TypeIntContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeStructContext extends TypeConstructorContext {
		public TypeContext type;
		public List<TypeContext> fieldTys = new ArrayList<TypeContext>();
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TypeStructContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeFuncRefContext extends TypeConstructorContext {
		public FuncSigContext funcSig() {
			return getRuleContext(FuncSigContext.class,0);
		}
		public TypeFuncRefContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeDoubleContext extends TypeConstructorContext {
		public TypeDoubleContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeUFuncPtrContext extends TypeConstructorContext {
		public FuncSigContext funcSig() {
			return getRuleContext(FuncSigContext.class,0);
		}
		public TypeUFuncPtrContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeVectorContext extends TypeConstructorContext {
		public TypeContext ty;
		public IntLiteralContext length;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public TypeVectorContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeUPtrContext extends TypeConstructorContext {
		public TypeContext ty;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TypeUPtrContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class TypeThreadRefContext extends TypeConstructorContext {
		public TypeThreadRefContext(TypeConstructorContext ctx) { copyFrom(ctx); }
	}

	public final TypeConstructorContext typeConstructor() throws RecognitionException {
		TypeConstructorContext _localctx = new TypeConstructorContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_typeConstructor);
		int _la;
		try {
			int _alt;
			setState(228);
			switch (_input.LA(1)) {
			case T__11:
				_localctx = new TypeIntContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(155);
				match(T__11);
				setState(156);
				match(T__4);
				setState(157);
				((TypeIntContext)_localctx).length = intLiteral();
				setState(158);
				match(T__5);
				}
				break;
			case T__12:
				_localctx = new TypeFloatContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(160);
				match(T__12);
				}
				break;
			case T__13:
				_localctx = new TypeDoubleContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(161);
				match(T__13);
				}
				break;
			case T__14:
				_localctx = new TypeRefContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(162);
				match(T__14);
				setState(163);
				match(T__4);
				setState(164);
				((TypeRefContext)_localctx).ty = type();
				setState(165);
				match(T__5);
				}
				break;
			case T__15:
				_localctx = new TypeIRefContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(167);
				match(T__15);
				setState(168);
				match(T__4);
				setState(169);
				((TypeIRefContext)_localctx).ty = type();
				setState(170);
				match(T__5);
				}
				break;
			case T__16:
				_localctx = new TypeWeakRefContext(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(172);
				match(T__16);
				setState(173);
				match(T__4);
				setState(174);
				((TypeWeakRefContext)_localctx).ty = type();
				setState(175);
				match(T__5);
				}
				break;
			case T__17:
				_localctx = new TypeStructContext(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(177);
				match(T__17);
				setState(178);
				match(T__4);
				setState(180); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(179);
					((TypeStructContext)_localctx).type = type();
					((TypeStructContext)_localctx).fieldTys.add(((TypeStructContext)_localctx).type);
					}
					}
					setState(182); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==GLOBAL_NAME );
				setState(184);
				match(T__5);
				}
				break;
			case T__18:
				_localctx = new TypeArrayContext(_localctx);
				enterOuterAlt(_localctx, 8);
				{
				setState(186);
				match(T__18);
				setState(187);
				match(T__4);
				setState(188);
				((TypeArrayContext)_localctx).ty = type();
				setState(189);
				((TypeArrayContext)_localctx).length = intLiteral();
				setState(190);
				match(T__5);
				}
				break;
			case T__19:
				_localctx = new TypeHybridContext(_localctx);
				enterOuterAlt(_localctx, 9);
				{
				setState(192);
				match(T__19);
				setState(193);
				match(T__4);
				setState(197);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(194);
						((TypeHybridContext)_localctx).type = type();
						((TypeHybridContext)_localctx).fieldTys.add(((TypeHybridContext)_localctx).type);
						}
						} 
					}
					setState(199);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
				}
				setState(200);
				((TypeHybridContext)_localctx).varTy = type();
				setState(201);
				match(T__5);
				}
				break;
			case T__20:
				_localctx = new TypeVoidContext(_localctx);
				enterOuterAlt(_localctx, 10);
				{
				setState(203);
				match(T__20);
				}
				break;
			case T__21:
				_localctx = new TypeFuncRefContext(_localctx);
				enterOuterAlt(_localctx, 11);
				{
				setState(204);
				match(T__21);
				setState(205);
				match(T__4);
				setState(206);
				funcSig();
				setState(207);
				match(T__5);
				}
				break;
			case T__22:
				_localctx = new TypeThreadRefContext(_localctx);
				enterOuterAlt(_localctx, 12);
				{
				setState(209);
				match(T__22);
				}
				break;
			case T__23:
				_localctx = new TypeStackRefContext(_localctx);
				enterOuterAlt(_localctx, 13);
				{
				setState(210);
				match(T__23);
				}
				break;
			case T__24:
				_localctx = new TypeTagRef64Context(_localctx);
				enterOuterAlt(_localctx, 14);
				{
				setState(211);
				match(T__24);
				}
				break;
			case T__25:
				_localctx = new TypeVectorContext(_localctx);
				enterOuterAlt(_localctx, 15);
				{
				setState(212);
				match(T__25);
				setState(213);
				match(T__4);
				setState(214);
				((TypeVectorContext)_localctx).ty = type();
				setState(215);
				((TypeVectorContext)_localctx).length = intLiteral();
				setState(216);
				match(T__5);
				}
				break;
			case T__26:
				_localctx = new TypeUPtrContext(_localctx);
				enterOuterAlt(_localctx, 16);
				{
				setState(218);
				match(T__26);
				setState(219);
				match(T__4);
				setState(220);
				((TypeUPtrContext)_localctx).ty = type();
				setState(221);
				match(T__5);
				}
				break;
			case T__27:
				_localctx = new TypeUFuncPtrContext(_localctx);
				enterOuterAlt(_localctx, 17);
				{
				setState(223);
				match(T__27);
				setState(224);
				match(T__4);
				setState(225);
				funcSig();
				setState(226);
				match(T__5);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncSigConstructorContext extends ParserRuleContext {
		public TypeContext type;
		public List<TypeContext> paramTys = new ArrayList<TypeContext>();
		public List<TypeContext> retTys = new ArrayList<TypeContext>();
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public FuncSigConstructorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcSigConstructor; }
	}

	public final FuncSigConstructorContext funcSigConstructor() throws RecognitionException {
		FuncSigConstructorContext _localctx = new FuncSigConstructorContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_funcSigConstructor);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(230);
			match(T__28);
			setState(234);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME) {
				{
				{
				setState(231);
				((FuncSigConstructorContext)_localctx).type = type();
				((FuncSigConstructorContext)_localctx).paramTys.add(((FuncSigConstructorContext)_localctx).type);
				}
				}
				setState(236);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(237);
			match(T__29);
			setState(238);
			match(T__30);
			setState(239);
			match(T__28);
			setState(243);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME) {
				{
				{
				setState(240);
				((FuncSigConstructorContext)_localctx).type = type();
				((FuncSigConstructorContext)_localctx).retTys.add(((FuncSigConstructorContext)_localctx).type);
				}
				}
				setState(245);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(246);
			match(T__29);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstConstructorContext extends ParserRuleContext {
		public ConstConstructorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constConstructor; }
	 
		public ConstConstructorContext() { }
		public void copyFrom(ConstConstructorContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class CtorFloatContext extends ConstConstructorContext {
		public FloatLiteralContext floatLiteral() {
			return getRuleContext(FloatLiteralContext.class,0);
		}
		public CtorFloatContext(ConstConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class CtorListContext extends ConstConstructorContext {
		public List<TerminalNode> GLOBAL_NAME() { return getTokens(UIRParser.GLOBAL_NAME); }
		public TerminalNode GLOBAL_NAME(int i) {
			return getToken(UIRParser.GLOBAL_NAME, i);
		}
		public CtorListContext(ConstConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class CtorDoubleContext extends ConstConstructorContext {
		public DoubleLiteralContext doubleLiteral() {
			return getRuleContext(DoubleLiteralContext.class,0);
		}
		public CtorDoubleContext(ConstConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class CtorNullContext extends ConstConstructorContext {
		public CtorNullContext(ConstConstructorContext ctx) { copyFrom(ctx); }
	}
	public static class CtorIntContext extends ConstConstructorContext {
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public CtorIntContext(ConstConstructorContext ctx) { copyFrom(ctx); }
	}

	public final ConstConstructorContext constConstructor() throws RecognitionException {
		ConstConstructorContext _localctx = new ConstConstructorContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_constConstructor);
		int _la;
		try {
			setState(260);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				_localctx = new CtorIntContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(248);
				intLiteral();
				}
				break;
			case 2:
				_localctx = new CtorFloatContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(249);
				floatLiteral();
				}
				break;
			case 3:
				_localctx = new CtorDoubleContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(250);
				doubleLiteral();
				}
				break;
			case 4:
				_localctx = new CtorListContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(251);
				match(T__31);
				setState(255);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==GLOBAL_NAME) {
					{
					{
					setState(252);
					match(GLOBAL_NAME);
					}
					}
					setState(257);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(258);
				match(T__32);
				}
				break;
			case 5:
				_localctx = new CtorNullContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(259);
				match(T__33);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeContext extends ParserRuleContext {
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_type);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(262);
			match(GLOBAL_NAME);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncSigContext extends ParserRuleContext {
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public FuncSigContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcSig; }
	}

	public final FuncSigContext funcSig() throws RecognitionException {
		FuncSigContext _localctx = new FuncSigContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_funcSig);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(264);
			match(GLOBAL_NAME);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstantContext extends ParserRuleContext {
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public ConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constant; }
	}

	public final ConstantContext constant() throws RecognitionException {
		ConstantContext _localctx = new ConstantContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_constant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(266);
			match(GLOBAL_NAME);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamListContext extends ParserRuleContext {
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public ParamListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramList; }
	}

	public final ParamListContext paramList() throws RecognitionException {
		ParamListContext _localctx = new ParamListContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_paramList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(268);
			match(T__28);
			setState(272);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
				{
				{
				setState(269);
				name();
				}
				}
				setState(274);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(275);
			match(T__29);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncBodyContext extends ParserRuleContext {
		public List<BasicBlockContext> basicBlock() {
			return getRuleContexts(BasicBlockContext.class);
		}
		public BasicBlockContext basicBlock(int i) {
			return getRuleContext(BasicBlockContext.class,i);
		}
		public FuncBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcBody; }
	}

	public final FuncBodyContext funcBody() throws RecognitionException {
		FuncBodyContext _localctx = new FuncBodyContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_funcBody);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(277);
			match(T__31);
			setState(281);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
				{
				{
				setState(278);
				basicBlock();
				}
				}
				setState(283);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(284);
			match(T__32);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicBlockContext extends ParserRuleContext {
		public LabelContext label() {
			return getRuleContext(LabelContext.class,0);
		}
		public List<InstContext> inst() {
			return getRuleContexts(InstContext.class);
		}
		public InstContext inst(int i) {
			return getRuleContext(InstContext.class,i);
		}
		public BasicBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicBlock; }
	}

	public final BasicBlockContext basicBlock() throws RecognitionException {
		BasicBlockContext _localctx = new BasicBlockContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_basicBlock);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(286);
			label();
			setState(288); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(287);
					inst();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(290); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LabelContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public List<BbParamContext> bbParam() {
			return getRuleContexts(BbParamContext.class);
		}
		public BbParamContext bbParam(int i) {
			return getRuleContext(BbParamContext.class,i);
		}
		public ExcParamContext excParam() {
			return getRuleContext(ExcParamContext.class,0);
		}
		public LabelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_label; }
	}

	public final LabelContext label() throws RecognitionException {
		LabelContext _localctx = new LabelContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_label);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(292);
			name();
			setState(293);
			match(T__28);
			setState(297);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__4) {
				{
				{
				setState(294);
				bbParam();
				}
				}
				setState(299);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(300);
			match(T__29);
			setState(302);
			_la = _input.LA(1);
			if (_la==T__35) {
				{
				setState(301);
				excParam();
				}
			}

			setState(304);
			match(T__34);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BbParamContext extends ParserRuleContext {
		public TypeContext ty;
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public BbParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bbParam; }
	}

	public final BbParamContext bbParam() throws RecognitionException {
		BbParamContext _localctx = new BbParamContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_bbParam);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(306);
			match(T__4);
			setState(307);
			((BbParamContext)_localctx).ty = type();
			setState(308);
			match(T__5);
			setState(309);
			name();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExcParamContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ExcParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_excParam; }
	}

	public final ExcParamContext excParam() throws RecognitionException {
		ExcParamContext _localctx = new ExcParamContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_excParam);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(311);
			match(T__35);
			setState(312);
			name();
			setState(313);
			match(T__36);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InstResultsContext extends ParserRuleContext {
		public NameContext name;
		public List<NameContext> results = new ArrayList<NameContext>();
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public InstResultsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_instResults; }
	}

	public final InstResultsContext instResults() throws RecognitionException {
		InstResultsContext _localctx = new InstResultsContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_instResults);
		int _la;
		try {
			setState(324);
			switch (_input.LA(1)) {
			case GLOBAL_NAME:
			case LOCAL_NAME:
				enterOuterAlt(_localctx, 1);
				{
				setState(315);
				((InstResultsContext)_localctx).name = name();
				((InstResultsContext)_localctx).results.add(((InstResultsContext)_localctx).name);
				}
				break;
			case T__28:
				enterOuterAlt(_localctx, 2);
				{
				setState(316);
				match(T__28);
				setState(320);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
					{
					{
					setState(317);
					((InstResultsContext)_localctx).name = name();
					((InstResultsContext)_localctx).results.add(((InstResultsContext)_localctx).name);
					}
					}
					setState(322);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(323);
				match(T__29);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InstContext extends ParserRuleContext {
		public InstBodyContext instBody() {
			return getRuleContext(InstBodyContext.class,0);
		}
		public InstResultsContext instResults() {
			return getRuleContext(InstResultsContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public InstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_inst; }
	}

	public final InstContext inst() throws RecognitionException {
		InstContext _localctx = new InstContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_inst);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(329);
			_la = _input.LA(1);
			if (_la==T__28 || _la==GLOBAL_NAME || _la==LOCAL_NAME) {
				{
				setState(326);
				instResults();
				setState(327);
				match(T__1);
				}
			}

			setState(335);
			_la = _input.LA(1);
			if (_la==T__35) {
				{
				setState(331);
				match(T__35);
				setState(332);
				name();
				setState(333);
				match(T__36);
				}
			}

			setState(337);
			instBody();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InstBodyContext extends ParserRuleContext {
		public InstBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_instBody; }
	 
		public InstBodyContext() { }
		public void copyFrom(InstBodyContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class InstExtractElementContext extends InstBodyContext {
		public TypeContext seqTy;
		public TypeContext indTy;
		public ValueContext opnd;
		public ValueContext index;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstExtractElementContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstFenceContext extends InstBodyContext {
		public MemordContext memord() {
			return getRuleContext(MemordContext.class,0);
		}
		public InstFenceContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstWatchPointContext extends InstBodyContext {
		public IntLiteralContext wpid;
		public DestClauseContext dis;
		public DestClauseContext ena;
		public DestClauseContext wpExc;
		public TypeListContext typeList() {
			return getRuleContext(TypeListContext.class,0);
		}
		public KeepAliveClauseContext keepAliveClause() {
			return getRuleContext(KeepAliveClauseContext.class,0);
		}
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public List<DestClauseContext> destClause() {
			return getRuleContexts(DestClauseContext.class);
		}
		public DestClauseContext destClause(int i) {
			return getRuleContext(DestClauseContext.class,i);
		}
		public InstWatchPointContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstCallContext extends InstBodyContext {
		public FuncCallBodyContext funcCallBody() {
			return getRuleContext(FuncCallBodyContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public KeepAliveClauseContext keepAliveClause() {
			return getRuleContext(KeepAliveClauseContext.class,0);
		}
		public InstCallContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstInsertValueContext extends InstBodyContext {
		public TypeContext ty;
		public ValueContext opnd;
		public ValueContext newVal;
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstInsertValueContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstConversionContext extends InstBodyContext {
		public TypeContext fromTy;
		public TypeContext toTy;
		public ValueContext opnd;
		public ConvopContext convop() {
			return getRuleContext(ConvopContext.class,0);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstConversionContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstSwitchContext extends InstBodyContext {
		public TypeContext ty;
		public ValueContext opnd;
		public DestClauseContext defDest;
		public ValueContext value;
		public List<ValueContext> caseVal = new ArrayList<ValueContext>();
		public DestClauseContext destClause;
		public List<DestClauseContext> caseDest = new ArrayList<DestClauseContext>();
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public List<DestClauseContext> destClause() {
			return getRuleContexts(DestClauseContext.class);
		}
		public DestClauseContext destClause(int i) {
			return getRuleContext(DestClauseContext.class,i);
		}
		public InstSwitchContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstLoadContext extends InstBodyContext {
		public Token ptr;
		public ValueContext loc;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public MemordContext memord() {
			return getRuleContext(MemordContext.class,0);
		}
		public InstLoadContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstAtomicRMWContext extends InstBodyContext {
		public Token ptr;
		public ValueContext loc;
		public ValueContext opnd;
		public MemordContext memord() {
			return getRuleContext(MemordContext.class,0);
		}
		public AtomicrmwopContext atomicrmwop() {
			return getRuleContext(AtomicrmwopContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstAtomicRMWContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstAllocaHybridContext extends InstBodyContext {
		public TypeContext allocTy;
		public TypeContext lenTy;
		public ValueContext length;
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstAllocaHybridContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstInsertElementContext extends InstBodyContext {
		public TypeContext seqTy;
		public TypeContext indTy;
		public ValueContext opnd;
		public ValueContext index;
		public ValueContext newVal;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstInsertElementContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstBranchContext extends InstBodyContext {
		public DestClauseContext dest;
		public DestClauseContext destClause() {
			return getRuleContext(DestClauseContext.class,0);
		}
		public InstBranchContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstGetVarPartIRefContext extends InstBodyContext {
		public Token ptr;
		public TypeContext refTy;
		public ValueContext opnd;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstGetVarPartIRefContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstThrowContext extends InstBodyContext {
		public ValueContext exc;
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstThrowContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstTrapContext extends InstBodyContext {
		public TypeListContext typeList() {
			return getRuleContext(TypeListContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public KeepAliveClauseContext keepAliveClause() {
			return getRuleContext(KeepAliveClauseContext.class,0);
		}
		public InstTrapContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstExtractValueContext extends InstBodyContext {
		public TypeContext ty;
		public ValueContext opnd;
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstExtractValueContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstSwapStackContext extends InstBodyContext {
		public ValueContext swappee;
		public CurStackClauseContext curStackClause() {
			return getRuleContext(CurStackClauseContext.class,0);
		}
		public NewStackClauseContext newStackClause() {
			return getRuleContext(NewStackClauseContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public KeepAliveClauseContext keepAliveClause() {
			return getRuleContext(KeepAliveClauseContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstSwapStackContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstTailCallContext extends InstBodyContext {
		public FuncCallBodyContext funcCallBody() {
			return getRuleContext(FuncCallBodyContext.class,0);
		}
		public InstTailCallContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstGetIRefContext extends InstBodyContext {
		public TypeContext refTy;
		public ValueContext opnd;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstGetIRefContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstShuffleVectorContext extends InstBodyContext {
		public TypeContext vecTy;
		public TypeContext maskTy;
		public ValueContext vec1;
		public ValueContext vec2;
		public ValueContext mask;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstShuffleVectorContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstCmpContext extends InstBodyContext {
		public TypeContext ty;
		public ValueContext op1;
		public ValueContext op2;
		public CmpopContext cmpop() {
			return getRuleContext(CmpopContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstCmpContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstShiftIRefContext extends InstBodyContext {
		public Token ptr;
		public TypeContext refTy;
		public TypeContext offTy;
		public ValueContext opnd;
		public ValueContext offset;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstShiftIRefContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstCCallContext extends InstBodyContext {
		public FlagContext callConv;
		public TypeContext funcTy;
		public ValueContext callee;
		public FuncSigContext funcSig() {
			return getRuleContext(FuncSigContext.class,0);
		}
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public KeepAliveClauseContext keepAliveClause() {
			return getRuleContext(KeepAliveClauseContext.class,0);
		}
		public FlagContext flag() {
			return getRuleContext(FlagContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstCCallContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstNewHybridContext extends InstBodyContext {
		public TypeContext allocTy;
		public TypeContext lenTy;
		public ValueContext length;
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstNewHybridContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstGetFieldIRefContext extends InstBodyContext {
		public Token ptr;
		public TypeContext refTy;
		public IntLiteralContext index;
		public ValueContext opnd;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstGetFieldIRefContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstCommInstContext extends InstBodyContext {
		public Token nam;
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public KeepAliveClauseContext keepAliveClause() {
			return getRuleContext(KeepAliveClauseContext.class,0);
		}
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public FlagListContext flagList() {
			return getRuleContext(FlagListContext.class,0);
		}
		public TypeListContext typeList() {
			return getRuleContext(TypeListContext.class,0);
		}
		public FuncSigListContext funcSigList() {
			return getRuleContext(FuncSigListContext.class,0);
		}
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public InstCommInstContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstCmpXchgContext extends InstBodyContext {
		public Token ptr;
		public Token isWeak;
		public MemordContext ordSucc;
		public MemordContext ordFail;
		public ValueContext loc;
		public ValueContext expected;
		public ValueContext desired;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public List<MemordContext> memord() {
			return getRuleContexts(MemordContext.class);
		}
		public MemordContext memord(int i) {
			return getRuleContext(MemordContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstCmpXchgContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstAllocaContext extends InstBodyContext {
		public TypeContext allocTy;
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public InstAllocaContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstStoreContext extends InstBodyContext {
		public Token ptr;
		public ValueContext loc;
		public ValueContext newVal;
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public MemordContext memord() {
			return getRuleContext(MemordContext.class,0);
		}
		public InstStoreContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstBranch2Context extends InstBodyContext {
		public ValueContext cond;
		public DestClauseContext ifTrue;
		public DestClauseContext ifFalse;
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public List<DestClauseContext> destClause() {
			return getRuleContexts(DestClauseContext.class);
		}
		public DestClauseContext destClause(int i) {
			return getRuleContext(DestClauseContext.class,i);
		}
		public InstBranch2Context(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstBinOpContext extends InstBodyContext {
		public TypeContext ty;
		public ValueContext op1;
		public ValueContext op2;
		public BinopContext binop() {
			return getRuleContext(BinopContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstBinOpContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstSelectContext extends InstBodyContext {
		public TypeContext condTy;
		public TypeContext resTy;
		public ValueContext cond;
		public ValueContext ifTrue;
		public ValueContext ifFalse;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstSelectContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstNewThreadContext extends InstBodyContext {
		public ValueContext stack;
		public NewStackClauseContext newStackClause() {
			return getRuleContext(NewStackClauseContext.class,0);
		}
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public InstNewThreadContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstWPBranchContext extends InstBodyContext {
		public IntLiteralContext wpid;
		public DestClauseContext dis;
		public DestClauseContext ena;
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public List<DestClauseContext> destClause() {
			return getRuleContexts(DestClauseContext.class);
		}
		public DestClauseContext destClause(int i) {
			return getRuleContext(DestClauseContext.class,i);
		}
		public InstWPBranchContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstGetElemIRefContext extends InstBodyContext {
		public Token ptr;
		public TypeContext refTy;
		public TypeContext indTy;
		public ValueContext opnd;
		public ValueContext index;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public InstGetElemIRefContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstNewContext extends InstBodyContext {
		public TypeContext allocTy;
		public ExcClauseContext excClause() {
			return getRuleContext(ExcClauseContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public InstNewContext(InstBodyContext ctx) { copyFrom(ctx); }
	}
	public static class InstRetContext extends InstBodyContext {
		public RetValsContext retVals() {
			return getRuleContext(RetValsContext.class,0);
		}
		public InstRetContext(InstBodyContext ctx) { copyFrom(ctx); }
	}

	public final InstBodyContext instBody() throws RecognitionException {
		InstBodyContext _localctx = new InstBodyContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_instBody);
		int _la;
		try {
			setState(644);
			switch (_input.LA(1)) {
			case T__82:
			case T__83:
			case T__84:
			case T__85:
			case T__86:
			case T__87:
			case T__88:
			case T__89:
			case T__90:
			case T__91:
			case T__92:
			case T__93:
			case T__94:
			case T__95:
			case T__96:
			case T__97:
			case T__98:
			case T__99:
				_localctx = new InstBinOpContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(339);
				binop();
				setState(340);
				match(T__4);
				setState(341);
				((InstBinOpContext)_localctx).ty = type();
				setState(342);
				match(T__5);
				setState(343);
				((InstBinOpContext)_localctx).op1 = value();
				setState(344);
				((InstBinOpContext)_localctx).op2 = value();
				setState(345);
				excClause();
				}
				break;
			case T__100:
			case T__101:
			case T__102:
			case T__103:
			case T__104:
			case T__105:
			case T__106:
			case T__107:
			case T__108:
			case T__109:
			case T__110:
			case T__111:
			case T__112:
			case T__113:
			case T__114:
			case T__115:
			case T__116:
			case T__117:
			case T__118:
			case T__119:
			case T__120:
			case T__121:
			case T__122:
			case T__123:
			case T__124:
			case T__125:
				_localctx = new InstCmpContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(347);
				cmpop();
				setState(348);
				match(T__4);
				setState(349);
				((InstCmpContext)_localctx).ty = type();
				setState(350);
				match(T__5);
				setState(351);
				((InstCmpContext)_localctx).op1 = value();
				setState(352);
				((InstCmpContext)_localctx).op2 = value();
				}
				break;
			case T__126:
			case T__127:
			case T__128:
			case T__129:
			case T__130:
			case T__131:
			case T__132:
			case T__133:
			case T__134:
			case T__135:
			case T__136:
			case T__137:
				_localctx = new InstConversionContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(354);
				convop();
				setState(355);
				match(T__4);
				setState(356);
				((InstConversionContext)_localctx).fromTy = type();
				setState(357);
				((InstConversionContext)_localctx).toTy = type();
				setState(358);
				match(T__5);
				setState(359);
				((InstConversionContext)_localctx).opnd = value();
				}
				break;
			case T__37:
				_localctx = new InstSelectContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(361);
				match(T__37);
				setState(362);
				match(T__4);
				setState(363);
				((InstSelectContext)_localctx).condTy = type();
				setState(364);
				((InstSelectContext)_localctx).resTy = type();
				setState(365);
				match(T__5);
				setState(366);
				((InstSelectContext)_localctx).cond = value();
				setState(367);
				((InstSelectContext)_localctx).ifTrue = value();
				setState(368);
				((InstSelectContext)_localctx).ifFalse = value();
				}
				break;
			case T__38:
				_localctx = new InstBranchContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(370);
				match(T__38);
				setState(371);
				((InstBranchContext)_localctx).dest = destClause();
				}
				break;
			case T__39:
				_localctx = new InstBranch2Context(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(372);
				match(T__39);
				setState(373);
				((InstBranch2Context)_localctx).cond = value();
				setState(374);
				((InstBranch2Context)_localctx).ifTrue = destClause();
				setState(375);
				((InstBranch2Context)_localctx).ifFalse = destClause();
				}
				break;
			case T__40:
				_localctx = new InstSwitchContext(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(377);
				match(T__40);
				setState(378);
				match(T__4);
				setState(379);
				((InstSwitchContext)_localctx).ty = type();
				setState(380);
				match(T__5);
				setState(381);
				((InstSwitchContext)_localctx).opnd = value();
				setState(382);
				((InstSwitchContext)_localctx).defDest = destClause();
				setState(383);
				match(T__31);
				setState(389);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
					{
					{
					setState(384);
					((InstSwitchContext)_localctx).value = value();
					((InstSwitchContext)_localctx).caseVal.add(((InstSwitchContext)_localctx).value);
					setState(385);
					((InstSwitchContext)_localctx).destClause = destClause();
					((InstSwitchContext)_localctx).caseDest.add(((InstSwitchContext)_localctx).destClause);
					}
					}
					setState(391);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(392);
				match(T__32);
				}
				break;
			case T__41:
				_localctx = new InstCallContext(_localctx);
				enterOuterAlt(_localctx, 8);
				{
				setState(394);
				match(T__41);
				setState(395);
				funcCallBody();
				setState(396);
				excClause();
				setState(397);
				keepAliveClause();
				}
				break;
			case T__42:
				_localctx = new InstTailCallContext(_localctx);
				enterOuterAlt(_localctx, 9);
				{
				setState(399);
				match(T__42);
				setState(400);
				funcCallBody();
				}
				break;
			case T__43:
				_localctx = new InstRetContext(_localctx);
				enterOuterAlt(_localctx, 10);
				{
				setState(401);
				match(T__43);
				setState(402);
				retVals();
				}
				break;
			case T__44:
				_localctx = new InstThrowContext(_localctx);
				enterOuterAlt(_localctx, 11);
				{
				setState(403);
				match(T__44);
				setState(404);
				((InstThrowContext)_localctx).exc = value();
				}
				break;
			case T__45:
				_localctx = new InstExtractValueContext(_localctx);
				enterOuterAlt(_localctx, 12);
				{
				setState(405);
				match(T__45);
				setState(406);
				match(T__4);
				setState(407);
				((InstExtractValueContext)_localctx).ty = type();
				setState(408);
				intLiteral();
				setState(409);
				match(T__5);
				setState(410);
				((InstExtractValueContext)_localctx).opnd = value();
				}
				break;
			case T__46:
				_localctx = new InstInsertValueContext(_localctx);
				enterOuterAlt(_localctx, 13);
				{
				setState(412);
				match(T__46);
				setState(413);
				match(T__4);
				setState(414);
				((InstInsertValueContext)_localctx).ty = type();
				setState(415);
				intLiteral();
				setState(416);
				match(T__5);
				setState(417);
				((InstInsertValueContext)_localctx).opnd = value();
				setState(418);
				((InstInsertValueContext)_localctx).newVal = value();
				}
				break;
			case T__47:
				_localctx = new InstExtractElementContext(_localctx);
				enterOuterAlt(_localctx, 14);
				{
				setState(420);
				match(T__47);
				setState(421);
				match(T__4);
				setState(422);
				((InstExtractElementContext)_localctx).seqTy = type();
				setState(423);
				((InstExtractElementContext)_localctx).indTy = type();
				setState(424);
				match(T__5);
				setState(425);
				((InstExtractElementContext)_localctx).opnd = value();
				setState(426);
				((InstExtractElementContext)_localctx).index = value();
				}
				break;
			case T__48:
				_localctx = new InstInsertElementContext(_localctx);
				enterOuterAlt(_localctx, 15);
				{
				setState(428);
				match(T__48);
				setState(429);
				match(T__4);
				setState(430);
				((InstInsertElementContext)_localctx).seqTy = type();
				setState(431);
				((InstInsertElementContext)_localctx).indTy = type();
				setState(432);
				match(T__5);
				setState(433);
				((InstInsertElementContext)_localctx).opnd = value();
				setState(434);
				((InstInsertElementContext)_localctx).index = value();
				setState(435);
				((InstInsertElementContext)_localctx).newVal = value();
				}
				break;
			case T__49:
				_localctx = new InstShuffleVectorContext(_localctx);
				enterOuterAlt(_localctx, 16);
				{
				setState(437);
				match(T__49);
				setState(438);
				match(T__4);
				setState(439);
				((InstShuffleVectorContext)_localctx).vecTy = type();
				setState(440);
				((InstShuffleVectorContext)_localctx).maskTy = type();
				setState(441);
				match(T__5);
				setState(442);
				((InstShuffleVectorContext)_localctx).vec1 = value();
				setState(443);
				((InstShuffleVectorContext)_localctx).vec2 = value();
				setState(444);
				((InstShuffleVectorContext)_localctx).mask = value();
				}
				break;
			case T__50:
				_localctx = new InstNewContext(_localctx);
				enterOuterAlt(_localctx, 17);
				{
				setState(446);
				match(T__50);
				setState(447);
				match(T__4);
				setState(448);
				((InstNewContext)_localctx).allocTy = type();
				setState(449);
				match(T__5);
				setState(450);
				excClause();
				}
				break;
			case T__51:
				_localctx = new InstNewHybridContext(_localctx);
				enterOuterAlt(_localctx, 18);
				{
				setState(452);
				match(T__51);
				setState(453);
				match(T__4);
				setState(454);
				((InstNewHybridContext)_localctx).allocTy = type();
				setState(455);
				((InstNewHybridContext)_localctx).lenTy = type();
				setState(456);
				match(T__5);
				setState(457);
				((InstNewHybridContext)_localctx).length = value();
				setState(458);
				excClause();
				}
				break;
			case T__52:
				_localctx = new InstAllocaContext(_localctx);
				enterOuterAlt(_localctx, 19);
				{
				setState(460);
				match(T__52);
				setState(461);
				match(T__4);
				setState(462);
				((InstAllocaContext)_localctx).allocTy = type();
				setState(463);
				match(T__5);
				setState(464);
				excClause();
				}
				break;
			case T__53:
				_localctx = new InstAllocaHybridContext(_localctx);
				enterOuterAlt(_localctx, 20);
				{
				setState(466);
				match(T__53);
				setState(467);
				match(T__4);
				setState(468);
				((InstAllocaHybridContext)_localctx).allocTy = type();
				setState(469);
				((InstAllocaHybridContext)_localctx).lenTy = type();
				setState(470);
				match(T__5);
				setState(471);
				((InstAllocaHybridContext)_localctx).length = value();
				setState(472);
				excClause();
				}
				break;
			case T__54:
				_localctx = new InstGetIRefContext(_localctx);
				enterOuterAlt(_localctx, 21);
				{
				setState(474);
				match(T__54);
				setState(475);
				match(T__4);
				setState(476);
				((InstGetIRefContext)_localctx).refTy = type();
				setState(477);
				match(T__5);
				setState(478);
				((InstGetIRefContext)_localctx).opnd = value();
				}
				break;
			case T__55:
				_localctx = new InstGetFieldIRefContext(_localctx);
				enterOuterAlt(_localctx, 22);
				{
				setState(480);
				match(T__55);
				{
				setState(482);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(481);
					((InstGetFieldIRefContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(484);
				match(T__4);
				setState(485);
				((InstGetFieldIRefContext)_localctx).refTy = type();
				setState(486);
				((InstGetFieldIRefContext)_localctx).index = intLiteral();
				setState(487);
				match(T__5);
				setState(488);
				((InstGetFieldIRefContext)_localctx).opnd = value();
				}
				break;
			case T__57:
				_localctx = new InstGetElemIRefContext(_localctx);
				enterOuterAlt(_localctx, 23);
				{
				setState(490);
				match(T__57);
				{
				setState(492);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(491);
					((InstGetElemIRefContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(494);
				match(T__4);
				setState(495);
				((InstGetElemIRefContext)_localctx).refTy = type();
				setState(496);
				((InstGetElemIRefContext)_localctx).indTy = type();
				setState(497);
				match(T__5);
				setState(498);
				((InstGetElemIRefContext)_localctx).opnd = value();
				setState(499);
				((InstGetElemIRefContext)_localctx).index = value();
				}
				break;
			case T__58:
				_localctx = new InstShiftIRefContext(_localctx);
				enterOuterAlt(_localctx, 24);
				{
				setState(501);
				match(T__58);
				{
				setState(503);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(502);
					((InstShiftIRefContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(505);
				match(T__4);
				setState(506);
				((InstShiftIRefContext)_localctx).refTy = type();
				setState(507);
				((InstShiftIRefContext)_localctx).offTy = type();
				setState(508);
				match(T__5);
				setState(509);
				((InstShiftIRefContext)_localctx).opnd = value();
				setState(510);
				((InstShiftIRefContext)_localctx).offset = value();
				}
				break;
			case T__59:
				_localctx = new InstGetVarPartIRefContext(_localctx);
				enterOuterAlt(_localctx, 25);
				{
				setState(512);
				match(T__59);
				{
				setState(514);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(513);
					((InstGetVarPartIRefContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(516);
				match(T__4);
				setState(517);
				((InstGetVarPartIRefContext)_localctx).refTy = type();
				setState(518);
				match(T__5);
				setState(519);
				((InstGetVarPartIRefContext)_localctx).opnd = value();
				}
				break;
			case T__60:
				_localctx = new InstLoadContext(_localctx);
				enterOuterAlt(_localctx, 26);
				{
				setState(521);
				match(T__60);
				{
				setState(523);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(522);
					((InstLoadContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(526);
				_la = _input.LA(1);
				if (((((_la - 139)) & ~0x3f) == 0 && ((1L << (_la - 139)) & ((1L << (T__138 - 139)) | (1L << (T__139 - 139)) | (1L << (T__140 - 139)) | (1L << (T__141 - 139)) | (1L << (T__142 - 139)) | (1L << (T__143 - 139)) | (1L << (T__144 - 139)))) != 0)) {
					{
					setState(525);
					memord();
					}
				}

				setState(528);
				match(T__4);
				setState(529);
				type();
				setState(530);
				match(T__5);
				setState(531);
				((InstLoadContext)_localctx).loc = value();
				setState(532);
				excClause();
				}
				break;
			case T__61:
				_localctx = new InstStoreContext(_localctx);
				enterOuterAlt(_localctx, 27);
				{
				setState(534);
				match(T__61);
				{
				setState(536);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(535);
					((InstStoreContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(539);
				_la = _input.LA(1);
				if (((((_la - 139)) & ~0x3f) == 0 && ((1L << (_la - 139)) & ((1L << (T__138 - 139)) | (1L << (T__139 - 139)) | (1L << (T__140 - 139)) | (1L << (T__141 - 139)) | (1L << (T__142 - 139)) | (1L << (T__143 - 139)) | (1L << (T__144 - 139)))) != 0)) {
					{
					setState(538);
					memord();
					}
				}

				setState(541);
				match(T__4);
				setState(542);
				type();
				setState(543);
				match(T__5);
				setState(544);
				((InstStoreContext)_localctx).loc = value();
				setState(545);
				((InstStoreContext)_localctx).newVal = value();
				setState(546);
				excClause();
				}
				break;
			case T__62:
				_localctx = new InstCmpXchgContext(_localctx);
				enterOuterAlt(_localctx, 28);
				{
				setState(548);
				match(T__62);
				{
				setState(550);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(549);
					((InstCmpXchgContext)_localctx).ptr = match(T__56);
					}
				}

				}
				{
				setState(553);
				_la = _input.LA(1);
				if (_la==T__63) {
					{
					setState(552);
					((InstCmpXchgContext)_localctx).isWeak = match(T__63);
					}
				}

				}
				setState(555);
				((InstCmpXchgContext)_localctx).ordSucc = memord();
				setState(556);
				((InstCmpXchgContext)_localctx).ordFail = memord();
				setState(557);
				match(T__4);
				setState(558);
				type();
				setState(559);
				match(T__5);
				setState(560);
				((InstCmpXchgContext)_localctx).loc = value();
				setState(561);
				((InstCmpXchgContext)_localctx).expected = value();
				setState(562);
				((InstCmpXchgContext)_localctx).desired = value();
				setState(563);
				excClause();
				}
				break;
			case T__64:
				_localctx = new InstAtomicRMWContext(_localctx);
				enterOuterAlt(_localctx, 29);
				{
				setState(565);
				match(T__64);
				{
				setState(567);
				_la = _input.LA(1);
				if (_la==T__56) {
					{
					setState(566);
					((InstAtomicRMWContext)_localctx).ptr = match(T__56);
					}
				}

				}
				setState(569);
				memord();
				setState(570);
				atomicrmwop();
				setState(571);
				match(T__4);
				setState(572);
				type();
				setState(573);
				match(T__5);
				setState(574);
				((InstAtomicRMWContext)_localctx).loc = value();
				setState(575);
				((InstAtomicRMWContext)_localctx).opnd = value();
				setState(576);
				excClause();
				}
				break;
			case T__65:
				_localctx = new InstFenceContext(_localctx);
				enterOuterAlt(_localctx, 30);
				{
				setState(578);
				match(T__65);
				setState(579);
				memord();
				}
				break;
			case T__66:
				_localctx = new InstTrapContext(_localctx);
				enterOuterAlt(_localctx, 31);
				{
				setState(580);
				match(T__66);
				setState(581);
				typeList();
				setState(582);
				excClause();
				setState(583);
				keepAliveClause();
				}
				break;
			case T__67:
				_localctx = new InstWatchPointContext(_localctx);
				enterOuterAlt(_localctx, 32);
				{
				setState(585);
				match(T__67);
				setState(586);
				((InstWatchPointContext)_localctx).wpid = intLiteral();
				setState(587);
				typeList();
				setState(588);
				((InstWatchPointContext)_localctx).dis = destClause();
				setState(589);
				((InstWatchPointContext)_localctx).ena = destClause();
				setState(595);
				_la = _input.LA(1);
				if (_la==T__68) {
					{
					setState(590);
					match(T__68);
					setState(591);
					match(T__28);
					setState(592);
					((InstWatchPointContext)_localctx).wpExc = destClause();
					setState(593);
					match(T__29);
					}
				}

				setState(597);
				keepAliveClause();
				}
				break;
			case T__69:
				_localctx = new InstWPBranchContext(_localctx);
				enterOuterAlt(_localctx, 33);
				{
				setState(599);
				match(T__69);
				setState(600);
				((InstWPBranchContext)_localctx).wpid = intLiteral();
				setState(601);
				((InstWPBranchContext)_localctx).dis = destClause();
				setState(602);
				((InstWPBranchContext)_localctx).ena = destClause();
				}
				break;
			case T__70:
				_localctx = new InstCCallContext(_localctx);
				enterOuterAlt(_localctx, 34);
				{
				setState(604);
				match(T__70);
				setState(605);
				((InstCCallContext)_localctx).callConv = flag();
				setState(606);
				match(T__4);
				setState(607);
				((InstCCallContext)_localctx).funcTy = type();
				setState(608);
				funcSig();
				setState(609);
				match(T__5);
				setState(610);
				((InstCCallContext)_localctx).callee = value();
				setState(611);
				argList();
				setState(612);
				excClause();
				setState(613);
				keepAliveClause();
				}
				break;
			case T__71:
				_localctx = new InstNewThreadContext(_localctx);
				enterOuterAlt(_localctx, 35);
				{
				setState(615);
				match(T__71);
				setState(616);
				((InstNewThreadContext)_localctx).stack = value();
				setState(617);
				newStackClause();
				setState(618);
				excClause();
				}
				break;
			case T__72:
				_localctx = new InstSwapStackContext(_localctx);
				enterOuterAlt(_localctx, 36);
				{
				setState(620);
				match(T__72);
				setState(621);
				((InstSwapStackContext)_localctx).swappee = value();
				setState(622);
				curStackClause();
				setState(623);
				newStackClause();
				setState(624);
				excClause();
				setState(625);
				keepAliveClause();
				}
				break;
			case T__73:
				_localctx = new InstCommInstContext(_localctx);
				enterOuterAlt(_localctx, 37);
				{
				setState(627);
				match(T__73);
				setState(628);
				((InstCommInstContext)_localctx).nam = match(GLOBAL_NAME);
				setState(630);
				switch ( getInterpreter().adaptivePredict(_input,31,_ctx) ) {
				case 1:
					{
					setState(629);
					flagList();
					}
					break;
				}
				setState(633);
				_la = _input.LA(1);
				if (_la==T__4) {
					{
					setState(632);
					typeList();
					}
				}

				setState(636);
				_la = _input.LA(1);
				if (_la==T__76) {
					{
					setState(635);
					funcSigList();
					}
				}

				setState(639);
				switch ( getInterpreter().adaptivePredict(_input,34,_ctx) ) {
				case 1:
					{
					setState(638);
					argList();
					}
					break;
				}
				setState(641);
				excClause();
				setState(642);
				keepAliveClause();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RetValsContext extends ParserRuleContext {
		public ValueContext value;
		public List<ValueContext> vals = new ArrayList<ValueContext>();
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public RetValsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_retVals; }
	}

	public final RetValsContext retVals() throws RecognitionException {
		RetValsContext _localctx = new RetValsContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_retVals);
		int _la;
		try {
			setState(655);
			switch (_input.LA(1)) {
			case T__28:
				enterOuterAlt(_localctx, 1);
				{
				setState(646);
				match(T__28);
				setState(650);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
					{
					{
					setState(647);
					((RetValsContext)_localctx).value = value();
					((RetValsContext)_localctx).vals.add(((RetValsContext)_localctx).value);
					}
					}
					setState(652);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(653);
				match(T__29);
				}
				break;
			case GLOBAL_NAME:
			case LOCAL_NAME:
				enterOuterAlt(_localctx, 2);
				{
				setState(654);
				((RetValsContext)_localctx).value = value();
				((RetValsContext)_localctx).vals.add(((RetValsContext)_localctx).value);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DestClauseContext extends ParserRuleContext {
		public BbContext bb() {
			return getRuleContext(BbContext.class,0);
		}
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public DestClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_destClause; }
	}

	public final DestClauseContext destClause() throws RecognitionException {
		DestClauseContext _localctx = new DestClauseContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_destClause);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(657);
			bb();
			setState(658);
			argList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BbContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public BbContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bb; }
	}

	public final BbContext bb() throws RecognitionException {
		BbContext _localctx = new BbContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_bb);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(660);
			name();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ValueContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_value; }
	}

	public final ValueContext value() throws RecognitionException {
		ValueContext _localctx = new ValueContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_value);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(662);
			name();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncCallBodyContext extends ParserRuleContext {
		public ValueContext callee;
		public FuncSigContext funcSig() {
			return getRuleContext(FuncSigContext.class,0);
		}
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public FuncCallBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcCallBody; }
	}

	public final FuncCallBodyContext funcCallBody() throws RecognitionException {
		FuncCallBodyContext _localctx = new FuncCallBodyContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_funcCallBody);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(664);
			match(T__4);
			setState(665);
			funcSig();
			setState(666);
			match(T__5);
			setState(667);
			((FuncCallBodyContext)_localctx).callee = value();
			setState(668);
			argList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExcClauseContext extends ParserRuleContext {
		public DestClauseContext nor;
		public DestClauseContext exc;
		public List<DestClauseContext> destClause() {
			return getRuleContexts(DestClauseContext.class);
		}
		public DestClauseContext destClause(int i) {
			return getRuleContext(DestClauseContext.class,i);
		}
		public ExcClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_excClause; }
	}

	public final ExcClauseContext excClause() throws RecognitionException {
		ExcClauseContext _localctx = new ExcClauseContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_excClause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(676);
			_la = _input.LA(1);
			if (_la==T__74) {
				{
				setState(670);
				match(T__74);
				setState(671);
				match(T__28);
				setState(672);
				((ExcClauseContext)_localctx).nor = destClause();
				setState(673);
				((ExcClauseContext)_localctx).exc = destClause();
				setState(674);
				match(T__29);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class KeepAliveClauseContext extends ParserRuleContext {
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public KeepAliveClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_keepAliveClause; }
	}

	public final KeepAliveClauseContext keepAliveClause() throws RecognitionException {
		KeepAliveClauseContext _localctx = new KeepAliveClauseContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_keepAliveClause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(687);
			_la = _input.LA(1);
			if (_la==T__75) {
				{
				setState(678);
				match(T__75);
				setState(679);
				match(T__28);
				setState(683);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
					{
					{
					setState(680);
					value();
					}
					}
					setState(685);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(686);
				match(T__29);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FlagListContext extends ParserRuleContext {
		public List<FlagContext> flag() {
			return getRuleContexts(FlagContext.class);
		}
		public FlagContext flag(int i) {
			return getRuleContext(FlagContext.class,i);
		}
		public FlagListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_flagList; }
	}

	public final FlagListContext flagList() throws RecognitionException {
		FlagListContext _localctx = new FlagListContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_flagList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(689);
			match(T__35);
			setState(693);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==FLAG) {
				{
				{
				setState(690);
				flag();
				}
				}
				setState(695);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(696);
			match(T__36);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeListContext extends ParserRuleContext {
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TypeListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeList; }
	}

	public final TypeListContext typeList() throws RecognitionException {
		TypeListContext _localctx = new TypeListContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_typeList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(698);
			match(T__4);
			setState(702);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME) {
				{
				{
				setState(699);
				type();
				}
				}
				setState(704);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(705);
			match(T__5);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncSigListContext extends ParserRuleContext {
		public List<FuncSigContext> funcSig() {
			return getRuleContexts(FuncSigContext.class);
		}
		public FuncSigContext funcSig(int i) {
			return getRuleContext(FuncSigContext.class,i);
		}
		public FuncSigListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcSigList; }
	}

	public final FuncSigListContext funcSigList() throws RecognitionException {
		FuncSigListContext _localctx = new FuncSigListContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_funcSigList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(707);
			match(T__76);
			setState(711);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME) {
				{
				{
				setState(708);
				funcSig();
				}
				}
				setState(713);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(714);
			match(T__77);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgListContext extends ParserRuleContext {
		public List<ValueContext> value() {
			return getRuleContexts(ValueContext.class);
		}
		public ValueContext value(int i) {
			return getRuleContext(ValueContext.class,i);
		}
		public ArgListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argList; }
	}

	public final ArgListContext argList() throws RecognitionException {
		ArgListContext _localctx = new ArgListContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_argList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(716);
			match(T__28);
			setState(720);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GLOBAL_NAME || _la==LOCAL_NAME) {
				{
				{
				setState(717);
				value();
				}
				}
				setState(722);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(723);
			match(T__29);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CurStackClauseContext extends ParserRuleContext {
		public CurStackClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_curStackClause; }
	 
		public CurStackClauseContext() { }
		public void copyFrom(CurStackClauseContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class CurStackRetWithContext extends CurStackClauseContext {
		public TypeListContext typeList() {
			return getRuleContext(TypeListContext.class,0);
		}
		public CurStackRetWithContext(CurStackClauseContext ctx) { copyFrom(ctx); }
	}
	public static class CurStackKillOldContext extends CurStackClauseContext {
		public CurStackKillOldContext(CurStackClauseContext ctx) { copyFrom(ctx); }
	}

	public final CurStackClauseContext curStackClause() throws RecognitionException {
		CurStackClauseContext _localctx = new CurStackClauseContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_curStackClause);
		try {
			setState(728);
			switch (_input.LA(1)) {
			case T__78:
				_localctx = new CurStackRetWithContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(725);
				match(T__78);
				setState(726);
				typeList();
				}
				break;
			case T__79:
				_localctx = new CurStackKillOldContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(727);
				match(T__79);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NewStackClauseContext extends ParserRuleContext {
		public NewStackClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_newStackClause; }
	 
		public NewStackClauseContext() { }
		public void copyFrom(NewStackClauseContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class NewStackThrowExcContext extends NewStackClauseContext {
		public ValueContext exc;
		public ValueContext value() {
			return getRuleContext(ValueContext.class,0);
		}
		public NewStackThrowExcContext(NewStackClauseContext ctx) { copyFrom(ctx); }
	}
	public static class NewStackPassValueContext extends NewStackClauseContext {
		public TypeListContext typeList() {
			return getRuleContext(TypeListContext.class,0);
		}
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public NewStackPassValueContext(NewStackClauseContext ctx) { copyFrom(ctx); }
	}

	public final NewStackClauseContext newStackClause() throws RecognitionException {
		NewStackClauseContext _localctx = new NewStackClauseContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_newStackClause);
		try {
			setState(736);
			switch (_input.LA(1)) {
			case T__80:
				_localctx = new NewStackPassValueContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(730);
				match(T__80);
				setState(731);
				typeList();
				setState(732);
				argList();
				}
				break;
			case T__81:
				_localctx = new NewStackThrowExcContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(734);
				match(T__81);
				setState(735);
				((NewStackThrowExcContext)_localctx).exc = value();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BinopContext extends ParserRuleContext {
		public BinopContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_binop; }
	}

	public final BinopContext binop() throws RecognitionException {
		BinopContext _localctx = new BinopContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_binop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(738);
			_la = _input.LA(1);
			if ( !(((((_la - 83)) & ~0x3f) == 0 && ((1L << (_la - 83)) & ((1L << (T__82 - 83)) | (1L << (T__83 - 83)) | (1L << (T__84 - 83)) | (1L << (T__85 - 83)) | (1L << (T__86 - 83)) | (1L << (T__87 - 83)) | (1L << (T__88 - 83)) | (1L << (T__89 - 83)) | (1L << (T__90 - 83)) | (1L << (T__91 - 83)) | (1L << (T__92 - 83)) | (1L << (T__93 - 83)) | (1L << (T__94 - 83)) | (1L << (T__95 - 83)) | (1L << (T__96 - 83)) | (1L << (T__97 - 83)) | (1L << (T__98 - 83)) | (1L << (T__99 - 83)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CmpopContext extends ParserRuleContext {
		public CmpopContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_cmpop; }
	}

	public final CmpopContext cmpop() throws RecognitionException {
		CmpopContext _localctx = new CmpopContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_cmpop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(740);
			_la = _input.LA(1);
			if ( !(((((_la - 101)) & ~0x3f) == 0 && ((1L << (_la - 101)) & ((1L << (T__100 - 101)) | (1L << (T__101 - 101)) | (1L << (T__102 - 101)) | (1L << (T__103 - 101)) | (1L << (T__104 - 101)) | (1L << (T__105 - 101)) | (1L << (T__106 - 101)) | (1L << (T__107 - 101)) | (1L << (T__108 - 101)) | (1L << (T__109 - 101)) | (1L << (T__110 - 101)) | (1L << (T__111 - 101)) | (1L << (T__112 - 101)) | (1L << (T__113 - 101)) | (1L << (T__114 - 101)) | (1L << (T__115 - 101)) | (1L << (T__116 - 101)) | (1L << (T__117 - 101)) | (1L << (T__118 - 101)) | (1L << (T__119 - 101)) | (1L << (T__120 - 101)) | (1L << (T__121 - 101)) | (1L << (T__122 - 101)) | (1L << (T__123 - 101)) | (1L << (T__124 - 101)) | (1L << (T__125 - 101)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConvopContext extends ParserRuleContext {
		public ConvopContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_convop; }
	}

	public final ConvopContext convop() throws RecognitionException {
		ConvopContext _localctx = new ConvopContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_convop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(742);
			_la = _input.LA(1);
			if ( !(((((_la - 127)) & ~0x3f) == 0 && ((1L << (_la - 127)) & ((1L << (T__126 - 127)) | (1L << (T__127 - 127)) | (1L << (T__128 - 127)) | (1L << (T__129 - 127)) | (1L << (T__130 - 127)) | (1L << (T__131 - 127)) | (1L << (T__132 - 127)) | (1L << (T__133 - 127)) | (1L << (T__134 - 127)) | (1L << (T__135 - 127)) | (1L << (T__136 - 127)) | (1L << (T__137 - 127)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MemordContext extends ParserRuleContext {
		public MemordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_memord; }
	}

	public final MemordContext memord() throws RecognitionException {
		MemordContext _localctx = new MemordContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_memord);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(744);
			_la = _input.LA(1);
			if ( !(((((_la - 139)) & ~0x3f) == 0 && ((1L << (_la - 139)) & ((1L << (T__138 - 139)) | (1L << (T__139 - 139)) | (1L << (T__140 - 139)) | (1L << (T__141 - 139)) | (1L << (T__142 - 139)) | (1L << (T__143 - 139)) | (1L << (T__144 - 139)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AtomicrmwopContext extends ParserRuleContext {
		public AtomicrmwopContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_atomicrmwop; }
	}

	public final AtomicrmwopContext atomicrmwop() throws RecognitionException {
		AtomicrmwopContext _localctx = new AtomicrmwopContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_atomicrmwop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(746);
			_la = _input.LA(1);
			if ( !(((((_la - 83)) & ~0x3f) == 0 && ((1L << (_la - 83)) & ((1L << (T__82 - 83)) | (1L << (T__83 - 83)) | (1L << (T__92 - 83)) | (1L << (T__93 - 83)) | (1L << (T__94 - 83)) | (1L << (T__145 - 83)))) != 0) || ((((_la - 147)) & ~0x3f) == 0 && ((1L << (_la - 147)) & ((1L << (T__146 - 147)) | (1L << (T__147 - 147)) | (1L << (T__148 - 147)) | (1L << (T__149 - 147)) | (1L << (T__150 - 147)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FlagContext extends ParserRuleContext {
		public TerminalNode FLAG() { return getToken(UIRParser.FLAG, 0); }
		public FlagContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_flag; }
	}

	public final FlagContext flag() throws RecognitionException {
		FlagContext _localctx = new FlagContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_flag);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(748);
			match(FLAG);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IntLiteralContext extends ParserRuleContext {
		public TerminalNode INT_DEC() { return getToken(UIRParser.INT_DEC, 0); }
		public TerminalNode INT_OCT() { return getToken(UIRParser.INT_OCT, 0); }
		public TerminalNode INT_HEX() { return getToken(UIRParser.INT_HEX, 0); }
		public IntLiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intLiteral; }
	}

	public final IntLiteralContext intLiteral() throws RecognitionException {
		IntLiteralContext _localctx = new IntLiteralContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_intLiteral);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(750);
			_la = _input.LA(1);
			if ( !(((((_la - 156)) & ~0x3f) == 0 && ((1L << (_la - 156)) & ((1L << (INT_DEC - 156)) | (1L << (INT_OCT - 156)) | (1L << (INT_HEX - 156)))) != 0)) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FloatLiteralContext extends ParserRuleContext {
		public FloatLiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_floatLiteral; }
	 
		public FloatLiteralContext() { }
		public void copyFrom(FloatLiteralContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class FloatBitsContext extends FloatLiteralContext {
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public FloatBitsContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class FloatNanContext extends FloatLiteralContext {
		public TerminalNode NAN() { return getToken(UIRParser.NAN, 0); }
		public FloatNanContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class FloatNumberContext extends FloatLiteralContext {
		public TerminalNode FP_NUM() { return getToken(UIRParser.FP_NUM, 0); }
		public FloatNumberContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class FloatInfContext extends FloatLiteralContext {
		public TerminalNode INF() { return getToken(UIRParser.INF, 0); }
		public FloatInfContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}

	public final FloatLiteralContext floatLiteral() throws RecognitionException {
		FloatLiteralContext _localctx = new FloatLiteralContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_floatLiteral);
		try {
			setState(763);
			switch (_input.LA(1)) {
			case FP_NUM:
				_localctx = new FloatNumberContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(752);
				match(FP_NUM);
				setState(753);
				match(T__151);
				}
				break;
			case INF:
				_localctx = new FloatInfContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(754);
				match(INF);
				setState(755);
				match(T__151);
				}
				break;
			case NAN:
				_localctx = new FloatNanContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(756);
				match(NAN);
				setState(757);
				match(T__151);
				}
				break;
			case T__152:
				_localctx = new FloatBitsContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(758);
				match(T__152);
				setState(759);
				match(T__28);
				setState(760);
				intLiteral();
				setState(761);
				match(T__29);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DoubleLiteralContext extends ParserRuleContext {
		public DoubleLiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_doubleLiteral; }
	 
		public DoubleLiteralContext() { }
		public void copyFrom(DoubleLiteralContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class DoubleBitsContext extends DoubleLiteralContext {
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public DoubleBitsContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class DoubleNumberContext extends DoubleLiteralContext {
		public TerminalNode FP_NUM() { return getToken(UIRParser.FP_NUM, 0); }
		public DoubleNumberContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class DoubleInfContext extends DoubleLiteralContext {
		public TerminalNode INF() { return getToken(UIRParser.INF, 0); }
		public DoubleInfContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class DoubleNanContext extends DoubleLiteralContext {
		public TerminalNode NAN() { return getToken(UIRParser.NAN, 0); }
		public DoubleNanContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}

	public final DoubleLiteralContext doubleLiteral() throws RecognitionException {
		DoubleLiteralContext _localctx = new DoubleLiteralContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_doubleLiteral);
		try {
			setState(776);
			switch (_input.LA(1)) {
			case FP_NUM:
				_localctx = new DoubleNumberContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(765);
				match(FP_NUM);
				setState(766);
				match(T__153);
				}
				break;
			case INF:
				_localctx = new DoubleInfContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(767);
				match(INF);
				setState(768);
				match(T__153);
				}
				break;
			case NAN:
				_localctx = new DoubleNanContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(769);
				match(NAN);
				setState(770);
				match(T__153);
				}
				break;
			case T__154:
				_localctx = new DoubleBitsContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(771);
				match(T__154);
				setState(772);
				match(T__28);
				setState(773);
				intLiteral();
				setState(774);
				match(T__29);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NameContext extends ParserRuleContext {
		public TerminalNode GLOBAL_NAME() { return getToken(UIRParser.GLOBAL_NAME, 0); }
		public TerminalNode LOCAL_NAME() { return getToken(UIRParser.LOCAL_NAME, 0); }
		public NameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_name; }
	}

	public final NameContext name() throws RecognitionException {
		NameContext _localctx = new NameContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_name);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(778);
			_la = _input.LA(1);
			if ( !(_la==GLOBAL_NAME || _la==LOCAL_NAME) ) {
			_errHandler.recoverInline(this);
			} else {
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\u00a8\u030f\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\3\2\7\2b\n\2\f\2\16\2e\13\2\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\5\3n\n\3\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3"+
		"\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3"+
		"\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\6\13\u00b7"+
		"\n\13\r\13\16\13\u00b8\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\7\13\u00c6\n\13\f\13\16\13\u00c9\13\13\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\5\13\u00e7\n\13\3\f\3"+
		"\f\7\f\u00eb\n\f\f\f\16\f\u00ee\13\f\3\f\3\f\3\f\3\f\7\f\u00f4\n\f\f\f"+
		"\16\f\u00f7\13\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r\7\r\u0100\n\r\f\r\16\r\u0103"+
		"\13\r\3\r\3\r\5\r\u0107\n\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3\21\7"+
		"\21\u0111\n\21\f\21\16\21\u0114\13\21\3\21\3\21\3\22\3\22\7\22\u011a\n"+
		"\22\f\22\16\22\u011d\13\22\3\22\3\22\3\23\3\23\6\23\u0123\n\23\r\23\16"+
		"\23\u0124\3\24\3\24\3\24\7\24\u012a\n\24\f\24\16\24\u012d\13\24\3\24\3"+
		"\24\5\24\u0131\n\24\3\24\3\24\3\25\3\25\3\25\3\25\3\25\3\26\3\26\3\26"+
		"\3\26\3\27\3\27\3\27\7\27\u0141\n\27\f\27\16\27\u0144\13\27\3\27\5\27"+
		"\u0147\n\27\3\30\3\30\3\30\5\30\u014c\n\30\3\30\3\30\3\30\3\30\5\30\u0152"+
		"\n\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\7\31\u0186\n\31\f\31\16"+
		"\31\u0189\13\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\5\31\u01e5\n\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\5\31\u01ef\n\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\5\31\u01fa\n\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\5\31\u0205\n\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\5\31\u020e"+
		"\n\31\3\31\5\31\u0211\n\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\5\31"+
		"\u021b\n\31\3\31\5\31\u021e\n\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3"+
		"\31\3\31\5\31\u0229\n\31\3\31\5\31\u022c\n\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\5\31\u023a\n\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\5\31\u0256\n\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\5\31\u0279\n\31\3\31\5\31\u027c\n\31\3\31\5\31\u027f\n\31\3"+
		"\31\5\31\u0282\n\31\3\31\3\31\3\31\5\31\u0287\n\31\3\32\3\32\7\32\u028b"+
		"\n\32\f\32\16\32\u028e\13\32\3\32\3\32\5\32\u0292\n\32\3\33\3\33\3\33"+
		"\3\34\3\34\3\35\3\35\3\36\3\36\3\36\3\36\3\36\3\36\3\37\3\37\3\37\3\37"+
		"\3\37\3\37\5\37\u02a7\n\37\3 \3 \3 \7 \u02ac\n \f \16 \u02af\13 \3 \5"+
		" \u02b2\n \3!\3!\7!\u02b6\n!\f!\16!\u02b9\13!\3!\3!\3\"\3\"\7\"\u02bf"+
		"\n\"\f\"\16\"\u02c2\13\"\3\"\3\"\3#\3#\7#\u02c8\n#\f#\16#\u02cb\13#\3"+
		"#\3#\3$\3$\7$\u02d1\n$\f$\16$\u02d4\13$\3$\3$\3%\3%\3%\5%\u02db\n%\3&"+
		"\3&\3&\3&\3&\3&\5&\u02e3\n&\3\'\3\'\3(\3(\3)\3)\3*\3*\3+\3+\3,\3,\3-\3"+
		"-\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\5.\u02fe\n.\3/\3/\3/\3/\3/\3/\3/\3"+
		"/\3/\3/\3/\5/\u030b\n/\3\60\3\60\3\60\2\2\61\2\4\6\b\n\f\16\20\22\24\26"+
		"\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJLNPRTVXZ\\^\2\t\3\2Uf\3\2g"+
		"\u0080\3\2\u0081\u008c\3\2\u008d\u0093\5\2UV_a\u0094\u0099\3\2\u009e\u00a0"+
		"\3\2\u00a4\u00a5\u034e\2c\3\2\2\2\4m\3\2\2\2\6o\3\2\2\2\bt\3\2\2\2\ny"+
		"\3\2\2\2\f\u0081\3\2\2\2\16\u0087\3\2\2\2\20\u008d\3\2\2\2\22\u0096\3"+
		"\2\2\2\24\u00e6\3\2\2\2\26\u00e8\3\2\2\2\30\u0106\3\2\2\2\32\u0108\3\2"+
		"\2\2\34\u010a\3\2\2\2\36\u010c\3\2\2\2 \u010e\3\2\2\2\"\u0117\3\2\2\2"+
		"$\u0120\3\2\2\2&\u0126\3\2\2\2(\u0134\3\2\2\2*\u0139\3\2\2\2,\u0146\3"+
		"\2\2\2.\u014b\3\2\2\2\60\u0286\3\2\2\2\62\u0291\3\2\2\2\64\u0293\3\2\2"+
		"\2\66\u0296\3\2\2\28\u0298\3\2\2\2:\u029a\3\2\2\2<\u02a6\3\2\2\2>\u02b1"+
		"\3\2\2\2@\u02b3\3\2\2\2B\u02bc\3\2\2\2D\u02c5\3\2\2\2F\u02ce\3\2\2\2H"+
		"\u02da\3\2\2\2J\u02e2\3\2\2\2L\u02e4\3\2\2\2N\u02e6\3\2\2\2P\u02e8\3\2"+
		"\2\2R\u02ea\3\2\2\2T\u02ec\3\2\2\2V\u02ee\3\2\2\2X\u02f0\3\2\2\2Z\u02fd"+
		"\3\2\2\2\\\u030a\3\2\2\2^\u030c\3\2\2\2`b\5\4\3\2a`\3\2\2\2be\3\2\2\2"+
		"ca\3\2\2\2cd\3\2\2\2d\3\3\2\2\2ec\3\2\2\2fn\5\6\4\2gn\5\b\5\2hn\5\n\6"+
		"\2in\5\f\7\2jn\5\16\b\2kn\5\20\t\2ln\5\22\n\2mf\3\2\2\2mg\3\2\2\2mh\3"+
		"\2\2\2mi\3\2\2\2mj\3\2\2\2mk\3\2\2\2ml\3\2\2\2n\5\3\2\2\2op\7\3\2\2pq"+
		"\7\u00a4\2\2qr\7\4\2\2rs\5\24\13\2s\7\3\2\2\2tu\7\5\2\2uv\7\u00a4\2\2"+
		"vw\7\4\2\2wx\5\26\f\2x\t\3\2\2\2yz\7\6\2\2z{\7\u00a4\2\2{|\7\7\2\2|}\5"+
		"\32\16\2}~\7\b\2\2~\177\7\4\2\2\177\u0080\5\30\r\2\u0080\13\3\2\2\2\u0081"+
		"\u0082\7\t\2\2\u0082\u0083\7\u00a4\2\2\u0083\u0084\7\7\2\2\u0084\u0085"+
		"\5\32\16\2\u0085\u0086\7\b\2\2\u0086\r\3\2\2\2\u0087\u0088\7\n\2\2\u0088"+
		"\u0089\7\u00a4\2\2\u0089\u008a\7\7\2\2\u008a\u008b\5\34\17\2\u008b\u008c"+
		"\7\b\2\2\u008c\17\3\2\2\2\u008d\u008e\7\13\2\2\u008e\u008f\7\u00a4\2\2"+
		"\u008f\u0090\7\f\2\2\u0090\u0091\5^\60\2\u0091\u0092\7\7\2\2\u0092\u0093"+
		"\5\34\17\2\u0093\u0094\7\b\2\2\u0094\u0095\5\"\22\2\u0095\21\3\2\2\2\u0096"+
		"\u0097\7\r\2\2\u0097\u0098\7\u00a4\2\2\u0098\u0099\7\4\2\2\u0099\u009a"+
		"\7\u00a4\2\2\u009a\u009b\5V,\2\u009b\u009c\7\u00a4\2\2\u009c\23\3\2\2"+
		"\2\u009d\u009e\7\16\2\2\u009e\u009f\7\7\2\2\u009f\u00a0\5X-\2\u00a0\u00a1"+
		"\7\b\2\2\u00a1\u00e7\3\2\2\2\u00a2\u00e7\7\17\2\2\u00a3\u00e7\7\20\2\2"+
		"\u00a4\u00a5\7\21\2\2\u00a5\u00a6\7\7\2\2\u00a6\u00a7\5\32\16\2\u00a7"+
		"\u00a8\7\b\2\2\u00a8\u00e7\3\2\2\2\u00a9\u00aa\7\22\2\2\u00aa\u00ab\7"+
		"\7\2\2\u00ab\u00ac\5\32\16\2\u00ac\u00ad\7\b\2\2\u00ad\u00e7\3\2\2\2\u00ae"+
		"\u00af\7\23\2\2\u00af\u00b0\7\7\2\2\u00b0\u00b1\5\32\16\2\u00b1\u00b2"+
		"\7\b\2\2\u00b2\u00e7\3\2\2\2\u00b3\u00b4\7\24\2\2\u00b4\u00b6\7\7\2\2"+
		"\u00b5\u00b7\5\32\16\2\u00b6\u00b5\3\2\2\2\u00b7\u00b8\3\2\2\2\u00b8\u00b6"+
		"\3\2\2\2\u00b8\u00b9\3\2\2\2\u00b9\u00ba\3\2\2\2\u00ba\u00bb\7\b\2\2\u00bb"+
		"\u00e7\3\2\2\2\u00bc\u00bd\7\25\2\2\u00bd\u00be\7\7\2\2\u00be\u00bf\5"+
		"\32\16\2\u00bf\u00c0\5X-\2\u00c0\u00c1\7\b\2\2\u00c1\u00e7\3\2\2\2\u00c2"+
		"\u00c3\7\26\2\2\u00c3\u00c7\7\7\2\2\u00c4\u00c6\5\32\16\2\u00c5\u00c4"+
		"\3\2\2\2\u00c6\u00c9\3\2\2\2\u00c7\u00c5\3\2\2\2\u00c7\u00c8\3\2\2\2\u00c8"+
		"\u00ca\3\2\2\2\u00c9\u00c7\3\2\2\2\u00ca\u00cb\5\32\16\2\u00cb\u00cc\7"+
		"\b\2\2\u00cc\u00e7\3\2\2\2\u00cd\u00e7\7\27\2\2\u00ce\u00cf\7\30\2\2\u00cf"+
		"\u00d0\7\7\2\2\u00d0\u00d1\5\34\17\2\u00d1\u00d2\7\b\2\2\u00d2\u00e7\3"+
		"\2\2\2\u00d3\u00e7\7\31\2\2\u00d4\u00e7\7\32\2\2\u00d5\u00e7\7\33\2\2"+
		"\u00d6\u00d7\7\34\2\2\u00d7\u00d8\7\7\2\2\u00d8\u00d9\5\32\16\2\u00d9"+
		"\u00da\5X-\2\u00da\u00db\7\b\2\2\u00db\u00e7\3\2\2\2\u00dc\u00dd\7\35"+
		"\2\2\u00dd\u00de\7\7\2\2\u00de\u00df\5\32\16\2\u00df\u00e0\7\b\2\2\u00e0"+
		"\u00e7\3\2\2\2\u00e1\u00e2\7\36\2\2\u00e2\u00e3\7\7\2\2\u00e3\u00e4\5"+
		"\34\17\2\u00e4\u00e5\7\b\2\2\u00e5\u00e7\3\2\2\2\u00e6\u009d\3\2\2\2\u00e6"+
		"\u00a2\3\2\2\2\u00e6\u00a3\3\2\2\2\u00e6\u00a4\3\2\2\2\u00e6\u00a9\3\2"+
		"\2\2\u00e6\u00ae\3\2\2\2\u00e6\u00b3\3\2\2\2\u00e6\u00bc\3\2\2\2\u00e6"+
		"\u00c2\3\2\2\2\u00e6\u00cd\3\2\2\2\u00e6\u00ce\3\2\2\2\u00e6\u00d3\3\2"+
		"\2\2\u00e6\u00d4\3\2\2\2\u00e6\u00d5\3\2\2\2\u00e6\u00d6\3\2\2\2\u00e6"+
		"\u00dc\3\2\2\2\u00e6\u00e1\3\2\2\2\u00e7\25\3\2\2\2\u00e8\u00ec\7\37\2"+
		"\2\u00e9\u00eb\5\32\16\2\u00ea\u00e9\3\2\2\2\u00eb\u00ee\3\2\2\2\u00ec"+
		"\u00ea\3\2\2\2\u00ec\u00ed\3\2\2\2\u00ed\u00ef\3\2\2\2\u00ee\u00ec\3\2"+
		"\2\2\u00ef\u00f0\7 \2\2\u00f0\u00f1\7!\2\2\u00f1\u00f5\7\37\2\2\u00f2"+
		"\u00f4\5\32\16\2\u00f3\u00f2\3\2\2\2\u00f4\u00f7\3\2\2\2\u00f5\u00f3\3"+
		"\2\2\2\u00f5\u00f6\3\2\2\2\u00f6\u00f8\3\2\2\2\u00f7\u00f5\3\2\2\2\u00f8"+
		"\u00f9\7 \2\2\u00f9\27\3\2\2\2\u00fa\u0107\5X-\2\u00fb\u0107\5Z.\2\u00fc"+
		"\u0107\5\\/\2\u00fd\u0101\7\"\2\2\u00fe\u0100\7\u00a4\2\2\u00ff\u00fe"+
		"\3\2\2\2\u0100\u0103\3\2\2\2\u0101\u00ff\3\2\2\2\u0101\u0102\3\2\2\2\u0102"+
		"\u0104\3\2\2\2\u0103\u0101\3\2\2\2\u0104\u0107\7#\2\2\u0105\u0107\7$\2"+
		"\2\u0106\u00fa\3\2\2\2\u0106\u00fb\3\2\2\2\u0106\u00fc\3\2\2\2\u0106\u00fd"+
		"\3\2\2\2\u0106\u0105\3\2\2\2\u0107\31\3\2\2\2\u0108\u0109\7\u00a4\2\2"+
		"\u0109\33\3\2\2\2\u010a\u010b\7\u00a4\2\2\u010b\35\3\2\2\2\u010c\u010d"+
		"\7\u00a4\2\2\u010d\37\3\2\2\2\u010e\u0112\7\37\2\2\u010f\u0111\5^\60\2"+
		"\u0110\u010f\3\2\2\2\u0111\u0114\3\2\2\2\u0112\u0110\3\2\2\2\u0112\u0113"+
		"\3\2\2\2\u0113\u0115\3\2\2\2\u0114\u0112\3\2\2\2\u0115\u0116\7 \2\2\u0116"+
		"!\3\2\2\2\u0117\u011b\7\"\2\2\u0118\u011a\5$\23\2\u0119\u0118\3\2\2\2"+
		"\u011a\u011d\3\2\2\2\u011b\u0119\3\2\2\2\u011b\u011c\3\2\2\2\u011c\u011e"+
		"\3\2\2\2\u011d\u011b\3\2\2\2\u011e\u011f\7#\2\2\u011f#\3\2\2\2\u0120\u0122"+
		"\5&\24\2\u0121\u0123\5.\30\2\u0122\u0121\3\2\2\2\u0123\u0124\3\2\2\2\u0124"+
		"\u0122\3\2\2\2\u0124\u0125\3\2\2\2\u0125%\3\2\2\2\u0126\u0127\5^\60\2"+
		"\u0127\u012b\7\37\2\2\u0128\u012a\5(\25\2\u0129\u0128\3\2\2\2\u012a\u012d"+
		"\3\2\2\2\u012b\u0129\3\2\2\2\u012b\u012c\3\2\2\2\u012c\u012e\3\2\2\2\u012d"+
		"\u012b\3\2\2\2\u012e\u0130\7 \2\2\u012f\u0131\5*\26\2\u0130\u012f\3\2"+
		"\2\2\u0130\u0131\3\2\2\2\u0131\u0132\3\2\2\2\u0132\u0133\7%\2\2\u0133"+
		"\'\3\2\2\2\u0134\u0135\7\7\2\2\u0135\u0136\5\32\16\2\u0136\u0137\7\b\2"+
		"\2\u0137\u0138\5^\60\2\u0138)\3\2\2\2\u0139\u013a\7&\2\2\u013a\u013b\5"+
		"^\60\2\u013b\u013c\7\'\2\2\u013c+\3\2\2\2\u013d\u0147\5^\60\2\u013e\u0142"+
		"\7\37\2\2\u013f\u0141\5^\60\2\u0140\u013f\3\2\2\2\u0141\u0144\3\2\2\2"+
		"\u0142\u0140\3\2\2\2\u0142\u0143\3\2\2\2\u0143\u0145\3\2\2\2\u0144\u0142"+
		"\3\2\2\2\u0145\u0147\7 \2\2\u0146\u013d\3\2\2\2\u0146\u013e\3\2\2\2\u0147"+
		"-\3\2\2\2\u0148\u0149\5,\27\2\u0149\u014a\7\4\2\2\u014a\u014c\3\2\2\2"+
		"\u014b\u0148\3\2\2\2\u014b\u014c\3\2\2\2\u014c\u0151\3\2\2\2\u014d\u014e"+
		"\7&\2\2\u014e\u014f\5^\60\2\u014f\u0150\7\'\2\2\u0150\u0152\3\2\2\2\u0151"+
		"\u014d\3\2\2\2\u0151\u0152\3\2\2\2\u0152\u0153\3\2\2\2\u0153\u0154\5\60"+
		"\31\2\u0154/\3\2\2\2\u0155\u0156\5L\'\2\u0156\u0157\7\7\2\2\u0157\u0158"+
		"\5\32\16\2\u0158\u0159\7\b\2\2\u0159\u015a\58\35\2\u015a\u015b\58\35\2"+
		"\u015b\u015c\5<\37\2\u015c\u0287\3\2\2\2\u015d\u015e\5N(\2\u015e\u015f"+
		"\7\7\2\2\u015f\u0160\5\32\16\2\u0160\u0161\7\b\2\2\u0161\u0162\58\35\2"+
		"\u0162\u0163\58\35\2\u0163\u0287\3\2\2\2\u0164\u0165\5P)\2\u0165\u0166"+
		"\7\7\2\2\u0166\u0167\5\32\16\2\u0167\u0168\5\32\16\2\u0168\u0169\7\b\2"+
		"\2\u0169\u016a\58\35\2\u016a\u0287\3\2\2\2\u016b\u016c\7(\2\2\u016c\u016d"+
		"\7\7\2\2\u016d\u016e\5\32\16\2\u016e\u016f\5\32\16\2\u016f\u0170\7\b\2"+
		"\2\u0170\u0171\58\35\2\u0171\u0172\58\35\2\u0172\u0173\58\35\2\u0173\u0287"+
		"\3\2\2\2\u0174\u0175\7)\2\2\u0175\u0287\5\64\33\2\u0176\u0177\7*\2\2\u0177"+
		"\u0178\58\35\2\u0178\u0179\5\64\33\2\u0179\u017a\5\64\33\2\u017a\u0287"+
		"\3\2\2\2\u017b\u017c\7+\2\2\u017c\u017d\7\7\2\2\u017d\u017e\5\32\16\2"+
		"\u017e\u017f\7\b\2\2\u017f\u0180\58\35\2\u0180\u0181\5\64\33\2\u0181\u0187"+
		"\7\"\2\2\u0182\u0183\58\35\2\u0183\u0184\5\64\33\2\u0184\u0186\3\2\2\2"+
		"\u0185\u0182\3\2\2\2\u0186\u0189\3\2\2\2\u0187\u0185\3\2\2\2\u0187\u0188"+
		"\3\2\2\2\u0188\u018a\3\2\2\2\u0189\u0187\3\2\2\2\u018a\u018b\7#\2\2\u018b"+
		"\u0287\3\2\2\2\u018c\u018d\7,\2\2\u018d\u018e\5:\36\2\u018e\u018f\5<\37"+
		"\2\u018f\u0190\5> \2\u0190\u0287\3\2\2\2\u0191\u0192\7-\2\2\u0192\u0287"+
		"\5:\36\2\u0193\u0194\7.\2\2\u0194\u0287\5\62\32\2\u0195\u0196\7/\2\2\u0196"+
		"\u0287\58\35\2\u0197\u0198\7\60\2\2\u0198\u0199\7\7\2\2\u0199\u019a\5"+
		"\32\16\2\u019a\u019b\5X-\2\u019b\u019c\7\b\2\2\u019c\u019d\58\35\2\u019d"+
		"\u0287\3\2\2\2\u019e\u019f\7\61\2\2\u019f\u01a0\7\7\2\2\u01a0\u01a1\5"+
		"\32\16\2\u01a1\u01a2\5X-\2\u01a2\u01a3\7\b\2\2\u01a3\u01a4\58\35\2\u01a4"+
		"\u01a5\58\35\2\u01a5\u0287\3\2\2\2\u01a6\u01a7\7\62\2\2\u01a7\u01a8\7"+
		"\7\2\2\u01a8\u01a9\5\32\16\2\u01a9\u01aa\5\32\16\2\u01aa\u01ab\7\b\2\2"+
		"\u01ab\u01ac\58\35\2\u01ac\u01ad\58\35\2\u01ad\u0287\3\2\2\2\u01ae\u01af"+
		"\7\63\2\2\u01af\u01b0\7\7\2\2\u01b0\u01b1\5\32\16\2\u01b1\u01b2\5\32\16"+
		"\2\u01b2\u01b3\7\b\2\2\u01b3\u01b4\58\35\2\u01b4\u01b5\58\35\2\u01b5\u01b6"+
		"\58\35\2\u01b6\u0287\3\2\2\2\u01b7\u01b8\7\64\2\2\u01b8\u01b9\7\7\2\2"+
		"\u01b9\u01ba\5\32\16\2\u01ba\u01bb\5\32\16\2\u01bb\u01bc\7\b\2\2\u01bc"+
		"\u01bd\58\35\2\u01bd\u01be\58\35\2\u01be\u01bf\58\35\2\u01bf\u0287\3\2"+
		"\2\2\u01c0\u01c1\7\65\2\2\u01c1\u01c2\7\7\2\2\u01c2\u01c3\5\32\16\2\u01c3"+
		"\u01c4\7\b\2\2\u01c4\u01c5\5<\37\2\u01c5\u0287\3\2\2\2\u01c6\u01c7\7\66"+
		"\2\2\u01c7\u01c8\7\7\2\2\u01c8\u01c9\5\32\16\2\u01c9\u01ca\5\32\16\2\u01ca"+
		"\u01cb\7\b\2\2\u01cb\u01cc\58\35\2\u01cc\u01cd\5<\37\2\u01cd\u0287\3\2"+
		"\2\2\u01ce\u01cf\7\67\2\2\u01cf\u01d0\7\7\2\2\u01d0\u01d1\5\32\16\2\u01d1"+
		"\u01d2\7\b\2\2\u01d2\u01d3\5<\37\2\u01d3\u0287\3\2\2\2\u01d4\u01d5\78"+
		"\2\2\u01d5\u01d6\7\7\2\2\u01d6\u01d7\5\32\16\2\u01d7\u01d8\5\32\16\2\u01d8"+
		"\u01d9\7\b\2\2\u01d9\u01da\58\35\2\u01da\u01db\5<\37\2\u01db\u0287\3\2"+
		"\2\2\u01dc\u01dd\79\2\2\u01dd\u01de\7\7\2\2\u01de\u01df\5\32\16\2\u01df"+
		"\u01e0\7\b\2\2\u01e0\u01e1\58\35\2\u01e1\u0287\3\2\2\2\u01e2\u01e4\7:"+
		"\2\2\u01e3\u01e5\7;\2\2\u01e4\u01e3\3\2\2\2\u01e4\u01e5\3\2\2\2\u01e5"+
		"\u01e6\3\2\2\2\u01e6\u01e7\7\7\2\2\u01e7\u01e8\5\32\16\2\u01e8\u01e9\5"+
		"X-\2\u01e9\u01ea\7\b\2\2\u01ea\u01eb\58\35\2\u01eb\u0287\3\2\2\2\u01ec"+
		"\u01ee\7<\2\2\u01ed\u01ef\7;\2\2\u01ee\u01ed\3\2\2\2\u01ee\u01ef\3\2\2"+
		"\2\u01ef\u01f0\3\2\2\2\u01f0\u01f1\7\7\2\2\u01f1\u01f2\5\32\16\2\u01f2"+
		"\u01f3\5\32\16\2\u01f3\u01f4\7\b\2\2\u01f4\u01f5\58\35\2\u01f5\u01f6\5"+
		"8\35\2\u01f6\u0287\3\2\2\2\u01f7\u01f9\7=\2\2\u01f8\u01fa\7;\2\2\u01f9"+
		"\u01f8\3\2\2\2\u01f9\u01fa\3\2\2\2\u01fa\u01fb\3\2\2\2\u01fb\u01fc\7\7"+
		"\2\2\u01fc\u01fd\5\32\16\2\u01fd\u01fe\5\32\16\2\u01fe\u01ff\7\b\2\2\u01ff"+
		"\u0200\58\35\2\u0200\u0201\58\35\2\u0201\u0287\3\2\2\2\u0202\u0204\7>"+
		"\2\2\u0203\u0205\7;\2\2\u0204\u0203\3\2\2\2\u0204\u0205\3\2\2\2\u0205"+
		"\u0206\3\2\2\2\u0206\u0207\7\7\2\2\u0207\u0208\5\32\16\2\u0208\u0209\7"+
		"\b\2\2\u0209\u020a\58\35\2\u020a\u0287\3\2\2\2\u020b\u020d\7?\2\2\u020c"+
		"\u020e\7;\2\2\u020d\u020c\3\2\2\2\u020d\u020e\3\2\2\2\u020e\u0210\3\2"+
		"\2\2\u020f\u0211\5R*\2\u0210\u020f\3\2\2\2\u0210\u0211\3\2\2\2\u0211\u0212"+
		"\3\2\2\2\u0212\u0213\7\7\2\2\u0213\u0214\5\32\16\2\u0214\u0215\7\b\2\2"+
		"\u0215\u0216\58\35\2\u0216\u0217\5<\37\2\u0217\u0287\3\2\2\2\u0218\u021a"+
		"\7@\2\2\u0219\u021b\7;\2\2\u021a\u0219\3\2\2\2\u021a\u021b\3\2\2\2\u021b"+
		"\u021d\3\2\2\2\u021c\u021e\5R*\2\u021d\u021c\3\2\2\2\u021d\u021e\3\2\2"+
		"\2\u021e\u021f\3\2\2\2\u021f\u0220\7\7\2\2\u0220\u0221\5\32\16\2\u0221"+
		"\u0222\7\b\2\2\u0222\u0223\58\35\2\u0223\u0224\58\35\2\u0224\u0225\5<"+
		"\37\2\u0225\u0287\3\2\2\2\u0226\u0228\7A\2\2\u0227\u0229\7;\2\2\u0228"+
		"\u0227\3\2\2\2\u0228\u0229\3\2\2\2\u0229\u022b\3\2\2\2\u022a\u022c\7B"+
		"\2\2\u022b\u022a\3\2\2\2\u022b\u022c\3\2\2\2\u022c\u022d\3\2\2\2\u022d"+
		"\u022e\5R*\2\u022e\u022f\5R*\2\u022f\u0230\7\7\2\2\u0230\u0231\5\32\16"+
		"\2\u0231\u0232\7\b\2\2\u0232\u0233\58\35\2\u0233\u0234\58\35\2\u0234\u0235"+
		"\58\35\2\u0235\u0236\5<\37\2\u0236\u0287\3\2\2\2\u0237\u0239\7C\2\2\u0238"+
		"\u023a\7;\2\2\u0239\u0238\3\2\2\2\u0239\u023a\3\2\2\2\u023a\u023b\3\2"+
		"\2\2\u023b\u023c\5R*\2\u023c\u023d\5T+\2\u023d\u023e\7\7\2\2\u023e\u023f"+
		"\5\32\16\2\u023f\u0240\7\b\2\2\u0240\u0241\58\35\2\u0241\u0242\58\35\2"+
		"\u0242\u0243\5<\37\2\u0243\u0287\3\2\2\2\u0244\u0245\7D\2\2\u0245\u0287"+
		"\5R*\2\u0246\u0247\7E\2\2\u0247\u0248\5B\"\2\u0248\u0249\5<\37\2\u0249"+
		"\u024a\5> \2\u024a\u0287\3\2\2\2\u024b\u024c\7F\2\2\u024c\u024d\5X-\2"+
		"\u024d\u024e\5B\"\2\u024e\u024f\5\64\33\2\u024f\u0255\5\64\33\2\u0250"+
		"\u0251\7G\2\2\u0251\u0252\7\37\2\2\u0252\u0253\5\64\33\2\u0253\u0254\7"+
		" \2\2\u0254\u0256\3\2\2\2\u0255\u0250\3\2\2\2\u0255\u0256\3\2\2\2\u0256"+
		"\u0257\3\2\2\2\u0257\u0258\5> \2\u0258\u0287\3\2\2\2\u0259\u025a\7H\2"+
		"\2\u025a\u025b\5X-\2\u025b\u025c\5\64\33\2\u025c\u025d\5\64\33\2\u025d"+
		"\u0287\3\2\2\2\u025e\u025f\7I\2\2\u025f\u0260\5V,\2\u0260\u0261\7\7\2"+
		"\2\u0261\u0262\5\32\16\2\u0262\u0263\5\34\17\2\u0263\u0264\7\b\2\2\u0264"+
		"\u0265\58\35\2\u0265\u0266\5F$\2\u0266\u0267\5<\37\2\u0267\u0268\5> \2"+
		"\u0268\u0287\3\2\2\2\u0269\u026a\7J\2\2\u026a\u026b\58\35\2\u026b\u026c"+
		"\5J&\2\u026c\u026d\5<\37\2\u026d\u0287\3\2\2\2\u026e\u026f\7K\2\2\u026f"+
		"\u0270\58\35\2\u0270\u0271\5H%\2\u0271\u0272\5J&\2\u0272\u0273\5<\37\2"+
		"\u0273\u0274\5> \2\u0274\u0287\3\2\2\2\u0275\u0276\7L\2\2\u0276\u0278"+
		"\7\u00a4\2\2\u0277\u0279\5@!\2\u0278\u0277\3\2\2\2\u0278\u0279\3\2\2\2"+
		"\u0279\u027b\3\2\2\2\u027a\u027c\5B\"\2\u027b\u027a\3\2\2\2\u027b\u027c"+
		"\3\2\2\2\u027c\u027e\3\2\2\2\u027d\u027f\5D#\2\u027e\u027d\3\2\2\2\u027e"+
		"\u027f\3\2\2\2\u027f\u0281\3\2\2\2\u0280\u0282\5F$\2\u0281\u0280\3\2\2"+
		"\2\u0281\u0282\3\2\2\2\u0282\u0283\3\2\2\2\u0283\u0284\5<\37\2\u0284\u0285"+
		"\5> \2\u0285\u0287\3\2\2\2\u0286\u0155\3\2\2\2\u0286\u015d\3\2\2\2\u0286"+
		"\u0164\3\2\2\2\u0286\u016b\3\2\2\2\u0286\u0174\3\2\2\2\u0286\u0176\3\2"+
		"\2\2\u0286\u017b\3\2\2\2\u0286\u018c\3\2\2\2\u0286\u0191\3\2\2\2\u0286"+
		"\u0193\3\2\2\2\u0286\u0195\3\2\2\2\u0286\u0197\3\2\2\2\u0286\u019e\3\2"+
		"\2\2\u0286\u01a6\3\2\2\2\u0286\u01ae\3\2\2\2\u0286\u01b7\3\2\2\2\u0286"+
		"\u01c0\3\2\2\2\u0286\u01c6\3\2\2\2\u0286\u01ce\3\2\2\2\u0286\u01d4\3\2"+
		"\2\2\u0286\u01dc\3\2\2\2\u0286\u01e2\3\2\2\2\u0286\u01ec\3\2\2\2\u0286"+
		"\u01f7\3\2\2\2\u0286\u0202\3\2\2\2\u0286\u020b\3\2\2\2\u0286\u0218\3\2"+
		"\2\2\u0286\u0226\3\2\2\2\u0286\u0237\3\2\2\2\u0286\u0244\3\2\2\2\u0286"+
		"\u0246\3\2\2\2\u0286\u024b\3\2\2\2\u0286\u0259\3\2\2\2\u0286\u025e\3\2"+
		"\2\2\u0286\u0269\3\2\2\2\u0286\u026e\3\2\2\2\u0286\u0275\3\2\2\2\u0287"+
		"\61\3\2\2\2\u0288\u028c\7\37\2\2\u0289\u028b\58\35\2\u028a\u0289\3\2\2"+
		"\2\u028b\u028e\3\2\2\2\u028c\u028a\3\2\2\2\u028c\u028d\3\2\2\2\u028d\u028f"+
		"\3\2\2\2\u028e\u028c\3\2\2\2\u028f\u0292\7 \2\2\u0290\u0292\58\35\2\u0291"+
		"\u0288\3\2\2\2\u0291\u0290\3\2\2\2\u0292\63\3\2\2\2\u0293\u0294\5\66\34"+
		"\2\u0294\u0295\5F$\2\u0295\65\3\2\2\2\u0296\u0297\5^\60\2\u0297\67\3\2"+
		"\2\2\u0298\u0299\5^\60\2\u02999\3\2\2\2\u029a\u029b\7\7\2\2\u029b\u029c"+
		"\5\34\17\2\u029c\u029d\7\b\2\2\u029d\u029e\58\35\2\u029e\u029f\5F$\2\u029f"+
		";\3\2\2\2\u02a0\u02a1\7M\2\2\u02a1\u02a2\7\37\2\2\u02a2\u02a3\5\64\33"+
		"\2\u02a3\u02a4\5\64\33\2\u02a4\u02a5\7 \2\2\u02a5\u02a7\3\2\2\2\u02a6"+
		"\u02a0\3\2\2\2\u02a6\u02a7\3\2\2\2\u02a7=\3\2\2\2\u02a8\u02a9\7N\2\2\u02a9"+
		"\u02ad\7\37\2\2\u02aa\u02ac\58\35\2\u02ab\u02aa\3\2\2\2\u02ac\u02af\3"+
		"\2\2\2\u02ad\u02ab\3\2\2\2\u02ad\u02ae\3\2\2\2\u02ae\u02b0\3\2\2\2\u02af"+
		"\u02ad\3\2\2\2\u02b0\u02b2\7 \2\2\u02b1\u02a8\3\2\2\2\u02b1\u02b2\3\2"+
		"\2\2\u02b2?\3\2\2\2\u02b3\u02b7\7&\2\2\u02b4\u02b6\5V,\2\u02b5\u02b4\3"+
		"\2\2\2\u02b6\u02b9\3\2\2\2\u02b7\u02b5\3\2\2\2\u02b7\u02b8\3\2\2\2\u02b8"+
		"\u02ba\3\2\2\2\u02b9\u02b7\3\2\2\2\u02ba\u02bb\7\'\2\2\u02bbA\3\2\2\2"+
		"\u02bc\u02c0\7\7\2\2\u02bd\u02bf\5\32\16\2\u02be\u02bd\3\2\2\2\u02bf\u02c2"+
		"\3\2\2\2\u02c0\u02be\3\2\2\2\u02c0\u02c1\3\2\2\2\u02c1\u02c3\3\2\2\2\u02c2"+
		"\u02c0\3\2\2\2\u02c3\u02c4\7\b\2\2\u02c4C\3\2\2\2\u02c5\u02c9\7O\2\2\u02c6"+
		"\u02c8\5\34\17\2\u02c7\u02c6\3\2\2\2\u02c8\u02cb\3\2\2\2\u02c9\u02c7\3"+
		"\2\2\2\u02c9\u02ca\3\2\2\2\u02ca\u02cc\3\2\2\2\u02cb\u02c9\3\2\2\2\u02cc"+
		"\u02cd\7P\2\2\u02cdE\3\2\2\2\u02ce\u02d2\7\37\2\2\u02cf\u02d1\58\35\2"+
		"\u02d0\u02cf\3\2\2\2\u02d1\u02d4\3\2\2\2\u02d2\u02d0\3\2\2\2\u02d2\u02d3"+
		"\3\2\2\2\u02d3\u02d5\3\2\2\2\u02d4\u02d2\3\2\2\2\u02d5\u02d6\7 \2\2\u02d6"+
		"G\3\2\2\2\u02d7\u02d8\7Q\2\2\u02d8\u02db\5B\"\2\u02d9\u02db\7R\2\2\u02da"+
		"\u02d7\3\2\2\2\u02da\u02d9\3\2\2\2\u02dbI\3\2\2\2\u02dc\u02dd\7S\2\2\u02dd"+
		"\u02de\5B\"\2\u02de\u02df\5F$\2\u02df\u02e3\3\2\2\2\u02e0\u02e1\7T\2\2"+
		"\u02e1\u02e3\58\35\2\u02e2\u02dc\3\2\2\2\u02e2\u02e0\3\2\2\2\u02e3K\3"+
		"\2\2\2\u02e4\u02e5\t\2\2\2\u02e5M\3\2\2\2\u02e6\u02e7\t\3\2\2\u02e7O\3"+
		"\2\2\2\u02e8\u02e9\t\4\2\2\u02e9Q\3\2\2\2\u02ea\u02eb\t\5\2\2\u02ebS\3"+
		"\2\2\2\u02ec\u02ed\t\6\2\2\u02edU\3\2\2\2\u02ee\u02ef\7\u00a6\2\2\u02ef"+
		"W\3\2\2\2\u02f0\u02f1\t\7\2\2\u02f1Y\3\2\2\2\u02f2\u02f3\7\u00a1\2\2\u02f3"+
		"\u02fe\7\u009a\2\2\u02f4\u02f5\7\u00a2\2\2\u02f5\u02fe\7\u009a\2\2\u02f6"+
		"\u02f7\7\u00a3\2\2\u02f7\u02fe\7\u009a\2\2\u02f8\u02f9\7\u009b\2\2\u02f9"+
		"\u02fa\7\37\2\2\u02fa\u02fb\5X-\2\u02fb\u02fc\7 \2\2\u02fc\u02fe\3\2\2"+
		"\2\u02fd\u02f2\3\2\2\2\u02fd\u02f4\3\2\2\2\u02fd\u02f6\3\2\2\2\u02fd\u02f8"+
		"\3\2\2\2\u02fe[\3\2\2\2\u02ff\u0300\7\u00a1\2\2\u0300\u030b\7\u009c\2"+
		"\2\u0301\u0302\7\u00a2\2\2\u0302\u030b\7\u009c\2\2\u0303\u0304\7\u00a3"+
		"\2\2\u0304\u030b\7\u009c\2\2\u0305\u0306\7\u009d\2\2\u0306\u0307\7\37"+
		"\2\2\u0307\u0308\5X-\2\u0308\u0309\7 \2\2\u0309\u030b\3\2\2\2\u030a\u02ff"+
		"\3\2\2\2\u030a\u0301\3\2\2\2\u030a\u0303\3\2\2\2\u030a\u0305\3\2\2\2\u030b"+
		"]\3\2\2\2\u030c\u030d\t\b\2\2\u030d_\3\2\2\2\63cm\u00b8\u00c7\u00e6\u00ec"+
		"\u00f5\u0101\u0106\u0112\u011b\u0124\u012b\u0130\u0142\u0146\u014b\u0151"+
		"\u0187\u01e4\u01ee\u01f9\u0204\u020d\u0210\u021a\u021d\u0228\u022b\u0239"+
		"\u0255\u0278\u027b\u027e\u0281\u0286\u028c\u0291\u02a6\u02ad\u02b1\u02b7"+
		"\u02c0\u02c9\u02d2\u02da\u02e2\u02fd\u030a";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}