// Generated from /home/andrew/Documents/git/MuBF/refimpl/src/main/antlr4/HAIL.g4 by ANTLR 4.5.1
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
public class HAILParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.5.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, INT_DEC=20, INT_OCT=21, INT_HEX=22, FP_NUM=23, INF=24, 
		NAN=25, GLOBAL_NAME=26, HAIL_NAME=27, WS=28, LINE_COMMENT=29;
	public static final int
		RULE_hail = 0, RULE_topLevelDef = 1, RULE_fixedAlloc = 2, RULE_hybridAlloc = 3, 
		RULE_memInit = 4, RULE_lValue = 5, RULE_rValue = 6, RULE_list = 7, RULE_index = 8, 
		RULE_intExpr = 9, RULE_type = 10, RULE_name = 11, RULE_intLiteral = 12, 
		RULE_floatLiteral = 13, RULE_doubleLiteral = 14;
	public static final String[] ruleNames = {
		"hail", "topLevelDef", "fixedAlloc", "hybridAlloc", "memInit", "lValue", 
		"rValue", "list", "index", "intExpr", "type", "name", "intLiteral", "floatLiteral", 
		"doubleLiteral"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'.new'", "'<'", "'>'", "'.newhybrid'", "'.init'", "'='", "'NULL'", 
		"'&'", "'*'", "'{'", "'}'", "'['", "']'", "'f'", "'bitsf'", "'('", "')'", 
		"'d'", "'bitsd'", null, null, null, null, null, "'nan'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, "INT_DEC", "INT_OCT", 
		"INT_HEX", "FP_NUM", "INF", "NAN", "GLOBAL_NAME", "HAIL_NAME", "WS", "LINE_COMMENT"
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
	public String getGrammarFileName() { return "HAIL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public HAILParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class HailContext extends ParserRuleContext {
		public List<TopLevelDefContext> topLevelDef() {
			return getRuleContexts(TopLevelDefContext.class);
		}
		public TopLevelDefContext topLevelDef(int i) {
			return getRuleContext(TopLevelDefContext.class,i);
		}
		public HailContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_hail; }
	}

	public final HailContext hail() throws RecognitionException {
		HailContext _localctx = new HailContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_hail);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(33);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__3) | (1L << T__4))) != 0)) {
				{
				{
				setState(30);
				topLevelDef();
				}
				}
				setState(35);
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
		public FixedAllocContext fixedAlloc() {
			return getRuleContext(FixedAllocContext.class,0);
		}
		public HybridAllocContext hybridAlloc() {
			return getRuleContext(HybridAllocContext.class,0);
		}
		public MemInitContext memInit() {
			return getRuleContext(MemInitContext.class,0);
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
			setState(39);
			switch (_input.LA(1)) {
			case T__0:
				enterOuterAlt(_localctx, 1);
				{
				setState(36);
				fixedAlloc();
				}
				break;
			case T__3:
				enterOuterAlt(_localctx, 2);
				{
				setState(37);
				hybridAlloc();
				}
				break;
			case T__4:
				enterOuterAlt(_localctx, 3);
				{
				setState(38);
				memInit();
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

	public static class FixedAllocContext extends ParserRuleContext {
		public Token nam;
		public TypeContext ty;
		public TerminalNode HAIL_NAME() { return getToken(HAILParser.HAIL_NAME, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public FixedAllocContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_fixedAlloc; }
	}

	public final FixedAllocContext fixedAlloc() throws RecognitionException {
		FixedAllocContext _localctx = new FixedAllocContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_fixedAlloc);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(41);
			match(T__0);
			setState(42);
			((FixedAllocContext)_localctx).nam = match(HAIL_NAME);
			setState(43);
			match(T__1);
			setState(44);
			((FixedAllocContext)_localctx).ty = type();
			setState(45);
			match(T__2);
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

	public static class HybridAllocContext extends ParserRuleContext {
		public Token nam;
		public TypeContext ty;
		public IntExprContext len;
		public TerminalNode HAIL_NAME() { return getToken(HAILParser.HAIL_NAME, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public IntExprContext intExpr() {
			return getRuleContext(IntExprContext.class,0);
		}
		public HybridAllocContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_hybridAlloc; }
	}

	public final HybridAllocContext hybridAlloc() throws RecognitionException {
		HybridAllocContext _localctx = new HybridAllocContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_hybridAlloc);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(47);
			match(T__3);
			setState(48);
			((HybridAllocContext)_localctx).nam = match(HAIL_NAME);
			setState(49);
			match(T__1);
			setState(50);
			((HybridAllocContext)_localctx).ty = type();
			setState(51);
			match(T__2);
			setState(52);
			((HybridAllocContext)_localctx).len = intExpr();
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

	public static class MemInitContext extends ParserRuleContext {
		public LValueContext lv;
		public RValueContext rv;
		public LValueContext lValue() {
			return getRuleContext(LValueContext.class,0);
		}
		public RValueContext rValue() {
			return getRuleContext(RValueContext.class,0);
		}
		public MemInitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_memInit; }
	}

	public final MemInitContext memInit() throws RecognitionException {
		MemInitContext _localctx = new MemInitContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_memInit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(54);
			match(T__4);
			setState(55);
			((MemInitContext)_localctx).lv = lValue();
			setState(56);
			match(T__5);
			setState(57);
			((MemInitContext)_localctx).rv = rValue();
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

	public static class LValueContext extends ParserRuleContext {
		public NameContext nam;
		public IndexContext index;
		public List<IndexContext> indices = new ArrayList<IndexContext>();
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public List<IndexContext> index() {
			return getRuleContexts(IndexContext.class);
		}
		public IndexContext index(int i) {
			return getRuleContext(IndexContext.class,i);
		}
		public LValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lValue; }
	}

	public final LValueContext lValue() throws RecognitionException {
		LValueContext _localctx = new LValueContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_lValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			((LValueContext)_localctx).nam = name();
			setState(63);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__11) {
				{
				{
				setState(60);
				((LValueContext)_localctx).index = index();
				((LValueContext)_localctx).indices.add(((LValueContext)_localctx).index);
				}
				}
				setState(65);
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

	public static class RValueContext extends ParserRuleContext {
		public RValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rValue; }
	 
		public RValueContext() { }
		public void copyFrom(RValueContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class RVIntContext extends RValueContext {
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public RVIntContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVGlobalContext extends RValueContext {
		public TerminalNode GLOBAL_NAME() { return getToken(HAILParser.GLOBAL_NAME, 0); }
		public RVGlobalContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVDoubleContext extends RValueContext {
		public DoubleLiteralContext doubleLiteral() {
			return getRuleContext(DoubleLiteralContext.class,0);
		}
		public RVDoubleContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVHailRefContext extends RValueContext {
		public TerminalNode HAIL_NAME() { return getToken(HAILParser.HAIL_NAME, 0); }
		public RVHailRefContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVValueOfContext extends RValueContext {
		public LValueContext lValue() {
			return getRuleContext(LValueContext.class,0);
		}
		public RVValueOfContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVFloatContext extends RValueContext {
		public FloatLiteralContext floatLiteral() {
			return getRuleContext(FloatLiteralContext.class,0);
		}
		public RVFloatContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVNullContext extends RValueContext {
		public RVNullContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVIRefOfContext extends RValueContext {
		public LValueContext lValue() {
			return getRuleContext(LValueContext.class,0);
		}
		public RVIRefOfContext(RValueContext ctx) { copyFrom(ctx); }
	}
	public static class RVListContext extends RValueContext {
		public ListContext list() {
			return getRuleContext(ListContext.class,0);
		}
		public RVListContext(RValueContext ctx) { copyFrom(ctx); }
	}

	public final RValueContext rValue() throws RecognitionException {
		RValueContext _localctx = new RValueContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_rValue);
		try {
			setState(77);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				_localctx = new RVGlobalContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(66);
				match(GLOBAL_NAME);
				}
				break;
			case 2:
				_localctx = new RVIntContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(67);
				intLiteral();
				}
				break;
			case 3:
				_localctx = new RVFloatContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(68);
				floatLiteral();
				}
				break;
			case 4:
				_localctx = new RVDoubleContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(69);
				doubleLiteral();
				}
				break;
			case 5:
				_localctx = new RVNullContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(70);
				match(T__6);
				}
				break;
			case 6:
				_localctx = new RVHailRefContext(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(71);
				match(HAIL_NAME);
				}
				break;
			case 7:
				_localctx = new RVIRefOfContext(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(72);
				match(T__7);
				setState(73);
				lValue();
				}
				break;
			case 8:
				_localctx = new RVValueOfContext(_localctx);
				enterOuterAlt(_localctx, 8);
				{
				setState(74);
				match(T__8);
				setState(75);
				lValue();
				}
				break;
			case 9:
				_localctx = new RVListContext(_localctx);
				enterOuterAlt(_localctx, 9);
				{
				setState(76);
				list();
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

	public static class ListContext extends ParserRuleContext {
		public RValueContext rValue;
		public List<RValueContext> rv = new ArrayList<RValueContext>();
		public List<RValueContext> rValue() {
			return getRuleContexts(RValueContext.class);
		}
		public RValueContext rValue(int i) {
			return getRuleContext(RValueContext.class,i);
		}
		public ListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_list; }
	}

	public final ListContext list() throws RecognitionException {
		ListContext _localctx = new ListContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_list);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(79);
			match(T__9);
			setState(83);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__6) | (1L << T__7) | (1L << T__8) | (1L << T__9) | (1L << T__14) | (1L << T__18) | (1L << INT_DEC) | (1L << INT_OCT) | (1L << INT_HEX) | (1L << FP_NUM) | (1L << INF) | (1L << NAN) | (1L << GLOBAL_NAME) | (1L << HAIL_NAME))) != 0)) {
				{
				{
				setState(80);
				((ListContext)_localctx).rValue = rValue();
				((ListContext)_localctx).rv.add(((ListContext)_localctx).rValue);
				}
				}
				setState(85);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(86);
			match(T__10);
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

	public static class IndexContext extends ParserRuleContext {
		public IntExprContext intExpr() {
			return getRuleContext(IntExprContext.class,0);
		}
		public IndexContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_index; }
	}

	public final IndexContext index() throws RecognitionException {
		IndexContext _localctx = new IndexContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_index);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(88);
			match(T__11);
			setState(89);
			intExpr();
			setState(90);
			match(T__12);
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

	public static class IntExprContext extends ParserRuleContext {
		public IntExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intExpr; }
	 
		public IntExprContext() { }
		public void copyFrom(IntExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class IntGlobalContext extends IntExprContext {
		public TerminalNode GLOBAL_NAME() { return getToken(HAILParser.GLOBAL_NAME, 0); }
		public IntGlobalContext(IntExprContext ctx) { copyFrom(ctx); }
	}
	public static class IntLitContext extends IntExprContext {
		public IntLiteralContext intLiteral() {
			return getRuleContext(IntLiteralContext.class,0);
		}
		public IntLitContext(IntExprContext ctx) { copyFrom(ctx); }
	}

	public final IntExprContext intExpr() throws RecognitionException {
		IntExprContext _localctx = new IntExprContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_intExpr);
		try {
			setState(94);
			switch (_input.LA(1)) {
			case INT_DEC:
			case INT_OCT:
			case INT_HEX:
				_localctx = new IntLitContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(92);
				intLiteral();
				}
				break;
			case GLOBAL_NAME:
				_localctx = new IntGlobalContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(93);
				match(GLOBAL_NAME);
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

	public static class TypeContext extends ParserRuleContext {
		public TerminalNode GLOBAL_NAME() { return getToken(HAILParser.GLOBAL_NAME, 0); }
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_type);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(96);
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

	public static class NameContext extends ParserRuleContext {
		public TerminalNode GLOBAL_NAME() { return getToken(HAILParser.GLOBAL_NAME, 0); }
		public TerminalNode HAIL_NAME() { return getToken(HAILParser.HAIL_NAME, 0); }
		public NameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_name; }
	}

	public final NameContext name() throws RecognitionException {
		NameContext _localctx = new NameContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_name);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(98);
			_la = _input.LA(1);
			if ( !(_la==GLOBAL_NAME || _la==HAIL_NAME) ) {
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

	public static class IntLiteralContext extends ParserRuleContext {
		public TerminalNode INT_DEC() { return getToken(HAILParser.INT_DEC, 0); }
		public TerminalNode INT_OCT() { return getToken(HAILParser.INT_OCT, 0); }
		public TerminalNode INT_HEX() { return getToken(HAILParser.INT_HEX, 0); }
		public IntLiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intLiteral; }
	}

	public final IntLiteralContext intLiteral() throws RecognitionException {
		IntLiteralContext _localctx = new IntLiteralContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_intLiteral);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(100);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << INT_DEC) | (1L << INT_OCT) | (1L << INT_HEX))) != 0)) ) {
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
		public TerminalNode NAN() { return getToken(HAILParser.NAN, 0); }
		public FloatNanContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class FloatNumberContext extends FloatLiteralContext {
		public TerminalNode FP_NUM() { return getToken(HAILParser.FP_NUM, 0); }
		public FloatNumberContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class FloatInfContext extends FloatLiteralContext {
		public TerminalNode INF() { return getToken(HAILParser.INF, 0); }
		public FloatInfContext(FloatLiteralContext ctx) { copyFrom(ctx); }
	}

	public final FloatLiteralContext floatLiteral() throws RecognitionException {
		FloatLiteralContext _localctx = new FloatLiteralContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_floatLiteral);
		try {
			setState(113);
			switch (_input.LA(1)) {
			case FP_NUM:
				_localctx = new FloatNumberContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(102);
				match(FP_NUM);
				setState(103);
				match(T__13);
				}
				break;
			case INF:
				_localctx = new FloatInfContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(104);
				match(INF);
				setState(105);
				match(T__13);
				}
				break;
			case NAN:
				_localctx = new FloatNanContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(106);
				match(NAN);
				setState(107);
				match(T__13);
				}
				break;
			case T__14:
				_localctx = new FloatBitsContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(108);
				match(T__14);
				setState(109);
				match(T__15);
				setState(110);
				intLiteral();
				setState(111);
				match(T__16);
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
		public TerminalNode FP_NUM() { return getToken(HAILParser.FP_NUM, 0); }
		public DoubleNumberContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class DoubleInfContext extends DoubleLiteralContext {
		public TerminalNode INF() { return getToken(HAILParser.INF, 0); }
		public DoubleInfContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}
	public static class DoubleNanContext extends DoubleLiteralContext {
		public TerminalNode NAN() { return getToken(HAILParser.NAN, 0); }
		public DoubleNanContext(DoubleLiteralContext ctx) { copyFrom(ctx); }
	}

	public final DoubleLiteralContext doubleLiteral() throws RecognitionException {
		DoubleLiteralContext _localctx = new DoubleLiteralContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_doubleLiteral);
		try {
			setState(126);
			switch (_input.LA(1)) {
			case FP_NUM:
				_localctx = new DoubleNumberContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(115);
				match(FP_NUM);
				setState(116);
				match(T__17);
				}
				break;
			case INF:
				_localctx = new DoubleInfContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(117);
				match(INF);
				setState(118);
				match(T__17);
				}
				break;
			case NAN:
				_localctx = new DoubleNanContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(119);
				match(NAN);
				setState(120);
				match(T__17);
				}
				break;
			case T__18:
				_localctx = new DoubleBitsContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(121);
				match(T__18);
				setState(122);
				match(T__15);
				setState(123);
				intLiteral();
				setState(124);
				match(T__16);
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

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\37\u0083\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\3\2\7\2\"\n\2\f\2"+
		"\16\2%\13\2\3\3\3\3\3\3\5\3*\n\3\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\7\3\7\7\7@\n\7\f\7\16\7C\13\7\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\bP\n\b\3\t\3\t\7\tT\n\t\f\t"+
		"\16\tW\13\t\3\t\3\t\3\n\3\n\3\n\3\n\3\13\3\13\5\13a\n\13\3\f\3\f\3\r\3"+
		"\r\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\5"+
		"\17t\n\17\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\5\20"+
		"\u0081\n\20\3\20\2\2\21\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36\2\4\3\2"+
		"\34\35\3\2\26\30\u0087\2#\3\2\2\2\4)\3\2\2\2\6+\3\2\2\2\b\61\3\2\2\2\n"+
		"8\3\2\2\2\f=\3\2\2\2\16O\3\2\2\2\20Q\3\2\2\2\22Z\3\2\2\2\24`\3\2\2\2\26"+
		"b\3\2\2\2\30d\3\2\2\2\32f\3\2\2\2\34s\3\2\2\2\36\u0080\3\2\2\2 \"\5\4"+
		"\3\2! \3\2\2\2\"%\3\2\2\2#!\3\2\2\2#$\3\2\2\2$\3\3\2\2\2%#\3\2\2\2&*\5"+
		"\6\4\2\'*\5\b\5\2(*\5\n\6\2)&\3\2\2\2)\'\3\2\2\2)(\3\2\2\2*\5\3\2\2\2"+
		"+,\7\3\2\2,-\7\35\2\2-.\7\4\2\2./\5\26\f\2/\60\7\5\2\2\60\7\3\2\2\2\61"+
		"\62\7\6\2\2\62\63\7\35\2\2\63\64\7\4\2\2\64\65\5\26\f\2\65\66\7\5\2\2"+
		"\66\67\5\24\13\2\67\t\3\2\2\289\7\7\2\29:\5\f\7\2:;\7\b\2\2;<\5\16\b\2"+
		"<\13\3\2\2\2=A\5\30\r\2>@\5\22\n\2?>\3\2\2\2@C\3\2\2\2A?\3\2\2\2AB\3\2"+
		"\2\2B\r\3\2\2\2CA\3\2\2\2DP\7\34\2\2EP\5\32\16\2FP\5\34\17\2GP\5\36\20"+
		"\2HP\7\t\2\2IP\7\35\2\2JK\7\n\2\2KP\5\f\7\2LM\7\13\2\2MP\5\f\7\2NP\5\20"+
		"\t\2OD\3\2\2\2OE\3\2\2\2OF\3\2\2\2OG\3\2\2\2OH\3\2\2\2OI\3\2\2\2OJ\3\2"+
		"\2\2OL\3\2\2\2ON\3\2\2\2P\17\3\2\2\2QU\7\f\2\2RT\5\16\b\2SR\3\2\2\2TW"+
		"\3\2\2\2US\3\2\2\2UV\3\2\2\2VX\3\2\2\2WU\3\2\2\2XY\7\r\2\2Y\21\3\2\2\2"+
		"Z[\7\16\2\2[\\\5\24\13\2\\]\7\17\2\2]\23\3\2\2\2^a\5\32\16\2_a\7\34\2"+
		"\2`^\3\2\2\2`_\3\2\2\2a\25\3\2\2\2bc\7\34\2\2c\27\3\2\2\2de\t\2\2\2e\31"+
		"\3\2\2\2fg\t\3\2\2g\33\3\2\2\2hi\7\31\2\2it\7\20\2\2jk\7\32\2\2kt\7\20"+
		"\2\2lm\7\33\2\2mt\7\20\2\2no\7\21\2\2op\7\22\2\2pq\5\32\16\2qr\7\23\2"+
		"\2rt\3\2\2\2sh\3\2\2\2sj\3\2\2\2sl\3\2\2\2sn\3\2\2\2t\35\3\2\2\2uv\7\31"+
		"\2\2v\u0081\7\24\2\2wx\7\32\2\2x\u0081\7\24\2\2yz\7\33\2\2z\u0081\7\24"+
		"\2\2{|\7\25\2\2|}\7\22\2\2}~\5\32\16\2~\177\7\23\2\2\177\u0081\3\2\2\2"+
		"\u0080u\3\2\2\2\u0080w\3\2\2\2\u0080y\3\2\2\2\u0080{\3\2\2\2\u0081\37"+
		"\3\2\2\2\n#)AOU`s\u0080";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}