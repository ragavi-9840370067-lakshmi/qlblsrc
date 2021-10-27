000100000000       IDENTIFICATION DIVISION.
000200000000       PROGRAM-ID.     STMTDUP.
000300000000       INSTALLATION.   Jewelstone Systems Inc.
000400000000       AUTHOR.         Jewelstone Systems Inc.
000500000000       DATE-WRITTEN.   May 8, 1997.
000600000000       DATE-COMPILED.
000700000000
000800000000      ******************************************************************
000900000000      *                                                                *
001000000000      *    STMTDUP:    Statement Duplicate Mailing Generation          *
001100000000      *                                                                *
001200000000      *    PURPOSE:    To format and create the Statement Duplicate    *
001300000000      *                Mailing file (MFASTMDFP).                       *
001400000000      *                                                                *
001500000000      *    LEVELS:      1. MAIL_NO     Mail Number                     *
001600000000      *                                                                *
001700000000      *    FILES:      (I) MFAADDP     Address                         *
001800000000      *                (I) MFACOUP     Country                         *
001900000000      *                (I) MFAMLSDTLA  Mailing List Detail             *
002000000000      *                (I) MFAMLSHDP   Mailing List Header             *
002100000000      *                (I) MFASTMEXP   Statement Extract               *
002200000000      *                (O) STMDUP1     Statement Duplicate Mailing     *
002300000000      *                (O) PRINTER     Control Report                  *
002400000000      *                                                                *
002500000000      *    CREATED:    07/JUL/97 (MFH)                                 *
002600971212      *    CHANGED:    21/DEC/97 (MFH)                                 *
002700000000      *                                                                *
002800000000      ******************************************************************
002900000000      /
003000000000      ******************************************************************
003100000000      *                                                                *
003200000000      *    C H A N G E   H I S T O R Y                                 *
003300000000      *                                                                *
003400000000      ******************************************************************
003500000000      ******************************************************************
003600000000      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE              *
003700000000      ******************************************************************
003800971024      *M.Howorth   *  1997/10/24  * RFS LOG #1502, #1544, #1549        *
003900971024      *            *              * - Modifications to use the new LDA *
004000971024      *            *              *   copy book (CPYLDASTMT).          *
004100030822      ******************************************************************
004200971212      *M.Howorth   *  1997/12/12  * RFS LOG #1849                      *
004300971212      *            *              * - Modifications to format          *
004400971212      *            *              *   addresses for Quebec as street   *
004500971212      *            *              *   number, street type code, street *
004600971212      *            *              *   name instead of street number,   *
004700971212      *            *              *   street name, street type code.   *
004800030822      ******************************************************************
004900981028      *H.SHENG     *  1998/10/28  * RFS 2939                           *
005000981028      *            *              * - Modifications to produce         *
005100981028      *            *              *   duplicate mailing record for     *
005200981028      *            *              *   confirm when form code on        *
005300981028      *            *              *   INVESTOR ADDITIONAL MAILING      *
005400981028      *            *              *   screen is equal to "CONFIRM"     *
005500030822      ******************************************************************
005600000803      *B.LU        *  2000/08/03  * RFS 7485                           *
005700000803      *            *              * - Populate the EOR-INDICATOR       *
005800000803      *            *              *   field with a "X".                *
005900030822      ******************************************************************
006000011022      * B.Lu       * 2001/10/22   * RFS10901, 11127, 10605
006100011022      *            *              * -Recompile only.
006200030822      ******************************************************************
006300011123      * A.ALNEMER  * 2001/11/23   * RFS12150
006400011123      *            *              * -Recompile only.
006500030822      ******************************************************************
006600020402      * B.LU       * 2002/01/30   * RFS12102
006700020402      *            *              * -Recompile only.
006800030822      ******************************************************************
006900020326      * A.ALNEMER  * 2002/03/26   * RFS12173
007000020326      *            *              * -Recompile only.
007100030822      ******************************************************************
007200020404      * A.ALNEMER  * 2002/04/04   * RFS13184
007300020404      *            *              * -Recompile only.
007400030822      ******************************************************************
007500020416      * A.ALNEMER  * 2002/04/16   * RFS13211
007600020416      *            *              * -Recompile only.
007700030822      ******************************************************************
007800020614      * A.ALNEMER  * 2002/06/14   * RFS13756
007900020614      *            *              * -Recompile only.
008000030822      ******************************************************************
008100020821      * A.ALNEMER  * 2002/08/21   * RFS13955
008200020821      *            *              * -Recompile only.
008300030822      ******************************************************************
008400020821      * A.ALNEMER  * 2002/08/21   * RFS14136
008500020821      *            *              * -Recompile only.
008600030822      ******************************************************************
008700020925      * B.LU       * 2002/09/25   * RFS14556  Recompile.
008800030822      ******************************************************************
008900021202      * A.ALNEMER  * 2002/12/02   * RFS15022
009000021202      *            *              * -Recompile only.
009100030822      ******************************************************************
009200030116      * Ping Fan   * 2003/01/16   * RFS 16020 - Recompile for          *
009300030116      *            *              *  MAIL-LIST-HEADER (MFAMLSHDP)      *
009400030116      *            *              *  MAIL-LIST-DETAILS(MFAMLSDTP)      *
009500030822      ******************************************************************
009600030630      * B.LU       * 2003/03/20   * RFS16256  Recompile.
009700030822      ******************************************************************
009800030630      * B.LU       * 2003/03/25   * RFS16633  Recompile.
009900030822      ******************************************************************
010000030630      * B.LU       * 2002/09/25   * RFS17789  ALLOW DUPLICATE MAILING
010100030630      *            *              *   FOR CONFIRMS.                    *
010200000000      ******************************************************************
010300030702      * Odette M   * 2003/05/08   * RFS 17165 - MFASTMEXP expanded from*
010400030702      *            *              * 1100 to 1350.                      *
010500030702      ******************************************************************
010600030702      * Odette     * 2003/06/04   * RFS 16881 - Recompile.             *
010700030702      * McMillan   *              * RFS 17217                          *
010800030702      ******************************************************************
010900030822      * Odette     * 2003/08/22   * RFS18091  Recompile for addition
011000030822      * McMillan   *              * of Section 17 to CPYJSISTMT.
011100030822      ******************************************************************
011200030825      * Odette     * 2003/08/25   * RFS17380  Recompile for additional
011300030825      * McMillan   *              * fields in Section 05 of CPYJSISTMT.
011400030825      ******************************************************************
011500031003      * Odette     * 2003/09/22   * RFS 18653 - Recompile.
011600031003      * McMillan   *              * Moved Section 17 rec counter field
011700031003      *            *              * in Section 99 to bottom of rec.
011800031003      ******************************************************************
011900031002      * B.LU       * 2003/10/02   * RFS17527  Recompile.
012000031002      ******************************************************************
012100031015      * ALI.A      * 2003/10/15   * RFS17626  Recompile.
012200031015      ******************************************************************
012300031020      * ALI.A      * 2003/10/20   * RFS18667  Recompile.
012400031020      ******************************************************************
012500031021      * ALI.A      * 2003/10/21   * RFS18312
012600031021      *            *              * Add new fields to the Header record
012700031125      ******************************************************************
012800031125      * ALI.A      * 2003/11/21   * RFS18515  Recompile.
012900031125      ******************************************************************
013000031125      * ALI.A      * 2003/11/21   * RFS18814  Recompile.
013100031124      ******************************************************************
013200040217      * ALI.A      * 2004/02/17   * RFS19444  Recompile.
013300040217      ******************************************************************
013400040406      * ALI.A      * 2004/04/06   * RFS20870  Recompile.
013500040406      ******************************************************************
013600040414      * ALI.A      * 2004/04/14   * RFS20758  Recompile.
013700040414      ******************************************************************
013800040514      * Odette     *  2004/04/27  * RFS21714 - Recompile
013900040514      * Aguinaldo- *              * Add new fields to Section 10 to
014000040514      * McMillan   *              * indicate if Dealer, Rep or Nominee
014100040514      *            *              * is/isn't suppressed.
014200040531      ******************************************************************
014300040531      * Reece Tam  * 2004/05/31   * RFS20804  Recompile - CPYLADASTMT.
014400040514      ******************************************************************
014500040908      * B.LU       * 2004/09/08   * RFS18924 -RECOMPILE - CPYJSISTMT.
014600040908      ******************************************************************
014700041012      * B.LU       * 2004/10/12   * RFS22856 -RECOMPILE - CPYJSISTMT.
014800041012      ******************************************************************
014900041025      * ALI.A      * 2004/10/25   * RFS18780 -RECOMPILE - CPYJSISTMT.
015000041025      ******************************************************************
015100041028      * ALI.A      * 2004/10/28   * RFS21715 -RECOMPILE - CPYJSISTMT.
015200041028      ******************************************************************
015300041220      * B.LU       * 2004/12/20   * RFS24514 -RECOMPILE - CPYJSISTMT
015400041220      ******************************************************************
015500050429      * CHANDANA R * 2005/03/09   * RFS26213 -RECOMPILE - CPYJSISTMT
015600050429      ******************************************************************
015700051002      * ADE ADEYEMI* 2005/09/08   * RFS29550 -RECOMPILE - CPYJSISTMT
015800051002      ******************************************************************
015900051002      * ADE ADEYEMI* 2005/09/28   * RFS30065 -RECOMPILE - CPYJSISTMT
016000051002      ******************************************************************
016100050921      * Ali.A      * 2005/09/18   * RFS28931 -RECOMPILE - CPYJSISTMT
016200050920      ******************************************************************
016300050926      * C.LENTINI  * 2005/09/26   * RFS28692 -RECOMPILE - CPYJSISTMT
016400050926      ******************************************************************
016500050926      * GEETA S.   * 2005/09/15   * RFS 28107: RECOMPILE FOR CPYJSISTMT
016600060327      ******************************************************************
016700060327      * Esin A.    * 2006/03/23   * RFS 31514: RECOMPILE FOR CPYJSISTMT
016800060821      ******************************************************************
016900060821      * Raymond Mui* 2006/08/21   * RFS 34848: RECOMPILE FOR CPYJSISTMT
017000060821      *            *              *            and CPYLDASTMT.
017100050926      ******************************************************************
017200061024      * L. Wong    * 2006/10/24   * RFS36342 - Recompile - CPYJSISTMT,
017300061024      *            *              *                        CPYLDASTMT.
017400080620      ******************************************************************
017500080620      * Bathuu L   * 2008/06/20   * RFS50993 - RECOMPILE FOR CPYJSISTMT
017600061024      ******************************************************************
017700091111      * A. Lo      * 2009/11/11   * RFS69515 - Recompile - CPYJSISTMT,
017800091111      *            *              *                        CPYLDASTMT.
017900091111      ******************************************************************
018000100308      * A. Lo      * 2010/03/08   * RFS78161 -Recompile - CPYJSISTMT.
018100100308      ***************************************************************
018200100409      * A. Lo      * 2010/03/08   * RFS74626 -Recompile - CPYJSISTMT.
018300100308      ***************************************************************
018400120127      * Rekha S    * 2012/01/27   * RFS93662 -Recompile - CPYJSISTMT.
018500120912      *****************************************************************
018600120912      * Suresh G.  * 2012/09/12   * RFS112003-Recompile for CPYJSISTMT.
018700120912      *****************************************************************
018800130408      * Andy Chan  * 2013/04/08   * RFS117038-Recompile for CPYJSISTMT.
018900130408      *****************************************************************
019000130529      * NOBLE T    * 2013/05/29   * RFS121116-RECOMPILE FOR CPYLDASTMT.
019100130529      *****************************************************************
019200130905      * PRAVEEN E  * 2013/09/05   *RFS123925 RECOMPIL ONLY - CPYJSISTMT.
019300140519      * NOBLE T    * 2014/05/02   * RFS130239 - RECOMPILE - CPYJSISTMT.
019400140418      *****************************************************************
019500140418      *SANGEETHA V  * 2014/03/25 * RFS129378 - RECOMPILE FOR CPYJSISTMT*
019600130905      *****************************************************************
019700140706      * Suresh G.   * 2014/07/05 * RFS132121 - RECOMPILE FOR CPYJSISTMT.
019800140706      *****************************************************************
019900140814      * Suresh G.   * 2014/08/14 * RFS133489 - RECOMPILE FOR CPYJSISTMT.
020000150216      * POOJA K     * 2015/02/16 * RFS 143392 - RECOMPILE FOR CPYJSISTMT
020100150330      * POOJA K     * 2015/03/27 * RFS 144030 - RECOMPILE FOR CPYJSISTMT
020200150424      * POOJA K     * 2015/04/21 * RFS 142884 - RECOMPILE FOR CPYJSISTMT
020300150511      * POOJA K     * 2015/05/08 * RFS 144587 - RECOMPILE FOR CPYJSISTMT
020301211019      * Surendran C* 2021/10/19   * RFS186592 -RECOMPILE FOR CPYJSISTMT*
020400140814      *****************************************************************
020500000000      /
020600000000       ENVIRONMENT DIVISION.
020700000000
020800000000      ******************************************************************
020900000000      *                                                                *
021000000000      *    E N V I R O N M E N T   D I V I S I O N                     *
021100000000      *                                                                *
021200000000      ******************************************************************
021300000000
021400000000       CONFIGURATION SECTION.
021500000000
021600000000      ******************************************************************
021700000000      *                                                                *
021800000000      *    C O N F I G U R A T I O N   S E C T I O N                   *
021900000000      *                                                                *
022000000000      ******************************************************************
022100000000
022200000000       SOURCE-COMPUTER.    AS-400.
022300000000       OBJECT-COMPUTER.    AS-400.
022400000000
022500000000       SPECIAL-NAMES.
022600000000
022700000000      *****
022800000000      *****    Special Names.
022900000000      *****
023000000000
023100000000           LOCAL-DATA          IS  WS-LOCAL.
023200000000      /
023300000000       INPUT-OUTPUT SECTION.
023400000000
023500000000      ******************************************************************
023600000000      *                                                                *
023700000000      *    I N P U T   /   O U T P U T   S E C T I O N                 *
023800000000      *                                                                *
023900000000      ******************************************************************
024000000000
024100000000       FILE-CONTROL.
024200000000
024300000000      *****
024400000000      *****    FILE CONTROL.
024500000000      *****
024600000000      /
024700000000           SELECT ADDRESS-FILE
024800000000               ASSIGN          TO  DATABASE-MFAADDP
024900000000               ORGANIZATION    IS  INDEXED
025000000000               ACCESS MODE     IS  RANDOM
025100000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
025200000000
025300000000           SELECT COUNTRY-FILE
025400000000               ASSIGN          TO  DATABASE-MFACOUP
025500000000               ORGANIZATION    IS  INDEXED
025600000000               ACCESS MODE     IS  RANDOM
025700000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
025800000000
025900000000           SELECT MAIL-LIST-DETAILS
026000000000               ASSIGN          TO  DATABASE-MFAMLSDTLA
026100000000               ORGANIZATION    IS  INDEXED
026200000000               ACCESS          IS  DYNAMIC
026300000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
026400000000
026500000000           SELECT MAIL-LIST-HEADER
026600000000               ASSIGN          TO  DATABASE-MFAMLSHDP
026700000000               ORGANIZATION    IS  INDEXED
026800000000               ACCESS          IS  RANDOM
026900000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
027000000000
027100000000           SELECT STATEMENT-EXTRACT
027200000000               ASSIGN          TO  DATABASE-MFASTMEXP
027300000000               ORGANIZATION    IS  INDEXED
027400000000               ACCESS          IS  DYNAMIC
027500000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
027600000000
027700000000           SELECT STMT-DUP-MAIL
027800000000               ASSIGN          TO  DATABASE-STMDUP1.
027900000000
028000000000           SELECT PRINT-FILE
028100000000               ASSIGN          TO  PRINTER-QPRINT.
028200000000      /
028300000000       DATA DIVISION.
028400000000
028500000000      ******************************************************************
028600000000      *                                                                *
028700000000      *    D A T A   D I V I S I O N                                   *
028800000000      *                                                                *
028900000000      ******************************************************************
029000000000
029100000000       FILE SECTION.
029200000000
029300000000      ******************************************************************
029400000000      *                                                                *
029500000000      *    F I L E   S E C T I O N                                     *
029600000000      *                                                                *
029700000000      ******************************************************************
029800000000      /
029900000000       FD  ADDRESS-FILE
030000000000           LABEL RECORDS    ARE  STANDARD.
030100000000
030200000000       01  ADDRESS-RECD.               COPY DD-ALL-FORMATS
030300000000                                            OF MFAADDP.
030400000000      /
030500000000       FD  COUNTRY-FILE
030600000000           LABEL RECORDS    ARE  STANDARD.
030700000000
030800000000       01  COUNTRY-RECD.               COPY DD-ALL-FORMATS
030900000000                                            OF MFACOUP.
031000000000      /
031100000000       FD  MAIL-LIST-DETAILS
031200000000           LABEL RECORDS      ARE  OMITTED.
031300000000
031400000000       01  MAIL-LIST-DETAILS-RECD.     COPY DD-ALL-FORMATS
031500000000                                           OF  MFAMLSDTLA.
031600000000      /
031700000000       FD  MAIL-LIST-HEADER
031800000000           LABEL RECORDS      ARE  OMITTED.
031900000000
032000000000       01  MAIL-LIST-HEADER-RECD.      COPY DD-ALL-FORMATS
032100000000                                           OF  MFAMLSHDP.
032200000000      /
032300000000       FD  STATEMENT-EXTRACT
032400000000           LABEL RECORDS      ARE  OMITTED.
032500000000
032600000000       01  STATEMENT-EXTRACT-RECD.     COPY DD-ALL-FORMATS
032700000000                                           OF  MFASTMEXP.
032800000000      /
032900000000       FD  STMT-DUP-MAIL
033000000000           LABEL RECORDS      ARE  STANDARD
033100000000           RECORD CONTAINS    350  CHARACTERS.
033200000000
033300000000       01  STMT-DUP-MAIL-RECD          PICTURE X(350).
033400000000      /
033500000000       FD  PRINT-FILE
033600000000           LABEL RECORDS      ARE  OMITTED.
033700000000
033800000000       01  PRINT-RECD                  PICTURE X(150).
033900000000      /
034000000000       WORKING-STORAGE SECTION.
034100000000
034200000000      ******************************************************************
034300000000      *                                                                *
034400000000      *    W O R K I N G   S T O R A G E   S E C T I O N               *
034500000000      *                                                                *
034600000000      ******************************************************************
034700000000      /
034800971024       01  WS-LOCAL-DATA-AREA.         COPY CPYLDASTMT.
034900000000      /
035000000000                                       COPY CPYDUPSTMT.
035100000000      /
035200000000                                       COPY CPYJSISTMT.
035300000000      /
035400000000      ******************************************************************
035500000000      *                                                                *
035600000000      *    H E A D I N G   P R I N T   L I N E S                       *
035700000000      *                                                                *
035800000000      ******************************************************************
035900000000
036000000000       01  HEADING-LINE-1.
036100000000           05  FILLER                  PICTURE X(11)       VALUE
036200000000               "DATE     : ".
036300000000           03  HL1-YY                  PICTURE 9(2).
036400000000           03  FILLER                  PICTURE X(1)        VALUE "/".
036500000000           03  HL1-MM                  PICTURE 9(2).
036600000000           03  FILLER                  PICTURE X(1)        VALUE "/".
036700000000           03  HL1-DD                  PICTURE 9(2).
036800000000           03  FILLER                  PICTURE X(95)       VALUE SPACES.
036900000000           03  FILLER                  PICTURE X(6)        VALUE
037000000000               "TIME: ".
037100000000           03  HL1-TIME                PICTURE 99B99B99B99.
037200000000           03  HL1-TIME-R              REDEFINES HL1-TIME.
037300000000               05  FILLER              PICTURE X(8).
037400000000               05  HL1-TIME-DOT        PICTURE X(1).
037500000000               05  FILLER              PICTURE X(2).
037600000000           03  FILLER                  PICTURE X(19)       VALUE SPACES.
037700000000
037800000000       01  HEADING-LINE-2.
037900000000           03  FILLER                  PICTURE X(11)       VALUE
038000000000               "REPORT NO: ".
038100000000           03  FILLER                  PICTURE X(10)       VALUE
038200000000               "STMTDUP   ".
038300000000           03  FILLER                  PICTURE X(15)       VALUE SPACES.
038400000000           03  FILLER                  PICTURE X(39)       VALUE
038500000000               "STATEMENT DUPLICATE MAIL CONTROL REPORT".
038600000000           03  FILLER                  PICTURE X(39)       VALUE SPACES.
038700000000           03  FILLER                  PICTURE X(5)        VALUE
038800000000               "PAGE:".
038900000000           03  FILLER                  PICTURE X(8)        VALUE SPACES.
039000000000           03  HL2-PGECNT              PICTURE ZZZZ9.
039100000000           03  FILLER                  PICTURE X(18)       VALUE SPACES.
039200000000
039300000000       01  HEADING-LINE-3.
039400000000           03  FILLER                  PICTURE X(7)        VALUE
039500000000               "MAIL HD".
039600000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
039700000000           03  FILLER                  PICTURE X(7)        VALUE
039800000000               "MAIL DT".
039900000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
040000000000           03  FILLER                  PICTURE X(9)        VALUE
040100000000               "INVESTOR#".
040200000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
040300000000           03  FILLER                  PICTURE X(50)       VALUE
040400000000               "MESSAGE                                           ".
040500000000           03  FILLER                  PICTURE X(74)       VALUE SPACES.
040600000000      /
040700000000      ******************************************************************
040800000000      *                                                                *
040900000000      *    E R R O R   P R I N T   L I N E S
041000000000      *                                                                *
041100000000      ******************************************************************
041200000000
041300000000       01  ERROR-LINE-1.
041400000000           03  EL1-MAIL-NO             PICTURE 9(7).
041500000000           03  FILLER                  PICTURE X(1).
041600000000           03  EL1-MAIL-DTL-NO         PICTURE 9(7).
041700000000           03  FILLER                  PICTURE X(1).
041800000000           03  EL1-INVESTOR-NO         PICTURE 9(9).
041900000000           03  FILLER                  PICTURE X(1).
042000000000           03  EL1-MSG                 PICTURE X(50).
042100000000           03  FILLER                  PICTURE X(74).
042200000000      /
042300000000      ******************************************************************
042400000000      *                                                                *
042500000000      *    C O N T R O L   P R I N T   L I N E S                       *
042600000000      *                                                                *
042700000000      ******************************************************************
042800000000
042900000000       01  CONTROL-LINE-1.
043000000000           03  FILLER                  PICTURE X(3).
043100000000           03  CL1-LIT                 PICTURE X(50) JUST RIGHT.
043200000000           03  FILLER                  PICTURE X(1).
043300000000           03  CL1-CNT                 PICTURE ZZZ,ZZZ,ZZ9.
043400000000           03  FILLER                  PICTURE X(85).
043500000000      /
043600000000       01  WS-PGM-DEFIND-WORK-ITEMS.
043700000000
043800000000      ******************************************************************
043900000000      *                                                                *
044000000000      *    P R O G R A M   D E F I N E D   W O R K   I T E M S         *
044100000000      *                                                                *
044200000000      ******************************************************************
044300000000
044400000000      *****
044500000000      *****    Control Report Counters.
044600000000      *****
044700000000
044800000000           03  WS-MFAMLSDTLA-ECNT      PICTURE S9(7)       COMP-3.
044900000000           03  WS-MFAMLSDTLA-MCNT      PICTURE S9(7)       COMP-3.
045000000000           03  WS-MFAMLSDTLA-RCNT      PICTURE S9(7)       COMP-3.
045100000000           03  WS-MFAMLSHDP-ECNT       PICTURE S9(7)       COMP-3.
045200000000           03  WS-MFAMLSHDP-MCNT       PICTURE S9(7)       COMP-3.
045300000000           03  WS-MFASTMEXP-BCNT       PICTURE S9(7)       COMP-3.
045400000000           03  WS-MFASTMEXP-RCNT       PICTURE S9(7)       COMP-3.
045500000000           03  WS-STMT-DUP-WCNT-10     PICTURE S9(7)       COMP-3.
045600000000           03  WS-STMT-DUP-WCNT-20     PICTURE S9(7)       COMP-3.
045700000000
045800000000      *****
045900000000      *****    Control Report Literals.
046000000000      *****
046100000000
046200000000           03  WS-MFAMLSDTLA-ELIT      PICTURE X(50)       VALUE
046300000000               "MAILING LIST DETAIL RECS BYPASSED (M/L HDR ERR):  ".
046400000000           03  WS-MFAMLSDTLA-MLIT      PICTURE X(50)       VALUE
046500000000               "MAILING LIST DETAIL RECS BYPASSED (NO STMT EXT):  ".
046600000000           03  WS-MFAMLSDTLA-RLIT      PICTURE X(50)       VALUE
046700000000               "MAILING LIST DETAIL RECS READ:                    ".
046800000000           03  WS-MFAMLSHDP-ELIT       PICTURE X(50)       VALUE
046900000000               "MAILING LIST HEADER RECS IN ERROR:                ".
047000000000           03  WS-MFAMLSHDP-MLIT       PICTURE X(50)       VALUE
047100000000               "MAILING LIST HEADER RECS MISSING:                 ".
047200000000           03  WS-MFASTMEXP-BLIT       PICTURE X(50)       VALUE
047300000000               "STATEMENT EXTRACT RECS BYPASSED:                  ".
047400000000           03  WS-MFASTMEXP-RLIT       PICTURE X(50)       VALUE
047500000000               "STATEMENT EXTRACT RECS READ:                      ".
047600000000           03  WS-STMT-DUP-WLIT-10     PICTURE X(50)       VALUE
047700000000               "STATEMENT DUPLICATE MAILING (HEADER) RECS WRITTEN:".
047800000000           03  WS-STMT-DUP-WLIT-20     PICTURE X(50)       VALUE
047900000000               "STATEMENT DUPLICATE MAILING (DETAIL) RECS WRITTEN:".
048000000000
048100000000      *****
048200000000      *****    Error Messages.
048300000000      *****
048400000000
048500000000           03  WS-MSGS.
048600000000               05  WS-MSG-01           PICTURE X(50)       VALUE
048700000000                   "M/L HEADER MISSING/INVALID, RECORD BYPASSED       ".
048800000000               05  WS-MSG-02           PICTURE X(50)       VALUE
048900000000                   "NO STATEMENT EXTRACT RECORDS FOUND, RECD BYPASSED ".
049000000000               05  WS-MSG-03           PICTURE X(50)       VALUE
049100000000                   "MAIL LIST HEADER RECORD NOT FOUND                 ".
049200000000               05  WS-MSG-04           PICTURE X(50)       VALUE
049300000000                   "MAIL LIST HEADER INVESTOR NUMBER IS ZEROES        ".
049400000000           03  WS-MSGS-R               REDEFINES WS-MSGS.
049500000000               05  WS-MSG              OCCURS   4  TIMES
049600000000                                       INDEXED BY  MSG
049700000000                                       PICTURE X(50).
049800000000
049900000000      *****
050000000000      *****    Non-Numeric Items.
050100000000      *****
050200000000
050300000000           03  WS-ADRTMP               PICTURE X(40).
050400000000
050500000000           03  WS-DATE.
050600000000               05  WS-DATE-YY          PICTURE 9(2).
050700000000               05  WS-DATE-MM          PICTURE 9(2).
050800000000               05  WS-DATE-DD          PICTURE 9(2).
050900000000
051000000000           03  WS-POSTAL.
051100000000               05  WS-POSTAL1          PICTURE X(3).
051200000000               05  WS-POSTAL2          PICTURE X(7).
051300000000           03  WS-POSTAL-R             REDEFINES WS-POSTAL.
051400000000               05  WS-POSTAL-R1        PICTURE X(5).
051500000000               05  WS-POSTAL-R2.
051600000000                   07  WS-POSTAL-R2A   PICTURE X(1).
051700000000                   07  FILLER          PICTURE X(4).
051800000000
051900000000           03  WS-POSTAL-OUT.
052000000000               05  WS-POSTAL-OUT1      PICTURE X(3).
052100000000               05  FILLER              PICTURE X(1).
052200000000               05  WS-POSTAL-OUT2      PICTURE X(6).
052300000000           03  WS-POSTAL-OUT-R         REDEFINES WS-POSTAL-OUT.
052400000000               05  WS-POSTAL-OUT-R1    PICTURE X(5).
052500000000               05  WS-POSTAL-OUT-R2    PICTURE X(1).
052600000000               05  WS-POSTAL-OUT-R3    PICTURE X(4).
052700000000
052800000000      *****
052900000000      *****    Numeric Items.
053000000000      *****
053100000000
053200000000           03  WS-DMD-SECTION-SEQUENCE PICTURE S9(7)       COMP-3.
053300000000           03  WS-LNECNT               PICTURE S9(3)       COMP-3.
053400000000           03  WS-LNEMAX               PICTURE S9(3)       COMP-3.
053500000000           03  WS-PGECNT               PICTURE S9(5)       COMP-3.
053600000000
053700000000      *****
053800000000      *****    Switches.
053900000000      *****
054000000000
054100000000           03  WS-EOJHDG-SW            PICTURE X(1).
054200000000           03  WS-L01BYP-SW            PICTURE X(1).
054300000000      /
054400000000       01  DUMP-ARGUMENTS.
054500000000
054600000000      ******************************************************************
054700000000      *                                                                *
054800000000      *    D U M P   P A S S   P A R A M E T E R S                     *
054900000000      *                                                                *
055000000000      ******************************************************************
055100000000
055200000000           03  DA-TYPE                 PICTURE X(1)        VALUE "F".
055300000000               88  COBOL-IDENTIFIERS                       VALUE "D".
055400000000               88  FULL-DUMP                               VALUE "F".
055500000000           03  FILLER                  PICTURE X(1).
055600000000           03  DA-PGM-NAME             PICTURE X(10)       VALUE
055700000000               "STMTPRT   ".
055800000000      /
055900000000      ******************************************************************
056000000000      *                                                                *
056100000000      *    C U R R E N T   /   P R E V I O U S   C O N T R O L S       *
056200000000      *                                                                *
056300000000      ******************************************************************
056400000000
056500000000       01  CURR-CONTROLS.
056600000000           03  CURR-LEVEL1.
056700000000               05  CURR-MAIL-NO        PICTURE 9(7).
056800000000
056900000000       01  PREV-CONTROLS.
057000000000           03  PREV-LEVEL1.
057100000000               05  PREV-MAIL-NO        PICTURE 9(7).
057200000000      /
057300000000       PROCEDURE DIVISION.
057400000000
057500000000      ******************************************************************
057600000000      *                                                                *
057700000000      *    P R O C E D U R E   D I V I S I O N                         *
057800000000      *                                                                *
057900000000      ******************************************************************
058000000000      /
058100000000       MAINLINE.
058200000000
058300000000      ******************************************************************
058400000000      *                                                                *
058500000000      *    M A I N L I N E                                             *
058600000000      *                                                                *
058700000000      ******************************************************************
058800000000
058900000000           PERFORM INITIAL-LOGIC-RTN  THRU ILR-EXIT.
059000000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
059100000000               GO TO MLR-0040.
059200000000
059300000000       MLR-0010.
059400000000           PERFORM GET-CURRENT-RECORD THRU GCR-EXIT.
059500000000           IF  PREV-LEVEL1    IS EQUAL TO  CURR-LEVEL1
059600000000               GO TO MLR-0030.
059700000000
059800000000           IF  PREV-CONTROLS  IS EQUAL TO  LOW-VALUES
059900000000               GO TO MLR-0020.
060000000000
060100000000           PERFORM LEVEL-SUMMARY-1    THRU LS1-EXIT.
060200000000
060300000000       MLR-0020.
060400000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
060500000000               GO TO MLR-0040.
060600000000
060700000000           PERFORM LEVEL-REINIT-1     THRU LR1-EXIT.
060800000000
060900000000       MLR-0030.
061000000000           PERFORM DETAIL-PROCESS-RTN THRU DPR-EXIT.
061100000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
061200000000               GO TO MLR-0040.
061300000000
061400000000           MOVE CURR-CONTROLS          TO  PREV-CONTROLS.
061500000000           GO TO MLR-0010.
061600000000
061700000000       MLR-0040.
061800000000           PERFORM END-JOB-RTN        THRU EJR-EXIT.
061900000000           STOP RUN.
062000000000      /
062100000000       GET-CURRENT-RECORD.
062200000000
062300000000      ******************************************************************
062400000000      *                                                                *
062500000000      *    G E T   C U R R E N T   R E C O R D   R O U T I N E         *
062600000000      *                                                                *
062700000000      ******************************************************************
062800000000
062900000000           CONTINUE.
063000000000
063100000000      *****
063200000000      *****    Read through the Mailing List Detail records, for client
063300000000      *****    statements only.
063400000000      *****
063500000000
063600000000       GCR-0010.
063700000000           READ MAIL-LIST-DETAILS NEXT AT  END
063800000000               MOVE HIGH-VALUES        TO  CURR-CONTROLS
063900000000               GO TO GCR-EXIT.
064000000000
064100030627R17789*    IF  WS-LDA-REQUEST-SCOPE NOT EQUAL TO "B"
064200030627R17789     IF WS-LDA-STMT-CNFM-FLAG   IS NOT EQUAL TO  "C"
064300981201           AND FORM-CODE
064400000000               OF MAIL-LIST-DETAILS-RECD
064500000000                          IS NOT EQUAL TO  "CLTSTCLT  "
064600000000               MOVE HIGH-VALUES        TO  CURR-CONTROLS
064700000000               GO TO GCR-EXIT.
064800000000
064900030627R17789*    IF  WS-LDA-REQUEST-SCOPE IS EQUAL TO "B"
065000030627R17789     IF WS-LDA-STMT-CNFM-FLAG   IS EQUAL TO  "C"
065100981201           AND FORM-CODE
065200981201               OF MAIL-LIST-DETAILS-RECD
065300030627R17789*                   IS NOT EQUAL TO  "CONFIRM"
065400030627R17789                    IS NOT EQUAL TO  "CONFMCLT"
065500981201               MOVE HIGH-VALUES        TO  CURR-CONTROLS
065600981201               GO TO GCR-EXIT.
065700981201
065800000000           ADD 1                       TO  WS-MFAMLSDTLA-RCNT.
065900000000
066000000000      *****
066100000000      *****    The record has been selected and is to be processed.
066200000000      *****    Format the current controls.
066300000000      *****
066400000000
066500000000           MOVE MAIL-NO
066600000000                OF MAIL-LIST-DETAILS-RECD
066700000000                                       TO  CURR-MAIL-NO.
066800000000
066900000000       GCR-EXIT.
067000000000           EXIT.
067100000000      /
067200000000       DETAIL-PROCESS-RTN.
067300000000
067400000000      ******************************************************************
067500000000      *                                                                *
067600000000      *    D E T A I L   P R O C E S S   R O U T I N E                 *
067700000000      *                                                                *
067800000000      ******************************************************************
067900000000
068000000000      *****
068100000000      *****    If an error was detected in a re-initialization routine,
068200000000      *****    an error message will be printed and the record bypassed.
068300000000      *****
068400000000
068500000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
068600000000               SET MSG                 TO  1
068700000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
068800000000               ADD 1                   TO  WS-MFAMLSDTLA-ECNT
068900000000               GO TO DPR-EXIT.
069000000000
069100000000      *****
069200000000      *****    For each record found on the Statement Extract, for the
069300000000      *****    investor/account requested, create a Statement Duplicate
069400000000      *****    Mailing Detail.  If no records are found, an error
069500000000      *****    message will be printed and the record bypassed.
069600000000      *****
069700000000
069800000000           MOVE MAIL-DTL-NO
069900000000                OF MAIL-LIST-DETAILS-RECD
070000000000                                       TO  DMH-MAIL-DTL-NO.
070100000000           IF  SELECT-ALL-ACCTS
070200000000               OF MAIL-LIST-DETAILS-RECD
070300000000                              IS EQUAL TO  "Y"
070400000000               MOVE ZEROES             TO  DMH-ACCOUNT
070500000000             ELSE
070600000000               MOVE ACCOUNT-NO
070700000000                    OF  MAIL-LIST-DETAILS-RECD
070800000000                                       TO  DMH-ACCOUNT.
070900000000           MOVE ZEROES                 TO  WS-DMD-SECTION-SEQUENCE.
071000000000
071100000000           INITIALIZE MFASTMEXP.
071200000000           MOVE INVESTOR-NO
071300000000                OF MAIL-LIST-HEADER-RECD
071400000000                                       TO  STMEX-INVESTOR-NO
071500000000                                           OF STATEMENT-EXTRACT-RECD.
071600030626
071700030627R17789     IF (WS-LDA-STMT-CNFM-FLAG    IS EQUAL TO  "S"
071800030627   ¦           AND WS-LDA-REQUEST-SCOPE IS EQUAL TO  "B"
071900030627   ¦           AND WS-LDA-ADHOC-BATCH   IS EQUAL TO  "B")
072000030627   ¦       OR (WS-LDA-STMT-CNFM-FLAG    IS EQUAL TO  "C"
072100030627   ¦           AND WS-LDA-REQUEST-SCOPE IS EQUAL TO  "B"
072200030627   ¦           AND WS-LDA-CNFM-DUP-FLAG IS EQUAL TO  "Y")
072300030627   ¦          MOVE ZEROES               TO STMEX-START-DATE
072400030627   ¦                                    OF STATEMENT-EXTRACT-RECD
072500030627   ¦          MOVE ZEROES               TO STMEX-END-DATE
072600030627   ¦                                    OF STATEMENT-EXTRACT-RECD
072700030626   ¦       ELSE
072800030627   ¦          MOVE WS-LDA-STRDTE        TO STMEX-START-DATE
072900030627   ¦                                    OF STATEMENT-EXTRACT-RECD
073000030627   ¦          MOVE WS-LDA-ENDDTE        TO STMEX-END-DATE
073100030627   V                                    OF STATEMENT-EXTRACT-RECD
073200030627R17789     END-IF.
073300030626
073400000000           START STATEMENT-EXTRACT
073500000000               KEY       IS NOT LESS THAN  EXTERNALLY-DESCRIBED-KEY
073600000000               INVALID KEY
073700000000                   GO TO DPR-0060.
073800000000
073900000000       DPR-0010.
074000000000           READ STATEMENT-EXTRACT NEXT AT  END
074100000000               GO TO DPR-0060.
074200000000
074300030627R17789     IF (WS-LDA-STMT-CNFM-FLAG    IS EQUAL TO  "S"
074400030627   ¦           AND WS-LDA-REQUEST-SCOPE IS EQUAL TO  "B"
074500030627   ¦           AND WS-LDA-ADHOC-BATCH   IS EQUAL TO  "B")
074600030627   ¦       OR (WS-LDA-STMT-CNFM-FLAG    IS EQUAL TO  "C"
074700030627   ¦           AND WS-LDA-REQUEST-SCOPE IS EQUAL TO  "B"
074800030627   ¦           AND WS-LDA-CNFM-DUP-FLAG IS EQUAL TO  "Y")
074900030626   ¦          IF  STMEX-INVESTOR-NO OF STATEMENT-EXTRACT-RECD NOT =
075000030626   ¦              INVESTOR-NO OF MAIL-LIST-HEADER-RECD
075100030626   ¦              GO TO DPR-0060
075200030626   ¦          END-IF
075300030626   ¦       ELSE
075400030626   ¦       IF STMEX-INVESTOR-NO OF STATEMENT-EXTRACT-RECD IS NOT
075500030626   ¦              EQUAL TO INVESTOR-NO  OF MAIL-LIST-HEADER-RECD
075600030626   ¦       OR STMEX-START-DATE  OF STATEMENT-EXTRACT-RECD
075700030626   ¦                       IS NOT EQUAL TO  WS-LDA-STRDTE
075800030626   ¦       OR STMEX-END-DATE    OF STATEMENT-EXTRACT-RECD
075900030626   ¦                       IS NOT EQUAL TO  WS-LDA-ENDDTE
076000030626   V            GO TO DPR-0060
076100030627R17789     END-IF.
076200030626
076300000000           ADD 1                       TO  WS-MFASTMEXP-RCNT.
076400000000
076500000000      *****
076600000000      *****    If all accounts were selected for the investor, all
076700000000      *****    Statement Extract records will be processed.
076800000000      *****
076900000000
077000000000           IF  SELECT-ALL-ACCTS
077100000000               OF MAIL-LIST-DETAILS-RECD
077200000000                              IS EQUAL TO  "Y"
077300000000               GO TO DPR-0040.
077400000000
077500000000      *****
077600000000      *****    If a specific accounts was selected for the investor,
077700000000      *****    only the Statement Extract records for the specified
077800000000      *****    account are to be processed.
077900000000      *****
078000000000
078100000000           IF  STMEX-SECTION-NO
078200000000               OF STATEMENT-EXTRACT-RECD
078300000000                             IS LESS THAN  10
078400000000               GO TO DPR-0040.
078500000000
078600000000           IF  STMEX-SECTION-NO
078700000000               OF STATEMENT-EXTRACT-RECD
078800000000                              IS EQUAL TO  10
078900000000               GO TO DPR-0020.
079000000000
079100000000           IF  STMEX-ACCOUNT-NO
079200000000               OF STATEMENT-EXTRACT-RECD
079300000000                              IS EQUAL TO  ACCOUNT-NO
079400000000                                           OF MAIL-LIST-DETAILS-RECD
079500000000               GO TO DPR-0040.
079600000000
079700000000           GO TO DPR-0030.
079800000000
079900000000       DPR-0020.
080000000000           MOVE STATEMENT-EXTRACT-RECD TO  INVESTOR-SUMMARY-SECTION.
080100000000           IF  IS-ACCOUNT2    IS EQUAL TO  ACCOUNT-NO
080200000000                                           OF MAIL-LIST-DETAILS-RECD
080300000000               GO TO DPR-0040.
080400000000
080500000000       DPR-0030.
080600000000           ADD 1                       TO  WS-MFASTMEXP-BCNT.
080700000000           GO TO DPR-0050.
080800000000
080900000000       DPR-0040.
081000000000
081100000000      *****
081200000000      *****    If the language has not yet been formatted for the
081300000000      *****    Statement Duplicate Mail Header record, format it if
081400000000      *****    processing the Statement Extract Investor Information
081500000000      *****    record.
081600000000      *****
081700000000
081800000000           IF  DMH-LANGUAGE-CODE EQUAL TO  SPACES
081900000000           AND STMEX-SECTION-NO
082000000000               OF STATEMENT-EXTRACT-RECD
082100000000                              IS EQUAL TO  1
082200000000               MOVE STATEMENT-EXTRACT-RECD
082300000000                                       TO  INVESTOR-INFO-SECTION
082400000000               MOVE II-LANGUAGE-CODE   TO  DMH-LANGUAGE-CODE.
082500000000
082600000000      *****
082700000000      *****    Format and create the Statement Duplicate Mail Detail
082800000000      *****    record.
082900000000      *****
083000000000
083100000000           ADD 1                       TO  WS-DMD-SECTION-SEQUENCE.
083200000000
083300000000           INITIALIZE MFASTMDFP-DETAIL-SECTION.
083400000000           MOVE DMH-MAIL-HDR-NO        TO  DMD-MAIL-HDR-NO.
083500000000           MOVE DMH-MAIL-DTL-NO        TO  DMD-MAIL-DTL-NO.
083600000000           MOVE DMH-LANGUAGE-CODE      TO  DMD-LANGUAGE-CODE.
083700000000           MOVE DMH-INVESTOR-NO        TO  DMD-INVESTOR-NO.
083800000000           MOVE DMH-ACCOUNT            TO  DMD-ACCOUNT.
083900000000           MOVE 20                     TO  DMD-SECTION-NO.
084000000000           MOVE WS-DMD-SECTION-SEQUENCE
084100000000                                       TO  DMD-SECTION-SEQUENCE.
084200000000           MOVE STMEX-INVESTOR-NO
084300000000                OF STATEMENT-EXTRACT-RECD
084400000000                                       TO  DMD-KEY-INVESTOR-NO.
084500000000           MOVE STMEX-ACCOUNT-NO
084600000000                OF STATEMENT-EXTRACT-RECD
084700000000                                       TO  DMD-KEY-ACCOUNT-NO.
084800000000           MOVE STMEX-SECTION-NO
084900000000                OF STATEMENT-EXTRACT-RECD
085000000000                                       TO  DMD-KEY-SECTION-NO.
085100000000           MOVE STMEX-SECTION-SEQ
085200000000                OF STATEMENT-EXTRACT-RECD
085300000000                                       TO  DMD-KEY-SECTION-SEQUENCE.
085400000000
085500000803RF7485     MOVE "X"                    TO  DMD-EOR-INDICATOR.
085600030627R17789     MOVE ORIGINAL-OR-COPY OF MAIL-LIST-DETAILS-RECD
085700030626   ¦                                   TO  DMD-ORIGINAL-OR-COPY.
085800030626   V       MOVE NO-OF-COPIES OF MAIL-LIST-DETAILS-RECD
085900030627R17789                                 TO  DMD-NO-OF-COPIES.
086000000000           MOVE MFASTMDFP-DETAIL-SECTION
086100000000                                       TO  STMT-DUP-MAIL-RECD.
086200000000           WRITE STMT-DUP-MAIL-RECD.
086300000000           ADD 1                       TO  WS-STMT-DUP-WCNT-20.
086400000000
086500000000       DPR-0050.
086600000000           GO TO DPR-0010.
086700000000
086800000000       DPR-0060.
086900000000
087000000000      *****
087100000000      *****    If no records were found for the investor, an error
087200000000      *****    message will be printed and the record bypassed.
087300000000      *****
087400000000
087500000000           IF  WS-DMD-SECTION-SEQUENCE
087600000000                      IS NOT GREATER THAN  ZEROES
087700971024           AND WS-LDA-REQUEST-SCOPE
087800000000                              IS EQUAL TO  "A"
087900000000               SET MSG                 TO  2
088000000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT.
088100000000           IF  WS-DMD-SECTION-SEQUENCE
088200000000                      IS NOT GREATER THAN  ZEROES
088300000000               ADD 1                   TO  WS-MFAMLSDTLA-MCNT
088400000000               GO TO DPR-EXIT.
088500000000
088600000000      *****
088700000000      *****    Create the formatted Statement Duplicate Mail Header
088800000000      *****    record.
088900000000      *****
089000000000
089100000803RF7485     MOVE "X"                    TO DMH-EOR-INDICATOR.
089200000803
089300000000           MOVE MFASTMDFP-HEADER-SECTION
089400000000                                       TO  STMT-DUP-MAIL-RECD.
089500000000           WRITE STMT-DUP-MAIL-RECD.
089600000000           ADD 1                       TO  WS-STMT-DUP-WCNT-10.
089700000000
089800000000       DPR-EXIT.
089900000000           EXIT.
090000000000      /
090100000000       LEVEL-SUMMARY-1.
090200000000
090300000000      ******************************************************************
090400000000      *                                                                *
090500000000      *    L E V E L   S U M M A R I Z A T I O N   1   R O U T I N E   *
090600000000      *                                                                *
090700000000      *        MAIL-NO:                Mail Number                     *
090800000000      *                                                                *
090900000000      ******************************************************************
091000000000
091100000000      *****
091200000000      *****    If an error was detected in the re-initialization
091300000000      *****    routine, bypass this routine.
091400000000      *****
091500000000
091600000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
091700000000               GO TO LS1-EXIT.
091800000000
091900000000      *****
092000000000      *****    Null routine.
092100000000      *****
092200000000
092300000000           CONTINUE.
092400000000
092500000000       LS1-EXIT.
092600000000           EXIT.
092700000000      /
092800000000       LEVEL-REINIT-1.
092900000000
093000000000      ******************************************************************
093100000000      *                                                                *
093200000000      *    L E V E L   R E I N I T I A L I Z A T I O N   1   R T N .   *
093300000000      *                                                                *
093400000000      *        MAIL-NO:                Mail Number                     *
093500000000      *                                                                *
093600000000      ******************************************************************
093700000000
093800000000           MOVE "Y"                    TO  WS-L01BYP-SW.
093900000000
094000000000      *****
094100000000      *****    Read the Mail List Header record.  If it is not found or
094200000000      *****    is found without an investor number, an error message
094300000000      *****    will be printed and all associated Mail List Details will
094400000000      *****    be bypassed.
094500000000      *****
094600000000
094700000000           INITIALIZE MFAMLSHDP.
094800000000           MOVE CURR-MAIL-NO           TO  MAIL-NO
094900000000                                           OF MAIL-LIST-HEADER-RECD.
095000000000           READ MAIL-LIST-HEADER
095100000000               INVALID KEY
095200000000                   SET MSG             TO  3
095300000000                   PERFORM PRINT-ERR-LINE
095400000000                                      THRU PEL-EXIT
095500000000                   ADD 1               TO  WS-MFAMLSHDP-MCNT
095600000000                   GO TO LR1-EXIT.
095700000000
095800030627R17789     IF  WS-LDA-REQUEST-SCOPE    IS NOT EQUAL TO "B"
095900030627   ¦       AND WS-LDA-STMT-CNFM-FLAG   IS NOT EQUAL TO "C"
096000030626   ¦       AND END-DATE OF MAIL-LIST-HEADER-RECD <  WS-LDA-ENDDTE
096100030626   V           GO TO LR1-EXIT
096200030627R17789     END-IF.
096300030626
096400030627R17789     IF  WS-LDA-REQUEST-SCOPE    IS EQUAL TO "B"
096500030627   ¦       AND WS-LDA-STMT-CNFM-FLAG   IS EQUAL TO "C"
096600030627   ¦       AND WS-LDA-CNFM-DUP-FLAG    IS EQUAL TO "N"
096700030627   ¦       AND END-DATE OF MAIL-LIST-HEADER-RECD <  WS-LDA-ENDDTE
096800030627   V           GO TO LR1-EXIT
096900030627R17789     END-IF.
097000030627
097100000000           IF  INVESTOR-NO
097200000000               OF MAIL-LIST-HEADER-RECD
097300000000                              IS EQUAL TO  ZEROES
097400000000               SET MSG                 TO  4
097500000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
097600000000               ADD 1               TO  WS-MFAMLSHDP-ECNT
097700000000               GO TO LR1-EXIT.
097800000000
097900000000           MOVE "N"                    TO  WS-L01BYP-SW.
098000000000
098100000000      *****
098200000000      *****    Initialize the Statement Duplicate Mail Header record.
098300000000      *****
098400000000
098500000000           INITIALIZE MFASTMDFP-HEADER-SECTION.
098600000000           MOVE CURR-MAIL-NO           TO  DMH-MAIL-HDR-NO.
098700000000           MOVE INVESTOR-NO
098800000000                OF MAIL-LIST-HEADER-RECD
098900000000                                       TO  DMH-INVESTOR-NO.
099000000000           MOVE 10                     TO  DMH-SECTION-NO.
099100000000           MOVE MAIL-DESCR
099200000000                OF MAIL-LIST-HEADER-RECD
099300000000                                       TO  DMH-MAIL-DESCRIPTION.
099400031027
099500031027R18312     MOVE WS-LDA-MGMT-COM-CDE   TO   DMH-CLIENT-CODE.
099600031027  ¦
099700031027  ¦        MOVE WS-LDA-AS-AT-DATE     TO   DMH-PROCESS-DATE.
099800031027  ¦
099900031027           EVALUATE WS-LDA-REQUEST-SCOPE
100000031027            WHEN "D"
100100031027               MOVE "D" TO DMH-STMT-TYPE-IND
100200031027            WHEN "R"
100300031027               MOVE "R" TO DMH-STMT-TYPE-IND
100400031027            WHEN "I"
100500031027               MOVE "I" TO DMH-STMT-TYPE-IND
100600031027           END-EVALUATE.
100700031027
100800031119           IF WS-LDA-REQUEST-SCOPE = "A"
100900031119             AND WS-LDA-CPYSELECT NOT = "A"
101000031119               AND WS-LDA-CPYSELECT NOT = " "
101100031119                 MOVE WS-LDA-CPYSELECT  TO  DMH-STMT-TYPE-IND
101200031119R18312     END-IF.
101300000000
101400000000      *****
101500000000      *****    Read the Address, from the Mail List Header, and format
101600000000      *****    the remaining fields of the Statement Duplicate Mail
101700000000      *****    Header record.
101800000000      *****
101900000000
102000000000           INITIALIZE MFAADDP.
102100000000           MOVE ADDR-NO
102200000000                OF MAIL-LIST-HEADER-RECD
102300000000                                       TO  ADDR-NO
102400000000                                           OF ADDRESS-RECD.
102500000000           READ ADDRESS-FILE
102600000000               INVALID KEY
102700000000                   INITIALIZE MFAADDP.
102800000000
102900000000           MOVE CO-OR-ATTENTION
103000000000                OF ADDRESS-RECD        TO  DMH-CO-LINE.
103100000000
103200000000      *****
103300000000      *****    Format the investor address line 1 with the street
103400000000      *****    number, street name, street type and street direction.
103500000000      *****    Each field will be handled separately in case one or more
103600000000      *****    of the fields contain spaces.
103700000000      *****
103800000000
103900000000           MOVE SPACES                 TO  WS-ADRTMP.
104000000000           STRING STREET-NO
104100000000                  OF ADDRESS-RECD
104200000000                             DELIMITED BY  "  "
104300000000                                     INTO  WS-ADRTMP.
104400971212
104500971212           IF  PROVINCE-CODE
104600971212               OF ADDRESS-RECD   EQUAL TO  "QC"
104700971212               GO TO LR1-0000-0010.
104800000000
104900000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
105000000000               STRING STREET-NAME
105100000000                      OF ADDRESS-RECD
105200000000                             DELIMITED BY  "  "
105300000000                                     INTO  WS-ADRTMP
105400000000             ELSE
105500000000               STRING WS-ADRTMP
105600000000                             DELIMITED BY  "  ",
105700000000                      " "    DELIMITED BY  SIZE,
105800000000                      STREET-NAME
105900000000                      OF ADDRESS-RECD
106000000000                             DELIMITED BY  "  "
106100000000                                     INTO  DMH-INVESTOR-ADDRESS1
106200000000               MOVE DMH-INVESTOR-ADDRESS1
106300000000                                       TO  WS-ADRTMP.
106400000000
106500000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
106600000000               STRING STREET-TYPE-CODE
106700000000                      OF ADDRESS-RECD
106800000000                             DELIMITED BY  "  "
106900000000                                     INTO  WS-ADRTMP
107000000000             ELSE
107100000000               STRING WS-ADRTMP
107200000000                             DELIMITED BY  "  ",
107300000000                      " "    DELIMITED BY  SIZE,
107400000000                      STREET-TYPE-CODE
107500000000                      OF ADDRESS-RECD
107600000000                             DELIMITED BY  "  "
107700000000                                     INTO  DMH-INVESTOR-ADDRESS1
107800000000               MOVE DMH-INVESTOR-ADDRESS1
107900000000                                       TO  WS-ADRTMP.
108000971212
108100971212           GO TO LR1-0000-0020.
108200971212
108300971212       LR1-0000-0010.
108400971212           IF  WS-ADRTMP      IS EQUAL TO  SPACES
108500971212               STRING STREET-TYPE-CODE
108600971212                      OF ADDRESS-RECD
108700971212                             DELIMITED BY  "  "
108800971212                                     INTO  WS-ADRTMP
108900971212             ELSE
109000971212               STRING WS-ADRTMP
109100971212                             DELIMITED BY  "  ",
109200971212                      " "    DELIMITED BY  SIZE,
109300971212                      STREET-TYPE-CODE
109400971212                      OF ADDRESS-RECD
109500971212                             DELIMITED BY  "  "
109600971212                                     INTO  DMH-INVESTOR-ADDRESS1
109700971212               MOVE DMH-INVESTOR-ADDRESS1
109800971212                                       TO  WS-ADRTMP.
109900971212
110000971212           IF  WS-ADRTMP      IS EQUAL TO  SPACES
110100971212               STRING STREET-NAME
110200971212                      OF ADDRESS-RECD
110300971212                             DELIMITED BY  "  "
110400971212                                     INTO  WS-ADRTMP
110500971212             ELSE
110600971212               STRING WS-ADRTMP
110700971212                             DELIMITED BY  "  ",
110800971212                      " "    DELIMITED BY  SIZE,
110900971212                      STREET-NAME
111000971212                      OF ADDRESS-RECD
111100971212                             DELIMITED BY  "  "
111200971212                                     INTO  DMH-INVESTOR-ADDRESS1
111300971212               MOVE DMH-INVESTOR-ADDRESS1
111400971212                                       TO  WS-ADRTMP.
111500000000
111600971212       LR1-0000-0020.
111700000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
111800000000               STRING STREET-DIRCT-CODE
111900000000                      OF ADDRESS-RECD
112000000000                             DELIMITED BY  "  "
112100000000                                     INTO  WS-ADRTMP
112200000000             ELSE
112300000000               STRING WS-ADRTMP
112400000000                             DELIMITED BY  "  ",
112500000000                      " "    DELIMITED BY  SIZE,
112600000000                      STREET-DIRCT-CODE
112700000000                      OF ADDRESS-RECD
112800000000                             DELIMITED BY  "  "
112900000000                                     INTO  DMH-INVESTOR-ADDRESS1
113000000000               MOVE DMH-INVESTOR-ADDRESS1
113100000000                                       TO  WS-ADRTMP.
113200000000
113300000000           MOVE WS-ADRTMP              TO  DMH-INVESTOR-ADDRESS1.
113400000000
113500000000      *****
113600000000      *****    Format the investor address line 2 with the miscellaneous
113700000000      *****    first address.
113800000000      *****
113900000000
114000000000           MOVE MISC-FIRST-ADDR-LINE
114100000000                OF ADDRESS-RECD        TO  DMH-INVESTOR-ADDRESS2.
114200000000
114300000000      *****
114400000000      *****    Format the investor address line 3 with the miscellaneous
114500000000      *****    second address.
114600000000      *****
114700000000
114800000000           MOVE MISC-SECOND-ADDR-LINE
114900000000                OF ADDRESS-RECD        TO  DMH-INVESTOR-ADDRESS3.
115000000000
115100000000      *****
115200000000      *****    Format the investor address line 4 with the city and the
115300000000      *****    province.  Formatting of this line will depend on the
115400000000      *****    country code.
115500000000      *****
115600000000
115700000000           MOVE SPACES                 TO  WS-ADRTMP.
115800000000           STRING CITY
115900000000                  OF ADDRESS-RECD
116000000000                             DELIMITED BY  "  ",
116100000000                                     INTO  WS-ADRTMP.
116200000000
116300000000           IF  PROVINCE-CODE
116400000000               OF ADDRESS-RECD   EQUAL TO  SPACES
116500000000               GO TO LR1-0010.
116600000000
116700000000           IF  COUNTRY-CODE
116800000000               OF ADDRESS-RECD
116900000000                          IS NOT EQUAL TO  "CAN"
117000000000           AND COUNTRY-CODE
117100000000               OF ADDRESS-RECD
117200000000                          IS NOT EQUAL TO  "USA"
117300000000               GO TO LR1-0010.
117400000000
117500000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
117600000000               STRING PROVINCE-CODE
117700000000                      OF ADDRESS-RECD
117800000000                             DELIMITED BY  "  "
117900000000                                     INTO  WS-ADRTMP
118000000000             ELSE
118100000000               STRING WS-ADRTMP
118200000000                             DELIMITED BY  "  ",
118300000000                      ", "   DELIMITED BY  SIZE,
118400000000                      PROVINCE-CODE
118500000000                      OF ADDRESS-RECD
118600000000                             DELIMITED BY  "  ",
118700000000                                     INTO  DMH-INVESTOR-ADDRESS4
118800000000               MOVE DMH-INVESTOR-ADDRESS4
118900000000                                       TO  WS-ADRTMP.
119000000000
119100000000       LR1-0010.
119200000000           MOVE WS-ADRTMP              TO  DMH-INVESTOR-ADDRESS4.
119300000000
119400000000      *****
119500000000      *****    Format the investor address line 5 with the country.
119600000000      *****    Formatting of this line will depend on the country code.
119700000000      *****
119800000000
119900000000           IF  COUNTRY-CODE
120000000000               OF ADDRESS-RECD   EQUAL TO  "CAN"
120100000000               GO TO LR1-0030.
120200000000
120300000000           IF  COUNTRY-CODE
120400000000               OF ADDRESS-RECD   EQUAL TO  "USA"
120500000000               GO TO LR1-0020.
120600000000
120700000000           INITIALIZE MFACOUP.
120800000000           MOVE COUNTRY-CODE
120900000000                OF ADDRESS-RECD        TO  COUNTRY-CODE
121000000000                                           OF COUNTRY-RECD.
121100000000           READ COUNTRY-FILE
121200000000               INVALID KEY
121300000000                   GO TO LR1-0020.
121400000000
121500000000           MOVE COUNTRY-NAME
121600000000                OF COUNTRY-RECD        TO  DMH-INVESTOR-ADDRESS5.
121700000000           GO TO LR1-0030.
121800000000
121900000000       LR1-0020.
122000000000           MOVE COUNTRY-CODE
122100000000                OF ADDRESS-RECD        TO  DMH-INVESTOR-ADDRESS5.
122200000000
122300000000       LR1-0030.
122400000000           IF  DMH-INVESTOR-ADDRESS1
122500000000                          IS NOT EQUAL TO  SPACES
122600000000               GO TO LR1-0040.
122700000000
122800000000           IF  DMH-INVESTOR-ADDRESS2
122900000000                          IS NOT EQUAL TO  SPACES
123000000000               MOVE DMH-INVESTOR-ADDRESS2
123100000000                                       TO  DMH-INVESTOR-ADDRESS1
123200000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS2
123300000000               GO TO LR1-0040.
123400000000
123500000000           IF  DMH-INVESTOR-ADDRESS3
123600000000                          IS NOT EQUAL TO  SPACES
123700000000               MOVE DMH-INVESTOR-ADDRESS3
123800000000                                       TO  DMH-INVESTOR-ADDRESS1
123900000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS3
124000000000               GO TO LR1-0040.
124100000000
124200000000           IF  DMH-INVESTOR-ADDRESS4
124300000000                          IS NOT EQUAL TO  SPACES
124400000000               MOVE DMH-INVESTOR-ADDRESS4
124500000000                                       TO  DMH-INVESTOR-ADDRESS1
124600000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS4
124700000000               GO TO LR1-0040.
124800000000
124900000000           IF  DMH-INVESTOR-ADDRESS5
125000000000                          IS NOT EQUAL TO  SPACES
125100000000               MOVE DMH-INVESTOR-ADDRESS5
125200000000                                       TO  DMH-INVESTOR-ADDRESS1
125300000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS5.
125400000000
125500000000           GO TO LR1-0070.
125600000000
125700000000       LR1-0040.
125800000000           IF  DMH-INVESTOR-ADDRESS2
125900000000                          IS NOT EQUAL TO  SPACES
126000000000               GO TO LR1-0050.
126100000000
126200000000           IF  DMH-INVESTOR-ADDRESS3
126300000000                          IS NOT EQUAL TO  SPACES
126400000000               MOVE DMH-INVESTOR-ADDRESS3
126500000000                                       TO  DMH-INVESTOR-ADDRESS2
126600000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS3
126700000000               GO TO LR1-0050.
126800000000
126900000000           IF  DMH-INVESTOR-ADDRESS4
127000000000                          IS NOT EQUAL TO  SPACES
127100000000               MOVE DMH-INVESTOR-ADDRESS4
127200000000                                       TO  DMH-INVESTOR-ADDRESS2
127300000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS4
127400000000               GO TO LR1-0050.
127500000000
127600000000           IF  DMH-INVESTOR-ADDRESS5
127700000000                          IS NOT EQUAL TO  SPACES
127800000000               MOVE DMH-INVESTOR-ADDRESS5
127900000000                                       TO  DMH-INVESTOR-ADDRESS2
128000000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS5.
128100000000
128200000000           GO TO LR1-0070.
128300000000
128400000000       LR1-0050.
128500000000           IF  DMH-INVESTOR-ADDRESS3
128600000000                          IS NOT EQUAL TO  SPACES
128700000000               GO TO LR1-0060.
128800000000
128900000000           IF  DMH-INVESTOR-ADDRESS4
129000000000                          IS NOT EQUAL TO  SPACES
129100000000               MOVE DMH-INVESTOR-ADDRESS4
129200000000                                       TO  DMH-INVESTOR-ADDRESS3
129300000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS4
129400000000               GO TO LR1-0060.
129500000000
129600000000           IF  DMH-INVESTOR-ADDRESS5
129700000000                          IS NOT EQUAL TO  SPACES
129800000000               MOVE DMH-INVESTOR-ADDRESS5
129900000000                                       TO  DMH-INVESTOR-ADDRESS3
130000000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS5.
130100000000
130200000000           GO TO LR1-0070.
130300000000
130400000000       LR1-0060.
130500000000           IF  DMH-INVESTOR-ADDRESS4
130600000000                          IS NOT EQUAL TO  SPACES
130700000000               GO TO LR1-0070.
130800000000
130900000000           IF  DMH-INVESTOR-ADDRESS5
131000000000                          IS NOT EQUAL TO  SPACES
131100000000               MOVE DMH-INVESTOR-ADDRESS5
131200000000                                       TO  DMH-INVESTOR-ADDRESS4
131300000000               MOVE SPACES             TO  DMH-INVESTOR-ADDRESS5.
131400000000
131500000000       LR1-0070.
131600000000
131700000000      *****
131800000000      *****    Format the postal code, depending on the country code.
131900000000      *****
132000000000
132100000000           MOVE POSTAL-CODE
132200000000                OF ADDRESS-RECD        TO  WS-POSTAL.
132300000000           MOVE SPACES                 TO  WS-POSTAL-OUT.
132400000000
132500000000           IF  COUNTRY-CODE
132600000000               OF ADDRESS-RECD   EQUAL TO  "CAN"
132700000000               MOVE WS-POSTAL1         TO  WS-POSTAL-OUT1
132800000000               MOVE WS-POSTAL2         TO  WS-POSTAL-OUT2
132900000000               GO TO LR1-0080.
133000000000
133100000000           IF  COUNTRY-CODE
133200000000               OF ADDRESS-RECD
133300000000                          IS NOT EQUAL TO  "USA"
133400000000               MOVE WS-POSTAL          TO  WS-POSTAL-OUT
133500000000               GO TO LR1-0080.
133600000000
133700000000           IF  WS-POSTAL-R2   IS EQUAL TO  SPACES
133800000000           OR  WS-POSTAL-R2A  IS EQUAL TO  "-"
133900000000               MOVE WS-POSTAL          TO  WS-POSTAL-OUT
134000000000               GO TO LR1-0080.
134100000000
134200000000           MOVE WS-POSTAL-R1           TO  WS-POSTAL-OUT-R1.
134300000000           MOVE "-"                    TO  WS-POSTAL-OUT-R2.
134400000000           MOVE WS-POSTAL-R2           TO  WS-POSTAL-OUT-R3.
134500000000
134600000000       LR1-0080.
134700000000           MOVE WS-POSTAL-OUT          TO  DMH-POSTAL-CODE.
134800000000           MOVE COUNTRY-CODE
134900000000                OF ADDRESS-RECD        TO  DMH-COUNTRY-CODE.
135000000000           MOVE HOLD-MAIL
135100000000                OF ADDRESS-RECD        TO  DMH-HOLD-MAIL.
135200000000
135300000000       LR1-EXIT.
135400000000           EXIT.
135500000000      /
135600000000       PRINT-ERR-LINE.
135700000000
135800000000      ******************************************************************
135900000000      *                                                                *
136000000000      *    P R I N T   E R R O R   L I N E   R O U T I N E             *
136100000000      *                                                                *
136200000000      ******************************************************************
136300000000
136400000000      *****
136500000000      *****    Format the error line.
136600000000      *****
136700000000
136800000000           MOVE SPACES                 TO  ERROR-LINE-1.
136900000000           MOVE MAIL-NO
137000000000                OF MAIL-LIST-DETAILS-RECD
137100000000                                       TO  EL1-MAIL-NO.
137200000000           MOVE MAIL-DTL-NO
137300000000                OF MAIL-LIST-DETAILS-RECD
137400000000                                       TO  EL1-MAIL-DTL-NO.
137500000000           MOVE INVESTOR-NO
137600000000                OF MAIL-LIST-HEADER-RECD
137700000000                                       TO  EL1-INVESTOR-NO.
137800000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
137900000000
138000000000      *****
138100000000      *****    If the page is full, start a new page and print the
138200000000      *****    Heading lines.
138300000000      *****
138400000000
138500000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
138600000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
138700000000
138800000000      *****
138900000000      *****    Print the error line.
139000000000      *****
139100000000
139200000000           WRITE PRINT-RECD          FROM  ERROR-LINE-1
139300000000                         AFTER  ADVANCING  1.
139400000000           ADD 1                       TO  WS-LNECNT.
139500000000
139600000000       PEL-EXIT.
139700000000           EXIT.
139800000000      /
139900000000       INITIAL-LOGIC-RTN.
140000000000
140100000000      ******************************************************************
140200000000      *                                                                *
140300000000      *    I N I T I A L   L O G I C   R O U T I N E                   *
140400000000      *                                                                *
140500000000      ******************************************************************
140600000000
140700000000      *****
140800000000      *****    Retrieve the local data area parameters.
140900000000      *****
141000000000
141100000000           ACCEPT WS-LOCAL-DATA-AREA FROM  WS-LOCAL.
141200000000
141300000000      *****
141400000000      *****    Initialize the program defined work items.
141500000000      *****
141600000000
141700000000           MOVE ZEROES                 TO  WS-MFAMLSDTLA-ECNT.
141800000000           MOVE ZEROES                 TO  WS-MFAMLSDTLA-MCNT.
141900000000           MOVE ZEROES                 TO  WS-MFAMLSDTLA-RCNT.
142000000000           MOVE ZEROES                 TO  WS-MFAMLSHDP-ECNT.
142100000000           MOVE ZEROES                 TO  WS-MFAMLSHDP-MCNT.
142200000000           MOVE ZEROES                 TO  WS-MFASTMEXP-BCNT.
142300000000           MOVE ZEROES                 TO  WS-MFASTMEXP-RCNT.
142400000000           MOVE ZEROES                 TO  WS-STMT-DUP-WCNT-10.
142500000000           MOVE ZEROES                 TO  WS-STMT-DUP-WCNT-20.
142600000000
142700000000           MOVE 99                     TO  WS-LNECNT.
142800000000           MOVE 60                     TO  WS-LNEMAX.
142900000000           MOVE ZEROES                 TO  WS-PGECNT.
143000000000
143100000000           MOVE "N"                    TO  WS-EOJHDG-SW.
143200000000
143300000000           MOVE LOW-VALUES             TO  CURR-CONTROLS.
143400000000           MOVE LOW-VALUES             TO  PREV-CONTROLS.
143500000000
143600000000      *****
143700000000      *****    Format the heading literals.
143800000000      *****
143900000000
144000000000           ACCEPT WS-DATE            FROM  DATE.
144100000000           MOVE WS-DATE-YY             TO  HL1-YY.
144200000000           MOVE WS-DATE-MM             TO  HL1-MM.
144300000000           MOVE WS-DATE-DD             TO  HL1-DD.
144400000000
144500000000           ACCEPT HL1-TIME           FROM  TIME.
144600000000           INSPECT HL1-TIME REPLACING ALL  SPACE
144700000000                                       BY  ":".
144800000000           MOVE "."                    TO  HL1-TIME-DOT.
144900000000
145000000000      *****
145100000000      *****    Open the Input files.
145200000000      *****
145300000000
145400000000           OPEN    INPUT   ADDRESS-FILE
145500000000                           COUNTRY-FILE
145600000000                           MAIL-LIST-DETAILS
145700000000                           MAIL-LIST-HEADER
145800000000                           STATEMENT-EXTRACT.
145900000000
146000000000      *****
146100000000      *****    Open the Output files.
146200000000      *****
146300000000
146400000000           OPEN    OUTPUT  STMT-DUP-MAIL
146500000000                           PRINT-FILE.
146600000000
146700031021      *****
146800031021      *****    Retrieve the client Company-Code and the Process-Date   m
146900031021      *****
147000031021
147100000000      *****
147200000000      *****    Start the Mail List Details for the client statement form
147300000000      *****    code.  If no records are found, exit.
147400000000      *****
147500000000
147600000000           INITIALIZE MFAMLSDTP.
147700981201
147800030627R17789*    IF WS-LDA-REQUEST-SCOPE IS NOT EQUAL TO "B"
147900030627R17789     IF WS-LDA-STMT-CNFM-FLAG   IS NOT EQUAL TO  "C"
148000981201              MOVE "CLTSTCLT  "           TO  FORM-CODE
148100981201                                           OF MAIL-LIST-DETAILS-RECD
148200981201           ELSE
148300030627R17789*       MOVE "CONFIRM"              TO  FORM-CODE
148400030627R17789        MOVE "CONFMCLT"             TO  FORM-CODE
148500981201                                          OF  MAIL-LIST-DETAILS-RECD.
148600000000           START MAIL-LIST-DETAILS
148700000000               KEY       IS NOT LESS THAN  EXTERNALLY-DESCRIBED-KEY
148800000000               INVALID KEY
148900000000                   MOVE HIGH-VALUES    TO  CURR-CONTROLS
149000000000                   GO TO ILR-EXIT.
149100031021
149200031021
149300000000       ILR-EXIT.
149400000000           EXIT.
149500000000      /
149600000000       END-JOB-RTN.
149700000000
149800000000      ******************************************************************
149900000000      *                                                                *
150000000000      *    E N D   J O B   R O U T I N E                               *
150100000000      *                                                                *
150200000000      ******************************************************************
150300000000
150400000000      *****
150500000000      *****    Print the Control Report on a separate page.
150600000000      *****
150700000000
150800000000           MOVE 99                     TO  WS-LNECNT.
150900000000           MOVE "Y"                    TO  WS-EOJHDG-SW.
151000000000
151100000000           MOVE SPACES                 TO  CONTROL-LINE-1.
151200000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
151300000000
151400000000           MOVE SPACES                 TO  CONTROL-LINE-1.
151500000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
151600000000
151700000000           MOVE SPACES                 TO  CONTROL-LINE-1.
151800000000           MOVE WS-MFAMLSDTLA-RLIT     TO  CL1-LIT.
151900000000           MOVE WS-MFAMLSDTLA-RCNT     TO  CL1-CNT.
152000000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
152100000000
152200000000           IF  WS-MFAMLSDTLA-ECNT
152300000000                          IS NOT EQUAL TO  ZEROES
152400000000               MOVE SPACES             TO  CONTROL-LINE-1
152500000000               MOVE WS-MFAMLSDTLA-ELIT TO  CL1-LIT
152600000000               MOVE WS-MFAMLSDTLA-ECNT TO  CL1-CNT
152700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
152800000000
152900000000           IF  WS-MFAMLSDTLA-MCNT
153000000000                          IS NOT EQUAL TO  ZEROES
153100000000               MOVE SPACES             TO  CONTROL-LINE-1
153200000000               MOVE WS-MFAMLSDTLA-MLIT TO  CL1-LIT
153300000000               MOVE WS-MFAMLSDTLA-MCNT TO  CL1-CNT
153400000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
153500000000
153600000000           MOVE SPACES                 TO  CONTROL-LINE-1.
153700000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
153800000000
153900000000           IF  WS-MFAMLSHDP-ECNT
154000000000                          IS NOT EQUAL TO  ZEROES
154100000000               MOVE SPACES             TO  CONTROL-LINE-1
154200000000               MOVE WS-MFAMLSHDP-ELIT  TO  CL1-LIT
154300000000               MOVE WS-MFAMLSHDP-ECNT  TO  CL1-CNT
154400000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
154500000000
154600000000           IF  WS-MFAMLSHDP-MCNT
154700000000                          IS NOT EQUAL TO  ZEROES
154800000000               MOVE SPACES             TO  CONTROL-LINE-1
154900000000               MOVE WS-MFAMLSHDP-MLIT  TO  CL1-LIT
155000000000               MOVE WS-MFAMLSHDP-MCNT  TO  CL1-CNT
155100000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
155200000000
155300000000           IF  WS-MFAMLSHDP-ECNT
155400000000                          IS NOT EQUAL TO  ZEROES
155500000000           OR  WS-MFAMLSHDP-MCNT
155600000000                          IS NOT EQUAL TO  ZEROES
155700000000               MOVE SPACES             TO  CONTROL-LINE-1
155800000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
155900000000
156000000000           MOVE SPACES                 TO  CONTROL-LINE-1.
156100000000           MOVE WS-MFASTMEXP-RLIT      TO  CL1-LIT.
156200000000           MOVE WS-MFASTMEXP-RCNT      TO  CL1-CNT.
156300000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
156400000000
156500000000           MOVE SPACES                 TO  CONTROL-LINE-1.
156600000000           MOVE WS-MFASTMEXP-BLIT      TO  CL1-LIT.
156700000000           MOVE WS-MFASTMEXP-BCNT      TO  CL1-CNT.
156800000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
156900000000
157000000000           MOVE SPACES                 TO  CONTROL-LINE-1.
157100000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
157200000000
157300000000           MOVE SPACES                 TO  CONTROL-LINE-1.
157400000000           MOVE WS-STMT-DUP-WLIT-10    TO  CL1-LIT.
157500000000           MOVE WS-STMT-DUP-WCNT-10    TO  CL1-CNT.
157600000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
157700000000
157800000000           MOVE SPACES                 TO  CONTROL-LINE-1.
157900000000           MOVE WS-STMT-DUP-WLIT-20    TO  CL1-LIT.
158000000000           MOVE WS-STMT-DUP-WCNT-20    TO  CL1-CNT.
158100000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
158200000000
158300000000      *****
158400000000      *****    Close all the files.
158500000000      *****
158600000000
158700000000           CLOSE           ADDRESS-FILE
158800000000                           COUNTRY-FILE
158900000000                           MAIL-LIST-DETAILS
159000000000                           MAIL-LIST-HEADER
159100000000                           STATEMENT-EXTRACT
159200000000                           STMT-DUP-MAIL
159300000000                           PRINT-FILE.
159400000000
159500000000       EJR-EXIT.
159600000000           EXIT.
159700000000      /
159800000000       PRINT-CTL-LINE.
159900000000
160000000000      ******************************************************************
160100000000      *                                                                *
160200000000      *    P R I N T   C O N T R O L   L I N E   R O U T I N E         *
160300000000      *                                                                *
160400000000      ******************************************************************
160500000000
160600000000      *****
160700000000      *****    If the page is full, start a new page and print the
160800000000      *****    Heading lines.
160900000000      *****
161000000000
161100000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
161200000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
161300000000
161400000000      *****
161500000000      *****    Print the Control line.
161600000000      *****
161700000000
161800000000           WRITE PRINT-RECD          FROM  CONTROL-LINE-1
161900000000                         AFTER  ADVANCING  1.
162000000000           ADD 1                       TO  WS-LNECNT.
162100000000
162200000000       PCL-EXIT.
162300000000           EXIT.
162400000000      /
162500000000       PRINT-HDG-LINE.
162600000000
162700000000      ******************************************************************
162800000000      *                                                                *
162900000000      *    P R I N T   H E A D I N G   L I N E   R O U T I N E         *
163000000000      *                                                                *
163100000000      ******************************************************************
163200000000
163300000000      *****
163400000000      *****    Print Heading line 1 on a new page.
163500000000      *****
163600000000
163700000000           WRITE PRINT-RECD          FROM  HEADING-LINE-1
163800000000                          AFTER ADVANCING  PAGE.
163900000000           ADD 1                       TO  WS-PGECNT.
164000000000           MOVE 1                      TO  WS-LNECNT.
164100000000
164200000000      *****
164300000000      *****    Format and print Heading line 2.
164400000000      *****
164500000000
164600000000           MOVE WS-PGECNT              TO  HL2-PGECNT.
164700000000
164800000000           WRITE PRINT-RECD          FROM  HEADING-LINE-2
164900000000                          AFTER ADVANCING  1.
165000000000           ADD 1                       TO  WS-LNECNT.
165100000000
165200000000      *****
165300000000      *****    Print Heading line 3, if required.
165400000000      *****
165500000000
165600000000           IF  WS-EOJHDG-SW   IS EQUAL TO  "Y"
165700000000               GO TO PHL-0010.
165800000000
165900000000           WRITE PRINT-RECD          FROM  HEADING-LINE-3
166000000000                          AFTER ADVANCING  2.
166100000000           ADD 2                       TO  WS-LNECNT.
166200000000
166300000000       PHL-0010.
166400000000
166500000000      *****
166600000000      *****    Format and print a blank line.
166700000000      *****
166800000000
166900000000           MOVE SPACES                 TO  PRINT-RECD.
167000000000           WRITE PRINT-RECD
167100000000                          AFTER ADVANCING  1.
167200000000           ADD 1                       TO  WS-LNECNT.
167300000000
167400000000       PHL-EXIT.
167500000000           EXIT.
167600000000      /
167700000000       DUMP-ROUTINE.
167800000000
167900000000      ******************************************************************
168000000000      *                                                                *
168100000000      *    D U M P   R O U T I N E                                     *
168200000000      *                                                                *
168300000000      ******************************************************************
168400000000
168500000000      *****
168600000000      *****    Produce a program dump for analysis.
168700000000      *****
168800000000
168900000000           CALL    "QLREXHAN"       USING  DUMP-ARGUMENTS.
169000000000
169100000000       DR-EXIT.
169200000000           EXIT.
