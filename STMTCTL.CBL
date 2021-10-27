000100000000       IDENTIFICATION DIVISION.
000200040929       PROGRAM-ID.     STMTCTL.
000300000000       INSTALLATION.   Jewelstone Systems Inc.
000400000000       AUTHOR.         Jewelstone Systems Inc.
000500040929       DATE-WRITTEN.   SEPT 29, 2004.
000600000000       DATE-COMPILED.
000700000000
000800000000      ******************************************************************
000900000000      *                                                                *
001000040929      *    STMTCNV:    Statement Exception Report                      *
001100000000      *                                                                *
001200040929      *    PURPOSE:    To report an exception spool file for the       *
001300040929      *                statement extract file.                         *
001400040929      *
001500040929      *    LEVELS:                                                     *
001600000000      *                                                                *
001700040929      *    FILES:      (I) STMALL      Statement Extract for Investor  *
001800040929      *                (I) STMALLDEA   Statement Extract for Dealer    *
001900040929      *                (I) STMALLREP   Statement Extract for Rep       *
002000040929      *                (I) STMHHM1     Statement Hold Mail File        *
002100000000      *                (O) PRINTER     Control Report                  *
002200000000      *                                                                *
002300040929      *    CREATED:    Sept/29/2004                                    *
002400000000      *                                                                *
002500000000      ******************************************************************
002600000000      /
002700000000      ******************************************************************
002800000000      *                                                                *
002900000000      *    C H A N G E   H I S T O R Y                                 *
003000000000      *                                                                *
003100000000      ******************************************************************
003200000000      ******************************************************************
003300000000      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE              *
003400000000      ******************************************************************
003500040929      * ALI.A      * 2004/09/29   * RFS22986
003600040929      ******************************************************************
003700041220      * Brian Lu   * 2004/12/20   * RFS24514 -RECOMPILE - CPYJSISTMT
003800041220      ******************************************************************
003900051002      * ADE ADEYEMI* 2005/09/08   * RFS29550 -RECOMPILE - CPYJSISTMT
004000051002      ******************************************************************
004100051002      * ADE ADEYEMI* 2005/09/28   * RFS30065 -RECOMPILE - CPYJSISTMT
004200051002      ******************************************************************
004300050920      * Ali.A      * 2005/09/18   * RFS28931 -RECOMPILE - CPYJSISTMT
004400050920      ******************************************************************
004500050926      * C.LENTINI  * 2005/09/26   * RFS28692 -RECOMPILE - CPYJSISTMT
004600050926      ******************************************************************
004700050926      * GEETA S.   * 2005/09/15   * RFS 28107: RECOMPILE FOR CPYJSISTMT
004800060327      ******************************************************************
004900060327      * Esin A.    * 2006/03/23   * RFS 31514: RECOMPILE FOR CPYJSISTMT
005000060818      ******************************************************************
005100060818      * Raymond Mui* 2006/08/18   * RFS 34848: RECOMPILE FOR CPYJSISTMT
005200060821      *            *              *            and CPYLDASTMT.
005300050926      ******************************************************************
005400061024      * L. Wong    * 2006/10/24   * RFS36342 - Recompile - CPYJSISTMT,
005500061024      *            *              *                        CPYLDASTMT.
005600080620      ******************************************************************
005700080620      * Bathuu L   * 2008/06/20   * RFS50993 - RECOMPILE FOR CPYJSISTMT
005800061024      ******************************************************************
005900091111      * Andy Lo    * 2009/11/11   * RFS69515 - RECOMPILE FOR CPYJSISTMT
006000091111      ******************************************************************
006100100308      * A. Lo      * 2010/03/08   * RFS78161 -Recompile - CPYJSISTMT.
006200100308      ***************************************************************
006300100409      * A. Lo      * 2010/03/08   * RFS74626 -Recompile - CPYJSISTMT.
006400100308      ***************************************************************
006500120127      * Rekha S    * 2012/01/27   * RFS93662 -Recompile - CPYJSISTMT.
006600120912      ******************************************************************
006700120912      * Suresh G.  * 2012/09/12   * RFS112003-Recompile for CPYJSISTMT.
006800120912      ******************************************************************
006900130408      * Andy Chan  * 2013/04/08   * RFS117038-Recompile for CPYJSISTMT.
007000130408      ******************************************************************
007100130529      * NOBLE T    * 2013/05/29   * RFS121116-RECOMPILE FOR CPYLDASTMT.
007200130529      ******************************************************************
007300130905      * PRAVEEN E  * 2013/09/05   *RFS123925 RECOMPIL ONLY - CPYJSISTMT.
007400140519      * NOBLE T    * 2014/05/02   * RFS130239 - RECOMPILE - CPYJSISTMT.
007500140418      ******************************************************************
007600140418      *SANGEETHA V  * 2014/03/25 * RFS129378 - RECOMPILE FOR CPYJSISTMT*
007700130905      ******************************************************************
007800140706      * Suresh G.   * 2014/07/05 * RFS132121 - RECOMPILE FOR CPYJSISTMT.
007900140706      ******************************************************************
008000140814      * Suresh G.   * 2014/08/14 * RFS133489 - RECOMPILE FOR CPYJSISTMT.
008100150216      * POOJA K    * 2015/02/16  * RFS 143392 - RECOMPILE FOR CPYJSISTMT
008200150330      * POOJA K    * 2015/03/27  * RFS 144030 - RECOMPILE FOR CPYJSISTMT
008300150424      * POOJA K    * 2015/04/21  * RFS 142884 - RECOMPILE FOR CPYJSISTMT
008400150511      * POOJA K    * 2015/05/08  * RFS 144587 - RECOMPILE FOR CPYJSISTMT
008401211019      * Surendran C* 2021/10/19   * RFS186592 -RECOMPILE FOR CPYJSISTMT*
008500140814      ******************************************************************
008600000000      /
008700000000       ENVIRONMENT DIVISION.
008800000000
008900000000      ******************************************************************
009000000000      *                                                                *
009100000000      *    E N V I R O N M E N T   D I V I S I O N                     *
009200000000      *                                                                *
009300000000      ******************************************************************
009400000000
009500000000       CONFIGURATION SECTION.
009600000000
009700000000      ******************************************************************
009800000000      *                                                                *
009900000000      *    C O N F I G U R A T I O N   S E C T I O N                   *
010000000000      *                                                                *
010100000000      ******************************************************************
010200000000
010300000000       SOURCE-COMPUTER.    AS-400.
010400000000       OBJECT-COMPUTER.    AS-400.
010500000000
010600000000       SPECIAL-NAMES.
010700000000
010800000000      *****
010900000000      *****    Special Names.
011000000000      *****
011100000000
011200000000           LOCAL-DATA          IS  WS-LOCAL.
011300000000      /
011400000000       INPUT-OUTPUT SECTION.
011500000000
011600000000      ******************************************************************
011700000000      *                                                                *
011800000000      *    I N P U T   /   O U T P U T   S E C T I O N                 *
011900000000      *                                                                *
012000000000      ******************************************************************
012100000000
012200000000       FILE-CONTROL.
012300000000
012400000000      *****
012500000000      *****    FILE CONTROL.
012600000000      *****
012700000000      /
012800000000      ******************************************************************
012900040930      *                                                                *
013000040929      *    STMALL   :  Statement Investor File                         *
013100000000      *                                                                *
013200000000      ******************************************************************
013300000000
013400040929           SELECT  STMALL-FILE
013500041012                   ASSIGN          TO  DATABASE-STMALL
013600041013                   ACCESS          IS  SEQUENTIAL
013700041012                   FILE STATUS     IS  WS-FILE-STATUS-ALL.
013800000000
013900000000      ******************************************************************
014000000000      *                                                                *
014100040929      *    STMALLDEA : Statements Dealer extract file                  *
014200000000      *                                                                *
014300000000      ******************************************************************
014400000000
014500040929           SELECT  STMALLDEA-FILE
014600041012                   ASSIGN          TO  DATABASE-STMALLDEA
014700041013                   ACCESS          IS  SEQUENTIAL
014800041012                   FILE STATUS     IS  WS-FILE-STATUS-DEA.
014900000000
015000000000      ******************************************************************
015100000000      *                                                                *
015200040929      *    STMALLREP : Statements Dealer Rep extract file              *
015300000000      *                                                                *
015400000000      ******************************************************************
015500000000
015600040929           SELECT  STMALLREP-FILE
015700041012                   ASSIGN          TO  DATABASE-STMALLREP
015800041013                   ACCESS          IS  SEQUENTIAL
015900041012                   FILE STATUS     IS  WS-FILE-STATUS-REP.
016000000000
016100000000      ******************************************************************
016200000000      *                                                                *
016300040929      *    STMHHM1   : Statement Hold Main File                        *
016400000000      *                                                                *
016500000000      ******************************************************************
016600000000
016700041012           SELECT  STMHHM1-FILE
016800041012                   ASSIGN          TO  DATABASE-HHMALL
016900041012                   ORGANIZATION    IS  SEQUENTIAL
017000041012                   FILE STATUS     IS  WS-FILE-STATUS-HHM.
017100000000
017200000000      ******************************************************************
017300000000      *                                                                *
017400040929      *    QPRINT :    Control Report                                  *
017500000000      *                                                                *
017600000000      ******************************************************************
017700000000
017800040929           SELECT  PRINT-FILE
017900040929                   ASSIGN          TO  PRINTER-QPRINT.
018000000000
018100000000      /
018200000000       DATA DIVISION.
018300000000
018400000000      ******************************************************************
018500000000      *                                                                *
018600000000      *    D A T A   D I V I S I O N                                   *
018700000000      *                                                                *
018800000000      ******************************************************************
018900000000
019000000000       FILE SECTION.
019100000000
019200000000      ******************************************************************
019300000000      *                                                                *
019400000000      *    F I L E   S E C T I O N                                     *
019500000000      *                                                                *
019600000000      ******************************************************************
019700000000      /
019800000000      ******************************************************************
019900000000      *                                                                *
020000040929      *    STMALL   :  Investor Extract File                           *
020100000000      *                                                                *
020200000000      ******************************************************************
020300000000
020400040929       FD  STMALL-FILE
020500041013           LABEL RECORDS      ARE  OMITTED.
020600040929
020700041013       01  STMALL-REC         PIC X(1350).
020800000000      /
020900000000      ******************************************************************
021000000000      *                                                                *
021100040929      *    STMALLDEA : Statements Dealer Extract File                  *
021200000000      *                                                                *
021300000000      ******************************************************************
021400000000
021500040929       FD  STMALLDEA-FILE
021600041013           LABEL RECORDS      ARE  OMITTED.
021700040929
021800041013       01  STMALLDEA-REC      PIC X(1350).
021900000000      /
022000000000      ******************************************************************
022100000000      *                                                                *
022200040929      *    STMALLREP : Statements Dealer-Rep Extract File              *
022300000000      *                                                                *
022400000000      ******************************************************************
022500000000
022600040929       FD  STMALLREP-FILE
022700041013           LABEL RECORDS      ARE  OMITTED.
022800040929
022900041013       01  STMALLREP-REC      PIC X(1350).
023000000000      /
023100000000      ******************************************************************
023200000000      *                                                                *
023300041005      *    STMHHM1 :  Statements Hold Mail File                        *
023400000000      *                                                                *
023500000000      ******************************************************************
023600000000
023700041005       FD  STMHHM1-FILE
023800041008           LABEL RECORDS      ARE STANDARD
023900041008           RECORD CONTAINS    400  CHARACTERS.
024000000000
024100041008       01  STMHHM1-REC        PICTURE X(400).
024200000000      /
024300000000      ******************************************************************
024400000000      *                                                                *
024500000000      *    QPRINT:     Control Report                                  *
024600000000      *                                                                *
024700000000      ******************************************************************
024800000000
024900000000       FD  PRINT-FILE
025000041013           LABEL RECORDS      ARE  OMITTED.
025100000000
025200000000       01  PRINT-REC                   PICTURE X(150).
025300000000      /
025400000000       WORKING-STORAGE SECTION.
025500000000
025600000000      ******************************************************************
025700000000      *                                                                *
025800000000      *    W O R K I N G   S T O R A G E   S E C T I O N               *
025900000000      *                                                                *
026000000000      ******************************************************************
026100000000      /
026200971024       01  WS-LOCAL-DATA-AREA.         COPY CPYLDASTMT.
026300000000      /
026400000000                                       COPY CPYJSISTMT.
026500000000      /
026600040929                                       COPY CPYHHMSTMT.
026700040929      /
026800041012                                       COPY CPWPRCDTP.
026900041012
027000000000      ******************************************************************
027100000000      *                                                                *
027200000000      *    H E A D I N G   P R I N T   L I N E S                       *
027300000000      *                                                                *
027400000000      ******************************************************************
027500040929
027600040929      *****
027700040929      *****    Date fields.
027800040929      *****
027900040929
028000041013           03  WS-FILE-STATUS-ALL   PICTURE X(2) VALUE SPACES.
028100041013           03  WS-FILE-STATUS-DEA   PICTURE X(2) VALUE SPACES.
028200041013           03  WS-FILE-STATUS-REP   PICTURE X(2) VALUE SPACES.
028300041013           03  WS-FILE-STATUS-HHM   PICTURE X(2) VALUE SPACES.
028400041007           03  WS-SYSDATE           PICTURE 9(6).
028500041007           03  WS-DATE REDEFINES WS-SYSDATE.
028600041007               05  WS-YY            PICTURE 9(2).
028700040930               05  WS-MM            PICTURE 9(2).
028800040930               05  WS-DD            PICTURE 9(2).
028900040929
029000040929      *****
029100040929      *****    Time fields.
029200040929      *****
029300040929
029400040929           03  WS-TIME.
029500041005               05  WS-TIME-HH            PICTURE 9(2).
029600041005               05  WS-TIME-MM            PICTURE 9(2).
029700041005               05  WS-TIME-SS            PICTURE 9(2).
029800040929
029900040929      *****
030000040929      *****    Date Forrmatted fields.
030100040929      *****
030200040929
030300040929           03  WS-DATE-FORMATTED.
030400041007               05  WS-DATE-YY          PICTURE 9(2).
030500040930               05  WS-SLASH1           PICTURE X(1) VALUE "/".
030600040930               05  WS-DATE-MM          PICTURE 9(2).
030700040930               05  WS-SLASH2           PICTURE X(1) VALUE "/".
030800040930               05  WS-DATE-DD          PICTURE 9(2).
030900040929
031000041013           03  WS-PDATE-FORMAT.
031100041013               05  WS-PDATEF-YYYY      PICTURE 9(4).
031200041013               05  WS-PDATEF-MM        PICTURE 9(2).
031300041013               05  WS-PDATEF-DD        PICTURE 9(2).
031400041013
031500041013           03  WS-PDATE-FORMATTED.
031600041013               05  WS-PDATE-YYYY       PICTURE 9(4).
031700041013               05  WS-SLASH1           PICTURE X(1) VALUE "/".
031800041013               05  WS-PDATE-MM         PICTURE 9(2).
031900041013               05  WS-SLASH2           PICTURE X(1) VALUE "/".
032000041013               05  WS-PDATE-DD         PICTURE 9(2).
032100041013
032200040929      *****
032300040929      *****    Time Forrmatted fields.
032400040929      *****
032500040929
032600040929           03  WS-TIME-FORMATTED.
032700041005               05  WS-TIME-FHH            PICTURE 9(2).
032800040930               05  WS-TIME-SEMI-COLON1   PICTURE X(1) VALUE ":".
032900041005               05  WS-TIME-FMM            PICTURE 9(2).
033000040930               05  WS-TIME-SEMI-COLON2   PICTURE X(1) VALUE ":".
033100041005               05  WS-TIME-FSS            PICTURE 9(2).
033200040929
033300040929      *****
033400040929      *****    End of file flag for the Investor extract file
033500040929      *****
033600040929       01  WS-STMALL-EOF-FLAG    PIC X(1) VALUE "N".
033700040929           88  STMALL-EOF     VALUE "Y".
033800040929
033900040929      *****
034000040929      *****    End of file flag for the Dealer-Rep extract file
034100040929      *****
034200040929       01  WS-STMALLREP-EOF-FLAG    PIC X(1) VALUE "N".
034300040929           88  STMALLREP-EOF        VALUE "Y".
034400040929
034500041005      *****
034600041005      *****    End of file flag for the Dealer-Rep extract file
034700041005      *****
034800041005       01  WS-STMALLDEA-EOF-FLAG    PIC X(1) VALUE "N".
034900041005           88  STMALLDEA-EOF        VALUE "Y".
035000041005
035100040929      *****
035200040929      *****    End of file flag for the Dealer-Rep extract file
035300040929      *****
035400040929       01  WS-STMHHM1-EOF-FLAG    PIC X(1) VALUE "N".
035500040929           88  STMHHM1-EOF        VALUE "Y".
035600040929
035700040929      *****
035800040929      *****    End of file flag for the Hold Mail extract file
035900040929      *****
036000040929       01  WS-STMHHM1-FLAG             PIC X(1) VALUE "N".
036100040929           88  PROCESS-STMHHM1         VALUE "Y".
036200040929
036300040929      *****
036400041013      *****    Flag to indicate Investor copy
036500040929      *****
036600040929       01  WS-INVESTOR-FLAG            PIC X(1) VALUE "N".
036700041013           88  INVESTOR-FILE                    VALUE "Y".
036800040929
036900040929      *****
037000041013      *****    Flag to indicate Dealer copy
037100040929      *****
037200040929       01  WS-DEALER-FLAG              PIC X(1) VALUE "N".
037300041013           88  DEALER-FILE                      VALUE "Y".
037400040929
037500040929      *****
037600041013      *****    Flag to indicate Rep copy
037700040929      *****
037800040929       01  WS-REP-FLAG              PIC X(1) VALUE "N".
037900041013           88  REP-FILE                      VALUE "Y".
038000040929
038100041005      *****
038200041005      *****    Heading records for the control report
038300041005      *****
038400041012       01  WS-PGECNT                PIC S9(5) VALUE ZEROES.
038500041005
038600040929      *****
038700040929      *****    Heading records for the control report
038800040929      *****
038900000000
039000041005       01  HEADER-LINE-1.
039100041010           05  FILLER          PICTURE X(09)   VALUE "Date:    ".
039200041007           05  HL1-RUN-DATE    PIC X(8).
039300041010           03  FILLER          PICTURE X(35)   VALUE SPACES.
039400041010           03  FILLER          PIC X(27) VALUE
039500041006                                  "Statement Generation Report".
039600041014           03  FILLER          PICTURE X(37)   VALUE SPACES.
039700041014           03  FILLER          PICTURE X(9)    VALUE "Page:    ".
039800041006           03  HL1-PAGE-NO     PICTURE Z,ZZ9.
039900041014           03  FILLER          PICTURE X(20) VALUE SPACES.
040000000000
040100041005       01  HEADER-LINE-2.
040200041010           03  FILLER            PICTURE X(9)    VALUE "Time:    ".
040300041007           03  HL2-TIME          PICTURE X(8).
040400041010           03  FILLER            PICTURE X(38)   VALUE SPACES.
040500041006           03  FILLER            PICTURE X(12)   VALUE "As At Date  ".
040600041006           03  HL2-PROCESS-DATE  PICTURE X(10).
040700041014           03  FILLER            PICTURE X(37)   VALUE SPACES.
040800041006           03  FILLER            PICTURE X(16)   VALUE
040900041010                                 "Report:  STMTCTL".
041000041014           03  FILLER            PICTURE X(20)   VALUE SPACES.
041100040930
041200041005       01  HEADER-LINE-3.
041300041006           03  FILLER          PICTURE X(14)   VALUE "Frequency:    ".
041400041006           03  HL3-FREQUENCY   PICTURE X(2).
041500040930           03  FILLER          PICTURE X(134)  VALUE SPACES.
041600040930
041700041005       01  HEADER-LINE-4.
041800041006           03  FILLER          PICTURE X(14)   VALUE "Environment:  ".
041900041005           03  HL4-CTL-ID      PICTURE X(3).
042000041005           03  FILLER          PICTURE X(133)  VALUE SPACES.
042100041005
042200041005       01  HEADER-INV-LINE.
042300040930           03  FILLER          PIC X(08) VALUE "Investor".
042400040930           03  FILLER          PIC X(52) VALUE SPACES.
042500040930           03  FILLER          PIC X(10) VALUE "Form Count".
042600040930           03  FILLER          PIC X(80) VALUE SPACES.
042700040930
042800040930       01  REP-LINE.
042900040930           03  FILLER          PIC X(3) VALUE "Rep".
043000040930           03  FILLER          PIC X(147) VALUE SPACES.
043100040930
043200040930       01  DEALER-LINE.
043300040930           03  FILLER          PIC X(6) VALUE "Dealer".
043400040930           03  FILLER          PIC X(144) VALUE SPACES.
043500040930
043600040930       01  TOTAL-DETAIL-LINE.
043700040930           03  FILLER               PIC X(2)   VALUE SPACES.
043800041006           03  TOTAL-DETAIL-TITLE   PIC X(52).
043900040930           03  FILLER               PIC X(5)   VALUE SPACES.
044000041006           03  TOTAL-DETAIL-SUM     PIC ZZZ,ZZZ,ZZ9.
044100040930           03  FILLER               PIC X(80)  VALUE SPACES.
044200040930
044300040930       01  HEADER-SUB-TOTAL-LINE.
044400040930           03  FILLER               PIC X(59)  VALUE SPACES.
044500041006           03  HST-DASH             PIC X(11)  VALUE ALL "-".
044600040930           03  FILLER               PIC X(80)  VALUE SPACES.
044700040930
044800040930       01  HEADER-TOTAL-LINE.
044900040930           03  FILLER               PIC X(24)  VALUE SPACES.
045000041006           03  HST-DASH             PIC X(35)  VALUE
045100040930                              "Total # of Statements Produced ".
045200040930           03  HDR-TOTAL            PIC ZZZ,ZZZ,ZZ9.
045300040930           03  FILLER               PIC X(80)  VALUE SPACES.
045400040930
045500040930       01  TOTAL-DETAIL-LINE-LITERALS.
045600041006           03  ENG-STMT-CLT-HLD-LIT PIC X(52) VALUE
045700041012               "Total Number of English Statements - Client Held".
045800041008           03  ENG-STMT-NOMHLD-LIT  PIC X(52)  VALUE
045900040930               "Total Number of English Statements - Nominee Held".
046000041006           03  FR-STMT-CLT-HLD-LIT  PIC X(52) VALUE
046100040930               "Total Number of French Statements  - Client Held ".
046200041006           03  FR-NOM-HLD-LIT       PIC X(52) VALUE
046300041012               "Total Number of French Statements  - Nominee Held".
046400041006           03  STMT-HOLDMAIL-LIT    PIC X(52) VALUE
046500040930               "Total Number of Statements with Holdmail ".
046600041006           03  STMT-PKG-HOUSEHOLD   PIC X(52) VALUE
046700040930               "Total Number of Statement Packages in Household file".
046800041006           03  REP-STMT-LIT         PIC X(52) VALUE
046900040930               "Total Number of Rep Statements ".
047000041006           03  DEALER-STMT-LIT      PIC X(52) VALUE
047100041012               "Total Number of Dealer Statements ".
047200040930
047300040930       01  TOTAL-DETAIL-LINE-TOTALS.
047400041012           03  ENG-CLT-HLD-TTL       PIC S9(9) COMP-3 VALUE ZEROES.
047500041012           03  ENG-NOM-HLD-TTL       PIC S9(9) COMP-3 VALUE ZEROES.
047600041012           03  FR-CLT-HLD-TTL        PIC S9(9) COMP-3 VALUE ZEROES.
047700041012           03  FR-NOM-HLD-TTL        PIC S9(9) COMP-3 VALUE ZEROES.
047800041012           03  HOLDMAIL-TTL          PIC S9(9) COMP-3 VALUE ZEROES.
047900041012           03  HOUSEHOLD-TTL         PIC S9(9) COMP-3 VALUE ZEROES.
048000041012           03  PKG-HOUSEHOLD-TTL     PIC S9(9) COMP-3 VALUE ZEROES.
048100041012           03  TOTAL-STMTS           PIC S9(9) COMP-3 VALUE ZEROES.
048200041012           03  GRAND-TOTAL-STMTS     PIC S9(9) COMP-3 VALUE ZEROES.
048300041012           03  REP-TTL               PIC S9(9) COMP-3 VALUE ZEROES.
048400041012           03  DEALER-TTL            PIC S9(9) COMP-3 VALUE ZEROES.
048500000000      /
048600000000       PROCEDURE DIVISION.
048700000000
048800000000      ******************************************************************
048900000000      *                                                                *
049000000000      *    P R O C E D U R E   D I V I S I O N                         *
049100000000      *                                                                *
049200000000      ******************************************************************
049300000000      /
049400000000       MAINLINE.
049500000000
049600000000      ******************************************************************
049700000000      *                                                                *
049800000000      *    M A I N L I N E                                             *
049900000000      *                                                                *
050000000000      ******************************************************************
050100000000
050200000000           PERFORM INITIAL-LOGIC-RTN  THRU ILR-EXIT.
050300000000           PERFORM GET-CURRENT-RECORD THRU GCR-EXIT.
050400000000           PERFORM END-JOB-RTN        THRU EJR-EXIT.
050500041013           STOP RUN.
050600041013      /
050700041013       INITIAL-LOGIC-RTN.
050800041013
050900041013      ******************************************************************
051000041013      *                                                                *
051100041013      *    I N I T I A L   L O G I C   R O U T I N E                   *
051200041013      *                                                                *
051300041013      ******************************************************************
051400041013
051500041013      *****
051600041013      *****    Open the Input files.
051700041013      *****
051800041013
051900041013           OPEN    INPUT   STMALL-FILE
052000041013                           STMALLDEA-FILE
052100041013                           STMALLREP-FILE
052200041013                           STMHHM1-FILE.
052300041013
052400041013      *****
052500041013      *****    Open the Output files.
052600041013      *****
052700041013
052800041013           OPEN    OUTPUT  PRINT-FILE.
052900041013
053000041013           IF WS-FILE-STATUS-ALL NOT = "00"
053100041013              SET STMALL-EOF TO TRUE
053200041013           END-IF.
053300041013
053400041013           IF WS-FILE-STATUS-DEA NOT = "00"
053500041013              SET STMALLDEA-EOF TO TRUE
053600041013           END-IF.
053700041013
053800041013           IF WS-FILE-STATUS-REP NOT = "00"
053900041013              SET STMALLREP-EOF TO TRUE
054000041013           END-IF.
054100041013
054200041013           IF WS-FILE-STATUS-HHM NOT = "00"
054300041013              SET STMHHM1-EOF TO TRUE
054400041013           END-IF.
054500041013
054600041013      *****
054700041013      *****    Retrieve the local data area parameters.
054800041013      *****
054900041013
055000041013           ACCEPT WS-LOCAL-DATA-AREA FROM  WS-LOCAL.
055100041013
055200041013      *****
055300041013      *****    Format the heading literals.
055400041013      *****
055500041013
055600041013           ACCEPT WS-SYSDATE FROM DATE.
055700041013           MOVE WS-YY             TO  WS-DATE-YY.
055800041013           MOVE WS-MM             TO  WS-DATE-MM.
055900041013           MOVE WS-DD             TO  WS-DATE-DD.
056000041013           MOVE WS-DATE-FORMATTED TO HL1-RUN-DATE.
056100041013
056200041013           ACCEPT WS-TIME FROM  TIME.
056300041013           MOVE WS-TIME-HH        TO  WS-TIME-FHH.
056400041013           MOVE WS-TIME-MM        TO  WS-TIME-FMM.
056500041013           MOVE WS-TIME-SS        TO  WS-TIME-FSS.
056600041013           MOVE WS-TIME-FORMATTED TO HL2-TIME.
056700041013
056800041013           CALL "RTVPRCDTP" USING CPWPRCDTP-LINKAGE.
056900041013           CANCEL "RTVPRCDTP".
057000041013
057100041013           MOVE PRCDTP-AS-AT-DATE  TO WS-PDATE-FORMAT.
057200041013           MOVE WS-PDATEF-YYYY     TO WS-PDATE-YYYY.
057300041013           MOVE WS-PDATEF-MM       TO WS-PDATE-MM.
057400041013           MOVE WS-PDATEF-DD       TO WS-PDATE-DD.
057500041013           MOVE WS-PDATE-FORMATTED TO HL2-PROCESS-DATE.
057600041013
057700041013           ADD +1 to WS-PGECNT.
057800041013           MOVE WS-PGECNT              TO  HL1-PAGE-NO.
057900041013           MOVE WS-LDA-MGMT-COM-CDE    TO HL4-CTL-ID.
058000041013           MOVE WS-LDA-FREQUENCY-CODE  TO HL3-FREQUENCY.
058100041013
058200041013           WRITE  PRINT-REC  FROM  HEADER-LINE-1 AFTER ADVANCING PAGE.
058300041013           WRITE  PRINT-REC  FROM  HEADER-LINE-2.
058400041013           WRITE  PRINT-REC  FROM  HEADER-LINE-3.
058500041013           WRITE  PRINT-REC  FROM  HEADER-LINE-4.
058600041013           WRITE  PRINT-REC  FROM  HEADER-INV-LINE.
058700041013
058800041013       ILR-EXIT.
058900041013           EXIT.
059000000000      /
059100000000       GET-CURRENT-RECORD.
059200000000
059300000000      ******************************************************************
059400000000      *                                                                *
059500000000      *    G E T   C U R R E N T   R E C O R D   R O U T I N E         *
059600000000      *                                                                *
059700000000      ******************************************************************
059800041012
059900041012           IF WS-FILE-STATUS-ALL = "00"
060000041012              PERFORM PROCESS-STMALL-RTN THRU PSR-EXIT
060100041012                UNTIL STMALL-EOF
060200041012           END-IF.
060300041006
060400041012           IF WS-FILE-STATUS-DEA = "00"
060500041012              PERFORM PROCESS-STMALLDEA-RTN THRU PSDR-EXIT
060600041012                UNTIL STMALLDEA-EOF
060700041012           END-IF.
060800041006
060900041012           IF WS-FILE-STATUS-REP = "00"
061000041012              PERFORM PROCESS-STMALLREP-RTN THRU PSRR-EXIT
061100041012                UNTIL STMALLREP-EOF
061200041012           END-IF.
061300041005
061400041012           IF WS-FILE-STATUS-HHM = "00"
061500041013              SET PROCESS-STMHHM1 TO TRUE
061600041005
061700041012              PERFORM PROCESS-STMHHM1-RTN THRU PSHR-EXIT
061800041012                UNTIL STMHHM1-EOF
061900041012           END-IF.
062000000000
062100041001       GCR-EXIT.
062200041001           EXIT.
062300041001
062400041001      **********                                                       *
062500041001      **********                                                       *
062600041001      **********                                                       *
062700041001
062800041001       PROCESS-STMALL-RTN.
062900041001
063000041001      ******************************************************************
063100041001      *                                                                *
063200041001      *    PROCESS READING THE STMALL ROUTINE.                         *
063300041001      *                                                                *
063400041001      ******************************************************************
063500041013           READ STMALL-FILE
063600041006             AT END
063700041006                MOVE "Y" TO WS-STMALL-EOF-FLAG
063800041013                GO TO PSR-EXIT
063900041001           END-READ.
064000041001
064100041008           MOVE STMALL-REC TO MFASTMTFP-SECTION.
064200041008
064300041001           PERFORM DETAIL-PROCESS-ROUTINE  THRU  DPR-EXIT.
064400041001
064500041001       PSR-EXIT.
064600041001           EXIT.
064700041001
064800041001      **********                                                       *
064900041001      **********                                                       *
065000041001      **********                                                       *
065100041001
065200041001       PROCESS-STMALLDEA-RTN.
065300041001
065400041001      ******************************************************************
065500041001      *                                                                *
065600041001      *    PROCESS READING THE STMALL ROUTINE.                         *
065700041001      *                                                                *
065800041001      ******************************************************************
065900041013           READ STMALLDEA-FILE
066000041006             AT END
066100041006                MOVE "Y" TO WS-STMALLDEA-EOF-FLAG
066200041013                GO TO PSDR-EXIT
066300041001           END-READ.
066400041005
066500041008           MOVE STMALLDEA-REC TO MFASTMTFP-SECTION.
066600041001
066700041001           PERFORM DETAIL-PROCESS-ROUTINE  THRU  DPR-EXIT.
066800041001
066900041001       PSDR-EXIT.
067000041006            EXIT.
067100041001
067200041001
067300041001      **********                                                       *
067400041001      **********                                                       *
067500041001      **********                                                       *
067600041001
067700041001       PROCESS-STMALLREP-RTN.
067800041001
067900041001      ******************************************************************
068000041001      *                                                                *
068100041001      *    PROCESS READING THE STMALL ROUTINE.                         *
068200041001      *                                                                *
068300041001      ******************************************************************
068400041013           READ STMALLREP-FILE
068500041006             AT END
068600041006                MOVE "Y" TO WS-STMALLREP-EOF-FLAG
068700041013                GO TO PSRR-EXIT
068800041001           END-READ.
068900041005
069000041008           MOVE STMALLREP-REC TO MFASTMTFP-SECTION.
069100041001
069200041001           PERFORM DETAIL-PROCESS-ROUTINE  THRU  DPR-EXIT.
069300041001
069400041001       PSRR-EXIT.
069500041001            EXIT.
069600041001
069700041001      **********                                                       *
069800041001      **********                                                       *
069900041001      **********                                                       *
070000041001
070100041005       PROCESS-STMHHM1-RTN.
070200041001
070300041001      ******************************************************************
070400041001      *                                                                *
070500041001      *    PROCESS READING THE STMALL ROUTINE.                         *
070600041001      *                                                                *
070700041001      ******************************************************************
070800041005           READ STMHHM1-FILE
070900041006             AT END
071000041006                MOVE "Y" TO WS-STMHHM1-EOF-FLAG
071100041013                GO TO PSHR-EXIT
071200041001           END-READ.
071300041001
071400041008           MOVE STMHHM1-REC TO MFASTMHFP-SECTION.
071500041008
071600041001           PERFORM DETAIL-PROCESS-ROUTINE  THRU  DPR-EXIT.
071700041001
071800041001       PSHR-EXIT.
071900041001            EXIT.
072000041001
072100041001      **********                                                       *
072200041001      **********                                                       *
072300041001      **********                                                       *
072400041001
072500000000      /
072600041005       DETAIL-PROCESS-ROUTINE.
072700000000
072800000000      ******************************************************************
072900000000      *                                                                *
073000000000      *    D E T A I L   P R O C E S S   R O U T I N E                 *
073100000000      *                                                                *
073200000000      ******************************************************************
073300000000
073400000000      *****
073500041001      *****    Process the Statement Data record
073600000000      *****
073700041001
073800041006           IF PROCESS-STMHHM1
073900041006              GO TO DPR-STMHHM1
074000041006           END-IF.
074100041001
074200041006           EVALUATE PR-SECTION-NO
074300041006              WHEN "00"
074400041006                GO TO DPR-0000-0000
074500041006              WHEN "01"
074600041006                GO TO DPR-0000-0001
074700041006              WHEN "05"
074800041006                GO TO DPR-0000-0005
074900041006              WHEN "20"
075000041006                GO TO DPR-0000-0020
075100041006              WHEN OTHER
075200041006                GO TO DPR-EXIT
075300041006           END-EVALUATE.
075400041001
075500041001       DPR-0000-0000.
075600041006
075700041012           MOVE MFASTMTFP-SECTION TO DELIVERY-INFO-SECTION.
075800041012
075900041001           EVALUATE  DI-STMT-TYPE-IND
076000041006             WHEN "I"
076100041014               MOVE "N" TO WS-DEALER-FLAG
076200041014               MOVE "N" TO WS-REP-FLAG
076300041013               SET INVESTOR-FILE TO TRUE
076400041006             WHEN "R"
076500041014               MOVE "N" TO WS-INVESTOR-FLAG
076600041014               MOVE "N" TO WS-DEALER-FLAG
076700041013               SET REP-FILE TO TRUE
076800041006             WHEN "D"
076900041014               MOVE "N" TO WS-INVESTOR-FLAG
077000041014               MOVE "N" TO WS-REP-FLAG
077100041013               SET DEALER-FILE TO TRUE
077200041001           END-EVALUATE.
077300041006
077400041006           GO TO DPR-EXIT.
077500041001
077600041001       DPR-0000-0001.
077700041006
077800041012           MOVE MFASTMTFP-SECTION TO INVESTOR-INFO-SECTION.
077900041012
078000041006           IF II-HOLD-MAIL = "Y"
078100041012              ADD +1 TO HOLDMAIL-TTL
078200041006           END-IF.
078300041006
078400041006           GO TO DPR-EXIT.
078500041001
078600041001       DPR-0000-0005.
078700041013
078800041013           IF REP-FILE
078900041012             ADD +1 TO REP-TTL
079000041006           END-IF.
079100041001
079200041013           IF DEALER-FILE
079300041012             ADD +1 TO DEALER-TTL
079400041006           END-IF.
079500041001
079600041006           GO TO DPR-EXIT.
079700041001
079800041001       DPR-0000-0020.
079900041012
080000041012           MOVE MFASTMTFP-SECTION TO ACCOUNT-DETAILS-SECTION.
080100041012
080200041001           IF  AD-SECTION-SEQUENCE = 999999
080300041006               IF AD-NOMINEE = "Y"
080400041006                  IF AD-LANGUAGE = "E"
080500041012                     ADD +1 TO ENG-NOM-HLD-TTL
080600041001                  ELSE
080700041012                     ADD +1 TO FR-NOM-HLD-TTL
080800041001                  END-IF
080900041001               ELSE
081000041006                  IF AD-LANGUAGE = "E"
081100041012                     ADD +1 TO ENG-CLT-HLD-TTL
081200041001                  ELSE
081300041012                     ADD +1 TO FR-CLT-HLD-TTL
081400041001                  END-IF
081500041001               END-IF
081600041006
081700041012               ADD +1 TO TOTAL-STMTS
081800041001           END-IF.
081900041001
082000041012           GO TO DPR-EXIT.
082100041012
082200041001       DPR-STMHHM1.
082300041012
082400041001            EVALUATE HMP-SECTION-NO
082500041001              WHEN 10
082600041012                 ADD +1 TO PKG-HOUSEHOLD-TTL
082700041001              WHEN OTHER
082800041001                 GO TO DPR-EXIT
082900041001            END-EVALUATE.
083000041001
083100000000       DPR-EXIT.
083200000000           EXIT.
083300041001
083400000000      /
083500000000       END-JOB-RTN.
083600000000
083700000000      ******************************************************************
083800000000      *                                                                *
083900000000      *    E N D   J O B   R O U T I N E                               *
084000000000      *                                                                *
084100000000      ******************************************************************
084200000000
084300000000      *****
084400000000      *****    Print the Control Report read values on a separate page.
084500000000      *****
084600041006
084700041013           PERFORM    PRINT-CONTROL-REPORT  THRU PCR-EXIT.
084800000000
084900040930           CLOSE           STMALL-FILE
085000040930                           STMALLDEA-FILE
085100040930                           STMALLREP-FILE
085200041012                           STMHHM1-FILE
085300041012                           PRINT-FILE.
085400000000
085500000000       EJR-EXIT.
085600000000           EXIT.
085700000000      /
085800040930       PRINT-CONTROL-REPORT.
085900040930      ******************************************************************
086000040930      *                                                                *
086100040930      *    P R I N T   C O N T R O L   L I N E   R O U T I N E         *
086200040930      *                                                                *
086300040930      ******************************************************************
086400040930
086500041006             COMPUTE GRAND-TOTAL-STMTS =
086600041006                 HOLDMAIL-TTL + PKG-HOUSEHOLD-TTL.
086700040930
086800041006             MOVE ENG-STMT-CLT-HLD-LIT TO TOTAL-DETAIL-TITLE.
086900041006             MOVE ENG-CLT-HLD-TTL      TO TOTAL-DETAIL-SUM.
087000041006
087100041006             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE.
087200041006
087300041006             MOVE ENG-STMT-NOMHLD-LIT TO TOTAL-DETAIL-TITLE.
087400041006             MOVE ENG-NOM-HLD-TTL     TO TOTAL-DETAIL-SUM.
087500041006
087600041006             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE.
087700041006
087800041006             MOVE FR-STMT-CLT-HLD-LIT TO TOTAL-DETAIL-TITLE.
087900041006             MOVE FR-CLT-HLD-TTL      TO TOTAL-DETAIL-SUM.
088000041006
088100041006             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE.
088200041006
088300041006             MOVE FR-NOM-HLD-LIT TO TOTAL-DETAIL-TITLE.
088400041006             MOVE FR-NOM-HLD-TTL TO TOTAL-DETAIL-SUM.
088500041006
088600041006             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE.
088700041006             WRITE PRINT-REC FROM HEADER-SUB-TOTAL-LINE.
088800041006
088900041006             MOVE TOTAL-STMTS TO HDR-TOTAL.
089000041006
089100041013             WRITE PRINT-REC FROM HEADER-TOTAL-LINE AFTER ADVANCING 2.
089200041006
089300041006             MOVE STMT-HOLDMAIL-LIT TO TOTAL-DETAIL-TITLE.
089400041006             MOVE HOLDMAIL-TTL      TO TOTAL-DETAIL-SUM.
089500041006
089600041013             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE AFTER ADVANCING 3.
089700041006
089800041006             MOVE STMT-PKG-HOUSEHOLD  TO TOTAL-DETAIL-TITLE.
089900041006             MOVE PKG-HOUSEHOLD-TTL   TO TOTAL-DETAIL-SUM.
090000041006
090100041013             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE.
090200041013             WRITE PRINT-REC FROM HEADER-SUB-TOTAL-LINE.
090300041006
090400041006             MOVE GRAND-TOTAL-STMTS TO HDR-TOTAL.
090500041006
090600041006             WRITE PRINT-REC FROM HEADER-TOTAL-LINE AFTER ADVANCING 2.
090700041006             WRITE PRINT-REC FROM REP-LINE          AFTER ADVANCING 4.
090800041006
090900041006             MOVE REP-STMT-LIT TO TOTAL-DETAIL-TITLE.
091000041006             MOVE REP-TTL      TO TOTAL-DETAIL-SUM.
091100041006
091200041006             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE AFTER ADVANCING 2.
091300041006             WRITE PRINT-REC FROM DEALER-LINE       AFTER ADVANCING 4.
091400041006
091500041006             MOVE DEALER-STMT-LIT TO TOTAL-DETAIL-TITLE.
091600041006             MOVE DEALER-TTL      TO TOTAL-DETAIL-SUM.
091700041006
091800041006             WRITE PRINT-REC FROM TOTAL-DETAIL-LINE AFTER ADVANCING 2.
091900040930
092000041013       PCR-EXIT.
092100040930           EXIT.
092200040930
