000100170313      * %ATTR OPTION(*LSTDBG) TGTRLS(V7R1M0)
000101888888      *%CSTD===========================================================*
000102888888      ** Application. : UTC        Unitrax Common Programs             *
000103888888      ** Component. . : STMTPRT3                      Type: CBL        *
000104888888      **===============================================================*
000105888888      ** Sub-system . :                                                *
000106888888      ** Function . . :                                                *
000107888888      ** Sub-function :                                                *
000108888888      **%S=============================================================*
000109190722      ** Description of functions:                                     *
000111888888      **                                                               *
000112888888      **                                                               *
000113888888      **                                                               *
000114888888      **%E=============================================================*
000115888888      ** AUTHOR:    ARCAD_PGMR 03/14/2017 18:24  Y18.4.BS              *
000116888888      ** MODIFS: ** ARCAD_PGMR 06/19/2019   :    Y18.4.BS    00/       *
000117888888      **         01 AK59489DV  06/19/2019 12:11  XR5ARTK     00/       *
000118888888      **         ** US04880DM  06/19/2019   :    C17.1.BS    00/       *
000119888888      *%ECSTD==========================================================*
002000000000       IDENTIFICATION DIVISION.
002100091123       PROGRAM-ID.     STMPRT3.
002200091123       INSTALLATION.   Citigroup.
002300091123       AUTHOR.         Citigroup.
002400091123       DATE-WRITTEN.   November 16, 2009.
002500000000       DATE-COMPILED.
002600000000
002700000000      ******************************************************************
002800000000      *                                                                *
002900091123      *    STMTPRT3:    Statement Print - Standard Form                *
003000000000      *                                                                *
003100000000      *    PURPOSE:    To format and print statements using the        *
003200000000      *                standard format and the Jewelstone Statement    *
003300091123      *                file (MFASTMTFP). Customized version for MBI    *
003400000000      *                                                                *
003500000000      *    LEVELS:      1. INVESTOR-NO                                 *
003600000000      *                 2. ACCOUNT                                     *
003700000000      *                                                                *
003800980326      *    FILES:      (I) MFAACOJLA   Investor Join
003900980326      *                (I) MFADEAMIP   Dealer Mailing Instruction      *
004000000000      *                (I) MFADEAP     Dealer                          *
004100000000      *                (I) MFADEAREP   Dealer Representative           *
004200000000      *                (I) MFADERMIP   Dealer Representative Mailing   *
004300000000      *                                Instruction                     *
004400000000      *                (I) MFADRADRP   Dealer Representative Address   *
004500000000      *                (I) MFAINADDP   Institution Address             *
004600000000      *                (I) MFAINSBRP   Institution Branch              *
004700980326      *                (I) MFANMTPYP   Nominee Account Type Descr
004800991104991103*                (I) MFAACCTYP   Account Type BK
004900000000      *                (I) MFANOMMIP   Nominee Mailing Instruction     *
005000000000      *                (I) MFASTMMSP   Statement Message               *
005100990116      *                (I) MFAACCNTP   Account
005200980116      *                (O) MFASTMPDP   Statement Print Detail          *
005300980116      *                (O) MFASTMPHP   Statement Print Header          *
005400000000      *                (I) MFASTMEXP   Statement Extract Data          *
005500000000      *                (O) PRINTER     Control Report                  *
005600000000      *                                                                *
005700000000      ******************************************************************
005800000000      /
005900000000      ******************************************************************
006000000000      *                                                                *
006100000000      *    C H A N G E   H I S T O R Y                                 *
006200000000      *                                                                *
006300000000      ******************************************************************
006400000000      ******************************************************************
006500000000      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE              *
006600000000      ******************************************************************
006700091109      * Tony V.    * 2009/11/09   * RFS 73890 - Create new version 'A'
006800091109      *            *              *             for MBI
006900091123      *            *              *             modelled after STMTPRT
007000100210      * Tony V.    * 2010/02/01   * RFS 79293 - other alignment issues
007100091109      ******************************************************************
007200100308      * A. Lo      * 2010/03/08   * RFS78161 -Recompile - CPYJSISTMT.
007300100308      ***************************************************************
007400100409      * A. Lo      * 2010/03/08   * RFS74626 -Recompile - CPYJSISTMT.
007500110805      * A Lo       * 2011/08/05   * RFS 91768 - Recompile.          *
007600100308      ***************************************************************
007700120314      * A. CHAN    * 2010/03/12   * RFS93662 -RECOMPILE - CPYJSISTMT.
007800120912      *****************************************************************
007900120912      * Suresh G.  * 2012/09/12   * RFS112003-Recompile for CPYJSISTMT.
008000120912      *****************************************************************
008100130408      * Andy Chan  * 2013/04/08   * RFS117038-Add Section 22
008200130408      *****************************************************************
008300130529      * NOBLE T .  * 2013/05/29   * RFS121116-RECOMPILE FOR CPYLDASTMT.
008400130702      * K Naveen.  * 2013/07/02   * RFS123719 - Recompile only
008500130529      *****************************************************************
008600130905      * PRAVEEN E  * 2013/09/05   *RFS123925 RECOMPIL ONLY - CPYJSISTMT.
008700140519      * NOBLE T    * 2014/05/02   * RFS130239 - RECOMPILE - CPYJSISTMT.
008800140418      *****************************************************************
008900140418      *SANGEETHA V  * 2014/03/25 * RFS129378 - RECOMPILE FOR CPYJSISTMT*
009000140624      * JEYARAJAN  * 2014/06/24   * RFS132121 - INTRODUCE A NEW FIELD  *
009100140624      *            *              * TO HANDLE THE ERROR ON SEC 16      *
009200141029      *****************************************************************
009300141007      * SURESH G.  * 2014/08/14   * RFS133489-RECOMPILE FOR CPYJSISTMT.
009400141007      * JEYARAJAN  * 2014/10/07   * RFS141165 - Jobconfirm MSW         *
009500150212      * SANGEETHA V * 2015/02/12  * RFS 133528 - MV Summary Totals     *
009600150212      *             *             * are truncated when the amount      *
009700150212      *             *             * is more than 999,999,999.99.       *
009800150224      * POOJA K    * 2015/02/16  * RFS143392 - RECOMPILE FOR CPYJSISTMT*
009900150424      * POOJA K    * 2015/03/27  * RFS144030 - RECOMPILE FOR CPYJSISTMT*
010000150424      * POOJA K    * 2015/04/21  * RFS142884 - RECOMPILE FOR CPYJSISTMT*
010100150511      * POOJA K    * 2015/05/08  * RFS144587 - RECOMPILE FOR CPYJSISTMT*
010200150617      * SANGEETHA V * 2015/05/06  * RFS 147149 - RECOMPILE.            *
010300150918      * KAHAKASHAN * 2015/09/18  * RFS 149112 - AVERAGE COST TRUNCATED
010400150918      *                          * IN SPOOL FILES
010500161012      * POOJA K    * 2016/08/31  * RFS156599 - ACCOUNT HOLDING MORE    *
010600161012      *            *             * THEN 500 INVESTMENTS NOT GETTING    *
010700161012      *            *             * REPORTED IN THE EXTRACT             *
010800161108      * ARTHY K    * 2016/10/28  * RFS164194 - ACCOUNT HOLDING MORE    *
010900161108      *            *             * THEN 2000 INVESTMENTS NOT GETTING   *
011000161108      *            *             * REPORTED IN THE EXTRACT             *
011100190619      * MAYILSAMY D* 2019/06/18   * RFS183449 - POPULATE ACCOUNT TYPE  *
011200190619      *            *              * CODE BASED UPON INVESTOR LANGUAGE  *
011300190905      *            *              * IN STATEMENT EXTRACT               *
011301190924      * Arthy K    * 2019/09/03   * RFS1018431 - Rollback changes done *
011302190924      *            *              * in RFS: 183449                     *
011303190924      * Arthy K    * 2019/09/24   * RFS184715 - Put back changes in RFS*
011304190924      *            *              * 183449 with additional logic of    *
011305190924      *            *              * new edit code 'STMATL'             *
011306200704      * Mayilsamy D* 2020/07/04   * RFS186092 - Recompile              *
011307211019      * Surendran C* 2021/10/19   * RFS186592 -RECOMPILE FOR CPYJSISTMT*
011400130905      *****************************************************************
011500991014
011600000000      /
011700000000       ENVIRONMENT DIVISION.
011800000000
011900000000      ******************************************************************
012000000000      *                                                                *
012100000000      *    E N V I R O N M E N T   D I V I S I O N                     *
012200000000      *                                                                *
012300000000      ******************************************************************
012400000000
012500000000       CONFIGURATION SECTION.
012600000000
012700000000      ******************************************************************
012800000000      *                                                                *
012900000000      *    C O N F I G U R A T I O N   S E C T I O N                   *
013000000000      *                                                                *
013100000000      ******************************************************************
013200000000
013300000000       SOURCE-COMPUTER.    AS-400.
013400000000       OBJECT-COMPUTER.    AS-400.
013500000000
013600000000       SPECIAL-NAMES.
013700000000
013800000000      *****
013900000000      *****    Special Names.
014000000000      *****
014100000000
014200000000           LOCAL-DATA          IS  WS-LOCAL.
014300000000      /
014400000000       INPUT-OUTPUT SECTION.
014500000000
014600000000      ******************************************************************
014700000000      *                                                                *
014800000000      *    I N P U T   /   O U T P U T   S E C T I O N                 *
014900000000      *                                                                *
015000000000      ******************************************************************
015100000000
015200000000       FILE-CONTROL.
015300000000
015400000000      *****
015500000000      *****    FILE CONTROL.
015600000000      *****
015700000000      /
015800990116           SELECT  ACCOUNT-FILE
015900990116                   ASSIGN          TO  DATABASE-MFAACCNTP
016000990116                   ORGANIZATION    IS  INDEXED
016100990116                   ACCESS          IS  DYNAMIC
016200990116                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
016300990116
016400190612      *RFS183449-Starts
016500190612           SELECT  ACCOUNT-FILE-2
016600190612                   ASSIGN          TO  DATABASE-MFAACCNTP
016700190612                   ORGANIZATION    IS  INDEXED
016800190612                   ACCESS          IS  DYNAMIC
016900190612                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
017000190612      *RFS183449-Ends
017100190612
017200000000           SELECT DEALER-MAILING-INSTR
017300000000               ASSIGN          TO  DATABASE-MFADEAMIP
017400000000               ORGANIZATION    IS  INDEXED
017500000000               ACCESS          IS  DYNAMIC
017600000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
017700000000               FILE STATUS     IS  WS-FILSTAT.
017800000000
017900000000           SELECT DEALER-FILE
018000000000               ASSIGN          TO  DATABASE-MFADEAP
018100000000               ORGANIZATION    IS  INDEXED
018200000000               ACCESS          IS  RANDOM
018300000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
018400000000               FILE STATUS     IS  WS-FILSTAT.
018500000000
018600000000           SELECT DEALER-REP-FILE
018700000000               ASSIGN          TO  DATABASE-MFADEAREP
018800000000               ORGANIZATION    IS  INDEXED
018900000000               ACCESS          IS  RANDOM
019000000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
019100000000               FILE STATUS     IS  WS-FILSTAT.
019200000000
019300000000           SELECT DEALER-REP-MAILING-INSTR
019400000000               ASSIGN          TO  DATABASE-MFADERMIP
019500000000               ORGANIZATION    IS  INDEXED
019600000000               ACCESS          IS  DYNAMIC
019700000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
019800000000               FILE STATUS     IS  WS-FILSTAT.
019900980326
020000000000           SELECT DEALER-REP-ADDRESS
020100000000               ASSIGN          TO  DATABASE-MFADRADRP
020200000000               ORGANIZATION    IS  INDEXED
020300000000               ACCESS          IS  RANDOM
020400000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
020500000000               FILE STATUS     IS  WS-FILSTAT.
020600000000
020700000000           SELECT INSTITUTION-ADDRESS
020800000000               ASSIGN          TO  DATABASE-MFAINADDP
020900000000               ORGANIZATION    IS  INDEXED
021000000000               ACCESS          IS  RANDOM
021100000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
021200000000               FILE STATUS     IS  WS-FILSTAT.
021300000000
021400000000           SELECT INSTITUTION-BRANCH
021500000000               ASSIGN          TO  DATABASE-MFAINSBRP
021600000000               ORGANIZATION    IS  INDEXED
021700000000               ACCESS          IS  RANDOM
021800000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
021900000000               FILE STATUS     IS  WS-FILSTAT.
022000980326
022100980326           SELECT  INVESTOR-JOIN
022200980326                   ASSIGN          TO  DATABASE-MFAACOJLA
022300980326                   ORGANIZATION    IS  INDEXED
022400980326                   ACCESS          IS  DYNAMIC
022500980326                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
022600980326                                 WITH  DUPLICATES.
022700980326           SELECT NOMINEE-ACCOUNT-TYPE-DESCR
022800980326               ASSIGN          TO  DATABASE-MFANMTPYP
022900980326               ORGANIZATION    IS  INDEXED
023000980326               ACCESS          IS  DYNAMIC
023100980326               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
023200991103
023300991103           SELECT ACCOUNT-TYPE-BK
023400991103               ASSIGN          TO  DATABASE-MFAACCTYP
023500991103               ORGANIZATION    IS  INDEXED
023600991103               ACCESS          IS  SEQUENTIAL
023700991103               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
023800991103
023900000000           SELECT NOMINEE-MAILING-INSTRUCTIONS
024000000000               ASSIGN          TO  DATABASE-MFANOMMIP
024100000000               ORGANIZATION    IS  INDEXED
024200000000               ACCESS          IS  DYNAMIC
024300000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
024400000000               FILE STATUS     IS  WS-FILSTAT.
024500000000
024600020920R14588     SELECT MASTER-ACCOUNT
024700020920               ASSIGN          TO  DATABASE-MFAMACCP
024800020920               ORGANIZATION    IS  INDEXED
024900020920               ACCESS          IS  DYNAMIC
025000020920               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
025100020919
025200020919R14588     SELECT DEALER-MISC-DETAILS
025300020919               ASSIGN          TO  DATABASE-MFADEAMDP
025400020919               ORGANIZATION    IS  INDEXED
025500020919               ACCESS          IS  DYNAMIC
025600020919               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
025700020919
025800000000           SELECT STMT-MSG-FILE
025900000000               ASSIGN          TO  DATABASE-MFASTMMSP
026000000000               ORGANIZATION    IS  INDEXED
026100000000               ACCESS          IS  DYNAMIC
026200000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
026300000000               FILE STATUS     IS  WS-FILSTAT.
026400000000
026500000000           SELECT STMT-PRT-DTL-FILE
026600000000               ASSIGN          TO  DATABASE-MFASTMPDP
026700000000               ORGANIZATION    IS  INDEXED
026800000000               ACCESS          IS  DYNAMIC
026900000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
027000000000               FILE STATUS     IS  WS-FILSTAT.
027100000000
027200000000           SELECT STMT-PRT-HDR-FILE
027300000000               ASSIGN          TO  DATABASE-MFASTMPHP
027400000000               ORGANIZATION    IS  INDEXED
027500000000               ACCESS          IS  DYNAMIC
027600000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
027700000000               FILE STATUS     IS  WS-FILSTAT.
027800000000
027900000000           SELECT STMT-DTA-FILE
028000000000               ASSIGN          TO  DATABASE-MFASTMEXP
028100000000               ORGANIZATION    IS  INDEXED
028200000000               ACCESS          IS  SEQUENTIAL
028300000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
028400000000               FILE STATUS     IS  WS-FILSTAT.
028500000000
028600000000           SELECT PRINT-FILE
028700000000               ASSIGN          TO  PRINTER-QPRINT.
028800000000      /
028900000000       DATA DIVISION.
029000000000
029100000000      ******************************************************************
029200000000      *                                                                *
029300000000      *    D A T A   D I V I S I O N                                   *
029400000000      *                                                                *
029500000000      ******************************************************************
029600000000
029700000000       FILE SECTION.
029800000000
029900000000      ******************************************************************
030000000000      *                                                                *
030100000000      *    F I L E   S E C T I O N                                     *
030200000000      *                                                                *
030300000000      ******************************************************************
030400990116      /
030500990116       FD  ACCOUNT-FILE
030600990116           LABEL RECORDS      ARE  STANDARD.
030700990116
030800990116       01  ACCOUNT-REC.                COPY DD-ALL-FORMATS
030900990116                                            OF MFAACCNTP.
031000000000      /
031100190612      *RFS183449-Starts
031200190612       FD  ACCOUNT-FILE-2
031300190612           LABEL RECORDS      ARE  STANDARD.
031400190612
031500190619       01  ACCOUNT-REC-2.
031600190619       COPY DD-ALL-FORMATS OF MFAACCNTP.
031700190612      *RFS183449-Ends
031800000000      ******************************************************************
031900000000      *                                                                *
032000000000      *    MFADEAMIP:  Dealer Mailing Instruction                      *
032100000000      *                                                                *
032200000000      ******************************************************************
032300990116      /
032400000000       FD  DEALER-MAILING-INSTR
032500000000           LABEL RECORDS      ARE  OMITTED.
032600000000
032700000000       01  DEALER-MAILING-INSTR-RECD.  COPY DD-ALL-FORMATS
032800000000                                           OF  MFADEAMIP.
032900000000      /
033000000000      ******************************************************************
033100000000      *                                                                *
033200000000      *    MFADEAP:    Dealer                                          *
033300000000      *                                                                *
033400000000      ******************************************************************
033500000000
033600000000       FD  DEALER-FILE
033700000000           LABEL RECORDS      ARE  OMITTED.
033800000000
033900000000       01  DEALER-RECD.                COPY DD-ALL-FORMATS
034000000000                                           OF  MFADEAP.
034100000000      /
034200000000      ******************************************************************
034300000000      *                                                                *
034400000000      *    MFADEAREP:  Dealer Representative                           *
034500000000      *                                                                *
034600000000      ******************************************************************
034700000000
034800000000       FD  DEALER-REP-FILE
034900000000           LABEL RECORDS      ARE  OMITTED.
035000000000
035100000000       01  DEALER-REP-RECD.            COPY DD-ALL-FORMATS
035200000000                                           OF  MFADEAREP.
035300000000      /
035400000000      ******************************************************************
035500000000      *                                                                *
035600000000      *    MFADERMIP:  Dealer Representative Mailing Instruction       *
035700000000      *                                                                *
035800000000      ******************************************************************
035900000000
036000000000       FD  DEALER-REP-MAILING-INSTR
036100000000           LABEL RECORDS      ARE  OMITTED.
036200000000
036300000000       01  DLRREP-MAILING-INSTR-RECD.  COPY DD-ALL-FORMATS
036400000000                                           OF  MFADERMIP.
036500000000      /
036600000000      ******************************************************************
036700000000      *                                                                *
036800000000      *    MFADRADRP:  Dealer Representative Address                   *
036900000000      *                                                                *
037000000000      ******************************************************************
037100000000
037200000000       FD  DEALER-REP-ADDRESS
037300000000           LABEL RECORDS      ARE  OMITTED.
037400000000
037500000000       01  DEALER-REP-ADDRESS-RECD.    COPY DD-ALL-FORMATS
037600000000                                           OF  MFADRADRP.
037700000000      /
037800000000      ******************************************************************
037900000000      *                                                                *
038000000000      *    MFAINADDP:  Institution Address                             *
038100000000      *                                                                *
038200000000      ******************************************************************
038300000000
038400000000       FD  INSTITUTION-ADDRESS
038500000000           LABEL RECORDS      ARE  OMITTED.
038600000000
038700000000       01  INSTITUTION-ADDRESS-RECD.   COPY DD-ALL-FORMATS
038800000000                                           OF  MFAINADDP.
038900000000      /
039000000000      ******************************************************************
039100000000      *                                                                *
039200000000      *    MFAINSBRP:  Institution Branch                              *
039300000000      *                                                                *
039400000000      ******************************************************************
039500000000
039600000000       FD  INSTITUTION-BRANCH
039700000000           LABEL RECORDS      ARE  OMITTED.
039800000000
039900000000       01  INSTITUTION-BRANCH-RECD.    COPY DD-ALL-FORMATS
040000000000                                           OF  MFAINSBRP.
040100000000      /
040200980326      ****************************************************************
040300980326      *                                                              *
040400980326      *  MFAACOJLA:    Investor Join                                 *
040500980326      *                                                              *
040600980326      ****************************************************************
040700980326
040800980326       FD  INVESTOR-JOIN
040900980326           LABEL RECORDS      ARE  STANDARD.
041000980326
041100980326       01  INVESTOR-J-REC.             COPY DD-ALL-FORMATS
041200980326                                            OF MFAACOJLA.
041300980326      /
041400980326      *****************************************************************
041500980326      *                                                               *
041600980326      *    MFANMTPYP:  Nominee Account Type Descr                     *
041700980326      *                                                               *
041800980326      *****************************************************************
041900980326
042000980326       FD  NOMINEE-ACCOUNT-TYPE-DESCR
042100980326           LABEL RECORDS ARE STANDARD.
042200980326
042300980326       01  NOMINEE-ACCOUNT-TYPE-DESCR-REC.
042400980326                               COPY DD-ALL-FORMATS OF MFANMTPYP.
042500991103
042600991103      /
042700991103       FD  ACCOUNT-TYPE-BK
042800991103           LABEL RECORDS      ARE  STANDARD.
042900991103
043000991103       01  ACCOUNT-TYPE-BK-REC.        COPY DD-ALL-FORMATS
043100991103                                            OF MFAACCTYP.
043200991103
043300000000      ******************************************************************
043400000000      *                                                                *
043500000000      *    MFANOMMIP:  Nominee Mailing Instruction                     *
043600000000      *                                                                *
043700000000      ******************************************************************
043800000000
043900000000       FD  NOMINEE-MAILING-INSTRUCTIONS
044000000000           LABEL RECORDS      ARE  OMITTED.
044100000000
044200000000       01  NOMINEE-MAILING-INST-RECD.  COPY DD-ALL-FORMATS
044300000000                                           OF  MFANOMMIP.
044400000000      /
044500020919      ******************************************************************
044600020919      *                                                                *
044700020919      *    MFADEAMDP:  DEALER-MISC-DETAILS                             *
044800020919      *                                                                *
044900020919      ******************************************************************
045000020919
045100020919R14588 FD  DEALER-MISC-DETAILS
045200020919           LABEL RECORDS ARE STANDARD.
045300020919       01  DEALER-MISC-DETAILS-REC.     COPY DD-ALL-FORMATS
045400020919                                          OF MFADEAMDP.
045500020919      /
045600020919      ******************************************************************
045700020919      *                                                                *
045800020919      *    MFAMACCP:  MASTER ACCOUNT FILE.                             *
045900020919      *                                                                *
046000020919      ******************************************************************
046100020919
046200020919R14588 FD  MASTER-ACCOUNT
046300020919           LABEL RECORDS      ARE  OMITTED.
046400020919       01  MASTER-ACCOUNT-REC.  COPY DD-ALL-FORMATS
046500020919                                             OF  MFAMACCP.
046600020919      /
046700000000      ******************************************************************
046800000000      *                                                                *
046900000000      *    MFASTMMSP:  Statement Message                               *
047000000000      *                                                                *
047100000000      ******************************************************************
047200000000
047300000000       FD  STMT-MSG-FILE
047400000000           LABEL RECORDS      ARE  OMITTED.
047500000000
047600000000       01  STMT-MSG-RECD.              COPY DD-ALL-FORMATS
047700000000                                           OF  MFASTMMSP.
047800000000      /
047900000000      ******************************************************************
048000000000      *                                                                *
048100000000      *    MFASTMPDP:  Statement Print Detail                          *
048200000000      *                                                                *
048300000000      ******************************************************************
048400000000
048500000000       FD  STMT-PRT-DTL-FILE
048600000000           LABEL RECORDS      ARE  OMITTED.
048700000000
048800000000       01  STMT-PRT-DTL-RECD.          COPY DD-ALL-FORMATS
048900000000                                           OF  MFASTMPDP.
049000000000      /
049100000000      ******************************************************************
049200000000      *                                                                *
049300000000      *    MFASTMPHP:  Statement Print Header                          *
049400000000      *                                                                *
049500000000      ******************************************************************
049600000000
049700000000       FD  STMT-PRT-HDR-FILE
049800000000           LABEL RECORDS      ARE  OMITTED.
049900000000
050000000000       01  STMT-PRT-HDR-RECD.          COPY DD-ALL-FORMATS
050100000000                                           OF  MFASTMPHP.
050200000000      /
050300000000      ******************************************************************
050400000000      *                                                                *
050500000000      *    MFASTMEXP:  Statement Extract Data                          *
050600000000      *                                                                *
050700000000      ******************************************************************
050800000000
050900000000       FD  STMT-DTA-FILE
051000000000           LABEL RECORDS      ARE  OMITTED.
051100000000
051200000000       01  STMT-DTA-RECD.              COPY DD-ALL-FORMATS
051300000000                                       OF MFASTMEXP.
051400000000      /
051500000000      ******************************************************************
051600000000      *                                                                *
051700000000      *    QPRINT:     Control Report                                  *
051800000000      *                                                                *
051900000000      ******************************************************************
052000000000
052100000000       FD  PRINT-FILE
052200000000           LABEL RECORDS      ARE  OMITTED.
052300000000
052400000000       01  PRINT-RECD                  PICTURE X(150).
052500000000      /
052600000000       WORKING-STORAGE SECTION.
052700000000
052800000000      ******************************************************************
052900000000      *                                                                *
053000000000      *    W O R K I N G   S T O R A G E   S E C T I O N               *
053100000000      *                                                                *
053200000000      ******************************************************************
053300000000      /
053400000000       01  WS-LOCAL-DATA-AREA.         COPY CPYLDASTMT.
053500000000      /
053600000000       01  WS-DEAMLGINS-REC.           COPY DD-ALL-FORMATS
053700000000                                           OF  MFADEAMIP.
053800000000      /
053900000000       01  WS-DERMLGINS-REC.           COPY DD-ALL-FORMATS
054000000000                                           OF  MFADERMIP.
054100000000      /
054200000000       01  WS-NOMMLGINS-REC.           COPY DD-ALL-FORMATS
054300000000                                           OF  MFANOMMIP.
054400000000      /
054500000000                                       COPY CPYJSISTMT.
054600000000      /
054700000000      ******************************************************************
054800000000      *                                                                *
054900000000      *    S T A T E M E N T   S A V E   P A G E                       *
055000000000      *                                                                *
055100000000      ******************************************************************
055200000000
055300000000       01  WS-STATEMENT-PAGE.
055400000000           03  WS-STMTPGE-LNE          OCCURS  88  TIMES
055500000000                                       INDEXED BY  SPL
055600000000                                       PICTURE X(150).
055700000000
055800000000      *****
055900030827      *****    Work Area for Currency
056000000000      *****
056100030827R18092 01  WS-CURRENCY-XXX             PICTURE X(3).
056200030827   |   01  WS-CURRENCY-XXX-R           REDEFINES WS-CURRENCY-XXX.
056300030827   |       05  WS-CURRENCY-XX          PICTURE X(2).
056400030827   |       05  WS-CURRENCY-X           PICTURE X.
056500030827   |   01  WS-CURRENCY-XXX-W.
056600030827   |       05  WS-CURRENCY-XX-W        PICTURE X(2).
056700030827   |       05  WS-CURRENCY-X-W         PICTURE X.
056800030827   |   01  WS-CURRENCY-PIC3            REDEFINES WS-CURRENCY-XXX-W
056900030827   V                                   PIC X(3).
057000030827      *****
057100030827      *****    Standard Version
057200030827      *****
057300030827
057400000000       01  WS-STATEMENT-PAGE-R         REDEFINES WS-STATEMENT-PAGE.
057500000000           03  FILLER                  PICTURE X(3750).
057600000000           03  WS-STMTPGE-DTL          PICTURE X(6450).
057700000000           03  FILLER                  PICTURE X(150).
057800000000           03  WS-STMTPGE-MSG          PICTURE X(2100).
057900000000           03  FILLER                  PICTURE X(750).
058000991213
058100091109R73890*****
058200091109  |   *****    Version A:  Used by MBI
058300091109  |   *****
058400091109  |
058500091109  |    01  WS-STATEMENT-PAGE-VA-R      REDEFINES WS-STATEMENT-PAGE.
058600091112  |        03  FILLER                  PICTURE X(4050).
058700091112  |        03  WS-STMTPGE-DTL-VA       PICTURE X(6450).
058800091112  |        03  FILLER                  PICTURE X(150).
058900091112  |        03  WS-STMTPGE-MSG-VA       PICTURE X(1800).
059000091112R73890     03  FILLER                  PICTURE X(750).
059100991021
059200000000      /
059300000000      ******************************************************************
059400000000      *                                                                *
059500000000      *    S T A T E M E N T   M E S S A G E   S A V E   A R E A       *
059600000000      *                                                                *
059700000000      ******************************************************************
059800000000
059900000000       01  WS-STATEMENT-PAGE-MESSAGE.
060000000000           03  WS-STMTPGE-MSGLNE       OCCURS  14  TIMES
060100000000                                       INDEXED BY  SPML
060200000000                                       PICTURE X(150).
060300000000      /
060400000000      ******************************************************************
060500000000      *                                                                *
060600000000      *    S T A T E M E N T   H E A D I N G   P R I N T   L I N E S   *
060700000000      *                                                                *
060800000000      ******************************************************************
060900000000
061000000000      *****
061100000000      *****    The statement heading lines will be re-defined for each
061200000000      *****    version of the statement.
061300091109R73890*****        Version A:  Used by MBI.
061400990927
061500000000      /
061600000000       01  STMT-HDG-01.
061700000000           03  FILLER                  PICTURE X(2).
061800000000           03  SH01-ACCOUNT-TYPE       PICTURE X(75).
061900000000           03  FILLER                  PICTURE X(4).
062000000000           03  SH01-LIT1               PICTURE X(20).
062100000000           03  FILLER                  PICTURE X(1).
062200980722           03  SH01-START-DATE         PICTURE XXXX/XX/XX.
062300000000           03  FILLER                  PICTURE X(1).
062400000000           03  SH01-LIT2               PICTURE X(2).
062500000000           03  FILLER                  PICTURE X(1).
062600980722           03  SH01-END-DATE           PICTURE XXXX/XX/XX.
062700980722           03  FILLER                  PICTURE X(24).
062800991213
062900091109R73890 01  STMT-HDG-01-VA-R            REDEFINES STMT-HDG-01.
063000091130  |        03  FILLER                  PICTURE X(2).
063100091109  |        03  SH01VA-ACCOUNT-TYPE     PICTURE X(75).
063200091130  |        03  FILLER                  PICTURE X(4).
063300091109  |        03  SH01VA-LIT1             PICTURE X(20).
063400091109  |        03  FILLER                  PICTURE X(1).
063500091109  |        03  SH01VA-START-DATE       PICTURE XXXX/XX/XX.
063600091109  |        03  FILLER                  PICTURE X(1).
063700091109  |        03  SH01VA-LIT2             PICTURE X(2).
063800091109  |        03  FILLER                  PICTURE X(1).
063900091109  |        03  SH01VA-END-DATE         PICTURE XXXX/XX/XX.
064000091109R73890     03  FILLER                  PICTURE X(24).
064100000000      /
064200000000       01  STMT-HDG-02.
064300000000           03  FILLER                  PICTURE X(6).
064400000000           03  SH02-ACCOUNT            PICTURE ZZZZZZZZ9.
064500000000           03  FILLER                  PICTURE X(12).
064600000000           03  SH02-SIN-NO             PICTURE X(11).
064700000000           03  FILLER                  PICTURE X(6).
064800000000           03  SH02-DEALER             PICTURE X(4).
064900000000           03  SH02-LITDSH             PICTURE X(1).
065000000222           03  SH02-SALESREP           PICTURE X(6).
065100050104      * RFS25032 - Begins
065200050104      *    03  FILLER                  PICTURE X(3).
065300050104           03  FILLER                  PICTURE X(2).
065400050104      * RFS25032 - Ends
065500980722           03  SH02-END-DATE           PICTURE XXXX/XX/XX.
065600050104      * RFS25032 - Begins
065700050104      *    03  FILLER                  PICTURE X(4).
065800050104           03  FILLER                  PICTURE X(5).
065900050104      * RFS25032 - Ends
066000000000           03  SH02-SPOUSAL-CONTRIB    PICTURE X(40).
066100050104      * RFS25032 - Begins
066200050104      *    03  FILLER                  PICTURE X(5).
066300050104           03  FILLER                  PICTURE X(1).
066400050104      * RFS25032 - Ends
066500000000           03  SH02-REG-CTB-CUR        PICTURE $$$$,$$$,$$$.99.
066600000000           03  SH02-REG-CTB-CUR-SIGN   PICTURE X(1).
066700000000           03  FILLER                  PICTURE X(1).
066800000000           03  SH02-PGENO              PICTURE ZZ9.
066900000000           03  FILLER                  PICTURE X(13).
067000991022
067100091109R73890 01  STMT-HDG-02-VA-R            REDEFINES STMT-HDG-02.
067200091112  |        03  FILLER                  PICTURE X(5).
067300091109  |        03  SH02VA-ACCOUNT          PICTURE ZZZZZZZZ9.
067400091109  |        03  FILLER                  PICTURE X(11).
067500091109  |        03  SH02VA-SIN-NO           PICTURE X(11).
067600091109  |        03  FILLER                  PICTURE X(6).
067700091109  |        03  SH02VA-DEALER           PICTURE X(4).
067800091109  |        03  SH02VA-LITDSH           PICTURE X(1).
067900091109  |        03  SH02VA-SALESREP         PICTURE X(6).
068000091109  |        03  FILLER                  PICTURE X(3).
068100091109  |        03  SH02VA-END-DATE         PICTURE XXXX/XX/XX.
068200091109  |        03  FILLER                  PICTURE X(4).
068300091109  |        03  SH02VA-SPOUSAL-CONTRIB  PICTURE X(40).
068400091109  |        03  FILLER                  PICTURE X(4).
068500091109  |        03  SH02VA-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
068600091109  |        03  SH02VA-REG-CTB-CUR-SIGN PICTURE X(1).
068700091109  |        03  FILLER                  PICTURE X(1).
068800091109  |        03  SH02VA-PGENO            PICTURE ZZ9.
068900091109R73890     03  FILLER                  PICTURE X(16).
069000000000      /
069100000000       01  STMT-HDG-03.
069200000000           03  FILLER                  PICTURE X(8).
069300000000           03  SH03-INVESTOR-NAME      PICTURE X(45).
069400000000           03  FILLER                  PICTURE X(34).
069500000000           03  SH03-DEALER-REP-NAMES   PICTURE X(51).
069600000000           03  FILLER                  PICTURE X(12).
069700991213
069800091111R73890 01  STMT-HDG-03-VA-R            REDEFINES STMT-HDG-03.
069900091111  |        03  FILLER                  PICTURE X(7).
070000091111  |        03  SH03VA-INVESTOR-NAME    PICTURE X(45).
070100091111  |        03  FILLER                  PICTURE X(32).
070200091111  |        03  SH03VA-DEALER-REP-NAMES PICTURE X(51).
070300091111R73890     03  FILLER                  PICTURE X(15).
070400091111
070500000000       01  STMT-HDG-04.
070600000000           03  FILLER                  PICTURE X(8).
070700000000           03  SH04-INVESTOR-ADDRESS1  PICTURE X(40).
070800000000           03  FILLER                  PICTURE X(39).
070900000000           03  SH04-BRANCH-ADDR1       PICTURE X(40).
071000000000           03  FILLER                  PICTURE X(23).
071100991213
071200091111R73890 01  STMT-HDG-04-VA-R            REDEFINES STMT-HDG-04.
071300091111  |        03  FILLER                  PICTURE X(7).
071400091111  |        03  SH04VA-INVESTOR-ADDRESS1
071500091111  |                                    PICTURE X(40).
071600091111  |        03  FILLER                  PICTURE X(37).
071700091111  |        03  SH04VA-BRANCH-ADDR1     PICTURE X(40).
071800091111R73890     03  FILLER                  PICTURE X(26).
071900000000      /
072000000000       01  STMT-HDG-05.
072100000000           03  FILLER                  PICTURE X(8).
072200000000           03  SH05-INVESTOR-ADDRESS2  PICTURE X(40).
072300000000           03  FILLER                  PICTURE X(39).
072400000000           03  SH05-BRANCH-ADDR2       PICTURE X(40).
072500000000           03  FILLER                  PICTURE X(23).
072600091111
072700091111R73890 01  STMT-HDG-05-VA-R            REDEFINES STMT-HDG-05.
072800091111  |        03  FILLER                  PICTURE X(7).
072900091111  |        03  SH05VA-INVESTOR-ADDRESS2
073000091111  |                                    PICTURE X(40).
073100091111  |        03  FILLER                  PICTURE X(37).
073200091111  |        03  SH05VA-BRANCH-ADDR2     PICTURE X(40).
073300091111R73890     03  FILLER                  PICTURE X(26).
073400991213
073500000000      /
073600000000       01  STMT-HDG-06.
073700000000           03  FILLER                  PICTURE X(8).
073800000000           03  SH06-INVESTOR-ADDRESS3  PICTURE X(40).
073900000000           03  FILLER                  PICTURE X(39).
074000000000           03  SH06-BRANCH-ADDR3       PICTURE X(40).
074100000000           03  FILLER                  PICTURE X(23).
074200091111
074300091111R73890 01  STMT-HDG-06-VA-R            REDEFINES STMT-HDG-06.
074400091111  |        03  FILLER                  PICTURE X(7).
074500091111  |        03  SH06VA-INVESTOR-ADDRESS3
074600091111  |                                    PICTURE X(40).
074700091111  |        03  FILLER                  PICTURE X(37).
074800091111  |        03  SH06VA-BRANCH-ADDR3     PICTURE X(40).
074900091111R73890     03  FILLER                  PICTURE X(26).
075000000000      /
075100000000       01  STMT-HDG-07.
075200000000           03  FILLER                  PICTURE X(8).
075300000000           03  SH07-INVESTOR-ADDRESS4  PICTURE X(40).
075400000000           03  FILLER                  PICTURE X(39).
075500000000           03  SH07-BRANCH-ADDR4       PICTURE X(40).
075600000000           03  FILLER                  PICTURE X(23).
075700991213
075800091111R73890 01  STMT-HDG-07-VA-R            REDEFINES STMT-HDG-07.
075900091111  |        03  FILLER                  PICTURE X(7).
076000091111  |        03  SH07VA-INVESTOR-ADDRESS4
076100091111  |                                    PICTURE X(40).
076200091111  |        03  FILLER                  PICTURE X(37).
076300091111  |        03  SH07VA-BRANCH-ADDR4     PICTURE X(40).
076400091111R73890     03  FILLER                  PICTURE X(26).
076500000000      /
076600000000       01  STMT-HDG-08.
076700000000           03  FILLER                  PICTURE X(8).
076800000000           03  SH08-INVESTOR-ADDRESS5  PICTURE X(40).
076900000000           03  FILLER                  PICTURE X(102).
077000991213
077100091111R73890 01  STMT-HDG-08-VA-R            REDEFINES STMT-HDG-08.
077200091111  |        03  FILLER                  PICTURE X(7).
077300091111  |        03  SH08VA-INVESTOR-ADDRESS5
077400091111  |                                    PICTURE X(40).
077500091111R73890     03  FILLER                  PICTURE X(103).
077600000000      /
077700000000       01  STMT-HDG-09.
077800000000           03  FILLER                  PICTURE X(8).
077900000000           03  SH09-POSTAL-CODE        PICTURE X(10).
078000000000           03  FILLER                  PICTURE X(132).
078100991213
078200091111R73890 01  STMT-HDG-09-VA-R            REDEFINES STMT-HDG-09.
078300091111  |        03  FILLER                  PICTURE X(7).
078400091111  |        03  SH09VA-POSTAL-CODE      PICTURE X(10).
078500091111R73890     03  FILLER                  PICTURE X(133).
078600000000      /
078700000000       01  STMT-HDG-10.
078800000000           03  FILLER                  PICTURE X(8).
078900000000           03  SH10-LIT1               PICTURE X(47).
079000000000           03  FILLER                  PICTURE X(32).
079100000000           03  SH10-PHONE              PICTURE X(14).
079200000000           03  FILLER                  PICTURE X(49).
079300000000
079400091111R73890 01  STMT-HDG-10-VA-R            REDEFINES STMT-HDG-10.
079500091111  |        03  FILLER                  PICTURE X(7).
079600091111  |        03  SH10VA-LIT1             PICTURE X(47).
079700091111  |        03  FILLER                  PICTURE X(30).
079800091111  |        03  SH10VA-PHONE            PICTURE X(14).
079900091111R73890     03  FILLER                  PICTURE X(52).
080000000000      /
080100000000       01  STMT-HDG-11.
080200000000           03  FILLER                  PICTURE X(8).
080300000000           03  SH11-NOMINEE            PICTURE X(53).
080400000000           03  FILLER                  PICTURE X(26).
080500000000           03  SH11-FAX                PICTURE X(14).
080600000000           03  FILLER                  PICTURE X(49).
080700000000
080800091111R73890 01  STMT-HDG-11-VA-R            REDEFINES STMT-HDG-11.
080900091111  |        03  FILLER                  PICTURE X(7).
081000091111  |        03  SH11VA-NOMINEE          PICTURE X(53).
081100091111  |        03  FILLER                  PICTURE X(24).
081200091111  |        03  SH11VA-FAX              PICTURE X(14).
081300091111R73890     03  FILLER                  PICTURE X(52).
081400000000      /
081500000000      ******************************************************************
081600000000      *                                                                *
081700000000      *    S T A T E M E N T   D E T A I L   P R I N T   L I N E S     *
081800000000      *                                                                *
081900000000      ******************************************************************
082000000000
082100000000      *****
082200000000      *****    The statement detail lines will be re-defined for each
082300000000      *****    version of the statement.
082400091111R73890*****        Version A:  Used by MBI.
082500990927
082600000000      /
082700000000       01  STMT-DTL-01.
082800000000           03  FILLER                  PICTURE X(8).
082900000000           03  SD01-INVESTMENT-NAME    PICTURE X(35).
083000000000           03  FILLER                  PICTURE X(107).
083100991213
083200091111R73890 01  STMT-DTL-01-VA-R            REDEFINES STMT-DTL-01.
083300091130  |        03  FILLER                  PICTURE X(7).
083400091111  |        03  SD01VA-INVESTMENT-NAME  PICTURE X(35).
083500091130R73890     03  FILLER                  PICTURE X(108).
083600000000      /
083700000000       01  STMT-DTL-02.
083800000000           03  SD02-LIT1.
083900000000               05  SD02-TRADE-DATE     PICTURE X(6).
084000000000           03  FILLER                  PICTURE X(2).
084100000000           03  SD02-TRANS-TYPE         PICTURE X(25).
084200990226           03  FILLER                  PICTURE X(3).
084300000000           03  SD02-GROSS              PICTURE $$$$,$$$,$$$.99.
084400000000           03  SD02-GROSS-SIGN         PICTURE X(1).
084500000000           03  SD02-DEDUCTIONS         PICTURE $$$,$$$,$$$.99.
084600000000           03  SD02-DEDUCTIONS-SIGN    PICTURE X(1).
084700000000           03  SD02-NET                PICTURE $$$$,$$$,$$$.99.
084800000000           03  SD02-NET-SIGN           PICTURE X(1).
084900000000           03  FILLER                  PICTURE X(3).
085000000000           03  SD02-UNIT-PRICE         PICTURE ZZZ.9999.
085100000000           03  SD02-UNIT-PRICE-SIGN    PICTURE X(1).
085200000000           03  SD02-NO-OF-UNITS        PICTURE ZZZ,ZZZ,ZZZ.999.
085300000000           03  SD02-NO-OF-UNITS-SIGN   PICTURE X(1).
085400000000           03  SD02-UNIT-BALANCE       PICTURE ZZZ,ZZZ,ZZZ.999.
085500000000           03  SD02-UNIT-BALANCE-SIGN  PICTURE X(1).
085600000000           03  SD02-CPTL-GAIN-OFLIT.
085700000000               05  SD02-CPTL-GAIN      PICTURE $$$$$$.99.
085800000000               05  SD02-CPTL-GAIN-SIGN PICTURE X(1).
085900990226           03  FILLER                  PICTURE X(13).
086000000000
086100091111R73890 01  STMT-DTL-02-VA-R            REDEFINES STMT-DTL-02.
086200091111  |        03  SD02VA-LIT1.
086300091111  |            05  SD02VA-TRADE-DATE   PICTURE X(6).
086400091126  |        03  FILLER                  PICTURE X(2).
086500091111  |        03  SD02VA-TRANS-TYPE       PICTURE X(25).
086600091111  |        03  SD02VA-GROSS            PICTURE $$$$,$$$,$$$.99.
086700091111  |        03  SD02VA-GROSS-SIGN       PICTURE X(1).
086800091111  |        03  SD02VA-DEDUCTIONS       PICTURE $$$,$$$,$$$.99.
086900091111  |        03  SD02VA-DEDUCTIONS-SIGN  PICTURE X(1).
087000091111  |        03  SD02VA-NET              PICTURE $$$,$$$,$$$.99.
087100091111  |        03  SD02VA-NET-SIGN         PICTURE X(1).
087200100210R79293*    03  FILLER                  PICTURE X(1).
087300091111  |        03  SD02VA-UNIT-PRICE       PICTURE Z,ZZZ.9999.
087400091111  |        03  SD02VA-UNIT-PRICE-SIGN  PICTURE X(1).
087500091111  |        03  SD02VA-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
087600091111  |        03  SD02VA-NO-OF-UNITS-SIGN PICTURE X(1).
087700091111  |        03  SD02VA-UNIT-BALANCE     PICTURE ZZZ,ZZZ,ZZZ.999.
087800091111  |        03  SD02VA-UNIT-BALANCE-SIGN
087900091111  |                                    PICTURE X(1).
088000091119  |        03  SD02VA-CPTL-GAIN-OFLIT.
088100091111  |            05  SD02VA-CPTL-GAIN    PICTURE $$$$$$$$.99.
088200091111  |            05  SD02VA-CPTL-GAIN-SIGN
088300091111  |                                    PICTURE X(1).
088400091111R73890     03  FILLER                  PICTURE X(16).
088500000000      /
088600000000      ******************************************************************
088700000000      *                                                                *
088800000000      *    S T A T E M E N T   S U M M A R Y   P R I N T   L I N E S   *
088900000000      *                                                                *
089000000000      ******************************************************************
089100000000
089200000000      *****
089300000000      *****    The statement summary lines will be re-defined for each
089400000000      *****    version of the statement.
089500091111R73890*****        Version A:  Used by MBI.
089600990925
089700000000      /
089800000000       01  STMT-SUM-01.
089900000000           03  SS01-LIT1               PICTURE X(137).
090000000000           03  FILLER                  PICTURE X(13).
090100091124
090200091111R73890 01  STMT-SUM-01-VA-R            REDEFINES STMT-SUM-01.
090300091111  |        03  SS01VA-LIT1             PICTURE X(137).
090400091111R73890     03  FILLER                  PICTURE X(13).
090500000000      /
090600000000       01  STMT-SUM-02.
090700000000           03  FILLER                  PICTURE X(40).
090800000000           03  SS02-LIT0               PICTURE X(11).
090900050104      *RFS25032 - Begins
091000050104      *    03  FILLER                  PICTURE X(35).
091100050104           03  FILLER                  PICTURE X(22).
091200050104      *RFS25032 - ENDS
091300000000           03  SS02-LIT1               PICTURE X(8).
091400000000           03  FILLER                  PICTURE X(3).
091500000000           03  SS02-LIT2               PICTURE X(13).
091600000000           03  FILLER                  PICTURE X(3).
091700000000           03  SS02-LIT3               PICTURE X(13).
091800000000           03  FILLER                  PICTURE X(2).
091900000000           03  SS02-LIT4               PICTURE X(8).
092000000000           03  FILLER                  PICTURE X(14).
092100000000
092200091111R73890 01  STMT-SUM-02-VA-R            REDEFINES STMT-SUM-02.
092300091111  |        03  FILLER                  PICTURE X(38).
092400091111  |        03  SS02VA-LIT0             PICTURE X(11).
092500091111  |        03  FILLER                  PICTURE X(32).
092600091111  |        03  SS02VA-LIT1             PICTURE X(8).
092700091111  |        03  FILLER                  PICTURE X(3).
092800091111  |        03  SS02VA-LIT2             PICTURE X(13).
092900091111  |        03  FILLER                  PICTURE X(3).
093000091111  |        03  SS02VA-LIT3             PICTURE X(13).
093100091111  |        03  FILLER                  PICTURE X(4).
093200091111  |        03  SS02VA-LIT4             PICTURE X(8).
093300091111R73890     03  FILLER                  PICTURE X(17).
093400000000      /
093500000000       01  STMT-SUM-03.
093600000000           03  FILLER                  PICTURE X(40).
093700000000           03  SS03-LIT0               PICTURE X(11).
093800050104      *RFS25032 - Begins
093900050104      *    03  FILLER                  PICTURE X(35).
094000050104           03  FILLER                  PICTURE X(22).
094100050104      *RFS25032 - Ends
094200000000           03  SS03-LIT1               PICTURE X(8).
094300000000           03  FILLER                  PICTURE X(3).
094400000000           03  SS03-LIT2               PICTURE X(13).
094500000000           03  FILLER                  PICTURE X(3).
094600000000           03  SS03-LIT3               PICTURE X(13).
094700000000           03  FILLER                  PICTURE X(2).
094800000000           03  SS03-LIT4               PICTURE X(8).
094900000000           03  FILLER                  PICTURE X(14).
095000000000
095100091111R73890 01  STMT-SUM-03-VA-R            REDEFINES STMT-SUM-03.
095200091111  |        03  FILLER                  PICTURE X(38).
095300091111  |        03  SS03VA-LIT0             PICTURE X(11).
095400091111  |        03  FILLER                  PICTURE X(32).
095500091111  |        03  SS03VA-LIT1             PICTURE X(8).
095600091111  |        03  FILLER                  PICTURE X(3).
095700091111  |        03  SS03VA-LIT2             PICTURE X(13).
095800091111  |        03  FILLER                  PICTURE X(3).
095900091111  |        03  SS03VA-LIT3             PICTURE X(13).
096000091111  |        03  FILLER                  PICTURE X(4).
096100091111  |        03  SS03VA-LIT4             PICTURE X(8).
096200091111R73890     03  FILLER                  PICTURE X(17).
096300000000      /
096400000000       01  STMT-SUM-04.
096500000000           03  SS04-CURRENCY           PICTURE X(3).
096600000000           03  FILLER                  PICTURE X(5).
096700000000           03  SS04-INVESTMENT-NAME.
096800000000               05  FILLER              PICTURE X(27).
096900000000               05  SS04-NET-INVESTED   PICTURE $$$$$,$$$,$$$.99.
097000000000               05  SS04-NET-INVESTED-SIGN
097100000000                                       PICTURE X(1).
097200050104      *RFS25032 - Begins
097300050104      *    03  FILLER                  PICTURE X(34).
097400050104           03  FILLER                  PICTURE X(22).
097500050104      *RFS25032 - Ends
097600000000           03  SS04-LAST-UNIT-PRICE    PICTURE ZZZ.9999.
097700000000           03  SS04-LAST-UNIT-PRICE-SIGN
097800000000                                       PICTURE X(1).
097900000000           03  SS04-NO-OF-UNITS        PICTURE ZZZ,ZZZ,ZZZ.999.
098000000000           03  SS04-NO-OF-UNITS-SIGN   PICTURE X(1).
098100150212133528*    03  SS04-MARKET-VALUE       PICTURE $$$$,$$$,$$$.99.
098200150212133528     03  SS04-MARKET-VALUE       PICTURE $$$,$$$,$$$,$$$.99.
098300000000           03  SS04-MARKET-VALUE-SIGN  PICTURE X(1).
098400000000           03  FILLER                  PICTURE X(1).
098500150918      *RFS149112 STARTS
098600150918      *    03  SS04-ACB-UNIT-PRICE     PICTURE ZZZ.9999.
098700150918           03  SS04-ACB-UNIT-PRICE     PICTURE ZZ,ZZZ.9999.
098800150918      *RFS149112 ENDS
098900000000           03  SS04-ACB-UNIT-PRICE-SIGN
099000000000                                       PICTURE X(1).
099100150212133528*    03  FILLER                  PICTURE X(13).
099200150918      *RFS149112 STARTS
099300150918133528*    03  FILLER                  PICTURE X(11).
099400150918           03  FILLER                  PICTURE X(9).
099500150918      *RFS149112 ENDS
099600091124R73890 01  STMT-SUM-04-VA-R            REDEFINES STMT-SUM-04.
099700091119  |        03  SS04VA-CURRENCY         PICTURE X(3).
099800091111  |        03  FILLER                  PICTURE X(4).
099900091119  |        03  SS04VA-INVESTMENT-NAME.
100000091111  |            05  FILLER              PICTURE X(26).
100100091119  |            05  SS04VA-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
100200091119  |            05  SS04VA-NET-INVESTED-SIGN
100300091111  |                                    PICTURE X(1).
100400091111  |        03  FILLER                  PICTURE X(29).
100500091119  |        03  SS04VA-LAST-UNIT-PRICE  PICTURE Z,ZZZ.9999.
100600091119  |        03  SS04VA-LAST-UNIT-PRICE-SIGN
100700091111  |                                    PICTURE X(1).
100800091119  |        03  SS04VA-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
100900091119  |        03  SS04VA-NO-OF-UNITS-SIGN PICTURE X(1).
101000150212      *RFS 133528 - BEGINS.
101100150212  |   *    03  SS04VA-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99.
101200150212           03  SS04VA-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99.
101300150212      *RFS 133528 - ENDS.
101400091119  |        03  SS04VA-MARKET-VALUE-SIGN
101500091111  |                                    PICTURE X(1).
101600091111  |        03  FILLER                  PICTURE X(1).
101700150918      *RFS149112 STARTS
101800150918  |   *    03  SS04VA-ACB-UNIT-PRICE   PICTURE Z,ZZZ.9999.
101900150918  |        03  SS04VA-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
102000150918      *RFS149112 ENDS
102100091119  |        03  SS04VA-ACB-UNIT-PRICE-SIGN
102200091111  |                                    PICTURE X(1).
102300091111R73890     03  FILLER                  PICTURE X(16).
102400150918      *RFS149112 STARTS
102500150918133528*    03  FILLER                  PICTURE X(14).
102600150918           03  FILLER                  PICTURE X(12).
102700150918      *RFS149112 ENDS
102800000000      /
102900000000       01  STMT-SUM-05.
103000050104      *RFS25032 - Begins
103100050104      *    03  FILLER                  PICTURE X(113).
103200050104           03  FILLER                  PICTURE X(100).
103300050104      *RFS25032 - Ends
103400000000           03  SS05-LIT1               PICTURE X(14).
103500000000           03  FILLER                  PICTURE X(23).
103600000000
103700091123R73890 01  STMT-SUM-05-VA-R            REDEFINES STMT-SUM-05.
103800091123  |        03  FILLER                  PICTURE X(108).
103900091123  |        03  SS05VA-LIT1             PICTURE X(14).
104000091123R73890     03  FILLER                  PICTURE X(28).
104100000000      /
104200000000       01  STMT-SUM-06.
104300000000           03  FILLER                  PICTURE X(8).
104400000000           03  SS06-LIT1               PICTURE X(35).
104500050104      *RFS25032 - Begins
104600050104      *    03  FILLER                  PICTURE X(38).
104700050104           03  FILLER                  PICTURE X(26).
104800050104      *RFS25032 - Ends
104900000000           03  SS06-LIT-CURRDESC       PICTURE X(30).
105000150212      *RFS 133528 - BEGINS.
105100150212      *    03  SS06-MARKET-VALUE       PICTURE $$$$,$$$,$$$.99-.
105200150212      *    03  FILLER                  PICTURE X(23).
105300150212           03  SS06-MARKET-VALUE       PICTURE $$$$,$$$,$$$,$$$.99-.
105400150212           03  FILLER                  PICTURE X(20).
105500150212      * RFS 133528 - END
105600000000
105700091123R73890 01  STMT-SUM-06-VA-R            REDEFINES STMT-SUM-06.
105800091123  |        03  FILLER                  PICTURE X(7).
105900091123  |        03  SS06VA-LIT1             PICTURE X(35).
106000091123  |        03  FILLER                  PICTURE X(31).
106100091123  |        03  SS06VA-LIT-CURRDESC     PICTURE X(30).
106200091123  |        03  SS06VA-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99-.
106300091123R73890     03  FILLER                  PICTURE X(28).
106400981207      /
106500091124       01  STMT-SUM-07-V1.
106600091124           03  SS07V1-YEAR             PICTURE X(4).
106700091124           03  FILLER                  PICTURE X(3).
106800091124           03  SS07V1-LIT              PICTURE X(25).
106900091124           03  FILLER                  PICTURE X(1).
107000091124           03  SS07V1-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
107100091124           03  FILLER                  PICTURE X(102).
107200091124
107300091124       01  STMT-SUM-08-V1.
107400091124           03  SS08V1-YEAR             PICTURE X(4).
107500091124           03  FILLER                  PICTURE X(3).
107600091124           03  SS08V1-LIT              PICTURE X(25).
107700091124           03  FILLER                  PICTURE X(1).
107800091124           03  SS08V1-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
107900091124           03  FILLER                  PICTURE X(102).
108000091124
108100981207       01  STMT-SUM-11.
108200981207           03  SS11-LIT1               PICTURE X(29).
108300971223           03  FILLER                  PICTURE X(7).
108400981207           03  SS11-CPG-FIXED          PICTURE $$$$,$$$,$$$.99.
108500981207           03  SS11-CPG-FIXED-SIGN     PICTURE X(1).
108600971223           03  FILLER                  PICTURE X(98).
108700971223
108800091123R73890 01  STMT-SUM-11-VA-R            REDEFINES STMT-SUM-11.
108900091123  |        03  SS11VA-LIT1             PICTURE X(29).
109000091123  |        03  FILLER                  PICTURE X(5).
109100091123  |        03  SS11VA-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
109200091123  |        03  SS11VA-CPG-FIXED-SIGN   PICTURE X(1).
109300091123R73890     03  FILLER                  PICTURE X(100).
109400000000      /
109500000000      ******************************************************************
109600000000      *                                                                *
109700000000      *    S T A T E M E N T   M E S S A G E   P R I N T   L I N E S   *
109800000000      *                                                                *
109900000000      ******************************************************************
110000000000
110100000000       01  STMT-MSG-01.
110200000000           03  FILLER                  PICTURE X(7).
110300000000           03  SM01-STATEMENT-MESSAGE  PICTURE X(115).
110400000000           03  FILLER                  PICTURE X(28).
110500000000      /
110600990623      ******************************************************************
110700990623      *                                                                *
110800990623      *    S T A T E M E N T   H E A D I N G  L I T E R A L S          *
110900990623      *                                                                *
111000990623      ******************************************************************
111100990623      /
111200990623       01  WS-STMT-HDG-LITS.
111300990623
111400990623      *****
111500990623      *****    English Literals.
111600990623      *****
111700990623
111800990623           03  WS-SH-HDGLIT01-E.
111900990623               05  FILLER              PICTURE X(50)       VALUE
112000990623                   "--------------------------------------------------".
112100990623               05  FILLER              PICTURE X(50)       VALUE
112200990623                   "--------------------------------------------------".
112300990623               05  FILLER              PICTURE X(50)       VALUE
112400990623                   "-------------------------------------             ".
112500990923
112600990623           03  WS-SH-HDGLIT02-E.
112700990623               05  FILLER              PICTURE X(50)       VALUE
112800990624                   " TRADE                                       GROSS".
112900990623               05  FILLER              PICTURE X(50)       VALUE
113000990624                   "                   NET INVESTED/      PRICE       ".
113100990623               05  FILLER              PICTURE X(50)       VALUE
113200990624                   "                     UNIT                         ".
113300990623           03  WS-SH-HDGLIT03-E.
113400990623               05  FILLER              PICTURE X(50)       VALUE
113500990624                   " DATE       TRANSACTION TYPE                 AMOUN".
113600990623               05  FILLER              PICTURE X(50)       VALUE
113700990624                   "T      DEDUCTIONS       PROCEEDS   PER UNIT       ".
113800990623               05  FILLER              PICTURE X(50)       VALUE
113900990624                   "    UNITS         BALANCE                         ".
114000990623
114100990623           03  WS-SH-HDGLIT04-E.
114200990623               05  FILLER              PICTURE X(50)       VALUE
114300990623                   "--------------------------------------------------".
114400990623               05  FILLER              PICTURE X(50)       VALUE
114500990623                   "--------------------------------------------------".
114600990623               05  FILLER              PICTURE X(50)       VALUE
114700990623                   "-------------------------------------             ".
114800990623
114900990923           03  WS-SH-HDGLIT06-1-E.
115000000223               05  FILLER              PICTURE X(7).
115100000222               05  FILLER              PICTURE X(40)       VALUE
115200000222                   "----------------------------------------"          .
115300000222               05  FILLER              PICTURE X(40)       VALUE
115400000222                   "----------------------------------------"          .
115500000222               05  FILLER              PICTURE X(24)       VALUE
115600000222                   "------------------------"                          .
115700000223               05  FILLER              PICTURE X(4).
115800990923
115900990922           03  WS-SH-HDGLIT06-2-E.
116000000223               05  FILLER              PICTURE X(8).
116100990923               05  FILLER              PICTURE X(10)  VALUE
116200990923                   "DATE      ".
116300991208               05  FILLER              PICTURE X(1).
116400991207               05  FILLER              PICTURE X(25)  VALUE
116500991207                   "DETAILS                  ".
116600000222               05  FILLER              PICTURE X(3).
116700990923               05  FILLER              PICTURE X(15)   VALUE
116800990924                   "         AMOUNT".
116900000222               05  FILLER              PICTURE X(4).
117000990924               05  FILLER              PICTURE X(8)   VALUE
117100990924                   "    UNIT".
117200990924               05  FILLER              PICTURE X(2).
117300990923               05  FILLER              PICTURE X(15)   VALUE
117400990924                   "          UNITS".
117500000222               05  FILLER              PICTURE X(4).
117600990923               05  FILLER              PICTURE X(15)   VALUE
117700990924                   "         MARKET".
117800000223               05  FILLER              PICTURE X(5).
117900990923
118000990923           03  WS-SH-HDGLIT06-3-E.
118100000223               05  FILLER              PICTURE X(8).
118200990923               05  FILLER              PICTURE X(10)  VALUE
118300990923                   "          ".
118400991208               05  FILLER              PICTURE X(1).
118500991207               05  FILLER              PICTURE X(25)  VALUE
118600991207                   "                         ".
118700000222               05  FILLER              PICTURE X(3).
118800990923               05  FILLER              PICTURE X(15)   VALUE
118900990923                   "               ".
119000000222               05  FILLER              PICTURE X(4).
119100990924               05  FILLER              PICTURE X(8)   VALUE
119200990924                   "   PRICE".
119300990924               05  FILLER              PICTURE X(2).
119400990923               05  FILLER              PICTURE X(15)   VALUE
119500990923                   "               ".
119600000222               05  FILLER              PICTURE X(4).
119700990923               05  FILLER              PICTURE X(15)   VALUE
119800990924                   "          VALUE".
119900000223               05  FILLER              PICTURE X(5).
120000990923
120100990923           03  WS-SH-HDGLIT06-4-E.
120200000223               05  FILLER              PICTURE X(7).
120300000222               05  FILLER              PICTURE X(40)       VALUE
120400000222                   "----------------------------------------"          .
120500000222               05  FILLER              PICTURE X(40)       VALUE
120600000222                   "----------------------------------------"          .
120700000222               05  FILLER              PICTURE X(24)       VALUE
120800000222                   "------------------------"                          .
120900000223               05  FILLER              PICTURE X(4).
121000990922
121100991019           03  WS-SH-HDGLIT07-1-E.
121200991019               05  FILLER              PICTURE X(50)       VALUE
121300991019                   "--------------------------------------------------".
121400991019               05  FILLER              PICTURE X(50)       VALUE
121500991019                   "--------------------------------------------------".
121600991019               05  FILLER              PICTURE X(50)       VALUE
121700991019                   "-------------------------------------             ".
121800991019
121900991019           03  WS-SH-HDGLIT07-2-E.
122000991019               05  FILLER              PICTURE X(8)        VALUE
122100991019                   " TRADE ".
122200991021               05  FILLER              PICTURE X(1).
122300991019               05  FILLER              PICTURE X(25).
122400991020               05  FILLER              PICTURE X(1).
122500991019               05  FILLER              PICTURE X(15)       VALUE
122600991019                   "          GROSS".
122700991022               05  FILLER              PICTURE X(1).
122800991022               05  FILLER              PICTURE X(15).
122900991022               05  FILLER              PICTURE X(1).
123000991019               05  FILLER              PICTURE X(14)       VALUE
123100991019                   " NET INVESTED/".
123200991022               05  FILLER              PICTURE X(1).
123300991019               05  FILLER              PICTURE X(8)        VALUE
123400991019                   "   PRICE"                                          .
123500991022               05  FILLER              PICTURE X(1).
123600991022               05  FILLER              PICTURE X(15).
123700991022               05  FILLER              PICTURE X(1).
123800991022               05  FILLER              PICTURE X(14)       VALUE
123900991022                   "          UNIT".
124000991022               05  FILLER              PICTURE X(1).
124100991022               05  FILLER              PICTURE X(28).
124200991019
124300991019           03  WS-SH-HDGLIT07-3-E.
124400991019               05  FILLER              PICTURE X(8)        VALUE
124500991019                   " DATE  ".
124600991021               05  FILLER              PICTURE X(1).
124700991019               05  FILLER              PICTURE X(25)       VALUE
124800991019                   "TRANSACTION TYPE        ".
124900991020               05  FILLER              PICTURE X(1).
125000991019               05  FILLER              PICTURE X(15)       VALUE
125100991019                   "         AMOUNT".
125200991022               05  FILLER              PICTURE X(1).
125300991022               05  FILLER              PICTURE X(15)       VALUE
125400991022                   "     DEDUCTIONS".
125500991022               05  FILLER              PICTURE X(1).
125600991019               05  FILLER              PICTURE X(14)       VALUE
125700991019                   "      PROCEEDS".
125800991022               05  FILLER              PICTURE X(1).
125900991019               05  FILLER              PICTURE X(8)        VALUE
126000991019                   "PER UNIT"                                          .
126100991022               05  FILLER              PICTURE X(1).
126200991019               05  FILLER              PICTURE X(15)       VALUE
126300991019                   "          UNITS".
126400991022               05  FILLER              PICTURE X(1).
126500991022               05  FILLER              PICTURE X(14)       VALUE
126600991022                   "       BALANCE".
126700991022               05  FILLER              PICTURE X(1).
126800991022               05  FILLER              PICTURE X(28).
126900991019
127000991019           03  WS-SH-HDGLIT07-4-E.
127100991019               05  FILLER              PICTURE X(50)       VALUE
127200991019                   "--------------------------------------------------".
127300991019               05  FILLER              PICTURE X(50)       VALUE
127400991019                   "--------------------------------------------------".
127500991019               05  FILLER              PICTURE X(50)       VALUE
127600991019                   "-------------------------------------             ".
127700991019
127800990623      *****
127900990623      *****    French Literals.
128000990623      *****
128100990623
128200990623           03  WS-SH-HDGLIT01-F.
128300990623               05  FILLER              PICTURE X(50)       VALUE
128400990623                   "--------------------------------------------------".
128500990623               05  FILLER              PICTURE X(50)       VALUE
128600990623                   "--------------------------------------------------".
128700990623               05  FILLER              PICTURE X(50)       VALUE
128800990623                   "-------------------------------------             ".
128900990923
129000990623           03  WS-SH-HDGLIT02-F.
129100990623               05  FILLER              PICTURE X(50)       VALUE
129200990624                   "                                             MONTA".
129300990623               05  FILLER              PICTURE X(50)       VALUE
129400030825                   "NT                   MONTANT NET        PRIX      ".
129500990623               05  FILLER              PICTURE X(50)       VALUE
129600990624                   "                 BALANCE                          ".
129700990623           03  WS-SH-HDGLIT03-F.
129800990623               05  FILLER              PICTURE X(50)       VALUE
129900990624                   " DATE       GENRE DE TRANSACTION             BRUT ".
130000990623               05  FILLER              PICTURE X(50)       VALUE
130100990624                   "       DEDUCTIONS  INVESTI/PRODUIT  UNITAIRE      ".
130200990623               05  FILLER              PICTURE X(50)       VALUE
130300030825                   "    UNITS        DE L'UNITE                       ".
130400990623
130500990623           03  WS-SH-HDGLIT04-F.
130600990623               05  FILLER              PICTURE X(50)       VALUE
130700990623                   "--------------------------------------------------".
130800990623               05  FILLER              PICTURE X(50)       VALUE
130900990623                   "--------------------------------------------------".
131000990623               05  FILLER              PICTURE X(50)       VALUE
131100990623                   "-------------------------------------             ".
131200990623
131300990923           03  WS-SH-HDGLIT06-1-F.
131400000223               05  FILLER              PICTURE X(7).
131500000222               05  FILLER              PICTURE X(40)       VALUE
131600000222                   "----------------------------------------"          .
131700000222               05  FILLER              PICTURE X(40)       VALUE
131800000222                   "----------------------------------------"          .
131900000222               05  FILLER              PICTURE X(24)       VALUE
132000000222                   "------------------------"                          .
132100000223               05  FILLER              PICTURE X(4).
132200000222
132300990923           03  WS-SH-HDGLIT06-2-F.
132400000223               05  FILLER              PICTURE X(8).
132500000222               05  FILLER              PICTURE X(10)  VALUE
132600000222                   "DATE      ".
132700000222               05  FILLER              PICTURE X(1).
132800000222               05  FILLER              PICTURE X(25)  VALUE
132900000222                   "DETAILS                  ".
133000000222               05  FILLER              PICTURE X(3).
133100000222               05  FILLER              PICTURE X(15)   VALUE
133200000222                   "         AMOUNT".
133300000222               05  FILLER              PICTURE X(4).
133400000222               05  FILLER              PICTURE X(8)   VALUE
133500000222                   "    UNIT".
133600000222               05  FILLER              PICTURE X(2).
133700000222               05  FILLER              PICTURE X(15)   VALUE
133800000222                   "          UNITS".
133900000222               05  FILLER              PICTURE X(4).
134000000222               05  FILLER              PICTURE X(15)   VALUE
134100000222                   "         MARKET".
134200000224               05  FILLER              PICTURE X(5).
134300000222
134400990923           03  WS-SH-HDGLIT06-3-F.
134500000223               05  FILLER              PICTURE X(8).
134600000222               05  FILLER              PICTURE X(10)  VALUE
134700000222                   "          ".
134800000222               05  FILLER              PICTURE X(1).
134900000222               05  FILLER              PICTURE X(25)  VALUE
135000000222                   "                         ".
135100000222               05  FILLER              PICTURE X(3).
135200000222               05  FILLER              PICTURE X(15)   VALUE
135300000222                   "               ".
135400000222               05  FILLER              PICTURE X(4).
135500000222               05  FILLER              PICTURE X(8)   VALUE
135600000222                   "   PRICE".
135700000222               05  FILLER              PICTURE X(2).
135800000222               05  FILLER              PICTURE X(15)   VALUE
135900000222                   "               ".
136000000222               05  FILLER              PICTURE X(4).
136100000222               05  FILLER              PICTURE X(15)   VALUE
136200000222                   "          VALUE".
136300000224               05  FILLER              PICTURE X(5).
136400990923
136500990923           03  WS-SH-HDGLIT06-4-F.
136600000223               05  FILLER              PICTURE X(7).
136700000222               05  FILLER              PICTURE X(40)       VALUE
136800000222                   "----------------------------------------"          .
136900000222               05  FILLER              PICTURE X(40)       VALUE
137000000222                   "----------------------------------------"          .
137100000222               05  FILLER              PICTURE X(24)       VALUE
137200000222                   "------------------------"                          .
137300000223               05  FILLER              PICTURE X(4).
137400000222
137500991019           03  WS-SH-HDGLIT07-1-F.
137600991019               05  FILLER              PICTURE X(50)       VALUE
137700991019                   "--------------------------------------------------".
137800991019               05  FILLER              PICTURE X(50)       VALUE
137900991019                   "--------------------------------------------------".
138000991019               05  FILLER              PICTURE X(50)       VALUE
138100991019                   "-------------------------------------             ".
138200991019
138300991019           03  WS-SH-HDGLIT07-2-F.
138400991019               05  FILLER              PICTURE X(8).
138500991021               05  FILLER              PICTURE X(1).
138600991019               05  FILLER              PICTURE X(25).
138700991020               05  FILLER              PICTURE X(1).
138800991019               05  FILLER              PICTURE X(15)       VALUE
138900991019                   "        MONTANT".
139000991020               05  FILLER              PICTURE X(2).
139100991020               05  FILLER              PICTURE X(16).
139200991019               05  FILLER              PICTURE X(15)       VALUE
139300991019                   "    MONTANT NET".
139400991020               05  FILLER              PICTURE X(1).
139500991019               05  FILLER              PICTURE X(8)        VALUE
139600991019                   "    PRIX"                                          .
139700991020               05  FILLER              PICTURE X(19).
139800991019               05  FILLER              PICTURE X(15)       VALUE
139900991019                   "        BALANCE".
140000991021               05  FILLER              PICTURE X(24).
140100991019
140200991019           03  WS-SH-HDGLIT07-3-F.
140300991019               05  FILLER              PICTURE X(8)        VALUE
140400991019                   " DATE  ".
140500991021               05  FILLER              PICTURE X(1).
140600991019               05  FILLER              PICTURE X(25)       VALUE
140700991019                   "GENRE DE TRANSACTION    ".
140800991020               05  FILLER              PICTURE X(1).
140900991019               05  FILLER              PICTURE X(15)       VALUE
141000991019                   "           BRUT".
141100991020               05  FILLER              PICTURE X(2).
141200991019               05  FILLER              PICTURE X(14)       VALUE
141300991019                   "    DEDUCTIONS".
141400991020               05  FILLER              PICTURE X(2).
141500991019               05  FILLER              PICTURE X(15)       VALUE
141600991019                   "INVESTI/PRODUIT".
141700991020               05  FILLER              PICTURE X(1).
141800991019               05  FILLER              PICTURE X(8)        VALUE
141900991019                   "UNITAIRE"                                          .
142000991020               05  FILLER              PICTURE X(2).
142100991019               05  FILLER              PICTURE X(15)       VALUE
142200991019                   "          UNITS".
142300991020               05  FILLER              PICTURE X(2).
142400991019               05  FILLER              PICTURE X(15)       VALUE
142500991019                   "     DE L'UNITE".
142600991021               05  FILLER              PICTURE X(24).
142700991019
142800991019           03  WS-SH-HDGLIT07-4-F.
142900991019               05  FILLER              PICTURE X(50)       VALUE
143000991019                   "--------------------------------------------------".
143100991019               05  FILLER              PICTURE X(50)       VALUE
143200991019                   "--------------------------------------------------".
143300991019               05  FILLER              PICTURE X(50)       VALUE
143400991019                   "-------------------------------------             ".
143500990923
143600990623      *****
143700990623      *****    Statement Heading Literals.
143800990623      *****
143900990623
144000990623       01  WS-STMT-HDG-LITS-R          REDEFINES WS-STMT-HDG-LITS.
144100990623           03  WS-STMT-HDG-LIT-LNG     OCCURS   2  TIMES
144200990623                                       INDEXED BY  SHL.
144300990623               05  WS-SHL01            PICTURE X(150).
144400990623               05  WS-SHL02            PICTURE X(150).
144500990623               05  WS-SHL03            PICTURE X(150).
144600990623               05  WS-SHL04            PICTURE X(150).
144700000222               05  WS-SHL06-1          PICTURE X(115).
144800000222               05  WS-SHL06-2          PICTURE X(115).
144900000222               05  WS-SHL06-3          PICTURE X(115).
145000000222               05  WS-SHL06-4          PICTURE X(115).
145100991019               05  WS-SHL07-1          PICTURE X(150).
145200991019               05  WS-SHL07-2          PICTURE X(150).
145300991019               05  WS-SHL07-3          PICTURE X(150).
145400991019               05  WS-SHL07-4          PICTURE X(150).
145500991019
145600990623      /
145700000000      ******************************************************************
145800000000      *                                                                *
145900000000      *    S T A T E M E N T   L I T E R A L S                         *
146000000000      *                                                                *
146100000000      ******************************************************************
146200000000
146300000000       01  WS-STMT-LITS.
146400000000
146500000000      *****
146600000000      *****    English Literals.
146700000000      *****
146800000000
146900000000           03  WS-SH01-LIT1-E          PICTURE X(20)       VALUE
147000000000               "  TRANSACTIONS FROM ".
147100000000           03  WS-SH01-LIT2-E          PICTURE X(2)        VALUE "TO".
147200000000           03  WS-SH10-LIT1-E          PICTURE X(47)       VALUE
147300000000               "PLEASE NOTE THAT THIS ACCOUNT IS REGISTERED TO:".
147400091202           03  WS-SH10-LIT2-E          PICTURE X(12)       VALUE
147500990923               "ACCOUNT NO :".
147600000000           03  WS-SD02-LIT1-E          PICTURE X(6)        VALUE
147700000000               "YYMMDD".
147800000000           03  WS-SS01-LIT1-E.
147900000000               05  FILLER              PICTURE X(49)       VALUE
148000091202                   "                                                 ".
148100000000               05  FILLER              PICTURE X(39)       VALUE
148200000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
148300000000               05  FILLER              PICTURE X(50)       VALUE
148400091202                   "                                                  ".
148500091123
148600000000           03  WS-SS02-LIT0-E          PICTURE X(11)       VALUE
148700000000               "        NET".
148800000000           03  WS-SS02-LIT1-E          PICTURE X(8)        VALUE
148900000000               "    UNIT".
149000000000           03  WS-SS02-LIT2-E          PICTURE X(13)       VALUE
149100000000               "         UNIT".
149200000000           03  WS-SS02-LIT3-E          PICTURE X(13)       VALUE
149300000000               "       MARKET".
149400000000           03  WS-SS02-LIT4-E          PICTURE X(8)        VALUE
149500000000               " AVERAGE".
149600000000           03  WS-SS03-LIT0-E          PICTURE X(11)       VALUE
149700000000               "   INVESTED".
149800000000           03  WS-SS03-LIT1-E          PICTURE X(8)        VALUE
149900000000               "   PRICE".
150000000000           03  WS-SS03-LIT2-E          PICTURE X(13)       VALUE
150100000000               "      BALANCE".
150200000000           03  WS-SS03-LIT3-E          PICTURE X(13)       VALUE
150300000000               "        VALUE".
150400000000           03  WS-SS03-LIT4-E          PICTURE X(8)        VALUE
150500000000               "    COST".
150600000000           03  WS-SS05-LIT1-E          PICTURE X(14)       VALUE
150700000000               " -------------".
150800000000           03  WS-SS05V1-LIT1-E        PICTURE X(13)       VALUE
150900000000               " ------------".
151000990611           03  WS-SS05V2-LIT1-E        PICTURE X(13)       VALUE
151100990226               " ------------".
151200000000           03  WS-SS05V3-LIT1-E        PICTURE X(14)       VALUE
151300000000               " -------------".
151400000000           03  WS-SS05V4-LIT1-E        PICTURE X(14)       VALUE
151500000000               " -------------".
151600000000           03  WS-SS05V5-LIT1-E        PICTURE X(14)       VALUE
151700000000               " -------------".
151800990922           03  WS-SS05V6-LIT1-E        PICTURE X(14)       VALUE
151900990922               " -------------".
152000000000           03  WS-SS06-LIT1-E          PICTURE X(35)       VALUE
152100000000               "ACCOUNT VALUE                      ".
152200981224           03  WS-SS07V1-LIT-E1          PICTURE X(25)       VALUE
152300981224               "MINIMUM RRIF PAYMENT:    ".
152400981224           03  WS-SS07V1-LIT-E2          PICTURE X(25)       VALUE
152500981224               "MINIMUM LIF PAYMENT :    ".
152600981224           03  WS-SS08V1-LIT-E           PICTURE X(25)       VALUE
152700981224               "MAXIMUM LIF PAYMENT :    ".
152800981207           03  WS-SS11-LIT1-E          PICTURE X(29)       VALUE
152900971223               "CANADIAN DOLLAR CAPITAL GAINS".
153000000000
153100000000      *****
153200000000      *****    French Literals.
153300000000      *****
153400000000
153500000000           03  WS-SH01-LIT1-F          PICTURE X(20)       VALUE
153600990611               "LES TRANSACTIONS DE ".
153700000000           03  WS-SH01-LIT2-F          PICTURE X(2)        VALUE "A ".
153800000000           03  WS-SH10-LIT1-F          PICTURE X(47)       VALUE
153900000000               "VEUILLEZ NOTER CE COMPTE EST ENREGISTRE A      ".
154000990923           03  WS-SH10-LIT2-F          PICTURE X(12)       VALUE
154100990923               "ACCOUNT NO :".
154200000000           03  WS-SD02-LIT1-F          PICTURE X(6)        VALUE
154300000000               "AAMMJJ".
154400000000           03  WS-SS01-LIT1-F.
154500000000               05  FILLER              PICTURE X(47)       VALUE
154600000000                   "* * * * * * * * * * * * * * * * * * * * * * * *".
154700000000               05  FILLER              PICTURE X(43)       VALUE
154800030825R18092             "   Position du compte à la date du relevé  ".
154900000000               05  FILLER              PICTURE X(48)       VALUE
155000000000                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
155100990929
155200971223           03  WS-SS02-LIT0-F          PICTURE X(11)       VALUE
155300971223               "    MONTANT".
155400000000           03  WS-SS02-LIT1-F          PICTURE X(8)        VALUE
155500030825R18092         " Prix de".
155600000000           03  WS-SS02-LIT2-F          PICTURE X(13)       VALUE
155700030825R18092         "             ".
155800000000           03  WS-SS02-LIT3-F          PICTURE X(13)       VALUE
155900030825R18092         "       Valeur".
156000000000           03  WS-SS02-LIT4-F          PICTURE X(8)        VALUE
156100030825R18092         "    Coût".
156200000000           03  WS-SS03-LIT0-F          PICTURE X(11)       VALUE
156300000000               "NET INVESTI".
156400000000           03  WS-SS03-LIT1-F          PICTURE X(8)        VALUE
156500030825R18092         " l'unité".
156600000000           03  WS-SS03-LIT2-F          PICTURE X(13)       VALUE
156700030825R18092         "        Solde".
156800000000           03  WS-SS03-LIT3-F          PICTURE X(13)       VALUE
156900030825R18092         "    marchande".
157000000000           03  WS-SS03-LIT4-F          PICTURE X(8)        VALUE
157100030825R18092         "   moyen".
157200000000           03  WS-SS05-LIT1-F          PICTURE X(14)       VALUE
157300000000               " -------------".
157400000000           03  WS-SS05V1-LIT1-F        PICTURE X(13)       VALUE
157500000000               " ------------".
157600990611           03  WS-SS05V2-LIT1-F        PICTURE X(13)       VALUE
157700990226               " ------------".
157800000000           03  WS-SS05V3-LIT1-F        PICTURE X(14)       VALUE
157900000000               " -------------".
158000000000           03  WS-SS05V4-LIT1-F        PICTURE X(14)       VALUE
158100000000               " -------------".
158200000000           03  WS-SS05V5-LIT1-F        PICTURE X(14)       VALUE
158300000000               " -------------".
158400990922           03  WS-SS05V6-LIT1-F        PICTURE X(14)       VALUE
158500990922               " -------------".
158600000000           03  WS-SS06-LIT1-F          PICTURE X(35)       VALUE
158700000000               "VALEUR DU COMPTE                   ".
158800981224           03  WS-SS07V1-LIT-F1          PICTURE X(25)       VALUE
158900981224               "VERSEMENT MINIMUM DE FER:".
159000981224           03  WS-SS07V1-LIT-F2          PICTURE X(25)       VALUE
159100981224               "VERSEMENT MINIMUM DE FRV:".
159200981224           03  WS-SS08V1-LIT-F           PICTURE X(25)       VALUE
159300981224               "VERSEMENT MAXIMUM DE FRV:".
159400981207           03  WS-SS11-LIT1-F          PICTURE X(29)       VALUE
159500971223               "GAINS EN CAPITAL EN CAN $    ".
159600000000
159700000000       01  WS-STMT-LITS-R              REDEFINES WS-STMT-LITS.
159800000000           03  WS-STMT-LIT             OCCURS   2  TIMES
159900000000                                       INDEXED BY  SL.
160000000000
160100000000      *****
160200000000      *****    Heading Literals.
160300000000      *****
160400000000
160500000000               05  WS-SH01-LIT1        PICTURE X(20).
160600000000               05  WS-SH01-LIT2        PICTURE X(2).
160700000000               05  WS-SH10-LIT1        PICTURE X(47).
160800990923               05  WS-SH10-LIT2        PICTURE X(12).
160900000000
161000000000      *****
161100000000      *****    Detail Literals.
161200000000      *****
161300000000
161400000000               05  WS-SD02-LIT1        PICTURE X(6).
161500000000
161600000000      *****
161700000000      *****    Summary Literals.
161800000000      *****
161900000000
162000000000               05  WS-SS01-LIT1        PICTURE X(138).
162100000000               05  WS-SS02-LIT0        PICTURE X(11).
162200000000               05  WS-SS02-LIT1        PICTURE X(8).
162300000000               05  WS-SS02-LIT2        PICTURE X(13).
162400000000               05  WS-SS02-LIT3        PICTURE X(13).
162500000000               05  WS-SS02-LIT4        PICTURE X(8).
162600000000               05  WS-SS03-LIT0        PICTURE X(11).
162700000000               05  WS-SS03-LIT1        PICTURE X(8).
162800000000               05  WS-SS03-LIT2        PICTURE X(13).
162900000000               05  WS-SS03-LIT3        PICTURE X(13).
163000000000               05  WS-SS03-LIT4        PICTURE X(8).
163100000000               05  WS-SS05-LIT1        PICTURE X(14).
163200000000               05  WS-SS05V1-LIT1      PICTURE X(13).
163300990226               05  WS-SS05V2-LIT1      PICTURE X(13).
163400000000               05  WS-SS05V3-LIT1      PICTURE X(14).
163500000000               05  WS-SS05V4-LIT1      PICTURE X(14).
163600000000               05  WS-SS05V5-LIT1      PICTURE X(14).
163700990922               05  WS-SS05V6-LIT1      PICTURE X(14).
163800000000               05  WS-SS06-LIT1        PICTURE X(35).
163900981224               05  WS-SS07V1-LIT1      PICTURE X(25).
164000981224               05  WS-SS07V1-LIT2      PICTURE X(25).
164100981224               05  WS-SS08V1-LIT       PICTURE X(25).
164200981207               05  WS-SS11-LIT1        PICTURE X(29).
164300000000      /
164400000000      ******************************************************************
164500000000      *                                                                *
164600000000      *    S T A T E M E N T   L I T E R A L S   ( C U R R E N C Y )   *
164700000000      *                                                                *
164800000000      ******************************************************************
164900000000
165000000000       01  WS-CURRENCY-TBL.
165100000000           03  WS-SC-CAD.
165200030827               05  FILLER              PICTURE X(3)        VALUE "CAD".
165300000000               05  FILLER              PICTURE X(30)       VALUE
165400030825R18092             "CANADIAN DOLLAR TOTAL         ".
165500000000               05  FILLER              PICTURE X(30)       VALUE
165600030825R18092             "TOTAL DOLLARS CANADIENS       ".
165700000223      *        05  FILLER              PICTURE X(7).
165800000223               05  FILLER              PICTURE X(8).
165900000000           03  WS-SC-USD.
166000030827               05  FILLER              PICTURE X(3)        VALUE "USD".
166100000000               05  FILLER              PICTURE X(30)       VALUE
166200030825R18092             "US DOLLAR TOTAL               ".
166300000000               05  FILLER              PICTURE X(30)       VALUE
166400030825R18092             "TOTAL DOLLARS AMERICAINS      ".
166500000223      *        05  FILLER              PICTURE X(7).
166600000223               05  FILLER              PICTURE X(8).
166700000000
166800000000       01  WS-CURRENCY-TBL-R           REDEFINES WS-CURRENCY-TBL.
166900030827           03  WS-SC-ENTRY             OCCURS   2  TIMES
167000000000                                       INDEXED BY  SC.
167100000000             04  WS-SC-CURRENCY        PICTURE X(3).
167200000000             04  WS-SC-CURRDESC        OCCURS   2  TIMES
167300000000                                       INDEXED BY  SCL
167400000223                                       PICTURE X(30).
167500000223      *      04  WS-SC-MARKET-VALUE    PICTURE S9(9)V9(2)  COMP-3.
167600000223             04  WS-SC-MARKET-VALUE    PICTURE S9(11)V9(2) COMP-3.
167700000000             04  WS-SC-RPTIND          PICTURE X(1).
167800000000      /
167900000000       01  WS-STMT-INVESTOR-ACCOUNT-TBL.
168000000000
168100000000      ******************************************************************
168200000000      *                                                                *
168300000000      *    S T A T E M E N T   I N V E S T O R / A C C O U N T   T B L *
168400000000      *                                                                *
168500000000      ******************************************************************
168600000000
168700011211           03  WS-SIA-ENT              OCCURS        1000  TIMES
168800000000                                       ASCENDING KEY   IS  WS-SIA-ACCT
168900000000                                       INDEXED         BY  SIA
169000000000                                                           SIA1
169100000000                                                           SIA2.
169200000000               05  WS-SIA-ACCT         PICTURE X(9).
169300000000               05  WS-SIA-NOMCDE       PICTURE X(5).
169400000000               05  WS-SIA-NOMNAM       PICTURE X(35).
169500000000               05  WS-SIA-NOMACCT      PICTURE X(15).
169600000000               05  WS-SIA-REGCTB       PICTURE 9(9)V9(2)   COMP-3.
169700000000               05  WS-SIA-REGCTBSGN    PICTURE X(1).
169800000000      /
169900000000       01  WS-STMT-ACCOUNT-FUND-TABLE.
170000000000
170100000000      ******************************************************************
170200000000      *                                                                *
170300000000      *    S T A T E M E N T   A C C O U N T / F U N D   T A B L E     *
170400000000      *                                                                *
170500000000      ******************************************************************
170600000000
17070001121112090 *    03  WS-STMT-ACCOUNT-FUND    OCCURS 100  TIMES
170800161012      * RFS156599 START
17090016101212090 *    03  WS-STMT-ACCOUNT-FUND    OCCURS 500  TIMES
171000161108164194*    03  WS-STMT-ACCOUNT-FUND    OCCURS 900  TIMES
171100161108164194     03  WS-STMT-ACCOUNT-FUND    OCCURS 2400 TIMES
171200000000                                       INDEXED BY  SAF.
171300161012      * RFS156599 END
171400000000               05  WS-SAF-ACB-UNIT-PRICE
171500000222                                       PICTURE S9(5)V9(4)  COMP-3.
171600000000               05  WS-SAF-ACB-UNIT-PRICE-SIGN
171700000000                                       PICTURE X(1).
171800000000               05  WS-SAF-CURRENCY     PICTURE X(3).
171900000000               05  WS-SAF-FUND-NO      PICTURE X(8).
172000000000               05  WS-SAF-INVESTMENT-NAME
172100000000                                       PICTURE X(35).
172200000000               05  WS-SAF-LAST-UNIT-PRICE
172300000222                                       PICTURE S9(5)V9(4)  COMP-3.
172400000000               05  WS-SAF-LAST-UNIT-PRICE-SIGN
172500000000                                       PICTURE X(1).
172600000222               05  WS-SAF-MARKET-VALUE PICTURE S9(11)V9(2)  COMP-3.
172700000000               05  WS-SAF-MARKET-VALUE-SIGN
172800000000                                       PICTURE X(1).
172900000000               05  WS-SAF-NET-INVESTED PICTURE S9(9)V9(2)  COMP-3.
173000000000               05  WS-SAF-NET-INVESTED-SIGN
173100000000                                       PICTURE X(1).
173200000000               05  WS-SAF-NO-OF-UNITS  PICTURE S9(10)V9(3) COMP-3.
173300000000               05  WS-SAF-NO-OF-UNITS-SIGN
173400000000                                       PICTURE X(1).
173500000000      /
173600991103
173700991103      *****
173800991103      *****    Account Type Table.
173900991103      *****    - This table contains only two fields: account type
174000991103      *****      description and accout type rule.
174100991103      *****
174200991103
174300991103       01  WS-ACCTYP-TBL.
17440001121212090      04  WS-ACCTYP-ENT         OCCURS 250  TIMES
174500991103                                     INDEXED BY  AT1.
174600991103               05  WS-AT-CODE          PICTURE X(5).
174700991103               05  WS-AT-RULE          PICTURE X(1).
174800991103
174900991103       01  WS-AT-LST               PICTURE S9(3)   COMP-3.
175000991103       01  WS-AT-MAX               PICTURE S9(3)   COMP-3
17510001121212090                                                     VALUE 250.
175200020917
175300000000      ******************************************************************
175400000000      *                                                                *
175500000000      *    H E A D I N G   P R I N T   L I N E S                       *
175600000000      *                                                                *
175700000000      ******************************************************************
175800000000
175900000000       01  HEADING-LINE-1.
176000000000           05  FILLER                  PICTURE X(11)       VALUE
176100000000               "DATE     : ".
176200000104           03  HL1-YY                  PICTURE 9(4).
176300000000           03  FILLER                  PICTURE X(1)        VALUE "/".
176400000000           03  HL1-MM                  PICTURE 9(2).
176500000000           03  FILLER                  PICTURE X(1)        VALUE "/".
176600000000           03  HL1-DD                  PICTURE 9(2).
176700000104           03  FILLER                  PICTURE X(93)       VALUE SPACES.
176800000000           03  FILLER                  PICTURE X(6)        VALUE
176900000000               "TIME: ".
177000000000           03  HL1-TIME                PICTURE 99B99B99B99.
177100000000           03  HL1-TIME-R              REDEFINES HL1-TIME.
177200000000               05  FILLER              PICTURE X(8).
177300000000               05  HL1-TIME-DOT        PICTURE X(1).
177400000000               05  FILLER              PICTURE X(2).
177500000000           03  FILLER                  PICTURE X(19)       VALUE SPACES.
177600000000
177700000000       01  HEADING-LINE-2.
177800000000           03  FILLER                  PICTURE X(11)       VALUE
177900000000               "REPORT NO: ".
178000000000           03  FILLER                  PICTURE X(10)       VALUE
178100000000               "STMTPRT   ".
178200000000           03  FILLER                  PICTURE X(15)       VALUE SPACES.
178300000000           03  FILLER                  PICTURE X(30)       VALUE
178400000000               "STATEMENT PRINT CONTROL REPORT".
178500000000           03  FILLER                  PICTURE X(48)       VALUE SPACES.
178600000000           03  FILLER                  PICTURE X(5)        VALUE
178700000000               "PAGE:".
178800000000           03  FILLER                  PICTURE X(8)        VALUE SPACES.
178900000000           03  HL2-PGECNT              PICTURE ZZZZ9.
179000000000           03  FILLER                  PICTURE X(18)       VALUE SPACES.
179100000000
179200000000       01  HEADING-LINE-3.
179300000000           03  FILLER                  PICTURE X(3)        VALUE SPACES.
179400000000           03  FILLER                  PICTURE X(9)        VALUE
179500000000               "INVESTOR#".
179600000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
179700000000           03  FILLER                  PICTURE X(9)        VALUE
179800000000               "ACCOUNT  ".
179900000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
180000000000           03  FILLER                  PICTURE X(9)        VALUE
180100000000               "SECTION  ".
180200000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
180300000000           03  FILLER                  PICTURE X(3)        VALUE "LNG".
180400000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
180500000000           03  FILLER                  PICTURE X(6)        VALUE
180600000000               "DEALER".
180700000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
180800000000           03  FILLER                  PICTURE X(5)        VALUE
180900000000               "REP. ".
181000000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
181100000000           03  FILLER                  PICTURE X(50)       VALUE
181200000000               "MESSAGE                                           ".
181300000000           03  FILLER                  PICTURE X(50)       VALUE SPACES.
181400000000      /
181500000000      ******************************************************************
181600000000      *                                                                *
181700000000      *    E R R O R   P R I N T   L I N E S
181800000000      *                                                                *
181900000000      ******************************************************************
182000000000
182100000000       01  ERROR-LINE-1.
182200000000           03  FILLER                  PICTURE X(3).
182300000000           03  EL1-INVESTOR-NO         PICTURE 9(9).
182400000000           03  FILLER                  PICTURE X(1).
182500000000           03  EL1-ACCOUNT             PICTURE 9(9).
182600000000           03  FILLER                  PICTURE X(1).
182700000000           03  EL1-SECTION-NO          PICTURE 9(2).
182800000000           03  EL1-SECTION-LITDSH      PICTURE X(1).
182900000000           03  EL1-SECTION-SEQUENCE    PICTURE 9(6).
183000000000           03  FILLER                  PICTURE X(2).
183100000000           03  EL1-LANGUAGE-CODE       PICTURE X(1).
183200000000           03  FILLER                  PICTURE X(2).
183300000000           03  EL1-DEALER              PICTURE X(4).
183400000000           03  FILLER                  PICTURE X(3).
183500000222           03  EL1-DEALER-REP-CODE     PICTURE X(6).
183600000000           03  FILLER                  PICTURE X(1).
183700000000           03  EL1-MSG                 PICTURE X(50).
183800000222           03  FILLER                  PICTURE X(49).
183900000000      /
184000000000      ******************************************************************
184100000000      *                                                                *
184200000000      *    C O N T R O L   P R I N T   L I N E S                       *
184300000000      *                                                                *
184400000000      ******************************************************************
184500000000
184600000000       01  CONTROL-LINE-1.
184700000000           03  FILLER                  PICTURE X(3).
184800000000           03  CL1-LIT                 PICTURE X(50) JUST RIGHT.
184900000000           03  FILLER                  PICTURE X(1).
185000000000           03  CL1-CNT                 PICTURE ZZZ,ZZZ,ZZ9.
185100000000           03  FILLER                  PICTURE X(85).
185200000000
185300000000       01  CONTROL-LINE-2.
185400000000           03  FILLER                  PICTURE X(3)        VALUE SPACES.
185500000000           03  FILLER                  PICTURE X(40)       VALUE
185600000000               "STATEMENTS FOR MANAGEMENT COMPANY CODE: ".
185700000000           03  CL2-MANAGEMENT-COMPANY-CODE
185800000000                                       PICTURE X(3).
185900000000           03  FILLER                  PICTURE X(104)      VALUE SPACES.
186000000000      /
186100000000       01  WS-PGM-DEFIND-WORK-ITEMS.
186200000000
186300000000      ******************************************************************
186400000000      *                                                                *
186500000000      *    P R O G R A M   D E F I N E D   W O R K   I T E M S         *
186600000000      *                                                                *
186700000000      ******************************************************************
186800000000
186900000000      *****
187000000000      *****    Control Report Counters.
187100000000      *****
187200000000
187300000000           03  WS-STMT-DTA-BCNT        PICTURE S9(7)       COMP-3.
187400000000           03  WS-STMT-DTA-BCNT-00     PICTURE S9(7)       COMP-3.
187500000000           03  WS-STMT-DTA-BCNT-05     PICTURE S9(7)       COMP-3.
187600000000           03  WS-STMT-DTA-BCNT-10     PICTURE S9(7)       COMP-3.
187700010308RF8633     03  WS-STMT-DTA-BCNT-12     PICTURE S9(7)       COMP-3.
187800000000           03  WS-STMT-DTA-BCNT-15     PICTURE S9(7)       COMP-3.
187900140624132121     03  WS-STMT-DTA-BCNT-16     PICTURE S9(7)       COMP-3.
188000000000           03  WS-STMT-DTA-BCNT-20     PICTURE S9(7)       COMP-3.
188100060616R35042     03  WS-STMT-DTA-BCNT-21     PICTURE S9(7)       COMP-3.
188200130408117038     03  WS-STMT-DTA-BCNT-22     PICTURE S9(7)       COMP-3.
188300010321RF8633     03  WS-STMT-DTA-BCNT-25     PICTURE S9(7)       COMP-3.
188400991126           03  WS-STMT-DTA-BCNT-30     PICTURE S9(7)       COMP-3.
188500040406R20870     03  WS-STMT-DTA-BCNT-33     PICTURE S9(7)       COMP-3.
188600991126           03  WS-STMT-DTA-BCNT-35     PICTURE S9(7)       COMP-3.
188700040406R20870     03  WS-STMT-DTA-BCNT-38     PICTURE S9(7)       COMP-3.
188800010308RF4638     03  WS-STMT-DTA-BCNT-40     PICTURE S9(7)       COMP-3.
188900010308RF4638     03  WS-STMT-DTA-BCNT-43     PICTURE S9(7)       COMP-3.
189000010308RF4638     03  WS-STMT-DTA-BCNT-45     PICTURE S9(7)       COMP-3.
189100000000           03  WS-STMT-DTA-BCNT-50     PICTURE S9(7)       COMP-3.
189200000000           03  WS-STMT-DTA-BCNT-60     PICTURE S9(7)       COMP-3.
189300991013           03  WS-STMT-DTA-BCNT-62     PICTURE S9(7)       COMP-3.
189400010323rf8633     03  WS-STMT-DTA-BCNT-67     PICTURE S9(7)       COMP-3.
189500000000           03  WS-STMT-DTA-BCNT-70     PICTURE S9(7)       COMP-3.
189600000000           03  WS-STMT-DTA-BCNT-82     PICTURE S9(7)       COMP-3.
189700991013           03  WS-STMT-DTA-BCNT-84     PICTURE S9(7)       COMP-3.
189800010308RF4638     03  WS-STMT-DTA-BCNT-90     PICTURE S9(7)       COMP-3.
189900000000           03  WS-STMT-DTA-RCNT        PICTURE S9(7)       COMP-3.
190000000000           03  WS-STMT-DTA-SCNT-01     PICTURE S9(7)       COMP-3.
190100000000           03  WS-STMT-DTA-SCNT-05     PICTURE S9(7)       COMP-3.
190200000000           03  WS-STMT-DTA-SCNT-10     PICTURE S9(7)       COMP-3.
190300000000           03  WS-STMT-DTA-SCNT-20     PICTURE S9(7)       COMP-3.
190400981207           03  WS-STMT-DTA-SCNT-60     PICTURE S9(7)       COMP-3.
190500000000           03  WS-STMT-DTA-SCNT-80     PICTURE S9(7)       COMP-3.
190600000000           03  WS-STMT-PRT-DTL-WCNT    PICTURE S9(7)       COMP-3.
190700000000           03  WS-STMT-PRT-HDR-WCNT    PICTURE S9(7)       COMP-3.
190800000000
190900000000      *****
191000000000      *****    Control Report Literals.
191100000000      *****
191200000000
191300000000           03  WS-STMT-DTA-BLIT        PICTURE X(50)       VALUE
191400000000               "STATEMENT DATA RECS BYPASSED:                     ".
191500000000           03  WS-STMT-DTA-BLIT-00     PICTURE X(50)       VALUE
191600000000               "STATEMENT DATA (DELIVERY INFO) RECS BYPASSED:     ".
191700000000           03  WS-STMT-DTA-BLIT-05     PICTURE X(50)       VALUE
191800000000               "STATEMENT DATA (DEALER) RECS BYPASSED:            ".
191900000000           03  WS-STMT-DTA-BLIT-10     PICTURE X(50)       VALUE
192000000000               "STATEMENT DATA (INVESTOR SUMMARY) RECS BYPASSED:  ".
192100010308RF8633     03  WS-STMT-DTA-BLIT-12     PICTURE X(50)       VALUE
192200010308               "STATEMENT DATA (INVES SUMMARY SEC2) RECS BYPASSED:".
192300000000           03  WS-STMT-DTA-BLIT-15     PICTURE X(50)       VALUE
192400000000               "STATEMENT DATA (ACCT ASSET CLASS) RECS BYPASSED:  ".
192500000000           03  WS-STMT-DTA-BLIT-20     PICTURE X(50)       VALUE
192600010321               "STATEMENT DATA (ACCOUNT DETAIL) RECS BYPASSED:    ".
192700060616R35042     03  WS-STMT-DTA-BLIT-21     PICTURE X(50)       VALUE
192800060623R35042         "STATEMENT DATA (ACCOUNT ADDITIONAL INFO) BYPASSED:".
192900130408117038     03  WS-STMT-DTA-BLIT-22     PICTURE X(50)       VALUE
193000130408117038         "STATEMENT DATA (ACCOUNT ADDITIONAL DTLS) BYPASSED:".
193100010321RF8633     03  WS-STMT-DTA-BLIT-25     PICTURE X(50)       VALUE
193200010321RF8633         "STATEMENT DATA (ACCOUNT GIC DETAIL) RECS BYPASSED:".
193300991126           03  WS-STMT-DTA-BLIT-30     PICTURE X(50)       VALUE
193400991126               "STATEMENT DATA (ACC PERF) RECS BYPASSED:          ".
193500040406R20870     03  WS-STMT-DTA-BLIT-33     PICTURE X(50)       VALUE
193600040406               "STATEMENT DATA (ACC ROR)  RECS BYPASSED:          ".
193700991126           03  WS-STMT-DTA-BLIT-35     PICTURE X(50)       VALUE
193800991126               "STATEMENT DATA (ACC FUND PERF) RECS BYPASSED:     ".
193900040406R20870     03  WS-STMT-DTA-BLIT-38     PICTURE X(50)       VALUE
194000040406               "STATEMENT DATA (ACC FUND ROR)  RECS BYPASSED:     ".
194100010308RF4638     03  WS-STMT-DTA-BLIT-40     PICTURE X(50)       VALUE
194200010308               "STATEMENT DATA (SEG FUND CONTR DTL) RECS BYPASSED:".
194300010308           03  WS-STMT-DTA-BLIT-43     PICTURE X(50)       VALUE
194400010308               "STATEMENT DATA (SEG FUND CONTR INV) RECS BYPASSED:".
194500010308           03  WS-STMT-DTA-BLIT-45     PICTURE X(50)       VALUE
194600010308RF4638         "STATEMENT DATA (ANNUITANT DETAILS) RECS BYPASSED: ".
194700000000           03  WS-STMT-DTA-BLIT-50     PICTURE X(50)       VALUE
194800000000               "STATEMENT DATA (ROLLOVER) RECS BYPASSED:          ".
194900000000           03  WS-STMT-DTA-BLIT-60     PICTURE X(50)       VALUE
195000000000               "STATEMENT DATA (RRIF DETAIL) RECS BYPASSED:       ".
195100991013           03  WS-STMT-DTA-BLIT-62     PICTURE X(50)       VALUE
195200991013               "STATEMENT DATA (RESP DETAIL) RECS BYPASSED:       ".
195300010323rf8633     03  WS-STMT-DTA-BLIT-67     PICTURE X(50)       VALUE
195400010323               "STATEMENT DATA (BENIFICIARY INFO) RECS BYPASSED:  ".
195500000000           03  WS-STMT-DTA-BLIT-70     PICTURE X(50)       VALUE
195600000000               "STATEMENT DATA (SYSTEM PLAN INFO) RECS BYPASSED:  ".
195700000000           03  WS-STMT-DTA-BLIT-82     PICTURE X(50)       VALUE
195800000000               "STATEMENT DATA (ACCT TRAN INST DTL) RECS BYPASSED:".
195900991013           03  WS-STMT-DTA-BLIT-84     PICTURE X(50)       VALUE
196000991013               "STATEMENT DATA (RESP TRAN BENE DTL) RECS BYPASSED:".
196100010308RF4638     03  WS-STMT-DTA-BLIT-90     PICTURE X(50)       VALUE
196200010308               "STATEMENT DATA (RESET CONTR DTL) RECS BYPASSED:   ".
196300000000           03  WS-STMT-DTA-RLIT        PICTURE X(50)       VALUE
196400000000               "STATEMENT DATA RECS READ:                         ".
196500000000           03  WS-STMT-DTA-SLIT-01     PICTURE X(50)       VALUE
196600000000               "STATEMENT DATA (INVESTOR INFO) RECS SELECTED:     ".
196700000000           03  WS-STMT-DTA-SLIT-05     PICTURE X(50)       VALUE
196800000000               "STATEMENT DATA (DEALER) RECS SELECTED:            ".
196900000000           03  WS-STMT-DTA-SLIT-10     PICTURE X(50)       VALUE
197000000000               "STATEMENT DATA (INVESTOR SUMMARY) RECS SELECTED:  ".
197100000000           03  WS-STMT-DTA-SLIT-20     PICTURE X(50)       VALUE
197200000000               "STATEMENT DATA (ACCOUNT DETAIL) RECS SELECTED:    ".
197300981207           03  WS-STMT-DTA-SLIT-60     PICTURE X(50)       VALUE
197400981207               "STATEMENT DATA (RRIF/LIF DETAILS) RECS SELECTED:  ".
197500000000           03  WS-STMT-DTA-SLIT-80     PICTURE X(50)       VALUE
197600000000               "STATEMENT DATA (ACCT. TRANSACTION) RECS SELECTED: ".
197700000000           03  WS-STMT-PRT-DTL-WLIT    PICTURE X(50)       VALUE
197800000000               "STATEMENT PRINT DETAIL RECS WRITTEN:              ".
197900000000           03  WS-STMT-PRT-HDR-WLIT    PICTURE X(50)       VALUE
198000000000               "STATEMENT PRINT HEADER RECS WRITTEN:              ".
198100000000
198200000000      *****
198300000000      *****    Error Messages.
198400000000      *****
198500000000
198600000000           03  WS-MSGS.
198700000000               05  WS-MSG-01           PICTURE X(50)       VALUE
198800000000                   "MFASTMTFP SECTION-NO INVALID, RECORD BYPASSED     ".
198900000000               05  WS-MSG-02           PICTURE X(50)       VALUE
199000000000                   "ACCOUNT DETAIL SECTION-NO INVALID, RECORD BYPASSED".
199100000000               05  WS-MSG-03           PICTURE X(50)       VALUE
199200000000                   "ACCOUNT DETAIL CONTAINS INVALID CURRENCY          ".
199300000000               05  WS-MSG-04           PICTURE X(50)       VALUE
199400000000                   "PROGRAM CURRENCY MAXIMUM EXCEEDED                 ".
199500000000               05  WS-MSG-05           PICTURE X(50)       VALUE
199600000000                   "WARNING: MISSING ACCT DTL REC FOR ACCT TRANS FUND ".
199700000000               05  WS-MSG-06           PICTURE X(50)       VALUE
199800000000                   "STATEMENT PRINT HEADER RECORD WRITE FAILED, ABORT ".
199900000000               05  WS-MSG-07           PICTURE X(50)       VALUE
200000000000                   "STATEMENT PRINT DETAIL RECORD WRITE FAILED, ABORT ".
200100000000               05  WS-MSG-08           PICTURE X(50)       VALUE
200200000000                   "MISSING INVESTOR INFORMATION RECORD FOR ACCOUNT   ".
200300000000               05  WS-MSG-09           PICTURE X(50)       VALUE
200400000000                   "MISSING DEALER RECORD FOR ACCOUNT                 ".
200500000000               05  WS-MSG-10           PICTURE X(50)       VALUE
200600000000                   "MISSING ACCOUNT DETAIL RECORD FOR ACCOUNT         ".
200700000000               05  WS-MSG-11           PICTURE X(50)       VALUE
200800000000                   "PROGRAM ACCOUNT MAXIMUM EXCEEDED                  ".
200900000000               05  WS-MSG-12           PICTURE X(50)       VALUE
201000000000                   "WARNING: NOMINEE ACCOUNT WITH NO NOMINEE DATA     ".
201100000000               05  WS-MSG-13           PICTURE X(50)       VALUE
201200000000                   "WARNING: NON-NOMINEE ACCOUNT WITH NOMINEE DATA    ".
201300000000           03  WS-MSGS-R               REDEFINES WS-MSGS.
201400000000               05  WS-MSG              OCCURS  13  TIMES
201500000000                                       INDEXED BY  MSG
201600000000                                       PICTURE X(50).
201700000000
201800000000      *****
201900000000      *****    Format Account Type Description Routine Work Items.
202000000000      *****
202100000000
202200000000           03  WS-ACTTYPGRP            PICTURE X(1).
202300000000
202400000000           03  WS-ACTTYPIN.
202500000000               05  WS-ACTTYPIN-PFX     PICTURE X(35).
202600000000               05  WS-ACTTYPIN-SFX     PICTURE X(2).
202700000000
202800000000           03  WS-ACTTYPLCK            PICTURE X(1).
202900000000           03  WS-ACTTYPLIT1           PICTURE X(13).
203000000000           03  WS-ACTTYPLIT2           PICTURE X(13).
203100000000           03  WS-ACTTYPLIT3           PICTURE X(13).
203200000000           03  WS-ACTTYPLNG            PICTURE X(1).
203300000000           03  WS-ACTTYPNOM            PICTURE X(1).
203400000000           03  WS-ACTTYPOUT            PICTURE X(80).
203500000000           03  WS-ACTTYPSHT            PICTURE X(5).
203600000000           03  WS-ACTTYPSPS            PICTURE X(1).
203700000000
203800000000      *****
203900000000      *****    Index/Subscript Related Items.
204000000000      *****
204100000000
204200161108164194*    03  WS-SAF-LST              PICTURE S9(3)       COMP-3.
204300161108164194     03  WS-SAF-LST              PICTURE S9(4)       COMP-3.
204400161108164194*    03  WS-SAF-MAX              PICTURE S9(3)       COMP-3.
204500161108164194     03  WS-SAF-MAX              PICTURE S9(4)       COMP-3.
204600000000           03  WS-SC-MAX               PICTURE S9(3)       COMP-3.
204700000000           03  WS-SIA-LST              PICTURE S9(5)       COMP-3.
204800000000           03  WS-SIA-MAX              PICTURE S9(5)       COMP-3.
204900000000           03  WS-SPL                  PICTURE S9(3)       COMP-3.
205000000000           03  WS-SPL-DTLFST           PICTURE S9(3)       COMP-3.
205100000000           03  WS-SPL-DTLLST           PICTURE S9(3)       COMP-3.
205200000000           03  WS-SPL-DTLMAX           PICTURE S9(3)       COMP-3.
205300000000           03  WS-SPL-DTLTST           PICTURE S9(3)       COMP-3.
205400000000           03  WS-SPL-HDGFST           PICTURE S9(3)       COMP-3.
205500000000           03  WS-SPL-MAX              PICTURE S9(3)       COMP-3.
205600000000           03  WS-SPL-SH02             PICTURE S9(3)       COMP-3.
205700990927           03  WS-SPL-SH03             PICTURE S9(3)       COMP-3.
205800000000           03  WS-SPML-MAX             PICTURE S9(3)       COMP-3.
205900021213R15028     03  WS-NEW-INVESTOR-FLAG    PICTURE X(1) VALUE " ".
206000000000
206100000000      *****
206200000000      *****    Non-Numeric Items.
206300000000      *****
206400000000
206500990104
206600000000           03  WS-ADRTMP               PICTURE X(40).
206700000000           03  WS-BRANCH-ADDR1         PICTURE X(40).
206800000000           03  WS-BRANCH-ADDR2         PICTURE X(40).
206900000000           03  WS-BRANCH-ADDR3         PICTURE X(40).
207000000000           03  WS-BRANCH-ADDR4         PICTURE X(40).
207100000000           03  WS-BRANCH-ADDR5         PICTURE X(40).
207200000000
207300000000           03  WS-DATE.
207400000104               05  WS-DATE-YYYY        PICTURE 9(4).
207500000000               05  WS-DATE-MM          PICTURE 9(2).
207600000000               05  WS-DATE-DD          PICTURE 9(2).
207700990120
207800990120           03  WS-SYS-DATE             PICTURE 9(8).
207900000000
208000000000           03  WS-DEALER-NAME          PICTURE X(35).
208100000000           03  WS-DEALER-REP-NAMES     PICTURE X(51).
208200000000
208300000000           03  WS-END-DATE.
208400991013             04 WS-SH-END-DATE         PICTURE X(8).
208500991013             04 WS-SH-END-DATE-R       REDEFINES WS-SH-END-DATE.
208600991013               05  WS-SH-END-YY        PICTURE X(2).
208700991013               05  WS-SH-END-DATE-2    PICTURE X(6).
208800000000
208900990923           03  WS-END-DATE-6.
209000990923               05  WS-SH-END-DATE-6    PICTURE X(8).
209100990923
209200000000           03  WS-FAX.
209300000000             04  WS-SH-FAX-01-03       PICTURE 9(3).
209400000000             04  WS-SH-FAX-04-10       PICTURE 9(7).
209500000000             04  WS-SH-FAX-04-10-R     REDEFINES WS-SH-FAX-04-10.
209600000000               05  WS-SH-FAX-04-06     PICTURE X(3).
209700000000               05  WS-SH-FAX-07-10     PICTURE X(4).
209800000000
209900000000           03  WS-FILSTAT              PICTURE X(2).
210000000000
210100000000           03  WS-FUND-NO.
210200000000               05  WS-MANAGEMENT-COMPANY-CODE
210300000000                                       PICTURE X(3).
210400000000               05  FILLER              PICTURE X(5).
210500000000
210600000000           03  WS-NOMINEE              PICTURE X(53).
210700000000           03  WS-NOMINEE-CODE         PICTURE X(5).
210800000000           03  WS-NOMINEE-NAME         PICTURE X(37).
210900000000           03  WS-NOMINEES-ACCT-NO     PICTURE X(15).
211000000000
211100000000           03  WS-PHONE.
211200000000             04  WS-SH-PHONE-01-03     PICTURE 9(3).
211300000000             04  WS-SH-PHONE-04-10     PICTURE 9(7).
211400000000             04  WS-SH-PHONE-04-10-R   REDEFINES WS-SH-PHONE-04-10.
211500000000               05  WS-SH-PHONE-04-06   PICTURE X(3).
211600000000               05  WS-SH-PHONE-07-10   PICTURE X(4).
211700000000
211800000000           03  WS-POSTAL.
211900000000               05  WS-POSTAL1          PICTURE X(3).
212000000000               05  WS-POSTAL2          PICTURE X(7).
212100000000
212200000000           03  WS-POSTAL-OUT.
212300000000               05  WS-POSTAL-OUT1      PICTURE X(3).
212400000000               05  FILLER              PICTURE X(1).
212500000000               05  WS-POSTAL-OUT2      PICTURE X(6).
212600000000
212700000000           03  WS-REP-NAME             PICTURE X(40).
212800990113           03  WS-REP-NAME-1           PICTURE X(40).
212900990113
213000000000           03  WS-SH02-SIN-NO.
213100000000               05  SH02-SIN-NO-1-3     PICTURE X(3).
213200000000               05  SH02-SIN-NO-LITDSH1 PICTURE X(1).
213300000000               05  SH02-SIN-NO-4-6     PICTURE X(3).
213400000000               05  SH02-SIN-NO-LITDSH2 PICTURE X(1).
213500000000               05  SH02-SIN-NO-7-9     PICTURE X(3).
213600000000
213700000000           03  WS-SH04-INVESTOR-ADDRESS1.
213800000000               05  SH04-POSTAL-CODE    PICTURE X(10).
213900000000               05  FILLER              PICTURE X(30).
214000000000
214100000000           03  WS-SH05-INVESTOR-ADDRESS2.
214200000000               05  SH05-POSTAL-CODE    PICTURE X(10).
214300000000               05  FILLER              PICTURE X(30).
214400000000
214500000000           03  WS-SH06-INVESTOR-ADDRESS3.
214600000000               05  SH06-POSTAL-CODE    PICTURE X(10).
214700000000               05  FILLER              PICTURE X(30).
214800000000
214900000000           03  WS-SH07-INVESTOR-ADDRESS4.
215000000000               05  SH07-POSTAL-CODE    PICTURE X(10).
215100000000               05  FILLER              PICTURE X(30).
215200000000
215300000000           03  WS-SH08-INVESTOR-ADDRESS5.
215400000000               05  SH08-POSTAL-CODE    PICTURE X(10).
215500000000               05  FILLER              PICTURE X(30).
215600000000
215700000000           03  WS-SH09-POSTAL-CODE     PICTURE X(10).
215800000000           03  WS-SH10-LIT1X           PICTURE X(47).
215900000000
216000000000           03  WS-SH10-PHONE.
216100000000               05  SH10-PHONE-LITLB    PICTURE X(1).
216200000000               05  SH10-PHONE-01-03    PICTURE X(3).
216300000000               05  SH10-PHONE-LITRB    PICTURE X(1).
216400000000               05  FILLER              PICTURE X(1).
216500000000               05  SH10-PHONE-04-06    PICTURE X(3).
216600000000               05  SH10-PHONE-LITDSH   PICTURE X(1).
216700000000               05  SH10-PHONE-07-10    PICTURE X(4).
216800000000
216900000000           03  WS-SH11-NOMINEE         PICTURE X(53).
217000000000
217100000000           03  WS-SH11-FAX.
217200000000               05  SH11-FAX-LITLB      PICTURE X(1).
217300000000               05  SH11-FAX-01-03      PICTURE X(3).
217400000000               05  SH11-FAX-LITRB      PICTURE X(1).
217500000000               05  FILLER              PICTURE X(1).
217600000000               05  SH11-FAX-04-06      PICTURE X(3).
217700000000               05  SH11-FAX-LITDSH     PICTURE X(1).
217800000000               05  SH11-FAX-07-10      PICTURE X(4).
217900000000
218000000000           03  WS-SIN-NO.
218100000000               05  WS-SIN-NO-1-3       PICTURE X(3).
218200000000               05  WS-SIN-NO-4-6       PICTURE X(3).
218300000000               05  WS-SIN-NO-7-9       PICTURE X(3).
218400000000
218500000000           03  WS-START-DATE.
218600980722               05  WS-SH-START-DATE    PICTURE X(8).
218700000000
218800000000           03  WS-TRADE-DATE.
218900000000               05  FILLER              PICTURE X(2).
219000000000               05  WS-SH-TRADE-DATE    PICTURE X(6).
219100000000
219200990923           03  WS-TRADE-DATE-6.
219300990923               05  WS-SH-TRADE-DATE-6    PICTURE X(8).
219400990923
219500000000      *****
219600000000      *****    Non-Numeric Literals.
219700000000      *****
219800000000
219900000000           03  WS-ACCOUNT-TYPE-SHORT-TOTAL
220000000000                                       PICTURE X(5)        VALUE
220100000000               "TOTAL".
220200000000           03  WS-ACCOUNT-TYPE-TOTAL   PICTURE X(35)       VALUE
220300000000               "TOTAL                              ".
220400000000           03  WS-INVESTMENT-NAME-TOTAL
220500000000                                       PICTURE X(35)       VALUE
220600000000               "TOTAL                              ".
220700000000
220800000000      *****
220900000000      *****    Numeric Items.
221000000000      *****
221100971223
221200971223           03  WS-CPG-FIXED            PICTURE S9(9)V9(2)  COMP-3.
221300971223           03  WS-CPG-FIXED-SIGN       PICTURE X(1).
221400000000
221500971223           03  WS-CPTL-GAIN            PICTURE 9(9)V9(2)   COMP-3.
221600000000           03  WS-CPTL-GAIN-SIGN       PICTURE X(1).
221700000000
221800000000           03  WS-DEDUCTIONS           PICTURE S9(9)V9(2)  COMP-3.
221900000000           03  WS-DEDUCTIONS-SIGN      PICTURE X(1).
222000000000
222100000000           03  WS-LNECNT               PICTURE S9(3)       COMP-3.
222200000000           03  WS-LNEMAX               PICTURE S9(3)       COMP-3.
222300000000           03  WS-NET-INVESTED         PICTURE S9(9)V9(2)  COMP-3.
222400000000           03  WS-PGECNT               PICTURE S9(5)       COMP-3.
222500000000
222600000000           03  WS-REG-CTB-CUR          PICTURE 9(9)V9(2)   COMP-3.
222700000000           03  WS-REG-CTB-CUR-SIGN     PICTURE X(1).
222800000000
222900000000           03  WS-SH-PGENO             PICTURE S9(3)       COMP-3.
223000091112R73890*    03  WS-STMTVRSN             PICTURE S9(3)       COMP-3.
223100091112R73890     03  WS-STMTVRSN             PICTURE X(1).
223200000000
223300000000      *****
223400000000      *****    Switches.
223500000000      *****
223600000000
223700000000           03  WS-BYPPRT-SW            PICTURE X(1).
223800000000           03  WS-CLTSPR-SW            PICTURE X(1).
223900000000           03  WS-EOJHDG-SW            PICTURE X(1).
224000000000           03  WS-L01BYP-SW            PICTURE X(1).
224100000000           03  WS-PSRERR-SW            PICTURE X(1).
224200000000           03  WS-PSTCDEPRT-SW         PICTURE X(1).
224300000000           03  WS-SELPRT-SW            PICTURE X(1).
224400000000           03  WS-STMT-DTA-SW-01       PICTURE X(1).
224500000000           03  WS-STMT-DTA-SW-05       PICTURE X(1).
224600981207           03  WS-STMT-DTA-SW-60       PICTURE X(1).
224700000000           03  WS-STMT-PRT-HDR-WSW     PICTURE X(1).
224800000000      /
224900000000       01  DUMP-ARGUMENTS.
225000000000
225100000000      ******************************************************************
225200000000      *                                                                *
225300000000      *    D U M P   P A S S   P A R A M E T E R S                     *
225400000000      *                                                                *
225500000000      ******************************************************************
225600000000
225700000000           03  DA-TYPE                 PICTURE X(1)        VALUE "F".
225800000000               88  COBOL-IDENTIFIERS                       VALUE "D".
225900000000               88  FULL-DUMP                               VALUE "F".
226000000000           03  FILLER                  PICTURE X(1).
226100000000           03  DA-PGM-NAME             PICTURE X(10)       VALUE
226200000000               "STMTPRT   ".
226300000000      /
226400000000      ******************************************************************
226500000000      *                                                                *
226600000000      *    C U R R E N T   /   P R E V I O U S   C O N T R O L S       *
226700000000      *                                                                *
226800000000      ******************************************************************
226900000000
227000000000       01  CURR-CONTROLS.
227100000000           03  CURR-LEVEL1.
227200000000             04  CURR-LEVEL2.
227300980128               05  CURR-START-DATE     PICTURE 9(8).
227400980128               05  CURR-END-DATE       PICTURE 9(8).
227500000000               05  CURR-INVESTOR-NO    PICTURE 9(9).
227600000000             04  CURR-ACCOUNT          PICTURE 9(9).
227700000000
227800000000       01  PREV-CONTROLS.
227900000000           03  PREV-LEVEL1.
228000000000             04  PREV-LEVEL2.
228100980128               05  PREV-START-DATE     PICTURE 9(8).
228200980128               05  PREV-END-DATE       PICTURE 9(8).
228300000000               05  PREV-INVESTOR-NO    PICTURE 9(9).
228400000000             04  PREV-ACCOUNT          PICTURE 9(9).
228500000000
228600000000       01  CURR-CTLS-DPR.
228700000000           03  CURR-CD-LEV1.
228800000000               05  CURR-CD-FUND-NO     PICTURE X(8).
228900000000
229000000000       01  PREV-CTLS-DPR.
229100000000           03  PREV-CD-LEV1.
229200000000               05  PREV-CD-FUND-NO     PICTURE X(8).
229300020921
22940002092114588  01    WS-MSTR-ACCOUNT-NO      PICTURE 9(9).
229500190612      *RFS183449-Starts
229600190612       01    Lc-Acct-Type-Code       PICTURE X(5).
229700190612      *RFS183449-Ends
229800000000      /
229900000000       PROCEDURE DIVISION.
230000000000
230100000000      ******************************************************************
230200000000      *                                                                *
230300000000      *    P R O C E D U R E   D I V I S I O N                         *
230400000000      *                                                                *
230500000000      ******************************************************************
230600000000      /
230700000000       MAINLINE.
230800000000
230900000000      ******************************************************************
231000000000      *                                                                *
231100000000      *    M A I N L I N E                                             *
231200000000      *                                                                *
231300000000      ******************************************************************
231400000000
231500000000           PERFORM INITIAL-LOGIC-RTN  THRU ILR-EXIT.
231600000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
231700000000               GO TO MLR-0050.
231800000000
231900000000       MLR-0010.
232000000000           PERFORM GET-CURRENT-RECORD THRU GCR-EXIT.
232100000000           IF  PREV-LEVEL1    IS EQUAL TO  CURR-LEVEL1
232200000000               GO TO MLR-0040.
232300000000
232400000000           IF  PREV-CONTROLS  IS EQUAL TO  LOW-VALUES
232500000000               GO TO MLR-0020.
232600000000
232700000000           PERFORM LEVEL-SUMMARY-1    THRU LS1-EXIT.
232800000000
232900000000           IF  PREV-LEVEL2    IS EQUAL TO  CURR-LEVEL2
233000000000               GO TO MLR-0030.
233100000000
233200000000           PERFORM LEVEL-SUMMARY-2    THRU LS2-EXIT.
233300000000
233400000000       MLR-0020.
233500000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
233600000000               GO TO MLR-0050.
233700000000
233800000000           PERFORM LEVEL-REINIT-2     THRU LR2-EXIT.
233900000000
234000000000       MLR-0030.
234100000000           PERFORM LEVEL-REINIT-1     THRU LR1-EXIT.
234200000000
234300000000       MLR-0040.
234400000000           PERFORM DETAIL-PROCESS-RTN THRU DPR-EXIT.
234500000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
234600000000               GO TO MLR-0050.
234700000000
234800000000           MOVE CURR-CONTROLS          TO  PREV-CONTROLS.
234900000000           GO TO MLR-0010.
235000000000
235100000000       MLR-0050.
235200000000           PERFORM END-JOB-RTN        THRU EJR-EXIT.
235300000000           STOP RUN.
235400000000      /
235500000000       GET-CURRENT-RECORD.
235600000000
235700000000      ******************************************************************
235800000000      *                                                                *
235900000000      *    G E T   C U R R E N T   R E C O R D   R O U T I N E         *
236000000000      *                                                                *
236100000000      ******************************************************************
236200000000
236300000000           CONTINUE.
236400000000
236500000000      *****
236600000000      *****    Read through the Statement Data records.
236700000000      *****
236800000000
236900000000       GCR-0010.
237000990921
237100000000           READ STMT-DTA-FILE          AT  END
237200000000               MOVE HIGH-VALUES        TO  CURR-CONTROLS
237300990921               GO TO GCR-EXIT
237400990921           END-READ
237500990921
237600000000           MOVE STMT-DTA-RECD          TO  MFASTMTFP-SECTION.
237700000000           ADD 1                       TO  WS-STMT-DTA-RCNT.
237800000000
237900000000      *****
238000000000      *****    Determine which Statement Data record is being processed.
238100000000      *****    If the record cannot be identified, an error message will
238200000000      *****    be printed and the record bypassed.  If this occurs, the
238300000000      *****    program should be changed to either bypass the record or
238400000000      *****    to handle it appropriately.
238500000000      *****
238600000000
238700990921           EVALUATE PR-SECTION-NO
238800990921             WHEN  "00"               GO TO GCR-0020
238900990921             WHEN  "01"               GO TO GCR-0030
239000990921             WHEN  "05"               GO TO GCR-0040
239100990921             WHEN  "10"               GO TO GCR-0050
239200010308RF8633       WHEN  "12"               GO TO GCR-0050-0005
239300990921             WHEN  "15"               GO TO GCR-0050-0010
239400140624132121       WHEN  "16"               GO TO GCR-0050-0016
239500991126             WHEN  "20"               GO TO GCR-0060
239600060616R35042       WHEN  "21"               GO TO GCR-0060-0002
239700130408117038       WHEN  "22"               GO TO GCR-0060-0003
239800010321RF8633       WHEN  "25"               GO TO GCR-0060-0005
239900991126             WHEN  "30"               GO TO GCR-0060-0010
240000040406R20870       WHEN  "33"               GO TO GCR-0060-0033
240100991126             WHEN  "35"               GO TO GCR-0060-0020
240200040406R20870       WHEN  "38"               GO TO GCR-0060-0038
240300010308RF4638       WHEN  "40"               GO TO GCR-0060-0030
240400010308RF4638       WHEN  "43"               GO TO GCR-0060-0040
240500010308RF4638       WHEN  "45"               GO TO GCR-0060-0050
240600990921             WHEN  "50"               GO TO GCR-0070
240700990921             WHEN  "60"               GO TO GCR-0070-0010
240800991013             WHEN  "62"               GO TO GCR-0070-0015
240900010323RF8633       WHEN  "67"               GO TO GCR-0070-0018
241000990921             WHEN  "70"               GO TO GCR-0070-0020
241100990921             WHEN  "80"               GO TO GCR-0080
241200990921             WHEN  "82"               GO TO GCR-0080-0010
241300991013             WHEN  "84"               GO TO GCR-0080-0020
241400010308RF4638       WHEN  "90"               GO TO GCR-0080-0030
241500990921             WHEN  OTHER
241600990921                   MOVE SPACES                 TO  ERROR-LINE-1
241700990921                   SET MSG                     TO  1
241800990921                   PERFORM PRINT-ERR-LINE      THRU PEL-EXIT
241900000000
242000990921                   ADD 1                       TO  WS-STMT-DTA-BCNT
242100990921                   GO TO GCR-0090
242200990921           END-EVALUATE.
242300990921
242400000000       GCR-0020.
242500000000
242600000000      *****
242700000000      *****    Processing a Delivery Information record, bypass it.
242800000000      *****
242900000000
243000000000           ADD 1                       TO  WS-STMT-DTA-BCNT-00.
243100000000           GO TO GCR-0090.
243200000000
243300000000       GCR-0030.
243400000000
243500000000      *****
243600000000      *****    Processing an Investor Information record, select it.
243700000000      *****
243800000000
243900000000           MOVE MFASTMTFP-SECTION      TO  INVESTOR-INFO-SECTION.
244000000000           ADD 1                       TO  WS-STMT-DTA-SCNT-01.
244100000000           GO TO GCR-0100.
244200000000
244300000000       GCR-0040.
244400000000
244500000000      *****
244600000000      *****    Processing a Dealer record.  The record will be selected
244700000000      *****    unless it is the primary record.
244800000000      *****
244900000000
245000000000           MOVE MFASTMTFP-SECTION      TO  DEALER-SECTION.
245100990921
245200990921           IF  DL-SECTION-SEQ IS GREATER THAN  1
245300000000               ADD 1                   TO  WS-STMT-DTA-BCNT-05
245400990921               GO TO GCR-0090
245500990921           END-IF
245600990921
245700000000           ADD 1                       TO  WS-STMT-DTA-SCNT-05.
245800000000           GO TO GCR-0100.
245900000000
246000000000       GCR-0050.
246100000000
246200000000      *****
246300000000      *****    Processing an Investor Summary record.  The record will
246400000000      *****    be selected unless it is a 'TOTAL' record.
246500000000      *****
246600000000
246700000000           MOVE MFASTMTFP-SECTION      TO  INVESTOR-SUMMARY-SECTION.
246800000000           IF  IS-ACCOUNT-TYPE   EQUAL TO  WS-ACCOUNT-TYPE-TOTAL
246900000000           AND IS-ACCOUNT-TYPE-SHORT
247000000000                              IS EQUAL TO  WS-ACCOUNT-TYPE-SHORT-TOTAL
247100000000               ADD 1                   TO  WS-STMT-DTA-BCNT-10
247200000000               GO TO GCR-0090.
247300020614
247400000000           ADD 1                       TO  WS-STMT-DTA-SCNT-10.
247500000000           GO TO GCR-0100.
247600000000
247700010308RF8633 GCR-0050-0005.
247800010308
247900010308      *****
248000010308      *****    Processing an Investor Summary Section-2 record,
248100010308      *****    bypass it.
248200010308      *****
248300010308
248400010308           MOVE MFASTMTFP-SECTION TO INVESTOR-SUMMARY-SECTION-2.
248500010308           ADD 1                       TO  WS-STMT-DTA-BCNT-12.
248600010308RF8633     GO TO GCR-0090.
248700010308
248800000000       GCR-0050-0010.
248900000000
249000000000      *****
249100000000      *****    Processing an Account Asset Class Summary record, bypass
249200000000      *****    it.
249300000000      *****
249400000000
249500000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ASSET-CLASS-SECTION.
249600000000           ADD 1                       TO  WS-STMT-DTA-BCNT-15.
249700000000           GO TO GCR-0090.
249800000000
249900140624      *RFS132121 - START
250000140708       GCR-0050-0016.
250100140624
250200140624      *****
250300140624      *****    Processing an Account Level Asset Allocation record,
250400140624      *****    bypass it.
250500140624      *****
250600140624
250700140708           MOVE MFASTMTFP-SECTION  TO  ACCOUNT-LEVEL-ASSET-ALLOC-DTL.
250800140708           ADD 1                   TO  WS-STMT-DTA-BCNT-16.
250900140624           GO TO GCR-0090.
251000140624
251100140624      *RFS132121 - END
251200140708
251300000000       GCR-0060.
251400000000
251500000000      *****
251600000000      *****    Processing an Account Detail record.  The record will be
251700000000      *****    selected unless it is a 'TOTAL' record.
251800000000      *****
251900000000
252000000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION.
252100000000           IF  AD-INVESTMENT-NAME
252200000000                              IS EQUAL TO  WS-INVESTMENT-NAME-TOTAL
252300000000               ADD 1                   TO  WS-STMT-DTA-BCNT-20
252400000000               GO TO GCR-0090.
252500000000
252600000000           ADD 1                       TO  WS-STMT-DTA-SCNT-20.
252700000000           GO TO GCR-0100.
252800000000
252900060616R35042 GCR-0060-0002.
253000060616  ¦
253100060616  ¦   *****
253200060616  ¦   *****    Processing a ACCOUNT-ADDITIONAL-INFO record, BYPASS IT.
253300060616  ¦   *****
253400060616  ¦
253500060616  ¦        MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ADDITIONAL-INFO.
253600060616  V        ADD 1                       TO  WS-STMT-DTA-BCNT-21.
253700060616R35042     GO TO GCR-0090.
253800060616
253900130408117038 GCR-0060-0003.
254000130408  ¦
254100130408  ¦        MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION-22.
254200130408  V        ADD 1                       TO  WS-STMT-DTA-BCNT-22.
254300130408117038     GO TO GCR-0090.
254400130408
254500010321RF8633 GCR-0060-0005.
254600010321
254700010321      *****
254800010321      *****    Processing an Account GIC Detail record. bypass it.
254900010321      *****
255000010321
255100010321           MOVE MFASTMTFP-SECTION TO ACCOUNT-GIC-DETAILS-SECTION.
255200010321           ADD 1                       TO  WS-STMT-DTA-BCNT-25.
255300010321RF8633     GO TO GCR-0090.
255400010321
255500991126       GCR-0060-0010.
255600991126
255700991126      *****
255800991126      *****    Processing a Account Performance record, bypass it.
255900991126      *****
256000991126
256100991126           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-PERFORMANCE-SECTION.
256200991126           ADD 1                       TO  WS-STMT-DTA-BCNT-30.
256300991126           GO TO GCR-0090.
256400991126
256500040406R20870 GCR-0060-0033.
256600040406
256700040406      *****
256800040406      *****    Processing a Account ROR, bypass it.
256900040406      *****
257000040406
257100040406           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ROR-FRQ-SECTION.
257200040406           ADD 1                       TO  WS-STMT-DTA-BCNT-33.
257300040406           GO TO GCR-0090.
257400040406
257500991126       GCR-0060-0020.
257600991126
257700991126      *****
257800991126      *****    Processing a Account Fund Performance record, bypass it.
257900991126      *****
258000991126
258100991126           MOVE MFASTMTFP-SECTION      TO ACCOUNT-FUND-PERFORM-SECTION.
258200991126           ADD 1                       TO WS-STMT-DTA-BCNT-35.
258300991126           GO TO GCR-0090.
258400991126
258500040406R20870 GCR-0060-0038.
258600040406
258700040406      *****
258800040406      *****    Processing a Account Fund ROR, bypass it.
258900040406      *****
259000040406
259100040406           MOVE MFASTMTFP-SECTION      TO ACCOUNT-FUND-ROR-FRQ-SECTION.
259200040406           ADD 1                       TO WS-STMT-DTA-BCNT-38.
259300040406           GO TO GCR-0090.
259400040406
259500010308RF4638 GCR-0060-0030.
259600010308
259700010308      *****
259800010308      *****    Processing a Seg Fund Contract Details record,
259900010308      *****    bypass it.
260000010308      *****
260100010308           MOVE MFASTMTFP-SECTION      TO SEG-FUND-CONTRACT-DETAILS.
260200010308           ADD 1                       TO WS-STMT-DTA-BCNT-40.
260300010308           GO TO GCR-0090.
260400010308
260500010308       GCR-0060-0040.
260600010308
260700010308      *****
260800010308      *****    Processing a Seg Fund Contract Investment Details
260900010308      *****    record, bypass it.
261000010308      *****
261100010308
261200010308           MOVE MFASTMTFP-SECTION TO SEG-FUND-CONTRACT-INVESTMENT.
261300010308           ADD 1                  TO WS-STMT-DTA-BCNT-43.
261400010308           GO TO GCR-0090.
261500010308
261600010308       GCR-0060-0050.
261700010308
261800010308      *****
261900010308      *****    Processing a Annuitant Details record, bypass it.
262000010308      *****
262100010308
262200010308           MOVE MFASTMTFP-SECTION      TO ANNUITANT-DETAILS.
262300010308           ADD 1                       TO WS-STMT-DTA-BCNT-45.
262400010308RF4638     GO TO GCR-0090.
262500010308
262600000000       GCR-0070.
262700000000
262800000000      *****
262900000000      *****    Processing a Rollover record, bypass it.
263000000000      *****
263100000000
263200000000           MOVE MFASTMTFP-SECTION      TO  ROLLOVER-SECTION.
263300000000           ADD 1                       TO  WS-STMT-DTA-BCNT-50.
263400000000           GO TO GCR-0090.
263500000000
263600000000       GCR-0070-0010.
263700000000
263800000000      *****
263900981207      *****    Processing a RRIF Detail record, bypass it if
264000981207      *****    RIF and LIF info is not required to print.
264100981207      *****
264200981215
264300000000           MOVE MFASTMTFP-SECTION      TO  RRIF-DETAIL-SECTION.
264400981207
264500981207           IF WS-LDA-RIF-LIF-MSG IS NOT EQUAL TO "Y"
264600981207              ADD 1                    TO  WS-STMT-DTA-BCNT-60
264700981207              GO TO GCR-0090.
264800981207
264900981207           ADD 1    TO WS-STMT-DTA-SCNT-60.
265000981207           GO TO GCR-0100.
265100981207
265200991013       GCR-0070-0015.
265300991013
265400991013      *****
265500991013      *****    Processing a RESP Detail record, bypass it.
265600991013      *****
265700991013
265800991013           MOVE MFASTMTFP-SECTION      TO  RESP-DETAIL-SECTION.
265900991013           ADD 1                       TO  WS-STMT-DTA-BCNT-62.
266000991013           GO TO GCR-0090.
266100010323
266200010323RF8633 GCR-0070-0018.
266300010323
266400010323      *****
266500010323      *****    Processing a BENIFICIARY INFORMATION record, bypass it.
266600010323      *****
266700010323
266800010323           MOVE MFASTMTFP-SECTION      TO  RESP-DETAIL-SECTION.
266900010323           ADD 1                       TO  WS-STMT-DTA-BCNT-67.
267000010323           GO TO GCR-0090.
267100991013
267200000000       GCR-0070-0020.
267300000000
267400000000      *****
267500000000      *****    Processing a Systematic Plan Info record, bypass it.
267600000000      *****
267700000000
267800000000           MOVE MFASTMTFP-SECTION      TO  SYSTEMATIC-PLAN-INFO-SECTION.
267900000000           ADD 1                       TO  WS-STMT-DTA-BCNT-70.
268000000000           GO TO GCR-0090.
268100000000
268200000000       GCR-0080.
268300000000
268400000000      *****
268500000000      *****    Processing an Account Transaction record, select it.
268600000000      *****
268700000000
268800000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-TXN-SECTION.
268900000000           ADD 1                       TO  WS-STMT-DTA-SCNT-80.
269000000000           GO TO GCR-0100.
269100000000
269200000000       GCR-0080-0010.
269300000000
269400000000      *****
269500991013      *****    Processing an RESP Acount transaction record, bypass it.
269600000000      *****
269700000000
269800991013           MOVE MFASTMTFP-SECTION      TO  AT-RESP-DETAIL-SECTION.
269900991013           ADD 1                       TO  WS-STMT-DTA-BCNT-82.
270000000000           GO TO GCR-0090.
270100000000
270200010308       GCR-0080-0020.
270300010308
270400010308      *****
270500010308      *****    Processing an Account Transaction Institution Detail
270600010308      *****    record, bypass it.
270700010308      *****
270800010308
270900010308           MOVE MFASTMTFP-SECTION      TO  AT-INSTITUTION-DETAIL-SECTION.
271000010308           ADD 1                       TO  WS-STMT-DTA-BCNT-84.
271100010308           GO TO GCR-0090.
271200010308
271300010308RF4638 GCR-0080-0030.
271400010308
271500010308      *****
271600010308      *****    Processing a Reset Contract Details record, bypass it.
271700010308      *****
271800010308
271900010308           MOVE MFASTMTFP-SECTION      TO  RESET-CONTRACT-DETAILS.
272000010308           ADD 1                       TO  WS-STMT-DTA-BCNT-90.
272100010308RF4638     GO TO GCR-0090.
272200010308
272300000000       GCR-0090.
272400000000
272500000000      *****
272600000000      *****    The record is to be bypassed.  Therefore, return to read
272700000000      *****    the next record.
272800000000      *****
272900000000
273000000000           GO TO GCR-0010.
273100000000
273200000000       GCR-0100.
273300000000
273400000000      *****
273500000000      *****    The record has been selected and is to be processed.
273600000000      *****    Format the current controls.
273700000000      *****
273800020921
273900020921           MOVE  ZEROES TO WS-MSTR-ACCOUNT-NO.
274000020921R14588     IF WS-LDA-STMT-CNFM-FLAG    IS NOT EQUAL TO "S"
274100020921  ¦           GO TO LR2-0095M
274200020921  ¦        END-IF.
274300020921  ¦
274400020921  ¦        INITIALIZE              DEALER-MISC-DETAILS-REC.
274500020921  ¦        MOVE AD-DEALER          TO DEALER-CODE
274600020921  ¦                                OF DEALER-MISC-DETAILS-REC.
274700020921  ¦
274800020921  ¦        READ DEALER-MISC-DETAILS
274900020921  ¦           INVALID KEY
275000020921  ¦               GO TO LR2-0095M
275100020921  ¦        END-READ.
275200020921  ¦
275300020921  ¦        IF MASTER-ACCT-PROCESS OF DEALER-MISC-DETAILS-REC
275400020921  ¦                                 IS NOT EQUAL TO "Y"
275500020921  ¦               GO TO LR2-0095M
275600020921  ¦        END-IF.
275700020921  ¦
275800020921  ¦        INITIALIZE              MASTER-ACCOUNT-REC.
275900020921  ¦        MOVE AD-ACCOUNT      TO ACCOUNT-NO OF MASTER-ACCOUNT-REC.
276000020921  ¦
276100020921  ¦        READ MASTER-ACCOUNT
276200020921  ¦           INVALID KEY
276300020921  ¦               GO TO LR2-0095M
276400020921  ¦        END-READ.
276500020921  ¦
276600020921  ¦        MOVE MASTER-ACCOUNT-NO OF MASTER-ACCOUNT-REC
276700020921  ¦                                    TO WS-MSTR-ACCOUNT-NO.
276800020921  ¦
276900020921R14588
277000020921
277100020921       LR2-0095M.
277200980210
277300980210           IF PR-START-DATE = SPACES
277400990921              MOVE ZEROES   TO PR-START-DATE
277500990921           END-IF
277600990921
277700980210           IF PR-END-DATE   = SPACES
277800990921              MOVE ZEROES   TO PR-END-DATE
277900990921           END-IF
278000990921
278100980128           MOVE PR-START-DATE          TO  CURR-START-DATE.
278200980128           MOVE PR-END-DATE            TO  CURR-END-DATE.
278300000000           MOVE PR-INVESTOR-NO         TO  CURR-INVESTOR-NO.
278400000000           MOVE PR-ACCOUNT             TO  CURR-ACCOUNT.
278500000000
278600990921      **********
278700990921
278800000000       GCR-EXIT.
278900000000           EXIT.
279000990921
279100990921      **********
279200990921
279300000000      /
279400000000       DETAIL-PROCESS-RTN.
279500000000
279600000000      ******************************************************************
279700000000      *                                                                *
279800000000      *    D E T A I L   P R O C E S S   R O U T I N E                 *
279900000000      *                                                                *
280000000000      ******************************************************************
280100000000
280200000000      *****
280300000000      *****    If an error was detected in a re-initialization routine,
280400000000      *****    bypass processing the record.
280500000000      *****
280600000000
280700000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
280800990922               GO TO DPR-EXIT
280900990922           END-IF
281000990922
281100000000      *****
281200000000      *****    Process the Statement Data record depending on the
281300000000      *****    section number.  If it cannot be identified, an error
281400000000      *****    message will be printed and the record bypassed.  If
281500000000      *****    this occurs, we have a real problem since the GET CURRENT
281600000000      *****    RECORD routine should have bypassed the record.
281700000000      *****
281800000000
281900990922           EVALUATE PR-SECTION-NO
282000990922               WHEN "01"            GO TO  DPR-0010
282100990922               WHEN "05"            GO TO  DPR-0020
282200990922               WHEN "10"            GO TO  DPR-0020-0010
282300990922               WHEN "20"            GO TO  DPR-0030
282400990922               WHEN "60"            GO TO  DPR-0180
282500990922               WHEN "80"            GO TO  DPR-0060
282600990922               WHEN OTHER
282700990922                  MOVE SPACES                 TO  ERROR-LINE-1
282800990922                  SET MSG                     TO  2
282900990922                  PERFORM PRINT-ERR-LINE     THRU PEL-EXIT
283000990922                  GO TO DPR-EXIT
283100990922           END-EVALUATE.
283200990922
283300000000       DPR-0010.
283400000000
283500000000      *****
283600000000      *****    Processing an Investor Information record.  Simply
283700000000      *****    indicate that one has been found for the account, then
283800000000      *****    exit.
283900000000      *****
284000000000
284100000000           MOVE "Y"                    TO  WS-STMT-DTA-SW-01.
284200000000           GO TO DPR-EXIT.
284300000000
284400000000       DPR-0020.
284500000000
284600000000      *****
284700000000      *****    Processing a Dealer record.  Simply indicate that one has
284800000000      *****    been found for the account, then exit.
284900000000      *****
285000000000
285100000000           MOVE "Y"                    TO  WS-STMT-DTA-SW-05.
285200000000           GO TO DPR-EXIT.
285300000000
285400000000       DPR-0020-0010.
285500000000
285600000000      *****
285700000000      *****    Processing an Investor Summary record.  Store the Account
285800000000      *****    information for Nominee and Registered Contribution.  If
285900000000      *****    the table is full, an error message will be printed and
286000000000      *****    the account will not be formatted.  If this occurs, the
286100000000      *****    table occurrence should be increased.  Remember to udpate
286200000000      *****    the setting of WS-SIA-MAX in INITIAL-LOGIC.
286300000000      *****
286400000000
286500000000           IF  WS-SIA-LST   NOT LESS THAN  WS-SIA-MAX
286600000000               MOVE SPACES             TO  ERROR-LINE-1
286700000000               SET MSG                 TO  11
286800000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
286900990922               GO TO DPR-0020-0060
287000990922           END-IF
287100990922
287200000000           IF  WS-SIA-LST     IS EQUAL TO  ZEROES
287300000000               SET SIA                 TO  1
287400990922               GO TO DPR-0020-0050
287500990922           END-IF
287600000000
287700000000           SET SIA                     TO  WS-SIA-LST.
287800990922
287900000000           IF  IS-ACCOUNT2   GREATER THAN  WS-SIA-ACCT (SIA)
288000000000               SET SIA              UP BY  1
288100990922               GO TO DPR-0020-0050
288200990922           END-IF
288300000000
288400000000           SET SIA                     TO  1.
288500000000
288600000000       DPR-0020-0020.
288700990922
288800000000           IF  IS-ACCOUNT2   IS LESS THAN  WS-SIA-ACCT (SIA)
288900990922               GO TO DPR-0020-0030
289000990922           END-IF
289100000000
289200000000           SET SIA                  UP BY  1.
289300990922
289400000000           IF  SIA    IS NOT GREATER THAN  WS-SIA-LST
289500990922               GO TO DPR-0020-0020
289600990922           END-IF.
289700000000
289800000000       DPR-0020-0030.
289900990922
290000000000           SET SIA1                    TO  WS-SIA-LST.
290100000000           SET SIA2                    TO  WS-SIA-LST.
290200990922           SET SIA2                    UP BY  1.
290300000000
290400000000       DPR-0020-0040.
290500990922
290600000000           IF  SIA1      IS NOT LESS THAN  SIA
290700000000               MOVE WS-SIA-ENT (SIA1)  TO  WS-SIA-ENT (SIA2)
290800000000               SET SIA1           DOWN BY  1
290900000000               SET SIA2           DOWN BY  1
291000990922               GO TO DPR-0020-0040
291100990922           END-IF.
291200000000
291300000000       DPR-0020-0050.
291400990922
291500000000           MOVE IS-ACCOUNT2            TO  WS-SIA-ACCT (SIA).
291600990922
291700990922           IF IS-NOMINEE     IS EQUAL  TO  "Y"
291800990922              MOVE IS-NOMINEE-CODE     TO  WS-SIA-NOMCDE (SIA)
291900990922              MOVE IS-NOMINEE-NAME     TO  WS-SIA-NOMNAM (SIA)
292000990922              MOVE IS-NOMINEES-ACCT-NO TO  WS-SIA-NOMACCT (SIA)
292100990922           ELSE
292200990922              MOVE SPACES              TO  WS-SIA-NOMCDE (SIA)
292300990922              MOVE SPACES              TO  WS-SIA-NOMNAM (SIA)
292400990922              MOVE SPACES              TO  WS-SIA-NOMACCT (SIA)
292500990922           END-IF
292600990922
292700000000           MOVE IS-REG-CTB-CUR         TO  WS-SIA-REGCTB (SIA).
292800000000           MOVE IS-REG-CTB-CUR-SIGN    TO  WS-SIA-REGCTBSGN (SIA).
292900000000           ADD 1                       TO  WS-SIA-LST.
293000000000
293100000000       DPR-0020-0060.
293200990922
293300000000           GO TO DPR-EXIT.
293400000000
293500000000       DPR-0030.
293600000000
293700000000      *****
293800000000      *****    Processing an Account Detail record.  These records will
293900000000      *****    be processed, after the Account Transaction records have
294000000000      *****    been processed, by the LEVEL SUMMARIZATION 1 routine.
294100000000      *****    For now, accumulate the fund market value by currency and
294200000000      *****    store the data.  If the currency cannot be identified, an
294300000000      *****    error message will be printed and the record bypassed.
294400000000      *****    If this occurs, the program should be changed to handle
294500000000      *****    the new currency.  Remember to update the setting of
294600000000      *****    the setting of WS-SC-MAX in INITIAL-LOGIC.
294700000000      *****
294800000000
294900000000           SET SC                      TO  1.
295000000000
295100000000       DPR-0040.
295200990922
295300000000           IF  AD-CURRENCY    IS EQUAL TO  WS-SC-CURRENCY (SC)
295400990922               GO TO DPR-0050
295500990922           END-IF
295600990922
295700000000           IF  SC            IS LESS THAN  WS-SC-MAX
295800000000               SET SC               UP BY  1
295900990922               GO TO DPR-0040
296000990922           END-IF
296100990922
296200000000           MOVE SPACES                 TO  ERROR-LINE-1.
296300000000           SET MSG                     TO  3.
296400000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
296500000000           GO TO DPR-EXIT.
296600000000
296700000000       DPR-0050.
296800990922
296900990922           IF AD-MARKET-VALUE-SIGN     IS EQUAL TO  SPACES
297000990922              ADD AD-MARKET-VALUE      TO  WS-SC-MARKET-VALUE (SC)
297100990922           ELSE
297200990922              SUBTRACT AD-MARKET-VALUE  FROM WS-SC-MARKET-VALUE (SC)
297300990922           END-IF
297400990922
297500000000           MOVE "Y"                    TO  WS-SC-RPTIND (SC).
297600000000
297700000000      *****
297800000000      *****    Format the fund in the Statement Account Fund table.  If
297900000000      *****    the table is full, an error message will be printed and
298000000000      *****    the record bypassed.  If this occurs, the program should
298100000000      *****    be changed to handle more funds for an account.  Remember
298200000000      *****    to update the setting of WS-SAF-MAX in INITIAL-LOGIC.
298300000000      *****
298400000000
298500000000           IF  WS-SAF-LST   NOT LESS THAN  WS-SAF-MAX
298600000000               MOVE SPACES             TO  ERROR-LINE-1
298700000000               SET MSG                 TO  4
298800000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
298900990922               GO TO DPR-EXIT
299000990922           END-IF
299100990922
299200000000      *****
299300000000      *****    Calculate the net invested, if required.
299400000000      *****
299500000000
299600000000           MOVE ZEROES                 TO  WS-NET-INVESTED.
299700000000
299800990922           IF  WS-LDA-PRT-NET-INVEST   IS NOT EQUAL TO  "Y"
299900990922               GO TO DPR-0050-0010
300000990922           END-IF
300100990922
300200000000           IF  AD-PI-SIGN     IS EQUAL TO  "-"
300300990922               SUBTRACT AD-PUR-INCEPT  FROM  WS-NET-INVESTED
300400990922           ELSE
300500990922               ADD AD-PUR-INCEPT       TO  WS-NET-INVESTED
300600990922           END-IF
300700990922
300800000000           IF  AD-RI-SIGN     IS EQUAL TO  "-"
300900000000               ADD AD-RED-INCEPT       TO  WS-NET-INVESTED
301000990922           ELSE
301100990922               SUBTRACT AD-RED-INCEPT  FROM  WS-NET-INVESTED
301200990922           END-IF
301300990922
301400000000           IF  AD-TI-SIGN     IS EQUAL TO  "-"
301500990922               SUBTRACT AD-TRI-INCEPT  FROM  WS-NET-INVESTED
301600990922           ELSE
301700990922               ADD AD-TRI-INCEPT       TO  WS-NET-INVESTED
301800990922           END-IF
301900990922
302000000000           IF  AD-OI-SIGN     IS EQUAL TO  "-"
302100000000               ADD AD-TRO-INCEPT       TO  WS-NET-INVESTED
302200990922           ELSE
302300990922               SUBTRACT AD-TRO-INCEPT  FROM  WS-NET-INVESTED
302400990922           END-IF.
302500020801
302600020806R13637     SUBTRACT AD-SWO-INCEPT  FROM  WS-NET-INVESTED.
302700020806R13637     ADD      AD-SWI-INCEPT  TO    WS-NET-INVESTED.
302800000000
302900000000       DPR-0050-0010.
303000990922
303100000000           ADD 1                       TO  WS-SAF-LST.
303200000000           SET SAF                     TO  WS-SAF-LST.
303300000000           MOVE AD-ACB-UNIT-PRICE      TO  WS-SAF-ACB-UNIT-PRICE (SAF).
303400000000           MOVE AD-ACB-UNIT-PRICE-SIGN TO  WS-SAF-ACB-UNIT-PRICE-SIGN
303500000000                                             (SAF).
303600030908R18092     IF AD-LANGUAGE = "E"
303700030908   |          MOVE AD-CURRENCY            TO  WS-SAF-CURRENCY (SAF)
303800030908   |       END-IF.
303900030908   |
304000030908   |       IF AD-LANGUAGE = "F"
304100030908   |          MOVE AD-CURRENCY            TO  WS-CURRENCY-XXX
304200030908   |          MOVE SPACES                 TO  WS-CURRENCY-XXX-W
304300030908   |          MOVE WS-CURRENCY-XX         TO  WS-CURRENCY-XX-W
304400030908   |          MOVE WS-CURRENCY-PIC3       TO  WS-SAF-CURRENCY (SAF)
304500030908   V       END-IF.
304600030908
304700000000           MOVE AD-FUND-NO             TO  WS-SAF-FUND-NO (SAF).
304800000000           MOVE AD-INVESTMENT-NAME     TO  WS-SAF-INVESTMENT-NAME (SAF).
304900000000           MOVE AD-LAST-UNIT-PRICE     TO  WS-SAF-LAST-UNIT-PRICE (SAF).
305000000000           MOVE AD-LAST-UNIT-PRICE-SIGN
305100000000                                       TO  WS-SAF-LAST-UNIT-PRICE-SIGN
305200000000                                             (SAF).
305300000000           MOVE AD-MARKET-VALUE        TO  WS-SAF-MARKET-VALUE (SAF).
305400000000           MOVE AD-MARKET-VALUE-SIGN   TO  WS-SAF-MARKET-VALUE-SIGN
305500000000                                             (SAF).
305600000000           IF  WS-NET-INVESTED  LESS THAN  ZEROES
305700000000               SUBTRACT WS-NET-INVESTED
305800000000                                     FROM  ZEROES
305900000000                                   GIVING  WS-SAF-NET-INVESTED (SAF)
306000000000               MOVE "-"                TO  WS-SAF-NET-INVESTED-SIGN
306100000000                                             (SAF)
306200990922           ELSE
306300000000               MOVE WS-NET-INVESTED    TO  WS-SAF-NET-INVESTED (SAF)
306400000000               MOVE SPACES             TO  WS-SAF-NET-INVESTED-SIGN
306500990922                                             (SAF)
306600990922           END-IF
306700990922
306800000000           MOVE AD-NO-OF-UNITS         TO  WS-SAF-NO-OF-UNITS (SAF).
306900000000           MOVE AD-NO-OF-UNITS-SIGN    TO  WS-SAF-NO-OF-UNITS-SIGN
307000000000                                             (SAF).
307100971223
307200971223      *****
307300971223      *****    Accumulate the Canadian dollar capital gains amount.
307400971223      *****
307500971223
307600990922           IF  AD-CPG-FIXED-SIGN EQUAL  TO  " "
307700990922               ADD AD-CPG-FIXED         TO  WS-CPG-FIXED
307800990922           ELSE
307900990922               SUBTRACT AD-CPG-FIXED    FROM  WS-CPG-FIXED
308000990922           END-IF
308100990922
308200000000           GO TO DPR-EXIT.
308300000000
308400000000       DPR-0060.
308500000000
308600000000      *****
308700000000      *****    Processing an Account Transaction record.  These records
308800000000      *****    will be processed using the MLS programming techniques,
308900000000      *****    without the performs.  The current and previous controls
309000000000      *****    have been initialized in the LEVEL REINITIALIZATION 1
309100000000      *****    routine and the records have been read in the GET CURRENT
309200000000      *****    routine.  Therefore, the first step will be to format the
309300000000      *****    current statement transaction controls.
309400000000      *****
309500000000
309600000000           MOVE AT-FUND-NO             TO  CURR-CD-FUND-NO.
309700000000
309800000000      *****
309900000000      *****    Determine if this is a new FUND being processed for the
310000000000      *****    account.
310100000000      *****
310200000000
310300000000           IF  PREV-CD-LEV1   IS EQUAL TO  CURR-CD-LEV1
310400990922               GO TO DPR-0120
310500990922           END-IF
310600000000
310700000000      *****
310800000000      *****    A new fund is being processed.  If this is the first fund
310900000000      *****    for the account, bypass the summary logic.
311000000000      *****
311100000000
311200000000           IF  PREV-CTLS-DPR  IS EQUAL TO  LOW-VALUES
311300990922               GO TO DPR-0070
311400990922           END-IF
311500990922
311600000000      *****
311700000000      *****    Place the fund summary logic here.  Currently, the only
311800000000      *****    processing required is to print a blank line.  Please
311900000000      *****    note that if the statement page is full, the blank line
312000000000      *****    is not formatted.  The next Account Transaction or the
312100000000      *****    Account Summary will skip to the new page, if required.
312200000000      *****
312300000000
312400000000           IF  SPL           IS LESS THAN  WS-SPL-DTLLST
312500990922               SET SPL              UP BY  1
312600990922           END-IF.
312700000000
312800000000       DPR-0070.
312900000000
313000000000      *****
313100000000      *****    Normally, MLS checks the current controls to determine if
313200000000      *****    processing is complete.  Since the Account Transactions
313300000000      *****    are read in the GET CURRENT RECORD routine, this will be
313400000000      *****    done by the normal MLS and will not be required here.
313500000000      *****    Therefore, place the fund re-initialization logic here.
313600000000      *****    Verify that an Account Detail has been read for the fund.
313700000000      *****    If one has not been, a warning message will be printed
313800000000      *****    and the record will be processed as normal.  If this
313900000000      *****    occurs, the program that creates the Statement Data file
314000000000      *****    should be reviewed.
314100000000      *****
314200000000
314300000000           SET SAF                     TO  1.
314400990922
314500000000           IF  SAF        IS GREATER THAN  WS-SAF-LST
314600990922               GO TO DPR-0090
314700990922           END-IF.
314800990922
314900000000       DPR-0080.
315000990922
315100000000           IF  CURR-CD-FUND-NO   EQUAL TO  WS-SAF-FUND-NO (SAF)
315200990922               GO TO DPR-0100
315300990922           END-IF
315400000000
315500000000           IF  SAF           IS LESS THAN  WS-SAF-LST
315600000000               SET SAF              UP BY  1
315700990922               GO TO DPR-0080
315800990922           END-IF.
315900000000
316000000000       DPR-0090.
316100990922
316200000000           MOVE SPACES                 TO  ERROR-LINE-1.
316300000000           SET MSG                     TO  5.
316400990922           PERFORM PRINT-ERR-LINE      THRU PEL-EXIT.
316500000000
316600000000       DPR-0100.
316700000000
316800000000      *****
316900000000      *****    Format the Investment Name.  If this line and at least
317000000000      *****    one transaction line cannot be printed on the current
317100000000      *****    page, process this page and start a subsequent page.
317200000000      *****
317300000000
317400000000           MOVE SPACES                 TO  STMT-DTL-01.
317500000000
317600990922           EVALUATE WS-STMTVRSN
317700091112R73890       WHEN "A"
317800091112R73890         MOVE AT-INVESTMENT-NAME TO  SD01VA-INVESTMENT-NAME
317900990922             WHEN OTHER
318000990922               MOVE AT-INVESTMENT-NAME TO  SD01-INVESTMENT-NAME
318100990922           END-EVALUATE.
318200990922
318300000000       DPR-0100-0010.
318400990921
318500000000           SET WS-SPL-DTLTST           TO  SPL.
318600990624
318700990624           IF  (WS-SPL-DTLTST           +  3)
318800000000                      IS NOT GREATER THAN  WS-SPL-DTLLST
318900990921               GO TO DPR-0110
319000990921           END-IF
319100990624
319200990624           IF WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
319300990624           AND (WS-SPL-DTLTST           +  5)
319400990624                      IS NOT GREATER THAN  WS-SPL-DTLLST
319500990921               GO TO DPR-0110
319600990921           END-IF
319700990624
319800000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
319900990921
320000000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
320100990921               GO TO DPR-EXIT
320200990921           END-IF.
320300000000
320400000000       DPR-0110.
320500990921
320600091112
320700990925           IF WS-STMTVRSN             IS EQUAL TO  6
320800990925              SET SPL                 UP BY 1
320900990925           ELSE
321000990925              IF WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
321100990925                 SET SPL              UP BY  3
321200990925              ELSE
321300990925                 SET SPL              UP BY  1
321400990925              END-IF
321500990925           END-IF
321600990925
321700990226           MOVE STMT-DTL-01         TO  WS-STMTPGE-LNE (SPL).
321800100210           SET SPL                  UP BY  1.
321900000000
322000091123
322100000000       DPR-0120.
322200000000
322300000000      *****
322400000000      *****    Place the detail processing logic here.  The Account
322500000000      *****    Transaction record is to be processed depending on the
322600000000      *****    Transaction Type Code.
322700000000      *****
322800000000
322900990921           IF  AT-TRANS-TYPE-CODE  IS EQUAL TO  "BFW"
323000990921               GO TO DPR-0130
323100990921           END-IF
323200000000
323300990921           IF  AT-TRANS-TYPE-CODE  IS EQUAL TO  "BCL"
323400990921               GO TO DPR-0130-0010
323500990921           END-IF
323600000000
323700000000           GO TO DPR-0140.
323800000000
323900000000       DPR-0130.
324000000000
324100000000      *****
324200000000      *****    Processing a BALANCE FORWARD transaction.  Format the
324300000000      *****    detail line.
324400000000      *****
324500000000
324600000000           MOVE SPACES                 TO  STMT-DTL-02.
324700000000
324800990921           EVALUATE WS-STMTVRSN
324900091119R73890       WHEN "A"
325000091207  |            MOVE WS-SD02-LIT1 (SL)  TO  SD02VA-LIT1
325100091119  |            MOVE AT-TRANS-TYPE      TO  SD02VA-TRANS-TYPE
325200091119  |            MOVE AT-UNIT-BALANCE    TO  SD02VA-UNIT-BALANCE
325300091119R73890         MOVE AT-UNIT-BALANCE-SIGN
325400091119                                       TO  SD02VA-UNIT-BALANCE-SIGN
325500990922             WHEN OTHER
325600990921               MOVE WS-SD02-LIT1 (SL)      TO  SD02-LIT1
325700990921               MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE
325800990921               MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE
325900990921               MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN
326000990921           END-EVALUATE.
326100990921
326200000000       DPR-0130-0000-0010.
326300990921
326400000000           MOVE ZEROES                 TO  WS-CPTL-GAIN.
326500000000           MOVE SPACES                 TO  WS-CPTL-GAIN-SIGN.
326600000000           GO TO DPR-0150.
326700000000
326800000000       DPR-0130-0010.
326900000000
327000000000      *****
327100000000      *****    Processing a BALANCE CLOSING transaction.  Format the
327200000000      *****    detail line.
327300000000      *****
327400000000
327500000000           MOVE SPACES                 TO  STMT-DTL-02.
327600000000
327700990921           EVALUATE WS-STMTVRSN
327800091119R73890       WHEN "A"
327900091119  |            MOVE WS-SH-END-DATE-2   TO  SD02VA-TRADE-DATE
328000091119  |            MOVE AT-TRANS-TYPE      TO  SD02VA-TRANS-TYPE
328100091119  |            MOVE AT-UNIT-BALANCE    TO  SD02VA-UNIT-BALANCE
328200091119R73890         MOVE AT-UNIT-BALANCE-SIGN
328300091119                                       TO  SD02VA-UNIT-BALANCE-SIGN
328400990922             WHEN OTHER
328500991013               MOVE WS-SH-END-DATE-2       TO  SD02-TRADE-DATE
328600990921               MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE
328700990921               MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE
328800990921               MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN
328900990921           END-EVALUATE.
329000990921
329100000000       DPR-0130-0020.
329200990921
329300000000           MOVE ZEROES                 TO  WS-CPTL-GAIN.
329400000000           MOVE SPACES                 TO  WS-CPTL-GAIN-SIGN.
329500000000           GO TO DPR-0150.
329600000000
329700000000       DPR-0140.
329800000000
329900000000      *****
330000000000      *****    Processing a transaction other than a BALANCE FORWARD or
330100000000      *****    BALANCE CLOSING.  Format the detail line.
330200000000      *****
330300000000      *****    Please note that the CAPITAL GAIN/LOSS is only being
330400000000      *****    formatted if it is non-zero.  If this does not satisfy
330500000000      *****    the requirements, logic should be added to format this
330600000000      *****    field base on the transaction type.
330700000000      *****
330800000000
330900000000           MOVE AT-TRADE-DATE          TO  WS-TRADE-DATE.
331000990923           MOVE AT-TRADE-DATE          TO  WS-TRADE-DATE-6.
331100000000           MOVE ZEROES                 TO  WS-DEDUCTIONS.
331200991013           MOVE SPACES                 TO  STMT-DTL-02.
331300991013
331400991103991103     IF WS-AT-RULE (AT1)            IS EQUAL TO "4"
331500991013           AND ( AT-TRANS-TYPE-CODE       IS EQUAL TO "BUY"
331600991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "SEL"
331700991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "TRI"
331800991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "TRO")
331900991013                MOVE AT-CONTR-REDEMP-DESCRIPTION
332000991013                              TO  AT-TRANS-TYPE
332100991013           END-IF
332200991103
332300991103991103     IF WS-LDA-MGMT-COM-CDE        IS EQUAL TO "CND"
332400991103           AND ( AT-CONTR-REDEMP-CODE    IS EQUAL TO "MFCCNV")
332500991103                MOVE AT-CONTR-REDEMP-DESCRIPTION
332600991103                              TO  AT-TRANS-TYPE
332700991103991103     END-IF
332800991013
332900990921           IF  AT-COMMISSION-SIGN  IS EQUAL TO  "-"
333000990921               SUBTRACT AT-COMMISSION FROM  WS-DEDUCTIONS
333100990921           ELSE
333200990921               ADD AT-COMMISSION       TO  WS-DEDUCTIONS
333300990921           END-IF
333400990921
333500001114RF4638     IF  AT-DSC-SIGN         IS EQUAL TO  "-"
333600001114               SUBTRACT AT-DSC        FROM  WS-DEDUCTIONS
333700001114           ELSE
333800001114               ADD AT-DSC              TO  WS-DEDUCTIONS
333900001114           END-IF
334000001114
334100000000           IF  AT-TAX-SIGN    IS EQUAL TO  "-"
334200000000               SUBTRACT AT-TAX       FROM  WS-DEDUCTIONS
334300990921           ELSE
334400990921               ADD AT-TAX              TO  WS-DEDUCTIONS
334500990921           END-IF
334600990921
334700010130RF9184     ADD  AT-ADMIN-FEES          TO  WS-DEDUCTIONS.
334800010130RF9298     ADD  AT-GST
334900050401R26894          AT-ERF
335000010130                AT-PST                 TO  WS-DEDUCTIONS.
335100010130
335200000000           IF  WS-DEDUCTIONS IS LESS THAN  ZEROES
335300000000               MOVE "-"                TO  WS-DEDUCTIONS-SIGN
335400990921           ELSE
335500990921               MOVE SPACES             TO  WS-DEDUCTIONS-SIGN
335600990921           END-IF
335700000000
335800990921980417     IF  WS-LDA-CAPITAL-GAIN  IS NOT EQUAL TO  "N"
335900000000               MOVE AT-CPTL-GAIN       TO  WS-CPTL-GAIN
336000000000               MOVE AT-CPTL-GAIN-SIGN  TO  WS-CPTL-GAIN-SIGN
336100990921           ELSE
336200000000               MOVE ZEROES             TO  WS-CPTL-GAIN
336300990921               MOVE SPACES             TO  WS-CPTL-GAIN-SIGN
336400091124           END-IF.
336500990921
336600091119       DPR-0140-0075.
336700091119R73890     IF  WS-STMTVRSN   NOT EQUAL TO  "A"
336800091119  |            GO TO DPR-0140-0080
336900091119  |        END-IF
337000091119  |
337100091119  |        MOVE WS-SH-TRADE-DATE       TO  SD02VA-TRADE-DATE.
337200091119  |        MOVE AT-TRANS-TYPE          TO  SD02VA-TRANS-TYPE.
337300091119  |        MOVE AT-GROSS               TO  SD02VA-GROSS.
337400091119  |        MOVE AT-GROSS-SIGN          TO  SD02VA-GROSS-SIGN.
337500091119  |        MOVE WS-DEDUCTIONS          TO  SD02VA-DEDUCTIONS.
337600091119  |        MOVE AT-NET                 TO  SD02VA-NET.
337700091119  |        MOVE AT-NET-SIGN            TO  SD02VA-NET-SIGN.
337800091119  |        MOVE AT-UNIT-PRICE          TO  SD02VA-UNIT-PRICE.
337900091119  |        MOVE AT-UNIT-PRICE-SIGN     TO  SD02VA-UNIT-PRICE-SIGN.
338000091119  |        MOVE AT-NO-OF-UNITS         TO  SD02VA-NO-OF-UNITS.
338100091119  |        MOVE AT-NO-OF-UNITS-SIGN    TO  SD02VA-NO-OF-UNITS-SIGN.
338200091119  |        MOVE AT-UNIT-BALANCE        TO  SD02VA-UNIT-BALANCE.
338300091119  |        MOVE AT-UNIT-BALANCE-SIGN   TO  SD02VA-UNIT-BALANCE-SIGN.
338400091119  |
338500091119  |        IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
338600091119  |            MOVE WS-CPTL-GAIN       TO  SD02VA-CPTL-GAIN
338700091119  |            MOVE WS-CPTL-GAIN-SIGN  TO  SD02VA-CPTL-GAIN-SIGN
338800091119R73890     END-IF
338900091119
339000991213           GO TO DPR-0140-9999.
339100991213
339200991213       DPR-0140-0080.
339300990921
339400000000           MOVE WS-SH-TRADE-DATE       TO  SD02-TRADE-DATE.
339500000000           MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE.
339600000000           MOVE AT-GROSS               TO  SD02-GROSS.
339700000000           MOVE AT-GROSS-SIGN          TO  SD02-GROSS-SIGN.
339800000000           MOVE WS-DEDUCTIONS          TO  SD02-DEDUCTIONS.
339900000000           MOVE AT-NET                 TO  SD02-NET.
340000000000           MOVE AT-NET-SIGN            TO  SD02-NET-SIGN.
340100000000           MOVE AT-UNIT-PRICE          TO  SD02-UNIT-PRICE.
340200000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02-UNIT-PRICE-SIGN.
340300000000           MOVE AT-NO-OF-UNITS         TO  SD02-NO-OF-UNITS.
340400000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02-NO-OF-UNITS-SIGN.
340500000000           MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE.
340600000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN.
340700990921
340800000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
340900000000               MOVE WS-CPTL-GAIN       TO  SD02-CPTL-GAIN
341000990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02-CPTL-GAIN-SIGN
341100990921           END-IF.
341200990921
341300000000       DPR-0140-9999.
341400990921
341500000000           GO TO DPR-0150.
341600000000
341700000000       DPR-0150.
341800000000
341900000000      *****
342000000000      *****    If the transaction cannot be printed on the current page,
342100000000      *****    process the current page and start a subsequent page.
342200000000      *****    When determining if the transaction can be printed on the
342300000000      *****    current page, additional checks regarding a capital gain
342400000000      *****    overflow line are included.
342500000000      *****
342600000000
342700990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  99999.99
342800000000           AND SPL           IS LESS THAN  WS-SPL-DTLLST
342900990921               GO TO DPR-0160
343000990921           END-IF
343100990921
343200000000           SET WS-SPL                  TO  SPL.
343300990921
343400000000           IF  WS-CPTL-GAIN  GREATER THAN  99999.99
343500000000           AND (WS-SPL + 1)  IS LESS THAN  WS-SPL-DTLLST
343600990921               GO TO DPR-0160
343700990921           END-IF
343800000000
343900000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
344000990921
344100000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
344200990921               GO TO DPR-EXIT
344300990921           END-IF.
344400000000
344500000000       DPR-0160.
344600990921
344700990921           SET SPL                     UP BY  1.
344800000000           MOVE STMT-DTL-02            TO  WS-STMTPGE-LNE (SPL).
344900000000
345000000000      *****
345100000000      *****    If the transaction Capital Gain value exceeds 99,999.99,
345200000000      *****    a second line will be printed to indicate that the value
345300000000      *****    is in excess of the printed amount.  This additional line
345400000000      *****    is due to space restrictions on the form itself.
345500000000      *****
345600000000
345700990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  99999.99
345800990921               GO TO DPR-0170
345900990921           END-IF
346000990921
346100000000           MOVE SPACES                 TO  STMT-DTL-02.
346200991214
346300091123R73890 DPR-0160-0085.
346400091123  |        IF  WS-STMTVRSN   NOT EQUAL TO  "A"
346500091123  |            GO TO DPR-0160-0090
346600091123  |        END-IF
346700091123  |
346800091123  |        IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
346900091123  |            GO TO DPR-0170
347000091123  |        END-IF
347100091123  |
347200091123  |        IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
347300091123  |            MOVE "(++++++++++)"     TO  SD02VA-CPTL-GAIN-OFLIT
347400091123  |          ELSE
347500091123  |            MOVE "(----------)"     TO  SD02VA-CPTL-GAIN-OFLIT
347600091123R73890     END-IF
347700091123
347800091123           GO TO DPR-0160-9999.
347900091123
348000091123       DPR-0160-0090.
348100990921
348200000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
348300000000               MOVE "(++++++++)"       TO  SD02-CPTL-GAIN-OFLIT
348400990921           ELSE
348500990921               MOVE "(--------)"       TO  SD02-CPTL-GAIN-OFLIT
348600990921           END-IF.
348700000000
348800000000       DPR-0160-9999.
348900990921
349000990924           IF WS-STMTVRSN              IS NOT EQUAL TO 6
349100990924             SET SPL                     UP BY  1
349200990924             MOVE STMT-DTL-02            TO  WS-STMTPGE-LNE (SPL)
349300990924           END-IF.
349400990924
349500000000       DPR-0170.
349600000000
349700000000      *****
349800000000      *****    The Account Transaction has been completely processed.
349900000000      *****    Update the previous statement transaction controls and
350000000000      *****    exit.  The next, if any, Account Transaction will be read
350100000000      *****    by the GET CURRENT RECORD routine.
350200000000      *****
350300000000
350400000000           MOVE CURR-CTLS-DPR          TO  PREV-CTLS-DPR.
350500981215           GO TO DPR-EXIT.
350600990921
350700981207       DPR-0180.
350800990921
350900981207           MOVE "Y"     TO WS-STMT-DTA-SW-60.
351000981207
351100990921      **********
351200990921
351300000000       DPR-EXIT.
351400000000           EXIT.
351500990921
351600990921      **********
351700990921
351800000000      /
351900000000       LEVEL-SUMMARY-1.
352000000000
352100000000      ******************************************************************
352200000000      *                                                                *
352300000000      *    L E V E L   S U M M A R I Z A T I O N   1   R O U T I N E   *
352400000000      *                                                                *
352500000000      *        ACCOUNT:                Account                         *
352600000000      *                                                                *
352700000000      ******************************************************************
352800000000
352900000000      *****
353000000000      *****    If Account is zeroes, bypass this routine.
353100000000      *****
353200000000
353300000000           IF  PREV-ACCOUNT   IS EQUAL TO  ZEROES
353400990921               GO TO LS1-EXIT
353500990921           END-IF
353600990921
353700000000      *****
353800000000      *****    If an error was detected by the LEVEL REINITIALIZATION 1
353900000000      *****    routine, bypass this routine.
354000000000      *****
354100000000
354200000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
354300990921               GO TO LS1-EXIT
354400990921           END-IF
354500000000
354600980529           IF  WS-LDA-SUM-SUPPRESS-FLAG  IS EQUAL TO "Y"
354700990921               GO TO LS1-0390
354800990921           END-IF
354900990921
355000000000      *****
355100000000      *****    If at least one Account Transaction was formatted, skip a
355200000000      *****    line.
355300000000      *****
355400000000
355500000000           IF  CURR-CTLS-DPR NOT EQUAL TO  LOW-VALUES
355600990921               SET SPL              UP BY  1
355700990921           END-IF
355800990921
355900000000      *****
356000000000      *****    If the account funds and currency totals cannot be
356100000000      *****    formatted on the current page, process the page and start
356200000000      *****    a subsequent page.  But, if the account funds and
356300000000      *****    currency totals cannot be formatted on their own page,
356400000000      *****    complete the current page.
356500000000      *****
356600000000
356700000000           SET WS-SPL-DTLTST           TO  SPL.
356800000000           ADD 4                       TO  WS-SPL-DTLTST.
356900000000           ADD WS-SAF-LST              TO  WS-SPL-DTLTST.
357000000000
357100000000           SET SC                      TO  1.
357200000000
357300000000       LS1-0010.
357400990921
357500000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
357600000000               ADD 1                   TO  WS-SPL-DTLTST
357700990921               GO TO LS1-0020
357800990921           END-IF
357900990921
358000000000           IF  SC            IS LESS THAN  WS-SC-MAX
358100000000               SET SC               UP BY  1
358200990921               GO TO LS1-0010
358300990921           END-IF.
358400000000
358500000000       LS1-0020.
358600990921
358700000000           SET SC                      TO  1.
358800000000
358900000000       LS1-0030.
359000990921
359100000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
359200990921               ADD 1                   TO  WS-SPL-DTLTST
359300990921           END-IF
359400000000
359500000000           IF  SC            IS LESS THAN  WS-SC-MAX
359600000000               SET SC               UP BY  1
359700990921               GO TO LS1-0030
359800990921           END-IF
359900000000
360000000000           IF  WS-SPL-DTLTST
360100000000                      IS NOT GREATER THAN  WS-SPL-DTLLST
360200990921               GO TO LS1-0050
360300990921           END-IF
360400990921
360500000000           SUBTRACT WS-SPL-DTLFST    FROM  WS-SPL-DTLLST
360600000000                                   GIVING  WS-SPL-DTLMAX.
360700000000           SET WS-SPL                  TO  SPL.
360800000000           SUBTRACT WS-SPL           FROM  WS-SPL-DTLTST.
360900990921
361000990921           IF  WS-SPL-DTLTST IS NOT GREATER THAN  WS-SPL-DTLMAX
361100990921               GO TO LS1-0040
361200990921           END-IF
361300990921
361400000000           SET SPL                  UP BY  7.
361500990921
361600000000           IF  SPL    IS NOT GREATER THAN  WS-SPL-DTLLST
361700000000               SET SPL            DOWN BY  7
361800990921               GO TO LS1-0050
361900990921           END-IF.
362000000000
362100000000       LS1-0040.
362200990921
362300000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
362400990921
362500000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
362600990921               GO TO LS1-EXIT
362700990921           END-IF.
362800990921
362900000000       LS1-0050.
363000000000
363100000000      *****
363200000000      *****    Format statement summary line 1.
363300000000      *****
363400000000
363500000000           MOVE SPACES                 TO  STMT-SUM-01.
363600990921
363700990921           EVALUATE WS-STMTVRSN
363800091123R73890       WHEN "A"
363900091207R73890         MOVE WS-SS01-LIT1 (SL) TO  SS01VA-LIT1
364000990922             WHEN OTHER
364100990921               MOVE WS-SS01-LIT1 (SL)   TO  SS01-LIT1
364200990921           END-EVALUATE.
364300990921
364400000000       LS1-0050-0010.
364500990921
364600000000           SET SPL                  UP BY  1.
364700990226           MOVE STMT-SUM-01         TO  WS-STMTPGE-LNE (SPL).
364800000000
364900000000      *****
365000000000      *****    Format statement summary line 2.
365100000000      *****
365200000000
365300000000           MOVE SPACES                 TO  STMT-SUM-02.
365400000000
365500091123R73890 LS1-0050-0010-0085.
365600091123  |
365700091123  |        IF  WS-STMTVRSN   NOT EQUAL TO  "A"
365800091123  |            GO TO LS1-0050-0010-0080
365900091123  |        END-IF.
366000091123  |
366100091123  |        IF WS-LDA-PRT-NET-INVEST IS EQUAL TO  "Y"
366200091207  |           MOVE WS-SS02-LIT0 (SL)      TO  SS02VA-LIT0
366300091123  |        END-IF.
366400091207  |           MOVE WS-SS02-LIT1 (SL)   TO  SS02VA-LIT1.
366500091207  |           MOVE WS-SS02-LIT2 (SL)   TO  SS02VA-LIT2.
366600091207  |           MOVE WS-SS02-LIT3 (SL)   TO  SS02VA-LIT3.
366700091207  |           MOVE WS-SS02-LIT4 (SL)   TO  SS02VA-LIT4.
366800091123R73890        GO TO LS1-0050-0020.
366900991213
367000991213       LS1-0050-0010-0080.
367100990921
367200990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
367300990923              MOVE WS-SS02-LIT0 (SL)      TO  SS02-LIT0
367400991013           END-IF.
367500991013              MOVE WS-SS02-LIT1 (SL)      TO  SS02-LIT1.
367600991013              MOVE WS-SS02-LIT2 (SL)      TO  SS02-LIT2.
367700991013              MOVE WS-SS02-LIT3 (SL)      TO  SS02-LIT3.
367800991013              MOVE WS-SS02-LIT4 (SL)      TO  SS02-LIT4.
367900000000
368000000000       LS1-0050-0020.
368100990921
368200990925           IF WS-STMTVRSN                 IS EQUAL TO  6
368300990925              SET SPL                     UP BY  2
368400990925           ELSE
368500990925              SET SPL                     UP BY  1
368600990925           END-IF
368700990925
368800990226           MOVE STMT-SUM-02         TO  WS-STMTPGE-LNE (SPL).
368900000000
369000000000      *****
369100000000      *****    Format statement summary line 3.
369200000000      *****
369300000000
369400000000           MOVE SPACES                 TO  STMT-SUM-03.
369500000000
369600091123R73890 LS1-0050-0020-0085.
369700091123  |
369800091123  |        IF  WS-STMTVRSN   NOT EQUAL TO  "A"
369900091123  |            GO TO LS1-0050-0020-0080
370000091123  |        END-IF.
370100091123  |
370200091123  |        IF WS-LDA-PRT-NET-INVEST IS EQUAL TO  "Y"
370300091207  |           MOVE WS-SS03-LIT0 (SL)      TO  SS03VA-LIT0
370400091123  |        END-IF.
370500091207  |           MOVE WS-SS03-LIT1 (SL)      TO  SS03VA-LIT1.
370600091207  |           MOVE WS-SS03-LIT2 (SL)      TO  SS03VA-LIT2.
370700091207  |           MOVE WS-SS03-LIT3 (SL)      TO  SS03VA-LIT3.
370800091207  |           MOVE WS-SS03-LIT4 (SL)      TO  SS03VA-LIT4.
370900091123R73890        GO TO LS1-0050-0030.
371000091123
371100991213       LS1-0050-0020-0080.
371200990921
371300990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
371400990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03-LIT0
371500991013           END-IF.
371600991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03-LIT1.
371700991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03-LIT2.
371800991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03-LIT3.
371900991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03-LIT4.
372000000000
372100000000       LS1-0050-0030.
372200990921
372300990925           SET SPL                        UP BY  1.
372400990925           MOVE STMT-SUM-03               TO  WS-STMTPGE-LNE (SPL).
372500000000
372600000000      *****
372700000000      *****    Format statement summary line 4, one line for each fund.
372800000000      *****
372900000000
373000000000           SET SAF                     TO  1.
373100000000
373200000000       LS1-0060.
373300990921
373400000000           IF  SAF        IS GREATER THAN  WS-SAF-LST
373500990921               GO TO LS1-0080
373600990921           END-IF
373700990921
373800000000           MOVE SPACES                 TO  STMT-SUM-04.
373900000000
374000091123R73890 LS1-0060-0085.
374100091123  |
374200091123  |        IF  WS-STMTVRSN   NOT EQUAL TO  "A"
374300091123  |            GO TO LS1-0060-0080
374400091123  |        END-IF
374500091123  |
374600091123  |        MOVE WS-SAF-CURRENCY (SAF)  TO  SS04VA-CURRENCY.
374700091123  |        MOVE WS-SAF-INVESTMENT-NAME (SAF)
374800091123  |                                    TO  SS04VA-INVESTMENT-NAME.
374900091123  |        IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
375000091123  |           MOVE WS-SAF-NET-INVESTED (SAF)
375100091123  |                                    TO  SS04VA-NET-INVESTED
375200091123  |           MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
375300091123  |                                    TO  SS04VA-NET-INVESTED-SIGN
375400091123  |        END-IF
375500091123  |
375600091123  |        MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
375700091123  |                                    TO  SS04VA-LAST-UNIT-PRICE.
375800091123  |        MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
375900091123  |                                    TO  SS04VA-LAST-UNIT-PRICE-SIGN.
376000091123  |        MOVE WS-SAF-NO-OF-UNITS (SAF)
376100091123  |                                    TO  SS04VA-NO-OF-UNITS.
376200091123  |        MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
376300091123  |                                    TO  SS04VA-NO-OF-UNITS-SIGN.
376400091123  |        MOVE WS-SAF-MARKET-VALUE (SAF)
376500091123  |                                    TO  SS04VA-MARKET-VALUE.
376600091123  |        MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
376700091123  |                                    TO  SS04VA-MARKET-VALUE-SIGN.
376800091123  |
376900091123  |        IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
377000091123  |           MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
377100091123  |                                    TO  SS04VA-ACB-UNIT-PRICE
377200091123  |           MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
377300091123  |                                    TO  SS04VA-ACB-UNIT-PRICE-SIGN
377400091123  |        END-IF
377500091123  |
377600091123R73890     GO TO LS1-0060-9999.
377700991213
377800991213       LS1-0060-0080.
377900990922
378000000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04-CURRENCY.
378100000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
378200000000                                       TO  SS04-INVESTMENT-NAME.
378300990922
378400990922           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
378500990922              MOVE WS-SAF-NET-INVESTED (SAF)
378600000000                                       TO  SS04-NET-INVESTED
378700990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
378800990922                                       TO  SS04-NET-INVESTED-SIGN
378900990922           END-IF
379000990922
379100000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
379200000000                                       TO  SS04-LAST-UNIT-PRICE.
379300000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
379400000000                                       TO  SS04-LAST-UNIT-PRICE-SIGN.
379500000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
379600000000                                       TO  SS04-NO-OF-UNITS.
379700000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
379800000000                                       TO  SS04-NO-OF-UNITS-SIGN.
379900000000           MOVE WS-SAF-MARKET-VALUE (SAF)
380000000000                                       TO  SS04-MARKET-VALUE.
380100000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
380200000000                                       TO  SS04-MARKET-VALUE-SIGN.
380300990922980417     IF WS-LDA-CAPITAL-GAIN     IS NOT EQUAL TO  "N"
380400990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
380500000000                                       TO  SS04-ACB-UNIT-PRICE
380600990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
380700990922                                       TO  SS04-ACB-UNIT-PRICE-SIGN
380800990922           END-IF.
380900990922
381000000000       LS1-0060-9999.
381100990922
381200000000           IF  SPL           IS LESS THAN  WS-SPL-DTLLST
381300000000               SET SPL              UP BY  1
381400990922               GO TO LS1-0070
381500990922           END-IF
381600000000
381700000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
381800990922
381900000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
382000990922               GO TO LS1-EXIT
382100990922           END-IF.
382200000000
382300000000       LS1-0070.
382400990922
382500000000           MOVE STMT-SUM-04            TO  WS-STMTPGE-LNE (SPL).
382600000000
382700990922           SET SAF                     UP BY  1.
382800000000           GO TO LS1-0060.
382900000000
383000000000       LS1-0080.
383100000000
383200000000      *****
383300000000      *****    If the currency totals cannot be formatted on the current
383400000000      *****    page, process the page and start a subsequent one, do not
383500000000      *****    print summary line 5.
383600000000      *****
383700000000
383800000000           SET WS-SPL-DTLTST           TO  SPL.
383900000000           SET SC                      TO  1.
384000000000
384100000000       LS1-0090.
384200990922
384300000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
384400000000               ADD 2                   TO  WS-SPL-DTLTST
384500990922               GO TO LS1-0100
384600990922           END-IF
384700000000
384800000000           IF  SC            IS LESS THAN  WS-SC-MAX
384900000000               SET SC               UP BY  1
385000990922               GO TO LS1-0090
385100990922           END-IF
385200000000
385300000000      *****
385400000000      *****    There are no currency total lines to be printed.
385500000000      *****
385600000000
385700000000           GO TO LS1-0160.
385800000000
385900000000       LS1-0100.
386000990922
386100000000           IF  SC        IS NOT LESS THAN  WS-SC-MAX
386200990922               GO TO LS1-0110
386300990922           END-IF
386400000000
386500000000           SET SC                   UP BY  1.
386600990922
386700000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
386800990922               ADD 1                   TO  WS-SPL-DTLTST
386900990922           END-IF
387000990922
387100000000           GO TO LS1-0100.
387200000000
387300000000       LS1-0110.
387400990922
387500990922           IF  WS-SPL-DTLTST  IS NOT GREATER THAN  WS-SPL-DTLLST
387600990922               GO TO LS1-0120
387700990922           END-IF
387800000000
387900000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
388000990922
388100000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
388200990922               GO TO LS1-EXIT
388300990922           END-IF
388400000000
388500000000           GO TO LS1-0130.
388600000000
388700000000       LS1-0120.
388800000000
388900000000      *****
389000000000      *****    Format statement summary line 5.
389100000000      *****
389200000000
389300990922           MOVE SPACES                   TO  STMT-SUM-05.
389400000000
389500990922           EVALUATE WS-STMTVRSN
389600091123R73890       WHEN "A"
389700091207R73890         MOVE WS-SS05-LIT1 (SL)  TO  SS05VA-LIT1
389800990922             WHEN OTHER
389900990922               MOVE WS-SS05-LIT1 (SL)      TO  SS05-LIT1
390000990922           END-EVALUATE.
390100000000
390200000000       LS1-0120-0010.
390300990922
390400990922           SET SPL                     UP BY  1.
390500000000           MOVE STMT-SUM-05            TO  WS-STMTPGE-LNE (SPL).
390600000000
390700000000       LS1-0130.
390800000000
390900000000      *****
391000000000      *****    Format statement summary line 6, one line for each
391100000000      *****    applicable currency.
391200000000      *****
391300000000
391400000000           SET SC                      TO  1.
391500000000
391600000000       LS1-0140.
391700990922
391800000000           IF  SC         IS GREATER THAN  WS-SC-MAX
391900990922               GO TO LS1-0160
392000990922           END-IF
392100000000
392200000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "N"
392300990922               GO TO LS1-0150
392400990922           END-IF
392500000000
392600000000           MOVE SPACES                 TO  STMT-SUM-06.
392700991213
392800091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
392900091207  |            MOVE WS-SS06-LIT1 (SL)  TO  SS06VA-LIT1
393000091123  |            MOVE WS-SC-CURRDESC (SC, SCL)
393100091123  |                                    TO  SS06VA-LIT-CURRDESC
393200091123  |            MOVE WS-SC-MARKET-VALUE (SC)
393300091123  |                                    TO  SS06VA-MARKET-VALUE
393400091123  |            GO TO LS1-0140-0010
393500091123R73890     END-IF
393600091123
393700990922           MOVE WS-SS06-LIT1 (SL)        TO  SS06-LIT1.
393800990922           MOVE WS-SC-CURRDESC (SC, SCL) TO  SS06-LIT-CURRDESC.
393900990922           MOVE WS-SC-MARKET-VALUE (SC)  TO  SS06-MARKET-VALUE.
394000000000
394100000000       LS1-0140-0010.
394200990922
394300990922           SET SPL                     UP BY  1.
394400000000           MOVE STMT-SUM-06            TO  WS-STMTPGE-LNE (SPL).
394500000000
394600000000       LS1-0150.
394700990922
394800990922           SET SC                      UP BY  1.
394900000000           GO TO LS1-0140.
395000000000
395100000000       LS1-0160.
395200971223
395300981207      *****
395400981207      *****    Determine if RRIF and LIF message is to be printed.
395500981207      *****
395600990922
395700981207           IF  WS-LDA-RIF-LIF-MSG IS NOT EQUAL TO "Y"
395800990922               GO TO LS1-0210
395900990922           END-IF
396000990922
396100981208           IF  WS-STMT-DTA-SW-60 IS NOT EQUAL TO "Y"
396200990922               GO TO LS1-0210
396300990922           END-IF
396400990922
396500981207           IF  WS-LDA-RRIF-FLAG IS NOT EQUAL TO "Y"
396600990922               GO TO LS1-0180
396700990922           END-IF
396800990922
396900981207           IF  AW-RIF-LIF-INDICATOR IS NOT EQUAL TO "R"
397000990922               GO TO LS1-0180
397100990922           END-IF
397200990922
397300981207           SET              SPL UP BY 5.
397400990922
397500981207           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
397600981207              SET           SPL DOWN BY 5
397700990922              GO TO LS1-0170
397800990922           END-IF
397900981207
398000981207           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
398100990922
398200981207           IF WS-PSRERR-SW NOT EQUAL TO SPACES
398300990922              GO TO LS1-EXIT
398400990922           END-IF.
398500981207
398600981207       LS1-0170.
398700990922
398800990120           MOVE WS-SYS-DATE               TO SS07V1-YEAR.
398900981207           MOVE WS-SS07V1-LIT1(SL)        TO SS07V1-LIT.
399000981207           MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V1-RRIF-LIF-AMT.
399100981207
399200981207           SET SPL UP BY 4.
399300981207           MOVE STMT-SUM-07-V1    TO WS-STMTPGE-LNE (SPL).
399400000104
399500981207           GO TO LS1-0210.
399600981207
399700981207       LS1-0180.
399800990922
399900981207           IF  WS-LDA-ACCOUNT-LIF IS NOT EQUAL TO "Y"
400000990922               GO TO LS1-0210
400100990922           END-IF
400200981207
400300981207           IF  AW-RIF-LIF-INDICATOR IS NOT EQUAL TO "L"
400400990922               GO TO LS1-0210
400500990922           END-IF
400600981207
400700981207           SET              SPL UP BY 5.
400800990922
400900981207           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
401000981207              SET           SPL DOWN BY 5
401100990922              GO TO LS1-0190
401200990922           END-IF
401300981207
401400981207           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
401500990922
401600981207           IF WS-PSRERR-SW NOT EQUAL TO SPACES
401700990922              GO TO LS1-EXIT
401800990922           END-IF.
401900981207
402000981207       LS1-0190.
402100990922
402200990120           MOVE WS-SYS-DATE               TO SS07V1-YEAR.
402300981207           MOVE WS-SS07V1-LIT2(SL)        TO SS07V1-LIT.
402400981207           MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V1-RRIF-LIF-AMT.
402500981207
402600981207           SET SPL UP BY 4.
402700981207           MOVE STMT-SUM-07-V1    TO WS-STMTPGE-LNE (SPL).
402800000104
402900981207           SET              SPL UP BY 2.
403000990922
403100981207           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
403200981207              SET           SPL DOWN BY 2
403300990922              GO TO LS1-0200
403400990922           END-IF
403500981207
403600981207           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
403700990922
403800981207           IF WS-PSRERR-SW NOT EQUAL TO SPACES
403900990922              GO TO LS1-EXIT
404000990922           END-IF.
404100981207
404200981207       LS1-0200.
404300990922
404400990120           MOVE WS-SYS-DATE               TO SS08V1-YEAR.
404500981207           MOVE WS-SS08V1-LIT(SL)         TO SS08V1-LIT.
404600981207           MOVE AW-MAXIMUM-ANNUAL-PAYMENT TO SS08V1-RRIF-LIF-AMT.
404700981207
404800981207           SET SPL UP BY 1.
404900000104           MOVE STMT-SUM-08-V1    TO WS-STMTPGE-LNE (SPL).
405000000104
405100981207       LS1-0210.
405200971223      *****
405300971223      *****    Determine if the Canadian dollar capital gains line is to
405400971223      *****    be printed.
405500971223      *****
405600971223
405700971223           IF  WS-CPG-FIXED   IS EQUAL TO  ZEROES
405800990929           OR  WS-STMTVRSN    IS EQUAL TO  6
405900990922               GO TO LS1-0390
406000990922           END-IF
406100971223
406200971223      *****
406300971223      *****    The Canadian dollar capital gains line is to be printed.
406400971223      *****    This line is to be printed on the second last detail
406500971223      *****    line, at least two lines below the last detail line
406600971223      *****    formatted.  If this is not possible, process the current
406700971223      *****    page and start a sbusequent one.
406800971223      *****
406900971223
407000971223           SUBTRACT 5                FROM  WS-SPL-DTLLST
407100971223                                   GIVING  WS-SPL-DTLTST.
407200971223           IF  WS-SPL-DTLTST GREATER THAN  SPL
407300990922               GO TO LS1-0370
407400990922           END-IF
407500990922
407600971223           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
407700990922
407800971223           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
407900990922               GO TO LS1-EXIT
408000990922           END-IF.
408100971223
408200971223       LS1-0370.
408300990922
408400971223           IF  WS-CPG-FIXED  IS LESS THAN  ZEROES
408500971223               MOVE "-"                TO  WS-CPG-FIXED-SIGN
408600990922           ELSE
408700990922               MOVE SPACES             TO  WS-CPG-FIXED-SIGN
408800990922           END-IF
408900971223
409000981207           MOVE SPACES                 TO  STMT-SUM-11.
409100971223
409200091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
409300091123   |           MOVE WS-SS11-LIT1 (SL)  TO  SS11VA-LIT1
409400091123   |           MOVE WS-CPG-FIXED       TO  SS11VA-CPG-FIXED
409500091123   |           MOVE WS-CPG-FIXED-SIGN  TO  SS11VA-CPG-FIXED-SIGN
409600091123   |           GO TO LS1-0380
409700091123R73890     END-IF.
409800091123
409900971223       LS1-0380.
410000990922
410100971223           SET SPL                     TO  WS-SPL-DTLLST.
410200971223           SET SPL                DOWN BY  2.
410300981207           MOVE STMT-SUM-11            TO  WS-STMTPGE-LNE (SPL).
410400971223
410500971223       LS1-0390.
410600000000
410700000000      *****
410800000000      *****    Process the current page.
410900000000      *****
411000000000
411100000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
411200990922
411300000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
411400990922               GO TO LS1-EXIT
411500990922           END-IF
411600990922
411700000000           CONTINUE.
411800000000
411900990922      **********
412000990922
412100000000       LS1-EXIT.
412200000000           EXIT.
412300990922
412400990922      **********
412500990922
412600000000      /
412700000000       PRINT-STMT-RTN.
412800000000
412900000000      ******************************************************************
413000000000      *                                                                *
413100000000      *    P R I N T   S T A T E M E N T   R O U T I N E               *
413200000000      *                                                                *
413300000000      ******************************************************************
413400000000
413500000000      *****
413600000000      *****    If the Statement Print Header record has not been
413700000000      *****    created, create it.  The record was formatted in the
413800000000      *****    LEVEL REINITIALIZATION 1 routine.
413900000000      *****
414000000000
414100000000           MOVE SPACES                 TO  WS-PSRERR-SW.
414200000000
414300990922           IF  WS-STMT-PRT-HDR-WSW     IS EQUAL TO  "Y"
414400990922               GO TO PSR-0010
414500990922           END-IF
414600990922
414700000000           WRITE STMT-PRT-HDR-RECD
414800000000               INVALID KEY
414900000000                   MOVE "H"            TO  WS-PSRERR-SW
415000000000                   GO TO PSR-9999.
415100000000
415200000000           ADD 1                       TO  WS-STMT-PRT-HDR-WCNT.
415300000000           MOVE "Y"                    TO  WS-STMT-PRT-HDR-WSW.
415400000000
415500000000       PSR-0010.
415600000000
415700000000      *****
415800000000      *****    Format and print the Statement Print Detail records.  One
415900000000      *****    record will be created for each statement line.  If the
416000000000      *****    statement line is spaces, no record will be created.
416100000000      *****
416200000000
416300000000           SET SPL                     TO  1.
416400000000
416500000000       PSR-0020.
416600990922
416700990922           IF  WS-STMTPGE-LNE (SPL)    IS EQUAL TO  SPACES
416800990922               GO TO PSR-0030
416900990922           END-IF
417000990922
417100000000           MOVE SPACES                 TO  STMT-PRT-DTL-RECD.
417200000000           MOVE INVESTOR-NO
417300000000                OF STMT-PRT-HDR-RECD   TO  INVESTOR-NO
417400000000                                           OF STMT-PRT-DTL-RECD.
417500000000           MOVE ACCOUNT
417600000000                OF STMT-PRT-HDR-RECD   TO  ACCOUNT
417700000000                                           OF STMT-PRT-DTL-RECD.
417800000000           MOVE LANGUAGE-CODE
417900000000                OF STMT-PRT-HDR-RECD   TO  LANGUAGE-CODE
418000000000                                           OF STMT-PRT-DTL-RECD.
418100000000           MOVE DEALER
418200000000                OF STMT-PRT-HDR-RECD   TO  DEALER
418300000000                                           OF STMT-PRT-DTL-RECD.
418400000000           MOVE DEALER-REP-CODE
418500000000                OF STMT-PRT-HDR-RECD   TO  DEALER-REP-CODE
418600000000                                           OF STMT-PRT-DTL-RECD.
418700000000           MOVE WS-SH-PGENO            TO  STMPGENO
418800000000                                           OF STMT-PRT-DTL-RECD.
418900000000           SET STMLNENO
419000000000               OF STMT-PRT-DTL-RECD    TO  SPL.
419100000000           MOVE WS-STMTPGE-LNE (SPL)   TO  STMLNE
419200000000                                           OF STMT-PRT-DTL-RECD.
419300980129           MOVE START-DATE
419400980129                OF STMT-PRT-HDR-RECD   TO  START-DATE
419500980129                                           OF STMT-PRT-DTL-RECD.
419600980129           MOVE END-DATE
419700980129                OF STMT-PRT-HDR-RECD   TO  END-DATE
419800980129                                           OF STMT-PRT-DTL-RECD.
419900980129
420000000000           WRITE STMT-PRT-DTL-RECD
420100000000               INVALID KEY
420200000000                   MOVE "D"            TO  WS-PSRERR-SW
420300000000                   GO TO PSR-9999.
420400000000
420500000000           ADD 1                       TO  WS-STMT-PRT-DTL-WCNT.
420600000000
420700000000       PSR-0030.
420800990922
420900000000           IF  SPL           IS LESS THAN  WS-SPL-MAX
421000000000               SET SPL              UP BY  1
421100990922               GO TO PSR-0020
421200990922           END-IF
421300990922
421400000000      *****
421500000000      *****    Format a new page for the account statement.
421600000000      *****
421700000000
421800000000           ADD 1                       TO  WS-SH-PGENO.
421900000000
422000990922           EVALUATE WS-STMTVRSN
422100091123R73890       WHEN "A"
422200091123R73890         MOVE WS-SH-PGENO        TO  SH02VA-PGENO
422300990922             WHEN OTHER
422400990922               MOVE WS-SH-PGENO        TO  SH02-PGENO
422500990922           END-EVALUATE.
422600000000
422700000000       PSR-0040.
422800990927
422900990927           IF WS-STMTVRSN              IS NOT EQUAL TO  6
423000990927              SET SPL                  TO  WS-SPL-SH02
423100990927              MOVE STMT-HDG-02         TO  WS-STMTPGE-LNE (SPL)
423200990927           END-IF
423300990927
423400091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
423500091123  |            MOVE SPACES             TO  WS-STMTPGE-DTL-VA
423600091123  |            GO TO PSR-0050
423700091123R73890     END-IF
423800991213
423900000000           MOVE SPACES                 TO  WS-STMTPGE-DTL.
424000000000
424100000000       PSR-0050.
424200990922
424300000000           SET SPL                     TO  WS-SPL-DTLFST.
424400000000           GO TO PSR-EXIT.
424500000000
424600000000       PSR-9999.
424700000000
424800000000      *****
424900000000      *****    An error was detected attempting to write either a
425000000000      *****    Statement Print Header record or a Statement Print Detail
425100000000      *****    record.  An error message will be printed and the program
425200000000      *****    terminated.
425300000000      *****
425400000000
425500000000           IF  WS-PSRERR-SW   IS EQUAL TO  "H"
425600000000               SET MSG                 TO  6
425700990922           ELSE
425800990922               SET MSG                 TO  7
425900990922           END-IF
426000990922
426100000000           MOVE SPACES                 TO  ERROR-LINE-1.
426200000000           MOVE INVESTOR-NO
426300000000                OF STMT-PRT-HDR-RECD   TO  EL1-INVESTOR-NO.
426400000000           MOVE ACCOUNT
426500000000                OF STMT-PRT-HDR-RECD   TO  EL1-ACCOUNT.
426600000000           MOVE LANGUAGE-CODE
426700000000                OF STMT-PRT-HDR-RECD   TO  EL1-LANGUAGE-CODE.
426800000000           MOVE DEALER
426900000000                OF STMT-PRT-HDR-RECD   TO  EL1-DEALER.
427000000000           MOVE DEALER-REP-CODE
427100000000                OF STMT-PRT-HDR-RECD   TO  EL1-DEALER-REP-CODE.
427200000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
427300000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
427400000000
427500000000           MOVE SPACES                 TO  ERROR-LINE-1.
427600990922
427700000000           IF  WS-PSRERR-SW   IS EQUAL TO  "H"
427800000000               MOVE STMT-PRT-HDR-RECD  TO  ERROR-LINE-1
427900990922           ELSE
428000990922               MOVE STMT-PRT-DTL-RECD  TO  ERROR-LINE-1
428100990922           END-IF
428200990922
428300000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
428400000000
428500000000           MOVE SPACES                 TO  ERROR-LINE-1.
428600000000           MOVE WS-FILSTAT             TO  ERROR-LINE-1.
428700000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
428800000000
428900000000           PERFORM DUMP-ROUTINE       THRU DR-EXIT.
429000000000           MOVE HIGH-VALUES            TO  CURR-CONTROLS.
429100000000
429200990922      **********
429300990922
429400000000       PSR-EXIT.
429500000000           EXIT.
429600990922
429700990922      **********
429800990922
429900000000      /
430000000000       DUMP-ROUTINE.
430100000000
430200000000      ******************************************************************
430300000000      *                                                                *
430400000000      *    D U M P   R O U T I N E                                     *
430500000000      *                                                                *
430600000000      ******************************************************************
430700000000
430800000000      *****
430900000000      *****    Produce a program dump for analysis.
431000000000      *****
431100000000
431200000000           CALL    "QLREXHAN"       USING  DUMP-ARGUMENTS.
431300000000
431400990922      **********
431500990922
431600000000       DR-EXIT.
431700000000           EXIT.
431800990922
431900990922      **********
432000990922
432100000000      /
432200000000       LEVEL-SUMMARY-2.
432300000000
432400000000      ******************************************************************
432500000000      *                                                                *
432600000000      *    L E V E L   S U M M A R I Z A T I O N   2   R O U T I N E   *
432700000000      *                                                                *
432800000000      *        INVESTOR-NO:            Investor Number                 *
432900000000      *                                                                *
433000000000      ******************************************************************
433100000000
433200000000      *****
433300000000      *****    Null routine.
433400000000      *****
433500000000
433600000000           CONTINUE.
433700000000
433800990921      **********
433900990921
434000000000       LS2-EXIT.
434100000000           EXIT.
434200990921
434300990921      **********
434400990921
434500000000      /
434600000000       LEVEL-REINIT-2.
434700000000
434800000000      ******************************************************************
434900000000      *                                                                *
435000000000      *    L E V E L   R E I N I T I A L I Z A T I O N   2   R T N .   *
435100000000      *                                                                *
435200000000      *        INVESTOR-NO:            Investor Number                 *
435300000000      *                                                                *
435400000000      ******************************************************************
435500000000
435600000000      *****
435700000000      *****    Indicate that the Investor Information record and the
435800000000      *****    Dealer record have not yet been read.
435900000000      *****
436000000000
436100000000           MOVE "N"                    TO  WS-STMT-DTA-SW-01.
436200000000           MOVE "N"                    TO  WS-STMT-DTA-SW-05.
436300000000
436400000000      *****
436500000000      *****    Initialize the statement investor accounts table.
436600000000      *****
436700000000
436800000000           MOVE HIGH-VALUES            TO  WS-STMT-INVESTOR-ACCOUNT-TBL.
436900000000           MOVE ZEROES                 TO  WS-SIA-LST.
437000000000
437100990921      **********
437200990921
437300000000       LR2-EXIT.
437400000000           EXIT.
437500990921
437600990921      **********
437700990921
437800000000      /
437900000000       LEVEL-REINIT-1.
438000000000
438100000000      ******************************************************************
438200000000      *                                                                *
438300000000      *    L E V E L   R E I N I T I A L I Z A T I O N   1   R T N .   *
438400000000      *                                                                *
438500000000      *        ACCOUNT:                Account                         *
438600000000      *                                                                *
438700000000      ******************************************************************
438800000000
438900000000      *****
439000000000      *****    If Account is zeroes, bypass this routine.
439100000000      *****
439200000000
439300990921           IF CURR-ACCOUNT   IS EQUAL TO  ZEROES
439400990921              GO TO LR1-EXIT
439500990921           END-IF
439600990921
439700000000      *****
439800000000      *****    If one of the following conditions is detected, an error
439900000000      *****    message will be printed and the account will be bypassed.
440000000000      *****       1. Investor Information record missing.
440100000000      *****       2. Dealer record missing.
440200000000      *****       3. Account Detail record missing.
440300000000      *****    If this occurs, the program that creates the Statement
440400000000      *****    Data file should be reviewed.
440500000000      *****
440600000000
440700000000           MOVE "N"                    TO  WS-L01BYP-SW.
440800981208           MOVE "N"                    TO  WS-STMT-DTA-SW-60.
440900981208
441000000000           IF  WS-STMT-DTA-SW-01 EQUAL TO  "Y"
441100000000           AND WS-STMT-DTA-SW-05 EQUAL TO  "Y"
441200000000           AND PR-SECTION-NO  IS EQUAL TO  20
441300991103991103         GO TO LR1-0015
441400990921           END-IF
441500990921
441600000000           MOVE SPACES                 TO  ERROR-LINE-1.
441700000000
441800000000           IF  WS-STMT-DTA-SW-01 EQUAL TO  "N"
441900000000               SET MSG                 TO  8
442000990921               GO TO LR1-0010
442100990921           END-IF
442200000000
442300000000           IF  WS-STMT-DTA-SW-05 EQUAL TO  "N"
442400000000               SET MSG                 TO  9
442500990921               GO TO LR1-0010
442600990921           END-IF
442700000000
442800000000           IF  PR-SECTION-NO NOT EQUAL TO  20
442900000000               SET MSG                 TO  10
443000990921               GO TO LR1-0010
443100990921           END-IF
443200000000
443300000000           CONTINUE.
443400000000
443500000000       LR1-0010.
443600990921
443700000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
443800000000           MOVE "Y"                    TO  WS-L01BYP-SW.
443900000000           GO TO LR1-EXIT.
444000991103
444100991103      *****************************************************
444200991103
444300991103       LR1-0015.
444400991103
444500991103      *****
444600991103      *****    Search the account type table for the correct
444700991103      *****    account type rule.
444800991103      *****
444900991103
445000991103           SET AT1                     TO  1.
445100991103           IF  WS-AT-LST        IS NOT GREATER THAN  ZEROES
445200991103               GO TO LR1-0020
445300991103           END-IF.
445400991103
445500991103       LR1-0015-000.
445600991103
445700190612      *RFS183449-Starts
445800190612991103*    IF  AD-ACCOUNT-TYPE-SHORT  IS EQUAL TO  WS-AT-CODE (AT1)
445900190612           INITIALIZE Lc-Acct-Type-Code
446000190619           PERFORM Get-Account-Type-Code
446100190612           IF Lc-Acct-Type-Code IS EQUAL TO  WS-AT-CODE (AT1)
446200190612      *RFS183449-Ends
446300991103               GO TO LR1-0020
446400991103           END-IF
446500991103
446600991103           SET AT1                  UP BY  1.
446700991103
446800991103           IF  AT1    IS NOT GREATER THAN  WS-AT-LST
446900991103               GO TO LR1-0015-000
447000991103           END-IF.
447100991103
447200991103      *************************************************************
447300991103
447400000000       LR1-0020.
447500000000
447600000000      *****
447700000000      *****    Find the account information in the investor account
447800000000      *****    table.  Format the nominee and registered contribution
447900000000      *****    data and print a message, if required.
448000000000      *****
448100000000
448200000000           MOVE SPACES                 TO  WS-NOMINEE.
448300000000           MOVE SPACES                 TO  WS-NOMINEE-CODE.
448400000000           MOVE SPACES                 TO  WS-NOMINEE-NAME.
448500000000           MOVE SPACES                 TO  WS-NOMINEES-ACCT-NO.
448600971224           MOVE ZEROES                 TO  WS-CPG-FIXED.
448700971224           MOVE SPACES                 TO  WS-CPG-FIXED-SIGN.
448800000000           MOVE ZEROES                 TO  WS-REG-CTB-CUR.
448900000000           MOVE SPACES                 TO  WS-REG-CTB-CUR-SIGN.
449000000000
449100000000           SEARCH ALL WS-SIA-ENT
449200000000               AT END
449300000000                   GO TO LR1-0020-0020
449400000000               WHEN WS-SIA-ACCT (SIA)
449500000000                              IS EQUAL TO  AD-ACCOUNT
449600000000                   MOVE WS-SIA-NOMCDE (SIA)
449700000000                                       TO  WS-NOMINEE-CODE
449800000000                   MOVE WS-SIA-NOMNAM (SIA)
449900000000                                       TO  WS-NOMINEE-NAME
450000000000                   MOVE WS-SIA-NOMACCT (SIA)
450100000000                                       TO  WS-NOMINEES-ACCT-NO
450200000000                   MOVE WS-SIA-REGCTB (SIA)
450300000000                                       TO  WS-REG-CTB-CUR
450400000000                   MOVE WS-SIA-REGCTBSGN (SIA)
450500020918                                       TO  WS-REG-CTB-CUR-SIGN.
450600000000
450700000000       LR1-0020-0020.
450800000000
450900000000      *****
451000000000      *****    If the account is flagged as NOMINEE but no nominee data
451100000000      *****    was found, an error message will be printed and the
451200000000      *****    nominee name will be defaulted to 'NOMINEE'.  If this
451300000000      *****    occurs, the program that creates the Statement Data file
451400000000      *****    should be reviewed.
451500000000      *****
451600000000
451700000000           IF  AD-NOMINEE     IS EQUAL TO  "Y"
451800000000           AND WS-NOMINEE-CODE   EQUAL TO  SPACES
451900000000           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
452000000000           AND WS-NOMINEES-ACCT-NO
452100000000                              IS EQUAL TO  SPACES
452200000000               MOVE SPACES             TO  ERROR-LINE-1
452300000000               SET MSG                 TO  12
452400000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
452500000000               MOVE "NOMINEE"          TO  WS-NOMINEE-NAME.
452600000000
452700000000      *****
452800000000      *****    If the account is not flagged as NOMINEE but nominee data
452900000000      *****    is found, an error message will be printed and the
453000000000      *****    nominee data will be set to spaces.  If this occurs, the
453100000000      *****    program that creates the Statement Data file should be
453200000000      *****    reviewed.
453300000000      *****
453400000000
453500000000           IF  AD-NOMINEE     IS EQUAL TO  "Y"
453600000000               GO TO LR1-0020-0030.
453700000000
453800000000           IF  WS-NOMINEE-CODE
453900000000                          IS NOT EQUAL TO  SPACES
454000000000           OR  WS-NOMINEE-NAME
454100000000                          IS NOT EQUAL TO  SPACES
454200000000           OR  WS-NOMINEES-ACCT-NO
454300000000                          IS NOT EQUAL TO  SPACES
454400000000               MOVE SPACES             TO  ERROR-LINE-1
454500000000               SET MSG                 TO  13
454600000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
454700000000               MOVE SPACES             TO  WS-NOMINEE-CODE
454800000000               MOVE SPACES             TO  WS-NOMINEE-NAME
454900000000               MOVE SPACES             TO  WS-NOMINEES-ACCT-NO.
455000000000
455100000000       LR1-0020-0030.
455200000000
455300000000      *****
455400000000      *****    Format the nominee data, if applicable.
455500000000      *****
455600000000
455700000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
455800000000           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
455900000000           AND WS-NOMINEES-ACCT-NO
456000000000                              IS EQUAL TO  SPACES
456100000000               GO TO LR1-0020-0040.
456200000000
456300000000           IF  WS-NOMINEES-ACCT-NO
456400000000                              IS EQUAL TO  SPACES
456500000000               MOVE WS-NOMINEE-NAME    TO  WS-NOMINEE
456600000000               GO TO LR1-0020-0040.
456700000000
456800000000           STRING WS-NOMINEE-NAME
456900000000                             DELIMITED BY  "  ",
457000000000                  " ("       DELIMITED BY  SIZE,
457100000000                  WS-NOMINEES-ACCT-NO
457200000000                             DELIMITED BY  " ",
457300000000                  ")"        DELIMITED BY  SIZE
457400000000                                     INTO  WS-NOMINEE.
457500000000
457600000000       LR1-0020-0040.
457700000000
457800000000      *****
457900000000      *****    The literals will be formatted depending on the account
458000000000      *****    language code.  If the language code is not handled,
458100000000      *****    default to English.
458200000000      *****
458300000000
458400000000           IF  AD-LANGUAGE    IS EQUAL TO  "E"
458500000000               SET SCL                 TO  1
458600990623               SET SHL                 TO  1
458700000000               SET SL                  TO  1
458800000000               GO TO LR1-0030.
458900000000
459000000000           IF  AD-LANGUAGE    IS EQUAL TO  "F"
459100000000               SET SCL                 TO  2
459200990623               SET SHL                 TO  2
459300000000               SET SL                  TO  2
459400000000               GO TO LR1-0030.
459500000000
459600000000           SET SCL                     TO  1.
459700990623           SET SHL                     TO  1.
459800000000           SET SL                      TO  1.
459900000000       LR1-0030.
460000000000
460100000000      *****
460200000000      *****    Format the required fields.
460300000000      *****
460400000000
460500000000           MOVE II-END-DATE            TO  WS-END-DATE.
460600990923           MOVE II-END-DATE            TO  WS-END-DATE-6.
460700000000           MOVE "N"                    TO  WS-PSTCDEPRT-SW.
460800000000           MOVE II-START-DATE          TO  WS-START-DATE.
460900000000
461000000000      *****
461100000000      *****    The Statement File Generation (ANNSTMTJSI) formats and
461200000000      *****    creates the Dealer record using the Dealer & Dealer
461300000000      *****    Representative of the first account for the investor.  If
461400000000      *****    an investor has more than one account, the Dealer &
461500000000      *****    Dealer Representative of the accounts may differ.  If
461600000000      *****    this is the case, the Dealer & Dealer Representative
461700000000      *****    names and address lines will be retrieved and formatted.
461800000000      *****
461900000000
462000000000           MOVE SPACES                 TO  WS-BRANCH-ADDR1.
462100000000           MOVE SPACES                 TO  WS-BRANCH-ADDR2.
462200000000           MOVE SPACES                 TO  WS-BRANCH-ADDR3.
462300000000           MOVE SPACES                 TO  WS-BRANCH-ADDR4.
462400000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
462500000000           MOVE SPACES                 TO  WS-DEALER-REP-NAMES.
462600000000           MOVE ZEROES                 TO  WS-FAX.
462700000000           MOVE ZEROES                 TO  WS-PHONE.
462800000000           MOVE SPACES                 TO  WS-REP-NAME.
462900990115           MOVE SPACES                 TO  WS-REP-NAME-1.
463000000000
463100000000           IF  AD-SALESREP    IS EQUAL TO  DL-SALESREP
463200000000           AND AD-DEALER      IS EQUAL TO  DL-DEALER
463300000000               MOVE DL-BRANCH-ADDR1    TO  WS-BRANCH-ADDR1
463400000000               MOVE DL-BRANCH-ADDR2    TO  WS-BRANCH-ADDR2
463500000000               MOVE DL-BRANCH-ADDR3    TO  WS-BRANCH-ADDR3
463600000000               MOVE DL-BRANCH-ADDR4    TO  WS-BRANCH-ADDR4
463700000000               STRING DL-DEALER-NAME
463800000000                             DELIMITED BY  "  ",
463900000000                      " /"   DELIMITED BY  SIZE,
464000000000                      DL-REP-NAME
464100000000                             DELIMITED BY  SIZE
464200000000                                     INTO  WS-DEALER-REP-NAMES
464300000000               MOVE DL-FAX             TO  WS-FAX
464400000000               MOVE DL-PHONE           TO  WS-PHONE
464500990113               MOVE DL-REP-NAME        TO  WS-REP-NAME-1
464600000000               GO TO LR1-0030-0090.
464700000000
464800000000      *****
464900000000      *****    The account Dealer & Dealer Representative are different
465000000000      *****    than the dealer.  The following steps will be executed to
465100000000      *****    retrieve and format the Dealer & Dealer Representative
465200000000      *****    names and address lines.
465300000000      *****       1. Read the Dealer file for the name.
465400000000      *****       2. Read the Dealer Representative for the name, fax
465500000000      *****          and phone number.
465600000000      *****       3. Read the Dealer Representative Address for the
465700000000      *****          address lines.
465800000000      *****       4. If required, read the Institution Branch and
465900000000      *****          Institution Address for the address lines.
466000000000      *****       5. Format the address lines field by field, in case
466100000000      *****          one or more of the fields contain spaces.
466200000000      *****       6. Compress the address lines.
466300000000      *****
466400000000
466500000000           INITIALIZE MFADEAP.
466600000000           MOVE AD-DEALER              TO  DEALER-CODE
466700000000                                           OF DEALER-RECD.
466800000000           READ DEALER-FILE
466900000000               INVALID KEY
467000000000                   MOVE "DEALER NAME NOT ON FILE            "
467100000000                                       TO  DEALER-NAME
467200000000                                           OF DEALER-RECD.
467300000000
467400000000           MOVE DEALER-NAME
467500000000                OF DEALER-RECD         TO  WS-DEALER-NAME.
467600000000
467700000000           INITIALIZE MFADEAREP.
467800000000           MOVE AD-DEALER              TO  DEALER-CODE
467900000000                                           OF DEALER-REP-RECD.
468000000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
468100000000                                           OF DEALER-REP-RECD.
468200000000           READ DEALER-REP-FILE
468300000000               INVALID KEY
468400000000                   INITIALIZE MFADEAREP.
468500000000
468600000000           MOVE SPACES                 TO  WS-ADRTMP.
468700000000           STRING SALUTATION-CODE
468800000000                  OF DEALER-REP-RECD
468900000000                             DELIMITED BY  " "
469000000000                                     INTO  WS-ADRTMP.
469100000000
469200000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
469300000000               STRING FIRST-NAME
469400000000                      OF DEALER-REP-RECD
469500000000                             DELIMITED BY  "  "
469600000000                                     INTO  WS-ADRTMP
469700000000             ELSE
469800000000               STRING WS-ADRTMP
469900000000                             DELIMITED BY  "  ",
470000000000                      " "    DELIMITED BY  SIZE,
470100000000                      FIRST-NAME
470200000000                      OF DEALER-REP-RECD
470300000000                             DELIMITED BY  "  "
470400000000                                     INTO  WS-REP-NAME
470500000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
470600000000
470700000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
470800000000               STRING INITIALS
470900000000                      OF DEALER-REP-RECD
471000000000                             DELIMITED BY  "  "
471100000000                                     INTO  WS-ADRTMP
471200000000             ELSE
471300000000               STRING WS-ADRTMP
471400000000                             DELIMITED BY  "  ",
471500000000                      " "    DELIMITED BY  SIZE,
471600000000                      INITIALS
471700000000                      OF DEALER-REP-RECD
471800000000                             DELIMITED BY  "  "
471900000000                                     INTO  WS-REP-NAME
472000000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
472100000000
472200000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
472300000000               STRING LAST-NAME
472400000000                      OF DEALER-REP-RECD
472500000000                             DELIMITED BY  "  "
472600000000                                     INTO  WS-ADRTMP
472700000000             ELSE
472800000000               STRING WS-ADRTMP
472900000000                             DELIMITED BY  "  ",
473000000000                      " "    DELIMITED BY  SIZE,
473100000000                      LAST-NAME
473200000000                      OF DEALER-REP-RECD
473300000000                             DELIMITED BY  "  "
473400000000                                     INTO  WS-REP-NAME
473500000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
473600000000
473700000000           MOVE WS-ADRTMP              TO  WS-REP-NAME.
473800000000
473900000000           IF  ADDR-NO
474000000000               OF DEALER-REP-RECD
474100000000                              IS EQUAL TO  ZEROES
474200000000               GO TO LR1-0030-0010.
474300000000
474400000000           INITIALIZE MFADRADRP.
474500000000           MOVE ADDR-NO
474600000000                OF DEALER-REP-RECD     TO  ADDR-NO
474700000000                                           OF DEALER-REP-ADDRESS-RECD.
474800000000            READ DEALER-REP-ADDRESS
474900000000                INVALID KEY
475000000000                    GO TO LR1-0030-0010.
475100000000
475200000000            MOVE CORR MFADRADRP        TO  MFAINADDP.
475300000000            GO TO LR1-0030-0030.
475400000000
475500000000       LR1-0030-0010.
475600990927
475700000000           INITIALIZE MFAINSBRP.
475800000000           MOVE INSTITUTION-CODE
475900000000                OF DEALER-RECD         TO  INSTITUTION-CODE
476000000000                                           OF INSTITUTION-BRANCH-RECD.
476100000000           MOVE BRANCH-CODE
476200000000                OF DEALER-REP-RECD     TO  BRANCH-CODE
476300000000                                           OF INSTITUTION-BRANCH-RECD.
476400000000           READ INSTITUTION-BRANCH
476500000000               INVALID KEY
476600000000                   GO TO LR1-0030-0020.
476700000000
476800000000           INITIALIZE MFAINADDP.
476900000000           MOVE ADDR-NO
477000000000                OF INSTITUTION-BRANCH-RECD
477100000000                                       TO  ADDR-NO
477200000000                                           OF INSTITUTION-ADDRESS-RECD.
477300000000           READ INSTITUTION-ADDRESS
477400000000               INVALID KEY
477500000000                   GO TO LR1-0030-0020.
477600000000
477700000000           GO TO LR1-0030-0030.
477800000000
477900000000       LR1-0030-0020.
478000990923
478100000000           INITIALIZE MFAINSBRP.
478200000000           MOVE INSTITUTION-CODE
478300000000                OF DEALER-RECD         TO  INSTITUTION-CODE
478400000000                                           OF INSTITUTION-BRANCH-RECD.
478500000000           MOVE BRANCH-CODE
478600000000                OF DEALER-RECD         TO  BRANCH-CODE
478700000000                                           OF INSTITUTION-BRANCH-RECD.
478800000000           READ INSTITUTION-BRANCH
478900000000               INVALID KEY
479000000000                   INITIALIZE MFAINADDP
479100000000                   GO TO LR1-0030-0030.
479200000000
479300000000           INITIALIZE MFAINADDP.
479400000000           MOVE ADDR-NO
479500000000                OF INSTITUTION-BRANCH-RECD
479600000000                                       TO  ADDR-NO
479700000000                                           OF INSTITUTION-ADDRESS-RECD.
479800000000           READ INSTITUTION-ADDRESS
479900000000               INVALID KEY
480000000000                   INITIALIZE MFAINADDP
480100000000                   GO TO LR1-0030-0030.
480200000000
480300000000       LR1-0030-0030.
480400990927
480500000000           MOVE SPACES                 TO  WS-ADRTMP.
480600000000           STRING STREET-NO
480700000000               OF INSTITUTION-ADDRESS
480800000000                             DELIMITED BY  "  "
480900000000                                     INTO  WS-ADRTMP.
481000000000
481100000000           IF  PROVINCE-CODE
481200000000               OF INSTITUTION-ADDRESS-RECD
481300000000                              IS EQUAL TO  "QC"
481400000000               GO TO LR1-0030-0030-0010.
481500000000
481600000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
481700000000               STRING STREET-NAME
481800000000                      OF INSTITUTION-ADDRESS
481900000000                             DELIMITED BY  "  "
482000000000                                     INTO  WS-ADRTMP
482100000000             ELSE
482200000000               STRING WS-ADRTMP
482300000000                             DELIMITED BY  "  ",
482400000000                      " "    DELIMITED BY  SIZE,
482500000000                      STREET-NAME
482600000000                      OF INSTITUTION-ADDRESS
482700000000                             DELIMITED BY  "  "
482800000000                                     INTO  WS-BRANCH-ADDR1
482900000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
483000000000
483100000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
483200000000               STRING STREET-TYPE-CODE
483300000000                      OF INSTITUTION-ADDRESS
483400000000                             DELIMITED BY  "  "
483500000000                                     INTO  WS-ADRTMP
483600000000             ELSE
483700000000               STRING WS-ADRTMP
483800000000                             DELIMITED BY  "  ",
483900000000                      " "    DELIMITED BY  SIZE,
484000000000                      STREET-TYPE-CODE
484100000000                      OF INSTITUTION-ADDRESS
484200000000                             DELIMITED BY  "  "
484300000000                                     INTO  WS-BRANCH-ADDR1
484400000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
484500000000
484600000000           GO TO LR1-0030-0030-0020.
484700000000
484800000000       LR1-0030-0030-0010.
484900990927
485000000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
485100000000               STRING STREET-TYPE-CODE
485200000000                      OF INSTITUTION-ADDRESS-RECD
485300000000                             DELIMITED BY  "  "
485400000000                                     INTO  WS-ADRTMP
485500000000             ELSE
485600000000               STRING WS-ADRTMP
485700000000                             DELIMITED BY  "  ",
485800000000                      " "    DELIMITED BY  SIZE,
485900000000                      STREET-TYPE-CODE
486000000000                      OF INSTITUTION-ADDRESS-RECD
486100000000                             DELIMITED BY  "  "
486200000000                                     INTO  WS-BRANCH-ADDR1
486300000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
486400000000
486500000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
486600000000               STRING STREET-NAME
486700000000                      OF INSTITUTION-ADDRESS-RECD
486800000000                             DELIMITED BY  "  "
486900000000                                     INTO  WS-ADRTMP
487000000000             ELSE
487100000000               STRING WS-ADRTMP
487200000000                             DELIMITED BY  "  ",
487300000000                      " "    DELIMITED BY  SIZE,
487400000000                      STREET-NAME
487500000000                      OF INSTITUTION-ADDRESS-RECD
487600000000                             DELIMITED BY  "  "
487700000000                                     INTO  WS-BRANCH-ADDR1
487800000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
487900000000
488000000000       LR1-0030-0030-0020.
488100990927
488200000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
488300000000               STRING STREET-DIRCT-CODE
488400000000                      OF INSTITUTION-ADDRESS
488500000000                             DELIMITED BY  "  "
488600000000                                     INTO  WS-ADRTMP
488700000000             ELSE
488800000000               STRING WS-ADRTMP
488900000000                             DELIMITED BY  "  ",
489000000000                      " "    DELIMITED BY  SIZE,
489100000000                      STREET-DIRCT-CODE
489200990922                      OF INSTITUTION-ADDRESS
489300000000                             DELIMITED BY  "  "
489400000000                                     INTO  WS-BRANCH-ADDR1
489500000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
489600000000
489700000000           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR1.
489800000000
489900000000           MOVE MISC-FIRST-ADDR-LINE
490000000000                OF INSTITUTION-ADDRESS-RECD
490100000000                                       TO  WS-BRANCH-ADDR2.
490200000000           MOVE MISC-SECOND-ADDR-LINE
490300000000                OF INSTITUTION-ADDRESS-RECD
490400000000                                       TO  WS-BRANCH-ADDR3.
490500000000           MOVE CO-OR-ATTENTION
490600000000                OF INSTITUTION-ADDRESS-RECD
490700000000                                       TO  WS-BRANCH-ADDR4.
490800000000
490900980420980420     IF  WS-LDA-ATTN-LINE-PRT = "N"
491000980420980420         MOVE SPACES TO WS-BRANCH-ADDR4.
491100980420
491200000000           MOVE POSTAL-CODE
491300000000                OF INSTITUTION-ADDRESS TO  WS-POSTAL.
491400000000           MOVE SPACES                 TO  WS-POSTAL-OUT.
491500000000           IF  COUNTRY-CODE
491600000000               OF INSTITUTION-ADDRESS
491700000000                              IS EQUAL TO  "CAN"
491800000000               MOVE WS-POSTAL1         TO  WS-POSTAL-OUT1
491900000000               MOVE WS-POSTAL2         TO  WS-POSTAL-OUT2
492000000000             ELSE
492100000000               MOVE WS-POSTAL          TO  WS-POSTAL-OUT.
492200000000
492300000000           MOVE SPACES                 TO  WS-ADRTMP.
492400000000           STRING CITY
492500000000               OF INSTITUTION-ADDRESS
492600000000                             DELIMITED BY  "  "
492700000000                                     INTO  WS-ADRTMP.
492800000000
492900000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
493000000000               STRING PROVINCE-CODE
493100000000                      OF INSTITUTION-ADDRESS
493200000000                             DELIMITED BY  "  "
493300000000                                     INTO  WS-ADRTMP
493400000000               GO TO LR1-0030-0040.
493500000000
493600000000           IF  PROVINCE-CODE
493700000000               OF INSTITUTION-ADDRESS
493800000000                              IS EQUAL TO  SPACES
493900000000               GO TO LR1-0030-0040.
494000000000
494100000000           STRING WS-ADRTMP  DELIMITED BY  "  ",
494200000000                  ", "       DELIMITED BY  SIZE,
494300000000                  PROVINCE-CODE
494400000000                  OF INSTITUTION-ADDRESS
494500000000                             DELIMITED BY  "  "
494600000000                                     INTO  WS-BRANCH-ADDR5.
494700000000           MOVE WS-BRANCH-ADDR5        TO  WS-ADRTMP.
494800000000
494900000000       LR1-0030-0040.
495000000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
495100000000               STRING WS-POSTAL-OUT
495200000000                             DELIMITED BY  "  "
495300000000                                     INTO  WS-ADRTMP
495400000000             ELSE
495500000000               STRING WS-ADRTMP
495600000000                             DELIMITED BY  "  ",
495700000000                      "  "   DELIMITED BY  SIZE,
495800000000                      WS-POSTAL-OUT
495900000000                             DELIMITED BY  "  "
496000000000                                     INTO  WS-BRANCH-ADDR5
496100000000               MOVE WS-BRANCH-ADDR5    TO  WS-ADRTMP.
496200000000
496300000000           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR5.
496400000000
496500000000           IF  WS-BRANCH-ADDR1
496600000000                          IS NOT EQUAL TO  SPACES
496700000000               GO TO LR1-0030-0050.
496800000000
496900000000           IF  WS-BRANCH-ADDR2
497000000000                          IS NOT EQUAL TO  SPACES
497100000000               MOVE WS-BRANCH-ADDR2    TO  WS-BRANCH-ADDR1
497200000000               MOVE SPACES             TO  WS-BRANCH-ADDR2
497300000000               GO TO LR1-0030-0050.
497400000000
497500000000           IF  WS-BRANCH-ADDR3
497600000000                          IS NOT EQUAL TO  SPACES
497700000000               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR1
497800000000               MOVE SPACES             TO  WS-BRANCH-ADDR3
497900000000               GO TO LR1-0030-0050.
498000000000
498100000000           IF  WS-BRANCH-ADDR4
498200000000                          IS NOT EQUAL TO  SPACES
498300000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR1
498400000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
498500000000               GO TO LR1-0030-0050.
498600000000
498700000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR1.
498800000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
498900000000           GO TO LR1-0030-0080.
499000000000
499100000000       LR1-0030-0050.
499200000000           IF  WS-BRANCH-ADDR2
499300000000                          IS NOT EQUAL TO  SPACES
499400000000               GO TO LR1-0030-0060.
499500000000
499600000000           IF  WS-BRANCH-ADDR3
499700000000                          IS NOT EQUAL TO  SPACES
499800000000               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR2
499900000000               MOVE SPACES             TO  WS-BRANCH-ADDR3
500000000000               GO TO LR1-0030-0060.
500100000000
500200000000           IF  WS-BRANCH-ADDR4
500300000000                          IS NOT EQUAL TO  SPACES
500400000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR2
500500000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
500600000000               GO TO LR1-0030-0060.
500700000000
500800000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR2.
500900000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
501000000000           GO TO LR1-0030-0080.
501100000000
501200000000       LR1-0030-0060.
501300990922
501400000000           IF  WS-BRANCH-ADDR3
501500000000                          IS NOT EQUAL TO  SPACES
501600000000               GO TO LR1-0030-0070.
501700000000
501800000000           IF  WS-BRANCH-ADDR4
501900000000                          IS NOT EQUAL TO  SPACES
502000000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR3
502100000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
502200000000               GO TO LR1-0030-0070.
502300000000
502400000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR3.
502500000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
502600000000           GO TO LR1-0030-0080.
502700000000
502800000000       LR1-0030-0070.
502900000000           IF  WS-BRANCH-ADDR4
503000000000                          IS NOT EQUAL TO  SPACES
503100000000               GO TO LR1-0030-0080.
503200000000
503300000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR4.
503400000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
503500000000
503600000000       LR1-0030-0080.
503700000000           STRING WS-DEALER-NAME
503800000000                             DELIMITED BY  "  ",
503900000000                  " /"       DELIMITED BY  SIZE,
504000000000                  WS-REP-NAME
504100000000                             DELIMITED BY  SIZE
504200000000                                     INTO  WS-DEALER-REP-NAMES.
504300990113
504400990113           MOVE WS-REP-NAME            TO  WS-REP-NAME-1.
504500000000           MOVE FAX-AREA
504600000000                OF DEALER-REP-RECD     TO  WS-SH-FAX-01-03.
504700000000           MOVE FAX-PHONE
504800000000                OF DEALER-REP-RECD     TO  WS-SH-FAX-04-10.
504900000000
505000000000           MOVE AREA-CODE
505100000000                OF DEALER-REP-RECD     TO  WS-SH-PHONE-01-03.
505200000000           MOVE PHONE-NO
505300000000                OF DEALER-REP-RECD     TO  WS-SH-PHONE-04-10.
505400000000
505500000000       LR1-0030-0090.
505600981230
505700981230      *****
505800981230      *****    CND wants to suppress Merrill Lynch from Statements
505900981230      *****    Dealer Code is 9270.
506000981230      *****
506100981230
506200990115           IF WS-LDA-MGMT-COM-CDE IS NOT EQUAL TO "CND"
506300990115           OR AD-DEALER IS NOT EQUAL TO "9270"
506400990525              GO TO LR1-0030-0095.
506500990116
506600990116           IF IS-DEALER-ACCOUNT-NO IS EQUAL TO SPACES
506700990116              PERFORM GET-DEALER-ACCOUNT THRU GDA-EXIT.
506800990116
506900990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KKAAAA"
507000990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KKK9999"
507100990116                 MOVE SPACES TO WS-DEALER-REP-NAMES
507200990116                 MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
507300990116              END-IF
507400990116           END-IF.
507500990116
507600990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KHBAAA"
507700990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KH9999"
507800990116                 MOVE SPACES TO WS-DEALER-REP-NAMES
507900990116                 MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
508000990116              END-IF
508100990116           END-IF.
508200990116
508300990116           IF IS-DEALER-ACCOUNT-NO IS >= "0A9KAAA"
508400990116              IF IS-DEALER-ACCOUNT-NO IS <= "0A9K999"
508500990116                 MOVE SPACES TO WS-DEALER-REP-NAMES
508600990116                 MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
508700990116              END-IF
508800990116           END-IF.
508900990116
509000990116           IF IS-DEALER-ACCOUNT-NO IS >= "0A9LAAA"
509100990116              IF IS-DEALER-ACCOUNT-NO IS <= "0A9L999"
509200990115               MOVE SPACES TO WS-DEALER-REP-NAMES
509300990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
509400990116              END-IF
509500990116           END-IF.
509600990116
509700990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KFAAAA"
509800990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KF9999"
509900990116               MOVE SPACES TO WS-DEALER-REP-NAMES
510000990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
510100990116              END-IF
510200990116           END-IF.
510300990104
510400990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KGAAAA"
510500990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KG9999"
510600990116               MOVE SPACES TO WS-DEALER-REP-NAMES
510700990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
510800990116              END-IF
510900990116           END-IF.
511000990116
511100990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KJAAAA"
511200990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KJ9999"
511300990116               MOVE SPACES TO WS-DEALER-REP-NAMES
511400990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
511500990116              END-IF
511600990116           END-IF.
511700990116
511800990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KNAAAA"
511900990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KN9999"
512000990116               MOVE SPACES TO WS-DEALER-REP-NAMES
512100990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
512200990116              END-IF
512300990116           END-IF.
512400990116
512500990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KMAAAA"
512600990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KM9999"
512700990116               MOVE SPACES TO WS-DEALER-REP-NAMES
512800990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
512900990116              END-IF
513000990116           END-IF.
513100990116
513200990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KQAAAA"
513300990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KQ9999"
513400990116               MOVE SPACES TO WS-DEALER-REP-NAMES
513500990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
513600990116              END-IF
513700990116           END-IF.
513800990116
513900981230       LR1-0030-0095.
514000981230
514100000000      *****
514200000000      *****    Format the account type description.
514300000000      *****
514400000000
514500000000           MOVE AD-GROUP               TO  WS-ACTTYPGRP.
514600000000           MOVE AD-ACCOUNT-TYPE        TO  WS-ACTTYPIN-PFX.
514700000000           MOVE AD-LOCKED-IN           TO  WS-ACTTYPLCK.
514800000000           MOVE AD-LANGUAGE            TO  WS-ACTTYPLNG.
514900000000           MOVE AD-NOMINEE             TO  WS-ACTTYPNOM.
515000190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  WS-ACTTYPSHT.
515100190619183449     MOVE Lc-Acct-Type-Code      TO  WS-ACTTYPSHT.
515200000000           MOVE AD-SPOUSAL             TO  WS-ACTTYPSPS.
515300990923
515400990923           IF WS-STMTVRSN              IS NOT EQUAL TO 6
515500990923              PERFORM FORMAT-ACTTYP-DESC THRU FAD-EXIT
515600990923           END-IF
515700990923
515800000000      *****
515900000000      *****    Initialize the statement for the new account.
516000000000      *****
516100000000
516200000000           MOVE SPACES                 TO  WS-STATEMENT-PAGE.
516300000000           MOVE 1                      TO  WS-SH-PGENO.
516400091123
516500000000      *****
516600000000      *****    Format statement heading line 1.
516700000000      *****
516800000000
516900000000           MOVE SPACES                 TO  STMT-HDG-01.
517000000000
517100091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
517200091123  |            MOVE WS-ACTTYPOUT       TO  SH01VA-ACCOUNT-TYPE
517300091207  |            MOVE WS-SH01-LIT1 (SL)  TO  SH01VA-LIT1
517400091123  |            MOVE WS-SH-START-DATE   TO  SH01VA-START-DATE
517500091207  |            MOVE WS-SH01-LIT2 (SL)  TO  SH01VA-LIT2
517600091123  |            MOVE WS-SH-END-DATE     TO  SH01VA-END-DATE
517700091123  |            GO TO LR1-0030-0100
517800091123R73890     END-IF
517900991019
518000991019           MOVE WS-ACTTYPOUT           TO  SH01-ACCOUNT-TYPE.
518100991019           MOVE WS-SH01-LIT1 (SL)      TO  SH01-LIT1.
518200991019           MOVE WS-SH-START-DATE       TO  SH01-START-DATE.
518300991019           MOVE WS-SH01-LIT2 (SL)      TO  SH01-LIT2.
518400991019           MOVE WS-SH-END-DATE         TO  SH01-END-DATE.
518500991019
518600000000       LR1-0030-0100.
518700990923
518800000000           SET SPL                     TO  WS-SPL-HDGFST.
518900000000           MOVE STMT-HDG-01            TO  WS-STMTPGE-LNE (SPL).
519000000000
519100000000      *****
519200000000      *****    Format statement heading line 2.
519300000000      *****
519400000000
519500000000           MOVE II-SIN-NO              TO  WS-SIN-NO.
519600000000           MOVE SPACES                 TO  WS-SH02-SIN-NO.
519700980206
519800980417980417     IF WS-LDA-SIN NOT = "N"
519900980206             MOVE WS-SIN-NO-1-3          TO  SH02-SIN-NO-1-3
520000980206             MOVE "-"                    TO  SH02-SIN-NO-LITDSH1
520100980206             MOVE WS-SIN-NO-4-6          TO  SH02-SIN-NO-4-6
520200980206             MOVE "-"                    TO  SH02-SIN-NO-LITDSH2
520300980206             MOVE WS-SIN-NO-7-9          TO  SH02-SIN-NO-7-9.
520400000000
520500020919
520600000000           MOVE SPACES                 TO  STMT-HDG-02.
520700000000
520800091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
520900091123  |          IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
521000091123  |            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02VA-ACCOUNT
521100091123  |          ELSE
521200091123  |            MOVE AD-ACCOUNT         TO  SH02VA-ACCOUNT
521300091123  |          END-IF
521400091123  |            MOVE WS-SH02-SIN-NO     TO  SH02VA-SIN-NO
521500091123  |            MOVE AD-DEALER          TO  SH02VA-DEALER
521600091123  |            MOVE "-"                TO  SH02VA-LITDSH
521700091123  |            MOVE AD-SALESREP        TO  SH02VA-SALESREP
521800091123  |            MOVE WS-SH-END-DATE     TO  SH02VA-END-DATE
521900091123  |            MOVE AD-SPOUSAL-CONTRIB TO  SH02VA-SPOUSAL-CONTRIB
522000091123  |            MOVE WS-REG-CTB-CUR     TO  SH02VA-REG-CTB-CUR
522100091123  |            MOVE WS-REG-CTB-CUR-SIGN
522200091123  |                                    TO  SH02VA-REG-CTB-CUR-SIGN
522300091123  |            MOVE WS-SH-PGENO        TO  SH02VA-PGENO
522400091127  |            SET SPL              UP BY  3
522500091123  |            GO TO LR1-0030-0110
522600091123R73890     END-IF
522700091123
522800020921R13756     IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
522900020921  ¦          MOVE WS-MSTR-ACCOUNT-NO  TO  SH02-ACCOUNT
523000020613R13756     ELSE
523100020613             MOVE AD-ACCOUNT             TO  SH02-ACCOUNT
523200020613R13756     END-IF
523300000000           MOVE WS-SH02-SIN-NO         TO  SH02-SIN-NO.
523400000000           MOVE AD-DEALER              TO  SH02-DEALER.
523500000000           MOVE "-"                    TO  SH02-LITDSH.
523600000000           MOVE AD-SALESREP            TO  SH02-SALESREP.
523700000000           MOVE WS-SH-END-DATE         TO  SH02-END-DATE.
523800000000           MOVE AD-SPOUSAL-CONTRIB     TO  SH02-SPOUSAL-CONTRIB.
523900000000           MOVE WS-REG-CTB-CUR         TO  SH02-REG-CTB-CUR.
524000000000           MOVE WS-REG-CTB-CUR-SIGN    TO  SH02-REG-CTB-CUR-SIGN.
524100000000           MOVE WS-SH-PGENO            TO  SH02-PGENO.
524200091127           SET SPL                  UP BY  4.
524300000000
524400000000       LR1-0030-0110.
524500990927
524600990927           IF WS-STMTVRSN              IS NOT EQUAL TO  6
524700990927              MOVE STMT-HDG-02            TO  WS-STMTPGE-LNE (SPL)
524800990927           END-IF
524900990927
525000990927           SET WS-SPL-SH02             TO  SPL.
525100000000
525200000000      *****
525300000000      *****    Format statement heading line 3.
525400000000      *****
525500000000
525600000000           MOVE SPACES                 TO  STMT-HDG-03.
525700000000
525800091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
525900091123  |            MOVE II-INVESTOR-NAME   TO  SH03VA-INVESTOR-NAME
526000091123  |            MOVE WS-DEALER-REP-NAMES
526100091123  |                                    TO  SH03VA-DEALER-REP-NAMES
526200091123  |            IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
526300091123  |               MOVE SPACES          TO  SH03VA-DEALER-REP-NAMES
526400091123  |               MOVE WS-REP-NAME-1   TO  SH03VA-DEALER-REP-NAMES
526500091123  |            END-IF
526600091127  |            SET SPL              UP BY  4
526700091123R73890         GO TO LR1-0030-0120.
526800091123
526900990923           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
527000990923
527100990923           IF  II-INVESTOR-ADDRESS1    IS EQUAL TO  SPACES
527200990923           AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
527300990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
527400990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
527500990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
527600990923               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
527700990923               MOVE "Y"                TO  WS-PSTCDEPRT-SW
527800990923           ELSE
527900990923               MOVE II-INVESTOR-ADDRESS1
528000990923                                       TO  WS-SH04-INVESTOR-ADDRESS1
528100990923           END-IF
528200990923
528300000000           MOVE II-INVESTOR-NAME       TO  SH03-INVESTOR-NAME.
528400000000           MOVE WS-DEALER-REP-NAMES    TO  SH03-DEALER-REP-NAMES.
528500991014           IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
528600991014              MOVE SPACES          TO  SH03-DEALER-REP-NAMES
528700991014              MOVE WS-REP-NAME-1   TO  SH03-DEALER-REP-NAMES
528800991014           END-IF.
528900991014
529000000000           SET SPL                  UP BY  3.
529100000000
529200000000       LR1-0030-0120.
529300990927
529400000000           MOVE STMT-HDG-03            TO  WS-STMTPGE-LNE (SPL).
529500990927
529600000000      *****
529700000000      *****    Format statement heading line 4.
529800000000      *****
529900000000
530000000000           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
530100990926
530200000000           IF  II-INVESTOR-ADDRESS1
530300000000                              IS EQUAL TO  SPACES
530400000000           AND II-INVESTOR-ADDRESS2
530500000000                              IS EQUAL TO  SPACES
530600000000           AND II-INVESTOR-ADDRESS3
530700000000                              IS EQUAL TO  SPACES
530800000000           AND II-INVESTOR-ADDRESS4
530900000000                              IS EQUAL TO  SPACES
531000000000           AND II-INVESTOR-ADDRESS5
531100000000                              IS EQUAL TO  SPACES
531200000000               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
531300000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
531400990922           ELSE
531500000000               MOVE II-INVESTOR-ADDRESS1
531600990922                                       TO  WS-SH04-INVESTOR-ADDRESS1
531700990922           END-IF
531800990922
531900980417980417     IF  WS-LDA-BRANCH-ADDRESS
532000980417980417                        IS EQUAL TO  "N"
532100000000               MOVE SPACES             TO  WS-BRANCH-ADDR1.
532200000000
532300000000           MOVE SPACES                 TO  STMT-HDG-04.
532400000000
532500091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
532600091123  |            MOVE WS-SH04-INVESTOR-ADDRESS1
532700091123  |                                    TO  SH04VA-INVESTOR-ADDRESS1
532800091123  |            MOVE WS-BRANCH-ADDR1    TO  SH04VA-BRANCH-ADDR1
532900091123  |            GO TO LR1-0030-0130
533000091123R73890     END-IF
533100091123
533200010314RF9554     IF  WS-STMTVRSN    IS EQUAL TO  6
533300010314
533400010314               MOVE SPACES         TO  WS-SH05-INVESTOR-ADDRESS2
533500990923
533600010314               IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
533700010314               AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
533800010314               AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
533900010314               AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
534000010314               AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
534100010314                   MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
534200010314                   MOVE "Y"                TO  WS-PSTCDEPRT-SW
534300010314               ELSE
534400010314                   MOVE II-INVESTOR-ADDRESS2
534500010314                                 TO  WS-SH05-INVESTOR-ADDRESS2
534600010314               END-IF
534700000000
534800000000           MOVE WS-SH04-INVESTOR-ADDRESS1
534900000000                                       TO  SH04-INVESTOR-ADDRESS1.
535000000000           MOVE WS-BRANCH-ADDR1        TO  SH04-BRANCH-ADDR1.
535100000000
535200000000       LR1-0030-0130.
535300990923
535400990923           SET SPL                     UP BY  1.
535500000000           MOVE STMT-HDG-04            TO  WS-STMTPGE-LNE (SPL).
535600000000
535700000000      *****
535800000000      *****    Format statement heading line 5.
535900000000      *****
536000000000
536100000000           MOVE SPACES                 TO  WS-SH05-INVESTOR-ADDRESS2.
536200990926
536300000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
536400000000           AND II-INVESTOR-ADDRESS2
536500000000                              IS EQUAL TO  SPACES
536600000000           AND II-INVESTOR-ADDRESS3
536700000000                              IS EQUAL TO  SPACES
536800000000           AND II-INVESTOR-ADDRESS4
536900000000                              IS EQUAL TO  SPACES
537000000000           AND II-INVESTOR-ADDRESS5
537100000000                              IS EQUAL TO  SPACES
537200000000               MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
537300000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
537400990922           ELSE
537500000000               MOVE II-INVESTOR-ADDRESS2
537600990922                                       TO  WS-SH05-INVESTOR-ADDRESS2
537700990922           END-IF
537800990922
537900990923980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO  "N"
538000990923               MOVE SPACES             TO  WS-BRANCH-ADDR2
538100990923           END-IF
538200990923
538300000000           MOVE SPACES                 TO  STMT-HDG-05.
538400000000
538500091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
538600091123  |            MOVE WS-SH05-INVESTOR-ADDRESS2
538700091123  |                                    TO  SH05VA-INVESTOR-ADDRESS2
538800091123  |            MOVE WS-BRANCH-ADDR2    TO  SH05VA-BRANCH-ADDR2
538900091123  |            GO TO LR1-0030-0140
539000091123R73890     END-IF
539100091123
539200000000           MOVE WS-SH05-INVESTOR-ADDRESS2
539300000000                                       TO  SH05-INVESTOR-ADDRESS2.
539400000000           MOVE WS-BRANCH-ADDR2        TO  SH05-BRANCH-ADDR2.
539500000000
539600000000       LR1-0030-0140.
539700990926
539800990926           SET SPL                     UP BY  1.
539900000000           MOVE STMT-HDG-05            TO  WS-STMTPGE-LNE (SPL).
540000000000
540100000000      *****
540200000000      *****    Format statement heading line 6.
540300000000      *****
540400000000
540500000000           MOVE SPACES                 TO  WS-SH06-INVESTOR-ADDRESS3.
540600990926
540700000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
540800990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
540900990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
541000990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
541100000000               MOVE II-POSTAL-CODE     TO  SH06-POSTAL-CODE
541200000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
541300990923           ELSE
541400000000               MOVE II-INVESTOR-ADDRESS3
541500990923                                       TO  WS-SH06-INVESTOR-ADDRESS3
541600990923           END-IF
541700990923
541800980417980417     IF  WS-LDA-BRANCH-ADDRESS
541900980417980417                        IS EQUAL TO  "N"
542000000000               MOVE SPACES             TO  WS-BRANCH-ADDR3.
542100000000
542200000000           MOVE SPACES                 TO  STMT-HDG-06.
542300000000
542400091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
542500091123  |            MOVE WS-SH06-INVESTOR-ADDRESS3
542600091123  |                                    TO  SH06VA-INVESTOR-ADDRESS3
542700091123  |            MOVE WS-BRANCH-ADDR3    TO  SH06VA-BRANCH-ADDR3
542800091123  |            GO TO LR1-0030-0150
542900091123R73890     END-IF
543000091123
543100000000           MOVE WS-SH06-INVESTOR-ADDRESS3
543200000000                                       TO  SH06-INVESTOR-ADDRESS3.
543300000000           MOVE WS-BRANCH-ADDR3        TO  SH06-BRANCH-ADDR3.
543400000000
543500000000       LR1-0030-0150.
543600990923
543700990923           SET SPL                     UP BY  1.
543800000000           MOVE STMT-HDG-06            TO  WS-STMTPGE-LNE (SPL).
543900000000
544000000000      *****
544100000000      *****    Format statement heading line 7.
544200000000      *****
544300000000
544400000000           MOVE SPACES                 TO  WS-SH07-INVESTOR-ADDRESS4.
544500000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
544600990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
544700990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
544800000000               MOVE II-POSTAL-CODE     TO  SH07-POSTAL-CODE
544900000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
545000990923           ELSE
545100000000               MOVE II-INVESTOR-ADDRESS4
545200990923                                       TO  WS-SH07-INVESTOR-ADDRESS4
545300990923           END-IF
545400990923
545500990923980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO "N"
545600990923               MOVE SPACES             TO  WS-BRANCH-ADDR4
545700990923           END-IF
545800990923
545900000000           MOVE SPACES                 TO  STMT-HDG-07.
546000000000
546100091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
546200091123  |            MOVE WS-SH07-INVESTOR-ADDRESS4
546300091123  |                                    TO  SH07VA-INVESTOR-ADDRESS4
546400091123  |            MOVE WS-BRANCH-ADDR4    TO  SH07VA-BRANCH-ADDR4
546500091123  |            GO TO LR1-0030-0160
546600091123R73890     END-IF
546700091123
546800000000           MOVE WS-SH07-INVESTOR-ADDRESS4
546900000000                                       TO  SH07-INVESTOR-ADDRESS4.
547000000000           MOVE WS-BRANCH-ADDR4        TO  SH07-BRANCH-ADDR4.
547100000000
547200000000       LR1-0030-0160.
547300990922
547400990926           SET SPL                     UP BY  1.
547500000000           MOVE STMT-HDG-07            TO  WS-STMTPGE-LNE (SPL).
547600000000
547700000000      *****
547800000000      *****    Format statement heading line 8.
547900000000      *****
548000000000
548100000000           MOVE SPACES                 TO  WS-SH08-INVESTOR-ADDRESS5.
548200990926
548300000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
548400990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
548500000000               MOVE II-POSTAL-CODE     TO  SH08-POSTAL-CODE
548600000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
548700990923           ELSE
548800000000               MOVE II-INVESTOR-ADDRESS5
548900990923                                       TO  WS-SH08-INVESTOR-ADDRESS5
549000990923           END-IF
549100990923
549200000000           MOVE SPACES                 TO  STMT-HDG-08.
549300000000
549400091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
549500091123  |            MOVE WS-SH08-INVESTOR-ADDRESS5
549600091123  |                                    TO  SH08VA-INVESTOR-ADDRESS5
549700091123  |            GO TO LR1-0030-0170
549800091123R73890     END-IF
549900091123
550000000000           MOVE WS-SH08-INVESTOR-ADDRESS5
550100000000                                       TO  SH08-INVESTOR-ADDRESS5.
550200000000
550300000000       LR1-0030-0170.
550400990923
550500990923           SET SPL                     UP BY  1.
550600000000           MOVE STMT-HDG-08            TO  WS-STMTPGE-LNE (SPL).
550700000000
550800000000      *****
550900000000      *****    Format statement heading line 9.
551000000000      *****
551100000000
551200990926           MOVE SPACES                 TO  WS-SH09-POSTAL-CODE.
551300990923
551400000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
551500990923               MOVE II-POSTAL-CODE     TO  WS-SH09-POSTAL-CODE
551600990923           END-IF
551700990923
551800000000           MOVE SPACES                 TO  STMT-HDG-09.
551900990926
552000091123R73890     IF  WS-STMTVRSN    IS EQUAL   TO  "A"
552100091123  |            MOVE WS-SH09-POSTAL-CODE  TO  SH09VA-POSTAL-CODE
552200091123  |            GO TO LR1-0030-0180
552300091123R73890     END-IF
552400091123
552500990926           MOVE WS-SH09-POSTAL-CODE      TO  SH09-POSTAL-CODE.
552600000000
552700000000       LR1-0030-0180.
552800990923
552900990927
553000990925           SET SPL                     UP BY  1
553100000000           MOVE STMT-HDG-09            TO  WS-STMTPGE-LNE (SPL).
553200990925
553300000000
553400000000      *****
553500000000      *****    Format statement heading line 10.
553600000000      *****
553700000000
553800000000           MOVE SPACES                 TO  WS-SH10-LIT1X.
553900990922
554000000000           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
554100091207               MOVE WS-SH10-LIT1 (SL)  TO  WS-SH10-LIT1X
554200990922           END-IF
554300990922
554400000000           MOVE SPACES                 TO  WS-SH10-PHONE.
554500990922
554600000000           IF  WS-PHONE   IS NOT EQUAL TO  ZEROES
554700000000               MOVE "("                TO  SH10-PHONE-LITLB
554800000000               MOVE WS-SH-PHONE-01-03  TO  SH10-PHONE-01-03
554900000000               MOVE ")"                TO  SH10-PHONE-LITRB
555000000000               MOVE WS-SH-PHONE-04-06  TO  SH10-PHONE-04-06
555100000000               MOVE "-"                TO  SH10-PHONE-LITDSH
555200990922               MOVE WS-SH-PHONE-07-10  TO  SH10-PHONE-07-10
555300990922           END-IF
555400990922
555500000000           MOVE SPACES                 TO  STMT-HDG-10.
555600000000
555700091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
555800091123  |            MOVE WS-SH10-LIT1X      TO  SH10VA-LIT1
555900091123  |            MOVE WS-SH10-PHONE      TO  SH10VA-PHONE
556000091123  |            GO TO LR1-0030-0190
556100091123R73890     END-IF
556200091123
556300000000           MOVE WS-SH10-LIT1X          TO  SH10-LIT1.
556400000000           MOVE WS-SH10-PHONE          TO  SH10-PHONE.
556500000000
556600000000       LR1-0030-0190.
556700990925
556800091207R73890*    SET SPL                     UP BY  3.
556900091207           MOVE STMT-HDG-10            TO  WS-STMTPGE-LNE (SPL).
557000000000
557100000000      *****
557200000000      *****    Format statement heading line 11.
557300000000      *****
557400000000
557500000000           MOVE SPACES                 TO  WS-SH11-NOMINEE.
557600990922
557700991013           IF  WS-NOMINEE              IS NOT EQUAL TO  SPACES
557800990922               MOVE WS-NOMINEE         TO  WS-SH11-NOMINEE
557900990922           END-IF
558000990922
558100000000           MOVE SPACES                 TO  WS-SH11-FAX.
558200990927
558300000000           IF  WS-FAX     IS NOT EQUAL TO  ZEROES
558400000000               MOVE "("                TO  SH11-FAX-LITLB
558500000000               MOVE WS-SH-FAX-01-03    TO  SH11-FAX-01-03
558600000000               MOVE ")"                TO  SH11-FAX-LITRB
558700000000               MOVE WS-SH-FAX-04-06    TO  SH11-FAX-04-06
558800000000               MOVE "-"                TO  SH11-FAX-LITDSH
558900990927               MOVE WS-SH-FAX-07-10    TO  SH11-FAX-07-10
559000990927           END-IF
559100990927
559200000000           MOVE SPACES                 TO  STMT-HDG-11.
559300000000
559400091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
559500091123  |            MOVE WS-SH11-NOMINEE    TO  SH11VA-NOMINEE
559600091123  |            MOVE WS-SH11-FAX        TO  SH11VA-FAX
559700091123  |            GO TO LR1-0030-0200
559800091123R73890     END-IF
559900991022
560000000000           MOVE WS-SH11-NOMINEE        TO  SH11-NOMINEE.
560100000000           MOVE WS-SH11-FAX            TO  SH11-FAX.
560200000000
560300000000       LR1-0030-0200.
560400990923
560500091208R73890     SET SPL                   UP BY  1.
560600000000           MOVE STMT-HDG-11            TO  WS-STMTPGE-LNE (SPL).
560700990925
560800991019           IF WS-STMTVRSN                   IS EQUAL TO  7
560900990925              IF WS-LDA-PRINT-HEADING IS NOT EQUAL TO "Y"
561000990925                 GO TO LR1-0030-0300
561100990925              ELSE
561200991013                 SET SPL                     UP BY  1
561300991019                 MOVE WS-SHL07-1 (SHL)       TO  WS-STMTPGE-LNE (SPL)
561400990925                 SET SPL                     UP BY  1
561500991019                 MOVE WS-SHL07-2 (SHL)       TO  WS-STMTPGE-LNE (SPL)
561600990925                 SET SPL                     UP BY  1
561700991019                 MOVE WS-SHL07-3 (SHL)       TO  WS-STMTPGE-LNE (SPL)
561800990925                 SET SPL                     UP BY  1
561900991019                 MOVE WS-SHL07-4 (SHL)       TO  WS-STMTPGE-LNE (SPL)
562000990925              END-IF
562100991019              GO TO LR1-0030-0300
562200991019           END-IF
562300990925
562400991020           IF WS-LDA-PRINT-HEADING           IS NOT EQUAL TO "Y"
562500991020              GO TO LR1-0030-0300
562600991020           END-IF
562700991020
562800991020           SET SPL                     UP BY  1.
562900991020           MOVE WS-SHL01 (SHL)         TO  WS-STMTPGE-LNE (SPL).
563000991020           SET SPL                     UP BY  1.
563100991020           MOVE WS-SHL02 (SHL)         TO  WS-STMTPGE-LNE (SPL).
563200991020           SET SPL                     UP BY  1.
563300991020           MOVE WS-SHL03 (SHL)         TO  WS-STMTPGE-LNE (SPL).
563400991020           SET SPL                     UP BY  1.
563500991020           MOVE WS-SHL04 (SHL)         TO  WS-STMTPGE-LNE (SPL).
563600991019
563700990624       LR1-0030-0300.
563800990925
563900000000      *****
564000000000      *****    Format the messages.  Initially, attempt to format the
564100000000      *****    statement messages using the Account Type Code.  If none
564200000000      *****    are found, use an Account Type Code of spaces.
564300021213R15028*****
564400021213  ¦   *****RFS15028 implements the following logic:
564500021213  ¦   *****    For new investor: get new investor's info. If not found
564600021213  ¦   *****    using existing info(new-investor = 'N'). If not found,
564700021213  ¦   *****    using All info(new-investor = 'A'). If not found, using
564800021213  ¦   *****    Account Type Code of spaces.
564900021213R15028*****
565000000000
565100000000           MOVE SPACES                 TO  WS-STATEMENT-PAGE-MESSAGE.
565200000000           MOVE AD-FUND-NO             TO  WS-FUND-NO.
565300000000
565400021213R15028 LR1-0030-0400.
565500021213  ¦
565600021213  ¦        MOVE LOW-VALUES             TO  STMT-MSG-RECD.
565700190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
565800190619183449     MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
565900021213  ¦                                        OF STMT-MSG-RECD.
566000021213  ¦        MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
566100021213  ¦                                        OF STMT-MSG-RECD.
566200021213  ¦        MOVE II-NEW-INVESTOR-FLAG   TO  WS-NEW-INVESTOR-FLAG.
566300021213  ¦        MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
566400021213  ¦                                        OF STMT-MSG-RECD.
566500021213  ¦        MOVE 1                      TO  LINE-NO
566600021213  ¦                                        OF STMT-MSG-RECD.
566700021213  ¦   *******
566800021213  ¦   *******   Direct read to determine if the specific investor(New or
566900021213  ¦   *******   not) infor exist or not.
567000021213  ¦   *******
567100021213  ¦
567200021213  ¦        READ STMT-MSG-FILE
567300021213  ¦          INVALID KEY
567400021213  ¦            IF WS-NEW-INVESTOR-FLAG  = "Y"
567500021213  ¦               MOVE "N"               TO WS-NEW-INVESTOR-FLAG
567600021213  ¦               MOVE WS-NEW-INVESTOR-FLAG  TO  NEW-INVESTOR
567700021213  ¦                                          OF STMT-MSG-RECD
567800021213  ¦               READ STMT-MSG-FILE
567900021213  ¦                   INVALID KEY
568000021213  ¦                   MOVE "A"           TO WS-NEW-INVESTOR-FLAG
568100021213  ¦               END-READ
568200021213  ¦            ELSE
568300021213  ¦               MOVE "A"               TO WS-NEW-INVESTOR-FLAG
568400021213  ¦            END-IF
568500021213R15028     END-READ.
568600021213
568700000000       LR1-0040.
568800990923
568900000000           MOVE LOW-VALUES             TO  STMT-MSG-RECD.
569000190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
569100190619183449     MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
569200000000                                           OF STMT-MSG-RECD.
569300000000           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
569400000000                                           OF STMT-MSG-RECD.
569500021213R15028     MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
569600021213  ¦                                        OF STMT-MSG-RECD.
569700021213  ¦        MOVE ZEROES                 TO  LINE-NO
569800021213R15028                                     OF STMT-MSG-RECD.
569900021213
570000000000           START STMT-MSG-FILE
570100000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
570200000000               INVALID KEY
570300990922                   GO TO LR1-0070
570400990922           END-START.
570500990922
570600000000       LR1-0050.
570700990922
570800000000           READ STMT-MSG-FILE NEXT     AT  END
570900990922               GO TO LR1-0070
571000990922           END-READ
571100990922
571200000000           IF  LINE-NO    IS GREATER THAN  WS-SPML-MAX
571300990922           OR  LANGUAGE-CODE OF STMT-MSG-RECD
571400000000                          IS NOT EQUAL TO  AD-LANGUAGE
571500990922           OR  ACCOUNT-TYPE-CODE OF STMT-MSG-RECD
571600190619183449*                   IS NOT EQUAL TO  AD-ACCOUNT-TYPE-SHORT
571700190619183449                    IS NOT EQUAL TO  Lc-Acct-Type-Code
571800021213R15028     OR  NEW-INVESTOR OF STMT-MSG-RECD  IS NOT EQUAL
571900021213R15028                         TO  WS-NEW-INVESTOR-FLAG
572000990922               GO TO LR1-0070
572100990922           END-IF
572200990922
572300000000           IF  LINE-NO   NOT GREATER THAN  ZEROES
572400990922               GO TO LR1-0060
572500990922           END-IF
572600000000
572700000000           MOVE SPACES                 TO  STMT-MSG-01.
572800000000           MOVE STATEMENT-MESSAGE      TO  SM01-STATEMENT-MESSAGE.
572900000000
573000000000           SET SPML                    TO  LINE-NO.
573100000000           MOVE STMT-MSG-01            TO  WS-STMTPGE-MSGLNE (SPML).
573200000000
573300000000       LR1-0060.
573400990922
573500000000           GO TO LR1-0050.
573600000000
573700000000       LR1-0070.
573800000000
573900000000      *****
574000000000      *****    If no statement messages were formatted and the Account
574100000000      *****    Type Code being used was not spaces, attempt to format
574200000000      *****    statement messages using an Account Type Code of spaces.
574300000000      *****
574400000000
574500990922           IF  WS-STATEMENT-PAGE-MESSAGE  IS EQUAL TO  SPACES
574600990922           AND AD-ACCOUNT-TYPE-SHORT      IS NOT EQUAL TO  SPACES
574700990925               MOVE SPACES                TO  AD-ACCOUNT-TYPE-SHORT
574800021213R15028*        GO TO LR1-0040
574900021213R15028         GO TO LR1-0030-0400
575000990922           END-IF
575100990922
575200091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
575300091123  |            MOVE WS-STATEMENT-PAGE-MESSAGE
575400091123  |                                    TO  WS-STMTPGE-MSG-VA
575500091123  |            GO TO LR1-0070-0000-0010
575600091123R73890     END-IF
575700091123
575800000000           MOVE WS-STATEMENT-PAGE-MESSAGE
575900000000                                       TO  WS-STMTPGE-MSG.
576000000000       LR1-0070-0000-0010.
576100000000
576200000000      *****
576300000000      *****    Position at first detail line.
576400000000      *****
576500000000
576600000000           SET SPL                     TO  WS-SPL-DTLFST.
576700000000
576800000000      *****
576900000000      *****    Determine if the client statement copy is to be
577000000000      *****    suppressed.  This is done depending on whether or not the
577100000000      *****    account being processed is a nominee account.
577200000000      *****
577300000000
577400000000           MOVE "N"                    TO  WS-CLTSPR-SW.
577500000000
577600000000           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
577700990923               GO TO LR1-0070-0030
577800990923           END-IF
577900000000
578000000000      *****
578100000000      *****    Processing a non-nominee account.  Initially read through
578200000000      *****    the Dealer Mailing Instruction.  If the form CLTSTSPR is
578300000000      *****    found, the client copy statement is to be suppressed.
578400000000      *****
578500000000
578600000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
578700000000           MOVE AD-DEALER              TO  DEALER-CODE
578800000000                                           OF DEALER-MAILING-INSTR-RECD.
578900000000           MOVE "CLTSTSPR  "           TO  FORM-CODE
579000000000                                           OF DEALER-MAILING-INSTR-RECD.
579100000000           START DEALER-MAILING-INSTR
579200000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
579300000000               INVALID KEY
579400990923                   GO TO LR1-0070-0010
579500990923           END-START
579600990923
579700000000           READ DEALER-MAILING-INSTR NEXT
579800990923             AT  END
579900990923               GO TO LR1-0070-0010
580000990923           END-READ
580100990923
580200990923           IF  FORM-CODE OF DEALER-MAILING-INSTR-RECD
580300990923                IS NOT EQUAL TO  "CLTSTSPR  "
580400990923           OR  DEALER-CODE OF DEALER-MAILING-INSTR-RECD
580500990923                IS NOT EQUAL TO  AD-DEALER
580600990923               GO TO LR1-0070-0010
580700990923           END-IF
580800990923
580900000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
581000000000           GO TO LR1-0070-0020.
581100000000
581200000000       LR1-0070-0010.
581300000000
581400000000      *****
581500000000      *****    Processing a non-nominee account.  The client statement
581600000000      *****    copy is to be printed, according to the Dealer Mailing
581700000000      *****    Instruction.  Now read through the Dealer Representative
581800000000      *****    Mailing Instruction.  If the form CLTSTSPR is found, the
581900000000      *****    client statement copy is to be suppressed.
582000000000      *****
582100000000
582200000000           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
582300000000           MOVE AD-DEALER              TO  DEALER-CODE
582400000000                                           OF DLRREP-MAILING-INSTR-RECD.
582500000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
582600000000                                           OF DLRREP-MAILING-INSTR-RECD.
582700000000           MOVE "CLTSTSPR  "           TO  FORM-CODE
582800000000                                           OF DLRREP-MAILING-INSTR-RECD.
582900000000           START DEALER-REP-MAILING-INSTR
583000000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
583100000000               INVALID KEY
583200000000                   GO TO LR1-0070-0020.
583300000000
583400000000           READ DEALER-REP-MAILING-INSTR NEXT
583500000000                                       AT  END
583600000000               GO TO LR1-0070-0020.
583700000000
583800000000           IF  FORM-CODE
583900000000               OF DLRREP-MAILING-INSTR-RECD
584000000000                          IS NOT EQUAL TO  "CLTSTSPR  "
584100000000           OR  DEALER-REP-CODE
584200000000               OF DLRREP-MAILING-INSTR-RECD
584300000000                          IS NOT EQUAL TO  AD-SALESREP
584400000000           OR  DEALER-CODE
584500000000               OF DLRREP-MAILING-INSTR-RECD
584600000000                          IS NOT EQUAL TO  AD-DEALER
584700000000               GO TO LR1-0070-0020.
584800000000
584900000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
585000000000
585100000000       LR1-0070-0020.
585200990925
585300000000           GO TO LR1-0070-0050.
585400000000
585500000000       LR1-0070-0030.
585600000000
585700000000      *****
585800000000      *****    Processing a nominee account.  Read through the Nominee
585900000000      *****    Mailing Instruction.  If the form CLTSTCLT is not found,
586000000000      *****    the client copy statement is to be suppressed.
586100000000      *****
586200000000
586300000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
586400000000               GO TO LR1-0070-0050.
586500000000
586600000000           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
586700000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
586800000000                                           OF NOMINEE-MAILING-INST-RECD.
586900000000           MOVE "CLTSTCLT  "           TO  FORM-CODE
587000000000                                           OF NOMINEE-MAILING-INST-RECD.
587100000000           START NOMINEE-MAILING-INSTRUCTIONS
587200000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
587300000000               INVALID KEY
587400000000                   GO TO LR1-0070-0040.
587500000000
587600000000           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
587700000000                                       AT  END
587800000000               GO TO LR1-0070-0040.
587900000000
588000000000           IF  FORM-CODE
588100000000               OF NOMINEE-MAILING-INST-RECD
588200000000                          IS NOT EQUAL TO  "CLTSTCLT  "
588300000000           OR  NOMINEE-CODE
588400000000               OF NOMINEE-MAILING-INST-RECD
588500000000                          IS NOT EQUAL TO  WS-NOMINEE-CODE
588600000000               GO TO LR1-0070-0040.
588700000000
588800000000           GO TO LR1-0070-0050.
588900000000
589000000000       LR1-0070-0040.
589100990925
589200000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
589300000000
589400000000       LR1-0070-0050.
589500000000
589600000000      *****
589700000000      *****    Read through the Dealer Mailing Instruction to determine
589800000000      *****    if the Statement Print Header is to be flagged to print
589900000000      *****    dealer copies of the statements.
590000000000      *****
590100000000
590200000000           MOVE SPACES                 TO  WS-DEAMLGINS-REC.
590300000000
590400000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
590500000000           MOVE AD-DEALER              TO  DEALER-CODE
590600000000                                           OF DEALER-MAILING-INSTR-RECD.
590700000000           MOVE "CLTSTDLR  "           TO  FORM-CODE
590800000000                                           OF DEALER-MAILING-INSTR-RECD.
590900000000           START DEALER-MAILING-INSTR
591000000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
591100000000               INVALID KEY
591200990925                   GO TO LR1-0080
591300990925           END-START
591400990925
591500000000           READ DEALER-MAILING-INSTR NEXT
591600000000                                       AT  END
591700990925               GO TO LR1-0080
591800990925           END-READ
591900990925
592000000000           IF  FORM-CODE
592100000000               OF DEALER-MAILING-INSTR-RECD
592200000000                          IS NOT EQUAL TO  "CLTSTDLR  "
592300000000           OR  DEALER-CODE
592400000000               OF DEALER-MAILING-INSTR-RECD
592500000000                          IS NOT EQUAL TO  AD-DEALER
592600000000               GO TO LR1-0080.
592700000000
592800000000           MOVE DEALER-MAILING-INSTR-RECD
592900000000                                       TO  WS-DEAMLGINS-REC.
593000000000
593100000000       LR1-0080.
593200000000
593300000000      *****
593400000000      *****    Determine if the Statement Print Header is to be flagged
593500000000      *****    to print dealer representative copies of the statements.
593600000000      *****    Initially read through the Dealer Mailing Instruction.
593700000000      *****    If a record is not found, read through the Dealer
593800000000      *****    Representative Mailing Instuction.
593900000000      *****
594000000000
594100000000           MOVE SPACES                 TO  WS-DERMLGINS-REC.
594200000000
594300000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
594400000000           MOVE AD-DEALER              TO  DEALER-CODE
594500000000                                           OF DEALER-MAILING-INSTR-RECD.
594600000000           MOVE "CLTSTREP  "           TO  FORM-CODE
594700000000                                           OF DEALER-MAILING-INSTR-RECD.
594800000000           START DEALER-MAILING-INSTR
594900000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
595000000000               INVALID KEY
595100000000                   GO TO LR1-0080-0010.
595200000000
595300000000           READ DEALER-MAILING-INSTR NEXT
595400000000                                       AT  END
595500000000               GO TO LR1-0080-0010.
595600000000
595700000000           IF  FORM-CODE
595800000000               OF DEALER-MAILING-INSTR-RECD
595900000000                          IS NOT EQUAL TO  "CLTSTREP  "
596000000000           OR  DEALER-CODE
596100000000               OF DEALER-MAILING-INSTR-RECD
596200000000                          IS NOT EQUAL TO  AD-DEALER
596300000000               GO TO LR1-0080-0010.
596400000000
596500000000           MOVE CORR MFADEAMIP
596600000000                     OF DEALER-MAILING-INSTR-RECD
596700000000                                       TO  MFADERMIP
596800000000                                           OF WS-DERMLGINS-REC.
596900000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
597000000000                                           OF WS-DERMLGINS-REC.
597100000000           GO TO LR1-0090.
597200000000
597300000000       LR1-0080-0010.
597400990923
597500000000           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
597600000000           MOVE AD-DEALER              TO  DEALER-CODE
597700000000                                           OF DLRREP-MAILING-INSTR-RECD.
597800000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
597900000000                                           OF DLRREP-MAILING-INSTR-RECD.
598000000000           MOVE "CLTSTREP  "           TO  FORM-CODE
598100000000                                           OF DLRREP-MAILING-INSTR-RECD.
598200000000           START DEALER-REP-MAILING-INSTR
598300000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
598400000000               INVALID KEY
598500000000                   GO TO LR1-0090.
598600000000
598700000000           READ DEALER-REP-MAILING-INSTR NEXT
598800000000                                       AT  END
598900000000               GO TO LR1-0090.
599000000000
599100000000           IF  FORM-CODE
599200000000               OF DLRREP-MAILING-INSTR-RECD
599300000000                          IS NOT EQUAL TO  "CLTSTREP  "
599400000000           OR  DEALER-REP-CODE
599500000000               OF DLRREP-MAILING-INSTR-RECD
599600000000                          IS NOT EQUAL TO  AD-SALESREP
599700000000           OR  DEALER-CODE
599800000000               OF DLRREP-MAILING-INSTR-RECD
599900000000                          IS NOT EQUAL TO  AD-DEALER
600000000000               GO TO LR1-0090.
600100000000
600200000000           MOVE DLRREP-MAILING-INSTR-RECD
600300000000                                       TO  WS-DERMLGINS-REC.
600400000000
600500000000       LR1-0090.
600600000000
600700000000      *****
600800000000      *****    Read through the Nominee Instruction to determine if the
600900000000      *****    Statement Print Header is to be flagged to print nominee
601000000000      *****    copies of the statements.
601100000000      *****
601200000000
601300000000           MOVE SPACES                 TO  WS-NOMMLGINS-REC.
601400000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
601500000000               GO TO LR1-0090-0010.
601600000000
601700000000           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
601800000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
601900000000                                           OF NOMINEE-MAILING-INST-RECD.
602000000000           MOVE "CLTSTNOM  "           TO  FORM-CODE
602100000000                                           OF NOMINEE-MAILING-INST-RECD.
602200000000           START NOMINEE-MAILING-INSTRUCTIONS
602300000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
602400000000               INVALID KEY
602500000000                   GO TO LR1-0090-0010.
602600000000
602700000000           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
602800000000                                       AT  END
602900000000               GO TO LR1-0090-0010.
603000000000
603100000000           IF  FORM-CODE
603200000000               OF NOMINEE-MAILING-INST-RECD
603300000000                          IS NOT EQUAL TO  "CLTSTNOM  "
603400000000           OR  NOMINEE-CODE
603500000000               OF NOMINEE-MAILING-INST-RECD
603600000000                          IS NOT EQUAL TO  WS-NOMINEE-CODE
603700000000               GO TO LR1-0090-0010.
603800000000
603900000000           MOVE NOMINEE-MAILING-INST-RECD
604000000000                                       TO  WS-NOMMLGINS-REC.
604100000000
604200000000       LR1-0090-0010.
604300000000
604400000000      *****
604500000000      *****    Format the Statement Print Header record.
604600000000      *****
604700020921
604800000000
604900000000           MOVE "N"                    TO  WS-STMT-PRT-HDR-WSW.
605000000000
605100000000           MOVE SPACES                 TO  STMT-PRT-HDR-RECD.
605200000000           MOVE AD-INVESTOR-NO         TO  INVESTOR-NO
605300000000                                           OF STMT-PRT-HDR-RECD.
605400020921R13756*    IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
605500020921  ¦   *     MOVE WS-MSTR-ACCOUNT-NO  TO  ACCOUNT
605600020921  ¦   *                                    OF STMT-PRT-HDR-RECD
605700020921R13756*    ELSE
605800020921            MOVE AD-ACCOUNT             TO  ACCOUNT
605900020921                                           OF STMT-PRT-HDR-RECD
606000020921R13756*    END-IF
606100000000           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
606200000000                                           OF STMT-PRT-HDR-RECD.
606300000000           MOVE AD-DEALER              TO  DEALER
606400000000                                           OF STMT-PRT-HDR-RECD.
606500000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
606600000000                                           OF STMT-PRT-HDR-RECD.
606700000000           MOVE II-SIN-NO              TO  SOCIAL-INSURANCE-NO
606800000000                                           OF STMT-PRT-HDR-RECD.
606900000000           MOVE WS-CLTSPR-SW           TO  CLIENT-COPY-SUPPRESSED
607000000000                                           OF STMT-PRT-HDR-RECD.
607100000000           MOVE II-HOLD-MAIL           TO  HOLD-MAIL
607200000000                                           OF STMT-PRT-HDR-RECD.
607300000000           MOVE BRANCH-CODE
607400000000                OF WS-DEAMLGINS-REC    TO  DEALER-BRANCH-CODE
607500000000                                           OF STMT-PRT-HDR-RECD.
607600000000           MOVE FREQUENCY-CODE
607700000000                OF WS-DEAMLGINS-REC    TO  DEALER-FREQUENCY-CODE
607800000000                                           OF STMT-PRT-HDR-RECD.
607900000000           MOVE METHOD-OF-DELIVERY
608000000000                OF WS-DEAMLGINS-REC    TO  DEALER-METHOD-OF-DELIVERY
608100000000                                           OF STMT-PRT-HDR-RECD.
608200000000           MOVE SEND-TO
608300000000                OF WS-DEAMLGINS-REC    TO  DEALER-SEND-TO
608400000000                                           OF STMT-PRT-HDR-RECD.
608500000000           MOVE BRANCH-CODE
608600000000                OF WS-DERMLGINS-REC    TO  DLR-REP-BRANCH-CODE
608700000000                                           OF STMT-PRT-HDR-RECD.
608800000000           MOVE FREQUENCY-CODE
608900000000                OF WS-DERMLGINS-REC    TO  DLR-REP-FREQUENCY-CODE
609000000000                                           OF STMT-PRT-HDR-RECD.
609100000000           MOVE METHOD-OF-DELIVERY
609200000000                OF WS-DERMLGINS-REC    TO  DLR-REP-METHOD-OF-DELIVERY
609300000000                                           OF STMT-PRT-HDR-RECD.
609400000000           MOVE SEND-TO
609500000000                OF WS-DERMLGINS-REC    TO  DLR-REP-SEND-TO
609600000000                                           OF STMT-PRT-HDR-RECD.
609700000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
609800000000                                           OF STMT-PRT-HDR-RECD.
609900000000           MOVE BRANCH-CODE
610000000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-BRANCH-CODE
610100000000                                           OF STMT-PRT-HDR-RECD.
610200000000           MOVE FREQUENCY-CODE
610300000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-FREQUENCY-CODE
610400000000                                           OF STMT-PRT-HDR-RECD.
610500000000           MOVE METHOD-OF-DELIVERY
610600000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-METHOD-OF-DELIVERY
610700000000                                           OF STMT-PRT-HDR-RECD.
610800000000           MOVE SEND-TO
610900000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-SEND-TO
611000000000                                           OF STMT-PRT-HDR-RECD.
611100000000           MOVE II-COUNTRY-CODE        TO  COUNTRY-CODE
611200000000                                           OF STMT-PRT-HDR-RECD.
611300980116           MOVE II-LAST-NAME           TO  LAST-NAME
611400980116                                           OF STMT-PRT-HDR-RECD.
611500980116           MOVE II-FIRST-NAME          TO  FIRST-NAME
611600980116                                           OF STMT-PRT-HDR-RECD.
611700980116           MOVE II-INITIALS            TO  INITIALS
611800980116                                           OF STMT-PRT-HDR-RECD.
611900990105           MOVE II-LEGAL-NAME          TO  LEGAL-NAME
612000990105                                           OF STMT-PRT-HDR-RECD.
612100980129           MOVE AD-START-DATE          TO  START-DATE
612200980129                                           OF STMT-PRT-HDR-RECD.
612300980129           MOVE AD-END-DATE            TO  END-DATE
612400980129                                           OF STMT-PRT-HDR-RECD.
612500980116
612600000000      *****
612700000000      *****    Initialize the statement currency totals.
612800000000      *****
612900000000
613000000000           SET SC                      TO  1.
613100000000
613200000000       LR1-0100.
613300990923
613400000000           MOVE ZEROES                 TO  WS-SC-MARKET-VALUE (SC).
613500000000           MOVE "N"                    TO  WS-SC-RPTIND (SC).
613600990925
613700000000           IF  SC            IS LESS THAN  WS-SC-MAX
613800000000               SET SC               UP BY  1
613900000000               GO TO LR1-0100.
614000000000
614100000000      *****
614200000000      *****    Initialize the statement account funds table.
614300000000      *****
614400000000
614500000000           MOVE SPACES                 TO  WS-STMT-ACCOUNT-FUND-TABLE.
614600000000           MOVE ZEROES                 TO  WS-SAF-LST.
614700000000
614800000000      *****
614900000000      *****    Initialize the statement transaction controls.
615000000000      *****
615100000000
615200000000           MOVE LOW-VALUES             TO  CURR-CTLS-DPR.
615300000000           MOVE LOW-VALUES             TO  PREV-CTLS-DPR.
615400000000
615500000000       LR1-EXIT.
615600000000           EXIT.
615700000000      /
615800000000       FORMAT-ACTTYP-DESC.
615900000000
616000000000      ******************************************************************
616100000000      *                                                                *
616200000000      *    F M T .   A C C O U N T   T Y P E   D E S C R I P T I O N   *
616300000000      *                                                                *
616400000000      ******************************************************************
616500000000
616600000000      *****
616700000000      *****    Initialize the work items.
616800000000      *****
616900000000
617000000000           MOVE SPACES                 TO  WS-ACTTYPIN-SFX.
617100000000           MOVE SPACES                 TO  WS-ACTTYPLIT1.
617200000000           MOVE SPACES                 TO  WS-ACTTYPLIT2.
617300000000           MOVE SPACES                 TO  WS-ACTTYPLIT3.
617400000000           MOVE SPACES                 TO  WS-ACTTYPOUT.
617500000000
617600000000      *****
617700980326      *****    The account type description will be formatted
617800980326      *****    If NOMINEE, format the description.
617900000000      *****
618000980326
618100980326980320 FAD-0001.
618200980326
618300980326980320     IF  WS-ACTTYPNOM   IS NOT = "Y"
618400980326980320         GO TO FAD-0003.
618500980326
618600980326980320     INITIALIZE INVESTOR-J-REC.
618700980326980320     MOVE PR-INVESTOR-NO TO ACIVRNO OF INVESTOR-J-REC.
618800980326980320     MOVE PR-ACCOUNT     TO ACACTNO OF INVESTOR-J-REC.
618900980326
619000980326980320     START INVESTOR-JOIN
619100980326980320           KEY IS NOT LESS THAN EXTERNALLY-DESCRIBED-KEY
619200980326980320           INVALID KEY
619300980326980320           GO TO FAD-0001.
619400980326
619500980326980320 FAD-0002.
619600980326980320     READ INVESTOR-JOIN NEXT RECORD
619700980326980320          AT END
619800980326980320          GO TO FAD-0030
619900980326980320     END-READ.
620000980326
620100980326980320     IF PR-INVESTOR-NO IS NOT = ACIVRNO
620200980326980320                          OF INVESTOR-J-REC
620300980326980320        GO TO FAD-0030.
620400980326
620500980326980320     IF PR-ACCOUNT     IS NOT = ACACTNO
620600980326980320                          OF INVESTOR-J-REC
620700980326980320        GO TO FAD-0030.
620800980326
620900980409           INITIALIZE MFANMTPYP.
621000980408980320     MOVE ANACTYP OF INVESTOR-JOIN TO NOMINEE-ACCOUNT-TYPE
621100980408980320                  OF NOMINEE-ACCOUNT-TYPE-DESCR.
621200980408980320     MOVE WS-ACTTYPLNG             TO LANGUAGE-CODE
621300980408980320                  OF NOMINEE-ACCOUNT-TYPE-DESCR.
621400980326
621500980408980320     READ NOMINEE-ACCOUNT-TYPE-DESCR
621600980408980320          INVALID KEY
621700980408980320          MOVE SPACES TO WS-ACTTYPOUT
621800980408980320     END-READ.
621900980326
622000990922990922     IF WS-STMTVRSN        IS NOT EQUAL 6
622100990922980320        MOVE ACCOUNT-TYPE-DESCR OF NOMINEE-ACCOUNT-TYPE-DESCR
622200990922980320                                TO WS-ACTTYPOUT
622300990922           ELSE
622400990922              MOVE SPACES             TO WS-ACTTYPOUT
622500990922           END-IF
622600980326
622700980408980320     GO TO FAD-0030.
622800980326
622900980326980320 FAD-0003.
623000000000
623100000000           IF  WS-ACTTYPLNG   IS EQUAL TO  "F"
623200990922               GO TO FAD-0040
623300990922           END-IF
623400990922
623500000000      *****
623600000000      *****    The English description may be formatted with a LOCKED-IN
623700000000      *****    literal, a SPOUSAL literal, a GROUP literal and the
623800000000      *****    original account type description.  If any of the
623900000000      *****    literals are not applicable, they will be suppressed.
624000000000      *****    Initially, determine which literals are applicable.
624100000000      *****
624200000000
624300000000           IF  WS-ACTTYPLCK   IS EQUAL TO  "Y"
624400000000           AND WS-ACTTYPSHT  NOT EQUAL TO  "LIRA "
624500000000               MOVE "LOCKED-IN    "    TO  WS-ACTTYPLIT1.
624600000000           IF  WS-ACTTYPGRP   IS EQUAL TO  "Y"
624700000000               MOVE "GROUP        "    TO  WS-ACTTYPLIT2.
624800000000           IF  WS-ACTTYPSPS   IS EQUAL TO  "Y"
624900000000               MOVE "SPOUSAL      "    TO  WS-ACTTYPLIT3.
625000000000
625100000000      *****
625200000000      *****    Compress the optional English literals.
625300000000      *****
625400000000
625500000000           IF  WS-ACTTYPLIT1 NOT EQUAL TO  SPACES
625600000000               GO TO FAD-0010.
625700000000
625800000000           IF  WS-ACTTYPLIT2 NOT EQUAL TO  SPACES
625900000000               MOVE WS-ACTTYPLIT2      TO  WS-ACTTYPLIT1
626000000000               MOVE SPACES             TO  WS-ACTTYPLIT2
626100000000               GO TO FAD-0010.
626200000000
626300000000           IF  WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
626400000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT1
626500000000               MOVE SPACES             TO  WS-ACTTYPLIT3
626600000000               GO TO FAD-0020.
626700000000
626800000000           GO TO FAD-0020.
626900000000
627000000000       FAD-0010.
627100990922
627200000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
627300000000           AND WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
627400000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT2
627500990922               MOVE SPACES             TO  WS-ACTTYPLIT3
627600990922           END-IF.
627700990922
627800000000       FAD-0020.
627900000000
628000000000      *****
628100000000      *****    Format the English account type description based on the
628200000000      *****    number of optional literals applicable.
628300000000      *****
628400000000
628500000000           IF  WS-ACTTYPLIT1  IS EQUAL TO  SPACES
628600000000               MOVE WS-ACTTYPIN        TO  WS-ACTTYPOUT
628700000000               GO TO FAD-0030.
628800000000
628900000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
629000000000               STRING WS-ACTTYPLIT1
629100000000                             DELIMITED BY  "  ",
629200000000                      " "    DELIMITED BY  SIZE,
629300000000                      WS-ACTTYPIN
629400000000                             DELIMITED BY  "  ",
629500000000                                     INTO  WS-ACTTYPOUT
629600000000               GO TO FAD-0030.
629700000000
629800000000           IF  WS-ACTTYPLIT3  IS EQUAL TO  SPACES
629900000000               STRING WS-ACTTYPLIT1
630000000000                             DELIMITED BY  "  ",
630100000000                      " "    DELIMITED BY  SIZE,
630200000000                      WS-ACTTYPLIT2
630300000000                             DELIMITED BY  "  ",
630400000000                      " "    DELIMITED BY  SIZE,
630500000000                      WS-ACTTYPIN
630600000000                             DELIMITED BY  "  ",
630700000000                                     INTO  WS-ACTTYPOUT
630800000000               GO TO FAD-0030.
630900000000
631000000000           STRING WS-ACTTYPLIT1
631100000000                             DELIMITED BY  "  ",
631200000000                  " "        DELIMITED BY  SIZE,
631300000000                  WS-ACTTYPLIT2
631400000000                             DELIMITED BY  "  ",
631500000000                  " "        DELIMITED BY  SIZE,
631600000000                  WS-ACTTYPLIT3
631700000000                             DELIMITED BY  "  ",
631800000000                  " "        DELIMITED BY  SIZE,
631900000000                  WS-ACTTYPIN
632000000000                             DELIMITED BY  "  ",
632100000000                                     INTO  WS-ACTTYPOUT.
632200000000
632300000000       FAD-0030.
632400000000           GO TO FAD-EXIT.
632500000000
632600000000       FAD-0040.
632700000000
632800000000      *****
632900000000      *****    The French description may be formatted with a GROUP
633000000000      *****    literal, a LOCKED-IN literal, a SPOUSAL literal and the
633100000000      *****    original account type description.  If any of the
633200000000      *****    literals are not applicable, they will be suppressed.
633300000000      *****    Initially, determine which literals are applicable.
633400000000      *****
633500000000
633600000000           IF  WS-ACTTYPGRP   IS EQUAL TO  "Y"
633700000000               MOVE "COLLECTIF    "    TO  WS-ACTTYPLIT1.
633800000000           IF  WS-ACTTYPLCK   IS EQUAL TO  "Y"
633900000000           AND WS-ACTTYPSHT  NOT EQUAL TO  "LIRA "
634000000000               MOVE "IMMOBILISE   "    TO  WS-ACTTYPLIT2.
634100000000           IF  WS-ACTTYPSPS   IS EQUAL TO  "Y"
634200000000               MOVE "DE CONJOINT  "    TO  WS-ACTTYPLIT3.
634300000000
634400000000      *****
634500000000      *****    Compress the optional French literals.
634600000000      *****
634700000000
634800000000           IF  WS-ACTTYPLIT1 NOT EQUAL TO  SPACES
634900000000               GO TO FAD-0050.
635000000000
635100000000           IF  WS-ACTTYPLIT2 NOT EQUAL TO  SPACES
635200000000               MOVE WS-ACTTYPLIT2      TO  WS-ACTTYPLIT1
635300000000               MOVE SPACES             TO  WS-ACTTYPLIT2
635400000000               GO TO FAD-0050.
635500000000
635600000000           IF  WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
635700000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT1
635800000000               MOVE SPACES             TO  WS-ACTTYPLIT3
635900000000               GO TO FAD-0060.
636000000000
636100000000           GO TO FAD-0060.
636200000000
636300000000       FAD-0050.
636400000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
636500000000           AND WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
636600000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT2
636700000000               MOVE SPACES             TO  WS-ACTTYPLIT3.
636800000000
636900000000       FAD-0060.
637000000000
637100000000      *****
637200000000      *****    Format the French account type description.  Since the
637300000000      *****    optional literals come after the original account
637400000000      *****    description and they have been compressed, it is not
637500000000      *****    necessary to base the formatting on which option literals
637600000000      *****    are applicable.
637700000000      *****
637800000000
637900000000           STRING WS-ACTTYPIN
638000000000                             DELIMITED BY  "  ",
638100000000                  " "        DELIMITED BY  SIZE,
638200000000                  WS-ACTTYPLIT1
638300000000                             DELIMITED BY  "  ",
638400000000                  " "        DELIMITED BY  SIZE,
638500000000                  WS-ACTTYPLIT2
638600000000                             DELIMITED BY  "  ",
638700000000                  " "        DELIMITED BY  SIZE,
638800000000                  WS-ACTTYPLIT3
638900000000                             DELIMITED BY  "  ",
639000000000                                     INTO  WS-ACTTYPOUT.
639100000000
639200000000       FAD-EXIT.
639300000000           EXIT.
639400000000      /
639500000000       PRINT-ERR-LINE.
639600000000
639700000000      ******************************************************************
639800000000      *                                                                *
639900000000      *    P R I N T   E R R O R   L I N E   R O U T I N E             *
640000000000      *                                                                *
640100000000      ******************************************************************
640200000000
640300000000      *****
640400000000      *****    If the error line has not been formatted, format it using
640500000000      *****    the Statement File - Primary record.
640600000000      *****
640700000000
640800000000           IF  ERROR-LINE-1  NOT EQUAL TO  SPACES
640900000000               GO TO PEL-0010.
641000000000
641100000000           MOVE PR-INVESTOR-NO         TO  EL1-INVESTOR-NO.
641200000000           MOVE PR-ACCOUNT             TO  EL1-ACCOUNT.
641300000000           MOVE PR-SECTION-NO          TO  EL1-SECTION-NO.
641400000000           MOVE "-"                    TO  EL1-SECTION-LITDSH.
641500000000           MOVE PR-SECTION-SEQUENCE    TO  EL1-SECTION-SEQUENCE.
641600000000           MOVE PR-LANGUAGE-CODE       TO  EL1-LANGUAGE-CODE.
641700000000           MOVE PR-DEALER              TO  EL1-DEALER.
641800000000           MOVE PR-DEALER-REP-CODE     TO  EL1-DEALER-REP-CODE.
641900000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
642000000000
642100000000       PEL-0010.
642200000000
642300000000      *****
642400000000      *****    If the page is full, start a new page and print the
642500000000      *****    Heading lines.
642600000000      *****
642700000000
642800000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
642900000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
643000000000
643100000000      *****
643200000000      *****    Print the error line.
643300000000      *****
643400000000
643500000000           WRITE PRINT-RECD          FROM  ERROR-LINE-1
643600000000                         AFTER  ADVANCING  1.
643700000000           ADD 1                       TO  WS-LNECNT.
643800000000
643900000000       PEL-EXIT.
644000000000           EXIT.
644100000000      /
644200000000       INITIAL-LOGIC-RTN.
644300000000
644400000000      ******************************************************************
644500000000      *                                                                *
644600000000      *    I N I T I A L   L O G I C   R O U T I N E                   *
644700000000      *                                                                *
644800000000      ******************************************************************
644900000000
645000000000      *****
645100000000      *****    Retrieve the Local Data Area.
645200000000      *****
645300000000
645400000000           ACCEPT WS-LOCAL-DATA-AREA FROM  WS-LOCAL.
645500000000
645600990119
645700000000      *****
645800000000      *****    Using the Management Company Code, determine which
645900000000      *****    version of the statement is to be produced.  The standard
646000000000      *****    statement will be zero.
646100000000      *****
646200000000
646300990921           MOVE ZEROES                    TO  WS-STMTVRSN.
646400990921           EVALUATE WS-LDA-STMT-VERSION
646500091123R73890         WHEN "A"       MOVE  "A"   TO WS-STMTVRSN
646600990921           END-EVALUATE.
646700000000
646800000000       ILR-0010.
646900000000
647000000000      *****
647100000000      *****    Initialize the program defined work items.
647200000000      *****
647300000000
647400000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT.
647500000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-00.
647600000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-05.
647700000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-10.
647800010308RF8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-12.
647900000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-15.
648000141007141165     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-16.
648100000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-20.
648200060616R35042     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-21.
648300130408117038     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-22.
648400010321RF8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-25.
648500991126           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-30.
648600040406R20870     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-33.
648700991126           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-35.
648800040406R20870     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-38.
648900010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-40.
649000010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-43.
649100010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-45.
649200000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-50.
649300000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-60.
649400991013           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-62.
649500010323rf8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-67.
649600000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-70.
649700000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-82.
649800991013           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-84.
649900010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-90.
650000000000           MOVE ZEROES                 TO  WS-STMT-DTA-RCNT.
650100000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-01.
650200000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-05.
650300000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-10.
650400000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-20.
650500981207           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-60.
650600000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-80.
650700000000           MOVE ZEROES                 TO  WS-STMT-PRT-DTL-WCNT.
650800000000           MOVE ZEROES                 TO  WS-STMT-PRT-HDR-WCNT.
650900980210
651000000000
65110001121112090 *    MOVE 100                    TO  WS-SAF-MAX.
651200161012      * RFS 156599 START
65130016101212090 *    MOVE 500                    TO  WS-SAF-MAX.
651400161108164194*    MOVE 900                    TO  WS-SAF-MAX.
651500161108164194     MOVE 2400                   TO  WS-SAF-MAX.
651600161012      * RFS 156599 END
651700000000           MOVE 2                      TO  WS-SC-MAX.
651800000000           MOVE 1000                   TO  WS-SIA-MAX.
651900000000           MOVE 87                     TO  WS-SPL-MAX.
652000991213
652100091123R73890     IF  WS-STMTVRSN    IS EQUAL TO  "A"
652200100224R79293*        MOVE 26                 TO  WS-SPL-DTLFST
652300100224R79293         MOVE 27                 TO  WS-SPL-DTLFST
652400091223  |            MOVE 68                 TO  WS-SPL-DTLLST
652500100224R79293*        MOVE 8                  TO  WS-SPL-HDGFST
652600100224R79293         MOVE 9                  TO  WS-SPL-HDGFST
652700091123  |            MOVE 12                 TO  WS-SPML-MAX
652800091123  |            GO TO ILR-0020
652900091123R73890     END-IF
653000091123
653100000000           MOVE 27                     TO  WS-SPL-DTLFST.
653200000000           MOVE 68                     TO  WS-SPL-DTLLST.
653300000000           MOVE 9                      TO  WS-SPL-HDGFST.
653400000000           MOVE 14                     TO  WS-SPML-MAX.
653500000000
653600000000       ILR-0020.
653700990921
653800000000           MOVE 99                     TO  WS-LNECNT.
653900000000           MOVE 60                     TO  WS-LNEMAX.
654000000000           MOVE ZEROES                 TO  WS-PGECNT.
654100000000
654200000000           MOVE "N"                    TO  WS-EOJHDG-SW.
654300000000
654400000000           MOVE LOW-VALUES             TO  CURR-CONTROLS.
654500000000           MOVE LOW-VALUES             TO  PREV-CONTROLS.
654600000000
654700000000      *****
654800000000      *****    Format the heading literals.
654900000000      *****
655000000104           MOVE   WS-LDA-AS-AT-DATE  TO        WS-DATE.
655100000104           MOVE   WS-LDA-AS-AT-DATE  TO        WS-SYS-DATE.
655200990120
655300000104           MOVE WS-DATE-YYYY           TO  HL1-YY.
655400000000           MOVE WS-DATE-MM             TO  HL1-MM.
655500000000           MOVE WS-DATE-DD             TO  HL1-DD.
655600000000
655700000000           ACCEPT HL1-TIME           FROM  TIME.
655800000000           INSPECT HL1-TIME REPLACING ALL  SPACE
655900000000                                       BY  ":".
656000000000           MOVE "."                    TO  HL1-TIME-DOT.
656100000000
656200990921
656300000000      *****
656400000000      *****    Open the Input files.
656500000000      *****
656600000000
656700000000           OPEN    INPUT   DEALER-MAILING-INSTR
656800000000                           DEALER-FILE
656900000000                           DEALER-REP-FILE
657000000000                           DEALER-REP-MAILING-INSTR
657100000000                           DEALER-REP-ADDRESS
657200000000                           INSTITUTION-ADDRESS
657300000000                           INSTITUTION-BRANCH
657400980326                           INVESTOR-JOIN
657500991103991103                     ACCOUNT-TYPE-BK
657600980326                           NOMINEE-ACCOUNT-TYPE-DESCR
657700000000                           NOMINEE-MAILING-INSTRUCTIONS
657800020920R14588                     DEALER-MISC-DETAILS
657900020920R14588                     MASTER-ACCOUNT
658000000000                           STMT-MSG-FILE
658100990116                           STMT-DTA-FILE
658200190612      *RFS183449-Starts
658300190612      *                    ACCOUNT-FILE.
658400190612                           ACCOUNT-FILE
658500190612                           ACCOUNT-FILE-2.
658600190612      *RFS183449-Ends
658700000000      *****
658800000000      *****    Open the Output files.
658900000000      *****
659000000000
659100000000           OPEN    OUTPUT  STMT-PRT-DTL-FILE
659200000000                           STMT-PRT-HDR-FILE
659300000000                           PRINT-FILE.
659400980210
659500991103       ILR-0040.
659600991103
659700991103      *****
659800991103      *****    Load the account type entries.
659900991103      *****
660000991103
660100991103           MOVE SPACES                 TO  WS-ACCTYP-TBL.
660200991103           MOVE ZEROES                 TO  WS-AT-LST.
660300991103
660400991103       ILR-0050.
660500991103
660600991103           READ ACCOUNT-TYPE-BK        AT  END
660700991103               GO TO ILR-0070
660800991103           END-READ
660900991103
661000991103           IF  WS-AT-LST IS NOT LESS THAN  WS-AT-MAX
661100991103               DISPLAY "ACCOUNT TYPES (MFAACTYP) EXCEED PROGRAM MAX"
661200991103               GO TO ILR-EXIT
661300991103           END-IF
661400991103
661500991103           ADD 1                       TO  WS-AT-LST.
661600991103           SET AT1                     TO  WS-AT-LST.
661700991103           MOVE ACCOUNT-TYPE-CODE
661800991103                OF ACCOUNT-TYPE-BK-REC TO  WS-AT-CODE (AT1).
661900991103           MOVE ACCOUNT-TYPE-RULE
662000991103                OF ACCOUNT-TYPE-BK-REC TO  WS-AT-RULE (AT1).
662100991103           GO TO ILR-0050.
662200991103
662300991103       ILR-0070.
662400991103
662500991103           IF  WS-AT-LST NOT GREATER THAN  ZEROES
662600991103               DISPLAY "NO ACCOUNT TYPES (MFAACTYP) FOUND"
662700991103           END-IF.
662800991103
662900991103      **********
663000000000
663100000000       ILR-EXIT.
663200000000           EXIT.
663300991103
663400991103      **********
663500000000      /
663600000000       END-JOB-RTN.
663700000000
663800000000      ******************************************************************
663900000000      *                                                                *
664000000000      *    E N D   J O B   R O U T I N E                               *
664100000000      *                                                                *
664200000000      ******************************************************************
664300000000
664400000000      *****
664500000000      *****    Print the Control Report on a separate page.
664600000000      *****
664700000000
664800000000           MOVE 99                     TO  WS-LNECNT.
664900000000           MOVE "N"                    TO  WS-BYPPRT-SW.
665000000000           MOVE "Y"                    TO  WS-EOJHDG-SW.
665100000000           MOVE "N"                    TO  WS-SELPRT-SW.
665200000000
665300000000           MOVE WS-MANAGEMENT-COMPANY-CODE
665400000000                                       TO  CL2-MANAGEMENT-COMPANY-CODE.
665500000000           MOVE CONTROL-LINE-2         TO  CONTROL-LINE-1.
665600000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
665700000000
665800000000           MOVE SPACES                 TO  CONTROL-LINE-1.
665900000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
666000000000
666100000000           MOVE SPACES                 TO  CONTROL-LINE-1.
666200000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
666300000000
666400000000           MOVE SPACES                 TO  CONTROL-LINE-1.
666500000000           MOVE WS-STMT-DTA-RLIT       TO  CL1-LIT.
666600000000           MOVE WS-STMT-DTA-RCNT       TO  CL1-CNT.
666700000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
666800000000
666900000000           MOVE SPACES                 TO  CONTROL-LINE-1.
667000000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
667100000000
667200000000           IF  WS-STMT-DTA-BCNT
667300000000                          IS GREATER THAN  ZEROES
667400000000               MOVE SPACES             TO  CONTROL-LINE-1
667500000000               MOVE WS-STMT-DTA-BLIT   TO  CL1-LIT
667600000000               MOVE WS-STMT-DTA-BCNT   TO  CL1-CNT
667700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
667800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
667900000000
668000000000           IF  WS-STMT-DTA-BCNT-00
668100000000                          IS GREATER THAN  ZEROES
668200000000               MOVE SPACES             TO  CONTROL-LINE-1
668300000000               MOVE WS-STMT-DTA-BLIT-00
668400000000                                       TO  CL1-LIT
668500000000               MOVE WS-STMT-DTA-BCNT-00
668600000000                                       TO  CL1-CNT
668700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
668800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
668900000000
669000000000           IF  WS-STMT-DTA-BCNT-05
669100000000                          IS GREATER THAN  ZEROES
669200000000               MOVE SPACES             TO  CONTROL-LINE-1
669300000000               MOVE WS-STMT-DTA-BLIT-05
669400000000                                       TO  CL1-LIT
669500000000               MOVE WS-STMT-DTA-BCNT-05
669600000000                                       TO  CL1-CNT
669700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
669800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
669900000000
670000000000           IF  WS-STMT-DTA-BCNT-10
670100000000                          IS GREATER THAN  ZEROES
670200000000               MOVE SPACES             TO  CONTROL-LINE-1
670300000000               MOVE WS-STMT-DTA-BLIT-10
670400000000                                       TO  CL1-LIT
670500000000               MOVE WS-STMT-DTA-BCNT-10
670600000000                                       TO  CL1-CNT
670700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
670800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
670900000000
671000010308RF8633     IF  WS-STMT-DTA-BCNT-12
671100010308                          IS GREATER THAN  ZEROES
671200010308               MOVE SPACES             TO  CONTROL-LINE-1
671300010308               MOVE WS-STMT-DTA-BLIT-12
671400010308                                       TO  CL1-LIT
671500010308               MOVE WS-STMT-DTA-BCNT-12
671600010308                                       TO  CL1-CNT
671700010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
671800010308RF8633         MOVE "Y"                TO  WS-BYPPRT-SW.
671900010308
672000000000           IF  WS-STMT-DTA-BCNT-15
672100000000                          IS GREATER THAN  ZEROES
672200000000               MOVE SPACES             TO  CONTROL-LINE-1
672300000000               MOVE WS-STMT-DTA-BLIT-15
672400000000                                       TO  CL1-LIT
672500000000               MOVE WS-STMT-DTA-BCNT-15
672600000000                                       TO  CL1-CNT
672700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
672800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
672900000000
673000000000           IF  WS-STMT-DTA-BCNT-20
673100000000                          IS GREATER THAN  ZEROES
673200000000               MOVE SPACES             TO  CONTROL-LINE-1
673300000000               MOVE WS-STMT-DTA-BLIT-20
673400000000                                       TO  CL1-LIT
673500000000               MOVE WS-STMT-DTA-BCNT-20
673600000000                                       TO  CL1-CNT
673700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
673800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
673900000000
674000060616R35042     IF  WS-STMT-DTA-BCNT-21
674100060616  ¦                       IS GREATER THAN  ZEROES
674200060616  ¦            MOVE SPACES             TO  CONTROL-LINE-1
674300060616  ¦            MOVE WS-STMT-DTA-BLIT-21
674400060616  ¦                                    TO  CL1-LIT
674500060616  ¦            MOVE WS-STMT-DTA-BCNT-21
674600060616  ¦                                    TO  CL1-CNT
674700060616  V            PERFORM PRINT-CTL-LINE THRU PCL-EXIT
674800060616R35042         MOVE "Y"                TO  WS-BYPPRT-SW.
674900060616
675000130408117038     IF  WS-STMT-DTA-BCNT-22
675100130408  ¦                       IS GREATER THAN  ZEROES
675200130408  ¦            MOVE SPACES             TO  CONTROL-LINE-1
675300130408  ¦            MOVE WS-STMT-DTA-BLIT-22
675400130408  ¦                                    TO  CL1-LIT
675500130408  ¦            MOVE WS-STMT-DTA-BCNT-22
675600130408  ¦                                    TO  CL1-CNT
675700130408  V            PERFORM PRINT-CTL-LINE THRU PCL-EXIT
675800130408117038         MOVE "Y"                TO  WS-BYPPRT-SW.
675900130408
676000010321RF8633     IF  WS-STMT-DTA-BCNT-25
676100010321                          IS GREATER THAN  ZEROES
676200010321               MOVE SPACES             TO  CONTROL-LINE-1
676300010321               MOVE WS-STMT-DTA-BLIT-25
676400010321                                       TO  CL1-LIT
676500010321               MOVE WS-STMT-DTA-BCNT-25
676600010321                                       TO  CL1-CNT
676700010321               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
676800010321RF8633         MOVE "Y"                TO  WS-BYPPRT-SW.
676900010321
677000991126991126     IF  WS-STMT-DTA-BCNT-30
677100991126                          IS GREATER THAN  ZEROES
677200991126               MOVE SPACES             TO  CONTROL-LINE-1
677300991126               MOVE WS-STMT-DTA-BLIT-30
677400991126                                       TO  CL1-LIT
677500991126               MOVE WS-STMT-DTA-BCNT-30
677600991126                                       TO  CL1-CNT
677700991126               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
677800991126               MOVE "Y"                TO  WS-BYPPRT-SW.
677900991126
678000040406R20870     IF  WS-STMT-DTA-BCNT-33
678100040406                          IS GREATER THAN  ZEROES
678200040406               MOVE SPACES             TO  CONTROL-LINE-1
678300040406               MOVE WS-STMT-DTA-BLIT-33
678400040406                                       TO  CL1-LIT
678500040406               MOVE WS-STMT-DTA-BCNT-33
678600040406                                       TO  CL1-CNT
678700040406               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
678800040406               MOVE "Y"                TO  WS-BYPPRT-SW.
678900040406
679000991126991126     IF  WS-STMT-DTA-BCNT-35
679100991126                          IS GREATER THAN  ZEROES
679200991126               MOVE SPACES             TO  CONTROL-LINE-1
679300991126               MOVE WS-STMT-DTA-BLIT-35
679400991126                                       TO  CL1-LIT
679500991126               MOVE WS-STMT-DTA-BCNT-35
679600991126                                       TO  CL1-CNT
679700991126               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
679800991126               MOVE "Y"                TO  WS-BYPPRT-SW.
679900991126
680000040406R20870     IF  WS-STMT-DTA-BCNT-38
680100040406                          IS GREATER THAN  ZEROES
680200040406               MOVE SPACES             TO  CONTROL-LINE-1
680300040406               MOVE WS-STMT-DTA-BLIT-38
680400040406                                       TO  CL1-LIT
680500040406               MOVE WS-STMT-DTA-BCNT-38
680600040406                                       TO  CL1-CNT
680700040406               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
680800040406               MOVE "Y"                TO  WS-BYPPRT-SW.
680900040406
681000010308RF4638     IF  WS-STMT-DTA-BCNT-40
681100010308                          IS GREATER THAN  ZEROES
681200010308               MOVE SPACES             TO  CONTROL-LINE-1
681300010308               MOVE WS-STMT-DTA-BLIT-40
681400010308                                       TO  CL1-LIT
681500010308               MOVE WS-STMT-DTA-BCNT-40
681600010308                                       TO  CL1-CNT
681700010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
681800010308               MOVE "Y"                TO  WS-BYPPRT-SW.
681900010308
682000010308RF4638     IF  WS-STMT-DTA-BCNT-43
682100010308                          IS GREATER THAN  ZEROES
682200010308               MOVE SPACES             TO  CONTROL-LINE-1
682300010308               MOVE WS-STMT-DTA-BLIT-43
682400010308                                       TO  CL1-LIT
682500010308               MOVE WS-STMT-DTA-BCNT-43
682600010308                                       TO  CL1-CNT
682700010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
682800010308               MOVE "Y"                TO  WS-BYPPRT-SW.
682900010308
683000010308RF4638     IF  WS-STMT-DTA-BCNT-45
683100010308                          IS GREATER THAN  ZEROES
683200010308               MOVE SPACES             TO  CONTROL-LINE-1
683300010308               MOVE WS-STMT-DTA-BLIT-45
683400010308                                       TO  CL1-LIT
683500010308               MOVE WS-STMT-DTA-BCNT-45
683600010308                                       TO  CL1-CNT
683700010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
683800010308               MOVE "Y"                TO  WS-BYPPRT-SW.
683900010308
684000000000           IF  WS-STMT-DTA-BCNT-50
684100000000                          IS GREATER THAN  ZEROES
684200000000               MOVE SPACES             TO  CONTROL-LINE-1
684300000000               MOVE WS-STMT-DTA-BLIT-50
684400000000                                       TO  CL1-LIT
684500000000               MOVE WS-STMT-DTA-BCNT-50
684600000000                                       TO  CL1-CNT
684700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
684800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
684900000000
685000000000           IF  WS-STMT-DTA-BCNT-60
685100000000                          IS GREATER THAN  ZEROES
685200000000               MOVE SPACES             TO  CONTROL-LINE-1
685300000000               MOVE WS-STMT-DTA-BLIT-60
685400000000                                       TO  CL1-LIT
685500000000               MOVE WS-STMT-DTA-BCNT-60
685600000000                                       TO  CL1-CNT
685700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
685800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
685900000000
686000991013           IF  WS-STMT-DTA-BCNT-62
686100991013                          IS GREATER THAN  ZEROES
686200991013               MOVE SPACES             TO  CONTROL-LINE-1
686300991013               MOVE WS-STMT-DTA-BLIT-62
686400991013                                       TO  CL1-LIT
686500991013               MOVE WS-STMT-DTA-BCNT-62
686600991013                                       TO  CL1-CNT
686700991013               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
686800991013               MOVE "Y"                TO  WS-BYPPRT-SW.
686900991013
687000010323rf8633     IF  WS-STMT-DTA-BCNT-67
687100010323                          IS GREATER THAN  ZEROES
687200010323               MOVE SPACES             TO  CONTROL-LINE-1
687300010323               MOVE WS-STMT-DTA-BLIT-67
687400010323                                       TO  CL1-LIT
687500010323               MOVE WS-STMT-DTA-BCNT-67
687600010323                                       TO  CL1-CNT
687700010323               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
687800010323               MOVE "Y"                TO  WS-BYPPRT-SW.
687900010323
688000000000           IF  WS-STMT-DTA-BCNT-70
688100000000                          IS GREATER THAN  ZEROES
688200000000               MOVE SPACES             TO  CONTROL-LINE-1
688300000000               MOVE WS-STMT-DTA-BLIT-70
688400000000                                       TO  CL1-LIT
688500000000               MOVE WS-STMT-DTA-BCNT-70
688600000000                                       TO  CL1-CNT
688700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
688800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
688900000000
689000000000           IF  WS-STMT-DTA-BCNT-82
689100000000                          IS GREATER THAN  ZEROES
689200000000               MOVE SPACES             TO  CONTROL-LINE-1
689300000000               MOVE WS-STMT-DTA-BLIT-82
689400000000                                       TO  CL1-LIT
689500000000               MOVE WS-STMT-DTA-BCNT-82
689600000000                                       TO  CL1-CNT
689700000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
689800000000               MOVE "Y"                TO  WS-BYPPRT-SW.
689900000000
690000991013           IF  WS-STMT-DTA-BCNT-84
690100991013                          IS GREATER THAN  ZEROES
690200991013               MOVE SPACES             TO  CONTROL-LINE-1
690300991013               MOVE WS-STMT-DTA-BLIT-84
690400991013                                       TO  CL1-LIT
690500991013               MOVE WS-STMT-DTA-BCNT-84
690600991013                                       TO  CL1-CNT
690700991013               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
690800991013               MOVE "Y"                TO  WS-BYPPRT-SW.
690900991013
691000010308RF4638     IF  WS-STMT-DTA-BCNT-90
691100010308                          IS GREATER THAN  ZEROES
691200010308               MOVE SPACES             TO  CONTROL-LINE-1
691300010308               MOVE WS-STMT-DTA-BLIT-90
691400010308                                       TO  CL1-LIT
691500010308               MOVE WS-STMT-DTA-BCNT-90
691600010308                                       TO  CL1-CNT
691700010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
691800010308               MOVE "Y"                TO  WS-BYPPRT-SW.
691900010308
692000000000           IF  WS-BYPPRT-SW   IS EQUAL TO  "Y"
692100000000               MOVE SPACES             TO  CONTROL-LINE-1
692200000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
692300000000
692400000000           IF  WS-STMT-DTA-SCNT-01
692500000000                          IS GREATER THAN  ZEROES
692600000000               MOVE SPACES                 TO  CONTROL-LINE-1
692700000000               MOVE WS-STMT-DTA-SLIT-01
692800000000                                       TO  CL1-LIT
692900000000               MOVE WS-STMT-DTA-SCNT-01
693000000000                                       TO  CL1-CNT
693100000000               PERFORM PRINT-CTL-LINE     THRU PCL-EXIT
693200000000               MOVE "Y"                TO  WS-SELPRT-SW.
693300000000
693400000000           IF  WS-STMT-DTA-SCNT-05
693500000000                          IS GREATER THAN  ZEROES
693600000000               MOVE SPACES             TO  CONTROL-LINE-1
693700000000               MOVE WS-STMT-DTA-SLIT-05
693800000000                                       TO  CL1-LIT
693900000000               MOVE WS-STMT-DTA-SCNT-05
694000000000                                       TO  CL1-CNT
694100000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
694200000000               MOVE "Y"                TO  WS-SELPRT-SW.
694300000000
694400000000           IF  WS-STMT-DTA-SCNT-10
694500000000                          IS GREATER THAN  ZEROES
694600000000               MOVE SPACES             TO  CONTROL-LINE-1
694700000000               MOVE WS-STMT-DTA-SLIT-10
694800000000                                       TO  CL1-LIT
694900000000               MOVE WS-STMT-DTA-SCNT-10
695000000000                                       TO  CL1-CNT
695100000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
695200000000               MOVE "Y"                TO  WS-SELPRT-SW.
695300000000
695400000000           IF  WS-STMT-DTA-SCNT-20
695500000000                          IS GREATER THAN  ZEROES
695600000000               MOVE SPACES             TO  CONTROL-LINE-1
695700000000               MOVE WS-STMT-DTA-SLIT-20
695800000000                                       TO  CL1-LIT
695900000000               MOVE WS-STMT-DTA-SCNT-20
696000000000                                       TO  CL1-CNT
696100000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
696200000000               MOVE "Y"                TO  WS-SELPRT-SW.
696300000000
696400981207           IF  WS-STMT-DTA-SCNT-60
696500981207                          IS GREATER THAN  ZEROES
696600981207               MOVE SPACES             TO  CONTROL-LINE-1
696700981207               MOVE WS-STMT-DTA-SLIT-60
696800981207                                       TO  CL1-LIT
696900981207               MOVE WS-STMT-DTA-SCNT-60
697000981207                                       TO  CL1-CNT
697100981207               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
697200981207               MOVE "Y"                TO  WS-SELPRT-SW.
697300981207
697400000000           IF  WS-STMT-DTA-SCNT-80
697500000000                          IS GREATER THAN  ZEROES
697600000000               MOVE SPACES             TO  CONTROL-LINE-1
697700000000               MOVE WS-STMT-DTA-SLIT-80
697800000000                                       TO  CL1-LIT
697900000000               MOVE WS-STMT-DTA-SCNT-80
698000000000                                       TO  CL1-CNT
698100000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
698200000000               MOVE "Y"                TO  WS-SELPRT-SW.
698300000000
698400000000           IF  WS-SELPRT-SW   IS EQUAL TO  "Y"
698500000000               MOVE SPACES             TO  CONTROL-LINE-1
698600000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
698700000000
698800000000           MOVE SPACES                 TO  CONTROL-LINE-1.
698900000000           MOVE WS-STMT-PRT-HDR-WLIT   TO  CL1-LIT.
699000000000           MOVE WS-STMT-PRT-HDR-WCNT   TO  CL1-CNT.
699100000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
699200000000
699300000000           MOVE SPACES                 TO  CONTROL-LINE-1.
699400000000           MOVE WS-STMT-PRT-DTL-WLIT   TO  CL1-LIT.
699500000000           MOVE WS-STMT-PRT-DTL-WCNT   TO  CL1-CNT.
699600000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
699700000000
699800000000      *****
699900000000      *****    Close all the files.
700000000000      *****
700100000000
700200000000           CLOSE           DEALER-MAILING-INSTR
700300000000                           DEALER-FILE
700400000000                           DEALER-REP-FILE
700500000000                           DEALER-REP-MAILING-INSTR
700600000000                           DEALER-REP-ADDRESS
700700000000                           INSTITUTION-ADDRESS
700800000000                           INSTITUTION-BRANCH
700900980326                           INVESTOR-JOIN
701000980326                           NOMINEE-ACCOUNT-TYPE-DESCR
701100000000                           NOMINEE-MAILING-INSTRUCTIONS
701200020920R14588                     DEALER-MISC-DETAILS
701300020920R14588                     MASTER-ACCOUNT
701400000000                           STMT-MSG-FILE
701500000000                           STMT-PRT-DTL-FILE
701600000000                           STMT-PRT-HDR-FILE
701700991103991103                     ACCOUNT-TYPE-BK
701800000000                           STMT-DTA-FILE
701900990116                           PRINT-FILE
702000190612      *RFS183449-Starts
702100190612      *                    ACCOUNT-FILE.
702200190612                           ACCOUNT-FILE
702300190612                           ACCOUNT-FILE-2.
702400190612      *RFS183449-Ends
702500000000
702600000000       EJR-EXIT.
702700000000           EXIT.
702800000000      /
702900000000       PRINT-CTL-LINE.
703000000000
703100000000      ******************************************************************
703200000000      *                                                                *
703300000000      *    P R I N T   C O N T R O L   L I N E   R O U T I N E         *
703400000000      *                                                                *
703500000000      ******************************************************************
703600000000
703700000000      *****
703800000000      *****    If the page is full, start a new page and print the
703900000000      *****    Heading lines.
704000000000      *****
704100000000
704200000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
704300000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
704400000000
704500000000      *****
704600000000      *****    Print the Control line.
704700000000      *****
704800000000
704900000000           WRITE PRINT-RECD          FROM  CONTROL-LINE-1
705000000000                         AFTER  ADVANCING  1.
705100000000           ADD 1                       TO  WS-LNECNT.
705200000000
705300000000       PCL-EXIT.
705400000000           EXIT.
705500000000      /
705600000000       PRINT-HDG-LINE.
705700000000
705800000000      ******************************************************************
705900000000      *                                                                *
706000000000      *    P R I N T   H E A D I N G   L I N E   R O U T I N E         *
706100000000      *                                                                *
706200000000      ******************************************************************
706300000000
706400000000      *****
706500000000      *****    Print Heading line 1 on a new page.
706600000000      *****
706700000000
706800000000           WRITE PRINT-RECD          FROM  HEADING-LINE-1
706900000000                          AFTER ADVANCING  PAGE.
707000000000           ADD 1                       TO  WS-PGECNT.
707100000000           MOVE 1                      TO  WS-LNECNT.
707200000000
707300000000      *****
707400000000      *****    Format and print Heading line 2.
707500000000      *****
707600000000
707700000000           MOVE WS-PGECNT              TO  HL2-PGECNT.
707800000000
707900000000           WRITE PRINT-RECD          FROM  HEADING-LINE-2
708000000000                          AFTER ADVANCING  1.
708100000000           ADD 1                       TO  WS-LNECNT.
708200000000
708300000000      *****
708400000000      *****    Print Heading line 3, if required.
708500000000      *****
708600000000
708700000000           IF  WS-EOJHDG-SW   IS EQUAL TO  "Y"
708800000000               GO TO PHL-0010.
708900000000
709000000000           WRITE PRINT-RECD          FROM  HEADING-LINE-3
709100000000                          AFTER ADVANCING  2.
709200000000           ADD 2                       TO  WS-LNECNT.
709300000000
709400000000       PHL-0010.
709500000000
709600000000      *****
709700000000      *****    Format and print a blank line.
709800000000      *****
709900000000
710000000000           MOVE SPACES                 TO  PRINT-RECD.
710100000000           WRITE PRINT-RECD
710200000000                          AFTER ADVANCING  1.
710300000000           ADD 1                       TO  WS-LNECNT.
710400000000
710500000000       PHL-EXIT.
710600000000           EXIT.
710700990116      /
710800990116       GET-DEALER-ACCOUNT.
710900990116           INITIALIZE MFAACCNTP OF ACCOUNT-REC.
711000990116           MOVE CURR-ACCOUNT
711100020611                    TO  ACCOUNT-NO OF ACCOUNT-REC
711200990116
711300990116           READ ACCOUNT-FILE
711400990116               INVALID KEY
711500990116                DISPLAY "INVALID DEALER ACCOUNT NO"
711600990116                GO TO GDA-EXIT.
711700990116
711800990116           MOVE DEALER-ACCOUNT-NO OF ACCOUNT-REC
711900990116                TO IS-DEALER-ACCOUNT-NO.
712000990116      /
712100990116       GDA-EXIT.
712200990116           EXIT.
712300190612
712400190612      *RFS-183449-Starts
712500190612       GET-ACCOUNT-TYPE-CODE.
712600190612           INITIALIZE MFAACCNTP OF ACCOUNT-REC-2.
712700190612           MOVE AD-ACCOUNT
712800190612                    TO  ACCOUNT-NO OF ACCOUNT-REC-2
712900190612
713000190612           READ ACCOUNT-FILE-2
713100190619             INVALID KEY
713200190619             DISPLAY "Account File 2 not found for Account:" AD-ACCOUNT
713300190619             GO TO GAT-EXIT.
713400190612
713500190612           MOVE ACCOUNT-TYPE-CODE OF ACCOUNT-REC-2
713600190612                TO Lc-Acct-Type-Code.
713700190619       GAT-EXIT.
713800190612           EXIT.
713900190612      *RFS183449-Ends
714000190612
