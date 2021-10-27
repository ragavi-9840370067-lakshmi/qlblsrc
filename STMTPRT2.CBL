000100170313      * %ATTR OPTION(*LSTDBG) TGTRLS(V7R1M0)
000101888888      *%CSTD===========================================================*
000102888888      ** Application. : UTC        Unitrax Common Programs             *
000103888888      ** Component. . : STMTPRT2                      Type: CBL        *
000104888888      **===============================================================*
000105888888      ** Sub-system . :                                                *
000106888888      ** Function . . :                                                *
000107888888      ** Sub-function :                                                *
000108888888      **%S=============================================================*
000109888888      ** Description of functions:                                     *
000111888888      **                                                               *
000112888888      **                                                               *
000113190722      **                                                               *
000114888888      **%E=============================================================*
000115888888      ** AUTHOR:    ARCAD_PGMR 03/14/2017 18:24  Y18.4.BS              *
000116888888      ** MODIFS: ** ARCAD_PGMR 06/19/2019   :    Y18.4.BS    00/       *
000117888888      **         01 AK59489DV  06/19/2019 12:10  XR5ARTK     00/       *
000118888888      **         ** US04880DM  06/19/2019   :    C17.1.BS    00/       *
000119888888      *%ECSTD==========================================================*
000290000000       IDENTIFICATION DIVISION.
000300010427       PROGRAM-ID.     STMTPRT2.
000310000000       INSTALLATION.   Jewelstone Systems Inc.
000320000000       AUTHOR.         Jewelstone Systems Inc.
000330000000       DATE-WRITTEN.   November 25, 1996.
000340000000       DATE-COMPILED.
000350000000
000360000000      ******************************************************************
000370000000      *                                                                *
000380010427      *    STMTPRT2:   Statement Print - Standard Form                 *
000390000000      *                                                                *
000400000000      *    PURPOSE:    To format and print statements using the        *
000410000000      *                standard format and the Jewelstone Statement    *
000420000000      *                file (MFASTMTFP).                               *
000430000000      *                                                                *
000440000000      *    LEVELS:      1. INVESTOR-NO                                 *
000450000000      *                 2. ACCOUNT                                     *
000460000000      *                                                                *
000470980326      *    FILES:      (I) MFAACOJLA   Investor Join
000480980326      *                (I) MFADEAMIP   Dealer Mailing Instruction      *
000490000000      *                (I) MFADEAP     Dealer                          *
000500000000      *                (I) MFADEAREP   Dealer Representative           *
000510000000      *                (I) MFADERMIP   Dealer Representative Mailing   *
000520000000      *                                Instruction                     *
000530000000      *                (I) MFADRADRP   Dealer Representative Address   *
000540000000      *                (I) MFAINADDP   Institution Address             *
000550000000      *                (I) MFAINSBRP   Institution Branch              *
000560980326      *                (I) MFANMTPYP   Nominee Account Type Descr
000570991104991103*                (I) MFAACCTYP   Account Type BK
000580000000      *                (I) MFANOMMIP   Nominee Mailing Instruction     *
000590000000      *                (I) MFASTMMSP   Statement Message               *
000600990116      *                (I) MFAACCNTP   Account
000610980116      *                (O) MFASTMPDP   Statement Print Detail          *
000620980116      *                (O) MFASTMPHP   Statement Print Header          *
000630000000      *                (I) MFASTMEXP   Statement Extract Data          *
000640000000      *                (O) PRINTER     Control Report                  *
000650000000      *                                                                *
000660000000      *    CREATED:    23/DEC/96 (MFH)                                 *
000670980218      *    CHANGED:    18/FEB/98 (MFH)                                 *
000680980202      *                                                                *
000690000000      ******************************************************************
000700000000      /
000710000000      ******************************************************************
000720000000      *                                                                *
000730000000      *    C H A N G E   H I S T O R Y                                 *
000740000000      *                                                                *
000750000000      ******************************************************************
000760000000      ******************************************************************
000770000000      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE              *
000780000000      ******************************************************************
000790010130      * B.LU       * 2001/01/30   * RFS 9298 9184                      *
000800010130      *            *              * - AT-GST. AT-PST and AT-ADMIN-FEES *
000810010130      *            *              *   were added to
000820010130      *            *              *  DEDUCTION that was missed before. *
000830030529      ******************************************************************
000840010309      * B.LU       * 2001/03/09   * RFS 8633                           *
000850010309      *            *              * -IS-TRI/TRO-CURRENT contain both   *
000860010309      *            *              *  TRI&SWI/TRO&SWO values before.
000870010309      *            *              *  In RFS8633, they were seprated.   *
000880010313      *            *              * -Added Section 12 and bypass it.
000890030529      ******************************************************************
000900010418      * K.LIU      * 2001/04/18   * RFS 10114
000910010418      *            *              * -The 'TOTAL PLAN' value and the
000920010418      *            *              *  'ACCUMULATED VALUE OF YOUR PLAN'
000930010418      *            *              *  were correctly populated by
000940010418      *            *              *  enlarging field size.
000950030529      ******************************************************************
000960010419      * K.LIU      * 2001/04/19   * RFS 9415
000970010419      *            *              * -Added separate spool file for
000980010419      *            *              *  nominee accounts.
000990010424      *            *              * -Wording corrections
001000030529      ******************************************************************
001010010515      * B.LU       * 2001/05/15   * RFS 10243
001020010515      *            *              * -ANNUITY DATE will not be populate
001030010515      *            *              *  for LIF, LRIF, and RRIF account.
001040010528      *            * 2001/05/28   * -Initialize WS-ANNUITANT-NAME.
001050030529      ******************************************************************
001060010912      * B.LU       * 2001/09/10   * RFS11175
001070010912      *            *              * -Added a new colunm to Statement
001080010912      *            *              *  Date Account Position section;
001090010912      *            *              * -Added a new line to Plan Summary
001100010912      *            *              *  section 2;
001110010912      *            * 2001/09/14   * -Other changes as in RFS11175.
001120030529      ******************************************************************
001130011105      * B.Lu       * 2001/10/22   * RFS10901, 10605,11127
001140011022      *            *              * -Recompile only.
001150030529      ******************************************************************
001160011211      * A.ALNEMER  * 2001/11/23   * RFS12150
001170011211      *            *              * -Recompile only.
001180030529      ******************************************************************
001190011206      * B.LU       * 2001/12/06   * RFS12327, 12338
001200011206      *            *              * -Spelling erros of French words
001210011206      *            *              *  were fixed.
001220030529      ******************************************************************
001230020111      * ALI.A      * 2001/12/11   * RFS12090
001240011211      *            *              * -Increae the size of the array that
001250011211      *            *              *  store and process the account
001260011211      *            *              *  detail information
001270030529      ******************************************************************
001280020402      * B.LU       * 2002/01/30   * RFS12102
001290020402      *            *              * -Recompile only.
001300030529      ******************************************************************
001310020111      * ALI.A      * 2002/01/11   * RFS12173
001320020213      *            *              * - Check the sign values for        *
001330020111      *            *              *   transfer and switches fields     *
001340020111      *            *              *   in section 10.                   *
001350030529      ******************************************************************
001360020213      * ALI.A      * 2002/02/13   * RFS12726
001370020213      *            *              * - Check the sign values for the    *
001380020213      *            *              *   Investor Summery and Account     *
001390020213      *            *              *   Details, sections 10 and 20      *
001400030529      ******************************************************************
001410020404      * ALI.A      * 2002/04/04   * RFS13184
001420020404      *            *              * - Recompile only                   *
001430030529      ******************************************************************
001440020416      * ALI.A      * 2002/04/16   * RFS13211
001450020416      *            *              * - Recompile only                   *
001460030529      ******************************************************************
001470020612      * A.ALNEMER  * 2002/06/11   * RFS13756
001480020612      *            *              * - If the Master Account number     *
001490020612      *            *              *   IS-MASTER-ACCOUNT-NO in section  *
001500020612      *            *              *   10 from the statement extract    *
001510020612      *            *              *   is populated, use it for         *
001520020612      *            *              *   for reporting and display        *
001530020612      *            *              *   instead of the Account-No.       *
001540030529      ******************************************************************
001550020815      * FATEMA HAJI*  2002/07/29  * RFS 14470 - RECOMPILE FOR
001560020815      *            *              * INSTITUTION-BRANCH FILE CHANGE.
001570030529      ******************************************************************
001580020801      * A.ALNEMER  * 2002/08/01   * RFS13637
001590020806      *            *              * - Subtract the SWO "SWITCH OUT" and*
001600020806      *            *              *   Add the SWI "SWITCH IN" to the   *
001610020806      *            *              *   NET INVESTED field.              *
001620030529      ******************************************************************
001630020910      * A.ALNEMER  * 2002/08/21   * RFS13955
001640020910      *            *              * - Recompile only.                  *
001650030529      ******************************************************************
001660020910      * A.ALNEMER  * 2002/08/21   * RFS14136
001670020910      *            *              * - Recompile only.                  *
001680030529      ******************************************************************
001690020904      * B.LU       * 2002/09/04   * RFS14493
001700020904      *            *              * -ROR Plan Performance description
001710020904      *            *              *  were changed.
001720030529      ******************************************************************
001730020921      * A.ALNEMER  * 2002/09/19   * RFS14588
001740020921      *            *              * - If the Child-account is linked   *
001750020921      *            *              *   to the Master-Account file then  *
001760020921      *            *              *   display the Master Account.      *
001770030529      ******************************************************************
001780020930      * B.LU       * 2002/09/25   * RFS14556  Recompile.
001790030529      ******************************************************************
001800021022      * B.LU       * 2002/10/22   * RFS14329
001810021022      *            *              * -Fixed the alignment problem when
001820021022      *            *              *  more than 23 Funds in an account.
001830030529      ******************************************************************
001840021202      * A.ALNEMER  * 2002/12/02   * RFS15022
001850021202      *            *              * - Recompile only.                  *
001860030529      ******************************************************************
001870021213      * B.LU       * 2002/12/13   * RFS15028                           *
001880021213      *            *              * -The Account Statement Message will*
001890021213      *            *              *  be reported on the bases of New   *
001900021213      *            *              *  Investor or not.
001910030529      ******************************************************************
001920030106      * B.LU       * 2003/01/06   * RFS16015
001930030106      *            *              * -Fix the spelling mistakes.
001940030529      ******************************************************************
001950030224      * C.LENTINI  * 2003/02/24   * RFS 16381 - Recompile
001960030529      ******************************************************************
001970030319      * B.LU       * 2003/03/19   * RFS16256  RECOMPILE.
001980030529      ******************************************************************
001990030325      * B.LU       * 2003/03/25   * RFS16633  RECOMPILE.
002000030529      ******************************************************************
002010030328      * ALNEMER.A  * 2003/27/06   * RFS16798
002020030328      *            *              * -Fix the spelling mistakes.
002030030529      ******************************************************************
002040030509      * PING FAN   * 2003/05/09   * RFS17050  RECOMPILE.
002050000000      ******************************************************************
002060030623      * O.McMillan * 2003/06/17   * RFS17217  RECOMPILE.
002070030623      ******************************************************************
002080030529      * Odette     * 2003/05/08   * RFS17165 - MFASTMEXP expanded from
002090030623      * McMillan   *              * 1100 to 1350. Change rec lengths.
002100030529      ******************************************************************
002110030624      * Odette     * 2003/06/04   * RFS16881 - Recompile.
002120030624      * McMillan   *              *
002130030822      ******************************************************************
002140030626      * Brian Lu   * 2003/06/23   * RFS17816                           *
002150030626      *            *              * -Fixed the alignment problem: there
002160030626      *            *              *  is no limt to the numbers of Funds
002170030626      *            *              *  in an Account.
002180030624      ******************************************************************
002190030822      * Odette     * 2003/08/22   * RFS18091  Recompile for addition
002200030822      * McMillan   *              * of Section 17 to CPYJSISTMT.
002210030822      ******************************************************************
002220030825      * Odette     * 2003/08/25   * RFS17380  Recompile for additional
002230030825      * McMillan   *              * fields in Section 05 of CPYJSISTMT.
002240030825      ******************************************************************
002250031003      * Odette     * 2003/09/22   * RFS 18653 - Recompile.
002260031003      * McMillan   *              * Moved Section 17 rec counter field
002270031003      *            *              * in Section 99 to bottom of rec.
002280031003      ******************************************************************
002290031002      * B.LU       * 2003/10/02   * RFS17527  Recompile.
002300031002      ******************************************************************
002310031015      * ALI.A      * 2003/10/15   * RFS17626  Recompile.
002320031015      ******************************************************************
002330031020      * ALI.A      * 2003/10/20   * RFS18667  Recompile.
002340031020      ******************************************************************
002350031021      * ALI.A      * 2003/10/21   * RFS18312  Recompile.
002360031021      ******************************************************************
002370031121      * ALI.A      * 2003/11/21   * RFS18515  Recompile.
002380031126      *****************************************************************
002390031126      * B.LU       * 2003/11/21   * RFS19248  RECOMPILE.
002400031121      ******************************************************************
002410031216      * B.LU       * 2003/12/16   * RFS18814  RECOMPILE.
002420031216      ******************************************************************
002430040217      * ALI.A      * 2004/02/17   * RFS19444  RECOMPILE.
002440040217      ******************************************************************
002450040414      * ALI.A      * 2004/02/14   * RFS20758  RECOMPILE.
002460040414      ******************************************************************
002470040514      * Odette     *  2004/04/27  * RFS21714 - Recompile
002480040514      * Aguinaldo- *              * Add new fields to Section 10 to
002490040514      * McMillan   *              * indicate if Dealer, Rep or Nominee
002500040514      *            *              * is/isn't suppressed.
002510040531      ******************************************************************
002520040531      * Reece Tam  * 2004/05/31   * RFS20804  Recompile - CPYLADASTMT.
002530040514      ******************************************************************
002540040908      * B.LU       * 2004/09/08   * RFS18924 -RECOMPILE - CPYJSISTMT.
002550040908      ******************************************************************
002560041012      * B.LU       * 2004/10/12   * RFS22856 -RECOMPILE - CPYJSISTMT.
002570041012      ******************************************************************
002580041025      * ALI.A      * 2004/10/25   * RFS18780 -RECOMPILE - CPYJSISTMT.
002590041025      ******************************************************************
002600041028      * ALI.A      * 2004/10/28   * RFS21715 -RECOMPILE - CPYJSISTMT.
002610041028      ******************************************************************
002620041220      * B.LU       * 2004/12/20   * RFS24514 -RECOMPILE - CPYJSISTMT
002630041220      ******************************************************************
002640050411      * B.LU       * 2005/04/11   * RFS26894 - Added AT-ERF to
002650050411      *            *              *            WS-DEDUCTIONS
002660050411      ******************************************************************
002670050429      * CHANDANA R * 2005/03/09   * RFS26213 -RECOMPILE - CPYJSISTMT
002680050429      ******************************************************************
002690051002      * ADE ADEYEMI* 2005/09/08   * RFS29550 -RECOMPILE - CPYJSISTMT
002700051002      ******************************************************************
002710051002      * ADE ADEYEMI* 2005/09/28   * RFS30065 -RECOMPILE - CPYJSISTMT
002720051002      ******************************************************************
002730050920      * Ali.A      * 2005/09/18   * RFS28931 -RECOMPILE - CPYJSISTMT
002740050920      ******************************************************************
002750050926      * C.LENTINI  * 2005/09/26   * RFS28692 -RECOMPILE - CPYJSISTMT
002760050926      ******************************************************************
002770050926      * Geeta S.   * 2005/09/15   * RFS 28107: Recompile for CPYJSISTMT
002780051110      *            *              *
002790051110      * Lev O      * 2005/11/10   * RFS30210 - The Program Prints
002800051110      *            *              * wrong ROR data. Solution:
002810051110      *            *              * Fix the program to print correct
002820051110      *            *              * ROR information.
002830060628      * Esin A.    * 2006/03/23   * RFS 31514 - Recompile only
002840060628      ******************************************************************
002850050926      ******************************************************************
002860060628      * B.LU       * 2006/06/28   * RFS35042 -Process Section 21
002870060821      ******************************************************************
002880060821      * Raymond Mui* 2006/08/21   * RFS34848 -Recompile - CPYJSISTMT
002890060821      *            *              *            and CPYLDASTMT.
002900060618      ******************************************************************
002910061024      * L. Wong    * 2006/10/24   * RFS36342 - Recompile - CPYJSISTMT,
002920061024      *            *              *                        CPYLDASTMT.
002930061027      *            *              * - Fix level 20 errors: remove VALUE
002940061027      *            *              *   clause in REDEFINE clause.
002950061027      *            *              * - Fix level 10 errors: delimiters -
002960061027      *            *              *   use quote instead of apostrophe.
002970061024      ******************************************************************
002980070625      * Lorne B    * 2007/06/25   * RFS 42682 - eGL Extract; Recompile
002990080620      ******************************************************************
003000080620      * Bathuu L   * 2008/06/20   * RFS50993 - RECOMPILE FOR CPYJSISTMT
003010070625      ******************************************************************
003020080903      * Kevin Shen * 2008/09/03   * RFS52366 - Add account type RLIF
003030080903      ******************************************************************
003040090824      * Tony Valeri* 2009/08/24   * RFS71771 - increase array size
003050091126      ******************************************************************
003060091126      * A. Lo      * 2009/11/11   * RFS69515 - Recompile - CPYJSISTMT,
003070091126      *            *              *                        CPYLDASTMT.
003080090824      ******************************************************************
003090100308      * A. Lo      * 2010/03/08   * RFS78161 -Recompile - CPYJSISTMT.
003100100308      ***************************************************************
003110100409      * A. Lo      * 2010/03/08   * RFS74626 -Recompile - CPYJSISTMT.
003120110805      * A Lo       * 2011/08/05   * RFS 91768 - Recompile.          *
003130100308      ***************************************************************
003140120127      * Rekha S    * 2012/01/27   * RFS93662 -Recompile - CPYJSISTMT.
003150120912      *****************************************************************
003160120912      * Suresh G.  * 2012/09/12   * RFS112003-Recompile for CPYJSISTMT.
003170120912      *****************************************************************
003180130408      * Andy Chan  * 2013/04/08   * RFS117038-Add Section 22
003190130408      *****************************************************************
003200130529      * NOBLE T .  * 2013/06/29   * RFS121116-RECOMPILE FOR CPYLDASTMT.
003210130702      * K Naveen   * 2013/07/02   * RFS123719 -  Recompile Only.
003220130529      *****************************************************************
003230130905      * PRAVEEN E  * 2013/09/05   *RFS123925 RECOMPIL ONLY - CPYJSISTMT.
003240140506      * NOBLE T    * 2014/05/06   * RFS130239 - RECOMPILE - CPYJSISTMT.
003250140610      * Antony S   * 2014/06/10   * RFS133528 - GGF Production Ad-hoc
003260140610      *            *              * statements truncated numbers
003270130905      *****************************************************************
003280140826      *SANGEETHA V  * 2014/03/25 * RFS129378 - RECOMPILE FOR CPYJSISTMT*
003290140826      * JEYARAJAN  * 2014/06/24   * RFS132121 - INTRODUCE A NEW FIELD  *
003300140826      *            *              * TO HANDLE THE ERROR ON SEC 16      *
003310141029      *****************************************************************
003320141007      * SURESH G.  * 2014/08/14   * RFS133489-RECOMPILE FOR CPYJSISTMT.
003330141007      * JEYARAJAN  * 2014/10/07   * RFS141165 - Jobconfirm MSW         *
003340150224      * POOJA K    * 2015/02/16  * RFS143392 - RECOMPILE FOR CPYJSISTMT*
003350150424      * POOJA K    * 2015/03/27  * RFS144030 - RECOMPILE FOR CPYJSISTMT*
003360150424      * POOJA K    * 2015/04/21  * RFS142884 - RECOMPILE FOR CPYJSISTMT*
003370150511      * POOJA K    * 2015/05/08  * RFS144587 - RECOMPILE FOR CPYJSISTMT*
003380150617      * SANGEETHA V * 2015/05/06  * RFS 147149 - RECOMPILE.            *
003390150918      * KAHAKASHAN * 2015/09/18  * RFS 149112 - AVERAGE COST TRUNCATED *
003400150918      *                          * IN SPOOL FILES                      *
003410161012      * POOJA K    * 2016/08/31  * RFS156599 - ACCOUNT HOLDING MORE    *
003420161012      *            *             * THEN 500 INVESTMENTS NOT GETTING    *
003430161012      *            *             * REPORTED IN THE EXTRACT             *
003440161108      * ARTHY K    * 2016/10/28  * RFS164194 - ACCOUNT HOLDING MORE    *
003450161108      *            *             * THEN 2000 INVESTMENTS NOT GETTING   *
003460161108      *            *             * REPORTED IN THE EXTRACT             *
003470190619      * MAYILSAMY D* 2019/06/18   * RFS183449 - POPULATE ACCOUNT TYPE  *
003480190619      *            *              * CODE BASED UPON INVESTOR LANGUAGE  *
003490190905      *            *              * IN STATEMENT EXTRACT               *
003491190924      * Arthy K    * 2019/09/03   * RFS1018431 - Rollback changes done *
003492190924      *            *              * in RFS: 183449                     *
003493190924      * Arthy K    * 2019/09/24   * RFS184715 - Put back changes in RFS*
003494190924      *            *              * 183449 with additional logic of    *
003495190924      *            *              * new edit code 'STMATL'             *
003496200704      * Mayilsamy D* 2020/07/04   * RFS186092 - Recompile              *
003497211019      * Surendran C* 2021/10/19   * RFS186592 -RECOMPILE FOR CPYJSISTMT*
003500140826      *****************************************************************
003510120127
003520000000      /
003530000000       ENVIRONMENT DIVISION.
003540000000
003550000000      ******************************************************************
003560000000      *                                                                *
003570000000      *    E N V I R O N M E N T   D I V I S I O N                     *
003580000000      *                                                                *
003590000000      ******************************************************************
003600000000
003610000000       CONFIGURATION SECTION.
003620000000
003630000000      ******************************************************************
003640000000      *                                                                *
003650000000      *    C O N F I G U R A T I O N   S E C T I O N                   *
003660000000      *                                                                *
003670000000      ******************************************************************
003680000000
003690000000       SOURCE-COMPUTER.    AS-400.
003700000000       OBJECT-COMPUTER.    AS-400.
003710000000
003720000000       SPECIAL-NAMES.
003730000000
003740000000      *****
003750000000      *****    Special Names.
003760000000      *****
003770000000
003780000000           LOCAL-DATA          IS  WS-LOCAL.
003790000000      /
003800000000       INPUT-OUTPUT SECTION.
003810000000
003820000000      ******************************************************************
003830000000      *                                                                *
003840000000      *    I N P U T   /   O U T P U T   S E C T I O N                 *
003850000000      *                                                                *
003860000000      ******************************************************************
003870000000
003880000000       FILE-CONTROL.
003890000000
003900000000      *****
003910000000      *****    FILE CONTROL.
003920000000      *****
003930000000      /
003940990116           SELECT  ACCOUNT-FILE
003950990116                   ASSIGN          TO  DATABASE-MFAACCNTP
003960990116                   ORGANIZATION    IS  INDEXED
003970990116                   ACCESS          IS  DYNAMIC
003980990116                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
003990990116
004000190612      *RFS183449-Starts
004010190612           SELECT  ACCOUNT-FILE-2
004020190612                   ASSIGN          TO  DATABASE-MFAACCNTP
004030190612                   ORGANIZATION    IS  INDEXED
004040190612                   ACCESS          IS  DYNAMIC
004050190612                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
004060190612      *RFS183449-Ends
004070190612
004080000000           SELECT DEALER-MAILING-INSTR
004090000000               ASSIGN          TO  DATABASE-MFADEAMIP
004100000000               ORGANIZATION    IS  INDEXED
004110000000               ACCESS          IS  DYNAMIC
004120000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004130000000               FILE STATUS     IS  WS-FILSTAT.
004140000000
004150000000           SELECT DEALER-FILE
004160000000               ASSIGN          TO  DATABASE-MFADEAP
004170000000               ORGANIZATION    IS  INDEXED
004180000000               ACCESS          IS  RANDOM
004190000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004200000000               FILE STATUS     IS  WS-FILSTAT.
004210000000
004220000000           SELECT DEALER-REP-FILE
004230000000               ASSIGN          TO  DATABASE-MFADEAREP
004240000000               ORGANIZATION    IS  INDEXED
004250000000               ACCESS          IS  RANDOM
004260000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004270000000               FILE STATUS     IS  WS-FILSTAT.
004280000000
004290000000           SELECT DEALER-REP-MAILING-INSTR
004300000000               ASSIGN          TO  DATABASE-MFADERMIP
004310000000               ORGANIZATION    IS  INDEXED
004320000000               ACCESS          IS  DYNAMIC
004330000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004340000000               FILE STATUS     IS  WS-FILSTAT.
004350980326
004360000000           SELECT DEALER-REP-ADDRESS
004370000000               ASSIGN          TO  DATABASE-MFADRADRP
004380000000               ORGANIZATION    IS  INDEXED
004390000000               ACCESS          IS  RANDOM
004400000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004410000000               FILE STATUS     IS  WS-FILSTAT.
004420000000
004430000000           SELECT INSTITUTION-ADDRESS
004440000000               ASSIGN          TO  DATABASE-MFAINADDP
004450000000               ORGANIZATION    IS  INDEXED
004460000000               ACCESS          IS  RANDOM
004470000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004480000000               FILE STATUS     IS  WS-FILSTAT.
004490000000
004500000000           SELECT INSTITUTION-BRANCH
004510000000               ASSIGN          TO  DATABASE-MFAINSBRP
004520000000               ORGANIZATION    IS  INDEXED
004530000000               ACCESS          IS  RANDOM
004540000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004550000000               FILE STATUS     IS  WS-FILSTAT.
004560980326
004570980326           SELECT  INVESTOR-JOIN
004580980326                   ASSIGN          TO  DATABASE-MFAACOJLA
004590980326                   ORGANIZATION    IS  INDEXED
004600980326                   ACCESS          IS  DYNAMIC
004610980326                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004620980326                                 WITH  DUPLICATES.
004630980326           SELECT NOMINEE-ACCOUNT-TYPE-DESCR
004640980326               ASSIGN          TO  DATABASE-MFANMTPYP
004650980326               ORGANIZATION    IS  INDEXED
004660980326               ACCESS          IS  DYNAMIC
004670980326               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
004680991103
004690991103           SELECT ACCOUNT-TYPE-BK
004700991103               ASSIGN          TO  DATABASE-MFAACCTYP
004710991103               ORGANIZATION    IS  INDEXED
004720991103               ACCESS          IS  SEQUENTIAL
004730991103               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
004740991103
004750000000           SELECT NOMINEE-MAILING-INSTRUCTIONS
004760000000               ASSIGN          TO  DATABASE-MFANOMMIP
004770000000               ORGANIZATION    IS  INDEXED
004780000000               ACCESS          IS  DYNAMIC
004790000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004800000000               FILE STATUS     IS  WS-FILSTAT.
004810000000
004820020921R14588     SELECT MASTER-ACCOUNT
004830020921               ASSIGN          TO  DATABASE-MFAMACCP
004840020921               ORGANIZATION    IS  INDEXED
004850020921               ACCESS          IS  DYNAMIC
004860020921               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
004870020921
004880020921R14588     SELECT DEALER-MISC-DETAILS
004890020921               ASSIGN          TO  DATABASE-MFADEAMDP
004900020921               ORGANIZATION    IS  INDEXED
004910020921               ACCESS          IS  DYNAMIC
004920020921               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
004930020921
004940000000           SELECT STMT-MSG-FILE
004950000000               ASSIGN          TO  DATABASE-MFASTMMSP
004960000000               ORGANIZATION    IS  INDEXED
004970000000               ACCESS          IS  DYNAMIC
004980000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
004990000000               FILE STATUS     IS  WS-FILSTAT.
005000000000
005010000000           SELECT STMT-PRT-DTL-FILE
005020000000               ASSIGN          TO  DATABASE-MFASTMPDP
005030000000               ORGANIZATION    IS  INDEXED
005040000000               ACCESS          IS  DYNAMIC
005050000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
005060000000               FILE STATUS     IS  WS-FILSTAT.
005070000000
005080000000           SELECT STMT-PRT-HDR-FILE
005090000000               ASSIGN          TO  DATABASE-MFASTMPHP
005100000000               ORGANIZATION    IS  INDEXED
005110000000               ACCESS          IS  DYNAMIC
005120000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
005130000000               FILE STATUS     IS  WS-FILSTAT.
005140000000
005150000000           SELECT STMT-DTA-FILE
005160000000               ASSIGN          TO  DATABASE-MFASTMEXP
005170000000               ORGANIZATION    IS  INDEXED
005180000000               ACCESS          IS  SEQUENTIAL
005190000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
005200000000               FILE STATUS     IS  WS-FILSTAT.
005210000000
005220000000           SELECT PRINT-FILE
005230000000               ASSIGN          TO  PRINTER-QPRINT.
005240000000      /
005250000000       DATA DIVISION.
005260000000
005270000000      ******************************************************************
005280000000      *                                                                *
005290000000      *    D A T A   D I V I S I O N                                   *
005300000000      *                                                                *
005310000000      ******************************************************************
005320000000
005330000000       FILE SECTION.
005340000000
005350000000      ******************************************************************
005360000000      *                                                                *
005370000000      *    F I L E   S E C T I O N                                     *
005380000000      *                                                                *
005390000000      ******************************************************************
005400990116      /
005410990116       FD  ACCOUNT-FILE
005420990116           LABEL RECORDS      ARE  STANDARD.
005430990116
005440990116       01  ACCOUNT-REC.                COPY DD-ALL-FORMATS
005450990116                                            OF MFAACCNTP.
005460000000      /
005470190612      *RFS183449-Starts
005480190612       FD  ACCOUNT-FILE-2
005490190612           LABEL RECORDS      ARE  STANDARD.
005500190612
005510190619       01  ACCOUNT-REC-2.
005520190619       COPY DD-ALL-FORMATS OF MFAACCNTP.
005530190612      *RFS183449-Ends
005540190612
005550000000      ******************************************************************
005560000000      *                                                                *
005570000000      *    MFADEAMIP:  Dealer Mailing Instruction                      *
005580000000      *                                                                *
005590000000      ******************************************************************
005600990116      /
005610000000       FD  DEALER-MAILING-INSTR
005620000000           LABEL RECORDS      ARE  OMITTED.
005630000000
005640000000       01  DEALER-MAILING-INSTR-RECD.  COPY DD-ALL-FORMATS
005650000000                                           OF  MFADEAMIP.
005660000000      /
005670000000      ******************************************************************
005680000000      *                                                                *
005690000000      *    MFADEAP:    Dealer                                          *
005700000000      *                                                                *
005710000000      ******************************************************************
005720000000
005730000000       FD  DEALER-FILE
005740000000           LABEL RECORDS      ARE  OMITTED.
005750000000
005760000000       01  DEALER-RECD.                COPY DD-ALL-FORMATS
005770000000                                           OF  MFADEAP.
005780000000      /
005790000000      ******************************************************************
005800000000      *                                                                *
005810000000      *    MFADEAREP:  Dealer Representative                           *
005820000000      *                                                                *
005830000000      ******************************************************************
005840000000
005850000000       FD  DEALER-REP-FILE
005860000000           LABEL RECORDS      ARE  OMITTED.
005870000000
005880000000       01  DEALER-REP-RECD.            COPY DD-ALL-FORMATS
005890000000                                           OF  MFADEAREP.
005900000000      /
005910000000      ******************************************************************
005920000000      *                                                                *
005930000000      *    MFADERMIP:  Dealer Representative Mailing Instruction       *
005940000000      *                                                                *
005950000000      ******************************************************************
005960000000
005970000000       FD  DEALER-REP-MAILING-INSTR
005980000000           LABEL RECORDS      ARE  OMITTED.
005990000000
006000000000       01  DLRREP-MAILING-INSTR-RECD.  COPY DD-ALL-FORMATS
006010000000                                           OF  MFADERMIP.
006020000000      /
006030000000      ******************************************************************
006040000000      *                                                                *
006050000000      *    MFADRADRP:  Dealer Representative Address                   *
006060000000      *                                                                *
006070000000      ******************************************************************
006080000000
006090000000       FD  DEALER-REP-ADDRESS
006100000000           LABEL RECORDS      ARE  OMITTED.
006110000000
006120000000       01  DEALER-REP-ADDRESS-RECD.    COPY DD-ALL-FORMATS
006130000000                                           OF  MFADRADRP.
006140000000      /
006150000000      ******************************************************************
006160000000      *                                                                *
006170000000      *    MFAINADDP:  Institution Address                             *
006180000000      *                                                                *
006190000000      ******************************************************************
006200000000
006210000000       FD  INSTITUTION-ADDRESS
006220000000           LABEL RECORDS      ARE  OMITTED.
006230000000
006240000000       01  INSTITUTION-ADDRESS-RECD.   COPY DD-ALL-FORMATS
006250000000                                           OF  MFAINADDP.
006260000000      /
006270000000      ******************************************************************
006280000000      *                                                                *
006290000000      *    MFAINSBRP:  Institution Branch                              *
006300000000      *                                                                *
006310000000      ******************************************************************
006320000000
006330000000       FD  INSTITUTION-BRANCH
006340000000           LABEL RECORDS      ARE  OMITTED.
006350000000
006360000000       01  INSTITUTION-BRANCH-RECD.    COPY DD-ALL-FORMATS
006370000000                                           OF  MFAINSBRP.
006380000000      /
006390980326      ****************************************************************
006400980326      *                                                              *
006410980326      *  MFAACOJLA:    Investor Join                                 *
006420980326      *                                                              *
006430980326      ****************************************************************
006440980326
006450980326       FD  INVESTOR-JOIN
006460980326           LABEL RECORDS      ARE  STANDARD.
006470980326
006480980326       01  INVESTOR-J-REC.             COPY DD-ALL-FORMATS
006490980326                                            OF MFAACOJLA.
006500980326      /
006510980326      *****************************************************************
006520980326      *                                                               *
006530980326      *    MFANMTPYP:  Nominee Account Type Descr                     *
006540980326      *                                                               *
006550980326      *****************************************************************
006560980326
006570980326       FD  NOMINEE-ACCOUNT-TYPE-DESCR
006580980326           LABEL RECORDS ARE STANDARD.
006590980326
006600980326       01  NOMINEE-ACCOUNT-TYPE-DESCR-REC.
006610980326                               COPY DD-ALL-FORMATS OF MFANMTPYP.
006620991103
006630991103      /
006640991103       FD  ACCOUNT-TYPE-BK
006650991103           LABEL RECORDS      ARE  STANDARD.
006660991103
006670991103       01  ACCOUNT-TYPE-BK-REC.        COPY DD-ALL-FORMATS
006680991103                                            OF MFAACCTYP.
006690991103
006700000000      ******************************************************************
006710000000      *                                                                *
006720000000      *    MFANOMMIP:  Nominee Mailing Instruction                     *
006730000000      *                                                                *
006740000000      ******************************************************************
006750000000
006760000000       FD  NOMINEE-MAILING-INSTRUCTIONS
006770000000           LABEL RECORDS      ARE  OMITTED.
006780000000
006790000000       01  NOMINEE-MAILING-INST-RECD.  COPY DD-ALL-FORMATS
006800000000                                           OF  MFANOMMIP.
006810000000      /
006820020921      ******************************************************************
006830020921      *                                                                *
006840020921      *    MFADEAMDP:  DEALER-MISC-DETAILS                             *
006850020921      *                                                                *
006860020921      ******************************************************************
006870020921
006880020921R14588 FD  DEALER-MISC-DETAILS
006890020921           LABEL RECORDS ARE STANDARD.
006900020921       01  DEALER-MISC-DETAILS-REC.     COPY DD-ALL-FORMATS
006910020921                                          OF MFADEAMDP.
006920020921      /
006930020921      ******************************************************************
006940020921      *                                                                *
006950020921      *    MFAMACCP:  MASTER ACCOUNT FILE.                             *
006960020921      *                                                                *
006970020921      ******************************************************************
006980020921
006990020921R14588 FD  MASTER-ACCOUNT
007000020921           LABEL RECORDS      ARE  OMITTED.
007010020921       01  MASTER-ACCOUNT-REC.  COPY DD-ALL-FORMATS
007020020921                                             OF  MFAMACCP.
007030020921      /
007040000000      ******************************************************************
007050000000      *                                                                *
007060000000      *    MFASTMMSP:  Statement Message                               *
007070000000      *                                                                *
007080000000      ******************************************************************
007090000000
007100000000       FD  STMT-MSG-FILE
007110000000           LABEL RECORDS      ARE  OMITTED.
007120000000
007130000000       01  STMT-MSG-RECD.              COPY DD-ALL-FORMATS
007140000000                                           OF  MFASTMMSP.
007150000000      /
007160000000      ******************************************************************
007170000000      *                                                                *
007180000000      *    MFASTMPDP:  Statement Print Detail                          *
007190000000      *                                                                *
007200000000      ******************************************************************
007210000000
007220000000       FD  STMT-PRT-DTL-FILE
007230000000           LABEL RECORDS      ARE  OMITTED.
007240000000
007250000000       01  STMT-PRT-DTL-RECD.          COPY DD-ALL-FORMATS
007260000000                                           OF  MFASTMPDP.
007270000000      /
007280000000      ******************************************************************
007290000000      *                                                                *
007300000000      *    MFASTMPHP:  Statement Print Header                          *
007310000000      *                                                                *
007320000000      ******************************************************************
007330000000
007340000000       FD  STMT-PRT-HDR-FILE
007350000000           LABEL RECORDS      ARE  OMITTED.
007360000000
007370000000       01  STMT-PRT-HDR-RECD.          COPY DD-ALL-FORMATS
007380000000                                           OF  MFASTMPHP.
007390000000      /
007400000000      ******************************************************************
007410000000      *                                                                *
007420000000      *    MFASTMEXP:  Statement Extract Data                          *
007430000000      *                                                                *
007440000000      ******************************************************************
007450000000
007460000000       FD  STMT-DTA-FILE
007470000000           LABEL RECORDS      ARE  OMITTED.
007480000000
007490000000       01  STMT-DTA-RECD.              COPY DD-ALL-FORMATS
007500000000                                       OF MFASTMEXP.
007510000000      /
007520000000      ******************************************************************
007530000000      *                                                                *
007540000000      *    QPRINT:     Control Report                                  *
007550000000      *                                                                *
007560000000      ******************************************************************
007570000000
007580000000       FD  PRINT-FILE
007590000000           LABEL RECORDS      ARE  OMITTED.
007600000000
007610000000       01  PRINT-RECD                  PICTURE X(150).
007620000000      /
007630000000       WORKING-STORAGE SECTION.
007640000000
007650000000      ******************************************************************
007660000000      *                                                                *
007670000000      *    W O R K I N G   S T O R A G E   S E C T I O N               *
007680000000      *                                                                *
007690000000      ******************************************************************
007700000000      /
007710000000       01  WS-LOCAL-DATA-AREA.         COPY CPYLDASTMT.
007720000000      /
007730000000       01  WS-DEAMLGINS-REC.           COPY DD-ALL-FORMATS
007740000000                                           OF  MFADEAMIP.
007750000000      /
007760000000       01  WS-DERMLGINS-REC.           COPY DD-ALL-FORMATS
007770000000                                           OF  MFADERMIP.
007780000000      /
007790000000       01  WS-NOMMLGINS-REC.           COPY DD-ALL-FORMATS
007800000000                                           OF  MFANOMMIP.
007810000000      /
007820000000                                       COPY CPYJSISTMT.
007830030529R17165 01  WS-INVESTOR-INFO       PIC X(1350).
007840030529R17165 01  WS-INVESTOR-INFO-TEMP  PIC X(1350).
007850010604
007860010419RF9415/
007870010419      *****************************************************************
007880010419      *
007890010419      *    D E A L E R       I N F O R M A T I O N   T A B L E
007900010419      *
007910010419      *****************************************************************
007920010419       01  WS-DEALER-INFO-TBL.
007930030529R17165     03  WS-DEALER-INFO     PIC X(1350)     OCCURS 100 TIMES
007940010421                                                  INDEXED BY DI.
007950010419
007960010427RF9415/
007970010427      *****************************************************************
007980010427      *
007990010427      *    S E G    F U N D    C O N T R A C T    D E T A I L S
008000010427      *
008010010427      *****************************************************************
008020010427       01 WS-SEG-FUND-CONTRACT-TBL.
008030090824R17165*   03 WS-SEG-FUND-CONTRACT-DETAILS PIC X(1350) OCCURS 100 TIMES
008040090824R71771    03 WS-SEG-FUND-CONTRACT-DETAILS PIC X(1350) OCCURS 300 TIMES
008050010427                                                      INDEXED BY SFC.
008060010427      /
008070010427      *****************************************************************
008080010427      *
008090010427      *    S E G    F U N D    C O N T R A C T    I N V E S T M E N T
008100010427      *
008110010427      *****************************************************************
008120010427       01 WS-SEG-FUND-CONTRACT-INV-TBL.
008130090824R17165*   03 WS-SEG-FUND-CONTRACT-INV  PIC X(1350)    OCCURS 100 TIMES
008140090824R71771    03 WS-SEG-FUND-CONTRACT-INV  PIC X(1350)    OCCURS 300 TIMES
008150010427                                                      INDEXED BY SFCI.
008160010427      /
008170010427      *****************************************************************
008180010427      *
008190010427      *    A N N U I T A N T    D E T A I L S
008200010427      *
008210010427      *****************************************************************
008220010427       01 WS-ANNUITANT-DETAILS-TBL.
008230030529R17165    03 WS-ANNUITANT-DETAILS      PIC X(1350)    OCCURS 100 TIMES
008240010427RF9415                                                INDEXED BY ANN.
008250010427
008260000000      ******************************************************************
008270000000      *                                                                *
008280000000      *    S T A T E M E N T   S A V E   P A G E                       *
008290000000      *                                                                *
008300000000      ******************************************************************
008310000000
008320000000       01  WS-STATEMENT-PAGE.
008330000000           03  WS-STMTPGE-LNE          OCCURS  88  TIMES
008340000000                                       INDEXED BY  SPL
008350000000                                       PICTURE X(150).
008360000000
008370000000      *****
008380000000      *****    Standard Version
008390000000      *****
008400000000
008410000000       01  WS-STATEMENT-PAGE-R         REDEFINES WS-STATEMENT-PAGE.
008420000000           03  FILLER                  PICTURE X(3750).
008430000000           03  WS-STMTPGE-DTL          PICTURE X(6450).
008440000000           03  FILLER                  PICTURE X(150).
008450000000           03  WS-STMTPGE-MSG          PICTURE X(2100).
008460000000           03  FILLER                  PICTURE X(750).
008470000000
008480991213      *****
008490001124      *****    Version 8:  Used MLA.
008500991213      *****
008510991213
008520991213       01  WS-STATEMENT-PAGE-V8-R      REDEFINES WS-STATEMENT-PAGE.
008530991213           03  FILLER                  PICTURE X(3900).
008540991213           03  WS-STMTPGE-DTL-V8       PICTURE X(6450).
008550991213           03  FILLER                  PICTURE X(150).
008560991213           03  WS-STMTPGE-MSG-V8       PICTURE X(2100).
008570991213           03  FILLER                  PICTURE X(600).
008580991213
008590991021
008600000000      /
008610000000      ******************************************************************
008620000000      *                                                                *
008630000000      *    S T A T E M E N T   M E S S A G E   S A V E   A R E A       *
008640000000      *                                                                *
008650000000      ******************************************************************
008660000000
008670000000       01  WS-STATEMENT-PAGE-MESSAGE.
008680000000           03  WS-STMTPGE-MSGLNE       OCCURS  14  TIMES
008690000000                                       INDEXED BY  SPML
008700000000                                       PICTURE X(150).
008710000000      /
008720000000      ******************************************************************
008730000000      *                                                                *
008740000000      *    S T A T E M E N T   H E A D I N G   P R I N T   L I N E S   *
008750000000      *                                                                *
008760000000      ******************************************************************
008770000000
008780000000      *****
008790000000      *****    The statement heading lines will be re-defined for each
008800000000      *****    version of the statement.
008810991213      *****        Version 8:  Used by MLA.
008820000000      *****
008830990927
008840000000      /
008850000000       01  STMT-HDG-01.
008860000000           03  FILLER                  PICTURE X(2).
008870001127           03  SH01-ACCOUNT-TYPE-DESC  PICTURE X(75).
008880000000           03  FILLER                  PICTURE X(4).
008890000000           03  SH01-LIT1               PICTURE X(20).
008900000000           03  FILLER                  PICTURE X(1).
008910980722           03  SH01-START-DATE         PICTURE XXXX/XX/XX.
008920000000           03  FILLER                  PICTURE X(1).
008930000000           03  SH01-LIT2               PICTURE X(2).
008940000000           03  FILLER                  PICTURE X(1).
008950980722           03  SH01-END-DATE           PICTURE XXXX/XX/XX.
008960980722           03  FILLER                  PICTURE X(24).
008970000000
008980991213       01  STMT-HDG-01-V8-R            REDEFINES STMT-HDG-01.
008990991213           03  FILLER                  PICTURE X(2).
009000001127           03  SH01V8-ACCOUNT-TYPE-DESC PICTURE X(75).
009010991213           03  FILLER                  PICTURE X(4).
009020991213           03  SH01V8-LIT1             PICTURE X(20).
009030991213           03  FILLER                  PICTURE X(1).
009040991213           03  SH01V8-START-DATE       PICTURE XXXX/XX/XX.
009050991213           03  FILLER                  PICTURE X(1).
009060991213           03  SH01V8-LIT2             PICTURE X(2).
009070991213           03  FILLER                  PICTURE X(1).
009080991213           03  SH01V8-END-DATE         PICTURE XXXX/XX/XX.
009090991213           03  FILLER                  PICTURE X(24).
009100000000      /
009110000000       01  STMT-HDG-02.
009120000000           03  FILLER                  PICTURE X(6).
009130000000           03  SH02-ACCOUNT            PICTURE ZZZZZZZZ9.
009140000000           03  FILLER                  PICTURE X(12).
009150000000           03  SH02-SIN-NO             PICTURE X(11).
009160000000           03  FILLER                  PICTURE X(6).
009170000000           03  SH02-DEALER             PICTURE X(4).
009180000000           03  SH02-LITDSH             PICTURE X(1).
009190000222           03  SH02-SALESREP           PICTURE X(6).
009200000222           03  FILLER                  PICTURE X(3).
009210980722           03  SH02-END-DATE           PICTURE XXXX/XX/XX.
009220000000           03  FILLER                  PICTURE X(4).
009230000000           03  SH02-SPOUSAL-CONTRIB    PICTURE X(40).
009240000000           03  FILLER                  PICTURE X(5).
009250000000           03  SH02-REG-CTB-CUR        PICTURE $$$$,$$$,$$$.99.
009260000000           03  SH02-REG-CTB-CUR-SIGN   PICTURE X(1).
009270000000           03  FILLER                  PICTURE X(1).
009280000000           03  SH02-PGENO              PICTURE ZZ9.
009290000000           03  FILLER                  PICTURE X(13).
009300000000
009310991213       01  STMT-HDG-02-V8-R            REDEFINES STMT-HDG-02.
009320991213           03  FILLER                  PICTURE X(5).
009330991213           03  SH02V8-ACCOUNT          PICTURE ZZZZZZZZ9.
009340991213           03  FILLER                  PICTURE X(11).
009350991213           03  SH02V8-SIN-NO           PICTURE X(11).
009360991213           03  FILLER                  PICTURE X(6).
009370991213           03  SH02V8-DEALER           PICTURE X(4).
009380991213           03  SH02V8-LITDSH           PICTURE X(1).
009390000222           03  SH02V8-SALESREP         PICTURE X(6).
009400000222           03  FILLER                  PICTURE X(3).
009410991213           03  SH02V8-END-DATE         PICTURE XXXX/XX/XX.
009420991213           03  FILLER                  PICTURE X(4).
009430991213           03  SH02V8-SPOUSAL-CONTRIB  PICTURE X(40).
009440991213           03  FILLER                  PICTURE X(4).
009450991213           03  SH02V8-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
009460991213           03  SH02V8-REG-CTB-CUR-SIGN PICTURE X(1).
009470991213           03  FILLER                  PICTURE X(1).
009480991213           03  SH02V8-PGENO            PICTURE ZZ9.
009490991213           03  FILLER                  PICTURE X(16).
009500991213
009510991213       01  STMT-HDG-022-V8-R           REDEFINES STMT-HDG-02.
009520991213           03  FILLER                  PICTURE X(14).
009530991213           03  SH022V8-ACCOUNT         PICTURE ZZZZZZZZ9.
009540991213           03  FILLER                  PICTURE X(19).
009550991213           03  SH022V8-DEALER          PICTURE X(4).
009560991213           03  SH022V8-LITDSH          PICTURE X(1).
009570000222           03  SH022V8-SALESREP        PICTURE X(6).
009580000222           03  FILLER                  PICTURE X(3).
009590991213           03  SH022V8-END-DATE        PICTURE XXXX/XX/XX.
009600991213           03  FILLER                  PICTURE X(4).
009610991213           03  SH022V8-SPOUSAL-CONTRIB PICTURE X(40).
009620991213           03  FILLER                  PICTURE X(4).
009630991213           03  SH022V8-REG-CTB-CUR     PICTURE $$$$,$$$,$$$.99.
009640991213           03  SH022V8-REG-CTB-CUR-SIGN PICTURE X(1).
009650991213           03  FILLER                  PICTURE X(1).
009660991213           03  SH022V8-PGENO           PICTURE ZZ9.
009670991213           03  FILLER                  PICTURE X(16).
009680000000      /
009690000000       01  STMT-HDG-03.
009700000000           03  FILLER                  PICTURE X(8).
009710000000           03  SH03-INVESTOR-NAME      PICTURE X(45).
009720010910R11175     03  FILLER                  PICTURE X(7).
009730010910R11175     03  SH03-LIT1               PICTURE X(23).
009740010426R9415A     03  FILLER                  PICTURE X(4).
009750000000           03  SH03-DEALER-REP-NAMES   PICTURE X(51).
009760000000           03  FILLER                  PICTURE X(12).
009770991213
009780991213       01  STMT-HDG-03-V8-R            REDEFINES STMT-HDG-03.
009790991213           03  FILLER                  PICTURE X(8).
009800991213           03  SH03V8-INVESTOR-NAME    PICTURE X(45).
009810010910R11175     03  FILLER                  PICTURE X(7).
009820010910R11175     03  SH03V8-LIT1             PICTURE X(23).
009830010426R9415A     03  FILLER                  PICTURE X(4).
009840991213           03  SH03V8-DEALER-REP-NAMES PICTURE X(51).
009850010426R9415A     03  FILLER                  PICTURE X(12).
009860000000      /
009870000000       01  STMT-HDG-04.
009880000000           03  FILLER                  PICTURE X(8).
009890000000           03  SH04-INVESTOR-ADDRESS1  PICTURE X(40).
009900010426           03  FILLER                  PICTURE X(39).
009910000000           03  SH04-BRANCH-ADDR1       PICTURE X(40).
009920000000           03  FILLER                  PICTURE X(23).
009930991213
009940991213       01  STMT-HDG-04-V8-R            REDEFINES STMT-HDG-04.
009950991213           03  FILLER                  PICTURE X(8).
009960991213           03  SH04V8-INVESTOR-ADDRESS1
009970991213                                       PICTURE X(40).
009980010426           03  FILLER                  PICTURE X(35).
009990991213           03  SH04V8-BRANCH-ADDR1     PICTURE X(40).
010000010426           03  FILLER                  PICTURE X(27).
010010000000      /
010020000000       01  STMT-HDG-05.
010030000000           03  FILLER                  PICTURE X(8).
010040010501           03  SH05-INVESTOR-ADDRESS2  PICTURE X(45).
010050010501R9415A     03  FILLER                  PICTURE X(8).
010060010427           03  SH05-LIT1               PICTURE X(22).
010070010426R9415A     03  FILLER                  PICTURE X(4).
010080010501           03  SH05-BRANCH-ADDR2       PICTURE X(51).
010090010501           03  FILLER                  PICTURE X(12).
010100000000
010110991213       01  STMT-HDG-05-V8-R            REDEFINES STMT-HDG-05.
010120991213           03  FILLER                  PICTURE X(8).
010130991213           03  SH05V8-INVESTOR-ADDRESS2
010140010501                                       PICTURE X(45).
010150010501R9415A     03  FILLER                  PICTURE X(8).
010160010427           03  SH05V8-LIT1             PICTURE X(22).
010170010426R9415A     03  FILLER                  PICTURE X(4).
010180010501           03  SH05V8-BRANCH-ADDR2     PICTURE X(51).
010190010501R9415A     03  FILLER                  PICTURE X(12).
010200991213
010210000000      /
010220000000       01  STMT-HDG-06.
010230000000           03  FILLER                  PICTURE X(8).
010240010501           03  SH06-INVESTOR-ADDRESS3  PICTURE X(45).
010250010501R9415A     03  FILLER                  PICTURE X(8).
010260010427           03  SH06-LIT1               PICTURE X(22).
010270010426R9415A     03  FILLER                  PICTURE X(4).
010280010501           03  SH06-BRANCH-ADDR3       PICTURE X(51).
010290010501           03  FILLER                  PICTURE X(12).
010300991213
010310991213       01  STMT-HDG-06-V8-R            REDEFINES STMT-HDG-06.
010320991213           03  FILLER                  PICTURE X(8).
010330991213           03  SH06V8-INVESTOR-ADDRESS3
010340010501                                       PICTURE X(45).
010350010501R9415A     03  FILLER                  PICTURE X(8).
010360010427           03  SH06V8-LIT1             PICTURE X(22).
010370010426R9415A     03  FILLER                  PICTURE X(4).
010380010501           03  SH06V8-BRANCH-ADDR3     PICTURE X(51).
010390010501R9415A     03  FILLER                  PICTURE X(12).
010400000000      /
010410000000       01  STMT-HDG-07.
010420000000           03  FILLER                  PICTURE X(8).
010430010501           03  SH07-INVESTOR-ADDRESS4  PICTURE X(45).
010440010501R9415A     03  FILLER                  PICTURE X(8).
010450010427           03  SH07-LIT1               PICTURE X(22).
010460010426R9415A     03  FILLER                  PICTURE X(4).
010470010501           03  SH07-BRANCH-ADDR4       PICTURE X(51).
010480010501           03  FILLER                  PICTURE X(12).
010490000000
010500991213       01  STMT-HDG-07-V8-R            REDEFINES STMT-HDG-07.
010510991213           03  FILLER                  PICTURE X(8).
010520991213           03  SH07V8-INVESTOR-ADDRESS4
010530010501                                       PICTURE X(45).
010540010501R9415A     03  FILLER                  PICTURE X(8).
010550010427           03  SH07V8-LIT1             PICTURE X(22).
010560010426R9415A     03  FILLER                  PICTURE X(4).
010570010501           03  SH07V8-BRANCH-ADDR4     PICTURE X(51).
010580010501R9415A     03  FILLER                  PICTURE X(12).
010590000000      /
010600000000       01  STMT-HDG-08.
010610000000           03  FILLER                  PICTURE X(8).
010620000000           03  SH08-INVESTOR-ADDRESS5  PICTURE X(40).
010630000000           03  FILLER                  PICTURE X(102).
010640001124
010650991213       01  STMT-HDG-08-V8-R            REDEFINES STMT-HDG-08.
010660991213           03  FILLER                  PICTURE X(8).
010670991213           03  SH08V8-INVESTOR-ADDRESS5
010680991213                                       PICTURE X(40).
010690991213           03  FILLER                  PICTURE X(102).
010700991213
010710000000      /
010720000000       01  STMT-HDG-09.
010730000000           03  FILLER                  PICTURE X(8).
010740000000           03  SH09-POSTAL-CODE        PICTURE X(10).
010750000000           03  FILLER                  PICTURE X(132).
010760991213
010770991213       01  STMT-HDG-09-V8-R            REDEFINES STMT-HDG-09.
010780991213           03  FILLER                  PICTURE X(8).
010790991213           03  SH09V8-POSTAL-CODE      PICTURE X(10).
010800991213           03  FILLER                  PICTURE X(132).
010810000000      /
010820000000       01  STMT-HDG-10.
010830000000           03  FILLER                  PICTURE X(8).
010840030328           03  SH10-LIT1               PICTURE X(47).
010850000000           03  FILLER                  PICTURE X(32).
010860000000           03  SH10-PHONE              PICTURE X(14).
010870000000           03  FILLER                  PICTURE X(49).
010880000000
010890991213       01  STMT-HDG-10-V8-R            REDEFINES STMT-HDG-10.
010900991213           03  FILLER                  PICTURE X(8).
010910030328           03  SH10V8-LIT1             PICTURE X(47).
010920991213           03  FILLER                  PICTURE X(30).
010930991213           03  SH10V8-PHONE            PICTURE X(14).
010940991213           03  FILLER                  PICTURE X(51).
010950000000      /
010960000000       01  STMT-HDG-11.
010970000000           03  FILLER                  PICTURE X(8).
010980000000           03  SH11-NOMINEE            PICTURE X(53).
010990000000           03  FILLER                  PICTURE X(26).
011000000000           03  SH11-FAX                PICTURE X(14).
011010000000           03  FILLER                  PICTURE X(49).
011020991213
011030991213       01  STMT-HDG-11-V8-R            REDEFINES STMT-HDG-11.
011040991213           03  FILLER                  PICTURE X(8).
011050991213           03  SH11V8-NOMINEE          PICTURE X(53).
011060991213           03  FILLER                  PICTURE X(24).
011070991213           03  SH11V8-FAX              PICTURE X(14).
011080991213           03  FILLER                  PICTURE X(51).
011090991213
011100001127      /
011110001127      ******************************************************************
011120001127      *                                                                *
011130001130      *    S T A T E M E N T   P L A N   S U M M U A R Y   L I N E S   *
011140001127      *                                                                *
011150001127      ******************************************************************
011160001127
011170001127      *****
011180001127      *****    The statement plan summary will be re-defined for each
011190001127      *****    version of the statement.
011200001127      *****        Version 8:  Used by MLA.
011210001127      *****
011220001127
011230001127      /
011240001127       01  STMT-PLS-01.
011250001127           03  FILLER                  PICTURE X(5).
011260001128           03  SP01-INVESTMENT-DESC    PICTURE X(35).
011270010418R10114     03  FILLER                  PICTURE X(1).
011280010418           03  SP01-JANROR             PICTURE $$$$,$$$,$$$.99.
011290010418           03  FILLER                  PICTURE X(1).
011300010418R10114     03  SP01-JUNROR             PICTURE $$$$,$$$,$$$.99.
011310001219           03  FILLER                  PICTURE X(10).
011320001215           03  SP01-YTD-6MO-SIGN       PICTURE X(1).
011330001219           03  SP01-YTD-6MO            PICTURE ZZZ.9.
011340001219           03  SP01-YTD-6MO-R          REDEFINES SP01-YTD-6MO.
011350001219               05 SP01-YTD-6MO-SPACE   PICTURE X(2).
011360001219               05 SP01-YTD-6MO-LIT     PICTURE X(3).
011370001205           03  SP01-YTD-6MOPERCENT     PICTURE X(1).
011380001219           03  FILLER                  PICTURE X(6).
011390001215           03  SP01-12MO-SIGN          PICTURE X(1).
011400001219           03  SP01-12MO               PICTURE ZZZ.9.
011410001211           03  SP01-12MO-R             REDEFINES SP01-12MO.
011420001219               05 SP01-12MO-SPACE      PICTURE X(2).
011430001211               05 SP01-12MO-LIT        PICTURE X(3).
011440001211           03  SP01-YTD-12MOPERCENT    PICTURE X(1).
011450001219           03  FILLER                  PICTURE X(10).
011460001215           03  SP01-SINCE-ISSUE-SIGN   PICTURE X(1).
011470001219           03  SP01-SINCE-ISSUE        PICTURE ZZZ.9.
011480001205           03  SP01-SINCE-ISSUEPERCENT PICTURE X(1).
011490001219           03  FILLER                  PICTURE X(32).
011500001127
011510001127       01  STMT-PLS-01-V8-R            REDEFINES STMT-PLS-01.
011520001127           03  FILLER                    PICTURE X(5).
011530001128           03  SP01V8-INVESTMENT-DESC    PICTURE X(35).
011540010608           03  FILLER                    PICTURE X(1).
011550010608           03  SP01V8-JANROR             PICTURE $$$$,$$$,$$$.99.
011560010608           03  FILLER                    PICTURE X(1).
011570010608           03  SP01V8-JUNROR             PICTURE $$$$,$$$,$$$.99.
011580001219           03  FILLER                    PICTURE X(10).
011590001215           03  SP01V8-YTD-6MO-SIGN       PICTURE X(1).
011600001219           03  SP01V8-YTD-6MO            PICTURE ZZZ.9.
011610001219           03  SP01V8-YTD-6MO-R          REDEFINES SP01V8-YTD-6MO.
011620001219               05 SP01V8-YTD-6MO-SPACE   PICTURE X(2).
011630001219               05 SP01V8-YTD-6MO-LIT     PICTURE X(3).
011640001205           03  SP01V8-YTD-6MOPERCENT     PICTURE X(1).
011650001219           03  FILLER                    PICTURE X(6).
011660001215           03  SP01V8-12MO-SIGN          PICTURE X(1).
011670001219           03  SP01V8-12MO               PICTURE ZZZ.9.
011680001211           03  SP01V8-12MO-R             REDEFINES SP01V8-12MO.
011690001219               05 SP01V8-12MO-SPACE      PICTURE X(2).
011700001211               05 SP01V8-12MO-LIT        PICTURE X(3).
011710001211           03  SP01V8-YTD-12MOPERCENT    PICTURE X(1).
011720001219           03  FILLER                    PICTURE X(10).
011730001215           03  SP01V8-SINCE-ISSUE-SIGN   PICTURE X(1).
011740001219           03  SP01V8-SINCE-ISSUE        PICTURE ZZZ.9.
011750001205           03  SP01V8-SINCE-ISSUEPERCENT PICTURE X(1).
011760001219           03  FILLER                    PICTURE X(32).
011770001127
011780001127       01  STMT-PLS-02.
011790001201           03  SP02-INVESTMENT-DESC    PICTURE X(46).
011800001127           03  SP02-JANROR-DOT         PICTURE X(10).
011810001201           03  FILLER                  PICTURE X(6).
011820001127           03  SP02-JUNROR-DOT         PICTURE X(10).
011830001201           03  FILLER                  PICTURE X(78).
011840001127
011850001127       01  STMT-PLS-02-V8-R            REDEFINES STMT-PLS-02.
011860001201           03  SP02V8-INVESTMENT-DESC    PICTURE X(46).
011870001127           03  SP02V8-JANROR-DOT         PICTURE X(10).
011880001201           03  FILLER                    PICTURE X(6).
011890001127           03  SP02V8-JUNROR-DOT         PICTURE X(10).
011900001201           03  FILLER                    PICTURE X(78).
011910001127
011920001127       01  STMT-PLS-03.
011930001127           03  FILLER                  PICTURE X(5).
011940001201           03  SP03-TOTAL-PLAN         PICTURE X(35).
011950010608           03  FILLER                  PICTURE X(1).
011960010608           03  SP03-JANROR-TOT         PICTURE $$$$,$$$,$$$.99.
011970010608           03  FILLER                  PICTURE X(1).
011980010608           03  SP03-JUNROR-TOT         PICTURE $$$$,$$$,$$$.99.
011990001219           03  FILLER                  PICTURE X(10).
012000001215           03  SP03-YTD-6MOTOT-SIGN    PICTURE X(1).
012010001219           03  SP03-YTD-6MOTOT         PICTURE ZZZ.9.
012020001219           03  SP03-YTD-6MOTOT-R       REDEFINES SP03-YTD-6MOTOT.
012030001219               05 SP03-YTD-6MOTOT-SPACE  PICTURE X(2).
012040001219               05 SP03-YTD-6MOTOT-LIT  PICTURE X(3).
012050001205           03  SP03-YTD-6MOPERCENT     PICTURE X(1).
012060001219           03  FILLER                  PICTURE X(6).
012070001215           03  SP03-12MOTOT-SIGN       PICTURE X(1).
012080001219           03  SP03-12MOTOT            PICTURE ZZZ.9.
012090001211           03  SP03-12MOTOT-R          REDEFINES SP03-12MOTOT.
012100001219               05 SP03-12MOTOT-SPACE   PICTURE X(2).
012110001211               05 SP03-12MOTOT-LIT     PICTURE X(3).
012120001211           03  SP03-YTD-12MOTOTPERCENT PICTURE X(1).
012130001219           03  FILLER                  PICTURE X(10).
012140001215           03  SP03-SINCE-ISSUETOT-SIGN   PICTURE X(1).
012150001219           03  SP03-SINCE-ISSUETOT     PICTURE ZZZ.9.
012160001205           03  SP03-SINCE-ISSUEPERCENT PICTURE X(1).
012170001219           03  FILLER                  PICTURE X(32).
012180001127
012190001127       01  STMT-PLS-03-V8-R            REDEFINES STMT-PLS-03.
012200001127           03  FILLER                    PICTURE X(5).
012210001201           03  SP03V8-TOTAL-PLAN         PICTURE X(35).
012220010418R10114     03  FILLER                    PICTURE X(1).
012230010418           03  SP03V8-JANROR-TOT         PICTURE $$$$,$$$,$$$.99.
012240010418           03  FILLER                    PICTURE X(1).
012250010418R10114     03  SP03V8-JUNROR-TOT         PICTURE $$$$,$$$,$$$.99.
012260001219           03  FILLER                    PICTURE X(10).
012270001215           03  SP03V8-YTD-6MOTOT-SIGN    PICTURE X(1).
012280001219           03  SP03V8-YTD-6MOTOT         PICTURE ZZZ.9.
012290001219           03  SP03V8-YTD-6MOTOT-R       REDEFINES SP03V8-YTD-6MOTOT.
012300001219               05 SP03V8-YTD-6MOTOT-SPACE  PICTURE X(2).
012310001219               05 SP03V8-YTD-6MOTOT-LIT  PICTURE X(3).
012320001205           03  SP03V8-YTD-6MOPERCENT     PICTURE X(1).
012330001219           03  FILLER                    PICTURE X(6).
012340001215           03  SP03V8-12MOTOT-SIGN       PICTURE X(1).
012350001219           03  SP03V8-12MOTOT            PICTURE ZZZ.9.
012360001211           03  SP03V8-12MOTOT-R          REDEFINES SP03V8-12MOTOT.
012370001219               05 SP03V8-12MOTOT-SPACE   PICTURE X(2).
012380001211               05 SP03V8-12MOTOT-LIT     PICTURE X(3).
012390001211           03  SP03V8-YTD-12MOTOTPERCENT PICTURE X(1).
012400001219           03  FILLER                    PICTURE X(10).
012410001215           03  SP03V8-SINCE-ISSUETOT-SIGN   PICTURE X(1).
012420001219           03  SP03V8-SINCE-ISSUETOT     PICTURE ZZZ.9.
012430001205           03  SP03V8-SINCE-ISSUEPERCENT PICTURE X(1).
012440001219           03  FILLER                    PICTURE X(32).
012450001128
012460001207       01  STMT-PLS-COLHD.
012470001207           03  FILLER                  PICTURE X(5).
012480001207           03  SPCH-DESCRIPTION1       PICTURE X(40).
012490001207           03  FILLER                  PICTURE X(14).
012500001207           03  SPCH-COL2-1             PICTURE X(9).
012510001207           03  SPCH-COL2-DASH          PICTURE X(1) VALUE "-".
012520001207           03  SPCH-COL2-2             PICTURE X(9).
012530001207           03  FILLER                  PICTURE X(4).
012540001207           03  SPCH-COL3               PICTURE X(25).
012550001207           03  FILLER                  PICTURE X(48).
012560001207
012570001207       01  STMT-PLS-COLHD-R            REDEFINES STMT-PLS-COLHD.
012580001207           03  FILLER                    PICTURE X(5).
012590001207           03  SPCHV8-DESCRIPTION1       PICTURE X(40).
012600001207           03  FILLER                    PICTURE X(14).
012610001207           03  SPCHV8-COL2-1             PICTURE X(9).
012620061027           03  SPCHV8-COL2-DASH          PICTURE X(1).
012630001207           03  SPCHV8-COL2-2             PICTURE X(9).
012640001207           03  FILLER                    PICTURE X(4).
012650001207           03  SPCHV8-COL3               PICTURE X(25).
012660001207           03  FILLER                    PICTURE X(48).
012670001207
012680001201       01  STMT-PLS-04.
012690001201           03  FILLER                  PICTURE X(5).
012700010911R11175     03  SP04-DESCRIPTION1       PICTURE X(35).
012710010911R11175     03  SP04-MONTH-COL1         PICTURE X(18).
012720010911R11175     03  FILLER                  PICTURE X(5).
012730010621           03  SP04-ACCUM-TOT          PICTURE $$$$,$$$,$$$.99.
012740010621           03  FILLER                  PICTURE X(11).
012750010912R11175*    03  SP04-ACCUM-ISSUE-TOT    PICTURE $$$$,$$$,$$$.99.
012760010912R11175     03  SP04-ACCUM-ISSUE-TOT    PICTURE X(15).
012770001205           03  FILLER                  PICTURE X(56).
012780001201
012790001201       01  STMT-PLS-04-V8-R            REDEFINES STMT-PLS-04.
012800001201           03  FILLER                    PICTURE X(5).
012810010911R11175     03  SP04V8-DESCRIPTION1       PICTURE X(35).
012820010911R11175     03  SP04V8-MONTH-COL1         PICTURE X(18).
012830010911R11175     03  FILLER                    PICTURE X(5).
012840010621           03  SP04V8-ACCUM-TOT          PICTURE $$$$,$$$,$$$.99.
012850010621           03  FILLER                    PICTURE X(11).
012860010912R11175*    03  SP04V8-ACCUM-ISSUE-TOT    PICTURE $$$$,$$$,$$$.99.
012870010912R11175     03  SP04V8-ACCUM-ISSUE-TOT    PICTURE X(15).
012880001205           03  FILLER                    PICTURE X(56).
012890001201
012900001201       01  STMT-PLS-05.
012910001128           03  FILLER                  PICTURE X(5).
012920001201           03  SP05-DESCRIPTION1       PICTURE X(40).
012930010621           03  FILLER                  PICTURE X(18).
012940010621           03  SP05-DEPOSIT-TOT        PICTURE $$$$,$$$,$$$.99.
012950010621           03  FILLER                  PICTURE X(11).
012960010621           03  SP05-DEPOSIT-TOT-ISSUE  PICTURE $$$$,$$$,$$$.99.
012970001205           03  FILLER                  PICTURE X(56).
012980001128
012990001201       01  STMT-PLS-05-V8-R            REDEFINES STMT-PLS-05.
013000001128           03  FILLER                    PICTURE X(5).
013010001201           03  SP05V8-DESCRIPTION1       PICTURE X(40).
013020010621           03  FILLER                    PICTURE X(18).
013030010621           03  SP05V8-DEPOSIT-TOT        PICTURE $$$$,$$$,$$$.99.
013040010621           03  FILLER                    PICTURE X(11).
013050010621           03  SP05V8-DEPOSIT-TOT-ISSUE  PICTURE $$$$,$$$,$$$.99.
013060001205           03  FILLER                    PICTURE X(56).
013070001127
013080001201       01  STMT-PLS-06.
013090001128           03  FILLER                  PICTURE X(5).
013100001201           03  SP06-DESCRIPTION1       PICTURE X(40).
013110010621           03  FILLER                  PICTURE X(18).
013120010621           03  SP06-WITHDRA-TOT        PICTURE $$$$,$$$,$$$.99.
013130010621           03  FILLER                  PICTURE X(11).
013140010621           03  SP06-WITHDRA-TOT-ISSUE  PICTURE $$$$,$$$,$$$.99.
013150001205           03  FILLER                  PICTURE X(56).
013160001128
013170001201       01  STMT-PLS-06-V8-R            REDEFINES STMT-PLS-06.
013180001128           03  FILLER                    PICTURE X(5).
013190001201           03  SP06V8-DESCRIPTION1       PICTURE X(40).
013200010621           03  FILLER                    PICTURE X(18).
013210010621           03  SP06V8-WITHDRA-TOT        PICTURE $$$$,$$$,$$$.99.
013220010621           03  FILLER                    PICTURE X(11).
013230010621           03  SP06V8-WITHDRA-TOT-ISSUE  PICTURE $$$$,$$$,$$$.99.
013240001205           03  FILLER                    PICTURE X(56).
013250001128
013260001201       01  STMT-PLS-07.
013270001128           03  FILLER                  PICTURE X(5).
013280001201           03  SP07-DESCRIPTION1       PICTURE X(40).
013290001205           03  FILLER                  PICTURE X(22).
013300001205           03  SP07-WITHDRA-TOT        PICTURE X(11)
013310061027                                               VALUE "-----------".
013320001128           03  FILLER                  PICTURE X(15).
013330001205           03  SP07-WITHDRA-TOT-ISSUE  PICTURE X(11)
013340061027                                               VALUE "-----------".
013350001205           03  FILLER                  PICTURE X(56).
013360001128
013370001201       01  STMT-PLS-07-V8-R            REDEFINES STMT-PLS-07.
013380001128           03  FILLER                    PICTURE X(5).
013390001201           03  SP07V8-DESCRIPTION1       PICTURE X(40).
013400001205           03  FILLER                    PICTURE X(22).
013410061027           03  SP07V8-WITHDRA-TOT        PICTURE X(11).
013420001128           03  FILLER                    PICTURE X(15).
013430061027           03  SP07V8-WITHDRA-TOT-ISSUE  PICTURE X(11).
013440001205           03  FILLER                    PICTURE X(56).
013450001128
013460001201       01  STMT-PLS-08.
013470001128           03  FILLER                  PICTURE X(5).
013480001201           03  SP08-DESCRIPTION1       PICTURE X(40).
013490010621           03  FILLER                  PICTURE X(18).
013500010621           03  SP08-DEP-WITHDRA        PICTURE $$$$,$$$,$$$.99.
013510001228           03  SP08-DEP-WITHDRA-SIGN   PICTURE X(1).
013520010621           03  FILLER                  PICTURE X(10).
013530010621           03  SP08-DEP-WITHDRA-ISSUE  PICTURE $$$$,$$$,$$$.99.
013540001228           03  SP08-DEP-WITHDRA-ISSUE-SIGN  PICTURE X(1).
013550001228           03  FILLER                  PICTURE X(55).
013560001128
013570001201       01  STMT-PLS-08-V8-R            REDEFINES STMT-PLS-08.
013580001128           03  FILLER                    PICTURE X(5).
013590001201           03  SP08V8-DESCRIPTION1       PICTURE X(40).
013600010621           03  FILLER                    PICTURE X(18).
013610010621           03  SP08V8-DEP-WITHDRA        PICTURE $$$$,$$$,$$$.99.
013620001228           03  SP08V8-DEP-WITHDRA-SIGN   PICTURE X(1).
013630010621           03  FILLER                    PICTURE X(10).
013640010621           03  SP08V8-DEP-WITHDRA-ISSUE  PICTURE $$$$,$$$,$$$.99.
013650001228           03  SP08V8-DEP-WITHDRA-ISSUE-SIGN  PICTURE X(1).
013660001228           03  FILLER                    PICTURE X(55).
013670001128
013680010912      ***
013690010912      ***  An extra line was added between previous line 8 and 9.
013700010912      ***  The extra line was name 9 and the previous line 9 was
013710010912      ***  changed to line 10 (RFS11175).
013720010912      ***
013730010912
013740010912R11175 01  STMT-PLS-09.
013750010912           03  FILLER                  PICTURE X(5).
013760010912           03  SP09-DESCRIPTION1       PICTURE X(52).
013770010912           03  FILLER                  PICTURE X(6).
013780010912           03  SP09-INCREASE-DECREASE1  PICTURE $$$$,$$$,$$$.99.
013790010912           03  SP09-INCREASE-DECREASE1S PICTURE X(1).
013800010912           03  FILLER                  PICTURE X(10).
013810010912           03  SP09-INCREASE-DECREASE2  PICTURE $$$$,$$$,$$$.99.
013820010912           03  SP09-INCREASE-DECREASE2S PICTURE X(1).
013830010912           03  FILLER                  PICTURE X(55).
013840010912
013850010912       01  STMT-PLS-09-V8-R            REDEFINES STMT-PLS-08.
013860010912           03  FILLER                    PICTURE X(5).
013870010912           03  SP09V8-DESCRIPTION1       PICTURE X(52).
013880010912           03  FILLER                    PICTURE X(6).
013890010912           03  SP09V8-INCREASE-DECREASE1  PICTURE $$$$,$$$,$$$.99.
013900010912           03  SP09V8-INCREASE-DECREASE1S PICTURE X(1).
013910010912           03  FILLER                    PICTURE X(10).
013920010912           03  SP09V8-INCREASE-DECREASE2  PICTURE $$$$,$$$,$$$.99.
013930010912           03  SP09V8-INCREASE-DECREASE2S PICTURE X(1).
013940010912R11175     03  FILLER                    PICTURE X(55).
013950010912
013960010912       01  STMT-PLS-10.
013970001128           03  FILLER                  PICTURE X(5).
013980010912           03  SP10-DESCRIPTION1       PICTURE X(35).
013990010912001218     03  SP10-MONTH-COL3         PICTURE X(18).
014000010426R10114     03  FILLER                  PICTURE X(5).
014010010912           03  SP10-TOTAL-VALUE1       PICTURE $$$$,$$$,$$$.99.
014020010426           03  FILLER                  PICTURE X(11).
014030010912R10114     03  SP10-TOTAL-VALUE2       PICTURE $$$$,$$$,$$$.99.
014040001205           03  FILLER                  PICTURE X(46).
014050001128
014060010912       01  STMT-PLS-10-V8-R            REDEFINES STMT-PLS-10.
014070001201           03  FILLER                    PICTURE X(5).
014080010912           03  SP10V8-DESCRIPTION1       PICTURE X(35).
014090010912001218     03  SP10V8-MONTH-COL3         PICTURE X(18).
014100010425R10114     03  FILLER                    PICTURE X(5).
014110010912           03  SP10V8-TOTAL-VALUE1       PICTURE $$$$,$$$,$$$.99.
014120010425           03  FILLER                    PICTURE X(11).
014130010912R10114     03  SP10V8-TOTAL-VALUE2       PICTURE $$$$,$$$,$$$.99.
014140001205           03  FILLER                    PICTURE X(46).
014150001128
014160000000      /
014170000000      ******************************************************************
014180000000      *                                                                *
014190000000      *    S T A T E M E N T   D E T A I L   P R I N T   L I N E S     *
014200000000      *                                                                *
014210000000      ******************************************************************
014220000000
014230000000      *****
014240000000      *****    The statement detail lines will be re-defined for each
014250000000      *****    version of the statement.
014260991213      *****        Version 8:  Used by MLA.
014270000000      *****
014280990927
014290000000      /
014300000000       01  STMT-DTL-01.
014310000000           03  FILLER                  PICTURE X(8).
014320000000           03  SD01-INVESTMENT-NAME    PICTURE X(35).
014330000000           03  FILLER                  PICTURE X(107).
014340991213
014350991213       01  STMT-DTL-01-V8-R            REDEFINES STMT-DTL-01.
014360991216           03  FILLER                  PICTURE X(10).
014370991213           03  SD01V8-INVESTMENT-NAME  PICTURE X(35).
014380991216           03  FILLER                  PICTURE X(105).
014390000000      /
014400000000       01  STMT-DTL-02.
014410000000           03  SD02-LIT1.
014420000000               05  SD02-TRADE-DATE     PICTURE X(6).
014430000000           03  FILLER                  PICTURE X(2).
014440000000           03  SD02-TRANS-TYPE         PICTURE X(25).
014450990226           03  FILLER                  PICTURE X(3).
014460000000           03  SD02-GROSS              PICTURE $$$$,$$$,$$$.99.
014470000000           03  SD02-GROSS-SIGN         PICTURE X(1).
014480000000           03  SD02-DEDUCTIONS         PICTURE $$$,$$$,$$$.99.
014490000000           03  SD02-DEDUCTIONS-SIGN    PICTURE X(1).
014500000000           03  SD02-NET                PICTURE $$$$,$$$,$$$.99.
014510000000           03  SD02-NET-SIGN           PICTURE X(1).
014520000000           03  FILLER                  PICTURE X(3).
014530000000           03  SD02-UNIT-PRICE         PICTURE ZZZ.9999.
014540000000           03  SD02-UNIT-PRICE-SIGN    PICTURE X(1).
014550000000           03  SD02-NO-OF-UNITS        PICTURE ZZZ,ZZZ,ZZZ.999.
014560000000           03  SD02-NO-OF-UNITS-SIGN   PICTURE X(1).
014570000000           03  SD02-UNIT-BALANCE       PICTURE ZZZ,ZZZ,ZZZ.999.
014580000000           03  SD02-UNIT-BALANCE-SIGN  PICTURE X(1).
014590000000           03  SD02-CPTL-GAIN-OFLIT.
014600000000               05  SD02-CPTL-GAIN      PICTURE $$$$$$.99.
014610000000               05  SD02-CPTL-GAIN-SIGN PICTURE X(1).
014620990226           03  FILLER                  PICTURE X(13).
014630000000
014640991213       01  STMT-DTL-02-V8-R            REDEFINES STMT-DTL-02.
014650991213           03  SD02V8-LIT1.
014660991216               05  SD02V8-TRADE-DATE   PICTURE X(9).
014670991213           03  FILLER                  PICTURE X(1).
014680991216           03  SD02V8-TRANS-TYPE       PICTURE X(22).
014690991213           03  FILLER                  PICTURE X(1).
014700991213           03  SD02V8-GROSS            PICTURE $$$$,$$$,$$$.99.
014710991213           03  SD02V8-GROSS-SIGN       PICTURE X(1).
014720991213           03  SD02V8-DEDUCTIONS       PICTURE $$$$,$$$,$$$.99.
014730991213           03  SD02V8-DEDUCTIONS-SIGN  PICTURE X(1).
014740991213           03  SD02V8-NET              PICTURE $$$,$$$,$$$.99.
014750991213           03  SD02V8-NET-SIGN         PICTURE X(1).
014760991213           03  FILLER                  PICTURE X(2).
014770991213           03  SD02V8-UNIT-PRICE       PICTURE ZZZ.9999.
014780991213           03  SD02V8-UNIT-PRICE-SIGN  PICTURE X(1).
014790991213           03  SD02V8-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
014800991213           03  SD02V8-NO-OF-UNITS-SIGN PICTURE X(1).
014810991213           03  SD02V8-UNIT-BALANCE     PICTURE ZZ,ZZZ,ZZZ.999.
014820991213           03  SD02V8-UNIT-BALANCE-SIGN
014830991213                                       PICTURE X(1).
014840991213           03  SD02V8-CPTL-GAIN-OFLIT.
014850991213               05  SD02V8-CPTL-GAIN    PICTURE $$$$$$$$.99.
014860991213               05  SD02V8-CPTL-GAIN-SIGN
014870991213                                       PICTURE X(1).
014880991216           03  FILLER                  PICTURE X(16).
014890991213
014900000000      /
014910000000      ******************************************************************
014920000000      *                                                                *
014930000000      *    S T A T E M E N T   S U M M A R Y   P R I N T   L I N E S   *
014940000000      *                                                                *
014950000000      ******************************************************************
014960000000
014970000000      *****
014980000000      *****    The statement summary lines will be re-defined for each
014990000000      *****    version of the statement.
015000991213      *****        Version 8:  Used by MLA.
015010000000      *****
015020990925
015030000000      /
015040000000       01  STMT-SUM-01.
015050000000           03  SS01-LIT1               PICTURE X(137).
015060000000           03  FILLER                  PICTURE X(13).
015070000000
015080991213       01  STMT-SUM-01-V8-R            REDEFINES STMT-SUM-01.
015090991213           03  SS01V8-LIT1             PICTURE X(137).
015100991213           03  FILLER                  PICTURE X(13).
015110000000      /
015120000000       01  STMT-SUM-02.
015130010912R11175*    03  FILLER                  PICTURE X(40).
015140010912R11175     03  FILLER                  PICTURE X(26).
015150000000           03  SS02-LIT0               PICTURE X(11).
015160000000           03  FILLER                  PICTURE X(35).
015170000000           03  SS02-LIT1               PICTURE X(8).
015180000000           03  FILLER                  PICTURE X(3).
015190000000           03  SS02-LIT2               PICTURE X(13).
015200000000           03  FILLER                  PICTURE X(3).
015210000000           03  SS02-LIT3               PICTURE X(13).
015220000000           03  FILLER                  PICTURE X(2).
015230000000           03  SS02-LIT4               PICTURE X(8).
015240010912R11175     03  FILLER                  PICTURE X(1).
015250010912R11175     03  SS02-LIT5               PICTURE X(13).
015260000000           03  FILLER                  PICTURE X(14).
015270991213
015280991213       01  STMT-SUM-02-V8-R            REDEFINES STMT-SUM-02.
015290010912R11175*    03  FILLER                  PICTURE X(37).
015300010912R11175     03  FILLER                  PICTURE X(24).
015310991213           03  SS02V8-LIT0             PICTURE X(11).
015320991213           03  FILLER                  PICTURE X(34).
015330991213           03  SS02V8-LIT1             PICTURE X(8).
015340991213           03  FILLER                  PICTURE X(3).
015350991213           03  SS02V8-LIT2             PICTURE X(13).
015360991213           03  FILLER                  PICTURE X(2).
015370991213           03  SS02V8-LIT3             PICTURE X(13).
015380991213           03  FILLER                  PICTURE X(2).
015390991213           03  SS02V8-LIT4             PICTURE X(8).
015400010912R11175     03  FILLER                  PICTURE X(1).
015410010912R11175     03  SS02V8-LIT5             PICTURE X(13).
015420991213           03  FILLER                  PICTURE X(17).
015430000000      /
015440000000       01  STMT-SUM-03.
015450010912R11175*    03  FILLER                  PICTURE X(40).
015460010912R11175     03  FILLER                  PICTURE X(26).
015470000000           03  SS03-LIT0               PICTURE X(11).
015480000000           03  FILLER                  PICTURE X(35).
015490000000           03  SS03-LIT1               PICTURE X(8).
015500000000           03  FILLER                  PICTURE X(3).
015510000000           03  SS03-LIT2               PICTURE X(13).
015520000000           03  FILLER                  PICTURE X(3).
015530000000           03  SS03-LIT3               PICTURE X(13).
015540000000           03  FILLER                  PICTURE X(2).
015550000000           03  SS03-LIT4               PICTURE X(8).
015560010912R11175     03  FILLER                  PICTURE X(1).
015570010912R11175     03  SS03-LIT5               PICTURE X(13).
015580000000           03  FILLER                  PICTURE X(14).
015590991213
015600991213       01  STMT-SUM-03-V8-R            REDEFINES STMT-SUM-03.
015610010912R11175*    03  FILLER                  PICTURE X(37).
015620010912R11175     03  FILLER                  PICTURE X(24).
015630991213           03  SS03V8-LIT0             PICTURE X(11).
015640991213           03  FILLER                  PICTURE X(34).
015650991213           03  SS03V8-LIT1             PICTURE X(8).
015660991213           03  FILLER                  PICTURE X(3).
015670991213           03  SS03V8-LIT2             PICTURE X(13).
015680991213           03  FILLER                  PICTURE X(2).
015690991213           03  SS03V8-LIT3             PICTURE X(13).
015700991213           03  FILLER                  PICTURE X(2).
015710991213           03  SS03V8-LIT4             PICTURE X(8).
015720010912R11175     03  FILLER                  PICTURE X(1).
015730010912R11175     03  SS03V8-LIT5             PICTURE X(13).
015740991213           03  FILLER                  PICTURE X(17).
015750000000      /
015760000000       01  STMT-SUM-04.
015770000000           03  SS04-CURRENCY           PICTURE X(3).
015780000000           03  FILLER                  PICTURE X(5).
015790000000           03  SS04-INVESTMENT-NAME.
015800000000               05  FILLER              PICTURE X(27).
015810000000               05  SS04-NET-INVESTED   PICTURE $$$$$,$$$,$$$.99.
015820000000               05  SS04-NET-INVESTED-SIGN
015830000000                                       PICTURE X(1).
015840010912R11175*    03  FILLER                  PICTURE X(34).
015850010912R11175     03  FILLER                  PICTURE X(20).
015860000000           03  SS04-LAST-UNIT-PRICE    PICTURE ZZZ.9999.
015870000000           03  SS04-LAST-UNIT-PRICE-SIGN
015880000000                                       PICTURE X(1).
015890000000           03  SS04-NO-OF-UNITS        PICTURE ZZZ,ZZZ,ZZZ.999.
015900000000           03  SS04-NO-OF-UNITS-SIGN   PICTURE X(1).
015910140617133528*    03  SS04-MARKET-VALUE       PICTURE $$$$,$$$,$$$.99.
015920140617133528     03  SS04-MARKET-VALUE       PICTURE $$$,$$$,$$$,$$$.99.
015930000000           03  SS04-MARKET-VALUE-SIGN  PICTURE X(1).
015940000000           03  FILLER                  PICTURE X(1).
015950150918      *RFS149112 STARTS
015960150918      *    03  SS04-ACB-UNIT-PRICE     PICTURE ZZZ.9999.
015970150918           03  SS04-ACB-UNIT-PRICE     PICTURE ZZ,ZZZ.9999.
015980150918      *RFS149112 ENDS
015990000000           03  SS04-ACB-UNIT-PRICE-SIGN
016000000000                                       PICTURE X(1).
016010010912R11175     03  FILLER                  PICTURE X(7).
016020010912R11175     03  SS04-PERCEN-OF-VALUE    PICTURE ZZ9.99.
016030010912R11175     03  SS04-PERCEN-OF-VALUE-P  PICTURE X(1).
016040010912
016050140617133528*    03  FILLER                  PICTURE X(13).
016060150918      *RFS149112 STARTS
016070150918133528*    03  FILLER                  PICTURE X(10).
016080150918           03  FILLER                  PICTURE X(8).
016090150918      *RFS149112 ENDS
016100000000
016110140610       01  STMT-SUM-04-V8-R            REDEFINES STMT-SUM-04.
016120991213           03  SS04V8-CURRENCY         PICTURE X(3).
016130991223           03  FILLER                  PICTURE X(7).
016140991213           03  SS04V8-INVESTMENT-NAME.
016150991223               05  FILLER              PICTURE X(22).
016160991213               05  SS04V8-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
016170991213               05  SS04V8-NET-INVESTED-SIGN
016180991213                                       PICTURE X(1).
016190010912R11175*    03  FILLER                  PICTURE X(33).
016200010912R11175     03  FILLER                  PICTURE X(20).
016210991213           03  SS04V8-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
016220991213           03  SS04V8-LAST-UNIT-PRICE-SIGN
016230991213                                       PICTURE X(1).
016240991213           03  SS04V8-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
016250991213           03  SS04V8-NO-OF-UNITS-SIGN PICTURE X(1).
016260140610133528*    03  SS04V8-MARKET-VALUE     PICTURE $$$,$$$,$$$.99.
016270140617133528     03  SS04V8-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99.
016280991213           03  SS04V8-MARKET-VALUE-SIGN
016290991213                                       PICTURE X(1).
016300140610133528*    03  FILLER                  PICTURE X(1).
016310150918      *RFS149112 STARTS
016320150918      *    03  SS04V8-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
016330150918           03  SS04V8-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
016340150918      *RFS149112 ENDS
016350991213           03  SS04V8-ACB-UNIT-PRICE-SIGN
016360991213                                       PICTURE X(1).
016370010912R11175     03  FILLER                  PICTURE X(6).
016380010912R11175     03  SS04V8-PERCEN-OF-VALUE    PICTURE ZZ9.99.
016390010912R11175     03  SS04V8-PERCEN-OF-VALUE-P  PICTURE X(1).
016400140617133528*    03  FILLER                  PICTURE X(16).
016410150918      *RFS149112 STARTS
016420150918133528*    03  FILLER                  PICTURE X(13).
016430150918           03  FILLER                  PICTURE X(11).
016440150918      *RFS149112 ENDS
016450000000      /
016460000000       01  STMT-SUM-05.
016470010912           03  FILLER                  PICTURE X(99).
016480000000           03  SS05-LIT1               PICTURE X(14).
016490010912R11175     03  FILLER                  PICTURE X(18).
016500010912R11175     03  SS05-LIT2               PICTURE X(6).
016510010912           03  FILLER                  PICTURE X(3).
016520001124
016530991213       01  STMT-SUM-05-V8-R            REDEFINES STMT-SUM-05.
016540010912           03  FILLER                  PICTURE X(95).
016550991213           03  SS05V8-LIT1             PICTURE X(13).
016560010912R11175     03  FILLER                  PICTURE X(18).
016570010912R11175     03  SS05V8-LIT2             PICTURE X(6).
016580010912           03  FILLER                  PICTURE X(8).
016590000000      /
016600000000       01  STMT-SUM-06.
016610000000           03  FILLER                  PICTURE X(8).
016620000000           03  SS06-LIT1               PICTURE X(35).
016630010912R11175     03  FILLER                  PICTURE X(24).
016640000000           03  SS06-LIT-CURRDESC       PICTURE X(30).
016650140617133528*    03  SS06-MARKET-VALUE       PICTURE $$$$,$$$,$$$.99-.
016660140617133528     03  SS06-MARKET-VALUE       PICTURE $$$,$$$,$$$,$$$.99-.
016670010912           03  FILLER                  PICTURE X(18).
016680010912R11175     03  SS06-PERCEN-OF-VALUE    PICTURE ZZZ.99.
016690010912R11175     03  SS06-PERCEN-OF-VALUE-P  PICTURE X(1).
016700140617133528*    03  FILLER                  PICTURE X(19).
016710140617133528     03  FILLER                  PICTURE X(16).
016720991213
016730991213       01  STMT-SUM-06-V8-R            REDEFINES STMT-SUM-06.
016740991223           03  FILLER                  PICTURE X(10).
016750991213           03  SS06V8-LIT1             PICTURE X(35).
016760010912R11175     03  FILLER                  PICTURE X(19).
016770991213           03  SS06V8-LIT-CURRDESC     PICTURE X(30).
016780140610133528*    03  SS06V8-MARKET-VALUE     PICTURE $$$,$$$,$$$.99-.
016790140610133528*    03  FILLER                  PICTURE X(16).
016800140617133528     03  SS06V8-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99-.
016810140617133528     03  FILLER                  PICTURE X(12).
016820010912R11175     03  SS06V8-PERCEN-OF-VALUE    PICTURE ZZZ.99.
016830010912R11175     03  SS06V8-PERCEN-OF-VALUE-P  PICTURE X(1).
016840010912           03  FILLER                  PICTURE X(19).
016850971223      /
016860991213       01  STMT-SUM-07-V8.
016870991213           03  SS07V8-YEAR             PICTURE X(4).
016880000104           03  FILLER                  PICTURE X(6).
016890000104           03  SS07V8-LIT              PICTURE X(25).
016900991213           03  FILLER                  PICTURE X(1).
016910991213           03  SS07V8-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
016920000104           03  FILLER                  PICTURE X(99).
016930981207      /
016940991213       01  STMT-SUM-08-V8.
016950991213           03  SS08V8-YEAR             PICTURE X(4).
016960991223           03  FILLER                  PICTURE X(6).
016970000104           03  SS08V8-LIT              PICTURE X(25).
016980991213           03  FILLER                  PICTURE X(1).
016990991213           03  SS08V8-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
017000000104           03  FILLER                  PICTURE X(99).
017010981207      /
017020981207       01  STMT-SUM-11.
017030981207           03  SS11-LIT1               PICTURE X(29).
017040971223           03  FILLER                  PICTURE X(7).
017050981207           03  SS11-CPG-FIXED          PICTURE $$$$,$$$,$$$.99.
017060981207           03  SS11-CPG-FIXED-SIGN     PICTURE X(1).
017070971223           03  FILLER                  PICTURE X(98).
017080991213
017090991213       01  STMT-SUM-11-V8-R            REDEFINES STMT-SUM-11.
017100991213           03  SS11V8-LIT1             PICTURE X(29).
017110991213           03  FILLER                  PICTURE X(4).
017120991213           03  SS11V8-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
017130991213           03  SS11V8-CPG-FIXED-SIGN   PICTURE X(1).
017140991213           03  FILLER                  PICTURE X(101).
017150991213
017160000000      /
017170000000      ******************************************************************
017180000000      *                                                                *
017190000000      *    S T A T E M E N T   M E S S A G E   P R I N T   L I N E S   *
017200000000      *                                                                *
017210000000      ******************************************************************
017220000000
017230000000       01  STMT-MSG-01.
017240000000           03  FILLER                  PICTURE X(7).
017250000000           03  SM01-STATEMENT-MESSAGE  PICTURE X(115).
017260000000           03  FILLER                  PICTURE X(28).
017270000000      /
017280990623      ******************************************************************
017290990623      *                                                                *
017300990623      *    S T A T E M E N T   H E A D I N G  L I T E R A L S          *
017310990623      *                                                                *
017320990623      ******************************************************************
017330990623      /
017340990623       01  WS-STMT-HDG-LITS.
017350990623
017360990623      *****
017370990623      *****    English Literals.
017380990623      *****
017390001127
017400001127001127     03  WS-SHPL-HDGLIT01-E.
017410001127               05  FILLER              PICTURE X(50)       VALUE
017420001127                   "                                                  ".
017430001127               05  FILLER              PICTURE X(50)       VALUE
017440001205                   "           PLAN SUMMARY                           ".
017450001127               05  FILLER              PICTURE X(50)       VALUE
017460001127                   "                                                  ".
017470001127
017480001127           03  WS-SHPL-HDGLIT02-E.
017490001127               05  FILLER              PICTURE X(50)       VALUE
017500001127                   "                                                  ".
017510001127               05  FILLER              PICTURE X(50)       VALUE
017520001127                   "              PLAN VALUE                    PLAN  ".
017530001127               05  FILLER              PICTURE X(50)       VALUE
017540001127                   "PERFORMANCE*                                      ".
017550001127
017560001127           03  WS-SHPL-HDGLIT03-E.
017570001127               05  FILLER              PICTURE X(50)       VALUE
017580001127                   "                                                  ".
017590001127               05  FILLER              PICTURE X(50)       VALUE
017600001214                   "                                                  ".
017610001127               05  FILLER              PICTURE X(50)       VALUE
017620001205                   "              SINCE                               ".
017630001127
017640001127           03  WS-SHPL-HDGLIT04-E.
017650001129               05  FILLER              PICTURE X(5).
017660001201               05  FILLER              PICTURE X(42)       VALUE
017670001201                   "INVESTMENT SUMMARY:                       "        .
017680001207               05  WS-SHPL04-COL1-E    PICTURE X(10).
017690001201               05  FILLER              PICTURE X(6).
017700001207               05  WS-SHPL04-COL2-E    PICTURE X(10).
017710001201               05  FILLER              PICTURE X(7).
017720001205               05  FILLER              PICTURE X(22)       VALUE
017730001214                   "     6 MO        12 MO"                            .
017740001205               05  FILLER              PICTURE X(48)       VALUE
017750001205                   "            ISSUE                               "  .
017760001127
017770001201           03  WS-SHPL-HDGLIT05-E.
017780001201               05  FILLER              PICTURE X(5).
017790001201               05  FILLER              PICTURE X(45)       VALUE
017800001201                   "CHANGE IN THE VALUE OF YOUR PLAN:            "     .
017810001205               05  FILLER              PICTURE X(9).
017820001201               05  WS-SHPL05-COL1      PICTURE X(9).
017830001201               05  WS-SHPL05-DASH      PICTURE X(1) VALUE "-".
017840001201               05  WS-SHPL05-COL2      PICTURE X(10).
017850001205               05  FILLER              PICTURE X(25)       VALUE
017860001205                   "              SINCE ISSUE"                         .
017870001205               05  FILLER              PICTURE X(47).
017880001201
017890001204           03  WS-SH-HDGLIT01-E.
017900001204               05  FILLER              PICTURE X(50).
017910001204               05  FILLER              PICTURE X(50)       VALUE
017920001205                   "          TRANSACTION DETAILS                     ".
017930001204               05  FILLER              PICTURE X(50).
017940990923
017950001204           03  WS-SH-HDGLIT02-E.
017960001204               05  FILLER              PICTURE X(50)       VALUE
017970001204                   " TRADE                                            ".
017980001204               05  FILLER              PICTURE X(50)       VALUE
017990001204                   "                                                  ".
018000001204               05  FILLER              PICTURE X(50)       VALUE
018010001204                   "                                                  ".
018020001204
018030001204           03  WS-SH-HDGLIT03-E.
018040990623               05  FILLER              PICTURE X(50)       VALUE
018050001205                   " DATE                                     GROSS   ".
018060990623               05  FILLER              PICTURE X(50)       VALUE
018070001205                   "               NET INVESTED/      PRICE           ".
018080990623               05  FILLER              PICTURE X(50)       VALUE
018090001205                   "               TOTAL     CAPITAL                  ".
018100001204
018110001204           03  WS-SH-HDGLIT04-E.
018120990623               05  FILLER              PICTURE X(50)       VALUE
018130001205                   "YYYYMMDD TRANSACTION TYPE                AMOUNT   ".
018140990623               05  FILLER              PICTURE X(50)       VALUE
018150001205                   "   DEDUCTIONS       PROCEEDS   PER UNIT           ".
018160990623               05  FILLER              PICTURE X(50)       VALUE
018170001205                   "UNITS          UNITS   GAIN/LOSS                  ".
018180990623
018190990623      *****
018200990623      *****    French Literals.
018210990623      *****
018220990623
018230001207001207     03  WS-SHPL-HDGLIT01-F.
018240001207               05  FILLER              PICTURE X(50)       VALUE
018250001207                   "                                                  ".
018260001207               05  FILLER              PICTURE X(50)       VALUE
018270001207                   "           SOMMAIRE DU CONTRAT                    ".
018280001207               05  FILLER              PICTURE X(50)       VALUE
018290001207                   "                                                  ".
018300001207
018310001207           03  WS-SHPL-HDGLIT02-F.
018320001207               05  FILLER              PICTURE X(50)       VALUE
018330001207                   "                                                  ".
018340001207               05  FILLER              PICTURE X(50)       VALUE
018350001221                   "             VALEUR DU CONTRAT              RENDEM".
018360001207               05  FILLER              PICTURE X(50)       VALUE
018370001220                   "ENT DU CONTRAT*                                   ".
018380001207
018390001207           03  WS-SHPL-HDGLIT03-F.
018400001207               05  FILLER              PICTURE X(50)       VALUE
018410001207                   "                                                  ".
018420001207               05  FILLER              PICTURE X(50)       VALUE
018430001214                   "                                                  ".
018440001207               05  FILLER              PICTURE X(50)       VALUE
018450001214                   "         DEPUIS LA                                ".
018460001207
018470001207           03  WS-SHPL-HDGLIT04-F.
018480001214               05  FILLER              PICTURE X(4).
018490001207               05  FILLER              PICTURE X(42)       VALUE
018500001214                   "SOMMAIRE DES PLACEMENTS:                  "        .
018510001207               05  WS-SHPL04-COL1-F    PICTURE X(10).
018520001207               05  FILLER              PICTURE X(6).
018530001207               05  WS-SHPL04-COL2-F    PICTURE X(10).
018540001207               05  FILLER              PICTURE X(7).
018550001207               05  FILLER              PICTURE X(22)       VALUE
018560001214                   "   6 MOIS      12 MOIS"                            .
018570001207               05  FILLER              PICTURE X(48)       VALUE
018580001207                   "     SOUSCRIPTION                               "  .
018590001207
018600001207           03  WS-SHPL-HDGLIT05-F.
018610001207               05  FILLER              PICTURE X(5).
018620001207               05  FILLER              PICTURE X(45)       VALUE
018630001207                   "CHANGEMENTS DE LA VALEUR DU CONTRAT:         "     .
018640001207               05  FILLER              PICTURE X(9).
018650001207               05  WS-SHPL05-COL1      PICTURE X(9).
018660001207               05  WS-SHPL05-DASH      PICTURE X(1) VALUE "-".
018670001207               05  WS-SHPL05-COL2      PICTURE X(10).
018680001207               05  FILLER              PICTURE X(25)       VALUE
018690001207                   "   DEPUIS LA SOUSCRIPTION"                         .
018700001207               05  FILLER              PICTURE X(47).
018710001207
018720001207           03  WS-SH-HDGLIT01-F.
018730001207               05  FILLER              PICTURE X(50).
018740001207               05  FILLER              PICTURE X(50)       VALUE
018750001217                   "        DÉTAILS DES TRANSACTIONS                  ".
018760001207               05  FILLER              PICTURE X(50).
018770001207
018780001207           03  WS-SH-HDGLIT02-F.
018790001207               05  FILLER              PICTURE X(50)       VALUE
018800001217                   "DATE DE LA                                        ".
018810001207               05  FILLER              PICTURE X(50)       VALUE
018820001207                   "                                                  ".
018830001207               05  FILLER              PICTURE X(50)       VALUE
018840001219                   "                             GAIN                 ".
018850001207
018860001207           03  WS-SH-HDGLIT03-F.
018870001207               05  FILLER              PICTURE X(50)       VALUE
018880001220                   "TRANSACTION                             MONTANT   ".
018890001207               05  FILLER              PICTURE X(50)       VALUE
018900001218                   "                 MONTANT NET    PRIX DE           ".
018910001207               05  FILLER              PICTURE X(50)       VALUE
018920001219                   "           TOTAL DES     OU PERTE                 ".
018930001207
018940001207           03  WS-SH-HDGLIT04-F.
018950001207               05  FILLER              PICTURE X(50)       VALUE
018960010424RF9415             "AAAAMMJJ TYPE DE TRANSACTION               BRUT   ".
018970001207               05  FILLER              PICTURE X(50)       VALUE
018980001218                   "   DEDUCTIONS INVEST/PRODUIT    L'UNITE          U".
018990001207               05  FILLER              PICTURE X(50)       VALUE
019000001218                   "NITES         UNITES   EN CAPITAL                 ".
019010001207
019020990623      *****
019030990623      *****    Statement Heading Literals.
019040990623      *****
019050990623
019060990623       01  WS-STMT-HDG-LITS-R          REDEFINES WS-STMT-HDG-LITS.
019070990623           03  WS-STMT-HDG-LIT-LNG     OCCURS   2  TIMES
019080990623                                       INDEXED BY  SHL.
019090001127001127         05  WS-SHLPL01          PICTURE X(150).
019100001127               05  WS-SHLPL02          PICTURE X(150).
019110001127               05  WS-SHLPL03          PICTURE X(150).
019120001127               05  WS-SHLPL04          PICTURE X(150).
019130001127               05  WS-SHLPL05          PICTURE X(150).
019140990623               05  WS-SHL01            PICTURE X(150).
019150990623               05  WS-SHL02            PICTURE X(150).
019160990623               05  WS-SHL03            PICTURE X(150).
019170990623               05  WS-SHL04            PICTURE X(150).
019180991019
019190001204001204/
019200001204      ******************************************************************
019210001204      *                                                                *
019220001204      *    S T A T E M E N T   P L A N   S U M M A R Y   M E S S A G E *
019230001204      *                                                                *
019240001204      ******************************************************************
019250001204      /
019260001204       01  WS-STMT-SPLS-MSG.
019270001204
019280001204      *****
019290001204      *****    English Messages.
019300001204      *****
019310001204
019320001204           03  WS-SPLS-MSG01-E.
019330020904R14493         05  FILLER              PICTURE X(5).
019340020904               05  WS-SPLS-MSG01-1     PICTURE X(50)       VALUE
019350020904                   "Your plan performance has been calculated using th".
019360001204               05  WS-SPLS-MSG01-2     PICTURE X(50)       VALUE
019370020904                   "e Time-Weighted formula. This formula takes into a".
019380001204               05  WS-SPLS-MSG01-3     PICTURE X(40)       VALUE
019390020904                   "ccount the amount of                   ".
019400001204
019410020904           03  WS-SPLS-MSG02-E.
019420001204               05  FILLER              PICTURE X(10).
019430001204               05  WS-SPLS-MSG02-1     PICTURE X(50)       VALUE
019440020904                   "time you have been invested in the fund(s) and how".
019450001204               05  WS-SPLS-MSG02-2     PICTURE X(50)       VALUE
019460030106R16015             " well they have performed over a given time period".
019470001204               05  WS-SPLS-MSG02-3     PICTURE X(40)       VALUE
019480030106R16015             ". It is not influenced                  ".
019490001204
019500010910           03  WS-SPLS-MSG03-E.
019510010910               05  FILLER              PICTURE X(10).
019520010910               05  WS-SPLS-MSG03-1     PICTURE X(50)       VALUE
019530030106R16015             "by the amount or timing of investments. Your actua".
019540010910               05  WS-SPLS-MSG03-2     PICTURE X(50)       VALUE
019550020904                   "l personal rate of return could be quite different".
019560010910               05  WS-SPLS-MSG03-3     PICTURE X(40)       VALUE
019570020904                   " if you have made a                    ".
019580010910
019590010910           03  WS-SPLS-MSG04-E.
019600010910               05  FILLER              PICTURE X(10).
019610010910               05  WS-SPLS-MSG04-1     PICTURE X(50)       VALUE
019620020904                   "number of deposits and withdrawals. Returns for pe".
019630010910               05  WS-SPLS-MSG04-2     PICTURE X(50)       VALUE
019640020904                   "riods longer than one year are annualized. Past pe".
019650010910               05  WS-SPLS-MSG04-3     PICTURE X(40)       VALUE
019660020904                   "rformance does not                     ".
019670010910
019680010910           03  WS-SPLS-MSG05-E.
019690010910               05  FILLER              PICTURE X(10).
019700010910               05  WS-SPLS-MSG05-1     PICTURE X(50)       VALUE
019710020904                   "guarantee future results. Your fund unit values an".
019720010910               05  WS-SPLS-MSG05-2     PICTURE X(50)       VALUE
019730020904                   "d investment returns will fluctuate.              ".
019740010910               05  WS-SPLS-MSG05-3     PICTURE X(40)       VALUE
019750020904                   "                                       ".
019760010910
019770001204      *****
019780001204      *****    French Messages.
019790001204      *****
019800001204
019810001204           03  WS-SPLS-MSG01-F.
019820020904               05  FILLER              PICTURE X(10).
019830001204               05  WS-SPLS-MSG01-1     PICTURE X(50)       VALUE
019840020904                   "Le taux de rendement de votre contrat a été calcul".
019850001204               05  WS-SPLS-MSG01-2     PICTURE X(50)       VALUE
019860020904                   "é à l'aide de la formule pondérée en fonction du t".
019870020904               05  WS-SPLS-MSG01-3     PICTURE X(40)       VALUE
019880020904                   "emps. Cette formule ne                  ".
019890001204
019900001204           03  WS-SPLS-MSG02-F.
019910001204               05  FILLER              PICTURE X(10).
019920001204               05  WS-SPLS-MSG02-1     PICTURE X(50)       VALUE
019930020904                   "tient pas compte du calendrier ou du montant de vo".
019940001204               05  WS-SPLS-MSG02-2     PICTURE X(50)       VALUE
019950020904                   "s placements, ni des retraits éventuels. Le résult".
019960020904               05  WS-SPLS-MSG02-3     PICTURE X(40)       VALUE
019970020904                   "at peut donc différer                   ".
019980001204
019990011026           03  WS-SPLS-MSG03-F.
020000010910               05  FILLER              PICTURE X(10).
020010010910               05  WS-SPLS-MSG03-1     PICTURE X(50)       VALUE
020020030328R16798             "du rendement réel de vos placements. Les résultats".
020030010910               05  WS-SPLS-MSG03-2     PICTURE X(50)       VALUE
020040020904                   " de périodes de plus d'un an sont annualisés. Les ".
020050010910               05  WS-SPLS-MSG03-3     PICTURE X(40)       VALUE
020060020904                   "rendements passés ne                    ".
020070010910
020080010910           03  WS-SPLS-MSG04-F.
020090010910               05  FILLER              PICTURE X(10).
020100010910               05  WS-SPLS-MSG04-1     PICTURE X(50)       VALUE
020110020904                   "sont pas une indication des rendements à venir. La".
020120010910               05  WS-SPLS-MSG04-2     PICTURE X(50)       VALUE
020130030403                   " valeur unitaire des fonds et le rendement de vos ".
020140010910               05  WS-SPLS-MSG04-3     PICTURE X(40)       VALUE
020150020904                   "placements fluctueront.                 ".
020160010910
020170010910           03  WS-SPLS-MSG05-F.
020180010910               05  FILLER              PICTURE X(10).
020190010910               05  WS-SPLS-MSG05-1     PICTURE X(50)       VALUE
020200020904                   "                                                  ".
020210010910               05  WS-SPLS-MSG05-2     PICTURE X(50)       VALUE
020220020904                   "                                                  ".
020230010910               05  WS-SPLS-MSG05-3     PICTURE X(40)       VALUE
020240020904R14493             "                                       ".
020250010910
020260001204
020270001204      *****
020280001204      *****    Statement Plan Summary Messages.
020290001204      *****
020300001204
020310001204       01  WS-STMT-SPLS-MSG-R          REDEFINES WS-STMT-SPLS-MSG.
020320001204           03  WS-STMT-SPLS-MSG-LNG    OCCURS   2  TIMES
020330001204                                       INDEXED BY  SPSM.
020340001204               05  WS-SPSML01          PICTURE X(150).
020350001204               05  WS-SPSML02          PICTURE X(150).
020360001204               05  WS-SPSML03          PICTURE X(150).
020370001207               05  WS-SPSML04          PICTURE X(150).
020380010910r11175         05  WS-SPSML05          PICTURE X(150).
020390001204
020400001207      /
020410001207      ******************************************************************
020420001207      *                                                                *
020430001207      *    S T A T E M E N T   P L A N   S U M M A R Y   S 2   L I T   *
020440001207      *                                                                *
020450001207      ******************************************************************
020460001207      /
020470001207       01  WS-STMT-PSS2-LITS.
020480001207
020490001207      *****
020500001207      *****    English Literals.
020510001207      *****
020520001207
020530001207001207     03  WS-SPS2-COLLIT01-E.
020540001207               05  FILLER              PICTURE X(50)       VALUE
020550001207                   "CHANGE IN THE VALUE OF YOUR PLAN                  ".
020560001207
020570001207           03  WS-SPS2-COLLIT02-E.
020580001207               05  FILLER              PICTURE X(50)       VALUE
020590001207                   "           SINCE ISSUE                            ".
020600001207
020610001207           03  WS-SPS2-COLLIT03-E.
020620001207               05  FILLER              PICTURE X(50)       VALUE
020630010912R11175             "Accumulated Value of your plan on:                ".
020640001207
020650001207           03  WS-SPS2-COLLIT04-E.
020660001207               05  FILLER              PICTURE X(50)       VALUE
020670001207                   "Total Deposits:                                   ".
020680001207
020690001207           03  WS-SPS2-COLLIT05-E.
020700001207               05  FILLER              PICTURE X(50)       VALUE
020710001207                   "Total Withdrawals:                                ".
020720001207
020730001207           03  WS-SPS2-COLLIT06-E.
020740001207               05  FILLER              PICTURE X(50)       VALUE
020750001207                   "Deposits minus withdrawals:                       ".
020760001207
020770001207           03  WS-SPS2-COLLIT07-E.
020780001207               05  FILLER              PICTURE X(50)       VALUE
020790001207                   "Accumulated Value of your plan on:                ".
020800001207
020810001214           03  WS-SPS2-COLLIT08-E.
020820001214               05  FILLER              PICTURE X(50)       VALUE
020830001214                   "TOTAL PLAN:                                       ".
020840010912
020850010913R11175     03  WS-SPS2-COLLIT09-E.
020860010913               05  FILLER              PICTURE X(52)       VALUE
020870010913                 "Increase/Decrease in Accumulated Value of your plan:".
020880010913
020890001207      *****
020900001207      *****    French Literals.
020910001207      *****
020920001207
020930001207001207     03  WS-SPS2-COLLIT01-F.
020940001207               05  FILLER              PICTURE X(50)       VALUE
020950010910R11175             "EVOLUTION DE LA VALEUR DU CONTRAT                 ".
020960001207
020970001207           03  WS-SPS2-COLLIT02-F.
020980001207               05  FILLER              PICTURE X(50)       VALUE
020990001207                   "DEPUIS LA SOUSCRIPTION                            ".
021000001207
021010001207           03  WS-SPS2-COLLIT03-F.
021020001207               05  FILLER              PICTURE X(50)       VALUE
021030010912R11175             "Valeur acquise du contrat au:                     ".
021040001207
021050001207           03  WS-SPS2-COLLIT04-F.
021060001207               05  FILLER              PICTURE X(50)       VALUE
021070001207                   "Total des dépôts:                                 ".
021080001207
021090001207           03  WS-SPS2-COLLIT05-F.
021100001207               05  FILLER              PICTURE X(50)       VALUE
021110001207                   "Total des retraits:                               ".
021120001207
021130001207           03  WS-SPS2-COLLIT06-F.
021140001207               05  FILLER              PICTURE X(50)       VALUE
021150010424RF9415             "Dépôts moins retraits:                            ".
021160001207
021170001207           03  WS-SPS2-COLLIT07-F.
021180001207               05  FILLER              PICTURE X(50)       VALUE
021190010424RF9415             "Valeur acquise du contrat au:                     ".
021200001207
021210001214           03  WS-SPS2-COLLIT08-F.
021220001214               05  FILLER              PICTURE X(50)       VALUE
021230001214                   "TOTAL DU CONTRAT:                                 ".
021240001214
021250010913R11175     03  WS-SPS2-COLLIT09-F.
021260010913               05  FILLER              PICTURE X(52)       VALUE
021270011206R12327           "Hausse/Baisse de la valeur acquise du contrat:      ".
021280010913
021290001207      *****
021300001207      *****    Statement Heading Literals.
021310001207      *****
021320001207
021330001207       01  WS-STMT-PSS2-LITS-R         REDEFINES WS-STMT-PSS2-LITS.
021340001207           03  WS-STMT-PSS2-LITSLAN    OCCURS   2  TIMES
021350001207                                       INDEXED BY  SPS2.
021360001207001207         05  WS-SPSS2L01         PICTURE X(50).
021370001207               05  WS-SPSS2L02         PICTURE X(50).
021380001207               05  WS-SPSS2L03         PICTURE X(50).
021390001207               05  WS-SPSS2L04         PICTURE X(50).
021400001207               05  WS-SPSS2L05         PICTURE X(50).
021410001207               05  WS-SPSS2L06         PICTURE X(50).
021420001207               05  WS-SPSS2L07         PICTURE X(50).
021430001214               05  WS-SPSS2L08         PICTURE X(50).
021440010913R11175         05  WS-SPSS2L09         PICTURE X(52).
021450001207
021460990623      /
021470000000      ******************************************************************
021480000000      *                                                                *
021490000000      *    S T A T E M E N T   L I T E R A L S                         *
021500000000      *                                                                *
021510000000      ******************************************************************
021520000000
021530000000       01  WS-STMT-LITS.
021540000000
021550000000      *****
021560000000      *****    English Literals.
021570000000      *****
021580000000
021590000000           03  WS-SH01-LIT1-E          PICTURE X(20)       VALUE
021600000000               "  TRANSACTIONS FROM ".
021610000000           03  WS-SH01-LIT2-E          PICTURE X(2)        VALUE "TO".
021620010910R11175     03  WS-SH03-LIT1-E          PICTURE X(23)       VALUE
021630010910R11175         "   INDEPENDENT ADVISOR:".
021640010427           03  WS-SH05-LIT1-E          PICTURE X(22)       VALUE
021650010427               "           ISSUE DATE:".
021660010427           03  WS-SH06-LIT1-E          PICTURE X(22)       VALUE
021670010427               "         ANNUITY DATE:".
021680010427           03  WS-SH07-LIT1-E          PICTURE X(22)       VALUE
021690010427R9415A         "            ANNUITANT:".
021700990923           03  WS-SH09-LIT1-E          PICTURE X(30)       VALUE
021710990925               "     UNITHOLDER STATEMENT     ".
021720000000           03  WS-SH10-LIT1-E          PICTURE X(47)       VALUE
021730000000               "PLEASE NOTE THAT THIS ACCOUNT IS REGISTERED TO:".
021740990923           03  WS-SH10-LIT2-E          PICTURE X(12)       VALUE
021750990923               "ACCOUNT NO :".
021760000000           03  WS-SD02-LIT1-E          PICTURE X(6)        VALUE
021770000000               "YYMMDD".
021780991019           03  WS-SD02V7-LIT1-E        PICTURE X(8)        VALUE
021790991019               "YYYYMMDD".
021800991213RF6007     03  WS-SD02V8-LIT1-E        PICTURE X(8)        VALUE
021810991213               "YYYYMMDD".
021820000000           03  WS-SS01-LIT1-E.
021830000000               05  FILLER              PICTURE X(49)       VALUE
021840001205                   "                                                 ".
021850000000               05  FILLER              PICTURE X(39)       VALUE
021860000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
021870000000               05  FILLER              PICTURE X(50)       VALUE
021880001205                   "                                                  ".
021890001205
021900991213RF6007     03  WS-SS01V8-LIT1-E.
021910991213               05  FILLER              PICTURE X(51)       VALUE
021920001205                 "                                                   ".
021930991213               05  FILLER              PICTURE X(39)       VALUE
021940991213                   "    STATEMENT DATE ACCOUNT POSITION    ".
021950991213               05  FILLER              PICTURE X(50)       VALUE
021960001205                   "                                                  ".
021970990923
021980000000           03  WS-SS02-LIT0-E          PICTURE X(11)       VALUE
021990000000               "        NET".
022000000000           03  WS-SS02-LIT1-E          PICTURE X(8)        VALUE
022010000000               "    UNIT".
022020000000           03  WS-SS02-LIT2-E          PICTURE X(13)       VALUE
022030000000               "         UNIT".
022040000000           03  WS-SS02-LIT3-E          PICTURE X(13)       VALUE
022050000000               "       MARKET".
022060000000           03  WS-SS02-LIT4-E          PICTURE X(8)        VALUE
022070000000               " AVERAGE".
022080010912R11175     03  WS-SS02-LIT5-E          PICTURE X(13)       VALUE
022090010912               "   % OF TOTAL".
022100000000           03  WS-SS03-LIT0-E          PICTURE X(11)       VALUE
022110000000               "   INVESTED".
022120000000           03  WS-SS03-LIT1-E          PICTURE X(8)        VALUE
022130000000               "   PRICE".
022140000000           03  WS-SS03-LIT2-E          PICTURE X(13)       VALUE
022150000000               "      BALANCE".
022160000000           03  WS-SS03-LIT3-E          PICTURE X(13)       VALUE
022170000000               "        VALUE".
022180000000           03  WS-SS03-LIT4-E          PICTURE X(8)        VALUE
022190000000               "    COST".
022200010912R11175     03  WS-SS03-LIT5-E          PICTURE X(13)       VALUE
022210010912               "        VALUE".
022220000000           03  WS-SS05-LIT1-E          PICTURE X(14)       VALUE
022230000000               " -------------".
022240010912R11175     03  WS-SS05-LIT2-E          PICTURE X(6)        VALUE
022250010912               "------".
022260000000           03  WS-SS05V1-LIT1-E        PICTURE X(13)       VALUE
022270000000               " ------------".
022280990611           03  WS-SS05V2-LIT1-E        PICTURE X(13)       VALUE
022290990226               " ------------".
022300000000           03  WS-SS05V3-LIT1-E        PICTURE X(14)       VALUE
022310000000               " -------------".
022320000000           03  WS-SS05V4-LIT1-E        PICTURE X(14)       VALUE
022330000000               " -------------".
022340000000           03  WS-SS05V5-LIT1-E        PICTURE X(14)       VALUE
022350000000               " -------------".
022360990922           03  WS-SS05V6-LIT1-E        PICTURE X(14)       VALUE
022370990922               " -------------".
022380000000           03  WS-SS06-LIT1-E          PICTURE X(35)       VALUE
022390000000               "ACCOUNT VALUE                      ".
022400981224           03  WS-SS07V1-LIT-E1          PICTURE X(25)       VALUE
022410981224               "MINIMUM RRIF PAYMENT:    ".
022420981224           03  WS-SS07V1-LIT-E2          PICTURE X(25)       VALUE
022430981224               "MINIMUM LIF PAYMENT :    ".
022440981224           03  WS-SS08V1-LIT-E           PICTURE X(25)       VALUE
022450981224               "MAXIMUM LIF PAYMENT :    ".
022460981207           03  WS-SS11-LIT1-E          PICTURE X(29)       VALUE
022470971223               "CANADIAN DOLLAR CAPITAL GAINS".
022480000000
022490000000      *****
022500000000      *****    French Literals.
022510000000      *****
022520000000
022530000000           03  WS-SH01-LIT1-F          PICTURE X(20)       VALUE
022540010424RF9415         "     TRANSACTIONS DU".
022550010424RF9415     03  WS-SH01-LIT2-F          PICTURE X(2)        VALUE "AU".
022560010910R11175     03  WS-SH03-LIT1-F          PICTURE X(23)       VALUE
022570011206R12327         "CONSEILLER INDEPENDANT:".
022580010427           03  WS-SH05-LIT1-F          PICTURE X(22)       VALUE
022590010427               " DATE DE SOUSCRIPTION:".
022600010427           03  WS-SH06-LIT1-F          PICTURE X(22)       VALUE
022610010427               "     JOUISSANCE RENTE:".
022620010427           03  WS-SH07-LIT1-F          PICTURE X(22)       VALUE
022630010427R9415A         "              RENTIER:".
022640990923           03  WS-SH09-LIT1-F          PICTURE X(30)       VALUE
022650990925               "     UNITHOLDER STATEMENT     ".
022660030328           03  WS-SH10-LIT1-F          PICTURE X(47)       VALUE
022670030328               "VEUILLEZ NOTER CE COMPTE EST ENREGISTRE A      ".
022680990923           03  WS-SH10-LIT2-F          PICTURE X(12)       VALUE
022690990923               "ACCOUNT NO :".
022700000000           03  WS-SD02-LIT1-F          PICTURE X(6)        VALUE
022710000000               "AAMMJJ".
022720991019           03  WS-SD02V7-LIT1-F        PICTURE X(8)        VALUE
022730991019               "YYYYMMDD".
022740991213           03  WS-SD02V8-LIT1-F        PICTURE X(8)        VALUE
022750991213               "YYYYMMDD".
022760000000           03  WS-SS01-LIT1-F.
022770000000               05  FILLER              PICTURE X(47)       VALUE
022780001205                   "                                               ".
022790000000               05  FILLER              PICTURE X(43)       VALUE
022800010424RF9415             "       ETAT DU COMPTE A LA DATE DU RELEVE  ".
022810000000               05  FILLER              PICTURE X(48)       VALUE
022820001205                   "                                                ".
022830000223
022840991213           03  WS-SS01V8-LIT1-F.
022850991213               05  FILLER              PICTURE X(49)       VALUE
022860001205                  "                                                 ".
022870991213               05  FILLER              PICTURE X(43)       VALUE
022880010424RF9415             "       ETAT DU COMPTE A LA DATE DU RELEVE  ".
022890991213               05  FILLER              PICTURE X(48)       VALUE
022900001205                   "                                                ".
022910990929
022920971223           03  WS-SS02-LIT0-F          PICTURE X(11)       VALUE
022930971223               "    MONTANT".
022940000000           03  WS-SS02-LIT1-F          PICTURE X(8)        VALUE
022950000000               " PRIX DE".
022960000000           03  WS-SS02-LIT2-F          PICTURE X(13)       VALUE
022970000000               "      BALANCE".
022980000000           03  WS-SS02-LIT3-F          PICTURE X(13)       VALUE
022990000000               "    VALEUR DU".
023000000000           03  WS-SS02-LIT4-F          PICTURE X(8)        VALUE
023010000000               "    COUT".
023020010912R11175     03  WS-SS02-LIT5-F          PICTURE X(13)       VALUE
023030010912               "%DE LA VALEUR".
023040000000           03  WS-SS03-LIT0-F          PICTURE X(11)       VALUE
023050000000               "NET INVESTI".
023060000000           03  WS-SS03-LIT1-F          PICTURE X(8)        VALUE
023070000000               " L'UNITE".
023080000000           03  WS-SS03-LIT2-F          PICTURE X(13)       VALUE
023090000000               "   DE L'UNITE".
023100000000           03  WS-SS03-LIT3-F          PICTURE X(13)       VALUE
023110000000               "       MARCHE".
023120000000           03  WS-SS03-LIT4-F          PICTURE X(8)        VALUE
023130000000               "   MOYEN".
023140010912R11175     03  WS-SS03-LIT5-F          PICTURE X(13)       VALUE
023150010912               "       TOTALE".
023160000000           03  WS-SS05-LIT1-F          PICTURE X(14)       VALUE
023170000000               " -------------".
023180010912R11175     03  WS-SS05-LIT2-F          PICTURE X(6)        VALUE
023190010912               "------".
023200000000           03  WS-SS05V1-LIT1-F        PICTURE X(13)       VALUE
023210000000               " ------------".
023220990611           03  WS-SS05V2-LIT1-F        PICTURE X(13)       VALUE
023230990226               " ------------".
023240000000           03  WS-SS05V3-LIT1-F        PICTURE X(14)       VALUE
023250000000               " -------------".
023260000000           03  WS-SS05V4-LIT1-F        PICTURE X(14)       VALUE
023270000000               " -------------".
023280000000           03  WS-SS05V5-LIT1-F        PICTURE X(14)       VALUE
023290000000               " -------------".
023300990922           03  WS-SS05V6-LIT1-F        PICTURE X(14)       VALUE
023310990922               " -------------".
023320000000           03  WS-SS06-LIT1-F          PICTURE X(35)       VALUE
023330000000               "VALEUR DU COMPTE                   ".
023340981224           03  WS-SS07V1-LIT-F1          PICTURE X(25)       VALUE
023350981224               "VERSEMENT MINIMUM DE FER:".
023360981224           03  WS-SS07V1-LIT-F2          PICTURE X(25)       VALUE
023370981224               "VERSEMENT MINIMUM DE FRV:".
023380981224           03  WS-SS08V1-LIT-F           PICTURE X(25)       VALUE
023390981224               "VERSEMENT MAXIMUM DE FRV:".
023400981207           03  WS-SS11-LIT1-F          PICTURE X(29)       VALUE
023410971223               "GAINS EN CAPITAL EN CAN $    ".
023420000000
023430000000       01  WS-STMT-LITS-R              REDEFINES WS-STMT-LITS.
023440000000           03  WS-STMT-LIT             OCCURS   2  TIMES
023450000000                                       INDEXED BY  SL.
023460000000
023470000000      *****
023480000000      *****    Heading Literals.
023490000000      *****
023500000000
023510000000               05  WS-SH01-LIT1        PICTURE X(20).
023520000000               05  WS-SH01-LIT2        PICTURE X(2).
023530010910R11175         05  WS-SH03-LIT1        PICTURE X(23).
023540010427               05  WS-SH05-LIT1        PICTURE X(22).
023550010427               05  WS-SH06-LIT1        PICTURE X(22).
023560010427R9415A         05  WS-SH07-LIT1        PICTURE X(22).
023570990923               05  WS-SH09-LIT1        PICTURE X(30).
023580030328               05  WS-SH10-LIT1        PICTURE X(47).
023590990923               05  WS-SH10-LIT2        PICTURE X(12).
023600000000
023610000000      *****
023620000000      *****    Detail Literals.
023630000000      *****
023640000000
023650000000               05  WS-SD02-LIT1        PICTURE X(6).
023660991019               05  WS-SD02V7-LIT1      PICTURE X(8).
023670991213RF6007         05  WS-SD02V8-LIT1      PICTURE X(8).
023680000000
023690000000      *****
023700000000      *****    Summary Literals.
023710000000      *****
023720000000
023730000000               05  WS-SS01-LIT1        PICTURE X(138).
023740991213RF6007         05  WS-SS01V8-LIT1      PICTURE X(140).
023750000000               05  WS-SS02-LIT0        PICTURE X(11).
023760000000               05  WS-SS02-LIT1        PICTURE X(8).
023770000000               05  WS-SS02-LIT2        PICTURE X(13).
023780000000               05  WS-SS02-LIT3        PICTURE X(13).
023790000000               05  WS-SS02-LIT4        PICTURE X(8).
023800010912R11175         05  WS-SS02-LIT5        PICTURE X(13).
023810000000               05  WS-SS03-LIT0        PICTURE X(11).
023820000000               05  WS-SS03-LIT1        PICTURE X(8).
023830000000               05  WS-SS03-LIT2        PICTURE X(13).
023840000000               05  WS-SS03-LIT3        PICTURE X(13).
023850000000               05  WS-SS03-LIT4        PICTURE X(8).
023860010912R11175         05  WS-SS03-LIT5        PICTURE X(13).
023870000000               05  WS-SS05-LIT1        PICTURE X(14).
023880010912R11175         05  WS-SS05-LIT2        PICTURE X(6).
023890000000               05  WS-SS05V1-LIT1      PICTURE X(13).
023900990226               05  WS-SS05V2-LIT1      PICTURE X(13).
023910000000               05  WS-SS05V3-LIT1      PICTURE X(14).
023920000000               05  WS-SS05V4-LIT1      PICTURE X(14).
023930000000               05  WS-SS05V5-LIT1      PICTURE X(14).
023940990922               05  WS-SS05V6-LIT1      PICTURE X(14).
023950000000               05  WS-SS06-LIT1        PICTURE X(35).
023960981224               05  WS-SS07V1-LIT1      PICTURE X(25).
023970981224               05  WS-SS07V1-LIT2      PICTURE X(25).
023980981224               05  WS-SS08V1-LIT       PICTURE X(25).
023990981207               05  WS-SS11-LIT1        PICTURE X(29).
024000000000      /
024010000000      ******************************************************************
024020000000      *                                                                *
024030000000      *    S T A T E M E N T   L I T E R A L S   ( C U R R E N C Y )   *
024040000000      *                                                                *
024050000000      ******************************************************************
024060000000
024070000000       01  WS-CURRENCY-TBL.
024080000000           03  WS-SC-CAD.
024090000000               05  FILLER              PICTURE X(3)        VALUE "CAD".
024100000000               05  FILLER              PICTURE X(30)       VALUE
024110000000                   "CANADIAN (CAD) DOLLAR TOTAL   ".
024120000000               05  FILLER              PICTURE X(30)       VALUE
024130000000                   "TOTAL DOLLARS CANADIENS (CAD) ".
024140010912R11175*        05  FILLER              PICTURE X(8).
024150010912R11175         05  FILLER              PICTURE X(11).
024160000000           03  WS-SC-USD.
024170000000               05  FILLER              PICTURE X(3)        VALUE "USD".
024180000000               05  FILLER              PICTURE X(30)       VALUE
024190000000                   "US (USD) DOLLAR TOTAL         ".
024200000000               05  FILLER              PICTURE X(30)       VALUE
024210000000                   "TOTAL DOLLARS AMERICAINS (USD)".
024220010912R11175*        05  FILLER              PICTURE X(8).
024230010912R11175         05  FILLER              PICTURE X(11).
024240000000
024250000000       01  WS-CURRENCY-TBL-R           REDEFINES WS-CURRENCY-TBL.
024260000000           03  WS-SC-ENTRY             OCCURS   2  TIMES
024270000000                                       INDEXED BY  SC.
024280000000             04  WS-SC-CURRENCY        PICTURE X(3).
024290000000             04  WS-SC-CURRDESC        OCCURS   2  TIMES
024300000000                                       INDEXED BY  SCL
024310000223                                       PICTURE X(30).
024320000223             04  WS-SC-MARKET-VALUE    PICTURE S9(11)V9(2) COMP-3.
024330000000             04  WS-SC-RPTIND          PICTURE X(1).
024340010912             04  WS-SC-MARKET-PERC     PICTURE S9(3)V9(2) COMP-3.
024350000000      /
024360000000       01  WS-STMT-INVESTOR-ACCOUNT-TBL.
024370000000
024380000000      ******************************************************************
024390000000      *                                                                *
024400000000      *    S T A T E M E N T   I N V E S T O R / A C C O U N T   T B L *
024410000000      *                                                                *
024420000000      ******************************************************************
024430000000
024440011211           03  WS-SIA-ENT              OCCURS        1000  TIMES
024450000000                                       ASCENDING KEY   IS  WS-SIA-ACCT
024460000000                                       INDEXED         BY  SIA
024470000000                                                           SIA1
024480000000                                                           SIA2.
024490000000               05  WS-SIA-ACCT         PICTURE X(9).
024500000000               05  WS-SIA-NOMCDE       PICTURE X(5).
024510000000               05  WS-SIA-NOMNAM       PICTURE X(35).
024520000000               05  WS-SIA-NOMACCT      PICTURE X(15).
024530000000               05  WS-SIA-REGCTB       PICTURE 9(9)V9(2)   COMP-3.
024540000000               05  WS-SIA-REGCTBSGN    PICTURE X(1).
024550001228001229         05  WS-SIA-DEPOSIT-CURRENT  PICTURE 9(11)V9(2)  COMP-3.
024560001228001229         05  WS-SIA-DEPOSIT-INCEPT   PICTURE 9(11)V9(2)  COMP-3.
024570001228001229         05  WS-SIA-WITHDRAW-CURRENT PICTURE 9(11)V9(2)  COMP-3.
024580001228001229         05  WS-SIA-WITHDRAW-INCEPT  PICTURE 9(11)V9(2)  COMP-3.
024590010419RF9415         05  WS-SIA-MAIL-FLAG    PICTURE X(1).
024600010419
024610000000      /
024620000000       01  WS-STMT-ACCOUNT-FUND-TABLE.
024630000000
024640000000      ******************************************************************
024650000000      *                                                                *
024660000000      *    S T A T E M E N T   A C C O U N T / F U N D   T A B L E     *
024670000000      *                                                                *
024680000000      ******************************************************************
024690000000
02470001121112090 *    03  WS-STMT-ACCOUNT-FUND    OCCURS 100  TIMES
024710161012      * RFS156599 START
02472016101212090 *    03  WS-STMT-ACCOUNT-FUND    OCCURS 500  TIMES
024730161108164194*    03  WS-STMT-ACCOUNT-FUND    OCCURS 900  TIMES
024740161108164194     03  WS-STMT-ACCOUNT-FUND    OCCURS 2400 TIMES
024750000000                                       INDEXED BY  SAF.
024760161012      * RFS156599 END
024770000000               05  WS-SAF-ACB-UNIT-PRICE
024780000222                                       PICTURE S9(5)V9(4)  COMP-3.
024790000000               05  WS-SAF-ACB-UNIT-PRICE-SIGN
024800000000                                       PICTURE X(1).
024810000000               05  WS-SAF-CURRENCY     PICTURE X(3).
024820000000               05  WS-SAF-FUND-NO      PICTURE X(8).
024830000000               05  WS-SAF-INVESTMENT-NAME
024840000000                                       PICTURE X(35).
024850000000               05  WS-SAF-LAST-UNIT-PRICE
024860000222                                       PICTURE S9(5)V9(4)  COMP-3.
024870000000               05  WS-SAF-LAST-UNIT-PRICE-SIGN
024880000000                                       PICTURE X(1).
024890000222               05  WS-SAF-MARKET-VALUE PICTURE S9(11)V9(2)  COMP-3.
024900000000               05  WS-SAF-MARKET-VALUE-SIGN
024910000000                                       PICTURE X(1).
024920000000               05  WS-SAF-NET-INVESTED PICTURE S9(9)V9(2)  COMP-3.
024930000000               05  WS-SAF-NET-INVESTED-SIGN
024940000000                                       PICTURE X(1).
024950000000               05  WS-SAF-NO-OF-UNITS  PICTURE S9(10)V9(3) COMP-3.
024960000000               05  WS-SAF-NO-OF-UNITS-SIGN
024970000000                                       PICTURE X(1).
024980010912R11175         05  WS-SAF-MARKET-PERC  PICTURE S9(3)V9(2) COMP-3.
024990001129
025000001130      /
025010001130      ******************************************************************
025020001130      *                                                                *
025030001130      *    S T A T E M E N T   A C C O U N T / F U N D   T A B L E     *
025040001130      *                                                                *
025050001130      ******************************************************************
025060001130
025070001130001130 01  WS-ACCOUNT-FUND-TABLE2.
025080010309      *    03  WS-ACCOUNT-DETAILS-SECTION  PIC X(850) OCCURS 100 TIMES
025090090824R17165*    03  WS-ACCOUNT-DETAILS-SECTION  PIC X(1350) OCCURS 100 TIMES
025100090824R71771     03  WS-ACCOUNT-DETAILS-SECTION  PIC X(1350) OCCURS 300 TIMES
025110010309                                                       INDEXED BY SAF2.
025120001130
025130001129      /
025140001129      ******************************************************************
025150001129      *                                                                *
025160001129      *    A C C O U N T / F U N D   P E R F O R M A N C E  T A B L E  *
025170001129      *                                                                *
025180001129      ******************************************************************
025190001129
025200001129001129 01  WS-ACCOUNT-FUND-PERFORM-TABLE.
025210010309      *    03  WS-ACCOUNT-FUND-PERFORM     PIC X(850) OCCURS 100 TIMES
025220090824R17165*    03  WS-ACCOUNT-FUND-PERFORM     PIC X(1350) OCCURS 100 TIMES
025230090824R71771     03  WS-ACCOUNT-FUND-PERFORM     PIC X(1350) OCCURS 300 TIMES
025240010309                                                       INDEXED BY AFP.
025250000000      /
025260991103
025270991103      *****
025280991103      *****    Account Type Table.
025290991103      *****    - This table contains only two fields: account type
025300991103      *****      description and accout type rule.
025310991103      *****
025320991103
025330991103       01  WS-ACCTYP-TBL.
02534001121212090 *    04  WS-ACCTYP-ENT         OCCURS  50  TIMES
02535001121212090      04  WS-ACCTYP-ENT         OCCURS 250  TIMES
025360991103                                     INDEXED BY  AT1.
025370991103               05  WS-AT-CODE          PICTURE X(5).
025380991103               05  WS-AT-RULE          PICTURE X(1).
025390991103
025400991103       01  WS-AT-LST               PICTURE S9(3)   COMP-3.
025410991103       01  WS-AT-MAX               PICTURE S9(3)   COMP-3
02542001121212090 *                                                    VALUE 50.
02543001121212090                                                     VALUE 250.
025440991103
025450991103
025460991103
025470000000      ******************************************************************
025480000000      *                                                                *
025490000000      *    H E A D I N G   P R I N T   L I N E S                       *
025500000000      *                                                                *
025510000000      ******************************************************************
025520000000
025530000000       01  HEADING-LINE-1.
025540000000           05  FILLER                  PICTURE X(11)       VALUE
025550000000               "DATE     : ".
025560000104           03  HL1-YY                  PICTURE 9(4).
025570000000           03  FILLER                  PICTURE X(1)        VALUE "/".
025580000000           03  HL1-MM                  PICTURE 9(2).
025590000000           03  FILLER                  PICTURE X(1)        VALUE "/".
025600000000           03  HL1-DD                  PICTURE 9(2).
025610000104           03  FILLER                  PICTURE X(93)       VALUE SPACES.
025620000000           03  FILLER                  PICTURE X(6)        VALUE
025630000000               "TIME: ".
025640000000           03  HL1-TIME                PICTURE 99B99B99B99.
025650000000           03  HL1-TIME-R              REDEFINES HL1-TIME.
025660000000               05  FILLER              PICTURE X(8).
025670000000               05  HL1-TIME-DOT        PICTURE X(1).
025680000000               05  FILLER              PICTURE X(2).
025690000000           03  FILLER                  PICTURE X(19)       VALUE SPACES.
025700000000
025710000000       01  HEADING-LINE-2.
025720000000           03  FILLER                  PICTURE X(11)       VALUE
025730000000               "REPORT NO: ".
025740000000           03  FILLER                  PICTURE X(10)       VALUE
025750010427               "STMTPRT2  ".
025760000000           03  FILLER                  PICTURE X(15)       VALUE SPACES.
025770000000           03  FILLER                  PICTURE X(30)       VALUE
025780000000               "STATEMENT PRINT CONTROL REPORT".
025790000000           03  FILLER                  PICTURE X(48)       VALUE SPACES.
025800000000           03  FILLER                  PICTURE X(5)        VALUE
025810000000               "PAGE:".
025820000000           03  FILLER                  PICTURE X(8)        VALUE SPACES.
025830000000           03  HL2-PGECNT              PICTURE ZZZZ9.
025840000000           03  FILLER                  PICTURE X(18)       VALUE SPACES.
025850000000
025860000000       01  HEADING-LINE-3.
025870000000           03  FILLER                  PICTURE X(3)        VALUE SPACES.
025880000000           03  FILLER                  PICTURE X(9)        VALUE
025890000000               "INVESTOR#".
025900000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
025910000000           03  FILLER                  PICTURE X(9)        VALUE
025920000000               "ACCOUNT  ".
025930000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
025940000000           03  FILLER                  PICTURE X(9)        VALUE
025950000000               "SECTION  ".
025960000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
025970000000           03  FILLER                  PICTURE X(3)        VALUE "LNG".
025980000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
025990000000           03  FILLER                  PICTURE X(6)        VALUE
026000000000               "DEALER".
026010000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
026020000000           03  FILLER                  PICTURE X(5)        VALUE
026030000000               "REP. ".
026040000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
026050000000           03  FILLER                  PICTURE X(50)       VALUE
026060000000               "MESSAGE                                           ".
026070000000           03  FILLER                  PICTURE X(50)       VALUE SPACES.
026080000000      /
026090000000      ******************************************************************
026100000000      *                                                                *
026110000000      *    E R R O R   P R I N T   L I N E S
026120000000      *                                                                *
026130000000      ******************************************************************
026140000000
026150000000       01  ERROR-LINE-1.
026160000000           03  FILLER                  PICTURE X(3).
026170000000           03  EL1-INVESTOR-NO         PICTURE 9(9).
026180000000           03  FILLER                  PICTURE X(1).
026190000000           03  EL1-ACCOUNT             PICTURE 9(9).
026200000000           03  FILLER                  PICTURE X(1).
026210000000           03  EL1-SECTION-NO          PICTURE 9(2).
026220000000           03  EL1-SECTION-LITDSH      PICTURE X(1).
026230000000           03  EL1-SECTION-SEQUENCE    PICTURE 9(6).
026240000000           03  FILLER                  PICTURE X(2).
026250000000           03  EL1-LANGUAGE-CODE       PICTURE X(1).
026260000000           03  FILLER                  PICTURE X(2).
026270000000           03  EL1-DEALER              PICTURE X(4).
026280000000           03  FILLER                  PICTURE X(3).
026290000222           03  EL1-DEALER-REP-CODE     PICTURE X(6).
026300000000           03  FILLER                  PICTURE X(1).
026310000000           03  EL1-MSG                 PICTURE X(50).
026320000222           03  FILLER                  PICTURE X(49).
026330000000      /
026340000000      ******************************************************************
026350000000      *                                                                *
026360000000      *    C O N T R O L   P R I N T   L I N E S                       *
026370000000      *                                                                *
026380000000      ******************************************************************
026390000000
026400000000       01  CONTROL-LINE-1.
026410000000           03  FILLER                  PICTURE X(3).
026420000000           03  CL1-LIT                 PICTURE X(50) JUST RIGHT.
026430000000           03  FILLER                  PICTURE X(1).
026440000000           03  CL1-CNT                 PICTURE ZZZ,ZZZ,ZZ9.
026450000000           03  FILLER                  PICTURE X(85).
026460000000
026470000000       01  CONTROL-LINE-2.
026480000000           03  FILLER                  PICTURE X(3)        VALUE SPACES.
026490000000           03  FILLER                  PICTURE X(40)       VALUE
026500000000               "STATEMENTS FOR MANAGEMENT COMPANY CODE: ".
026510000000           03  CL2-MANAGEMENT-COMPANY-CODE
026520000000                                       PICTURE X(3).
026530000000           03  FILLER                  PICTURE X(104)      VALUE SPACES.
026540000000      /
026550000000       01  WS-PGM-DEFIND-WORK-ITEMS.
026560000000
026570000000      ******************************************************************
026580000000      *                                                                *
026590000000      *    P R O G R A M   D E F I N E D   W O R K   I T E M S         *
026600000000      *                                                                *
026610010313      **************************************************************** *
026620000000
026630000000      *****
026640000000      *****    Control Report Counters.
026650000000      *****
026660000000
026670000000           03  WS-STMT-DTA-BCNT        PICTURE S9(7)       COMP-3.
026680000000           03  WS-STMT-DTA-BCNT-00     PICTURE S9(7)       COMP-3.
026690000000           03  WS-STMT-DTA-BCNT-05     PICTURE S9(7)       COMP-3.
026700000000           03  WS-STMT-DTA-BCNT-10     PICTURE S9(7)       COMP-3.
026710010313RF8633     03  WS-STMT-DTA-BCNT-12     PICTURE S9(7)       COMP-3.
026720000000           03  WS-STMT-DTA-BCNT-15     PICTURE S9(7)       COMP-3.
026730140826132121     03  WS-STMT-DTA-BCNT-16     PICTURE S9(7)       COMP-3.
026740000000           03  WS-STMT-DTA-BCNT-20     PICTURE S9(7)       COMP-3.
026750060618R35042     03  WS-STMT-DTA-BCNT-21     PICTURE S9(7)       COMP-3.
026760130408117038     03  WS-STMT-DTA-BCNT-22     PICTURE S9(7)       COMP-3.
026770010323RF8633     03  WS-STMT-DTA-BCNT-25     PICTURE S9(7)       COMP-3.
026780991126           03  WS-STMT-DTA-BCNT-30     PICTURE S9(7)       COMP-3.
026790991126           03  WS-STMT-DTA-BCNT-35     PICTURE S9(7)       COMP-3.
026800001114001114     03  WS-STMT-DTA-BCNT-40     PICTURE S9(7)       COMP-3.
026810001114           03  WS-STMT-DTA-BCNT-43     PICTURE S9(7)       COMP-3.
026820001114           03  WS-STMT-DTA-BCNT-45     PICTURE S9(7)       COMP-3.
026830000000           03  WS-STMT-DTA-BCNT-50     PICTURE S9(7)       COMP-3.
026840000000           03  WS-STMT-DTA-BCNT-60     PICTURE S9(7)       COMP-3.
026850991013           03  WS-STMT-DTA-BCNT-62     PICTURE S9(7)       COMP-3.
026860001114001114     03  WS-STMT-DTA-BCNT-67     PICTURE S9(7)       COMP-3.
026870000000           03  WS-STMT-DTA-BCNT-70     PICTURE S9(7)       COMP-3.
026880000000           03  WS-STMT-DTA-BCNT-82     PICTURE S9(7)       COMP-3.
026890991013           03  WS-STMT-DTA-BCNT-84     PICTURE S9(7)       COMP-3.
026900000000           03  WS-STMT-DTA-RCNT        PICTURE S9(7)       COMP-3.
026910000000           03  WS-STMT-DTA-SCNT-01     PICTURE S9(7)       COMP-3.
026920000000           03  WS-STMT-DTA-SCNT-05     PICTURE S9(7)       COMP-3.
026930000000           03  WS-STMT-DTA-SCNT-10     PICTURE S9(7)       COMP-3.
026940000000           03  WS-STMT-DTA-SCNT-20     PICTURE S9(7)       COMP-3.
026950060618R35042     03  WS-STMT-DTA-SCNT-21     PICTURE S9(7)       COMP-3.
026960130408117038     03  WS-STMT-DTA-SCNT-22     PICTURE S9(7)       COMP-3.
026970010323RF8633     03  WS-STMT-DTA-SCNT-25     PICTURE S9(7)       COMP-3.
026980001129           03  WS-STMT-DTA-SCNT-30     PICTURE S9(7)       COMP-3.
026990040406R20870     03  WS-STMT-DTA-SCNT-33     PICTURE S9(7)       COMP-3.
027000001129           03  WS-STMT-DTA-SCNT-35     PICTURE S9(7)       COMP-3.
027010040406R20870     03  WS-STMT-DTA-SCNT-38     PICTURE S9(7)       COMP-3.
027020981207           03  WS-STMT-DTA-SCNT-60     PICTURE S9(7)       COMP-3.
027030000000           03  WS-STMT-DTA-SCNT-80     PICTURE S9(7)       COMP-3.
027040001114001114     03  WS-STMT-DTA-BCNT-90     PICTURE S9(7)       COMP-3.
027050000000           03  WS-STMT-PRT-DTL-WCNT    PICTURE S9(7)       COMP-3.
027060000000           03  WS-STMT-PRT-HDR-WCNT    PICTURE S9(7)       COMP-3.
027070000000
027080000000      *****
027090000000      *****    Control Report Literals.
027100000000      *****
027110000000
027120000000           03  WS-STMT-DTA-BLIT        PICTURE X(50)       VALUE
027130000000               "STATEMENT DATA RECS BYPASSED:                     ".
027140000000           03  WS-STMT-DTA-BLIT-00     PICTURE X(50)       VALUE
027150000000               "STATEMENT DATA (DELIVERY INFO) RECS BYPASSED:     ".
027160000000           03  WS-STMT-DTA-BLIT-05     PICTURE X(50)       VALUE
027170000000               "STATEMENT DATA (DEALER) RECS BYPASSED:            ".
027180000000           03  WS-STMT-DTA-BLIT-10     PICTURE X(50)       VALUE
027190000000               "STATEMENT DATA (INVESTOR SUMMARY) RECS BYPASSED:  ".
027200010313rf8633     03  WS-STMT-DTA-BLIT-12     PICTURE X(50)       VALUE
027210010323               "STATEMENT DATA (INVESTOR SUMMARY SEC2) BYPASSED:  ".
027220000000           03  WS-STMT-DTA-BLIT-15     PICTURE X(50)       VALUE
027230000000               "STATEMENT DATA (ACCT ASSET CLASS) RECS BYPASSED:  ".
027240000000           03  WS-STMT-DTA-BLIT-20     PICTURE X(50)       VALUE
027250000000               "STATEMENT DATA (ACCOUNT DETAIL) RECS BYPASSED:    ".
027260060618R35042     03  WS-STMT-DTA-BLIT-21     PICTURE X(50)       VALUE
027270060623R35042         "STATEMENT DATA (ACCOUNT ADDITIONAL INFO) BYPASSED:".
027280130408117038     03  WS-STMT-DTA-BLIT-21     PICTURE X(50)       VALUE
027290130408117038         "STATEMENT DATA (ACCOUNT ADDITIONAL DTLS) BYPASSED:".
027300010323rf8633     03  WS-STMT-DTA-BLIT-25     PICTURE X(50)       VALUE
027310010323               "STATEMENT DATA (ACCOUNT GIC DETAILS  ) BYPASSED:  ".
027320991126           03  WS-STMT-DTA-BLIT-30     PICTURE X(50)       VALUE
027330991126               "STATEMENT DATA (ACC PERF) RECS BYPASSED:          ".
027340991126           03  WS-STMT-DTA-BLIT-35     PICTURE X(50)       VALUE
027350991126               "STATEMENT DATA (ACC FUND PERF) RECS BYPASSED:     ".
027360001114001114     03  WS-STMT-DTA-BLIT-40     PICTURE X(50)       VALUE
027370001114               "STATEMENT DATA (SEG FUND CONT) RECS BYPASSED:     ".
027380001114001114     03  WS-STMT-DTA-BLIT-43     PICTURE X(50)       VALUE
027390001114               "STATEMENT DATA (SEG FUND INVS) RECS BYPASSED:     ".
027400001114001114     03  WS-STMT-DTA-BLIT-45     PICTURE X(50)       VALUE
027410001114               "STATEMENT DATA (ANNUITANT    ) RECS BYPASSED:     ".
027420000000           03  WS-STMT-DTA-BLIT-50     PICTURE X(50)       VALUE
027430000000               "STATEMENT DATA (ROLLOVER) RECS BYPASSED:          ".
027440000000           03  WS-STMT-DTA-BLIT-60     PICTURE X(50)       VALUE
027450000000               "STATEMENT DATA (RRIF DETAIL) RECS BYPASSED:       ".
027460991013           03  WS-STMT-DTA-BLIT-62     PICTURE X(50)       VALUE
027470991013               "STATEMENT DATA (RESP DETAIL) RECS BYPASSED:       ".
027480001114001114     03  WS-STMT-DTA-BLIT-67     PICTURE X(50)       VALUE
027490001114               "STATEMENT DATA (BENEFICIARY) RECS BYPASSED:       ".
027500000000           03  WS-STMT-DTA-BLIT-70     PICTURE X(50)       VALUE
027510000000               "STATEMENT DATA (SYSTEM PLAN INFO) RECS BYPASSED:  ".
027520000000           03  WS-STMT-DTA-BLIT-82     PICTURE X(50)       VALUE
027530000000               "STATEMENT DATA (ACCT TRAN INST DTL) RECS BYPASSED:".
027540991013           03  WS-STMT-DTA-BLIT-84     PICTURE X(50)       VALUE
027550991013               "STATEMENT DATA (RESP TRAN BENE DTL) RECS BYPASSED:".
027560001114001114     03  WS-STMT-DTA-BLIT-90     PICTURE X(50)       VALUE
027570001114               "STATEMENT DATA (RESET CONTRAC) RECS BYPASSED:     ".
027580000000           03  WS-STMT-DTA-RLIT        PICTURE X(50)       VALUE
027590000000               "STATEMENT DATA RECS READ:                         ".
027600000000           03  WS-STMT-DTA-SLIT-01     PICTURE X(50)       VALUE
027610000000               "STATEMENT DATA (INVESTOR INFO) RECS SELECTED:     ".
027620000000           03  WS-STMT-DTA-SLIT-05     PICTURE X(50)       VALUE
027630000000               "STATEMENT DATA (DEALER) RECS SELECTED:            ".
027640000000           03  WS-STMT-DTA-SLIT-10     PICTURE X(50)       VALUE
027650000000               "STATEMENT DATA (INVESTOR SUMMARY) RECS SELECTED:  ".
027660000000           03  WS-STMT-DTA-SLIT-20     PICTURE X(50)       VALUE
027670000000               "STATEMENT DATA (ACCOUNT DETAIL) RECS SELECTED:    ".
027680060619R35042     03  WS-STMT-DTA-SLIT-21     PICTURE X(50)       VALUE
027690060623R35042         "STATEMENT DATA (ACCOUNT ADDITIONAL INFO) BYPASSED:".
027700130408117038     03  WS-STMT-DTA-SLIT-22     PICTURE X(50)       VALUE
027710130408117038         "STATEMENT DATA (ACCOUNT ADDITIONAL DTLS) BYPASSED:".
027720010323           03  WS-STMT-DTA-SLIT-25     PICTURE X(50)       VALUE
027730010323               "STATEMENT DATA (ACCOUNT GIC DETAIL) RECS SELECTED:".
027740001130           03  WS-STMT-DTA-SLIT-30     PICTURE X(50)       VALUE
027750001130               "STATEMENT DATA (ACCOUNT PERFOR) RECS SELECTED:    ".
027760040406R20870     03  WS-STMT-DTA-SLIT-33     PICTURE X(50)       VALUE
027770040406               "STATEMENT DATA (ACCOUNT ROR)    RECS SELECTED:    ".
027780001130           03  WS-STMT-DTA-SLIT-35     PICTURE X(50)       VALUE
027790001130               "STATEMENT DATA (ACCOUNT FUND P) RECS SELECTED:    ".
027800040406R20870     03  WS-STMT-DTA-SLIT-38     PICTURE X(50)       VALUE
027810040406               "STATEMENT DATA (ACCOUNT FUND ROR) RECS SELECTED:  ".
027820981207           03  WS-STMT-DTA-SLIT-60     PICTURE X(50)       VALUE
027830981207               "STATEMENT DATA (RRIF/LIF DETAILS) RECS SELECTED:  ".
027840000000           03  WS-STMT-DTA-SLIT-80     PICTURE X(50)       VALUE
027850000000               "STATEMENT DATA (ACCT. TRANSACTION) RECS SELECTED: ".
027860000000           03  WS-STMT-PRT-DTL-WLIT    PICTURE X(50)       VALUE
027870000000               "STATEMENT PRINT DETAIL RECS WRITTEN:              ".
027880000000           03  WS-STMT-PRT-HDR-WLIT    PICTURE X(50)       VALUE
027890000000               "STATEMENT PRINT HEADER RECS WRITTEN:              ".
027900000000
027910000000      *****
027920000000      *****    Error Messages.
027930000000      *****
027940000000
027950000000           03  WS-MSGS.
027960000000               05  WS-MSG-01           PICTURE X(50)       VALUE
027970000000                   "MFASTMTFP SECTION-NO INVALID, RECORD BYPASSED     ".
027980000000               05  WS-MSG-02           PICTURE X(50)       VALUE
027990000000                   "ACCOUNT DETAIL SECTION-NO INVALID, RECORD BYPASSED".
028000000000               05  WS-MSG-03           PICTURE X(50)       VALUE
028010000000                   "ACCOUNT DETAIL CONTAINS INVALID CURRENCY          ".
028020000000               05  WS-MSG-04           PICTURE X(50)       VALUE
028030000000                   "PROGRAM CURRENCY MAXIMUM EXCEEDED                 ".
028040000000               05  WS-MSG-05           PICTURE X(50)       VALUE
028050000000                   "WARNING: MISSING ACCT DTL REC FOR ACCT TRANS FUND ".
028060000000               05  WS-MSG-06           PICTURE X(50)       VALUE
028070000000                   "STATEMENT PRINT HEADER RECORD WRITE FAILED, ABORT ".
028080000000               05  WS-MSG-07           PICTURE X(50)       VALUE
028090000000                   "STATEMENT PRINT DETAIL RECORD WRITE FAILED, ABORT ".
028100000000               05  WS-MSG-08           PICTURE X(50)       VALUE
028110000000                   "MISSING INVESTOR INFORMATION RECORD FOR ACCOUNT   ".
028120000000               05  WS-MSG-09           PICTURE X(50)       VALUE
028130000000                   "MISSING DEALER RECORD FOR ACCOUNT                 ".
028140000000               05  WS-MSG-10           PICTURE X(50)       VALUE
028150000000                   "MISSING ACCOUNT DETAIL RECORD FOR ACCOUNT         ".
028160000000               05  WS-MSG-11           PICTURE X(50)       VALUE
028170000000                   "PROGRAM ACCOUNT MAXIMUM EXCEEDED                  ".
028180000000               05  WS-MSG-12           PICTURE X(50)       VALUE
028190000000                   "WARNING: NOMINEE ACCOUNT WITH NO NOMINEE DATA     ".
028200000000               05  WS-MSG-13           PICTURE X(50)       VALUE
028210000000                   "WARNING: NON-NOMINEE ACCOUNT WITH NOMINEE DATA    ".
028220000000           03  WS-MSGS-R               REDEFINES WS-MSGS.
028230000000               05  WS-MSG              OCCURS  13  TIMES
028240000000                                       INDEXED BY  MSG
028250000000                                       PICTURE X(50).
028260000000
028270000000      *****
028280000000      *****    Format Account Type Description Routine Work Items.
028290000000      *****
028300000000
028310000000           03  WS-ACTTYPGRP            PICTURE X(1).
028320000000
028330000000           03  WS-ACTTYPIN.
028340000000               05  WS-ACTTYPIN-PFX     PICTURE X(35).
028350000000               05  WS-ACTTYPIN-SFX     PICTURE X(2).
028360000000
028370000000           03  WS-ACTTYPLCK            PICTURE X(1).
028380000000           03  WS-ACTTYPLIT1           PICTURE X(13).
028390000000           03  WS-ACTTYPLIT2           PICTURE X(13).
028400000000           03  WS-ACTTYPLIT3           PICTURE X(13).
028410000000           03  WS-ACTTYPLNG            PICTURE X(1).
028420000000           03  WS-ACTTYPNOM            PICTURE X(1).
028430000000           03  WS-ACTTYPOUT            PICTURE X(80).
028440000000           03  WS-ACTTYPSHT            PICTURE X(5).
028450000000           03  WS-ACTTYPSPS            PICTURE X(1).
028460000000
028470000000      *****
028480000000      *****    Index/Subscript Related Items.
028490000000      *****
028500000000
028510161108164194*    03  WS-SAF-LST              PICTURE S9(3)       COMP-3.
028520161108164194     03  WS-SAF-LST              PICTURE S9(4)       COMP-3.
028530001130           03  WS-SAF2-LST             PICTURE S9(3)       COMP-3.
028540161108164194*    03  WS-SAF-MAX              PICTURE S9(3)       COMP-3.
028550161108164194     03  WS-SAF-MAX              PICTURE S9(4)       COMP-3.
028560001129           03  WS-SAFP-LST             PICTURE S9(3)       COMP-3.
028570001129           03  WS-SAFP-MAX             PICTURE S9(3)       COMP-3.
028580000000           03  WS-SC-MAX               PICTURE S9(3)       COMP-3.
028590000000           03  WS-SIA-LST              PICTURE S9(5)       COMP-3.
028600000000           03  WS-SIA-MAX              PICTURE S9(5)       COMP-3.
028610000000           03  WS-SPL                  PICTURE S9(3)       COMP-3.
028620000000           03  WS-SPL-DTLFST           PICTURE S9(3)       COMP-3.
028630001204           03  WS-SHPL-DTLFST          PICTURE S9(3)       COMP-3.
028640000000           03  WS-SPL-DTLLST           PICTURE S9(3)       COMP-3.
028650000000           03  WS-SPL-DTLMAX           PICTURE S9(3)       COMP-3.
028660000000           03  WS-SPL-DTLTST           PICTURE S9(3)       COMP-3.
028670000000           03  WS-SPL-HDGFST           PICTURE S9(3)       COMP-3.
028680000000           03  WS-SPL-MAX              PICTURE S9(3)       COMP-3.
028690000000           03  WS-SPL-SH02             PICTURE S9(3)       COMP-3.
028700990927           03  WS-SPL-SH03             PICTURE S9(3)       COMP-3.
028710000000           03  WS-SPML-MAX             PICTURE S9(3)       COMP-3.
028720010419RF9415     03  WS-DI-LST               PICTURE S9(3)       COMP-3.
028730010419RF9415     03  WS-DI-MAX               PICTURE S9(3)       COMP-3.
028740010427RF9415     03  WS-SFC-LST              PICTURE S9(3)       COMP-3.
028750010427RF9415     03  WS-SFC-MAX              PICTURE S9(3)       COMP-3.
028760010427RF9415     03  WS-SFCI-LST             PICTURE S9(3)       COMP-3.
028770010427RF9415     03  WS-SFCI-MAX             PICTURE S9(3)       COMP-3.
028780010427RF9415     03  WS-ANN-LST              PICTURE S9(3)       COMP-3.
028790010427RF9415     03  WS-ANN-MAX              PICTURE S9(3)       COMP-3.
028800021212R15028     03  WS-NEW-INVESTOR-FLAG    PICTURE X(1) VALUE " ".
028810010427
028820000000      *****
028830000000      *****    Non-Numeric Items.
028840000000      *****
028850000000
028860990104
028870000000           03  WS-ADRTMP               PICTURE X(40).
028880010501RF9415     03  WS-ANNUITANT-NAME       PICTURE X(51).
028890000000           03  WS-BRANCH-ADDR1         PICTURE X(40).
028900000000           03  WS-BRANCH-ADDR2         PICTURE X(40).
028910000000           03  WS-BRANCH-ADDR3         PICTURE X(40).
028920000000           03  WS-BRANCH-ADDR4         PICTURE X(40).
028930000000           03  WS-BRANCH-ADDR5         PICTURE X(40).
028940010419RF9415     03  WS-MAILING-ADDRESS-INDICATOR PIC X(1).
028950000000
028960000000           03  WS-DATE.
028970000104               05  WS-DATE-YYYY        PICTURE 9(4).
028980000000               05  WS-DATE-MM          PICTURE 9(2).
028990000000               05  WS-DATE-DD          PICTURE 9(2).
029000990120
029010990120           03  WS-SYS-DATE             PICTURE 9(8).
029020000000
029030000000           03  WS-DEALER-NAME          PICTURE X(35).
029040000000           03  WS-DEALER-REP-NAMES     PICTURE X(51).
029050000000
029060000000           03  WS-END-DATE.
029070991013             04 WS-SH-END-DATE         PICTURE X(8).
029080991013             04 WS-SH-END-DATE-R       REDEFINES WS-SH-END-DATE.
029090991013               05  WS-SH-END-YY        PICTURE X(2).
029100991013               05  WS-SH-END-DATE-2    PICTURE X(6).
029110000000
029120990923           03  WS-END-DATE-6.
029130990923               05  WS-SH-END-DATE-6    PICTURE X(8).
029140990923
029150000000           03  WS-FAX.
029160000000             04  WS-SH-FAX-01-03       PICTURE 9(3).
029170000000             04  WS-SH-FAX-04-10       PICTURE 9(7).
029180000000             04  WS-SH-FAX-04-10-R     REDEFINES WS-SH-FAX-04-10.
029190000000               05  WS-SH-FAX-04-06     PICTURE X(3).
029200000000               05  WS-SH-FAX-07-10     PICTURE X(4).
029210000000
029220000000           03  WS-FILSTAT              PICTURE X(2).
029230000000
029240000000           03  WS-FUND-NO.
029250000000               05  WS-MANAGEMENT-COMPANY-CODE
029260000000                                       PICTURE X(3).
029270000000               05  FILLER              PICTURE X(5).
029280000000
029290000000           03  WS-NOMINEE              PICTURE X(53).
029300000000           03  WS-NOMINEE-CODE         PICTURE X(5).
029310000000           03  WS-NOMINEE-NAME         PICTURE X(37).
029320000000           03  WS-NOMINEES-ACCT-NO     PICTURE X(15).
029330000000
029340000000           03  WS-PHONE.
029350000000             04  WS-SH-PHONE-01-03     PICTURE 9(3).
029360000000             04  WS-SH-PHONE-04-10     PICTURE 9(7).
029370000000             04  WS-SH-PHONE-04-10-R   REDEFINES WS-SH-PHONE-04-10.
029380000000               05  WS-SH-PHONE-04-06   PICTURE X(3).
029390000000               05  WS-SH-PHONE-07-10   PICTURE X(4).
029400000000
029410000000           03  WS-POSTAL.
029420000000               05  WS-POSTAL1          PICTURE X(3).
029430000000               05  WS-POSTAL2          PICTURE X(7).
029440000000
029450000000           03  WS-POSTAL-OUT.
029460000000               05  WS-POSTAL-OUT1      PICTURE X(3).
029470000000               05  FILLER              PICTURE X(1).
029480000000               05  WS-POSTAL-OUT2      PICTURE X(6).
029490000000
029500000000           03  WS-REP-NAME             PICTURE X(40).
029510990113           03  WS-REP-NAME-1           PICTURE X(40).
029520990113
029530000000           03  WS-SH02-SIN-NO.
029540000000               05  SH02-SIN-NO-1-3     PICTURE X(3).
029550000000               05  SH02-SIN-NO-LITDSH1 PICTURE X(1).
029560000000               05  SH02-SIN-NO-4-6     PICTURE X(3).
029570000000               05  SH02-SIN-NO-LITDSH2 PICTURE X(1).
029580000000               05  SH02-SIN-NO-7-9     PICTURE X(3).
029590000000
029600000000           03  WS-SH04-INVESTOR-ADDRESS1.
029610000000               05  SH04-POSTAL-CODE    PICTURE X(10).
029620000000               05  FILLER              PICTURE X(30).
029630000000
029640000000           03  WS-SH05-INVESTOR-ADDRESS2.
029650000000               05  SH05-POSTAL-CODE    PICTURE X(10).
029660000000               05  FILLER              PICTURE X(30).
029670000000
029680000000           03  WS-SH06-INVESTOR-ADDRESS3.
029690000000               05  SH06-POSTAL-CODE    PICTURE X(10).
029700000000               05  FILLER              PICTURE X(30).
029710000000
029720000000           03  WS-SH07-INVESTOR-ADDRESS4.
029730000000               05  SH07-POSTAL-CODE    PICTURE X(10).
029740000000               05  FILLER              PICTURE X(30).
029750000000
029760000000           03  WS-SH08-INVESTOR-ADDRESS5.
029770000000               05  SH08-POSTAL-CODE    PICTURE X(10).
029780000000               05  FILLER              PICTURE X(30).
029790000000
029800000000           03  WS-SH09-POSTAL-CODE     PICTURE X(10).
029810030328           03  WS-SH10-LIT1X           PICTURE X(47).
029820000000
029830000000           03  WS-SH10-PHONE.
029840000000               05  SH10-PHONE-LITLB    PICTURE X(1).
029850000000               05  SH10-PHONE-01-03    PICTURE X(3).
029860000000               05  SH10-PHONE-LITRB    PICTURE X(1).
029870000000               05  FILLER              PICTURE X(1).
029880000000               05  SH10-PHONE-04-06    PICTURE X(3).
029890000000               05  SH10-PHONE-LITDSH   PICTURE X(1).
029900000000               05  SH10-PHONE-07-10    PICTURE X(4).
029910000000
029920000000           03  WS-SH11-NOMINEE         PICTURE X(53).
029930000000
029940000000           03  WS-SH11-FAX.
029950000000               05  SH11-FAX-LITLB      PICTURE X(1).
029960000000               05  SH11-FAX-01-03      PICTURE X(3).
029970000000               05  SH11-FAX-LITRB      PICTURE X(1).
029980000000               05  FILLER              PICTURE X(1).
029990000000               05  SH11-FAX-04-06      PICTURE X(3).
030000000000               05  SH11-FAX-LITDSH     PICTURE X(1).
030010000000               05  SH11-FAX-07-10      PICTURE X(4).
030020000000
030030000000           03  WS-SIN-NO.
030040000000               05  WS-SIN-NO-1-3       PICTURE X(3).
030050000000               05  WS-SIN-NO-4-6       PICTURE X(3).
030060000000               05  WS-SIN-NO-7-9       PICTURE X(3).
030070000000
030080000000           03  WS-START-DATE.
030090980722               05  WS-SH-START-DATE    PICTURE X(8).
030100001130
030110000000           03  WS-TRADE-DATE.
030120000000               05  FILLER              PICTURE X(2).
030130000000               05  WS-SH-TRADE-DATE    PICTURE X(6).
030140000000
030150990923           03  WS-TRADE-DATE-6.
030160001129               05  WS-SH-TRADE-DATE-6  PICTURE X(8).
030170990923
030180001130001128     03 WS-AD-MARKET-VALUE-PRIOR-TOT  PICTURE 9(11)V9(2).
030190001130           03 WS-AD-MARKET-VALUE-TOT        PICTURE 9(11)V9(2).
030200001219001219     03 WS-AD-DEPOSIT1-TOT            PICTURE 9(11)V9(2).
030210001219001219     03 WS-AD-DEPOSIT2-TOT            PICTURE 9(11)V9(2).
030220001219           03 WS-AD-WITHDRAWALS1-TOT        PICTURE 9(11)V9(2).
030230001219           03 WS-AD-WITHDRAWALS2-TOT        PICTURE 9(11)V9(2).
030240001228           03 WS-AD-DEP-MINUS-WITHD1        PICTURE S9(11)V9(2).
030250001228           03 WS-AD-DEP-MINUS-WITHD2        PICTURE S9(11)V9(2).
030260010912R11175     03 WS-INCREASE-DECREASE-1        PICTURE S9(11)V9(2).
030270010912R11175     03 WS-INCREASE-DECREASE-2        PICTURE S9(11)V9(2).
030280001128
030290001130001130     03 WS-SHPL-HD-STDATE        PICTURE 9(8).
030300001130           03 WS-SHPL-HD-STDATE-R REDEFINES WS-SHPL-HD-STDATE.
030310001130              05 WS-SHPL-HD-STDATE-CC   PICTURE 9(2).
030320001130              05 WS-SHPL-HD-STDATE-YY   PICTURE 9(2).
030330001130              05 WS-SHPL-HD-STDATE-MM   PICTURE 9(2).
030340001130              05 WS-SHPL-HD-STDATE-DD   PICTURE 9(2).
030350001130
030360010430RF9415     03 WS-SHGL-HD-ISDATE        PICTURE 9(8).
030370010430           03 WS-SHGL-HD-ISDATE-R REDEFINES WS-SHGL-HD-ISDATE.
030380010430              05 WS-SHGL-HD-ISDATE-CC   PICTURE 9(2).
030390010430              05 WS-SHGL-HD-ISDATE-YY   PICTURE 9(2).
030400010430              05 WS-SHGL-HD-ISDATE-MM   PICTURE 9(2).
030410010430              05 WS-SHGL-HD-ISDATE-DD   PICTURE 9(2).
030420010430
030430010430RF9415     03 WS-SHGL-HD-ANDATE        PICTURE 9(8).
030440010430           03 WS-SHGL-HD-ANDATE-R REDEFINES WS-SHGL-HD-ANDATE.
030450010430              05 WS-SHGL-HD-ANDATE-CC   PICTURE 9(2).
030460010430              05 WS-SHGL-HD-ANDATE-YY   PICTURE 9(2).
030470010430              05 WS-SHGL-HD-ANDATE-MM   PICTURE 9(2).
030480010430              05 WS-SHGL-HD-ANDATE-DD   PICTURE 9(2).
030490010430
030500001130           03 WS-SHPL-HD-ENDATE        PICTURE 9(8).
030510001130           03 WS-SHPL-HD-ENDATE-R REDEFINES WS-SHPL-HD-ENDATE.
030520001130              05 WS-SHPL-HD-ENDATE-CC   PICTURE 9(2).
030530001130              05 WS-SHPL-HD-ENDATE-YY   PICTURE 9(2).
030540001130              05 WS-SHPL-HD-ENDATE-MM   PICTURE 9(2).
030550001130              05 WS-SHPL-HD-ENDATE-DD   PICTURE 9(2).
030560001130
030570001130      *** Month value in character.
030580001217
030590001217       01 WS-SHORT-MONTH-LITS.
030600001217
030610001217      ***  English Literals.
030620001217
030630001217           03  WS-SML-JAN-E.
030640061027               05 FILLER    PIC X(3)  VALUE "JAN".
030650001217           03  WS-SML-FEB-E.
030660061027               05 FILLER    PIC X(3)  VALUE "FEB".
030670001217           03  WS-SML-MAR-E.
030680061027               05 FILLER    PIC X(3)  VALUE "MAR".
030690001217           03  WS-SML-APR-E.
030700061027               05 FILLER    PIC X(3)  VALUE "APR".
030710001217           03  WS-SML-MAY-E.
030720061027               05 FILLER    PIC X(3)  VALUE "MAY".
030730001217           03  WS-SML-JUN-E.
030740061027               05 FILLER    PIC X(3)  VALUE "JUN".
030750001217           03  WS-SML-JUL-E.
030760061027               05 FILLER    PIC X(3)  VALUE "JUL".
030770001217           03  WS-SML-AUG-E.
030780061027               05 FILLER    PIC X(3)  VALUE "AUG".
030790001217           03  WS-SML-SEP-E.
030800061027               05 FILLER    PIC X(3)  VALUE "SEP".
030810001217           03  WS-SML-OCT-E.
030820061027               05 FILLER    PIC X(3)  VALUE "OCT".
030830001217           03  WS-SML-NOV-E.
030840061027               05 FILLER    PIC X(3)  VALUE "NOV".
030850001217           03  WS-SML-DEC-E.
030860061027               05 FILLER    PIC X(3)  VALUE "DEC".
030870001217
030880001217      ***  French Literals.
030890001217
030900001217           03  WS-SML-JAN-E.
030910061027               05 FILLER    PIC X(3)  VALUE "JAN".
030920001217           03  WS-SML-FEB-E.
030930061027               05 FILLER    PIC X(3)  VALUE "FÉV".
030940001217           03  WS-SML-MAR-E.
030950061027               05 FILLER    PIC X(3)  VALUE "MAR".
030960001217           03  WS-SML-APR-E.
030970061027               05 FILLER    PIC X(3)  VALUE "AVR".
030980001217           03  WS-SML-MAY-E.
030990061027               05 FILLER    PIC X(3)  VALUE "MAI".
031000001217           03  WS-SML-JUN-E.
031010061027               05 FILLER    PIC X(3)  VALUE "JUN".
031020001217           03  WS-SML-JUL-E.
031030061027               05 FILLER    PIC X(3)  VALUE "JUL".
031040001217           03  WS-SML-AUG-E.
031050061027               05 FILLER    PIC X(3)  VALUE "AOÛ".
031060001217           03  WS-SML-SEP-E.
031070061027               05 FILLER    PIC X(3)  VALUE "SEP".
031080001217           03  WS-SML-OCT-E.
031090061027               05 FILLER    PIC X(3)  VALUE "OCT".
031100001217           03  WS-SML-NOV-E.
031110061027               05 FILLER    PIC X(3)  VALUE "NOV".
031120001217           03  WS-SML-DEC-E.
031130061027               05 FILLER    PIC X(3)  VALUE "DEC".
031140001217
031150001217       01 WS-SHORT-MONTH-LITS-R          REDEFINES WS-SHORT-MONTH-LITS.
031160001217           03  WS-SHORT-MONTH-LITSLAN    OCCURS   2  TIMES
031170001217                                         INDEXED BY  SML.
031180001217               05 DI-JAN    PIC X(3).
031190001217               05 DI-FEB    PIC X(3).
031200001217               05 DI-MAR    PIC X(3).
031210001217               05 DI-APR    PIC X(3).
031220001217               05 DI-MAY    PIC X(3).
031230001217               05 DI-JUN    PIC X(3).
031240001217               05 DI-JUL    PIC X(3).
031250001217               05 DI-AUG    PIC X(3).
031260001217               05 DI-SEP    PIC X(3).
031270001217               05 DI-OCT    PIC X(3).
031280001217               05 DI-NOV    PIC X(3).
031290001217               05 DI-DEC    PIC X(3).
031300001217      /
031310001204      *** Full Month value in character.
031320001217
031330001217       01 WS-FULL-MONTH-LITS.
031340001217
031350001217      ***  English Literals.
031360001217
031370001217           03  WS-FML-JAN-E.
031380061027               05 FILLER     PIC X(9)  VALUE "  JANUARY".
031390001217           03  WS-FML-FEB-E.
031400061027               05 FILLER     PIC X(9)  VALUE " FEBRUARY".
031410001217           03  WS-FML-MAR-E.
031420061027               05 FILLER     PIC X(9)  VALUE "    MARCH".
031430001217           03  WS-FML-APR-E.
031440061027               05 FILLER     PIC X(9)  VALUE "    APRIL".
031450001217           03  WS-FML-MAY-E.
031460061027               05 FILLER     PIC X(9)  VALUE "      MAY".
031470001217           03  WS-FML-JUN-E.
031480061027               05 FILLER     PIC X(9)  VALUE "     JUNE".
031490001217           03  WS-FML-JUL-E.
031500061027               05 FILLER     PIC X(9)  VALUE "     JULY".
031510001217           03  WS-FML-AUG-E.
031520061027               05 FILLER     PIC X(9)  VALUE "   AUGUST".
031530001217           03  WS-FML-SEP-E.
031540061027               05 FILLER     PIC X(9)  VALUE "SEPTEMBER".
031550001217           03  WS-FML-OCT-E.
031560061027               05 FILLER     PIC X(9)  VALUE "  OCTOBER".
031570001217           03  WS-FML-NOV-E.
031580061027               05 FILLER     PIC X(9)  VALUE " NOVEMBER".
031590001217           03  WS-FML-DEC-E.
031600061027               05 FILLER     PIC X(9)  VALUE " DECEMBER".
031610001204
031620001217           03  WS-FML-JAN-F.
031630061027               05 FILLER     PIC X(9)  VALUE "  JANVIER".
031640001217           03  WS-FML-FEB-F.
031650061027               05 FILLER     PIC X(9)  VALUE "  FÉVRIER".
031660001217           03  WS-FML-MAR-F.
031670061027               05 FILLER     PIC X(9)  VALUE "     MARS".
031680001217           03  WS-FML-APR-F.
031690061027               05 FILLER     PIC X(9)  VALUE "    AVRIL".
031700001217           03  WS-FML-MAY-F.
031710061027               05 FILLER     PIC X(9)  VALUE "      MAI".
031720001217           03  WS-FML-JUN-F.
031730061027               05 FILLER     PIC X(9)  VALUE "     JUIN".
031740001217           03  WS-FML-JUL-F.
031750061027               05 FILLER     PIC X(9)  VALUE "  JUILLET".
031760001217           03  WS-FML-AUG-F.
031770061027               05 FILLER     PIC X(9)  VALUE "     AOÛT".
031780001217           03  WS-FML-SEP-F.
031790061027               05 FILLER     PIC X(9)  VALUE "SEPTEMBRE".
031800001217           03  WS-FML-OCT-F.
031810061027               05 FILLER     PIC X(9)  VALUE "  OCTOBRE".
031820001217           03  WS-FML-NOV-F.
031830061027               05 FILLER     PIC X(9)  VALUE " NOVEMBRE".
031840001217           03  WS-FML-DEC-F.
031850061027               05 FILLER     PIC X(9)  VALUE " DÉCEMBRE".
031860001217
031870001217       01  WS-FULL-MONTH-LITS-R          REDEFINES WS-FULL-MONTH-LITS.
031880001217           03  WS-FULL-MONTH-LITSLAN     OCCURS   2  TIMES
031890001217                                         INDEXED BY  FML.
031900001217               05 FDI-JAN                PIC X(9).
031910001217               05 FDI-FEB                PIC X(9).
031920001217               05 FDI-MAR                PIC X(9).
031930001217               05 FDI-APR                PIC X(9).
031940001217               05 FDI-MAY                PIC X(9).
031950001217               05 FDI-JUN                PIC X(9).
031960001217               05 FDI-JUL                PIC X(9).
031970001217               05 FDI-AUG                PIC X(9).
031980001217               05 FDI-SEP                PIC X(9).
031990001217               05 FDI-OCT                PIC X(9).
032000001217               05 FDI-NOV                PIC X(9).
032010001217               05 FDI-DEC                PIC X(9).
032020001217
032030010501      /
032040010501      *** Full Month value in character.
032050010501
032060010501       01 WS-FULL-MONTH-LITS-HDG.
032070010501
032080010501      ***  English Literals.
032090010501
032100010501           03  WS-FMLH-JAN-E.
032110061027               05 FILLER     PIC X(9)  VALUE "JANUARY".
032120010501           03  WS-FMLH-FEB-E.
032130061027               05 FILLER     PIC X(9)  VALUE "FEBRUARY".
032140010501           03  WS-FMLH-MAR-E.
032150061027               05 FILLER     PIC X(9)  VALUE "MARCH".
032160010501           03  WS-FMLH-APR-E.
032170061027               05 FILLER     PIC X(9)  VALUE "APRIL".
032180010501           03  WS-FMLH-MAY-E.
032190061027               05 FILLER     PIC X(9)  VALUE "MAY".
032200010501           03  WS-FMLH-JUN-E.
032210061027               05 FILLER     PIC X(9)  VALUE "JUNE".
032220010501           03  WS-FMLH-JUL-E.
032230061027               05 FILLER     PIC X(9)  VALUE "JULY".
032240010501           03  WS-FMLH-AUG-E.
032250061027               05 FILLER     PIC X(9)  VALUE "AUGUST".
032260010501           03  WS-FMLH-SEP-E.
032270061027               05 FILLER     PIC X(9)  VALUE "SEPTEMBER".
032280010501           03  WS-FMLH-OCT-E.
032290061027               05 FILLER     PIC X(9)  VALUE "OCTOBER".
032300010501           03  WS-FMLH-NOV-E.
032310061027               05 FILLER     PIC X(9)  VALUE "NOVEMBER".
032320010501           03  WS-FMLH-DEC-E.
032330061027               05 FILLER     PIC X(9)  VALUE "DECEMBER".
032340010501
032350010501           03  WS-FMLH-JAN-F.
032360061027               05 FILLER     PIC X(9)  VALUE "JANVIER".
032370010501           03  WS-FMLH-FEB-F.
032380061027               05 FILLER     PIC X(9)  VALUE "FÉVRIER".
032390010501           03  WS-FMLH-MAR-F.
032400061027               05 FILLER     PIC X(9)  VALUE "MARS".
032410010501           03  WS-FMLH-APR-F.
032420061027               05 FILLER     PIC X(9)  VALUE "AVRIL".
032430010501           03  WS-FMLH-MAY-F.
032440061027               05 FILLER     PIC X(9)  VALUE "MAI".
032450010501           03  WS-FMLH-JUN-F.
032460061027               05 FILLER     PIC X(9)  VALUE "JUIN".
032470010501           03  WS-FMLH-JUL-F.
032480061027               05 FILLER     PIC X(9)  VALUE "JUILLET".
032490010501           03  WS-FMLH-AUG-F.
032500061027               05 FILLER     PIC X(9)  VALUE "AOÛT".
032510010501           03  WS-FMLH-SEP-F.
032520061027               05 FILLER     PIC X(9)  VALUE "SEPTEMBRE".
032530010501           03  WS-FMLH-OCT-F.
032540061027               05 FILLER     PIC X(9)  VALUE "OCTOBRE".
032550010501           03  WS-FMLH-NOV-F.
032560061027               05 FILLER     PIC X(9)  VALUE "NOVEMBRE".
032570010501           03  WS-FMLH-DEC-F.
032580061027               05 FILLER     PIC X(9)  VALUE "DÉCEMBRE".
032590010501
032600010501       01  WS-FULL-MONTH-LITS-HDG-R   REDEFINES WS-FULL-MONTH-LITS-HDG.
032610010501           03  WS-FULL-MONTH-LITSHLAN   OCCURS   2  TIMES
032620010501                                         INDEXED BY  FMLH.
032630010501               05 FDIH-JAN                PIC X(9).
032640010501               05 FDIH-FEB                PIC X(9).
032650010501               05 FDIH-MAR                PIC X(9).
032660010501               05 FDIH-APR                PIC X(9).
032670010501               05 FDIH-MAY                PIC X(9).
032680010501               05 FDIH-JUN                PIC X(9).
032690010501               05 FDIH-JUL                PIC X(9).
032700010501               05 FDIH-AUG                PIC X(9).
032710010501               05 FDIH-SEP                PIC X(9).
032720010501               05 FDIH-OCT                PIC X(9).
032730010501               05 FDIH-NOV                PIC X(9).
032740010501               05 FDIH-DEC                PIC X(9).
032750010501
032760001218      *** English colomun headings.
032770001218
032780001130           03  WS-MONTH-COL1.
032790001201               05  WS-MONTH-LIT1          PIC X(4).
032800001201               05  WS-MONTH-LIT1-DD       PIC ZZ.
032810001218               05  WS-MONTH-LIT1S         PIC X(1).
032820001130               05  WS-MONTH-LIT1-YY       PIC 9(2).
032830010430
032840001130           03  WS-MONTH-COL2.
032850001201               05  WS-MONTH-LIT2          PIC X(4).
032860001201               05  WS-MONTH-LIT2-DD       PIC ZZ.
032870001218               05  WS-MONTH-LIT2S         PIC X(1).
032880001130               05  WS-MONTH-LIT2-YY       PIC 9(2).
032890010430
032900001204           03  WS-MONTH-COL3.
032910001204               05  WS-MONTH-LIT3          PIC X(9).
032920001218               05  FILLER                 PIC X(1).
032930001204               05  WS-MONTH-LIT3-DD       PIC ZZ.
032940001218               05  WS-MONTH-LIT3S         PIC X(1).
032950001218               05  FILLER                 PIC X(1).
032960001204               05  WS-MONTH-LIT3-CC       PIC 9(2).
032970001204               05  WS-MONTH-LIT3-YY       PIC 9(2).
032980001218
032990010430RF9415     03  WS-MONTH-COL4.
033000010430               05  WS-MONTH-LIT4          PIC X(9).
033010010430               05  FILLER                 PIC X(1).
033020010501               05  WS-MONTH-LIT4-DD       PIC X(2).
033030010430               05  WS-MONTH-LIT4S         PIC X(1).
033040010430               05  FILLER                 PIC X(1).
033050010501               05  WS-MONTH-LIT4-CC       PIC X(2).
033060010501               05  WS-MONTH-LIT4-YY       PIC X(2).
033070010430
033080010430RF9415     03  WS-MONTH-COL5.
033090010430               05  WS-MONTH-LIT5          PIC X(9).
033100010430               05  FILLER                 PIC X(1).
033110010501               05  WS-MONTH-LIT5-DD       PIC X(2).
033120010430               05  WS-MONTH-LIT5S         PIC X(1).
033130010430               05  FILLER                 PIC X(1).
033140010501               05  WS-MONTH-LIT5-CC       PIC X(2).
033150010501               05  WS-MONTH-LIT5-YY       PIC X(2).
033160010430
033170010911R11175     03  WS-MONTH-COL6.
033180010911               05  WS-MONTH-LIT6          PIC X(9).
033190010911               05  FILLER                 PIC X(1).
033200010912               05  WS-MONTH-LIT6-DD       PIC ZZ.
033210010911               05  WS-MONTH-LIT6S         PIC X(1).
033220010911               05  FILLER                 PIC X(1).
033230010911               05  WS-MONTH-LIT6-CC       PIC X(2).
033240010911               05  WS-MONTH-LIT6-YY       PIC X(2).
033250010911
033260001218      *** French colomun headings.
033270001218
033280001218           03  WS-MONTH-COL1-F.
033290001218               05  WS-MONTH-LIT1-DD-F     PIC ZZ.
033300001218               05  FILLER                 PIC X.
033310001218               05  WS-MONTH-LIT1-F        PIC X(3).
033320001218               05  WS-MONTH-LIT1S-F       PIC X(1).
033330001218               05  WS-MONTH-LIT1-YY-F     PIC 9(2).
033340010430
033350001218           03  WS-MONTH-COL2-F.
033360001218               05  WS-MONTH-LIT2-DD-F     PIC ZZ.
033370001218               05  FILLER                 PIC X.
033380001218               05  WS-MONTH-LIT2-F        PIC X(3).
033390001218               05  WS-MONTH-LIT2S-F       PIC X(1).
033400001218               05  WS-MONTH-LIT2-YY-F     PIC 9(2).
033410010430
033420001218           03  WS-MONTH-COL3-F.
033430001218               05  WS-MONTH-LIT3-DD-F     PIC ZZ.
033440001218               05  FILLER                 PIC X.
033450001218               05  WS-MONTH-LIT3-F        PIC X(9).
033460010912               05  WS-MONTH-LIT3S-F       PIC X(1).
033470001218               05  WS-MONTH-LIT3-CC-F     PIC 9(2).
033480001218               05  WS-MONTH-LIT3-YY-F     PIC 9(2).
033490010501
033500010501RF9415     03  WS-MONTH-COL4-F.
033510010501               05  WS-MONTH-LIT4-DD-F     PIC X(2).
033520010501               05  FILLER                 PIC X.
033530010501               05  WS-MONTH-LIT4-F        PIC X(9).
033540010912               05  WS-MONTH-LIT4S-F       PIC X(1).
033550010501               05  WS-MONTH-LIT4-CC-F     PIC X(2).
033560010501               05  WS-MONTH-LIT4-YY-F     PIC X(2).
033570010430
033580010430RF9415     03  WS-MONTH-COL5-F.
033590010501               05  WS-MONTH-LIT5-DD-F     PIC X(2).
033600010430               05  FILLER                 PIC X.
033610010430               05  WS-MONTH-LIT5-F        PIC X(9).
033620010912               05  WS-MONTH-LIT5S-F       PIC X(1).
033630010501               05  WS-MONTH-LIT5-CC-F     PIC X(2).
033640010501               05  WS-MONTH-LIT5-YY-F     PIC X(2).
033650001218
033660010911R11175     03  WS-MONTH-COL6-F.
033670010912               05  WS-MONTH-LIT6-DD-F     PIC ZZ.
033680010911               05  FILLER                 PIC X.
033690010911               05  WS-MONTH-LIT6-F        PIC X(9).
033700010912               05  WS-MONTH-LIT6S-F       PIC X(1).
033710010911               05  WS-MONTH-LIT6-CC-F     PIC X(2).
033720010911               05  WS-MONTH-LIT6-YY-F     PIC X(2).
033730010911
033740001218       01  WS-PROGRAM-USED-LITS.
033750061027           03  WS-PERCENTAGE-LIT          PIC X(1) VALUE "%".
033760061027           03  WS-COMMA-LIT               PIC X(1) VALUE ",".
033770061027           03  WS-NOT-AVAILABLE-LIT       PIC X(3) VALUE "N/A".
033780001205
033790000000      *****
033800000000      *****    Non-Numeric Literals.
033810000000      *****
033820000000
033830000000           03  WS-ACCOUNT-TYPE-SHORT-TOTAL
033840000000                                       PICTURE X(5)        VALUE
033850000000               "TOTAL".
033860000000           03  WS-ACCOUNT-TYPE-TOTAL   PICTURE X(35)       VALUE
033870000000               "TOTAL                              ".
033880000000           03  WS-INVESTMENT-NAME-TOTAL
033890000000                                       PICTURE X(35)       VALUE
033900000000               "TOTAL                              ".
033910000000
033920000000      *****
033930000000      *****    Numeric Items.
033940000000      *****
033950971223
033960971223           03  WS-CPG-FIXED            PICTURE S9(9)V9(2)  COMP-3.
033970971223           03  WS-CPG-FIXED-SIGN       PICTURE X(1).
033980000000
033990971223           03  WS-CPTL-GAIN            PICTURE 9(9)V9(2)   COMP-3.
034000000000           03  WS-CPTL-GAIN-SIGN       PICTURE X(1).
034010000000
034020000000           03  WS-DEDUCTIONS           PICTURE S9(9)V9(2)  COMP-3.
034030000000           03  WS-DEDUCTIONS-SIGN      PICTURE X(1).
034040000000
034050000000           03  WS-LNECNT               PICTURE S9(3)       COMP-3.
034060000000           03  WS-LNEMAX               PICTURE S9(3)       COMP-3.
034070000000           03  WS-NET-INVESTED         PICTURE S9(9)V9(2)  COMP-3.
034080000000           03  WS-PGECNT               PICTURE S9(5)       COMP-3.
034090000000
034100000000           03  WS-REG-CTB-CUR          PICTURE 9(9)V9(2)   COMP-3.
034110000000           03  WS-REG-CTB-CUR-SIGN     PICTURE X(1).
034120000000
034130000000           03  WS-SH-PGENO             PICTURE S9(3)       COMP-3.
034140000000           03  WS-STMTVRSN             PICTURE S9(3)       COMP-3.
034150000000
034160000000      *****
034170000000      *****    Switches.
034180000000      *****
034190000000
034200000000           03  WS-BYPPRT-SW            PICTURE X(1).
034210000000           03  WS-CLTSPR-SW            PICTURE X(1).
034220000000           03  WS-EOJHDG-SW            PICTURE X(1).
034230000000           03  WS-L01BYP-SW            PICTURE X(1).
034240000000           03  WS-PSRERR-SW            PICTURE X(1).
034250000000           03  WS-PSTCDEPRT-SW         PICTURE X(1).
034260000000           03  WS-SELPRT-SW            PICTURE X(1).
034270000000           03  WS-STMT-DTA-SW-01       PICTURE X(1).
034280000000           03  WS-STMT-DTA-SW-05       PICTURE X(1).
034290001129           03  WS-STMT-DTA-SW-30       PICTURE X(1).
034300001129           03  WS-STMT-DTA-SW-35       PICTURE X(1).
034310061027RF9415     03  WS-STMT-DTA-SW-40       PICTURE X(1) VALUE "N".
034320061027RF9415     03  WS-STMT-DTA-SW-43       PICTURE X(1) VALUE "N".
034330061027RF9415     03  WS-STMT-DTA-SW-45       PICTURE X(1) VALUE "N".
034340010427RF9415     03  WS-ACCOUNT-INV-SW       PICTURE X(1).
034350010430RF9415     03  WS-LR1-PROCESSED-SW     PICTURE X(1).
034360001220001220     03  WS-STMT-DTA-SW-80       PICTURE X(1).
034370001201           03  WS-PLAN-SUM-SW          PICTURE X(1).
034380001205           03  WS-TRANS-HEADING-SW     PICTURE X(1).
034390981207           03  WS-STMT-DTA-SW-60       PICTURE X(1).
034400000000           03  WS-STMT-PRT-HDR-WSW     PICTURE X(1).
034410061027001215     03  WS-STMT-SUMM-HDG-SW     PICTURE X(1) VALUE "N".
034420000000      /
034430000000       01  DUMP-ARGUMENTS.
034440000000
034450000000      ******************************************************************
034460000000      *                                                                *
034470000000      *    D U M P   P A S S   P A R A M E T E R S                     *
034480000000      *                                                                *
034490000000      ******************************************************************
034500000000
034510000000           03  DA-TYPE                 PICTURE X(1)        VALUE "F".
034520000000               88  COBOL-IDENTIFIERS                       VALUE "D".
034530000000               88  FULL-DUMP                               VALUE "F".
034540000000           03  FILLER                  PICTURE X(1).
034550000000           03  DA-PGM-NAME             PICTURE X(10)       VALUE
034560010427               "STMTPRT2  ".
034570000000      /
034580000000      ******************************************************************
034590000000      *                                                                *
034600000000      *    C U R R E N T   /   P R E V I O U S   C O N T R O L S       *
034610000000      *                                                                *
034620000000      ******************************************************************
034630000000
034640000000       01  CURR-CONTROLS.
034650000000           03  CURR-LEVEL1.
034660000000             04  CURR-LEVEL2.
034670980128               05  CURR-START-DATE     PICTURE 9(8).
034680980128               05  CURR-END-DATE       PICTURE 9(8).
034690000000               05  CURR-INVESTOR-NO    PICTURE 9(9).
034700000000             04  CURR-ACCOUNT          PICTURE 9(9).
034710000000
034720000000       01  PREV-CONTROLS.
034730000000           03  PREV-LEVEL1.
034740000000             04  PREV-LEVEL2.
034750980128               05  PREV-START-DATE     PICTURE 9(8).
034760980128               05  PREV-END-DATE       PICTURE 9(8).
034770000000               05  PREV-INVESTOR-NO    PICTURE 9(9).
034780000000             04  PREV-ACCOUNT          PICTURE 9(9).
034790000000
034800000000       01  CURR-CTLS-DPR.
034810000000           03  CURR-CD-LEV1.
034820000000               05  CURR-CD-FUND-NO     PICTURE X(8).
034830000000
034840000000       01  PREV-CTLS-DPR.
034850000000           03  PREV-CD-LEV1.
034860000000               05  PREV-CD-FUND-NO     PICTURE X(8).
034870020921
03488002092114588  01    WS-MSTR-ACCOUNT-NO      PICTURE 9(9).
034890190612      *RFS183449-Starts
034900190612       01    Lc-Acct-Type-Code       PICTURE X(5).
034910190612      *RFS183449-Ends
034920000000      /
034930051110      *
034940051110      * RFS30210 - Begin
034950051110      * Variables to be returned from new paragraph - populated with
034960051110      * current ROR information.
034970051110      *
034980051110       01 WS-CURR-PERIOD-ROR-VARS.
034990051111         03 WS-CURR-PERIOD-ROR             PIC 9(09)V9(05) VALUE 0.
035000051110         03 WS-CURR-PERIOD-ROR-SIGN        PIC X(01) VALUE SPACES.
035010051110
035020061027       01 SW-CURR-PERIOD-EXISTS-FLAG       PIC 1 VALUE B"0".
035030061027         88 SW-CURR-PERIOD-NOT-EXISTS      VALUE B"0".
035040061027         88 SW-CURR-PERIOD-EXISTS          VALUE B"1".
035050051110
035060051110       01 SW-PERIOD-TO-CHECK-FLAG          PIC X(05) VALUE SPACES.
035070051110         88 SW-PERIOD-TO-CHECK-INC         VALUE "INC".
035080051110         88 SW-PERIOD-TO-CHECK-6M          VALUE "6M".
035090051110         88 SW-PERIOD-TO-CHECK-1Y          VALUE "1Y".
035100051110
035110051110       01 SW-PERIOD-ROR-APPLICABLE-FLAG    PIC X(01) VALUE SPACES.
035120051110         88 SW-PERIOD-ROR-APPLICABLE-INIT  VALUE SPACE.
035130051110         88 SW-PERIOD-ROR-APPLICABLE-NO    VALUE "N".
035140051111
035150051110      * RFS30210 - End
035160000000       PROCEDURE DIVISION.
035170000000
035180000000      ******************************************************************
035190000000      *                                                                *
035200000000      *    P R O C E D U R E   D I V I S I O N                         *
035210000000      *                                                                *
035220000000      ******************************************************************
035230000000      /
035240000000       MAINLINE.
035250000000
035260000000      ******************************************************************
035270000000      *                                                                *
035280000000      *    M A I N L I N E                                             *
035290000000      *                                                                *
035300000000      ******************************************************************
035310000000
035320000000           PERFORM INITIAL-LOGIC-RTN  THRU ILR-EXIT.
035330000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
035340000000               GO TO MLR-0050.
035350000000
035360000000       MLR-0010.
035370000000           PERFORM GET-CURRENT-RECORD THRU GCR-EXIT.
035380000000           IF  PREV-LEVEL1    IS EQUAL TO  CURR-LEVEL1
035390000000               GO TO MLR-0040.
035400000000
035410000000           IF  PREV-CONTROLS  IS EQUAL TO  LOW-VALUES
035420000000               GO TO MLR-0020.
035430000000
035440001205           PERFORM LEVEL-SUMMARY-1    THRU LS1-EXIT.
035450000000
035460000000           IF  PREV-LEVEL2    IS EQUAL TO  CURR-LEVEL2
035470010430               GO TO MLR-0030.
035480000000
035490000000           PERFORM LEVEL-SUMMARY-2    THRU LS2-EXIT.
035500000000
035510000000       MLR-0020.
035520000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
035530000000               GO TO MLR-0050.
035540000000
035550000000           PERFORM LEVEL-REINIT-2     THRU LR2-EXIT.
035560000000
035570000000       MLR-0030.
035580000000           PERFORM LEVEL-REINIT-1     THRU LR1-EXIT.
035590000000
035600000000       MLR-0040.
035610000000           PERFORM DETAIL-PROCESS-RTN THRU DPR-EXIT.
035620000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
035630000000               GO TO MLR-0050.
035640000000
035650000000           MOVE CURR-CONTROLS          TO  PREV-CONTROLS.
035660000000           GO TO MLR-0010.
035670000000
035680000000       MLR-0050.
035690000000           PERFORM END-JOB-RTN        THRU EJR-EXIT.
035700000000           STOP RUN.
035710000000      /
035720000000       GET-CURRENT-RECORD.
035730000000
035740000000      ******************************************************************
035750000000      *                                                                *
035760000000      *    G E T   C U R R E N T   R E C O R D   R O U T I N E         *
035770000000      *                                                                *
035780000000      ******************************************************************
035790000000
035800000000           CONTINUE.
035810000000
035820000000      *****
035830000000      *****    Read through the Statement Data records.
035840000000      *****
035850000000
035860000000       GCR-0010.
035870990921
035880000000           READ STMT-DTA-FILE          AT  END
035890000000               MOVE HIGH-VALUES        TO  CURR-CONTROLS
035900990921               GO TO GCR-EXIT
035910990921           END-READ
035920990921
035930000000           MOVE STMT-DTA-RECD          TO  MFASTMTFP-SECTION.
035940000000           ADD 1                       TO  WS-STMT-DTA-RCNT.
035950000000
035960000000      *****
035970000000      *****    Determine which Statement Data record is being processed.
035980000000      *****    If the record cannot be identified, an error message will
035990000000      *****    be printed and the record bypassed.  If this occurs, the
036000000000      *****    program should be changed to either bypass the record or
036010000000      *****    to handle it appropriately.
036020000000      *****
036030000000
036040990921           EVALUATE PR-SECTION-NO
036050990921             WHEN  "00"               GO TO GCR-0020
036060990921             WHEN  "01"               GO TO GCR-0030
036070990921             WHEN  "05"               GO TO GCR-0040
036080990921             WHEN  "10"               GO TO GCR-0050
036090010313RF8633       WHEN  "12"               GO TO GCR-0050-0005
036100990921             WHEN  "15"               GO TO GCR-0050-0010
036110140826132121       WHEN  "16"               GO TO GCR-0050-0016
036120991126             WHEN  "20"               GO TO GCR-0060
036130060618R35042       WHEN  "21"               GO TO GCR-0060-0002
036140130408117038       WHEN  "22"               GO TO GCR-0060-0003
036150010323RF8633       WHEN  "25"               GO TO GCR-0060-0005
036160991126             WHEN  "30"               GO TO GCR-0060-0010
036170040406R20870       WHEN  "33"               GO TO GCR-0060-0033
036180991126             WHEN  "35"               GO TO GCR-0060-0020
036190040406R20870       WHEN  "38"               GO TO GCR-0060-0038
036200001114001114       WHEN  "40"               GO TO GCR-0060-0030
036210001114             WHEN  "43"               GO TO GCR-0060-0040
036220001114             WHEN  "45"               GO TO GCR-0060-0050
036230990921             WHEN  "50"               GO TO GCR-0070
036240990921             WHEN  "60"               GO TO GCR-0070-0010
036250991013             WHEN  "62"               GO TO GCR-0070-0015
036260001114001114       WHEN  "67"               GO TO GCR-0070-0025
036270990921             WHEN  "70"               GO TO GCR-0070-0020
036280990921             WHEN  "80"               GO TO GCR-0080
036290990921             WHEN  "82"               GO TO GCR-0080-0010
036300991013             WHEN  "84"               GO TO GCR-0080-0020
036310001114001114       WHEN  "90"               GO TO GCR-0080-0030
036320990921             WHEN  OTHER
036330990921                   MOVE SPACES                 TO  ERROR-LINE-1
036340990921                   SET MSG                     TO  1
036350990921                   PERFORM PRINT-ERR-LINE      THRU PEL-EXIT
036360000000
036370990921                   ADD 1                       TO  WS-STMT-DTA-BCNT
036380990921                   GO TO GCR-0090
036390990921           END-EVALUATE.
036400990921
036410000000       GCR-0020.
036420000000
036430000000      *****
036440000000      *****    Processing a Delivery Information record, bypass it.
036450000000      *****
036460000000
036470000000           ADD 1                       TO  WS-STMT-DTA-BCNT-00.
036480000000           GO TO GCR-0090.
036490000000
036500000000       GCR-0030.
036510000000
036520000000      *****
036530000000      *****    Processing an Investor Information record, select it.
036540000000      *****
036550000000
036560000000           MOVE MFASTMTFP-SECTION      TO  INVESTOR-INFO-SECTION.
036570010604R10243     MOVE MFASTMTFP-SECTION      TO  WS-INVESTOR-INFO-TEMP.
036580000000           ADD 1                       TO  WS-STMT-DTA-SCNT-01.
036590000000           GO TO GCR-0100.
036600000000
036610000000       GCR-0040.
036620000000
036630000000      *****
036640000000      *****    Processing a Dealer record.  The record will be selected
036650000000      *****    unless it is the primary record.
036660000000      *****
036670000000
036680000000           MOVE MFASTMTFP-SECTION      TO  DEALER-SECTION.
036690990921
036700010423      *    IF  DL-SECTION-SEQ IS GREATER THAN  1
036710010423      *        ADD 1                   TO  WS-STMT-DTA-BCNT-05
036720010423      *        GO TO GCR-0090
036730010423      *    END-IF
036740990921
036750000000           ADD 1                       TO  WS-STMT-DTA-SCNT-05.
036760000000           GO TO GCR-0100.
036770000000
036780000000       GCR-0050.
036790000000
036800000000      *****
036810000000      *****    Processing an Investor Summary record.  The record will
036820000000      *****    be selected unless it is a 'TOTAL' record.
036830000000      *****
036840000000
036850000000           MOVE MFASTMTFP-SECTION      TO  INVESTOR-SUMMARY-SECTION.
036860000000           IF  IS-ACCOUNT-TYPE   EQUAL TO  WS-ACCOUNT-TYPE-TOTAL
036870000000           AND IS-ACCOUNT-TYPE-SHORT
036880000000                              IS EQUAL TO  WS-ACCOUNT-TYPE-SHORT-TOTAL
036890000000               ADD 1                   TO  WS-STMT-DTA-BCNT-10
036900000000               GO TO GCR-0090.
036910020614
036920000000           ADD 1                       TO  WS-STMT-DTA-SCNT-10.
036930000000           GO TO GCR-0100.
036940010313
036950010313RF8633 GCR-0050-0005.
036960010313
036970010313      *****
036980010313      *****    Processing an Investor Summary Section 2 record, bypass
036990010313      *****    it.
037000010313      *****
037010010313
037020010313           MOVE MFASTMTFP-SECTION      TO  INVESTOR-SUMMARY-SECTION-2.
037030010313           ADD 1                       TO  WS-STMT-DTA-BCNT-12.
037040010313rf8633     GO TO GCR-0090.
037050000000
037060000000       GCR-0050-0010.
037070000000
037080000000      *****
037090000000      *****    Processing an Account Asset Class Summary record, bypass
037100000000      *****    it.
037110000000      *****
037120000000
037130000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ASSET-CLASS-SECTION.
037140000000           ADD 1                       TO  WS-STMT-DTA-BCNT-15.
037150000000           GO TO GCR-0090.
037160140826      *RFS132121 - START
037170140826       GCR-0050-0016.
037180140826      *****
037190140826      *****    PROCESSING AN ACCOUNT LEVEL ASSET ALLOCATION RECORD,
037200140826      *****    BYPASS IT.
037210140826      *****
037220140826           MOVE MFASTMTFP-SECTION      TO ACCOUNT-LEVEL-ASSET-ALLOC-DTL.
037230140826           ADD 1                       TO WS-STMT-DTA-BCNT-16.
037240140826           GO TO GCR-0090.
037250140826      *RFS132121 - END
037260000000
037270000000       GCR-0060.
037280000000
037290000000      *****
037300000000      *****    Processing an Account Detail record.  The record will be
037310000000      *****    selected unless it is a 'TOTAL' record.
037320000000      *****
037330000000
037340000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION.
037350000000           IF  AD-INVESTMENT-NAME
037360000000                              IS EQUAL TO  WS-INVESTMENT-NAME-TOTAL
037370000000               ADD 1                   TO  WS-STMT-DTA-BCNT-20
037380000000               GO TO GCR-0090.
037390000000
037400000000           ADD 1                       TO  WS-STMT-DTA-SCNT-20.
037410000000           GO TO GCR-0100.
037420060618
037430060618R35042 GCR-0060-0002.
037440060618  ¦
037450060618  ¦   *****
037460060618  ¦   *****    Processing a ACCOUNT-ADDITIONAL-INFO record, BYPASS IT.
037470060618  ¦   *****
037480060618  ¦
037490060618  ¦        MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ADDITIONAL-INFO.
037500060618  V        ADD 1                       TO  WS-STMT-DTA-SCNT-21.
037510060618R35042     GO TO GCR-0090.
037520010323
037530130408117038 GCR-0060-0003.
037540130408  ¦
037550130408  ¦        MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION-22.
037560130408  V        ADD 1                       TO  WS-STMT-DTA-SCNT-22.
037570130408117038     GO TO GCR-0090.
037580130408
037590010323RF8633 GCR-0060-0005.
037600010323
037610010323      *****
037620010323      *****    Processing a ACCOUNT GIC DETAILS record, select it.
037630010323      *****
037640010323
037650010323           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-PERFORMANCE-SECTION.
037660010323           ADD 1                       TO  WS-STMT-DTA-SCNT-25.
037670010427           GO TO GCR-0090.
037680000000
037690991126       GCR-0060-0010.
037700991126
037710991126      *****
037720001129      *****    Processing a Account Performance record, select it.
037730991126      *****
037740991126
037750991126           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-PERFORMANCE-SECTION.
037760001129           ADD 1                       TO  WS-STMT-DTA-SCNT-30.
037770001201           GO TO GCR-0100.
037780991126
037790040406R20870 GCR-0060-0033.
037800040406
037810040406      *****
037820040406      *****    Processing a Account ROR, select it.
037830040406      *****
037840040406
037850040406           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ROR-FRQ-SECTION.
037860040406           ADD 1                       TO  WS-STMT-DTA-SCNT-33.
037870040406           GO TO GCR-0100.
037880040406
037890991126       GCR-0060-0020.
037900991126
037910991126      *****
037920001129      *****    Processing a Account Fund Performance record, select it.
037930991126      *****
037940991126
037950991126           MOVE MFASTMTFP-SECTION      TO ACCOUNT-FUND-PERFORM-SECTION.
037960001129           ADD 1                       TO WS-STMT-DTA-SCNT-35.
037970001201001127     GO TO GCR-0100.
037980001114
037990040406R20870 GCR-0060-0038.
038000040406
038010040406      *****
038020040406      *****    Processing a Account Fund ROR, select it.
038030040406      *****
038040040406
038050040406           MOVE MFASTMTFP-SECTION      TO ACCOUNT-FUND-ROR-FRQ-SECTION.
038060040406           ADD 1                       TO WS-STMT-DTA-SCNT-38.
038070040406           GO TO GCR-0100.
038080040406
038090010427RF9415 GCR-0060-0030.
038100010427
038110010427      *****
038120010427      *****  Processing a SEG Fund Contract details record, select it.
038130010427      *****
038140010427
038150010427           MOVE MFASTMTFP-SECTION     TO SEG-FUND-CONTRACT-DETAILS.
038160010427           ADD 1                      TO WS-STMT-DTA-BCNT-40.
038170010427           GO TO GCR-0100.
038180010427
038190010427       GCR-0060-0040.
038200010427
038210010427      *****
038220010427      *****  Processing a SEG Fund Contract Investment details record,
038230010427      *****  select  it.
038240010427      *****
038250010427
038260010427           MOVE MFASTMTFP-SECTION     TO SEG-FUND-CONTRACT-INVESTMENT.
038270010427           ADD 1                      TO WS-STMT-DTA-BCNT-43.
038280010427           GO TO GCR-0100.
038290010427
038300010427       GCR-0060-0050.
038310010427
038320010427      *****
038330010427      *****  Processing an Annuitant details record, select  it.
038340010427      *****
038350010427
038360010427           MOVE MFASTMTFP-SECTION     TO ANNUITANT-DETAILS.
038370010427           ADD 1                      TO WS-STMT-DTA-BCNT-45.
038380010427           GO TO GCR-0100.
038390010427
038400010427       GCR-0070.
038410000000
038420000000      *****
038430000000      *****    Processing a Rollover record, bypass it.
038440000000      *****
038450000000
038460000000           MOVE MFASTMTFP-SECTION      TO  ROLLOVER-SECTION.
038470000000           ADD 1                       TO  WS-STMT-DTA-BCNT-50.
038480000000           GO TO GCR-0090.
038490000000
038500000000       GCR-0070-0010.
038510000000
038520000000      *****
038530981207      *****    Processing a RRIF Detail record, bypass it if
038540981207      *****    RIF and LIF info is not required to print.
038550981207      *****
038560981215
038570000000           MOVE MFASTMTFP-SECTION      TO  RRIF-DETAIL-SECTION.
038580981207
038590981207           IF WS-LDA-RIF-LIF-MSG IS NOT EQUAL TO "Y"
038600981207              ADD 1                    TO  WS-STMT-DTA-BCNT-60
038610981207              GO TO GCR-0090.
038620981207
038630981207           ADD 1    TO WS-STMT-DTA-SCNT-60.
038640981207           GO TO GCR-0100.
038650981207
038660991013       GCR-0070-0015.
038670991013
038680991013      *****
038690991013      *****    Processing a RESP Detail record, bypass it.
038700991013      *****
038710991013
038720991013           MOVE MFASTMTFP-SECTION      TO  RESP-DETAIL-SECTION.
038730991013           ADD 1                       TO  WS-STMT-DTA-BCNT-62.
038740991013           GO TO GCR-0090.
038750991013
038760001114       GCR-0070-0025.
038770001114
038780001114      *****
038790001114      *****    Processing a Beneficiary record, bypass it.
038800001114      *****
038810001114
038820001114           MOVE MFASTMTFP-SECTION      TO  BENEFICIARY-INFORMATION.
038830001114           ADD 1                       TO  WS-STMT-DTA-BCNT-67.
038840001114           GO TO GCR-0090.
038850001114
038860000000       GCR-0070-0020.
038870000000
038880000000      *****
038890000000      *****    Processing a Systematic Plan Info record, bypass it.
038900000000      *****
038910000000
038920000000           MOVE MFASTMTFP-SECTION      TO  SYSTEMATIC-PLAN-INFO-SECTION.
038930000000           ADD 1                       TO  WS-STMT-DTA-BCNT-70.
038940000000           GO TO GCR-0090.
038950000000
038960000000       GCR-0080.
038970000000
038980000000      *****
038990000000      *****    Processing an Account Transaction record, select it.
039000000000      *****
039010000000
039020000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-TXN-SECTION.
039030000000           ADD 1                       TO  WS-STMT-DTA-SCNT-80.
039040000000           GO TO GCR-0100.
039050000000
039060000000       GCR-0080-0010.
039070000000
039080000000      *****
039090991013      *****    Processing an RESP Acount transaction record, bypass it.
039100000000      *****
039110000000
039120991013           MOVE MFASTMTFP-SECTION      TO  AT-RESP-DETAIL-SECTION.
039130991013           ADD 1                       TO  WS-STMT-DTA-BCNT-82.
039140000000           GO TO GCR-0090.
039150000000
039160991013       GCR-0080-0020.
039170991013
039180991013      *****
039190991013      *****    Processing an Account Transaction Institution Detail
039200991013      *****    record, bypass it.
039210991013      *****
039220991013
039230991013           MOVE MFASTMTFP-SECTION      TO  AT-INSTITUTION-DETAIL-SECTION.
039240991013           ADD 1                       TO  WS-STMT-DTA-BCNT-84.
039250991013           GO TO GCR-0090.
039260991013
039270001114       GCR-0080-0030.
039280001114
039290001114      *****
039300001114      *****    PROCESSING A RESET CONTRACT DETAILS RECORD, BYPASS IT.
039310001114      *****
039320001114
039330001114           MOVE MFASTMTFP-SECTION      TO  RESET-CONTRACT-DETAILS.
039340001114           ADD 1                       TO  WS-STMT-DTA-BCNT-90.
039350001114           GO TO GCR-0090.
039360001114
039370000000       GCR-0090.
039380000000
039390000000      *****
039400000000      *****    The record is to be bypassed.  Therefore, return to read
039410000000      *****    the next record.
039420000000      *****
039430000000
039440000000           GO TO GCR-0010.
039450000000
039460000000       GCR-0100.
039470000000
039480000000      *****
039490000000      *****    The record has been selected and is to be processed.
039500000000      *****    Format the current controls.
039510000000      *****
039520020921
039530020921           MOVE  ZEROES TO WS-MSTR-ACCOUNT-NO.
039540020921R14588     IF WS-LDA-STMT-CNFM-FLAG    IS NOT EQUAL TO "S"
039550020921  ¦           GO TO LR2-0095M
039560020921  ¦        END-IF.
039570020921  ¦
039580020921  ¦        INITIALIZE              DEALER-MISC-DETAILS-REC.
039590020921  ¦        MOVE AD-DEALER          TO DEALER-CODE
039600020921  ¦                                OF DEALER-MISC-DETAILS-REC.
039610020921  ¦
039620020921  ¦        READ DEALER-MISC-DETAILS
039630020921  ¦           INVALID KEY
039640020921  ¦               GO TO LR2-0095M
039650020921  ¦        END-READ.
039660020921  ¦
039670020921  ¦        IF MASTER-ACCT-PROCESS OF DEALER-MISC-DETAILS-REC
039680020921  ¦                                 IS NOT EQUAL TO "Y"
039690020921  ¦               GO TO LR2-0095M
039700020921  ¦        END-IF.
039710020921  ¦
039720020921  ¦        INITIALIZE              MASTER-ACCOUNT-REC.
039730020921  ¦        MOVE AD-ACCOUNT      TO ACCOUNT-NO OF MASTER-ACCOUNT-REC.
039740020921  ¦
039750020921  ¦        READ MASTER-ACCOUNT
039760020921  ¦           INVALID KEY
039770020921  ¦               GO TO LR2-0095M
039780020921  ¦        END-READ.
039790020921  ¦
039800020921  ¦        MOVE MASTER-ACCOUNT-NO OF MASTER-ACCOUNT-REC
039810020921  ¦                                    TO WS-MSTR-ACCOUNT-NO.
039820020921  ¦
039830020921R14588
039840020921
039850020923       LR2-0095M.
039860980210
039870980210           IF PR-START-DATE = SPACES
039880990921              MOVE ZEROES   TO PR-START-DATE
039890990921           END-IF
039900990921
039910980210           IF PR-END-DATE   = SPACES
039920990921              MOVE ZEROES   TO PR-END-DATE
039930990921           END-IF
039940990921
039950980128           MOVE PR-START-DATE          TO  CURR-START-DATE.
039960980128           MOVE PR-END-DATE            TO  CURR-END-DATE.
039970000000           MOVE PR-INVESTOR-NO         TO  CURR-INVESTOR-NO.
039980000000           MOVE PR-ACCOUNT             TO  CURR-ACCOUNT.
039990000000
040000990921      **********
040010990921
040020000000       GCR-EXIT.
040030000000           EXIT.
040040990921
040050990921      **********
040060990921
040070000000      /
040080000000       DETAIL-PROCESS-RTN.
040090000000
040100000000      ******************************************************************
040110000000      *                                                                *
040120000000      *    D E T A I L   P R O C E S S   R O U T I N E                 *
040130000000      *                                                                *
040140000000      ******************************************************************
040150000000
040160000000      *****
040170000000      *****    If an error was detected in a re-initialization routine,
040180000000      *****    bypass processing the record.
040190000000      *****
040200000000
040210000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
040220990922               GO TO DPR-EXIT
040230990922           END-IF
040240990922
040250000000      *****
040260000000      *****    Process the Statement Data record depending on the
040270000000      *****    section number.  If it cannot be identified, an error
040280000000      *****    message will be printed and the record bypassed.  If
040290000000      *****    this occurs, we have a real problem since the GET CURRENT
040300000000      *****    RECORD routine should have bypassed the record.
040310000000      *****
040320000000
040330990922           EVALUATE PR-SECTION-NO
040340990922               WHEN "01"            GO TO  DPR-0010
040350990922               WHEN "05"            GO TO  DPR-0020
040360990922               WHEN "10"            GO TO  DPR-0020-0010
040370990922               WHEN "20"            GO TO  DPR-0030
040380001129               WHEN "30"            GO TO  DPR-0040
040390001129               WHEN "35"            GO TO  DPR-0050
040400010427RF9415         WHEN "40"            GO TO  DPR-0050-0010
040410010427RF9415         WHEN "43"            GO TO  DPR-0050-0050
040420010427RF9415         WHEN "45"            GO TO  DPR-0050-0100
040430001129               WHEN "60"            GO TO  DPR-0055
040440990922               WHEN "80"            GO TO  DPR-0060
040450990922               WHEN OTHER
040460990922                  MOVE SPACES                 TO  ERROR-LINE-1
040470990922                  SET MSG                     TO  2
040480990922                  PERFORM PRINT-ERR-LINE     THRU PEL-EXIT
040490990922                  GO TO DPR-EXIT
040500990922           END-EVALUATE.
040510990922
040520000000       DPR-0010.
040530000000
040540000000      *****
040550000000      *****    Processing an Investor Information record.  Simply
040560000000      *****    indicate that one has been found for the account, then
040570000000      *****    exit.
040580000000      *****
040590000000
040600000000           MOVE "Y"                    TO  WS-STMT-DTA-SW-01.
040610010604R10243     MOVE MFASTMTFP-SECTION      TO  WS-INVESTOR-INFO.
040620000000           GO TO DPR-EXIT.
040630000000
040640000000       DPR-0020.
040650000000
040660000000      *****
040670000000      *****    Processing a Dealer record.  Simply indicate that one has
040680000000      *****    been found for the account, then exit.
040690000000      *****
040700010419
040710010419RF9415     IF WS-DI-LST NOT LESS THAN WS-DI-MAX
040720010419              MOVE SPACES TO ERROR-LINE-1
040730010419              SET MSG     TO 4
040740010419              PERFORM PRINT-ERR-LINE THRU PEL-EXIT
040750010419              GO TO DPR-EXIT
040760010419           END-IF
040770010419
040780010419           ADD 1      TO WS-DI-LST.
040790010419           SET DI     TO WS-DI-LST.
040800010419
040810010419RF9415     MOVE DEALER-SECTION         TO  WS-DEALER-INFO (DI).
040820010419
040830000000           MOVE "Y"                    TO  WS-STMT-DTA-SW-05.
040840000000           GO TO DPR-EXIT.
040850000000
040860000000       DPR-0020-0010.
040870000000
040880000000      *****
040890000000      *****    Processing an Investor Summary record.  Store the Account
040900000000      *****    information for Nominee and Registered Contribution.  If
040910000000      *****    the table is full, an error message will be printed and
040920000000      *****    the account will not be formatted.  If this occurs, the
040930000000      *****    table occurrence should be increased.  Remember to udpate
040940000000      *****    the setting of WS-SIA-MAX in INITIAL-LOGIC.
040950000000      *****
040960000000
040970000000           IF  WS-SIA-LST   NOT LESS THAN  WS-SIA-MAX
040980000000               MOVE SPACES             TO  ERROR-LINE-1
040990000000               SET MSG                 TO  11
041000000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
041010990922               GO TO DPR-0020-0060
041020990922           END-IF
041030990922
041040000000           IF  WS-SIA-LST     IS EQUAL TO  ZEROES
041050000000               SET SIA                 TO  1
041060990922               GO TO DPR-0020-0050
041070990922           END-IF
041080000000
041090000000           SET SIA                     TO  WS-SIA-LST.
041100990922
041110000000           IF  IS-ACCOUNT2   GREATER THAN  WS-SIA-ACCT (SIA)
041120000000               SET SIA              UP BY  1
041130990922               GO TO DPR-0020-0050
041140990922           END-IF
041150000000
041160000000           SET SIA                     TO  1.
041170000000
041180000000       DPR-0020-0020.
041190990922
041200000000           IF  IS-ACCOUNT2   IS LESS THAN  WS-SIA-ACCT (SIA)
041210990922               GO TO DPR-0020-0030
041220990922           END-IF
041230000000
041240000000           SET SIA                  UP BY  1.
041250990922
041260000000           IF  SIA    IS NOT GREATER THAN  WS-SIA-LST
041270990922               GO TO DPR-0020-0020
041280990922           END-IF.
041290000000
041300000000       DPR-0020-0030.
041310990922
041320000000           SET SIA1                    TO  WS-SIA-LST.
041330000000           SET SIA2                    TO  WS-SIA-LST.
041340990922           SET SIA2                    UP BY  1.
041350000000
041360000000       DPR-0020-0040.
041370990922
041380000000           IF  SIA1      IS NOT LESS THAN  SIA
041390000000               MOVE WS-SIA-ENT (SIA1)  TO  WS-SIA-ENT (SIA2)
041400000000               SET SIA1           DOWN BY  1
041410000000               SET SIA2           DOWN BY  1
041420990922               GO TO DPR-0020-0040
041430990922           END-IF.
041440000000
041450000000       DPR-0020-0050.
041460990922
041470000000           MOVE IS-ACCOUNT2            TO  WS-SIA-ACCT (SIA).
041480990922
041490990922           IF IS-NOMINEE     IS EQUAL  TO  "Y"
041500990922              MOVE IS-NOMINEE-CODE     TO  WS-SIA-NOMCDE (SIA)
041510990922              MOVE IS-NOMINEE-NAME     TO  WS-SIA-NOMNAM (SIA)
041520990922              MOVE IS-NOMINEES-ACCT-NO TO  WS-SIA-NOMACCT (SIA)
041530990922           ELSE
041540990922              MOVE SPACES              TO  WS-SIA-NOMCDE (SIA)
041550990922              MOVE SPACES              TO  WS-SIA-NOMNAM (SIA)
041560990922              MOVE SPACES              TO  WS-SIA-NOMACCT (SIA)
041570990922           END-IF
041580990922
041590000000           MOVE IS-REG-CTB-CUR         TO  WS-SIA-REGCTB (SIA).
041600000000           MOVE IS-REG-CTB-CUR-SIGN    TO  WS-SIA-REGCTBSGN (SIA).
041610020111
041620020111R12173     IF  IS-TRI-INCEPT-SIGN   IS EQUAL TO " "
041630001228001229     ADD IS-PUR-INCEPT
041640020111               IS-TRI-INCEPT     GIVING WS-SIA-DEPOSIT-INCEPT (SIA)
041650020111R12173     ELSE
041660020111  ¦        SUBTRACT IS-PUR-INCEPT  FROM
041670020111  ¦               IS-TRI-INCEPT     GIVING WS-SIA-DEPOSIT-INCEPT (SIA)
041680020111R12173     END-IF.
041690020111
041700020111R12173     IF  IS-TRO-INCEPT-SIGN   IS EQUAL TO " "
041710020213R12726     AND IS-RI-SIGN   IS EQUAL TO " "
041720001228           ADD IS-RED-INCEPT
041730020111               IS-TRO-INCEPT     GIVING WS-SIA-WITHDRAW-INCEPT (SIA)
041740020111R12173     ELSE
041750020111  ¦        SUBTRACT IS-RED-INCEPT  FROM
041760020111  ¦               IS-TRO-INCEPT     GIVING WS-SIA-DEPOSIT-INCEPT (SIA)
041770020111R12173     END-IF.
041780020111
041790020111
041800020111
041810020111R12173     IF  IS-TRI-CURRENT-SIGN   IS EQUAL TO " "
041820001228001229     ADD IS-PUR-CURRENT
041830020111               IS-TRI-CURRENT    GIVING WS-SIA-DEPOSIT-CURRENT (SIA)
041840020111           ELSE
041850020111R12173     SUBTRACT  IS-PUR-CURRENT FROM
041860020111  ¦            IS-TRI-CURRENT    GIVING WS-SIA-DEPOSIT-CURRENT (SIA)
041870020111R12173     END-IF
041880020111
041890010309RF8633*    SUBTRACT IS-SWI-CURRENT FROM WS-SIA-DEPOSIT-CURRENT (SIA).
041900001228
041910020111R12173     IF  IS-TRO-CURRENT-SIGN   IS EQUAL TO " "
041920020213R12726     AND IS-RC-SIGN   IS EQUAL TO " "
041930001228           ADD IS-RED-CURRENT
041940020111               IS-TRO-CURRENT    GIVING WS-SIA-WITHDRAW-CURRENT (SIA)
041950020111R12173     ELSE
041960020111  ¦        SUBTRACT IS-RED-CURRENT FROM
041970020111  ¦            IS-TRO-CURRENT    GIVING WS-SIA-WITHDRAW-CURRENT (SIA)
041980020111R12173     END-IF
041990020111
042000010309RF8633*    SUBTRACT IS-SWO-CURRENT FROM WS-SIA-WITHDRAW-CURRENT (SIA).
042010001228
042020010419RF9415     MOVE IS-MAILING-ADDRESS-INDICATOR
042030010419                                       TO  WS-SIA-MAIL-FLAG (SIA).
042040010419
042050000000           ADD 1                       TO  WS-SIA-LST.
042060000000
042070000000       DPR-0020-0060.
042080990922
042090000000           GO TO DPR-EXIT.
042100000000
042110000000       DPR-0030.
042120000000
042130000000      *****
042140000000      *****    Processing an Account Detail record.  These records will
042150000000      *****    be processed, after the Account Transaction records have
042160000000      *****    been processed, by the LEVEL SUMMARIZATION 1 routine.
042170000000      *****    For now, accumulate the fund market value by currency and
042180000000      *****    store the data.  If the currency cannot be identified, an
042190000000      *****    error message will be printed and the record bypassed.
042200000000      *****    If this occurs, the program should be changed to handle
042210000000      *****    the new currency.  Remember to update the setting of
042220000000      *****    the setting of WS-SC-MAX in INITIAL-LOGIC.
042230000000      *****
042240000000
042250010501RF9415     MOVE "N"                    TO  WS-STMT-DTA-SW-40.
042260010501RF9415     MOVE "N"                    TO  WS-STMT-DTA-SW-43.
042270010501RF9415     MOVE "N"                    TO  WS-STMT-DTA-SW-45.
042280010501
042290000000           SET SC                      TO  1.
042300000000
042310001129       DPR-0030-0010.
042320990922
042330000000           IF  AD-CURRENCY    IS EQUAL TO  WS-SC-CURRENCY (SC)
042340001129               GO TO DPR-0030-0020
042350990922           END-IF
042360990922
042370000000           IF  SC            IS LESS THAN  WS-SC-MAX
042380000000               SET SC               UP BY  1
042390001130               GO TO DPR-0030-0010
042400990922           END-IF
042410990922
042420000000           MOVE SPACES                 TO  ERROR-LINE-1.
042430000000           SET MSG                     TO  3.
042440000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
042450000000           GO TO DPR-EXIT.
042460000000
042470001129       DPR-0030-0020.
042480990922
042490990922           IF AD-MARKET-VALUE-SIGN     IS EQUAL TO  SPACES
042500990922              ADD AD-MARKET-VALUE      TO  WS-SC-MARKET-VALUE (SC)
042510001201001201        ADD AD-MARKET-VALUE      TO  WS-AD-MARKET-VALUE-TOT
042520990922           ELSE
042530990922              SUBTRACT AD-MARKET-VALUE  FROM WS-SC-MARKET-VALUE (SC)
042540001201001201        SUBTRACT AD-MARKET-VALUE  FROM WS-AD-MARKET-VALUE-TOT
042550990922           END-IF
042560990922
042570020613R11175     ADD AD-ACCOUNT-PERC         TO  WS-SC-MARKET-PERC (SC).
042580010912
042590001201           IF AD-MARKET-VALUE-PRIOR-SIGN     IS EQUAL TO  SPACES
042600001201001201        ADD AD-MARKET-VALUE-PRIOR
042610001201001201                              TO  WS-AD-MARKET-VALUE-PRIOR-TOT
042620001201           ELSE
042630001201001201        SUBTRACT AD-MARKET-VALUE-PRIOR
042640001201001201                              FROM WS-AD-MARKET-VALUE-PRIOR-TOT
042650001201           END-IF
042660001201
042670001219001219***  Populate Deposit and withdrawals in the date range
042680001219
042690001228      *    ADD AD-TRI-CURRENT
042700001228      *        AD-PUR-CURRENT           TO   WS-AD-DEPOSIT1-TOT.
042710001228      *    SUBTRACT AD-SWI-CURRENT      FROM WS-AD-DEPOSIT1-TOT.
042720001219
042730001228      *    ADD AD-TRO-CURRENT
042740001228      *        AD-RED-CURRENT           TO   WS-AD-WITHDRAWALS1-TOT.
042750001228      *    SUBTRACT AD-SWO-CURRENT      FROM WS-AD-WITHDRAWALS1-TOT.
042760001219
042770001228001219***  Populate Deposit and withdrawals since the opening.
042780001219
042790001228      *    ADD AD-TRI-INCEPT
042800001228      *        AD-PUR-INCEPT            TO   WS-AD-DEPOSIT2-TOT.
042810001219
042820001228      *    ADD AD-TRO-INCEPT
042830001228      *        AD-RED-INCEPT            TO   WS-AD-WITHDRAWALS2-TOT.
042840001219
042850000000           MOVE "Y"                    TO  WS-SC-RPTIND (SC).
042860000000
042870000000      *****
042880000000      *****    Format the fund in the Statement Account Fund table.  If
042890000000      *****    the table is full, an error message will be printed and
042900000000      *****    the record bypassed.  If this occurs, the program should
042910000000      *****    be changed to handle more funds for an account.  Remember
042920000000      *****    to update the setting of WS-SAF-MAX in INITIAL-LOGIC.
042930000000      *****
042940000000
042950000000           IF  WS-SAF-LST   NOT LESS THAN  WS-SAF-MAX
042960000000               MOVE SPACES             TO  ERROR-LINE-1
042970000000               SET MSG                 TO  4
042980000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
042990990922               GO TO DPR-EXIT
043000990922           END-IF
043010990922
043020000000      *****
043030000000      *****    Calculate the net invested, if required.
043040000000      *****
043050000000
043060000000           MOVE ZEROES                 TO  WS-NET-INVESTED.
043070000000
043080990922           IF  WS-LDA-PRT-NET-INVEST   IS NOT EQUAL TO  "Y"
043090001129               GO TO DPR-0030-0030
043100990922           END-IF
043110990922
043120000000           IF  AD-PI-SIGN     IS EQUAL TO  "-"
043130990922               SUBTRACT AD-PUR-INCEPT  FROM  WS-NET-INVESTED
043140990922           ELSE
043150990922               ADD AD-PUR-INCEPT       TO  WS-NET-INVESTED
043160990922           END-IF
043170990922
043180000000           IF  AD-RI-SIGN     IS EQUAL TO  "-"
043190000000               ADD AD-RED-INCEPT       TO  WS-NET-INVESTED
043200990922           ELSE
043210990922               SUBTRACT AD-RED-INCEPT  FROM  WS-NET-INVESTED
043220990922           END-IF
043230990922
043240000000           IF  AD-TI-SIGN     IS EQUAL TO  "-"
043250990922               SUBTRACT AD-TRI-INCEPT  FROM  WS-NET-INVESTED
043260990922           ELSE
043270020801               ADD AD-TRI-INCEPT       TO  WS-NET-INVESTED
043280990922           END-IF
043290990922
043300000000           IF  AD-OI-SIGN     IS EQUAL TO  "-"
043310000000               ADD AD-TRO-INCEPT       TO  WS-NET-INVESTED
043320990922           ELSE
043330990922               SUBTRACT AD-TRO-INCEPT  FROM  WS-NET-INVESTED
043340990922           END-IF.
043350020801
043360020806R13637     SUBTRACT AD-SWO-INCEPT  FROM  WS-NET-INVESTED.
043370020806R13637     ADD      AD-SWI-INCEPT  TO    WS-NET-INVESTED.
043380000000
043390001129       DPR-0030-0030.
043400990922
043410000000           ADD 1                       TO  WS-SAF-LST.
043420000000           SET SAF                     TO  WS-SAF-LST.
043430000000           MOVE AD-ACB-UNIT-PRICE      TO  WS-SAF-ACB-UNIT-PRICE (SAF).
043440000000           MOVE AD-ACB-UNIT-PRICE-SIGN TO  WS-SAF-ACB-UNIT-PRICE-SIGN
043450000000                                             (SAF).
043460000000           MOVE AD-CURRENCY            TO  WS-SAF-CURRENCY (SAF).
043470000000           MOVE AD-FUND-NO             TO  WS-SAF-FUND-NO (SAF).
043480000000           MOVE AD-INVESTMENT-NAME     TO  WS-SAF-INVESTMENT-NAME (SAF).
043490000000           MOVE AD-LAST-UNIT-PRICE     TO  WS-SAF-LAST-UNIT-PRICE (SAF).
043500000000           MOVE AD-LAST-UNIT-PRICE-SIGN
043510000000                                       TO  WS-SAF-LAST-UNIT-PRICE-SIGN
043520000000                                             (SAF).
043530000000           MOVE AD-MARKET-VALUE        TO  WS-SAF-MARKET-VALUE (SAF).
043540000000           MOVE AD-MARKET-VALUE-SIGN   TO  WS-SAF-MARKET-VALUE-SIGN
043550000000                                             (SAF).
043560010912R11175     MOVE AD-ACCOUNT-PERC        TO  WS-SAF-MARKET-PERC (SAF).
043570010912
043580000000           IF  WS-NET-INVESTED  LESS THAN  ZEROES
043590000000               SUBTRACT WS-NET-INVESTED
043600000000                                     FROM  ZEROES
043610000000                                   GIVING  WS-SAF-NET-INVESTED (SAF)
043620000000               MOVE "-"                TO  WS-SAF-NET-INVESTED-SIGN
043630000000                                             (SAF)
043640990922           ELSE
043650000000               MOVE WS-NET-INVESTED    TO  WS-SAF-NET-INVESTED (SAF)
043660000000               MOVE SPACES             TO  WS-SAF-NET-INVESTED-SIGN
043670990922                                             (SAF)
043680990922           END-IF
043690990922
043700000000           MOVE AD-NO-OF-UNITS         TO  WS-SAF-NO-OF-UNITS (SAF).
043710000000           MOVE AD-NO-OF-UNITS-SIGN    TO  WS-SAF-NO-OF-UNITS-SIGN
043720000000                                             (SAF).
043730971223
043740971223      *****
043750971223      *****    Accumulate the Canadian dollar capital gains amount.
043760971223      *****
043770971223
043780990922           IF  AD-CPG-FIXED-SIGN EQUAL  TO  " "
043790990922               ADD AD-CPG-FIXED         TO  WS-CPG-FIXED
043800990922           ELSE
043810990922               SUBTRACT AD-CPG-FIXED    FROM  WS-CPG-FIXED
043820990922           END-IF
043830990922
043840001130
043850001130      *****
043860001130      *****    After account-details record was processed above,
043870001130      *****    move it to account fund table.
043880001130      *****
043890001130
043900001130           ADD 1                       TO  WS-SAF2-LST.
043910001130           SET SAF2                    TO  WS-SAF2-LST.
043920001130
043930001130           MOVE ACCOUNT-DETAILS-SECTION
043940001130                              TO  WS-ACCOUNT-DETAILS-SECTION (SAF2).
043950001130
043960000000           GO TO DPR-EXIT.
043970000000
043980001129       DPR-0040.
043990001127
044000001129      *****
044010001129      *****    Processing an Account Performance record, indicate that
044020001129      *****    one has been found for the account, then exit.
044030001129      *****
044040001129
044050001129           MOVE "Y"                    TO  WS-STMT-DTA-SW-30.
044060001129           GO TO DPR-EXIT.
044070001129
044080001129       DPR-0050.
044090001127
044100001127      *****
044110001129      *****    Processing an Account Fund Performance record.  Simply
044120001129      *****    indicate that one has been found for the account. The
044130001127      *****    recors will be processed befor the Account Transactions
044140001127      *****    processing.
044150001127      *****
044160001127
044170001129           ADD 1                       TO  WS-SAFP-LST.
044180001130           SET AFP                     TO  WS-SAFP-LST.
044190001129
044200001129           MOVE ACCOUNT-FUND-PERFORM-SECTION
044210001130                              TO  WS-ACCOUNT-FUND-PERFORM (AFP).
044220001129
044230001129           MOVE "Y"                    TO  WS-STMT-DTA-SW-35.
044240001129           GO TO DPR-EXIT.
044250001129
044260010427       DPR-0050-0010.
044270010427
044280010427      *****
044290010427      *****    Processing a SEG-FUND-CONTRACT-DETAILS record, select its
044300010427      *****
044310010427
044320010427RF9415     IF WS-SFC-LST NOT LESS THAN WS-SFC-MAX
044330010427              MOVE SPACES TO ERROR-LINE-1
044340010427              SET MSG     TO 4
044350010427              PERFORM PRINT-ERR-LINE THRU PEL-EXIT
044360010427              GO TO DPR-EXIT
044370010427           END-IF
044380010427
044390010427           ADD 1       TO WS-SFC-LST.
044400010427           SET SFC     TO WS-SFC-LST.
044410010427
044420010427           MOVE SEG-FUND-CONTRACT-DETAILS
044430010427                            TO  WS-SEG-FUND-CONTRACT-DETAILS (SFC)
044440010427           MOVE "Y"         TO  WS-STMT-DTA-SW-40.
044450010427           GO TO DPR-EXIT.
044460010427
044470010427       DPR-0050-0050.
044480010427
044490010427      *****
044500010427      *****    Processing a SEG-FUND-CONTRACT-INVESTMENT RECORD, selects
044510010427      *****    it.
044520010427      *****
044530010427
044540010427RF9415     IF WS-SFCI-LST NOT LESS THAN WS-SFCI-MAX
044550010427              MOVE SPACES TO ERROR-LINE-1
044560010427              SET MSG     TO 4
044570010427              PERFORM PRINT-ERR-LINE THRU PEL-EXIT
044580010427              GO TO DPR-EXIT
044590010427           END-IF
044600010427
044610010427           ADD 1       TO WS-SFCI-LST.
044620010515           SET SFCI    TO WS-SFCI-LST.
044630010427
044640010427RF9415     MOVE SEG-FUND-CONTRACT-INVESTMENT
044650010427                            TO  WS-SEG-FUND-CONTRACT-INV (SFCI)
044660010427           MOVE "Y"         TO  WS-STMT-DTA-SW-43.
044670010427           GO TO DPR-EXIT.
044680010427
044690010427       DPR-0050-0100.
044700010427
044710010427      *****
044720010427      *****    Processing a ANNUITANT-DETAILS record, select it.       s
044730010427      *****
044740010427
044750010427RF9415     IF WS-ANN-LST NOT LESS THAN WS-ANN-MAX
044760010427              MOVE SPACES TO ERROR-LINE-1
044770010427              SET MSG     TO 4
044780010427              PERFORM PRINT-ERR-LINE THRU PEL-EXIT
044790010427              GO TO DPR-EXIT
044800010427           END-IF
044810010427
044820010427           ADD 1       TO WS-ANN-LST.
044830010427           SET ANN     TO WS-ANN-LST.
044840010427
044850010427RF9415     MOVE ANNUITANT-DETAILS      TO  WS-ANNUITANT-DETAILS (ANN)
044860010427           MOVE "Y"                    TO  WS-STMT-DTA-SW-45.
044870010427           GO TO DPR-EXIT.
044880010427
044890001129       DPR-0055.
044900001129
044910001129      *****
044920001129      *****    Processing a RRIF?LIF Detail record.  Simply indicate
044930001129      *****    that one has been found for the account, then exit.
044940001129      *****
044950001129
044960001129           MOVE "Y"     TO WS-STMT-DTA-SW-60.
044970001129           GO TO DPR-EXIT.
044980001129
044990000000       DPR-0060.
045000000000
045010000000      *****
045020000000      *****    Processing an Account Transaction record.  These records
045030000000      *****    will be processed using the MLS programming techniques,
045040000000      *****    without the performs.  The current and previous controls
045050000000      *****    have been initialized in the LEVEL REINITIALIZATION 1
045060000000      *****    routine and the records have been read in the GET CURRENT
045070000000      *****    routine.  Therefore, the first step will be to format the
045080000000      *****    current statement transaction controls.
045090000000      *****
045100000000
045110001220           MOVE "Y"                     TO WS-STMT-DTA-SW-80.
045120010427
045130010430           IF  WS-LR1-PROCESSED-SW  EQUAL TO  "Y"
045140010430               GO TO DPR-0060-0005
045150010430           END-IF
045160010430
045170010427           IF  WS-STMT-DTA-SW-01 EQUAL TO  "Y"
045180010427           AND WS-STMT-DTA-SW-05 EQUAL TO  "Y"
045190010501RF9415*    AND WS-STMT-DTA-SW-40 EQUAL TO  "Y"
045200010427               PERFORM LEVEL-REINIT-1     THRU LR1-EXIT
045210010430               MOVE "Y"                   TO  WS-LR1-PROCESSED-SW
045220010427           END-IF
045230010427
045240001201001128     IF  WS-STMT-DTA-SW-30        IS EQUAL TO "Y"
045250001201001129     AND WS-STMT-DTA-SW-35        IS EQUAL TO "Y"
045260010501RF9415*    AND WS-STMT-DTA-SW-40        IS EQUAL TO "Y"
045270001201001129     AND WS-PLAN-SUM-SW           IS EQUAL TO "N"
045280001129              PERFORM POPULATE-PLAN-VALUE-ROUTINE       THRU PPVR-EXIT
045290001201              MOVE "Y"                  TO WS-PLAN-SUM-SW
045300001201           END-IF.
045310001128
045320010430       DPR-0060-0005.
045330010430
045340001204001204     IF WS-LDA-PRINT-HEADING           IS NOT EQUAL TO "Y"
045350001204              GO TO DPR-0060-0010
045360001204           END-IF
045370001204
045380001205001128     IF WS-TRANS-HEADING-SW      IS EQUAL TO "N"
045390001215      *       SET SPL                     UP BY  3
045400001205              MOVE WS-SHL01 (SHL)         TO  WS-STMTPGE-LNE (SPL)
045410001205              SET SPL                     UP BY  3
045420001205              MOVE WS-SHL02 (SHL)         TO  WS-STMTPGE-LNE (SPL)
045430001205              SET SPL                     UP BY  1
045440001205              MOVE WS-SHL03 (SHL)         TO  WS-STMTPGE-LNE (SPL)
045450001205              SET SPL                     UP BY  1
045460001205              MOVE WS-SHL04 (SHL)         TO  WS-STMTPGE-LNE (SPL)
045470001205              MOVE "Y"                    TO WS-TRANS-HEADING-SW
045480001205           END-IF.
045490001205
045500001205
045510001204       DPR-0060-0010.
045520001204
045530000000      *****
045540000000      *****    Determine if this is a new FUND being processed for the
045550000000      *****    account.
045560000000      *****
045570000000
045580001204           MOVE AT-FUND-NO             TO  CURR-CD-FUND-NO.
045590001204
045600000000           IF  PREV-CD-LEV1   IS EQUAL TO  CURR-CD-LEV1
045610990922               GO TO DPR-0120
045620990922           END-IF
045630000000
045640000000      *****
045650000000      *****    A new fund is being processed.  If this is the first fund
045660000000      *****    for the account, bypass the summary logic.
045670000000      *****
045680000000
045690000000           IF  PREV-CTLS-DPR  IS EQUAL TO  LOW-VALUES
045700990922               GO TO DPR-0070
045710990922           END-IF
045720990922
045730000000      *****
045740000000      *****    Place the fund summary logic here.  Currently, the only
045750000000      *****    processing required is to print a blank line.  Please
045760000000      *****    note that if the statement page is full, the blank line
045770000000      *****    is not formatted.  The next Account Transaction or the
045780000000      *****    Account Summary will skip to the new page, if required.
045790000000      *****
045800000000
045810000000           IF  SPL           IS LESS THAN  WS-SPL-DTLLST
045820990922               SET SPL              UP BY  1
045830990922           END-IF.
045840000000
045850000000       DPR-0070.
045860000000
045870000000      *****
045880000000      *****    Normally, MLS checks the current controls to determine if
045890000000      *****    processing is complete.  Since the Account Transactions
045900000000      *****    are read in the GET CURRENT RECORD routine, this will be
045910000000      *****    done by the normal MLS and will not be required here.
045920000000      *****    Therefore, place the fund re-initialization logic here.
045930000000      *****    Verify that an Account Detail has been read for the fund.
045940000000      *****    If one has not been, a warning message will be printed
045950000000      *****    and the record will be processed as normal.  If this
045960000000      *****    occurs, the program that creates the Statement Data file
045970000000      *****    should be reviewed.
045980000000      *****
045990000000
046000000000           SET SAF                     TO  1.
046010990922
046020000000           IF  SAF        IS GREATER THAN  WS-SAF-LST
046030990922               GO TO DPR-0090
046040990922           END-IF.
046050990922
046060000000       DPR-0080.
046070990922
046080000000           IF  CURR-CD-FUND-NO   EQUAL TO  WS-SAF-FUND-NO (SAF)
046090990922               GO TO DPR-0100
046100990922           END-IF
046110000000
046120000000           IF  SAF           IS LESS THAN  WS-SAF-LST
046130000000               SET SAF              UP BY  1
046140990922               GO TO DPR-0080
046150990922           END-IF.
046160000000
046170000000       DPR-0090.
046180990922
046190000000           MOVE SPACES                 TO  ERROR-LINE-1.
046200000000           SET MSG                     TO  5.
046210990922           PERFORM PRINT-ERR-LINE      THRU PEL-EXIT.
046220000000
046230000000       DPR-0100.
046240000000
046250000000      *****
046260000000      *****    Format the Investment Name.  If this line and at least
046270000000      *****    one transaction line cannot be printed on the current
046280000000      *****    page, process this page and start a subsequent page.
046290000000      *****
046300000000
046310000000           MOVE SPACES                 TO  STMT-DTL-01.
046320000000
046330990922           EVALUATE WS-STMTVRSN
046340001219             WHEN 9
046350991213               MOVE AT-INVESTMENT-NAME TO  SD01V8-INVESTMENT-NAME
046360990922             WHEN OTHER
046370990922               MOVE AT-INVESTMENT-NAME TO  SD01-INVESTMENT-NAME
046380990922           END-EVALUATE.
046390990922
046400000000       DPR-0100-0010.
046410990921
046420000000           SET WS-SPL-DTLTST           TO  SPL.
046430990624
046440990624           IF  (WS-SPL-DTLTST           +  3)
046450000000                      IS NOT GREATER THAN  WS-SPL-DTLLST
046460990921               GO TO DPR-0110
046470990921           END-IF
046480990624
046490990624           IF WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
046500990624           AND (WS-SPL-DTLTST           +  5)
046510990624                      IS NOT GREATER THAN  WS-SPL-DTLLST
046520990921               GO TO DPR-0110
046530990921           END-IF
046540990624
046550000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
046560990921
046570000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
046580990921               GO TO DPR-EXIT
046590990921           END-IF.
046600000000
046610000000       DPR-0110.
046620990921
046630001124           IF WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
046640001220      *       SET SPL              UP BY  3
046650001220001220        SET SPL              UP BY  2
046660001124           ELSE
046670001124              SET SPL              UP BY  1
046680001124           END-IF
046690990925
046700990226           MOVE STMT-DTL-01         TO  WS-STMTPGE-LNE (SPL).
046710000000           SET SPL                  UP BY  1.
046720000000
046730000000       DPR-0120.
046740000000
046750000000      *****
046760000000      *****    Place the detail processing logic here.  The Account
046770000000      *****    Transaction record is to be processed depending on the
046780000000      *****    Transaction Type Code.
046790000000      *****
046800000000
046810990921           IF  AT-TRANS-TYPE-CODE  IS EQUAL TO  "BFW"
046820990921               GO TO DPR-0130
046830990921           END-IF
046840000000
046850990921           IF  AT-TRANS-TYPE-CODE  IS EQUAL TO  "BCL"
046860990921               GO TO DPR-0130-0010
046870990921           END-IF
046880000000
046890000000           GO TO DPR-0140.
046900000000
046910000000       DPR-0130.
046920000000
046930000000      *****
046940000000      *****    Processing a BALANCE FORWARD transaction.  Format the
046950000000      *****    detail line.
046960000000      *****
046970000000
046980000000           MOVE SPACES                 TO  STMT-DTL-02.
046990000000
047000990921           EVALUATE WS-STMTVRSN
047010001219             WHEN 9
047020001124               MOVE AT-OPEN-DATE          TO  SD02V8-TRADE-DATE
047030991213               MOVE AT-TRANS-TYPE         TO  SD02V8-TRANS-TYPE
047040001124               MOVE AT-MKT-VALUE          TO  SD02V8-GROSS
047050001124               MOVE AT-UNIT-PRICE         TO  SD02V8-UNIT-PRICE
047060001124               MOVE AT-UNIT-PRICE-SIGN    TO  SD02V8-UNIT-PRICE-SIGN
047070991213               MOVE AT-UNIT-BALANCE       TO  SD02V8-UNIT-BALANCE
047080991213               MOVE AT-UNIT-BALANCE-SIGN
047090991213                                       TO  SD02V8-UNIT-BALANCE-SIGN
047100990922             WHEN OTHER
047110990921               MOVE WS-SD02-LIT1 (SL)      TO  SD02-LIT1
047120990921               MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE
047130990921               MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE
047140990921               MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN
047150990921           END-EVALUATE.
047160990921
047170000000       DPR-0130-0000-0010.
047180990921
047190000000           MOVE ZEROES                 TO  WS-CPTL-GAIN.
047200000000           MOVE SPACES                 TO  WS-CPTL-GAIN-SIGN.
047210000000           GO TO DPR-0150.
047220000000
047230000000       DPR-0130-0010.
047240000000
047250000000      *****
047260000000      *****    Processing a BALANCE CLOSING transaction.  Format the
047270000000      *****    detail line.
047280000000      *****
047290000000
047300000000           MOVE SPACES                 TO  STMT-DTL-02.
047310000000
047320990921           EVALUATE WS-STMTVRSN
047330001219             WHEN 9
047340991213               MOVE WS-SH-END-DATE     TO  SD02V8-TRADE-DATE
047350991213               MOVE AT-TRANS-TYPE      TO  SD02V8-TRANS-TYPE
047360000808RF7330         MOVE AT-MKT-VALUE          TO  SD02V8-GROSS
047370000808RF7330         MOVE AT-UNIT-PRICE        TO  SD02V8-UNIT-PRICE
047380000808RF7330         MOVE AT-UNIT-PRICE-SIGN   TO  SD02V8-UNIT-PRICE-SIGN
047390991213               MOVE AT-UNIT-BALANCE    TO  SD02V8-UNIT-BALANCE
047400991213               MOVE AT-UNIT-BALANCE-SIGN
047410991213                                       TO  SD02V8-UNIT-BALANCE-SIGN
047420990922             WHEN OTHER
047430991013               MOVE WS-SH-END-DATE-2       TO  SD02-TRADE-DATE
047440990921               MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE
047450990921               MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE
047460990921               MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN
047470990921           END-EVALUATE.
047480990921
047490000000       DPR-0130-0020.
047500990921
047510000000           MOVE ZEROES                 TO  WS-CPTL-GAIN.
047520000000           MOVE SPACES                 TO  WS-CPTL-GAIN-SIGN.
047530000000           GO TO DPR-0150.
047540000000
047550000000       DPR-0140.
047560000000
047570000000      *****
047580000000      *****    Processing a transaction other than a BALANCE FORWARD or
047590000000      *****    BALANCE CLOSING.  Format the detail line.
047600000000      *****
047610000000      *****    Please note that the CAPITAL GAIN/LOSS is only being
047620000000      *****    formatted if it is non-zero.  If this does not satisfy
047630000000      *****    the requirements, logic should be added to format this
047640000000      *****    field base on the transaction type.
047650000000      *****
047660000000
047670000000           MOVE AT-TRADE-DATE          TO  WS-TRADE-DATE.
047680990923           MOVE AT-TRADE-DATE          TO  WS-TRADE-DATE-6.
047690000000           MOVE ZEROES                 TO  WS-DEDUCTIONS.
047700991013           MOVE SPACES                 TO  STMT-DTL-02.
047710991013
047720001124991103     IF WS-AT-RULE (AT1)           IS EQUAL TO "4"
047730001124           AND ( AT-TRANS-TYPE-CODE      IS EQUAL TO "BUY"
047740991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "SEL"
047750991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "TRI"
047760991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "TRO")
047770991013                MOVE AT-CONTR-REDEMP-DESCRIPTION
047780001124                                         TO AT-TRANS-TYPE
047790991013           END-IF
047800991013
047810990921           IF  AT-COMMISSION-SIGN  IS EQUAL TO  "-"
047820990921               SUBTRACT AT-COMMISSION FROM  WS-DEDUCTIONS
047830990921           ELSE
047840990921               ADD AT-COMMISSION       TO  WS-DEDUCTIONS
047850990921           END-IF
047860990921
047870001012RF4638     IF  AT-DSC-SIGN         IS EQUAL TO  "-"
047880001012               SUBTRACT AT-DSC        FROM  WS-DEDUCTIONS
047890001012           ELSE
047900001012               ADD AT-DSC              TO  WS-DEDUCTIONS
047910001012           END-IF
047920001012
047930000000           IF  AT-TAX-SIGN    IS EQUAL TO  "-"
047940000000               SUBTRACT AT-TAX       FROM  WS-DEDUCTIONS
047950990921           ELSE
047960990921               ADD AT-TAX              TO  WS-DEDUCTIONS
047970990921           END-IF
047980010130
047990010130RF9184     ADD  AT-ADMIN-FEES          TO  WS-DEDUCTIONS.
048000010130RF9298     ADD  AT-GST
048010050402R26894          AT-ERF
048020010130                AT-PST                 TO  WS-DEDUCTIONS.
048030010130
048040000000           IF  WS-DEDUCTIONS IS LESS THAN  ZEROES
048050000000               MOVE "-"                TO  WS-DEDUCTIONS-SIGN
048060990921           ELSE
048070990921               MOVE SPACES             TO  WS-DEDUCTIONS-SIGN
048080990921           END-IF
048090000000
048100990921980417     IF  WS-LDA-CAPITAL-GAIN  IS NOT EQUAL TO  "N"
048110000000               MOVE AT-CPTL-GAIN       TO  WS-CPTL-GAIN
048120000000               MOVE AT-CPTL-GAIN-SIGN  TO  WS-CPTL-GAIN-SIGN
048130990921           ELSE
048140000000               MOVE ZEROES             TO  WS-CPTL-GAIN
048150990921               MOVE SPACES             TO  WS-CPTL-GAIN-SIGN
048160990921           END-IF
048170990921
048180000000
048190990921      *** MOVE DATE TO STMT-DTL-02 DEPENDING ON STMTVRSN
048200990921
048210001219           IF  WS-STMTVRSN   NOT EQUAL TO  9
048220991213               GO TO DPR-0140-0080
048230991213           END-IF
048240991213
048250991213           MOVE WS-SH-TRADE-DATE-6     TO  SD02V8-TRADE-DATE.
048260991213           MOVE AT-TRANS-TYPE          TO  SD02V8-TRANS-TYPE.
048270991213           MOVE AT-GROSS               TO  SD02V8-GROSS.
048280991213           MOVE AT-GROSS-SIGN          TO  SD02V8-GROSS-SIGN.
048290991213           MOVE WS-DEDUCTIONS          TO  SD02V8-DEDUCTIONS.
048300991213           MOVE AT-NET                 TO  SD02V8-NET.
048310991213           MOVE AT-NET-SIGN            TO  SD02V8-NET-SIGN.
048320991213           MOVE AT-UNIT-PRICE          TO  SD02V8-UNIT-PRICE.
048330991213           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V8-UNIT-PRICE-SIGN.
048340991213           MOVE AT-NO-OF-UNITS         TO  SD02V8-NO-OF-UNITS.
048350991213           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V8-NO-OF-UNITS-SIGN.
048360991213           MOVE AT-UNIT-BALANCE        TO  SD02V8-UNIT-BALANCE.
048370991213           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V8-UNIT-BALANCE-SIGN.
048380991213
048390991213           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
048400991213               MOVE WS-CPTL-GAIN       TO  SD02V8-CPTL-GAIN
048410991213               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V8-CPTL-GAIN-SIGN
048420991213           END-IF
048430991213
048440991213           GO TO DPR-0140-9999.
048450991213
048460991213       DPR-0140-0080.
048470990921
048480000000           MOVE WS-SH-TRADE-DATE       TO  SD02-TRADE-DATE.
048490000000           MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE.
048500000000           MOVE AT-GROSS               TO  SD02-GROSS.
048510000000           MOVE AT-GROSS-SIGN          TO  SD02-GROSS-SIGN.
048520000000           MOVE WS-DEDUCTIONS          TO  SD02-DEDUCTIONS.
048530000000           MOVE AT-NET                 TO  SD02-NET.
048540000000           MOVE AT-NET-SIGN            TO  SD02-NET-SIGN.
048550000000           MOVE AT-UNIT-PRICE          TO  SD02-UNIT-PRICE.
048560000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02-UNIT-PRICE-SIGN.
048570000000           MOVE AT-NO-OF-UNITS         TO  SD02-NO-OF-UNITS.
048580000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02-NO-OF-UNITS-SIGN.
048590000000           MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE.
048600000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN.
048610990921
048620000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
048630000000               MOVE WS-CPTL-GAIN       TO  SD02-CPTL-GAIN
048640990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02-CPTL-GAIN-SIGN
048650990921           END-IF.
048660990921
048670000000       DPR-0140-9999.
048680990921
048690000000           GO TO DPR-0150.
048700000000
048710000000       DPR-0150.
048720000000
048730000000      *****
048740000000      *****    If the transaction cannot be printed on the current page,
048750000000      *****    process the current page and start a subsequent page.
048760000000      *****    When determining if the transaction can be printed on the
048770000000      *****    current page, additional checks regarding a capital gain
048780000000      *****    overflow line are included.
048790000000      *****
048800000000
048810990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  99999.99
048820000000           AND SPL           IS LESS THAN  WS-SPL-DTLLST
048830990921               GO TO DPR-0160
048840990921           END-IF
048850990921
048860000000           SET WS-SPL                  TO  SPL.
048870990921
048880000000           IF  WS-CPTL-GAIN  GREATER THAN  99999.99
048890000000           AND (WS-SPL + 1)  IS LESS THAN  WS-SPL-DTLLST
048900990921               GO TO DPR-0160
048910990921           END-IF
048920000000
048930000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
048940990921
048950000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
048960990921               GO TO DPR-EXIT
048970990921           END-IF.
048980000000
048990000000       DPR-0160.
049000990921
049010990921           SET SPL                     UP BY  1.
049020000000           MOVE STMT-DTL-02            TO  WS-STMTPGE-LNE (SPL).
049030000000
049040000000      *****
049050000000      *****    If the transaction Capital Gain value exceeds 99,999.99,
049060000000      *****    a second line will be printed to indicate that the value
049070000000      *****    is in excess of the printed amount.  This additional line
049080000000      *****    is due to space restrictions on the form itself.
049090000000      *****
049100000000
049110990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  99999.99
049120990921               GO TO DPR-0170
049130990921           END-IF
049140990921
049150000000           MOVE SPACES                 TO  STMT-DTL-02.
049160991019
049170991214       DPR-0160-0080.
049180991214
049190001219           IF  WS-STMTVRSN   NOT EQUAL TO  9
049200991214               GO TO DPR-0160-0090
049210991214           END-IF
049220991214
049230991214           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
049240991214               GO TO DPR-0170
049250991214           END-IF
049260991214
049270991214           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
049280991214               MOVE "(++++++++++)"     TO  SD02V8-CPTL-GAIN-OFLIT
049290991214           ELSE
049300991214               MOVE "(----------)"     TO  SD02V8-CPTL-GAIN-OFLIT
049310991214           END-IF
049320991214
049330991214           GO TO DPR-0160-9999.
049340991214
049350991214       DPR-0160-0090.
049360990921
049370000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
049380000000               MOVE "(++++++++)"       TO  SD02-CPTL-GAIN-OFLIT
049390990921           ELSE
049400990921               MOVE "(--------)"       TO  SD02-CPTL-GAIN-OFLIT
049410990921           END-IF.
049420000000
049430000000       DPR-0160-9999.
049440990921
049450001127           SET SPL                   UP BY  1.
049460001127           MOVE STMT-DTL-02          TO  WS-STMTPGE-LNE (SPL).
049470990924
049480000000       DPR-0170.
049490000000
049500000000      *****
049510000000      *****    The Account Transaction has been completely processed.
049520000000      *****    Update the previous statement transaction controls and
049530000000      *****    exit.  The next, if any, Account Transaction will be read
049540000000      *****    by the GET CURRENT RECORD routine.
049550000000      *****
049560000000
049570000000           MOVE CURR-CTLS-DPR          TO  PREV-CTLS-DPR.
049580981215           GO TO DPR-EXIT.
049590990921
049600990921      **********
049610990921
049620000000       DPR-EXIT.
049630000000           EXIT.
049640990921
049650990921      **********
049660990921
049670000000      /
049680000000       LEVEL-SUMMARY-1.
049690000000
049700000000      ******************************************************************
049710000000      *                                                                *
049720000000      *    L E V E L   S U M M A R I Z A T I O N   1   R O U T I N E   *
049730000000      *                                                                *
049740000000      *        ACCOUNT:                Account                         *
049750000000      *                                                                *
049760000000      ******************************************************************
049770000000
049780000000      *****
049790000000      *****    If Account is zeroes, bypass this routine.
049800000000      *****
049810000000
049820000000           IF  PREV-ACCOUNT   IS EQUAL TO  ZEROES
049830990921               GO TO LS1-EXIT
049840990921           END-IF
049850990921
049860000000      *****
049870000000      *****    If an error was detected by the LEVEL REINITIALIZATION 1
049880000000      *****    routine, bypass this routine.
049890000000      *****
049900000000
049910000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
049920990921               GO TO LS1-EXIT
049930990921           END-IF
049940001205
049950980529           IF  WS-LDA-SUM-SUPPRESS-FLAG  IS EQUAL TO "Y"
049960990921               GO TO LS1-0390
049970990921           END-IF
049980990921
049990001220001128     IF  WS-STMT-DTA-SW-30        IS EQUAL TO "Y"
050000001220001129     AND WS-STMT-DTA-SW-35        IS EQUAL TO "Y"
050010010502RF9415*    AND WS-STMT-DTA-SW-40        IS EQUAL TO "Y"
050020010502RF9415*    AND WS-STMT-DTA-SW-43        IS EQUAL TO "Y"
050030010502RF9415*    AND WS-STMT-DTA-SW-45        IS EQUAL TO "Y"
050040001220001129     AND WS-STMT-DTA-SW-80        IS EQUAL TO "N"
050050010604              IF CURR-LEVEL2         IS NOT EQUAL TO PREV-LEVEL2
050060010604                 INITIALIZE  INVESTOR-INFO-SECTION
050070010604                 MOVE WS-INVESTOR-INFO   TO INVESTOR-INFO-SECTION
050080010604              END-IF
050090010517010517        PERFORM GET-NO-TRANS-ACCOUNT1 THRU LRA1-EXIT
050100010517010502*       PERFORM GET-NO-TRANS-ACCOUNT THRU LRA-EXIT
050110001220              PERFORM POPULATE-PLAN-VALUE-ROUTINE       THRU PPVR-EXIT
050120010604              IF CURR-LEVEL2         IS NOT EQUAL TO PREV-LEVEL2
050130010604                 MOVE WS-INVESTOR-INFO-TEMP TO  INVESTOR-INFO-SECTION
050140010604              END-IF
050150001220              MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION
050160001220           END-IF.
050170971223
050180971223       LS1-0390.
050190010620
050200000000      *****
050210000000      *****    Process the current page.
050220000000      *****
050230000000
050240000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
050250990922
050260010430           MOVE "N"             TO   WS-ACCOUNT-INV-SW.
050270010430           MOVE "N"             TO   WS-LR1-PROCESSED-SW.
050280010430
050290000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
050300990922               GO TO LS1-EXIT
050310990922           END-IF
050320990922
050330000000           CONTINUE.
050340000000
050350990922      **********
050360990922
050370000000       LS1-EXIT.
050380000000           EXIT.
050390990922
050400990922      **********
050410990922
050420000000      /
050430000000       PRINT-STMT-RTN.
050440000000
050450000000      ******************************************************************
050460000000      *                                                                *
050470000000      *    P R I N T   S T A T E M E N T   R O U T I N E               *
050480000000      *                                                                *
050490000000      ******************************************************************
050500000000
050510000000      *****
050520000000      *****    If the Statement Print Header record has not been
050530000000      *****    created, create it.  The record was formatted in the
050540000000      *****    LEVEL REINITIALIZATION 1 routine.
050550000000      *****
050560000000
050570000000           MOVE SPACES                 TO  WS-PSRERR-SW.
050580000000
050590990922           IF  WS-STMT-PRT-HDR-WSW     IS EQUAL TO  "Y"
050600990922               GO TO PSR-0010
050610990922           END-IF
050620990922
050630000000           WRITE STMT-PRT-HDR-RECD
050640000000               INVALID KEY
050650000000                   MOVE "H"            TO  WS-PSRERR-SW
050660000000                   GO TO PSR-9999.
050670000000
050680000000           ADD 1                       TO  WS-STMT-PRT-HDR-WCNT.
050690000000           MOVE "Y"                    TO  WS-STMT-PRT-HDR-WSW.
050700000000
050710000000       PSR-0010.
050720000000
050730000000      *****
050740000000      *****    Format and print the Statement Print Detail records.  One
050750000000      *****    record will be created for each statement line.  If the
050760000000      *****    statement line is spaces, no record will be created.
050770000000      *****
050780000000
050790000000           SET SPL                     TO  1.
050800000000
050810000000       PSR-0020.
050820990922
050830990922           IF  WS-STMTPGE-LNE (SPL)    IS EQUAL TO  SPACES
050840990922               GO TO PSR-0030
050850990922           END-IF
050860990922
050870000000           MOVE SPACES                 TO  STMT-PRT-DTL-RECD.
050880000000           MOVE INVESTOR-NO
050890000000                OF STMT-PRT-HDR-RECD   TO  INVESTOR-NO
050900000000                                           OF STMT-PRT-DTL-RECD.
050910000000           MOVE ACCOUNT
050920000000                OF STMT-PRT-HDR-RECD   TO  ACCOUNT
050930000000                                           OF STMT-PRT-DTL-RECD.
050940000000           MOVE LANGUAGE-CODE
050950000000                OF STMT-PRT-HDR-RECD   TO  LANGUAGE-CODE
050960000000                                           OF STMT-PRT-DTL-RECD.
050970000000           MOVE DEALER
050980000000                OF STMT-PRT-HDR-RECD   TO  DEALER
050990000000                                           OF STMT-PRT-DTL-RECD.
051000000000           MOVE DEALER-REP-CODE
051010000000                OF STMT-PRT-HDR-RECD   TO  DEALER-REP-CODE
051020000000                                           OF STMT-PRT-DTL-RECD.
051030000000           MOVE WS-SH-PGENO            TO  STMPGENO
051040000000                                           OF STMT-PRT-DTL-RECD.
051050000000           SET STMLNENO
051060000000               OF STMT-PRT-DTL-RECD    TO  SPL.
051070000000           MOVE WS-STMTPGE-LNE (SPL)   TO  STMLNE
051080000000                                           OF STMT-PRT-DTL-RECD.
051090980129           MOVE START-DATE
051100980129                OF STMT-PRT-HDR-RECD   TO  START-DATE
051110980129                                           OF STMT-PRT-DTL-RECD.
051120980129           MOVE END-DATE
051130980129                OF STMT-PRT-HDR-RECD   TO  END-DATE
051140980129                                           OF STMT-PRT-DTL-RECD.
051150980129
051160000000           WRITE STMT-PRT-DTL-RECD
051170000000               INVALID KEY
051180000000                   MOVE "D"            TO  WS-PSRERR-SW
051190000000                   GO TO PSR-9999.
051200000000
051210000000           ADD 1                       TO  WS-STMT-PRT-DTL-WCNT.
051220000000
051230000000       PSR-0030.
051240990922
051250000000           IF  SPL           IS LESS THAN  WS-SPL-MAX
051260000000               SET SPL              UP BY  1
051270990922               GO TO PSR-0020
051280990922           END-IF
051290990922
051300000000      *****
051310000000      *****    Format a new page for the account statement.
051320000000      *****
051330000000
051340000000           ADD 1                       TO  WS-SH-PGENO.
051350000000
051360990922           EVALUATE WS-STMTVRSN
051370001219             WHEN 9
051380991213               MOVE WS-SH-PGENO        TO  SH02V8-PGENO
051390990922             WHEN OTHER
051400990922               MOVE WS-SH-PGENO        TO  SH02-PGENO
051410990922           END-EVALUATE.
051420000000
051430000000       PSR-0040.
051440990927
051450001124           SET SPL                  TO  WS-SPL-SH02.
051460001124           MOVE STMT-HDG-02         TO  WS-STMTPGE-LNE (SPL).
051470991019
051480001219RF6007     IF WS-STMTVRSN    IS EQUAL TO  9
051490001124              MOVE SPACES             TO  WS-STMTPGE-DTL-V8
051500001124              GO TO PSR-0050
051510991213           END-IF
051520991213
051530000000           MOVE SPACES                 TO  WS-STMTPGE-DTL.
051540000000
051550000000       PSR-0050.
051560990922
051570000000           SET SPL                     TO  WS-SPL-DTLFST.
051580001205
051590001215001215     IF WS-STMT-SUMM-HDG-SW      IS EQUAL TO "Y"
051600001215              GO TO PSR-EXIT
051610001215           END-IF.
051620001215      ***************************************
051630001215001216     IF WS-TRANS-HEADING-SW      IS EQUAL TO "Y"
051640001215001216*       SET SPL                     UP BY  3
051650001215              MOVE WS-SHL01 (SHL)         TO  WS-STMTPGE-LNE (SPL)
051660001215              SET SPL                     UP BY  3
051670001215              MOVE WS-SHL02 (SHL)         TO  WS-STMTPGE-LNE (SPL)
051680001215              SET SPL                     UP BY  1
051690001215              MOVE WS-SHL03 (SHL)         TO  WS-STMTPGE-LNE (SPL)
051700001215              SET SPL                     UP BY  1
051710001215              MOVE WS-SHL04 (SHL)         TO  WS-STMTPGE-LNE (SPL)
051720001219001219        SET SPL                     UP BY  1
051730001215              MOVE "Y"                    TO WS-TRANS-HEADING-SW
051740001215           END-IF.
051750001205
051760000000           GO TO PSR-EXIT.
051770000000
051780000000       PSR-9999.
051790000000
051800000000      *****
051810000000      *****    An error was detected attempting to write either a
051820000000      *****    Statement Print Header record or a Statement Print Detail
051830000000      *****    record.  An error message will be printed and the program
051840000000      *****    terminated.
051850000000      *****
051860000000
051870000000           IF  WS-PSRERR-SW   IS EQUAL TO  "H"
051880000000               SET MSG                 TO  6
051890990922           ELSE
051900990922               SET MSG                 TO  7
051910990922           END-IF
051920990922
051930000000           MOVE SPACES                 TO  ERROR-LINE-1.
051940000000           MOVE INVESTOR-NO
051950000000                OF STMT-PRT-HDR-RECD   TO  EL1-INVESTOR-NO.
051960000000           MOVE ACCOUNT
051970000000                OF STMT-PRT-HDR-RECD   TO  EL1-ACCOUNT.
051980000000           MOVE LANGUAGE-CODE
051990000000                OF STMT-PRT-HDR-RECD   TO  EL1-LANGUAGE-CODE.
052000000000           MOVE DEALER
052010000000                OF STMT-PRT-HDR-RECD   TO  EL1-DEALER.
052020000000           MOVE DEALER-REP-CODE
052030000000                OF STMT-PRT-HDR-RECD   TO  EL1-DEALER-REP-CODE.
052040000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
052050000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
052060000000
052070000000           MOVE SPACES                 TO  ERROR-LINE-1.
052080990922
052090000000           IF  WS-PSRERR-SW   IS EQUAL TO  "H"
052100000000               MOVE STMT-PRT-HDR-RECD  TO  ERROR-LINE-1
052110990922           ELSE
052120990922               MOVE STMT-PRT-DTL-RECD  TO  ERROR-LINE-1
052130990922           END-IF
052140990922
052150000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
052160000000
052170000000           MOVE SPACES                 TO  ERROR-LINE-1.
052180000000           MOVE WS-FILSTAT             TO  ERROR-LINE-1.
052190000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
052200000000
052210000000           PERFORM DUMP-ROUTINE       THRU DR-EXIT.
052220000000           MOVE HIGH-VALUES            TO  CURR-CONTROLS.
052230000000
052240990922      **********
052250990922
052260000000       PSR-EXIT.
052270000000           EXIT.
052280990922
052290990922      **********
052300990922
052310000000      /
052320000000       DUMP-ROUTINE.
052330000000
052340000000      ******************************************************************
052350000000      *                                                                *
052360000000      *    D U M P   R O U T I N E                                     *
052370000000      *                                                                *
052380000000      ******************************************************************
052390000000
052400000000      *****
052410000000      *****    Produce a program dump for analysis.
052420000000      *****
052430000000
052440000000           CALL    "QLREXHAN"       USING  DUMP-ARGUMENTS.
052450000000
052460990922      **********
052470990922
052480000000       DR-EXIT.
052490000000           EXIT.
052500990922
052510990922      **********
052520990922
052530001128      /
052540001129       POPULATE-PLAN-VALUE-ROUTINE.
052550001128
052560001128      ******************************************************************
052570001128      *                                                                *
052580001129      *    C R E A T E   P L A N   V A L U E    L I N E S              *
052590001128      *                                                                *
052600001128      ******************************************************************
052610001128
052620001129
052630001130      ***
052640001130      ***   Format statement plan summary heading line 1 to 4.
052650001130      ***
052660001130
052670001214001215     SET SPL                     TO WS-SPL-DTLFST.
052680001129           MOVE WS-SHLPL01 (SHL)         TO  WS-STMTPGE-LNE (SPL).
052690001129           SET SPL                     UP BY  3.
052700001129           MOVE WS-SHLPL02 (SHL)         TO  WS-STMTPGE-LNE (SPL).
052710001129           SET SPL                     UP BY  2.
052720001129           MOVE WS-SHLPL03 (SHL)         TO  WS-STMTPGE-LNE (SPL).
052730001129           SET SPL                     UP BY  1.
052740001130
052750001218001218     IF SHL                      IS EQUAL TO 1
052760001218             MOVE WS-MONTH-COL1            TO  WS-SHPL04-COL1-E
052770001218             MOVE WS-MONTH-COL2            TO  WS-SHPL04-COL2-E
052780001218           ELSE
052790001218             MOVE WS-MONTH-COL1-F          TO  WS-SHPL04-COL1-F
052800001218             MOVE WS-MONTH-COL2-F          TO  WS-SHPL04-COL2-F
052810001218           END-IF.
052820001218
052830001218
052840001129           MOVE WS-SHLPL04 (SHL)         TO  WS-STMTPGE-LNE (SPL).
052850001201           SET SPL                     UP BY  1.
052860001129
052870001130      ***
052880001130      ***  Format statement plan summary lines. One line for each fund.
052890001130      ***
052900001130
052910001201001201     SET AFP                     TO  1.
052920001130001120     SET SAF2                    TO  1.
052930001130
052940010421       PPVR-0010.
052950010421
052960010421           IF AFP                      IS GREATER THAN   WS-SAFP-LST
052970010421              INITIALIZE ACCOUNT-FUND-PERFORM-SECTION
052980010421              MOVE WS-ACCOUNT-FUND-PERFORM (AFP)
052990010421                                       TO ACCOUNT-FUND-PERFORM-SECTION
053000010421              GO TO PPVR-0040
053010010421           END-IF.
053020010421
053030010421           INITIALIZE ACCOUNT-FUND-PERFORM-SECTION.
053040010421           MOVE WS-ACCOUNT-FUND-PERFORM (AFP)
053050010421                                       TO ACCOUNT-FUND-PERFORM-SECTION.
053060010421       PPVR-0020.
053070010421
053080010421           IF SAF2                     IS GREATER THAN   WS-SAF2-LST
053090010421              SET SAF2                 TO 1
053100010421           END-IF.
053110010421
053120010421           INITIALIZE ACCOUNT-DETAILS-SECTION.
053130010421           MOVE WS-ACCOUNT-DETAILS-SECTION (SAF2)
053140010421                                       TO ACCOUNT-DETAILS-SECTION.
053150010421           IF AFP-FUND-NO OF ACCOUNT-FUND-PERFORM-SECTION  IS NOT EQUAL
053160010421                          TO AD-FUND-NO OF ACCOUNT-DETAILS-SECTION
053170010421              SET SAF2    UP BY 1
053180010421              GO TO PPVR-0020
053190010421           END-IF.
053200010421
053210001130       PPVR-0030.
053220001130
053230001201      ***  Format statement plan summary line 1.
053240001201
053250001130           INITIALIZE  STMT-PLS-01.
053260001130
053270001219           IF  WS-STMTVRSN                   IS NOT EQUAL TO  9
053280001130               GO TO PPVR-0040
053290001130           END-IF
053300001130
053310001130           MOVE AFP-INVESTMENT-NAME    TO SP01-INVESTMENT-DESC.
053320001130           MOVE AD-MARKET-VALUE-PRIOR  TO SP01-JANROR.
053330001130           MOVE AD-MARKET-VALUE        TO SP01-JUNROR.
053340001219
053350010501RF9415     ADD AD-MARKET-VALUE         TO  WS-AD-MARKET-VALUE-TOT.
053360010501RF9415     ADD AD-MARKET-VALUE-PRIOR TO  WS-AD-MARKET-VALUE-PRIOR-TOT.
053370010501
053380051110      * RFS30210 - Begin
053390051110           SET SW-PERIOD-TO-CHECK-6M     TO TRUE
053400051111           PERFORM SEARCH-CURR-PERIOD-AFP
053410051110           IF SW-PERIOD-ROR-APPLICABLE-NO
053420051110             MOVE WS-NOT-AVAILABLE-LIT   TO SP01-YTD-6MO-LIT
053430051110             MOVE SPACES                 TO SP01-YTD-6MO-SPACE
053440051110             MOVE SPACES                 TO SP01-YTD-6MOPERCENT
053450051110           ELSE
053460051110             COMPUTE SP01-YTD-6MO ROUNDED  = WS-CURR-PERIOD-ROR * 1
053470051110             MOVE WS-CURR-PERIOD-ROR-SIGN  TO SP01-YTD-6MO-SIGN
053480051110             MOVE WS-PERCENTAGE-LIT        TO SP01-YTD-6MOPERCENT
053490051110           END-IF
053500051110      * -----------------------------------------------------------
053510051110      *
053520051110001219*    IF AFP-PERIOD-ROR-APPLICABLE-2 IS EQUAL TO "N"
053530051110      *       MOVE WS-NOT-AVAILABLE-LIT   TO SP01-YTD-6MO-LIT
053540051110      *       MOVE SPACES                 TO SP01-YTD-6MO-SPACE
053550051110      *       MOVE SPACES                 TO SP01-YTD-6MOPERCENT
053560051110      *    ELSE
053570051110      *       COMPUTE SP01-YTD-6MO ROUNDED  = AFP-PERIOD-ROR-2
053580051110      *                                     * 1
053590051110      *       MOVE AFP-PERIOD-ROR-SIGN-2  TO SP01-YTD-6MO-SIGN
053600051110      *       MOVE WS-PERCENTAGE-LIT      TO SP01-YTD-6MOPERCENT
053610051110      *    END-IF.
053620051110      * -----------------------------------------------------------
053630001219
053640051110           SET SW-PERIOD-TO-CHECK-1Y     TO TRUE
053650051111           PERFORM SEARCH-CURR-PERIOD-AFP
053660051110           IF SW-PERIOD-ROR-APPLICABLE-NO
053670051110             MOVE WS-NOT-AVAILABLE-LIT   TO SP01-12MO-LIT
053680051110             MOVE SPACES                 TO SP01-12MO-SPACE
053690051110             MOVE SPACES                 TO SP01-YTD-12MOPERCENT
053700051110           ELSE
053710051110             COMPUTE SP01-12MO    ROUNDED  = WS-CURR-PERIOD-ROR * 1
053720051110             MOVE WS-CURR-PERIOD-ROR-SIGN  TO SP01-12MO-SIGN
053730051110             MOVE WS-PERCENTAGE-LIT        TO SP01-YTD-12MOPERCENT
053740051110           END-IF
053750051110      * RFS30210 - Begin
053760051110      * ---------------------------------------------------------------
053770051110001211*    IF AFP-PERIOD-ROR-APPLICABLE-6 IS EQUAL TO "N"
053780051110      *       MOVE WS-NOT-AVAILABLE-LIT   TO SP01-12MO-LIT
053790051110      *       MOVE SPACES                 TO SP01-12MO-SPACE
053800051110      *       MOVE SPACES                 TO SP01-YTD-12MOPERCENT
053810051110      *    ELSE
053820051110      *       COMPUTE SP01-12MO ROUNDED  = AFP-PERIOD-ROR-6
053830051110      *                                  * 1
053840051110      *       MOVE AFP-PERIOD-ROR-SIGN-6  TO SP01-12MO-SIGN
053850051110      *       MOVE WS-PERCENTAGE-LIT      TO SP01-YTD-12MOPERCENT
053860051110      *    END-IF.
053870051110      * ---------------------------------------------------------------
053880001211
053890051110           SET SW-PERIOD-TO-CHECK-INC    TO TRUE
053900051111           PERFORM SEARCH-CURR-PERIOD-AFP
053910051110           COMPUTE SP01-SINCE-ISSUE ROUNDED  = WS-CURR-PERIOD-ROR * 1
053920051110           MOVE WS-CURR-PERIOD-ROR-SIGN  TO SP01-SINCE-ISSUE-SIGN
053930051110           MOVE WS-PERCENTAGE-LIT        TO SP01-SINCE-ISSUEPERCENT
053940051110      * ---------------------------------------------------------------
053950051110      *    COMPUTE SP01-SINCE-ISSUE ROUNDED  = AFP-PERIOD-ROR-4
053960051110      *                                      * 1.
053970051110      *    MOVE AFP-PERIOD-ROR-SIGN-4  TO SP01-SINCE-ISSUE-SIGN.
053980051110      *    MOVE WS-PERCENTAGE-LIT      TO SP01-SINCE-ISSUEPERCENT.
053990051110      * ---------------------------------------------------------------
054000051110      * RFS30210 - End
054010001130
054020001201           SET SPL                     UP BY  1.
054030021018
054040021018R14329     SET WS-SPL-DTLTST           TO  SPL.
054050021018
054060021018           IF  (WS-SPL-DTLTST           +  5)
054070021018                      IS NOT GREATER THAN  WS-SPL-DTLLST
054080021018               GO TO PPVR-0035
054090021018           END-IF
054100021018
054110021018           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
054120021018
054130021018           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
054140021018               GO TO PPVR-EXIT
054150021018           END-IF.
054160021021
054170021021      ***
054180021021      ***   Format statement plan summary heading line 1 to 4
054190021021      ***   for the next page if the number of Funds can not be
054200021021      ***   reported on one page.
054210021021      ***
054220021021
054230021021           SET SPL                     TO WS-SPL-DTLFST.
054240021021           MOVE WS-SHLPL01 (SHL)         TO  WS-STMTPGE-LNE (SPL).
054250021021           SET SPL                     UP BY  3.
054260021021           MOVE WS-SHLPL02 (SHL)         TO  WS-STMTPGE-LNE (SPL).
054270021021           SET SPL                     UP BY  2.
054280021021           MOVE WS-SHLPL03 (SHL)         TO  WS-STMTPGE-LNE (SPL).
054290021021           SET SPL                     UP BY  1.
054300021021
054310021021           IF SHL                      IS EQUAL TO 1
054320021021             MOVE WS-MONTH-COL1            TO  WS-SHPL04-COL1-E
054330021021             MOVE WS-MONTH-COL2            TO  WS-SHPL04-COL2-E
054340021021           ELSE
054350021021             MOVE WS-MONTH-COL1-F          TO  WS-SHPL04-COL1-F
054360021021             MOVE WS-MONTH-COL2-F          TO  WS-SHPL04-COL2-F
054370021021           END-IF.
054380021021
054390021021           MOVE WS-SHLPL04 (SHL)         TO  WS-STMTPGE-LNE (SPL).
054400021021           SET SPL                     UP BY  2.
054410021021
054420021018R14329 PPVR-0035.
054430021018
054440001130           MOVE STMT-PLS-01            TO  WS-STMTPGE-LNE (SPL).
054450001130
054460001130           SET AFP                     UP BY  1.
054470010421      *    SET SAF2                    UP BY  1.
054480001201           GO TO PPVR-0010.
054490001130
054500001130       PPVR-0040.
054510001130
054520001201      ***  Format statement plan summary line 2.
054530001201
054540030626R17816     SET WS-SPL-DTLTST           TO  SPL.
054550030626  ¦
054560030626  ¦        IF  (WS-SPL-DTLTST           +  5)
054570030626  ¦                   IS NOT GREATER THAN  WS-SPL-DTLLST
054580030626  ¦            GO TO PPVR-0035-0010
054590030626  ¦        END-IF
054600030626  ¦
054610030626  ¦        PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
054620030626  ¦
054630030626  ¦        IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
054640030626  ¦            GO TO PPVR-EXIT
054650030626  ¦        END-IF.
054660030626  ¦
054670030626  V
054680030626R17816 PPVR-0035-0010.
054690030626
054700001201           INITIALIZE STMT-PLS-02.
054710001128
054720001219           IF WS-STMTVRSN                    IS  EQUAL TO  9
054730001128              MOVE "----------"              TO  SP02V8-JANROR-DOT
054740001128              MOVE "----------"              TO  SP02V8-JUNROR-DOT
054750001128           ELSE
054760001128              MOVE "----------"              TO  SP02-JANROR-DOT
054770001128              MOVE "----------"              TO  SP02-JUNROR-DOT
054780001128           END-IF.
054790001128
054800001201            SET SPL                  UP BY  1.
054810001128            MOVE STMT-PLS-02         TO  WS-STMTPGE-LNE (SPL).
054820001128
054830010313      *PPVR-0040.
054840001201
054850001201      ***  Format statement plan summary line 3.
054860001201
054870001201           INITIALIZE STMT-PLS-03.
054880001128
054890001219           IF WS-STMTVRSN                    IS  EQUAL TO  9
054900001214              MOVE WS-SPSS2L08 (SPS2)        TO  SP03V8-TOTAL-PLAN
054910001130              MOVE WS-AD-MARKET-VALUE-PRIOR-TOT  TO  SP03V8-JANROR-TOT
054920001130              MOVE WS-AD-MARKET-VALUE-TOT    TO  SP03V8-JUNROR-TOT
054930001219
054940051110      * RFS30210 - Begin
054950051110              SET SW-PERIOD-TO-CHECK-6M     TO TRUE
054960051111              PERFORM SEARCH-CURR-PERIOD-AP
054970051110              IF SW-PERIOD-ROR-APPLICABLE-NO
054980051110                MOVE WS-NOT-AVAILABLE-LIT   TO SP03V8-YTD-6MOTOT-LIT
054990051110                MOVE SPACES                 TO SP03V8-YTD-6MOTOT-SPACE
055000051110                MOVE SPACES                 TO SP03V8-YTD-6MOPERCENT
055010051110              ELSE
055020051110                COMPUTE SP03V8-YTD-6MOTOT ROUNDED =
055030051110                                                WS-CURR-PERIOD-ROR * 1
055040051110                MOVE WS-CURR-PERIOD-ROR-SIGN  TO SP03V8-YTD-6MOTOT-SIGN
055050051110                MOVE WS-PERCENTAGE-LIT        TO SP03V8-YTD-6MOPERCENT
055060051110              END-IF
055070051110      * ---------------------------------------------------------------
055080051110001219*       IF AP-PERIOD-ROR-APPLICABLE-2 IS EQUAL TO "N"
055090051110      *          MOVE WS-NOT-AVAILABLE-LIT   TO SP03V8-YTD-6MOTOT-LIT
055100051110      *          MOVE SPACES                 TO SP03V8-YTD-6MOTOT-SPACE
055110051110      *          MOVE SPACES                 TO SP03V8-YTD-6MOPERCENT
055120051110      *       ELSE
055130051110      *          COMPUTE SP03V8-YTD-6MOTOT ROUNDED  = AP-PERIOD-ROR-2
055140051110      *                                             * 1
055150051110      *          MOVE AP-PERIOD-ROR-SIGN-2   TO  SP03V8-YTD-6MOTOT-SIGN
055160051110      *          MOVE WS-PERCENTAGE-LIT      TO  SP03V8-YTD-6MOPERCENT
055170051110      *       END-IF
055180051110      * ---------------------------------------------------------------
055190001219
055200051110              SET SW-PERIOD-TO-CHECK-1Y     TO TRUE
055210051111              PERFORM SEARCH-CURR-PERIOD-AP
055220051110              IF SW-PERIOD-ROR-APPLICABLE-NO
055230051110                 MOVE WS-NOT-AVAILABLE-LIT TO SP03V8-12MOTOT-LIT
055240051110                 MOVE SPACES               TO SP03V8-12MOTOT-SPACE
055250051110                 MOVE SPACES               TO SP03V8-YTD-12MOTOTPERCENT
055260051110              ELSE
055270051110                COMPUTE SP03V8-12MOTOT    ROUNDED =
055280051110                                                WS-CURR-PERIOD-ROR * 1
055290051110                MOVE WS-CURR-PERIOD-ROR-SIGN TO SP03V8-12MOTOT-SIGN
055300051110                MOVE WS-PERCENTAGE-LIT       TO
055310051110                                              SP03V8-YTD-12MOTOTPERCENT
055320051110              END-IF
055330051110      * ---------------------------------------------------------------
055340051110001211*       IF AP-PERIOD-ROR-APPLICABLE-6     IS EQUAL TO "N"
055350051110      *          MOVE WS-NOT-AVAILABLE-LIT      TO SP03V8-12MOTOT-LIT
055360051110      *          MOVE SPACES                    TO SP03V8-12MOTOT-SPACE
055370051110      *          MOVE SPACES               TO SP03V8-YTD-12MOTOTPERCENT
055380051110      *       ELSE
055390051110      *         COMPUTE SP03V8-12MOTOT  ROUNDED  = AP-PERIOD-ROR-6
055400051110      *                                          * 1
055410051110      *          MOVE AP-PERIOD-ROR-SIGN-6  TO SP03V8-12MOTOT-SIGN
055420051110      *          MOVE WS-PERCENTAGE-LIT    TO SP03V8-YTD-12MOTOTPERCENT
055430051110      *       END-IF
055440051110      * ---------------------------------------------------------------
055450051110              SET SW-PERIOD-TO-CHECK-INC    TO TRUE
055460051111              PERFORM SEARCH-CURR-PERIOD-AP
055470051110              COMPUTE SP03V8-SINCE-ISSUETOT ROUNDED =
055480051110                                              WS-CURR-PERIOD-ROR * 1
055490051110              MOVE WS-CURR-PERIOD-ROR-SIGN TO
055500051110                                            SP03V8-SINCE-ISSUETOT-SIGN
055510051110              MOVE WS-PERCENTAGE-LIT       TO
055520051110                                            SP03V8-SINCE-ISSUEPERCENT
055530051110      * ---------------------------------------------------------------
055540051110      *       COMPUTE SP03V8-SINCE-ISSUETOT ROUNDED  = AP-PERIOD-ROR-4
055550051110      *                                              * 1
055560051110      *       MOVE AP-PERIOD-ROR-SIGN-4  TO  SP03V8-SINCE-ISSUETOT-SIGN
055570051110      *       MOVE WS-PERCENTAGE-LIT     TO SP03V8-SINCE-ISSUEPERCENT
055580051110      * ---------------------------------------------------------------
055590051110      * RFS30210 - End
055600001128           ELSE
055610001214              MOVE WS-SPSS2L08 (SPS2)        TO  SP03-TOTAL-PLAN
055620001130              MOVE WS-AD-MARKET-VALUE-PRIOR-TOT    TO  SP03-JANROR-TOT
055630001130              MOVE WS-AD-MARKET-VALUE-TOT    TO  SP03-JUNROR-TOT
055640001219
055650051110      * RFS30210 - Begin
055660051110              SET SW-PERIOD-TO-CHECK-6M     TO TRUE
055670051111              PERFORM SEARCH-CURR-PERIOD-AP
055680051110              IF SW-PERIOD-ROR-APPLICABLE-NO
055690051110                MOVE WS-NOT-AVAILABLE-LIT   TO SP03-YTD-6MOTOT-LIT
055700051110                MOVE SPACES                 TO SP03-YTD-6MOTOT-SPACE
055710051110                MOVE SPACES                 TO SP03-YTD-6MOPERCENT
055720051110              ELSE
055730051110                COMPUTE SP03-YTD-6MOTOT ROUNDED =
055740051110                                                WS-CURR-PERIOD-ROR * 1
055750051110                MOVE WS-CURR-PERIOD-ROR-SIGN  TO SP03-YTD-6MOTOT-SIGN
055760051110                MOVE WS-PERCENTAGE-LIT        TO SP03-YTD-6MOPERCENT
055770051110              END-IF
055780051110      * ---------------------------------------------------------------
055790051110001219*       IF AFP-PERIOD-ROR-APPLICABLE-2 IS EQUAL TO "N"
055800051110      *          MOVE WS-NOT-AVAILABLE-LIT   TO SP03-YTD-6MOTOT-LIT
055810051110      *          MOVE SPACES                 TO SP03-YTD-6MOTOT-SPACE
055820051110      *          MOVE SPACES                 TO SP03-YTD-6MOPERCENT
055830051110      *       ELSE
055840051110      *          COMPUTE SP03-YTD-6MOTOT ROUNDED  = AP-PERIOD-ROR-2
055850051110      *                                           * 1
055860051110      *          MOVE AP-PERIOD-ROR-SIGN-2    TO  SP03-YTD-6MOTOT-SIGN
055870051110      *          MOVE WS-PERCENTAGE-LIT       TO  SP03-YTD-6MOPERCENT
055880051110      *       END-IF
055890051110      * ---------------------------------------------------------------
055900001219
055910051110              SET SW-PERIOD-TO-CHECK-1Y     TO TRUE
055920051111              PERFORM SEARCH-CURR-PERIOD-AP
055930051110              IF SW-PERIOD-ROR-APPLICABLE-NO
055940051110                MOVE WS-NOT-AVAILABLE-LIT TO SP03-12MOTOT-LIT
055950051110                MOVE SPACES               TO SP03-12MOTOT-SPACE
055960051110                MOVE SPACES               TO SP03-YTD-12MOTOTPERCENT
055970051110              ELSE
055980051110                COMPUTE SP03-12MOTOT    ROUNDED =
055990051110                                                WS-CURR-PERIOD-ROR * 1
056000051110                MOVE WS-CURR-PERIOD-ROR-SIGN TO SP03-12MOTOT-SIGN
056010051110                MOVE WS-PERCENTAGE-LIT       TO SP03-YTD-12MOTOTPERCENT
056020051110              END-IF
056030051110      * ---------------------------------------------------------------
056040051110001211*       IF AP-PERIOD-ROR-APPLICABLE-6     IS EQUAL TO "N"
056050051110      *          MOVE WS-NOT-AVAILABLE-LIT      TO SP03-12MOTOT-LIT
056060051110      *          MOVE SPACES                    TO SP03-12MOTOT-SPACE
056070051110      *          MOVE SPACES               TO SP03-YTD-12MOTOTPERCENT
056080051110      *       ELSE
056090051110      *          COMPUTE SP03-12MOTOT  ROUNDED  = AP-PERIOD-ROR-6
056100051110      *                                         * 1
056110051110      *          MOVE AP-PERIOD-ROR-SIGN-6      TO SP03-12MOTOT-SIGN
056120051110      *          MOVE WS-PERCENTAGE-LIT    TO SP03-YTD-12MOTOTPERCENT
056130051110      *       END-IF
056140051110      * ---------------------------------------------------------------
056150001211
056160051110              SET SW-PERIOD-TO-CHECK-INC    TO TRUE
056170051111              PERFORM SEARCH-CURR-PERIOD-AP
056180051110              COMPUTE SP03-SINCE-ISSUETOT ROUNDED =
056190051110                                              WS-CURR-PERIOD-ROR * 1
056200051110              MOVE WS-CURR-PERIOD-ROR-SIGN TO SP03-SINCE-ISSUETOT-SIGN
056210051110              MOVE WS-PERCENTAGE-LIT       TO SP03-SINCE-ISSUEPERCENT
056220051110      * ---------------------------------------------------------------
056230051110      *       COMPUTE SP03-SINCE-ISSUETOT ROUNDED  = AP-PERIOD-ROR-4
056240051110      *                                              * 1
056250051110      *       MOVE AP-PERIOD-ROR-SIGN-4  TO SP03-SINCE-ISSUETOT-SIGN
056260051110      *       MOVE WS-PERCENTAGE-LIT     TO SP03-SINCE-ISSUEPERCENT
056270051110      * ---------------------------------------------------------------
056280001128           END-IF.
056290001128
056300001201            SET SPL                  UP BY  1.
056310001128            MOVE STMT-PLS-03         TO  WS-STMTPGE-LNE (SPL).
056320001128
056330001204      ***  Format statement plan summary hardcoded message lines.
056340001205      ***  Three lines were reported. Two more line were reserverd.
056350001204
056360001205       PPVR-0040-0010.
056370001204
056380001205           SET SPL                  UP BY  3.
056390030626
056400030626R17816     SET WS-SPL-DTLTST           TO  SPL.
056410030626  ¦
056420030626  ¦        IF  (WS-SPL-DTLTST           +  5)
056430030626  ¦                   IS NOT GREATER THAN  WS-SPL-DTLLST
056440030626  ¦            GO TO PPVR-0040-0020
056450030626  ¦        END-IF
056460030626  ¦
056470030626  ¦        PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
056480030626  ¦
056490030626  ¦        IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
056500030626  ¦            GO TO PPVR-EXIT
056510030626  ¦        END-IF.
056520030626  V
056530030626R17816 PPVR-0040-0020.
056540030626
056550001204           MOVE WS-SPSML01 (SPSM)         TO  WS-STMTPGE-LNE (SPL).
056560001204           SET SPL                     UP BY  1.
056570001204           MOVE WS-SPSML02 (SPSM)         TO  WS-STMTPGE-LNE (SPL).
056580001204           SET SPL                     UP BY  1.
056590001204           MOVE WS-SPSML03 (SPSM)         TO  WS-STMTPGE-LNE (SPL).
056600001204           SET SPL                     UP BY  1.
056610001214           MOVE WS-SPSML04 (SPSM)         TO  WS-STMTPGE-LNE (SPL).
056620001214           SET SPL                     UP BY  1.
056630010910R11175     MOVE WS-SPSML05 (SPSM)         TO  WS-STMTPGE-LNE (SPL).
056640010910           SET SPL                     UP BY  1.
056650001204
056660001201       PPVR-0050.
056670001201
056680001201      ***  Format statement plan summary section2 heading line.
056690001201
056700001218001218     SET WS-SPL-DTLTST           TO  SPL.
056710001218
056720010912R11175***
056730010912      ***  If current page can not accomodate the whole section,
056740010912      ***  the section will be moved to a new page.
056750010912      ***
056760010912
056770010912R11175*    IF  (WS-SPL-DTLTST           +  10)
056780010912R11175     IF  (WS-SPL-DTLTST           +  13)
056790001218                      IS NOT GREATER THAN  WS-SPL-DTLLST
056800001218               GO TO PPVR-0050-0010
056810001218           END-IF
056820001218
056830001218           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
056840001218
056850001218           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
056860001218               GO TO PPVR-EXIT
056870001218           END-IF.
056880001218
056890001218      ***********************************************************
056900001218
056910001218       PPVR-0050-0010.
056920001218
056930001207           MOVE WS-SPSS2L01 (SPS2)       TO  SPCH-DESCRIPTION1.
056940001218           IF SPS2                   IS EQUAL TO 1
056950001218             MOVE WS-MONTH-COL1            TO  SPCH-COL2-1
056960001218             MOVE WS-MONTH-COL2            TO  SPCH-COL2-2
056970001218           ELSE
056980001218             MOVE WS-MONTH-COL1-F          TO  SPCH-COL2-1
056990001218             MOVE WS-MONTH-COL2-F          TO  SPCH-COL2-2
057000001218           END-IF
057010001218
057020001207           MOVE WS-SPSS2L02 (SPS2)       TO  SPCH-COL3.
057030001207
057040001201           SET SPL                   UP BY  3.
057050001207           MOVE STMT-PLS-COLHD           TO  WS-STMTPGE-LNE (SPL).
057060001228
057070001201       PPVR-0060.
057080001201
057090001228
057100001201      ***  Format statement plan summary line 4.
057110001201
057120001201           INITIALIZE STMT-PLS-04.
057130001201
057140001207      *    MOVE "Acuumulated Value of your plan:" TO SP04-DESCRIPTION1.
057150001207           MOVE WS-SPSS2L03 (SPS2)                TO SP04-DESCRIPTION1.
057160010912
057170010912R11175     IF SPS2                          IS EQUAL TO  1
057180010912             MOVE WS-MONTH-COL6             TO SP04-MONTH-COL1
057190010912           ELSE
057200010912             MOVE WS-MONTH-COL6-F           TO SP04-MONTH-COL1
057210010912           END-IF
057220010912
057230001201           MOVE WS-AD-MARKET-VALUE-PRIOR-TOT    TO  SP04-ACCUM-TOT.
057240010912R11175*    MOVE ZEROS                      TO  SP04-ACCUM-ISSUE-TOT.
057250010912R11175     MOVE SPACES                     TO  SP04-ACCUM-ISSUE-TOT.
057260001201
057270001201           SET SPL                   UP BY  2.
057280001201           MOVE STMT-PLS-04         TO  WS-STMTPGE-LNE (SPL).
057290001201
057300001201       PPVR-0070.
057310001201
057320001201      ***  Format statement plan summary line 5.
057330001201
057340001201           INITIALIZE STMT-PLS-05.
057350001201
057360001219      *    MOVE "Total Deposits:         " TO SP05-DESCRIPTION1.
057370001219           MOVE WS-SPSS2L04 (SPS2)         TO SP05-DESCRIPTION1.
057380001219001219     MOVE WS-AD-DEPOSIT1-TOT         TO  SP05-DEPOSIT-TOT.
057390001219           MOVE WS-AD-DEPOSIT2-TOT         TO  SP05-DEPOSIT-TOT-ISSUE.
057400001201
057410001201           SET SPL                   UP BY  1.
057420001201           MOVE STMT-PLS-05         TO  WS-STMTPGE-LNE (SPL).
057430001201
057440001201       PPVR-0080.
057450001201
057460001201      ***  Format statement plan summary line 6.
057470001201
057480001201           INITIALIZE STMT-PLS-06.
057490001201
057500001219      *    MOVE "Total Withdrawals:      " TO SP06-DESCRIPTION1.
057510001219           MOVE WS-SPSS2L05 (SPS2)         TO SP06-DESCRIPTION1.
057520001219           MOVE WS-AD-WITHDRAWALS1-TOT     TO SP06-WITHDRA-TOT.
057530001219           MOVE WS-AD-WITHDRAWALS2-TOT     TO SP06-WITHDRA-TOT-ISSUE.
057540001201
057550001201           SET SPL                   UP BY  1.
057560001201           MOVE STMT-PLS-06         TO  WS-STMTPGE-LNE (SPL).
057570001201
057580001201       PPVR-0090.
057590001201
057600001201      ***  Format statement plan summary line 7.
057610001201
057620001201           SET SPL                   UP BY  1.
057630001201           MOVE STMT-PLS-07         TO  WS-STMTPGE-LNE (SPL).
057640001201
057650001201       PPVR-0100.
057660001201
057670001201      ***  Format statement plan summary line 8.
057680001201
057690001201           INITIALIZE STMT-PLS-08.
057700001201
057710001219001219     COMPUTE WS-AD-DEP-MINUS-WITHD1    = WS-AD-DEPOSIT1-TOT
057720001219                                             - WS-AD-WITHDRAWALS1-TOT.
057730001219001219     COMPUTE WS-AD-DEP-MINUS-WITHD2    = WS-AD-DEPOSIT2-TOT
057740001219                                             - WS-AD-WITHDRAWALS2-TOT.
057750001219
057760001219      *    MOVE "Deposit minus withdrawals:  " TO SP08-DESCRIPTION1.
057770001219           MOVE WS-SPSS2L06 (SPS2)             TO SP08-DESCRIPTION1.
057780001219           MOVE WS-AD-DEP-MINUS-WITHD1         TO  SP08-DEP-WITHDRA.
057790001228
057800001228           IF WS-AD-DEP-MINUS-WITHD1           IS LESS THAN ZEROS
057810001228              MOVE "-"                TO   SP08-DEP-WITHDRA-SIGN
057820001228           END-IF
057830001228
057840001219           MOVE WS-AD-DEP-MINUS-WITHD2     TO  SP08-DEP-WITHDRA-ISSUE.
057850001228
057860001228           IF WS-AD-DEP-MINUS-WITHD2           IS LESS THAN ZEROS
057870001228              MOVE "-"                TO   SP08-DEP-WITHDRA-ISSUE-SIGN
057880001228           END-IF
057890001201
057900001201           SET SPL                   UP BY  1.
057910001201           MOVE STMT-PLS-08         TO  WS-STMTPGE-LNE (SPL).
057920001201
057930010912R11175***
057940010912      ***  An extra line was added between previous line 8 and 9.
057950010912      ***
057960010912
057970010912       PPVR-0105.
057980010912
057990010912      ***  Format statement plan summary line 9.
058000010912
058010010912           INITIALIZE STMT-PLS-09.
058020010912
058030010912           COMPUTE WS-INCREASE-DECREASE-1 = WS-AD-MARKET-VALUE-TOT
058040010912                                          - WS-AD-DEP-MINUS-WITHD1
058050010912                                        - WS-AD-MARKET-VALUE-PRIOR-TOT.
058060010912           COMPUTE WS-INCREASE-DECREASE-2 = WS-AD-MARKET-VALUE-TOT
058070010912                                          - WS-AD-DEP-MINUS-WITHD2.
058080010912
058090010913      *    MOVE "Increase/Decrease in Accumulated Value of your plan:"
058100020613           MOVE WS-SPSS2L09 (SPS2)         TO SP09-DESCRIPTION1.
058110010912
058120010912           MOVE WS-INCREASE-DECREASE-1     TO SP09-INCREASE-DECREASE1.
058130010912
058140010912           IF WS-INCREASE-DECREASE-1       IS LESS THAN ZEROS
058150010912              MOVE "-"                     TO SP09-INCREASE-DECREASE1S
058160010912           END-IF.
058170010912
058180010912           MOVE WS-INCREASE-DECREASE-2     TO SP09-INCREASE-DECREASE2.
058190010912
058200010912           IF WS-INCREASE-DECREASE-2       IS LESS THAN ZEROS
058210010912              MOVE "-"                     TO SP09-INCREASE-DECREASE2S
058220010912           END-IF.
058230010912
058240010912           SET SPL                   UP BY  2.
058250010912           MOVE STMT-PLS-09         TO  WS-STMTPGE-LNE (SPL).
058260010912
058270001201       PPVR-0110.
058280001201
058290010912r11175***  Format statement plan summary line 10.
058300001201
058310010912           INITIALIZE STMT-PLS-10.
058320001201
058330001207      *    MOVE "Accumulated Value of your plan on: "
058340001207      *                                   TO  SP09-DESCRIPTION1.
058350010912           MOVE WS-SPSS2L07 (SPS2)                TO SP10-DESCRIPTION1.
058360001218           IF SPS2                        IS EQUAL TO  1
058370010912             MOVE WS-MONTH-COL3             TO  SP10-MONTH-COL3
058380001218           ELSE
058390010912             MOVE WS-MONTH-COL3-F           TO  SP10-MONTH-COL3
058400001218           END-IF
058410001218
058420010912           MOVE WS-AD-MARKET-VALUE-TOT    TO  SP10-TOTAL-VALUE1.
058430010912           MOVE WS-AD-MARKET-VALUE-TOT    TO  SP10-TOTAL-VALUE2.
058440001201
058450001201           SET SPL                   UP BY  2.
058460010912           MOVE STMT-PLS-10         TO  WS-STMTPGE-LNE (SPL).
058470001205           SET SPL                   UP BY  2.
058480001201
058490001215      *************************************************************
058500001215001215*****
058510001215      *****    Process the current page.
058520001215      *****
058530001215
058540061027001215     MOVE "Y"                   TO  WS-STMT-SUMM-HDG-SW.
058550001215
058560001215           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
058570061027001216     MOVE "N"                   TO  WS-STMT-SUMM-HDG-SW.
058580001215
058590001215           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
058600001215               GO TO PPVR-EXIT
058610001215           END-IF.
058620001215
058630001215      *************************************************************
058640001215
058650001203      *****
058660001203      *****    Format statement summary lines.
058670001203      *****
058680001203
058690001203       PPVR-0200.
058700001203
058710001203      *****
058720001203      *****    Format statement summary line 1.
058730001203      *****
058740001203
058750001203           MOVE SPACES                 TO  STMT-SUM-01.
058760001203
058770001203           EVALUATE WS-STMTVRSN
058780001219             WHEN 9
058790001203               MOVE WS-SS01V8-LIT1 (SL) TO  SS01V8-LIT1
058800001203             WHEN OTHER
058810001203               MOVE WS-SS01-LIT1 (SL)   TO  SS01-LIT1
058820001203           END-EVALUATE.
058830001203
058840001203       PPVR-0200-0010.
058850001203
058860001203           SET SPL                  UP BY  1.
058870001203           MOVE STMT-SUM-01         TO  WS-STMTPGE-LNE (SPL).
058880001203
058890001203      *****
058900001203      *****    Format statement summary line 2.
058910001203      *****
058920001203
058930001203           MOVE SPACES                 TO  STMT-SUM-02.
058940001203
058950001203       PPVR-0200-0020.
058960001203
058970001219           IF  WS-STMTVRSN   NOT EQUAL TO  9
058980001203               GO TO PPVR-0200-0030
058990001203           END-IF.
059000001203
059010001203           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
059020001203              MOVE WS-SS02-LIT0 (SL)      TO  SS02V8-LIT0
059030001203           END-IF.
059040001203
059050001203           MOVE WS-SS02-LIT1 (SL)      TO  SS02V8-LIT1.
059060001203           MOVE WS-SS02-LIT2 (SL)      TO  SS02V8-LIT2.
059070001203           MOVE WS-SS02-LIT3 (SL)      TO  SS02V8-LIT3.
059080001203           MOVE WS-SS02-LIT4 (SL)      TO  SS02V8-LIT4.
059090010912R11175     MOVE WS-SS02-LIT5 (SL)      TO  SS02V8-LIT5.
059100001203           GO TO PPVR-0200-0040.
059110001203
059120001203       PPVR-0200-0030.
059130001203
059140001203           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
059150001203              MOVE WS-SS02-LIT0 (SL)      TO  SS02-LIT0
059160001203           END-IF.
059170001203              MOVE WS-SS02-LIT1 (SL)      TO  SS02-LIT1.
059180001203              MOVE WS-SS02-LIT2 (SL)      TO  SS02-LIT2.
059190001203              MOVE WS-SS02-LIT3 (SL)      TO  SS02-LIT3.
059200001203              MOVE WS-SS02-LIT4 (SL)      TO  SS02-LIT4.
059210010912R11175        MOVE WS-SS02-LIT5 (SL)      TO  SS02-LIT5.
059220001203
059230001203       PPVR-0200-0040.
059240001203
059250001205           SET SPL                     UP BY  2.
059260001203
059270001203           MOVE STMT-SUM-02         TO  WS-STMTPGE-LNE (SPL).
059280001203
059290001203      *****
059300001203      *****    Format statement summary line 3.
059310001203      *****
059320001203
059330001203           MOVE SPACES                 TO  STMT-SUM-03.
059340001203
059350001203       PPVR-0200-0050.
059360001203
059370001219           IF  WS-STMTVRSN   NOT EQUAL TO  9
059380001203               GO TO PPVR-0200-0060
059390001203           END-IF.
059400001203
059410001203           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
059420001203              MOVE WS-SS03-LIT0 (SL)      TO  SS03V8-LIT0
059430001203           END-IF.
059440001203              MOVE WS-SS03-LIT1 (SL)      TO  SS03V8-LIT1.
059450001203              MOVE WS-SS03-LIT2 (SL)      TO  SS03V8-LIT2.
059460001203              MOVE WS-SS03-LIT3 (SL)      TO  SS03V8-LIT3.
059470001203              MOVE WS-SS03-LIT4 (SL)      TO  SS03V8-LIT4.
059480010912R11175        MOVE WS-SS03-LIT5 (SL)      TO  SS03V8-LIT5.
059490001203              GO TO PPVR-0200-0070.
059500001203
059510001203       PPVR-0200-0060.
059520001203
059530001203           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
059540001203              MOVE WS-SS03-LIT0 (SL)      TO  SS03-LIT0
059550001203           END-IF.
059560001203              MOVE WS-SS03-LIT1 (SL)      TO  SS03-LIT1.
059570001203              MOVE WS-SS03-LIT2 (SL)      TO  SS03-LIT2.
059580001203              MOVE WS-SS03-LIT3 (SL)      TO  SS03-LIT3.
059590001203              MOVE WS-SS03-LIT4 (SL)      TO  SS03-LIT4.
059600010912R11175        MOVE WS-SS03-LIT5 (SL)      TO  SS03-LIT5.
059610001203
059620001203       PPVR-0200-0070.
059630001203
059640001203           SET SPL                        UP BY  1.
059650001203           MOVE STMT-SUM-03               TO  WS-STMTPGE-LNE (SPL).
059660001203
059670001203      *****
059680001203      *****    Format statement summary line 4, one line for each fund.
059690001203      *****
059700001203
059710001203           SET SAF                     TO  1.
059720001203
059730001203       PPVR-0300.
059740001203
059750001203           IF  SAF        IS GREATER THAN  WS-SAF-LST
059760001203               GO TO PPVR-0500
059770001203           END-IF
059780001203
059790001203           MOVE SPACES                 TO  STMT-SUM-04.
059800001203
059810001203       PPVR-0300-0010.
059820001203
059830001219           IF  WS-STMTVRSN   NOT EQUAL TO  9
059840001203               GO TO PPVR-0300-0020
059850001203           END-IF
059860001203
059870001227001227     IF WS-SAF-NO-OF-UNITS (SAF)         IS EQUAL TO ZEROS
059880001227              SET SAF                     UP BY  1
059890001227              GO TO PPVR-0300
059900001227001227     END-IF.
059910001227
059920001203           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V8-CURRENCY.
059930001203           MOVE WS-SAF-INVESTMENT-NAME (SAF)
059940001203                                       TO  SS04V8-INVESTMENT-NAME.
059950001203           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
059960001203              MOVE WS-SAF-NET-INVESTED (SAF)
059970001203                                       TO  SS04V8-NET-INVESTED
059980001203              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
059990001203                                       TO  SS04V8-NET-INVESTED-SIGN
060000001203           END-IF
060010001203
060020001203           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
060030001203                                       TO  SS04V8-LAST-UNIT-PRICE.
060040001203           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
060050001203                                       TO  SS04V8-LAST-UNIT-PRICE-SIGN.
060060001203           MOVE WS-SAF-NO-OF-UNITS (SAF)
060070001203                                       TO  SS04V8-NO-OF-UNITS.
060080001203           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
060090001203                                       TO  SS04V8-NO-OF-UNITS-SIGN.
060100001203           MOVE WS-SAF-MARKET-VALUE (SAF)
060110001203                                       TO  SS04V8-MARKET-VALUE.
060120001203           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
060130001203                                       TO  SS04V8-MARKET-VALUE-SIGN.
060140010912R11175     MOVE WS-SAF-MARKET-PERC (SAF)
060150010912                                       TO  SS04V8-PERCEN-OF-VALUE.
060160010912           MOVE "%"                    TO  SS04V8-PERCEN-OF-VALUE-P.
060170010912
060180001203980417     IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
060190001203              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
060200001203                                       TO  SS04V8-ACB-UNIT-PRICE
060210001203              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
060220001203                                       TO  SS04V8-ACB-UNIT-PRICE-SIGN
060230001203           END-IF
060240001203
060250001203           GO TO PPVR-0300-0030.
060260001203
060270001203       PPVR-0300-0020.
060280001203
060290001203           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04-CURRENCY.
060300001203           MOVE WS-SAF-INVESTMENT-NAME (SAF)
060310001203                                       TO  SS04-INVESTMENT-NAME.
060320001203
060330001203           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
060340001203              MOVE WS-SAF-NET-INVESTED (SAF)
060350001203                                       TO  SS04-NET-INVESTED
060360001203              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
060370001203                                       TO  SS04-NET-INVESTED-SIGN
060380001203           END-IF
060390001203
060400001203           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
060410001203                                       TO  SS04-LAST-UNIT-PRICE.
060420001203           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
060430001203                                       TO  SS04-LAST-UNIT-PRICE-SIGN.
060440001203           MOVE WS-SAF-NO-OF-UNITS (SAF)
060450001203                                       TO  SS04-NO-OF-UNITS.
060460001203           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
060470001203                                       TO  SS04-NO-OF-UNITS-SIGN.
060480001203           MOVE WS-SAF-MARKET-VALUE (SAF)
060490001203                                       TO  SS04-MARKET-VALUE.
060500001203           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
060510001203                                       TO  SS04-MARKET-VALUE-SIGN.
060520010912R11175     MOVE WS-SAF-MARKET-PERC (SAF)
060530010912                                       TO  SS04-PERCEN-OF-VALUE.
060540010912           MOVE "%"                    TO  SS04-PERCEN-OF-VALUE-P.
060550010912
060560001218           IF WS-LDA-CAPITAL-GAIN     IS NOT EQUAL TO  "N"
060570001203              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
060580001203                                       TO  SS04-ACB-UNIT-PRICE
060590001203              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
060600001203                                       TO  SS04-ACB-UNIT-PRICE-SIGN
060610001203           END-IF.
060620001203
060630001203       PPVR-0300-0030.
060640001203
060650001203           IF  SPL           IS LESS THAN  WS-SPL-DTLLST
060660001203               SET SPL              UP BY  1
060670001203               GO TO PPVR-0300-0040
060680001203           END-IF
060690001203
060700061027001215     MOVE "Y"                   TO  WS-STMT-SUMM-HDG-SW.
060710001215
060720001203           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
060730061027001215     MOVE "N"                   TO  WS-STMT-SUMM-HDG-SW.
060740001203
060750001203           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
060760001203               GO TO PPVR-EXIT
060770001203           END-IF.
060780001203
060790001203       PPVR-0300-0040.
060800001203
060810001203           MOVE STMT-SUM-04            TO  WS-STMTPGE-LNE (SPL).
060820001203
060830001203           SET SAF                     UP BY  1.
060840001203           GO TO PPVR-0300.
060850001203
060860001203       PPVR-0500.
060870001203
060880001203      *****
060890001203      *****    If the currency totals cannot be formatted on the current
060900001203      *****    page, process the page and start a subsequent one, do not
060910001203      *****    print summary line 5.
060920001203      *****
060930001203
060940001203           SET WS-SPL-DTLTST           TO  SPL.
060950001203           SET SC                      TO  1.
060960001203
060970001203       PPVR-0500-0010.
060980001203
060990001203           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
061000001203               ADD 2                   TO  WS-SPL-DTLTST
061010001203               GO TO PPVR-0500-0020
061020001203           END-IF
061030001203
061040001203           IF  SC            IS LESS THAN  WS-SC-MAX
061050001203               SET SC               UP BY  1
061060001203               GO TO PPVR-0500-0010
061070001203           END-IF
061080001203
061090001203      *****
061100001203      *****    There are no currency total lines to be printed.
061110001203      *****
061120001203
061130001203           GO TO PPVR-0800.
061140001203
061150001203       PPVR-0500-0020.
061160001203
061170001203           IF  SC        IS NOT LESS THAN  WS-SC-MAX
061180001203               GO TO PPVR-0500-0030
061190001203           END-IF
061200001203
061210001203           SET SC                   UP BY  1.
061220001203
061230001203           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
061240001203               ADD 1                   TO  WS-SPL-DTLTST
061250001203           END-IF
061260001203
061270001203           GO TO PPVR-0500-0020.
061280001203
061290001203       PPVR-0500-0030.
061300001203
061310001203           IF  WS-SPL-DTLTST  IS NOT GREATER THAN  WS-SPL-DTLLST
061320001203               GO TO PPVR-0600
061330001203           END-IF
061340001203
061350001203           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
061360001203
061370001203           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
061380001203               GO TO PPVR-EXIT
061390001203           END-IF
061400001203
061410001203           GO TO PPVR-0700.
061420001203
061430001203       PPVR-0600.
061440001203
061450001203      *****
061460001203      *****    Format statement summary line 5.
061470001203      *****
061480001203
061490001203           MOVE SPACES                   TO  STMT-SUM-05.
061500001203
061510001203           EVALUATE WS-STMTVRSN
061520001219             WHEN 9
061530001203               MOVE WS-SS05V1-LIT1 (SL)  TO  SS05V8-LIT1
061540010912R11175         MOVE WS-SS05-LIT2 (SL)    TO  SS05V8-LIT2
061550001203             WHEN OTHER
061560001203               MOVE WS-SS05-LIT1 (SL)      TO  SS05-LIT1
061570010912R11175         MOVE WS-SS05-LIT2 (SL)      TO  SS05-LIT2
061580001203           END-EVALUATE.
061590001203
061600001203       PPVR-0600-0010.
061610001203
061620001203           SET SPL                     UP BY  1.
061630001203           MOVE STMT-SUM-05            TO  WS-STMTPGE-LNE (SPL).
061640001203
061650001203       PPVR-0700.
061660001203
061670001203      *****
061680001203      *****    Format statement summary line 6, one line for each
061690001203      *****    applicable currency.
061700001203      *****
061710001203
061720001203           SET SC                      TO  1.
061730001203
061740001203       PPVR-0700-0010.
061750001203
061760001203           IF  SC         IS GREATER THAN  WS-SC-MAX
061770001203               GO TO PPVR-0800
061780001203           END-IF
061790001203
061800001203           IF  WS-SC-RPTIND (SC) EQUAL TO  "N"
061810001203               GO TO PPVR-0700-0030
061820001203           END-IF
061830001203
061840001203           MOVE SPACES                 TO  STMT-SUM-06.
061850001203
061860001219           IF  WS-STMTVRSN    IS EQUAL TO  9
061870001203               MOVE WS-SS06-LIT1 (SL)  TO  SS06V8-LIT1
061880001203               MOVE WS-SC-CURRDESC (SC, SCL)
061890001203                                       TO  SS06V8-LIT-CURRDESC
061900001203               MOVE WS-SC-MARKET-VALUE (SC)
061910001203                                       TO  SS06V8-MARKET-VALUE
061920010912R11175         MOVE WS-SC-MARKET-PERC  (SC)
061930010912                                       TO  SS06V8-PERCEN-OF-VALUE
061940010912               MOVE "%"                TO  SS06V8-PERCEN-OF-VALUE-P
061950001203               GO TO PPVR-0700-0020
061960001203           END-IF
061970001203
061980001203           MOVE WS-SS06-LIT1 (SL)        TO  SS06-LIT1.
061990001203           MOVE WS-SC-CURRDESC (SC, SCL) TO  SS06-LIT-CURRDESC.
062000001203           MOVE WS-SC-MARKET-VALUE (SC)  TO  SS06-MARKET-VALUE.
062010010912R11175     MOVE WS-SC-MARKET-PERC  (SC)
062020010912                                   TO  SS06-PERCEN-OF-VALUE.
062030010912           MOVE "%"                TO  SS06-PERCEN-OF-VALUE-P.
062040001203
062050001203       PPVR-0700-0020.
062060001203
062070001203           SET SPL                     UP BY  1.
062080001203           MOVE STMT-SUM-06            TO  WS-STMTPGE-LNE (SPL).
062090001203
062100001203       PPVR-0700-0030.
062110001203
062120001203           SET SC                      UP BY  1.
062130001203           GO TO PPVR-0700-0010.
062140001203
062150001203       PPVR-0800.
062160001203
062170001203      *****
062180001203      *****    Determine if RRIF and LIF message is to be printed.
062190001203      *****
062200001203
062210001203           IF  WS-LDA-RIF-LIF-MSG IS NOT EQUAL TO "Y"
062220001203               GO TO PPVR-1030
062230001203           END-IF
062240001203
062250001203           IF  WS-STMT-DTA-SW-60 IS NOT EQUAL TO "Y"
062260001203               GO TO PPVR-1030
062270001203           END-IF
062280001203
062290001203           IF  WS-LDA-RRIF-FLAG IS NOT EQUAL TO "Y"
062300001203               GO TO PPVR-1000
062310001203           END-IF
062320001203
062330001203           IF  AW-RIF-LIF-INDICATOR IS NOT EQUAL TO "R"
062340001203               GO TO PPVR-1000
062350001203           END-IF
062360001203
062370001203           SET              SPL UP BY 5.
062380001203
062390001203           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
062400001203              SET           SPL DOWN BY 5
062410001203              GO TO PPVR-0900
062420001203           END-IF
062430001203
062440001203           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
062450001203
062460001203           IF WS-PSRERR-SW NOT EQUAL TO SPACES
062470001203              GO TO PPVR-EXIT
062480001203           END-IF.
062490001203
062500001203       PPVR-0900.
062510001203
062520001219           IF WS-STMTVRSN                 IS EQUAL TO  9
062530001203              MOVE WS-SYS-DATE               TO SS07V8-YEAR
062540001203              MOVE WS-SS07V1-LIT1(SL)        TO SS07V8-LIT
062550001203              MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V8-RRIF-LIF-AMT
062560001203           END-IF
062570001203
062580001203           SET SPL UP BY 4.
062590001203
062600001219           IF WS-STMTVRSN    IS EQUAL TO  9
062610001203              MOVE STMT-SUM-07-V8    TO WS-STMTPGE-LNE (SPL)
062620001203           END-IF
062630001203
062640001203           GO TO PPVR-1030.
062650001203
062660001203       PPVR-1000.
062670001203
062680001203           IF  WS-LDA-ACCOUNT-LIF IS NOT EQUAL TO "Y"
062690001203               GO TO PPVR-1030
062700001203           END-IF
062710001203
062720001203           IF  AW-RIF-LIF-INDICATOR IS NOT EQUAL TO "L"
062730001203               GO TO PPVR-1030
062740001203           END-IF
062750001203
062760001203           SET              SPL UP BY 5.
062770001203
062780001203           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
062790001203              SET           SPL DOWN BY 5
062800001203              GO TO PPVR-1010
062810001203           END-IF
062820001203
062830001203           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
062840001203
062850001203           IF WS-PSRERR-SW NOT EQUAL TO SPACES
062860001203              GO TO PPVR-EXIT
062870001203           END-IF.
062880001203
062890001203       PPVR-1010.
062900001203
062910001219           IF WS-STMTVRSN                 IS EQUAL TO  9
062920001203              MOVE WS-SYS-DATE               TO SS07V8-YEAR
062930001203              MOVE WS-SS07V1-LIT2(SL)        TO SS07V8-LIT
062940001203              MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V8-RRIF-LIF-AMT
062950001203           END-IF
062960001203
062970001203           SET SPL UP BY 4.
062980001203
062990001219           IF WS-STMTVRSN                 IS EQUAL TO  9
063000001203              MOVE STMT-SUM-07-V8    TO WS-STMTPGE-LNE (SPL)
063010001203           END-IF
063020001203
063030001203           SET              SPL UP BY 2.
063040001203
063050001203           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
063060001203              SET           SPL DOWN BY 2
063070001203              GO TO PPVR-1020
063080001203           END-IF
063090001203
063100001203           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
063110001203
063120001203           IF WS-PSRERR-SW NOT EQUAL TO SPACES
063130001203              GO TO PPVR-EXIT
063140001203           END-IF.
063150001203
063160001203       PPVR-1020.
063170001203
063180001219           IF WS-STMTVRSN                 IS EQUAL TO  9
063190001203              MOVE WS-SYS-DATE               TO SS08V8-YEAR
063200001203              MOVE WS-SS08V1-LIT(SL)         TO SS08V8-LIT
063210001203              MOVE AW-MAXIMUM-ANNUAL-PAYMENT TO SS08V8-RRIF-LIF-AMT
063220001203           END-IF
063230001203
063240001203           SET SPL UP BY 1.
063250001203
063260001219           IF WS-STMTVRSN                 IS EQUAL TO  9
063270001203              MOVE STMT-SUM-08-V8    TO WS-STMTPGE-LNE (SPL)
063280001203           END-IF.
063290001203
063300001203       PPVR-1030.
063310001203
063320001203      *****
063330001203      *****    Determine if the Canadian dollar capital gains line is to
063340001203      *****    be printed.
063350001203      *****
063360001203
063370001203           IF  WS-CPG-FIXED   IS EQUAL TO  ZEROES
063380001203           OR  WS-STMTVRSN    IS EQUAL TO  6
063390001203               GO TO PPVR-1060
063400001203           END-IF
063410001203
063420001203      *****
063430001203      *****    The Canadian dollar capital gains line is to be printed.
063440001203      *****    This line is to be printed on the second last detail
063450001203      *****    line, at least two lines below the last detail line
063460001203      *****    formatted.  If this is not possible, process the current
063470001203      *****    page and start a sbusequent one.
063480001203      *****
063490001203
063500001203           SUBTRACT 5                FROM  WS-SPL-DTLLST
063510001203                                   GIVING  WS-SPL-DTLTST.
063520001203           IF  WS-SPL-DTLTST GREATER THAN  SPL
063530001203               GO TO PPVR-1040
063540001203           END-IF
063550001203
063560001203           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
063570001203
063580001203           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
063590001203               GO TO PPVR-EXIT
063600001203           END-IF.
063610001203
063620001203       PPVR-1040.
063630001203
063640001203           IF  WS-CPG-FIXED  IS LESS THAN  ZEROES
063650001203               MOVE "-"                TO  WS-CPG-FIXED-SIGN
063660001203           ELSE
063670001203               MOVE SPACES             TO  WS-CPG-FIXED-SIGN
063680001203           END-IF
063690001203
063700001203           MOVE SPACES                 TO  STMT-SUM-11.
063710001203
063720001219           IF  WS-STMTVRSN    IS EQUAL TO  9
063730001203               MOVE WS-SS11-LIT1 (SL)  TO  SS11V8-LIT1
063740001203               MOVE WS-CPG-FIXED       TO  SS11V8-CPG-FIXED
063750001203               MOVE WS-CPG-FIXED-SIGN  TO  SS11V8-CPG-FIXED-SIGN
063760001203               GO TO PPVR-1050
063770001203           END-IF
063780001203
063790001203           MOVE WS-SS11-LIT1 (SL)      TO  SS11-LIT1.
063800001203           MOVE WS-CPG-FIXED           TO  SS11-CPG-FIXED.
063810001203           MOVE WS-CPG-FIXED-SIGN      TO  SS11-CPG-FIXED-SIGN.
063820001203
063830001203       PPVR-1050.
063840001203
063850001218001218     SET SPL                     TO  WS-SPL-DTLLST.
063860001218001218     SET SPL                     DOWN BY  2.
063870001203           MOVE STMT-SUM-11            TO  WS-STMTPGE-LNE (SPL).
063880001203
063890001203       PPVR-1060.
063900001203
063910001203      *****
063920001203      *****    Process the current page.
063930001203      *****
063940001203
063950001218001218     SET SPL                     UP  BY  3.
063960001218001218     SET WS-SPL-DTLTST           TO  SPL.
063970001218
063980001218           IF  (WS-SPL-DTLTST           +  6)
063990001218                      IS NOT GREATER THAN  WS-SPL-DTLLST
064000001218               GO TO PPVR-EXIT
064010001218           END-IF
064020001218
064030001218           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
064040001218
064050001218           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
064060001218               GO TO PPVR-EXIT
064070001218           END-IF.
064080001218
064090001218001218*    PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
064100001218
064110001218      *    IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
064120001218      *        GO TO PPVR-EXIT
064130001218      *    END-IF.
064140001203
064150001128      **********
064160001128
064170001128       PPVR-EXIT.
064180001128            EXIT.
064190001128
064200001128      **********
064210001128
064220000000      /
064230000000       LEVEL-SUMMARY-2.
064240000000
064250000000      ******************************************************************
064260000000      *                                                                *
064270000000      *    L E V E L   S U M M A R I Z A T I O N   2   R O U T I N E   *
064280000000      *                                                                *
064290000000      *        INVESTOR-NO:            Investor Number                 *
064300000000      *                                                                *
064310000000      ******************************************************************
064320000000
064330000000      *****
064340000000      *****    Null routine.
064350000000      *****
064360000000
064370000000           CONTINUE.
064380000000
064390990921      **********
064400990921
064410000000       LS2-EXIT.
064420000000           EXIT.
064430990921
064440990921      **********
064450990921
064460000000      /
064470000000       LEVEL-REINIT-2.
064480000000
064490000000      ******************************************************************
064500000000      *                                                                *
064510000000      *    L E V E L   R E I N I T I A L I Z A T I O N   2   R T N .   *
064520000000      *                                                                *
064530000000      *        INVESTOR-NO:            Investor Number                 *
064540000000      *                                                                *
064550000000      ******************************************************************
064560000000
064570000000      *****
064580000000      *****    Indicate that the Investor Information record and the
064590000000      *****    Dealer record have not yet been read.
064600000000      *****
064610000000
064620000000           MOVE "N"                    TO  WS-STMT-DTA-SW-01.
064630000000           MOVE "N"                    TO  WS-STMT-DTA-SW-05.
064640001129           MOVE "N"                    TO  WS-STMT-DTA-SW-30.
064650001129           MOVE "N"                    TO  WS-STMT-DTA-SW-35.
064660010427RF9415     MOVE "N"                    TO  WS-STMT-DTA-SW-40.
064670010427RF9415     MOVE "N"                    TO  WS-STMT-DTA-SW-43.
064680010427RF9415     MOVE "N"                    TO  WS-STMT-DTA-SW-45.
064690010427RF9415     MOVE "N"                    TO  WS-ACCOUNT-INV-SW.
064700010430RF9415     MOVE "N"                    TO  WS-LR1-PROCESSED-SW.
064710001220001220     MOVE "N"                    TO  WS-STMT-DTA-SW-80.
064720010604010531     MOVE "N"                    TO  WS-STMT-DTA-SW-60.
064730000000
064740000000      *****
064750000000      *****    Initialize the statement investor accounts table.
064760010430      *****    (SECTION 10)
064770010430      *****
064780000000
064790000000           MOVE HIGH-VALUES            TO  WS-STMT-INVESTOR-ACCOUNT-TBL.
064800000000           MOVE ZEROES                 TO  WS-SIA-LST.
064810000000
064820010419RF9415*****
064830010430      *****    Initialize the dealer information table(SECTION 5)
064840010419      *****
064850010419
064860010419           INITIALIZE                  WS-DEALER-INFO-TBL.
064870010419           MOVE ZEROES                 TO  WS-DI-LST.
064880010419
064890010604R10243*****
064900010604      *****    Initialize the WS-INVESTOR-INFO(SECTION 1)
064910010604      *****
064920010604
064930010604           INITIALIZE                  WS-INVESTOR-INFO.
064940010604           INITIALIZE                  WS-INVESTOR-INFO-TEMP.
064950010604
064960010604
064970990921      **********
064980990921
064990000000       LR2-EXIT.
065000000000           EXIT.
065010990921
065020990921      **********
065030990921
065040000000      /
065050000000       LEVEL-REINIT-1.
065060000000
065070000000      ******************************************************************
065080000000      *                                                                *
065090000000      *    L E V E L   R E I N I T I A L I Z A T I O N   1   R T N .   *
065100000000      *                                                                *
065110000000      *        ACCOUNT:                Account                         *
065120000000      *                                                                *
065130000000      ******************************************************************
065140000000
065150000000      *****
065160000000      *****    If Account is zeroes, bypass this routine.
065170000000      *****
065180000000
065190990921           IF CURR-ACCOUNT   IS EQUAL TO  ZEROES
065200990921              GO TO LR1-EXIT
065210990921           END-IF
065220010430
065230010427           IF CURR-ACCOUNT   IS NOT EQUAL TO  ZEROES
065240010427RF9415     AND WS-ACCOUNT-INV-SW EQUAL TO  "N"
065250010427              MOVE ZEROES                 TO  WS-CPG-FIXED
065260010427              MOVE SPACES                 TO  WS-CPG-FIXED-SIGN
065270010427              GO TO LR1-0090-0020
065280010427           END-IF
065290010427
065300010501      *    IF CURR-ACCOUNT   IS NOT EQUAL TO  ZEROES
065310010501RF9415*    AND WS-STMT-DTA-SW-40 EQUAL TO  "N"
065320010501      *       GO TO LR1-EXIT
065330010501      *    END-IF
065340010427
065350010430      ** TEST
065360010430      **   IF WS-LR1-PROCESSED-SW     IS EQUAL TO "Y"
065370010502010502*    IF PR-SECTION-NO           IS NOT EQUAL TO  80
065380010502      *       GO TO LR1-EXIT
065390010502      *    END-IF
065400010430      ** TEST
065410010430
065420000000      *****
065430000000      *****    If one of the following conditions is detected, an error
065440000000      *****    message will be printed and the account will be bypassed.
065450000000      *****       1. Investor Information record missing.
065460000000      *****       2. Dealer record missing.
065470000000      *****       3. Account Detail record missing.
065480000000      *****    If this occurs, the program that creates the Statement
065490000000      *****    Data file should be reviewed.
065500000000      *****
065510000000
065520000000           MOVE "N"                    TO  WS-L01BYP-SW.
065530010604010531*    MOVE "N"                    TO  WS-STMT-DTA-SW-60.
065540001220001220     MOVE "N"                    TO  WS-STMT-DTA-SW-80.
065550001205           MOVE "N"                    TO  WS-PLAN-SUM-SW.
065560001218           MOVE "N"                    TO  WS-TRANS-HEADING-SW.
065570981208
065580000000           IF  WS-STMT-DTA-SW-01 EQUAL TO  "Y"
065590000000           AND WS-STMT-DTA-SW-05 EQUAL TO  "Y"
065600010501RF9415*    AND WS-STMT-DTA-SW-40 EQUAL TO  "Y"
065610010502      *    AND PR-SECTION-NO  IS EQUAL TO  80
065620991103991103         GO TO LR1-0015
065630990921           END-IF
065640990921
065650000000           MOVE SPACES                 TO  ERROR-LINE-1.
065660000000
065670000000           IF  WS-STMT-DTA-SW-01 EQUAL TO  "N"
065680000000               SET MSG                 TO  8
065690990921               GO TO LR1-0010
065700990921           END-IF
065710000000
065720000000           IF  WS-STMT-DTA-SW-05 EQUAL TO  "N"
065730000000               SET MSG                 TO  9
065740990921               GO TO LR1-0010
065750990921           END-IF
065760000000
065770000000           IF  PR-SECTION-NO NOT EQUAL TO  20
065780000000               SET MSG                 TO  10
065790990921               GO TO LR1-0010
065800990921           END-IF
065810000000
065820000000           CONTINUE.
065830000000
065840000000       LR1-0010.
065850990921
065860000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
065870000000           MOVE "Y"                    TO  WS-L01BYP-SW.
065880000000           GO TO LR1-EXIT.
065890991103
065900991103      *****************************************************
065910991103
065920991103       LR1-0015.
065930991103
065940991103      *****
065950991103      *****    Search the account type table for the correct
065960991103      *****    account type rule.
065970991103      *****
065980991103
065990991103           SET AT1                     TO  1.
066000991103           IF  WS-AT-LST        IS NOT GREATER THAN  ZEROES
066010991103               GO TO LR1-0020
066020991103           END-IF.
066030991103
066040991103       LR1-0015-000.
066050991103
066060190612      *RFS183449-Starts
066070190612991103*    IF  AD-ACCOUNT-TYPE-SHORT  IS EQUAL TO  WS-AT-CODE (AT1)
066080190612           INITIALIZE Lc-Acct-Type-Code
066090190619           PERFORM Get-Account-Type-Code
066100190612           IF Lc-Acct-Type-Code IS EQUAL TO  WS-AT-CODE (AT1)
066110190612      *RFS183449-Ends
066120991103               GO TO LR1-0020
066130991103           END-IF
066140991103
066150991103           SET AT1                  UP BY  1.
066160991103
066170991103           IF  AT1    IS NOT GREATER THAN  WS-AT-LST
066180991103               GO TO LR1-0015-000
066190991103           END-IF.
066200991103
066210991103      *************************************************************
066220991103
066230000000       LR1-0020.
066240000000
066250000000      *****
066260000000      *****    Find the account information in the investor account
066270000000      *****    table.  Format the nominee and registered contribution
066280000000      *****    data and print a message, if required.
066290000000      *****
066300000000
066310000000           MOVE SPACES                 TO  WS-NOMINEE.
066320000000           MOVE SPACES                 TO  WS-NOMINEE-CODE.
066330000000           MOVE SPACES                 TO  WS-NOMINEE-NAME.
066340000000           MOVE SPACES                 TO  WS-NOMINEES-ACCT-NO.
066350971224           MOVE ZEROES                 TO  WS-CPG-FIXED.
066360971224           MOVE SPACES                 TO  WS-CPG-FIXED-SIGN.
066370000000           MOVE ZEROES                 TO  WS-REG-CTB-CUR.
066380000000           MOVE SPACES                 TO  WS-REG-CTB-CUR-SIGN.
066390000000
066400001130001128     MOVE ZEROES              TO  WS-AD-MARKET-VALUE-PRIOR-TOT.
066410001130           MOVE ZEROES              TO  WS-AD-MARKET-VALUE-TOT.
066420001219001219     MOVE ZEROES              TO  WS-AD-DEPOSIT1-TOT.
066430001219001219     MOVE ZEROES              TO  WS-AD-DEPOSIT2-TOT.
066440001219001219     MOVE ZEROES              TO  WS-AD-WITHDRAWALS1-TOT.
066450001219001219     MOVE ZEROES              TO  WS-AD-WITHDRAWALS2-TOT.
066460001219001219     MOVE ZEROES              TO  WS-AD-DEP-MINUS-WITHD1.
066470001219001219     MOVE ZEROES              TO  WS-AD-DEP-MINUS-WITHD2.
066480010912R11175     MOVE ZEROES              TO  WS-INCREASE-DECREASE-1.
066490010912R11175     MOVE ZEROES              TO  WS-INCREASE-DECREASE-2.
066500001128
066510000000           SEARCH ALL WS-SIA-ENT
066520000000               AT END
066530000000                   GO TO LR1-0020-0020
066540000000               WHEN WS-SIA-ACCT (SIA)
066550000000                              IS EQUAL TO  AD-ACCOUNT
066560000000                   MOVE WS-SIA-NOMCDE (SIA)
066570000000                                       TO  WS-NOMINEE-CODE
066580000000                   MOVE WS-SIA-NOMNAM (SIA)
066590000000                                       TO  WS-NOMINEE-NAME
066600000000                   MOVE WS-SIA-NOMACCT (SIA)
066610000000                                       TO  WS-NOMINEES-ACCT-NO
066620000000                   MOVE WS-SIA-REGCTB (SIA)
066630000000                                       TO  WS-REG-CTB-CUR
066640000000                   MOVE WS-SIA-REGCTBSGN (SIA)
066650000000                                       TO  WS-REG-CTB-CUR-SIGN.
066660001229001229             MOVE WS-SIA-DEPOSIT-CURRENT (SIA)
066670001229                                       TO  WS-AD-DEPOSIT1-TOT
066680001229                   MOVE WS-SIA-DEPOSIT-INCEPT (SIA)
066690001229                                       TO  WS-AD-DEPOSIT2-TOT
066700001229                   MOVE WS-SIA-WITHDRAW-CURRENT (SIA)
066710001229                                       TO  WS-AD-WITHDRAWALS1-TOT
066720001229                   MOVE WS-SIA-WITHDRAW-INCEPT (SIA)
066730001229                                       TO  WS-AD-WITHDRAWALS2-TOT.
066740010419RF9415             MOVE WS-SIA-MAIL-FLAG (SIA)
066750010419                                   TO  WS-MAILING-ADDRESS-INDICATOR.
066760010419
066770000000       LR1-0020-0020.
066780000000
066790000000      *****
066800000000      *****    If the account is flagged as NOMINEE but no nominee data
066810000000      *****    was found, an error message will be printed and the
066820000000      *****    nominee name will be defaulted to 'NOMINEE'.  If this
066830000000      *****    occurs, the program that creates the Statement Data file
066840000000      *****    should be reviewed.
066850000000      *****
066860000000
066870000000           IF  AD-NOMINEE     IS EQUAL TO  "Y"
066880000000           AND WS-NOMINEE-CODE   EQUAL TO  SPACES
066890000000           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
066900000000           AND WS-NOMINEES-ACCT-NO
066910000000                              IS EQUAL TO  SPACES
066920000000               MOVE SPACES             TO  ERROR-LINE-1
066930000000               SET MSG                 TO  12
066940000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
066950000000               MOVE "NOMINEE"          TO  WS-NOMINEE-NAME.
066960000000
066970000000      *****
066980000000      *****    If the account is not flagged as NOMINEE but nominee data
066990000000      *****    is found, an error message will be printed and the
067000000000      *****    nominee data will be set to spaces.  If this occurs, the
067010000000      *****    program that creates the Statement Data file should be
067020000000      *****    reviewed.
067030000000      *****
067040000000
067050000000           IF  AD-NOMINEE     IS EQUAL TO  "Y"
067060000000               GO TO LR1-0020-0030.
067070000000
067080000000           IF  WS-NOMINEE-CODE
067090000000                          IS NOT EQUAL TO  SPACES
067100000000           OR  WS-NOMINEE-NAME
067110000000                          IS NOT EQUAL TO  SPACES
067120000000           OR  WS-NOMINEES-ACCT-NO
067130000000                          IS NOT EQUAL TO  SPACES
067140000000               MOVE SPACES             TO  ERROR-LINE-1
067150000000               SET MSG                 TO  13
067160000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
067170000000               MOVE SPACES             TO  WS-NOMINEE-CODE
067180000000               MOVE SPACES             TO  WS-NOMINEE-NAME
067190000000               MOVE SPACES             TO  WS-NOMINEES-ACCT-NO.
067200000000
067210000000       LR1-0020-0030.
067220000000
067230000000      *****
067240000000      *****    Format the nominee data, if applicable.
067250000000      *****
067260000000
067270000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
067280000000           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
067290000000           AND WS-NOMINEES-ACCT-NO
067300000000                              IS EQUAL TO  SPACES
067310000000               GO TO LR1-0020-0040.
067320000000
067330000000           IF  WS-NOMINEES-ACCT-NO
067340000000                              IS EQUAL TO  SPACES
067350000000               MOVE WS-NOMINEE-NAME    TO  WS-NOMINEE
067360000000               GO TO LR1-0020-0040.
067370000000
067380000000           STRING WS-NOMINEE-NAME
067390000000                             DELIMITED BY  "  ",
067400000000                  " ("       DELIMITED BY  SIZE,
067410000000                  WS-NOMINEES-ACCT-NO
067420000000                             DELIMITED BY  " ",
067430000000                  ")"        DELIMITED BY  SIZE
067440000000                                     INTO  WS-NOMINEE.
067450000000
067460000000       LR1-0020-0040.
067470000000
067480000000      *****
067490000000      *****    The literals will be formatted depending on the account
067500000000      *****    language code.  If the language code is not handled,
067510000000      *****    default to English.
067520000000      *****
067530000000
067540000000           IF  AD-LANGUAGE    IS EQUAL TO  "E"
067550000000               SET SCL                 TO  1
067560990623               SET SHL                 TO  1
067570000000               SET SL                  TO  1
067580001204               SET SPSM                TO  1
067590001207               SET SPS2                TO  1
067600001217               SET FML                 TO  1
067610001218               SET SML                 TO  1
067620010912R11175         SET FMLH                TO  1
067630000000               GO TO LR1-0030.
067640000000
067650000000           IF  AD-LANGUAGE    IS EQUAL TO  "F"
067660000000               SET SCL                 TO  2
067670990623               SET SHL                 TO  2
067680000000               SET SL                  TO  2
067690001204               SET SPSM                TO  2
067700001207               SET SPS2                TO  2
067710001217               SET FML                 TO  2.
067720001218               SET SML                 TO  2.
067730010912R11175         SET FMLH                TO  2
067740000000               GO TO LR1-0030.
067750000000
067760000000           SET SCL                     TO  1.
067770990623           SET SHL                     TO  1.
067780000000           SET SL                      TO  1.
067790001204           SET SPSM                    TO  1.
067800001207           SET SPS2                    TO  1.
067810001217           SET FML                     TO  1.
067820001218           SET SML                     TO  1.
067830010912R11175     SET FMLH                    TO  1.
067840000000
067850000000       LR1-0030.
067860000000
067870010419RF9415     SET DI                      TO  1.
067880010430RF9415     SET SFC                     TO  1.
067890010430RF9415     SET SFCI                    TO  1.
067900010430RF9415     SET ANN                     TO  1.
067910010423
067920010430       LR1-0030-0010-0010.
067930010423
067940010419           IF DI    IS GREATER THAN   WS-DI-LST
067950010419              INITIALIZE DEALER-SECTION
067960010419              MOVE WS-DEALER-INFO (DI) TO DEALER-SECTION
067970010430              GO TO LR1-0030-0010-0020
067980010419           END-IF
067990010423
068000010419           INITIALIZE DEALER-SECTION.
068010010423           MOVE WS-DEALER-INFO (DI)      TO  DEALER-SECTION.
068020010423
068030010517           IF DL-ACCOUNT2 OF DEALER-SECTION IS
068040010517                                       NOT EQUAL TO AD-ACCOUNT
068050010517      *    IF DL-DEALER   OF DEALER-SECTION IS
068060010517      *                                NOT EQUAL TO AD-DEALER
068070010423              SET DI                   UP BY 1
068080010430              GO TO LR1-0030-0010-0010
068090010419           END-IF.
068100010419
068110010430       LR1-0030-0010-0020.
068120010430
068130010501           IF WS-STMT-DTA-SW-40         IS EQUAL TO "N"
068140010501              INITIALIZE SEG-FUND-CONTRACT-DETAILS
068150010515              MOVE ZEROS      TO  CD-START-DATE1
068160010604              MOVE ZEROS      TO  CD-FIRST-TRADE-DATE
068170010515              MOVE ZEROS      TO  CD-MATURITY-DATE
068180010501              GO TO LR1-0030-0010-0030
068190010501           END-IF
068200010501
068210010430           IF SFC   IS GREATER THAN   WS-SFC-LST
068220010430              INITIALIZE SEG-FUND-CONTRACT-DETAILS
068230010430              MOVE WS-SEG-FUND-CONTRACT-DETAILS (SFC)
068240010430                            TO SEG-FUND-CONTRACT-DETAILS
068250010430              GO TO LR1-0030-0010-0030
068260010430           END-IF
068270010430
068280010430           INITIALIZE SEG-FUND-CONTRACT-DETAILS.
068290010430           MOVE WS-SEG-FUND-CONTRACT-DETAILS (SFC)
068300010430                            TO SEG-FUND-CONTRACT-DETAILS.
068310010430
068320010430           IF CD-ACCOUNT OF SEG-FUND-CONTRACT-DETAILS
068330010430                                       NOT EQUAL TO AD-ACCOUNT
068340010430              SET SFC                  UP BY 1
068350010430              GO TO LR1-0030-0010-0020
068360010430           END-IF.
068370010430
068380010430       LR1-0030-0010-0030.
068390010501
068400010501           IF WS-STMT-DTA-SW-43         IS EQUAL TO "N"
068410010501              INITIALIZE SEG-FUND-CONTRACT-INVESTMENT
068420010501              GO TO LR1-0030-0010-0040
068430010501           END-IF
068440010501
068450010430           IF SFCI  IS GREATER THAN   WS-SFCI-LST
068460010430              INITIALIZE SEG-FUND-CONTRACT-INVESTMENT
068470010430              MOVE WS-SEG-FUND-CONTRACT-INV (SFCI)
068480010430                           TO  SEG-FUND-CONTRACT-INVESTMENT
068490010430              GO TO LR1-0030-0010-0040
068500010430           END-IF
068510010430
068520010430           INITIALIZE SEG-FUND-CONTRACT-INVESTMENT.
068530010430           MOVE WS-SEG-FUND-CONTRACT-INV (SFCI)
068540010430                            TO SEG-FUND-CONTRACT-INVESTMENT.
068550010430
068560010430           IF CI-ACCOUNT OF SEG-FUND-CONTRACT-INVESTMENT
068570010430                                       NOT EQUAL TO AD-ACCOUNT
068580010430              SET SFCI                 UP BY 1
068590010430              GO TO LR1-0030-0010-0030
068600010430           END-IF.
068610010430
068620010430       LR1-0030-0010-0040.
068630010430
068640010501           IF WS-STMT-DTA-SW-45         IS EQUAL TO "N"
068650010501              INITIALIZE ANNUITANT-DETAILS
068660010501              GO TO LR1-0030-0005
068670010501           END-IF
068680010501
068690010430           IF ANN   IS GREATER THAN   WS-ANN-LST
068700010430              INITIALIZE ANNUITANT-DETAILS
068710010430              MOVE WS-ANNUITANT-DETAILS (ANN)
068720010430                           TO  ANNUITANT-DETAILS
068730010430              GO TO LR1-0030-0005
068740010430           END-IF
068750010430
068760010430           INITIALIZE ANNUITANT-DETAILS.
068770010430           MOVE WS-ANNUITANT-DETAILS (ANN)
068780010430                            TO ANNUITANT-DETAILS.
068790010430
068800010430           IF AN-ACCOUNT OF ANNUITANT-DETAILS
068810010430                                       NOT EQUAL TO AD-ACCOUNT
068820010430              SET ANN                  UP BY 1
068830010430              GO TO LR1-0030-0010-0040
068840010430           END-IF.
068850010430
068860010419       LR1-0030-0005.
068870010419
068880000000      *****
068890000000      *****    Format the required fields.
068900000000      *****
068910000000
068920000000           MOVE II-END-DATE            TO  WS-END-DATE.
068930990923           MOVE II-END-DATE            TO  WS-END-DATE-6.
068940000000           MOVE "N"                    TO  WS-PSTCDEPRT-SW.
068950000000           MOVE II-START-DATE          TO  WS-START-DATE.
068960001130001130     MOVE II-START-DATE          TO  WS-SHPL-HD-STDATE.
068970001130           MOVE II-END-DATE            TO  WS-SHPL-HD-ENDATE.
068980000000
068990010604RF9415*    MOVE CD-START-DATE1         TO  WS-SHGL-HD-ISDATE.
069000010604R10243     MOVE CD-FIRST-TRADE-DATE    TO  WS-SHGL-HD-ISDATE.
069010010430RF9415     MOVE CD-MATURITY-DATE       TO  WS-SHGL-HD-ANDATE.
069020010430
069030010430      ***
069040010430      ***     Process Annuitant-name.
069050010430      ***
069060010430
069070010430           MOVE SPACES                 TO  WS-ADRTMP.
069080010528R10243     MOVE SPACES                 TO  WS-ANNUITANT-NAME.
069090010430           STRING  AN-SALUTATION-CODE
069100010430                             DELIMITED BY  " "
069110010430                                     INTO  WS-ADRTMP.
069120010430
069130010430           IF  WS-ADRTMP      IS EQUAL TO  SPACES
069140010430               STRING AN-FIRST-NAME
069150010430                             DELIMITED BY  "  "
069160010430                                     INTO  WS-ADRTMP
069170010430             ELSE
069180010430               STRING WS-ADRTMP
069190010430                             DELIMITED BY  "  ",
069200010430                      " "    DELIMITED BY  SIZE,
069210010430                      AN-FIRST-NAME
069220010430                             DELIMITED BY  "  "
069230010430                                     INTO  WS-ANNUITANT-NAME
069240010430               MOVE WS-ANNUITANT-NAME  TO  WS-ADRTMP.
069250010430
069260010430           IF  WS-ADRTMP      IS EQUAL TO  SPACES
069270010430               STRING AN-INITIALS
069280010430                             DELIMITED BY  "  "
069290010430                                     INTO  WS-ADRTMP
069300010430             ELSE
069310010430               STRING WS-ADRTMP
069320010430                             DELIMITED BY  "  ",
069330010430                      " "    DELIMITED BY  SIZE,
069340010430                      AN-INITIALS
069350010430                             DELIMITED BY  "  "
069360010430                                     INTO  WS-ANNUITANT-NAME
069370010430               MOVE WS-ANNUITANT-NAME  TO  WS-ADRTMP.
069380010430
069390010430           IF  WS-ADRTMP      IS EQUAL TO  SPACES
069400010430               STRING AN-LAST-NAME
069410010430                             DELIMITED BY  "  "
069420010430                                     INTO  WS-ADRTMP
069430010430             ELSE
069440010430               STRING WS-ADRTMP
069450010430                             DELIMITED BY  "  ",
069460010430                      " "    DELIMITED BY  SIZE,
069470010430                      AN-LAST-NAME
069480010430                             DELIMITED BY  "  "
069490010430                                     INTO  WS-ANNUITANT-NAME
069500010430               MOVE WS-ANNUITANT-NAME  TO  WS-ADRTMP.
069510010430
069520010430           MOVE WS-ADRTMP              TO  WS-ANNUITANT-NAME.
069530010430
069540001130           EVALUATE WS-SHPL-HD-STDATE-MM
069550001130              WHEN 01
069560001218                  MOVE DI-JAN (SML) TO WS-MONTH-LIT1
069570001218                  MOVE DI-JAN (SML) TO WS-MONTH-LIT1-F
069580010912R11175            MOVE FDI-JAN (FML)   TO WS-MONTH-LIT6
069590010912                  MOVE FDI-JAN (FML)   TO WS-MONTH-LIT6-F
069600001130              WHEN 02
069610001218                  MOVE DI-FEB (SML) TO WS-MONTH-LIT1
069620001218                  MOVE DI-FEB (SML) TO WS-MONTH-LIT1-F
069630010912R11175            MOVE FDI-FEB (FML)   TO WS-MONTH-LIT6
069640010912                  MOVE FDI-FEB (FML)   TO WS-MONTH-LIT6-F
069650001130              WHEN 03
069660001218                  MOVE DI-MAR (SML) TO WS-MONTH-LIT1
069670001218                  MOVE DI-MAR (SML) TO WS-MONTH-LIT1-F
069680010912R11175            MOVE FDI-MAR (FML)   TO WS-MONTH-LIT6
069690010912                  MOVE FDI-MAR (FML)   TO WS-MONTH-LIT6-F
069700001130              WHEN 04
069710001218                  MOVE DI-APR (SML) TO WS-MONTH-LIT1
069720001218                  MOVE DI-APR (SML) TO WS-MONTH-LIT1-F
069730010912R11175            MOVE FDI-APR (FML)   TO WS-MONTH-LIT6
069740010912                  MOVE FDI-APR (FML)   TO WS-MONTH-LIT6-F
069750001130              WHEN 05
069760001218                  MOVE DI-MAY (SML) TO WS-MONTH-LIT1
069770001218                  MOVE DI-MAY (SML) TO WS-MONTH-LIT1-F
069780010912R11175            MOVE FDI-MAY (FML)   TO WS-MONTH-LIT6
069790010912                  MOVE FDI-MAY (FML)   TO WS-MONTH-LIT6-F
069800001130              WHEN 06
069810001218                  MOVE DI-JUN (SML) TO WS-MONTH-LIT1
069820001218                  MOVE DI-JUN (SML) TO WS-MONTH-LIT1-F
069830010912R11175            MOVE FDI-JUN (FML)   TO WS-MONTH-LIT6
069840010912                  MOVE FDI-JUN (FML)   TO WS-MONTH-LIT6-F
069850001130              WHEN 07
069860001218                  MOVE DI-JUL (SML) TO WS-MONTH-LIT1
069870001218                  MOVE DI-JUL (SML) TO WS-MONTH-LIT1-F
069880010912R11175            MOVE FDI-JUL (FML)   TO WS-MONTH-LIT6
069890010912                  MOVE FDI-JUL (FML)   TO WS-MONTH-LIT6-F
069900001130              WHEN 08
069910001218                  MOVE DI-AUG (SML) TO WS-MONTH-LIT1
069920001218                  MOVE DI-AUG (SML) TO WS-MONTH-LIT1-F
069930010912R11175            MOVE FDI-AUG (FML)   TO WS-MONTH-LIT6
069940010912                  MOVE FDI-AUG (FML)   TO WS-MONTH-LIT6-F
069950001130              WHEN 09
069960001218                  MOVE DI-SEP (SML) TO WS-MONTH-LIT1
069970001218                  MOVE DI-SEP (SML) TO WS-MONTH-LIT1-F
069980010912R11175            MOVE FDI-SEP (FML)   TO WS-MONTH-LIT6
069990010912                  MOVE FDI-SEP (FML)   TO WS-MONTH-LIT6-F
070000001130              WHEN 10
070010001218                  MOVE DI-OCT (SML) TO WS-MONTH-LIT1
070020001218                  MOVE DI-OCT (SML) TO WS-MONTH-LIT1-F
070030010912R11175            MOVE FDI-OCT (FML)   TO WS-MONTH-LIT6
070040010912                  MOVE FDI-OCT (FML)   TO WS-MONTH-LIT6-F
070050001130              WHEN 11
070060001218                  MOVE DI-NOV (SML) TO WS-MONTH-LIT1
070070001218                  MOVE DI-NOV (SML) TO WS-MONTH-LIT1-F
070080010912R11175            MOVE FDI-NOV (FML)   TO WS-MONTH-LIT6
070090010912                  MOVE FDI-NOV (FML)   TO WS-MONTH-LIT6-F
070100001130              WHEN 12
070110001218                  MOVE DI-DEC (SML) TO WS-MONTH-LIT1
070120001218                  MOVE DI-DEC (SML) TO WS-MONTH-LIT1-F
070130010912R11175            MOVE FDI-DEC (FML)   TO WS-MONTH-LIT6
070140010912                  MOVE FDI-DEC (FML)   TO WS-MONTH-LIT6-F
070150001130           END-EVALUATE.
070160001130
070170001130           MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT1-DD.
070180010911R11175     MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT6-DD.
070190010911R11175     MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT6-DD-F.
070200001130           MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT1-YY.
070210010911R11175     MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT6-YY.
070220010911R11175     MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT6-YY-F.
070230010911R11175     MOVE  WS-SHPL-HD-STDATE-CC  TO  WS-MONTH-LIT6-CC.
070240010911R11175     MOVE  WS-SHPL-HD-STDATE-CC  TO  WS-MONTH-LIT6-CC-F.
070250001218           MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT1-DD-F.
070260001218           MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT1-YY-F.
070270001218           MOVE  "/"                   TO  WS-MONTH-LIT1S.
070280001218           MOVE  "/"                   TO  WS-MONTH-LIT1S-F.
070290010911R11175     MOVE  ","                   TO  WS-MONTH-LIT6S.
070300001130
070310001130           EVALUATE WS-SHPL-HD-ENDATE-MM
070320001130              WHEN 01
070330001217                  MOVE DI-JAN  (SML)   TO WS-MONTH-LIT2
070340001218                  MOVE DI-JAN  (SML)   TO WS-MONTH-LIT2-F
070350001217                  MOVE FDI-JAN (FML)   TO WS-MONTH-LIT3
070360001218                  MOVE FDI-JAN (FML)   TO WS-MONTH-LIT3-F
070370001217              WHEN 02
070380001217                  MOVE DI-FEB  (SML)   TO WS-MONTH-LIT2
070390001218                  MOVE DI-FEB  (SML)   TO WS-MONTH-LIT2-F
070400001217                  MOVE FDI-FEB (FML)   TO WS-MONTH-LIT3
070410001218                  MOVE FDI-FEB (FML)   TO WS-MONTH-LIT3-F
070420001130              WHEN 03
070430001217                  MOVE DI-MAR  (SML)   TO WS-MONTH-LIT2
070440001218                  MOVE DI-MAR  (SML)   TO WS-MONTH-LIT2-F
070450001217                  MOVE FDI-MAR (FML)   TO WS-MONTH-LIT3
070460001218                  MOVE FDI-MAR (FML)   TO WS-MONTH-LIT3-F
070470001130              WHEN 04
070480001217                  MOVE DI-APR  (SML)   TO WS-MONTH-LIT2
070490001218                  MOVE DI-APR  (SML)   TO WS-MONTH-LIT2-F
070500001217                  MOVE FDI-APR (FML)   TO WS-MONTH-LIT3
070510001218                  MOVE FDI-APR (FML)   TO WS-MONTH-LIT3-F
070520001130              WHEN 05
070530001217                  MOVE DI-MAY  (SML)   TO WS-MONTH-LIT2
070540001218                  MOVE DI-MAY  (SML)   TO WS-MONTH-LIT2-F
070550001217                  MOVE FDI-MAY (FML)   TO WS-MONTH-LIT3
070560001218                  MOVE FDI-MAY (FML)   TO WS-MONTH-LIT3-F
070570001130              WHEN 06
070580001217                  MOVE DI-JUN  (SML)   TO WS-MONTH-LIT2
070590001218                  MOVE DI-JUN  (SML)   TO WS-MONTH-LIT2-F
070600001217                  MOVE FDI-JUN (FML)   TO WS-MONTH-LIT3
070610001218                  MOVE FDI-JUN (FML)   TO WS-MONTH-LIT3-F
070620001130              WHEN 07
070630001217                  MOVE DI-JUL  (SML)   TO WS-MONTH-LIT2
070640001218                  MOVE DI-JUL  (SML)   TO WS-MONTH-LIT2-F
070650001217                  MOVE FDI-JUL (FML)   TO WS-MONTH-LIT3
070660001218                  MOVE FDI-JUL (FML)   TO WS-MONTH-LIT3-F
070670001130              WHEN 08
070680001217                  MOVE DI-AUG  (SML)   TO WS-MONTH-LIT2
070690001218                  MOVE DI-AUG  (SML)   TO WS-MONTH-LIT2-F
070700001217                  MOVE FDI-AUG (FML)   TO WS-MONTH-LIT3
070710001218                  MOVE FDI-AUG (FML)   TO WS-MONTH-LIT3-F
070720001130              WHEN 09
070730001217                  MOVE DI-SEP  (SML)   TO WS-MONTH-LIT2
070740001218                  MOVE DI-SEP  (SML)   TO WS-MONTH-LIT2-F
070750001217                  MOVE FDI-SEP (FML)   TO WS-MONTH-LIT3
070760001218                  MOVE FDI-SEP (FML)   TO WS-MONTH-LIT3-F
070770001130              WHEN 10
070780001217                  MOVE DI-OCT  (SML)   TO WS-MONTH-LIT2
070790001218                  MOVE DI-OCT  (SML)   TO WS-MONTH-LIT2-F
070800001217                  MOVE FDI-OCT (FML)   TO WS-MONTH-LIT3
070810001218                  MOVE FDI-OCT (FML)   TO WS-MONTH-LIT3-F
070820001130              WHEN 11
070830001217                  MOVE DI-NOV  (SML)   TO WS-MONTH-LIT2
070840001218                  MOVE DI-NOV  (SML)   TO WS-MONTH-LIT2-F
070850001217                  MOVE FDI-NOV (FML)   TO WS-MONTH-LIT3
070860001218                  MOVE FDI-NOV (FML)   TO WS-MONTH-LIT3-F
070870001130              WHEN 12
070880001217                  MOVE DI-DEC  (SML)   TO WS-MONTH-LIT2
070890001218                  MOVE DI-DEC  (SML)   TO WS-MONTH-LIT2-F
070900001217                  MOVE FDI-DEC (FML)   TO WS-MONTH-LIT3
070910001218                  MOVE FDI-DEC (FML)   TO WS-MONTH-LIT3-F
070920001130           END-EVALUATE.
070930001130
070940001130           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT2-DD.
070950001218           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT3-DD.
070960001218           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT2-DD-F.
070970001218           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT3-DD-F.
070980001130           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT2-YY.
070990001218           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT3-YY.
071000001218           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT2-YY-F.
071010001218           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT3-YY-F.
071020001218           MOVE  WS-SHPL-HD-ENDATE-CC  TO  WS-MONTH-LIT3-CC.
071030001218           MOVE  WS-SHPL-HD-ENDATE-CC  TO  WS-MONTH-LIT3-CC-F.
071040001218           MOVE  "/"                   TO  WS-MONTH-LIT2S.
071050001218           MOVE  "/"                   TO  WS-MONTH-LIT2S-F.
071060001218           MOVE  ","                   TO  WS-MONTH-LIT3S.
071070001218
071080010430      ***
071090010430      ***    Process Issue-date.
071100010430      ***
071110010430
071120010430           EVALUATE WS-SHGL-HD-ISDATE-MM
071130010430              WHEN 01
071140010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT4
071150010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT4-F
071160010430              WHEN 02
071170010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT4
071180010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT4-F
071190010430              WHEN 03
071200010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT4
071210010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT4-F
071220010430              WHEN 04
071230010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT4
071240010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT4-F
071250010430              WHEN 05
071260010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT4
071270010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT4-F
071280010430              WHEN 06
071290010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT4
071300010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT4-F
071310010430              WHEN 07
071320010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT4
071330010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT4-F
071340010430              WHEN 08
071350010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT4
071360010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT4-F
071370010430              WHEN 09
071380010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT4
071390010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT4-F
071400010430              WHEN 10
071410010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT4
071420010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT4-F
071430010430              WHEN 11
071440010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT4
071450010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT4-F
071460010430              WHEN 12
071470010912                  MOVE FDIH-DEC (FMLH)   TO WS-MONTH-LIT4
071480010912                  MOVE FDIH-DEC (FMLH)  TO WS-MONTH-LIT4-F
071490010501              WHEN OTHER
071500010501                  MOVE SPACES          TO WS-MONTH-LIT4
071510010501                  MOVE SPACES          TO WS-MONTH-LIT4-F
071520010501                  MOVE SPACES          TO WS-MONTH-COL4
071530010501                  MOVE SPACES          TO WS-MONTH-COL4-F
071540010501                  GO TO LR1-0030-0005-0020
071550010430           END-EVALUATE.
071560010430
071570010501           MOVE  WS-SHGL-HD-ISDATE-DD  TO  WS-MONTH-LIT4-DD.
071580010501           MOVE  WS-SHGL-HD-ISDATE-DD  TO  WS-MONTH-LIT4-DD-F.
071590010501           MOVE  WS-SHGL-HD-ISDATE-YY  TO  WS-MONTH-LIT4-YY.
071600010501           MOVE  WS-SHGL-HD-ISDATE-YY  TO  WS-MONTH-LIT4-YY-F.
071610010501           MOVE  WS-SHGL-HD-ISDATE-CC  TO  WS-MONTH-LIT4-CC.
071620010501           MOVE  WS-SHGL-HD-ISDATE-CC  TO  WS-MONTH-LIT4-CC-F.
071630010430           MOVE  "/"                   TO  WS-MONTH-LIT4S.
071640010612           MOVE  " "                   TO  WS-MONTH-LIT4S-F.
071650010430           MOVE  ","                   TO  WS-MONTH-LIT4S.
071660010430
071670010430      ***
071680010430      ***    Process Annuity-date.
071690010430      ***
071700010430
071710010501       LR1-0030-0005-0020.
071720010501
071730010430           EVALUATE WS-SHGL-HD-ANDATE-MM
071740010430              WHEN 01
071750010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT5
071760010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT5-F
071770010430              WHEN 02
071780010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT5
071790010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT5-F
071800010430              WHEN 03
071810010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT5
071820010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT5-F
071830010430              WHEN 04
071840010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT5
071850010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT5-F
071860010430              WHEN 05
071870010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT5
071880010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT5-F
071890010430              WHEN 06
071900010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT5
071910010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT5-F
071920010430              WHEN 07
071930010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT5
071940010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT5-F
071950010430              WHEN 08
071960010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT5
071970010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT5-F
071980010430              WHEN 09
071990010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT5
072000010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT5-F
072010010430              WHEN 10
072020010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT5
072030010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT5-F
072040010430              WHEN 11
072050010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT5
072060010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT5-F
072070010430              WHEN 12
072080010912                  MOVE FDIH-DEC (FMLH)   TO WS-MONTH-LIT5
072090010912                  MOVE FDIH-DEC (FMLH)   TO WS-MONTH-LIT5-F
072100010501              WHEN OTHER
072110010501                  MOVE SPACES          TO WS-MONTH-LIT5
072120010501                  MOVE SPACES          TO WS-MONTH-LIT5-F
072130010501                  MOVE SPACES          TO WS-MONTH-COL5
072140010501                  MOVE SPACES          TO WS-MONTH-COL5-F
072150010501                  GO TO LR1-0030-0005-0030
072160010430           END-EVALUATE.
072170010430
072180010501           MOVE  WS-SHGL-HD-ANDATE-DD  TO  WS-MONTH-LIT5-DD.
072190010501           MOVE  WS-SHGL-HD-ANDATE-DD  TO  WS-MONTH-LIT5-DD-F.
072200010501           MOVE  WS-SHGL-HD-ANDATE-YY  TO  WS-MONTH-LIT5-YY.
072210010501           MOVE  WS-SHGL-HD-ANDATE-YY  TO  WS-MONTH-LIT5-YY-F.
072220010501           MOVE  WS-SHGL-HD-ANDATE-CC  TO  WS-MONTH-LIT5-CC.
072230010501           MOVE  WS-SHGL-HD-ANDATE-CC  TO  WS-MONTH-LIT5-CC-F.
072240010430           MOVE  "/"                   TO  WS-MONTH-LIT5S.
072250010612           MOVE  " "                   TO  WS-MONTH-LIT5S-F.
072260010430           MOVE  ","                   TO  WS-MONTH-LIT5S.
072270010430
072280000000      *****
072290000000      *****    The Statement File Generation (ANNSTMTJSI) formats and
072300000000      *****    creates the Dealer record using the Dealer & Dealer
072310000000      *****    Representative of the first account for the investor.  If
072320000000      *****    an investor has more than one account, the Dealer &
072330000000      *****    Dealer Representative of the accounts may differ.  If
072340000000      *****    this is the case, the Dealer & Dealer Representative
072350000000      *****    names and address lines will be retrieved and formatted.
072360000000      *****
072370000000
072380010501       LR1-0030-0005-0030.
072390010501
072400000000           MOVE SPACES                 TO  WS-BRANCH-ADDR1.
072410000000           MOVE SPACES                 TO  WS-BRANCH-ADDR2.
072420000000           MOVE SPACES                 TO  WS-BRANCH-ADDR3.
072430000000           MOVE SPACES                 TO  WS-BRANCH-ADDR4.
072440000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
072450000000           MOVE SPACES                 TO  WS-DEALER-REP-NAMES.
072460000000           MOVE ZEROES                 TO  WS-FAX.
072470000000           MOVE ZEROES                 TO  WS-PHONE.
072480000000           MOVE SPACES                 TO  WS-REP-NAME.
072490990115           MOVE SPACES                 TO  WS-REP-NAME-1.
072500000000
072510000000           IF  AD-SALESREP    IS EQUAL TO  DL-SALESREP
072520000000           AND AD-DEALER      IS EQUAL TO  DL-DEALER
072530000000               MOVE DL-BRANCH-ADDR1    TO  WS-BRANCH-ADDR1
072540000000               MOVE DL-BRANCH-ADDR2    TO  WS-BRANCH-ADDR2
072550000000               MOVE DL-BRANCH-ADDR3    TO  WS-BRANCH-ADDR3
072560000000               MOVE DL-BRANCH-ADDR4    TO  WS-BRANCH-ADDR4
072570000000               STRING DL-DEALER-NAME
072580000000                             DELIMITED BY  "  ",
072590000000                      " /"   DELIMITED BY  SIZE,
072600000000                      DL-REP-NAME
072610000000                             DELIMITED BY  SIZE
072620000000                                     INTO  WS-DEALER-REP-NAMES
072630000000               MOVE DL-FAX             TO  WS-FAX
072640000000               MOVE DL-PHONE           TO  WS-PHONE
072650990113               MOVE DL-REP-NAME        TO  WS-REP-NAME-1
072660000000               GO TO LR1-0030-0090.
072670000000
072680000000      *****
072690000000      *****    The account Dealer & Dealer Representative are different
072700000000      *****    than the dealer.  The following steps will be executed to
072710000000      *****    retrieve and format the Dealer & Dealer Representative
072720000000      *****    names and address lines.
072730000000      *****       1. Read the Dealer file for the name.
072740000000      *****       2. Read the Dealer Representative for the name, fax
072750000000      *****          and phone number.
072760000000      *****       3. Read the Dealer Representative Address for the
072770000000      *****          address lines.
072780000000      *****       4. If required, read the Institution Branch and
072790000000      *****          Institution Address for the address lines.
072800000000      *****       5. Format the address lines field by field, in case
072810000000      *****          one or more of the fields contain spaces.
072820000000      *****       6. Compress the address lines.
072830000000      *****
072840000000
072850000000           INITIALIZE MFADEAP.
072860000000           MOVE AD-DEALER              TO  DEALER-CODE
072870000000                                           OF DEALER-RECD.
072880000000           READ DEALER-FILE
072890000000               INVALID KEY
072900000000                   MOVE "DEALER NAME NOT ON FILE            "
072910000000                                       TO  DEALER-NAME
072920000000                                           OF DEALER-RECD.
072930000000
072940000000           MOVE DEALER-NAME
072950000000                OF DEALER-RECD         TO  WS-DEALER-NAME.
072960000000
072970000000           INITIALIZE MFADEAREP.
072980000000           MOVE AD-DEALER              TO  DEALER-CODE
072990000000                                           OF DEALER-REP-RECD.
073000000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
073010000000                                           OF DEALER-REP-RECD.
073020000000           READ DEALER-REP-FILE
073030000000               INVALID KEY
073040000000                   INITIALIZE MFADEAREP.
073050000000
073060000000           MOVE SPACES                 TO  WS-ADRTMP.
073070000000           STRING SALUTATION-CODE
073080000000                  OF DEALER-REP-RECD
073090000000                             DELIMITED BY  " "
073100000000                                     INTO  WS-ADRTMP.
073110000000
073120000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
073130000000               STRING FIRST-NAME
073140000000                      OF DEALER-REP-RECD
073150000000                             DELIMITED BY  "  "
073160000000                                     INTO  WS-ADRTMP
073170000000             ELSE
073180000000               STRING WS-ADRTMP
073190000000                             DELIMITED BY  "  ",
073200000000                      " "    DELIMITED BY  SIZE,
073210000000                      FIRST-NAME
073220000000                      OF DEALER-REP-RECD
073230000000                             DELIMITED BY  "  "
073240000000                                     INTO  WS-REP-NAME
073250000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
073260000000
073270000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
073280000000               STRING INITIALS
073290000000                      OF DEALER-REP-RECD
073300000000                             DELIMITED BY  "  "
073310000000                                     INTO  WS-ADRTMP
073320000000             ELSE
073330000000               STRING WS-ADRTMP
073340000000                             DELIMITED BY  "  ",
073350000000                      " "    DELIMITED BY  SIZE,
073360000000                      INITIALS
073370000000                      OF DEALER-REP-RECD
073380000000                             DELIMITED BY  "  "
073390000000                                     INTO  WS-REP-NAME
073400000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
073410000000
073420000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
073430000000               STRING LAST-NAME
073440000000                      OF DEALER-REP-RECD
073450000000                             DELIMITED BY  "  "
073460000000                                     INTO  WS-ADRTMP
073470000000             ELSE
073480000000               STRING WS-ADRTMP
073490000000                             DELIMITED BY  "  ",
073500000000                      " "    DELIMITED BY  SIZE,
073510000000                      LAST-NAME
073520000000                      OF DEALER-REP-RECD
073530000000                             DELIMITED BY  "  "
073540000000                                     INTO  WS-REP-NAME
073550000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
073560000000
073570000000           MOVE WS-ADRTMP              TO  WS-REP-NAME.
073580000000
073590000000           IF  ADDR-NO
073600000000               OF DEALER-REP-RECD
073610000000                              IS EQUAL TO  ZEROES
073620000000               GO TO LR1-0030-0010.
073630000000
073640000000           INITIALIZE MFADRADRP.
073650000000           MOVE ADDR-NO
073660000000                OF DEALER-REP-RECD     TO  ADDR-NO
073670000000                                           OF DEALER-REP-ADDRESS-RECD.
073680000000            READ DEALER-REP-ADDRESS
073690000000                INVALID KEY
073700000000                    GO TO LR1-0030-0010.
073710000000
073720000000            MOVE CORR MFADRADRP        TO  MFAINADDP.
073730000000            GO TO LR1-0030-0030.
073740000000
073750000000       LR1-0030-0010.
073760990927
073770000000           INITIALIZE MFAINSBRP.
073780000000           MOVE INSTITUTION-CODE
073790000000                OF DEALER-RECD         TO  INSTITUTION-CODE
073800000000                                           OF INSTITUTION-BRANCH-RECD.
073810000000           MOVE BRANCH-CODE
073820000000                OF DEALER-REP-RECD     TO  BRANCH-CODE
073830000000                                           OF INSTITUTION-BRANCH-RECD.
073840000000           READ INSTITUTION-BRANCH
073850000000               INVALID KEY
073860000000                   GO TO LR1-0030-0020.
073870000000
073880000000           INITIALIZE MFAINADDP.
073890000000           MOVE ADDR-NO
073900000000                OF INSTITUTION-BRANCH-RECD
073910000000                                       TO  ADDR-NO
073920000000                                           OF INSTITUTION-ADDRESS-RECD.
073930000000           READ INSTITUTION-ADDRESS
073940000000               INVALID KEY
073950000000                   GO TO LR1-0030-0020.
073960000000
073970000000           GO TO LR1-0030-0030.
073980000000
073990000000       LR1-0030-0020.
074000990923
074010000000           INITIALIZE MFAINSBRP.
074020000000           MOVE INSTITUTION-CODE
074030000000                OF DEALER-RECD         TO  INSTITUTION-CODE
074040000000                                           OF INSTITUTION-BRANCH-RECD.
074050000000           MOVE BRANCH-CODE
074060000000                OF DEALER-RECD         TO  BRANCH-CODE
074070000000                                           OF INSTITUTION-BRANCH-RECD.
074080000000           READ INSTITUTION-BRANCH
074090000000               INVALID KEY
074100000000                   INITIALIZE MFAINADDP
074110000000                   GO TO LR1-0030-0030.
074120000000
074130000000           INITIALIZE MFAINADDP.
074140000000           MOVE ADDR-NO
074150000000                OF INSTITUTION-BRANCH-RECD
074160000000                                       TO  ADDR-NO
074170000000                                           OF INSTITUTION-ADDRESS-RECD.
074180000000           READ INSTITUTION-ADDRESS
074190000000               INVALID KEY
074200000000                   INITIALIZE MFAINADDP
074210000000                   GO TO LR1-0030-0030.
074220000000
074230000000       LR1-0030-0030.
074240990927
074250000000           MOVE SPACES                 TO  WS-ADRTMP.
074260000000           STRING STREET-NO
074270000000               OF INSTITUTION-ADDRESS
074280000000                             DELIMITED BY  "  "
074290000000                                     INTO  WS-ADRTMP.
074300000000
074310000000           IF  PROVINCE-CODE
074320000000               OF INSTITUTION-ADDRESS-RECD
074330000000                              IS EQUAL TO  "QC"
074340000000               GO TO LR1-0030-0030-0010.
074350000000
074360000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
074370000000               STRING STREET-NAME
074380000000                      OF INSTITUTION-ADDRESS
074390000000                             DELIMITED BY  "  "
074400000000                                     INTO  WS-ADRTMP
074410000000             ELSE
074420000000               STRING WS-ADRTMP
074430000000                             DELIMITED BY  "  ",
074440000000                      " "    DELIMITED BY  SIZE,
074450000000                      STREET-NAME
074460000000                      OF INSTITUTION-ADDRESS
074470000000                             DELIMITED BY  "  "
074480000000                                     INTO  WS-BRANCH-ADDR1
074490000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
074500000000
074510000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
074520000000               STRING STREET-TYPE-CODE
074530000000                      OF INSTITUTION-ADDRESS
074540000000                             DELIMITED BY  "  "
074550000000                                     INTO  WS-ADRTMP
074560000000             ELSE
074570000000               STRING WS-ADRTMP
074580000000                             DELIMITED BY  "  ",
074590000000                      " "    DELIMITED BY  SIZE,
074600000000                      STREET-TYPE-CODE
074610000000                      OF INSTITUTION-ADDRESS
074620000000                             DELIMITED BY  "  "
074630000000                                     INTO  WS-BRANCH-ADDR1
074640000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
074650000000
074660000000           GO TO LR1-0030-0030-0020.
074670000000
074680000000       LR1-0030-0030-0010.
074690990927
074700000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
074710000000               STRING STREET-TYPE-CODE
074720000000                      OF INSTITUTION-ADDRESS-RECD
074730000000                             DELIMITED BY  "  "
074740000000                                     INTO  WS-ADRTMP
074750000000             ELSE
074760000000               STRING WS-ADRTMP
074770000000                             DELIMITED BY  "  ",
074780000000                      " "    DELIMITED BY  SIZE,
074790000000                      STREET-TYPE-CODE
074800000000                      OF INSTITUTION-ADDRESS-RECD
074810000000                             DELIMITED BY  "  "
074820000000                                     INTO  WS-BRANCH-ADDR1
074830000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
074840000000
074850000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
074860000000               STRING STREET-NAME
074870000000                      OF INSTITUTION-ADDRESS-RECD
074880000000                             DELIMITED BY  "  "
074890000000                                     INTO  WS-ADRTMP
074900000000             ELSE
074910000000               STRING WS-ADRTMP
074920000000                             DELIMITED BY  "  ",
074930000000                      " "    DELIMITED BY  SIZE,
074940000000                      STREET-NAME
074950000000                      OF INSTITUTION-ADDRESS-RECD
074960000000                             DELIMITED BY  "  "
074970000000                                     INTO  WS-BRANCH-ADDR1
074980000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
074990000000
075000000000       LR1-0030-0030-0020.
075010990927
075020000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
075030000000               STRING STREET-DIRCT-CODE
075040000000                      OF INSTITUTION-ADDRESS
075050000000                             DELIMITED BY  "  "
075060000000                                     INTO  WS-ADRTMP
075070000000             ELSE
075080000000               STRING WS-ADRTMP
075090000000                             DELIMITED BY  "  ",
075100000000                      " "    DELIMITED BY  SIZE,
075110000000                      STREET-DIRCT-CODE
075120990922                      OF INSTITUTION-ADDRESS
075130000000                             DELIMITED BY  "  "
075140000000                                     INTO  WS-BRANCH-ADDR1
075150000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
075160000000
075170000000           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR1.
075180000000
075190000000           MOVE MISC-FIRST-ADDR-LINE
075200000000                OF INSTITUTION-ADDRESS-RECD
075210000000                                       TO  WS-BRANCH-ADDR2.
075220000000           MOVE MISC-SECOND-ADDR-LINE
075230000000                OF INSTITUTION-ADDRESS-RECD
075240000000                                       TO  WS-BRANCH-ADDR3.
075250000000           MOVE CO-OR-ATTENTION
075260000000                OF INSTITUTION-ADDRESS-RECD
075270000000                                       TO  WS-BRANCH-ADDR4.
075280000000
075290980420980420     IF  WS-LDA-ATTN-LINE-PRT = "N"
075300980420980420         MOVE SPACES TO WS-BRANCH-ADDR4.
075310980420
075320000000           MOVE POSTAL-CODE
075330000000                OF INSTITUTION-ADDRESS TO  WS-POSTAL.
075340000000           MOVE SPACES                 TO  WS-POSTAL-OUT.
075350000000           IF  COUNTRY-CODE
075360000000               OF INSTITUTION-ADDRESS
075370000000                              IS EQUAL TO  "CAN"
075380000000               MOVE WS-POSTAL1         TO  WS-POSTAL-OUT1
075390000000               MOVE WS-POSTAL2         TO  WS-POSTAL-OUT2
075400000000             ELSE
075410000000               MOVE WS-POSTAL          TO  WS-POSTAL-OUT.
075420000000
075430000000           MOVE SPACES                 TO  WS-ADRTMP.
075440000000           STRING CITY
075450000000               OF INSTITUTION-ADDRESS
075460000000                             DELIMITED BY  "  "
075470000000                                     INTO  WS-ADRTMP.
075480000000
075490000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
075500000000               STRING PROVINCE-CODE
075510000000                      OF INSTITUTION-ADDRESS
075520000000                             DELIMITED BY  "  "
075530000000                                     INTO  WS-ADRTMP
075540000000               GO TO LR1-0030-0040.
075550000000
075560000000           IF  PROVINCE-CODE
075570000000               OF INSTITUTION-ADDRESS
075580000000                              IS EQUAL TO  SPACES
075590000000               GO TO LR1-0030-0040.
075600000000
075610000000           STRING WS-ADRTMP  DELIMITED BY  "  ",
075620000000                  ", "       DELIMITED BY  SIZE,
075630000000                  PROVINCE-CODE
075640000000                  OF INSTITUTION-ADDRESS
075650000000                             DELIMITED BY  "  "
075660000000                                     INTO  WS-BRANCH-ADDR5.
075670000000           MOVE WS-BRANCH-ADDR5        TO  WS-ADRTMP.
075680000000
075690000000       LR1-0030-0040.
075700000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
075710000000               STRING WS-POSTAL-OUT
075720000000                             DELIMITED BY  "  "
075730000000                                     INTO  WS-ADRTMP
075740000000             ELSE
075750000000               STRING WS-ADRTMP
075760000000                             DELIMITED BY  "  ",
075770000000                      "  "   DELIMITED BY  SIZE,
075780000000                      WS-POSTAL-OUT
075790000000                             DELIMITED BY  "  "
075800000000                                     INTO  WS-BRANCH-ADDR5
075810000000               MOVE WS-BRANCH-ADDR5    TO  WS-ADRTMP.
075820000000
075830000000           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR5.
075840000000
075850000000           IF  WS-BRANCH-ADDR1
075860000000                          IS NOT EQUAL TO  SPACES
075870000000               GO TO LR1-0030-0050.
075880000000
075890000000           IF  WS-BRANCH-ADDR2
075900000000                          IS NOT EQUAL TO  SPACES
075910000000               MOVE WS-BRANCH-ADDR2    TO  WS-BRANCH-ADDR1
075920000000               MOVE SPACES             TO  WS-BRANCH-ADDR2
075930000000               GO TO LR1-0030-0050.
075940000000
075950000000           IF  WS-BRANCH-ADDR3
075960000000                          IS NOT EQUAL TO  SPACES
075970000000               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR1
075980000000               MOVE SPACES             TO  WS-BRANCH-ADDR3
075990000000               GO TO LR1-0030-0050.
076000000000
076010000000           IF  WS-BRANCH-ADDR4
076020000000                          IS NOT EQUAL TO  SPACES
076030000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR1
076040000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
076050000000               GO TO LR1-0030-0050.
076060000000
076070000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR1.
076080000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
076090000000           GO TO LR1-0030-0080.
076100000000
076110000000       LR1-0030-0050.
076120000000           IF  WS-BRANCH-ADDR2
076130000000                          IS NOT EQUAL TO  SPACES
076140000000               GO TO LR1-0030-0060.
076150000000
076160000000           IF  WS-BRANCH-ADDR3
076170000000                          IS NOT EQUAL TO  SPACES
076180000000               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR2
076190000000               MOVE SPACES             TO  WS-BRANCH-ADDR3
076200000000               GO TO LR1-0030-0060.
076210000000
076220000000           IF  WS-BRANCH-ADDR4
076230000000                          IS NOT EQUAL TO  SPACES
076240000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR2
076250000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
076260000000               GO TO LR1-0030-0060.
076270000000
076280000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR2.
076290000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
076300000000           GO TO LR1-0030-0080.
076310000000
076320000000       LR1-0030-0060.
076330990922
076340000000           IF  WS-BRANCH-ADDR3
076350000000                          IS NOT EQUAL TO  SPACES
076360000000               GO TO LR1-0030-0070.
076370000000
076380000000           IF  WS-BRANCH-ADDR4
076390000000                          IS NOT EQUAL TO  SPACES
076400000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR3
076410000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
076420000000               GO TO LR1-0030-0070.
076430000000
076440000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR3.
076450000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
076460000000           GO TO LR1-0030-0080.
076470000000
076480000000       LR1-0030-0070.
076490000000           IF  WS-BRANCH-ADDR4
076500000000                          IS NOT EQUAL TO  SPACES
076510000000               GO TO LR1-0030-0080.
076520000000
076530000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR4.
076540000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
076550000000
076560000000       LR1-0030-0080.
076570000000           STRING WS-DEALER-NAME
076580000000                             DELIMITED BY  "  ",
076590000000                  " /"       DELIMITED BY  SIZE,
076600000000                  WS-REP-NAME
076610000000                             DELIMITED BY  SIZE
076620000000                                     INTO  WS-DEALER-REP-NAMES.
076630990113
076640990113           MOVE WS-REP-NAME            TO  WS-REP-NAME-1.
076650000000           MOVE FAX-AREA
076660000000                OF DEALER-REP-RECD     TO  WS-SH-FAX-01-03.
076670000000           MOVE FAX-PHONE
076680000000                OF DEALER-REP-RECD     TO  WS-SH-FAX-04-10.
076690000000
076700000000           MOVE AREA-CODE
076710000000                OF DEALER-REP-RECD     TO  WS-SH-PHONE-01-03.
076720000000           MOVE PHONE-NO
076730000000                OF DEALER-REP-RECD     TO  WS-SH-PHONE-04-10.
076740000000
076750000000       LR1-0030-0090.
076760981230
076770000000      *****
076780000000      *****    Format the account type description.
076790000000      *****
076800000000
076810000000           MOVE AD-GROUP               TO  WS-ACTTYPGRP.
076820000000           MOVE AD-ACCOUNT-TYPE        TO  WS-ACTTYPIN-PFX.
076830000000           MOVE AD-LOCKED-IN           TO  WS-ACTTYPLCK.
076840000000           MOVE AD-LANGUAGE            TO  WS-ACTTYPLNG.
076850000000           MOVE AD-NOMINEE             TO  WS-ACTTYPNOM.
076860190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  WS-ACTTYPSHT.
076870190619183449     MOVE Lc-Acct-Type-Code      TO  WS-ACTTYPSHT.
076880000000           MOVE AD-SPOUSAL             TO  WS-ACTTYPSPS.
076890990923
076900001124           PERFORM FORMAT-ACTTYP-DESC THRU FAD-EXIT.
076910990923
076920000000      *****
076930000000      *****    Initialize the statement for the new account.
076940000000      *****
076950000000
076960000000           MOVE SPACES                 TO  WS-STATEMENT-PAGE.
076970000000           MOVE 1                      TO  WS-SH-PGENO.
076980000000
076990000000      *****
077000000000      *****    Format statement heading line 1.
077010000000      *****
077020000000
077030000000           MOVE SPACES                 TO  STMT-HDG-01.
077040991213
077050001219RF6007     IF  WS-STMTVRSN    IS EQUAL TO  9
077060001127               MOVE WS-ACTTYPOUT       TO  SH01V8-ACCOUNT-TYPE-DESC
077070991213               MOVE WS-SH01-LIT1 (SL)  TO  SH01V8-LIT1
077080991213               MOVE WS-SH-START-DATE   TO  SH01V8-START-DATE
077090991213               MOVE WS-SH01-LIT2 (SL)  TO  SH01V8-LIT2
077100991213               MOVE WS-SH-END-DATE     TO  SH01V8-END-DATE
077110991213               GO TO LR1-0030-0100
077120991213           END-IF
077130991213
077140991019
077150001127           MOVE WS-ACTTYPOUT           TO  SH01-ACCOUNT-TYPE-DESC.
077160991019           MOVE WS-SH01-LIT1 (SL)      TO  SH01-LIT1.
077170991019           MOVE WS-SH-START-DATE       TO  SH01-START-DATE.
077180991019           MOVE WS-SH01-LIT2 (SL)      TO  SH01-LIT2.
077190991019           MOVE WS-SH-END-DATE         TO  SH01-END-DATE.
077200991019
077210000000       LR1-0030-0100.
077220990923
077230000000           SET SPL                     TO  WS-SPL-HDGFST.
077240000000           MOVE STMT-HDG-01            TO  WS-STMTPGE-LNE (SPL).
077250000000
077260000000      *****
077270000000      *****    Format statement heading line 2.
077280000000      *****
077290000000
077300000000           MOVE II-SIN-NO              TO  WS-SIN-NO.
077310000000           MOVE SPACES                 TO  WS-SH02-SIN-NO.
077320980206
077330980417980417     IF WS-LDA-SIN NOT = "N"
077340980206             MOVE WS-SIN-NO-1-3          TO  SH02-SIN-NO-1-3
077350980206             MOVE "-"                    TO  SH02-SIN-NO-LITDSH1
077360980206             MOVE WS-SIN-NO-4-6          TO  SH02-SIN-NO-4-6
077370980206             MOVE "-"                    TO  SH02-SIN-NO-LITDSH2
077380980206             MOVE WS-SIN-NO-7-9          TO  SH02-SIN-NO-7-9.
077390000000
077400000000           MOVE SPACES                 TO  STMT-HDG-02.
077410000000
077420001219           IF  WS-STMTVRSN    IS EQUAL TO  9
077430991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
077440020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
077450020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V8-ACCOUNT
077460020613R13756       ELSE
077470020613               MOVE AD-ACCOUNT         TO  SH02V8-ACCOUNT
077480020613R13756       END-IF
077490991213               MOVE WS-SH02-SIN-NO     TO  SH02V8-SIN-NO
077500991213               MOVE AD-DEALER          TO  SH02V8-DEALER
077510020613               MOVE "-"                TO  SH02V8-LITDSH
077520991213               MOVE AD-SALESREP        TO  SH02V8-SALESREP
077530991213               MOVE WS-SH-END-DATE     TO  SH02V8-END-DATE
077540991213               MOVE AD-SPOUSAL-CONTRIB TO  SH02V8-SPOUSAL-CONTRIB
077550991213               MOVE WS-REG-CTB-CUR     TO  SH02V8-REG-CTB-CUR
077560991213               MOVE WS-REG-CTB-CUR-SIGN
077570991213                                       TO  SH02V8-REG-CTB-CUR-SIGN
077580001127
077590991213               MOVE WS-SH-PGENO        TO  SH02V8-PGENO
077600991213               SET SPL              UP BY  3
077610991213               GO TO LR1-0030-0110
077620001127
077630991213           END-IF
077640991213
077650001219           IF  WS-STMTVRSN    IS EQUAL TO  9
077660991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
077670020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
077680020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH022V8-ACCOUNT
077690020613R13756       ELSE
077700020613               MOVE AD-ACCOUNT         TO  SH022V8-ACCOUNT
077710020613R13756       END-IF
077720991213               MOVE AD-DEALER          TO  SH022V8-DEALER
077730020613               MOVE "-"                TO  SH022V8-LITDSH
077740991213               MOVE AD-SALESREP        TO  SH022V8-SALESREP
077750991213               MOVE WS-SH-END-DATE     TO  SH022V8-END-DATE
077760991213               MOVE AD-SPOUSAL-CONTRIB TO  SH022V8-SPOUSAL-CONTRIB
077770991213               MOVE WS-REG-CTB-CUR     TO  SH022V8-REG-CTB-CUR
077780991213               MOVE WS-REG-CTB-CUR-SIGN
077790991213                                       TO  SH022V8-REG-CTB-CUR-SIGN
077800001127
077810991213               MOVE WS-SH-PGENO        TO  SH022V8-PGENO
077820991213               SET SPL              UP BY  3
077830991213               GO TO LR1-0030-0110
077840001127
077850991213           END-IF
077860991213
077870991019
077880020921R13756     IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
077890020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02-ACCOUNT
077900020613R13756     ELSE
077910020613               MOVE AD-ACCOUNT             TO  SH02-ACCOUNT
077920020613R13756     END-IF
077930000000           MOVE WS-SH02-SIN-NO         TO  SH02-SIN-NO.
077940000000           MOVE AD-DEALER              TO  SH02-DEALER.
077950020613           MOVE "-"                    TO  SH02-LITDSH.
077960000000           MOVE AD-SALESREP            TO  SH02-SALESREP.
077970000000           MOVE WS-SH-END-DATE         TO  SH02-END-DATE.
077980000000           MOVE AD-SPOUSAL-CONTRIB     TO  SH02-SPOUSAL-CONTRIB.
077990000000           MOVE WS-REG-CTB-CUR         TO  SH02-REG-CTB-CUR.
078000000000           MOVE WS-REG-CTB-CUR-SIGN    TO  SH02-REG-CTB-CUR-SIGN.
078010000000           MOVE WS-SH-PGENO            TO  SH02-PGENO.
078020000000           SET SPL                  UP BY  3.
078030000000
078040000000       LR1-0030-0110.
078050990927
078060001124           MOVE STMT-HDG-02            TO  WS-STMTPGE-LNE (SPL)
078070990927
078080990927           SET WS-SPL-SH02             TO  SPL.
078090000000
078100000000      *****
078110000000      *****    Format statement heading line 3.
078120000000      *****
078130000000
078140000000           MOVE SPACES                 TO  STMT-HDG-03.
078150991213
078160001219           IF  WS-STMTVRSN    IS EQUAL TO  9
078170010419RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
078180010419                  MOVE WS-NOMINEE-NAME  TO  SH03V8-INVESTOR-NAME
078190010419               ELSE
078200010419                  MOVE II-INVESTOR-NAME   TO  SH03V8-INVESTOR-NAME
078210010419RF9415         END-IF
078220991213               MOVE WS-DEALER-REP-NAMES
078230991213                                       TO  SH03V8-DEALER-REP-NAMES
078240991213
078250991213               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
078260991213                  MOVE SPACES          TO  SH03V8-DEALER-REP-NAMES
078270991213                  MOVE WS-REP-NAME-1   TO  SH03V8-DEALER-REP-NAMES
078280991213               END-IF
078290991213
078300010427R9415A         MOVE WS-SH03-LIT1 (SL)  TO  SH03V8-LIT1
078310991213               SET SPL              UP BY  3
078320991213               GO TO LR1-0030-0120
078330991213           END-IF
078340991019
078350010427R9415A     MOVE WS-SH03-LIT1 (SL)      TO  SH03-LIT1.
078360990923           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
078370990923
078380990923           IF  II-INVESTOR-ADDRESS1    IS EQUAL TO  SPACES
078390990923           AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
078400990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
078410990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
078420990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
078430990923               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
078440990923               MOVE "Y"                TO  WS-PSTCDEPRT-SW
078450990923           ELSE
078460990923               MOVE II-INVESTOR-ADDRESS1
078470990923                                       TO  WS-SH04-INVESTOR-ADDRESS1
078480990923           END-IF
078490990923
078500010419RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
078510010419              MOVE WS-NOMINEE-NAME  TO  SH03-INVESTOR-NAME
078520010419           ELSE
078530010419              MOVE II-INVESTOR-NAME       TO  SH03-INVESTOR-NAME
078540010419RF9415     END-IF
078550010419      *    MOVE II-INVESTOR-NAME       TO  SH03-INVESTOR-NAME.
078560000000           MOVE WS-DEALER-REP-NAMES    TO  SH03-DEALER-REP-NAMES.
078570001127
078580991014           IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
078590991014              MOVE SPACES          TO  SH03-DEALER-REP-NAMES
078600991014              MOVE WS-REP-NAME-1   TO  SH03-DEALER-REP-NAMES
078610991014           END-IF.
078620991014
078630000000           SET SPL                  UP BY  3.
078640000000
078650000000       LR1-0030-0120.
078660990927
078670000000           MOVE STMT-HDG-03            TO  WS-STMTPGE-LNE (SPL).
078680990927
078690000000      *****
078700000000      *****    Format statement heading line 4.
078710000000      *****
078720000000
078730000000           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
078740990926
078750000000           IF  II-INVESTOR-ADDRESS1
078760000000                              IS EQUAL TO  SPACES
078770000000           AND II-INVESTOR-ADDRESS2
078780000000                              IS EQUAL TO  SPACES
078790000000           AND II-INVESTOR-ADDRESS3
078800000000                              IS EQUAL TO  SPACES
078810000000           AND II-INVESTOR-ADDRESS4
078820000000                              IS EQUAL TO  SPACES
078830000000           AND II-INVESTOR-ADDRESS5
078840000000                              IS EQUAL TO  SPACES
078850000000               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
078860000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
078870990922           ELSE
078880000000               MOVE II-INVESTOR-ADDRESS1
078890990922                                       TO  WS-SH04-INVESTOR-ADDRESS1
078900990922           END-IF
078910990922
078920980417980417     IF  WS-LDA-BRANCH-ADDRESS
078930980417980417                        IS EQUAL TO  "N"
078940000000               MOVE SPACES             TO  WS-BRANCH-ADDR1.
078950000000
078960000000           MOVE SPACES                 TO  STMT-HDG-04.
078970000000
078980001219           IF  WS-STMTVRSN    IS EQUAL TO  9
078990010419RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
079000010419                  MOVE DL-NOMINEE-ADDR1 TO SH04V8-INVESTOR-ADDRESS1
079010010419               ELSE
079020010419                  MOVE WS-SH04-INVESTOR-ADDRESS1
079030010419                                       TO  SH04V8-INVESTOR-ADDRESS1
079040010419               END-IF
079050010419RF9415*        MOVE WS-SH04-INVESTOR-ADDRESS1
079060010419      *                                TO  SH04V8-INVESTOR-ADDRESS1
079070991213               MOVE WS-BRANCH-ADDR1    TO  SH04V8-BRANCH-ADDR1
079080991213
079090991213               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
079100991213                  MOVE SPACES TO SH04V8-BRANCH-ADDR1
079110991213               END-IF
079120991213
079130991213               GO TO LR1-0030-0130
079140991213           END-IF
079150991213
079160990923           MOVE SPACES                 TO  WS-SH05-INVESTOR-ADDRESS2.
079170990923
079180990923           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
079190990923           AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
079200990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
079210990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
079220990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
079230990923               MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
079240990923               MOVE "Y"                TO  WS-PSTCDEPRT-SW
079250990923           ELSE
079260990923               MOVE II-INVESTOR-ADDRESS2
079270990923                                       TO  WS-SH05-INVESTOR-ADDRESS2
079280990923           END-IF
079290990922
079300010419RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
079310010419              MOVE DL-NOMINEE-ADDR1 TO SH04-INVESTOR-ADDRESS1
079320010419           ELSE
079330010419              MOVE WS-SH04-INVESTOR-ADDRESS1
079340010419                                    TO  SH04-INVESTOR-ADDRESS1
079350010419RF9415     END-IF
079360010419      *    MOVE WS-SH04-INVESTOR-ADDRESS1
079370010419      *                                TO  SH04-INVESTOR-ADDRESS1.
079380000000           MOVE WS-BRANCH-ADDR1        TO  SH04-BRANCH-ADDR1.
079390000000
079400000000       LR1-0030-0130.
079410990923
079420990923           SET SPL                     UP BY  1.
079430000000           MOVE STMT-HDG-04            TO  WS-STMTPGE-LNE (SPL).
079440000000
079450000000      *****
079460000000      *****    Format statement heading line 5.
079470000000      *****
079480000000
079490000000           MOVE SPACES                 TO  WS-SH05-INVESTOR-ADDRESS2.
079500990926
079510000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
079520000000           AND II-INVESTOR-ADDRESS2
079530000000                              IS EQUAL TO  SPACES
079540000000           AND II-INVESTOR-ADDRESS3
079550000000                              IS EQUAL TO  SPACES
079560000000           AND II-INVESTOR-ADDRESS4
079570000000                              IS EQUAL TO  SPACES
079580000000           AND II-INVESTOR-ADDRESS5
079590000000                              IS EQUAL TO  SPACES
079600000000               MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
079610000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
079620990922           ELSE
079630000000               MOVE II-INVESTOR-ADDRESS2
079640990922                                       TO  WS-SH05-INVESTOR-ADDRESS2
079650990922           END-IF
079660990922
079670990923980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO  "N"
079680990923               MOVE SPACES             TO  WS-BRANCH-ADDR2
079690990923           END-IF
079700990923
079710000000           MOVE SPACES                 TO  STMT-HDG-05.
079720000000
079730001219           IF  WS-STMTVRSN    IS EQUAL TO  9
079740010419RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
079750010419                  MOVE DL-NOMINEE-ADDR2    TO SH05V8-INVESTOR-ADDRESS2
079760010419               ELSE
079770010419                  MOVE WS-SH05-INVESTOR-ADDRESS2
079780010419                                          TO  SH05V8-INVESTOR-ADDRESS2
079790010419RF9415         END-IF
079800010419
079810010419      *        MOVE WS-SH05-INVESTOR-ADDRESS2
079820010419      *                                TO  SH05V8-INVESTOR-ADDRESS2
079830010501      *        MOVE WS-BRANCH-ADDR2    TO  SH05V8-BRANCH-ADDR2
079840991213
079850991213               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
079860991213                  MOVE SPACES TO SH05V8-BRANCH-ADDR2
079870991213               END-IF
079880010427
079890010430RF9415         MOVE WS-SH05-LIT1 (SL)        TO  SH05V8-LIT1
079900010501               IF WS-STMT-DTA-SW-40          IS EQUAL TO "N"
079910010501                  INITIALIZE                 WS-MONTH-COL4
079920010501                  MOVE WS-MONTH-COL4         TO  SH05-BRANCH-ADDR2
079930010501               ELSE
079940010612                  IF SL                      IS EQUAL TO 1
079950010612                     MOVE WS-MONTH-COL4      TO  SH05-BRANCH-ADDR2
079960010612                  ELSE
079970010612                     MOVE WS-MONTH-COL4-F    TO  SH05-BRANCH-ADDR2
079980010612                  END-IF
079990010501               END-IF
080000991213               GO TO LR1-0030-0140
080010991213           END-IF
080020991213
080030010419RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
080040010419              MOVE DL-NOMINEE-ADDR2    TO SH05-INVESTOR-ADDRESS2
080050010419           ELSE
080060010419              MOVE WS-SH05-INVESTOR-ADDRESS2
080070010419                                       TO  SH05-INVESTOR-ADDRESS2
080080010419RF9415     END-IF
080090010419      *    MOVE WS-SH05-INVESTOR-ADDRESS2
080100010419      *                                TO  SH05-INVESTOR-ADDRESS2.
080110000000           MOVE WS-BRANCH-ADDR2        TO  SH05-BRANCH-ADDR2.
080120010427
080130010427R9415A     MOVE WS-SH05-LIT1 (SL)      TO  SH05-LIT1.
080140010427
080150000000       LR1-0030-0140.
080160990926
080170990926           SET SPL                     UP BY  1.
080180000000           MOVE STMT-HDG-05            TO  WS-STMTPGE-LNE (SPL).
080190000000
080200000000      *****
080210000000      *****    Format statement heading line 6.
080220000000      *****
080230000000
080240000000           MOVE SPACES                 TO  WS-SH06-INVESTOR-ADDRESS3.
080250990926
080260000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
080270990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
080280990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
080290990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
080300000000               MOVE II-POSTAL-CODE     TO  SH06-POSTAL-CODE
080310000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
080320990923           ELSE
080330000000               MOVE II-INVESTOR-ADDRESS3
080340990923                                       TO  WS-SH06-INVESTOR-ADDRESS3
080350990923           END-IF
080360990923
080370980417980417     IF  WS-LDA-BRANCH-ADDRESS
080380980417980417                        IS EQUAL TO  "N"
080390000000               MOVE SPACES             TO  WS-BRANCH-ADDR3.
080400000000
080410000000           MOVE SPACES                 TO  STMT-HDG-06.
080420000000
080430001219           IF  WS-STMTVRSN    IS EQUAL TO  9
080440010419RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
080450010419                  MOVE DL-NOMINEE-ADDR3    TO SH06V8-INVESTOR-ADDRESS3
080460010419               ELSE
080470010419                  MOVE WS-SH06-INVESTOR-ADDRESS3
080480010419                                          TO  SH06V8-INVESTOR-ADDRESS3
080490010419RF9415         END-IF
080500991213
080510991213               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
080520991213                  MOVE SPACES TO SH06V8-BRANCH-ADDR3
080530991213               END-IF
080540010427
080550010515      ***
080560010515      ***   For RRIF, LIF, and LRIF account, ANNUITY DATE line will
080570010515      ***   not populate. Leave it as a blank line.
080580010515      ***
080590010515
080600010515R10243         MOVE SPACES                 TO  SH06V8-LIT1
080610010515               MOVE SPACES                 TO  SH06-BRANCH-ADDR3
080620010515
080630190612      *RFS183449-Starts
080640190612      *        IF  AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "RRIF "
080650190612      *        AND AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "LIF  "
080660190612R52366*        AND AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "RLIF "
080670190612      *        AND AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "LRIF "
080680190612               IF  Lc-Acct-Type-Code       IS NOT EQUAL TO "RRIF "
080690190612               AND Lc-Acct-Type-Code       IS NOT EQUAL TO "LIF  "
080700190612               AND Lc-Acct-Type-Code       IS NOT EQUAL TO "RLIF "
080710190612               AND Lc-Acct-Type-Code       IS NOT EQUAL TO "LRIF "
080720190612      *RFS183449-Ends
080730010515RF9415             MOVE WS-SH06-LIT1 (SL)  TO  SH06V8-LIT1
080740010515                   IF WS-STMT-DTA-SW-40          IS EQUAL TO "N"
080750010515                      INITIALIZE  WS-MONTH-COL5
080760010515                      MOVE WS-MONTH-COL5         TO  SH06-BRANCH-ADDR3
080770010515                   ELSE
080780010612                      IF SL                    IS EQUAL TO 1
080790010612                         MOVE WS-MONTH-COL5    TO  SH06-BRANCH-ADDR3
080800010612                      ELSE
080810010612                         MOVE WS-MONTH-COL5-F  TO  SH06-BRANCH-ADDR3
080820010612                      END-IF
080830010515                   END-IF
080840010515               END-IF
080850991213               GO TO LR1-0030-0150
080860991213           END-IF
080870991213
080880010419RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
080890010419              MOVE DL-NOMINEE-ADDR3    TO SH06-INVESTOR-ADDRESS3
080900010419           ELSE
080910010419              MOVE WS-SH06-INVESTOR-ADDRESS3
080920010419                                       TO  SH06-INVESTOR-ADDRESS3
080930010419RF9415     END-IF
080940010419      *    MOVE WS-SH06-INVESTOR-ADDRESS3
080950010419      *                                TO  SH06-INVESTOR-ADDRESS3.
080960000000           MOVE WS-BRANCH-ADDR3        TO  SH06-BRANCH-ADDR3.
080970010427
080980010427R9415A     MOVE WS-SH06-LIT1 (SL)      TO  SH06-LIT1.
080990010427
081000000000       LR1-0030-0150.
081010990923
081020990923           SET SPL                     UP BY  1.
081030000000           MOVE STMT-HDG-06            TO  WS-STMTPGE-LNE (SPL).
081040000000
081050000000      *****
081060000000      *****    Format statement heading line 7.
081070000000      *****
081080000000
081090000000           MOVE SPACES                 TO  WS-SH07-INVESTOR-ADDRESS4.
081100000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
081110990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
081120990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
081130000000               MOVE II-POSTAL-CODE     TO  SH07-POSTAL-CODE
081140000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
081150990923           ELSE
081160000000               MOVE II-INVESTOR-ADDRESS4
081170990923                                       TO  WS-SH07-INVESTOR-ADDRESS4
081180990923           END-IF
081190990923
081200990923980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO "N"
081210990923               MOVE SPACES             TO  WS-BRANCH-ADDR4
081220990923           END-IF
081230990923
081240000000           MOVE SPACES                 TO  STMT-HDG-07.
081250000000
081260001219           IF  WS-STMTVRSN    IS EQUAL TO  9
081270010419RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
081280010419                  MOVE DL-NOMINEE-ADDR4    TO SH07V8-INVESTOR-ADDRESS4
081290010419               ELSE
081300010419                  MOVE WS-SH07-INVESTOR-ADDRESS4
081310010419                                          TO  SH07V8-INVESTOR-ADDRESS4
081320010419RF9415         END-IF
081330010419      *        MOVE WS-SH07-INVESTOR-ADDRESS4
081340010419      *                                TO  SH07V8-INVESTOR-ADDRESS4
081350010501      *        MOVE WS-BRANCH-ADDR4    TO  SH07V8-BRANCH-ADDR4
081360991213
081370991213               IF WS-LDA-DLR-REP-PRINT        IS EQUAL TO "R"
081380991213                  MOVE SPACES TO SH07V8-BRANCH-ADDR4
081390991213               END-IF
081400010427
081410010430RF9415         MOVE WS-SH07-LIT1 (SL)  TO  SH07V8-LIT1
081420010430               MOVE WS-ANNUITANT-NAME      TO  SH07-BRANCH-ADDR4
081430010427
081440991213               GO TO LR1-0030-0160
081450991213           END-IF
081460991019
081470010419RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
081480010419              MOVE DL-NOMINEE-ADDR4    TO SH07-INVESTOR-ADDRESS4
081490010419           ELSE
081500010419              MOVE WS-SH07-INVESTOR-ADDRESS4
081510010419                                       TO  SH07-INVESTOR-ADDRESS4
081520010419RF9415     END-IF
081530010419      *    MOVE WS-SH07-INVESTOR-ADDRESS4
081540010419      *                                TO  SH07-INVESTOR-ADDRESS4.
081550000000           MOVE WS-BRANCH-ADDR4        TO  SH07-BRANCH-ADDR4.
081560010427
081570010427R9415A     MOVE WS-SH07-LIT1 (SL)      TO  SH07-LIT1.
081580010427
081590000000       LR1-0030-0160.
081600990922
081610990926           SET SPL                     UP BY  1.
081620000000           MOVE STMT-HDG-07            TO  WS-STMTPGE-LNE (SPL).
081630000000
081640000000      *****
081650000000      *****    Format statement heading line 8.
081660000000      *****
081670000000
081680000000           MOVE SPACES                 TO  WS-SH08-INVESTOR-ADDRESS5.
081690990926
081700000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
081710990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
081720000000               MOVE II-POSTAL-CODE     TO  SH08-POSTAL-CODE
081730000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
081740990923           ELSE
081750000000               MOVE II-INVESTOR-ADDRESS5
081760990923                                       TO  WS-SH08-INVESTOR-ADDRESS5
081770990923           END-IF
081780990923
081790000000           MOVE SPACES                 TO  STMT-HDG-08.
081800000000
081810001219           IF  WS-STMTVRSN    IS EQUAL TO  9
081820010419               MOVE WS-SH08-INVESTOR-ADDRESS5
081830010419                                       TO  SH08V8-INVESTOR-ADDRESS5
081840991213               GO TO LR1-0030-0170
081850991213           END-IF
081860991213
081870010419           MOVE WS-SH08-INVESTOR-ADDRESS5
081880010419                                       TO  SH08-INVESTOR-ADDRESS5.
081890000000
081900000000       LR1-0030-0170.
081910990923
081920990923           SET SPL                     UP BY  1.
081930000000           MOVE STMT-HDG-08            TO  WS-STMTPGE-LNE (SPL).
081940000000
081950000000      *****
081960000000      *****    Format statement heading line 9.
081970000000      *****
081980000000
081990990926           MOVE SPACES                 TO  WS-SH09-POSTAL-CODE.
082000990923
082010000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
082020990923               MOVE II-POSTAL-CODE     TO  WS-SH09-POSTAL-CODE
082030990923           END-IF
082040990923
082050000000           MOVE SPACES                 TO  STMT-HDG-09.
082060990926
082070001219           IF  WS-STMTVRSN    IS EQUAL   TO  9
082080991213               MOVE WS-SH09-POSTAL-CODE  TO  SH09V8-POSTAL-CODE
082090991213               GO TO LR1-0030-0180
082100991213           END-IF
082110991213
082120990926           MOVE WS-SH09-POSTAL-CODE      TO  SH09-POSTAL-CODE.
082130000000
082140000000       LR1-0030-0180.
082150990923
082160990925           SET SPL                     UP BY  1
082170000000           MOVE STMT-HDG-09            TO  WS-STMTPGE-LNE (SPL).
082180990925
082190000000      *****
082200000000      *****    Format statement heading line 10.
082210000000      *****
082220000000
082230000000           MOVE SPACES                 TO  WS-SH10-LIT1X.
082240990922
082250000000           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
082260990922               MOVE WS-SH10-LIT1 (SL)  TO  WS-SH10-LIT1X
082270990922           END-IF
082280990922
082290000000           MOVE SPACES                 TO  WS-SH10-PHONE.
082300990922
082310000000           IF  WS-PHONE   IS NOT EQUAL TO  ZEROES
082320000000               MOVE "("                TO  SH10-PHONE-LITLB
082330000000               MOVE WS-SH-PHONE-01-03  TO  SH10-PHONE-01-03
082340000000               MOVE ")"                TO  SH10-PHONE-LITRB
082350000000               MOVE WS-SH-PHONE-04-06  TO  SH10-PHONE-04-06
082360000000               MOVE "-"                TO  SH10-PHONE-LITDSH
082370990922               MOVE WS-SH-PHONE-07-10  TO  SH10-PHONE-07-10
082380990922           END-IF
082390990922
082400000000           MOVE SPACES                 TO  STMT-HDG-10.
082410000000
082420001219           IF  WS-STMTVRSN    IS EQUAL TO  9
082430991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
082440991213                  MOVE WS-SH10-LIT1X      TO  SH10V8-LIT1
082450991213                  MOVE WS-SH10-PHONE      TO  SH10V8-PHONE
082460991213               GO TO LR1-0030-0190
082470991213           END-IF
082480991213
082490001219           IF  WS-STMTVRSN    IS EQUAL TO  9
082500991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
082510991213                  MOVE WS-SH10-LIT1X      TO  SH10V8-LIT1
082520991213                  MOVE SPACES             TO  SH10V8-PHONE
082530991213               GO TO LR1-0030-0190
082540991213           END-IF
082550991019
082560000000           MOVE WS-SH10-LIT1X          TO  SH10-LIT1.
082570000000           MOVE WS-SH10-PHONE          TO  SH10-PHONE.
082580000000
082590000000       LR1-0030-0190.
082600990925
082610990922           SET SPL                     UP BY  1.
082620000000           MOVE STMT-HDG-10            TO  WS-STMTPGE-LNE (SPL).
082630000000
082640000000      *****
082650000000      *****    Format statement heading line 11.
082660000000      *****
082670000000
082680000000           MOVE SPACES                 TO  WS-SH11-NOMINEE.
082690990922
082700991013           IF  WS-NOMINEE              IS NOT EQUAL TO  SPACES
082710990922               MOVE WS-NOMINEE         TO  WS-SH11-NOMINEE
082720990922           END-IF
082730990922
082740000000           MOVE SPACES                 TO  WS-SH11-FAX.
082750990927
082760000000           IF  WS-FAX     IS NOT EQUAL TO  ZEROES
082770000000               MOVE "("                TO  SH11-FAX-LITLB
082780000000               MOVE WS-SH-FAX-01-03    TO  SH11-FAX-01-03
082790000000               MOVE ")"                TO  SH11-FAX-LITRB
082800000000               MOVE WS-SH-FAX-04-06    TO  SH11-FAX-04-06
082810000000               MOVE "-"                TO  SH11-FAX-LITDSH
082820990927               MOVE WS-SH-FAX-07-10    TO  SH11-FAX-07-10
082830990927           END-IF
082840990927
082850000000           MOVE SPACES                 TO  STMT-HDG-11.
082860000000
082870001219           IF  WS-STMTVRSN    IS EQUAL TO  9
082880991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
082890991213               MOVE WS-SH11-NOMINEE    TO  SH11V8-NOMINEE
082900991213               MOVE WS-SH11-FAX        TO  SH11V8-FAX
082910991213               GO TO LR1-0030-0200
082920991213           END-IF
082930991213
082940001219           IF  WS-STMTVRSN    IS EQUAL TO  9
082950991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
082960991213               MOVE WS-SH11-NOMINEE    TO  SH11V8-NOMINEE
082970991213               MOVE WS-SH02-SIN-NO     TO  SH11V8-FAX
082980991213               GO TO LR1-0030-0200
082990991213           END-IF
083000991213
083010991022
083020000000           MOVE WS-SH11-NOMINEE        TO  SH11-NOMINEE.
083030000000           MOVE WS-SH11-FAX            TO  SH11-FAX.
083040000000
083050000000       LR1-0030-0200.
083060990923
083070990923           SET SPL                     UP BY  1.
083080000000           MOVE STMT-HDG-11            TO  WS-STMTPGE-LNE (SPL).
083090991019
083100001129      *    IF WS-LDA-PRINT-HEADING           IS NOT EQUAL TO "Y"
083110001129      *       GO TO LR1-0030-0300
083120001129      *    END-IF
083130991020
083140001129      *    SET SPL                     UP BY  3.
083150001129001127*    MOVE WS-SHLPL01 (SHL)         TO  WS-STMTPGE-LNE (SPL).
083160001129      *    SET SPL                     UP BY  3.
083170001129      *    MOVE WS-SHLPL02 (SHL)         TO  WS-STMTPGE-LNE (SPL).
083180001129      *    SET SPL                     UP BY  2.
083190001129      *    MOVE WS-SHLPL03 (SHL)         TO  WS-STMTPGE-LNE (SPL).
083200001129      *    SET SPL                     UP BY  1.
083210001129      *    MOVE WS-SHLPL04 (SHL)         TO  WS-STMTPGE-LNE (SPL).
083220991019
083230990624       LR1-0030-0300.
083240990925
083250000000      *****
083260000000      *****    Format the messages.  Initially, attempt to format the
083270000000      *****    statement messages using the Account Type Code.  If none
083280000000      *****    are found, use an Account Type Code of spaces.
083290021212R15028*****
083300021212  ¦   *****RFS15028 implements the following logic:
083310021212  ¦   *****    For new investor: get new investor's info. If not found
083320021212  ¦   *****    using existing info(new-investor = 'N'). If not found,
083330021212  ¦   *****    using All info(new-investor = 'A'). If not found, using
083340021212  ¦   *****    Account Type Code of spaces.
083350021212R15028*****
083360000000
083370000000           MOVE SPACES                 TO  WS-STATEMENT-PAGE-MESSAGE.
083380000000           MOVE AD-FUND-NO             TO  WS-FUND-NO.
083390000000
083400021212R15028 LR1-0030-0400.
083410021212  ¦
083420021212  ¦        MOVE LOW-VALUES             TO  STMT-MSG-RECD.
083430190619      *RFS 183449 -Start
083440190619  ¦   *    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
083450190619  ¦        MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
083460190619      *RFS 183449 -End
083470021212  ¦                                        OF STMT-MSG-RECD.
083480021212  ¦        MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
083490021212  ¦                                        OF STMT-MSG-RECD.
083500021212  ¦        MOVE II-NEW-INVESTOR-FLAG   TO  WS-NEW-INVESTOR-FLAG.
083510021212  ¦        MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
083520021212  ¦                                        OF STMT-MSG-RECD.
083530021212  ¦        MOVE 1                      TO  LINE-NO
083540021212  ¦                                        OF STMT-MSG-RECD.
083550021212  ¦   *******
083560021212  ¦   *******   Direct read to determine if the specific investor(New or
083570021212  ¦   *******   not) infor exist or not.
083580021212  ¦   *******
083590021212  ¦
083600021212  ¦        READ STMT-MSG-FILE
083610021212  ¦          INVALID KEY
083620021212  ¦            IF WS-NEW-INVESTOR-FLAG  = "Y"
083630021212  ¦               MOVE "N"               TO WS-NEW-INVESTOR-FLAG
083640021212  ¦               MOVE WS-NEW-INVESTOR-FLAG  TO  NEW-INVESTOR
083650021212  ¦                                          OF STMT-MSG-RECD
083660021212  ¦               READ STMT-MSG-FILE
083670021212  ¦                   INVALID KEY
083680021212  ¦                   MOVE "A"           TO WS-NEW-INVESTOR-FLAG
083690021212  ¦               END-READ
083700021212  ¦            ELSE
083710021212  ¦               MOVE "A"               TO WS-NEW-INVESTOR-FLAG
083720021212  ¦            END-IF
083730021212R15028     END-READ.
083740021212
083750000000       LR1-0040.
083760990923
083770000000           MOVE LOW-VALUES             TO  STMT-MSG-RECD.
083780190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
083790190619183449     MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
083800000000                                           OF STMT-MSG-RECD.
083810000000           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
083820000000                                           OF STMT-MSG-RECD.
083830021212R15028     MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
083840021212  ¦                                        OF STMT-MSG-RECD.
083850021212  ¦        MOVE ZEROES                 TO  LINE-NO
083860021212R15028                                     OF STMT-MSG-RECD.
083870021211
083880000000           START STMT-MSG-FILE
083890000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
083900000000               INVALID KEY
083910990922                   GO TO LR1-0070
083920990922           END-START.
083930990922
083940000000       LR1-0050.
083950990922
083960000000           READ STMT-MSG-FILE NEXT     AT  END
083970990922               GO TO LR1-0070
083980990922           END-READ
083990021211
084000000000           IF  LINE-NO    IS GREATER THAN  WS-SPML-MAX
084010990922           OR  LANGUAGE-CODE OF STMT-MSG-RECD
084020000000                          IS NOT EQUAL TO  AD-LANGUAGE
084030990922           OR  ACCOUNT-TYPE-CODE OF STMT-MSG-RECD
084040190619183449*                   IS NOT EQUAL TO  AD-ACCOUNT-TYPE-SHORT
084050190619183449                    IS NOT EQUAL TO  Lc-Acct-Type-Code
084060021212R15028     OR  NEW-INVESTOR OF STMT-MSG-RECD  IS NOT EQUAL
084070021212R15028                         TO  WS-NEW-INVESTOR-FLAG
084080990922               GO TO LR1-0070
084090990922           END-IF
084100990922
084110000000           IF  LINE-NO   NOT GREATER THAN  ZEROES
084120990922               GO TO LR1-0060
084130990922           END-IF
084140000000
084150000000           MOVE SPACES                 TO  STMT-MSG-01.
084160000000           MOVE STATEMENT-MESSAGE      TO  SM01-STATEMENT-MESSAGE.
084170000000
084180000000           SET SPML                    TO  LINE-NO.
084190000000           MOVE STMT-MSG-01            TO  WS-STMTPGE-MSGLNE (SPML).
084200000000
084210000000       LR1-0060.
084220990922
084230000000           GO TO LR1-0050.
084240000000
084250000000       LR1-0070.
084260000000
084270000000      *****
084280000000      *****    If no statement messages were formatted and the Account
084290000000      *****    Type Code being used was not spaces, attempt to format
084300000000      *****    statement messages using an Account Type Code of spaces.
084310000000      *****
084320000000
084330990922           IF  WS-STATEMENT-PAGE-MESSAGE  IS EQUAL TO  SPACES
084340990922           AND AD-ACCOUNT-TYPE-SHORT      IS NOT EQUAL TO  SPACES
084350990925               MOVE SPACES                TO  AD-ACCOUNT-TYPE-SHORT
084360021212R15028*        GO TO LR1-0040
084370021212R15028         GO TO LR1-0030-0400
084380990922           END-IF
084390990922
084400001219           IF  WS-STMTVRSN    IS EQUAL TO  9
084410991213               MOVE WS-STATEMENT-PAGE-MESSAGE
084420991213                                       TO  WS-STMTPGE-MSG-V8
084430991213               GO TO LR1-0070-0000-0010
084440991213           END-IF
084450991213
084460000000           MOVE WS-STATEMENT-PAGE-MESSAGE
084470000000                                       TO  WS-STMTPGE-MSG.
084480000000       LR1-0070-0000-0010.
084490000000
084500000000      *****
084510000000      *****    Position at first detail line.
084520000000      *****
084530000000
084540001204      *    SET SPL                     TO  WS-SPL-DTLFST.
084550001204           SET SPL                     TO  WS-SHPL-DTLFST.
084560000000
084570000000      *****
084580000000      *****    Determine if the client statement copy is to be
084590000000      *****    suppressed.  This is done depending on whether or not the
084600000000      *****    account being processed is a nominee account.
084610000000      *****
084620000000
084630000000           MOVE "N"                    TO  WS-CLTSPR-SW.
084640000000
084650000000           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
084660990923               GO TO LR1-0070-0030
084670990923           END-IF
084680000000
084690000000      *****
084700000000      *****    Processing a non-nominee account.  Initially read through
084710000000      *****    the Dealer Mailing Instruction.  If the form CLTSTSPR is
084720000000      *****    found, the client copy statement is to be suppressed.
084730000000      *****
084740000000
084750000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
084760000000           MOVE AD-DEALER              TO  DEALER-CODE
084770000000                                           OF DEALER-MAILING-INSTR-RECD.
084780000000           MOVE "CLTSTSPR  "           TO  FORM-CODE
084790000000                                           OF DEALER-MAILING-INSTR-RECD.
084800000000           START DEALER-MAILING-INSTR
084810000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
084820000000               INVALID KEY
084830990923                   GO TO LR1-0070-0010
084840990923           END-START
084850990923
084860000000           READ DEALER-MAILING-INSTR NEXT
084870990923             AT  END
084880990923               GO TO LR1-0070-0010
084890990923           END-READ
084900990923
084910990923           IF  FORM-CODE OF DEALER-MAILING-INSTR-RECD
084920990923                IS NOT EQUAL TO  "CLTSTSPR  "
084930990923           OR  DEALER-CODE OF DEALER-MAILING-INSTR-RECD
084940990923                IS NOT EQUAL TO  AD-DEALER
084950990923               GO TO LR1-0070-0010
084960990923           END-IF
084970990923
084980000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
084990000000           GO TO LR1-0070-0020.
085000000000
085010000000       LR1-0070-0010.
085020000000
085030000000      *****
085040000000      *****    Processing a non-nominee account.  The client statement
085050000000      *****    copy is to be printed, according to the Dealer Mailing
085060000000      *****    Instruction.  Now read through the Dealer Representative
085070000000      *****    Mailing Instruction.  If the form CLTSTSPR is found, the
085080000000      *****    client statement copy is to be suppressed.
085090000000      *****
085100000000
085110000000           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
085120000000           MOVE AD-DEALER              TO  DEALER-CODE
085130000000                                           OF DLRREP-MAILING-INSTR-RECD.
085140000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
085150000000                                           OF DLRREP-MAILING-INSTR-RECD.
085160000000           MOVE "CLTSTSPR  "           TO  FORM-CODE
085170000000                                           OF DLRREP-MAILING-INSTR-RECD.
085180000000           START DEALER-REP-MAILING-INSTR
085190000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
085200000000               INVALID KEY
085210000000                   GO TO LR1-0070-0020.
085220000000
085230000000           READ DEALER-REP-MAILING-INSTR NEXT
085240000000                                       AT  END
085250000000               GO TO LR1-0070-0020.
085260000000
085270000000           IF  FORM-CODE
085280000000               OF DLRREP-MAILING-INSTR-RECD
085290000000                          IS NOT EQUAL TO  "CLTSTSPR  "
085300000000           OR  DEALER-REP-CODE
085310000000               OF DLRREP-MAILING-INSTR-RECD
085320000000                          IS NOT EQUAL TO  AD-SALESREP
085330000000           OR  DEALER-CODE
085340000000               OF DLRREP-MAILING-INSTR-RECD
085350000000                          IS NOT EQUAL TO  AD-DEALER
085360000000               GO TO LR1-0070-0020.
085370000000
085380000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
085390000000
085400000000       LR1-0070-0020.
085410990925
085420000000           GO TO LR1-0070-0050.
085430000000
085440000000       LR1-0070-0030.
085450000000
085460000000      *****
085470000000      *****    Processing a nominee account.  Read through the Nominee
085480000000      *****    Mailing Instruction.  If the form CLTSTCLT is not found,
085490000000      *****    the client copy statement is to be suppressed.
085500000000      *****
085510000000
085520000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
085530000000               GO TO LR1-0070-0050.
085540000000
085550000000           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
085560000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
085570000000                                           OF NOMINEE-MAILING-INST-RECD.
085580000000           MOVE "CLTSTCLT  "           TO  FORM-CODE
085590000000                                           OF NOMINEE-MAILING-INST-RECD.
085600000000           START NOMINEE-MAILING-INSTRUCTIONS
085610000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
085620000000               INVALID KEY
085630000000                   GO TO LR1-0070-0040.
085640000000
085650000000           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
085660000000                                       AT  END
085670000000               GO TO LR1-0070-0040.
085680000000
085690000000           IF  FORM-CODE
085700000000               OF NOMINEE-MAILING-INST-RECD
085710000000                          IS NOT EQUAL TO  "CLTSTCLT  "
085720000000           OR  NOMINEE-CODE
085730000000               OF NOMINEE-MAILING-INST-RECD
085740000000                          IS NOT EQUAL TO  WS-NOMINEE-CODE
085750000000               GO TO LR1-0070-0040.
085760000000
085770000000           GO TO LR1-0070-0050.
085780000000
085790000000       LR1-0070-0040.
085800990925
085810000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
085820000000
085830000000       LR1-0070-0050.
085840000000
085850000000      *****
085860000000      *****    Read through the Dealer Mailing Instruction to determine
085870000000      *****    if the Statement Print Header is to be flagged to print
085880000000      *****    dealer copies of the statements.
085890000000      *****
085900000000
085910000000           MOVE SPACES                 TO  WS-DEAMLGINS-REC.
085920000000
085930000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
085940000000           MOVE AD-DEALER              TO  DEALER-CODE
085950000000                                           OF DEALER-MAILING-INSTR-RECD.
085960000000           MOVE "CLTSTDLR  "           TO  FORM-CODE
085970000000                                           OF DEALER-MAILING-INSTR-RECD.
085980000000           START DEALER-MAILING-INSTR
085990000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
086000000000               INVALID KEY
086010990925                   GO TO LR1-0080
086020990925           END-START
086030990925
086040000000           READ DEALER-MAILING-INSTR NEXT
086050000000                                       AT  END
086060990925               GO TO LR1-0080
086070990925           END-READ
086080990925
086090000000           IF  FORM-CODE
086100000000               OF DEALER-MAILING-INSTR-RECD
086110000000                          IS NOT EQUAL TO  "CLTSTDLR  "
086120000000           OR  DEALER-CODE
086130000000               OF DEALER-MAILING-INSTR-RECD
086140000000                          IS NOT EQUAL TO  AD-DEALER
086150000000               GO TO LR1-0080.
086160000000
086170000000           MOVE DEALER-MAILING-INSTR-RECD
086180000000                                       TO  WS-DEAMLGINS-REC.
086190000000
086200000000       LR1-0080.
086210000000
086220000000      *****
086230000000      *****    Determine if the Statement Print Header is to be flagged
086240000000      *****    to print dealer representative copies of the statements.
086250000000      *****    Initially read through the Dealer Mailing Instruction.
086260000000      *****    If a record is not found, read through the Dealer
086270000000      *****    Representative Mailing Instuction.
086280000000      *****
086290000000
086300000000           MOVE SPACES                 TO  WS-DERMLGINS-REC.
086310000000
086320000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
086330000000           MOVE AD-DEALER              TO  DEALER-CODE
086340000000                                           OF DEALER-MAILING-INSTR-RECD.
086350000000           MOVE "CLTSTREP  "           TO  FORM-CODE
086360000000                                           OF DEALER-MAILING-INSTR-RECD.
086370000000           START DEALER-MAILING-INSTR
086380000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
086390000000               INVALID KEY
086400000000                   GO TO LR1-0080-0010.
086410000000
086420000000           READ DEALER-MAILING-INSTR NEXT
086430000000                                       AT  END
086440000000               GO TO LR1-0080-0010.
086450000000
086460000000           IF  FORM-CODE
086470000000               OF DEALER-MAILING-INSTR-RECD
086480000000                          IS NOT EQUAL TO  "CLTSTREP  "
086490000000           OR  DEALER-CODE
086500000000               OF DEALER-MAILING-INSTR-RECD
086510000000                          IS NOT EQUAL TO  AD-DEALER
086520000000               GO TO LR1-0080-0010.
086530000000
086540000000           MOVE CORR MFADEAMIP
086550000000                     OF DEALER-MAILING-INSTR-RECD
086560000000                                       TO  MFADERMIP
086570000000                                           OF WS-DERMLGINS-REC.
086580000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
086590000000                                           OF WS-DERMLGINS-REC.
086600000000           GO TO LR1-0090.
086610000000
086620000000       LR1-0080-0010.
086630990923
086640000000           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
086650000000           MOVE AD-DEALER              TO  DEALER-CODE
086660000000                                           OF DLRREP-MAILING-INSTR-RECD.
086670000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
086680000000                                           OF DLRREP-MAILING-INSTR-RECD.
086690000000           MOVE "CLTSTREP  "           TO  FORM-CODE
086700000000                                           OF DLRREP-MAILING-INSTR-RECD.
086710000000           START DEALER-REP-MAILING-INSTR
086720000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
086730000000               INVALID KEY
086740000000                   GO TO LR1-0090.
086750000000
086760000000           READ DEALER-REP-MAILING-INSTR NEXT
086770000000                                       AT  END
086780000000               GO TO LR1-0090.
086790000000
086800000000           IF  FORM-CODE
086810000000               OF DLRREP-MAILING-INSTR-RECD
086820000000                          IS NOT EQUAL TO  "CLTSTREP  "
086830000000           OR  DEALER-REP-CODE
086840000000               OF DLRREP-MAILING-INSTR-RECD
086850000000                          IS NOT EQUAL TO  AD-SALESREP
086860000000           OR  DEALER-CODE
086870000000               OF DLRREP-MAILING-INSTR-RECD
086880000000                          IS NOT EQUAL TO  AD-DEALER
086890000000               GO TO LR1-0090.
086900000000
086910000000           MOVE DLRREP-MAILING-INSTR-RECD
086920000000                                       TO  WS-DERMLGINS-REC.
086930000000
086940000000       LR1-0090.
086950000000
086960000000      *****
086970000000      *****    Read through the Nominee Instruction to determine if the
086980000000      *****    Statement Print Header is to be flagged to print nominee
086990000000      *****    copies of the statements.
087000000000      *****
087010000000
087020000000           MOVE SPACES                 TO  WS-NOMMLGINS-REC.
087030000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
087040000000               GO TO LR1-0090-0010.
087050000000
087060000000           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
087070000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
087080000000                                           OF NOMINEE-MAILING-INST-RECD.
087090000000           MOVE "CLTSTNOM  "           TO  FORM-CODE
087100000000                                           OF NOMINEE-MAILING-INST-RECD.
087110000000           START NOMINEE-MAILING-INSTRUCTIONS
087120000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
087130000000               INVALID KEY
087140000000                   GO TO LR1-0090-0010.
087150000000
087160000000           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
087170000000                                       AT  END
087180000000               GO TO LR1-0090-0010.
087190000000
087200000000           IF  FORM-CODE
087210000000               OF NOMINEE-MAILING-INST-RECD
087220000000                          IS NOT EQUAL TO  "CLTSTNOM  "
087230000000           OR  NOMINEE-CODE
087240000000               OF NOMINEE-MAILING-INST-RECD
087250000000                          IS NOT EQUAL TO  WS-NOMINEE-CODE
087260000000               GO TO LR1-0090-0010.
087270000000
087280000000           MOVE NOMINEE-MAILING-INST-RECD
087290000000                                       TO  WS-NOMMLGINS-REC.
087300000000
087310000000       LR1-0090-0010.
087320000000
087330000000      *****
087340000000      *****    Format the Statement Print Header record.
087350000000      *****
087360000000
087370000000           MOVE "N"                    TO  WS-STMT-PRT-HDR-WSW.
087380000000
087390000000           MOVE SPACES                 TO  STMT-PRT-HDR-RECD.
087400000000           MOVE AD-INVESTOR-NO         TO  INVESTOR-NO
087410000000                                           OF STMT-PRT-HDR-RECD.
087420020921R13756     IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
087430020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  ACCOUNT
087440020903  ¦                                        OF STMT-PRT-HDR-RECD
087450020903R13756     ELSE
087460000000           MOVE AD-ACCOUNT             TO  ACCOUNT
087470020613                                           OF STMT-PRT-HDR-RECD
087480020903R13756     END-IF
087490000000           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
087500000000                                           OF STMT-PRT-HDR-RECD.
087510000000           MOVE AD-DEALER              TO  DEALER
087520000000                                           OF STMT-PRT-HDR-RECD.
087530000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
087540000000                                           OF STMT-PRT-HDR-RECD.
087550000000           MOVE II-SIN-NO              TO  SOCIAL-INSURANCE-NO
087560000000                                           OF STMT-PRT-HDR-RECD.
087570000000           MOVE WS-CLTSPR-SW           TO  CLIENT-COPY-SUPPRESSED
087580000000                                           OF STMT-PRT-HDR-RECD.
087590000000           MOVE II-HOLD-MAIL           TO  HOLD-MAIL
087600000000                                           OF STMT-PRT-HDR-RECD.
087610000000           MOVE BRANCH-CODE
087620000000                OF WS-DEAMLGINS-REC    TO  DEALER-BRANCH-CODE
087630000000                                           OF STMT-PRT-HDR-RECD.
087640000000           MOVE FREQUENCY-CODE
087650000000                OF WS-DEAMLGINS-REC    TO  DEALER-FREQUENCY-CODE
087660000000                                           OF STMT-PRT-HDR-RECD.
087670000000           MOVE METHOD-OF-DELIVERY
087680000000                OF WS-DEAMLGINS-REC    TO  DEALER-METHOD-OF-DELIVERY
087690000000                                           OF STMT-PRT-HDR-RECD.
087700000000           MOVE SEND-TO
087710000000                OF WS-DEAMLGINS-REC    TO  DEALER-SEND-TO
087720000000                                           OF STMT-PRT-HDR-RECD.
087730000000           MOVE BRANCH-CODE
087740000000                OF WS-DERMLGINS-REC    TO  DLR-REP-BRANCH-CODE
087750000000                                           OF STMT-PRT-HDR-RECD.
087760000000           MOVE FREQUENCY-CODE
087770000000                OF WS-DERMLGINS-REC    TO  DLR-REP-FREQUENCY-CODE
087780000000                                           OF STMT-PRT-HDR-RECD.
087790000000           MOVE METHOD-OF-DELIVERY
087800000000                OF WS-DERMLGINS-REC    TO  DLR-REP-METHOD-OF-DELIVERY
087810000000                                           OF STMT-PRT-HDR-RECD.
087820000000           MOVE SEND-TO
087830000000                OF WS-DERMLGINS-REC    TO  DLR-REP-SEND-TO
087840000000                                           OF STMT-PRT-HDR-RECD.
087850000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
087860000000                                           OF STMT-PRT-HDR-RECD.
087870000000           MOVE BRANCH-CODE
087880000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-BRANCH-CODE
087890000000                                           OF STMT-PRT-HDR-RECD.
087900000000           MOVE FREQUENCY-CODE
087910000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-FREQUENCY-CODE
087920000000                                           OF STMT-PRT-HDR-RECD.
087930000000           MOVE METHOD-OF-DELIVERY
087940000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-METHOD-OF-DELIVERY
087950000000                                           OF STMT-PRT-HDR-RECD.
087960000000           MOVE SEND-TO
087970000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-SEND-TO
087980000000                                           OF STMT-PRT-HDR-RECD.
087990000000           MOVE II-COUNTRY-CODE        TO  COUNTRY-CODE
088000000000                                           OF STMT-PRT-HDR-RECD.
088010980116           MOVE II-LAST-NAME           TO  LAST-NAME
088020980116                                           OF STMT-PRT-HDR-RECD.
088030980116           MOVE II-FIRST-NAME          TO  FIRST-NAME
088040980116                                           OF STMT-PRT-HDR-RECD.
088050980116           MOVE II-INITIALS            TO  INITIALS
088060980116                                           OF STMT-PRT-HDR-RECD.
088070990105           MOVE II-LEGAL-NAME          TO  LEGAL-NAME
088080990105                                           OF STMT-PRT-HDR-RECD.
088090980129           MOVE AD-START-DATE          TO  START-DATE
088100980129                                           OF STMT-PRT-HDR-RECD.
088110980129           MOVE AD-END-DATE            TO  END-DATE
088120980129                                           OF STMT-PRT-HDR-RECD.
0881300104239415       MOVE DL-DEALER-CATEGORY-CODE TO  DEALER-CATEGORY-CODE
088140010423                                           OF STMT-PRT-HDR-RECD.
088150980116
088160010427       LR1-0090-0020.
088170010427
088180000000      *****
088190000000      *****    Initialize the statement currency totals.
088200000000      *****
088210000000
088220010427           IF WS-ACCOUNT-INV-SW             IS EQUAL TO "Y"
088230010427              GO TO LR1-EXIT
088240010427           END-IF.
088250010427
088260000000           SET SC                      TO  1.
088270000000
088280000000       LR1-0100.
088290990923
088300001201           MOVE ZEROS                  TO WS-AD-MARKET-VALUE-PRIOR-TOT.
088310001201           MOVE ZEROS                  TO WS-AD-MARKET-VALUE-TOT.
088320001201
088330000000           MOVE ZEROES                 TO  WS-SC-MARKET-VALUE (SC).
088340010912R11175     MOVE ZEROES                 TO  WS-SC-MARKET-PERC (SC).
088350000000           MOVE "N"                    TO  WS-SC-RPTIND (SC).
088360990925
088370000000           IF  SC            IS LESS THAN  WS-SC-MAX
088380000000               SET SC               UP BY  1
088390000000               GO TO LR1-0100.
088400000000
088410000000      *****
088420001130      *****    Initialize the statement account funds tables.
088430000000      *****
088440000000
088450010430      *    MOVE SPACES                 TO  WS-STMT-ACCOUNT-FUND-TABLE.
088460010430           INITIALIZE                      WS-STMT-ACCOUNT-FUND-TABLE.
088470000000           MOVE ZEROES                 TO  WS-SAF-LST.
088480000000
088490001130           INITIALIZE                  WS-ACCOUNT-FUND-TABLE2.
088500001130           MOVE ZEROES                 TO  WS-SAF2-LST.
088510001129
088520001129      *****
088530001129      *****  Initialize the statement account fund performance table.
088540001129      *****
088550001129
088560001130           INITIALIZE             WS-ACCOUNT-FUND-PERFORM-TABLE.
088570001129           MOVE ZEROES            TO  WS-SAFP-LST.
088580001129
088590010430      *****
088600010430      *****    Initialize the SEG-FUND-CONTRACT-DETAILS table.
088610010430      *****    (SECTION 40)
088620010430      *****
088630010430
088640010430           INITIALIZE             WS-SEG-FUND-CONTRACT-TBL.
088650010430           MOVE ZEROES            TO  WS-SFC-LST.
088660010430
088670010430      *****
088680010430      *****    Initialize the SEG-FUND-CONTRACT-INVESTMENT table.
088690010430      *****    (SECTION 43)
088700010430      *****
088710010430
088720010430           INITIALIZE             WS-SEG-FUND-CONTRACT-INV-TBL.
088730010430           MOVE ZEROES            TO  WS-SFCI-LST.
088740010430
088750010430      *****
088760010430      *****    Initialize the SEG-FUND-CONTRACT-INVESTMENT table.
088770010430      *****    (SECTION 45)
088780010430      *****
088790010430
088800010430           INITIALIZE             WS-ANNUITANT-DETAILS-TBL.
088810010430           MOVE ZEROES            TO  WS-ANN-LST.
088820010430
088830000000      *****
088840000000      *****    Initialize the statement transaction controls.
088850000000      *****
088860000000
088870000000           MOVE LOW-VALUES             TO  CURR-CTLS-DPR.
088880000000           MOVE LOW-VALUES             TO  PREV-CTLS-DPR.
088890000000
088900010427           MOVE "Y"                    TO  WS-ACCOUNT-INV-SW.
088910010427
088920001129      **********
088930001129
088940000000       LR1-EXIT.
088950000000           EXIT.
088960001129
088970001129      **********
088980001129
088990000000      /
089000000000       FORMAT-ACTTYP-DESC.
089010000000
089020000000      ******************************************************************
089030000000      *                                                                *
089040000000      *    F M T .   A C C O U N T   T Y P E   D E S C R I P T I O N   *
089050000000      *                                                                *
089060000000      ******************************************************************
089070000000
089080000000      *****
089090000000      *****    Initialize the work items.
089100000000      *****
089110000000
089120000000           MOVE SPACES                 TO  WS-ACTTYPIN-SFX.
089130000000           MOVE SPACES                 TO  WS-ACTTYPLIT1.
089140000000           MOVE SPACES                 TO  WS-ACTTYPLIT2.
089150000000           MOVE SPACES                 TO  WS-ACTTYPLIT3.
089160001127           MOVE SPACES                 TO  WS-ACTTYPOUT.
089170000000
089180000000      *****
089190980326      *****    The account type description will be formatted
089200980326      *****    If NOMINEE, format the description.
089210000000      *****
089220980326
089230980326980320 FAD-0001.
089240980326
089250980326980320     IF  WS-ACTTYPNOM   IS NOT = "Y"
089260980326980320         GO TO FAD-0003.
089270980326
089280980326980320     INITIALIZE INVESTOR-J-REC.
089290010615
089300010615R10404*****
089310010615      *****    When no transaction, Account-type-desc was lost,
089320010615      *****    because a new investor record already been read.
089330010615      *****    Therefore, AD-INVESTOR-NO, AD-ACCOUNT was used.
089340010615      *****
089350010615
089360010615           MOVE AD-INVESTOR-NO TO ACIVRNO OF INVESTOR-J-REC.
089370010615           MOVE AD-ACCOUNT     TO ACACTNO OF INVESTOR-J-REC.
089380010615
089390010615980320*    MOVE PR-INVESTOR-NO TO ACIVRNO OF INVESTOR-J-REC.
089400010615980320*    MOVE PR-ACCOUNT     TO ACACTNO OF INVESTOR-J-REC.
089410980326
089420980326980320     START INVESTOR-JOIN
089430980326980320           KEY IS NOT LESS THAN EXTERNALLY-DESCRIBED-KEY
089440980326980320           INVALID KEY
089450980326980320           GO TO FAD-0001.
089460980326
089470980326980320 FAD-0002.
089480980326980320     READ INVESTOR-JOIN NEXT RECORD
089490980326980320          AT END
089500980326980320          GO TO FAD-0030
089510980326980320     END-READ.
089520980326
089530010615980320*    IF PR-INVESTOR-NO IS NOT = ACIVRNO
089540010615980320     IF AD-INVESTOR-NO IS NOT = ACIVRNO
089550980326980320                          OF INVESTOR-J-REC
089560980326980320        GO TO FAD-0030.
089570980326
089580010615980320*    IF PR-ACCOUNT     IS NOT = ACACTNO
089590010615980320     IF AD-ACCOUNT     IS NOT = ACACTNO
089600980326980320                          OF INVESTOR-J-REC
089610980326980320        GO TO FAD-0030.
089620980326
089630980409           INITIALIZE MFANMTPYP.
089640980408980320     MOVE ANACTYP OF INVESTOR-JOIN TO NOMINEE-ACCOUNT-TYPE
089650980408980320                  OF NOMINEE-ACCOUNT-TYPE-DESCR.
089660980408980320     MOVE WS-ACTTYPLNG             TO LANGUAGE-CODE
089670980408980320                  OF NOMINEE-ACCOUNT-TYPE-DESCR.
089680980326
089690980408980320     READ NOMINEE-ACCOUNT-TYPE-DESCR
089700980408980320          INVALID KEY
089710980408980320          MOVE SPACES TO WS-ACTTYPOUT
089720980408980320     END-READ.
089730980326
089740990922990922     IF WS-STMTVRSN        IS NOT EQUAL 6
089750990922980320        MOVE ACCOUNT-TYPE-DESCR OF NOMINEE-ACCOUNT-TYPE-DESCR
089760990922980320                                TO WS-ACTTYPOUT
089770990922           ELSE
089780990922              MOVE SPACES             TO WS-ACTTYPOUT
089790990922           END-IF
089800980326
089810980408980320     GO TO FAD-0030.
089820980326
089830980326980320 FAD-0003.
089840000000
089850000000           IF  WS-ACTTYPLNG   IS EQUAL TO  "F"
089860990922               GO TO FAD-0040
089870990922           END-IF
089880990922
089890000000      *****
089900000000      *****    The English description may be formatted with a LOCKED-IN
089910000000      *****    literal, a SPOUSAL literal, a GROUP literal and the
089920000000      *****    original account type description.  If any of the
089930000000      *****    literals are not applicable, they will be suppressed.
089940000000      *****    Initially, determine which literals are applicable.
089950000000      *****
089960000000
089970000000           IF  WS-ACTTYPLCK   IS EQUAL TO  "Y"
089980000000           AND WS-ACTTYPSHT  NOT EQUAL TO  "LIRA "
089990000000               MOVE "LOCKED-IN    "    TO  WS-ACTTYPLIT1.
090000000000           IF  WS-ACTTYPGRP   IS EQUAL TO  "Y"
090010000000               MOVE "GROUP        "    TO  WS-ACTTYPLIT2.
090020000000           IF  WS-ACTTYPSPS   IS EQUAL TO  "Y"
090030000000               MOVE "SPOUSAL      "    TO  WS-ACTTYPLIT3.
090040000000
090050000000      *****
090060000000      *****    Compress the optional English literals.
090070000000      *****
090080000000
090090000000           IF  WS-ACTTYPLIT1 NOT EQUAL TO  SPACES
090100000000               GO TO FAD-0010.
090110000000
090120000000           IF  WS-ACTTYPLIT2 NOT EQUAL TO  SPACES
090130000000               MOVE WS-ACTTYPLIT2      TO  WS-ACTTYPLIT1
090140000000               MOVE SPACES             TO  WS-ACTTYPLIT2
090150000000               GO TO FAD-0010.
090160000000
090170000000           IF  WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
090180000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT1
090190000000               MOVE SPACES             TO  WS-ACTTYPLIT3
090200000000               GO TO FAD-0020.
090210000000
090220000000           GO TO FAD-0020.
090230000000
090240000000       FAD-0010.
090250990922
090260000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
090270000000           AND WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
090280000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT2
090290990922               MOVE SPACES             TO  WS-ACTTYPLIT3
090300990922           END-IF.
090310990922
090320000000       FAD-0020.
090330000000
090340000000      *****
090350000000      *****    Format the English account type description based on the
090360000000      *****    number of optional literals applicable.
090370000000      *****
090380000000
090390000000           IF  WS-ACTTYPLIT1  IS EQUAL TO  SPACES
090400000000               MOVE WS-ACTTYPIN        TO  WS-ACTTYPOUT
090410000000               GO TO FAD-0030.
090420000000
090430000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
090440000000               STRING WS-ACTTYPLIT1
090450000000                             DELIMITED BY  "  ",
090460000000                      " "    DELIMITED BY  SIZE,
090470000000                      WS-ACTTYPIN
090480000000                             DELIMITED BY  "  ",
090490000000                                     INTO  WS-ACTTYPOUT
090500000000               GO TO FAD-0030.
090510000000
090520000000           IF  WS-ACTTYPLIT3  IS EQUAL TO  SPACES
090530000000               STRING WS-ACTTYPLIT1
090540000000                             DELIMITED BY  "  ",
090550000000                      " "    DELIMITED BY  SIZE,
090560000000                      WS-ACTTYPLIT2
090570000000                             DELIMITED BY  "  ",
090580000000                      " "    DELIMITED BY  SIZE,
090590000000                      WS-ACTTYPIN
090600000000                             DELIMITED BY  "  ",
090610000000                                     INTO  WS-ACTTYPOUT
090620000000               GO TO FAD-0030.
090630000000
090640000000           STRING WS-ACTTYPLIT1
090650000000                             DELIMITED BY  "  ",
090660000000                  " "        DELIMITED BY  SIZE,
090670000000                  WS-ACTTYPLIT2
090680000000                             DELIMITED BY  "  ",
090690000000                  " "        DELIMITED BY  SIZE,
090700000000                  WS-ACTTYPLIT3
090710000000                             DELIMITED BY  "  ",
090720000000                  " "        DELIMITED BY  SIZE,
090730000000                  WS-ACTTYPIN
090740000000                             DELIMITED BY  "  ",
090750000000                                     INTO  WS-ACTTYPOUT.
090760000000
090770000000       FAD-0030.
090780000000           GO TO FAD-EXIT.
090790000000
090800000000       FAD-0040.
090810000000
090820000000      *****
090830000000      *****    The French description may be formatted with a GROUP
090840000000      *****    literal, a LOCKED-IN literal, a SPOUSAL literal and the
090850000000      *****    original account type description.  If any of the
090860000000      *****    literals are not applicable, they will be suppressed.
090870000000      *****    Initially, determine which literals are applicable.
090880000000      *****
090890000000
090900000000           IF  WS-ACTTYPGRP   IS EQUAL TO  "Y"
090910000000               MOVE "COLLECTIF    "    TO  WS-ACTTYPLIT1.
090920000000           IF  WS-ACTTYPLCK   IS EQUAL TO  "Y"
090930000000           AND WS-ACTTYPSHT  NOT EQUAL TO  "LIRA "
090940000000               MOVE "IMMOBILISE   "    TO  WS-ACTTYPLIT2.
090950000000           IF  WS-ACTTYPSPS   IS EQUAL TO  "Y"
090960000000               MOVE "DE CONJOINT  "    TO  WS-ACTTYPLIT3.
090970000000
090980000000      *****
090990000000      *****    Compress the optional French literals.
091000000000      *****
091010000000
091020000000           IF  WS-ACTTYPLIT1 NOT EQUAL TO  SPACES
091030000000               GO TO FAD-0050.
091040000000
091050000000           IF  WS-ACTTYPLIT2 NOT EQUAL TO  SPACES
091060000000               MOVE WS-ACTTYPLIT2      TO  WS-ACTTYPLIT1
091070000000               MOVE SPACES             TO  WS-ACTTYPLIT2
091080000000               GO TO FAD-0050.
091090000000
091100000000           IF  WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
091110000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT1
091120000000               MOVE SPACES             TO  WS-ACTTYPLIT3
091130000000               GO TO FAD-0060.
091140000000
091150000000           GO TO FAD-0060.
091160000000
091170000000       FAD-0050.
091180000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
091190000000           AND WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
091200000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT2
091210000000               MOVE SPACES             TO  WS-ACTTYPLIT3.
091220000000
091230000000       FAD-0060.
091240000000
091250000000      *****
091260000000      *****    Format the French account type description.  Since the
091270000000      *****    optional literals come after the original account
091280000000      *****    description and they have been compressed, it is not
091290000000      *****    necessary to base the formatting on which option literals
091300000000      *****    are applicable.
091310000000      *****
091320000000
091330000000           STRING WS-ACTTYPIN
091340000000                             DELIMITED BY  "  ",
091350000000                  " "        DELIMITED BY  SIZE,
091360000000                  WS-ACTTYPLIT1
091370000000                             DELIMITED BY  "  ",
091380000000                  " "        DELIMITED BY  SIZE,
091390000000                  WS-ACTTYPLIT2
091400000000                             DELIMITED BY  "  ",
091410000000                  " "        DELIMITED BY  SIZE,
091420000000                  WS-ACTTYPLIT3
091430000000                             DELIMITED BY  "  ",
091440000000                                     INTO  WS-ACTTYPOUT.
091450000000
091460000000       FAD-EXIT.
091470000000           EXIT.
091480000000      /
091490000000       PRINT-ERR-LINE.
091500000000
091510000000      ******************************************************************
091520000000      *                                                                *
091530000000      *    P R I N T   E R R O R   L I N E   R O U T I N E             *
091540000000      *                                                                *
091550000000      ******************************************************************
091560000000
091570000000      *****
091580000000      *****    If the error line has not been formatted, format it using
091590000000      *****    the Statement File - Primary record.
091600000000      *****
091610000000
091620000000           IF  ERROR-LINE-1  NOT EQUAL TO  SPACES
091630000000               GO TO PEL-0010.
091640000000
091650000000           MOVE PR-INVESTOR-NO         TO  EL1-INVESTOR-NO.
091660000000           MOVE PR-ACCOUNT             TO  EL1-ACCOUNT.
091670000000           MOVE PR-SECTION-NO          TO  EL1-SECTION-NO.
091680000000           MOVE "-"                    TO  EL1-SECTION-LITDSH.
091690000000           MOVE PR-SECTION-SEQUENCE    TO  EL1-SECTION-SEQUENCE.
091700000000           MOVE PR-LANGUAGE-CODE       TO  EL1-LANGUAGE-CODE.
091710000000           MOVE PR-DEALER              TO  EL1-DEALER.
091720000000           MOVE PR-DEALER-REP-CODE     TO  EL1-DEALER-REP-CODE.
091730000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
091740000000
091750000000       PEL-0010.
091760000000
091770000000      *****
091780000000      *****    If the page is full, start a new page and print the
091790000000      *****    Heading lines.
091800000000      *****
091810000000
091820000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
091830000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
091840000000
091850000000      *****
091860000000      *****    Print the error line.
091870000000      *****
091880000000
091890000000           WRITE PRINT-RECD          FROM  ERROR-LINE-1
091900000000                         AFTER  ADVANCING  1.
091910000000           ADD 1                       TO  WS-LNECNT.
091920000000
091930000000       PEL-EXIT.
091940000000           EXIT.
091950000000      /
091960000000       INITIAL-LOGIC-RTN.
091970000000
091980000000      ******************************************************************
091990000000      *                                                                *
092000000000      *    I N I T I A L   L O G I C   R O U T I N E                   *
092010000000      *                                                                *
092020000000      ******************************************************************
092030000000
092040000000      *****
092050000000      *****    Retrieve the Local Data Area.
092060000000      *****
092070000000
092080000000           ACCEPT WS-LOCAL-DATA-AREA FROM  WS-LOCAL.
092090000000
092100990119
092110000000      *****
092120000000      *****    Using the Management Company Code, determine which
092130000000      *****    version of the statement is to be produced.  The standard
092140000000      *****    statement will be zero.
092150000000      *****
092160000000
092170990921           MOVE ZEROES                    TO  WS-STMTVRSN.
092180990921           EVALUATE WS-LDA-STMT-VERSION
092190001219               WHEN 9         MOVE  9     TO WS-STMTVRSN
092200990921           END-EVALUATE.
092210000000
092220000000       ILR-0010.
092230000000
092240000000      *****
092250000000      *****    Initialize the program defined work items.
092260000000      *****
092270000000
092280000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT.
092290000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-00.
092300000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-05.
092310000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-10.
092320010313rf8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-12.
092330000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-15.
092340141007141165     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-16.
092350000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-20.
092360010323rf8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-25.
092370991126           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-30.
092380991126           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-35.
092390001114001114     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-40.
092400001114           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-43.
092410001114           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-45.
092420000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-50.
092430000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-60.
092440991013           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-62.
092450001114001114     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-67.
092460000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-70.
092470000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-82.
092480991013           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-84.
092490000000           MOVE ZEROES                 TO  WS-STMT-DTA-RCNT.
092500000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-01.
092510000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-05.
092520000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-10.
092530000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-20.
092540060618R35042     MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-21.
092550130408117038     MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-22.
092560010323RF8633     MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-25.
092570001129           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-30.
092580040406R20870     MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-33.
092590001129           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-35.
092600040406R20870     MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-38.
092610981207           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-60.
092620000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-80.
092630001114001114     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-90.
092640000000           MOVE ZEROES                 TO  WS-STMT-PRT-DTL-WCNT.
092650000000           MOVE ZEROES                 TO  WS-STMT-PRT-HDR-WCNT.
092660980210
092670000000
09268001121112090 *    MOVE 100                    TO  WS-SAF-MAX.
092690161012      * RFS 156599 START
09270016101212090 *    MOVE 500                    TO  WS-SAF-MAX.
092710161108164194*    MOVE 900                    TO  WS-SAF-MAX.
092720161108164194     MOVE 2400                   TO  WS-SAF-MAX.
092730161012      * RFS 156599 END
092740090824           MOVE 100                    TO  WS-SAFP-MAX.
092750000000           MOVE 2                      TO  WS-SC-MAX.
092760000000           MOVE 1000                   TO  WS-SIA-MAX.
092770000000           MOVE 87                     TO  WS-SPL-MAX.
092780010419RF9415     MOVE 50                     TO  WS-DI-MAX.
092790010427RF9415     MOVE 50                     TO  WS-DI-MAX.
092800090824RF9415*    MOVE 50                     TO  WS-SFC-MAX.
092810090824R71771     MOVE 300                    TO  WS-SFC-MAX.
092820090824RF9415*    MOVE 50                     TO  WS-SFCI-MAX.
092830090824R71771     MOVE 300                    TO  WS-SFCI-MAX.
092840010427RF9415     MOVE 50                     TO  WS-ANN-MAX.
092850010427
092860000000
092870001219           IF  WS-STMTVRSN    IS EQUAL TO  9
092880991213               MOVE 27                 TO  WS-SPL-DTLFST
092890001204               MOVE 27                 TO  WS-SHPL-DTLFST
092900991213               MOVE 69                 TO  WS-SPL-DTLLST
092910991213               MOVE 9                  TO  WS-SPL-HDGFST
092920991213               MOVE 14                 TO  WS-SPML-MAX
092930991213               GO TO ILR-0020
092940991213           END-IF
092950991213
092960000000           MOVE 27                     TO  WS-SPL-DTLFST.
092970000000           MOVE 68                     TO  WS-SPL-DTLLST.
092980000000           MOVE 9                      TO  WS-SPL-HDGFST.
092990000000           MOVE 14                     TO  WS-SPML-MAX.
093000000000
093010000000       ILR-0020.
093020990921
093030000000           MOVE 99                     TO  WS-LNECNT.
093040000000           MOVE 60                     TO  WS-LNEMAX.
093050000000           MOVE ZEROES                 TO  WS-PGECNT.
093060000000
093070000000           MOVE "N"                    TO  WS-EOJHDG-SW.
093080000000
093090000000           MOVE LOW-VALUES             TO  CURR-CONTROLS.
093100000000           MOVE LOW-VALUES             TO  PREV-CONTROLS.
093110000000
093120000000      *****
093130000000      *****    Format the heading literals.
093140000000      *****
093150000000
093160000104           MOVE   WS-LDA-AS-AT-DATE  TO        WS-DATE.
093170000104           MOVE   WS-LDA-AS-AT-DATE  TO        WS-SYS-DATE.
093180990120
093190000104           MOVE WS-DATE-YYYY           TO  HL1-YY.
093200000000           MOVE WS-DATE-MM             TO  HL1-MM.
093210000000           MOVE WS-DATE-DD             TO  HL1-DD.
093220000000
093230000000           ACCEPT HL1-TIME           FROM  TIME.
093240000000           INSPECT HL1-TIME REPLACING ALL  SPACE
093250000000                                       BY  ":".
093260000000           MOVE "."                    TO  HL1-TIME-DOT.
093270000000
093280000000      *****
093290000000      *****    Open the Input files.
093300000000      *****
093310000000
093320000000           OPEN    INPUT   DEALER-MAILING-INSTR
093330000000                           DEALER-FILE
093340000000                           DEALER-REP-FILE
093350000000                           DEALER-REP-MAILING-INSTR
093360000000                           DEALER-REP-ADDRESS
093370000000                           INSTITUTION-ADDRESS
093380000000                           INSTITUTION-BRANCH
093390980326                           INVESTOR-JOIN
093400991103991103                     ACCOUNT-TYPE-BK
093410980326                           NOMINEE-ACCOUNT-TYPE-DESCR
093420000000                           NOMINEE-MAILING-INSTRUCTIONS
093430020921R14588                     DEALER-MISC-DETAILS
093440020921R14588                     MASTER-ACCOUNT
093450000000                           STMT-MSG-FILE
093460990116                           STMT-DTA-FILE
093470190612      *RFS183449-Starts
093480190612      *                    ACCOUNT-FILE.
093490190612                           ACCOUNT-FILE
093500190612                           ACCOUNT-FILE-2.
093510190612      *RFS183449-Ends
093520000000      *****
093530000000      *****    Open the Output files.
093540000000      *****
093550000000
093560000000           OPEN    OUTPUT  STMT-PRT-DTL-FILE
093570000000                           STMT-PRT-HDR-FILE
093580000000                           PRINT-FILE.
093590980210
093600991103       ILR-0040.
093610991103
093620991103      *****
093630991103      *****    Load the account type entries.
093640991103      *****
093650991103
093660991103           MOVE SPACES                 TO  WS-ACCTYP-TBL.
093670991103           MOVE ZEROES                 TO  WS-AT-LST.
093680991103
093690991103       ILR-0050.
093700991103
093710991103           READ ACCOUNT-TYPE-BK        AT  END
093720991103               GO TO ILR-0070
093730991103           END-READ
093740991103
093750991103           IF  WS-AT-LST IS NOT LESS THAN  WS-AT-MAX
093760991103               DISPLAY "ACCOUNT TYPES (MFAACTYP) EXCEED PROGRAM MAX"
093770991103               GO TO ILR-EXIT
093780991103           END-IF
093790991103
093800991103           ADD 1                       TO  WS-AT-LST.
093810991103           SET AT1                     TO  WS-AT-LST.
093820991103           MOVE ACCOUNT-TYPE-CODE
093830991103                OF ACCOUNT-TYPE-BK-REC TO  WS-AT-CODE (AT1).
093840991103           MOVE ACCOUNT-TYPE-RULE
093850991103                OF ACCOUNT-TYPE-BK-REC TO  WS-AT-RULE (AT1).
093860991103           GO TO ILR-0050.
093870991103
093880991103       ILR-0070.
093890991103
093900991103           IF  WS-AT-LST NOT GREATER THAN  ZEROES
093910991103               DISPLAY "NO ACCOUNT TYPES (MFAACTYP) FOUND"
093920991103           END-IF.
093930991103
093940991103      **********
093950000000
093960000000       ILR-EXIT.
093970000000           EXIT.
093980991103
093990991103      **********
094000000000      /
094010000000       END-JOB-RTN.
094020000000
094030000000      ******************************************************************
094040000000      *                                                                *
094050000000      *    E N D   J O B   R O U T I N E                               *
094060000000      *                                                                *
094070000000      ******************************************************************
094080000000
094090000000      *****
094100000000      *****    Print the Control Report on a separate page.
094110000000      *****
094120000000
094130000000           MOVE 99                     TO  WS-LNECNT.
094140000000           MOVE "N"                    TO  WS-BYPPRT-SW.
094150000000           MOVE "Y"                    TO  WS-EOJHDG-SW.
094160000000           MOVE "N"                    TO  WS-SELPRT-SW.
094170000000
094180000000           MOVE WS-MANAGEMENT-COMPANY-CODE
094190000000                                       TO  CL2-MANAGEMENT-COMPANY-CODE.
094200000000           MOVE CONTROL-LINE-2         TO  CONTROL-LINE-1.
094210000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
094220000000
094230000000           MOVE SPACES                 TO  CONTROL-LINE-1.
094240000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
094250000000
094260000000           MOVE SPACES                 TO  CONTROL-LINE-1.
094270000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
094280000000
094290000000           MOVE SPACES                 TO  CONTROL-LINE-1.
094300000000           MOVE WS-STMT-DTA-RLIT       TO  CL1-LIT.
094310000000           MOVE WS-STMT-DTA-RCNT       TO  CL1-CNT.
094320000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
094330000000
094340000000           MOVE SPACES                 TO  CONTROL-LINE-1.
094350000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
094360000000
094370000000           IF  WS-STMT-DTA-BCNT
094380000000                          IS GREATER THAN  ZEROES
094390000000               MOVE SPACES             TO  CONTROL-LINE-1
094400000000               MOVE WS-STMT-DTA-BLIT   TO  CL1-LIT
094410000000               MOVE WS-STMT-DTA-BCNT   TO  CL1-CNT
094420000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
094430000000               MOVE "Y"                TO  WS-BYPPRT-SW.
094440000000
094450000000           IF  WS-STMT-DTA-BCNT-00
094460000000                          IS GREATER THAN  ZEROES
094470000000               MOVE SPACES             TO  CONTROL-LINE-1
094480000000               MOVE WS-STMT-DTA-BLIT-00
094490000000                                       TO  CL1-LIT
094500000000               MOVE WS-STMT-DTA-BCNT-00
094510000000                                       TO  CL1-CNT
094520000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
094530000000               MOVE "Y"                TO  WS-BYPPRT-SW.
094540000000
094550000000           IF  WS-STMT-DTA-BCNT-05
094560000000                          IS GREATER THAN  ZEROES
094570000000               MOVE SPACES             TO  CONTROL-LINE-1
094580000000               MOVE WS-STMT-DTA-BLIT-05
094590000000                                       TO  CL1-LIT
094600000000               MOVE WS-STMT-DTA-BCNT-05
094610000000                                       TO  CL1-CNT
094620000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
094630000000               MOVE "Y"                TO  WS-BYPPRT-SW.
094640000000
094650000000           IF  WS-STMT-DTA-BCNT-10
094660000000                          IS GREATER THAN  ZEROES
094670000000               MOVE SPACES             TO  CONTROL-LINE-1
094680000000               MOVE WS-STMT-DTA-BLIT-10
094690000000                                       TO  CL1-LIT
094700000000               MOVE WS-STMT-DTA-BCNT-10
094710000000                                       TO  CL1-CNT
094720000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
094730000000               MOVE "Y"                TO  WS-BYPPRT-SW.
094740010313
094750010313rf8633     IF  WS-STMT-DTA-BCNT-12
094760010313                          IS GREATER THAN  ZEROES
094770010313               MOVE SPACES             TO  CONTROL-LINE-1
094780010313               MOVE WS-STMT-DTA-BLIT-12
094790010313                                       TO  CL1-LIT
094800010313               MOVE WS-STMT-DTA-BCNT-12
094810010313                                       TO  CL1-CNT
094820010313               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
094830010313rf8633         MOVE "Y"                TO  WS-BYPPRT-SW.
094840000000
094850000000           IF  WS-STMT-DTA-BCNT-15
094860000000                          IS GREATER THAN  ZEROES
094870000000               MOVE SPACES             TO  CONTROL-LINE-1
094880000000               MOVE WS-STMT-DTA-BLIT-15
094890000000                                       TO  CL1-LIT
094900000000               MOVE WS-STMT-DTA-BCNT-15
094910000000                                       TO  CL1-CNT
094920000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
094930000000               MOVE "Y"                TO  WS-BYPPRT-SW.
094940000000
094950000000           IF  WS-STMT-DTA-BCNT-20
094960000000                          IS GREATER THAN  ZEROES
094970000000               MOVE SPACES             TO  CONTROL-LINE-1
094980000000               MOVE WS-STMT-DTA-BLIT-20
094990000000                                       TO  CL1-LIT
095000000000               MOVE WS-STMT-DTA-BCNT-20
095010000000                                       TO  CL1-CNT
095020000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095030000000               MOVE "Y"                TO  WS-BYPPRT-SW.
095040000000
095050010323RF8633     IF  WS-STMT-DTA-BCNT-25
095060010323                          IS GREATER THAN  ZEROES
095070010323               MOVE SPACES             TO  CONTROL-LINE-1
095080010323               MOVE WS-STMT-DTA-BLIT-25
095090010323                                       TO  CL1-LIT
095100010323               MOVE WS-STMT-DTA-BCNT-25
095110010323                                       TO  CL1-CNT
095120010323               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095130010323               MOVE "Y"                TO  WS-BYPPRT-SW.
095140010323
095150991126991126     IF  WS-STMT-DTA-BCNT-30
095160991126                          IS GREATER THAN  ZEROES
095170991126               MOVE SPACES             TO  CONTROL-LINE-1
095180991126               MOVE WS-STMT-DTA-BLIT-30
095190991126                                       TO  CL1-LIT
095200991126               MOVE WS-STMT-DTA-BCNT-30
095210991126                                       TO  CL1-CNT
095220991126               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095230991126               MOVE "Y"                TO  WS-BYPPRT-SW.
095240991126
095250991126991126     IF  WS-STMT-DTA-BCNT-35
095260991126                          IS GREATER THAN  ZEROES
095270991126               MOVE SPACES             TO  CONTROL-LINE-1
095280991126               MOVE WS-STMT-DTA-BLIT-35
095290991126                                       TO  CL1-LIT
095300991126               MOVE WS-STMT-DTA-BCNT-35
095310991126                                       TO  CL1-CNT
095320991126               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095330991126               MOVE "Y"                TO  WS-BYPPRT-SW.
095340991126
095350001114001114     IF  WS-STMT-DTA-BCNT-40
095360001114                          IS GREATER THAN  ZEROES
095370001114               MOVE SPACES             TO  CONTROL-LINE-1
095380001114               MOVE WS-STMT-DTA-BLIT-40
095390001114                                       TO  CL1-LIT
095400001114               MOVE WS-STMT-DTA-BCNT-40
095410001114                                       TO  CL1-CNT
095420001114               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095430001114               MOVE "Y"                TO  WS-BYPPRT-SW
095440001114           END-IF.
095450001114
095460001114001114     IF  WS-STMT-DTA-BCNT-43
095470001114                          IS GREATER THAN  ZEROES
095480001114               MOVE SPACES             TO  CONTROL-LINE-1
095490001114               MOVE WS-STMT-DTA-BLIT-43
095500001114                                       TO  CL1-LIT
095510001114               MOVE WS-STMT-DTA-BCNT-43
095520001114                                       TO  CL1-CNT
095530001114               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095540001114               MOVE "Y"                TO  WS-BYPPRT-SW
095550001114           END-IF.
095560001114
095570001114001114     IF  WS-STMT-DTA-BCNT-45
095580001114                          IS GREATER THAN  ZEROES
095590001114               MOVE SPACES             TO  CONTROL-LINE-1
095600001114               MOVE WS-STMT-DTA-BLIT-45
095610001114                                       TO  CL1-LIT
095620001114               MOVE WS-STMT-DTA-BCNT-45
095630001114                                       TO  CL1-CNT
095640001114               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095650001114               MOVE "Y"                TO  WS-BYPPRT-SW
095660001114           END-IF.
095670001114
095680000000           IF  WS-STMT-DTA-BCNT-50
095690000000                          IS GREATER THAN  ZEROES
095700000000               MOVE SPACES             TO  CONTROL-LINE-1
095710000000               MOVE WS-STMT-DTA-BLIT-50
095720000000                                       TO  CL1-LIT
095730000000               MOVE WS-STMT-DTA-BCNT-50
095740000000                                       TO  CL1-CNT
095750000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095760000000               MOVE "Y"                TO  WS-BYPPRT-SW.
095770000000
095780000000           IF  WS-STMT-DTA-BCNT-60
095790000000                          IS GREATER THAN  ZEROES
095800000000               MOVE SPACES             TO  CONTROL-LINE-1
095810000000               MOVE WS-STMT-DTA-BLIT-60
095820000000                                       TO  CL1-LIT
095830000000               MOVE WS-STMT-DTA-BCNT-60
095840000000                                       TO  CL1-CNT
095850000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095860000000               MOVE "Y"                TO  WS-BYPPRT-SW.
095870000000
095880991013           IF  WS-STMT-DTA-BCNT-62
095890991013                          IS GREATER THAN  ZEROES
095900991013               MOVE SPACES             TO  CONTROL-LINE-1
095910991013               MOVE WS-STMT-DTA-BLIT-62
095920991013                                       TO  CL1-LIT
095930991013               MOVE WS-STMT-DTA-BCNT-62
095940991013                                       TO  CL1-CNT
095950991013               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
095960991013               MOVE "Y"                TO  WS-BYPPRT-SW.
095970991013
095980001114001114     IF  WS-STMT-DTA-BCNT-67
095990001114                          IS GREATER THAN  ZEROES
096000001114               MOVE SPACES             TO  CONTROL-LINE-1
096010001114               MOVE WS-STMT-DTA-BLIT-67
096020001114                                       TO  CL1-LIT
096030001114               MOVE WS-STMT-DTA-BCNT-67
096040001114                                       TO  CL1-CNT
096050001114               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096060001114               MOVE "Y"                TO  WS-BYPPRT-SW
096070001114           END-IF.
096080001114
096090000000           IF  WS-STMT-DTA-BCNT-70
096100000000                          IS GREATER THAN  ZEROES
096110000000               MOVE SPACES             TO  CONTROL-LINE-1
096120000000               MOVE WS-STMT-DTA-BLIT-70
096130000000                                       TO  CL1-LIT
096140000000               MOVE WS-STMT-DTA-BCNT-70
096150000000                                       TO  CL1-CNT
096160000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096170000000               MOVE "Y"                TO  WS-BYPPRT-SW.
096180000000
096190000000           IF  WS-STMT-DTA-BCNT-82
096200000000                          IS GREATER THAN  ZEROES
096210000000               MOVE SPACES             TO  CONTROL-LINE-1
096220000000               MOVE WS-STMT-DTA-BLIT-82
096230000000                                       TO  CL1-LIT
096240000000               MOVE WS-STMT-DTA-BCNT-82
096250000000                                       TO  CL1-CNT
096260000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096270000000               MOVE "Y"                TO  WS-BYPPRT-SW.
096280000000
096290991013           IF  WS-STMT-DTA-BCNT-84
096300991013                          IS GREATER THAN  ZEROES
096310991013               MOVE SPACES             TO  CONTROL-LINE-1
096320991013               MOVE WS-STMT-DTA-BLIT-84
096330991013                                       TO  CL1-LIT
096340991013               MOVE WS-STMT-DTA-BCNT-84
096350991013                                       TO  CL1-CNT
096360991013               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096370991013               MOVE "Y"                TO  WS-BYPPRT-SW.
096380991013
096390001114001114     IF  WS-STMT-DTA-BCNT-90
096400001114                          IS GREATER THAN  ZEROES
096410001114               MOVE SPACES             TO  CONTROL-LINE-1
096420001114               MOVE WS-STMT-DTA-BLIT-90
096430001114                                       TO  CL1-LIT
096440001114               MOVE WS-STMT-DTA-BCNT-90
096450001114                                       TO  CL1-CNT
096460001114               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096470001114               MOVE "Y"                TO  WS-BYPPRT-SW
096480001114           END-IF.
096490001114
096500000000           IF  WS-BYPPRT-SW   IS EQUAL TO  "Y"
096510000000               MOVE SPACES             TO  CONTROL-LINE-1
096520000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
096530000000
096540000000           IF  WS-STMT-DTA-SCNT-01
096550000000                          IS GREATER THAN  ZEROES
096560000000               MOVE SPACES                 TO  CONTROL-LINE-1
096570000000               MOVE WS-STMT-DTA-SLIT-01
096580000000                                       TO  CL1-LIT
096590000000               MOVE WS-STMT-DTA-SCNT-01
096600000000                                       TO  CL1-CNT
096610000000               PERFORM PRINT-CTL-LINE     THRU PCL-EXIT
096620000000               MOVE "Y"                TO  WS-SELPRT-SW.
096630000000
096640000000           IF  WS-STMT-DTA-SCNT-05
096650000000                          IS GREATER THAN  ZEROES
096660000000               MOVE SPACES             TO  CONTROL-LINE-1
096670000000               MOVE WS-STMT-DTA-SLIT-05
096680000000                                       TO  CL1-LIT
096690000000               MOVE WS-STMT-DTA-SCNT-05
096700000000                                       TO  CL1-CNT
096710000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096720000000               MOVE "Y"                TO  WS-SELPRT-SW.
096730000000
096740000000           IF  WS-STMT-DTA-SCNT-10
096750000000                          IS GREATER THAN  ZEROES
096760000000               MOVE SPACES             TO  CONTROL-LINE-1
096770000000               MOVE WS-STMT-DTA-SLIT-10
096780000000                                       TO  CL1-LIT
096790000000               MOVE WS-STMT-DTA-SCNT-10
096800000000                                       TO  CL1-CNT
096810000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096820000000               MOVE "Y"                TO  WS-SELPRT-SW.
096830000000
096840000000           IF  WS-STMT-DTA-SCNT-20
096850000000                          IS GREATER THAN  ZEROES
096860000000               MOVE SPACES             TO  CONTROL-LINE-1
096870000000               MOVE WS-STMT-DTA-SLIT-20
096880000000                                       TO  CL1-LIT
096890000000               MOVE WS-STMT-DTA-SCNT-20
096900000000                                       TO  CL1-CNT
096910000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
096920000000               MOVE "Y"                TO  WS-SELPRT-SW.
096930000000
096940060618R35042     IF  WS-STMT-DTA-SCNT-21
096950060618  ¦                       IS GREATER THAN  ZEROES
096960060618  ¦            MOVE SPACES             TO  CONTROL-LINE-1
096970060618  ¦            MOVE WS-STMT-DTA-SLIT-21
096980060618  ¦                                    TO  CL1-LIT
096990060618  ¦            MOVE WS-STMT-DTA-SCNT-21
097000060618  ¦                                    TO  CL1-CNT
097010060618  V            PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097020060618R35042         MOVE "Y"                TO  WS-SELPRT-SW.
097030060618
097040130408117038     IF  WS-STMT-DTA-SCNT-22
097050130408  ¦                       IS GREATER THAN  ZEROES
097060130408  ¦            MOVE SPACES             TO  CONTROL-LINE-1
097070130408  ¦            MOVE WS-STMT-DTA-SLIT-22
097080130408  ¦                                    TO  CL1-LIT
097090130408  ¦            MOVE WS-STMT-DTA-SCNT-22
097100130408  ¦                                    TO  CL1-CNT
097110130408  V            PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097120130408117038         MOVE "Y"                TO  WS-SELPRT-SW.
097130130408
097140010323RF8633     IF  WS-STMT-DTA-SCNT-25
097150010323                          IS GREATER THAN  ZEROES
097160010323               MOVE SPACES             TO  CONTROL-LINE-1
097170010323               MOVE WS-STMT-DTA-SLIT-25
097180010323                                       TO  CL1-LIT
097190010323               MOVE WS-STMT-DTA-SCNT-25
097200010323                                       TO  CL1-CNT
097210010323               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097220010323               MOVE "Y"                TO  WS-SELPRT-SW.
097230010323
097240001129           IF  WS-STMT-DTA-SCNT-30
097250001129                          IS GREATER THAN  ZEROES
097260001129               MOVE SPACES             TO  CONTROL-LINE-1
097270001129               MOVE WS-STMT-DTA-SLIT-30
097280001129                                       TO  CL1-LIT
097290001129               MOVE WS-STMT-DTA-SCNT-30
097300001129                                       TO  CL1-CNT
097310001129               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097320001129               MOVE "Y"                TO  WS-SELPRT-SW.
097330001129
097340040406R20870     IF  WS-STMT-DTA-SCNT-33
097350040406                          IS GREATER THAN  ZEROES
097360040406               MOVE SPACES             TO  CONTROL-LINE-1
097370040406               MOVE WS-STMT-DTA-SLIT-33
097380040406                                       TO  CL1-LIT
097390040406               MOVE WS-STMT-DTA-SCNT-33
097400040406                                       TO  CL1-CNT
097410040406               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097420040406               MOVE "Y"                TO  WS-SELPRT-SW.
097430040406
097440001129           IF  WS-STMT-DTA-SCNT-35
097450001129                          IS GREATER THAN  ZEROES
097460001129               MOVE SPACES             TO  CONTROL-LINE-1
097470001129               MOVE WS-STMT-DTA-SLIT-35
097480001129                                       TO  CL1-LIT
097490001129               MOVE WS-STMT-DTA-SCNT-35
097500001129                                       TO  CL1-CNT
097510001129               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097520001129               MOVE "Y"                TO  WS-SELPRT-SW.
097530001129
097540040406R20870     IF  WS-STMT-DTA-SCNT-38
097550040406                          IS GREATER THAN  ZEROES
097560040406               MOVE SPACES             TO  CONTROL-LINE-1
097570040406               MOVE WS-STMT-DTA-SLIT-38
097580040406                                       TO  CL1-LIT
097590040406               MOVE WS-STMT-DTA-SCNT-38
097600040406                                       TO  CL1-CNT
097610040406               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097620040406               MOVE "Y"                TO  WS-SELPRT-SW.
097630040406
097640981207           IF  WS-STMT-DTA-SCNT-60
097650981207                          IS GREATER THAN  ZEROES
097660981207               MOVE SPACES             TO  CONTROL-LINE-1
097670981207               MOVE WS-STMT-DTA-SLIT-60
097680981207                                       TO  CL1-LIT
097690981207               MOVE WS-STMT-DTA-SCNT-60
097700981207                                       TO  CL1-CNT
097710981207               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097720981207               MOVE "Y"                TO  WS-SELPRT-SW.
097730981207
097740000000           IF  WS-STMT-DTA-SCNT-80
097750000000                          IS GREATER THAN  ZEROES
097760000000               MOVE SPACES             TO  CONTROL-LINE-1
097770000000               MOVE WS-STMT-DTA-SLIT-80
097780000000                                       TO  CL1-LIT
097790000000               MOVE WS-STMT-DTA-SCNT-80
097800000000                                       TO  CL1-CNT
097810000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
097820000000               MOVE "Y"                TO  WS-SELPRT-SW.
097830000000
097840000000           IF  WS-SELPRT-SW   IS EQUAL TO  "Y"
097850000000               MOVE SPACES             TO  CONTROL-LINE-1
097860000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
097870000000
097880000000           MOVE SPACES                 TO  CONTROL-LINE-1.
097890000000           MOVE WS-STMT-PRT-HDR-WLIT   TO  CL1-LIT.
097900000000           MOVE WS-STMT-PRT-HDR-WCNT   TO  CL1-CNT.
097910000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
097920000000
097930000000           MOVE SPACES                 TO  CONTROL-LINE-1.
097940000000           MOVE WS-STMT-PRT-DTL-WLIT   TO  CL1-LIT.
097950000000           MOVE WS-STMT-PRT-DTL-WCNT   TO  CL1-CNT.
097960000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
097970000000
097980000000      *****
097990000000      *****    Close all the files.
098000000000      *****
098010000000
098020000000           CLOSE           DEALER-MAILING-INSTR
098030000000                           DEALER-FILE
098040000000                           DEALER-REP-FILE
098050000000                           DEALER-REP-MAILING-INSTR
098060000000                           DEALER-REP-ADDRESS
098070000000                           INSTITUTION-ADDRESS
098080000000                           INSTITUTION-BRANCH
098090980326                           INVESTOR-JOIN
098100980326                           NOMINEE-ACCOUNT-TYPE-DESCR
098110000000                           NOMINEE-MAILING-INSTRUCTIONS
098120020921R14588                     DEALER-MISC-DETAILS
098130020921R14588                     MASTER-ACCOUNT
098140000000                           STMT-MSG-FILE
098150000000                           STMT-PRT-DTL-FILE
098160000000                           STMT-PRT-HDR-FILE
098170991103991103                     ACCOUNT-TYPE-BK
098180000000                           STMT-DTA-FILE
098190990116                           PRINT-FILE
098200190612      *RFS183449-Starts
098210190612      *                    ACCOUNT-FILE.
098220190612                           ACCOUNT-FILE
098230190612                           ACCOUNT-FILE-2.
098240190612      *RFS183449-Ends
098250990116
098260000000
098270000000       EJR-EXIT.
098280000000           EXIT.
098290000000      /
098300000000       PRINT-CTL-LINE.
098310000000
098320000000      ******************************************************************
098330000000      *                                                                *
098340000000      *    P R I N T   C O N T R O L   L I N E   R O U T I N E         *
098350000000      *                                                                *
098360000000      ******************************************************************
098370000000
098380000000      *****
098390000000      *****    If the page is full, start a new page and print the
098400000000      *****    Heading lines.
098410000000      *****
098420000000
098430000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
098440000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
098450000000
098460000000      *****
098470000000      *****    Print the Control line.
098480000000      *****
098490000000
098500000000           WRITE PRINT-RECD          FROM  CONTROL-LINE-1
098510000000                         AFTER  ADVANCING  1.
098520000000           ADD 1                       TO  WS-LNECNT.
098530000000
098540000000       PCL-EXIT.
098550000000           EXIT.
098560000000      /
098570000000       PRINT-HDG-LINE.
098580000000
098590000000      ******************************************************************
098600000000      *                                                                *
098610000000      *    P R I N T   H E A D I N G   L I N E   R O U T I N E         *
098620000000      *                                                                *
098630000000      ******************************************************************
098640000000
098650000000      *****
098660000000      *****    Print Heading line 1 on a new page.
098670000000      *****
098680000000
098690000000           WRITE PRINT-RECD          FROM  HEADING-LINE-1
098700000000                          AFTER ADVANCING  PAGE.
098710000000           ADD 1                       TO  WS-PGECNT.
098720000000           MOVE 1                      TO  WS-LNECNT.
098730000000
098740000000      *****
098750000000      *****    Format and print Heading line 2.
098760000000      *****
098770000000
098780000000           MOVE WS-PGECNT              TO  HL2-PGECNT.
098790000000
098800000000           WRITE PRINT-RECD          FROM  HEADING-LINE-2
098810000000                          AFTER ADVANCING  1.
098820000000           ADD 1                       TO  WS-LNECNT.
098830000000
098840000000      *****
098850000000      *****    Print Heading line 3, if required.
098860000000      *****
098870000000
098880000000           IF  WS-EOJHDG-SW   IS EQUAL TO  "Y"
098890000000               GO TO PHL-0010.
098900000000
098910000000           WRITE PRINT-RECD          FROM  HEADING-LINE-3
098920000000                          AFTER ADVANCING  2.
098930000000           ADD 2                       TO  WS-LNECNT.
098940000000
098950000000       PHL-0010.
098960000000
098970000000      *****
098980000000      *****    Format and print a blank line.
098990000000      *****
099000000000
099010000000           MOVE SPACES                 TO  PRINT-RECD.
099020000000           WRITE PRINT-RECD
099030000000                          AFTER ADVANCING  1.
099040000000           ADD 1                       TO  WS-LNECNT.
099050000000
099060000000       PHL-EXIT.
099070000000           EXIT.
099080990116      /
099090990116       GET-DEALER-ACCOUNT.
099100990116           INITIALIZE MFAACCNTP OF ACCOUNT-REC.
099110990116           MOVE CURR-ACCOUNT
099120020612                    TO  ACCOUNT-NO OF ACCOUNT-REC
099130990116
099140990116           READ ACCOUNT-FILE
099150990116               INVALID KEY
099160990116                DISPLAY "INVALID DEALER ACCOUNT NO"
099170990116                GO TO GDA-EXIT.
099180990116
099190990116           MOVE DEALER-ACCOUNT-NO OF ACCOUNT-REC
099200990116                TO IS-DEALER-ACCOUNT-NO.
099210990116      /
099220990116       GDA-EXIT.
099230990116           EXIT.
099240010502
099250010502      *****************************************************
099260010502
099270010502       GET-NO-TRANS-ACCOUNT.
099280010502
099290010502      *****
099300010502      *****    Search the account type table for the correct
099310010502      *****    account type rule.
099320010502      *****
099330010502
099340010502           SET AT1                     TO  1.
099350010502           IF  WS-AT-LST        IS NOT GREATER THAN  ZEROES
099360010502               GO TO LRA-0020
099370010502           END-IF.
099380010502
099390010502       LRA-0015-000.
099400190619      *RFS 183449-Start
099410190619991103*    IF  AD-ACCOUNT-TYPE-SHORT  IS EQUAL TO  WS-AT-CODE (AT1)
099420190619991103     IF  Lc-Acct-Type-Code      IS EQUAL TO  WS-AT-CODE (AT1)
099430190619      *RFS 183449-End
099440010502               GO TO LRA-0020
099450010502           END-IF
099460010502
099470010502           SET AT1                  UP BY  1.
099480010502
099490010502           IF  AT1    IS NOT GREATER THAN  WS-AT-LST
099500010502               GO TO LRA-0015-000
099510010502           END-IF.
099520010502
099530010502      *************************************************************
099540010502
099550010502       LRA-0020.
099560010502
099570010502      *****
099580010502      *****    Find the account information in the investor account
099590010502      *****    table.  Format the nominee and registered contribution
099600010502      *****    data and print a message, if required.
099610010502      *****
099620010502
099630010502           MOVE SPACES                 TO  WS-NOMINEE.
099640010502           MOVE SPACES                 TO  WS-NOMINEE-CODE.
099650010502           MOVE SPACES                 TO  WS-NOMINEE-NAME.
099660010502           MOVE SPACES                 TO  WS-NOMINEES-ACCT-NO.
099670010502           MOVE ZEROES                 TO  WS-CPG-FIXED.
099680010502           MOVE SPACES                 TO  WS-CPG-FIXED-SIGN.
099690010502           MOVE ZEROES                 TO  WS-REG-CTB-CUR.
099700010502           MOVE SPACES                 TO  WS-REG-CTB-CUR-SIGN.
099710010502
099720010502001128     MOVE ZEROES              TO  WS-AD-MARKET-VALUE-PRIOR-TOT.
099730010502           MOVE ZEROES              TO  WS-AD-MARKET-VALUE-TOT.
099740010502001219     MOVE ZEROES              TO  WS-AD-DEPOSIT1-TOT.
099750010502001219     MOVE ZEROES              TO  WS-AD-DEPOSIT2-TOT.
099760010502001219     MOVE ZEROES              TO  WS-AD-WITHDRAWALS1-TOT.
099770010502001219     MOVE ZEROES              TO  WS-AD-WITHDRAWALS2-TOT.
099780010502001219     MOVE ZEROES              TO  WS-AD-DEP-MINUS-WITHD1.
099790010502001219     MOVE ZEROES              TO  WS-AD-DEP-MINUS-WITHD2.
099800010912R11175     MOVE ZEROES              TO  WS-INCREASE-DECREASE-1.
099810010912R11175     MOVE ZEROES              TO  WS-INCREASE-DECREASE-2.
099820010502
099830010502           SEARCH ALL WS-SIA-ENT
099840010502               AT END
099850010502                   GO TO LRA-0020-0020
099860010502               WHEN WS-SIA-ACCT (SIA)
099870010502                              IS EQUAL TO  AD-ACCOUNT
099880010502                   MOVE WS-SIA-NOMCDE (SIA)
099890010502                                       TO  WS-NOMINEE-CODE
099900010502                   MOVE WS-SIA-NOMNAM (SIA)
099910010502                                       TO  WS-NOMINEE-NAME
099920010502                   MOVE WS-SIA-NOMACCT (SIA)
099930010502                                       TO  WS-NOMINEES-ACCT-NO
099940010502                   MOVE WS-SIA-REGCTB (SIA)
099950010502                                       TO  WS-REG-CTB-CUR
099960010502                   MOVE WS-SIA-REGCTBSGN (SIA)
099970010502                                       TO  WS-REG-CTB-CUR-SIGN.
099980010502001229             MOVE WS-SIA-DEPOSIT-CURRENT (SIA)
099990010502                                       TO  WS-AD-DEPOSIT1-TOT
100000010502                   MOVE WS-SIA-DEPOSIT-INCEPT (SIA)
100010010502                                       TO  WS-AD-DEPOSIT2-TOT
100020010502                   MOVE WS-SIA-WITHDRAW-CURRENT (SIA)
100030010502                                       TO  WS-AD-WITHDRAWALS1-TOT
100040010502                   MOVE WS-SIA-WITHDRAW-INCEPT (SIA)
100050010502                                       TO  WS-AD-WITHDRAWALS2-TOT.
100060010502RF9415             MOVE WS-SIA-MAIL-FLAG (SIA)
100070010502                                   TO  WS-MAILING-ADDRESS-INDICATOR.
100080010502
100090010502       LRA-0020-0020.
100100010502
100110010502      *****
100120010502      *****    If the account is flagged as NOMINEE but no nominee data
100130010502      *****    was found, an error message will be printed and the
100140010502      *****    nominee name will be defaulted to 'NOMINEE'.  If this
100150010502      *****    occurs, the program that creates the Statement Data file
100160010502      *****    should be reviewed.
100170010502      *****
100180010502
100190010502           IF  AD-NOMINEE     IS EQUAL TO  "Y"
100200010502           AND WS-NOMINEE-CODE   EQUAL TO  SPACES
100210010502           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
100220010502           AND WS-NOMINEES-ACCT-NO
100230010502                              IS EQUAL TO  SPACES
100240010502               MOVE SPACES             TO  ERROR-LINE-1
100250010502               SET MSG                 TO  12
100260010502               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
100270010502               MOVE "NOMINEE"          TO  WS-NOMINEE-NAME.
100280010502
100290010502      *****
100300010502      *****    If the account is not flagged as NOMINEE but nominee data
100310010502      *****    is found, an error message will be printed and the
100320010502      *****    nominee data will be set to spaces.  If this occurs, the
100330010502      *****    program that creates the Statement Data file should be
100340010502      *****    reviewed.
100350010502      *****
100360010502
100370010502           IF  AD-NOMINEE     IS EQUAL TO  "Y"
100380010502               GO TO LRA-0020-0030.
100390010502
100400010502           IF  WS-NOMINEE-CODE
100410010502                          IS NOT EQUAL TO  SPACES
100420010502           OR  WS-NOMINEE-NAME
100430010502                          IS NOT EQUAL TO  SPACES
100440010502           OR  WS-NOMINEES-ACCT-NO
100450010502                          IS NOT EQUAL TO  SPACES
100460010502               MOVE SPACES             TO  ERROR-LINE-1
100470010502               SET MSG                 TO  13
100480010502               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
100490010502               MOVE SPACES             TO  WS-NOMINEE-CODE
100500010502               MOVE SPACES             TO  WS-NOMINEE-NAME
100510010502               MOVE SPACES             TO  WS-NOMINEES-ACCT-NO.
100520010502
100530010502       LRA-0020-0030.
100540010502
100550010502      *****
100560010502      *****    Format the nominee data, if applicable.
100570010502      *****
100580010502
100590010502           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
100600010502           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
100610010502           AND WS-NOMINEES-ACCT-NO
100620010502                              IS EQUAL TO  SPACES
100630010502               GO TO LRA-0020-0040.
100640010502
100650010502           IF  WS-NOMINEES-ACCT-NO
100660010502                              IS EQUAL TO  SPACES
100670010502               MOVE WS-NOMINEE-NAME    TO  WS-NOMINEE
100680010502               GO TO LRA-0020-0040.
100690010502
100700010502           STRING WS-NOMINEE-NAME
100710010502                             DELIMITED BY  "  ",
100720010502                  " ("       DELIMITED BY  SIZE,
100730010502                  WS-NOMINEES-ACCT-NO
100740010502                             DELIMITED BY  " ",
100750010502                  ")"        DELIMITED BY  SIZE
100760010502                                     INTO  WS-NOMINEE.
100770010502
100780010502       LRA-0020-0040.
100790010502
100800010502      *****
100810010502      *****    The literals will be formatted depending on the account
100820010502      *****    language code.  If the language code is not handled,
100830010502      *****    default to English.
100840010502      *****
100850010502
100860010502           IF  AD-LANGUAGE    IS EQUAL TO  "E"
100870010502               SET SCL                 TO  1
100880010502               SET SHL                 TO  1
100890010502               SET SL                  TO  1
100900010502               SET SPSM                TO  1
100910010502               SET SPS2                TO  1
100920010502               SET FML                 TO  1
100930010502               SET SML                 TO  1
100940010502               GO TO LRA-0030.
100950010502
100960010502           IF  AD-LANGUAGE    IS EQUAL TO  "F"
100970010502               SET SCL                 TO  2
100980010502               SET SHL                 TO  2
100990010502               SET SL                  TO  2
101000010502               SET SPSM                TO  2
101010010502               SET SPS2                TO  2
101020010502               SET FML                 TO  2.
101030010502               SET SML                 TO  2.
101040010502               GO TO LRA-0030.
101050010502
101060010502           SET SCL                     TO  1.
101070010502           SET SHL                     TO  1.
101080010502           SET SL                      TO  1.
101090010502           SET SPSM                    TO  1.
101100010502           SET SPS2                    TO  1.
101110010502           SET FML                     TO  1.
101120010502           SET SML                     TO  1.
101130010502
101140010502       LRA-0030.
101150010502
101160010502RF9415     SET DI                      TO  1.
101170010502RF9415     SET SFC                     TO  1.
101180010502RF9415     SET SFCI                    TO  1.
101190010502RF9415     SET ANN                     TO  1.
101200010502
101210010502       LRA-0030-0010-0010.
101220010502
101230010502           IF DI    IS GREATER THAN   WS-DI-LST
101240010502              INITIALIZE DEALER-SECTION
101250010502              MOVE WS-DEALER-INFO (DI) TO DEALER-SECTION
101260010502              GO TO LRA-0030-0010-0020
101270010502           END-IF
101280010502
101290010502           INITIALIZE DEALER-SECTION.
101300010502           MOVE WS-DEALER-INFO (DI)      TO  DEALER-SECTION.
101310010502
101320010517           IF DL-ACCOUNT2 OF DEALER-SECTION IS
101330010517                                       NOT EQUAL TO AD-ACCOUNT
101340010517      *    IF DL-DEALER   OF DEALER-SECTION IS
101350010517      *                                NOT EQUAL TO AD-DEALER
101360010502              SET DI                   UP BY 1
101370010502              GO TO LRA-0030-0010-0010
101380010502           END-IF.
101390010502
101400010502       LRA-0030-0010-0020.
101410010502
101420010502           IF WS-STMT-DTA-SW-40         IS EQUAL TO "N"
101430010502              INITIALIZE SEG-FUND-CONTRACT-DETAILS
101440010515              MOVE ZEROS      TO  CD-START-DATE1
101450010604              MOVE ZEROS      TO  CD-FIRST-TRADE-DATE
101460010515              MOVE ZEROS      TO  CD-MATURITY-DATE
101470010502              GO TO LRA-0030-0010-0030
101480010502           END-IF
101490010502
101500010502           IF SFC   IS GREATER THAN   WS-SFC-LST
101510010502              INITIALIZE SEG-FUND-CONTRACT-DETAILS
101520010515              SET SFC                DOWN BY 1
101530010502              MOVE WS-SEG-FUND-CONTRACT-DETAILS (SFC)
101540010502                            TO SEG-FUND-CONTRACT-DETAILS
101550010502              GO TO LRA-0030-0010-0030
101560010502           END-IF
101570010502
101580010502           INITIALIZE SEG-FUND-CONTRACT-DETAILS.
101590010502           MOVE WS-SEG-FUND-CONTRACT-DETAILS (SFC)
101600010502                            TO SEG-FUND-CONTRACT-DETAILS.
101610010502
101620010502           IF CD-ACCOUNT OF SEG-FUND-CONTRACT-DETAILS
101630010502                                       NOT EQUAL TO AD-ACCOUNT
101640010502              SET SFC                  UP BY 1
101650010502              GO TO LRA-0030-0010-0020
101660010502           END-IF.
101670010502
101680010502       LRA-0030-0010-0030.
101690010502
101700010502           IF WS-STMT-DTA-SW-43         IS EQUAL TO "N"
101710010502              INITIALIZE SEG-FUND-CONTRACT-INVESTMENT
101720010502              GO TO LRA-0030-0010-0040
101730010502           END-IF
101740010502
101750010502           IF SFCI  IS GREATER THAN   WS-SFCI-LST
101760010502              INITIALIZE SEG-FUND-CONTRACT-INVESTMENT
101770010502              MOVE WS-SEG-FUND-CONTRACT-INV (SFCI)
101780010502                           TO  SEG-FUND-CONTRACT-INVESTMENT
101790010502              GO TO LRA-0030-0010-0040
101800010502           END-IF
101810010502
101820010502           INITIALIZE SEG-FUND-CONTRACT-INVESTMENT.
101830010502           MOVE WS-SEG-FUND-CONTRACT-INV (SFCI)
101840010502                            TO SEG-FUND-CONTRACT-INVESTMENT.
101850010515
101860010502           IF CI-ACCOUNT OF SEG-FUND-CONTRACT-INVESTMENT
101870010502                                       NOT EQUAL TO AD-ACCOUNT
101880010502              SET SFCI                 UP BY 1
101890010502              GO TO LRA-0030-0010-0030
101900010502           END-IF.
101910010502
101920010502       LRA-0030-0010-0040.
101930010502
101940010502           IF WS-STMT-DTA-SW-45         IS EQUAL TO "N"
101950010502              INITIALIZE ANNUITANT-DETAILS
101960010502              GO TO LRA-0030-0005
101970010502           END-IF
101980010502
101990010502           IF ANN   IS GREATER THAN   WS-ANN-LST
102000010502              INITIALIZE ANNUITANT-DETAILS
102010010502              MOVE WS-ANNUITANT-DETAILS (ANN)
102020010502                           TO  ANNUITANT-DETAILS
102030010502              GO TO LRA-0030-0005
102040010502           END-IF
102050010502
102060010502           INITIALIZE ANNUITANT-DETAILS.
102070010502           MOVE WS-ANNUITANT-DETAILS (ANN)
102080010502                            TO ANNUITANT-DETAILS.
102090010502
102100010502           IF AN-ACCOUNT OF ANNUITANT-DETAILS
102110010502                                       NOT EQUAL TO AD-ACCOUNT
102120010502              SET ANN                  UP BY 1
102130010502              GO TO LRA-0030-0010-0040
102140010502           END-IF.
102150010502
102160010502       LRA-0030-0005.
102170010502
102180010502      *****
102190010502      *****    Format the required fields.
102200010502      *****
102210010502
102220010502           MOVE II-END-DATE            TO  WS-END-DATE.
102230010502           MOVE II-END-DATE            TO  WS-END-DATE-6.
102240010502           MOVE "N"                    TO  WS-PSTCDEPRT-SW.
102250010502           MOVE II-START-DATE          TO  WS-START-DATE.
102260010502001130     MOVE II-START-DATE          TO  WS-SHPL-HD-STDATE.
102270010502           MOVE II-END-DATE            TO  WS-SHPL-HD-ENDATE.
102280010502
102290010604RF9415*    MOVE CD-START-DATE1         TO  WS-SHGL-HD-ISDATE.
102300010604R10243     MOVE CD-FIRST-TRADE-DATE    TO  WS-SHGL-HD-ISDATE.
102310010502RF9415     MOVE CD-MATURITY-DATE       TO  WS-SHGL-HD-ANDATE.
102320010502
102330010502      ***
102340010502      ***     Process Annuitant-name.
102350010502      ***
102360010502
102370010502           MOVE SPACES                 TO  WS-ADRTMP.
102380010528R10243     MOVE SPACES                 TO  WS-ANNUITANT-NAME.
102390010502           STRING  AN-SALUTATION-CODE
102400010502                             DELIMITED BY  " "
102410010502                                     INTO  WS-ADRTMP.
102420010502
102430010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
102440010502               STRING AN-FIRST-NAME
102450010502                             DELIMITED BY  "  "
102460010502                                     INTO  WS-ADRTMP
102470010502             ELSE
102480010502               STRING WS-ADRTMP
102490010502                             DELIMITED BY  "  ",
102500010502                      " "    DELIMITED BY  SIZE,
102510010502                      AN-FIRST-NAME
102520010502                             DELIMITED BY  "  "
102530010502                                     INTO  WS-ANNUITANT-NAME
102540010502               MOVE WS-ANNUITANT-NAME  TO  WS-ADRTMP.
102550010502
102560010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
102570010502               STRING AN-INITIALS
102580010502                             DELIMITED BY  "  "
102590010502                                     INTO  WS-ADRTMP
102600010502             ELSE
102610010502               STRING WS-ADRTMP
102620010502                             DELIMITED BY  "  ",
102630010502                      " "    DELIMITED BY  SIZE,
102640010502                      AN-INITIALS
102650010502                             DELIMITED BY  "  "
102660010502                                     INTO  WS-ANNUITANT-NAME
102670010502               MOVE WS-ANNUITANT-NAME  TO  WS-ADRTMP.
102680010502
102690010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
102700010502               STRING AN-LAST-NAME
102710010502                             DELIMITED BY  "  "
102720010502                                     INTO  WS-ADRTMP
102730010502             ELSE
102740010502               STRING WS-ADRTMP
102750010502                             DELIMITED BY  "  ",
102760010502                      " "    DELIMITED BY  SIZE,
102770010502                      AN-LAST-NAME
102780010502                             DELIMITED BY  "  "
102790010502                                     INTO  WS-ANNUITANT-NAME
102800010502               MOVE WS-ANNUITANT-NAME  TO  WS-ADRTMP.
102810010502
102820010502           MOVE WS-ADRTMP              TO  WS-ANNUITANT-NAME.
102830010502
102840010912           EVALUATE WS-SHPL-HD-STDATE-MM
102850010912              WHEN 01
102860010912                  MOVE DI-JAN (SML) TO WS-MONTH-LIT1
102870010912                  MOVE DI-JAN (SML) TO WS-MONTH-LIT1-F
102880010912R11175            MOVE FDI-JAN (FML)   TO WS-MONTH-LIT6
102890010912                  MOVE FDI-JAN (FML)   TO WS-MONTH-LIT6-F
102900010912              WHEN 02
102910010912                  MOVE DI-FEB (SML) TO WS-MONTH-LIT1
102920010912                  MOVE DI-FEB (SML) TO WS-MONTH-LIT1-F
102930010912R11175            MOVE FDI-FEB (FML)   TO WS-MONTH-LIT6
102940010912                  MOVE FDI-FEB (FML)   TO WS-MONTH-LIT6-F
102950010912              WHEN 03
102960010912                  MOVE DI-MAR (SML) TO WS-MONTH-LIT1
102970010912                  MOVE DI-MAR (SML) TO WS-MONTH-LIT1-F
102980010912R11175            MOVE FDI-MAR (FML)   TO WS-MONTH-LIT6
102990010912                  MOVE FDI-MAR (FML)   TO WS-MONTH-LIT6-F
103000010912              WHEN 04
103010010912                  MOVE DI-APR (SML) TO WS-MONTH-LIT1
103020010912                  MOVE DI-APR (SML) TO WS-MONTH-LIT1-F
103030010912R11175            MOVE FDI-APR (FML)   TO WS-MONTH-LIT6
103040010912                  MOVE FDI-APR (FML)   TO WS-MONTH-LIT6-F
103050010912              WHEN 05
103060010912                  MOVE DI-MAY (SML) TO WS-MONTH-LIT1
103070010912                  MOVE DI-MAY (SML) TO WS-MONTH-LIT1-F
103080010912R11175            MOVE FDI-MAY (FML)   TO WS-MONTH-LIT6
103090010912                  MOVE FDI-MAY (FML)   TO WS-MONTH-LIT6-F
103100010912              WHEN 06
103110010912                  MOVE DI-JUN (SML) TO WS-MONTH-LIT1
103120010912                  MOVE DI-JUN (SML) TO WS-MONTH-LIT1-F
103130010912R11175            MOVE FDI-JUN (FML)   TO WS-MONTH-LIT6
103140010912                  MOVE FDI-JUN (FML)   TO WS-MONTH-LIT6-F
103150010912              WHEN 07
103160010912                  MOVE DI-JUL (SML) TO WS-MONTH-LIT1
103170010912                  MOVE DI-JUL (SML) TO WS-MONTH-LIT1-F
103180010912R11175            MOVE FDI-JUL (FML)   TO WS-MONTH-LIT6
103190010912                  MOVE FDI-JUL (FML)   TO WS-MONTH-LIT6-F
103200010912              WHEN 08
103210010912                  MOVE DI-AUG (SML) TO WS-MONTH-LIT1
103220010912                  MOVE DI-AUG (SML) TO WS-MONTH-LIT1-F
103230010912R11175            MOVE FDI-AUG (FML)   TO WS-MONTH-LIT6
103240010912                  MOVE FDI-AUG (FML)   TO WS-MONTH-LIT6-F
103250010912              WHEN 09
103260010912                  MOVE DI-SEP (SML) TO WS-MONTH-LIT1
103270010912                  MOVE DI-SEP (SML) TO WS-MONTH-LIT1-F
103280010912R11175            MOVE FDI-SEP (FML)   TO WS-MONTH-LIT6
103290010912                  MOVE FDI-SEP (FML)   TO WS-MONTH-LIT6-F
103300010912              WHEN 10
103310010912                  MOVE DI-OCT (SML) TO WS-MONTH-LIT1
103320010912                  MOVE DI-OCT (SML) TO WS-MONTH-LIT1-F
103330010912R11175            MOVE FDI-OCT (FML)   TO WS-MONTH-LIT6
103340010912                  MOVE FDI-OCT (FML)   TO WS-MONTH-LIT6-F
103350010912              WHEN 11
103360010912                  MOVE DI-NOV (SML) TO WS-MONTH-LIT1
103370010912                  MOVE DI-NOV (SML) TO WS-MONTH-LIT1-F
103380010912R11175            MOVE FDI-NOV (FML)   TO WS-MONTH-LIT6
103390010912                  MOVE FDI-NOV (FML)   TO WS-MONTH-LIT6-F
103400010912              WHEN 12
103410010912                  MOVE DI-DEC (SML) TO WS-MONTH-LIT1
103420010912                  MOVE DI-DEC (SML) TO WS-MONTH-LIT1-F
103430010912R11175            MOVE FDI-DEC (FML)   TO WS-MONTH-LIT6
103440010912                  MOVE FDI-DEC (FML)   TO WS-MONTH-LIT6-F
103450010912           END-EVALUATE.
103460010912
103470010912           MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT1-DD.
103480010912R11175     MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT6-DD.
103490010912R11175     MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT6-DD-F.
103500010912           MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT1-YY.
103510010912R11175     MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT6-YY.
103520010912R11175     MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT6-YY-F.
103530010912R11175     MOVE  WS-SHPL-HD-STDATE-CC  TO  WS-MONTH-LIT6-CC.
103540010912R11175     MOVE  WS-SHPL-HD-STDATE-CC  TO  WS-MONTH-LIT6-CC-F.
103550010912           MOVE  WS-SHPL-HD-STDATE-DD  TO  WS-MONTH-LIT1-DD-F.
103560010912           MOVE  WS-SHPL-HD-STDATE-YY  TO  WS-MONTH-LIT1-YY-F.
103570010912           MOVE  "/"                   TO  WS-MONTH-LIT1S.
103580010912           MOVE  "/"                   TO  WS-MONTH-LIT1S-F.
103590010912R11175     MOVE  ","                   TO  WS-MONTH-LIT6S.
103600010502
103610010502           EVALUATE WS-SHPL-HD-ENDATE-MM
103620010502              WHEN 01
103630010502                  MOVE DI-JAN  (SML)   TO WS-MONTH-LIT2
103640010502                  MOVE DI-JAN  (SML)   TO WS-MONTH-LIT2-F
103650010502                  MOVE FDI-JAN (FML)   TO WS-MONTH-LIT3
103660010502                  MOVE FDI-JAN (FML)   TO WS-MONTH-LIT3-F
103670010502              WHEN 02
103680010502                  MOVE DI-FEB  (SML)   TO WS-MONTH-LIT2
103690010502                  MOVE DI-FEB  (SML)   TO WS-MONTH-LIT2-F
103700010502                  MOVE FDI-FEB (FML)   TO WS-MONTH-LIT3
103710010502                  MOVE FDI-FEB (FML)   TO WS-MONTH-LIT3-F
103720010502              WHEN 03
103730010502                  MOVE DI-MAR  (SML)   TO WS-MONTH-LIT2
103740010502                  MOVE DI-MAR  (SML)   TO WS-MONTH-LIT2-F
103750010502                  MOVE FDI-MAR (FML)   TO WS-MONTH-LIT3
103760010502                  MOVE FDI-MAR (FML)   TO WS-MONTH-LIT3-F
103770010502              WHEN 04
103780010502                  MOVE DI-APR  (SML)   TO WS-MONTH-LIT2
103790010502                  MOVE DI-APR  (SML)   TO WS-MONTH-LIT2-F
103800010502                  MOVE FDI-APR (FML)   TO WS-MONTH-LIT3
103810010502                  MOVE FDI-APR (FML)   TO WS-MONTH-LIT3-F
103820010502              WHEN 05
103830010502                  MOVE DI-MAY  (SML)   TO WS-MONTH-LIT2
103840010502                  MOVE DI-MAY  (SML)   TO WS-MONTH-LIT2-F
103850010502                  MOVE FDI-MAY (FML)   TO WS-MONTH-LIT3
103860010502                  MOVE FDI-MAY (FML)   TO WS-MONTH-LIT3-F
103870010502              WHEN 06
103880010502                  MOVE DI-JUN  (SML)   TO WS-MONTH-LIT2
103890010502                  MOVE DI-JUN  (SML)   TO WS-MONTH-LIT2-F
103900010502                  MOVE FDI-JUN (FML)   TO WS-MONTH-LIT3
103910010502                  MOVE FDI-JUN (FML)   TO WS-MONTH-LIT3-F
103920010502              WHEN 07
103930010502                  MOVE DI-JUL  (SML)   TO WS-MONTH-LIT2
103940010502                  MOVE DI-JUL  (SML)   TO WS-MONTH-LIT2-F
103950010502                  MOVE FDI-JUL (FML)   TO WS-MONTH-LIT3
103960010502                  MOVE FDI-JUL (FML)   TO WS-MONTH-LIT3-F
103970010502              WHEN 08
103980010502                  MOVE DI-AUG  (SML)   TO WS-MONTH-LIT2
103990010502                  MOVE DI-AUG  (SML)   TO WS-MONTH-LIT2-F
104000010502                  MOVE FDI-AUG (FML)   TO WS-MONTH-LIT3
104010010502                  MOVE FDI-AUG (FML)   TO WS-MONTH-LIT3-F
104020010502              WHEN 09
104030010502                  MOVE DI-SEP  (SML)   TO WS-MONTH-LIT2
104040010502                  MOVE DI-SEP  (SML)   TO WS-MONTH-LIT2-F
104050010502                  MOVE FDI-SEP (FML)   TO WS-MONTH-LIT3
104060010502                  MOVE FDI-SEP (FML)   TO WS-MONTH-LIT3-F
104070010502              WHEN 10
104080010502                  MOVE DI-OCT  (SML)   TO WS-MONTH-LIT2
104090010502                  MOVE DI-OCT  (SML)   TO WS-MONTH-LIT2-F
104100010502                  MOVE FDI-OCT (FML)   TO WS-MONTH-LIT3
104110010502                  MOVE FDI-OCT (FML)   TO WS-MONTH-LIT3-F
104120010502              WHEN 11
104130010502                  MOVE DI-NOV  (SML)   TO WS-MONTH-LIT2
104140010502                  MOVE DI-NOV  (SML)   TO WS-MONTH-LIT2-F
104150010502                  MOVE FDI-NOV (FML)   TO WS-MONTH-LIT3
104160010502                  MOVE FDI-NOV (FML)   TO WS-MONTH-LIT3-F
104170010502              WHEN 12
104180010502                  MOVE DI-DEC  (SML)   TO WS-MONTH-LIT2
104190010502                  MOVE DI-DEC  (SML)   TO WS-MONTH-LIT2-F
104200010502                  MOVE FDI-DEC (FML)   TO WS-MONTH-LIT3
104210010502                  MOVE FDI-DEC (FML)   TO WS-MONTH-LIT3-F
104220010502           END-EVALUATE.
104230010502
104240010502           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT2-DD.
104250010502           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT3-DD.
104260010502           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT2-DD-F.
104270010502           MOVE  WS-SHPL-HD-ENDATE-DD  TO  WS-MONTH-LIT3-DD-F.
104280010502           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT2-YY.
104290010502           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT3-YY.
104300010502           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT2-YY-F.
104310010502           MOVE  WS-SHPL-HD-ENDATE-YY  TO  WS-MONTH-LIT3-YY-F.
104320010502           MOVE  WS-SHPL-HD-ENDATE-CC  TO  WS-MONTH-LIT3-CC.
104330010502           MOVE  WS-SHPL-HD-ENDATE-CC  TO  WS-MONTH-LIT3-CC-F.
104340010502           MOVE  "/"                   TO  WS-MONTH-LIT2S.
104350010502           MOVE  "/"                   TO  WS-MONTH-LIT2S-F.
104360010502           MOVE  ","                   TO  WS-MONTH-LIT3S.
104370010502
104380010502      ***
104390010502      ***    Process Issue-date.
104400010502      ***
104410010502
104420010502           EVALUATE WS-SHGL-HD-ISDATE-MM
104430010502              WHEN 01
104440010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT4
104450010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT4-F
104460010502              WHEN 02
104470010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT4
104480010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT4-F
104490010502              WHEN 03
104500010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT4
104510010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT4-F
104520010502              WHEN 04
104530010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT4
104540010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT4-F
104550010502              WHEN 05
104560010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT4
104570010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT4-F
104580010502              WHEN 06
104590010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT4
104600010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT4-F
104610010502              WHEN 07
104620010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT4
104630010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT4-F
104640010502              WHEN 08
104650010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT4
104660010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT4-F
104670010502              WHEN 09
104680010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT4
104690010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT4-F
104700010502              WHEN 10
104710010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT4
104720010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT4-F
104730010502              WHEN 11
104740010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT4
104750010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT4-F
104760010502              WHEN 12
104770010912                  MOVE FDIH-DEC (FMLH)   TO WS-MONTH-LIT4
104780010912                  MOVE FDIH-DEC (FMLH)   TO WS-MONTH-LIT4-F
104790010502              WHEN OTHER
104800010502                  MOVE SPACES          TO WS-MONTH-LIT4
104810010502                  MOVE SPACES          TO WS-MONTH-LIT4-F
104820010502                  MOVE SPACES          TO WS-MONTH-COL4
104830010502                  MOVE SPACES          TO WS-MONTH-COL4-F
104840010502                  GO TO LRA-0030-0005-0020
104850010502           END-EVALUATE.
104860010502
104870010502           MOVE  WS-SHGL-HD-ISDATE-DD  TO  WS-MONTH-LIT4-DD.
104880010502           MOVE  WS-SHGL-HD-ISDATE-DD  TO  WS-MONTH-LIT4-DD-F.
104890010502           MOVE  WS-SHGL-HD-ISDATE-YY  TO  WS-MONTH-LIT4-YY.
104900010502           MOVE  WS-SHGL-HD-ISDATE-YY  TO  WS-MONTH-LIT4-YY-F.
104910010502           MOVE  WS-SHGL-HD-ISDATE-CC  TO  WS-MONTH-LIT4-CC.
104920010502           MOVE  WS-SHGL-HD-ISDATE-CC  TO  WS-MONTH-LIT4-CC-F.
104930010502           MOVE  "/"                   TO  WS-MONTH-LIT4S.
104940010612           MOVE  " "                   TO  WS-MONTH-LIT4S-F.
104950010502           MOVE  ","                   TO  WS-MONTH-LIT4S.
104960010502
104970010502      ***
104980010502      ***    Process Annuity-date.
104990010502      ***
105000010502
105010010502       LRA-0030-0005-0020.
105020010502
105030010502           EVALUATE WS-SHGL-HD-ANDATE-MM
105040010502              WHEN 01
105050010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT5
105060010912                  MOVE FDIH-JAN (FMLH)   TO WS-MONTH-LIT5-F
105070010502              WHEN 02
105080010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT5
105090010912                  MOVE FDIH-FEB (FMLH)   TO WS-MONTH-LIT5-F
105100010502              WHEN 03
105110010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT5
105120010912                  MOVE FDIH-MAR (FMLH)   TO WS-MONTH-LIT5-F
105130010502              WHEN 04
105140010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT5
105150010912                  MOVE FDIH-APR (FMLH)   TO WS-MONTH-LIT5-F
105160010502              WHEN 05
105170010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT5
105180010912                  MOVE FDIH-MAY (FMLH)   TO WS-MONTH-LIT5-F
105190010502              WHEN 06
105200010912                  MOVE FDIH-JUN (FMLH)   TO WS-MONTH-LIT5
105210010912                  MOVE FDIH-JUN (FMLH)  TO WS-MONTH-LIT5-F
105220010502              WHEN 07
105230010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT5
105240010912                  MOVE FDIH-JUL (FMLH)   TO WS-MONTH-LIT5-F
105250010502              WHEN 08
105260010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT5
105270010912                  MOVE FDIH-AUG (FMLH)   TO WS-MONTH-LIT5-F
105280010502              WHEN 09
105290010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT5
105300010912                  MOVE FDIH-SEP (FMLH)   TO WS-MONTH-LIT5-F
105310010502              WHEN 10
105320010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT5
105330010912                  MOVE FDIH-OCT (FMLH)   TO WS-MONTH-LIT5-F
105340010502              WHEN 11
105350010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT5
105360010912                  MOVE FDIH-NOV (FMLH)   TO WS-MONTH-LIT5-F
105370010502              WHEN 12
105380010912                  MOVE FDIH-DEC (FMLH)   TO WS-MONTH-LIT5
105390010912                  MOVE FDIH-DEC (FMLH)  TO WS-MONTH-LIT5-F
105400010502              WHEN OTHER
105410010502                  MOVE SPACES          TO WS-MONTH-LIT5
105420010502                  MOVE SPACES          TO WS-MONTH-LIT5-F
105430010502                  MOVE SPACES          TO WS-MONTH-COL5
105440010502                  MOVE SPACES          TO WS-MONTH-COL5-F
105450010502                  GO TO LRA-0030-0005-0030
105460010502           END-EVALUATE.
105470010502
105480010502           MOVE  WS-SHGL-HD-ANDATE-DD  TO  WS-MONTH-LIT5-DD.
105490010502           MOVE  WS-SHGL-HD-ANDATE-DD  TO  WS-MONTH-LIT5-DD-F.
105500010502           MOVE  WS-SHGL-HD-ANDATE-YY  TO  WS-MONTH-LIT5-YY.
105510010502           MOVE  WS-SHGL-HD-ANDATE-YY  TO  WS-MONTH-LIT5-YY-F.
105520010502           MOVE  WS-SHGL-HD-ANDATE-CC  TO  WS-MONTH-LIT5-CC.
105530010502           MOVE  WS-SHGL-HD-ANDATE-CC  TO  WS-MONTH-LIT5-CC-F.
105540010502           MOVE  "/"                   TO  WS-MONTH-LIT5S.
105550010612           MOVE  " "                   TO  WS-MONTH-LIT5S-F.
105560010502           MOVE  ","                   TO  WS-MONTH-LIT5S.
105570010502
105580010502      *****
105590010502      *****    The Statement File Generation (ANNSTMTJSI) formats and
105600010502      *****    creates the Dealer record using the Dealer & Dealer
105610010502      *****    Representative of the first account for the investor.  If
105620010502      *****    an investor has more than one account, the Dealer &
105630010502      *****    Dealer Representative of the accounts may differ.  If
105640010502      *****    this is the case, the Dealer & Dealer Representative
105650010502      *****    names and address lines will be retrieved and formatted.
105660010502      *****
105670010502
105680010502       LRA-0030-0005-0030.
105690010502
105700010502           MOVE SPACES                 TO  WS-BRANCH-ADDR1.
105710010502           MOVE SPACES                 TO  WS-BRANCH-ADDR2.
105720010502           MOVE SPACES                 TO  WS-BRANCH-ADDR3.
105730010502           MOVE SPACES                 TO  WS-BRANCH-ADDR4.
105740010502           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
105750010502           MOVE SPACES                 TO  WS-DEALER-REP-NAMES.
105760010502           MOVE ZEROES                 TO  WS-FAX.
105770010502           MOVE ZEROES                 TO  WS-PHONE.
105780010502           MOVE SPACES                 TO  WS-REP-NAME.
105790010502           MOVE SPACES                 TO  WS-REP-NAME-1.
105800010502
105810010502           IF  AD-SALESREP    IS EQUAL TO  DL-SALESREP
105820010502           AND AD-DEALER      IS EQUAL TO  DL-DEALER
105830010502               MOVE DL-BRANCH-ADDR1    TO  WS-BRANCH-ADDR1
105840010502               MOVE DL-BRANCH-ADDR2    TO  WS-BRANCH-ADDR2
105850010502               MOVE DL-BRANCH-ADDR3    TO  WS-BRANCH-ADDR3
105860010502               MOVE DL-BRANCH-ADDR4    TO  WS-BRANCH-ADDR4
105870010502               STRING DL-DEALER-NAME
105880010502                             DELIMITED BY  "  ",
105890010502                      " /"   DELIMITED BY  SIZE,
105900010502                      DL-REP-NAME
105910010502                             DELIMITED BY  SIZE
105920010502                                     INTO  WS-DEALER-REP-NAMES
105930010502               MOVE DL-FAX             TO  WS-FAX
105940010502               MOVE DL-PHONE           TO  WS-PHONE
105950010502               MOVE DL-REP-NAME        TO  WS-REP-NAME-1
105960010502               GO TO LRA-0030-0090.
105970010502
105980010502      *****
105990010502      *****    The account Dealer & Dealer Representative are different
106000010502      *****    than the dealer.  The following steps will be executed to
106010010502      *****    retrieve and format the Dealer & Dealer Representative
106020010502      *****    names and address lines.
106030010502      *****       1. Read the Dealer file for the name.
106040010502      *****       2. Read the Dealer Representative for the name, fax
106050010502      *****          and phone number.
106060010502      *****       3. Read the Dealer Representative Address for the
106070010502      *****          address lines.
106080010502      *****       4. If required, read the Institution Branch and
106090010502      *****          Institution Address for the address lines.
106100010502      *****       5. Format the address lines field by field, in case
106110010502      *****          one or more of the fields contain spaces.
106120010502      *****       6. Compress the address lines.
106130010502      *****
106140010502
106150010502           INITIALIZE MFADEAP.
106160010502           MOVE AD-DEALER              TO  DEALER-CODE
106170010502                                           OF DEALER-RECD.
106180010502           READ DEALER-FILE
106190010502               INVALID KEY
106200010502                   MOVE "DEALER NAME NOT ON FILE            "
106210010502                                       TO  DEALER-NAME
106220010502                                           OF DEALER-RECD.
106230010502
106240010502           MOVE DEALER-NAME
106250010502                OF DEALER-RECD         TO  WS-DEALER-NAME.
106260010502
106270010502           INITIALIZE MFADEAREP.
106280010502           MOVE AD-DEALER              TO  DEALER-CODE
106290010502                                           OF DEALER-REP-RECD.
106300010502           MOVE AD-SALESREP            TO  DEALER-REP-CODE
106310010502                                           OF DEALER-REP-RECD.
106320010502           READ DEALER-REP-FILE
106330010502               INVALID KEY
106340010502                   INITIALIZE MFADEAREP.
106350010502
106360010502           MOVE SPACES                 TO  WS-ADRTMP.
106370010502           STRING SALUTATION-CODE
106380010502                  OF DEALER-REP-RECD
106390010502                             DELIMITED BY  " "
106400010502                                     INTO  WS-ADRTMP.
106410010502
106420010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
106430010502               STRING FIRST-NAME
106440010502                      OF DEALER-REP-RECD
106450010502                             DELIMITED BY  "  "
106460010502                                     INTO  WS-ADRTMP
106470010502             ELSE
106480010502               STRING WS-ADRTMP
106490010502                             DELIMITED BY  "  ",
106500010502                      " "    DELIMITED BY  SIZE,
106510010502                      FIRST-NAME
106520010502                      OF DEALER-REP-RECD
106530010502                             DELIMITED BY  "  "
106540010502                                     INTO  WS-REP-NAME
106550010502               MOVE WS-REP-NAME        TO  WS-ADRTMP.
106560010502
106570010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
106580010502               STRING INITIALS
106590010502                      OF DEALER-REP-RECD
106600010502                             DELIMITED BY  "  "
106610010502                                     INTO  WS-ADRTMP
106620010502             ELSE
106630010502               STRING WS-ADRTMP
106640010502                             DELIMITED BY  "  ",
106650010502                      " "    DELIMITED BY  SIZE,
106660010502                      INITIALS
106670010502                      OF DEALER-REP-RECD
106680010502                             DELIMITED BY  "  "
106690010502                                     INTO  WS-REP-NAME
106700010502               MOVE WS-REP-NAME        TO  WS-ADRTMP.
106710010502
106720010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
106730010502               STRING LAST-NAME
106740010502                      OF DEALER-REP-RECD
106750010502                             DELIMITED BY  "  "
106760010502                                     INTO  WS-ADRTMP
106770010502             ELSE
106780010502               STRING WS-ADRTMP
106790010502                             DELIMITED BY  "  ",
106800010502                      " "    DELIMITED BY  SIZE,
106810010502                      LAST-NAME
106820010502                      OF DEALER-REP-RECD
106830010502                             DELIMITED BY  "  "
106840010502                                     INTO  WS-REP-NAME
106850010502               MOVE WS-REP-NAME        TO  WS-ADRTMP.
106860010502
106870010502           MOVE WS-ADRTMP              TO  WS-REP-NAME.
106880010502
106890010502           IF  ADDR-NO
106900010502               OF DEALER-REP-RECD
106910010502                              IS EQUAL TO  ZEROES
106920010502               GO TO LRA-0030-0010.
106930010502
106940010502           INITIALIZE MFADRADRP.
106950010502           MOVE ADDR-NO
106960010502                OF DEALER-REP-RECD     TO  ADDR-NO
106970010502                                           OF DEALER-REP-ADDRESS-RECD.
106980010502            READ DEALER-REP-ADDRESS
106990010502                INVALID KEY
107000010502                    GO TO LRA-0030-0010.
107010010502
107020010502            MOVE CORR MFADRADRP        TO  MFAINADDP.
107030010502            GO TO LRA-0030-0030.
107040010502
107050010502       LRA-0030-0010.
107060010502
107070010502           INITIALIZE MFAINSBRP.
107080010502           MOVE INSTITUTION-CODE
107090010502                OF DEALER-RECD         TO  INSTITUTION-CODE
107100010502                                           OF INSTITUTION-BRANCH-RECD.
107110010502           MOVE BRANCH-CODE
107120010502                OF DEALER-REP-RECD     TO  BRANCH-CODE
107130010502                                           OF INSTITUTION-BRANCH-RECD.
107140010502           READ INSTITUTION-BRANCH
107150010502               INVALID KEY
107160010502                   GO TO LRA-0030-0020.
107170010502
107180010502           INITIALIZE MFAINADDP.
107190010502           MOVE ADDR-NO
107200010502                OF INSTITUTION-BRANCH-RECD
107210010502                                       TO  ADDR-NO
107220010502                                           OF INSTITUTION-ADDRESS-RECD.
107230010502           READ INSTITUTION-ADDRESS
107240010502               INVALID KEY
107250010502                   GO TO LRA-0030-0020.
107260010502
107270010502           GO TO LRA-0030-0030.
107280010502
107290010502       LRA-0030-0020.
107300010502
107310010502           INITIALIZE MFAINSBRP.
107320010502           MOVE INSTITUTION-CODE
107330010502                OF DEALER-RECD         TO  INSTITUTION-CODE
107340010502                                           OF INSTITUTION-BRANCH-RECD.
107350010502           MOVE BRANCH-CODE
107360010502                OF DEALER-RECD         TO  BRANCH-CODE
107370010502                                           OF INSTITUTION-BRANCH-RECD.
107380010502           READ INSTITUTION-BRANCH
107390010502               INVALID KEY
107400010502                   INITIALIZE MFAINADDP
107410010502                   GO TO LRA-0030-0030.
107420010502
107430010502           INITIALIZE MFAINADDP.
107440010502           MOVE ADDR-NO
107450010502                OF INSTITUTION-BRANCH-RECD
107460010502                                       TO  ADDR-NO
107470010502                                           OF INSTITUTION-ADDRESS-RECD.
107480010502           READ INSTITUTION-ADDRESS
107490010502               INVALID KEY
107500010502                   INITIALIZE MFAINADDP
107510010502                   GO TO LRA-0030-0030.
107520010502
107530010502       LRA-0030-0030.
107540010502
107550010502           MOVE SPACES                 TO  WS-ADRTMP.
107560010502           STRING STREET-NO
107570010502               OF INSTITUTION-ADDRESS
107580010502                             DELIMITED BY  "  "
107590010502                                     INTO  WS-ADRTMP.
107600010502
107610010502           IF  PROVINCE-CODE
107620010502               OF INSTITUTION-ADDRESS-RECD
107630010502                              IS EQUAL TO  "QC"
107640010502               GO TO LRA-0030-0030-0010.
107650010502
107660010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
107670010502               STRING STREET-NAME
107680010502                      OF INSTITUTION-ADDRESS
107690010502                             DELIMITED BY  "  "
107700010502                                     INTO  WS-ADRTMP
107710010502             ELSE
107720010502               STRING WS-ADRTMP
107730010502                             DELIMITED BY  "  ",
107740010502                      " "    DELIMITED BY  SIZE,
107750010502                      STREET-NAME
107760010502                      OF INSTITUTION-ADDRESS
107770010502                             DELIMITED BY  "  "
107780010502                                     INTO  WS-BRANCH-ADDR1
107790010502               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
107800010502
107810010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
107820010502               STRING STREET-TYPE-CODE
107830010502                      OF INSTITUTION-ADDRESS
107840010502                             DELIMITED BY  "  "
107850010502                                     INTO  WS-ADRTMP
107860010502             ELSE
107870010502               STRING WS-ADRTMP
107880010502                             DELIMITED BY  "  ",
107890010502                      " "    DELIMITED BY  SIZE,
107900010502                      STREET-TYPE-CODE
107910010502                      OF INSTITUTION-ADDRESS
107920010502                             DELIMITED BY  "  "
107930010502                                     INTO  WS-BRANCH-ADDR1
107940010502               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
107950010502
107960010502           GO TO LRA-0030-0030-0020.
107970010502
107980010502       LRA-0030-0030-0010.
107990010502
108000010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
108010010502               STRING STREET-TYPE-CODE
108020010502                      OF INSTITUTION-ADDRESS-RECD
108030010502                             DELIMITED BY  "  "
108040010502                                     INTO  WS-ADRTMP
108050010502             ELSE
108060010502               STRING WS-ADRTMP
108070010502                             DELIMITED BY  "  ",
108080010502                      " "    DELIMITED BY  SIZE,
108090010502                      STREET-TYPE-CODE
108100010502                      OF INSTITUTION-ADDRESS-RECD
108110010502                             DELIMITED BY  "  "
108120010502                                     INTO  WS-BRANCH-ADDR1
108130010502               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
108140010502
108150010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
108160010502               STRING STREET-NAME
108170010502                      OF INSTITUTION-ADDRESS-RECD
108180010502                             DELIMITED BY  "  "
108190010502                                     INTO  WS-ADRTMP
108200010502             ELSE
108210010502               STRING WS-ADRTMP
108220010502                             DELIMITED BY  "  ",
108230010502                      " "    DELIMITED BY  SIZE,
108240010502                      STREET-NAME
108250010502                      OF INSTITUTION-ADDRESS-RECD
108260010502                             DELIMITED BY  "  "
108270010502                                     INTO  WS-BRANCH-ADDR1
108280010502               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
108290010502
108300010502       LRA-0030-0030-0020.
108310010502
108320010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
108330010502               STRING STREET-DIRCT-CODE
108340010502                      OF INSTITUTION-ADDRESS
108350010502                             DELIMITED BY  "  "
108360010502                                     INTO  WS-ADRTMP
108370010502             ELSE
108380010502               STRING WS-ADRTMP
108390010502                             DELIMITED BY  "  ",
108400010502                      " "    DELIMITED BY  SIZE,
108410010502                      STREET-DIRCT-CODE
108420010502                      OF INSTITUTION-ADDRESS
108430010502                             DELIMITED BY  "  "
108440010502                                     INTO  WS-BRANCH-ADDR1
108450010502               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
108460010502
108470010502           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR1.
108480010502
108490010502           MOVE MISC-FIRST-ADDR-LINE
108500010502                OF INSTITUTION-ADDRESS-RECD
108510010502                                       TO  WS-BRANCH-ADDR2.
108520010502           MOVE MISC-SECOND-ADDR-LINE
108530010502                OF INSTITUTION-ADDRESS-RECD
108540010502                                       TO  WS-BRANCH-ADDR3.
108550010502           MOVE CO-OR-ATTENTION
108560010502                OF INSTITUTION-ADDRESS-RECD
108570010502                                       TO  WS-BRANCH-ADDR4.
108580010502
108590010502980420     IF  WS-LDA-ATTN-LINE-PRT = "N"
108600010502980420         MOVE SPACES TO WS-BRANCH-ADDR4.
108610010502
108620010502           MOVE POSTAL-CODE
108630010502                OF INSTITUTION-ADDRESS TO  WS-POSTAL.
108640010502           MOVE SPACES                 TO  WS-POSTAL-OUT.
108650010502           IF  COUNTRY-CODE
108660010502               OF INSTITUTION-ADDRESS
108670010502                              IS EQUAL TO  "CAN"
108680010502               MOVE WS-POSTAL1         TO  WS-POSTAL-OUT1
108690010502               MOVE WS-POSTAL2         TO  WS-POSTAL-OUT2
108700010502             ELSE
108710010502               MOVE WS-POSTAL          TO  WS-POSTAL-OUT.
108720010502
108730010502           MOVE SPACES                 TO  WS-ADRTMP.
108740010502           STRING CITY
108750010502               OF INSTITUTION-ADDRESS
108760010502                             DELIMITED BY  "  "
108770010502                                     INTO  WS-ADRTMP.
108780010502
108790010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
108800010502               STRING PROVINCE-CODE
108810010502                      OF INSTITUTION-ADDRESS
108820010502                             DELIMITED BY  "  "
108830010502                                     INTO  WS-ADRTMP
108840010502               GO TO LRA-0030-0040.
108850010502
108860010502           IF  PROVINCE-CODE
108870010502               OF INSTITUTION-ADDRESS
108880010502                              IS EQUAL TO  SPACES
108890010502               GO TO LRA-0030-0040.
108900010502
108910010502           STRING WS-ADRTMP  DELIMITED BY  "  ",
108920010502                  ", "       DELIMITED BY  SIZE,
108930010502                  PROVINCE-CODE
108940010502                  OF INSTITUTION-ADDRESS
108950010502                             DELIMITED BY  "  "
108960010502                                     INTO  WS-BRANCH-ADDR5.
108970010502           MOVE WS-BRANCH-ADDR5        TO  WS-ADRTMP.
108980010502
108990010502       LRA-0030-0040.
109000010502           IF  WS-ADRTMP      IS EQUAL TO  SPACES
109010010502               STRING WS-POSTAL-OUT
109020010502                             DELIMITED BY  "  "
109030010502                                     INTO  WS-ADRTMP
109040010502             ELSE
109050010502               STRING WS-ADRTMP
109060010502                             DELIMITED BY  "  ",
109070010502                      "  "   DELIMITED BY  SIZE,
109080010502                      WS-POSTAL-OUT
109090010502                             DELIMITED BY  "  "
109100010502                                     INTO  WS-BRANCH-ADDR5
109110010502               MOVE WS-BRANCH-ADDR5    TO  WS-ADRTMP.
109120010502
109130010502           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR5.
109140010502
109150010502           IF  WS-BRANCH-ADDR1
109160010502                          IS NOT EQUAL TO  SPACES
109170010502               GO TO LRA-0030-0050.
109180010502
109190010502           IF  WS-BRANCH-ADDR2
109200010502                          IS NOT EQUAL TO  SPACES
109210010502               MOVE WS-BRANCH-ADDR2    TO  WS-BRANCH-ADDR1
109220010502               MOVE SPACES             TO  WS-BRANCH-ADDR2
109230010502               GO TO LRA-0030-0050.
109240010502
109250010502           IF  WS-BRANCH-ADDR3
109260010502                          IS NOT EQUAL TO  SPACES
109270010502               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR1
109280010502               MOVE SPACES             TO  WS-BRANCH-ADDR3
109290010502               GO TO LRA-0030-0050.
109300010502
109310010502           IF  WS-BRANCH-ADDR4
109320010502                          IS NOT EQUAL TO  SPACES
109330010502               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR1
109340010502               MOVE SPACES             TO  WS-BRANCH-ADDR4
109350010502               GO TO LRA-0030-0050.
109360010502
109370010502           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR1.
109380010502           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
109390010502           GO TO LRA-0030-0080.
109400010502
109410010502       LRA-0030-0050.
109420010502           IF  WS-BRANCH-ADDR2
109430010502                          IS NOT EQUAL TO  SPACES
109440010502               GO TO LRA-0030-0060.
109450010502
109460010502           IF  WS-BRANCH-ADDR3
109470010502                          IS NOT EQUAL TO  SPACES
109480010502               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR2
109490010502               MOVE SPACES             TO  WS-BRANCH-ADDR3
109500010502               GO TO LRA-0030-0060.
109510010502
109520010502           IF  WS-BRANCH-ADDR4
109530010502                          IS NOT EQUAL TO  SPACES
109540010502               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR2
109550010502               MOVE SPACES             TO  WS-BRANCH-ADDR4
109560010502               GO TO LRA-0030-0060.
109570010502
109580010502           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR2.
109590010502           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
109600010502           GO TO LRA-0030-0080.
109610010502
109620010502       LRA-0030-0060.
109630010502
109640010502           IF  WS-BRANCH-ADDR3
109650010502                          IS NOT EQUAL TO  SPACES
109660010502               GO TO LRA-0030-0070.
109670010502
109680010502           IF  WS-BRANCH-ADDR4
109690010502                          IS NOT EQUAL TO  SPACES
109700010502               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR3
109710010502               MOVE SPACES             TO  WS-BRANCH-ADDR4
109720010502               GO TO LRA-0030-0070.
109730010502
109740010502           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR3.
109750010502           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
109760010502           GO TO LRA-0030-0080.
109770010502
109780010502       LRA-0030-0070.
109790010502           IF  WS-BRANCH-ADDR4
109800010502                          IS NOT EQUAL TO  SPACES
109810010502               GO TO LRA-0030-0080.
109820010502
109830010502           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR4.
109840010502           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
109850010502
109860010502       LRA-0030-0080.
109870010502           STRING WS-DEALER-NAME
109880010502                             DELIMITED BY  "  ",
109890010502                  " /"       DELIMITED BY  SIZE,
109900010502                  WS-REP-NAME
109910010502                             DELIMITED BY  SIZE
109920010502                                     INTO  WS-DEALER-REP-NAMES.
109930010502
109940010502           MOVE WS-REP-NAME            TO  WS-REP-NAME-1.
109950010502           MOVE FAX-AREA
109960010502                OF DEALER-REP-RECD     TO  WS-SH-FAX-01-03.
109970010502           MOVE FAX-PHONE
109980010502                OF DEALER-REP-RECD     TO  WS-SH-FAX-04-10.
109990010502
110000010502           MOVE AREA-CODE
110010010502                OF DEALER-REP-RECD     TO  WS-SH-PHONE-01-03.
110020010502           MOVE PHONE-NO
110030010502                OF DEALER-REP-RECD     TO  WS-SH-PHONE-04-10.
110040010502
110050010502       LRA-0030-0090.
110060010502
110070010502      *****
110080010502      *****    Format the account type description.
110090010502      *****
110100010502
110110010502           MOVE AD-GROUP               TO  WS-ACTTYPGRP.
110120010502           MOVE AD-ACCOUNT-TYPE        TO  WS-ACTTYPIN-PFX.
110130010502           MOVE AD-LOCKED-IN           TO  WS-ACTTYPLCK.
110140010502           MOVE AD-LANGUAGE            TO  WS-ACTTYPLNG.
110150010502           MOVE AD-NOMINEE             TO  WS-ACTTYPNOM.
110160190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  WS-ACTTYPSHT.
110170190619183449     MOVE Lc-Acct-Type-Code      TO  WS-ACTTYPSHT.
110180010502           MOVE AD-SPOUSAL             TO  WS-ACTTYPSPS.
110190010502
110200010502           PERFORM FORMAT-ACTTYP-DESC THRU FAD-EXIT.
110210010502
110220010502      *****
110230010502      *****    Initialize the statement for the new account.
110240010502      *****
110250010502
110260010502           MOVE SPACES                 TO  WS-STATEMENT-PAGE.
110270010502           MOVE 1                      TO  WS-SH-PGENO.
110280010502
110290010502      *****
110300010502      *****    Format statement heading line 1.
110310010502      *****
110320010502
110330010502           MOVE SPACES                 TO  STMT-HDG-01.
110340010502
110350010502RF6007     IF  WS-STMTVRSN    IS EQUAL TO  9
110360010502               MOVE WS-ACTTYPOUT       TO  SH01V8-ACCOUNT-TYPE-DESC
110370010502               MOVE WS-SH01-LIT1 (SL)  TO  SH01V8-LIT1
110380010502               MOVE WS-SH-START-DATE   TO  SH01V8-START-DATE
110390010502               MOVE WS-SH01-LIT2 (SL)  TO  SH01V8-LIT2
110400010502               MOVE WS-SH-END-DATE     TO  SH01V8-END-DATE
110410010502               GO TO LRA-0030-0100
110420010502           END-IF
110430010502
110440010502
110450010502           MOVE WS-ACTTYPOUT           TO  SH01-ACCOUNT-TYPE-DESC.
110460010502           MOVE WS-SH01-LIT1 (SL)      TO  SH01-LIT1.
110470010502           MOVE WS-SH-START-DATE       TO  SH01-START-DATE.
110480010502           MOVE WS-SH01-LIT2 (SL)      TO  SH01-LIT2.
110490010502           MOVE WS-SH-END-DATE         TO  SH01-END-DATE.
110500010502
110510010502       LRA-0030-0100.
110520010502
110530010502           SET SPL                     TO  WS-SPL-HDGFST.
110540010502           MOVE STMT-HDG-01            TO  WS-STMTPGE-LNE (SPL).
110550010502
110560010502      *****
110570010502      *****    Format statement heading line 2.
110580010502      *****
110590010502
110600010502           MOVE II-SIN-NO              TO  WS-SIN-NO.
110610010502           MOVE SPACES                 TO  WS-SH02-SIN-NO.
110620010502
110630010502980417     IF WS-LDA-SIN NOT = "N"
110640010502             MOVE WS-SIN-NO-1-3          TO  SH02-SIN-NO-1-3
110650010502             MOVE "-"                    TO  SH02-SIN-NO-LITDSH1
110660010502             MOVE WS-SIN-NO-4-6          TO  SH02-SIN-NO-4-6
110670010502             MOVE "-"                    TO  SH02-SIN-NO-LITDSH2
110680010502             MOVE WS-SIN-NO-7-9          TO  SH02-SIN-NO-7-9.
110690010502
110700010502           MOVE SPACES                 TO  STMT-HDG-02.
110710010502
110720010502           IF  WS-STMTVRSN    IS EQUAL TO  9
110730010502           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
110740020921R13756         IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
110750020921  ¦             MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V8-ACCOUNT
110760020613R13756         ELSE
110770020613                MOVE AD-ACCOUNT         TO  SH02V8-ACCOUNT
110780020613R13756         END-IF
110790010502               MOVE WS-SH02-SIN-NO     TO  SH02V8-SIN-NO
110800010502               MOVE AD-DEALER          TO  SH02V8-DEALER
110810020613               MOVE "-"                TO  SH02V8-LITDSH
110820010502               MOVE AD-SALESREP        TO  SH02V8-SALESREP
110830010502               MOVE WS-SH-END-DATE     TO  SH02V8-END-DATE
110840010502               MOVE AD-SPOUSAL-CONTRIB TO  SH02V8-SPOUSAL-CONTRIB
110850010502               MOVE WS-REG-CTB-CUR     TO  SH02V8-REG-CTB-CUR
110860010502               MOVE WS-REG-CTB-CUR-SIGN
110870010502                                       TO  SH02V8-REG-CTB-CUR-SIGN
110880010502
110890010502               MOVE WS-SH-PGENO        TO  SH02V8-PGENO
110900010502               SET SPL              UP BY  3
110910010502               GO TO LRA-0030-0110
110920010502
110930010502           END-IF
110940010502
110950010502           IF  WS-STMTVRSN    IS EQUAL TO  9
110960010502           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
110970020921R13756         IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
110980020921  ¦             MOVE WS-MSTR-ACCOUNT-NO  TO  SH022V8-ACCOUNT
110990020613R13756         ELSE
111000020613                MOVE AD-ACCOUNT         TO  SH022V8-ACCOUNT
111010020613R13756         END-IF
111020010502               MOVE AD-DEALER          TO  SH022V8-DEALER
111030020613               MOVE "-"                TO  SH022V8-LITDSH
111040010502               MOVE AD-SALESREP        TO  SH022V8-SALESREP
111050010502               MOVE WS-SH-END-DATE     TO  SH022V8-END-DATE
111060010502               MOVE AD-SPOUSAL-CONTRIB TO  SH022V8-SPOUSAL-CONTRIB
111070010502               MOVE WS-REG-CTB-CUR     TO  SH022V8-REG-CTB-CUR
111080010502               MOVE WS-REG-CTB-CUR-SIGN
111090010502                                       TO  SH022V8-REG-CTB-CUR-SIGN
111100010502
111110010502               MOVE WS-SH-PGENO        TO  SH022V8-PGENO
111120010502               SET SPL              UP BY  3
111130010502               GO TO LRA-0030-0110
111140010502
111150010502           END-IF
111160010502
111170010502
111180020921R13756     IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
111190020921  ¦         MOVE WS-MSTR-ACCOUNT-NO  TO  SH02-ACCOUNT
111200020613R13756     ELSE
111210020613            MOVE AD-ACCOUNT             TO  SH02-ACCOUNT
111220020613R13756     END-IF
111230010502           MOVE WS-SH02-SIN-NO         TO  SH02-SIN-NO.
111240010502           MOVE AD-DEALER              TO  SH02-DEALER.
111250020613           MOVE "-"                    TO  SH02-LITDSH.
111260010502           MOVE AD-SALESREP            TO  SH02-SALESREP.
111270010502           MOVE WS-SH-END-DATE         TO  SH02-END-DATE.
111280010502           MOVE AD-SPOUSAL-CONTRIB     TO  SH02-SPOUSAL-CONTRIB.
111290010502           MOVE WS-REG-CTB-CUR         TO  SH02-REG-CTB-CUR.
111300010502           MOVE WS-REG-CTB-CUR-SIGN    TO  SH02-REG-CTB-CUR-SIGN.
111310010502           MOVE WS-SH-PGENO            TO  SH02-PGENO.
111320010502           SET SPL                  UP BY  3.
111330010502
111340010502       LRA-0030-0110.
111350010502
111360010502           MOVE STMT-HDG-02            TO  WS-STMTPGE-LNE (SPL)
111370010502
111380010502           SET WS-SPL-SH02             TO  SPL.
111390010502
111400010502      *****
111410010502      *****    Format statement heading line 3.
111420010502      *****
111430010502
111440010502           MOVE SPACES                 TO  STMT-HDG-03.
111450010502
111460010502           IF  WS-STMTVRSN    IS EQUAL TO  9
111470010502RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
111480010502                  MOVE WS-NOMINEE-NAME  TO  SH03V8-INVESTOR-NAME
111490010502               ELSE
111500010502                  MOVE II-INVESTOR-NAME   TO  SH03V8-INVESTOR-NAME
111510010502RF9415         END-IF
111520010502               MOVE WS-DEALER-REP-NAMES
111530010502                                       TO  SH03V8-DEALER-REP-NAMES
111540010502
111550010502               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
111560010502                  MOVE SPACES          TO  SH03V8-DEALER-REP-NAMES
111570010502                  MOVE WS-REP-NAME-1   TO  SH03V8-DEALER-REP-NAMES
111580010502               END-IF
111590010502
111600010502R9415A         MOVE WS-SH03-LIT1 (SL)  TO  SH03V8-LIT1
111610010502               SET SPL              UP BY  3
111620010502               GO TO LRA-0030-0120
111630010502           END-IF
111640010502
111650010502R9415A     MOVE WS-SH03-LIT1 (SL)      TO  SH03-LIT1.
111660010502           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
111670010502
111680010502           IF  II-INVESTOR-ADDRESS1    IS EQUAL TO  SPACES
111690010502           AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
111700010502           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
111710010502           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
111720010502           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
111730010502               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
111740010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
111750010502           ELSE
111760010502               MOVE II-INVESTOR-ADDRESS1
111770010502                                       TO  WS-SH04-INVESTOR-ADDRESS1
111780010502           END-IF
111790010502
111800010502RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
111810010502              MOVE WS-NOMINEE-NAME  TO  SH03-INVESTOR-NAME
111820010502           ELSE
111830010502              MOVE II-INVESTOR-NAME       TO  SH03-INVESTOR-NAME
111840010502RF9415     END-IF
111850010502      *    MOVE II-INVESTOR-NAME       TO  SH03-INVESTOR-NAME.
111860010502           MOVE WS-DEALER-REP-NAMES    TO  SH03-DEALER-REP-NAMES.
111870010502
111880010502           IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
111890010502              MOVE SPACES          TO  SH03-DEALER-REP-NAMES
111900010502              MOVE WS-REP-NAME-1   TO  SH03-DEALER-REP-NAMES
111910010502           END-IF.
111920010502
111930010502           SET SPL                  UP BY  3.
111940010502
111950010502       LRA-0030-0120.
111960010502
111970010502           MOVE STMT-HDG-03            TO  WS-STMTPGE-LNE (SPL).
111980010502
111990010502      *****
112000010502      *****    Format statement heading line 4.
112010010502      *****
112020010502
112030010502           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
112040010502
112050010502           IF  II-INVESTOR-ADDRESS1
112060010502                              IS EQUAL TO  SPACES
112070010502           AND II-INVESTOR-ADDRESS2
112080010502                              IS EQUAL TO  SPACES
112090010502           AND II-INVESTOR-ADDRESS3
112100010502                              IS EQUAL TO  SPACES
112110010502           AND II-INVESTOR-ADDRESS4
112120010502                              IS EQUAL TO  SPACES
112130010502           AND II-INVESTOR-ADDRESS5
112140010502                              IS EQUAL TO  SPACES
112150010502               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
112160010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
112170010502           ELSE
112180010502               MOVE II-INVESTOR-ADDRESS1
112190010502                                       TO  WS-SH04-INVESTOR-ADDRESS1
112200010502           END-IF
112210010502
112220010502980417     IF  WS-LDA-BRANCH-ADDRESS
112230010502980417                        IS EQUAL TO  "N"
112240010502               MOVE SPACES             TO  WS-BRANCH-ADDR1.
112250010502
112260010502           MOVE SPACES                 TO  STMT-HDG-04.
112270010502
112280010502           IF  WS-STMTVRSN    IS EQUAL TO  9
112290010502RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
112300010502                  MOVE DL-NOMINEE-ADDR1 TO SH04V8-INVESTOR-ADDRESS1
112310010502               ELSE
112320010502                  MOVE WS-SH04-INVESTOR-ADDRESS1
112330010502                                       TO  SH04V8-INVESTOR-ADDRESS1
112340010502               END-IF
112350010502RF9415*        MOVE WS-SH04-INVESTOR-ADDRESS1
112360010502      *                                TO  SH04V8-INVESTOR-ADDRESS1
112370010502               MOVE WS-BRANCH-ADDR1    TO  SH04V8-BRANCH-ADDR1
112380010502
112390010502               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
112400010502                  MOVE SPACES TO SH04V8-BRANCH-ADDR1
112410010502               END-IF
112420010502
112430010502               GO TO LRA-0030-0130
112440010502           END-IF
112450010502
112460010502           MOVE SPACES                 TO  WS-SH05-INVESTOR-ADDRESS2.
112470010502
112480010502           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
112490010502           AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
112500010502           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
112510010502           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
112520010502           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
112530010502               MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
112540010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
112550010502           ELSE
112560010502               MOVE II-INVESTOR-ADDRESS2
112570010502                                       TO  WS-SH05-INVESTOR-ADDRESS2
112580010502           END-IF
112590010502
112600010502RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
112610010502              MOVE DL-NOMINEE-ADDR1 TO SH04-INVESTOR-ADDRESS1
112620010502           ELSE
112630010502              MOVE WS-SH04-INVESTOR-ADDRESS1
112640010502                                    TO  SH04-INVESTOR-ADDRESS1
112650010502RF9415     END-IF
112660010502      *    MOVE WS-SH04-INVESTOR-ADDRESS1
112670010502      *                                TO  SH04-INVESTOR-ADDRESS1.
112680010502           MOVE WS-BRANCH-ADDR1        TO  SH04-BRANCH-ADDR1.
112690010502
112700010502       LRA-0030-0130.
112710010502
112720010502           SET SPL                     UP BY  1.
112730010502           MOVE STMT-HDG-04            TO  WS-STMTPGE-LNE (SPL).
112740010502
112750010502      *****
112760010502      *****    Format statement heading line 5.
112770010502      *****
112780010502
112790010502           MOVE SPACES                 TO  WS-SH05-INVESTOR-ADDRESS2.
112800010502
112810010502           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
112820010502           AND II-INVESTOR-ADDRESS2
112830010502                              IS EQUAL TO  SPACES
112840010502           AND II-INVESTOR-ADDRESS3
112850010502                              IS EQUAL TO  SPACES
112860010502           AND II-INVESTOR-ADDRESS4
112870010502                              IS EQUAL TO  SPACES
112880010502           AND II-INVESTOR-ADDRESS5
112890010502                              IS EQUAL TO  SPACES
112900010502               MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
112910010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
112920010502           ELSE
112930010502               MOVE II-INVESTOR-ADDRESS2
112940010502                                       TO  WS-SH05-INVESTOR-ADDRESS2
112950010502           END-IF
112960010502
112970010502980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO  "N"
112980010502               MOVE SPACES             TO  WS-BRANCH-ADDR2
112990010502           END-IF
113000010502
113010010502           MOVE SPACES                 TO  STMT-HDG-05.
113020010502
113030010502           IF  WS-STMTVRSN    IS EQUAL TO  9
113040010502RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
113050010502                  MOVE DL-NOMINEE-ADDR2    TO SH05V8-INVESTOR-ADDRESS2
113060010502               ELSE
113070010502                  MOVE WS-SH05-INVESTOR-ADDRESS2
113080010502                                          TO  SH05V8-INVESTOR-ADDRESS2
113090010502RF9415         END-IF
113100010502
113110010502      *        MOVE WS-SH05-INVESTOR-ADDRESS2
113120010502      *                                TO  SH05V8-INVESTOR-ADDRESS2
113130010502      *        MOVE WS-BRANCH-ADDR2    TO  SH05V8-BRANCH-ADDR2
113140010502
113150010502               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
113160010502                  MOVE SPACES TO SH05V8-BRANCH-ADDR2
113170010502               END-IF
113180010502
113190010502RF9415         MOVE WS-SH05-LIT1 (SL)        TO  SH05V8-LIT1
113200010502               IF WS-STMT-DTA-SW-40          IS EQUAL TO "N"
113210010502                  INITIALIZE                 WS-MONTH-COL4
113220010502                  MOVE WS-MONTH-COL4         TO  SH05-BRANCH-ADDR2
113230010502               ELSE
113240010612                  IF SL                      IS EQUAL TO 1
113250010612                     MOVE WS-MONTH-COL4      TO  SH05-BRANCH-ADDR2
113260010612                  ELSE
113270010612                     MOVE WS-MONTH-COL4-F    TO  SH05-BRANCH-ADDR2
113280010612                  END-IF
113290010502               END-IF
113300010502               GO TO LRA-0030-0140
113310010502           END-IF
113320010502
113330010502RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
113340010502              MOVE DL-NOMINEE-ADDR2    TO SH05-INVESTOR-ADDRESS2
113350010502           ELSE
113360010502              MOVE WS-SH05-INVESTOR-ADDRESS2
113370010502                                       TO  SH05-INVESTOR-ADDRESS2
113380010502RF9415     END-IF
113390010502      *    MOVE WS-SH05-INVESTOR-ADDRESS2
113400010502      *                                TO  SH05-INVESTOR-ADDRESS2.
113410010502           MOVE WS-BRANCH-ADDR2        TO  SH05-BRANCH-ADDR2.
113420010502
113430010502R9415A     MOVE WS-SH05-LIT1 (SL)      TO  SH05-LIT1.
113440010502
113450010502       LRA-0030-0140.
113460010502
113470010502           SET SPL                     UP BY  1.
113480010502           MOVE STMT-HDG-05            TO  WS-STMTPGE-LNE (SPL).
113490010502
113500010502      *****
113510010502      *****    Format statement heading line 6.
113520010502      *****
113530010502
113540010502           MOVE SPACES                 TO  WS-SH06-INVESTOR-ADDRESS3.
113550010502
113560010502           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
113570010502           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
113580010502           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
113590010502           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
113600010502               MOVE II-POSTAL-CODE     TO  SH06-POSTAL-CODE
113610010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
113620010502           ELSE
113630010502               MOVE II-INVESTOR-ADDRESS3
113640010502                                       TO  WS-SH06-INVESTOR-ADDRESS3
113650010502           END-IF
113660010502
113670010502980417     IF  WS-LDA-BRANCH-ADDRESS
113680010502980417                        IS EQUAL TO  "N"
113690010502               MOVE SPACES             TO  WS-BRANCH-ADDR3.
113700010502
113710010502           MOVE SPACES                 TO  STMT-HDG-06.
113720010502
113730010502           IF  WS-STMTVRSN    IS EQUAL TO  9
113740010502RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
113750010502                  MOVE DL-NOMINEE-ADDR3    TO SH06V8-INVESTOR-ADDRESS3
113760010502               ELSE
113770010502                  MOVE WS-SH06-INVESTOR-ADDRESS3
113780010502                                          TO  SH06V8-INVESTOR-ADDRESS3
113790010502RF9415         END-IF
113800010502
113810010502               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
113820010502                  MOVE SPACES TO SH06V8-BRANCH-ADDR3
113830010502               END-IF
113840010502
113850010515R10243         MOVE SPACES                 TO  SH06V8-LIT1
113860010515               MOVE SPACES                 TO  SH06-BRANCH-ADDR3
113870010515
113880190612      *RFS183449-Starts
113890190612      *        IF  AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "RRIF "
113900190612      *        AND AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "LIF  "
113910190612R52366*        AND AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "RLIF "
113920190612      *        AND AD-ACCOUNT-TYPE-SHORT   IS NOT EQUAL TO "LRIF "
113930190612               IF  Lc-Acct-Type-Code       IS NOT EQUAL TO "RRIF "
113940190612               AND Lc-Acct-Type-Code       IS NOT EQUAL TO "LIF  "
113950190612               AND Lc-Acct-Type-Code       IS NOT EQUAL TO "RLIF "
113960190612               AND Lc-Acct-Type-Code       IS NOT EQUAL TO "LRIF "
113970190612      *RFS183449-Ends
113980010515RF9415             MOVE WS-SH06-LIT1 (SL)  TO  SH06V8-LIT1
113990010515                   IF WS-STMT-DTA-SW-40          IS EQUAL TO "N"
114000010515                      INITIALIZE  WS-MONTH-COL5
114010010515                      MOVE WS-MONTH-COL5         TO  SH06-BRANCH-ADDR3
114020010515                   ELSE
114030010612                      IF SL                    IS EQUAL TO 1
114040010612                         MOVE WS-MONTH-COL5    TO  SH06-BRANCH-ADDR3
114050010612                      ELSE
114060010612                         MOVE WS-MONTH-COL5-F  TO  SH06-BRANCH-ADDR3
114070010612                      END-IF
114080010515                   END-IF
114090010515               END-IF
114100010502               GO TO LRA-0030-0150
114110010502           END-IF
114120010502
114130010502RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
114140010502              MOVE DL-NOMINEE-ADDR3    TO SH06-INVESTOR-ADDRESS3
114150010502           ELSE
114160010502              MOVE WS-SH06-INVESTOR-ADDRESS3
114170010502                                       TO  SH06-INVESTOR-ADDRESS3
114180010502RF9415     END-IF
114190010502      *    MOVE WS-SH06-INVESTOR-ADDRESS3
114200010502      *                                TO  SH06-INVESTOR-ADDRESS3.
114210010502           MOVE WS-BRANCH-ADDR3        TO  SH06-BRANCH-ADDR3.
114220010502
114230010502R9415A     MOVE WS-SH06-LIT1 (SL)      TO  SH06-LIT1.
114240010502
114250010502       LRA-0030-0150.
114260010502
114270010502           SET SPL                     UP BY  1.
114280010502           MOVE STMT-HDG-06            TO  WS-STMTPGE-LNE (SPL).
114290010502
114300010502      *****
114310010502      *****    Format statement heading line 7.
114320010502      *****
114330010502
114340010502           MOVE SPACES                 TO  WS-SH07-INVESTOR-ADDRESS4.
114350010502           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
114360010502           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
114370010502           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
114380010502               MOVE II-POSTAL-CODE     TO  SH07-POSTAL-CODE
114390010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
114400010502           ELSE
114410010502               MOVE II-INVESTOR-ADDRESS4
114420010502                                       TO  WS-SH07-INVESTOR-ADDRESS4
114430010502           END-IF
114440010502
114450010502980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO "N"
114460010502               MOVE SPACES             TO  WS-BRANCH-ADDR4
114470010502           END-IF
114480010502
114490010502           MOVE SPACES                 TO  STMT-HDG-07.
114500010502
114510010502           IF  WS-STMTVRSN    IS EQUAL TO  9
114520010502RF9415         IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
114530010502                  MOVE DL-NOMINEE-ADDR4    TO SH07V8-INVESTOR-ADDRESS4
114540010502               ELSE
114550010502                  MOVE WS-SH07-INVESTOR-ADDRESS4
114560010502                                          TO  SH07V8-INVESTOR-ADDRESS4
114570010502RF9415         END-IF
114580010502      *        MOVE WS-SH07-INVESTOR-ADDRESS4
114590010502      *                                TO  SH07V8-INVESTOR-ADDRESS4
114600010502      *        MOVE WS-BRANCH-ADDR4    TO  SH07V8-BRANCH-ADDR4
114610010502
114620010502               IF WS-LDA-DLR-REP-PRINT        IS EQUAL TO "R"
114630010502                  MOVE SPACES TO SH07V8-BRANCH-ADDR4
114640010502               END-IF
114650010502
114660010502RF9415         MOVE WS-SH07-LIT1 (SL)  TO  SH07V8-LIT1
114670010502               MOVE WS-ANNUITANT-NAME      TO  SH07-BRANCH-ADDR4
114680010502
114690010502               GO TO LRA-0030-0160
114700010502           END-IF
114710010502
114720010502RF9415     IF WS-MAILING-ADDRESS-INDICATOR     IS EQUAL TO "N"
114730010502              MOVE DL-NOMINEE-ADDR4    TO SH07-INVESTOR-ADDRESS4
114740010502           ELSE
114750010502              MOVE WS-SH07-INVESTOR-ADDRESS4
114760010502                                       TO  SH07-INVESTOR-ADDRESS4
114770010502RF9415     END-IF
114780010502      *    MOVE WS-SH07-INVESTOR-ADDRESS4
114790010502      *                                TO  SH07-INVESTOR-ADDRESS4.
114800010502           MOVE WS-BRANCH-ADDR4        TO  SH07-BRANCH-ADDR4.
114810010502
114820010502R9415A     MOVE WS-SH07-LIT1 (SL)      TO  SH07-LIT1.
114830010502
114840010502       LRA-0030-0160.
114850010502
114860010502           SET SPL                     UP BY  1.
114870010502           MOVE STMT-HDG-07            TO  WS-STMTPGE-LNE (SPL).
114880010502
114890010502      *****
114900010502      *****    Format statement heading line 8.
114910010502      *****
114920010502
114930010502           MOVE SPACES                 TO  WS-SH08-INVESTOR-ADDRESS5.
114940010502
114950010502           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
114960010502           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
114970010502               MOVE II-POSTAL-CODE     TO  SH08-POSTAL-CODE
114980010502               MOVE "Y"                TO  WS-PSTCDEPRT-SW
114990010502           ELSE
115000010502               MOVE II-INVESTOR-ADDRESS5
115010010502                                       TO  WS-SH08-INVESTOR-ADDRESS5
115020010502           END-IF
115030010502
115040010502           MOVE SPACES                 TO  STMT-HDG-08.
115050010502
115060010502           IF  WS-STMTVRSN    IS EQUAL TO  9
115070010502               MOVE WS-SH08-INVESTOR-ADDRESS5
115080010502                                       TO  SH08V8-INVESTOR-ADDRESS5
115090010502               GO TO LRA-0030-0170
115100010502           END-IF
115110010502
115120010502           MOVE WS-SH08-INVESTOR-ADDRESS5
115130010502                                       TO  SH08-INVESTOR-ADDRESS5.
115140010502
115150010502       LRA-0030-0170.
115160010502
115170010502           SET SPL                     UP BY  1.
115180010502           MOVE STMT-HDG-08            TO  WS-STMTPGE-LNE (SPL).
115190010502
115200010502      *****
115210010502      *****    Format statement heading line 9.
115220010502      *****
115230010502
115240010502           MOVE SPACES                 TO  WS-SH09-POSTAL-CODE.
115250010502
115260010502           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
115270010502               MOVE II-POSTAL-CODE     TO  WS-SH09-POSTAL-CODE
115280010502           END-IF
115290010502
115300010502           MOVE SPACES                 TO  STMT-HDG-09.
115310010502
115320010502           IF  WS-STMTVRSN    IS EQUAL   TO  9
115330010502               MOVE WS-SH09-POSTAL-CODE  TO  SH09V8-POSTAL-CODE
115340010502               GO TO LRA-0030-0180
115350010502           END-IF
115360010502
115370010502           MOVE WS-SH09-POSTAL-CODE      TO  SH09-POSTAL-CODE.
115380010502
115390010502       LRA-0030-0180.
115400010502
115410010502           SET SPL                     UP BY  1
115420010502           MOVE STMT-HDG-09            TO  WS-STMTPGE-LNE (SPL).
115430010502
115440010502      *****
115450010502      *****    Format statement heading line 10.
115460010502      *****
115470010502
115480010502           MOVE SPACES                 TO  WS-SH10-LIT1X.
115490010502
115500010502           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
115510010502               MOVE WS-SH10-LIT1 (SL)  TO  WS-SH10-LIT1X
115520010502           END-IF
115530010502
115540010502           MOVE SPACES                 TO  WS-SH10-PHONE.
115550010502
115560010502           IF  WS-PHONE   IS NOT EQUAL TO  ZEROES
115570010502               MOVE "("                TO  SH10-PHONE-LITLB
115580010502               MOVE WS-SH-PHONE-01-03  TO  SH10-PHONE-01-03
115590010502               MOVE ")"                TO  SH10-PHONE-LITRB
115600010502               MOVE WS-SH-PHONE-04-06  TO  SH10-PHONE-04-06
115610010502               MOVE "-"                TO  SH10-PHONE-LITDSH
115620010502               MOVE WS-SH-PHONE-07-10  TO  SH10-PHONE-07-10
115630010502           END-IF
115640010502
115650010502           MOVE SPACES                 TO  STMT-HDG-10.
115660010502
115670010502           IF  WS-STMTVRSN    IS EQUAL TO  9
115680010502           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
115690010502                  MOVE WS-SH10-LIT1X      TO  SH10V8-LIT1
115700010502                  MOVE WS-SH10-PHONE      TO  SH10V8-PHONE
115710010502               GO TO LRA-0030-0190
115720010502           END-IF
115730010502
115740010502           IF  WS-STMTVRSN    IS EQUAL TO  9
115750010502           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
115760010502                  MOVE WS-SH10-LIT1X      TO  SH10V8-LIT1
115770010502                  MOVE SPACES             TO  SH10V8-PHONE
115780010502               GO TO LRA-0030-0190
115790010502           END-IF
115800010502
115810010502           MOVE WS-SH10-LIT1X          TO  SH10-LIT1.
115820010502           MOVE WS-SH10-PHONE          TO  SH10-PHONE.
115830010502
115840010502       LRA-0030-0190.
115850010502
115860010502           SET SPL                     UP BY  1.
115870010502           MOVE STMT-HDG-10            TO  WS-STMTPGE-LNE (SPL).
115880010502
115890010502      *****
115900010502      *****    Format statement heading line 11.
115910010502      *****
115920010502
115930010502           MOVE SPACES                 TO  WS-SH11-NOMINEE.
115940010502
115950010502           IF  WS-NOMINEE              IS NOT EQUAL TO  SPACES
115960010502               MOVE WS-NOMINEE         TO  WS-SH11-NOMINEE
115970010502           END-IF
115980010502
115990010502           MOVE SPACES                 TO  WS-SH11-FAX.
116000010502
116010010502           IF  WS-FAX     IS NOT EQUAL TO  ZEROES
116020010502               MOVE "("                TO  SH11-FAX-LITLB
116030010502               MOVE WS-SH-FAX-01-03    TO  SH11-FAX-01-03
116040010502               MOVE ")"                TO  SH11-FAX-LITRB
116050010502               MOVE WS-SH-FAX-04-06    TO  SH11-FAX-04-06
116060010502               MOVE "-"                TO  SH11-FAX-LITDSH
116070010502               MOVE WS-SH-FAX-07-10    TO  SH11-FAX-07-10
116080010502           END-IF
116090010502
116100010502           MOVE SPACES                 TO  STMT-HDG-11.
116110010502
116120010502           IF  WS-STMTVRSN    IS EQUAL TO  9
116130010502           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
116140010502               MOVE WS-SH11-NOMINEE    TO  SH11V8-NOMINEE
116150010502               MOVE WS-SH11-FAX        TO  SH11V8-FAX
116160010502               GO TO LRA-0030-0200
116170010502           END-IF
116180010502
116190010502           IF  WS-STMTVRSN    IS EQUAL TO  9
116200010502           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
116210010502               MOVE WS-SH11-NOMINEE    TO  SH11V8-NOMINEE
116220010502               MOVE WS-SH02-SIN-NO     TO  SH11V8-FAX
116230010502               GO TO LRA-0030-0200
116240010502           END-IF
116250010502
116260010502
116270010502           MOVE WS-SH11-NOMINEE        TO  SH11-NOMINEE.
116280010502           MOVE WS-SH11-FAX            TO  SH11-FAX.
116290010502
116300010502       LRA-0030-0200.
116310010502
116320010502           SET SPL                     UP BY  1.
116330010502           MOVE STMT-HDG-11            TO  WS-STMTPGE-LNE (SPL).
116340010502
116350010502      *    IF WS-LDA-PRINT-HEADING           IS NOT EQUAL TO "Y"
116360010502      *       GO TO LR1-0030-0300
116370010502      *    END-IF
116380010502
116390010502      *    SET SPL                     UP BY  3.
116400010502001127*    MOVE WS-SHLPL01 (SHL)         TO  WS-STMTPGE-LNE (SPL).
116410010502      *    SET SPL                     UP BY  3.
116420010502      *    MOVE WS-SHLPL02 (SHL)         TO  WS-STMTPGE-LNE (SPL).
116430010502      *    SET SPL                     UP BY  2.
116440010502      *    MOVE WS-SHLPL03 (SHL)         TO  WS-STMTPGE-LNE (SPL).
116450010502      *    SET SPL                     UP BY  1.
116460010502      *    MOVE WS-SHLPL04 (SHL)         TO  WS-STMTPGE-LNE (SPL).
116470010502
116480010502       LRA-0030-0300.
116490010502
116500010502      *****
116510010502      *****    Format the messages.  Initially, attempt to format the
116520010502      *****    statement messages using the Account Type Code.  If none
116530010502      *****    are found, use an Account Type Code of spaces.
116540021212R15028*****
116550021212  ¦   *****RFS15028 implements the following logic:
116560021212  ¦   *****    For new investor: get new investor's info. If not found
116570021212  ¦   *****    using existing info(new-investor = 'N'). If not found,
116580021212  ¦   *****    using All info(new-investor = 'A'). If not found, using
116590021212  ¦   *****    Account Type Code of spaces.
116600021212R15028*****
116610010502
116620010502           MOVE SPACES                 TO  WS-STATEMENT-PAGE-MESSAGE.
116630010502           MOVE AD-FUND-NO             TO  WS-FUND-NO.
116640010502
116650021212R15028 LRA-0030-0400.
116660021212  ¦
116670021212  ¦        MOVE LOW-VALUES             TO  STMT-MSG-RECD.
116680190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
116690190619183449     MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
116700021212  ¦                                        OF STMT-MSG-RECD.
116710021212  ¦        MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
116720021212  ¦                                        OF STMT-MSG-RECD.
116730021212  ¦        MOVE II-NEW-INVESTOR-FLAG   TO  WS-NEW-INVESTOR-FLAG.
116740021212  ¦        MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
116750021212  ¦                                        OF STMT-MSG-RECD.
116760021212  ¦        MOVE 1                      TO  LINE-NO
116770021212  ¦                                        OF STMT-MSG-RECD.
116780021212  ¦   *******
116790021212  ¦   *******   Direct read to determine if the specific investor(New or
116800021212  ¦   *******   not) infor exist or not.
116810021212  ¦   *******
116820021212  ¦
116830021212  ¦        READ STMT-MSG-FILE
116840021212  ¦          INVALID KEY
116850021212  ¦            IF WS-NEW-INVESTOR-FLAG  = "Y"
116860021212  ¦               MOVE "N"               TO WS-NEW-INVESTOR-FLAG
116870021212  ¦               MOVE WS-NEW-INVESTOR-FLAG  TO  NEW-INVESTOR
116880021212  ¦                                          OF STMT-MSG-RECD
116890021212  ¦               READ STMT-MSG-FILE
116900021212  ¦                   INVALID KEY
116910021212  ¦                   MOVE "A"           TO WS-NEW-INVESTOR-FLAG
116920021212  ¦               END-READ
116930021212  ¦            ELSE
116940021212  ¦               MOVE "A"               TO WS-NEW-INVESTOR-FLAG
116950021212  ¦            END-IF
116960021212R15028     END-READ.
116970021212
116980021212       LRA-0040.
116990021212
117000021212           MOVE LOW-VALUES             TO  STMT-MSG-RECD.
117010190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
117020190619183449     MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
117030021212                                           OF STMT-MSG-RECD.
117040021212           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
117050021212                                           OF STMT-MSG-RECD.
117060021212R15028     MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
117070021212  ¦                                        OF STMT-MSG-RECD.
117080021212  ¦        MOVE ZEROES                 TO  LINE-NO
117090021212R15028                                     OF STMT-MSG-RECD.
117100021212
117110021212           START STMT-MSG-FILE
117120021212               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
117130021212               INVALID KEY
117140021212                   GO TO LRA-0070
117150021212           END-START.
117160021212
117170021212       LRA-0050.
117180021212
117190021212           READ STMT-MSG-FILE NEXT     AT  END
117200021212               GO TO LRA-0070
117210021212           END-READ
117220021212
117230021212           IF  LINE-NO    IS GREATER THAN  WS-SPML-MAX
117240021212           OR  LANGUAGE-CODE OF STMT-MSG-RECD
117250021212                          IS NOT EQUAL TO  AD-LANGUAGE
117260021212           OR  ACCOUNT-TYPE-CODE OF STMT-MSG-RECD
117270190619183449*                   IS NOT EQUAL TO  AD-ACCOUNT-TYPE-SHORT
117280190619183449                    IS NOT EQUAL TO  Lc-Acct-Type-Code
117290021212R15028     OR  NEW-INVESTOR OF STMT-MSG-RECD  IS NOT EQUAL
117300021212R15028                         TO  WS-NEW-INVESTOR-FLAG
117310021212               GO TO LRA-0070
117320021212           END-IF
117330021212
117340021212           IF  LINE-NO   NOT GREATER THAN  ZEROES
117350021212               GO TO LRA-0060
117360021212           END-IF
117370021212
117380021212           MOVE SPACES                 TO  STMT-MSG-01.
117390021212           MOVE STATEMENT-MESSAGE      TO  SM01-STATEMENT-MESSAGE.
117400021212
117410021212           SET SPML                    TO  LINE-NO.
117420021212           MOVE STMT-MSG-01            TO  WS-STMTPGE-MSGLNE (SPML).
117430021212
117440021212       LRA-0060.
117450021212
117460021212           GO TO LRA-0050.
117470021212
117480021212       LRA-0070.
117490021212
117500010502      *****
117510010502      *****    If no statement messages were formatted and the Account
117520010502      *****    Type Code being used was not spaces, attempt to format
117530010502      *****    statement messages using an Account Type Code of spaces.
117540010502      *****
117550010502
117560010502           IF  WS-STATEMENT-PAGE-MESSAGE  IS EQUAL TO  SPACES
117570010502           AND AD-ACCOUNT-TYPE-SHORT      IS NOT EQUAL TO  SPACES
117580010502               MOVE SPACES                TO  AD-ACCOUNT-TYPE-SHORT
117590021212R15028*        GO TO LRA-0040
117600021212R15028         GO TO LRA-0030-0400
117610010502           END-IF
117620010502
117630010502           IF  WS-STMTVRSN    IS EQUAL TO  9
117640010502               MOVE WS-STATEMENT-PAGE-MESSAGE
117650010502                                       TO  WS-STMTPGE-MSG-V8
117660010502               GO TO LRA-0070-0000-0010
117670010502           END-IF
117680010502
117690010502           MOVE WS-STATEMENT-PAGE-MESSAGE
117700010502                                       TO  WS-STMTPGE-MSG.
117710010502       LRA-0070-0000-0010.
117720010502
117730010502      *****
117740010502      *****    Position at first detail line.
117750010502      *****
117760010502
117770010502      *    SET SPL                     TO  WS-SPL-DTLFST.
117780010502           SET SPL                     TO  WS-SHPL-DTLFST.
117790010502
117800010502      *****
117810010502      *****    Determine if the client statement copy is to be
117820010502      *****    suppressed.  This is done depending on whether or not the
117830010502      *****    account being processed is a nominee account.
117840010502      *****
117850010502
117860010502           MOVE "N"                    TO  WS-CLTSPR-SW.
117870010502
117880010502           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
117890010502               GO TO LRA-0070-0030
117900010502           END-IF
117910010502
117920010502      *****
117930010502      *****    Processing a non-nominee account.  Initially read through
117940010502      *****    the Dealer Mailing Instruction.  If the form CLTSTSPR is
117950010502      *****    found, the client copy statement is to be suppressed.
117960010502      *****
117970010502
117980010502           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
117990010502           MOVE AD-DEALER              TO  DEALER-CODE
118000010502                                           OF DEALER-MAILING-INSTR-RECD.
118010010502           MOVE "CLTSTSPR  "           TO  FORM-CODE
118020010502                                           OF DEALER-MAILING-INSTR-RECD.
118030010502           START DEALER-MAILING-INSTR
118040010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
118050010502               INVALID KEY
118060010502                   GO TO LRA-0070-0010
118070010502           END-START
118080010502
118090010502           READ DEALER-MAILING-INSTR NEXT
118100010502             AT  END
118110010502               GO TO LRA-0070-0010
118120010502           END-READ
118130010502
118140010502           IF  FORM-CODE OF DEALER-MAILING-INSTR-RECD
118150010502                IS NOT EQUAL TO  "CLTSTSPR  "
118160010502           OR  DEALER-CODE OF DEALER-MAILING-INSTR-RECD
118170010502                IS NOT EQUAL TO  AD-DEALER
118180010502               GO TO LRA-0070-0010
118190010502           END-IF
118200010502
118210010502           MOVE "Y"                    TO  WS-CLTSPR-SW.
118220010502           GO TO LRA-0070-0020.
118230010502
118240010502       LRA-0070-0010.
118250010502
118260010502      *****
118270010502      *****    Processing a non-nominee account.  The client statement
118280010502      *****    copy is to be printed, according to the Dealer Mailing
118290010502      *****    Instruction.  Now read through the Dealer Representative
118300010502      *****    Mailing Instruction.  If the form CLTSTSPR is found, the
118310010502      *****    client statement copy is to be suppressed.
118320010502      *****
118330010502
118340010502           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
118350010502           MOVE AD-DEALER              TO  DEALER-CODE
118360010502                                           OF DLRREP-MAILING-INSTR-RECD.
118370010502           MOVE AD-SALESREP            TO  DEALER-REP-CODE
118380010502                                           OF DLRREP-MAILING-INSTR-RECD.
118390010502           MOVE "CLTSTSPR  "           TO  FORM-CODE
118400010502                                           OF DLRREP-MAILING-INSTR-RECD.
118410010502           START DEALER-REP-MAILING-INSTR
118420010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
118430010502               INVALID KEY
118440010502                   GO TO LRA-0070-0020.
118450010502
118460010502           READ DEALER-REP-MAILING-INSTR NEXT
118470010502                                       AT  END
118480010502               GO TO LRA-0070-0020.
118490010502
118500010502           IF  FORM-CODE
118510010502               OF DLRREP-MAILING-INSTR-RECD
118520010502                          IS NOT EQUAL TO  "CLTSTSPR  "
118530010502           OR  DEALER-REP-CODE
118540010502               OF DLRREP-MAILING-INSTR-RECD
118550010502                          IS NOT EQUAL TO  AD-SALESREP
118560010502           OR  DEALER-CODE
118570010502               OF DLRREP-MAILING-INSTR-RECD
118580010502                          IS NOT EQUAL TO  AD-DEALER
118590010502               GO TO LRA-0070-0020.
118600010502
118610010502           MOVE "Y"                    TO  WS-CLTSPR-SW.
118620010502
118630010502       LRA-0070-0020.
118640010502
118650010502           GO TO LRA-0070-0050.
118660010502
118670010502       LRA-0070-0030.
118680010502
118690010502      *****
118700010502      *****    Processing a nominee account.  Read through the Nominee
118710010502      *****    Mailing Instruction.  If the form CLTSTCLT is not found,
118720010502      *****    the client copy statement is to be suppressed.
118730010502      *****
118740010502
118750010502           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
118760010502               GO TO LRA-0070-0050.
118770010502
118780010502           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
118790010502           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
118800010502                                           OF NOMINEE-MAILING-INST-RECD.
118810010502           MOVE "CLTSTCLT  "           TO  FORM-CODE
118820010502                                           OF NOMINEE-MAILING-INST-RECD.
118830010502           START NOMINEE-MAILING-INSTRUCTIONS
118840010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
118850010502               INVALID KEY
118860010502                   GO TO LRA-0070-0040.
118870010502
118880010502           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
118890010502                                       AT  END
118900010502               GO TO LRA-0070-0040.
118910010502
118920010502           IF  FORM-CODE
118930010502               OF NOMINEE-MAILING-INST-RECD
118940010502                          IS NOT EQUAL TO  "CLTSTCLT  "
118950010502           OR  NOMINEE-CODE
118960010502               OF NOMINEE-MAILING-INST-RECD
118970010502                          IS NOT EQUAL TO  WS-NOMINEE-CODE
118980010502               GO TO LRA-0070-0040.
118990010502
119000010502           GO TO LRA-0070-0050.
119010010502
119020010502       LRA-0070-0040.
119030010502
119040010502           MOVE "Y"                    TO  WS-CLTSPR-SW.
119050010502
119060010502       LRA-0070-0050.
119070010502
119080010502      *****
119090010502      *****    Read through the Dealer Mailing Instruction to determine
119100010502      *****    if the Statement Print Header is to be flagged to print
119110010502      *****    dealer copies of the statements.
119120010502      *****
119130010502
119140010502           MOVE SPACES                 TO  WS-DEAMLGINS-REC.
119150010502
119160010502           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
119170010502           MOVE AD-DEALER              TO  DEALER-CODE
119180010502                                           OF DEALER-MAILING-INSTR-RECD.
119190010502           MOVE "CLTSTDLR  "           TO  FORM-CODE
119200010502                                           OF DEALER-MAILING-INSTR-RECD.
119210010502           START DEALER-MAILING-INSTR
119220010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
119230010502               INVALID KEY
119240010502                   GO TO LRA-0080
119250010502           END-START
119260010502
119270010502           READ DEALER-MAILING-INSTR NEXT
119280010502                                       AT  END
119290010502               GO TO LRA-0080
119300010502           END-READ
119310010502
119320010502           IF  FORM-CODE
119330010502               OF DEALER-MAILING-INSTR-RECD
119340010502                          IS NOT EQUAL TO  "CLTSTDLR  "
119350010502           OR  DEALER-CODE
119360010502               OF DEALER-MAILING-INSTR-RECD
119370010502                          IS NOT EQUAL TO  AD-DEALER
119380010502               GO TO LRA-0080.
119390010502
119400010502           MOVE DEALER-MAILING-INSTR-RECD
119410010502                                       TO  WS-DEAMLGINS-REC.
119420010502
119430010502       LRA-0080.
119440010502
119450010502      *****
119460010502      *****    Determine if the Statement Print Header is to be flagged
119470010502      *****    to print dealer representative copies of the statements.
119480010502      *****    Initially read through the Dealer Mailing Instruction.
119490010502      *****    If a record is not found, read through the Dealer
119500010502      *****    Representative Mailing Instuction.
119510010502      *****
119520010502
119530010502           MOVE SPACES                 TO  WS-DERMLGINS-REC.
119540010502
119550010502           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
119560010502           MOVE AD-DEALER              TO  DEALER-CODE
119570010502                                           OF DEALER-MAILING-INSTR-RECD.
119580010502           MOVE "CLTSTREP  "           TO  FORM-CODE
119590010502                                           OF DEALER-MAILING-INSTR-RECD.
119600010502           START DEALER-MAILING-INSTR
119610010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
119620010502               INVALID KEY
119630010502                   GO TO LRA-0080-0010.
119640010502
119650010502           READ DEALER-MAILING-INSTR NEXT
119660010502                                       AT  END
119670010502               GO TO LRA-0080-0010.
119680010502
119690010502           IF  FORM-CODE
119700010502               OF DEALER-MAILING-INSTR-RECD
119710010502                          IS NOT EQUAL TO  "CLTSTREP  "
119720010502           OR  DEALER-CODE
119730010502               OF DEALER-MAILING-INSTR-RECD
119740010502                          IS NOT EQUAL TO  AD-DEALER
119750010502               GO TO LRA-0080-0010.
119760010502
119770010502           MOVE CORR MFADEAMIP
119780010502                     OF DEALER-MAILING-INSTR-RECD
119790010502                                       TO  MFADERMIP
119800010502                                           OF WS-DERMLGINS-REC.
119810010502           MOVE AD-SALESREP            TO  DEALER-REP-CODE
119820010502                                           OF WS-DERMLGINS-REC.
119830010502           GO TO LRA-0090.
119840010502
119850010502       LRA-0080-0010.
119860010502
119870010502           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
119880010502           MOVE AD-DEALER              TO  DEALER-CODE
119890010502                                           OF DLRREP-MAILING-INSTR-RECD.
119900010502           MOVE AD-SALESREP            TO  DEALER-REP-CODE
119910010502                                           OF DLRREP-MAILING-INSTR-RECD.
119920010502           MOVE "CLTSTREP  "           TO  FORM-CODE
119930010502                                           OF DLRREP-MAILING-INSTR-RECD.
119940010502           START DEALER-REP-MAILING-INSTR
119950010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
119960010502               INVALID KEY
119970010502                   GO TO LRA-0090.
119980010502
119990010502           READ DEALER-REP-MAILING-INSTR NEXT
120000010502                                       AT  END
120010010502               GO TO LRA-0090.
120020010502
120030010502           IF  FORM-CODE
120040010502               OF DLRREP-MAILING-INSTR-RECD
120050010502                          IS NOT EQUAL TO  "CLTSTREP  "
120060010502           OR  DEALER-REP-CODE
120070010502               OF DLRREP-MAILING-INSTR-RECD
120080010502                          IS NOT EQUAL TO  AD-SALESREP
120090010502           OR  DEALER-CODE
120100010502               OF DLRREP-MAILING-INSTR-RECD
120110010502                          IS NOT EQUAL TO  AD-DEALER
120120010502               GO TO LRA-0090.
120130010502
120140010502           MOVE DLRREP-MAILING-INSTR-RECD
120150010502                                       TO  WS-DERMLGINS-REC.
120160010502
120170010502       LRA-0090.
120180010502
120190010502      *****
120200010502      *****    Read through the Nominee Instruction to determine if the
120210010502      *****    Statement Print Header is to be flagged to print nominee
120220010502      *****    copies of the statements.
120230010502      *****
120240010502
120250010502           MOVE SPACES                 TO  WS-NOMMLGINS-REC.
120260010502           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
120270010502               GO TO LRA-0090-0010.
120280010502
120290010502           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
120300010502           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
120310010502                                           OF NOMINEE-MAILING-INST-RECD.
120320010502           MOVE "CLTSTNOM  "           TO  FORM-CODE
120330010502                                           OF NOMINEE-MAILING-INST-RECD.
120340010502           START NOMINEE-MAILING-INSTRUCTIONS
120350010502               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
120360010502               INVALID KEY
120370010502                   GO TO LRA-0090-0010.
120380010502
120390010502           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
120400010502                                       AT  END
120410010502               GO TO LRA-0090-0010.
120420010502
120430010502           IF  FORM-CODE
120440010502               OF NOMINEE-MAILING-INST-RECD
120450010502                          IS NOT EQUAL TO  "CLTSTNOM  "
120460010502           OR  NOMINEE-CODE
120470010502               OF NOMINEE-MAILING-INST-RECD
120480010502                          IS NOT EQUAL TO  WS-NOMINEE-CODE
120490010502               GO TO LRA-0090-0010.
120500010502
120510010502           MOVE NOMINEE-MAILING-INST-RECD
120520010502                                       TO  WS-NOMMLGINS-REC.
120530010502
120540010502       LRA-0090-0010.
120550010502
120560010502      *****
120570010502      *****    Format the Statement Print Header record.
120580010502      *****
120590010502
120600010502           MOVE "N"                    TO  WS-STMT-PRT-HDR-WSW.
120610010502
120620010502           MOVE SPACES                 TO  STMT-PRT-HDR-RECD.
120630010502           MOVE AD-INVESTOR-NO         TO  INVESTOR-NO
120640010502                                           OF STMT-PRT-HDR-RECD.
120650020921R13756*    IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
120660020921  ¦   *     MOVE WS-MSTR-ACCOUNT-NO  TO  ACCOUNT
120670020921  ¦   *                                    OF STMT-PRT-HDR-RECD
120680020921R13756*    ELSE
120690020613            MOVE AD-ACCOUNT             TO  ACCOUNT
120700020613                                           OF STMT-PRT-HDR-RECD
120710020921R13756*    END-IF
120720010502           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
120730010502                                           OF STMT-PRT-HDR-RECD.
120740010502           MOVE AD-DEALER              TO  DEALER
120750010502                                           OF STMT-PRT-HDR-RECD.
120760010502           MOVE AD-SALESREP            TO  DEALER-REP-CODE
120770010502                                           OF STMT-PRT-HDR-RECD.
120780010502           MOVE II-SIN-NO              TO  SOCIAL-INSURANCE-NO
120790010502                                           OF STMT-PRT-HDR-RECD.
120800010502           MOVE WS-CLTSPR-SW           TO  CLIENT-COPY-SUPPRESSED
120810010502                                           OF STMT-PRT-HDR-RECD.
120820010502           MOVE II-HOLD-MAIL           TO  HOLD-MAIL
120830010502                                           OF STMT-PRT-HDR-RECD.
120840010502           MOVE BRANCH-CODE
120850010502                OF WS-DEAMLGINS-REC    TO  DEALER-BRANCH-CODE
120860010502                                           OF STMT-PRT-HDR-RECD.
120870010502           MOVE FREQUENCY-CODE
120880010502                OF WS-DEAMLGINS-REC    TO  DEALER-FREQUENCY-CODE
120890010502                                           OF STMT-PRT-HDR-RECD.
120900010502           MOVE METHOD-OF-DELIVERY
120910010502                OF WS-DEAMLGINS-REC    TO  DEALER-METHOD-OF-DELIVERY
120920010502                                           OF STMT-PRT-HDR-RECD.
120930010502           MOVE SEND-TO
120940010502                OF WS-DEAMLGINS-REC    TO  DEALER-SEND-TO
120950010502                                           OF STMT-PRT-HDR-RECD.
120960010502           MOVE BRANCH-CODE
120970010502                OF WS-DERMLGINS-REC    TO  DLR-REP-BRANCH-CODE
120980010502                                           OF STMT-PRT-HDR-RECD.
120990010502           MOVE FREQUENCY-CODE
121000010502                OF WS-DERMLGINS-REC    TO  DLR-REP-FREQUENCY-CODE
121010010502                                           OF STMT-PRT-HDR-RECD.
121020010502           MOVE METHOD-OF-DELIVERY
121030010502                OF WS-DERMLGINS-REC    TO  DLR-REP-METHOD-OF-DELIVERY
121040010502                                           OF STMT-PRT-HDR-RECD.
121050010502           MOVE SEND-TO
121060010502                OF WS-DERMLGINS-REC    TO  DLR-REP-SEND-TO
121070010502                                           OF STMT-PRT-HDR-RECD.
121080010502           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
121090010502                                           OF STMT-PRT-HDR-RECD.
121100010502           MOVE BRANCH-CODE
121110010502                OF WS-NOMMLGINS-REC    TO  NOMINEE-BRANCH-CODE
121120010502                                           OF STMT-PRT-HDR-RECD.
121130010502           MOVE FREQUENCY-CODE
121140010502                OF WS-NOMMLGINS-REC    TO  NOMINEE-FREQUENCY-CODE
121150010502                                           OF STMT-PRT-HDR-RECD.
121160010502           MOVE METHOD-OF-DELIVERY
121170010502                OF WS-NOMMLGINS-REC    TO  NOMINEE-METHOD-OF-DELIVERY
121180010502                                           OF STMT-PRT-HDR-RECD.
121190010502           MOVE SEND-TO
121200010502                OF WS-NOMMLGINS-REC    TO  NOMINEE-SEND-TO
121210010502                                           OF STMT-PRT-HDR-RECD.
121220010502           MOVE II-COUNTRY-CODE        TO  COUNTRY-CODE
121230010502                                           OF STMT-PRT-HDR-RECD.
121240010502           MOVE II-LAST-NAME           TO  LAST-NAME
121250010502                                           OF STMT-PRT-HDR-RECD.
121260010502           MOVE II-FIRST-NAME          TO  FIRST-NAME
121270010502                                           OF STMT-PRT-HDR-RECD.
121280010502           MOVE II-INITIALS            TO  INITIALS
121290010502                                           OF STMT-PRT-HDR-RECD.
121300010502           MOVE II-LEGAL-NAME          TO  LEGAL-NAME
121310010502                                           OF STMT-PRT-HDR-RECD.
121320010502           MOVE AD-START-DATE          TO  START-DATE
121330010502                                           OF STMT-PRT-HDR-RECD.
121340010502           MOVE AD-END-DATE            TO  END-DATE
121350010502                                           OF STMT-PRT-HDR-RECD.
1213600105029415       MOVE DL-DEALER-CATEGORY-CODE TO  DEALER-CATEGORY-CODE
121370010502                                           OF STMT-PRT-HDR-RECD.
121380010502
121390010502       LRA-0090-0020.
121400010502
121410010502      *****
121420010502      *****    Initialize the statement currency totals.
121430010502      *****
121440010502
121450010502           IF WS-ACCOUNT-INV-SW             IS EQUAL TO "Y"
121460010502              GO TO LRA-EXIT
121470010502           END-IF.
121480010502
121490010502           SET SC                      TO  1.
121500010502
121510010502       LRA-0100.
121520010502
121530010502           MOVE ZEROS                  TO WS-AD-MARKET-VALUE-PRIOR-TOT.
121540010502           MOVE ZEROS                  TO WS-AD-MARKET-VALUE-TOT.
121550010502
121560010502           MOVE ZEROES                 TO  WS-SC-MARKET-VALUE (SC).
12157001091211175      MOVE ZEROES                 TO  WS-SC-MARKET-PERC (SC).
121580010502           MOVE "N"                    TO  WS-SC-RPTIND (SC).
121590010502
121600010502           IF  SC            IS LESS THAN  WS-SC-MAX
121610010502               SET SC               UP BY  1
121620010502               GO TO LRA-0100.
121630010502
121640010502      *****
121650010502      *****    Initialize the statement account funds tables.
121660010502      *****
121670010502
121680010502      *    MOVE SPACES                 TO  WS-STMT-ACCOUNT-FUND-TABLE.
121690010502           INITIALIZE                      WS-STMT-ACCOUNT-FUND-TABLE.
121700010502           MOVE ZEROES                 TO  WS-SAF-LST.
121710010502
121720010502           INITIALIZE                  WS-ACCOUNT-FUND-TABLE2.
121730010502           MOVE ZEROES                 TO  WS-SAF2-LST.
121740010502
121750010502      *****
121760010502      *****  Initialize the statement account fund performance table.
121770010502      *****
121780010502
121790010502           INITIALIZE             WS-ACCOUNT-FUND-PERFORM-TABLE.
121800010502           MOVE ZEROES            TO  WS-SAFP-LST.
121810010502
121820010502      *****
121830010502      *****    Initialize the SEG-FUND-CONTRACT-DETAILS table.
121840010502      *****    (SECTION 40)
121850010502      *****
121860010502
121870010502           INITIALIZE             WS-SEG-FUND-CONTRACT-TBL.
121880010502           MOVE ZEROES            TO  WS-SFC-LST.
121890010502
121900010502      *****
121910010502      *****    Initialize the SEG-FUND-CONTRACT-INVESTMENT table.
121920010502      *****    (SECTION 43)
121930010502      *****
121940010502
121950010502           INITIALIZE             WS-SEG-FUND-CONTRACT-INV-TBL.
121960010502           MOVE ZEROES            TO  WS-SFCI-LST.
121970010502
121980010502      *****
121990010502      *****    Initialize the SEG-FUND-CONTRACT-INVESTMENT table.
122000010502      *****    (SECTION 45)
122010010502      *****
122020010502
122030010502           INITIALIZE             WS-ANNUITANT-DETAILS-TBL.
122040010502           MOVE ZEROES            TO  WS-ANN-LST.
122050010502
122060010502      *****
122070010502      *****    Initialize the statement transaction controls.
122080010502      *****
122090010502
122100010502           MOVE LOW-VALUES             TO  CURR-CTLS-DPR.
122110010502           MOVE LOW-VALUES             TO  PREV-CTLS-DPR.
122120010502
122130010502           MOVE "Y"                    TO  WS-ACCOUNT-INV-SW.
122140010502
122150010502      **********
122160010502
122170010502       LRA-EXIT.
122180010502           EXIT.
122190010502
122200010502      **********
122210010502
122220010517010517 GET-NO-TRANS-ACCOUNT1.
122230010517
122240010517           SET SAF2                    TO  1.
122250010517
122260010517       GNTA-0010.
122270010517
122280010517           IF SAF2                     IS GREATER THAN   WS-SAF2-LST
122290010517              GO TO LRA1-EXIT
122300010517           END-IF.
122310010517
122320010517           INITIALIZE ACCOUNT-DETAILS-SECTION.
122330010517           MOVE WS-ACCOUNT-DETAILS-SECTION (SAF2)
122340010517                                       TO ACCOUNT-DETAILS-SECTION.
122350010517010517     PERFORM GET-NO-TRANS-ACCOUNT             THRU LRA-EXIT.
122360010517           SET SAF2                 UP BY  1.
122370010517           GO TO  GNTA-0010.
122380010517
122390010517      **********
122400010517
122410010517       LRA1-EXIT.
122420010517            EXIT.
122430010517
122440010517      **********
122450051110      * RFS30210 - Begin
122460051110      * ---------------------------------
122470051111       SEARCH-CURR-PERIOD-AP.
122480051110      * ---------------------------------
122490051110      * Search current period (6M, 1Y, INC ...) using passing
122500051110      * parameters.
122510051110      * Initialize returned parameters
122520051110            INITIALIZE                    WS-CURR-PERIOD-ROR
122530051110                                          WS-CURR-PERIOD-ROR-SIGN
122540051110
122550051110            SET SW-PERIOD-ROR-APPLICABLE-INIT TO TRUE
122560051110            SET SW-CURR-PERIOD-NOT-EXISTS     TO TRUE
122570051110      * Start comparing and set returned vars to some value ...
122580051110      * if found.
122590051110            EVALUATE SW-PERIOD-TO-CHECK-FLAG
122600051110              WHEN AP-PERIOD-CODE-1
122610051110                SET SW-CURR-PERIOD-EXISTS       TO TRUE
122620051111                MOVE AP-PERIOD-ROR-APPLICABLE-1 TO
122630051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
122640051111                MOVE AP-PERIOD-ROR-1        TO WS-CURR-PERIOD-ROR
122650051111                MOVE AP-PERIOD-ROR-SIGN-1   TO
122660051111                               WS-CURR-PERIOD-ROR-SIGN
122670051111              WHEN AP-PERIOD-CODE-2
122680051111                SET SW-CURR-PERIOD-EXISTS       TO TRUE
122690051111                MOVE AP-PERIOD-ROR-APPLICABLE-2 TO
122700051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
122710051111                MOVE AP-PERIOD-ROR-2        TO WS-CURR-PERIOD-ROR
122720051111                MOVE AP-PERIOD-ROR-SIGN-2   TO
122730051111                               WS-CURR-PERIOD-ROR-SIGN
122740051110              WHEN AP-PERIOD-CODE-3
122750051110                SET SW-CURR-PERIOD-EXISTS       TO TRUE
122760051111                MOVE AP-PERIOD-ROR-APPLICABLE-3 TO
122770051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
122780051111                MOVE AP-PERIOD-ROR-3        TO WS-CURR-PERIOD-ROR
122790051111                MOVE AP-PERIOD-ROR-SIGN-3   TO
122800051111                               WS-CURR-PERIOD-ROR-SIGN
122810051110              WHEN AP-PERIOD-CODE-4
122820051111                SET SW-CURR-PERIOD-EXISTS       TO TRUE
122830051111                MOVE AP-PERIOD-ROR-APPLICABLE-4 TO
122840051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
122850051111                MOVE AP-PERIOD-ROR-4          TO WS-CURR-PERIOD-ROR
122860051111                MOVE AP-PERIOD-ROR-SIGN-4       TO
122870051111                               WS-CURR-PERIOD-ROR-SIGN
122880051110              WHEN AP-PERIOD-CODE-5
122890051110                SET SW-CURR-PERIOD-EXISTS       TO TRUE
122900051111                MOVE AP-PERIOD-ROR-APPLICABLE-5 TO
122910051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
122920051111                MOVE AP-PERIOD-ROR-5        TO WS-CURR-PERIOD-ROR
122930051111                MOVE AP-PERIOD-ROR-SIGN-5       TO
122940051111                               WS-CURR-PERIOD-ROR-SIGN
122950051110              WHEN AP-PERIOD-CODE-6
122960051110                SET SW-CURR-PERIOD-EXISTS       TO TRUE
122970051111                MOVE AP-PERIOD-ROR-APPLICABLE-6 TO
122980051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
122990051111                MOVE AP-PERIOD-ROR-6         TO WS-CURR-PERIOD-ROR
123000051111                MOVE AP-PERIOD-ROR-SIGN-6       TO
123010051111                               WS-CURR-PERIOD-ROR-SIGN
123020051110              WHEN AP-PERIOD-CODE-7
123030051110                SET SW-CURR-PERIOD-EXISTS       TO TRUE
123040051111                MOVE AP-PERIOD-ROR-APPLICABLE-7 TO
123050051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123060051111                MOVE AP-PERIOD-ROR-7        TO WS-CURR-PERIOD-ROR
123070051111                MOVE AP-PERIOD-ROR-SIGN-7   TO
123080051111                               WS-CURR-PERIOD-ROR-SIGN
123090051110              WHEN AP-PERIOD-CODE-8
123100051110                SET SW-CURR-PERIOD-EXISTS       TO TRUE
123110051111                 MOVE AP-PERIOD-ROR-APPLICABLE-8 TO
123120051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123130051111                 MOVE AP-PERIOD-ROR-8        TO WS-CURR-PERIOD-ROR
123140051111                 MOVE AP-PERIOD-ROR-SIGN-8   TO
123150051111                               WS-CURR-PERIOD-ROR-SIGN
123160051110              WHEN AP-PERIOD-CODE-9
123170051111                SET SW-CURR-PERIOD-EXISTS       TO TRUE
123180051111                MOVE AP-PERIOD-ROR-APPLICABLE-9 TO
123190051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123200051111                MOVE AP-PERIOD-ROR-9        TO WS-CURR-PERIOD-ROR
123210051111                MOVE AP-PERIOD-ROR-SIGN-9   TO
123220051111                               WS-CURR-PERIOD-ROR-SIGN
123230051110              WHEN AP-PERIOD-CODE-10
123240051110                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123250051111                MOVE AP-PERIOD-ROR-APPLICABLE-10 TO
123260051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123270051111                MOVE AP-PERIOD-ROR-10        TO WS-CURR-PERIOD-ROR
123280051111                MOVE AP-PERIOD-ROR-SIGN-10   TO
123290051111                               WS-CURR-PERIOD-ROR-SIGN
123300051110              WHEN OTHER
123310051110                CONTINUE
123320051110            END-EVALUATE.
123330051110
123340051111      * ---------------------------------
123350051111       SEARCH-CURR-PERIOD-AFP.
123360051111      * ---------------------------------
123370051111      * Search current period (6M, 1Y, INC ...) using passing
123380051111      * parameters.
123390051111      * Initialize returned parameters
123400051111            INITIALIZE                    WS-CURR-PERIOD-ROR
123410051111                                          WS-CURR-PERIOD-ROR-SIGN
123420051111
123430051111            SET SW-PERIOD-ROR-APPLICABLE-INIT TO TRUE
123440051111            SET SW-CURR-PERIOD-NOT-EXISTS     TO TRUE
123450051111      * Start comparing and set returned vars to some value ...
123460051111      * if found.
123470051111            EVALUATE SW-PERIOD-TO-CHECK-FLAG
123480051111              WHEN AFP-PERIOD-CODE-1
123490051111                SET SW-CURR-PERIOD-EXISTS       TO TRUE
123500051111                MOVE AFP-PERIOD-ROR-APPLICABLE-1 TO
123510051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123520051111                MOVE AFP-PERIOD-ROR-1        TO WS-CURR-PERIOD-ROR
123530051111                MOVE AFP-PERIOD-ROR-SIGN-1   TO
123540051111                               WS-CURR-PERIOD-ROR-SIGN
123550051111              WHEN AFP-PERIOD-CODE-2
123560051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123570051111                MOVE AFP-PERIOD-ROR-APPLICABLE-2 TO
123580051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123590051111                MOVE AFP-PERIOD-ROR-2        TO WS-CURR-PERIOD-ROR
123600051111                MOVE AFP-PERIOD-ROR-SIGN-2   TO
123610051111                               WS-CURR-PERIOD-ROR-SIGN
123620051111              WHEN AFP-PERIOD-CODE-3
123630051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123640051111                MOVE AFP-PERIOD-ROR-APPLICABLE-3 TO
123650051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123660051111                MOVE AFP-PERIOD-ROR-3        TO WS-CURR-PERIOD-ROR
123670051111                MOVE AFP-PERIOD-ROR-SIGN-3   TO
123680051111                               WS-CURR-PERIOD-ROR-SIGN
123690051111              WHEN AFP-PERIOD-CODE-4
123700051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123710051111                MOVE AFP-PERIOD-ROR-APPLICABLE-4 TO
123720051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123730051111                MOVE AFP-PERIOD-ROR-4            TO WS-CURR-PERIOD-ROR
123740051111                MOVE AFP-PERIOD-ROR-SIGN-4       TO
123750051111                               WS-CURR-PERIOD-ROR-SIGN
123760051111              WHEN AFP-PERIOD-CODE-5
123770051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123780051111                MOVE AFP-PERIOD-ROR-APPLICABLE-5 TO
123790051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123800051111                MOVE AFP-PERIOD-ROR-5            TO WS-CURR-PERIOD-ROR
123810051111                MOVE AFP-PERIOD-ROR-SIGN-5       TO
123820051111                               WS-CURR-PERIOD-ROR-SIGN
123830051111              WHEN AFP-PERIOD-CODE-6
123840051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123850051111                MOVE AFP-PERIOD-ROR-APPLICABLE-6 TO
123860051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123870051111                MOVE AFP-PERIOD-ROR-6            TO WS-CURR-PERIOD-ROR
123880051111                MOVE AFP-PERIOD-ROR-SIGN-6       TO
123890051111                               WS-CURR-PERIOD-ROR-SIGN
123900051111              WHEN AFP-PERIOD-CODE-7
123910051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
123920051111                MOVE AFP-PERIOD-ROR-APPLICABLE-7 TO
123930051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
123940051111                MOVE AFP-PERIOD-ROR-7        TO WS-CURR-PERIOD-ROR
123950051111                MOVE AFP-PERIOD-ROR-SIGN-7   TO
123960051111                               WS-CURR-PERIOD-ROR-SIGN
123970051111              WHEN AFP-PERIOD-CODE-8
123980051111                SET SW-CURR-PERIOD-EXISTS         TO TRUE
123990051111                 MOVE AFP-PERIOD-ROR-APPLICABLE-8 TO
124000051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
124010051111                 MOVE AFP-PERIOD-ROR-8        TO WS-CURR-PERIOD-ROR
124020051111                 MOVE AFP-PERIOD-ROR-SIGN-8   TO
124030051111                               WS-CURR-PERIOD-ROR-SIGN
124040051111              WHEN AFP-PERIOD-CODE-9
124050051111                SET SW-CURR-PERIOD-EXISTS        TO TRUE
124060051111                MOVE AFP-PERIOD-ROR-APPLICABLE-9 TO
124070051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
124080051111                MOVE AFP-PERIOD-ROR-9        TO WS-CURR-PERIOD-ROR
124090051111                MOVE AFP-PERIOD-ROR-SIGN-9   TO
124100051111                               WS-CURR-PERIOD-ROR-SIGN
124110051111              WHEN AFP-PERIOD-CODE-10
124120051111                SET SW-CURR-PERIOD-EXISTS         TO TRUE
124130051111                MOVE AFP-PERIOD-ROR-APPLICABLE-10 TO
124140051111                               SW-PERIOD-ROR-APPLICABLE-FLAG
124150051111                MOVE AFP-PERIOD-ROR-10        TO WS-CURR-PERIOD-ROR
124160051111                MOVE AFP-PERIOD-ROR-SIGN-10   TO
124170051111                               WS-CURR-PERIOD-ROR-SIGN
124180051111              WHEN OTHER
124190051111                CONTINUE
124200051111            END-EVALUATE.
124210051111
124220051110      * RFS30210 - End
124230190612      *RFS-183449-Starts
124250190612       GET-ACCOUNT-TYPE-CODE.
124260190612           INITIALIZE MFAACCNTP OF ACCOUNT-REC-2.
124270190612           MOVE AD-ACCOUNT
124280190612                    TO  ACCOUNT-NO OF ACCOUNT-REC-2
124290190612
124300190612           READ ACCOUNT-FILE-2
124310190619             INVALID KEY
124320190619             DISPLAY "Account File 2 not found for Account:" AD-ACCOUNT
124340190619             GO TO GAT-EXIT.
124350190612
124360190612           MOVE ACCOUNT-TYPE-CODE OF ACCOUNT-REC-2
124370190612                TO Lc-Acct-Type-Code.
124390190619       GAT-EXIT.
124400190612           EXIT.
124410190612      *RFS183449-Ends
