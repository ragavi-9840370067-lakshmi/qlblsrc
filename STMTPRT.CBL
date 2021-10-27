000100000000       IDENTIFICATION DIVISION.
000101888888      *%CSTD===========================================================*
000102888888      ** Application. : UTC        Unitrax Common Programs             *
000103888888      ** Component. . : STMTPRT                       Type: CBL        *
000104888888      **===============================================================*
000105888888      ** Sub-system . :                                                *
000106888888      ** Function . . :                                                *
000107888888      ** Sub-function :                                                *
000108190722      **%S=============================================================*
000109888888      ** Description of functions:                                     *
000111888888      **                                                               *
000112888888      **                                                               *
000113888888      **                                                               *
000114888888      **%E=============================================================*
000115888888      ** AUTHOR:    AK59489DV  06/19/2019 12:10  XR5ARTK               *
000116888888      ** MODIFS: ** AK59489DV  06/19/2019   :    XR5ARTK     00/       *
000117888888      *%ECSTD==========================================================*
000270000000       PROGRAM-ID.     STMTPRT.
000280000000       INSTALLATION.   Jewelstone Systems Inc.
000290000000       AUTHOR.         Jewelstone Systems Inc.
000300000000       DATE-WRITTEN.   November 25, 1996.
000310000000       DATE-COMPILED.
000320000000
000330000000      ******************************************************************
000340000000      *                                                                *
000350000000      *    STMTPRT:    Statement Print - Standard Form                 *
000360000000      *                                                                *
000370000000      *    PURPOSE:    To format and print statements using the        *
000380000000      *                standard format and the Jewelstone Statement    *
000390000000      *                file (MFASTMTFP).                               *
000400000000      *                                                                *
000410000000      *    LEVELS:      1. INVESTOR-NO                                 *
000420000000      *                 2. ACCOUNT                                     *
000430000000      *                                                                *
000440980326      *    FILES:      (I) MFAACOJLA   Investor Join
000450980326      *                (I) MFADEAMIP   Dealer Mailing Instruction      *
000460000000      *                (I) MFADEAP     Dealer                          *
000470000000      *                (I) MFADEAREP   Dealer Representative           *
000480000000      *                (I) MFADERMIP   Dealer Representative Mailing   *
000490000000      *                                Instruction                     *
000500000000      *                (I) MFADRADRP   Dealer Representative Address   *
000510000000      *                (I) MFAINADDP   Institution Address             *
000520000000      *                (I) MFAINSBRP   Institution Branch              *
000530980326      *                (I) MFANMTPYP   Nominee Account Type Descr
000540991104991103*                (I) MFAACCTYP   Account Type BK
000550000000      *                (I) MFANOMMIP   Nominee Mailing Instruction     *
000560000000      *                (I) MFASTMMSP   Statement Message               *
000570990116      *                (I) MFAACCNTP   Account
000580980116      *                (O) MFASTMPDP   Statement Print Detail          *
000590980116      *                (O) MFASTMPHP   Statement Print Header          *
000600000000      *                (I) MFASTMEXP   Statement Extract Data          *
000610000000      *                (O) PRINTER     Control Report                  *
000620000000      *                                                                *
000630000000      *    CREATED:    23/DEC/96 (MFH)                                 *
000640980218      *    CHANGED:    18/FEB/98 (MFH)                                 *
000650980202      *                                                                *
000660000000      ******************************************************************
000670000000      /
000680000000      ******************************************************************
000690000000      *                                                                *
000700000000      *    C H A N G E   H I S T O R Y                                 *
000710000000      *                                                                *
000720000000      ******************************************************************
000730000000      ******************************************************************
000740000000      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE              *
000750000000      ******************************************************************
000760000000      *M.Howorth   *  1996/12/31  * - Modifications to format the      *
000770000000      *            *     thru     *   following new fields added to    *
000780000000      *            *  1997/01/03  *   the Statement Print Header file. *
000790000000      *            *              *     CLTSPR:     Client Copy to be  *
000800000000      *            *              *                 Suppressed         *
000810000000      *            *              *     HLDML:      Hold Mail          *
000820000000      *            *              *     NOMCD:      Nominee Code       *
000830000000      *            *              *     NOMBRCCD:   Nominee Branch     *
000840000000      *            *              *                 Code               *
000850000000      *            *              *     NOMFRQCD:   Nominee Frequency  *
000860000000      *            *              *                 Code               *
000870000000      *            *              *     NOMDLV:     Nominee Method of  *
000880000000      *            *              *                 Delivery           *
000890000000      *            *              *     NOMSNDTO:   Nominee Send To    *
000900000000      *            *              * - Modifications to the formatting  *
000910000000      *            *              *   of the following Statement Print *
000920000000      *            *              *   Header file fields.              *
000930000000      *            *              *     DERBRCCD:   Nominee Branch     *
000940000000      *            *              *                 Code               *
000950000000      *            *              *     DERFRQCD:   Nominee Frequency  *
000960000000      *            *              *                 Code               *
000970000000      *            *              *     DERDLV:     Nominee Method of  *
000980000000      *            *              *                 Delivery           *
000990000000      *            *              *     DERSNDTO:   Nominee Send To    *
001000000000      *            *              * - Modifications to format the      *
001010000000      *            *              *   Nominee Name, Account Number and *
001020000000      *            *              *   heading literal on the           *
001030000000      *            *              *   statement, if applicable.        *
001040000000      *            *              * - Modifications to suppress any    *
001050000000      *            *              *   blank lines between the last     *
001060000000      *            *              *   Investor address line and the    *
001070000000      *            *              *   Investor postal code.            *
001080030529      **************************************************************   *
001090000000      *M.Howorth   *  1997/01/08  * - Modifications to correctly read  *
001100000000      *            *     thru     *   the Dealer Representative        *
001110000000      *            *  1997/01/09  *   Mailing Instructions file with   *
001120000000      *            *              *   the Dealer Representative.       *
001130030529      **************************************************************   *
001140000000      *M.Howorth   *  1997/01/14  * - Modifications to correctly       *
001150000000      *            *              *   report the RRSP Contributions.   *
001160000000      *            *              * - Modifications to increase the    *
001170000000      *            *              *   number of accounts per investor, *
001180000000      *            *              *   SIA entries.                     *
001190030529      **************************************************************   *
001200000000      *M.Howorth   *  1997/01/16  * - Modifications to correctly       *
001210000000      *            *              *   format the first account entry   *
001220000000      *            *              *   for the investor (SIA).          *
001230030529      **************************************************************   *
001240000000      *M.Howorth   *  1997/01/17  * - Modifications to correctly       *
001250000000      *            *              *   format the country code on the   *
001260000000      *            *              *   Statement Print Header           *
001270000000      *            *              *   (MFASTMPHP).                     *
001280000000      *            *              * - Modifications for alignment.     *
001290030529      **************************************************************   *
001300000000      *M.Howorth   *  1997/01/20  * - Modifications to format the SIN  *
001310000000      *            *              *   as 999-999-999.                  *
001320030529      **************************************************************   *
001330000000      *M.Howorth   *  1997/01/22  * - Modifications to retrieve,       *
001340000000      *            *              *   format and print the correct     *
001350000000      *            *              *   Dealer Representative name and   *
001360000000      *            *              *   address, if different than the   *
001370000000      *            *              *   Dealer (DL) record.              *
001380030529      **************************************************************   *
001390000000      *M.Howorth   *  1997/01/28  * - Modifications to correctly check *
001400000000      *            *              *   if the Nominee account is to be  *
001410000000      *            *              *   suppressed for the ALL client    *
001420000000      *            *              *   copy run.                        *
001430030529      **************************************************************   *
001440000000      *M.Howorth   *  1997/01/29  * - Modifications to correctly spell *
001450000000      *            *              *   'VEUILLEZ', not 'VIUILLEZ'.      *
001460030529      **************************************************************   *
001470000000      *M.Howorth   *  1997/01/31  * - Modifications to string the      *
001480000000      *            *              *   spousal, locked-in and group     *
001490000000      *            *              *   flags into the account type      *
001500000000      *            *              *   description.                     *
001510030529      **************************************************************   *
001520000000      *M.Howorth   *  1997/02/04  * - Modifications to correctly       *
001530000000      *            *              *   format and print the Dealer      *
001540000000      *            *              *   Representative name and address. *
001550030529      **************************************************************   *
001560000000      *M.Howorth   *  1997/04/18  * RFS LOG # 398                      *
001570000000      *            *              * - Modifications to include         *
001580000000      *            *              *   Commissions and Withheld Taxes   *
001590000000      *            *              *   when formatting deductions.      *
001600030529      **************************************************************   *
001610000000      *H.SHENG     *  1997/05/28  * CHANGE OF MFASTMTFP FILE INTO      *
001620000000      *            *              * MFASTMEXP FILE                     *
001630030529      **************************************************************   *
001640000000      *M.Howorth   *  1997/06/09  * RFS LOG # 387                      *
001650000000      *            *     thru     * - Modifications to handle the      *
001660000000      *            *  1997/06/10  *   'Balance Closing' transaction.   *
001670030529      **************************************************************   *
001680000000      *M.Howorth   *  1997/07/21  * - Modifications to correctly       *
001690000000      *            *     thru     *   the statement for Clean          *
001700000000      *            *  1997/07/24  *   Environment (CEM).               *
001710030529      **************************************************************   *
001720000000      *M.Howorth   *  1997/08/11  * RFS LOG # 387                      *
001730000000      *            *     thru     * - Modifications to handle client   *
001740000000      *            *  1997/08/12  *   specific statements.  This will  *
001750000000      *            *              *   remove the need to have separate *
001760000000      *            *              *   versions of the program for each *
001770000000      *            *              *   client with different print      *
001780000000      *            *              *   requirements.                    *
001790030529      **************************************************************   *
001800000000      * M.Howorth  *  1997/10/21  * RFS #1634                          *
001810000000      *            *              * - Modifications to the detail      *
001820000000      *            *              *   print lines due to the size of a *
001830000000      *            *              *   capital gain.                    *
001840030529      **************************************************************   *
001850000000      *M.Howorth   *  1997/10/24  * RFS LOG #1502, #1544, #1549        *
001860000000      *            *     thru     * - Modifications to use the new LDA *
001870000000      *            *  1997/10/27  *   copy book (CPYLDASTMT).          *
001880000000      *            *              * - Modifications to bypass any      *
001890000000      *            *              *   unused records and to only       *
001900000000      *            *              *   report bypassed count totals.    *
001910030529      **************************************************************   *
001920000000      *M.Howorth   *  1997/10/31  * RFS LOG # 387                      *
001930000000      *            *              * - Modifications to correct         *
001940000000      *            *              *   alignment problems.              *
001950030529      **************************************************************   *
001960000000      *M.Howorth   *  1997/11/24  * RFS LOG # 1786                     *
001970000000      *            *              * - Modifications to change CUNDILL  *
001980000000      *            *              *   to version 3 from version 2.     *
001990030529      **************************************************************   *
002000000000      *M.Howorth   *  1997/11/28  * RFS LOG # 1786                     *
002010000000      *            *              * - Modifications to change CUNDILL  *
002020000000      *            *              *   to the standard version from     *
002030000000      *            *              *   version 3.                       *
002040030529      **************************************************************   *
002050000000      *M.Howorth   *  1997/12/01  * RFS LOG # 1815                     *
002060000000      *            *              * - Modifications to change Pacific  *
002070000000      *            *              *   Capital Management (PCM) to LEO  *
002080000000      *            *              *   Funds (LEO).                     *
002090030529      **************************************************************   *
002100000000      *M.Howorth   *  1997/12/02  * RFS LOG # 1636                     *
002110000000      *            *              * - Modifications to add the Net     *
002120000000      *            *              *   Invested to the summary line.    *
002130030529      **************************************************************   *
002140000000      *M.Howorth   *  1997/12/03  * RFS LOG # 1831                     *
002150000000      *            *              * - Modifications to change MARITIME *
002160000000      *            *              *   LIFE from the standard version   *
002170000000      *            *              *   to version 1.                    *
002180030529      **************************************************************   *
002190000000      *M.Howorth   *  1997/12/09  * RFS LOG # 1847                     *
002200000000      *            *              * - Modifications to change the      *
002210000000      *            *              *   French literal "UNIT" to         *
002220000000      *            *              *   "L'UNITE".                       *
002230030529      **************************************************************   *
002240000000      *M.Howorth   *  1997/12/12  * RFS LOG #1849                      *
002250000000      *            *              * - Modifications to format          *
002260000000      *            *              *   addresses for Quebec as street   *
002270000000      *            *              *   number, street type code, street *
002280000000      *            *              *   name instead of street number,   *
002290000000      *            *              *   street name, street type code.   *
002300030529      **************************************************************   *
002310000000      *M.Howorth   *  1997/12/23  * RFS LOG #1857                      *
002320971224      *            *     thru     * - Modifications to format a new    *
002330971224      *            *  1997/12/24  *   summary line for CANADIAN DOLLAR *
002340971223      *            *              *   CAPITAL GAINS.  The line is the  *
002350971223      *            *              *   Canadian dollar equivalent of    *
002360971223      *            *              *   fixed fund capital gain          *
002370971223      *            *              *   transactions with no units for   *
002380971223      *            *              *   non-registered accounts.  Fixed  *
002390971223      *            *              *   fund capital gain transactions   *
002400971223      *            *              *   with no units are suppressed.    *
002410971223      *            *              * RFS LOG #1918                      *
002420971223      *            *              * - Modifications to correctly print *
002430971223      *            *              *   French literals.                 *
002440030529      **************************************************************   *
002450980202      *H.SHENG     *  1998/01/15  * RFS LOG #1389                      *
002460980128      *            *              * -Modifications to add last name    *
002470980128      *            *              *  first name and initials to        *
002480980128      *            *              *  mfastmphp                         *
002490030529      **************************************************************   *
002500980206      *H.SHENG     *  1998/01/28  * RFS# 1871                          *
002510980128      *            *              * - Modifications to handle duplicate*
002520980128      *            *              *   confirmations and statements to  *
002530980128      *            *              *   the new JSI standard confirmation*
002540980128      *            *              *   and old statement                *
002550030529      **************************************************************   *
002560980202      *M.Howorth   *  1998/02/02  * RFS LOG # 2101                     *
002570980202      *            *              * - Modifications to handle McLean   *
002580980202      *            *              *   Budden using standard version 5. *
002590030529      **************************************************************
002600980218      *H.SHENG     *  1998/02/06  * RFS LOG # 2127                     *
002610980218      *            *              * - Modifications to suppress        *
002620980218      *            *              *   client's SIN for RBF             *
002630030529      **************************************************************   *
002640980218      *M.Howorth   *  1998/02/18  * RFS LOG # 2189                     *
002650980218      *            *              * - Modifications to alignment of    *
002660980218      *            *              *   standard version 5.              *
002670030529      **************************************************************   *
002680980331      *H.SHENG     *  1998/03/20  * RFS #2220                          *
002690980331      *            *              * - Modifications to correctly print *
002700980331      *            *              *   detailed account type and base   *
002710980331      *            *              *   the language of 'Account Type'   *
002720980417      *            *              *   on the language code of investor *
002730030529      **************************************************************   *
002740980417      * H.SHENG    *  1998/04/17  * RFS #1852                          *
002750980420      *            *              * - Add several new parms and do the *
002760980420      *            *              *   corresponding change to the CBL  *
002770980420      *            *              *   program                          *
002780030529      **************************************************************   *
002790980420      * H.SHENG    * 1998/04/20   * RFS #2482                          *
002800980420      *            *              * -Infinity would like to remove the *
002810980420      *            *              *  c/o attn line for the rep         *
002820980420      *            *              *  address                           *
002830030529      **************************************************************   *
002840981207      * DAISY LY   * 1998/07/21   * RFS 2317 - Include century for     *
002850981207      *            *              *  date fieled in the heading        *
002860030529      **************************************************************   *
002870981207      * H.SHENG    * 1998/12/07   * RFS 3502                           *
002880981207      *            *              * -MLA wants to display RIF and LIF  *
002890981207      *            *              *  max and min on statements.        *
002900030529      **************************************************************   *
002910990104      * H.SHENG    * 1998/01/04   * RFS 3706                           *
002920990104      *            *              * -Suppress the Merrill Lynch name   *
002930990104      *            *              *  from statements according to      *
002940990104      *            *              *  attached letter.                  *
002950030529      **************************************************************   *
002960990105      * H.SHENG    * 1998/12/29   * RFS 3611                           *
002970990105      *            *              * -Populate Legal Name field of      *
002980990105      *            *              *  statement header file.            *
002990030529      **************************************************************   *
003000990302      * H.SHENG    * 1999/03/02   * RFS 4069                           *
003010990302      *            *              * -Change statement layout.          *
003020030529      **************************************************************   *
003030990525      * H.SHENG    * 1999/05/25   * RFS 4620                           *
003040990525      *            *              * -Display only the Rep name on      *
003050990525      *            *              *  the statement.                    *
003060030529      **************************************************************   *
003070990611      * H.SHENG    * 1999/06/11   * RFS 4810                           *
003080990611      *            *              * -Modifications to fix an extra     *
003090990611      *            *              *  letter showing on the statement   *
003100990611      *            *              *  if the language is in French.     *
003110030529      **************************************************************   *
003120990611      * H.SHENG    * 1999/06/11   * RFS 4812                           *
003130990611      *            *              * -Modifications to only see the rep *
003140990611      *            *              *  name not even the address on the  *
003150990611      *            *              *  statement.                        *
003160030529      **************************************************************   *
003170990629      * H.SHENG    * 1999/06/25   * RFS 4764                           *
003180990629      *            *              * -Modifications to move the SIN to  *
003190990629      *            *              *  where the dealer/rep phone no is  *
003200990629      *            *              *  and center the account no.        *
003210030529      **************************************************************   *
003220990623      * H.SHENG    * 1999/06/23   * RFS 4902                           *
003230990623      *            *              * -Modifications for BTG to add the  *
003240990623      *            *              *  column headings to the base semi  *
003250990623      *            *              *  annual run.                       *
003260030529      **************************************************************   *
003270990716      * H.SHENG    * 1999/07/16   * RFS 5045 5054                      *
003280990716      *            *              * -Modifications for BTG to move     *
003290990716      *            *              *  down a couple of lines and also   *
003300990716      *            *              *  add columns headings to the base  *
003310990716      *            *              *  statement.                        *
003320030529      **************************************************************   *
003330990903      * H.SHENG    * 1999/09/03   * RFS 5159                           *
003340991014      *            *              * -Change WS-LDA-REP-NAME-ONLY to    *
003350990903      *            *              *  WS-LDA-DLR-REP-PRINT              *
003360030529      **************************************************************   *
003370990927      * B.LU       * 1999/09/27   * RFS 5464                           *
003380990927      *            *              * -Create a new version for UBS to   *
003390990927      *            *              *  use a new layout: Dealer/rep      *
003400990927      *            *              *  information, account position     *
003410990927      *            *              *  information, account type,        *
003420990927      *            *              *  deduction and net invested        *
003430990927      *            *              *  proceeds columns were removed;    *
003440990927      *            *              *  Column headings were also changed *
003450990927      *            *              *  in accordance with UBS' requires. *
003460030529      **************************************************************   *
003470991014      * B.LU       * 1999/10/14   * RFS 5010                           *
003480991014      *            *              * -Modifications were made to print  *
003490991014      *            *              *  the transaction contribute        *
003500991014      *            *              *  redemption description
003510991014      *            *              *  rather than transaction type on   *
003520991014      *            *              *  the statement report if it
003530991014      *            *              *  was an RESP "BUY" or "SEL" or     *
003540991014      *            *              *  "TRI" or "TRO" trad.              *
003550030529      **************************************************************   *
003560991021      * B.LU       * 1999/10/21   * RFS 5527                           *
003570991021      *            *              * -Modifications were made for GGF so*
003580991021      *            *              *  that the 6-digit date previously  *
003590991021      *            *              *  used be replaced by 8-digit date;
003600991021      *            *              *  Dealer/Rep name and address were  *
003610991021      *            *              *  moved 5 spaces to the left on the
003620991021      *            *              *  Statement Report.                 *
003630991022      *            *              *  A new version(7) was set for GGF. *
003640030529      **************************************************************   *
003650991104      * B.LU       * 1999/11/03   * RFS 5763                           *
003660991104      *            *              * -Modifications were made for CND to*
003670991104      *            *              *  override the transaction          *
003680991104      *            *              *  description with the contribution/
003690991104      *            *              *  redemption code description if the
003700991104      *            *              *  contribution/redemption code for
003710991104      *            *              *  the transaction was "MFCCNV".     *
003720030529      **************************************************************   *
003730991126      * B.LU       * 1999/11/26   * RFS 5148                           *
003740991126      *            *              * -Bypass Section-30 and Section-35. *
003750030529      **************************************************************   *
003760991216      * B.LU       * 1999/12/16   * RFS 5823                           *
003770991209      *            *              * -Print files were change for UBS.  *
003780991209      *            *              *  Font size was increase by 2 pts.  *
003790991209      *            *              *  Original Font(253 8), the changed
003800991216      *            *              *  Font(220 8). The changes were
003810991216      *            *              *  corespondance to the changes in   *
003820991216      *            *              *  print file.                       *
003830030529      **************************************************************   *
003840991216      * B.LU       * 1999/12/16   * RFS 6007                           *
003850991216      *            *              * -Modifications were made for MLA so*
003860991216      *            *              *  that the previously used 6-digit  *
003870991216      *            *              *  date be replaced by 8-digit date;
003880991216      *            *              *  Trade date column was increase by *
003890991216      *            *              *  3 and Trans-type column was
003900991216      *            *              *  decreased by 3. Keep all the rest.*
003910991216      *            *              *  A new version(8) was set for MLA. *
003920030529      **************************************************************   *
003930000105      * B.LU       * 2000/01/04   * ADHOC #342                         *
003940000105      *            *              * -Previously hard coded '19' year   *
003950000105      *            *              *  was replaced by As-at-date year.  *
003960030529      **************************************************************
003970000225      * B.LU       * 2000/02/21   * RFS 6532                           *
003980000225      *            *      TO 23   * -Increase field size for AMOUNT,   *
003990000225      *            *              *  UNIT-PRICE, and MARKET_VALUE.     *
004000000225      *            *              *  Changes apply only to  UBS.
004010030529      **************************************************************
004020000420      * B.LU       * 2000/04/20   * RFS 6365                           *
004030000420      *            *              * -The 6 digit rep code was printed  *
004040000420      *            *              *  on Statement and Confirm for MLA  *
004050000420      *            *              *  and all other clients.
004060030529      **************************************************************
004070000809      * B.LU       * 2000/08/09   * RFS 7330                           *
004080000809      *            *              * -MLA's version 8 was changed.The   *
004090000809      *            *              *  YYYYMMDD was populated with a     *
004100000809      *            *              *  date.
004110030529      **************************************************************
004120010115      * K.LIU      * 2001/01/15   * RFS 8480
004130010115      *            *              * - Modification of MARKET VALUE
004140010115      *            *              *   field. Increase its size from
004150010116      *            *              *   14 to 15 for Version 1(ATL).
004160030529      **************************************************************
004170010130      * B.LU       * 2001/01/22   * RFS 9298 9184                      *
004180010130      *            *              * - AT-GST. AT-PST and AT-ADMIN-FEES *
004190010130      *            *              *   were added to
004200010130      *            *              *  DEDUCTION that was missed before. *
004210030529      **************************************************************   *
004220010308      * K.LIU      * 2001/03/07   * RFS 8633
004230010321      *            *              * -BYPASS SECTION-12 25
004240030529      **************************************************************
004250010308      * K.LIU      * 2001/03/08   * RFS 4638
004260010314      *            *              * -BYPASS SECTION-40 43 45 90
004270030529      **************************************************************
004280010314      * K.LIU      * 2001/03/14   * RFS 9554
004290030529      **************************************************************
004300011022      * B.Lu       * 2001/10/22   * RFS10901, 11127, 10605
004310011022      *            *              * -Recompile only.
004320030529      **************************************************************   *
004330011123      * A.ALNEMER  * 2001/11/23   * RFS12150
004340011123      *            *              * -Recompile only.
004350030529      **************************************************************   *
004360020221      * B.LU       * 2002/02/04   * RFS12102
004370020221      *            *              * -Recompile only.
004380030529      **************************************************************   *
004390011211      * A.ALNEMER  * 2001/12/11   * RFS12090
004400011211      *            *              * -Increase the size of the array
004410011211      *            *              *  that store and process the        *
004420011211      *            *              *  account detail information        *
004430030529      **************************************************************   *
004440020313      * B.LU       * 2002/02/27   * RFS13107
004450020313      *            *              * -The 'Unit-price' field length
004460020313      *            *              * was changed from ZZZ.9999 to
004470020313      *            *              * Z,ZZZ,9999 for Version P5.
004480030529      **************************************************************   *
004490020326      * A.ALNEMER  * 2002/03/26   * RFS12173
004500020326      *            *              * -Recompile only.
004510030529      **************************************************************   *
004520020404      * A.ALNEMER  * 2002/04/04   * RFS13184
004530020404      *            *              * -Recompile only.
004540030529      **************************************************************   *
004550020416      * A.ALNEMER  * 2002/04/16   * RFS13211
004560020416      *            *              * -Recompile only.
004570030529      **************************************************************   *
004580020611      * A.ALNEMER  * 2002/06/11   * RFS13756
004590020611      *            *              * - If the Master Account number     *
004600020611      *            *              *   IS-MASTER-ACCOUNT-NO in section  *
004610020611      *            *              *   10 from the statement extract    *
004620020611      *            *              *   is populated, use it for         *
004630020611      *            *              *   for reporting and display        *
004640020611      *            *              *   instead of the Account-No.       *
004650030529      **************************************************************   *
004660020815      * FATEMA HAJI*  2002/07/29  * RFS 14470 - RECOMPILE FOR
004670020815      *            *              * INSTITUTION-BRANCH FILE CHANGE.
004680030529      **************************************************************   *
004690020801      * A.ALNEMER  * 2002/08/01   * RFS13637
004700020806      *            *              * - Subtract the SWO "SWITCH OUT" and*
004710020806      *            *              *   Add the SWI "SWITCH IN" to the   *
004720020806      *            *              *   NET INVESTED field.              *
004730030529      **************************************************************   *
004740020821      * A.ALNEMER  * 2002/08/21   * RFS13955
004750020821      *            *              * - Recompile only.                  *
004760030529      **************************************************************   *
004770020919      * A.ALNEMER  * 2002/08/21   * RFS14136
004780020821      *            *              * - Recompile only.                  *
004790030529      **************************************************************   *
004800020919      * A.ALNEMER  * 2002/09/19   * RFS14588
004810020919      *            *              * - If the Child-account is linked   *
004820020919      *            *              *   to the Master-Account file then  *
004830020919      *            *              *   display the Master Account.      *
004840030529      **************************************************************   *
004850020930      * B.LU       * 2002/09/25   * RFS14556  Recompile.
004860030529      **************************************************************   *
004870021202      * A.ALNEMER  * 2002/08/21   * RFS15022
004880021202      *            *              * - Recompile only.                  *
004890030529      **************************************************************
004900021213      * B.LU       * 2002/12/13   * RFS15028                           *
004910021213      *            *              * -The Account Statement Message will*
004920021213      *            *              *  be reported on the bases of New   *
004930021213      *            *              *  Investor or not.
004940030529      **************************************************************
004950030224      * C.LENTINI  * 2003/02/24   * RFS 16381 - Recompile
004960030529      **************************************************************
004970030319      * B.LU       * 2003/03/19   * RFS16256  RECOMPILE.
004980030529      **************************************************************
004990030325      * B.LU       * 2003/03/25   * RFS16633  RECOMPILE.
005000030529      **************************************************************
005010030509      * PING FAN   * 2003/05/09   * RFS17050  RECOMPILE.
005020010115      **************************************************************
005030030623      * O.McMillan * 2003/06/17   * RFS17217  RECOMPILE.
005040030623      **************************************************************
005050030529      * Odette     * 2003/05/08   * RFS17165  MFASTMEXP expanded from
005060030529      * McMillan   *              * 1100 to 1350. Recompile
005070030529      **************************************************************
005080030624      * Odette     * 2003/06/04   * RFS16881  Recompile.
005090030624      * McMillan   *              *
005100030722      **************************************************************
005110030722      * Brian Lu   * 2003/07/22   * RFS18156
005120030722      *            *              * - Modification of MARKET VALUE
005130030722      *            *              *   field. Increase its size from
005140030722      *            *              *   14 to 15 for Version 2(BTG).
005150030624      **************************************************************
005160030822      * Odette     * 2003/08/22   * RFS18091  Recompile for addition
005170030822      * McMillan   *              * of Section 17 to CPYJSISTMT.
005180030822      ******************************************************************
005190030825      * Odette     * 2003/08/25   * RFS18092  Correct French grammar &
005200030825      * McMillan   *              * accents.
005210030825      ******************************************************************
005220030903      * Odette     * 2003/08/25   * RFS17380  Recompile for additional
005230030903      * McMillan   *              * fields in Section 05 of CPYJSISTMT.
005240030903      ******************************************************************
005250031003      * Odette     * 2003/09/22   * RFS 18653 - Recompile.
005260031003      * McMillan   *              * Moved Section 17 rec counter field
005270031003      *            *              * in Section 99 to bottom of rec.
005280031003      ******************************************************************
005290031002      * B.LU       * 2003/10/02   * RFS17527  Recompile.
005300031002      ******************************************************************
005310031015      * ALI.A      * 2003/10/15   * RFS17626  Recompile.
005320031015      ******************************************************************
005330031020      * ALI.A      * 2003/10/20   * RFS18667  Recompile.
005340031020      ******************************************************************
005350031021      * ALI.A      * 2003/10/21   * RFS18312  Recompile.
005360031021      ******************************************************************
005370031121      * ALI.A      * 2003/11/21   * RFS18515  Recompile.
005380031126      *****************************************************************
005390031126      * B.LU       * 2003/11/21   * RFS19248  RECOMPILE.
005400031121      ******************************************************************
005410031216      * B.LU       * 2003/12/16   * RFS18814  RECOMPILE.
005420031216      ******************************************************************
005430040217      * ALI.A      * 2004/02/17   * RFS19444  RECOMPILE.
005440040217      ******************************************************************
005450040406      * ALI.A      * 2004/04/06   * RFS20870
005460040406      *            *              * Validate sections 33-38
005470040406      ******************************************************************
005480040414      * ALI.A      * 2004/04/14   * RFS20758  RECOMPILE.
005490040531      ******************************************************************
005500040531      * Reece Tam  * 2004/05/31   * RFS20804  Recompile - CPYLADASTMT.
005510040414      ******************************************************************
005520040908      * B.LU       * 2004/09/08   * RFS18924 -RECOMPILE - CPYJSISTMT.
005530040908      ******************************************************************
005540041012      * B.LU       * 2004/10/12   * RFS22856 -RECOMPILE - CPYJSISTMT.
005550041012      ******************************************************************
005560041025      * ALI.A      * 2004/10/25   * RFS18780 -RECOMPILE - CPYJSISTMT.
005570041025      ******************************************************************
005580041028      * ALI.A      * 2004/10/28   * RFS21715 -RECOMPILE - CPYJSISTMT.
005590041028      ******************************************************************
005600050104      * Villamar   * 2004/12/03   * RFS25032  Three sections of
005610050104      *            *              *  the duplicate statement do not
005620050104      *            *              *  align with the pre-printed form.
005630050104      *            *              *  >> Account Line
005640050104      *            *              *  >> Investment Summary
005650050104      *            *              *  >> Investment Summary Totals
005660050104      ******************************************************************
005670041220      * B.LU       * 2004/12/20   * RFS24514 -RECOMPILE - CPYJSISTMT
005680041220      ******************************************************************
005690050411      * B.LU       * 2005/04/11   * RFS26894 - Added AT-ERF to
005700050411      *            *              *            WS-DEDUCTIONS
005710050411      ******************************************************************
005720050427      * B.LU       * 2005/04/14   * RFS27421 -Remove the changes for
005730050427      *            *              * V1 under RFS25032.
005740050427      ******************************************************************
005750050429      * CHANDANA R * 2005/03/09   * RFS26213 -RECOMPILE - CPYJSISTMT
005760050429      ******************************************************************
005770050908      * ADE ADEYEMI* 2005/09/08   * RFS29550 -RECOMPILE - CPYJSISTMT
005780050908      ******************************************************************
005790051002      * ADE ADEYEMI* 2005/09/28   * RFS30065 -RECOMPILE - CPYJSISTMT
005800051002      ******************************************************************
005810051002      * Ali.A      * 2005/09/18   * RFS28931 -RECOMPILE - CPYJSISTMT
005820051002      ******************************************************************
005830050926      * C.LENTINI  * 2005/09/26   * RFS28692 -RECOMPILE - CPYJSISTMT
005840050926      ******************************************************************
005850051002      * Geeta S.   * 2005/09/15   * RFS 28107: Recompile for CPYJSISTMT
005860051002      ******************************************************************
005870060628      * Esin A.    * 2006/03/23   * RFS 31514: Recompile for CPYJSISTMT
005880060628      ******************************************************************
005890060628      * B.LU       * 2006/06/28   * RFS35042 -Process Section 21
005900060821      ******************************************************************
005910060821      * Raymond Mui* 2006/08/21   * RFS34848 - Recompile for CPYJSISTMT
005920060821      *            *              *            and CPYLDASTMT.
005930060616      ******************************************************************
005940061024      * L. Wong    * 2006/10/24   * RFS36342 - Recompile - CPYJSISTMT,
005950061024      *            *              *                        CPYLDASTMT.
005960061024      ******************************************************************
005970070625      * Lorne B    * 2007/06/25   * RFS 42682 - eGL Extract; Recompile
005980070625      ******************************************************************
005990071126      * Tony V.    * 2007/11/26   * RFS 45948 - expand market value
006000071126      *            *              *             total field for version
006010071126      *            *              *             5
006020080620      ******************************************************************
006030080620      * Bathuu L   * 2008/06/20   * RFS50993 - RECOMPILE FOR CPYJSISTMT
006040091111      ******************************************************************
006050091111      * A. Lo      * 2009/11/11   * RFS69515 - Recompile - CPYJSISTMT,
006060091111      *            *              *                        CPYLDASTMT.
006070091125      *            *              *            Process Section 63.
006080071126      ******************************************************************
006090100308      * A. Lo      * 2010/03/08   * RFS78161 -Recompile - CPYJSISTMT.
006100100308      ***************************************************************
006110100409      * A. Lo      * 2010/03/08   * RFS74626 -Recompile - CPYJSISTMT.
006120110805      * A Lo       * 2011/08/05   * RFS 91768 - Recompile.          *
006130100308      ***************************************************************
006140120913      * Brian Lu   * 2012/08/02   * RFS110996 - Modification of
006150120913      *            *              * MARKET VALUE field.  Increase its
006160120913      *            *              * size from 15 to 18 for Version 2
006170120913      *            *              * (BTG).
006180120913      * L. Wong    * 2012/08/27   * Unit tested Brian's changes.
006190120913      ******************************************************************
006200120920      * Suresh G.  * 2012/09/12   * RFS112003-Recompile for CPYJSISTMT.
006210120920      ******************************************************************
006220130408      * A. Chan    * 2013/04/08   * 117038 - Add Section 22
006230130408      ******************************************************************
006240130529      * NOBLE T .  * 2013/05/29   * RFS121116-RECOMPILE FOR CPYLDASTMT.
006250130702      * K Naveen   * 2013/07/02   * RFS123719 -  Recompile Only.
006260130529      ******************************************************************
006270130905      * PRAVEEN E  * 2013/09/05   *RFS123925 RECOMPIL ONLY - CPYJSISTMT.
006280140519      * NOBLE T    * 2014/05/02   * RFS130239 - RECOMPILE - CPYJSISTMT.
006290140418      ******************************************************************
006300140418      *SANGEETHA V  * 2014/03/25 * RFS129378 - RECOMPILE FOR CPYJSISTMT*
006310140624      * JEYARAJAN  * 2014/06/24   * RFS132121 - INTRODUCE A NEW FIELD  *
006320140624      *            *              * TO HANDLE THE ERROR ON SEC 16      *
006330141029      ******************************************************************
006340141007      * SURESH G.  * 2014/08/14   * RFS133489-RECOMPILE FOR CPYJSISTMT.
006350141007      * JEYARAJAN  * 2014/10/07   * RFS141165 - Jobconfirm MSW         *
006360150212      * SANGEETHA V * 2015/02/12  * RFS 133528 - MV Summary Totals     *
006370150212      *             *             * are truncated when the amount      *
006380150212      *             *             * is more than 999,999,999.99.       *
006390150224      * POOJA K    * 2015/02/16  * RFS143392 - RECOMPILE FOR CPYJSISTMT*
006400150703      * POOJA K    * 2015/03/27  * RFS144030 - RECOMPILE FOR CPYJSISTMT*
006410150703      * POOJA K    * 2015/04/21  * RFS142884 - RECOMPILE FOR CPYJSISTMT*
006420150703      * POOJA K    * 2015/05/08  * RFS144587 - RECOMPILE FOR CPYJSISTMT*
006430150515      * SANGEETHA V * 2015/05/06  * RFS 147149 - AVERAGE COST HEADER IS*
006440150515      *             *             * NOT POPULATED FOR VERSION P4.
006450150609      * Sangeetha V * 2015/06/09  * RFS148012 - Average cost truncated *
006460150609      *             *             * for version P7.                    *
006470150902      * KAHAKASHAN  * 2015/09/02  * RFS149112 - AVERAGE COST TRUNCATED *
006480150902      *             *             * IN SPOOL FILE.                     *
006490151218      * VINSFY J    * 2015/12/17  * RFS154219 - AVERAGE COST TRUNCATED *
006500151217      *             *             * ON PRINT VERSION 2.                *
006510160928      * MUTHU S     * 2016/03/18  * RFS155470 AVERAGE COST UNIT IS     *
006520160928      *             *             * TRUNCATED ON STATEMENT SPOOLED FILE*
006530160713      * RAGAVI S    * 2016/05/06  * RFS157301 - THE AVERAGE COST FIGURE*
006540160713      *             *             * ON STATEMENTS IS BEING TRUNCATED   *
006550161012      * POOJA K     * 2016/08/31  * RFS156599 - ACCOUNT HOLDING MORE   *
006560161012      *             *             * THEN 500 INVESTMENTS NOT GETTING   *
006570161012      *             *             * REPORTED IN THE EXTRACT            *
006580161108      * ARTHY K     * 2016/10/28  * RFS164194 - ACCOUNT HOLDING MORE   *
006590161108      *             *             * THEN 2000 INVESTMENTS NOT GETTING  *
006600161108      *             *             * REPORTED IN THE EXTRACT            *
006610190619      * MAYILSAMY D* 2019/06/18   * RFS183449 - POPULATE ACCOUNT TYPE  *
006620190619      *            *              * CODE BASED UPON INVESTOR LANGUAGE  *
006630190905      *            *              * IN STATEMENT EXTRACT               *
006631190924      * Arthy K    * 2019/09/03   * RFS1018431 - Rollback changes done *
006632190924      *            *              * in RFS: 183449                     *
006633190924      * Arthy K    * 2019/09/24   * RFS184715 - Put back changes in RFS*
006634190924      *            *              * 183449 with additional logic of    *
006635190924      *            *              * new edit code 'STMATL'             *
006636200704      * Mayilsamy D* 2020/07/04   * RFS186092 - Recompile              *
006637211019      * Surendran C* 2021/10/19   * RFS186592 -RECOMPILE FOR CPYJSISTMT*
006640130905      ******************************************************************
006650991014
006660000000      /
006670000000       ENVIRONMENT DIVISION.
006680000000
006690000000      ******************************************************************
006700000000      *                                                                *
006710000000      *    E N V I R O N M E N T   D I V I S I O N                     *
006720000000      *                                                                *
006730000000      ******************************************************************
006740000000
006750000000       CONFIGURATION SECTION.
006760000000
006770000000      ******************************************************************
006780000000      *                                                                *
006790000000      *    C O N F I G U R A T I O N   S E C T I O N                   *
006800000000      *                                                                *
006810000000      ******************************************************************
006820000000
006830000000       SOURCE-COMPUTER.    AS-400.
006840000000       OBJECT-COMPUTER.    AS-400.
006850000000
006860000000       SPECIAL-NAMES.
006870000000
006880000000      *****
006890000000      *****    Special Names.
006900000000      *****
006910000000
006920000000           LOCAL-DATA          IS  WS-LOCAL.
006930000000      /
006940000000       INPUT-OUTPUT SECTION.
006950000000
006960000000      ******************************************************************
006970000000      *                                                                *
006980000000      *    I N P U T   /   O U T P U T   S E C T I O N                 *
006990000000      *                                                                *
007000000000      ******************************************************************
007010000000
007020000000       FILE-CONTROL.
007030000000
007040000000      *****
007050000000      *****    FILE CONTROL.
007060000000      *****
007070000000      /
007080990116           SELECT  ACCOUNT-FILE
007090990116                   ASSIGN          TO  DATABASE-MFAACCNTP
007100990116                   ORGANIZATION    IS  INDEXED
007110990116                   ACCESS          IS  DYNAMIC
007120990116                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
007130990116
007140190612      *RFS183449-Starts
007150190612           SELECT  ACCOUNT-FILE-2
007160190612                   ASSIGN          TO  DATABASE-MFAACCNTP
007170190612                   ORGANIZATION    IS  INDEXED
007180190612                   ACCESS          IS  DYNAMIC
007190190612                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
007200190612      *RFS183449-Ends
007210190612
007220000000           SELECT DEALER-MAILING-INSTR
007230000000               ASSIGN          TO  DATABASE-MFADEAMIP
007240000000               ORGANIZATION    IS  INDEXED
007250000000               ACCESS          IS  DYNAMIC
007260000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007270000000               FILE STATUS     IS  WS-FILSTAT.
007280000000
007290000000           SELECT DEALER-FILE
007300000000               ASSIGN          TO  DATABASE-MFADEAP
007310000000               ORGANIZATION    IS  INDEXED
007320000000               ACCESS          IS  RANDOM
007330000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007340000000               FILE STATUS     IS  WS-FILSTAT.
007350000000
007360000000           SELECT DEALER-REP-FILE
007370000000               ASSIGN          TO  DATABASE-MFADEAREP
007380000000               ORGANIZATION    IS  INDEXED
007390000000               ACCESS          IS  RANDOM
007400000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007410000000               FILE STATUS     IS  WS-FILSTAT.
007420000000
007430000000           SELECT DEALER-REP-MAILING-INSTR
007440000000               ASSIGN          TO  DATABASE-MFADERMIP
007450000000               ORGANIZATION    IS  INDEXED
007460000000               ACCESS          IS  DYNAMIC
007470000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007480000000               FILE STATUS     IS  WS-FILSTAT.
007490980326
007500000000           SELECT DEALER-REP-ADDRESS
007510000000               ASSIGN          TO  DATABASE-MFADRADRP
007520000000               ORGANIZATION    IS  INDEXED
007530000000               ACCESS          IS  RANDOM
007540000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007550000000               FILE STATUS     IS  WS-FILSTAT.
007560000000
007570000000           SELECT INSTITUTION-ADDRESS
007580000000               ASSIGN          TO  DATABASE-MFAINADDP
007590000000               ORGANIZATION    IS  INDEXED
007600000000               ACCESS          IS  RANDOM
007610000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007620000000               FILE STATUS     IS  WS-FILSTAT.
007630000000
007640000000           SELECT INSTITUTION-BRANCH
007650000000               ASSIGN          TO  DATABASE-MFAINSBRP
007660000000               ORGANIZATION    IS  INDEXED
007670000000               ACCESS          IS  RANDOM
007680000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007690000000               FILE STATUS     IS  WS-FILSTAT.
007700980326
007710980326           SELECT  INVESTOR-JOIN
007720980326                   ASSIGN          TO  DATABASE-MFAACOJLA
007730980326                   ORGANIZATION    IS  INDEXED
007740980326                   ACCESS          IS  DYNAMIC
007750980326                   RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007760980326                                 WITH  DUPLICATES.
007770980326           SELECT NOMINEE-ACCOUNT-TYPE-DESCR
007780980326               ASSIGN          TO  DATABASE-MFANMTPYP
007790980326               ORGANIZATION    IS  INDEXED
007800980326               ACCESS          IS  DYNAMIC
007810980326               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
007820991103
007830991103           SELECT ACCOUNT-TYPE-BK
007840991103               ASSIGN          TO  DATABASE-MFAACCTYP
007850991103               ORGANIZATION    IS  INDEXED
007860991103               ACCESS          IS  SEQUENTIAL
007870991103               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
007880991103
007890000000           SELECT NOMINEE-MAILING-INSTRUCTIONS
007900000000               ASSIGN          TO  DATABASE-MFANOMMIP
007910000000               ORGANIZATION    IS  INDEXED
007920000000               ACCESS          IS  DYNAMIC
007930000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
007940000000               FILE STATUS     IS  WS-FILSTAT.
007950000000
007960020920R14588     SELECT MASTER-ACCOUNT
007970020920               ASSIGN          TO  DATABASE-MFAMACCP
007980020920               ORGANIZATION    IS  INDEXED
007990020920               ACCESS          IS  DYNAMIC
008000020920               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
008010020919
008020020919R14588     SELECT DEALER-MISC-DETAILS
008030020919               ASSIGN          TO  DATABASE-MFADEAMDP
008040020919               ORGANIZATION    IS  INDEXED
008050020919               ACCESS          IS  DYNAMIC
008060020919               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY.
008070020919
008080000000           SELECT STMT-MSG-FILE
008090000000               ASSIGN          TO  DATABASE-MFASTMMSP
008100000000               ORGANIZATION    IS  INDEXED
008110000000               ACCESS          IS  DYNAMIC
008120000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
008130000000               FILE STATUS     IS  WS-FILSTAT.
008140000000
008150000000           SELECT STMT-PRT-DTL-FILE
008160000000               ASSIGN          TO  DATABASE-MFASTMPDP
008170000000               ORGANIZATION    IS  INDEXED
008180000000               ACCESS          IS  DYNAMIC
008190000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
008200000000               FILE STATUS     IS  WS-FILSTAT.
008210000000
008220000000           SELECT STMT-PRT-HDR-FILE
008230000000               ASSIGN          TO  DATABASE-MFASTMPHP
008240000000               ORGANIZATION    IS  INDEXED
008250000000               ACCESS          IS  DYNAMIC
008260000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
008270000000               FILE STATUS     IS  WS-FILSTAT.
008280000000
008290000000           SELECT STMT-DTA-FILE
008300000000               ASSIGN          TO  DATABASE-MFASTMEXP
008310000000               ORGANIZATION    IS  INDEXED
008320000000               ACCESS          IS  SEQUENTIAL
008330000000               RECORD KEY      IS  EXTERNALLY-DESCRIBED-KEY
008340000000               FILE STATUS     IS  WS-FILSTAT.
008350000000
008360000000           SELECT PRINT-FILE
008370000000               ASSIGN          TO  PRINTER-QPRINT.
008380000000      /
008390000000       DATA DIVISION.
008400000000
008410000000      ******************************************************************
008420000000      *                                                                *
008430000000      *    D A T A   D I V I S I O N                                   *
008440000000      *                                                                *
008450000000      ******************************************************************
008460000000
008470000000       FILE SECTION.
008480000000
008490000000      ******************************************************************
008500000000      *                                                                *
008510000000      *    F I L E   S E C T I O N                                     *
008520000000      *                                                                *
008530000000      ******************************************************************
008540990116      /
008550990116       FD  ACCOUNT-FILE
008560990116           LABEL RECORDS      ARE  STANDARD.
008570990116
008580990116       01  ACCOUNT-REC.                COPY DD-ALL-FORMATS
008590990116                                            OF MFAACCNTP.
008600000000      /
008610190612      *RFS183449-Starts
008620190612       FD  ACCOUNT-FILE-2
008630190612           LABEL RECORDS      ARE  STANDARD.
008640190612
008650190619       01  ACCOUNT-REC-2.
008660190619       COPY DD-ALL-FORMATS OF MFAACCNTP.
008661190619
008670190612      *RFS183449-Ends
008680000000      ******************************************************************
008690000000      *                                                                *
008700000000      *    MFADEAMIP:  Dealer Mailing Instruction                      *
008710000000      *                                                                *
008720000000      ******************************************************************
008730990116      /
008740000000       FD  DEALER-MAILING-INSTR
008750000000           LABEL RECORDS      ARE  OMITTED.
008760000000
008770000000       01  DEALER-MAILING-INSTR-RECD.  COPY DD-ALL-FORMATS
008780000000                                           OF  MFADEAMIP.
008790000000      /
008800000000      ******************************************************************
008810000000      *                                                                *
008820000000      *    MFADEAP:    Dealer                                          *
008830000000      *                                                                *
008840000000      ******************************************************************
008850000000
008860000000       FD  DEALER-FILE
008870000000           LABEL RECORDS      ARE  OMITTED.
008880000000
008890000000       01  DEALER-RECD.                COPY DD-ALL-FORMATS
008900000000                                           OF  MFADEAP.
008910000000      /
008920000000      ******************************************************************
008930000000      *                                                                *
008940000000      *    MFADEAREP:  Dealer Representative                           *
008950000000      *                                                                *
008960000000      ******************************************************************
008970000000
008980000000       FD  DEALER-REP-FILE
008990000000           LABEL RECORDS      ARE  OMITTED.
009000000000
009010000000       01  DEALER-REP-RECD.            COPY DD-ALL-FORMATS
009020000000                                           OF  MFADEAREP.
009030000000      /
009040000000      ******************************************************************
009050000000      *                                                                *
009060000000      *    MFADERMIP:  Dealer Representative Mailing Instruction       *
009070000000      *                                                                *
009080000000      ******************************************************************
009090000000
009100000000       FD  DEALER-REP-MAILING-INSTR
009110000000           LABEL RECORDS      ARE  OMITTED.
009120000000
009130000000       01  DLRREP-MAILING-INSTR-RECD.  COPY DD-ALL-FORMATS
009140000000                                           OF  MFADERMIP.
009150000000      /
009160000000      ******************************************************************
009170000000      *                                                                *
009180000000      *    MFADRADRP:  Dealer Representative Address                   *
009190000000      *                                                                *
009200000000      ******************************************************************
009210000000
009220000000       FD  DEALER-REP-ADDRESS
009230000000           LABEL RECORDS      ARE  OMITTED.
009240000000
009250000000       01  DEALER-REP-ADDRESS-RECD.    COPY DD-ALL-FORMATS
009260000000                                           OF  MFADRADRP.
009270000000      /
009280000000      ******************************************************************
009290000000      *                                                                *
009300000000      *    MFAINADDP:  Institution Address                             *
009310000000      *                                                                *
009320000000      ******************************************************************
009330000000
009340000000       FD  INSTITUTION-ADDRESS
009350000000           LABEL RECORDS      ARE  OMITTED.
009360000000
009370000000       01  INSTITUTION-ADDRESS-RECD.   COPY DD-ALL-FORMATS
009380000000                                           OF  MFAINADDP.
009390000000      /
009400000000      ******************************************************************
009410000000      *                                                                *
009420000000      *    MFAINSBRP:  Institution Branch                              *
009430000000      *                                                                *
009440000000      ******************************************************************
009450000000
009460000000       FD  INSTITUTION-BRANCH
009470000000           LABEL RECORDS      ARE  OMITTED.
009480000000
009490000000       01  INSTITUTION-BRANCH-RECD.    COPY DD-ALL-FORMATS
009500000000                                           OF  MFAINSBRP.
009510000000      /
009520980326      ****************************************************************
009530980326      *                                                              *
009540980326      *  MFAACOJLA:    Investor Join                                 *
009550980326      *                                                              *
009560980326      ****************************************************************
009570980326
009580980326       FD  INVESTOR-JOIN
009590980326           LABEL RECORDS      ARE  STANDARD.
009600980326
009610980326       01  INVESTOR-J-REC.             COPY DD-ALL-FORMATS
009620980326                                            OF MFAACOJLA.
009630980326      /
009640980326      *****************************************************************
009650980326      *                                                               *
009660980326      *    MFANMTPYP:  Nominee Account Type Descr                     *
009670980326      *                                                               *
009680980326      *****************************************************************
009690980326
009700980326       FD  NOMINEE-ACCOUNT-TYPE-DESCR
009710980326           LABEL RECORDS ARE STANDARD.
009720980326
009730980326       01  NOMINEE-ACCOUNT-TYPE-DESCR-REC.
009740980326                               COPY DD-ALL-FORMATS OF MFANMTPYP.
009750991103
009760991103      /
009770991103       FD  ACCOUNT-TYPE-BK
009780991103           LABEL RECORDS      ARE  STANDARD.
009790991103
009800991103       01  ACCOUNT-TYPE-BK-REC.        COPY DD-ALL-FORMATS
009810991103                                            OF MFAACCTYP.
009820991103
009830000000      ******************************************************************
009840000000      *                                                                *
009850000000      *    MFANOMMIP:  Nominee Mailing Instruction                     *
009860000000      *                                                                *
009870000000      ******************************************************************
009880000000
009890000000       FD  NOMINEE-MAILING-INSTRUCTIONS
009900000000           LABEL RECORDS      ARE  OMITTED.
009910000000
009920000000       01  NOMINEE-MAILING-INST-RECD.  COPY DD-ALL-FORMATS
009930000000                                           OF  MFANOMMIP.
009940000000      /
009950020919      ******************************************************************
009960020919      *                                                                *
009970020919      *    MFADEAMDP:  DEALER-MISC-DETAILS                             *
009980020919      *                                                                *
009990020919      ******************************************************************
010000020919
010010020919R14588 FD  DEALER-MISC-DETAILS
010020020919           LABEL RECORDS ARE STANDARD.
010030020919       01  DEALER-MISC-DETAILS-REC.     COPY DD-ALL-FORMATS
010040020919                                          OF MFADEAMDP.
010050020919      /
010060020919      ******************************************************************
010070020919      *                                                                *
010080020919      *    MFAMACCP:  MASTER ACCOUNT FILE.                             *
010090020919      *                                                                *
010100020919      ******************************************************************
010110020919
010120020919R14588 FD  MASTER-ACCOUNT
010130020919           LABEL RECORDS      ARE  OMITTED.
010140020919       01  MASTER-ACCOUNT-REC.  COPY DD-ALL-FORMATS
010150020919                                             OF  MFAMACCP.
010160020919      /
010170000000      ******************************************************************
010180000000      *                                                                *
010190000000      *    MFASTMMSP:  Statement Message                               *
010200000000      *                                                                *
010210000000      ******************************************************************
010220000000
010230000000       FD  STMT-MSG-FILE
010240000000           LABEL RECORDS      ARE  OMITTED.
010250000000
010260000000       01  STMT-MSG-RECD.              COPY DD-ALL-FORMATS
010270000000                                           OF  MFASTMMSP.
010280000000      /
010290000000      ******************************************************************
010300000000      *                                                                *
010310000000      *    MFASTMPDP:  Statement Print Detail                          *
010320000000      *                                                                *
010330000000      ******************************************************************
010340000000
010350000000       FD  STMT-PRT-DTL-FILE
010360000000           LABEL RECORDS      ARE  OMITTED.
010370000000
010380000000       01  STMT-PRT-DTL-RECD.          COPY DD-ALL-FORMATS
010390000000                                           OF  MFASTMPDP.
010400000000      /
010410000000      ******************************************************************
010420000000      *                                                                *
010430000000      *    MFASTMPHP:  Statement Print Header                          *
010440000000      *                                                                *
010450000000      ******************************************************************
010460000000
010470000000       FD  STMT-PRT-HDR-FILE
010480000000           LABEL RECORDS      ARE  OMITTED.
010490000000
010500000000       01  STMT-PRT-HDR-RECD.          COPY DD-ALL-FORMATS
010510000000                                           OF  MFASTMPHP.
010520000000      /
010530000000      ******************************************************************
010540000000      *                                                                *
010550000000      *    MFASTMEXP:  Statement Extract Data                          *
010560000000      *                                                                *
010570000000      ******************************************************************
010580000000
010590000000       FD  STMT-DTA-FILE
010600000000           LABEL RECORDS      ARE  OMITTED.
010610000000
010620000000       01  STMT-DTA-RECD.              COPY DD-ALL-FORMATS
010630000000                                       OF MFASTMEXP.
010640000000      /
010650000000      ******************************************************************
010660000000      *                                                                *
010670000000      *    QPRINT:     Control Report                                  *
010680000000      *                                                                *
010690000000      ******************************************************************
010700000000
010710000000       FD  PRINT-FILE
010720000000           LABEL RECORDS      ARE  OMITTED.
010730000000
010740000000       01  PRINT-RECD                  PICTURE X(150).
010750000000      /
010760000000       WORKING-STORAGE SECTION.
010770000000
010780000000      ******************************************************************
010790000000      *                                                                *
010800000000      *    W O R K I N G   S T O R A G E   S E C T I O N               *
010810000000      *                                                                *
010820000000      ******************************************************************
010830000000      /
010840000000       01  WS-LOCAL-DATA-AREA.         COPY CPYLDASTMT.
010850000000      /
010860000000       01  WS-DEAMLGINS-REC.           COPY DD-ALL-FORMATS
010870000000                                           OF  MFADEAMIP.
010880000000      /
010890000000       01  WS-DERMLGINS-REC.           COPY DD-ALL-FORMATS
010900000000                                           OF  MFADERMIP.
010910000000      /
010920000000       01  WS-NOMMLGINS-REC.           COPY DD-ALL-FORMATS
010930000000                                           OF  MFANOMMIP.
010940000000      /
010950000000                                       COPY CPYJSISTMT.
010960000000      /
010970000000      ******************************************************************
010980000000      *                                                                *
010990000000      *    S T A T E M E N T   S A V E   P A G E                       *
011000000000      *                                                                *
011010000000      ******************************************************************
011020000000
011030000000       01  WS-STATEMENT-PAGE.
011040000000           03  WS-STMTPGE-LNE          OCCURS  88  TIMES
011050000000                                       INDEXED BY  SPL
011060000000                                       PICTURE X(150).
011070000000
011080000000      *****
011090030827      *****    Work Area for Currency
011100000000      *****
011110030827R18092 01  WS-CURRENCY-XXX             PICTURE X(3).
011120030827   |   01  WS-CURRENCY-XXX-R           REDEFINES WS-CURRENCY-XXX.
011130030827   |       05  WS-CURRENCY-XX          PICTURE X(2).
011140030827   |       05  WS-CURRENCY-X           PICTURE X.
011150030827   |   01  WS-CURRENCY-XXX-W.
011160030827   |       05  WS-CURRENCY-XX-W        PICTURE X(2).
011170030827   |       05  WS-CURRENCY-X-W         PICTURE X.
011180030827   |   01  WS-CURRENCY-PIC3            REDEFINES WS-CURRENCY-XXX-W
011190030827   V                                   PIC X(3).
011200030827      *****
011210030827      *****    Standard Version
011220030827      *****
011230030827
011240000000       01  WS-STATEMENT-PAGE-R         REDEFINES WS-STATEMENT-PAGE.
011250000000           03  FILLER                  PICTURE X(3750).
011260000000           03  WS-STMTPGE-DTL          PICTURE X(6450).
011270000000           03  FILLER                  PICTURE X(150).
011280000000           03  WS-STMTPGE-MSG          PICTURE X(2100).
011290000000           03  FILLER                  PICTURE X(750).
011300000000
011310000000      *****
011320000000      *****    Version 1:  Used by AIC, INF, MLA.
011330000000      *****
011340000000
011350000000       01  WS-STATEMENT-PAGE-V1-R      REDEFINES WS-STATEMENT-PAGE.
011360000000           03  FILLER                  PICTURE X(3900).
011370000000           03  WS-STMTPGE-DTL-V1       PICTURE X(6450).
011380000000           03  FILLER                  PICTURE X(150).
011390000000           03  WS-STMTPGE-MSG-V1       PICTURE X(2100).
011400000000           03  FILLER                  PICTURE X(600).
011410000000
011420000000      *****
011430990716      *****    Version 2:  Used by CND
011440000000      *****
011450000000
011460000000       01  WS-STATEMENT-PAGE-V2-R      REDEFINES WS-STATEMENT-PAGE.
011470000000           03  FILLER                  PICTURE X(3900).
011480990226           03  WS-STMTPGE-DTL-V2       PICTURE X(6450).
011490990301           03  WS-STMTPGE-MSG-V2       PICTURE X(2250).
011500990226           03  FILLER                  PICTURE X(600).
011510000000
011520990716      *****
011530990727      *****    Version 2:  Used by BTG & GRS
011540990716      *****
011550990716
011560990726       01  WS-STATEMENT-PAGE-V21-R     REDEFINES WS-STATEMENT-PAGE.
011570990716           03  FILLER                  PICTURE X(4650).
011580990726           03  WS-STMTPGE-DTL-V21      PICTURE X(6000).
011590990716           03  FILLER                  PICTURE X(150).
011600990726           03  WS-STMTPGE-MSG-V21      PICTURE X(1800).
011610990722           03  FILLER                  PICTURE X(600).
011620990726
011630000000      *****
011640000000      *****    Version 3:  Used by LEO & NFC.
011650000000      *****
011660000000
011670000000       01  WS-STATEMENT-PAGE-V3-R      REDEFINES WS-STATEMENT-PAGE.
011680000000           03  FILLER                  PICTURE X(4050).
011690000000           03  WS-STMTPGE-DTL-V3       PICTURE X(6450).
011700000000           03  FILLER                  PICTURE X(150).
011710000000           03  WS-STMTPGE-MSG-V3       PICTURE X(2100).
011720000000           03  FILLER                  PICTURE X(450).
011730000000
011740000000      *****
011750000000      *****    Version 4:  Used by RBF.
011760000000      *****
011770000000
011780000000       01  WS-STATEMENT-PAGE-V4-R      REDEFINES WS-STATEMENT-PAGE.
011790000000           03  FILLER                  PICTURE X(4050).
011800000000           03  WS-STMTPGE-DTL-V4       PICTURE X(6450).
011810000000           03  FILLER                  PICTURE X(150).
011820000000           03  WS-STMTPGE-MSG-V4       PICTURE X(1800).
011830000000           03  FILLER                  PICTURE X(750).
011840000000
011850000000      *****
011860980202      *****    Version 5:  Used by GLS & MBU.
011870000000      *****
011880000000
011890000000       01  WS-STATEMENT-PAGE-V5-R      REDEFINES WS-STATEMENT-PAGE.
011900000000           03  FILLER                  PICTURE X(4050).
011910000000           03  WS-STMTPGE-DTL-V5       PICTURE X(6450).
011920000000           03  FILLER                  PICTURE X(150).
011930000000           03  WS-STMTPGE-MSG-V5       PICTURE X(1800).
011940000000           03  FILLER                  PICTURE X(750).
011950990922
011960990922      *****
011970990922      *****    Version 6:  Used by UBS.
011980990922      *****
011990990922
012000990922       01  WS-STATEMENT-PAGE-V6-R      REDEFINES WS-STATEMENT-PAGE.
012010990927           03  FILLER                  PICTURE X(4650).
012020990927           03  WS-STMTPGE-DTL-V6       PICTURE X(5850).
012030990922           03  FILLER                  PICTURE X(150).
012040990922           03  WS-STMTPGE-MSG-V6       PICTURE X(1800).
012050990922           03  FILLER                  PICTURE X(750).
012060991019
012070991019      *****
012080991019      *****    Version 7:  Used by GGF.
012090991019      *****
012100991019
012110991021       01  WS-STATEMENT-PAGE-V7-R      REDEFINES WS-STATEMENT-PAGE.
012120991021           03  FILLER                  PICTURE X(3900).
012130991021           03  WS-STMTPGE-DTL-V7       PICTURE X(6450).
012140991021           03  FILLER                  PICTURE X(150).
012150991021           03  WS-STMTPGE-MSG-V7       PICTURE X(2100).
012160991021           03  FILLER                  PICTURE X(600).
012170991213
012180991213      *****
012190991213      *****    Version 8:  Used MLA.
012200991213      *****
012210991213
012220991213       01  WS-STATEMENT-PAGE-V8-R      REDEFINES WS-STATEMENT-PAGE.
012230991213           03  FILLER                  PICTURE X(3900).
012240991213           03  WS-STMTPGE-DTL-V8       PICTURE X(6450).
012250991213           03  FILLER                  PICTURE X(150).
012260991213           03  WS-STMTPGE-MSG-V8       PICTURE X(2100).
012270991213           03  FILLER                  PICTURE X(600).
012280991213
012290991021
012300000000      /
012310000000      ******************************************************************
012320000000      *                                                                *
012330000000      *    S T A T E M E N T   M E S S A G E   S A V E   A R E A       *
012340000000      *                                                                *
012350000000      ******************************************************************
012360000000
012370000000       01  WS-STATEMENT-PAGE-MESSAGE.
012380000000           03  WS-STMTPGE-MSGLNE       OCCURS  14  TIMES
012390000000                                       INDEXED BY  SPML
012400000000                                       PICTURE X(150).
012410000000      /
012420000000      ******************************************************************
012430000000      *                                                                *
012440000000      *    S T A T E M E N T   H E A D I N G   P R I N T   L I N E S   *
012450000000      *                                                                *
012460000000      ******************************************************************
012470000000
012480000000      *****
012490000000      *****    The statement heading lines will be re-defined for each
012500000000      *****    version of the statement.
012510000000      *****        Version 1:  Used by AIC, INF, MLA.
012520990716      *****        Version 2:  Used by CND, BTG, GRS
012530000000      *****        Version 3:  Used by LEO & NFC.
012540000000      *****        Version 4:  Used by RBF.
012550980202      *****        Version 5:  Used by GLS & MBU.
012560990922      *****        Version 6:  Used by UBS.
012570991019      *****        Version 7:  Used by GGF.
012580991213      *****        Version 8:  Used by MLA.
012590000000      *****
012600990927
012610000000      /
012620000000       01  STMT-HDG-01.
012630000000           03  FILLER                  PICTURE X(2).
012640000000           03  SH01-ACCOUNT-TYPE       PICTURE X(75).
012650000000           03  FILLER                  PICTURE X(4).
012660000000           03  SH01-LIT1               PICTURE X(20).
012670000000           03  FILLER                  PICTURE X(1).
012680980722           03  SH01-START-DATE         PICTURE XXXX/XX/XX.
012690000000           03  FILLER                  PICTURE X(1).
012700000000           03  SH01-LIT2               PICTURE X(2).
012710000000           03  FILLER                  PICTURE X(1).
012720980722           03  SH01-END-DATE           PICTURE XXXX/XX/XX.
012730980722           03  FILLER                  PICTURE X(24).
012740000000
012750000000       01  STMT-HDG-01-V1-R            REDEFINES STMT-HDG-01.
012760000000           03  FILLER                  PICTURE X(2).
012770000000           03  SH01V1-ACCOUNT-TYPE     PICTURE X(75).
012780000000           03  FILLER                  PICTURE X(4).
012790000000           03  SH01V1-LIT1             PICTURE X(20).
012800000000           03  FILLER                  PICTURE X(1).
012810980722           03  SH01V1-START-DATE       PICTURE XXXX/XX/XX.
012820000000           03  FILLER                  PICTURE X(1).
012830000000           03  SH01V1-LIT2             PICTURE X(2).
012840000000           03  FILLER                  PICTURE X(1).
012850980722           03  SH01V1-END-DATE         PICTURE XXXX/XX/XX.
012860980722           03  FILLER                  PICTURE X(24).
012870000000
012880000000       01  STMT-HDG-01-V2-R            REDEFINES STMT-HDG-01.
012890000000           03  FILLER                  PICTURE X(2).
012900000000           03  SH01V2-ACCOUNT-TYPE     PICTURE X(75).
012910000000           03  FILLER                  PICTURE X(4).
012920000000           03  SH01V2-LIT1             PICTURE X(20).
012930000000           03  FILLER                  PICTURE X(1).
012940980722           03  SH01V2-START-DATE       PICTURE XXXX/XX/XX.
012950000000           03  FILLER                  PICTURE X(1).
012960000000           03  SH01V2-LIT2             PICTURE X(2).
012970000000           03  FILLER                  PICTURE X(1).
012980980722           03  SH01V2-END-DATE         PICTURE XXXX/XX/XX.
012990980722           03  FILLER                  PICTURE X(24).
013000000000
013010000000       01  STMT-HDG-01-V3-R            REDEFINES STMT-HDG-01.
013020000000           03  FILLER                  PICTURE X(2).
013030000000           03  SH01V3-ACCOUNT-TYPE     PICTURE X(75).
013040000000           03  FILLER                  PICTURE X(4).
013050000000           03  SH01V3-LIT1             PICTURE X(20).
013060000000           03  FILLER                  PICTURE X(1).
013070980722           03  SH01V3-START-DATE       PICTURE XXXX/XX/XX.
013080000000           03  FILLER                  PICTURE X(1).
013090000000           03  SH01V3-LIT2             PICTURE X(2).
013100000000           03  FILLER                  PICTURE X(1).
013110980722           03  SH01V3-END-DATE         PICTURE XXXX/XX/XX.
013120980722           03  FILLER                  PICTURE X(24).
013130000000
013140000000       01  STMT-HDG-01-V4-R            REDEFINES STMT-HDG-01.
013150000000           03  FILLER                  PICTURE X(2).
013160000000           03  SH01V4-ACCOUNT-TYPE     PICTURE X(75).
013170000000           03  FILLER                  PICTURE X(4).
013180000000           03  SH01V4-LIT1             PICTURE X(20).
013190000000           03  FILLER                  PICTURE X(1).
013200980722           03  SH01V4-START-DATE       PICTURE XXXX/XX/XX.
013210000000           03  FILLER                  PICTURE X(1).
013220000000           03  SH01V4-LIT2             PICTURE X(2).
013230000000           03  FILLER                  PICTURE X(1).
013240980722           03  SH01V4-END-DATE         PICTURE XXXX/XX/XX.
013250980722           03  FILLER                  PICTURE X(24).
013260000000
013270000000       01  STMT-HDG-01-V5-R            REDEFINES STMT-HDG-01.
013280000000           03  FILLER                  PICTURE X(2).
013290000000           03  SH01V5-ACCOUNT-TYPE     PICTURE X(75).
013300000000           03  FILLER                  PICTURE X(4).
013310000000           03  SH01V5-LIT1             PICTURE X(20).
013320000000           03  FILLER                  PICTURE X(1).
013330980722           03  SH01V5-START-DATE       PICTURE XXXX/XX/XX.
013340000000           03  FILLER                  PICTURE X(1).
013350000000           03  SH01V5-LIT2             PICTURE X(2).
013360000000           03  FILLER                  PICTURE X(1).
013370980722           03  SH01V5-END-DATE         PICTURE XXXX/XX/XX.
013380980722           03  FILLER                  PICTURE X(24).
013390990922
013400990922       01  STMT-HDG-01-V6-R            REDEFINES STMT-HDG-01.
013410000223           03  FILLER                  PICTURE X(8).
013420991208           03  SH01V6-INVESTOR-NAME    PICTURE X(47).
013430000223           03  FILLER                  PICTURE X(10).
013440990922           03  SH01V6-LIT1             PICTURE X(20).
013450990922           03  FILLER                  PICTURE X(1).
013460990922           03  SH01V6-START-DATE       PICTURE XXXX/XX/XX.
013470990922           03  FILLER                  PICTURE X(1).
013480990922           03  SH01V6-LIT2             PICTURE X(2).
013490990922           03  FILLER                  PICTURE X(1).
013500990922           03  SH01V6-END-DATE         PICTURE XXXX/XX/XX.
013510000223           03  FILLER                  PICTURE X(8).
013520991019
013530991019       01  STMT-HDG-01-V7-R            REDEFINES STMT-HDG-01.
013540991019           03  FILLER                  PICTURE X(2).
013550991019           03  SH01V7-ACCOUNT-TYPE     PICTURE X(75).
013560991019           03  FILLER                  PICTURE X(4).
013570991019           03  SH01V7-LIT1             PICTURE X(20).
013580991019           03  FILLER                  PICTURE X(1).
013590991019           03  SH01V7-START-DATE       PICTURE XXXX/XX/XX.
013600991019           03  FILLER                  PICTURE X(1).
013610991019           03  SH01V7-LIT2             PICTURE X(2).
013620991019           03  FILLER                  PICTURE X(1).
013630991019           03  SH01V7-END-DATE         PICTURE XXXX/XX/XX.
013640991019           03  FILLER                  PICTURE X(24).
013650991213
013660991213       01  STMT-HDG-01-V8-R            REDEFINES STMT-HDG-01.
013670991213           03  FILLER                  PICTURE X(2).
013680991213           03  SH01V8-ACCOUNT-TYPE     PICTURE X(75).
013690991213           03  FILLER                  PICTURE X(4).
013700991213           03  SH01V8-LIT1             PICTURE X(20).
013710991213           03  FILLER                  PICTURE X(1).
013720991213           03  SH01V8-START-DATE       PICTURE XXXX/XX/XX.
013730991213           03  FILLER                  PICTURE X(1).
013740991213           03  SH01V8-LIT2             PICTURE X(2).
013750991213           03  FILLER                  PICTURE X(1).
013760991213           03  SH01V8-END-DATE         PICTURE XXXX/XX/XX.
013770991213           03  FILLER                  PICTURE X(24).
013780991213
013790000000      /
013800000000       01  STMT-HDG-02.
013810000000           03  FILLER                  PICTURE X(6).
013820000000           03  SH02-ACCOUNT            PICTURE ZZZZZZZZ9.
013830000000           03  FILLER                  PICTURE X(12).
013840000000           03  SH02-SIN-NO             PICTURE X(11).
013850000000           03  FILLER                  PICTURE X(6).
013860000000           03  SH02-DEALER             PICTURE X(4).
013870000000           03  SH02-LITDSH             PICTURE X(1).
013880000222           03  SH02-SALESREP           PICTURE X(6).
013890050104      * RFS25032 - Begins
013900050104      *    03  FILLER                  PICTURE X(3).
013910050104           03  FILLER                  PICTURE X(2).
013920050104      * RFS25032 - Ends
013930980722           03  SH02-END-DATE           PICTURE XXXX/XX/XX.
013940050104      * RFS25032 - Begins
013950050104      *    03  FILLER                  PICTURE X(4).
013960050104           03  FILLER                  PICTURE X(5).
013970050104      * RFS25032 - Ends
013980000000           03  SH02-SPOUSAL-CONTRIB    PICTURE X(40).
013990050104      * RFS25032 - Begins
014000050104      *    03  FILLER                  PICTURE X(5).
014010050104           03  FILLER                  PICTURE X(1).
014020050104      * RFS25032 - Ends
014030000000           03  SH02-REG-CTB-CUR        PICTURE $$$$,$$$,$$$.99.
014040000000           03  SH02-REG-CTB-CUR-SIGN   PICTURE X(1).
014050000000           03  FILLER                  PICTURE X(1).
014060000000           03  SH02-PGENO              PICTURE ZZ9.
014070000000           03  FILLER                  PICTURE X(13).
014080000000
014090000000       01  STMT-HDG-02-V1-R            REDEFINES STMT-HDG-02.
014100000000           03  FILLER                  PICTURE X(5).
014110000000           03  SH02V1-ACCOUNT          PICTURE ZZZZZZZZ9.
014120000000           03  FILLER                  PICTURE X(11).
014130000000           03  SH02V1-SIN-NO           PICTURE X(11).
014140000000           03  FILLER                  PICTURE X(6).
014150000000           03  SH02V1-DEALER           PICTURE X(4).
014160000000           03  SH02V1-LITDSH           PICTURE X(1).
014170000222           03  SH02V1-SALESREP         PICTURE X(6).
014180000222           03  FILLER                  PICTURE X(3).
014190980722           03  SH02V1-END-DATE         PICTURE XXXX/XX/XX.
014200000000           03  FILLER                  PICTURE X(4).
014210000000           03  SH02V1-SPOUSAL-CONTRIB  PICTURE X(40).
014220000000           03  FILLER                  PICTURE X(4).
014230000000           03  SH02V1-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
014240000000           03  SH02V1-REG-CTB-CUR-SIGN PICTURE X(1).
014250000000           03  FILLER                  PICTURE X(1).
014260000000           03  SH02V1-PGENO            PICTURE ZZ9.
014270000000           03  FILLER                  PICTURE X(16).
014280000000
014290990629       01  STMT-HDG-022-V1-R           REDEFINES STMT-HDG-02.
014300990629           03  FILLER                  PICTURE X(14).
014310990629           03  SH022V1-ACCOUNT         PICTURE ZZZZZZZZ9.
014320990629           03  FILLER                  PICTURE X(19).
014330990629           03  SH022V1-DEALER          PICTURE X(4).
014340990629           03  SH022V1-LITDSH          PICTURE X(1).
014350000222           03  SH022V1-SALESREP        PICTURE X(6).
014360000222           03  FILLER                  PICTURE X(3).
014370990629           03  SH022V1-END-DATE        PICTURE XXXX/XX/XX.
014380990629           03  FILLER                  PICTURE X(4).
014390990629           03  SH022V1-SPOUSAL-CONTRIB PICTURE X(40).
014400990629           03  FILLER                  PICTURE X(4).
014410990629           03  SH022V1-REG-CTB-CUR     PICTURE $$$$,$$$,$$$.99.
014420990629           03  SH022V1-REG-CTB-CUR-SIGN PICTURE X(1).
014430990629           03  FILLER                  PICTURE X(1).
014440990629           03  SH022V1-PGENO           PICTURE ZZ9.
014450990629           03  FILLER                  PICTURE X(16).
014460990629
014470000000       01  STMT-HDG-02-V2-R            REDEFINES STMT-HDG-02.
014480990226           03  FILLER                  PICTURE X(5).
014490000000           03  SH02V2-ACCOUNT          PICTURE ZZZZZZZZ9.
014500000000           03  FILLER                  PICTURE X(11).
014510990226           03  SH02V2-SIN-NO           PICTURE X(11).
014520990226           03  FILLER                  PICTURE X(6).
014530000000           03  SH02V2-DEALER           PICTURE X(4).
014540000000           03  SH02V2-LITDSH           PICTURE X(1).
014550000222           03  SH02V2-SALESREP         PICTURE X(6).
014560000222           03  FILLER                  PICTURE X(3).
014570980722           03  SH02V2-END-DATE         PICTURE XXXX/XX/XX.
014580990226           03  FILLER                  PICTURE X(4).
014590990226           03  SH02V2-SPOUSAL-CONTRIB  PICTURE X(40).
014600990226           03  FILLER                  PICTURE X(4).
014610000000           03  SH02V2-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99
014620000000                                               BLANK WHEN ZERO.
014630000000           03  SH02V2-REG-CTB-CUR-SIGN PICTURE X(1).
014640990302           03  FILLER                  PICTURE X(3).
014650000000           03  SH02V2-PGENO            PICTURE ZZ9.
014660990302           03  FILLER                  PICTURE X(14).
014670000000
014680000000       01  STMT-HDG-02-V3-R            REDEFINES STMT-HDG-02.
014690000000           03  FILLER                  PICTURE X(6).
014700000000           03  SH02V3-ACCOUNT          PICTURE ZZZZZZZZ9.
014710000000           03  FILLER                  PICTURE X(12).
014720000000           03  SH02V3-SIN-NO           PICTURE X(11).
014730000000           03  FILLER                  PICTURE X(6).
014740000000           03  SH02V3-DEALER           PICTURE X(4).
014750000000           03  SH02V3-LITDSH           PICTURE X(1).
014760000222           03  SH02V3-SALESREP         PICTURE X(6).
014770000222           03  FILLER                  PICTURE X(3).
014780980722           03  SH02V3-END-DATE         PICTURE XXXX/XX/XX.
014790000000           03  FILLER                  PICTURE X(4).
014800000000           03  SH02V3-SPOUSAL-CONTRIB  PICTURE X(40).
014810000000           03  FILLER                  PICTURE X(4).
014820000000           03  SH02V3-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
014830000000           03  SH02V3-REG-CTB-CUR-SIGN PICTURE X(1).
014840000000           03  FILLER                  PICTURE X(1).
014850000000           03  SH02V3-PGENO            PICTURE ZZ9.
014860000000           03  FILLER                  PICTURE X(14).
014870000000
014880000000       01  STMT-HDG-02-V4-R            REDEFINES STMT-HDG-02.
014890000000           03  FILLER                  PICTURE X(5).
014900000000           03  SH02V4-ACCOUNT          PICTURE ZZZZZZZZ9.
014910000000           03  FILLER                  PICTURE X(11).
014920000000           03  SH02V4-SIN-NO           PICTURE X(11).
014930000000           03  FILLER                  PICTURE X(6).
014940000000           03  SH02V4-DEALER           PICTURE X(4).
014950000000           03  SH02V4-LITDSH           PICTURE X(1).
014960000222           03  SH02V4-SALESREP         PICTURE X(6).
014970000222           03  FILLER                  PICTURE X(3).
014980980722           03  SH02V4-END-DATE         PICTURE XXXX/XX/XX.
014990000000           03  FILLER                  PICTURE X(4).
015000000000           03  SH02V4-SPOUSAL-CONTRIB  PICTURE X(40).
015010000000           03  FILLER                  PICTURE X(4).
015020000000           03  SH02V4-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
015030000000           03  SH02V4-REG-CTB-CUR-SIGN PICTURE X(1).
015040000000           03  FILLER                  PICTURE X(1).
015050000000           03  SH02V4-PGENO            PICTURE ZZ9.
015060000000           03  FILLER                  PICTURE X(16).
015070000000
015080000000       01  STMT-HDG-02-V5-R            REDEFINES STMT-HDG-02.
015090000000           03  FILLER                  PICTURE X(5).
015100000000           03  SH02V5-ACCOUNT          PICTURE ZZZZZZZZ9.
015110000000           03  FILLER                  PICTURE X(11).
015120000000           03  SH02V5-SIN-NO           PICTURE X(11).
015130000000           03  FILLER                  PICTURE X(6).
015140000000           03  SH02V5-DEALER           PICTURE X(4).
015150000000           03  SH02V5-LITDSH           PICTURE X(1).
015160000222           03  SH02V5-SALESREP         PICTURE X(6).
015170000222           03  FILLER                  PICTURE X(3).
015180980722           03  SH02V5-END-DATE         PICTURE XXXX/XX/XX.
015190000000           03  FILLER                  PICTURE X(4).
015200000000           03  SH02V5-SPOUSAL-CONTRIB  PICTURE X(40).
015210000000           03  FILLER                  PICTURE X(4).
015220000000           03  SH02V5-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
015230000000           03  SH02V5-REG-CTB-CUR-SIGN PICTURE X(1).
015240000000           03  FILLER                  PICTURE X(1).
015250000000           03  SH02V5-PGENO            PICTURE ZZ9.
015260000000           03  FILLER                  PICTURE X(16).
015270990922
015280990922       01  STMT-HDG-02-V6-R            REDEFINES STMT-HDG-02.
015290000223           03  FILLER                  PICTURE X(8).
015300990922           03  SH02V6-ACCOUNT          PICTURE ZZZZZZZZ9.
015310991208           03  FILLER                  PICTURE X(1).
015320990922           03  SH02V6-SIN-NO           PICTURE X(11).
015330991208           03  FILLER                  PICTURE X(1).
015340990922           03  SH02V6-DEALER           PICTURE X(4).
015350990922           03  SH02V6-LITDSH           PICTURE X(1).
015360000222           03  SH02V6-SALESREP         PICTURE X(6).
015370991208           03  FILLER                  PICTURE X(1).
015380990922           03  SH02V6-END-DATE         PICTURE XXXX/XX/XX.
015390991208           03  FILLER                  PICTURE X(1).
015400990922           03  SH02V6-SPOUSAL-CONTRIB  PICTURE X(40).
015410000223           03  FILLER                  PICTURE X(5).
015420990922           03  SH02V6-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
015430990922           03  SH02V6-REG-CTB-CUR-SIGN PICTURE X(1).
015440990922           03  FILLER                  PICTURE X(1).
015450990927           03  SH02V6-PGENO            PICTURE ZZ9.
015460000222           03  FILLER                  PICTURE X(1).
015470991019
015480991019       01  STMT-HDG-02-V7-R            REDEFINES STMT-HDG-02.
015490991021           03  FILLER                  PICTURE X(5).
015500991019           03  SH02V7-ACCOUNT          PICTURE ZZZZZZZZ9.
015510991021           03  FILLER                  PICTURE X(11).
015520991019           03  SH02V7-SIN-NO           PICTURE X(11).
015530991019           03  FILLER                  PICTURE X(6).
015540991019           03  SH02V7-DEALER           PICTURE X(4).
015550991019           03  SH02V7-LITDSH           PICTURE X(1).
015560000222           03  SH02V7-SALESREP         PICTURE X(6).
015570000222           03  FILLER                  PICTURE X(3).
015580991019           03  SH02V7-END-DATE         PICTURE XXXX/XX/XX.
015590991021           03  FILLER                  PICTURE X(2).
015600991019           03  SH02V7-SPOUSAL-CONTRIB  PICTURE X(40).
015610991021           03  FILLER                  PICTURE X(1).
015620991019           03  SH02V7-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
015630991019           03  SH02V7-REG-CTB-CUR-SIGN PICTURE X(1).
015640991019           03  FILLER                  PICTURE X(1).
015650991019           03  SH02V7-PGENO            PICTURE ZZ9.
015660991021           03  FILLER                  PICTURE X(21).
015670991022
015680991022       01  STMT-HDG-022-V7-R           REDEFINES STMT-HDG-02.
015690991022           03  FILLER                  PICTURE X(14).
015700991022           03  SH022V7-ACCOUNT         PICTURE ZZZZZZZZ9.
015710991022           03  FILLER                  PICTURE X(19).
015720991022           03  SH022V7-DEALER          PICTURE X(4).
015730991022           03  SH022V7-LITDSH          PICTURE X(1).
015740000222           03  SH022V7-SALESREP        PICTURE X(6).
015750000222           03  FILLER                  PICTURE X(3).
015760991022           03  SH022V7-END-DATE        PICTURE XXXX/XX/XX.
015770991022           03  FILLER                  PICTURE X(2).
015780991022           03  SH022V7-SPOUSAL-CONTRIB PICTURE X(40).
015790991022           03  FILLER                  PICTURE X(1).
015800991022           03  SH022V7-REG-CTB-CUR     PICTURE $$$$,$$$,$$$.99.
015810991022           03  SH022V7-REG-CTB-CUR-SIGN PICTURE X(1).
015820991022           03  FILLER                  PICTURE X(1).
015830991022           03  SH022V7-PGENO           PICTURE ZZ9.
015840991022           03  FILLER                  PICTURE X(21).
015850991213
015860991213       01  STMT-HDG-02-V8-R            REDEFINES STMT-HDG-02.
015870991213           03  FILLER                  PICTURE X(5).
015880991213           03  SH02V8-ACCOUNT          PICTURE ZZZZZZZZ9.
015890991213           03  FILLER                  PICTURE X(11).
015900991213           03  SH02V8-SIN-NO           PICTURE X(11).
015910991213           03  FILLER                  PICTURE X(6).
015920991213           03  SH02V8-DEALER           PICTURE X(4).
015930991213           03  SH02V8-LITDSH           PICTURE X(1).
015940000222           03  SH02V8-SALESREP         PICTURE X(6).
015950000222           03  FILLER                  PICTURE X(3).
015960991213           03  SH02V8-END-DATE         PICTURE XXXX/XX/XX.
015970991213           03  FILLER                  PICTURE X(4).
015980991213           03  SH02V8-SPOUSAL-CONTRIB  PICTURE X(40).
015990991213           03  FILLER                  PICTURE X(4).
016000991213           03  SH02V8-REG-CTB-CUR      PICTURE $$$$,$$$,$$$.99.
016010991213           03  SH02V8-REG-CTB-CUR-SIGN PICTURE X(1).
016020991213           03  FILLER                  PICTURE X(1).
016030991213           03  SH02V8-PGENO            PICTURE ZZ9.
016040991213           03  FILLER                  PICTURE X(16).
016050991213
016060991213       01  STMT-HDG-022-V8-R           REDEFINES STMT-HDG-02.
016070991213           03  FILLER                  PICTURE X(14).
016080991213           03  SH022V8-ACCOUNT         PICTURE ZZZZZZZZ9.
016090991213           03  FILLER                  PICTURE X(19).
016100991213           03  SH022V8-DEALER          PICTURE X(4).
016110991213           03  SH022V8-LITDSH          PICTURE X(1).
016120000222           03  SH022V8-SALESREP        PICTURE X(6).
016130000222           03  FILLER                  PICTURE X(3).
016140991213           03  SH022V8-END-DATE        PICTURE XXXX/XX/XX.
016150991213           03  FILLER                  PICTURE X(4).
016160991213           03  SH022V8-SPOUSAL-CONTRIB PICTURE X(40).
016170991213           03  FILLER                  PICTURE X(4).
016180991213           03  SH022V8-REG-CTB-CUR     PICTURE $$$$,$$$,$$$.99.
016190991213           03  SH022V8-REG-CTB-CUR-SIGN PICTURE X(1).
016200991213           03  FILLER                  PICTURE X(1).
016210991213           03  SH022V8-PGENO           PICTURE ZZ9.
016220991213           03  FILLER                  PICTURE X(16).
016230991022
016240000000      /
016250000000       01  STMT-HDG-03.
016260000000           03  FILLER                  PICTURE X(8).
016270000000           03  SH03-INVESTOR-NAME      PICTURE X(45).
016280000000           03  FILLER                  PICTURE X(34).
016290000000           03  SH03-DEALER-REP-NAMES   PICTURE X(51).
016300000000           03  FILLER                  PICTURE X(12).
016310000000
016320000000       01  STMT-HDG-03-V1-R            REDEFINES STMT-HDG-03.
016330000000           03  FILLER                  PICTURE X(8).
016340000000           03  SH03V1-INVESTOR-NAME    PICTURE X(45).
016350000000           03  FILLER                  PICTURE X(30).
016360000000           03  SH03V1-DEALER-REP-NAMES PICTURE X(51).
016370000000           03  FILLER                  PICTURE X(16).
016380000000
016390000000       01  STMT-HDG-03-V2-R            REDEFINES STMT-HDG-03.
016400000000           03  FILLER                  PICTURE X(8).
016410000000           03  SH03V2-INVESTOR-NAME    PICTURE X(45).
016420990226           03  FILLER                  PICTURE X(30).
016430000000           03  SH03V2-DEALER-REP-NAMES PICTURE X(51).
016440990226           03  FILLER                  PICTURE X(16).
016450000000
016460000000       01  STMT-HDG-03-V3-R            REDEFINES STMT-HDG-03.
016470000000           03  FILLER                  PICTURE X(8).
016480000000           03  SH03V3-INVESTOR-NAME    PICTURE X(45).
016490000000           03  FILLER                  PICTURE X(32).
016500000000           03  SH03V3-DEALER-REP-NAMES PICTURE X(51).
016510000000           03  FILLER                  PICTURE X(14).
016520000000
016530000000       01  STMT-HDG-03-V4-R            REDEFINES STMT-HDG-03.
016540000000           03  FILLER                  PICTURE X(7).
016550000000           03  SH03V4-INVESTOR-NAME    PICTURE X(45).
016560000000           03  FILLER                  PICTURE X(32).
016570000000           03  SH03V4-DEALER-REP-NAMES PICTURE X(51).
016580000000           03  FILLER                  PICTURE X(15).
016590000000
016600000000       01  STMT-HDG-03-V5-R            REDEFINES STMT-HDG-03.
016610000000           03  FILLER                  PICTURE X(7).
016620000000           03  SH03V5-INVESTOR-NAME    PICTURE X(45).
016630000000           03  FILLER                  PICTURE X(32).
016640000000           03  SH03V5-DEALER-REP-NAMES PICTURE X(51).
016650000000           03  FILLER                  PICTURE X(15).
016660990922
016670990922       01  STMT-HDG-03-V6-R            REDEFINES STMT-HDG-03.
016680000223           03  FILLER                  PICTURE X(8).
016690991208           03  SH03V6-INVESTOR-ADDRESS1 PICTURE X(57).
016700000223           03  FILLER                  PICTURE X(35).
016710990927           03  SH03V6-DEALER-REP-NAMES PICTURE X(6).
016720990927           03  SH03V6-PGENO            PICTURE ZZ9.
016730000223           03  FILLER                  PICTURE X(9).
016740991019
016750991019       01  STMT-HDG-03-V7-R            REDEFINES STMT-HDG-03.
016760991019           03  FILLER                  PICTURE X(8).
016770991019           03  SH03V7-INVESTOR-NAME    PICTURE X(45).
016780991021           03  FILLER                  PICTURE X(25).
016790991019           03  SH03V7-DEALER-REP-NAMES PICTURE X(51).
016800991021           03  FILLER                  PICTURE X(21).
016810991213
016820991213       01  STMT-HDG-03-V8-R            REDEFINES STMT-HDG-03.
016830991213           03  FILLER                  PICTURE X(8).
016840991213           03  SH03V8-INVESTOR-NAME    PICTURE X(45).
016850991213           03  FILLER                  PICTURE X(30).
016860991213           03  SH03V8-DEALER-REP-NAMES PICTURE X(51).
016870991213           03  FILLER                  PICTURE X(16).
016880991213
016890000000      /
016900000000       01  STMT-HDG-04.
016910000000           03  FILLER                  PICTURE X(8).
016920000000           03  SH04-INVESTOR-ADDRESS1  PICTURE X(40).
016930000000           03  FILLER                  PICTURE X(39).
016940000000           03  SH04-BRANCH-ADDR1       PICTURE X(40).
016950000000           03  FILLER                  PICTURE X(23).
016960000000
016970000000       01  STMT-HDG-04-V1-R            REDEFINES STMT-HDG-04.
016980000000           03  FILLER                  PICTURE X(8).
016990000000           03  SH04V1-INVESTOR-ADDRESS1
017000000000                                       PICTURE X(40).
017010000000           03  FILLER                  PICTURE X(35).
017020000000           03  SH04V1-BRANCH-ADDR1     PICTURE X(40).
017030000000           03  FILLER                  PICTURE X(27).
017040000000
017050000000       01  STMT-HDG-04-V2-R            REDEFINES STMT-HDG-04.
017060000000           03  FILLER                  PICTURE X(8).
017070000000           03  SH04V2-INVESTOR-ADDRESS1
017080000000                                       PICTURE X(40).
017090990226           03  FILLER                  PICTURE X(35).
017100000000           03  SH04V2-BRANCH-ADDR1     PICTURE X(40).
017110990226           03  FILLER                  PICTURE X(27).
017120000000
017130000000       01  STMT-HDG-04-V3-R            REDEFINES STMT-HDG-04.
017140000000           03  FILLER                  PICTURE X(8).
017150000000           03  SH04V3-INVESTOR-ADDRESS1
017160000000                                       PICTURE X(40).
017170000000           03  FILLER                  PICTURE X(37).
017180000000           03  SH04V3-BRANCH-ADDR1     PICTURE X(40).
017190000000           03  FILLER                  PICTURE X(25).
017200000000
017210000000       01  STMT-HDG-04-V4-R            REDEFINES STMT-HDG-04.
017220000000           03  FILLER                  PICTURE X(7).
017230000000           03  SH04V4-INVESTOR-ADDRESS1
017240000000                                       PICTURE X(40).
017250000000           03  FILLER                  PICTURE X(37).
017260000000           03  SH04V4-BRANCH-ADDR1     PICTURE X(40).
017270000000           03  FILLER                  PICTURE X(26).
017280000000
017290000000       01  STMT-HDG-04-V5-R            REDEFINES STMT-HDG-04.
017300000000           03  FILLER                  PICTURE X(7).
017310000000           03  SH04V5-INVESTOR-ADDRESS1
017320000000                                       PICTURE X(40).
017330000000           03  FILLER                  PICTURE X(37).
017340000000           03  SH04V5-BRANCH-ADDR1     PICTURE X(40).
017350000000           03  FILLER                  PICTURE X(26).
017360990922
017370990922       01  STMT-HDG-04-V6-R            REDEFINES STMT-HDG-04.
017380000223           03  FILLER                  PICTURE X(8).
017390990923           03  SH04V6-INVESTOR-ADDRESS2
017400990923                                       PICTURE X(45).
017410000222           03  FILLER                  PICTURE X(21).
017420000223           03  SH04V6-BRANCH-ADDR1     PICTURE X(44).
017430991019
017440991019       01  STMT-HDG-04-V7-R            REDEFINES STMT-HDG-04.
017450991019           03  FILLER                  PICTURE X(8).
017460991019           03  SH04V7-INVESTOR-ADDRESS1  PICTURE X(40).
017470991021           03  FILLER                  PICTURE X(30).
017480991019           03  SH04V7-BRANCH-ADDR1     PICTURE X(40).
017490991021           03  FILLER                  PICTURE X(32).
017500991213
017510991213       01  STMT-HDG-04-V8-R            REDEFINES STMT-HDG-04.
017520991213           03  FILLER                  PICTURE X(8).
017530991213           03  SH04V8-INVESTOR-ADDRESS1
017540991213                                       PICTURE X(40).
017550991213           03  FILLER                  PICTURE X(35).
017560991213           03  SH04V8-BRANCH-ADDR1     PICTURE X(40).
017570991213           03  FILLER                  PICTURE X(27).
017580991213
017590000000      /
017600000000       01  STMT-HDG-05.
017610000000           03  FILLER                  PICTURE X(8).
017620000000           03  SH05-INVESTOR-ADDRESS2  PICTURE X(40).
017630000000           03  FILLER                  PICTURE X(39).
017640000000           03  SH05-BRANCH-ADDR2       PICTURE X(40).
017650000000           03  FILLER                  PICTURE X(23).
017660000000
017670000000       01  STMT-HDG-05-V1-R            REDEFINES STMT-HDG-05.
017680000000           03  FILLER                  PICTURE X(8).
017690000000           03  SH05V1-INVESTOR-ADDRESS2
017700000000                                       PICTURE X(40).
017710000000           03  FILLER                  PICTURE X(35).
017720000000           03  SH05V1-BRANCH-ADDR2     PICTURE X(40).
017730000000           03  FILLER                  PICTURE X(27).
017740000000
017750000000       01  STMT-HDG-05-V2-R            REDEFINES STMT-HDG-05.
017760000000           03  FILLER                  PICTURE X(8).
017770000000           03  SH05V2-INVESTOR-ADDRESS2
017780000000                                       PICTURE X(40).
017790990226           03  FILLER                  PICTURE X(35).
017800000000           03  SH05V2-BRANCH-ADDR2     PICTURE X(40).
017810990226           03  FILLER                  PICTURE X(27).
017820000000
017830000000       01  STMT-HDG-05-V3-R            REDEFINES STMT-HDG-05.
017840000000           03  FILLER                  PICTURE X(8).
017850000000           03  SH05V3-INVESTOR-ADDRESS2
017860000000                                       PICTURE X(40).
017870000000           03  FILLER                  PICTURE X(37).
017880000000           03  SH05V3-BRANCH-ADDR2     PICTURE X(40).
017890000000           03  FILLER                  PICTURE X(25).
017900000000
017910000000       01  STMT-HDG-05-V4-R            REDEFINES STMT-HDG-05.
017920000000           03  FILLER                  PICTURE X(7).
017930000000           03  SH05V4-INVESTOR-ADDRESS2
017940000000                                       PICTURE X(40).
017950000000           03  FILLER                  PICTURE X(37).
017960000000           03  SH05V4-BRANCH-ADDR2     PICTURE X(40).
017970000000           03  FILLER                  PICTURE X(26).
017980000000
017990000000       01  STMT-HDG-05-V5-R            REDEFINES STMT-HDG-05.
018000000000           03  FILLER                  PICTURE X(7).
018010000000           03  SH05V5-INVESTOR-ADDRESS2
018020000000                                       PICTURE X(40).
018030000000           03  FILLER                  PICTURE X(37).
018040000000           03  SH05V5-BRANCH-ADDR2     PICTURE X(40).
018050000000           03  FILLER                  PICTURE X(26).
018060990922
018070990922       01  STMT-HDG-05-V6-R            REDEFINES STMT-HDG-05.
018080000223           03  FILLER                  PICTURE X(8).
018090990923           03  SH05V6-INVESTOR-ADDRESS3
018100990922                                       PICTURE X(40).
018110000222           03  FILLER                  PICTURE X(31).
018120000223           03  SH05V6-BRANCH-ADDR2     PICTURE X(39).
018130991019
018140991019       01  STMT-HDG-05-V7-R            REDEFINES STMT-HDG-05.
018150991019           03  FILLER                  PICTURE X(8).
018160991019           03  SH05V7-INVESTOR-ADDRESS2  PICTURE X(40).
018170991021           03  FILLER                  PICTURE X(30).
018180991019           03  SH05V7-BRANCH-ADDR2     PICTURE X(40).
018190991021           03  FILLER                  PICTURE X(32).
018200991213
018210991213       01  STMT-HDG-05-V8-R            REDEFINES STMT-HDG-05.
018220991213           03  FILLER                  PICTURE X(8).
018230991213           03  SH05V8-INVESTOR-ADDRESS2
018240991213                                       PICTURE X(40).
018250991213           03  FILLER                  PICTURE X(35).
018260991213           03  SH05V8-BRANCH-ADDR2     PICTURE X(40).
018270991213           03  FILLER                  PICTURE X(27).
018280991213
018290000000      /
018300000000       01  STMT-HDG-06.
018310000000           03  FILLER                  PICTURE X(8).
018320000000           03  SH06-INVESTOR-ADDRESS3  PICTURE X(40).
018330000000           03  FILLER                  PICTURE X(39).
018340000000           03  SH06-BRANCH-ADDR3       PICTURE X(40).
018350000000           03  FILLER                  PICTURE X(23).
018360000000
018370000000       01  STMT-HDG-06-V1-R            REDEFINES STMT-HDG-06.
018380000000           03  FILLER                  PICTURE X(8).
018390000000           03  SH06V1-INVESTOR-ADDRESS3
018400000000                                       PICTURE X(40).
018410000000           03  FILLER                  PICTURE X(35).
018420000000           03  SH06V1-BRANCH-ADDR3     PICTURE X(40).
018430000000           03  FILLER                  PICTURE X(27).
018440000000
018450000000       01  STMT-HDG-06-V2-R            REDEFINES STMT-HDG-06.
018460000000           03  FILLER                  PICTURE X(8).
018470000000           03  SH06V2-INVESTOR-ADDRESS3
018480000000                                       PICTURE X(40).
018490990226           03  FILLER                  PICTURE X(35).
018500000000           03  SH06V2-BRANCH-ADDR3     PICTURE X(40).
018510990226           03  FILLER                  PICTURE X(27).
018520000000
018530000000       01  STMT-HDG-06-V3-R            REDEFINES STMT-HDG-06.
018540000000           03  FILLER                  PICTURE X(8).
018550000000           03  SH06V3-INVESTOR-ADDRESS3
018560000000                                       PICTURE X(40).
018570000000           03  FILLER                  PICTURE X(37).
018580000000           03  SH06V3-BRANCH-ADDR3     PICTURE X(40).
018590000000           03  FILLER                  PICTURE X(25).
018600000000
018610000000       01  STMT-HDG-06-V4-R            REDEFINES STMT-HDG-06.
018620000000           03  FILLER                  PICTURE X(7).
018630000000           03  SH06V4-INVESTOR-ADDRESS3
018640000000                                       PICTURE X(40).
018650000000           03  FILLER                  PICTURE X(37).
018660000000           03  SH06V4-BRANCH-ADDR3     PICTURE X(40).
018670000000           03  FILLER                  PICTURE X(26).
018680000000
018690000000       01  STMT-HDG-06-V5-R            REDEFINES STMT-HDG-06.
018700000000           03  FILLER                  PICTURE X(7).
018710000000           03  SH06V5-INVESTOR-ADDRESS3
018720000000                                       PICTURE X(40).
018730000000           03  FILLER                  PICTURE X(37).
018740000000           03  SH06V5-BRANCH-ADDR3     PICTURE X(40).
018750000000           03  FILLER                  PICTURE X(26).
018760990922
018770990922       01  STMT-HDG-06-V6-R            REDEFINES STMT-HDG-06.
018780000223           03  FILLER                  PICTURE X(8).
018790990923           03  SH06V6-INVESTOR-ADDRESS4
018800990923                                       PICTURE X(40).
018810000222           03  FILLER                  PICTURE X(31).
018820000223           03  SH06V6-BRANCH-ADDR3     PICTURE X(39).
018830991019
018840991019       01  STMT-HDG-06-V7-R            REDEFINES STMT-HDG-06.
018850991019           03  FILLER                  PICTURE X(8).
018860991019           03  SH06V7-INVESTOR-ADDRESS3  PICTURE X(40).
018870991021           03  FILLER                  PICTURE X(30).
018880991019           03  SH06V7-BRANCH-ADDR3     PICTURE X(40).
018890991021           03  FILLER                  PICTURE X(32).
018900991213
018910991213       01  STMT-HDG-06-V8-R            REDEFINES STMT-HDG-06.
018920991213           03  FILLER                  PICTURE X(8).
018930991213           03  SH06V8-INVESTOR-ADDRESS3
018940991213                                       PICTURE X(40).
018950991213           03  FILLER                  PICTURE X(35).
018960991213           03  SH06V8-BRANCH-ADDR3     PICTURE X(40).
018970991213           03  FILLER                  PICTURE X(27).
018980000000      /
018990000000       01  STMT-HDG-07.
019000000000           03  FILLER                  PICTURE X(8).
019010000000           03  SH07-INVESTOR-ADDRESS4  PICTURE X(40).
019020000000           03  FILLER                  PICTURE X(39).
019030000000           03  SH07-BRANCH-ADDR4       PICTURE X(40).
019040000000           03  FILLER                  PICTURE X(23).
019050000000
019060000000       01  STMT-HDG-07-V1-R            REDEFINES STMT-HDG-07.
019070000000           03  FILLER                  PICTURE X(8).
019080000000           03  SH07V1-INVESTOR-ADDRESS4
019090000000                                       PICTURE X(40).
019100000000           03  FILLER                  PICTURE X(35).
019110000000           03  SH07V1-BRANCH-ADDR4     PICTURE X(40).
019120000000           03  FILLER                  PICTURE X(27).
019130000000
019140000000       01  STMT-HDG-07-V2-R            REDEFINES STMT-HDG-07.
019150000000           03  FILLER                  PICTURE X(8).
019160000000           03  SH07V2-INVESTOR-ADDRESS4
019170000000                                       PICTURE X(40).
019180990226           03  FILLER                  PICTURE X(35).
019190000000           03  SH07V2-BRANCH-ADDR4     PICTURE X(40).
019200990226           03  FILLER                  PICTURE X(27).
019210000000
019220000000       01  STMT-HDG-07-V3-R            REDEFINES STMT-HDG-07.
019230000000           03  FILLER                  PICTURE X(8).
019240000000           03  SH07V3-INVESTOR-ADDRESS4
019250000000                                       PICTURE X(40).
019260000000           03  FILLER                  PICTURE X(37).
019270000000           03  SH07V3-BRANCH-ADDR4     PICTURE X(40).
019280000000           03  FILLER                  PICTURE X(25).
019290000000
019300000000       01  STMT-HDG-07-V4-R            REDEFINES STMT-HDG-07.
019310000000           03  FILLER                  PICTURE X(7).
019320000000           03  SH07V4-INVESTOR-ADDRESS4
019330000000                                       PICTURE X(40).
019340000000           03  FILLER                  PICTURE X(37).
019350000000           03  SH07V4-BRANCH-ADDR4     PICTURE X(40).
019360000000           03  FILLER                  PICTURE X(26).
019370000000
019380000000       01  STMT-HDG-07-V5-R            REDEFINES STMT-HDG-07.
019390000000           03  FILLER                  PICTURE X(7).
019400000000           03  SH07V5-INVESTOR-ADDRESS4
019410000000                                       PICTURE X(40).
019420000000           03  FILLER                  PICTURE X(37).
019430000000           03  SH07V5-BRANCH-ADDR4     PICTURE X(40).
019440000000           03  FILLER                  PICTURE X(26).
019450990922
019460990922       01  STMT-HDG-07-V6-R            REDEFINES STMT-HDG-07.
019470000223           03  FILLER                  PICTURE X(8).
019480990923           03  SH07V6-INVESTOR-ADDRESS5
019490990923                                       PICTURE X(40).
019500000222           03  FILLER                  PICTURE X(31).
019510000223           03  SH07V6-BRANCH-ADDR4     PICTURE X(39).
019520991019
019530991019       01  STMT-HDG-07-V7-R            REDEFINES STMT-HDG-07.
019540991019           03  FILLER                  PICTURE X(8).
019550991019           03  SH07V7-INVESTOR-ADDRESS4  PICTURE X(40).
019560991021           03  FILLER                  PICTURE X(30).
019570991019           03  SH07V7-BRANCH-ADDR4     PICTURE X(40).
019580991021           03  FILLER                  PICTURE X(32).
019590991213
019600991213       01  STMT-HDG-07-V8-R            REDEFINES STMT-HDG-07.
019610991213           03  FILLER                  PICTURE X(8).
019620991213           03  SH07V8-INVESTOR-ADDRESS4
019630991213                                       PICTURE X(40).
019640991213           03  FILLER                  PICTURE X(35).
019650991213           03  SH07V8-BRANCH-ADDR4     PICTURE X(40).
019660991213           03  FILLER                  PICTURE X(27).
019670991213
019680000000      /
019690000000       01  STMT-HDG-08.
019700000000           03  FILLER                  PICTURE X(8).
019710000000           03  SH08-INVESTOR-ADDRESS5  PICTURE X(40).
019720000000           03  FILLER                  PICTURE X(102).
019730000000
019740000000       01  STMT-HDG-08-V1-R            REDEFINES STMT-HDG-08.
019750000000           03  FILLER                  PICTURE X(8).
019760000000           03  SH08V1-INVESTOR-ADDRESS5
019770000000                                       PICTURE X(40).
019780000000           03  FILLER                  PICTURE X(102).
019790000000
019800000000       01  STMT-HDG-08-V2-R            REDEFINES STMT-HDG-08.
019810000000           03  FILLER                  PICTURE X(8).
019820000000           03  SH08V2-INVESTOR-ADDRESS5
019830000000                                       PICTURE X(40).
019840000000           03  FILLER                  PICTURE X(102).
019850000000
019860000000       01  STMT-HDG-08-V3-R            REDEFINES STMT-HDG-08.
019870000000           03  FILLER                  PICTURE X(8).
019880000000           03  SH08V3-INVESTOR-ADDRESS5
019890000000                                       PICTURE X(40).
019900000000           03  FILLER                  PICTURE X(102).
019910000000
019920000000       01  STMT-HDG-08-V4-R            REDEFINES STMT-HDG-08.
019930000000           03  FILLER                  PICTURE X(7).
019940000000           03  SH08V4-INVESTOR-ADDRESS5
019950000000                                       PICTURE X(40).
019960000000           03  FILLER                  PICTURE X(103).
019970000000
019980000000       01  STMT-HDG-08-V5-R            REDEFINES STMT-HDG-08.
019990000000           03  FILLER                  PICTURE X(7).
020000000000           03  SH08V5-INVESTOR-ADDRESS5
020010000000                                       PICTURE X(40).
020020000000           03  FILLER                  PICTURE X(103).
020030990922
020040990922       01  STMT-HDG-08-V6-R            REDEFINES STMT-HDG-08.
020050000223           03  FILLER                  PICTURE X(8).
020060990923           03  SH08V6-POSTAL-CODE      PICTURE X(10).
020070000223           03  FILLER                  PICTURE X(100).
020080991019
020090991019       01  STMT-HDG-08-V7-R            REDEFINES STMT-HDG-08.
020100991019           03  FILLER                  PICTURE X(8).
020110991019           03  SH08V7-INVESTOR-ADDRESS5  PICTURE X(40).
020120991019           03  FILLER                  PICTURE X(102).
020130991213
020140991213       01  STMT-HDG-08-V8-R            REDEFINES STMT-HDG-08.
020150991213           03  FILLER                  PICTURE X(8).
020160991213           03  SH08V8-INVESTOR-ADDRESS5
020170991213                                       PICTURE X(40).
020180991213           03  FILLER                  PICTURE X(102).
020190991213
020200000000      /
020210000000       01  STMT-HDG-09.
020220000000           03  FILLER                  PICTURE X(8).
020230000000           03  SH09-POSTAL-CODE        PICTURE X(10).
020240000000           03  FILLER                  PICTURE X(132).
020250000000
020260000000       01  STMT-HDG-09-V1-R            REDEFINES STMT-HDG-09.
020270000000           03  FILLER                  PICTURE X(8).
020280000000           03  SH09V1-POSTAL-CODE      PICTURE X(10).
020290000000           03  FILLER                  PICTURE X(132).
020300000000
020310000000       01  STMT-HDG-09-V2-R            REDEFINES STMT-HDG-09.
020320000000           03  FILLER                  PICTURE X(8).
020330000000           03  SH09V2-POSTAL-CODE      PICTURE X(10).
020340000000           03  FILLER                  PICTURE X(132).
020350000000
020360000000       01  STMT-HDG-09-V3-R            REDEFINES STMT-HDG-09.
020370000000           03  FILLER                  PICTURE X(8).
020380000000           03  SH09V3-POSTAL-CODE      PICTURE X(10).
020390000000           03  FILLER                  PICTURE X(132).
020400000000
020410000000       01  STMT-HDG-09-V4-R            REDEFINES STMT-HDG-09.
020420000000           03  FILLER                  PICTURE X(7).
020430000000           03  SH09V4-POSTAL-CODE      PICTURE X(10).
020440000000           03  FILLER                  PICTURE X(133).
020450000000
020460000000       01  STMT-HDG-09-V5-R            REDEFINES STMT-HDG-09.
020470000000           03  FILLER                  PICTURE X(7).
020480000000           03  SH09V5-POSTAL-CODE      PICTURE X(10).
020490000000           03  FILLER                  PICTURE X(133).
020500990922
020510990922       01  STMT-HDG-09-V6-R            REDEFINES STMT-HDG-09.
020520000223           03  FILLER                  PICTURE X(43).
020530990923           03  SH09-LIT1               PICTURE X(30).
020540000223           03  FILLER                  PICTURE X(45).
020550991019
020560991019       01  STMT-HDG-09-V7-R            REDEFINES STMT-HDG-09.
020570991019           03  FILLER                  PICTURE X(8).
020580991019           03  SH09V7-POSTAL-CODE      PICTURE X(10).
020590991019           03  FILLER                  PICTURE X(132).
020600991213
020610991213       01  STMT-HDG-09-V8-R            REDEFINES STMT-HDG-09.
020620991213           03  FILLER                  PICTURE X(8).
020630991213           03  SH09V8-POSTAL-CODE      PICTURE X(10).
020640991213           03  FILLER                  PICTURE X(132).
020650991213
020660000000      /
020670000000       01  STMT-HDG-10.
020680000000           03  FILLER                  PICTURE X(8).
020690000000           03  SH10-LIT1               PICTURE X(47).
020700000000           03  FILLER                  PICTURE X(32).
020710000000           03  SH10-PHONE              PICTURE X(14).
020720000000           03  FILLER                  PICTURE X(49).
020730000000
020740000000       01  STMT-HDG-10-V1-R            REDEFINES STMT-HDG-10.
020750000000           03  FILLER                  PICTURE X(8).
020760000000           03  SH10V1-LIT1             PICTURE X(47).
020770000000           03  FILLER                  PICTURE X(30).
020780000000           03  SH10V1-PHONE            PICTURE X(14).
020790000000           03  FILLER                  PICTURE X(51).
020800000000
020810000000       01  STMT-HDG-10-V2-R            REDEFINES STMT-HDG-10.
020820000000           03  FILLER                  PICTURE X(8).
020830000000           03  SH10V2-LIT1             PICTURE X(47).
020840990226           03  FILLER                  PICTURE X(31).
020850000000           03  SH10V2-PHONE            PICTURE X(14).
020860990226           03  FILLER                  PICTURE X(50).
020870000000
020880000000       01  STMT-HDG-10-V3-R            REDEFINES STMT-HDG-10.
020890000000           03  FILLER                  PICTURE X(8).
020900000000           03  SH10V3-LIT1             PICTURE X(47).
020910000000           03  FILLER                  PICTURE X(30).
020920000000           03  SH10V3-PHONE            PICTURE X(14).
020930000000           03  FILLER                  PICTURE X(51).
020940000000
020950000000       01  STMT-HDG-10-V4-R            REDEFINES STMT-HDG-10.
020960000000           03  FILLER                  PICTURE X(7).
020970000000           03  SH10V4-LIT1             PICTURE X(47).
020980000000           03  FILLER                  PICTURE X(30).
020990000000           03  SH10V4-PHONE            PICTURE X(14).
021000000000           03  FILLER                  PICTURE X(52).
021010000000
021020000000       01  STMT-HDG-10-V5-R            REDEFINES STMT-HDG-10.
021030000000           03  FILLER                  PICTURE X(7).
021040000000           03  SH10V5-LIT1             PICTURE X(47).
021050000000           03  FILLER                  PICTURE X(30).
021060000000           03  SH10V5-PHONE            PICTURE X(14).
021070000000           03  FILLER                  PICTURE X(52).
021080990922
021090990922       01  STMT-HDG-10-V6-R            REDEFINES STMT-HDG-10.
021100000223           03  FILLER                  PICTURE X(8).
021110990923           03  SH10V6-LIT2             PICTURE X(12).
021120991208           03  FILLER                  PICTURE X(1).
021130990923           03  SH10V6-ACCOUNT          PICTURE X(10).
021140000223           03  FILLER                  PICTURE X(87).
021150991019
021160991019       01  STMT-HDG-10-V7-R            REDEFINES STMT-HDG-10.
021170991019           03  FILLER                  PICTURE X(8).
021180991019           03  SH10V7-LIT1             PICTURE X(47).
021190991021           03  FILLER                  PICTURE X(23).
021200991019           03  SH10V7-PHONE            PICTURE X(14).
021210991021           03  FILLER                  PICTURE X(58).
021220991213
021230991213       01  STMT-HDG-10-V8-R            REDEFINES STMT-HDG-10.
021240991213           03  FILLER                  PICTURE X(8).
021250991213           03  SH10V8-LIT1             PICTURE X(47).
021260991213           03  FILLER                  PICTURE X(30).
021270991213           03  SH10V8-PHONE            PICTURE X(14).
021280991213           03  FILLER                  PICTURE X(51).
021290991213
021300000000      /
021310000000       01  STMT-HDG-11.
021320000000           03  FILLER                  PICTURE X(8).
021330000000           03  SH11-NOMINEE            PICTURE X(53).
021340000000           03  FILLER                  PICTURE X(26).
021350000000           03  SH11-FAX                PICTURE X(14).
021360000000           03  FILLER                  PICTURE X(49).
021370000000
021380000000       01  STMT-HDG-11-V1-R            REDEFINES STMT-HDG-11.
021390000000           03  FILLER                  PICTURE X(8).
021400000000           03  SH11V1-NOMINEE          PICTURE X(53).
021410000000           03  FILLER                  PICTURE X(24).
021420000000           03  SH11V1-FAX              PICTURE X(14).
021430000000           03  FILLER                  PICTURE X(51).
021440000000
021450000000       01  STMT-HDG-11-V2-R            REDEFINES STMT-HDG-11.
021460000000           03  FILLER                  PICTURE X(8).
021470000000           03  SH11V2-NOMINEE          PICTURE X(53).
021480990226           03  FILLER                  PICTURE X(25).
021490000000           03  SH11V2-FAX              PICTURE X(14).
021500990226           03  FILLER                  PICTURE X(50).
021510000000
021520000000       01  STMT-HDG-11-V3-R            REDEFINES STMT-HDG-11.
021530000000           03  FILLER                  PICTURE X(8).
021540000000           03  SH11V3-NOMINEE          PICTURE X(53).
021550000000           03  FILLER                  PICTURE X(24).
021560000000           03  SH11V3-FAX              PICTURE X(14).
021570000000           03  FILLER                  PICTURE X(51).
021580000000
021590000000       01  STMT-HDG-11-V4-R            REDEFINES STMT-HDG-11.
021600000000           03  FILLER                  PICTURE X(7).
021610000000           03  SH11V4-NOMINEE          PICTURE X(53).
021620000000           03  FILLER                  PICTURE X(24).
021630000000           03  SH11V4-FAX              PICTURE X(14).
021640000000           03  FILLER                  PICTURE X(52).
021650000000
021660000000       01  STMT-HDG-11-V5-R            REDEFINES STMT-HDG-11.
021670000000           03  FILLER                  PICTURE X(7).
021680000000           03  SH11V5-NOMINEE          PICTURE X(53).
021690000000           03  FILLER                  PICTURE X(24).
021700000000           03  SH11V5-FAX              PICTURE X(14).
021710000000           03  FILLER                  PICTURE X(52).
021720990922
021730990922       01  STMT-HDG-11-V6-R            REDEFINES STMT-HDG-11.
021740000223           03  FILLER                  PICTURE X(8).
021750990923           03  FILLER                  PICTURE X(30).
021760000223           03  FILLER                  PICTURE X(80).
021770991019
021780991019       01  STMT-HDG-11-V7-R            REDEFINES STMT-HDG-11.
021790991019           03  FILLER                  PICTURE X(8).
021800991019           03  SH11V7-NOMINEE          PICTURE X(53).
021810991021           03  FILLER                  PICTURE X(17).
021820991019           03  SH11V7-FAX              PICTURE X(14).
021830991021           03  FILLER                  PICTURE X(58).
021840991213
021850991213       01  STMT-HDG-11-V8-R            REDEFINES STMT-HDG-11.
021860991213           03  FILLER                  PICTURE X(8).
021870991213           03  SH11V8-NOMINEE          PICTURE X(53).
021880991213           03  FILLER                  PICTURE X(24).
021890991213           03  SH11V8-FAX              PICTURE X(14).
021900991213           03  FILLER                  PICTURE X(51).
021910991213
021920000000      /
021930000000      ******************************************************************
021940000000      *                                                                *
021950000000      *    S T A T E M E N T   D E T A I L   P R I N T   L I N E S     *
021960000000      *                                                                *
021970000000      ******************************************************************
021980000000
021990000000      *****
022000000000      *****    The statement detail lines will be re-defined for each
022010000000      *****    version of the statement.
022020000000      *****        Version 1:  Used by AIC, INF, MLA.
022030990716      *****        Version 2:  Used by CND, BTG, GRS
022040000000      *****        Version 3:  Used by LEO & NFC.
022050000000      *****        Version 4:  Used by RBF.
022060980202      *****        Version 5:  Used by GLS & MBU.
022070990922      *****        Version 6:  Used by UBS.
022080991019      *****        Version 7:  Used by GGF.
022090991213      *****        Version 8:  Used by MLA.
022100000000      *****
022110990927
022120000000      /
022130000000       01  STMT-DTL-01.
022140000000           03  FILLER                  PICTURE X(8).
022150000000           03  SD01-INVESTMENT-NAME    PICTURE X(35).
022160000000           03  FILLER                  PICTURE X(107).
022170000000
022180000000       01  STMT-DTL-01-V1-R            REDEFINES STMT-DTL-01.
022190000000           03  FILLER                  PICTURE X(7).
022200000000           03  SD01V1-INVESTMENT-NAME  PICTURE X(35).
022210000000           03  FILLER                  PICTURE X(108).
022220000000
022230000000       01  STMT-DTL-01-V2-R            REDEFINES STMT-DTL-01.
022240990226           03  FILLER                  PICTURE X(7).
022250000000           03  SD01V2-INVESTMENT-NAME  PICTURE X(35).
022260990226           03  FILLER                  PICTURE X(108).
022270000000
022280000000       01  STMT-DTL-01-V3-R            REDEFINES STMT-DTL-01.
022290000000           03  FILLER                  PICTURE X(8).
022300000000           03  SD01V3-INVESTMENT-NAME  PICTURE X(35).
022310000000           03  FILLER                  PICTURE X(107).
022320000000
022330000000       01  STMT-DTL-01-V4-R            REDEFINES STMT-DTL-01.
022340000000           03  FILLER                  PICTURE X(7).
022350000000           03  SD01V4-INVESTMENT-NAME  PICTURE X(35).
022360000000           03  FILLER                  PICTURE X(108).
022370000000
022380000000       01  STMT-DTL-01-V5-R            REDEFINES STMT-DTL-01.
022390000000           03  FILLER                  PICTURE X(7).
022400000000           03  SD01V5-INVESTMENT-NAME  PICTURE X(35).
022410000000           03  FILLER                  PICTURE X(108).
022420990922
022430990922       01  STMT-DTL-01-V6-R            REDEFINES STMT-DTL-01.
022440000223           03  FILLER                  PICTURE X(19).
022450990922           03  SD01V6-INVESTMENT-NAME  PICTURE X(35).
022460000223           03  FILLER                  PICTURE X(64).
022470991019
022480991019       01  STMT-DTL-01-V7-R            REDEFINES STMT-DTL-01.
022490991021           03  FILLER                  PICTURE X(9).
022500991019           03  SD01V7-INVESTMENT-NAME  PICTURE X(35).
022510991021           03  FILLER                  PICTURE X(106).
022520991213
022530991213       01  STMT-DTL-01-V8-R            REDEFINES STMT-DTL-01.
022540991216           03  FILLER                  PICTURE X(10).
022550991213           03  SD01V8-INVESTMENT-NAME  PICTURE X(35).
022560991216           03  FILLER                  PICTURE X(105).
022570991213
022580000000      /
022590000000       01  STMT-DTL-02.
022600000000           03  SD02-LIT1.
022610000000               05  SD02-TRADE-DATE     PICTURE X(6).
022620000000           03  FILLER                  PICTURE X(2).
022630000000           03  SD02-TRANS-TYPE         PICTURE X(25).
022640990226           03  FILLER                  PICTURE X(3).
022650000000           03  SD02-GROSS              PICTURE $$$$,$$$,$$$.99.
022660000000           03  SD02-GROSS-SIGN         PICTURE X(1).
022670000000           03  SD02-DEDUCTIONS         PICTURE $$$,$$$,$$$.99.
022680000000           03  SD02-DEDUCTIONS-SIGN    PICTURE X(1).
022690000000           03  SD02-NET                PICTURE $$$$,$$$,$$$.99.
022700000000           03  SD02-NET-SIGN           PICTURE X(1).
022710000000           03  FILLER                  PICTURE X(3).
022720000000           03  SD02-UNIT-PRICE         PICTURE ZZZ.9999.
022730000000           03  SD02-UNIT-PRICE-SIGN    PICTURE X(1).
022740000000           03  SD02-NO-OF-UNITS        PICTURE ZZZ,ZZZ,ZZZ.999.
022750000000           03  SD02-NO-OF-UNITS-SIGN   PICTURE X(1).
022760000000           03  SD02-UNIT-BALANCE       PICTURE ZZZ,ZZZ,ZZZ.999.
022770000000           03  SD02-UNIT-BALANCE-SIGN  PICTURE X(1).
022780000000           03  SD02-CPTL-GAIN-OFLIT.
022790000000               05  SD02-CPTL-GAIN      PICTURE $$$$$$.99.
022800000000               05  SD02-CPTL-GAIN-SIGN PICTURE X(1).
022810990226           03  FILLER                  PICTURE X(13).
022820000000
022830000000       01  STMT-DTL-02-V1-R            REDEFINES STMT-DTL-02.
022840000000           03  SD02V1-LIT1.
022850000000               05  SD02V1-TRADE-DATE   PICTURE X(6).
022860000000           03  FILLER                  PICTURE X(1).
022870000000           03  SD02V1-TRANS-TYPE       PICTURE X(25).
022880000000           03  FILLER                  PICTURE X(1).
022890000000           03  SD02V1-GROSS            PICTURE $$$$,$$$,$$$.99.
022900000000           03  SD02V1-GROSS-SIGN       PICTURE X(1).
022910000000           03  SD02V1-DEDUCTIONS       PICTURE $$$$,$$$,$$$.99.
022920000000           03  SD02V1-DEDUCTIONS-SIGN  PICTURE X(1).
022930000000           03  SD02V1-NET              PICTURE $$$,$$$,$$$.99.
022940000000           03  SD02V1-NET-SIGN         PICTURE X(1).
022950000000           03  FILLER                  PICTURE X(2).
022960000000           03  SD02V1-UNIT-PRICE       PICTURE ZZZ.9999.
022970000000           03  SD02V1-UNIT-PRICE-SIGN  PICTURE X(1).
022980000000           03  SD02V1-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
022990000000           03  SD02V1-NO-OF-UNITS-SIGN PICTURE X(1).
023000000000           03  SD02V1-UNIT-BALANCE     PICTURE ZZ,ZZZ,ZZZ.999.
023010000000           03  SD02V1-UNIT-BALANCE-SIGN
023020000000                                       PICTURE X(1).
023030000000           03  SD02V1-CPTL-GAIN-OFLIT.
023040000000               05  SD02V1-CPTL-GAIN    PICTURE $$$$$$$$.99.
023050000000               05  SD02V1-CPTL-GAIN-SIGN
023060000000                                       PICTURE X(1).
023070000000           03  FILLER                  PICTURE X(16).
023080000000
023090000000       01  STMT-DTL-02-V2-R            REDEFINES STMT-DTL-02.
023100990226           03  SD02V2-LIT1.
023110990226               05  SD02V2-TRADE-DATE   PICTURE X(6).
023120990226           03  FILLER                  PICTURE X(1).
023130990226           03  SD02V2-TRANS-TYPE       PICTURE X(25).
023140990303           03  FILLER                  PICTURE X(3).
023150990226           03  SD02V2-GROSS            PICTURE $$$$,$$$,$$$.99.
023160990226           03  SD02V2-GROSS-SIGN       PICTURE X(1).
023170990226           03  SD02V2-DEDUCTIONS       PICTURE $$$$,$$$,$$$.99.
023180010105           03  SD02V2-DEDUCTIONS-SIGN  PICTURE X(1).
023190990226           03  SD02V2-NET              PICTURE $$$,$$$,$$$.99.
023200990226           03  SD02V2-NET-SIGN         PICTURE X(1).
023210990226           03  FILLER                  PICTURE X(2).
023220990226           03  SD02V2-UNIT-PRICE       PICTURE ZZZ.9999.
023230990226           03  SD02V2-UNIT-PRICE-SIGN  PICTURE X(1).
023240990226           03  SD02V2-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
023250990226           03  SD02V2-NO-OF-UNITS-SIGN PICTURE X(1).
023260990226           03  SD02V2-UNIT-BALANCE     PICTURE ZZ,ZZZ,ZZZ.999.
023270010105           03  SD02V2-UNIT-BALANCE-SIGN
023280990226                                       PICTURE X(1).
023290990226           03  SD02V2-CPTL-GAIN-OFLIT.
023300010105               05  SD02V2-CPTL-GAIN    PICTURE $$$$$$$$.99.
023310990226               05  SD02V2-CPTL-GAIN-SIGN
023320990226                                       PICTURE X(1).
023330010105           03  FILLER                  PICTURE X(14).
023340000000
023350000000       01  STMT-DTL-02-V3-R            REDEFINES STMT-DTL-02.
023360000000           03  SD02V3-LIT1.
023370000000               05  SD02V3-TRADE-DATE   PICTURE X(6).
023380000000           03  FILLER                  PICTURE X(2).
023390000000           03  SD02V3-TRANS-TYPE       PICTURE X(25).
023400000000           03  FILLER                  PICTURE X(2).
023410000000           03  SD02V3-GROSS            PICTURE $$$$,$$$,$$$.99.
023420000000           03  SD02V3-GROSS-SIGN       PICTURE X(1).
023430000000           03  SD02V3-DEDUCTIONS       PICTURE $$$$,$$$,$$$.99.
023440000000           03  SD02V3-DEDUCTIONS-SIGN  PICTURE X(1).
023450000000           03  SD02V3-NET              PICTURE $$$$,$$$,$$$.99.
023460000000           03  SD02V3-NET-SIGN         PICTURE X(1).
023470000000           03  FILLER                  PICTURE X(2).
023480000000           03  SD02V3-UNIT-PRICE       PICTURE ZZZ.9999.
023490000000           03  SD02V3-UNIT-PRICE-SIGN  PICTURE X(1).
023500000000           03  SD02V3-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
023510000000           03  SD02V3-NO-OF-UNITS-SIGN PICTURE X(1).
023520000000           03  SD02V3-UNIT-BALANCE     PICTURE ZZZ,ZZZ,ZZZ.999.
023530000000           03  SD02V3-UNIT-BALANCE-SIGN
023540000000                                       PICTURE X(1).
023550000000           03  SD02V3-CPTL-GAIN-OFLIT.
023560000000               05  SD02V3-CPTL-GAIN    PICTURE $$$$$$.99.
023570000000               05  SD02V3-CPTL-GAIN-SIGN
023580000000                                       PICTURE X(1).
023590000000           03  FILLER                  PICTURE X(14).
023600000000
023610000000       01  STMT-DTL-02-V4-R            REDEFINES STMT-DTL-02.
023620000000           03  SD02V4-LIT1.
023630000000               05  SD02V4-TRADE-DATE   PICTURE X(6).
023640000000           03  FILLER                  PICTURE X(1).
023650000000           03  SD02V4-TRANS-TYPE       PICTURE X(25).
023660000000           03  FILLER                  PICTURE X(2).
023670000000           03  SD02V4-GROSS            PICTURE $$$$,$$$,$$$.99.
023680000000           03  SD02V4-GROSS-SIGN       PICTURE X(1).
023690000000           03  SD02V4-DEDUCTIONS       PICTURE $$$,$$$,$$$.99.
023700000000           03  SD02V4-DEDUCTIONS-SIGN  PICTURE X(1).
023710000000           03  SD02V4-NET              PICTURE $$$,$$$,$$$.99.
023720000000           03  SD02V4-NET-SIGN         PICTURE X(1).
023730000000           03  FILLER                  PICTURE X(1).
023740000000           03  SD02V4-UNIT-PRICE       PICTURE ZZZ.9999.
023750000000           03  SD02V4-UNIT-PRICE-SIGN  PICTURE X(1).
023760000000           03  SD02V4-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
023770000000           03  SD02V4-NO-OF-UNITS-SIGN PICTURE X(1).
023780000000           03  SD02V4-UNIT-BALANCE     PICTURE ZZZ,ZZZ,ZZZ.999.
023790000000           03  SD02V4-UNIT-BALANCE-SIGN
023800000000                                       PICTURE X(1).
023810000000           03  SD02V4-CPTL-GAIN-OFLIT.
023820000000               05  SD02V4-CPTL-GAIN    PICTURE $$$$$$$$.99.
023830000000               05  SD02V4-CPTL-GAIN-SIGN
023840000000                                       PICTURE X(1).
023850000000           03  FILLER                  PICTURE X(16).
023860000000
023870000000       01  STMT-DTL-02-V5-R            REDEFINES STMT-DTL-02.
023880000000           03  SD02V5-LIT1.
023890000000               05  SD02V5-TRADE-DATE   PICTURE X(6).
023900000000           03  FILLER                  PICTURE X(1).
023910000000           03  SD02V5-TRANS-TYPE       PICTURE X(25).
023920020313R13107*    03  FILLER                  PICTURE X(2).
023930000000           03  SD02V5-GROSS            PICTURE $$$$,$$$,$$$.99.
023940000000           03  SD02V5-GROSS-SIGN       PICTURE X(1).
023950000000           03  SD02V5-DEDUCTIONS       PICTURE $$$,$$$,$$$.99.
023960000000           03  SD02V5-DEDUCTIONS-SIGN  PICTURE X(1).
023970000000           03  SD02V5-NET              PICTURE $$$,$$$,$$$.99.
023980000000           03  SD02V5-NET-SIGN         PICTURE X(1).
023990000000           03  FILLER                  PICTURE X(1).
024000020313R13107*    03  SD02V5-UNIT-PRICE       PICTURE ZZZ.9999.
024010020313R13107     03  SD02V5-UNIT-PRICE       PICTURE Z,ZZZ.9999.
024020000000           03  SD02V5-UNIT-PRICE-SIGN  PICTURE X(1).
024030000000           03  SD02V5-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
024040000000           03  SD02V5-NO-OF-UNITS-SIGN PICTURE X(1).
024050000000           03  SD02V5-UNIT-BALANCE     PICTURE ZZZ,ZZZ,ZZZ.999.
024060000000           03  SD02V5-UNIT-BALANCE-SIGN
024070000000                                       PICTURE X(1).
024080000000           03  SD02V5-CPTL-GAIN-OFLIT.
024090000000               05  SD02V5-CPTL-GAIN    PICTURE $$$$$$$$.99.
024100000000               05  SD02V5-CPTL-GAIN-SIGN
024110000000                                       PICTURE X(1).
024120000000           03  FILLER                  PICTURE X(16).
024130990922
024140990922       01  STMT-DTL-02-V6-R            REDEFINES STMT-DTL-02.
024150000223           03  FILLER                  PICTURE X(8).
024160990923           03  SD02V6-LIT1.
024170990923               05  SD02V6-TRADE-DATE   PICTURE XXXX/XX/XX.
024180991208           03  FILLER                  PICTURE X(1).
024190990922           03  SD02V6-TRANS-TYPE       PICTURE X(25).
024200000222           03  SD02V6-GROSS            PICTURE $$$,$$$,$$$,$$$.99.
024210990922           03  SD02V6-GROSS-SIGN       PICTURE X(1).
024220000228      *    03  FILLER                  PICTURE X(1).
024230000228           03  SD02V6-UNIT-PRICE       PICTURE ZZ,ZZZ.9999.
024240990922           03  SD02V6-UNIT-PRICE-SIGN  PICTURE X(1).
024250990924           03  FILLER                  PICTURE X(1).
024260990922           03  SD02V6-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
024270990922           03  SD02V6-NO-OF-UNITS-SIGN PICTURE X(1).
024280990924           03  FILLER                  PICTURE X(1).
024290000222           03  SD02V6-UNIT-BALANCE     PICTURE ZZ,ZZZ,ZZZ,ZZZ.99.
024300990922           03  SD02V6-UNIT-BALANCE-SIGN
024310990922                                       PICTURE X(1).
024320000223           03  FILLER                  PICTURE X(9).
024330991019
024340991019       01  STMT-DTL-02-V7-R            REDEFINES STMT-DTL-02.
024350991019           03  SD02V7-LIT1.
024360991019               05  SD02V7-TRADE-DATE   PICTURE X(8).
024370991022           03  FILLER                  PICTURE X(1).
024380991022           03  SD02V7-TRANS-TYPE       PICTURE X(25).
024390991022           03  FILLER                  PICTURE X(1).
024400991022           03  SD02V7-GROSS            PICTURE $$$$,$$$,$$$.99.
024410991022           03  SD02V7-GROSS-SIGN       PICTURE X(1).
024420991022           03  SD02V7-DEDUCTIONS       PICTURE $$$$,$$$,$$$.99.
024430991022           03  SD02V7-DEDUCTIONS-SIGN  PICTURE X(1).
024440991022           03  SD02V7-NET              PICTURE $$$,$$$,$$$.99.
024450991022           03  SD02V7-NET-SIGN         PICTURE X(1).
024460991022           03  SD02V7-UNIT-PRICE       PICTURE ZZZ.9999.
024470991022           03  SD02V7-UNIT-PRICE-SIGN  PICTURE X(1).
024480991022           03  SD02V7-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
024490991022           03  SD02V7-NO-OF-UNITS-SIGN PICTURE X(1).
024500991022           03  SD02V7-UNIT-BALANCE     PICTURE ZZ,ZZZ,ZZZ.999.
024510991022           03  SD02V7-UNIT-BALANCE-SIGN
024520991022                                       PICTURE X(1).
024530991022           03  SD02V7-CPTL-GAIN-OFLIT.
024540991022               05  SD02V7-CPTL-GAIN    PICTURE $$$$$$$$.99.
024550991022               05  SD02V7-CPTL-GAIN-SIGN
024560991022                                       PICTURE X(1).
024570991022           03  FILLER                  PICTURE X(17).
024580991022
024590991213       01  STMT-DTL-02-V8-R            REDEFINES STMT-DTL-02.
024600991213           03  SD02V8-LIT1.
024610991216               05  SD02V8-TRADE-DATE   PICTURE X(9).
024620991213           03  FILLER                  PICTURE X(1).
024630991216           03  SD02V8-TRANS-TYPE       PICTURE X(22).
024640991213           03  FILLER                  PICTURE X(1).
024650991213           03  SD02V8-GROSS            PICTURE $$$$,$$$,$$$.99.
024660991213           03  SD02V8-GROSS-SIGN       PICTURE X(1).
024670991213           03  SD02V8-DEDUCTIONS       PICTURE $$$$,$$$,$$$.99.
024680991213           03  SD02V8-DEDUCTIONS-SIGN  PICTURE X(1).
024690991213           03  SD02V8-NET              PICTURE $$$,$$$,$$$.99.
024700991213           03  SD02V8-NET-SIGN         PICTURE X(1).
024710991213           03  FILLER                  PICTURE X(2).
024720991213           03  SD02V8-UNIT-PRICE       PICTURE ZZZ.9999.
024730991213           03  SD02V8-UNIT-PRICE-SIGN  PICTURE X(1).
024740991213           03  SD02V8-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
024750991213           03  SD02V8-NO-OF-UNITS-SIGN PICTURE X(1).
024760991213           03  SD02V8-UNIT-BALANCE     PICTURE ZZ,ZZZ,ZZZ.999.
024770991213           03  SD02V8-UNIT-BALANCE-SIGN
024780991213                                       PICTURE X(1).
024790991213           03  SD02V8-CPTL-GAIN-OFLIT.
024800991213               05  SD02V8-CPTL-GAIN    PICTURE $$$$$$$$.99.
024810991213               05  SD02V8-CPTL-GAIN-SIGN
024820991213                                       PICTURE X(1).
024830991216           03  FILLER                  PICTURE X(16).
024840991213
024850000000      /
024860000000      ******************************************************************
024870000000      *                                                                *
024880000000      *    S T A T E M E N T   S U M M A R Y   P R I N T   L I N E S   *
024890000000      *                                                                *
024900000000      ******************************************************************
024910000000
024920000000      *****
024930000000      *****    The statement summary lines will be re-defined for each
024940000000      *****    version of the statement.
024950000000      *****        Version 1:  Used by AIC, INF, MLA.
024960990716      *****        Version 2:  Used by CND, BTG, GRS
024970000000      *****        Version 3:  Used by LEO & NFC.
024980000000      *****        Version 4:  Used by RBF.
024990980202      *****        Version 5:  Used by GLS & MBU.
025000990922      *****        Version 6:  Used by UBS.
025010991021      *****        Version 7:  Used by GGF.
025020991213      *****        Version 8:  Used by MLA.
025030000000      *****
025040990925
025050000000      /
025060000000       01  STMT-SUM-01.
025070000000           03  SS01-LIT1               PICTURE X(137).
025080000000           03  FILLER                  PICTURE X(13).
025090000000
025100000000       01  STMT-SUM-01-V1-R            REDEFINES STMT-SUM-01.
025110000000           03  SS01V1-LIT1             PICTURE X(137).
025120000000           03  FILLER                  PICTURE X(13).
025130000000
025140000000       01  STMT-SUM-01-V2-R            REDEFINES STMT-SUM-01.
025150990226           03  SS01V2-LIT1             PICTURE X(137).
025160990226           03  FILLER                  PICTURE X(13).
025170000000
025180000000       01  STMT-SUM-01-V3-R            REDEFINES STMT-SUM-01.
025190000000           03  SS01V3-LIT1             PICTURE X(137).
025200000000           03  FILLER                  PICTURE X(13).
025210000000
025220000000       01  STMT-SUM-01-V4-R            REDEFINES STMT-SUM-01.
025230000000           03  SS01V4-LIT1             PICTURE X(137).
025240000000           03  FILLER                  PICTURE X(13).
025250000000
025260000000       01  STMT-SUM-01-V5-R            REDEFINES STMT-SUM-01.
025270000000           03  SS01V5-LIT1             PICTURE X(137).
025280000000           03  FILLER                  PICTURE X(13).
025290990922
025300990922       01  STMT-SUM-01-V6-R            REDEFINES STMT-SUM-01.
025310000223           03  FILLER                  PICTURE X(7).
025320000223           03  SS01V6-LIT1             PICTURE X(108).
025330000223           03  FILLER                  PICTURE X(3).
025340991021
025350991021       01  STMT-SUM-01-V7-R            REDEFINES STMT-SUM-01.
025360991021           03  SS01V7-LIT1             PICTURE X(137).
025370991021           03  FILLER                  PICTURE X(13).
025380991213
025390991213       01  STMT-SUM-01-V8-R            REDEFINES STMT-SUM-01.
025400991213           03  SS01V8-LIT1             PICTURE X(137).
025410991213           03  FILLER                  PICTURE X(13).
025420991213
025430000000      /
025440000000       01  STMT-SUM-02.
025450000000           03  FILLER                  PICTURE X(40).
025460000000           03  SS02-LIT0               PICTURE X(11).
025470050104      *RFS25032 - Begins
025480050104      *    03  FILLER                  PICTURE X(35).
025490050104           03  FILLER                  PICTURE X(22).
025500050104      *RFS25032 - ENDS
025510000000           03  SS02-LIT1               PICTURE X(8).
025520000000           03  FILLER                  PICTURE X(3).
025530000000           03  SS02-LIT2               PICTURE X(13).
025540000000           03  FILLER                  PICTURE X(3).
025550000000           03  SS02-LIT3               PICTURE X(13).
025560150212133528*    03  FILLER                  PICTURE X(2).
025570150212133528     03  FILLER                  PICTURE X(6).
025580000000           03  SS02-LIT4               PICTURE X(8).
025590150212133528*    03  FILLER                  PICTURE X(14).
025600150212133528     03  FILLER                  PICTURE X(10).
025610000000
025620000000       01  STMT-SUM-02-V1-R            REDEFINES STMT-SUM-02.
025630160713157301*    03  FILLER                  PICTURE X(37).
025640160713157301     03  FILLER                  PICTURE X(29).
025650160713           03  SS02V1-LIT0             PICTURE X(11).
025660000000           03  FILLER                  PICTURE X(34).
025670000000           03  SS02V1-LIT1             PICTURE X(8).
025680000000           03  FILLER                  PICTURE X(3).
025690000000           03  SS02V1-LIT2             PICTURE X(13).
025700000000           03  FILLER                  PICTURE X(2).
025710000000           03  SS02V1-LIT3             PICTURE X(13).
025720160713157301*    03  FILLER                  PICTURE X(4).
025730160713157301     03  FILLER                  PICTURE X(6).
025740000000           03  SS02V1-LIT4             PICTURE X(8).
025750160713157301*    03  FILLER                  PICTURE X(17).
025760160713157301     03  FILLER                  PICTURE X(15).
025770150220
025780150220
025790000000       01  STMT-SUM-02-V2-R            REDEFINES STMT-SUM-02.
025800990226           03  FILLER                  PICTURE X(37).
025810990226           03  SS02V2-LIT0             PICTURE X(11).
025820151217154219*    03  FILLER                  PICTURE X(34).
025830151217154219     03  FILLER                  PICTURE X(30).
025840990226           03  SS02V2-LIT1             PICTURE X(8).
025850990226           03  FILLER                  PICTURE X(3).
025860990226           03  SS02V2-LIT2             PICTURE X(13).
025870120913110996     03  FILLER                  PICTURE X(6).
025880990226           03  SS02V2-LIT3             PICTURE X(13).
025890120913110996     03  FILLER                  PICTURE X(1).
025900990226           03  SS02V2-LIT4             PICTURE X(8).
025910120913110996     03  FILLER                  PICTURE X(16).
025920000000
025930000000       01  STMT-SUM-02-V3-R            REDEFINES STMT-SUM-02.
025940000000           03  FILLER                  PICTURE X(39).
025950000000           03  SS02V3-LIT0             PICTURE X(11).
025960151218154219*    03  FILLER                  PICTURE X(35).
025970151218154219     03  FILLER                  PICTURE X(32).
025980000000           03  SS02V3-LIT1             PICTURE X(8).
025990000000           03  FILLER                  PICTURE X(3).
026000000000           03  SS02V3-LIT2             PICTURE X(13).
026010000000           03  FILLER                  PICTURE X(3).
026020000000           03  SS02V3-LIT3             PICTURE X(13).
026030150220133528*    03  FILLER                  PICTURE X(2).
026040150213133528     03  FILLER                  PICTURE X(4).
026050000000           03  SS02V3-LIT4             PICTURE X(8).
026060150220133528*    03  FILLER                  PICTURE X(15).
026070150220133528     03  FILLER                  PICTURE X(13).
026080000000
026090000000       01  STMT-SUM-02-V4-R            REDEFINES STMT-SUM-02.
026100000000           03  FILLER                  PICTURE X(38).
026110000000           03  SS02V4-LIT0             PICTURE X(11).
026120000000           03  FILLER                  PICTURE X(32).
026130000000           03  SS02V4-LIT1             PICTURE X(8).
026140000000           03  FILLER                  PICTURE X(3).
026150000000           03  SS02V4-LIT2             PICTURE X(13).
026160000000           03  FILLER                  PICTURE X(3).
026170000000           03  SS02V4-LIT3             PICTURE X(13).
026180000000           03  FILLER                  PICTURE X(4).
026190000000           03  SS02V4-LIT4             PICTURE X(8).
026200000000           03  FILLER                  PICTURE X(17).
026210000000
026220000000       01  STMT-SUM-02-V5-R            REDEFINES STMT-SUM-02.
026230000000           03  FILLER                  PICTURE X(38).
026240000000           03  SS02V5-LIT0             PICTURE X(11).
026250160302155470*    03  FILLER                  PICTURE X(32).
026260160302155470     03  FILLER                  PICTURE X(28).
026270000000           03  SS02V5-LIT1             PICTURE X(8).
026280000000           03  FILLER                  PICTURE X(3).
026290000000           03  SS02V5-LIT2             PICTURE X(13).
026300000000           03  FILLER                  PICTURE X(3).
026310000000           03  SS02V5-LIT3             PICTURE X(13).
026320000000           03  FILLER                  PICTURE X(4).
026330000000           03  SS02V5-LIT4             PICTURE X(8).
026340000000           03  FILLER                  PICTURE X(17).
026350990922
026360990922       01  STMT-SUM-02-V6-R            REDEFINES STMT-SUM-02.
026370000223           03  FILLER                  PICTURE X(48).
026380000223           03  FILLER                  PICTURE X(18).
026390990922           03  SS02V6-LIT1             PICTURE X(8).
026400990925           03  FILLER                  PICTURE X(4).
026410990925           03  SS02V6-LIT2             PICTURE X(13).
026420000222           03  FILLER                  PICTURE X(6).
026430990925           03  SS02V6-LIT3             PICTURE X(13).
026440000223           03  FILLER                  PICTURE X(10).
026450991021
026460991021       01  STMT-SUM-02-V7-R            REDEFINES STMT-SUM-02.
026470150610148012*    03  FILLER                  PICTURE X(37).
026480150610148012     03  FILLER                  PICTURE X(29).
026490991021           03  SS02V7-LIT0             PICTURE X(11).
026500991021           03  FILLER                  PICTURE X(34).
026510991021           03  SS02V7-LIT1             PICTURE X(8).
026520991021           03  FILLER                  PICTURE X(3).
026530991021           03  SS02V7-LIT2             PICTURE X(13).
026540991021           03  FILLER                  PICTURE X(2).
026550991021           03  SS02V7-LIT3             PICTURE X(13).
026560150610148012*    03  FILLER                  PICTURE X(4).
026570150610148012     03  FILLER                  PICTURE X(6).
026580991021           03  SS02V7-LIT4             PICTURE X(8).
026590150610148012*    03  FILLER                  PICTURE X(17).
026600150610148012     03  FILLER                  PICTURE X(15).
026610991213
026620991213       01  STMT-SUM-02-V8-R            REDEFINES STMT-SUM-02.
026630991216           03  FILLER                  PICTURE X(37).
026640991213           03  SS02V8-LIT0             PICTURE X(11).
026650991213           03  FILLER                  PICTURE X(34).
026660991213           03  SS02V8-LIT1             PICTURE X(8).
026670991213           03  FILLER                  PICTURE X(3).
026680991213           03  SS02V8-LIT2             PICTURE X(13).
026690991213           03  FILLER                  PICTURE X(2).
026700991213           03  SS02V8-LIT3             PICTURE X(13).
026710150216133528*    03  FILLER                  PICTURE X(2).
026720150216133528     03  FILLER                  PICTURE X(4).
026730991213           03  SS02V8-LIT4             PICTURE X(8).
026740150216133528*    03  FILLER                  PICTURE X(17).
026750150216133528     03  FILLER                  PICTURE X(15).
026760991213
026770000000      /
026780000000       01  STMT-SUM-03.
026790000000           03  FILLER                  PICTURE X(40).
026800000000           03  SS03-LIT0               PICTURE X(11).
026810050104      *RFS25032 - Begins
026820050104      *    03  FILLER                  PICTURE X(35).
026830050104           03  FILLER                  PICTURE X(22).
026840050104      *RFS25032 - Ends
026850000000           03  SS03-LIT1               PICTURE X(8).
026860000000           03  FILLER                  PICTURE X(3).
026870000000           03  SS03-LIT2               PICTURE X(13).
026880000000           03  FILLER                  PICTURE X(3).
026890000000           03  SS03-LIT3               PICTURE X(13).
026900150212133528*    03  FILLER                  PICTURE X(2).
026910150212133528     03  FILLER                  PICTURE X(6).
026920000000           03  SS03-LIT4               PICTURE X(8).
026930150212133528*    03  FILLER                  PICTURE X(14).
026940150212133528     03  FILLER                  PICTURE X(10).
026950000000
026960000000       01  STMT-SUM-03-V1-R            REDEFINES STMT-SUM-03.
026970160713157301*    03  FILLER                  PICTURE X(37).
026980160713157301     03  FILLER                  PICTURE X(29).
026990000000           03  SS03V1-LIT0             PICTURE X(11).
027000000000           03  FILLER                  PICTURE X(34).
027010000000           03  SS03V1-LIT1             PICTURE X(8).
027020000000           03  FILLER                  PICTURE X(3).
027030000000           03  SS03V1-LIT2             PICTURE X(13).
027040000000           03  FILLER                  PICTURE X(2).
027050000000           03  SS03V1-LIT3             PICTURE X(13).
027060160713157301*    03  FILLER                  PICTURE X(4).
027070160713157301     03  FILLER                  PICTURE X(6).
027080000000           03  SS03V1-LIT4             PICTURE X(8).
027090160713157301*    03  FILLER                  PICTURE X(17).
027100160713157301     03  FILLER                  PICTURE X(15).
027110000000
027120000000       01  STMT-SUM-03-V2-R            REDEFINES STMT-SUM-03.
027130990226           03  FILLER                  PICTURE X(37).
027140990226           03  SS03V2-LIT0             PICTURE X(11).
027150151217154219*    03  FILLER                  PICTURE X(34).
027160151217154219     03  FILLER                  PICTURE X(30).
027170990226           03  SS03V2-LIT1             PICTURE X(8).
027180990226           03  FILLER                  PICTURE X(3).
027190990226           03  SS03V2-LIT2             PICTURE X(13).
027200120913110996     03  FILLER                  PICTURE X(6).
027210990226           03  SS03V2-LIT3             PICTURE X(13).
027220120913110996     03  FILLER                  PICTURE X(1).
027230990226           03  SS03V2-LIT4             PICTURE X(8).
027240120913110996     03  FILLER                  PICTURE X(16).
027250000000
027260000000       01  STMT-SUM-03-V3-R            REDEFINES STMT-SUM-03.
027270000000           03  FILLER                  PICTURE X(39).
027280000000           03  SS03V3-LIT0             PICTURE X(11).
027290000000           03  FILLER                  PICTURE X(35).
027300000000           03  SS03V3-LIT1             PICTURE X(8).
027310000000           03  FILLER                  PICTURE X(3).
027320000000           03  SS03V3-LIT2             PICTURE X(13).
027330000000           03  FILLER                  PICTURE X(3).
027340000000           03  SS03V3-LIT3             PICTURE X(13).
027350000000           03  FILLER                  PICTURE X(2).
027360000000           03  SS03V3-LIT4             PICTURE X(8).
027370000000           03  FILLER                  PICTURE X(15).
027380000000
027390000000       01  STMT-SUM-03-V4-R            REDEFINES STMT-SUM-03.
027400000000           03  FILLER                  PICTURE X(38).
027410000000           03  SS03V4-LIT0             PICTURE X(11).
027420000000           03  FILLER                  PICTURE X(32).
027430000000           03  SS03V4-LIT1             PICTURE X(8).
027440000000           03  FILLER                  PICTURE X(3).
027450000000           03  SS03V4-LIT2             PICTURE X(13).
027460000000           03  FILLER                  PICTURE X(3).
027470000000           03  SS03V4-LIT3             PICTURE X(13).
027480000000           03  FILLER                  PICTURE X(4).
027490000000           03  SS03V4-LIT4             PICTURE X(8).
027500000000           03  FILLER                  PICTURE X(17).
027510000000
027520000000       01  STMT-SUM-03-V5-R            REDEFINES STMT-SUM-03.
027530000000           03  FILLER                  PICTURE X(38).
027540000000           03  SS03V5-LIT0             PICTURE X(11).
027550160303155470*    03  FILLER                  PICTURE X(32).
027560160303155470     03  FILLER                  PICTURE X(28).
027570000000           03  SS03V5-LIT1             PICTURE X(8).
027580000000           03  FILLER                  PICTURE X(3).
027590000000           03  SS03V5-LIT2             PICTURE X(13).
027600000000           03  FILLER                  PICTURE X(3).
027610000000           03  SS03V5-LIT3             PICTURE X(13).
027620000000           03  FILLER                  PICTURE X(4).
027630000000           03  SS03V5-LIT4             PICTURE X(8).
027640000000           03  FILLER                  PICTURE X(17).
027650990922
027660990922       01  STMT-SUM-03-V6-R            REDEFINES STMT-SUM-03.
027670000223           03  FILLER                  PICTURE X(48).
027680000223           03  FILLER                  PICTURE X(18).
027690990922           03  SS03V6-LIT1             PICTURE X(8).
027700990925           03  FILLER                  PICTURE X(4).
027710990925           03  SS03V6-LIT2             PICTURE X(13).
027720000222           03  FILLER                  PICTURE X(6).
027730990925           03  SS03V6-LIT3             PICTURE X(13).
027740000223           03  FILLER                  PICTURE X(10).
027750991021
027760991021       01  STMT-SUM-03-V7-R            REDEFINES STMT-SUM-03.
027770150610148012*    03  FILLER                  PICTURE X(37).
027780150610148012     03  FILLER                  PICTURE X(29).
027790991021           03  SS03V7-LIT0             PICTURE X(11).
027800991021           03  FILLER                  PICTURE X(34).
027810991021           03  SS03V7-LIT1             PICTURE X(8).
027820991021           03  FILLER                  PICTURE X(3).
027830991021           03  SS03V7-LIT2             PICTURE X(13).
027840991021           03  FILLER                  PICTURE X(2).
027850991021           03  SS03V7-LIT3             PICTURE X(13).
027860150610148012*    03  FILLER                  PICTURE X(4).
027870150610148012     03  FILLER                  PICTURE X(6).
027880991021           03  SS03V7-LIT4             PICTURE X(8).
027890150610148012*    03  FILLER                  PICTURE X(17).
027900150610148012     03  FILLER                  PICTURE X(15).
027910991213
027920991213       01  STMT-SUM-03-V8-R            REDEFINES STMT-SUM-03.
027930991216           03  FILLER                  PICTURE X(37).
027940991213           03  SS03V8-LIT0             PICTURE X(11).
027950991213           03  FILLER                  PICTURE X(34).
027960991213           03  SS03V8-LIT1             PICTURE X(8).
027970991213           03  FILLER                  PICTURE X(3).
027980991213           03  SS03V8-LIT2             PICTURE X(13).
027990991213           03  FILLER                  PICTURE X(2).
028000991213           03  SS03V8-LIT3             PICTURE X(13).
028010991213           03  FILLER                  PICTURE X(2).
028020991213           03  SS03V8-LIT4             PICTURE X(8).
028030991213           03  FILLER                  PICTURE X(17).
028040991213
028050000000      /
028060000000       01  STMT-SUM-04.
028070000000           03  SS04-CURRENCY           PICTURE X(3).
028080000000           03  FILLER                  PICTURE X(5).
028090000000           03  SS04-INVESTMENT-NAME.
028100000000               05  FILLER              PICTURE X(27).
028110000000               05  SS04-NET-INVESTED   PICTURE $$$$$,$$$,$$$.99.
028120000000               05  SS04-NET-INVESTED-SIGN
028130160713                                       PICTURE X(1).
028140050104      *RFS25032 - Begins
028150050104      *    03  FILLER                  PICTURE X(34).
028160050104           03  FILLER                  PICTURE X(22).
028170050104      *RFS25032 - Ends
028180000000           03  SS04-LAST-UNIT-PRICE    PICTURE ZZZ.9999.
028190000000           03  SS04-LAST-UNIT-PRICE-SIGN
028200000000                                       PICTURE X(1).
028210000000           03  SS04-NO-OF-UNITS        PICTURE ZZZ,ZZZ,ZZZ.999.
028220000000           03  SS04-NO-OF-UNITS-SIGN   PICTURE X(1).
028230150212133528*    03  SS04-MARKET-VALUE       PICTURE $$$$,$$$,$$$.99.
028240150212133528     03  SS04-MARKET-VALUE       PICTURE $$$$,$$$,$$$,$$$.99.
028250000000           03  SS04-MARKET-VALUE-SIGN  PICTURE X(1).
028260151218      *RFS154219-STARTS
028270151218      *    03  FILLER                  PICTURE X(1).
028280151218           03  FILLER                  PICTURE X(3).
028290151218      *RFS154219-STARTS
028300150918      *RFS149112 STARTS
028310150918      *    03  SS04-ACB-UNIT-PRICE     PICTURE ZZZ.9999.
028320150918           03  SS04-ACB-UNIT-PRICE     PICTURE ZZ,ZZZ.9999.
028330150918      *RFS149112 ENDS
028340000000           03  SS04-ACB-UNIT-PRICE-SIGN
028350000000                                       PICTURE X(1).
028360150212133528*    03  FILLER                  PICTURE X(13).
028370150918      *RFS149112 STARTS
028380150918133528*    03  FILLER                  PICTURE X(10).
028390150918           03  FILLER                  PICTURE X(8).
028400150918      *RFS149112 ENDS
028410000000
028420000000       01  STMT-SUM-04-V1-R            REDEFINES STMT-SUM-04.
028430000000           03  SS04V1-CURRENCY         PICTURE X(3).
028440000000           03  FILLER                  PICTURE X(4).
028450000000           03  SS04V1-INVESTMENT-NAME.
028460000000               05  FILLER              PICTURE X(25).
028470000000               05  SS04V1-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
028480000000               05  SS04V1-NET-INVESTED-SIGN
028490000000                                       PICTURE X(1).
028500160713      *RFS157301 STARTS
028510050104      *RFS25032 - Begins
028520160713157301     03  FILLER                  PICTURE X(25).
028530160713R27421*    03  FILLER                  PICTURE X(33).
028540050427R27421*    03  FILLER                  PICTURE X(23).
028550050104      *RFS25032 - Ends
028560160713      *RFS157301 ENDS
028570000000           03  SS04V1-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
028580000000           03  SS04V1-LAST-UNIT-PRICE-SIGN
028590000000                                       PICTURE X(1).
028600000000           03  SS04V1-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
028610000000           03  SS04V1-NO-OF-UNITS-SIGN PICTURE X(1).
028620150212      *RFS 133528 - BEGINS.
028630150212RF8480*    03  SS04V1-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99.
028640150212           03  SS04V1-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
028650150212      *RFS 133528 - ENDS.
028660000000           03  SS04V1-MARKET-VALUE-SIGN
028670000000                                       PICTURE X(1).
028680151218      *RFS 154219 - STARTS.
028690151218RF8480*    03  FILLER                  PICTURE X(2).
028700151218RF8480     03  FILLER                  PICTURE X(1).
028710151218      *RFS 154219 - ENDS.
028720150918      *RFS149112 STARTS
028730150918      *    03  SS04V1-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
028740150918           03  SS04V1-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
028750150918      *RFS149112 ENDS
028760000000           03  SS04V1-ACB-UNIT-PRICE-SIGN
028770000000                                       PICTURE X(1).
028780150212133528*    03  FILLER                  PICTURE X(16).
028790150918      *RFS149112 STARTS
028800150918133528*    03  FILLER                  PICTURE X(13).
028810150918           03  FILLER                  PICTURE X(11).
028820150918      *RFS149112 ENDS
028830000000       01  STMT-SUM-04-V2-R            REDEFINES STMT-SUM-04.
028840990226           03  SS04V2-CURRENCY         PICTURE X(3).
028850990226           03  FILLER                  PICTURE X(4).
028860990226           03  SS04V2-INVESTMENT-NAME.
028870990226               05  FILLER              PICTURE X(25).
028880990226               05  SS04V2-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
028890990226               05  SS04V2-NET-INVESTED-SIGN
028900990226                                       PICTURE X(1).
028910151217      *RFS154219- START
028920151217      *    03  FILLER                  PICTURE X(33).
028930151217           03  FILLER                  PICTURE X(29).
028940151217      *RFS154219- END
028950990226           03  SS04V2-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
028960990226           03  SS04V2-LAST-UNIT-PRICE-SIGN
028970990226                                       PICTURE X(1).
028980990226           03  SS04V2-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
028990990226           03  SS04V2-NO-OF-UNITS-SIGN PICTURE X(1).
029000030721R18156*    03  SS04V2-MARKET-VALUE     PICTURE $$$,$$$,$$$.99.
029010120913110996*    03  SS04V2-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99.
029020150212      *RFS 133528 - BEGINS
029030150212110996*    03  SS04V2-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99.
029040150212           03  SS04V2-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
029050150212      *RFS 133528 - ENDS.
029060990226           03  SS04V2-MARKET-VALUE-SIGN
029070990226                                       PICTURE X(1).
029080120913110996*    03  FILLER                  PICTURE X(2).
029090150918      *RFS149112 STARTS
029100150918      *    03  SS04V2-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
029110150918           03  SS04V2-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
029120150918      *RFS149112 ENDS
029130990226           03  SS04V2-ACB-UNIT-PRICE-SIGN
029140990226                                       PICTURE X(1).
029150150212133528*    03  FILLER                  PICTURE X(16).
029160150918      *RFS149112 STARTS
029170150918133528*    03  FILLER                  PICTURE X(15).
029180150918           03  FILLER                  PICTURE X(13).
029190150918      *RFS149112 ENDS
029200000000
029210000000       01  STMT-SUM-04-V3-R            REDEFINES STMT-SUM-04.
029220000000           03  SS04V3-CURRENCY         PICTURE X(3).
029230000000           03  FILLER                  PICTURE X(5).
029240000000           03  SS04V3-INVESTMENT-NAME.
029250000000               05  FILLER              PICTURE X(26).
029260000000               05  SS04V3-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
029270000000               05  SS04V3-NET-INVESTED-SIGN
029280000000                                       PICTURE X(1).
029290151218      *RFS154219 STARTS
029300151218      *    03  FILLER                  PICTURE X(34).
029310151218           03  FILLER                  PICTURE X(31).
029320151218      *RFS154219 ENDS
029330000000           03  SS04V3-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
029340000000           03  SS04V3-LAST-UNIT-PRICE-SIGN
029350000000                                       PICTURE X(1).
029360000000           03  SS04V3-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
029370000000           03  SS04V3-NO-OF-UNITS-SIGN PICTURE X(1).
029380150212      *RFS 133528 - BEGINS.
029390150212      *    03  SS04V3-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99.
029400150212           03  SS04V3-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
029410150212      *RFS 133528 - ENDS.
029420000000           03  SS04V3-MARKET-VALUE-SIGN
029430000000                                       PICTURE X(1).
029440000000           03  FILLER                  PICTURE X(1).
029450150918      *RFS149112 STARTS
029460150918      *    03  SS04V3-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
029470150918           03  SS04V3-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
029480150918      *RFS149112 ENDS
029490000000           03  SS04V3-ACB-UNIT-PRICE-SIGN
029500000000                                       PICTURE X(1).
029510150212133528*    03  FILLER                  PICTURE X(14).
029520150918      *RFS149112 STARTS
029530150918133528*    03  FILLER                  PICTURE X(11).
029540150918           03  FILLER                  PICTURE X(9).
029550150918      *RFS149112 ENDS
029560000000
029570000000       01  STMT-SUM-04-V4-R            REDEFINES STMT-SUM-04.
029580000000           03  SS04V4-CURRENCY         PICTURE X(3).
029590000000           03  FILLER                  PICTURE X(4).
029600000000           03  SS04V4-INVESTMENT-NAME.
029610000000               05  FILLER              PICTURE X(26).
029620000000               05  SS04V4-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
029630000000               05  SS04V4-NET-INVESTED-SIGN
029640000000                                       PICTURE X(1).
029650000000           03  FILLER                  PICTURE X(31).
029660000000           03  SS04V4-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
029670000000           03  SS04V4-LAST-UNIT-PRICE-SIGN
029680000000                                       PICTURE X(1).
029690000000           03  SS04V4-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
029700000000           03  SS04V4-NO-OF-UNITS-SIGN PICTURE X(1).
029710150212      *RFS 133528 - BEGINS.
029720150212      *    03  SS04V4-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99.
029730150212           03  SS04V4-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
029740150212      *RFS 133528 - ENDS.
029750000000           03  SS04V4-MARKET-VALUE-SIGN
029760000000                                       PICTURE X(1).
029770151218      *RFS154219 STARTS
029780151218      *    03  FILLER                  PICTURE X(3).
029790151218           03  FILLER                  PICTURE X(1).
029800151218      *RFS154219 ENDS
029810150918      *RFS149112 STARTS
029820150918      *    03  SS04V4-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
029830150918           03  SS04V4-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
029840150918      *RFS149112 ENDS
029850000000           03  SS04V4-ACB-UNIT-PRICE-SIGN
029860000000                                       PICTURE X(1).
029870150212133528*    03  FILLER                  PICTURE X(16).
029880150918      *RFS149112 STARTS
029890150918133528*    03  FILLER                  PICTURE X(13).
029900150918           03  FILLER                  PICTURE X(11).
029910150918      *RFS149112 ENDS
029920000000
029930000000       01  STMT-SUM-04-V5-R            REDEFINES STMT-SUM-04.
029940000000           03  SS04V5-CURRENCY         PICTURE X(3).
029950000000           03  FILLER                  PICTURE X(4).
029960000000           03  SS04V5-INVESTMENT-NAME.
029970000000               05  FILLER              PICTURE X(26).
029980000000               05  SS04V5-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
029990000000               05  SS04V5-NET-INVESTED-SIGN
030000000000                                       PICTURE X(1).
030010020313R13107*    03  FILLER                  PICTURE X(31).
030020160223155470*    03  FILLER                  PICTURE X(29).
030030160223155470     03  FILLER                  PICTURE X(25).
030040020313      *    03  SS04V5-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
030050020313R13107     03  SS04V5-LAST-UNIT-PRICE  PICTURE Z,ZZZ.9999.
030060000000           03  SS04V5-LAST-UNIT-PRICE-SIGN
030070000000                                       PICTURE X(1).
030080000000           03  SS04V5-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
030090000000           03  SS04V5-NO-OF-UNITS-SIGN PICTURE X(1).
030100150212133528*    03  SS04V5-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99.
030110150212133528     03  SS04V5-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
030120000000           03  SS04V5-MARKET-VALUE-SIGN
030130000000                                       PICTURE X(1).
030140020313R13107*    03  FILLER                  PICTURE X(3).
030150020313           03  FILLER                  PICTURE X(1).
030160020313      *    03  SS04V5-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
030170150918      *RFS149112 STARTS
030180150918R13107*    03  SS04V5-ACB-UNIT-PRICE   PICTURE Z,ZZZ.9999.
030190150918           03  SS04V5-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
030200150918      *RFS149112 ENDS
030210000000           03  SS04V5-ACB-UNIT-PRICE-SIGN
030220000000                                       PICTURE X(1).
030230150212133528*    03  FILLER                  PICTURE X(16).
030240150918      *RFS149112 STARTS
030250150918133528*    03  FILLER                  PICTURE X(13).
030260150918           03  FILLER                  PICTURE X(11).
030270150918      *RFS149112 ENDS
030280990922
030290990922       01  STMT-SUM-04-V6-R            REDEFINES STMT-SUM-04.
030300000223           03  SS04V6-CURRENCY         PICTURE X(6).
030310991208           03  FILLER                  PICTURE X(2).
030320990922           03  SS04V6-INVESTMENT-NAME.
030330990922               05  FILLER              PICTURE X(26).
030340990922               05  SS04V6-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
030350990922               05  SS04V6-NET-INVESTED-SIGN
030360990922                                       PICTURE X(1).
030370000223           03  FILLER                  PICTURE X(13).
030380000222           03  SS04V6-LAST-UNIT-PRICE  PICTURE ZZZZZ.9999.
030390990922           03  SS04V6-LAST-UNIT-PRICE-SIGN
030400990922                                       PICTURE X(1).
030410990925           03  FILLER                  PICTURE X(1).
030420990922           03  SS04V6-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
030430990922           03  SS04V6-NO-OF-UNITS-SIGN PICTURE X(1).
030440150212133528*    03  SS04V6-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99.
030450150212133528     03  SS04V6-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
030460990922           03  SS04V6-MARKET-VALUE-SIGN
030470990922                                       PICTURE X(1).
030480150212133528*    03  FILLER                  PICTURE X(8).
030490150212133528     03  FILLER                  PICTURE X(7).
030500991021
030510991021       01  STMT-SUM-04-V7-R            REDEFINES STMT-SUM-04.
030520991021           03  SS04V7-CURRENCY         PICTURE X(3).
030530991021           03  FILLER                  PICTURE X(4).
030540991021           03  SS04V7-INVESTMENT-NAME.
030550991021               05  FILLER              PICTURE X(25).
030560991021               05  SS04V7-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
030570991021               05  SS04V7-NET-INVESTED-SIGN
030580991021                                       PICTURE X(1).
030590150609148012*    03  FILLER                  PICTURE X(33).
030600150609148012     03  FILLER                  PICTURE X(25).
030610991021           03  SS04V7-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
030620991021           03  SS04V7-LAST-UNIT-PRICE-SIGN
030630991021                                       PICTURE X(1).
030640991021           03  SS04V7-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
030650991021           03  SS04V7-NO-OF-UNITS-SIGN PICTURE X(1).
030660150212133528*    03  SS04V7-MARKET-VALUE     PICTURE $$$,$$$,$$$.99.
030670150212133528     03  SS04V7-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
030680991021           03  SS04V7-MARKET-VALUE-SIGN
030690991021                                       PICTURE X(1).
030700991021           03  FILLER                  PICTURE X(3).
030710150918      *RFS149112 STARTS
030720150918      *    03  SS04V7-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
030730150918           03  SS04V7-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
030740150918      *RFS149112 ENDS
030750991021           03  SS04V7-ACB-UNIT-PRICE-SIGN
030760991021                                       PICTURE X(1).
030770150212133528*    03  FILLER                  PICTURE X(16).
030780150918      *RFS149112 STARTS
030790150918133528*    03  FILLER                  PICTURE X(12).
030800150918           03  FILLER                  PICTURE X(10).
030810150918      *RFS149112 ENDS
030820991021
030830991213       01  STMT-SUM-04-V8-R            REDEFINES STMT-SUM-04.
030840991213           03  SS04V8-CURRENCY         PICTURE X(3).
030850991223           03  FILLER                  PICTURE X(7).
030860991213           03  SS04V8-INVESTMENT-NAME.
030870991223               05  FILLER              PICTURE X(22).
030880991213               05  SS04V8-NET-INVESTED PICTURE $$$$$,$$$,$$$.99.
030890991213               05  SS04V8-NET-INVESTED-SIGN
030900991213                                       PICTURE X(1).
030910991213           03  FILLER                  PICTURE X(33).
030920991213           03  SS04V8-LAST-UNIT-PRICE  PICTURE ZZZ.9999.
030930991213           03  SS04V8-LAST-UNIT-PRICE-SIGN
030940991213                                       PICTURE X(1).
030950991213           03  SS04V8-NO-OF-UNITS      PICTURE ZZZ,ZZZ,ZZZ.999.
030960991213           03  SS04V8-NO-OF-UNITS-SIGN PICTURE X(1).
030970150212133528*    03  SS04V8-MARKET-VALUE     PICTURE $$$,$$$,$$$.99.
030980150212133528     03  SS04V8-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99.
030990991213           03  SS04V8-MARKET-VALUE-SIGN
031000991213                                       PICTURE X(1).
031010991213           03  FILLER                  PICTURE X(1).
031020150918      *RFS149112 STARTS
031030150918      *    03  SS04V8-ACB-UNIT-PRICE   PICTURE ZZZ.9999.
031040150918           03  SS04V8-ACB-UNIT-PRICE   PICTURE ZZ,ZZZ.9999.
031050150918      *RFS149112 ENDS
031060991213           03  SS04V8-ACB-UNIT-PRICE-SIGN
031070991213                                       PICTURE X(1).
031080150212133528*    03  FILLER                  PICTURE X(16).
031090150918      *RFS149112 STARTS
031100150918133528*    03  FILLER                  PICTURE X(12).
031110150918           03  FILLER                  PICTURE X(10).
031120150918      *RFS149112 ENDS
031130991213
031140000000      /
031150000000       01  STMT-SUM-05.
031160050104      *RFS25032 - Begins
031170050104      *    03  FILLER                  PICTURE X(113).
031180151218           03  FILLER                  PICTURE X(100).
031190050104      *RFS25032 - Ends
031200000000           03  SS05-LIT1               PICTURE X(14).
031210000000           03  FILLER                  PICTURE X(23).
031220000000
031230000000       01  STMT-SUM-05-V1-R            REDEFINES STMT-SUM-05.
031240160713      *RFS157301 STARTS
031250151218      *RFS154219- Begins
031260151218      *    03  FILLER                  PICTURE X(109).
031270160713157301*    03  FILLER                  PICTURE X(113).
031280160713157301     03  FILLER                  PICTURE X(108).
031290151218      *RFS154219- ENDS
031300000000           03  SS05V1-LIT1             PICTURE X(13).
031310000000           03  FILLER                  PICTURE X(28).
031320000000
031330000000       01  STMT-SUM-05-V2-R            REDEFINES STMT-SUM-05.
031340151218      *RFS154219- Begins
031350151217110996*    03  FILLER                  PICTURE X(112).
031360151218           03  FILLER                  PICTURE X(109).
031370151218      *RFS154219- ENDS
031380990226           03  SS05V2-LIT1             PICTURE X(13).
031390120913110996     03  FILLER                  PICTURE X(25).
031400000000
031410000000       01  STMT-SUM-05-V3-R            REDEFINES STMT-SUM-05.
031420000000           03  FILLER                  PICTURE X(112).
031430000000           03  SS05V3-LIT1             PICTURE X(14).
031440000000           03  FILLER                  PICTURE X(24).
031450000000
031460000000       01  STMT-SUM-05-V4-R            REDEFINES STMT-SUM-05.
031470151218      *RFS154219- Begins
031480151218      *    03  FILLER                  PICTURE X(108).
031490151218           03  FILLER                  PICTURE X(111).
031500151218      *RFS154219- ENDS
031510000000           03  SS05V4-LIT1             PICTURE X(14).
031520000000           03  FILLER                  PICTURE X(28).
031530000000
031540000000       01  STMT-SUM-05-V5-R            REDEFINES STMT-SUM-05.
031550000000           03  FILLER                  PICTURE X(108).
031560000000           03  SS05V5-LIT1             PICTURE X(14).
031570000000           03  FILLER                  PICTURE X(28).
031580990922
031590990922       01  STMT-SUM-05-V6-R            REDEFINES STMT-SUM-05.
031600000223           03  FILLER                  PICTURE X(97).
031610990922           03  SS05V6-LIT1             PICTURE X(14).
031620000223           03  FILLER                  PICTURE X(7).
031630991021
031640991021       01  STMT-SUM-05-V7-R            REDEFINES STMT-SUM-05.
031650150610148012*    03  FILLER                  PICTURE X(109).
031660150610148012     03  FILLER                  PICTURE X(101).
031670991021           03  SS05V7-LIT1             PICTURE X(13).
031680991021           03  FILLER                  PICTURE X(28).
031690991213
031700991213       01  STMT-SUM-05-V8-R            REDEFINES STMT-SUM-05.
031710991216           03  FILLER                  PICTURE X(109).
031720991213           03  SS05V8-LIT1             PICTURE X(13).
031730991216           03  FILLER                  PICTURE X(28).
031740000000      /
031750000000       01  STMT-SUM-06.
031760000000           03  FILLER                  PICTURE X(8).
031770000000           03  SS06-LIT1               PICTURE X(35).
031780050104      *RFS25032 - Begins
031790050104      *    03  FILLER                  PICTURE X(38).
031800050104           03  FILLER                  PICTURE X(26).
031810050104      *RFS25032 - Ends
031820000000           03  SS06-LIT-CURRDESC       PICTURE X(30).
031830150211      * RFS 133528 - begin
031840150211      *    03  SS06-MARKET-VALUE       PICTURE $$$$,$$$,$$$.99-.
031850150211      *    03  FILLER                  PICTURE X(23).
031860150211           03  SS06-MARKET-VALUE       PICTURE $$$$,$$$,$$$,$$$.99-.
031870150211           03  FILLER                  PICTURE X(19).
031880150211      * RFS 133528 - end
031890000000
031900000000       01  STMT-SUM-06-V1-R            REDEFINES STMT-SUM-06.
031910000000           03  FILLER                  PICTURE X(7).
031920000000           03  SS06V1-LIT1             PICTURE X(35).
031930160713157301*    03  FILLER                  PICTURE X(35).
031940160713157301     03  FILLER                  PICTURE X(27).
031950000000           03  SS06V1-LIT-CURRDESC     PICTURE X(30).
031960150211      * RFS 133528 - begin
031970150211      * increase the MV at the Currency Total level
031980150211RF8480*    03  SS06V1-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99-.
031990150211RF8480*    03  FILLER                  PICTURE X(27).
032000150211RF8480     03  SS06V1-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99-.
032010150211RF8480     03  FILLER                  PICTURE X(23).
032020150211      * RFS 133528 - end
032030000000
032040000000       01  STMT-SUM-06-V2-R            REDEFINES STMT-SUM-06.
032050990226           03  FILLER                  PICTURE X(7).
032060990226           03  SS06V2-LIT1             PICTURE X(35).
032070151218      *RFS 154219 - STARTS
032080151217R18156*    03  FILLER                  PICTURE X(33).
032090151218           03  FILLER                  PICTURE X(30).
032100151218      *RFS 154219 - ENDS
032110990226           03  SS06V2-LIT-CURRDESC     PICTURE X(30).
032120120913110996     03  FILLER                  PICTURE X(2).
032130120913110996*    03  SS06V2-MARKET-VALUE     PICTURE $$,$$$,$$$,$$$.99-.
032140120913110996     03  SS06V2-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99-.
032150120913110996     03  FILLER                  PICTURE X(25).
032160000000
032170000000       01  STMT-SUM-06-V3-R            REDEFINES STMT-SUM-06.
032180000000           03  FILLER                  PICTURE X(8).
032190000000           03  SS06V3-LIT1             PICTURE X(35).
032200151218154219*    03  FILLER                  PICTURE X(37).
032210151218154219     03  FILLER                  PICTURE X(34).
032220000000           03  SS06V3-LIT-CURRDESC     PICTURE X(30).
032230150213133528*    03  SS06V3-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99-.
032240150213133528     03  SS06V3-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99-.
032250150213133528*    03  FILLER                  PICTURE X(24).
032260150213133528     03  FILLER                  PICTURE X(20).
032270000000
032280000000       01  STMT-SUM-06-V4-R            REDEFINES STMT-SUM-06.
032290000000           03  FILLER                  PICTURE X(7).
032300000000           03  SS06V4-LIT1             PICTURE X(35).
032310000000           03  FILLER                  PICTURE X(34).
032320000000           03  SS06V4-LIT-CURRDESC     PICTURE X(30).
032330150213133528*    03  SS06V4-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99-.
032340150213133528     03  SS06V4-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99-.
032350150213133528*    03  FILLER                  PICTURE X(28).
032360150213133528     03  FILLER                  PICTURE X(24).
032370000000
032380000000       01  STMT-SUM-06-V5-R            REDEFINES STMT-SUM-06.
032390000000           03  FILLER                  PICTURE X(7).
032400000000           03  SS06V5-LIT1             PICTURE X(35).
032410071127R45948*    03  FILLER                  PICTURE X(34).
032420071127R45948     03  FILLER                  PICTURE X(31).
032430000000           03  SS06V5-LIT-CURRDESC     PICTURE X(30).
032440071126R45948*    03  SS06V5-MARKET-VALUE     PICTURE $$$$,$$$,$$$.99-.
032450071126R45948     03  SS06V5-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99-.
032460071127           03  FILLER                  PICTURE X(28).
032470990922
032480990922       01  STMT-SUM-06-V6-R            REDEFINES STMT-SUM-06.
032490000223           03  FILLER                  PICTURE X(8).
032500990923           03  SS06V6-LIT1             PICTURE X(30).
032510000223           03  FILLER                  PICTURE X(24).
032520000222           03  SS06V6-LIT-CURRDESC     PICTURE X(30).
032530000222           03  SS06V6-MARKET-VALUE     PICTURE $$$,$$$,$$$,$$$.99-.
032540000223           03  FILLER                  PICTURE X(7).
032550991021
032560991021       01  STMT-SUM-06-V7-R            REDEFINES STMT-SUM-06.
032570991021           03  FILLER                  PICTURE X(7).
032580991021           03  SS06V7-LIT1             PICTURE X(35).
032590150609148012*    03  FILLER                  PICTURE X(35).
032600150609148012     03  FILLER                  PICTURE X(27).
032610991021           03  SS06V7-LIT-CURRDESC     PICTURE X(30).
032620150213133528*    03  SS06V7-MARKET-VALUE     PICTURE $$$,$$$,$$$.99-.
032630150213133528     03  SS06V7-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99-.
032640150213133528*    03  FILLER                  PICTURE X(28).
032650150213133528     03  FILLER                  PICTURE X(23).
032660991213
032670991213       01  STMT-SUM-06-V8-R            REDEFINES STMT-SUM-06.
032680991223           03  FILLER                  PICTURE X(10).
032690991213           03  SS06V8-LIT1             PICTURE X(35).
032700991223           03  FILLER                  PICTURE X(32).
032710991213           03  SS06V8-LIT-CURRDESC     PICTURE X(30).
032720150213133528*    03  SS06V8-MARKET-VALUE     PICTURE $$$,$$$,$$$.99-.
032730150213133528     03  SS06V8-MARKET-VALUE     PICTURE $$$$,$$$,$$$,$$$.99-.
032740150213133528*    03  FILLER                  PICTURE X(28).
032750150213133528     03  FILLER                  PICTURE X(23).
032760991213
032770971223      /
032780981207       01  STMT-SUM-07-V1.
032790981207           03  SS07V1-YEAR             PICTURE X(4).
032800981207           03  FILLER                  PICTURE X(3).
032810981224           03  SS07V1-LIT              PICTURE X(25).
032820981229           03  FILLER                  PICTURE X(1).
032830981207           03  SS07V1-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
032840981229           03  FILLER                  PICTURE X(102).
032850991213
032860991213       01  STMT-SUM-07-V8.
032870991213           03  SS07V8-YEAR             PICTURE X(4).
032880000104           03  FILLER                  PICTURE X(6).
032890000104           03  SS07V8-LIT              PICTURE X(25).
032900991213           03  FILLER                  PICTURE X(1).
032910991213           03  SS07V8-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
032920000104           03  FILLER                  PICTURE X(99).
032930981207      /
032940981207       01  STMT-SUM-08-V1.
032950981207           03  SS08V1-YEAR             PICTURE X(4).
032960981207           03  FILLER                  PICTURE X(3).
032970981224           03  SS08V1-LIT              PICTURE X(25).
032980981229           03  FILLER                  PICTURE X(1).
032990981207           03  SS08V1-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
033000981229           03  FILLER                  PICTURE X(102).
033010991213
033020991213       01  STMT-SUM-08-V8.
033030991213           03  SS08V8-YEAR             PICTURE X(4).
033040991223           03  FILLER                  PICTURE X(6).
033050000104           03  SS08V8-LIT              PICTURE X(25).
033060991213           03  FILLER                  PICTURE X(1).
033070991213           03  SS08V8-RRIF-LIF-AMT     PICTURE $$$$,$$$,$$$.99.
033080000104           03  FILLER                  PICTURE X(99).
033090981207      /
033100981207       01  STMT-SUM-11.
033110981207           03  SS11-LIT1               PICTURE X(29).
033120971223           03  FILLER                  PICTURE X(7).
033130981207           03  SS11-CPG-FIXED          PICTURE $$$$,$$$,$$$.99.
033140981207           03  SS11-CPG-FIXED-SIGN     PICTURE X(1).
033150971223           03  FILLER                  PICTURE X(98).
033160971223
033170981207       01  STMT-SUM-11-V1-R            REDEFINES STMT-SUM-11.
033180981207           03  SS11V1-LIT1             PICTURE X(29).
033190971223           03  FILLER                  PICTURE X(4).
033200981207           03  SS11V1-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033210981207           03  SS11V1-CPG-FIXED-SIGN   PICTURE X(1).
033220971223           03  FILLER                  PICTURE X(101).
033230971223
033240981207       01  STMT-SUM-11-V2-R            REDEFINES STMT-SUM-11.
033250990226           03  SS11V2-LIT1             PICTURE X(29).
033260990226           03  FILLER                  PICTURE X(4).
033270990226           03  SS11V2-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033280990226           03  SS11V2-CPG-FIXED-SIGN   PICTURE X(1).
033290990226           03  FILLER                  PICTURE X(101).
033300971223
033310981207       01  STMT-SUM-11-V3-R            REDEFINES STMT-SUM-11.
033320981207           03  SS11V3-LIT1             PICTURE X(29).
033330971223           03  FILLER                  PICTURE X(6).
033340981207           03  SS11V3-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033350981207           03  SS11V3-CPG-FIXED-SIGN   PICTURE X(1).
033360971223           03  FILLER                  PICTURE X(99).
033370971223
033380981207       01  STMT-SUM-11-V4-R            REDEFINES STMT-SUM-11.
033390981207           03  SS11V4-LIT1             PICTURE X(29).
033400971223           03  FILLER                  PICTURE X(5).
033410981207           03  SS11V4-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033420981207           03  SS11V4-CPG-FIXED-SIGN   PICTURE X(1).
033430971223           03  FILLER                  PICTURE X(100).
033440971223
033450981207       01  STMT-SUM-11-V5-R            REDEFINES STMT-SUM-11.
033460981207           03  SS11V5-LIT1             PICTURE X(29).
033470971223           03  FILLER                  PICTURE X(5).
033480981207           03  SS11V5-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033490981207           03  SS11V5-CPG-FIXED-SIGN   PICTURE X(1).
033500971223           03  FILLER                  PICTURE X(100).
033510990922
033520990922       01  STMT-SUM-11-V6-R            REDEFINES STMT-SUM-11.
033530000223           03  FILLER                  PICTURE X(3).
033540991208           03  SS11V6-LIT1             PICTURE X(9).
033550000222           03  FILLER                  PICTURE X(10).
033560990922           03  SS11V6-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033570990922           03  SS11V6-CPG-FIXED-SIGN   PICTURE X(1).
033580000223           03  FILLER                  PICTURE X(80).
033590991021
033600991021       01  STMT-SUM-11-V7-R            REDEFINES STMT-SUM-11.
033610991021           03  SS11V7-LIT1             PICTURE X(29).
033620991021           03  FILLER                  PICTURE X(4).
033630991021           03  SS11V7-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033640991021           03  SS11V7-CPG-FIXED-SIGN   PICTURE X(1).
033650991021           03  FILLER                  PICTURE X(101).
033660991213
033670991213       01  STMT-SUM-11-V8-R            REDEFINES STMT-SUM-11.
033680991213           03  SS11V8-LIT1             PICTURE X(29).
033690991213           03  FILLER                  PICTURE X(4).
033700991213           03  SS11V8-CPG-FIXED        PICTURE $$$$,$$$,$$$.99.
033710991213           03  SS11V8-CPG-FIXED-SIGN   PICTURE X(1).
033720991213           03  FILLER                  PICTURE X(101).
033730991213
033740000000      /
033750000000      ******************************************************************
033760000000      *                                                                *
033770000000      *    S T A T E M E N T   M E S S A G E   P R I N T   L I N E S   *
033780000000      *                                                                *
033790000000      ******************************************************************
033800000000
033810000000       01  STMT-MSG-01.
033820000000           03  FILLER                  PICTURE X(7).
033830000000           03  SM01-STATEMENT-MESSAGE  PICTURE X(115).
033840000000           03  FILLER                  PICTURE X(28).
033850000000      /
033860990623      ******************************************************************
033870990623      *                                                                *
033880990623      *    S T A T E M E N T   H E A D I N G  L I T E R A L S          *
033890990623      *                                                                *
033900990623      ******************************************************************
033910990623      /
033920990623       01  WS-STMT-HDG-LITS.
033930990623
033940990623      *****
033950990623      *****    English Literals.
033960990623      *****
033970990623
033980990623           03  WS-SH-HDGLIT01-E.
033990990623               05  FILLER              PICTURE X(50)       VALUE
034000990623                   "--------------------------------------------------".
034010990623               05  FILLER              PICTURE X(50)       VALUE
034020990623                   "--------------------------------------------------".
034030990623               05  FILLER              PICTURE X(50)       VALUE
034040990623                   "-------------------------------------             ".
034050990923
034060990623           03  WS-SH-HDGLIT02-E.
034070990623               05  FILLER              PICTURE X(50)       VALUE
034080990624                   " TRADE                                       GROSS".
034090990623               05  FILLER              PICTURE X(50)       VALUE
034100990624                   "                   NET INVESTED/      PRICE       ".
034110990623               05  FILLER              PICTURE X(50)       VALUE
034120990624                   "                     UNIT                         ".
034130990623           03  WS-SH-HDGLIT03-E.
034140990623               05  FILLER              PICTURE X(50)       VALUE
034150990624                   " DATE       TRANSACTION TYPE                 AMOUN".
034160990623               05  FILLER              PICTURE X(50)       VALUE
034170990624                   "T      DEDUCTIONS       PROCEEDS   PER UNIT       ".
034180990623               05  FILLER              PICTURE X(50)       VALUE
034190990624                   "    UNITS         BALANCE                         ".
034200990623
034210990623           03  WS-SH-HDGLIT04-E.
034220990623               05  FILLER              PICTURE X(50)       VALUE
034230990623                   "--------------------------------------------------".
034240990623               05  FILLER              PICTURE X(50)       VALUE
034250990623                   "--------------------------------------------------".
034260990623               05  FILLER              PICTURE X(50)       VALUE
034270990623                   "-------------------------------------             ".
034280990623
034290990923           03  WS-SH-HDGLIT06-1-E.
034300000223               05  FILLER              PICTURE X(7).
034310000222               05  FILLER              PICTURE X(40)       VALUE
034320000222                   "----------------------------------------"          .
034330000222               05  FILLER              PICTURE X(40)       VALUE
034340000222                   "----------------------------------------"          .
034350000222               05  FILLER              PICTURE X(24)       VALUE
034360000222                   "------------------------"                          .
034370000223               05  FILLER              PICTURE X(4).
034380990923
034390990922           03  WS-SH-HDGLIT06-2-E.
034400000223               05  FILLER              PICTURE X(8).
034410990923               05  FILLER              PICTURE X(10)  VALUE
034420990923                   "DATE      ".
034430991208               05  FILLER              PICTURE X(1).
034440991207               05  FILLER              PICTURE X(25)  VALUE
034450991207                   "DETAILS                  ".
034460000222               05  FILLER              PICTURE X(3).
034470990923               05  FILLER              PICTURE X(15)   VALUE
034480990924                   "         AMOUNT".
034490000222               05  FILLER              PICTURE X(4).
034500990924               05  FILLER              PICTURE X(8)   VALUE
034510990924                   "    UNIT".
034520990924               05  FILLER              PICTURE X(2).
034530990923               05  FILLER              PICTURE X(15)   VALUE
034540990924                   "          UNITS".
034550000222               05  FILLER              PICTURE X(4).
034560990923               05  FILLER              PICTURE X(15)   VALUE
034570990924                   "         MARKET".
034580000223               05  FILLER              PICTURE X(5).
034590990923
034600990923           03  WS-SH-HDGLIT06-3-E.
034610000223               05  FILLER              PICTURE X(8).
034620990923               05  FILLER              PICTURE X(10)  VALUE
034630990923                   "          ".
034640991208               05  FILLER              PICTURE X(1).
034650991207               05  FILLER              PICTURE X(25)  VALUE
034660991207                   "                         ".
034670000222               05  FILLER              PICTURE X(3).
034680990923               05  FILLER              PICTURE X(15)   VALUE
034690990923                   "               ".
034700000222               05  FILLER              PICTURE X(4).
034710990924               05  FILLER              PICTURE X(8)   VALUE
034720990924                   "   PRICE".
034730990924               05  FILLER              PICTURE X(2).
034740990923               05  FILLER              PICTURE X(15)   VALUE
034750990923                   "               ".
034760000222               05  FILLER              PICTURE X(4).
034770990923               05  FILLER              PICTURE X(15)   VALUE
034780990924                   "          VALUE".
034790000223               05  FILLER              PICTURE X(5).
034800990923
034810990923           03  WS-SH-HDGLIT06-4-E.
034820000223               05  FILLER              PICTURE X(7).
034830000222               05  FILLER              PICTURE X(40)       VALUE
034840000222                   "----------------------------------------"          .
034850000222               05  FILLER              PICTURE X(40)       VALUE
034860000222                   "----------------------------------------"          .
034870000222               05  FILLER              PICTURE X(24)       VALUE
034880000222                   "------------------------"                          .
034890000223               05  FILLER              PICTURE X(4).
034900990922
034910991019           03  WS-SH-HDGLIT07-1-E.
034920991019               05  FILLER              PICTURE X(50)       VALUE
034930991019                   "--------------------------------------------------".
034940991019               05  FILLER              PICTURE X(50)       VALUE
034950991019                   "--------------------------------------------------".
034960991019               05  FILLER              PICTURE X(50)       VALUE
034970991019                   "-------------------------------------             ".
034980991019
034990991019           03  WS-SH-HDGLIT07-2-E.
035000991019               05  FILLER              PICTURE X(8)        VALUE
035010991019                   " TRADE ".
035020991021               05  FILLER              PICTURE X(1).
035030991019               05  FILLER              PICTURE X(25).
035040991020               05  FILLER              PICTURE X(1).
035050991019               05  FILLER              PICTURE X(15)       VALUE
035060991019                   "          GROSS".
035070991022               05  FILLER              PICTURE X(1).
035080991022               05  FILLER              PICTURE X(15).
035090991022               05  FILLER              PICTURE X(1).
035100991019               05  FILLER              PICTURE X(14)       VALUE
035110991019                   " NET INVESTED/".
035120991022               05  FILLER              PICTURE X(1).
035130991019               05  FILLER              PICTURE X(8)        VALUE
035140991019                   "   PRICE"                                          .
035150991022               05  FILLER              PICTURE X(1).
035160991022               05  FILLER              PICTURE X(15).
035170991022               05  FILLER              PICTURE X(1).
035180991022               05  FILLER              PICTURE X(14)       VALUE
035190991022                   "          UNIT".
035200991022               05  FILLER              PICTURE X(1).
035210991022               05  FILLER              PICTURE X(28).
035220991019
035230991019           03  WS-SH-HDGLIT07-3-E.
035240991019               05  FILLER              PICTURE X(8)        VALUE
035250991019                   " DATE  ".
035260991021               05  FILLER              PICTURE X(1).
035270991019               05  FILLER              PICTURE X(25)       VALUE
035280991019                   "TRANSACTION TYPE        ".
035290991020               05  FILLER              PICTURE X(1).
035300991019               05  FILLER              PICTURE X(15)       VALUE
035310991019                   "         AMOUNT".
035320991022               05  FILLER              PICTURE X(1).
035330991022               05  FILLER              PICTURE X(15)       VALUE
035340991022                   "     DEDUCTIONS".
035350991022               05  FILLER              PICTURE X(1).
035360991019               05  FILLER              PICTURE X(14)       VALUE
035370991019                   "      PROCEEDS".
035380991022               05  FILLER              PICTURE X(1).
035390991019               05  FILLER              PICTURE X(8)        VALUE
035400991019                   "PER UNIT"                                          .
035410991022               05  FILLER              PICTURE X(1).
035420991019               05  FILLER              PICTURE X(15)       VALUE
035430991019                   "          UNITS".
035440991022               05  FILLER              PICTURE X(1).
035450991022               05  FILLER              PICTURE X(14)       VALUE
035460991022                   "       BALANCE".
035470991022               05  FILLER              PICTURE X(1).
035480991022               05  FILLER              PICTURE X(28).
035490991019
035500991019           03  WS-SH-HDGLIT07-4-E.
035510991019               05  FILLER              PICTURE X(50)       VALUE
035520991019                   "--------------------------------------------------".
035530991019               05  FILLER              PICTURE X(50)       VALUE
035540991019                   "--------------------------------------------------".
035550991019               05  FILLER              PICTURE X(50)       VALUE
035560991019                   "-------------------------------------             ".
035570991019
035580990623      *****
035590990623      *****    French Literals.
035600990623      *****
035610990623
035620990623           03  WS-SH-HDGLIT01-F.
035630990623               05  FILLER              PICTURE X(50)       VALUE
035640990623                   "--------------------------------------------------".
035650990623               05  FILLER              PICTURE X(50)       VALUE
035660990623                   "--------------------------------------------------".
035670990623               05  FILLER              PICTURE X(50)       VALUE
035680990623                   "-------------------------------------             ".
035690990923
035700990623           03  WS-SH-HDGLIT02-F.
035710990623               05  FILLER              PICTURE X(50)       VALUE
035720990624                   "                                             MONTA".
035730990623               05  FILLER              PICTURE X(50)       VALUE
035740030825                   "NT                   MONTANT NET        PRIX      ".
035750990623               05  FILLER              PICTURE X(50)       VALUE
035760990624                   "                 BALANCE                          ".
035770990623           03  WS-SH-HDGLIT03-F.
035780990623               05  FILLER              PICTURE X(50)       VALUE
035790990624                   " DATE       GENRE DE TRANSACTION             BRUT ".
035800990623               05  FILLER              PICTURE X(50)       VALUE
035810990624                   "       DEDUCTIONS  INVESTI/PRODUIT  UNITAIRE      ".
035820990623               05  FILLER              PICTURE X(50)       VALUE
035830030825                   "    UNITS        DE L'UNITE                       ".
035840990623
035850990623           03  WS-SH-HDGLIT04-F.
035860990623               05  FILLER              PICTURE X(50)       VALUE
035870990623                   "--------------------------------------------------".
035880990623               05  FILLER              PICTURE X(50)       VALUE
035890990623                   "--------------------------------------------------".
035900990623               05  FILLER              PICTURE X(50)       VALUE
035910990623                   "-------------------------------------             ".
035920990623
035930990923           03  WS-SH-HDGLIT06-1-F.
035940000223               05  FILLER              PICTURE X(7).
035950000222               05  FILLER              PICTURE X(40)       VALUE
035960000222                   "----------------------------------------"          .
035970000222               05  FILLER              PICTURE X(40)       VALUE
035980000222                   "----------------------------------------"          .
035990000222               05  FILLER              PICTURE X(24)       VALUE
036000000222                   "------------------------"                          .
036010000223               05  FILLER              PICTURE X(4).
036020000222
036030990923           03  WS-SH-HDGLIT06-2-F.
036040000223               05  FILLER              PICTURE X(8).
036050000222               05  FILLER              PICTURE X(10)  VALUE
036060000222                   "DATE      ".
036070000222               05  FILLER              PICTURE X(1).
036080000222               05  FILLER              PICTURE X(25)  VALUE
036090000222                   "DETAILS                  ".
036100000222               05  FILLER              PICTURE X(3).
036110000222               05  FILLER              PICTURE X(15)   VALUE
036120000222                   "         AMOUNT".
036130000222               05  FILLER              PICTURE X(4).
036140000222               05  FILLER              PICTURE X(8)   VALUE
036150000222                   "    UNIT".
036160000222               05  FILLER              PICTURE X(2).
036170000222               05  FILLER              PICTURE X(15)   VALUE
036180000222                   "          UNITS".
036190000222               05  FILLER              PICTURE X(4).
036200000222               05  FILLER              PICTURE X(15)   VALUE
036210000222                   "         MARKET".
036220000224               05  FILLER              PICTURE X(5).
036230000222
036240990923           03  WS-SH-HDGLIT06-3-F.
036250000223               05  FILLER              PICTURE X(8).
036260000222               05  FILLER              PICTURE X(10)  VALUE
036270000222                   "          ".
036280000222               05  FILLER              PICTURE X(1).
036290000222               05  FILLER              PICTURE X(25)  VALUE
036300000222                   "                         ".
036310000222               05  FILLER              PICTURE X(3).
036320000222               05  FILLER              PICTURE X(15)   VALUE
036330000222                   "               ".
036340000222               05  FILLER              PICTURE X(4).
036350000222               05  FILLER              PICTURE X(8)   VALUE
036360000222                   "   PRICE".
036370000222               05  FILLER              PICTURE X(2).
036380000222               05  FILLER              PICTURE X(15)   VALUE
036390000222                   "               ".
036400000222               05  FILLER              PICTURE X(4).
036410000222               05  FILLER              PICTURE X(15)   VALUE
036420000222                   "          VALUE".
036430000224               05  FILLER              PICTURE X(5).
036440990923
036450990923           03  WS-SH-HDGLIT06-4-F.
036460000223               05  FILLER              PICTURE X(7).
036470000222               05  FILLER              PICTURE X(40)       VALUE
036480000222                   "----------------------------------------"          .
036490000222               05  FILLER              PICTURE X(40)       VALUE
036500000222                   "----------------------------------------"          .
036510000222               05  FILLER              PICTURE X(24)       VALUE
036520000222                   "------------------------"                          .
036530000223               05  FILLER              PICTURE X(4).
036540000222
036550991019           03  WS-SH-HDGLIT07-1-F.
036560991019               05  FILLER              PICTURE X(50)       VALUE
036570991019                   "--------------------------------------------------".
036580991019               05  FILLER              PICTURE X(50)       VALUE
036590991019                   "--------------------------------------------------".
036600991019               05  FILLER              PICTURE X(50)       VALUE
036610991019                   "-------------------------------------             ".
036620991019
036630991019           03  WS-SH-HDGLIT07-2-F.
036640991019               05  FILLER              PICTURE X(8).
036650991021               05  FILLER              PICTURE X(1).
036660991019               05  FILLER              PICTURE X(25).
036670991020               05  FILLER              PICTURE X(1).
036680991019               05  FILLER              PICTURE X(15)       VALUE
036690991019                   "        MONTANT".
036700991020               05  FILLER              PICTURE X(2).
036710991020               05  FILLER              PICTURE X(16).
036720991019               05  FILLER              PICTURE X(15)       VALUE
036730991019                   "    MONTANT NET".
036740991020               05  FILLER              PICTURE X(1).
036750991019               05  FILLER              PICTURE X(8)        VALUE
036760991019                   "    PRIX"                                          .
036770991020               05  FILLER              PICTURE X(19).
036780991019               05  FILLER              PICTURE X(15)       VALUE
036790991019                   "        BALANCE".
036800991021               05  FILLER              PICTURE X(24).
036810991019
036820991019           03  WS-SH-HDGLIT07-3-F.
036830991019               05  FILLER              PICTURE X(8)        VALUE
036840991019                   " DATE  ".
036850991021               05  FILLER              PICTURE X(1).
036860991019               05  FILLER              PICTURE X(25)       VALUE
036870991019                   "GENRE DE TRANSACTION    ".
036880991020               05  FILLER              PICTURE X(1).
036890991019               05  FILLER              PICTURE X(15)       VALUE
036900991019                   "           BRUT".
036910991020               05  FILLER              PICTURE X(2).
036920991019               05  FILLER              PICTURE X(14)       VALUE
036930991019                   "    DEDUCTIONS".
036940991020               05  FILLER              PICTURE X(2).
036950991019               05  FILLER              PICTURE X(15)       VALUE
036960991019                   "INVESTI/PRODUIT".
036970991020               05  FILLER              PICTURE X(1).
036980991019               05  FILLER              PICTURE X(8)        VALUE
036990991019                   "UNITAIRE"                                          .
037000991020               05  FILLER              PICTURE X(2).
037010991019               05  FILLER              PICTURE X(15)       VALUE
037020991019                   "          UNITS".
037030991020               05  FILLER              PICTURE X(2).
037040991019               05  FILLER              PICTURE X(15)       VALUE
037050991019                   "     DE L'UNITE".
037060991021               05  FILLER              PICTURE X(24).
037070991019
037080991019           03  WS-SH-HDGLIT07-4-F.
037090991019               05  FILLER              PICTURE X(50)       VALUE
037100991019                   "--------------------------------------------------".
037110991019               05  FILLER              PICTURE X(50)       VALUE
037120991019                   "--------------------------------------------------".
037130991019               05  FILLER              PICTURE X(50)       VALUE
037140991019                   "-------------------------------------             ".
037150990923
037160990623      *****
037170990623      *****    Statement Heading Literals.
037180990623      *****
037190990623
037200990623       01  WS-STMT-HDG-LITS-R          REDEFINES WS-STMT-HDG-LITS.
037210990623           03  WS-STMT-HDG-LIT-LNG     OCCURS   2  TIMES
037220990623                                       INDEXED BY  SHL.
037230990623               05  WS-SHL01            PICTURE X(150).
037240990623               05  WS-SHL02            PICTURE X(150).
037250990623               05  WS-SHL03            PICTURE X(150).
037260990623               05  WS-SHL04            PICTURE X(150).
037270000222               05  WS-SHL06-1          PICTURE X(115).
037280000222               05  WS-SHL06-2          PICTURE X(115).
037290000222               05  WS-SHL06-3          PICTURE X(115).
037300000222               05  WS-SHL06-4          PICTURE X(115).
037310991019               05  WS-SHL07-1          PICTURE X(150).
037320991019               05  WS-SHL07-2          PICTURE X(150).
037330991019               05  WS-SHL07-3          PICTURE X(150).
037340991019               05  WS-SHL07-4          PICTURE X(150).
037350991019
037360990623      /
037370000000      ******************************************************************
037380000000      *                                                                *
037390000000      *    S T A T E M E N T   L I T E R A L S                         *
037400000000      *                                                                *
037410000000      ******************************************************************
037420000000
037430000000       01  WS-STMT-LITS.
037440000000
037450000000      *****
037460000000      *****    English Literals.
037470000000      *****
037480000000
037490000000           03  WS-SH01-LIT1-E          PICTURE X(20)       VALUE
037500000000               "  TRANSACTIONS FROM ".
037510000000           03  WS-SH01-LIT2-E          PICTURE X(2)        VALUE "TO".
037520990923           03  WS-SH09-LIT1-E          PICTURE X(30)       VALUE
037530990925               "     UNITHOLDER STATEMENT     ".
037540000000           03  WS-SH10-LIT1-E          PICTURE X(47)       VALUE
037550000000               "PLEASE NOTE THAT THIS ACCOUNT IS REGISTERED TO:".
037560990923           03  WS-SH10-LIT2-E          PICTURE X(12)       VALUE
037570990923               "ACCOUNT NO :".
037580000000           03  WS-SD02-LIT1-E          PICTURE X(6)        VALUE
037590000000               "YYMMDD".
037600991019           03  WS-SD02V7-LIT1-E        PICTURE X(8)        VALUE
037610991019               "YYYYMMDD".
037620991213RF6007     03  WS-SD02V8-LIT1-E        PICTURE X(8)        VALUE
037630991213               "YYYYMMDD".
037640000000           03  WS-SS01-LIT1-E.
037650000000               05  FILLER              PICTURE X(49)       VALUE
037660000000                   "* * * * * * * * * * * * * * * * * * * * * * * * *".
037670000000               05  FILLER              PICTURE X(39)       VALUE
037680000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
037690000000               05  FILLER              PICTURE X(50)       VALUE
037700000000                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
037710000000           03  WS-SS01V1-LIT1-E.
037720000000               05  FILLER              PICTURE X(49)       VALUE
037730000000                   "* * * * * * * * * * * * * * * * * * * * * * * * *".
037740000000               05  FILLER              PICTURE X(39)       VALUE
037750000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
037760000000               05  FILLER              PICTURE X(50)       VALUE
037770000000                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
037780000000           03  WS-SS01V2-LIT1-E.
037790990226               05  FILLER              PICTURE X(49)       VALUE
037800990226                   "* * * * * * * * * * * * * * * * * * * * * * * * *".
037810990226               05  FILLER              PICTURE X(39)       VALUE
037820990226                   "    STATEMENT DATE ACCOUNT POSITION    ".
037830990226               05  FILLER              PICTURE X(50)       VALUE
037840990226                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
037850000000           03  WS-SS01V3-LIT1-E.
037860000000               05  FILLER              PICTURE X(49)       VALUE
037870000000                   "* * * * * * * * * * * * * * * * * * * * * * * * *".
037880000000               05  FILLER              PICTURE X(39)       VALUE
037890000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
037900000000               05  FILLER              PICTURE X(50)       VALUE
037910000000                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
037920000000           03  WS-SS01V4-LIT1-E.
037930000000               05  FILLER              PICTURE X(49)       VALUE
037940000000                   "* * * * * * * * * * * * * * * * * * * * * * * * *".
037950000000               05  FILLER              PICTURE X(39)       VALUE
037960000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
037970000000               05  FILLER              PICTURE X(50)       VALUE
037980000000                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
037990000000           03  WS-SS01V5-LIT1-E.
038000000000               05  FILLER              PICTURE X(49)       VALUE
038010000000                   "* * * * * * * * * * * * * * * * * * * * * * * * *".
038020000000               05  FILLER              PICTURE X(39)       VALUE
038030000000                   "    STATEMENT DATE ACCOUNT POSITION    ".
038040000000               05  FILLER              PICTURE X(50)       VALUE
038050000000                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
038060990922           03  WS-SS01V6-LIT1-E.
038070991208               05  FILLER              PICTURE X(35)       VALUE
038080991209                   "* * * * * * * * * * * * * * * * * *".
038090000222               05  FILLER              PICTURE X(36)       VALUE
038100000222                   " * * *   ACCOUNT  SUMMARY   * * * * ".
038110000223               05  FILLER              PICTURE X(37)       VALUE
038120000223                   "* * * * * * * * * * * * * * * * *".
038130991213RF6007     03  WS-SS01V8-LIT1-E.
038140991213               05  FILLER              PICTURE X(51)       VALUE
038150991213                 "* * * * * * * * * * * * * * * * * * * * * * * * * *".
038160991213               05  FILLER              PICTURE X(39)       VALUE
038170991213                   "    STATEMENT DATE ACCOUNT POSITION    ".
038180991213               05  FILLER              PICTURE X(50)       VALUE
038190991213                   "* * * * * * * * * * * * * * * * * * * * * * * * * ".
038200990923
038210000000           03  WS-SS02-LIT0-E          PICTURE X(11)       VALUE
038220000000               "        NET".
038230000000           03  WS-SS02-LIT1-E          PICTURE X(8)        VALUE
038240000000               "    UNIT".
038250000000           03  WS-SS02-LIT2-E          PICTURE X(13)       VALUE
038260000000               "         UNIT".
038270000000           03  WS-SS02-LIT3-E          PICTURE X(13)       VALUE
038280000000               "       MARKET".
038290000000           03  WS-SS02-LIT4-E          PICTURE X(8)        VALUE
038300000000               " AVERAGE".
038310000000           03  WS-SS03-LIT0-E          PICTURE X(11)       VALUE
038320000000               "   INVESTED".
038330000000           03  WS-SS03-LIT1-E          PICTURE X(8)        VALUE
038340000000               "   PRICE".
038350000000           03  WS-SS03-LIT2-E          PICTURE X(13)       VALUE
038360000000               "      BALANCE".
038370000000           03  WS-SS03-LIT3-E          PICTURE X(13)       VALUE
038380000000               "        VALUE".
038390000000           03  WS-SS03-LIT4-E          PICTURE X(8)        VALUE
038400000000               "    COST".
038410000000           03  WS-SS05-LIT1-E          PICTURE X(14)       VALUE
038420000000               " -------------".
038430000000           03  WS-SS05V1-LIT1-E        PICTURE X(13)       VALUE
038440000000               " ------------".
038450990611           03  WS-SS05V2-LIT1-E        PICTURE X(13)       VALUE
038460990226               " ------------".
038470000000           03  WS-SS05V3-LIT1-E        PICTURE X(14)       VALUE
038480000000               " -------------".
038490000000           03  WS-SS05V4-LIT1-E        PICTURE X(14)       VALUE
038500000000               " -------------".
038510000000           03  WS-SS05V5-LIT1-E        PICTURE X(14)       VALUE
038520000000               " -------------".
038530990922           03  WS-SS05V6-LIT1-E        PICTURE X(14)       VALUE
038540990922               " -------------".
038550000000           03  WS-SS06-LIT1-E          PICTURE X(35)       VALUE
038560000000               "ACCOUNT VALUE                      ".
038570981224           03  WS-SS07V1-LIT-E1          PICTURE X(25)       VALUE
038580981224               "MINIMUM RRIF PAYMENT:    ".
038590981224           03  WS-SS07V1-LIT-E2          PICTURE X(25)       VALUE
038600981224               "MINIMUM LIF PAYMENT :    ".
038610981224           03  WS-SS08V1-LIT-E           PICTURE X(25)       VALUE
038620981224               "MAXIMUM LIF PAYMENT :    ".
038630981207           03  WS-SS11-LIT1-E          PICTURE X(29)       VALUE
038640971223               "CANADIAN DOLLAR CAPITAL GAINS".
038650000000
038660000000      *****
038670000000      *****    French Literals.
038680000000      *****
038690000000
038700000000           03  WS-SH01-LIT1-F          PICTURE X(20)       VALUE
038710990611               "LES TRANSACTIONS DE ".
038720000000           03  WS-SH01-LIT2-F          PICTURE X(2)        VALUE "A ".
038730990923           03  WS-SH09-LIT1-F          PICTURE X(30)       VALUE
038740990925               "     UNITHOLDER STATEMENT     ".
038750000000           03  WS-SH10-LIT1-F          PICTURE X(47)       VALUE
038760000000               "VEUILLEZ NOTER CE COMPTE EST ENREGISTRE A      ".
038770990923           03  WS-SH10-LIT2-F          PICTURE X(12)       VALUE
038780990923               "ACCOUNT NO :".
038790000000           03  WS-SD02-LIT1-F          PICTURE X(6)        VALUE
038800000000               "AAMMJJ".
038810991019           03  WS-SD02V7-LIT1-F        PICTURE X(8)        VALUE
038820991019               "YYYYMMDD".
038830991213           03  WS-SD02V8-LIT1-F        PICTURE X(8)        VALUE
038840991213               "YYYYMMDD".
038850000000           03  WS-SS01-LIT1-F.
038860000000               05  FILLER              PICTURE X(47)       VALUE
038870000000                   "* * * * * * * * * * * * * * * * * * * * * * * *".
038880000000               05  FILLER              PICTURE X(43)       VALUE
038890030825R18092             "   Position du compte à la date du relevé  ".
038900000000               05  FILLER              PICTURE X(48)       VALUE
038910000000                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
038920000000           03  WS-SS01V1-LIT1-F.
038930000000               05  FILLER              PICTURE X(47)       VALUE
038940000000                   "* * * * * * * * * * * * * * * * * * * * * * * *".
038950000000               05  FILLER              PICTURE X(43)       VALUE
038960030825R18092             "   Position du compte à la date du relevé  ".
038970000000               05  FILLER              PICTURE X(48)       VALUE
038980000000                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
038990000000           03  WS-SS01V2-LIT1-F.
039000990226               05  FILLER              PICTURE X(47)       VALUE
039010990226                   "* * * * * * * * * * * * * * * * * * * * * * * *".
039020990226               05  FILLER              PICTURE X(43)       VALUE
039030030825R18092             "   Position du compte à la date du relevé  ".
039040990226               05  FILLER              PICTURE X(48)       VALUE
039050990226                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
039060000000           03  WS-SS01V3-LIT1-F.
039070000000               05  FILLER              PICTURE X(47)       VALUE
039080000000                   "* * * * * * * * * * * * * * * * * * * * * * * *".
039090000000               05  FILLER              PICTURE X(43)       VALUE
039100030827R18092             "   Position du compte à la date du relevé  ".
039110000000               05  FILLER              PICTURE X(48)       VALUE
039120000000                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
039130000000           03  WS-SS01V4-LIT1-F.
039140000000               05  FILLER              PICTURE X(47)       VALUE
039150000000                   "* * * * * * * * * * * * * * * * * * * * * * * *".
039160000000               05  FILLER              PICTURE X(43)       VALUE
039170030825R18092             "   Position du compte à la date du relevé  ".
039180000000               05  FILLER              PICTURE X(48)       VALUE
039190000000                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
039200000000           03  WS-SS01V5-LIT1-F.
039210000000               05  FILLER              PICTURE X(47)       VALUE
039220000000                   "* * * * * * * * * * * * * * * * * * * * * * * *".
039230000000               05  FILLER              PICTURE X(43)       VALUE
039240030825R18092             "   Position du compte à la date du relevé  ".
039250000000               05  FILLER              PICTURE X(48)       VALUE
039260000000                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
039270990922           03  WS-SS01V6-LIT1-F.
039280000223               05  FILLER              PICTURE X(35)       VALUE
039290000223                   "* * * * * * * * * * * * * * * * * *".
039300000223               05  FILLER              PICTURE X(36)       VALUE
039310000223                   " * * *   ACCOUNT  SUMMARY   * * * * ".
039320000223               05  FILLER              PICTURE X(37)       VALUE
039330000223                   "* * * * * * * * * * * * * * * * *".
039340000223
039350991213           03  WS-SS01V8-LIT1-F.
039360991213               05  FILLER              PICTURE X(49)       VALUE
039370991213                  "* * * * * * * * * * * * * * * * * * * * * * * * *".
039380991213               05  FILLER              PICTURE X(43)       VALUE
039390030825R18092             "   Position du compte à la date du relevé  ".
039400991213               05  FILLER              PICTURE X(48)       VALUE
039410991213                   "* * * * * * * * * * * * * * * * * * * * * * * * ".
039420990929
039430971223           03  WS-SS02-LIT0-F          PICTURE X(11)       VALUE
039440971223               "    MONTANT".
039450000000           03  WS-SS02-LIT1-F          PICTURE X(8)        VALUE
039460030825R18092         " Prix de".
039470000000           03  WS-SS02-LIT2-F          PICTURE X(13)       VALUE
039480030825R18092         "             ".
039490000000           03  WS-SS02-LIT3-F          PICTURE X(13)       VALUE
039500030825R18092         "       Valeur".
039510000000           03  WS-SS02-LIT4-F          PICTURE X(8)        VALUE
039520030825R18092         "    Coût".
039530000000           03  WS-SS03-LIT0-F          PICTURE X(11)       VALUE
039540000000               "NET INVESTI".
039550000000           03  WS-SS03-LIT1-F          PICTURE X(8)        VALUE
039560030825R18092         " l'unité".
039570000000           03  WS-SS03-LIT2-F          PICTURE X(13)       VALUE
039580030825R18092         "        Solde".
039590000000           03  WS-SS03-LIT3-F          PICTURE X(13)       VALUE
039600030825R18092         "    marchande".
039610000000           03  WS-SS03-LIT4-F          PICTURE X(8)        VALUE
039620030825R18092         "   moyen".
039630000000           03  WS-SS05-LIT1-F          PICTURE X(14)       VALUE
039640000000               " -------------".
039650000000           03  WS-SS05V1-LIT1-F        PICTURE X(13)       VALUE
039660000000               " ------------".
039670990611           03  WS-SS05V2-LIT1-F        PICTURE X(13)       VALUE
039680990226               " ------------".
039690000000           03  WS-SS05V3-LIT1-F        PICTURE X(14)       VALUE
039700000000               " -------------".
039710000000           03  WS-SS05V4-LIT1-F        PICTURE X(14)       VALUE
039720000000               " -------------".
039730000000           03  WS-SS05V5-LIT1-F        PICTURE X(14)       VALUE
039740000000               " -------------".
039750990922           03  WS-SS05V6-LIT1-F        PICTURE X(14)       VALUE
039760990922               " -------------".
039770000000           03  WS-SS06-LIT1-F          PICTURE X(35)       VALUE
039780000000               "VALEUR DU COMPTE                   ".
039790981224           03  WS-SS07V1-LIT-F1          PICTURE X(25)       VALUE
039800981224               "VERSEMENT MINIMUM DE FER:".
039810981224           03  WS-SS07V1-LIT-F2          PICTURE X(25)       VALUE
039820981224               "VERSEMENT MINIMUM DE FRV:".
039830981224           03  WS-SS08V1-LIT-F           PICTURE X(25)       VALUE
039840981224               "VERSEMENT MAXIMUM DE FRV:".
039850981207           03  WS-SS11-LIT1-F          PICTURE X(29)       VALUE
039860971223               "GAINS EN CAPITAL EN CAN $    ".
039870000000
039880000000       01  WS-STMT-LITS-R              REDEFINES WS-STMT-LITS.
039890000000           03  WS-STMT-LIT             OCCURS   2  TIMES
039900000000                                       INDEXED BY  SL.
039910000000
039920000000      *****
039930000000      *****    Heading Literals.
039940000000      *****
039950000000
039960000000               05  WS-SH01-LIT1        PICTURE X(20).
039970000000               05  WS-SH01-LIT2        PICTURE X(2).
039980990923               05  WS-SH09-LIT1        PICTURE X(30).
039990000000               05  WS-SH10-LIT1        PICTURE X(47).
040000990923               05  WS-SH10-LIT2        PICTURE X(12).
040010000000
040020000000      *****
040030000000      *****    Detail Literals.
040040000000      *****
040050000000
040060000000               05  WS-SD02-LIT1        PICTURE X(6).
040070991019               05  WS-SD02V7-LIT1      PICTURE X(8).
040080991213RF6007         05  WS-SD02V8-LIT1      PICTURE X(8).
040090000000
040100000000      *****
040110000000      *****    Summary Literals.
040120000000      *****
040130000000
040140000000               05  WS-SS01-LIT1        PICTURE X(138).
040150000000               05  WS-SS01V1-LIT1      PICTURE X(138).
040160990226               05  WS-SS01V2-LIT1      PICTURE X(138).
040170000000               05  WS-SS01V3-LIT1      PICTURE X(138).
040180000000               05  WS-SS01V4-LIT1      PICTURE X(138).
040190000000               05  WS-SS01V5-LIT1      PICTURE X(138).
040200000223               05  WS-SS01V6-LIT1      PICTURE X(108).
040210991213RF6007         05  WS-SS01V8-LIT1      PICTURE X(140).
040220000000               05  WS-SS02-LIT0        PICTURE X(11).
040230000000               05  WS-SS02-LIT1        PICTURE X(8).
040240000000               05  WS-SS02-LIT2        PICTURE X(13).
040250000000               05  WS-SS02-LIT3        PICTURE X(13).
040260000000               05  WS-SS02-LIT4        PICTURE X(8).
040270000000               05  WS-SS03-LIT0        PICTURE X(11).
040280000000               05  WS-SS03-LIT1        PICTURE X(8).
040290000000               05  WS-SS03-LIT2        PICTURE X(13).
040300000000               05  WS-SS03-LIT3        PICTURE X(13).
040310000000               05  WS-SS03-LIT4        PICTURE X(8).
040320000000               05  WS-SS05-LIT1        PICTURE X(14).
040330000000               05  WS-SS05V1-LIT1      PICTURE X(13).
040340990226               05  WS-SS05V2-LIT1      PICTURE X(13).
040350000000               05  WS-SS05V3-LIT1      PICTURE X(14).
040360000000               05  WS-SS05V4-LIT1      PICTURE X(14).
040370000000               05  WS-SS05V5-LIT1      PICTURE X(14).
040380990922               05  WS-SS05V6-LIT1      PICTURE X(14).
040390000000               05  WS-SS06-LIT1        PICTURE X(35).
040400981224               05  WS-SS07V1-LIT1      PICTURE X(25).
040410981224               05  WS-SS07V1-LIT2      PICTURE X(25).
040420981224               05  WS-SS08V1-LIT       PICTURE X(25).
040430981207               05  WS-SS11-LIT1        PICTURE X(29).
040440000000      /
040450000000      ******************************************************************
040460000000      *                                                                *
040470000000      *    S T A T E M E N T   L I T E R A L S   ( C U R R E N C Y )   *
040480000000      *                                                                *
040490000000      ******************************************************************
040500000000
040510000000       01  WS-CURRENCY-TBL.
040520000000           03  WS-SC-CAD.
040530030827               05  FILLER              PICTURE X(3)        VALUE "CAD".
040540000000               05  FILLER              PICTURE X(30)       VALUE
040550030825R18092             "CANADIAN DOLLAR TOTAL         ".
040560000000               05  FILLER              PICTURE X(30)       VALUE
040570030825R18092             "TOTAL DOLLARS CANADIENS       ".
040580000223      *        05  FILLER              PICTURE X(7).
040590000223               05  FILLER              PICTURE X(8).
040600000000           03  WS-SC-USD.
040610030827               05  FILLER              PICTURE X(3)        VALUE "USD".
040620000000               05  FILLER              PICTURE X(30)       VALUE
040630030825R18092             "US DOLLAR TOTAL               ".
040640000000               05  FILLER              PICTURE X(30)       VALUE
040650030825R18092             "TOTAL DOLLARS AMERICAINS      ".
040660000223      *        05  FILLER              PICTURE X(7).
040670000223               05  FILLER              PICTURE X(8).
040680000000
040690000000       01  WS-CURRENCY-TBL-R           REDEFINES WS-CURRENCY-TBL.
040700030827           03  WS-SC-ENTRY             OCCURS   2  TIMES
040710000000                                       INDEXED BY  SC.
040720000000             04  WS-SC-CURRENCY        PICTURE X(3).
040730000000             04  WS-SC-CURRDESC        OCCURS   2  TIMES
040740000000                                       INDEXED BY  SCL
040750000223                                       PICTURE X(30).
040760000223      *      04  WS-SC-MARKET-VALUE    PICTURE S9(9)V9(2)  COMP-3.
040770000223             04  WS-SC-MARKET-VALUE    PICTURE S9(11)V9(2) COMP-3.
040780000000             04  WS-SC-RPTIND          PICTURE X(1).
040790000000      /
040800000000       01  WS-STMT-INVESTOR-ACCOUNT-TBL.
040810000000
040820000000      ******************************************************************
040830000000      *                                                                *
040840000000      *    S T A T E M E N T   I N V E S T O R / A C C O U N T   T B L *
040850000000      *                                                                *
040860000000      ******************************************************************
040870000000
040880011211           03  WS-SIA-ENT              OCCURS        1000  TIMES
040890000000                                       ASCENDING KEY   IS  WS-SIA-ACCT
040900000000                                       INDEXED         BY  SIA
040910000000                                                           SIA1
040920000000                                                           SIA2.
040930000000               05  WS-SIA-ACCT         PICTURE X(9).
040940000000               05  WS-SIA-NOMCDE       PICTURE X(5).
040950000000               05  WS-SIA-NOMNAM       PICTURE X(35).
040960000000               05  WS-SIA-NOMACCT      PICTURE X(15).
040970000000               05  WS-SIA-REGCTB       PICTURE 9(9)V9(2)   COMP-3.
040980000000               05  WS-SIA-REGCTBSGN    PICTURE X(1).
040990000000      /
041000000000       01  WS-STMT-ACCOUNT-FUND-TABLE.
041010000000
041020000000      ******************************************************************
041030000000      *                                                                *
041040000000      *    S T A T E M E N T   A C C O U N T / F U N D   T A B L E     *
041050000000      *                                                                *
041060000000      ******************************************************************
041070000000
04108001121112090 *    03  WS-STMT-ACCOUNT-FUND    OCCURS 100  TIMES
041090161012      * RFS156599 START
04110016101212090 *    03  WS-STMT-ACCOUNT-FUND    OCCURS 500  TIMES
041110161108164194*    03  WS-STMT-ACCOUNT-FUND    OCCURS 900  TIMES
041120161108164194     03  WS-STMT-ACCOUNT-FUND    OCCURS 2400  TIMES
041130000000                                       INDEXED BY  SAF.
041140161012      * RFS156599 END
041150000000               05  WS-SAF-ACB-UNIT-PRICE
041160000222                                       PICTURE S9(5)V9(4)  COMP-3.
041170000000               05  WS-SAF-ACB-UNIT-PRICE-SIGN
041180000000                                       PICTURE X(1).
041190000000               05  WS-SAF-CURRENCY     PICTURE X(3).
041200000000               05  WS-SAF-FUND-NO      PICTURE X(8).
041210000000               05  WS-SAF-INVESTMENT-NAME
041220000000                                       PICTURE X(35).
041230000000               05  WS-SAF-LAST-UNIT-PRICE
041240000222                                       PICTURE S9(5)V9(4)  COMP-3.
041250000000               05  WS-SAF-LAST-UNIT-PRICE-SIGN
041260000000                                       PICTURE X(1).
041270000222               05  WS-SAF-MARKET-VALUE PICTURE S9(11)V9(2)  COMP-3.
041280000000               05  WS-SAF-MARKET-VALUE-SIGN
041290000000                                       PICTURE X(1).
041300000000               05  WS-SAF-NET-INVESTED PICTURE S9(9)V9(2)  COMP-3.
041310000000               05  WS-SAF-NET-INVESTED-SIGN
041320000000                                       PICTURE X(1).
041330000000               05  WS-SAF-NO-OF-UNITS  PICTURE S9(10)V9(3) COMP-3.
041340000000               05  WS-SAF-NO-OF-UNITS-SIGN
041350000000                                       PICTURE X(1).
041360000000      /
041370991103
041380991103      *****
041390991103      *****    Account Type Table.
041400991103      *****    - This table contains only two fields: account type
041410991103      *****      description and accout type rule.
041420991103      *****
041430991103
041440991103       01  WS-ACCTYP-TBL.
04145001121212090      04  WS-ACCTYP-ENT         OCCURS 250  TIMES
041460991103                                     INDEXED BY  AT1.
041470991103               05  WS-AT-CODE          PICTURE X(5).
041480991103               05  WS-AT-RULE          PICTURE X(1).
041490991103
041500991103       01  WS-AT-LST               PICTURE S9(3)   COMP-3.
041510991103       01  WS-AT-MAX               PICTURE S9(3)   COMP-3
04152001121212090                                                     VALUE 250.
041530020917
041540000000      ******************************************************************
041550000000      *                                                                *
041560000000      *    H E A D I N G   P R I N T   L I N E S                       *
041570000000      *                                                                *
041580000000      ******************************************************************
041590000000
041600000000       01  HEADING-LINE-1.
041610000000           05  FILLER                  PICTURE X(11)       VALUE
041620000000               "DATE     : ".
041630000104           03  HL1-YY                  PICTURE 9(4).
041640000000           03  FILLER                  PICTURE X(1)        VALUE "/".
041650000000           03  HL1-MM                  PICTURE 9(2).
041660000000           03  FILLER                  PICTURE X(1)        VALUE "/".
041670000000           03  HL1-DD                  PICTURE 9(2).
041680000104           03  FILLER                  PICTURE X(93)       VALUE SPACES.
041690000000           03  FILLER                  PICTURE X(6)        VALUE
041700000000               "TIME: ".
041710000000           03  HL1-TIME                PICTURE 99B99B99B99.
041720000000           03  HL1-TIME-R              REDEFINES HL1-TIME.
041730000000               05  FILLER              PICTURE X(8).
041740000000               05  HL1-TIME-DOT        PICTURE X(1).
041750000000               05  FILLER              PICTURE X(2).
041760000000           03  FILLER                  PICTURE X(19)       VALUE SPACES.
041770000000
041780000000       01  HEADING-LINE-2.
041790000000           03  FILLER                  PICTURE X(11)       VALUE
041800000000               "REPORT NO: ".
041810000000           03  FILLER                  PICTURE X(10)       VALUE
041820000000               "STMTPRT   ".
041830000000           03  FILLER                  PICTURE X(15)       VALUE SPACES.
041840000000           03  FILLER                  PICTURE X(30)       VALUE
041850000000               "STATEMENT PRINT CONTROL REPORT".
041860000000           03  FILLER                  PICTURE X(48)       VALUE SPACES.
041870000000           03  FILLER                  PICTURE X(5)        VALUE
041880000000               "PAGE:".
041890000000           03  FILLER                  PICTURE X(8)        VALUE SPACES.
041900000000           03  HL2-PGECNT              PICTURE ZZZZ9.
041910000000           03  FILLER                  PICTURE X(18)       VALUE SPACES.
041920000000
041930000000       01  HEADING-LINE-3.
041940000000           03  FILLER                  PICTURE X(3)        VALUE SPACES.
041950000000           03  FILLER                  PICTURE X(9)        VALUE
041960000000               "INVESTOR#".
041970000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
041980000000           03  FILLER                  PICTURE X(9)        VALUE
041990000000               "ACCOUNT  ".
042000000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
042010000000           03  FILLER                  PICTURE X(9)        VALUE
042020000000               "SECTION  ".
042030000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
042040000000           03  FILLER                  PICTURE X(3)        VALUE "LNG".
042050000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
042060000000           03  FILLER                  PICTURE X(6)        VALUE
042070000000               "DEALER".
042080000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
042090000000           03  FILLER                  PICTURE X(5)        VALUE
042100000000               "REP. ".
042110000000           03  FILLER                  PICTURE X(1)        VALUE SPACES.
042120000000           03  FILLER                  PICTURE X(50)       VALUE
042130000000               "MESSAGE                                           ".
042140000000           03  FILLER                  PICTURE X(50)       VALUE SPACES.
042150000000      /
042160000000      ******************************************************************
042170000000      *                                                                *
042180000000      *    E R R O R   P R I N T   L I N E S
042190000000      *                                                                *
042200000000      ******************************************************************
042210000000
042220000000       01  ERROR-LINE-1.
042230000000           03  FILLER                  PICTURE X(3).
042240000000           03  EL1-INVESTOR-NO         PICTURE 9(9).
042250000000           03  FILLER                  PICTURE X(1).
042260000000           03  EL1-ACCOUNT             PICTURE 9(9).
042270000000           03  FILLER                  PICTURE X(1).
042280000000           03  EL1-SECTION-NO          PICTURE 9(2).
042290000000           03  EL1-SECTION-LITDSH      PICTURE X(1).
042300000000           03  EL1-SECTION-SEQUENCE    PICTURE 9(6).
042310000000           03  FILLER                  PICTURE X(2).
042320000000           03  EL1-LANGUAGE-CODE       PICTURE X(1).
042330000000           03  FILLER                  PICTURE X(2).
042340000000           03  EL1-DEALER              PICTURE X(4).
042350000000           03  FILLER                  PICTURE X(3).
042360000222           03  EL1-DEALER-REP-CODE     PICTURE X(6).
042370000000           03  FILLER                  PICTURE X(1).
042380000000           03  EL1-MSG                 PICTURE X(50).
042390000222           03  FILLER                  PICTURE X(49).
042400000000      /
042410000000      ******************************************************************
042420000000      *                                                                *
042430000000      *    C O N T R O L   P R I N T   L I N E S                       *
042440000000      *                                                                *
042450000000      ******************************************************************
042460000000
042470000000       01  CONTROL-LINE-1.
042480000000           03  FILLER                  PICTURE X(3).
042490000000           03  CL1-LIT                 PICTURE X(50) JUST RIGHT.
042500000000           03  FILLER                  PICTURE X(1).
042510000000           03  CL1-CNT                 PICTURE ZZZ,ZZZ,ZZ9.
042520000000           03  FILLER                  PICTURE X(85).
042530000000
042540000000       01  CONTROL-LINE-2.
042550000000           03  FILLER                  PICTURE X(3)        VALUE SPACES.
042560000000           03  FILLER                  PICTURE X(40)       VALUE
042570000000               "STATEMENTS FOR MANAGEMENT COMPANY CODE: ".
042580000000           03  CL2-MANAGEMENT-COMPANY-CODE
042590000000                                       PICTURE X(3).
042600000000           03  FILLER                  PICTURE X(104)      VALUE SPACES.
042610000000      /
042620000000       01  WS-PGM-DEFIND-WORK-ITEMS.
042630000000
042640000000      ******************************************************************
042650000000      *                                                                *
042660000000      *    P R O G R A M   D E F I N E D   W O R K   I T E M S         *
042670000000      *                                                                *
042680000000      ******************************************************************
042690000000
042700000000      *****
042710000000      *****    Control Report Counters.
042720000000      *****
042730000000
042740000000           03  WS-STMT-DTA-BCNT        PICTURE S9(7)       COMP-3.
042750000000           03  WS-STMT-DTA-BCNT-00     PICTURE S9(7)       COMP-3.
042760000000           03  WS-STMT-DTA-BCNT-05     PICTURE S9(7)       COMP-3.
042770000000           03  WS-STMT-DTA-BCNT-10     PICTURE S9(7)       COMP-3.
042780010308RF8633     03  WS-STMT-DTA-BCNT-12     PICTURE S9(7)       COMP-3.
042790000000           03  WS-STMT-DTA-BCNT-15     PICTURE S9(7)       COMP-3.
042800140624132121     03  WS-STMT-DTA-BCNT-16     PICTURE S9(7)       COMP-3.
042810000000           03  WS-STMT-DTA-BCNT-20     PICTURE S9(7)       COMP-3.
042820060616R35042     03  WS-STMT-DTA-BCNT-21     PICTURE S9(7)       COMP-3.
042830130408117038     03  WS-STMT-DTA-BCNT-22     PICTURE S9(7)       COMP-3.
042840010321RF8633     03  WS-STMT-DTA-BCNT-25     PICTURE S9(7)       COMP-3.
042850991126           03  WS-STMT-DTA-BCNT-30     PICTURE S9(7)       COMP-3.
042860040406R20870     03  WS-STMT-DTA-BCNT-33     PICTURE S9(7)       COMP-3.
042870991126           03  WS-STMT-DTA-BCNT-35     PICTURE S9(7)       COMP-3.
042880040406R20870     03  WS-STMT-DTA-BCNT-38     PICTURE S9(7)       COMP-3.
042890010308RF4638     03  WS-STMT-DTA-BCNT-40     PICTURE S9(7)       COMP-3.
042900010308RF4638     03  WS-STMT-DTA-BCNT-43     PICTURE S9(7)       COMP-3.
042910010308RF4638     03  WS-STMT-DTA-BCNT-45     PICTURE S9(7)       COMP-3.
042920000000           03  WS-STMT-DTA-BCNT-50     PICTURE S9(7)       COMP-3.
042930000000           03  WS-STMT-DTA-BCNT-60     PICTURE S9(7)       COMP-3.
042940991013           03  WS-STMT-DTA-BCNT-62     PICTURE S9(7)       COMP-3.
042950091125R69515     03  WS-STMT-DTA-BCNT-63     PICTURE S9(7)       COMP-3.
042960010323rf8633     03  WS-STMT-DTA-BCNT-67     PICTURE S9(7)       COMP-3.
042970000000           03  WS-STMT-DTA-BCNT-70     PICTURE S9(7)       COMP-3.
042980000000           03  WS-STMT-DTA-BCNT-82     PICTURE S9(7)       COMP-3.
042990991013           03  WS-STMT-DTA-BCNT-84     PICTURE S9(7)       COMP-3.
043000010308RF4638     03  WS-STMT-DTA-BCNT-90     PICTURE S9(7)       COMP-3.
043010000000           03  WS-STMT-DTA-RCNT        PICTURE S9(7)       COMP-3.
043020000000           03  WS-STMT-DTA-SCNT-01     PICTURE S9(7)       COMP-3.
043030000000           03  WS-STMT-DTA-SCNT-05     PICTURE S9(7)       COMP-3.
043040000000           03  WS-STMT-DTA-SCNT-10     PICTURE S9(7)       COMP-3.
043050000000           03  WS-STMT-DTA-SCNT-20     PICTURE S9(7)       COMP-3.
043060981207           03  WS-STMT-DTA-SCNT-60     PICTURE S9(7)       COMP-3.
043070000000           03  WS-STMT-DTA-SCNT-80     PICTURE S9(7)       COMP-3.
043080000000           03  WS-STMT-PRT-DTL-WCNT    PICTURE S9(7)       COMP-3.
043090000000           03  WS-STMT-PRT-HDR-WCNT    PICTURE S9(7)       COMP-3.
043100000000
043110000000      *****
043120000000      *****    Control Report Literals.
043130000000      *****
043140000000
043150000000           03  WS-STMT-DTA-BLIT        PICTURE X(50)       VALUE
043160000000               "STATEMENT DATA RECS BYPASSED:                     ".
043170000000           03  WS-STMT-DTA-BLIT-00     PICTURE X(50)       VALUE
043180000000               "STATEMENT DATA (DELIVERY INFO) RECS BYPASSED:     ".
043190000000           03  WS-STMT-DTA-BLIT-05     PICTURE X(50)       VALUE
043200000000               "STATEMENT DATA (DEALER) RECS BYPASSED:            ".
043210000000           03  WS-STMT-DTA-BLIT-10     PICTURE X(50)       VALUE
043220000000               "STATEMENT DATA (INVESTOR SUMMARY) RECS BYPASSED:  ".
043230010308RF8633     03  WS-STMT-DTA-BLIT-12     PICTURE X(50)       VALUE
043240010308               "STATEMENT DATA (INVES SUMMARY SEC2) RECS BYPASSED:".
043250000000           03  WS-STMT-DTA-BLIT-15     PICTURE X(50)       VALUE
043260000000               "STATEMENT DATA (ACCT ASSET CLASS) RECS BYPASSED:  ".
043270000000           03  WS-STMT-DTA-BLIT-20     PICTURE X(50)       VALUE
043280010321               "STATEMENT DATA (ACCOUNT DETAIL) RECS BYPASSED:    ".
043290060616R35042     03  WS-STMT-DTA-BLIT-21     PICTURE X(50)       VALUE
043300060623R35042         "STATEMENT DATA (ACCOUNT ADDITIONAL INFO) BYPASSED:".
043310130408117038     03  WS-STMT-DTA-BLIT-22     PICTURE X(50)       VALUE
043320130408117038         "STATEMENT DATA (ACCOUNT ADDITIONAL DTL)  BYPASSED:".
043330010321RF8633     03  WS-STMT-DTA-BLIT-25     PICTURE X(50)       VALUE
043340010321RF8633         "STATEMENT DATA (ACCOUNT GIC DETAIL) RECS BYPASSED:".
043350991126           03  WS-STMT-DTA-BLIT-30     PICTURE X(50)       VALUE
043360991126               "STATEMENT DATA (ACC PERF) RECS BYPASSED:          ".
043370040406R20870     03  WS-STMT-DTA-BLIT-33     PICTURE X(50)       VALUE
043380040406               "STATEMENT DATA (ACC ROR)  RECS BYPASSED:          ".
043390991126           03  WS-STMT-DTA-BLIT-35     PICTURE X(50)       VALUE
043400991126               "STATEMENT DATA (ACC FUND PERF) RECS BYPASSED:     ".
043410040406R20870     03  WS-STMT-DTA-BLIT-38     PICTURE X(50)       VALUE
043420040406               "STATEMENT DATA (ACC FUND ROR)  RECS BYPASSED:     ".
043430010308RF4638     03  WS-STMT-DTA-BLIT-40     PICTURE X(50)       VALUE
043440010308               "STATEMENT DATA (SEG FUND CONTR DTL) RECS BYPASSED:".
043450010308           03  WS-STMT-DTA-BLIT-43     PICTURE X(50)       VALUE
043460010308               "STATEMENT DATA (SEG FUND CONTR INV) RECS BYPASSED:".
043470010308           03  WS-STMT-DTA-BLIT-45     PICTURE X(50)       VALUE
043480010308RF4638         "STATEMENT DATA (ANNUITANT DETAILS) RECS BYPASSED: ".
043490000000           03  WS-STMT-DTA-BLIT-50     PICTURE X(50)       VALUE
043500000000               "STATEMENT DATA (ROLLOVER) RECS BYPASSED:          ".
043510000000           03  WS-STMT-DTA-BLIT-60     PICTURE X(50)       VALUE
043520000000               "STATEMENT DATA (RRIF DETAIL) RECS BYPASSED:       ".
043530991013           03  WS-STMT-DTA-BLIT-62     PICTURE X(50)       VALUE
043540991013               "STATEMENT DATA (RESP DETAIL) RECS BYPASSED:       ".
043550091125R69515     03  WS-STMT-DTA-BLIT-63     PICTURE X(50)       VALUE
043560091125R69515         "STATEMENT DATA (RDSP) RECS BYPASSED:              ".
043570010323rf8633     03  WS-STMT-DTA-BLIT-67     PICTURE X(50)       VALUE
043580010323               "STATEMENT DATA (BENIFICIARY INFO) RECS BYPASSED:  ".
043590000000           03  WS-STMT-DTA-BLIT-70     PICTURE X(50)       VALUE
043600000000               "STATEMENT DATA (SYSTEM PLAN INFO) RECS BYPASSED:  ".
043610000000           03  WS-STMT-DTA-BLIT-82     PICTURE X(50)       VALUE
043620000000               "STATEMENT DATA (ACCT TRAN INST DTL) RECS BYPASSED:".
043630991013           03  WS-STMT-DTA-BLIT-84     PICTURE X(50)       VALUE
043640991013               "STATEMENT DATA (RESP TRAN BENE DTL) RECS BYPASSED:".
043650010308RF4638     03  WS-STMT-DTA-BLIT-90     PICTURE X(50)       VALUE
043660010308               "STATEMENT DATA (RESET CONTR DTL) RECS BYPASSED:   ".
043670000000           03  WS-STMT-DTA-RLIT        PICTURE X(50)       VALUE
043680000000               "STATEMENT DATA RECS READ:                         ".
043690000000           03  WS-STMT-DTA-SLIT-01     PICTURE X(50)       VALUE
043700000000               "STATEMENT DATA (INVESTOR INFO) RECS SELECTED:     ".
043710000000           03  WS-STMT-DTA-SLIT-05     PICTURE X(50)       VALUE
043720000000               "STATEMENT DATA (DEALER) RECS SELECTED:            ".
043730000000           03  WS-STMT-DTA-SLIT-10     PICTURE X(50)       VALUE
043740000000               "STATEMENT DATA (INVESTOR SUMMARY) RECS SELECTED:  ".
043750000000           03  WS-STMT-DTA-SLIT-20     PICTURE X(50)       VALUE
043760000000               "STATEMENT DATA (ACCOUNT DETAIL) RECS SELECTED:    ".
043770981207           03  WS-STMT-DTA-SLIT-60     PICTURE X(50)       VALUE
043780981207               "STATEMENT DATA (RRIF/LIF DETAILS) RECS SELECTED:  ".
043790000000           03  WS-STMT-DTA-SLIT-80     PICTURE X(50)       VALUE
043800000000               "STATEMENT DATA (ACCT. TRANSACTION) RECS SELECTED: ".
043810000000           03  WS-STMT-PRT-DTL-WLIT    PICTURE X(50)       VALUE
043820000000               "STATEMENT PRINT DETAIL RECS WRITTEN:              ".
043830000000           03  WS-STMT-PRT-HDR-WLIT    PICTURE X(50)       VALUE
043840000000               "STATEMENT PRINT HEADER RECS WRITTEN:              ".
043850000000
043860000000      *****
043870000000      *****    Error Messages.
043880000000      *****
043890000000
043900000000           03  WS-MSGS.
043910000000               05  WS-MSG-01           PICTURE X(50)       VALUE
043920000000                   "MFASTMTFP SECTION-NO INVALID, RECORD BYPASSED     ".
043930000000               05  WS-MSG-02           PICTURE X(50)       VALUE
043940000000                   "ACCOUNT DETAIL SECTION-NO INVALID, RECORD BYPASSED".
043950000000               05  WS-MSG-03           PICTURE X(50)       VALUE
043960000000                   "ACCOUNT DETAIL CONTAINS INVALID CURRENCY          ".
043970000000               05  WS-MSG-04           PICTURE X(50)       VALUE
043980000000                   "PROGRAM CURRENCY MAXIMUM EXCEEDED                 ".
043990000000               05  WS-MSG-05           PICTURE X(50)       VALUE
044000000000                   "WARNING: MISSING ACCT DTL REC FOR ACCT TRANS FUND ".
044010000000               05  WS-MSG-06           PICTURE X(50)       VALUE
044020000000                   "STATEMENT PRINT HEADER RECORD WRITE FAILED, ABORT ".
044030000000               05  WS-MSG-07           PICTURE X(50)       VALUE
044040000000                   "STATEMENT PRINT DETAIL RECORD WRITE FAILED, ABORT ".
044050000000               05  WS-MSG-08           PICTURE X(50)       VALUE
044060000000                   "MISSING INVESTOR INFORMATION RECORD FOR ACCOUNT   ".
044070000000               05  WS-MSG-09           PICTURE X(50)       VALUE
044080000000                   "MISSING DEALER RECORD FOR ACCOUNT                 ".
044090000000               05  WS-MSG-10           PICTURE X(50)       VALUE
044100000000                   "MISSING ACCOUNT DETAIL RECORD FOR ACCOUNT         ".
044110000000               05  WS-MSG-11           PICTURE X(50)       VALUE
044120000000                   "PROGRAM ACCOUNT MAXIMUM EXCEEDED                  ".
044130000000               05  WS-MSG-12           PICTURE X(50)       VALUE
044140000000                   "WARNING: NOMINEE ACCOUNT WITH NO NOMINEE DATA     ".
044150000000               05  WS-MSG-13           PICTURE X(50)       VALUE
044160000000                   "WARNING: NON-NOMINEE ACCOUNT WITH NOMINEE DATA    ".
044170000000           03  WS-MSGS-R               REDEFINES WS-MSGS.
044180000000               05  WS-MSG              OCCURS  13  TIMES
044190000000                                       INDEXED BY  MSG
044200000000                                       PICTURE X(50).
044210000000
044220000000      *****
044230000000      *****    Format Account Type Description Routine Work Items.
044240000000      *****
044250000000
044260000000           03  WS-ACTTYPGRP            PICTURE X(1).
044270000000
044280000000           03  WS-ACTTYPIN.
044290000000               05  WS-ACTTYPIN-PFX     PICTURE X(35).
044300000000               05  WS-ACTTYPIN-SFX     PICTURE X(2).
044310000000
044320000000           03  WS-ACTTYPLCK            PICTURE X(1).
044330000000           03  WS-ACTTYPLIT1           PICTURE X(13).
044340000000           03  WS-ACTTYPLIT2           PICTURE X(13).
044350000000           03  WS-ACTTYPLIT3           PICTURE X(13).
044360000000           03  WS-ACTTYPLNG            PICTURE X(1).
044370000000           03  WS-ACTTYPNOM            PICTURE X(1).
044380000000           03  WS-ACTTYPOUT            PICTURE X(80).
044390000000           03  WS-ACTTYPSHT            PICTURE X(5).
044400000000           03  WS-ACTTYPSPS            PICTURE X(1).
044410000000
044420000000      *****
044430000000      *****    Index/Subscript Related Items.
044440000000      *****
044450000000
044460161108164194*    03  WS-SAF-LST              PICTURE S9(3)       COMP-3.
044470161108164194     03  WS-SAF-LST              PICTURE S9(4)       COMP-3.
044480161108164194*    03  WS-SAF-MAX              PICTURE S9(3)       COMP-3.
044490161108164194     03  WS-SAF-MAX              PICTURE S9(4)       COMP-3.
044500000000           03  WS-SC-MAX               PICTURE S9(3)       COMP-3.
044510000000           03  WS-SIA-LST              PICTURE S9(5)       COMP-3.
044520000000           03  WS-SIA-MAX              PICTURE S9(5)       COMP-3.
044530000000           03  WS-SPL                  PICTURE S9(3)       COMP-3.
044540000000           03  WS-SPL-DTLFST           PICTURE S9(3)       COMP-3.
044550000000           03  WS-SPL-DTLLST           PICTURE S9(3)       COMP-3.
044560000000           03  WS-SPL-DTLMAX           PICTURE S9(3)       COMP-3.
044570000000           03  WS-SPL-DTLTST           PICTURE S9(3)       COMP-3.
044580000000           03  WS-SPL-HDGFST           PICTURE S9(3)       COMP-3.
044590000000           03  WS-SPL-MAX              PICTURE S9(3)       COMP-3.
044600000000           03  WS-SPL-SH02             PICTURE S9(3)       COMP-3.
044610990927           03  WS-SPL-SH03             PICTURE S9(3)       COMP-3.
044620000000           03  WS-SPML-MAX             PICTURE S9(3)       COMP-3.
044630021213R15028     03  WS-NEW-INVESTOR-FLAG    PICTURE X(1) VALUE " ".
044640000000
044650000000      *****
044660000000      *****    Non-Numeric Items.
044670000000      *****
044680000000
044690990104
044700000000           03  WS-ADRTMP               PICTURE X(40).
044710000000           03  WS-BRANCH-ADDR1         PICTURE X(40).
044720000000           03  WS-BRANCH-ADDR2         PICTURE X(40).
044730000000           03  WS-BRANCH-ADDR3         PICTURE X(40).
044740000000           03  WS-BRANCH-ADDR4         PICTURE X(40).
044750000000           03  WS-BRANCH-ADDR5         PICTURE X(40).
044760000000
044770000000           03  WS-DATE.
044780000104               05  WS-DATE-YYYY        PICTURE 9(4).
044790000000               05  WS-DATE-MM          PICTURE 9(2).
044800000000               05  WS-DATE-DD          PICTURE 9(2).
044810990120
044820990120           03  WS-SYS-DATE             PICTURE 9(8).
044830000000
044840000000           03  WS-DEALER-NAME          PICTURE X(35).
044850000000           03  WS-DEALER-REP-NAMES     PICTURE X(51).
044860000000
044870000000           03  WS-END-DATE.
044880991013             04 WS-SH-END-DATE         PICTURE X(8).
044890991013             04 WS-SH-END-DATE-R       REDEFINES WS-SH-END-DATE.
044900991013               05  WS-SH-END-YY        PICTURE X(2).
044910991013               05  WS-SH-END-DATE-2    PICTURE X(6).
044920000000
044930990923           03  WS-END-DATE-6.
044940990923               05  WS-SH-END-DATE-6    PICTURE X(8).
044950990923
044960000000           03  WS-FAX.
044970000000             04  WS-SH-FAX-01-03       PICTURE 9(3).
044980000000             04  WS-SH-FAX-04-10       PICTURE 9(7).
044990000000             04  WS-SH-FAX-04-10-R     REDEFINES WS-SH-FAX-04-10.
045000000000               05  WS-SH-FAX-04-06     PICTURE X(3).
045010000000               05  WS-SH-FAX-07-10     PICTURE X(4).
045020000000
045030000000           03  WS-FILSTAT              PICTURE X(2).
045040000000
045050000000           03  WS-FUND-NO.
045060000000               05  WS-MANAGEMENT-COMPANY-CODE
045070000000                                       PICTURE X(3).
045080000000               05  FILLER              PICTURE X(5).
045090000000
045100000000           03  WS-NOMINEE              PICTURE X(53).
045110000000           03  WS-NOMINEE-CODE         PICTURE X(5).
045120000000           03  WS-NOMINEE-NAME         PICTURE X(37).
045130000000           03  WS-NOMINEES-ACCT-NO     PICTURE X(15).
045140000000
045150000000           03  WS-PHONE.
045160000000             04  WS-SH-PHONE-01-03     PICTURE 9(3).
045170000000             04  WS-SH-PHONE-04-10     PICTURE 9(7).
045180000000             04  WS-SH-PHONE-04-10-R   REDEFINES WS-SH-PHONE-04-10.
045190000000               05  WS-SH-PHONE-04-06   PICTURE X(3).
045200000000               05  WS-SH-PHONE-07-10   PICTURE X(4).
045210000000
045220000000           03  WS-POSTAL.
045230000000               05  WS-POSTAL1          PICTURE X(3).
045240000000               05  WS-POSTAL2          PICTURE X(7).
045250000000
045260000000           03  WS-POSTAL-OUT.
045270000000               05  WS-POSTAL-OUT1      PICTURE X(3).
045280000000               05  FILLER              PICTURE X(1).
045290000000               05  WS-POSTAL-OUT2      PICTURE X(6).
045300000000
045310000000           03  WS-REP-NAME             PICTURE X(40).
045320990113           03  WS-REP-NAME-1           PICTURE X(40).
045330990113
045340000000           03  WS-SH02-SIN-NO.
045350000000               05  SH02-SIN-NO-1-3     PICTURE X(3).
045360000000               05  SH02-SIN-NO-LITDSH1 PICTURE X(1).
045370000000               05  SH02-SIN-NO-4-6     PICTURE X(3).
045380000000               05  SH02-SIN-NO-LITDSH2 PICTURE X(1).
045390000000               05  SH02-SIN-NO-7-9     PICTURE X(3).
045400000000
045410000000           03  WS-SH04-INVESTOR-ADDRESS1.
045420000000               05  SH04-POSTAL-CODE    PICTURE X(10).
045430000000               05  FILLER              PICTURE X(30).
045440000000
045450000000           03  WS-SH05-INVESTOR-ADDRESS2.
045460000000               05  SH05-POSTAL-CODE    PICTURE X(10).
045470000000               05  FILLER              PICTURE X(30).
045480000000
045490000000           03  WS-SH06-INVESTOR-ADDRESS3.
045500000000               05  SH06-POSTAL-CODE    PICTURE X(10).
045510000000               05  FILLER              PICTURE X(30).
045520000000
045530000000           03  WS-SH07-INVESTOR-ADDRESS4.
045540000000               05  SH07-POSTAL-CODE    PICTURE X(10).
045550000000               05  FILLER              PICTURE X(30).
045560000000
045570000000           03  WS-SH08-INVESTOR-ADDRESS5.
045580000000               05  SH08-POSTAL-CODE    PICTURE X(10).
045590000000               05  FILLER              PICTURE X(30).
045600000000
045610000000           03  WS-SH09-POSTAL-CODE     PICTURE X(10).
045620000000           03  WS-SH10-LIT1X           PICTURE X(47).
045630000000
045640000000           03  WS-SH10-PHONE.
045650000000               05  SH10-PHONE-LITLB    PICTURE X(1).
045660000000               05  SH10-PHONE-01-03    PICTURE X(3).
045670000000               05  SH10-PHONE-LITRB    PICTURE X(1).
045680000000               05  FILLER              PICTURE X(1).
045690000000               05  SH10-PHONE-04-06    PICTURE X(3).
045700000000               05  SH10-PHONE-LITDSH   PICTURE X(1).
045710000000               05  SH10-PHONE-07-10    PICTURE X(4).
045720000000
045730000000           03  WS-SH11-NOMINEE         PICTURE X(53).
045740000000
045750000000           03  WS-SH11-FAX.
045760000000               05  SH11-FAX-LITLB      PICTURE X(1).
045770000000               05  SH11-FAX-01-03      PICTURE X(3).
045780000000               05  SH11-FAX-LITRB      PICTURE X(1).
045790000000               05  FILLER              PICTURE X(1).
045800000000               05  SH11-FAX-04-06      PICTURE X(3).
045810000000               05  SH11-FAX-LITDSH     PICTURE X(1).
045820000000               05  SH11-FAX-07-10      PICTURE X(4).
045830000000
045840000000           03  WS-SIN-NO.
045850000000               05  WS-SIN-NO-1-3       PICTURE X(3).
045860000000               05  WS-SIN-NO-4-6       PICTURE X(3).
045870000000               05  WS-SIN-NO-7-9       PICTURE X(3).
045880000000
045890000000           03  WS-START-DATE.
045900980722               05  WS-SH-START-DATE    PICTURE X(8).
045910000000
045920000000           03  WS-TRADE-DATE.
045930000000               05  FILLER              PICTURE X(2).
045940000000               05  WS-SH-TRADE-DATE    PICTURE X(6).
045950000000
045960990923           03  WS-TRADE-DATE-6.
045970990923               05  WS-SH-TRADE-DATE-6    PICTURE X(8).
045980990923
045990000000      *****
046000000000      *****    Non-Numeric Literals.
046010000000      *****
046020000000
046030000000           03  WS-ACCOUNT-TYPE-SHORT-TOTAL
046040000000                                       PICTURE X(5)        VALUE
046050000000               "TOTAL".
046060000000           03  WS-ACCOUNT-TYPE-TOTAL   PICTURE X(35)       VALUE
046070000000               "TOTAL                              ".
046080000000           03  WS-INVESTMENT-NAME-TOTAL
046090000000                                       PICTURE X(35)       VALUE
046100000000               "TOTAL                              ".
046110000000
046120000000      *****
046130000000      *****    Numeric Items.
046140000000      *****
046150971223
046160971223           03  WS-CPG-FIXED            PICTURE S9(9)V9(2)  COMP-3.
046170971223           03  WS-CPG-FIXED-SIGN       PICTURE X(1).
046180000000
046190971223           03  WS-CPTL-GAIN            PICTURE 9(9)V9(2)   COMP-3.
046200000000           03  WS-CPTL-GAIN-SIGN       PICTURE X(1).
046210000000
046220000000           03  WS-DEDUCTIONS           PICTURE S9(9)V9(2)  COMP-3.
046230000000           03  WS-DEDUCTIONS-SIGN      PICTURE X(1).
046240000000
046250000000           03  WS-LNECNT               PICTURE S9(3)       COMP-3.
046260000000           03  WS-LNEMAX               PICTURE S9(3)       COMP-3.
046270000000           03  WS-NET-INVESTED         PICTURE S9(9)V9(2)  COMP-3.
046280000000           03  WS-PGECNT               PICTURE S9(5)       COMP-3.
046290000000
046300000000           03  WS-REG-CTB-CUR          PICTURE 9(9)V9(2)   COMP-3.
046310000000           03  WS-REG-CTB-CUR-SIGN     PICTURE X(1).
046320000000
046330000000           03  WS-SH-PGENO             PICTURE S9(3)       COMP-3.
046340000000           03  WS-STMTVRSN             PICTURE S9(3)       COMP-3.
046350000000
046360000000      *****
046370000000      *****    Switches.
046380000000      *****
046390000000
046400000000           03  WS-BYPPRT-SW            PICTURE X(1).
046410000000           03  WS-CLTSPR-SW            PICTURE X(1).
046420000000           03  WS-EOJHDG-SW            PICTURE X(1).
046430000000           03  WS-L01BYP-SW            PICTURE X(1).
046440000000           03  WS-PSRERR-SW            PICTURE X(1).
046450000000           03  WS-PSTCDEPRT-SW         PICTURE X(1).
046460000000           03  WS-SELPRT-SW            PICTURE X(1).
046470000000           03  WS-STMT-DTA-SW-01       PICTURE X(1).
046480000000           03  WS-STMT-DTA-SW-05       PICTURE X(1).
046490981207           03  WS-STMT-DTA-SW-60       PICTURE X(1).
046500000000           03  WS-STMT-PRT-HDR-WSW     PICTURE X(1).
046510000000      /
046520000000       01  DUMP-ARGUMENTS.
046530000000
046540000000      ******************************************************************
046550000000      *                                                                *
046560000000      *    D U M P   P A S S   P A R A M E T E R S                     *
046570000000      *                                                                *
046580000000      ******************************************************************
046590000000
046600000000           03  DA-TYPE                 PICTURE X(1)        VALUE "F".
046610000000               88  COBOL-IDENTIFIERS                       VALUE "D".
046620000000               88  FULL-DUMP                               VALUE "F".
046630000000           03  FILLER                  PICTURE X(1).
046640000000           03  DA-PGM-NAME             PICTURE X(10)       VALUE
046650000000               "STMTPRT   ".
046660000000      /
046670000000      ******************************************************************
046680000000      *                                                                *
046690000000      *    C U R R E N T   /   P R E V I O U S   C O N T R O L S       *
046700000000      *                                                                *
046710000000      ******************************************************************
046720000000
046730000000       01  CURR-CONTROLS.
046740000000           03  CURR-LEVEL1.
046750000000             04  CURR-LEVEL2.
046760980128               05  CURR-START-DATE     PICTURE 9(8).
046770980128               05  CURR-END-DATE       PICTURE 9(8).
046780000000               05  CURR-INVESTOR-NO    PICTURE 9(9).
046790000000             04  CURR-ACCOUNT          PICTURE 9(9).
046800000000
046810000000       01  PREV-CONTROLS.
046820000000           03  PREV-LEVEL1.
046830000000             04  PREV-LEVEL2.
046840980128               05  PREV-START-DATE     PICTURE 9(8).
046850980128               05  PREV-END-DATE       PICTURE 9(8).
046860000000               05  PREV-INVESTOR-NO    PICTURE 9(9).
046870000000             04  PREV-ACCOUNT          PICTURE 9(9).
046880000000
046890000000       01  CURR-CTLS-DPR.
046900000000           03  CURR-CD-LEV1.
046910000000               05  CURR-CD-FUND-NO     PICTURE X(8).
046920000000
046930000000       01  PREV-CTLS-DPR.
046940000000           03  PREV-CD-LEV1.
046950000000               05  PREV-CD-FUND-NO     PICTURE X(8).
046960020921
04697002092114588  01    WS-MSTR-ACCOUNT-NO      PICTURE 9(9).
046980190612      *RFS183449-Starts
046990190612       01    Lc-Acct-Type-Code       PICTURE X(5).
047000190612      *RFS183449-Ends
047010000000      /
047020000000       PROCEDURE DIVISION.
047030000000
047040000000      ******************************************************************
047050000000      *                                                                *
047060000000      *    P R O C E D U R E   D I V I S I O N                         *
047070000000      *                                                                *
047080000000      ******************************************************************
047090000000      /
047100000000       MAINLINE.
047110000000
047120000000      ******************************************************************
047130000000      *                                                                *
047140000000      *    M A I N L I N E                                             *
047150000000      *                                                                *
047160000000      ******************************************************************
047170000000
047180000000           PERFORM INITIAL-LOGIC-RTN  THRU ILR-EXIT.
047190000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
047200000000               GO TO MLR-0050.
047210000000
047220000000       MLR-0010.
047230000000           PERFORM GET-CURRENT-RECORD THRU GCR-EXIT.
047240000000           IF  PREV-LEVEL1    IS EQUAL TO  CURR-LEVEL1
047250000000               GO TO MLR-0040.
047260000000
047270000000           IF  PREV-CONTROLS  IS EQUAL TO  LOW-VALUES
047280000000               GO TO MLR-0020.
047290000000
047300000000           PERFORM LEVEL-SUMMARY-1    THRU LS1-EXIT.
047310000000
047320000000           IF  PREV-LEVEL2    IS EQUAL TO  CURR-LEVEL2
047330000000               GO TO MLR-0030.
047340000000
047350000000           PERFORM LEVEL-SUMMARY-2    THRU LS2-EXIT.
047360000000
047370000000       MLR-0020.
047380000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
047390000000               GO TO MLR-0050.
047400000000
047410000000           PERFORM LEVEL-REINIT-2     THRU LR2-EXIT.
047420000000
047430000000       MLR-0030.
047440000000           PERFORM LEVEL-REINIT-1     THRU LR1-EXIT.
047450000000
047460000000       MLR-0040.
047470000000           PERFORM DETAIL-PROCESS-RTN THRU DPR-EXIT.
047480000000           IF  CURR-CONTROLS  IS EQUAL TO  HIGH-VALUES
047490000000               GO TO MLR-0050.
047500000000
047510000000           MOVE CURR-CONTROLS          TO  PREV-CONTROLS.
047520000000           GO TO MLR-0010.
047530000000
047540000000       MLR-0050.
047550000000           PERFORM END-JOB-RTN        THRU EJR-EXIT.
047560000000           STOP RUN.
047570000000      /
047580000000       GET-CURRENT-RECORD.
047590000000
047600000000      ******************************************************************
047610000000      *                                                                *
047620000000      *    G E T   C U R R E N T   R E C O R D   R O U T I N E         *
047630000000      *                                                                *
047640000000      ******************************************************************
047650000000
047660000000           CONTINUE.
047670000000
047680000000      *****
047690000000      *****    Read through the Statement Data records.
047700000000      *****
047710000000
047720000000       GCR-0010.
047730990921
047740000000           READ STMT-DTA-FILE          AT  END
047750000000               MOVE HIGH-VALUES        TO  CURR-CONTROLS
047760990921               GO TO GCR-EXIT
047770990921           END-READ
047780990921
047790000000           MOVE STMT-DTA-RECD          TO  MFASTMTFP-SECTION.
047800000000           ADD 1                       TO  WS-STMT-DTA-RCNT.
047810000000
047820000000      *****
047830000000      *****    Determine which Statement Data record is being processed.
047840000000      *****    If the record cannot be identified, an error message will
047850000000      *****    be printed and the record bypassed.  If this occurs, the
047860000000      *****    program should be changed to either bypass the record or
047870000000      *****    to handle it appropriately.
047880000000      *****
047890000000
047900990921           EVALUATE PR-SECTION-NO
047910990921             WHEN  "00"               GO TO GCR-0020
047920990921             WHEN  "01"               GO TO GCR-0030
047930990921             WHEN  "05"               GO TO GCR-0040
047940990921             WHEN  "10"               GO TO GCR-0050
047950010308RF8633       WHEN  "12"               GO TO GCR-0050-0005
047960990921             WHEN  "15"               GO TO GCR-0050-0010
047970140624132121       WHEN  "16"               GO TO GCR-0050-0016
047980991126             WHEN  "20"               GO TO GCR-0060
047990060616R35042       WHEN  "21"               GO TO GCR-0060-0002
048000130408117038       WHEN  "22"               GO TO GCR-0060-0003
048010010321RF8633       WHEN  "25"               GO TO GCR-0060-0005
048020991126             WHEN  "30"               GO TO GCR-0060-0010
048030040406R20870       WHEN  "33"               GO TO GCR-0060-0033
048040991126             WHEN  "35"               GO TO GCR-0060-0020
048050040406R20870       WHEN  "38"               GO TO GCR-0060-0038
048060010308RF4638       WHEN  "40"               GO TO GCR-0060-0030
048070010308RF4638       WHEN  "43"               GO TO GCR-0060-0040
048080010308RF4638       WHEN  "45"               GO TO GCR-0060-0050
048090990921             WHEN  "50"               GO TO GCR-0070
048100990921             WHEN  "60"               GO TO GCR-0070-0010
048110991013             WHEN  "62"               GO TO GCR-0070-0015
048120091125R69515       WHEN  "63"               GO TO GCR-0070-0016
048130010323RF8633       WHEN  "67"               GO TO GCR-0070-0018
048140990921             WHEN  "70"               GO TO GCR-0070-0020
048150990921             WHEN  "80"               GO TO GCR-0080
048160990921             WHEN  "82"               GO TO GCR-0080-0010
048170991013             WHEN  "84"               GO TO GCR-0080-0020
048180010308RF4638       WHEN  "90"               GO TO GCR-0080-0030
048190990921             WHEN  OTHER
048200990921                   MOVE SPACES                 TO  ERROR-LINE-1
048210990921                   SET MSG                     TO  1
048220990921                   PERFORM PRINT-ERR-LINE      THRU PEL-EXIT
048230000000
048240990921                   ADD 1                       TO  WS-STMT-DTA-BCNT
048250990921                   GO TO GCR-0090
048260990921           END-EVALUATE.
048270990921
048280000000       GCR-0020.
048290000000
048300000000      *****
048310000000      *****    Processing a Delivery Information record, bypass it.
048320000000      *****
048330000000
048340000000           ADD 1                       TO  WS-STMT-DTA-BCNT-00.
048350000000           GO TO GCR-0090.
048360000000
048370000000       GCR-0030.
048380000000
048390000000      *****
048400000000      *****    Processing an Investor Information record, select it.
048410000000      *****
048420000000
048430000000           MOVE MFASTMTFP-SECTION      TO  INVESTOR-INFO-SECTION.
048440000000           ADD 1                       TO  WS-STMT-DTA-SCNT-01.
048450000000           GO TO GCR-0100.
048460000000
048470000000       GCR-0040.
048480000000
048490000000      *****
048500000000      *****    Processing a Dealer record.  The record will be selected
048510000000      *****    unless it is the primary record.
048520000000      *****
048530000000
048540000000           MOVE MFASTMTFP-SECTION      TO  DEALER-SECTION.
048550990921
048560990921           IF  DL-SECTION-SEQ IS GREATER THAN  1
048570000000               ADD 1                   TO  WS-STMT-DTA-BCNT-05
048580990921               GO TO GCR-0090
048590990921           END-IF
048600990921
048610000000           ADD 1                       TO  WS-STMT-DTA-SCNT-05.
048620000000           GO TO GCR-0100.
048630000000
048640000000       GCR-0050.
048650000000
048660000000      *****
048670000000      *****    Processing an Investor Summary record.  The record will
048680000000      *****    be selected unless it is a 'TOTAL' record.
048690000000      *****
048700000000
048710000000           MOVE MFASTMTFP-SECTION      TO  INVESTOR-SUMMARY-SECTION.
048720000000           IF  IS-ACCOUNT-TYPE   EQUAL TO  WS-ACCOUNT-TYPE-TOTAL
048730000000           AND IS-ACCOUNT-TYPE-SHORT
048740000000                              IS EQUAL TO  WS-ACCOUNT-TYPE-SHORT-TOTAL
048750000000               ADD 1                   TO  WS-STMT-DTA-BCNT-10
048760000000               GO TO GCR-0090.
048770020614
048780000000           ADD 1                       TO  WS-STMT-DTA-SCNT-10.
048790000000           GO TO GCR-0100.
048800000000
048810010308RF8633 GCR-0050-0005.
048820010308
048830010308      *****
048840010308      *****    Processing an Investor Summary Section-2 record,
048850010308      *****    bypass it.
048860010308      *****
048870010308
048880010308           MOVE MFASTMTFP-SECTION TO INVESTOR-SUMMARY-SECTION-2.
048890010308           ADD 1                       TO  WS-STMT-DTA-BCNT-12.
048900010308RF8633     GO TO GCR-0090.
048910010308
048920000000       GCR-0050-0010.
048930000000
048940000000      *****
048950000000      *****    Processing an Account Asset Class Summary record, bypass
048960000000      *****    it.
048970000000      *****
048980000000
048990000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ASSET-CLASS-SECTION.
049000000000           ADD 1                       TO  WS-STMT-DTA-BCNT-15.
049010000000           GO TO GCR-0090.
049020000000
049030140624      *RFS132121 - START
049040140624       GCR-0050-0016.
049050140624
049060140624      *****
049070140624      *****    Processing an Account Level Asset Allocation record,
049080140624      *****    bypass it.
049090140624      *****
049100140624
049110140624           MOVE MFASTMTFP-SECTION      TO ACCOUNT-LEVEL-ASSET-ALLOC-DTL.
049120140624           ADD 1                       TO WS-STMT-DTA-BCNT-16.
049130140624           GO TO GCR-0090.
049140140624
049150140624      *RFS132121 - END
049160000000       GCR-0060.
049170000000
049180000000      *****
049190000000      *****    Processing an Account Detail record.  The record will be
049200000000      *****    selected unless it is a 'TOTAL' record.
049210000000      *****
049220000000
049230000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION.
049240000000           IF  AD-INVESTMENT-NAME
049250000000                              IS EQUAL TO  WS-INVESTMENT-NAME-TOTAL
049260000000               ADD 1                   TO  WS-STMT-DTA-BCNT-20
049270000000               GO TO GCR-0090.
049280000000
049290000000           ADD 1                       TO  WS-STMT-DTA-SCNT-20.
049300000000           GO TO GCR-0100.
049310000000
049320060616R35042 GCR-0060-0002.
049330060616  ¦
049340060616  ¦   *****
049350060616  ¦   *****    Processing a ACCOUNT-ADDITIONAL-INFO record, BYPASS IT.
049360060616  ¦   *****
049370060616  ¦
049380130408  ¦        MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ADDITIONAL-INFO.
049390130408  V        ADD 1                       TO  WS-STMT-DTA-BCNT-21.
049400060616R35042     GO TO GCR-0090.
049410060616
049420130408117038 GCR-0060-0003.
049430130408  ¦
049440130408  ¦        MOVE MFASTMTFP-SECTION      TO  ACCOUNT-DETAILS-SECTION-22.
049450130408  V        ADD 1                       TO  WS-STMT-DTA-BCNT-22.
049460130408117038     GO TO GCR-0090.
049470130408
049480010321RF8633 GCR-0060-0005.
049490010321
049500010321      *****
049510010321      *****    Processing an Account GIC Detail record. bypass it.
049520010321      *****
049530010321
049540010321           MOVE MFASTMTFP-SECTION TO ACCOUNT-GIC-DETAILS-SECTION.
049550010321           ADD 1                       TO  WS-STMT-DTA-BCNT-25.
049560010321RF8633     GO TO GCR-0090.
049570010321
049580991126       GCR-0060-0010.
049590991126
049600991126      *****
049610991126      *****    Processing a Account Performance record, bypass it.
049620991126      *****
049630991126
049640991126           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-PERFORMANCE-SECTION.
049650991126           ADD 1                       TO  WS-STMT-DTA-BCNT-30.
049660991126           GO TO GCR-0090.
049670991126
049680040406R20870 GCR-0060-0033.
049690040406
049700040406      *****
049710040406      *****    Processing a Account ROR, bypass it.
049720040406      *****
049730040406
049740040406           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-ROR-FRQ-SECTION.
049750040406           ADD 1                       TO  WS-STMT-DTA-BCNT-33.
049760040406           GO TO GCR-0090.
049770040406
049780991126       GCR-0060-0020.
049790991126
049800991126      *****
049810991126      *****    Processing a Account Fund Performance record, bypass it.
049820991126      *****
049830991126
049840991126           MOVE MFASTMTFP-SECTION      TO ACCOUNT-FUND-PERFORM-SECTION.
049850991126           ADD 1                       TO WS-STMT-DTA-BCNT-35.
049860991126           GO TO GCR-0090.
049870991126
049880040406R20870 GCR-0060-0038.
049890040406
049900040406      *****
049910040406      *****    Processing a Account Fund ROR, bypass it.
049920040406      *****
049930040406
049940040406           MOVE MFASTMTFP-SECTION      TO ACCOUNT-FUND-ROR-FRQ-SECTION.
049950040406           ADD 1                       TO WS-STMT-DTA-BCNT-38.
049960040406           GO TO GCR-0090.
049970040406
049980010308RF4638 GCR-0060-0030.
049990010308
050000010308      *****
050010010308      *****    Processing a Seg Fund Contract Details record,
050020010308      *****    bypass it.
050030010308      *****
050040010308           MOVE MFASTMTFP-SECTION      TO SEG-FUND-CONTRACT-DETAILS.
050050010308           ADD 1                       TO WS-STMT-DTA-BCNT-40.
050060010308           GO TO GCR-0090.
050070010308
050080010308       GCR-0060-0040.
050090010308
050100010308      *****
050110010308      *****    Processing a Seg Fund Contract Investment Details
050120010308      *****    record, bypass it.
050130010308      *****
050140010308
050150010308           MOVE MFASTMTFP-SECTION TO SEG-FUND-CONTRACT-INVESTMENT.
050160010308           ADD 1                  TO WS-STMT-DTA-BCNT-43.
050170010308           GO TO GCR-0090.
050180010308
050190010308       GCR-0060-0050.
050200010308
050210010308      *****
050220010308      *****    Processing a Annuitant Details record, bypass it.
050230010308      *****
050240010308
050250010308           MOVE MFASTMTFP-SECTION      TO ANNUITANT-DETAILS.
050260010308           ADD 1                       TO WS-STMT-DTA-BCNT-45.
050270010308RF4638     GO TO GCR-0090.
050280010308
050290000000       GCR-0070.
050300000000
050310000000      *****
050320000000      *****    Processing a Rollover record, bypass it.
050330000000      *****
050340000000
050350000000           MOVE MFASTMTFP-SECTION      TO  ROLLOVER-SECTION.
050360000000           ADD 1                       TO  WS-STMT-DTA-BCNT-50.
050370000000           GO TO GCR-0090.
050380000000
050390000000       GCR-0070-0010.
050400000000
050410000000      *****
050420981207      *****    Processing a RRIF Detail record, bypass it if
050430981207      *****    RIF and LIF info is not required to print.
050440981207      *****
050450981215
050460000000           MOVE MFASTMTFP-SECTION      TO  RRIF-DETAIL-SECTION.
050470981207
050480981207           IF WS-LDA-RIF-LIF-MSG IS NOT EQUAL TO "Y"
050490981207              ADD 1                    TO  WS-STMT-DTA-BCNT-60
050500981207              GO TO GCR-0090.
050510981207
050520981207           ADD 1    TO WS-STMT-DTA-SCNT-60.
050530981207           GO TO GCR-0100.
050540981207
050550991013       GCR-0070-0015.
050560991013
050570991013      *****
050580991013      *****    Processing a RESP Detail record, bypass it.
050590991013      *****
050600991013
050610991013           MOVE MFASTMTFP-SECTION      TO  RESP-DETAIL-SECTION.
050620991013           ADD 1                       TO  WS-STMT-DTA-BCNT-62.
050630991013           GO TO GCR-0090.
050640091125      *RFS69515 Begin
050650091125       GCR-0070-0016.
050660091125
050670091125      *****
050680091125      *****    Processing a RDSP Detail record, bypass it.
050690091125      *****
050700091125
050710091125           MOVE MFASTMTFP-SECTION      TO  RDSP-DETAIL-SECTION.
050720091125           ADD 1                       TO  WS-STMT-DTA-BCNT-63.
050730091125           GO TO GCR-0090.
050740091125      *RFS69515 End
050750091125
050760010323RF8633 GCR-0070-0018.
050770010323
050780010323      *****
050790010323      *****    Processing a BENIFICIARY INFORMATION record, bypass it.
050800010323      *****
050810010323
050820010323           MOVE MFASTMTFP-SECTION      TO  RESP-DETAIL-SECTION.
050830010323           ADD 1                       TO  WS-STMT-DTA-BCNT-67.
050840010323           GO TO GCR-0090.
050850991013
050860000000       GCR-0070-0020.
050870000000
050880000000      *****
050890000000      *****    Processing a Systematic Plan Info record, bypass it.
050900000000      *****
050910000000
050920000000           MOVE MFASTMTFP-SECTION      TO  SYSTEMATIC-PLAN-INFO-SECTION.
050930000000           ADD 1                       TO  WS-STMT-DTA-BCNT-70.
050940000000           GO TO GCR-0090.
050950000000
050960000000       GCR-0080.
050970000000
050980000000      *****
050990000000      *****    Processing an Account Transaction record, select it.
051000000000      *****
051010000000
051020000000           MOVE MFASTMTFP-SECTION      TO  ACCOUNT-TXN-SECTION.
051030000000           ADD 1                       TO  WS-STMT-DTA-SCNT-80.
051040000000           GO TO GCR-0100.
051050000000
051060000000       GCR-0080-0010.
051070000000
051080000000      *****
051090991013      *****    Processing an RESP Acount transaction record, bypass it.
051100000000      *****
051110000000
051120991013           MOVE MFASTMTFP-SECTION      TO  AT-RESP-DETAIL-SECTION.
051130991013           ADD 1                       TO  WS-STMT-DTA-BCNT-82.
051140000000           GO TO GCR-0090.
051150000000
051160010308       GCR-0080-0020.
051170010308
051180010308      *****
051190010308      *****    Processing an Account Transaction Institution Detail
051200010308      *****    record, bypass it.
051210010308      *****
051220010308
051230010308           MOVE MFASTMTFP-SECTION      TO  AT-INSTITUTION-DETAIL-SECTION.
051240010308           ADD 1                       TO  WS-STMT-DTA-BCNT-84.
051250010308           GO TO GCR-0090.
051260010308
051270010308RF4638 GCR-0080-0030.
051280010308
051290010308      *****
051300010308      *****    Processing a Reset Contract Details record, bypass it.
051310010308      *****
051320010308
051330010308           MOVE MFASTMTFP-SECTION      TO  RESET-CONTRACT-DETAILS.
051340010308           ADD 1                       TO  WS-STMT-DTA-BCNT-90.
051350010308RF4638     GO TO GCR-0090.
051360010308
051370000000       GCR-0090.
051380000000
051390000000      *****
051400000000      *****    The record is to be bypassed.  Therefore, return to read
051410000000      *****    the next record.
051420000000      *****
051430000000
051440000000           GO TO GCR-0010.
051450000000
051460000000       GCR-0100.
051470000000
051480000000      *****
051490000000      *****    The record has been selected and is to be processed.
051500000000      *****    Format the current controls.
051510000000      *****
051520020921
051530020921           MOVE  ZEROES TO WS-MSTR-ACCOUNT-NO.
051540020921R14588     IF WS-LDA-STMT-CNFM-FLAG    IS NOT EQUAL TO "S"
051550020921  ¦           GO TO LR2-0095M
051560020921  ¦        END-IF.
051570020921  ¦
051580020921  ¦        INITIALIZE              DEALER-MISC-DETAILS-REC.
051590020921  ¦        MOVE AD-DEALER          TO DEALER-CODE
051600020921  ¦                                OF DEALER-MISC-DETAILS-REC.
051610020921  ¦
051620020921  ¦        READ DEALER-MISC-DETAILS
051630020921  ¦           INVALID KEY
051640020921  ¦               GO TO LR2-0095M
051650020921  ¦        END-READ.
051660020921  ¦
051670020921  ¦        IF MASTER-ACCT-PROCESS OF DEALER-MISC-DETAILS-REC
051680020921  ¦                                 IS NOT EQUAL TO "Y"
051690020921  ¦               GO TO LR2-0095M
051700020921  ¦        END-IF.
051710020921  ¦
051720020921  ¦        INITIALIZE              MASTER-ACCOUNT-REC.
051730020921  ¦        MOVE AD-ACCOUNT      TO ACCOUNT-NO OF MASTER-ACCOUNT-REC.
051740020921  ¦
051750020921  ¦        READ MASTER-ACCOUNT
051760020921  ¦           INVALID KEY
051770020921  ¦               GO TO LR2-0095M
051780020921  ¦        END-READ.
051790020921  ¦
051800020921  ¦        MOVE MASTER-ACCOUNT-NO OF MASTER-ACCOUNT-REC
051810020921  ¦                                    TO WS-MSTR-ACCOUNT-NO.
051820020921  ¦
051830020921R14588
051840020921
051850020921       LR2-0095M.
051860980210
051870980210           IF PR-START-DATE = SPACES
051880990921              MOVE ZEROES   TO PR-START-DATE
051890990921           END-IF
051900990921
051910980210           IF PR-END-DATE   = SPACES
051920990921              MOVE ZEROES   TO PR-END-DATE
051930990921           END-IF
051940990921
051950980128           MOVE PR-START-DATE          TO  CURR-START-DATE.
051960980128           MOVE PR-END-DATE            TO  CURR-END-DATE.
051970000000           MOVE PR-INVESTOR-NO         TO  CURR-INVESTOR-NO.
051980000000           MOVE PR-ACCOUNT             TO  CURR-ACCOUNT.
051990000000
052000990921      **********
052010990921
052020000000       GCR-EXIT.
052030000000           EXIT.
052040990921
052050990921      **********
052060990921
052070000000      /
052080000000       DETAIL-PROCESS-RTN.
052090000000
052100000000      ******************************************************************
052110000000      *                                                                *
052120000000      *    D E T A I L   P R O C E S S   R O U T I N E                 *
052130000000      *                                                                *
052140000000      ******************************************************************
052150000000
052160000000      *****
052170000000      *****    If an error was detected in a re-initialization routine,
052180000000      *****    bypass processing the record.
052190000000      *****
052200000000
052210000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
052220990922               GO TO DPR-EXIT
052230990922           END-IF
052240990922
052250000000      *****
052260000000      *****    Process the Statement Data record depending on the
052270000000      *****    section number.  If it cannot be identified, an error
052280000000      *****    message will be printed and the record bypassed.  If
052290000000      *****    this occurs, we have a real problem since the GET CURRENT
052300000000      *****    RECORD routine should have bypassed the record.
052310000000      *****
052320000000
052330990922           EVALUATE PR-SECTION-NO
052340990922               WHEN "01"            GO TO  DPR-0010
052350990922               WHEN "05"            GO TO  DPR-0020
052360990922               WHEN "10"            GO TO  DPR-0020-0010
052370990922               WHEN "20"            GO TO  DPR-0030
052380990922               WHEN "60"            GO TO  DPR-0180
052390990922               WHEN "80"            GO TO  DPR-0060
052400990922               WHEN OTHER
052410990922                  MOVE SPACES                 TO  ERROR-LINE-1
052420990922                  SET MSG                     TO  2
052430990922                  PERFORM PRINT-ERR-LINE     THRU PEL-EXIT
052440990922                  GO TO DPR-EXIT
052450990922           END-EVALUATE.
052460990922
052470000000       DPR-0010.
052480000000
052490000000      *****
052500000000      *****    Processing an Investor Information record.  Simply
052510000000      *****    indicate that one has been found for the account, then
052520000000      *****    exit.
052530000000      *****
052540000000
052550000000           MOVE "Y"                    TO  WS-STMT-DTA-SW-01.
052560000000           GO TO DPR-EXIT.
052570000000
052580000000       DPR-0020.
052590000000
052600000000      *****
052610000000      *****    Processing a Dealer record.  Simply indicate that one has
052620000000      *****    been found for the account, then exit.
052630000000      *****
052640000000
052650000000           MOVE "Y"                    TO  WS-STMT-DTA-SW-05.
052660000000           GO TO DPR-EXIT.
052670000000
052680000000       DPR-0020-0010.
052690000000
052700000000      *****
052710000000      *****    Processing an Investor Summary record.  Store the Account
052720000000      *****    information for Nominee and Registered Contribution.  If
052730000000      *****    the table is full, an error message will be printed and
052740000000      *****    the account will not be formatted.  If this occurs, the
052750000000      *****    table occurrence should be increased.  Remember to udpate
052760000000      *****    the setting of WS-SIA-MAX in INITIAL-LOGIC.
052770000000      *****
052780000000
052790000000           IF  WS-SIA-LST   NOT LESS THAN  WS-SIA-MAX
052800000000               MOVE SPACES             TO  ERROR-LINE-1
052810000000               SET MSG                 TO  11
052820000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
052830990922               GO TO DPR-0020-0060
052840990922           END-IF
052850990922
052860000000           IF  WS-SIA-LST     IS EQUAL TO  ZEROES
052870000000               SET SIA                 TO  1
052880990922               GO TO DPR-0020-0050
052890990922           END-IF
052900000000
052910000000           SET SIA                     TO  WS-SIA-LST.
052920990922
052930000000           IF  IS-ACCOUNT2   GREATER THAN  WS-SIA-ACCT (SIA)
052940000000               SET SIA              UP BY  1
052950990922               GO TO DPR-0020-0050
052960990922           END-IF
052970000000
052980000000           SET SIA                     TO  1.
052990000000
053000000000       DPR-0020-0020.
053010990922
053020000000           IF  IS-ACCOUNT2   IS LESS THAN  WS-SIA-ACCT (SIA)
053030990922               GO TO DPR-0020-0030
053040990922           END-IF
053050000000
053060000000           SET SIA                  UP BY  1.
053070990922
053080000000           IF  SIA    IS NOT GREATER THAN  WS-SIA-LST
053090990922               GO TO DPR-0020-0020
053100990922           END-IF.
053110000000
053120000000       DPR-0020-0030.
053130990922
053140000000           SET SIA1                    TO  WS-SIA-LST.
053150000000           SET SIA2                    TO  WS-SIA-LST.
053160990922           SET SIA2                    UP BY  1.
053170000000
053180000000       DPR-0020-0040.
053190990922
053200000000           IF  SIA1      IS NOT LESS THAN  SIA
053210000000               MOVE WS-SIA-ENT (SIA1)  TO  WS-SIA-ENT (SIA2)
053220000000               SET SIA1           DOWN BY  1
053230000000               SET SIA2           DOWN BY  1
053240990922               GO TO DPR-0020-0040
053250990922           END-IF.
053260000000
053270000000       DPR-0020-0050.
053280990922
053290000000           MOVE IS-ACCOUNT2            TO  WS-SIA-ACCT (SIA).
053300990922
053310990922           IF IS-NOMINEE     IS EQUAL  TO  "Y"
053320990922              MOVE IS-NOMINEE-CODE     TO  WS-SIA-NOMCDE (SIA)
053330990922              MOVE IS-NOMINEE-NAME     TO  WS-SIA-NOMNAM (SIA)
053340990922              MOVE IS-NOMINEES-ACCT-NO TO  WS-SIA-NOMACCT (SIA)
053350990922           ELSE
053360990922              MOVE SPACES              TO  WS-SIA-NOMCDE (SIA)
053370990922              MOVE SPACES              TO  WS-SIA-NOMNAM (SIA)
053380990922              MOVE SPACES              TO  WS-SIA-NOMACCT (SIA)
053390990922           END-IF
053400990922
053410000000           MOVE IS-REG-CTB-CUR         TO  WS-SIA-REGCTB (SIA).
053420000000           MOVE IS-REG-CTB-CUR-SIGN    TO  WS-SIA-REGCTBSGN (SIA).
053430000000           ADD 1                       TO  WS-SIA-LST.
053440000000
053450000000       DPR-0020-0060.
053460990922
053470000000           GO TO DPR-EXIT.
053480000000
053490000000       DPR-0030.
053500000000
053510000000      *****
053520000000      *****    Processing an Account Detail record.  These records will
053530000000      *****    be processed, after the Account Transaction records have
053540000000      *****    been processed, by the LEVEL SUMMARIZATION 1 routine.
053550000000      *****    For now, accumulate the fund market value by currency and
053560000000      *****    store the data.  If the currency cannot be identified, an
053570000000      *****    error message will be printed and the record bypassed.
053580000000      *****    If this occurs, the program should be changed to handle
053590000000      *****    the new currency.  Remember to update the setting of
053600000000      *****    the setting of WS-SC-MAX in INITIAL-LOGIC.
053610000000      *****
053620000000
053630000000           SET SC                      TO  1.
053640000000
053650000000       DPR-0040.
053660990922
053670000000           IF  AD-CURRENCY    IS EQUAL TO  WS-SC-CURRENCY (SC)
053680990922               GO TO DPR-0050
053690990922           END-IF
053700990922
053710000000           IF  SC            IS LESS THAN  WS-SC-MAX
053720000000               SET SC               UP BY  1
053730990922               GO TO DPR-0040
053740990922           END-IF
053750990922
053760000000           MOVE SPACES                 TO  ERROR-LINE-1.
053770000000           SET MSG                     TO  3.
053780000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
053790000000           GO TO DPR-EXIT.
053800000000
053810000000       DPR-0050.
053820990922
053830990922           IF AD-MARKET-VALUE-SIGN     IS EQUAL TO  SPACES
053840990922              ADD AD-MARKET-VALUE      TO  WS-SC-MARKET-VALUE (SC)
053850990922           ELSE
053860990922              SUBTRACT AD-MARKET-VALUE  FROM WS-SC-MARKET-VALUE (SC)
053870990922           END-IF
053880990922
053890000000           MOVE "Y"                    TO  WS-SC-RPTIND (SC).
053900000000
053910000000      *****
053920000000      *****    Format the fund in the Statement Account Fund table.  If
053930000000      *****    the table is full, an error message will be printed and
053940000000      *****    the record bypassed.  If this occurs, the program should
053950000000      *****    be changed to handle more funds for an account.  Remember
053960000000      *****    to update the setting of WS-SAF-MAX in INITIAL-LOGIC.
053970000000      *****
053980000000
053990000000           IF  WS-SAF-LST   NOT LESS THAN  WS-SAF-MAX
054000000000               MOVE SPACES             TO  ERROR-LINE-1
054010000000               SET MSG                 TO  4
054020000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
054030990922               GO TO DPR-EXIT
054040990922           END-IF
054050990922
054060000000      *****
054070000000      *****    Calculate the net invested, if required.
054080000000      *****
054090000000
054100000000           MOVE ZEROES                 TO  WS-NET-INVESTED.
054110000000
054120990922           IF  WS-LDA-PRT-NET-INVEST   IS NOT EQUAL TO  "Y"
054130990922               GO TO DPR-0050-0010
054140990922           END-IF
054150990922
054160000000           IF  AD-PI-SIGN     IS EQUAL TO  "-"
054170990922               SUBTRACT AD-PUR-INCEPT  FROM  WS-NET-INVESTED
054180990922           ELSE
054190990922               ADD AD-PUR-INCEPT       TO  WS-NET-INVESTED
054200990922           END-IF
054210990922
054220000000           IF  AD-RI-SIGN     IS EQUAL TO  "-"
054230000000               ADD AD-RED-INCEPT       TO  WS-NET-INVESTED
054240990922           ELSE
054250990922               SUBTRACT AD-RED-INCEPT  FROM  WS-NET-INVESTED
054260990922           END-IF
054270990922
054280000000           IF  AD-TI-SIGN     IS EQUAL TO  "-"
054290990922               SUBTRACT AD-TRI-INCEPT  FROM  WS-NET-INVESTED
054300990922           ELSE
054310990922               ADD AD-TRI-INCEPT       TO  WS-NET-INVESTED
054320990922           END-IF
054330990922
054340000000           IF  AD-OI-SIGN     IS EQUAL TO  "-"
054350000000               ADD AD-TRO-INCEPT       TO  WS-NET-INVESTED
054360990922           ELSE
054370990922               SUBTRACT AD-TRO-INCEPT  FROM  WS-NET-INVESTED
054380990922           END-IF.
054390020801
054400020806R13637     SUBTRACT AD-SWO-INCEPT  FROM  WS-NET-INVESTED.
054410020806R13637     ADD      AD-SWI-INCEPT  TO    WS-NET-INVESTED.
054420000000
054430000000       DPR-0050-0010.
054440990922
054450000000           ADD 1                       TO  WS-SAF-LST.
054460000000           SET SAF                     TO  WS-SAF-LST.
054470000000           MOVE AD-ACB-UNIT-PRICE      TO  WS-SAF-ACB-UNIT-PRICE (SAF).
054480000000           MOVE AD-ACB-UNIT-PRICE-SIGN TO  WS-SAF-ACB-UNIT-PRICE-SIGN
054490000000                                             (SAF).
054500030908R18092     IF AD-LANGUAGE = "E"
054510030908   |          MOVE AD-CURRENCY            TO  WS-SAF-CURRENCY (SAF)
054520030908   |       END-IF.
054530030908   |
054540030908   |       IF AD-LANGUAGE = "F"
054550030908   |          MOVE AD-CURRENCY            TO  WS-CURRENCY-XXX
054560030908   |          MOVE SPACES                 TO  WS-CURRENCY-XXX-W
054570030908   |          MOVE WS-CURRENCY-XX         TO  WS-CURRENCY-XX-W
054580030908   |          MOVE WS-CURRENCY-PIC3       TO  WS-SAF-CURRENCY (SAF)
054590030908   V       END-IF.
054600030908
054610000000           MOVE AD-FUND-NO             TO  WS-SAF-FUND-NO (SAF).
054620000000           MOVE AD-INVESTMENT-NAME     TO  WS-SAF-INVESTMENT-NAME (SAF).
054630000000           MOVE AD-LAST-UNIT-PRICE     TO  WS-SAF-LAST-UNIT-PRICE (SAF).
054640000000           MOVE AD-LAST-UNIT-PRICE-SIGN
054650000000                                       TO  WS-SAF-LAST-UNIT-PRICE-SIGN
054660000000                                             (SAF).
054670000000           MOVE AD-MARKET-VALUE        TO  WS-SAF-MARKET-VALUE (SAF).
054680000000           MOVE AD-MARKET-VALUE-SIGN   TO  WS-SAF-MARKET-VALUE-SIGN
054690000000                                             (SAF).
054700000000           IF  WS-NET-INVESTED  LESS THAN  ZEROES
054710000000               SUBTRACT WS-NET-INVESTED
054720000000                                     FROM  ZEROES
054730000000                                   GIVING  WS-SAF-NET-INVESTED (SAF)
054740000000               MOVE "-"                TO  WS-SAF-NET-INVESTED-SIGN
054750000000                                             (SAF)
054760990922           ELSE
054770000000               MOVE WS-NET-INVESTED    TO  WS-SAF-NET-INVESTED (SAF)
054780000000               MOVE SPACES             TO  WS-SAF-NET-INVESTED-SIGN
054790990922                                             (SAF)
054800990922           END-IF
054810990922
054820000000           MOVE AD-NO-OF-UNITS         TO  WS-SAF-NO-OF-UNITS (SAF).
054830000000           MOVE AD-NO-OF-UNITS-SIGN    TO  WS-SAF-NO-OF-UNITS-SIGN
054840000000                                             (SAF).
054850971223
054860971223      *****
054870971223      *****    Accumulate the Canadian dollar capital gains amount.
054880971223      *****
054890971223
054900990922           IF  AD-CPG-FIXED-SIGN EQUAL  TO  " "
054910990922               ADD AD-CPG-FIXED         TO  WS-CPG-FIXED
054920990922           ELSE
054930990922               SUBTRACT AD-CPG-FIXED    FROM  WS-CPG-FIXED
054940990922           END-IF
054950990922
054960000000           GO TO DPR-EXIT.
054970000000
054980000000       DPR-0060.
054990000000
055000000000      *****
055010000000      *****    Processing an Account Transaction record.  These records
055020000000      *****    will be processed using the MLS programming techniques,
055030000000      *****    without the performs.  The current and previous controls
055040000000      *****    have been initialized in the LEVEL REINITIALIZATION 1
055050000000      *****    routine and the records have been read in the GET CURRENT
055060000000      *****    routine.  Therefore, the first step will be to format the
055070000000      *****    current statement transaction controls.
055080000000      *****
055090000000
055100000000           MOVE AT-FUND-NO             TO  CURR-CD-FUND-NO.
055110000000
055120000000      *****
055130000000      *****    Determine if this is a new FUND being processed for the
055140000000      *****    account.
055150000000      *****
055160000000
055170000000           IF  PREV-CD-LEV1   IS EQUAL TO  CURR-CD-LEV1
055180990922               GO TO DPR-0120
055190990922           END-IF
055200000000
055210000000      *****
055220000000      *****    A new fund is being processed.  If this is the first fund
055230000000      *****    for the account, bypass the summary logic.
055240000000      *****
055250000000
055260000000           IF  PREV-CTLS-DPR  IS EQUAL TO  LOW-VALUES
055270990922               GO TO DPR-0070
055280990922           END-IF
055290990922
055300000000      *****
055310000000      *****    Place the fund summary logic here.  Currently, the only
055320000000      *****    processing required is to print a blank line.  Please
055330000000      *****    note that if the statement page is full, the blank line
055340000000      *****    is not formatted.  The next Account Transaction or the
055350000000      *****    Account Summary will skip to the new page, if required.
055360000000      *****
055370000000
055380000000           IF  SPL           IS LESS THAN  WS-SPL-DTLLST
055390990922               SET SPL              UP BY  1
055400990922           END-IF.
055410000000
055420000000       DPR-0070.
055430000000
055440000000      *****
055450000000      *****    Normally, MLS checks the current controls to determine if
055460000000      *****    processing is complete.  Since the Account Transactions
055470000000      *****    are read in the GET CURRENT RECORD routine, this will be
055480000000      *****    done by the normal MLS and will not be required here.
055490000000      *****    Therefore, place the fund re-initialization logic here.
055500000000      *****    Verify that an Account Detail has been read for the fund.
055510000000      *****    If one has not been, a warning message will be printed
055520000000      *****    and the record will be processed as normal.  If this
055530000000      *****    occurs, the program that creates the Statement Data file
055540000000      *****    should be reviewed.
055550000000      *****
055560000000
055570000000           SET SAF                     TO  1.
055580990922
055590000000           IF  SAF        IS GREATER THAN  WS-SAF-LST
055600990922               GO TO DPR-0090
055610990922           END-IF.
055620990922
055630000000       DPR-0080.
055640990922
055650000000           IF  CURR-CD-FUND-NO   EQUAL TO  WS-SAF-FUND-NO (SAF)
055660990922               GO TO DPR-0100
055670990922           END-IF
055680000000
055690000000           IF  SAF           IS LESS THAN  WS-SAF-LST
055700000000               SET SAF              UP BY  1
055710990922               GO TO DPR-0080
055720990922           END-IF.
055730000000
055740000000       DPR-0090.
055750990922
055760000000           MOVE SPACES                 TO  ERROR-LINE-1.
055770000000           SET MSG                     TO  5.
055780990922           PERFORM PRINT-ERR-LINE      THRU PEL-EXIT.
055790000000
055800000000       DPR-0100.
055810000000
055820000000      *****
055830000000      *****    Format the Investment Name.  If this line and at least
055840000000      *****    one transaction line cannot be printed on the current
055850000000      *****    page, process this page and start a subsequent page.
055860000000      *****
055870000000
055880000000           MOVE SPACES                 TO  STMT-DTL-01.
055890000000
055900990922           EVALUATE WS-STMTVRSN
055910990922             WHEN 1
055920990922               MOVE AT-INVESTMENT-NAME TO  SD01V1-INVESTMENT-NAME
055930990922             WHEN 2
055940990922               MOVE AT-INVESTMENT-NAME TO  SD01V2-INVESTMENT-NAME
055950990922             WHEN 3
055960990922               MOVE AT-INVESTMENT-NAME TO  SD01V3-INVESTMENT-NAME
055970990922             WHEN 4
055980990922               MOVE AT-INVESTMENT-NAME TO  SD01V4-INVESTMENT-NAME
055990990922             WHEN 5
056000990922               MOVE AT-INVESTMENT-NAME TO  SD01V5-INVESTMENT-NAME
056010990922             WHEN 6
056020990922               MOVE AT-INVESTMENT-NAME TO  SD01V6-INVESTMENT-NAME
056030991019             WHEN 7
056040991019               MOVE AT-INVESTMENT-NAME TO  SD01V7-INVESTMENT-NAME
056050991213             WHEN 8
056060991213               MOVE AT-INVESTMENT-NAME TO  SD01V8-INVESTMENT-NAME
056070990922             WHEN OTHER
056080990922               MOVE AT-INVESTMENT-NAME TO  SD01-INVESTMENT-NAME
056090990922           END-EVALUATE.
056100990922
056110000000       DPR-0100-0010.
056120990921
056130000000           SET WS-SPL-DTLTST           TO  SPL.
056140990624
056150990624           IF  (WS-SPL-DTLTST           +  3)
056160000000                      IS NOT GREATER THAN  WS-SPL-DTLLST
056170990921               GO TO DPR-0110
056180990921           END-IF
056190990624
056200990624           IF WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
056210990624           AND (WS-SPL-DTLTST           +  5)
056220990624                      IS NOT GREATER THAN  WS-SPL-DTLLST
056230990921               GO TO DPR-0110
056240990921           END-IF
056250990624
056260000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
056270990921
056280000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
056290990921               GO TO DPR-EXIT
056300990921           END-IF.
056310000000
056320000000       DPR-0110.
056330990921
056340990925           IF WS-STMTVRSN             IS EQUAL TO  6
056350990925              SET SPL                 UP BY 1
056360990925           ELSE
056370990925              IF WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
056380990925                 SET SPL              UP BY  3
056390990925              ELSE
056400990925                 SET SPL              UP BY  1
056410990925              END-IF
056420990925           END-IF
056430990925
056440990226           MOVE STMT-DTL-01         TO  WS-STMTPGE-LNE (SPL).
056450000000           SET SPL                  UP BY  1.
056460000000
056470000000       DPR-0120.
056480000000
056490000000      *****
056500000000      *****    Place the detail processing logic here.  The Account
056510000000      *****    Transaction record is to be processed depending on the
056520000000      *****    Transaction Type Code.
056530000000      *****
056540000000
056550990921           IF  AT-TRANS-TYPE-CODE  IS EQUAL TO  "BFW"
056560990921               GO TO DPR-0130
056570990921           END-IF
056580000000
056590990921           IF  AT-TRANS-TYPE-CODE  IS EQUAL TO  "BCL"
056600990921               GO TO DPR-0130-0010
056610990921           END-IF
056620000000
056630000000           GO TO DPR-0140.
056640000000
056650000000       DPR-0130.
056660000000
056670000000      *****
056680000000      *****    Processing a BALANCE FORWARD transaction.  Format the
056690000000      *****    detail line.
056700000000      *****
056710000000
056720000000           MOVE SPACES                 TO  STMT-DTL-02.
056730000000
056740990921           EVALUATE WS-STMTVRSN
056750990921             WHEN 1
056760990921               MOVE WS-SD02-LIT1 (SL)  TO  SD02V1-LIT1
056770990921               MOVE AT-TRANS-TYPE      TO  SD02V1-TRANS-TYPE
056780990921               MOVE AT-UNIT-BALANCE    TO  SD02V1-UNIT-BALANCE
056790990921               MOVE AT-UNIT-BALANCE-SIGN
056800990921                                       TO  SD02V1-UNIT-BALANCE-SIGN
056810990921             WHEN 2
056820000000               MOVE WS-SD02-LIT1 (SL)  TO  SD02V2-LIT1
056830000000               MOVE AT-TRANS-TYPE      TO  SD02V2-TRANS-TYPE
056840000000               MOVE AT-UNIT-BALANCE    TO  SD02V2-UNIT-BALANCE
056850000000               MOVE AT-UNIT-BALANCE-SIGN
056860000000                                       TO  SD02V2-UNIT-BALANCE-SIGN
056870990921             WHEN 3
056880000000               MOVE WS-SD02-LIT1 (SL)  TO  SD02V3-LIT1
056890000000               MOVE AT-TRANS-TYPE      TO  SD02V3-TRANS-TYPE
056900000000               MOVE AT-UNIT-BALANCE    TO  SD02V3-UNIT-BALANCE
056910000000               MOVE AT-UNIT-BALANCE-SIGN
056920000000                                       TO  SD02V3-UNIT-BALANCE-SIGN
056930990921             WHEN 4
056940000000               MOVE WS-SD02-LIT1 (SL)  TO  SD02V4-LIT1
056950000000               MOVE AT-TRANS-TYPE      TO  SD02V4-TRANS-TYPE
056960000000               MOVE AT-UNIT-BALANCE    TO  SD02V4-UNIT-BALANCE
056970000000               MOVE AT-UNIT-BALANCE-SIGN
056980000000                                       TO  SD02V4-UNIT-BALANCE-SIGN
056990990921             WHEN 5
057000000000               MOVE WS-SD02-LIT1 (SL)  TO  SD02V5-LIT1
057010000000               MOVE AT-TRANS-TYPE      TO  SD02V5-TRANS-TYPE
057020000000               MOVE AT-UNIT-BALANCE    TO  SD02V5-UNIT-BALANCE
057030000000               MOVE AT-UNIT-BALANCE-SIGN
057040000000                                       TO  SD02V5-UNIT-BALANCE-SIGN
057050990922             WHEN 6
057060990923               MOVE SPACES             TO  SD02V6-LIT1
057070990922               MOVE AT-TRANS-TYPE      TO  SD02V6-TRANS-TYPE
057080990929               MOVE AT-NO-OF-UNITS     TO  SD02V6-NO-OF-UNITS
057090990929               MOVE AT-NO-OF-UNITS-SIGN TO  SD02V6-NO-OF-UNITS-SIGN
057100990927               MOVE AT-MKT-VALUE       TO  SD02V6-UNIT-BALANCE
057110990927               MOVE SPACES
057120990922                                       TO  SD02V6-UNIT-BALANCE-SIGN
057130991019             WHEN 7
057140991022               MOVE WS-SD02V7-LIT1 (SL)    TO  SD02V7-LIT1
057150991022               MOVE AT-TRANS-TYPE          TO  SD02V7-TRANS-TYPE
057160991022               MOVE AT-UNIT-BALANCE        TO  SD02V7-UNIT-BALANCE
057170991019               MOVE AT-UNIT-BALANCE-SIGN
057180991022                                           TO SD02V7-UNIT-BALANCE-SIGN
057190991213             WHEN 8
057200000809RF7330         MOVE AT-OPEN-DATE          TO  SD02V8-TRADE-DATE
057210000809      *        MOVE WS-SD02V8-LIT1 (SL)   TO  SD02V8-LIT1
057220991213               MOVE AT-TRANS-TYPE         TO  SD02V8-TRANS-TYPE
057230000808RF7330         MOVE AT-MKT-VALUE          TO  SD02V8-GROSS
057240000808RF7330         MOVE AT-UNIT-PRICE        TO  SD02V8-UNIT-PRICE
057250000808RF7330         MOVE AT-UNIT-PRICE-SIGN   TO  SD02V8-UNIT-PRICE-SIGN
057260991213               MOVE AT-UNIT-BALANCE       TO  SD02V8-UNIT-BALANCE
057270991213               MOVE AT-UNIT-BALANCE-SIGN
057280991213                                       TO  SD02V8-UNIT-BALANCE-SIGN
057290990922             WHEN OTHER
057300990921               MOVE WS-SD02-LIT1 (SL)      TO  SD02-LIT1
057310990921               MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE
057320990921               MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE
057330990921               MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN
057340990921           END-EVALUATE.
057350990921
057360000000       DPR-0130-0000-0010.
057370990921
057380000000           MOVE ZEROES                 TO  WS-CPTL-GAIN.
057390000000           MOVE SPACES                 TO  WS-CPTL-GAIN-SIGN.
057400000000           GO TO DPR-0150.
057410000000
057420000000       DPR-0130-0010.
057430000000
057440000000      *****
057450000000      *****    Processing a BALANCE CLOSING transaction.  Format the
057460000000      *****    detail line.
057470000000      *****
057480000000
057490000000           MOVE SPACES                 TO  STMT-DTL-02.
057500000000
057510990921           EVALUATE WS-STMTVRSN
057520990921             WHEN 1
057530991013               MOVE WS-SH-END-DATE-2   TO  SD02V1-TRADE-DATE
057540000000               MOVE AT-TRANS-TYPE      TO  SD02V1-TRANS-TYPE
057550000000               MOVE AT-UNIT-BALANCE    TO  SD02V1-UNIT-BALANCE
057560000000               MOVE AT-UNIT-BALANCE-SIGN
057570000000                                       TO  SD02V1-UNIT-BALANCE-SIGN
057580990921             WHEN 2
057590991013               MOVE WS-SH-END-DATE-2   TO  SD02V2-TRADE-DATE
057600000000               MOVE AT-TRANS-TYPE      TO  SD02V2-TRANS-TYPE
057610000000               MOVE AT-UNIT-BALANCE    TO  SD02V2-UNIT-BALANCE
057620000000               MOVE AT-UNIT-BALANCE-SIGN
057630000000                                       TO  SD02V2-UNIT-BALANCE-SIGN
057640990921             WHEN 3
057650991013               MOVE WS-SH-END-DATE-2   TO  SD02V3-TRADE-DATE
057660000000               MOVE AT-TRANS-TYPE      TO  SD02V3-TRANS-TYPE
057670000000               MOVE AT-UNIT-BALANCE    TO  SD02V3-UNIT-BALANCE
057680000000               MOVE AT-UNIT-BALANCE-SIGN
057690000000                                       TO  SD02V3-UNIT-BALANCE-SIGN
057700990921             WHEN 4
057710991013               MOVE WS-SH-END-DATE-2   TO  SD02V4-TRADE-DATE
057720000000               MOVE AT-TRANS-TYPE      TO  SD02V4-TRANS-TYPE
057730000000               MOVE AT-UNIT-BALANCE    TO  SD02V4-UNIT-BALANCE
057740000000               MOVE AT-UNIT-BALANCE-SIGN
057750000000                                       TO  SD02V4-UNIT-BALANCE-SIGN
057760990921             WHEN 5
057770991013               MOVE WS-SH-END-DATE-2   TO  SD02V5-TRADE-DATE
057780000000               MOVE AT-TRANS-TYPE      TO  SD02V5-TRANS-TYPE
057790000000               MOVE AT-UNIT-BALANCE    TO  SD02V5-UNIT-BALANCE
057800000000               MOVE AT-UNIT-BALANCE-SIGN
057810000000                                       TO  SD02V5-UNIT-BALANCE-SIGN
057820990922             WHEN 6
057830990923               MOVE WS-SH-END-DATE-6   TO  SD02V6-TRADE-DATE
057840990922               MOVE AT-TRANS-TYPE      TO  SD02V6-TRANS-TYPE
057850990929               MOVE AT-UNIT-PRICE      TO  SD02V6-UNIT-PRICE
057860990929               MOVE AT-UNIT-PRICE-SIGN TO  SD02V6-UNIT-PRICE-SIGN
057870990929               MOVE AT-NO-OF-UNITS     TO  SD02V6-NO-OF-UNITS
057880990929               MOVE AT-NO-OF-UNITS-SIGN TO  SD02V6-NO-OF-UNITS-SIGN
057890990927               MOVE AT-MKT-VALUE       TO  SD02V6-UNIT-BALANCE
057900990927               MOVE SPACES
057910990922                                       TO  SD02V6-UNIT-BALANCE-SIGN
057920991019             WHEN 7
057930991019               MOVE WS-SH-END-DATE     TO  SD02V7-TRADE-DATE
057940991019               MOVE AT-TRANS-TYPE      TO  SD02V7-TRANS-TYPE
057950991019               MOVE AT-UNIT-BALANCE    TO  SD02V7-UNIT-BALANCE
057960991019               MOVE AT-UNIT-BALANCE-SIGN
057970991019                                       TO  SD02V7-UNIT-BALANCE-SIGN
057980991213             WHEN 8
057990991213               MOVE WS-SH-END-DATE     TO  SD02V8-TRADE-DATE
058000991213               MOVE AT-TRANS-TYPE      TO  SD02V8-TRANS-TYPE
058010000808RF7330         MOVE AT-MKT-VALUE          TO  SD02V8-GROSS
058020000808RF7330         MOVE AT-UNIT-PRICE        TO  SD02V8-UNIT-PRICE
058030000808RF7330         MOVE AT-UNIT-PRICE-SIGN   TO  SD02V8-UNIT-PRICE-SIGN
058040991213               MOVE AT-UNIT-BALANCE    TO  SD02V8-UNIT-BALANCE
058050991213               MOVE AT-UNIT-BALANCE-SIGN
058060991213                                       TO  SD02V8-UNIT-BALANCE-SIGN
058070990922             WHEN OTHER
058080991013               MOVE WS-SH-END-DATE-2       TO  SD02-TRADE-DATE
058090990921               MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE
058100990921               MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE
058110990921               MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN
058120990921           END-EVALUATE.
058130990921
058140000000       DPR-0130-0020.
058150990921
058160000000           MOVE ZEROES                 TO  WS-CPTL-GAIN.
058170000000           MOVE SPACES                 TO  WS-CPTL-GAIN-SIGN.
058180000000           GO TO DPR-0150.
058190000000
058200000000       DPR-0140.
058210000000
058220000000      *****
058230000000      *****    Processing a transaction other than a BALANCE FORWARD or
058240000000      *****    BALANCE CLOSING.  Format the detail line.
058250000000      *****
058260000000      *****    Please note that the CAPITAL GAIN/LOSS is only being
058270000000      *****    formatted if it is non-zero.  If this does not satisfy
058280000000      *****    the requirements, logic should be added to format this
058290000000      *****    field base on the transaction type.
058300000000      *****
058310000000
058320000000           MOVE AT-TRADE-DATE          TO  WS-TRADE-DATE.
058330990923           MOVE AT-TRADE-DATE          TO  WS-TRADE-DATE-6.
058340000000           MOVE ZEROES                 TO  WS-DEDUCTIONS.
058350991013           MOVE SPACES                 TO  STMT-DTL-02.
058360991013
058370991103991103     IF WS-AT-RULE (AT1)            IS EQUAL TO "4"
058380991013           AND ( AT-TRANS-TYPE-CODE       IS EQUAL TO "BUY"
058390991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "SEL"
058400991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "TRI"
058410991013                OR AT-TRANS-TYPE-CODE    IS EQUAL TO "TRO")
058420991013                MOVE AT-CONTR-REDEMP-DESCRIPTION
058430991013                              TO  AT-TRANS-TYPE
058440991013           END-IF
058450991103
058460991103991103     IF WS-LDA-MGMT-COM-CDE        IS EQUAL TO "CND"
058470991103           AND ( AT-CONTR-REDEMP-CODE    IS EQUAL TO "MFCCNV")
058480991103                MOVE AT-CONTR-REDEMP-DESCRIPTION
058490991103                              TO  AT-TRANS-TYPE
058500991103991103     END-IF
058510991013
058520991013           IF WS-STMTVRSN              IS EQUAL TO 6
058530991019              GO TO DPR-0140-0050
058540991013           END-IF
058550991013
058560990921           IF  AT-COMMISSION-SIGN  IS EQUAL TO  "-"
058570990921               SUBTRACT AT-COMMISSION FROM  WS-DEDUCTIONS
058580990921           ELSE
058590990921               ADD AT-COMMISSION       TO  WS-DEDUCTIONS
058600990921           END-IF
058610990921
058620001114RF4638     IF  AT-DSC-SIGN         IS EQUAL TO  "-"
058630001114               SUBTRACT AT-DSC        FROM  WS-DEDUCTIONS
058640001114           ELSE
058650001114               ADD AT-DSC              TO  WS-DEDUCTIONS
058660001114           END-IF
058670001114
058680000000           IF  AT-TAX-SIGN    IS EQUAL TO  "-"
058690000000               SUBTRACT AT-TAX       FROM  WS-DEDUCTIONS
058700990921           ELSE
058710990921               ADD AT-TAX              TO  WS-DEDUCTIONS
058720990921           END-IF
058730990921
058740010130RF9184     ADD  AT-ADMIN-FEES          TO  WS-DEDUCTIONS.
058750010130RF9298     ADD  AT-GST
058760050401R26894          AT-ERF
058770010130                AT-PST                 TO  WS-DEDUCTIONS.
058780010130
058790000000           IF  WS-DEDUCTIONS IS LESS THAN  ZEROES
058800000000               MOVE "-"                TO  WS-DEDUCTIONS-SIGN
058810990921           ELSE
058820990921               MOVE SPACES             TO  WS-DEDUCTIONS-SIGN
058830990921           END-IF
058840000000
058850990921980417     IF  WS-LDA-CAPITAL-GAIN  IS NOT EQUAL TO  "N"
058860000000               MOVE AT-CPTL-GAIN       TO  WS-CPTL-GAIN
058870000000               MOVE AT-CPTL-GAIN-SIGN  TO  WS-CPTL-GAIN-SIGN
058880990921           ELSE
058890000000               MOVE ZEROES             TO  WS-CPTL-GAIN
058900990921               MOVE SPACES             TO  WS-CPTL-GAIN-SIGN
058910990921           END-IF
058920990921
058930000000
058940990921      *** MOVE DATE TO STMT-DTL-02 DEPENDING ON STMTVRSN
058950990921
058960000000           IF  WS-STMTVRSN   NOT EQUAL TO  1
058970990921               GO TO DPR-0140-0010
058980990921           END-IF
058990990921
059000000000           MOVE WS-SH-TRADE-DATE       TO  SD02V1-TRADE-DATE.
059010000000           MOVE AT-TRANS-TYPE          TO  SD02V1-TRANS-TYPE.
059020000000           MOVE AT-GROSS               TO  SD02V1-GROSS.
059030000000           MOVE AT-GROSS-SIGN          TO  SD02V1-GROSS-SIGN.
059040000000           MOVE WS-DEDUCTIONS          TO  SD02V1-DEDUCTIONS.
059050000000           MOVE AT-NET                 TO  SD02V1-NET.
059060000000           MOVE AT-NET-SIGN            TO  SD02V1-NET-SIGN.
059070000000           MOVE AT-UNIT-PRICE          TO  SD02V1-UNIT-PRICE.
059080000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V1-UNIT-PRICE-SIGN.
059090000000           MOVE AT-NO-OF-UNITS         TO  SD02V1-NO-OF-UNITS.
059100000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V1-NO-OF-UNITS-SIGN.
059110000000           MOVE AT-UNIT-BALANCE        TO  SD02V1-UNIT-BALANCE.
059120000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V1-UNIT-BALANCE-SIGN.
059130990921
059140000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
059150000000               MOVE WS-CPTL-GAIN       TO  SD02V1-CPTL-GAIN
059160990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V1-CPTL-GAIN-SIGN
059170990921           END-IF
059180990921
059190000000           GO TO DPR-0140-9999.
059200000000
059210000000       DPR-0140-0010.
059220990921
059230000000           IF  WS-STMTVRSN   NOT EQUAL TO  2
059240990921               GO TO DPR-0140-0020
059250990921           END-IF
059260990921
059270000000           MOVE WS-SH-TRADE-DATE       TO  SD02V2-TRADE-DATE.
059280000000           MOVE AT-TRANS-TYPE          TO  SD02V2-TRANS-TYPE.
059290000000           MOVE AT-GROSS               TO  SD02V2-GROSS.
059300000000           MOVE AT-GROSS-SIGN          TO  SD02V2-GROSS-SIGN.
059310000000           MOVE WS-DEDUCTIONS          TO  SD02V2-DEDUCTIONS.
059320000000           MOVE AT-NET                 TO  SD02V2-NET.
059330000000           MOVE AT-NET-SIGN            TO  SD02V2-NET-SIGN.
059340000000           MOVE AT-UNIT-PRICE          TO  SD02V2-UNIT-PRICE.
059350000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V2-UNIT-PRICE-SIGN.
059360000000           MOVE AT-NO-OF-UNITS         TO  SD02V2-NO-OF-UNITS.
059370000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V2-NO-OF-UNITS-SIGN.
059380000000           MOVE AT-UNIT-BALANCE        TO  SD02V2-UNIT-BALANCE.
059390000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V2-UNIT-BALANCE-SIGN.
059400990921
059410000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
059420000000               MOVE WS-CPTL-GAIN       TO  SD02V2-CPTL-GAIN
059430990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V2-CPTL-GAIN-SIGN
059440990921           END-IF
059450990921
059460000000           GO TO DPR-0140-9999.
059470000000
059480000000       DPR-0140-0020.
059490990921
059500000000           IF  WS-STMTVRSN   NOT EQUAL TO  3
059510990921               GO TO DPR-0140-0030
059520990921           END-IF
059530990921
059540000000           MOVE WS-SH-TRADE-DATE       TO  SD02V3-TRADE-DATE.
059550000000           MOVE AT-TRANS-TYPE          TO  SD02V3-TRANS-TYPE.
059560000000           MOVE AT-GROSS               TO  SD02V3-GROSS.
059570000000           MOVE AT-GROSS-SIGN          TO  SD02V3-GROSS-SIGN.
059580000000           MOVE WS-DEDUCTIONS          TO  SD02V3-DEDUCTIONS.
059590000000           MOVE AT-NET                 TO  SD02V3-NET.
059600000000           MOVE AT-NET-SIGN            TO  SD02V3-NET-SIGN.
059610000000           MOVE AT-UNIT-PRICE          TO  SD02V3-UNIT-PRICE.
059620000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V3-UNIT-PRICE-SIGN.
059630000000           MOVE AT-NO-OF-UNITS         TO  SD02V3-NO-OF-UNITS.
059640000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V3-NO-OF-UNITS-SIGN.
059650000000           MOVE AT-UNIT-BALANCE        TO  SD02V3-UNIT-BALANCE.
059660000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V3-UNIT-BALANCE-SIGN.
059670990921
059680000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
059690000000               MOVE WS-CPTL-GAIN       TO  SD02V3-CPTL-GAIN
059700990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V3-CPTL-GAIN-SIGN
059710990921           END-IF
059720990921
059730000000           GO TO DPR-0140-9999.
059740000000
059750000000       DPR-0140-0030.
059760990921
059770000000           IF  WS-STMTVRSN   NOT EQUAL TO  4
059780990921               GO TO DPR-0140-0040
059790990921           END-IF
059800000000
059810000000           MOVE WS-SH-TRADE-DATE       TO  SD02V4-TRADE-DATE.
059820000000           MOVE AT-TRANS-TYPE          TO  SD02V4-TRANS-TYPE.
059830000000           MOVE AT-GROSS               TO  SD02V4-GROSS.
059840000000           MOVE AT-GROSS-SIGN          TO  SD02V4-GROSS-SIGN.
059850000000           MOVE WS-DEDUCTIONS          TO  SD02V4-DEDUCTIONS.
059860000000           MOVE AT-NET                 TO  SD02V4-NET.
059870000000           MOVE AT-NET-SIGN            TO  SD02V4-NET-SIGN.
059880000000           MOVE AT-UNIT-PRICE          TO  SD02V4-UNIT-PRICE.
059890000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V4-UNIT-PRICE-SIGN.
059900000000           MOVE AT-NO-OF-UNITS         TO  SD02V4-NO-OF-UNITS.
059910000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V4-NO-OF-UNITS-SIGN.
059920000000           MOVE AT-UNIT-BALANCE        TO  SD02V4-UNIT-BALANCE.
059930000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V4-UNIT-BALANCE-SIGN.
059940990921
059950000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
059960000000               MOVE WS-CPTL-GAIN       TO  SD02V4-CPTL-GAIN
059970990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V4-CPTL-GAIN-SIGN
059980990921           END-IF
059990990921
060000000000           GO TO DPR-0140-9999.
060010000000
060020000000       DPR-0140-0040.
060030990921
060040000000           IF  WS-STMTVRSN   NOT EQUAL TO  5
060050991019               GO TO DPR-0140-0050
060060990921           END-IF
060070000000
060080000000           MOVE WS-SH-TRADE-DATE       TO  SD02V5-TRADE-DATE.
060090000000           MOVE AT-TRANS-TYPE          TO  SD02V5-TRANS-TYPE.
060100000000           MOVE AT-GROSS               TO  SD02V5-GROSS.
060110000000           MOVE AT-GROSS-SIGN          TO  SD02V5-GROSS-SIGN.
060120000000           MOVE WS-DEDUCTIONS          TO  SD02V5-DEDUCTIONS.
060130000000           MOVE AT-NET                 TO  SD02V5-NET.
060140000000           MOVE AT-NET-SIGN            TO  SD02V5-NET-SIGN.
060150000000           MOVE AT-UNIT-PRICE          TO  SD02V5-UNIT-PRICE.
060160000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V5-UNIT-PRICE-SIGN.
060170000000           MOVE AT-NO-OF-UNITS         TO  SD02V5-NO-OF-UNITS.
060180000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V5-NO-OF-UNITS-SIGN.
060190000000           MOVE AT-UNIT-BALANCE        TO  SD02V5-UNIT-BALANCE.
060200000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V5-UNIT-BALANCE-SIGN.
060210990921
060220000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
060230000000               MOVE WS-CPTL-GAIN       TO  SD02V5-CPTL-GAIN
060240990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V5-CPTL-GAIN-SIGN
060250990921           END-IF
060260990921
060270000000           GO TO DPR-0140-9999.
060280000000
060290991019       DPR-0140-0050.
060300990921
060310990922           IF  WS-STMTVRSN   NOT EQUAL TO  6
060320991019               GO TO DPR-0140-0060
060330990921           END-IF
060340990921
060350990923           MOVE WS-SH-TRADE-DATE-6     TO  SD02V6-TRADE-DATE.
060360990922           MOVE AT-TRANS-TYPE          TO  SD02V6-TRANS-TYPE.
060370990922           MOVE AT-GROSS               TO  SD02V6-GROSS.
060380990922           MOVE AT-GROSS-SIGN          TO  SD02V6-GROSS-SIGN.
060390990922           MOVE AT-UNIT-PRICE          TO  SD02V6-UNIT-PRICE.
060400990922           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V6-UNIT-PRICE-SIGN.
060410990922           MOVE AT-NO-OF-UNITS         TO  SD02V6-NO-OF-UNITS.
060420990922           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V6-NO-OF-UNITS-SIGN.
060430990927           MOVE AT-MKT-VALUE           TO  SD02V6-UNIT-BALANCE.
060440990927           MOVE SPACES                 TO  SD02V6-UNIT-BALANCE-SIGN.
060450990921           GO TO DPR-0140-9999.
060460990921
060470991019       DPR-0140-0060.
060480991019
060490991019           IF  WS-STMTVRSN   NOT EQUAL TO  7
060500991019               GO TO DPR-0140-0070
060510991019           END-IF
060520991019
060530991019           MOVE WS-SH-TRADE-DATE-6     TO  SD02V7-TRADE-DATE.
060540991019           MOVE AT-TRANS-TYPE          TO  SD02V7-TRANS-TYPE.
060550991019           MOVE AT-GROSS               TO  SD02V7-GROSS.
060560991019           MOVE AT-GROSS-SIGN          TO  SD02V7-GROSS-SIGN.
060570991019           MOVE WS-DEDUCTIONS          TO  SD02V7-DEDUCTIONS.
060580991019           MOVE AT-NET                 TO  SD02V7-NET.
060590991019           MOVE AT-NET-SIGN            TO  SD02V7-NET-SIGN.
060600991019           MOVE AT-UNIT-PRICE          TO  SD02V7-UNIT-PRICE.
060610991019           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V7-UNIT-PRICE-SIGN.
060620991019           MOVE AT-NO-OF-UNITS         TO  SD02V7-NO-OF-UNITS.
060630991019           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V7-NO-OF-UNITS-SIGN.
060640991019           MOVE AT-UNIT-BALANCE        TO  SD02V7-UNIT-BALANCE.
060650991019           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V7-UNIT-BALANCE-SIGN.
060660991019
060670991019           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
060680991019               MOVE WS-CPTL-GAIN       TO  SD02V7-CPTL-GAIN
060690991019               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V7-CPTL-GAIN-SIGN
060700991019           END-IF
060710991019
060720991019           GO TO DPR-0140-9999.
060730991019
060740991213       DPR-0140-0070.
060750991213
060760991213           IF  WS-STMTVRSN   NOT EQUAL TO  8
060770991213               GO TO DPR-0140-0080
060780991213           END-IF
060790991213
060800991213           MOVE WS-SH-TRADE-DATE-6     TO  SD02V8-TRADE-DATE.
060810991213           MOVE AT-TRANS-TYPE          TO  SD02V8-TRANS-TYPE.
060820991213           MOVE AT-GROSS               TO  SD02V8-GROSS.
060830991213           MOVE AT-GROSS-SIGN          TO  SD02V8-GROSS-SIGN.
060840991213           MOVE WS-DEDUCTIONS          TO  SD02V8-DEDUCTIONS.
060850991213           MOVE AT-NET                 TO  SD02V8-NET.
060860991213           MOVE AT-NET-SIGN            TO  SD02V8-NET-SIGN.
060870991213           MOVE AT-UNIT-PRICE          TO  SD02V8-UNIT-PRICE.
060880991213           MOVE AT-UNIT-PRICE-SIGN     TO  SD02V8-UNIT-PRICE-SIGN.
060890991213           MOVE AT-NO-OF-UNITS         TO  SD02V8-NO-OF-UNITS.
060900991213           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02V8-NO-OF-UNITS-SIGN.
060910991213           MOVE AT-UNIT-BALANCE        TO  SD02V8-UNIT-BALANCE.
060920991213           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02V8-UNIT-BALANCE-SIGN.
060930991213
060940991213           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
060950991213               MOVE WS-CPTL-GAIN       TO  SD02V8-CPTL-GAIN
060960991213               MOVE WS-CPTL-GAIN-SIGN  TO  SD02V8-CPTL-GAIN-SIGN
060970991213           END-IF
060980991213
060990991213           GO TO DPR-0140-9999.
061000991213
061010991213       DPR-0140-0080.
061020990921
061030000000           MOVE WS-SH-TRADE-DATE       TO  SD02-TRADE-DATE.
061040000000           MOVE AT-TRANS-TYPE          TO  SD02-TRANS-TYPE.
061050000000           MOVE AT-GROSS               TO  SD02-GROSS.
061060000000           MOVE AT-GROSS-SIGN          TO  SD02-GROSS-SIGN.
061070000000           MOVE WS-DEDUCTIONS          TO  SD02-DEDUCTIONS.
061080000000           MOVE AT-NET                 TO  SD02-NET.
061090000000           MOVE AT-NET-SIGN            TO  SD02-NET-SIGN.
061100000000           MOVE AT-UNIT-PRICE          TO  SD02-UNIT-PRICE.
061110000000           MOVE AT-UNIT-PRICE-SIGN     TO  SD02-UNIT-PRICE-SIGN.
061120000000           MOVE AT-NO-OF-UNITS         TO  SD02-NO-OF-UNITS.
061130000000           MOVE AT-NO-OF-UNITS-SIGN    TO  SD02-NO-OF-UNITS-SIGN.
061140000000           MOVE AT-UNIT-BALANCE        TO  SD02-UNIT-BALANCE.
061150000000           MOVE AT-UNIT-BALANCE-SIGN   TO  SD02-UNIT-BALANCE-SIGN.
061160990921
061170000000           IF  WS-CPTL-GAIN  NOT EQUAL TO  ZEROES
061180000000               MOVE WS-CPTL-GAIN       TO  SD02-CPTL-GAIN
061190990921               MOVE WS-CPTL-GAIN-SIGN  TO  SD02-CPTL-GAIN-SIGN
061200990921           END-IF.
061210990921
061220000000       DPR-0140-9999.
061230990921
061240000000           GO TO DPR-0150.
061250000000
061260000000       DPR-0150.
061270000000
061280000000      *****
061290000000      *****    If the transaction cannot be printed on the current page,
061300000000      *****    process the current page and start a subsequent page.
061310000000      *****    When determining if the transaction can be printed on the
061320000000      *****    current page, additional checks regarding a capital gain
061330000000      *****    overflow line are included.
061340000000      *****
061350000000
061360990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  99999.99
061370000000           AND SPL           IS LESS THAN  WS-SPL-DTLLST
061380990921               GO TO DPR-0160
061390990921           END-IF
061400990921
061410000000           SET WS-SPL                  TO  SPL.
061420990921
061430000000           IF  WS-CPTL-GAIN  GREATER THAN  99999.99
061440000000           AND (WS-SPL + 1)  IS LESS THAN  WS-SPL-DTLLST
061450990921               GO TO DPR-0160
061460990921           END-IF
061470000000
061480000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
061490990921
061500000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
061510990921               GO TO DPR-EXIT
061520990921           END-IF.
061530000000
061540000000       DPR-0160.
061550990921
061560990921           SET SPL                     UP BY  1.
061570000000           MOVE STMT-DTL-02            TO  WS-STMTPGE-LNE (SPL).
061580000000
061590000000      *****
061600000000      *****    If the transaction Capital Gain value exceeds 99,999.99,
061610000000      *****    a second line will be printed to indicate that the value
061620000000      *****    is in excess of the printed amount.  This additional line
061630000000      *****    is due to space restrictions on the form itself.
061640000000      *****
061650000000
061660990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  99999.99
061670990921               GO TO DPR-0170
061680990921           END-IF
061690990921
061700000000           MOVE SPACES                 TO  STMT-DTL-02.
061710000000
061720000000           IF  WS-STMTVRSN   NOT EQUAL TO  1
061730990921               GO TO DPR-0160-0010
061740990921           END-IF
061750000000
061760990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
061770990921               GO TO DPR-0170
061780990921           END-IF
061790000000
061800000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
061810000000               MOVE "(++++++++++)"     TO  SD02V1-CPTL-GAIN-OFLIT
061820990921           ELSE
061830990921               MOVE "(----------)"     TO  SD02V1-CPTL-GAIN-OFLIT
061840990921           END-IF
061850990921
061860000000           GO TO DPR-0160-9999.
061870000000
061880000000       DPR-0160-0010.
061890990921
061900000000           IF  WS-STMTVRSN   NOT EQUAL TO  2
061910990921               GO TO DPR-0160-0020
061920990921           END-IF
061930000000
061940990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
061950990921               GO TO DPR-0170
061960990921           END-IF
061970990226
061980990226           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
061990990226               MOVE "(++++++++++)"     TO  SD02V2-CPTL-GAIN-OFLIT
062000990921           ELSE
062010990921               MOVE "(----------)"     TO  SD02V2-CPTL-GAIN-OFLIT
062020990921           END-IF
062030990921
062040000000           GO TO DPR-0160-9999.
062050000000
062060000000       DPR-0160-0020.
062070990921
062080000000           IF  WS-STMTVRSN   NOT EQUAL TO  3
062090990921               GO TO DPR-0160-0030
062100990921           END-IF
062110000000
062120000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
062130000000               MOVE "(++++++++)"       TO  SD02V3-CPTL-GAIN-OFLIT
062140990921           ELSE
062150990921               MOVE "(--------)"       TO  SD02V3-CPTL-GAIN-OFLIT
062160990921           END-IF
062170990921
062180000000           GO TO DPR-0160-9999.
062190000000
062200000000       DPR-0160-0030.
062210990921
062220000000           IF  WS-STMTVRSN   NOT EQUAL TO  4
062230990921               GO TO DPR-0160-0040
062240990921           END-IF
062250000000
062260990921           IF  WS-CPTL-GAIN IS NOT GREATER THAN  9999999.99
062270990921               GO TO DPR-0170
062280990921           END-IF
062290000000
062300000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
062310000000               MOVE "(++++++++++)"     TO  SD02V4-CPTL-GAIN-OFLIT
062320990921           ELSE
062330990921               MOVE "(----------)"     TO  SD02V4-CPTL-GAIN-OFLIT
062340990921           END-IF
062350990921
062360000000           GO TO DPR-0160-9999.
062370000000
062380000000       DPR-0160-0040.
062390990921
062400000000           IF  WS-STMTVRSN   NOT EQUAL TO  5
062410990921               GO TO DPR-0160-0050
062420990921           END-IF
062430000000
062440990921           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
062450990921               GO TO DPR-0170
062460990921           END-IF
062470000000
062480000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
062490000000               MOVE "(++++++++++)"     TO  SD02V5-CPTL-GAIN-OFLIT
062500000000             ELSE
062510990921               MOVE "(----------)"     TO  SD02V5-CPTL-GAIN-OFLIT
062520990921           END-IF
062530990921
062540000000           GO TO DPR-0160-9999.
062550000000
062560990921       DPR-0160-0050.
062570990921
062580990922           IF  WS-STMTVRSN   NOT EQUAL TO  6
062590991019               GO TO DPR-0160-0060
062600990921           END-IF
062610990921
062620990924           IF  WS-STMTVRSN       EQUAL TO  6
062630990924               GO TO DPR-0160-9999
062640990924           END-IF.
062650990921
062660991019       DPR-0160-0060.
062670991019
062680991019           IF  WS-STMTVRSN   NOT EQUAL TO  7
062690991019               GO TO DPR-0160-0080
062700991019           END-IF
062710991019
062720991019           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
062730991019               GO TO DPR-0170
062740991019           END-IF
062750991019
062760991019           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
062770991021               MOVE "(++++++++++)"     TO  SD02V7-CPTL-GAIN-OFLIT
062780991021           ELSE
062790991021               MOVE "(----------)"     TO  SD02V7-CPTL-GAIN-OFLIT
062800991019           END-IF
062810991019
062820991019           GO TO DPR-0160-9999.
062830991019
062840991214       DPR-0160-0080.
062850991214
062860991214           IF  WS-STMTVRSN   NOT EQUAL TO  8
062870991214               GO TO DPR-0160-0090
062880991214           END-IF
062890991214
062900991214           IF  WS-CPTL-GAIN  IS NOT GREATER THAN  9999999.99
062910991214               GO TO DPR-0170
062920991214           END-IF
062930991214
062940991214           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
062950991214               MOVE "(++++++++++)"     TO  SD02V8-CPTL-GAIN-OFLIT
062960991214           ELSE
062970991214               MOVE "(----------)"     TO  SD02V8-CPTL-GAIN-OFLIT
062980991214           END-IF
062990991214
063000991214           GO TO DPR-0160-9999.
063010991214
063020991214       DPR-0160-0090.
063030990921
063040000000           IF  WS-CPTL-GAIN-SIGN EQUAL TO  SPACES
063050000000               MOVE "(++++++++)"       TO  SD02-CPTL-GAIN-OFLIT
063060990921           ELSE
063070990921               MOVE "(--------)"       TO  SD02-CPTL-GAIN-OFLIT
063080990921           END-IF.
063090000000
063100000000       DPR-0160-9999.
063110990921
063120990924           IF WS-STMTVRSN              IS NOT EQUAL TO 6
063130990924             SET SPL                     UP BY  1
063140990924             MOVE STMT-DTL-02            TO  WS-STMTPGE-LNE (SPL)
063150990924           END-IF.
063160990924
063170000000       DPR-0170.
063180000000
063190000000      *****
063200000000      *****    The Account Transaction has been completely processed.
063210000000      *****    Update the previous statement transaction controls and
063220000000      *****    exit.  The next, if any, Account Transaction will be read
063230000000      *****    by the GET CURRENT RECORD routine.
063240000000      *****
063250000000
063260000000           MOVE CURR-CTLS-DPR          TO  PREV-CTLS-DPR.
063270981215           GO TO DPR-EXIT.
063280990921
063290981207       DPR-0180.
063300990921
063310981207           MOVE "Y"     TO WS-STMT-DTA-SW-60.
063320981207
063330990921      **********
063340990921
063350000000       DPR-EXIT.
063360000000           EXIT.
063370990921
063380990921      **********
063390990921
063400000000      /
063410000000       LEVEL-SUMMARY-1.
063420000000
063430000000      ******************************************************************
063440000000      *                                                                *
063450000000      *    L E V E L   S U M M A R I Z A T I O N   1   R O U T I N E   *
063460000000      *                                                                *
063470000000      *        ACCOUNT:                Account                         *
063480000000      *                                                                *
063490000000      ******************************************************************
063500000000
063510000000      *****
063520000000      *****    If Account is zeroes, bypass this routine.
063530000000      *****
063540000000
063550000000           IF  PREV-ACCOUNT   IS EQUAL TO  ZEROES
063560990921               GO TO LS1-EXIT
063570990921           END-IF
063580990921
063590000000      *****
063600000000      *****    If an error was detected by the LEVEL REINITIALIZATION 1
063610000000      *****    routine, bypass this routine.
063620000000      *****
063630000000
063640000000           IF  WS-L01BYP-SW   IS EQUAL TO  "Y"
063650990921               GO TO LS1-EXIT
063660990921           END-IF
063670000000
063680980529           IF  WS-LDA-SUM-SUPPRESS-FLAG  IS EQUAL TO "Y"
063690990921               GO TO LS1-0390
063700990921           END-IF
063710990921
063720000000      *****
063730000000      *****    If at least one Account Transaction was formatted, skip a
063740000000      *****    line.
063750000000      *****
063760000000
063770000000           IF  CURR-CTLS-DPR NOT EQUAL TO  LOW-VALUES
063780990921               SET SPL              UP BY  1
063790990921           END-IF
063800990921
063810000000      *****
063820000000      *****    If the account funds and currency totals cannot be
063830000000      *****    formatted on the current page, process the page and start
063840000000      *****    a subsequent page.  But, if the account funds and
063850000000      *****    currency totals cannot be formatted on their own page,
063860000000      *****    complete the current page.
063870000000      *****
063880000000
063890000000           SET WS-SPL-DTLTST           TO  SPL.
063900000000           ADD 4                       TO  WS-SPL-DTLTST.
063910000000           ADD WS-SAF-LST              TO  WS-SPL-DTLTST.
063920000000
063930000000           SET SC                      TO  1.
063940000000
063950000000       LS1-0010.
063960990921
063970000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
063980000000               ADD 1                   TO  WS-SPL-DTLTST
063990990921               GO TO LS1-0020
064000990921           END-IF
064010990921
064020000000           IF  SC            IS LESS THAN  WS-SC-MAX
064030000000               SET SC               UP BY  1
064040990921               GO TO LS1-0010
064050990921           END-IF.
064060000000
064070000000       LS1-0020.
064080990921
064090000000           SET SC                      TO  1.
064100000000
064110000000       LS1-0030.
064120990921
064130000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
064140990921               ADD 1                   TO  WS-SPL-DTLTST
064150990921           END-IF
064160000000
064170000000           IF  SC            IS LESS THAN  WS-SC-MAX
064180000000               SET SC               UP BY  1
064190990921               GO TO LS1-0030
064200990921           END-IF
064210000000
064220000000           IF  WS-SPL-DTLTST
064230000000                      IS NOT GREATER THAN  WS-SPL-DTLLST
064240990921               GO TO LS1-0050
064250990921           END-IF
064260990921
064270000000           SUBTRACT WS-SPL-DTLFST    FROM  WS-SPL-DTLLST
064280000000                                   GIVING  WS-SPL-DTLMAX.
064290000000           SET WS-SPL                  TO  SPL.
064300000000           SUBTRACT WS-SPL           FROM  WS-SPL-DTLTST.
064310990921
064320990921           IF  WS-SPL-DTLTST IS NOT GREATER THAN  WS-SPL-DTLMAX
064330990921               GO TO LS1-0040
064340990921           END-IF
064350990921
064360000000           SET SPL                  UP BY  7.
064370990921
064380000000           IF  SPL    IS NOT GREATER THAN  WS-SPL-DTLLST
064390000000               SET SPL            DOWN BY  7
064400990921               GO TO LS1-0050
064410990921           END-IF.
064420000000
064430000000       LS1-0040.
064440990921
064450000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
064460990921
064470000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
064480990921               GO TO LS1-EXIT
064490990921           END-IF.
064500990921
064510000000       LS1-0050.
064520000000
064530000000      *****
064540000000      *****    Format statement summary line 1.
064550000000      *****
064560000000
064570000000           MOVE SPACES                 TO  STMT-SUM-01.
064580990921
064590990921           EVALUATE WS-STMTVRSN
064600990921             WHEN 1
064610990921               MOVE WS-SS01V1-LIT1 (SL) TO  SS01V1-LIT1
064620990921             WHEN 2
064630990921               MOVE WS-SS01V2-LIT1 (SL) TO  SS01V2-LIT1
064640990921             WHEN 3
064650990921               MOVE WS-SS01V3-LIT1 (SL) TO  SS01V3-LIT1
064660990921             WHEN 4
064670990921               MOVE WS-SS01V4-LIT1 (SL) TO  SS01V4-LIT1
064680990921             WHEN 5
064690990921               MOVE WS-SS01V5-LIT1 (SL) TO  SS01V5-LIT1
064700990922             WHEN 6
064710990922               MOVE WS-SS01V6-LIT1 (SL) TO  SS01V6-LIT1
064720991021             WHEN 7
064730991021               MOVE WS-SS01V1-LIT1 (SL) TO  SS01V7-LIT1
064740991213             WHEN 8
064750991213               MOVE WS-SS01V8-LIT1 (SL) TO  SS01V8-LIT1
064760990922             WHEN OTHER
064770990921               MOVE WS-SS01-LIT1 (SL)   TO  SS01-LIT1
064780990921           END-EVALUATE.
064790990921
064800000000       LS1-0050-0010.
064810990921
064820000000           SET SPL                  UP BY  1.
064830990226           MOVE STMT-SUM-01         TO  WS-STMTPGE-LNE (SPL).
064840000000
064850000000      *****
064860000000      *****    Format statement summary line 2.
064870000000      *****
064880000000
064890000000           MOVE SPACES                 TO  STMT-SUM-02.
064900000000
064910000000           IF  WS-STMTVRSN   NOT EQUAL TO  1
064920990921               GO TO LS1-0050-0010-0010
064930991013           END-IF.
064940990921
064950990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
064960990921              MOVE WS-SS02-LIT0 (SL)      TO  SS02V1-LIT0
064970991013           END-IF.
064980991013
064990991013           MOVE WS-SS02-LIT1 (SL)      TO  SS02V1-LIT1.
065000991013           MOVE WS-SS02-LIT2 (SL)      TO  SS02V1-LIT2.
065010991013           MOVE WS-SS02-LIT3 (SL)      TO  SS02V1-LIT3.
065020991013           MOVE WS-SS02-LIT4 (SL)      TO  SS02V1-LIT4.
065030991013           GO TO LS1-0050-0020.
065040991013
065050000000
065060000000       LS1-0050-0010-0010.
065070990921
065080000000           IF  WS-STMTVRSN   NOT EQUAL TO  2
065090990921               GO TO LS1-0050-0010-0020
065100991013           END-IF.
065110990921
065120990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
065130990921              MOVE WS-SS02-LIT0 (SL)      TO  SS02V2-LIT0
065140991013           END-IF.
065150991013
065160991013              MOVE WS-SS02-LIT1 (SL)      TO  SS02V2-LIT1.
065170991013              MOVE WS-SS02-LIT2 (SL)      TO  SS02V2-LIT2.
065180991013              MOVE WS-SS02-LIT3 (SL)      TO  SS02V2-LIT3.
065190991013              MOVE WS-SS02-LIT4 (SL)      TO  SS02V2-LIT4.
065200991013              GO TO LS1-0050-0020.
065210000000
065220000000       LS1-0050-0010-0020.
065230990921
065240000000           IF  WS-STMTVRSN   NOT EQUAL TO  3
065250990921               GO TO LS1-0050-0010-0030
065260991013           END-IF.
065270990921
065280990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
065290990921              MOVE WS-SS02-LIT0 (SL)      TO  SS02V3-LIT0
065300991013           END-IF.
065310991013              MOVE WS-SS02-LIT1 (SL)      TO  SS02V3-LIT1.
065320991013              MOVE WS-SS02-LIT2 (SL)      TO  SS02V3-LIT2.
065330991013              MOVE WS-SS02-LIT3 (SL)      TO  SS02V3-LIT3.
065340991013              MOVE WS-SS02-LIT4 (SL)      TO  SS02V3-LIT4.
065350991013              GO TO LS1-0050-0020.
065360000000
065370000000       LS1-0050-0010-0030.
065380990921
065390000000           IF  WS-STMTVRSN   NOT EQUAL TO  4
065400990921               GO TO LS1-0050-0010-0040
065410991013           END-IF.
065420990921
065430990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
065440990921              MOVE WS-SS02-LIT0 (SL)      TO  SS02V4-LIT0
065450991013           END-IF.
065460991013              MOVE WS-SS02-LIT1 (SL)      TO  SS02V4-LIT1.
065470991013              MOVE WS-SS02-LIT2 (SL)      TO  SS02V4-LIT2.
065480991013              MOVE WS-SS02-LIT3 (SL)      TO  SS02V4-LIT3.
065490991013              MOVE WS-SS02-LIT4 (SL)      TO  SS02V4-LIT4.
065500991013              GO TO LS1-0050-0020.
065510990921
065520000000       LS1-0050-0010-0040.
065530990921
065540000000           IF  WS-STMTVRSN   NOT EQUAL TO  5
065550990921               GO TO LS1-0050-0010-0050
065560991013           END-IF.
065570990921
065580990921           IF WS-LDA-PRT-NET-INVEST IS EQUAL TO  "Y"
065590990921              MOVE WS-SS02-LIT0 (SL)      TO  SS02V5-LIT0
065600991013           END-IF.
065610991013              MOVE WS-SS02-LIT1 (SL)      TO  SS02V5-LIT1.
065620991013              MOVE WS-SS02-LIT2 (SL)      TO  SS02V5-LIT2.
065630991013              MOVE WS-SS02-LIT3 (SL)      TO  SS02V5-LIT3.
065640991013              MOVE WS-SS02-LIT4 (SL)      TO  SS02V5-LIT4.
065650991013              GO TO LS1-0050-0020.
065660000000
065670990921       LS1-0050-0010-0050.
065680990921
065690990922           IF  WS-STMTVRSN   NOT EQUAL TO  6
065700990921               GO TO LS1-0050-0010-0060
065710991013           END-IF.
065720990921
065730991013           MOVE WS-SS02-LIT1 (SL)      TO  SS02V6-LIT1.
065740991013           MOVE WS-SS02-LIT2 (SL)      TO  SS02V6-LIT2.
065750991013           MOVE WS-SS02-LIT3 (SL)      TO  SS02V6-LIT3.
065760990923           GO TO LS1-0050-0020.
065770990921
065780991021       LS1-0050-0010-0060.
065790991021
065800991021           IF  WS-STMTVRSN   NOT EQUAL TO  7
065810991021               GO TO LS1-0050-0010-0070
065820991021           END-IF.
065830991021
065840991021           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
065850991021              MOVE WS-SS02-LIT0 (SL)      TO  SS02V7-LIT0
065860991021           END-IF.
065870991021
065880991021           MOVE WS-SS02-LIT1 (SL)      TO  SS02V7-LIT1.
065890991021           MOVE WS-SS02-LIT2 (SL)      TO  SS02V7-LIT2.
065900991021           MOVE WS-SS02-LIT3 (SL)      TO  SS02V7-LIT3.
065910991021           MOVE WS-SS02-LIT4 (SL)      TO  SS02V7-LIT4.
065920991021           GO TO LS1-0050-0020.
065930991021
065940991213       LS1-0050-0010-0070.
065950991213
065960991213           IF  WS-STMTVRSN   NOT EQUAL TO  8
065970991213               GO TO LS1-0050-0010-0080
065980991213           END-IF.
065990991213
066000991213           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
066010991213              MOVE WS-SS02-LIT0 (SL)      TO  SS02V8-LIT0
066020991213           END-IF.
066030991213
066040991213           MOVE WS-SS02-LIT1 (SL)      TO  SS02V8-LIT1.
066050991213           MOVE WS-SS02-LIT2 (SL)      TO  SS02V8-LIT2.
066060991213           MOVE WS-SS02-LIT3 (SL)      TO  SS02V8-LIT3.
066070991213           MOVE WS-SS02-LIT4 (SL)      TO  SS02V8-LIT4.
066080991213           GO TO LS1-0050-0020.
066090991213
066100991213       LS1-0050-0010-0080.
066110990921
066120990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
066130990923              MOVE WS-SS02-LIT0 (SL)      TO  SS02-LIT0
066140991013           END-IF.
066150991013              MOVE WS-SS02-LIT1 (SL)      TO  SS02-LIT1.
066160991013              MOVE WS-SS02-LIT2 (SL)      TO  SS02-LIT2.
066170991013              MOVE WS-SS02-LIT3 (SL)      TO  SS02-LIT3.
066180991013              MOVE WS-SS02-LIT4 (SL)      TO  SS02-LIT4.
066190000000
066200000000       LS1-0050-0020.
066210990921
066220990925           IF WS-STMTVRSN                 IS EQUAL TO  6
066230990925              SET SPL                     UP BY  2
066240990925           ELSE
066250990925              SET SPL                     UP BY  1
066260990925           END-IF
066270990925
066280990226           MOVE STMT-SUM-02         TO  WS-STMTPGE-LNE (SPL).
066290000000
066300000000      *****
066310000000      *****    Format statement summary line 3.
066320000000      *****
066330000000
066340000000           MOVE SPACES                 TO  STMT-SUM-03.
066350000000
066360000000           IF  WS-STMTVRSN   NOT EQUAL TO  1
066370990921               GO TO LS1-0050-0020-0010
066380991013           END-IF.
066390990921
066400990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
066410990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03V1-LIT0
066420991013           END-IF.
066430991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03V1-LIT1.
066440991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03V1-LIT2.
066450991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03V1-LIT3.
066460991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03V1-LIT4.
066470991013              GO TO LS1-0050-0030.
066480990921
066490000000       LS1-0050-0020-0010.
066500990921
066510000000           IF  WS-STMTVRSN   NOT EQUAL TO  2
066520990921               GO TO LS1-0050-0020-0020
066530991013           END-IF.
066540990921
066550990921           IF WS-LDA-PRT-NET-INVEST IS EQUAL TO  "Y"
066560990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03V2-LIT0
066570991013           END-IF.
066580991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03V2-LIT1.
066590991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03V2-LIT2.
066600991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03V2-LIT3.
066610991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03V2-LIT4.
066620991013              GO TO LS1-0050-0030.
066630000000
066640000000       LS1-0050-0020-0020.
066650990921
066660000000           IF  WS-STMTVRSN   NOT EQUAL TO  3
066670990921               GO TO LS1-0050-0020-0030
066680991013           END-IF.
066690000000
066700990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
066710990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03V3-LIT0
066720991013           END-IF.
066730991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03V3-LIT1.
066740991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03V3-LIT2.
066750991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03V3-LIT3.
066760991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03V3-LIT4.
066770991013              GO TO LS1-0050-0030.
066780000000
066790000000       LS1-0050-0020-0030.
066800990921
066810000000           IF  WS-STMTVRSN   NOT EQUAL TO  4
066820990921               GO TO LS1-0050-0020-0040
066830991013           END-IF.
066840000000
066850990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
066860990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03V4-LIT0
066870991013           END-IF.
066880991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03V4-LIT1.
066890991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03V4-LIT2.
066900991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03V4-LIT3.
066910991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03V4-LIT4.
066920991013              GO TO LS1-0050-0030.
066930000000
066940000000       LS1-0050-0020-0040.
066950990921
066960000000           IF  WS-STMTVRSN   NOT EQUAL TO  5
066970990921               GO TO LS1-0050-0020-0050
066980991013           END-IF.
066990000000
067000990921           IF WS-LDA-PRT-NET-INVEST IS EQUAL TO  "Y"
067010990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03V5-LIT0
067020991013           END-IF.
067030991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03V5-LIT1.
067040991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03V5-LIT2.
067050991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03V5-LIT3.
067060991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03V5-LIT4.
067070991013              GO TO LS1-0050-0030.
067080990921
067090990921       LS1-0050-0020-0050.
067100990921
067110990922           IF  WS-STMTVRSN   NOT EQUAL TO  6
067120990921               GO TO LS1-0050-0020-0060
067130990921           END-IF
067140990921
067150991013           MOVE WS-SS03-LIT1 (SL)      TO  SS03V6-LIT1.
067160991013           MOVE WS-SS03-LIT2 (SL)      TO  SS03V6-LIT2.
067170991013           MOVE WS-SS03-LIT3 (SL)      TO  SS03V6-LIT3.
067180990923           GO TO LS1-0050-0030.
067190990921
067200991021       LS1-0050-0020-0060.
067210991021
067220991021           IF WS-STMTVRSN   NOT EQUAL TO  7
067230991021              GO TO LS1-0050-0020-0070
067240991021           END-IF
067250991021
067260991021           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
067270991021              MOVE WS-SS03-LIT0 (SL)      TO  SS03V7-LIT0
067280991021           END-IF
067290991021
067300991021           MOVE WS-SS03-LIT1 (SL)      TO  SS03V7-LIT1.
067310991021           MOVE WS-SS03-LIT2 (SL)      TO  SS03V7-LIT2.
067320991021           MOVE WS-SS03-LIT3 (SL)      TO  SS03V7-LIT3.
067330991021           MOVE WS-SS03-LIT4 (SL)      TO  SS03V7-LIT4.
067340991021           GO TO LS1-0050-0030.
067350991021
067360991213       LS1-0050-0020-0070.
067370991213
067380991213           IF  WS-STMTVRSN   NOT EQUAL TO  8
067390991213               GO TO LS1-0050-0020-0080
067400991213           END-IF.
067410991213
067420991213           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
067430991213              MOVE WS-SS03-LIT0 (SL)      TO  SS03V8-LIT0
067440991213           END-IF.
067450991213              MOVE WS-SS03-LIT1 (SL)      TO  SS03V8-LIT1.
067460991213              MOVE WS-SS03-LIT2 (SL)      TO  SS03V8-LIT2.
067470991213              MOVE WS-SS03-LIT3 (SL)      TO  SS03V8-LIT3.
067480991213              MOVE WS-SS03-LIT4 (SL)      TO  SS03V8-LIT4.
067490991213              GO TO LS1-0050-0030.
067500991213
067510991213       LS1-0050-0020-0080.
067520990921
067530990921           IF WS-LDA-PRT-NET-INVEST  IS EQUAL TO  "Y"
067540990921              MOVE WS-SS03-LIT0 (SL)      TO  SS03-LIT0
067550991013           END-IF.
067560991013              MOVE WS-SS03-LIT1 (SL)      TO  SS03-LIT1.
067570991013              MOVE WS-SS03-LIT2 (SL)      TO  SS03-LIT2.
067580991013              MOVE WS-SS03-LIT3 (SL)      TO  SS03-LIT3.
067590991013              MOVE WS-SS03-LIT4 (SL)      TO  SS03-LIT4.
067600000000
067610000000       LS1-0050-0030.
067620990921
067630990925           SET SPL                        UP BY  1.
067640990925           MOVE STMT-SUM-03               TO  WS-STMTPGE-LNE (SPL).
067650990925
067660990925           IF WS-STMTVRSN                 IS EQUAL TO  6
067670990925              SET SPL                     UP BY  1
067680990925           END-IF.
067690000000
067700000000      *****
067710000000      *****    Format statement summary line 4, one line for each fund.
067720000000      *****
067730000000
067740000000           SET SAF                     TO  1.
067750000000
067760000000       LS1-0060.
067770990921
067780000000           IF  SAF        IS GREATER THAN  WS-SAF-LST
067790990921               GO TO LS1-0080
067800990921           END-IF
067810990921
067820000000           MOVE SPACES                 TO  STMT-SUM-04.
067830000000
067840000000           IF  WS-STMTVRSN   NOT EQUAL TO  1
067850990922               GO TO LS1-0060-0010
067860990922           END-IF
067870990922
067880000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V1-CURRENCY.
067890000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
067900000000                                       TO  SS04V1-INVESTMENT-NAME.
067910990922           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
067920990922              MOVE WS-SAF-NET-INVESTED (SAF)
067930000000                                       TO  SS04V1-NET-INVESTED
067940990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
067950990922                                       TO  SS04V1-NET-INVESTED-SIGN
067960990922           END-IF
067970990922
067980000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
067990000000                                       TO  SS04V1-LAST-UNIT-PRICE.
068000000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
068010000000                                       TO  SS04V1-LAST-UNIT-PRICE-SIGN.
068020000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
068030000000                                       TO  SS04V1-NO-OF-UNITS.
068040000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
068050000000                                       TO  SS04V1-NO-OF-UNITS-SIGN.
068060000000           MOVE WS-SAF-MARKET-VALUE (SAF)
068070000000                                       TO  SS04V1-MARKET-VALUE.
068080000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
068090000000                                       TO  SS04V1-MARKET-VALUE-SIGN.
068100990922980417     IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
068110990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
068120000000                                       TO  SS04V1-ACB-UNIT-PRICE
068130990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
068140990922                                       TO  SS04V1-ACB-UNIT-PRICE-SIGN
068150990922           END-IF
068160990922
068170000000           GO TO LS1-0060-9999.
068180000000
068190000000       LS1-0060-0010.
068200990922
068210000000           IF  WS-STMTVRSN   NOT EQUAL TO  2
068220990922               GO TO LS1-0060-0020
068230990922           END-IF
068240990922
068250000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V2-CURRENCY.
068260000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
068270000000                                       TO  SS04V2-INVESTMENT-NAME.
068280990922           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
068290990922              MOVE WS-SAF-NET-INVESTED (SAF)
068300000000                                       TO  SS04V2-NET-INVESTED
068310990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
068320990922                                       TO  SS04V2-NET-INVESTED-SIGN
068330990922           END-IF
068340990922
068350000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
068360000000                                       TO  SS04V2-LAST-UNIT-PRICE.
068370000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
068380000000                                       TO  SS04V2-LAST-UNIT-PRICE-SIGN.
068390000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
068400000000                                       TO  SS04V2-NO-OF-UNITS.
068410000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
068420000000                                       TO  SS04V2-NO-OF-UNITS-SIGN.
068430000000           MOVE WS-SAF-MARKET-VALUE (SAF)
068440000000                                       TO  SS04V2-MARKET-VALUE.
068450000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
068460000000                                       TO  SS04V2-MARKET-VALUE-SIGN.
068470990922980417     IF WS-LDA-CAPITAL-GAIN     IS NOT EQUAL TO  "N"
068480990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
068490000000                                       TO  SS04V2-ACB-UNIT-PRICE
068500990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
068510990922                                       TO  SS04V2-ACB-UNIT-PRICE-SIGN
068520990922           END-IF
068530990922
068540000000           GO TO LS1-0060-9999.
068550000000
068560000000       LS1-0060-0020.
068570990922
068580000000           IF  WS-STMTVRSN   NOT EQUAL TO  3
068590990922               GO TO LS1-0060-0030
068600990922           END-IF
068610990922
068620000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V3-CURRENCY.
068630000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
068640000000                                       TO  SS04V3-INVESTMENT-NAME.
068650990922           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
068660990922              MOVE WS-SAF-NET-INVESTED (SAF)
068670000000                                       TO  SS04V3-NET-INVESTED
068680990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
068690990922                                       TO  SS04V3-NET-INVESTED-SIGN
068700990922           END-IF
068710990922
068720000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
068730000000                                       TO  SS04V3-LAST-UNIT-PRICE.
068740000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
068750000000                                       TO  SS04V3-LAST-UNIT-PRICE-SIGN.
068760000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
068770000000                                       TO  SS04V3-NO-OF-UNITS.
068780000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
068790000000                                       TO  SS04V3-NO-OF-UNITS-SIGN.
068800000000           MOVE WS-SAF-MARKET-VALUE (SAF)
068810000000                                       TO  SS04V3-MARKET-VALUE.
068820000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
068830000000                                       TO  SS04V3-MARKET-VALUE-SIGN.
068840990922
068850990922980417     IF WS-LDA-CAPITAL-GAIN     IS NOT EQUAL TO  "N"
068860990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
068870000000                                       TO  SS04V3-ACB-UNIT-PRICE
068880990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
068890990922                                       TO  SS04V3-ACB-UNIT-PRICE-SIGN
068900990922           END-IF
068910990922
068920000000           GO TO LS1-0060-9999.
068930000000
068940000000       LS1-0060-0030.
068950990922
068960000000           IF  WS-STMTVRSN   NOT EQUAL TO  4
068970990922               GO TO LS1-0060-0040
068980990922           END-IF
068990990922
069000000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V4-CURRENCY.
069010000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
069020000000                                       TO  SS04V4-INVESTMENT-NAME.
069030990922           IF WS-LDA-PRT-NET-INVEST    IS EQUAL TO  "Y"
069040990922              MOVE WS-SAF-NET-INVESTED (SAF)
069050000000                                       TO  SS04V4-NET-INVESTED
069060990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
069070990922                                       TO  SS04V4-NET-INVESTED-SIGN
069080990922           END-IF
069090990922
069100000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
069110000000                                       TO  SS04V4-LAST-UNIT-PRICE.
069120000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
069130000000                                       TO  SS04V4-LAST-UNIT-PRICE-SIGN.
069140000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
069150000000                                       TO  SS04V4-NO-OF-UNITS.
069160000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
069170000000                                       TO  SS04V4-NO-OF-UNITS-SIGN.
069180000000           MOVE WS-SAF-MARKET-VALUE (SAF)
069190000000                                       TO  SS04V4-MARKET-VALUE.
069200000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
069210000000                                       TO  SS04V4-MARKET-VALUE-SIGN.
069220990922980417     IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
069230990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
069240000000                                       TO  SS04V4-ACB-UNIT-PRICE
069250990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
069260990922                                       TO  SS04V4-ACB-UNIT-PRICE-SIGN
069270990922           END-IF
069280990922
069290000000           GO TO LS1-0060-9999.
069300000000
069310000000       LS1-0060-0040.
069320990922
069330000000           IF  WS-STMTVRSN   NOT EQUAL TO  5
069340990922               GO TO LS1-0060-0050
069350990922           END-IF
069360990922
069370000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V5-CURRENCY.
069380000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
069390000000                                       TO  SS04V5-INVESTMENT-NAME.
069400990922           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
069410990922              MOVE WS-SAF-NET-INVESTED (SAF)
069420000000                                       TO  SS04V5-NET-INVESTED
069430990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
069440990922                                       TO  SS04V5-NET-INVESTED-SIGN
069450990922           END-IF
069460990922
069470000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
069480000000                                       TO  SS04V5-LAST-UNIT-PRICE.
069490000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
069500000000                                       TO  SS04V5-LAST-UNIT-PRICE-SIGN.
069510000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
069520000000                                       TO  SS04V5-NO-OF-UNITS.
069530000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
069540000000                                       TO  SS04V5-NO-OF-UNITS-SIGN.
069550000000           MOVE WS-SAF-MARKET-VALUE (SAF)
069560000000                                       TO  SS04V5-MARKET-VALUE.
069570000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
069580000000                                       TO  SS04V5-MARKET-VALUE-SIGN.
069590990922
069600990922980417     IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
069610990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
069620000000                                       TO  SS04V5-ACB-UNIT-PRICE
069630990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
069640990922                                       TO  SS04V5-ACB-UNIT-PRICE-SIGN
069650990922           END-IF
069660990922
069670000000           GO TO LS1-0060-9999.
069680000000
069690990922       LS1-0060-0050.
069700990922
069710990922           IF  WS-STMTVRSN   NOT EQUAL TO  6
069720990922               GO TO LS1-0060-0060
069730990922           END-IF
069740990922
069750990930           MOVE SPACE                  TO  SS04V6-CURRENCY.
069760990922           MOVE WS-SAF-INVESTMENT-NAME (SAF)
069770990922                                       TO  SS04V6-INVESTMENT-NAME.
069780990922           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
069790990922                                       TO  SS04V6-LAST-UNIT-PRICE.
069800990922           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
069810990922                                       TO  SS04V6-LAST-UNIT-PRICE-SIGN.
069820990922           MOVE WS-SAF-NO-OF-UNITS (SAF)
069830990922                                       TO  SS04V6-NO-OF-UNITS.
069840990922           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
069850990922                                       TO  SS04V6-NO-OF-UNITS-SIGN.
069860990922           MOVE WS-SAF-MARKET-VALUE (SAF)
069870990922                                       TO  SS04V6-MARKET-VALUE.
069880990922           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
069890990922                                       TO  SS04V6-MARKET-VALUE-SIGN.
069900990922           GO TO LS1-0060-9999.
069910990922
069920991021       LS1-0060-0060.
069930991021
069940991021           IF  WS-STMTVRSN   NOT EQUAL TO  7
069950991021               GO TO LS1-0060-0070
069960991021           END-IF
069970991021
069980991021           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V7-CURRENCY.
069990991021           MOVE WS-SAF-INVESTMENT-NAME (SAF)
070000991021                                       TO  SS04V7-INVESTMENT-NAME.
070010991021           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
070020991021              MOVE WS-SAF-NET-INVESTED (SAF)
070030991021                                       TO  SS04V7-NET-INVESTED
070040991021              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
070050991021                                       TO  SS04V7-NET-INVESTED-SIGN
070060991021           END-IF
070070991021
070080991021           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
070090991021                                       TO  SS04V7-LAST-UNIT-PRICE.
070100991021           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
070110991021                                       TO  SS04V7-LAST-UNIT-PRICE-SIGN.
070120991021           MOVE WS-SAF-NO-OF-UNITS (SAF)
070130991021                                       TO  SS04V7-NO-OF-UNITS.
070140991021           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
070150991021                                       TO  SS04V7-NO-OF-UNITS-SIGN.
070160991021           MOVE WS-SAF-MARKET-VALUE (SAF)
070170991021                                       TO  SS04V7-MARKET-VALUE.
070180991021           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
070190991021                                       TO  SS04V7-MARKET-VALUE-SIGN.
070200991021980417     IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
070210991021              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
070220991021                                       TO  SS04V7-ACB-UNIT-PRICE
070230991021              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
070240991021                                       TO  SS04V7-ACB-UNIT-PRICE-SIGN
070250991021           END-IF
070260991021
070270991021           GO TO LS1-0060-9999.
070280991021
070290991213       LS1-0060-0070.
070300991213
070310991213           IF  WS-STMTVRSN   NOT EQUAL TO  8
070320991213               GO TO LS1-0060-0080
070330991213           END-IF
070340991213
070350991213           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04V8-CURRENCY.
070360991213           MOVE WS-SAF-INVESTMENT-NAME (SAF)
070370991213                                       TO  SS04V8-INVESTMENT-NAME.
070380991213           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
070390991213              MOVE WS-SAF-NET-INVESTED (SAF)
070400991213                                       TO  SS04V8-NET-INVESTED
070410991213              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
070420991213                                       TO  SS04V8-NET-INVESTED-SIGN
070430991213           END-IF
070440991213
070450991213           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
070460991213                                       TO  SS04V8-LAST-UNIT-PRICE.
070470991213           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
070480991213                                       TO  SS04V8-LAST-UNIT-PRICE-SIGN.
070490991213           MOVE WS-SAF-NO-OF-UNITS (SAF)
070500991213                                       TO  SS04V8-NO-OF-UNITS.
070510991213           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
070520991213                                       TO  SS04V8-NO-OF-UNITS-SIGN.
070530991213           MOVE WS-SAF-MARKET-VALUE (SAF)
070540991213                                       TO  SS04V8-MARKET-VALUE.
070550991213           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
070560991213                                       TO  SS04V8-MARKET-VALUE-SIGN.
070570991213980417     IF WS-LDA-CAPITAL-GAIN      IS NOT EQUAL TO  "N"
070580991213              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
070590991213                                       TO  SS04V8-ACB-UNIT-PRICE
070600991213              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
070610991213                                       TO  SS04V8-ACB-UNIT-PRICE-SIGN
070620991213           END-IF
070630991213
070640991213           GO TO LS1-0060-9999.
070650991213
070660991213
070670991213       LS1-0060-0080.
070680990922
070690000000           MOVE WS-SAF-CURRENCY (SAF)  TO  SS04-CURRENCY.
070700000000           MOVE WS-SAF-INVESTMENT-NAME (SAF)
070710000000                                       TO  SS04-INVESTMENT-NAME.
070720990922
070730990922           IF WS-LDA-PRT-NET-INVEST   IS EQUAL TO  "Y"
070740990922              MOVE WS-SAF-NET-INVESTED (SAF)
070750000000                                       TO  SS04-NET-INVESTED
070760990922              MOVE WS-SAF-NET-INVESTED-SIGN (SAF)
070770990922                                       TO  SS04-NET-INVESTED-SIGN
070780990922           END-IF
070790990922
070800000000           MOVE WS-SAF-LAST-UNIT-PRICE (SAF)
070810000000                                       TO  SS04-LAST-UNIT-PRICE.
070820000000           MOVE WS-SAF-LAST-UNIT-PRICE-SIGN (SAF)
070830000000                                       TO  SS04-LAST-UNIT-PRICE-SIGN.
070840000000           MOVE WS-SAF-NO-OF-UNITS (SAF)
070850000000                                       TO  SS04-NO-OF-UNITS.
070860000000           MOVE WS-SAF-NO-OF-UNITS-SIGN (SAF)
070870000000                                       TO  SS04-NO-OF-UNITS-SIGN.
070880000000           MOVE WS-SAF-MARKET-VALUE (SAF)
070890000000                                       TO  SS04-MARKET-VALUE.
070900000000           MOVE WS-SAF-MARKET-VALUE-SIGN (SAF)
070910000000                                       TO  SS04-MARKET-VALUE-SIGN.
070920990922980417     IF WS-LDA-CAPITAL-GAIN     IS NOT EQUAL TO  "N"
070930990922              MOVE WS-SAF-ACB-UNIT-PRICE (SAF)
070940000000                                       TO  SS04-ACB-UNIT-PRICE
070950990922              MOVE WS-SAF-ACB-UNIT-PRICE-SIGN (SAF)
070960990922                                       TO  SS04-ACB-UNIT-PRICE-SIGN
070970990922           END-IF.
070980990922
070990000000       LS1-0060-9999.
071000990922
071010000000           IF  SPL           IS LESS THAN  WS-SPL-DTLLST
071020000000               SET SPL              UP BY  1
071030990922               GO TO LS1-0070
071040990922           END-IF
071050000000
071060000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
071070990922
071080000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
071090990922               GO TO LS1-EXIT
071100990922           END-IF.
071110000000
071120000000       LS1-0070.
071130990922
071140000000           MOVE STMT-SUM-04            TO  WS-STMTPGE-LNE (SPL).
071150000000
071160990922           SET SAF                     UP BY  1.
071170000000           GO TO LS1-0060.
071180000000
071190000000       LS1-0080.
071200000000
071210000000      *****
071220000000      *****    If the currency totals cannot be formatted on the current
071230000000      *****    page, process the page and start a subsequent one, do not
071240000000      *****    print summary line 5.
071250000000      *****
071260000000
071270000000           SET WS-SPL-DTLTST           TO  SPL.
071280000000           SET SC                      TO  1.
071290000000
071300000000       LS1-0090.
071310990922
071320000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
071330000000               ADD 2                   TO  WS-SPL-DTLTST
071340990922               GO TO LS1-0100
071350990922           END-IF
071360000000
071370000000           IF  SC            IS LESS THAN  WS-SC-MAX
071380000000               SET SC               UP BY  1
071390990922               GO TO LS1-0090
071400990922           END-IF
071410000000
071420000000      *****
071430000000      *****    There are no currency total lines to be printed.
071440000000      *****
071450000000
071460000000           GO TO LS1-0160.
071470000000
071480000000       LS1-0100.
071490990922
071500000000           IF  SC        IS NOT LESS THAN  WS-SC-MAX
071510990922               GO TO LS1-0110
071520990922           END-IF
071530000000
071540000000           SET SC                   UP BY  1.
071550990922
071560000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "Y"
071570990922               ADD 1                   TO  WS-SPL-DTLTST
071580990922           END-IF
071590990922
071600000000           GO TO LS1-0100.
071610000000
071620000000       LS1-0110.
071630990922
071640990922           IF  WS-SPL-DTLTST  IS NOT GREATER THAN  WS-SPL-DTLLST
071650990922               GO TO LS1-0120
071660990922           END-IF
071670000000
071680000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
071690990922
071700000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
071710990922               GO TO LS1-EXIT
071720990922           END-IF
071730000000
071740000000           GO TO LS1-0130.
071750000000
071760000000       LS1-0120.
071770000000
071780000000      *****
071790000000      *****    Format statement summary line 5.
071800000000      *****
071810000000
071820990922           MOVE SPACES                   TO  STMT-SUM-05.
071830000000
071840990922           EVALUATE WS-STMTVRSN
071850990922             WHEN 1
071860990922               MOVE WS-SS05V1-LIT1 (SL)  TO  SS05V1-LIT1
071870990922             WHEN 2
071880990922               MOVE WS-SS05V2-LIT1 (SL)  TO  SS05V2-LIT1
071890990922             WHEN 3
071900990922               MOVE WS-SS05V3-LIT1 (SL)  TO  SS05V3-LIT1
071910990922             WHEN 4
071920990922               MOVE WS-SS05V4-LIT1 (SL)  TO  SS05V4-LIT1
071930990922             WHEN 5
071940990922               MOVE WS-SS05V5-LIT1 (SL)  TO  SS05V5-LIT1
071950990922             WHEN 6
071960990922               MOVE WS-SS05V6-LIT1 (SL)  TO  SS05V6-LIT1
071970991021991021       WHEN 7
071980991021991021         MOVE WS-SS05V1-LIT1 (SL)  TO  SS05V7-LIT1
071990991213             WHEN 8
072000991213               MOVE WS-SS05V1-LIT1 (SL)  TO  SS05V8-LIT1
072010990922             WHEN OTHER
072020990922               MOVE WS-SS05-LIT1 (SL)      TO  SS05-LIT1
072030990922           END-EVALUATE.
072040000000
072050000000       LS1-0120-0010.
072060990922
072070990922           SET SPL                     UP BY  1.
072080000000           MOVE STMT-SUM-05            TO  WS-STMTPGE-LNE (SPL).
072090000000
072100000000       LS1-0130.
072110000000
072120000000      *****
072130000000      *****    Format statement summary line 6, one line for each
072140000000      *****    applicable currency.
072150000000      *****
072160000000
072170000000           SET SC                      TO  1.
072180000000
072190000000       LS1-0140.
072200990922
072210000000           IF  SC         IS GREATER THAN  WS-SC-MAX
072220990922               GO TO LS1-0160
072230990922           END-IF
072240000000
072250000000           IF  WS-SC-RPTIND (SC) EQUAL TO  "N"
072260990922               GO TO LS1-0150
072270990922           END-IF
072280000000
072290000000           MOVE SPACES                 TO  STMT-SUM-06.
072300000000
072310000000           IF  WS-STMTVRSN    IS EQUAL TO  1
072320000000               MOVE WS-SS06-LIT1 (SL)  TO  SS06V1-LIT1
072330000000               MOVE WS-SC-CURRDESC (SC, SCL)
072340000000                                       TO  SS06V1-LIT-CURRDESC
072350000000               MOVE WS-SC-MARKET-VALUE (SC)
072360000000                                       TO  SS06V1-MARKET-VALUE
072370990922               GO TO LS1-0140-0010
072380990922           END-IF
072390000000
072400000000           IF  WS-STMTVRSN    IS EQUAL TO  2
072410000000               MOVE WS-SS06-LIT1 (SL)  TO  SS06V2-LIT1
072420000000               MOVE WS-SC-CURRDESC (SC, SCL)
072430000000                                       TO  SS06V2-LIT-CURRDESC
072440000000               MOVE WS-SC-MARKET-VALUE (SC)
072450000000                                       TO  SS06V2-MARKET-VALUE
072460990922               GO TO LS1-0140-0010
072470990922           END-IF
072480000000
072490000000           IF  WS-STMTVRSN    IS EQUAL TO  3
072500000000               MOVE WS-SS06-LIT1 (SL)  TO  SS06V3-LIT1
072510000000               MOVE WS-SC-CURRDESC (SC, SCL)
072520000000                                       TO  SS06V3-LIT-CURRDESC
072530000000               MOVE WS-SC-MARKET-VALUE (SC)
072540000000                                       TO  SS06V3-MARKET-VALUE
072550990922               GO TO LS1-0140-0010
072560990922           END-IF
072570000000
072580000000           IF  WS-STMTVRSN    IS EQUAL TO  4
072590000000               MOVE WS-SS06-LIT1 (SL)  TO  SS06V4-LIT1
072600000000               MOVE WS-SC-CURRDESC (SC, SCL)
072610000000                                       TO  SS06V4-LIT-CURRDESC
072620000000               MOVE WS-SC-MARKET-VALUE (SC)
072630000000                                       TO  SS06V4-MARKET-VALUE
072640990922               GO TO LS1-0140-0010
072650990922           END-IF
072660000000
072670000000           IF  WS-STMTVRSN    IS EQUAL TO  5
072680000000               MOVE WS-SS06-LIT1 (SL)  TO  SS06V5-LIT1
072690000000               MOVE WS-SC-CURRDESC (SC, SCL)
072700000000                                       TO  SS06V5-LIT-CURRDESC
072710000000               MOVE WS-SC-MARKET-VALUE (SC)
072720000000                                       TO  SS06V5-MARKET-VALUE
072730990922               GO TO LS1-0140-0010
072740990922           END-IF
072750990922
072760990922           IF  WS-STMTVRSN    IS EQUAL TO  6
072770990922               MOVE WS-SS06-LIT1 (SL)  TO  SS06V6-LIT1
072780990922               MOVE WS-SC-CURRDESC (SC, SCL)
072790990922                                       TO  SS06V6-LIT-CURRDESC
072800990922               MOVE WS-SC-MARKET-VALUE (SC)
072810990922                                       TO  SS06V6-MARKET-VALUE
072820990922               GO TO LS1-0140-0010
072830990922           END-IF
072840991021
072850991021           IF  WS-STMTVRSN    IS EQUAL TO  7
072860991021               MOVE WS-SS06-LIT1 (SL)  TO  SS06V7-LIT1
072870991021               MOVE WS-SC-CURRDESC (SC, SCL)
072880991021                                       TO  SS06V7-LIT-CURRDESC
072890991021               MOVE WS-SC-MARKET-VALUE (SC)
072900991021                                       TO  SS06V7-MARKET-VALUE
072910991021               GO TO LS1-0140-0010
072920991021           END-IF
072930991213
072940991213           IF  WS-STMTVRSN    IS EQUAL TO  8
072950991213               MOVE WS-SS06-LIT1 (SL)  TO  SS06V8-LIT1
072960991213               MOVE WS-SC-CURRDESC (SC, SCL)
072970991213                                       TO  SS06V8-LIT-CURRDESC
072980991213               MOVE WS-SC-MARKET-VALUE (SC)
072990991213                                       TO  SS06V8-MARKET-VALUE
073000991213               GO TO LS1-0140-0010
073010991213           END-IF
073020991213
073030990922           MOVE WS-SS06-LIT1 (SL)        TO  SS06-LIT1.
073040990922           MOVE WS-SC-CURRDESC (SC, SCL) TO  SS06-LIT-CURRDESC.
073050990922           MOVE WS-SC-MARKET-VALUE (SC)  TO  SS06-MARKET-VALUE.
073060000000
073070000000       LS1-0140-0010.
073080990922
073090990922           SET SPL                     UP BY  1.
073100000000           MOVE STMT-SUM-06            TO  WS-STMTPGE-LNE (SPL).
073110000000
073120000000       LS1-0150.
073130990922
073140990922           SET SC                      UP BY  1.
073150000000           GO TO LS1-0140.
073160000000
073170000000       LS1-0160.
073180971223
073190981207      *****
073200981207      *****    Determine if RRIF and LIF message is to be printed.
073210981207      *****
073220990922
073230981207           IF  WS-LDA-RIF-LIF-MSG IS NOT EQUAL TO "Y"
073240990922               GO TO LS1-0210
073250990922           END-IF
073260990922
073270981208           IF  WS-STMT-DTA-SW-60 IS NOT EQUAL TO "Y"
073280990922               GO TO LS1-0210
073290990922           END-IF
073300990922
073310981207           IF  WS-LDA-RRIF-FLAG IS NOT EQUAL TO "Y"
073320990922               GO TO LS1-0180
073330990922           END-IF
073340990922
073350981207           IF  AW-RIF-LIF-INDICATOR IS NOT EQUAL TO "R"
073360990922               GO TO LS1-0180
073370990922           END-IF
073380990922
073390981207           SET              SPL UP BY 5.
073400990922
073410981207           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
073420981207              SET           SPL DOWN BY 5
073430990922              GO TO LS1-0170
073440990922           END-IF
073450981207
073460981207           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
073470990922
073480981207           IF WS-PSRERR-SW NOT EQUAL TO SPACES
073490990922              GO TO LS1-EXIT
073500990922           END-IF.
073510981207
073520981207       LS1-0170.
073530990922
073540990120           MOVE WS-SYS-DATE               TO SS07V1-YEAR.
073550981207           MOVE WS-SS07V1-LIT1(SL)        TO SS07V1-LIT.
073560981207           MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V1-RRIF-LIF-AMT.
073570981207
073580000104           IF WS-STMTVRSN                 IS EQUAL TO  8
073590000104              MOVE WS-SYS-DATE               TO SS07V8-YEAR
073600000104              MOVE WS-SS07V1-LIT1(SL)        TO SS07V8-LIT
073610000104              MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V8-RRIF-LIF-AMT
073620000104           END-IF
073630000104
073640981207           SET SPL UP BY 4.
073650981207           MOVE STMT-SUM-07-V1    TO WS-STMTPGE-LNE (SPL).
073660000104
073670000104           IF WS-STMTVRSN    IS EQUAL TO  8
073680000104              MOVE STMT-SUM-07-V8    TO WS-STMTPGE-LNE (SPL)
073690000104           END-IF
073700000104
073710981207           GO TO LS1-0210.
073720981207
073730981207       LS1-0180.
073740990922
073750981207           IF  WS-LDA-ACCOUNT-LIF IS NOT EQUAL TO "Y"
073760990922               GO TO LS1-0210
073770990922           END-IF
073780981207
073790981207           IF  AW-RIF-LIF-INDICATOR IS NOT EQUAL TO "L"
073800990922               GO TO LS1-0210
073810990922           END-IF
073820981207
073830981207           SET              SPL UP BY 5.
073840990922
073850981207           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
073860981207              SET           SPL DOWN BY 5
073870990922              GO TO LS1-0190
073880990922           END-IF
073890981207
073900981207           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
073910990922
073920981207           IF WS-PSRERR-SW NOT EQUAL TO SPACES
073930990922              GO TO LS1-EXIT
073940990922           END-IF.
073950981207
073960981207       LS1-0190.
073970990922
073980990120           MOVE WS-SYS-DATE               TO SS07V1-YEAR.
073990981207           MOVE WS-SS07V1-LIT2(SL)        TO SS07V1-LIT.
074000981207           MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V1-RRIF-LIF-AMT.
074010981207
074020000104           IF WS-STMTVRSN                 IS EQUAL TO  8
074030000104              MOVE WS-SYS-DATE               TO SS07V8-YEAR
074040000104              MOVE WS-SS07V1-LIT2(SL)        TO SS07V8-LIT
074050000104              MOVE AW-MINIMUM-ANNUAL-PAYMENT TO SS07V8-RRIF-LIF-AMT
074060000104           END-IF
074070000104
074080981207           SET SPL UP BY 4.
074090981207           MOVE STMT-SUM-07-V1    TO WS-STMTPGE-LNE (SPL).
074100000104
074110000104           IF WS-STMTVRSN                 IS EQUAL TO  8
074120000104              MOVE STMT-SUM-07-V8    TO WS-STMTPGE-LNE (SPL)
074130000104           END-IF
074140000104
074150981207           SET              SPL UP BY 2.
074160990922
074170981207           IF SPL IS NOT GREATER THAN WS-SPL-DTLLST
074180981207              SET           SPL DOWN BY 2
074190990922              GO TO LS1-0200
074200990922           END-IF
074210981207
074220981207           PERFORM PRINT-STMT-RTN THRU PSR-EXIT.
074230990922
074240981207           IF WS-PSRERR-SW NOT EQUAL TO SPACES
074250990922              GO TO LS1-EXIT
074260990922           END-IF.
074270981207
074280981207       LS1-0200.
074290990922
074300990120           MOVE WS-SYS-DATE               TO SS08V1-YEAR.
074310981207           MOVE WS-SS08V1-LIT(SL)         TO SS08V1-LIT.
074320981207           MOVE AW-MAXIMUM-ANNUAL-PAYMENT TO SS08V1-RRIF-LIF-AMT.
074330981207
074340000104           IF WS-STMTVRSN                 IS EQUAL TO  8
074350000104              MOVE WS-SYS-DATE               TO SS08V8-YEAR
074360000104              MOVE WS-SS08V1-LIT(SL)         TO SS08V8-LIT
074370000104              MOVE AW-MAXIMUM-ANNUAL-PAYMENT TO SS08V8-RRIF-LIF-AMT
074380000104           END-IF
074390000104
074400981207           SET SPL UP BY 1.
074410000104           MOVE STMT-SUM-08-V1    TO WS-STMTPGE-LNE (SPL).
074420000104
074430000104           IF WS-STMTVRSN                 IS EQUAL TO  8
074440000104              MOVE STMT-SUM-08-V8    TO WS-STMTPGE-LNE (SPL)
074450000104           END-IF.
074460981207
074470981207       LS1-0210.
074480971223      *****
074490971223      *****    Determine if the Canadian dollar capital gains line is to
074500971223      *****    be printed.
074510971223      *****
074520971223
074530971223           IF  WS-CPG-FIXED   IS EQUAL TO  ZEROES
074540990929           OR  WS-STMTVRSN    IS EQUAL TO  6
074550990922               GO TO LS1-0390
074560990922           END-IF
074570971223
074580971223      *****
074590971223      *****    The Canadian dollar capital gains line is to be printed.
074600971223      *****    This line is to be printed on the second last detail
074610971223      *****    line, at least two lines below the last detail line
074620971223      *****    formatted.  If this is not possible, process the current
074630971223      *****    page and start a sbusequent one.
074640971223      *****
074650971223
074660971223           SUBTRACT 5                FROM  WS-SPL-DTLLST
074670971223                                   GIVING  WS-SPL-DTLTST.
074680971223           IF  WS-SPL-DTLTST GREATER THAN  SPL
074690990922               GO TO LS1-0370
074700990922           END-IF
074710990922
074720971223           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
074730990922
074740971223           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
074750990922               GO TO LS1-EXIT
074760990922           END-IF.
074770971223
074780971223       LS1-0370.
074790990922
074800971223           IF  WS-CPG-FIXED  IS LESS THAN  ZEROES
074810971223               MOVE "-"                TO  WS-CPG-FIXED-SIGN
074820990922           ELSE
074830990922               MOVE SPACES             TO  WS-CPG-FIXED-SIGN
074840990922           END-IF
074850971223
074860981207           MOVE SPACES                 TO  STMT-SUM-11.
074870971223
074880971223           IF  WS-STMTVRSN    IS EQUAL TO  1
074890981207               MOVE WS-SS11-LIT1 (SL)  TO  SS11V1-LIT1
074900981207               MOVE WS-CPG-FIXED       TO  SS11V1-CPG-FIXED
074910981207               MOVE WS-CPG-FIXED-SIGN  TO  SS11V1-CPG-FIXED-SIGN
074920990922               GO TO LS1-0380
074930990922           END-IF
074940971223
074950971223           IF  WS-STMTVRSN    IS EQUAL TO  2
074960981207               MOVE WS-SS11-LIT1 (SL)  TO  SS11V2-LIT1
074970981207               MOVE WS-CPG-FIXED       TO  SS11V2-CPG-FIXED
074980981207               MOVE WS-CPG-FIXED-SIGN  TO  SS11V2-CPG-FIXED-SIGN
074990990922               GO TO LS1-0380
075000990922           END-IF
075010971223
075020971223           IF  WS-STMTVRSN    IS EQUAL TO  3
075030981207               MOVE WS-SS11-LIT1 (SL)  TO  SS11V3-LIT1
075040981207               MOVE WS-CPG-FIXED       TO  SS11V3-CPG-FIXED
075050981207               MOVE WS-CPG-FIXED-SIGN  TO  SS11V3-CPG-FIXED-SIGN
075060990922               GO TO LS1-0380
075070990922           END-IF
075080971223
075090971223           IF  WS-STMTVRSN    IS EQUAL TO  4
075100981207               MOVE WS-SS11-LIT1 (SL)  TO  SS11V4-LIT1
075110981207               MOVE WS-CPG-FIXED       TO  SS11V4-CPG-FIXED
075120981207               MOVE WS-CPG-FIXED-SIGN  TO  SS11V4-CPG-FIXED-SIGN
075130990922               GO TO LS1-0380
075140990922           END-IF
075150971223
075160971223           IF  WS-STMTVRSN    IS EQUAL TO  5
075170981207               MOVE WS-SS11-LIT1 (SL)  TO  SS11V5-LIT1
075180981207               MOVE WS-CPG-FIXED       TO  SS11V5-CPG-FIXED
075190981207               MOVE WS-CPG-FIXED-SIGN  TO  SS11V5-CPG-FIXED-SIGN
075200990922               GO TO LS1-0380
075210990922           END-IF
075220990922
075230990922           IF  WS-STMTVRSN    IS EQUAL TO  6
075240990922               MOVE WS-SS11-LIT1 (SL)  TO  SS11V6-LIT1
075250990922               MOVE WS-CPG-FIXED       TO  SS11V6-CPG-FIXED
075260990922               MOVE WS-CPG-FIXED-SIGN  TO  SS11V6-CPG-FIXED-SIGN
075270990922               GO TO LS1-0380
075280990922           END-IF
075290991021
075300991021           IF  WS-STMTVRSN    IS EQUAL TO  7
075310991021               MOVE WS-SS11-LIT1 (SL)  TO  SS11V7-LIT1
075320991021               MOVE WS-CPG-FIXED       TO  SS11V7-CPG-FIXED
075330991021               MOVE WS-CPG-FIXED-SIGN  TO  SS11V7-CPG-FIXED-SIGN
075340991021               GO TO LS1-0380
075350991021           END-IF
075360991213
075370991213           IF  WS-STMTVRSN    IS EQUAL TO  8
075380991213               MOVE WS-SS11-LIT1 (SL)  TO  SS11V8-LIT1
075390991213               MOVE WS-CPG-FIXED       TO  SS11V8-CPG-FIXED
075400991213               MOVE WS-CPG-FIXED-SIGN  TO  SS11V8-CPG-FIXED-SIGN
075410991213               GO TO LS1-0380
075420991213           END-IF
075430991213
075440981207           MOVE WS-SS11-LIT1 (SL)      TO  SS11-LIT1.
075450981207           MOVE WS-CPG-FIXED           TO  SS11-CPG-FIXED.
075460981207           MOVE WS-CPG-FIXED-SIGN      TO  SS11-CPG-FIXED-SIGN.
075470971223
075480971223       LS1-0380.
075490990922
075500971223           SET SPL                     TO  WS-SPL-DTLLST.
075510971223           SET SPL                DOWN BY  2.
075520981207           MOVE STMT-SUM-11            TO  WS-STMTPGE-LNE (SPL).
075530971223
075540971223       LS1-0390.
075550000000
075560000000      *****
075570000000      *****    Process the current page.
075580000000      *****
075590000000
075600000000           PERFORM PRINT-STMT-RTN     THRU PSR-EXIT.
075610990922
075620000000           IF  WS-PSRERR-SW  NOT EQUAL TO  SPACES
075630990922               GO TO LS1-EXIT
075640990922           END-IF
075650990922
075660000000           CONTINUE.
075670000000
075680990922      **********
075690990922
075700000000       LS1-EXIT.
075710000000           EXIT.
075720990922
075730990922      **********
075740990922
075750000000      /
075760000000       PRINT-STMT-RTN.
075770000000
075780000000      ******************************************************************
075790000000      *                                                                *
075800000000      *    P R I N T   S T A T E M E N T   R O U T I N E               *
075810000000      *                                                                *
075820000000      ******************************************************************
075830000000
075840000000      *****
075850000000      *****    If the Statement Print Header record has not been
075860000000      *****    created, create it.  The record was formatted in the
075870000000      *****    LEVEL REINITIALIZATION 1 routine.
075880000000      *****
075890000000
075900000000           MOVE SPACES                 TO  WS-PSRERR-SW.
075910000000
075920990922           IF  WS-STMT-PRT-HDR-WSW     IS EQUAL TO  "Y"
075930990922               GO TO PSR-0010
075940990922           END-IF
075950990922
075960000000           WRITE STMT-PRT-HDR-RECD
075970000000               INVALID KEY
075980000000                   MOVE "H"            TO  WS-PSRERR-SW
075990000000                   GO TO PSR-9999.
076000000000
076010000000           ADD 1                       TO  WS-STMT-PRT-HDR-WCNT.
076020000000           MOVE "Y"                    TO  WS-STMT-PRT-HDR-WSW.
076030000000
076040000000       PSR-0010.
076050000000
076060000000      *****
076070000000      *****    Format and print the Statement Print Detail records.  One
076080000000      *****    record will be created for each statement line.  If the
076090000000      *****    statement line is spaces, no record will be created.
076100000000      *****
076110000000
076120000000           SET SPL                     TO  1.
076130000000
076140000000       PSR-0020.
076150990922
076160990922           IF  WS-STMTPGE-LNE (SPL)    IS EQUAL TO  SPACES
076170990922               GO TO PSR-0030
076180990922           END-IF
076190990922
076200000000           MOVE SPACES                 TO  STMT-PRT-DTL-RECD.
076210000000           MOVE INVESTOR-NO
076220000000                OF STMT-PRT-HDR-RECD   TO  INVESTOR-NO
076230000000                                           OF STMT-PRT-DTL-RECD.
076240000000           MOVE ACCOUNT
076250000000                OF STMT-PRT-HDR-RECD   TO  ACCOUNT
076260000000                                           OF STMT-PRT-DTL-RECD.
076270000000           MOVE LANGUAGE-CODE
076280000000                OF STMT-PRT-HDR-RECD   TO  LANGUAGE-CODE
076290000000                                           OF STMT-PRT-DTL-RECD.
076300000000           MOVE DEALER
076310000000                OF STMT-PRT-HDR-RECD   TO  DEALER
076320000000                                           OF STMT-PRT-DTL-RECD.
076330000000           MOVE DEALER-REP-CODE
076340000000                OF STMT-PRT-HDR-RECD   TO  DEALER-REP-CODE
076350000000                                           OF STMT-PRT-DTL-RECD.
076360000000           MOVE WS-SH-PGENO            TO  STMPGENO
076370000000                                           OF STMT-PRT-DTL-RECD.
076380000000           SET STMLNENO
076390000000               OF STMT-PRT-DTL-RECD    TO  SPL.
076400000000           MOVE WS-STMTPGE-LNE (SPL)   TO  STMLNE
076410000000                                           OF STMT-PRT-DTL-RECD.
076420980129           MOVE START-DATE
076430980129                OF STMT-PRT-HDR-RECD   TO  START-DATE
076440980129                                           OF STMT-PRT-DTL-RECD.
076450980129           MOVE END-DATE
076460980129                OF STMT-PRT-HDR-RECD   TO  END-DATE
076470980129                                           OF STMT-PRT-DTL-RECD.
076480980129
076490000000           WRITE STMT-PRT-DTL-RECD
076500000000               INVALID KEY
076510000000                   MOVE "D"            TO  WS-PSRERR-SW
076520000000                   GO TO PSR-9999.
076530000000
076540000000           ADD 1                       TO  WS-STMT-PRT-DTL-WCNT.
076550000000
076560000000       PSR-0030.
076570990922
076580000000           IF  SPL           IS LESS THAN  WS-SPL-MAX
076590000000               SET SPL              UP BY  1
076600990922               GO TO PSR-0020
076610990922           END-IF
076620990922
076630000000      *****
076640000000      *****    Format a new page for the account statement.
076650000000      *****
076660000000
076670000000           ADD 1                       TO  WS-SH-PGENO.
076680000000
076690990922           EVALUATE WS-STMTVRSN
076700990922             WHEN 1
076710990922               MOVE WS-SH-PGENO        TO  SH02V1-PGENO
076720990922             WHEN 2
076730990922               MOVE WS-SH-PGENO        TO  SH02V2-PGENO
076740990922             WHEN 3
076750990922               MOVE WS-SH-PGENO        TO  SH02V3-PGENO
076760990922             WHEN 4
076770990922               MOVE WS-SH-PGENO        TO  SH02V4-PGENO
076780990922             WHEN 5
076790990922               MOVE WS-SH-PGENO        TO  SH02V5-PGENO
076800990922             WHEN 6
076810990927               MOVE WS-SH-PGENO        TO  SH03V6-PGENO
076820991022             WHEN 7
076830991022               MOVE WS-SH-PGENO        TO  SH02V7-PGENO
076840991213             WHEN 8
076850991213               MOVE WS-SH-PGENO        TO  SH02V8-PGENO
076860990922             WHEN OTHER
076870990922               MOVE WS-SH-PGENO        TO  SH02-PGENO
076880990922           END-EVALUATE.
076890000000
076900000000       PSR-0040.
076910990927
076920990927           IF WS-STMTVRSN              IS NOT EQUAL TO  6
076930990927              SET SPL                  TO  WS-SPL-SH02
076940990927              MOVE STMT-HDG-02         TO  WS-STMTPGE-LNE (SPL)
076950990927           END-IF
076960990927
07697099092799927      IF WS-STMTVRSN              IS  EQUAL TO  6
07698099092799927         SET SPL                  TO  WS-SPL-SH03
07699099092799927         MOVE STMT-HDG-03         TO  WS-STMTPGE-LNE (SPL)
07700099092799927      END-IF
077010990927
077020000000           IF  WS-STMTVRSN    IS EQUAL TO  1
077030000000               MOVE SPACES             TO  WS-STMTPGE-DTL-V1
077040990922               GO TO PSR-0050
077050990922           END-IF
077060990922
077070000000           IF  WS-STMTVRSN    IS EQUAL TO  2
077080990716           AND WS-LDA-PRINT-HEADING IS EQUAL TO "N"
077090000000               MOVE SPACES             TO  WS-STMTPGE-DTL-V2
077100990922               GO TO PSR-0050
077110990922           END-IF
077120000000
077130990716           IF  WS-STMTVRSN    IS EQUAL TO  2
077140990716           AND WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
077150990726               MOVE SPACES             TO  WS-STMTPGE-DTL-V21
077160990922               GO TO PSR-0050
077170990922           END-IF
077180990726
077190000000           IF  WS-STMTVRSN    IS EQUAL TO  3
077200000000               MOVE SPACES             TO  WS-STMTPGE-DTL-V3
077210990922               GO TO PSR-0050
077220990922           END-IF
077230000000
077240000000           IF  WS-STMTVRSN    IS EQUAL TO  4
077250000000               MOVE SPACES             TO  WS-STMTPGE-DTL-V4
077260990922               GO TO PSR-0050
077270990922           END-IF
077280000000
077290000000           IF  WS-STMTVRSN    IS EQUAL TO  5
077300000000               MOVE SPACES             TO  WS-STMTPGE-DTL-V5
077310990922               GO TO PSR-0050
077320990922           END-IF
077330000000
077340990922           IF  WS-STMTVRSN    IS EQUAL TO  6
077350990922               MOVE SPACES             TO  WS-STMTPGE-DTL-V6
077360990922               GO TO PSR-0050
077370990922           END-IF
077380990922
077390991019           IF  WS-STMTVRSN    IS EQUAL TO  7
077400991019               MOVE SPACES             TO  WS-STMTPGE-DTL-V7
077410991019               GO TO PSR-0050
077420991019           END-IF
077430991019
077440991213RF6007     IF  WS-STMTVRSN    IS EQUAL TO  8
077450991213               MOVE SPACES             TO  WS-STMTPGE-DTL-V8
077460991213               GO TO PSR-0050
077470991213           END-IF
077480991213
077490000000           MOVE SPACES                 TO  WS-STMTPGE-DTL.
077500000000
077510000000       PSR-0050.
077520990922
077530000000           SET SPL                     TO  WS-SPL-DTLFST.
077540000000           GO TO PSR-EXIT.
077550000000
077560000000       PSR-9999.
077570000000
077580000000      *****
077590000000      *****    An error was detected attempting to write either a
077600000000      *****    Statement Print Header record or a Statement Print Detail
077610000000      *****    record.  An error message will be printed and the program
077620000000      *****    terminated.
077630000000      *****
077640000000
077650000000           IF  WS-PSRERR-SW   IS EQUAL TO  "H"
077660000000               SET MSG                 TO  6
077670990922           ELSE
077680990922               SET MSG                 TO  7
077690990922           END-IF
077700990922
077710000000           MOVE SPACES                 TO  ERROR-LINE-1.
077720000000           MOVE INVESTOR-NO
077730000000                OF STMT-PRT-HDR-RECD   TO  EL1-INVESTOR-NO.
077740000000           MOVE ACCOUNT
077750000000                OF STMT-PRT-HDR-RECD   TO  EL1-ACCOUNT.
077760000000           MOVE LANGUAGE-CODE
077770000000                OF STMT-PRT-HDR-RECD   TO  EL1-LANGUAGE-CODE.
077780000000           MOVE DEALER
077790000000                OF STMT-PRT-HDR-RECD   TO  EL1-DEALER.
077800000000           MOVE DEALER-REP-CODE
077810000000                OF STMT-PRT-HDR-RECD   TO  EL1-DEALER-REP-CODE.
077820000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
077830000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
077840000000
077850000000           MOVE SPACES                 TO  ERROR-LINE-1.
077860990922
077870000000           IF  WS-PSRERR-SW   IS EQUAL TO  "H"
077880000000               MOVE STMT-PRT-HDR-RECD  TO  ERROR-LINE-1
077890990922           ELSE
077900990922               MOVE STMT-PRT-DTL-RECD  TO  ERROR-LINE-1
077910990922           END-IF
077920990922
077930000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
077940000000
077950000000           MOVE SPACES                 TO  ERROR-LINE-1.
077960000000           MOVE WS-FILSTAT             TO  ERROR-LINE-1.
077970000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
077980000000
077990000000           PERFORM DUMP-ROUTINE       THRU DR-EXIT.
078000000000           MOVE HIGH-VALUES            TO  CURR-CONTROLS.
078010000000
078020990922      **********
078030990922
078040000000       PSR-EXIT.
078050000000           EXIT.
078060990922
078070990922      **********
078080990922
078090000000      /
078100000000       DUMP-ROUTINE.
078110000000
078120000000      ******************************************************************
078130000000      *                                                                *
078140000000      *    D U M P   R O U T I N E                                     *
078150000000      *                                                                *
078160000000      ******************************************************************
078170000000
078180000000      *****
078190000000      *****    Produce a program dump for analysis.
078200000000      *****
078210000000
078220000000           CALL    "QLREXHAN"       USING  DUMP-ARGUMENTS.
078230000000
078240990922      **********
078250990922
078260000000       DR-EXIT.
078270000000           EXIT.
078280990922
078290990922      **********
078300990922
078310000000      /
078320000000       LEVEL-SUMMARY-2.
078330000000
078340000000      ******************************************************************
078350000000      *                                                                *
078360000000      *    L E V E L   S U M M A R I Z A T I O N   2   R O U T I N E   *
078370000000      *                                                                *
078380000000      *        INVESTOR-NO:            Investor Number                 *
078390000000      *                                                                *
078400000000      ******************************************************************
078410000000
078420000000      *****
078430000000      *****    Null routine.
078440000000      *****
078450000000
078460000000           CONTINUE.
078470000000
078480990921      **********
078490990921
078500000000       LS2-EXIT.
078510000000           EXIT.
078520990921
078530990921      **********
078540990921
078550000000      /
078560000000       LEVEL-REINIT-2.
078570000000
078580000000      ******************************************************************
078590000000      *                                                                *
078600000000      *    L E V E L   R E I N I T I A L I Z A T I O N   2   R T N .   *
078610000000      *                                                                *
078620000000      *        INVESTOR-NO:            Investor Number                 *
078630000000      *                                                                *
078640000000      ******************************************************************
078650000000
078660000000      *****
078670000000      *****    Indicate that the Investor Information record and the
078680000000      *****    Dealer record have not yet been read.
078690000000      *****
078700000000
078710000000           MOVE "N"                    TO  WS-STMT-DTA-SW-01.
078720000000           MOVE "N"                    TO  WS-STMT-DTA-SW-05.
078730000000
078740000000      *****
078750000000      *****    Initialize the statement investor accounts table.
078760000000      *****
078770000000
078780000000           MOVE HIGH-VALUES            TO  WS-STMT-INVESTOR-ACCOUNT-TBL.
078790000000           MOVE ZEROES                 TO  WS-SIA-LST.
078800000000
078810990921      **********
078820990921
078830000000       LR2-EXIT.
078840000000           EXIT.
078850990921
078860990921      **********
078870990921
078880000000      /
078890000000       LEVEL-REINIT-1.
078900000000
078910000000      ******************************************************************
078920000000      *                                                                *
078930000000      *    L E V E L   R E I N I T I A L I Z A T I O N   1   R T N .   *
078940000000      *                                                                *
078950000000      *        ACCOUNT:                Account                         *
078960000000      *                                                                *
078970000000      ******************************************************************
078980000000
078990000000      *****
079000000000      *****    If Account is zeroes, bypass this routine.
079010000000      *****
079020000000
079030990921           IF CURR-ACCOUNT   IS EQUAL TO  ZEROES
079040990921              GO TO LR1-EXIT
079050990921           END-IF
079060990921
079070000000      *****
079080000000      *****    If one of the following conditions is detected, an error
079090000000      *****    message will be printed and the account will be bypassed.
079100000000      *****       1. Investor Information record missing.
079110000000      *****       2. Dealer record missing.
079120000000      *****       3. Account Detail record missing.
079130000000      *****    If this occurs, the program that creates the Statement
079140000000      *****    Data file should be reviewed.
079150000000      *****
079160000000
079170000000           MOVE "N"                    TO  WS-L01BYP-SW.
079180981208           MOVE "N"                    TO  WS-STMT-DTA-SW-60.
079190981208
079200000000           IF  WS-STMT-DTA-SW-01 EQUAL TO  "Y"
079210000000           AND WS-STMT-DTA-SW-05 EQUAL TO  "Y"
079220000000           AND PR-SECTION-NO  IS EQUAL TO  20
079230991103991103         GO TO LR1-0015
079240990921           END-IF
079250990921
079260000000           MOVE SPACES                 TO  ERROR-LINE-1.
079270000000
079280000000           IF  WS-STMT-DTA-SW-01 EQUAL TO  "N"
079290000000               SET MSG                 TO  8
079300990921               GO TO LR1-0010
079310990921           END-IF
079320000000
079330000000           IF  WS-STMT-DTA-SW-05 EQUAL TO  "N"
079340000000               SET MSG                 TO  9
079350990921               GO TO LR1-0010
079360990921           END-IF
079370000000
079380000000           IF  PR-SECTION-NO NOT EQUAL TO  20
079390000000               SET MSG                 TO  10
079400990921               GO TO LR1-0010
079410990921           END-IF
079420000000
079430000000           CONTINUE.
079440000000
079450000000       LR1-0010.
079460990921
079470000000           PERFORM PRINT-ERR-LINE     THRU PEL-EXIT.
079480000000           MOVE "Y"                    TO  WS-L01BYP-SW.
079490000000           GO TO LR1-EXIT.
079500991103
079510991103      *****************************************************
079520991103
079530991103       LR1-0015.
079540991103
079550991103      *****
079560991103      *****    Search the account type table for the correct
079570991103      *****    account type rule.
079580991103      *****
079590991103
079600991103           SET AT1                     TO  1.
079610991103           IF  WS-AT-LST        IS NOT GREATER THAN  ZEROES
079620991103               GO TO LR1-0020
079630991103           END-IF.
079640991103
079650991103       LR1-0015-000.
079660991103
079670190612      *RFS183449-Starts
079680190612991103*    IF  AD-ACCOUNT-TYPE-SHORT  IS EQUAL TO  WS-AT-CODE (AT1)
079690190612           INITIALIZE Lc-Acct-Type-Code
079700190619           PERFORM Get-Account-Type-Code
079710190612           IF Lc-Acct-Type-Code IS EQUAL TO  WS-AT-CODE (AT1)
079720190612      *RFS183449-Ends
079730991103               GO TO LR1-0020
079740991103           END-IF
079750991103
079760991103           SET AT1                  UP BY  1.
079770991103
079780991103           IF  AT1    IS NOT GREATER THAN  WS-AT-LST
079790991103               GO TO LR1-0015-000
079800991103           END-IF.
079810991103
079820991103      *************************************************************
079830991103
079840000000       LR1-0020.
079850000000
079860000000      *****
079870000000      *****    Find the account information in the investor account
079880000000      *****    table.  Format the nominee and registered contribution
079890000000      *****    data and print a message, if required.
079900000000      *****
079910000000
079920000000           MOVE SPACES                 TO  WS-NOMINEE.
079930000000           MOVE SPACES                 TO  WS-NOMINEE-CODE.
079940000000           MOVE SPACES                 TO  WS-NOMINEE-NAME.
079950000000           MOVE SPACES                 TO  WS-NOMINEES-ACCT-NO.
079960971224           MOVE ZEROES                 TO  WS-CPG-FIXED.
079970971224           MOVE SPACES                 TO  WS-CPG-FIXED-SIGN.
079980000000           MOVE ZEROES                 TO  WS-REG-CTB-CUR.
079990000000           MOVE SPACES                 TO  WS-REG-CTB-CUR-SIGN.
080000000000
080010000000           SEARCH ALL WS-SIA-ENT
080020000000               AT END
080030000000                   GO TO LR1-0020-0020
080040000000               WHEN WS-SIA-ACCT (SIA)
080050000000                              IS EQUAL TO  AD-ACCOUNT
080060000000                   MOVE WS-SIA-NOMCDE (SIA)
080070000000                                       TO  WS-NOMINEE-CODE
080080000000                   MOVE WS-SIA-NOMNAM (SIA)
080090000000                                       TO  WS-NOMINEE-NAME
080100000000                   MOVE WS-SIA-NOMACCT (SIA)
080110000000                                       TO  WS-NOMINEES-ACCT-NO
080120000000                   MOVE WS-SIA-REGCTB (SIA)
080130000000                                       TO  WS-REG-CTB-CUR
080140000000                   MOVE WS-SIA-REGCTBSGN (SIA)
080150020918                                       TO  WS-REG-CTB-CUR-SIGN.
080160000000
080170000000       LR1-0020-0020.
080180000000
080190000000      *****
080200000000      *****    If the account is flagged as NOMINEE but no nominee data
080210000000      *****    was found, an error message will be printed and the
080220000000      *****    nominee name will be defaulted to 'NOMINEE'.  If this
080230000000      *****    occurs, the program that creates the Statement Data file
080240000000      *****    should be reviewed.
080250000000      *****
080260000000
080270000000           IF  AD-NOMINEE     IS EQUAL TO  "Y"
080280000000           AND WS-NOMINEE-CODE   EQUAL TO  SPACES
080290000000           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
080300000000           AND WS-NOMINEES-ACCT-NO
080310000000                              IS EQUAL TO  SPACES
080320000000               MOVE SPACES             TO  ERROR-LINE-1
080330000000               SET MSG                 TO  12
080340000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
080350000000               MOVE "NOMINEE"          TO  WS-NOMINEE-NAME.
080360000000
080370000000      *****
080380000000      *****    If the account is not flagged as NOMINEE but nominee data
080390000000      *****    is found, an error message will be printed and the
080400000000      *****    nominee data will be set to spaces.  If this occurs, the
080410000000      *****    program that creates the Statement Data file should be
080420000000      *****    reviewed.
080430000000      *****
080440000000
080450000000           IF  AD-NOMINEE     IS EQUAL TO  "Y"
080460000000               GO TO LR1-0020-0030.
080470000000
080480000000           IF  WS-NOMINEE-CODE
080490000000                          IS NOT EQUAL TO  SPACES
080500000000           OR  WS-NOMINEE-NAME
080510000000                          IS NOT EQUAL TO  SPACES
080520000000           OR  WS-NOMINEES-ACCT-NO
080530000000                          IS NOT EQUAL TO  SPACES
080540000000               MOVE SPACES             TO  ERROR-LINE-1
080550000000               SET MSG                 TO  13
080560000000               PERFORM PRINT-ERR-LINE THRU PEL-EXIT
080570000000               MOVE SPACES             TO  WS-NOMINEE-CODE
080580000000               MOVE SPACES             TO  WS-NOMINEE-NAME
080590000000               MOVE SPACES             TO  WS-NOMINEES-ACCT-NO.
080600000000
080610000000       LR1-0020-0030.
080620000000
080630000000      *****
080640000000      *****    Format the nominee data, if applicable.
080650000000      *****
080660000000
080670000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
080680000000           AND WS-NOMINEE-NAME   EQUAL TO  SPACES
080690000000           AND WS-NOMINEES-ACCT-NO
080700000000                              IS EQUAL TO  SPACES
080710000000               GO TO LR1-0020-0040.
080720000000
080730000000           IF  WS-NOMINEES-ACCT-NO
080740000000                              IS EQUAL TO  SPACES
080750000000               MOVE WS-NOMINEE-NAME    TO  WS-NOMINEE
080760000000               GO TO LR1-0020-0040.
080770000000
080780000000           STRING WS-NOMINEE-NAME
080790000000                             DELIMITED BY  "  ",
080800000000                  " ("       DELIMITED BY  SIZE,
080810000000                  WS-NOMINEES-ACCT-NO
080820000000                             DELIMITED BY  " ",
080830000000                  ")"        DELIMITED BY  SIZE
080840000000                                     INTO  WS-NOMINEE.
080850000000
080860000000       LR1-0020-0040.
080870000000
080880000000      *****
080890000000      *****    The literals will be formatted depending on the account
080900000000      *****    language code.  If the language code is not handled,
080910000000      *****    default to English.
080920000000      *****
080930000000
080940000000           IF  AD-LANGUAGE    IS EQUAL TO  "E"
080950000000               SET SCL                 TO  1
080960990623               SET SHL                 TO  1
080970000000               SET SL                  TO  1
080980000000               GO TO LR1-0030.
080990000000
081000000000           IF  AD-LANGUAGE    IS EQUAL TO  "F"
081010000000               SET SCL                 TO  2
081020990623               SET SHL                 TO  2
081030000000               SET SL                  TO  2
081040000000               GO TO LR1-0030.
081050000000
081060000000           SET SCL                     TO  1.
081070990623           SET SHL                     TO  1.
081080000000           SET SL                      TO  1.
081090000000
081100000000       LR1-0030.
081110000000
081120000000      *****
081130000000      *****    Format the required fields.
081140000000      *****
081150000000
081160000000           MOVE II-END-DATE            TO  WS-END-DATE.
081170990923           MOVE II-END-DATE            TO  WS-END-DATE-6.
081180000000           MOVE "N"                    TO  WS-PSTCDEPRT-SW.
081190000000           MOVE II-START-DATE          TO  WS-START-DATE.
081200000000
081210000000      *****
081220000000      *****    The Statement File Generation (ANNSTMTJSI) formats and
081230000000      *****    creates the Dealer record using the Dealer & Dealer
081240000000      *****    Representative of the first account for the investor.  If
081250000000      *****    an investor has more than one account, the Dealer &
081260000000      *****    Dealer Representative of the accounts may differ.  If
081270000000      *****    this is the case, the Dealer & Dealer Representative
081280000000      *****    names and address lines will be retrieved and formatted.
081290000000      *****
081300000000
081310000000           MOVE SPACES                 TO  WS-BRANCH-ADDR1.
081320000000           MOVE SPACES                 TO  WS-BRANCH-ADDR2.
081330000000           MOVE SPACES                 TO  WS-BRANCH-ADDR3.
081340000000           MOVE SPACES                 TO  WS-BRANCH-ADDR4.
081350000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
081360000000           MOVE SPACES                 TO  WS-DEALER-REP-NAMES.
081370000000           MOVE ZEROES                 TO  WS-FAX.
081380000000           MOVE ZEROES                 TO  WS-PHONE.
081390000000           MOVE SPACES                 TO  WS-REP-NAME.
081400990115           MOVE SPACES                 TO  WS-REP-NAME-1.
081410000000
081420000000           IF  AD-SALESREP    IS EQUAL TO  DL-SALESREP
081430000000           AND AD-DEALER      IS EQUAL TO  DL-DEALER
081440000000               MOVE DL-BRANCH-ADDR1    TO  WS-BRANCH-ADDR1
081450000000               MOVE DL-BRANCH-ADDR2    TO  WS-BRANCH-ADDR2
081460000000               MOVE DL-BRANCH-ADDR3    TO  WS-BRANCH-ADDR3
081470000000               MOVE DL-BRANCH-ADDR4    TO  WS-BRANCH-ADDR4
081480000000               STRING DL-DEALER-NAME
081490000000                             DELIMITED BY  "  ",
081500000000                      " /"   DELIMITED BY  SIZE,
081510000000                      DL-REP-NAME
081520000000                             DELIMITED BY  SIZE
081530000000                                     INTO  WS-DEALER-REP-NAMES
081540000000               MOVE DL-FAX             TO  WS-FAX
081550000000               MOVE DL-PHONE           TO  WS-PHONE
081560990113               MOVE DL-REP-NAME        TO  WS-REP-NAME-1
081570000000               GO TO LR1-0030-0090.
081580000000
081590000000      *****
081600000000      *****    The account Dealer & Dealer Representative are different
081610000000      *****    than the dealer.  The following steps will be executed to
081620000000      *****    retrieve and format the Dealer & Dealer Representative
081630000000      *****    names and address lines.
081640000000      *****       1. Read the Dealer file for the name.
081650000000      *****       2. Read the Dealer Representative for the name, fax
081660000000      *****          and phone number.
081670000000      *****       3. Read the Dealer Representative Address for the
081680000000      *****          address lines.
081690000000      *****       4. If required, read the Institution Branch and
081700000000      *****          Institution Address for the address lines.
081710000000      *****       5. Format the address lines field by field, in case
081720000000      *****          one or more of the fields contain spaces.
081730000000      *****       6. Compress the address lines.
081740000000      *****
081750000000
081760000000           INITIALIZE MFADEAP.
081770000000           MOVE AD-DEALER              TO  DEALER-CODE
081780000000                                           OF DEALER-RECD.
081790000000           READ DEALER-FILE
081800000000               INVALID KEY
081810000000                   MOVE "DEALER NAME NOT ON FILE            "
081820000000                                       TO  DEALER-NAME
081830000000                                           OF DEALER-RECD.
081840000000
081850000000           MOVE DEALER-NAME
081860000000                OF DEALER-RECD         TO  WS-DEALER-NAME.
081870000000
081880000000           INITIALIZE MFADEAREP.
081890000000           MOVE AD-DEALER              TO  DEALER-CODE
081900000000                                           OF DEALER-REP-RECD.
081910000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
081920000000                                           OF DEALER-REP-RECD.
081930000000           READ DEALER-REP-FILE
081940000000               INVALID KEY
081950000000                   INITIALIZE MFADEAREP.
081960000000
081970000000           MOVE SPACES                 TO  WS-ADRTMP.
081980000000           STRING SALUTATION-CODE
081990000000                  OF DEALER-REP-RECD
082000000000                             DELIMITED BY  " "
082010000000                                     INTO  WS-ADRTMP.
082020000000
082030000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
082040000000               STRING FIRST-NAME
082050000000                      OF DEALER-REP-RECD
082060000000                             DELIMITED BY  "  "
082070000000                                     INTO  WS-ADRTMP
082080000000             ELSE
082090000000               STRING WS-ADRTMP
082100000000                             DELIMITED BY  "  ",
082110000000                      " "    DELIMITED BY  SIZE,
082120000000                      FIRST-NAME
082130000000                      OF DEALER-REP-RECD
082140000000                             DELIMITED BY  "  "
082150000000                                     INTO  WS-REP-NAME
082160000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
082170000000
082180000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
082190000000               STRING INITIALS
082200000000                      OF DEALER-REP-RECD
082210000000                             DELIMITED BY  "  "
082220000000                                     INTO  WS-ADRTMP
082230000000             ELSE
082240000000               STRING WS-ADRTMP
082250000000                             DELIMITED BY  "  ",
082260000000                      " "    DELIMITED BY  SIZE,
082270000000                      INITIALS
082280000000                      OF DEALER-REP-RECD
082290000000                             DELIMITED BY  "  "
082300000000                                     INTO  WS-REP-NAME
082310000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
082320000000
082330000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
082340000000               STRING LAST-NAME
082350000000                      OF DEALER-REP-RECD
082360000000                             DELIMITED BY  "  "
082370000000                                     INTO  WS-ADRTMP
082380000000             ELSE
082390000000               STRING WS-ADRTMP
082400000000                             DELIMITED BY  "  ",
082410000000                      " "    DELIMITED BY  SIZE,
082420000000                      LAST-NAME
082430000000                      OF DEALER-REP-RECD
082440000000                             DELIMITED BY  "  "
082450000000                                     INTO  WS-REP-NAME
082460000000               MOVE WS-REP-NAME        TO  WS-ADRTMP.
082470000000
082480000000           MOVE WS-ADRTMP              TO  WS-REP-NAME.
082490000000
082500000000           IF  ADDR-NO
082510000000               OF DEALER-REP-RECD
082520000000                              IS EQUAL TO  ZEROES
082530000000               GO TO LR1-0030-0010.
082540000000
082550000000           INITIALIZE MFADRADRP.
082560000000           MOVE ADDR-NO
082570000000                OF DEALER-REP-RECD     TO  ADDR-NO
082580000000                                           OF DEALER-REP-ADDRESS-RECD.
082590000000            READ DEALER-REP-ADDRESS
082600000000                INVALID KEY
082610000000                    GO TO LR1-0030-0010.
082620000000
082630000000            MOVE CORR MFADRADRP        TO  MFAINADDP.
082640000000            GO TO LR1-0030-0030.
082650000000
082660000000       LR1-0030-0010.
082670990927
082680000000           INITIALIZE MFAINSBRP.
082690000000           MOVE INSTITUTION-CODE
082700000000                OF DEALER-RECD         TO  INSTITUTION-CODE
082710000000                                           OF INSTITUTION-BRANCH-RECD.
082720000000           MOVE BRANCH-CODE
082730000000                OF DEALER-REP-RECD     TO  BRANCH-CODE
082740000000                                           OF INSTITUTION-BRANCH-RECD.
082750000000           READ INSTITUTION-BRANCH
082760000000               INVALID KEY
082770000000                   GO TO LR1-0030-0020.
082780000000
082790000000           INITIALIZE MFAINADDP.
082800000000           MOVE ADDR-NO
082810000000                OF INSTITUTION-BRANCH-RECD
082820000000                                       TO  ADDR-NO
082830000000                                           OF INSTITUTION-ADDRESS-RECD.
082840000000           READ INSTITUTION-ADDRESS
082850000000               INVALID KEY
082860000000                   GO TO LR1-0030-0020.
082870000000
082880000000           GO TO LR1-0030-0030.
082890000000
082900000000       LR1-0030-0020.
082910990923
082920000000           INITIALIZE MFAINSBRP.
082930000000           MOVE INSTITUTION-CODE
082940000000                OF DEALER-RECD         TO  INSTITUTION-CODE
082950000000                                           OF INSTITUTION-BRANCH-RECD.
082960000000           MOVE BRANCH-CODE
082970000000                OF DEALER-RECD         TO  BRANCH-CODE
082980000000                                           OF INSTITUTION-BRANCH-RECD.
082990000000           READ INSTITUTION-BRANCH
083000000000               INVALID KEY
083010000000                   INITIALIZE MFAINADDP
083020000000                   GO TO LR1-0030-0030.
083030000000
083040000000           INITIALIZE MFAINADDP.
083050000000           MOVE ADDR-NO
083060000000                OF INSTITUTION-BRANCH-RECD
083070000000                                       TO  ADDR-NO
083080000000                                           OF INSTITUTION-ADDRESS-RECD.
083090000000           READ INSTITUTION-ADDRESS
083100000000               INVALID KEY
083110000000                   INITIALIZE MFAINADDP
083120000000                   GO TO LR1-0030-0030.
083130000000
083140000000       LR1-0030-0030.
083150990927
083160000000           MOVE SPACES                 TO  WS-ADRTMP.
083170000000           STRING STREET-NO
083180000000               OF INSTITUTION-ADDRESS
083190000000                             DELIMITED BY  "  "
083200000000                                     INTO  WS-ADRTMP.
083210000000
083220000000           IF  PROVINCE-CODE
083230000000               OF INSTITUTION-ADDRESS-RECD
083240000000                              IS EQUAL TO  "QC"
083250000000               GO TO LR1-0030-0030-0010.
083260000000
083270000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
083280000000               STRING STREET-NAME
083290000000                      OF INSTITUTION-ADDRESS
083300000000                             DELIMITED BY  "  "
083310000000                                     INTO  WS-ADRTMP
083320000000             ELSE
083330000000               STRING WS-ADRTMP
083340000000                             DELIMITED BY  "  ",
083350000000                      " "    DELIMITED BY  SIZE,
083360000000                      STREET-NAME
083370000000                      OF INSTITUTION-ADDRESS
083380000000                             DELIMITED BY  "  "
083390000000                                     INTO  WS-BRANCH-ADDR1
083400000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
083410000000
083420000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
083430000000               STRING STREET-TYPE-CODE
083440000000                      OF INSTITUTION-ADDRESS
083450000000                             DELIMITED BY  "  "
083460000000                                     INTO  WS-ADRTMP
083470000000             ELSE
083480000000               STRING WS-ADRTMP
083490000000                             DELIMITED BY  "  ",
083500000000                      " "    DELIMITED BY  SIZE,
083510000000                      STREET-TYPE-CODE
083520000000                      OF INSTITUTION-ADDRESS
083530000000                             DELIMITED BY  "  "
083540000000                                     INTO  WS-BRANCH-ADDR1
083550000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
083560000000
083570000000           GO TO LR1-0030-0030-0020.
083580000000
083590000000       LR1-0030-0030-0010.
083600990927
083610000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
083620000000               STRING STREET-TYPE-CODE
083630000000                      OF INSTITUTION-ADDRESS-RECD
083640000000                             DELIMITED BY  "  "
083650000000                                     INTO  WS-ADRTMP
083660000000             ELSE
083670000000               STRING WS-ADRTMP
083680000000                             DELIMITED BY  "  ",
083690000000                      " "    DELIMITED BY  SIZE,
083700000000                      STREET-TYPE-CODE
083710000000                      OF INSTITUTION-ADDRESS-RECD
083720000000                             DELIMITED BY  "  "
083730000000                                     INTO  WS-BRANCH-ADDR1
083740000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
083750000000
083760000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
083770000000               STRING STREET-NAME
083780000000                      OF INSTITUTION-ADDRESS-RECD
083790000000                             DELIMITED BY  "  "
083800000000                                     INTO  WS-ADRTMP
083810000000             ELSE
083820000000               STRING WS-ADRTMP
083830000000                             DELIMITED BY  "  ",
083840000000                      " "    DELIMITED BY  SIZE,
083850000000                      STREET-NAME
083860000000                      OF INSTITUTION-ADDRESS-RECD
083870000000                             DELIMITED BY  "  "
083880000000                                     INTO  WS-BRANCH-ADDR1
083890000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
083900000000
083910000000       LR1-0030-0030-0020.
083920990927
083930000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
083940000000               STRING STREET-DIRCT-CODE
083950000000                      OF INSTITUTION-ADDRESS
083960000000                             DELIMITED BY  "  "
083970000000                                     INTO  WS-ADRTMP
083980000000             ELSE
083990000000               STRING WS-ADRTMP
084000000000                             DELIMITED BY  "  ",
084010000000                      " "    DELIMITED BY  SIZE,
084020000000                      STREET-DIRCT-CODE
084030990922                      OF INSTITUTION-ADDRESS
084040000000                             DELIMITED BY  "  "
084050000000                                     INTO  WS-BRANCH-ADDR1
084060000000               MOVE WS-BRANCH-ADDR1    TO  WS-ADRTMP.
084070000000
084080000000           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR1.
084090000000
084100000000           MOVE MISC-FIRST-ADDR-LINE
084110000000                OF INSTITUTION-ADDRESS-RECD
084120000000                                       TO  WS-BRANCH-ADDR2.
084130000000           MOVE MISC-SECOND-ADDR-LINE
084140000000                OF INSTITUTION-ADDRESS-RECD
084150000000                                       TO  WS-BRANCH-ADDR3.
084160000000           MOVE CO-OR-ATTENTION
084170000000                OF INSTITUTION-ADDRESS-RECD
084180000000                                       TO  WS-BRANCH-ADDR4.
084190000000
084200980420980420     IF  WS-LDA-ATTN-LINE-PRT = "N"
084210980420980420         MOVE SPACES TO WS-BRANCH-ADDR4.
084220980420
084230000000           MOVE POSTAL-CODE
084240000000                OF INSTITUTION-ADDRESS TO  WS-POSTAL.
084250000000           MOVE SPACES                 TO  WS-POSTAL-OUT.
084260000000           IF  COUNTRY-CODE
084270000000               OF INSTITUTION-ADDRESS
084280000000                              IS EQUAL TO  "CAN"
084290000000               MOVE WS-POSTAL1         TO  WS-POSTAL-OUT1
084300000000               MOVE WS-POSTAL2         TO  WS-POSTAL-OUT2
084310000000             ELSE
084320000000               MOVE WS-POSTAL          TO  WS-POSTAL-OUT.
084330000000
084340000000           MOVE SPACES                 TO  WS-ADRTMP.
084350000000           STRING CITY
084360000000               OF INSTITUTION-ADDRESS
084370000000                             DELIMITED BY  "  "
084380000000                                     INTO  WS-ADRTMP.
084390000000
084400000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
084410000000               STRING PROVINCE-CODE
084420000000                      OF INSTITUTION-ADDRESS
084430000000                             DELIMITED BY  "  "
084440000000                                     INTO  WS-ADRTMP
084450000000               GO TO LR1-0030-0040.
084460000000
084470000000           IF  PROVINCE-CODE
084480000000               OF INSTITUTION-ADDRESS
084490000000                              IS EQUAL TO  SPACES
084500000000               GO TO LR1-0030-0040.
084510000000
084520000000           STRING WS-ADRTMP  DELIMITED BY  "  ",
084530000000                  ", "       DELIMITED BY  SIZE,
084540000000                  PROVINCE-CODE
084550000000                  OF INSTITUTION-ADDRESS
084560000000                             DELIMITED BY  "  "
084570000000                                     INTO  WS-BRANCH-ADDR5.
084580000000           MOVE WS-BRANCH-ADDR5        TO  WS-ADRTMP.
084590000000
084600000000       LR1-0030-0040.
084610000000           IF  WS-ADRTMP      IS EQUAL TO  SPACES
084620000000               STRING WS-POSTAL-OUT
084630000000                             DELIMITED BY  "  "
084640000000                                     INTO  WS-ADRTMP
084650000000             ELSE
084660000000               STRING WS-ADRTMP
084670000000                             DELIMITED BY  "  ",
084680000000                      "  "   DELIMITED BY  SIZE,
084690000000                      WS-POSTAL-OUT
084700000000                             DELIMITED BY  "  "
084710000000                                     INTO  WS-BRANCH-ADDR5
084720000000               MOVE WS-BRANCH-ADDR5    TO  WS-ADRTMP.
084730000000
084740000000           MOVE WS-ADRTMP              TO  WS-BRANCH-ADDR5.
084750000000
084760000000           IF  WS-BRANCH-ADDR1
084770000000                          IS NOT EQUAL TO  SPACES
084780000000               GO TO LR1-0030-0050.
084790000000
084800000000           IF  WS-BRANCH-ADDR2
084810000000                          IS NOT EQUAL TO  SPACES
084820000000               MOVE WS-BRANCH-ADDR2    TO  WS-BRANCH-ADDR1
084830000000               MOVE SPACES             TO  WS-BRANCH-ADDR2
084840000000               GO TO LR1-0030-0050.
084850000000
084860000000           IF  WS-BRANCH-ADDR3
084870000000                          IS NOT EQUAL TO  SPACES
084880000000               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR1
084890000000               MOVE SPACES             TO  WS-BRANCH-ADDR3
084900000000               GO TO LR1-0030-0050.
084910000000
084920000000           IF  WS-BRANCH-ADDR4
084930000000                          IS NOT EQUAL TO  SPACES
084940000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR1
084950000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
084960000000               GO TO LR1-0030-0050.
084970000000
084980000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR1.
084990000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
085000000000           GO TO LR1-0030-0080.
085010000000
085020000000       LR1-0030-0050.
085030000000           IF  WS-BRANCH-ADDR2
085040000000                          IS NOT EQUAL TO  SPACES
085050000000               GO TO LR1-0030-0060.
085060000000
085070000000           IF  WS-BRANCH-ADDR3
085080000000                          IS NOT EQUAL TO  SPACES
085090000000               MOVE WS-BRANCH-ADDR3    TO  WS-BRANCH-ADDR2
085100000000               MOVE SPACES             TO  WS-BRANCH-ADDR3
085110000000               GO TO LR1-0030-0060.
085120000000
085130000000           IF  WS-BRANCH-ADDR4
085140000000                          IS NOT EQUAL TO  SPACES
085150000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR2
085160000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
085170000000               GO TO LR1-0030-0060.
085180000000
085190000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR2.
085200000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
085210000000           GO TO LR1-0030-0080.
085220000000
085230000000       LR1-0030-0060.
085240990922
085250000000           IF  WS-BRANCH-ADDR3
085260000000                          IS NOT EQUAL TO  SPACES
085270000000               GO TO LR1-0030-0070.
085280000000
085290000000           IF  WS-BRANCH-ADDR4
085300000000                          IS NOT EQUAL TO  SPACES
085310000000               MOVE WS-BRANCH-ADDR4    TO  WS-BRANCH-ADDR3
085320000000               MOVE SPACES             TO  WS-BRANCH-ADDR4
085330000000               GO TO LR1-0030-0070.
085340000000
085350000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR3.
085360000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
085370000000           GO TO LR1-0030-0080.
085380000000
085390000000       LR1-0030-0070.
085400000000           IF  WS-BRANCH-ADDR4
085410000000                          IS NOT EQUAL TO  SPACES
085420000000               GO TO LR1-0030-0080.
085430000000
085440000000           MOVE WS-BRANCH-ADDR5        TO  WS-BRANCH-ADDR4.
085450000000           MOVE SPACES                 TO  WS-BRANCH-ADDR5.
085460000000
085470000000       LR1-0030-0080.
085480000000           STRING WS-DEALER-NAME
085490000000                             DELIMITED BY  "  ",
085500000000                  " /"       DELIMITED BY  SIZE,
085510000000                  WS-REP-NAME
085520000000                             DELIMITED BY  SIZE
085530000000                                     INTO  WS-DEALER-REP-NAMES.
085540990113
085550990113           MOVE WS-REP-NAME            TO  WS-REP-NAME-1.
085560000000           MOVE FAX-AREA
085570000000                OF DEALER-REP-RECD     TO  WS-SH-FAX-01-03.
085580000000           MOVE FAX-PHONE
085590000000                OF DEALER-REP-RECD     TO  WS-SH-FAX-04-10.
085600000000
085610000000           MOVE AREA-CODE
085620000000                OF DEALER-REP-RECD     TO  WS-SH-PHONE-01-03.
085630000000           MOVE PHONE-NO
085640000000                OF DEALER-REP-RECD     TO  WS-SH-PHONE-04-10.
085650000000
085660000000       LR1-0030-0090.
085670981230
085680981230      *****
085690981230      *****    CND wants to suppress Merrill Lynch from Statements
085700981230      *****    Dealer Code is 9270.
085710981230      *****
085720981230
085730990115           IF WS-LDA-MGMT-COM-CDE IS NOT EQUAL TO "CND"
085740990115           OR AD-DEALER IS NOT EQUAL TO "9270"
085750990525              GO TO LR1-0030-0095.
085760990116
085770990116           IF IS-DEALER-ACCOUNT-NO IS EQUAL TO SPACES
085780990116              PERFORM GET-DEALER-ACCOUNT THRU GDA-EXIT.
085790990116
085800990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KKAAAA"
085810990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KKK9999"
085820990116                 MOVE SPACES TO WS-DEALER-REP-NAMES
085830990116                 MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
085840990116              END-IF
085850990116           END-IF.
085860990116
085870990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KHBAAA"
085880990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KH9999"
085890990116                 MOVE SPACES TO WS-DEALER-REP-NAMES
085900990116                 MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
085910990116              END-IF
085920990116           END-IF.
085930990116
085940990116           IF IS-DEALER-ACCOUNT-NO IS >= "0A9KAAA"
085950990116              IF IS-DEALER-ACCOUNT-NO IS <= "0A9K999"
085960990116                 MOVE SPACES TO WS-DEALER-REP-NAMES
085970990116                 MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
085980990116              END-IF
085990990116           END-IF.
086000990116
086010990116           IF IS-DEALER-ACCOUNT-NO IS >= "0A9LAAA"
086020990116              IF IS-DEALER-ACCOUNT-NO IS <= "0A9L999"
086030990115               MOVE SPACES TO WS-DEALER-REP-NAMES
086040990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086050990116              END-IF
086060990116           END-IF.
086070990116
086080990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KFAAAA"
086090990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KF9999"
086100990116               MOVE SPACES TO WS-DEALER-REP-NAMES
086110990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086120990116              END-IF
086130990116           END-IF.
086140990104
086150990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KGAAAA"
086160990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KG9999"
086170990116               MOVE SPACES TO WS-DEALER-REP-NAMES
086180990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086190990116              END-IF
086200990116           END-IF.
086210990116
086220990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KJAAAA"
086230990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KJ9999"
086240990116               MOVE SPACES TO WS-DEALER-REP-NAMES
086250990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086260990116              END-IF
086270990116           END-IF.
086280990116
086290990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KNAAAA"
086300990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KN9999"
086310990116               MOVE SPACES TO WS-DEALER-REP-NAMES
086320990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086330990116              END-IF
086340990116           END-IF.
086350990116
086360990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KMAAAA"
086370990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KM9999"
086380990116               MOVE SPACES TO WS-DEALER-REP-NAMES
086390990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086400990116              END-IF
086410990116           END-IF.
086420990116
086430990116           IF IS-DEALER-ACCOUNT-NO IS >= "9KQAAAA"
086440990116              IF IS-DEALER-ACCOUNT-NO IS <= "9KQ9999"
086450990116               MOVE SPACES TO WS-DEALER-REP-NAMES
086460990116               MOVE WS-REP-NAME-1 TO WS-DEALER-REP-NAMES
086470990116              END-IF
086480990116           END-IF.
086490990116
086500981230       LR1-0030-0095.
086510981230
086520000000      *****
086530000000      *****    Format the account type description.
086540000000      *****
086550000000
086560000000           MOVE AD-GROUP               TO  WS-ACTTYPGRP.
086570000000           MOVE AD-ACCOUNT-TYPE        TO  WS-ACTTYPIN-PFX.
086580000000           MOVE AD-LOCKED-IN           TO  WS-ACTTYPLCK.
086590000000           MOVE AD-LANGUAGE            TO  WS-ACTTYPLNG.
086600000000           MOVE AD-NOMINEE             TO  WS-ACTTYPNOM.
086610190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  WS-ACTTYPSHT.
086620190619183449     MOVE Lc-Acct-Type-Code      TO  WS-ACTTYPSHT.
086630000000           MOVE AD-SPOUSAL             TO  WS-ACTTYPSPS.
086640990923
086650990923           IF WS-STMTVRSN              IS NOT EQUAL TO 6
086660990923              PERFORM FORMAT-ACTTYP-DESC THRU FAD-EXIT
086670990923           END-IF
086680990923
086690000000      *****
086700000000      *****    Initialize the statement for the new account.
086710000000      *****
086720000000
086730000000           MOVE SPACES                 TO  WS-STATEMENT-PAGE.
086740000000           MOVE 1                      TO  WS-SH-PGENO.
086750000000
086760000000      *****
086770000000      *****    Format statement heading line 1.
086780000000      *****
086790000000
086800000000           MOVE SPACES                 TO  STMT-HDG-01.
086810000000
086820000000           IF  WS-STMTVRSN    IS EQUAL TO  1
086830000000               MOVE WS-ACTTYPOUT       TO  SH01V1-ACCOUNT-TYPE
086840000000               MOVE WS-SH01-LIT1 (SL)  TO  SH01V1-LIT1
086850000000               MOVE WS-SH-START-DATE   TO  SH01V1-START-DATE
086860000000               MOVE WS-SH01-LIT2 (SL)  TO  SH01V1-LIT2
086870000000               MOVE WS-SH-END-DATE     TO  SH01V1-END-DATE
086880990922               GO TO LR1-0030-0100
086890990922           END-IF
086900000000
086910000000           IF  WS-STMTVRSN    IS EQUAL TO  2
086920000000               MOVE WS-ACTTYPOUT       TO  SH01V2-ACCOUNT-TYPE
086930000000               MOVE WS-SH01-LIT1 (SL)  TO  SH01V2-LIT1
086940000000               MOVE WS-SH-START-DATE   TO  SH01V2-START-DATE
086950000000               MOVE WS-SH01-LIT2 (SL)  TO  SH01V2-LIT2
086960000000               MOVE WS-SH-END-DATE     TO  SH01V2-END-DATE
086970990922               GO TO LR1-0030-0100
086980990922           END-IF
086990000000
087000000000           IF  WS-STMTVRSN    IS EQUAL TO  3
087010000000               MOVE WS-ACTTYPOUT       TO  SH01V3-ACCOUNT-TYPE
087020000000               MOVE WS-SH01-LIT1 (SL)  TO  SH01V3-LIT1
087030000000               MOVE WS-SH-START-DATE   TO  SH01V3-START-DATE
087040000000               MOVE WS-SH01-LIT2 (SL)  TO  SH01V3-LIT2
087050000000               MOVE WS-SH-END-DATE     TO  SH01V3-END-DATE
087060990922               GO TO LR1-0030-0100
087070990922           END-IF
087080000000
087090000000           IF  WS-STMTVRSN    IS EQUAL TO  4
087100000000               MOVE WS-ACTTYPOUT       TO  SH01V4-ACCOUNT-TYPE
087110000000               MOVE WS-SH01-LIT1 (SL)  TO  SH01V4-LIT1
087120000000               MOVE WS-SH-START-DATE   TO  SH01V4-START-DATE
087130000000               MOVE WS-SH01-LIT2 (SL)  TO  SH01V4-LIT2
087140000000               MOVE WS-SH-END-DATE     TO  SH01V4-END-DATE
087150990922               GO TO LR1-0030-0100
087160990922           END-IF
087170000000
087180000000           IF  WS-STMTVRSN    IS EQUAL TO  5
087190000000               MOVE WS-ACTTYPOUT       TO  SH01V5-ACCOUNT-TYPE
087200000000               MOVE WS-SH01-LIT1 (SL)  TO  SH01V5-LIT1
087210000000               MOVE WS-SH-START-DATE   TO  SH01V5-START-DATE
087220000000               MOVE WS-SH01-LIT2 (SL)  TO  SH01V5-LIT2
087230000000               MOVE WS-SH-END-DATE     TO  SH01V5-END-DATE
087240990922               GO TO LR1-0030-0100
087250990922           END-IF
087260990922
087270990922           IF  WS-STMTVRSN    IS EQUAL TO  6
087280990923               MOVE II-INVESTOR-NAME   TO  SH01V6-INVESTOR-NAME
087290990922               MOVE WS-SH01-LIT1 (SL)  TO  SH01V6-LIT1
087300990922               MOVE WS-SH-START-DATE   TO  SH01V6-START-DATE
087310990922               MOVE WS-SH01-LIT2 (SL)  TO  SH01V6-LIT2
087320990922               MOVE WS-SH-END-DATE     TO  SH01V6-END-DATE
087330990922               GO TO LR1-0030-0100
087340990922           END-IF
087350990922
087360991019           IF WS-STMTVRSN    IS EQUAL TO  7
087370991019              MOVE WS-ACTTYPOUT           TO  SH01V7-ACCOUNT-TYPE
087380991019              MOVE WS-SH01-LIT1 (SL)      TO  SH01V7-LIT1
087390991019              MOVE WS-SH-START-DATE       TO  SH01V7-START-DATE
087400991019              MOVE WS-SH01-LIT2 (SL)      TO  SH01V7-LIT2
087410991019              MOVE WS-SH-END-DATE         TO  SH01V7-END-DATE
087420991019              GO TO LR1-0030-0100
087430991019           END-IF
087440991213
087450991213RF6007     IF  WS-STMTVRSN    IS EQUAL TO  8
087460991213               MOVE WS-ACTTYPOUT       TO  SH01V8-ACCOUNT-TYPE
087470991213               MOVE WS-SH01-LIT1 (SL)  TO  SH01V8-LIT1
087480991213               MOVE WS-SH-START-DATE   TO  SH01V8-START-DATE
087490991213               MOVE WS-SH01-LIT2 (SL)  TO  SH01V8-LIT2
087500991213               MOVE WS-SH-END-DATE     TO  SH01V8-END-DATE
087510991213               GO TO LR1-0030-0100
087520991213           END-IF
087530991213
087540991019
087550991019           MOVE WS-ACTTYPOUT           TO  SH01-ACCOUNT-TYPE.
087560991019           MOVE WS-SH01-LIT1 (SL)      TO  SH01-LIT1.
087570991019           MOVE WS-SH-START-DATE       TO  SH01-START-DATE.
087580991019           MOVE WS-SH01-LIT2 (SL)      TO  SH01-LIT2.
087590991019           MOVE WS-SH-END-DATE         TO  SH01-END-DATE.
087600991019
087610000000       LR1-0030-0100.
087620990923
087630000000           SET SPL                     TO  WS-SPL-HDGFST.
087640000000           MOVE STMT-HDG-01            TO  WS-STMTPGE-LNE (SPL).
087650000000
087660000000      *****
087670000000      *****    Format statement heading line 2.
087680000000      *****
087690000000
087700000000           MOVE II-SIN-NO              TO  WS-SIN-NO.
087710000000           MOVE SPACES                 TO  WS-SH02-SIN-NO.
087720980206
087730980417980417     IF WS-LDA-SIN NOT = "N"
087740980206             MOVE WS-SIN-NO-1-3          TO  SH02-SIN-NO-1-3
087750980206             MOVE "-"                    TO  SH02-SIN-NO-LITDSH1
087760980206             MOVE WS-SIN-NO-4-6          TO  SH02-SIN-NO-4-6
087770980206             MOVE "-"                    TO  SH02-SIN-NO-LITDSH2
087780980206             MOVE WS-SIN-NO-7-9          TO  SH02-SIN-NO-7-9.
087790000000
087800020919
087810000000           MOVE SPACES                 TO  STMT-HDG-02.
087820000000
087830000000           IF  WS-STMTVRSN    IS EQUAL TO  1
087840991014           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
087850020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
087860020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V1-ACCOUNT
087870020921R13756       ELSE
087880020921               MOVE AD-ACCOUNT         TO  SH02V1-ACCOUNT
087890020921R13756       END-IF
087900000000               MOVE WS-SH02-SIN-NO     TO  SH02V1-SIN-NO
087910000000               MOVE AD-DEALER          TO  SH02V1-DEALER
087920000000               MOVE "-"                TO  SH02V1-LITDSH
087930000000               MOVE AD-SALESREP        TO  SH02V1-SALESREP
087940000000               MOVE WS-SH-END-DATE     TO  SH02V1-END-DATE
087950000000               MOVE AD-SPOUSAL-CONTRIB TO  SH02V1-SPOUSAL-CONTRIB
087960000000               MOVE WS-REG-CTB-CUR     TO  SH02V1-REG-CTB-CUR
087970000000               MOVE WS-REG-CTB-CUR-SIGN
087980000000                                       TO  SH02V1-REG-CTB-CUR-SIGN
087990000000               MOVE WS-SH-PGENO        TO  SH02V1-PGENO
088000000000               SET SPL              UP BY  3
088010990922               GO TO LR1-0030-0110
088020990922           END-IF
088030000000
088040990629           IF  WS-STMTVRSN    IS EQUAL TO  1
088050991014           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
088060020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
088070020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH022V1-ACCOUNT
088080020613R13756       ELSE
088090990629               MOVE AD-ACCOUNT         TO  SH022V1-ACCOUNT
088100020613R13756       END-IF
088110990629               MOVE AD-DEALER          TO  SH022V1-DEALER
088120990629               MOVE "-"                TO  SH022V1-LITDSH
088130990629               MOVE AD-SALESREP        TO  SH022V1-SALESREP
088140990629               MOVE WS-SH-END-DATE     TO  SH022V1-END-DATE
088150990629               MOVE AD-SPOUSAL-CONTRIB TO  SH022V1-SPOUSAL-CONTRIB
088160990629               MOVE WS-REG-CTB-CUR     TO  SH022V1-REG-CTB-CUR
088170990629               MOVE WS-REG-CTB-CUR-SIGN
088180990629                                       TO  SH022V1-REG-CTB-CUR-SIGN
088190990629               MOVE WS-SH-PGENO        TO  SH022V1-PGENO
088200990629               SET SPL              UP BY  3
088210990922               GO TO LR1-0030-0110
088220990922           END-IF
088230990629
088240000000           IF  WS-STMTVRSN    IS EQUAL TO  2
088250020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
088260020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V2-ACCOUNT
088270020613R13756       ELSE
088280000000               MOVE AD-ACCOUNT         TO  SH02V2-ACCOUNT
088290020613R13756       END-IF
088300000000               MOVE WS-SH02-SIN-NO     TO  SH02V2-SIN-NO
088310000000               MOVE AD-DEALER          TO  SH02V2-DEALER
088320000000               MOVE "-"                TO  SH02V2-LITDSH
088330000000               MOVE AD-SALESREP        TO  SH02V2-SALESREP
088340000000               MOVE WS-SH-END-DATE     TO  SH02V2-END-DATE
088350000000               MOVE AD-SPOUSAL-CONTRIB TO  SH02V2-SPOUSAL-CONTRIB
088360000000               MOVE WS-REG-CTB-CUR     TO  SH02V2-REG-CTB-CUR
088370000000               MOVE WS-REG-CTB-CUR-SIGN
088380000000                                       TO  SH02V2-REG-CTB-CUR-SIGN
088390000000               MOVE WS-SH-PGENO        TO  SH02V2-PGENO
088400000000               SET SPL              UP BY  3
088410990922               GO TO LR1-0030-0110
088420990922           END-IF
088430000000
088440000000           IF  WS-STMTVRSN    IS EQUAL TO  3
088450020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
088460020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V3-ACCOUNT
088470020613R13756       ELSE
088480000000               MOVE AD-ACCOUNT         TO  SH02V3-ACCOUNT
088490020613R13756       END-IF
088500000000               MOVE WS-SH02-SIN-NO     TO  SH02V3-SIN-NO
088510000000               MOVE AD-DEALER          TO  SH02V3-DEALER
088520000000               MOVE "-"                TO  SH02V3-LITDSH
088530000000               MOVE AD-SALESREP        TO  SH02V3-SALESREP
088540000000               MOVE WS-SH-END-DATE     TO  SH02V3-END-DATE
088550000000               MOVE AD-SPOUSAL-CONTRIB TO  SH02V3-SPOUSAL-CONTRIB
088560000000               MOVE WS-REG-CTB-CUR     TO  SH02V3-REG-CTB-CUR
088570000000               MOVE WS-REG-CTB-CUR-SIGN
088580000000                                       TO  SH02V3-REG-CTB-CUR-SIGN
088590000000               MOVE WS-SH-PGENO        TO  SH02V3-PGENO
088600000000               SET SPL              UP BY  4
088610990922               GO TO LR1-0030-0110
088620990922           END-IF
088630000000
088640000000           IF  WS-STMTVRSN    IS EQUAL TO  4
088650020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
088660020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V4-ACCOUNT
088670020613R13756       ELSE
088680000000               MOVE AD-ACCOUNT         TO  SH02V4-ACCOUNT
088690020613R13756       END-IF
088700000000               MOVE WS-SH02-SIN-NO     TO  SH02V4-SIN-NO
088710000000               MOVE AD-DEALER          TO  SH02V4-DEALER
088720000000               MOVE "-"                TO  SH02V4-LITDSH
088730000000               MOVE AD-SALESREP        TO  SH02V4-SALESREP
088740000000               MOVE WS-SH-END-DATE     TO  SH02V4-END-DATE
088750000000               MOVE AD-SPOUSAL-CONTRIB TO  SH02V4-SPOUSAL-CONTRIB
088760000000               MOVE WS-REG-CTB-CUR     TO  SH02V4-REG-CTB-CUR
088770000000               MOVE WS-REG-CTB-CUR-SIGN
088780000000                                       TO  SH02V4-REG-CTB-CUR-SIGN
088790000000               MOVE WS-SH-PGENO        TO  SH02V4-PGENO
088800000000               SET SPL              UP BY  4
088810990922               GO TO LR1-0030-0110
088820990922           END-IF
088830000000
088840000000           IF  WS-STMTVRSN    IS EQUAL TO  5
088850020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
088860020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V5-ACCOUNT
088870020613R13756       ELSE
088880000000               MOVE AD-ACCOUNT         TO  SH02V5-ACCOUNT
088890020613R13756       END-IF
088900000000               MOVE WS-SH02-SIN-NO     TO  SH02V5-SIN-NO
088910000000               MOVE AD-DEALER          TO  SH02V5-DEALER
088920000000               MOVE "-"                TO  SH02V5-LITDSH
088930000000               MOVE AD-SALESREP        TO  SH02V5-SALESREP
088940000000               MOVE WS-SH-END-DATE     TO  SH02V5-END-DATE
088950000000               MOVE AD-SPOUSAL-CONTRIB TO  SH02V5-SPOUSAL-CONTRIB
088960000000               MOVE WS-REG-CTB-CUR     TO  SH02V5-REG-CTB-CUR
088970000000               MOVE WS-REG-CTB-CUR-SIGN
088980000000                                       TO  SH02V5-REG-CTB-CUR-SIGN
088990000000               MOVE WS-SH-PGENO        TO  SH02V5-PGENO
089000980218               SET SPL              UP BY  4
089010990922               GO TO LR1-0030-0110
089020990922           END-IF
089030990922
089040990922           IF  WS-STMTVRSN    IS EQUAL TO  6
089050990927               MOVE WS-SH-PGENO        TO  SH03V6-PGENO
089060990922               GO TO LR1-0030-0110
089070990922           END-IF
089080990922
089090991019           IF  WS-STMTVRSN    IS EQUAL TO  7
089100991022           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
089110020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
089120020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V7-ACCOUNT
089130020613R13756       ELSE
089140991019              MOVE AD-ACCOUNT             TO  SH02V7-ACCOUNT
089150020613R13756       END-IF
089160991019              MOVE WS-SH02-SIN-NO         TO  SH02V7-SIN-NO
089170991019              MOVE AD-DEALER              TO  SH02V7-DEALER
089180991019              MOVE "-"                    TO  SH02V7-LITDSH
089190991019              MOVE AD-SALESREP            TO  SH02V7-SALESREP
089200991019              MOVE WS-SH-END-DATE         TO  SH02V7-END-DATE
089210991019              MOVE AD-SPOUSAL-CONTRIB     TO  SH02V7-SPOUSAL-CONTRIB
089220991019              MOVE WS-REG-CTB-CUR         TO  SH02V7-REG-CTB-CUR
089230991019              MOVE WS-REG-CTB-CUR-SIGN    TO  SH02V7-REG-CTB-CUR-SIGN
089240991019              MOVE WS-SH-PGENO            TO  SH02V7-PGENO
089250991019              SET SPL                  UP BY  3
089260991019              GO TO LR1-0030-0110
089270991019           END-IF
089280991022
089290991022           IF  WS-STMTVRSN    IS EQUAL TO  7
089300991022           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
089310020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
089320020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH022V7-ACCOUNT
089330020613R13756       ELSE
089340991022               MOVE AD-ACCOUNT         TO  SH022V7-ACCOUNT
089350020613R13756       END-IF
089360991022               MOVE AD-DEALER          TO  SH022V7-DEALER
089370991022               MOVE "-"                TO  SH022V7-LITDSH
089380991022               MOVE AD-SALESREP        TO  SH022V7-SALESREP
089390991022               MOVE WS-SH-END-DATE     TO  SH022V7-END-DATE
089400991022               MOVE AD-SPOUSAL-CONTRIB TO  SH022V7-SPOUSAL-CONTRIB
089410991022               MOVE WS-REG-CTB-CUR     TO  SH022V7-REG-CTB-CUR
089420991022               MOVE WS-REG-CTB-CUR-SIGN
089430991022                                       TO  SH022V7-REG-CTB-CUR-SIGN
089440991022               MOVE WS-SH-PGENO        TO  SH022V7-PGENO
089450991022               SET SPL              UP BY  3
089460991022               GO TO LR1-0030-0110
089470991022           END-IF
089480991213
089490991213           IF  WS-STMTVRSN    IS EQUAL TO  8
089500991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
089510020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
089520020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH02V8-ACCOUNT
089530020613R13756       ELSE
089540991213               MOVE AD-ACCOUNT         TO  SH02V8-ACCOUNT
089550020613R13756       END-IF
089560991213               MOVE WS-SH02-SIN-NO     TO  SH02V8-SIN-NO
089570991213               MOVE AD-DEALER          TO  SH02V8-DEALER
089580991213               MOVE "-"                TO  SH02V8-LITDSH
089590991213               MOVE AD-SALESREP        TO  SH02V8-SALESREP
089600991213               MOVE WS-SH-END-DATE     TO  SH02V8-END-DATE
089610991213               MOVE AD-SPOUSAL-CONTRIB TO  SH02V8-SPOUSAL-CONTRIB
089620991213               MOVE WS-REG-CTB-CUR     TO  SH02V8-REG-CTB-CUR
089630991213               MOVE WS-REG-CTB-CUR-SIGN
089640991213                                       TO  SH02V8-REG-CTB-CUR-SIGN
089650991213               MOVE WS-SH-PGENO        TO  SH02V8-PGENO
089660991213               SET SPL              UP BY  3
089670991213               GO TO LR1-0030-0110
089680991213           END-IF
089690991213
089700991213           IF  WS-STMTVRSN    IS EQUAL TO  8
089710991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
089720020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
089730020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH022V8-ACCOUNT
089740020613R13756       ELSE
089750991213               MOVE AD-ACCOUNT         TO  SH022V8-ACCOUNT
089760020613R13756       END-IF
089770991213               MOVE AD-DEALER          TO  SH022V8-DEALER
089780991213               MOVE "-"                TO  SH022V8-LITDSH
089790991213               MOVE AD-SALESREP        TO  SH022V8-SALESREP
089800991213               MOVE WS-SH-END-DATE     TO  SH022V8-END-DATE
089810991213               MOVE AD-SPOUSAL-CONTRIB TO  SH022V8-SPOUSAL-CONTRIB
089820991213               MOVE WS-REG-CTB-CUR     TO  SH022V8-REG-CTB-CUR
089830991213               MOVE WS-REG-CTB-CUR-SIGN
089840991213                                       TO  SH022V8-REG-CTB-CUR-SIGN
089850991213               MOVE WS-SH-PGENO        TO  SH022V8-PGENO
089860991213               SET SPL              UP BY  3
089870991213               GO TO LR1-0030-0110
089880991213           END-IF
089890991213
089900991019
089910020921R13756     IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
089920020921  ¦          MOVE WS-MSTR-ACCOUNT-NO  TO  SH02-ACCOUNT
089930020613R13756     ELSE
089940020613             MOVE AD-ACCOUNT             TO  SH02-ACCOUNT
089950020613R13756     END-IF
089960000000           MOVE WS-SH02-SIN-NO         TO  SH02-SIN-NO.
089970000000           MOVE AD-DEALER              TO  SH02-DEALER.
089980000000           MOVE "-"                    TO  SH02-LITDSH.
089990000000           MOVE AD-SALESREP            TO  SH02-SALESREP.
090000000000           MOVE WS-SH-END-DATE         TO  SH02-END-DATE.
090010000000           MOVE AD-SPOUSAL-CONTRIB     TO  SH02-SPOUSAL-CONTRIB.
090020000000           MOVE WS-REG-CTB-CUR         TO  SH02-REG-CTB-CUR.
090030000000           MOVE WS-REG-CTB-CUR-SIGN    TO  SH02-REG-CTB-CUR-SIGN.
090040000000           MOVE WS-SH-PGENO            TO  SH02-PGENO.
090050000000           SET SPL                  UP BY  3.
090060000000
090070000000       LR1-0030-0110.
090080990927
090090990927           IF WS-STMTVRSN              IS NOT EQUAL TO  6
090100990927              MOVE STMT-HDG-02            TO  WS-STMTPGE-LNE (SPL)
090110990927           END-IF
090120990927
090130990927           SET WS-SPL-SH02             TO  SPL.
090140000000
090150000000      *****
090160000000      *****    Format statement heading line 3.
090170000000      *****
090180000000
090190000000           MOVE SPACES                 TO  STMT-HDG-03.
090200000000
090210000000           IF  WS-STMTVRSN    IS EQUAL TO  1
090220000000               MOVE II-INVESTOR-NAME   TO  SH03V1-INVESTOR-NAME
090230000000               MOVE WS-DEALER-REP-NAMES
090240000000                                       TO  SH03V1-DEALER-REP-NAMES
090250990922
090260991014               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090270990525                  MOVE SPACES          TO  SH03V1-DEALER-REP-NAMES
090280990525                  MOVE WS-REP-NAME-1   TO  SH03V1-DEALER-REP-NAMES
090290990525               END-IF
090300991022
090310000000               SET SPL              UP BY  3
090320991014               GO TO LR1-0030-0120.
090330000000
090340000000           IF  WS-STMTVRSN    IS EQUAL TO  2
090350000000               MOVE II-INVESTOR-NAME   TO  SH03V2-INVESTOR-NAME
090360000000               MOVE WS-DEALER-REP-NAMES
090370000000                                       TO  SH03V2-DEALER-REP-NAMES
090380991014
090390991014               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090400991014                  MOVE SPACES          TO  SH03V2-DEALER-REP-NAMES
090410991014                  MOVE WS-REP-NAME-1   TO  SH03V2-DEALER-REP-NAMES
090420991014               END-IF
090430991014
090440000000               SET SPL              UP BY  3
090450991014               GO TO LR1-0030-0120.
090460000000
090470000000           IF  WS-STMTVRSN    IS EQUAL TO  3
090480000000               MOVE II-INVESTOR-NAME   TO  SH03V3-INVESTOR-NAME
090490000000               MOVE WS-DEALER-REP-NAMES
090500000000                                       TO  SH03V3-DEALER-REP-NAMES
090510991014               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090520991014                  MOVE SPACES          TO  SH03V3-DEALER-REP-NAMES
090530991014                  MOVE WS-REP-NAME-1   TO  SH03V3-DEALER-REP-NAMES
090540991014               END-IF
090550000000               SET SPL              UP BY  3
090560991014               GO TO LR1-0030-0120.
090570000000
090580000000           IF  WS-STMTVRSN    IS EQUAL TO  4
090590000000               MOVE II-INVESTOR-NAME   TO  SH03V4-INVESTOR-NAME
090600000000               MOVE WS-DEALER-REP-NAMES
090610000000                                       TO  SH03V4-DEALER-REP-NAMES
090620991014               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090630991014                  MOVE SPACES          TO  SH03V4-DEALER-REP-NAMES
090640991014                  MOVE WS-REP-NAME-1   TO  SH03V4-DEALER-REP-NAMES
090650991014               END-IF
090660000000               SET SPL              UP BY  2
090670991014               GO TO LR1-0030-0120.
090680000000
090690000000           IF  WS-STMTVRSN    IS EQUAL TO  5
090700000000               MOVE II-INVESTOR-NAME   TO  SH03V5-INVESTOR-NAME
090710000000               MOVE WS-DEALER-REP-NAMES
090720000000                                       TO  SH03V5-DEALER-REP-NAMES
090730991014               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090740991014                  MOVE SPACES          TO  SH03V5-DEALER-REP-NAMES
090750991014                  MOVE WS-REP-NAME-1   TO  SH03V5-DEALER-REP-NAMES
090760991014               END-IF
090770000000               SET SPL              UP BY  3
090780991014               GO TO LR1-0030-0120.
090790990922
090800991019           IF WS-STMTVRSN    IS EQUAL TO  7
090810991019              MOVE II-INVESTOR-NAME       TO  SH03V7-INVESTOR-NAME
090820991019              MOVE WS-DEALER-REP-NAMES    TO  SH03V7-DEALER-REP-NAMES
090830991019              IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090840991019                 MOVE SPACES          TO  SH03V7-DEALER-REP-NAMES
090850991019                 MOVE WS-REP-NAME-1   TO  SH03V7-DEALER-REP-NAMES
090860991019              END-IF
090870991019              SET SPL                  UP BY  3
090880991019              GO TO LR1-0030-0120
090890991019           END-IF
090900991213
090910991213           IF  WS-STMTVRSN    IS EQUAL TO  8
090920991213               MOVE II-INVESTOR-NAME   TO  SH03V8-INVESTOR-NAME
090930991213               MOVE WS-DEALER-REP-NAMES
090940991213                                       TO  SH03V8-DEALER-REP-NAMES
090950991213
090960991213               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
090970991213                  MOVE SPACES          TO  SH03V8-DEALER-REP-NAMES
090980991213                  MOVE WS-REP-NAME-1   TO  SH03V8-DEALER-REP-NAMES
090990991213               END-IF
091000991213
091010991213               SET SPL              UP BY  3
091020991213               GO TO LR1-0030-0120
091030991213           END-IF
091040991019
091050990923           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
091060990923
091070990923           IF  II-INVESTOR-ADDRESS1    IS EQUAL TO  SPACES
091080990923           AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
091090990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
091100990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
091110990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
091120990923               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
091130990923               MOVE "Y"                TO  WS-PSTCDEPRT-SW
091140990923           ELSE
091150990923               MOVE II-INVESTOR-ADDRESS1
091160990923                                       TO  WS-SH04-INVESTOR-ADDRESS1
091170990923           END-IF
091180990923
091190990922           IF  WS-STMTVRSN    IS EQUAL TO  6
091200990922               MOVE WS-SH04-INVESTOR-ADDRESS1
091210990923                                       TO  SH03V6-INVESTOR-ADDRESS1
091220990927               MOVE "PAGE"             TO  SH03V6-DEALER-REP-NAMES
091230990927               MOVE WS-SH-PGENO        TO  SH03V6-PGENO
091240990927               SET SPL              UP BY  1
091250990922               GO TO LR1-0030-0120
091260990922           END-IF
091270990922
091280000000           MOVE II-INVESTOR-NAME       TO  SH03-INVESTOR-NAME.
091290000000           MOVE WS-DEALER-REP-NAMES    TO  SH03-DEALER-REP-NAMES.
091300991014           IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
091310991014              MOVE SPACES          TO  SH03-DEALER-REP-NAMES
091320991014              MOVE WS-REP-NAME-1   TO  SH03-DEALER-REP-NAMES
091330991014           END-IF.
091340991014
091350000000           SET SPL                  UP BY  3.
091360000000
091370000000       LR1-0030-0120.
091380990927
091390000000           MOVE STMT-HDG-03            TO  WS-STMTPGE-LNE (SPL).
091400990927
09141099092799927      IF WS-STMTVRSN              IS  EQUAL TO  6
09142099092799927         SET WS-SPL-SH03          TO  SPL
09143099092799927      END-IF
091440990927
091450000000      *****
091460000000      *****    Format statement heading line 4.
091470000000      *****
091480000000
091490000000           MOVE SPACES                 TO  WS-SH04-INVESTOR-ADDRESS1.
091500990926
091510000000           IF  II-INVESTOR-ADDRESS1
091520000000                              IS EQUAL TO  SPACES
091530000000           AND II-INVESTOR-ADDRESS2
091540000000                              IS EQUAL TO  SPACES
091550000000           AND II-INVESTOR-ADDRESS3
091560000000                              IS EQUAL TO  SPACES
091570000000           AND II-INVESTOR-ADDRESS4
091580000000                              IS EQUAL TO  SPACES
091590000000           AND II-INVESTOR-ADDRESS5
091600000000                              IS EQUAL TO  SPACES
091610000000               MOVE II-POSTAL-CODE     TO  SH04-POSTAL-CODE
091620000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
091630990922           ELSE
091640000000               MOVE II-INVESTOR-ADDRESS1
091650990922                                       TO  WS-SH04-INVESTOR-ADDRESS1
091660990922           END-IF
091670990922
091680980417980417     IF  WS-LDA-BRANCH-ADDRESS
091690980417980417                        IS EQUAL TO  "N"
091700000000               MOVE SPACES             TO  WS-BRANCH-ADDR1.
091710000000
091720000000           MOVE SPACES                 TO  STMT-HDG-04.
091730000000
091740000000           IF  WS-STMTVRSN    IS EQUAL TO  1
091750000000               MOVE WS-SH04-INVESTOR-ADDRESS1
091760000000                                       TO  SH04V1-INVESTOR-ADDRESS1
091770000000               MOVE WS-BRANCH-ADDR1    TO  SH04V1-BRANCH-ADDR1
091780990922
091790991014               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
091800990611                  MOVE SPACES TO SH04V1-BRANCH-ADDR1
091810990611               END-IF
091820990922
091830990922               GO TO LR1-0030-0130
091840990922           END-IF
091850990922
091860000000           IF  WS-STMTVRSN    IS EQUAL TO  2
091870000000               MOVE WS-SH04-INVESTOR-ADDRESS1
091880000000                                       TO  SH04V2-INVESTOR-ADDRESS1
091890000000               MOVE WS-BRANCH-ADDR1    TO  SH04V2-BRANCH-ADDR1
091900990922               GO TO LR1-0030-0130
091910990922           END-IF
091920000000
091930000000           IF  WS-STMTVRSN    IS EQUAL TO  3
091940000000               MOVE WS-SH04-INVESTOR-ADDRESS1
091950000000                                       TO  SH04V3-INVESTOR-ADDRESS1
091960000000               MOVE WS-BRANCH-ADDR1    TO  SH04V3-BRANCH-ADDR1
091970990922               GO TO LR1-0030-0130
091980990922           END-IF
091990000000
092000000000           IF  WS-STMTVRSN    IS EQUAL TO  4
092010000000               MOVE WS-SH04-INVESTOR-ADDRESS1
092020000000                                       TO  SH04V4-INVESTOR-ADDRESS1
092030000000               MOVE WS-BRANCH-ADDR1    TO  SH04V4-BRANCH-ADDR1
092040990922               GO TO LR1-0030-0130
092050990922           END-IF
092060000000
092070000000           IF  WS-STMTVRSN    IS EQUAL TO  5
092080000000               MOVE WS-SH04-INVESTOR-ADDRESS1
092090000000                                       TO  SH04V5-INVESTOR-ADDRESS1
092100000000               MOVE WS-BRANCH-ADDR1    TO  SH04V5-BRANCH-ADDR1
092110990922               GO TO LR1-0030-0130
092120990922           END-IF
092130990923
092140991019           IF WS-STMTVRSN    IS EQUAL TO  7
092150991019              MOVE WS-SH04-INVESTOR-ADDRESS1
092160991019                                          TO  SH04V7-INVESTOR-ADDRESS1
092170991019              MOVE WS-BRANCH-ADDR1        TO  SH04V7-BRANCH-ADDR1
092180991022
092190991022               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
092200991022                  MOVE SPACES TO SH04V7-BRANCH-ADDR1
092210991022               END-IF
092220991022
092230991019              GO TO LR1-0030-0130
092240991019           END-IF
092250991019
092260991213           IF  WS-STMTVRSN    IS EQUAL TO  8
092270991213               MOVE WS-SH04-INVESTOR-ADDRESS1
092280991213                                       TO  SH04V8-INVESTOR-ADDRESS1
092290991213               MOVE WS-BRANCH-ADDR1    TO  SH04V8-BRANCH-ADDR1
092300991213
092310991213               IF WS-LDA-DLR-REP-PRINT  IS EQUAL TO "R"
092320991213                  MOVE SPACES TO SH04V8-BRANCH-ADDR1
092330991213               END-IF
092340991213
092350991213               GO TO LR1-0030-0130
092360991213           END-IF
092370010314
092380010314RF9554     IF  WS-STMTVRSN    IS EQUAL TO  6
092390010314
092400010314               MOVE SPACES         TO  WS-SH05-INVESTOR-ADDRESS2
092410990923
092420010314               IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
092430010314               AND II-INVESTOR-ADDRESS2    IS EQUAL TO  SPACES
092440010314               AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
092450010314               AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
092460010314               AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
092470010314                   MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
092480010314                   MOVE "Y"                TO  WS-PSTCDEPRT-SW
092490010314               ELSE
092500010314                   MOVE II-INVESTOR-ADDRESS2
092510010314                                 TO  WS-SH05-INVESTOR-ADDRESS2
092520010314               END-IF
092530000000
092540010314RF9554*    IF  WS-STMTVRSN    IS EQUAL TO  6
092550990922               MOVE WS-SH05-INVESTOR-ADDRESS2
092560990923                                       TO  SH04V6-INVESTOR-ADDRESS2
092570990922               MOVE SPACES             TO  SH04V6-BRANCH-ADDR1
092580990922               GO TO LR1-0030-0130
092590010314RF9554     END-IF
092600990922
092610000000           MOVE WS-SH04-INVESTOR-ADDRESS1
092620000000                                       TO  SH04-INVESTOR-ADDRESS1.
092630000000           MOVE WS-BRANCH-ADDR1        TO  SH04-BRANCH-ADDR1.
092640000000
092650000000       LR1-0030-0130.
092660990923
092670990923           SET SPL                     UP BY  1.
092680000000           MOVE STMT-HDG-04            TO  WS-STMTPGE-LNE (SPL).
092690000000
092700000000      *****
092710000000      *****    Format statement heading line 5.
092720000000      *****
092730000000
092740000000           MOVE SPACES                 TO  WS-SH05-INVESTOR-ADDRESS2.
092750990926
092760000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
092770000000           AND II-INVESTOR-ADDRESS2
092780000000                              IS EQUAL TO  SPACES
092790000000           AND II-INVESTOR-ADDRESS3
092800000000                              IS EQUAL TO  SPACES
092810000000           AND II-INVESTOR-ADDRESS4
092820000000                              IS EQUAL TO  SPACES
092830000000           AND II-INVESTOR-ADDRESS5
092840000000                              IS EQUAL TO  SPACES
092850000000               MOVE II-POSTAL-CODE     TO  SH05-POSTAL-CODE
092860000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
092870990922           ELSE
092880000000               MOVE II-INVESTOR-ADDRESS2
092890990922                                       TO  WS-SH05-INVESTOR-ADDRESS2
092900990922           END-IF
092910990922
092920990923980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO  "N"
092930990923               MOVE SPACES             TO  WS-BRANCH-ADDR2
092940990923           END-IF
092950990923
092960000000           MOVE SPACES                 TO  STMT-HDG-05.
092970000000
092980000000           IF  WS-STMTVRSN    IS EQUAL TO  1
092990000000               MOVE WS-SH05-INVESTOR-ADDRESS2
093000000000                                       TO  SH05V1-INVESTOR-ADDRESS2
093010000000               MOVE WS-BRANCH-ADDR2    TO  SH05V1-BRANCH-ADDR2
093020990611
093030991014               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
093040990611                  MOVE SPACES TO SH05V1-BRANCH-ADDR2
093050990611               END-IF
093060990611
093070000000               GO TO LR1-0030-0140.
093080000000
093090000000           IF  WS-STMTVRSN    IS EQUAL TO  2
093100000000               MOVE WS-SH05-INVESTOR-ADDRESS2
093110000000                                       TO  SH05V2-INVESTOR-ADDRESS2
093120000000               MOVE WS-BRANCH-ADDR2    TO  SH05V2-BRANCH-ADDR2
093130000000               GO TO LR1-0030-0140.
093140000000
093150000000           IF  WS-STMTVRSN    IS EQUAL TO  3
093160000000               MOVE WS-SH05-INVESTOR-ADDRESS2
093170000000                                       TO  SH05V3-INVESTOR-ADDRESS2
093180000000               MOVE WS-BRANCH-ADDR2    TO  SH05V3-BRANCH-ADDR2
093190000000               GO TO LR1-0030-0140.
093200000000
093210000000           IF  WS-STMTVRSN    IS EQUAL TO  4
093220000000               MOVE WS-SH05-INVESTOR-ADDRESS2
093230000000                                       TO  SH05V4-INVESTOR-ADDRESS2
093240000000               MOVE WS-BRANCH-ADDR2    TO  SH05V4-BRANCH-ADDR2
093250000000               GO TO LR1-0030-0140.
093260000000
093270000000           IF  WS-STMTVRSN    IS EQUAL TO  5
093280000000               MOVE WS-SH05-INVESTOR-ADDRESS2
093290000000                                       TO  SH05V5-INVESTOR-ADDRESS2
093300000000               MOVE WS-BRANCH-ADDR2    TO  SH05V5-BRANCH-ADDR2
093310990922               GO TO LR1-0030-0140
093320990922           END-IF
093330990923
093340990926           IF WS-STMTVRSN    IS EQUAL TO  6
093350990926              MOVE SPACES                 TO  WS-SH06-INVESTOR-ADDRESS3
093360990923
093370990926              IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
093380990926              AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
093390990926              AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
093400990926              AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
093410990926                  MOVE II-POSTAL-CODE     TO  SH06-POSTAL-CODE
093420990926                  MOVE "Y"                TO  WS-PSTCDEPRT-SW
093430990926              ELSE
093440990926                  MOVE II-INVESTOR-ADDRESS3
093450990923                                       TO  WS-SH06-INVESTOR-ADDRESS3
093460990926              END-IF
093470990922
093480990926              MOVE WS-SH06-INVESTOR-ADDRESS3
093490990926                                      TO  SH05V6-INVESTOR-ADDRESS3
093500990926              MOVE SPACES             TO  SH05V6-BRANCH-ADDR2
093510990926              GO TO LR1-0030-0140
093520990922           END-IF
093530990922
093540991019           IF  WS-STMTVRSN    IS EQUAL TO  7
093550991019               MOVE WS-SH05-INVESTOR-ADDRESS2
093560991019                                       TO  SH05V7-INVESTOR-ADDRESS2
093570991019               MOVE WS-BRANCH-ADDR2    TO  SH05V7-BRANCH-ADDR2
093580991022
093590991022               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
093600991022                  MOVE SPACES TO SH05V7-BRANCH-ADDR2
093610991022               END-IF
093620991022
093630991019               GO TO LR1-0030-0140
093640991019           END-IF
093650991019
093660991213           IF  WS-STMTVRSN    IS EQUAL TO  8
093670991213               MOVE WS-SH05-INVESTOR-ADDRESS2
093680991213                                       TO  SH05V8-INVESTOR-ADDRESS2
093690991213               MOVE WS-BRANCH-ADDR2    TO  SH05V8-BRANCH-ADDR2
093700991213
093710991213               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
093720991213                  MOVE SPACES TO SH05V8-BRANCH-ADDR2
093730991213               END-IF
093740991213
093750991213               GO TO LR1-0030-0140
093760991213           END-IF
093770991213
093780000000           MOVE WS-SH05-INVESTOR-ADDRESS2
093790000000                                       TO  SH05-INVESTOR-ADDRESS2.
093800000000           MOVE WS-BRANCH-ADDR2        TO  SH05-BRANCH-ADDR2.
093810000000
093820000000       LR1-0030-0140.
093830990926
093840990926           SET SPL                     UP BY  1.
093850000000           MOVE STMT-HDG-05            TO  WS-STMTPGE-LNE (SPL).
093860000000
093870000000      *****
093880000000      *****    Format statement heading line 6.
093890000000      *****
093900000000
093910000000           MOVE SPACES                 TO  WS-SH06-INVESTOR-ADDRESS3.
093920990926
093930000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
093940990923           AND II-INVESTOR-ADDRESS3    IS EQUAL TO  SPACES
093950990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
093960990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
093970000000               MOVE II-POSTAL-CODE     TO  SH06-POSTAL-CODE
093980000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
093990990923           ELSE
094000000000               MOVE II-INVESTOR-ADDRESS3
094010990923                                       TO  WS-SH06-INVESTOR-ADDRESS3
094020990923           END-IF
094030990923
094040980417980417     IF  WS-LDA-BRANCH-ADDRESS
094050980417980417                        IS EQUAL TO  "N"
094060000000               MOVE SPACES             TO  WS-BRANCH-ADDR3.
094070000000
094080000000           MOVE SPACES                 TO  STMT-HDG-06.
094090000000
094100000000           IF  WS-STMTVRSN    IS EQUAL TO  1
094110000000               MOVE WS-SH06-INVESTOR-ADDRESS3
094120000000                                       TO  SH06V1-INVESTOR-ADDRESS3
094130000000               MOVE WS-BRANCH-ADDR3    TO  SH06V1-BRANCH-ADDR3
094140990611
094150991014               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
094160990611                  MOVE SPACES TO SH06V1-BRANCH-ADDR3
094170990611               END-IF
094180990611
094190000000               GO TO LR1-0030-0150.
094200000000
094210000000           IF  WS-STMTVRSN    IS EQUAL TO  2
094220000000               MOVE WS-SH06-INVESTOR-ADDRESS3
094230000000                                       TO  SH06V2-INVESTOR-ADDRESS3
094240000000               MOVE WS-BRANCH-ADDR3    TO  SH06V2-BRANCH-ADDR3
094250000000               GO TO LR1-0030-0150.
094260000000
094270000000           IF  WS-STMTVRSN    IS EQUAL TO  3
094280000000               MOVE WS-SH06-INVESTOR-ADDRESS3
094290000000                                       TO  SH06V3-INVESTOR-ADDRESS3
094300000000               MOVE WS-BRANCH-ADDR3    TO  SH06V3-BRANCH-ADDR3
094310000000               GO TO LR1-0030-0150.
094320000000
094330000000           IF  WS-STMTVRSN    IS EQUAL TO  4
094340000000               MOVE WS-SH06-INVESTOR-ADDRESS3
094350000000                                       TO  SH06V4-INVESTOR-ADDRESS3
094360000000               MOVE WS-BRANCH-ADDR3    TO  SH06V4-BRANCH-ADDR3
094370000000               GO TO LR1-0030-0150.
094380000000
094390000000           IF  WS-STMTVRSN    IS EQUAL TO  5
094400000000               MOVE WS-SH06-INVESTOR-ADDRESS3
094410000000                                       TO  SH06V5-INVESTOR-ADDRESS3
094420000000               MOVE WS-BRANCH-ADDR3    TO  SH06V5-BRANCH-ADDR3
094430990922               GO TO LR1-0030-0150
094440990922           END-IF
094450990923
094460990926           IF WS-STMTVRSN    IS EQUAL TO  6
094470990926              MOVE SPACES                 TO  WS-SH07-INVESTOR-ADDRESS4
094480990923
094490990926              IF WS-PSTCDEPRT-SW   EQUAL TO  "N"
094500990926                 AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
094510990926                 AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
094520990926                 MOVE II-POSTAL-CODE     TO  SH07-POSTAL-CODE
094530990926                 MOVE "Y"                TO  WS-PSTCDEPRT-SW
094540990926              ELSE
094550990926                 MOVE II-INVESTOR-ADDRESS4
094560990923                                       TO  WS-SH07-INVESTOR-ADDRESS4
094570990926              END-IF
094580990922
094590990922               MOVE WS-SH07-INVESTOR-ADDRESS4
094600990923                                       TO  SH06V6-INVESTOR-ADDRESS4
094610990922               MOVE SPACES             TO  SH06V6-BRANCH-ADDR3
094620990922               GO TO LR1-0030-0150
094630990922           END-IF
094640990922
094650991019           IF  WS-STMTVRSN    IS EQUAL TO  7
094660991019               MOVE WS-SH06-INVESTOR-ADDRESS3
094670991019                                       TO  SH06V7-INVESTOR-ADDRESS3
094680991019               MOVE WS-BRANCH-ADDR3    TO  SH06V7-BRANCH-ADDR3
094690991022
094700991022               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
094710991022                  MOVE SPACES TO SH06V7-BRANCH-ADDR3
094720991022               END-IF
094730991022
094740991019               GO TO LR1-0030-0150
094750991019           END-IF
094760991019
094770991213           IF  WS-STMTVRSN    IS EQUAL TO  8
094780991213               MOVE WS-SH06-INVESTOR-ADDRESS3
094790991213                                       TO  SH06V8-INVESTOR-ADDRESS3
094800991213               MOVE WS-BRANCH-ADDR3    TO  SH06V8-BRANCH-ADDR3
094810991213
094820991213               IF WS-LDA-DLR-REP-PRINT      IS EQUAL TO "R"
094830991213                  MOVE SPACES TO SH06V8-BRANCH-ADDR3
094840991213               END-IF
094850991213
094860991213               GO TO LR1-0030-0150
094870991213           END-IF
094880991213
094890000000           MOVE WS-SH06-INVESTOR-ADDRESS3
094900000000                                       TO  SH06-INVESTOR-ADDRESS3.
094910000000           MOVE WS-BRANCH-ADDR3        TO  SH06-BRANCH-ADDR3.
094920000000
094930000000       LR1-0030-0150.
094940990923
094950990923           SET SPL                     UP BY  1.
094960000000           MOVE STMT-HDG-06            TO  WS-STMTPGE-LNE (SPL).
094970000000
094980000000      *****
094990000000      *****    Format statement heading line 7.
095000000000      *****
095010000000
095020000000           MOVE SPACES                 TO  WS-SH07-INVESTOR-ADDRESS4.
095030000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
095040990923           AND II-INVESTOR-ADDRESS4    IS EQUAL TO  SPACES
095050990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
095060000000               MOVE II-POSTAL-CODE     TO  SH07-POSTAL-CODE
095070000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
095080990923           ELSE
095090000000               MOVE II-INVESTOR-ADDRESS4
095100990923                                       TO  WS-SH07-INVESTOR-ADDRESS4
095110990923           END-IF
095120990923
095130990923980417     IF  WS-LDA-BRANCH-ADDRESS   IS EQUAL TO "N"
095140990923               MOVE SPACES             TO  WS-BRANCH-ADDR4
095150990923           END-IF
095160990923
095170000000           MOVE SPACES                 TO  STMT-HDG-07.
095180000000
095190000000           IF  WS-STMTVRSN    IS EQUAL TO  1
095200000000               MOVE WS-SH07-INVESTOR-ADDRESS4
095210000000                                       TO  SH07V1-INVESTOR-ADDRESS4
095220000000               MOVE WS-BRANCH-ADDR4    TO  SH07V1-BRANCH-ADDR4
095230990611
095240991014               IF WS-LDA-DLR-REP-PRINT        IS EQUAL TO "R"
095250990611                  MOVE SPACES TO SH07V1-BRANCH-ADDR4
095260990611               END-IF
095270990611
095280000000               GO TO LR1-0030-0160.
095290000000
095300000000           IF  WS-STMTVRSN    IS EQUAL TO  2
095310000000               MOVE WS-SH07-INVESTOR-ADDRESS4
095320000000                                       TO  SH07V2-INVESTOR-ADDRESS4
095330000000               MOVE WS-BRANCH-ADDR4    TO  SH07V2-BRANCH-ADDR4
095340000000               GO TO LR1-0030-0160.
095350000000
095360000000           IF  WS-STMTVRSN    IS EQUAL TO  3
095370000000               MOVE WS-SH07-INVESTOR-ADDRESS4
095380000000                                       TO  SH07V3-INVESTOR-ADDRESS4
095390000000               MOVE WS-BRANCH-ADDR4    TO  SH07V3-BRANCH-ADDR4
095400000000               GO TO LR1-0030-0160.
095410000000
095420000000           IF  WS-STMTVRSN    IS EQUAL TO  4
095430000000               MOVE WS-SH07-INVESTOR-ADDRESS4
095440000000                                       TO  SH07V4-INVESTOR-ADDRESS4
095450000000               MOVE WS-BRANCH-ADDR4    TO  SH07V4-BRANCH-ADDR4
095460000000               GO TO LR1-0030-0160.
095470000000
095480000000           IF  WS-STMTVRSN    IS EQUAL TO  5
095490000000               MOVE WS-SH07-INVESTOR-ADDRESS4
095500000000                                       TO  SH07V5-INVESTOR-ADDRESS4
095510000000               MOVE WS-BRANCH-ADDR4    TO  SH07V5-BRANCH-ADDR4
095520990922               GO TO LR1-0030-0160
095530990922           END-IF
095540990922
095550990926           IF WS-STMTVRSN    IS EQUAL TO  6
095560990926              MOVE SPACES                 TO  WS-SH08-INVESTOR-ADDRESS5
095570990923
095580990926              IF WS-PSTCDEPRT-SW   EQUAL TO  "N"
095590990926                 AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
095600990926                 MOVE II-POSTAL-CODE     TO  SH08-POSTAL-CODE
095610990926                 MOVE "Y"                TO  WS-PSTCDEPRT-SW
095620990926              ELSE
095630990926                 MOVE II-INVESTOR-ADDRESS5
095640990923                                       TO  WS-SH08-INVESTOR-ADDRESS5
095650990926              END-IF
095660990926              MOVE WS-SH08-INVESTOR-ADDRESS5
095670990926                                      TO  SH07V6-INVESTOR-ADDRESS5
095680990926              MOVE SPACES             TO  SH07V6-BRANCH-ADDR4
095690990926              GO TO LR1-0030-0160
095700990923           END-IF
095710990923
095720991019           IF  WS-STMTVRSN    IS EQUAL TO  7
095730991019               MOVE WS-SH07-INVESTOR-ADDRESS4
095740991019                                       TO  SH07V7-INVESTOR-ADDRESS4
095750991019               MOVE WS-BRANCH-ADDR4    TO  SH07V7-BRANCH-ADDR4
095760991022
095770991022               IF WS-LDA-DLR-REP-PRINT        IS EQUAL TO "R"
095780991022                  MOVE SPACES TO SH07V7-BRANCH-ADDR4
095790991022               END-IF
095800991022
095810991019               GO TO LR1-0030-0160
095820991019           END-IF
095830991213
095840991213           IF  WS-STMTVRSN    IS EQUAL TO  8
095850991213               MOVE WS-SH07-INVESTOR-ADDRESS4
095860991213                                       TO  SH07V8-INVESTOR-ADDRESS4
095870991213               MOVE WS-BRANCH-ADDR4    TO  SH07V8-BRANCH-ADDR4
095880991213
095890991213               IF WS-LDA-DLR-REP-PRINT        IS EQUAL TO "R"
095900991213                  MOVE SPACES TO SH07V8-BRANCH-ADDR4
095910991213               END-IF
095920991213
095930991213               GO TO LR1-0030-0160
095940991213           END-IF
095950991019
095960000000           MOVE WS-SH07-INVESTOR-ADDRESS4
095970000000                                       TO  SH07-INVESTOR-ADDRESS4.
095980000000           MOVE WS-BRANCH-ADDR4        TO  SH07-BRANCH-ADDR4.
095990000000
096000000000       LR1-0030-0160.
096010990922
096020990926           SET SPL                     UP BY  1.
096030000000           MOVE STMT-HDG-07            TO  WS-STMTPGE-LNE (SPL).
096040000000
096050000000      *****
096060000000      *****    Format statement heading line 8.
096070000000      *****
096080000000
096090000000           MOVE SPACES                 TO  WS-SH08-INVESTOR-ADDRESS5.
096100990926
096110000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
096120990923           AND II-INVESTOR-ADDRESS5    IS EQUAL TO  SPACES
096130000000               MOVE II-POSTAL-CODE     TO  SH08-POSTAL-CODE
096140000000               MOVE "Y"                TO  WS-PSTCDEPRT-SW
096150990923           ELSE
096160000000               MOVE II-INVESTOR-ADDRESS5
096170990923                                       TO  WS-SH08-INVESTOR-ADDRESS5
096180990923           END-IF
096190990923
096200000000           MOVE SPACES                 TO  STMT-HDG-08.
096210000000
096220000000           IF  WS-STMTVRSN    IS EQUAL TO  1
096230000000               MOVE WS-SH08-INVESTOR-ADDRESS5
096240000000                                       TO  SH08V1-INVESTOR-ADDRESS5
096250990923               GO TO LR1-0030-0170
096260990923           END-IF
096270000000
096280000000           IF  WS-STMTVRSN    IS EQUAL TO  2
096290000000               MOVE WS-SH08-INVESTOR-ADDRESS5
096300000000                                       TO  SH08V2-INVESTOR-ADDRESS5
096310990923               GO TO LR1-0030-0170
096320990923           END-IF
096330000000
096340000000           IF  WS-STMTVRSN    IS EQUAL TO  3
096350000000               MOVE WS-SH08-INVESTOR-ADDRESS5
096360000000                                       TO  SH08V3-INVESTOR-ADDRESS5
096370990923               GO TO LR1-0030-0170
096380990923           END-IF
096390000000
096400000000           IF  WS-STMTVRSN    IS EQUAL TO  4
096410000000               MOVE WS-SH08-INVESTOR-ADDRESS5
096420000000                                       TO  SH08V4-INVESTOR-ADDRESS5
096430990923               GO TO LR1-0030-0170
096440990923           END-IF
096450000000
096460000000           IF  WS-STMTVRSN    IS EQUAL TO  5
096470000000               MOVE WS-SH08-INVESTOR-ADDRESS5
096480000000                                       TO  SH08V5-INVESTOR-ADDRESS5
096490990922               GO TO LR1-0030-0170
096500990922           END-IF
096510990923
096520990926           IF WS-STMTVRSN    IS EQUAL  TO  6
096530990926              MOVE SPACES                TO  WS-SH09-POSTAL-CODE
096540990926              IF WS-PSTCDEPRT-SW   EQUAL TO  "N"
096550990926                 MOVE II-POSTAL-CODE     TO  WS-SH09-POSTAL-CODE
096560990926              END-IF
096570990926              MOVE WS-SH09-POSTAL-CODE   TO  SH08V6-POSTAL-CODE
096580990926              GO TO LR1-0030-0170
096590990922           END-IF
096600990922
096610991019           IF  WS-STMTVRSN    IS EQUAL TO  7
096620991019               MOVE WS-SH08-INVESTOR-ADDRESS5
096630991019                                       TO  SH08V7-INVESTOR-ADDRESS5
096640991019               GO TO LR1-0030-0170
096650991019           END-IF
096660991213
096670991213           IF  WS-STMTVRSN    IS EQUAL TO  8
096680991213               MOVE WS-SH08-INVESTOR-ADDRESS5
096690991213                                       TO  SH08V8-INVESTOR-ADDRESS5
096700991213               GO TO LR1-0030-0170
096710991213           END-IF
096720991213
096730000000           MOVE WS-SH08-INVESTOR-ADDRESS5
096740000000                                       TO  SH08-INVESTOR-ADDRESS5.
096750000000
096760000000       LR1-0030-0170.
096770990923
096780990923           SET SPL                     UP BY  1.
096790000000           MOVE STMT-HDG-08            TO  WS-STMTPGE-LNE (SPL).
096800000000
096810000000      *****
096820000000      *****    Format statement heading line 9.
096830000000      *****
096840000000
096850990926           MOVE SPACES                 TO  WS-SH09-POSTAL-CODE.
096860990923
096870000000           IF  WS-PSTCDEPRT-SW   EQUAL TO  "N"
096880990923               MOVE II-POSTAL-CODE     TO  WS-SH09-POSTAL-CODE
096890990923           END-IF
096900990923
096910000000           MOVE SPACES                 TO  STMT-HDG-09.
096920990926
096930990923           IF  WS-STMTVRSN    IS EQUAL   TO  1
096940990923               MOVE WS-SH09-POSTAL-CODE  TO  SH09V1-POSTAL-CODE
096950990923               GO TO LR1-0030-0180
096960990923           END-IF
096970000000
096980990923           IF  WS-STMTVRSN    IS EQUAL   TO  2
096990990923               MOVE WS-SH09-POSTAL-CODE  TO  SH09V2-POSTAL-CODE
097000990923               GO TO LR1-0030-0180
097010990923           END-IF
097020000000
097030990923           IF  WS-STMTVRSN    IS EQUAL   TO  3
097040990923               MOVE WS-SH09-POSTAL-CODE  TO  SH09V3-POSTAL-CODE
097050990923               GO TO LR1-0030-0180
097060990923           END-IF
097070000000
097080990923           IF  WS-STMTVRSN    IS EQUAL   TO  4
097090990923               MOVE WS-SH09-POSTAL-CODE  TO  SH09V4-POSTAL-CODE
097100990923               GO TO LR1-0030-0180
097110990923           END-IF
097120000000
097130990923           IF  WS-STMTVRSN    IS EQUAL   TO  5
097140990923               MOVE WS-SH09-POSTAL-CODE  TO  SH09V5-POSTAL-CODE
097150990922               GO TO LR1-0030-0180
097160990922           END-IF
097170990922
097180990923           IF  WS-STMTVRSN    IS EQUAL   TO  6
097190990923               MOVE WS-SH09-LIT1 (SL)       TO  SH09-LIT1
097200990922               GO TO LR1-0030-0180
097210990922           END-IF
097220990922
097230991019           IF  WS-STMTVRSN    IS EQUAL   TO  7
097240991019               MOVE WS-SH09-POSTAL-CODE  TO  SH09V7-POSTAL-CODE
097250991019               GO TO LR1-0030-0180
097260991019           END-IF
097270991019
097280991213           IF  WS-STMTVRSN    IS EQUAL   TO  8
097290991213               MOVE WS-SH09-POSTAL-CODE  TO  SH09V8-POSTAL-CODE
097300991213               GO TO LR1-0030-0180
097310991213           END-IF
097320991213
097330990926           MOVE WS-SH09-POSTAL-CODE      TO  SH09-POSTAL-CODE.
097340000000
097350000000       LR1-0030-0180.
097360990923
097370990927           IF WS-STMTVRSN              IS EQUAL TO  6
097380990927              SET SPL                  UP BY  1
097390990927           END-IF
097400990927
097410990925           SET SPL                     UP BY  1
097420000000           MOVE STMT-HDG-09            TO  WS-STMTPGE-LNE (SPL).
097430990925
097440990925           IF WS-STMTVRSN              IS EQUAL TO  6
097450990925              SET SPL                  UP BY  1
097460990925           END-IF
097470000000
097480000000      *****
097490000000      *****    Format statement heading line 10.
097500000000      *****
097510000000
097520000000           MOVE SPACES                 TO  WS-SH10-LIT1X.
097530990922
097540000000           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
097550990922               MOVE WS-SH10-LIT1 (SL)  TO  WS-SH10-LIT1X
097560990922           END-IF
097570990922
097580000000           MOVE SPACES                 TO  WS-SH10-PHONE.
097590990922
097600000000           IF  WS-PHONE   IS NOT EQUAL TO  ZEROES
097610000000               MOVE "("                TO  SH10-PHONE-LITLB
097620000000               MOVE WS-SH-PHONE-01-03  TO  SH10-PHONE-01-03
097630000000               MOVE ")"                TO  SH10-PHONE-LITRB
097640000000               MOVE WS-SH-PHONE-04-06  TO  SH10-PHONE-04-06
097650000000               MOVE "-"                TO  SH10-PHONE-LITDSH
097660990922               MOVE WS-SH-PHONE-07-10  TO  SH10-PHONE-07-10
097670990922           END-IF
097680990922
097690000000           MOVE SPACES                 TO  STMT-HDG-10.
097700000000
097710000000           IF  WS-STMTVRSN    IS EQUAL TO  1
097720991014           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
097730990225                  MOVE WS-SH10-LIT1X      TO  SH10V1-LIT1
097740990225                  MOVE WS-SH10-PHONE      TO  SH10V1-PHONE
097750990922               GO TO LR1-0030-0190
097760990922           END-IF
097770000000
097780990629           IF  WS-STMTVRSN    IS EQUAL TO  1
097790991014           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
097800990629                  MOVE WS-SH10-LIT1X      TO  SH10V1-LIT1
097810990629                  MOVE SPACES             TO  SH10V1-PHONE
097820990922               GO TO LR1-0030-0190
097830990922           END-IF
097840990629
097850000000           IF  WS-STMTVRSN    IS EQUAL TO  2
097860000000               MOVE WS-SH10-LIT1X      TO  SH10V2-LIT1
097870000000               MOVE WS-SH10-PHONE      TO  SH10V2-PHONE
097880990922               GO TO LR1-0030-0190
097890990922           END-IF
097900000000
097910000000           IF  WS-STMTVRSN    IS EQUAL TO  3
097920000000               MOVE WS-SH10-LIT1X      TO  SH10V3-LIT1
097930000000               MOVE WS-SH10-PHONE      TO  SH10V3-PHONE
097940990922               GO TO LR1-0030-0190
097950990922           END-IF
097960000000
097970000000           IF  WS-STMTVRSN    IS EQUAL TO  4
097980000000               MOVE WS-SH10-LIT1X      TO  SH10V4-LIT1
097990000000               MOVE WS-SH10-PHONE      TO  SH10V4-PHONE
098000990922               GO TO LR1-0030-0190
098010990922           END-IF
098020000000
098030000000           IF  WS-STMTVRSN    IS EQUAL TO  5
098040000000               MOVE WS-SH10-LIT1X      TO  SH10V5-LIT1
098050000000               MOVE WS-SH10-PHONE      TO  SH10V5-PHONE
098060990922               GO TO LR1-0030-0190
098070990922           END-IF
098080990922
098090990922           IF  WS-STMTVRSN    IS EQUAL TO  6
098100990923               MOVE WS-SH10-LIT2 (SL)  TO  SH10V6-LIT2
098110020921R13756       IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
098120020921  ¦            MOVE WS-MSTR-ACCOUNT-NO  TO  SH10V6-ACCOUNT
098130020613R13756       ELSE
098140990923               MOVE AD-ACCOUNT         TO  SH10V6-ACCOUNT
098150020613R13756       END-IF
098160990922               GO TO LR1-0030-0190
098170990922           END-IF
098180990922
098190991019           IF  WS-STMTVRSN    IS EQUAL TO  7
098200991022           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
098210991019               MOVE WS-SH10-LIT1X      TO  SH10V7-LIT1
098220991019               MOVE WS-SH10-PHONE      TO  SH10V7-PHONE
098230991019               GO TO LR1-0030-0190
098240991019           END-IF
098250991022
098260991022           IF  WS-STMTVRSN    IS EQUAL TO  7
098270991022           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
098280991022                  MOVE WS-SH10-LIT1X      TO  SH10V7-LIT1
098290991022                  MOVE SPACES             TO  SH10V7-PHONE
098300991022               GO TO LR1-0030-0190
098310991022           END-IF
098320991022
098330991213           IF  WS-STMTVRSN    IS EQUAL TO  8
098340991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
098350991213                  MOVE WS-SH10-LIT1X      TO  SH10V8-LIT1
098360991213                  MOVE WS-SH10-PHONE      TO  SH10V8-PHONE
098370991213               GO TO LR1-0030-0190
098380991213           END-IF
098390991213
098400991213           IF  WS-STMTVRSN    IS EQUAL TO  8
098410991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
098420991213                  MOVE WS-SH10-LIT1X      TO  SH10V8-LIT1
098430991213                  MOVE SPACES             TO  SH10V8-PHONE
098440991213               GO TO LR1-0030-0190
098450991213           END-IF
098460991019
098470000000           MOVE WS-SH10-LIT1X          TO  SH10-LIT1.
098480000000           MOVE WS-SH10-PHONE          TO  SH10-PHONE.
098490000000
098500000000       LR1-0030-0190.
098510990925
098520990922           SET SPL                     UP BY  1.
098530000000           MOVE STMT-HDG-10            TO  WS-STMTPGE-LNE (SPL).
098540000000
098550000000      *****
098560000000      *****    Format statement heading line 11.
098570000000      *****
098580000000
098590000000           MOVE SPACES                 TO  WS-SH11-NOMINEE.
098600990922
098610991013           IF  WS-NOMINEE              IS NOT EQUAL TO  SPACES
098620990922               MOVE WS-NOMINEE         TO  WS-SH11-NOMINEE
098630990922           END-IF
098640990922
098650000000           MOVE SPACES                 TO  WS-SH11-FAX.
098660990927
098670000000           IF  WS-FAX     IS NOT EQUAL TO  ZEROES
098680000000               MOVE "("                TO  SH11-FAX-LITLB
098690000000               MOVE WS-SH-FAX-01-03    TO  SH11-FAX-01-03
098700000000               MOVE ")"                TO  SH11-FAX-LITRB
098710000000               MOVE WS-SH-FAX-04-06    TO  SH11-FAX-04-06
098720000000               MOVE "-"                TO  SH11-FAX-LITDSH
098730990927               MOVE WS-SH-FAX-07-10    TO  SH11-FAX-07-10
098740990927           END-IF
098750990927
098760000000           MOVE SPACES                 TO  STMT-HDG-11.
098770000000
098780000000           IF  WS-STMTVRSN    IS EQUAL TO  1
098790991014           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
098800000000               MOVE WS-SH11-NOMINEE    TO  SH11V1-NOMINEE
098810000000               MOVE WS-SH11-FAX        TO  SH11V1-FAX
098820990922               GO TO LR1-0030-0200
098830990922           END-IF
098840990922
098850990629           IF  WS-STMTVRSN    IS EQUAL TO  1
098860991014           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
098870990629               MOVE WS-SH11-NOMINEE    TO  SH11V1-NOMINEE
098880990629               MOVE WS-SH02-SIN-NO     TO  SH11V1-FAX
098890990922               GO TO LR1-0030-0200
098900990922           END-IF
098910990629
098920000000           IF  WS-STMTVRSN    IS EQUAL TO  2
098930000000               MOVE WS-SH11-NOMINEE    TO  SH11V2-NOMINEE
098940000000               MOVE WS-SH11-FAX        TO  SH11V2-FAX
098950990922               GO TO LR1-0030-0200
098960990922           END-IF
098970000000
098980000000           IF  WS-STMTVRSN    IS EQUAL TO  3
098990000000               MOVE WS-SH11-NOMINEE    TO  SH11V3-NOMINEE
099000000000               MOVE WS-SH11-FAX        TO  SH11V3-FAX
099010990922               GO TO LR1-0030-0200
099020990922           END-IF
099030000000
099040000000           IF  WS-STMTVRSN    IS EQUAL TO  4
099050000000               MOVE WS-SH11-NOMINEE    TO  SH11V4-NOMINEE
099060000000               MOVE WS-SH11-FAX        TO  SH11V4-FAX
099070990922               GO TO LR1-0030-0200
099080990922           END-IF
099090000000
099100000000           IF  WS-STMTVRSN    IS EQUAL TO  5
099110000000               MOVE WS-SH11-NOMINEE    TO  SH11V5-NOMINEE
099120000000               MOVE WS-SH11-FAX        TO  SH11V5-FAX
099130990922               GO TO LR1-0030-0200
099140990922           END-IF
099150000000
099160990922           IF  WS-STMTVRSN    IS EQUAL TO  6
099170990922               GO TO LR1-0030-0200
099180990922           END-IF
099190990922
099200991019           IF  WS-STMTVRSN    IS EQUAL TO  7
099210991022           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
099220991019               MOVE WS-SH11-NOMINEE    TO  SH11V7-NOMINEE
099230991019               MOVE WS-SH11-FAX        TO  SH11V7-FAX
099240991019               GO TO LR1-0030-0200
099250991019           END-IF
099260991022
099270991022           IF  WS-STMTVRSN    IS EQUAL TO  7
099280991022           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
099290991022               MOVE WS-SH11-NOMINEE    TO  SH11V7-NOMINEE
099300991022               MOVE WS-SH02-SIN-NO     TO  SH11V7-FAX
099310991022               GO TO LR1-0030-0200
099320991022           END-IF
099330991213
099340991213           IF  WS-STMTVRSN    IS EQUAL TO  8
099350991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "Y"
099360991213               MOVE WS-SH11-NOMINEE    TO  SH11V8-NOMINEE
099370991213               MOVE WS-SH11-FAX        TO  SH11V8-FAX
099380991213               GO TO LR1-0030-0200
099390991213           END-IF
099400991213
099410991213           IF  WS-STMTVRSN    IS EQUAL TO  8
099420991213           AND WS-LDA-PHONE-FAX-PRINT          IS EQUAL TO "N"
099430991213               MOVE WS-SH11-NOMINEE    TO  SH11V8-NOMINEE
099440991213               MOVE WS-SH02-SIN-NO     TO  SH11V8-FAX
099450991213               GO TO LR1-0030-0200
099460991213           END-IF
099470991213
099480991022
099490000000           MOVE WS-SH11-NOMINEE        TO  SH11-NOMINEE.
099500000000           MOVE WS-SH11-FAX            TO  SH11-FAX.
099510000000
099520000000       LR1-0030-0200.
099530990923
099540990923           SET SPL                     UP BY  1.
099550000000           MOVE STMT-HDG-11            TO  WS-STMTPGE-LNE (SPL).
099560990925
099570990925           IF WS-STMTVRSN                   IS EQUAL TO  6
099580990925              SET SPL                       UP BY  1
099590990925              MOVE WS-SHL06-1 (SHL)         TO  WS-STMTPGE-LNE (SPL)
099600990925              SET SPL                       UP BY  1
099610990925              MOVE WS-SHL06-2 (SHL)         TO  WS-STMTPGE-LNE (SPL)
099620990925              SET SPL                       UP BY  1
099630990925              MOVE WS-SHL06-3 (SHL)         TO  WS-STMTPGE-LNE (SPL)
099640990925              SET SPL                       UP BY  1
099650990925              MOVE WS-SHL06-4 (SHL)         TO  WS-STMTPGE-LNE (SPL)
099660991019              GO TO LR1-0030-0300
099670991019           END-IF
099680991019
099690991019           IF WS-STMTVRSN                   IS EQUAL TO  7
099700990925              IF WS-LDA-PRINT-HEADING IS NOT EQUAL TO "Y"
099710990925                 GO TO LR1-0030-0300
099720990925              ELSE
099730991013                 SET SPL                     UP BY  1
099740991019                 MOVE WS-SHL07-1 (SHL)       TO  WS-STMTPGE-LNE (SPL)
099750990925                 SET SPL                     UP BY  1
099760991019                 MOVE WS-SHL07-2 (SHL)       TO  WS-STMTPGE-LNE (SPL)
099770990925                 SET SPL                     UP BY  1
099780991019                 MOVE WS-SHL07-3 (SHL)       TO  WS-STMTPGE-LNE (SPL)
099790990925                 SET SPL                     UP BY  1
099800991019                 MOVE WS-SHL07-4 (SHL)       TO  WS-STMTPGE-LNE (SPL)
099810990925              END-IF
099820991019              GO TO LR1-0030-0300
099830991019           END-IF
099840990925
099850991020           IF WS-LDA-PRINT-HEADING           IS NOT EQUAL TO "Y"
099860991020              GO TO LR1-0030-0300
099870991020           END-IF
099880991020
099890991020           SET SPL                     UP BY  1.
099900991020           MOVE WS-SHL01 (SHL)         TO  WS-STMTPGE-LNE (SPL).
099910991020           SET SPL                     UP BY  1.
099920991020           MOVE WS-SHL02 (SHL)         TO  WS-STMTPGE-LNE (SPL).
099930991020           SET SPL                     UP BY  1.
099940991020           MOVE WS-SHL03 (SHL)         TO  WS-STMTPGE-LNE (SPL).
099950991020           SET SPL                     UP BY  1.
099960991020           MOVE WS-SHL04 (SHL)         TO  WS-STMTPGE-LNE (SPL).
099970991019
099980990624       LR1-0030-0300.
099990990925
100000000000      *****
100010000000      *****    Format the messages.  Initially, attempt to format the
100020000000      *****    statement messages using the Account Type Code.  If none
100030000000      *****    are found, use an Account Type Code of spaces.
100040021213R15028*****
100050021213  ¦   *****RFS15028 implements the following logic:
100060021213  ¦   *****    For new investor: get new investor's info. If not found
100070021213  ¦   *****    using existing info(new-investor = 'N'). If not found,
100080021213  ¦   *****    using All info(new-investor = 'A'). If not found, using
100090021213  ¦   *****    Account Type Code of spaces.
100100021213R15028*****
100110000000
100120000000           MOVE SPACES                 TO  WS-STATEMENT-PAGE-MESSAGE.
100130000000           MOVE AD-FUND-NO             TO  WS-FUND-NO.
100140000000
100150021213R15028 LR1-0030-0400.
100160021213  ¦
100170021213  ¦        MOVE LOW-VALUES             TO  STMT-MSG-RECD.
100180190619      *RFS183449 - Start
100190190619  ¦   *    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
100200190619  ¦        MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
100210190619      *RFS183449 - End
100220021213  ¦                                        OF STMT-MSG-RECD.
100230021213  ¦        MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
100240021213  ¦                                        OF STMT-MSG-RECD.
100250021213  ¦        MOVE II-NEW-INVESTOR-FLAG   TO  WS-NEW-INVESTOR-FLAG.
100260021213  ¦        MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
100270021213  ¦                                        OF STMT-MSG-RECD.
100280021213  ¦        MOVE 1                      TO  LINE-NO
100290021213  ¦                                        OF STMT-MSG-RECD.
100300021213  ¦   *******
100310021213  ¦   *******   Direct read to determine if the specific investor(New or
100320021213  ¦   *******   not) infor exist or not.
100330021213  ¦   *******
100340021213  ¦
100350021213  ¦        READ STMT-MSG-FILE
100360021213  ¦          INVALID KEY
100370021213  ¦            IF WS-NEW-INVESTOR-FLAG  = "Y"
100380021213  ¦               MOVE "N"               TO WS-NEW-INVESTOR-FLAG
100390021213  ¦               MOVE WS-NEW-INVESTOR-FLAG  TO  NEW-INVESTOR
100400021213  ¦                                          OF STMT-MSG-RECD
100410021213  ¦               READ STMT-MSG-FILE
100420021213  ¦                   INVALID KEY
100430021213  ¦                   MOVE "A"           TO WS-NEW-INVESTOR-FLAG
100440021213  ¦               END-READ
100450021213  ¦            ELSE
100460021213  ¦               MOVE "A"               TO WS-NEW-INVESTOR-FLAG
100470021213  ¦            END-IF
100480021213R15028     END-READ.
100490021213
100500000000       LR1-0040.
100510990923
100520000000           MOVE LOW-VALUES             TO  STMT-MSG-RECD.
100530190619183449*    MOVE AD-ACCOUNT-TYPE-SHORT  TO  ACCOUNT-TYPE-CODE
100540190619183449     MOVE Lc-Acct-Type-Code      TO  ACCOUNT-TYPE-CODE
100550000000                                           OF STMT-MSG-RECD.
100560000000           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
100570000000                                           OF STMT-MSG-RECD.
100580021213R15028     MOVE WS-NEW-INVESTOR-FLAG   TO  NEW-INVESTOR
100590021213  ¦                                        OF STMT-MSG-RECD.
100600021213  ¦        MOVE ZEROES                 TO  LINE-NO
100610021213R15028                                     OF STMT-MSG-RECD.
100620021213
100630000000           START STMT-MSG-FILE
100640000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
100650000000               INVALID KEY
100660990922                   GO TO LR1-0070
100670990922           END-START.
100680990922
100690000000       LR1-0050.
100700990922
100710000000           READ STMT-MSG-FILE NEXT     AT  END
100720990922               GO TO LR1-0070
100730990922           END-READ
100740990922
100750000000           IF  LINE-NO    IS GREATER THAN  WS-SPML-MAX
100760990922           OR  LANGUAGE-CODE OF STMT-MSG-RECD
100770000000                          IS NOT EQUAL TO  AD-LANGUAGE
100780990922           OR  ACCOUNT-TYPE-CODE OF STMT-MSG-RECD
100790190619183449*                   IS NOT EQUAL TO  AD-ACCOUNT-TYPE-SHORT
100800190619183449                    IS NOT EQUAL TO  Lc-Acct-Type-Code
100810021213R15028     OR  NEW-INVESTOR OF STMT-MSG-RECD  IS NOT EQUAL
100820021213R15028                         TO  WS-NEW-INVESTOR-FLAG
100830990922               GO TO LR1-0070
100840990922           END-IF
100850990922
100860000000           IF  LINE-NO   NOT GREATER THAN  ZEROES
100870990922               GO TO LR1-0060
100880990922           END-IF
100890000000
100900000000           MOVE SPACES                 TO  STMT-MSG-01.
100910000000           MOVE STATEMENT-MESSAGE      TO  SM01-STATEMENT-MESSAGE.
100920000000
100930000000           SET SPML                    TO  LINE-NO.
100940000000           MOVE STMT-MSG-01            TO  WS-STMTPGE-MSGLNE (SPML).
100950000000
100960000000       LR1-0060.
100970990922
100980000000           GO TO LR1-0050.
100990000000
101000000000       LR1-0070.
101010000000
101020000000      *****
101030000000      *****    If no statement messages were formatted and the Account
101040000000      *****    Type Code being used was not spaces, attempt to format
101050000000      *****    statement messages using an Account Type Code of spaces.
101060000000      *****
101070000000
101080990922           IF  WS-STATEMENT-PAGE-MESSAGE  IS EQUAL TO  SPACES
101090990922           AND AD-ACCOUNT-TYPE-SHORT      IS NOT EQUAL TO  SPACES
101100990925               MOVE SPACES                TO  AD-ACCOUNT-TYPE-SHORT
101110021213R15028*        GO TO LR1-0040
101120021213R15028         GO TO LR1-0030-0400
101130990922           END-IF
101140990922
101150000000           IF  WS-STMTVRSN    IS EQUAL TO  1
101160000000               MOVE WS-STATEMENT-PAGE-MESSAGE
101170000000                                       TO  WS-STMTPGE-MSG-V1
101180990922               GO TO LR1-0070-0000-0010
101190990922           END-IF
101200000000
101210000000           IF  WS-STMTVRSN    IS EQUAL TO  2
101220990716           AND WS-LDA-PRINT-HEADING IS EQUAL TO "N"
101230000000               MOVE WS-STATEMENT-PAGE-MESSAGE
101240000000                                       TO  WS-STMTPGE-MSG-V2
101250990922               GO TO LR1-0070-0000-0010
101260990922           END-IF
101270000000
101280990716           IF  WS-STMTVRSN    IS EQUAL TO  2
101290990716           AND WS-LDA-PRINT-HEADING IS EQUAL TO "Y"
101300990716               MOVE WS-STATEMENT-PAGE-MESSAGE
101310990727                                       TO  WS-STMTPGE-MSG-V21
101320990922               GO TO LR1-0070-0000-0010
101330990922           END-IF
101340990716
101350000000           IF  WS-STMTVRSN    IS EQUAL TO  3
101360000000               MOVE WS-STATEMENT-PAGE-MESSAGE
101370000000                                       TO  WS-STMTPGE-MSG-V3
101380990922               GO TO LR1-0070-0000-0010
101390990922           END-IF
101400000000
101410000000           IF  WS-STMTVRSN    IS EQUAL TO  4
101420000000               MOVE WS-STATEMENT-PAGE-MESSAGE
101430000000                                       TO  WS-STMTPGE-MSG-V4
101440990922               GO TO LR1-0070-0000-0010
101450990922           END-IF
101460000000
101470000000           IF  WS-STMTVRSN    IS EQUAL TO  5
101480000000               MOVE WS-STATEMENT-PAGE-MESSAGE
101490000000                                       TO  WS-STMTPGE-MSG-V5
101500990922               GO TO LR1-0070-0000-0010
101510990922           END-IF
101520000000
101530990925           IF  WS-STMTVRSN    IS EQUAL TO  6
101540990925               MOVE SPACES             TO  WS-STMTPGE-MSG-V6
101550990925               GO TO LR1-0070-0000-0010
101560990925           END-IF
101570990925
101580991019           IF  WS-STMTVRSN    IS EQUAL TO  7
101590991019               MOVE WS-STATEMENT-PAGE-MESSAGE
101600991019                                       TO  WS-STMTPGE-MSG-V7
101610991019               GO TO LR1-0070-0000-0010
101620991019           END-IF
101630991213
101640991213           IF  WS-STMTVRSN    IS EQUAL TO  8
101650991213               MOVE WS-STATEMENT-PAGE-MESSAGE
101660991213                                       TO  WS-STMTPGE-MSG-V8
101670991213               GO TO LR1-0070-0000-0010
101680991213           END-IF
101690991213
101700000000           MOVE WS-STATEMENT-PAGE-MESSAGE
101710000000                                       TO  WS-STMTPGE-MSG.
101720000000       LR1-0070-0000-0010.
101730000000
101740000000      *****
101750000000      *****    Position at first detail line.
101760000000      *****
101770000000
101780000000           SET SPL                     TO  WS-SPL-DTLFST.
101790000000
101800000000      *****
101810000000      *****    Determine if the client statement copy is to be
101820000000      *****    suppressed.  This is done depending on whether or not the
101830000000      *****    account being processed is a nominee account.
101840000000      *****
101850000000
101860000000           MOVE "N"                    TO  WS-CLTSPR-SW.
101870000000
101880000000           IF  WS-NOMINEE IS NOT EQUAL TO  SPACES
101890990923               GO TO LR1-0070-0030
101900990923           END-IF
101910000000
101920000000      *****
101930000000      *****    Processing a non-nominee account.  Initially read through
101940000000      *****    the Dealer Mailing Instruction.  If the form CLTSTSPR is
101950000000      *****    found, the client copy statement is to be suppressed.
101960000000      *****
101970000000
101980000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
101990000000           MOVE AD-DEALER              TO  DEALER-CODE
102000000000                                           OF DEALER-MAILING-INSTR-RECD.
102010000000           MOVE "CLTSTSPR  "           TO  FORM-CODE
102020000000                                           OF DEALER-MAILING-INSTR-RECD.
102030000000           START DEALER-MAILING-INSTR
102040000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
102050000000               INVALID KEY
102060990923                   GO TO LR1-0070-0010
102070990923           END-START
102080990923
102090000000           READ DEALER-MAILING-INSTR NEXT
102100990923             AT  END
102110990923               GO TO LR1-0070-0010
102120990923           END-READ
102130990923
102140990923           IF  FORM-CODE OF DEALER-MAILING-INSTR-RECD
102150990923                IS NOT EQUAL TO  "CLTSTSPR  "
102160990923           OR  DEALER-CODE OF DEALER-MAILING-INSTR-RECD
102170990923                IS NOT EQUAL TO  AD-DEALER
102180990923               GO TO LR1-0070-0010
102190990923           END-IF
102200990923
102210000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
102220000000           GO TO LR1-0070-0020.
102230000000
102240000000       LR1-0070-0010.
102250000000
102260000000      *****
102270000000      *****    Processing a non-nominee account.  The client statement
102280000000      *****    copy is to be printed, according to the Dealer Mailing
102290000000      *****    Instruction.  Now read through the Dealer Representative
102300000000      *****    Mailing Instruction.  If the form CLTSTSPR is found, the
102310000000      *****    client statement copy is to be suppressed.
102320000000      *****
102330000000
102340000000           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
102350000000           MOVE AD-DEALER              TO  DEALER-CODE
102360000000                                           OF DLRREP-MAILING-INSTR-RECD.
102370000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
102380000000                                           OF DLRREP-MAILING-INSTR-RECD.
102390000000           MOVE "CLTSTSPR  "           TO  FORM-CODE
102400000000                                           OF DLRREP-MAILING-INSTR-RECD.
102410000000           START DEALER-REP-MAILING-INSTR
102420000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
102430000000               INVALID KEY
102440000000                   GO TO LR1-0070-0020.
102450000000
102460000000           READ DEALER-REP-MAILING-INSTR NEXT
102470000000                                       AT  END
102480000000               GO TO LR1-0070-0020.
102490000000
102500000000           IF  FORM-CODE
102510000000               OF DLRREP-MAILING-INSTR-RECD
102520000000                          IS NOT EQUAL TO  "CLTSTSPR  "
102530000000           OR  DEALER-REP-CODE
102540000000               OF DLRREP-MAILING-INSTR-RECD
102550000000                          IS NOT EQUAL TO  AD-SALESREP
102560000000           OR  DEALER-CODE
102570000000               OF DLRREP-MAILING-INSTR-RECD
102580000000                          IS NOT EQUAL TO  AD-DEALER
102590000000               GO TO LR1-0070-0020.
102600000000
102610000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
102620000000
102630000000       LR1-0070-0020.
102640990925
102650000000           GO TO LR1-0070-0050.
102660000000
102670000000       LR1-0070-0030.
102680000000
102690000000      *****
102700000000      *****    Processing a nominee account.  Read through the Nominee
102710000000      *****    Mailing Instruction.  If the form CLTSTCLT is not found,
102720000000      *****    the client copy statement is to be suppressed.
102730000000      *****
102740000000
102750000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
102760000000               GO TO LR1-0070-0050.
102770000000
102780000000           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
102790000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
102800000000                                           OF NOMINEE-MAILING-INST-RECD.
102810000000           MOVE "CLTSTCLT  "           TO  FORM-CODE
102820000000                                           OF NOMINEE-MAILING-INST-RECD.
102830000000           START NOMINEE-MAILING-INSTRUCTIONS
102840000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
102850000000               INVALID KEY
102860000000                   GO TO LR1-0070-0040.
102870000000
102880000000           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
102890000000                                       AT  END
102900000000               GO TO LR1-0070-0040.
102910000000
102920000000           IF  FORM-CODE
102930000000               OF NOMINEE-MAILING-INST-RECD
102940000000                          IS NOT EQUAL TO  "CLTSTCLT  "
102950000000           OR  NOMINEE-CODE
102960000000               OF NOMINEE-MAILING-INST-RECD
102970000000                          IS NOT EQUAL TO  WS-NOMINEE-CODE
102980000000               GO TO LR1-0070-0040.
102990000000
103000000000           GO TO LR1-0070-0050.
103010000000
103020000000       LR1-0070-0040.
103030990925
103040000000           MOVE "Y"                    TO  WS-CLTSPR-SW.
103050000000
103060000000       LR1-0070-0050.
103070000000
103080000000      *****
103090000000      *****    Read through the Dealer Mailing Instruction to determine
103100000000      *****    if the Statement Print Header is to be flagged to print
103110000000      *****    dealer copies of the statements.
103120000000      *****
103130000000
103140000000           MOVE SPACES                 TO  WS-DEAMLGINS-REC.
103150000000
103160000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
103170000000           MOVE AD-DEALER              TO  DEALER-CODE
103180000000                                           OF DEALER-MAILING-INSTR-RECD.
103190000000           MOVE "CLTSTDLR  "           TO  FORM-CODE
103200000000                                           OF DEALER-MAILING-INSTR-RECD.
103210000000           START DEALER-MAILING-INSTR
103220000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
103230000000               INVALID KEY
103240990925                   GO TO LR1-0080
103250990925           END-START
103260990925
103270000000           READ DEALER-MAILING-INSTR NEXT
103280000000                                       AT  END
103290990925               GO TO LR1-0080
103300990925           END-READ
103310990925
103320000000           IF  FORM-CODE
103330000000               OF DEALER-MAILING-INSTR-RECD
103340000000                          IS NOT EQUAL TO  "CLTSTDLR  "
103350000000           OR  DEALER-CODE
103360000000               OF DEALER-MAILING-INSTR-RECD
103370000000                          IS NOT EQUAL TO  AD-DEALER
103380000000               GO TO LR1-0080.
103390000000
103400000000           MOVE DEALER-MAILING-INSTR-RECD
103410000000                                       TO  WS-DEAMLGINS-REC.
103420000000
103430000000       LR1-0080.
103440000000
103450000000      *****
103460000000      *****    Determine if the Statement Print Header is to be flagged
103470000000      *****    to print dealer representative copies of the statements.
103480000000      *****    Initially read through the Dealer Mailing Instruction.
103490000000      *****    If a record is not found, read through the Dealer
103500000000      *****    Representative Mailing Instuction.
103510000000      *****
103520000000
103530000000           MOVE SPACES                 TO  WS-DERMLGINS-REC.
103540000000
103550000000           MOVE SPACES                 TO  DEALER-MAILING-INSTR-RECD.
103560000000           MOVE AD-DEALER              TO  DEALER-CODE
103570000000                                           OF DEALER-MAILING-INSTR-RECD.
103580000000           MOVE "CLTSTREP  "           TO  FORM-CODE
103590000000                                           OF DEALER-MAILING-INSTR-RECD.
103600000000           START DEALER-MAILING-INSTR
103610000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
103620000000               INVALID KEY
103630000000                   GO TO LR1-0080-0010.
103640000000
103650000000           READ DEALER-MAILING-INSTR NEXT
103660000000                                       AT  END
103670000000               GO TO LR1-0080-0010.
103680000000
103690000000           IF  FORM-CODE
103700000000               OF DEALER-MAILING-INSTR-RECD
103710000000                          IS NOT EQUAL TO  "CLTSTREP  "
103720000000           OR  DEALER-CODE
103730000000               OF DEALER-MAILING-INSTR-RECD
103740000000                          IS NOT EQUAL TO  AD-DEALER
103750000000               GO TO LR1-0080-0010.
103760000000
103770000000           MOVE CORR MFADEAMIP
103780000000                     OF DEALER-MAILING-INSTR-RECD
103790000000                                       TO  MFADERMIP
103800000000                                           OF WS-DERMLGINS-REC.
103810000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
103820000000                                           OF WS-DERMLGINS-REC.
103830000000           GO TO LR1-0090.
103840000000
103850000000       LR1-0080-0010.
103860990923
103870000000           MOVE SPACES                 TO  DLRREP-MAILING-INSTR-RECD.
103880000000           MOVE AD-DEALER              TO  DEALER-CODE
103890000000                                           OF DLRREP-MAILING-INSTR-RECD.
103900000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
103910000000                                           OF DLRREP-MAILING-INSTR-RECD.
103920000000           MOVE "CLTSTREP  "           TO  FORM-CODE
103930000000                                           OF DLRREP-MAILING-INSTR-RECD.
103940000000           START DEALER-REP-MAILING-INSTR
103950000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
103960000000               INVALID KEY
103970000000                   GO TO LR1-0090.
103980000000
103990000000           READ DEALER-REP-MAILING-INSTR NEXT
104000000000                                       AT  END
104010000000               GO TO LR1-0090.
104020000000
104030000000           IF  FORM-CODE
104040000000               OF DLRREP-MAILING-INSTR-RECD
104050000000                          IS NOT EQUAL TO  "CLTSTREP  "
104060000000           OR  DEALER-REP-CODE
104070000000               OF DLRREP-MAILING-INSTR-RECD
104080000000                          IS NOT EQUAL TO  AD-SALESREP
104090000000           OR  DEALER-CODE
104100000000               OF DLRREP-MAILING-INSTR-RECD
104110000000                          IS NOT EQUAL TO  AD-DEALER
104120000000               GO TO LR1-0090.
104130000000
104140000000           MOVE DLRREP-MAILING-INSTR-RECD
104150000000                                       TO  WS-DERMLGINS-REC.
104160000000
104170000000       LR1-0090.
104180000000
104190000000      *****
104200000000      *****    Read through the Nominee Instruction to determine if the
104210000000      *****    Statement Print Header is to be flagged to print nominee
104220000000      *****    copies of the statements.
104230000000      *****
104240000000
104250000000           MOVE SPACES                 TO  WS-NOMMLGINS-REC.
104260000000           IF  WS-NOMINEE-CODE   EQUAL TO  SPACES
104270000000               GO TO LR1-0090-0010.
104280000000
104290000000           MOVE SPACES                 TO  NOMINEE-MAILING-INST-RECD.
104300000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
104310000000                                           OF NOMINEE-MAILING-INST-RECD.
104320000000           MOVE "CLTSTNOM  "           TO  FORM-CODE
104330000000                                           OF NOMINEE-MAILING-INST-RECD.
104340000000           START NOMINEE-MAILING-INSTRUCTIONS
104350000000               KEY        IS GREATER THAN  EXTERNALLY-DESCRIBED-KEY
104360000000               INVALID KEY
104370000000                   GO TO LR1-0090-0010.
104380000000
104390000000           READ NOMINEE-MAILING-INSTRUCTIONS NEXT
104400000000                                       AT  END
104410000000               GO TO LR1-0090-0010.
104420000000
104430000000           IF  FORM-CODE
104440000000               OF NOMINEE-MAILING-INST-RECD
104450000000                          IS NOT EQUAL TO  "CLTSTNOM  "
104460000000           OR  NOMINEE-CODE
104470000000               OF NOMINEE-MAILING-INST-RECD
104480000000                          IS NOT EQUAL TO  WS-NOMINEE-CODE
104490000000               GO TO LR1-0090-0010.
104500000000
104510000000           MOVE NOMINEE-MAILING-INST-RECD
104520000000                                       TO  WS-NOMMLGINS-REC.
104530000000
104540000000       LR1-0090-0010.
104550000000
104560000000      *****
104570000000      *****    Format the Statement Print Header record.
104580000000      *****
104590020921
104600000000
104610000000           MOVE "N"                    TO  WS-STMT-PRT-HDR-WSW.
104620000000
104630000000           MOVE SPACES                 TO  STMT-PRT-HDR-RECD.
104640000000           MOVE AD-INVESTOR-NO         TO  INVESTOR-NO
104650000000                                           OF STMT-PRT-HDR-RECD.
104660020921R13756*    IF WS-MSTR-ACCOUNT-NO IS NOT EQUAL TO 0
104670020921  ¦   *     MOVE WS-MSTR-ACCOUNT-NO  TO  ACCOUNT
104680020921  ¦   *                                    OF STMT-PRT-HDR-RECD
104690020921R13756*    ELSE
104700020921            MOVE AD-ACCOUNT             TO  ACCOUNT
104710020921                                           OF STMT-PRT-HDR-RECD
104720020921R13756*    END-IF
104730000000           MOVE AD-LANGUAGE            TO  LANGUAGE-CODE
104740000000                                           OF STMT-PRT-HDR-RECD.
104750000000           MOVE AD-DEALER              TO  DEALER
104760000000                                           OF STMT-PRT-HDR-RECD.
104770000000           MOVE AD-SALESREP            TO  DEALER-REP-CODE
104780000000                                           OF STMT-PRT-HDR-RECD.
104790000000           MOVE II-SIN-NO              TO  SOCIAL-INSURANCE-NO
104800000000                                           OF STMT-PRT-HDR-RECD.
104810000000           MOVE WS-CLTSPR-SW           TO  CLIENT-COPY-SUPPRESSED
104820000000                                           OF STMT-PRT-HDR-RECD.
104830000000           MOVE II-HOLD-MAIL           TO  HOLD-MAIL
104840000000                                           OF STMT-PRT-HDR-RECD.
104850000000           MOVE BRANCH-CODE
104860000000                OF WS-DEAMLGINS-REC    TO  DEALER-BRANCH-CODE
104870000000                                           OF STMT-PRT-HDR-RECD.
104880000000           MOVE FREQUENCY-CODE
104890000000                OF WS-DEAMLGINS-REC    TO  DEALER-FREQUENCY-CODE
104900000000                                           OF STMT-PRT-HDR-RECD.
104910000000           MOVE METHOD-OF-DELIVERY
104920000000                OF WS-DEAMLGINS-REC    TO  DEALER-METHOD-OF-DELIVERY
104930000000                                           OF STMT-PRT-HDR-RECD.
104940000000           MOVE SEND-TO
104950000000                OF WS-DEAMLGINS-REC    TO  DEALER-SEND-TO
104960000000                                           OF STMT-PRT-HDR-RECD.
104970000000           MOVE BRANCH-CODE
104980000000                OF WS-DERMLGINS-REC    TO  DLR-REP-BRANCH-CODE
104990000000                                           OF STMT-PRT-HDR-RECD.
105000000000           MOVE FREQUENCY-CODE
105010000000                OF WS-DERMLGINS-REC    TO  DLR-REP-FREQUENCY-CODE
105020000000                                           OF STMT-PRT-HDR-RECD.
105030000000           MOVE METHOD-OF-DELIVERY
105040000000                OF WS-DERMLGINS-REC    TO  DLR-REP-METHOD-OF-DELIVERY
105050000000                                           OF STMT-PRT-HDR-RECD.
105060000000           MOVE SEND-TO
105070000000                OF WS-DERMLGINS-REC    TO  DLR-REP-SEND-TO
105080000000                                           OF STMT-PRT-HDR-RECD.
105090000000           MOVE WS-NOMINEE-CODE        TO  NOMINEE-CODE
105100000000                                           OF STMT-PRT-HDR-RECD.
105110000000           MOVE BRANCH-CODE
105120000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-BRANCH-CODE
105130000000                                           OF STMT-PRT-HDR-RECD.
105140000000           MOVE FREQUENCY-CODE
105150000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-FREQUENCY-CODE
105160000000                                           OF STMT-PRT-HDR-RECD.
105170000000           MOVE METHOD-OF-DELIVERY
105180000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-METHOD-OF-DELIVERY
105190000000                                           OF STMT-PRT-HDR-RECD.
105200000000           MOVE SEND-TO
105210000000                OF WS-NOMMLGINS-REC    TO  NOMINEE-SEND-TO
105220000000                                           OF STMT-PRT-HDR-RECD.
105230000000           MOVE II-COUNTRY-CODE        TO  COUNTRY-CODE
105240000000                                           OF STMT-PRT-HDR-RECD.
105250980116           MOVE II-LAST-NAME           TO  LAST-NAME
105260980116                                           OF STMT-PRT-HDR-RECD.
105270980116           MOVE II-FIRST-NAME          TO  FIRST-NAME
105280980116                                           OF STMT-PRT-HDR-RECD.
105290980116           MOVE II-INITIALS            TO  INITIALS
105300980116                                           OF STMT-PRT-HDR-RECD.
105310990105           MOVE II-LEGAL-NAME          TO  LEGAL-NAME
105320990105                                           OF STMT-PRT-HDR-RECD.
105330980129           MOVE AD-START-DATE          TO  START-DATE
105340980129                                           OF STMT-PRT-HDR-RECD.
105350980129           MOVE AD-END-DATE            TO  END-DATE
105360980129                                           OF STMT-PRT-HDR-RECD.
105370980116
105380000000      *****
105390000000      *****    Initialize the statement currency totals.
105400000000      *****
105410000000
105420000000           SET SC                      TO  1.
105430000000
105440000000       LR1-0100.
105450990923
105460000000           MOVE ZEROES                 TO  WS-SC-MARKET-VALUE (SC).
105470000000           MOVE "N"                    TO  WS-SC-RPTIND (SC).
105480990925
105490000000           IF  SC            IS LESS THAN  WS-SC-MAX
105500000000               SET SC               UP BY  1
105510000000               GO TO LR1-0100.
105520000000
105530000000      *****
105540000000      *****    Initialize the statement account funds table.
105550000000      *****
105560000000
105570000000           MOVE SPACES                 TO  WS-STMT-ACCOUNT-FUND-TABLE.
105580000000           MOVE ZEROES                 TO  WS-SAF-LST.
105590000000
105600000000      *****
105610000000      *****    Initialize the statement transaction controls.
105620000000      *****
105630000000
105640000000           MOVE LOW-VALUES             TO  CURR-CTLS-DPR.
105650000000           MOVE LOW-VALUES             TO  PREV-CTLS-DPR.
105660000000
105670000000       LR1-EXIT.
105680000000           EXIT.
105690000000      /
105700000000       FORMAT-ACTTYP-DESC.
105710000000
105720000000      ******************************************************************
105730000000      *                                                                *
105740000000      *    F M T .   A C C O U N T   T Y P E   D E S C R I P T I O N   *
105750000000      *                                                                *
105760000000      ******************************************************************
105770000000
105780000000      *****
105790000000      *****    Initialize the work items.
105800000000      *****
105810000000
105820000000           MOVE SPACES                 TO  WS-ACTTYPIN-SFX.
105830000000           MOVE SPACES                 TO  WS-ACTTYPLIT1.
105840000000           MOVE SPACES                 TO  WS-ACTTYPLIT2.
105850000000           MOVE SPACES                 TO  WS-ACTTYPLIT3.
105860000000           MOVE SPACES                 TO  WS-ACTTYPOUT.
105870000000
105880000000      *****
105890980326      *****    The account type description will be formatted
105900980326      *****    If NOMINEE, format the description.
105910000000      *****
105920980326
105930980326980320 FAD-0001.
105940980326
105950980326980320     IF  WS-ACTTYPNOM   IS NOT = "Y"
105960980326980320         GO TO FAD-0003.
105970980326
105980980326980320     INITIALIZE INVESTOR-J-REC.
105990980326980320     MOVE PR-INVESTOR-NO TO ACIVRNO OF INVESTOR-J-REC.
106000980326980320     MOVE PR-ACCOUNT     TO ACACTNO OF INVESTOR-J-REC.
106010980326
106020980326980320     START INVESTOR-JOIN
106030980326980320           KEY IS NOT LESS THAN EXTERNALLY-DESCRIBED-KEY
106040980326980320           INVALID KEY
106050980326980320           GO TO FAD-0001.
106060980326
106070980326980320 FAD-0002.
106080980326980320     READ INVESTOR-JOIN NEXT RECORD
106090980326980320          AT END
106100980326980320          GO TO FAD-0030
106110980326980320     END-READ.
106120980326
106130980326980320     IF PR-INVESTOR-NO IS NOT = ACIVRNO
106140980326980320                          OF INVESTOR-J-REC
106150980326980320        GO TO FAD-0030.
106160980326
106170980326980320     IF PR-ACCOUNT     IS NOT = ACACTNO
106180980326980320                          OF INVESTOR-J-REC
106190980326980320        GO TO FAD-0030.
106200980326
106210980409           INITIALIZE MFANMTPYP.
106220980408980320     MOVE ANACTYP OF INVESTOR-JOIN TO NOMINEE-ACCOUNT-TYPE
106230980408980320                  OF NOMINEE-ACCOUNT-TYPE-DESCR.
106240980408980320     MOVE WS-ACTTYPLNG             TO LANGUAGE-CODE
106250980408980320                  OF NOMINEE-ACCOUNT-TYPE-DESCR.
106260980326
106270980408980320     READ NOMINEE-ACCOUNT-TYPE-DESCR
106280980408980320          INVALID KEY
106290980408980320          MOVE SPACES TO WS-ACTTYPOUT
106300980408980320     END-READ.
106310980326
106320990922990922     IF WS-STMTVRSN        IS NOT EQUAL 6
106330990922980320        MOVE ACCOUNT-TYPE-DESCR OF NOMINEE-ACCOUNT-TYPE-DESCR
106340990922980320                                TO WS-ACTTYPOUT
106350990922           ELSE
106360990922              MOVE SPACES             TO WS-ACTTYPOUT
106370990922           END-IF
106380980326
106390980408980320     GO TO FAD-0030.
106400980326
106410980326980320 FAD-0003.
106420000000
106430000000           IF  WS-ACTTYPLNG   IS EQUAL TO  "F"
106440990922               GO TO FAD-0040
106450990922           END-IF
106460990922
106470000000      *****
106480000000      *****    The English description may be formatted with a LOCKED-IN
106490000000      *****    literal, a SPOUSAL literal, a GROUP literal and the
106500000000      *****    original account type description.  If any of the
106510000000      *****    literals are not applicable, they will be suppressed.
106520000000      *****    Initially, determine which literals are applicable.
106530000000      *****
106540000000
106550000000           IF  WS-ACTTYPLCK   IS EQUAL TO  "Y"
106560000000           AND WS-ACTTYPSHT  NOT EQUAL TO  "LIRA "
106570000000               MOVE "LOCKED-IN    "    TO  WS-ACTTYPLIT1.
106580000000           IF  WS-ACTTYPGRP   IS EQUAL TO  "Y"
106590000000               MOVE "GROUP        "    TO  WS-ACTTYPLIT2.
106600000000           IF  WS-ACTTYPSPS   IS EQUAL TO  "Y"
106610000000               MOVE "SPOUSAL      "    TO  WS-ACTTYPLIT3.
106620000000
106630000000      *****
106640000000      *****    Compress the optional English literals.
106650000000      *****
106660000000
106670000000           IF  WS-ACTTYPLIT1 NOT EQUAL TO  SPACES
106680000000               GO TO FAD-0010.
106690000000
106700000000           IF  WS-ACTTYPLIT2 NOT EQUAL TO  SPACES
106710000000               MOVE WS-ACTTYPLIT2      TO  WS-ACTTYPLIT1
106720000000               MOVE SPACES             TO  WS-ACTTYPLIT2
106730000000               GO TO FAD-0010.
106740000000
106750000000           IF  WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
106760000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT1
106770000000               MOVE SPACES             TO  WS-ACTTYPLIT3
106780000000               GO TO FAD-0020.
106790000000
106800000000           GO TO FAD-0020.
106810000000
106820000000       FAD-0010.
106830990922
106840000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
106850000000           AND WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
106860000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT2
106870990922               MOVE SPACES             TO  WS-ACTTYPLIT3
106880990922           END-IF.
106890990922
106900000000       FAD-0020.
106910000000
106920000000      *****
106930000000      *****    Format the English account type description based on the
106940000000      *****    number of optional literals applicable.
106950000000      *****
106960000000
106970000000           IF  WS-ACTTYPLIT1  IS EQUAL TO  SPACES
106980000000               MOVE WS-ACTTYPIN        TO  WS-ACTTYPOUT
106990000000               GO TO FAD-0030.
107000000000
107010000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
107020000000               STRING WS-ACTTYPLIT1
107030000000                             DELIMITED BY  "  ",
107040000000                      " "    DELIMITED BY  SIZE,
107050000000                      WS-ACTTYPIN
107060000000                             DELIMITED BY  "  ",
107070000000                                     INTO  WS-ACTTYPOUT
107080000000               GO TO FAD-0030.
107090000000
107100000000           IF  WS-ACTTYPLIT3  IS EQUAL TO  SPACES
107110000000               STRING WS-ACTTYPLIT1
107120000000                             DELIMITED BY  "  ",
107130000000                      " "    DELIMITED BY  SIZE,
107140000000                      WS-ACTTYPLIT2
107150000000                             DELIMITED BY  "  ",
107160000000                      " "    DELIMITED BY  SIZE,
107170000000                      WS-ACTTYPIN
107180000000                             DELIMITED BY  "  ",
107190000000                                     INTO  WS-ACTTYPOUT
107200000000               GO TO FAD-0030.
107210000000
107220000000           STRING WS-ACTTYPLIT1
107230000000                             DELIMITED BY  "  ",
107240000000                  " "        DELIMITED BY  SIZE,
107250000000                  WS-ACTTYPLIT2
107260000000                             DELIMITED BY  "  ",
107270000000                  " "        DELIMITED BY  SIZE,
107280000000                  WS-ACTTYPLIT3
107290000000                             DELIMITED BY  "  ",
107300000000                  " "        DELIMITED BY  SIZE,
107310000000                  WS-ACTTYPIN
107320000000                             DELIMITED BY  "  ",
107330000000                                     INTO  WS-ACTTYPOUT.
107340000000
107350000000       FAD-0030.
107360000000           GO TO FAD-EXIT.
107370000000
107380000000       FAD-0040.
107390000000
107400000000      *****
107410000000      *****    The French description may be formatted with a GROUP
107420000000      *****    literal, a LOCKED-IN literal, a SPOUSAL literal and the
107430000000      *****    original account type description.  If any of the
107440000000      *****    literals are not applicable, they will be suppressed.
107450000000      *****    Initially, determine which literals are applicable.
107460000000      *****
107470000000
107480000000           IF  WS-ACTTYPGRP   IS EQUAL TO  "Y"
107490000000               MOVE "COLLECTIF    "    TO  WS-ACTTYPLIT1.
107500000000           IF  WS-ACTTYPLCK   IS EQUAL TO  "Y"
107510000000           AND WS-ACTTYPSHT  NOT EQUAL TO  "LIRA "
107520000000               MOVE "IMMOBILISE   "    TO  WS-ACTTYPLIT2.
107530000000           IF  WS-ACTTYPSPS   IS EQUAL TO  "Y"
107540000000               MOVE "DE CONJOINT  "    TO  WS-ACTTYPLIT3.
107550000000
107560000000      *****
107570000000      *****    Compress the optional French literals.
107580000000      *****
107590000000
107600000000           IF  WS-ACTTYPLIT1 NOT EQUAL TO  SPACES
107610000000               GO TO FAD-0050.
107620000000
107630000000           IF  WS-ACTTYPLIT2 NOT EQUAL TO  SPACES
107640000000               MOVE WS-ACTTYPLIT2      TO  WS-ACTTYPLIT1
107650000000               MOVE SPACES             TO  WS-ACTTYPLIT2
107660000000               GO TO FAD-0050.
107670000000
107680000000           IF  WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
107690000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT1
107700000000               MOVE SPACES             TO  WS-ACTTYPLIT3
107710000000               GO TO FAD-0060.
107720000000
107730000000           GO TO FAD-0060.
107740000000
107750000000       FAD-0050.
107760000000           IF  WS-ACTTYPLIT2  IS EQUAL TO  SPACES
107770000000           AND WS-ACTTYPLIT3 NOT EQUAL TO  SPACES
107780000000               MOVE WS-ACTTYPLIT3      TO  WS-ACTTYPLIT2
107790000000               MOVE SPACES             TO  WS-ACTTYPLIT3.
107800000000
107810000000       FAD-0060.
107820000000
107830000000      *****
107840000000      *****    Format the French account type description.  Since the
107850000000      *****    optional literals come after the original account
107860000000      *****    description and they have been compressed, it is not
107870000000      *****    necessary to base the formatting on which option literals
107880000000      *****    are applicable.
107890000000      *****
107900000000
107910000000           STRING WS-ACTTYPIN
107920000000                             DELIMITED BY  "  ",
107930000000                  " "        DELIMITED BY  SIZE,
107940000000                  WS-ACTTYPLIT1
107950000000                             DELIMITED BY  "  ",
107960000000                  " "        DELIMITED BY  SIZE,
107970000000                  WS-ACTTYPLIT2
107980000000                             DELIMITED BY  "  ",
107990000000                  " "        DELIMITED BY  SIZE,
108000000000                  WS-ACTTYPLIT3
108010000000                             DELIMITED BY  "  ",
108020000000                                     INTO  WS-ACTTYPOUT.
108030000000
108040000000       FAD-EXIT.
108050000000           EXIT.
108060000000      /
108070000000       PRINT-ERR-LINE.
108080000000
108090000000      ******************************************************************
108100000000      *                                                                *
108110000000      *    P R I N T   E R R O R   L I N E   R O U T I N E             *
108120000000      *                                                                *
108130000000      ******************************************************************
108140000000
108150000000      *****
108160000000      *****    If the error line has not been formatted, format it using
108170000000      *****    the Statement File - Primary record.
108180000000      *****
108190000000
108200000000           IF  ERROR-LINE-1  NOT EQUAL TO  SPACES
108210000000               GO TO PEL-0010.
108220000000
108230000000           MOVE PR-INVESTOR-NO         TO  EL1-INVESTOR-NO.
108240000000           MOVE PR-ACCOUNT             TO  EL1-ACCOUNT.
108250000000           MOVE PR-SECTION-NO          TO  EL1-SECTION-NO.
108260000000           MOVE "-"                    TO  EL1-SECTION-LITDSH.
108270000000           MOVE PR-SECTION-SEQUENCE    TO  EL1-SECTION-SEQUENCE.
108280000000           MOVE PR-LANGUAGE-CODE       TO  EL1-LANGUAGE-CODE.
108290000000           MOVE PR-DEALER              TO  EL1-DEALER.
108300000000           MOVE PR-DEALER-REP-CODE     TO  EL1-DEALER-REP-CODE.
108310000000           MOVE WS-MSG (MSG)           TO  EL1-MSG.
108320000000
108330000000       PEL-0010.
108340000000
108350000000      *****
108360000000      *****    If the page is full, start a new page and print the
108370000000      *****    Heading lines.
108380000000      *****
108390000000
108400000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
108410000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
108420000000
108430000000      *****
108440000000      *****    Print the error line.
108450000000      *****
108460000000
108470000000           WRITE PRINT-RECD          FROM  ERROR-LINE-1
108480000000                         AFTER  ADVANCING  1.
108490000000           ADD 1                       TO  WS-LNECNT.
108500000000
108510000000       PEL-EXIT.
108520000000           EXIT.
108530000000      /
108540000000       INITIAL-LOGIC-RTN.
108550000000
108560000000      ******************************************************************
108570000000      *                                                                *
108580000000      *    I N I T I A L   L O G I C   R O U T I N E                   *
108590000000      *                                                                *
108600000000      ******************************************************************
108610000000
108620000000      *****
108630000000      *****    Retrieve the Local Data Area.
108640000000      *****
108650000000
108660000000           ACCEPT WS-LOCAL-DATA-AREA FROM  WS-LOCAL.
108670000000
108680990119
108690000000      *****
108700000000      *****    Using the Management Company Code, determine which
108710000000      *****    version of the statement is to be produced.  The standard
108720000000      *****    statement will be zero.
108730000000      *****
108740000000
108750990921           MOVE ZEROES                    TO  WS-STMTVRSN.
108760990921           EVALUATE WS-LDA-STMT-VERSION
108770990921               WHEN 1         MOVE  1     TO WS-STMTVRSN
108780990921               WHEN 2         MOVE  2     TO WS-STMTVRSN
108790990921               WHEN 3         MOVE  3     TO WS-STMTVRSN
108800990921               WHEN 4         MOVE  4     TO WS-STMTVRSN
108810990921               WHEN 5         MOVE  5     TO WS-STMTVRSN
108820990922               WHEN 6         MOVE  6     TO WS-STMTVRSN
108830991019               WHEN 7         MOVE  7     TO WS-STMTVRSN
108840991213               WHEN 8         MOVE  8     TO WS-STMTVRSN
108850990921           END-EVALUATE.
108860000000
108870000000       ILR-0010.
108880000000
108890000000      *****
108900000000      *****    Initialize the program defined work items.
108910000000      *****
108920000000
108930000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT.
108940000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-00.
108950000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-05.
108960000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-10.
108970010308RF8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-12.
108980000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-15.
108990141007141165     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-16.
109000000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-20.
109010060616R35042     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-21.
109020130408117038     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-22.
109030010321RF8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-25.
109040991126           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-30.
109050040406R20870     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-33.
109060991126           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-35.
109070040406R20870     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-38.
109080010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-40.
109090010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-43.
109100010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-45.
109110000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-50.
109120000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-60.
109130991013           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-62.
109140091125R69515     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-63.
109150010323rf8633     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-67.
109160000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-70.
109170000000           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-82.
109180991013           MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-84.
109190010308RF4638     MOVE ZEROES                 TO  WS-STMT-DTA-BCNT-90.
109200000000           MOVE ZEROES                 TO  WS-STMT-DTA-RCNT.
109210000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-01.
109220000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-05.
109230000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-10.
109240000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-20.
109250981207           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-60.
109260000000           MOVE ZEROES                 TO  WS-STMT-DTA-SCNT-80.
109270000000           MOVE ZEROES                 TO  WS-STMT-PRT-DTL-WCNT.
109280000000           MOVE ZEROES                 TO  WS-STMT-PRT-HDR-WCNT.
109290980210
109300000000
10931001121112090 *    MOVE 100                    TO  WS-SAF-MAX.
109320161012      * RFS156599 START
10933016101212090 *    MOVE 500                    TO  WS-SAF-MAX.
109340161108164194*    MOVE 900                    TO  WS-SAF-MAX.
109350161108164194     MOVE 2400                   TO  WS-SAF-MAX.
109360161012      * RFS156599 END
109370000000           MOVE 2                      TO  WS-SC-MAX.
109380000000           MOVE 1000                   TO  WS-SIA-MAX.
109390000000           MOVE 87                     TO  WS-SPL-MAX.
109400000000
109410990921           IF  WS-STMTVRSN    IS EQUAL TO  1
109420000000               MOVE 27                 TO  WS-SPL-DTLFST
109430000000               MOVE 69                 TO  WS-SPL-DTLLST
109440000000               MOVE 9                  TO  WS-SPL-HDGFST
109450000000               MOVE 14                 TO  WS-SPML-MAX
109460990921               GO TO ILR-0020
109470990921           END-IF
109480990921
109490990921           IF  WS-STMTVRSN    IS EQUAL TO  2
109500990921               IF WS-LDA-PRINT-HEADING IS EQUAL TO "N"
109510990921                  MOVE 27                 TO  WS-SPL-DTLFST
109520990921                  MOVE 69                 TO  WS-SPL-DTLLST
109530990921                  MOVE 9                  TO  WS-SPL-HDGFST
109540990921                  MOVE 14                 TO  WS-SPML-MAX
109550990921               ELSE
109560990921                  MOVE 32                 TO  WS-SPL-DTLFST
109570990921                  MOVE 71                 TO  WS-SPL-DTLLST
109580990921                  MOVE 13                 TO  WS-SPL-HDGFST
109590990921                  MOVE 14                 TO  WS-SPML-MAX
109600990921               END-IF
109610990921               GO TO ILR-0020
109620990921           END-IF
109630990726
109640000000           IF  WS-STMTVRSN    IS EQUAL TO  3
109650000000               MOVE 28                 TO  WS-SPL-DTLFST
109660000000               MOVE 70                 TO  WS-SPL-DTLLST
109670000000               MOVE 10                 TO  WS-SPL-HDGFST
109680000000               MOVE 14                 TO  WS-SPML-MAX
109690990921               GO TO ILR-0020
109700990921           END-IF
109710000000
109720000000           IF  WS-STMTVRSN    IS EQUAL TO  4
109730000000               MOVE 28                 TO  WS-SPL-DTLFST
109740000000               MOVE 70                 TO  WS-SPL-DTLLST
109750000000               MOVE 10                 TO  WS-SPL-HDGFST
109760000000               MOVE 12                 TO  WS-SPML-MAX
109770990921               GO TO ILR-0020
109780990921           END-IF
109790000000
109800000000           IF  WS-STMTVRSN    IS EQUAL TO  5
109810000000               MOVE 28                 TO  WS-SPL-DTLFST
109820000000               MOVE 70                 TO  WS-SPL-DTLLST
109830980218               MOVE 9                  TO  WS-SPL-HDGFST
109840000000               MOVE 12                 TO  WS-SPML-MAX
109850990921               GO TO ILR-0020
109860990921           END-IF
109870000000
109880990922           IF  WS-STMTVRSN    IS EQUAL TO  6
109890990927               MOVE 32                 TO  WS-SPL-DTLFST
109900990921               MOVE 70                 TO  WS-SPL-DTLLST
109910990927               MOVE 15                 TO  WS-SPL-HDGFST
109920990921               MOVE 12                 TO  WS-SPML-MAX
109930990921               GO TO ILR-0020
109940990921           END-IF
109950991019
109960991019           IF  WS-STMTVRSN    IS EQUAL TO  7
109970991019               MOVE 27                 TO  WS-SPL-DTLFST
109980991019               MOVE 69                 TO  WS-SPL-DTLLST
109990991019               MOVE 9                  TO  WS-SPL-HDGFST
110000991019               MOVE 14                 TO  WS-SPML-MAX
110010991019               GO TO ILR-0020
110020991019           END-IF
110030991213
110040991213           IF  WS-STMTVRSN    IS EQUAL TO  8
110050991213               MOVE 27                 TO  WS-SPL-DTLFST
110060991213               MOVE 69                 TO  WS-SPL-DTLLST
110070991213               MOVE 9                  TO  WS-SPL-HDGFST
110080991213               MOVE 14                 TO  WS-SPML-MAX
110090991213               GO TO ILR-0020
110100991213           END-IF
110110991213
110120000000           MOVE 27                     TO  WS-SPL-DTLFST.
110130000000           MOVE 68                     TO  WS-SPL-DTLLST.
110140000000           MOVE 9                      TO  WS-SPL-HDGFST.
110150000000           MOVE 14                     TO  WS-SPML-MAX.
110160000000
110170000000       ILR-0020.
110180990921
110190000000           MOVE 99                     TO  WS-LNECNT.
110200000000           MOVE 60                     TO  WS-LNEMAX.
110210000000           MOVE ZEROES                 TO  WS-PGECNT.
110220000000
110230000000           MOVE "N"                    TO  WS-EOJHDG-SW.
110240000000
110250000000           MOVE LOW-VALUES             TO  CURR-CONTROLS.
110260000000           MOVE LOW-VALUES             TO  PREV-CONTROLS.
110270000000
110280000000      *****
110290000000      *****    Format the heading literals.
110300000000      *****
110310000104           MOVE   WS-LDA-AS-AT-DATE  TO        WS-DATE.
110320000104           MOVE   WS-LDA-AS-AT-DATE  TO        WS-SYS-DATE.
110330990120
110340000104           MOVE WS-DATE-YYYY           TO  HL1-YY.
110350000000           MOVE WS-DATE-MM             TO  HL1-MM.
110360000000           MOVE WS-DATE-DD             TO  HL1-DD.
110370000000
110380000000           ACCEPT HL1-TIME           FROM  TIME.
110390000000           INSPECT HL1-TIME REPLACING ALL  SPACE
110400000000                                       BY  ":".
110410000000           MOVE "."                    TO  HL1-TIME-DOT.
110420150515      *RFS 147149 - BEGINS.
110430150515980417*    IF  WS-LDA-STMT-VERSION  IS EQUAL TO  "4"
110440150515      *        MOVE SPACES             TO  WS-SS02-LIT4-E
110450150515      *        MOVE SPACES             TO  WS-SS02-LIT4-F
110460150515      *        MOVE SPACES             TO  WS-SS03-LIT4-E
110470150515      *        MOVE SPACES             TO  WS-SS03-LIT4-F
110480150515      *    END-IF
110490150515      *RFS 147149 - ENDS.
110500000000      *****
110510000000      *****    Open the Input files.
110520000000      *****
110530000000
110540000000           OPEN    INPUT   DEALER-MAILING-INSTR
110550000000                           DEALER-FILE
110560000000                           DEALER-REP-FILE
110570000000                           DEALER-REP-MAILING-INSTR
110580000000                           DEALER-REP-ADDRESS
110590000000                           INSTITUTION-ADDRESS
110600000000                           INSTITUTION-BRANCH
110610980326                           INVESTOR-JOIN
110620991103991103                     ACCOUNT-TYPE-BK
110630980326                           NOMINEE-ACCOUNT-TYPE-DESCR
110640000000                           NOMINEE-MAILING-INSTRUCTIONS
110650020920R14588                     DEALER-MISC-DETAILS
110660020920R14588                     MASTER-ACCOUNT
110670000000                           STMT-MSG-FILE
110680990116                           STMT-DTA-FILE
110690190612      *RFS183449-Starts
110700190612      *                    ACCOUNT-FILE.
110710190612                           ACCOUNT-FILE
110720190612                           ACCOUNT-FILE-2.
110730190612      *RFS183449-Ends
110740000000      *****
110750000000      *****    Open the Output files.
110760000000      *****
110770000000
110780000000           OPEN    OUTPUT  STMT-PRT-DTL-FILE
110790000000                           STMT-PRT-HDR-FILE
110800000000                           PRINT-FILE.
110810980210
110820991103       ILR-0040.
110830991103
110840991103      *****
110850991103      *****    Load the account type entries.
110860991103      *****
110870991103
110880991103           MOVE SPACES                 TO  WS-ACCTYP-TBL.
110890991103           MOVE ZEROES                 TO  WS-AT-LST.
110900991103
110910991103       ILR-0050.
110920991103
110930991103           READ ACCOUNT-TYPE-BK        AT  END
110940991103               GO TO ILR-0070
110950991103           END-READ
110960991103
110970991103           IF  WS-AT-LST IS NOT LESS THAN  WS-AT-MAX
110980991103               DISPLAY "ACCOUNT TYPES (MFAACTYP) EXCEED PROGRAM MAX"
110990991103               GO TO ILR-EXIT
111000991103           END-IF
111010991103
111020991103           ADD 1                       TO  WS-AT-LST.
111030991103           SET AT1                     TO  WS-AT-LST.
111040991103           MOVE ACCOUNT-TYPE-CODE
111050991103                OF ACCOUNT-TYPE-BK-REC TO  WS-AT-CODE (AT1).
111060991103           MOVE ACCOUNT-TYPE-RULE
111070991103                OF ACCOUNT-TYPE-BK-REC TO  WS-AT-RULE (AT1).
111080991103           GO TO ILR-0050.
111090991103
111100991103       ILR-0070.
111110991103
111120991103           IF  WS-AT-LST NOT GREATER THAN  ZEROES
111130991103               DISPLAY "NO ACCOUNT TYPES (MFAACTYP) FOUND"
111140991103           END-IF.
111150991103
111160991103      **********
111170000000
111180000000       ILR-EXIT.
111190000000           EXIT.
111200991103
111210991103      **********
111220000000      /
111230000000       END-JOB-RTN.
111240000000
111250000000      ******************************************************************
111260000000      *                                                                *
111270000000      *    E N D   J O B   R O U T I N E                               *
111280000000      *                                                                *
111290000000      ******************************************************************
111300000000
111310000000      *****
111320000000      *****    Print the Control Report on a separate page.
111330000000      *****
111340000000
111350000000           MOVE 99                     TO  WS-LNECNT.
111360000000           MOVE "N"                    TO  WS-BYPPRT-SW.
111370000000           MOVE "Y"                    TO  WS-EOJHDG-SW.
111380000000           MOVE "N"                    TO  WS-SELPRT-SW.
111390000000
111400000000           MOVE WS-MANAGEMENT-COMPANY-CODE
111410000000                                       TO  CL2-MANAGEMENT-COMPANY-CODE.
111420000000           MOVE CONTROL-LINE-2         TO  CONTROL-LINE-1.
111430000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
111440000000
111450000000           MOVE SPACES                 TO  CONTROL-LINE-1.
111460000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
111470000000
111480000000           MOVE SPACES                 TO  CONTROL-LINE-1.
111490000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
111500000000
111510000000           MOVE SPACES                 TO  CONTROL-LINE-1.
111520000000           MOVE WS-STMT-DTA-RLIT       TO  CL1-LIT.
111530000000           MOVE WS-STMT-DTA-RCNT       TO  CL1-CNT.
111540000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
111550000000
111560000000           MOVE SPACES                 TO  CONTROL-LINE-1.
111570000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
111580000000
111590000000           IF  WS-STMT-DTA-BCNT
111600000000                          IS GREATER THAN  ZEROES
111610000000               MOVE SPACES             TO  CONTROL-LINE-1
111620000000               MOVE WS-STMT-DTA-BLIT   TO  CL1-LIT
111630000000               MOVE WS-STMT-DTA-BCNT   TO  CL1-CNT
111640000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
111650000000               MOVE "Y"                TO  WS-BYPPRT-SW.
111660000000
111670000000           IF  WS-STMT-DTA-BCNT-00
111680000000                          IS GREATER THAN  ZEROES
111690000000               MOVE SPACES             TO  CONTROL-LINE-1
111700000000               MOVE WS-STMT-DTA-BLIT-00
111710000000                                       TO  CL1-LIT
111720000000               MOVE WS-STMT-DTA-BCNT-00
111730000000                                       TO  CL1-CNT
111740000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
111750000000               MOVE "Y"                TO  WS-BYPPRT-SW.
111760000000
111770000000           IF  WS-STMT-DTA-BCNT-05
111780000000                          IS GREATER THAN  ZEROES
111790000000               MOVE SPACES             TO  CONTROL-LINE-1
111800000000               MOVE WS-STMT-DTA-BLIT-05
111810000000                                       TO  CL1-LIT
111820000000               MOVE WS-STMT-DTA-BCNT-05
111830000000                                       TO  CL1-CNT
111840000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
111850000000               MOVE "Y"                TO  WS-BYPPRT-SW.
111860000000
111870000000           IF  WS-STMT-DTA-BCNT-10
111880000000                          IS GREATER THAN  ZEROES
111890000000               MOVE SPACES             TO  CONTROL-LINE-1
111900000000               MOVE WS-STMT-DTA-BLIT-10
111910000000                                       TO  CL1-LIT
111920000000               MOVE WS-STMT-DTA-BCNT-10
111930000000                                       TO  CL1-CNT
111940000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
111950000000               MOVE "Y"                TO  WS-BYPPRT-SW.
111960000000
111970010308RF8633     IF  WS-STMT-DTA-BCNT-12
111980010308                          IS GREATER THAN  ZEROES
111990010308               MOVE SPACES             TO  CONTROL-LINE-1
112000010308               MOVE WS-STMT-DTA-BLIT-12
112010010308                                       TO  CL1-LIT
112020010308               MOVE WS-STMT-DTA-BCNT-12
112030010308                                       TO  CL1-CNT
112040010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112050010308RF8633         MOVE "Y"                TO  WS-BYPPRT-SW.
112060010308
112070000000           IF  WS-STMT-DTA-BCNT-15
112080000000                          IS GREATER THAN  ZEROES
112090000000               MOVE SPACES             TO  CONTROL-LINE-1
112100000000               MOVE WS-STMT-DTA-BLIT-15
112110000000                                       TO  CL1-LIT
112120000000               MOVE WS-STMT-DTA-BCNT-15
112130000000                                       TO  CL1-CNT
112140000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112150000000               MOVE "Y"                TO  WS-BYPPRT-SW.
112160000000
112170000000           IF  WS-STMT-DTA-BCNT-20
112180000000                          IS GREATER THAN  ZEROES
112190000000               MOVE SPACES             TO  CONTROL-LINE-1
112200000000               MOVE WS-STMT-DTA-BLIT-20
112210000000                                       TO  CL1-LIT
112220000000               MOVE WS-STMT-DTA-BCNT-20
112230000000                                       TO  CL1-CNT
112240000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112250000000               MOVE "Y"                TO  WS-BYPPRT-SW.
112260000000
112270060616R35042     IF  WS-STMT-DTA-BCNT-21
112280060616  ¦                       IS GREATER THAN  ZEROES
112290060616  ¦            MOVE SPACES             TO  CONTROL-LINE-1
112300060616  ¦            MOVE WS-STMT-DTA-BLIT-21
112310060616  ¦                                    TO  CL1-LIT
112320060616  ¦            MOVE WS-STMT-DTA-BCNT-21
112330060616  ¦                                    TO  CL1-CNT
112340060616  V            PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112350060616R35042         MOVE "Y"                TO  WS-BYPPRT-SW.
112360060616
112370130408117038     IF  WS-STMT-DTA-BCNT-22
112380130408  ¦                       IS GREATER THAN  ZEROES
112390130408  ¦            MOVE SPACES             TO  CONTROL-LINE-1
112400130408  ¦            MOVE WS-STMT-DTA-BLIT-22
112410130408  ¦                                    TO  CL1-LIT
112420130408  ¦            MOVE WS-STMT-DTA-BCNT-22
112430130408  ¦                                    TO  CL1-CNT
112440130408  V            PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112450130408117038         MOVE "Y"                TO  WS-BYPPRT-SW.
112460130408
112470010321RF8633     IF  WS-STMT-DTA-BCNT-25
112480010321                          IS GREATER THAN  ZEROES
112490010321               MOVE SPACES             TO  CONTROL-LINE-1
112500010321               MOVE WS-STMT-DTA-BLIT-25
112510010321                                       TO  CL1-LIT
112520010321               MOVE WS-STMT-DTA-BCNT-25
112530010321                                       TO  CL1-CNT
112540010321               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112550010321RF8633         MOVE "Y"                TO  WS-BYPPRT-SW.
112560010321
112570991126991126     IF  WS-STMT-DTA-BCNT-30
112580991126                          IS GREATER THAN  ZEROES
112590991126               MOVE SPACES             TO  CONTROL-LINE-1
112600991126               MOVE WS-STMT-DTA-BLIT-30
112610991126                                       TO  CL1-LIT
112620991126               MOVE WS-STMT-DTA-BCNT-30
112630991126                                       TO  CL1-CNT
112640991126               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112650991126               MOVE "Y"                TO  WS-BYPPRT-SW.
112660991126
112670040406R20870     IF  WS-STMT-DTA-BCNT-33
112680040406                          IS GREATER THAN  ZEROES
112690040406               MOVE SPACES             TO  CONTROL-LINE-1
112700040406               MOVE WS-STMT-DTA-BLIT-33
112710040406                                       TO  CL1-LIT
112720040406               MOVE WS-STMT-DTA-BCNT-33
112730040406                                       TO  CL1-CNT
112740040406               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112750040406               MOVE "Y"                TO  WS-BYPPRT-SW.
112760040406
112770991126991126     IF  WS-STMT-DTA-BCNT-35
112780991126                          IS GREATER THAN  ZEROES
112790991126               MOVE SPACES             TO  CONTROL-LINE-1
112800991126               MOVE WS-STMT-DTA-BLIT-35
112810991126                                       TO  CL1-LIT
112820991126               MOVE WS-STMT-DTA-BCNT-35
112830991126                                       TO  CL1-CNT
112840991126               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112850991126               MOVE "Y"                TO  WS-BYPPRT-SW.
112860991126
112870040406R20870     IF  WS-STMT-DTA-BCNT-38
112880040406                          IS GREATER THAN  ZEROES
112890040406               MOVE SPACES             TO  CONTROL-LINE-1
112900040406               MOVE WS-STMT-DTA-BLIT-38
112910040406                                       TO  CL1-LIT
112920040406               MOVE WS-STMT-DTA-BCNT-38
112930040406                                       TO  CL1-CNT
112940040406               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
112950040406               MOVE "Y"                TO  WS-BYPPRT-SW.
112960040406
112970010308RF4638     IF  WS-STMT-DTA-BCNT-40
112980010308                          IS GREATER THAN  ZEROES
112990010308               MOVE SPACES             TO  CONTROL-LINE-1
113000010308               MOVE WS-STMT-DTA-BLIT-40
113010010308                                       TO  CL1-LIT
113020010308               MOVE WS-STMT-DTA-BCNT-40
113030010308                                       TO  CL1-CNT
113040010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113050010308               MOVE "Y"                TO  WS-BYPPRT-SW.
113060010308
113070010308RF4638     IF  WS-STMT-DTA-BCNT-43
113080010308                          IS GREATER THAN  ZEROES
113090010308               MOVE SPACES             TO  CONTROL-LINE-1
113100010308               MOVE WS-STMT-DTA-BLIT-43
113110010308                                       TO  CL1-LIT
113120010308               MOVE WS-STMT-DTA-BCNT-43
113130010308                                       TO  CL1-CNT
113140010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113150010308               MOVE "Y"                TO  WS-BYPPRT-SW.
113160010308
113170010308RF4638     IF  WS-STMT-DTA-BCNT-45
113180010308                          IS GREATER THAN  ZEROES
113190010308               MOVE SPACES             TO  CONTROL-LINE-1
113200010308               MOVE WS-STMT-DTA-BLIT-45
113210010308                                       TO  CL1-LIT
113220010308               MOVE WS-STMT-DTA-BCNT-45
113230010308                                       TO  CL1-CNT
113240010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113250010308               MOVE "Y"                TO  WS-BYPPRT-SW.
113260010308
113270000000           IF  WS-STMT-DTA-BCNT-50
113280000000                          IS GREATER THAN  ZEROES
113290000000               MOVE SPACES             TO  CONTROL-LINE-1
113300000000               MOVE WS-STMT-DTA-BLIT-50
113310000000                                       TO  CL1-LIT
113320000000               MOVE WS-STMT-DTA-BCNT-50
113330000000                                       TO  CL1-CNT
113340000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113350000000               MOVE "Y"                TO  WS-BYPPRT-SW.
113360000000
113370000000           IF  WS-STMT-DTA-BCNT-60
113380000000                          IS GREATER THAN  ZEROES
113390000000               MOVE SPACES             TO  CONTROL-LINE-1
113400000000               MOVE WS-STMT-DTA-BLIT-60
113410000000                                       TO  CL1-LIT
113420000000               MOVE WS-STMT-DTA-BCNT-60
113430000000                                       TO  CL1-CNT
113440000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113450000000               MOVE "Y"                TO  WS-BYPPRT-SW.
113460000000
113470991013           IF  WS-STMT-DTA-BCNT-62
113480991013                          IS GREATER THAN  ZEROES
113490991013               MOVE SPACES             TO  CONTROL-LINE-1
113500991013               MOVE WS-STMT-DTA-BLIT-62
113510991013                                       TO  CL1-LIT
113520991013               MOVE WS-STMT-DTA-BCNT-62
113530991013                                       TO  CL1-CNT
113540991013               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113550991013               MOVE "Y"                TO  WS-BYPPRT-SW.
113560091125      * RFS69515 Begin
113570091125           IF  WS-STMT-DTA-BCNT-63
113580091125                          IS GREATER THAN  ZEROES
113590091125               MOVE SPACES             TO  CONTROL-LINE-1
113600091125               MOVE WS-STMT-DTA-BLIT-63
113610091125                                       TO  CL1-LIT
113620091125               MOVE WS-STMT-DTA-BCNT-63
113630091125                                       TO  CL1-CNT
113640091125               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113650091125               MOVE "Y"                TO  WS-BYPPRT-SW.
113660091125      * RFS69515 End
113670091125
113680010323rf8633     IF  WS-STMT-DTA-BCNT-67
113690010323                          IS GREATER THAN  ZEROES
113700010323               MOVE SPACES             TO  CONTROL-LINE-1
113710010323               MOVE WS-STMT-DTA-BLIT-67
113720010323                                       TO  CL1-LIT
113730010323               MOVE WS-STMT-DTA-BCNT-67
113740010323                                       TO  CL1-CNT
113750010323               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113760010323               MOVE "Y"                TO  WS-BYPPRT-SW.
113770010323
113780000000           IF  WS-STMT-DTA-BCNT-70
113790000000                          IS GREATER THAN  ZEROES
113800000000               MOVE SPACES             TO  CONTROL-LINE-1
113810000000               MOVE WS-STMT-DTA-BLIT-70
113820000000                                       TO  CL1-LIT
113830000000               MOVE WS-STMT-DTA-BCNT-70
113840000000                                       TO  CL1-CNT
113850000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113860000000               MOVE "Y"                TO  WS-BYPPRT-SW.
113870000000
113880000000           IF  WS-STMT-DTA-BCNT-82
113890000000                          IS GREATER THAN  ZEROES
113900000000               MOVE SPACES             TO  CONTROL-LINE-1
113910000000               MOVE WS-STMT-DTA-BLIT-82
113920000000                                       TO  CL1-LIT
113930000000               MOVE WS-STMT-DTA-BCNT-82
113940000000                                       TO  CL1-CNT
113950000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
113960000000               MOVE "Y"                TO  WS-BYPPRT-SW.
113970000000
113980991013           IF  WS-STMT-DTA-BCNT-84
113990991013                          IS GREATER THAN  ZEROES
114000991013               MOVE SPACES             TO  CONTROL-LINE-1
114010991013               MOVE WS-STMT-DTA-BLIT-84
114020991013                                       TO  CL1-LIT
114030991013               MOVE WS-STMT-DTA-BCNT-84
114040991013                                       TO  CL1-CNT
114050991013               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114060991013               MOVE "Y"                TO  WS-BYPPRT-SW.
114070991013
114080010308RF4638     IF  WS-STMT-DTA-BCNT-90
114090010308                          IS GREATER THAN  ZEROES
114100010308               MOVE SPACES             TO  CONTROL-LINE-1
114110010308               MOVE WS-STMT-DTA-BLIT-90
114120010308                                       TO  CL1-LIT
114130010308               MOVE WS-STMT-DTA-BCNT-90
114140010308                                       TO  CL1-CNT
114150010308               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114160010308               MOVE "Y"                TO  WS-BYPPRT-SW.
114170010308
114180000000           IF  WS-BYPPRT-SW   IS EQUAL TO  "Y"
114190000000               MOVE SPACES             TO  CONTROL-LINE-1
114200000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
114210000000
114220000000           IF  WS-STMT-DTA-SCNT-01
114230000000                          IS GREATER THAN  ZEROES
114240000000               MOVE SPACES                 TO  CONTROL-LINE-1
114250000000               MOVE WS-STMT-DTA-SLIT-01
114260000000                                       TO  CL1-LIT
114270000000               MOVE WS-STMT-DTA-SCNT-01
114280000000                                       TO  CL1-CNT
114290000000               PERFORM PRINT-CTL-LINE     THRU PCL-EXIT
114300000000               MOVE "Y"                TO  WS-SELPRT-SW.
114310000000
114320000000           IF  WS-STMT-DTA-SCNT-05
114330000000                          IS GREATER THAN  ZEROES
114340000000               MOVE SPACES             TO  CONTROL-LINE-1
114350000000               MOVE WS-STMT-DTA-SLIT-05
114360000000                                       TO  CL1-LIT
114370000000               MOVE WS-STMT-DTA-SCNT-05
114380000000                                       TO  CL1-CNT
114390000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114400000000               MOVE "Y"                TO  WS-SELPRT-SW.
114410000000
114420000000           IF  WS-STMT-DTA-SCNT-10
114430000000                          IS GREATER THAN  ZEROES
114440000000               MOVE SPACES             TO  CONTROL-LINE-1
114450000000               MOVE WS-STMT-DTA-SLIT-10
114460000000                                       TO  CL1-LIT
114470000000               MOVE WS-STMT-DTA-SCNT-10
114480000000                                       TO  CL1-CNT
114490000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114500000000               MOVE "Y"                TO  WS-SELPRT-SW.
114510000000
114520000000           IF  WS-STMT-DTA-SCNT-20
114530000000                          IS GREATER THAN  ZEROES
114540000000               MOVE SPACES             TO  CONTROL-LINE-1
114550000000               MOVE WS-STMT-DTA-SLIT-20
114560000000                                       TO  CL1-LIT
114570000000               MOVE WS-STMT-DTA-SCNT-20
114580000000                                       TO  CL1-CNT
114590000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114600000000               MOVE "Y"                TO  WS-SELPRT-SW.
114610000000
114620981207           IF  WS-STMT-DTA-SCNT-60
114630981207                          IS GREATER THAN  ZEROES
114640981207               MOVE SPACES             TO  CONTROL-LINE-1
114650981207               MOVE WS-STMT-DTA-SLIT-60
114660981207                                       TO  CL1-LIT
114670981207               MOVE WS-STMT-DTA-SCNT-60
114680981207                                       TO  CL1-CNT
114690981207               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114700981207               MOVE "Y"                TO  WS-SELPRT-SW.
114710981207
114720000000           IF  WS-STMT-DTA-SCNT-80
114730000000                          IS GREATER THAN  ZEROES
114740000000               MOVE SPACES             TO  CONTROL-LINE-1
114750000000               MOVE WS-STMT-DTA-SLIT-80
114760000000                                       TO  CL1-LIT
114770000000               MOVE WS-STMT-DTA-SCNT-80
114780000000                                       TO  CL1-CNT
114790000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT
114800000000               MOVE "Y"                TO  WS-SELPRT-SW.
114810000000
114820000000           IF  WS-SELPRT-SW   IS EQUAL TO  "Y"
114830000000               MOVE SPACES             TO  CONTROL-LINE-1
114840000000               PERFORM PRINT-CTL-LINE THRU PCL-EXIT.
114850000000
114860000000           MOVE SPACES                 TO  CONTROL-LINE-1.
114870000000           MOVE WS-STMT-PRT-HDR-WLIT   TO  CL1-LIT.
114880000000           MOVE WS-STMT-PRT-HDR-WCNT   TO  CL1-CNT.
114890000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
114900000000
114910000000           MOVE SPACES                 TO  CONTROL-LINE-1.
114920000000           MOVE WS-STMT-PRT-DTL-WLIT   TO  CL1-LIT.
114930000000           MOVE WS-STMT-PRT-DTL-WCNT   TO  CL1-CNT.
114940000000           PERFORM PRINT-CTL-LINE     THRU PCL-EXIT.
114950000000
114960000000      *****
114970000000      *****    Close all the files.
114980000000      *****
114990000000
115000000000           CLOSE           DEALER-MAILING-INSTR
115010000000                           DEALER-FILE
115020000000                           DEALER-REP-FILE
115030000000                           DEALER-REP-MAILING-INSTR
115040000000                           DEALER-REP-ADDRESS
115050000000                           INSTITUTION-ADDRESS
115060000000                           INSTITUTION-BRANCH
115070980326                           INVESTOR-JOIN
115080980326                           NOMINEE-ACCOUNT-TYPE-DESCR
115090000000                           NOMINEE-MAILING-INSTRUCTIONS
115100020920R14588                     DEALER-MISC-DETAILS
115110020920R14588                     MASTER-ACCOUNT
115120000000                           STMT-MSG-FILE
115130000000                           STMT-PRT-DTL-FILE
115140000000                           STMT-PRT-HDR-FILE
115150991103991103                     ACCOUNT-TYPE-BK
115160000000                           STMT-DTA-FILE
115170990116                           PRINT-FILE
115180190612      *RFS183449-Starts
115190190612      *                    ACCOUNT-FILE.
115200190612                           ACCOUNT-FILE
115210190612                           ACCOUNT-FILE-2.
115220190612      *RFS183449-Ends
115230000000
115240000000       EJR-EXIT.
115250000000           EXIT.
115260000000      /
115270000000       PRINT-CTL-LINE.
115280000000
115290000000      ******************************************************************
115300000000      *                                                                *
115310000000      *    P R I N T   C O N T R O L   L I N E   R O U T I N E         *
115320000000      *                                                                *
115330000000      ******************************************************************
115340000000
115350000000      *****
115360000000      *****    If the page is full, start a new page and print the
115370000000      *****    Heading lines.
115380000000      *****
115390000000
115400000000           IF  WS-LNECNT  IS GREATER THAN  WS-LNEMAX
115410000000               PERFORM PRINT-HDG-LINE THRU PHL-EXIT.
115420000000
115430000000      *****
115440000000      *****    Print the Control line.
115450000000      *****
115460000000
115470000000           WRITE PRINT-RECD          FROM  CONTROL-LINE-1
115480000000                         AFTER  ADVANCING  1.
115490000000           ADD 1                       TO  WS-LNECNT.
115500000000
115510000000       PCL-EXIT.
115520000000           EXIT.
115530000000      /
115540000000       PRINT-HDG-LINE.
115550000000
115560000000      ******************************************************************
115570000000      *                                                                *
115580000000      *    P R I N T   H E A D I N G   L I N E   R O U T I N E         *
115590000000      *                                                                *
115600000000      ******************************************************************
115610000000
115620000000      *****
115630000000      *****    Print Heading line 1 on a new page.
115640000000      *****
115650000000
115660000000           WRITE PRINT-RECD          FROM  HEADING-LINE-1
115670000000                          AFTER ADVANCING  PAGE.
115680000000           ADD 1                       TO  WS-PGECNT.
115690000000           MOVE 1                      TO  WS-LNECNT.
115700000000
115710000000      *****
115720000000      *****    Format and print Heading line 2.
115730000000      *****
115740000000
115750000000           MOVE WS-PGECNT              TO  HL2-PGECNT.
115760000000
115770000000           WRITE PRINT-RECD          FROM  HEADING-LINE-2
115780000000                          AFTER ADVANCING  1.
115790000000           ADD 1                       TO  WS-LNECNT.
115800000000
115810000000      *****
115820000000      *****    Print Heading line 3, if required.
115830000000      *****
115840000000
115850000000           IF  WS-EOJHDG-SW   IS EQUAL TO  "Y"
115860000000               GO TO PHL-0010.
115870000000
115880000000           WRITE PRINT-RECD          FROM  HEADING-LINE-3
115890000000                          AFTER ADVANCING  2.
115900000000           ADD 2                       TO  WS-LNECNT.
115910000000
115920000000       PHL-0010.
115930000000
115940000000      *****
115950000000      *****    Format and print a blank line.
115960000000      *****
115970000000
115980000000           MOVE SPACES                 TO  PRINT-RECD.
115990000000           WRITE PRINT-RECD
116000000000                          AFTER ADVANCING  1.
116010000000           ADD 1                       TO  WS-LNECNT.
116020000000
116030000000       PHL-EXIT.
116040000000           EXIT.
116050990116      /
116060990116       GET-DEALER-ACCOUNT.
116070990116           INITIALIZE MFAACCNTP OF ACCOUNT-REC.
116080990116           MOVE CURR-ACCOUNT
116090020611                    TO  ACCOUNT-NO OF ACCOUNT-REC
116100990116
116110990116           READ ACCOUNT-FILE
116120990116               INVALID KEY
116130990116                DISPLAY "INVALID DEALER ACCOUNT NO"
116140990116                GO TO GDA-EXIT.
116150990116
116160990116           MOVE DEALER-ACCOUNT-NO OF ACCOUNT-REC
116170990116                TO IS-DEALER-ACCOUNT-NO.
116180990116      /
116190990116       GDA-EXIT.
116200990116           EXIT.
116210190612      *RFS-183449-Starts
116220190612       GET-ACCOUNT-TYPE-CODE.
116230190612           INITIALIZE MFAACCNTP OF ACCOUNT-REC-2.
116240190612           MOVE AD-ACCOUNT
116250190612                    TO  ACCOUNT-NO OF ACCOUNT-REC-2
116260190612
116270190612           READ ACCOUNT-FILE-2
116280190619             INVALID KEY
116290190619             DISPLAY "Account File 2 not found for Account:" AD-ACCOUNT
116300190619             GO TO GAT-EXIT.
116310190612
116320190612           MOVE ACCOUNT-TYPE-CODE OF ACCOUNT-REC-2
116330190612                TO Lc-Acct-Type-Code.
116340190619       GAT-EXIT.
116350190612           EXIT.
116360190612      *RFS183449-Ends
