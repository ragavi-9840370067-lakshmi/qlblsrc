000100190110      ******************************************************************
000200181101      *                                                                *
000300160229      *    CPYJSISTMT: Copy Book - JSI Statement Record Layouts        *
000400190328      *    CREATED:    DD/MMM/YY (III)                                 *
000500160229      *    CHANGED:    26/JUN/98 (MFH)                                 *
000600160229      *    LENGTH:     750                                             *
000700160229      *                                                                *
000800160229      ******************************************************************
000900200303
001000210823      ******************************************************************
001100160229      *                                                                *
001200210823      *    RECORDS:    (PR)      MFASTMTFP SECTION                     *
001300160229      *                (DI)      DELIVERY INFO SECTION                 *
001400160229      *                (II)      INVESTOR INFO SECTION                 *
001500181012      *                (DL)      DEALER SECTION                        *
001600160229      *                (IS)      INVESTOR SUMMARY SECTION              *
001700160229      *                (AC)      ACCOUNT ASSET CLASS SECTION           *
001800160229      *                (AD)      ACCOUNT DETAILS SECTION               *
001900160229      *                (AP)      ACCOUNT PERFORMANCE SECTION           *
002000160229      *                (AFP)     ACCOUNT FUND PERFORMANCE SECTION      *
002100160229      *                (AR)      ROLLOVER SECTION                      *
002200160229      *                (AW)      RRIF DETAIL SECTION                   *
002300160229      *                (RE)      RESP DETAIL SECTION
002400160229      *                (GR)      GROUP INFORMATION SECTION             *
002500160229      *                (SP)      SYSTEMATIC PLAN INFO SECTION          *
002600160229      *                (AT)      ACCOUNT TXN SECTION                   *
002700160229      *                (DT)      TRANS-DED-SECTION
002800160229      *                (TI)      AT INSTITUTION DETAIL SECTION         *
002900160229      *                                                                *
003000160229      ******************************************************************
003100160229
003200160229      ******************************************************************
003300160229      *                                                                *
003400160229      *    C H A N G E   H I S T O R Y                                 *
003500160229      *                                                                *
003600160229      ******************************************************************
003700160229      ******************************************************************
003800160229      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE              *
003900160229      ******************************************************************
004000160229      *            *              *                                    *
004100160229      * H. Sheng   *  2000/02/22  * RFS 6532 - Change statement extract*
004200160229      *            *              * and print programs to support up to*
004300160229      *            *              * a $99,999.99 and 99B market value  *
004400160229      *            *              * and book value. Increase dealer rep*
004500160229      *            *              * length from 5 to 6.
004600160229      ******************************************************************
004700160229      * B.LU       *  2000/04/17  * - A new sections were added        *
004800160229      *            *              *  to the standard statement extract *
004900160229      *            *              *  file: Section 67.                 *
005000160229      ******************************************************************
005100160229      * B.LU       *  2000/05/15  * RFS 7051 - Added the following     *
005200160229      *            *              * field to the Delivery Information. *
005300160229      *            *              *     DI-RECORD-COUNT-30             *
005400160229      *            *              *     DI-RECORD-COUNT-35             *
005500160229      *            *              *     DI-RECORD-COUNT-67             *
005600160229      *            *              *     DI-RECORD-COUNT-84.            *
005700160229      ******************************************************************
005800160229      * B.LU       *  2000/05/29  * RFS 6645 - Ten new fields were     *
005900160229      *            *              *  added to each of Section 30 and   *
006000160229      *            *              *  Section 35:                       *
006100160229      *            *              *  AP-PERIOD-ROR-APPLICABLE-(1-10)   *
006200160229      *            *              *  AFP-PERIOD-ROR-APPLICABLE-(1-10)  *
006300160229      ******************************************************************
006400160229      * B.LU       *  2000/06/16  * RFS 7172, 6725 - Add Nominee       *
006500160229      *            *              *  address in Section 5, one fields  *
006600160229      *            *              *  in Section 10, and two fields     *
006700160229      *            *              *  in Section 80.                    *
006800160229      ******************************************************************
006900160229      * B.LU       *  2000/08/14  * RFS 7503 6697                      *
007000160229      *            *  2000/09/11  * - Increased the record size from   *
007100160229      *            *              *   750 to 800 characters for both   *
007200160229      *            *              *   DDS and CPYJSISTMT.              *
007300160229      *            *              * - Added five fields to Section 20. *
007400160229      *            *              * - A new section: Section 25 was    *
007500160229      *            *              *   created in Extract file.         *
007600160229      *            *              * - A new parameter was added to     *
007700160229      *            *              *   Statement table.                 *
007800160229      *            *              * - Added two fields to section 80.  *
007900160229      ******************************************************************
008000160229      * B.LU       *  2000/10/03  * RFS 7703 7633 7776 7777 7778       *
008100160229      *            *  2000/10/12  * - Increased the record size from   *
008200160229      *            *              *   800 to 850 characters for both   *
008300160229      *            *              *   DDS and CPYJSISTMT.              *
008400160229      *            *              * - Added one field to section 01.   *
008500160229      *            *              * - Added DL-BRANCH-CODE to          *
008600160229      *            *              *   Section 05.                      *
008700160229      *            *              * - Added two fields to section 10.  *
008800160229      *            *              * - Added three fields to Section 20,*
008900160229      *            *              *   AD-BOOK-VALUE renamed to AD-BOOK *
009000160229      *            *              *   -VALUE-BASE, Switches were not   *
009100160229      *            *              *   included in AD-TRI-INCEPT, AD-TRO*
009200160229      *            *              *   -INCEPT.                         *
009300160229      *            *              * - Added six fields to section 67.  *
009400160229      ******************************************************************
009500160229      * B.LU       *  2000/10/12  * RFS 4638                           *
009600190206      *            *  2000/10/19  * - Added Section 40.                *
009700160229      *            *              * - Added Section 43.                *
009800160229      *            *              * - Added Section 45.                *
009900160229      *            *              * - Added Section 90.                *
010000160229      *            *              * - Added BE-Primary-Contingent to   *
010100160229      *            *              *   Section 67.                      *
010200160229      *            *              * - AT-DEDUCTION, AT-DEDUCTION-SIGN  *
010300160229      *            *              *   were renamed to AT-DSC,          *
010400160229      *            *              *   AT-DSC-SIGN.                     *
010500160229      *            *              * - Added six new fields to          *
010600160229      *            *              *   Section 80.                      *
010700160229      *            *              * - Added AW-PAYMENT-OPTION to       *
010800160229      *            *              *   Section 60.                      *
010900160229      ******************************************************************
011000160229      * K.LIU      *  2001/02/27  * RFS 8633
011100160229      *            *              * - Increased the record size from
011200160229      *            *              *   850 to 1100 characters for both
011300160229      *            *              *   DDS and CPYJSISTMT.
011400160229      *            *              * - Added Section 12.
011500160229      *            *              * - Added eighteen new fields to
011600160229      *            *              *   Section 10.
011700160229      ******************************************************************
011800160229      * B.LU       *  2001/02/27  * RFS 9331
011900160229      *            *              * -ADDED RE-RESP-TYPE TO
012000160229      *            *              *  SECTION 62.
012100160229      *            *              * - Added TRAILER-INFO-SECTION.
012200160229      ******************************************************************
012300160229      * B.LU       *  2001/05/23  * RFS 9909
012400160229      *            *              * -Added RE-PLAN-SETUP-DATE to
012500160229      *            *              *  Section 62.
012600160229      ******************************************************************
012700160229      * B.LU       *  2001/06/01  * RFS 10243 9415
012800160229      *            *              * -Added CD-FIRST-TRADE-DATE to
012900160229      *            *              *  Section 40.
013000160229      ******************************************************************
013100160229      * B.LU       *  2001/06/20  * RFS 9850
013200160229      *            *              * -Added IS-INVESTMENT-STRUCTURE-CODE
013300160229      *            *              *  TO SECTION 10.
013400160229      ******************************************************************
013500160229      * K.LIU      *  2001/07/04  * RFS 10362
013600160229      *            *              * -Added AW-PAY-FREQ-DESCR and AW-PAY
013700160229      *            *              *  -OPTION-DESCR to Section 60.
013800160229      ******************************************************************
013900160229      * K.LIU      *  2001/08/08  * RFS 10605
014000160229      *            *              * -Added IS-DATE-OF-BIRTH to Section
014100160229      *            *              *  10.
014200160229      *            *              * -Added AT-REP-NAME to Section 80
014300160229      ******************************************************************
014400160229      * B.LU       *  2001/09/26  * RFS11127
014500160229      *            *              * -Added DL-REP-WHOLESALE-REGION
014600160229      *            *              *        to Section 05;
014700160229      *            *              * -Added AD-YTD-DISP-PROCEEDS-BASE
014800160229      *            *              *        AD-YTD-DISP-PROCEEDS-BASE-S
014900160229      *            *              *        AD-YTD-DISP-BOOK-VALUE-BASE
015000160229      *            *              *       AD-YTD-DISP-BOOK-VALUE-BASE-S
015100160229      *            *              *        AD-YTD-DISP-GAIN-LOSS-BASE
015200160229      *            *              *        AD-YTD-DISP-GAIN-LOSS-BASE-S
015300160229      *            *              *        to Section 20;
015400160229      *            *              * -Adede AT-NON-TAX-DEDUCTIONS
015500160229      *            *              *        to Section 80;
015600160229      ******************************************************************
015700160229      * B.LU       *  2001/10/11  * RFS10901
015800160229      *            *              * -Added six fields to Section 5:
015900160229      *            *              *  DL-CONTACT-METHOD1
016000160229      *            *              *  DL-CONTACT-METHOD-DESP1
016100160229      *            *              *  DL-CONTACT-METHOD2
016200160229      *            *              *  DL-CONTACT-METHOD-DESP2
016300160229      *            *              *  DL-CONTACT-METHOD3
016400160229      *            *              *  DL-CONTACT-METHOD-DESP3
016500160229      ******************************************************************
016600160229      * B.LU       *  2001/11/26  * RFS12154
016700160229      *            *              * -Added AT-NON-TAX-DEDUCT-SIGN
016800160229      *            *              *        to Section 80;
016900160229      ******************************************************************
017000160229      * ALI.A      *  2001/01/10  * RFS12173
017100160229      *            *              * - Add field sign indicators for
017200160229      *            *              *   transfers and switches in
017300160229      *            *              *   section 10
017400160229      ******************************************************************
017500160229      * B.LU       *  2002/01/30  * RFS12102
017600160229      *            *              * -Added AD-PHANTOM-FUND-NUMBER      *
017700160229      *            *              *        AT-PHANTOM-FUND-NUMBER.     *
017800160229      ******************************************************************
017900160229      * B.LU       *  2002/04/08  * RFS13184
018000160229      *            *              * -Added six  fields to Section 40:
018100160229      *            *              *  CD-GUARANTEED-BASE-VALUE-OPEN
018200160229      *            *              *  CD-GUARANTEED-BASE-VALUE-CHG
018300160229      *            *              *  CD-G-B-V-C-SIGN
018400160229      *            *              *  CD-CONTRACT-MARKET-VALUE
018500160229      *            *              *  CD-LAST-RESET-DATE
018600160229      *            *              *  CD-CONTRACT-CLOSE-DATE
018700160229      ******************************************************************
018800160229      * ALI.A      *  2002/04/16  * RFS13211
018900160229      *            *              * - Added AD-ALT-ACCOUNT-NO
019000160229      ******************************************************************
019100160229      * ALI.A      *  2002/06/10  * RFS13300
019200160229      *            *              * - Add a sign indicator to field
019300160229      *            *              *   AG-ACCRUED-INTEREST in section 25
019400160229      ******************************************************************
019500160229      * B.LU       *  2002/06/10  * RFS13756
019600160229      *            *              * -Added IS-MASTER-ACCOUNT-NO
019700160229      *            *              *  to Section 10;
019800160229      ******************************************************************
019900160229      * ALI.A      *  2002/08/09  * RFS13955
020000160229      *            *              * - Add GR-GROUP-PLAN-CODE-DESC and
020100160229      *            *              *   GR-GROUP-EMPLOYER-NO TO SECTION
020200160229      *            *              *   65 (GROUP-INFORMATION)
020300160229      ******************************************************************
020400160229      * ALI.A      *  2002/08/09  * RFS14136
020500160229      *            *              * - Add Current and YTD federal and
020600160229      *            *              *   provential WITHDOLDING-TAX fields
020700160229      *            *              *   ,also Current,YTD and incept
020800160229      *            *              *   management fee.
020900160229      ******************************************************************
021000160229      * B.LU       *  2002/09/19  * RFS14556
021100160229      *            *              * -Added IS-TRI-INCEPT-COST
021200160229      *            *              *        IS-TRI-INCEPT-COST-SIGN
021300160229      *            *              *        IS-TRO-INCEPT-COST
021400160229      *            *              *        IS-TRO-INCEPT-COST-SIGN
021500160229      *            *              *     to Section 10;
021600160229      *            *              * -Added AD-TRI-INCEPT-COST
021700160229      *            *              *        AD-TRI-INCEPT-COST-SIGN
021800160229      *            *              *        AD-TRO-INCEPT-COST
021900160229      *            *              *        AD-TRO-INCEPT-COST-SIGN
022000160229      *            *              *     to Section 20;
022100160229      *            *              * -Added BE-BENE-TYPE
022200160229      *            *              *     to Section 67;
022300160229      ******************************************************************
022400160229      * ALI.A      * 2002/10/04   * RFS 14858
022500160229      *            *              * -Added IS2-BAJ-CUR
022600160229      *            *              *        IS2-BAJ-CUR-SIGN
022700160229      *            *              *        IS2-BAJ-YTD
022800160229      *            *              *        IS2-BAJ-YTD-SIGN
022900160229      *            *              *        IS2-BAJ-INCEPT
023000160229      *            *              *        IS2-BAJ-INCEPT-SIGN
023100160229      *            *              *        IS2-BFW-CUR
023200160229      *            *              *        IS2-BFW-CUR-SIGN
023300160229      *            *              *        IS2-BFW-YTD
023400160229      *            *              *        IS2-BFW-YTD-SIGN
023500160229      *            *              *        IS2-BFW-INCEPT
023600160229      *            *              *        IS2-BFW-INCEPT-SIGN
023700160229      *            *              *     to Section 12;
023800160229      *            *              * -Added AD-BAJ-CUR
023900160229      *            *              *        AD-BAJ-CUR-SIGN
024000160229      *            *              *        AD-BAJ-YTD
024100160229      *            *              *        AD-BAJ-YTD-SIGN
024200160229      *            *              *        AD-BAJ-INCEPT
024300160229      *            *              *        AD-BAJ-INCEPT-SIGN
024400160229      *            *              *        AD-BFW-CUR
024500160229      *            *              *        AD-BFW-CUR-SIGN
024600160229      *            *              *        AD-BFW-YTD
024700160229      *            *              *        AD-BFW-YTD-SIGN
024800160229      *            *              *        AD-BFW-INCEPT
024900160229      *            *              *        AD-BFW-INCEPT-SIGN
025000160229      *            *              *     to Section 20;
025100160229      ******************************************************************
025200160229      * ALI.A      * 2002/12/06   * RFS 15022
025300160229      *            *              * -Added Gain or loss redemption
025400160229      *            *              *  fields in sections 10 and 20.
025500160229      ******************************************************************
025600160229      * B.LU       *  2003/04/03  * RFS16990
025700160229      *            *              * -Added new fields to Section 10:
025800160229      *            *              *      IS-NET-ADD-CURRENT
025900160229      *            *              *      IS-NET-ADD-CURRENT-SIGN
026000160229      *            *              *      IS-NET-WITHDRAW-CURRENT
026100160229      *            *              *      IS-NET-WITHDRAW-CURRENT-SIGN
026200160229      *            *              *      IS-DISTRIBUTION-CURRENT
026300160229      *            *              *      IS-DISTRIBUTION-CURRENT-SIGN
026400160229      *            *              *      IS-UNREAL-GAIN-LOSS-CURR
026500160229      *            *              *      IS-UNREAL-GAIN-LOSS-CURR-SIGN
026600160229      *            *              *      IS-NET-INV-CAPTAL-INCEPT
026700160229      *            *              *      IS-NET-INV-CAPTAL-INCEPT-SIGN
026800160229      *            *              * -Added new fields to Section 80:
026900160229      *            *              *      AT-TOTAL-DEDUCTIONS
027000160229      *            *              *      AT-TOTAL-DEDUCTIONS-SIGN
027100160229      ******************************************************************
027200160229      * B.LU       *  2003/03/17  * RFS16256
027300160229      *            *              * -Added IS-TRI-INCEPT-MKT
027400160229      *            *              *        IS-TRI-INCEPT-MKT-SIGN
027500160229      *            *              *        IS-TRO-INCEPT-MKT
027600160229      *            *              *        IS-TRO-INCEPT-MKT-SIGN
027700160229      *            *              *     to Section 10;
027800160229      *            *              * -Added AD-TRI-INCEPT-MKT
027900160229      *            *              *        AD-TRI-INCEPT-MKT-SIGN
028000160229      *            *              *        AD-TRO-INCEPT-MKT
028100160229      *            *              *        AD-TRO-INCEPT-MKT-SIGN
028200160229      *            *              *     to Section 20;
028300160229      ******************************************************************
028400160229      * ALI.A      *  2003/03/18  * RFS16419
028500160229      *            *              * - Added IS-USERID
028600160229      *            *              *     to Section 10;
028700160229      ******************************************************************
028800160229      * B.LU       *  2003/03/24  * RFS16633
028900160229      *            *              * -Added Sign fields to Section 65:
029000160229      *            *              *  GR-TOT-EMPLOYEE-CONTRIYTD-SIGN
029100160229      *            *              *  GR-TOT-EMPLOYER-CONTRIYTD-SIGN
029200160229      *            *              *  GR-TOT-CONTRIB-YTD-SIGN
029300160229      *            *              *  GR-EMPLOYEE-CONTRIFIRST60-SIGN
029400160229      *            *              *  GR-EMPLOYEE-CONTRI-REMAIN-SIGN
029500160229      *            *              *  GR-EMPLOYER-CONTRIFIRST60-SIGN
029600160229      *            *              *  GR-EMPLOYER-CONTRIBREMAIN-SIGN
029700160229      *            *              * -Added Sign fields to Section 67:
029800160229      *            *              *  BE-YTD-CONTRIBUTIONS-SIGN
029900160229      *            *              *  BE-LTD-CONTRIBUTIONS-SIGN
030000160229      *            *              *  BE-YTD-WITHDRAWLS-SIGN
030100160229      *            *              *  BE-LTD-WITHDRAWLS-SIGN
030200160229      *            *              *  BE-YTD-CESG-SIGN
030300160229      *            *              *  BE-LTD-CESG-SIGN
030400160229      *            *              *  BE-YTD-CAP-TRANSFERS-IN-SIGN
030500160229      *            *              *  BE-LTD-CAP-TRANSFERS-IN-SIGN
030600160229      *            *              *  BE-YTD-INC-TRANSFERS-IN-SIGN
030700160229      *            *              *  BE-LTD-INC-TRANSFERS-IN-SIGN
030800160229      *            *              *  BE-YTD-CESG-TRANSFERS-IN-SIGN
030900160229      *            *              *  BE-LTD-CESG-TRANSFERS-IN-SIGN
031000160229      *            *              *
031100160229      ******************************************************************
031200160229      * Odette     *              * RFS17217  Added DI-CLIENT-CODE
031300160229      * McMillan   * 2003/06/05   *  DI-STMT-TYPE-IND TO CPYJSISTMT.
031400160229      ******************************************************************
031500160229      * Odette     * 2003/05/15   * RFS 17165
031600160229      * McMillan   *              * - Expand MFASTMEXP from 1100 to
031700160229      *            *              *   1350 record length.
031800160229      ******************************************************************
031900160229      * Odette     * 2003/06/04   * RFS 16881
032000160229      * McMillan   *              * - Add new fields to report Phantom
032100160229      *            *              * fund info when money is leaving/
032200160229      *            *              * coming into an account. Add new
032300160229      *            *              * fields to sections 10 (Investor
032400160229      *            *              * detail) and 20 (Account detail).
032500160229      ******************************************************************
032600160229      * Brian Lu & *  2003/08/19  * RFS16511/18091                     *
032700160229      * Odette     *              * -Added Section 17.                 *
032800160229      * McMillan   *              * -Added AC-TARGET-MIX-PERCENT       *
032900160229      *            *              *  to Section 15:                    *
033000160229      ******************************************************************
033100160229      * Odette     *  2003/08/22  * RFS17380                           *
033200160229      * McMillan   *              * -Added DL-BRANCH-CO-ATTN, DL-DEALER*
033300160229      *            *              *  -CO-ATTN, DL-NOMINEE-CO-ATTN to   *
033400160229      *            *              *  to Section 5.                     *
033500160229      ******************************************************************
033600160229      * Odette     * 2003/09/22   * RFS 18653
033700160229      * McMillan   *              * - Fix position of Section 17 rec
033800160229      *            *              * counter - should be at end of
033900160229      *            *              * layout for Section 00 & 99.
034000160229      ******************************************************************
034100160229      * B.LU       *  2003/09/26  * RFS17527
034200160229      *            *              * -Added IS-CHARGES
034300160229      *            *              *        IS-CHARGES-SIGN
034400160229      *            *              *        IS-REALIZED-GAIN-LOSS
034500160229      *            *              *        IS-REALIZED-GAIN-LOSS-SIGN
034600160229      *            *              *        IS-UNREALIZED-GAIN-LOSS
034700160229      *            *              *        IS-UNREALIZED-GAIN-LOSS-SIGN
034800160229      *            *              *        IS-ACCRUED-INCOME
034900160229      *            *              *        IS-ACCRUED-INCOME-SIGN
035000160229      *            *              *        IS-IPS-GROUP-ID
035100160229      *            *              *     to Section 10
035200160229      *            *              * -Added AC-IPS-GROUP-ID
035300160229      *            *              *     to Section 15
035400160229      *            *              * -Added AD-NET-AMOUNT-CURR
035500160229      *            *              *        AD-NET-AMOUNT-CURR-SIGN
035600160229      *            *              *        AD-NO-OF-UNITS-CURR
035700160229      *            *              *        AD-NO-OF-UNITS-CURR-SIGN
035800160229      *            *              *        AD-GROUP-PERCENTAGE
035900160229      *            *              *        AD-INVESTMENT-GROUP-CODE
036000160229      *            *              *        AD-IPS-GROUP-ID
036100160229      *            *              *     to Section 20
036200160229      ******************************************************************
036300160229      * ALI        *  2003/09/30  * RFS17626
036400160229      * ALNEMER    *              * - Add Section-33 (Account-ROR)
036500160229      *            *              *   by Frequency
036600160229      *            *              * - Add Section-38 (Fund-ROR)
036700160229      *            *              *   by Frequency
036800160229      *            *              * - Add the following fields to
036900160229      ******************************************************************
037000160229      * B.LU       *  2003/10/14  * RFS18682
037100160229      *            *              * - Renamed IS-CHARGES and
037200160229      *            *              *           IS-CHARGES-SIGN
037300160229      *            *              *   to      IS-FEES    and
037400160229      *            *              *           IS-FEES-SIGN
037500160229      *            *              *   in Section 10.
037600160229      ******************************************************************
037700160229      * B.LU       *  2003/10/14  * RFS18667
037800160229      *            *              * -Added new fields to Section 17:
037900160229      *            *              *  IPS-REP-PHONE-AREA-CODE
038000160229      *            *              *  IPS-REP-PHONE-NUMBER
038100160229      *            *              *  IPS-REP-PHONE-EXT
038200160229      *            *              *  IPS-REP-FAX-AREA-CODE
038300160229      *            *              *  IPS-REP-FAX-NUMBER
038400160229      ******************************************************************
038500160229      * Ali.A      *  2003/10/21  * RFS18312
038600160229      *            *              * -Added new fields to Section 20:
038700160229      *            *              *  AD-DEPOSIT
038800160229      *            *              *  AD-WITHDRAWALS
038900160229      *            *              *  AD-AVAIL-FGN-CONTENT-SIGN
039000160229      ******************************************************************
039100160229      * Ali.A      *  2003/11/18  * RFS18515
039200160229      *            *              * -Added new field Section-10:
039300160229      *            *              *  IS-FIRST-TRADE-DATE
039400160229      ******************************************************************
039500160229      * Ali.A      *  2003/11/20  * RFS18814
039600160229      *            *              * -Added new field Section 30 AND 33
039700160229      *            *              *  AP-IPS-GROUP-ID.
039800160229      *            *              *  APF-IPS-GROUP-ID.
039900160229      ******************************************************************
040000160229      * Ali.A      *  2004/02/17  * RFS19444
040100160229      *            *              * -Added new field Section 17
040200160229      *            *              *  IPS-ADDREES-1
040300160229      *            *              *  IPS-ADDREES-2
040400160229      *            *              *  IPS-ADDREES-3
040500160229      *            *              *  IPS-ADDREES-4
040600160229      *            *              *  IPS-ADDREES-5
040700160229      *            *              *  IPS-POSTAL-CODE
040800160229      *            *              *  IPS-COUNTRY
040900160229      *            *              *  IPS-PROVINCE-CODE
041000160229      ******************************************************************
041100160229      * ALI.A      *  2003/03/11  * RFS 20870 - Add these fields to    *
041200160229      *            *              * the DELIVERY-INFO-SECTION.         *
041300160229      *            *              *     DI-RECORD-COUNT-33             *
041400160229      *            *              *     DI-RECORD-COUNT-38             *
041500160229      ******************************************************************
041600160229      * Ali.A      *  2004/03/30  * RFS20758
041700160229      *            *              * -Added a new field to report the
041800160229      *            *              *  the calculation date for Sect-30
041900160229      *            *              *  AP-CLAC-DATE
042000160229      *            *              * -Added new fields to report ROC
042100160229      *            *              *  calculations for Sect-12
042200160229      *            *              *  IS2-ROC-CUR
042300160229      *            *              *  IS2-ROC-CUR-SIGN
042400160229      *            *              *  IS2-ROC-INCEPT
042500160229      *            *              *  IS2-ROC-INCEPT-SIGN
042600160229      *            *              *  IS2-ROC-YTD
042700160229      *            *              *  IS2-ROC-YTD-SIGN
042800160229      *            *              * -Added new fields to report ROC
042900160229      *            *              *  calculations for Sect-20
043000160229      *            *              *  AD-ROC-CUR
043100160229      *            *              *  AD-ROC-CUR-SIGN
043200160229      *            *              *  AD-ROC-INCEPT
043300160229      *            *              *  AD-ROC-INCEPT-SIGN
043400160229      *            *              *  AD-ROC-YTD
043500160229      *            *              *  AD-ROC-YTD-SIGN
043600160229      ******************************************************************
043700160229      * Odette     *  2004/04/27  * RFS 21714 - Add new fields that    *
043800160229      * Aguinaldo- *              * will indicate to EDM, which forms  *
043900160229      * McMillan   *              * will be extracted. These will be   *
044000160229      *            *              * added to the INVESTOR-SUMMARY-     *
044100160229      *            *              * SECTION (Section 10):              *
044200160229      *            *              *   IS-CLTSTCLT-FLAG                 *
044300160229      *            *              *   IS-CLTSTREP-FLAG                 *
044400160229      *            *              *   IS-CLTSTSPR-FLAG                 *
044500160229      *            *              *   IS-NOMREPSP-FLAG                 *
044600160229      ******************************************************************
044700160229      * B.LU       *  2004/08/19  * RFS18924
044800160229      *            *              * -Added new fields to Section 90:
044900160229      *            *              *  RC-OLD-GUARANTEE-DEATH
045000160229      *            *              *  RC-NEW-GUARANTEE-DEATH
045100160229      *            *              *  RC-GDV-RESET-IND
045200160229      ******************************************************************
045300160229      * B.LU       *  2004/10/04  * RFS22856
045400160229      *            *              * -Added new fields to Section 40:
045500160229      *            *              *  CD-GUAR-DTH-BENEFIT-VALUE
045600160229      *            *              *  CD-GUAR-DTH-BENEFIT-VALUE-IND
045700160229      *            *              *  CD-GUAR-MAT-BENEFIT-VALUE
045800160229      *            *              *  CD-GUAR-MAT-BENEFIT-VALUE-IND
045900160229      *            *              *  CD-SUM-ALL-AWD
046000160229      *            *              *  CD-SUM-ALL-AWD-IND
046100160229      ******************************************************************
046200160229      * ALI.A      *  2004/07/20  * RFS 18780 - Add these fields to    *
046300160229      *            *              * the ACCOUNT-TXN-SECTION (SECT 80)  *
046400160229      *            *              *     AT-FEDC-CLAWBACK               *
046500160229      *            *              *     AT-PRVC-CLAWBACK               *
046600160229      ******************************************************************
046700160229      * B.LU       *  2004/12/06  * RFS24514
046800160229      *            *              * -Added A new fields to Section 70:
046900160229      *            *              *  SP-INVESTMENT-NAME
047000160229      ******************************************************************
047100160229      * Chandana R.*  2005/03/09  * RFS26213
047200160229      *            *              * -Added new fields to Section 20:
047300160229      *            *              *  AD-UNIT-BALANCE-PRIOR-Q1
047400160229      *            *              *  AD-UNIT-BALANCE-PRIOR-Q2
047500160229      *            *              *  AD-UNIT-BALANCE-PRIOR-Q3
047600160229      *            *              *  AD-UNIT-BALANCE-PRIOR-Q4
047700160229      *            *              *  AD-UNIT-BALANCE-PRIOR-YE
047800160229      *            *              *  AD-UNIT-PRICE-PRIOR-Q1
047900160229      *            *              *  AD-UNIT-PRICE-PRIOR-Q2
048000160229      *            *              *  AD-UNIT-PRICE-PRIOR-Q3
048100160229      *            *              *  AD-UNIT-PRICE-PRIOR-Q4
048200160229      *            *              *  AD-UNIT-PRICE-PRIOR-YE
048300160229      ******************************************************************
048400160229      * B.LU       *  2005/03/30  * RFS26894
048500160229      *            *              * -Added AT-ERF TO SECTION 80        *
048600160229      ******************************************************************
048700160229      * Ali        * 2005/08/25   * RFS 28931
048800160229      * Alnemer    *              * - Add a new section for account
048900160229      *            *              *   additional information.
049000160229      ******************************************************************
049100160229      * ADE .A     *  2005/08/29  * RFS 29550 - Add these fields to    *
049200160229      *            *              * the ACCOUNT-TXN-SECTION (SECT 80)  *
049300160229      *            *              *     AT-US-BACKUP-WHTX              *
049400160229      *            *              *     AT-US-BACKUP-WHTX-SIGN         *
049500160229      ******************************************************************
049600160229      * ADE A.     *  2005/09/28  * RFS30065
049700160229      *            *              * -Added FILLER TO SECTION 80        *
049800160229      ******************************************************************
049900160229      * C.LENTINI  *  2005/09/07  * RFS28692
050000160229      *            *              * -Added new fields to Section 80.   *
050100160229      *            *              * - AT-ROC-RATE                      *
050200160229      *            *              * - AT-ROC-RATE-SIGN                 *
050300160229      ******************************************************************
050400160229      * Geeta S.   *  2005/09/15  * RFS 28107- Added 2 fields & reduced
050500160229      *            *              * the size of filler for section 40.
050600160229      ******************************************************************
050700160229      * Esin A.    *  2006/03/07  * RFS31514 - Added AWD Carried Over
050800160229      *            *              * field and the reduced the size of
050900160229      *            *              * filler to 1051 for the section 40.
051000160229      *            *              * (SEG-FUND-CONTRACT-DETAILS)
051100160229      * Pawel A.   *  2006/05/18  * RFS31514 - SQA support AWD Carried
051200160229      *            *              * Over field moved after
051300160229      *            *              * CD-VALIDATION-DATE
051400160229      * Bharti M   * 2006/06/21   * RFS 33331 - Added new fields
051500160229      *            *              * to section 10 -
051600160229      *            *              * IS-GAIN-LOSS-PD-OUT and
051700160229      *            *              * IS-GAIN-LOSS-PD-OUT-SIGN
051800160229      * Brian Lu & *  2006/06/28  * RFS35042                           *
051900160229      *            *              * -Added Section 21 COUNTER.         *
052000160229      * FATIMA Y.  *  2006/06/15  * RFS34072
052100160229      *            *              * -Added AT-REDEM-CHARGE             *
052200160229      *            *              *        AT-REDEM-CHARGE-SIGN        *
052300160229      * Raymond Mui*  2006/08/18  * RFS34848
052400160229      *            *              * -Added Section 81 Deduction charges*
052500160229      *            *              *        for confirmation extract.   *
052600160229      * Andy Lo    *  2006/09/20  * RFS34848.5
052700160229      *            *              * -Added DI-RECORD-COUNT-81 to       *
052800160229      *            *              *  DELIVERY-INFO-SECTION.            *
052900160229      * L. Wong    *  2006/10/19  * RFS36342
053000160229      *            *              * -Add new fields to Section 21:
053100160229      *            *              *  AAI-CURR-DISP-PROCEEDS-BASE
053200160229      *            *              *  AAI-CURR-DISP-PROCEEDS-BASE-S
053300160229      *            *              *  AAI-CURR-DISP-BOOK-VAL-BASE
053400160229      *            *              *  AAI-CURR-DISP-BOOK-VAL-BASE-S
053500160229      *            *              *  AAI-CURR-DISP-GAIN-LOSS-BASE
053600160229      *            *              *  AAI-CURR-DISP-GAIN-LOSS-BASE-S
053700160229      *            *              *  AAI-GROSS-SOLD-YTD-BASE
053800160229      *            *              *  AAI-GROSS-SOLD-YTD-BASE-SIGN
053900160229      *            *              *  AAI-GROSS-SOLD-CURR-BASE
054000160229      *            *              *  AAI-GROSS-SOLD-CURR-BASE-SIGN
054100160229      *            *              *  AAI-DEDUCTIONS-DSC-YTD-BASE
054200160229      *            *              *  AAI-DEDUCTIONS-DSC-YTD-BASE-S
054300160229      *            *              *  AAI-DEDUCTIONS-DSC-CURR-BASE
054400160229      *            *              *  AAI-DEDUCTIONS-DSC-CURR-BASE-S
054500160229      * Pawel A    *  2007/03/13  * RFS38639
054600160229      *            *              * -Add new fields to Section
054700160229      *            *              * Statement File - Seg Fund Invest
054800160229      *            *              *  RC-RESET-PROCESS-DATE
054900160229      * Abdi Hassan* 2007/03/21 * RFS37754 - Added new field
055000160229      *            *            * Cd-TopUpNotionalAmt in section 40
055100160229      *            *              *                                    *
055200160229      * PAWEL A    * 2007/06/04  * RFS39466 ADD NEW FIELD TO SECTION 20
055300160229      *            *             * AD-APP-RECEIVED
055400160229      *            *             * ADD NEW FIELDS TO SECTION 80
055500160229      *            *             * AT-MATURITY-TYPE AND AT-CONTRACT-XRE
055600160229      *            * 2007/09/06 * AND AT-CONTRACT-NO
055700160229      *            *              *                                    *
055800160229      * Allen Cha  * 2007/09/11   * RFS43691                           *
055900160229      *            *              * - Added new DL-PROF-DES field      *
056000160229      *            *              *      to Section 5 (Dealer)         *
056100160229      * Raymond Mui* 2008/04/28   * RFS47406
056200160229      *            *              * Added GLWB related fields for      *
056300160229      *            *              * section40 and section 80.          *
056400160229      * Bathuu L   * 2008/06/16   * RFS50993
056500160229      *            *              * -Add new fields to Section 10:
056600160229      *            *              *  IS-MULTIPLE-CONTRACT-FLAG
056700160229      *            *              *  IS-JOINT-LIFE-FLAG
056800160229      *            *              * -Add new fields to Section 40:
056900160229      *            *              *  CD-DOB-FOR-CALC
057000160229      *            *              *  CD-NEW-DEPOSIT
057100160229      *            *              *  CD-SERIES-DESCRIPTION-FRENCH
057200160229      * ABDI H.    * 2008/08/18   * RFS46797 - ADD BEN-STATUS AND
057300160229      *            *              * BE-ACCT-TYPE-RULE IN SECTION 67    *
057400160229      * Brian Lu   * 2008/09/22   * RFS50823 - ADD 4 fields to Section
057500160229      *            *              *            21.                     *
057600160229      * Alan A.    * 2008/10/16   * RFS52815
057700160229      *            *              * -Add new fields to Section 01 & 40
057800160229      *            *              *  for GLWB and RRIF YE Statement
057900160229      *            *              *  extract development
058000160229      * C.LENTINI  * 2008/10/27   * RFS50492 - Add DORMANT-FUND Flag
058100160229      *            *              * to sections 20 & 21.               *
058200160229      * B.LU       *  2009/04/16  * RFS61356
058300160229      *            *              * -Added IS-LTD-CONTRIBUTION
058400160229      *            *              *        IS-LTD-CONTRIBUTION-SIGN
058500160229      *            *              *        IS-LTD-WITHDRAWAL
058600160229      *            *              *        IS-LTD-WITHDRAWAL-SIGN
058700160229      *            *              *        IS-YTD-CONTRIBUTION
058800160229      *            *              *        IS-YTD-CONTRIBUTION-SIGN
058900160229      *            *              *        IS-YTD-WITHDRAWAL
059000160229      *            *              *        IS-YTD-WITHDRAWAL-SIGN
059100160229      * R Miguel   *  2009/05/13  * RFS 69135
059200160229      *            *              * Rearrange the sequence of the fields
059300160229      *            *              * in 61356.
059400160229      * Andy Chan  *  2009/05/19  * RFS 69424 - Recompile
059500160229      * Brian Lu   * 2009/06/19   * RFS64262 - ADD 4 fields to Section
059600160229      *            *              *            40 AND 90               *
059700160229      * Andy Lo    *  2009/11/10  * RFS69515 - Add section 63 for RDSP.
059800160229      * Andy Lo    *  2010/03/08  * RFS78161 - Add sign fields to sect 40.
059900160229      * Andy Lo    *  2010/03/08  * RFS74626 - Add Contract Status to sec 40.
060000160229      * R Miguel   *  2010/05/03  * RFS79298 - Additional columns for
060100160229      *            *              * Section 63.
060200160229      * B.Lu       *  2010/07/16  * RFS82793 - Added 2 fields to Section 1:
060300160229      *            *              *        II-INVESTOR-PROVINCE-CODE
060400160229      * Fatema Haji*  2010/08/17  * RFS80555 - Added 4 fields to Sectio
060500160229      *            *              * GPP Flag,Original or Current YR
060600160229      *            *              * GLWA,Original or Current YR GLWA
060700160229      *            *              * sign,GLWA % at YE before LID
060800160229      * Brian Lu & *  2010/10/27  * RFS83788                           *
060900160229      *            *              * -Added 16 FIELDS TO SECTION 67     *
061000160229      * Brian Lu & *  2010/11/19  * RFS86495                           *
061100160229      *            *              * -Added 2 FIELDS TO SECTION 80      *
061200160229      * Geeta S.   *  2010/10/02  * RFS 84980 - Added P-Grant for Sec 62.
061300160229      * Brian Lu & *  2011/02/24  * RFS80605                           *
061400160229      *            *              * -Added 2 FIELDS TO SECTION 40      *
061500160229      * Andy Lo    *  2011/04/25  * RFS63488 - Add fields to Section 90.
061600160229      * Jeyarajan  *  2011/05/05  * RFS92477 - Add the below new fields.
061700160229      *            *              * AT-TRANS-SUB-TYPE
061800160229      *            *              * AT-TRANS-SUB-TYPE-DESC
061900160229      * J.O'CALLAGH*  2011/09/07  * RFS96081 - Add CI-EW-Alert --field.
062000160229      * Rekha S    *  2012/01/24  * RFS93662 - Added 7 fields to
062100160229      *            *              * section 10,
062200160229      *            *              * 3 fields to section 70
062300160229      *            *              * and 3 fields to section 80
062400160229      * Brian Lu & *  2012/03/15  * RFS102716                          *
062500160229      *            *              * -Added 6 FIELDS TO SECTION 21      *
062600160229      * Brian Lu & *  2012/07/27  * RFS91272                           *
062700160229      *            *              * -Added 4 FIELDS TO SECTION 63      *
062800160229      * Nasra F    *  2012/09/17  * RFS91272  Recompile                *
062900160229      * ---------------------------------------------------------------*
063000160229      * Suresh G.  *  2012/09/18  * RFS112003 - Added 3 fields and     *
063100160229      *            *              * Changed the length of the FILLER   *
063200160229      *            *              * field from 1130 to 1055 under sect-*
063300160229      *            *              * ion "SYSTEMATIC-PLAN-INFO-SECTION".*
063400160229      * Daphne D   *  2013/03/18  * RFS117038 - Added new fields       *
063500160229      * Abhisek M  *  2013/03/18  * RFS110904 - Guarantee Benefit      *
063600160229      *            *              * for Nominee Payout Account         *
063700160229      * MUTHU      *  2013/07/12  * RFS120931 - MAXIMUM WITHDRAWAL     *
063800160229      * LAKSHMI M  *              * PAYOUT GURANTEE AMOUNT.            *
063900160229      * Praveen E  *  2013/09/17  * RFS123925 - Added new feilds and   *
064000160229      *            *              * reduced the filler length from 1050*
064100160229      *            *              * to 1026.                           *
064200160229      * SURESH G.  *  2013/11/11  * RFS130402 - FIX FOR INCORRECT      *
064300160229      *            *              * AMOUNTS IS REPORTED WITHOUT THE    *
064400160229      *            *              * DECIMAL VALUES FOR FIELDS          *
064500160229      *            *              * CD-MAX-WITHDRAWAL-AMOUNT AND       *
064600160229      *            *              * CD-YTD-WITHDRAWN-AMOUNT, REDUCED   *
064700160229      *            *              * THE FILLER LENGTH FROM 620 TO 618  *
064800160229      *            *              * IN SECTION 40                      *
064900160229      *            *              * (SEG-FUND-CONTRACT-DETAILS).       *
065000160229      * Michael Fan*  2013/11/11  * RFS130640 - Remove changes on      *
065100160229      *            *              * RFS130402 that the fields          *
065200160229      *            *              * CD-MAX-WITHDRAWAL-AMOUNT and       *
065300160229      *            *              * CD-YTD-WITHDRAWN-AMOUNT should not *
065400160229      *            *              * contain decimal point.             *
065500160229      ******************************************************************
065600160229      * ---------------------------------------------------------------*
065700160229      * Suresh G.  *  2013/10/15  * RFS124157 - Added 4 fields and     *
065800160229      *            *              * Changed the length of the FILLER   *
065900160229      *            *              * field from 1026 to 1002 under sect-*
066000160229      *            *              * ion "RDSP-DETAIL-SECTION".         *
066100160229      * SANGEETHA V*  2014/01/28  * RFS129378 - EMPIRE PHASE 2         *
066200160229      *            *              * CHANGES TO STATEMENT EXTRACT FOR   *
066300160229      *            *              * GIO/TIO                            *
066400160229      * NOBLE T    *  2014/05/02  * RFS130239 - ADDED 4 FIELDS AND     *
066500160229      *            *              * CHANGED THE LENGTH OF THE FILLER   *
066600160229      *            *              * FIELD FROM 873 TO 850 UNDER SECTION*
066700160229      *            *              * 12 "INVESTOR-SUMMARY-SECTION-2".   *
066800160229      * RAMYA K    *  2014/03/24  * RFS124070 - FINAL TOP UP DATE      *
066900160229      *            *              * ON AN INCOME PLAN ACCOUNT          *
067000160229      * JEYARJAN   *  2014/07/07  * RFS132121 -INRODUCE NEW FIELDS IN  *
067100160229      *            *              * RRIF-DETAIL-SECTION &              *
067200160229      *            *              * BENEFICIARY-INFORMATION            *
067300160229      *            *              * INTRODUCE NEW SECTION              *
067400160229      *            *              * ACCOUNT-LEVEL-ASSET-ALLOC-DTL &    *
067500160229      *            *              * TRAILER-INFO-SECTION.              *
067600160229      * SIDDHARTH N* 2014/08/12   * RFS130038 -ADDED A NEW FIELD CALLED*
067700160229      *            *              *       AT-OMS-SUPPRESS.             *
067800160229      * NAGAMNAI S * 2014/08/13   * RFS139007 - RECOMPILE FOR CPGLWBRULE
067900160229      * SIDDHARTH N* 2014/09/11   * RFS139185 - ADDED TWO NEW FIELD IN *
068000160229      *            *              *             SECTION 12.            *
068100160229      * Jeyarajan  *  2014/08/04  * RFS133489 - Added 8 fields and     *
068200160229      *            *              * changed the length of the filler   *
068300160229      *            *              * field from 841 TO 793 under section*
068400160229      *            *              * 67 "BENEFICIARY-INFORMATION".      *
068500160229      * JEYARAJAN  * 2014/09/19   * RFS140627 - RECOMPILE FOR ANNSTMTJSI
068600160229      * JEYARAJAN  * 2014/09/26   * RFS141019 - RECOMPILE CPYSTM16AA   *
068700160229      * SIDDHARTH N* 2014/10/22   * RFS139988 - ADDING A NEW FIELD IN  *
068800160229      *            *              * SECTION 25.                        *
068900160229      * MEENU K    * 2014/10/13   * RFS138310 - ADDED 2 FIELDS AND     *
069000160229      *            *              * CHANGED THE LENGTH OF THE FILLER   *
069100160229      *            *              * FIELD FROM 1104 TO 1082 UNDER      *
069200160229      *            *              * SECTION 60 "RRIF-DETAIL-SECTION".  *
069300160229      * POOJA K    * 2015/02/16   * RFS143392 - REPORTING DSC FUNDS ON *
069400160229      *            *              * STATEMENTS                         *
069500160229      * POOJA K    * 2015/03/26   * RFS144030 - REPORTING TFSA SUCCESSOR
069600160229      *            *              * HOLDER NAME ON STATEMENT EXTRACT   *
069700160229      *POOJA K     * 2015/04/21   * RFS142884-STATEMENTS AND CONFIRMS  *
069800160229      *            *              * EXTRACS ENHANCEMENT FOR SECTION    *
069900160229      *            *              * 01,25,40,80
070000160229      * POOJA K    * 2015/05/08   * RFS144587 - CHANGING SELECTION     *
070100160229      *            *              * CRITERIA FOR INCOME PAYMENT AND    *
070200160229      *            *              * WITHOLDING TAX                     *
070300160229      * AMRUTA A   * 2015/07/21   * RFS148595 - ADD GWA, LWA, & EWA    *
070400160229      *            *              * ORIGINAL VALUE FIELDS TO SECTION 40*
070500160229      *            *              * OF STATEMENT/CONFIRM EXTRACT       *
070600160229      * SHAIK RABS * 2015/08/24   * RFS152451-ADDITIONAL FEE CHANGES   *
070700160229      * JEYARAJAN  * 2015/10/19   * RFS151145 - IN SECTION 21 NEW FIELD*
070800160229      *             *              * AAI-CROSS-REF-NUMBER ADDED        *
070900160229      * JEYARAJAN  * 2015/12/16   * RFS147795 - IN SECTION 21 ADDED    *
071000160229      *             *              * TWO NEW FIELDS.                   *
071100160229      *             *              * AAI-YTD-TRAILER                   *
071200160229      *             *              * AAI-YTD-TRAILER-SIGN              *
071300160229      * SHAIK RABS  *2016/01/25    *RFS148326 ADD ADDTIONAL DATA       *
071400160229      *             *              *   ELEMENTS TO STATEMENT EXTRACT   *
071500160229      *             *              *                                   *
071600160614      * BHARGAV M   * 2016/05/17  * RFS156335 - SECTION 21 NEW FIELDS  *
071700160614      *             *             * ARE ADDED & SECTION 15 ASSET       *
071800160614      *             *             * CLASS BOOK VALUE IS POPULATED.
071900161104      * POOJA K    * 2016/10/12   * RFS156507-INCLUDE TRANSACTIONS ON  *
072000161104      *            *              * THE STATEMENTS EXTRACT BASED ON    *
072100161104      *            *              * THE TRADE DATE                     *
072200170217      * JANAPRIYA J* 2017/01/10   * RFS164528 -CRM2-CASH FLOWS EXTRACT *
072300170508      * Arthy K    * 2017/05/08 * RFS163106 Changes to RESP Processing.
072400170512      * Aarthi M   * 2017/05/12   * RFS145044-Added Delivery option in *
072500170512      *            *              * Section 10.                        *
072600180530      * Joel Dsouza* 2018/05/29   * RFS177152 - Section 40 enchancement*
072700180820      * Sethu B    * 2018/08/06   * RFS172713 - Section 67 enchancement*
072800180926      * Digvijay   * 2018/09/03   * RFS176735 - Section 80 enchancement*
072900181023      *            *              * with new additional two fields.    *
073000190723      * Niranjan   * 2019/06/25   * RFS183097 - Include Joint Tenant   *
073100190723      *            *              * name in Confirm/Statement          *
073200191030      * Priya N    * 2019/10/22   * RFS184366 - Added 2 new fields     *
073300191030      *            *              * to Section 05.                     *
073400191030      *            *              * DL-PRIMARY-NAME                    *
073500191030      *            *              * DL-INCORP-REP-IND                  *
073600200128      * K Naveen   * 2020/01/22   * RFS184872 - Increase the length for*
073700200128      *            *              * the field DL-PRIMARY-NAME from     *
073800200128      *            *              * 60-100 characters                  *
073900200515      * Jebastin S * 2020/05/15   * RFS185916 - Populate prod type in  *
074000200515      *            *              * the Section 79.                    *
074100201118      * Vignesh    * 2020/11/18   * RFS185455 -                        *
074200201118      *            *              * Death and Maturity Resets          *
074300210105      * Surendran C* 2020/12/02   * RFS186446- To Identify first Policy*
074400210105      *            *              * in section 80 statement/confirm    *
074500210105      *            *              * Extract.                           *
074600210823      * Kamal T    * 2021/06/23   * RFS186199 - Added layout for new   *
074700210823      *            *              * section 14 and modified existing   *
074800210823      *            *              * section 40 for new fields related  *
074900210823      *            *              * MDBA guarantee. Also update section*
075000210823      *            *              * 99 to include new section count var*
075100210823      * SURENDRAN C* 2021/06/30   * RFS186199- TO INCLUDE SECTION83    *
075101211019      * Surendran C* 2021/08/11   * RFS186592- To include new Guarantee*
075102211019      *            *              * Reset Option in Section 12         *
075200160229      ******************************************************************
075300160229
075400160229      /
075500160229       01  MFASTMTFP-SECTION.
075600190723
075700160229      ******************************************************************
075800160229      *                                                                *
075900160229      *    CPYJSISTMT: Statement File - Primary Record                 *
076000160229      *    CREATED:    03/DEC/96 (MFH)                                 *
076100160229      *    CHANGED:    22/MAY/98 (MFH)                                 *
076200160229      *    LENGTH:     750                                             *
076300160229      *                                                                *
076400160229      ******************************************************************
076500160229
076600160229      *****
076700160229      *****    This record is to be used as a temporary holding area.
076800160229      *****  The first 2 fields will be part of the record key.
076900160229      *****
077000160229
077100160229           03  PR-INVESTOR-NO          PICTURE 9(9).
077200160229           03  PR-ACCOUNT              PICTURE 9(9).
077300160229           03  PR-SECTION-NO-X.
077400160229               05  PR-SECTION-NO       PICTURE 9(2).
077500160229           03  PR-SECTION-SEQUENCE     PICTURE 9(6).
077600160229           03  PR-LANGUAGE-CODE        PICTURE X(1).
077700160229           03  PR-DEALER               PICTURE X(4).
077800160229           03  PR-DEALER-REP-CODE      PICTURE X(6).
077900160229R17165     03  FILLER                  PICTURE X(1290).
078000160229           03  PR-SECTION-SEQUENCE-2   PICTURE 9(6).
078100160229           03  PR-START-DATE           PICTURE 9(08).
078200160229           03  PR-END-DATE             PICTURE 9(08).
078300160229           03  PR-EOR-INDICATOR        PICTURE X(1).
078400160229      /
078500160229       01  DELIVERY-INFO-SECTION.
078600160229
078700160229      ******************************************************************
078800160229      *                                                                *
078900160229      *    CPYJSISTMT: Statement File - Delivery Information Record    *
079000160229      *    CREATED:    DD/MMM/YY (III)                                 *
079100160229      *    CHANGED:    15/DEC/97 (MFH)                                 *
079200160229      *    CHANGED:    20/JUL/98 (HS)
079300160229      *    LENGTH:     750                                             *
079400160229      *                                                                *
079500160229      ******************************************************************
079600160229
079700160229      *****
079800160229      *****    - INVESTOR-NO will always be zeroes.
079900160229      *****    - ACCOUNT will always be zeroes.
080000160229      *****    - SECTION-NO will always be zeroes.
080100160229      *****    - SECTION-SEQUENCE will always be zeroes.
080200160229      *****    - LANGUAGE-CODE will be one of the following.
080300160229      *****        'E' English
080400160229      *****        'F' French
080500160229      *****    - DEALER will always be spaces.
080600160229      *****    - DEALER-REP-CODE will always be spaces.
080700160229      *****
080800160229
080900160229           03  DI-INVESTOR-NO          PICTURE 9(9).
081000160229           03  DI-ACCOUNT              PICTURE 9(9).
081100160229           03  DI-SECTION-NO           PICTURE 9(2).
081200160229           03  DI-SECTION-SEQUENCE     PICTURE 9(6).
081300160229           03  DI-LANGUAGE-CODE        PICTURE X(1).
081400160229           03  DI-DEALER               PICTURE X(4).
081500160229           03  DI-DEALER-REP-CODE      PICTURE X(6).
081600160229           03  DI-DELIVERY-METHOD      PICTURE X(3).
081700160229           03  DI-FAX-NUMBER           PICTURE 9(12).
081800160229           03  DI-PRINTER-ID           PICTURE X(10).
081900160229           03  DI-RECIPIENT            PICTURE X(45).
082000160229           03  DI-COMPANY              PICTURE X(40).
082100160229           03  DI-RECORD-COUNT-TOT     PICTURE 9(9).
082200160229           03  DI-RECORD-COUNT-00      PICTURE 9(9).
082300160229           03  DI-RECORD-COUNT-01      PICTURE 9(9).
082400160229           03  DI-RECORD-COUNT-05      PICTURE 9(9).
082500160229           03  DI-RECORD-COUNT-10      PICTURE 9(9).
082600160229           03  DI-RECORD-COUNT-15      PICTURE 9(9).
082700160229           03  DI-RECORD-COUNT-20      PICTURE 9(9).
082800160229RF7051     03  DI-RECORD-COUNT-30      PICTURE 9(9).
082900160229           03  DI-RECORD-COUNT-35      PICTURE 9(9).
083000160229           03  DI-RECORD-COUNT-50      PICTURE 9(9).
083100160229           03  DI-RECORD-COUNT-60      PICTURE 9(9).
083200160229           03  DI-RECORD-COUNT-67      PICTURE 9(9).
083300160229           03  DI-RECORD-COUNT-80      PICTURE 9(9).
083400160229           03  DI-RECORD-COUNT-82      PICTURE 9(9).
083500160229           03  DI-RECORD-COUNT-84      PICTURE 9(9).
083600160229           03  DI-NO-OF-UNITS          PICTURE 9(14)V9(3).
083700160229           03  DI-UNIT-PRICE           PICTURE 9(9)V9(4).
083800160229           03  DI-MARKET-VALUE         PICTURE 9(15)V9(2).
083900160229           03  DI-BOOK-VALUE           PICTURE 9(15)V9(2).
084000160229           03  DI-UNIT-BALANCE         PICTURE 9(14)V9(3).
084100160229           03  DI-RECORD-COUNT-70      PICTURE 9(9).
084200160229           03  DI-USERID               PICTURE X(10).
084300160229           03  DI-REQUEST-DATE         PICTURE 9(8).
084400160229           03  DI-REQUEST-TIME         PICTURE 9(8).
084500160229           03  DI-RECORD-COUNT-65      PICTURE 9(9).
084600160229           03  DI-REQUEST-FILE         PICTURE X(10).
084700160229           03  DI-RECORD-COUNT-62      PICTURE 9(9).
084800160229           03  DI-FOREIGN-CONTENT-LIMIT  PICTURE 9(3).
084900160229RF8633     03  DI-RECORD-COUNT-12      PICTURE 9(9).
085000160229RF8633     03  DI-RECORD-COUNT-25      PICTURE 9(9).
085100160229RF8633     03  DI-RECORD-COUNT-40      PICTURE 9(9).
085200160229RF8633     03  DI-RECORD-COUNT-43      PICTURE 9(9).
085300160229RF8633     03  DI-RECORD-COUNT-45      PICTURE 9(9).
085400160229RF8633     03  DI-RECORD-COUNT-90      PICTURE 9(9).
085500160229R17217     03  DI-CLIENT-CODE          PICTURE X(3).
085600160229R17217     03  DI-STMT-TYPE-IND        PICTURE X(1).
085700160229R18653     03  DI-RECORD-COUNT-17      PICTURE 9(9).
085800160229R20870     03  DI-RECORD-COUNT-33      PICTURE 9(9).
085900160229R20870     03  DI-RECORD-COUNT-38      PICTURE 9(9).
086000160229R35042     03  DI-RECORD-COUNT-21      PICTURE 9(9).
086100160229R34848     03  DI-RECORD-COUNT-81      PICTURE 9(9).
086200160229R34848*    03  FILLER                  PICTURE X(795).
086300160229R69515     03  DI-RECORD-COUNT-63      PICTURE 9(9).
086400160229117038     03  DI-RECORD-COUNT-22      PICTURE 9(9).
086500160229117038     03  FILLER                  PICTURE X(777).
086600160229117038*    03  FILLER                  PICTURE X(786).
086700160229           03  DI-SECTION-SEQUENCE-2   PICTURE 9(6).
086800160229           03  DI-START-DATE           PICTURE 9(08).
086900160229           03  DI-END-DATE             PICTURE 9(08).
087000160229           03  DI-EOR-INDICATOR        PICTURE X(1).
087100160229      /
087200160229       01  INVESTOR-INFO-SECTION.
087300160229
087400160229      ******************************************************************
087500160229      *                                                                *
087600160229      *    CPYJSISTMT: Statement File - Investor Information Record    *
087700160229      *    CREATED:    DD/MMM/YY (III)                                 *
087800160229      *    CHANGED:    09/JUN/98 (MFH)                                 *
087900160229      *    LENGTH:     750                                             *
088000160229      *                                                                *
088100160229      ******************************************************************
088200160229
088300160229      *****
088400160229      *****    - INVESTOR-NO will be formatted.
088500160229      *****    - ACCOUNT will always be zeroes.
088600160229      *****    - CECTION-NO will always be 01.
088700160229      *****    - SECTION-SEQUENCE will always be zeroes.
088800160229      *****    - LANGUAGE-CODE will be one of the following.
088900160229      *****        'E' English
089000160229      *****        'F' French
089100160229      *****    - DEALER will be formatted.
089200160229      *****    - DEALER-REP-CODE will be formatted.
089300160229      *****
089400160229
089500160229           03  II-INVESTOR-NO          PICTURE 9(9).
089600160229           03  II-ACCOUNT              PICTURE 9(9).
089700160229           03  II-SECTION-NO           PICTURE 9(2).
089800160229           03  II-SECTION-SEQUENCE     PICTURE 9(6).
089900160229           03  II-LANGUAGE-CODE        PICTURE X(1).
090000160229           03  II-DEALER               PICTURE X(4).
090100160229           03  II-DEALER-REP-CODE      PICTURE X(6).
090200160229           03  II-START-DATE-D         PICTURE 9(8).
090300160229           03  II-END-DATE-D           PICTURE 9(8).
090400160229           03  II-INVESTOR-NAME        PICTURE X(45).
090500160229           03  II-INVESTOR-NAME-2      PICTURE X(45).
090600160229           03  II-INVESTOR-ADDRESS1    PICTURE X(40).
090700160229           03  II-INVESTOR-ADDRESS2    PICTURE X(40).
090800160229           03  II-INVESTOR-ADDRESS3    PICTURE X(40).
090900160229           03  II-INVESTOR-ADDRESS4    PICTURE X(40).
091000160229           03  II-INVESTOR-ADDRESS5    PICTURE X(40).
091100160229           03  II-POSTAL-CODE          PICTURE X(10).
091200160229           03  II-HOLD-MAIL            PICTURE X(1).
091300160229           03  II-RESIDENCY            PICTURE X(1).
091400160229           03  II-SIN-NO               PICTURE 9(9).
091500160229           03  II-LEGAL-NAME           PICTURE X(35).
091600160229           03  II-FUND-986             PICTURE X(1).
091700160229           03  II-COUNTRY-CODE         PICTURE X(3).
091800160229           03  II-INVESTOR-TYPE        PICTURE X(25).
091900160229           03  II-INVESTOR-ATTRIBUTE-CODE
092000160229                                       PICTURE X(5).
092100160229           03  II-STATEMENT-FREQUENCY  PICTURE X(2).
092200160229           03  II-LAST-NAME            PICTURE X(20).
092300160229           03  II-FIRST-NAME           PICTURE X(20).
092400160229           03  II-INITIALS             PICTURE X(3).
092500160229           03  II-FUND-902             PICTURE X(1).
092600160229           03  II-DELIVERY-METHOD      PICTURE X(3).
092700160229           03  II-STAFF                PICTURE X(1).
092800160229           03  II-CREATION-DATE        PICTURE 9(8).
092900160229           03  II-NEW-INVESTOR-FLAG    PICTURE X(1).
093000160229           03  II-ACCOUNT2             PICTURE 9(9).
093100160229           03  II-SOLICITATION-CODE    PICTURE X(2).
093200160229           03  II-SALUTATION-CODE      PICTURE X(4).
093300160229R52815     03  II-IVR-Spouse-DOB       PICTURE 9(8).
093400160229R82793     03  II-INVESTOR-PROVINCE-CODE   PIC X(3).
093500160229R82793     03  II-INVESTOR-CITY        PICTURE X(30).
093600160229      * RFS 142884 STARTS
093700160229           03  II-INVESTOR-HOME-PH-ARCDE   PIC 9(3).
093800160229           03  II-INVESTOR-HOME-PHONE      PIC 9(7).
093900160229           03  II-INVESTOR-WORK-PH-ARCDE   PIC 9(3).
094000160229           03  II-INVESTOR-WORK-PHONE      PIC 9(7).
094100190731           03  II-INVESTOR-EXTN            PIC S9(10).
094200160229           03  II-INVESTOR-FAX             PIC X(60).
094300160229           03  II-INVESTOR-EMAIL           PIC X(60).
094400190625183097     03  II-TENANTNAME1              PIC X(45).
094500190625183097     03  II-TENANTNAME2              PIC X(45).
094600190625183097     03  II-TENANTNAME3              PIC X(45).
094700190625183097*    03  FILLER                  PICTURE X(629).
094800190625183097     03  FILLER                  PICTURE X(494).
094900160229R82793*    03  FILLER                  PICTURE X(779).
095000160229      * RFS 142884 ENDS
095100160229R52815*    03  FILLER                  PICTURE X(812).
095200160229R52815*    03  FILLER                  PICTURE X(820).
095300160229           03  II-SECTION-SEQUENCE-2   PICTURE 9(6).
095400160229           03  II-START-DATE           PICTURE 9(8).
095500160229           03  II-END-DATE             PICTURE 9(8).
095600160229           03  II-EOR-INDICATOR        PICTURE X(1).
095700160229      /
095800160229       01  DEALER-SECTION.
095900160229
096000160229      ******************************************************************
096100160229      *                                                                *
096200160229      *    CPYJSISTMT: Statement File - Dealer Record                  *
096300160229      *    CREATED:    DD/MMM/YY (III)                                 *
096400160229      *    CHANGED:    26/JUN/98
096500160229      *    LENGTH:     750                                             *
096600160229      *                                                                *
096700160229      ******************************************************************
096800160229
096900160229      *****
097000160229      *****    - INVESTOR will be formatted.
097100160229      *****    - ACCOUNT will always be zeroes.
097200160229      *****    - SECTION will always be 05.
097300160229      *****    - SECTION-SEQ will always be 000001.
097400160229      *****    - LANGUAGE will be one of the following.
097500160229      *****        'E' English
097600160229      *****        'F' French
097700160229      *****    - DEALER will be formatted.
097800160229      *****    - SALESREP will be formatted.
097900160229      *****
098000160229
098100160229           03  DL-INVESTOR             PICTURE 9(9).
098200160229           03  DL-ACCOUNT              PICTURE 9(9).
098300160229           03  DL-SECTION              PICTURE 9(2).
098400160229           03  DL-SECTION-SEQ          PICTURE 9(6).
098500160229           03  DL-LANGUAGE             PICTURE X(1).
098600160229           03  DL-DEALER               PICTURE X(4).
098700160229           03  DL-SALESREP             PICTURE X(6).
098800160229           03  DL-DEALER-NAME          PICTURE X(40).
098900160229           03  DL-REP-NAME             PICTURE X(40).
099000160229           03  DL-BRANCH-NAME          PICTURE X(40).
099100160229           03  DL-BRANCH-ADDR1         PICTURE X(40).
099200160229           03  DL-BRANCH-ADDR2         PICTURE X(40).
099300160229           03  DL-BRANCH-ADDR3         PICTURE X(40).
099400160229           03  DL-BRANCH-ADDR4         PICTURE X(40).
099500160229           03  DL-PHONE                PICTURE 9(10).
099600160229           03  DL-FAX                  PICTURE 9(10).
099700160229           03  DL-ADVISOR-TYPE         PICTURE X(3).
099800160229           03  DL-ACCOUNT2             PICTURE X(9).
099900160229           03  DL-DELIVERY-METHOD      PICTURE X(3).
100000160229           03  DL-DEALER-ADDR1         PICTURE X(40).
100100160229           03  DL-DEALER-ADDR2         PICTURE X(40).
100200160229           03  DL-DEALER-ADDR3         PICTURE X(40).
100300160229           03  DL-DEALER-ADDR4         PICTURE X(40).
100400160229           03  DL-BRANCH-PHONE         PICTURE 9(10).
100500160229           03  DL-NOMINEE-ADDR1        PICTURE X(40).
100600160229           03  DL-NOMINEE-ADDR2        PICTURE X(40).
100700160229           03  DL-NOMINEE-ADDR3        PICTURE X(40).
100800160229           03  DL-NOMINEE-ADDR4        PICTURE X(40).
100900160229           03  DL-BRANCH-CODE          PICTURE X(5).
101000160229           03  DL-DEALER-CATEGORY-CODE PICTURE X(4).
101100160229R11127     03  DL-REP-WHOLESALE-REGION PICTURE X(5).
101200160229R10901     03  DL-CONTACT-METHOD1      PICTURE X(10).
101300210315           03  DL-CONTACT-METHOD-DESP1 PICTURE X(40).
101400160229           03  DL-CONTACT-METHOD2      PICTURE X(10).
101500210315           03  DL-CONTACT-METHOD-DESP2 PICTURE X(40).
101600160229           03  DL-CONTACT-METHOD3      PICTURE X(10).
101700210315           03  DL-CONTACT-METHOD-DESP3 PICTURE X(40).
101800160229R17380     03  DL-BRANCH-CO-ATTN       PICTURE X(40).
101900160229  |        03  DL-DEALER-CO-ATTN       PICTURE X(40).
102000160229  |        03  DL-NOMINEE-CO-ATTN      PICTURE X(40).
102100160229      ** RFS43691 - Start
102200160229           03  DL-PROF-DES             PICTURE X(20).
102300160229      *RFS117038 - Start
102400160229  V   *    03  FILLER                  PICTURE X(341).
102500160229           03  DL-REP-STATUS           PICTURE X(1).
102600160229           03  DL-REP-EMAIL            PICTURE X(60).
102700160229           03  DL-REP-FIRST-NAME       PICTURE X(20).
102800160229           03  DL-REP-MIDDLE-NAME      PICTURE X(03).
102900160229           03  DL-REP-LAST-NAME        PICTURE X(20).
103000160229      *RFS148326 STARTS
103100160229      *    03  FILLER                  PICTURE X(237).
103200160229           03  DL-REP-ASST-NAME        PICTURE X(35).
103300160229           03  DL-REP-ASST-PHONE-NO    PICTURE 9(10).
103400191030      *RFS184366 STARTS
103500191030      *    03  FILLER                  PICTURE X(192).
103600200128      *RFS184872 - Starts
103700200128      *    03  DL-PRIMARY-NAME         PICTURE X(60).
103800200128           03  DL-PRIMARY-NAME         PICTURE X(100).
103900200128      *RFS184872 - Ends
104000191030           03  DL-INCORP-REP-IND       PICTURE X(01).
104100200128      *RFS184872 - Starts
104200200128      *    03  FILLER                  PICTURE X(131).
104300210315      *RFS1112345 - Starts
104400201110      *RFS1103499 - Starts
104500210315           03  FILLER                  PICTURE X(91).
104600210315      *    03  FILLER                  PICTURE X(31).
104700201110      *RFS1103499 - Ends
104800210315      *RFS1112345 - Ends
104900200128      *RFS184872 - Ends
105000191030      *RFS184366 END
105100160229      *RFS148326 END
105200160229      *RFS117038 - End
105300160229      ** RFS43691 - End
105400160229           03  DL-SECTION-SEQ-2        PICTURE 9(6).
105500160229           03  DL-START-DATE           PICTURE 9(08).
105600160229           03  DL-END-DATE             PICTURE 9(08).
105700160229           03  DL-EOR-INDICATOR        PICTURE X(1).
105800160229      /
105900160229       01  INVESTOR-SUMMARY-SECTION.
106000160229
106100160229      ******************************************************************
106200160229      *                                                                *
106300160229      *    CPYJSISTMT: Statement File - Investor Summary Record        *
106400160229      *    CREATED:    DD/MMM/YY (III)                                 *
106500160229      *    CHANGED:    15/DEC/97 (MFH)                                 *
106600160229      *    LENGTH:     1100                                            *
106700160229      *                                                                *
106800160229      ******************************************************************
106900160229
107000160229      *****
107100160229      *****    - INVESTOR-NO will be formatted.
107200160229      *****    - ACCOUNT will always be zeroes.
107300160229      *****    - SECTION-NO will always be 10.
107400160229      *****    - SECTION-SEQUENCE will always be zeroes.
107500160229      *****    - LANGUAGE will be one of the following.
107600160229      *****        'E' English
107700160229      *****        'F' French
107800160229      *****    - DEALER will be formatted.
107900160229      *****    - SALESREP will be formatted.
108000160229      *****
108100160229
108200160229           03  IS-INVESTOR-NO          PICTURE 9(9).
108300160229           03  IS-ACCOUNT              PICTURE 9(9).
108400160229           03  IS-SECTION-NO           PICTURE 9(2).
108500160229           03  IS-SECTION-SEQUENCE     PICTURE 9(6).
108600160229           03  IS-LANGUAGE             PICTURE X(1).
108700160229           03  IS-DEALER               PICTURE X(4).
108800160229           03  IS-SALESREP             PICTURE X(6).
108900160229           03  IS-ACCOUNT2             PICTURE 9(9).
109000160229           03  IS-ACCOUNT-TYPE-SHORT   PICTURE X(5).
109100160229           03  IS-ACCOUNT-TYPE         PICTURE X(35).
109200160229           03  IS-SPOUSAL              PICTURE X(1).
109300160229           03  IS-LOCKED-IN            PICTURE X(1).
109400160229           03  IS-NOMINEE              PICTURE X(1).
109500160229           03  IS-GROUP                PICTURE X(1).
109600160229           03  IS-DEALER-ACCOUNT-NO    PICTURE X(15).
109700160229           03  IS-NOMINEE-CODE         PICTURE X(5).
109800160229           03  IS-NOMINEE-NAME         PICTURE X(35).
109900160229           03  IS-NOMINEES-ACCT-NO     PICTURE X(15).
110000160229           03  IS-ACCOUNT-ATTRIBUTE-CODE
110100160229                                       PICTURE X(5).
110200160229           03  IS-ACCOUNT-BENEFICIARY  PICTURE X(45).
110300160229           03  IS-PROVINCE-OF-AGREEMENT
110400160229                                       PICTURE X(2).
110500160229           03  IS-CURRENCY             PICTURE X(3).
110600160229           03  IS-VALUE                PICTURE 9(11)V9(2).
110700160229           03  IS-VALUE-SIGN           PICTURE X(1).
110800160229           03  IS-FIRST60              PICTURE 9(9)V9(2).
110900160229           03  IS-FIRST60-SIGN         PICTURE X(1).
111000160229           03  IS-REMAINDER            PICTURE 9(9)V9(2).
111100160229           03  IS-REMAINDER-SIGN       PICTURE X(1).
111200160229           03  IS-WITHDRAWALS          PICTURE 9(9)V9(2).
111300160229           03  IS-WITHDRAWALS-SIGN     PICTURE X(1).
111400160229           03  IS-WITHDRAWALS-YTD      PICTURE 9(9)V9(2).
111500160229           03  IS-WITHDRAWALS-YTD-SIGN PICTURE X(1).
111600160229           03  IS-PUR-INCEPT           PICTURE 9(9)V9(2).
111700160229           03  IS-PI-SIGN              PICTURE X(1).
111800160229           03  IS-PUR-CURRENT          PICTURE 9(9)V9(2).
111900160229           03  IS-PC-SIGN              PICTURE X(1).
112000160229           03  IS-PUR-YTD              PICTURE 9(9)V9(2).
112100160229           03  IS-PUR-YTD-SIGN         PICTURE X(1).
112200160229           03  IS-RED-INCEPT           PICTURE 9(9)V9(2).
112300160229           03  IS-RI-SIGN              PICTURE X(1).
112400160229           03  IS-RED-CURRENT          PICTURE 9(9)V9(2).
112500160229           03  IS-RC-SIGN              PICTURE X(1).
112600160229           03  IS-RED-YTD              PICTURE 9(9)V9(2).
112700160229           03  IS-RED-YTD-SIGN         PICTURE X(1).
112800160229           03  IS-CPG-INCEPT           PICTURE 9(9)V9(2).
112900160229           03  IS-CI-SIGN              PICTURE X(1).
113000160229           03  IS-CPG-CURRENT          PICTURE 9(9)V9(2).
113100160229           03  IS-CC-SIGN              PICTURE X(1).
113200160229           03  IS-CPG-YTD              PICTURE 9(9)V9(2).
113300160229           03  IS-CPG-YTD-SIGN         PICTURE X(1).
113400160229           03  IS-INC-INCEPT           PICTURE 9(9)V9(2).
113500160229           03  IS-II-SIGN              PICTURE X(1).
113600160229           03  IS-INC-CURRENT          PICTURE 9(9)V9(2).
113700160229           03  IS-IC-SIGN              PICTURE X(1).
113800160229           03  IS-INC-YTD              PICTURE 9(9)V9(2).
113900160229           03  IS-INC-YTD-SIGN         PICTURE X(1).
114000160229           03  IS-INT-INCEPT           PICTURE 9(9)V9(2).
114100160229           03  IS-NI-SIGN              PICTURE X(1).
114200160229           03  IS-INT-CURRENT          PICTURE 9(9)V9(2).
114300160229           03  IS-NC-SIGN              PICTURE X(1).
114400160229           03  IS-INT-YTD              PICTURE 9(9)V9(2).
114500160229           03  IS-INT-YTD-SIGN         PICTURE X(1).
114600160229           03  IS-REG-CTB-CUR          PICTURE 9(9)V9(2).
114700160229           03  IS-REG-CTB-CUR-SIGN     PICTURE X(1).
114800160229           03  IS-REG-CTB-INCPT        PICTURE 9(9)V9(2).
114900160229           03  IS-REG-CTB-INCPT-SIGN   PICTURE X(1).
115000160229           03  IS-FOREIGN-CONTENT      PICTURE 9(3)V9(2).
115100160229           03  IS-CDN-CONTENT          PICTURE 9(3)V9(2).
115200160229           03  IS-BOOK-VALUE-CDN       PICTURE 9(11)V9(2).
115300160229           03  IS-BOOK-VALUE-CDN-SIGN  PICTURE X(1).
115400160229           03  IS-BOOK-VALUE-FGN       PICTURE 9(11)V9(2).
115500160229           03  IS-BOOK-VALUE-FGN-SIGN  PICTURE X(1).
115600160229           03  IS-MKT-VALUE-CHG-INC    PICTURE 9(11)V9(2).
115700160229           03  IS-MKT-VALUE-CHG-INC-SIGN
115800160229                                       PICTURE X(1).
115900160229           03  IS-MKT-VALUE-CHG-CUR    PICTURE 9(9)V9(2).
116000160229           03  IS-MKT-VALUE-CHG-CUR-SIGN
116100160229                                       PICTURE X(1).
116200160229           03  IS-INVESTOR-PERC        PICTURE 9(3)V9(2).
116300160229           03  IS-12MO-ROR             PICTURE 9(3)V9(2).
116400160229           03  IS-12MO-ROR-SIGN        PICTURE X(1).
116500160229           03  IS-WHTAX-YTD            PICTURE 9(9)V9(2).
116600160229           03  IS-WHTAX-YTD-SIGN       PICTURE X(1).
116700160229           03  IS-WHTAX-CURRENT        PICTURE 9(9)V9(2).
116800160229           03  IS-WHTAX-CURRENT-SIGN   PICTURE X(1).
116900160229           03  IS-NRTAX-YTD            PICTURE 9(9)V9(2).
117000160229           03  IS-NRTAX-YTD-SIGN       PICTURE X(1).
117100160229           03  IS-NRTAX-CUR            PICTURE 9(9)V9(2).
117200160229           03  IS-NRTAX-CUR-SIGN       PICTURE X(1).
117300160229           03  IS-NRTAX-INCPT          PICTURE 9(9)V9(2).
117400160229           03  IS-NRTAX-INCPT-SIGN     PICTURE X(1).
117500160229           03  IS-PRINT-STATEMENT      PICTURE X(1).
117600160229           03  IS-OPENING-MARKET-VALUE PICTURE 9(11)V9(2).
117700160229           03  IS-COUNTRY-CODE         PICTURE X(3).
117800160229           03  IS-CREATION-DATE        PICTURE 9(8).
117900160229           03  IS-NEW-ACCOUNT-INDICATOR
118000160229                                       PICTURE X(1).
118100160229           03  IS-ORIGINAL-COST            PIC 9(9)V9(2).
118200160229           03  IS-TOTAL-CONTRIBUTIONS-YTD  PIC 9(9)V9(2).
118300160229           03  IS-TRI-INCEPT               PIC 9(9)V9(2).
118400160229R12173     03  IS-TRI-INCEPT-SIGN          PIC X(1).
118500160229           03  IS-TRO-INCEPT               PIC 9(9)V9(2).
118600160229R12173     03  IS-TRO-INCEPT-SIGN          PIC X(1).
118700160229           03  IS-ENTERED-BY               PICTURE X(10).
118800160229           03  IS-LEVEL-OF-SERVICE         PICTURE X(3).
118900160229           03  IS-MARKET-VALUE-LAST-YE     PICTURE 9(11)V9(2).
119000160229           03  IS-MARKET-VALUE-LAST-YE-SIGN   PICTURE X.
119100160229           03  IS-MAILING-ADDRESS-INDICATOR   PICTURE X.
119200160229           03  IS-SWI-INCEPT               PIC 9(9)V9(2).
119300160229R12173     03  IS-SWI-INCEPT-SIGN          PIC X(1).
119400160229           03  IS-SWO-INCEPT               PIC 9(9)V9(2).
119500160229R12173     03  IS-SWO-INCEPT-SIGN          PIC X(1).
119600160229           03  IS-DISCRETIONARY-FEE-FUND   PIC X(5).
119700160229           03  IS-DISCRETIONARY-FEE-RATE   PIC 9(3)V9(2).
119800160229RF8113     03  IS-SWI-CURRENT              PIC 9(9)V9(2).
119900160229R12173     03  IS-SWI-CURRENT-SIGN         PIC X(1).
120000160229           03  IS-SWO-CURRENT              PIC 9(9)V9(2).
120100160229R12173     03  IS-SWO-CURRENT-SIGN         PIC X(1).
120200160229RF8113     03  IS-TRI-CURRENT              PIC 9(9)V9(2).
120300160229R12173     03  IS-TRI-CURRENT-SIGN         PIC X(1).
120400160229           03  IS-TRO-CURRENT              PIC 9(9)V9(2).
120500160229R12173     03  IS-TRO-CURRENT-SIGN         PIC X(1).
120600160229RF8633     03  IS-TRI-YTD                  PIC 9(9)V9(2).
120700160229           03  IS-TRI-YTD-SIGN             PIC X(1).
120800160229           03  IS-TRO-YTD                  PIC 9(9)V9(2).
120900160229           03  IS-TRO-YTD-SIGN             PIC X(1).
121000160229           03  IS-SWI-YTD                  PIC 9(9)V9(2).
121100160229           03  IS-SWI-YTD-SIGN             PIC X(1).
121200160229           03  IS-SWO-YTD                  PIC 9(9)V9(2).
121300160229           03  IS-SWO-YTD-SIGN             PIC X(1).
121400160229           03  IS-PROFESSIONAL-FEE-INCEPT  PIC 9(9)V9(2).
121500160229           03  IS-PROFESSIONAL-FEE-INCEPT-S
121600160229                                           PIC X(1).
121700160229           03  IS-PROFESSIONAL-FEE-YTD     PIC 9(9)V9(2).
121800160229           03  IS-PROFESSIONAL-FEE-YTD-SIGN
121900160229RF8633                                     PIC X(1).
122000160229           03  IS-PROFESSIONAL-FEE-CUR     PIC 9(9)V9(2).
122100160229           03  IS-PROFESSIONAL-FEE-CUR-SIGN
122200160229                                           PIC X(1).
122300160229           03  IS-MAILING-ADDRESS-INDICATOR2  PICTURE X.
122400160229           03  IS-INVESTMENT-STRUCTURE-CODE   PICTURE X(5).
122500160229R10605     03  IS-DATE-OF-BIRTH            PICTURE 9(8).
122600160229R13756     03  IS-MASTER-ACCOUNT-NO        PICTURE 9(9).
122700160229           03  IS-TRI-INCEPT-COST          PIC 9(9)V9(2).
122800160229           03  IS-TRI-INCEPT-COST-SIGN     PIC X(1).
122900160229           03  IS-TRO-INCEPT-COST          PIC 9(9)V9(2).
123000160229           03  IS-TRO-INCEPT-COST-SIGN     PIC X(1).
123100160229R15022     03  IS-DISP-GAIN-LOSS-CUR         PICTURE 9(11)V9(2).
123200160229  ¦        03  IS-DISP-GAIN-LOSS-CUR-SIGN    PICTURE X(1).
123300160229  ¦        03  IS-DISP-GAIN-LOSS-YTD         PICTURE 9(11)V9(2).
123400160229  ¦        03  IS-DISP-GAIN-LOSS-YTD-SIGN    PICTURE X(1).
123500160229  ¦        03  IS-DISP-GAIN-LOSS-INCEPT      PICTURE 9(11)V9(2).
123600160229  ¦        03  IS-DISP-GAIN-LOSS-INCEPT-SIGN PICTURE X(1).
123700160229R16990     03  IS-NET-ADD-CURRENT            PICTURE 9(11)V9(2).
123800160229  ¦        03  IS-NET-ADD-CURRENT-SIGN       PICTURE X(1).
123900160229  ¦        03  IS-NET-WITHDRAW-CURRENT       PICTURE 9(11)V9(2).
124000160229  ¦        03  IS-NET-WITHDRAW-CURRENT-SIGN  PICTURE X(1).
124100160229  ¦        03  IS-DISTRIBUTION-CURRENT       PICTURE 9(11)V9(2).
124200160229  ¦        03  IS-DISTRIBUTION-CURRENT-SIGN  PICTURE X(1).
124300160229  ¦        03  IS-UNREAL-GAIN-LOSS-CURR      PICTURE 9(11)V9(2).
124400160229  ¦        03  IS-UNREAL-GAIN-LOSS-CURR-SIGN PICTURE X(1).
124500160229  V        03  IS-NET-INV-CAPTAL-INCEPT      PICTURE 9(11)V9(2).
124600160229R16990     03  IS-NET-INV-CAPTAL-INCEPT-SIGN PICTURE X(1).
124700160229           03  IS-TRI-INCEPT-MKT           PIC 9(9)V9(2).
124800160229           03  IS-TRI-INCEPT-MKT-SIGN      PIC X(1).
124900160229           03  IS-TRO-INCEPT-MKT           PIC 9(9)V9(2).
125000160229           03  IS-TRO-INCEPT-MKT-SIGN      PIC X(1).
125100160229R16419     03  IS-USERID                   PIC X(10).
125200160229R16881     03  IS-TRI-INCEPT-PHANTOM         PICTURE 9(9)V9(2).
125300160229  ¦        03  IS-TRI-INCEPT-PHANTOM-SIGN    PICTURE X(01).
125400160229  ¦        03  IS-TRO-INCEPT-PHANTOM         PICTURE 9(9)V9(2).
125500160229  ¦        03  IS-TRO-INCEPT-PHANTOM-SIGN    PICTURE X(01).
125600160229  ¦        03  IS-TRI-YTD-PHANTOM            PICTURE 9(9)V9(2).
125700160229  ¦        03  IS-TRI-YTD-PHANTOM-SIGN       PICTURE X(01).
125800160229  ¦        03  IS-TRO-YTD-PHANTOM            PICTURE 9(9)V9(2).
125900160229  ¦        03  IS-TRO-YTD-PHANTOM-SIGN       PICTURE X(01).
126000160229  ¦        03  IS-TRI-CUR-PHANTOM            PICTURE 9(9)V9(2).
126100160229  ¦        03  IS-TRI-CUR-PHANTOM-SIGN       PICTURE X(01).
126200160229  ¦        03  IS-TRO-CUR-PHANTOM            PICTURE 9(9)V9(2).
126300160229  V        03  IS-TRO-CUR-PHANTOM-SIGN       PICTURE X(01).
126400160229R18682     03  IS-FEES                       PICTURE 9(11)V9(2).
126500160229R18682     03  IS-FEES-SIGN                  PICTURE X(1).
126600160229  ¦        03  IS-REALIZED-GAIN-LOSS         PICTURE 9(11)V9(2).
126700160229  ¦        03  IS-REALIZED-GAIN-LOSS-SIGN    PICTURE X(1).
126800160229  ¦        03  IS-UNREALIZED-GAIN-LOSS       PICTURE 9(11)V9(2).
126900160229  ¦        03  IS-UNREALIZED-GAIN-LOSS-SIGN  PICTURE X(1).
127000160229  ¦        03  IS-ACCRUED-INCOME             PICTURE 9(11)V9(2).
127100160229  ¦        03  IS-ACCRUED-INCOME-SIGN        PICTURE X(1).
127200160229R17527     03  IS-IPS-GROUP-ID               PICTURE X(20).
127300160229R18515     03  IS-FIRST-TRADE-DATE           PICTURE 9(8).
127400160229R21714     03  IS-CLTSTCLT-FLAG              PICTURE X(1).
127500160229R21714     03  IS-CLTSTREP-FLAG              PICTURE X(1).
127600160229R21714     03  IS-CLTSTSPR-FLAG              PICTURE X(1).
127700160229R21714     03  IS-NOMREPSP-FLAG              PICTURE X(1).
127800160229R33331     03  IS-GAIN-LOSS-PD-OUT           PICTURE 9(11)V9(2).
127900160229  ¦        03  IS-GAIN-LOSS-PD-OUT-SIGN      PICTURE X(1).
128000160229           03  IS-MULTIPLE-CONTRACT-FLAG     PICTURE X(1).
128100160229           03  IS-JOINT-LIFE-FLAG            PICTURE X(1).
128200160229R61356*    03  IS-LTD-CONTRIBUTION           PICTURE 9(11)V9(2).
128300160229  |   *    03  IS-LTD-CONTRIBUTION-SIGN      PICTURE X(1).
128400160229  |   *    03  IS-LTD-WITHDRAWAL             PICTURE 9(11)V9(2).
128500160229  |   *    03  IS-LTD-WITHDRAWAL-SIGN        PICTURE X(1).
128600160229  |   *    03  IS-YTD-CONTRIBUTION           PICTURE 9(11)V9(2).
128700160229  |   *    03  IS-YTD-CONTRIBUTION-SIGN      PICTURE X(1).
128800160229  |   *    03  IS-YTD-WITHDRAWAL             PICTURE 9(11)V9(2).
128900160229  V   *    03  IS-YTD-WITHDRAWAL-SIGN        PICTURE X(1).
129000160229R69135     03  IS-YTD-CONTRIBUTION           PICTURE 9(11)V9(2).
129100160229  |        03  IS-YTD-CONTRIBUTION-SIGN      PICTURE X(1).
129200160229  |        03  IS-LTD-CONTRIBUTION           PICTURE 9(11)V9(2).
129300160229  |        03  IS-LTD-CONTRIBUTION-SIGN      PICTURE X(1).
129400160229  |        03  IS-YTD-WITHDRAWAL             PICTURE 9(11)V9(2).
129500160229  |        03  IS-YTD-WITHDRAWAL-SIGN        PICTURE X(1).
129600160229  |        03  IS-LTD-WITHDRAWAL             PICTURE 9(11)V9(2).
129700160229R69135     03  IS-LTD-WITHDRAWAL-SIGN        PICTURE X(1).
129800160229R61356*93662  03  FILLER                        PICTURE X(20).
129900160229      *RFS93662 - Begin
130000160229           03  IS-OMS-ACCOUNT-TYPE-CODE    PICTURE X(5).
130100160229           03  IS-OMS-NOMINEE-TYPE-CODE    PICTURE X(5).
130200160229           03  IS-OMS-NOMINEE              PICTURE X(1).
130300160229           03  IS-OMS-GROUP                PICTURE X(1).
130400160229           03  IS-OMS-SPOUSAL              PICTURE X(1).
130500160229           03  IS-OMS-LOCKED-IN            PICTURE X(1).
130600160229           03  IS-OMS-JOINT                PICTURE X(1).
130700160229           03  IS-OMS-CMPCDE               PICTURE X(3).
130800160229      * RFS115161 - Start
130900160229           03  IS-LOCKED                   PICTURE X(1).
131000170512      *RFS145044 - START
131100170512      *    03  FILLER                      PICTURE X(1).
131200170512           03  IS-DELIVERY-OPTION          PICTURE X(1).
131300170512      *RFS145044 - END
131400160229      *    03  FILLER                      PICTURE X(2).
131500160229      * RFS115161 - End
131600160229      *RFS93662 - End
131700160229           03  IS-SECTION-SEQUENCE-2       PICTURE 9(6).
131800160229           03  IS-START-DATE               PICTURE 9(08).
131900160229           03  IS-END-DATE                 PICTURE 9(08).
132000160229           03  IS-EOR-INDICATOR            PICTURE X(1).
132100160229
132200160229      /
132300160229       01 INVESTOR-SUMMARY-SECTION-2.
132400160229
132500160229      ****************************************************************
132600160229      *
132700160229      *    CPYJSISTMT: Statement File - Investor Summary Section-2
132800160229      *                                 Record
132900160229      *    CREATED:    27/FEB/2001
133000160229      *    LENGTH:     850
133100160229      *
133200160229      ****************************************************************
133300160229
133400160229      *****
133500160229      *****    - INVESTOR-NO will be formatted.
133600160229      *****    - ACCOUNT will be formatted.
133700160229      *****    - SECTION-NO will always be 12.
133800160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
133900160229      *****    - LANGUAGE will always be formatted.
134000160229      *****    - DEALER will always be formatted.
134100160229      *****    - SALESREP will always be formatted.
134200160229      *****
134300160229
134400160229           03  IS2-INVESTOR-NO            PICTURE 9(9).
134500160229           03  IS2-ACCOUNT                PICTURE 9(9).
134600160229           03  IS2-SECTION-NO             PICTURE 9(2).
134700160229           03  IS2-SECTION-SEQUENCE       PICTURE 9(6).
134800160229           03  IS2-LANGUAGE               PICTURE X(1).
134900160229           03  IS2-DEALER                 PICTURE X(4).
135000160229           03  IS2-SALESREP               PICTURE X(6).
135100160229           03  IS2-ACCOUNT2               PICTURE 9(9).
135200160229           03  IS2-INT-INCEPT-CASH        PICTURE 9(9)V9(2).
135300160229           03  IS2-INT-INCEPT-CASH-SIGN   PICTURE X(1).
135400160229           03  IS2-INT-YTD-CASH           PICTURE 9(9)V9(2).
135500160229           03  IS2-INT-YTD-CASH-SIGN      PICTURE X(1).
135600160229           03  IS2-INT-CURRENT-CASH       PICTURE 9(9)V9(2).
135700160229           03  IS2-INT-CURRENT-CASH-SIGN  PICTURE X(1).
135800160229           03  IS2-INT-INCEPT-REINV       PICTURE 9(9)V9(2).
135900160229           03  IS2-INT-INCEPT-REINV-SIGN  PICTURE X(1).
136000160229           03  IS2-INT-YTD-REINV          PICTURE 9(9)V9(2).
136100160229           03  IS2-INT-YTD-REINV-SIGN     PICTURE X(1).
136200160229           03  IS2-INT-CURRENT-REINV      PICTURE 9(9)V9(2).
136300160229           03  IS2-INT-CURRENT-REINV-SIGN PICTURE X(1).
136400160229           03  IS2-CPG-INCEPT-CASH        PICTURE 9(9)V9(2).
136500160229           03  IS2-CPG-INCEPT-CASH-SIGN   PICTURE X(1).
136600160229           03  IS2-CPG-YTD-CASH           PICTURE 9(9)V9(2).
136700160229           03  IS2-CPG-YTD-CASH-SIGN      PICTURE X(1).
136800160229           03  IS2-CPG-CURRENT-CASH       PICTURE 9(9)V9(2).
136900160229           03  IS2-CPG-CURRENT-CASH-SIGN  PICTURE X(1).
137000160229           03  IS2-CPG-INCEPT-REINV       PICTURE 9(9)V9(2).
137100160229           03  IS2-CPG-INCEPT-REINV-SIGN  PICTURE X(1).
137200160229           03  IS2-CPG-YTD-REINV          PICTURE 9(9)V9(2).
137300160229           03  IS2-CPG-YTD-REINV-SIGN     PICTURE X(1).
137400160229           03  IS2-CPG-CURRENT-REINV      PICTURE 9(9)V9(2).
137500160229           03  IS2-CPG-CURRENT-REINV-SIGN PICTURE X(1).
137600160229           03  IS2-INC-INCEPT-CASH        PICTURE 9(9)V9(2).
137700160229           03  IS2-INC-INCEPT-CASH-SIGN   PICTURE X(1).
137800160229           03  IS2-INC-YTD-CASH           PICTURE 9(9)V9(2).
137900160229           03  IS2-INC-YTD-CASH-SIGN      PICTURE X(1).
138000160229           03  IS2-INC-CURRENT-CASH       PICTURE 9(9)V9(2).
138100160229           03  IS2-INC-CURRENT-CASH-SIGN  PICTURE X(1).
138200160229           03  IS2-INC-INCEPT-REINV       PICTURE 9(9)V9(2).
138300160229           03  IS2-INC-INCEPT-REINV-SIGN  PICTURE X(1).
138400160229           03  IS2-INC-YTD-REINV          PICTURE 9(9)V9(2).
138500160229           03  IS2-INC-YTD-REINV-SIGN     PICTURE X(1).
138600160229           03  IS2-INC-CURRENT-REINV      PICTURE 9(9)V9(2).
138700160229           03  IS2-INC-CURRENT-REINV-SIGN PICTURE X(1).
138800160229R14136     03  IS2-MFR-CUR                PICTURE 9(9)V9(2).
138900160229  ¦        03  IS2-MFR-CUR-SIGN           PICTURE X(1).
139000160229  ¦        03  IS2-MFR-YTD                PICTURE 9(9)V9(2).
139100160229  ¦        03  IS2-MFR-YTD-SIGN           PICTURE X(1).
139200160229  ¦        03  IS2-MFR-INCEPT             PICTURE 9(9)V9(2).
139300160229  ¦        03  IS2-MFR-INCEPT-SIGN        PICTURE X(1).
139400160229  ¦        03  IS2-FED-WHTAX-CUR          PICTURE 9(9)V9(2).
139500160229  ¦        03  IS2-FED-WHTAX-CUR-SIGN     PICTURE X(1).
139600160229  ¦        03  IS2-FED-WHTAX-YTD          PICTURE 9(9)V9(2).
139700160229  ¦        03  IS2-FED-WHTAX-YTD-SIGN     PICTURE X(1).
139800160229  ¦        03  IS2-PROV-WHTAX-CUR         PICTURE 9(9)V9(2).
139900160229  ¦        03  IS2-PROV-WHTAX-CUR-SIGN    PICTURE X(1).
140000160229  ¦        03  IS2-PROV-WHTAX-YTD         PICTURE 9(9)V9(2).
140100160229R14136     03  IS2-PROV-WHTAX-YTD-SIGN    PICTURE X(1).
140200160229R14858     03  IS2-BAJ-CUR                PICTURE 9(9)V9(2).
140300160229  ¦        03  IS2-BAJ-CUR-SIGN           PICTURE X(1).
140400160229  ¦        03  IS2-BAJ-YTD                PICTURE 9(9)V9(2).
140500160229  ¦        03  IS2-BAJ-YTD-SIGN           PICTURE X(1).
140600160229  ¦        03  IS2-BAJ-INCEPT             PICTURE 9(9)V9(2).
140700160229  ¦        03  IS2-BAJ-INCEPT-SIGN        PICTURE X(1).
140800160229  ¦        03  IS2-BFW-CUR                PICTURE 9(9)V9(2).
140900160229  ¦        03  IS2-BFW-CUR-SIGN           PICTURE X(1).
141000160229  ¦        03  IS2-BFW-YTD                PICTURE 9(9)V9(2).
141100160229  ¦        03  IS2-BFW-YTD-SIGN           PICTURE X(1).
141200160229  ¦        03  IS2-BFW-INCEPT             PICTURE 9(9)V9(2).
141300160229R14858     03  IS2-BFW-INCEPT-SIGN        PICTURE X(1).
141400160229R20758     03  IS2-ROC-CUR                PICTURE 9(9)V9(2).
141500160229  ¦        03  IS2-ROC-CUR-SIGN           PICTURE X(1).
141600160229  ¦        03  IS2-ROC-YTD                PICTURE 9(9)V9(2).
141700160229  ¦        03  IS2-ROC-YTD-SIGN           PICTURE X(1).
141800160229  ¦        03  IS2-ROC-INCEPT             PICTURE 9(9)V9(2).
141900160229R20758     03  IS2-ROC-INCEPT-SIGN        PICTURE X(1).
142000160229      *RFS130239 START
142100160229R20758*    03  FILLER                     PICTURE X(873).
142200160229           03  IS2-LTD-SUM-PUR-TRI-INCEP  PICTURE 9(9)V9(2).
142300160229           03  IS2-LTD-SUM-PUR-TRI-INCEP-SIGN
142400160229                                          PICTURE X(1).
142500160229           03  IS2-LTD-SUM-RED-TRO-INCEP  PICTURE 9(9)V9(2).
142600160229           03  IS2-LTD-SUM-RED-TRO-INCEP-SIGN
142700160229                                          PICTURE X(1).
142800160229      *RFS139185 - BEGINS *
142900160229      *    03  FILLER                     PICTURE X(849).
143000160229           03  IS2-MKT-VALUE-CHG-INCEPT   PICTURE 9(11)V9(2).
143100160229           03  IS2-MKT-VALUE-CHG-INCEPT-SIGN
143200160229                                          PICTURE X(1).
143300160229      *RFS148326 - BEGINS *
143400160229      *    03  FILLER                     PICTURE X(835).
143500160229           03  IS2-KYC-INVESTMENT-OBJECTIVE
143600160229                                          PICTURE X(25).
143700160229           03  IS2-KYC-TIME-HORIZON       PICTURE X(25).
143800160229           03  IS2-KYC-RISK-TOLERANCE     PICTURE X(25).
143900160229           03  IS2-MONEY-IN               PICTURE 9(11)V9(2).
144000160229           03  IS2-MONEY-IN-SIGN          PICTURE X(1).
144100160229           03  IS2-MONEY-IN-SINCE-INCEPTION
144200160229                                          PICTURE 9(11)V9(2).
144300160229           03  IS2-MONEY-IN-SINCE-INCEP-SIGN
144400160229                                          PICTURE X(1).
144500160229           03  IS2-MONEY-OUT              PICTURE 9(11)V9(2).
144600160229           03  IS2-MONEY-OUT-SIGN         PICTURE X(1).
144700160229           03  IS2-MONEY-OUT-SINCE-INCEPTION
144800160229                                          PICTURE 9(11)V9(2).
144900160229           03  IS2-MONEY-OUT-SINCE-INCEP-SIGN
145000160229                                          PICTURE X(1).
145100160229           03  IS2-DAY1-OPENING-VALUE     PICTURE 9(11)V9(2).
145200160229           03  IS2-DAY1-OPENING-VALUE-SIGN
145300160229                                          PICTURE X(1).
145400160229           03  IS2-INV-INCREASE-VALUE     PICTURE 9(11)V9(2).
145500160229           03  IS2-INV-INCREASE-VALUE-SIGN
145600160229                                          PICTURE X(1).
145700160229           03  IS2-INV-INC-VALUE-SINCE-INCEP
145800160229                                          PICTURE 9(11)V9(2).
145900160229           03  IS2-INV-INC-VALUE-SINCE-SIGN
146000160229                                          PICTURE X(1).
146100160229           03  IS2-ACC-WEIGHTED-MER       PICTURE 9(3)V9(2).
146200160229           03  IS2-ACC-WEIGHTED-MER-SIGN  PICTURE X(1).
146300160229           03  IS2-ACC-CHANGE-IN-VALUE    PICTURE 9(11)V9(2).
146400160229           03  IS2-ACC-CHANGE-IN-VALUE-SIGN
146500160229                                          PICTURE X(1).
146501211019      *RFS186592 Starts*
146502211019      *    03  FILLER                     PICTURE X(642).
146503211019           03  IS2-GUAR-RESET-OPTION-CODE PICTURE X(1).
146504211019           03  FILLER                     PICTURE X(641).
146505211019      *RFS186592 Ends*
146700160229      *RFS148326 - ENDS *
146800160229      *RFS139185 - ENDS *
146900160229
147000160229      *RFS130239 END
147100160229           03  IS2-SECTION-SEQUENCE-2     PICTURE 9(6).
147200160229           03  IS2-START-DATE             PICTURE 9(8).
147300160229           03  IS2-END-DATE               PICTURE 9(8).
147400160229           03  IS2-EOR-INDICATOR          PICTURE X(1).
147500160229
147600160229      /
147700160229       01  ACCOUNT-ASSET-CLASS-SECTION.
147800160229
147900160229      ******************************************************************
148000160229      *                                                                *
148100160229      *    CPYJSISTMT: Statement File - Account Asset Class Summary    *
148200160229      *                                 Record                         *
148300160229      *    CREATED:    17/APR/97 (MFH)                                 *
148400160229      *    CHANGED:    DD/MMM/YY (III)                                 *
148500160229      *    LENGTH:     750                                             *
148600160229      *                                                                *
148700160229      ******************************************************************
148800160229
148900160229      *****
149000160229      *****    - INVESTOR-NO will be formatted.
149100160229      *****    - ACCOUNT will be formatted.
149200160229      *****    - SECTION-NO will always be 15.
149300160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
149400160229      *****                       999999 will be reserved for the total.
149500160229      *****    - LANGUAGE will be one of the following.
149600160229      *****        'E' English
149700160229      *****        'F' French
149800160229      *****    - DEALER will be formatted.
149900160229      *****    - SALESREP will be formatted.
150000160229      *****
150100160229
150200160229           03  AC-INVESTOR-NO          PICTURE 9(9).
150300160229           03  AC-ACCOUNT              PICTURE 9(9).
150400160229           03  AC-SECTION-NO           PICTURE 9(2).
150500160229           03  AC-SECTION-SEQUENCE     PICTURE 9(6).
150600160229           03  AC-LANGUAGE             PICTURE X(1).
150700160229           03  AC-DEALER               PICTURE X(4).
150800160229           03  AC-SALESREP             PICTURE X(6).
150900160229           03  AC-ASSET-CLASS-CODE     PICTURE X(6).
151000160229           03  AC-ASSET-CLASS-DESCRIPTION
151100160229                                       PICTURE X(25).
151200160229           03  AC-ASSET-SUBCLASS-CODE  PICTURE X(6).
151300160229           03  AC-ASSET-SUBCLASS-DESCRIPTION
151400160229                                       PICTURE X(25).
151500160229           03  AC-MARKET-VALUE         PICTURE 9(11)V9(2).
151600160229           03  AC-PERCENTAGE           PICTURE 9(3)V9(4).
151700160229R16511     03  AC-TARGET-MIX-PERC      PICTURE 9(5)V9(2).
151800160229R17527     03  AC-IPS-GROUP-ID         PICTURE X(20).
151900160614156335     03  AC-BOOK-VALUE           PICTURE 9(11)V9(2).
152000160614156335     03  FILLER                  PICTURE X(1168).
152100160614156335*R16511     03  FILLER                  PICTURE X(1181).
152200160229           03  AC-SECTION-SEQUENCE-2   PICTURE 9(6).
152300160229           03  AC-START-DATE           PICTURE 9(08).
152400160229           03  AC-END-DATE             PICTURE 9(08).
152500160229           03  AC-EOR-INDICATOR        PICTURE X(1).
152600160229      /
152700160229       01  ACCOUNT-IPS-DETAILS.
152800160229
152900160229      ******************************************************************
153000160229      *                                                                *
153100160229      *    CPYJSISTMT: Statement File - Account IPS Record             *
153200160229      *    CREATED:    18/Aug/03 (MFH)                                 *
153300160229      *    CHANGED:    DD/MMM/YY (III)                                 *
153400160229      *    LENGTH:     1350                                            *
153500160229      *                                                                *
153600160229      ******************************************************************
153700160229
153800160229      *****
153900160229      *****    - INVESTOR-NO will be formatted.
154000160229      *****    - ACCOUNT will be formatted.
154100160229      *****    - SECTION-NO will always be 17.
154200160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
154300160229      *****                       999999 will be reserved for the total.
154400160229      *****    - LANGUAGE will be one of the following.
154500160229      *****        'E' English
154600160229      *****        'F' French
154700160229      *****    - DEALER will be formatted.
154800160229      *****    - SALESREP will be formatted.
154900160229      *****
155000160229
155100160229           03  IPS-INVESTOR-NO          PICTURE 9(9).
155200160229           03  IPS-ACCOUNT              PICTURE 9(9).
155300160229           03  IPS-SECTION-NO           PICTURE 9(2).
155400160229           03  IPS-SECTION-SEQUENCE     PICTURE 9(6).
155500160229           03  IPS-LANGUAGE             PICTURE X(1).
155600160229           03  IPS-DEALER               PICTURE X(4).
155700160229           03  IPS-SALESREP             PICTURE X(6).
155800160229           03  IPS-GROUP-ID             PICTURE X(20).
155900160229           03  IPS-GROUP-NAME           PICTURE X(80).
156000160229           03  IPS-EFFECTIVE-DATE       PICTURE 9(8).
156100160229           03  IPS-DEALER-NAME          PICTURE X(35).
156200160229           03  IPS-REP-FIRST-NAME       PICTURE X(20).
156300160229           03  IPS-REP-LAST-NAME        PICTURE X(20).
156400160229           03  IPS-REP-ADDR1            PICTURE X(40).
156500160229           03  IPS-REP-ADDR2            PICTURE X(40).
156600160229           03  IPS-REP-ADDR3            PICTURE X(40).
156700160229           03  IPS-REP-ADDR4            PICTURE X(40).
156800160229           03  IPS-REP-ADDR5            PICTURE X(40).
156900160229           03  IPS-REP-POSTAL-CODE      PICTURE X(12).
157000160229           03  IPS-REP-COUNTRY-CODE     PICTURE X(3).
157100160229           03  IPS-MAILING-ACCOUNT-NO   PICTURE 9(9).
157200160229R18667     03  IPS-REP-PHONE-AREA-CODE  PICTURE 9(3).
157300160229   ¦       03  IPS-REP-PHONE-NUMBER     PICTURE 9(7).
157400160229   ¦       03  IPS-REP-PHONE-EXT        PICTURE 9(6).
157500160229   ¦       03  IPS-REP-FAX-AREA-CODE    PICTURE 9(3).
157600160229R18867     03  IPS-REP-FAX-NUMBER       PICTURE 9(7).
157700160229R19444     03  IPS-ADDRESS-1            PICTURE X(40).
157800160229  ¦        03  IPS-ADDRESS-2            PICTURE X(40).
157900160229  ¦        03  IPS-ADDRESS-3            PICTURE X(40).
158000160229  ¦        03  IPS-ADDRESS-4            PICTURE X(40).
158100160229  ¦        03  IPS-ADDRESS-5            PICTURE X(40).
158200160229  ¦        03  IPS-POSTAL               PICTURE X(12).
158300160229  ¦        03  IPS-COUNTRY              PICTURE X(3).
158400160229  ¦        03  IPS-PROVINCE             PICTURE X(3).
158500160229R19444     03  FILLER                   PICTURE X(639).
158600160229           03  IPS-SECTION-SEQUENCE-2   PICTURE 9(6).
158700160229           03  IPS-START-DATE           PICTURE 9(08).
158800160229           03  IPS-END-DATE             PICTURE 9(08).
158900160229           03  IPS-EOR-INDICATOR        PICTURE X(1).
159000160229      /
159100160229       01  ACCOUNT-DETAILS-SECTION.
159200160229
159300160229      ******************************************************************
159400160229      *                                                                *
159500160229      *    CPYJSISTMT: Statement File - Account Detail Record          *
159600160229      *    CREATED:    DD/MMM/YY (III)                                 *
159700160229      *    CHANGED:    23/JUN/98 (MFH)                                 *
159800160229      *    LENGTH:     750                                             *
159900160229      *                                                                *
160000160229      ******************************************************************
160100160229
160200160229      *****
160300160229      *****    - INVESTOR-NO will be formatted.
160400160229      *****    - ACCOUNT will be formatted.
160500160229      *****    - SECTION-NO will always be 20.
160600160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
160700160229      *****                       999999 will be reserved for the total.
160800160229      *****    - LANGUAGE will be one of the following.
160900160229      *****        'E' English
161000160229      *****        'F' French
161100160229      *****    - DEALER will be formatted.
161200160229      *****    - SALESREP will be formatted.
161300160229      *****
161400160229
161500160229           03  AD-INVESTOR-NO          PICTURE 9(9).
161600160229           03  AD-ACCOUNT              PICTURE 9(9).
161700160229           03  AD-SECTION-NO           PICTURE 9(2).
161800160229           03  AD-SECTION-SEQUENCE     PICTURE 9(6).
161900160229           03  AD-LANGUAGE             PICTURE X(1).
162000160229           03  AD-DEALER               PICTURE X(4).
162100160229           03  AD-SALESREP             PICTURE X(6).
162200160229           03  AD-ACCOUNT-TYPE-SHORT   PICTURE X(5).
162300160229           03  AD-ACCOUNT-TYPE         PICTURE X(35).
162400160229           03  AD-SPOUSAL              PICTURE X(1).
162500160229           03  AD-LOCKED-IN            PICTURE X(1).
162600160229           03  AD-NOMINEE              PICTURE X(1).
162700160229           03  AD-GROUP                PICTURE X(1).
162800160229           03  AD-SPOUSAL-CONTRIB      PICTURE X(40).
162900160229           03  AD-INVESTMENT-NAME      PICTURE X(35).
163000160229           03  AD-CURRENCY             PICTURE X(3).
163100160229           03  AD-CURRENCY-DESCRIPTION PICTURE X(25).
163200160229           03  AD-NO-OF-UNITS          PICTURE 9(9)V9(3).
163300160229           03  AD-NO-OF-UNITS-SIGN     PICTURE X(1).
163400160229           03  AD-BOOK-VALUE-BASE      PICTURE 9(11)V9(2).
163500160229           03  AD-BOOK-VALUE-SIGN      PICTURE X(1).
163600160229           03  AD-MARKET-VALUE         PICTURE 9(11)V9(2).
163700160229           03  AD-MARKET-VALUE-SIGN    PICTURE X(1).
163800160229           03  AD-MARKET-VALUE-BASE    PICTURE 9(11)V9(2).
163900160229           03  AD-MARKET-VALUE-BASE-SIGN
164000160229                                       PICTURE X(1).
164100160229           03  AD-ACB-UNIT-PRICE       PICTURE 9(5)V9(4).
164200160229           03  AD-ACB-UNIT-PRICE-SIGN  PICTURE X(1).
164300160229           03  AD-ACB-UNIT-PRICE-BASE  PICTURE 9(5)V9(4).
164400160229           03  AD-ACB-UNIT-PRICE-BASE-SIGN
164500160229                                       PICTURE X(1).
164600160229           03  AD-EXCHANGE-RATE        PICTURE 9(2)V9(7).
164700160229           03  AD-ACCRUED-INTEREST     PICTURE 9(9)V9(2).
164800160229           03  AD-FOREIGN-CONTENT      PICTURE 9(3)V9(2).
164900160229           03  AD-ACCOUNT-PERC         PICTURE 9(3)V9(2).
165000160229           03  AD-LAST-UNIT-PRICE      PICTURE 9(5)V9(4).
165100160229           03  AD-LAST-UNIT-PRICE-SIGN PICTURE X(1).
165200160229           03  AD-FUND-NO              PICTURE X(8).
165300160229           03  AD-ASSET-CLASS          PICTURE X(25).
165400160229           03  AD-PUR-INCEPT           PICTURE 9(9)V9(2).
165500160229           03  AD-PI-SIGN              PICTURE X(1).
165600160229           03  AD-PUR-CURRENT          PICTURE 9(9)V9(2).
165700160229           03  AD-PC-SIGN              PICTURE X(1).
165800160229           03  AD-RED-INCEPT           PICTURE 9(9)V9(2).
165900160229           03  AD-RI-SIGN              PICTURE X(1).
166000160229           03  AD-RED-CURRENT          PICTURE 9(9)V9(2).
166100160229           03  AD-RC-SIGN              PICTURE X(1).
166200160229           03  AD-TRI-INCEPT           PICTURE 9(9)V9(2).
166300160229           03  AD-TI-SIGN              PICTURE X(1).
166400160229           03  AD-TRI-CURRENT          PICTURE 9(9)V9(2).
166500160229           03  AD-TC-SIGN              PICTURE X(1).
166600160229           03  AD-TRO-INCEPT           PICTURE 9(9)V9(2).
166700160229           03  AD-OI-SIGN              PICTURE X(1).
166800160229           03  AD-TRO-CURRENT          PICTURE 9(9)V9(2).
166900160229           03  AD-OC-SIGN              PICTURE X(1).
167000160229           03  AD-DIVS-INCEPT          PICTURE 9(9)V9(2).
167100160229           03  AD-DI-SIGN              PICTURE X(1).
167200160229           03  AD-DIVS-CURRENT         PICTURE 9(9)V9(2).
167300160229           03  AD-DC-SIGN              PICTURE X(1).
167400160229           03  AD-CPG-INCEPT           PICTURE 9(9)V9(2).
167500160229           03  AD-CI-SIGN              PICTURE X(1).
167600160229           03  AD-CPG-CURRENT          PICTURE 9(9)V9(2).
167700160229           03  AD-CC-SIGN              PICTURE X(1).
167800160229           03  AD-INC-INCEPT           PICTURE 9(9)V9(2).
167900160229           03  AD-II-SIGN              PICTURE X(1).
168000160229           03  AD-INC-CURRENT          PICTURE 9(9)V9(2).
168100160229           03  AD-IC-SIGN              PICTURE X(1).
168200160229           03  AD-INT-INCEPT           PICTURE 9(9)V9(2).
168300160229           03  AD-NI-SIGN              PICTURE X(1).
168400160229           03  AD-INT-CURRENT          PICTURE 9(9)V9(2).
168500160229           03  AD-NC-SIGN              PICTURE X(1).
168600160229           03  AD-REG-CTB-CUR          PICTURE 9(9)V9(2).
168700160229           03  AD-REG-CTB-CUR-SIGN     PICTURE X(1).
168800160229           03  AD-REG-CTB-INCPT        PICTURE 9(9)V9(2).
168900160229           03  AD-REG-CTB-INCPT-SIGN   PICTURE X(1).
169000160229           03  AD-TXNS-ON-STATEMENT    PICTURE X(1).
169100160229           03  AD-MARKET-VALUE-PRIOR   PICTURE 9(11)V9(2).
169200160229           03  AD-MARKET-VALUE-PRIOR-SIGN
169300160229                                       PICTURE X(1).
169400160229           03  AD-LAST-UNIT-PRICE-BASE PICTURE 9(5)V9(4).
169500160229           03  AD-LAST-UNIT-PRICE-BASE-SIGN
169600160229                                       PICTURE X(1).
169700160229           03  AD-CPG-FIXED            PICTURE 9(9)V9(2).
169800160229           03  AD-CPG-FIXED-SIGN       PICTURE X(1).
169900160229           03  AD-CONSTITUTE-FOREIGN   PICTURE X(1).
170000160229           03  AD-INVALID-AVERAGE-COST PICTURE X(1).
170100160229           03  AD-NOMINEE-ACCOUNT-TYPE PICTURE X(5).
170200160229           03  AD-BOOK-PERC            PICTURE 9(3)V9(2).
170300160229           03  AD-UNITS-BOUGHT-LTD       PICTURE 9(9)V9(3).
170400160229           03  AD-UNITS-BOUGHT-LTD-SIGN  PICTURE X.
170500160229           03  AD-UNITS-SOLD-LTD         PICTURE 9(9)V9(3).
170600160229           03  AD-UNITS-SOLD-LTD-SIGN    PICTURE X.
170700160229           03  AD-UNITS-DIV-LTD          PICTURE 9(9)V9(3).
170800160229           03  AD-UNITS-DIV-LTD-SIGN     PICTURE X.
170900160229           03  AD-BOOK-VALUE-PRIOR       PICTURE 9(11)V9(2).
171000160229           03  AD-BOOK-VALUE-PRIOR-SIGN  PICTURE X.
171100160229           03  AD-DSP-BOOK-VALUE-P-AUG       PICTURE 9(9)V9(2).
171200160229           03  AD-DSP-BOOK-VALUE-P-AUG-SIGN  PICTURE X.
171300160229           03  AD-INVESTMENT-GRP-CATEGORY    PICTURE X(5).
171400160229           03  AD-INVESTMENT-GRP-CAT-DESC    PICTURE X(25).
171500160229           03  AD-MARKET-VALUE-PRIOR-Q1      PICTURE 9(11)V9(2).
171600160229           03  AD-MARKET-VALUE-PRIOR-Q2      PICTURE 9(11)V9(2).
171700160229           03  AD-MARKET-VALUE-PRIOR-Q3      PICTURE 9(11)V9(2).
171800160229           03  AD-MARKET-VALUE-PRIOR-Q4      PICTURE 9(11)V9(2).
171900160229           03  AD-DII-RATE                   PICTURE 9(3)V9(3).
172000160229           03  AD-SWI-INCEPT                 PICTURE 9(9)V9(2).
172100160229           03  AD-SWO-INCEPT                 PICTURE 9(9)V9(2).
172200160229           03  AD-BOOK-VALUE                 PICTURE 9(11)V9(2).
172300160229R11127     03  AD-YTD-DISP-PROCEEDS-BASE     PICTURE 9(11)V9(2).
172400160229           03  AD-YTD-DISP-PROCEEDS-BASE-S   PICTURE X.
172500160229           03  AD-YTD-DISP-BOOK-VALUE-BASE   PICTURE 9(11)V9(2).
172600160229           03  AD-YTD-DISP-BOOK-VALUE-BASE-S PICTURE X.
172700160229           03  AD-YTD-DISP-GAIN-LOSS-BASE    PICTURE 9(11)V9(2).
172800160229           03  AD-YTD-DISP-GAIN-LOSS-BASE-S  PICTURE X.
172900160229R12102     03  AD-PHANTOM-FUND-NUMBER        PICTURE X(5).
173000160229R13211     03  AD-ALT-ACCOUNT-NO             PICTURE X(12).
173100160229           03  AD-TRI-INCEPT-COST            PICTURE 9(9)V9(2).
173200160229           03  AD-TRI-INCEPT-COST-SIGN       PICTURE X(1).
173300160229           03  AD-TRO-INCEPT-COST            PICTURE 9(9)V9(2).
173400160229           03  AD-TRO-INCEPT-COST-SIGN       PICTURE X(1).
173500160229R14858     03  AD-BAJ-CUR                    PICTURE 9(9)V9(2).
173600160229  ¦        03  AD-BAJ-CUR-SIGN               PICTURE X(1).
173700160229  ¦        03  AD-BAJ-YTD                    PICTURE 9(9)V9(2).
173800160229  ¦        03  AD-BAJ-YTD-SIGN               PICTURE X(1).
173900160229  ¦        03  AD-BAJ-INCEPT                 PICTURE 9(9)V9(2).
174000160229  ¦        03  AD-BAJ-INCEPT-SIGN            PICTURE X(1).
174100160229  ¦        03  AD-BFW-CUR                    PICTURE 9(9)V9(2).
174200160229  ¦        03  AD-BFW-CUR-SIGN               PICTURE X(1).
174300160229  ¦        03  AD-BFW-YTD                    PICTURE 9(9)V9(2).
174400160229  ¦        03  AD-BFW-YTD-SIGN               PICTURE X(1).
174500160229  ¦        03  AD-BFW-INCEPT                 PICTURE 9(9)V9(2).
174600160229R14858     03  AD-BFW-INCEPT-SIGN            PICTURE X(1).
174700160229R15022     03  AD-DISP-GAIN-LOSS-CUR         PICTURE 9(11)V9(2).
174800160229  ¦        03  AD-DISP-GAIN-LOSS-CUR-SIGN    PICTURE X(1).
174900160229  ¦        03  AD-DISP-GAIN-LOSS-YTD         PICTURE 9(11)V9(2).
175000160229  ¦        03  AD-DISP-GAIN-LOSS-YTD-SIGN    PICTURE X(1).
175100160229  ¦        03  AD-DISP-GAIN-LOSS-INCEPT      PICTURE 9(11)V9(2).
175200160229  ¦        03  AD-DISP-GAIN-LOSS-INCEPT-SIGN PICTURE X(1).
175300160229           03  AD-TRI-INCEPT-MKT             PICTURE 9(9)V9(2).
175400160229           03  AD-TRI-INCEPT-MKT-SIGN        PICTURE X(1).
175500160229           03  AD-TRO-INCEPT-MKT             PICTURE 9(9)V9(2).
175600160229           03  AD-TRO-INCEPT-MKT-SIGN        PICTURE X(1).
175700160229R16881     03  AD-TRI-INCEPT-PHANTOM         PICTURE 9(9)V9(2).
175800160229   ¦       03  AD-TRI-INCEPT-PHANTOM-SIGN    PICTURE X(1).
175900160229   ¦       03  AD-TRO-INCEPT-PHANTOM         PICTURE 9(9)V9(2).
176000160229   ¦       03  AD-TRO-INCEPT-PHANTOM-SIGN    PICTURE X(1).
176100160229   ¦       03  AD-TRI-YTD-PHANTOM            PICTURE 9(9)V9(2).
176200160229   ¦       03  AD-TRI-YTD-PHANTOM-SIGN       PICTURE X(1).
176300160229   ¦       03  AD-TRO-YTD-PHANTOM            PICTURE 9(9)V9(2).
176400160229   ¦       03  AD-TRO-YTD-PHANTOM-SIGN       PICTURE X(1).
176500160229   ¦       03  AD-TRI-CUR-PHANTOM            PICTURE 9(9)V9(2).
176600160229   ¦       03  AD-TRI-CUR-PHANTOM-SIGN       PICTURE X(1).
176700160229   ¦       03  AD-TRO-CUR-PHANTOM            PICTURE 9(9)V9(2).
176800160229   V       03  AD-TRO-CUR-PHANTOM-SIGN       PICTURE X(1).
176900160229R17527     03  AD-NET-AMOUNT-CURR            PICTURE 9(11)V9(2).
177000160229   ¦       03  AD-NET-AMOUNT-CURR-SIGN       PICTURE X(1).
177100160229   ¦       03  AD-NO-OF-UNITS-CURR           PICTURE 9(9)V9(3).
177200160229   ¦       03  AD-NO-OF-UNITS-CURR-SIGN      PICTURE X(1).
177300160229   ¦       03  AD-GROUP-PERCENTAGE           PICTURE 9(3)V9(2).
177400160229   ¦       03  AD-INVESTMENT-GROUP-CODE      PICTURE X(5).
177500160229   ¦       03  AD-IPS-GROUP-ID               PICTURE X(20).
177600160229R18312     03  AD-DEPOSIT                    PICTURE 9(11)V99.
177700160229   ¦       03  AD-DEPOSIT-SIGN               PICTURE X(1).
177800160229   ¦       03  AD-WITHDRAWALS                PICTURE 9(11)V99.
177900160229   ¦       03  AD-WITHDRAWALS-SIGN           PICTURE X(1).
178000160229   ¦       03  AD-AVAIL-FGN-CONTENT          PICTURE 9(11)V99.
178100160229R18312     03  AD-AVAIL-FGN-CONTENT-SIGN     PICTURE X(1).
178200160229R20758     03  AD-ROC-CUR                    PICTURE 9(9)V99.
178300160229   ¦       03  AD-ROC-CUR-SIGN               PICTURE X(1).
178400160229   ¦       03  AD-ROC-YTD                    PICTURE 9(9)V99.
178500160229   ¦       03  AD-ROC-YTD-SIGN               PICTURE X(1).
178600160229   ¦       03  AD-ROC-INCEPT                 PICTURE 9(9)V99.
178700160229R20758     03  AD-ROC-INCEPT-SIGN            PICTURE X(1).
178800160229R26213     03  AD-UNIT-BALANCE-PRIOR-Q1      PICTURE 9(9)V999.
178900160229R26213     03  AD-UNIT-BALANCE-PRIOR-Q2      PICTURE 9(9)V999.
179000160229R26213     03  AD-UNIT-BALANCE-PRIOR-Q3      PICTURE 9(9)V999.
179100160229R26213     03  AD-UNIT-BALANCE-PRIOR-Q4      PICTURE 9(9)V999.
179200160229R26213     03  AD-UNIT-BALANCE-PRIOR-YE      PICTURE 9(9)V999.
179300160229R26213     03  AD-UNIT-PRICE-PRIOR-Q1        PICTURE 9(5)V9999.
179400160229R26213     03  AD-UNIT-PRICE-PRIOR-Q2        PICTURE 9(5)V9999.
179500160229R26213     03  AD-UNIT-PRICE-PRIOR-Q3        PICTURE 9(5)V9999.
179600160229R26213     03  AD-UNIT-PRICE-PRIOR-Q4        PICTURE 9(5)V9999.
179700160229R26213     03  AD-UNIT-PRICE-PRIOR-YE        PICTURE 9(5)V9999.
179800160229R39466     03  AD-APP-RECEIVED               PICTURE X(1).
179900160229R50492     03  AD-DORMANT-FUND               PICTURE X(1).
180000160229      * RFS 143392 STARTS *
180100160229           03 AD-DSC                         PICTURE X(1).
180200160229R50492*    03  FILLER                        PICTURE X(7).
180300200515      * RFS 185916 - Begins
180400200520      *    03  FILLER                        PICTURE X(6).
180500200515           03  AD-PROD-TYPE-CODE             PICTURE X(03).
180600200515           03  FILLER                        PICTURE X(3).
180700200515      * RFS 185916 - Ends
180800160229      * RFS 143392 ENDS *
180900160229           03  AD-SECTION-SEQUENCE-2   PICTURE 9(6).
181000160229           03  AD-START-DATE           PICTURE 9(08).
181100160229           03  AD-END-DATE             PICTURE 9(08).
181200160229           03  AD-EOR-INDICATOR        PICTURE X(1).
181300160229      /
181400160229       01  ACCOUNT-ADDITIONAL-INFO.
181500160229      ******************************************************************
181600160229      *                                                                *
181700160229      *    CPYJSISTMT: Statement File - Primary Record                 *
181800160229      *    CREATED:    05/AUG/05 (MFH)                                 *
181900160229      *    CHANGED:    05/AUG/05 (MFH)                                 *
182000160229      *    LENGTH:     750                                             *
182100160229      *    RFS:        28931                                           *
182200160229      *                                                                *
182300160229      ******************************************************************
182400160229
182500160229      *****
182600160229      *****    - INVESTOR-NO will be formatted.
182700160229      *****    - ACCOUNT will be formatted.
182800160229      *****    - SECTION-NO will always be 21.
182900160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
183000160229      *****                       999999 will be reserved for the total.
183100160229      *****    - LANGUAGE will be one of the following.
183200160229      *****        'E' English
183300160229      *****        'F' French
183400160229      *****    - DEALER will be formatted.
183500160229      *****    - SALESREP will be formatted.
183600160229      *****
183700160229
183800160229           03  AAI-INVESTOR-NO          PICTURE 9(9).
183900160229           03  AAI-ACCOUNT              PICTURE 9(9).
184000160229           03  AAI-SECTION-NO           PICTURE 9(2).
184100160229           03  AAI-SECTION-SEQUENCE     PICTURE 9(6).
184200160229           03  AAI-LANGUAGE             PICTURE X(1).
184300160229           03  AAI-DEALER               PICTURE X(4).
184400160229           03  AAI-SALESREP             PICTURE X(6).
184500160229
184600160229           03  AAI-ACCOUNT-TYPE-SHORT        PICTURE X(5).
184700160229           03  AAI-ACCOUNT-TYPE              PICTURE X(35).
184800160229           03  AAI-INVESTMENT-NAME           PICTURE X(35).
184900160229           03  AAI-PURCH-LAST-12-MONTHS      PICTURE 9(11)V9(2).
185000160229           03  AAI-PURCH-LAST-12-MONTHS-SIGN PICTURE X(1).
185100160229           03  AAI-REDEM-LAST-12-MONTHS      PICTURE 9(11)V9(2).
185200160229           03  AAI-REDEM-LAST-12-MONTHS-SIGN PICTURE X(1).
185300160229           03  AAI-TRI-LAST-12-MONTHS        PICTURE 9(11)V9(2).
185400160229           03  AAI-TRI-LAST-12-MONTHS-SIGN   PICTURE X(1).
185500160229           03  AAI-TRO-LAST-12-MONTHS        PICTURE 9(11)V9(2).
185600160229           03  AAI-TRO-LAST-12-MONTHS-SIGN   PICTURE X(1).
185700160229R36342     03  AAI-CURR-DISP-PROCEEDS-BASE   PICTURE 9(11)V9(2).
185800160229R36342     03  AAI-CURR-DISP-PROCEEDS-BASE-S PICTURE X(1).
185900160229R36342     03  AAI-CURR-DISP-BOOK-VAL-BASE   PICTURE 9(11)V9(2).
186000160229R36342     03  AAI-CURR-DISP-BOOK-VAL-BASE-S PICTURE X(1).
186100160229R36342     03  AAI-CURR-DISP-GAIN-LOSS-BASE  PICTURE 9(11)V9(2).
186200160229R36342     03  AAI-CURR-DISP-GAIN-LOSS-BASE-S PICTURE X(1).
186300160229R36342     03  AAI-GROSS-SOLD-YTD-BASE       PICTURE 9(11)V9(2).
186400160229R36342     03  AAI-GROSS-SOLD-YTD-BASE-SIGN  PICTURE X(1).
186500160229R36342     03  AAI-GROSS-SOLD-CURR-BASE      PICTURE 9(11)V9(2).
186600160229R36342     03  AAI-GROSS-SOLD-CURR-BASE-SIGN PICTURE X(1).
186700160229R36342     03  AAI-DEDUCTIONS-DSC-YTD-BASE   PICTURE 9(11)V9(2).
186800160229R36342     03  AAI-DEDUCTIONS-DSC-YTD-BASE-S PICTURE X(1).
186900160229R36342     03  AAI-DEDUCTIONS-DSC-CURR-BASE  PICTURE 9(11)V9(2).
187000160229R36342     03  AAI-DEDUCTIONS-DSC-CURR-BASE-S PICTURE X(1).
187100160229
187200160229R50823     03  AAI-GMA-VALUE                 PICTURE 9(11)V9(2).
187300160229  |        03  AAI-GMA-VALUE-SIGN            PICTURE X(1).
187400160229  |        03  AAI-GMP-CLOSING-DATE          PICTURE 9(8).
187500160229  |        03  AAI-GMP-PRICE                 PICTURE 9(5)V9(4).
187600160229  V        03  AAI-GMP-PRICE-SIGN            PICTURE X(1).
187700160229R50492     03  AAI-DORMANT-FUND              PICTURE X(1).
187800160229102716     03  AAI-CPG-YTD                   PICTURE 9(9)V9(2).
187900160229  |        03  AAI-CPG-YTD-SIGN              PICTURE X(1).
188000160229  |        03  AAI-INC-YTD                   PICTURE 9(9)V9(2).
188100160229  |        03  AAI-INC-YTD-SIGN              PICTURE X(1).
188200160229  |        03  AAI-INT-YTD                   PICTURE 9(9)V9(2).
188300160229  V        03  AAI-INT-YTD-SIGN              PICTURE X(1).
188400160229      *RFS151145 START
188500160229102716*    03  FILLER                        PICTURE X(992).
188600160229           03  AAI-CROSS-REF-NUMBER          PICTURE X(10).
188700160229      *RFS147795 START
188800160229           03  AAI-YTD-TRAILER               PICTURE 9(11)V9(2).
188900160229           03  AAI-YTD-TRAILER-SIGN          PICTURE X(1).
189000160229      *RFS148326 START
189100160229      *    03  FILLER                        PICTURE X(968).
189200160229           03  AAI-ORIG-COST-INFO            PICTURE 9(11)V9(2).
189300160229           03  AAI-ORIG-COST-INFO-SIGN       PICTURE X(1).
189400160229           03  AAI-RISK-RATING               PICTURE X(20).
189500160229           03  AAI-MER                       PICTURE 9(3)V9(2).
189600160614      *RFS156335 CHANGES START
189700160614156335     03  AAI-ASSET-CLASS-CODE          PICTURE X(6).
189800160614156335     03  AAI-ASSET-CLASS-DESCRIPTION   PICTURE X(25).
189900160614156335     03  AAI-INVESTMENT-EXT-LOAD-TYPE  PICTURE X(3).
190000160614156335     03  AAI-INVESTMENT-EXT-LOAD-DESC  PICTURE X(25).
190100160614156335     03  FILLER                        PICTURE X(870).
190200160614156335*    03  FILLER                        PICTURE X(929).
190300160614      *RFS156335 CHANGES END
190400160229      *RFS148326 END
190500160229      *    03  FILLER                        PICTURE X(982).
190600160229      *RFS147795 END
190700160229      *RFS151145 END
190800160229
190900160229           03  AAI-SECTION-SEQUENCE-2        PICTURE 9(6).
191000160229           03  AAI-START-DATE                PICTURE 9(08).
191100160229           03  AAI-END-DATE                  PICTURE 9(08).
191200160229           03  AAI-EOR-INDICATOR             PICTURE X(1).
191300160229
191400160229      /
191500160229       01  ACCOUNT-GIC-DETAILS-SECTION.
191600160229
191700160229      ******************************************************************
191800160229      *                                                                *
191900160229      *    CPYJSISTMT: Statement File - ACCOUNT-GIC-DETAILS-RECORDS    *
192000160229      *    CREATED:    25/08/2000 (III)                                *
192100160229      *    CHANGED:      /   /   (MFH)                                 *
192200160229      *    LENGTH:     800                                             *
192300160229      *                                                                *
192400160229      ******************************************************************
192500160229
192600160229      *****
192700160229      *****    - INVESTOR-NO will be formatted.
192800160229      *****    - ACCOUNT will be formatted.
192900160229      *****    - SECTION-NO will always be 25.
193000160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
193100160229      *****                       999999 will be reserved for the total.
193200160229      *****    - LANGUAGE will be one of the following.
193300160229      *****        'E' English
193400160229      *****        'F' French
193500160229      *****    - DEALER will be formatted.
193600160229      *****    - SALESREP will be formatted.
193700160229      *****
193800160229
193900160229           03  AG-INVESTOR-NO          PICTURE 9(9).
194000160229           03  AG-ACCOUNT              PICTURE 9(9).
194100160229           03  AG-SECTION-NO           PICTURE 9(2).
194200160229           03  AG-SECTION-SEQUENCE     PICTURE 9(6).
194300160229           03  AG-LANGUAGE             PICTURE X(1).
194400160229           03  AG-DEALER               PICTURE X(4).
194500160229           03  AG-SALESREP             PICTURE X(6).
194600160229
194700160229           03  AG-FUND-NO              PICTURE X(8).
194800160229           03  AG-CERTIFICATE-NO       PICTURE X(12).
194900160229           03  AG-CERTIFICATE-STATUS   PICTURE X(1).
195000160229           03  AG-ISSUED-DATE          PICTURE 9(8).
195100160229           03  AG-MATURITY-DATE        PICTURE 9(8).
195200160229           03  AG-TERM                 PICTURE 9(2)V9(3).
195300160229           03  AG-RATE                 PICTURE 9(3)V9(3).
195400160229           03  AG-INITIAL-AMOUNT       PICTURE 9(11)V9(2).
195500160229           03  AG-MATURITY-VALUE       PICTURE 9(11)V9(2).
195600160229           03  AG-INTEREST-PAY-OPTION  PICTURE X(1).
195700160229           03  AG-INTEREST-FREQUENCY   PICTURE X(2).
195800160229           03  AG-CAPITAL-PAY-OPTION   PICTURE X(1).
195900160229           03  AG-NEXT-TERM-REQUEST    PICTURE 9(2)V9(3).
196000160229           03  AG-BALANCE              PICTURE 9(11)V9(2).
196100160229           03  AG-ACCRUED-INTEREST     PICTURE 9(11)V9(2).
196200160229R13300     03  AG-ACCRUED-INTEREST-SIGN PICTURE X(1).
196300160229           03  AG-AUTOMATIC-RENEWAL    PICTURE X(1).
196400160229           03  AG-INTEREST-RATE-CATEGORY PICTURE X(1).
196500160229           03  AG-EARLY-REDEMPTION-CODE  PICTURE X(1).
196600160229           03  AG-MOVABLE-MORTGAGE     PICTURE X(1).
196700160229      *RFS139988 - BEGINS
196800160229           03  AG-TERM-ACCOUNT-MATURITY  PICTURE X(1).
196900160229      * RFS 142884 STARTS
197000160229           03  AG-TERM-GROUP             PICTURE X(10).
197100160229           03  AG-BALANCE-FORWARD        PICTURE 9(11)V9(2).
197200160229           03  AG-BALANCE-FORWARD-SIGN   PICTURE X(1).
197300160229           03  AG-CLOSING-BALANCE        PICTURE 9(11)V9(2).
197400160229           03  AG-CLOSING-BALANCE-SIGN   PICTURE X(1).
197500160229R17165*    03  FILLER                    PICTURE X(1176).
197600160229      *    03  FILLER                    PICTURE X(1175).
197700160229           03  FILLER                    PICTURE X(1137).
197800160229      * RFS 142884 ENDS
197900160229      *RFS139988 - ENDS
198000160229           03  AG-SECTION-SEQUENCE-2   PICTURE 9(6).
198100160229           03  AG-START-DATE           PICTURE 9(08).
198200160229           03  AG-END-DATE             PICTURE 9(08).
198300160229           03  AG-EOR-INDICATOR        PICTURE X(1).
198400160229
198500160229      /
198600160229       01  ACCOUNT-PERFORMANCE-SECTION.
198700160229
198800160229      ******************************************************************
198900160229      *                                                                *
199000160229      *    CPYJSISTMT: Statement File - Account Performance-Record     *
199100160229      *    CREATED:    22/11/1999 (III)                                *
199200160229      *    CHANGED:    23/JUN/98 (MFH)                                 *
199300160229      *    LENGTH:     750                                             *
199400160229      *                                                                *
199500160229      ******************************************************************
199600160229
199700160229      *****
199800160229      *****    - INVESTOR-NO will be formatted.
199900160229      *****    - ACCOUNT will be formatted.
200000160229      *****    - SECTION-NO will always be 30.
200100160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
200200160229      *****                       999999 will be reserved for the total.
200300160229      *****    - LANGUAGE will be one of the following.
200400160229      *****        'E' English
200500160229      *****        'F' French
200600160229      *****    - DEALER will be formatted.
200700160229      *****    - SALESREP will be formatted.
200800160229      *****
200900160229
201000160229           03  AP-INVESTOR-NO          PICTURE 9(9).
201100160229           03  AP-ACCOUNT              PICTURE 9(9).
201200160229           03  AP-SECTION-NO           PICTURE 9(2).
201300160229           03  AP-SECTION-SEQUENCE     PICTURE 9(6).
201400160229           03  AP-LANGUAGE             PICTURE X(1).
201500160229           03  AP-DEALER               PICTURE X(4).
201600160229           03  AP-SALESREP             PICTURE X(6).
201700160229
201800160229           03  AP-PERIOD-CODE-1        PICTURE X(5).
201900160229           03  AP-PERIOD-DESCRIPTION-1 PICTURE X(25).
202000160229           03  AP-PERIOD-INDICATOR-1   PICTURE X(1).
202100160229           03  AP-PERIOD-ROR-1         PICTURE 9(8)V9(5).
202200160229           03  AP-PERIOD-ROR-SIGN-1    PICTURE X(1).
202300160229           03  AP-PERIOD-ROR-APPLICABLE-1 PICTURE X(1).
202400160229           03  AP-PERIOD-CODE-2        PICTURE X(5).
202500160229           03  AP-PERIOD-DESCRIPTION-2 PICTURE X(25).
202600160229           03  AP-PERIOD-INDICATOR-2   PICTURE X(1).
202700160229           03  AP-PERIOD-ROR-2         PICTURE 9(8)V9(5).
202800160229           03  AP-PERIOD-ROR-SIGN-2    PICTURE X(1).
202900160229           03  AP-PERIOD-ROR-APPLICABLE-2 PICTURE X(1).
203000160229           03  AP-PERIOD-CODE-3        PICTURE X(5).
203100160229           03  AP-PERIOD-DESCRIPTION-3 PICTURE X(25).
203200160229           03  AP-PERIOD-INDICATOR-3   PICTURE X(1).
203300160229           03  AP-PERIOD-ROR-3         PICTURE 9(8)V9(5).
203400160229           03  AP-PERIOD-ROR-SIGN-3    PICTURE X(1).
203500160229           03  AP-PERIOD-ROR-APPLICABLE-3 PICTURE X(1).
203600160229           03  AP-PERIOD-CODE-4        PICTURE X(5).
203700160229           03  AP-PERIOD-DESCRIPTION-4 PICTURE X(25).
203800160229           03  AP-PERIOD-INDICATOR-4   PICTURE X(1).
203900160229           03  AP-PERIOD-ROR-4         PICTURE 9(8)V9(5).
204000160229           03  AP-PERIOD-ROR-SIGN-4    PICTURE X(1).
204100160229           03  AP-PERIOD-ROR-APPLICABLE-4 PICTURE X(1).
204200160229           03  AP-PERIOD-CODE-5        PICTURE X(5).
204300160229           03  AP-PERIOD-DESCRIPTION-5 PICTURE X(25).
204400160229           03  AP-PERIOD-INDICATOR-5   PICTURE X(1).
204500160229           03  AP-PERIOD-ROR-5         PICTURE 9(8)V9(5).
204600160229           03  AP-PERIOD-ROR-SIGN-5    PICTURE X(1).
204700160229           03  AP-PERIOD-ROR-APPLICABLE-5 PICTURE X(1).
204800160229           03  AP-PERIOD-CODE-6        PICTURE X(5).
204900160229           03  AP-PERIOD-DESCRIPTION-6 PICTURE X(25).
205000160229           03  AP-PERIOD-INDICATOR-6   PICTURE X(1).
205100160229           03  AP-PERIOD-ROR-6         PICTURE 9(8)V9(5).
205200160229           03  AP-PERIOD-ROR-SIGN-6    PICTURE X(1).
205300160229           03  AP-PERIOD-ROR-APPLICABLE-6 PICTURE X(1).
205400160229           03  AP-PERIOD-CODE-7        PICTURE X(5).
205500160229           03  AP-PERIOD-DESCRIPTION-7 PICTURE X(25).
205600160229           03  AP-PERIOD-INDICATOR-7   PICTURE X(1).
205700160229           03  AP-PERIOD-ROR-7         PICTURE 9(8)V9(5).
205800160229           03  AP-PERIOD-ROR-SIGN-7    PICTURE X(1).
205900160229           03  AP-PERIOD-ROR-APPLICABLE-7 PICTURE X(1).
206000160229           03  AP-PERIOD-CODE-8        PICTURE X(5).
206100160229           03  AP-PERIOD-DESCRIPTION-8 PICTURE X(25).
206200160229           03  AP-PERIOD-INDICATOR-8   PICTURE X(1).
206300160229           03  AP-PERIOD-ROR-8         PICTURE 9(8)V9(5).
206400160229           03  AP-PERIOD-ROR-SIGN-8    PICTURE X(1).
206500160229           03  AP-PERIOD-ROR-APPLICABLE-8 PICTURE X(1).
206600160229           03  AP-PERIOD-CODE-9        PICTURE X(5).
206700160229           03  AP-PERIOD-DESCRIPTION-9 PICTURE X(25).
206800160229           03  AP-PERIOD-INDICATOR-9   PICTURE X(1).
206900160229           03  AP-PERIOD-ROR-9         PICTURE 9(8)V9(5).
207000160229           03  AP-PERIOD-ROR-SIGN-9    PICTURE X(1).
207100160229           03  AP-PERIOD-ROR-APPLICABLE-9 PICTURE X(1).
207200160229           03  AP-PERIOD-CODE-10       PICTURE X(5).
207300160229           03  AP-PERIOD-DESCRIPTION-10  PICTURE X(25).
207400160229           03  AP-PERIOD-INDICATOR-10  PICTURE X(1).
207500160229           03  AP-PERIOD-ROR-10        PICTURE 9(8)V9(5).
207600160229           03  AP-PERIOD-ROR-SIGN-10   PICTURE X(1).
207700160229           03  AP-PERIOD-ROR-APPLICABLE-10 PICTURE X(1).
207800160229R18814     03  AP-IPS-GROUP-ID         PICTURE X(20).
207900160229R20758     03  AP-CALC-DATE            PICTURE 9(8).
208000160229R20758     03  FILLER                  PICTURE X(802).
208100160229           03  AP-SECTION-SEQUENCE-2   PICTURE 9(6).
208200160229           03  AP-START-DATE           PICTURE 9(08).
208300160229           03  AP-END-DATE             PICTURE 9(08).
208400160229           03  AP-EOR-INDICATOR        PICTURE X(1).
208500160229
208600170217      *RFS164528 -STARTS
208700170217       01  ACC-ROR-PERIOD-MKTVAL-SECTION.
208800170217      ******************************************************************
208900170217      *                                                                *
209000170217      *    CPYJSISTMT: STATEMENT FILE - ACCOUNT ROR PERIOD MARKET VALUE*
209100170217      *    CREATED:    10/01/2017 (III)                                *
209200170217      *    CHANGED:    XX/XXX/XX                                       *
209300170217      *    LENGTH:     1350                                            *
209400170217      *                                                                *
209500170217      ******************************************************************
209600170217      *****
209700170217      *****    - INVESTOR-NO WILL BE FORMATTED.
209800170217      *****    - ACCOUNT WILL BE FORMATTED.
209900170217      *****    - SECTION-NO WILL ALWAYS BE 31.
210000170217      *****    - SECTION-SEQUENCE WILL FORMATTED FROM 000001 TO 999998,
210100170217      *****                       999999 WILL BE RESERVED FOR THE TOTAL.
210200170217      *****    - LANGUAGE WILL BE ONE OF THE FOLLOWING.
210300170217      *****        'E' ENGLISH
210400170217      *****        'F' FRENCH
210500170217      *****    - DEALER WILL BE FORMATTED.
210600170217      *****    - SALESREP WILL BE FORMATTED.
210700170217      *****
210800170217      *COMMON FIELD LAYOUT FOR ALL SECTIONS
210900170217           03  A31-INVESTOR-NO                  PICTURE 9(9).
211000170217           03  A31-ACCOUNT                      PICTURE 9(9).
211100170217           03  A31-SECTION-NO                   PICTURE 9(2).
211200170217           03  A31-SECTION-SEQUENCE             PICTURE 9(6).
211300170217           03  A31-LANGUAGE                     PICTURE X(1).
211400170217           03  A31-DEALER                       PICTURE X(4).
211500170217           03  A31-SALESREP                     PICTURE X(6).
211600170217
211700170217      *COMMON FIELDS TO ALL PERIODS
211800170217           03  A31-CLOSING-VALUE                PICTURE 9(11)V9(2).
211900170217           03  A31-CLOSING-VALUE-SIGN           PICTURE X(1).
212000170217
212100170217      *PERIOD1 FIELDS
212200170217           03  A31-PERIOD-CODE-1                PICTURE X(5).
212300170217           03  A31-PERIOD-DESCRIPTION-1         PICTURE X(25).
212400170217           03  A31-PERIOD-CF-START-DATE-1       PICTURE 9(8).
212500170217           03  A31-PERIOD-CF-OPENINGVALUE-1     PICTURE 9(11)V9(2).
212600170217           03  A31-PERIOD-CF-OPNINGVL-1-SIGN    PICTURE X(1).
212700170217           03  A31-PERIOD-CF-VALUECHG-1         PICTURE 9(11)V9(2).
212800170217           03  A31-PERIOD-CF-VALUECHG-1-SIGN    PICTURE X(1).
212900170217           03  A31-PERIOD-CF-TOTAMTINV-1        PICTURE 9(11)V9(2).
213000170217           03  A31-PERIOD-CF-TOTAMTINV-1-SIGN   PICTURE X(1).
213100170217           03  A31-PERIOD-CF-TOTAMTRED-1        PICTURE 9(11)V9(2).
213200170217           03  A31-PERIOD-CF-TOTAMTRED-1-SIGN   PICTURE X(1).
213300170217
213400170217      *PERIOD2 FIELDS
213500170217           03  A31-PERIOD-CODE-2                PICTURE X(5).
213600170217           03  A31-PERIOD-DESCRIPTION-2         PICTURE X(25).
213700170217           03  A31-PERIOD-CF-START-DATE-2       PICTURE 9(8).
213800170217           03  A31-PERIOD-CF-OPENINGVALUE-2     PICTURE 9(11)V9(2).
213900170217           03  A31-PERIOD-CF-OPNINGVL-2-SIGN    PICTURE X(1).
214000170217           03  A31-PERIOD-CF-VALUECHG-2         PICTURE 9(11)V9(2).
214100170217           03  A31-PERIOD-CF-VALUECHG-2-SIGN    PICTURE X(1).
214200170217           03  A31-PERIOD-CF-TOTAMTINV-2        PICTURE 9(11)V9(2).
214300170217           03  A31-PERIOD-CF-TOTAMTINV-2-SIGN   PICTURE X(1).
214400170217           03  A31-PERIOD-CF-TOTAMTRED-2        PICTURE 9(11)V9(2).
214500170217           03  A31-PERIOD-CF-TOTAMTRED-2-SIGN   PICTURE X(1).
214600170217
214700170217      *PERIOD3 FIELDS
214800170217           03  A31-PERIOD-CODE-3                PICTURE X(5).
214900170217           03  A31-PERIOD-DESCRIPTION-3         PICTURE X(25).
215000170217           03  A31-PERIOD-CF-START-DATE-3       PICTURE 9(8).
215100170217           03  A31-PERIOD-CF-OPENINGVALUE-3     PICTURE 9(11)V9(2).
215200170217           03  A31-PERIOD-CF-OPNINGVL-3-SIGN    PICTURE X(1).
215300170217           03  A31-PERIOD-CF-VALUECHG-3         PICTURE 9(11)V9(2).
215400170217           03  A31-PERIOD-CF-VALUECHG-3-SIGN    PICTURE X(1).
215500170217           03  A31-PERIOD-CF-TOTAMTINV-3        PICTURE 9(11)V9(2).
215600170217           03  A31-PERIOD-CF-TOTAMTINV-3-SIGN   PICTURE X(1).
215700170217           03  A31-PERIOD-CF-TOTAMTRED-3        PICTURE 9(11)V9(2).
215800170217           03  A31-PERIOD-CF-TOTAMTRED-3-SIGN   PICTURE X(1).
215900170217
216000170217      *PERIOD4 FIELDS
216100170217           03  A31-PERIOD-CODE-4                PICTURE X(5).
216200170217           03  A31-PERIOD-DESCRIPTION-4         PICTURE X(25).
216300170217           03  A31-PERIOD-CF-START-DATE-4       PICTURE 9(8).
216400170217           03  A31-PERIOD-CF-OPENINGVALUE-4     PICTURE 9(11)V9(2).
216500170217           03  A31-PERIOD-CF-OPNINGVL-4-SIGN    PICTURE X(1).
216600170217           03  A31-PERIOD-CF-VALUECHG-4         PICTURE 9(11)V9(2).
216700170217           03  A31-PERIOD-CF-VALUECHG-4-SIGN    PICTURE X(1).
216800170217           03  A31-PERIOD-CF-TOTAMTINV-4        PICTURE 9(11)V9(2).
216900170217           03  A31-PERIOD-CF-TOTAMTINV-4-SIGN   PICTURE X(1).
217000170217           03  A31-PERIOD-CF-TOTAMTRED-4        PICTURE 9(11)V9(2).
217100170217           03  A31-PERIOD-CF-TOTAMTRED-4-SIGN   PICTURE X(1).
217200170217
217300170217      *PERIOD5 FIELDS
217400170217           03  A31-PERIOD-CODE-5                PICTURE X(5).
217500170217           03  A31-PERIOD-DESCRIPTION-5         PICTURE X(25).
217600170217           03  A31-PERIOD-CF-START-DATE-5       PICTURE 9(8).
217700170217           03  A31-PERIOD-CF-OPENINGVALUE-5     PICTURE 9(11)V9(2).
217800170217           03  A31-PERIOD-CF-OPNINGVL-5-SIGN    PICTURE X(1).
217900170217           03  A31-PERIOD-CF-VALUECHG-5         PICTURE 9(11)V9(2).
218000170217           03  A31-PERIOD-CF-VALUECHG-5-SIGN    PICTURE X(1).
218100170217           03  A31-PERIOD-CF-TOTAMTINV-5        PICTURE 9(11)V9(2).
218200170217           03  A31-PERIOD-CF-TOTAMTINV-5-SIGN   PICTURE X(1).
218300170217           03  A31-PERIOD-CF-TOTAMTRED-5        PICTURE 9(11)V9(2).
218400170217           03  A31-PERIOD-CF-TOTAMTRED-5-SIGN   PICTURE X(1).
218500170217
218600170217      *PERIOD6 FIELDS
218700170217           03  A31-PERIOD-CODE-6                PICTURE X(5).
218800170217           03  A31-PERIOD-DESCRIPTION-6         PICTURE X(25).
218900170217           03  A31-PERIOD-CF-START-DATE-6       PICTURE 9(8).
219000170217           03  A31-PERIOD-CF-OPENINGVALUE-6     PICTURE 9(11)V9(2).
219100170217           03  A31-PERIOD-CF-OPNINGVL-6-SIGN    PICTURE X(1).
219200170217           03  A31-PERIOD-CF-VALUECHG-6         PICTURE 9(11)V9(2).
219300170217           03  A31-PERIOD-CF-VALUECHG-6-SIGN    PICTURE X(1).
219400170217           03  A31-PERIOD-CF-TOTAMTINV-6        PICTURE 9(11)V9(2).
219500170217           03  A31-PERIOD-CF-TOTAMTINV-6-SIGN   PICTURE X(1).
219600170217           03  A31-PERIOD-CF-TOTAMTRED-6        PICTURE 9(11)V9(2).
219700170217           03  A31-PERIOD-CF-TOTAMTRED-6-SIGN   PICTURE X(1).
219800170217
219900170217      *PERIOD7 FIELDS
220000170217           03  A31-PERIOD-CODE-7                PICTURE X(5).
220100170217           03  A31-PERIOD-DESCRIPTION-7         PICTURE X(25).
220200170217           03  A31-PERIOD-CF-START-DATE-7       PICTURE 9(8).
220300170217           03  A31-PERIOD-CF-OPENINGVALUE-7     PICTURE 9(11)V9(2).
220400170217           03  A31-PERIOD-CF-OPNINGVL-7-SIGN    PICTURE X(1).
220500170217           03  A31-PERIOD-CF-VALUECHG-7         PICTURE 9(11)V9(2).
220600170217           03  A31-PERIOD-CF-VALUECHG-7-SIGN    PICTURE X(1).
220700170217           03  A31-PERIOD-CF-TOTAMTINV-7        PICTURE 9(11)V9(2).
220800170217           03  A31-PERIOD-CF-TOTAMTINV-7-SIGN   PICTURE X(1).
220900170217           03  A31-PERIOD-CF-TOTAMTRED-7        PICTURE 9(11)V9(2).
221000170217           03  A31-PERIOD-CF-TOTAMTRED-7-SIGN   PICTURE X(1).
221100170217
221200170217      *PERIOD8 FIELDS
221300170217           03  A31-PERIOD-CODE-8                PICTURE X(5).
221400170217           03  A31-PERIOD-DESCRIPTION-8         PICTURE X(25).
221500170217           03  A31-PERIOD-CF-START-DATE-8       PICTURE 9(8).
221600170217           03  A31-PERIOD-CF-OPENINGVALUE-8     PICTURE 9(11)V9(2).
221700170217           03  A31-PERIOD-CF-OPNINGVL-8-SIGN    PICTURE X(1).
221800170217           03  A31-PERIOD-CF-VALUECHG-8         PICTURE 9(11)V9(2).
221900170217           03  A31-PERIOD-CF-VALUECHG-8-SIGN    PICTURE X(1).
222000170217           03  A31-PERIOD-CF-TOTAMTINV-8        PICTURE 9(11)V9(2).
222100170217           03  A31-PERIOD-CF-TOTAMTINV-8-SIGN   PICTURE X(1).
222200170217           03  A31-PERIOD-CF-TOTAMTRED-8        PICTURE 9(11)V9(2).
222300170217           03  A31-PERIOD-CF-TOTAMTRED-8-SIGN   PICTURE X(1).
222400170217
222500170217      *PERIOD9 FIELDS
222600170217           03  A31-PERIOD-CODE-9                PICTURE X(5).
222700170217           03  A31-PERIOD-DESCRIPTION-9         PICTURE X(25).
222800170217           03  A31-PERIOD-CF-START-DATE-9       PICTURE 9(8).
222900170217           03  A31-PERIOD-CF-OPENINGVALUE-9     PICTURE 9(11)V9(2).
223000170217           03  A31-PERIOD-CF-OPNINGVL-9-SIGN    PICTURE X(1).
223100170217           03  A31-PERIOD-CF-VALUECHG-9         PICTURE 9(11)V9(2).
223200170217           03  A31-PERIOD-CF-VALUECHG-9-SIGN    PICTURE X(1).
223300170217           03  A31-PERIOD-CF-TOTAMTINV-9        PICTURE 9(11)V9(2).
223400170217           03  A31-PERIOD-CF-TOTAMTINV-9-SIGN   PICTURE X(1).
223500170217           03  A31-PERIOD-CF-TOTAMTRED-9        PICTURE 9(11)V9(2).
223600170217           03  A31-PERIOD-CF-TOTAMTRED-9-SIGN   PICTURE X(1).
223700170217
223800170217      *PERIOD10 FIELDS
223900170217           03  A31-PERIOD-CODE-10               PICTURE X(5).
224000170217           03  A31-PERIOD-DESCRIPTION-10        PICTURE X(25).
224100170217           03  A31-PERIOD-CF-START-DATE-10      PICTURE 9(8).
224200170217           03  A31-PERIOD-CF-OPENINGVALUE-10    PICTURE 9(11)V9(2).
224300170217           03  A31-PERIOD-CF-OPNINGVL-10-SIGN   PICTURE X(1).
224400170217           03  A31-PERIOD-CF-VALUECHG-10        PICTURE 9(11)V9(2).
224500170217           03  A31-PERIOD-CF-VALUECHG-10-SIGN   PICTURE X(1).
224600170217           03  A31-PERIOD-CF-TOTAMTINV-10       PICTURE 9(11)V9(2).
224700170217           03  A31-PERIOD-CF-TOTAMTIV-10-SIGN   PICTURE X(1).
224800170217           03  A31-PERIOD-CF-TOTAMTRED-10       PICTURE 9(11)V9(2).
224900170217           03  A31-PERIOD-CF-TOTAMTRD-10-SIGN   PICTURE X(1).
225000170217           03  FILLER                           PICTURE X(336).
225100170217
225200170217      *COMMON FIELD LAYOUT FOR ALL SECTIONS
225300170217           03  A31-SECTION-SEQUENCE-2           PICTURE 9(6).
225400170217           03  A31-START-DATE                   PICTURE 9(08).
225500170217           03  A31-END-DATE                     PICTURE 9(08).
225600170217           03  A31-EOR-INDICATOR                PICTURE X(1).
225700170217      *RFS164528 -ENDS
225800170217
225900160229
226000160229      /
226100160229       01  ACCOUNT-ROR-FRQ-SECTION.
226200160229      ******************************************************************
226300160229      *                                                                *
226400160229      *    CPYJSISTMT: Statement File - Account Performance Record     *
226500160229      *    CREATED:    30/09/2003 (III)                                *
226600160229      *    CHANGED:    30/SEP/03 (MFH)                                 *
226700160229      *    LENGTH:     1100                                            *
226800160229      *    RFS   :     17626                                           *
226900160229      ******************************************************************
227000160229
227100160229      *****
227200160229      *****    - INVESTOR-NO will be formatted.
227300160229      *****    - ACCOUNT will be formatted.
227400160229      *****    - SECTION-NO will always be 33.
227500160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
227600160229      *****                       999999 will be reserved for the total.
227700160229      *****    - LANGUAGE will be one of the following.
227800160229      *****        'E' English
227900160229      *****        'F' French
228000160229      *****    - DEALER will be formatted.
228100160229      *****    - SALESREP will be formatted.
228200160229      *****
228300160229
228400160229           03  APF-INVESTOR-NO          PICTURE 9(9).
228500160229           03  APF-ACCOUNT              PICTURE 9(9).
228600160229           03  APF-SECTION-NO           PICTURE 9(2).
228700160229           03  APF-SECTION-SEQUENCE     PICTURE 9(6).
228800160229           03  APF-LANGUAGE             PICTURE X(1).
228900160229           03  APF-DEALER               PICTURE X(4).
229000160229           03  APF-SALESREP             PICTURE X(6).
229100160229
229200160229           03  APF-PERIOD-DATE-1         PICTURE 9(8).
229300160229           03  APF-PERIOD-CODE-1         PICTURE X(5).
229400160229           03  APF-PERIOD-DESCRIPTION-1  PICTURE X(25).
229500160229           03  APF-PERIOD-INDICATOR-1    PICTURE X(1).
229600160229           03  APF-PERIOD-ROR-1          PICTURE 9(8)V9(5).
229700160229           03  APF-PERIOD-ROR-SIGN-1     PICTURE X(1).
229800160229           03  APF-PERIOD-ROR-APPLICABLE-1 PICTURE X(1).
229900160229
230000160229           03  APF-PERIOD-DATE-2         PICTURE 9(8).
230100160229           03  APF-PERIOD-CODE-2         PICTURE X(5).
230200160229           03  APF-PERIOD-DESCRIPTION-2  PICTURE X(25).
230300160229           03  APF-PERIOD-INDICATOR-2    PICTURE X(1).
230400160229           03  APF-PERIOD-ROR-2          PICTURE 9(8)V9(5).
230500160229           03  APF-PERIOD-ROR-SIGN-2     PICTURE X(1).
230600160229           03  APF-PERIOD-ROR-APPLICABLE-2 PICTURE X(1).
230700160229
230800160229           03  APF-PERIOD-DATE-3         PICTURE 9(8).
230900160229           03  APF-PERIOD-CODE-3         PICTURE X(5).
231000160229           03  APF-PERIOD-DESCRIPTION-3  PICTURE X(25).
231100160229           03  APF-PERIOD-INDICATOR-3    PICTURE X(1).
231200160229           03  APF-PERIOD-ROR-3          PICTURE 9(8)V9(5).
231300160229           03  APF-PERIOD-ROR-SIGN-3     PICTURE X(1).
231400160229           03  APF-PERIOD-ROR-APPLICABLE-3 PICTURE X(1).
231500160229
231600160229           03  APF-PERIOD-DATE-4         PICTURE 9(8).
231700160229           03  APF-PERIOD-CODE-4         PICTURE X(5).
231800160229           03  APF-PERIOD-DESCRIPTION-4  PICTURE X(25).
231900160229           03  APF-PERIOD-INDICATOR-4    PICTURE X(1).
232000160229           03  APF-PERIOD-ROR-4          PICTURE 9(8)V9(5).
232100160229           03  APF-PERIOD-ROR-SIGN-4     PICTURE X(1).
232200160229           03  APF-PERIOD-ROR-APPLICABLE-4 PICTURE X(1).
232300160229
232400160229           03  APF-PERIOD-DATE-5         PICTURE 9(8).
232500160229           03  APF-PERIOD-CODE-5         PICTURE X(5).
232600160229           03  APF-PERIOD-DESCRIPTION-5  PICTURE X(25).
232700160229           03  APF-PERIOD-INDICATOR-5    PICTURE X(1).
232800160229           03  APF-PERIOD-ROR-5          PICTURE 9(8)V9(5).
232900160229           03  APF-PERIOD-ROR-SIGN-5     PICTURE X(1).
233000160229           03  APF-PERIOD-ROR-APPLICABLE-5 PICTURE X(1).
233100160229
233200160229           03  APF-PERIOD-DATE-6         PICTURE 9(8).
233300160229           03  APF-PERIOD-CODE-6         PICTURE X(5).
233400160229           03  APF-PERIOD-DESCRIPTION-6  PICTURE X(25).
233500160229           03  APF-PERIOD-INDICATOR-6    PICTURE X(1).
233600160229           03  APF-PERIOD-ROR-6          PICTURE 9(8)V9(5).
233700160229           03  APF-PERIOD-ROR-SIGN-6     PICTURE X(1).
233800160229           03  APF-PERIOD-ROR-APPLICABLE-6 PICTURE X(1).
233900160229
234000160229           03  APF-PERIOD-DATE-7         PICTURE 9(8).
234100160229           03  APF-PERIOD-CODE-7         PICTURE X(5).
234200160229           03  APF-PERIOD-DESCRIPTION-7  PICTURE X(25).
234300160229           03  APF-PERIOD-INDICATOR-7    PICTURE X(1).
234400160229           03  APF-PERIOD-ROR-7          PICTURE 9(8)V9(5).
234500160229           03  APF-PERIOD-ROR-SIGN-7     PICTURE X(1).
234600160229           03  APF-PERIOD-ROR-APPLICABLE-7 PICTURE X(1).
234700160229
234800160229           03  APF-PERIOD-DATE-8         PICTURE 9(8).
234900160229           03  APF-PERIOD-CODE-8         PICTURE X(5).
235000160229           03  APF-PERIOD-DESCRIPTION-8  PICTURE X(25).
235100160229           03  APF-PERIOD-INDICATOR-8    PICTURE X(1).
235200160229           03  APF-PERIOD-ROR-8          PICTURE 9(8)V9(5).
235300160229           03  APF-PERIOD-ROR-SIGN-8     PICTURE X(1).
235400160229           03  APF-PERIOD-ROR-APPLICABLE-8 PICTURE X(1).
235500160229
235600160229           03  APF-PERIOD-DATE-9         PICTURE 9(8).
235700160229           03  APF-PERIOD-CODE-9         PICTURE X(5).
235800160229           03  APF-PERIOD-DESCRIPTION-9  PICTURE X(25).
235900160229           03  APF-PERIOD-INDICATOR-9    PICTURE X(1).
236000160229           03  APF-PERIOD-ROR-9          PICTURE 9(8)V9(5).
236100160229           03  APF-PERIOD-ROR-SIGN-9     PICTURE X(1).
236200160229           03  APF-PERIOD-ROR-APPLICABLE-9 PICTURE X(1).
236300160229
236400160229           03  APF-PERIOD-DATE-10        PICTURE 9(8).
236500160229           03  APF-PERIOD-CODE-10        PICTURE X(5).
236600160229           03  APF-PERIOD-DESCRIPTION-10 PICTURE X(25).
236700160229           03  APF-PERIOD-INDICATOR-10   PICTURE X(1).
236800160229           03  APF-PERIOD-ROR-10         PICTURE 9(8)V9(5).
236900160229           03  APF-PERIOD-ROR-SIGN-10    PICTURE X(1).
237000160229           03  APF-PERIOD-ROR-APPLICABLE-10 PICTURE X(1).
237100160229
237200160229R18814     03  APF-IPS-GROUP-ID          PICTURE X(20).
237300160229
237400160229R18814     03  FILLER                    PICTURE X(730).
237500160229           03  APF-SECTION-SEQUENCE-2    PICTURE 9(6).
237600160229           03  APF-START-DATE            PICTURE 9(08).
237700160229           03  APF-END-DATE              PICTURE 9(08).
237800160229           03  APF-EOR-INDICATOR         PICTURE X(1).
237900160229
238000160229      /
238100160229       01  ACCOUNT-FUND-PERFORM-SECTION.
238200160229
238300160229      ******************************************************************
238400160229      *                                                                *
238500160229      *    CPYJSISTMT: Statement File - Account Fund Performance Record*
238600160229      *    CREATED:    22/11/1999 (III)                                *
238700160229      *    CHANGED:    23/JUN/98 (MFH)                                 *
238800160229      *    LENGTH:     750                                             *
238900160229      *                                                                *
239000160229      ******************************************************************
239100160229
239200160229      *****
239300160229      *****    - INVESTOR-NO will be formatted.
239400160229      *****    - ACCOUNT will be formatted.
239500160229      *****    - SECTION-NO will always be 35.
239600160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
239700160229      *****                       999999 will be reserved for the total.
239800160229      *****    - LANGUAGE will be one of the following.
239900160229      *****        'E' English
240000160229      *****        'F' French
240100160229      *****    - DEALER will be formatted.
240200160229      *****    - SALESREP will be formatted.
240300160229      *****
240400160229
240500160229           03  AFP-INVESTOR-NO          PICTURE 9(9).
240600160229           03  AFP-ACCOUNT              PICTURE 9(9).
240700160229           03  AFP-SECTION-NO           PICTURE 9(2).
240800160229           03  AFP-SECTION-SEQUENCE     PICTURE 9(6).
240900160229           03  AFP-LANGUAGE             PICTURE X(1).
241000160229           03  AFP-DEALER               PICTURE X(4).
241100160229           03  AFP-SALESREP             PICTURE X(6).
241200160229
241300160229           03  AFP-FUND-NO              PICTURE X(8).
241400160229           03  AFP-INVESTMENT-NAME      PICTURE X(35).
241500160229
241600160229           03  AFP-PERIOD-CODE-1        PICTURE X(5).
241700160229           03  AFP-PERIOD-DESCRIPTION-1 PICTURE X(25).
241800160229           03  AFP-PERIOD-INDICATOR-1   PICTURE X(1).
241900160229           03  AFP-PERIOD-ROR-1         PICTURE 9(8)V9(5).
242000160229           03  AFP-PERIOD-ROR-SIGN-1    PICTURE X(1).
242100160229           03  AFP-PERIOD-ROR-APPLICABLE-1 PICTURE X(1).
242200160229           03  AFP-PERIOD-CODE-2        PICTURE X(5).
242300160229           03  AFP-PERIOD-DESCRIPTION-2 PICTURE X(25).
242400160229           03  AFP-PERIOD-INDICATOR-2   PICTURE X(1).
242500160229           03  AFP-PERIOD-ROR-2         PICTURE 9(8)V9(5).
242600160229           03  AFP-PERIOD-ROR-SIGN-2    PICTURE X(1).
242700160229           03  AFP-PERIOD-ROR-APPLICABLE-2 PICTURE X(1).
242800160229           03  AFP-PERIOD-CODE-3        PICTURE X(5).
242900160229           03  AFP-PERIOD-DESCRIPTION-3 PICTURE X(25).
243000160229           03  AFP-PERIOD-INDICATOR-3   PICTURE X(1).
243100160229           03  AFP-PERIOD-ROR-3         PICTURE 9(8)V9(5).
243200160229           03  AFP-PERIOD-ROR-SIGN-3    PICTURE X(1).
243300160229           03  AFP-PERIOD-ROR-APPLICABLE-3 PICTURE X(1).
243400160229           03  AFP-PERIOD-CODE-4        PICTURE X(5).
243500160229           03  AFP-PERIOD-DESCRIPTION-4 PICTURE X(25).
243600160229           03  AFP-PERIOD-INDICATOR-4   PICTURE X(1).
243700160229           03  AFP-PERIOD-ROR-4         PICTURE 9(8)V9(5).
243800160229           03  AFP-PERIOD-ROR-SIGN-4    PICTURE X(1).
243900160229           03  AFP-PERIOD-ROR-APPLICABLE-4 PICTURE X(1).
244000160229           03  AFP-PERIOD-CODE-5        PICTURE X(5).
244100160229           03  AFP-PERIOD-DESCRIPTION-5 PICTURE X(25).
244200160229           03  AFP-PERIOD-INDICATOR-5   PICTURE X(1).
244300160229           03  AFP-PERIOD-ROR-5         PICTURE 9(8)V9(5).
244400160229           03  AFP-PERIOD-ROR-SIGN-5    PICTURE X(1).
244500160229           03  AFP-PERIOD-ROR-APPLICABLE-5 PICTURE X(1).
244600160229           03  AFP-PERIOD-CODE-6        PICTURE X(5).
244700160229           03  AFP-PERIOD-DESCRIPTION-6 PICTURE X(25).
244800160229           03  AFP-PERIOD-INDICATOR-6   PICTURE X(1).
244900160229           03  AFP-PERIOD-ROR-6         PICTURE 9(8)V9(5).
245000160229           03  AFP-PERIOD-ROR-SIGN-6    PICTURE X(1).
245100160229           03  AFP-PERIOD-ROR-APPLICABLE-6 PICTURE X(1).
245200160229           03  AFP-PERIOD-CODE-7        PICTURE X(5).
245300160229           03  AFP-PERIOD-DESCRIPTION-7 PICTURE X(25).
245400160229           03  AFP-PERIOD-INDICATOR-7   PICTURE X(1).
245500160229           03  AFP-PERIOD-ROR-7         PICTURE 9(8)V9(5).
245600160229           03  AFP-PERIOD-ROR-SIGN-7    PICTURE X(1).
245700160229           03  AFP-PERIOD-ROR-APPLICABLE-7 PICTURE X(1).
245800160229           03  AFP-PERIOD-CODE-8        PICTURE X(5).
245900160229           03  AFP-PERIOD-DESCRIPTION-8 PICTURE X(25).
246000160229           03  AFP-PERIOD-INDICATOR-8   PICTURE X(1).
246100160229           03  AFP-PERIOD-ROR-8         PICTURE 9(8)V9(5).
246200160229           03  AFP-PERIOD-ROR-SIGN-8    PICTURE X(1).
246300160229           03  AFP-PERIOD-ROR-APPLICABLE-8 PICTURE X(1).
246400160229           03  AFP-PERIOD-CODE-9        PICTURE X(5).
246500160229           03  AFP-PERIOD-DESCRIPTION-9 PICTURE X(25).
246600160229           03  AFP-PERIOD-INDICATOR-9   PICTURE X(1).
246700160229           03  AFP-PERIOD-ROR-9         PICTURE 9(8)V9(5).
246800160229           03  AFP-PERIOD-ROR-SIGN-9    PICTURE X(1).
246900160229           03  AFP-PERIOD-ROR-APPLICABLE-9 PICTURE X(1).
247000160229           03  AFP-PERIOD-CODE-10       PICTURE X(5).
247100160229           03  AFP-PERIOD-DESCRIPTION-10  PICTURE X(25).
247200160229           03  AFP-PERIOD-INDICATOR-10  PICTURE X(1).
247300160229           03  AFP-PERIOD-ROR-10        PICTURE 9(8)V9(5).
247400160229           03  AFP-PERIOD-ROR-SIGN-10   PICTURE X(1).
247500160229           03  AFP-PERIOD-ROR-APPLICABLE-10 PICTURE X(1).
247600160229R17165     03  FILLER                   PICTURE X(787).
247700160229           03  AFP-SECTION-SEQUENCE-2   PICTURE 9(6).
247800160229           03  AFP-START-DATE           PICTURE 9(08).
247900160229           03  AFP-END-DATE             PICTURE 9(08).
248000160229           03  AFP-EOR-INDICATOR        PICTURE X(1).
248100160229
248200160229
248300160229      /
248400160229       01  ACCOUNT-FUND-ROR-FRQ-SECTION.
248500160229      ******************************************************************
248600160229      *                                                                *
248700160229      *    CPYJSISTMT: Statement File - Account FUND Performance Record*
248800160229      *    CREATED:    30/09/2003 (III)                                *
248900160229      *    CHANGED:    30/SEP/03 (MFH)                                 *
249000160229      *    LENGTH:     1100                                            *
249100160229      *    RFS   :     17626                                           *
249200160229      ******************************************************************
249300160229
249400160229      *****
249500160229      *****    - INVESTOR-NO will be formatted.
249600160229      *****    - ACCOUNT will be formatted.
249700160229      *****    - SECTION-NO will always be 38.
249800160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
249900160229      *****                       999999 will be reserved for the total.
250000160229      *****    - LANGUAGE will be one of the following.
250100160229      *****        'E' English
250200160229      *****        'F' French
250300160229      *****    - DEALER will be formatted.
250400160229      *****    - SALESREP will be formatted.
250500160229      *****
250600160229
250700160229           03  AFPF-INVESTOR-NO          PICTURE 9(9).
250800160229           03  AFPF-ACCOUNT              PICTURE 9(9).
250900160229           03  AFPF-SECTION-NO           PICTURE 9(2).
251000160229           03  AFPF-SECTION-SEQUENCE     PICTURE 9(6).
251100160229           03  AFPF-LANGUAGE             PICTURE X(1).
251200160229           03  AFPF-DEALER               PICTURE X(4).
251300160229           03  AFPF-SALESREP             PICTURE X(6).
251400160229
251500160229           03  AFPF-FUND-NO              PICTURE X(8).
251600160229           03  AFPF-INVESTMENT-NAME      PICTURE X(35).
251700160229
251800160229           03  AFPF-PERIOD-DATE-1         PICTURE 9(8).
251900160229           03  AFPF-PERIOD-CODE-1         PICTURE X(5).
252000160229           03  AFPF-PERIOD-DESCRIPTION-1  PICTURE X(25).
252100160229           03  AFPF-PERIOD-INDICATOR-1    PICTURE X(1).
252200160229           03  AFPF-PERIOD-ROR-1          PICTURE 9(8)V9(5).
252300160229           03  AFPF-PERIOD-ROR-SIGN-1     PICTURE X(1).
252400160229           03  AFPF-PERIOD-ROR-APPLICABLE-1 PICTURE X(1).
252500160229
252600160229           03  AFPF-PERIOD-DATE-2         PICTURE 9(8).
252700160229           03  AFPF-PERIOD-CODE-2         PICTURE X(5).
252800160229           03  AFPF-PERIOD-DESCRIPTION-2  PICTURE X(25).
252900160229           03  AFPF-PERIOD-INDICATOR-2    PICTURE X(1).
253000160229           03  AFPF-PERIOD-ROR-2          PICTURE 9(8)V9(5).
253100160229           03  AFPF-PERIOD-ROR-SIGN-2     PICTURE X(1).
253200160229           03  AFPF-PERIOD-ROR-APPLICABLE-2 PICTURE X(1).
253300160229
253400160229           03  AFPF-PERIOD-DATE-3         PICTURE 9(8).
253500160229           03  AFPF-PERIOD-CODE-3         PICTURE X(5).
253600160229           03  AFPF-PERIOD-DESCRIPTION-3  PICTURE X(25).
253700160229           03  AFPF-PERIOD-INDICATOR-3    PICTURE X(1).
253800160229           03  AFPF-PERIOD-ROR-3          PICTURE 9(8)V9(5).
253900160229           03  AFPF-PERIOD-ROR-SIGN-3     PICTURE X(1).
254000160229           03  AFPF-PERIOD-ROR-APPLICABLE-3 PICTURE X(1).
254100160229
254200160229           03  AFPF-PERIOD-DATE-4         PICTURE 9(8).
254300160229           03  AFPF-PERIOD-CODE-4         PICTURE X(5).
254400160229           03  AFPF-PERIOD-DESCRIPTION-4  PICTURE X(25).
254500160229           03  AFPF-PERIOD-INDICATOR-4    PICTURE X(1).
254600160229           03  AFPF-PERIOD-ROR-4          PICTURE 9(8)V9(5).
254700160229           03  AFPF-PERIOD-ROR-SIGN-4     PICTURE X(1).
254800160229           03  AFPF-PERIOD-ROR-APPLICABLE-4 PICTURE X(1).
254900160229
255000160229           03  AFPF-PERIOD-DATE-5         PICTURE 9(8).
255100160229           03  AFPF-PERIOD-CODE-5         PICTURE X(5).
255200160229           03  AFPF-PERIOD-DESCRIPTION-5  PICTURE X(25).
255300160229           03  AFPF-PERIOD-INDICATOR-5    PICTURE X(1).
255400160229           03  AFPF-PERIOD-ROR-5          PICTURE 9(8)V9(5).
255500160229           03  AFPF-PERIOD-ROR-SIGN-5     PICTURE X(1).
255600160229           03  AFPF-PERIOD-ROR-APPLICABLE-5 PICTURE X(1).
255700160229
255800160229           03  AFPF-PERIOD-DATE-6         PICTURE 9(8).
255900160229           03  AFPF-PERIOD-CODE-6         PICTURE X(5).
256000160229           03  AFPF-PERIOD-DESCRIPTION-6  PICTURE X(25).
256100160229           03  AFPF-PERIOD-INDICATOR-6    PICTURE X(1).
256200160229           03  AFPF-PERIOD-ROR-6          PICTURE 9(8)V9(5).
256300160229           03  AFPF-PERIOD-ROR-SIGN-6     PICTURE X(1).
256400160229           03  AFPF-PERIOD-ROR-APPLICABLE-6 PICTURE X(1).
256500160229
256600160229           03  AFPF-PERIOD-DATE-7         PICTURE 9(8).
256700160229           03  AFPF-PERIOD-CODE-7         PICTURE X(5).
256800160229           03  AFPF-PERIOD-DESCRIPTION-7  PICTURE X(25).
256900160229           03  AFPF-PERIOD-INDICATOR-7    PICTURE X(1).
257000160229           03  AFPF-PERIOD-ROR-7          PICTURE 9(8)V9(5).
257100160229           03  AFPF-PERIOD-ROR-SIGN-7     PICTURE X(1).
257200160229           03  AFPF-PERIOD-ROR-APPLICABLE-7 PICTURE X(1).
257300160229
257400160229           03  AFPF-PERIOD-DATE-8         PICTURE 9(8).
257500160229           03  AFPF-PERIOD-CODE-8         PICTURE X(5).
257600160229           03  AFPF-PERIOD-DESCRIPTION-8  PICTURE X(25).
257700160229           03  AFPF-PERIOD-INDICATOR-8    PICTURE X(1).
257800160229           03  AFPF-PERIOD-ROR-8          PICTURE 9(8)V9(5).
257900160229           03  AFPF-PERIOD-ROR-SIGN-8     PICTURE X(1).
258000160229           03  AFPF-PERIOD-ROR-APPLICABLE-8 PICTURE X(1).
258100160229
258200160229           03  AFPF-PERIOD-DATE-9         PICTURE 9(8).
258300160229           03  AFPF-PERIOD-CODE-9         PICTURE X(5).
258400160229           03  AFPF-PERIOD-DESCRIPTION-9  PICTURE X(25).
258500160229           03  AFPF-PERIOD-INDICATOR-9    PICTURE X(1).
258600160229           03  AFPF-PERIOD-ROR-9          PICTURE 9(8)V9(5).
258700160229           03  AFPF-PERIOD-ROR-SIGN-9     PICTURE X(1).
258800160229           03  AFPF-PERIOD-ROR-APPLICABLE-9 PICTURE X(1).
258900160229
259000160229           03  AFPF-PERIOD-DATE-10        PICTURE 9(8).
259100160229           03  AFPF-PERIOD-CODE-10        PICTURE X(5).
259200160229           03  AFPF-PERIOD-DESCRIPTION-10 PICTURE X(25).
259300160229           03  AFPF-PERIOD-INDICATOR-10   PICTURE X(1).
259400160229           03  AFPF-PERIOD-ROR-10         PICTURE 9(8)V9(5).
259500160229           03  AFPF-PERIOD-ROR-SIGN-10    PICTURE X(1).
259600160229           03  AFPF-PERIOD-ROR-APPLICABLE-10 PICTURE X(1).
259700160229
259800160229           03  FILLER                     PICTURE X(707).
259900160229           03  AFPF-SECTION-SEQUENCE-2    PICTURE 9(6).
260000160229           03  AFPF-START-DATE            PICTURE 9(08).
260100160229           03  AFPF-END-DATE              PICTURE 9(08).
260200160229           03  AFPF-EOR-INDICATOR         PICTURE X(1).
260300160229
260400160229      /
260500160229       01  SEG-FUND-CONTRACT-DETAILS.
260600160229
260700160229      ******************************************************************
260800160229      *                                                                *
260900160229      *    CPYJSISTMT: Statement File - Seg Fund Contract Record       *
261000160229      *    CREATED:    12/10/2000                                      *
261100160229      *    CHANGED:    16/APR/97 (MFH)                                 *
261200160229      *    CHANGED:    07/MAR/06                                       *
261300160229      *    LENGTH:     850                                             *
261400160229      *                                                                *
261500160229      ******************************************************************
261600160229
261700160229      *****
261800160229      *****    - INVESTOR-NO will be formatted.
261900160229      *****    - ACCOUNT will be formatted.
262000160229      *****    - SECTION-NO will always be 40.
262100160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
262200160229      *****    - LANGUAGE will always be formatted.
262300160229      *****    - DEALER will always be formatted.
262400160229      *****    - SALESREP will always be formatted.
262500160229      *****
262600160229
262700160229           03  CD-INVESTOR-NO          PICTURE 9(9).
262800160229           03  CD-ACCOUNT              PICTURE 9(9).
262900160229           03  CD-SECTION-NO           PICTURE 9(2).
263000160229           03  CD-SECTION-SEQUENCE     PICTURE 9(6).
263100160229           03  CD-LANGUAGE             PICTURE X(1).
263200160229           03  CD-DEALER               PICTURE X(4).
263300160229           03  CD-SALESREP             PICTURE X(6).
263400160229           03  CD-CONTRACT-NO          PICTURE 9(9).
263500160229           03  CD-CONTRACT-XREF        PICTURE X(9).
263600160229           03  CD-START-DATE1          PICTURE 9(8).
263700160229           03  CD-MATURITY-DATE        PICTURE 9(8).
263800160229           03  CD-DEPOSIT-MATURITY-DATE PICTURE 9(8).
263900160229           03  CD-DEPOSIT-RENEWAL-DATE PICTURE 9(8).
264000160229           03  CD-RESETS-ALLOWED       PICTURE 9(3).
264100160229           03  CD-RESETS-EXCERCISED    PICTURE 9(3).
264200160229           03  CD-RESETS-REMAINING     PICTURE 9(3).
264300160229           03  CD-GUARANTEED-BASE-VALUE     PICTURE 9(11)V9(2).
264400160229           03  CD-GUARANTEED-MATURITY-VALUE PICTURE 9(11)V9(2).
264500160229           03  CD-GUARANTEED-DEATH-VALUE    PICTURE 9(11)V9(2).
264600160229           03  CD-NET-DEPOSIT-VALUE         PICTURE 9(11)V9(2).
264700160229           03  CD-FIRST-TRADE-DATE     PICTURE 9(8).
264800160229           03  CD-GUARANTEED-BASE-VALUE-OPEN PICTURE 9(11)V9(2).
264900160229           03  CD-GUARANTEED-BASE-VALUE-CHG  PICTURE 9(11)V9(2).
265000160229           03  CD-G-B-V-C-SIGN               PICTURE X(1).
265100160229           03  CD-CONTRACT-MARKET-VALUE      PICTURE 9(11)V9(2).
265200160229           03  CD-LAST-RESET-DATE            PICTURE 9(8).
265300160229           03  CD-CONTRACT-CLOSE-DATE        PICTURE 9(8).
265400160229R22856     03  CD-GUAR-DTH-BENEFIT-VALUE     PICTURE S9(11)V9(2).
265500160229   ¦       03  CD-GUAR-DTH-BENEFIT-VALUE-IND PICTURE X(1).
265600160229   ¦       03  CD-GUAR-MAT-BENEFIT-VALUE     PICTURE S9(11)V9(2).
265700160229   ¦       03  CD-GUAR-MAT-BENEFIT-VALUE-IND PICTURE X(1).
265800160229   ¦       03  CD-SUM-ALL-AWD                PICTURE S9(11)V9(2).
265900160229R22856     03  CD-SUM-ALL-AWD-IND            PICTURE X(1).
26600016022928107      03  CD-GBV-INDICATOR        PICTURE X(1).
26610016022928107      03  CD-VALIDATION-DATE      PICTURE 9(08).
266200160229      *-- RFS31514 Begins
266300160229           03  ld-AwdCarOvr            PICTURE 9(11)V99.
266400160229      *-- RFS37754 Begins
266500160229           03  Cd-TopUpNotionalAmt     PICTURE S9(11)V99.
266600160229      *-- RFS47406 Start
266700160229           03  CD-Series-ID                    PICTURE X(05).
266800160229           03  CD-Series-Description           PICTURE X(45).
266900160229           03  CD-Contract-Year                PICTURE S9(03).
267000160229           03  CD-Maturity-Benefit-Percentage  PICTURE 9(3)V99.
267100160229           03  CD-Death-Benefit-Percentage     PICTURE 9(3)V99.
267200160229           03  CD-GW-Base-Value                PICTURE S9(11)V99.
26730016022978161      03  CD-GW-Base-Value-Sign           PICTURE X(1).
267400160229           03  CD-FIE-Base-Value               PICTURE S9(11)V99.
26750016022978161      03  CD-FIE-Base-Value-Sign          PICTURE X(1).
267600160229           03  CD-GLWA-Rate                    PICTURE 9(03)V99.
26770016022978161      03  CD-GLWA-Rate-Sign               PICTURE X(1).
267800160229           03  CD-GLWA-Rate-Locked-In          PICTURE X(01).
267900160229           03  CD-GLWA-Previous-YR-Value       PICTURE S9(11)V99.
26800016022978161      03  CD-GLWA-Previous-YR-Value-Sign  PICTURE X(1).
268100160229           03  CD-GLWA-Current-YR-Value        PICTURE S9(11)V99.
26820016022978161      03  CD-GLWA-Current-YR-Value-Sign   PICTURE X(1).
268300160229           03  CD-GLWA-Remaining-YTD-Value     PICTURE S9(11)V99.
26840016022978161      03  CD-GLWA-Remaining-YTD-Value-Sg  PICTURE X(1).
268500160229           03  CD-GLWA-Withdrawn-YTD-Value     PICTURE S9(11)V99.
26860016022978161      03  CD-GLWA-Withdrawn-YTD-Value-Sg  PICTURE X(1).
268700160229           03  CD-Excess-GLWA-Withdrawn-YTD    PICTURE S9(11)V99.
26880016022978161      03  CD-Excess-GLWA-Withdrawn-YTD-S  PICTURE X(1).
268900160229           03  CD-GLWA-AWD-Withdrawal-Option   PICTURE X(5).
269000160229           03  CD-GLWA-AWD-Withdrawal-Amount   PICTURE S9(11)V99.
26910016022978161      03  CD-GLWA-AWD-Withdrawal-Amt-Sg   PICTURE X(1).
269200160229           03  CD-GLWA-AWD-Split-Option        PICTURE X(01).
269300160229           03  CD-GLWA-Init-Deposit-Days       PICTURE 9(05).
269400160229           03  CD-GLWA-Init-Deposit-End-Dte    PICTURE 9(08).
269500160229           03  CD-GLWA-Annuitant-Calendar-Age  PICTURE 9(03).
269600160229           03  CD-GLWA-Eligible-Date           PICTURE 9(08).
269700160229           03  CD-GLWA-GLWA-Force-Start-Date   PICTURE 9(08).
269800160229           03  CD-GWA-Start-Date               PICTURE 9(08).
269900160229      *RFS50993 - Start
270000160229      *    03  FILLER                          PICTURE X(819).
270100160229           03  CD-DOB-FOR-CALC                 PICTURE S9(08).
270200160229           03  CD-NEW-DEPOSIT                  PICTURE X(1).
270300160229           03  CD-SERIES-DESCRIPTION-FRENCH    PICTURE X(45).
270400160229R52815     03  CD-Bonus-Eligibility            PICTURE X(01).
270500160229  |        03  CD-Bonus-Added-To-GWBASE        PICTURE 9(11)V9(2).
270600160229  |        03  CD-Original-GLWA                PICTURE 9(11)V9(2).
270700160229  |        03  CD-GLWA-RIF-Min                 PICTURE 9(11)V9(2).
270800160229  |        03  CD-Qualify-Nominee-RRIF         PICTURE X(1).
270900160229R52815     03  CD-Qualify-DOB-Nominee-RRIF     PICTURE X(1).
271000160229R64262     03  CD-LAST-GBV-RESET-DATE          PICTURE 9(8).
271100160229  ]        03  CD-LAST-GDV-RESET-DATE          PICTURE 9(8).
271200160229  ]        03  CD-AUTO-GBV-RESET-FREQ          PICTURE 9(2).
271300160229  V        03  CD-AUTO-GDV-RESET-FREQ          PICTURE 9(2).
271400160229R64262*    03  FILLER                          PICTURE X(703).
271500160229R78161*    03  FILLER                          PICTURE X(703).
271600160229R78161* 74626   03  FILLER                          PICTURE X(694).
271700160229R74626     03  CD-CONTRACT-STATUS              PICTURE X(1).
271800160229R80555     03  CD-GPP-Flag                     PICTURE X(1).
271900160229R80555     03  CD-Orig-Curr-Yr-GLWA            PICTURE 9(11)V99.
272000160229R80555     03  CD-Orig-Curr-Yr-GLWA-Sign       PICTURE X(1).
272100160229R80555     03  CD-GLWA-YE-Percentage           PICTURE 9(3)V99.
272200160229R80605     03  CD-GDV-AWD-CARRIED-OVER         PICTURE S9(11)V99.
272300160229  |        03  CD-GDV-SUM-ALL-AWD              PICTURE S9(11)V99.
272400160229      * RFS110904 - Start
272500160229           03  CD-GUARANTEE-BENEFIT            PICTURE X(01).
272600160229      * RFS120931 - START
272700160229      * RFS130402 - START
272800160229130640     03  CD-MAX-WITHDRAWAL-AMOUNT        PICTURE S9(11)V99.
272900160229130640     03  CD-YTD-WITHDRAWN-AMOUNT         PICTURE S9(11)V99.
273000160229      * RFS 142884 STARTS
273100160229           03  CD-GWA-CURRENT-YR-VALUE         PICTURE 9(11)V99.
273200160229           03  CD-GWA-CURRENT-YR-VALUE-SIGN    PICTURE X(1).
273300160229           03  CD-LWA-CURRENT-YR-VALUE         PICTURE 9(11)V99.
273400160229           03  CD-LWA-CURRENT-YR-VALUE-SIGN    PICTURE X(1).
273500160229           03  CD-EWA-CURRENT-YR-VALUE         PICTURE 9(11)V99.
273600160229           03  CD-EWA-CURRENT-YR-VALUE-SIGN    PICTURE X(1).
273700160229      * RFS148595 - STARTS
273800160229           03 CD-GWA-ORIGINAL-VALUE   PIC 9(11)V9(2).
273900160229           03 CD-GWA-ORIGINAL-VALUE-SIGN  PIC X(1).
274000160229           03 CD-LWA-ORIGINAL-VALUE   PIC 9(11)V9(2).
274100160229           03 CD-LWA-ORIGINAL-VALUE-SIGN  PIC X(1).
274200160229           03 CD-EWA-ORIGINAL-VALUE   PIC 9(11)V9(2).
274300160229           03 CD-EWA-ORIGINAL-VALUE-SIGN  PIC X(1).
274400160229      * RFS148595 - ENDS
274500160229
274600160229130640*    03  CD-MAX-WITHDRAWAL-AMOUNT        PICTURE  9(11).99.
274700160229130640*    03  CD-YTD-WITHDRAWN-AMOUNT         PICTURE  9(11).99.
274800160229      * RFS130402 - END
274900160229R80605*    03  FILLER                          PICTURE X(647).
275000160229      * RFS130402 - START
275100160229130640*    03  FILLER                          PICTURE X(620).
275200160229130640*    03  FILLER                          PICTURE X(618).
275300160229148595*    03  FILLER                          PICTURE X(578).
275400180619      *RFS177152 START
275500180530148595*    03  FILLER                          PICTURE X(536).
275600180627           03  CD-GUAR-DEATH-VALUE-OPEN        PIC 9(11)V9(2).
275700180627           03  CD-GUAR-MAT-VALUE-OPEN          PIC 9(11)V9(2).
275800180704           03  CD-ATTAINED-AGE-1               PIC 9(3).
275900180627           03  CD-LWA-PROJECTION-1             PIC 9(11)V9(2).
276000180704           03  CD-ATTAINED-AGE-2               PIC 9(3).
276100180627           03  CD-LWA-PROJECTION-2             PIC 9(11)V9(2).
276200180704           03  CD-ATTAINED-AGE-3               PIC 9(3).
276300180627           03  CD-LWA-PROJECTION-3             PIC 9(11)V9(2).
276400180704           03  CD-ATTAINED-AGE-4               PIC 9(3).
276500180627           03  CD-LWA-PROJECTION-4             PIC 9(11)V9(2).
276600210823      * RFS186199 - Begin
276700210823      *    03  FILLER                          PICTURE X(446).
276800210823           03  CD-MDBA-INFLADJ-RESET-REASON    PIC X(5).
276900210823           03  CD-MDBA-NINFADJ-RESET-REASON    PIC X(5).
277000210823           03  CD-MDBA-RESET-DATE              PIC 9(8).
277100210823           03  CD-PREVIOUS-MDBA-RESET-AMT      PIC 9(11)V9(2).
277200210823           03  CD-RESULTING-MDBA-RESET-AMT     PIC 9(11)V9(2).
277300210823           03  CD-MDBA-INFL-ADJ-AMT            PIC 9(11)V9(2).
277400210823           03  CD-MDBA-NON-INFL-ADJ-AMT        PIC 9(11)V9(2).
277500210823           03  FILLER                          PICTURE X(376).
277600210823      * RFS186199 - End
277700180530      *RFS177152 END
277800160229      * RFS 142884 ENDS
277900160229      * RFS130402 - END
278000160229      * RFS110904 - End
278100160229      * RFS120931 - END
278200160229R74626* 80555   03  FILLER                          PICTURE X(693).
278300160229           03  CD-SECTION-SEQUENCE-2   PICTURE 9(6).
278400160229           03  CD-START-DATE           PICTURE 9(08).
278500160229           03  CD-END-DATE             PICTURE 9(08).
278600160229           03  CD-EOR-INDICATOR        PICTURE X(1).
278700160229
278800160229      /
278900160229       01  SEG-FUND-CONTRACT-INVESTMENT.
279000160229
279100160229      ******************************************************************
279200160229      *                                                                *
279300160229      *    CPYJSISTMT: Statement File - Seg Fund Contract Investment   *
279400160229      *    CREATED:    12/10/2000                                      *
279500160229      *    CHANGED:    dd/mmm/yy (MFH)                                 *
279600160229      *    LENGTH:     850                                             *
279700160229      *                                                                *
279800160229      ******************************************************************
279900160229
280000160229      *****
280100160229      *****    - INVESTOR-NO will be formatted.
280200160229      *****    - ACCOUNT will be formatted.
280300160229      *****    - SECTION-NO will always be 43.
280400160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
280500160229      *****    - LANGUAGE will always be formatted.
280600160229      *****    - DEALER will always be formatted.
280700160229      *****    - SALESREP will always be formatted.
280800160229      *****
280900160229
281000160229           03  CI-INVESTOR-NO          PICTURE 9(9).
281100160229           03  CI-ACCOUNT              PICTURE 9(9).
281200160229           03  CI-SECTION-NO           PICTURE 9(2).
281300160229           03  CI-SECTION-SEQUENCE     PICTURE 9(6).
281400160229           03  CI-LANGUAGE             PICTURE X(1).
281500160229           03  CI-DEALER               PICTURE X(4).
281600160229           03  CI-SALESREP             PICTURE X(6).
281700160229           03  CI-CONTRACT-NO          PICTURE 9(9).
281800160229           03  CI-CONTRACT-XREF        PICTURE X(9).
281900160229           03  CI-INVESTMENT-CODE      PICTURE X(5).
282000160229           03  CI-START-DATE1          PICTURE 9(8).
282100160229           03  CI-MATURITY-DATE        PICTURE 9(8).
282200160229           03  CI-DEPOSIT-MATURITY-DATE PICTURE 9(8).
282300160229           03  CI-DEPOSIT-RENEWAL-DATE PICTURE 9(8).
282400160229           03  CI-RESETS-ALLOWED       PICTURE 9(3).
282500160229           03  CI-RESETS-EXCERCISED    PICTURE 9(3).
282600160229           03  CI-RESETS-REMAINING     PICTURE 9(3).
282700160229           03  CI-GUARANTEED-BASE-VALUE     PICTURE 9(11)V9(2).
282800160229           03  CI-GUARANTEED-MATURITY-VALUE PICTURE 9(11)V9(2).
282900160229           03  CI-GUARANTEED-DEATH-VALUE    PICTURE 9(11)V9(2).
283000160229           03  CI-NET-DEPOSIT-VALUE         PICTURE 9(11)V9(2).
283100160229           03  CI-UNITS                PICTURE 9(11)V9(3).
283200160229           03  CI-MARKET-VALUE         PICTURE 9(11)V9(2).
283300160229R96081     03  CI-EW-Alert             PIC X(50).
283400160229R96081     03  FILLER                  PICTURE X(1097).
283500160229R17165*    03  FILLER                  PICTURE X(1147).
283600160229           03  CI-SECTION-SEQUENCE-2   PICTURE 9(6).
283700160229           03  CI-START-DATE           PICTURE 9(8).
283800160229           03  CI-END-DATE             PICTURE 9(8).
283900160229           03  CI-EOR-INDICATOR        PICTURE X(1).
284000160229
284100160229      /
284200160229       01  ANNUITANT-DETAILS.
284300160229
284400160229      ******************************************************************
284500160229      *                                                                *
284600160229      *    CPYJSISTMT: Statement File - Seg Fund Contract Investment   *
284700160229      *    CREATED:    12/10/2000                                      *
284800160229      *    CHANGED:    dd/mmm/yy (MFH)                                 *
284900160229      *    LENGTH:     850                                             *
285000160229      *                                                                *
285100160229      ******************************************************************
285200160229
285300160229      *****
285400160229      *****    - INVESTOR-NO will be formatted.
285500160229      *****    - ACCOUNT will be formatted.
285600160229      *****    - SECTION-NO will always be 45.
285700160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
285800160229      *****    - LANGUAGE will always be formatted.
285900160229      *****    - DEALER will always be formatted.
286000160229      *****    - SALESREP will always be formatted.
286100160229      *****
286200160229
286300160229           03  AN-INVESTOR-NO          PICTURE 9(9).
286400160229           03  AN-ACCOUNT              PICTURE 9(9).
286500160229           03  AN-SECTION-NO           PICTURE 9(2).
286600160229           03  AN-SECTION-SEQUENCE     PICTURE 9(6).
286700160229           03  AN-LANGUAGE             PICTURE X(1).
286800160229           03  AN-DEALER               PICTURE X(4).
286900160229           03  AN-SALESREP             PICTURE X(6).
287000160229           03  AN-CONTRACT-NO          PICTURE 9(9).
287100160229           03  AN-CONTRACT-XREF        PICTURE X(9).
287200160229           03  AN-SUCCESSOR-ANNUITANT  PICTURE X(1).
287300160229           03  AN-ANNUITANT-STATUS     PICTURE X(1).
287400160229           03  AN-LAST-NAME            PICTURE X(20).
287500160229           03  AN-INITIALS             PICTURE X(3).
287600160229           03  AN-FIRST-NAME           PICTURE X(20).
287700160229           03  AN-SALUTATION-CODE      PICTURE X(4).
287800160229           03  AN-SOCIAL-INSURANCE-NO  PICTURE 9(9).
287900160229           03  AN-DATE-OF-BIRTH        PICTURE 9(8).
288000160229           03  AN-SEX                  PICTURE X(1).
288100160229           03  AN-LANGUAGE-CODE        PICTURE X(1).
288200160229           03  AN-DEATH-PROCESS-DATE   PICTURE 9(8).
288300160229           03  AN-ANNUITANT-DEATH      PICTURE 9(8).
288400160229           03  AN-ADDRESS1             PICTURE X(40).
288500160229           03  AN-ADDRESS2             PICTURE X(40).
288600160229           03  AN-ADDRESS3             PICTURE X(40).
288700160229           03  AN-ADDRESS4             PICTURE X(40).
288800160229           03  AN-ADDRESS5             PICTURE X(40).
288900160229           03  AN-POSTAL-CODE          PICTURE X(10).
289000160229           03  AN-HOLD-MAIL            PICTURE X(1).
289100160229R17165     03  FILLER                  PICTURE X(977).
289200160229           03  AN-SECTION-SEQUENCE-2   PICTURE 9(6).
289300160229           03  AN-START-DATE           PICTURE 9(8).
289400160229           03  AN-END-DATE             PICTURE 9(8).
289500160229           03  AN-EOR-INDICATOR        PICTURE X(1).
289600160229
289700160229      /
289800160229       01  ROLLOVER-SECTION.
289900160229
290000160229      ******************************************************************
290100160229      *                                                                *
290200160229      *    CPYJSISTMT: Statement File - Rollover Record                *
290300160229      *    CREATED:    DD/MMM/YY (III)                                 *
290400160229      *    CHANGED:    16/APR/97 (MFH)                                 *
290500160229      *    LENGTH:     750                                             *
290600160229      *                                                                *
290700160229      ******************************************************************
290800160229
290900160229      *****
291000160229      *****    - INVESTOR-NO will be formatted.
291100160229      *****    - ACCOUNT will be formatted.
291200160229      *****    - SECTION-NO will always be 50.
291300160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
291400160229      *****    - LANGUAGE will always be formatted.
291500160229      *****    - DEALER will always be formatted.
291600160229      *****    - SALESREP will always be formatted.
291700160229      *****
291800160229
291900160229           03  AR-INVESTOR-NO          PICTURE 9(9).
292000160229           03  AR-ACCOUNT              PICTURE 9(9).
292100160229           03  AR-SECTION-NO           PICTURE 9(2).
292200160229           03  AR-SECTION-SEQUENCE     PICTURE 9(6).
292300160229           03  AR-LANGUAGE             PICTURE X(1).
292400160229           03  AR-DEALER               PICTURE X(4).
292500160229           03  AR-SALESREP             PICTURE X(6).
292600160229           03  AR-ROLLOVER-NO          PICTURE 9(7).
292700160229           03  AR-INSTITUTION          PICTURE X(35).
292800160229           03  AR-INSTITUTION-PLAN-NO  PICTURE X(15).
292900160229           03  AR-CREATION-DATE        PICTURE 9(8).
293000160229           03  AR-ROLLOVER-TYPE        PICTURE X(1).
293100160229           03  AR-ROLLOVER-STATUS      PICTURE X(1).
293200160229           03  AR-FOLLOWUP-DATE        PICTURE 9(8).
293300160229           03  AR-APPROXIMATE-VALUE    PICTURE 9(9)V9(2).
293400160229           03  AR-APPROX-VALUE-SIGN    PICTURE X(1).
293500160229           03  AR-ACTUAL-VALUE         PICTURE 9(9)V9(2).
293600160229           03  AR-ACTUAL-VALUE-SIGN    PICTURE X(1).
293700160229           03  AR-NO-OF-RECORDS        PICTURE 9(4).
293800160229R17165     03  FILLER                  PICTURE X(1187).
293900160229           03  AR-SECTION-SEQUENCE-2   PICTURE 9(6).
294000160229           03  AR-START-DATE           PICTURE 9(08).
294100160229           03  AR-END-DATE             PICTURE 9(08).
294200160229           03  AR-EOR-INDICATOR        PICTURE X(1).
294300160229      /
294400160229       01  RRIF-DETAIL-SECTION.
294500160229
294600160229      ******************************************************************
294700160229      *                                                                *
294800160229      *    CPYJSISTMT: Statement File - RRIF Detail Record             *
294900160229      *    CREATED:    17/APR/97 (MFH)                                 *
295000160229      *    CHANGED:    12/JUN/98 (MFH)                                 *
295100160229      *    LENGTH:     750                                             *
295200160229      *                                                                *
295300160229      ******************************************************************
295400160229
295500160229      *****
295600160229      *****    - INVESTOR-NO will be formatted.
295700160229      *****    - ACCOUNT will be formatted.
295800160229      *****    - SECTION-NO will always be 60.
295900160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
296000160229      *****    - LANGUAGE will be one of the following.
296100160229      *****        'E' English
296200160229      *****        'F' French
296300160229      *****    - DEALER will be formatted.
296400160229      *****    - SALESREP will be formatted.
296500160229      *****
296600160229
296700160229           03  AW-INVESTOR-NO          PICTURE 9(9).
296800160229           03  AW-ACCOUNT              PICTURE 9(9).
296900160229           03  AW-SECTION-NO           PICTURE 9(2).
297000160229           03  AW-SECTION-SEQUENCE     PICTURE 9(6).
297100160229           03  AW-LANGUAGE             PICTURE X(1).
297200160229           03  AW-DEALER               PICTURE X(4).
297300160229           03  AW-SALESREP             PICTURE X(6).
297400160229           03  AW-MINIMUM-ANNUAL-PAYMENT
297500160229                                       PICTURE 9(9)V9(2).
297600160229           03  AW-AGE                  PICTURE 9(3).
297700160229           03  AW-PAYMENT-FREQUENCY    PICTURE X(2).
297800160229           03  AW-ANNUAL-PAYMENT       PICTURE 9(9)V9(2).
297900160229           03  AW-PAYMENT-START-DATE   PICTURE 9(8).
298000160229           03  AW-PREVIOUS-TAXABLE-INCOME
298100160229                                       PICTURE 9(9)V9(2).
298200160229           03  AW-PREVIOUS-TAX-WITHHELD
298300160229                                       PICTURE 9(9)V9(2).
298400160229           03  AW-MAXIMUM-ANNUAL-PAYMENT
298500160229                                       PICTURE 9(9)V9(2).
298600160229           03  AW-SCHEDULED-PAYMENT    PICTURE 9(9)V9(2).
298700160229           03  AW-RIF-LIF-INDICATOR    PICTURE X(1).
298800160229           03  AW-REFERENCE-TEMP-INCOME  PICTURE 9(9)V9(2).
298900160229           03  AW-MAX-PENSIONABLE-EARNINGS  PICTURE 9(9)V9(2).
299000160229           03  AW-PAYMENT-OPTION       PICTURE X(1).
299100160229           03  AW-PAY-FREQ-DESCR       PICTURE X(25).
299200160229           03  AW-PAY-OPTION-DESCR     PICTURE X(25).
299300160229      * RFS132121-START
299400160229           03  AW60-GROSS-YTD-TOTAL-PAID     PICTURE 9(9)V9(2).
299500160229           03  AW60-MINIMUM-LEFT-TO-BE-PAID  PICTURE 9(9)V9(2).
299600160229           03  AW60-GROSS-YTD-MINIMUM-PAID   PICTURE 9(9)V9(2).
299700160229      * RFS138310-START
299800160229      * RFS 144587 STARTS
299900160229      *    03  AW-INCOME-PAYMENT             PICTURE 9(9)V9(2).
300000160229      *    03  AW-WH-TAX                     PICTURE 9(9)V9(2).
300100160229           03  AW-INCOME-PAYMENT-YTD         PICTURE 9(9)V9(2).
300200160229           03  AW-WH-TAX-YTD                 PICTURE 9(9)V9(2).
300300160229      * RFS 144587 ENDS
300400160229           03  FILLER                  PICTURE X(1082).
300500160229      *    03  FILLER                  PICTURE X(1104).
300600160229      * RFS138310-END
300700160229R17165*    03  FILLER                  PICTURE X(1137).
300800160229      * RFS132121-END
300900160229           03  AW-SECTION-SEQUENCE-2   PICTURE 9(6).
301000160229           03  AW-START-DATE           PICTURE 9(08).
301100160229           03  AW-END-DATE             PICTURE 9(08).
301200160229           03  AW-EOR-INDICATOR        PICTURE X(1).
301300160229      /
301400160229       01  RESP-DETAIL-SECTION.
301500160229
301600160229      ******************************************************************
301700160229      *                                                                *
301800160229      *    CPYJSISTMT: Statement File - RRIF Detail Record             *
301900160229      *    CREATED:    17/APR/97 (MFH)                                 *
302000160229      *    CHANGED:    10/DEC/98 (HS)                                  *
302100160229      *    LENGTH:     850                                             *
302200160229      *                                                                *
302300160229      ******************************************************************
302400160229
302500160229      *****
302600160229      *****    - INVESTOR-NO will be formatted.
302700160229      *****    - ACCOUNT will be formatted.
302800160229      *****    - SECTION-NO will always be 62.
302900160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
303000160229      *****    - LANGUAGE will be one of the following.
303100160229      *****        'E' English
303200160229      *****        'F' French
303300160229      *****    - DEALER will be formatted.
303400160229      *****    - SALESREP will be formatted.
303500160229      *****
303600160229
303700160229           03  RE-INVESTOR-NO           PICTURE 9(9).
303800160229           03  RE-ACCOUNT               PICTURE 9(9).
303900160229           03  RE-SECTION-NO            PICTURE 9(2).
304000160229           03  RE-SECTION-SEQUENCE      PICTURE 9(6).
304100160229           03  RE-LANGUAGE              PICTURE X(1).
304200160229           03  RE-DEALER                PICTURE X(4).
304300160229           03  RE-SALESREP              PICTURE X(6).
304400160229           03  RE-PREV-YEARS-CONTRIBUTION
304500160229                                        PICTURE 9(9)V9(2).
304600160229           03  RE-CURR-YEAR-CONTRIBUTION
304700160229                                        PICTURE 9(9)V9(2).
304800160229           03  RE-CES-GRANT-AMOUNT
304900160229                                        PICTURE 9(9)V9(2).
305000160229           03  RE-ACCOUNT-BENEFICIARY1  PICTURE X(45).
305100160229           03  RE-ACCOUNT-BENEFICIARY2  PICTURE X(45).
305200160229           03  RE-ACCOUNT-BENEFICIARY3  PICTURE X(45).
305300160229           03  RE-ACCOUNT-BENEFICIARY4  PICTURE X(45).
305400160229           03  RE-ACCOUNT-BENEFICIARY5  PICTURE X(45).
305500160229           03  RE-ACCOUNT-BENEFICIARY6  PICTURE X(45).
305600160229           03  RE-RESP-TYPE             PICTURE X(1).
305700160229           03  RE-PLAN-SETUP-DATE       PICTURE 9(8).
305800160229      * RFS76998 Begin
305900160229           03  RE-ASSISTED-CAPITAL-IND  PIC X(1).
306000160229           03  RE-ASSISTED-CAPITAL      PIC 9(9)V9(2).
306100160229           03  RE-UNASSISTED-CAPITAL-IND PIC X(1).
306200160229           03  RE-UNASSISTED-CAPITAL    PIC 9(9)V9(2).
306300160229           03  RE-NET-CESG-IND          PIC X(1).
306400160229           03  RE-NET-CESG              PIC 9(11)V9(2).
306500160229           03  RE-NET-CLB-IND           PIC X(1).
306600160229           03  RE-NET-CLB               PIC 9(11)V9(2).
306700160229           03  RE-NET-IQEE-IND          PIC X(1).
306800160229           03  RE-NET-IQEE              PIC 9(11)V9(2).
306900160229R84980     03  RE-NET-PGRANT-IND        PIC X(1).
307000160229R84980     03  RE-NET-PGRANT            PIC 9(11)V9(2).
307100160229      * RFS 148326 STARTS
307200160229R84980*    03  FILLER                   PICTURE X(898).
307300160229           03  RE-MATURITY-DATE         PICTURE 9(8).
307400160229           03  RE-INCOME-IND            PIC X(1).
307500160229           03  RE-INCOME                PIC 9(11)V9(2).
307600160229           03  RE-TOTAL-CONTRIBUTION-IND
307700160229                                        PICTURE X(1).
307800160229           03  RE-TOTAL-CONTRIBUTION    PIC 9(11)V9(2).
307900170508      *RFS163106 Begin
308000170508           03  RE-NET-SAGES-IND         PIC X(1).
308100170508           03  RE-NET-SAGES             PIC 9(11)V9(2).
308200170508           03  RE-NET-BCTESG-IND        PIC X(1).
308300170508           03  RE-NET-BCTESG            PIC 9(11)V9(2).
308400170508           03  FILLER                   PICTURE X(834).
308500170508      *    03  FILLER                   PICTURE X(862).
308600170508      *RFS163106 End
308700160229      * RFS 148326 ENDS
308800160229R84980*    03  FILLER                   PICTURE X(912).
308900160229R17165*    03  FILLER                   PICTURE X(978).
309000160229      * RFS76998 End
309100160229           03  RE-SECTION-SEQUENCE-2    PICTURE 9(6).
309200160229           03  RE-START-DATE            PICTURE 9(08).
309300160229           03  RE-END-DATE              PICTURE 9(08).
309400160229           03  RE-EOR-INDICATOR         PICTURE X(1).
309500160229      /
309600160229       01  GROUP-INFORMATION.
309700160229
309800160229      ******************************************************************
309900160229      *                                                                *
310000160229      *    CPYJSISTMT: Statement File - Group Information Record       *
310100160229      *    CREATED:    08/OCT/97 (HS)                                  *
310200160229      *    CHANGED:    23/MAY/98 (MFH)                                 *
310300160229      *    LENGTH:     750                                             *
310400160229      *                                                                *
310500160229      ******************************************************************
310600160229
310700160229      *****
310800160229      *****    - INVESTOR-NO will be formatted.
310900160229      *****    - ACCOUNT will be formatted.
311000160229      *****    - SECTION-NO will always be 65.
311100160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
311200160229      *****    - LANGUAGE will be one of the following.
311300160229      *****        'E' English
311400160229      *****        'F' French
311500160229      *****    - DEALER will be formatted.
311600160229      *****    - SALESREP will be formatted.
311700160229      *****
311800160229
311900160229           03  GR-INVESTOR-NO          PICTURE 9(9).
312000160229           03  GR-ACCOUNT              PICTURE 9(9).
312100160229           03  GR-SECTION-NO           PICTURE 9(2).
312200160229           03  GR-SECTION-SEQUENCE     PICTURE 9(6).
312300160229           03  GR-LANGUAGE             PICTURE X(1).
312400160229           03  GR-DEALER               PICTURE X(4).
312500160229           03  GR-SALESREP             PICTURE X(6).
312600160229           03  GR-GROUP-ACCOUNT-NO     PICTURE X(6).
312700160229           03  GR-EMPLOYER-NAME        PICTURE X(50).
312800160229           03  GR-TOTAL-EMPLOYEE-CONTRIB-YTD  PIC 9(9)V9(2).
312900160229           03  GR-TOT-EMPLOYEE-CONTRIYTD-SIGN PIC X(1).
313000160229           03  GR-TOTAL-EMPLOYER-CONTRIB-YTD  PIC 9(9)V9(2).
313100160229           03  GR-TOT-EMPLOYER-CONTRIYTD-SIGN PIC X(1).
313200160229           03  GR-TOTAL-CONTRIB-YTD           PIC 9(9)V9(2).
313300160229           03  GR-TOT-CONTRIB-YTD-SIGN        PIC X(1).
313400160229           03  GR-EMPLOYEE-CONTRIB-FIRST60    PIC 9(9)V9(2).
313500160229           03  GR-EMPLOYEE-CONTRIFIRST60-SIGN PIC X(1).
313600160229           03  GR-EMPLOYEE-CONTRIB-REMAIN     PIC 9(9)V9(2).
313700160229           03  GR-EMPLOYEE-CONTRI-REMAIN-SIGN PIC X(1).
313800160229           03  GR-EMPLOYER-CONTRIB-FIRST60    PIC 9(9)V9(2).
313900160229           03  GR-EMPLOYER-CONTRIFIRST60-SIGN PIC X(1).
314000160229           03  GR-EMPLOYER-CONTRIB-REMAIN     PIC 9(9)V9(2).
314100160229           03  GR-EMPLOYER-CONTRIBREMAIN-SIGN PIC X(1).
314200160229R13955     03  GR-GROUP-PLAN-CODE-DESC PICTURE X(25).
314300160229R13955     03  GR-GROUP-EMPLOYEE-NO    PICTURE X(15).
314400160229R17165     03  FILLER                  PICTURE X(1110).
314500160229           03  GR-SECTION-SEQUENCE-2   PICTURE 9(6).
314600160229           03  GR-START-DATE           PICTURE 9(8).
314700160229           03  GR-END-DATE             PICTURE 9(8).
314800160229           03  GR-EOR-INDICATOR        PICTURE X(1).
314900160229
315000160229      /
315100160229       01  BENEFICIARY-INFORMATION.
315200160229
315300160229      ******************************************************************
315400160229      *                                                                *
315500160229      *    CPYJSISTMT: Statement File - Group Information Record       *
315600160229      *    CREATED:    04/APR/2000 (HS)                                *
315700160229      *    CHANGED:      /   /   (MFH)                                 *
315800160229      *    LENGTH:     750                                             *
315900160229      *                                                                *
316000160229      ******************************************************************
316100160229
316200160229      *****
316300160229      *****    - INVESTOR-NO will be formatted.
316400160229      *****    - ACCOUNT will be formatted.
316500160229      *****    - SECTION-NO will always be 67.
316600160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
316700160229      *****    - LANGUAGE will be one of the following.
316800160229      *****        'E' English
316900160229      *****        'F' French
317000160229      *****    - DEALER will be formatted.
317100160229      *****    - SALESREP will be formatted.
317200160229      *****
317300160229
317400160229           03  BE-INVESTOR-NO          PICTURE 9(9).
317500160229           03  BE-ACCOUNT              PICTURE 9(9).
317600160229           03  BE-SECTION-NO           PICTURE 9(2).
317700160229           03  BE-SECTION-SEQUENCE     PICTURE 9(6).
317800160229           03  BE-LANGUAGE             PICTURE X(1).
317900160229           03  BE-DEALER               PICTURE X(4).
318000160229           03  BE-SALESREP             PICTURE X(6).
318100160229           03  BE-CONTRACT-DATE        PICTURE 9(8).
318200160229           03  BE-SALUTATION-CODE      PICTURE X(4).
318300160229           03  BE-LAST-NAME            PICTURE X(20).
318400160229           03  BE-FIRST-NAME           PICTURE X(20).
318500160229           03  BE-REVOCABLE            PICTURE X(1).
318600160229           03  BE-BIRTH-DATE           PICTURE 9(8).
318700160229           03  BE-SOCIAL-INSURANCE-NO  PICTURE 9(9).
318800160229           03  BE-BENE-TYPE-DESC       PICTURE X(25).
318900160229           03  BE-BENE-RELATIONSHIP    PICTURE X(35).
319000160229           03  BE-GENDER               PICTURE X(1).
319100160229           03  BE-CUST-PARENT          PICTURE X(22).
319200160229           03  BE-BENE-LANGUAGE        PICTURE X(1).
319300160229           03  BE-YTD-CONTRIBUTIONS          PICTURE 9(9)V9(2).
319400160229           03  BE-YTD-CONTRIBUTIONS-SIGN     PICTURE X(1).
319500160229           03  BE-LTD-CONTRIBUTIONS          PICTURE 9(9)V9(2).
319600160229           03  BE-LTD-CONTRIBUTIONS-SIGN     PICTURE X(1).
319700160229           03  BE-YTD-WITHDRAWLS             PICTURE 9(9)V9(2).
319800160229           03  BE-YTD-WITHDRAWLS-SIGN        PICTURE X(1).
319900160229           03  BE-LTD-WITHDRAWLS             PICTURE 9(9)V9(2).
320000160229           03  BE-LTD-WITHDRAWLS-SIGN        PICTURE X(1).
320100160229           03  BE-YTD-CESG                   PICTURE 9(9)V9(2).
320200160229           03  BE-YTD-CESG-SIGN              PICTURE X(1).
320300160229           03  BE-LTD-CESG                   PICTURE 9(9)V9(2).
320400160229           03  BE-LTD-CESG-SIGN              PICTURE X(1).
320500160229           03  BE-PRIMARY-CONTINGENT         PICTURE X(1).
320600160229           03  BE-YTD-CAP-TRANSFERS-IN       PICTURE 9(9)V9(2).
320700160229           03  BE-YTD-CAP-TRANSFERS-IN-SIGN  PICTURE X(1).
320800160229           03  BE-LTD-CAP-TRANSFERS-IN       PICTURE 9(9)V9(2).
320900160229           03  BE-LTD-CAP-TRANSFERS-IN-SIGN  PICTURE X(1).
321000160229           03  BE-YTD-INC-TRANSFERS-IN       PICTURE 9(9)V9(2).
321100160229           03  BE-YTD-INC-TRANSFERS-IN-SIGN  PICTURE X(1).
321200160229           03  BE-LTD-INC-TRANSFERS-IN       PICTURE 9(9)V9(2).
321300160229           03  BE-LTD-INC-TRANSFERS-IN-SIGN  PICTURE X(1).
321400160229           03  BE-YTD-CESG-TRANSFERS-IN      PICTURE 9(9)V9(2).
321500160229           03  BE-YTD-CESG-TRANSFERS-IN-SIGN PICTURE X(1).
321600160229           03  BE-LTD-CESG-TRANSFERS-IN      PICTURE 9(9)V9(2).
321700160229           03  BE-LTD-CESG-TRANSFERS-IN-SIGN PICTURE X(1).
321800160229           03  BE-BENE-TYPE            PICTURE X(1).
321900160229      **RFS46797 - BEGIN
322000160229           03  BE-STATUS               PICTURE X(01).
322100160229           03  BE-ACCT-TYPE-RULE       PICTURE X(02).
322200160229      **RFS83788 - BEGIN
322300160229           03  BE-YTD-CLB              PICTURE 9(9)V9(2).
322400160229           03  BE-YTD-CLB-SIGN         PICTURE X(1).
322500160229           03  BE-LTD-CLB              PICTURE 9(9)V9(2).
322600160229           03  BE-LTD-CLB-SIGN         PICTURE X(1).
322700160229           03  BE-YTD-PGRANT           PICTURE 9(9)V9(2).
322800160229           03  BE-YTD-PGRANT-SIGN      PICTURE X(1).
322900160229           03  BE-LTD-PGRANT           PICTURE 9(9)V9(2).
323000160229           03  BE-LTD-PGRANT-SIGN      PICTURE X(1).
323100160229           03  BE-YTD-IQEE             PICTURE 9(9)V9(2).
323200160229           03  BE-YTD-IQEE-SIGN        PICTURE X(1).
323300160229           03  BE-LTD-IQEE             PICTURE 9(9)V9(2).
323400160229           03  BE-LTD-IQEE-SIGN        PICTURE X(1).
323500160229           03  BE-YTD-TOTAL-GRANT-MONEY       PICTURE 9(9)V9(2).
323600160229           03  BE-YTD-TOTAL-GRANT-MONEY-SIGN  PICTURE X(1).
323700160229           03  BE-LTD-TOTAL-GRANT-MONEY       PICTURE 9(9)V9(2).
323800160229           03  BE-LTD-TOTAL-GRANT-MONEY-SIGN  PICTURE X(1).
323900160229      *RFS117038 - Start
324000160229      *    03  FILLER                  PICTURE X(891).
324100160229           03  BE-First-Name-Only      PICTURE X(20).
324200160229           03  BE-Middle-Name-Only     PICTURE X(03).
324300160229           03  BE-Last-Name-Only       PICTURE X(20).
324400160229      * RFS132121-START
324500160229           03  BE67-NON-RESP-ALLOC-PERCENT  PICTURE 9(5)V9(2).
324600160229133489*    03  FILLER                       PICTURE X(841).
324700160229      *    03  FILLER                  PICTURE X(848).
324800160229      *RFS117038 - End
324900160229      * RFS132121-END
325000160229      **RFS83788 - END
325100160229      *RFS133489 - Start
325200160229           03  BE-YTD-EAP              PICTURE 9(9)V9(2).
325300160229           03  BE-YTD-EAP-SIGN         PICTURE X(1).
325400160229           03  BE-LTD-EAP              PICTURE 9(9)V9(2).
325500160229           03  BE-LTD-EAP-SIGN         PICTURE X(1).
325600160229           03  BE-YTD-PSE              PICTURE 9(9)V9(2).
325700160229           03  BE-YTD-PSE-SIGN         PICTURE X(1).
325800160229           03  BE-LTD-PSE              PICTURE 9(9)V9(2).
325900160229           03  BE-LTD-PSE-SIGN         PICTURE X(1).
326000160229      * RFS 144030 STARTS
326100160229           03  BE-SU-FIRST-NAME        PICTURE X(20).
326200160229           03  BE-SU-INITIAL           PICTURE X(03).
326300160229           03  BE-SU-LASTNAME          PICTURE X(20).
326400160229           03  BE-FS-FIRST-NAME        PICTURE X(20).
326500160229           03  BE-FS-INITIAL           PICTURE X(03).
326600160229           03  BE-FS-LASTNAME          PICTURE X(20).
326700160229           03  BE-PO-FIRST-NAME        PICTURE X(20).
326800160229           03  BE-PO-INITIAL           PICTURE X(03).
326900160229           03  BE-PO-LASTNAME          PICTURE X(20).
327000160229      *    03  FILLER                  PICTURE X(793).
327100160229      * RFS 148326 STARTS
327200160229      *    03  FILLER                  PICTURE X(664).
327300160229           03  BE-LTD-CESG-EAP         PICTURE 9(9)V9(2).
327400160229           03  BE-LTD-CESG-EAP-SIGN    PICTURE X(1).
327500180820      * RFS 172713 STARTS
327600180820      *    03  FILLER                  PICTURE X(652).
327700180820           03  BE-PO-DOB               PICTURE 9(8).
327800180827           03  BE-PO-DOD               PICTURE 9(8).
327900180820           03  FILLER                  PICTURE X(636).
328000180820      * RFS 172713 ENDS
328100180820      * RFS 148326 ENDS
328200160229      * RFS 144030 ENDS
328300160229      *RFS133489 - End
328400160229           03  BE-SECTION-SEQUENCE-2   PICTURE 9(6).
328500160229           03  BE-START-DATE           PICTURE 9(8).
328600160229           03  BE-END-DATE             PICTURE 9(8).
328700160229           03  BE-EOR-INDICATOR        PICTURE X(1).
328800160229
328900160229      /
329000160229       01  SYSTEMATIC-PLAN-INFO-SECTION.
329100160229
329200160229      ******************************************************************
329300160229      *                                                                *
329400160229      *    CPYJSISTMT: Statement File - Systematic Plan Info Record    *
329500160229      *    CREATED:    08/OCT/97 (MFH)                                 *
329600160229      *    CHANGED:    DD/MMM/YY (MFH)                                 *
329700160229      *    LENGTH:     750                                             *
329800160229      *                                                                *
329900160229      ******************************************************************
330000160229
330100160229      *****
330200160229      *****    - INVESTOR-NO will be formatted.
330300160229      *****    - ACCOUNT will be formatted.
330400160229      *****    - SECTION-NO will always be 70.
330500160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
330600160229      *****    - LANGUAGE will be one of the following.
330700160229      *****        'E' English
330800160229      *****        'F' French
330900160229      *****    - DEALER will be formatted.
331000160229      *****    - SALESREP will be formatted.
331100160229      *****
331200160229
331300160229           03  SP-INVESTOR-NO          PICTURE 9(9).
331400160229           03  SP-ACCOUNT              PICTURE 9(9).
331500160229           03  SP-SECTION-NO           PICTURE 9(2).
331600160229           03  SP-SECTION-SEQUENCE     PICTURE 9(6).
331700160229           03  SP-LANGUAGE             PICTURE X(1).
331800160229           03  SP-DEALER               PICTURE X(4).
331900160229           03  SP-SALESREP             PICTURE X(6).
332000160229           03  SP-PLAN-TYPE            PICTURE X(3).
332100160229           03  SP-PLAN-FREQUENCY       PICTURE X(2).
332200160229           03  SP-START-DATE-D         PICTURE 9(8).
332300160229           03  SP-STOP-DATE            PICTURE 9(8).
332400160229           03  SP-RESTART-DATE         PICTURE 9(8).
332500160229           03  SP-PLAN-STATUS          PICTURE X(2).
332600160229           03  SP-CURRENCY             PICTURE X(3).
332700160229           03  SP-PAYMENT-TYPE         PICTURE X(1).
332800160229           03  SP-AMOUNT-TYPE          PICTURE X(1).
332900160229           03  SP-GROSS-NET-PROCEEDS   PICTURE X(1).
333000160229           03  SP-PLAN-TOTAL-AMOUNT    PICTURE 9(9)V9(4).
333100160229           03  SP-PLAN-INSTITUTION-NAME
333200160229                                       PICTURE X(35).
333300160229           03  SP-FUNDNO               PICTURE X(8).
333400160229           03  SP-FUND-AMOUNT          PICTURE 9(9)V9(4).
333500160229           03  SP-COMMISSION           PICTURE 9(9)V9(2).
333600160229R24514     03  SP-INVESTMENT-NAME      PICTURE X(35).
333700160229R24514*    03  FILLER  *93662*         PICTURE X(1138).
333800160229      *RFS93662 - start
333900160229           03 SP-OMS-CMPCDE            PICTURE X(3).
334000160229           03 SP-OMS-FUND-NO            PICTURE X(5).
334100160229      *RFS112003 - Starts
334200160229           03  SP-PLAN-TYPE-DESC       PICTURE X(40).
334300160229           03  SP-PLAN-FREQUENCY-DESC  PICTURE X(25).
334400160229           03  SP-PLAN-TYPE-FRENCH     PICTURE X(10).
334500200827      *RFS186053 START
334600200827      *    03  FILLER                  PICTURE X(1055).
334700200827           03  SP-USE-WITHD-HIER       PICTURE X(01).
334800200827           03  FILLER                  PICTURE X(1054).
334900200827      *RFS186053 END
335000160229      *    03  FILLER                  PICTURE X(1130).
335100160229      *RFS112003 - Ends
335200160229      *RFS93662 - End
335300160229           03  SP-SECTION-SEQUENCE-2   PICTURE 9(6).
335400160229           03  SP-START-DATE           PICTURE 9(08).
335500160229           03  SP-END-DATE             PICTURE 9(08).
335600160229           03  SP-EOR-INDICATOR        PICTURE X(1).
335700160229      /
335800160229       01  ACCOUNT-TXN-SECTION.
335900160229
336000160229      ******************************************************************
336100160229      *                                                                *
336200160229      *    CPYJSISTMT: Statement File - Account Transaction Record     *
336300160229      *    CREATED:    DD/MMM/YY (III)                                 *
336400160229      *    CHANGED:    15/DEC/97 (MFH)                                 *
336500160229      *    CHANGED:    20/JUL/98 (HS)                                  *
336600160229      *    LENGTH:     750                                             *
336700160229      *                                                                *
336800160229      ******************************************************************
336900160229
337000160229      *****
337100160229      *****    - INVESTOR-NO will be formatted.
337200160229      *****    - ACCOUNT will be formatted.
337300160229      *****    - SECTION-NO will always be 80.
337400160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
337500160229      *****    - LANGUAGE will be one of the following.
337600160229      *****        'E' English
337700160229      *****        'F' French
337800160229      *****    - DEALER will be formatted.
337900160229      *****    - SALESREP will be formatted.
338000160229      *****
338100160229
338200160229           03  AT-INVESTOR-NO          PICTURE 9(9).
338300160229           03  AT-ACCOUNT              PICTURE 9(9).
338400160229           03  AT-SECTION-NO           PICTURE 9(2).
338500160229           03  AT-SECTION-SEQUENCE     PICTURE 9(6).
338600160229           03  AT-LANGUAGE             PICTURE X(1).
338700160229           03  AT-DEALER               PICTURE X(4).
338800160229           03  AT-SALESREP             PICTURE X(6).
338900160229           03  AT-ACCOUNT-TYPE         PICTURE X(35).
339000160229           03  AT-SPOUSAL              PICTURE X(1).
339100160229           03  AT-LOCKED-IN            PICTURE X(1).
339200160229           03  AT-NOMINEE              PICTURE X(1).
339300160229           03  AT-GROUP                PICTURE X(1).
339400160229           03  AT-SPOUSAL-CONTRIB      PICTURE X(40).
339500160229           03  AT-SPOUSAL-SIN          PICTURE 9(9).
339600160229           03  AT-TRADE-DATE           PICTURE 9(8).
339700160229           03  AT-PROCESS-DATE         PICTURE 9(8).
339800160229           03  AT-FUND-NO              PICTURE X(8).
339900160229           03  AT-INVESTMENT-NAME      PICTURE X(35).
340000160229           03  AT-CURRENCY             PICTURE X(3).
340100160229           03  AT-OPEN-DATE            PICTURE 9(8).
340200160229           03  AT-TRANS-TYPE-CODE      PICTURE X(3).
340300160229           03  AT-TRANS-TYPE           PICTURE X(25).
340400160229           03  AT-GROSS                PICTURE 9(11)V9(2).
340500160229           03  AT-GROSS-SIGN           PICTURE X(1).
340600160229           03  AT-DSC                  PICTURE 9(9)V9(2).
340700160229           03  AT-DSC-SIGN             PICTURE X(1).
340800160229           03  AT-TAX                  PICTURE 9(9)V9(2).
340900160229           03  AT-TAX-SIGN             PICTURE X(1).
341000160229           03  AT-COMMISSION           PICTURE 9(9)V9(2).
341100160229           03  AT-COMMISSION-SIGN      PICTURE X(1).
341200160229           03  AT-NET                  PICTURE 9(11)V9(2).
341300160229           03  AT-NET-SIGN             PICTURE X(1).
341400160229           03  AT-UNIT-PRICE           PICTURE 9(5)V9(4).
341500160229           03  AT-UNIT-PRICE-SIGN      PICTURE X(1).
341600160229           03  AT-NO-OF-UNITS          PICTURE 9(9)V9(3).
341700160229           03  AT-NO-OF-UNITS-SIGN     PICTURE X(1).
341800160229           03  AT-UNIT-BALANCE         PICTURE 9(9)V9(3).
341900160229           03  AT-UNIT-BALANCE-SIGN    PICTURE X(1).
342000160229           03  AT-ACB-PRICE            PICTURE 9(5)V9(4).
342100160229           03  AT-ACB-SIGN             PICTURE X(1).
342200160229           03  AT-CPTL-GAIN            PICTURE 9(11)V9(2).
342300160229           03  AT-CPTL-GAIN-SIGN       PICTURE X(1).
342400160229           03  AT-ACCOUNT-TYPE-SHORT   PICTURE X(5).
342500160229           03  AT-UNIT-PRICE-BASE      PICTURE 9(5)V9(4).
342600160229           03  AT-MKT-VALUE            PICTURE 9(11)V9(2).
342700160229           03  AT-MKT-VALUE-BASE       PICTURE 9(11)V9(2).
342800160229           03  AT-ACB-UNITS-TRANSACTED PICTURE 9(5)V9(4).
342900160229           03  AT-EXCHANGE-RATE        PICTURE 9(2)V9(7).
343000160229           03  AT-ADJUSTMENT-REASON    PICTURE X(40).
343100160229           03  AT-CONTR-REDEMP-DESCRIPTION
343200160229                                       PICTURE X(25).
343300160229           03  AT-PAYMENT-TYPE-CODE    PICTURE X(1).
343400160229           03  AT-OTHER-ACCOUNT        PICTURE 9(9).
343500160229           03  AT-OTHER-FUND-NO        PICTURE X(8).
343600160229           03  AT-OTHER-INVESTMENT-NAME
343700160229                                       PICTURE X(35).
343800160229           03  AT-GROSS-NET-DIFF       PICTURE 9(9)V9(2).
343900160229           03  AT-GROSS-NET-DIFF-SIGN  PICTURE X(1).
344000160229           03  AT-TRANS-STATUS-CODE    PICTURE X(3).
344100160229           03  AT-REVERSAL-INDICATOR   PICTURE X(1).
344200160229           03  AT-TRANS-ORIGIN-CODE    PICTURE X(3).
344300160229           03  AT-CPTL-GAIN-BASE       PICTURE 9(11)V9(2).
344400160229           03  AT-CPTL-GAIN-BASE-SIGN  PICTURE X(1).
344500160229           03  AT-SALES-CHARGE         PICTURE 9(9)V9(2).
344600160229           03  AT-SALES-CHARGE-SIGN    PICTURE X(1).
344700160229           03  AT-CONTR-REDEMP-CODE    PICTURE X(6).
344800160229           03  AT-ASSIGNMENT-INDICATOR PICTURE X.
344900160229           03  AT-ASSIGNED-UNITS       PICTURE 9(9)V9(3).
345000160229           03  AT-INVESTMENT-GRP-CATEGORY PICTURE X(5).
345100160229           03  AT-INVESTMENT-GRP-CAT-DESC PICTURE X(25).
345200160229           03  AT-DISTRIBUTION-ROC     PICTURE 9(9)V9(2).
345300160229           03  AT-DISTRIBUTION-ROC-SIGN  PICTURE X.
345400160229           03  AT-TAXABLE-AMOUNT       PICTURE 9(9)V9(2).
345500160229           03  AT-TAXABLE-AMOUNT-SIGN  PICTURE X.
345600160229           03  AT-GST                  PICTURE 9(9)V9(2).
345700160229           03  AT-PST                  PICTURE 9(9)V9(2).
345800160229           03  AT-DSC-SCHEDULE         PICTURE X(5).
345900160229           03  AT-ADMIN-FEES           PICTURE 9(9)V9(2).
346000160229           03  AT-FED-TAX              PICTURE 9(9)V9(2).
346100160229           03  AT-PROV-TAX             PICTURE 9(9)V9(2).
346200160229           03  AT-TAX-PROVINCE         PICTURE X(3).
346300160229           03  AT-TAX-COUNTRY          PICTURE X(3).
346400160229R10605     03  AT-REP-NAME             PICTURE X(40).
346500160229R11127     03  AT-NON-TAX-DEDUCTIONS   PICTURE 9(11)V9(2).
346600160229R12154     03  AT-NON-TAX-DEDUCT-SIGN  PICTURE X(1).
346700160229R12102     03  AT-PHANTOM-FUND-NUMBER  PICTURE X(5).
346800160229R16990     03  AT-TOTAL-DEDUCTIONS     PICTURE 9(9)V9(2).
346900160229R16990     03  AT-TOTAL-DEDUCTIONS-SIGN PICTURE X(1).
347000160229R18780     03  AT-FEDC-CLAWBACK        PICTURE 9(9)V9(2).
347100160229R18780     03  AT-PRVC-CLAWBACK        PICTURE 9(9)V9(2).
347200160229R26894     03  AT-ERF                  PICTURE 9(9)V9(2).
347300160229R28692     03  AT-ROC-RATE             PICTURE 9(5)V9(4).
347400160229R28692     03  AT-ROC-RATE-SIGN        PICTURE X(1).
347500160229R29550     03  AT-US-BACKUP-WHTX       PICTURE 9(11)V9(2).
347600160229R29550     03  AT-US-BACKUP-WHTX-SIGN  PICTURE X(1).
347700160229R34072     03  AT-REDEM-CHARGE         PICTURE 9(9)V9(2).
347800160229R34072     03  AT-REDEM-CHARGE-SIGN    PICTURE X(1).
347900160229R39466     03  AT-MATURITY-TYPE        PICTURE X(1).
348000160229R39466     03  AT-CONTRACT-XREF        PICTURE X(9).
348100160229R39466     03  AT-CONTRACT-NO          PICTURE X(9).
348200160229      *-- RFS47406 Start
348300160229           03  AT-DEPOSIT-INDICATOR            PICTURE X(10).
348400160229           03  AT-GLWB-DEPOSIT-ALLOCATION      PICTURE 9(03)V99.
348500160229           03  AT-GLWA-DEPOSIT                 PICTURE S9(11)V99.
348600160229           03  AT-FIRST-WTHDRWL-AFT-ELIG-AGE   PICTURE X(01).
348700160229           03  AT-GLWA-WITHDRAWN               PICTURE S9(11)V99.
348800160229           03  AT-EXCESS-GLWA-WITHDRAWN        PICTURE S9(11)V99.
348900160229           03  AT-GW-BASE-BEFORE-TRADE         PICTURE S9(11)V99.
349000160229           03  AT-GW-BASE-AFTER-TRADE          PICTURE S9(11)V99.
349100160229           03  AT-GW-BASE-CAL-METHOD           PICTURE X(15).
349200160229           03  AT-FIE-BASE-BEFORE-TRADE        PICTURE S9(11)V99.
349300160229           03  AT-FIE-BASE-AFTER-TRADE         PICTURE S9(11)V99.
349400160229           03  AT-FIE-BASE-CAL-METHOD          PICTURE X(15).
349500160229R86495     03  AT-PLACEMENT-DATE               PICTURE 9(08).
349600160229  |        03  AT-TRANS-NO                     PICTURE 9(09).
349700160229  V   *    03  FILLER                          PICTURE X(307). 92477
349800160229R86495*    03  FILLER                          PICTURE X(324).
349900160229      *-- RFS92477 Start
350000160229           03  AT-TRANS-SUB-TYPE               PICTURE X(05).
350100160229           03  AT-TRANS-SUB-TYPE-DESC          PICTURE X(25).
350200160229      *RFS93662 - Start
350300160229           03 AT-OMS-CMPCDE            PICTURE X(3).
350400160229           03 AT-OMS-FUND-NO           PICTURE X(5).
350500160229           03 AT-OMS-TRANS-TYPE-CODE   PICTURE X(3).
350600160229           03 AT-OMS-TRANS-ORIGIN-CODE PICTURE X(3).
350700160229           03 AT-OMS-TRANS-SUB-TYPE    PICTURE X(5).
350800160229           03 AT-OMS-REVERSE           PICTURE X(1).
350900160229           03 AT-OMS-CONTR-REDEEM      PICTURE X(6).
351000160229      *RFS117038 - Start
351100160229           03 AT-OMS-INCLUDE           PICTURE X(1).
351200160229      *    03 FILLER                   PICTURE X(251).
351300160229      *RFS129378 STARTS
351400160229      *    03 FILLER                   PICTURE X(250).
351500160229           03 AT-RATE                  PICTURE 9(3)V9(3).
351600160229           03 AT-MATURITY-DATE         PICTURE 9(8).
351700160229           03 AT-MATURITY-VALUE        PICTURE 9(11)V9(2).
351800160229130038     03 AT-OMS-SUPPRESS          PICTURE X(1).
351900160229      * RFS 142884 STARTS
352000160229           03 AT-CERTIFICATE-NO        PICTURE X(12).
352100160229           03 AT-TERM                  PICTURE S9(5).
352200160229           03 AT-INTEREST-RATE         PICTURE S9(4)V9(3).
352300160229           03 AT-TERM-GROUP            PICTURE X(10).
352400160229130038*    03 FILLER                   PICTURE X(223).
352500160229130038*    03 FILLER                   PICTURE X(222).
352600160229      * RFS148326 STARTS
352700160229      *    03 FILLER                   PICTURE X(188).
352800160229           03 AT-TRANSACTED-BOOK-VALUE PICTURE 9(11)V9(2).
352900160229           03 AT-TRANSACTED-BOOK-VALUE-SIGN
353000160229                                       PICTURE X(1).
353100180926      * RFS176735 - Starts
353200180926      *    03 FILLER                   PICTURE X(174).
353300180926           03 AT-INTEREST-AMT          PICTURE S9(11)V9(2).
353400180926           03 AT-NET-PROCEEDS          PICTURE S9(11)V9(2).
353500200515      * RFS 185916 - Begins
353600200515      *    03 FILLER                   PICTURE X(148).
353700200515           03 AT-PROD-TYPE-CODE        PICTURE X(03).
353800210105      * RFS186446 Starts *
353900210105           03 AT-FIRST-PURCHASE        PICTURE X(01).
354000210105      *    03 FILLER                   PICTURE X(145).
354100210105           03 FILLER                   PICTURE X(144).
354200210105      * RFS186446 Ends*
354300200515      * RFS 185916 - Begins
354400180926      * RFS176735 - Ends
354500160229      * RFS148326 ENDS
354600160229      * RFS 142884 ENDS
354700160229      *RFS129378 ENDS
354800160229      *RFS117038 - End
354900160229      *RFS93662 - End
35500016022993662 *    03  FILLER                          PICTURE X(277).
355100160229      *-- RFS92477 End
355200160229      *-- RFS47406 End
355300160229           03  AT-SECTION-SEQUENCE-2   PICTURE 9(6).
355400160229           03  AT-START-DATE           PICTURE 9(08).
355500160229           03  AT-END-DATE             PICTURE 9(08).
355600160229           03  AT-EOR-INDICATOR        PICTURE X(1).
355700160229
355800160229
355900160229R34848/*Starts
356000160229R34848 01  TRANS-DED-SECTION.
356100160229
356200160229      ******************************************************************
356300160229      *                                                                *
356400160229      *    CPYJSISTMT: Deduction Transaction Record                    *
356500160229      *                                                                *
356600160229      ******************************************************************
356700160229
356800160229      *****
356900160229      *****    - INVESTOR-NO will be formatted.
357000160229      *****    - ACCOUNT will be formatted.
357100160229      *****    - SECTION-NO will always be 81.
357200160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
357300160229      *****    - LANGUAGE will be one of the following.
357400160229      *****        'E' English
357500160229      *****        'F' French
357600160229      *****    - DEALER will be formatted.
357700160229      *****    - SALESREP will be formatted.
357800160229      *****
357900160229           03 DT-INVESTOR-NO                 PICTURE 9(9).
358000160229           03 DT-ACCOUNT                     PICTURE 9(9).
358100160229           03 DT-SECTION-NO                  PICTURE 9(2).
358200160229           03 DT-SECTION-SEQUENCE            PICTURE 9(6).
358300160229           03 DT-LANGUAGE                    PICTURE X(1).
358400160229           03 DT-DEALER                      PICTURE X(4).
358500160229           03 DT-SALESREP                    PICTURE X(6).
358600160229           03 DT-DEDUCTION-AMOUNT            PICTURE 9(11)V9(2).
358700160229           03 DT-DEDUCTION-CODE              PICTURE X(4).
358800160229           03 DT-DEDUCTION-DESC-SHORT        PICTURE X(10).
358900160229           03 DT-DEDUCTION-DESC-LONG         PICTURE X(25).
359000160229           03 FILLER                         PICTURE X(1238).
359100160229           03 DT-SECTION-SEQUENCE-2          PICTURE 9(6).
359200160229           03 DT-START-DATE                  PICTURE 9(8).
359300160229           03 DT-END-DATE                    PICTURE 9(8).
359400160229           03 DT-EOR-INDICATOR               PICTURE X(1).
359500160229R34848* Ends
359600160229      /
359700160229       01  AT-INSTITUTION-DETAIL-SECTION.
359800160229
359900160229      ******************************************************************
360000160229      *                                                                *
360100160229      *    CPYJSISTMT: Statement File - Account Transaction            *
360200160229      *                                 Institution Detail Record      *
360300160229      *    CREATED:    25/APR/97 (MFH)                                 *
360400160229      *    CHANGED:    DD/MMM/YY (III)                                 *
360500160229      *    LENGTH:     750                                             *
360600160229      *                                                                *
360700160229      ******************************************************************
360800160229
360900160229      *****
361000160229      *****    - INVESTOR-NO will be formatted.
361100160229      *****    - ACCOUNT will be formatted.
361200160229      *****    - SECTION-NO will always be 82.
361300160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
361400160229      *****    - LANGUAGE will be one of the following.
361500160229      *****        'E' English
361600160229      *****        'F' French
361700160229      *****    - DEALER will be formatted.
361800160229      *****    - SALESREP will be formatted.
361900160229      *****
362000160229
362100160229           03  TI-INVESTOR-NO          PICTURE 9(9).
362200160229           03  TI-ACCOUNT              PICTURE 9(9).
362300160229           03  TI-SECTION-NO           PICTURE 9(2).
362400160229           03  TI-SECTION-SEQUENCE     PICTURE 9(6).
362500160229           03  TI-LANGUAGE             PICTURE X(1).
362600160229           03  TI-DEALER               PICTURE X(4).
362700160229           03  TI-SALESREP             PICTURE X(6).
362800160229           03  TI-INSTITUTION-CODE     PICTURE X(5).
362900160229           03  TI-BRANCH-CODE          PICTURE X(5).
363000160229           03  TI-INSTITUTION-NAME     PICTURE X(35).
363100160229           03  TI-PLAN-NUMBER          PICTURE X(15).
363200160229R17165     03  FILLER                  PICTURE X(1230).
363300160229           03  TI-SECTION-SEQUENCE-2   PICTURE 9(6).
363400160229           03  TI-START-DATE           PICTURE 9(08).
363500160229           03  TI-END-DATE             PICTURE 9(08).
363600160229           03  TI-EOR-INDICATOR        PICTURE X(1).
363700160229
363800160229      /
363900160229       01  AT-RESP-DETAIL-SECTION.
364000160229
364100160229      ******************************************************************
364200160229      *                                                                *
364300160229      *    CPYJSISTMT: Statement File - Account Transaction            *
364400160229      *                                 RESP Detail Record             *
364500160229      *    CREATED:    07/OCT/99 (MFH)                                 *
364600160229      *    CHANGED:    DD/MMM/YY (III)                                 *
364700160229      *    LENGTH:     750                                             *
364800160229      *                                                                *
364900160229      ******************************************************************
365000160229
365100160229      *****
365200160229      *****    - INVESTOR-NO will be formatted.
365300160229      *****    - ACCOUNT will be formatted.
365400160229      *****    - SECTION-NO will always be 84.
365500160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
365600160229      *****    - LANGUAGE will be one of the following.
365700160229      *****        'E' English
365800160229      *****        'F' French
365900160229      *****    - DEALER will be formatted.
366000160229      *****    - SALESREP will be formatted.
366100160229      *****
366200160229
366300160229           03  TE-INVESTOR-NO          PICTURE 9(9).
366400160229           03  TE-ACCOUNT              PICTURE 9(9).
366500160229           03  TE-SECTION-NO           PICTURE 9(2).
366600160229           03  TE-SECTION-SEQUENCE     PICTURE 9(6).
366700160229           03  TE-LANGUAGE             PICTURE X(1).
366800160229           03  TE-DEALER               PICTURE X(4).
366900160229           03  TE-SALESREP             PICTURE X(6).
367000160229           03  TE-ACCOUNT-BENEFICIARY1 PICTURE X(45).
367100160229           03  TE-AMOUNT1              PICTURE 9(9)V9(2).
367200160229           03  TE-AMOUNT-SIGN1         PICTURE X(1).
367300160229           03  TE-PERCENT1             PICTURE 9(3)V9(2).
367400160229           03  TE-ACCOUNT-BENEFICIARY2 PICTURE X(45).
367500160229           03  TE-AMOUNT2              PICTURE 9(9)V9(2).
367600160229           03  TE-AMOUNT-SIGN2         PICTURE X(1).
367700160229           03  TE-PERCENT2             PICTURE 9(3)V9(2).
367800160229           03  TE-ACCOUNT-BENEFICIARY3 PICTURE X(45).
367900160229           03  TE-AMOUNT3              PICTURE 9(9)V9(2).
368000160229           03  TE-AMOUNT-SIGN3         PICTURE X(1).
368100160229           03  TE-PERCENT3             PICTURE 9(3)V9(2).
368200160229           03  TE-ACCOUNT-BENEFICIARY4 PICTURE X(45).
368300160229           03  TE-AMOUNT4              PICTURE 9(9)V9(2).
368400160229           03  TE-AMOUNT-SIGN4         PICTURE X(1).
368500160229           03  TE-PERCENT4             PICTURE 9(3)V9(2).
368600160229           03  TE-ACCOUNT-BENEFICIARY5 PICTURE X(45).
368700160229           03  TE-AMOUNT5              PICTURE 9(9)V9(2).
368800160229           03  TE-AMOUNT-SIGN5         PICTURE X(1).
368900160229           03  TE-PERCENT5             PICTURE 9(3)V9(2).
369000160229           03  TE-ACCOUNT-BENEFICIARY6 PICTURE X(45).
369100160229           03  TE-AMOUNT6              PICTURE 9(9)V9(2).
369200160229           03  TE-AMOUNT-SIGN6         PICTURE X(1).
369300160229           03  TE-PERCENT6             PICTURE 9(3)V9(2).
369400160229R17165     03  FILLER                  PICTURE X(918).
369500160229           03  TE-SECTION-SEQUENCE-2   PICTURE 9(6).
369600160229           03  TE-START-DATE           PICTURE 9(08).
369700160229           03  TE-END-DATE             PICTURE 9(08).
369800160229           03  TE-EOR-INDICATOR        PICTURE X(1).
369900160229
370000160229      /
370100160229       01  RESET-CONTRACT-DETAILS.
370200160229
370300160229      ******************************************************************
370400160229      *                                                                *
370500160229      *    CPYJSISTMT: Statement File - Seg Fund Contract Investment   *
370600160229      *    CREATED:    12/10/2000                                      *
370700160229      *    CHANGED:    dd/mmm/yy (MFH)                                 *
370800160229      *    LENGTH:     850                                             *
370900160229      *                                                                *
371000160229      ******************************************************************
371100160229
371200160229      *****
371300160229      *****    - INVESTOR-NO will be formatted.
371400160229      *****    - ACCOUNT will be formatted.
371500160229      *****    - SECTION-NO will always be 90.
371600160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999999.
371700160229      *****    - LANGUAGE will always be formatted.
371800160229      *****    - DEALER will always be formatted.
371900160229      *****    - SALESREP will always be formatted.
372000160229      *****
372100160229
372200160229           03  RC-INVESTOR-NO          PICTURE 9(9).
372300160229           03  RC-ACCOUNT              PICTURE 9(9).
372400160229           03  RC-SECTION-NO           PICTURE 9(2).
372500160229           03  RC-SECTION-SEQUENCE     PICTURE 9(6).
372600160229           03  RC-LANGUAGE             PICTURE X(1).
372700160229           03  RC-DEALER               PICTURE X(4).
372800160229           03  RC-SALESREP             PICTURE X(6).
372900160229           03  RC-CONTRACT-NO          PICTURE 9(9).
373000160229           03  RC-CONTRACT-XREF        PICTURE X(9).
373100160229           03  RC-INVESTMENT-CODE      PICTURE X(5).
373200160229           03  RC-RESET-NUMBER         PICTURE 9(3).
373300160229           03  RC-RESET-REQUEST-DATE   PICTURE 9(8).
373400160229           03  RC-EFFECTIVE-DATE       PICTURE 9(8).
373500160229           03  RC-TRADE-DATE           PICTURE 9(8).
373600160229           03  RC-UNIT-PRICE           PICTURE 9(5)V9(4).
373700160229           03  RC-UNIT-AMT             PICTURE 9(11)V9(3).
373800160229           03  RC-EXCHANGE-RATE        PICTURE 9(2)V9(7).
373900160229           03  RC-OLD-NET-DEPOSIT-VALUE-BASE  PIC 9(11)V9(2).
374000160229           03  RC-NEW-NET-DEPOSIT-VALUE-BASE  PIC 9(11)V9(2).
374100160229           03  RC-OLD-NET-DEPOSIT-VALUE       PIC 9(11)V9(2).
374200160229           03  RC-NEW-NET-DEPOSIT-VALUE       PIC 9(11)V9(2).
374300160229           03  RC-OLD-GUARANTEE-BASE-BASE     PIC 9(11)V9(2).
374400160229           03  RC-NEW-GUARANTEE-BASE-BASE     PIC 9(11)V9(2).
374500160229           03  RC-OLD-GUARANTEE-BASE          PIC 9(11)V9(2).
374600160229           03  RC-NEW-GUARANTEE-BASE          PIC 9(11)V9(2).
374700160229           03  RC-LAST-RESET-DATE      PICTURE 9(8).
374800160229           03  RC-LAST-RESET-NUMBER    PICTURE 9(3).
374900160229R18924     03  RC-OLD-GUARANTEE-DEATH         PIC 9(11)V9(2).
375000160229   ¦       03  RC-NEW-GUARANTEE-DEATH         PIC 9(11)V9(2).
375100160229R64262*    03  RC-GDV-RESET-IND               PIC X(1).
375200160229R64262     03  RC-RESET-CATEGORY              PIC X(1).
375300160229R38639     03  RC-RESET-PROCESS-DATE          PIC 9(8).
375400160229R64262     03  RC-RESET-TYPE                  PIC X(3).
375500160229  ]        03  RC-NEW-GUARANTEE-MAT-VALUE     PIC 9(11)V9(2).
375600160229  ]        03  RC-OLD-GUARANTEE-MAT-VALUE     PIC 9(11)V9(2).
375700160229R38639*    03  FILLER                  PICTURE X(1058).
375800160229R63488     03  RC-OLD-GUAR-BASE-BASE-2        PIC 9(11)V9(2).
375900160229R63488     03  RC-OLD-GUAR-BASE-BASE-2-SIGN   PIC X.
376000160229R63488     03  RC-OLD-GUAR-BASE-2             PIC 9(11)V9(2).
376100160229R63488     03  RC-OLD-GUAR-BASE-2-SIGN        PIC X.
376200160229R63488     03  RC-SERIES-TYPE                 PIC X(5).
376300160229R63488     03  FILLER                  PICTURE X(996).
376400160229R64262*R63488    03  FILLER                  PICTURE X(1029).
376500160229           03  RC-SECTION-SEQUENCE-2   PICTURE 9(6).
376600160229           03  RC-START-DATE           PICTURE 9(8).
376700160229           03  RC-END-DATE             PICTURE 9(8).
376800160229           03  RC-EOR-INDICATOR        PICTURE X(1).
376900160229
377000160229      /
377100160229       01  TRAILER-INFO-SECTION.
377200160229
377300160229      ******************************************************************
377400160229      *                                                                *
377500160229      *    CPYJSISTMT: Statement File - Trailer Record                 *
377600160229      *    CREATED:    28/03/2001                                      *
377700160229      *    CHANGED:    dd/mmm/yy (MFH)                                 *
377800160229      *    LENGTH:     1100                                            *
377900160229      *                                                                *
378000160229      ******************************************************************
378100160229
378200160229      *****
378300160229      *****    - INVESTOR-NO will be "999999999".
378400160229      *****    - ACCOUNT will be "999999999".
378500160229      *****    - SECTION-NO will always be 99.
378600160229      *****    - SECTION-SEQUENCE will formatted 999999.
378700160229      *****    - LANGUAGE will always be "E".
378800160229      *****    - DEALER will always be "ZZZZ".
378900160229      *****    - SALESREP will always be "ZZZZZZ".
379000160229      *****    - SECTION-SEQUENCE-2 will always be "999999'.
379100160229      *****
379200160229
379300160229           03  TR-INVESTOR-NO         PICTURE 9(9).
379400160229           03  TR-ACCOUNT             PICTURE 9(9).
379500160229           03  TR-SECTION-NO          PICTURE 9(2).
379600160229           03  TR-SECTION-SEQUENCE    PICTURE 9(6).
379700160229           03  TR-LANGUAGE-CODE       PICTURE X(1).
379800160229           03  TR-DEALER              PICTURE X(4).
379900160229           03  TR-DEALER-REP-CODE     PICTURE X(6).
380000160229           03  TR-STMT-DTA-RCNT-TOT   PICTURE 9(9).
380100160229           03  TR-STMT-DTA-RCNT-00    PICTURE 9(9).
380200160229           03  TR-STMT-DTA-RCNT-01    PICTURE 9(9).
380300160229           03  TR-STMT-DTA-RCNT-05    PICTURE 9(9).
380400160229           03  TR-STMT-DTA-RCNT-10    PICTURE 9(9).
380500160229           03  TR-STMT-DTA-RCNT-15    PICTURE 9(9).
380600160229           03  TR-STMT-DTA-RCNT-20    PICTURE 9(9).
380700160229           03  TR-STMT-DTA-RCNT-30    PICTURE 9(9).
380800160229           03  TR-STMT-DTA-RCNT-35    PICTURE 9(9).
380900160229           03  TR-STMT-DTA-RCNT-50    PICTURE 9(9).
381000160229           03  TR-STMT-DTA-RCNT-60    PICTURE 9(9).
381100160229           03  TR-STMT-DTA-RCNT-67    PICTURE 9(9).
381200160229           03  TR-STMT-DTA-RCNT-80    PICTURE 9(9).
381300160229           03  TR-STMT-DTA-RCNT-82    PICTURE 9(9).
381400160229           03  TR-STMT-DTA-RCNT-84    PICTURE 9(9).
381500160229           03  TR-STMT-DTA-RCNT-70    PICTURE 9(9).
381600160229           03  TR-STMT-DTA-RCNT-65    PICTURE 9(9).
381700160229           03  TR-STMT-DTA-RCNT-62    PICTURE 9(9).
381800160229           03  TR-STMT-DTA-RCNT-40    PICTURE 9(9).
381900160229           03  TR-STMT-DTA-RCNT-43    PICTURE 9(9).
382000160229           03  TR-STMT-DTA-RCNT-45    PICTURE 9(9).
382100160229           03  TR-STMT-DTA-RCNT-90    PICTURE 9(9).
382200160229RF8633     03  TR-STMT-DTA-RCNT-12    PICTURE 9(9).
382300160229RF8633     03  TR-STMT-DTA-RCNT-25    PICTURE 9(9).
382400160229R18653     03  TR-STMT-DTA-RCNT-17    PICTURE 9(9).
382500160229R20870     03  TR-STMT-DTA-RCNT-33    PICTURE 9(9).
382600160229R20870     03  TR-STMT-DTA-RCNT-38    PICTURE 9(9).
382700160229R35042     03  TR-STMT-DTA-RCNT-21    PICTURE 9(9).
382800160229R34848     03  TR-STMT-DTA-RCNT-81    PICTURE 9(9).
382900160229R69515     03  TR-STMT-DTA-RCNT-63    PICTURE 9(9).
383000160229117038     03  TR-STMT-DTA-RCNT-22    PICTURE 9(9).
383100160229      * RFS132121-START
383200160229           03  TR-STMT-DTA-RCNT-16    PICTURE 9(9).
383300210823      * RFS186199 - Begin
383400210823      *    03  FILLER                 PICTURE X(1002).
383500210823           03  TR-STMT-DTA-RCNT-14    PICTURE 9(9).
383600210823           03  TR-STMT-DTA-RCNT-83    PICTURE 9(9).
383700210823           03  FILLER                 PICTURE X(984).
383800210823      * RFS186199 - End
383900160229117038*    03  FILLER                 PICTURE X(1011).
384000160229      * RFS132121-END
384100160229117038***  03  FILLER                 PICTURE X(1020).
384200160229R34848***  03  FILLER                 PICTURE X(1029).
384300160229R35042***  03  FILLER                 PICTURE X(1038).
384400160229           03  TR-SECTION-SEQUENCE-2   PICTURE 9(6).
384500160229           03  TR-START-DATE           PICTURE 9(8).
384600160229           03  TR-END-DATE             PICTURE 9(8).
384700160229           03  TR-EOR-INDICATOR        PICTURE X(1).
384800160229      /
384900160229      *RFS69515 Begin RDSP
385000160229
385100160229       01  RDSP-DETAIL-SECTION.
385200160229
385300160229           03  RD-INVESTOR-NO                      PICTURE 9(9).
385400160229           03  RD-ACCOUNT                          PICTURE 9(9).
385500160229           03  RD-SECTION-NO                       PICTURE 9(2).
385600160229           03  RD-SECTION-SEQUENCE                 PICTURE 9(6).
385700160229           03  RD-LANGUAGE                         PICTURE X(1).
385800160229           03  RD-DEALER                           PICTURE X(4).
385900160229           03  RD-SALESREP                         PICTURE X(6).
386000160229           03  RD-YTD-CONTRIBUTIONS                PICTURE 9(9)V9(2).
386100160229           03  RD-YTD-CONTRIBUTIONS-SIGN           PICTURE X(1).
386200160229           03  RD-LTD-CONTRIBUTIONS                PICTURE 9(9)V9(2).
386300160229           03  RD-LTD-CONTRIBUTIONS-SIGN           PICTURE X(1).
386400160229           03  RD-YTD-CDS-GRANT                    PICTURE 9(9)V9(2).
386500160229           03  RD-YTD-CDS-GRANT-SIGN               PICTURE X(1).
386600160229           03  RD-LTD-CDS-GRANT                    PICTURE 9(9)V9(2).
386700160229           03  RD-LTD-CDS-GRANT-SIGN               PICTURE X(1).
386800160229           03  RD-YTD-CDS-BOND                     PICTURE 9(9)V9(2).
386900160229           03  RD-YTD-CDS-BOND-SIGN                PICTURE X(1).
387000160229           03  RD-LTD-CDS-BOND                     PICTURE 9(9)V9(2).
387100160229           03  RD-LTD-CDS-BOND-SIGN                PICTURE X(1).
387200160229           03  RD-YTD-TOTAL-CONTRIBUTION           PICTURE 9(9)V9(2).
387300160229           03  RD-YTD-TOTAL-CONTRIBUTION-SIGN      PICTURE X(1).
387400160229           03  RD-LTD-TOTAL-CONTRIBUTION           PICTURE 9(9)V9(2).
387500160229           03  RD-LTD-TOTAL-CONTRIBUTION-SIGN      PICTURE X(1).
387600160229      * RFS 79298 - begin
387700160229           03  RD-YTD-DAP-PAYMENT                  PICTURE 9(9)V9(2).
387800160229           03  RD-YTD-DAP-PAYMENT-SIGN             PIC X(1).
387900160229           03  RD-LTD-DAP-PAYMENT                  PICTURE 9(9)V9(2).
388000160229           03  RD-LTD-DAP-PAYMENT-SIGN             PIC X(1).
388100160229           03  RD-YTD-LDAP-PAYMENT                 PICTURE 9(9)V9(2).
388200160229           03  RD-YTD-LDAP-PAYMENT-SIGN            PIC X(1).
388300160229           03  RD-LTD-LDAP-PAYMENT                 PICTURE 9(9)V9(2).
388400160229           03  RD-LTD-LDAP-PAYMENT-SIGN            PIC X(1).
388500160229           03  RD-YTD-TOTAL-DAP-LDAP               PICTURE 9(9)V9(2).
388600160229           03  RD-YTD-DAP-LDAP-SIGN                PIC X(1).
388700160229           03  RD-LTD-TOTAL-DAP-LDAP               PICTURE 9(9)V9(2).
388800160229           03  RD-LTD-DAP-LDAP-SIGN                PIC X(1).
388900160229           03  RD-YTD-REPAYMENT-BOND               PICTURE 9(9)V9(2).
389000160229           03  RD-YTD-REPAYMENT-BOND-SIGN          PIC X(1).
389100160229           03  RD-LTD-REPAYMENT-BOND               PICTURE 9(9)V9(2).
389200160229           03  RD-LTD-REPAYMENT-BOND-SIGN          PIC X(1).
389300160229           03  RD-YTD-REPAYMENT-GRANT              PICTURE 9(9)V9(2).
389400160229           03  RD-YTD-REPAYMENT-GRANT-SIGN         PIC X(1).
389500160229           03  RD-LTD-REPAYMENT-GRANT              PICTURE 9(9)V9(2).
389600160229           03  RD-LTD-REPAYMENT-GRANT-SIGN         PIC X(1).
389700160229      *    03  FILLER                              PICTURE X(1194).
389800160229R91272     03  RD-YTD-ROLLOVER                     PICTURE 9(9)V9(2).
389900160229  |        03  RD-YTD-ROLLOVER-SIGN                PIC X(1).
390000160229  |        03  RD-LTD-ROLLOVER                     PICTURE 9(9)V9(2).
390100160229  |        03  RD-LTD-ROLLOVER-SIGN                PIC X(1).
390200160229      * RFS123925 - Begin
390300160229  V   *    03  FILLER                              PICTURE X(1050).
390400160229           03  RD-LDAP-Maximum-Amount              PICTURE 9(9)V9(2).
390500160229           03  RD-LDAP-Maximum-SIGN                PIC X(1).
390600160229           03  RD-PGAP-Maximum-Amount              PICTURE 9(9)V9(2).
390700160229           03  RD-PGAP-Maximum-SIGN                PIC X(1).
390800160229      * RFS124157 - Start
390900160229      *    03  FILLER                              PICTURE X(1026).
391000160229      * RFS123925 - End
391100160229           03  RD-YTD-RESP-ROLLOVER                PICTURE 9(9)V9(2).
391200160229           03  RD-YTD-RESP-ROLLOVER-SIGN           PICTURE X(1).
391300160229           03  RD-LTD-RESP-ROLLOVER                PICTURE 9(9)V9(2).
391400160229           03  RD-LTD-RESP-ROLLOVER-SIGN           PICTURE X(1).
391500160229           03  FILLER                              PICTURE X(1002).
391600160229      * RFS124157 - End
391700160229R91272*    03  FILLER                              PICTURE X(1074).
391800160229      * RFS 79298 - end
391900160229
392000160229           03  RD-SECTION-SEQUENCE-2               PICTURE 9(6).
392100160229           03  RD-START-DATE                       PICTURE 9(08).
392200160229           03  RD-END-DATE                         PICTURE 9(08).
392300160229           03  RD-EOR-INDICATOR                    PICTURE X(1).
392400160229      *RFS69515 End - RDSP
392500160229
392600160229      *RFS117038 begin
392700160229
392800160229       01  ACCOUNT-DETAILS-SECTION-22.
392900160229
393000160229      ******************************************************************
393100160229      *                                                                *
393200160229      *    CPYJSISTMT: Statement File - Account Addition Detail Record *
393300160229      *                                                                *
393400160229      ******************************************************************
393500160229
393600160229      *****
393700160229      *****    - INVESTOR-NO will be formatted.
393800160229      *****    - ACCOUNT will be formatted.
393900160229      *****    - SECTION-NO will always be 22.
394000160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
394100160229      *****                       999999 will be reserved for the total.
394200160229      *****    - LANGUAGE will be one of the following.
394300160229      *****        'E' English
394400160229      *****        'F' French
394500160229      *****    - DEALER will be formatted.
394600160229      *****    - SALESREP will be formatted.
394700160229      *****
394800160229
394900160229           03  AD22-INVESTOR-NO                    PICTURE 9(9).
395000160229           03  AD22-ACCOUNT                        PICTURE 9(9).
395100160229           03  AD22-SECTION-NO                     PICTURE 9(2).
395200160229           03  AD22-SECTION-SEQUENCE               PICTURE 9(6).
395300160229           03  AD22-LANGUAGE                       PICTURE X(1).
395400160229           03  AD22-DEALER                         PICTURE X(4).
395500160229           03  AD22-SALESREP                       PICTURE X(6).
395600160229           03  AD22-DATA                           PICTURE X(1290).
395700160229           03  AD22-SECTION-SEQUENCE-2             PICTURE 9(6).
395800160229           03  AD22-START-DATE                     PICTURE 9(08).
395900160229           03  AD22-END-DATE                       PICTURE 9(08).
396000160229           03  AD22-EOR-INDICATOR                  PICTURE X(1).
396100160229
396200160229      ** ACCOUNT FUND LEVEL, SECTION SEQUENCE FROM 1 TO 999998
396300160229           03  AD22-FUNDLEVEL REDEFINES AD22-DATA.
396400160229             05  AD22-FUNDNUMBER                   PICTURE X(8).
396500160229             05  AD22-FUNDNETDEPOSIT-SIGN          PICTURE X(1).
396600160229             05  AD22-FUNDNETDEPOSIT               PICTURE 9(11)V9(2).
396700160229             05  AD22-FUNDGROSSWITHDRAWAL-SIGN     PICTURE X(1).
396800160229             05  AD22-FUNDGROSSWITHDRAWAL          PICTURE 9(11)V9(2).
396900160229      *RFS152451-STARTS *
397000160229             05  AD22-YTDAMSFEESIGN                PICTURE X(1).
397100160229             05  AD22-YTDAMSFEE                    PICTURE 9(11)V9(2).
397200160229             05  AD22-YTDAMSFEDTAXSIGN             PICTURE X(1).
397300160229             05  AD22-YTDAMSFEDTAX                 PICTURE 9(11)V9(2).
397400160229             05  AD22-YTDAMSPROVINCIALTAXSIGN      PICTURE X(1).
397500160229             05  AD22-YTDAMSPROVINCIALTAX          PICTURE 9(11)V9(2).
397600160229             05  AD22-YTDMANAGFEESIGN              PICTURE X(1).
397700160229             05  AD22-YTDMANAGFEE                  PICTURE 9(11)V9(2).
397800160229             05  AD22-YTDMANAGFEEFEDTAXSIGN        PICTURE X(1).
397900160229             05  AD22-YTDMANAGFEEFEDTAX            PICTURE 9(11)V9(2).
398000160229             05  AD22-YTDMANAGFEEPROVTAXSIGN       PICTURE X(1).
398100160229             05  AD22-YTDMANAGFEEPROVTAX           PICTURE 9(11)V9(2).
398200160229             05  AD22-FUNDFILLER                   PICTURE X(1170).
398300160229152451*      05  AD22-FUNDFILLER                   PICTURE X(1254).
398400160229      *RFS152451-ENDS *
398500160229
398600160229      ** ACCOUNT LEVEL, SECTION SEQUENCE = 999999
398700160229            03  AD22-ACTLEVEL REDEFINES AD22-DATA.
398800160229              05  AD22-FUNDFILLER                  PICTURE X(36).
398900160229              05  AD22-PRODUCTTYPENAME             PICTURE X(25).
399000160229              05  AD22-ACCOUNTMATURITYDATE         PICTURE 9(8).
399100160229              05  AD22-ACCOUNTEFFECTIVEDATE        PICTURE 9(8).
399200160229              05  AD22-TOTALOPENINGMV              PICTURE 9(11)V9(2).
399300160229              05  AD22-TOTALNETDEPOSIT-SIGN        PICTURE X(1).
399400160229              05  AD22-TOTALNETDEPOSIT             PICTURE 9(11)V9(2).
399500160229              05  AD22-TOTALGROSSWITHDRAWAL-SIGN   PICTURE X(1).
399600160229              05  AD22-TOTALGROSSWITHDRAWAL        PICTURE 9(11)V9(2).
399700160229              05  AD22-TOTALCLOSINGMV              PICTURE 9(11)V9(2).
399800160229              05  AD22-TOTALINCREASE-SIGN          PICTURE X(1).
399900160229              05  AD22-TOTALINCREASE               PICTURE 9(11)V9(2).
400000160229              05  AD22-SPOUSEFIRSTNAME             PICTURE X(20).
400100160229              05  AD22-SPOUSEMIDDLENAME            PICTURE X(20).
400200160229              05  AD22-SPOUSELASTNAME              PICTURE X(20).
400300160229              05  AD22-RRSPTORRIF                  PICTURE X(1).
400400160229              05  AD22-TOTALACCRUALINTERESTPRIOR   PICTURE 9(11)V9(2).
400500160229124070        05  AD22-FINALTOPUPDATE              PICTURE 9(8).
400600201118185455        05  AD22-DBRST                       PICTURE X(5).
400700201118185455        05  AD22-DBGNEXTRESETDTE             PICTURE 9(8).
400800201118185455        05  AD22-DBGFINALRESETDTE            PICTURE 9(8).
400900201118185455        05  AD22-MBRST                       PICTURE X(5).
401000201118185455        05  AD22-MBGNEXTRESETDTE             PICTURE 9(8).
401100201118185455        05  AD22-MBGFINALRESETDTE            PICTURE 9(8).
401200201224185455        05  AD22-ACTFILLER                   PICTURE X(1021).
401300160229124070*       05  AD22-ACTFILLER                   PICTURE X(1071).
401400160229
401500160229
401600160229
401700160229
401800160229      *RFS117038 End
401900160229
402000160229      * RFS132121-START
402100160229       01  ACCOUNT-LEVEL-ASSET-ALLOC-DTL.
402200160229      ***********************************************************************
402300160229      *                                                                     *
402400160229      * CPYJSISTMT: Statement File - ACCOUNT-LEVEL-ASSET-ALLOCATION-DETAILS *
402500160229      *                                                                     *
402600160229      ***********************************************************************
402700160229
402800160229      *****
402900160229      *****    - INVESTOR-NO will be formatted.
403000160229      *****    - ACCOUNT will be formatted.
403100160229      *****    - SECTION-NO will always be 16.
403200160229      *****    - SECTION-SEQUENCE will formatted from 000001 to 999998,
403300160229      *****                       999999 will be reserved for the total.
403400160229      *****    - LANGUAGE will be one of the following.
403500160229      *****        'E' English
403600160229      *****        'F' French
403700160229      *****    - DEALER will be formatted.
403800160229      *****    - SALESREP will be formatted.
403900160229      *****
404000160229
404100160229           03  AAA16-INVESTOR-NO                 PICTURE 9(9).
404200160229           03  AAA16-ACCOUNT-NO                  PICTURE 9(9).
404300160229           03  AAA16-SECTION-NO                  PICTURE 9(2).
404400160229           03  AAA16-SECTION-SEQUENCE            PICTURE 9(6).
404500160229           03  AAA16-LANGUAGE                    PICTURE X(1).
404600160229           03  AAA16-DEALER-CODE                 PICTURE X(4).
404700160229           03  AAA16-DEALER-REP-CODE             PICTURE X(6).
404800160229           03  AAA16-MODEL-TYPE                  PICTURE X(1).
404900160229           03  AAA16-MODEL-CODE                  PICTURE X(5).
405000160229           03  AAA16-MODEL-DESCRIPTION           PICTURE X(35).
405100160229           03  AAA16-START-DATE                  PICTURE 9(8).
405200160229           03  AAA16-END-DATE                    PICTURE 9(8).
405300160229           03  AAA16-INVESTMENT-STRUC-CODE       PICTURE X(5).
405400160229           03  AAA16-INVESTMENT-STRUCE-DESC      PICTURE X(25).
405500160229           03  AAA16-INVESTMENT-CODE             PICTURE X(5).
405600160229           03  AAA16-TARGET-ALLOC-PERCENT        PICTURE 9(05)V9(2).
405700160229           03  AAA16-ACTUAL-ALLOC-PERCENT        PICTURE 9(05)V9(2).
405800160229           03  AAA16-ALLOC-VARIANCE-PERCENT      PICTURE 9(05)V9(2).
405900160229           03  AAA16-ALLOC-VARIANCE-PERC-SIGN    PICTURE X(1).
406000160229           03  AAA16-FILLER                      PICTURE X(1176).
406100160229           03  AAA16-SECTION-SEQUENCE-2          PICTURE 9(6).
406200160229           03  AAA16-STATEMENT-START-DATE        PICTURE 9(8).
406300160229           03  AAA16-STATEMENT-END-DATE          PICTURE 9(8).
406400160229           03  AAA16-EOR-INDICATOR               PICTURE X(1).
406500161012      * RFS132121-END
406600161012
406700161012      * RFS156507 START *
406800161012
406900160229       01  ACCOUNT-BPTTXN-SECTION.
407000161013
407100161013      *****    - INVESTOR-NO WILL BE FORMATTED.
407200161013      *****    - ACCOUNT WILL BE FORMATTED.
407300161013      *****    - SECTION-NO WILL ALWAYS BE 79.
407400161013      *****    - SECTION-SEQUENCE WILL FORMATTED FROM 000001 TO 999999.
407500161013      *****    - LANGUAGE WILL BE ONE OF THE FOLLOWING.
407600161013      *****        'E' ENGLISH
407700161013      *****        'F' FRENCH
407800161013      *****    - DEALER WILL BE FORMATTED.
407900161013      *****    - SALESREP WILL BE FORMATTED.
408000161013      *****
408100161013           03  BPT-INVESTOR-NO                  PICTURE 9(9).
408200161013           03  BPT-ACCOUNT                      PICTURE 9(9).
408300161013           03  BPT-SECTION-NO                   PICTURE 9(2).
408400161013           03  BPT-SECTION-SEQUENCE             PICTURE 9(6).
408500161013           03  BPT-LANGUAGE                     PICTURE X(1).
408600161013           03  BPT-DEALER                       PICTURE X(4).
408700161013           03  BPT-SALESREP                     PICTURE X(6).
408800161013           03  BPT-ACCOUNT-TYPE                 PICTURE X(35).
408900161013           03  BPT-SPOUSAL                      PICTURE X(1).
409000161013           03  BPT-LOCKED-IN                    PICTURE X(1).
409100161013           03  BPT-NOMINEE                      PICTURE X(1).
409200161013           03  BPT-GROUP                        PICTURE X(1).
409300161013           03  BPT-SPOUSAL-CONTRIB              PICTURE X(40).
409400161013           03  BPT-SPOUSAL-SIN                  PICTURE 9(9).
409500161013           03  BPT-TRADE-DATE                   PICTURE 9(8).
409600161013           03  BPT-PROCESS-DATE                 PICTURE 9(8).
409700161013           03  BPT-FUND-NO                      PICTURE X(8).
409800161013           03  BPT-INVESTMENT-NAME              PICTURE X(35).
409900161013           03  BPT-CURRENCY                     PICTURE X(3).
410000161013           03  BPT-OPEN-DATE                    PICTURE 9(8).
410100161013           03  BPT-TRANS-TYPE-CODE              PICTURE X(3).
410200161013           03  BPT-TRANS-TYPE                   PICTURE X(25).
410300161013           03  BPT-GROSS                        PICTURE 9(11)V9(2).
410400161013           03  BPT-GROSS-SIGN                   PICTURE X(1).
410500161013           03  BPT-DSC                          PICTURE 9(9)V9(2).
410600161013           03  BPT-DSC-SIGN                     PICTURE X(1).
410700161013           03  BPT-TAX                          PICTURE 9(9)V9(2).
410800161013           03  BPT-TAX-SIGN                     PICTURE X(1).
410900161013           03  BPT-COMMISSION                   PICTURE 9(9)V9(2).
411000161013           03  BPT-COMMISSION-SIGN              PICTURE X(1).
411100161013           03  BPT-NET                          PICTURE 9(11)V9(2).
411200161013           03  BPT-NET-SIGN                     PICTURE X(1).
411300161013           03  BPT-UNIT-PRICE                   PICTURE 9(5)V9(4).
411400161013           03  BPT-UNIT-PRICE-SIGN              PICTURE X(1).
411500161013           03  BPT-NO-OF-UNITS                  PICTURE 9(9)V9(3).
411600161013           03  BPT-NO-OF-UNITS-SIGN             PICTURE X(1).
411700161013           03  BPT-UNIT-BALANCE                 PICTURE 9(9)V9(3).
411800161013           03  BPT-UNIT-BALANCE-SIGN            PICTURE X(1).
411900161013           03  BPT-ACB-PRICE                    PICTURE 9(5)V9(4).
412000161013           03  BPT-ACB-SIGN                     PICTURE X(1).
412100161013           03  BPT-CPTL-GAIN                    PICTURE 9(11)V9(2).
412200161013           03  BPT-CPTL-GAIN-SIGN               PICTURE X(1).
412300161013           03  BPT-ACCOUNT-TYPE-SHORT           PICTURE X(5).
412400161013           03  BPT-UNIT-PRICE-BASE              PICTURE 9(5)V9(4).
412500161013           03  BPT-MKT-VALUE                    PICTURE 9(11)V9(2).
412600161013           03  BPT-MKT-VALUE-BASE               PICTURE 9(11)V9(2).
412700161013           03  BPT-ACB-UNITS-TRANSACTED         PICTURE 9(5)V9(4).
412800161013           03  BPT-EXCHANGE-RATE                PICTURE 9(2)V9(7).
412900161013           03  BPT-ADJUSTMENT-REASON            PICTURE X(40).
413000161013           03  BPT-CONTR-REDEMP-DESCRIPTION     PICTURE X(25).
413100161013           03  BPT-PAYMENT-TYPE-CODE            PICTURE X(01).
413200161013           03  BPT-OTHER-ACCOUNT                PICTURE 9(9).
413300161013           03  BPT-OTHER-FUND-NO                PICTURE X(8).
413400161013           03  BPT-OTHER-INVESTMENT-NAME        PICTURE X(35).
413500161013           03  BPT-GROSS-NET-DIFF               PICTURE 9(9)V9(2).
413600161013           03  BPT-GROSS-NET-DIFF-SIGN          PICTURE X(1).
413700161013           03  BPT-TRANS-STATUS-CODE            PICTURE X(3).
413800161013           03  BPT-REVERSAL-INDICATOR           PICTURE X(1).
413900161013           03  BPT-TRANS-ORIGIN-CODE            PICTURE X(3).
414000161013           03  BPT-CPTL-GAIN-BASE               PICTURE 9(11)V9(2).
414100161013           03  BPT-CPTL-GAIN-BASE-SIGN          PICTURE X(1).
414200161013           03  BPT-SALES-CHARGE                 PICTURE 9(9)V9(2).
414300161013           03  BPT-SALES-CHARGE-SIGN            PICTURE X(1).
414400161013           03  BPT-CONTR-REDEMP-CODE            PICTURE X(6).
414500161013           03  BPT-ASSIGNMENT-INDICATOR         PICTURE X.
414600161013           03  BPT-ASSIGNED-UNITS               PICTURE 9(9)V9(3).
414700161013           03  BPT-INVESTMENT-GRP-CATEGORY      PICTURE X(5).
414800161013           03  BPT-INVESTMENT-GRP-CAT-DESC      PICTURE X(25).
414900161013           03  BPT-DISTRIBUTION-ROC             PICTURE 9(9)V9(2).
415000161013           03  BPT-DISTRIBUTION-ROC-SIGN        PICTURE X.
415100161013           03  BPT-TAXABLE-AMOUNT               PICTURE 9(9)V9(2).
415200161013           03  BPT-TAXABLE-AMOUNT-SIGN          PICTURE X.
415300161013           03  BPT-GST                          PICTURE 9(9)V9(2).
415400161013           03  BPT-PST                          PICTURE 9(9)V9(2).
415500161013           03  BPT-DSC-SCHEDULE                 PICTURE X(5).
415600161013           03  BPT-ADMIN-FEES                   PICTURE 9(9)V9(2).
415700161013           03  BPT-FED-TAX                      PICTURE 9(9)V9(2).
415800161013           03  BPT-PROV-TAX                     PICTURE 9(9)V9(2).
415900161013           03  BPT-TAX-PROVINCE                 PICTURE X(3).
416000161013           03  BPT-TAX-COUNTRY                  PICTURE X(3).
416100161013           03  BPT-REP-NAME                     PICTURE X(40).
416200161013           03  BPT-NON-TAX-DEDUCTIONS           PICTURE 9(11)V9(2).
416300161013           03  BPT-NON-TAX-DEDUCT-SIGN          PICTURE X(1).
416400161013           03  BPT-PHANTOM-FUND-NUMBER          PICTURE X(5).
416500161013           03  BPT-TOTAL-DEDUCTIONS             PICTURE 9(9)V9(2).
416600161013           03  BPT-TOTAL-DEDUCTIONS-SIGN        PICTURE X(1).
416700161013           03  BPT-FEDC-CLAWBACK                PICTURE 9(9)V9(2).
416800161013           03  BPT-PRVC-CLAWBACK                PICTURE 9(9)V9(2).
416900161013           03  BPT-ERF                          PICTURE 9(9)V9(2).
417000161013           03  BPT-ROC-RATE                     PICTURE 9(5)V9(4).
417100161013           03  BPT-ROC-RATE-SIGN                PICTURE X(1).
417200161013           03  BPT-US-BACKUP-WHTX               PICTURE 9(11)V9(2).
417300161013           03  BPT-US-BACKUP-WHTX-SIGN          PICTURE X(1).
417400161013           03  BPT-REDEM-CHARGE                 PICTURE 9(9)V9(2).
417500161013           03  BPT-REDEM-CHARGE-SIGN            PICTURE X(1).
417600161013           03  BPT-MATURITY-TYPE                PICTURE X(1).
417700161013           03  BPT-CONTRACT-XREF                PICTURE X(9).
417800161013           03  BPT-CONTRACT-NO                  PICTURE X(9).
417900161013           03  BPT-DEPOSIT-INDICATOR            PICTURE X(10).
418000161013           03  BPT-GLWB-DEPOSIT-ALLOCATION      PICTURE 9(03)V99.
418100161013           03  BPT-GLWA-DEPOSIT                 PICTURE S9(11)V99.
418200161013           03  BPT-FIRST-WTHDRWL-AFT-ELIG-AGE   PICTURE X(01).
418300161013           03  BPT-GLWA-WITHDRAWN               PICTURE S9(11)V99.
418400161013           03  BPT-EXCESS-GLWA-WITHDRAWN        PICTURE S9(11)V99.
418500161013           03  BPT-GW-BASE-BEFORE-TRADE         PICTURE S9(11)V99.
418600161013           03  BPT-GW-BASE-AFTER-TRADE          PICTURE S9(11)V99.
418700161013           03  BPT-GW-BASE-CAL-METHOD           PICTURE X(15).
418800161013           03  BPT-FIE-BASE-BEFORE-TRADE        PICTURE S9(11)V99.
418900161013           03  BPT-FIE-BASE-AFTER-TRADE         PICTURE S9(11)V99.
419000161013           03  BPT-FIE-BASE-CAL-METHOD          PICTURE X(15).
419100161013           03  BPT-PLACEMENT-DATE               PICTURE 9(08).
419200161013           03  BPT-TRANS-NO                     PICTURE 9(09).
419300161013           03  BPT-TRANS-SUB-TYPE               PICTURE X(05).
419400161013           03  BPT-TRANS-SUB-TYPE-DESC          PICTURE X(25).
419500161013           03  BPT-OMS-CMPCDE                   PICTURE X(3).
419600161013           03  BPT-OMS-FUND-NO                  PICTURE X(5).
419700161013           03  BPT-OMS-TRANS-TYPE-CODE          PICTURE X(3).
419800161013           03  BPT-OMS-TRANS-ORIGIN-CODE        PICTURE X(3).
419900161013           03  BPT-OMS-TRANS-SUB-TYPE           PICTURE X(5).
420000161013           03  BPT-OMS-REVERSE                  PICTURE X(1).
420100161013           03  BPT-OMS-CONTR-REDEEM             PICTURE X(6).
420200161013           03  BPT-OMS-INCLUDE                  PICTURE X(1).
420300161013           03  BPT-RATE                         PICTURE 9(3)V9(3).
420400161013           03  BPT-MATURITY-DATE                PICTURE 9(8).
420500161013           03  BPT-MATURITY-VALUE               PICTURE 9(11)V9(2).
420600161013           03  BPT-OMS-SUPPRESS                 PICTURE X(1).
420700161013           03  BPT-CERTIFICATE-NO               PICTURE X(12).
420800161013           03  BPT-TERM                         PICTURE S9(5).
420900161013           03  BPT-INTEREST-RATE                PICTURE S9(4)V9(3).
421000161013           03  BPT-TERM-GROUP                   PICTURE X(10).
421100161013           03  BPT-TRANSACTED-BOOK-VALUE        PICTURE 9(11)V9(2).
421200161013           03  BPT-TRANSACTED-BOOK-VALUE-SIGN   PICTURE X(1).
421300161013           03  FILLER                           PICTURE X(174).
421400161013           03  BPT-SECTION-SEQUENCE-2           PICTURE 9(6).
421500161013           03  BPT-START-DATE                   PICTURE 9(08).
421600161013           03  BPT-END-DATE                     PICTURE 9(08).
421700161013           03  BPT-EOR-INDICATOR                PICTURE X(1).
421800161013
421900999999      * RFS156507 ENDS *
422000210823      * RFS186199 - Begin
422001210823       01  INVESTOR-SUMMARY-SECTION-3.
422002210823           03  IS3-INVESTOR-NO                  PICTURE 9(9).
422003210823           03  IS3-ACCOUNT                      PICTURE 9(9).
422004210823           03  IS3-SECTION-NO                   PICTURE 9(2).
422005210823           03  IS3-SECTION-SEQUENCE             PICTURE 9(6).
422006210823           03  IS3-LANGUAGE                     PICTURE X(1).
422007210823           03  IS3-DEALER                       PICTURE X(4).
422008210823           03  IS3-SALESREP                     PICTURE X(6).
422009210823           03  IS3-ACCOUNT2                     PICTURE 9(9).
422010210823           03  IS3-SERIES-ID                    PICTURE X(5).
422011210823           03  IS3-SERIES-DESCRIPTION           PICTURE X(45).
422012210823           03  IS3-SERIES-DESCRIPTION-FRENCH    PICTURE X(45).
422013210823           03  IS3-ELECTION-DATE                PICTURE 9(8).
422014210823           03  IS3-EXERCISE-DATE                PICTURE 9(8).
422015210823           03  IS3-ELIGIBLE-DATE                PICTURE 9(8).
422016210823           03  IS3-GWA-START-DATE               PICTURE 9(8).
422017210823           03  IS3-GLWA-WITHDRAWN-YTD-VALUE     PICTURE 9(11)V9(2).
422018210823           03  IS3-GLWA-WITHDR-YTD-VAL-SIGN     PICTURE X(1).
422019210823           03  IS3-GLWA-REMAINING-YTD-VALUE     PICTURE 9(11)V9(2).
422020210823           03  IS3-GLWA-REMAIN-YTD-VAL-SIGN     PICTURE X(1).
422021210823           03  IS3-GLWA-PREVIOUS-YR-VALUE       PICTURE 9(11)V9(2).
422022210823           03  IS3-GLWA-PREV-YR-VALUE-SIGN      PICTURE X(1).
422023210823           03  IS3-GLWA-RATE                    PICTURE 9(3)V9(2).
422024210823           03  IS3-EXCESS-GLWA-WITHDR-YTD-VAL   PICTURE 9(11)V9(2).
422025210823           03  IS3-EXCESS-WITHDR-YTD-VAL-SIGN   PICTURE X(1).
422026210823           03  IS3-GLWA-AWD-WITHDRAWAL-OPTION   PICTURE X(1).
422027210823           03  IS3-GLWA-AWD-WITHDRAWAL-AMOUNT   PICTURE 9(11)V9(2).
422028210823           03  IS3-GLWA-AWD-SPLIT-OPTION        PICTURE X(1).
422029210823           03  IS3-FIE-BASE-VALUE               PICTURE 9(11)V9(2).
422030210823           03  IS3-FIE-BASE-VALUE-SIGN          PICTURE X(1).
422031210823           03  IS3-BONUS-ELIG-INDICATOR         PICTURE X(1).
422032210823           03  IS3-BONUS-ADDED-TO-GW-BASE       PICTURE 9(11)V9(2).
422033210823           03  IS3-BONUS-ADDED-TO-GW-BASE-DTE   PICTURE 9(8).
422034210823           03  IS3-BONUS-PERCENTAGE             PICTURE 9(3)V9(2).
422035210823           03  IS3-GBB-PREVIOUS-VALUE           PICTURE 9(11)V9(2).
422036210823           03  IS3-GBB-RESULTING-VALUE          PICTURE 9(11)V9(2).
422037210823           03  IS3-GLWA-INITIAL-DEPOSIT-DAYS    PICTURE 9(5).
422038210823           03  IS3-ORIGINAL-GLWA                PICTURE 9(11)V9(2).
422039210823           03  IS3-GLWA-RIF-MIN                 PICTURE 9(11)V9(2).
422040210823           03  IS3-GW-BASE-VALUE                PICTURE 9(11)V9(2).
422041210823           03  IS3-GW-BASE-VALUE-SIGN           PICTURE X(1).
422042210823           03  IS3-GWB-PREVIOUS-VALUE           PICTURE 9(11)V9(2).
422043210823           03  IS3-GWB-RESULTING-VALUE          PICTURE 9(11)V9(2).
422044210823           03  IS3-NW-BASE-VALUE                PICTURE 9(11)V9(2).
422045210823           03  IS3-NW-BASE-VALUE-SIGN           PICTURE X(1).
422046210823           03  IS3-NWB-PREVIOUS-VALUE           PICTURE 9(11)V9(2).
422047210823           03  IS3-NWB-RESULTING-VALUE          PICTURE 9(11)V9(2).
422048210823           03  IS3-RIDER-CANCELLATION-DATE      PICTURE 9(8).
422049210823           03  IS3-LAST-RESET-DATE              PICTURE 9(8).
422050210823           03  IS3-ACCOUNT-MARKET-VALUE         PICTURE 9(11)V9(2).
422051210823           03  IS3-MDBA-RESET-DATE              PICTURE 9(8).
422052210823           03  IS3-PREVIOUS-MDBA-RESET-AMT      PICTURE 9(11)V9(2).
422053210823           03  IS3-RESULTING-MDBA-RESET-AMT     PICTURE 9(11)V9(2).
422054210823           03  IS3-MDBA-INFLATION-ADJ-AMT       PICTURE 9(11)V9(2).
422055210823           03  IS3-MDBA-NON-INFLATION-ADJ-AMT   PICTURE 9(11)V9(2).
422056210823           03  IS3-ACCOUNT-GUAR-MAT-VALUE       PICTURE 9(11)V9(2).
422057210823           03  IS3-ACCOUNT-GUAR-DEATH-VALUE     PICTURE 9(11)V9(2).
422058210823           03  IS3-COG-RESET-EXECUTED           PICTURE X(1).
422059210823           03  IS3-GMV-RESET-EXECUTED           PICTURE X(1).
422060210823           03  IS3-FILLER                       PICTURE X(783).
422061210823           03  IS3-SECTION-SEQUENCE-2           PICTURE 9(6).
422062210823           03  IS3-START-DATE                   PICTURE 9(8).
422063210823           03  IS3-END-DATE                     PICTURE 9(8).
422064210823           03  IS3-EOR-INDICATOR                PICTURE X(1).
422065210823      * RFS186199 - End
422066210823      * RFS186199 STARTS*
422067210823       01  FUND-LONG-DESC-SECTION.
422068210823           03  FLD-INVESTOR-NO                  PICTURE 9(9).
422069210823           03  FLD-ACCOUNT                      PICTURE 9(9).
422070210823           03  FLD-SECTION-NO                   PICTURE 9(2).
422071210823           03  FLD-SECTION-SEQUENCE             PICTURE 9(6).
422072210823           03  FLD-LANGUAGE                     PICTURE X(1).
422073210823           03  FLD-DEALER                       PICTURE X(4).
422074210823           03  FLD-SALESREP                     PICTURE X(6).
422075210823           03  FLD-INVESTMENT-CODE              PICTURE X(5).
422076210823           03  FLD-ENGLISH                      PICTURE X(80).
422077210823           03  FLD-FRENCH                       PICTURE X(80).
422078210823           03  FLD-FILLER                       PICTURE X(1125).
422079210823           03  FLD-SECTION-SEQUENCE-2           PICTURE 9(6).
422080210823           03  FLD-START-DATE                   PICTURE 9(8).
422081210823           03  FLD-END-DATE                     PICTURE 9(8).
422082210823           03  FLD-EOR-INDICATOR                PICTURE X(1).
422083210823      * RFS186199 ENDS*
422084210823
