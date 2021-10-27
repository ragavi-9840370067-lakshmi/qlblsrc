000100170404       IDENTIFICATION DIVISION.
000101170404       PROGRAM-ID.     AXYTRNEXT.
000102170404       INSTALLATION.   CITIGROUP.
000103170404       AUTHOR.         A. Chan.
000104170404       DATE-WRITTEN.   Jun 9, 2008.
000105170404       DATE-COMPILED.
000106170404      ******************************************************************
000107170404      *                                                                *
000108170404      *    RFS-NUMBER : RFS-45389                                      *
000109170404      *                                                                *
000110170404      *    DESCRIPTION: This module is used extract daily transaction  *
000111170404      *       into Qtemp file MFAAXYTRN for Saxon`s AXY System.        *
000112170404      *                                                                *
000113170404      *     PARAMETERS: Process Date                                   *
000114170404      *                                                                *
000115170404      * LINKED MODULES: *NONE                                          *
000116170404      *                                                                *
000117170404      ******************************************************************
000118170404      *                                                                *
000119170404      *    C H A N G E   H I S T O R Y                                 *
000120170404      *                                                                *
000121170404      ******************************************************************
000122170404      * PROGRAMMER | YYYY/MM/DD   | DESCRIPTION                        *
000123170404      ******************************************************************
000124170404      * A. Chan    | 2008/06/09   | RFS45389 - Initial Coding          *
000125170404      ******************************************************************
000126170404
000127170404       ENVIRONMENT DIVISION.
000128170404       CONFIGURATION SECTION.
000129170404       SOURCE-COMPUTER. IBM-AS400.
000130170404       OBJECT-COMPUTER. IBM-AS400.
000131170404       SPECIAL-NAMES.
000132170404      /
000133170404       INPUT-OUTPUT SECTION.
000134170404       FILE-CONTROL.
000135170404      **
000136170404      /
000137170404      ******************************************************************
000138170404      *    D A T A   D I V I S I O N                                   *
000139170404      ******************************************************************
000140170404      *                                                                *
000141170404       DATA DIVISION.
000142170404      ******************************************************************
000143170404      *    F I L E   S E C T I O N                                     *
000144170404      ******************************************************************
000145170404      *                                                                *
000146170404       FILE SECTION.
000147170404      **
000148170404      /
000149170404      ******************************************************************
000150170404      *     W O R K I N G  S T O R A G E  S E C T I O N                *
000151170404      ******************************************************************
000152170404       WORKING-STORAGE SECTION.
000153170404
000154170404      * SQL usage Variables
000155170404
000156170404       01  lt-SQL-Variables.
000157170404           05 ld-Date                      PIC S9(9) COMP-3.
000158170404           05 li-Count                     PIC S9(9) COMP-3.
000159170404
000160170404      * Cobol standard copybook
000161170404
000162170404          COPY CPYSQLFLD
000163170404               REPLACING    "CURRENT_PROGRAM"  BY "AXYTRNEXT ".
000164170404
000165170404           EXEC SQL
000166170404             INCLUDE SQLCA
000167170404           END-EXEC.
000168170404
000169170404           EXEC SQL
000170170404             INCLUDE SQLDA
000171170404           END-EXEC.
000172170404
000173170404       01  WS-ERR-CODE                PIC X(02) VALUE "  ".
000174170404           88 lb-ErrorOK                        VALUE "00".
000175170404           88 lb-Error10-Drop                   VALUE "10".
000176170404           88 lb-Error20-Create                 VALUE "20".
000177170404           88 lb-Error30-Insert                 VALUE "30".
000178170404           88 lb-Error40-Declare                VALUE "40".
000179170404           88 lb-Error50-Fetch                  VALUE "50".
000180170404           88 lb-Error60-Insert                 VALUE "60".
000181170404           88 lb-Error70-Insert                 VALUE "70".
000182170404
000183170404      /
000184170404      ******************************************************************
000185170404      *         L I N K A G E    S E C T I O N                         *
000186170404      ******************************************************************
000187170404       LINKAGE SECTION.
000188170404
000189170404       01  pc-ProcessDate                          Pic X(08).
000190170404
000191170404       01  pd-ProcessDate redefines pc-ProcessDate Pic 9(8).
000192170404
000193170404      ******************************************************************
000194170404      *                                                                *
000195170404      *               M A I N L I N E                                  *
000196170404      *                                                                *
000197170404      ******************************************************************
000198170404       PROCEDURE DIVISION USING pc-ProcessDate.
000199170404
000200170404       MAINLINE.
000201170404
000202170404           PERFORM CREATE-QTEMP-TABLES
000203170404
000204170404           PERFORM SUMMARIZE-WITHHOLDING-TAX
000205170404
000206170404           PERFORM EXTRACT-TRANSACTIONS
000207170404
000208170404           PERFORM END-JOB
000209170404           .
000210170404      /
000211170404       EXTRACT-TRANSACTIONS.
000212170404
000213170404           EXEC SQL
000214170404
000215170404              INSERT INTO QTEMP/MFAAXYTRN
000216170404                SELECT
000217170404
000218170404test  *           trim(char(trn.placement_date)) || ","
000219170404test  *           || trim(char(trn.trans_no)) || ":" ||
000220170404test  *           trn.trans_type_code || "-" || trn.trans_status_code
000221170404test  *           || "-" || trn.trans_origin_code
000222170404test  *           || "-" || trn.investment_code || "|" ||
000223170404
000224170404                  TRIM(ACXR.ACCOUNT_NO_XREF2) || "," ||
000225170404                  TRIM(COALESCE(EXTRN.EXT_MAPPING_CODE1, " "))
000226170404                  || ", ,"  ||
000227170404                  TRIM(COALESCE(EXSEC.EXT_MAPPING_CODE1, " "))
000228170404                  || ","  ||
000229170404                  TRIM(COALESCE(EXSEC.EXT_MAPPING_CODE3, " "))
000230170404                  || ","  ||
000231170404                  SUBSTR(CHAR(TRADE_DATE),5,2) ||
000232170404                  SUBSTR(CHAR(TRADE_DATE),7,2) ||
000233170404                  SUBSTR(CHAR(TRADE_DATE),1,4) || "," || TRIM(
000234170404                  SUBSTR(CHAR(SETTLEMENT_DATE),5,2)  ||
000235170404                  SUBSTR(CHAR(SETTLEMENT_DATE),7,2)  ||
000236170404                  SUBSTR(CHAR(SETTLEMENT_DATE),1,4)) || ",," || TRIM(
000237170404                  CHAR(UNIT_AMT)) || ",,,caca,c,,,,,"   ||
000238170404                  TRIM(CHAR(GROSS_AMT)) || ",,,"  || TRIM(
000239170404                  CHAR(COALESCE(CHG.TOTCHG, 0))) || ",,,,,n,,,n,"
000240170404                  || Case When Trn.Trans_Type_Code = "BUY"
000241170404                  then "253" else " " end || ",,,"  ||
000242170404                  ",,,,,,,,,1,,,n,y,,,,,,,,,,,,,"
000243170404                FROM MFATRNP TRN
000244170404
000245170404                JOIN MFAACXR2P ACXR ON
000246170404                TRN.ACCOUNT_NO   = ACXR.ACCOUNT_NO AND
000247170404                ACXR.ACCOUNT_NO_XREF2 <> " "
000248170404
000249170404                LEFT OUTER JOIN QTEMP/AXYTOTCHG CHG ON
000250170404                TRN.PLACEMENT_DATE   = CHG.PLACEMENT_DATE    AND
000251170404                TRN.TRANS_NO         = CHG.TRANS_NO
000252170404
000253170404                LEFT OUTER JOIN MFAEXTCDP EXTRN ON
000254170404                EXTRN.EXTERNAL_ID    = "AXYTRNCDE"           AND
000255170404                TRN.TRANS_TYPE_CODE   = EXTRN.TRANS_TYPE_CODE  AND
000256170404                TRN.TRANS_STATUS_CODE = EXTRN.TRANS_STATUS_CODE
000257170404
000258170404                LEFT OUTER JOIN MFAEXTCDP EXSEC ON
000259170404                EXSEC.EXTERNAL_ID      = "AXYSECCDE"         AND
000260170404                TRN.INDUSTRY_FUND_CODE = EXSEC.INDUSTRY_FUND_CODE
000261170404
000262170404                WHERE TRN.PROCESS_DATE = :ld-Date      AND
000263170404                  (  (TRN.TRANS_STATUS_CODE = "HST"    AND
000264170404                      TRN.REVERSE          in(" ","N") AND
000265170404                      TRN.TRANS_ORIGIN_CODE <> "WOR")
000266170404                  OR (TRN.TRANS_STATUS_CODE = "HST"    AND
000267170404                      TRN.TRANS_ORIGIN_CODE = "WOR"    AND
000268170404                      TRN.REVERSE          in(" ","N") AND
000269170404                      NOT TRN.TRANS_TYPE_CODE IN("BUY","SEL"))
000270170404                  OR (TRN.TRANS_STATUS_CODE in("RVS","CAN"))
000271170404                  OR (TRN.TRANS_STATUS_CODE = "CON"    AND
000272170404                      TRN.TRANS_ORIGIN_CODE = "WOR"    AND
000273170404                      TRN.REVERSE          in(" ","N") AND
000274170404                      TRN.TRANS_TYPE_CODE IN("BUY","SEL"))   )
000275170404             ORDER BY ACXR.ACCOUNT_NO_XREF2
000276170404           END-EXEC
000277170404
000278170404           IF SQLSTATE NOT = WS-SQL-SUCCESSFUL AND
000279170404              SQLSTATE NOT = WS-SQL-END
000280170404             SET lb-Error30-Insert  TO TRUE
000281170404             MOVE SQLSTATE          TO WS-SQLSTATE
000282170404             PERFORM DSP-ERROR
000283170404             PERFORM END-JOB
000284170404           END-IF
000285170404
000286170404           EXEC SQL
000287170404             Select count(1) into :li-Count
000288170404               from qtemp/mfaaxytrn
000289170404           END-EXEC
000290170404
000291170404           if li-Count = 0
000292170404              EXEC SQL
000293170404                Insert into QTEMP/MFAAXYTRN
000294170404                  Values("0 record were found.")
000295170404              END-EXEC
000296170404           End-If
000297170404           .
000298170404
000299170404       CREATE-QTEMP-TABLES.
000300170404
000301170404           Move pd-ProcessDate to ld-Date
000302170404
000303170404           EXEC SQL
000304170404             Drop table QTEMP/AXYTRNCHG
000305170404           END-EXEC
000306170404
000307170404           EXEC SQL
000308170404             Drop table QTEMP/AXYTOTCHG
000309170404           END-EXEC
000310170404
000311170404           EXEC SQL
000312170404             Delete from QTEMP/MFAAXYTRN
000313170404           END-EXEC
000314170404
000315170404           EXEC SQL
000316170404            CREATE TABLE QTEMP/AXYTRNCHG  AS
000317170404              (SELECT PLACEMENT_DATE, TRANS_NO, DEDUCTION_CODE,
000318170404                      CHARGE FROM MFATRNCHP) WITH NO DATA
000319170404           END-EXEC
000320170404
000321170404           EXEC SQL
000322170404            CREATE TABLE QTEMP/AXYTOTCHG  AS
000323170404              (SELECT PLACEMENT_DATE, TRANS_NO,
000324170404                      SUM(CHARGE) AS TOTCHG FROM MFATRNCHP
000325170404               GROUP BY PLACEMENT_DATE, TRANS_NO) WITH NO DATA
000326170404           END-EXEC
000327170404
000328170404           .
000329170404
000330170404       SUMMARIZE-WITHHOLDING-TAX.
000331170404
000332170404           EXEC SQL
000333170404            INSERT INTO QTEMP/AXYTRNCHG
000334170404              (SELECT TRN.PLACEMENT_DATE, TRN.TRANS_NO,
000335170404                      CH.DEDUCTION_CODE, CH.CHARGE
000336170404                 FROM MFATRNP TRN, MFATRNCHP CH
000337170404                WHERE TRN.PROCESS_DATE   =  :ld-Date
000338170404                  AND TRN.PLACEMENT_DATE =  CH.PLACEMENT_DATE
000339170404                  AND TRN.TRANS_NO       =  CH.TRANS_NO
000340170404                  AND CH.DEDUCTION_CODE IN("NONT","PRVT","FEDT","USWT",
000341170404                                           "GST ","PST ")
000342170404              )
000343170404           END-EXEC
000344170404
000345170404           EXEC SQL
000346170404            INSERT INTO QTEMP/AXYTOTCHG
000347170404              (SELECT PLACEMENT_DATE, TRANS_NO,
000348170404                      SUM(CHARGE)
000349170404                 FROM QTEMP/AXYTRNCHG
000350170404               GROUP BY PLACEMENT_DATE, TRANS_NO
000351170404              )
000352170404           END-EXEC
000353170404
000354170404           .
000355170404
000356170404      ******************************************************************
000357170404      *  END OF JOB                                                    *
000358170404      ******************************************************************
000359170404       END-JOB.
000360170404
000361170404           GOBACK.
000362170404
000363170404      /
000364170404      ******************************************************************
000365170404      * DSP-ERROR AND FORCE-MSGW ROUTINES                              *
000366170404      ******************************************************************
000367170404          COPY CPYSQLRTN.
000368170404      *
000369170404      /
