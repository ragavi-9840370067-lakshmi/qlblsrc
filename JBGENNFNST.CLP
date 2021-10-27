000100190411/********************************************************************/
000200190411/* TITLE        : NON FINANCIAL STATISTICS REPORT - PROMPT          */
000300190411/*                                                                  */
000400190411/* AUTHOR       : JEWELSTONES SYSTEMS INC.                          */
000500190411/*                                                                  */
000600190411/* PROGRAM      : JBGENNFNST                                        */
000700190411/*                                                                  */
000800190411/* DATE-WRITTEN : APRIL 2019                                        */
000900190411/*                                                                  */
001000190411/* DESCRIPTION  : THIS WILL GENERATE REPORT ON NUMBER OF ADDITIONS  */
001100190411/*                OR CHANGES MADE TO NON FIANCIAL DATA.             */
001200190411/********************************************************************/
001300190411/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION                          */
001400190502/* KAVYA K    *  2019/04/11  * RFS183345-PH REWRITE FOR GENNFNST QTP*/
001500190411/********************************************************************/
001600190411PGM
001700190411            DCL        VAR(&STARTDATE) TYPE(*CHAR) LEN(8)
001800190411            DCL        VAR(&ENDDATE) TYPE(*CHAR) LEN(8)
001900190411            RTVDTAARA  DTAARA(*LDA (1 8)) RTNVAR(&STARTDATE)
002000190411            RTVDTAARA  DTAARA(*LDA (9 8)) RTNVAR(&ENDDATE)
002100190411            CHGDTAARA  DTAARA(*LDA (608 8)) VALUE(&STARTDATE)
002200190411            CHGDTAARA  DTAARA(*LDA (616 8)) VALUE(&ENDDATE)
002300190411            DSPDTAARA  DTAARA(*LDA)
002400190412            CALL PGM(JOBNONFNST)
002500190411            QUIZ  AUTO(*LIBL/GENNFNST)
002600190411ENDPGM
