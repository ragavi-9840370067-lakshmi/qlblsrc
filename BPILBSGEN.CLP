000100021030/********************************************************************/
000200021030/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE                */
000300021030/********************************************************************/
000400021030/* Fatema Haji* 2002/10/30   * RFS15384. Recompile for              */
000500021030/*            *              * TRANS-BUY-LSIF                       */
000600021030/********************************************************************/
000700021030
000800960731             PGM
000900960729
001000980903/* *************** IMPORTANT!! *******************/
001100980903/* MUST CHANGE THE LIBRARY BEFORE RUNNING THIS   */
001200980903/* CONVERSION PROGRAM TO UPDATE THE FILES IN THE */
001300980903/* CORRECT LIBRARY.                              */
001400980903/*                                               */
001500980903/* Convert TRANS to TRANS-BUY/SELL-LSIF and      */
001600980903/* access the conversion file via the logical    */
001700980903/* index - transaction date and transaction code */
001800980903/*-----------------------------------------------*/
001900980903
002000980903/*-----------------------------------------------*/
002100971104/* Reset all data files to be loaded             */
002200971104/*-----------------------------------------------*/
002300971104
002400981002             CLRPFM     FILE(MFAPRDBPDB/MFATBLP)
002500971216             MONMSG     MSGID(CPF0000)
002600981002             CLRPFM     FILE(MFAPRDBPDB/MFATSLP)
002700971216             MONMSG     MSGID(CPF0000)
002800981002             CLRPFM     FILE(MFAPRDBPDB/MFATLTIP)
002900971216             MONMSG     MSGID(CPF0000)
003000981002             CLRPFM     FILE(MFAPRDBPDB/MFAMBLNFP)
003100971216             MONMSG     MSGID(CPF0000)
003200981002             CLRPFM     FILE(MFAPRDBPDB/MFAMBLNDP)
003300971216             MONMSG     MSGID(CPF0000)
003400980903
003500980903/* Copy the BPITRCTGP file from BPIDEVDB library  */
003600980903
003700980903             CPYF       FROMFILE(BPIDEVDB/BPITRCTGP) +
003800981002                          TOFILE(MFAPRDBPOJ/BPITRCTGP) +
003900980903                          MBROPT(*REPLACE) CRTFILE(*YES) FMTOPT(*NOCHK)
004000980903             MONMSG     MSGID(CPF0000)
004100980903
004200971104
004300971104/*-----------------------------------------------*/
004400980903/* BPILOAD process generates the LBSLOAD subfile */
004500980903/* and sorts the TRANS records in 2 parts and    */
004600980903/* this is done in two requests:                 */
004700980903/* 1. For Historically converted                 */
004800980903/* 2. For transactions processed in UNITRAX      */
004900971104/*-----------------------------------------------*/
005000980903
005100981002             QTP        AUTO(MFAPRDBPOJ/BPILOAD)
005200971107
005300980903/*---------------------------------------------------*/
005400980903/* Loads all the transactions in LBSLOAD subfile     */
005500980903/* into the new LSIF buy and sell files.  This       */
005600980903/* includes purchases, redemptions, and reversals    */
005700980903/*---------------------------------------------------*/
005800980903
005900981002             CALL       PGM(MFAPRDBPOJ/GENSEL)
006000971107
006100971105/*----------------------------------------------------------------*/
006200980903/* This BPIGENC process loads the corresponding reversal records  */
006300980903/* into the new LSIF buy and sell files.                          */
006400971105/*----------------------------------------------------------------*/
006500980903
006600981002             QTP        AUTO(MFAPRDBPOJ/BPIGENC)
006700971105
006800971128/*----------------------------------------------------------------*/
006900971128/* Load the clawback information into the TRANS-SELL-LSIF files.  */
007000980903/* It loads the waive-clawback codes for both federal and         */
007100980903/* provincial as well as the respective amounts.                  */
007200971128/*----------------------------------------------------------------*/
007300980903
007400981002             QTP        AUTO(MFAPRDBPOJ/BPICLWBK)
007500971128
007600971127/*----------------------------------------------------------------*/
007700980903/* Load the MOF transaction ids and generate the non-financial    */
007800980903/* records.                                                       */
007900971105/*----------------------------------------------------------------*/
008000980903
008100981002             QTP        AUTO(MFAPRDBPOJ/BPITRC)
008200971105
008300971217/*----------------------------------------------------------------*/
008400971217/* Run report to check for accounts with unit balances out.       */
008500971217/*----------------------------------------------------------------*/
008600981002             QTP        AUTO(MFAPRDBPOJ/BPIUNIT)
008700981002             QUIZ       AUTO(MFAPRDBPOJ/BPIUNIT)
008800971217
008900981002/*----------------------------------------------------------------*/
009000981002/* Remove the BPITRCTGP file from MFAPRDBPOJ library.             */
009100981002/*----------------------------------------------------------------*/
009200981002             DLTF       FILE(MFAPRDBPOJ/BPITRCTGP)
009300981002             MONMSG     MSGID(CPF0000)
009400981002
009500930824             ENDPGM
