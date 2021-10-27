000100121026/********************************************************************/
000200121026/* TITLE        : ADHOC JOB - TO GENERATE RESERVE EXTRACT           */
000300121210/* SYSTEM       : AS/400                                           */
000400121026/* PROGRAM TYPE : CL                                                */
000500121026/* PROGRAMMER   : DEEPIKA A                                         */
000600121026/* DESCRIPTION  : THIS IS A NEW ADHOC JOB TO GENERATE               */
000700121026/*                RESERVE EXTRACT                                   */
000800121026/* DATE CREATED : 2012/10/26                                        */
000900121026/* RFS NUMBER   : RFS110949                                         */
001000121026/* CALLED BY    : ADHOC JOB                                         */
001100121026/* PARAMETERS   : NONE                                              */
001200121026/* CALLS PROGRAM: JOBSEGCMT - BATCH JOB TO CREATE RESERVE EXTRACT   */
001300060509/********************************************************************/
001400121026/*    C H A N G E   H I S T O R Y                                   */
001500121026/* ******************************************************************/
001600121026/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE                */
001700121026/* ******************************************************************/
001800121026/* DEEPIKA A  * 2012/10/26   * RFS110949 - THIS IS AN ADHOC JOB     */
001900121026/*            *              * WHICH CALLS A MONTHLY JOB TO         */
002000121210/*            *              * CREATE RESERVE EXTRACT FILE          */
002100121026/* ******************************************************************/
002200060509
002300060509             PGM
002400060509
002500060509            /* Set up LDA (Byte 17  = 'A') for the Adhoc  mode  */
002600060509             CHGDTAARA  DTAARA(*LDA (17 1)) VALUE('A')
002700060509
002800060509            /* Display Local-Data-Area             */
002900060509             DSPDTAARA  DTAARA(*LDA) OUTPUT(*PRINT)
003000060509
003100121027            /* CALL RESERVE EXTRACT PROGRAM */
003200121026             CALL      PGM(JOBSEGCMT)
003300060509
003400060509            /* Set up LDA (Byte 17  = ' ') back to space       */
003500060509             CHGDTAARA  DTAARA(*LDA (17 1)) VALUE(' ')
003600121027             ENDPGM
