000100060509/********************************************************************/
000200060509/*=>                                                                */
000300060509/* Description: MBI CUSTOM ACCOUNT EXTRACTS (ADHOC)                 */
000400060509/*              This program indicates which mode it is running     */
000500060509/*              and calls driver program to produce                 */
000600060509/*              MBI CUSTOM ACCOUNT EXTRACTS                         */
000700060509/* ******************************************************************/
000800060509/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE                */
000900060509/* ******************************************************************/
001000060509/* Brian Lu   * 2006/05/09   * RFS32465 - created the CL program    */
001100060509/*            *              * This replaces qtp programs           */
001200060509/*            *              * GENCLUE and GENCLUE1.                */
001300060509/* ******************************************************************/
001400060509
001500060509             PGM
001600060509
001700060509            /* Set up LDA (Byte 46  = 'M') for the Adhoc  mode  */
001800060509             CHGDTAARA  DTAARA(*LDA (46 1)) VALUE('M')
001900060509
002000060509            /* Display Local-Data-Area             */
002100060509             DSPDTAARA  DTAARA(*LDA) OUTPUT(*PRINT)
002200060509
002300060509            /* Call CLUE Extract Files Driver Programs */
002400060509             CALL      PGM(JOBMBIEXT)
002500060509
002600060509            /* Set up LDA (Byte 46  = ' ') back to space       */
002700060509             CHGDTAARA  DTAARA(*LDA (46 1)) VALUE(' ')
002800930922ENDPGM
