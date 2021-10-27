000100060509/********************************************************************/
000200060509/*=>                                                                */
000300060509/* Description: ACCOUNT & INVESTMENT ATTRIBUTES          (AD HOC)   */
000400060509/*              This program indicates which mode it is running     */
000500060509/*              and calls driver program to produce                 */
000600060509/*              ACCOUNT & INVESTMENT ATTRIBUTES EXTRCT FILE.        */
000700060509/* ******************************************************************/
000800060509/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE                */
000900060509/* ******************************************************************/
001000060509/* Brian Lu   * 2006/05/09   * RFS32465 - created the CL program    */
001100060509/*            *              * This replaces qtp programs           */
001200060509/*            *              * ACCATTRI and ACCATTR1.               */
001300060509/* ******************************************************************/
001400060509
001500060509             PGM
001600060509
001700060509            /* Display Local-Data-Area             */
001800060509
001900060509             DSPDTAARA  DTAARA(*LDA) OUTPUT(*PRINT)
002000060510
002100060509            /* ACCOUNT & INVESTMENT ATTRIBUTE EXTRACT  */
002200060509            /* Driver Programs */
002300060509             CALL      PGM(JOBACCATT)
002400060509
002500930922ENDPGM
