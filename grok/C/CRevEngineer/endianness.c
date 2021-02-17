/*
 *  Program to determine "Endiance" of the underlying
 *  hardware architecture.
 */

#include <sys/types.h>
#include <stdio.h>

int main(void)
{
  union u_t {
      u_int8_t u8;
      u_int16_t u16;
      u_int32_t u32;
      #if __WORDSIZE >= 64
      u_int64_t u64;
      #endif
  } u = { .u64 = 0x4A };

  int littleEndian =    u.u8 == u.u16
                     && u.u8 == u.u32
                     #if __WORDSIZE >= 64
                     && u.u8 == u.u64
                     #endif
                     ;

  int bigEndian =    u.u8 == (u.u16 +  8)
                  && u.u8 == (u.u32 + 24)
                  #if __WORDSIZE >= 64
                  && u.u8 == (u.u64 + 56)
                  #endif
                  ;

  int mixedEndian = ! (littleEndian || bigEndian);

  if (littleEndian) puts("little-endian");
  if (bigEndian) puts("big-endian");
  if (mixedEndian) puts("mixed-endian");

  return 0;
}

/* Notes:
 *
 *  1. Assumes 32 or 64 bit architecture.
 *  2. Use of macros I think unavoidable.
 *  3. Little-endian nice since all the correct bytes
 *     line up with each other in memory.  Usually this
 *     fact is used more by optimizers than high level
 *     programmers.
 *  4. Don't know how portable these u_intXX_t types are.
 *  5. Don't think in terms of right to left nor top to bottom,
 *     think "in the direction of increasing address."
 *  6. Big-endian -> high order (most significant) bytes first
 *  7. Little-endian -> low order (least significant) bytes first
 *  8. There is not really an 'endianess" at the bit level, most
 *     processors work directly with 8-byte blocks in parallel.
 *     For instance, the shift operators << ans >> are not as
 *     low level as one might suspect, They have to consistently
 *     fiddle the bits in adjacent bytes when proforming "shifts.".
 *     We lie to beginners when we say computers work with
 *     base 2, unless you are working with real low level embedded
 *     device drivers, computers really use base 256, all the
 *     possible states of an 8-bit word.
 *  9. Most network protocols, at least the higher level ones
 *     like IP, are big-endian.  Recent Intel x86 and x86-64
 *     CPUs have a MOVBE instruction which when fetching/writing
 *     will do big-endian/little-endian conversion.
 * 10. The term "endian" comes from "Gulliver's Travels", where a
 *     civil war erupts over whether the big end or the little end
 *     of a boiled egg is the proper end to crack open
 */
