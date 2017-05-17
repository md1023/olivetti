import re
import math


text = '''Man is distinguished, not only by his reason, but by this
singular passion from other animals, which is a lust of the mind, that
by a perseverance of delight in the continued and indefatigable
generation of knowledge, exceeds the short vehemence of any carnal
pleasure.'''


def ascii_to_ascii85_gen(text):
    for chunk in re.finditer('.{1,4}', text):
        # convert chars into ascii codes and pad zero bytes to chunk
        # lesser than 4 characters
        ordinals = (
            ord(c) for c in chunk.group().ljust(4, '\0')
        )
        # turn combination of four ascii codes into 32-bit integer
        base2 = int(''.join(
            bin(o)[2:].zfill(8)for o in ordinals
        ), 2)
        # collect factors for base 85 out of 32-bit integer
        base85 = (
            base2 % (85 ** (i)) // (85 ** (i - 1))
            # ascii85 has 5 bytes sequence
            for i in range(5, 0, -1)
        )
        # get characters from ascii codes
        chars85 = (chr(o + 33) for o in base85)
        yield from chars85


print(
    ''.join(ascii_to_ascii85_gen(text))
)
