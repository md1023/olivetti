import re
import math


text = '''Man is distinguished, not only by his reason, but by this
singular passion from other animals, which is a lust of the mind, that
by a perseverance of delight in the continued and indefatigable
generation of knowledge, exceeds the short vehemence of any carnal
pleasure.'''
# text = 'easy'
# text = 'somewhat difficult'


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
        # get characters from ascii codes and throw away as much
        # characters as much zeros were padded to a chunk
        chars85 = list(chr(o + 33) for o in base85)[
            :
            5 - (4 - len(chunk.group()))
        ]
        yield from chars85


ascii85 = ''.join(ascii_to_ascii85_gen(text))
print(ascii85)

for chunk in list(re.finditer('.{1,5}', ascii85))[-2:]:
    # convert chars into ascii codes and pad zero bytes to chunk
    # lesser than 5 characters
    # for c in chunk.group().ljust(5, '\0'):
    #     print(c)
    ordinals = list(
        ord(c) - 33 for c in chunk.group().ljust(5, 'u')
    )
    # turn combination of five ascii codes into 32-bit integer
    base2 = sum(
        code * 85 ** (i - 1)
        for i, code in zip(range(5, 0, -1), ordinals)
    )
    binarybase2 = bin(base2)[2:].zfill(32)

    print(chunk.group(), ordinals, base2, grouped_bits)
