import re
import math


text = '''Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.'''
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


def ascii85_to_ascii_gen(text):
    for chunk in re.finditer('.{1,5}', text):
        # convert chars into ascii codes and pad u's to chunk
        # lesser than 5 characters
        ordinals = (
            ord(c) - 33 for c in chunk.group().ljust(5, 'u')
        )
        # turn combination of five ascii codes into 32-bit integer
        base2 = sum(
            code * 85 ** (i - 1)
            for i, code in zip(range(5, 0, -1), ordinals)
        )
        # turn base2 into 32-bit form
        binarybase2 = bin(base2)[2:].zfill(32)
        # break 32-bit form into groups of four bytes
        codes = (
            int(b, 2) for b in re.findall('.{8}', binarybase2)
        )
        # get characters from ascii codes and throw away as much
        # characters as much u's were padded to a chunk
        chars = list(chr(c) for c in codes)[
            :
            4 - (5 - len(chunk.group()))
        ]
        yield from chars


def ascii_to_ascii85(text):
    return ''.join(ascii_to_ascii85_gen(text))


def ascii85_to_ascii(text):
    return ''.join(ascii85_to_ascii_gen(text))


print(text)
print(ascii_to_ascii85(text))
print(ascii85_to_ascii(ascii_to_ascii85(text)))

assert text == ascii85_to_ascii(ascii_to_ascii85(text))
