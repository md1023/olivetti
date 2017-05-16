import math

v = 1298230816

for i in range(round(math.log(v, 85))):
    print(
        v % (85 ** (i + 1)) // (85 ** i)
    )
